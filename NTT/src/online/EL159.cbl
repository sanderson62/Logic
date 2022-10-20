      *((program: EL159.cl2))
000001 ID DIVISION.
000002
000003 PROGRAM-ID. EL159.
000004*
000005*AUTHOR.     PABLO
000006*            OMAHA NE
000007
000008*DATE-COMPILED.
000009*SECURITY.   *****************************************************
000010*            *                                                   *
000011*            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
000012*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000013*            *   OF CENTRAL STATES IS EXPRESSLY PROHIBITED       *
000014*            *   WITHOUT THE PRIOR WRITTEN PERMISSION OF         *
000015*            *   CENTRAL STATES                                  *
000016*            *                                                   *
000017*            *****************************************************
000018
000019*REMARKS.
000020
000021*        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
000022*    FOR PRODUCT DEFINITIONS.
000023
000024*    SCREENS     - EL159A - PRODUCT DEFINITION
000025
000026*    ENTERED BY  - EL601 - MAINTENANCE MENU
000027
000028*    EXIT TO     - EL601 - MAINTENANCE MENU
000029
000030*    INPUT FILE  - ERPDEF -              - PRODUCT DEFINITION
000031
000032*    OUTPUT FILE - ERPDEF -              - PRODUCT DEFINITION
000033
000034*    COMMAREA    - PASSED
000035
000036*    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
000037*                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
000038*                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
000039*                  ENTRIES (XCTL FROM CICS VIA E031) THE SCREEN
000040*                  WILL BE READ AND ACTION WILL BE BASED ON THE
000041*                  MAINTENANCE TYPE INDICATED.
000042
000043******************************************************************
000044*                   C H A N G E   L O G
000045*
000046* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000047*-----------------------------------------------------------------
000048*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000049* EFFECTIVE    NUMBER
000050*-----------------------------------------------------------------
000051* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000052* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000053* 100314  CR2014061900001  PEMA  ADD BENEFIT PERCENT
000054* 110618  CR2018100400001  TANA  ADD OTHER CLAIM TYPE
000055* 080322  CR2021100800003  TANA  Add B and H claim types
000056******************************************************************
000057 ENVIRONMENT DIVISION.
000058 DATA DIVISION.
000059 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000060 77  FILLER  PIC X(32)  VALUE '********************************'.
000061 77  FILLER  PIC X(32)  VALUE '*    EL159 WORKING STORAGE     *'.
000062 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
000063
000064 77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.
000065     88  FIRST-READ-PREV                         VALUE 'Y'.
000066 77  M1                              PIC S999 COMP-3 VALUE +0.
000067 77  R1                              PIC S999 COMP-3 VALUE +0.
000068 77  PAGE-NBR                        PIC S999 COMP-3 VALUE +0.
000069 01  WS-LINE-NBR-CHAR.
000070     12  WS-LINE-NBR.
000071         16  WS-LINE-NBR-1           PIC 9.
000072         16  WS-LINE-NBR-P           PIC X(02).
000073     12  WS-LINE-NBR-R REDEFINES WS-LINE-NBR.
000074         16  WS-LINE-NBR-2           PIC 9(02).
000075         16  WS-LINE-NBR-P2          PIC X(01).
000076
000077 01  ACCESS-KEYS.
000078     12  ERPDEF-KEY.
000079         16  ERPDEF-COMPANY-CD       PIC X.
000080         16  ERPDEF-STATE            PIC XX.
000081         16  ERPDEF-PROD-CD          PIC XXX.
000082         16  FILLER                  PIC X(7).
000083         16  ERPDEF-BEN-TYPE         PIC X.
000084         16  ERPDEF-BEN-CODE         PIC XX.
000085         16  ERPDEF-EXP-DT           PIC XX.
000086
000087     12  ELCNTL-KEY.
000088         16  ELCNTL-COMPANY-ID       PIC X(03).
000089         16  ELCNTL-RECORD-TYPE      PIC X(01).
000090         16  ELCNTL-ACCESS           PIC X(04).
000091         16  ELCNTL-STATE-ACCESS REDEFINES ELCNTL-ACCESS.
000092             20  ELCNTL-STATE-CD     PIC  X(02).
000093             20  FILLER              PIC  X(02).
000094         16  ELCNTL-BENEFIT-ACCESS REDEFINES ELCNTL-ACCESS.
000095             20  FILLER              PIC  X(02).
000096             20  ELCNTL-BENE-CD      PIC  X(02).
000097         16  ELCNTL-SEQUENCE-NO      PIC S9(04)      COMP.
000098
000099 01  WS-DATE-AREA.
000100     05  SAVE-DATE                   PIC X(08)   VALUE SPACES.
000101     05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.
000102
000103 01  MISC-WORK-AREAS.
000104
000105     12  WS-RESPONSE             PIC S9(8)   COMP.
000106         88  RESP-NORMAL              VALUE +00.
000107         88  RESP-ERROR               VALUE +01.
000108         88  RESP-TERMIDERR           VALUE +11.
000109         88  RESP-NOTFND              VALUE +13.
000110         88  RESP-DUPREC              VALUE +14.
000111         88  RESP-DUPKEY              VALUE +15.
000112         88  RESP-INVREQ              VALUE +16.
000113         88  RESP-NOTOPEN             VALUE +19.
000114         88  RESP-ENDFILE             VALUE +20.
000115         88  RESP-ILLOGIC             VALUE +21.
000116         88  RESP-LENGERR             VALUE +22.
000117     12  WS-NUMVAL.
000118         16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
000119         16  WS-999V99-OF-DEEDIT REDEFINES
000120             WS-NUMVAL-OF-DEEDIT     PIC 9(9)V99.
000121         16  WS-9V999-OF-DEEDIT REDEFINES
000122             WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
000123     12  DEEDIT-FIELD                PIC X(11).
000124     12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD
000125                                     PIC S9(11).
000126     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD
000127                                     PIC S9(09)V99.
000128
000129     12  WS-BENEFIT-FIELD.
000130         16  WS-BENE-TYPE            PIC X(01).
000131         16  WS-BENE-CODE            PIC X(02).
000132
000133     12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.
000134
000135 01  STANDARD-AREAS.
000136     12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.
000137     12  TRANS-ID                    PIC X(04)   VALUE 'E031'.
000138     12  EL150-TRANS-ID              PIC X(04)   VALUE 'EX23'.
000139     12  EL1591-TRANS-ID             PIC X(04)   VALUE 'E032'.
000140     12  PGM-NAME                    PIC X(08).
000141     12  TIME-IN                     PIC S9(07).
000142     12  TIME-OUT-R  REDEFINES TIME-IN.
000143         16  FILLER                  PIC X(01).
000144         16  TIME-OUT                PIC 99V99.
000145         16  FILLER                  PIC X(02).
000146     12  XCTL-005                    PIC X(08)   VALUE 'EL005'.
000147     12  XCTL-010                    PIC X(08)   VALUE 'EL010'.
000148     12  XCTL-155                    PIC X(08)   VALUE 'EL155'.
000149     12  XCTL-626                    PIC X(08)   VALUE 'EL626'.
000150     12  LINK-001                    PIC X(08)   VALUE 'EL001'.
000151     12  LINK-004                    PIC X(08)   VALUE 'EL004'.
000152     12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.
000153     12  THIS-PGM                    PIC X(08)   VALUE 'EL159'.
000154     12  ERPDEF-FILE-ID              PIC X(08)   VALUE 'ERPDEF'.
000155     12  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.
000156     12  ERPDEF-LENGTH               PIC S9(04)  VALUE +1319 COMP.
000157     12  SUB                         PIC 9(02).
000158     12  SUB-1                       PIC 9(02).
000159     12  SUB2                        PIC 9(02).
000160     12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.
000161     12  MAPSET-NAME                 PIC X(08)   VALUE 'EL159S'.
000162     12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL159A'.
000163     12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.
000164
000165     12  WS-CNTL-REC-FOUND-SW        PIC X(01)   VALUE 'N'.
000166         88  CNTL-RECORD-FOUND                   VALUE 'Y'.
000167
000168     12  WS-PRE-EXIST-CODES          PIC X(02)   VALUE ZEROS.
000169         88  VALID-PRE-EXIST-CODE                VALUES ARE '00'
000170                                                       THRU '99'.
000171
000172     12  WS-DISABILITY-CODES         PIC X(02)   VALUE ZEROS.
000173         88  VALID-DISABILITY-CODE               VALUES ARE '00'
000174                                                       THRU '99'.
000175
000176     12  WS-REFUND-METHOD            PIC X       VALUE ZEROS.
000177         88  VALID-REFUND-METHOD                 VALUES ARE ' ',
000178                                                   '1' THRU '9'.
000179
000180     12  WS-BENEFIT-SW               PIC X       VALUE SPACE.
000181         88  BENEFIT-FOUND                       VALUE 'Y'.
000182         88  BENEFIT-NOT-FOUND                   VALUE 'N'.
000183
000184     12  WS-MAXAMT                   PIC S9(9)    VALUE +0 COMP-3.
000185     12  WS-MAXATTAGE                PIC S999     VALUE +0 COMP-3.
000186     12  WS-MINAGE                   PIC S999     VALUE +0 COMP-3.
000187     12  WS-MAXAGE                   PIC S999     VALUE +0 COMP-3.
000188     12  WS-MAXTERM                  PIC S999     VALUE +0 COMP-3.
000189     12  WS-EXCL                     PIC S999     VALUE +0 COMP-3.
000190     12  WS-COV-ENDS                 PIC S999     VALUE +0 COMP-3.
000191     12  WS-ACC-ONLY                 PIC S999     VALUE +0 COMP-3.
000192     12  WS-CRIT-PER                 PIC S999     VALUE +0 COMP-3.
000193
000194
000195*                                    COPY ELCSCTM.
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
000196*                                    COPY ELCSCRTY.
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
000197
000198 01  ERROR-MESSAGES.
000199     12  ER-0000                     PIC X(04)   VALUE '0000'.
000200     12  ER-0023                     PIC X(04)   VALUE '0023'.
000201     12  ER-0029                     PIC X(04)   VALUE '0029'.
000202     12  ER-0050                     PIC X(04)   VALUE '0050'.
000203     12  ER-0067                     PIC X(04)   VALUE '0067'.
000204     12  ER-0068                     PIC X(04)   VALUE '0068'.
000205     12  ER-0070                     PIC X(04)   VALUE '0070'.
000206     12  ER-0073                     PIC X(04)   VALUE '0073'.
000207     12  ER-0130                     PIC X(04)   VALUE '0130'.
000208     12  ER-0131                     PIC X(04)   VALUE '0131'.
000209     12  ER-0132                     PIC X(04)   VALUE '0132'.
000210     12  ER-0138                     PIC X(04)   VALUE '0138'.
000211     12  ER-0144                     PIC X(04)   VALUE '0144'.
000212     12  ER-0145                     PIC X(04)   VALUE '0145'.
000213     12  ER-0418                     PIC X(04)   VALUE '0418'.
000214     12  ER-0582                     PIC X(04)   VALUE '0582'.
000215     12  ER-0701                     PIC X(04)   VALUE '0701'.
000216     12  ER-0702                     PIC X(04)   VALUE '0702'.
000217     12  ER-0703                     PIC X(04)   VALUE '0703'.
000218     12  ER-0704                     PIC X(04)   VALUE '0704'.
000219     12  ER-0705                     PIC X(04)   VALUE '0705'.
000220     12  ER-0706                     PIC X(04)   VALUE '0706'.
000221     12  ER-0707                     PIC X(04)   VALUE '0707'.
000222     12  ER-0708                     PIC X(04)   VALUE '0708'.
000223     12  ER-0709                     PIC X(04)   VALUE '0709'.
000224     12  ER-0710                     PIC X(04)   VALUE '0710'.
000225     12  ER-0711                     PIC X(04)   VALUE '0711'.
000226     12  ER-0712                     PIC X(04)   VALUE '0712'.
000227     12  ER-0713                     PIC X(04)   VALUE '0713'.
000228     12  ER-0717                     PIC X(04)   VALUE '0717'.
000229     12  ER-0718                     PIC X(04)   VALUE '0718'.
000230     12  ER-0719                     PIC X(04)   VALUE '0719'.
000231     12  ER-0720                     PIC X(04)   VALUE '0720'.
000232     12  ER-0721                     PIC X(04)   VALUE '0721'.
000233     12  ER-0722                     PIC X(04)   VALUE '0722'.
000234     12  ER-0723                     PIC X(04)   VALUE '0723'.
000235     12  ER-0724                     PIC X(04)   VALUE '0724'.
000236     12  ER-0725                     PIC X(04)   VALUE '0725'.
000237     12  ER-0726                     PIC X(04)   VALUE '0726'.
000238     12  ER-0727                     PIC X(04)   VALUE '0727'.
000239     12  ER-0729                     PIC X(04)   VALUE '0729'.
000240     12  ER-0754                     PIC X(04)   VALUE '0754'.
000241     12  ER-1164                     PIC X(04)   VALUE '1164'.
000242     12  ER-2241                     PIC X(04)   VALUE '2241'.
000243     12  ER-2276                     PIC X(04)   VALUE '2276'.
000244     12  ER-7008                     PIC X(04)   VALUE '7008'.
000245     12  ER-7031                     PIC X(04)   VALUE '7031'.
000246     12  ER-7123                     PIC X(04)   VALUE '7123'.
000247     12  ER-7132                     PIC X(04)   VALUE '7132'.
000248     12  ER-8150                     PIC X(04)   VALUE '8150'.
000249     12  ER-9999                     PIC XXXX    VALUE '9999'.
000250*                                    COPY ELCDATE.
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
000251
000252*                                    COPY ELCLOGOF.
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
000253
000254*                                    COPY ELCATTR.
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
000255
000256*                                    COPY ELCEMIB.
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
000257
000258*                                    COPY ELCJPFX.
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
000259                                     PIC X(530).
000260
000261
000262*                                    COPY ELCINTF.
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
000263     12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
000264
000265         16  PI-FORM-NUMBER          PIC X(12).
000266
000267         16  PI-PREV-PROD-KEY.
000268             20  PI-PREV-COMPANY-CD  PIC X(01).
000269             20  PI-PREV-STATE       PIC XX.
000270             20  PI-PREV-PROD-CD     PIC XXX.
000271             20  FILLER              PIC X(7).
000272             20  PI-PREV-BEN-TYPE    PIC X.
000273             20  PI-PREV-BEN-CODE    PIC XX.
000274             20  PI-PREV-EXP-DT      PIC XX.
000275
000276         16  PI-PROD-KEY.
000277             20  PI-PK-COMPANY-CD    PIC X.
000278             20  PI-PK-STATE         PIC XX.
000279             20  PI-PK-PROD-CD       PIC XXX.
000280             20  FILLER              PIC X(7).
000281             20  PI-BEN-TYPE         PIC X.
000282             20  PI-BEN-CODE         PIC XX.
000283             20  PI-EXP-DT           PIC XX.
000284         16  FILLER                  PIC X(589).
000285
000286*                                    COPY ELCAID.
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
000287 01  FILLER    REDEFINES DFHAID.
000288     12  FILLER                      PIC X(08).
000289     12  PF-VALUES                   PIC X         OCCURS 24.
000290
000291*                                    COPY EL159S.
      *>>((file: EL159S))
000001 01  EL159AI.
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
000016     05  MAINTL PIC S9(0004) COMP.
000017     05  MAINTF PIC  X(0001).
000018     05  FILLER REDEFINES MAINTF.
000019         10  MAINTA PIC  X(0001).
000020     05  MAINTI PIC  X(0001).
000021*    -------------------------------
000022     05  MAINTBYL PIC S9(0004) COMP.
000023     05  MAINTBYF PIC  X(0001).
000024     05  FILLER REDEFINES MAINTBYF.
000025         10  MAINTBYA PIC  X(0001).
000026     05  MAINTBYI PIC  X(0004).
000027*    -------------------------------
000028     05  MAINTONL PIC S9(0004) COMP.
000029     05  MAINTONF PIC  X(0001).
000030     05  FILLER REDEFINES MAINTONF.
000031         10  MAINTONA PIC  X(0001).
000032     05  MAINTONI PIC  X(0008).
000033*    -------------------------------
000034     05  MAINTATL PIC S9(0004) COMP.
000035     05  MAINTATF PIC  X(0001).
000036     05  FILLER REDEFINES MAINTATF.
000037         10  MAINTATA PIC  X(0001).
000038     05  MAINTATI PIC  X(0005).
000039*    -------------------------------
000040     05  STATEL PIC S9(0004) COMP.
000041     05  STATEF PIC  X(0001).
000042     05  FILLER REDEFINES STATEF.
000043         10  STATEA PIC  X(0001).
000044     05  STATEI PIC  X(0002).
000045*    -------------------------------
000046     05  PRODCDL PIC S9(0004) COMP.
000047     05  PRODCDF PIC  X(0001).
000048     05  FILLER REDEFINES PRODCDF.
000049         10  PRODCDA PIC  X(0001).
000050     05  PRODCDI PIC  X(0003).
000051*    -------------------------------
000052     05  BENTYPL PIC S9(0004) COMP.
000053     05  BENTYPF PIC  X(0001).
000054     05  FILLER REDEFINES BENTYPF.
000055         10  BENTYPA PIC  X(0001).
000056     05  BENTYPI PIC  X(0001).
000057*    -------------------------------
000058     05  BENCODEL PIC S9(0004) COMP.
000059     05  BENCODEF PIC  X(0001).
000060     05  FILLER REDEFINES BENCODEF.
000061         10  BENCODEA PIC  X(0001).
000062     05  BENCODEI PIC  X(0002).
000063*    -------------------------------
000064     05  EXPDTL PIC S9(0004) COMP.
000065     05  EXPDTF PIC  X(0001).
000066     05  FILLER REDEFINES EXPDTF.
000067         10  EXPDTA PIC  X(0001).
000068     05  EXPDTI PIC  X(0010).
000069*    -------------------------------
000070     05  PDESCL PIC S9(0004) COMP.
000071     05  PDESCF PIC  X(0001).
000072     05  FILLER REDEFINES PDESCF.
000073         10  PDESCA PIC  X(0001).
000074     05  PDESCI PIC  X(0080).
000075*    -------------------------------
000076     05  AFAL PIC S9(0004) COMP.
000077     05  AFAF PIC  X(0001).
000078     05  FILLER REDEFINES AFAF.
000079         10  AFAA PIC  X(0001).
000080     05  AFAI PIC  X(0006).
000081*    -------------------------------
000082     05  TRUNCL PIC S9(0004) COMP.
000083     05  TRUNCF PIC  X(0001).
000084     05  FILLER REDEFINES TRUNCF.
000085         10  TRUNCA PIC  X(0001).
000086     05  TRUNCI PIC  X(0001).
000087*    -------------------------------
000088     05  PROD1L PIC S9(0004) COMP.
000089     05  PROD1F PIC  X(0001).
000090     05  FILLER REDEFINES PROD1F.
000091         10  PROD1A PIC  X(0001).
000092     05  PROD1I PIC  X(0001).
000093*    -------------------------------
000094     05  ATTAGE1L PIC S9(0004) COMP.
000095     05  ATTAGE1F PIC  X(0001).
000096     05  FILLER REDEFINES ATTAGE1F.
000097         10  ATTAGE1A PIC  X(0001).
000098     05  ATTAGE1I PIC  X(0002).
000099*    -------------------------------
000100     05  MINAGE1L PIC S9(0004) COMP.
000101     05  MINAGE1F PIC  X(0001).
000102     05  FILLER REDEFINES MINAGE1F.
000103         10  MINAGE1A PIC  X(0001).
000104     05  MINAGE1I PIC  X(0002).
000105*    -------------------------------
000106     05  MAXAGE1L PIC S9(0004) COMP.
000107     05  MAXAGE1F PIC  X(0001).
000108     05  FILLER REDEFINES MAXAGE1F.
000109         10  MAXAGE1A PIC  X(0001).
000110     05  MAXAGE1I PIC  X(0002).
000111*    -------------------------------
000112     05  MAXTRM1L PIC S9(0004) COMP.
000113     05  MAXTRM1F PIC  X(0001).
000114     05  FILLER REDEFINES MAXTRM1F.
000115         10  MAXTRM1A PIC  X(0001).
000116     05  MAXTRM1I PIC  X(0003).
000117*    -------------------------------
000118     05  BENPCT1L PIC S9(0004) COMP.
000119     05  BENPCT1F PIC  X(0001).
000120     05  FILLER REDEFINES BENPCT1F.
000121         10  BENPCT1A PIC  X(0001).
000122     05  BENPCT1I PIC  X(0004).
000123*    -------------------------------
000124     05  MAXAMT1L PIC S9(0004) COMP.
000125     05  MAXAMT1F PIC  X(0001).
000126     05  FILLER REDEFINES MAXAMT1F.
000127         10  MAXAMT1A PIC  X(0001).
000128     05  MAXAMT1I PIC  X(0009).
000129*    -------------------------------
000130     05  PRE1L PIC S9(0004) COMP.
000131     05  PRE1F PIC  X(0001).
000132     05  FILLER REDEFINES PRE1F.
000133         10  PRE1A PIC  X(0001).
000134     05  PRE1I PIC  X(0002).
000135*    -------------------------------
000136     05  EXCL1L PIC S9(0004) COMP.
000137     05  EXCL1F PIC  X(0001).
000138     05  FILLER REDEFINES EXCL1F.
000139         10  EXCL1A PIC  X(0001).
000140     05  EXCL1I PIC  X(0003).
000141*    -------------------------------
000142     05  COVEND1L PIC S9(0004) COMP.
000143     05  COVEND1F PIC  X(0001).
000144     05  FILLER REDEFINES COVEND1F.
000145         10  COVEND1A PIC  X(0001).
000146     05  COVEND1I PIC  X(0003).
000147*    -------------------------------
000148     05  ACCPER1L PIC S9(0004) COMP.
000149     05  ACCPER1F PIC  X(0001).
000150     05  FILLER REDEFINES ACCPER1F.
000151         10  ACCPER1A PIC  X(0001).
000152     05  ACCPER1I PIC  9999.
000153*    -------------------------------
000154     05  CRITPD1L PIC S9(0004) COMP.
000155     05  CRITPD1F PIC  X(0001).
000156     05  FILLER REDEFINES CRITPD1F.
000157         10  CRITPD1A PIC  X(0001).
000158     05  CRITPD1I PIC  999.
000159*    -------------------------------
000160     05  RECCP1L PIC S9(0004) COMP.
000161     05  RECCP1F PIC  X(0001).
000162     05  FILLER REDEFINES RECCP1F.
000163         10  RECCP1A PIC  X(0001).
000164     05  RECCP1I PIC  X(0002).
000165*    -------------------------------
000166     05  RTW1L PIC S9(0004) COMP.
000167     05  RTW1F PIC  X(0001).
000168     05  FILLER REDEFINES RTW1F.
000169         10  RTW1A PIC  X(0001).
000170     05  RTW1I PIC  999.
000171*    -------------------------------
000172     05  MEXT1L PIC S9(0004) COMP.
000173     05  MEXT1F PIC  X(0001).
000174     05  FILLER REDEFINES MEXT1F.
000175         10  MEXT1A PIC  X(0001).
000176     05  MEXT1I PIC  999.
000177*    -------------------------------
000178     05  PROD2L PIC S9(0004) COMP.
000179     05  PROD2F PIC  X(0001).
000180     05  FILLER REDEFINES PROD2F.
000181         10  PROD2A PIC  X(0001).
000182     05  PROD2I PIC  X(0001).
000183*    -------------------------------
000184     05  ATTAGE2L PIC S9(0004) COMP.
000185     05  ATTAGE2F PIC  X(0001).
000186     05  FILLER REDEFINES ATTAGE2F.
000187         10  ATTAGE2A PIC  X(0001).
000188     05  ATTAGE2I PIC  X(0002).
000189*    -------------------------------
000190     05  MINAGE2L PIC S9(0004) COMP.
000191     05  MINAGE2F PIC  X(0001).
000192     05  FILLER REDEFINES MINAGE2F.
000193         10  MINAGE2A PIC  X(0001).
000194     05  MINAGE2I PIC  X(0002).
000195*    -------------------------------
000196     05  MAXAGE2L PIC S9(0004) COMP.
000197     05  MAXAGE2F PIC  X(0001).
000198     05  FILLER REDEFINES MAXAGE2F.
000199         10  MAXAGE2A PIC  X(0001).
000200     05  MAXAGE2I PIC  X(0002).
000201*    -------------------------------
000202     05  MAXTRM2L PIC S9(0004) COMP.
000203     05  MAXTRM2F PIC  X(0001).
000204     05  FILLER REDEFINES MAXTRM2F.
000205         10  MAXTRM2A PIC  X(0001).
000206     05  MAXTRM2I PIC  X(0003).
000207*    -------------------------------
000208     05  BENPCT2L PIC S9(0004) COMP.
000209     05  BENPCT2F PIC  X(0001).
000210     05  FILLER REDEFINES BENPCT2F.
000211         10  BENPCT2A PIC  X(0001).
000212     05  BENPCT2I PIC  X(0004).
000213*    -------------------------------
000214     05  MAXAMT2L PIC S9(0004) COMP.
000215     05  MAXAMT2F PIC  X(0001).
000216     05  FILLER REDEFINES MAXAMT2F.
000217         10  MAXAMT2A PIC  X(0001).
000218     05  MAXAMT2I PIC  X(0009).
000219*    -------------------------------
000220     05  PRE2L PIC S9(0004) COMP.
000221     05  PRE2F PIC  X(0001).
000222     05  FILLER REDEFINES PRE2F.
000223         10  PRE2A PIC  X(0001).
000224     05  PRE2I PIC  X(0002).
000225*    -------------------------------
000226     05  EXCL2L PIC S9(0004) COMP.
000227     05  EXCL2F PIC  X(0001).
000228     05  FILLER REDEFINES EXCL2F.
000229         10  EXCL2A PIC  X(0001).
000230     05  EXCL2I PIC  X(0003).
000231*    -------------------------------
000232     05  COVEND2L PIC S9(0004) COMP.
000233     05  COVEND2F PIC  X(0001).
000234     05  FILLER REDEFINES COVEND2F.
000235         10  COVEND2A PIC  X(0001).
000236     05  COVEND2I PIC  X(0003).
000237*    -------------------------------
000238     05  ACCPER2L PIC S9(0004) COMP.
000239     05  ACCPER2F PIC  X(0001).
000240     05  FILLER REDEFINES ACCPER2F.
000241         10  ACCPER2A PIC  X(0001).
000242     05  ACCPER2I PIC  9999.
000243*    -------------------------------
000244     05  CRITPD2L PIC S9(0004) COMP.
000245     05  CRITPD2F PIC  X(0001).
000246     05  FILLER REDEFINES CRITPD2F.
000247         10  CRITPD2A PIC  X(0001).
000248     05  CRITPD2I PIC  999.
000249*    -------------------------------
000250     05  RECCP2L PIC S9(0004) COMP.
000251     05  RECCP2F PIC  X(0001).
000252     05  FILLER REDEFINES RECCP2F.
000253         10  RECCP2A PIC  X(0001).
000254     05  RECCP2I PIC  X(0002).
000255*    -------------------------------
000256     05  RTW2L PIC S9(0004) COMP.
000257     05  RTW2F PIC  X(0001).
000258     05  FILLER REDEFINES RTW2F.
000259         10  RTW2A PIC  X(0001).
000260     05  RTW2I PIC  999.
000261*    -------------------------------
000262     05  MEXT2L PIC S9(0004) COMP.
000263     05  MEXT2F PIC  X(0001).
000264     05  FILLER REDEFINES MEXT2F.
000265         10  MEXT2A PIC  X(0001).
000266     05  MEXT2I PIC  999.
000267*    -------------------------------
000268     05  PROD3L PIC S9(0004) COMP.
000269     05  PROD3F PIC  X(0001).
000270     05  FILLER REDEFINES PROD3F.
000271         10  PROD3A PIC  X(0001).
000272     05  PROD3I PIC  X(0001).
000273*    -------------------------------
000274     05  ATTAGE3L PIC S9(0004) COMP.
000275     05  ATTAGE3F PIC  X(0001).
000276     05  FILLER REDEFINES ATTAGE3F.
000277         10  ATTAGE3A PIC  X(0001).
000278     05  ATTAGE3I PIC  X(0002).
000279*    -------------------------------
000280     05  MINAGE3L PIC S9(0004) COMP.
000281     05  MINAGE3F PIC  X(0001).
000282     05  FILLER REDEFINES MINAGE3F.
000283         10  MINAGE3A PIC  X(0001).
000284     05  MINAGE3I PIC  X(0002).
000285*    -------------------------------
000286     05  MAXAGE3L PIC S9(0004) COMP.
000287     05  MAXAGE3F PIC  X(0001).
000288     05  FILLER REDEFINES MAXAGE3F.
000289         10  MAXAGE3A PIC  X(0001).
000290     05  MAXAGE3I PIC  X(0002).
000291*    -------------------------------
000292     05  MAXTRM3L PIC S9(0004) COMP.
000293     05  MAXTRM3F PIC  X(0001).
000294     05  FILLER REDEFINES MAXTRM3F.
000295         10  MAXTRM3A PIC  X(0001).
000296     05  MAXTRM3I PIC  X(0003).
000297*    -------------------------------
000298     05  BENPCT3L PIC S9(0004) COMP.
000299     05  BENPCT3F PIC  X(0001).
000300     05  FILLER REDEFINES BENPCT3F.
000301         10  BENPCT3A PIC  X(0001).
000302     05  BENPCT3I PIC  X(0004).
000303*    -------------------------------
000304     05  MAXAMT3L PIC S9(0004) COMP.
000305     05  MAXAMT3F PIC  X(0001).
000306     05  FILLER REDEFINES MAXAMT3F.
000307         10  MAXAMT3A PIC  X(0001).
000308     05  MAXAMT3I PIC  X(0009).
000309*    -------------------------------
000310     05  PRE3L PIC S9(0004) COMP.
000311     05  PRE3F PIC  X(0001).
000312     05  FILLER REDEFINES PRE3F.
000313         10  PRE3A PIC  X(0001).
000314     05  PRE3I PIC  X(0002).
000315*    -------------------------------
000316     05  EXCL3L PIC S9(0004) COMP.
000317     05  EXCL3F PIC  X(0001).
000318     05  FILLER REDEFINES EXCL3F.
000319         10  EXCL3A PIC  X(0001).
000320     05  EXCL3I PIC  X(0003).
000321*    -------------------------------
000322     05  COVEND3L PIC S9(0004) COMP.
000323     05  COVEND3F PIC  X(0001).
000324     05  FILLER REDEFINES COVEND3F.
000325         10  COVEND3A PIC  X(0001).
000326     05  COVEND3I PIC  X(0003).
000327*    -------------------------------
000328     05  ACCPER3L PIC S9(0004) COMP.
000329     05  ACCPER3F PIC  X(0001).
000330     05  FILLER REDEFINES ACCPER3F.
000331         10  ACCPER3A PIC  X(0001).
000332     05  ACCPER3I PIC  9999.
000333*    -------------------------------
000334     05  CRITPD3L PIC S9(0004) COMP.
000335     05  CRITPD3F PIC  X(0001).
000336     05  FILLER REDEFINES CRITPD3F.
000337         10  CRITPD3A PIC  X(0001).
000338     05  CRITPD3I PIC  999.
000339*    -------------------------------
000340     05  RECCP3L PIC S9(0004) COMP.
000341     05  RECCP3F PIC  X(0001).
000342     05  FILLER REDEFINES RECCP3F.
000343         10  RECCP3A PIC  X(0001).
000344     05  RECCP3I PIC  X(0002).
000345*    -------------------------------
000346     05  RTW3L PIC S9(0004) COMP.
000347     05  RTW3F PIC  X(0001).
000348     05  FILLER REDEFINES RTW3F.
000349         10  RTW3A PIC  X(0001).
000350     05  RTW3I PIC  999.
000351*    -------------------------------
000352     05  MEXT3L PIC S9(0004) COMP.
000353     05  MEXT3F PIC  X(0001).
000354     05  FILLER REDEFINES MEXT3F.
000355         10  MEXT3A PIC  X(0001).
000356     05  MEXT3I PIC  999.
000357*    -------------------------------
000358     05  PROD4L PIC S9(0004) COMP.
000359     05  PROD4F PIC  X(0001).
000360     05  FILLER REDEFINES PROD4F.
000361         10  PROD4A PIC  X(0001).
000362     05  PROD4I PIC  X(0001).
000363*    -------------------------------
000364     05  ATTAGE4L PIC S9(0004) COMP.
000365     05  ATTAGE4F PIC  X(0001).
000366     05  FILLER REDEFINES ATTAGE4F.
000367         10  ATTAGE4A PIC  X(0001).
000368     05  ATTAGE4I PIC  X(0002).
000369*    -------------------------------
000370     05  MINAGE4L PIC S9(0004) COMP.
000371     05  MINAGE4F PIC  X(0001).
000372     05  FILLER REDEFINES MINAGE4F.
000373         10  MINAGE4A PIC  X(0001).
000374     05  MINAGE4I PIC  X(0002).
000375*    -------------------------------
000376     05  MAXAGE4L PIC S9(0004) COMP.
000377     05  MAXAGE4F PIC  X(0001).
000378     05  FILLER REDEFINES MAXAGE4F.
000379         10  MAXAGE4A PIC  X(0001).
000380     05  MAXAGE4I PIC  X(0002).
000381*    -------------------------------
000382     05  MAXTRM4L PIC S9(0004) COMP.
000383     05  MAXTRM4F PIC  X(0001).
000384     05  FILLER REDEFINES MAXTRM4F.
000385         10  MAXTRM4A PIC  X(0001).
000386     05  MAXTRM4I PIC  X(0003).
000387*    -------------------------------
000388     05  BENPCT4L PIC S9(0004) COMP.
000389     05  BENPCT4F PIC  X(0001).
000390     05  FILLER REDEFINES BENPCT4F.
000391         10  BENPCT4A PIC  X(0001).
000392     05  BENPCT4I PIC  X(0004).
000393*    -------------------------------
000394     05  MAXAMT4L PIC S9(0004) COMP.
000395     05  MAXAMT4F PIC  X(0001).
000396     05  FILLER REDEFINES MAXAMT4F.
000397         10  MAXAMT4A PIC  X(0001).
000398     05  MAXAMT4I PIC  X(0009).
000399*    -------------------------------
000400     05  PRE4L PIC S9(0004) COMP.
000401     05  PRE4F PIC  X(0001).
000402     05  FILLER REDEFINES PRE4F.
000403         10  PRE4A PIC  X(0001).
000404     05  PRE4I PIC  X(0002).
000405*    -------------------------------
000406     05  EXCL4L PIC S9(0004) COMP.
000407     05  EXCL4F PIC  X(0001).
000408     05  FILLER REDEFINES EXCL4F.
000409         10  EXCL4A PIC  X(0001).
000410     05  EXCL4I PIC  X(0003).
000411*    -------------------------------
000412     05  COVEND4L PIC S9(0004) COMP.
000413     05  COVEND4F PIC  X(0001).
000414     05  FILLER REDEFINES COVEND4F.
000415         10  COVEND4A PIC  X(0001).
000416     05  COVEND4I PIC  X(0003).
000417*    -------------------------------
000418     05  ACCPER4L PIC S9(0004) COMP.
000419     05  ACCPER4F PIC  X(0001).
000420     05  FILLER REDEFINES ACCPER4F.
000421         10  ACCPER4A PIC  X(0001).
000422     05  ACCPER4I PIC  9999.
000423*    -------------------------------
000424     05  CRITPD4L PIC S9(0004) COMP.
000425     05  CRITPD4F PIC  X(0001).
000426     05  FILLER REDEFINES CRITPD4F.
000427         10  CRITPD4A PIC  X(0001).
000428     05  CRITPD4I PIC  999.
000429*    -------------------------------
000430     05  RECCP4L PIC S9(0004) COMP.
000431     05  RECCP4F PIC  X(0001).
000432     05  FILLER REDEFINES RECCP4F.
000433         10  RECCP4A PIC  X(0001).
000434     05  RECCP4I PIC  X(0002).
000435*    -------------------------------
000436     05  RTW4L PIC S9(0004) COMP.
000437     05  RTW4F PIC  X(0001).
000438     05  FILLER REDEFINES RTW4F.
000439         10  RTW4A PIC  X(0001).
000440     05  RTW4I PIC  999.
000441*    -------------------------------
000442     05  MEXT4L PIC S9(0004) COMP.
000443     05  MEXT4F PIC  X(0001).
000444     05  FILLER REDEFINES MEXT4F.
000445         10  MEXT4A PIC  X(0001).
000446     05  MEXT4I PIC  999.
000447*    -------------------------------
000448     05  PROD5L PIC S9(0004) COMP.
000449     05  PROD5F PIC  X(0001).
000450     05  FILLER REDEFINES PROD5F.
000451         10  PROD5A PIC  X(0001).
000452     05  PROD5I PIC  X(0001).
000453*    -------------------------------
000454     05  ATTAGE5L PIC S9(0004) COMP.
000455     05  ATTAGE5F PIC  X(0001).
000456     05  FILLER REDEFINES ATTAGE5F.
000457         10  ATTAGE5A PIC  X(0001).
000458     05  ATTAGE5I PIC  X(0002).
000459*    -------------------------------
000460     05  MINAGE5L PIC S9(0004) COMP.
000461     05  MINAGE5F PIC  X(0001).
000462     05  FILLER REDEFINES MINAGE5F.
000463         10  MINAGE5A PIC  X(0001).
000464     05  MINAGE5I PIC  X(0002).
000465*    -------------------------------
000466     05  MAXAGE5L PIC S9(0004) COMP.
000467     05  MAXAGE5F PIC  X(0001).
000468     05  FILLER REDEFINES MAXAGE5F.
000469         10  MAXAGE5A PIC  X(0001).
000470     05  MAXAGE5I PIC  X(0002).
000471*    -------------------------------
000472     05  MAXTRM5L PIC S9(0004) COMP.
000473     05  MAXTRM5F PIC  X(0001).
000474     05  FILLER REDEFINES MAXTRM5F.
000475         10  MAXTRM5A PIC  X(0001).
000476     05  MAXTRM5I PIC  X(0003).
000477*    -------------------------------
000478     05  BENPCT5L PIC S9(0004) COMP.
000479     05  BENPCT5F PIC  X(0001).
000480     05  FILLER REDEFINES BENPCT5F.
000481         10  BENPCT5A PIC  X(0001).
000482     05  BENPCT5I PIC  X(0004).
000483*    -------------------------------
000484     05  MAXAMT5L PIC S9(0004) COMP.
000485     05  MAXAMT5F PIC  X(0001).
000486     05  FILLER REDEFINES MAXAMT5F.
000487         10  MAXAMT5A PIC  X(0001).
000488     05  MAXAMT5I PIC  X(0009).
000489*    -------------------------------
000490     05  PRE5L PIC S9(0004) COMP.
000491     05  PRE5F PIC  X(0001).
000492     05  FILLER REDEFINES PRE5F.
000493         10  PRE5A PIC  X(0001).
000494     05  PRE5I PIC  X(0002).
000495*    -------------------------------
000496     05  EXCL5L PIC S9(0004) COMP.
000497     05  EXCL5F PIC  X(0001).
000498     05  FILLER REDEFINES EXCL5F.
000499         10  EXCL5A PIC  X(0001).
000500     05  EXCL5I PIC  X(0003).
000501*    -------------------------------
000502     05  COVEND5L PIC S9(0004) COMP.
000503     05  COVEND5F PIC  X(0001).
000504     05  FILLER REDEFINES COVEND5F.
000505         10  COVEND5A PIC  X(0001).
000506     05  COVEND5I PIC  X(0003).
000507*    -------------------------------
000508     05  ACCPER5L PIC S9(0004) COMP.
000509     05  ACCPER5F PIC  X(0001).
000510     05  FILLER REDEFINES ACCPER5F.
000511         10  ACCPER5A PIC  X(0001).
000512     05  ACCPER5I PIC  9999.
000513*    -------------------------------
000514     05  CRITPD5L PIC S9(0004) COMP.
000515     05  CRITPD5F PIC  X(0001).
000516     05  FILLER REDEFINES CRITPD5F.
000517         10  CRITPD5A PIC  X(0001).
000518     05  CRITPD5I PIC  999.
000519*    -------------------------------
000520     05  RECCP5L PIC S9(0004) COMP.
000521     05  RECCP5F PIC  X(0001).
000522     05  FILLER REDEFINES RECCP5F.
000523         10  RECCP5A PIC  X(0001).
000524     05  RECCP5I PIC  X(0002).
000525*    -------------------------------
000526     05  RTW5L PIC S9(0004) COMP.
000527     05  RTW5F PIC  X(0001).
000528     05  FILLER REDEFINES RTW5F.
000529         10  RTW5A PIC  X(0001).
000530     05  RTW5I PIC  999.
000531*    -------------------------------
000532     05  MEXT5L PIC S9(0004) COMP.
000533     05  MEXT5F PIC  X(0001).
000534     05  FILLER REDEFINES MEXT5F.
000535         10  MEXT5A PIC  X(0001).
000536     05  MEXT5I PIC  999.
000537*    -------------------------------
000538     05  PROD6L PIC S9(0004) COMP.
000539     05  PROD6F PIC  X(0001).
000540     05  FILLER REDEFINES PROD6F.
000541         10  PROD6A PIC  X(0001).
000542     05  PROD6I PIC  X(0001).
000543*    -------------------------------
000544     05  ATTAGE6L PIC S9(0004) COMP.
000545     05  ATTAGE6F PIC  X(0001).
000546     05  FILLER REDEFINES ATTAGE6F.
000547         10  ATTAGE6A PIC  X(0001).
000548     05  ATTAGE6I PIC  X(0002).
000549*    -------------------------------
000550     05  MINAGE6L PIC S9(0004) COMP.
000551     05  MINAGE6F PIC  X(0001).
000552     05  FILLER REDEFINES MINAGE6F.
000553         10  MINAGE6A PIC  X(0001).
000554     05  MINAGE6I PIC  X(0002).
000555*    -------------------------------
000556     05  MAXAGE6L PIC S9(0004) COMP.
000557     05  MAXAGE6F PIC  X(0001).
000558     05  FILLER REDEFINES MAXAGE6F.
000559         10  MAXAGE6A PIC  X(0001).
000560     05  MAXAGE6I PIC  X(0002).
000561*    -------------------------------
000562     05  MAXTRM6L PIC S9(0004) COMP.
000563     05  MAXTRM6F PIC  X(0001).
000564     05  FILLER REDEFINES MAXTRM6F.
000565         10  MAXTRM6A PIC  X(0001).
000566     05  MAXTRM6I PIC  X(0003).
000567*    -------------------------------
000568     05  BENPCT6L PIC S9(0004) COMP.
000569     05  BENPCT6F PIC  X(0001).
000570     05  FILLER REDEFINES BENPCT6F.
000571         10  BENPCT6A PIC  X(0001).
000572     05  BENPCT6I PIC  X(0004).
000573*    -------------------------------
000574     05  MAXAMT6L PIC S9(0004) COMP.
000575     05  MAXAMT6F PIC  X(0001).
000576     05  FILLER REDEFINES MAXAMT6F.
000577         10  MAXAMT6A PIC  X(0001).
000578     05  MAXAMT6I PIC  X(0009).
000579*    -------------------------------
000580     05  PRE6L PIC S9(0004) COMP.
000581     05  PRE6F PIC  X(0001).
000582     05  FILLER REDEFINES PRE6F.
000583         10  PRE6A PIC  X(0001).
000584     05  PRE6I PIC  X(0002).
000585*    -------------------------------
000586     05  EXCL6L PIC S9(0004) COMP.
000587     05  EXCL6F PIC  X(0001).
000588     05  FILLER REDEFINES EXCL6F.
000589         10  EXCL6A PIC  X(0001).
000590     05  EXCL6I PIC  X(0003).
000591*    -------------------------------
000592     05  COVEND6L PIC S9(0004) COMP.
000593     05  COVEND6F PIC  X(0001).
000594     05  FILLER REDEFINES COVEND6F.
000595         10  COVEND6A PIC  X(0001).
000596     05  COVEND6I PIC  X(0003).
000597*    -------------------------------
000598     05  ACCPER6L PIC S9(0004) COMP.
000599     05  ACCPER6F PIC  X(0001).
000600     05  FILLER REDEFINES ACCPER6F.
000601         10  ACCPER6A PIC  X(0001).
000602     05  ACCPER6I PIC  9999.
000603*    -------------------------------
000604     05  CRITPD6L PIC S9(0004) COMP.
000605     05  CRITPD6F PIC  X(0001).
000606     05  FILLER REDEFINES CRITPD6F.
000607         10  CRITPD6A PIC  X(0001).
000608     05  CRITPD6I PIC  999.
000609*    -------------------------------
000610     05  RECCP6L PIC S9(0004) COMP.
000611     05  RECCP6F PIC  X(0001).
000612     05  FILLER REDEFINES RECCP6F.
000613         10  RECCP6A PIC  X(0001).
000614     05  RECCP6I PIC  X(0002).
000615*    -------------------------------
000616     05  RTW6L PIC S9(0004) COMP.
000617     05  RTW6F PIC  X(0001).
000618     05  FILLER REDEFINES RTW6F.
000619         10  RTW6A PIC  X(0001).
000620     05  RTW6I PIC  999.
000621*    -------------------------------
000622     05  MEXT6L PIC S9(0004) COMP.
000623     05  MEXT6F PIC  X(0001).
000624     05  FILLER REDEFINES MEXT6F.
000625         10  MEXT6A PIC  X(0001).
000626     05  MEXT6I PIC  999.
000627*    -------------------------------
000628     05  PROD7L PIC S9(0004) COMP.
000629     05  PROD7F PIC  X(0001).
000630     05  FILLER REDEFINES PROD7F.
000631         10  PROD7A PIC  X(0001).
000632     05  PROD7I PIC  X(0001).
000633*    -------------------------------
000634     05  ATTAGE7L PIC S9(0004) COMP.
000635     05  ATTAGE7F PIC  X(0001).
000636     05  FILLER REDEFINES ATTAGE7F.
000637         10  ATTAGE7A PIC  X(0001).
000638     05  ATTAGE7I PIC  X(0002).
000639*    -------------------------------
000640     05  MINAGE7L PIC S9(0004) COMP.
000641     05  MINAGE7F PIC  X(0001).
000642     05  FILLER REDEFINES MINAGE7F.
000643         10  MINAGE7A PIC  X(0001).
000644     05  MINAGE7I PIC  X(0002).
000645*    -------------------------------
000646     05  MAXAGE7L PIC S9(0004) COMP.
000647     05  MAXAGE7F PIC  X(0001).
000648     05  FILLER REDEFINES MAXAGE7F.
000649         10  MAXAGE7A PIC  X(0001).
000650     05  MAXAGE7I PIC  X(0002).
000651*    -------------------------------
000652     05  MAXTRM7L PIC S9(0004) COMP.
000653     05  MAXTRM7F PIC  X(0001).
000654     05  FILLER REDEFINES MAXTRM7F.
000655         10  MAXTRM7A PIC  X(0001).
000656     05  MAXTRM7I PIC  X(0003).
000657*    -------------------------------
000658     05  BENPCT7L PIC S9(0004) COMP.
000659     05  BENPCT7F PIC  X(0001).
000660     05  FILLER REDEFINES BENPCT7F.
000661         10  BENPCT7A PIC  X(0001).
000662     05  BENPCT7I PIC  X(0004).
000663*    -------------------------------
000664     05  MAXAMT7L PIC S9(0004) COMP.
000665     05  MAXAMT7F PIC  X(0001).
000666     05  FILLER REDEFINES MAXAMT7F.
000667         10  MAXAMT7A PIC  X(0001).
000668     05  MAXAMT7I PIC  X(0009).
000669*    -------------------------------
000670     05  PRE7L PIC S9(0004) COMP.
000671     05  PRE7F PIC  X(0001).
000672     05  FILLER REDEFINES PRE7F.
000673         10  PRE7A PIC  X(0001).
000674     05  PRE7I PIC  X(0002).
000675*    -------------------------------
000676     05  EXCL7L PIC S9(0004) COMP.
000677     05  EXCL7F PIC  X(0001).
000678     05  FILLER REDEFINES EXCL7F.
000679         10  EXCL7A PIC  X(0001).
000680     05  EXCL7I PIC  X(0003).
000681*    -------------------------------
000682     05  COVEND7L PIC S9(0004) COMP.
000683     05  COVEND7F PIC  X(0001).
000684     05  FILLER REDEFINES COVEND7F.
000685         10  COVEND7A PIC  X(0001).
000686     05  COVEND7I PIC  X(0003).
000687*    -------------------------------
000688     05  ACCPER7L PIC S9(0004) COMP.
000689     05  ACCPER7F PIC  X(0001).
000690     05  FILLER REDEFINES ACCPER7F.
000691         10  ACCPER7A PIC  X(0001).
000692     05  ACCPER7I PIC  9999.
000693*    -------------------------------
000694     05  CRITPD7L PIC S9(0004) COMP.
000695     05  CRITPD7F PIC  X(0001).
000696     05  FILLER REDEFINES CRITPD7F.
000697         10  CRITPD7A PIC  X(0001).
000698     05  CRITPD7I PIC  999.
000699*    -------------------------------
000700     05  RECCP17L PIC S9(0004) COMP.
000701     05  RECCP17F PIC  X(0001).
000702     05  FILLER REDEFINES RECCP17F.
000703         10  RECCP17A PIC  X(0001).
000704     05  RECCP17I PIC  X(0002).
000705*    -------------------------------
000706     05  RTW7L PIC S9(0004) COMP.
000707     05  RTW7F PIC  X(0001).
000708     05  FILLER REDEFINES RTW7F.
000709         10  RTW7A PIC  X(0001).
000710     05  RTW7I PIC  999.
000711*    -------------------------------
000712     05  MEXT7L PIC S9(0004) COMP.
000713     05  MEXT7F PIC  X(0001).
000714     05  FILLER REDEFINES MEXT7F.
000715         10  MEXT7A PIC  X(0001).
000716     05  MEXT7I PIC  999.
000717*    -------------------------------
000718     05  PROD8L PIC S9(0004) COMP.
000719     05  PROD8F PIC  X(0001).
000720     05  FILLER REDEFINES PROD8F.
000721         10  PROD8A PIC  X(0001).
000722     05  PROD8I PIC  X(0001).
000723*    -------------------------------
000724     05  ATTAGE8L PIC S9(0004) COMP.
000725     05  ATTAGE8F PIC  X(0001).
000726     05  FILLER REDEFINES ATTAGE8F.
000727         10  ATTAGE8A PIC  X(0001).
000728     05  ATTAGE8I PIC  X(0002).
000729*    -------------------------------
000730     05  MINAGE8L PIC S9(0004) COMP.
000731     05  MINAGE8F PIC  X(0001).
000732     05  FILLER REDEFINES MINAGE8F.
000733         10  MINAGE8A PIC  X(0001).
000734     05  MINAGE8I PIC  X(0002).
000735*    -------------------------------
000736     05  MAXAGE8L PIC S9(0004) COMP.
000737     05  MAXAGE8F PIC  X(0001).
000738     05  FILLER REDEFINES MAXAGE8F.
000739         10  MAXAGE8A PIC  X(0001).
000740     05  MAXAGE8I PIC  X(0002).
000741*    -------------------------------
000742     05  MAXTRM8L PIC S9(0004) COMP.
000743     05  MAXTRM8F PIC  X(0001).
000744     05  FILLER REDEFINES MAXTRM8F.
000745         10  MAXTRM8A PIC  X(0001).
000746     05  MAXTRM8I PIC  X(0003).
000747*    -------------------------------
000748     05  BENPCT8L PIC S9(0004) COMP.
000749     05  BENPCT8F PIC  X(0001).
000750     05  FILLER REDEFINES BENPCT8F.
000751         10  BENPCT8A PIC  X(0001).
000752     05  BENPCT8I PIC  X(0004).
000753*    -------------------------------
000754     05  MAXAMT8L PIC S9(0004) COMP.
000755     05  MAXAMT8F PIC  X(0001).
000756     05  FILLER REDEFINES MAXAMT8F.
000757         10  MAXAMT8A PIC  X(0001).
000758     05  MAXAMT8I PIC  X(0009).
000759*    -------------------------------
000760     05  PRE8L PIC S9(0004) COMP.
000761     05  PRE8F PIC  X(0001).
000762     05  FILLER REDEFINES PRE8F.
000763         10  PRE8A PIC  X(0001).
000764     05  PRE8I PIC  X(0002).
000765*    -------------------------------
000766     05  EXCL8L PIC S9(0004) COMP.
000767     05  EXCL8F PIC  X(0001).
000768     05  FILLER REDEFINES EXCL8F.
000769         10  EXCL8A PIC  X(0001).
000770     05  EXCL8I PIC  X(0003).
000771*    -------------------------------
000772     05  COVEND8L PIC S9(0004) COMP.
000773     05  COVEND8F PIC  X(0001).
000774     05  FILLER REDEFINES COVEND8F.
000775         10  COVEND8A PIC  X(0001).
000776     05  COVEND8I PIC  X(0003).
000777*    -------------------------------
000778     05  ACCPER8L PIC S9(0004) COMP.
000779     05  ACCPER8F PIC  X(0001).
000780     05  FILLER REDEFINES ACCPER8F.
000781         10  ACCPER8A PIC  X(0001).
000782     05  ACCPER8I PIC  9999.
000783*    -------------------------------
000784     05  CRITPD8L PIC S9(0004) COMP.
000785     05  CRITPD8F PIC  X(0001).
000786     05  FILLER REDEFINES CRITPD8F.
000787         10  CRITPD8A PIC  X(0001).
000788     05  CRITPD8I PIC  999.
000789*    -------------------------------
000790     05  RECCP8L PIC S9(0004) COMP.
000791     05  RECCP8F PIC  X(0001).
000792     05  FILLER REDEFINES RECCP8F.
000793         10  RECCP8A PIC  X(0001).
000794     05  RECCP8I PIC  X(0002).
000795*    -------------------------------
000796     05  RTW8L PIC S9(0004) COMP.
000797     05  RTW8F PIC  X(0001).
000798     05  FILLER REDEFINES RTW8F.
000799         10  RTW8A PIC  X(0001).
000800     05  RTW8I PIC  999.
000801*    -------------------------------
000802     05  MEXT8L PIC S9(0004) COMP.
000803     05  MEXT8F PIC  X(0001).
000804     05  FILLER REDEFINES MEXT8F.
000805         10  MEXT8A PIC  X(0001).
000806     05  MEXT8I PIC  999.
000807*    -------------------------------
000808     05  ERRMSG1L PIC S9(0004) COMP.
000809     05  ERRMSG1F PIC  X(0001).
000810     05  FILLER REDEFINES ERRMSG1F.
000811         10  ERRMSG1A PIC  X(0001).
000812     05  ERRMSG1I PIC  X(0079).
000813*    -------------------------------
000814     05  PFKEYL PIC S9(0004) COMP.
000815     05  PFKEYF PIC  X(0001).
000816     05  FILLER REDEFINES PFKEYF.
000817         10  PFKEYA PIC  X(0001).
000818     05  PFKEYI PIC  99.
000819 01  EL159AO REDEFINES EL159AI.
000820     05  FILLER            PIC  X(0012).
000821*    -------------------------------
000822     05  FILLER            PIC  X(0003).
000823     05  DATEO PIC  X(0008).
000824*    -------------------------------
000825     05  FILLER            PIC  X(0003).
000826     05  TIMEO PIC  99.99.
000827*    -------------------------------
000828     05  FILLER            PIC  X(0003).
000829     05  MAINTO PIC  X(0001).
000830*    -------------------------------
000831     05  FILLER            PIC  X(0003).
000832     05  MAINTBYO PIC  X(0004).
000833*    -------------------------------
000834     05  FILLER            PIC  X(0003).
000835     05  MAINTONO PIC  X(0008).
000836*    -------------------------------
000837     05  FILLER            PIC  X(0003).
000838     05  MAINTATO PIC  99.99.
000839*    -------------------------------
000840     05  FILLER            PIC  X(0003).
000841     05  STATEO PIC  X(0002).
000842*    -------------------------------
000843     05  FILLER            PIC  X(0003).
000844     05  PRODCDO PIC  X(0003).
000845*    -------------------------------
000846     05  FILLER            PIC  X(0003).
000847     05  BENTYPO PIC  X(0001).
000848*    -------------------------------
000849     05  FILLER            PIC  X(0003).
000850     05  BENCODEO PIC  X(0002).
000851*    -------------------------------
000852     05  FILLER            PIC  X(0003).
000853     05  EXPDTO PIC  X(0010).
000854*    -------------------------------
000855     05  FILLER            PIC  X(0003).
000856     05  PDESCO PIC  X(0080).
000857*    -------------------------------
000858     05  FILLER            PIC  X(0003).
000859     05  AFAO PIC  X(0006).
000860*    -------------------------------
000861     05  FILLER            PIC  X(0003).
000862     05  TRUNCO PIC  X(0001).
000863*    -------------------------------
000864     05  FILLER            PIC  X(0003).
000865     05  PROD1O PIC  X(0001).
000866*    -------------------------------
000867     05  FILLER            PIC  X(0003).
000868     05  ATTAGE1O PIC  X(0002).
000869*    -------------------------------
000870     05  FILLER            PIC  X(0003).
000871     05  MINAGE1O PIC  X(0002).
000872*    -------------------------------
000873     05  FILLER            PIC  X(0003).
000874     05  MAXAGE1O PIC  X(0002).
000875*    -------------------------------
000876     05  FILLER            PIC  X(0003).
000877     05  MAXTRM1O PIC  X(0003).
000878*    -------------------------------
000879     05  FILLER            PIC  X(0003).
000880     05  BENPCT1O PIC  X(0004).
000881*    -------------------------------
000882     05  FILLER            PIC  X(0003).
000883     05  MAXAMT1O PIC  Z,ZZZ,ZZZ.
000884*    -------------------------------
000885     05  FILLER            PIC  X(0003).
000886     05  PRE1O PIC  X(0002).
000887*    -------------------------------
000888     05  FILLER            PIC  X(0003).
000889     05  EXCL1O PIC  X(0003).
000890*    -------------------------------
000891     05  FILLER            PIC  X(0003).
000892     05  COVEND1O PIC  X(0003).
000893*    -------------------------------
000894     05  FILLER            PIC  X(0003).
000895     05  ACCPER1O PIC  ZZ99.
000896*    -------------------------------
000897     05  FILLER            PIC  X(0003).
000898     05  CRITPD1O PIC  Z99.
000899*    -------------------------------
000900     05  FILLER            PIC  X(0003).
000901     05  RECCP1O PIC  X(0002).
000902*    -------------------------------
000903     05  FILLER            PIC  X(0003).
000904     05  RTW1O PIC  Z99.
000905*    -------------------------------
000906     05  FILLER            PIC  X(0003).
000907     05  MEXT1O PIC  Z99.
000908*    -------------------------------
000909     05  FILLER            PIC  X(0003).
000910     05  PROD2O PIC  X(0001).
000911*    -------------------------------
000912     05  FILLER            PIC  X(0003).
000913     05  ATTAGE2O PIC  X(0002).
000914*    -------------------------------
000915     05  FILLER            PIC  X(0003).
000916     05  MINAGE2O PIC  X(0002).
000917*    -------------------------------
000918     05  FILLER            PIC  X(0003).
000919     05  MAXAGE2O PIC  X(0002).
000920*    -------------------------------
000921     05  FILLER            PIC  X(0003).
000922     05  MAXTRM2O PIC  X(0003).
000923*    -------------------------------
000924     05  FILLER            PIC  X(0003).
000925     05  BENPCT2O PIC  X(0004).
000926*    -------------------------------
000927     05  FILLER            PIC  X(0003).
000928     05  MAXAMT2O PIC  Z,ZZZ,ZZZ.
000929*    -------------------------------
000930     05  FILLER            PIC  X(0003).
000931     05  PRE2O PIC  X(0002).
000932*    -------------------------------
000933     05  FILLER            PIC  X(0003).
000934     05  EXCL2O PIC  X(0003).
000935*    -------------------------------
000936     05  FILLER            PIC  X(0003).
000937     05  COVEND2O PIC  X(0003).
000938*    -------------------------------
000939     05  FILLER            PIC  X(0003).
000940     05  ACCPER2O PIC  ZZ99.
000941*    -------------------------------
000942     05  FILLER            PIC  X(0003).
000943     05  CRITPD2O PIC  Z99.
000944*    -------------------------------
000945     05  FILLER            PIC  X(0003).
000946     05  RECCP2O PIC  X(0002).
000947*    -------------------------------
000948     05  FILLER            PIC  X(0003).
000949     05  RTW2O PIC  Z99.
000950*    -------------------------------
000951     05  FILLER            PIC  X(0003).
000952     05  MEXT2O PIC  Z99.
000953*    -------------------------------
000954     05  FILLER            PIC  X(0003).
000955     05  PROD3O PIC  X(0001).
000956*    -------------------------------
000957     05  FILLER            PIC  X(0003).
000958     05  ATTAGE3O PIC  X(0002).
000959*    -------------------------------
000960     05  FILLER            PIC  X(0003).
000961     05  MINAGE3O PIC  X(0002).
000962*    -------------------------------
000963     05  FILLER            PIC  X(0003).
000964     05  MAXAGE3O PIC  X(0002).
000965*    -------------------------------
000966     05  FILLER            PIC  X(0003).
000967     05  MAXTRM3O PIC  X(0003).
000968*    -------------------------------
000969     05  FILLER            PIC  X(0003).
000970     05  BENPCT3O PIC  X(0004).
000971*    -------------------------------
000972     05  FILLER            PIC  X(0003).
000973     05  MAXAMT3O PIC  Z,ZZZ,ZZZ.
000974*    -------------------------------
000975     05  FILLER            PIC  X(0003).
000976     05  PRE3O PIC  X(0002).
000977*    -------------------------------
000978     05  FILLER            PIC  X(0003).
000979     05  EXCL3O PIC  X(0003).
000980*    -------------------------------
000981     05  FILLER            PIC  X(0003).
000982     05  COVEND3O PIC  X(0003).
000983*    -------------------------------
000984     05  FILLER            PIC  X(0003).
000985     05  ACCPER3O PIC  ZZ99.
000986*    -------------------------------
000987     05  FILLER            PIC  X(0003).
000988     05  CRITPD3O PIC  Z99.
000989*    -------------------------------
000990     05  FILLER            PIC  X(0003).
000991     05  RECCP3O PIC  X(0002).
000992*    -------------------------------
000993     05  FILLER            PIC  X(0003).
000994     05  RTW3O PIC  Z99.
000995*    -------------------------------
000996     05  FILLER            PIC  X(0003).
000997     05  MEXT3O PIC  Z99.
000998*    -------------------------------
000999     05  FILLER            PIC  X(0003).
001000     05  PROD4O PIC  X(0001).
001001*    -------------------------------
001002     05  FILLER            PIC  X(0003).
001003     05  ATTAGE4O PIC  X(0002).
001004*    -------------------------------
001005     05  FILLER            PIC  X(0003).
001006     05  MINAGE4O PIC  X(0002).
001007*    -------------------------------
001008     05  FILLER            PIC  X(0003).
001009     05  MAXAGE4O PIC  X(0002).
001010*    -------------------------------
001011     05  FILLER            PIC  X(0003).
001012     05  MAXTRM4O PIC  X(0003).
001013*    -------------------------------
001014     05  FILLER            PIC  X(0003).
001015     05  BENPCT4O PIC  X(0004).
001016*    -------------------------------
001017     05  FILLER            PIC  X(0003).
001018     05  MAXAMT4O PIC  Z,ZZZ,ZZZ.
001019*    -------------------------------
001020     05  FILLER            PIC  X(0003).
001021     05  PRE4O PIC  X(0002).
001022*    -------------------------------
001023     05  FILLER            PIC  X(0003).
001024     05  EXCL4O PIC  X(0003).
001025*    -------------------------------
001026     05  FILLER            PIC  X(0003).
001027     05  COVEND4O PIC  X(0003).
001028*    -------------------------------
001029     05  FILLER            PIC  X(0003).
001030     05  ACCPER4O PIC  ZZ99.
001031*    -------------------------------
001032     05  FILLER            PIC  X(0003).
001033     05  CRITPD4O PIC  Z99.
001034*    -------------------------------
001035     05  FILLER            PIC  X(0003).
001036     05  RECCP4O PIC  X(0002).
001037*    -------------------------------
001038     05  FILLER            PIC  X(0003).
001039     05  RTW4O PIC  Z99.
001040*    -------------------------------
001041     05  FILLER            PIC  X(0003).
001042     05  MEXT4O PIC  Z99.
001043*    -------------------------------
001044     05  FILLER            PIC  X(0003).
001045     05  PROD5O PIC  X(0001).
001046*    -------------------------------
001047     05  FILLER            PIC  X(0003).
001048     05  ATTAGE5O PIC  X(0002).
001049*    -------------------------------
001050     05  FILLER            PIC  X(0003).
001051     05  MINAGE5O PIC  X(0002).
001052*    -------------------------------
001053     05  FILLER            PIC  X(0003).
001054     05  MAXAGE5O PIC  X(0002).
001055*    -------------------------------
001056     05  FILLER            PIC  X(0003).
001057     05  MAXTRM5O PIC  X(0003).
001058*    -------------------------------
001059     05  FILLER            PIC  X(0003).
001060     05  BENPCT5O PIC  X(0004).
001061*    -------------------------------
001062     05  FILLER            PIC  X(0003).
001063     05  MAXAMT5O PIC  Z,ZZZ,ZZZ.
001064*    -------------------------------
001065     05  FILLER            PIC  X(0003).
001066     05  PRE5O PIC  X(0002).
001067*    -------------------------------
001068     05  FILLER            PIC  X(0003).
001069     05  EXCL5O PIC  X(0003).
001070*    -------------------------------
001071     05  FILLER            PIC  X(0003).
001072     05  COVEND5O PIC  X(0003).
001073*    -------------------------------
001074     05  FILLER            PIC  X(0003).
001075     05  ACCPER5O PIC  ZZ99.
001076*    -------------------------------
001077     05  FILLER            PIC  X(0003).
001078     05  CRITPD5O PIC  Z99.
001079*    -------------------------------
001080     05  FILLER            PIC  X(0003).
001081     05  RECCP5O PIC  X(0002).
001082*    -------------------------------
001083     05  FILLER            PIC  X(0003).
001084     05  RTW5O PIC  Z99.
001085*    -------------------------------
001086     05  FILLER            PIC  X(0003).
001087     05  MEXT5O PIC  Z99.
001088*    -------------------------------
001089     05  FILLER            PIC  X(0003).
001090     05  PROD6O PIC  X(0001).
001091*    -------------------------------
001092     05  FILLER            PIC  X(0003).
001093     05  ATTAGE6O PIC  X(0002).
001094*    -------------------------------
001095     05  FILLER            PIC  X(0003).
001096     05  MINAGE6O PIC  X(0002).
001097*    -------------------------------
001098     05  FILLER            PIC  X(0003).
001099     05  MAXAGE6O PIC  X(0002).
001100*    -------------------------------
001101     05  FILLER            PIC  X(0003).
001102     05  MAXTRM6O PIC  X(0003).
001103*    -------------------------------
001104     05  FILLER            PIC  X(0003).
001105     05  BENPCT6O PIC  X(0004).
001106*    -------------------------------
001107     05  FILLER            PIC  X(0003).
001108     05  MAXAMT6O PIC  Z,ZZZ,ZZZ.
001109*    -------------------------------
001110     05  FILLER            PIC  X(0003).
001111     05  PRE6O PIC  X(0002).
001112*    -------------------------------
001113     05  FILLER            PIC  X(0003).
001114     05  EXCL6O PIC  X(0003).
001115*    -------------------------------
001116     05  FILLER            PIC  X(0003).
001117     05  COVEND6O PIC  X(0003).
001118*    -------------------------------
001119     05  FILLER            PIC  X(0003).
001120     05  ACCPER6O PIC  ZZ99.
001121*    -------------------------------
001122     05  FILLER            PIC  X(0003).
001123     05  CRITPD6O PIC  Z99.
001124*    -------------------------------
001125     05  FILLER            PIC  X(0003).
001126     05  RECCP6O PIC  X(0002).
001127*    -------------------------------
001128     05  FILLER            PIC  X(0003).
001129     05  RTW6O PIC  Z99.
001130*    -------------------------------
001131     05  FILLER            PIC  X(0003).
001132     05  MEXT6O PIC  Z99.
001133*    -------------------------------
001134     05  FILLER            PIC  X(0003).
001135     05  PROD7O PIC  X(0001).
001136*    -------------------------------
001137     05  FILLER            PIC  X(0003).
001138     05  ATTAGE7O PIC  X(0002).
001139*    -------------------------------
001140     05  FILLER            PIC  X(0003).
001141     05  MINAGE7O PIC  X(0002).
001142*    -------------------------------
001143     05  FILLER            PIC  X(0003).
001144     05  MAXAGE7O PIC  X(0002).
001145*    -------------------------------
001146     05  FILLER            PIC  X(0003).
001147     05  MAXTRM7O PIC  X(0003).
001148*    -------------------------------
001149     05  FILLER            PIC  X(0003).
001150     05  BENPCT7O PIC  X(0004).
001151*    -------------------------------
001152     05  FILLER            PIC  X(0003).
001153     05  MAXAMT7O PIC  Z,ZZZ,ZZZ.
001154*    -------------------------------
001155     05  FILLER            PIC  X(0003).
001156     05  PRE7O PIC  X(0002).
001157*    -------------------------------
001158     05  FILLER            PIC  X(0003).
001159     05  EXCL7O PIC  X(0003).
001160*    -------------------------------
001161     05  FILLER            PIC  X(0003).
001162     05  COVEND7O PIC  X(0003).
001163*    -------------------------------
001164     05  FILLER            PIC  X(0003).
001165     05  ACCPER7O PIC  ZZ99.
001166*    -------------------------------
001167     05  FILLER            PIC  X(0003).
001168     05  CRITPD7O PIC  Z99.
001169*    -------------------------------
001170     05  FILLER            PIC  X(0003).
001171     05  RECCP17O PIC  X(0002).
001172*    -------------------------------
001173     05  FILLER            PIC  X(0003).
001174     05  RTW7O PIC  Z99.
001175*    -------------------------------
001176     05  FILLER            PIC  X(0003).
001177     05  MEXT7O PIC  Z99.
001178*    -------------------------------
001179     05  FILLER            PIC  X(0003).
001180     05  PROD8O PIC  X(0001).
001181*    -------------------------------
001182     05  FILLER            PIC  X(0003).
001183     05  ATTAGE8O PIC  X(0002).
001184*    -------------------------------
001185     05  FILLER            PIC  X(0003).
001186     05  MINAGE8O PIC  X(0002).
001187*    -------------------------------
001188     05  FILLER            PIC  X(0003).
001189     05  MAXAGE8O PIC  X(0002).
001190*    -------------------------------
001191     05  FILLER            PIC  X(0003).
001192     05  MAXTRM8O PIC  X(0003).
001193*    -------------------------------
001194     05  FILLER            PIC  X(0003).
001195     05  BENPCT8O PIC  X(0004).
001196*    -------------------------------
001197     05  FILLER            PIC  X(0003).
001198     05  MAXAMT8O PIC  Z,ZZZ,ZZZ.
001199*    -------------------------------
001200     05  FILLER            PIC  X(0003).
001201     05  PRE8O PIC  X(0002).
001202*    -------------------------------
001203     05  FILLER            PIC  X(0003).
001204     05  EXCL8O PIC  X(0003).
001205*    -------------------------------
001206     05  FILLER            PIC  X(0003).
001207     05  COVEND8O PIC  X(0003).
001208*    -------------------------------
001209     05  FILLER            PIC  X(0003).
001210     05  ACCPER8O PIC  ZZ99.
001211*    -------------------------------
001212     05  FILLER            PIC  X(0003).
001213     05  CRITPD8O PIC  Z99.
001214*    -------------------------------
001215     05  FILLER            PIC  X(0003).
001216     05  RECCP8O PIC  X(0002).
001217*    -------------------------------
001218     05  FILLER            PIC  X(0003).
001219     05  RTW8O PIC  Z99.
001220*    -------------------------------
001221     05  FILLER            PIC  X(0003).
001222     05  MEXT8O PIC  Z99.
001223*    -------------------------------
001224     05  FILLER            PIC  X(0003).
001225     05  ERRMSG1O PIC  X(0079).
001226*    -------------------------------
001227     05  FILLER            PIC  X(0003).
001228     05  PFKEYO PIC  99.
001229*    -------------------------------
      *<<((file: EL159S))
000292 01  EL159AI-R REDEFINES EL159AI.
000293     12  FILLER                      PIC X(177).
000294     12  AFA-LEN                     PIC S9(04)  COMP.
000295     12  AFA-ATTRB                   PIC X.
000296     12  AFA-AMT                     PIC X(6).
000297     12  AFA-AMT-IN REDEFINES AFA-AMT PIC 9(4)V99.
000298     12  AFA-AMT-OUT REDEFINES AFA-AMT
000299                                     PIC ZZ9.99.
000300     12  F                           PIC X(4).
000301     12  EL159-PROD-TABLE OCCURS 8.
000302         16  LINE-NBR-LEN            PIC S9(04)  COMP.
000303         16  LINE-NBR-ATTRB          PIC X(01).
000304         16  LINE-NBR                PIC X(03).
000305         16  PROD-CODE-LEN           PIC S9(04)  COMP.
000306         16  PROD-CODE-ATTRB         PIC X(01).
000307         16  PROD-CODE               PIC X.
000308         16  MAX-ATT-AGE-LEN         PIC S9(04)  COMP.
000309         16  MAX-ATT-AGE-ATTRB       PIC X(01).
000310         16  MAX-ATT-AGE             PIC 99.
000311*        16  MIN-AGE-LEN             PIC S9(04)  COMP.
000312*        16  MIN-AGE-ATTRB           PIC X(01).
000313*        16  MIN-AGE                 PIC 99.
000314         16  WAIT-PR-LEN             PIC S9(04)  COMP.
000315         16  WAIT-PR-ATTRB           PIC X(01).
000316         16  WAIT-PR                 PIC X(03).
000317         16  MAX-TERM-LEN            PIC S9(04)  COMP.
000318         16  MAX-TERM-ATTRB          PIC X(01).
000319         16  MAX-TERM                PIC 999.
000320         16  ben-pct-len             pic s9(04)  comp.
000321         16  ben-pct-attrb           pic x.
000322         16  ben-pct-in              pic v9(4).
000323         16  ben-pct-out redefines
000324             ben-pct-in              pic .999.
000325         16  MAX-AMT-LEN             PIC S9(04)  COMP.
000326         16  MAX-AMT-ATTRB           PIC X(01).
000327         16  MAX-AMT-IN              PIC 9(9).
000328         16  MAX-AMT-OUT REDEFINES
000329             MAX-AMT-IN              PIC Z,ZZZ,999.
000330         16  PRE-EXIST-LEN           PIC S9(04)  COMP.
000331         16  PRE-EXIST-ATTRB         PIC X(01).
000332         16  PRE-EXIST               PIC 99.
000333         16  EXCL-PER-LEN            PIC S9(04)  COMP.
000334         16  EXCL-PER-ATTRB          PIC X(01).
000335         16  EXCL-PERIOD             PIC 999.
000336         16  COV-ENDS-LEN            PIC S9(04)  COMP.
000337         16  COV-ENDS-ATTRB          PIC X(01).
000338         16  COV-ENDS                PIC 999.
000339         16  ACC-PER-ENDS-LEN        PIC S9(04)  COMP.
000340         16  ACC-PER-ENDS-ATTRB      PIC X(01).
000341         16  ACC-PER-ENDS            PIC 9999.
000342         16  ACC-PER-ENDS-OUT REDEFINES ACC-PER-ENDS
000343                                     PIC ZZ99.
000344         16  CRIT-PER-LEN            PIC S9(04)  COMP.
000345         16  CRIT-PER-ATTRB          PIC X(01).
000346         16  CRIT-PER                PIC 999.
000347         16  CRIT-PER-OUT REDEFINES CRIT-PER
000348                                     PIC Z99.
000349         16  REC-CP-LEN              PIC S9(04)  COMP.
000350         16  REC-CP-ATTRB            PIC X(01).
000351         16  REC-CRIT-PER            PIC XX.
000352         16  REC-CRIT-PER-N REDEFINES REC-CRIT-PER
000353                                     PIC 99.
000354         16  RTW-MOS-LEN             PIC S9(04)  COMP.
000355         16  RTW-MOS-ATTRB           PIC X(01).
000356         16  RTW-MOS                 PIC 999.
000357         16  RTW-MOS-OUT REDEFINES RTW-MOS
000358                                     PIC Z99.
000359         16  MAX-EXT-LEN             PIC S9(04)  COMP.
000360         16  MAX-EXT-ATTRB           PIC X(01).
000361         16  MAX-EXT                 PIC 999.
000362         16  MAX-EXT-OUT REDEFINES MAX-EXT
000363                                     PIC Z99.
000364
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
000366
000367 01  DFHCOMMAREA                     PIC X(1024).
000368
000369*                                    COPY ERCPDEF.
      *>>((file: ERCPDEF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPDEF.                            *
000005*                                                                *
000006*    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
000007*                                                                *
000008*    FILE TYPE = VSAM,KSDS                                       *
000009*    RECORD SIZE = 1319 RECFORM = FIXED                          *
000010*                                                                *
000011*    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
000012*                                                                *
000013*    LOG = YES                                                   *
000014*    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000024* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000025* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
000026* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000027******************************************************************
000028 01  PRODUCT-MASTER.
000029    12  PD-RECORD-ID                 PIC X(02).
000030        88  VALID-PD-ID                  VALUE 'PD'.
000031
000032    12  PD-CONTROL-PRIMARY.
000033        16  PD-COMPANY-CD            PIC X.
000034        16  PD-STATE                 PIC XX.
000035        16  PD-PRODUCT-CD            PIC XXX.
000036        16  PD-FILLER                PIC X(7).
000037        16  PD-BEN-TYPE              PIC X.
000038        16  PD-BEN-CODE              PIC XX.
000039        16  PD-PROD-EXP-DT           PIC XX.
000040
000041    12  FILLER                       PIC X(50).
000042
000043    12  PD-PRODUCT-DATA OCCURS 8.
000044        16  PD-PROD-CODE             PIC X.
000045            88  PD-PROD-LIFE           VALUE 'L'.
000046            88  PD-PROD-PROP           VALUE 'P'.
000047            88  PD-PROD-AH             VALUE 'A'.
000048            88  PD-PROD-IU             VALUE 'I'.
000049            88  PD-PROD-GAP            VALUE 'G'.
000050            88  PD-PROD-FAML           VALUE 'F'.
000051            88  PD-PROD-OTH            VALUE 'O'.
000052        16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
000053        16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
000054        16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
000055        16  PD-MAX-TERM              PIC S999        COMP-3.
000056        16  PD-MAX-AMT               PIC S9(07)      COMP-3.
000057        16  FILLER                   PIC X.
000058        16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
000059        16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
000060        16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
000061        16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
000062        16  PD-CRIT-PERIOD           PIC S999        COMP-3.
000063        16  PD-REC-CRIT-PERIOD       PIC 99.
000064        16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
000065            20  PD-RECURRING-YN      PIC X.
000066            20  FILLER               PIC X.
000067        16  PD-RTW-MOS               PIC 99.
000068        16  PD-MAX-EXTENSION         PIC 99.
000069        16  pd-ben-pct               pic sv999 comp-3.
000070*       16  FILLER                   PIC XX.
000071
000072    12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
000073
000074    12  PD-TERM-LIMITS OCCURS 15.
000075        16  PD-LOW-TERM              PIC S999        COMP-3.
000076        16  PD-HI-TERM               PIC S999        COMP-3.
000077
000078*  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
000079    12  PD-LOAN-AMT-LIMITS OCCURS 15.
000080        16  PD-LOW-AMT               PIC S9(5)       COMP-3.
000081        16  PD-HI-AMT                PIC S9(7)       COMP-3.
000082
000083    12  PD-EARN-FACTORS.
000084        16  FILLER OCCURS 15.
000085            20  FILLER OCCURS 15.
000086                24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
000087
000088    12  PD-PRODUCT-DESC              PIC X(80).
000089    12  PD-TRUNCATED                 PIC X.
000090    12  FILLER                       PIC X(59).
000091
000092    12  PD-MAINT-INFORMATION.
000093        16  PD-LAST-MAINT-DT         PIC X(02).
000094        16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
000095        16  PD-LAST-MAINT-BY         PIC X(04).
      *<<((file: ERCPDEF))
000370
000371*                                    COPY ELCCNTL.
      *>>((file: ELCCNTL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCNTL.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.059                          *
000007*                                                                *
000008*   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 750  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 082503                   PEMA  ADD BENEFIT GROUP
000027* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000028* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000029* 092705    2005050300006  PEMA  ADD SPP LEASES
000030* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
000031* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
000032* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
000033* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
000034* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
000035* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000036* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
000037* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
000038* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000039* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000040* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
000041******************************************************************
000042*
000043 01  CONTROL-FILE.
000044     12  CF-RECORD-ID                       PIC XX.
000045         88  VALID-CF-ID                        VALUE 'CF'.
000046
000047     12  CF-CONTROL-PRIMARY.
000048         16  CF-COMPANY-ID                  PIC XXX.
000049         16  CF-RECORD-TYPE                 PIC X.
000050             88  CF-COMPANY-MASTER              VALUE '1'.
000051             88  CF-PROCESSOR-MASTER            VALUE '2'.
000052             88  CF-STATE-MASTER                VALUE '3'.
000053             88  CF-LF-BENEFIT-MASTER           VALUE '4'.
000054             88  CF-AH-BENEFIT-MASTER           VALUE '5'.
000055             88  CF-CARRIER-MASTER              VALUE '6'.
000056             88  CF-MORTALITY-MASTER            VALUE '7'.
000057             88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
000058             88  CF-TERMINAL-MASTER             VALUE '9'.
000059             88  CF-AH-EDIT-MASTER              VALUE 'A'.
000060             88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
000061             88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
000062             88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
000063             88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
000064             88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
000065             88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
000066             88  CF-REMINDERS-MASTER            VALUE 'R'.
000067             88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
000068         16  CF-ACCESS-CD-GENL              PIC X(4).
000069         16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
000070             20  CF-PROCESSOR               PIC X(4).
000071         16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
000072             20  CF-STATE-CODE              PIC XX.
000073             20  FILLER                     PIC XX.
000074         16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
000075             20  FILLER                     PIC XX.
000076             20  CF-HI-BEN-IN-REC           PIC XX.
000077         16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
000078             20  FILLER                     PIC XXX.
000079             20  CF-CARRIER-CNTL            PIC X.
000080         16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
000081             20  FILLER                     PIC XX.
000082             20  CF-HI-TYPE-IN-REC          PIC 99.
000083         16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
000084             20  CF-CRDB-TABLE-INDICATOR    PIC X.
000085                 88  CF-CRDB-NAIC-TABLE         VALUE '9'.
000086             20  CF-CRDB-BENEFIT-TYPE       PIC X.
000087             20  CF-CRDB-WAITING-PERIOD     PIC XX.
000088         16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
000089             20  FILLER                     PIC X.
000090             20  CF-CUSTOM-REPORT-NO        PIC 999.
000091         16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
000092             20  FILLER                     PIC XX.
000093             20  CF-MORTGAGE-PLAN           PIC XX.
000094         16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
000095
000096     12  CF-LAST-MAINT-DT                   PIC XX.
000097     12  CF-LAST-MAINT-BY                   PIC X(4).
000098     12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
000099
000100     12  CF-RECORD-BODY                     PIC X(728).
000101
000102
000103****************************************************************
000104*             COMPANY MASTER RECORD                            *
000105****************************************************************
000106
000107     12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000108         16  CF-COMPANY-ADDRESS.
000109             20  CF-CL-MAIL-TO-NAME         PIC X(30).
000110             20  CF-CL-IN-CARE-OF           PIC X(30).
000111             20  CF-CL-ADDR-LINE-1          PIC X(30).
000112             20  CF-CL-ADDR-LINE-2          PIC X(30).
000113             20  CF-CL-CITY-STATE           PIC X(30).
000114             20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
000115             20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
000116         16  CF-COMPANY-CD                  PIC X.
000117         16  CF-COMPANY-PASSWORD            PIC X(8).
000118         16  CF-SECURITY-OPTION             PIC X.
000119             88  ALL-SECURITY                   VALUE '1'.
000120             88  COMPANY-VERIFY                 VALUE '2'.
000121             88  PROCESSOR-VERIFY               VALUE '3'.
000122             88  NO-SECURITY                    VALUE '4'.
000123             88  ALL-BUT-TERM                   VALUE '5'.
000124         16  CF-CARRIER-CONTROL-LEVEL       PIC X.
000125             88  USE-ACTUAL-CARRIER             VALUE SPACE.
000126         16  CF-LGX-INTERFACE-CNTL          PIC X.
000127             88  LGX-TIME-SHR-COMPANY           VALUE '1'.
000128         16  CF-INFORCE-LOCATION            PIC X.
000129             88  CERTS-ARE-ONLINE               VALUE '1'.
000130             88  CERTS-ARE-OFFLINE              VALUE '2'.
000131             88  NO-CERTS-AVAILABLE             VALUE '3'.
000132         16  CF-LOWER-CASE-LETTERS          PIC X.
000133         16  CF-CERT-ACCESS-CONTROL         PIC X.
000134             88  CF-ST-ACCNT-CNTL               VALUE ' '.
000135             88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000136             88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
000137             88  CF-ACCNT-CNTL                  VALUE '3'.
000138             88  CF-CARR-ACCNT-CNTL             VALUE '4'.
000139
000140         16  CF-FORMS-PRINTER-ID            PIC X(4).
000141         16  CF-CHECK-PRINTER-ID            PIC X(4).
000142
000143         16  CF-LGX-CREDIT-USER             PIC X.
000144             88  CO-IS-NOT-USER                 VALUE 'N'.
000145             88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
000146
000147         16 CF-CREDIT-CALC-CODES.
000148             20  CF-CR-REM-TERM-CALC PIC X.
000149               88  CR-EARN-AFTER-15TH           VALUE '1'.
000150               88  CR-EARN-ON-HALF-MO           VALUE '2'.
000151               88  CR-EARN-ON-1ST-DAY           VALUE '3'.
000152               88  CR-EARN-ON-FULL-MO           VALUE '4'.
000153               88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
000154               88  CR-EARN-AFTER-14TH           VALUE '6'.
000155               88  CR-EARN-AFTER-16TH           VALUE '7'.
000156             20  CF-CR-R78-METHOD           PIC X.
000157               88  USE-TERM-PLUS-ONE            VALUE SPACE.
000158               88  DONT-USE-PLUS-ONE            VALUE '1'.
000159
000160         16  CF-CLAIM-CONTROL-COUNTS.
000161             20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
000162                 88  CO-CLM-COUNT-RESET         VALUE +99999.
000163
000164             20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
000165                 88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
000166
000167             20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
000168                 88  CO-CHECK-COUNT-RESET       VALUE +9999999.
000169
000170             20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
000171                 88  CO-QUE-COUNT-RESET         VALUE +9999999.
000172
000173         16  CF-CURRENT-MONTH-END           PIC XX.
000174
000175         16  CF-CO-CALC-QUOTE-TOLERANCE.
000176             20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
000177             20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
000178             20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
000179             20  CF-CO-CLAIM-REJECT-SW      PIC X.
000180                 88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
000181                 88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
000182             20  CF-CO-PREM-REJECT-SW       PIC X.
000183                 88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
000184                 88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
000185             20  CF-CO-REF-REJECT-SW        PIC X.
000186                 88 CO-WARN-IF-REF-OUT          VALUE SPACE.
000187                 88 CO-FORCE-IF-REF-OUT         VALUE '1'.
000188
000189         16  CF-CO-REPORTING-DT             PIC XX.
000190         16  CF-CO-REPORTING-MONTH-DT       PIC XX.
000191         16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
000192           88  CF-CO-NOT-MONTH-END              VALUE SPACES.
000193           88  CF-CO-MONTH-END                  VALUE '1'.
000194
000195         16  CF-LGX-CLAIM-USER              PIC X.
000196             88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
000197             88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
000198
000199         16  CF-CREDIT-EDIT-CONTROLS.
000200             20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
000201             20  CF-MIN-AGE                 PIC 99.
000202             20  CF-DEFAULT-AGE             PIC 99.
000203             20  CF-MIN-TERM                PIC S999      COMP-3.
000204             20  CF-MAX-TERM                PIC S999      COMP-3.
000205             20  CF-DEFAULT-SEX             PIC X.
000206             20  CF-JOINT-AGE-INPUT         PIC X.
000207                 88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
000208             20  CF-BIRTH-DATE-INPUT        PIC X.
000209                 88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
000210             20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
000211                 88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
000212                 88  CF-ZERO-CARRIER            VALUE '1'.
000213                 88  CF-ZERO-GROUPING           VALUE '2'.
000214                 88  CF-ZERO-CAR-GROUP          VALUE '3'.
000215             20  CF-EDIT-SW                 PIC X.
000216                 88  CF-START-EDIT-TONIGHT      VALUE '1'.
000217             20  CF-EDIT-RESTART-BATCH      PIC X(6).
000218             20  CF-CR-PR-METHOD            PIC X.
000219               88  USE-NORMAL-PR-METHOD         VALUE SPACE.
000220               88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
000221             20  FILLER                     PIC X.
000222
000223         16  CF-CREDIT-MISC-CONTROLS.
000224             20  CF-REIN-TABLE-SW           PIC X.
000225                 88 REIN-TABLES-ARE-USED        VALUE '1'.
000226             20  CF-COMP-TABLE-SW           PIC X.
000227                 88 COMP-TABLES-ARE-USED        VALUE '1'.
000228             20  CF-EXPERIENCE-RETENTION-AGE
000229                                            PIC S9        COMP-3.
000230             20  CF-CONVERSION-DT           PIC XX.
000231             20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
000232             20  CF-RUN-FREQUENCY-SW        PIC X.
000233                 88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
000234                 88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
000235
000236             20  CF-CR-CHECK-NO-CONTROL.
000237                 24  CF-CR-CHECK-NO-METHOD    PIC X.
000238                     88  CR-CHECK-NO-MANUAL       VALUE '1'.
000239                     88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
000240                     88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
000241                 24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
000242                     88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
000243
000244                 24  CF-CR-CHECK-COUNT       REDEFINES
000245                     CF-CR-CHECK-COUNTER      PIC X(4).
000246
000247                 24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
000248                     88  CR-QUE-COUNT-RESET      VALUE +9999999.
000249
000250                 24  CF-CR-CHECK-QUE-COUNT   REDEFINES
000251                     CF-CR-CHECK-QUE-COUNTER  PIC X(4).
000252                 24  CF-MAIL-PROCESSING       PIC X.
000253                     88  MAIL-PROCESSING          VALUE 'Y'.
000254
000255         16  CF-MISC-SYSTEM-CONTROL.
000256             20  CF-SYSTEM-C                 PIC X.
000257                 88  CONFIRMATION-SYS-USED       VALUE '1'.
000258             20  CF-SYSTEM-D                 PIC X.
000259                 88  DAILY-BILL-SYS-USED         VALUE '1'.
000260             20  CF-SOC-SEC-NO-SW            PIC X.
000261                 88  SOC-SEC-NO-USED             VALUE '1'.
000262             20  CF-MEMBER-NO-SW             PIC X.
000263                 88  MEMBER-NO-USED              VALUE '1'.
000264             20  CF-TAX-ID-NUMBER            PIC X(11).
000265             20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
000266             20  CF-PAYMENT-APPROVAL-SW      PIC X.
000267                 88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
000268                 88  CF-NO-APPROVAL              VALUE ' ' 'N'.
000269                 88  CF-ALL-APPROVED             VALUE 'Y'.
000270                 88  CF-GRADUATED-APPROVAL       VALUE 'G'.
000271             20  CF-SYSTEM-E                 PIC X.
000272                 88  CF-AR-SYSTEM-USED           VALUE 'Y'.
000273
000274         16  CF-LGX-LIFE-USER               PIC X.
000275             88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
000276             88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
000277
000278         16  CF-CR-MONTH-END-DT             PIC XX.
000279
000280         16  CF-FILE-MAINT-DATES.
000281             20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
000282                 88  CF-LAST-BATCH-RESET        VALUE +999999.
000283             20  CF-LAST-BATCH       REDEFINES
000284                 CF-LAST-BATCH-NO               PIC X(4).
000285             20  CF-RATES-FILE-MAINT-DT         PIC XX.
000286             20  CF-RATES-FILE-CREATE-DT        PIC XX.
000287             20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
000288             20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
000289             20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
000290             20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
000291             20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
000292             20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
000293             20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
000294             20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
000295
000296         16  CF-NEXT-COMPANY-ID             PIC XXX.
000297         16  FILLER                         PIC X.
000298
000299         16  CF-ALT-MORT-CODE               PIC X(4).
000300         16  CF-MEMBER-CAPTION              PIC X(10).
000301
000302         16  CF-LIFE-ACCESS-CONTROL         PIC X.
000303             88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
000304             88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
000305             88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
000306             88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
000307             88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
000308
000309         16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
000310
000311         16  CF-LIFE-OVERRIDE-L1            PIC X.
000312         16  CF-LIFE-OVERRIDE-L2            PIC XX.
000313         16  CF-LIFE-OVERRIDE-L6            PIC X(6).
000314         16  CF-LIFE-OVERRIDE-L12           PIC X(12).
000315
000316         16  CF-AH-OVERRIDE-L1              PIC X.
000317         16  CF-AH-OVERRIDE-L2              PIC XX.
000318         16  CF-AH-OVERRIDE-L6              PIC X(6).
000319         16  CF-AH-OVERRIDE-L12             PIC X(12).
000320
000321         16  CF-REPORT-CD1-CAPTION          PIC X(10).
000322         16  CF-REPORT-CD2-CAPTION          PIC X(10).
000323
000324         16  CF-CLAIM-CUTOFF-DATE           PIC XX.
000325         16  CF-AR-LAST-EL860-DT            PIC XX.
000326         16  CF-MP-MONTH-END-DT             PIC XX.
000327
000328         16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
000329         16  CF-CLAIM-PAID-THRU-TO          PIC X.
000330             88  CF-CLAIM-PAID-TO               VALUE '1'.
000331
000332         16  CF-AR-MONTH-END-DT             PIC XX.
000333
000334         16  CF-CRDTCRD-USER                PIC X.
000335             88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
000336             88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
000337
000338         16  CF-CC-MONTH-END-DT             PIC XX.
000339
000340         16  CF-PRINT-ADDRESS-LABELS        PIC X.
000341
000342         16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
000343             88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
000344             88  CF-USE-ALL-AGE-LAST            VALUE '2'.
000345             88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
000346         16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
000347         16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
000348         16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
000349         16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
000350             88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
000351             88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
000352         16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
000353         16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
000354         16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
000355             88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
000356         16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
000357
000358         16  CF-CL-ZIP-CODE.
000359             20  CF-CL-ZIP-PRIME.
000360                 24  CF-CL-ZIP-1ST          PIC X.
000361                     88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
000362                 24  FILLER                 PIC X(4).
000363             20  CF-CL-ZIP-PLUS4            PIC X(4).
000364         16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
000365             20  CF-CL-CAN-POSTAL-1         PIC XXX.
000366             20  CF-CL-CAN-POSTAL-2         PIC XXX.
000367             20  FILLER                     PIC XXX.
000368
000369         16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
000370         16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
000371         16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
000372         16  CF-CO-OPTION-START-DATE        PIC XX.
000373         16  CF-REM-TRM-CALC-OPTION         PIC X.
000374           88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
000375                                                      '3' '4'.
000376           88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
000377           88  CF-30-DAY-MONTH                  VALUE '1' '3'.
000378           88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
000379           88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
000380           88  CF-EXT-30-DAY-MONTH              VALUE '3'.
000381           88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
000382
000383         16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
000384
000385         16  CF-PAYMENT-APPROVAL-LEVELS.
000386             20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
000387             20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
000388             20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
000389             20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
000390             20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
000391             20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
000392
000393         16  CF-END-USER-REPORTING-USER     PIC X.
000394             88  CO-NO-END-USER-REPORTING       VALUE 'N'.
000395             88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
000396
000397         16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
000398             88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
000399             88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
000400
000401         16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
000402
000403         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
000404         16  FILLER                         PIC X.
000405
000406         16  CF-CREDIT-ARCHIVE-CNTL.
000407             20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
000408             20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
000409             20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
000410
000411         16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
000412
000413         16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
000414             88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
000415             88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
000416
000417         16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
000418             88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
000419             88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
000420
000421         16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
000422
000423         16  CF-CO-ACH-ID-CODE              PIC  X.
000424             88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
000425             88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
000426             88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
000427         16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
000428         16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
000429         16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
000430         16  CF-CO-ACH-ADMIN-NO             PIC X(09).
000431         16  CF-CO-ACH-RECV-NAME            PIC X(23).
000432         16  CF-CO-ACH-RECV-NO              PIC X(08).
000433         16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
000434         16  CF-CO-ACH-COMPANY-ID           PIC X(09).
000435         16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
000436                 88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
000437         16  CF-CO-ACH-TRACE-SPACE REDEFINES
000438                 CF-CO-ACH-TRACE-NO         PIC X(4).
000439
000440         16  CF-CO-OVER-SHORT.
000441             20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
000442             20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000443
000444*         16  FILLER                         PIC X(102).
000445         16  CF-PAYMENT-APPROVAL-LEVELS-2.
000446             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
000447             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
000448
000449         16  CF-AH-APPROVAL-DAYS.
000450             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
000451             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
000452             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
000453             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
000454
000455         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
000456
000457         16  CF-APPROV-LEV-5.
000458             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
000459             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
000460             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
000461
000462         16  FILLER                         PIC X(68).
000463****************************************************************
000464*             PROCESSOR/USER RECORD                            *
000465****************************************************************
000466
000467     12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000468         16  CF-PROCESSOR-NAME              PIC X(30).
000469         16  CF-PROCESSOR-PASSWORD          PIC X(11).
000470         16  CF-PROCESSOR-TITLE             PIC X(26).
000471         16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
000472                 88  MESSAGE-YES                VALUE 'Y'.
000473                 88  MESSAGE-NO                 VALUE ' ' 'N'.
000474
000475*****************************************************
000476****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
000477****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
000478****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
000479****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
000480*****************************************************
000481
000482         16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
000483             20  CF-ADMINISTRATION-CONTROLS PIC XX.
000484             20  CF-APPLICATION-FORCE       PIC X.
000485             20  CF-INDIVIDUAL-APP.
000486                 24  CF-APP-SWITCHES  OCCURS  44 TIMES.
000487                     28  CF-BROWSE-APP      PIC X.
000488                     28  CF-UPDATE-APP      PIC X.
000489
000490         16  CF-CURRENT-TERM-ON             PIC X(4).
000491         16  CF-PROCESSOR-LIMITS-CLAIMS.
000492             20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
000493             20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
000494             20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
000495             20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
000496             20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
000497             20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
000498             20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
000499         16  CF-PROCESSOR-CARRIER           PIC X.
000500             88  NO-CARRIER-SECURITY            VALUE ' '.
000501         16  CF-PROCESSOR-ACCOUNT           PIC X(10).
000502             88  NO-ACCOUNT-SECURITY            VALUE SPACES.
000503         16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
000504             88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
000505         16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
000506             88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
000507
000508         16  CF-PROC-SYS-ACCESS-SW.
000509             20  CF-PROC-CREDIT-CLAIMS-SW.
000510                 24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
000511                     88  ACCESS-TO-CREDIT           VALUE 'Y'.
000512                 24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
000513                     88  ACCESS-TO-CLAIMS           VALUE 'Y'.
000514             20  CF-PROC-CREDIT-CLAIMS   REDEFINES
000515                 CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
000516                 88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
000517             20  CF-PROC-LIFE-GNRLDGR-SW.
000518                 24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
000519                     88  ACCESS-TO-LIFE             VALUE 'Y'.
000520                 24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
000521                     88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
000522             20  CF-PROC-LIFE-GNRLDGR    REDEFINES
000523                 CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
000524                 88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
000525         16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
000526             CF-PROC-SYS-ACCESS-SW              PIC X(4).
000527             88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
000528         16  CF-PROCESSOR-PRINTER               PIC X(4).
000529
000530         16  CF-APPROVAL-LEVEL                  PIC X.
000531             88  APPROVAL-LEVEL-1                   VALUE '1'.
000532             88  APPROVAL-LEVEL-2                   VALUE '2'.
000533             88  APPROVAL-LEVEL-3                   VALUE '3'.
000534             88  APPROVAL-LEVEL-4                   VALUE '4'.
000535             88  APPROVAL-LEVEL-5                   VALUE '5'.
000536
000537         16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
000538
000539         16  CF-LANGUAGE-TYPE                   PIC X.
000540             88  CF-LANG-IS-ENG                     VALUE 'E'.
000541             88  CF-LANG-IS-FR                      VALUE 'F'.
000542
000543         16  CF-CSR-IND                         PIC X.
000544         16  FILLER                             PIC X(239).
000545
000546****************************************************************
000547*             PROCESSOR/REMINDERS RECORD                       *
000548****************************************************************
000549
000550     12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
000551         16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
000552             20  CF-START-REMIND-DT         PIC XX.
000553             20  CF-END-REMIND-DT           PIC XX.
000554             20  CF-REMINDER-TEXT           PIC X(50).
000555         16  FILLER                         PIC X(296).
000556
000557
000558****************************************************************
000559*             STATE MASTER RECORD                              *
000560****************************************************************
000561
000562     12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000563         16  CF-STATE-ABBREVIATION          PIC XX.
000564         16  CF-STATE-NAME                  PIC X(25).
000565         16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
000566         16  CF-ST-CALC-QUOTE-TOLERANCE.
000567             20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
000568             20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
000569             20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
000570             20  CF-ST-CLAIM-REJECT-SW      PIC X.
000571                 88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
000572                 88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
000573             20  CF-ST-PREM-REJECT-SW       PIC X.
000574                 88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
000575                 88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
000576             20  CF-ST-REF-REJECT-SW        PIC X.
000577                 88 ST-WARN-IF-REF-OUT          VALUE SPACE.
000578                 88 ST-FORCE-IF-REF-OUT         VALUE '1'.
000579         16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
000580         16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
000581         16  CF-ST-REFUND-RULES.
000582             20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
000583             20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
000584             20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
000585         16  CF-ST-FST-PMT-EXTENSION.
000586             20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
000587             20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
000588                 88  CF-ST-EXT-NO-CHG           VALUE ' '.
000589                 88  CF-ST-EXT-CHG-LF           VALUE '1'.
000590                 88  CF-ST-EXT-CHG-AH           VALUE '2'.
000591                 88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
000592         16  CF-ST-STATE-CALL.
000593             20  CF-ST-CALL-UNEARNED        PIC X.
000594             20  CF-ST-CALL-RPT-CNTL        PIC X.
000595             20  CF-ST-CALL-RATE-DEV        PIC XXX.
000596         16  CF-REPLACEMENT-LAW-SW          PIC X.
000597             88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
000598             88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
000599         16  CF-REPLACEMENT-LETTER          PIC X(4).
000600         16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
000601         16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
000602         16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
000603         16  CF-ST-SPLIT-PAYMENT            PIC X.
000604         16  FILLER                         PIC X.
000605         16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
000606             20  CF-ST-BENEFIT-CD           PIC XX.
000607             20  CF-ST-BENEFIT-KIND         PIC X.
000608                 88  CF-ST-LIFE-KIND            VALUE 'L'.
000609                 88  CF-ST-AH-KIND              VALUE 'A'.
000610             20  CF-ST-REM-TERM-CALC        PIC X.
000611                 88  ST-REM-TERM-NOT-USED       VALUE SPACE.
000612                 88  ST-EARN-AFTER-15TH         VALUE '1'.
000613                 88  ST-EARN-ON-HALF-MO         VALUE '2'.
000614                 88  ST-EARN-ON-1ST-DAY         VALUE '3'.
000615                 88  ST-EARN-ON-FULL-MO         VALUE '4'.
000616                 88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
000617                 88  ST-EARN-AFTER-14TH         VALUE '6'.
000618                 88  ST-EARN-AFTER-16TH         VALUE '7'.
000619
000620             20  CF-ST-REFUND-CALC          PIC X.
000621                 88  ST-REFUND-NOT-USED         VALUE SPACE.
000622                 88  ST-REFD-BY-R78             VALUE '1'.
000623                 88  ST-REFD-BY-PRO-RATA        VALUE '2'.
000624                 88  ST-REFD-AS-CALIF           VALUE '3'.
000625                 88  ST-REFD-AS-TEXAS           VALUE '4'.
000626                 88  ST-REFD-IS-NET-PAY         VALUE '5'.
000627                 88  ST-REFD-ANTICIPATION       VALUE '6'.
000628                 88  ST-REFD-UTAH               VALUE '7'.
000629                 88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
000630                 88  ST-REFD-REG-BALLOON        VALUE 'B'.
000631                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
000632
000633             20  CF-ST-EARNING-CALC         PIC X.
000634                 88  ST-EARNING-NOT-USED        VALUE SPACE.
000635                 88  ST-EARN-BY-R78             VALUE '1'.
000636                 88  ST-EARN-BY-PRO-RATA        VALUE '2'.
000637                 88  ST-EARN-AS-CALIF           VALUE '3'.
000638                 88  ST-EARN-AS-TEXAS           VALUE '4'.
000639                 88  ST-EARN-IS-NET-PAY         VALUE '5'.
000640                 88  ST-EARN-ANTICIPATION       VALUE '6'.
000641                 88  ST-EARN-MEAN               VALUE '8'.
000642                 88  ST-EARN-REG-BALLOON        VALUE 'B'.
000643
000644             20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
000645                 88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
000646                 88  ST-OVRD-BY-R78             VALUE '1'.
000647                 88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
000648                 88  ST-OVRD-AS-CALIF           VALUE '3'.
000649                 88  ST-OVRD-AS-TEXAS           VALUE '4'.
000650                 88  ST-OVRD-IS-NET-PAY         VALUE '5'.
000651                 88  ST-OVRD-ANTICIPATION       VALUE '6'.
000652                 88  ST-OVRD-MEAN               VALUE '8'.
000653                 88  ST-OVRD-REG-BALLOON        VALUE 'B'.
000654             20  cf-st-extra-periods        pic 9.
000655*            20  FILLER                     PIC X.
000656
000657         16  CF-ST-COMMISSION-CAPS.
000658             20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
000659             20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
000660             20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
000661             20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
000662         16  CF-COMM-CAP-LIMIT-TO           PIC X.
000663                 88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
000664                 88  ST-LIMIT-TO-GA             VALUE 'G'.
000665                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
000666
000667         16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
000668
000669         16  CF-ST-STATUTORY-INTEREST.
000670             20  CF-ST-STAT-DATE-FROM       PIC X.
000671                 88  ST-STAT-FROM-INCURRED      VALUE 'I'.
000672                 88  ST-STAT-FROM-REPORTED      VALUE 'R'.
000673             20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
000674             20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
000675             20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
000676             20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
000677             20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
000678
000679         16  CF-ST-OVER-SHORT.
000680             20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
000681             20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
000682
000683         16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
000684
000685         16  CF-ST-RT-CALC                  PIC X.
000686
000687         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
000688         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
000689         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
000690         16  CF-ST-RF-LR-CALC               PIC X.
000691         16  CF-ST-RF-LL-CALC               PIC X.
000692         16  CF-ST-RF-LN-CALC               PIC X.
000693         16  CF-ST-RF-AH-CALC               PIC X.
000694         16  CF-ST-RF-CP-CALC               PIC X.
000695*        16  FILLER                         PIC X(206).
000696*CIDMOD         16  FILLER                         PIC X(192).
000697         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
000698             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
000699         16  CF-ST-REF-AH-DEATH-IND         PIC X.
000700         16  CF-ST-VFY-2ND-BENE             PIC X.
000701         16  CF-ST-CAUSAL-STATE             PIC X.
000702         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
000703         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
000704         16  CF-ST-AGENT-SIG-EDIT           PIC X.
000705             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
000706         16  CF-ST-NET-ONLY-STATE           PIC X.
000707             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
000708         16  cf-commission-cap-required     pic x.
000709         16  CF-ST-GA-COMMISSION-CAPS.
000710             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
000711             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
000712             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
000713             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
000714         16  CF-ST-TOT-COMMISSION-CAPS.
000715             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
000716             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
000717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
000718             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
000719         16  FILLER                         PIC X(156).
000720
000721****************************************************************
000722*             BENEFIT MASTER RECORD                            *
000723****************************************************************
000724
000725     12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000726         16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
000727             20  CF-BENEFIT-CODE            PIC XX.
000728             20  CF-BENEFIT-NUMERIC  REDEFINES
000729                 CF-BENEFIT-CODE            PIC XX.
000730             20  CF-BENEFIT-ALPHA           PIC XXX.
000731             20  CF-BENEFIT-DESCRIP         PIC X(10).
000732             20  CF-BENEFIT-COMMENT         PIC X(10).
000733
000734             20  CF-LF-COVERAGE-TYPE        PIC X.
000735                 88  CF-REDUCING                VALUE 'R'.
000736                 88  CF-LEVEL                   VALUE 'L' 'P'.
000737
000738             20  CF-SPECIAL-CALC-CD         PIC X.
000739                 88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
000740                 88  CF-NP-0-MO-INT             VALUE 'A'.
000741                 88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
000742                 88  CF-CRITICAL-PERIOD         VALUE 'C'.
000743                 88  CF-TERM-IN-DAYS            VALUE 'D'.
000744                 88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
000745                 88  CF-FARM-PLAN               VALUE 'F'.
000746                 88  CF-RATE-AS-STANDARD        VALUE 'G'.
000747                 88  CF-2-MTH-INTEREST          VALUE 'I'.
000748                 88  CF-3-MTH-INTEREST          VALUE 'J'.
000749                 88  CF-4-MTH-INTEREST          VALUE 'K'.
000750                 88  CF-BALLOON-LAST-PMT        VALUE 'L'.
000751                 88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
000752                 88  CF-PRUDENTIAL              VALUE 'P'.
000753                 88  CF-OUTSTANDING-BAL         VALUE 'O'.
000754                 88  CF-TRUNCATED-LIFE          VALUE 'T'.
000755                 88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
000756                 88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
000757                 88  CF-NET-PAY-SIMPLE          VALUE 'S'.
000758                 88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
000759
000760             20  CF-JOINT-INDICATOR         PIC X.
000761                 88  CF-JOINT-COVERAGE          VALUE 'J'.
000762
000763*            20  FILLER                     PIC X(12).
000764             20  cf-maximum-benefits        pic s999 comp-3.
000765             20  FILLER                     PIC X(09).
000766             20  CF-BENEFIT-CATEGORY        PIC X.
000767             20  CF-LOAN-TYPE               PIC X(8).
000768
000769             20  CF-CO-REM-TERM-CALC        PIC X.
000770                 88  CO-EARN-AFTER-15TH         VALUE '1'.
000771                 88  CO-EARN-ON-HALF-MO         VALUE '2'.
000772                 88  CO-EARN-ON-1ST-DAY         VALUE '3'.
000773                 88  CO-EARN-ON-FULL-MO         VALUE '4'.
000774                 88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
000775
000776             20  CF-CO-EARNINGS-CALC        PIC X.
000777                 88  CO-EARN-BY-R78             VALUE '1'.
000778                 88  CO-EARN-BY-PRO-RATA        VALUE '2'.
000779                 88  CO-EARN-AS-CALIF           VALUE '3'.
000780                 88  CO-EARN-AS-TEXAS           VALUE '4'.
000781                 88  CO-EARN-IS-NET-PAY         VALUE '5'.
000782                 88  CO-EARN-ANTICIPATION       VALUE '6'.
000783                 88  CO-EARN-AS-MEAN            VALUE '8'.
000784                 88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
000785
000786             20  CF-CO-REFUND-CALC          PIC X.
000787                 88  CO-REFUND-NOT-USED         VALUE SPACE.
000788                 88  CO-REFD-BY-R78             VALUE '1'.
000789                 88  CO-REFD-BY-PRO-RATA        VALUE '2'.
000790                 88  CO-REFD-AS-CALIF           VALUE '3'.
000791                 88  CO-REFD-AS-TEXAS           VALUE '4'.
000792                 88  CO-REFD-IS-NET-PAY         VALUE '5'.
000793                 88  CO-REFD-ANTICIPATION       VALUE '6'.
000794                 88  CO-REFD-MEAN               VALUE '8'.
000795                 88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
000796                 88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
000797                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
000798
000799             20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
000800                 88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
000801                 88  CO-OVRD-BY-R78             VALUE '1'.
000802                 88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
000803                 88  CO-OVRD-AS-CALIF           VALUE '3'.
000804                 88  CO-OVRD-AS-TEXAS           VALUE '4'.
000805                 88  CO-OVRD-IS-NET-PAY         VALUE '5'.
000806                 88  CO-OVRD-ANTICIPATION       VALUE '6'.
000807                 88  CO-OVRD-MEAN               VALUE '8'.
000808                 88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
000809
000810             20  CF-CO-BEN-I-G-CD           PIC X.
000811                 88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
000812                 88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
000813                 88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
000814
000815         16  FILLER                         PIC X(304).
000816
000817
000818****************************************************************
000819*             CARRIER MASTER RECORD                            *
000820****************************************************************
000821
000822     12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000823         16  CF-ADDRESS-DATA.
000824             20  CF-MAIL-TO-NAME            PIC X(30).
000825             20  CF-IN-CARE-OF              PIC X(30).
000826             20  CF-ADDRESS-LINE-1          PIC X(30).
000827             20  CF-ADDRESS-LINE-2          PIC X(30).
000828             20  CF-CITY-STATE              PIC X(30).
000829             20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
000830             20  CF-PHONE-NO                PIC 9(11)     COMP-3.
000831
000832         16  CF-CLAIM-NO-CONTROL.
000833             20  CF-CLAIM-NO-METHOD         PIC X.
000834                 88  CLAIM-NO-MANUAL            VALUE '1'.
000835                 88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
000836                 88  CLAIM-NO-SEQ               VALUE '3'.
000837                 88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
000838             20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
000839                 88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
000840                 88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
000841                 88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
000842
000843         16  CF-CHECK-NO-CONTROL.
000844             20  CF-CHECK-NO-METHOD         PIC X.
000845                 88  CHECK-NO-MANUAL            VALUE '1'.
000846                 88  CHECK-NO-AUTO-SEQ          VALUE '2'.
000847                 88  CHECK-NO-CARR-SEQ          VALUE '3'.
000848                 88  CHECK-NO-AT-PRINT          VALUE '4'.
000849             20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
000850                 88  CHECK-CNT-RESET-VALUE      VALUE +999999.
000851
000852         16  CF-DOMICILE-STATE              PIC XX.
000853
000854         16  CF-EXPENSE-CONTROLS.
000855             20  CF-EXPENSE-METHOD          PIC X.
000856                 88  EXPENSE-CALC-MANUAL        VALUE '1'.
000857                 88  DOLLARS-PER-PMT            VALUE '2'.
000858                 88  PERCENT-OF-PAYMENT         VALUE '3'.
000859                 88  DOLLARS-PER-MONTH          VALUE '4'.
000860             20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
000861             20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
000862
000863         16  CF-CORRESPONDENCE-CONTROL.
000864             20  CF-LETTER-RESEND-OPT       PIC X.
000865                 88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
000866                 88  LETTERS-ARE-ARCHIVED       VALUE '1'.
000867             20  FILLER                     PIC X(4).
000868
000869         16  CF-RESERVE-CONTROLS.
000870             20  CF-MANUAL-SW               PIC X.
000871                 88  CF-MANUAL-RESERVES-USED    VALUE '1'.
000872             20  CF-FUTURE-SW               PIC X.
000873                 88  CF-FUTURE-RESERVES-USED    VALUE '1'.
000874             20  CF-PTC-SW                  PIC X.
000875                 88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
000876             20  CF-IBNR-SW                 PIC X.
000877                 88  CF-IBNR-RESERVES-USED      VALUE '1'.
000878             20  CF-PTC-LF-SW               PIC X.
000879                 88  CF-LF-PTC-USED             VALUE '1'.
000880             20  CF-CDT-ACCESS-METHOD       PIC X.
000881                 88  CF-CDT-ROUND-NEAR          VALUE '1'.
000882                 88  CF-CDT-ROUND-HIGH          VALUE '2'.
000883                 88  CF-CDT-INTERPOLATED        VALUE '3'.
000884             20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
000885
000886         16  CF-CLAIM-CALC-METHOD           PIC X.
000887             88  360-PLUS-MONTHS                VALUE '1'.
000888             88  365-PLUS-MONTHS                VALUE '2'.
000889             88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
000890             88  360-DAILY                      VALUE '4'.
000891             88  365-DAILY                      VALUE '5'.
000892
000893         16  CF-LAST-ALPHA-CHARACTER        PIC X.
000894         16  FILLER                         PIC X(11).
000895
000896         16  CF-LIMIT-AMOUNTS.
000897             20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
000898             20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
000899             20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
000900             20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
000901             20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
000902             20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
000903             20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
000904             20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
000905             20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
000906             20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
000907
000908         16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
000909         16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
000910         16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
000911
000912         16  CF-ZIP-CODE.
000913             20  CF-ZIP-PRIME.
000914                 24  CF-ZIP-1ST             PIC X.
000915                     88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000916                 24  FILLER                 PIC X(4).
000917             20  CF-ZIP-PLUS4               PIC X(4).
000918         16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
000919             20  CF-CAN-POSTAL-1            PIC XXX.
000920             20  CF-CAN-POSTAL-2            PIC XXX.
000921             20  FILLER                     PIC XXX.
000922
000923         16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
000924         16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
000925         16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
000926
000927         16  CF-RATING-SWITCH               PIC X.
000928             88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
000929             88  CF-NO-RATING                   VALUE 'N'.
000930
000931         16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
000932
000933         16  CF-CARRIER-OVER-SHORT.
000934             20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
000935             20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000936
000937         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
000938         16  CF-SECPAY-SWITCH               PIC X.
000939             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
000940             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
000941         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
000942         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
000943         16  FILLER                         PIC X(444).
000944*        16  FILLER                         PIC X(452).
000945
000946
000947****************************************************************
000948*             MORTALITY MASTER RECORD                          *
000949****************************************************************
000950
000951     12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
000952         16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
000953                                INDEXED BY CF-MORT-NDX.
000954             20  CF-MORT-TABLE              PIC X(5).
000955             20  CF-MORT-TABLE-TYPE         PIC X.
000956                 88  CF-MORT-JOINT              VALUE 'J'.
000957                 88  CF-MORT-SINGLE             VALUE 'S'.
000958                 88  CF-MORT-COMBINED           VALUE 'C'.
000959                 88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
000960                 88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
000961             20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
000962             20  CF-MORT-AGE-METHOD         PIC XX.
000963                 88  CF-AGE-LAST                VALUE 'AL'.
000964                 88  CF-AGE-NEAR                VALUE 'AN'.
000965             20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
000966             20  CF-MORT-ADJUSTMENT-DIRECTION
000967                                            PIC X.
000968                 88  CF-MINUS                   VALUE '-'.
000969                 88  CF-PLUS                    VALUE '+'.
000970             20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
000971             20  CF-MORT-JOINT-CODE         PIC X.
000972                 88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
000973             20  CF-MORT-PC-Q               PIC X.
000974                 88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
000975             20  CF-MORT-TABLE-CODE         PIC X(4).
000976             20  CF-MORT-COMMENTS           PIC X(15).
000977             20  FILLER                     PIC X(14).
000978
000979         16  FILLER                         PIC X(251).
000980
000981
000982****************************************************************
000983*             BUSSINESS TYPE MASTER RECORD                     *
000984****************************************************************
000985
000986     12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
000987* FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
000988* RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
000989* AND RECORD 05 IS TYPES 81-99
000990         16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
000991             20  CF-BUSINESS-TITLE          PIC  X(19).
000992             20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
000993                                            PIC S9V9(4) COMP-3.
000994             20  CF-BUS-EXCL-ST-CALL        PIC  X.
000995             20  FILLER                     PIC  X.
000996         16  FILLER                         PIC  X(248).
000997
000998
000999****************************************************************
001000*             TERMINAL MASTER RECORD                           *
001001****************************************************************
001002
001003     12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001004
001005         16  CF-COMPANY-TERMINALS.
001006             20  CF-TERMINAL-ID  OCCURS 120 TIMES
001007                                  PIC X(4).
001008         16  FILLER               PIC X(248).
001009
001010
001011****************************************************************
001012*             LIFE EDIT MASTER RECORD                          *
001013****************************************************************
001014
001015     12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001016         16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
001017             20  CF-LIFE-CODE-IN            PIC XX.
001018             20  CF-LIFE-CODE-OUT           PIC XX.
001019         16  FILLER                         PIC X(248).
001020
001021
001022****************************************************************
001023*             AH EDIT MASTER RECORD                            *
001024****************************************************************
001025
001026     12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001027         16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
001028             20  CF-AH-CODE-IN              PIC XXX.
001029             20  CF-AH-CODE-OUT             PIC XX.
001030         16  FILLER                         PIC X(248).
001031
001032
001033****************************************************************
001034*             CREDIBILITY TABLES                               *
001035****************************************************************
001036
001037     12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
001038         16  CF-CRDB-ENTRY   OCCURS 36 TIMES
001039                             INDEXED BY CF-CRDB-NDX.
001040             20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
001041             20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
001042             20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
001043         16  FILLER                         PIC  X(332).
001044
001045
001046****************************************************************
001047*             REPORT CUSTOMIZATION RECORD                      *
001048****************************************************************
001049
001050     12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
001051         16  CF-ACCOUNT-MASTER-STATUS       PIC X.
001052             88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
001053             88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
001054             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
001055**** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
001056****       A T-TRANSFER.                                   ****
001057             88  CF-ALL-ACCOUNTS                VALUE 'B'.
001058
001059         16  FILLER                         PIC XX.
001060
001061         16  CF-CARRIER-CNTL-OPT.
001062             20  CF-CARRIER-OPT-SEQ         PIC 9.
001063                 88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
001064                 88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
001065             20  CF-CARRIER-SELECT OCCURS 3 TIMES
001066                                            PIC X.
001067         16  CF-GROUP-CNTL-OPT.
001068             20  CF-GROUP-OPT-SEQ           PIC 9.
001069                 88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
001070                 88  CF-GROUP-OPT-NOT-USED      VALUE 0.
001071             20  CF-GROUP-SELECT OCCURS 3 TIMES
001072                                            PIC X(6).
001073         16  CF-STATE-CNTL-OPT.
001074             20  CF-STATE-OPT-SEQ           PIC 9.
001075                 88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
001076                 88  CF-STATE-OPT-NOT-USED      VALUE 0.
001077             20  CF-STATE-SELECT OCCURS 3 TIMES
001078                                            PIC XX.
001079         16  CF-ACCOUNT-CNTL-OPT.
001080             20  CF-ACCOUNT-OPT-SEQ         PIC 9.
001081                 88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
001082                 88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
001083             20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
001084                                            PIC X(10).
001085         16  CF-BUS-TYP-CNTL-OPT.
001086             20  CF-BUS-TYP-OPT-SEQ         PIC 9.
001087                 88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
001088                 88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
001089             20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
001090                                            PIC XX.
001091         16  CF-LF-TYP-CNTL-OPT.
001092             20  CF-LF-TYP-OPT-SEQ          PIC 9.
001093                 88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
001094                 88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
001095             20  CF-BUS-LF-SELECT OCCURS 3 TIMES
001096                                            PIC XX.
001097         16  CF-AH-TYP-CNTL-OPT.
001098             20  CF-AH-TYP-OPT-SEQ          PIC 9.
001099                 88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
001100                 88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
001101             20  CF-BUS-AH-SELECT OCCURS 3 TIMES
001102                                            PIC XX.
001103         16  CF-REPTCD1-CNTL-OPT.
001104             20  CF-REPTCD1-OPT-SEQ         PIC 9.
001105                 88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
001106                 88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
001107             20  CF-REPTCD1-SELECT OCCURS 3 TIMES
001108                                            PIC X(10).
001109         16  CF-REPTCD2-CNTL-OPT.
001110             20  CF-REPTCD2-OPT-SEQ         PIC 9.
001111                 88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
001112                 88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
001113             20  CF-REPTCD2-SELECT OCCURS 3 TIMES
001114                                            PIC X(10).
001115         16  CF-USER1-CNTL-OPT.
001116             20  CF-USER1-OPT-SEQ           PIC 9.
001117                 88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
001118                 88  CF-USER1-OPT-NOT-USED      VALUE 0.
001119             20  CF-USER1-SELECT OCCURS 3 TIMES
001120                                            PIC X(10).
001121         16  CF-USER2-CNTL-OPT.
001122             20  CF-USER2-OPT-SEQ           PIC 9.
001123                 88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
001124                 88  CF-USER2-OPT-NOT-USED      VALUE 0.
001125             20  CF-USER2-SELECT OCCURS 3 TIMES
001126                                            PIC X(10).
001127         16  CF-USER3-CNTL-OPT.
001128             20  CF-USER3-OPT-SEQ           PIC 9.
001129                 88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
001130                 88  CF-USER3-OPT-NOT-USED      VALUE 0.
001131             20  CF-USER3-SELECT OCCURS 3 TIMES
001132                                            PIC X(10).
001133         16  CF-USER4-CNTL-OPT.
001134             20  CF-USER4-OPT-SEQ           PIC 9.
001135                 88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
001136                 88  CF-USER4-OPT-NOT-USED      VALUE 0.
001137             20  CF-USER4-SELECT OCCURS 3 TIMES
001138                                            PIC X(10).
001139         16  CF-USER5-CNTL-OPT.
001140             20  CF-USER5-OPT-SEQ           PIC 9.
001141                 88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
001142                 88  CF-USER5-OPT-NOT-USED      VALUE 0.
001143             20  CF-USER5-SELECT OCCURS 3 TIMES
001144                                            PIC X(10).
001145         16  CF-REINS-CNTL-OPT.
001146             20  CF-REINS-OPT-SEQ           PIC 9.
001147                 88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
001148                 88  CF-REINS-OPT-NOT-USED      VALUE 0.
001149             20  CF-REINS-SELECT OCCURS 3 TIMES.
001150                 24  CF-REINS-PRIME         PIC XXX.
001151                 24  CF-REINS-SUB           PIC XXX.
001152
001153         16  CF-AGENT-CNTL-OPT.
001154             20  CF-AGENT-OPT-SEQ           PIC 9.
001155                 88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
001156                 88  CF-AGENT-OPT-NOT-USED      VALUE 0.
001157             20  CF-AGENT-SELECT OCCURS 3 TIMES
001158                                            PIC X(10).
001159
001160         16  FILLER                         PIC X(43).
001161
001162         16  CF-LOSS-RATIO-SELECT.
001163             20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
001164             20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
001165         16  CF-ENTRY-DATE-SELECT.
001166             20  CF-SEL-LO-ENTRY-DATE       PIC XX.
001167             20  CF-SEL-HI-ENTRY-DATE       PIC XX.
001168         16  CF-EFFECTIVE-DATE-SELECT.
001169             20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
001170             20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
001171
001172         16  CF-EXCEPTION-LIST-IND          PIC X.
001173             88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
001174
001175         16  FILLER                         PIC X(318).
001176
001177****************************************************************
001178*                  EXCEPTION REPORTING RECORD                  *
001179****************************************************************
001180
001181     12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
001182         16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
001183             88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
001184
001185         16  CF-COMBINED-LIFE-AH-OPT.
001186             20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
001187             20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
001188             20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
001189             20  CF-CANCELLATION-RATIO      PIC S9(02).
001190
001191         16  CF-LIFE-OPT.
001192             20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001193             20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001194             20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001195             20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001196             20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001197             20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001198             20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001199             20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001200             20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001201             20  CF-LF-AVG-AGE-MAX          PIC S9(02).
001202
001203         16  CF-AH-OPT.
001204             20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001205             20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001206             20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001207             20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001208             20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001209             20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001210             20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001211             20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001212             20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001213             20  CF-AH-AVG-AGE-MAX          PIC S9(02).
001214
001215         16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
001216             88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
001217             88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
001218             88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
001219
001220         16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
001221
001222         16  FILLER                         PIC X(673).
001223
001224
001225****************************************************************
001226*             MORTGAGE SYSTEM PLAN RECORD                      *
001227****************************************************************
001228
001229     12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
001230         16  CF-PLAN-TYPE                   PIC X.
001231             88  CF-LIFE-MORT-PLAN             VALUE 'L'.
001232             88  CF-DISAB-MORT-PLAN            VALUE 'D'.
001233             88  CF-AD-D-MORT-PLAN             VALUE 'A'.
001234         16  CF-PLAN-ABBREV                 PIC XXX.
001235         16  CF-PLAN-DESCRIPT               PIC X(10).
001236         16  CF-PLAN-NOTES                  PIC X(20).
001237         16  CF-PLAN-ESTABLISH-DATE         PIC XX.
001238         16  CF-PLAN-UNDERWRITING.
001239             20  CF-PLAN-TERM-DATA.
001240                 24  CF-MINIMUM-TERM        PIC S999      COMP-3.
001241                 24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
001242             20  CF-PLAN-AGE-DATA.
001243                 24  CF-MINIMUM-AGE         PIC S999      COMP-3.
001244                 24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
001245                 24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
001246             20  CF-PLAN-BENEFIT-DATA.
001247                 24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001248                 24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001249                 24  CF-MAXIMUM-MONTHLY-BENEFIT
001250                                            PIC S9(7)V99  COMP-3.
001251         16  CF-PLAN-POLICY-FORMS.
001252             20  CF-POLICY-FORM             PIC X(12).
001253             20  CF-MASTER-APPLICATION      PIC X(12).
001254             20  CF-MASTER-POLICY           PIC X(12).
001255         16  CF-PLAN-RATING.
001256             20  CF-RATE-CODE               PIC X(5).
001257             20  CF-SEX-RATING              PIC X.
001258                 88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
001259                 88  CF-PLAN-SEX-RATED         VALUE '2'.
001260             20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
001261             20  CF-SUB-STD-TYPE            PIC X.
001262                 88  CF-PCT-OF-PREM            VALUE '1'.
001263                 88  CF-PCT-OF-BENE            VALUE '2'.
001264         16  CF-PLAN-PREM-TOLERANCES.
001265             20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
001266             20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
001267         16  CF-PLAN-PYMT-TOLERANCES.
001268             20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
001269             20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
001270         16  CF-PLAN-MISC-DATA.
001271             20  FILLER                     PIC X.
001272             20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
001273             20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
001274         16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
001275         16  CF-PLAN-IND-GRP                PIC X.
001276             88  CF-MORT-INDIV-PLAN            VALUE 'I'
001277                                                     '1'.
001278             88  CF-MORT-GROUP-PLAN            VALUE 'G'
001279                                                     '2'.
001280         16  CF-MIB-SEARCH-SW               PIC X.
001281             88  CF-MIB-SEARCH-ALL             VALUE '1'.
001282             88  CF-MIB-SEARCH-NONE            VALUE '2'.
001283             88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
001284             88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
001285         16  CF-ALPHA-SEARCH-SW             PIC X.
001286             88  CF-MIB-ALPHA-ALL              VALUE '1'.
001287             88  CF-MIB-ALPHA-NONE             VALUE '2'.
001288             88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
001289             88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
001290             88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
001291             88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
001292             88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
001293             88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
001294             88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
001295             88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
001296                                                     'A' 'B' 'C'
001297                                                     'X' 'Y' 'Z'.
001298         16  CF-EFF-DT-RULE-SW              PIC X.
001299             88  CF-EFF-DT-ENTER               VALUE 'E'.
001300             88  CF-EFF-DT-MONTH               VALUE 'M'.
001301             88  CF-EFF-DT-QTR                 VALUE 'Q'.
001302             88  CF-EFF-DT-SEMI                VALUE 'S'.
001303             88  CF-EFF-DT-ANN                 VALUE 'A'.
001304         16  FILLER                         PIC X(4).
001305         16  CF-HEALTH-QUESTIONS            PIC X.
001306             88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
001307         16  CF-GRACE-PERIOD                PIC S999      COMP-3.
001308         16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
001309         16  CF-PLAN-SNGL-JNT               PIC X.
001310             88  CF-COMBINED-PLAN              VALUE 'C'.
001311             88  CF-JNT-PLAN                   VALUE 'J'.
001312             88  CF-SNGL-PLAN                  VALUE 'S'.
001313         16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
001314         16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
001315         16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
001316         16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
001317         16  CF-RERATE-CNTL                 PIC  X.
001318             88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
001319             88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
001320             88  CF-DO-NOT-RERATE               VALUE '3' ' '.
001321             88  CF-AUTO-RECALC                 VALUE '4'.
001322         16  CF-BENEFIT-TYPE                PIC  X.
001323             88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
001324             88  CF-BENEFIT-REDUCES             VALUE '2'.
001325         16  CF-POLICY-FEE                  PIC S999V99
001326                                                    COMP-3.
001327         16  CF-1ST-NOTICE-FORM             PIC  X(04).
001328         16  CF-2ND-NOTICE-FORM             PIC  X(04).
001329         16  CF-3RD-NOTICE-FORM             PIC  X(04).
001330         16  CF-4TH-NOTICE-FORM             PIC  X(04).
001331         16  FILLER                         PIC  X(32).
001332         16  CF-TERMINATION-FORM            PIC  X(04).
001333         16  FILLER                         PIC  X(08).
001334         16  CF-CLAIM-CAP                   PIC S9(7)V99
001335                                                       COMP-3.
001336         16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
001337         16  CF-ISSUE-LETTER                PIC  X(4).
001338         16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
001339         16  CF-DEPENDENT-COVERAGE          PIC  X.
001340             88  CF-YES-DEP-COV                 VALUE 'Y'.
001341             88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
001342         16  CF-MP-REFUND-CALC              PIC X.
001343             88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
001344             88  CF-MP-REFD-BY-R78              VALUE '1'.
001345             88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
001346             88  CF-MP-REFD-AS-CALIF            VALUE '3'.
001347             88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
001348             88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
001349             88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
001350             88  CF-MP-REFD-MEAN                VALUE '8'.
001351         16  CF-ALT-RATE-CODE               PIC  X(5).
001352
001353
001354         16  FILLER                         PIC X(498).
001355****************************************************************
001356*             MORTGAGE COMPANY MASTER RECORD                   *
001357****************************************************************
001358
001359     12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001360         16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
001361         16  CF-MORTG-ACCESS-CONTROL        PIC X.
001362             88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
001363             88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
001364             88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
001365             88  CF-MORT-PROD-CNTL                   VALUE '3'.
001366             88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
001367
001368         16  CF-MORTG-CONVERSION-DATE       PIC XX.
001369         16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
001370         16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
001371         16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
001372         16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
001373
001374         16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
001375             88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
001376         16  CF-MP-RECON-USE-IND            PIC X(1).
001377             88  CF-MP-USE-RECON             VALUE 'Y'.
001378         16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
001379             88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
001380         16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
001381             88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
001382             88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
001383         16  FILLER                         PIC X(1).
001384         16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
001385             88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
001386         16  CF-MORTG-MIB-VERSION           PIC X.
001387             88  CF-MORTG-MIB-BATCH         VALUE '1'.
001388             88  CF-MORTG-MIB-ONLINE        VALUE '2'.
001389             88  CF-MORTG-MIB-BOTH          VALUE '3'.
001390         16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
001391             20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
001392                 88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
001393             20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
001394                 88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
001395             20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
001396                 88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
001397             20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
001398                 88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
001399             20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
001400                 88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
001401             20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
001402                 88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
001403         16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
001404         16  FILLER                         PIC X(7).
001405         16  CF-MORTG-DESTINATION-SYMBOL.
001406             20  CF-MORTG-MIB-COMM          PIC X(5).
001407             20  CF-MORTG-MIB-TERM          PIC X(5).
001408         16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
001409             88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
001410         16  FILLER                         PIC X(03).
001411         16  CF-MP-CHECK-NO-CONTROL.
001412             20  CF-MP-CHECK-NO-METHOD      PIC X(01).
001413                 88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
001414                 88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
001415                                                ' ' LOW-VALUES.
001416                 88  CF-MP-CHECK-NO-PRE-PRINTED
001417                                               VALUE '3'.
001418         16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
001419         16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
001420         16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
001421             20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
001422                 88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
001423             20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
001424                 88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
001425             20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
001426                 88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
001427             20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
001428                 88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
001429             20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
001430                 88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
001431             20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
001432                 88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
001433         16  CF-MORTG-BILLING-AREA.
001434             20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
001435                                            PIC X.
001436         16  CF-MORTG-MONTH-END-DT          PIC XX.
001437         16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
001438         16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
001439         16  CF-MORTG-MIB-DEST-SW           PIC X.
001440             88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
001441             88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
001442         16  FILLER                         PIC X.
001443         16  CF-MORTG-LABEL-CONTROL         PIC X.
001444             88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
001445             88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
001446         16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
001447         16  FILLER                         PIC X(8).
001448         16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
001449         16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
001450         16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
001451         16  CF-ACH-COMPANY-ID.
001452             20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
001453                 88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
001454                 88  CF-ACH-ICD-DUNS                VALUE '3'.
001455                 88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
001456             20  CF-ACH-COMPANY-ID-NO       PIC X(9).
001457         16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
001458             88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
001459         16  CF-RATE-DEV-AUTHORIZATION      PIC X.
001460             88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
001461             88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
001462         16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
001463         16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
001464         16  FILLER                         PIC X(536).
001465
001466****************************************************************
001467*             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
001468****************************************************************
001469
001470     12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
001471         16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
001472             20  CF-FEMALE-HEIGHT.
001473                 24  CF-FEMALE-FT           PIC 99.
001474                 24  CF-FEMALE-IN           PIC 99.
001475             20  CF-FEMALE-MIN-WT           PIC 999.
001476             20  CF-FEMALE-MAX-WT           PIC 999.
001477         16  FILLER                         PIC X(428).
001478
001479     12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
001480         16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
001481             20  CF-MALE-HEIGHT.
001482                 24  CF-MALE-FT             PIC 99.
001483                 24  CF-MALE-IN             PIC 99.
001484             20  CF-MALE-MIN-WT             PIC 999.
001485             20  CF-MALE-MAX-WT             PIC 999.
001486         16  FILLER                         PIC X(428).
001487******************************************************************
001488*             AUTOMATIC ACTIVITY RECORD                          *
001489******************************************************************
001490     12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
001491         16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
001492             20  CF-SYS-ACTIVE-SW           PIC X(01).
001493             20  CF-SYS-LETTER-ID           PIC X(04).
001494             20  CF-SYS-RESEND-DAYS         PIC 9(03).
001495             20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
001496             20  CF-SYS-RESET-SW            PIC X(01).
001497             20  CF-SYS-REPORT-DAYS         PIC 9(03).
001498             20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
001499
001500         16  FILLER                         PIC X(50).
001501
001502         16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
001503             20  CF-USER-ACTIVE-SW          PIC X(01).
001504             20  CF-USER-LETTER-ID          PIC X(04).
001505             20  CF-USER-RESEND-DAYS        PIC 9(03).
001506             20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
001507             20  CF-USER-RESET-SW           PIC X(01).
001508             20  CF-USER-REPORT-DAYS        PIC 9(03).
001509             20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
001510             20  CF-USER-ACTIVITY-DESC      PIC X(20).
001511
001512         16  FILLER                         PIC X(246).
      *<<((file: ELCCNTL))
000372
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL159' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000373 VCOBOL-DUMMY-PROCEDURE.
000374
000375     MOVE EIBDATE                TO DC-JULIAN-YYDDD
000376     MOVE '5'                    TO DC-OPTION-CODE
000377     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
000378     MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
000379     MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
000380
000381     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
000382     IF EIBCALEN = 0
000383        GO TO 8800-UNAUTHORIZED-ACCESS.
000384
000385     MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
000386     MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
000387     MOVE 1                      TO EMI-NUMBER-OF-LINES
000388
000389     
      * EXEC CICS HANDLE CONDITION
000390*        NOTOPEN    (8870-NOTOPEN)
000391*        NOTFND     (8880-NOT-FOUND)
000392*        PGMIDERR   (9600-PGMID-ERROR)
000393*        ERROR      (9990-ABEND)
000394*    END-EXEC.
      *    MOVE '"$JIL.                ! " #00004113' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' &
                X'202020202020202020202120' &
                X'2220233030303034313133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000395
000396     IF PI-CALLING-PROGRAM = 'EL150'
000397        MOVE SPACES                 TO ERPDEF-KEY
000398        MOVE PI-COMPANY-CD          TO ERPDEF-COMPANY-CD
000399        MOVE PI-FORM-NUMBER (1:2)   TO ERPDEF-STATE
000400        MOVE PI-FORM-NUMBER (3:3)   TO ERPDEF-PROD-CD
000401        MOVE PI-FORM-NUMBER (6:1)   TO ERPDEF-BEN-TYPE
000402        MOVE PI-FORM-NUMBER (7:2)   TO ERPDEF-BEN-CODE
000403        MOVE PI-CERT-EFF-DT         TO ERPDEF-EXP-DT
000404
000405        
      * EXEC CICS READ
000406*           DATASET    (ERPDEF-FILE-ID)
000407*           SET        (ADDRESS OF PRODUCT-MASTER)
000408*           RIDFLD     (ERPDEF-KEY)
000409*           GTEQ
000410*           RESP       (WS-RESPONSE)
000411*       END-EXEC
      *    MOVE '&"S        G          (  N#00004129' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303034313239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000412
000413        IF RESP-NORMAL
000414           AND PI-COMPANY-CD   =  PD-COMPANY-CD
000415           AND PD-STATE        =  PI-FORM-NUMBER (1:2)
000416           AND PD-PRODUCT-CD   =  PI-FORM-NUMBER (3:3)
000417           AND PD-BEN-TYPE     =  PI-FORM-NUMBER (6:1)
000418           AND PD-BEN-CODE     =  PI-FORM-NUMBER (7:2)
000419           GO TO 0150-SET-PROGRAM-SAVES
000420        ELSE
000421           MOVE 'EL159'          TO PGM-NAME
000422           GO TO 9300-XCTL
000423        END-IF
000424     END-IF
000425
000426     .
000427 0150-SET-PROGRAM-SAVES.
000428
000429     IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
000430         IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
000431             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000432             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000433             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000434             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000435             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000436             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000437             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000438             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000439         ELSE
000440             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000441             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000442             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000443             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000444             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000445             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000446             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000447             MOVE SPACES               TO PI-SAVED-PROGRAM-6
000448     ELSE
000449         GO TO 0200-RECEIVE.
000450
000451     IF EIBTRNID = EL150-TRANS-ID
000452        MOVE 'S'                     TO MAINTI
000453        MOVE +1                      TO MAINTL
000454        MOVE DFHENTER                TO EIBAID
000455        MOVE PI-FORM-NUMBER (1:2)    TO STATEI
000456        MOVE +2                      TO STATEL
000457        MOVE PI-FORM-NUMBER (3:3)    TO PRODCDI
000458        MOVE +3                      TO PRODCDL
000459        MOVE PI-FORM-NUMBER (6:1)    TO BENTYPI
000460        MOVE +1                      TO BENTYPL
000461        MOVE PI-FORM-NUMBER (7:2)    TO BENCODEI
000462        MOVE +2                      TO BENCODEL
000463        MOVE PD-PROD-EXP-DT          TO DC-BIN-DATE-1
000464        MOVE ' '                     TO DC-OPTION-CODE
000465        MOVE +0                      TO DC-ELAPSED-DAYS
000466                                        DC-ELAPSED-MONTHS
000467        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
000468        IF NO-CONVERSION-ERROR
000469           MOVE DC-GREG-DATE-A-EDIT  TO EXPDTO
000470           GO TO 1000-SHOW-PROD-RECORD
000471        ELSE
000472           MOVE LOW-VALUES           TO EXPDTO
000473           GO TO 1000-SHOW-PROD-RECORD.
000474
000475     IF EIBTRNID = EL1591-TRANS-ID
000476        MOVE PI-PROD-KEY         TO ERPDEF-KEY
000477        GO TO 1000-CONTINUE-SHOW
000478     END-IF
000479
000480     MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.
000481     MOVE LOW-VALUES                 TO  EL159AO.
000482     MOVE -1                         TO  MAINTL.
000483     GO TO 8100-SEND-INITIAL-MAP
000484
000485     .
000486 0200-RECEIVE.
000487
000488     IF EIBAID = DFHCLEAR
000489        GO TO 9400-CLEAR.
000490
000491     IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
000492         MOVE LOW-VALUES             TO  EL159AI
000493         MOVE ER-7008                TO  EMI-ERROR
000494         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000495         MOVE -1                     TO  MAINTL
000496         GO TO 8200-SEND-DATAONLY.
000497
000498     IF PI-PROCESSOR-ID EQUAL 'LGXX'
000499         NEXT SENTENCE
000500     ELSE
000501         
      * EXEC CICS READQ TS
000502*            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
000503*            INTO   (SECURITY-CONTROL)
000504*            LENGTH (SC-COMM-LENGTH)
000505*            ITEM   (SC-ITEM)
000506*        END-EXEC
      *    MOVE '*$II   L              ''   #00004225' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034323235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000507         MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP
000508         MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.
000509
000510     
      * EXEC CICS RECEIVE
000511*        MAP      (WS-MAP-NAME)
000512*        MAPSET   (MAPSET-NAME)
000513*        INTO     (EL159AI)
000514*    END-EXEC.
           MOVE LENGTH OF
            EL159AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004234' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034323334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000515
000516     IF PFKEYL = +0
000517         GO TO 0300-CHECK-PFKEYS.
000518
000519     IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)
000520         MOVE PF-VALUES (PFKEYI)     TO  EIBAID
000521     ELSE
000522         MOVE ER-0029                TO  EMI-ERROR
000523         GO TO 0320-INPUT-ERROR.
000524
000525
000526 0300-CHECK-PFKEYS.
000527
000528     IF EIBAID EQUAL DFHPF23
000529         GO TO 8810-PF23.
000530
000531     IF EIBAID EQUAL DFHPF24
000532         GO TO 9200-RETURN-MAIN-MENU.
000533
000534     IF EIBAID EQUAL DFHPF12
000535         GO TO 9500-PF12.
000536
000537     IF (MAINTL <> 0) AND (EIBAID <> DFHENTER)
000538         MOVE ER-0050            TO EMI-ERROR
000539         GO TO 0320-INPUT-ERROR.
000540
000541     IF EIBAID EQUAL DFHPF1
000542         GO TO 5000-FIND-NEXT-PROD-RECORD.
000543
000544     IF EIBAID EQUAL DFHPF2
000545         GO TO 5100-FIND-PREV-PROD-RECORD.
000546
000547     IF EIBAID = DFHPF3
000548        MOVE 'EL1591'            TO PGM-NAME
000549        GO TO 9300-XCTL
000550     END-IF
000551
000552     IF EIBAID = DFHPF4
000553        GO TO 5200-PAGE-FORWARD.
000554
000555     IF EIBAID = DFHPF5
000556        GO TO 5300-PAGE-BACKWARD.
000557
000558     IF EIBAID EQUAL DFHENTER
000559         GO TO 0330-EDIT-DATA.
000560
000561     MOVE ER-0029                    TO  EMI-ERROR.
000562
000563 0320-INPUT-ERROR.
000564
000565     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000566     MOVE AL-UNBON                   TO  PFKEYA.
000567     MOVE -1                         TO  PFKEYL.
000568     GO TO 8200-SEND-DATAONLY.
000569
000570     EJECT
000571 0330-EDIT-DATA.
000572     MOVE LINE-NBR (1) TO WS-LINE-NBR-CHAR.
000573     IF WS-LINE-NBR-1 = 1
000574        MOVE 1 TO PAGE-NBR
000575     ELSE
000576        MOVE 2 TO PAGE-NBR
000577     END-IF
000578
000579     IF NOT DISPLAY-CAP
000580         MOVE 'READ'                 TO  SM-READ
000581         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000582         MOVE ER-0070                TO  EMI-ERROR
000583         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000584         MOVE -1                     TO  MAINTL
000585         GO TO 8100-SEND-INITIAL-MAP.
000586
000587     IF (STATEL > +0 AND
000588         PRODCDL > +0 AND
000589         BENTYPL > +0 AND
000590         BENCODEL > +0 AND
000591         EXPDTL > +0)
000592         NEXT SENTENCE
000593     ELSE
000594         IF (MAINTI    EQUAL   'S'   AND
000595             EXPDTL    EQUAL   +0)
000596             GO TO 1000-SHOW-PROD-RECORD
000597         ELSE
000598             MOVE ER-0754        TO EMI-ERROR
000599             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000600             MOVE -1             TO STATEL
000601             MOVE AL-UABON       TO BENTYPA
000602                                    PRODCDA
000603                                    STATEA
000604                                    BENCODEA
000605                                    EXPDTA
000606             GO TO 8200-SEND-DATAONLY.
000607
000608
000609     IF MAINTI EQUAL 'S'
000610         GO TO 1000-SHOW-PROD-RECORD.
000611
000612     IF MAINTI = 'A' OR 'C' OR 'D'OR 'K'
000613        IF NOT MODIFY-CAP
000614            PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000615            MOVE ER-0070             TO  EMI-ERROR
000616            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000617            MOVE LOW-VALUES          TO  EL159AO
000618            MOVE -1                  TO  MAINTL
000619            GO TO 8100-SEND-INITIAL-MAP.
000620
000621     IF MAINTI EQUAL 'C'
000622         GO TO 2000-CHANGE-PROD-RECORD.
000623
000624     IF MAINTI EQUAL 'A'
000625         GO TO 3000-ADD-PROD-RECORD.
000626
000627     IF MAINTI EQUAL 'D'
000628         GO TO 4000-DELETE-PROD-RECORD.
000629
000630     IF MAINTI EQUAL 'K'
000631         GO TO 2500-COPY-PROD-RECORD.
000632
000633     MOVE ER-0023                    TO  EMI-ERROR.
000634     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000635     MOVE -1                         TO  MAINTL.
000636     MOVE AL-UABON                   TO  MAINTA.
000637     GO TO 8200-SEND-DATAONLY.
000638
000639 1000-SHOW-PROD-RECORD.
000640
000641     MOVE SPACES                     TO  ERPDEF-KEY
000642     MOVE PI-COMPANY-CD              TO  ERPDEF-COMPANY-CD
000643     MOVE STATEI                     TO  ERPDEF-STATE
000644     MOVE PRODCDI                    TO  ERPDEF-PROD-CD
000645     MOVE BENTYPI                    TO  ERPDEF-BEN-TYPE
000646     MOVE BENCODEI                   TO  ERPDEF-BEN-CODE
000647
000648     IF MAINTI = 'S'
000649        IF EXPDTL = +0
000650           MOVE  HIGH-VALUES     TO ERPDEF-EXP-DT
000651           GO TO 1000-CONTINUE-SHOW
000652        END-IF
000653     ELSE
000654        MOVE PI-PREV-PROD-KEY    TO ERPDEF-KEY
000655        GO TO 1000-CONTINUE-SHOW
000656     END-IF
000657
000658     MOVE EXPDTI                 TO  DEEDIT-FIELD
000659     PERFORM 8600-DEEDIT         THRU 8600-EXIT
000660     IF WS-NUMVAL-OF-DEEDIT >= 999999
000661        MOVE HIGH-VALUES         TO ERPDEF-EXP-DT
000662     ELSE
000663        STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
000664           DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
000665        END-STRING
000666        MOVE 'L'                 TO DC-OPTION-CODE
000667        MOVE +0                  TO DC-ELAPSED-DAYS
000668                                    DC-ELAPSED-MONTHS
000669        PERFORM 9700-LINK-DATE-CONVERT
000670                                 THRU 9700-EXIT
000671        IF NO-CONVERSION-ERROR
000672           MOVE DC-BIN-DATE-1    TO ERPDEF-EXP-DT
000673        ELSE
000674           MOVE LOW-VALUES       TO ERPDEF-EXP-DT
000675        END-IF
000676     END-IF
000677     MOVE 1 TO R1
000678
000679     .
000680 1000-CONTINUE-SHOW.
000681     IF PAGE-NBR = 1
000682        MOVE 1 TO R1
000683     ELSE
000684        MOVE 9 TO R1
000685     END-IF
000686
000687
000688     MOVE ERPDEF-KEY             TO PI-PROD-KEY
000689                                    PI-PREV-PROD-KEY
000690
000691     
      * EXEC CICS READ
000692*        DATASET    (ERPDEF-FILE-ID)
000693*        SET        (ADDRESS OF PRODUCT-MASTER)
000694*        RIDFLD     (ERPDEF-KEY)
000695*        RESP       (WS-RESPONSE)
000696*    END-EXEC.
      *    MOVE '&"S        E          (  N#00004415' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034343135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000697
000698     IF RESP-NORMAL
000699        GO TO 7000-BUILD-OUTPUT-MAP
000700     ELSE
000701        MOVE ER-0073             TO EMI-ERROR
000702        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000703        MOVE -1                  TO MAINTL
000704        MOVE AL-UABON            TO BENTYPA
000705                                    STATEA
000706                                    PRODCDA
000707                                    BENCODEA
000708                                    EXPDTA
000709
000710        GO TO 8200-SEND-DATAONLY
000711     END-IF
000712
000713     .
000714 2000-CHANGE-PROD-RECORD.
000715
000716     IF STATEL > +0
000717        MOVE STATEI              TO PI-PK-STATE
000718     END-IF
000719     IF PRODCDL > +0
000720        MOVE PRODCDI             TO PI-PK-PROD-CD
000721     END-IF
000722     IF BENTYPL > +0
000723        MOVE BENTYPI             TO PI-BEN-TYPE
000724     END-IF
000725     IF BENCODEL > +0
000726        MOVE BENCODEI            TO PI-BEN-CODE
000727     END-IF
000728
000729     IF EXPDTL > +0
000730        MOVE EXPDTI              TO DEEDIT-FIELD
000731        PERFORM 8600-DEEDIT      THRU 8600-EXIT
000732        IF WS-NUMVAL-OF-DEEDIT >= 999999
000733           MOVE HIGH-VALUES      TO WS-EXP-DT
000734                                    PI-EXP-DT
000735        ELSE
000736           STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
000737              DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
000738           END-STRING
000739           MOVE 'L'              TO DC-OPTION-CODE
000740           MOVE +0               TO DC-ELAPSED-DAYS
000741                                    DC-ELAPSED-MONTHS
000742           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
000743           MOVE DC-BIN-DATE-1    TO WS-EXP-DT
000744                                    PI-EXP-DT
000745        END-IF
000746     END-IF
000747
000748     IF PI-PROD-KEY NOT = PI-PREV-PROD-KEY
000749        MOVE ER-0145             TO EMI-ERROR
000750        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000751        MOVE -1                  TO MAINTL
000752        GO TO 8200-SEND-DATAONLY
000753     END-IF
000754
000755     PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
000756
000757     IF NOT EMI-NO-ERRORS
000758         GO TO 8200-SEND-DATAONLY.
000759
000760     MOVE PI-PROD-KEY            TO ERPDEF-KEY
000761
000762     
      * EXEC CICS READ
000763*        DATASET    (ERPDEF-FILE-ID)
000764*        SET        (ADDRESS OF PRODUCT-MASTER)
000765*        RIDFLD     (ERPDEF-KEY)
000766*        UPDATE
000767*        RESP       (WS-RESPONSE)
000768*    END-EXEC
      *    MOVE '&"S        EU         (  N#00004486' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303034343836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000769
000770     IF RESP-NORMAL
000771        CONTINUE
000772     ELSE
000773        MOVE ER-0073             TO EMI-ERROR
000774        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000775        MOVE -1                  TO MAINTL
000776        MOVE AL-UABON            TO BENTYPA
000777                                    STATEA
000778                                    PRODCDA
000779                                    BENCODEA
000780                                    EXPDTA
000781
000782        GO TO 8200-SEND-DATAONLY
000783     END-IF
000784
000785     IF PD-LAST-MAINT-BY     NOT = PI-UPDATE-BY OR
000786        PD-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
000787         
      * EXEC CICS UNLOCK
000788*            DATASET   (ERPDEF-FILE-ID)
000789*        END-EXEC
      *    MOVE '&*                    #   #00004511' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034353131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000790         MOVE ER-0068                TO  EMI-ERROR
000791         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000792         GO TO 1000-SHOW-PROD-RECORD.
000793
000794     MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.
000795     MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.
000796     MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.
000797
000798     IF PDESCL > +0
000799        MOVE PDESCI              TO PD-PRODUCT-DESC
000800     END-IF
000801     IF AFA-LEN > +0
000802        MOVE AFA-AMT-IN          TO PD-1ST-YR-ADMIN-ALLOW
000803     END-IF
000804
000805     IF PAGE-NBR = 1
000806        MOVE 1 TO R1
000807     ELSE
000808        MOVE 9 TO R1
000809     END-IF
000810
000811     PERFORM VARYING M1 FROM +1 BY +1 UNTIL
000812        M1 > +8
000813        IF PROD-CODE-LEN (M1) > +0
000814           MOVE PROD-CODE (M1)   TO PD-PROD-CODE (R1)
000815        END-IF
000816        IF MAX-ATT-AGE-LEN (M1) > +0
000817           MOVE MAX-ATT-AGE (M1) TO PD-MAX-ATT-AGE (R1)
000818        END-IF
000819*       IF MIN-AGE-LEN (M1) > +0
000820*          MOVE MIN-AGE (M1)     TO PD-MIN-ISSUE-AGE (R1)
000821*       END-IF
000822        IF WAIT-PR-LEN (M1) > +0
000823           MOVE WAIT-PR (M1)     TO PD-WAIT-PERIOD (R1)
000824        END-IF
000825        IF MAX-TERM-LEN (M1) > +0
000826           MOVE MAX-TERM (M1)    TO PD-MAX-TERM (R1)
000827        END-IF
000828        IF MAX-AMT-LEN (M1) > +0
000829           MOVE MAX-AMT-IN (M1)  TO PD-MAX-AMT (R1)
000830        END-IF
000831        IF ben-pct-LEN (M1) > +0
000832           MOVE ben-pct-IN (M1)  TO PD-ben-pct (R1)
000833        END-IF
000834        IF PRE-EXIST-LEN (M1) > +0
000835           MOVE PRE-EXIST (M1)   TO PD-PRE-EXIST-EXCL-TYPE (R1)
000836        END-IF
000837        IF EXCL-PER-LEN (M1) > +0
000838           MOVE EXCL-PERIOD (M1) TO PD-EXCLUSION-PERIOD-DAYS (R1)
000839        END-IF
000840        IF COV-ENDS-LEN (M1) > +0
000841           MOVE COV-ENDS (M1)    TO PD-COVERAGE-ENDS-MOS (R1)
000842        END-IF
000843        IF ACC-PER-ENDS-LEN (M1) > +0
000844           MOVE ACC-PER-ENDS (M1) TO PD-ACCIDENT-ONLY-MOS (R1)
000845        END-IF
000846        IF CRIT-PER-LEN (M1) > +0
000847           MOVE CRIT-PER (M1) TO PD-CRIT-PERIOD (R1)
000848        END-IF
000849        IF REC-CP-LEN (M1) > +0
000850           IF REC-CRIT-PER (M1) NUMERIC
000851              MOVE REC-CRIT-PER-N (M1) TO PD-REC-CRIT-PERIOD (R1)
000852           ELSE
000853              MOVE REC-CRIT-PER (M1) TO PD-REC-CP-ALPHA (R1)
000854           END-IF
000855        END-IF
000856        IF RTW-MOS-LEN (M1) > +0
000857           MOVE RTW-MOS (M1)     TO PD-RTW-MOS (R1)
000858        END-IF
000859        IF MAX-EXT-LEN (M1) > +0
000860           MOVE MAX-EXT (M1)     TO PD-MAX-EXTENSION (R1)
000861        END-IF
000862        ADD 1 TO R1
000863     END-PERFORM
000864
000865     IF TRUNCL > ZEROS
000866        MOVE TRUNCI              TO PD-TRUNCATED
000867     END-IF
000868
000869     .
000870 2000-CONTINUE-CHANGE.
000871
000872     
      * EXEC CICS REWRITE
000873*        DATASET   (ERPDEF-FILE-ID)
000874*        FROM      (PRODUCT-MASTER)
000875*    END-EXEC.
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004596' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034353936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000876
000877     MOVE ER-0000                TO EMI-ERROR
000878     PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
000879     MOVE PD-CONTROL-PRIMARY     TO ERPDEF-KEY
000880     GO TO 1000-CONTINUE-SHOW
000881
000882     .
000883 2500-COPY-PROD-RECORD.
000884
000885     MOVE EXPDTI                 TO  DEEDIT-FIELD.
000886     PERFORM 8600-DEEDIT THRU 8600-EXIT.
000887     IF WS-NUMVAL-OF-DEEDIT >= 999999
000888        MOVE HIGH-VALUES         TO  WS-EXP-DT
000889     ELSE
000890        STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
000891           DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
000892        END-STRING
000893        MOVE 'L'                 TO  DC-OPTION-CODE
000894        MOVE +0                  TO  DC-ELAPSED-DAYS
000895                                         DC-ELAPSED-MONTHS
000896        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
000897        MOVE DC-BIN-DATE-1       TO  WS-EXP-DT
000898     END-IF
000899
000900***********  need to edit the key here
000901**** PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
000902
000903     IF NOT EMI-NO-ERRORS
000904         GO TO 8200-SEND-DATAONLY.
000905
000906     MOVE PI-PROD-KEY            TO ERPDEF-KEY
000907
000908     
      * EXEC CICS READ
000909*        DATASET    (ERPDEF-FILE-ID)
000910*        SET        (ADDRESS OF PRODUCT-MASTER)
000911*        RIDFLD     (ERPDEF-KEY)
000912*        RESP       (WS-RESPONSE)
000913*    END-EXEC.
      *    MOVE '&"S        E          (  N#00004632' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034363332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000914
000915     IF RESP-NORMAL
000916        CONTINUE
000917     ELSE
000918        MOVE ER-0073             TO EMI-ERROR
000919        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000920        MOVE -1                  TO MAINTL
000921        MOVE AL-UABON            TO BENTYPA
000922                                    STATEA
000923                                    PRODCDA
000924                                    BENCODEA
000925                                    EXPDTA
000926
000927        GO TO 8200-SEND-DATAONLY
000928     END-IF
000929
000930     MOVE PI-PROCESSOR-ID        TO PD-LAST-MAINT-BY
000931     MOVE EIBTIME                TO PD-LAST-MAINT-HHMMSS
000932     MOVE SAVE-BIN-DATE          TO PD-LAST-MAINT-DT
000933
000934     IF STATEL > +0
000935        MOVE STATEI              TO PD-STATE
000936                                    PI-PK-STATE
000937     END-IF
000938
000939     IF PRODCDL > +0
000940        MOVE PRODCDI             TO PD-PRODUCT-CD
000941                                    PI-PK-PROD-CD
000942     END-IF
000943
000944     IF BENTYPL > +0
000945        MOVE BENTYPI             TO PD-BEN-TYPE
000946                                    PI-BEN-TYPE
000947     END-IF
000948
000949     IF BENCODEL > +0
000950        MOVE BENCODEI            TO PD-BEN-CODE
000951                                    PI-BEN-CODE
000952     END-IF
000953
000954     MOVE EXPDTI                 TO DEEDIT-FIELD
000955     PERFORM 8600-DEEDIT THRU 8600-EXIT
000956     IF WS-NUMVAL-OF-DEEDIT >= 999999
000957         MOVE HIGH-VALUES            TO  PD-PROD-EXP-DT
000958                                         PI-EXP-DT
000959     ELSE
000960        STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
000961           DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
000962        END-STRING
000963         MOVE 'L'                    TO  DC-OPTION-CODE
000964         MOVE +0                     TO  DC-ELAPSED-DAYS
000965                                         DC-ELAPSED-MONTHS
000966         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
000967         IF NO-CONVERSION-ERROR
000968             MOVE DC-BIN-DATE-1      TO  PD-PROD-EXP-DT
000969                                         PI-EXP-DT
000970          ELSE
000971             MOVE LOW-VALUES         TO  PD-PROD-EXP-DT
000972                                         PI-EXP-DT.
000973
000974     
      * EXEC CICS WRITE
000975*        DATASET    (ERPDEF-FILE-ID)
000976*        FROM       (PRODUCT-MASTER)
000977*        RIDFLD     (PD-CONTROL-PRIMARY)
000978*        RESP       (WS-RESPONSE)
000979*    END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00004698' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303034363938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 PD-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000980
000981     IF NOT RESP-NORMAL
000982        MOVE ER-0132             TO EMI-ERROR
000983        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000984        MOVE -1                  TO MAINTL
000985        GO TO 8200-SEND-DATAONLY
000986     END-IF
000987
000988     MOVE ER-0000                TO EMI-ERROR
000989     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000990     MOVE PD-CONTROL-PRIMARY     TO ERPDEF-KEY
000991     GO TO 1000-CONTINUE-SHOW
000992
000993     .
000994 3000-ADD-PROD-RECORD.
000995
000996     PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.
000997
000998     IF NOT EMI-NO-ERRORS
000999         GO TO 8200-SEND-DATAONLY.
001000
001001     
      * EXEC CICS GETMAIN
001002*        SET       (ADDRESS OF PRODUCT-MASTER)
001003*        LENGTH    (ERPDEF-LENGTH)
001004*        INITIMG   (GETMAIN-SPACE)
001005*    END-EXEC.
      *    MOVE ',"IL                  $   #00004725' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034373235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPDEF-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001006
001007     INITIALIZE PRODUCT-MASTER
001008
001009     MOVE 'PD'                       TO  PD-RECORD-ID
001010     MOVE PI-COMPANY-CD              TO  PD-COMPANY-CD
001011     MOVE STATEI                     TO  PD-STATE
001012                                         PI-PK-STATE
001013     MOVE PRODCDI                    TO  PD-PRODUCT-CD
001014                                         PI-PK-PROD-CD
001015     MOVE BENTYPI                    TO  PD-BEN-TYPE
001016                                         PI-BEN-TYPE
001017     MOVE BENCODEI                   TO  PD-BEN-CODE
001018                                         PI-BEN-CODE
001019
001020     MOVE EXPDTI                     TO  DEEDIT-FIELD.
001021     PERFORM 8600-DEEDIT THRU 8600-EXIT.
001022     IF WS-NUMVAL-OF-DEEDIT >= 999999
001023         MOVE HIGH-VALUES            TO  PD-PROD-EXP-DT
001024                                         PI-EXP-DT
001025     ELSE
001026        STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
001027           DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
001028        END-STRING
001029         MOVE 'L'                    TO  DC-OPTION-CODE
001030         MOVE +0                     TO  DC-ELAPSED-DAYS
001031                                         DC-ELAPSED-MONTHS
001032         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001033         IF NO-CONVERSION-ERROR
001034             MOVE DC-BIN-DATE-1      TO  PD-PROD-EXP-DT
001035                                         PI-EXP-DT
001036          ELSE
001037             MOVE LOW-VALUES         TO  PD-PROD-EXP-DT
001038                                         PI-EXP-DT.
001039
001040     IF PDESCL > +0
001041        MOVE PDESCI              TO PD-PRODUCT-DESC
001042     END-IF
001043
001044     IF AFA-LEN > +0
001045        MOVE AFA-AMT-IN          TO PD-1ST-YR-ADMIN-ALLOW
001046     END-IF
001047
001048     PERFORM VARYING M1 FROM +1 BY +1 UNTIL
001049        M1 > +8
001050        IF PROD-CODE-LEN (M1) > +0
001051           MOVE PROD-CODE (M1)   TO PD-PROD-CODE (M1)
001052        END-IF
001053        IF MAX-ATT-AGE-LEN (M1) > +0
001054           MOVE MAX-ATT-AGE (M1) TO PD-MAX-ATT-AGE (M1)
001055        END-IF
001056*       IF MIN-AGE-LEN (M1) > +0
001057*          MOVE MIN-AGE (M1)     TO PD-MIN-ISSUE-AGE (M1)
001058*       END-IF
001059        IF WAIT-PR-LEN (M1) > +0
001060           MOVE WAIT-PR (M1)     TO PD-WAIT-PERIOD (M1)
001061        END-IF
001062        IF MAX-TERM-LEN (M1) > +0
001063           MOVE MAX-TERM (M1)    TO PD-MAX-TERM (M1)
001064        END-IF
001065        IF MAX-AMT-LEN (M1) > +0
001066           MOVE MAX-AMT-IN (M1)  TO PD-MAX-AMT (M1)
001067        END-IF
001068        IF ben-pct-LEN (M1) > +0
001069           MOVE ben-pct-IN (M1)  TO PD-ben-pct (M1)
001070        END-IF
001071        IF PRE-EXIST-LEN (M1) > +0
001072           MOVE PRE-EXIST (M1)   TO PD-PRE-EXIST-EXCL-TYPE (M1)
001073        END-IF
001074        IF EXCL-PER-LEN (M1) > +0
001075           MOVE EXCL-PERIOD (M1) TO PD-EXCLUSION-PERIOD-DAYS (M1)
001076        END-IF
001077        IF COV-ENDS-LEN (M1) > +0
001078           MOVE COV-ENDS (M1)    TO PD-COVERAGE-ENDS-MOS (M1)
001079        END-IF
001080        IF ACC-PER-ENDS-LEN (M1) > +0
001081           MOVE ACC-PER-ENDS (M1) TO PD-ACCIDENT-ONLY-MOS (M1)
001082        END-IF
001083        IF CRIT-PER-LEN (M1) > +0
001084           MOVE CRIT-PER (M1) TO PD-CRIT-PERIOD (M1)
001085        END-IF
001086        IF REC-CP-LEN (M1) > +0
001087           IF REC-CRIT-PER (M1) NUMERIC
001088              MOVE REC-CRIT-PER-N (M1) TO PD-REC-CRIT-PERIOD (m1)
001089           ELSE
001090              MOVE REC-CRIT-PER (M1) TO PD-REC-CP-ALPHA (M1)
001091           END-IF
001092        END-IF
001093        IF RTW-MOS-LEN (M1) > +0
001094           MOVE RTW-MOS (M1)     TO PD-RTW-MOS (M1)
001095        END-IF
001096        IF MAX-EXT-LEN (M1) > +0
001097           MOVE MAX-EXT (M1)     TO PD-MAX-EXTENSION (M1)
001098        END-IF
001099     END-PERFORM
001100
001101
001102     MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.
001103     MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.
001104     MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.
001105
001106     .
001107 3005-WRITE-ERPDEF-FILE.
001108
001109     
      * EXEC CICS WRITE
001110*        DATASET    (ERPDEF-FILE-ID)
001111*        FROM       (PRODUCT-MASTER)
001112*        RIDFLD     (PD-CONTROL-PRIMARY)
001113*        RESP       (WS-RESPONSE)
001114*    END-EXEC.
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00004833' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303034383333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 PD-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001115
001116     IF NOT RESP-NORMAL
001117        MOVE ER-0132             TO EMI-ERROR
001118        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001119        MOVE -1                  TO MAINTL
001120        GO TO 8200-SEND-DATAONLY
001121     END-IF
001122
001123     MOVE PD-CONTROL-PRIMARY     TO PI-PROD-KEY
001124                                    PI-PREV-PROD-KEY
001125                                    ERPDEF-KEY
001126
001127     MOVE ER-0000                TO EMI-ERROR
001128     PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
001129     MOVE LOW-VALUES                 TO  EL159AO
001130     MOVE -1                         TO  MAINTL
001131     GO TO 1000-CONTINUE-SHOW
001132     .
001133 4000-DELETE-PROD-RECORD.
001134
001135     MOVE SPACES                     TO ERPDEF-KEY
001136     MOVE PI-COMPANY-CD              TO  ERPDEF-COMPANY-CD.
001137     MOVE STATEI                     TO ERPDEF-STATE
001138     MOVE PRODCDI                    TO ERPDEF-PROD-CD
001139     MOVE BENTYPI                    TO  ERPDEF-BEN-TYPE
001140     MOVE BENCODEI                   TO  ERPDEF-BEN-CODE
001141
001142     MOVE EXPDTI                     TO  DEEDIT-FIELD.
001143     PERFORM 8600-DEEDIT THRU 8600-EXIT.
001144     IF WS-NUMVAL-OF-DEEDIT >= 999999
001145         MOVE HIGH-VALUES            TO  ERPDEF-EXP-DT
001146     ELSE
001147        STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
001148           DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
001149        END-STRING
001150         MOVE 'L'                    TO  DC-OPTION-CODE
001151         MOVE +0                     TO  DC-ELAPSED-DAYS
001152                                         DC-ELAPSED-MONTHS
001153         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001154         IF NO-CONVERSION-ERROR
001155             MOVE DC-BIN-DATE-1      TO  ERPDEF-EXP-DT
001156         ELSE
001157             MOVE LOW-VALUES         TO  ERPDEF-EXP-DT.
001158
001159     
      * EXEC CICS READ
001160*        DATASET   (ERPDEF-FILE-ID)
001161*        SET       (ADDRESS OF PRODUCT-MASTER)
001162*        RIDFLD    (ERPDEF-KEY)
001163*        UPDATE
001164*    END-EXEC.
      *    MOVE '&"S        EU         (   #00004883' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034383833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001165
001166     IF PD-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
001167        PD-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
001168         
      * EXEC CICS UNLOCK
001169*            DATASET   (ERPDEF-FILE-ID)
001170*            END-EXEC
      *    MOVE '&*                    #   #00004892' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001171         MOVE ER-0068                TO  EMI-ERROR
001172         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001173         GO TO 1000-SHOW-PROD-RECORD.
001174
001175     
      * EXEC CICS DELETE
001176*        DATASET   (ERPDEF-FILE-ID)
001177*        END-EXEC.
      *    MOVE '&(                    &   #00004899' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034383939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001178
001179     MOVE ER-0000                    TO  EMI-ERROR.
001180     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001181     MOVE LOW-VALUES                 TO  EL159AO.
001182     MOVE -1                         TO  MAINTL.
001183     GO TO 8100-SEND-INITIAL-MAP.
001184
001185     EJECT
001186 5000-FIND-NEXT-PROD-RECORD.
001187
001188     MOVE PI-COMPANY-CD              TO  ERPDEF-KEY
001189
001190     IF STATEL > +0
001191        MOVE STATEI              TO ERPDEF-STATE
001192     END-IF
001193     IF PRODCDL > +0
001194        MOVE PRODCDI             TO ERPDEF-PROD-CD
001195     END-IF
001196     IF BENTYPL > +0
001197         MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.
001198     IF BENCODEL > +0
001199         MOVE BENCODEI               TO  ERPDEF-BEN-CODE.
001200     IF EXPDTL IS GREATER THAN +0
001201         MOVE EXPDTI                 TO  DEEDIT-FIELD
001202         PERFORM 8600-DEEDIT THRU 8600-EXIT
001203         IF WS-NUMVAL-OF-DEEDIT >= 999999
001204             MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT
001205         ELSE
001206           STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
001207              DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
001208           END-STRING
001209            MOVE 'L'                    TO  DC-OPTION-CODE
001210             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001211             IF NO-CONVERSION-ERROR
001212                 MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT
001213             ELSE
001214                 MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.
001215
001216     
      * EXEC CICS HANDLE CONDITION
001217*        ENDFILE (5000-UNSUCCESSFUL-SEARCH)
001218*    END-EXEC.
      *    MOVE '"$''                   ! # #00004940' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303034393430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001219
001220     
      * EXEC CICS STARTBR
001221*        DATASET   (ERPDEF-FILE-ID)
001222*        RIDFLD    (ERPDEF-KEY)
001223*        GTEQ
001224*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004944' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303034393434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001225
001226 5000-READNEXT-LOOP.
001227     
      * EXEC CICS READNEXT
001228*        DATASET   (ERPDEF-FILE-ID)
001229*        SET       (ADDRESS OF PRODUCT-MASTER)
001230*        RIDFLD    (ERPDEF-KEY)
001231*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004951' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303034393531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001232
001233     IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
001234         GO TO 5000-UNSUCCESSFUL-SEARCH.
001235
001236     IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
001237        display ' key = prev key '
001238         GO TO 5000-READNEXT-LOOP.
001239
001240     MOVE ERPDEF-KEY             TO PI-PROD-KEY
001241
001242     PERFORM 5000-END-BROWSE
001243     MOVE 1 TO R1
001244
001245     GO TO 7000-BUILD-OUTPUT-MAP
001246
001247     .
001248 5000-END-BROWSE.
001249
001250     
      * EXEC CICS ENDBR
001251*        DATASET   (ERPDEF-FILE-ID)
001252*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004974' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034393734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001253
001254 5000-UNSUCCESSFUL-SEARCH.
001255
001256     PERFORM 5000-END-BROWSE
001257     MOVE -1                         TO  PFKEYL
001258     MOVE ER-0130                    TO  EMI-ERROR
001259     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001260     GO TO 1000-SHOW-PROD-RECORD
001261
001262     .
001263 5100-FIND-PREV-PROD-RECORD.
001264
001265     MOVE PI-PREV-PROD-KEY           TO  ERPDEF-KEY
001266
001267     IF STATEL > +0
001268        MOVE STATEI              TO ERPDEF-STATE
001269     END-IF
001270     IF PRODCDL > +0
001271        MOVE PRODCDI             TO ERPDEF-PROD-CD
001272     END-IF
001273     IF BENTYPL > +0
001274         MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.
001275     IF BENCODEL > +0
001276         MOVE BENCODEI               TO  ERPDEF-BEN-CODE.
001277     IF EXPDTL IS GREATER THAN +0
001278         MOVE EXPDTI                 TO  DEEDIT-FIELD
001279         PERFORM 8600-DEEDIT THRU 8600-EXIT
001280         IF WS-NUMVAL-OF-DEEDIT >= 999999
001281             MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT
001282         ELSE
001283           STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
001284              DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
001285           END-STRING
001286            MOVE 'L'                    TO  DC-OPTION-CODE
001287             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001288             IF NO-CONVERSION-ERROR
001289                 MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT
001290             ELSE
001291                 MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.
001292
001293     
      * EXEC CICS HANDLE CONDITION
001294*        ENDFILE (5100-UNSUCCESSFUL-SEARCH)
001295*    END-EXEC.
      *    MOVE '"$''                   ! $ #00005017' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303035303137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001296
001297     
      * EXEC CICS STARTBR
001298*        DATASET   (ERPDEF-FILE-ID)
001299*        RIDFLD    (ERPDEF-KEY)
001300*        GTEQ
001301*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005021' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035303231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001302
001303 5100-READPREV-LOOP.
001304     
      * EXEC CICS READPREV
001305*        DATASET   (ERPDEF-FILE-ID)
001306*        SET       (ADDRESS OF PRODUCT-MASTER)
001307*        RIDFLD    (ERPDEF-KEY)
001308*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005028' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035303238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001309
001310     IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
001311         GO TO 5100-UNSUCCESSFUL-SEARCH.
001312
001313     IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
001314         GO TO 5100-READPREV-LOOP.
001315
001316     MOVE ERPDEF-KEY             TO PI-PROD-KEY
001317     MOVE 1 TO R1
001318     GO TO 7000-BUILD-OUTPUT-MAP.
001319
001320 5100-END-BROWSE.
001321     
      * EXEC CICS ENDBR
001322*        DATASET   (ERPDEF-FILE-ID)
001323*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005045' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035303435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001324
001325 5100-UNSUCCESSFUL-SEARCH.
001326
001327     PERFORM 5100-END-BROWSE.
001328     MOVE -1                         TO  PFKEYL.
001329     MOVE ER-0131                    TO  EMI-ERROR.
001330     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001331     GO TO 1000-SHOW-PROD-RECORD.
001332
001333     .
001334 5200-PAGE-FORWARD.
001335     IF PROD-CODE (8) NOT > SPACES
001336        MOVE ER-1164              TO EMI-ERROR
001337        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001338        MOVE -1                   TO MAINTL
001339        MOVE AL-UABON             TO MAINTA
001340        GO TO 8200-SEND-DATAONLY
001341     END-IF
001342
001343     MOVE 9 TO R1
001344     MOVE PI-PROD-KEY            TO ERPDEF-KEY
001345
001346     GO TO 1000-CONTINUE-SHOW.
001347
001348 5300-PAGE-BACKWARD.
001349     MOVE 1 TO PAGE-NBR
001350
001351     MOVE LINE-NBR (1) TO WS-LINE-NBR-CHAR.
001352     IF WS-LINE-NBR-1 = 1
001353        MOVE ER-0067              TO EMI-ERROR
001354        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001355        MOVE -1                   TO MAINTL
001356        MOVE AL-UABON             TO MAINTA
001357        GO TO 8200-SEND-DATAONLY
001358     END-IF
001359
001360     MOVE PI-PROD-KEY            TO ERPDEF-KEY
001361
001362     GO TO 1000-CONTINUE-SHOW.
001363
001364 6000-EDIT-INPUT-DATA.
001365
001366     IF MAINTI = 'A'
001367        AND +0 = BENTYPL OR BENCODEL OR EXPDTL
001368              OR STATEL OR PRODCDL
001369        MOVE ER-0144             TO EMI-ERROR
001370        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001371        MOVE -1                  TO BENTYPL
001372        MOVE AL-UABON            TO BENTYPA
001373        GO TO 6000-EXIT
001374     END-IF
001375
001376     IF MAINTI = 'A'
001377        IF STATEL > +0
001378           MOVE PI-COMPANY-ID    TO ELCNTL-COMPANY-ID
001379           MOVE '3'              TO ELCNTL-RECORD-TYPE
001380           MOVE STATEI           TO ELCNTL-ACCESS
001381           MOVE +0               TO ELCNTL-SEQUENCE-NO
001382           
      * EXEC CICS READ
001383*             DATASET   (ELCNTL-FILE-ID)
001384*             RIDFLD    (ELCNTL-KEY)
001385*             SET       (ADDRESS OF CONTROL-FILE)
001386*             RESP      (WS-RESPONSE)
001387*          END-EXEC
      *    MOVE '&"S        E          (  N#00005106' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035313036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001388           IF NOT RESP-NORMAL
001389              MOVE ER-0144       TO EMI-ERROR
001390              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001391              MOVE -1            TO STATEL
001392              MOVE AL-UABON      TO STATEA
001393           END-IF
001394        END-IF
001395
001396        IF BENTYPL > +0
001397           IF BENTYPI = PI-LIFE-OVERRIDE-L1 OR PI-AH-OVERRIDE-L1
001398              CONTINUE
001399           ELSE
001400              MOVE ER-0713       TO EMI-ERROR
001401              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001402              MOVE -1            TO BENTYPL
001403              MOVE AL-UABON      TO BENTYPA
001404           END-IF
001405        END-IF
001406
001407        IF BENCODEL > +0
001408           MOVE SPACES           TO ELCNTL-KEY
001409           IF BENTYPI = PI-LIFE-OVERRIDE-L1
001410              MOVE '4'           TO ELCNTL-RECORD-TYPE
001411           ELSE
001412              MOVE '5'           TO ELCNTL-RECORD-TYPE
001413           END-IF
001414           MOVE BENCODEI         TO ELCNTL-BENE-CD
001415                                    WS-BENE-CODE
001416           MOVE PI-COMPANY-ID    TO ELCNTL-COMPANY-ID
001417           MOVE ZEROS            TO ELCNTL-SEQUENCE-NO
001418           PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
001419           IF CNTL-RECORD-FOUND
001420              MOVE AL-UANON      TO BENCODEA
001421           ELSE
001422              MOVE ER-7123       TO EMI-ERROR
001423              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001424              MOVE -1            TO BENCODEL
001425              MOVE AL-UABON      TO BENCODEA
001426           END-IF
001427        END-IF
001428
001429        IF EXPDTL > +0
001430           MOVE EXPDTI           TO DEEDIT-FIELD
001431           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001432           IF WS-NUMVAL-OF-DEEDIT >= 999999
001433              MOVE '99/99/9999'  TO EXPDTO
001434              MOVE AL-UANON      TO EXPDTA
001435           ELSE
001436              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2)
001437                 WS-NUMVAL (6:2)
001438                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
001439              END-STRING
001440              MOVE 'L'           TO DC-OPTION-CODE
001441              MOVE +0            TO DC-ELAPSED-DAYS
001442                                    DC-ELAPSED-MONTHS
001443              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001444              IF NO-CONVERSION-ERROR
001445                 MOVE DC-GREG-DATE-A-EDIT TO  EXPDTO
001446                 MOVE AL-UANON   TO EXPDTA
001447              ELSE
001448                 MOVE ER-0705    TO EMI-ERROR
001449                 perform 9900-ERROR-FORMAT thru 9900-exit
001450                 MOVE -1         TO EXPDTL
001451                 MOVE AL-UABON   TO EXPDTA
001452              END-IF
001453           END-IF
001454        END-IF
001455     END-IF
001456
001457     IF PDESCL > +0
001458        MOVE AL-UANON            TO PDESCA
001459     END-IF
001460
001461     IF AFA-LEN > +0
001462        MOVE AFA-AMT             TO DEEDIT-FIELD
001463        PERFORM 8500-DEEDIT      THRU 8500-EXIT
001464        MOVE WS-999V99-OF-DEEDIT TO AFA-AMT-IN
001465        MOVE AL-UNNON            TO AFA-ATTRB
001466     END-IF
001467
001468***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
001469***                                                            ***
001470***  This was added so the user can remove an entire occurance ***
001471***  if they space out the prod code then we will zero the     ***
001472***  rest of the stuff out.                                    ***
001473***                                                            ***
001474***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
001475
001476     PERFORM VARYING M1 FROM +1 BY +1 UNTIL
001477        M1 > +8
001478        IF PROD-CODE-LEN (M1) > +0
001479           if prod-code (m1) = spaces
001480              move +1            to max-att-age-len  (m1)
001481*                                   min-age-len      (m1)
001482                                    WAIT-PR-len      (m1)
001483                                    max-amt-len      (m1)
001484                                    ben-pct-len      (m1)
001485                                    EXCL-PER-LEN     (M1)
001486                                    cov-ends-len     (m1)
001487                                    acc-per-ends-len (m1)
001488                                    crit-per-len     (m1)
001489                                    rec-cp-len       (m1)
001490                                    rtw-mos-len      (m1)
001491                                    max-ext-len      (m1)
001492              move zeros         to max-att-age  (m1)
001493*                                   min-age      (m1)
001494                                    WAIT-PR      (m1)
001495                                    max-amt-in   (m1)
001496                                    ben-pct-in   (m1)
001497                                    EXCL-PERiod  (M1)
001498                                    cov-ends     (m1)
001499                                    acc-per-ends (m1)
001500                                    crit-per     (m1)
001501                                    rec-crit-per (m1)
001502                                    rtw-mos      (m1)
001503                                    max-ext      (m1)
001504           end-if
001505
001506           IF PROD-CODE (M1) = 'P' OR 'L' OR 'A'
001507                            OR 'I' OR 'G' or ' ' or 'F' OR 'O'
001508                            OR 'B' OR 'H'
001509              MOVE AL-UANON      TO PROD-CODE-ATTRB (M1)
001510           ELSE
001511              MOVE ER-9999       TO EMI-ERROR
001512              perform 9900-ERROR-FORMAT thru 9900-exit
001513              MOVE -1            TO PROD-CODE-LEN (M1)
001514              MOVE AL-UABON      TO PROD-CODE-ATTRB (M1)
001515           END-IF
001516        END-IF
001517        IF MAX-ATT-AGE-LEN (M1) > +0
001518           MOVE MAX-ATT-AGE (M1)  TO DEEDIT-FIELD
001519           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001520           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001521              MOVE WS-NUMVAL-OF-DEEDIT TO MAX-ATT-AGE (M1)
001522              MOVE AL-UANON      TO MAX-ATT-AGE-ATTRB (M1)
001523           ELSE
001524              MOVE ER-9999       TO EMI-ERROR
001525              perform 9900-ERROR-FORMAT thru 9900-exit
001526              MOVE -1            TO MAX-ATT-AGE-LEN (M1)
001527              MOVE AL-UABON      TO MAX-ATT-AGE-ATTRB (M1)
001528           END-IF
001529        END-IF
001530*       IF MIN-AGE-LEN (M1) > +0
001531*          MOVE MIN-AGE (M1)      TO DEEDIT-FIELD
001532*          PERFORM 8600-DEEDIT   THRU 8600-EXIT
001533*          IF WS-NUMVAL-OF-DEEDIT NUMERIC
001534*             MOVE WS-NUMVAL-OF-DEEDIT TO MIN-AGE (M1)
001535*             MOVE AL-UANON      TO MIN-AGE-ATTRB (M1)
001536*          ELSE
001537*             MOVE ER-9999       TO EMI-ERROR
001538*             perform 9900-ERROR-FORMAT thru 9900-exit
001539*             MOVE -1            TO MIN-AGE-LEN (M1)
001540*             MOVE AL-UABON      TO MIN-AGE-ATTRB (M1)
001541*          END-IF
001542*       END-IF
001543*       IF WAIT-PR-LEN (M1) > +0
001544*          MOVE WAIT-PR (M1)      TO DEEDIT-FIELD
001545*          PERFORM 8600-DEEDIT   THRU 8600-EXIT
001546*          IF WS-NUMVAL-OF-DEEDIT NUMERIC
001547*             MOVE WS-NUMVAL-OF-DEEDIT TO WAIT-PR (M1)
001548*             MOVE AL-UANON      TO WAIT-PR-ATTRB (M1)
001549*          ELSE
001550*             MOVE ER-9999       TO EMI-ERROR
001551*             perform 9900-ERROR-FORMAT thru 9900-exit
001552*             MOVE -1            TO WAIT-PR-LEN (M1)
001553*             MOVE AL-UABON      TO WAIT-PR-ATTRB (M1)
001554*          END-IF
001555*       END-IF
001556        IF MAX-TERM-LEN (M1) > +0
001557           MOVE MAX-TERM (M1)      TO DEEDIT-FIELD
001558           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001559           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001560              MOVE WS-NUMVAL-OF-DEEDIT TO MAX-TERM (M1)
001561              MOVE AL-UANON      TO MAX-TERM-ATTRB (M1)
001562           ELSE
001563              MOVE ER-9999       TO EMI-ERROR
001564              perform 9900-ERROR-FORMAT thru 9900-exit
001565              MOVE -1            TO MAX-TERM-LEN (M1)
001566              MOVE AL-UABON      TO MAX-TERM-ATTRB (M1)
001567           END-IF
001568        END-IF
001569        IF MAX-AMT-LEN (M1) > +0
001570           MOVE MAX-AMT-IN (M1)  TO DEEDIT-FIELD
001571           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001572           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001573              MOVE WS-NUMVAL-OF-DEEDIT
001574                                 TO MAX-AMT-IN (M1)
001575              MOVE AL-UANON      TO MAX-AMT-ATTRB (M1)
001576           ELSE
001577              MOVE ER-9999       TO EMI-ERROR
001578              perform 9900-ERROR-FORMAT thru 9900-exit
001579              MOVE -1            TO MAX-AMT-LEN (M1)
001580              MOVE AL-UABON      TO MAX-AMT-ATTRB (M1)
001581           END-IF
001582        END-IF
001583        IF ben-pct-LEN (M1) > +0
001584           MOVE ben-pct-IN (M1)  TO DEEDIT-FIELD
001585           compute ws-9v999-of-deedit =
001586              function numval(deedit-field)
001587           IF WS-9V999-OF-DEEDIT NUMERIC
001588              MOVE WS-9v999-OF-DEEDIT
001589                                 TO ben-pct-IN (M1)
001590              MOVE AL-UANON      TO ben-pct-ATTRB (M1)
001591           ELSE
001592              MOVE ER-7132       TO EMI-ERROR
001593              perform 9900-ERROR-FORMAT thru 9900-exit
001594              MOVE -1            TO ben-pct-LEN (M1)
001595              MOVE AL-UABON      TO ben-pct-ATTRB (M1)
001596           END-IF
001597        END-IF
001598        IF PRE-EXIST-LEN (M1) > +0
001599           MOVE AL-UANON         TO PRE-EXIST-ATTRB (M1)
001600        END-IF
001601        IF EXCL-PER-LEN (M1) > +0
001602           MOVE EXCL-PERIOD (M1)      TO DEEDIT-FIELD
001603           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001604           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001605              MOVE WS-NUMVAL-OF-DEEDIT TO EXCL-PERIOD (M1)
001606              MOVE AL-UANON      TO EXCL-PER-ATTRB (M1)
001607           ELSE
001608              MOVE ER-9999       TO EMI-ERROR
001609              perform 9900-ERROR-FORMAT thru 9900-exit
001610              MOVE -1            TO EXCL-PER-LEN (M1)
001611              MOVE AL-UABON      TO EXCL-PER-ATTRB (M1)
001612           END-IF
001613        END-IF
001614        IF COV-ENDS-LEN (M1) > +0
001615           MOVE COV-ENDS (M1)      TO DEEDIT-FIELD
001616           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001617           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001618              MOVE WS-NUMVAL-OF-DEEDIT TO COV-ENDS (M1)
001619              MOVE AL-UANON      TO COV-ENDS-ATTRB (M1)
001620           ELSE
001621              MOVE ER-9999       TO EMI-ERROR
001622              perform 9900-ERROR-FORMAT thru 9900-exit
001623              MOVE -1            TO COV-ENDS-LEN (M1)
001624              MOVE AL-UABON      TO COV-ENDS-ATTRB (M1)
001625           END-IF
001626        END-IF
001627        IF ACC-PER-ENDS-LEN (M1) > +0
001628           MOVE ACC-PER-ENDS (M1) TO DEEDIT-FIELD
001629           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001630           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001631              MOVE WS-NUMVAL-OF-DEEDIT TO ACC-PER-ENDS-OUT (M1)
001632              MOVE AL-UANON      TO ACC-PER-ENDS-ATTRB (M1)
001633           ELSE
001634              MOVE ER-9999       TO EMI-ERROR
001635              perform 9900-ERROR-FORMAT thru 9900-exit
001636              MOVE -1            TO ACC-PER-ENDS-LEN (M1)
001637              MOVE AL-UABON      TO ACC-PER-ENDS-ATTRB (M1)
001638           END-IF
001639        END-IF
001640        IF CRIT-PER-LEN (M1) > +0
001641           MOVE CRIT-PER (M1)    TO DEEDIT-FIELD
001642           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001643           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001644              MOVE WS-NUMVAL-OF-DEEDIT TO CRIT-PER-OUT (M1)
001645              MOVE AL-UANON      TO CRIT-PER-ATTRB (M1)
001646           ELSE
001647              MOVE ER-9999       TO EMI-ERROR
001648              perform 9900-ERROR-FORMAT thru 9900-exit
001649              MOVE -1            TO CRIT-PER-LEN (M1)
001650              MOVE AL-UABON      TO CRIT-PER-ATTRB (M1)
001651           END-IF
001652        END-IF
001653        IF REC-CP-LEN (M1) > +0
001654           EVALUATE TRUE
001655              WHEN REC-CRIT-PER (M1) = ' Y' OR ' N'
001656                 MOVE REC-CRIT-PER (M1) (2:1)
001657                                 TO REC-CRIT-PER (M1) (1:2)
001658                 MOVE SPACES     TO REC-CRIT-PER (M1) (2:1)
001659                 MOVE AL-UANON   TO REC-CP-ATTRB (M1)
001660              WHEN REC-CRIT-PER (M1) = 'Y ' OR 'N ' OR '  '
001661                 MOVE AL-UANON   TO REC-CP-ATTRB (M1)
001662              WHEN OTHER
001663                 MOVE REC-CRIT-PER (M1)
001664                                 TO DEEDIT-FIELD
001665                 PERFORM 8600-DEEDIT
001666                                 THRU 8600-EXIT
001667                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
001668                    MOVE WS-NUMVAL-OF-DEEDIT
001669                                 TO REC-CRIT-PER-N (M1)
001670                    MOVE AL-UANON TO REC-CP-ATTRB (M1)
001671                 ELSE
001672                    MOVE ER-9999 TO EMI-ERROR
001673                    PERFORM 9700-LINK-DATE-CONVERT
001674                                 THRU 9700-EXIT
001675                    MOVE -1      TO REC-CP-LEN (M1)
001676                    MOVE AL-UABON TO REC-CP-ATTRB (M1)
001677                 END-IF
001678           END-EVALUATE
001679        END-IF
001680        IF RTW-MOS-LEN (M1) > +0
001681           MOVE RTW-MOS  (M1)    TO DEEDIT-FIELD
001682           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001683           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001684              MOVE WS-NUMVAL-OF-DEEDIT TO RTW-MOS-OUT (M1)
001685              MOVE AL-UANON      TO RTW-MOS-ATTRB (M1)
001686           ELSE
001687              MOVE ER-9999       TO EMI-ERROR
001688              perform 9900-ERROR-FORMAT thru 9900-exit
001689              MOVE -1            TO RTW-MOS-LEN (M1)
001690              MOVE AL-UABON      TO RTW-MOS-ATTRB (M1)
001691           END-IF
001692        END-IF
001693        IF MAX-EXT-LEN (M1) > +0
001694           MOVE MAX-EXT  (M1)    TO DEEDIT-FIELD
001695           PERFORM 8600-DEEDIT   THRU 8600-EXIT
001696           IF WS-NUMVAL-OF-DEEDIT NUMERIC
001697              MOVE WS-NUMVAL-OF-DEEDIT TO MAX-EXT-OUT  (M1)
001698              MOVE AL-UANON      TO MAX-EXT-ATTRB (M1)
001699           ELSE
001700              MOVE ER-9999       TO EMI-ERROR
001701              perform 9900-ERROR-FORMAT thru 9900-exit
001702              MOVE -1            TO MAX-EXT-LEN (M1)
001703              MOVE AL-UABON      TO MAX-EXT-ATTRB (M1)
001704           END-IF
001705        END-IF
001706     END-PERFORM
001707
001708     IF TRUNCL > ZEROS
001709        IF TRUNCI = 'Y' OR 'N' OR ' '
001710           CONTINUE
001711        ELSE
001712           MOVE ER-9999          TO EMI-ERROR
001713           perform 9900-ERROR-FORMAT thru 9900-exit
001714           MOVE -1               TO TRUNCL
001715           MOVE AL-UABON         TO TRUNCA
001716        END-IF
001717     END-IF
001718
001719     .
001720 6000-EXIT.
001721     EXIT.
001722
001723     EJECT
001724 7000-BUILD-OUTPUT-MAP.
001725
001726     MOVE LOW-VALUES                 TO  EL159AO
001727     MOVE PI-COMPANY-CD              TO  PI-PREV-COMPANY-CD
001728     MOVE PD-STATE                   TO  STATEO
001729                                         PI-PREV-STATE
001730     MOVE PD-PRODUCT-CD              TO  PRODCDO
001731                                         PI-PREV-PROD-CD
001732     MOVE PD-BEN-TYPE                TO  BENTYPO
001733                                         PI-PREV-BEN-TYPE
001734     MOVE PD-BEN-CODE                TO  BENCODEO
001735                                         PI-PREV-BEN-CODE
001736     IF PD-PROD-EXP-DT = HIGH-VALUES
001737         MOVE '99/99/9999'           TO  EXPDTO
001738         MOVE HIGH-VALUES            TO  PI-PREV-EXP-DT
001739     ELSE
001740         MOVE PD-PROD-EXP-DT         TO  DC-BIN-DATE-1
001741                                         PI-PREV-EXP-DT
001742         MOVE ' '                    TO  DC-OPTION-CODE
001743         MOVE +0                     TO  DC-ELAPSED-DAYS
001744                                         DC-ELAPSED-MONTHS
001745         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001746         IF NO-CONVERSION-ERROR
001747             MOVE DC-GREG-DATE-A-EDIT TO EXPDTO
001748         ELSE
001749             MOVE LOW-VALUES         TO  EXPDTO.
001750
001751     MOVE PD-PRODUCT-DESC        TO PDESCO
001752     MOVE PD-1ST-YR-ADMIN-ALLOW  TO AFA-AMT-OUT
001753
001754     PERFORM VARYING M1 FROM +1 BY +1 UNTIL
001755        M1 > +8
001756        IF R1 < 10
001757           MOVE R1                   TO WS-LINE-NBR-1
001758           MOVE ')'                  TO WS-LINE-NBR-P
001759        ELSE
001760           IF R1 > 11
001761              MOVE LOW-VALUES           TO EL159-PROD-TABLE (M1)
001762              MOVE SPACES               TO LINE-NBR (M1)
001763                                           WS-LINE-NBR-CHAR
001764              MOVE AL-SADON             TO PROD-CODE-ATTRB (M1)
001765                                           MAX-ATT-AGE-ATTRB (M1)
001766*                                          MIN-AGE-ATTRB  (M1)
001767                                           WAIT-PR-ATTRB  (M1)
001768                                           MAX-TERM-ATTRB (M1)
001769                                           ben-pct-attrb  (M1)
001770                                           MAX-AMT-ATTRB  (M1)
001771                                           PRE-EXIST-ATTRB(M1)
001772                                           EXCL-PER-ATTRB (M1)
001773                                           COV-ENDS-ATTRB (M1)
001774                                           ACC-PER-ENDS-ATTRB (M1)
001775                                           CRIT-PER-ATTRB (M1)
001776                                           REC-CP-ATTRB   (M1)
001777                                           RTW-MOS-ATTRB  (M1)
001778                                           MAX-EXT-ATTRB  (M1)
001779           ELSE
001780              MOVE R1                   TO WS-LINE-NBR-2
001781              MOVE ')'                  TO WS-LINE-NBR-P2
001782           END-IF
001783        END-IF
001784        MOVE WS-LINE-NBR-CHAR        TO LINE-NBR (M1)
001785        IF PD-PROD-CODE (R1) NOT = SPACES
001786          AND R1 NOT > 11
001787           MOVE PD-PROD-CODE (R1)       TO PROD-CODE (M1)
001788           MOVE PD-MAX-ATT-AGE (R1)     TO MAX-ATT-AGE (M1)
001789*          MOVE PD-MIN-ISSUE-AGE (R1)   TO MIN-AGE (M1)
001790           MOVE PD-WAIT-PERIOD (R1)     TO WAIT-PR (M1)
001791           MOVE PD-MAX-TERM (R1)        TO MAX-TERM (M1)
001792           MOVE PD-MAX-AMT (R1)         TO MAX-AMT-OUT (M1)
001793           if pd-ben-pct (R1) not numeric
001794              move zeros to pd-ben-pct (R1)
001795           end-if
001796           move pd-ben-pct (R1)         to ben-pct-out (m1)
001797           MOVE PD-PRE-EXIST-EXCL-TYPE (R1) TO PRE-EXIST (M1)
001798           MOVE PD-EXCLUSION-PERIOD-DAYS (R1) TO EXCL-PERIOD (M1)
001799           MOVE PD-COVERAGE-ENDS-MOS (R1) TO COV-ENDS (M1)
001800           MOVE PD-ACCIDENT-ONLY-MOS (R1) TO ACC-PER-ENDS-OUT (M1)
001801           MOVE PD-CRIT-PERIOD (R1)     TO CRIT-PER-OUT (M1)
001802           MOVE PD-REC-CP-ALPHA    (R1) TO REC-CRIT-PER (M1)
001803           MOVE PD-RTW-MOS (R1)         TO RTW-MOS-OUT (M1)
001804           MOVE PD-MAX-EXTENSION (R1)   TO MAX-EXT-OUT (M1)
001805        END-IF
001806        ADD 1 TO R1
001807     END-PERFORM
001808     IF PAGE-NBR = 1
001809       AND PD-PROD-CODE (9) > SPACES
001810        MOVE 'YES' TO MORERECO
001811     ELSE
001812        MOVE 'NO'  TO MORERECO
001813     END-IF
001814
001815     IF PD-TRUNCATED = 'Y'
001816        MOVE 'Y'                     TO TRUNCO
001817     ELSE
001818        MOVE 'N'                     TO TRUNCO
001819     END-IF
001820     MOVE PD-LAST-MAINT-BY           TO  MAINTBYO
001821                                         PI-UPDATE-BY.
001822     MOVE PD-LAST-MAINT-HHMMSS       TO  TIME-IN
001823                                         PI-UPDATE-HHMMSS.
001824     MOVE TIME-OUT                   TO  MAINTATO.
001825     MOVE PD-LAST-MAINT-DT           TO  DC-BIN-DATE-1.
001826     MOVE ' '                        TO  DC-OPTION-CODE.
001827     MOVE +0                         TO  DC-ELAPSED-DAYS
001828                                         DC-ELAPSED-MONTHS.
001829     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
001830     IF NO-CONVERSION-ERROR
001831         MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO
001832     ELSE
001833         MOVE LOW-VALUES             TO  MAINTONO.
001834
001835
001836     MOVE -1                         TO  MAINTL.
001837     MOVE AL-UANON                   TO  STATEA
001838                                         PRODCDA
001839                                         BENTYPA
001840                                         BENCODEA
001841                                         EXPDTA
001842     GO TO 8100-SEND-INITIAL-MAP.
001843
001844 7100-READ-BENEFIT.
001845
001846     
      * EXEC CICS HANDLE CONDITION
001847*        NOTFND   (7120-NOT-FOUND)
001848*    END-EXEC.
      *    MOVE '"$I                   ! % #00005570' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303035353730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001849
001850     
      * EXEC CICS READ
001851*        DATASET   (ELCNTL-FILE-ID)
001852*        RIDFLD    (ELCNTL-KEY)
001853*        SET       (ADDRESS OF CONTROL-FILE)
001854*        GTEQ
001855*    END-EXEC.
      *    MOVE '&"S        G          (   #00005574' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303035353734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001856
001857     IF PI-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR
001858        ELCNTL-RECORD-TYPE IS NOT EQUAL TO CF-RECORD-TYPE
001859         GO TO 7120-NOT-FOUND.
001860
001861     MOVE +1                         TO  SUB-1.
001862
001863 7110-LOOP.
001864
001865     IF SUB-1 IS EQUAL TO +9
001866         GO TO 7120-NOT-FOUND.
001867
001868     IF WS-BENE-CODE IS NOT EQUAL TO CF-BENEFIT-CODE (SUB-1)
001869         ADD +1                      TO  SUB-1
001870         GO TO 7110-LOOP.
001871
001872     MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.
001873     GO TO 7199-EXIT.
001874
001875 7120-NOT-FOUND.
001876     MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.
001877
001878 7199-EXIT.
001879     EXIT.
001880     EJECT
001881 8000-READ-CNTL.
001882
001883     
      * EXEC CICS HANDLE CONDITION
001884*        NOTFND   (8009-NOTFND)
001885*    END-EXEC.
      *    MOVE '"$I                   ! & #00005607' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303035363037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001886
001887     
      * EXEC CICS READ
001888*        DATASET   (ELCNTL-FILE-ID)
001889*        RIDFLD    (ELCNTL-KEY)
001890*        SET       (ADDRESS OF CONTROL-FILE)
001891*    END-EXEC.
      *    MOVE '&"S        E          (   #00005611' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035363131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001892
001893     MOVE 'Y'                         TO  WS-CNTL-REC-FOUND-SW.
001894     GO TO 8010-EXIT.
001895
001896 8009-NOTFND.
001897     MOVE 'N'                         TO  WS-CNTL-REC-FOUND-SW.
001898
001899 8010-EXIT.
001900     EXIT.
001901     EJECT
001902 8100-SEND-INITIAL-MAP.
001903
001904     MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
001905     MOVE EIBTIME                    TO  TIME-IN.
001906     MOVE SAVE-DATE                  TO  DATEO.
001907     MOVE TIME-OUT                   TO  TIMEO.
001908
001909     
      * EXEC CICS SEND
001910*        MAP      (WS-MAP-NAME)
001911*        MAPSET   (MAPSET-NAME)
001912*        FROM     (EL159AO)
001913*        ERASE
001914*        CURSOR
001915*    END-EXEC.
           MOVE LENGTH OF
            EL159AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005633' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303035363333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159AO, 
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
           
001916
001917     MOVE '159A'                 TO PI-CURRENT-SCREEN-NO
001918
001919     GO TO 9100-RETURN-TRAN
001920
001921     .
001922 8200-SEND-DATAONLY.
001923
001924     MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
001925     MOVE EIBTIME                    TO  TIME-IN.
001926     MOVE SAVE-DATE                  TO  DATEO.
001927     MOVE TIME-OUT                   TO  TIMEO.
001928
001929     
      * EXEC CICS SEND
001930*        MAP      (WS-MAP-NAME)
001931*        MAPSET   (MAPSET-NAME)
001932*        FROM     (EL159AO)
001933*        DATAONLY
001934*        CURSOR
001935*    END-EXEC.
           MOVE LENGTH OF
            EL159AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005653' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303035363533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159AO, 
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
           
001936
001937     GO TO 9100-RETURN-TRAN
001938
001939     .
001940 8300-SEND-TEXT.
001941
001942     
      * EXEC CICS SEND TEXT
001943*        FROM  (LOGOFF-TEXT)
001944*        LENGTH(LOGOFF-LENGTH)
001945*        ERASE
001946*        FREEKB
001947*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005666' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303035363636' TO DFHEIV0
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
           
001948
001949     
      * EXEC CICS RETURN
001950*        END-EXEC.
      *    MOVE '.(                    ''   #00005673' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035363733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001951
001952     .
001953 8500-DEEDIT.
001954
001955     display ' before ' deedit-field
001956     MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
001957                                 TO WS-999V99-OF-DEEDIT
001958     display ' after  ' WS-999V99-OF-DEEDIT
001959
001960     .
001961 8500-EXIT.
001962     EXIT.
001963
001964
001965 8600-DEEDIT.
001966
001967     MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
001968                                 TO WS-NUMVAL-OF-DEEDIT
001969
001970     .
001971 8600-EXIT.
001972     EXIT.
001973
001974 8800-UNAUTHORIZED-ACCESS.
001975     MOVE UNACCESS-MSG               TO  LOGOFF-MSG.
001976     GO TO 8300-SEND-TEXT.
001977
001978 8810-PF23.
001979     MOVE EIBAID                     TO  PI-ENTRY-CD-1.
001980     MOVE XCTL-005                   TO  PGM-NAME.
001981     GO TO 9300-XCTL.
001982
001983 8870-NOTOPEN.
001984
001985     MOVE LOW-VALUES                 TO  EL159AO.
001986     MOVE -1                         TO  MAINTL.
001987     MOVE ER-0701                    TO  EMI-ERROR.
001988     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001989     GO TO 8100-SEND-INITIAL-MAP.
001990
001991 8880-NOT-FOUND.
001992
001993     MOVE ER-0702                    TO  EMI-ERROR.
001994     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001995     MOVE -1                         TO  BENTYPL
001996     MOVE AL-UABON                   TO  BENTYPA BENCODEA EXPDTA
001997     GO TO 8100-SEND-INITIAL-MAP.
001998
001999     .
002000 9100-RETURN-TRAN.
002001     MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.
002002
002003     
      * EXEC CICS RETURN
002004*        TRANSID    (TRANS-ID)
002005*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
002006*        LENGTH     (PI-COMM-LENGTH)
002007*    END-EXEC.
      *    MOVE '.(CT                  ''   #00005727' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035373237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002008
002009 9200-RETURN-MAIN-MENU.
002010
002011     MOVE XCTL-626                   TO  PGM-NAME.
002012     GO TO 9300-XCTL.
002013
002014 9300-XCTL.
002015     
      * EXEC CICS XCTL
002016*        PROGRAM    (PGM-NAME)
002017*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
002018*        LENGTH     (PI-COMM-LENGTH)
002019*    END-EXEC.
      *    MOVE '.$C                   %   #00005739' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035373339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002020
002021 9400-CLEAR.
002022     MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.
002023     GO TO 9300-XCTL.
002024
002025 9500-PF12.
002026     MOVE XCTL-010                   TO  PGM-NAME.
002027     GO TO 9300-XCTL.
002028
002029 9600-PGMID-ERROR.
002030     
      * EXEC CICS HANDLE CONDITION
002031*        PGMIDERR   (8300-SEND-TEXT)
002032*    END-EXEC.
      *    MOVE '"$L                   ! '' #00005754' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303035373534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002033
002034     MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.
002035     MOVE ' '                        TO  PI-ENTRY-CD-1.
002036     MOVE XCTL-005                   TO  PGM-NAME.
002037     MOVE PGM-NAME                   TO  LOGOFF-PGM.
002038     MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.
002039     GO TO 9300-XCTL.
002040
002041     EJECT
002042 9700-LINK-DATE-CONVERT.
002043     
      * EXEC CICS LINK
002044*        PROGRAM    ('ELDATCV')
002045*        COMMAREA   (DATE-CONVERSION-DATA)
002046*        LENGTH     (DC-COMM-LENGTH)
002047*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005767' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035373637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002048
002049 9700-EXIT.
002050     EXIT.
002051
002052 9900-ERROR-FORMAT.
002053     IF NOT EMI-ERRORS-COMPLETE
002054         MOVE LINK-001               TO  PGM-NAME
002055         
      * EXEC CICS LINK
002056*            PROGRAM    (PGM-NAME)
002057*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
002058*            LENGTH     (EMI-COMM-LENGTH)
002059*        END-EXEC.
      *    MOVE '."C                   (   #00005779' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035373739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002060
002061 9900-EXIT.
002062     EXIT.
002063
002064 9990-ABEND.
002065     MOVE LINK-004                   TO  PGM-NAME.
002066     MOVE DFHEIBLK                   TO  EMI-LINE1.
002067     
      * EXEC CICS LINK
002068*        PROGRAM   (PGM-NAME)
002069*        COMMAREA  (EMI-LINE1)
002070*        LENGTH    (72)
002071*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005791' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035373931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002072
002073     GO TO 8100-SEND-INITIAL-MAP.
002074
002075     EJECT
002076 9995-SECURITY-VIOLATION.
002077*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005819' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035383139' TO DFHEIV0
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
002078
002079 9995-EXIT.
002080     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL159' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 5000-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5100-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7120-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8009-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL159' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
