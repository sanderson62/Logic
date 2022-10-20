      *((program: EL6521.cl2))
000001 ID DIVISION.
000002
000003 PROGRAM-ID. EL6521.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 11/18/94 10:26:25.
000007*                            VMOD=2.004
000008*
000009*AUTHOR.     LOGIC,INC.
000010*            DALLAS, TEXAS.
000011
000012*DATE-COMPILED.
000013*SECURITY.   *****************************************************
000014*            *                                                   *
000015*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000016*            *                                                   *
000017*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000018*                                                                *
000019*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000020*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000021*            *                                                   *
000022*            *****************************************************
000023*
000024*REMARKS.    TRANSACTION - EXD8 - COMPENSATION GA STATUS DATA.
000025*
000026******************************************************************
000027*                   C H A N G E   L O G
000028*
000029* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000030*-----------------------------------------------------------------
000031*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000032* EFFECTIVE    NUMBER
000033*-----------------------------------------------------------------
000034* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000035* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
000036* 092707    2004020600003  PEMA  ADD DELIVER TO MEL SWITCH
000037* 030211    2010012100001  PEMA  ADD RDS EMAILS TO LOGIC
000038* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000039* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000040* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000041******************************************************************
000042
000043 ENVIRONMENT DIVISION.
000044
000045     EJECT
000046 DATA DIVISION.
000047 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000048 77  FILLER  PIC X(32)  VALUE '********************************'.
000049 77  FILLER  PIC X(32)  VALUE '*    EL6521 WORKING STORAGE    *'.
000050 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.
000051
000052 01  WS-DATE-AREA.
000053     05  WS-SAVE-DATE                PIC X(8) VALUE SPACES.
000054     05  WS-SAVE-BIN-DT              PIC XX   VALUE SPACES.
000055     05  WS-EFF-YMD                  PIC X(6).
000056     05  WS-SAVE-BIN-EFFDT           PIC XX   VALUE SPACES.
000057     05  WS-TRM-YMD                  PIC X(6).
000058     05  WS-SAVE-BIN-TRMDT           PIC XX   VALUE SPACES.
000059     05  WS-ACH-SW                   PIC X    VALUE 'N'.
000060         88  ACH-HAS-CHANGED             VALUE 'Y'.
000061         88  NO-ACH-CHANGE               VALUE 'N'.
000062     05  SUPPRESS-MAP-SW             PIC X    VALUE SPACE.
000063         88  DO-NOT-MOVE-TO-MAP          VALUE 'N'.
000064         88  MOVE-TO-MAP                 VALUE 'Y'.
000065     05  MAP-CHANGED-SW              PIC X    VALUE 'N'.
000066         88  MAP-NOT-CHANGED             VALUE 'N'.
000067         88  MAP-CHANGED                 VALUE 'Y'.
000068     05  BILL-INST-SW                PIC X.
000069         88  BILL-INST-CHANGED           VALUE 'Y'.
000070
000071
000072 01  WS-RESPONSE                 PIC S9(8)   COMP.
000073     88  RESP-NORMAL                  VALUE +00.
000074     88  RESP-NOTFND                  VALUE +13.
000075     88  RESP-NOTOPEN                 VALUE +19.
000076     88  RESP-ENDFILE                 VALUE +20.
000077
000078 01  STANDARD-AREAS.
000079     12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
000080     12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1024.
000081     12  WS-SUB                      PIC S9(4) COMP VALUE +0.
000082     12  MAP-NAME                    PIC X(8) VALUE 'EL6521A'.
000083     12  MAPSET-NAME                 PIC X(8) VALUE 'EL6521S'.
000084     12  SCREEN-NUMBER               PIC X(4) VALUE '652B'.
000085     12  TRANS-ID                    PIC X(4) VALUE 'EXD8'.
000086     12  EL611-TRANS-ID              PIC X(4) VALUE 'EXL3'.
000087     12  EL6525-TRANS-ID             PIC X(4) VALUE 'EXDF'.
000088     12  EL6526-TRANS-ID             PIC X(4) VALUE 'EXDG'.
000089     12  THIS-PGM                    PIC X(8) VALUE 'EL6521'.
000090     12  PGM-NAME                    PIC X(8).
000091     12  TIME-IN                     PIC S9(7).
000092     12  TIME-OUT-R  REDEFINES TIME-IN.
000093         16  FILLER                  PIC X.
000094         16  TIME-OUT                PIC 99V99.
000095         16  FILLER                  PIC XX.
000096     12  XCTL-005                    PIC X(8) VALUE 'EL005'.
000097     12  XCTL-010                    PIC X(8) VALUE 'EL010'.
000098     12  XCTL-626                    PIC X(8) VALUE 'EL626'.
000099     12  XCTL-611                    PIC X(8) VALUE 'EL611'.
000100     12  XCTL-6525                   PIC X(8) VALUE 'EL6525'.
000101     12  XCTL-6526                   PIC X(8) VALUE 'EL6526'.
000102     12  XCTL-652                    PIC X(8) VALUE 'EL652'.
000103     12  LINK-001                    PIC X(8) VALUE 'EL001'.
000104     12  LINK-004                    PIC X(8) VALUE 'EL004'.
000105     12  QID.
000106         16  QID-TERM                PIC X(4) VALUE SPACES.
000107         16  FILLER                  PIC X(4) VALUE '521A'.
000108     12  MAP-LENGTH                  PIC S9(4) VALUE +600  COMP.
000109     12  ERCOBI-FILE-ID              PIC X(8) VALUE 'ERCOBI'.
000110     12  ERCOMP-FILE-ID              PIC X(8) VALUE 'ERCOMP'.
000111     12  ELACHP-FILE-ID              PIC X(8) VALUE 'ELACHP'.
000112     12  ELBANK-FILE-ID              PIC X(8) VALUE 'ELBANK'.
000113     12  WS-AGENT-BANK-DESC.
000114         16 FILLER                   PIC X(7) VALUE '  AGENT'.
000115         16 FILLER                   PIC X(1) VALUE X'7D'.
000116         16 FILLER                   PIC X(15)
000117                                      VALUE 'S BANK ACCOUNT:'.
000118
000119     12  DEEDIT-FIELD                PIC X(15).
000120     12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
000121
000122
000123     12  ERCOBI-KEY.
000124         16  ERCOBI-COMPANY-CD      PIC X.
000125         16  ERCOBI-STMT-OWNER      PIC X(4).
000126         16  ERCOBI-RGID            PIC X(12).
000127
000128     12  WS-BILL-INST-SW            PIC X  VALUE SPACES.
000129         88  BILLING-INSTRUCTIONS-FOUND  VALUE 'Y'.
000130     12  ERROR-MESSAGES.
000131         16  ER-0000                 PIC X(4) VALUE '0000'.
000132         16  ER-0004                 PIC X(4) VALUE '0004'.
000133         16  ER-0008                 PIC X(4) VALUE '0008'.
000134         16  ER-0029                 PIC X(4) VALUE '0029'.
000135         16  ER-0068                 PIC X(4) VALUE '0068'.
000136         16  ER-0070                 PIC X(4) VALUE '0070'.
000137         16  ER-0348                 PIC X(4) VALUE '0348'.
000138         16  ER-0454                 PIC X(4) VALUE '0454'.
000139         16  ER-0876                 PIC X(4) VALUE '0876'.
000140         16  ER-1228                 PIC X(4) VALUE '1228'.
000141         16  ER-1626                 PIC X(4) VALUE '1626'.
000142         16  ER-1629                 PIC X(4) VALUE '1629'.
000143         16  ER-2039                 PIC X(4) VALUE '2039'.
000144         16  ER-2233                 PIC X(4) VALUE '2233'.
000145         16  ER-2797                 PIC X(4) VALUE '2797'.
000146         16  ER-7430                 PIC X(4) VALUE '7430'.
000147         16  ER-7431                 PIC X(4) VALUE '7431'.
000148         16  ER-7432                 PIC X(4) VALUE '7432'.
000149         16  ER-7434                 PIC X(4) VALUE '7434'.
000150         16  ER-7435                 PIC X(4) VALUE '7435'.
000151         16  ER-7436                 PIC X(4) VALUE '7436'.
000152         16  ER-7438                 PIC X(4) VALUE '7438'.
000153         16  ER-7440                 PIC X(4) VALUE '7440'.
000154         16  ER-7447                 PIC X(4) VALUE '7447'.
000155         16  ER-7449                 PIC X(4) VALUE '7449'.
000156         16  ER-7462                 PIC X(4) VALUE '7462'.
000157         16  ER-7465                 PIC X(4) VALUE '7465'.
000158         16  ER-7468                 PIC X(4) VALUE '7468'.
000159         16  ER-7469                 PIC X(4) VALUE '7469'.
000160         16  ER-8799                 PIC X(4) VALUE '8799'.
000161         16  ER-9388                 PIC X(4) VALUE '9388'.
000162         16  ER-9399                 PIC X(4) VALUE '9399'.
000163         16  ER-9999                 PIC X(4) VALUE '9999'.
000164
000165     12  ELCNTL-KEY.
000166         16  CNTL-COMP-ID            PIC X(3) VALUE SPACES.
000167         16  CNTL-REC-TYPE           PIC X    VALUE SPACES.
000168         16  CNTL-ACCESS             PIC X(4) VALUE SPACES.
000169         16  CNTL-SEQ-NO             PIC S9(4) VALUE +0  COMP.
000170
000171     12  WS-BANK-INFORMATION.
000172         16 WS-BANK-DATA.
000173             20 WS-TRANSIT1              PIC X(4).
000174             20 WS-TRANSIT2              PIC X(4).
000175             20 WS-BKACCTI               PIC X(17).
000176         16 WS-ACTCDI                    PIC X(1).
000177
000178     EJECT
000179*    COPY ELCLOGOF.
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
000180     EJECT
000181*    COPY ELCDATE.
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
000182     EJECT
000183*    COPY ELCATTR.
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
000184     EJECT
000185*    COPY ELCEMIB.
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
000186     EJECT
000187*    COPY ELCSCTM.
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
000188     EJECT
000189*    COPY ELCSCRTY.
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
000190     EJECT
000191*    COPY ELCINTF.
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
000192     12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
000193         16  PI-MAINT                PIC  X.
000194         16  PI-CHECK-TYPE           PIC  X.
000195         16  PI-CHECK-CARRY-BAL      PIC  X.
000196         16  PI-FIRST-TIME-SW        PIC  X.
000197             88  FIRST-TIME                  VALUE 'Y'.
000198         16  PI-ERCOMP-EOF-SW        PIC  X.
000199             88  ERCOMP-EOF                  VALUE 'Y'.
000200         16  PI-SAVE-PHONE           PIC  X(10).
000201         16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).
000202         16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.
000203         16  PI-ERCOMP-KEY.
000204             20  PI-ERC-GROUP-CD     PIC  X.
000205             20  PI-ERC-CARRIER      PIC  X.
000206             20  PI-ERC-GROUP        PIC  X(6).
000207             20  PI-ERC-RESP         PIC  X(10).
000208             20  PI-ERC-ACCT         PIC  X(10).
000209             20  PI-ERC-TYPE         PIC  X.
000210         16  PI-SAVE-ERCOMP-KEY      PIC  X(29).
000211         16  PI-BANK-TRANSIT-NUMBER.
000212             20  PI-BANK-COMPANY-CD  PIC X.
000213             20  PI-FEDERAL-NUMBER   PIC X(4).
000214             20  PI-BANK-NUMBER      PIC X(4).
000215         16  PI-BANK-ACCOUNT-NO      PIC X(17).
000216         16  PI-BANK-ACTION-CODE     PIC X.
000217         16  PI-ERCOBI-KEY.
000218             20  PI-ERCOBI-COMPANY-CD PIC X.
000219             20  PI-ERCOBI-STMT-OWNER PIC X(4).
000220             20  PI-ERCOBI-RGID      PIC X(12).
000221         16  PI-SAVE-ERCOBI-KEY      PIC X(17).
000222         16  FILLER                  PIC  X(500).
000223
000224     EJECT
000225*    COPY ELCAID.
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
000226 01  FILLER    REDEFINES DFHAID.
000227     12  FILLER                      PIC X(8).
000228     12  PF-VALUES                   PIC X       OCCURS 2.
000229
000230     EJECT
000231*    COPY EL6521S.
      *>>((file: EL6521S))
000001 01  EL6521AI.
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
000028     05  MAINTYPL PIC S9(0004) COMP.
000029     05  MAINTYPF PIC  X(0001).
000030     05  FILLER REDEFINES MAINTYPF.
000031         10  MAINTYPA PIC  X(0001).
000032     05  MAINTYPI PIC  X(0001).
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
000046     05  TYPEL PIC S9(0004) COMP.
000047     05  TYPEF PIC  X(0001).
000048     05  FILLER REDEFINES TYPEF.
000049         10  TYPEA PIC  X(0001).
000050     05  TYPEI PIC  X(0001).
000051*    -------------------------------
000052     05  FINRESPL PIC S9(0004) COMP.
000053     05  FINRESPF PIC  X(0001).
000054     05  FILLER REDEFINES FINRESPF.
000055         10  FINRESPA PIC  X(0001).
000056     05  FINRESPI PIC  X(0010).
000057*    -------------------------------
000058     05  COACCTL PIC S9(0004) COMP.
000059     05  COACCTF PIC  X(0001).
000060     05  FILLER REDEFINES COACCTF.
000061         10  COACCTA PIC  X(0001).
000062     05  COACCTI PIC  X(0010).
000063*    -------------------------------
000064     05  EFFDTL PIC S9(0004) COMP.
000065     05  EFFDTF PIC  X(0001).
000066     05  FILLER REDEFINES EFFDTF.
000067         10  EFFDTA PIC  X(0001).
000068     05  EFFDTI PIC  X(0008).
000069*    -------------------------------
000070     05  TRMDTL PIC S9(0004) COMP.
000071     05  TRMDTF PIC  X(0001).
000072     05  FILLER REDEFINES TRMDTF.
000073         10  TRMDTA PIC  X(0001).
000074     05  TRMDTI PIC  X(0008).
000075*    -------------------------------
000076     05  STATL PIC S9(0004) COMP.
000077     05  STATF PIC  X(0001).
000078     05  FILLER REDEFINES STATF.
000079         10  STATA PIC  X(0001).
000080     05  STATI PIC  X(0001).
000081*    -------------------------------
000082     05  WTHLDL PIC S9(0004) COMP.
000083     05  WTHLDF PIC  X(0001).
000084     05  FILLER REDEFINES WTHLDF.
000085         10  WTHLDA PIC  X(0001).
000086     05  WTHLDI PIC  S99V9999.
000087*    -------------------------------
000088     05  GADDL PIC S9(0004) COMP.
000089     05  GADDF PIC  X(0001).
000090     05  FILLER REDEFINES GADDF.
000091         10  GADDA PIC  X(0001).
000092     05  GADDI PIC  X(0001).
000093*    -------------------------------
000094     05  APCHKL PIC S9(0004) COMP.
000095     05  APCHKF PIC  X(0001).
000096     05  FILLER REDEFINES APCHKF.
000097         10  APCHKA PIC  X(0001).
000098     05  APCHKI PIC  X(0001).
000099*    -------------------------------
000100     05  MELSWL PIC S9(0004) COMP.
000101     05  MELSWF PIC  X(0001).
000102     05  FILLER REDEFINES MELSWF.
000103         10  MELSWA PIC  X(0001).
000104     05  MELSWI PIC  X(0001).
000105*    -------------------------------
000106     05  MDACTL PIC S9(0004) COMP.
000107     05  MDACTF PIC  X(0001).
000108     05  FILLER REDEFINES MDACTF.
000109         10  MDACTA PIC  X(0001).
000110     05  MDACTI PIC  X(0010).
000111*    -------------------------------
000112     05  MDDIVL PIC S9(0004) COMP.
000113     05  MDDIVF PIC  X(0001).
000114     05  FILLER REDEFINES MDDIVF.
000115         10  MDDIVA PIC  X(0001).
000116     05  MDDIVI PIC  X(0002).
000117*    -------------------------------
000118     05  MDCNTRL PIC S9(0004) COMP.
000119     05  MDCNTRF PIC  X(0001).
000120     05  FILLER REDEFINES MDCNTRF.
000121         10  MDCNTRA PIC  X(0001).
000122     05  MDCNTRI PIC  X(0004).
000123*    -------------------------------
000124     05  MDLOBL PIC S9(0004) COMP.
000125     05  MDLOBF PIC  X(0001).
000126     05  FILLER REDEFINES MDLOBF.
000127         10  MDLOBA PIC  X(0001).
000128     05  MDLOBI PIC  X(0006).
000129*    -------------------------------
000130     05  MDSTL PIC S9(0004) COMP.
000131     05  MDSTF PIC  X(0001).
000132     05  FILLER REDEFINES MDSTF.
000133         10  MDSTA PIC  X(0001).
000134     05  MDSTI PIC  X(0002).
000135*    -------------------------------
000136     05  MDAMTL PIC S9(0004) COMP.
000137     05  MDAMTF PIC  X(0001).
000138     05  FILLER REDEFINES MDAMTF.
000139         10  MDAMTA PIC  X(0001).
000140     05  MDAMTI PIC  S999999V99.
000141*    -------------------------------
000142     05  DELTOL PIC S9(0004) COMP.
000143     05  DELTOF PIC  X(0001).
000144     05  FILLER REDEFINES DELTOF.
000145         10  DELTOA PIC  X(0001).
000146     05  DELTOI PIC  X(0004).
000147*    -------------------------------
000148     05  RGIDL PIC S9(0004) COMP.
000149     05  RGIDF PIC  X(0001).
000150     05  FILLER REDEFINES RGIDF.
000151         10  RGIDA PIC  X(0001).
000152     05  RGIDI PIC  X(0012).
000153*    -------------------------------
000154     05  COMM1L PIC S9(0004) COMP.
000155     05  COMM1F PIC  X(0001).
000156     05  FILLER REDEFINES COMM1F.
000157         10  COMM1A PIC  X(0001).
000158     05  COMM1I PIC  X(0040).
000159*    -------------------------------
000160     05  COMM2L PIC S9(0004) COMP.
000161     05  COMM2F PIC  X(0001).
000162     05  FILLER REDEFINES COMM2F.
000163         10  COMM2A PIC  X(0001).
000164     05  COMM2I PIC  X(0040).
000165*    -------------------------------
000166     05  COMM3L PIC S9(0004) COMP.
000167     05  COMM3F PIC  X(0001).
000168     05  FILLER REDEFINES COMM3F.
000169         10  COMM3A PIC  X(0001).
000170     05  COMM3I PIC  X(0040).
000171*    -------------------------------
000172     05  COMM4L PIC S9(0004) COMP.
000173     05  COMM4F PIC  X(0001).
000174     05  FILLER REDEFINES COMM4F.
000175         10  COMM4A PIC  X(0001).
000176     05  COMM4I PIC  X(0040).
000177*    -------------------------------
000178     05  BKLITL PIC S9(0004) COMP.
000179     05  BKLITF PIC  X(0001).
000180     05  FILLER REDEFINES BKLITF.
000181         10  BKLITA PIC  X(0001).
000182     05  BKLITI PIC  X(0023).
000183*    -------------------------------
000184     05  TRNSIT1L PIC S9(0004) COMP.
000185     05  TRNSIT1F PIC  X(0001).
000186     05  FILLER REDEFINES TRNSIT1F.
000187         10  TRNSIT1A PIC  X(0001).
000188     05  TRNSIT1I PIC  X(0004).
000189*    -------------------------------
000190     05  TRDASHL PIC S9(0004) COMP.
000191     05  TRDASHF PIC  X(0001).
000192     05  FILLER REDEFINES TRDASHF.
000193         10  TRDASHA PIC  X(0001).
000194     05  TRDASHI PIC  X(0001).
000195*    -------------------------------
000196     05  TRNSIT2L PIC S9(0004) COMP.
000197     05  TRNSIT2F PIC  X(0001).
000198     05  FILLER REDEFINES TRNSIT2F.
000199         10  TRNSIT2A PIC  X(0001).
000200     05  TRNSIT2I PIC  X(0004).
000201*    -------------------------------
000202     05  BKDESCL PIC S9(0004) COMP.
000203     05  BKDESCF PIC  X(0001).
000204     05  FILLER REDEFINES BKDESCF.
000205         10  BKDESCA PIC  X(0001).
000206     05  BKDESCI PIC  X(0023).
000207*    -------------------------------
000208     05  BKACCTL PIC S9(0004) COMP.
000209     05  BKACCTF PIC  X(0001).
000210     05  FILLER REDEFINES BKACCTF.
000211         10  BKACCTA PIC  X(0001).
000212     05  BKACCTI PIC  X(0017).
000213*    -------------------------------
000214     05  ACTLITL PIC S9(0004) COMP.
000215     05  ACTLITF PIC  X(0001).
000216     05  FILLER REDEFINES ACTLITF.
000217         10  ACTLITA PIC  X(0001).
000218     05  ACTLITI PIC  X(0023).
000219*    -------------------------------
000220     05  ACTCDL PIC S9(0004) COMP.
000221     05  ACTCDF PIC  X(0001).
000222     05  FILLER REDEFINES ACTCDF.
000223         10  ACTCDA PIC  X(0001).
000224     05  ACTCDI PIC  X(0001).
000225*    -------------------------------
000226     05  CRSLITL PIC S9(0004) COMP.
000227     05  CRSLITF PIC  X(0001).
000228     05  FILLER REDEFINES CRSLITF.
000229         10  CRSLITA PIC  X(0001).
000230     05  CRSLITI PIC  X(0017).
000231*    -------------------------------
000232     05  CRSTATL PIC S9(0004) COMP.
000233     05  CRSTATF PIC  X(0001).
000234     05  FILLER REDEFINES CRSTATF.
000235         10  CRSTATA PIC  X(0001).
000236     05  CRSTATI PIC  X(0010).
000237*    -------------------------------
000238     05  ERRMSG1L PIC S9(0004) COMP.
000239     05  ERRMSG1F PIC  X(0001).
000240     05  FILLER REDEFINES ERRMSG1F.
000241         10  ERRMSG1A PIC  X(0001).
000242     05  ERRMSG1I PIC  X(0071).
000243*    -------------------------------
000244     05  ERRMSG2L PIC S9(0004) COMP.
000245     05  ERRMSG2F PIC  X(0001).
000246     05  FILLER REDEFINES ERRMSG2F.
000247         10  ERRMSG2A PIC  X(0001).
000248     05  ERRMSG2I PIC  X(0071).
000249*    -------------------------------
000250     05  PFENTERL PIC S9(0004) COMP.
000251     05  PFENTERF PIC  X(0001).
000252     05  FILLER REDEFINES PFENTERF.
000253         10  PFENTERA PIC  X(0001).
000254     05  PFENTERI PIC  99.
000255*    -------------------------------
000256     05  ACHPF1L PIC S9(0004) COMP.
000257     05  ACHPF1F PIC  X(0001).
000258     05  FILLER REDEFINES ACHPF1F.
000259         10  ACHPF1A PIC  X(0001).
000260     05  ACHPF1I PIC  X(0015).
000261 01  EL6521AO REDEFINES EL6521AI.
000262     05  FILLER            PIC  X(0012).
000263*    -------------------------------
000264     05  FILLER            PIC  X(0003).
000265     05  DATEO PIC  X(0008).
000266*    -------------------------------
000267     05  FILLER            PIC  X(0003).
000268     05  TIMEO PIC  99.99.
000269*    -------------------------------
000270     05  FILLER            PIC  X(0003).
000271     05  CMPNYIDO PIC  X(0003).
000272*    -------------------------------
000273     05  FILLER            PIC  X(0003).
000274     05  USERIDO PIC  X(0004).
000275*    -------------------------------
000276     05  FILLER            PIC  X(0003).
000277     05  MAINTYPO PIC  X(0001).
000278*    -------------------------------
000279     05  FILLER            PIC  X(0003).
000280     05  CARRIERO PIC  X(0001).
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  GROUPO PIC  X(0006).
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  TYPEO PIC  X(0001).
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  FINRESPO PIC  X(0010).
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  COACCTO PIC  X(0010).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  EFFDTO PIC  X(0008).
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  TRMDTO PIC  X(0008).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  STATO PIC  X(0001).
000302*    -------------------------------
000303     05  FILLER            PIC  X(0003).
000304     05  WTHLDO PIC  Z.9999.
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  GADDO PIC  X(0001).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  APCHKO PIC  X(0001).
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  MELSWO PIC  X(0001).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  MDACTO PIC  X(0010).
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  MDDIVO PIC  X(0002).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  MDCNTRO PIC  X(0004).
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  MDLOBO PIC  X(0006).
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  MDSTO PIC  X(0002).
000329*    -------------------------------
000330     05  FILLER            PIC  X(0003).
000331     05  MDAMTO PIC  ZZZZZ.99.
000332*    -------------------------------
000333     05  FILLER            PIC  X(0003).
000334     05  DELTOO PIC  X(0004).
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  RGIDO PIC  X(0012).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  COMM1O PIC  X(0040).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  COMM2O PIC  X(0040).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  COMM3O PIC  X(0040).
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  COMM4O PIC  X(0040).
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  BKLITO PIC  X(0023).
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  TRNSIT1O PIC  X(0004).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  TRDASHO PIC  X(0001).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  TRNSIT2O PIC  X(0004).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  BKDESCO PIC  X(0023).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  BKACCTO PIC  X(0017).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  ACTLITO PIC  X(0023).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  ACTCDO PIC  X(0001).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  CRSLITO PIC  X(0017).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  CRSTATO PIC  X(0010).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  ERRMSG1O PIC  X(0071).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  ERRMSG2O PIC  X(0071).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  PFENTERO PIC  99.
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  ACHPF1O PIC  X(0015).
000392*    -------------------------------
      *<<((file: EL6521S))
000232
000233     EJECT
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
000235 01  DFHCOMMAREA                     PIC X(1024).
000236
000237*    COPY ERCCOMP.
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
000238*    COPY ERCCOBI.
      *>>((file: ERCCOBI))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCOBI                             *
000005*                                                                *
000006*   ONLINE CREDIT SYSTEM                                         *
000007*                                                                *
000008*   FILE DESCRIPTION = COMPENSATION MASTER BILLING INSTRUCTIONS  *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 620   RECFORM = FIXED                          *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERCOBI                   RKP=2,LEN=17    *
000014*       ALTERNATE PATH = NONE                                    *
000015*                                                                *
000016*   LOG = NO                                                     *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018*                                                                *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 081808    2008061100001  PEMA  NEW FILE FOR BILLING INSTRUCTIONS
000028******************************************************************
000029
000030 01  COMP-BILLING-INSTRUCTIONS.
000031     12  BL-RECORD-ID                          PIC XX.
000032         88  VALID-BL-ID                          VALUE 'BL'.
000033
000034     12  BL-CONTROL-PRIMARY.
000035         16  BL-COMPANY-CD                     PIC X.
000036         16  BL-STMT-OWNER                     PIC X(4).
000037         16  BL-REPORT-GROUP-ID                PIC X(12).
000038
000039     12  BL-MAINT-INFORMATION.
000040         16  BL-LAST-MAINT-DT                  PIC XX.
000041         16  BL-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000042         16  BL-LAST-MAINT-USER                PIC X(4).
000043         16  FILLER                            PIC X(10).
000044
000045     12  BL-ACCOUNT-NAME                       PIC X(35).
000046     12  BL-CONTACT-NAME                       PIC X(35).
000047     12  BL-ADDR1                              PIC X(30).
000048     12  BL-ADDR2                              PIC X(30).
000049     12  BL-CITY                               PIC X(30).
000050     12  BL-STATE                              PIC XX.
000051     12  BL-ZIP.
000052         16  BL-ZIP-PRIME.
000053             20  BL-ZIP-PRI-1ST                PIC X.
000054                 88  BL-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
000055             20  FILLER                        PIC X(4).
000056         16  BL-ZIP-PLUS4                      PIC X(4).
000057     12  BL-CANADIAN-POSTAL-CODE  REDEFINES  BL-ZIP.
000058         16  BL-CAN-POSTAL-1                   PIC XXX.
000059         16  BL-CAN-POSTAL-2                   PIC XXX.
000060         16  FILLER                            PIC XXX.
000061*    12  FILLER                                PIC X(30).
000062     12  BL-CHECK-HANDLING                     PIC X.
000063         88  BL-CHECKS-NET                VALUE '1' ' '.
000064         88  BL-CHECKS-SEPARATE           VALUE '2'.
000065     12  BL-SPECIAL-INSTRUCTIONS.
000066         16  BL-SI-LINE-1                      PIC X(70).
000067         16  BL-SI-LINE-2                      PIC X(70).
000068         16  BL-SI-LINE-3                      PIC X(70).
000069         16  BL-SI-LINE-4                      PIC X(70).
000070         16  BL-SI-LINE-5                      PIC X(70).
000071     12  FILLER                                PIC X(59).
000072
000073
000074
000075******************************************************************
      *<<((file: ERCCOBI))
000239*    COPY ELCACHP.
      *>>((file: ELCACHP))
000001******************************************************************
000002*                                                                *
000003*                           ELCACHP                              *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ACH PRE-NOTIFICATION                      *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 120  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELACHP                         RKP=2,LEN=29   *
000013*       ALTERNATE (NONE)                                         *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  IN  COPYBOOK  ELCACHP                          *
000019******************************************************************
000020
000021 01  ACH-PRENOTIFICATION.
000022     12  AP-RECORD-ID                      PIC XX.
000023         88  VALID-AP-ID                      VALUE 'AP'.
000024
000025******************************************************************
000026*   BASE CLUSTER = ELACHP         (BASE KEY)      RKP=2,LEN=29   *
000027******************************************************************
000028
000029     12  AP-CONTROL-PRIMARY.
000030         16  AP-CONTROL-CD                 PIC X.
000031         16  AP-CARRIER                    PIC X.
000032         16  AP-GROUPING                   PIC X(6).
000033         16  AP-FIN-RESP                   PIC X(10).
000034         16  AP-ACCT-AGENT                 PIC X(10).
000035         16  AP-CO-TYPE                    PIC X.
000036
000037     12  AP-CONTROL-ALT   REDEFINES  AP-CONTROL-PRIMARY.
000038         16  AP-COMPANY-CD-ALT             PIC X.
000039         16  AP-BENEFICIARY                PIC X(10).
000040         16  FILLER                        PIC X(18).
000041
000042******************************************************************
000043*                 FILE SYNCHRONIZATION DATA                      *
000044******************************************************************
000045
000046     12  AP-FILE-SYNCH-DATA.
000047         16  AP-LAST-CHANGE-DT             PIC XX.
000048         16  AP-LAST-CHANGE-TIME           PIC S9(7)  COMP-3.
000049         16  AP-LAST-CHANGE-PROCESSOR      PIC X(4).
000050
000051******************************************************************
000052*                     PRENOTE INFORMATION
000053******************************************************************
000054
000055     12  AP-BANK-INFORMATION.
000056         16  AP-TRANSIT-NUMBER             PIC X(8).
000057         16  AP-BANK-ACCOUNT-NO            PIC X(17).
000058         16  AP-BANK-NAME                  PIC X(23).
000059
000060     12  FILLER                            PIC X(31).
000061
000062******************************************************************
      *<<((file: ELCACHP))
000240*    COPY ELCBANK.
      *>>((file: ELCBANK))
000001******************************************************************
000002*                                                                *
000003*                           ELCBANK                              *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = BANK MASTER FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 210  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELBANK                         RKP=2,LEN=9    *
000013*       ALTERNATE (NONE)                                         *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  IN  COPYBOOK  ELCBANK                          *
000019******************************************************************
000020
000021 01  BANK-MASTER.
000022     12  BM-RECORD-ID                      PIC XX.
000023         88  VALID-BM-ID                      VALUE 'BM'.
000024
000025******************************************************************
000026*   BASE CLUSTER = MPBANK         (BASE KEY)      RKP=2,LEN=9    *
000027******************************************************************
000028
000029     12  BM-CONTROL-PRIMARY.
000030         16  BM-COMPANY-CD                 PIC X.
000031         16  BM-TRANSIT-NUMBER.
000032             20  BM-FEDERAL-NUMBER         PIC X(4).
000033             20  BM-BANK-NUMBER            PIC X(4).
000034     12  FILLER                            PIC X(20).
000035
000036******************************************************************
000037*                 FILE SYNCHRONIZATION DATA                      *
000038******************************************************************
000039
000040     12  BM-FILE-SYNCH-DATA.
000041         16  BM-LAST-CHANGE-DT             PIC XX.
000042         16  BM-LAST-CHANGE-TIME           PIC S9(7)  COMP-3.
000043         16  BM-LAST-CHANGE-PROCESSOR      PIC X(4).
000044
000045******************************************************************
000046*                       BANK INFORMATION                         *
000047******************************************************************
000048
000049     12  BM-BANK-INFORMATION.
000050         16  BM-NAME                       PIC X(30).
000051         16  BM-ADDRESS1                   PIC X(30).
000052         16  BM-ADDRESS2                   PIC X(30).
000053         16  BM-CITY                       PIC X(25).
000054         16  BM-STATE                      PIC X(25).
000055         16  BM-ZIP.
000056             20  BM-ZIP1                   PIC X(5).
000057             20  BM-ZIP2                   PIC X(4).
000058     12  FILLER                            PIC X(20).
000059
000060******************************************************************
      *<<((file: ELCBANK))
000241
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL6521' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000242 VCOBOL-DUMMY-PROCEDURE.
000243
000244     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000245     MOVE '5'                    TO DC-OPTION-CODE.
000246     PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
000247     MOVE DC-GREG-DATE-1-EDIT    TO WS-SAVE-DATE.
000248     MOVE DC-BIN-DATE-1          TO WS-SAVE-BIN-DT.
000249
000250     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000251     MOVE EIBTRMID               TO QID-TERM.
000252     MOVE +2                     TO EMI-NUMBER-OF-LINES.
000253     MOVE SPACE                  TO SUPPRESS-MAP-SW.
000254
000255     IF EIBCALEN = 0
000256        GO TO 8800-UNAUTHORIZED-ACCESS
000257     END-IF
000258
000259     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000260        MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM
000261     END-IF
000262
000263     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000264        IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000265           MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6
000266           MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5
000267           MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4
000268           MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3
000269           MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2
000270           MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1
000271           MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM
000272           MOVE THIS-PGM              TO PI-CALLING-PROGRAM
000273        ELSE
000274           MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM
000275           MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM
000276           MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1
000277           MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2
000278           MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3
000279           MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4
000280           MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5
000281           MOVE SPACES                TO PI-SAVED-PROGRAM-6
000282        END-IF
000283     END-IF
000284
000285     MOVE LOW-VALUES             TO EL6521AI
000286
000287     IF EIBTRNID = EL611-TRANS-ID OR EL6525-TRANS-ID OR
000288           EL6526-TRANS-ID
000289        CONTINUE
000290     ELSE
000291        GO TO 0100-NOT-EL611-RETURN
000292     END-IF
000293
000294     PERFORM 0600-RECOVER-TS  THRU  0600-EXIT.
000295     MOVE XCTL-652           TO PI-RETURN-TO-PROGRAM.
000296
000297     MOVE PI-FEDERAL-NUMBER      TO TRNSIT1O.
000298     MOVE PI-BANK-NUMBER         TO TRNSIT2O.
000299     MOVE PI-BANK-ACCOUNT-NO     TO BKACCTO.
000300
000301     IF PI-BANK-ACTION-CODE EQUAL   'P'
000302        MOVE 'PENDING'           TO CRSTATO.
000303
000304     IF PI-BANK-ACTION-CODE EQUAL   'A'
000305        MOVE 'ACTIVE '           TO CRSTATO.
000306
000307     IF MAINTYPL GREATER THAN ZERO
000308         MOVE AL-UABON           TO MAINTYPA.
000309
000310     IF STATL    GREATER THAN ZERO
000311         MOVE 'Y'                TO MAP-CHANGED-SW
000312         MOVE AL-UANON           TO STATA.
000313
000314     IF WTHLDL > ZERO
000315        MOVE 'Y'                 TO MAP-CHANGED-SW
000316        MOVE AL-UNNON            TO WTHLDA
000317     END-IF
000318
000319     IF GADDL > ZERO
000320        MOVE 'Y'                 TO MAP-CHANGED-SW
000321        MOVE AL-UANON            TO GADDA
000322     END-IF
000323
000324     IF MELSWL > ZERO
000325        MOVE 'Y'                 TO MAP-CHANGED-SW
000326        MOVE AL-UANON            TO MELSWA
000327     END-IF
000328
000329     IF APCHKL > ZERO
000330        MOVE 'Y'                 TO MAP-CHANGED-SW
000331        MOVE AL-UANON            TO APCHKA
000332     END-IF
000333
000334     IF MDACTL > ZERO
000335        MOVE 'Y'                 TO MAP-CHANGED-SW
000336        MOVE AL-UANON            TO MDACTA
000337     END-IF
000338
000339     IF MDDIVL > ZERO
000340        MOVE 'Y'                 TO MAP-CHANGED-SW
000341        MOVE AL-UANON            TO MDDIVA
000342     END-IF
000343
000344     IF MDCNTRL > ZERO
000345        MOVE 'Y'                 TO MAP-CHANGED-SW
000346        MOVE AL-UANON            TO MDCNTRA
000347     END-IF
000348
000349     IF MDAMTL > ZERO
000350        MOVE 'Y'                 TO MAP-CHANGED-SW
000351        MOVE AL-UNNON            TO MDAMTA
000352     END-IF
000353
000354     IF EFFDTL   GREATER THAN ZERO
000355         MOVE 'Y'                TO MAP-CHANGED-SW
000356         MOVE AL-UANON           TO EFFDTA.
000357
000358     IF TRMDTL   GREATER THAN ZERO
000359         MOVE 'Y'                TO MAP-CHANGED-SW
000360         MOVE AL-UANON           TO TRMDTA.
000361
000362     IF COMM1L   GREATER THAN ZERO
000363         MOVE 'Y'                TO MAP-CHANGED-SW
000364         MOVE AL-UANON           TO COMM1A.
000365
000366     IF COMM2L   GREATER THAN ZERO
000367         MOVE 'Y'                TO MAP-CHANGED-SW
000368         MOVE AL-UANON           TO COMM2A.
000369
000370     IF COMM3L   GREATER THAN ZERO
000371         MOVE 'Y'                TO MAP-CHANGED-SW
000372         MOVE AL-UANON           TO COMM3A.
000373
000374     IF COMM4L   GREATER THAN ZERO
000375         MOVE 'Y'                TO MAP-CHANGED-SW
000376         MOVE AL-UANON           TO COMM4A.
000377
000378     IF TRNSIT1L GREATER THAN ZERO
000379         MOVE 'Y'                TO MAP-CHANGED-SW
000380         MOVE AL-UABON           TO TRNSIT1A.
000381
000382     IF TRNSIT2L GREATER THAN ZERO
000383         MOVE 'Y'                TO MAP-CHANGED-SW
000384         MOVE AL-UABON           TO TRNSIT2A.
000385
000386     IF BKACCTL  GREATER THAN ZERO
000387         MOVE 'Y'                TO MAP-CHANGED-SW
000388         MOVE AL-UABON           TO BKACCTA.
000389
000390     IF ACTCDL   GREATER THAN ZERO
000391         MOVE 'Y'                TO MAP-CHANGED-SW
000392         MOVE AL-UABON           TO ACTCDA.
000393
000394     IF DELTOL > 0
000395        MOVE 'Y'                 TO MAP-CHANGED-SW
000396        MOVE AL-UANON            TO DELTOA
000397     END-IF
000398
000399     IF RGIDL > 0
000400        MOVE 'Y'                 TO MAP-CHANGED-SW
000401        MOVE AL-UANON            TO RGIDA
000402     END-IF
000403
000404     IF MAP-NOT-CHANGED
000405         GO TO 0100-NOT-EL611-RETURN.
000406
000407     GO TO 8100-SEND-INITIAL-MAP.
000408
000409 0100-NOT-EL611-RETURN.
000410
000411     IF EIBTRNID NOT = TRANS-ID
000412         MOVE PI-MAINT           TO MAINTYPO
000413         MOVE AL-UANON           TO MAINTYPA
000414         MOVE -1                 TO MAINTYPL
000415         IF PI-MAINT = 'S' OR 'C'
000416             GO TO 4000-SHOW
000417         ELSE
000418             GO TO 8100-SEND-INITIAL-MAP.
000419
000420     
      * EXEC CICS HANDLE CONDITION
000421*        PGMIDERR  (9600-PGMID-ERROR)
000422*        ERROR     (9990-ABEND)
000423*    END-EXEC.
      *    MOVE '"$L.                  ! " #00002174' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303032313734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000424
000425     IF EIBAID = DFHCLEAR
000426         GO TO 9400-CLEAR.
000427
000428     EJECT
000429 0200-RECEIVE.
000430     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000431         MOVE ER-0008            TO EMI-ERROR
000432         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000433         MOVE -1                 TO PFENTERL
000434         GO TO 8200-SEND-DATAONLY.
000435
000436     
      * EXEC CICS RECEIVE
000437*        MAP      (MAP-NAME)
000438*        MAPSET   (MAPSET-NAME)
000439*        INTO     (EL6521AI)
000440*    END-EXEC.
           MOVE LENGTH OF
            EL6521AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002190' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032313930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6521AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000441
000442     IF PFENTERL = 0
000443         GO TO 0300-CHECK-PFKEYS.
000444
000445     IF EIBAID NOT = DFHENTER
000446         MOVE ER-0004            TO EMI-ERROR
000447         GO TO 0320-INPUT-ERROR.
000448
000449     IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
000450         MOVE PF-VALUES (PFENTERI) TO EIBAID
000451     ELSE
000452         MOVE ER-0029              TO EMI-ERROR
000453         GO TO 0320-INPUT-ERROR.
000454
000455     EJECT
000456
000457 0300-CHECK-PFKEYS.
000458     IF PI-AR-PROCESSING
000459        IF EIBAID = DFHPF1
000460             PERFORM 0500-WRITE-TS  THRU  0500-EXIT
000461             MOVE XCTL-611       TO PGM-NAME
000462             GO TO 9300-XCTL.
000463
000464     IF EIBAID = DFHPF2
000465        MOVE PI-COMPANY-CD       TO PI-ERCOBI-COMPANY-CD
000466        MOVE DELTOI              TO PI-ERCOBI-STMT-OWNER
000467        MOVE RGIDI               TO PI-ERCOBI-RGID
000468        PERFORM 0500-WRITE-TS    THRU 0500-EXIT
000469        MOVE XCTL-6525           TO PGM-NAME
000470        GO TO 9300-XCTL
000471     END-IF
000472
000473     IF EIBAID = DFHPF3
000474        PERFORM 0500-WRITE-TS    THRU 0500-EXIT
000475        MOVE XCTL-6526           TO PGM-NAME
000476        GO TO 9300-XCTL
000477     END-IF
000478
000479     IF EIBAID = DFHPF23
000480         GO TO 8810-PF23.
000481     IF EIBAID = DFHPF24
000482         GO TO 9200-RETURN-MAIN-MENU.
000483     IF EIBAID = DFHPF12
000484         GO TO 9500-PF12.
000485     IF EIBAID = DFHENTER
000486         GO TO 0330-CHECK-MAINTYP.
000487
000488     MOVE ER-0029                TO EMI-ERROR.
000489 0320-INPUT-ERROR.
000490     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000491     MOVE AL-UNBON               TO PFENTERA.
000492     MOVE -1                     TO PFENTERL.
000493     GO TO 8200-SEND-DATAONLY.
000494
000495 EJECT
000496 0330-CHECK-MAINTYP.
000497
000498     IF MAINTYPL GREATER ZERO
000499         IF MAINTYPI = 'S' OR 'C'
000500             MOVE AL-UANON       TO MAINTYPA
000501             MOVE MAINTYPI       TO PI-MAINT
000502         ELSE
000503             MOVE -1             TO MAINTYPL
000504             MOVE AL-UABON       TO MAINTYPA
000505             MOVE ER-2039        TO EMI-ERROR
000506             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000507             GO TO 8200-SEND-DATAONLY
000508     ELSE
000509         MOVE -1                 TO MAINTYPL
000510         MOVE AL-UABON           TO MAINTYPA
000511         MOVE ER-2039            TO EMI-ERROR
000512         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000513         GO TO 8200-SEND-DATAONLY.
000514
000515     IF PI-MAINT = 'S'
000516         GO TO 4000-SHOW.
000517
000518     GO TO 4200-MAINT.
000519
000520     EJECT
000521
000522 0500-WRITE-TS.
000523      
      * EXEC CICS WRITEQ TS
000524*         QUEUE  (QID)
000525*         FROM   (EL6521AO)
000526*         LENGTH (MAP-LENGTH)
000527*    END-EXEC.
      *    MOVE '*"     L              ''   #00002277' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032323737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL6521AO, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000528
000529 0500-EXIT.
000530      EXIT.
000531
000532 0600-RECOVER-TS.
000533      
      * EXEC CICS READQ TS
000534*         QUEUE  (QID)
000535*         INTO (EL6521AO)
000536*         LENGTH (MAP-LENGTH)
000537*    END-EXEC.
      *    MOVE '*$I    L              ''   #00002287' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032323837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL6521AO, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000538
000539     PERFORM 0700-DELETE-TS  THRU  0700-EXIT.
000540
000541 0600-EXIT.
000542      EXIT.
000543
000544 0700-DELETE-TS.
000545     
      * EXEC CICS HANDLE CONDITION
000546*        QIDERR (0700-EXIT)
000547*    END-EXEC.
      *    MOVE '"$N                   ! # #00002299' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303032323939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000548
000549     
      * EXEC CICS DELETEQ TS
000550*        QUEUE (QID)
000551*    END-EXEC.
      *    MOVE '*&                    #   #00002303' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032333033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000552
000553 0700-EXIT.
000554      EXIT.
000555      EJECT
000556
000557 4000-SHOW.
000558
000559     PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.
000560
000561     IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
000562           EL6526-TRANS-ID
000563         NEXT SENTENCE
000564     ELSE
000565         MOVE LOW-VALUES         TO EL6521AO.
000566
000567     GO TO 5000-BUILD-INITIAL-SCREEN.
000568
000569     EJECT
000570 4200-MAINT.
000571     IF NOT MODIFY-CAP
000572         MOVE 'UPDATE'           TO SM-READ
000573         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000574         MOVE ER-0070            TO EMI-ERROR
000575         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000576         GO TO 8100-SEND-INITIAL-MAP.
000577
000578     MOVE 'N'                     TO SUPPRESS-MAP-SW.
000579     PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.
000580
000581     IF EMI-NO-ERRORS
000582         NEXT SENTENCE
000583     ELSE
000584       IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
000585              EL6526-TRANS-ID
000586           GO TO 8100-SEND-INITIAL-MAP
000587       ELSE
000588           GO TO 8200-SEND-DATAONLY.
000589
000590     PERFORM 7000-EDIT THRU 7099-EXIT.
000591
000592     IF EMI-NO-ERRORS
000593         NEXT SENTENCE
000594     ELSE
000595       IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
000596             EL6526-TRANS-ID
000597           GO TO 8100-SEND-INITIAL-MAP
000598       ELSE
000599           GO TO 8200-SEND-DATAONLY.
000600
000601     MOVE ER-0000               TO EMI-ERROR
000602     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000603
000604     PERFORM 7300-READ-ERCOMP-UPDATE THRU 7300-EXIT.
000605
000606     PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.
000607
000608     MOVE PI-PROCESSOR-ID        TO CO-LAST-MAINT-USER.
000609     MOVE EIBTIME                TO CO-LAST-MAINT-HHMMSS.
000610     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000611     MOVE '5'                    TO DC-OPTION-CODE.
000612     PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
000613
000614     MOVE DC-BIN-DATE-1          TO CO-LAST-MAINT-DT.
000615
000616     PERFORM 5500-PROCESS-ELACHP THRU 5599-PROCESS-EXIT
000617
000618     PERFORM 6100-CHECK-BILL-INSTR
000619                                 THRU 6100-EXIT
000620
000621
000622     
      * EXEC CICS REWRITE
000623*        DATASET  (ERCOMP-FILE-ID)
000624*        FROM     (COMPENSATION-MASTER)
000625*    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002376' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303032333736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000626
000627     PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.
000628     MOVE LOW-VALUES             TO EL6521AO.
000629     MOVE 'C'                    TO PI-MAINT.
000630
000631     EJECT
000632
000633 5000-BUILD-INITIAL-SCREEN.
000634     MOVE CO-CARRIER             TO CARRIERO.
000635     MOVE CO-GROUPING            TO GROUPO.
000636     MOVE CO-TYPE                TO TYPEO.
000637     MOVE CO-RESP-NO             TO FINRESPO.
000638     MOVE CO-CTL-2               TO COACCTO.
000639     MOVE CO-GA-STATUS-CODE      TO STATO.
000640
000641     IF CO-GA-STATUS-CODE = 'A' OR 'I' OR 'P'
000642         MOVE CO-GA-STATUS-CODE  TO STATO
000643         MOVE AL-UANON           TO STATA
000644         MOVE +1                 TO STATL
000645     ELSE
000646         MOVE AL-UANOF           TO STATA
000647     END-IF
000648
000649     IF CO-GA-WITHOLD-PCT NOT NUMERIC
000650        MOVE ZEROS               TO CO-GA-WITHOLD-PCT
000651     END-IF
000652     MOVE CO-GA-WITHOLD-PCT      TO WTHLDO
000653     IF CO-GA-WITHOLD-PCT NOT = ZEROS
000654        MOVE +3                  TO WTHLDL
000655        MOVE AL-UANON            TO WTHLDA
000656     END-IF
000657     IF CO-GA-DIRECT-DEP = 'Y'
000658        MOVE 'Y'                 TO GADDO
000659        MOVE AL-UANON            TO GADDA
000660        MOVE +1                  TO GADDL
000661     ELSE
000662        MOVE 'N'                 TO GADDO
000663        MOVE AL-UANON            TO GADDA
000664        MOVE +1                  TO GADDL
000665     END-IF
000666
000667     IF CO-DELIVER-CK-TO-MEL = 'Y'
000668        MOVE 'Y'                 TO MELSWO
000669        MOVE AL-UANON            TO MELSWA
000670        MOVE +1                  TO MELSWL
000671     ELSE
000672        MOVE 'N'                 TO MELSWO
000673        MOVE AL-UANON            TO MELSWA
000674        MOVE +1                  TO MELSWL
000675     END-IF
000676
000677     IF CO-CREATE-AP-CHECK = 'Y'
000678        MOVE 'Y'                 TO APCHKO
000679        MOVE AL-UANON            TO APCHKA
000680        MOVE +1                  TO APCHKL
000681     ELSE
000682        MOVE 'N'                 TO APCHKO
000683        MOVE AL-UANON            TO APCHKA
000684        MOVE +1                  TO APCHKL
000685     END-IF
000686
000687     IF CO-MD-GL-ACCT = SPACES OR LOW-VALUES
000688        MOVE AL-UANOF            TO MDACTA
000689     ELSE
000690        MOVE CO-MD-GL-ACCT       TO MDACTO
000691        MOVE +2                  TO MDACTL
000692        MOVE AL-UANON            TO MDACTA
000693     END-IF
000694
000695     IF CO-MD-DIV = SPACES OR LOW-VALUES
000696        MOVE AL-UANOF            TO MDDIVA
000697     ELSE
000698        MOVE CO-MD-DIV           TO MDDIVO
000699        MOVE AL-UANON            TO MDDIVA
000700     END-IF
000701
000702     IF CO-MD-CENTER = SPACES OR LOW-VALUES
000703        MOVE AL-UANOF            TO MDCNTRA
000704     ELSE
000705        MOVE CO-MD-CENTER        TO MDCNTRO
000706        MOVE +2                  TO MDCNTRL
000707        MOVE AL-UANON            TO MDCNTRA
000708     END-IF
000709
000710     IF CO-MD-AMT NOT NUMERIC
000711        MOVE ZEROS               TO CO-MD-AMT
000712     END-IF
000713     MOVE CO-MD-AMT              TO MDAMTO
000714     IF CO-MD-AMT NOT = ZEROS
000715        MOVE +3                  TO MDAMTL
000716        MOVE AL-UANON            TO MDAMTA
000717     END-IF
000718     MOVE ZEROS                  TO MDLOBO
000719                                    MDSTO
000720
000721     IF CO-GA-EFFECTIVE-DT = LOW-VALUES OR SPACES
000722         MOVE AL-UANOF           TO EFFDTA
000723     ELSE
000724         MOVE CO-GA-EFFECTIVE-DT TO DC-BIN-DATE-1
000725         MOVE ' '                TO DC-OPTION-CODE
000726         PERFORM 9700-DATE-CONVERT THRU 9700-EXIT
000727         MOVE DC-GREG-DATE-1-EDIT
000728                                 TO EFFDTO
000729         MOVE AL-UANON           TO EFFDTA
000730         MOVE +8                 TO EFFDTL.
000731
000732     IF CO-GA-TERMINATION-DT = HIGH-VALUES
000733        MOVE '99/99/99'          TO TRMDTO
000734        MOVE AL-UANON            TO TRMDTA
000735     ELSE
000736        IF CO-GA-TERMINATION-DT = LOW-VALUES OR SPACES
000737            MOVE AL-UANOF        TO TRMDTA
000738        ELSE
000739           MOVE CO-GA-TERMINATION-DT
000740                                 TO DC-BIN-DATE-1
000741           MOVE ' '              TO DC-OPTION-CODE
000742           PERFORM 9700-DATE-CONVERT
000743                                 THRU 9700-EXIT
000744           MOVE DC-GREG-DATE-1-EDIT
000745                                 TO TRMDTO
000746           MOVE AL-UANON         TO TRMDTA
000747           MOVE +8               TO TRMDTL
000748        END-IF
000749     END-IF
000750
000751     IF CO-GA-COMMENT-1 = LOW-VALUES OR SPACES
000752         MOVE AL-UANOF            TO COMM1A
000753     ELSE
000754         MOVE CO-GA-COMMENT-1    TO COMM1O
000755         MOVE AL-UANON           TO COMM1A
000756         MOVE +40                TO COMM1L.
000757
000758     IF CO-GA-COMMENT-2 = LOW-VALUES OR SPACES
000759         MOVE AL-UANOF            TO COMM2A
000760     ELSE
000761         MOVE CO-GA-COMMENT-2    TO COMM2O
000762         MOVE AL-UANON           TO COMM2A
000763         MOVE +40                TO COMM2L.
000764
000765     IF CO-GA-COMMENT-3 = LOW-VALUES OR SPACES
000766         MOVE AL-UANOF           TO COMM3A
000767     ELSE
000768         MOVE CO-GA-COMMENT-3    TO COMM3O
000769         MOVE AL-UANON           TO COMM3A
000770         MOVE +40                TO COMM3L.
000771
000772     IF CO-GA-COMMENT-4 = LOW-VALUES OR SPACES
000773         MOVE AL-UANOF           TO COMM4A
000774     ELSE
000775         MOVE CO-GA-COMMENT-4    TO COMM4O
000776         MOVE AL-UANON           TO COMM4A
000777         MOVE +40                TO COMM4L.
000778
000779     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
000780           or 'FNL'
000781        CONTINUE
000782     ELSE
000783        IF (CO-BANK-TRANSIT-NO (1:4) = LOW-VALUES OR SPACES) AND
000784           (CO-BANK-TRANSIT-NO (5:4) = LOW-VALUES OR SPACES)
000785            MOVE AL-UANOF        TO TRNSIT1A
000786            MOVE AL-UANOF        TO TRNSIT2A
000787        ELSE
000788           MOVE CO-BANK-TRANSIT-NO (1:4)
000789                                 TO PI-BANK-TRANSIT-NUMBER (2:4)
000790                                    TRNSIT1O
000791           MOVE CO-BANK-TRANSIT-NO (5:4)
000792                                 TO PI-BANK-TRANSIT-NUMBER (6:4)
000793                                    TRNSIT2O
000794        END-IF
000795        IF CO-BANK-ACCOUNT-NUMBER = LOW-VALUES OR SPACES
000796           MOVE AL-UANOF         TO BKACCTA
000797        ELSE
000798           MOVE CO-BANK-ACCOUNT-NUMBER
000799                                 TO PI-BANK-ACCOUNT-NO
000800                                    BKACCTO
000801        END-IF
000802     END-IF
000803
000804     MOVE    '       '           TO CRSTATO.
000805
000806     MOVE CO-ACH-STATUS          TO PI-BANK-ACTION-CODE.
000807
000808     IF CO-ACH-STATUS IS EQUAL      'P'
000809        MOVE 'PENDING'           TO CRSTATO.
000810
000811     IF CO-ACH-STATUS IS EQUAL      'A'
000812        MOVE 'ACTIVE '           TO CRSTATO.
000813
000814     IF CO-STMT-OWNER NOT = SPACES AND LOW-VALUES
000815        MOVE CO-STMT-OWNER       TO DELTOO
000816        MOVE AL-UANON            TO DELTOA
000817        MOVE +4                  TO DELTOL
000818     ELSE
000819        MOVE AL-UANOF            TO DELTOA
000820     END-IF
000821
000822     IF CO-REPORT-GROUP-ID NOT = SPACES AND LOW-VALUES
000823        MOVE CO-REPORT-GROUP-ID  TO RGIDO
000824        MOVE AL-UANON            TO RGIDA
000825        MOVE +12                 TO RGIDL
000826     ELSE
000827        MOVE AL-UANOF            TO RGIDA
000828     END-IF
000829
000830     MOVE PI-MAINT               TO MAINTYPO.
000831     MOVE AL-UANOF               TO MAINTYPA.
000832     MOVE -1                     TO MAINTYPL.
000833
000834     GO TO 8100-SEND-INITIAL-MAP.
000835
000836 5099-EXIT.
000837     EXIT.
000838     EJECT
000839 5500-PROCESS-ELACHP.
000840
000841     IF ACTCDI = 'D'
000842        PERFORM 7400-DELETE-PRE-NOTE THRU 7499-EXIT
000843        MOVE SPACES              TO CO-BANK-INFORMATION
000844                                    CO-ACH-STATUS
000845        GO TO 5599-PROCESS-EXIT.
000846
000847     IF ACTCDI = 'C'
000848         PERFORM 7400-DELETE-PRE-NOTE THRU 7499-EXIT
000849         PERFORM 7600-WRITE-ELACHP THRU 7620-WRITE-EXIT
000850         MOVE WS-BANK-DATA       TO CO-BANK-INFORMATION
000851         MOVE WS-BKACCTI         TO CO-BANK-ACCOUNT-NUMBER
000852         MOVE 'P'                TO CO-ACH-STATUS
000853         GO TO 5599-PROCESS-EXIT.
000854
000855     IF ACTCDI =  'N'
000856         PERFORM 7600-WRITE-ELACHP THRU 7620-WRITE-EXIT
000857         MOVE WS-BANK-DATA       TO CO-BANK-INFORMATION
000858         MOVE WS-BKACCTI         TO CO-BANK-ACCOUNT-NUMBER
000859         MOVE 'P'                TO CO-ACH-STATUS  WS-ACTCDI
000860         GO TO 5599-PROCESS-EXIT.
000861
000862     IF ACTCDI = 'A'
000863         MOVE 'A'                TO CO-ACH-STATUS  WS-ACTCDI
000864         GO TO 5599-PROCESS-EXIT.
000865
000866 5599-PROCESS-EXIT.
000867
000868     EXIT.
000869 6000-CHECK-FOR-UPDATE.
000870
000871     IF STATL GREATER ZERO
000872        MOVE STATI               TO CO-GA-STATUS-CODE.
000873
000874     IF EFFDTL GREATER ZERO
000875        MOVE WS-SAVE-BIN-EFFDT   TO CO-GA-EFFECTIVE-DT.
000876
000877     IF TRMDTL GREATER ZERO
000878        MOVE WS-SAVE-BIN-TRMDT   TO CO-GA-TERMINATION-DT.
000879
000880     IF GADDL > ZERO
000881        MOVE GADDI               TO CO-GA-DIRECT-DEP
000882     END-IF
000883
000884     IF MELSWL > ZERO
000885        MOVE MELSWI              TO CO-DELIVER-CK-TO-MEL
000886     END-IF
000887
000888     IF APCHKL > ZERO
000889        MOVE APCHKI              TO CO-CREATE-AP-CHECK
000890     END-IF
000891
000892     IF MDACTL > ZERO
000893        MOVE MDACTI              TO CO-MD-GL-ACCT
000894     END-IF
000895
000896     IF MDDIVL > ZERO
000897        MOVE MDDIVI              TO CO-MD-DIV
000898     END-IF
000899
000900     IF MDCNTRL > ZERO
000901        MOVE MDCNTRI             TO CO-MD-CENTER
000902     END-IF
000903
000904     IF WTHLDL > ZERO
000905        MOVE WTHLDI              TO CO-GA-WITHOLD-PCT
000906     END-IF
000907
000908     IF MDAMTL > ZERO
000909        MOVE MDAMTI              TO CO-MD-AMT
000910     END-IF
000911
000912     IF DELTOL > ZERO
000913        MOVE DELTOI              TO CO-STMT-OWNER
000914     END-IF
000915
000916     IF RGIDL > ZERO
000917        MOVE RGIDI               TO CO-REPORT-GROUP-ID
000918     END-IF
000919
000920     IF COMM1L GREATER ZERO
000921        MOVE COMM1I              TO CO-GA-COMMENT-1.
000922
000923     IF COMM2L GREATER ZERO
000924        MOVE COMM2I              TO CO-GA-COMMENT-2.
000925
000926     IF COMM3L GREATER ZERO
000927        MOVE COMM3I              TO CO-GA-COMMENT-3.
000928
000929     IF COMM4L GREATER ZERO
000930        MOVE COMM4I              TO CO-GA-COMMENT-4.
000931
000932 6049-EXIT.
000933     EXIT.
000934
000935 6100-CHECK-BILL-INSTR.
000936
000937     MOVE PI-COMPANY-CD          TO ERCOBI-COMPANY-CD
000938     MOVE DELTOI                 TO ERCOBI-STMT-OWNER
000939     MOVE RGIDI                  TO ERCOBI-RGID
000940
000941     
      * EXEC CICS READ
000942*       DATASET (ERCOBI-FILE-ID)
000943*       SET     (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
000944*       RIDFLD  (ERCOBI-KEY)
000945*       RESP    (WS-RESPONSE)
000946*       UPDATE
000947*    END-EXEC
      *    MOVE '&"S        EU         (  N#00002695' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032363935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOBI-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOBI-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMP-BILLING-INSTRUCTIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000948
000949     IF NOT RESP-NORMAL
000950        MOVE AL-UABON            TO DELTOA
000951                                    RGIDA
000952        MOVE ER-2797             TO EMI-ERROR
000953        MOVE -1                  TO DELTOL
000954        PERFORM 9900-ERROR-FORMAT
000955                                 THRU 9900-EXIT
000956     END-IF
000957
000958     .
000959 6100-EXIT.
000960     EXIT.
000961
000962 7000-EDIT.
000963     IF STATL GREATER ZERO
000964         IF STATI = 'A' OR 'I' OR 'P'
000965             NEXT SENTENCE
000966          ELSE
000967             MOVE -1             TO STATL
000968             MOVE AL-UABON       TO STATA
000969             MOVE ER-7438        TO EMI-ERROR
000970             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000971
000972     MOVE LOW-VALUES             TO WS-SAVE-BIN-EFFDT.
000973     MOVE LOW-VALUES             TO WS-SAVE-BIN-TRMDT.
000974
000975     IF EFFDTL NOT = ZERO
000976         MOVE EFFDTI             TO DEEDIT-FIELD
000977         PERFORM 8600-DEEDIT
000978         MOVE DEEDIT-FIELD-V0    TO DC-GREG-DATE-1-MDY
000979         MOVE '4'                TO DC-OPTION-CODE
000980         PERFORM 9700-DATE-CONVERT THRU 9700-EXIT
000981         IF DATE-CONVERSION-ERROR
000982             MOVE ER-0348        TO EMI-ERROR
000983             MOVE -1             TO EFFDTL
000984             MOVE AL-UABON       TO EFFDTA
000985             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000986         ELSE
000987             MOVE DC-GREG-DATE-1-EDIT  TO EFFDTI
000988             MOVE AL-UANON             TO EFFDTA
000989             MOVE DC-BIN-DATE-1        TO WS-SAVE-BIN-EFFDT.
000990
000991     IF TRMDTL NOT = ZERO
000992        MOVE TRMDTI              TO DEEDIT-FIELD
000993        PERFORM 8600-DEEDIT
000994        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000995        IF DC-GREG-DATE-1-MDY = 999999
000996           MOVE HIGH-VALUES      TO WS-SAVE-BIN-TRMDT
000997        ELSE
000998           MOVE '4'              TO DC-OPTION-CODE
000999           PERFORM 9700-DATE-CONVERT
001000                                 THRU 9700-EXIT
001001           IF DATE-CONVERSION-ERROR
001002              MOVE ER-0454       TO EMI-ERROR
001003              MOVE -1            TO TRMDTL
001004              MOVE AL-UABON      TO TRMDTA
001005              PERFORM 9900-ERROR-FORMAT
001006                                 THRU 9900-EXIT
001007           ELSE
001008              MOVE DC-GREG-DATE-1-EDIT
001009                                 TO TRMDTI
001010              MOVE AL-UANON      TO TRMDTA
001011              MOVE DC-BIN-DATE-1 TO WS-SAVE-BIN-TRMDT
001012           END-IF
001013        END-IF
001014     END-IF
001015
001016     IF GADDL > ZERO
001017        IF GADDI = 'Y' OR 'N' OR ' '
001018           MOVE AL-UANON         TO GADDA
001019        ELSE
001020           MOVE AL-UABON         TO GADDA
001021           MOVE ER-0876          TO EMI-ERROR
001022           MOVE -1               TO GADDL
001023           PERFORM 9900-ERROR-FORMAT
001024                                 THRU 9900-EXIT
001025        END-IF
001026     END-IF
001027
001028     IF MELSWL > ZERO
001029        IF MELSWI = 'Y' OR 'N' OR ' '
001030           MOVE AL-UANON         TO MELSWA
001031        ELSE
001032           MOVE AL-UABON         TO MELSWA
001033           MOVE ER-0876          TO EMI-ERROR
001034           MOVE -1               TO MELSWL
001035           PERFORM 9900-ERROR-FORMAT
001036                                 THRU 9900-EXIT
001037        END-IF
001038     END-IF
001039
001040     IF APCHKL > ZERO
001041        IF APCHKI = 'Y' OR 'N' OR ' '
001042           MOVE AL-UANON         TO APCHKA
001043        ELSE
001044           MOVE AL-UABON         TO APCHKA
001045           MOVE ER-0876          TO EMI-ERROR
001046           MOVE -1               TO APCHKL
001047           PERFORM 9900-ERROR-FORMAT
001048                                 THRU 9900-EXIT
001049        END-IF
001050     END-IF
001051
001052     IF WTHLDL  > ZERO
001053        
      * EXEC CICS BIF
001054*          DEEDIT
001055*          FIELD   (WTHLDI)
001056*          LENGTH  (6)
001057*       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002807' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032383037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WTHLDI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001058        IF WTHLDI NUMERIC
001059           MOVE AL-UNNON         TO WTHLDA
001060        ELSE
001061           MOVE -1               TO WTHLDL
001062           MOVE AL-UABON         TO WTHLDA
001063           MOVE ER-8799          TO EMI-ERROR
001064           PERFORM 9900-ERROR-FORMAT
001065                                 THRU  9900-EXIT
001066        END-IF
001067     END-IF
001068
001069     IF MDAMTL  > ZERO
001070        
      * EXEC CICS BIF
001071*          DEEDIT
001072*          FIELD   (MDAMTI)
001073*          LENGTH  (8)
001074*       END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002824' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032383234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MDAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001075        IF MDAMTI NUMERIC
001076           MOVE AL-UNNON         TO MDAMTA
001077        ELSE
001078           MOVE -1               TO MDAMTL
001079           MOVE AL-UABON         TO MDAMTA
001080           MOVE ER-8799          TO EMI-ERROR
001081           PERFORM 9900-ERROR-FORMAT
001082                                 THRU  9900-EXIT
001083        END-IF
001084     END-IF
001085
001086     IF WS-SAVE-BIN-EFFDT = LOW-VALUES OR
001087        WS-SAVE-BIN-TRMDT = LOW-VALUES
001088         NEXT SENTENCE
001089       ELSE
001090         IF WS-SAVE-BIN-TRMDT LESS THAN WS-SAVE-BIN-EFFDT
001091             MOVE ER-1228        TO EMI-ERROR
001092             MOVE -1             TO EFFDTL
001093             MOVE AL-UABON       TO EFFDTA
001094             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001095
001096      IF ((DELTOL > +0)
001097         OR (RGIDL > +0))
001098         AND ((DELTOI NOT = SPACES)
001099         AND (RGIDI NOT = SPACES))
001100         PERFORM 6100-CHECK-BILL-INSTR
001101                                 THRU 6100-EXIT
001102      END-IF
001103
001104     MOVE SPACES                 TO WS-BANK-INFORMATION.
001105
001106     IF PI-AR-PROCESSING
001107        NEXT SENTENCE
001108     ELSE
001109        MOVE ZEROS               TO ACTCDL, TRNSIT1L TRNSIT2L
001110                                    BKACCTL
001111        GO TO 7099-EXIT.
001112
001113     IF (TRNSIT1L NOT = ZERO) OR
001114        (TRNSIT2L NOT = ZERO) OR
001115        (BKACCTL  NOT = ZERO) OR
001116        (ACTCDL   NOT = ZERO)
001117         MOVE 'Y'                TO WS-ACH-SW
001118     ELSE
001119         GO TO 7099-EXIT.
001120
001121     IF (ACTCDL = ZERO) OR
001122        (ACTCDI NOT = 'N' AND 'C' AND 'A' AND 'D')
001123         MOVE -1                 TO ACTCDL
001124         MOVE AL-UABON           TO ACTCDA
001125         MOVE ER-7436            TO EMI-ERROR
001126         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001127         GO TO 7099-EXIT.
001128
001129     IF ACTCDI = 'N'
001130         GO TO 7020-NEW-ACH.
001131
001132     IF ACTCDI = 'C'
001133         GO TO 7040-CHANGE-ACH.
001134
001135     IF ACTCDI = 'A'
001136         GO TO 7060-ACTIVATE-ACH.
001137
001138****   ACH DELETE PROCESSING EDITS
001139
001140     IF TRNSIT1L NOT = ZERO OR
001141        TRNSIT2L NOT = ZERO OR
001142        BKACCTL  NOT = ZERO
001143         MOVE -1                 TO ACTCDL
001144         MOVE AL-UABON           TO ACTCDA
001145         MOVE ER-7430            TO EMI-ERROR
001146         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001147
001148     GO TO 7099-EXIT.
001149
001150****   ACH NEW ENTRY PROCESSING EDITS
001151 7020-NEW-ACH.
001152
001153     IF CO-ACH-STATUS = 'A' OR 'P'
001154         MOVE -1                 TO ACTCDL
001155         MOVE AL-UABON           TO ACTCDA
001156         MOVE ER-7434            TO EMI-ERROR
001157         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001158         GO TO 7099-EXIT.
001159
001160     IF TRNSIT1L = ZERO OR
001161        TRNSIT2L = ZERO OR
001162        BKACCTL = ZERO
001163         MOVE -1                 TO TRNSIT1L
001164         MOVE ER-7431            TO EMI-ERROR
001165         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001166         GO TO 7099-EXIT.
001167
001168     IF TRNSIT1L GREATER THAN ZERO
001169         IF TRNSIT1I = SPACES
001170             MOVE -1             TO TRNSIT1L
001171             MOVE ER-7435        TO EMI-ERROR
001172             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001173             GO TO 7099-EXIT
001174         ELSE
001175             MOVE TRNSIT1I       TO WS-TRANSIT1.
001176
001177     IF TRNSIT2L GREATER THAN ZERO
001178         IF TRNSIT2I = SPACES
001179             MOVE -1             TO TRNSIT2L
001180             MOVE ER-7435        TO EMI-ERROR
001181             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001182             GO TO 7099-EXIT
001183         ELSE
001184             MOVE TRNSIT2I       TO WS-TRANSIT2.
001185
001186     IF BKACCTL GREATER THAN ZERO
001187         IF BKACCTI = SPACES
001188             MOVE -1             TO BKACCTL
001189             MOVE ER-7435        TO EMI-ERROR
001190             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001191             GO TO 7099-EXIT
001192         ELSE
001193             MOVE BKACCTI        TO WS-BKACCTI.
001194         MOVE BKACCTI            TO WS-BKACCTI.
001195
001196     PERFORM 7500-VALIDATE-TRANSIT  THRU  7500-VALIDATE-EXIT.
001197
001198     GO TO 7099-EXIT.
001199
001200****   ACH CHANGE ENTRY PROCESSING EDITS
001201 7040-CHANGE-ACH.
001202
001203     IF (TRNSIT1L = ZERO) AND
001204        (TRNSIT2L = ZERO) AND
001205        (BKACCTL = ZERO)
001206         MOVE -1                 TO TRNSIT1L
001207         MOVE ER-7435            TO EMI-ERROR
001208         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001209         GO TO 7099-EXIT.
001210
001211     IF CO-BANK-INFORMATION = SPACES OR LOW-VALUES
001212         MOVE -1                 TO ACTCDL
001213         MOVE ER-7432            TO EMI-ERROR
001214         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001215         GO TO 7099-EXIT.
001216
001217     MOVE CO-BANK-INFORMATION    TO WS-BANK-INFORMATION.
001218
001219     IF TRNSIT1L GREATER THAN ZERO
001220         IF TRNSIT1I = SPACES
001221             MOVE -1             TO TRNSIT1L
001222             MOVE ER-7435        TO EMI-ERROR
001223             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001224             GO TO 7099-EXIT
001225         ELSE
001226             MOVE TRNSIT1I       TO WS-TRANSIT1.
001227
001228     IF TRNSIT2L GREATER THAN ZERO
001229         IF TRNSIT2I = SPACES
001230             MOVE -1             TO TRNSIT2L
001231             MOVE ER-7435        TO EMI-ERROR
001232             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001233             GO TO 7099-EXIT
001234         ELSE
001235             MOVE TRNSIT2I       TO WS-TRANSIT2.
001236
001237     IF BKACCTL GREATER THAN ZERO
001238         IF BKACCTI = SPACES
001239             MOVE -1             TO BKACCTL
001240             MOVE ER-7435        TO EMI-ERROR
001241             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001242             GO TO 7099-EXIT
001243         ELSE
001244             MOVE BKACCTI        TO WS-BKACCTI.
001245
001246     IF CO-BANK-INFORMATION = WS-BANK-INFORMATION
001247         MOVE -1                 TO ACTCDL
001248         MOVE ER-7449            TO EMI-ERROR
001249         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001250         GO TO 7099-EXIT.
001251
001252     IF TRNSIT1L GREATER THAN ZERO OR
001253        TRNSIT2L GREATER THAN ZERO
001254         PERFORM 7500-VALIDATE-TRANSIT  THRU  7500-VALIDATE-EXIT.
001255
001256     GO TO 7099-EXIT.
001257
001258****   ACH ACTIVATE ENTRY PROCESSING EDITS
001259 7060-ACTIVATE-ACH.
001260
001261     IF NOT CO-ACH-PENDING
001262         MOVE -1                 TO ACTCDL
001263         MOVE ER-7465            TO EMI-ERROR
001264         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001265         GO TO 7099-EXIT.
001266
001267     IF TRNSIT1L NOT = ZERO OR
001268        TRNSIT2L NOT = ZERO OR
001269        BKACCTL  NOT = ZERO
001270         MOVE -1                 TO ACTCDL
001271         MOVE ER-7440            TO EMI-ERROR
001272         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001273         GO TO 7099-EXIT.
001274
001275 7099-EXIT.
001276     EXIT.
001277     EJECT
001278
001279 7100-READ-ERCOMP.
001280
001281     
      * EXEC CICS HANDLE CONDITION
001282*        NOTOPEN (7100-NOTOPEN)
001283*        NOTFND  (7100-NOTFND)
001284*    END-EXEC.
      *    MOVE '"$JI                  ! $ #00003035' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303033303335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001285
001286     
      * EXEC CICS READ
001287*         DATASET  (ERCOMP-FILE-ID)
001288*         SET      (ADDRESS OF COMPENSATION-MASTER)
001289*         RIDFLD   (PI-ERCOMP-KEY)
001290*    END-EXEC.
      *    MOVE '&"S        E          (   #00003040' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033303430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001291
001292*    PERFORM 7200-READ-ERCOBI    THRU 7200-EXIT
001293
001294     IF DO-NOT-MOVE-TO-MAP
001295        MOVE 'Y'                 TO SUPPRESS-MAP-SW
001296        GO TO 7100-EXIT.
001297
001298     MOVE CO-LAST-MAINT-USER     TO PI-UPDATE-BY.
001299     MOVE CO-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
001300     MOVE CO-CONTROL-PRIMARY     TO PI-ERCOMP-KEY.
001301     MOVE CO-CARRIER             TO CARRIERO.
001302     MOVE CO-GROUPING            TO GROUPO.
001303     MOVE CO-TYPE                TO TYPEO.
001304     MOVE CO-RESP-NO             TO FINRESPO.
001305     MOVE CO-CTL-2               TO COACCTO.
001306     MOVE CO-BANK-INFORMATION    TO PI-BANK-TRANSIT-NUMBER (2:8)
001307
001308     MOVE PI-FEDERAL-NUMBER      TO TRNSIT1O.
001309     MOVE PI-BANK-NUMBER         TO TRNSIT2O.
001310     GO TO 7100-EXIT.
001311
001312 7100-NOTFND.
001313     MOVE ER-7462               TO EMI-ERROR
001314     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001315     GO TO 7100-EXIT.
001316
001317 7100-NOTOPEN.
001318     MOVE ER-2233               TO EMI-ERROR
001319     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001320     GO TO 7100-EXIT.
001321
001322 7100-EXIT.
001323     EXIT.
001324     EJECT
001325
001326 7200-READ-ERCOBI.
001327
001328     MOVE CO-CONTROL-PRIMARY     TO ERCOBI-KEY
001329
001330     
      * EXEC CICS READ
001331*       DATASET (ERCOBI-FILE-ID)
001332*       SET     (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
001333*       RIDFLD  (ERCOBI-KEY)
001334*       RESP    (WS-RESPONSE)
001335*    END-EXEC
      *    MOVE '&"S        E          (  N#00003084' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303033303834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOBI-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOBI-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMP-BILLING-INSTRUCTIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001336
001337     IF RESP-NORMAL
001338        SET BILLING-INSTRUCTIONS-FOUND TO TRUE
001339     END-IF
001340
001341     .
001342 7200-EXIT.
001343     EXIT.
001344
001345 7300-READ-ERCOMP-UPDATE.
001346     
      * EXEC CICS HANDLE CONDITION
001347*        NOTFND  (7300-NOTFND)
001348*        NOTOPEN (7300-NOTOPEN)
001349*    END-EXEC.
      *    MOVE '"$IJ                  ! % #00003100' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303033313030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001350
001351     
      * EXEC CICS READ
001352*         DATASET  (ERCOMP-FILE-ID)
001353*         SET      (ADDRESS OF COMPENSATION-MASTER)
001354*         RIDFLD   (PI-ERCOMP-KEY)
001355*         UPDATE
001356*    END-EXEC.
      *    MOVE '&"S        EU         (   #00003105' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033313035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001357     GO TO 7300-EXIT.
001358
001359 7300-NOTOPEN.
001360
001361     MOVE ER-2233               TO EMI-ERROR
001362     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001363     GO TO 7300-EXIT.
001364 7300-NOTFND.
001365
001366     MOVE ER-7462               TO EMI-ERROR
001367     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001368     GO TO 7300-EXIT.
001369
001370 7300-EXIT.
001371     EXIT.
001372     EJECT
001373
001374 7400-DELETE-PRE-NOTE.
001375
001376     
      * EXEC CICS HANDLE CONDITION
001377*        NOTFND  (7499-EXIT)
001378*        NOTOPEN (7450-NOTOPEN)
001379*    END-EXEC.
      *    MOVE '"$IJ                  ! & #00003130' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303033313330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001380
001381     
      * EXEC CICS READ
001382*         DATASET  (ELACHP-FILE-ID)
001383*         SET     (ADDRESS OF ACH-PRENOTIFICATION)
001384*         RIDFLD   (CO-CONTROL-PRIMARY)
001385*         UPDATE
001386*    END-EXEC.
      *    MOVE '&"S        EU         (   #00003135' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033313335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACHP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACH-PRENOTIFICATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001387
001388     
      * EXEC CICS DELETE
001389*        DATASET   (ELACHP-FILE-ID)
001390*    END-EXEC.
      *    MOVE '&(                    &   #00003142' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303033313432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACHP-FILE-ID, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001391
001392     GO TO 7499-EXIT.
001393
001394 7450-NOTOPEN.
001395     MOVE ER-7469               TO EMI-ERROR
001396     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001397
001398 7499-EXIT.
001399     EXIT.
001400     EJECT
001401
001402******************************************************************
001403*             V A L I D T E  T R A N S I T  N U M B E R
001404******************************************************************
001405 7500-VALIDATE-TRANSIT.
001406
001407     
      * EXEC CICS HANDLE CONDITION
001408*        NOTFND   (7500-NOT-FOUND)
001409*        NOTOPEN  (7500-NOT-OPEN)
001410*    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00003161' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303033313631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001411
001412     MOVE WS-TRANSIT1          TO PI-FEDERAL-NUMBER.
001413     MOVE WS-TRANSIT2          TO PI-BANK-NUMBER.
001414     MOVE PI-COMPANY-CD        TO PI-BANK-COMPANY-CD.
001415
001416     
      * EXEC CICS READ
001417*        EQUAL
001418*        DATASET   (ELBANK-FILE-ID)
001419*        SET       (ADDRESS OF BANK-MASTER)
001420*        RIDFLD    (PI-BANK-TRANSIT-NUMBER)
001421*    END-EXEC.
      *    MOVE '&"S        E          (   #00003170' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033313730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBANK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-BANK-TRANSIT-NUMBER, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001422
001423     GO TO 7500-VALIDATE-EXIT.
001424
001425 7500-NOT-OPEN.
001426
001427     MOVE -1                    TO TRNSIT1L
001428     MOVE AL-UABON              TO TRNSIT1A
001429     MOVE AL-UABON              TO TRNSIT2A
001430     MOVE ER-7468               TO EMI-ERROR .
001431     PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
001432     GO TO 7500-VALIDATE-EXIT.
001433
001434 7500-NOT-FOUND.
001435
001436     MOVE -1                    TO TRNSIT1L
001437     MOVE AL-UABON              TO TRNSIT1A
001438     MOVE AL-UABON              TO TRNSIT2A
001439     MOVE ER-9388               TO EMI-ERROR .
001440     PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
001441     GO TO 7500-VALIDATE-EXIT.
001442
001443 7500-VALIDATE-EXIT.
001444     EXIT.
001445     EJECT
001446
001447 7600-WRITE-ELACHP.
001448     
      * EXEC CICS HANDLE CONDITION
001449*        NOTOPEN (7450-NOTOPEN)
001450*        DUPREC   (7610-DUPREC)
001451*    END-EXEC.
      *    MOVE '"$J%                  ! ( #00003202' TO DFHEIV0
           MOVE X'22244A252020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303033323032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001452
001453     
      * EXEC CICS GETMAIN
001454*        SET      (ADDRESS OF ACH-PRENOTIFICATION)
001455*        LENGTH   (210)
001456*    END-EXEC.
           MOVE 210
             TO DFHEIV11
      *    MOVE '," L                  $   #00003207' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033323037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 DFHEIV99
           SET ADDRESS OF ACH-PRENOTIFICATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001457
001458     INITIALIZE  ACH-PRENOTIFICATION.
001459
001460     MOVE  'AP'                  TO AP-RECORD-ID.
001461
001462     MOVE PI-PROCESSOR-ID        TO AP-LAST-CHANGE-PROCESSOR
001463     MOVE CO-CONTROL-PRIMARY     TO AP-CONTROL-PRIMARY
001464     MOVE EIBTIME                TO AP-LAST-CHANGE-TIME
001465     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
001466     MOVE '5'                    TO DC-OPTION-CODE.
001467     PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
001468
001469     MOVE DC-BIN-DATE-1          TO AP-LAST-CHANGE-DT.
001470     MOVE WS-BANK-DATA           TO AP-BANK-INFORMATION.
001471     MOVE BM-NAME                TO AP-BANK-NAME.
001472
001473     
      * EXEC CICS WRITE
001474*        FROM      (ACH-PRENOTIFICATION)
001475*        DATASET   (ELACHP-FILE-ID)
001476*        RIDFLD    (CO-CONTROL-PRIMARY)
001477*    END-EXEC.
           MOVE LENGTH OF
            ACH-PRENOTIFICATION
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003227' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033323237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACHP-FILE-ID, 
                 ACH-PRENOTIFICATION, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001478
001479     GO TO 7620-WRITE-EXIT.
001480
001481 7610-DUPREC.
001482
001483     MOVE    ER-7447            TO EMI-ERROR
001484     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001485     GO TO 7620-WRITE-EXIT.
001486
001487 7620-WRITE-EXIT.
001488      EXIT.
001489     EJECT
001490******************************************************************
001491 8100-SEND-INITIAL-MAP.
001492     MOVE WS-SAVE-DATE           TO DATEO.
001493     MOVE EIBTIME                TO TIME-IN.
001494     MOVE TIME-OUT               TO TIMEO.
001495     MOVE PI-COMPANY-ID          TO CMPNYIDO.
001496     MOVE PI-PROCESSOR-ID        TO USERIDO.
001497     MOVE -1                     TO MAINTYPL.
001498     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
001499     MOVE WS-AGENT-BANK-DESC    TO BKDESCO.
001500
001501* HIDE ACH FIELDS IF NOT AR PROCESSING
001502     IF PI-AR-PROCESSING
001503        NEXT SENTENCE
001504     ELSE
001505        MOVE AL-SADOF             TO BKLITA, BKDESCA  CRSTATA
001506                                     ACTLITA CRSLITA ACHPF1A
001507                                     TRNSIT1A TRNSIT2A TRDASHA
001508                                     BKACCTA, ACTCDA.
001509     
      * EXEC CICS SEND
001510*        MAP      (MAP-NAME)
001511*        MAPSET   (MAPSET-NAME)
001512*        FROM     (EL6521AO)
001513*        ERASE
001514*        CURSOR
001515*    END-EXEC.
           MOVE LENGTH OF
            EL6521AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003263' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303033323633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6521AO, 
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
           
001516
001517     GO TO 9100-RETURN-TRAN.
001518
001519 EJECT
001520 8200-SEND-DATAONLY.
001521     MOVE WS-SAVE-DATE           TO DATEO.
001522     MOVE EIBTIME                TO TIME-IN.
001523     MOVE TIME-OUT               TO TIMEO.
001524     MOVE PI-COMPANY-ID          TO CMPNYIDO.
001525     MOVE PI-PROCESSOR-ID        TO USERIDO.
001526     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
001527     MOVE WS-AGENT-BANK-DESC     TO BKDESCO.
001528
001529* HIDE ACH FIELDS IF NOT AR PROCESSING
001530     IF PI-AR-PROCESSING
001531        NEXT SENTENCE
001532     ELSE
001533        MOVE AL-SADOF             TO BKLITA, BKDESCA  CRSTATA
001534                                     ACTLITA CRSLITA ACHPF1A
001535                                     TRNSIT1A TRNSIT2A TRDASHA
001536                                     BKACCTA, ACTCDA.
001537     
      * EXEC CICS SEND
001538*        MAP      (MAP-NAME)
001539*        MAPSET   (MAPSET-NAME)
001540*        FROM     (EL6521AO)
001541*        DATAONLY
001542*        CURSOR
001543*    END-EXEC.
           MOVE LENGTH OF
            EL6521AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003291' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303033323931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6521AO, 
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
           
001544
001545     GO TO 9100-RETURN-TRAN.
001546
001547 EJECT
001548 8300-SEND-TEXT.
001549     
      * EXEC CICS SEND TEXT
001550*        FROM     (LOGOFF-TEXT)
001551*        LENGTH   (LOGOFF-LENGTH)
001552*        ERASE
001553*        FREEKB
001554*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003303' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303033333033' TO DFHEIV0
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
           
001555
001556     
      * EXEC CICS RETURN
001557*    END-EXEC.
      *    MOVE '.(                    ''   #00003310' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033333130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001558
001559 EJECT
001560 8600-DEEDIT.
001561     
      * EXEC CICS BIF DEEDIT
001562*         FIELD(DEEDIT-FIELD)
001563*         LENGTH(15)
001564*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003315' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033333135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001565
001566 EJECT
001567 8800-UNAUTHORIZED-ACCESS.
001568     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
001569     GO TO 8300-SEND-TEXT.
001570
001571 8810-PF23.
001572     MOVE EIBAID                 TO PI-ENTRY-CD-1.
001573     MOVE XCTL-005               TO PGM-NAME.
001574     GO TO 9300-XCTL.
001575 EJECT
001576
001577 9100-RETURN-TRAN.
001578     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
001579     MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
001580     
      * EXEC CICS RETURN
001581*        TRANSID    (TRANS-ID)
001582*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
001583*        LENGTH     (WS-COMM-LENGTH)
001584*    END-EXEC.
      *    MOVE '.(CT                  ''   #00003334' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033333334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001585
001586 9200-RETURN-MAIN-MENU.
001587     MOVE XCTL-626               TO PGM-NAME.
001588     GO TO 9300-XCTL.
001589
001590 EJECT
001591 9300-XCTL.
001592     
      * EXEC CICS XCTL
001593*        PROGRAM    (PGM-NAME)
001594*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
001595*        LENGTH     (WS-COMM-LENGTH)
001596*    END-EXEC.
      *    MOVE '.$C                   %   #00003346' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033333436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001597
001598 9400-CLEAR.
001599     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
001600     GO TO 9300-XCTL.
001601
001602 9500-PF12.
001603     MOVE XCTL-010               TO PGM-NAME.
001604     GO TO 9300-XCTL.
001605
001606 9600-PGMID-ERROR.
001607     
      * EXEC CICS HANDLE CONDITION
001608*        PGMIDERR    (8300-SEND-TEXT)
001609*    END-EXEC.
      *    MOVE '"$L                   ! ) #00003361' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303033333631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001610
001611     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
001612     MOVE ' '                    TO PI-ENTRY-CD-1.
001613     MOVE XCTL-005               TO PGM-NAME.
001614     MOVE PGM-NAME               TO LOGOFF-PGM.
001615     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
001616     GO TO 9300-XCTL.
001617
001618 9700-DATE-CONVERT.
001619     
      * EXEC CICS LINK
001620*        PROGRAM    ('ELDATCV')
001621*        COMMAREA   (DATE-CONVERSION-DATA)
001622*        LENGTH     (DC-COMM-LENGTH)
001623*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00003373' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033333733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001624
001625 9700-EXIT.
001626     EXIT.
001627
001628 EJECT
001629 9900-ERROR-FORMAT.
001630
001631     IF NOT EMI-ERRORS-COMPLETE
001632         MOVE LINK-001           TO PGM-NAME
001633         
      * EXEC CICS LINK
001634*            PROGRAM    (PGM-NAME)
001635*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
001636*            LENGTH     (EMI-COMM-LENGTH)
001637*        END-EXEC.
      *    MOVE '."C                   (   #00003387' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033333837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001638
001639 9900-EXIT.
001640     EXIT.
001641
001642 9990-ABEND.
001643     MOVE LINK-004               TO PGM-NAME.
001644     MOVE DFHEIBLK               TO EMI-LINE1.
001645     
      * EXEC CICS LINK
001646*        PROGRAM   (PGM-NAME)
001647*        COMMAREA  (EMI-LINE1)
001648*        LENGTH    (72)
001649*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003399' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033333939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001650
001651     GO TO 8200-SEND-DATAONLY.
001652
001653 9995-SECURITY-VIOLATION.
001654*           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003426' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033343236' TO DFHEIV0
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
001655
001656 9995-EXIT.
001657      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6521' TO DFHEIV1
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
               GO TO 0700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7100-NOTOPEN,
                     7100-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7300-NOTFND,
                     7300-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7499-EXIT,
                     7450-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7500-NOT-FOUND,
                     7500-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7450-NOTOPEN,
                     7610-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6521' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
