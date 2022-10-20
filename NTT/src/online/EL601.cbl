      *((program: EL601.cl2))
000001 ID DIVISION.
000002
000003 PROGRAM-ID. EL601.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 12/06/94 08:32:30.
000007*                            VMOD=2.008.
000008*
000009*
000010*AUTHOR.     LOGIC,INC.
000011*            DALLAS, TEXAS.
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
000024
000025*REMARKS.    TRANSACTION - EG1A - SYSTEM MAINTENANCE MENU.
000026******************************************************************
000027*                   C H A N G E   L O G
000028*
000029* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000030*-----------------------------------------------------------------
000031*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000032* EFFECTIVE    NUMBER
000033*-----------------------------------------------------------------
000034* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000035* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000036******************************************************************
000037
000038 ENVIRONMENT DIVISION.
000039
000040     EJECT
000041 DATA DIVISION.
000042 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000043 77  FILLER  PIC X(32)  VALUE '********************************'.
000044 77  FILLER  PIC X(32)  VALUE '*    EL601 WORKING STORAGE     *'.
000045 77  FILLER  PIC X(32)  VALUE '*************V/M 2.008 *********'.
000046
000047 01  WS-DATE-AREA.
000048     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000049     05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
000050
000051 01  MISC-WORK-AREAS.
000052
000053     12  SCREEN-SELECT-CHANGES.
000054         16  SCREEN-RESERVE-10   PIC X(32) VALUE
000055                 ' 10. RESERVED'.
000056         16  SCREEN-RESERVE-11   PIC X(32) VALUE
000057                 ' 11. RESERVED'.
000058         16  SCREEN-RESERVE-13   PIC X(32) VALUE
000059                 ' 13. RESERVED'.
000060         16  SCREEN-RESERVE-14   PIC X(32) VALUE
000061                 ' 14. RESERVED'.
000062         16  SCREEN-RESERVE-15   PIC X(32) VALUE
000063                 ' 15. RESERVED'.
000064         16  SCREEN-RESERVE-16   PIC X(32) VALUE
000065                 ' 16. RESERVED'.
000066         16  SCREEN-RESERVE-17   PIC X(32) VALUE
000067                 ' 17. RESERVED'.
000068         16  SCREEN-SELECT-10    PIC X(32) VALUE
000069                 ' 10. GENERAL LEDGER TABLE LOOKUP'.
000070         16  SCREEN-SELECT-11    PIC X(32) VALUE
000071                 ' 11. RETRO HISTORY MAINTENANCE'.
000072         16  SCREEN-SELECT-12    PIC X(32) VALUE
000073                 ' 12. BANK MASTER MAINTENANCE'.
000074*        16  SCREEN-SELECT-13    PIC X(32) VALUE
000075*                ' 13. RETRO MASTER MAINTENANCE'.
000076         16  SCREEN-SELECT-13    PIC X(32) VALUE
000077                 ' 13. LIFE CLAIM INT. MAINT   '.
000078         16  SCREEN-SELECT-14    PIC X(29) VALUE
000079                 ' 14. RESIDENT STATE TAX MAINT'.
000080         16  SCREEN-SELECT-15    PIC X(38) VALUE
000081                 ' 15. ACCOUNT RESIDENT STATE COMM MAINT'.
000082         16  SCREEN-SELECT-16    PIC X(37) VALUE
000083                 ' 16. RESIDENT ST COMMISSION CAP MAINT'.
000084         16  SCREEN-SELECT-17    PIC X(37) VALUE
000085                 ' 17. CODES TABLE MAINTENANCE'.
000086         16  SCREEN-SELECT-18    PIC X(37) VALUE
000087                 ' 18. HIERARCHY TABLE'.
000088
000089     12  MAP-EL601A          PIC X(8)    VALUE 'EL601A'.
000090     12  MAPSET-EL601S       PIC X(8)    VALUE 'EL601S'.
000091     12  TRANS-EXA1          PIC X(4)    VALUE 'EXA1'.
000092     12  PGM-EL601           PIC X(8)    VALUE 'EL601'.
000093     12  PGM-NAME            PIC X(8).
000094     12  TIME-IN             PIC S9(7).
000095     12  TIME-OUT-R  REDEFINES TIME-IN.
000096         16  FILLER          PIC X.
000097         16  TIME-OUT        PIC 99V99.
000098         16  FILLER          PIC X(2).
000099     12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
000100     12  LINK-EL001          PIC X(8)    VALUE 'EL001'.
000101     12  LINK-EL004          PIC X(8)    VALUE 'EL004'.
000102
000103     12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.
000104     12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.
000105     12  XCTL-EL102          PIC X(8)    VALUE 'EL102'.
000106     12  XCTL-EL103          PIC X(8)    VALUE 'EL103'.
000107     12  XCTL-EL104          PIC X(8)    VALUE 'EL104'.
000108     12  XCTL-EL105          PIC X(8)    VALUE 'EL105'.
000109     12  XCTL-EL106          PIC X(8)    VALUE 'EL106'.
000110     12  XCTL-EL107          PIC X(8)    VALUE 'EL107'.
000111     12  XCTL-EL108          PIC X(8)    VALUE 'EL108'.
000112     12  XCTL-EL110          PIC X(8)    VALUE 'EL110'.
000113     12  XCTL-EL111          PIC X(8)    VALUE 'EL111'.
000114     12  XCTL-EL112          PIC X(8)    VALUE 'EL112'.
000115     12  XCTL-EL115          PIC X(8)    VALUE 'EL115'.
000116     12  XCTL-EL119          PIC X(8)    VALUE 'EL119'.
000117     12  XCTL-EL126          PIC X(8)    VALUE 'EL800'.
000118     12  XCTL-EL158          PIC X(8)    VALUE 'EL158'.
000119     12  XCTL-EL159          PIC X(8)    VALUE 'EL159'.
000120     12  XCTL-EL400          PIC X(8)    VALUE 'EL400NCB'.
000121     12  XCTL-EL602          PIC X(8)    VALUE 'EL602'.
000122     12  XCTL-EL603          PIC X(8)    VALUE 'EL603'.
000123     12  XCTL-EL604          PIC X(8)    VALUE 'EL604'.
000124     12  XCTL-EL605          PIC X(8)    VALUE 'EL605'.
000125     12  XCTL-EL606          PIC X(8)    VALUE 'EL606'.
000126     12  XCTL-EL607          PIC X(8)    VALUE 'EL607'.
000127     12  XCTL-EL608          PIC X(8)    VALUE 'EL608 '.
000128     12  XCTL-EL610          PIC X(8)    VALUE 'EL610'.
000129     12  XCTL-EL611          PIC X(8)    VALUE 'EL611'.
000130     12  XCTL-EL613          PIC X(8)    VALUE 'EL613'.
000131     12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.
000132     12  XCTL-EL650          PIC X(8)    VALUE 'EL650'.
000133     12  XCTL-EL651          PIC X(8)    VALUE 'EL651'.
000134     12  XCTL-EL652          PIC X(8)    VALUE 'EL652'.
000135     12  XCTL-EL653          PIC X(8)    VALUE 'EL653'.
000136     12  XCTL-EL654          PIC X(8)    VALUE 'EL654'.
000137     12  XCTL-EL656          PIC X(8)    VALUE 'EL656'.
000138     12  XCTL-EL658          PIC X(8)    VALUE 'EL658'.
000139     12  XCTL-EL659          PIC X(8)    VALUE 'EL659'.
000140     12  XCTL-EL1062         PIC X(8)    VALUE 'EL1062'.
000141     12  XCTL-EL1064         PIC X(8)    VALUE 'EL1064'.
000142
000143     12  ER-0002             PIC X(4)    VALUE '0002'.
000144     12  ER-2370             PIC X(4)    VALUE '2370'.
000145     12  ER-2371             PIC X(4)    VALUE '2371'.
000146     12  ER-7000             PIC X(4)    VALUE '7000'.
000147     12  ER-7001             PIC X(4)    VALUE '7001'.
000148     12  ER-7002             PIC X(4)    VALUE '7002'.
000149     12  ER-7003             PIC X(4)    VALUE '7003'.
000150     12  ER-7008             PIC X(4)    VALUE '7008'.
000151     12  ER-7448             PIC X(4)    VALUE '7448'.
000152
000153     EJECT
000154*                            COPY ELCLOGOF.
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
000155
000156     EJECT
000157*                            COPY ELCDATE.
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
000158
000159     EJECT
000160*                            COPY ELCATTR.
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
000161
000162     EJECT
000163*                            COPY ELCEMIB.
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
000164
000165     EJECT
000166*                            COPY ELCINTF.
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
000167     12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
000168         16  PI-FILE-ID                    PIC XX.
000169         16  FILLER                        PIC X(638).
000170
000171     EJECT
000172*                            COPY ELCAID.
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
000173 01  FILLER    REDEFINES DFHAID.
000174     12  FILLER              PIC X(8).
000175     12  PF-VALUES           PIC X       OCCURS 2.
000176
000177     EJECT
000178*                            COPY EL601S.
      *>>((file: EL601S))
000001 01  EL601AI.
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
000016     05  ITEM11L PIC S9(0004) COMP.
000017     05  ITEM11F PIC  X(0001).
000018     05  FILLER REDEFINES ITEM11F.
000019         10  ITEM11A PIC  X(0001).
000020     05  ITEM11I PIC  X(0032).
000021*    -------------------------------
000022     05  ITEM13L PIC S9(0004) COMP.
000023     05  ITEM13F PIC  X(0001).
000024     05  FILLER REDEFINES ITEM13F.
000025         10  ITEM13A PIC  X(0001).
000026     05  ITEM13I PIC  X(0032).
000027*    -------------------------------
000028     05  ITEM14L PIC S9(0004) COMP.
000029     05  ITEM14F PIC  X(0001).
000030     05  FILLER REDEFINES ITEM14F.
000031         10  ITEM14A PIC  X(0001).
000032     05  ITEM14I PIC  X(0029).
000033*    -------------------------------
000034     05  ITEM15L PIC S9(0004) COMP.
000035     05  ITEM15F PIC  X(0001).
000036     05  FILLER REDEFINES ITEM15F.
000037         10  ITEM15A PIC  X(0001).
000038     05  ITEM15I PIC  X(0038).
000039*    -------------------------------
000040     05  ITEM16L PIC S9(0004) COMP.
000041     05  ITEM16F PIC  X(0001).
000042     05  FILLER REDEFINES ITEM16F.
000043         10  ITEM16A PIC  X(0001).
000044     05  ITEM16I PIC  X(0037).
000045*    -------------------------------
000046     05  ITEM17L PIC S9(0004) COMP.
000047     05  ITEM17F PIC  X(0001).
000048     05  FILLER REDEFINES ITEM17F.
000049         10  ITEM17A PIC  X(0001).
000050     05  ITEM17I PIC  X(0037).
000051*    -------------------------------
000052     05  ITEM18L PIC S9(0004) COMP.
000053     05  ITEM18F PIC  X(0001).
000054     05  FILLER REDEFINES ITEM18F.
000055         10  ITEM18A PIC  X(0001).
000056     05  ITEM18I PIC  X(0037).
000057*    -------------------------------
000058     05  ERRMSGL PIC S9(0004) COMP.
000059     05  ERRMSGF PIC  X(0001).
000060     05  FILLER REDEFINES ERRMSGF.
000061         10  ERRMSGA PIC  X(0001).
000062     05  ERRMSGI PIC  X(0079).
000063*    -------------------------------
000064     05  SELECTL PIC S9(0004) COMP.
000065     05  SELECTF PIC  X(0001).
000066     05  FILLER REDEFINES SELECTF.
000067         10  SELECTA PIC  X(0001).
000068     05  SELECTI PIC  X(0002).
000069*    -------------------------------
000070     05  PROGL PIC S9(0004) COMP.
000071     05  PROGF PIC  X(0001).
000072     05  FILLER REDEFINES PROGF.
000073         10  PROGA PIC  X(0001).
000074     05  PROGI PIC  X(0008).
000075*    -------------------------------
000076     05  PFKEYL PIC S9(0004) COMP.
000077     05  PFKEYF PIC  X(0001).
000078     05  FILLER REDEFINES PFKEYF.
000079         10  PFKEYA PIC  X(0001).
000080     05  PFKEYI PIC  X(0002).
000081 01  EL601AO REDEFINES EL601AI.
000082     05  FILLER            PIC  X(0012).
000083*    -------------------------------
000084     05  FILLER            PIC  X(0003).
000085     05  DATEO PIC  X(0008).
000086*    -------------------------------
000087     05  FILLER            PIC  X(0003).
000088     05  TIMEO PIC  99.99.
000089*    -------------------------------
000090     05  FILLER            PIC  X(0003).
000091     05  ITEM11O PIC  X(0032).
000092*    -------------------------------
000093     05  FILLER            PIC  X(0003).
000094     05  ITEM13O PIC  X(0032).
000095*    -------------------------------
000096     05  FILLER            PIC  X(0003).
000097     05  ITEM14O PIC  X(0029).
000098*    -------------------------------
000099     05  FILLER            PIC  X(0003).
000100     05  ITEM15O PIC  X(0038).
000101*    -------------------------------
000102     05  FILLER            PIC  X(0003).
000103     05  ITEM16O PIC  X(0037).
000104*    -------------------------------
000105     05  FILLER            PIC  X(0003).
000106     05  ITEM17O PIC  X(0037).
000107*    -------------------------------
000108     05  FILLER            PIC  X(0003).
000109     05  ITEM18O PIC  X(0037).
000110*    -------------------------------
000111     05  FILLER            PIC  X(0003).
000112     05  ERRMSGO PIC  X(0079).
000113*    -------------------------------
000114     05  FILLER            PIC  X(0003).
000115     05  SELECTO PIC  X(0002).
000116*    -------------------------------
000117     05  FILLER            PIC  X(0003).
000118     05  PROGO PIC  X(0008).
000119*    -------------------------------
000120     05  FILLER            PIC  X(0003).
000121     05  PFKEYO PIC  X(0002).
000122*    -------------------------------
      *<<((file: EL601S))
000179
000180     EJECT
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
000182 01  DFHCOMMAREA             PIC X(1024).
000183
000184     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL601' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000185 VCOBOL-DUMMY-PROCEDURE.
000186
000187     MOVE DFHCOMMAREA   TO PROGRAM-INTERFACE-BLOCK.
000188
000189     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000190     MOVE '5'                    TO DC-OPTION-CODE.
000191     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000192     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000193     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000194
000195     IF EIBCALEN = 0
000196         GO TO 8800-UNAUTHORIZED-ACCESS.
000197
000198     IF PI-CALLING-PROGRAM NOT = PGM-EL601
000199         IF PI-RETURN-TO-PROGRAM NOT = PGM-EL601
000200             MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6
000201             MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5
000202             MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4
000203             MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3
000204             MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2
000205             MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1
000206             MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM
000207             MOVE PGM-EL601              TO PI-CALLING-PROGRAM
000208         ELSE
000209             MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM
000210             MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM
000211             MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1
000212             MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2
000213             MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3
000214             MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4
000215             MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5
000216             MOVE SPACES                 TO PI-SAVED-PROGRAM-6.
000217
000218     
      * EXEC CICS HANDLE CONDITION
000219*        PGMIDERR  (9600-PGMID-ERROR)
000220*        ERROR     (9990-ABEND)
000221*    END-EXEC.
      *    MOVE '"$L.                  ! " #00001135' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303031313335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000222
000223     IF EIBTRNID  = TRANS-EXA1
000224         GO TO 0100-SAME-TRAN.
000225
000226     GO TO 8100-SEND-INITIAL-MAP.
000227
000228 0100-SAME-TRAN.
000229     IF EIBAID = DFHCLEAR
000230         GO TO 9400-CLEAR.
000231
000232     EJECT
000233 0200-RECEIVE.
000234     MOVE LOW-VALUES   TO EL601AI.
000235
000236     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000237         MOVE -1                 TO SELECTL
000238         MOVE ER-7008         TO EMI-ERROR
000239         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000240         GO TO 8200-SEND-DATAONLY.
000241
000242     
      * EXEC CICS RECEIVE
000243*        MAP      (MAP-EL601A)
000244*        MAPSET   (MAPSET-EL601S)
000245*        INTO     (EL601AI)
000246*    END-EXEC.
           MOVE LENGTH OF
            EL601AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001159' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303031313539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-EL601A, 
                 EL601AI, 
                 DFHEIV11, 
                 MAPSET-EL601S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000247
000248     MOVE SPACES                 TO PI-ENTRY-CD-1.
000249
000250     IF EIBAID = DFHPF12
000251        GO TO 9500-HELP
000252     ELSE
000253        IF EIBAID = DFHPF23
000254           GO TO 8810-PF23
000255        ELSE
000256           IF EIBAID = DFHPF24
000257              GO TO 9200-PF24.
000258
000259     IF PFKEYL = ZEROS
000260         NEXT SENTENCE
000261     ELSE
000262         IF PFKEYI = '12'
000263            GO TO 9500-HELP
000264         ELSE
000265            IF PFKEYI = '23'
000266               GO TO 8810-PF23
000267            ELSE
000268               IF PFKEYI = '24'
000269                  GO TO 9200-PF24.
000270
000271     IF EIBAID NOT = DFHENTER
000272         MOVE ER-0002         TO EMI-ERROR
000273         GO TO 0320-INPUT-ERROR.
000274
000275     IF SELECTL = 0
000276         MOVE ER-0002         TO EMI-ERROR
000277         GO TO 0320-INPUT-ERROR.
000278
000279 0310-CHECK-PFKEYS.
000280     IF SELECTI = '01'
000281         MOVE XCTL-EL650 TO PGM-NAME
000282         GO TO 9300-XCTL.
000283
000284     IF SELECTI = '02'
000285         MOVE XCTL-EL652 TO PGM-NAME
000286         GO TO 9300-XCTL.
000287
000288     IF SELECTI = '03'
000289         MOVE XCTL-EL656 TO PGM-NAME
000290         GO TO 9300-XCTL.
000291
000292     IF SELECTI = '04'
000293         MOVE XCTL-EL651 TO PGM-NAME
000294         GO TO 9300-XCTL.
000295
000296     IF SELECTI = '05'
000297         MOVE XCTL-EL653 TO PGM-NAME
000298         GO TO 9300-XCTL.
000299
000300     IF SELECTI = '06'
000301         MOVE XCTL-EL658 TO PGM-NAME
000302         GO TO 9300-XCTL.
000303
000304     IF SELECTI = '07'
000305         MOVE XCTL-EL610 TO PGM-NAME
000306         GO TO 9300-XCTL.
000307
000308     IF SELECTI = '08'
000309         MOVE XCTL-EL659 TO PGM-NAME
000310         GO TO 9300-XCTL.
000311
000312     IF SELECTI = '09'
000313         MOVE XCTL-EL158 TO PGM-NAME
000314         GO TO 9300-XCTL.
000315
000316     IF SELECTI = '10'
000317        MOVE XCTL-EL159 TO PGM-NAME
000318             GO TO 9300-XCTL.
000319
000320     IF SELECTI = '11'
000321         IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
000322            MOVE XCTL-EL607 TO PGM-NAME
000323            GO TO 9300-XCTL.
000324
000325     IF SELECTI = '12'
000326       IF PI-AR-PROCESSING
000327          MOVE XCTL-EL611 TO PGM-NAME
000328          GO TO 9300-XCTL
000329       ELSE
000330          MOVE ER-7448   TO EMI-ERROR
000331          GO TO 0320-INPUT-ERROR.
000332
000333     IF SELECTI = '13'
000334        IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
000335           MOVE XCTL-EL605       TO PGM-NAME
000336           GO TO 9300-XCTL
000337        END-IF
000338     END-IF
000339
000340     IF SELECTI = '13'
000341         IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
000342             MOVE XCTL-EL606 TO PGM-NAME
000343             GO TO 9300-XCTL.
000344
000345     IF SELECTI = '14'
000346         IF PI-COMPANY-ID  =  'DMD'
000347             MOVE XCTL-EL1062 TO PGM-NAME
000348             GO TO 9300-XCTL.
000349
000350     IF SELECTI = '15'
000351         IF PI-COMPANY-ID  =  'DMD'
000352             MOVE XCTL-EL608 TO PGM-NAME
000353             GO TO 9300-XCTL.
000354
000355     IF SELECTI = '16'
000356         IF PI-COMPANY-ID  =  'DMD'
000357             MOVE XCTL-EL1064 TO PGM-NAME
000358             GO TO 9300-XCTL.
000359
000360*    IF SELECTI = '17'
000361*        IF PI-COMPANY-ID  =  'DMD'
000362*            MOVE XCTL-ELXXXX TO PGM-NAME (CODES TABLE MAINT)
000363*            GO TO 9300-XCTL.
000364*
000365*CODE ABOVE COMMENTED OUT PENDING SPECS APPROVAL
000366*
000367
000368     IF SELECTI = '18'
000369         IF PI-COMPANY-ID  =  'NCB'
000370             MOVE XCTL-EL400 TO PGM-NAME
000371             GO TO 9300-XCTL.
000372
000373     IF SELECTI = '21'
000374         MOVE XCTL-EL102 TO PGM-NAME
000375         GO TO 9300-XCTL.
000376
000377     IF SELECTI = '22'
000378         MOVE XCTL-EL105 TO PGM-NAME
000379         GO TO 9300-XCTL.
000380
000381     IF SELECTI = '23'
000382       IF PI-AR-PROCESSING
000383          MOVE XCTL-EL613 TO PGM-NAME
000384          GO TO 9300-XCTL
000385       ELSE
000386          MOVE ER-7448   TO EMI-ERROR
000387          GO TO 0320-INPUT-ERROR.
000388
000389     IF SELECTI = '24'
000390         MOVE XCTL-EL106 TO PGM-NAME
000391         GO TO 9300-XCTL.
000392
000393     IF SELECTI = '25'
000394        MOVE XCTL-EL115          TO PGM-NAME
000395        GO TO 9300-XCTL
000396     END-IF
000397
000398     IF SELECTI = '26'
000399         MOVE XCTL-EL107 TO PGM-NAME
000400         GO TO 9300-XCTL.
000401
000402     IF SELECTI = '27'
000403         MOVE XCTL-EL604 TO PGM-NAME
000404         GO TO 9300-XCTL.
000405
000406     IF SELECTI = '28'
000407         MOVE XCTL-EL602 TO PGM-NAME
000408         GO TO 9300-XCTL.
000409
000410     IF SELECTI = '29'
000411         MOVE XCTL-EL603 TO PGM-NAME
000412         GO TO 9300-XCTL.
000413
000414     IF SELECTI = '30'
000415         MOVE XCTL-EL654 TO PGM-NAME
000416         GO TO 9300-XCTL.
000417
000418     IF SELECTI = '31'
000419         MOVE XCTL-EL103 TO PGM-NAME
000420         GO TO 9300-XCTL.
000421
000422     IF SELECTI = '32'
000423         MOVE XCTL-EL119 TO PGM-NAME
000424         GO TO 9300-XCTL.
000425
000426     IF SELECTI = '33'
000427         MOVE XCTL-EL108 TO PGM-NAME
000428         GO TO 9300-XCTL.
000429
000430     IF SELECTI = '34'
000431         MOVE XCTL-EL104 TO PGM-NAME
000432         GO TO 9300-XCTL.
000433
000434     IF SELECTI = '35'
000435         MOVE XCTL-EL110 TO PGM-NAME
000436         GO TO 9300-XCTL.
000437
000438     IF SELECTI = '36'
000439         MOVE XCTL-EL111 TO PGM-NAME
000440         GO TO 9300-XCTL.
000441
000442     IF SELECTI = '37'
000443         MOVE XCTL-EL112 TO PGM-NAME
000444         GO TO 9300-XCTL.
000445
000446     MOVE ER-7002                TO EMI-ERROR.
000447
000448 0320-INPUT-ERROR.
000449     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000450     MOVE AL-UNBON               TO SELECTA.
000451     MOVE -1                     TO SELECTL.
000452     GO TO 8200-SEND-DATAONLY.
000453
000454     EJECT
000455 8100-SEND-INITIAL-MAP.
000456     MOVE LOW-VALUES             TO EL601AO.
000457     MOVE EIBTIME                TO TIME-IN.
000458     MOVE TIME-OUT               TO TIMEO.
000459     MOVE SAVE-DATE              TO DATEO.
000460
000461     MOVE SCREEN-RESERVE-11      TO ITEM11O.
000462
000463     MOVE SCREEN-SELECT-12       TO ITEM13O.
000464
000465     IF PI-COMPANY-ID  =  'CID' OR 'AHL' OR 'FNL'
000466        MOVE SCREEN-SELECT-13    TO ITEM14O
000467     END-IF
000468
000469     IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
000470         MOVE SCREEN-SELECT-11   TO ITEM11O
000471         MOVE SCREEN-SELECT-13   TO ITEM14O
000472         MOVE SPACES             TO ITEM15O ITEM16O
000473                                    ITEM17O ITEM18O.
000474
000475     IF PI-COMPANY-ID  =  'DMD'
000476         MOVE SCREEN-RESERVE-13  TO  ITEM14O
000477         MOVE SCREEN-SELECT-14   TO  ITEM15O
000478         MOVE SCREEN-SELECT-15   TO  ITEM16O
000479         MOVE SCREEN-SELECT-16   TO  ITEM17O
000480         MOVE SPACES             TO  ITEM18O.
000481*        MOVE SCREEN-SELECT-17   TO  ITEM17O.
000482
000483     IF PI-COMPANY-ID  =  'NCB'
000484         MOVE SCREEN-RESERVE-13  TO ITEM14O
000485         MOVE SCREEN-RESERVE-14  TO ITEM15O
000486         MOVE SCREEN-RESERVE-15  TO ITEM16O
000487         MOVE SCREEN-RESERVE-16  TO ITEM17O
000488         MOVE SCREEN-SELECT-18   TO ITEM18O.
000489
000490     MOVE -1                     TO SELECTL.
000491
000492     
      * EXEC CICS SEND
000493*        MAP     (MAP-EL601A)
000494*        MAPSET  (MAPSET-EL601S)
000495*        FROM    (EL601AO)
000496*        ERASE
000497*        CURSOR
000498*    END-EXEC.
           MOVE LENGTH OF
            EL601AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001409' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303031343039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-EL601A, 
                 EL601AO, 
                 DFHEIV12, 
                 MAPSET-EL601S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000499
000500     GO TO 9100-RETURN-TRAN.
000501
000502 8200-SEND-DATAONLY.
000503     MOVE SAVE-DATE              TO DATEO.
000504     MOVE EIBTIME                TO TIME-IN.
000505     MOVE TIME-OUT               TO TIMEO.
000506     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
000507
000508     MOVE SCREEN-RESERVE-11      TO ITEM11O.
000509
000510     MOVE SCREEN-SELECT-12       TO ITEM13O.
000511
000512     IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
000513         MOVE SCREEN-SELECT-11   TO  ITEM11O
000514         MOVE SCREEN-SELECT-13   TO  ITEM14O
000515         MOVE SPACES             TO ITEM15O ITEM16O
000516                                    ITEM17O ITEM18O.
000517
000518     IF PI-COMPANY-ID  =  'DMD'
000519         MOVE SCREEN-RESERVE-13  TO  ITEM14O
000520         MOVE SCREEN-SELECT-14   TO  ITEM15O
000521         MOVE SCREEN-SELECT-15   TO  ITEM16O
000522         MOVE SCREEN-SELECT-16   TO  ITEM17O
000523         MOVE SPACES             TO  ITEM18O.
000524*        MOVE SCREEN-SELECT-17   TO  ITEM17O.
000525
000526     IF PI-COMPANY-ID  =  'NCB'
000527         MOVE SCREEN-RESERVE-13  TO ITEM14O
000528         MOVE SCREEN-RESERVE-14  TO ITEM15O
000529         MOVE SCREEN-RESERVE-15  TO ITEM16O
000530         MOVE SCREEN-RESERVE-16  TO ITEM17O
000531         MOVE SCREEN-SELECT-18   TO ITEM18O.
000532
000533     
      * EXEC CICS SEND
000534*        MAP      (MAP-EL601A)
000535*        MAPSET   (MAPSET-EL601S)
000536*        FROM     (EL601AO)
000537*        DATAONLY
000538*        CURSOR
000539*    END-EXEC.
           MOVE LENGTH OF
            EL601AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001450' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303031343530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-EL601A, 
                 EL601AO, 
                 DFHEIV12, 
                 MAPSET-EL601S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000540
000541     GO TO 9100-RETURN-TRAN.
000542
000543 8300-SEND-TEXT.
000544     
      * EXEC CICS SEND TEXT
000545*        FROM     (LOGOFF-TEXT)
000546*        LENGTH   (LOGOFF-LENGTH)
000547*        ERASE
000548*        FREEKB
000549*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001461' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303031343631' TO DFHEIV0
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
           
000550
000551     
      * EXEC CICS RETURN
000552*    END-EXEC.
      *    MOVE '.(                    ''   #00001468' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031343638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000553
000554 8800-UNAUTHORIZED-ACCESS.
000555     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
000556     GO TO 8300-SEND-TEXT.
000557
000558 8810-PF23.
000559     MOVE DFHPF23                TO PI-ENTRY-CD-1.
000560     MOVE XCTL-EL005             TO PGM-NAME.
000561     GO TO 9300-XCTL.
000562
000563 9100-RETURN-TRAN.
000564     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
000565     MOVE '601A'                 TO PI-CURRENT-SCREEN-NO.
000566
000567     
      * EXEC CICS RETURN
000568*        TRANSID   (TRANS-EXA1)
000569*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
000570*        LENGTH    (PI-COMM-LENGTH)
000571*    END-EXEC.
      *    MOVE '.(CT                  ''   #00001484' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031343834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXA1, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000572
000573 9200-PF24.
000574     MOVE XCTL-EL626             TO PGM-NAME.
000575     GO TO 9300-XCTL.
000576
000577 9300-XCTL.
000578     MOVE SPACES                 TO PI-ENTRY-CD-2
000579                                    PI-RETURN-CODES
000580                                    PI-UPDATE-BY
000581                                    PI-PROGRAM-WORK-AREA.
000582     MOVE ZEROS                  TO PI-UPDATE-HHMMSS.
000583
000584     
      * EXEC CICS XCTL
000585*        PROGRAM    (PGM-NAME)
000586*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
000587*        LENGTH     (PI-COMM-LENGTH)
000588*    END-EXEC.
      *    MOVE '.$C                   %   #00001501' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303031353031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000589
000590 9400-CLEAR.
000591     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
000592     GO TO 9300-XCTL.
000593
000594 9500-HELP.
000595     MOVE XCTL-EL010             TO PGM-NAME.
000596     GO TO 9300-XCTL.
000597
000598 9600-PGMID-ERROR.
000599     MOVE PGM-NAME               TO PROGO.
000600     MOVE AL-UNBON               TO SELECTA.
000601     MOVE ER-7003                TO EMI-ERROR.
000602
000603     
      * EXEC CICS HANDLE CONDITION
000604*        PGMIDERR  (8300-SEND-TEXT)
000605*    END-EXEC.
      *    MOVE '"$L                   ! # #00001520' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303031353230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000606
000607     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
000608     MOVE ' '                    TO PI-ENTRY-CD-1.
000609     MOVE XCTL-EL005             TO PGM-NAME.
000610     MOVE PGM-NAME               TO LOGOFF-PGM.
000611     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
000612     GO TO 9300-XCTL.
000613
000614 9700-LINK-DATE-CONVERT.
000615     
      * EXEC CICS LINK
000616*        PROGRAM    (LINK-ELDATCV)
000617*        COMMAREA   (DATE-CONVERSION-DATA)
000618*        LENGTH     (DC-COMM-LENGTH)
000619*    END-EXEC.
      *    MOVE '."C                   (   #00001532' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031353332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000620
000621 9700-EXIT.
000622     EXIT.
000623
000624 9900-ERROR-FORMAT.
000625     IF NOT EMI-ERRORS-COMPLETE
000626         MOVE LINK-EL001         TO PGM-NAME
000627         
      * EXEC CICS LINK
000628*            PROGRAM    (PGM-NAME)
000629*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
000630*            LENGTH     (EMI-COMM-LENGTH)
000631*        END-EXEC.
      *    MOVE '."C                   (   #00001544' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031353434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000632
000633 9900-EXIT.
000634     EXIT.
000635
000636 9990-ABEND.
000637     MOVE LINK-EL004             TO PGM-NAME.
000638     MOVE DFHEIBLK               TO EMI-LINE1.
000639
000640     
      * EXEC CICS LINK
000641*        PROGRAM   (PGM-NAME)
000642*        COMMAREA  (EMI-LINE1)
000643*        LENGTH    (72)
000644*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00001557' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303031353537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000645
000646     GO TO 8200-SEND-DATAONLY.
000647

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL601' TO DFHEIV1
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
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL601' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
