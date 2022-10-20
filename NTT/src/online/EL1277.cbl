      *((program: EL1277.cl2))
000001 ID DIVISION.
000002
000003 PROGRAM-ID. EL1277.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 02/13/96 10:00:12.
000007*                            VMOD=2.005.
000008*
000009*
000010*AUTHOR.     LOGIC,INC.
000011*            DALLAS, TEXAS.
000012
000013*DATE-COMPILED.
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
000025*REMARKS.    TRANSACTION - EXX7 - CERTIFICATE MAILING DATA.
000026*
000027******************************************************************
000028*                   C H A N G E   L O G
000029*
000030* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000031*-----------------------------------------------------------------
000032*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000033* EFFECTIVE    NUMBER
000034*-----------------------------------------------------------------
000035* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000036* 080406    2006051800002  AJRA  ADD POSTCARD MAIL STATUS
000037* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000038* 070313  CR2013052300001  PEMA  REMOVE SSN FROM MAP.
000039* 120513  IR2013112500001  PEMA  CHANGE EDIT ON ADDRESS
000040* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000041* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
000042* 042221  IR2021041300001  PEMA  Fix birth date problems.
000043******************************************************************
000044
000045 ENVIRONMENT DIVISION.
000046
000047     EJECT
000048 DATA DIVISION.
000049 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000050 77  FILLER  PIC X(32)  VALUE '********************************'.
000051 77  FILLER  PIC X(32)  VALUE '*    EL1277 WORKING STORAGE    *'.
000052 77  FILLER  PIC X(32)  VALUE '************ V/M 2.005 *********'.
000053
000054*    COPY ELCSCTM.
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
000055
000056*    COPY ELCSCRTY.
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
000057
000058 01  WS-DATE-AREA.
000059     05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000060     05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
000061
000062 01  WS-RESPONSE                 PIC S9(8)   COMP.
000063     88  RESP-NORMAL                  VALUE +00.
000064     88  RESP-NOTFND                  VALUE +13.
000065     88  RESP-DUPKEY                  VALUE +15.
000066     88  RESP-NOTOPEN                 VALUE +19.
000067     88  RESP-ENDFILE                 VALUE +20.
000068 01  STANDARD-AREAS.
000069     12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.
000070     12  ELCNTL-KEY.
000071         16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
000072         16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
000073         16  ELCNTL-ACCESS.
000074             20  FILLER          PIC XX.
000075             20  FILLER          PIC XX.
000076         16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.
000077     12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
000078     12  GETMAIN-SPACE               PIC X       VALUE SPACE.
000079     12  MAP-NAME                    PIC X(8)    VALUE 'EL127G'.
000080     12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1277S'.
000081     12  SCREEN-NUMBER               PIC X(4)    VALUE '127G'.
000082     12  TRANS-ID                    PIC X(4)    VALUE 'EXX7'.
000083     12  THIS-PGM                    PIC X(8)    VALUE 'EL1277'.
000084     12  PGM-NAME                    PIC X(8).
000085     12  TIME-IN                     PIC S9(7).
000086     12  TIME-OUT-R  REDEFINES TIME-IN.
000087         16  FILLER                  PIC X.
000088         16  TIME-OUT                PIC 99V99.
000089         16  FILLER                  PIC X(2).
000090     12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
000091     12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
000092     12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
000093     12  XCTL-1272                   PIC X(8)    VALUE 'EL1272'.
000094     12  XCTL-1273                   PIC X(8)    VALUE 'EL1273'.
000095     12  XCTL-1274                   PIC X(8)    VALUE 'EL1274'.
000096     12  XCTL-1275                   PIC X(8)    VALUE 'EL1275'.
000097     12  XCTL-1276                   PIC X(8)    VALUE 'EL1276'.
000098     12  LINK-001                    PIC X(8)    VALUE 'EL001'.
000099     12  LINK-004                    PIC X(8)    VALUE 'EL004'.
000100     12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
000101     12  ERMAIL-ID                   PIC X(8)    VALUE 'ERMAIL'.
000102     12  ELCERT-ID                   PIC X(8)    VALUE 'ELCERT'.
000103     12  WS-RECORD-LENGTHS   COMP.
000104*        16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)  VALUE +250.
000105         16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)  VALUE +374.
000106         16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)  VALUE +450.
000107
000108     12  DEEDIT-FIELD                PIC X(15).
000109     12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
000110
000111     12  WS-BIRTHDT                  PIC XX.
000112     12  WS-JNT-BIRTHDT              PIC XX.
000113
000114     12  RETURN-FROM                 PIC X(8).
000115     12  QID.
000116         16  QID-TERM                PIC X(4).
000117         16  FILLER                  PIC X(4)    VALUE '127G'.
000118     12  WS-RECORD-FOUND-SW          PIC  X      VALUE SPACE.
000119         88  RECORD-FOUND                        VALUE 'Y'.
000120         88  RECORD-NOT-FOUND                    VALUE 'N'.
000121     12  WS-DUPREC-SW                PIC  X      VALUE SPACE.
000122         88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
000123     12  W-ZIP-TEST                  PIC  X(1)   VALUE SPACE.
000124         88  W-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
000125
000126     EJECT
000127     12  ERROR-MESSAGES.
000128         16  ER-0000                 PIC  X(4)   VALUE '0000'.
000129         16  ER-0004                 PIC  X(4)   VALUE '0004'.
000130         16  ER-0008                 PIC  X(4)   VALUE '0008'.
000131         16  ER-0023                 PIC  X(4)   VALUE '0023'.
000132         16  ER-0029                 PIC  X(4)   VALUE '0029'.
000133         16  ER-0070                 PIC  X(4)   VALUE '0070'.
000134         16  ER-0142                 PIC  X(4)   VALUE '0142'.
000135         16  ER-0219                 PIC  X(4)   VALUE '0219'.
000136         16  ER-0220                 PIC  X(4)   VALUE '0220'.
000137         16  er-2050                 pic  x(4)   value '2050'.
000138         16  ER-2187                 PIC  X(4)   VALUE '2187'.
000139         16  ER-2199                 PIC  X(4)   VALUE '2199'.
000140         16  ER-2209                 PIC  X(4)   VALUE '2209'.
000141         16  er-3061                 pic  x(4)   value '3061'.
000142         16  er-3062                 pic  x(4)   value '3062'.
000143         16  er-3063                 pic  x(4)   value '3063'.
000144         16  ER-7049                 PIC  X(4)   VALUE '7049'.
000145         16  ER-7428                 PIC  X(4)   VALUE '7428'.
000146
000147 01  WS-PHONE                        PIC 9(10)   VALUE ZEROS.
000148 01  WS-PHONE-R REDEFINES WS-PHONE.
000149     12  WS-PHONE-AC                 PIC 999.
000150     12  WS-PHONE-EXT                PIC 999.
000151     12  WS-PHONE-NO                 PIC 9999.
000152 01  EDITED-PHONE-NO                 PIC X(12)   VALUE ZEROS.
000153 01  EDITED-PHONE-NO-R REDEFINES EDITED-PHONE-NO.
000154     12  ED-PHONE-AC                 PIC 999.
000155     12  ED-PHONE-DASH1              PIC X.
000156     12  ED-PHONE-EXT                PIC 999.
000157     12  ED-PHONE-DASH2              PIC X.
000158     12  ED-PHONE-NO                 PIC 9999.
000159
000160
000161 01  WS-CM-CONTROL-PRIMARY.
000162     05  WS-CM-COMPANY-CD            PIC  X.
000163     05  WS-CM-CARRIER               PIC  X.
000164     05  WS-CM-GROUPING              PIC  X(6).
000165     05  WS-CM-STATE                 PIC  XX.
000166     05  WS-CM-ACCOUNT               PIC  X(10).
000167     05  WS-CM-CERT-EFF-DT           PIC  XX.
000168     05  WS-CM-CERT-NO.
000169         10  WS-CM-CERT-PRIME        PIC  X(10).
000170         10  WS-CM-CERT-SFX          PIC  X.
000171
000172 01  zipcd-pass-area-len         pic s9(4) comp value +67.
000173 01  zipcd-pass-area.
000174     03  PA-zip                  PIC X(5).
000175     03  PA-ErrorCode-zip        PIC X(10).
000176     03  PA-city                 PIC x(50).
000177     03  PA-state                PIC Xx.
000178
000179 01  WS-MAILED-AREA.
000180     05  WS-SUB                      PIC S9(5) COMP VALUE +0.
000181     05  WS-MAILED-BLD-STATUS.
000182         10  WS-MAILED-STAT          PIC X(5).
000183         10  FILLER                  PIC X(1) VALUE SPACE.
000184         10  WS-MAILED-DATE          PIC X(8).
000185
000186     05  WS-MAILED-DATA OCCURS 7 TIMES.
000187         10  WS-MAILED-TYPE          PIC X(3).
000188         10  WS-MAILED-STATUS        PIC X(14).
000189
000190*                                COPY ELCDATE.
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
000191*                                COPY ELCLOGOF.
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
000192*                                COPY ELCATTR.
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
000193*                                COPY ELCEMIB.
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
000194*                                COPY ELCINTF.
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
000195     12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
000196         16  FILLER                  PIC X(317).
000197         16  PI-PEND-SW              PIC X.
000198         16  FILLER                  PIC X(322).
000199
000200     EJECT
000201
000202*    COPY ELCAID.
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
000203 01  FILLER    REDEFINES DFHAID.
000204     12  FILLER                      PIC X(8).
000205     12  PF-VALUES                   PIC X       OCCURS 24 TIMES.
000206
000207     EJECT
000208*    COPY EL1277S.
      *>>((file: EL1277S))
000001 01  EL127GI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  GDATEL PIC S9(0004) COMP.
000005     05  GDATEF PIC  X(0001).
000006     05  FILLER REDEFINES GDATEF.
000007         10  GDATEA PIC  X(0001).
000008     05  GDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  GTIMEL PIC S9(0004) COMP.
000011     05  GTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES GTIMEF.
000013         10  GTIMEA PIC  X(0001).
000014     05  GTIMEI PIC  999V99.
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
000028     05  GMAINTL PIC S9(0004) COMP.
000029     05  GMAINTF PIC  X(0001).
000030     05  FILLER REDEFINES GMAINTF.
000031         10  GMAINTA PIC  X(0001).
000032     05  GMAINTI PIC  X(0001).
000033*    -------------------------------
000034     05  GCERTNOL PIC S9(0004) COMP.
000035     05  GCERTNOF PIC  X(0001).
000036     05  FILLER REDEFINES GCERTNOF.
000037         10  GCERTNOA PIC  X(0001).
000038     05  GCERTNOI PIC  X(0010).
000039*    -------------------------------
000040     05  GCRTSFXL PIC S9(0004) COMP.
000041     05  GCRTSFXF PIC  X(0001).
000042     05  FILLER REDEFINES GCRTSFXF.
000043         10  GCRTSFXA PIC  X(0001).
000044     05  GCRTSFXI PIC  X(0001).
000045*    -------------------------------
000046     05  GACCTNOL PIC S9(0004) COMP.
000047     05  GACCTNOF PIC  X(0001).
000048     05  FILLER REDEFINES GACCTNOF.
000049         10  GACCTNOA PIC  X(0001).
000050     05  GACCTNOI PIC  X(0010).
000051*    -------------------------------
000052     05  GSTATEL PIC S9(0004) COMP.
000053     05  GSTATEF PIC  X(0001).
000054     05  FILLER REDEFINES GSTATEF.
000055         10  GSTATEA PIC  X(0001).
000056     05  GSTATEI PIC  X(0002).
000057*    -------------------------------
000058     05  GCARIERL PIC S9(0004) COMP.
000059     05  GCARIERF PIC  X(0001).
000060     05  FILLER REDEFINES GCARIERF.
000061         10  GCARIERA PIC  X(0001).
000062     05  GCARIERI PIC  X(0001).
000063*    -------------------------------
000064     05  GGROUPL PIC S9(0004) COMP.
000065     05  GGROUPF PIC  X(0001).
000066     05  FILLER REDEFINES GGROUPF.
000067         10  GGROUPA PIC  X(0001).
000068     05  GGROUPI PIC  X(0006).
000069*    -------------------------------
000070     05  GEFFDTL PIC S9(0004) COMP.
000071     05  GEFFDTF PIC  X(0001).
000072     05  FILLER REDEFINES GEFFDTF.
000073         10  GEFFDTA PIC  X(0001).
000074     05  GEFFDTI PIC  X(0008).
000075*    -------------------------------
000076     05  GSTATUSL PIC S9(0004) COMP.
000077     05  GSTATUSF PIC  X(0001).
000078     05  FILLER REDEFINES GSTATUSF.
000079         10  GSTATUSA PIC  X(0001).
000080     05  GSTATUSI PIC  X(0007).
000081*    -------------------------------
000082     05  GADDBYL PIC S9(0004) COMP.
000083     05  GADDBYF PIC  X(0001).
000084     05  FILLER REDEFINES GADDBYF.
000085         10  GADDBYA PIC  X(0001).
000086     05  GADDBYI PIC  X(0004).
000087*    -------------------------------
000088     05  GADDDTL PIC S9(0004) COMP.
000089     05  GADDDTF PIC  X(0001).
000090     05  FILLER REDEFINES GADDDTF.
000091         10  GADDDTA PIC  X(0001).
000092     05  GADDDTI PIC  X(0008).
000093*    -------------------------------
000094     05  GLSTUSRL PIC S9(0004) COMP.
000095     05  GLSTUSRF PIC  X(0001).
000096     05  FILLER REDEFINES GLSTUSRF.
000097         10  GLSTUSRA PIC  X(0001).
000098     05  GLSTUSRI PIC  X(0004).
000099*    -------------------------------
000100     05  GLSTDTL PIC S9(0004) COMP.
000101     05  GLSTDTF PIC  X(0001).
000102     05  FILLER REDEFINES GLSTDTF.
000103         10  GLSTDTA PIC  X(0001).
000104     05  GLSTDTI PIC  X(0008).
000105*    -------------------------------
000106     05  GLSTIMEL PIC S9(0004) COMP.
000107     05  GLSTIMEF PIC  X(0001).
000108     05  FILLER REDEFINES GLSTIMEF.
000109         10  GLSTIMEA PIC  X(0001).
000110     05  GLSTIMEI PIC  999V99.
000111*    -------------------------------
000112     05  GCODE1L PIC S9(0004) COMP.
000113     05  GCODE1F PIC  X(0001).
000114     05  FILLER REDEFINES GCODE1F.
000115         10  GCODE1A PIC  X(0001).
000116     05  GCODE1I PIC  X(0002).
000117*    -------------------------------
000118     05  GCODE2L PIC S9(0004) COMP.
000119     05  GCODE2F PIC  X(0001).
000120     05  FILLER REDEFINES GCODE2F.
000121         10  GCODE2A PIC  X(0001).
000122     05  GCODE2I PIC  X(0002).
000123*    -------------------------------
000124     05  GCODE3L PIC S9(0004) COMP.
000125     05  GCODE3F PIC  X(0001).
000126     05  FILLER REDEFINES GCODE3F.
000127         10  GCODE3A PIC  X(0001).
000128     05  GCODE3I PIC  X(0002).
000129*    -------------------------------
000130     05  GCODE4L PIC S9(0004) COMP.
000131     05  GCODE4F PIC  X(0001).
000132     05  FILLER REDEFINES GCODE4F.
000133         10  GCODE4A PIC  X(0001).
000134     05  GCODE4I PIC  X(0002).
000135*    -------------------------------
000136     05  GCODE5L PIC S9(0004) COMP.
000137     05  GCODE5F PIC  X(0001).
000138     05  FILLER REDEFINES GCODE5F.
000139         10  GCODE5A PIC  X(0001).
000140     05  GCODE5I PIC  X(0002).
000141*    -------------------------------
000142     05  GLNAMEL PIC S9(0004) COMP.
000143     05  GLNAMEF PIC  X(0001).
000144     05  FILLER REDEFINES GLNAMEF.
000145         10  GLNAMEA PIC  X(0001).
000146     05  GLNAMEI PIC  X(0015).
000147*    -------------------------------
000148     05  GFNAMEL PIC S9(0004) COMP.
000149     05  GFNAMEF PIC  X(0001).
000150     05  FILLER REDEFINES GFNAMEF.
000151         10  GFNAMEA PIC  X(0001).
000152     05  GFNAMEI PIC  X(0010).
000153*    -------------------------------
000154     05  GINITL PIC S9(0004) COMP.
000155     05  GINITF PIC  X(0001).
000156     05  FILLER REDEFINES GINITF.
000157         10  GINITA PIC  X(0001).
000158     05  GINITI PIC  X(0001).
000159*    -------------------------------
000160     05  GADDR1L PIC S9(0004) COMP.
000161     05  GADDR1F PIC  X(0001).
000162     05  FILLER REDEFINES GADDR1F.
000163         10  GADDR1A PIC  X(0001).
000164     05  GADDR1I PIC  X(0030).
000165*    -------------------------------
000166     05  GADDR2L PIC S9(0004) COMP.
000167     05  GADDR2F PIC  X(0001).
000168     05  FILLER REDEFINES GADDR2F.
000169         10  GADDR2A PIC  X(0001).
000170     05  GADDR2I PIC  X(0030).
000171*    -------------------------------
000172     05  GCITYL PIC S9(0004) COMP.
000173     05  GCITYF PIC  X(0001).
000174     05  FILLER REDEFINES GCITYF.
000175         10  GCITYA PIC  X(0001).
000176     05  GCITYI PIC  X(0028).
000177*    -------------------------------
000178     05  GISTATEL PIC S9(0004) COMP.
000179     05  GISTATEF PIC  X(0001).
000180     05  FILLER REDEFINES GISTATEF.
000181         10  GISTATEA PIC  X(0001).
000182     05  GISTATEI PIC  X(0002).
000183*    -------------------------------
000184     05  GZIP1L PIC S9(0004) COMP.
000185     05  GZIP1F PIC  X(0001).
000186     05  FILLER REDEFINES GZIP1F.
000187         10  GZIP1A PIC  X(0001).
000188     05  GZIP1I PIC  X(0005).
000189*    -------------------------------
000190     05  GZIP2L PIC S9(0004) COMP.
000191     05  GZIP2F PIC  X(0001).
000192     05  FILLER REDEFINES GZIP2F.
000193         10  GZIP2A PIC  X(0001).
000194     05  GZIP2I PIC  X(0004).
000195*    -------------------------------
000196     05  GPHONEL PIC S9(0004) COMP.
000197     05  GPHONEF PIC  X(0001).
000198     05  FILLER REDEFINES GPHONEF.
000199         10  GPHONEA PIC  X(0001).
000200     05  GPHONEI PIC  X(0012).
000201*    -------------------------------
000202     05  GSSNOL PIC S9(0004) COMP.
000203     05  GSSNOF PIC  X(0001).
000204     05  FILLER REDEFINES GSSNOF.
000205         10  GSSNOA PIC  X(0001).
000206     05  GSSNOI PIC  X(0011).
000207*    -------------------------------
000208     05  GAGEL PIC S9(0004) COMP.
000209     05  GAGEF PIC  X(0001).
000210     05  FILLER REDEFINES GAGEF.
000211         10  GAGEA PIC  X(0001).
000212     05  GAGEI PIC  X(0002).
000213*    -------------------------------
000214     05  GBDTL PIC S9(0004) COMP.
000215     05  GBDTF PIC  X(0001).
000216     05  FILLER REDEFINES GBDTF.
000217         10  GBDTA PIC  X(0001).
000218     05  GBDTI PIC  X(0008).
000219*    -------------------------------
000220     05  GSEXL PIC S9(0004) COMP.
000221     05  GSEXF PIC  X(0001).
000222     05  FILLER REDEFINES GSEXF.
000223         10  GSEXA PIC  X(0001).
000224     05  GSEXI PIC  X(0001).
000225*    -------------------------------
000226     05  GJDOBL PIC S9(0004) COMP.
000227     05  GJDOBF PIC  X(0001).
000228     05  FILLER REDEFINES GJDOBF.
000229         10  GJDOBA PIC  X(0001).
000230     05  GJDOBI PIC  X(0008).
000231*    -------------------------------
000232     05  GBNAMEL PIC S9(0004) COMP.
000233     05  GBNAMEF PIC  X(0001).
000234     05  FILLER REDEFINES GBNAMEF.
000235         10  GBNAMEA PIC  X(0001).
000236     05  GBNAMEI PIC  X(0025).
000237*    -------------------------------
000238     05  GBADD1L PIC S9(0004) COMP.
000239     05  GBADD1F PIC  X(0001).
000240     05  FILLER REDEFINES GBADD1F.
000241         10  GBADD1A PIC  X(0001).
000242     05  GBADD1I PIC  X(0030).
000243*    -------------------------------
000244     05  GBADD2L PIC S9(0004) COMP.
000245     05  GBADD2F PIC  X(0001).
000246     05  FILLER REDEFINES GBADD2F.
000247         10  GBADD2A PIC  X(0001).
000248     05  GBADD2I PIC  X(0030).
000249*    -------------------------------
000250     05  GBCITYL PIC S9(0004) COMP.
000251     05  GBCITYF PIC  X(0001).
000252     05  FILLER REDEFINES GBCITYF.
000253         10  GBCITYA PIC  X(0001).
000254     05  GBCITYI PIC  X(0028).
000255*    -------------------------------
000256     05  GBSTATEL PIC S9(0004) COMP.
000257     05  GBSTATEF PIC  X(0001).
000258     05  FILLER REDEFINES GBSTATEF.
000259         10  GBSTATEA PIC  X(0001).
000260     05  GBSTATEI PIC  X(0002).
000261*    -------------------------------
000262     05  GBZIP1L PIC S9(0004) COMP.
000263     05  GBZIP1F PIC  X(0001).
000264     05  FILLER REDEFINES GBZIP1F.
000265         10  GBZIP1A PIC  X(0001).
000266     05  GBZIP1I PIC  X(0005).
000267*    -------------------------------
000268     05  GBZIP2L PIC S9(0004) COMP.
000269     05  GBZIP2F PIC  X(0001).
000270     05  FILLER REDEFINES GBZIP2F.
000271         10  GBZIP2A PIC  X(0001).
000272     05  GBZIP2I PIC  X(0004).
000273*    -------------------------------
000274     05  GTYPE1L PIC S9(0004) COMP.
000275     05  GTYPE1F PIC  X(0001).
000276     05  FILLER REDEFINES GTYPE1F.
000277         10  GTYPE1A PIC  X(0001).
000278     05  GTYPE1I PIC  X(0003).
000279*    -------------------------------
000280     05  GSTAT1L PIC S9(0004) COMP.
000281     05  GSTAT1F PIC  X(0001).
000282     05  FILLER REDEFINES GSTAT1F.
000283         10  GSTAT1A PIC  X(0001).
000284     05  GSTAT1I PIC  X(0014).
000285*    -------------------------------
000286     05  GTYPE2L PIC S9(0004) COMP.
000287     05  GTYPE2F PIC  X(0001).
000288     05  FILLER REDEFINES GTYPE2F.
000289         10  GTYPE2A PIC  X(0001).
000290     05  GTYPE2I PIC  X(0003).
000291*    -------------------------------
000292     05  GSTAT2L PIC S9(0004) COMP.
000293     05  GSTAT2F PIC  X(0001).
000294     05  FILLER REDEFINES GSTAT2F.
000295         10  GSTAT2A PIC  X(0001).
000296     05  GSTAT2I PIC  X(0014).
000297*    -------------------------------
000298     05  GTYPE3L PIC S9(0004) COMP.
000299     05  GTYPE3F PIC  X(0001).
000300     05  FILLER REDEFINES GTYPE3F.
000301         10  GTYPE3A PIC  X(0001).
000302     05  GTYPE3I PIC  X(0003).
000303*    -------------------------------
000304     05  GSTAT3L PIC S9(0004) COMP.
000305     05  GSTAT3F PIC  X(0001).
000306     05  FILLER REDEFINES GSTAT3F.
000307         10  GSTAT3A PIC  X(0001).
000308     05  GSTAT3I PIC  X(0014).
000309*    -------------------------------
000310     05  GTYPE4L PIC S9(0004) COMP.
000311     05  GTYPE4F PIC  X(0001).
000312     05  FILLER REDEFINES GTYPE4F.
000313         10  GTYPE4A PIC  X(0001).
000314     05  GTYPE4I PIC  X(0003).
000315*    -------------------------------
000316     05  GSTAT4L PIC S9(0004) COMP.
000317     05  GSTAT4F PIC  X(0001).
000318     05  FILLER REDEFINES GSTAT4F.
000319         10  GSTAT4A PIC  X(0001).
000320     05  GSTAT4I PIC  X(0014).
000321*    -------------------------------
000322     05  GTYPE5L PIC S9(0004) COMP.
000323     05  GTYPE5F PIC  X(0001).
000324     05  FILLER REDEFINES GTYPE5F.
000325         10  GTYPE5A PIC  X(0001).
000326     05  GTYPE5I PIC  X(0003).
000327*    -------------------------------
000328     05  GSTAT5L PIC S9(0004) COMP.
000329     05  GSTAT5F PIC  X(0001).
000330     05  FILLER REDEFINES GSTAT5F.
000331         10  GSTAT5A PIC  X(0001).
000332     05  GSTAT5I PIC  X(0014).
000333*    -------------------------------
000334     05  GTYPE6L PIC S9(0004) COMP.
000335     05  GTYPE6F PIC  X(0001).
000336     05  FILLER REDEFINES GTYPE6F.
000337         10  GTYPE6A PIC  X(0001).
000338     05  GTYPE6I PIC  X(0003).
000339*    -------------------------------
000340     05  GSTAT6L PIC S9(0004) COMP.
000341     05  GSTAT6F PIC  X(0001).
000342     05  FILLER REDEFINES GSTAT6F.
000343         10  GSTAT6A PIC  X(0001).
000344     05  GSTAT6I PIC  X(0014).
000345*    -------------------------------
000346     05  GTYPE7L PIC S9(0004) COMP.
000347     05  GTYPE7F PIC  X(0001).
000348     05  FILLER REDEFINES GTYPE7F.
000349         10  GTYPE7A PIC  X(0001).
000350     05  GTYPE7I PIC  X(0003).
000351*    -------------------------------
000352     05  GSTAT7L PIC S9(0004) COMP.
000353     05  GSTAT7F PIC  X(0001).
000354     05  FILLER REDEFINES GSTAT7F.
000355         10  GSTAT7A PIC  X(0001).
000356     05  GSTAT7I PIC  X(0014).
000357*    -------------------------------
000358     05  GERMSG1L PIC S9(0004) COMP.
000359     05  GERMSG1F PIC  X(0001).
000360     05  FILLER REDEFINES GERMSG1F.
000361         10  GERMSG1A PIC  X(0001).
000362     05  GERMSG1I PIC  X(0079).
000363*    -------------------------------
000364     05  GERMSG2L PIC S9(0004) COMP.
000365     05  GERMSG2F PIC  X(0001).
000366     05  FILLER REDEFINES GERMSG2F.
000367         10  GERMSG2A PIC  X(0001).
000368     05  GERMSG2I PIC  X(0079).
000369*    -------------------------------
000370     05  GPFKEYL PIC S9(0004) COMP.
000371     05  GPFKEYF PIC  X(0001).
000372     05  FILLER REDEFINES GPFKEYF.
000373         10  GPFKEYA PIC  X(0001).
000374     05  GPFKEYI PIC  99.
000375 01  EL127GO REDEFINES EL127GI.
000376     05  FILLER            PIC  X(0012).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  GDATEO PIC  X(0008).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  GTIMEO PIC  99.99.
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  CMPNYIDO PIC  X(0003).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  USERIDO PIC  X(0004).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  GMAINTO PIC  X(0001).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  GCERTNOO PIC  X(0010).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  GCRTSFXO PIC  X(0001).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  GACCTNOO PIC  X(0010).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  GSTATEO PIC  X(0002).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  GCARIERO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  GGROUPO PIC  X(0006).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  GEFFDTO PIC  X(0008).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  GSTATUSO PIC  X(0007).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  GADDBYO PIC  X(0004).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  GADDDTO PIC  X(0008).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  GLSTUSRO PIC  X(0004).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  GLSTDTO PIC  X(0008).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  GLSTIMEO PIC  99.99.
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  GCODE1O PIC  X(0002).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  GCODE2O PIC  X(0002).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  GCODE3O PIC  X(0002).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  GCODE4O PIC  X(0002).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  GCODE5O PIC  X(0002).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  GLNAMEO PIC  X(0015).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  GFNAMEO PIC  X(0010).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  GINITO PIC  X(0001).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  GADDR1O PIC  X(0030).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  GADDR2O PIC  X(0030).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  GCITYO PIC  X(0028).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  GISTATEO PIC  X(0002).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  GZIP1O PIC  X(0005).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  GZIP2O PIC  X(0004).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  GPHONEO PIC  X(0012).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  GSSNOO PIC  X(0011).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  GAGEO PIC  X(0002).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  GBDTO PIC  X(0008).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  GSEXO PIC  X(0001).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  GJDOBO PIC  X(0008).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  GBNAMEO PIC  X(0025).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  GBADD1O PIC  X(0030).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  GBADD2O PIC  X(0030).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  GBCITYO PIC  X(0028).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  GBSTATEO PIC  X(0002).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  GBZIP1O PIC  X(0005).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  GBZIP2O PIC  X(0004).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  GTYPE1O PIC  X(0003).
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  GSTAT1O PIC  X(0014).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  GTYPE2O PIC  X(0003).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  GSTAT2O PIC  X(0014).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  GTYPE3O PIC  X(0003).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  GSTAT3O PIC  X(0014).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  GTYPE4O PIC  X(0003).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  GSTAT4O PIC  X(0014).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  GTYPE5O PIC  X(0003).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  GSTAT5O PIC  X(0014).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  GTYPE6O PIC  X(0003).
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  GSTAT6O PIC  X(0014).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  GTYPE7O PIC  X(0003).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  GSTAT7O PIC  X(0014).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  GERMSG1O PIC  X(0079).
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  GERMSG2O PIC  X(0079).
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  GPFKEYO PIC  99.
000563*    -------------------------------
      *<<((file: EL1277S))
000209
000210     EJECT
000211
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
000213 01  DFHCOMMAREA                     PIC X(1024).
000214
000215*                                COPY ELCCNTL.
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
000216
000217*    COPY ERCMAIL.
      *>>((file: ERCMAIL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCMAIL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
000014*   ALTERNATE PATH    = NOT USED                                 *
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
000026* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
000027* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000028* 111108                   PEMA  ADD CRED BENE ADDR2
000029******************************************************************
000030
000031 01  MAILING-DATA.
000032     12  MA-RECORD-ID                      PIC XX.
000033         88  VALID-MA-ID                       VALUE 'MA'.
000034
000035     12  MA-CONTROL-PRIMARY.
000036         16  MA-COMPANY-CD                 PIC X.
000037         16  MA-CARRIER                    PIC X.
000038         16  MA-GROUPING.
000039             20  MA-GROUPING-PREFIX        PIC XXX.
000040             20  MA-GROUPING-PRIME         PIC XXX.
000041         16  MA-STATE                      PIC XX.
000042         16  MA-ACCOUNT.
000043             20  MA-ACCOUNT-PREFIX         PIC X(4).
000044             20  MA-ACCOUNT-PRIME          PIC X(6).
000045         16  MA-CERT-EFF-DT                PIC XX.
000046         16  MA-CERT-NO.
000047             20  MA-CERT-PRIME             PIC X(10).
000048             20  MA-CERT-SFX               PIC X.
000049
000050     12  FILLER                            PIC XX.
000051
000052     12  MA-ACCESS-CONTROL.
000053         16  MA-SOURCE-SYSTEM              PIC XX.
000054             88  MA-FROM-CREDIT                VALUE 'CR'.
000055             88  MA-FROM-VSI                   VALUE 'VS'.
000056             88  MA-FROM-WARRANTY              VALUE 'WA'.
000057             88  MA-FROM-OTHER                 VALUE 'OT'.
000058         16  MA-RECORD-ADD-DT              PIC XX.
000059         16  MA-RECORD-ADDED-BY            PIC XXXX.
000060         16  MA-LAST-MAINT-DT              PIC XX.
000061         16  MA-LAST-MAINT-BY              PIC XXXX.
000062         16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000063
000064     12  MA-PROFILE-INFO.
000065         16  MA-QUALIFY-CODE-1             PIC XX.
000066         16  MA-QUALIFY-CODE-2             PIC XX.
000067         16  MA-QUALIFY-CODE-3             PIC XX.
000068         16  MA-QUALIFY-CODE-4             PIC XX.
000069         16  MA-QUALIFY-CODE-5             PIC XX.
000070
000071         16  MA-INSURED-LAST-NAME          PIC X(15).
000072         16  MA-INSURED-FIRST-NAME         PIC X(10).
000073         16  MA-INSURED-MIDDLE-INIT        PIC X.
000074         16  MA-INSURED-ISSUE-AGE          PIC 99.
000075         16  MA-INSURED-BIRTH-DT           PIC XX.
000076         16  MA-INSURED-SEX                PIC X.
000077             88  MA-SEX-MALE                   VALUE 'M'.
000078             88  MA-SEX-FEMALE                 VALUE 'F'.
000079         16  MA-INSURED-SOC-SEC-NO         PIC X(11).
000080
000081         16  MA-ADDRESS-CORRECTED          PIC X.
000082         16  MA-JOINT-BIRTH-DT             PIC XX.
000083*        16  FILLER                        PIC X(12).
000084
000085         16  MA-ADDRESS-LINE-1             PIC X(30).
000086         16  MA-ADDRESS-LINE-2             PIC X(30).
000087         16  MA-CITY-STATE.
000088             20  MA-CITY                   PIC X(28).
000089             20  MA-ADDR-STATE             PIC XX.
000090         16  MA-ZIP.
000091             20  MA-ZIP-CODE.
000092                 24  MA-ZIP-CODE-1ST       PIC X(1).
000093                     88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000094                 24  FILLER                PIC X(4).
000095             20  MA-ZIP-PLUS4              PIC X(4).
000096         16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
000097             20  MA-CAN-POSTAL-CODE-1      PIC X(3).
000098             20  MA-CAN-POSTAL-CODE-2      PIC X(3).
000099             20  FILLER                    PIC X(3).
000100
000101         16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
000102
000103         16  FILLER                        PIC XXX.
000104*        16  FILLER                        PIC X(10).
000105
000106
000107     12  MA-CRED-BENE-INFO.
000108         16  MA-CRED-BENE-NAME                 PIC X(25).
000109         16  MA-CRED-BENE-ADDR                 PIC X(30).
000110         16  MA-CRED-BENE-ADDR2                PIC X(30).
000111         16  MA-CRED-BENE-CTYST.
000112             20  MA-CRED-BENE-CITY             PIC X(28).
000113             20  MA-CRED-BENE-STATE            PIC XX.
000114         16  MA-CRED-BENE-ZIP.
000115             20  MA-CB-ZIP-CODE.
000116                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
000117                     88  MA-CB-CANADIAN-POST-CODE
000118                                           VALUE 'A' THRU 'Z'.
000119                 24  FILLER                    PIC X(4).
000120             20  MA-CB-ZIP-PLUS4               PIC X(4).
000121         16  MA-CB-CANADIAN-POSTAL-CODE
000122                            REDEFINES MA-CRED-BENE-ZIP.
000123             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
000124             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
000125             20  FILLER                        PIC X(3).
000126     12  MA-POST-CARD-MAIL-DATA.
000127         16  MA-MAIL-DATA OCCURS 7.
000128             20  MA-MAIL-TYPE              PIC X.
000129                 88  MA-12MO-MAILING           VALUE '1'.
000130                 88  MA-EXP-MAILING            VALUE '2'.
000131             20  MA-MAIL-STATUS            PIC X.
000132                 88  MA-MAIL-ST-MAILED         VALUE '1'.
000133                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
000134                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
000135             20  MA-MAIL-DATE              PIC XX.
000136     12  FILLER                            PIC XX.
000137     12  FILLER                            PIC XX.
000138*    12  FILLER                            PIC X(30).
000139******************************************************************
      *<<((file: ERCMAIL))
000218
000219     EJECT
000220
000221*    COPY ELCCERT.
      *>>((file: ELCCERT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCERT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.013                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CERTIFICATE MASTER                        *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 450  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
000013*       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
000014*       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
000015*       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
000016*       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
000017*                                                                *
000018*   LOG = YES                                                    *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000029* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000030* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
000031* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000032* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000033* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
000034* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000035* 032612  CR2011110200001  PEMA  AHL CHANGES
000036* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
000037* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000038* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000039******************************************************************
000040
000041 01  CERTIFICATE-MASTER.
000042     12  CM-RECORD-ID                      PIC XX.
000043         88  VALID-CM-ID                      VALUE 'CM'.
000044
000045     12  CM-CONTROL-PRIMARY.
000046         16  CM-COMPANY-CD                 PIC X.
000047         16  CM-CARRIER                    PIC X.
000048         16  CM-GROUPING.
000049             20  CM-GROUPING-PREFIX        PIC X(3).
000050             20  CM-GROUPING-PRIME         PIC X(3).
000051         16  CM-STATE                      PIC XX.
000052         16  CM-ACCOUNT.
000053             20  CM-ACCOUNT-PREFIX         PIC X(4).
000054             20  CM-ACCOUNT-PRIME          PIC X(6).
000055         16  CM-CERT-EFF-DT                PIC XX.
000056         16  CM-CERT-NO.
000057             20  CM-CERT-PRIME             PIC X(10).
000058             20  CM-CERT-SFX               PIC X.
000059
000060     12  CM-CONTROL-BY-NAME.
000061         16  CM-COMPANY-CD-A1              PIC X.
000062         16  CM-INSURED-LAST-NAME          PIC X(15).
000063         16  CM-INSURED-INITIALS.
000064             20  CM-INSURED-INITIAL1       PIC X.
000065             20  CM-INSURED-INITIAL2       PIC X.
000066
000067     12  CM-CONTROL-BY-SSN.
000068         16  CM-COMPANY-CD-A2              PIC X.
000069         16  CM-SOC-SEC-NO.
000070             20  CM-SSN-STATE              PIC XX.
000071             20  CM-SSN-ACCOUNT            PIC X(6).
000072             20  CM-SSN-LN3.
000073                 25  CM-INSURED-INITIALS-A2.
000074                     30 CM-INSURED-INITIAL1-A2   PIC X.
000075                     30 CM-INSURED-INITIAL2-A2   PIC X.
000076                 25 CM-PART-LAST-NAME-A2         PIC X.
000077
000078     12  CM-CONTROL-BY-CERT-NO.
000079         16  CM-COMPANY-CD-A4              PIC X.
000080         16  CM-CERT-NO-A4                 PIC X(11).
000081
000082     12  CM-CONTROL-BY-MEMB.
000083         16  CM-COMPANY-CD-A5              PIC X.
000084         16  CM-MEMBER-NO.
000085             20  CM-MEMB-STATE             PIC XX.
000086             20  CM-MEMB-ACCOUNT           PIC X(6).
000087             20  CM-MEMB-LN4.
000088                 25  CM-INSURED-INITIALS-A5.
000089                     30 CM-INSURED-INITIAL1-A5   PIC X.
000090                     30 CM-INSURED-INITIAL2-A5   PIC X.
000091                 25 CM-PART-LAST-NAME-A5         PIC XX.
000092
000093     12  CM-INSURED-PROFILE-DATA.
000094         16  CM-INSURED-FIRST-NAME.
000095             20  CM-INSURED-1ST-INIT       PIC X.
000096             20  FILLER                    PIC X(9).
000097         16  CM-INSURED-ISSUE-AGE          PIC 99.
000098         16  CM-INSURED-SEX                PIC X.
000099             88  CM-SEX-MALE                  VALUE 'M'.
000100             88  CM-SEX-FEMAL                 VALUE 'F'.
000101         16  CM-INSURED-JOINT-AGE          PIC 99.
000102         16  CM-JOINT-INSURED-NAME.
000103             20  CM-JT-LAST-NAME           PIC X(15).
000104             20  CM-JT-FIRST-NAME.
000105                 24  CM-JT-1ST-INIT        PIC X.
000106                 24  FILLER                PIC X(9).
000107             20  CM-JT-INITIAL             PIC X.
000108
000109     12  CM-LIFE-DATA.
000110         16  CM-LF-BENEFIT-CD              PIC XX.
000111         16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
000112         16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
000113         16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
000114         16  CM-LF-DEV-CODE                PIC XXX.
000115         16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
000116         16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
000117         16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000118         16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
000119         16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000120         16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000121         16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
000122         16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000123         16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
000124         16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000125         16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
000126         16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
000127         16  cm-temp-epiq                  pic xx.
000128             88  EPIQ-CLASS                  value 'EQ'.
000129*        16  FILLER                        PIC XX.
000130
000131     12  CM-AH-DATA.
000132         16  CM-AH-BENEFIT-CD              PIC XX.
000133         16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
000134         16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
000135         16  CM-AH-DEV-CODE                PIC XXX.
000136         16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
000137         16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
000138         16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000139         16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000140         16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000141         16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
000142         16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
000143         16  CM-AH-PAID-THRU-DT            PIC XX.
000144             88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
000145         16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000146         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
000147         16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
000148         16  FILLER                        PIC X.
000149
000150     12  CM-LOAN-INFORMATION.
000151         16  CM-LIVES                      PIC S9(7)     COMP-3.
000152         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
000153                                           PIC S9(5)V99  COMP-3.
000154         16  CM-BILLED                     PIC S9(7)     COMP-3.
000155         16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
000156         16  CM-PAY-FREQUENCY              PIC S99.
000157         16  CM-LOAN-TERM                  PIC S999      COMP-3.
000158         16  CM-RATE-CLASS                 PIC XX.
000159         16  CM-BENEFICIARY                PIC X(25).
000160         16  CM-POLICY-FORM-NO             PIC X(12).
000161         16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
000162         16  CM-LAST-ADD-ON-DT             PIC XX.
000163         16  CM-DEDUCTIBLE-AMOUNTS.
000164             20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
000165             20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
000166         16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000167             20  CM-RESIDENT-STATE         PIC XX.
000168             20  CM-RATE-CODE              PIC X(4).
000169             20  FILLER                    PIC XX.
000170         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000171             20  CM-LOAN-OFFICER           PIC X(5).
000172             20  FILLER                    PIC XXX.
000173         16  CM-CSR-CODE                   PIC XXX.
000174         16  CM-UNDERWRITING-CODE          PIC X.
000175             88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
000176         16  CM-POST-CARD-IND              PIC X.
000177         16  CM-REF-INTERFACE-SW           PIC X.
000178         16  CM-PREMIUM-TYPE               PIC X.
000179             88  CM-SING-PRM                  VALUE '1'.
000180             88  CM-O-B-COVERAGE              VALUE '2'.
000181             88  CM-OPEN-END                  VALUE '3'.
000182         16  CM-IND-GRP-TYPE               PIC X.
000183             88  CM-INDIVIDUAL                VALUE 'I'.
000184             88  CM-GROUP                     VALUE 'G'.
000185         16  CM-SKIP-CODE                  PIC X.
000186             88  NO-MONTHS-SKIPPED            VALUE SPACE.
000187             88  SKIP-JULY                    VALUE '1'.
000188             88  SKIP-AUGUST                  VALUE '2'.
000189             88  SKIP-SEPTEMBER               VALUE '3'.
000190             88  SKIP-JULY-AUG                VALUE '4'.
000191             88  SKIP-AUG-SEPT                VALUE '5'.
000192             88  SKIP-JULY-AUG-SEPT           VALUE '6'.
000193             88  SKIP-JUNE-JULY-AUG           VALUE '7'.
000194             88  SKIP-JUNE                    VALUE '8'.
000195             88  SKIP-JUNE-JULY               VALUE '9'.
000196             88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
000197             88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
000198         16  CM-PAYMENT-MODE               PIC X.
000199             88  PAY-MONTHLY                  VALUE SPACE.
000200             88  PAY-WEEKLY                   VALUE '1'.
000201             88  PAY-SEMI-MONTHLY             VALUE '2'.
000202             88  PAY-BI-WEEKLY                VALUE '3'.
000203             88  PAY-SEMI-ANUALLY             VALUE '4'.
000204         16  CM-LOAN-NUMBER                PIC X(8).
000205         16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
000206         16  CM-OLD-LOF                    PIC XXX.
000207*        16  CM-LOAN-OFFICER               PIC XXX.
000208         16  CM-REIN-TABLE                 PIC XXX.
000209         16  CM-SPECIAL-REIN-CODE          PIC X.
000210         16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
000211         16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
000212         16  CM-LOAN-1ST-PMT-DT            PIC XX.
000213
000214     12  CM-STATUS-DATA.
000215         16  CM-ENTRY-STATUS               PIC X.
000216         16  CM-ENTRY-DT                   PIC XX.
000217
000218         16  CM-LF-STATUS-AT-CANCEL        PIC X.
000219         16  CM-LF-CANCEL-DT               PIC XX.
000220         16  CM-LF-CANCEL-EXIT-DT          PIC XX.
000221
000222         16  CM-LF-STATUS-AT-DEATH         PIC X.
000223         16  CM-LF-DEATH-DT                PIC XX.
000224         16  CM-LF-DEATH-EXIT-DT           PIC XX.
000225
000226         16  CM-LF-CURRENT-STATUS          PIC X.
000227             88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000228                                                'M' '4' '5' '9'.
000229             88  CM-LF-NORMAL-ENTRY           VALUE '1'.
000230             88  CM-LF-POLICY-PENDING         VALUE '2'.
000231             88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
000232             88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
000233             88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
000234             88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
000235             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000236             88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
000237             88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000238             88  CM-LF-CANCEL-APPLIED         VALUE '8'.
000239             88  CM-LF-IS-REIN-ONLY           VALUE '9'.
000240             88  CM-LF-DECLINED               VALUE 'D'.
000241             88  CM-LF-VOIDED                 VALUE 'V'.
000242
000243         16  CM-AH-STATUS-AT-CANCEL        PIC X.
000244         16  CM-AH-CANCEL-DT               PIC XX.
000245         16  CM-AH-CANCEL-EXIT-DT          PIC XX.
000246
000247         16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
000248         16  CM-AH-SETTLEMENT-DT           PIC XX.
000249         16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
000250
000251         16  CM-AH-CURRENT-STATUS          PIC X.
000252             88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000253                                                'M' '4' '5' '9'.
000254             88  CM-AH-NORMAL-ENTRY           VALUE '1'.
000255             88  CM-AH-POLICY-PENDING         VALUE '2'.
000256             88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
000257             88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
000258             88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
000259             88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
000260             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000261             88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
000262             88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000263             88  CM-AH-CANCEL-APPLIED         VALUE '8'.
000264             88  CM-AH-IS-REIN-ONLY           VALUE '9'.
000265             88  CM-AH-DECLINED               VALUE 'D'.
000266             88  CM-AH-VOIDED                 VALUE 'V'.
000267
000268         16  CM-CLAIM-INTERFACE-SW         PIC X.
000269             88  NO-CLAIM-ATTACHED            VALUE SPACE.
000270             88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
000271             88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
000272         16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
000273
000274         16  CM-ENTRY-BATCH                PIC X(6).
000275         16  CM-LF-EXIT-BATCH              PIC X(6).
000276         16  CM-AH-EXIT-BATCH              PIC X(6).
000277         16  CM-LAST-MONTH-END             PIC XX.
000278
000279     12  CM-NOTE-SW                        PIC X.
000280         88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
000281         88  CERT-NOTES-PRESENT               VALUE '1'.
000282         88  BILLING-NOTES-PRESENT            VALUE '2'.
000283         88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
000284         88  CLAIM-NOTES-PRESENT              VALUE '4'.
000285         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
000286         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
000287         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
000288     12  CM-COMP-EXCP-SW                   PIC X.
000289         88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
000290         88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
000291     12  CM-INSURED-ADDRESS-SW             PIC X.
000292         88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
000293         88  INSURED-ADDR-PRESENT             VALUE '1'.
000294
000295*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
000296     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
000297     12  FILLER                            PIC X.
000298
000299*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
000300     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
000301     12  FILLER                            PIC X.
000302*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
000303     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
000304
000305     12  CM-CREDIT-INTERFACE-SW-1          PIC X.
000306         88  CERT-ADDED-BATCH                 VALUE ' '.
000307         88  CERT-ADDED-ONLINE                VALUE '1'.
000308         88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
000309         88  CERT-PURGED-OFFLINE              VALUE '3'.
000310         88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
000311     12  CM-CREDIT-INTERFACE-SW-2          PIC X.
000312         88  CERT-AS-LOADED                   VALUE ' '.
000313         88  CERT-CANCELLED-ONLINE            VALUE '1'.
000314         88  CERT-CLAIM-ONLINE                VALUE '2'.
000315         88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
000316         88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
000317         88  CERT-PEND-CANCEL-VOID            VALUE '5'.
000318         88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
000319         88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
000320
000321     12  CM-ACCOUNT-COMM-PCTS.
000322         16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
000323         16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
000324
000325     12  CM-USER-FIELD                     PIC X.
000326     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
000327     12  CM-CLP-STATE                      PIC XX.
000328     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
000329     12  CM-USER-RESERVED                  PIC XXX.
000330     12  FILLER REDEFINES CM-USER-RESERVED.
000331         16  CM-AH-CLASS-CD                PIC XX.
000332         16  F                             PIC X.
000333******************************************************************
      *<<((file: ELCCERT))
000222
000223     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1277' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000224 VCOBOL-DUMMY-PROCEDURE.
000225
000226     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000227     MOVE '5'                    TO  DC-OPTION-CODE.
000228     PERFORM 9700-DATE-LINK.
000229     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000230     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000231
000232     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000233     MOVE 1                      TO  EMI-NUMBER-OF-LINES.
000234     MOVE EIBTRMID               TO  QID-TERM.
000235
000236     IF EIBCALEN = 0
000237         GO TO 8800-UNAUTHORIZED-ACCESS.
000238
000239     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000240         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000241             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
000242             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
000243             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
000244             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
000245             MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
000246             MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
000247             MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
000248             MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
000249         ELSE
000250             MOVE PI-CALLING-PROGRAM   TO  RETURN-FROM
000251             MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
000252             MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
000253             MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
000254             MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
000255             MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
000256             MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
000257             MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
000258             MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
000259
000260     IF EIBAID = DFHCLEAR
000261         GO TO 9400-CLEAR.
000262
000263     IF PI-PROCESSOR-ID = 'LGXX'
000264         NEXT SENTENCE
000265     ELSE
000266         
      * EXEC CICS READQ TS
000267*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000268*            INTO    (SECURITY-CONTROL)
000269*            LENGTH  (SC-COMM-LENGTH)
000270*            ITEM    (SC-ITEM)
000271*        END-EXEC
      *    MOVE '*$II   L              ''   #00003667' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033363637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000272         MOVE SC-CREDIT-DISPLAY (33)   TO  PI-DISPLAY-CAP
000273         MOVE SC-CREDIT-UPDATE  (33)   TO  PI-MODIFY-CAP
000274         IF NOT DISPLAY-CAP
000275             MOVE 'READ'               TO  SM-READ
000276             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000277             MOVE ER-0070              TO  EMI-ERROR
000278             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000279             GO TO 8100-SEND-INITIAL-MAP.
000280
000281     IF  EIBTRNID NOT = TRANS-ID
000282         MOVE LOW-VALUES         TO  EL127GI
000283         PERFORM 7000-FORMAT-SCREEN
000284         GO TO 8100-SEND-INITIAL-MAP.
000285
000286     IF NOT MODIFY-CAP
000287         MOVE 'UPDATE'            TO  SM-READ
000288         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000289         MOVE ER-0070             TO  EMI-ERROR
000290         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000291         GO TO 8100-SEND-INITIAL-MAP.
000292
000293     
      * EXEC CICS    HANDLE    CONDITION
000294*         PGMIDERR          (9600-PGMID-ERROR)
000295*         ERROR             (9990-ABEND)
000296*         END-EXEC.
      *    MOVE '"$L.                  ! " #00003694' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303033363934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000297
000298     EJECT
000299
000300 0200-RECEIVE.
000301     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000302         MOVE ER-0008            TO  EMI-ERROR
000303         PERFORM 9900-ERROR-FORMAT
000304         MOVE -1                 TO  GMAINTL
000305         GO TO 8200-SEND-DATAONLY.
000306
000307     
      * EXEC CICS RECEIVE
000308*        MAP      (MAP-NAME)
000309*        MAPSET   (MAPSET-NAME)
000310*        INTO     (EL127GI)
000311*    END-EXEC.
           MOVE LENGTH OF
            EL127GI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003708' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033373038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127GI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000312
000313     IF  GPFKEYL = 0
000314         GO TO 0300-CHECK-PFKEYS.
000315
000316     IF  EIBAID NOT = DFHENTER
000317         MOVE ER-0004            TO  EMI-ERROR
000318         GO TO 0320-INPUT-ERROR.
000319
000320     IF  GPFKEYI NUMERIC
000321         IF  GPFKEYI = '12' OR '23' OR '24'
000322             MOVE PF-VALUES (GPFKEYI) TO  EIBAID
000323         ELSE
000324             MOVE ER-0029             TO  EMI-ERROR
000325             GO TO 0320-INPUT-ERROR.
000326
000327 0300-CHECK-PFKEYS.
000328     IF EIBAID = DFHPF23
000329         GO TO 8810-PF23.
000330
000331     IF EIBAID = DFHPF24
000332         GO TO 9200-RETURN-MAIN-MENU.
000333
000334     IF EIBAID = DFHPF12
000335         GO TO 9500-PF12.
000336
000337     IF EIBAID = DFHENTER or dfhpf7
000338         GO TO 400-EDIT-INPUT-DATA.
000339
000340     MOVE ER-0029                TO  EMI-ERROR.
000341
000342 0320-INPUT-ERROR.
000343     PERFORM 9900-ERROR-FORMAT.
000344     MOVE AL-UNBON               TO  GPFKEYA.
000345     IF GPFKEYL = 0
000346         MOVE -1                 TO  GMAINTL
000347     ELSE
000348         MOVE -1                 TO  GPFKEYL.
000349     GO TO 8200-SEND-DATAONLY.
000350
000351     EJECT
000352
000353 400-EDIT-INPUT-DATA.
000354     IF GMAINTI = 'A' OR 'C' OR 'S'
000355         NEXT SENTENCE
000356     ELSE
000357         MOVE ER-0023            TO  EMI-ERROR
000358         MOVE -1                 TO  GMAINTL
000359         MOVE AL-UABON           TO  GMAINTA
000360         PERFORM 9900-ERROR-FORMAT
000361         GO TO 8200-SEND-DATAONLY.
000362
000363 410-CHECK-ERRORS.
000364     IF EMI-ERROR = ZEROS
000365         NEXT SENTENCE
000366     ELSE
000367         GO TO 8200-SEND-DATAONLY.
000368
000369     IF GMAINTI = 'A'
000370        GO TO 1000-ADD-RECORD.
000371
000372     IF GMAINTI = 'C'
000373        GO TO 2000-CHANGE-RECORD.
000374
000375     IF GMAINTI = 'S'
000376        PERFORM 7000-FORMAT-SCREEN
000377        GO TO 8200-SEND-DATAONLY.
000378
000379     EJECT
000380
000381 1000-ADD-RECORD       SECTION.
000382     PERFORM 3000-EDIT-MAIL-DATA.
000383
000384     IF NOT EMI-NO-ERRORS
000385         GO TO 8200-SEND-DATAONLY.
000386
000387     
      * EXEC CICS GETMAIN
000388*        SET      (ADDRESS OF MAILING-DATA)
000389*        LENGTH   (WS-ERMAIL-RECORD-LENGTH)
000390*        INITIMG  (GETMAIN-SPACE)
000391*    END-EXEC.
      *    MOVE ',"IL                  $   #00003788' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033373838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ERMAIL-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000392
000393     MOVE  SPACES                TO  MAILING-DATA.
000394
000395     MOVE  'MA'                  TO  MA-RECORD-ID.
000396
000397     MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
000398     MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
000399     MOVE  PI-STATE              TO  WS-CM-STATE.
000400     MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
000401     MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
000402     MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
000403     MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
000404     MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
000405
000406     MOVE  WS-CM-CONTROL-PRIMARY TO  MA-CONTROL-PRIMARY.
000407
000408     MOVE PI-PROCESSOR-ID        TO  MA-RECORD-ADDED-BY
000409                                     MA-LAST-MAINT-BY.
000410     MOVE EIBTIME                TO  MA-LAST-MAINT-HHMMSS.
000411     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000412     MOVE '5'                    TO  DC-OPTION-CODE.
000413     PERFORM 9700-DATE-LINK.
000414     IF DATE-CONVERSION-ERROR
000415         MOVE LOW-VALUES         TO  MA-RECORD-ADD-DT
000416                                     MA-LAST-MAINT-DT
000417     ELSE
000418         MOVE DC-BIN-DATE-1      TO  MA-RECORD-ADD-DT
000419                                     MA-LAST-MAINT-DT.
000420
000421     IF  GCODE1L GREATER ZERO
000422         MOVE GCODE1I            TO  MA-QUALIFY-CODE-1
000423     ELSE
000424         MOVE SPACES             TO  MA-QUALIFY-CODE-1.
000425
000426     IF  GCODE2L GREATER ZERO
000427         MOVE GCODE2I            TO  MA-QUALIFY-CODE-2
000428     ELSE
000429         MOVE SPACES             TO  MA-QUALIFY-CODE-2.
000430
000431     IF  GCODE3L GREATER ZERO
000432         MOVE GCODE3I            TO  MA-QUALIFY-CODE-3
000433     ELSE
000434         MOVE SPACES             TO  MA-QUALIFY-CODE-3.
000435
000436     IF GCODE4L GREATER ZERO
000437         MOVE GCODE4I            TO  MA-QUALIFY-CODE-4
000438     ELSE
000439         MOVE SPACES             TO  MA-QUALIFY-CODE-4.
000440
000441     IF GCODE5L GREATER ZERO
000442         MOVE GCODE5I            TO  MA-QUALIFY-CODE-5
000443     ELSE
000444         MOVE SPACES             TO  MA-QUALIFY-CODE-5.
000445
000446     IF GLNAMEL GREATER ZERO
000447         MOVE GLNAMEI            TO  MA-INSURED-LAST-NAME
000448     ELSE
000449         MOVE SPACES             TO  MA-INSURED-LAST-NAME.
000450
000451     IF GFNAMEL GREATER ZERO
000452         MOVE GFNAMEI            TO  MA-INSURED-FIRST-NAME
000453     ELSE
000454         MOVE SPACES             TO  MA-INSURED-FIRST-NAME.
000455
000456     IF GINITL GREATER ZERO
000457         MOVE GINITI             TO  MA-INSURED-MIDDLE-INIT
000458     ELSE
000459         MOVE SPACES             TO  MA-INSURED-MIDDLE-INIT.
000460
000461     IF GADDR1L GREATER ZERO
000462         MOVE GADDR1I            TO  MA-ADDRESS-LINE-1
000463     ELSE
000464         MOVE SPACES             TO  MA-ADDRESS-LINE-1.
000465
000466     IF GADDR2L GREATER ZERO
000467         MOVE GADDR2I            TO  MA-ADDRESS-LINE-2
000468     ELSE
000469         MOVE SPACES             TO  MA-ADDRESS-LINE-2.
000470
000471     IF GCITYL GREATER ZERO
000472         MOVE GCITYI           TO  MA-CITY
000473     ELSE
000474         MOVE SPACES             TO  MA-CITY.
000475
000476     IF GISTATEL GREATER ZERO
000477         MOVE GISTATEI           TO  MA-ADDR-STATE
000478     ELSE
000479         MOVE SPACES             TO  MA-ADDR-STATE.
000480
000481     IF GZIP1L GREATER ZERO
000482         MOVE GZIP1I             TO  W-ZIP-TEST
000483
000484         IF  W-CANADIAN-POST-CODE
000485             MOVE GZIP1I         TO  MA-CAN-POSTAL-CODE-1
000486
000487         ELSE
000488             MOVE GZIP1I         TO  MA-ZIP-CODE
000489
000490     ELSE
000491         MOVE ZEROS              TO  MA-ZIP-CODE.
000492
000493     IF GZIP2L GREATER ZERO
000494
000495         IF  MA-CANADIAN-POST-CODE
000496             MOVE GZIP2I         TO  MA-CAN-POSTAL-CODE-2
000497
000498         ELSE
000499             MOVE GZIP2I         TO  MA-ZIP-PLUS4
000500
000501     ELSE
000502         MOVE ZEROS              TO  MA-ZIP-PLUS4.
000503
000504     IF GPHONEL GREATER ZERO
000505         MOVE WS-PHONE           TO  MA-PHONE-NO
000506     ELSE
000507         MOVE ZEROS              TO  MA-PHONE-NO.
000508
000509*    IF GSSNOL GREATER ZERO
000510*        MOVE GSSNOI             TO  MA-INSURED-SOC-SEC-NO
000511*    ELSE
000512*        MOVE ZEROS              TO  MA-INSURED-SOC-SEC-NO.
000513
000514     IF GAGEL GREATER ZERO
000515         MOVE GAGEI              TO  MA-INSURED-ISSUE-AGE
000516     ELSE
000517         MOVE ZEROS              TO  MA-INSURED-ISSUE-AGE.
000518
000519     IF GBDTL GREATER ZERO
000520         MOVE WS-BIRTHDT         TO  MA-INSURED-BIRTH-DT
000521     ELSE
000522         MOVE LOW-VALUES         TO  MA-INSURED-BIRTH-DT.
000523
000524     IF GJDOBL > ZERO
000525        MOVE WS-JNT-BIRTHDT      TO MA-JOINT-BIRTH-DT
000526     ELSE
000527        MOVE LOW-VALUES          TO MA-JOINT-BIRTH-DT
000528     END-IF
000529
000530     IF GSEXL GREATER ZERO
000531         MOVE GSEXI              TO  MA-INSURED-SEX
000532     ELSE
000533         MOVE SPACES             TO  MA-INSURED-SEX.
000534
000535     IF GBNAMEL > 0
000536        MOVE GBNAMEI             TO MA-CRED-BENE-NAME
000537     ELSE
000538        MOVE SPACES              TO MA-CRED-BENE-NAME
000539     END-IF
000540
000541     IF GBADD1L > 0
000542        MOVE GBADD1I             TO MA-CRED-BENE-ADDR
000543     ELSE
000544        MOVE SPACES              TO MA-CRED-BENE-ADDR
000545     END-IF
000546
000547     IF GBADD2L > 0
000548        MOVE GBADD2I             TO MA-CRED-BENE-ADDR2
000549     ELSE
000550        MOVE SPACES              TO MA-CRED-BENE-ADDR2
000551     END-IF
000552
000553     IF GBCITYL > 0
000554        MOVE GBCITYI            TO MA-CRED-BENE-CITY
000555     ELSE
000556        MOVE SPACES              TO MA-CRED-BENE-CITY
000557     END-IF
000558
000559     IF GBSTATEL > 0
000560        MOVE GBSTATEI            TO MA-CRED-BENE-STATE
000561     ELSE
000562        MOVE SPACES              TO MA-CRED-BENE-STATE
000563     END-IF
000564
000565     IF GBZIP1L > 0
000566        MOVE GZIP1I              TO W-ZIP-TEST
000567        IF W-CANADIAN-POST-CODE
000568           MOVE GBZIP1I          TO MA-CB-CAN-POSTAL-CODE-1
000569        ELSE
000570           MOVE GBZIP1I          TO MA-CB-ZIP-CODE
000571        END-IF
000572     ELSE
000573        MOVE ZEROS               TO MA-CB-ZIP-CODE
000574     END-IF
000575
000576     IF GBZIP2L > 0
000577        IF MA-CB-CANADIAN-POST-CODE
000578           MOVE GBZIP2I          TO MA-CB-CAN-POSTAL-CODE-2
000579        ELSE
000580           MOVE GBZIP2I          TO MA-CB-ZIP-PLUS4
000581        END-IF
000582     ELSE
000583        MOVE ZEROS               TO MA-CB-ZIP-PLUS4
000584     END-IF
000585
000586     .
000587 1700-WRITE-RECORD.
000588
000589     PERFORM 6400-WRITE-MAIL-RECORD.
000590
000591     IF  DUPLICATE-RECORD-FOUND
000592         GO TO 1800-DUPLICATE-RECORD.
000593
000594     MOVE ER-0000                TO  EMI-ERROR.
000595     PERFORM 9900-ERROR-FORMAT.
000596     PERFORM 7000-FORMAT-SCREEN.
000597     PERFORM 4000-CERTIFICATE-UPDATE.
000598     GO TO 8100-SEND-INITIAL-MAP.
000599
000600 1800-DUPLICATE-RECORD.
000601     MOVE ER-2199                TO  EMI-ERROR.
000602     PERFORM 9900-ERROR-FORMAT.
000603     PERFORM 7000-FORMAT-SCREEN.
000604     GO TO 8100-SEND-INITIAL-MAP.
000605
000606 1900-EXIT.
000607     EXIT.
000608
000609     EJECT
000610
000611 2000-CHANGE-RECORD   SECTION.
000612     PERFORM 3000-EDIT-MAIL-DATA.
000613
000614     if (emi-fatal-ctr = zeros)
000615        and (emi-forcable-ctr = 1)
000616        and (emi-error = er-3061)
000617        and (eibaid = dfhpf7)
000618        move '2'                 to emi-switch1
000619        continue
000620     else
000621        IF NOT EMI-NO-ERRORS
000622           GO TO 8200-SEND-DATAONLY
000623        end-if
000624     end-if
000625
000626     MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
000627     MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
000628     MOVE  PI-STATE              TO  WS-CM-STATE.
000629     MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
000630     MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
000631     MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
000632     MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
000633     MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
000634
000635     PERFORM 6300-READ-MAIL-FILE-UPDT.
000636
000637     IF  RECORD-NOT-FOUND
000638         GO TO 1000-ADD-RECORD.
000639
000640     MOVE PI-PROCESSOR-ID        TO  MA-LAST-MAINT-BY.
000641     MOVE EIBTIME                TO  MA-LAST-MAINT-HHMMSS.
000642     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000643     MOVE '5'                    TO  DC-OPTION-CODE.
000644
000645     PERFORM 9700-DATE-LINK.
000646     IF DATE-CONVERSION-ERROR
000647         MOVE LOW-VALUES         TO  MA-LAST-MAINT-DT
000648     ELSE
000649         MOVE DC-BIN-DATE-1      TO  MA-LAST-MAINT-DT.
000650
000651     IF  GCODE1L GREATER ZERO
000652         MOVE GCODE1I            TO  MA-QUALIFY-CODE-1.
000653     IF  GCODE2L GREATER ZERO
000654         MOVE GCODE2I            TO  MA-QUALIFY-CODE-2.
000655     IF  GCODE3L GREATER ZERO
000656         MOVE GCODE3I            TO  MA-QUALIFY-CODE-3.
000657     IF  GCODE4L GREATER ZERO
000658         MOVE GCODE4I            TO  MA-QUALIFY-CODE-4.
000659     IF  GCODE5L GREATER ZERO
000660         MOVE GCODE5I            TO  MA-QUALIFY-CODE-5.
000661     IF  GLNAMEL GREATER ZERO
000662         MOVE GLNAMEI            TO  MA-INSURED-LAST-NAME.
000663     IF  GFNAMEL GREATER ZERO
000664         MOVE GFNAMEI            TO  MA-INSURED-FIRST-NAME.
000665     IF  GINITL GREATER ZERO
000666         MOVE GINITI             TO  MA-INSURED-MIDDLE-INIT.
000667     IF  GADDR1L GREATER ZERO
000668         MOVE GADDR1I            TO  MA-ADDRESS-LINE-1.
000669     IF  GADDR2L GREATER ZERO
000670         MOVE GADDR2I            TO  MA-ADDRESS-LINE-2.
000671     IF  GCITYL GREATER ZERO
000672         MOVE GCITYI           TO  MA-CITY.
000673
000674     IF  GISTATEL GREATER ZERO
000675         MOVE GISTATEI          TO  MA-ADDR-STATE.
000676
000677     IF GZIP1L <> 0
000678         MOVE GZIP1I             TO  W-ZIP-TEST
000679
000680         IF  W-CANADIAN-POST-CODE
000681             MOVE GZIP1I         TO  MA-CAN-POSTAL-CODE-1
000682
000683         ELSE
000684             MOVE GZIP1I         TO  MA-ZIP-CODE
000685
000686     ELSE
000687         MOVE ZEROS              TO  MA-ZIP-CODE.
000688
000689     IF GZIP2L GREATER ZERO
000690
000691         IF  MA-CANADIAN-POST-CODE
000692             MOVE GZIP2I         TO  MA-CAN-POSTAL-CODE-2
000693
000694         ELSE
000695             MOVE GZIP2I         TO  MA-ZIP-PLUS4
000696
000697     ELSE
000698         MOVE ZEROS              TO  MA-ZIP-PLUS4.
000699
000700     IF GPHONEL GREATER ZERO
000701         MOVE WS-PHONE           TO  MA-PHONE-NO.
000702*    IF  GSSNOL GREATER ZERO
000703*        MOVE GSSNOI             TO  MA-INSURED-SOC-SEC-NO.
000704     IF  GAGEL GREATER ZERO
000705         MOVE GAGEI              TO  MA-INSURED-ISSUE-AGE.
000706     IF  GBDTL GREATER ZERO
000707         MOVE WS-BIRTHDT         TO  MA-INSURED-BIRTH-DT.
000708     IF GJDOBL > 0
000709        MOVE WS-JNT-BIRTHDT      TO MA-JOINT-BIRTH-DT
000710     END-IF
000711     IF  GSEXL GREATER ZERO
000712         MOVE GSEXI              TO  MA-INSURED-SEX.
000713
000714     IF GBNAMEL > 0
000715        MOVE GBNAMEI             TO MA-CRED-BENE-NAME
000716     END-IF
000717
000718     IF GBADD1L > 0
000719        MOVE GBADD1I             TO MA-CRED-BENE-ADDR
000720     END-IF
000721
000722     IF GBADD2L > 0
000723        MOVE GBADD2I             TO MA-CRED-BENE-ADDR2
000724     END-IF
000725
000726     IF GBCITYL > 0
000727        MOVE GBCITYI            TO MA-CRED-BENE-CITY
000728     END-IF
000729
000730     IF GBSTATEL > 0
000731        MOVE GBSTATEI            TO MA-CRED-BENE-STATE
000732     END-IF
000733
000734     IF GBZIP1L > 0
000735        MOVE GBZIP1I             TO W-ZIP-TEST
000736        IF W-CANADIAN-POST-CODE
000737           MOVE GBZIP1I          TO MA-CB-CAN-POSTAL-CODE-1
000738        ELSE
000739           MOVE GBZIP1I          TO MA-CB-ZIP-CODE
000740        END-IF
000741     ELSE
000742        MOVE ZEROS               TO MA-CB-ZIP-CODE
000743     END-IF
000744
000745     IF GBZIP2L > 0
000746        IF MA-CB-CANADIAN-POST-CODE
000747           MOVE GBZIP2I          TO MA-CB-CAN-POSTAL-CODE-2
000748        ELSE
000749           MOVE GBZIP2I          TO MA-CB-ZIP-PLUS4
000750        END-IF
000751     ELSE
000752        MOVE ZEROS               TO MA-CB-ZIP-PLUS4
000753     END-IF
000754
000755     .
000756 2700-REWRITE-RECORD.
000757
000758     PERFORM 6500-REWRITE-MAIL-RECORD.
000759
000760     IF PI-COMPANY-ID NOT = 'DCC' OR 'VPP'
000761        IF GBNAMEL > 0
000762           
      * EXEC CICS READ
000763*             EQUAL
000764*             DATASET   (ELCERT-ID)
000765*             SET       (ADDRESS OF CERTIFICATE-MASTER)
000766*             RIDFLD    (WS-CM-CONTROL-PRIMARY)
000767*             UPDATE
000768*             RESP       (WS-RESPONSE)
000769*          END-EXEC
      *    MOVE '&"S        EU         (  N#00004163' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303034313633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000770           IF RESP-NORMAL
000771              IF GBNAMEI NOT = CM-BENEFICIARY
000772                 MOVE GBNAMEI       TO CM-BENEFICIARY
000773                 
      * EXEC CICS REWRITE
000774*                   FROM    (CERTIFICATE-MASTER)
000775*                   DATASET (ELCERT-ID)
000776*                   RESP    (WS-RESPONSE)
000777*                END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00004174' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303034313734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000778              ELSE
000779                 
      * EXEC CICS UNLOCK
000780*                   DATASET  (ELCERT-ID)
000781*                   RESP     (WS-RESPONSE)
000782*                END-EXEC
      *    MOVE '&*                    #  N#00004180' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'204E233030303034313830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000783              END-IF
000784           END-IF
000785        END-IF
000786     END-IF
000787
000788     MOVE ER-0000                TO  EMI-ERROR.
000789     PERFORM 9900-ERROR-FORMAT.
000790     PERFORM 7000-FORMAT-SCREEN.
000791     GO TO 8100-SEND-INITIAL-MAP.
000792
000793 2900-EXIT.
000794     EXIT.
000795
000796     EJECT
000797
000798 3000-EDIT-MAIL-DATA   SECTION.
000799     IF GAGEL NOT = 0
000800         IF GAGEI NOT NUMERIC
000801             MOVE -1             TO  GAGEL
000802             MOVE AL-UABON       TO  GAGEA
000803             MOVE ER-2187        TO  EMI-ERROR
000804             PERFORM 9900-ERROR-FORMAT.
000805
000806     IF GBDTL NOT = 0
000807        MOVE GBDTI               TO DEEDIT-FIELD
000808        PERFORM 8600-DEEDIT
000809        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000810        MOVE '4'                 TO DC-OPTION-CODE
000811        PERFORM 9700-DATE-LINK
000812        IF DATE-CONVERSION-ERROR
000813           MOVE -1               TO GBDTL
000814           MOVE AL-UABON         TO GBDTA
000815           MOVE ER-0220          TO EMI-ERROR
000816           PERFORM 9900-ERROR-FORMAT
000817        ELSE
000818           if dc-bin-date-1 > pi-cert-eff-dt
000819              MOVE DEEDIT-FIELD-V0
000820                                 TO DC-GREG-DATE-1-MDY
000821              MOVE '4'           TO DC-OPTION-CODE
000822              move '1'           to dc-century-adjustment
000823              PERFORM 9700-DATE-LINK
000824              if no-conversion-error
000825                 MOVE DC-BIN-DATE-1
000826                                 TO WS-BIRTHDT
000827              end-if
000828           else
000829              MOVE DC-BIN-DATE-1 TO WS-BIRTHDT
000830           end-if
000831        end-if
000832     end-if
000833
000834     IF GJDOBL NOT = 0
000835         MOVE GJDOBI          TO  DEEDIT-FIELD
000836         PERFORM 8600-DEEDIT
000837         MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
000838         MOVE '4'                TO  DC-OPTION-CODE
000839         PERFORM 9700-DATE-LINK
000840         IF DATE-CONVERSION-ERROR
000841             MOVE -1             TO  GJDOBL
000842             MOVE AL-UABON       TO  GJDOBA
000843             MOVE ER-0220        TO  EMI-ERROR
000844             PERFORM 9900-ERROR-FORMAT
000845         ELSE
000846           if dc-bin-date-1 > pi-cert-eff-dt
000847              MOVE DEEDIT-FIELD-V0
000848                                 TO DC-GREG-DATE-1-MDY
000849              MOVE '4'           TO DC-OPTION-CODE
000850              move '1'           to dc-century-adjustment
000851              PERFORM 9700-DATE-LINK
000852              if no-conversion-error
000853                 MOVE DC-BIN-DATE-1
000854                                 TO WS-JNT-BIRTHDT
000855              end-if
000856           else
000857              MOVE DC-BIN-DATE-1 TO WS-JNT-BIRTHDT
000858           end-if
000859        end-if
000860     end-if
000861
000862     IF GSEXL NOT = 0
000863         IF GSEXI NOT = 'M' AND 'F'
000864             MOVE -1             TO  GSEXL
000865             MOVE AL-UABON       TO  GSEXA
000866             MOVE ER-0219        TO  EMI-ERROR
000867             PERFORM 9900-ERROR-FORMAT.
000868
000869     IF GPHONEL NOT = 0
000870         MOVE GPHONEI            TO  DEEDIT-FIELD
000871         PERFORM 8600-DEEDIT
000872         MOVE DEEDIT-FIELD-V0    TO  WS-PHONE.
000873
000874     if  (GCITYI NOT GREATER THAN SPACES)
000875             or
000876         (GISTATEI NOT GREATER THAN SPACES)
000877         MOVE -1                 TO  GCITYL
000878                                     GISTATEL
000879         MOVE AL-UABON           TO  GCITYA
000880                                     GISTATEA
000881         MOVE ER-7428            TO  EMI-ERROR
000882         PERFORM 9900-ERROR-FORMAT.
000883
000884     IF GISTATEL > +0
000885        MOVE SPACES              TO ELCNTL-KEY
000886        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
000887        MOVE '3'                 TO ELCNTL-REC-TYPE
000888        MOVE GISTATEI            TO ELCNTL-ACCESS
000889        MOVE +0                  TO ELCNTL-SEQ
000890        
      * EXEC CICS READ
000891*          DATASET   (FILE-ID-ELCNTL)
000892*          SET       (ADDRESS OF CONTROL-FILE)
000893*          RIDFLD    (ELCNTL-KEY)
000894*          RESP      (WS-RESPONSE)
000895*       END-EXEC
      *    MOVE '&"S        E          (  N#00004291' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034323931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
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
000896        IF RESP-NORMAL
000897           MOVE AL-UANON         TO GISTATEA
000898        ELSE
000899           MOVE ER-2209          TO EMI-ERROR
000900           MOVE -1               TO GISTATEL
000901           MOVE AL-UABON         TO GISTATEA
000902           PERFORM 9900-ERROR-FORMAT
000903                                 THRU 9900-EXIT
000904        END-IF
000905     END-IF
000906
000907     if gzip1l <> zeros
000908        if (gzip1i numeric)
000909           and (gzip1i <> zeros)
000910           continue
000911        else
000912           move er-2050          to emi-error
000913           move -1               to gzip1l
000914           move al-uabon         to gzip1a
000915           perform 9900-error-format
000916                                 thru 9900-exit
000917        end-if
000918     end-if
000919
000920     IF EMI-NO-ERRORS
000921        move spaces              to zipcd-pass-area
000922        move gzip1i              to pa-zip
000923        move gcityi              to pa-city
000924        move gistatei            to pa-state
000925        perform 7790-call-zip-verify
000926                                 thru 7790-exit
000927        if pa-errorcode-zip (1:1) = ' '
000928           continue
000929        else
000930           MOVE ER-3061          TO emi-ERROR
000931           move -1               to gzip1l
000932           PERFORM 9900-ERROR-FORMAT
000933                                 THRU 9900-EXIT
000934        end-if
000935     end-if
000936
000937     IF (GBSTATEL > +0)
000938        AND (GBSTATEI NOT = SPACES AND LOW-VALUES)
000939        MOVE SPACES              TO ELCNTL-KEY
000940        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
000941        MOVE '3'                 TO ELCNTL-REC-TYPE
000942        MOVE GBSTATEI            TO ELCNTL-ACCESS
000943        MOVE +0                  TO ELCNTL-SEQ
000944        
      * EXEC CICS READ
000945*          DATASET   (FILE-ID-ELCNTL)
000946*          SET       (ADDRESS OF CONTROL-FILE)
000947*          RIDFLD    (ELCNTL-KEY)
000948*          RESP      (WS-RESPONSE)
000949*       END-EXEC
      *    MOVE '&"S        E          (  N#00004345' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034333435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
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
000950        IF RESP-NORMAL
000951           MOVE AL-UANON         TO GBSTATEA
000952        ELSE
000953           MOVE ER-2209          TO EMI-ERROR
000954           MOVE -1               TO GBSTATEL
000955           MOVE AL-UABON         TO GBSTATEA
000956           PERFORM 9900-ERROR-FORMAT
000957                                 THRU 9900-EXIT
000958        END-IF
000959     END-IF
000960
000961     .
000962 4000-CERTIFICATE-UPDATE         SECTION.
000963     
      * EXEC CICS HANDLE CONDITION
000964*        NOTFND   (4400-NOT-FOUND)
000965*    END-EXEC.
      *    MOVE '"$I                   ! # #00004364' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303034333634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000966
000967     
      * EXEC CICS READ
000968*        EQUAL
000969*        DATASET   (ELCERT-ID)
000970*        SET       (ADDRESS OF CERTIFICATE-MASTER)
000971*        RIDFLD    (WS-CM-CONTROL-PRIMARY)
000972*        UPDATE
000973*    END-EXEC.
      *    MOVE '&"S        EU         (   #00004368' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034333638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000974
000975 4200-REWRITE-CERT-MASTER.
000976     MOVE '1'                    TO  CM-INSURED-ADDRESS-SW
000977
000978     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
000979        IF GBNAMEL > 0
000980           IF GBNAMEI NOT = CM-BENEFICIARY
000981              MOVE GBNAMEI          TO CM-BENEFICIARY
000982           END-IF
000983        END-IF
000984     END-IF
000985
000986     
      * EXEC CICS REWRITE
000987*        FROM      (CERTIFICATE-MASTER)
000988*        DATASET   (ELCERT-ID)
000989*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004387' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034333837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000990
000991     GO TO 4500-EXIT.
000992
000993 4400-NOT-FOUND.
000994     MOVE -1                     TO  GMAINTL.
000995     MOVE ER-0142                TO  EMI-ERROR.
000996     PERFORM 9900-ERROR-FORMAT.
000997     GO TO 8200-SEND-DATAONLY.
000998
000999 4500-EXIT.
001000     EXIT.
001001
001002     EJECT
001003
001004 6200-READ-MAIL-FILE             SECTION.
001005     
      * EXEC CICS HANDLE CONDITION
001006*        NOTFND   (6250-NOT-FOUND)
001007*    END-EXEC.
      *    MOVE '"$I                   ! $ #00004406' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303034343036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001008
001009     
      * EXEC CICS READ
001010*        EQUAL
001011*        DATASET   (ERMAIL-ID)
001012*        SET       (ADDRESS OF MAILING-DATA)
001013*        RIDFLD    (WS-CM-CONTROL-PRIMARY)
001014*    END-EXEC.
      *    MOVE '&"S        E          (   #00004410' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303034343130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001015
001016     GO TO 6290-EXIT.
001017
001018 6250-NOT-FOUND.
001019     MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
001020
001021 6290-EXIT.
001022      EXIT.
001023
001024     EJECT
001025
001026 6300-READ-MAIL-FILE-UPDT        SECTION.
001027     
      * EXEC CICS HANDLE CONDITION
001028*        NOTFND   (6350-NOT-FOUND)
001029*    END-EXEC.
      *    MOVE '"$I                   ! % #00004428' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303034343238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001030
001031     
      * EXEC CICS READ
001032*        EQUAL
001033*        DATASET   (ERMAIL-ID)
001034*        SET       (ADDRESS OF MAILING-DATA)
001035*        RIDFLD    (WS-CM-CONTROL-PRIMARY)
001036*        UPDATE
001037*    END-EXEC.
      *    MOVE '&"S        EU         (   #00004432' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034343332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001038
001039     GO TO 6390-EXIT.
001040
001041 6350-NOT-FOUND.
001042     MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
001043
001044 6390-EXIT.
001045      EXIT.
001046
001047      EJECT
001048
001049 6400-WRITE-MAIL-RECORD          SECTION.
001050     
      * EXEC CICS HANDLE CONDITION
001051*        DUPREC   (6450-DUPLICATE-RECORD)
001052*    END-EXEC.
      *    MOVE '"$%                   ! & #00004451' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303034343531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001053
001054     
      * EXEC CICS WRITE
001055*        FROM      (MAILING-DATA)
001056*        DATASET   (ERMAIL-ID)
001057*        RIDFLD    (WS-CM-CONTROL-PRIMARY)
001058*    END-EXEC.
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004455' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034343535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001059
001060     GO TO 6490-EXIT.
001061
001062 6450-DUPLICATE-RECORD.
001063     MOVE 'Y'                    TO  WS-DUPREC-SW.
001064
001065 6490-EXIT.
001066     EXIT.
001067
001068     EJECT
001069
001070 6500-REWRITE-MAIL-RECORD        SECTION.
001071     
      * EXEC CICS REWRITE
001072*        FROM      (MAILING-DATA)
001073*        DATASET   (ERMAIL-ID)
001074*    END-EXEC.
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004472' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034343732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001075
001076 6590-EXIT.
001077     EXIT.
001078
001079     EJECT
001080
001081 6600-DELETE-MAIL-RECORD         SECTION.
001082     
      * EXEC CICS HANDLE CONDITION
001083*        NOTFND (6650-RECORD-PREVS-DELETED)
001084*    END-EXEC.
      *    MOVE '"$I                   ! '' #00004483' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303034343833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001085
001086     
      * EXEC CICS DELETE
001087*        DATASET   (ERMAIL-ID)
001088*    END-EXEC.
      *    MOVE '&(                    &   #00004487' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034343837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001089
001090     GO TO 6690-EXIT.
001091
001092 6650-RECORD-PREVS-DELETED.
001093     MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
001094
001095 6690-EXIT.
001096     EXIT.
001097
001098     EJECT
001099
001100 7000-FORMAT-SCREEN              SECTION.
001101     MOVE LOW-VALUES             TO  EL127GO.
001102
001103     move  -1                    to  gmaintl
001104     MOVE  PI-CARRIER            TO  GCARIERO.
001105     MOVE  PI-GROUPING           TO  GGROUPO.
001106     MOVE  PI-STATE              TO  GSTATEO.
001107     MOVE  PI-ACCOUNT            TO  GACCTNOO.
001108     MOVE  PI-CERT-PRIME         TO  GCERTNOO.
001109     MOVE  PI-CERT-SFX           TO  GCRTSFXO.
001110     MOVE  ' '                   TO  DC-OPTION-CODE.
001111     MOVE  PI-CERT-EFF-DT        TO  DC-BIN-DATE-1.
001112     PERFORM 9700-DATE-LINK.
001113     MOVE DC-GREG-DATE-1-EDIT    TO  GEFFDTO.
001114
001115     MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
001116     MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
001117     MOVE  PI-STATE              TO  WS-CM-STATE.
001118     MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
001119     MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
001120     MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
001121     MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
001122     MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
001123
001124     PERFORM 6200-READ-MAIL-FILE.
001125
001126     IF RECORD-NOT-FOUND
001127         IF PI-PEND-SW = 'P'
001128             MOVE 'S'            TO  GMAINTI
001129             MOVE AL-SANON       TO  GMAINTA
001130             MOVE ER-7049        TO  EMI-ERROR
001131             PERFORM 9900-ERROR-FORMAT
001132             GO TO 7000-EXIT
001133         ELSE
001134             MOVE 'A'            TO  GMAINTI
001135             MOVE AL-UANON       TO  GMAINTA
001136             GO TO 7000-EXIT.
001137
001138     MOVE 'C'                    TO  GMAINTO.
001139     MOVE AL-UANON               TO  GMAINTA.
001140
001141     IF PI-PEND-SW = 'P'
001142         MOVE 'S'                TO  GMAINTI
001143         MOVE AL-SANON           TO  GMAINTA
001144         MOVE ER-7049            TO  EMI-ERROR
001145         PERFORM 9900-ERROR-FORMAT.
001146
001147     MOVE MA-LAST-MAINT-BY       TO  GLSTUSRO.
001148     MOVE MA-LAST-MAINT-HHMMSS   TO  TIME-IN.
001149     MOVE TIME-OUT               TO  GLSTIMEO.
001150     MOVE ' '                    TO  DC-OPTION-CODE.
001151     MOVE MA-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
001152     PERFORM 9700-DATE-LINK.
001153     IF DATE-CONVERSION-ERROR
001154         MOVE ZEROS                  TO  GLSTDTO
001155     ELSE
001156         MOVE DC-GREG-DATE-1-EDIT    TO  GLSTDTO.
001157
001158     MOVE MA-RECORD-ADDED-BY     TO  GADDBYO.
001159     MOVE ' '                    TO  DC-OPTION-CODE.
001160     MOVE MA-RECORD-ADD-DT       TO  DC-BIN-DATE-1.
001161     PERFORM 9700-DATE-LINK.
001162     IF DATE-CONVERSION-ERROR
001163         MOVE ZEROS                  TO  GADDDTO
001164     ELSE
001165         MOVE DC-GREG-DATE-1-EDIT    TO  GADDDTO.
001166
001167     MOVE MA-QUALIFY-CODE-1      TO  GCODE1O.
001168     MOVE MA-QUALIFY-CODE-2      TO  GCODE2O.
001169     MOVE MA-QUALIFY-CODE-3      TO  GCODE3O.
001170     MOVE MA-QUALIFY-CODE-4      TO  GCODE4O.
001171     MOVE MA-QUALIFY-CODE-5      TO  GCODE5O.
001172     MOVE MA-INSURED-LAST-NAME   TO  GLNAMEO.
001173     MOVE MA-INSURED-FIRST-NAME  TO  GFNAMEO.
001174     MOVE MA-INSURED-MIDDLE-INIT TO  GINITO.
001175     MOVE MA-ADDRESS-LINE-1      TO  GADDR1O.
001176     MOVE MA-ADDRESS-LINE-2      TO  GADDR2O.
001177     MOVE MA-CITY                TO  GCITYO.
001178     MOVE MA-ADDR-STATE          TO  GISTATEO
001179
001180     IF  MA-CANADIAN-POST-CODE
001181         MOVE MA-CAN-POSTAL-CODE-1 TO GZIP1O
001182
001183     ELSE
001184         MOVE MA-ZIP-CODE        TO  GZIP1O.
001185
001186     IF  MA-CANADIAN-POST-CODE
001187         MOVE MA-CAN-POSTAL-CODE-2
001188                                 TO  GZIP2O
001189
001190     ELSE
001191         MOVE MA-ZIP-PLUS4       TO  GZIP2O.
001192
001193*    MOVE MA-INSURED-SOC-SEC-NO  TO  GSSNOO.
001194
001195     IF MA-INSURED-SEX NOT = SPACES
001196         MOVE MA-INSURED-SEX     TO  GSEXO
001197         MOVE AL-UANON           TO  GSEXA.
001198
001199     IF MA-INSURED-ISSUE-AGE NOT NUMERIC OR
001200         MA-INSURED-ISSUE-AGE = ZEROS
001201             MOVE LOW-VALUES             TO  GAGEO
001202             MOVE AL-UANOF               TO  GAGEA
001203     ELSE
001204             MOVE AL-UANON               TO  GAGEA
001205             MOVE MA-INSURED-ISSUE-AGE   TO  GAGEO.
001206
001207     IF MA-INSURED-BIRTH-DT IS EQUAL TO LOW-VALUES OR SPACES
001208         MOVE SPACES                 TO  GBDTO
001209         MOVE AL-UANOF               TO  GBDTA
001210     ELSE
001211         MOVE ' '                    TO  DC-OPTION-CODE
001212         MOVE MA-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1
001213         PERFORM 9700-DATE-LINK
001214         IF DATE-CONVERSION-ERROR
001215             MOVE LOW-VALUES             TO  GBDTO
001216             MOVE AL-UANOF               TO  GBDTA
001217         ELSE
001218             MOVE AL-UANON               TO  GBDTA
001219             MOVE DC-GREG-DATE-1-EDIT    TO  GBDTO.
001220
001221     IF MA-JOINT-BIRTH-DT = LOW-VALUES OR SPACES
001222        MOVE SPACES              TO GJDOBO
001223        MOVE AL-UANOF            TO GJDOBA
001224     ELSE
001225        MOVE ' '                 TO DC-OPTION-CODE
001226        MOVE MA-JOINT-BIRTH-DT   TO DC-BIN-DATE-1
001227        PERFORM 9700-DATE-LINK
001228        IF DATE-CONVERSION-ERROR
001229           MOVE LOW-VALUES       TO GJDOBO
001230           MOVE AL-UANOF         TO GJDOBA
001231        ELSE
001232           MOVE AL-UANON         TO GJDOBA
001233           MOVE DC-GREG-DATE-1-EDIT
001234                                 TO  GJDOBO
001235        END-IF
001236     END-IF
001237
001238     IF MA-PHONE-NO NOT NUMERIC OR
001239         MA-PHONE-NO = ZEROS
001240             MOVE LOW-VALUES             TO  GPHONEO
001241             MOVE AL-UANOF               TO  GPHONEA
001242     ELSE
001243             MOVE MA-PHONE-NO            TO  WS-PHONE
001244             MOVE WS-PHONE-AC            TO  ED-PHONE-AC
001245             MOVE WS-PHONE-EXT           TO  ED-PHONE-EXT
001246             MOVE WS-PHONE-NO            TO  ED-PHONE-NO
001247             MOVE '-'                    TO  ED-PHONE-DASH1
001248                                             ED-PHONE-DASH2
001249             MOVE EDITED-PHONE-NO        TO  GPHONEO
001250             MOVE AL-UANON               TO  GPHONEA
001251     END-IF
001252
001253     MOVE MA-CRED-BENE-NAME      TO GBNAMEO
001254     MOVE MA-CRED-BENE-ADDR      TO GBADD1O
001255     MOVE MA-CRED-BENE-ADDR2     TO GBADD2O
001256     MOVE MA-CRED-BENE-CITY      TO GBCITYO
001257     MOVE MA-CRED-BENE-STATE     TO GBSTATEO
001258
001259     IF MA-CB-CANADIAN-POST-CODE
001260        MOVE MA-CB-CAN-POSTAL-CODE-1
001261                                 TO GBZIP1O
001262     ELSE
001263        MOVE MA-CB-ZIP-CODE      TO GBZIP1O
001264     END-IF
001265
001266     IF MA-CB-CANADIAN-POST-CODE
001267        MOVE MA-CB-CAN-POSTAL-CODE-2
001268                                 TO GBZIP2O
001269     ELSE
001270        MOVE MA-CB-ZIP-PLUS4     TO GBZIP2O
001271     END-IF
001272
001273     MOVE +0 TO WS-SUB.
001274     PERFORM 7 TIMES
001275         ADD +1 TO WS-SUB
001276         IF MA-MAIL-TYPE (WS-SUB) NOT = LOW-VALUES AND SPACES
001277             IF MA-MAIL-TYPE (WS-SUB) = '1'
001278                MOVE '12M' TO WS-MAILED-TYPE (WS-SUB)
001279             ELSE
001280                MOVE 'EXP' TO WS-MAILED-TYPE (WS-SUB)
001281             END-IF
001282             IF MA-MAIL-STATUS (WS-SUB) = '3'
001283                MOVE 'NOT MAILED'  TO  WS-MAILED-STATUS (WS-SUB)
001284             ELSE
001285                 IF MA-MAIL-STATUS (WS-SUB) = '2'
001286                     MOVE 'RETRN'  TO  WS-MAILED-STAT
001287                 ELSE
001288                     MOVE 'MAILD'  TO  WS-MAILED-STAT
001289                 END-IF
001290                 MOVE MA-MAIL-DATE (WS-SUB) TO  DC-BIN-DATE-1
001291                 PERFORM 9700-DATE-LINK
001292                 MOVE DC-GREG-DATE-1-EDIT   TO WS-MAILED-DATE
001293                 MOVE WS-MAILED-BLD-STATUS TO
001294                                   WS-MAILED-STATUS (WS-SUB)
001295             END-IF
001296         ELSE
001297             MOVE SPACES TO WS-MAILED-TYPE (WS-SUB)
001298                            WS-MAILED-STATUS (WS-SUB)
001299         END-IF
001300     END-PERFORM.
001301
001302     MOVE WS-MAILED-TYPE (1)   TO GTYPE1O.
001303     MOVE WS-MAILED-STATUS (1) TO GSTAT1O.
001304     MOVE WS-MAILED-TYPE (2)   TO GTYPE2O.
001305     MOVE WS-MAILED-STATUS (2) TO GSTAT2O.
001306     MOVE WS-MAILED-TYPE (3)   TO GTYPE3O.
001307     MOVE WS-MAILED-STATUS (3) TO GSTAT3O.
001308     MOVE WS-MAILED-TYPE (4)   TO GTYPE4O.
001309     MOVE WS-MAILED-STATUS (4) TO GSTAT4O.
001310     MOVE WS-MAILED-TYPE (5)   TO GTYPE5O.
001311     MOVE WS-MAILED-STATUS (5) TO GSTAT5O.
001312     MOVE WS-MAILED-TYPE (6)   TO GTYPE6O.
001313     MOVE WS-MAILED-STATUS (6) TO GSTAT6O.
001314     MOVE WS-MAILED-TYPE (7)   TO GTYPE7O.
001315     MOVE WS-MAILED-STATUS (7) TO GSTAT7O.
001316
001317     move al-sadof               to  gssnoa
001318     MOVE AL-UANON               TO  GCODE1A   GCODE2A    GCODE3A
001319                                     GCODE4A   GCODE5A    GLNAMEA
001320                                     GFNAMEA   GINITA
001321                                     GADDR1A   GADDR2A    GCITYA
001322                                     GISTATEA
001323                                     GZIP1A    GZIP2A     GBNAMEA
001324                                     GBADD1A   GBADD2A    GBCITYA
001325                                     GBSTATEA
001326                                     GBZIP1A   GBZIP2A
001327     .
001328 7000-EXIT.
001329     EXIT.
001330
001331 7790-CALL-ZIP-VERIFY section.
001332
001333     
      * EXEC CICS LINK
001334*        PROGRAM    ('WSZIPCD')
001335*        COMMAREA   (ZIPCD-PASS-AREA)
001336*        LENGTH     (ZIPCD-PASS-AREA-LEN)
001337*    END-EXEC.
           MOVE 'WSZIPCD' TO DFHEIV1
      *    MOVE '."C                   (   #00004734' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034373334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ZIPCD-PASS-AREA, 
                 ZIPCD-PASS-AREA-LEN, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001338
001339 7790-EXIT.
001340     EXIT.
001341
001342 8100-SEND-INITIAL-MAP SECTION.
001343     MOVE SAVE-DATE              TO  GDATEO.
001344     MOVE EIBTIME                TO  TIME-IN.
001345     MOVE TIME-OUT               TO  GTIMEO.
001346     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
001347     MOVE PI-PROCESSOR-ID        TO  USERIDO.
001348     MOVE EMI-MESSAGE-AREA (1)   TO  GERMSG1O.
001349
001350     IF PI-PEND-SW = 'P'
001351         MOVE -1                 TO  GPFKEYL
001352     ELSE
001353         MOVE -1                 TO  GMAINTL.
001354
001355     
      * EXEC CICS SEND
001356*        MAP      (MAP-NAME)
001357*        MAPSET   (MAPSET-NAME)
001358*        FROM     (EL127GO)
001359*        ERASE
001360*        CURSOR
001361*     END-EXEC.
           MOVE LENGTH OF
            EL127GO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004756' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303034373536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127GO, 
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
           
001362
001363     GO TO 9100-RETURN-TRAN.
001364
001365 8200-SEND-DATAONLY     SECTION.
001366     MOVE SAVE-DATE              TO  GDATEO.
001367     MOVE EIBTIME                TO  TIME-IN.
001368     MOVE TIME-OUT               TO  GTIMEO.
001369     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
001370     MOVE PI-PROCESSOR-ID        TO  USERIDO.
001371     MOVE EMI-MESSAGE-AREA (1)   TO  GERMSG1O.
001372
001373     IF PI-PEND-SW = 'P'
001374         MOVE -1                 TO  GPFKEYL.
001375*    ELSE
001376*        MOVE -1                 TO  GMAINTL.
001377
001378     
      * EXEC CICS SEND
001379*        MAP      (MAP-NAME)
001380*        MAPSET   (MAPSET-NAME)
001381*        FROM     (EL127GO)
001382*        DATAONLY
001383*        ERASEAUP
001384*        CURSOR
001385*     END-EXEC.
           MOVE LENGTH OF
            EL127GO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00004779' TO DFHEIV0
           MOVE X'382444202020204354202041' &
                X'2020202048204C2046202C20' &
                X'2020233030303034373739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127GO, 
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
           
001386
001387     GO TO 9100-RETURN-TRAN.
001388
001389     EJECT
001390
001391 8300-SEND-TEXT         SECTION.
001392     
      * EXEC CICS SEND TEXT
001393*        FROM     (LOGOFF-TEXT)
001394*        LENGTH   (LOGOFF-LENGTH)
001395*        ERASE
001396*        FREEKB
001397*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004793' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303034373933' TO DFHEIV0
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
           
001398
001399     
      * EXEC CICS RETURN
001400*    END-EXEC.
      *    MOVE '.(                    ''   #00004800' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034383030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001401
001402     EJECT
001403
001404
001405 8600-DEEDIT           SECTION.
001406     
      * EXEC CICS BIF DEEDIT
001407*         FIELD   (DEEDIT-FIELD)
001408*         LENGTH  (15)
001409*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004807' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001410
001411 8800-UNAUTHORIZED-ACCESS        SECTION.
001412     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
001413     GO TO 8300-SEND-TEXT.
001414
001415 8810-PF23              SECTION.
001416     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
001417     MOVE XCTL-005               TO  PGM-NAME.
001418     GO TO 9300-XCTL.
001419
001420 9100-RETURN-TRAN       SECTION.
001421     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
001422     MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
001423     
      * EXEC CICS RETURN
001424*        TRANSID    (TRANS-ID)
001425*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
001426*        LENGTH     (PI-COMM-LENGTH)
001427*    END-EXEC.
      *    MOVE '.(CT                  ''   #00004824' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034383234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001428
001429 9200-RETURN-MAIN-MENU SECTION.
001430     MOVE XCTL-126               TO  PGM-NAME.
001431     GO TO 9300-XCTL.
001432
001433 9300-XCTL             SECTION.
001434     
      * EXEC CICS XCTL
001435*        PROGRAM    (PGM-NAME)
001436*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
001437*        LENGTH     (PI-COMM-LENGTH)
001438*    END-EXEC.
      *    MOVE '.$C                   %   #00004835' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034383335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001439
001440 9400-CLEAR            SECTION.
001441     MOVE ' '                    TO  PI-PEND-SW.
001442     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
001443     GO TO 9300-XCTL.
001444
001445 9500-PF12             SECTION.
001446     MOVE XCTL-010               TO  PGM-NAME.
001447     GO TO 9300-XCTL.
001448
001449 9600-PGMID-ERROR      SECTION.
001450     
      * EXEC CICS HANDLE CONDITION
001451*        PGMIDERR    (8300-SEND-TEXT)
001452*    END-EXEC.
      *    MOVE '"$L                   ! ( #00004851' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303034383531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001453     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
001454     MOVE ' '                    TO  PI-ENTRY-CD-1.
001455     MOVE XCTL-005               TO  PGM-NAME.
001456     MOVE PGM-NAME               TO  LOGOFF-PGM.
001457     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
001458     GO TO 9300-XCTL.
001459
001460 9700-DATE-LINK         SECTION.
001461     MOVE LINK-ELDATCV           TO  PGM-NAME
001462     
      * EXEC CICS LINK
001463*        PROGRAM    (PGM-NAME)
001464*        COMMAREA   (DATE-CONVERSION-DATA)
001465*        LENGTH     (DC-COMM-LENGTH)
001466*    END-EXEC.
      *    MOVE '."C                   (   #00004863' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001467
001468
001469 9900-ERROR-FORMAT       SECTION.
001470     IF NOT EMI-ERRORS-COMPLETE
001471         MOVE LINK-001           TO  PGM-NAME
001472         
      * EXEC CICS LINK
001473*            PROGRAM    (PGM-NAME)
001474*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
001475*            LENGTH     (EMI-COMM-LENGTH)
001476*        END-EXEC.
      *    MOVE '."C                   (   #00004873' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001477
001478 9900-EXIT.
001479     EXIT.
001480
001481 9990-ABEND             SECTION.
001482     MOVE LINK-004               TO  PGM-NAME.
001483     MOVE DFHEIBLK               TO  EMI-LINE1.
001484
001485     
      * EXEC CICS LINK
001486*        PROGRAM   (PGM-NAME)
001487*        COMMAREA  (EMI-LINE1)
001488*        LENGTH    (72)
001489*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004886' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001490
001491     MOVE -1                     TO  GMAINTL.
001492
001493     GO TO 8200-SEND-DATAONLY.
001494
001495 9995-SECURITY-VIOLATION.
001496*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00004915' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034393135' TO DFHEIV0
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
001497
001498 9995-EXIT.
001499     EXIT.
001500

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1277' TO DFHEIV1
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
               GO TO 4400-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 6250-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6350-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6450-DUPLICATE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6650-RECORD-PREVS-DELETED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1277' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
