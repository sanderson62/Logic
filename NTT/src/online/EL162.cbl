      *((program: EL162.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL162 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 05/02/95 14:07:28.
000007*                            VMOD=2.015
000008*
000009*AUTHOR.     LOGIC,INC.
000010*            DALLAS, TEXAS.
000011
000012*REMARKS.    TRANSACTION - EX17 - RECORD MAIL RECEIVED
000013*            USED TO REVIEW PENDING MAIL AND TO RECORD
000014*            INCOMING MAIL.
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000024* 121406    2006081100001  PEMA  ADD RECEIVE DATE EDIT
000025* 051107    2006052500001  AJRA  POPULATE UBY WITH USER ID
000026* 100610    2009122800001  AJRA  BYPASS LETTER W/ RESEND PRINTED D
000027* 102610    2009122800001  AJRA    REPLACE VERIFY W/ STOP DATE
000028* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000029* 010416    2015072900002  TANA  ADD PF7 TO CLAIM MEMO SCREEN
000030* 062217    2017050300002  TANA  ADD AUTH RCVD FIELD
000031* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000032* 022619  CR2019021100002  PEMA  ADD EDIT TO RECEIVE DATE
000033* 080322  CR2021100800003  TANA  Add B and H claim types
000034******************************************************************
000035
000036     EJECT
000037 ENVIRONMENT DIVISION.
000038 DATA DIVISION.
000039 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000040 77  FILLER  PIC X(32)  VALUE '********************************'.
000041 77  FILLER  PIC X(32)  VALUE '*    EL162 WORKING STORAGE     *'.
000042 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.015 *********'.
000043
000044*                            COPY ELCSCTM.
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
000045
000046*                            COPY ELCSCRTY.
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
000047
000048 01  WS-DATE-AREA.
000049     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000050     05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
000051     05  CURRENT-MINUS1-SAVE PIC XX      VALUE SPACES.
000052
000053 01  ws-response                 PIC S9(8)   COMP.
000054     88  resp-normal                  VALUE +00.
000055     88  resp-notfnd                  VALUE +13.
000056     88  resp-duprec                  value +14.
000057     88  resp-dupkey                  value +15.
000058     88  resp-notopen                 VALUE +19.
000059     88  resp-endfile                 VALUE +20.
000060
000061 01  STANDARD-AREAS.
000062     12  SC-ITEM                 PIC S9(4)   VALUE +1  COMP.
000063     12  MAP-NAME.
000064         16  MAP-PRE             PIC XX      VALUE 'EL'.
000065         16  MAP-NUMBER          PIC X(4)    VALUE '162A'.
000066         16  FILLER              PIC XX      VALUE SPACES.
000067     12  MAPSET-NAME             PIC X(8)    VALUE 'EL162S'.
000068     12  GETMAIN-SPACE           PIC X       VALUE SPACE.
000069     12  TRANS-ID                PIC X(4)    VALUE 'EX17'.
000070     12  PGM-NAME                PIC X(8).
000071     12  TIME-IN                 PIC S9(7).
000072     12  TIME-OUT-R  REDEFINES TIME-IN.
000073         16  FILLER              PIC X.
000074         16  TIME-OUT            PIC 99V99.
000075         16  FILLER              PIC XX.
000076     12  XCTL-005                PIC X(5)    VALUE 'EL005'.
000077     12  XCTL-010                PIC X(5)    VALUE 'EL010'.
000078     12  XCTL-126                PIC X(5)    VALUE 'EL126'.
000079     12  LINK-001                PIC X(5)    VALUE 'EL001'.
000080     12  LINK-004                PIC X(5)    VALUE 'EL004'.
000081     12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.
000082     12  THIS-PGM                PIC X(8)    VALUE 'EL162'.
000083     12  PGM-EL141               PIC X(8)    VALUE 'EL141'.
000084     12  PGM-EL132               PIC X(8)    VALUE 'EL132'.
000085     12  PGM-EL150               PIC X(8)    VALUE 'EL150'.
000086     12  PGM-EL1284              PIC X(8)    VALUE 'EL1284'.
000087     12  ELMSTR-FILE-ID          PIC X(8)    VALUE 'ELMSTR'.
000088     12  ELTRLR-FILE-ID          PIC X(8)    VALUE 'ELTRLR'.
000089     12  ERACCT-FILE-ID          PIC X(8)    VALUE 'ERACCT'.
000090     12  SUB                     PIC S999    COMP-3.
000091     EJECT
000092 01  ERROR-MESSAGES.
000093     12  ER-ZEROS                PIC X(4)    VALUE '0000'.
000094     12  ER-0004                 PIC X(4)    VALUE '0004'.
000095     12  ER-0008                 PIC X(4)    VALUE '0008'.
000096     12  ER-0029                 PIC X(4)    VALUE '0029'.
000097     12  ER-0050                 PIC X(4)    VALUE '0050'.
000098     12  ER-0021                 PIC X(4)    VALUE '0021'.
000099     12  ER-0042                 PIC X(4)    VALUE '0042'.
000100     12  ER-0066                 PIC X(4)    VALUE '0066'.
000101     12  ER-0067                 PIC X(4)    VALUE '0067'.
000102     12  ER-0070                 PIC X(4)    VALUE '0070'.
000103     12  ER-0133                 PIC X(4)    VALUE '0133'.
000104     12  ER-0168                 PIC X(4)    VALUE '0168'.
000105     12  ER-0172                 PIC X(4)    VALUE '0172'.
000106     12  ER-0179                 PIC X(4)    VALUE '0179'.
000107     12  ER-0287                 PIC X(4)    VALUE '0287'.
000108     12  ER-0318                 PIC X(4)    VALUE '0318'.
000109     12  ER-0319                 PIC X(4)    VALUE '0319'.
000110     12  ER-0320                 PIC X(4)    VALUE '0320'.
000111     12  ER-0321                 PIC X(4)    VALUE '0321'.
000112     12  ER-0322                 PIC X(4)    VALUE '0322'.
000113     12  ER-0514                 PIC X(4)    VALUE '0514'.
000114     12  ER-0539                 PIC X(4)    VALUE '0539'.
000115     12  ER-0895                 PIC X(4)    VALUE '0895'.
000116     12  ER-0896                 PIC X(4)    VALUE '0896'.
000117     12  er-1825                 pic x(4)    value '1825'.
000118     12  ER-7839                 PIC X(4)    VALUE '7839'.
000119
000120     12  BIN-CURRENT-DATE        PIC XX.
000121     12  INDX-WORK               PIC 9(4).
000122     12  REC-DATA-SW             PIC X       VALUE ' '.
000123         88  REC-DATA                        VALUE '1'.
000124     12  VER-DATA-SW             PIC X       VALUE ' '.
000125         88  VER-DATA                        VALUE '1'.
000126     12  UNSOL-DATA-SW           PIC X       VALUE ' '.
000127         88  UNSOL-DATA                      VALUE '1'.
000128         88  FILE-DATA                       VALUE '2'.
000129     12  ADDR-TYPE               PIC X.
000130     12  ADDR-SEQ                PIC S9(4) COMP.
000131     12  ACTV-LENGTH             PIC S9(4) COMP VALUE +200.
000132     12  USENT-SAVE              PIC XX    VALUE LOW-VALUES.
000133     12  URECDTE-SAVE            PIC XX    VALUE LOW-VALUES.
000134     12  REC-DATE-SAVE OCCURS 4 TIMES INDEXED BY INDX     PIC XX.
000135     12  STOP-DATE-SAVE OCCURS 4 TIMES INDEXED BY INDX2   PIC XX.
000136     12  DEEDIT-FIELD            PIC X(15).
000137     12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).
000138
000139     12  ACCT-KEY.
000140         16  ACCT-PARTIAL-KEY.
000141             20  ACCT-CO         PIC X.
000142             20  ACCT-CARRIER    PIC X.
000143             20  ACCT-GROUPING   PIC X(6).
000144             20  ACCT-STATE      PIC XX.
000145             20  ACCT-ACCOUNT    PIC X(10).
000146         16  ACCT-EXP-DATE       PIC XX.
000147     12  FILLER                  PIC S9(9)  VALUE +0 COMP.
000148     12  ACCT-SAVE-KEY           PIC X(20).
000149     12  WS-ZIP.
000150         16  WS-ZIP-CODE         PIC X(5).
000151         16  WS-DASH             PIC X     VALUE '-'.
000152         16  WS-ZIP-PLUS4        PIC X(4).
000153     12  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.
000154         16  WS-CAN-POSTAL-CD-1  PIC X(3).
000155         16  WS-DASH-CAN         PIC X.
000156         16  WS-CAN-POSTAL-CD-2  PIC X(3).
000157         16  WS-CAN-FILLER       PIC X(3).
000158     12  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.
000159     12  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
000160                                 PIC 9(10).
000161     12  WS-REASON               PIC X(70).
000162
000163     EJECT
000164*                            COPY ELCDATE.
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
000165
000166     EJECT
000167*                            COPY ELCLOGOF.
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
000168
000169     EJECT
000170*                            COPY ELCATTR.
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
000171
000172     EJECT
000173*                            COPY ELCEMIB.
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
000174     EJECT
000175*                            COPY ELCINTF.
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
000176     12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
000177         16  PI-NEXT-TRLR-SUB    PIC S999  COMP-3.
000178         16  PI-ACTV-SAVE        PIC X(20).
000179         16  PI-ACTV-SAVE-SEQ    PIC S9(04) COMP.
000180         16  PI-ACTV-KEY.
000181           18  PI-ACTV-PARTIAL.
000182             20  PI-ACTV-COMP-CD     PIC X.
000183             20  PI-ACTV-CARRIER     PIC X.
000184             20  PI-ACTV-CLAIM       PIC X(7).
000185             20  PI-ACTV-CERT-NO.
000186                 22  PI-ACTV-CERT-NO-PRIME PIC X(10).
000187                 22  PI-ACTV-CERT-NO-SUFX  PIC X.
000188           18  PI-ACTV-SEQ           PIC S9(4) COMP.
000189
000190         16  PI-DISPLAYED-TRAILER-CODES.
000191             18  PI-TRLRS OCCURS 50 TIMES    PIC S9(4) COMP.
000192         16  PI-CLAM-KEY.
000193             20  PI-CLAM-COMP-CD     PIC X.
000194             20  PI-CLAM-CARRIER     PIC X.
000195             20  PI-CLAM-CLAIM       PIC X(7).
000196             20  PI-CLAM-CERT-NO.
000197                 24  PI-CLAM-CERT-NO-PRIME  PIC X(10).
000198                 24  PI-CLAM-CERT-NO-SUFX   PIC X.
000199         16  PI-DMD-FORCE-ERROR      PIC X.
000200             88  PI-DMD-FORCED             VALUE 'X'.
000201         16  pi-receive-dt-cnt       pic 9.
000202         16  pi-prev-rec-dt          pic xx.
000203         16  FILLER                  PIC X(470).
000204
000205     EJECT
000206*                            COPY ELCAID.
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
000207 01  FILLER    REDEFINES DFHAID.
000208     12  FILLER              PIC X(8).
000209     12  PF-VALUES           PIC X       OCCURS 2.
000210
000211     EJECT
000212*                            COPY EL162S.
      *>>((file: EL162S))
000001 01  EL162AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  RUNDTEL PIC S9(0004) COMP.
000005     05  RUNDTEF PIC  X(0001).
000006     05  FILLER REDEFINES RUNDTEF.
000007         10  RUNDTEA PIC  X(0001).
000008     05  RUNDTEI PIC  X(0008).
000009*    -------------------------------
000010     05  RUNTIMEL PIC S9(0004) COMP.
000011     05  RUNTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES RUNTIMEF.
000013         10  RUNTIMEA PIC  X(0001).
000014     05  RUNTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  CLAIMNOL PIC S9(0004) COMP.
000017     05  CLAIMNOF PIC  X(0001).
000018     05  FILLER REDEFINES CLAIMNOF.
000019         10  CLAIMNOA PIC  X(0001).
000020     05  CLAIMNOI PIC  X(0007).
000021*    -------------------------------
000022     05  TYPEL PIC S9(0004) COMP.
000023     05  TYPEF PIC  X(0001).
000024     05  FILLER REDEFINES TYPEF.
000025         10  TYPEA PIC  X(0001).
000026     05  TYPEI PIC  X(0006).
000027*    -------------------------------
000028     05  CARRL PIC S9(0004) COMP.
000029     05  CARRF PIC  X(0001).
000030     05  FILLER REDEFINES CARRF.
000031         10  CARRA PIC  X(0001).
000032     05  CARRI PIC  X(0001).
000033*    -------------------------------
000034     05  CERTNOL PIC S9(0004) COMP.
000035     05  CERTNOF PIC  X(0001).
000036     05  FILLER REDEFINES CERTNOF.
000037         10  CERTNOA PIC  X(0001).
000038     05  CERTNOI PIC  X(0011).
000039*    -------------------------------
000040     05  LASTNMEL PIC S9(0004) COMP.
000041     05  LASTNMEF PIC  X(0001).
000042     05  FILLER REDEFINES LASTNMEF.
000043         10  LASTNMEA PIC  X(0001).
000044     05  LASTNMEI PIC  X(0015).
000045*    -------------------------------
000046     05  FILEL PIC S9(0004) COMP.
000047     05  FILEF PIC  X(0001).
000048     05  FILLER REDEFINES FILEF.
000049         10  FILEA PIC  X(0001).
000050     05  FILEI PIC  X(0004).
000051*    -------------------------------
000052     05  RECDTE1L PIC S9(0004) COMP.
000053     05  RECDTE1F PIC  X(0001).
000054     05  FILLER REDEFINES RECDTE1F.
000055         10  RECDTE1A PIC  X(0001).
000056     05  RECDTE1I PIC  X(0008).
000057*    -------------------------------
000058     05  STOPDT1L PIC S9(0004) COMP.
000059     05  STOPDT1F PIC  X(0001).
000060     05  FILLER REDEFINES STOPDT1F.
000061         10  STOPDT1A PIC  X(0001).
000062     05  STOPDT1I PIC  X(0008).
000063*    -------------------------------
000064     05  SENT1L PIC S9(0004) COMP.
000065     05  SENT1F PIC  X(0001).
000066     05  FILLER REDEFINES SENT1F.
000067         10  SENT1A PIC  X(0001).
000068     05  SENT1I PIC  X(0008).
000069*    -------------------------------
000070     05  BY1L PIC S9(0004) COMP.
000071     05  BY1F PIC  X(0001).
000072     05  FILLER REDEFINES BY1F.
000073         10  BY1A PIC  X(0001).
000074     05  BY1I PIC  X(0004).
000075*    -------------------------------
000076     05  RESEND1L PIC S9(0004) COMP.
000077     05  RESEND1F PIC  X(0001).
000078     05  FILLER REDEFINES RESEND1F.
000079         10  RESEND1A PIC  X(0001).
000080     05  RESEND1I PIC  X(0008).
000081*    -------------------------------
000082     05  FOLLOW1L PIC S9(0004) COMP.
000083     05  FOLLOW1F PIC  X(0001).
000084     05  FILLER REDEFINES FOLLOW1F.
000085         10  FOLLOW1A PIC  X(0001).
000086     05  FOLLOW1I PIC  X(0008).
000087*    -------------------------------
000088     05  FORM1L PIC S9(0004) COMP.
000089     05  FORM1F PIC  X(0001).
000090     05  FILLER REDEFINES FORM1F.
000091         10  FORM1A PIC  X(0001).
000092     05  FORM1I PIC  X(0004).
000093*    -------------------------------
000094     05  TO1L PIC S9(0004) COMP.
000095     05  TO1F PIC  X(0001).
000096     05  FILLER REDEFINES TO1F.
000097         10  TO1A PIC  X(0001).
000098     05  TO1I PIC  X(0008).
000099*    -------------------------------
000100     05  ARCH1L PIC S9(0004) COMP.
000101     05  ARCH1F PIC  X(0001).
000102     05  FILLER REDEFINES ARCH1F.
000103         10  ARCH1A PIC  X(0001).
000104     05  ARCH1I PIC  X(0008).
000105*    -------------------------------
000106     05  TRLR1L PIC S9(0004) COMP.
000107     05  TRLR1F PIC  X(0001).
000108     05  FILLER REDEFINES TRLR1F.
000109         10  TRLR1A PIC  X(0001).
000110     05  TRLR1I PIC  X(0004).
000111*    -------------------------------
000112     05  TYPE1L PIC S9(0004) COMP.
000113     05  TYPE1F PIC  X(0001).
000114     05  FILLER REDEFINES TYPE1F.
000115         10  TYPE1A PIC  X(0001).
000116     05  TYPE1I PIC  X(0001).
000117*    -------------------------------
000118     05  RE1L PIC S9(0004) COMP.
000119     05  RE1F PIC  X(0001).
000120     05  FILLER REDEFINES RE1F.
000121         10  RE1A PIC  X(0001).
000122     05  RE1I PIC  X(0070).
000123*    -------------------------------
000124     05  RECDTE2L PIC S9(0004) COMP.
000125     05  RECDTE2F PIC  X(0001).
000126     05  FILLER REDEFINES RECDTE2F.
000127         10  RECDTE2A PIC  X(0001).
000128     05  RECDTE2I PIC  X(0008).
000129*    -------------------------------
000130     05  STOPDT2L PIC S9(0004) COMP.
000131     05  STOPDT2F PIC  X(0001).
000132     05  FILLER REDEFINES STOPDT2F.
000133         10  STOPDT2A PIC  X(0001).
000134     05  STOPDT2I PIC  X(0008).
000135*    -------------------------------
000136     05  SENT2L PIC S9(0004) COMP.
000137     05  SENT2F PIC  X(0001).
000138     05  FILLER REDEFINES SENT2F.
000139         10  SENT2A PIC  X(0001).
000140     05  SENT2I PIC  X(0008).
000141*    -------------------------------
000142     05  BY2L PIC S9(0004) COMP.
000143     05  BY2F PIC  X(0001).
000144     05  FILLER REDEFINES BY2F.
000145         10  BY2A PIC  X(0001).
000146     05  BY2I PIC  X(0004).
000147*    -------------------------------
000148     05  RESEND2L PIC S9(0004) COMP.
000149     05  RESEND2F PIC  X(0001).
000150     05  FILLER REDEFINES RESEND2F.
000151         10  RESEND2A PIC  X(0001).
000152     05  RESEND2I PIC  X(0008).
000153*    -------------------------------
000154     05  FOLLOW2L PIC S9(0004) COMP.
000155     05  FOLLOW2F PIC  X(0001).
000156     05  FILLER REDEFINES FOLLOW2F.
000157         10  FOLLOW2A PIC  X(0001).
000158     05  FOLLOW2I PIC  X(0008).
000159*    -------------------------------
000160     05  FORM2L PIC S9(0004) COMP.
000161     05  FORM2F PIC  X(0001).
000162     05  FILLER REDEFINES FORM2F.
000163         10  FORM2A PIC  X(0001).
000164     05  FORM2I PIC  X(0004).
000165*    -------------------------------
000166     05  TO2L PIC S9(0004) COMP.
000167     05  TO2F PIC  X(0001).
000168     05  FILLER REDEFINES TO2F.
000169         10  TO2A PIC  X(0001).
000170     05  TO2I PIC  X(0008).
000171*    -------------------------------
000172     05  ARCH2L PIC S9(0004) COMP.
000173     05  ARCH2F PIC  X(0001).
000174     05  FILLER REDEFINES ARCH2F.
000175         10  ARCH2A PIC  X(0001).
000176     05  ARCH2I PIC  X(0008).
000177*    -------------------------------
000178     05  TRLR2L PIC S9(0004) COMP.
000179     05  TRLR2F PIC  X(0001).
000180     05  FILLER REDEFINES TRLR2F.
000181         10  TRLR2A PIC  X(0001).
000182     05  TRLR2I PIC  X(0004).
000183*    -------------------------------
000184     05  TYPE2L PIC S9(0004) COMP.
000185     05  TYPE2F PIC  X(0001).
000186     05  FILLER REDEFINES TYPE2F.
000187         10  TYPE2A PIC  X(0001).
000188     05  TYPE2I PIC  X(0001).
000189*    -------------------------------
000190     05  RE2L PIC S9(0004) COMP.
000191     05  RE2F PIC  X(0001).
000192     05  FILLER REDEFINES RE2F.
000193         10  RE2A PIC  X(0001).
000194     05  RE2I PIC  X(0070).
000195*    -------------------------------
000196     05  RECDTE3L PIC S9(0004) COMP.
000197     05  RECDTE3F PIC  X(0001).
000198     05  FILLER REDEFINES RECDTE3F.
000199         10  RECDTE3A PIC  X(0001).
000200     05  RECDTE3I PIC  X(0008).
000201*    -------------------------------
000202     05  STOPDT3L PIC S9(0004) COMP.
000203     05  STOPDT3F PIC  X(0001).
000204     05  FILLER REDEFINES STOPDT3F.
000205         10  STOPDT3A PIC  X(0001).
000206     05  STOPDT3I PIC  X(0008).
000207*    -------------------------------
000208     05  SENT3L PIC S9(0004) COMP.
000209     05  SENT3F PIC  X(0001).
000210     05  FILLER REDEFINES SENT3F.
000211         10  SENT3A PIC  X(0001).
000212     05  SENT3I PIC  X(0008).
000213*    -------------------------------
000214     05  BY3L PIC S9(0004) COMP.
000215     05  BY3F PIC  X(0001).
000216     05  FILLER REDEFINES BY3F.
000217         10  BY3A PIC  X(0001).
000218     05  BY3I PIC  X(0004).
000219*    -------------------------------
000220     05  RESEND3L PIC S9(0004) COMP.
000221     05  RESEND3F PIC  X(0001).
000222     05  FILLER REDEFINES RESEND3F.
000223         10  RESEND3A PIC  X(0001).
000224     05  RESEND3I PIC  X(0008).
000225*    -------------------------------
000226     05  FOLLOW3L PIC S9(0004) COMP.
000227     05  FOLLOW3F PIC  X(0001).
000228     05  FILLER REDEFINES FOLLOW3F.
000229         10  FOLLOW3A PIC  X(0001).
000230     05  FOLLOW3I PIC  X(0008).
000231*    -------------------------------
000232     05  FORM3L PIC S9(0004) COMP.
000233     05  FORM3F PIC  X(0001).
000234     05  FILLER REDEFINES FORM3F.
000235         10  FORM3A PIC  X(0001).
000236     05  FORM3I PIC  X(0004).
000237*    -------------------------------
000238     05  TO3L PIC S9(0004) COMP.
000239     05  TO3F PIC  X(0001).
000240     05  FILLER REDEFINES TO3F.
000241         10  TO3A PIC  X(0001).
000242     05  TO3I PIC  X(0008).
000243*    -------------------------------
000244     05  ARCH3L PIC S9(0004) COMP.
000245     05  ARCH3F PIC  X(0001).
000246     05  FILLER REDEFINES ARCH3F.
000247         10  ARCH3A PIC  X(0001).
000248     05  ARCH3I PIC  X(0008).
000249*    -------------------------------
000250     05  TRLR3L PIC S9(0004) COMP.
000251     05  TRLR3F PIC  X(0001).
000252     05  FILLER REDEFINES TRLR3F.
000253         10  TRLR3A PIC  X(0001).
000254     05  TRLR3I PIC  X(0004).
000255*    -------------------------------
000256     05  TYPE3L PIC S9(0004) COMP.
000257     05  TYPE3F PIC  X(0001).
000258     05  FILLER REDEFINES TYPE3F.
000259         10  TYPE3A PIC  X(0001).
000260     05  TYPE3I PIC  X(0001).
000261*    -------------------------------
000262     05  RE3L PIC S9(0004) COMP.
000263     05  RE3F PIC  X(0001).
000264     05  FILLER REDEFINES RE3F.
000265         10  RE3A PIC  X(0001).
000266     05  RE3I PIC  X(0070).
000267*    -------------------------------
000268     05  RECDTE4L PIC S9(0004) COMP.
000269     05  RECDTE4F PIC  X(0001).
000270     05  FILLER REDEFINES RECDTE4F.
000271         10  RECDTE4A PIC  X(0001).
000272     05  RECDTE4I PIC  X(0008).
000273*    -------------------------------
000274     05  STOPDT4L PIC S9(0004) COMP.
000275     05  STOPDT4F PIC  X(0001).
000276     05  FILLER REDEFINES STOPDT4F.
000277         10  STOPDT4A PIC  X(0001).
000278     05  STOPDT4I PIC  X(0008).
000279*    -------------------------------
000280     05  SENT4L PIC S9(0004) COMP.
000281     05  SENT4F PIC  X(0001).
000282     05  FILLER REDEFINES SENT4F.
000283         10  SENT4A PIC  X(0001).
000284     05  SENT4I PIC  X(0008).
000285*    -------------------------------
000286     05  BY4L PIC S9(0004) COMP.
000287     05  BY4F PIC  X(0001).
000288     05  FILLER REDEFINES BY4F.
000289         10  BY4A PIC  X(0001).
000290     05  BY4I PIC  X(0004).
000291*    -------------------------------
000292     05  RESEND4L PIC S9(0004) COMP.
000293     05  RESEND4F PIC  X(0001).
000294     05  FILLER REDEFINES RESEND4F.
000295         10  RESEND4A PIC  X(0001).
000296     05  RESEND4I PIC  X(0008).
000297*    -------------------------------
000298     05  FOLLOW4L PIC S9(0004) COMP.
000299     05  FOLLOW4F PIC  X(0001).
000300     05  FILLER REDEFINES FOLLOW4F.
000301         10  FOLLOW4A PIC  X(0001).
000302     05  FOLLOW4I PIC  X(0008).
000303*    -------------------------------
000304     05  FORM4L PIC S9(0004) COMP.
000305     05  FORM4F PIC  X(0001).
000306     05  FILLER REDEFINES FORM4F.
000307         10  FORM4A PIC  X(0001).
000308     05  FORM4I PIC  X(0004).
000309*    -------------------------------
000310     05  TO4L PIC S9(0004) COMP.
000311     05  TO4F PIC  X(0001).
000312     05  FILLER REDEFINES TO4F.
000313         10  TO4A PIC  X(0001).
000314     05  TO4I PIC  X(0008).
000315*    -------------------------------
000316     05  ARCH4L PIC S9(0004) COMP.
000317     05  ARCH4F PIC  X(0001).
000318     05  FILLER REDEFINES ARCH4F.
000319         10  ARCH4A PIC  X(0001).
000320     05  ARCH4I PIC  X(0008).
000321*    -------------------------------
000322     05  TRLR4L PIC S9(0004) COMP.
000323     05  TRLR4F PIC  X(0001).
000324     05  FILLER REDEFINES TRLR4F.
000325         10  TRLR4A PIC  X(0001).
000326     05  TRLR4I PIC  X(0004).
000327*    -------------------------------
000328     05  TYPE4L PIC S9(0004) COMP.
000329     05  TYPE4F PIC  X(0001).
000330     05  FILLER REDEFINES TYPE4F.
000331         10  TYPE4A PIC  X(0001).
000332     05  TYPE4I PIC  X(0001).
000333*    -------------------------------
000334     05  RE4L PIC S9(0004) COMP.
000335     05  RE4F PIC  X(0001).
000336     05  FILLER REDEFINES RE4F.
000337         10  RE4A PIC  X(0001).
000338     05  RE4I PIC  X(0070).
000339*    -------------------------------
000340     05  URECDTEL PIC S9(0004) COMP.
000341     05  URECDTEF PIC  X(0001).
000342     05  FILLER REDEFINES URECDTEF.
000343         10  URECDTEA PIC  X(0001).
000344     05  URECDTEI PIC  X(0008).
000345*    -------------------------------
000346     05  USENTL PIC S9(0004) COMP.
000347     05  USENTF PIC  X(0001).
000348     05  FILLER REDEFINES USENTF.
000349         10  USENTA PIC  X(0001).
000350     05  USENTI PIC  X(0008).
000351*    -------------------------------
000352     05  UBYL PIC S9(0004) COMP.
000353     05  UBYF PIC  X(0001).
000354     05  FILLER REDEFINES UBYF.
000355         10  UBYA PIC  X(0001).
000356     05  UBYI PIC  X(0004).
000357*    -------------------------------
000358     05  UFORML PIC S9(0004) COMP.
000359     05  UFORMF PIC  X(0001).
000360     05  FILLER REDEFINES UFORMF.
000361         10  UFORMA PIC  X(0001).
000362     05  UFORMI PIC  X(0004).
000363*    -------------------------------
000364     05  AUTHRCVL PIC S9(0004) COMP.
000365     05  AUTHRCVF PIC  X(0001).
000366     05  FILLER REDEFINES AUTHRCVF.
000367         10  AUTHRCVA PIC  X(0001).
000368     05  AUTHRCVI PIC  X(0001).
000369*    -------------------------------
000370     05  UREL PIC S9(0004) COMP.
000371     05  UREF PIC  X(0001).
000372     05  FILLER REDEFINES UREF.
000373         10  UREA PIC  X(0001).
000374     05  UREI PIC  X(0070).
000375*    -------------------------------
000376     05  ADDR1L PIC S9(0004) COMP.
000377     05  ADDR1F PIC  X(0001).
000378     05  FILLER REDEFINES ADDR1F.
000379         10  ADDR1A PIC  X(0001).
000380     05  ADDR1I PIC  X(0030).
000381*    -------------------------------
000382     05  ADDR2L PIC S9(0004) COMP.
000383     05  ADDR2F PIC  X(0001).
000384     05  FILLER REDEFINES ADDR2F.
000385         10  ADDR2A PIC  X(0001).
000386     05  ADDR2I PIC  X(0030).
000387*    -------------------------------
000388     05  ADDR3L PIC S9(0004) COMP.
000389     05  ADDR3F PIC  X(0001).
000390     05  FILLER REDEFINES ADDR3F.
000391         10  ADDR3A PIC  X(0001).
000392     05  ADDR3I PIC  X(0030).
000393*    -------------------------------
000394     05  ADDR4L PIC S9(0004) COMP.
000395     05  ADDR4F PIC  X(0001).
000396     05  FILLER REDEFINES ADDR4F.
000397         10  ADDR4A PIC  X(0001).
000398     05  ADDR4I PIC  X(0030).
000399*    -------------------------------
000400     05  ZIPL PIC S9(0004) COMP.
000401     05  ZIPF PIC  X(0001).
000402     05  FILLER REDEFINES ZIPF.
000403         10  ZIPA PIC  X(0001).
000404     05  ZIPI PIC  X(0010).
000405*    -------------------------------
000406     05  PHONEL PIC S9(0004) COMP.
000407     05  PHONEF PIC  X(0001).
000408     05  FILLER REDEFINES PHONEF.
000409         10  PHONEA PIC  X(0001).
000410     05  PHONEI PIC  X(0012).
000411*    -------------------------------
000412     05  TRLRL PIC S9(0004) COMP.
000413     05  TRLRF PIC  X(0001).
000414     05  FILLER REDEFINES TRLRF.
000415         10  TRLRA PIC  X(0001).
000416     05  TRLRI PIC  X(0004).
000417*    -------------------------------
000418     05  ERRMSG1L PIC S9(0004) COMP.
000419     05  ERRMSG1F PIC  X(0001).
000420     05  FILLER REDEFINES ERRMSG1F.
000421         10  ERRMSG1A PIC  X(0001).
000422     05  ERRMSG1I PIC  X(0072).
000423*    -------------------------------
000424     05  ENTERPFL PIC S9(0004) COMP.
000425     05  ENTERPFF PIC  X(0001).
000426     05  FILLER REDEFINES ENTERPFF.
000427         10  ENTERPFA PIC  X(0001).
000428     05  ENTERPFI PIC  99.
000429*    -------------------------------
000430     05  PF4KEYL PIC S9(0004) COMP.
000431     05  PF4KEYF PIC  X(0001).
000432     05  FILLER REDEFINES PF4KEYF.
000433         10  PF4KEYA PIC  X(0001).
000434     05  PF4KEYI PIC  X(0015).
000435*    -------------------------------
000436     05  PF5KEYL PIC S9(0004) COMP.
000437     05  PF5KEYF PIC  X(0001).
000438     05  FILLER REDEFINES PF5KEYF.
000439         10  PF5KEYA PIC  X(0001).
000440     05  PF5KEYI PIC  X(0014).
000441 01  EL162AO REDEFINES EL162AI.
000442     05  FILLER            PIC  X(0012).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  RUNDTEO PIC  X(0008).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  RUNTIMEO PIC  99.99.
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  CLAIMNOO PIC  X(0007).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  TYPEO PIC  X(0006).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  CARRO PIC  X(0001).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  CERTNOO PIC  X(0011).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  LASTNMEO PIC  X(0015).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  FILEO PIC  X(0004).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  RECDTE1O PIC  X(0008).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  STOPDT1O PIC  X(0008).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  SENT1O PIC  X(0008).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  BY1O PIC  X(0004).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  RESEND1O PIC  X(0008).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  FOLLOW1O PIC  X(0008).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  FORM1O PIC  X(0004).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  TO1O PIC  X(0008).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  ARCH1O PIC  X(0008).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  TRLR1O PIC  9999.
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  TYPE1O PIC  X(0001).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  RE1O PIC  X(0070).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  RECDTE2O PIC  X(0008).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  STOPDT2O PIC  X(0008).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  SENT2O PIC  X(0008).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  BY2O PIC  X(0004).
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  RESEND2O PIC  X(0008).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  FOLLOW2O PIC  X(0008).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  FORM2O PIC  X(0004).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  TO2O PIC  X(0008).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  ARCH2O PIC  X(0008).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  TRLR2O PIC  9999.
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  TYPE2O PIC  X(0001).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  RE2O PIC  X(0070).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  RECDTE3O PIC  X(0008).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  STOPDT3O PIC  X(0008).
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  SENT3O PIC  X(0008).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  BY3O PIC  X(0004).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  RESEND3O PIC  X(0008).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  FOLLOW3O PIC  X(0008).
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  FORM3O PIC  X(0004).
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  TO3O PIC  X(0008).
000563*    -------------------------------
000564     05  FILLER            PIC  X(0003).
000565     05  ARCH3O PIC  X(0008).
000566*    -------------------------------
000567     05  FILLER            PIC  X(0003).
000568     05  TRLR3O PIC  9999.
000569*    -------------------------------
000570     05  FILLER            PIC  X(0003).
000571     05  TYPE3O PIC  X(0001).
000572*    -------------------------------
000573     05  FILLER            PIC  X(0003).
000574     05  RE3O PIC  X(0070).
000575*    -------------------------------
000576     05  FILLER            PIC  X(0003).
000577     05  RECDTE4O PIC  X(0008).
000578*    -------------------------------
000579     05  FILLER            PIC  X(0003).
000580     05  STOPDT4O PIC  X(0008).
000581*    -------------------------------
000582     05  FILLER            PIC  X(0003).
000583     05  SENT4O PIC  X(0008).
000584*    -------------------------------
000585     05  FILLER            PIC  X(0003).
000586     05  BY4O PIC  X(0004).
000587*    -------------------------------
000588     05  FILLER            PIC  X(0003).
000589     05  RESEND4O PIC  X(0008).
000590*    -------------------------------
000591     05  FILLER            PIC  X(0003).
000592     05  FOLLOW4O PIC  X(0008).
000593*    -------------------------------
000594     05  FILLER            PIC  X(0003).
000595     05  FORM4O PIC  X(0004).
000596*    -------------------------------
000597     05  FILLER            PIC  X(0003).
000598     05  TO4O PIC  X(0008).
000599*    -------------------------------
000600     05  FILLER            PIC  X(0003).
000601     05  ARCH4O PIC  X(0008).
000602*    -------------------------------
000603     05  FILLER            PIC  X(0003).
000604     05  TRLR4O PIC  9999.
000605*    -------------------------------
000606     05  FILLER            PIC  X(0003).
000607     05  TYPE4O PIC  X(0001).
000608*    -------------------------------
000609     05  FILLER            PIC  X(0003).
000610     05  RE4O PIC  X(0070).
000611*    -------------------------------
000612     05  FILLER            PIC  X(0003).
000613     05  URECDTEO PIC  99B99B99.
000614*    -------------------------------
000615     05  FILLER            PIC  X(0003).
000616     05  USENTO PIC  99B99B99.
000617*    -------------------------------
000618     05  FILLER            PIC  X(0003).
000619     05  UBYO PIC  X(0004).
000620*    -------------------------------
000621     05  FILLER            PIC  X(0003).
000622     05  UFORMO PIC  X(0004).
000623*    -------------------------------
000624     05  FILLER            PIC  X(0003).
000625     05  AUTHRCVO PIC  X(0001).
000626*    -------------------------------
000627     05  FILLER            PIC  X(0003).
000628     05  UREO PIC  X(0070).
000629*    -------------------------------
000630     05  FILLER            PIC  X(0003).
000631     05  ADDR1O PIC  X(0030).
000632*    -------------------------------
000633     05  FILLER            PIC  X(0003).
000634     05  ADDR2O PIC  X(0030).
000635*    -------------------------------
000636     05  FILLER            PIC  X(0003).
000637     05  ADDR3O PIC  X(0030).
000638*    -------------------------------
000639     05  FILLER            PIC  X(0003).
000640     05  ADDR4O PIC  X(0030).
000641*    -------------------------------
000642     05  FILLER            PIC  X(0003).
000643     05  ZIPO PIC  X(0010).
000644*    -------------------------------
000645     05  FILLER            PIC  X(0003).
000646     05  PHONEO PIC  999B999B9999.
000647*    -------------------------------
000648     05  FILLER            PIC  X(0003).
000649     05  TRLRO PIC  9999.
000650*    -------------------------------
000651     05  FILLER            PIC  X(0003).
000652     05  ERRMSG1O PIC  X(0072).
000653*    -------------------------------
000654     05  FILLER            PIC  X(0003).
000655     05  ENTERPFO PIC  X(0002).
000656*    -------------------------------
000657     05  FILLER            PIC  X(0003).
000658     05  PF4KEYO PIC  X(0015).
000659*    -------------------------------
000660     05  FILLER            PIC  X(0003).
000661     05  PF5KEYO PIC  X(0014).
000662*    -------------------------------
      *<<((file: EL162S))
000213
000214 01  MAP-REDEF REDEFINES EL162AI.
000215     12  FILLER              PIC X(93).
000216     12  TRL-LINES.
000217       14  TRAILER-LINES  OCCURS 4 TIMES INDEXED BY SC-INDX.
000218         16  SC-RECL         PIC S9(4)  COMP.
000219         16  SC-RECA         PIC X.
000220         16  SC-REC          PIC X(8).
000221         16  SC-REC-O REDEFINES SC-REC PIC 99B99B99.
000222         16  SC-STOPL        PIC S9(4)  COMP.
000223         16  SC-STOPA        PIC X.
000224         16  SC-STOP         PIC X(8).
000225         16  SC-STOP-O REDEFINES SC-STOP PIC 99B99B99.
000226         16  FILLER          PIC X(3).
000227         16  SC-SENT         PIC X(8).
000228         16  FILLER          PIC X(3).
000229         16  SC-BY           PIC X(4).
000230         16  FILLER          PIC X(3).
000231         16  SC-RESEND       PIC X(8).
000232         16  FILLER          PIC X(3).
000233         16  SC-FOLLOWUP     PIC X(8).
000234         16  FILLER          PIC X(3).
000235         16  SC-FORM         PIC X(4).
000236         16  FILLER          PIC X(3).
000237         16  SC-TO           PIC X(8).
000238         16  FILLER          PIC X(3).
000239         16  SC-ARCHIVE      PIC X(8).
000240         16  FILLER          PIC X(3).
000241         16  SC-TRLRNO       PIC 9(4).
000242         16  FILLER          PIC X(3).
000243         16  SC-TYPE         PIC X.
000244         16  FILLER          PIC X(3).
000245         16  SC-REASON       PIC X(70).
000246
000247     EJECT
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
000249
000250 01  DFHCOMMAREA             PIC X(1024).
000251
000252*01 PARMLIST .
000253*    02  FILLER              PIC S9(8)   COMP.
000254*    02  CLAM-POINTER        PIC S9(8)   COMP.
000255*    02  ACTV-POINTER        PIC S9(8)   COMP.
000256*    02  ACCT-POINTER        PIC S9(8)   COMP.
000257
000258     EJECT
000259*                            COPY ELCMSTR.
      *>>((file: ELCMSTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCMSTR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.012                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CLAIM MASTER FILE                         *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 350  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
000013*       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
000014*       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000015*       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
000016*       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
000017*                                                 RKP=75,LEN=21  *
000018*                                                                *
000019*   **** NOTE ****                                               *
000020*             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
000021*             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
000022*                                                                *
000023*   LOG = YES                                                    *
000024*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000034* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
000035* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
000036* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000037* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000038* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
000039* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000040******************************************************************
000041 01  CLAIM-MASTER.
000042     12  CL-RECORD-ID                PIC XX.
000043         88  VALID-CL-ID         VALUE 'CL'.
000044
000045     12  CL-CONTROL-PRIMARY.
000046         16  CL-COMPANY-CD           PIC X.
000047         16  CL-CARRIER              PIC X.
000048         16  CL-CLAIM-NO             PIC X(7).
000049         16  CL-CERT-NO.
000050             20  CL-CERT-PRIME       PIC X(10).
000051             20  CL-CERT-SFX         PIC X.
000052
000053     12  CL-CONTROL-BY-NAME.
000054         16  CL-COMPANY-CD-A1        PIC X.
000055         16  CL-INSURED-LAST-NAME    PIC X(15).
000056         16  CL-INSURED-NAME.
000057             20  CL-INSURED-1ST-NAME PIC X(12).
000058             20  CL-INSURED-MID-INIT PIC X.
000059
000060     12  CL-CONTROL-BY-SSN.
000061         16  CL-COMPANY-CD-A2        PIC X.
000062         16  CL-SOC-SEC-NO.
000063             20  CL-SSN-STATE        PIC XX.
000064             20  CL-SSN-ACCOUNT      PIC X(6).
000065             20  CL-SSN-LN3          PIC X(3).
000066
000067     12  CL-CONTROL-BY-CERT-NO.
000068         16  CL-COMPANY-CD-A4        PIC X.
000069         16  CL-CERT-NO-A4.
000070             20  CL-CERT-A4-PRIME    PIC X(10).
000071             20  CL-CERT-A4-SFX      PIC X.
000072
000073     12  CL-CONTROL-BY-CCN.
000074         16  CL-COMPANY-CD-A5        PIC X.
000075         16  CL-CCN-A5.
000076             20  CL-CCN.
000077                 24  CL-CCN-PREFIX-A5 PIC X(4).
000078                 24  CL-CCN-PRIME-A5 PIC X(12).
000079             20  CL-CCN-FILLER-A5    PIC X(4).
000080
000081     12  CL-INSURED-PROFILE-DATA.
000082         16  CL-INSURED-BIRTH-DT     PIC XX.
000083         16  CL-INSURED-SEX-CD       PIC X.
000084             88  INSURED-IS-MALE        VALUE 'M'.
000085             88  INSURED-IS-FEMALE      VALUE 'F'.
000086             88  INSURED-SEX-UNKNOWN    VALUE ' '.
000087         16  CL-INSURED-OCC-CD       PIC X(6).
000088         16  FILLER                  PIC X(5).
000089
000090     12  CL-PROCESSING-INFO.
000091         16  CL-PROCESSOR-ID         PIC X(4).
000092         16  CL-CLAIM-STATUS         PIC X.
000093             88  CLAIM-IS-OPEN          VALUE 'O'.
000094             88  CLAIM-IS-CLOSED        VALUE 'C'.
000095         16  CL-CLAIM-TYPE           PIC X.
000096*            88  AH-CLAIM               VALUE 'A'.
000097*            88  LIFE-CLAIM             VALUE 'L'.
000098*            88  PROPERTY-CLAIM         VALUE 'P'.
000099*            88  IUI-CLAIM              VALUE 'I'.
000100*            88  GAP-CLAIM              VALUE 'G'.
000101*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
000102*            88  OTHER-CLAIM            VALUE 'O'.
000103         16  CL-CLAIM-PREM-TYPE      PIC X.
000104             88  SINGLE-PREMIUM         VALUE '1'.
000105             88  O-B-COVERAGE           VALUE '2'.
000106             88  OPEN-END-COVERAGE      VALUE '3'.
000107         16  CL-INCURRED-DT          PIC XX.
000108         16  CL-REPORTED-DT          PIC XX.
000109         16  CL-FILE-ESTABLISH-DT    PIC XX.
000110         16  CL-EST-END-OF-DISAB-DT  PIC XX.
000111         16  CL-LAST-PMT-DT          PIC XX.
000112         16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000113         16  CL-PAID-THRU-DT         PIC XX.
000114         16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000115         16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000116         16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000117         16  CL-PMT-CALC-METHOD      PIC X.
000118             88  CL-360-DAY-YR          VALUE '1'.
000119             88  CL-365-DAY-YR          VALUE '2'.
000120             88  CL-FULL-MONTHS         VALUE '3'.
000121         16  CL-CAUSE-CD             PIC X(6).
000122
000123         16  CL-PRIME-CERT-NO.
000124             20  CL-PRIME-CERT-PRIME PIC X(10).
000125             20  CL-PRIME-CERT-SFX   PIC X.
000126
000127         16  CL-SYSTEM-IDENTIFIER    PIC XX.
000128             88  CL-CREDIT-CLAIM        VALUE 'CR'.
000129             88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
000130
000131         16  CL-MICROFILM-NO         PIC X(10).
000132         16  FILLER REDEFINES CL-MICROFILM-NO.
000133             20  CL-BENEFIT-PERIOD   PIC 99.
000134             20  FILLER              PIC X(8).
000135         16  CL-PROG-FORM-TYPE       PIC X.
000136         16  CL-LAST-ADD-ON-DT       PIC XX.
000137
000138         16  CL-LAST-REOPEN-DT       PIC XX.
000139         16  CL-LAST-CLOSE-DT        PIC XX.
000140         16  CL-LAST-CLOSE-REASON    PIC X(01).
000141             88  FINAL-PAID             VALUE '1'.
000142             88  CLAIM-DENIED           VALUE '2'.
000143             88  AUTO-CLOSE             VALUE '3'.
000144             88  MANUAL-CLOSE           VALUE '4'.
000145             88  BENEFITS-CHANGED       VALUE 'C'.
000146             88  SETUP-ERRORS           VALUE 'E'.
000147         16  CL-ASSOC-CERT-SEQU      PIC S99.
000148         16  CL-ASSOC-CERT-TOTAL     PIC S99.
000149         16  CL-CLAIM-PAYMENT-STATUS PIC 9.
000150             88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
000151         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
000152         16  FILLER                  PIC X.
000153
000154     12  CL-CERTIFICATE-DATA.
000155         16  CL-CERT-ORIGIN          PIC X.
000156             88  CERT-WAS-ONLINE        VALUE '1'.
000157             88  CERT-WAS-CREATED       VALUE '2'.
000158             88  COVERAGE-WAS-ADDED     VALUE '3'.
000159         16  CL-CERT-KEY-DATA.
000160             20  CL-CERT-CARRIER     PIC X.
000161             20  CL-CERT-GROUPING    PIC X(6).
000162             20  CL-CERT-STATE       PIC XX.
000163             20  CL-CERT-ACCOUNT.
000164                 24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
000165                 24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
000166             20  CL-CERT-EFF-DT      PIC XX.
000167
000168     12  CL-STATUS-CONTROLS.
000169         16  CL-PRIORITY-CD          PIC X.
000170             88  CONFIDENTIAL-DATA      VALUE '8'.
000171             88  HIGHEST-PRIORITY       VALUE '9'.
000172         16  CL-SUPV-ATTN-CD         PIC X.
000173             88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
000174             88  SUPV-IS-REQUIRED       VALUE 'Y'.
000175         16  CL-PURGED-DT            PIC XX.
000176         16  CL-RESTORED-DT          PIC XX.
000177         16  CL-NEXT-AUTO-PAY-DT     PIC XX.
000178         16  CL-NEXT-RESEND-DT       PIC XX.
000179         16  CL-NEXT-FOLLOWUP-DT     PIC XX.
000180         16  CL-CRITICAL-PERIOD      PIC 99.
000181*        16  FILLER                  PIC XX.
000182         16  CL-LAST-MAINT-DT        PIC XX.
000183         16  CL-LAST-MAINT-USER      PIC X(4).
000184         16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000185         16  CL-LAST-MAINT-TYPE      PIC X.
000186             88  CLAIM-SET-UP           VALUE ' '.
000187             88  PAYMENT-MADE           VALUE '1'.
000188             88  LETTER-SENT            VALUE '2'.
000189             88  MASTER-WAS-ALTERED     VALUE '3'.
000190             88  MASTER-WAS-RESTORED    VALUE '4'.
000191             88  INCURRED-DATE-CHANGED  VALUE '5'.
000192             88  FILE-CONVERTED         VALUE '6'.
000193             88  CHANGE-OF-BENEFITS     VALUE 'C'.
000194             88  ERROR-CORRECTION       VALUE 'E'.
000195         16  CL-RELATED-CLAIM-NO     PIC X(7).
000196         16  CL-HISTORY-ARCHIVE-DT   PIC XX.
000197         16  CL-BENEFICIARY          PIC X(10).
000198         16  CL-FILE-ESTABLISHED-BY  PIC X(4).
000199         16  CL-DENIAL-TYPE          PIC X.
000200             88  CL-TYPE-DENIAL          VALUE '1'.
000201             88  CL-TYPE-RESCISSION      VALUE '2'.
000202             88  CL-TYPE-REFORMATION     VALUE '3'.
000203             88  CL-TYPE-REF-TO-RES      VALUE '4'.
000204             88  CL-TYPE-RECONSIDERED    VALUE '5'.
000205         16  CL-NO-OF-EXTENSIONS     PIC 99.
000206
000207         16  filler                  pic x(3).
000208*        16  CL-CRIT-PER-RECURRENT   PIC X.
000209*        16  CL-CRIT-PER-RTW-MOS     PIC 99.
000210*        16  CL-RTW-DT               PIC XX.
000211
000212     12  CL-TRAILER-CONTROLS.
000213         16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000214             88  CL-1ST-TRL-AVAIL       VALUE +4095.
000215             88  CL-LAST-TRL-AVAIL      VALUE +100.
000216             88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
000217         16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000218         16  FILLER                  PIC XX.
000219         16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000220         16  CL-ADDRESS-TRAILER-CNT.
000221             20  CL-INSURED-ADDR-CNT  PIC S9(1).
000222                 88  NO-INSURED-AVAILABLE    VALUE ZERO.
000223             20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
000224                 88  ACCOUNT-IS-ONLINE       VALUE ZERO.
000225             20  CL-BENIF-ADDR-CNT    PIC S9(1).
000226                 88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
000227             20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
000228                 88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
000229             20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
000230                 88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
000231             20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
000232                 88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
000233             20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
000234                 88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
000235
000236     12  CL-CV-REFERENCE-NO.
000237         16  CL-CV-REFNO-PRIME       PIC X(18).
000238         16  CL-CV-REFNO-SFX         PIC XX.
000239
000240     12  CL-FILE-LOCATION            PIC X(4).
000241
000242     12  CL-PROCESS-ERRORS.
000243         16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000244             88  NO-FATAL-ERRORS        VALUE ZERO.
000245         16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000246             88  NO-FORCABLE-ERRORS     VALUE ZERO.
000247
000248     12  CL-PRODUCT-CD               PIC X.
000249
000250     12  CL-CURRENT-KEY-DATA.
000251         16  CL-CURRENT-CARRIER      PIC X.
000252         16  CL-CURRENT-GROUPING     PIC X(6).
000253         16  CL-CURRENT-STATE        PIC XX.
000254         16  CL-CURRENT-ACCOUNT      PIC X(10).
000255
000256     12  CL-ASSOCIATES               PIC X.
000257         88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
000258         88  CL-ASSOC-INTERFACE         VALUE 'I'.
000259         88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000260         88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
000261
000262     12  CL-ACTIVITY-CODE            PIC 99.
000263     12  CL-ACTIVITY-MAINT-DT        PIC XX.
000264     12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
000265
000266     12  CL-LAPSE-REPORT-CODE        PIC 9.
000267     12  CL-LAG-REPORT-CODE          PIC 9.
000268     12  CL-LOAN-TYPE                PIC XX.
000269     12  CL-LEGAL-STATE              PIC XX.
000270
000271     12  CL-YESNOSW                  PIC X.
000272     12  CL-ACCIDENT-CLAIM-SW        PIC X.
000273         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
000274         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
000275         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
000276     12  cl-insured-type             pic x.
000277         88  cl-claim-on-primary         value 'P'.
000278         88  cl-claim-on-co-borrower     value 'C'.
000279     12  cl-benefit-expiration-dt    PIC XX.
      *<<((file: ELCMSTR))
000260
000261     EJECT
000262*                            COPY ELCTRLR.
      *>>((file: ELCTRLR))
000001******************************************************************
000002*                                                                *
000003*                            ELCTRLR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.014                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 200    RECFORM = FIXED                         *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
000013*       ALTERNATE INDEX = NONE                                   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000026* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
000027* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
000028* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
000029* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
000030* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
000031* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
000032* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
000033* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
000034* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
000035* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
000036* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
000037* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
000038* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
000039* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
000040* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000041* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
000042* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
000043* 040814    2014030500002  AJRA  ADD ICD CODES
000044* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000045* 013017  CR2016053100001  PEMA  ACH PROCESSING
000046* 062217  CR2017050300002  TANA  ADD AUTH RCVD
000047* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000048* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
000049******************************************************************
000050 01  ACTIVITY-TRAILERS.
000051     12  AT-RECORD-ID                    PIC XX.
000052         88  VALID-AT-ID                       VALUE 'AT'.
000053
000054     12  AT-CONTROL-PRIMARY.
000055         16  AT-COMPANY-CD               PIC X.
000056         16  AT-CARRIER                  PIC X.
000057         16  AT-CLAIM-NO                 PIC X(7).
000058         16  AT-CERT-NO.
000059             20  AT-CERT-PRIME           PIC X(10).
000060             20  AT-CERT-SFX             PIC X.
000061         16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
000062             88  AT-1ST-TRL-AVAIL             VALUE +4095.
000063             88  AT-LAST-TRL-AVAIL            VALUE +100.
000064             88  AT-RESV-EXP-HIST-TRL         VALUE +0.
000065             88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
000066             88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
000067             88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
000068             88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
000069             88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
000070             88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
000071             88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
000072             88  AT-DIAGNOSIS-TRL             VALUE +90.
000073             88  AT-BENEFICIARY-TRL           VALUE +91.
000074             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
000075             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
000076             88  AT-VFY-CAUSAL-STATE          VALUE +94.
000077             88  AT-ERROR-MSGS-TRL            VALUE +95.
000078
000079     12  AT-TRAILER-TYPE                 PIC X.
000080         88  RESERVE-EXPENSE-TR               VALUE '1'.
000081         88  PAYMENT-TR                       VALUE '2'.
000082         88  AUTO-PAY-TR                      VALUE '3'.
000083         88  CORRESPONDENCE-TR                VALUE '4'.
000084         88  ADDRESS-TR                       VALUE '5'.
000085         88  GENERAL-INFO-TR                  VALUE '6'.
000086         88  AUTO-PROMPT-TR                   VALUE '7'.
000087         88  DENIAL-TR                        VALUE '8'.
000088         88  INCURRED-CHG-TR                  VALUE '9'.
000089         88  FORM-CONTROL-TR                  VALUE 'A'.
000090
000091     12  AT-RECORDED-DT                  PIC XX.
000092     12  AT-RECORDED-BY                  PIC X(4).
000093     12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
000094
000095     12  AT-TRAILER-BODY                 PIC X(165).
000096
000097     12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
000098         16  AT-RESERVE-CONTROLS.
000099             20  AT-MANUAL-SW            PIC X.
000100                 88  AT-MANUAL-RESERVES-USED VALUE '1'.
000101             20  AT-FUTURE-SW            PIC X.
000102                 88  AT-FUTURE-RESERVES-USED VALUE '1'.
000103             20  AT-PTC-SW               PIC X.
000104                 88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
000105             20  AT-IBNR-SW              PIC X.
000106                 88  AT-IBNR-RESERVES-USED   VALUE '1'.
000107             20  AT-PTC-LF-SW            PIC X.
000108                 88  AT-LF-PTC-USED          VALUE '1'.
000109             20  AT-CDT-ACCESS-METHOD    PIC X.
000110                 88  AT-CDT-ROUND-NEAR       VALUE '1'.
000111                 88  AT-CDT-ROUND-HIGH       VALUE '2'.
000112                 88  AT-CDT-INTERPOLATED     VALUE '3'.
000113             20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
000114         16  AT-LAST-COMPUTED-DT         PIC XX.
000115         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
000116         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
000117         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
000118         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
000119         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
000120         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
000121         16  AT-EXPENSE-CONTROLS.
000122             20  AT-EXPENSE-METHOD       PIC X.
000123                 88  NO-EXPENSE-CALCULATED    VALUE '1'.
000124                 88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
000125                 88  PERCENT-OF-PMT           VALUE '3'.
000126                 88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
000127             20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
000128             20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
000129         16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
000130         16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
000131
000132         16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
000133         16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
000134
000135*        16  FILLER                      PIC X(53).
000136         16  FILLER                      PIC X(47).
000137
000138         16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
000139         16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
000140
000141         16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
000142             20  AT-OPEN-CLOSE-DATE      PIC XX.
000143             20  AT-OPEN-CLOSE-TYPE      PIC X.
000144*                    C = CLOSED
000145*                    O = OPEN
000146             20  AT-OPEN-CLOSE-REASON    PIC X(5).
000147*                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
000148
000149     12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
000150         16  AT-PAYMENT-TYPE             PIC X.
000151             88  PARTIAL-PAYMENT                VALUE '1'.
000152             88  FINAL-PAYMENT                  VALUE '2'.
000153             88  LUMP-SUM-PAYMENT               VALUE '3'.
000154             88  ADDITIONAL-PAYMENT             VALUE '4'.
000155             88  CHARGEABLE-EXPENSE             VALUE '5'.
000156             88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
000157             88  VOIDED-PAYMENT                 VALUE '9'.
000158             88  TRANSFER                       VALUE 'T'.
000159             88  LIFE-INTEREST                  VALUE 'I'.
000160
000161         16  AT-CLAIM-TYPE               PIC X.
000162             88  PAID-FOR-AH                    VALUE 'A'.
000163             88  PAID-FOR-LIFE                  VALUE 'L'.
000164             88  PAID-FOR-IUI                   VALUE 'I'.
000165             88  PAID-FOR-GAP                   VALUE 'G'.
000166             88  PAID-FOR-FAM                   VALUE 'F'.
000167             88  PAID-FOR-OTH                   VALUE 'O'.
000168         16  AT-CLAIM-PREM-TYPE          PIC X.
000169             88  AT-SINGLE-PREMIUM              VALUE '1'.
000170             88  AT-O-B-COVERAGE                VALUE '2'.
000171             88  AT-OPEN-END-COVERAGE           VALUE '3'.
000172         16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
000173         16  AT-CHECK-NO                 PIC X(7).
000174         16  AT-PAID-FROM-DT             PIC XX.
000175         16  AT-PAID-THRU-DT             PIC XX.
000176         16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
000177         16  AT-ACH-PAYMENT              PIC X.
000178*        16  FILLER                      PIC X.
000179         16  AT-PAYEES-NAME              PIC X(30).
000180         16  AT-PAYMENT-ORIGIN           PIC X.
000181             88  ONLINE-MANUAL-PMT              VALUE '1'.
000182             88  ONLINE-AUTO-PMT                VALUE '2'.
000183             88  OFFLINE-PMT                    VALUE '3'.
000184         16  AT-CHECK-WRITTEN-DT         PIC XX.
000185         16  AT-TO-BE-WRITTEN-DT         PIC XX.
000186         16  AT-VOID-DATA.
000187             20  AT-VOID-DT              PIC XX.
000188*00144       20  AT-VOID-REASON          PIC X(30).
000189             20  AT-VOID-REASON          PIC X(26).
000190         16  AT-PMT-APPROVED-BY          PIC X(04).
000191         16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
000192         16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
000193         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
000194                                         PIC S99V9(5)  COMP-3.
000195         16  AT-CREDIT-INTERFACE.
000196             20  AT-PMT-SELECT-DT        PIC XX.
000197                 88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
000198             20  AT-PMT-ACCEPT-DT        PIC XX.
000199                 88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
000200             20  AT-VOID-SELECT-DT       PIC XX.
000201                 88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
000202             20  AT-VOID-ACCEPT-DT       PIC XX.
000203                 88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
000204
000205         16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
000206                 88  PAYMENT-NOT-QUEUED           VALUE ZERO.
000207                 88  CONVERSION-PAYMENT           VALUE +99999999.
000208         16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
000209
000210         16  AT-FORCE-CONTROL            PIC X.
000211             88  PAYMENT-WAS-FORCED           VALUE '1'.
000212         16  AT-PREV-LAST-PMT-DT         PIC XX.
000213         16  AT-PREV-PAID-THRU-DT        PIC XX.
000214         16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
000215         16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
000216         16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
000217         16  AT-BENEFIT-TYPE             PIC X.
000218
000219         16  AT-EXPENSE-TYPE             PIC X.
000220         16  AT-PAYMENT-APPROVAL-SW      PIC X.
000221
000222         16  AT-PAYEE-TYPE-CD.
000223             20  AT-PAYEE-TYPE           PIC X.
000224                 88  INSURED-PAID           VALUE 'I'.
000225                 88  BENEFICIARY-PAID       VALUE 'B'.
000226                 88  ACCOUNT-PAID           VALUE 'A'.
000227                 88  OTHER-1-PAID           VALUE 'O'.
000228                 88  OTHER-2-PAID           VALUE 'Q'.
000229                 88  DOCTOR-PAID            VALUE 'P'.
000230                 88  EMPLOYER-PAID          VALUE 'E'.
000231             20  AT-PAYEE-SEQ            PIC X.
000232
000233         16  AT-CASH-PAYMENT             PIC X.
000234         16  AT-GROUPED-PAYMENT          PIC X.
000235         16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
000236         16  AT-APPROVAL-LEVEL-REQD      PIC X.
000237         16  AT-APPROVED-LEVEL           PIC X.
000238         16  AT-VOID-TYPE                PIC X.
000239             88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
000240             88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
000241         16  AT-AIG-UNEMP-IND            PIC X.
000242             88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
000243         16  AT-ASSOCIATES               PIC X.
000244             88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
000245             88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
000246
000247         16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
000248         16  AT-CV-PMT-CODE              PIC X.
000249             88  FULL-DEATH-PAYMENT         VALUE '1'.
000250             88  HALF-DEATH-PAYMENT         VALUE '2'.
000251             88  FULL-ADD-PAYMENT           VALUE '3'.
000252             88  HALF-ADD-PAYMENT           VALUE '4'.
000253             88  FULL-RIDER-PAYMENT         VALUE '5'.
000254             88  HALF-RIDER-PAYMENT         VALUE '6'.
000255             88  NON-CHG-EXP-PAYMENT        VALUE '7'.
000256             88  ADDL-PAYMENT               VALUE '8'.
000257
000258         16  AT-EOB-CODE1                PIC XXX.
000259         16  AT-EOB-CODE2                PIC XXX.
000260         16  AT-EOB-CODE3                PIC XXX.
000261         16  FILLER REDEFINES AT-EOB-CODE3.
000262             20  AT-PRINT-CLM-FORM       PIC X.
000263             20  AT-PRINT-SURVEY         PIC X.
000264             20  AT-SPECIAL-RELEASE      PIC X.
000265         16  AT-EOB-CODE4                PIC XXX.
000266         16  FILLER REDEFINES AT-EOB-CODE4.
000267             20  AT-INT-PMT-SELECT-DT    PIC XX.
000268             20  FILLER                  PIC X.
000269         16  AT-EOB-CODE5                PIC XXX.
000270         16  FILLER REDEFINES AT-EOB-CODE5.
000271             20  AT-PMT-PROOF-DT         PIC XX.
000272             20  FILLER                  PIC X.
000273
000274         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
000275             88  AT-PRINT-EOB            VALUE 'Y'.
000276
000277         16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
000278         16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
000279
000280     12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
000281         16  AT-SCHEDULE-START-DT        PIC XX.
000282         16  AT-SCHEDULE-END-DT          PIC XX.
000283         16  AT-TERMINATED-DT            PIC XX.
000284         16  AT-LAST-PMT-TYPE            PIC X.
000285             88  LAST-PMT-IS-FINAL              VALUE 'F'.
000286             88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
000287         16  AT-FIRST-PMT-DATA.
000288             20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
000289             20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
000290             20  AT-1ST-PAY-THRU-DT      PIC XX.
000291         16  AT-REGULAR-PMT-DATA.
000292             20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
000293             20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
000294             20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
000295         16  AT-AUTO-PAYEE-CD.
000296             20  AT-AUTO-PAYEE-TYPE      PIC X.
000297                 88  INSURED-PAID-AUTO      VALUE 'I'.
000298                 88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
000299                 88  ACCOUNT-PAID-AUTO      VALUE 'A'.
000300                 88  OTHER-1-PAID-AUTO      VALUE 'O'.
000301                 88  OTHER-2-PAID-AUTO      VALUE 'Q'.
000302             20  AT-AUTO-PAYEE-SEQ       PIC X.
000303         16  AT-AUTO-PAY-DAY             PIC 99.
000304         16  AT-AUTO-CASH                PIC X.
000305             88  AT-CASH                      VALUE 'Y'.
000306             88  AT-NON-CASH                  VALUE 'N'.
000307*        16  FILLER                      PIC X(129).
000308         16  AT-AUTO-END-LETTER          PIC X(4).
000309         16  FILLER                      PIC X(125).
000310
000311         16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
000312         16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
000313
000314     12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
000315         16  AT-LETTER-SENT-DT           PIC XX.
000316         16  AT-RECEIPT-FOLLOW-UP        PIC XX.
000317         16  AT-AUTO-RE-SEND-DT          PIC XX.
000318         16  AT-LETTER-ANSWERED-DT       PIC XX.
000319         16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
000320         16  AT-LETTER-ORIGIN            PIC X.
000321             88  ONLINE-CREATION              VALUE '1' '3'.
000322             88  OFFLINE-CREATION             VALUE '2' '4'.
000323             88  NAPER-ONLINE-CREATION        VALUE '3'.
000324             88  NAPER-OFFLINE-CREATION       VALUE '4'.
000325         16  AT-STD-LETTER-FORM          PIC X(4).
000326         16  AT-REASON-TEXT              PIC X(70).
000327         16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
000328         16  AT-ADDRESEE-TYPE            PIC X.
000329              88  INSURED-ADDRESEE            VALUE 'I'.
000330              88  BENEFICIARY-ADDRESEE        VALUE 'B'.
000331              88  ACCOUNT-ADDRESEE            VALUE 'A'.
000332              88  PHYSICIAN-ADDRESEE          VALUE 'P'.
000333              88  EMPLOYER-ADDRESEE           VALUE 'E'.
000334              88  OTHER-ADDRESEE-1            VALUE 'O'.
000335              88  OTHER-ADDRESEE-2            VALUE 'Q'.
000336         16  AT-ADDRESSEE-NAME           PIC X(30).
000337         16  AT-INITIAL-PRINT-DATE       PIC XX.
000338         16  AT-RESEND-PRINT-DATE        PIC XX.
000339         16  AT-CORR-SOL-UNSOL           PIC X.
000340         16  AT-LETTER-PURGED-DT         PIC XX.
000341*
000342*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
000343*
000344         16  AT-CSO-REDEFINITION.
000345             20  AT-RESEND-LETTER-FORM   PIC X(4).
000346             20  AT-AUTO-CLOSE-IND       PIC X(1).
000347             20  AT-LETTER-TO-BENE       PIC X(1).
000348             20  AT-STOP-LETTER-DT       PIC X(2).
000349             20  AT-AUTH-RCVD            PIC X(1).
000350             20  FILLER                  PIC X(18).
000351*             20  FILLER                  PIC X(27).
000352             20  AT-CSO-LETTER-STATUS    PIC X.
000353                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
000354                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
000355                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
000356             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
000357             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
000358*
000359*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
000360*
000361*        16  FILLER                      PIC X(26).
000362*
000363*        16  AT-DMD-BSR-CODE             PIC X.
000364*            88  AT-AUTOMATED-BSR              VALUE 'A'.
000365*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
000366*
000367*        16  AT-DMD-LETTER-STATUS        PIC X.
000368*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
000369*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
000370*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
000371*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
000372*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
000373
000374         16  AT-CORR-LAST-MAINT-DT       PIC XX.
000375         16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
000376
000377     12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
000378         16  AT-ADDRESS-TYPE             PIC X.
000379             88  INSURED-ADDRESS               VALUE 'I'.
000380             88  BENEFICIARY-ADDRESS           VALUE 'B'.
000381             88  ACCOUNT-ADDRESS               VALUE 'A'.
000382             88  PHYSICIAN-ADDRESS             VALUE 'P'.
000383             88  EMPLOYER-ADDRESS              VALUE 'E'.
000384             88  OTHER-ADDRESS-1               VALUE 'O'.
000385             88  OTHER-ADDRESS-2               VALUE 'Q'.
000386         16  AT-MAIL-TO-NAME             PIC X(30).
000387         16  AT-ADDRESS-LINE-1           PIC X(30).
000388         16  AT-ADDRESS-LINE-2           PIC X(30).
000389         16  AT-CITY-STATE.
000390             20  AT-CITY                 PIC X(28).
000391             20  AT-STATE                PIC XX.
000392         16  AT-ZIP.
000393             20  AT-ZIP-CODE.
000394                 24  AT-ZIP-1ST          PIC X.
000395                     88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000396                 24  FILLER              PIC X(4).
000397             20  AT-ZIP-PLUS4            PIC X(4).
000398         16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
000399             20  AT-CAN-POSTAL-1         PIC XXX.
000400             20  AT-CAN-POSTAL-2         PIC XXX.
000401             20  FILLER                  PIC XXX.
000402         16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
000403*         16  FILLER                      PIC X(23).
000404         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
000405         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
000406         16  FILLER                      PIC X(13).
000407         16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
000408         16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
000409
000410     12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
000411         16  AT-INFO-LINE-1              PIC X(60).
000412         16  FILLER REDEFINES AT-INFO-LINE-1.
000413             20  AT-NOTE-ERROR-NO OCCURS 15
000414                                         PIC X(4).
000415         16  AT-INFO-LINE-2              PIC X(60).
000416         16  FILLER REDEFINES AT-INFO-LINE-2.
000417             20  AT-ICD-CODE-1           PIC X(8).
000418             20  AT-ICD-CODE-2           PIC X(8).
000419             20  FILLER                  PIC X(44).
000420         16  AT-INFO-TRAILER-TYPE        PIC X.
000421             88  AT-ERRORS-NOTE          VALUE 'E'.
000422             88  AT-PAYMENT-NOTE         VALUE 'P'.
000423             88  AT-CALL-NOTE            VALUE 'C'.
000424             88  AT-MAINT-NOTE           VALUE 'M'.
000425             88  AT-CERT-CHANGE          VALUE 'X'.
000426             88  AT-APPROVAL-NOTE        VALUE 'R'.
000427             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
000428             88  AT-CERT-CANCELLED       VALUE 'T'.
000429         16  AT-CALL-TYPE                PIC X.
000430             88  AT-PHONE-CALL-IN        VALUE 'I'.
000431             88  AT-PHONE-CALL-NEW       VALUE 'N'.
000432             88  AT-PHONE-CALL-OUT       VALUE 'O'.
000433         16  AT-NOTE-CONTINUATION        PIC X.
000434             88  AT-CONTINUED-NOTE       VALUE 'X'.
000435         16  AT-EOB-CODES-EXIST          PIC X.
000436             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
000437         16  FILLER                      PIC X(35).
000438         16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
000439         16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
000440
000441     12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
000442         16  AT-PROMPT-LINE-1            PIC X(60).
000443         16  AT-PROMPT-LINE-2            PIC X(60).
000444         16  AT-PROMPT-START-DT          PIC XX.
000445         16  AT-PROMPT-END-DT            PIC XX.
000446         16  FILLER                      PIC X(35).
000447         16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
000448         16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
000449
000450     12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
000451         16  AT-DENIAL-INFO-1            PIC X(60).
000452         16  AT-DENIAL-INFO-2            PIC X(60).
000453         16  AT-DENIAL-DT                PIC XX.
000454         16  AT-RETRACTION-DT            PIC XX.
000455         16  AT-DENIAL-REASON-CODE       PIC X(4).
000456*         16  FILLER                      PIC X(31).
000457         16  AT-DENIAL-PROOF-DT          PIC XX.
000458         16  FILLER                      PIC X(29).
000459         16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
000460         16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
000461
000462     12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
000463         16  AT-OLD-INCURRED-DT          PIC XX.
000464         16  AT-OLD-REPORTED-DT          PIC XX.
000465         16  AT-OLD-ESTABLISHED-DT       PIC XX.
000466         16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
000467         16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
000468         16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
000469         16  AT-OLD-PAID-THRU-DT         PIC XX.
000470         16  AT-LAST-PMT-MADE-DT         PIC XX.
000471         16  FILLER                      PIC X(26).
000472         16  AT-OLD-DIAG-CODE            PIC X(6).
000473         16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
000474         16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
000475         16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
000476         16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
000477         16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
000478         16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
000479         16  AT-OLD-DIAG-DESCRIP         PIC X(60).
000480         16  AT-OLD-ICD-CODE-1           PIC X(8).
000481         16  AT-OLD-ICD-CODE-2           PIC X(8).
000482         16  FILLER                      PIC X(9).
000483         16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
000484
000485     12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
000486         16  AT-FORM-SEND-ON-DT          PIC XX.
000487         16  AT-FORM-FOLLOW-UP-DT        PIC XX.
000488         16  AT-FORM-RE-SEND-DT          PIC XX.
000489         16  AT-FORM-ANSWERED-DT         PIC XX.
000490         16  AT-FORM-PRINTED-DT          PIC XX.
000491         16  AT-FORM-REPRINT-DT          PIC XX.
000492         16  AT-FORM-TYPE                PIC X.
000493             88  INITIAL-FORM                  VALUE '1'.
000494             88  PROGRESS-FORM                 VALUE '2'.
000495         16  AT-INSTRUCT-LN-1            PIC X(28).
000496         16  AT-INSTRUCT-LN-2            PIC X(28).
000497         16  AT-INSTRUCT-LN-3            PIC X(28).
000498         16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
000499         16  AT-FORM-ADDRESS             PIC X.
000500             88  FORM-TO-INSURED              VALUE 'I'.
000501             88  FORM-TO-ACCOUNT              VALUE 'A'.
000502             88  FORM-TO-OTHER-1              VALUE 'O'.
000503             88  FORM-TO-OTHER-2              VALUE 'Q'.
000504         16  AT-RELATED-1.
000505             20 AT-REL-CARR-1            PIC X.
000506             20 AT-REL-CLAIM-1           PIC X(7).
000507             20 AT-REL-CERT-1            PIC X(11).
000508         16  AT-RELATED-2.
000509             20 AT-REL-CARR-2            PIC X.
000510             20 AT-REL-CLAIM-2           PIC X(7).
000511             20 AT-REL-CERT-2            PIC X(11).
000512         16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
000513         16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
000514         16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
000515         16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
000516         16  AT-FORM-REM-PRINT-DT        PIC XX.
000517         16  AT-STOP-FORM-DT             PIC X(2).
000518
000519         16  FILLER                      PIC X(09).
000520         16  AT-FORM-LAST-MAINT-DT       PIC XX.
000521         16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
000522******************************************************************
      *<<((file: ELCTRLR))
000263
000264     EJECT
000265*                            COPY ERCACCT.
      *>>((file: ERCACCT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCACCT                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.031                          *
000007*                                                                *
000008*   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
000009*                                                                *
000010*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000011*   VSAM ACCOUNT MASTER FILES.                                   *
000012*                                                                *
000013*   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
000014*                                                                *
000015*   FILE TYPE = VSAM,KSDS                                        *
000016*   RECORD SIZE = 2000  RECFORM = FIX                            *
000017*                                                                *
000018*   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
000019*       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
000020*                                                                *
000021*   LOG = NO                                                     *
000022*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000023*                                                                *
000024*                                                                *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
000034* 092705    2005050300006  PEMA  ADD SPP LEASES
000035* 022808    2007083100002  PEMA  ADD FREEZE STATUS
000036* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000037* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
000038* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
000039* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
000040* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
000041******************************************************************
000042
000043 01  ACCOUNT-MASTER.
000044     12  AM-RECORD-ID                      PIC XX.
000045         88  VALID-AM-ID                      VALUE 'AM'.
000046
000047     12  AM-CONTROL-PRIMARY.
000048         16  AM-COMPANY-CD                 PIC X.
000049         16  AM-MSTR-CNTRL.
000050             20  AM-CONTROL-A.
000051                 24  AM-CARRIER            PIC X.
000052                 24  AM-GROUPING.
000053                     28 AM-GROUPING-PREFIX PIC XXX.
000054                     28 AM-GROUPING-PRIME  PIC XXX.
000055                 24  AM-STATE              PIC XX.
000056                 24  AM-ACCOUNT.
000057                     28  AM-ACCOUNT-PREFIX PIC X(4).
000058                     28  AM-ACCOUNT-PRIME  PIC X(6).
000059             20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
000060                                           PIC X(19).
000061             20  AM-CNTRL-B.
000062                 24  AM-EXPIRATION-DT      PIC XX.
000063                 24  FILLER                PIC X(4).
000064             20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
000065                 24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
000066
000067     12  AM-CONTROL-BY-VAR-GRP.
000068         16  AM-COMPANY-CD-A1              PIC X.
000069         16  AM-VG-CARRIER                 PIC X.
000070         16  AM-VG-GROUPING                PIC X(6).
000071         16  AM-VG-STATE                   PIC XX.
000072         16  AM-VG-ACCOUNT                 PIC X(10).
000073         16  AM-VG-DATE.
000074             20  AM-VG-EXPIRATION-DT       PIC XX.
000075             20  FILLER                    PIC X(4).
000076         16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
000077                                           PIC 9(11)      COMP-3.
000078     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
000079         16  FILLER                        PIC X(10).
000080         16  AM-VG-KEY3.
000081             20  AM-VG3-ACCOUNT            PIC X(10).
000082             20  AM-VG3-EXP-DT             PIC XX.
000083         16  FILLER                        PIC X(4).
000084     12  AM-MAINT-INFORMATION.
000085         16  AM-LAST-MAINT-DT              PIC XX.
000086         16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000087         16  AM-LAST-MAINT-USER            PIC X(4).
000088         16  FILLER                        PIC XX.
000089
000090     12  AM-EFFECTIVE-DT                   PIC XX.
000091     12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
000092
000093     12  AM-PREV-DATES  COMP-3.
000094         16  AM-PREV-EXP-DT                PIC 9(11).
000095         16  AM-PREV-EFF-DT                PIC 9(11).
000096
000097     12  AM-REPORT-CODE-1                  PIC X(10).
000098     12  AM-REPORT-CODE-2                  PIC X(10).
000099
000100     12  AM-CITY-CODE                      PIC X(4).
000101     12  AM-COUNTY-PARISH                  PIC X(6).
000102
000103     12  AM-NAME                           PIC X(30).
000104     12  AM-PERSON                         PIC X(30).
000105     12  AM-ADDRS                          PIC X(30).
000106     12  AM-CITY.
000107         16  AM-ADDR-CITY                  PIC X(28).
000108         16  AM-ADDR-STATE                 PIC XX.
000109     12  AM-ZIP.
000110         16  AM-ZIP-PRIME.
000111             20  AM-ZIP-PRI-1ST            PIC X.
000112                 88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
000113             20  FILLER                    PIC X(4).
000114         16  AM-ZIP-PLUS4                  PIC X(4).
000115     12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
000116         16  AM-CAN-POSTAL-1               PIC XXX.
000117         16  AM-CAN-POSTAL-2               PIC XXX.
000118         16  FILLER                        PIC XXX.
000119     12  AM-TEL-NO.
000120         16  AM-AREA-CODE                  PIC 999.
000121         16  AM-TEL-PRE                    PIC 999.
000122         16  AM-TEL-NBR                    PIC 9(4).
000123     12  AM-TEL-LOC                        PIC X.
000124         88  AM-TEL-AT-HOME                   VALUE 'H'.
000125         88  AM-TEL-AT-BUSINESS               VALUE 'B'.
000126
000127     12  AM-COMM-STRUCTURE.
000128         16  AM-DEFN-1.
000129             20  AM-AGT-COMMS       OCCURS 10 TIMES.
000130                 24  AM-AGT.
000131                     28  AM-AGT-PREFIX     PIC X(4).
000132                     28  AM-AGT-PRIME      PIC X(6).
000133                 24  AM-COM-TYP            PIC X.
000134                 24  AM-L-COM              PIC SV9(5)     COMP-3.
000135                 24  AM-J-COM              PIC SV9(5)     COMP-3.
000136                 24  AM-A-COM              PIC SV9(5)     COMP-3.
000137                 24  AM-RECALC-LV-INDIC    PIC X.
000138                 24  AM-RETRO-LV-INDIC     PIC X.
000139                 24  AM-GL-CODES           PIC X.
000140                 24  AM-COMM-CHARGEBACK    PIC 9(02).
000141                 24  FILLER                PIC X(01).
000142         16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
000143             20  AM-COM-TBLS        OCCURS 10 TIMES.
000144                 24  FILLER                PIC X(11).
000145                 24  AM-L-COMA             PIC XXX.
000146                 24  AM-J-COMA             PIC XXX.
000147                 24  AM-A-COMA             PIC XXX.
000148                 24  FILLER                PIC X(6).
000149
000150     12  AM-COMM-CHANGE-STATUS             PIC X.
000151         88  AM-COMMISSIONS-CHANGED           VALUE '*'.
000152
000153     12  AM-CSR-CODE                       PIC X(4).
000154
000155     12  AM-BILLING-STATUS                 PIC X.
000156         88  AM-ACCOUNT-BILLED                VALUE 'B'.
000157         88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
000158     12  AM-AUTO-REFUND-SW                 PIC X.
000159         88  AUTO-REFUNDS-USED                VALUE 'Y'.
000160         88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
000161     12  AM-GPCD                           PIC 99.
000162     12  AM-IG                             PIC X.
000163         88  AM-HAS-INDIVIDUAL                VALUE '1'.
000164         88  AM-HAS-GROUP                     VALUE '2'.
000165     12  AM-STATUS                         PIC X.
000166         88  AM-ACCOUNT-ACTIVE                VALUE '0'.
000167         88  AM-ACCOUNT-INACTIVE              VALUE '1'.
000168         88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
000169         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
000170         88  AM-ACCOUNT-FROZEN                VALUE '4'.
000171         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
000172         88  AM-ACCOUNT-DROPPED               VALUE '6'.
000173         88  AM-ACCOUNT-LAPSED                VALUE '7'.
000174         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
000175         88  AM-ACCOUNT-PENDING               VALUE '9'.
000176     12  AM-REMIT-TO                       PIC 99.
000177     12  AM-ID-NO                          PIC X(11).
000178
000179     12  AM-CAL-TABLE                      PIC XX.
000180     12  AM-LF-DEVIATION                   PIC XXX.
000181     12  AM-AH-DEVIATION                   PIC XXX.
000182     12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000183     12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000184     12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
000185     12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
000186     12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000187     12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000188
000189     12  AM-USER-FIELDS.
000190         16  AM-FLD-1                      PIC XX.
000191         16  AM-FLD-2                      PIC XX.
000192         16  AM-FLD-3                      PIC XX.
000193         16  AM-FLD-4                      PIC XX.
000194         16  AM-FLD-5                      PIC XX.
000195
000196     12  AM-1ST-PROD-DATE.
000197         16  AM-1ST-PROD-YR                PIC XX.
000198         16  AM-1ST-PROD-MO                PIC XX.
000199         16  AM-1ST-PROD-DA                PIC XX.
000200     12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
000201     12  AM-CERTS-PURGED-DATE.
000202         16  AM-PUR-YR                     PIC XX.
000203         16  AM-PUR-MO                     PIC XX.
000204         16  AM-PUR-DA                     PIC XX.
000205     12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
000206     12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
000207     12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
000208     12  AM-INACTIVE-DATE.
000209         16  AM-INA-MO                     PIC 99.
000210         16  AM-INA-DA                     PIC 99.
000211         16  AM-INA-YR                     PIC 99.
000212     12  AM-AR-HI-CERT-DATE                PIC XX.
000213
000214     12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000215     12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000216
000217     12  AM-OB-PAYMENT-MODE                PIC X.
000218         88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
000219         88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
000220         88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
000221         88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
000222
000223     12  AM-AH-ONLY-INDICATOR              PIC X.
000224         88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
000225         88  AM-NO-AH-ONLY                    VALUE 'N'.
000226
000227     12  AM-EDIT-LOAN-OFC                  PIC X(01).
000228
000229     12  AM-OVER-SHORT.
000230         16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
000231         16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
000232
000233     12  AM-DCC-PRODUCT-CODE               PIC XXX.
000234     12  AM-DCC-CLP-STATE                  PIC XX.
000235
000236     12  AM-RECALC-COMM                    PIC X.
000237     12  AM-RECALC-REIN                    PIC X.
000238
000239     12  AM-REI-TABLE                      PIC XXX.
000240     12  AM-REI-ET-LF                      PIC X.
000241     12  AM-REI-ET-AH                      PIC X.
000242     12  AM-REI-PE-LF                      PIC X.
000243     12  AM-REI-PE-AH                      PIC X.
000244     12  AM-REI-PRT-ST                     PIC X.
000245     12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
000246     12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
000247     12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
000248     12  AM-REI-GROUP-A                    PIC X(6).
000249     12  AM-REI-MORT                       PIC X(4).
000250     12  AM-REI-PRT-OW                     PIC X.
000251     12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
000252     12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
000253     12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
000254     12  AM-REI-GROUP-B                    PIC X(6).
000255
000256     12  AM-TRUST-TYPE                     PIC X(2).
000257
000258     12  AM-EMPLOYER-STMT-USED             PIC X.
000259     12  AM-GROUPED-CHECKS-Y-N             PIC X.
000260
000261     12  AM-STD-AH-TYPE                    PIC XX.
000262     12  AM-EARN-METHODS.
000263         16  AM-EARN-METHOD-R              PIC X.
000264             88 AM-REF-RL-R78                 VALUE 'R'.
000265             88 AM-REF-RL-PR                  VALUE 'P'.
000266             88 AM-REF-RL-MEAN                VALUE 'M'.
000267             88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
000268         16  AM-EARN-METHOD-L              PIC X.
000269             88 AM-REF-LL-R78                 VALUE 'R'.
000270             88 AM-REF-LL-PR                  VALUE 'P'.
000271             88 AM-REF-LL-MEAN                VALUE 'M'.
000272             88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
000273         16  AM-EARN-METHOD-A              PIC X.
000274             88 AM-REF-AH-R78                 VALUE 'R'.
000275             88 AM-REF-AH-PR                  VALUE 'P'.
000276             88 AM-REF-AH-MEAN                VALUE 'M'.
000277             88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
000278             88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
000279             88 AM-REF-AH-NET                 VALUE 'N'.
000280
000281     12  AM-TOL-PREM                       PIC S999V99    COMP-3.
000282     12  AM-TOL-REF                        PIC S999V99    COMP-3.
000283     12  AM-TOL-CLM                        PIC S999V99    COMP-3.
000284
000285     12  AM-RET-Y-N                        PIC X.
000286     12  AM-RET-P-E                        PIC X.
000287     12  AM-LF-RET                         PIC S9V9999    COMP-3.
000288     12  AM-AH-RET                         PIC S9V9999    COMP-3.
000289     12  AM-RET-GRP                        PIC X(6).
000290     12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
000291         16  AM-POOL-PRIME                 PIC XXX.
000292         16  AM-POOL-SUB                   PIC XXX.
000293     12  AM-RETRO-EARNINGS.
000294         16  AM-RET-EARN-R                 PIC X.
000295         16  AM-RET-EARN-L                 PIC X.
000296         16  AM-RET-EARN-A                 PIC X.
000297     12  AM-RET-ST-TAX-USE                 PIC X.
000298         88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
000299         88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
000300     12  AM-RETRO-BEG-EARNINGS.
000301         16  AM-RET-BEG-EARN-R             PIC X.
000302         16  AM-RET-BEG-EARN-L             PIC X.
000303         16  AM-RET-BEG-EARN-A             PIC X.
000304     12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
000305     12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
000306
000307     12  AM-USER-SELECT-OPTIONS.
000308         16  AM-USER-SELECT-1              PIC X(10).
000309         16  AM-USER-SELECT-2              PIC X(10).
000310         16  AM-USER-SELECT-3              PIC X(10).
000311         16  AM-USER-SELECT-4              PIC X(10).
000312         16  AM-USER-SELECT-5              PIC X(10).
000313
000314     12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000315
000316     12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000317
000318     12  AM-RPT045A-SWITCH                 PIC X.
000319         88  RPT045A-OFF                   VALUE 'N'.
000320
000321     12  AM-INSURANCE-LIMITS.
000322         16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
000323         16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
000324
000325     12  AM-PROFILE-CHANGE-SWITCH          PIC X.
000326         88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
000327
000328     12  AM-DISMBR-COVERAGE-SW             PIC X.
000329         88  AM-DISMBR-COVERAGE               VALUE 'Y'.
000330         88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
000331
000332     12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
000333
000334     12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
000335     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
000336     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
000337     12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
000338     12  AM-DCC-UEF-STATE                  PIC XX.
000339     12  FILLER                            PIC XXX.
000340     12  AM-REPORT-CODE-3                  PIC X(10).
000341*    12  FILLER                            PIC X(22).
000342
000343     12  AM-RESERVE-DATE.
000344         16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
000345         16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
000346         16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
000347
000348     12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
000349     12  AM-NOTIFICATION-TYPES.
000350         16  AM-NOTIF-OF-LETTERS           PIC X.
000351         16  AM-NOTIF-OF-PAYMENTS          PIC X.
000352         16  AM-NOTIF-OF-REPORTS           PIC X.
000353         16  AM-NOTIF-OF-STATUS            PIC X.
000354
000355     12  AM-BENEFIT-TABLE-USAGE            PIC X.
000356         88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
000357         88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
000358         88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
000359         88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
000360
000361     12  AM-BENEFIT-CONTROLS.
000362         16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
000363             20  AM-BENEFIT-CODE           PIC XX.
000364             20  AM-BENEFIT-TYPE           PIC X.
000365             20  AM-BENEFIT-REVISION       PIC XXX.
000366             20  AM-BENEFIT-REM-TERM       PIC X.
000367             20  AM-BENEFIT-RETRO-Y-N      PIC X.
000368             20  FILLER                    PIC XX.
000369         16  FILLER                        PIC X(80).
000370
000371     12  AM-TRANSFER-DATA.
000372         16  AM-TRANSFERRED-FROM.
000373             20  AM-TRNFROM-CARRIER        PIC X.
000374             20  AM-TRNFROM-GROUPING.
000375                 24  AM-TRNFROM-GRP-PREFIX PIC XXX.
000376                 24  AM-TRNFROM-GRP-PRIME  PIC XXX.
000377             20  AM-TRNFROM-STATE          PIC XX.
000378             20  AM-TRNFROM-ACCOUNT.
000379                 24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
000380                 24  AM-TRNFROM-ACCT-PRIME PIC X(6).
000381             20  AM-TRNFROM-DTE            PIC XX.
000382         16  AM-TRANSFERRED-TO.
000383             20  AM-TRNTO-CARRIER          PIC X.
000384             20  AM-TRNTO-GROUPING.
000385                 24  AM-TRNTO-GRP-PREFIX   PIC XXX.
000386                 24  AM-TRNTO-GRP-PRIME    PIC XXX.
000387             20  AM-TRNTO-STATE            PIC XX.
000388             20  AM-TRNTO-ACCOUNT.
000389                 24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
000390                 24  AM-TRNTO-ACCT-PRIME   PIC X(6).
000391             20  AM-TRNTO-DTE              PIC XX.
000392         16  FILLER                        PIC X(10).
000393
000394     12  AM-SAVED-REMIT-TO                 PIC 99.
000395
000396     12  AM-COMM-STRUCTURE-SAVED.
000397         16  AM-DEFN-1-SAVED.
000398             20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
000399                 24  AM-AGT-SV             PIC X(10).
000400                 24  AM-COM-TYP-SV         PIC X.
000401                 24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
000402                 24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
000403                 24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
000404                 24  AM-RECALC-LV-INDIC-SV PIC X.
000405                 24  FILLER                PIC X.
000406                 24  AM-GL-CODES-SV        PIC X.
000407                 24  AM-COM-CHARGEBACK-SV  PIC 99.
000408                 24  FILLER                PIC X.
000409         16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
000410             20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
000411                 24  FILLER                PIC X(11).
000412                 24  AM-L-COMA-SV          PIC XXX.
000413                 24  AM-J-COMA-SV          PIC XXX.
000414                 24  AM-A-COMA-SV          PIC XXX.
000415                 24  FILLER                PIC X(6).
000416
000417     12  AM-FLC-NET-PREMIUM-ALLOWANCE.
000418         16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
000419            20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
000420            20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
000421            20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
000422
000423     12  AM-ORIG-DEALER-NO                 PIC X(10).
000424     12  FILLER                            PIC X(120).
000425
000426     12  AM-ACCOUNT-EXECUTIVE-DATA.
000427         16  AM-CONTROL-NAME               PIC X(30).
000428         16  AM-EXECUTIVE-ONE.
000429             20  AM-EXEC1-NAME             PIC X(15).
000430             20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
000431                                                          COMP-3.
000432             20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
000433                                                          COMP-3.
000434         16  AM-EXECUTIVE-TWO.
000435             20  AM-EXEC2-NAME             PIC X(15).
000436             20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
000437                                                          COMP-3.
000438             20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
000439                                                          COMP-3.
000440
000441     12  AM-RETRO-ADDITIONAL-DATA.
000442         16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
000443         16  AM-RETRO-PREM-P-E             PIC X.
000444         16  AM-RETRO-CLMS-P-I             PIC X.
000445         16  AM-RETRO-RET-BRACKET-LF.
000446             20  AM-RETRO-RET-METHOD-LF    PIC X.
000447                 88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
000448                 88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
000449             20  AM-RETRO-RET-BASIS-LF     PIC X.
000450                 88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
000451                 88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
000452             20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
000453                 24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
000454                 24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
000455         16  AM-RETRO-RET-BRACKET-AH.
000456             20  AM-RETRO-RET-METHOD-AH    PIC X.
000457                 88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
000458                 88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
000459                 88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
000460             20  AM-RETRO-RET-BASIS-AH     PIC X.
000461                 88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
000462                 88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
000463             20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
000464                 24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
000465                 24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
000466
000467     12  AM-COMMENTS.
000468         16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
000469
000470     12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
000471         16  AM-FLI-RETRO-SHARE-CODE       PIC X.
000472         16  AM-FLI-BILLING-CODE           PIC X.
000473         16  AM-FLI-ALT-STATE-CODE         PIC XX.
000474         16  AM-FLI-UNITED-IDENT           PIC X.
000475         16  AM-FLI-INTEREST-LOST-DATA.
000476             20  AM-FLI-BANK-NO            PIC X(5).
000477             20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
000478             20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
000479             20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
000480         16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
000481             20  AM-FLI-AGT                PIC X(9).
000482             20  AM-FLI-AGT-COMM-ACC       PIC X.
000483             20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
000484         16  FILLER                        PIC X(102).
000485
000486     12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
000487         16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
000488             20  AM-BENEFIT-DMD-CODE         PIC XX.
000489             20  AM-BENEFIT-DMD-TYPE         PIC X.
000490             20  AM-BENEFIT-DMD-REVISION     PIC XXX.
000491             20  AM-BENEFIT-DMD-REM-TERM     PIC X.
000492             20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
000493         16  FILLER                          PIC X(10).
000494******************************************************************
      *<<((file: ERCACCT))
000266
000267     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL162' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000268 VCOBOL-DUMMY-PROCEDURE.
000269
000270     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000271     MOVE '5'                   TO DC-OPTION-CODE.
000272     PERFORM 9700-DATE-LINK.
000273     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000274     MOVE DC-BIN-DATE-1         TO  BIN-CURRENT-DATE.
000275
000276     MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
000277
000278     MOVE -1                     TO DC-ELAPSED-MONTHS
000279     MOVE +0                     TO DC-ELAPSED-DAYS
000280     MOVE '6'                    TO DC-OPTION-CODE
000281     PERFORM 9700-DATE-LINK
000282     MOVE DC-BIN-DATE-2          TO CURRENT-MINUS1-SAVE
000283
000284     IF EIBCALEN NOT GREATER THAN ZERO
000285         GO TO 8800-UNAUTHORIZED-ACCESS.
000286
000287     IF PI-CALLING-PROGRAM = PGM-EL150
000288         MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000289         MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000290         MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000291         MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000292         MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000293         MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000294         MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000295         MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000296         MOVE +1                   TO SUB
000297         PERFORM  2000-ZERO-PI-TRLR-TABLE THRU 2010-EXIT
000298         MOVE LOW-VALUES           TO EL162AO
000299                                      PI-DISPLAYED-TRAILER-CODES
000300         MOVE 1                    TO PI-ACTV-SEQ
000301                                      PI-NEXT-TRLR-SUB
000302         GO TO 7000-BUILD-SCREEN.
000303
000304     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000305         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000306************ FIRST TIME IN FROM MENU, FORCE THE CLAIM
000307************ TO BE DESIGNATED THRU EL132.
000308             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000309             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000310             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000311             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000312             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000313             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000314             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000315             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000316             MOVE PGM-EL132            TO PGM-NAME
000317             GO TO 9300-XCTL
000318         ELSE
000319             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000320             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000321             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000322             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000323             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000324             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000325             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000326             MOVE SPACES               TO PI-SAVED-PROGRAM-6
000327             MOVE +1                   TO SUB
000328             PERFORM  2000-ZERO-PI-TRLR-TABLE THRU 2010-EXIT
000329             MOVE LOW-VALUES           TO EL162AO
000330                                       PI-DISPLAYED-TRAILER-CODES
000331             MOVE 1                    TO PI-ACTV-SEQ
000332                                          PI-NEXT-TRLR-SUB
000333             GO TO 7000-BUILD-SCREEN.
000334
000335     IF EIBAID = DFHCLEAR
000336         GO TO 9400-CLEAR.
000337
000338     
      * EXEC CICS HANDLE CONDITION
000339*        PGMIDERR(9600-PGMID-ERROR)
000340*        ERROR   (9990-ABEND)
000341*    END-EXEC.
      *    MOVE '"$L.                  ! " #00003149' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303033313439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000342
000343     IF PI-PROCESSOR-ID = 'LGXX'
000344         GO TO 0200-RECEIVE.
000345
000346     
      * EXEC CICS READQ TS
000347*        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000348*        INTO    (SECURITY-CONTROL)
000349*        LENGTH  (SC-COMM-LENGTH)
000350*        ITEM    (SC-ITEM)
000351*    END-EXEC.
      *    MOVE '*$II   L              ''   #00003157' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033313537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000352
000353     MOVE SC-CLAIMS-DISPLAY (2)    TO  PI-DISPLAY-CAP.
000354     MOVE SC-CLAIMS-UPDATE  (2)    TO  PI-MODIFY-CAP.
000355
000356     IF NOT MODIFY-CAP
000357         MOVE 'UPDATE'             TO  SM-READ
000358         PERFORM 9995-SECURITY-VIOLATION
000359         MOVE ER-0070              TO  EMI-ERROR
000360         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000361         GO TO 8100-SEND-INITIAL-MAP.
000362
000363     EJECT
000364 0200-RECEIVE.
000365
000366     MOVE LOW-VALUES TO EL162AI.
000367
000368     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000369         MOVE ER-0008            TO EMI-ERROR
000370         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000371         GO TO 8200-SEND-DATAONLY.
000372
000373     
      * EXEC CICS RECEIVE
000374*        MAP   (MAP-NAME)
000375*        MAPSET(MAPSET-NAME)
000376*        INTO  (EL162AI)
000377*    END-EXEC.
           MOVE LENGTH OF
            EL162AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003184' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL162AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000378
000379     IF ENTERPFL = ZERO
000380         GO TO 0300-CHECK-PFKEYS.
000381
000382     IF EIBAID NOT = DFHENTER
000383         MOVE ER-0004            TO EMI-ERROR
000384         GO TO 0320-INPUT-ERROR.
000385
000386     IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
000387         MOVE PF-VALUES (ENTERPFI) TO EIBAID
000388     ELSE
000389         MOVE ER-0029            TO EMI-ERROR
000390         GO TO 0320-INPUT-ERROR.
000391
000392 0300-CHECK-PFKEYS.
000393     IF EIBAID = DFHPF23
000394         GO TO 8810-PF23.
000395
000396     IF EIBAID = DFHPF24
000397         GO TO 9200-RETURN-MAIN-MENU.
000398
000399     IF EIBAID = DFHPF12
000400         GO TO 9500-PF12.
000401
000402     IF PI-COMPANY-ID = 'DMD'
000403         MOVE SPACE              TO PI-DMD-FORCE-ERROR.
000404
000405     IF EIBAID = DFHPF4
000406         IF PI-COMPANY-ID NOT = 'DMD'
000407             MOVE ER-0029        TO EMI-ERROR
000408             GO TO 0320-INPUT-ERROR
000409         ELSE
000410             MOVE 'X'            TO PI-DMD-FORCE-ERROR
000411             MOVE DFHENTER       TO EIBAID
000412             GO TO 0330-EDIT-DATA.
000413
000414     IF EIBAID = DFHPF5
000415         IF PI-RETURN-TO-PROGRAM = 'EL150'
000416             MOVE ER-0029        TO EMI-ERROR
000417             GO TO 0320-INPUT-ERROR
000418         ELSE
000419             MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
000420             MOVE PGM-EL132      TO PGM-NAME
000421             GO TO 9300-XCTL.
000422
000423     IF PI-CLAIM-NO = SPACES
000424        MOVE ER-0319             TO EMI-ERROR
000425        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000426        GO TO 8100-SEND-INITIAL-MAP.
000427
000428     IF SC-RECL (1) > 0 OR SC-STOPL (1) > 0 OR
000429        SC-RECL (2) > 0 OR SC-STOPL (2) > 0 OR
000430        SC-RECL (3) > 0 OR SC-STOPL (3) > 0 OR
000431        SC-RECL (4) > 0 OR SC-STOPL (4) > 0 OR
000432        UREL > 0  OR USENTL > 0  OR
000433        UFORML > 0  OR URECDTEL > 0
000434          IF EIBAID NOT = DFHENTER
000435             MOVE ER-0050            TO EMI-ERROR
000436             GO TO 0320-INPUT-ERROR.
000437
000438     IF EIBAID = DFHPF1
000439         GO TO 6000-BROWSE-FORWARD.
000440
000441     IF EIBAID = DFHPF2
000442         GO TO 6100-BROWSE-BACKWARD.
000443
000444     IF EIBAID = DFHPF6
000445         MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
000446         MOVE PGM-EL141          TO PGM-NAME
000447         GO TO 9300-XCTL.
000448
000449     IF EIBAID = DFHPF7
000450         MOVE PGM-EL1284          TO PGM-NAME
000451         GO TO 9300-XCTL.
000452
000453     IF EIBAID = DFHENTER
000454         GO TO 0330-EDIT-DATA.
000455
000456     MOVE ER-0029                TO EMI-ERROR.
000457
000458 0320-INPUT-ERROR.
000459     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000460
000461     MOVE AL-UNBON        TO ENTERPFA.
000462
000463     IF ENTERPFL = 0
000464         MOVE -1          TO URECDTEL
000465     ELSE
000466         MOVE -1          TO ENTERPFL.
000467
000468     GO TO 8200-SEND-DATAONLY.
000469
000470     EJECT
000471 0330-EDIT-DATA.
000472     SET SC-INDX  INDX           TO 1.
000473     SET INDX2                   TO 1.
000474
000475     perform 3000-BUILD-KEYS
000476     
      * EXEC CICS READ
000477*       DATASET (ELMSTR-FILE-ID)
000478*       RIDFLD  (PI-CLAM-KEY)
000479*       SET     (ADDRESS OF CLAIM-MASTER)
000480*       resp    (ws-response)
000481*    END-EXEC
      *    MOVE '&"S        E          (  N#00003287' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303033323837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000482
000483     if not resp-normal
000484        go to 8860-CLAIM-NOT-FOUND
000485     end-if
000486
000487     .
000488 0331-LOOP.
000489
000490     IF SC-RECL (SC-INDX) > ZERO
000491        MOVE SC-REC (SC-INDX)    TO DEEDIT-FIELD
000492        PERFORM 8600-DEEDIT
000493        MOVE '1'                 TO REC-DATA-SW
000494        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000495        MOVE '4'                 TO DC-OPTION-CODE
000496        PERFORM 9700-DATE-LINK
000497        IF DATE-CONVERSION-ERROR
000498           MOVE ER-0021          TO EMI-ERROR
000499           MOVE -1               TO SC-RECL (SC-INDX)
000500           MOVE AL-UABON         TO SC-RECA (SC-INDX)
000501           PERFORM 9900-ERROR-FORMAT
000502                                 THRU 9900-EXIT
000503        ELSE
000504           IF DC-BIN-DATE-1 > BIN-CURRENT-DATE
000505              MOVE ER-0539       TO EMI-ERROR
000506              MOVE -1            TO SC-RECL (SC-INDX)
000507              MOVE AL-UABON      TO SC-RECA (SC-INDX)
000508              PERFORM 9900-ERROR-FORMAT
000509                                 THRU 9900-EXIT
000510           ELSE
000511              MOVE DC-BIN-DATE-1 TO REC-DATE-SAVE (INDX)
000512              MOVE DEEDIT-FIELD-V0
000513                                 TO SC-REC-O (SC-INDX)
000514              INSPECT SC-REC (SC-INDX) CONVERTING ' ' TO '/'
000515           END-IF
000516        END-IF
000517     ELSE
000518        MOVE LOW-VALUES          TO REC-DATE-SAVE (INDX)
000519     END-IF
000520
000521     IF PI-COMPANY-ID = 'DMD'
000522       IF SC-RECL (SC-INDX) > 0
000523         IF REC-DATE-SAVE (INDX) < CURRENT-MINUS1-SAVE
000524         IF NOT PI-DMD-FORCED
000525           MOVE ER-7839        TO EMI-ERROR
000526           MOVE -1             TO SC-RECL (SC-INDX)
000527           MOVE AL-UABON       TO SC-RECA (SC-INDX)
000528           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000529         END-IF
000530         ELSE
000531         IF REC-DATE-SAVE (INDX) > BIN-CURRENT-DATE
000532           MOVE ER-0539          TO EMI-ERROR
000533           MOVE -1               TO SC-RECL (SC-INDX)
000534           MOVE AL-UABON         TO SC-RECA (SC-INDX)
000535           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000536
000537     IF SC-STOPL (SC-INDX) > ZERO
000538        MOVE SC-STOP (SC-INDX)    TO DEEDIT-FIELD
000539        PERFORM 8600-DEEDIT
000540        MOVE '1'                 TO REC-DATA-SW
000541        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000542        MOVE '4'                 TO DC-OPTION-CODE
000543        PERFORM 9700-DATE-LINK
000544        IF DATE-CONVERSION-ERROR
000545           MOVE ER-0021          TO EMI-ERROR
000546           MOVE -1               TO SC-STOPL (SC-INDX)
000547           MOVE AL-UABON         TO SC-STOPA (SC-INDX)
000548           PERFORM 9900-ERROR-FORMAT
000549                                 THRU 9900-EXIT
000550        ELSE
000551           IF DC-BIN-DATE-1 > BIN-CURRENT-DATE
000552              MOVE ER-0895       TO EMI-ERROR
000553              MOVE -1            TO SC-STOPL (SC-INDX)
000554              MOVE AL-UABON      TO SC-STOPA (SC-INDX)
000555              PERFORM 9900-ERROR-FORMAT
000556                                 THRU 9900-EXIT
000557           ELSE
000558              MOVE DC-BIN-DATE-1 TO STOP-DATE-SAVE (INDX2)
000559              MOVE DEEDIT-FIELD-V0 TO SC-STOP-O (SC-INDX)
000560              INSPECT SC-STOP (SC-INDX) CONVERTING ' ' TO '/'
000561           END-IF
000562        END-IF
000563     ELSE
000564        MOVE LOW-VALUES          TO STOP-DATE-SAVE (INDX2)
000565     END-IF.
000566
000567     SET SC-INDX
000568            INDX2
000569            INDX UP BY 1.
000570
000571     IF SC-INDX NOT = 5
000572        GO TO 0331-LOOP.
000573
000574     IF URECDTEL > 0
000575        MOVE URECDTEI            TO DEEDIT-FIELD
000576        PERFORM 8600-DEEDIT
000577        MOVE '1'                 TO UNSOL-DATA-SW
000578        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000579        MOVE '4'                 TO DC-OPTION-CODE
000580        PERFORM 9700-DATE-LINK
000581        IF DATE-CONVERSION-ERROR
000582           MOVE ER-0021          TO EMI-ERROR
000583           MOVE -1               TO URECDTEL
000584           MOVE AL-UABON         TO URECDTEA
000585           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000586        ELSE
000587           MOVE DC-BIN-DATE-1    TO URECDTE-SAVE
000588           MOVE DEEDIT-FIELD-V0  TO URECDTEO
000589           INSPECT URECDTEI CONVERTING ' ' TO '/'
000590           if (urecdte-save < cl-incurred-dt)
000591              or (urecdte-save < cl-reported-dt)
000592              or (urecdte-save < cl-file-establish-dt)
000593              or (urecdte-save < cl-cert-eff-dt)
000594              or (urecdte-save < CURRENT-MINUS1-SAVE)
000595              if (pi-receive-dt-cnt > 0)
000596                 and (urecdte-save = pi-prev-rec-dt)
000597                 move 0          to pi-receive-dt-cnt
000598                 move low-values to pi-prev-rec-dt
000599              else
000600                 move 1          to pi-receive-dt-cnt
000601                 move urecdte-save
000602                                 to pi-prev-rec-dt
000603                 move er-1825    to emi-error
000604                 move -1         to urecdtel
000605                 move al-uabon   to urecdtea
000606                 perform 9900-error-format
000607                                 thru 9900-exit
000608              end-if
000609           end-if
000610        end-if
000611     end-if
000612
000613     IF PI-COMPANY-ID = 'DMD'
000614       IF URECDTEL > 0
000615         IF URECDTE-SAVE < CURRENT-MINUS1-SAVE
000616         IF NOT PI-DMD-FORCED
000617           MOVE ER-7839        TO EMI-ERROR
000618           MOVE -1             TO URECDTEL
000619           MOVE AL-UABON       TO URECDTEA
000620           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000621         END-IF
000622         ELSE
000623         IF URECDTE-SAVE > BIN-CURRENT-DATE
000624           MOVE ER-0539          TO EMI-ERROR
000625           MOVE -1               TO URECDTEL
000626           MOVE AL-UABON         TO URECDTEA
000627           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000628
000629     IF USENTL > 0
000630        MOVE USENTI              TO DEEDIT-FIELD
000631        PERFORM 8600-DEEDIT
000632        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
000633        MOVE '4'                 TO DC-OPTION-CODE
000634        PERFORM 9700-DATE-LINK
000635        IF DATE-CONVERSION-ERROR
000636           MOVE ER-0021          TO EMI-ERROR
000637           MOVE -1               TO USENTL
000638           MOVE AL-UABON         TO USENTA
000639           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000640          ELSE
000641           MOVE DC-BIN-DATE-1    TO USENT-SAVE
000642           MOVE DEEDIT-FIELD-V0  TO USENTO
000643           INSPECT USENTI CONVERTING ' ' TO '/'
000644        ELSE
000645           MOVE LOW-VALUES       TO USENT-SAVE.
000646
000647     IF USENT-SAVE   NOT = LOW-VALUES AND
000648        URECDTE-SAVE NOT = LOW-VALUES
000649          IF USENT-SAVE GREATER THAN URECDTE-SAVE
000650             MOVE ER-0514        TO EMI-ERROR
000651             MOVE -1             TO URECDTEL
000652             MOVE AL-UABON       TO URECDTEA
000653             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000654
000655     IF URECDTE-SAVE NOT = LOW-VALUES
000656          IF URECDTE-SAVE GREATER THAN BIN-CURRENT-DATE
000657             MOVE ER-0539        TO EMI-ERROR
000658             MOVE -1             TO URECDTEL
000659             MOVE AL-UABON       TO URECDTEA
000660             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000661
000662     IF URECDTEL = ZEROS
000663        IF USENTL > ZEROS  OR
000664*00570            UBYL   > ZEROS  OR
000665           UFORML > ZEROS  OR
000666           UREL   > ZEROS
000667              MOVE ER-0321          TO EMI-ERROR
000668              MOVE -1               TO URECDTEL
000669              PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT.
000670
000671     IF AUTHRCVL > 0
000672        IF AUTHRCVI = 'N' OR 'Y'
000673           CONTINUE
000674        ELSE
000675           MOVE ER-0287        TO EMI-ERROR
000676           MOVE -1             TO AUTHRCVL
000677           MOVE AL-UABON       TO AUTHRCVA
000678           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000679        END-IF
000680     END-IF.
000681
000682
000683     IF NOT EMI-NO-ERRORS
000684        GO TO 8200-SEND-DATAONLY.
000685
000686     IF NOT MODIFY-CAP
000687         MOVE ER-0070            TO EMI-ERROR
000688         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000689         MOVE LOW-VALUES TO EL162AO
000690         GO TO 8100-SEND-INITIAL-MAP.
000691
000692     EJECT
000693     PERFORM 3000-BUILD-KEYS.
000694
000695     IF REC-DATA
000696        PERFORM 4000-UPDATE-RECEIVE-DATES THRU 4099-EXIT
000697                VARYING SC-INDX FROM 1 BY 1 UNTIL
000698                SC-INDX = 5.
000699
000700     PERFORM 4100-CREATE-NEW-TRLR THRU 4199-EXIT.
000701
000702     MOVE ER-ZEROS               TO EMI-ERROR.
000703     MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I
000704                                    ZIPI PHONEI.
000705
000706     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000707
000708     MOVE -1                     TO SC-RECL (1).
000709     GO TO 8200-SEND-DATAONLY.
000710
000711     EJECT
000712 2000-ZERO-PI-TRLR-TABLE.
000713
000714     MOVE +0                     TO PI-TRLRS (SUB).
000715     ADD +1 TO SUB.
000716
000717     IF SUB > +50
000718        MOVE +0                  TO SUB
000719        move 0                   to pi-receive-dt-cnt
000720        move low-values          to pi-prev-rec-dt
000721        GO TO 2010-EXIT.
000722
000723     GO TO 2000-ZERO-PI-TRLR-TABLE.
000724
000725 2010-EXIT.
000726     EXIT.
000727
000728 2200-edit-receive-date.
000729***  The ELMSTR has been read at this point and each of the
000730***  received dates will be edited here.
000731
000732     move sc-trlrno(sc-indx)     to pi-actv-seq
000733
000734     
      * EXEC CICS READ
000735*       DATASET (ELTRLR-FILE-ID)
000736*       RIDFLD  (PI-ACTV-KEY)
000737*       SET     (ADDRESS OF ACTIVITY-TRAILERS)
000738*       resp    (ws-response)
000739*    END-EXEC
      *    MOVE '&"S        E          (  N#00003545' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303033353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000740
000741     if not resp-normal
000742        display 'error eltrlr read ' ws-response
000743        go to 2200-exit
000744     end-if
000745     if not correspondence-tr
000746        display ' is not corres trailer '
000747        go to 2200-exit
000748     end-if
000749
000750     .
000751 2200-exit.
000752     exit.
000753
000754 3000-BUILD-KEYS.
000755     MOVE PI-CLAIM-NO            TO PI-CLAM-CLAIM
000756                                    PI-ACTV-CLAIM
000757                                    CLAIMNOI.
000758
000759     MOVE PI-CERT-NO             TO PI-CLAM-CERT-NO
000760                                    PI-ACTV-CERT-NO
000761                                    CERTNOI.
000762
000763     MOVE PI-COMPANY-CD          TO PI-CLAM-COMP-CD
000764                                    PI-ACTV-COMP-CD.
000765
000766     MOVE PI-CARRIER             TO PI-CLAM-CARRIER
000767                                    PI-ACTV-CARRIER
000768                                    CARRI.
000769     EJECT
000770 4000-UPDATE-RECEIVE-DATES.
000771
000772     IF SC-RECL (SC-INDX) = ZERO AND SC-STOPL (SC-INDX)
000773        GO TO 4099-EXIT.
000774
000775     MOVE SC-TRLRNO (SC-INDX)    TO PI-ACTV-SEQ.
000776
000777*00648      IF SC-RECL (SC-INDX) = ZERO
000778*00649          EXEC CICS READ
000779*00650               DATASET (ELTRLR-FILE-ID)
000780*00651               RIDFLD  (PI-ACTV-KEY)
000781*00652               SET     (ADDRESS OF ACTIVITY-TRAILERS)
000782*00653          END-EXEC
000783*00654         ELSE
000784         
      * EXEC CICS READ
000785*             DATASET(ELTRLR-FILE-ID)
000786*             RIDFLD (PI-ACTV-KEY)
000787*             SET    (ADDRESS OF ACTIVITY-TRAILERS)
000788*             UPDATE
000789*        END-EXEC.
      *    MOVE '&"S        EU         (   #00003595' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033353935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000790
000791     SET INDX2 TO SC-INDX.
000792
000793     IF SC-STOPL (SC-INDX) = ZEROS
000794         GO TO 4000-BYPASS-STOP-DATE
000795     END-IF.
000796
000797     IF (CORRESPONDENCE-TR AND
000798         STOP-DATE-SAVE (INDX2) LESS THAN AT-LETTER-SENT-DT)
000799              OR
000800        (FORM-CONTROL-TR AND
000801         STOP-DATE-SAVE (INDX2) LESS THAN AT-FORM-SEND-ON-DT)
000802           MOVE ER-0896          TO EMI-ERROR
000803           MOVE -1               TO SC-STOPL (SC-INDX)
000804           MOVE AL-UABON         TO SC-STOPA (SC-INDX)
000805           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000806           
      * EXEC CICS SYNCPOINT
000807*               ROLLBACK
000808*          END-EXEC
      *    MOVE '6"R                   !   #00003617' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303033363137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000809           GO TO 8200-SEND-DATAONLY
000810     END-IF.
000811
000812     IF CORRESPONDENCE-TR
000813        MOVE STOP-DATE-SAVE (INDX2) TO AT-STOP-LETTER-DT
000814        MOVE PI-PROCESSOR-ID     TO AT-CORR-LAST-UPDATED-BY
000815        MOVE BIN-CURRENT-DATE    TO AT-CORR-LAST-MAINT-DT
000816     END-IF.
000817
000818     IF FORM-CONTROL-TR
000819        MOVE STOP-DATE-SAVE (INDX2) TO AT-STOP-FORM-DT
000820        MOVE PI-PROCESSOR-ID     TO AT-FORM-LAST-UPDATED-BY
000821        MOVE BIN-CURRENT-DATE    TO AT-FORM-LAST-MAINT-DT
000822     END-IF.
000823     MOVE AL-UANOF               TO SC-STOPA (SC-INDX).
000824
000825 4000-BYPASS-STOP-DATE.
000826
000827     IF SC-RECL (SC-INDX) = ZEROS
000828*00673         GO TO 4099-EXIT.
000829        GO TO 4090-REWRITE
000830     END-IF.
000831
000832     SET INDX TO SC-INDX.
000833
000834     IF (CORRESPONDENCE-TR AND
000835         REC-DATE-SAVE (INDX)  LESS THAN AT-LETTER-SENT-DT)
000836              OR
000837        (FORM-CONTROL-TR AND
000838         REC-DATE-SAVE (INDX)  LESS THAN AT-FORM-SEND-ON-DT)
000839           MOVE ER-0514          TO EMI-ERROR
000840           MOVE -1               TO SC-RECL (SC-INDX)
000841           MOVE AL-UABON         TO SC-RECA (SC-INDX)
000842           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000843           
      * EXEC CICS SYNCPOINT
000844*               ROLLBACK
000845*          END-EXEC
      *    MOVE '6"R                   !   #00003654' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303033363534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000846           GO TO 8200-SEND-DATAONLY.
000847
000848     IF CORRESPONDENCE-TR
000849        MOVE REC-DATE-SAVE (INDX)   TO AT-LETTER-ANSWERED-DT
000850        MOVE PI-PROCESSOR-ID        TO AT-CORR-LAST-UPDATED-BY
000851        MOVE BIN-CURRENT-DATE       TO AT-CORR-LAST-MAINT-DT.
000852
000853     IF FORM-CONTROL-TR
000854        MOVE PI-PROCESSOR-ID        TO AT-FORM-LAST-UPDATED-BY
000855        MOVE BIN-CURRENT-DATE       TO AT-FORM-LAST-MAINT-DT
000856        IF SC-TYPE (SC-INDX) = 'C'
000857           MOVE REC-DATE-SAVE (INDX)   TO AT-FORM-ANSWERED-DT
000858        ELSE
000859        IF SC-TYPE (SC-INDX) = 'E'
000860           MOVE REC-DATE-SAVE (INDX)   TO AT-EMP-FORM-ANSWERED-DT
000861        ELSE
000862        IF SC-TYPE (SC-INDX) = 'P'
000863           MOVE REC-DATE-SAVE (INDX)   TO AT-PHY-FORM-ANSWERED-DT.
000864
000865     MOVE AL-UANOF               TO SC-RECA (SC-INDX).
000866
000867 4090-REWRITE.
000868
000869     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
000870
000871     IF PI-COMPANY-ID = 'DMD'
000872         MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY.
000873
000874     
      * EXEC CICS REWRITE
000875*         FROM   (ACTIVITY-TRAILERS)
000876*         DATASET(ELTRLR-FILE-ID)
000877*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003685' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033363835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000878
000879 4099-EXIT.
000880      EXIT.
000881
000882     EJECT
000883 4100-CREATE-NEW-TRLR.
000884
000885     
      * EXEC CICS READ
000886*         DATASET(ELMSTR-FILE-ID)
000887*         SET    (ADDRESS OF CLAIM-MASTER)
000888*         RIDFLD (PI-CLAM-KEY)
000889*         UPDATE
000890*    END-EXEC.
      *    MOVE '&"S        EU         (   #00003696' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033363936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000891
000892     IF FILEL > ZEROS
000893        MOVE FILEI               TO CL-FILE-LOCATION.
000894
000895     IF NOT UNSOL-DATA
000896        GO TO 4150-REWRITE-CLAIM.
000897
000898     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
000899
000900     
      * EXEC CICS GETMAIN
000901*         SET    (ADDRESS OF ACTIVITY-TRAILERS)
000902*         INITIMG(GETMAIN-SPACE)
000903*         LENGTH (ACTV-LENGTH)
000904*    END-EXEC.
      *    MOVE ',"IL                  $   #00003711' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033373131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTV-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000905
000906     MOVE 'AT'                   TO AT-RECORD-ID.
000907     MOVE  4                     TO AT-TRAILER-TYPE.
000908     MOVE BIN-CURRENT-DATE       TO AT-RECORDED-DT
000909                                    AT-CORR-LAST-MAINT-DT
000910
000911     IF UBYL > +0
000912        MOVE UBYI                TO AT-RECORDED-BY
000913                                    AT-CORR-LAST-UPDATED-BY
000914     ELSE
000915        MOVE PI-PROCESSOR-ID     TO AT-RECORDED-BY
000916                                    AT-CORR-LAST-UPDATED-BY.
000917
000918     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
000919     MOVE PI-ACTV-KEY            TO AT-CONTROL-PRIMARY.
000920     MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.
000921     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
000922     MOVE USENT-SAVE             TO AT-LETTER-SENT-DT.
000923     MOVE LOW-VALUES             TO AT-RECEIPT-FOLLOW-UP
000924                                    AT-AUTO-RE-SEND-DT
000925                                    AT-INITIAL-PRINT-DATE
000926                                    AT-RESEND-PRINT-DATE.
000927     MOVE URECDTE-SAVE           TO AT-LETTER-ANSWERED-DT.
000928     MOVE ZEROS                  TO AT-LETTER-ARCHIVE-NO.
000929     MOVE '1'                    TO AT-LETTER-ORIGIN.
000930
000931     IF UFORML > 0
000932        MOVE UFORMI              TO AT-STD-LETTER-FORM
000933       ELSE
000934        MOVE SPACES              TO AT-STD-LETTER-FORM.
000935
000936     IF UREL > 0
000937        MOVE UREI                TO AT-REASON-TEXT
000938       ELSE
000939        MOVE SPACES              TO AT-REASON-TEXT.
000940
000941     IF AUTHRCVL > 0
000942        MOVE AUTHRCVI            TO AT-AUTH-RCVD
000943     ELSE
000944        MOVE SPACES              TO AT-AUTH-RCVD
000945     END-IF.
000946
000947
000948     MOVE SPACES                 TO AT-ADDRESSEE-NAME
000949                                    AT-ADDRESEE-TYPE.
000950     MOVE ZEROS                  TO AT-ADDRESS-REC-SEQ-NO.
000951     MOVE 'U'                    TO AT-CORR-SOL-UNSOL.
000952
000953     
      * EXEC CICS WRITE
000954*         DATASET(ELTRLR-FILE-ID)
000955*         FROM   (ACTIVITY-TRAILERS)
000956*         RIDFLD (AT-CONTROL-PRIMARY)
000957*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003764' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033373634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000958
000959     MOVE SPACES                 TO URECDTEI
000960                                    USENTI
000961*00795                                     UBYI
000962                                    UFORMI
000963                                    UREI.
000964
000965     MOVE AL-UANOF               TO URECDTEA
000966                                    USENTA
000967*00801                                     UBYA
000968                                    UFORMA
000969                                    UREA.
000970
000971     MOVE PI-PROCESSOR-ID        TO UBYI.
000972
000973 4150-REWRITE-CLAIM.
000974     
      * EXEC CICS HANDLE CONDITION
000975*        DUPKEY (4199-EXIT)
000976*    END-EXEC.
      *    MOVE '"$$                   ! # #00003785' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303033373835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000977
000978     MOVE PI-PROCESSOR-ID   TO  CL-LAST-MAINT-USER.
000979     MOVE EIBTIME           TO  CL-LAST-MAINT-HHMMSS.
000980     MOVE BIN-CURRENT-DATE  TO  CL-LAST-MAINT-DT.
000981     MOVE '2'               TO  CL-LAST-MAINT-TYPE.
000982
000983     IF PI-COMPANY-ID = 'DMD'
000984         MOVE 11                 TO CL-ACTIVITY-CODE
000985         MOVE BIN-CURRENT-DATE   TO CL-ACTIVITY-MAINT-DT
000986         MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE
000987         MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.
000988
000989     
      * EXEC CICS REWRITE
000990*         DATASET(ELMSTR-FILE-ID)
000991*         FROM   (CLAIM-MASTER)
000992*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003800' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033383030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000993
000994 4199-EXIT.
000995      EXIT.
000996
000997     EJECT
000998 4200-LOCATE-ADDRESS.
000999     IF ADDR-TYPE = '3' AND ADDR-SEQ = ZEROS
001000        GO TO 4210-READ-ACCOUNT.
001001
001002     IF ADDR-TYPE = SPACES OR ADDR-SEQ = ZEROS
001003        GO TO 4290-ERROR.
001004
001005     MOVE ADDR-SEQ               TO PI-ACTV-SEQ.
001006
001007     
      * EXEC CICS HANDLE CONDITION
001008*         NOTFND(4290-ERROR)
001009*    END-EXEC.
      *    MOVE '"$I                   ! $ #00003818' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303033383138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001010
001011     
      * EXEC CICS READ
001012*         DATASET(ELTRLR-FILE-ID)
001013*         RIDFLD (PI-ACTV-KEY)
001014*         SET    (ADDRESS OF ACTIVITY-TRAILERS)
001015*    END-EXEC.
      *    MOVE '&"S        E          (   #00003822' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033383232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001016
001017     MOVE AT-MAIL-TO-NAME        TO ADDR1O.
001018     MOVE AT-ADDRESS-LINE-1      TO ADDR2O.
001019     MOVE AT-ADDRESS-LINE-2      TO ADDR3O.
001020*    MOVE AT-CITY-STATE          TO ADDR4O.
001021     STRING AT-CITY ' ' AT-STATE
001022        DELIMITED BY '  ' INTO ADDR4O
001023     END-STRING
001024     MOVE AT-PHONE-NO            TO PHONEO.
001025     INSPECT PHONEI CONVERTING ' ' TO '-'.
001026
001027     IF AT-CANADIAN-POST-CODE
001028         MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
001029         MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
001030         MOVE SPACES             TO WS-DASH-CAN
001031                                    WS-CAN-FILLER
001032         MOVE WS-CANADIAN-POSTAL-CODES
001033                                 TO ZIPO
001034     ELSE
001035         MOVE AT-ZIP-CODE        TO WS-ZIP-CODE
001036         IF AT-ZIP-PLUS4 = SPACES OR ZEROS
001037             MOVE SPACES         TO WS-ZIP-PLUS4
001038                                    WS-DASH
001039             MOVE WS-ZIP         TO ZIPO
001040         ELSE
001041             MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4
001042             MOVE '-'            TO WS-DASH
001043             MOVE WS-ZIP         TO ZIPO.
001044
001045     GO TO 4299-EXIT.
001046
001047 4210-READ-ACCOUNT.
001048     MOVE PI-COMPANY-CD          TO ACCT-CO.
001049     MOVE PI-CARRIER             TO ACCT-CARRIER.
001050     MOVE PI-GROUPING            TO ACCT-GROUPING.
001051     MOVE PI-STATE               TO ACCT-STATE.
001052     MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.
001053     MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DATE.
001054
001055     
      * EXEC CICS HANDLE CONDITION
001056*         NOTOPEN(8880-ACCT-NOT-OPEN)
001057*         NOTFND (4250-ACCT-NOT-FOUND)
001058*    END-EXEC.
      *    MOVE '"$JI                  ! % #00003866' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303033383636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001059
001060     
      * EXEC CICS STARTBR
001061*         RIDFLD   (ACCT-KEY)
001062*         DATASET  (ERACCT-FILE-ID)
001063*         KEYLENGTH(13)
001064*         GENERIC
001065*    END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00003871' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303033383731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001066
001067     MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.
001068
001069 4230-READNEXT.
001070     
      * EXEC CICS READNEXT
001071*         DATASET(ERACCT-FILE-ID)
001072*         SET    (ADDRESS OF ACCOUNT-MASTER)
001073*         RIDFLD (ACCT-KEY)
001074*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003881' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303033383831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001075
001076     IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
001077        GO TO 4250-ACCT-NOT-FOUND.
001078
001079     IF PI-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT AND
001080        PI-CERT-EFF-DT     LESS THAN AM-EXPIRATION-DT
001081        NEXT SENTENCE
001082      ELSE
001083        GO TO 4230-READNEXT.
001084
001085     MOVE AM-NAME                TO ADDR1O.
001086     MOVE AM-PERSON              TO ADDR2O.
001087     MOVE AM-ADDRS               TO ADDR3O.
001088*    MOVE AM-CITY                TO ADDR4O.
001089     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
001090        DELIMITED BY '  ' INTO ADDR4O
001091     END-STRING
001092
001093     MOVE AM-TEL-NO              TO WS-WORK-PHONE.
001094     INSPECT WS-WORK-PHONE CONVERTING ' ' TO '0'.
001095     MOVE WS-NUMERIC-PHONE       TO PHONEO.
001096     INSPECT PHONEI CONVERTING ' ' TO '-'.
001097
001098     IF AM-CANADIAN-POST-CODE
001099         MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
001100         MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
001101         MOVE SPACES             TO WS-DASH-CAN
001102                                    WS-CAN-FILLER
001103         MOVE WS-CANADIAN-POSTAL-CODES
001104                                 TO ZIPO
001105     ELSE
001106         MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
001107         IF AM-ZIP-PLUS4 = SPACES OR ZEROS
001108             MOVE SPACES         TO WS-ZIP-PLUS4
001109                                    WS-DASH
001110             MOVE WS-ZIP         TO ZIPO
001111         ELSE
001112             MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
001113             MOVE '-'            TO WS-DASH
001114             MOVE WS-ZIP         TO ZIPO.
001115
001116     PERFORM 4240-ENDBR.
001117
001118     GO TO 4299-EXIT.
001119
001120 4240-ENDBR.
001121     
      * EXEC CICS ENDBR
001122*         DATASET(ERACCT-FILE-ID)
001123*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003932' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033393332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001124
001125 4250-ACCT-NOT-FOUND.
001126     PERFORM 4240-ENDBR.
001127     MOVE ER-0179                TO EMI-ERROR.
001128     MOVE -1                     TO SC-RECL (1).
001129     MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I
001130                                    ZIPI PHONEI.
001131
001132     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001133
001134 4290-ERROR.
001135     MOVE ER-0322                TO EMI-ERROR.
001136     MOVE -1                     TO SC-RECL (1).
001137     MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I
001138                                    ZIPI PHONEI.
001139
001140     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001141
001142 4299-EXIT.
001143      EXIT.
001144
001145     EJECT
001146 6000-BROWSE-FORWARD.
001147     IF PI-ACTV-SAVE NOT = PI-ACTV-PARTIAL
001148        MOVE ER-0066             TO EMI-ERROR
001149        MOVE -1                  TO SC-RECL (1)
001150        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001151        GO TO 8200-SEND-DATAONLY.
001152
001153     MOVE LOW-VALUES             TO EL162AI.
001154     ADD 1 TO PI-ACTV-SEQ.
001155
001156     GO TO 7000-BUILD-SCREEN.
001157
001158 6100-BROWSE-BACKWARD.
001159
001160     IF PI-NEXT-TRLR-SUB > 8
001161        SUBTRACT 4 FROM PI-NEXT-TRLR-SUB.
001162
001163     MOVE PI-NEXT-TRLR-SUB       TO SUB.
001164
001165     IF SUB = 1 OR 5
001166        MOVE ER-0067             TO EMI-ERROR
001167        MOVE -1                  TO SC-RECL (1)
001168        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001169        MOVE  1                  TO SUB
001170        MOVE  +1                 TO PI-NEXT-TRLR-SUB
001171     ELSE
001172        SUBTRACT 4 FROM SUB
001173        MOVE SUB                 TO PI-NEXT-TRLR-SUB.
001174
001175     MOVE LOW-VALUES             TO EL162AI
001176                                    TRL-LINES.
001177     MOVE PI-ACTV-SAVE           TO PI-ACTV-PARTIAL.
001178
001179     MOVE PI-TRLRS (SUB)         TO PI-ACTV-SEQ.
001180
001181     GO TO 7000-BUILD-SCREEN.
001182
001183     EJECT
001184 6200-FORMAT-DATA.
001185
001186     IF FORM-CONTROL-TR
001187        GO TO 6250-FORMAT-FORM.
001188
001189     IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
001190        MOVE AT-LETTER-ANSWERED-DT TO DC-BIN-DATE-1
001191        MOVE SPACES                TO DC-OPTION-CODE
001192        PERFORM 9700-DATE-LINK
001193        MOVE DC-GREG-DATE-1-EDIT   TO SC-REC (SC-INDX).
001194
001195     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
001196        MOVE AT-STOP-LETTER-DT     TO DC-BIN-DATE-1
001197        MOVE SPACES                TO DC-OPTION-CODE
001198        PERFORM 9700-DATE-LINK
001199        MOVE DC-GREG-DATE-1-EDIT   TO SC-STOP (SC-INDX)
001200     END-IF.
001201
001202     IF AT-LETTER-SENT-DT NOT = LOW-VALUES
001203        MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1
001204        MOVE SPACES              TO DC-OPTION-CODE
001205        PERFORM 9700-DATE-LINK
001206        MOVE DC-GREG-DATE-1-EDIT TO SC-SENT (SC-INDX).
001207
001208     IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
001209        MOVE AT-AUTO-RE-SEND-DT  TO DC-BIN-DATE-1
001210        MOVE SPACES              TO DC-OPTION-CODE
001211        PERFORM 9700-DATE-LINK
001212        MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
001213
001214     IF AT-RECEIPT-FOLLOW-UP NOT = LOW-VALUES
001215        MOVE AT-RECEIPT-FOLLOW-UP TO DC-BIN-DATE-1
001216        MOVE SPACES               TO DC-OPTION-CODE
001217        PERFORM 9700-DATE-LINK
001218        MOVE DC-GREG-DATE-1-EDIT  TO SC-FOLLOWUP (SC-INDX).
001219
001220     MOVE AT-STD-LETTER-FORM     TO SC-FORM (SC-INDX).
001221
001222     IF INSURED-ADDRESEE
001223        MOVE 'INSURED'           TO SC-TO (SC-INDX)
001224     ELSE
001225     IF BENEFICIARY-ADDRESEE
001226        MOVE 'BENEFI '           TO SC-TO (SC-INDX)
001227     ELSE
001228     IF ACCOUNT-ADDRESEE
001229        MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
001230     ELSE
001231     IF PHYSICIAN-ADDRESEE
001232        MOVE 'DOCTOR'            TO SC-TO (SC-INDX)
001233     ELSE
001234     IF EMPLOYER-ADDRESEE
001235        MOVE 'EMPLOYER'          TO SC-TO (SC-INDX)
001236     ELSE
001237     IF OTHER-ADDRESEE-1
001238        MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
001239     ELSE
001240        MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
001241
001242     IF AT-LETTER-ARCHIVE-NO NOT = +0
001243        MOVE AT-LETTER-ARCHIVE-NO TO SC-ARCHIVE (SC-INDX)
001244     ELSE
001245        MOVE SPACES              TO SC-ARCHIVE (SC-INDX).
001246
001247     MOVE 'L'                    TO SC-TYPE   (SC-INDX).
001248
001249     MOVE AT-REASON-TEXT         TO SC-REASON (SC-INDX).
001250
001251*    IF PI-COMPANY-ID = 'DMD'
001252*       IF AT-DMD-LETTER-PURGED
001253*          MOVE AT-REASON-TEXT          TO WS-REASON
001254*          MOVE AT-DMD-LETTER-PURGE-DT  TO DC-BIN-DATE-1
001255*          MOVE SPACES                  TO DC-OPTION-CODE
001256*          PERFORM 9700-DATE-LINK
001257*          MOVE 'PURGED -'              TO WS-REASON (51:9)
001258*          MOVE DC-GREG-DATE-1-EDIT     TO WS-REASON (60:11)
001259*          MOVE WS-REASON               TO SC-REASON (SC-INDX).
001260*
001261     MOVE AT-RECORDED-BY         TO SC-BY     (SC-INDX).
001262     MOVE AT-SEQUENCE-NO         TO SC-TRLRNO (SC-INDX).
001263     MOVE AL-UANOF               TO SC-RECA   (SC-INDX)
001264                                    SC-STOPA  (SC-INDX).
001265
001266     GO TO 6299-EXIT.
001267
001268 6250-FORMAT-FORM.
001269
001270     IF AT-STOP-FORM-DT NOT = LOW-VALUES AND SPACES
001271        MOVE AT-STOP-FORM-DT       TO DC-BIN-DATE-1
001272        MOVE SPACES                TO DC-OPTION-CODE
001273        PERFORM 9700-DATE-LINK
001274        MOVE DC-GREG-DATE-1-EDIT   TO SC-STOP (SC-INDX)
001275     END-IF.
001276
001277     IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES
001278        IF (AT-FORM-ANSWERED-DT = LOW-VALUES)
001279                         OR
001280           (AT-FORM-ANSWERED-DT NOT = LOW-VALUES  AND
001281            AT-FORM-ANSWERED-DT = BIN-CURRENT-DATE)
001282             NEXT SENTENCE
001283         ELSE
001284             GO TO 6260-CHECK-EMPLOYER-FORM
001285     ELSE
001286        GO TO 6260-CHECK-EMPLOYER-FORM.
001287
001288     IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
001289        MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1
001290        MOVE SPACES              TO DC-OPTION-CODE
001291        PERFORM 9700-DATE-LINK
001292        MOVE DC-GREG-DATE-1-EDIT TO SC-REC (SC-INDX).
001293
001294     IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES
001295        MOVE AT-FORM-SEND-ON-DT  TO DC-BIN-DATE-1
001296        MOVE SPACES              TO DC-OPTION-CODE
001297        PERFORM 9700-DATE-LINK
001298        MOVE DC-GREG-DATE-1-EDIT TO SC-SENT (SC-INDX).
001299
001300     IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
001301        MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1
001302        MOVE SPACES              TO DC-OPTION-CODE
001303        PERFORM 9700-DATE-LINK
001304        MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
001305
001306     IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
001307        MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1
001308        MOVE SPACES                 TO DC-OPTION-CODE
001309        PERFORM 9700-DATE-LINK
001310        MOVE DC-GREG-DATE-1-EDIT TO SC-FOLLOWUP (SC-INDX).
001311
001312     IF AT-FORM-TYPE = '1'
001313        MOVE 'INIT'              TO SC-FORM (SC-INDX)
001314     ELSE
001315        MOVE 'PROG'              TO SC-FORM (SC-INDX).
001316
001317     IF FORM-TO-INSURED
001318        MOVE 'INSURED'           TO SC-TO (SC-INDX)
001319     ELSE
001320     IF FORM-TO-ACCOUNT
001321        MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
001322     ELSE
001323     IF FORM-TO-OTHER-1
001324        MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
001325     ELSE
001326     IF FORM-TO-OTHER-2
001327        MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
001328
001329     MOVE 'CLAIMANT'             TO SC-ARCHIVE (SC-INDX).
001330     MOVE 'C'                    TO SC-TYPE    (SC-INDX).
001331     MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)
001332     MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).
001333     MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).
001334     MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).
001335     MOVE AL-UANOF               TO SC-RECA    (SC-INDX).
001336     MOVE AL-UANOF               TO SC-STOPA   (SC-INDX).
001337
001338 6260-CHECK-EMPLOYER-FORM.
001339
001340     IF AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES
001341        IF (AT-EMP-FORM-ANSWERED-DT = LOW-VALUES)
001342                          OR
001343           (AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND
001344            AT-EMP-FORM-ANSWERED-DT = BIN-CURRENT-DATE)
001345              NEXT SENTENCE
001346          ELSE
001347              GO TO 6280-CHECK-PHYSICIAN-FORM
001348     ELSE
001349        GO TO 6280-CHECK-PHYSICIAN-FORM.
001350
001351     IF SC-TYPE (SC-INDX) = 'C'
001352        SET SC-INDX UP BY 1
001353        ADD +1 TO PI-NEXT-TRLR-SUB.
001354
001355     IF AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES
001356        MOVE AT-EMP-FORM-ANSWERED-DT TO DC-BIN-DATE-1
001357        MOVE SPACES                  TO DC-OPTION-CODE
001358        PERFORM 9700-DATE-LINK
001359        MOVE DC-GREG-DATE-1-EDIT     TO SC-REC (SC-INDX).
001360
001361     IF AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES
001362        MOVE AT-EMP-FORM-SEND-ON-DT TO DC-BIN-DATE-1
001363        MOVE SPACES                 TO DC-OPTION-CODE
001364        PERFORM 9700-DATE-LINK
001365        MOVE DC-GREG-DATE-1-EDIT    TO SC-SENT (SC-INDX).
001366
001367     IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
001368        MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1
001369        MOVE SPACES              TO DC-OPTION-CODE
001370        PERFORM 9700-DATE-LINK
001371        MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
001372
001373     IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
001374        MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1
001375        MOVE SPACES                 TO DC-OPTION-CODE
001376        PERFORM 9700-DATE-LINK
001377        MOVE DC-GREG-DATE-1-EDIT    TO SC-FOLLOWUP (SC-INDX).
001378
001379     IF AT-FORM-TYPE = '1'
001380        MOVE 'INIT'              TO SC-FORM (SC-INDX)
001381     ELSE
001382        MOVE 'PROG'              TO SC-FORM (SC-INDX).
001383
001384     IF FORM-TO-INSURED
001385        MOVE 'INSURED'           TO SC-TO (SC-INDX)
001386     ELSE
001387     IF FORM-TO-ACCOUNT
001388        MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
001389     ELSE
001390     IF FORM-TO-OTHER-1
001391        MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
001392     ELSE
001393     IF FORM-TO-OTHER-2
001394        MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
001395
001396     MOVE 'EMPLOYER'             TO SC-ARCHIVE (SC-INDX).
001397     MOVE 'E'                    TO SC-TYPE    (SC-INDX).
001398     MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)
001399     MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).
001400     MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).
001401     MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).
001402     MOVE AL-UANOF               TO SC-RECA    (SC-INDX)
001403                                    SC-STOPA   (SC-INDX).
001404
001405 6280-CHECK-PHYSICIAN-FORM.
001406
001407     IF AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES
001408        IF (AT-PHY-FORM-ANSWERED-DT = LOW-VALUES)
001409                         OR
001410           (AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND
001411            AT-PHY-FORM-ANSWERED-DT = BIN-CURRENT-DATE)
001412              NEXT SENTENCE
001413          ELSE
001414              GO TO 6299-EXIT
001415     ELSE
001416        GO TO 6299-EXIT.
001417
001418     IF SC-TYPE (SC-INDX) = 'C' OR 'E'
001419        SET SC-INDX UP BY 1
001420        ADD +1 TO PI-NEXT-TRLR-SUB.
001421
001422     IF AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES
001423        MOVE AT-PHY-FORM-ANSWERED-DT TO DC-BIN-DATE-1
001424        MOVE SPACES                  TO DC-OPTION-CODE
001425        PERFORM 9700-DATE-LINK
001426        MOVE DC-GREG-DATE-1-EDIT     TO SC-REC (SC-INDX).
001427
001428     IF AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES
001429        MOVE AT-PHY-FORM-SEND-ON-DT TO DC-BIN-DATE-1
001430        MOVE SPACES                 TO DC-OPTION-CODE
001431        PERFORM 9700-DATE-LINK
001432        MOVE DC-GREG-DATE-1-EDIT    TO SC-SENT (SC-INDX).
001433
001434     IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
001435        MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1
001436        MOVE SPACES              TO DC-OPTION-CODE
001437        PERFORM 9700-DATE-LINK
001438        MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
001439
001440     IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
001441        MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1
001442        MOVE SPACES                 TO DC-OPTION-CODE
001443        PERFORM 9700-DATE-LINK
001444        MOVE DC-GREG-DATE-1-EDIT    TO SC-FOLLOWUP (SC-INDX).
001445
001446     IF AT-FORM-TYPE = '1'
001447        MOVE 'INIT'              TO SC-FORM (SC-INDX)
001448     ELSE
001449        MOVE 'PROG'              TO SC-FORM (SC-INDX).
001450
001451     IF FORM-TO-INSURED
001452        MOVE 'INSURED'           TO SC-TO (SC-INDX)
001453     ELSE
001454     IF FORM-TO-ACCOUNT
001455        MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
001456     ELSE
001457     IF FORM-TO-OTHER-1
001458        MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
001459     ELSE
001460     IF FORM-TO-OTHER-2
001461        MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
001462
001463     MOVE ' DOCTOR '             TO SC-ARCHIVE (SC-INDX).
001464     MOVE 'P'                    TO SC-TYPE    (SC-INDX).
001465     MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)
001466     MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).
001467     MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).
001468     MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).
001469     MOVE AL-UANOF               TO SC-RECA    (SC-INDX)
001470                                    SC-STOPA   (SC-INDX).
001471
001472 6299-EXIT.
001473      EXIT.
001474
001475     EJECT
001476 7000-BUILD-SCREEN.
001477
001478     IF PI-CLAIM-NO = SPACES
001479        MOVE ER-0319             TO EMI-ERROR
001480        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001481        GO TO 8100-SEND-INITIAL-MAP.
001482
001483     PERFORM 3000-BUILD-KEYS.
001484
001485     MOVE PI-PROCESSOR-ID        TO UBYI.
001486
001487     
      * EXEC CICS HANDLE CONDITION
001488*         NOTOPEN(8870-CLAIM-NOT-OPEN)
001489*         NOTFND (8860-CLAIM-NOT-FOUND)
001490*    END-EXEC.
      *    MOVE '"$JI                  ! & #00004298' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303034323938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001491
001492     
      * EXEC CICS READ
001493*         DATASET(ELMSTR-FILE-ID)
001494*         RIDFLD (PI-CLAM-KEY)
001495*         SET    (ADDRESS OF CLAIM-MASTER)
001496*    END-EXEC.
      *    MOVE '&"S        E          (   #00004303' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303034333033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001497
001498     EVALUATE TRUE
001499     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
001500        MOVE PI-LIFE-OVERRIDE-L6 TO TYPEI
001501
001502     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
001503        MOVE PI-AH-OVERRIDE-L6   TO TYPEI
001504
001505     WHEN CL-CLAIM-TYPE = 'I'
001506        MOVE ' IU   '            TO TYPEI
001507
001508     WHEN CL-CLAIM-TYPE = 'G'
001509        MOVE ' GAP  '            TO TYPEI
001510
001511     WHEN CL-CLAIM-TYPE = 'F'
001512        MOVE ' FAM  '            TO TYPEI
001513
001514     when cl-claim-type = 'B'
001515        move ' BRV  '            TO TYPEI
001516     when cl-claim-type = 'H'
001517        move ' HOSP '            TO TYPEI
001518
001519     WHEN CL-CLAIM-TYPE = 'O'
001520        MOVE ' OTH  '            TO TYPEI
001521
001522     END-EVALUATE
001523
001524     MOVE CL-INSURED-LAST-NAME   TO LASTNMEI.
001525     MOVE CL-FILE-LOCATION       TO FILEI.
001526     MOVE PI-ACTV-PARTIAL        TO PI-ACTV-SAVE.
001527
001528     IF CL-1ST-TRL-AVAIL
001529        GO TO 7100-NOT-FOUND.
001530
001531 7010-START-BROWSE.
001532
001533     
      * EXEC CICS HANDLE CONDITION
001534*         NOTOPEN(8820-ACTV-NOT-OPEN)
001535*         NOTFND (7100-NOT-FOUND)
001536*         ENDFILE(7100-NOT-FOUND)
001537*    END-EXEC.
      *    MOVE '"$JI''                 ! '' #00004344' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2720233030303034333434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001538
001539     
      * EXEC CICS STARTBR
001540*         DATASET(ELTRLR-FILE-ID)
001541*         RIDFLD (PI-ACTV-KEY)
001542*         GTEQ
001543*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004350' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303034333530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001544
001545     SET SC-INDX TO 1.
001546
001547 7020-READ-NEXT.
001548
001549     
      * EXEC CICS READNEXT
001550*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
001551*         RIDFLD   (PI-ACTV-KEY)
001552*         DATASET  (ELTRLR-FILE-ID)
001553*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004360' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303034333630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001554
001555     IF PI-ACTV-PARTIAL NOT = PI-ACTV-SAVE
001556        GO TO 7100-NOT-FOUND.
001557
001558     IF AT-TRAILER-TYPE = '4' OR  'A'
001559        NEXT SENTENCE
001560     ELSE
001561        GO TO 7020-READ-NEXT.
001562
001563     IF CORRESPONDENCE-TR
001564        IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND
001565           AT-LETTER-ANSWERED-DT NOT = BIN-CURRENT-DATE
001566             GO TO 7020-READ-NEXT.
001567
001568     IF CORRESPONDENCE-TR AND
001569        AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND
001570        AT-RESEND-LETTER-FORM NOT = LOW-VALUES
001571             GO TO 7020-READ-NEXT
001572     END-IF.
001573
001574     IF CORRESPONDENCE-TR AND
001575        AT-STOP-LETTER-DT NOT = LOW-VALUES AND
001576        AT-STOP-LETTER-DT NOT = SPACES AND
001577        AT-STOP-LETTER-DT NOT = BIN-CURRENT-DATE
001578*        AT-STOP-LETTER-DT = AT-LETTER-SENT-DT
001579             GO TO 7020-READ-NEXT
001580     END-IF.
001581
001582     IF NOT FORM-CONTROL-TR
001583        GO TO 7030-CONTINUE.
001584
001585     IF ((AT-FORM-ANSWERED-DT NOT = LOW-VALUES AND
001586          AT-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR
001587          AT-FORM-SEND-ON-DT = LOW-VALUES)
001588       AND
001589        ((AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND
001590          AT-EMP-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR
001591          AT-EMP-FORM-SEND-ON-DT = LOW-VALUES)
001592       AND
001593        ((AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND
001594          AT-PHY-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR
001595          AT-PHY-FORM-SEND-ON-DT = LOW-VALUES)
001596             GO TO 7020-READ-NEXT.
001597
001598     IF (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES) AND
001599        (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES)
001600        IF SC-INDX > 2
001601           MOVE PI-ACTV-SAVE-SEQ TO PI-ACTV-SEQ
001602           GO TO 7040-END-BUILD-SCREEN.
001603
001604     IF (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES) OR
001605        (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES)
001606        IF SC-INDX > 3
001607           MOVE PI-ACTV-SAVE-SEQ TO PI-ACTV-SEQ
001608           GO TO 7040-END-BUILD-SCREEN.
001609
001610 7030-CONTINUE.
001611
001612     PERFORM 6200-FORMAT-DATA THRU 6299-EXIT.
001613
001614     MOVE PI-ACTV-SEQ             TO PI-ACTV-SAVE-SEQ.
001615
001616     MOVE AT-SEQUENCE-NO          TO PI-TRLRS (PI-NEXT-TRLR-SUB).
001617
001618     ADD 1   TO PI-NEXT-TRLR-SUB.
001619     SET SC-INDX UP BY 1.
001620
001621     IF SC-INDX NOT = 5
001622        GO TO 7020-READ-NEXT.
001623
001624 7040-END-BUILD-SCREEN.
001625
001626     IF SC-INDX = +2
001627        ADD +3 TO PI-NEXT-TRLR-SUB
001628     ELSE
001629     IF SC-INDX = +3
001630        ADD +2 TO PI-NEXT-TRLR-SUB
001631     ELSE
001632     IF SC-INDX = +4
001633        ADD +1 TO PI-NEXT-TRLR-SUB.
001634
001635     MOVE -1                     TO SC-RECL (1).
001636     GO TO 8100-SEND-INITIAL-MAP.
001637
001638 7100-NOT-FOUND.
001639     IF PI-NEXT-TRLR-SUB = 1
001640        MOVE ER-0320             TO EMI-ERROR
001641        MOVE -1                  TO URECDTEL
001642        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001643        GO TO 8100-SEND-INITIAL-MAP.
001644
001645     MOVE -1                     TO SC-RECL (1).
001646
001647*    IF ANY LINES WERE PRINTED THEN
001648*    SET THE SUB TO ALWAYS BE 1 GREATER THAN A MULTIPLE OF 4.
001649
001650     SET INDX-WORK TO SC-INDX.
001651
001652     IF SC-INDX NOT = 1
001653        COMPUTE PI-NEXT-TRLR-SUB = (4 - INDX-WORK) + 1 +
001654                                    PI-NEXT-TRLR-SUB.
001655
001656     IF EIBAID NOT = DFHPF1
001657        GO TO 8100-SEND-INITIAL-MAP.
001658
001659     MOVE ER-0066                TO EMI-ERROR.
001660     MOVE -1                     TO SC-RECL (1).
001661
001662     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001663
001664     GO TO 8100-SEND-INITIAL-MAP.
001665
001666     EJECT
001667 8100-SEND-INITIAL-MAP.
001668     MOVE SAVE-DATE    TO RUNDTEO.
001669     MOVE EIBTIME      TO TIME-IN.
001670     MOVE TIME-OUT     TO RUNTIMEO.
001671
001672     IF NOT EMI-NO-ERRORS
001673         SET EMI-INDX TO 1
001674         MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSG1O.
001675
001676     IF PI-COMPANY-ID NOT = 'DMD'
001677         MOVE SPACES             TO PF4KEYO.
001678
001679     IF PI-RETURN-TO-PROGRAM = 'EL150'
001680         MOVE SPACES             TO PF5KEYO.
001681
001682     
      * EXEC CICS SEND
001683*        MAP   (MAP-NAME)
001684*        MAPSET(MAPSET-NAME)
001685*        FROM  (EL162AO)
001686*        ERASE
001687*        CURSOR
001688*    END-EXEC.
           MOVE LENGTH OF
            EL162AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004493' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303034343933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL162AO, 
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
           
001689
001690     GO TO 9100-RETURN-TRAN.
001691
001692 8200-SEND-DATAONLY.
001693     MOVE SAVE-DATE            TO RUNDTEO.
001694     MOVE EIBTIME              TO TIME-IN.
001695     MOVE TIME-OUT             TO RUNTIMEO.
001696     MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
001697
001698     IF PI-COMPANY-ID NOT = 'DMD'
001699         MOVE SPACES             TO PF4KEYO.
001700
001701     IF PI-RETURN-TO-PROGRAM = 'EL150'
001702         MOVE SPACES             TO PF5KEYO.
001703
001704     
      * EXEC CICS SEND
001705*        MAP   (MAP-NAME)
001706*        MAPSET(MAPSET-NAME)
001707*        FROM  (EL162AO)
001708*        DATAONLY
001709*        CURSOR
001710*    END-EXEC.
           MOVE LENGTH OF
            EL162AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004515' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303034353135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL162AO, 
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
           
001711
001712     GO TO 9100-RETURN-TRAN.
001713
001714 8300-SEND-TEXT.
001715     
      * EXEC CICS SEND TEXT
001716*        FROM  (LOGOFF-TEXT)
001717*        LENGTH(LOGOFF-LENGTH)
001718*        ERASE
001719*        FREEKB
001720*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004526' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303034353236' TO DFHEIV0
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
           
001721
001722     
      * EXEC CICS RETURN
001723*    END-EXEC.
      *    MOVE '.(                    ''   #00004533' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034353333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001724
001725     EJECT
001726 8600-DEEDIT.
001727     
      * EXEC CICS BIF DEEDIT
001728*        FIELD (DEEDIT-FIELD)
001729*        LENGTH(15)
001730*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004538' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034353338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001731
001732 8800-UNAUTHORIZED-ACCESS.
001733     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
001734     GO TO 8300-SEND-TEXT.
001735
001736 8810-PF23.
001737     MOVE EIBAID                   TO PI-ENTRY-CD-1.
001738     MOVE XCTL-005 TO PGM-NAME.
001739     GO TO 9300-XCTL.
001740
001741 8820-ACTV-NOT-OPEN.
001742     MOVE ER-0172                TO EMI-ERROR.
001743     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001744     GO TO 8200-SEND-DATAONLY.
001745
001746 8860-CLAIM-NOT-FOUND.
001747     MOVE ER-0319                TO EMI-ERROR.
001748     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001749     GO TO 8100-SEND-INITIAL-MAP.
001750
001751 8870-CLAIM-NOT-OPEN.
001752     MOVE ER-0042                TO EMI-ERROR.
001753     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001754     GO TO 8200-SEND-DATAONLY.
001755
001756 8880-ACCT-NOT-OPEN.
001757     MOVE ER-0168                TO EMI-ERROR
001758     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001759     GO TO 8200-SEND-DATAONLY.
001760
001761 9100-RETURN-TRAN.
001762     MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
001763     MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.
001764     
      * EXEC CICS RETURN
001765*        TRANSID(TRANS-ID)
001766*        COMMAREA(PROGRAM-INTERFACE-BLOCK)
001767*        LENGTH(PI-COMM-LENGTH)
001768*    END-EXEC.
      *    MOVE '.(CT                  ''   #00004575' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034353735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001769
001770 9200-RETURN-MAIN-MENU.
001771     MOVE XCTL-126 TO PGM-NAME.
001772     GO TO 9300-XCTL.
001773
001774 9300-XCTL.
001775     
      * EXEC CICS XCTL
001776*        PROGRAM(PGM-NAME)
001777*        COMMAREA(PROGRAM-INTERFACE-BLOCK)
001778*        LENGTH(PI-COMM-LENGTH)
001779*    END-EXEC.
      *    MOVE '.$C                   %   #00004586' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034353836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001780
001781 9400-CLEAR.
001782     MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
001783     GO TO 9300-XCTL.
001784
001785 9500-PF12.
001786     MOVE XCTL-010 TO PGM-NAME.
001787     GO TO 9300-XCTL.
001788
001789 9600-PGMID-ERROR.
001790     
      * EXEC CICS HANDLE CONDITION
001791*        PGMIDERR(8300-SEND-TEXT)
001792*    END-EXEC.
      *    MOVE '"$L                   ! ( #00004601' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303034363031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001793
001794     MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
001795     MOVE ' '          TO PI-ENTRY-CD-1.
001796     MOVE XCTL-005     TO PGM-NAME.
001797     MOVE PGM-NAME     TO LOGOFF-PGM.
001798     MOVE PGMIDERR-MSG TO LOGOFF-FILL.
001799     GO TO 9300-XCTL.
001800
001801 9700-DATE-LINK.
001802     MOVE LINK-ELDATCV           TO PGM-NAME.
001803
001804     
      * EXEC CICS LINK
001805*         PROGRAM (PGM-NAME)
001806*         COMMAREA(DATE-CONVERSION-DATA)
001807*         LENGTH  (DC-COMM-LENGTH)
001808*    END-EXEC.
      *    MOVE '."C                   (   #00004615' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034363135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001809
001810     EJECT
001811 9900-ERROR-FORMAT.
001812     IF NOT EMI-ERRORS-COMPLETE
001813         MOVE LINK-001 TO PGM-NAME
001814         
      * EXEC CICS LINK
001815*            PROGRAM (PGM-NAME)
001816*            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
001817*            LENGTH  (EMI-COMM-LENGTH)
001818*        END-EXEC.
      *    MOVE '."C                   (   #00004625' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034363235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001819
001820 9900-EXIT.
001821     EXIT.
001822
001823 9990-ABEND.
001824     MOVE LINK-004 TO PGM-NAME.
001825     MOVE DFHEIBLK TO EMI-LINE1.
001826
001827     
      * EXEC CICS LINK
001828*        PROGRAM (PGM-NAME)
001829*        COMMAREA(EMI-LINE1)
001830*        LENGTH  (72)
001831*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004638' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034363338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001832
001833     GO TO 8200-SEND-DATAONLY.
001834
001835 9995-SECURITY-VIOLATION.
001836*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00004665' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034363635' TO DFHEIV0
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
001837

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL162' TO DFHEIV1
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
               GO TO 4199-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4290-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8880-ACCT-NOT-OPEN,
                     4250-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8870-CLAIM-NOT-OPEN,
                     8860-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8820-ACTV-NOT-OPEN,
                     7100-NOT-FOUND,
                     7100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL162' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
