      *((program: EL143.cl2))
000001 ID DIVISION.
000002
000003 PROGRAM-ID. EL143.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 02/13/96 09:51:40.
000007*                            VMOD=2.018.
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
000020*                                                                *
000021*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000022*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000023*            *                                                   *
000024*            *****************************************************
000025
000026*REMARKS.    TRANSACTION - EX30 - PAYMENT APPROVAL.
000027******************************************************************
000028*                   C H A N G E   L O G
000029*
000030* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000031*-----------------------------------------------------------------
000032*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000033* EFFECTIVE    NUMBER
000034*-----------------------------------------------------------------
000035* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000036* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
000037* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000038* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000039* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000040* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000041* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000042* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000043* 080322  CR2021100800003  TANA  Add B and H claim types
000044******************************************************************
000045
000046     EJECT
000047 ENVIRONMENT DIVISION.
000048 DATA DIVISION.
000049
000050 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000051 77  FILLER  PIC X(32)  VALUE '********************************'.
000052 77  FILLER  PIC X(32)  VALUE '*    EL143  WORKING STORAGE    *'.
000053 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.
000054 77  S1                          PIC S999 COMP-3 VALUE +0.
000055 77  S2                          PIC S999 COMP-3 VALUE +0.
000056 77  ws-max-bens                 pic s999 comp-3 value +0.
000057 77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
000058 77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
000059 77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
000060 77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
000061 77  ws-pd-bens                  pic s9(5) comp-3 value +0.
000062 77  ws-at-amount-paid           pic s9(7)v99 comp-4 value +0.
000063 77  ws-at-days-in-period        pic s9(5) comp-3 value +0.
000064 77  ws-at-payment-type          pic x          value ' '.
000065 77  ws-cm-ah-orig-term          pic s999 comp-3 value +0.
000066 77  ws-cm-ah-benefit-amt        pic s9(7)v99 comp-3 value +0.
000067
000068
000069     EJECT
000070 01  WS-DATE-AREA.
000071     05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000072     05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000073
000074 01  WS-QID.
000075     05  WS-QID-TERM                 PIC X(4)    VALUE SPACES.
000076     05  FILLER                      PIC X(4)    VALUE '143A'.
000077
000078 01  STANDARD-AREAS.
000079     12  WS-RESPONSE         PIC S9(8)   COMP.
000080         88  RESP-NORMAL              VALUE +00.
000081         88  RESP-ERROR               VALUE +01.
000082         88  RESP-NOTFND              VALUE +13.
000083         88  RESP-DUPREC              VALUE +14.
000084         88  RESP-ENDFILE             VALUE +20.
000085     12  WS-BROWSE-SW                PIC X       VALUE SPACES.
000086     12  MAP-NAME                    PIC X(8)    VALUE 'EL143A  '.
000087     12  MAPSET-NAME                 PIC X(8)    VALUE 'EL143S  '.
000088     12  SCREEN-NUMBER               PIC X(4)    VALUE '143A'.
000089     12  TRANS-ID                    PIC X(4)    VALUE 'EX30'.
000090     12  THIS-PGM                    PIC X(8)    VALUE 'EL143'.
000091     12  PGM-NAME                    PIC X(8).
000092     12  TIME-IN                     PIC S9(7).
000093     12  TIME-OUT-R  REDEFINES TIME-IN.
000094         16  FILLER                  PIC X.
000095         16  TIME-OUT                PIC 99V99.
000096         16  FILLER                  PIC XX.
000097     12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
000098     12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
000099     12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
000100     12  XCTL-150                    PIC X(8)    VALUE 'EL150'.
000101     12  LINK-001                    PIC X(8)    VALUE 'EL001'.
000102     12  LINK-004                    PIC X(8)    VALUE 'EL004'.
000103     12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
000104     12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
000105     12  ELACTQ-FILE-ID              PIC X(8)    VALUE 'ELACTQ'.
000106     12  ELMSTR-FILE-ID              PIC X(8)    VALUE 'ELMSTR'.
000107     12  ELTRLR-FILE-ID              PIC X(8)    VALUE 'ELTRLR'.
000108     12  ELCERT-FILE-ID              PIC X(8)    VALUE 'ELCERT'.
000109     12  EMPLCY-FILE-ID              PIC X(8)    VALUE 'MPPLCY'.
000110
000111     12  DEEDIT-FIELD                PIC X(15).
000112     12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
000113     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
000114
000115     12  RETURN-FROM                 PIC X(8).
000116
000117     12  WS-LF-COVERAGE-TYPE         PIC X(01)   VALUE SPACE.
000118     12  WS-BEN-SEARCH-SW            PIC X(01)   VALUE 'N'.
000119         88  BENEFIT-FOUND                       VALUE 'Y'.
000120         88  NO-BENEFIT-FOUND                    VALUE 'N'.
000121     12  WS-ACCESS.
000122         16  FILLER                  PIC X(02)   VALUE SPACES.
000123         16  WS-BEN-CD               PIC X(02)   VALUE SPACES.
000124     12  SUB                         PIC 9(01)   VALUE ZEROS.
000125     12  SUB-1                       PIC S9(04)  VALUE +0  COMP.
000126
000127     12  WS-CV-PMT-CODE              PIC X(01)   VALUE ' '.
000128     12  WS-BLANK                    PIC X       VALUE ' '.
000129
000130     EJECT
000131 01   ERROR-MESSAGES.
000132     12  ER-0000                     PIC  X(4)   VALUE '0000'.
000133     12  ER-0004                     PIC  X(4)   VALUE '0004'.
000134     12  ER-0005                     PIC  X(4)   VALUE '0005'.
000135     12  ER-0023                     PIC  X(4)   VALUE '0023'.
000136     12  ER-0029                     PIC  X(4)   VALUE '0029'.
000137     12  ER-0050                     PIC  X(4)   VALUE '0050'.
000138     12  ER-0068                     PIC  X(4)   VALUE '0068'.
000139     12  ER-0070                     PIC  X(4)   VALUE '0070'.
000140     12  ER-0138                     PIC  X(4)   VALUE '0138'.
000141     12  ER-0142                     PIC  X(4)   VALUE '0142'.
000142     12  ER-0282                     PIC  X(4)   VALUE '0282'.
000143     12  ER-0303                     PIC  X(4)   VALUE '0303'.
000144     12  ER-0627                     PIC  X(4)   VALUE '0627'.
000145     12  ER-0628                     PIC  X(4)   VALUE '0628'.
000146     12  ER-0629                     PIC  X(4)   VALUE '0629'.
000147     12  ER-2237                     PIC  X(4)   VALUE '2237'.
000148     12  ER-2238                     PIC  X(4)   VALUE '2238'.
000149     12  ER-2779                     PIC  X(4)   VALUE '2779'.
000150     12  ER-3342                     PIC  X(4)   VALUE '3342'.
000151     12  ER-3343                     PIC  X(4)   VALUE '3343'.
000152     12  ER-7008                     PIC  X(4)   VALUE '7008'.
000153     12  ER-9211                     PIC  X(4)   VALUE '9211'.
000154     12  ER-9819                     PIC  X(4)   VALUE '9819'.
000155
000156     EJECT
000157 01  MISC.
000158     12  WS-HOLD-KEY.
000159         16  WS-HOLD-COMPANY-CD     PIC X.
000160         16  WS-HOLD-CARR           PIC X.
000161         16  WS-HOLD-CLAIM          PIC X(7).
000162         16  WS-HOLD-CERT.
000163             20  WS-HOLD-CERT-PRIME PIC X(10).
000164             20  WS-HOLD-CERT-SFX   PIC X.
000165     12  WS-PAY-TYPE                PIC X.
000166     12  WS-APPROVAL-LEVEL          PIC X.
000167     12  WS-PAYMENT-APPROVAL-SW     PIC X.
000168         88 WS-GRADUATED-APPROVAL          VALUE 'G'.
000169     12  SC-ITEM                    PIC S9(4)    COMP   VALUE +1.
000170     12  WS-AMOUNT-PAID             PIC S9(7)V99 COMP-3 VALUE +0.
000171
000172 01  CSO-WORK-FIELDS.
000173     05  ERROR-ON-OUTPUT-SW             PIC X        VALUE 'N'.
000174       88  ERROR-ON-OUTPUT                           VALUE 'Y'.
000175
000176 01  ACCESS-KEYS.
000177     12  WS-HOLD-ELTRLR-KEY             PIC X(20).
000178     12  ELCNTL-KEY.
000179         16  ELCNTL-COMPANY-ID          PIC  X(3).
000180         16  ELCNTL-REC-TYPE            PIC  X.
000181         16  ELCNTL-ACCESS              PIC  X(4).
000182         16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.
000183     12  ELMSTR-KEY.
000184         16  ELMSTR-COMPANY-CD          PIC X.
000185         16  ELMSTR-CARRIER             PIC X.
000186         16  ELMSTR-CLAIM-NO            PIC X(7).
000187         16  ELMSTR-CERT-NO.
000188             20  ELMSTR-CERT-PRIME      PIC X(10).
000189             20  ELMSTR-CERT-SFX        PIC X.
000190     12  ELTRLR-KEY.
000191         16  ELTRLR-COMPANY-CD          PIC X.
000192         16  ELTRLR-CARRIER             PIC X.
000193         16  ELTRLR-CLAIM-NO            PIC X(7).
000194         16  ELTRLR-CERT-NO.
000195             20  ELTRLR-CERT-PRIME      PIC X(10).
000196             20  ELTRLR-CERT-SFX        PIC X.
000197         16  ELTRLR-SEQ-NO              PIC S9(4)   COMP.
000198     12  ELACTQ-KEY.
000199         16  ELACTQ-COMPANY-CD          PIC X.
000200         16  ELACTQ-CARRIER             PIC X.
000201         16  ELACTQ-CLAIM-NO            PIC X(7).
000202         16  ELACTQ-CERT-NO.
000203             20  ELACTQ-CERT-PRIME      PIC X(10).
000204             20  ELACTQ-CERT-SFX        PIC X.
000205     12  ELCERT-KEY.
000206         16  ELCERT-COMPANY-CD          PIC X.
000207         16  ELCERT-CARRIER             PIC X.
000208         16  ELCERT-GROUP               PIC X(6).
000209         16  ELCERT-STATE               PIC X(2).
000210         16  ELCERT-ACCOUNT             PIC X(10).
000211         16  ELCERT-EFF-DATE            PIC X(2).
000212         16  ELCERT-CERT-NO             PIC X(11).
000213     12  EMPLCY-KEY.
000214         16  EMPLCY-COMPANY-CD          PIC X(01).
000215         16  EMPLCY-CARRIER             PIC X(01).
000216         16  EMPLCY-GROUPING            PIC X(06).
000217         16  EMPLCY-STATE               PIC X(02).
000218         16  EMPLCY-PRODUCER            PIC X(10).
000219         16  EMPLCY-EFF-DATE            PIC X(02).
000220         16  EMPLCY-REFERENCE-NO        PIC X(20).
000221
000222 01  ELCRTT-KEY.
000223     05  CTRLR-COMP-CD       PIC X.
000224     05  CTRLR-CARRIER       PIC X.
000225     05  CTRLR-GROUPING      PIC X(6).
000226     05  CTRLR-STATE         PIC X(2).
000227     05  CTRLR-ACCOUNT       PIC X(10).
000228     05  CTRLR-EFF-DT        PIC XX.
000229     05  CTRLR-CERT-NO       PIC X(11).
000230     05  CTRLR-REC-TYPE      PIC X.
000231
000232     EJECT
000233*    COPY ELCSCTM.
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
000234     EJECT
000235*    COPY ELCSCRTY.
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
000236     EJECT
000237*    COPY ELCDATE.
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
000238     EJECT
000239*    COPY ELCLOGOF.
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
000240     EJECT
000241*    COPY ELCATTR.
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
000242     EJECT
000243*    COPY ELCEMIB.
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
000244     EJECT
000245*    COPY MPCPOLUP.
      *>>((file: MPCPOLUP))
000001******************************************************************
000002*                                                                *
000003*                           MPCPOLUP                             *
000004*                            VMOD=1.031                          *
000005*                                                                *
000006*   THIS COPY BOOK IS USED TO PASS DATA TO THE POLICY UPDATE     *
000007*   PROGRAM (EMPLCY).  WHEN USING COBOL-2 IT IS NECESSARY TO     *
000008*   INITIALIZE THE ENTIRE COPYBOOK TO LOW-VALUES, EXCEPT FOR THE *
000009*   LENGTH FIELD.  BY MOVING LOW-VALUES INTO THE GROUP FIELD     *
000010*   WS-POLICY-UPDATE-WORKING-GRPS, THIS REQUIREMENT CAN BE       *
000011*   SATISFIED.                                                   *
000012*   THE ACTUAL DATA BEING USED TO UPDATE THE POLICY AREA IS      *
000013*   FOUND IN THE WS-MPPLCY-AREA.  DATA SHOULD ONLY MOVED INTO    *
000014*   THESE FIELDS WHEN THAT DATA DIFFERS FROM THE VALUES FOUND ON *
000015*   THE POLICY RECORD, OTHERWISE THEY SHOULD CONTAIN LOW-VALUES. *
000016*   EACH UPDATED FIELD WILL CREATE A POLICY HISTORY RECORD       *
000017*   CONTAINING THE BEFORE CHANGE IMAGE.                          *
000018*                                                                *
000019*   THE CALLING PROGRAM MUST ALSO PROVIDE THE FOLLOWING DATA:    *
000020*      WS-LAST-CHANGE-PROCESSOR                                  *
000021*      WS-CONTROL-PRIMARY                                        *
000022*                                                                *
000023*   PROGRAMS THAT ARE DOING OTHER UPDATE I/O FUNCTIONS WILL USE  *
000024*   'WS-EMPLCY-FUNCTION'. (ALL BATCH PGM'S MUST USE THIS).       *
000025*   THE PROGRAMS THAT USE THE FUNCTION FIELD CAN USE ONE OF      *
000026*   THE FOLLOWING CODES:                                         *
000027*                                                                *
000028*      OO - OPEN FILES (I-O MODE)                                *
000029*      OI - OPEN FILES (INPUT MODE)                              *
000030*      CC - CLOSE FILES                                          *
000031*      SS - START                                                *
000032*      RN - READ NEXT                                            *
000033*      RW - REWRITE                                              *
000034*      WW - WRITE                                                *
000035*      DD - DELETE                                               *
000036*      AC - AGENT COMMISSION ACCOUNTING                          *
000037*      ED - EFFECTIVE DATE CHANGE                                *
000038*      KC - KEY CHANGE                                           *
000039*      NC - NAME CHANGE                                          *
000040*                                                                *
000041*   IF THE FUNCTION FIELD WAS USED, CHECK                        *
000042*   'WS-EMPLCY-RETURN-CODE' FOR LOW VALUES TO ASSURE A           *
000043*   SUCCESSFUL RETURN.                                           *
000044*                                                                *
000045*   **** NOTE ****
000046*   MAKE SURE PROGRAMS CLEAR THE 'WS-MPPLCY-AREA' IF YOUR        *
000047*   PROGRAM CALLS EMPLCY TWICE, BY LEAVING DATA IN THE 'WS'      *
000048*   FIELDS EMPLCY WILL UPDATE AND CREATE HISTORY TWICE FOR       *
000049*   THE LEFT OVER FIELDS.  WHEN CHANGING FUNCTIONS, ETC., MAKE   *
000050*   SURE TO CLEAR OR CHANGE THE REQUIRED CONTROL FIELDS.         *
000051*                                                                *
000052******************************************************************
000053
000054******************************************************************
000055*                    PRIMARY CONTROL                             *
000056******************************************************************
000057 01  WS-POLICY-MASTER-UPDATE-AREA.
000058******************************************************************
000059
000060  12 WS-PM-COMM-LNGTH            PIC S9(4) COMP VALUE +1500.
000061  12 WS-POLICY-UPDATE-WORKING-GRPS.
000062  16 WS-MONTH-END-DT             PIC XX.
000063
000064  16 WS-EMPLCY-FUNCTION          PIC XX.
000065     88  WS-OPEN                   VALUE 'OO'.
000066     88  WS-OPEN-INPUT             VALUE 'OI'.
000067     88  WS-CLOSE                  VALUE 'CC'.
000068     88  WS-START                  VALUE 'SS'.
000069     88  WS-READ                   VALUE 'RR'.
000070     88  WS-READNEXT               VALUE 'RN'.
000071     88  WS-REWRITE                VALUE 'RW' SPACES LOW-VALUES.
000072     88  WS-WRITE                  VALUE 'WW'.
000073     88  WS-DELETE                 VALUE 'DD'.
000074     88  WS-AGENT-COMM-ACTG        VALUE 'AG'.
000075     88  WS-EFFDT-CHG              VALUE 'ED'.
000076     88  WS-KEY-CHG                VALUE 'KC'.
000077     88  WS-NAME-CHANGE            VALUE 'NC'.
000078
000079  16 WS-CALCULATION-AREA.
000080     20  WS-C-INS-MONTH-PREMIUM  PIC S9(5)V999999  COMP-3.
000081
000082  16 WS-ACCOUNTING-ACTIVITY      PIC X(3).
000083     88  PREMIUM-ACCOUNTING
000084                           VALUES ARE '101' '102' '103' '104'
000085                                      '105' '106' '107' '108'
000086                                      '201' '202' '203' '204'
000087                                      '205' '206' '207' '208'
000088                                      '301' '302' '303'
000089                                      '401' '402' '403' '404'
000090                                      '601' '602' '603'.
000091     88  COMMISSION-ACCOUNTING
000092                                  VALUES ARE '501' '502' '503'.
000093* CASH WITH APP (CWA) ACTIVITY
000094  16 WS-ACTG-CWA-ACTIVITY        PIC X(3).
000095     88  CREATE-PM-WITH-CWA                        VALUE '101'.
000096     88  DELETE-PM-WITH-CWA                        VALUE '102'.
000097     88  CHANGE-PM-CWA                             VALUE '103'.
000098     88  ISSUE-PM-WITH-CWA                         VALUE '104'.
000099     88  CANCEL-PM-WITH-CWA                        VALUE '105'.
000100     88  REINSTATE-PM-WITH-CWA                     VALUE '106'.
000101     88  DECLINE-PM-WITH-CWA                       VALUE '107'.
000102     88  UNDECLINE-PM-WITH-CWA                     VALUE '108'.
000103     88  CWA-ACTIVITY                   VALUES ARE '101' '102'
000104                                                   '103' '104'
000105                                                   '105' '106'
000106                                                   '107' '108'.
000107* POLICY FEE ACTIVITY
000108  16 WS-ACTG-PLCY-FEE-ACTIVITY   PIC X(3).
000109     88  CREATE-PM-WITH-PLCY-FEE                   VALUE '201'.
000110     88  DELETE-PM-WITH-PLCY-FEE                   VALUE '202'.
000111     88  CHANGE-PM-PLCY-FEE                        VALUE '203'.
000112     88  ISSUE-PM-WITH-PLCY-FEE                    VALUE '204'.
000113     88  CANCEL-PM-WITH-PLCY-FEE                   VALUE '205'.
000114     88  REINSTATE-PM-WITH-PLCY-FEE                VALUE '206'.
000115     88  DECLINE-PM-WITH-PLCY-FEE                  VALUE '207'.
000116     88  UNDECLINE-PM-WITH-PLCY-FEE                VALUE '208'.
000117     88  POLICY-FEE-ACITIVY             VALUES ARE '201' '202'
000118                                                   '203' '204'
000119                                                   '205' '206'
000120                                                   '207' '208'.
000121* PAYMENT ACTIVITY
000122  16 WS-ACTG-PYMT-ACTIVITY       PIC X(3).
000123     88  APPLY-PAYMENT                             VALUE '301'.
000124     88  REVERSE-PAYMENT                           VALUE '302'.
000125     88  GROUP-PAYMENT-WITH-OVER-SHORT             VALUE '303'.
000126     88  PAYMENT-ACITIVY                VALUES ARE '301' '302'
000127                                                         '303'.
000128* REFUND ACTIVITY
000129  16 WS-ACTG-REFUND-ACTIVITY     PIC X(3).
000130     88  CREATE-REFUND                             VALUE '401'.
000131     88  DELETE-REFUND                             VALUE '402'.
000132     88  CHANGE-REFUND                             VALUE '403'.
000133     88  REINSTATE-REFUND                          VALUE '404'.
000134     88  REFUND-ACTIVITY                VALUES ARE '401' '402'
000135                                                   '403' '404'.
000136
000137* COMMISSION ACTIVITY
000138  16 WS-ACTG-COMMISSION-ACTIVITY PIC X(3).
000139     88  COMMISSION-ADVANCE                        VALUE '501'.
000140     88  COMMISSION-PAYMENT                        VALUE '502'.
000141     88  COMMISSION-REVERSAL                       VALUE '503'.
000142     88  COMMISSION-ACTIVITY            VALUES ARE '501' '502'
000143                                                         '503'.
000144* EXPENSE ACTIVITY
000145  16 WS-ACTG-EXPENSE-ACTIVITY    PIC X(3).
000146     88  CREATE-EXPENSE                            VALUE '601'.
000147     88  DELETE-EXPENSE                            VALUE '602'.
000148     88  CHANGE-EXPENSE                            VALUE '603'.
000149     88  EXPENSE-ACTIVITY               VALUES ARE '601' '602'
000150                                                         '603'.
000151
000152  16 WS-AGENT-KEY.
000153     20  WS-AGT-COMPANY-CD                 PIC X.
000154     20  WS-AGT-CARRIER                    PIC X.
000155     20  WS-AGT-GROUPING                   PIC X(6).
000156     20  WS-AGT-FINCL-RESPONSE             PIC X(10).
000157     20  WS-AGT-PROD-AGT                   PIC X(10).
000158     20  WS-AGT-TYPE                       PIC X.
000159  16  WS-COMM-ADJUSTMENT-AMT               PIC S9(7)V99 COMP-3.
000160  16  WS-1STYR-COMM-AMT                    PIC S9(7)V99 COMP-3.
000161  16  WS-RENEW-COMM-AMT                    PIC S9(7)V99 COMP-3.
000162
000163  16  WS-OLD-EFF-DT                        PIC X(2).
000164  16  WS-NEW-EFF-DT                        PIC X(2).
000165
000166  16  WS-NEW-CONTROL-PRIMARY.
000167      20  WS-NEW-COMPANY-CD                PIC X.
000168      20  WS-NEW-CARRIER                   PIC X.
000169      20  WS-NEW-GROUPING.
000170          24  WS-NEW-GROUPING-PREFIX       PIC X(3).
000171          24  WS-NEW-GROUPING-PRIME        PIC X(3).
000172      20  WS-NEW-STATE                     PIC XX.
000173      20  WS-NEW-PRODUCER.
000174          24  WS-NEW-PRODUCER-PREFIX       PIC X(4).
000175          24  WS-NEW-PRODUCER-PRIME        PIC X(6).
000176
000177  16  WS-NEW-SECURITY-ACCESS-CODE          PIC X.
000178
000179  16  WS-CALLING-PGM                       PIC X(8).
000180
000181  16  WS-EXPENSE-DT                        PIC XX.
000182  16  WS-EXPENSE-AMT                       PIC S9(5)V99   COMP-3.
000183  16  WS-OLD-EXPENSE-AMT                   PIC S9(5)V99   COMP-3.
000184
000185  16 WS-POINTER-AREA.
000186     20  WS-POINTER-SW                     PIC  X.
000187         88 WS-KEEP-POINTERS               VALUE 'Y'.
000188     20  WS-ACTY-POINTER                   PIC S9(08) COMP.
000189     20  WS-ACTG-POINTER                   PIC S9(08) COMP.
000190     20  WS-ALPH-POINTER                   PIC S9(08) COMP.
000191     20  WS-DPND-POINTER                   PIC S9(08) COMP.
000192     20  WS-MIB-POINTER                    PIC S9(08) COMP.
000193     20  WS-NOTE-POINTER                   PIC S9(08) COMP.
000194     20  WS-PLCY-POINTER                   PIC S9(08) COMP.
000195     20  WS-PHSTC-POINTER                  PIC S9(08) COMP.
000196     20  WS-PHST-POINTER                   PIC S9(08) COMP.
000197     20  WS-PREM-POINTER                   PIC S9(08) COMP.
000198     20  WS-UHST-POINTER                   PIC S9(08) COMP.
000199  16 WS-RECORDS-PROCESSED                  PIC S9(04) COMP.
000200  16 WS-USE-POLICY-LINKAGE-IND             PIC  X.
000201     88  WS-USE-POLICY-LINKAGE                 VALUE 'Y'.
000202  16 WS-COMPANY-ID                         PIC  X(03).
000203  16 WS-ACTIVE-RECORDS-IND                 PIC  X.
000204     88  WS-OTHER-ACTIVE-RCRDS-EXISTS          VALUE 'Y'.
000205  16 WS-FATAL-ERROR                        PIC  9(04).
000206  16 WS-STOP-PROCESSING-IND                PIC  X.
000207     88  WS-STOP-PROCESSING                    VALUE 'Y'.
000208  16 FILLER                                PIC X(123).
000209
000210  16 WS-MPPLCY-AREA.
000211     20  WS-EMPLCY-RETURN-CODE             PIC XX.
000212     20  WS-JOURNAL-FILE-ID                PIC S9(4) COMP.
000213
000214     20  WS-CONTROL-PRIMARY.
000215         24  WS-COMPANY-CD                 PIC X.
000216         24  WS-CARRIER                    PIC X.
000217         24  WS-GROUPING.
000218             28  WS-GROUPING-PREFIX        PIC X(3).
000219             28  WS-GROUPING-PRIME         PIC X(3).
000220         24  WS-STATE                      PIC XX.
000221         24  WS-PRODUCER.
000222             28  WS-PRODUCER-PREFIX        PIC X(4).
000223             28  WS-PRODUCER-PRIME         PIC X(6).
000224         24  WS-POLICY-EFF-DT              PIC XX.
000225         24  WS-REFERENCE-NUMBER.
000226             28  WS-REFNO-PRIME            PIC X(18).
000227             28  WS-REFNO-SFX              PIC XX.
000228
000229******************************************************************
000230*              CONTROL BY SOC. SEC. NUMBER                       *
000231******************************************************************
000232     20  WS-CONTROL-BY-SSN.
000233         24  WS-COMPANY-CD-A3              PIC X.
000234         24  WS-SOC-SEC-NO.
000235             28  WS-SSN-STATE              PIC XX.
000236             28  WS-SSN-PRODUCER           PIC X(6).
000237             28  WS-SSN-LN3.
000238                 32  WS-INSURED-INITIALS-A3.
000239                     36 WS-INSURED-INITIAL1-A3   PIC X.
000240                     36 WS-INSURED-INITIAL2-A3   PIC X.
000241                 32 WS-PART-LAST-NAME-A3         PIC X.
000242
000243******************************************************************
000244*              CONTROL BY REFERENCE NUMBER                       *
000245******************************************************************
000246     20  WS-CONTROL-BY-POLICY-NO.
000247         24  WS-COMPANY-CD-A4              PIC X.
000248         24  WS-POLICY-NO-A4.
000249             28  WS-POLICY-PRIME-A4        PIC X(18).
000250             28  WS-POLICY-SFX-A4          PIC XX.
000251
000252******************************************************************
000253*                 FILE SYNCHRONIZATION DATA                      *
000254******************************************************************
000255     20  WS-FILE-SYNCH-DATA.
000256         24  WS-LAST-CHANGE-DT             PIC XX.
000257         24  WS-LAST-CHANGE-TIME           PIC S9(7).
000258         24  WS-LAST-CHANGE-PROCESSOR      PIC X(4).
000259
000260******************************************************************
000261*                    INSUREDS PROFILE DATA                       *
000262******************************************************************
000263     20  WS-INSURED-PROFILE-DATA.
000264         24  WS-INSURED-NAME.
000265             28  WS-INSURED-LAST-NAME      PIC X(15).
000266             28  WS-INSURED-FIRST-NAME.
000267                 32  WS-INSURED-1ST-INIT   PIC X.
000268                 32  FILLER                PIC X(9).
000269             28  WS-INSURED-MIDDLE-INIT    PIC X.
000270         24  WS-INSURED-ADDRESS.
000271             28  WS-ADDRESS-LINE-1         PIC X(30).
000272             28  WS-ADDRESS-LINE-2         PIC X(30).
000273             28  WS-CITY                   PIC X(25).
000274             28  WS-RESIDENT-STATE         PIC XX.
000275             28  WS-ZIP-CD.
000276                 32  WS-ZIP-FIRST-FIVE     PIC X(5).
000277                 32  WS-ZIP-PLUS-FOUR      PIC X(4).
000278         24  WS-INSURED-PERSONAL.
000279             28  WS-INSURED-OCC-CLASS      PIC X.
000280             28  WS-INSURED-OCC-CD         PIC X(3).
000281             28  WS-INSURED-SEX            PIC X.
000282             28  WS-INSURED-BIRTH-DT       PIC XX.
000283             28  WS-INSURED-ISSUE-AGE      PIC S9(3).
000284             28  WS-INSURED-ISSUE-AGE-X REDEFINES
000285                 WS-INSURED-ISSUE-AGE      PIC  X(3).
000286             28  WS-INSURED-HEIGHT-FT      PIC S9(3).
000287             28  WS-INSURED-HEIGHT-FT-X REDEFINES
000288                 WS-INSURED-HEIGHT-FT      PIC  X(3).
000289             28  WS-INSURED-HEIGHT-IN      PIC S9(3).
000290             28  WS-INSURED-HEIGHT-IN-X REDEFINES
000291                 WS-INSURED-HEIGHT-IN      PIC  X(3).
000292             28  WS-INSURED-WEIGHT         PIC S9(3).
000293             28  WS-INSURED-WEIGHT-X    REDEFINES
000294                 WS-INSURED-WEIGHT         PIC  X(3).
000295             28  WS-INSURED-BIRTH-STATE    PIC XX.
000296             28  WS-INSURED-PHONE-NO       PIC X(13).
000297             28  WS-INSURED-RATED-AGE      PIC S9(3).
000298             28  WS-INSURED-RATED-AGE-X REDEFINES
000299                 WS-INSURED-RATED-AGE      PIC  X(3).
000300         24  WS-INS-LANGUAGE-IND           PIC X.
000301         24  WS-INSURED-TOT-BENEFIT        PIC S9(7)V99.
000302         24  WS-INSURED-AGE-IND            PIC X.
000303     20  FILLER                            PIC X(02).
000304
000305******************************************************************
000306*                JOINT INSUREDS PROFILE DATA                     *
000307******************************************************************
000308     20  WS-JOINT-PROFILE-DATA.
000309         24  WS-JOINT-NAME.
000310             28  WS-JOINT-LAST-NAME         PIC X(15).
000311             28  WS-JOINT-FIRST-NAME.
000312                 32  WS-JOINT-1ST-INIT      PIC X.
000313                 32  FILLER                 PIC X(9).
000314             28  WS-JOINT-MIDDLE-INIT       PIC X.
000315         24  WS-JOINT-SOC-SEC-NO           PIC X(11).
000316         24  WS-JOINT-PERSONAL.
000317             28  WS-JOINT-OCC-CLASS        PIC X.
000318             28  WS-JOINT-OCC-CD           PIC X(3).
000319             28  WS-JOINT-SEX              PIC X.
000320             28  WS-JOINT-BIRTH-DT         PIC XX.
000321             28  WS-JOINT-ISSUE-AGE        PIC S9(3).
000322             28  WS-JOINT-ISSUE-AGE-X   REDEFINES
000323                 WS-JOINT-ISSUE-AGE        PIC  X(3).
000324             28  WS-JOINT-HEIGHT-FT        PIC S9(3).
000325             28  WS-JOINT-HEIGHT-FT-X   REDEFINES
000326                 WS-JOINT-HEIGHT-FT        PIC  X(3).
000327             28  WS-JOINT-HEIGHT-IN        PIC S9(3).
000328             28  WS-JOINT-HEIGHT-IN-X   REDEFINES
000329                 WS-JOINT-HEIGHT-IN        PIC  X(3).
000330             28  WS-JOINT-WEIGHT           PIC S9(3).
000331             28  WS-JOINT-WEIGHT-X      REDEFINES
000332                 WS-JOINT-WEIGHT           PIC  X(3).
000333             28  WS-JOINT-BIRTH-STATE      PIC XX.
000334             28  WS-JOINT-RATED-AGE        PIC S9(3).
000335             28  WS-JOINT-RATED-AGE-X   REDEFINES
000336                 WS-JOINT-RATED-AGE        PIC  X(3).
000337         24  WS-JOINT-TOT-BENEFIT          PIC S9(7)V99.
000338         24  WS-JOINT-AGE-IND              PIC X.
000339     20  FILLER                            PIC X(03).
000340
000341******************************************************************
000342*                  INSURANCE COVERAGE DATA                       *
000343******************************************************************
000344     20  WS-INS-COVERAGE-DATA.
000345         24  WS-FREE-PERIOD                PIC S9(03) COMP-3.
000346         24  WS-LOAN-NUMBER                PIC X(20).
000347         24  WS-LOAN-TERM                  PIC S9(3).
000348         24  WS-LOAN-APR                   PIC S9V9999.
000349         24  WS-LOAN-DT                    PIC XX.
000350         24  WS-LOAN-PYMT                  PIC S9(5)V99.
000351         24  WS-LOAN-BALC                  PIC S9(7)V99.
000352         24  WS-INS-MONTH-BENEFIT          PIC S9(7)V99.
000353         24  WS-INS-BENEFIT-MONTHS         PIC S9(3).
000354         24  WS-INS-TOTAL-BENEFIT          PIC S9(7)V99.
000355         24  WS-INS-PLAN-TYPE              PIC X.
000356         24  WS-INS-PLAN-CD                PIC XX.
000357         24  WS-INS-PLAN-REVISION          PIC X(3).
000358         24  WS-INS-POLICY-FORM            PIC X(12).
000359         24  WS-INS-MSTR-POLICY            PIC X(12).
000360         24  WS-INS-MSTR-APP               PIC X(12).
000361         24  WS-INS-RATE-CD                PIC X(5).
000362         24  WS-INS-SEX-RATING             PIC X.
000363         24  WS-INS-SUBSTANDARD-PCT        PIC S9V9999.
000364         24  WS-INS-SUBSTANDARD-TYPE       PIC X.
000365         24  WS-INS-TERMINATION-DT         PIC XX.
000366         24  WS-INS-MONTH-PREMIUM          PIC S9(5)V999999.
000367         24  WS-INS-CALC-MO-PREM           PIC S9(5)V999999.
000368         24  WS-REINSURANCE-TABLE          PIC X(3).
000369         24  WS-MORTALITY-CD               PIC X(4).
000370         24  WS-INS-TYPE                   PIC X.
000371         24  WS-LOAN-OFFICER               PIC X(5).
000372         24  WS-POLICY-FEE                 PIC S9(3)V99.
000373         24  WS-DEPENDENT-COUNT            PIC S99.
000374         24  WS-CWA-AMOUNT                 PIC S9(5)V99.
000375         24  WS-LAST-AUTO-RERATE-DT        PIC XX.
000376         24  WS-PREM-FINANCED-SW           PIC X.
000377         24  WS-INS-TERM-LETTER-IND        PIC X.
000378         24  WS-INS-UNDERWRITER-MAX-BEN    PIC S9(7)V99.
000379
000380******************************************************************
000381*                    POLICY BILLING DATA                         *
000382******************************************************************
000383     20  WS-BILLING-DATA.
000384         24  WS-BILLING-MODE               PIC X(1).
000385         24  WS-BILLING-SCHEDULE           PIC X(1).
000386         24  WS-BILLING-SW                 PIC X(1).
000387         24  WS-BILLING-TYPE               PIC X(1).
000388         24  WS-PAYMENT-AMT                PIC S9(5)V99.
000389         24  WS-PAYMENT-AMT-X          REDEFINES
000390             WS-PAYMENT-AMT                PIC  X(7).
000391         24  WS-OVER-SHORT-AMT             PIC S9(5)V99.
000392         24  WS-OVER-SHORT-AMT-X       REDEFINES
000393             WS-OVER-SHORT-AMT             PIC  X(7).
000394         24  WS-LAST-BILL-DT               PIC XX.
000395         24  WS-LAST-BILL-AMT              PIC S9(5)V99.
000396         24  WS-LAST-BILL-AMT-X        REDEFINES
000397             WS-LAST-BILL-AMT              PIC  X(7).
000398         24  WS-BILL-TO-DT                 PIC XX.
000399         24  WS-LAST-PYMT-DT               PIC XX.
000400         24  WS-PAID-TO-DT                 PIC XX.
000401         24  WS-PYMT-INVOICE-NUMBER        PIC X(6).
000402         24  WS-MONTHS-PAID                PIC S9(3).
000403         24  WS-MONTHS-PAID-X          REDEFINES
000404             WS-MONTHS-PAID                PIC  X(3).
000405         24  WS-TOTAL-PREM-RECVD           PIC S9(7)V99.
000406         24  WS-TOTAL-PREM-RECVD-X     REDEFINES
000407             WS-TOTAL-PREM-RECVD           PIC  X(9).
000408         24  WS-BANK-TRANSIT-NUMBER.
000409             28  WS-FEDERAL-NUMBER         PIC X(4).
000410             28  WS-BANK-NUMBER            PIC X(4).
000411         24  WS-BANK-ACCOUNT-NUMBER        PIC X(20).
000412         24  WS-BILLING-GROUPING-CODE      PIC X(6).
000413         24  WS-CHARGE-CARD-EXP-DT         PIC X(2).
000414         24  WS-CHARGE-CARD-TYPE           PIC X(2).
000415         24  WS-BILL-INVOICE-NUMBER        PIC X(6).
000416         24  WS-BILL-DAY                   PIC S99.
000417         24  WS-RES-PREM-TAX               PIC S9(3)V999999.
000418     20  FILLER                            PIC X(06).
000419
000420******************************************************************
000421*                     CLAIM PAYMENT DATA                         *
000422******************************************************************
000423     20  WS-CLAIM-PAYMENT-DATA.
000424         24  WS-CLAIM-BENEFICIARY-NAME     PIC X(25).
000425         24  WS-CLAIM-INCURRED-DT          PIC XX.
000426         24  WS-CLAIM-PAID-TO-DT           PIC XX.
000427         24  WS-CLAIM-PAYMENT-CNT          PIC S9(3).
000428         24  WS-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99.
000429         24  WS-CLAIM-EXPENSES-ITD         PIC S9(7)V99.
000430         24  WS-CLAIM-PAYMENTS-ITD         PIC S9(7)V99.
000431         24  WS-CLAIM-ACCUMULATOR          PIC S9(7)V99.
000432         24  WS-CLAIM-INTERFACE-SW         PIC X.
000433     20  FILLER                            PIC X(10).
000434
000435******************************************************************
000436*                POLICY STATUS AND DISPOSITION                   *
000437******************************************************************
000438     20  WS-STATUS-DISPOSITION-DATA.
000439         24  WS-ISSUE-EOM-DT               PIC XX.
000440         24  WS-REPLACEMENT-SWITCH         PIC X.
000441         24  WS-APPL-SIGN-DT               PIC XX.
000442         24  WS-UNDERWRITER                PIC X(3).
000443         24  WS-ENTRY-PROCESSOR            PIC X(4).
000444         24  WS-ENTRY-STATUS               PIC X.
000445         24  WS-ENTRY-DT                   PIC XX.
000446         24  WS-ENTRY-TIME                 PIC S9(7).
000447         24  WS-EXIT-DT                    PIC XX.
000448         24  WS-CURRENT-STATUS             PIC X.
000449             88  WS-LAPSE                     VALUE '0'.
000450             88  WS-ACTIVE                    VALUE '1'.
000451             88  WS-PENDING-ISSUE             VALUE '2'.
000452             88  WS-DECLINED                  VALUE '3'.
000453             88  WS-PENDING-CANCEL            VALUE '4'.
000454             88  WS-PENDING-ISSUE-ERROR       VALUE '5'.
000455             88  WS-CLAIM-APPLIED             VALUE '6'.
000456             88  WS-CANCEL                    VALUE '7'.
000457             88  WS-PENDING-UNWTR-REVW        VALUE '8'.
000458             88  WS-PENDING-CANCEL-ERROR      VALUE '9'.
000459             88  WS-CANCEL-TRANSFER           VALUE 'C'.
000460             88  WS-CLAIM-SETTLEMENT          VALUE 'F'.
000461             88  WS-TERMINATE                 VALUE 'T'.
000462             88  WS-BILLABLE-STATUS   VALUES ARE '0' '1' '6'.
000463             88  WS-PENDING-STATUS
000464                                VALUES ARE '2' '4' '5' '8' '9'.
000465             88  WS-PENDING-ISSUE-STATUS
000466                                VALUES ARE '2' '5' '8'.
000467             88  WS-CANCEL-STATUS
000468                                VALUES ARE '4' '7' '9' 'C'.
000469         24  WS-CANCEL-CAUSE-CD            PIC X(3).
000470         24  WS-CANCEL-DT                  PIC XX.
000471         24  WS-REFUND-FIELDS.
000472             28  WS-REFUND-AMT             PIC S9(5)V99.
000473             28  WS-CALC-REFUND-AMT        PIC S9(5)V99.
000474         24  WS-DECLINE-CD                 PIC X(3).
000475         24  WS-DECLINE-DT                 PIC XX.
000476         24  WS-LAST-LAPSE-DT              PIC XX.
000477         24  WS-LAST-REINSTATE-DT          PIC XX.
000478         24  WS-SECURITY-ACCESS-CODE       PIC X.
000479         24  WS-PREV-CONTROL-PRIMARY.
000480             28  WS-PREV-COMPANY-CD             PIC X.
000481             28  WS-PREV-CARRIER                PIC X.
000482             28  WS-PREV-GROUPING.
000483                 32  WS-PREV-GROUPING-PREFIX    PIC X(3).
000484                 32  WS-PREV-GROUPING-PRIME     PIC X(3).
000485             28  WS-PREV-STATE                  PIC XX.
000486             28  WS-PREV-PRODUCER.
000487                 32  WS-PREV-PRODUCER-PREFIX    PIC X(4).
000488                 32  WS-PREV-PRODUCER-PRIME     PIC X(6).
000489             28  WS-PREV-POLICY-EFF-DT          PIC XX.
000490             28  WS-PREV-REFERENCE-NUMBER.
000491                 32  WS-PREV-REFNO-PRIME        PIC X(18).
000492                 32  WS-PREV-REFNO-SFX          PIC XX.
000493         24  WS-ACTION-DT                       PIC XX.
000494         24  WS-ACTION-CODE                     PIC X(3).
000495         24  WS-ACTION-DT-2                     PIC XX.
000496         24  WS-ACTION-CODE-2                   PIC X(3).
000497         24  WS-ACTION-DT-3                     PIC XX.
000498         24  WS-ACTION-CODE-3                   PIC X(3).
000499         24  WS-ACTION-DT-4                     PIC XX.
000500         24  WS-ACTION-CODE-4                   PIC X(3).
000501         24  WS-ACTION-DT-5                     PIC XX.
000502         24  WS-ACTION-CODE-5                   PIC X(3).
000503         24  WS-KEY-CHANGE                      PIC X.
000504         24  WS-KEY-CHANGE-DT                   PIC XX.
000505         24  WS-RTI-INDICATOR                   PIC X.
000506         24  WS-REASON-CODE                     PIC X(3).
000507         24  WS-IN-OUT-PROCESSING-IND           PIC X(1).
000508     20  FILLER                            PIC X(07).
000509
000510******************************************************************
000511*                 AGENT AND COMMISSION DATA                      *
000512******************************************************************
000513     20  WS-COMMISSION-DATA.
000514         24  WS-REMIT-TO                   PIC S9(3) COMP-3.
000515         24  WS-COMM-CHANGE-SW             PIC X.
000516         24  WS-AGENT-INFO-GRP.
000517             28  WS-AGENT-INFORMATION   OCCURS   5 TIMES
000518                                        INDEXED BY WS-COMM-NDX
000519                                                   WS-COMM-NDX2.
000520                 32  WS-AGENT-NUMBER       PIC X(10).
000521                 32  WS-AGENT-TYPE         PIC X.
000522                 32  WS-COMMISSION-BILLED-PAID
000523                                           PIC X(1).
000524                 32  WS-AGENT-COMP-1ST-YEAR
000525                                           PIC S99V999.
000526                 32  WS-COMP-1ST-YEAR-TYPE PIC X(1).
000527                 32  WS-AGENT-RENEWAL-DATA OCCURS 6 TIMES
000528                                        INDEXED BY WS-RENEW-NDX
000529                                                   WS-RENEW-NDX2.
000530                     36 WS-RENEW-MONTHS      PIC S999 COMP-3.
000531                     36 WS-RENEW-COMMISSION PIC S99V999 COMP-3.
000532                     36 WS-RENEW-TYPE        PIC X(1).
000533                 32  WS-COMP-RECALC-FLAG   PIC X(1).
000534
000535******************************************************************
000536*               ADDITIONAL CLAIM DATA                            *
000537******************************************************************
000538     20  WS-ADDL-CLAIM-DATA.
000539         24  WS-CLAIM-ATTACH-CNT           PIC S9(3).
000540         24  WS-CLAIM-LIFE-ITD             PIC S9(7)V99.
000541         24  WS-CLAIM-AH-ITD               PIC S9(7)V99.
000542         24  WS-CLAIM-RIDER-ITD            PIC S9(7)V99.
000543
000544******************************************************************
000545*                 FILLER                                         *
000546******************************************************************
000547     20  FILLER                            PIC X(40).
000548
000549******************************************************************
000550*                 FILLER                                         *
000551******************************************************************
000552  16 WS-POLICY-MASTER-RECORD
000553       REDEFINES WS-MPPLCY-AREA            PIC X(1200).
000554
      *<<((file: MPCPOLUP))
000246     EJECT
000247*    COPY ELCINTF.
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
000248     12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
000249         16  PI-LAST-ELACTQ-KEY      PIC X(20).
000250         16  PI-LAST-TRLR-SEQ-NO     PIC S9(4)   COMP.
000251         16  PI-ELTRLR-UPDATE-BY     PIC X(4).
000252         16  PI-ELTRLR-UPDATE-HHMMSS PIC S9(6)   COMP-3.
000253         16  PI-UNAPPROVED-COUNT     PIC S9      COMP-3.
000254         16  PI-DIAGNOSIS            PIC X(25).
000255         16  PI-FIRST-TIME-SW        PIC X(01).
000256         16  PI-LAST-ELTRLR-KEY      PIC X(22).
000257         16  PI-PAY-TYPE             PIC X(01).
000258         16  FILLER                  PIC X(560).
000259
000260     EJECT
000261*    COPY ELCAID.
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
000262 01  FILLER    REDEFINES DFHAID.
000263     12  FILLER              PIC X(8).
000264     12  PF-VALUES           PIC X       OCCURS 24 TIMES.
000265
000266     EJECT
000267*    COPY EL143S.
      *>>((file: EL143S))
000001 01  EL143AI.
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
000022     05  CARRDL PIC S9(0004) COMP.
000023     05  CARRDF PIC  X(0001).
000024     05  FILLER REDEFINES CARRDF.
000025         10  CARRDA PIC  X(0001).
000026     05  CARRDI PIC  X(0016).
000027*    -------------------------------
000028     05  CARRL PIC S9(0004) COMP.
000029     05  CARRF PIC  X(0001).
000030     05  FILLER REDEFINES CARRF.
000031         10  CARRA PIC  X(0001).
000032     05  CARRI PIC  X(0001).
000033*    -------------------------------
000034     05  TYPEL PIC S9(0004) COMP.
000035     05  TYPEF PIC  X(0001).
000036     05  FILLER REDEFINES TYPEF.
000037         10  TYPEA PIC  X(0001).
000038     05  TYPEI PIC  X(0006).
000039*    -------------------------------
000040     05  STATUSL PIC S9(0004) COMP.
000041     05  STATUSF PIC  X(0001).
000042     05  FILLER REDEFINES STATUSF.
000043         10  STATUSA PIC  X(0001).
000044     05  STATUSI PIC  X(0006).
000045*    -------------------------------
000046     05  CLAIML PIC S9(0004) COMP.
000047     05  CLAIMF PIC  X(0001).
000048     05  FILLER REDEFINES CLAIMF.
000049         10  CLAIMA PIC  X(0001).
000050     05  CLAIMI PIC  X(0007).
000051*    -------------------------------
000052     05  INCDTEL PIC S9(0004) COMP.
000053     05  INCDTEF PIC  X(0001).
000054     05  FILLER REDEFINES INCDTEF.
000055         10  INCDTEA PIC  X(0001).
000056     05  INCDTEI PIC  X(0008).
000057*    -------------------------------
000058     05  REPDTEL PIC S9(0004) COMP.
000059     05  REPDTEF PIC  X(0001).
000060     05  FILLER REDEFINES REPDTEF.
000061         10  REPDTEA PIC  X(0001).
000062     05  REPDTEI PIC  X(0008).
000063*    -------------------------------
000064     05  CERTL PIC S9(0004) COMP.
000065     05  CERTF PIC  X(0001).
000066     05  FILLER REDEFINES CERTF.
000067         10  CERTA PIC  X(0001).
000068     05  CERTI PIC  X(0010).
000069*    -------------------------------
000070     05  SUFFIXL PIC S9(0004) COMP.
000071     05  SUFFIXF PIC  X(0001).
000072     05  FILLER REDEFINES SUFFIXF.
000073         10  SUFFIXA PIC  X(0001).
000074     05  SUFFIXI PIC  X(0001).
000075*    -------------------------------
000076     05  PDTHHD1L PIC S9(0004) COMP.
000077     05  PDTHHD1F PIC  X(0001).
000078     05  FILLER REDEFINES PDTHHD1F.
000079         10  PDTHHD1A PIC  X(0001).
000080     05  PDTHHD1I PIC  X(0010).
000081*    -------------------------------
000082     05  PAYTHRL PIC S9(0004) COMP.
000083     05  PAYTHRF PIC  X(0001).
000084     05  FILLER REDEFINES PAYTHRF.
000085         10  PAYTHRA PIC  X(0001).
000086     05  PAYTHRI PIC  X(0008).
000087*    -------------------------------
000088     05  LSTPAIDL PIC S9(0004) COMP.
000089     05  LSTPAIDF PIC  X(0001).
000090     05  FILLER REDEFINES LSTPAIDF.
000091         10  LSTPAIDA PIC  X(0001).
000092     05  LSTPAIDI PIC  X(0008).
000093*    -------------------------------
000094     05  PROCL PIC S9(0004) COMP.
000095     05  PROCF PIC  X(0001).
000096     05  FILLER REDEFINES PROCF.
000097         10  PROCA PIC  X(0001).
000098     05  PROCI PIC  X(0004).
000099*    -------------------------------
000100     05  NOPMTSL PIC S9(0004) COMP.
000101     05  NOPMTSF PIC  X(0001).
000102     05  FILLER REDEFINES NOPMTSF.
000103         10  NOPMTSA PIC  X(0001).
000104     05  NOPMTSI PIC  X(0003).
000105*    -------------------------------
000106     05  DAYPAIDL PIC S9(0004) COMP.
000107     05  DAYPAIDF PIC  X(0001).
000108     05  FILLER REDEFINES DAYPAIDF.
000109         10  DAYPAIDA PIC  X(0001).
000110     05  DAYPAIDI PIC  X(0003).
000111*    -------------------------------
000112     05  LNAMEL PIC S9(0004) COMP.
000113     05  LNAMEF PIC  X(0001).
000114     05  FILLER REDEFINES LNAMEF.
000115         10  LNAMEA PIC  X(0001).
000116     05  LNAMEI PIC  X(0012).
000117*    -------------------------------
000118     05  DIAGNL PIC S9(0004) COMP.
000119     05  DIAGNF PIC  X(0001).
000120     05  FILLER REDEFINES DIAGNF.
000121         10  DIAGNA PIC  X(0001).
000122     05  DIAGNI PIC  X(0025).
000123*    -------------------------------
000124     05  UCOUNTL PIC S9(0004) COMP.
000125     05  UCOUNTF PIC  X(0001).
000126     05  FILLER REDEFINES UCOUNTF.
000127         10  UCOUNTA PIC  X(0001).
000128     05  UCOUNTI PIC  9.
000129*    -------------------------------
000130     05  PMTTYPL PIC S9(0004) COMP.
000131     05  PMTTYPF PIC  X(0001).
000132     05  FILLER REDEFINES PMTTYPF.
000133         10  PMTTYPA PIC  X(0001).
000134     05  PMTTYPI PIC  X(0010).
000135*    -------------------------------
000136     05  CHECKL PIC S9(0004) COMP.
000137     05  CHECKF PIC  X(0001).
000138     05  FILLER REDEFINES CHECKF.
000139         10  CHECKA PIC  X(0001).
000140     05  CHECKI PIC  X(0007).
000141*    -------------------------------
000142     05  AMTPDL PIC S9(0004) COMP.
000143     05  AMTPDF PIC  X(0001).
000144     05  FILLER REDEFINES AMTPDF.
000145         10  AMTPDA PIC  X(0001).
000146     05  AMTPDI PIC  X(0010).
000147*    -------------------------------
000148     05  SELDTEL PIC S9(0004) COMP.
000149     05  SELDTEF PIC  X(0001).
000150     05  FILLER REDEFINES SELDTEF.
000151         10  SELDTEA PIC  X(0001).
000152     05  SELDTEI PIC  X(0008).
000153*    -------------------------------
000154     05  PDFROML PIC S9(0004) COMP.
000155     05  PDFROMF PIC  X(0001).
000156     05  FILLER REDEFINES PDFROMF.
000157         10  PDFROMA PIC  X(0001).
000158     05  PDFROMI PIC  X(0008).
000159*    -------------------------------
000160     05  COMMENTL PIC S9(0004) COMP.
000161     05  COMMENTF PIC  X(0001).
000162     05  FILLER REDEFINES COMMENTF.
000163         10  COMMENTA PIC  X(0001).
000164     05  COMMENTI PIC  X(0025).
000165*    -------------------------------
000166     05  PDTHHD2L PIC S9(0004) COMP.
000167     05  PDTHHD2F PIC  X(0001).
000168     05  FILLER REDEFINES PDTHHD2F.
000169         10  PDTHHD2A PIC  X(0001).
000170     05  PDTHHD2I PIC  X(0016).
000171*    -------------------------------
000172     05  PDTHRUL PIC S9(0004) COMP.
000173     05  PDTHRUF PIC  X(0001).
000174     05  FILLER REDEFINES PDTHRUF.
000175         10  PDTHRUA PIC  X(0001).
000176     05  PDTHRUI PIC  X(0008).
000177*    -------------------------------
000178     05  SEQL PIC S9(0004) COMP.
000179     05  SEQF PIC  X(0001).
000180     05  FILLER REDEFINES SEQF.
000181         10  SEQA PIC  X(0001).
000182     05  SEQI PIC  X(0004).
000183*    -------------------------------
000184     05  AREQL PIC S9(0004) COMP.
000185     05  AREQF PIC  X(0001).
000186     05  FILLER REDEFINES AREQF.
000187         10  AREQA PIC  X(0001).
000188     05  AREQI PIC  X(0001).
000189*    -------------------------------
000190     05  PDTOL PIC S9(0004) COMP.
000191     05  PDTOF PIC  X(0001).
000192     05  FILLER REDEFINES PDTOF.
000193         10  PDTOA PIC  X(0001).
000194     05  PDTOI PIC  X(0010).
000195*    -------------------------------
000196     05  PAIDBYL PIC S9(0004) COMP.
000197     05  PAIDBYF PIC  X(0001).
000198     05  FILLER REDEFINES PAIDBYF.
000199         10  PAIDBYA PIC  X(0001).
000200     05  PAIDBYI PIC  X(0004).
000201*    -------------------------------
000202     05  ALEVL PIC S9(0004) COMP.
000203     05  ALEVF PIC  X(0001).
000204     05  FILLER REDEFINES ALEVF.
000205         10  ALEVA PIC  X(0001).
000206     05  ALEVI PIC  X(0001).
000207*    -------------------------------
000208     05  RECDTEL PIC S9(0004) COMP.
000209     05  RECDTEF PIC  X(0001).
000210     05  FILLER REDEFINES RECDTEF.
000211         10  RECDTEA PIC  X(0001).
000212     05  RECDTEI PIC  X(0008).
000213*    -------------------------------
000214     05  FORCEL PIC S9(0004) COMP.
000215     05  FORCEF PIC  X(0001).
000216     05  FILLER REDEFINES FORCEF.
000217         10  FORCEA PIC  X(0001).
000218     05  FORCEI PIC  X(0001).
000219*    -------------------------------
000220     05  ERRMSGL PIC S9(0004) COMP.
000221     05  ERRMSGF PIC  X(0001).
000222     05  FILLER REDEFINES ERRMSGF.
000223         10  ERRMSGA PIC  X(0001).
000224     05  ERRMSGI PIC  X(0079).
000225*    -------------------------------
000226     05  PFKEYL PIC S9(0004) COMP.
000227     05  PFKEYF PIC  X(0001).
000228     05  FILLER REDEFINES PFKEYF.
000229         10  PFKEYA PIC  X(0001).
000230     05  PFKEYI PIC  99.
000231 01  EL143AO REDEFINES EL143AI.
000232     05  FILLER            PIC  X(0012).
000233*    -------------------------------
000234     05  FILLER            PIC  X(0003).
000235     05  DATEO PIC  X(0008).
000236*    -------------------------------
000237     05  FILLER            PIC  X(0003).
000238     05  TIMEO PIC  99.99.
000239*    -------------------------------
000240     05  FILLER            PIC  X(0003).
000241     05  MAINTO PIC  X(0001).
000242*    -------------------------------
000243     05  FILLER            PIC  X(0003).
000244     05  CARRDO PIC  X(0016).
000245*    -------------------------------
000246     05  FILLER            PIC  X(0003).
000247     05  CARRO PIC  X(0001).
000248*    -------------------------------
000249     05  FILLER            PIC  X(0003).
000250     05  TYPEO PIC  X(0006).
000251*    -------------------------------
000252     05  FILLER            PIC  X(0003).
000253     05  STATUSO PIC  X(0006).
000254*    -------------------------------
000255     05  FILLER            PIC  X(0003).
000256     05  CLAIMO PIC  X(0007).
000257*    -------------------------------
000258     05  FILLER            PIC  X(0003).
000259     05  INCDTEO PIC  X(0008).
000260*    -------------------------------
000261     05  FILLER            PIC  X(0003).
000262     05  REPDTEO PIC  X(0008).
000263*    -------------------------------
000264     05  FILLER            PIC  X(0003).
000265     05  CERTO PIC  X(0010).
000266*    -------------------------------
000267     05  FILLER            PIC  X(0003).
000268     05  SUFFIXO PIC  X(0001).
000269*    -------------------------------
000270     05  FILLER            PIC  X(0003).
000271     05  PDTHHD1O PIC  X(0010).
000272*    -------------------------------
000273     05  FILLER            PIC  X(0003).
000274     05  PAYTHRO PIC  X(0008).
000275*    -------------------------------
000276     05  FILLER            PIC  X(0003).
000277     05  LSTPAIDO PIC  X(0008).
000278*    -------------------------------
000279     05  FILLER            PIC  X(0003).
000280     05  PROCO PIC  X(0004).
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  NOPMTSO PIC  ZZ9.
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  DAYPAIDO PIC  ZZ9.
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  LNAMEO PIC  X(0012).
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  DIAGNO PIC  X(0025).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  UCOUNTO PIC  9.
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  PMTTYPO PIC  X(0010).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  CHECKO PIC  X(0007).
000302*    -------------------------------
000303     05  FILLER            PIC  X(0003).
000304     05  AMTPDO PIC  ZZZZZZZ.99.
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  SELDTEO PIC  X(0008).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  PDFROMO PIC  X(0008).
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  COMMENTO PIC  X(0025).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  PDTHHD2O PIC  X(0016).
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  PDTHRUO PIC  X(0008).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  SEQO PIC  ZZZ9.
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  AREQO PIC  X(0001).
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  PDTOO PIC  X(0010).
000329*    -------------------------------
000330     05  FILLER            PIC  X(0003).
000331     05  PAIDBYO PIC  X(0004).
000332*    -------------------------------
000333     05  FILLER            PIC  X(0003).
000334     05  ALEVO PIC  X(0001).
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  RECDTEO PIC  X(0008).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  FORCEO PIC  X(0001).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  ERRMSGO PIC  X(0079).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  PFKEYO PIC  X(0002).
000347*    -------------------------------
      *<<((file: EL143S))
000268
000269     EJECT
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
000271 01  DFHCOMMAREA             PIC X(1024).
000272
000273     EJECT
000274*    COPY ELCCNTL.
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
000275     EJECT
000276*    COPY ELCMSTR.
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
000277     EJECT
000278*    COPY ELCTRLR.
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
000279     EJECT
000280*    COPY ELCACTQ.
      *>>((file: ELCACTQ))
000001******************************************************************
000002*                                                                *
000003*                            ELCACTQ.                            *
000004*           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.004                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 60     RECFORM = FIXED                         *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
000013*       ALTERNATE INDEX = NONE                                   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
000019******************************************************************
000020
000021 01  ACTIVITY-QUE.
000022     12  AQ-RECORD-ID                PIC XX.
000023         88  VALID-AQ-ID                VALUE 'AQ'.
000024
000025     12  AQ-CONTROL-PRIMARY.
000026         16  AQ-COMPANY-CD           PIC X.
000027         16  AQ-CARRIER              PIC X.
000028         16  AQ-CLAIM-NO             PIC X(7).
000029         16  AQ-CERT-NO.
000030             20  AQ-CERT-PRIME       PIC X(10).
000031             20  AQ-CERT-SFX         PIC X.
000032
000033     12  AQ-PENDING-ACTIVITY-FLAGS.
000034         88  NO-PENDING-ACTIVITY        VALUE SPACES.
000035         16  AQ-PENDING-PAYMENT-FLAG PIC X.
000036             88  PENDING-PAYMENTS       VALUE '1'.
000037         16  AQ-PENDING-STATUS-FLAG  PIC X.
000038             88  PENDING-FULL-PRINT     VALUE '1'.
000039             88  PENDING-PART-PRINT     VALUE '2'.
000040         16  AQ-PENDING-LETTER-FLAG  PIC X.
000041             88  PENDING-LETTERS        VALUE '1'.
000042         16  AQ-PENDING-CLAIM-RESTORE PIC X.
000043             88  PENDING-RESTORE        VALUE 'C'.
000044             88  PENDING-RESTORE-LETTER VALUE 'L'.
000045
000046     12  FILLER                      PIC X(20).
000047
000048     12  AQ-RESEND-DATE              PIC XX.
000049     12  AQ-FOLLOWUP-DATE            PIC XX.
000050     12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
000051     12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
000052     12  AQ-AUTO-LETTER              PIC X(4).
000053     12  FILLER                      PIC XX.
000054     12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
000055*****************************************************************
      *<<((file: ELCACTQ))
000281     EJECT
000282*    COPY ELCCERT.
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
000283*                                    copy ELCCRTT.
      *>>((file: ELCCRTT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTT.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 552  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
000023* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
000024* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
000025* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000027* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000028* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000029* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
000030* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000031* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
000032* 091318  CR2018073000001  PEMA  ADD Refund methods
000033* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000034******************************************************************
000035
000036 01  CERTIFICATE-TRAILERS.
000037     12  CS-RECORD-ID                      PIC XX.
000038         88  VALID-CS-ID                      VALUE 'CS'.
000039
000040     12  CS-CONTROL-PRIMARY.
000041         16  CS-COMPANY-CD                 PIC X.
000042         16  CS-CARRIER                    PIC X.
000043         16  CS-GROUPING                   PIC X(6).
000044         16  CS-STATE                      PIC XX.
000045         16  CS-ACCOUNT                    PIC X(10).
000046         16  CS-CERT-EFF-DT                PIC XX.
000047         16  CS-CERT-NO.
000048             20  CS-CERT-PRIME             PIC X(10).
000049             20  CS-CERT-SFX               PIC X.
000050         16  CS-TRAILER-TYPE               PIC X.
000051             88  COMM-TRLR           VALUE 'A'.
000052             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
000053             88  CERT-DATA-TRLR      VALUE 'C'.
000054
000055     12  CS-DATA-AREA                      PIC X(516).
000056
000057     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
000058         16  CS-BANK-COMMISSION-AREA.
000059             20  CS-BANK-COMMS       OCCURS 10.
000060                 24  CS-AGT                PIC X(10).
000061                 24  CS-COM-TYP            PIC X.
000062                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
000063                 24  CS-RECALC-LV-INDIC    PIC X.
000064                 24  FILLER                PIC X(10).
000065
000066         16  FILLER                        PIC X(256).
000067
000068     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
000069****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
000070
000071         16  CS-MB-CLAIM-DATA OCCURS 24.
000072             20  CS-CLAIM-NO               PIC X(7).
000073             20  CS-CLAIM-TYPE             PIC X.
000074                 88  CS-AH-CLM               VALUE 'A'.
000075                 88  CS-IU-CLM               VALUE 'I'.
000076                 88  CS-GP-CLM               VALUE 'G'.
000077                 88  CS-LF-CLM               VALUE 'L'.
000078                 88  CS-PR-CLM               VALUE 'P'.
000079                 88  CS-FL-CLM               VALUE 'F'.
000080                 88  CS-OT-CLM               VALUE 'O'.
000081             20  CS-INSURED-TYPE           PIC X.
000082                 88  CS-PRIM-INSURED          VALUE 'P'.
000083                 88  CS-CO-BORROWER           VALUE 'C'.
000084             20  CS-BENEFIT-PERIOD         PIC 99.
000085             20  CS-DAYS-PAID              PIC S9(5) COMP-3.
000086             20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
000087             20  CS-REMAINING-BENS         PIC S999 COMP-3.
000088         16  FILLER                        PIC X(12).
000089
000090     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
000091         16  CS-VIN-NUMBER                 PIC X(17).
000092         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
000093         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
000094         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
000095         16  cs-agent-name.
000096             20  cs-agent-fname            pic x(20).
000097             20  cs-agent-mi               pic x.
000098             20  cs-agent-lname            pic x(25).
000099         16  cs-license-no                 pic x(15).
000100         16  cs-npn-number                 pic x(10).
000101         16  cs-agent-edit-status          pic x.
000102             88  cs-ae-refer-to-manager      value 'M'.
000103             88  cs-ae-cover-sheet           value 'C'.
000104             88  cs-ae-sig-form              value 'S'.
000105             88  cs-ae-verified              value 'V'.
000106             88  cs-unidentified-signature   value 'U'.
000107             88  cs-cert-returned            value 'R'.
000108             88  cs-accept-no-commission     value 'N'.
000109         16  cs-year                       pic 9999.
000110         16  cs-make                       pic x(20).
000111         16  cs-model                      pic x(20).
000112         16  cs-future                     pic x(20).
000113         16  cs-vehicle-odometer           pic s9(7) comp-3.
000114         16  cs-claim-verification-status  pic x.
000115             88  cs-clm-ver-eligible         value 'A'.
000116             88  cs-clm-ver-partial-elig     value 'B'.
000117             88  cs-clm-ver-not-eligible     value 'C'.
000118             88  cs-clm-ver-not-elig-opn-clm value 'D'.
000119             88  cs-clm-ver-not-part-elig-rw value 'E'.
000120             88  cs-clm-ver-ND-CERT          value 'F'.
000121             88  cs-clm-ver-spec-other       value 'G'.
000122             88  cs-clam-ver-pratial-corrected
000123                                             value 'H'.
000124             88  cs-clm-ver-no-matches       value 'I'.
000125             88  cs-clm-ver-not-elig-corrected
000126                                             value 'J'.
000127             88  cs-clm-ver-needs-review     value 'R'.
000128             88  cs-clm-ver-sent-to-claims   value 'W'.
000129         16  CS-LF-REFUND-METHOD           PIC X.
000130         16  CS-AH-REFUND-METHOD           PIC X.
000131         16  FILLER                        PIC X(353). *> was 420
000132*        16  FILLER                        PIC X(496).
      *<<((file: ELCCRTT))
000285*    COPY ELCDAR.
      *>>((file: ELCDAR))
000001******************************************************************
000002*                                                                *
000003*   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
000004*   FILE TYPE = VSAM,KSDS                                        *
000005*   RECORD SIZE = 25   RECFORM = FIXED                           *
000006*   BASE CLUSTER = DLYACTV                                       *
000007*   LOG = YES                                                    *
000008*   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
000009*               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
000010*               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
000011*               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
000012*               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
000013*               BY PROGRAM "LGINFCE".                            *
000014*                                                                *
000015******************************************************************
000016 01  DAILY-ACTIVITY-RECORD.
000017     05  DA-KEY.
000018         10  DA-COMP-CD          PIC X.
000019         10  DA-CARRIER          PIC X.
000020         10  DA-CLAIM-NO         PIC X(7).
000021         10  DA-CERT-NO.
000022             15  DA-CERT-PRIME   PIC X(10).
000023             15  DA-CERT-SFX     PIC X.
000024     05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
000025     05  DA-RECORD-TYPE          PIC X.
000026     05  FILLER                  PIC X(2).
000027******************************************************************
      *<<((file: ELCDAR))
000286     EJECT
000287*    COPY MPCPLCY.
      *>>((file: MPCPLCY))
000001******************************************************************
000002*                                                                *
000003*                           MPCPLCY                              *
000004*                            VMOD=1.024                          *
000005*                                                                *
000006*   FILE DESCRIPTION = POLICY MASTER                             *
000007*                                                                *
000008*   FILE TYPE = VSAM,KSDS                                        *
000009*   RECORD SIZE = 1200 RECFORM = FIXED                           *
000010*                                                                *
000011*   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
000012*       ALTERNATE PATH2 = ** NOT USED **                         *
000013*       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
000014*       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
000015*       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
000016*       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
000017*       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
000018*                                                                *
000019*   LOG = YES                                                    *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021******************************************************************
000022**WARNING*********************************************************
000023**ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
000024**TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
000025**                             MPCPHSTD                          *
000026**                             MPCPHSTC                          *
000027**                             MPCPHSTT                          *
000028**                                                               *
000029******************************************************************
000030
000031 01  POLICY-MASTER.
000032     12  PM-RECORD-ID                      PIC XX.
000033         88  VALID-PM-ID                      VALUE 'PM'.
000034
000035******************************************************************
000036*   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
000037******************************************************************
000038
000039     12  PM-CONTROL-PRIMARY.
000040         16  PM-PRODUCER-PRIMARY.
000041             20  PM-PROD-PRIMARY.
000042                 24  PM-COMPANY-CD         PIC X.
000043                 24  PM-CGSP-KEY.
000044                     28  PM-CARRIER        PIC X.
000045                     28  PM-GROUPING.
000046                         32  PM-GROUPING-PREFIX
000047                                           PIC X(3).
000048                         32  PM-GROUPING-PRIME
000049                                           PIC X(3).
000050                     28  PM-STATE          PIC X(2).
000051                     28  PM-PRODUCER.
000052                         32  PM-PRODUCER-PREFIX
000053                                           PIC X(4).
000054                         32  PM-PRODUCER-PRIME
000055                                           PIC X(6).
000056             20  PM-POLICY-EFF-DT              PIC XX.
000057         16  PM-REFERENCE-NUMBER.
000058             20  PM-REFNO-PRIME            PIC X(18).
000059             20  PM-REFNO-SFX              PIC XX.
000060
000061******************************************************************
000062*       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
000063******************************************************************
000064
000065     12  PM-CONTROL-BY-SSN.
000066         16  PM-COMPANY-CD-A3              PIC X.
000067         16  PM-SOC-SEC-NO.
000068             20  PM-SSN-STATE              PIC XX.
000069             20  PM-SSN-PRODUCER           PIC X(6).
000070             20  PM-SSN-LN3.
000071                 25  PM-INSURED-INITIALS-A3.
000072                     30 PM-INSURED-INITIAL1-A3 PIC X.
000073                     30 PM-INSURED-INITIAL2-A3 PIC X.
000074                 25 PM-PART-LAST-NAME-A3         PIC X.
000075         16  PM-DATE-A3                     PIC XX.
000076         16  PM-TIME-A3                     PIC S9(04)   COMP.
000077
000078******************************************************************
000079*       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
000080******************************************************************
000081
000082     12  PM-CONTROL-BY-POLICY-NO.
000083         16  PM-COMPANY-CD-A4              PIC X.
000084         16  PM-POLICY-NO-A4.
000085             20  PM-POLICY-PRIME-A4        PIC X(18).
000086             20  PM-POLICY-SFX-A4          PIC XX.
000087         16  PM-DATE-A4                    PIC XX.
000088         16  PM-TIME-A4                    PIC S9(04)   COMP.
000089
000090******************************************************************
000091*       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
000092******************************************************************
000093
000094     12  PM-CONTROL-BY-ACCOUNT.
000095         16  PM-COMPANY-CD-A5              PIC X.
000096         16  PM-BANK-ACCOUNT-NUMBER        PIC X(20).
000097         16  PM-DATE-A5                    PIC XX.
000098         16  PM-TIME-A5                    PIC S9(07)   COMP.
000099
000100******************************************************************
000101*       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
000102******************************************************************
000103
000104     12  PM-CONTROL-BY-TRANSIT.
000105         16  PM-COMPANY-CD-A6              PIC X.
000106         16  PM-BANK-TRANSIT-NUMBER.
000107             20  PM-FEDERAL-NUMBER         PIC X(4).
000108             20  PM-BANK-NUMBER            PIC X(4).
000109         16  PM-DATE-A6                    PIC XX.
000110         16  PM-TIME-A6                    PIC S9(07)   COMP.
000111
000112******************************************************************
000113*       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
000114******************************************************************
000115
000116     12  PM-CONTROL-BY-LOAN-NO.
000117         16  PM-COMPANY-CD-A7              PIC X.
000118         16  PM-LOAN-NUMBER                PIC X(20).
000119         16  PM-DATE-A7                    PIC XX.
000120         16  PM-TIME-A7                    PIC S9(07)   COMP.
000121
000122******************************************************************
000123*                 FILE SYNCHRONIZATION DATA                      *
000124******************************************************************
000125
000126     12  FILLER                            PIC X(05).
000127     12  PM-FILE-SYNCH-DATA.
000128         16  PM-LAST-CHANGE-DT             PIC XX.
000129         16  PM-LAST-CHANGE-TIME           PIC S9(7)    COMP.
000130         16  PM-LAST-CHANGE-PROCESSOR      PIC X(4).
000131     12  FILLER                            PIC X(05).
000132
000133******************************************************************
000134*                    INSUREDS PROFILE DATA                       *
000135******************************************************************
000136
000137     12  PM-INSURED-PROFILE-DATA.
000138         16  PM-INSURED-NAME.
000139             20  PM-INSURED-LAST-NAME     PIC X(15).
000140             20  PM-INSURED-FIRST-NAME.
000141                 24  PM-INSURED-1ST-INIT PIC X.
000142                 24  FILLER               PIC X(9).
000143             20  PM-INSURED-MIDDLE-INIT PIC X.
000144         16  PM-INSURED-ADDRESS.
000145             20  PM-ADDRESS-LINE-1         PIC X(30).
000146             20  PM-ADDRESS-LINE-2         PIC X(30).
000147             20  PM-CITY                   PIC X(25).
000148             20  PM-RESIDENT-STATE         PIC XX.
000149             20  PM-ZIP-CD.
000150                 24  PM-ZIP-FIRST-FIVE     PIC X(5).
000151                 24  PM-ZIP-PLUS-FOUR      PIC X(4).
000152         16  PM-INSURED-PERSONAL.
000153             20  PM-INSURED-OCC-CLASS      PIC X.
000154                 88  PM-PREFERRED            VALUE '1'.
000155                 88  PM-STANDARD             VALUE '2'.
000156                 88  PM-HAZARDOUS            VALUE '3'.
000157                 88  PM-VERY-HAZARDOUS       VALUE '4'.
000158                 88  PM-EXTREME-HAZARDOUS VALUE '5'.
000159                 88  PM-NOT-OCC              VALUE '6'.
000160                 88  PM-OCC-UNKNOWN          VALUE '9'.
000161             20  PM-INSURED-OCC-CD         PIC X(3).
000162             20  PM-INSURED-OCC-CD-NUM REDEFINES
000163                 PM-INSURED-OCC-CD         PIC 9(3).
000164             20  PM-INSURED-SEX            PIC X.
000165                 88  PM-INSURED-SEX-MALE      VALUE 'M'.
000166                 88  PM-INSURED-SEX-FEMALE VALUE 'F'.
000167             20  PM-INSURED-BIRTH-DT       PIC XX.
000168             20  PM-INSURED-ISSUE-AGE      PIC S9(3)     COMP-3.
000169             20  PM-INSURED-HEIGHT-FT      PIC S9(3)     COMP-3.
000170             20  PM-INSURED-HEIGHT-IN      PIC S9(3)     COMP-3.
000171             20  PM-INSURED-WEIGHT         PIC S9(3)     COMP-3.
000172             20  PM-INSURED-BIRTH-STATE PIC XX.
000173             20  PM-INSURED-PHONE-NO       PIC X(13).
000174             20  PM-INSURED-RATED-AGE      PIC S9(3)     COMP-3.
000175         16  PM-INS-LANGUAGE-IND           PIC X(01).
000176             88  PM-ENGLISH                           VALUE 'E'.
000177             88  PM-FRENCH                            VALUE 'F'.
000178             88  PM-SPANISH                           VALUE 'S'.
000179         16  PM-INSURED-TOT-BENEFIT        PIC S9(7)V99  COMP-3.
000180
000181         16  PM-INSURED-AGE-IND            PIC X(01).
000182             88  PM-INSURED-AGE-75-REACHED            VALUE 'Y'.
000183     12  FILLER                            PIC X(13).
000184
000185******************************************************************
000186*                JOINT INSUREDS PROFILE DATA                     *
000187******************************************************************
000188
000189     12  PM-JOINT-PROFILE-DATA.
000190         16  PM-JOINT-NAME.
000191             20  PM-JOINT-LAST-NAME        PIC X(15).
000192             20  PM-JOINT-FIRST-NAME.
000193                 24  PM-JOINT-1ST-INIT     PIC X.
000194                 24  FILLER                PIC X(9).
000195             20  PM-JOINT-MIDDLE-INIT      PIC X.
000196         16  PM-JOINT-SOC-SEC-NO.
000197             20  PM-JT-SSN-STATE           PIC XX.
000198             20  PM-JT-SSN-PRODUCER        PIC X(6).
000199             20  PM-JT-SSN-LN3.
000200                 25  PM-JT-INSURED-INITIALS-A3.
000201                     30 PM-JT-INSURED-INITIAL1-A3 PIC X.
000202                     30 PM-JT-INSURED-INITIAL2-A3 PIC X.
000203                 25 PM-JT-PART-LAST-NAME-A3        PIC X.
000204         16  PM-JOINT-PERSONAL.
000205             20  PM-JOINT-OCC-CLASS        PIC X.
000206                 88 PM-JNT-PREFERRED          VALUE '1'.
000207                 88 PM-JNT-STANDARD           VALUE '2'.
000208                 88 PM-JNT-HAZARDOUS          VALUE '3'.
000209                 88 PM-JNT-VERY-HAZARDOUS     VALUE '4'.
000210                 88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
000211                 88 PM-JNT-NOT-OCC            VALUE '6'.
000212                 88 PM-JNT-OCC-UNKNOWN        VALUE '9'.
000213             20  PM-JOINT-OCC-CD           PIC X(3).
000214             20  PM-JOINT-SEX              PIC X.
000215                 88  PM-JOINT-SEX-MALE        VALUE 'M'.
000216                 88  PM-JOINT-SEX-FEMALE      VALUE 'F'.
000217             20  PM-JOINT-BIRTH-DT         PIC XX.
000218             20  PM-JOINT-ISSUE-AGE        PIC S9(3)     COMP-3.
000219             20  PM-JOINT-HEIGHT-FT        PIC S9(3)     COMP-3.
000220             20  PM-JOINT-HEIGHT-IN        PIC S9(3)     COMP-3.
000221             20  PM-JOINT-WEIGHT           PIC S9(3)     COMP-3.
000222             20  PM-JOINT-BIRTH-STATE      PIC XX.
000223             20  PM-JOINT-RATED-AGE        PIC S9(3)     COMP-3.
000224         16  PM-JOINT-TOT-BENEFIT          PIC S9(7)V99  COMP-3.
000225         16  PM-JOINT-AGE-IND              PIC X(01).
000226             88  PM-JOINT-AGE-75-REACHED              VALUE 'Y'.
000227
000228     12  FILLER                            PIC X(12).
000229
000230******************************************************************
000231*                  INSURANCE COVERAGE DATA                       *
000232******************************************************************
000233
000234     12  PM-INS-COVERAGE-DATA.
000235         16  PM-FREE-PERIOD                PIC S9(03)    COMP-3.
000236         16  PM-LOAN-TERM                  PIC S9(3)     COMP-3.
000237         16  PM-LOAN-APR                   PIC S9V9999   COMP-3.
000238         16  PM-LOAN-DT                    PIC XX.
000239         16  PM-LOAN-PYMT                  PIC S9(5)V99  COMP-3.
000240         16  PM-LOAN-BALC                  PIC S9(7)V99  COMP-3.
000241         16  PM-INS-BENEFIT-MONTHS         PIC S9(3)     COMP-3.
000242         16  PM-INS-MONTH-BENEFIT          PIC S9(7)V99  COMP-3.
000243         16  PM-INS-TOTAL-BENEFIT          PIC S9(7)V99  COMP-3.
000244         16  PM-INS-PLAN-TYPE              PIC X.
000245             88  PM-AH-MORT-PLAN              VALUE 'A'.
000246             88  PM-AD-D-MORT-PLAN            VALUE 'E'.
000247             88  PM-DISMEM-MORT-PLAN          VALUE 'D'.
000248             88  PM-LIFE-MORT-PLAN            VALUE 'L'.
000249         16  PM-INS-PLAN-CD                PIC XX.
000250         16  PM-INS-PLAN-REVISION          PIC X(3).
000251         16  PM-INS-POLICY-FORM            PIC X(12).
000252         16  PM-INS-MSTR-POLICY.
000253             20  PM-FREE-TYPE              PIC X(04).
000254             20  FILLER                    PIC X(08).
000255         16  PM-INS-MSTR-APP.
000256             20  FILLER                    PIC X(11).
000257             20  PM-INS-B-C-TYPE           PIC X(01).
000258         16  PM-INS-RATE-CD                PIC X(5).
000259         16  PM-INS-SEX-RATING             PIC X.
000260             88  PM-NOT-SEX-RATED              VALUE '1'.
000261             88  PM-SEX-RATED                  VALUE '2'.
000262         16  PM-INS-SUBSTANDARD-PCT        PIC S9V9999   COMP-3.
000263         16  PM-INS-SUBSTANDARD-TYPE       PIC X.
000264         16  PM-INS-TERMINATION-DT         PIC XX.
000265         16  PM-INS-MONTH-PREMIUM      PIC S9(5)V999999  COMP-3.
000266         16  PM-INS-CALC-MO-PREM       PIC S9(5)V999999  COMP-3.
000267         16  PM-REINSURANCE-TABLE          PIC X(3).
000268         16  PM-MORTALITY-CD               PIC X(4).
000269         16  PM-INS-TYPE                   PIC X.
000270             88  PM-INDIVIDUAL                VALUES ARE '1' 'I'.
000271             88  PM-GROUP                     VALUES ARE '2' 'G'.
000272         16  PM-LOAN-OFFICER               PIC X(5).
000273         16  PM-POLICY-FEE                 PIC S9(3)V99 COMP-3.
000274         16  PM-DEPENDENT-COUNT            PIC S99      COMP-3.
000275         16  PM-CWA-AMOUNT                 PIC S9(5)V99  COMP-3.
000276         16  PM-LAST-AUTO-RERATE-DT        PIC XX.
000277         16  PM-PREM-FINANCED-SW           PIC X.
000278             88  PM-PREM-FINANCED              VALUE 'Y'.
000279             88  PM-PREM-NOT-FINANCED          VALUE 'N'.
000280
000281         16  PM-INS-TERM-LETTER-IND        PIC X.
000282             88  PM-TERM-INITIALIZED           VALUE 'Y'.
000283         16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99     COMP-3.
000284     12  FILLER                            PIC X(11).
000285
000286******************************************************************
000287*                    POLICY BILLING DATA                         *
000288******************************************************************
000289
000290     12  PM-BILLING-DATA.
000291         16  PM-BILLING-MODE               PIC X(1).
000292             88  PM-ANNUAL                    VALUE '1'.
000293             88  PM-SEMI-ANNUAL               VALUE '2'.
000294             88  PM-QUARTERLY                 VALUE '3'.
000295             88  PM-MONTHLY                   VALUE '4'.
000296             88  PM-BI-MONTHLY                VALUE '5'.
000297             88  PM-SINGLE-PREM               VALUE '6'.
000298         16  PM-BILLING-SCHEDULE           PIC X(1).
000299         16  PM-BILLING-SW                 PIC X(1).
000300             88  PM-FIRST-BILLING             VALUE 'Y'.
000301             88  PM-PAID-IN-ADVANCE           VALUE 'A'.
000302             88  PM-POLICY-FEE-REFUNDED       VALUE 'F'.
000303         16  PM-BILLING-TYPE               PIC X(1).
000304             88  PM-LIST-BILL                 VALUE '1'.
000305             88  PM-TAPE-BILL                 VALUE '2'.
000306             88  PM-TAPE-LIST-BILL            VALUE '3'.
000307             88  PM-GROUP-BILL          VALUE ARE '1' '2' '3'.
000308             88  PM-DIRECT-BILL               VALUE '4'.
000309             88  PM-PAC-BILL            VALUE ARE '5' 'C' 'S'.
000310             88  PM-CHARGE-CARD-BILL          VALUE '6'.
000311             88  PM-INDIV-BILL
000312                                  VALUE ARE '4' '5' '6' 'C' 'S'.
000313             88  PM-GRP-PLCY-BILL             VALUE '7'.
000314             88  PM-GRP-PLCY-PAC              VALUE '8'.
000315             88  PM-GRP-PLCY-CR-CRD           VALUE '9'.
000316             88  PM-GRP-PLCY            VALUE ARE '7' '8' '9'.
000317             88  PM-GRP-PROD                  VALUE 'A'.
000318             88  PM-EFT-CHECKING              VALUE 'C'.
000319             88  PM-EFT-SAVINGS               VALUE 'S'.
000320         16  PM-PAYMENT-AMT                PIC S9(5)V99  COMP-3.
000321         16  PM-OVER-SHORT-AMT             PIC S9(5)V99  COMP-3.
000322         16  PM-LAST-BILL-DT               PIC XX.
000323         16  PM-LAST-BILL-AMT              PIC S9(5)V99  COMP-3.
000324         16  PM-BILL-TO-DT                 PIC XX.
000325         16  PM-LAST-PYMT-DT               PIC XX.
000326         16  PM-PAID-TO-DT                 PIC XX.
000327         16  PM-PYMT-INVOICE-NUMBER        PIC X(6).
000328         16  PM-MONTHS-PAID                PIC S9(3)     COMP-3.
000329         16  PM-TOTAL-PREM-RECVD           PIC S9(7)V99  COMP-3.
000330         16  PM-BILLING-GROUPING-CODE      PIC X(6).
000331         16  PM-CHARGE-CARD-EXP-DT         PIC X(2).
000332         16  PM-CHARGE-CARD-TYPE           PIC X(2).
000333             88  PM-VISA                      VALUE 'VI'.
000334             88  PM-MSTR-CARD                 VALUE 'MC'.
000335             88  PM-DINERS-CLUB               VALUE 'DN'.
000336             88  PM-DISCOVER                  VALUE 'DS'.
000337             88  PM-CARTE-BLANCHE             VALUE 'CB'.
000338             88  PM-AMERICAN-EXPRESS          VALUE 'AE'.
000339         16  PM-BILL-INVOICE-NUMBER        PIC X(6).
000340         16  PM-BILL-DAY                   PIC S99       COMP-3.
000341         16  PM-RES-PREM-TAX           PIC S9(3)V999999  COMP-3.
000342     12  FILLER                            PIC X(15).
000343
000344******************************************************************
000345*                     CLAIM PAYMENT DATA                         *
000346******************************************************************
000347
000348     12  PM-CLAIM-PAYMENT-DATA.
000349         16  PM-CLAIM-BENEFICIARY-NAME     PIC X(25).
000350         16  PM-CLAIM-INTERFACE-SW         PIC X.
000351             88  PM-NO-CLAIM-ATTACHED         VALUE SPACE.
000352             88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
000353             88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
000354             88  PM-CLAIM-CLOSED              VALUE '3'.
000355             88  PM-ACTIVE-CLAIM              VALUE '1' '2'.
000356             88  PM-CLAIM-ATTACHED            VALUE '1' '2' '3'.
000357         16  PM-CLAIM-INCURRED-DT          PIC XX.
000358         16  PM-CLAIM-PAID-TO-DT           PIC XX.
000359         16  PM-CLAIM-PAYMENT-CNT          PIC S9(3)     COMP-3.
000360         16  PM-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99  COMP-3.
000361         16  PM-CLAIM-EXPENSES-ITD         PIC S9(7)V99  COMP-3.
000362         16  PM-CLAIM-PAYMENTS-ITD         PIC S9(7)V99  COMP-3.
000363         16  PM-CLAIM-ACCUMULATOR          PIC S9(7)V99  COMP-3.
000364         16  PM-CLAIM-ATTACH-CNT           PIC S9(3)     COMP-3.
000365         16  PM-CLAIM-LIFE-ITD             PIC S9(7)V99  COMP-3.
000366         16  PM-CLAIM-AH-ITD               PIC S9(7)V99  COMP-3.
000367         16  PM-CLAIM-RIDER-ITD            PIC S9(7)V99  COMP-3.
000368
000369     12  FILLER                            PIC X(03).
000370
000371******************************************************************
000372*                POLICY STATUS AND DISPOSITION                   *
000373******************************************************************
000374
000375     12  PM-STATUS-DISPOSITION-DATA.
000376         16  PM-ISSUE-EOM-DT               PIC XX.
000377         16  PM-REPLACEMENT-SWITCH         PIC X.
000378         16  PM-APPL-SIGN-DT               PIC XX.
000379         16  PM-UNDERWRITER                PIC X(3).
000380         16  PM-ENTRY-PROCESSOR            PIC X(4).
000381         16  PM-ENTRY-STATUS               PIC X.
000382             88  PM-NORMAL                    VALUE '1'.
000383             88  PM-TAKE-OVER                 VALUE '2'.
000384             88  PM-CONVERSION                VALUE '4'.
000385             88  PM-RE-ISSUE                  VALUE '5'.
000386             88  PM-REINSURANCE-ONLY          VALUE '9'.
000387         16  PM-ENTRY-DT                   PIC XX.
000388         16  PM-ENTRY-TIME                 PIC S9(7) COMP-3.
000389         16  PM-EXIT-DT                    PIC XX.
000390         16  PM-CURRENT-STATUS             PIC X.
000391             88  PM-LAPSE                     VALUE '0'.
000392             88  PM-ACTIVE                    VALUE '1'.
000393             88  PM-PENDING-ISSUE             VALUE '2'.
000394             88  PM-DECLINED                  VALUE '3'.
000395             88  PM-PENDING-CANCEL            VALUE '4'.
000396             88  PM-PENDING-ISSUE-ERROR       VALUE '5'.
000397             88  PM-CLAIM-APPLIED             VALUE '6'.
000398             88  PM-CANCEL                    VALUE '7'.
000399             88  PM-PENDING-UNWTR-REVW        VALUE '8'.
000400             88  PM-PENDING-CANCEL-ERROR      VALUE '9'.
000401             88  PM-CANCEL-TRANSFER           VALUE 'C'.
000402             88  PM-CLAIM-SETTLEMENT          VALUE 'F'.
000403             88  PM-TERMINATE                 VALUE 'T'.
000404** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
000405** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
000406** THESE GROUPS.
000407             88  PM-TYPE-STAT-1
000408                     VALUES ARE '0' '1' '4' '6' '7' '9'
000409                                'C' 'F' 'T'.
000410             88  PM-TYPE-STAT-2
000411                     VALUES ARE '2' '3' '5' '8'.
000412             88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
000413             88  PM-PENDING-STATUS
000414                                VALUES ARE '2' '4' '5' '8' '9'.
000415             88  PM-PENDING-ISSUE-STATUS
000416                                VALUES ARE '2' '5' '8'.
000417             88  PM-CANCEL-STATUS
000418                                VALUES ARE '4' '7' '9' 'C'.
000419         16  PM-CANCEL-CAUSE-CD            PIC X(3).
000420         16  PM-CANCEL-DT                  PIC XX.
000421         16  PM-REFUND-AMT                 PIC S9(5)V99  COMP-3.
000422         16  PM-CALC-REFUND-AMT            PIC S9(5)V99  COMP-3.
000423         16  PM-DECLINE-CD                 PIC X(3).
000424         16  PM-DECLINE-DT                 PIC XX.
000425         16  PM-LAST-LAPSE-DT              PIC XX.
000426         16  PM-LAST-REINSTATE-DT          PIC XX.
000427         16  PM-SECURITY-ACCESS-CODE       PIC X.
000428         16  PM-PREV-CONTROL-PRIMARY.
000429             20  PM-PREV-COMPANY-CD             PIC X.
000430             20  PM-PREV-CARRIER                PIC X.
000431             20  PM-PREV-GROUPING.
000432                 24  PM-PREV-GROUPING-PREFIX PIC X(3).
000433                 24  PM-PREV-GROUPING-PRIME     PIC X(3).
000434             20  PM-PREV-STATE                  PIC XX.
000435             20  PM-PREV-PRODUCER.
000436                 24  PM-PREV-PRODUCER-PREFIX PIC X(4).
000437                 24  PM-PREV-PRODUCER-PRIME     PIC X(6).
000438             20  PM-PREV-POLICY-EFF-DT          PIC XX.
000439             20  PM-PREV-REFERENCE-NUMBER.
000440                 24  PM-PREV-REFNO-PRIME        PIC X(18).
000441                 24  PM-PREV-REFNO-SFX          PIC XX.
000442         16  PM-ACTION-DT                  PIC XX.
000443         16  PM-ACTION-CODE                PIC X(3).
000444         16  PM-ACTION-DT-2                PIC XX.
000445         16  PM-ACTION-CODE-2              PIC X(3).
000446         16  PM-ACTION-DT-3                PIC XX.
000447         16  PM-ACTION-CODE-3              PIC X(3).
000448         16  PM-ACTION-DT-4                PIC XX.
000449         16  PM-ACTION-CODE-4              PIC X(3).
000450         16  PM-ACTION-DT-5                PIC XX.
000451         16  PM-ACTION-CODE-5              PIC X(3).
000452
000453         16  PM-KEY-CHANGE                 PIC X.
000454                 88  PM-NO-KEY-CHG      VALUES ARE ' ' 'N'.
000455                 88  PM-KEY-CHG              VALUE 'Y'.
000456         16  PM-KEY-CHANGE-DT              PIC XX.
000457
000458         16  PM-RTI-INDICATOR              PIC X.
000459         16  PM-REASON-CODE                PIC X(3).
000460         16  PM-IN-OUT-PROCESSING-IND      PIC X(1).
000461             88  PM-IN-OUT-PROCESSING      VALUE 'Y'.
000462             88  PM-NOT-IN-OUT-PROCESSING  VALUE SPACES.
000463
000464     12  FILLER                            PIC X(12).
000465
000466******************************************************************
000467*                 AGENT AND COMMISSION DATA                      *
000468******************************************************************
000469
000470     12  PM-COMMISSION-DATA.
000471         16  PM-REMIT-TO                   PIC S9(3) COMP-3.
000472         16  PM-COMM-CHANGE-SW             PIC X.
000473                 88  PM-COMMISSION-CHANGE     VALUE 'Y'.
000474         16  PM-AGENT-INFORMATION OCCURS     5 TIMES.
000475             20  PM-AGENT-NUMBER           PIC X(10).
000476             20  PM-AGENT-TYPE             PIC X.
000477                 88  PM-PRODUCER-LEVEL-AGENT
000478                                              VALUES ARE 'C' 'D'.
000479                 88  PM-AGENT-GROSS           VALUE 'C'.
000480                 88  PM-AGENT-REINS           VALUE 'R'.
000481                 88  PM-AGENT-GROSS-REINS     VALUE 'D'.
000482                 88  PM-OVERWRITE-GROSS       VALUE 'O'.
000483                 88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
000484                 88  PM-OVERWRITE-REINS       VALUE 'T'.
000485                 88  PM-REINS-ONLY            VALUE 'W'.
000486             20  PM-COMMISSION-BILL-PAID PIC X(1).
000487                 88  PM-GENERATE-BILL         VALUE 'B'.
000488                 88  PM-GENERATE-PAID         VALUE 'P'.
000489             20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
000490             20  PM-COMP-1ST-YEAR-TYPE     PIC X(1).
000491                 88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
000492                 88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
000493                 88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
000494             20  PM-RENEWAL-DATA.
000495                 24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
000496                     28  PM-RENEW-MONTHS     PIC S999    COMP-3.
000497                     28  PM-RENEW-COMMISSION
000498                                             PIC S99V999 COMP-3.
000499                     28  PM-RENEW-TYPE       PIC X(1).
000500                         88  PM-COMP-RENEW-PERCENT      VALUE '1'.
000501                         88  PM-COMP-RENEW-DOLLARS      VALUE '2'.
000502                         88  PM-COMP-RENEW-NOT-USED     VALUE '3'.
000503             20  PM-COMP-RECALC-FLAG       PIC X(1).
000504                 88  PM-BYPASS-RECALC         VALUE 'N'.
000505     12  FILLER                            PIC X(20).
000506******************************************************************
000507*             CUSTOMER DATA                                      *
000508******************************************************************
000509     12  PM-CUSTOMER-ID                    PIC X(20).
000510******************************************************************
000511     12  FILLER                            PIC X(43).
000512******************************************************************
      *<<((file: MPCPLCY))
000288
000289     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL143' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000290 VCOBOL-DUMMY-PROCEDURE.
000291
000292     MOVE EIBTRMID               TO WS-QID-TERM.
000293
000294     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000295     MOVE '5'                    TO DC-OPTION-CODE.
000296     PERFORM 9700-DATE-LINK.
000297     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000298     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000299
000300     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000301     MOVE 1                      TO EMI-NUMBER-OF-LINES.
000302
000303     IF EIBCALEN EQUAL 0
000304         GO TO 8800-UNAUTHORIZED-ACCESS.
000305
000306     IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
000307         IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
000308             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000309             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000310             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000311             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000312             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000313             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000314             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000315             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000316         ELSE
000317             MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
000318             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000319             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000320             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000321             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000322             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000323             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000324             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000325             MOVE SPACES               TO PI-SAVED-PROGRAM-6
000326             PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT
000327             GO TO 3000-SHOW-CLAIM-PAYMENT.
000328
000329     
      * EXEC CICS    HANDLE    CONDITION
000330*         PGMIDERR          (9600-PGMID-ERROR)
000331*         ERROR             (9990-ABEND)
000332*    END-EXEC.
      *    MOVE '"$L.                  ! " #00005467' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303035343637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000333
000334 0100-SEND-NEW.
000335     IF  EIBTRNID NOT EQUAL TRANS-ID
000336         MOVE LOW-VALUES         TO  EL143AI PI-LAST-ELACTQ-KEY
000337         MOVE 'Y'                TO  PI-FIRST-TIME-SW
000338         MOVE +0                 TO  PI-UNAPPROVED-COUNT
000339                                     PI-LAST-TRLR-SEQ-NO
000340                                     PI-ELTRLR-UPDATE-HHMMSS
000341         MOVE SPACES             TO  PI-ELTRLR-UPDATE-BY
000342                                     PI-DIAGNOSIS
000343         GO TO 8100-SEND-INITIAL-MAP.
000344
000345     IF EIBAID EQUAL DFHCLEAR
000346         GO TO 9400-CLEAR.
000347
000348     IF PI-PROCESSOR-ID EQUAL 'LGXX'
000349         GO TO 0200-RECEIVE.
000350
000351     
      * EXEC CICS  READQ TS
000352*        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000353*        INTO    (SECURITY-CONTROL)
000354*        LENGTH  (SC-COMM-LENGTH)
000355*        ITEM    (SC-ITEM)
000356*    END-EXEC.
      *    MOVE '*$II   L              ''   #00005489' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035343839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000357
000358     MOVE SC-CLAIMS-DISPLAY (28)  TO  PI-DISPLAY-CAP.
000359     MOVE SC-CLAIMS-UPDATE  (28)  TO  PI-MODIFY-CAP.
000360
000361     IF NOT DISPLAY-CAP
000362         MOVE 'READ'          TO SM-READ
000363         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000364         MOVE ER-0070        TO  EMI-ERROR
000365         PERFORM 9900-ERROR-FORMAT
000366         GO TO 8100-SEND-INITIAL-MAP.
000367
000368 0200-RECEIVE.
000369
000370     IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
000371        MOVE ER-7008            TO EMI-ERROR
000372        PERFORM 9900-ERROR-FORMAT
000373        MOVE -1                 TO MAINTL
000374        GO TO 8200-SEND-DATAONLY.
000375
000376     
      * EXEC CICS RECEIVE
000377*        MAP      (MAP-NAME)
000378*        MAPSET   (MAPSET-NAME)
000379*        INTO     (EL143AI)
000380*    END-EXEC.
           MOVE LENGTH OF
            EL143AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005514' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035353134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL143AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000381
000382     IF  PFKEYL EQUAL +0
000383         GO TO 0300-CHECK-PFKEYS.
000384
000385     IF  EIBAID NOT EQUAL DFHENTER
000386         MOVE ER-0004            TO EMI-ERROR
000387         GO TO 0320-INPUT-ERROR.
000388
000389     IF PFKEYI NOT NUMERIC
000390        MOVE ER-0029     TO EMI-ERROR
000391        GO TO 0320-INPUT-ERROR.
000392
000393     IF PFKEYI GREATER THAN 01 AND
000394                  LESS THAN 25
000395        MOVE PF-VALUES (PFKEYI) TO EIBAID
000396     ELSE
000397        MOVE ER-0029        TO EMI-ERROR
000398        GO TO 0320-INPUT-ERROR.
000399
000400 0300-CHECK-PFKEYS.
000401     IF EIBAID EQUAL DFHPF23
000402         GO TO 8810-PF23.
000403
000404     IF EIBAID EQUAL DFHPF24
000405         GO TO 9200-RETURN-MAIN-MENU.
000406
000407     IF EIBAID EQUAL DFHPF12
000408         GO TO 9500-PF12.
000409
000410     IF EIBAID = DFHPF6 AND
000411         PI-LAST-ELACTQ-KEY  NOT EQUAL TO LOW-VALUES
000412         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
000413         MOVE XCTL-150               TO  PGM-NAME
000414         GO TO 9300-XCTL.
000415
000416     IF MAINTL GREATER THAN +0 AND
000417              EIBAID NOT EQUAL DFHENTER
000418        MOVE -1             TO  MAINTL
000419        MOVE  ER-0050       TO  EMI-ERROR
000420        PERFORM 9900-ERROR-FORMAT
000421        GO TO 8200-SEND-DATAONLY.
000422
000423     PERFORM 7600-READ-COMPANY-REC THRU 7600-EXIT.
000424     PERFORM 7700-READ-USER-REC    THRU 7700-EXIT.
000425
000426     IF  EIBAID EQUAL DFHPF1
000427         GO TO 7000-BROWSE-FWRD-NEXT-CLAIM.
000428
000429     IF  EIBAID EQUAL DFHPF2
000430         GO TO 7100-BROWSE-BWRD-NEXT-CLAIM.
000431
000432     IF  EIBAID EQUAL DFHPF3
000433         GO TO 7200-BROWSE-FWRD-NEXT-PAYMENT.
000434
000435     IF  EIBAID EQUAL DFHPF4
000436         GO TO 7300-BROWSE-BWRD-NEXT-PAYMENT.
000437
000438     IF EIBAID = DFHPF5
000439         GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
000440
000441     IF EIBAID EQUAL DFHENTER
000442         GO TO 0400-EDIT-INPUT-DATA.
000443
000444     MOVE ER-0029                TO EMI-ERROR.
000445
000446 0320-INPUT-ERROR.
000447
000448     PERFORM 9900-ERROR-FORMAT.
000449     MOVE AL-UNBON               TO PFKEYA.
000450     IF PFKEYL EQUAL 0
000451         MOVE -1                 TO MAINTL
000452     ELSE
000453         MOVE -1                 TO PFKEYL.
000454
000455     GO TO 8200-SEND-DATAONLY.
000456
000457     EJECT
000458 0400-EDIT-INPUT-DATA.
000459
000460     IF MAINTI EQUAL 'S'
000461        GO TO 3000-SHOW-CLAIM-PAYMENT.
000462
000463     IF NOT  MODIFY-CAP
000464        MOVE 'UPDATE'           TO  SM-READ
000465        PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT
000466        MOVE ER-0070            TO  EMI-ERROR
000467        MOVE -1                 TO MAINTL
000468        PERFORM 9900-ERROR-FORMAT
000469        GO TO 8100-SEND-INITIAL-MAP.
000470
000471     IF MAINTI EQUAL 'A'
000472        GO TO 1000-APPROVE-PAYMENT.
000473
000474     IF MAINTI EQUAL 'V'
000475        GO TO 2000-VOID-PAYMENT.
000476
000477     MOVE  ER-0023            TO EMI-ERROR
000478     MOVE -1                  TO MAINTL
000479     MOVE AL-UABON            TO MAINTA
000480     PERFORM 9900-ERROR-FORMAT
000481     GO TO 8200-SEND-DATAONLY.
000482
000483     EJECT
000484 0500-CREATE-TEMP-STORAGE.
000485
000486     
      * EXEC CICS WRITEQ TS
000487*        QUEUE    (WS-QID)
000488*        FROM     (PROGRAM-INTERFACE-BLOCK)
000489*        LENGTH   (PI-COMM-LENGTH)
000490*    END-EXEC.
      *    MOVE '*"     L              ''   #00005624' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035363234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000491
000492 0599-EXIT.
000493      EXIT.
000494
000495     EJECT
000496 0600-RECOVER-TEMP-STORAGE.
000497
000498     MOVE LOW-VALUES                   TO EL143AI.
000499     
      * EXEC CICS HANDLE CONDITION
000500*        NOTFND  (0100-SEND-NEW)
000501*        QIDERR  (0100-SEND-NEW)
000502*    END-EXEC.
      *    MOVE '"$IN                  ! # #00005637' TO DFHEIV0
           MOVE X'2224494E2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303035363337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000503
000504
000505     
      * EXEC CICS READQ TS
000506*         QUEUE    (WS-QID)
000507*         INTO    (PROGRAM-INTERFACE-BLOCK)
000508*         LENGTH  (PI-COMM-LENGTH)
000509*        END-EXEC.
      *    MOVE '*$I    L              ''   #00005643' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035363433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000510
000511     
      * EXEC CICS DELETEQ TS
000512*         QUEUE    (WS-QID)
000513*    END-EXEC.
      *    MOVE '*&                    #   #00005649' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035363439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000514
000515     MOVE PI-LAST-ELTRLR-KEY     TO ELACTQ-KEY.
000516     MOVE ELACTQ-CLAIM-NO        TO CLAIMO.
000517     MOVE ELACTQ-CARRIER         TO CARRO.
000518     MOVE ELACTQ-CERT-PRIME      TO CERTO.
000519     MOVE ELACTQ-CERT-SFX        TO SUFFIXO.
000520
000521 0699-EXIT.
000522     EXIT.
000523
000524     EJECT
000525 1000-APPROVE-PAYMENT.
000526
000527     MOVE CARRI         TO WS-HOLD-CARR.
000528     MOVE CLAIMI        TO WS-HOLD-CLAIM.
000529     MOVE CERTI         TO WS-HOLD-CERT-PRIME.
000530     MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
000531     MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
000532
000533     IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
000534        MOVE ER-0138     TO EMI-ERROR
000535        MOVE -1          TO MAINTL
000536        MOVE AL-UNBON    TO MAINTA
000537        PERFORM 9900-ERROR-FORMAT
000538        GO TO 8200-SEND-DATAONLY.
000539
000540     MOVE PI-LAST-ELACTQ-KEY    TO ELACTQ-KEY.
000541
000542     
      * EXEC CICS HANDLE CONDITION
000543*         NOTFND    (1010-NOT-FOUND)
000544*         ENDFILE   (1010-NOT-FOUND)
000545*    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005680' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303035363830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000546
000547     
      * EXEC CICS READ
000548*         DATASET    (ELACTQ-FILE-ID)
000549*         RIDFLD     (ELACTQ-KEY)
000550*         SET        (ADDRESS OF ACTIVITY-QUE)
000551*         UPDATE
000552*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005685' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035363835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000553
000554     IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC OR
000555        AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
000556         MOVE -1                     TO  MAINTL
000557         MOVE  ER-0627               TO  EMI-ERROR
000558         PERFORM 9900-ERROR-FORMAT
000559         GO TO 8200-SEND-DATAONLY.
000560
000561     MOVE ELACTQ-KEY          TO ELTRLR-KEY.
000562     MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.
000563
000564     
      * EXEC CICS READ
000565*         DATASET   (ELTRLR-FILE-ID)
000566*         RIDFLD    (ELTRLR-KEY)
000567*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
000568*         UPDATE
000569*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005702' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035373032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000570
000571     IF AT-PAYMENT-LAST-UPDATED-BY EQUAL PI-ELTRLR-UPDATE-BY AND
000572        AT-LAST-MAINT-HHMMSS EQUAL PI-ELTRLR-UPDATE-HHMMSS
000573        NEXT SENTENCE
000574     ELSE
000575        MOVE ER-0068        TO EMI-ERROR
000576        MOVE AL-UABON       TO MAINTA
000577        MOVE -1             TO MAINTL
000578        PERFORM 9900-ERROR-FORMAT
000579        GO TO 8200-SEND-DATAONLY.
000580
000581     IF (AT-TRAILER-TYPE NOT EQUAL '2')
000582            OR
000583        (AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U')
000584        GO TO 1010-NOT-FOUND.
000585
000586     IF (AT-RECORDED-BY EQUAL PI-PROCESSOR-ID)
000587      AND
000588        (PI-PROCESSOR-ID NOT EQUAL 'LGXX')
000589        MOVE ER-0629       TO EMI-ERROR
000590        MOVE AL-UABON      TO MAINTA
000591        MOVE -1            TO MAINTL
000592        PERFORM 9900-ERROR-FORMAT
000593        GO TO 8200-SEND-DATAONLY.
000594
000595     IF NOT WS-GRADUATED-APPROVAL
000596         GO TO 1005-APPROVE.
000597
000598* If user approval level > trailer, make equal.
000599* This eliminates the need for intermediate approvals.
000600     IF AT-APPROVED-LEVEL < WS-APPROVAL-LEVEL
000601        MOVE WS-APPROVAL-LEVEL TO AT-APPROVED-LEVEL
000602     END-IF.
000603
000604     IF AT-APPROVED-LEVEL = WS-APPROVAL-LEVEL
000605         NEXT SENTENCE
000606       ELSE
000607         MOVE ER-2779       TO EMI-ERROR
000608         MOVE AL-UABON      TO MAINTA
000609         MOVE -1            TO MAINTL
000610         PERFORM 9900-ERROR-FORMAT
000611         GO TO 8200-SEND-DATAONLY.
000612
000613     IF AT-APPROVED-LEVEL = '1'
000614         MOVE '2'             TO AT-APPROVED-LEVEL
000615      ELSE
000616     IF AT-APPROVED-LEVEL = '2'
000617         MOVE '3'             TO AT-APPROVED-LEVEL
000618      ELSE
000619     IF AT-APPROVED-LEVEL = '3'
000620*         MOVE '4'             TO AT-APPROVED-LEVEL.
000621         MOVE '4'             TO AT-APPROVED-LEVEL
000622      ELSE
000623     IF AT-APPROVED-LEVEL = '4'
000624         MOVE '5'             TO AT-APPROVED-LEVEL
000625      ELSE
000626     IF AT-APPROVED-LEVEL = '5'
000627         MOVE '6'             TO AT-APPROVED-LEVEL.
000628
000629     IF AT-APPROVED-LEVEL GREATER AT-APPROVAL-LEVEL-REQD
000630         GO TO 1005-APPROVE.
000631
000632     MOVE PI-PROCESSOR-ID  TO AT-PAYMENT-LAST-UPDATED-BY.
000633     MOVE SAVE-BIN-DATE    TO AT-PAYMENT-LAST-MAINT-DT.
000634     MOVE EIBTIME          TO AT-LAST-MAINT-HHMMSS.
000635
000636     
      * EXEC CICS REWRITE
000637*        DATASET (ELTRLR-FILE-ID)
000638*        FROM    (ACTIVITY-TRAILERS)
000639*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005774' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035373734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000640
000641     MOVE ER-3342          TO EMI-ERROR
000642
000643     GO TO 1006-INTERMEDIATE-APPROVAL.
000644
000645 1005-APPROVE.
000646     MOVE 'A'              TO AT-PAYMENT-APPROVAL-SW.
000647
000648     MOVE PI-PROCESSOR-ID  TO AT-PAYMENT-LAST-UPDATED-BY.
000649     MOVE PI-PROCESSOR-ID  TO AT-PMT-APPROVED-BY.
000650     MOVE SAVE-BIN-DATE    TO AT-PAYMENT-LAST-MAINT-DT.
000651     MOVE EIBTIME          TO AT-LAST-MAINT-HHMMSS.
000652
000653     
      * EXEC CICS REWRITE
000654*         DATASET   (ELTRLR-FILE-ID)
000655*         FROM      (ACTIVITY-TRAILERS)
000656*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005791' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035373931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000657
000658     IF AQ-PMT-UNAPPROVED-COUNT GREATER THAN +0
000659        SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT.
000660
000661     
      * EXEC CICS REWRITE
000662*         DATASET   (ELACTQ-FILE-ID)
000663*         FROM      (ACTIVITY-QUE)
000664*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005799' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035373939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000665
000666     SUBTRACT +1    FROM PI-UNAPPROVED-COUNT.
000667     MOVE PI-UNAPPROVED-COUNT    TO UCOUNTO.
000668     MOVE ER-3343                TO  EMI-ERROR.
000669
000670 1006-INTERMEDIATE-APPROVAL.
000671     MOVE -1                     TO MAINTL.
000672     MOVE SPACES                 TO MAINTO.
000673     MOVE AL-UANOF               TO MAINTA.
000674     PERFORM 9900-ERROR-FORMAT
000675     GO TO 8200-SEND-DATAONLY.
000676
000677     EJECT
000678 1010-NOT-FOUND.
000679
000680     MOVE  ER-0142               TO EMI-ERROR.
000681     MOVE AL-UNBON               TO CLAIMA
000682                                    CERTA
000683                                    CARRA.
000684     MOVE -1                     TO CARRL.
000685
000686     PERFORM 9900-ERROR-FORMAT.
000687     GO TO 8200-SEND-DATAONLY.
000688
000689     EJECT
000690 2000-VOID-PAYMENT.
000691
000692     MOVE CARRI         TO WS-HOLD-CARR.
000693     MOVE CLAIMI        TO WS-HOLD-CLAIM.
000694     MOVE CERTI         TO WS-HOLD-CERT-PRIME.
000695     MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
000696     MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
000697
000698     IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
000699        MOVE ER-0138     TO EMI-ERROR
000700        MOVE -1          TO MAINTL
000701        MOVE AL-UNBON    TO MAINTA
000702        PERFORM 9900-ERROR-FORMAT
000703        GO TO 8200-SEND-DATAONLY.
000704
000705     MOVE PI-LAST-ELACTQ-KEY    TO ELACTQ-KEY.
000706
000707     
      * EXEC CICS HANDLE CONDITION
000708*         NOTFND    (2095-NOT-FOUND)
000709*         ENDFILE   (2095-NOT-FOUND)
000710*    END-EXEC.
      *    MOVE '"$I''                  ! % #00005845' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303035383435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000711
000712     
      * EXEC CICS READ
000713*         DATASET    (ELACTQ-FILE-ID)
000714*         RIDFLD     (ELACTQ-KEY)
000715*         SET        (ADDRESS OF ACTIVITY-QUE)
000716*    END-EXEC.
      *    MOVE '&"S        E          (   #00005850' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035383530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000717
000718     MOVE AQ-CONTROL-PRIMARY   TO ELMSTR-KEY.
000719
000720     
      * EXEC CICS READ
000721*         DATASET    (ELMSTR-FILE-ID)
000722*         RIDFLD     (ELMSTR-KEY)
000723*         SET        (ADDRESS OF CLAIM-MASTER)
000724*         UPDATE
000725*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005858' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035383538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000726
000727     MOVE CL-CONTROL-PRIMARY   TO ELTRLR-KEY.
000728     MOVE PI-LAST-TRLR-SEQ-NO  TO ELTRLR-SEQ-NO.
000729
000730     
      * EXEC CICS READ
000731*         DATASET    (ELTRLR-FILE-ID)
000732*         RIDFLD     (ELTRLR-KEY)
000733*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
000734*         UPDATE
000735*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005868' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035383638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000736
000737     IF AT-PAYMENT-LAST-UPDATED-BY EQUAL PI-ELTRLR-UPDATE-BY AND
000738        AT-LAST-MAINT-HHMMSS EQUAL PI-ELTRLR-UPDATE-HHMMSS
000739        NEXT SENTENCE
000740     ELSE
000741        MOVE ER-0068        TO EMI-ERROR
000742        MOVE AL-UABON       TO MAINTA
000743        MOVE -1             TO MAINTL
000744        PERFORM 9900-ERROR-FORMAT
000745        GO TO 8200-SEND-DATAONLY.
000746
000747     IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'
000748        GO TO 2095-NOT-FOUND.
000749
000750     IF (AT-RECORDED-BY EQUAL PI-PROCESSOR-ID)
000751      AND
000752        (PI-PROCESSOR-ID NOT EQUAL 'LGXX')
000753        MOVE ER-0629       TO EMI-ERROR
000754        MOVE AL-UABON      TO MAINTA
000755        MOVE -1            TO MAINTL
000756        PERFORM 9900-ERROR-FORMAT
000757        GO TO 8200-SEND-DATAONLY.
000758
000759     MOVE '7'                  TO PI-PAY-TYPE.
000760     MOVE AT-PAYMENT-TYPE      TO WS-PAY-TYPE.
000761     MOVE AT-AMOUNT-PAID       TO WS-AMOUNT-PAID.
000762     MOVE AT-CV-PMT-CODE       TO WS-CV-PMT-CODE.
000763
000764     IF (AT-PAYMENT-TYPE NOT EQUAL '5' AND '6')
000765         SUBTRACT AT-AMOUNT-PAID     FROM CL-TOTAL-PAID-AMT
000766         SUBTRACT AT-DAYS-IN-PERIOD  FROM CL-NO-OF-DAYS-PAID
000767         IF (AT-PAYMENT-TYPE NOT EQUAL '4')
000768            SUBTRACT +1                 FROM CL-NO-OF-PMTS-MADE
000769            IF AT-PAID-THRU-DT NOT EQUAL CL-PAID-THRU-DT OR
000770               AT-RECORDED-BY EQUAL 'ZZZZ'
000771               NEXT SENTENCE
000772            ELSE
000773               MOVE AT-PREV-LAST-PMT-DT    TO CL-LAST-PMT-DT
000774               MOVE AT-PREV-PAID-THRU-DT   TO CL-PAID-THRU-DT
000775               MOVE AT-PREV-LAST-PMT-AMT   TO CL-LAST-PMT-AMT.
000776
000777     IF CL-TOTAL-PAID-AMT LESS THAN +0
000778        MOVE +0                  TO CL-TOTAL-PAID-AMT.
000779
000780     IF CL-NO-OF-DAYS-PAID LESS THAN +0
000781        MOVE +0                  TO CL-NO-OF-DAYS-PAID.
000782
000783     IF CL-NO-OF-PMTS-MADE LESS THAN +0
000784        MOVE +0                  TO CL-NO-OF-PMTS-MADE.
000785
000786     MOVE SAVE-BIN-DATE          TO AT-VOID-DT
000787                                    CL-LAST-REOPEN-DT.
000788
000789     MOVE 'PAYMENT DISAPPROVED'  TO AT-VOID-REASON.
000790     MOVE 'V'                    TO AT-PAYMENT-APPROVAL-SW.
000791
000792     MOVE LOW-VALUES             TO AT-PMT-SELECT-DT
000793                                    AT-VOID-SELECT-DT.
000794
000795     MOVE PI-PROCESSOR-ID        TO AT-PAYMENT-LAST-UPDATED-BY.
000796     MOVE SAVE-BIN-DATE          TO AT-PAYMENT-LAST-MAINT-DT.
000797     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
000798
000799     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' or 'FNL'
000800         PERFORM 9870-OUTPUT-ACTIVITY-RECORD THRU
000801                 9870-EXIT
000802         IF ERROR-ON-OUTPUT
000803             MOVE -1             TO PFKEYL
000804             MOVE AL-UANON       TO PFKEYA
000805*            MOVE MAP-NAMEA      TO MAP-NAME
000806             PERFORM 9900-ERROR-FORMAT THRU
000807                     9995-EXIT
000808             GO TO 8200-SEND-DATAONLY
000809         END-IF
000810     END-IF.
000811
000812     move at-amount-paid         to ws-at-amount-paid
000813     move at-days-in-period      to ws-at-days-in-period
000814     move at-payment-type        to ws-at-payment-type
000815
000816     
      * EXEC CICS REWRITE
000817*         DATASET  (ELTRLR-FILE-ID)
000818*         FROM     (ACTIVITY-TRAILERS)
000819*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005954' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035393534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000820
000821 2010-UPDATE-ZERO-TRAILER.
000822
000823     MOVE CL-CONTROL-PRIMARY    TO ELTRLR-KEY.
000824     MOVE +0                    TO ELTRLR-SEQ-NO.
000825
000826     
      * EXEC CICS READ
000827*         DATASET    (ELTRLR-FILE-ID)
000828*         RIDFLD     (ELTRLR-KEY)
000829*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
000830*         UPDATE
000831*    END-EXEC.
      *    MOVE '&"S        EU         (   #00005964' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035393634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000832
000833     IF WS-PAY-TYPE EQUAL '5'
000834        SUBTRACT WS-AMOUNT-PAID  FROM AT-ITD-CHARGEABLE-EXPENSE.
000835
000836     IF WS-PAY-TYPE EQUAL '6'
000837        SUBTRACT WS-AMOUNT-PAID  FROM AT-ITD-PAID-EXPENSES.
000838
000839     IF AT-INITIAL-MANUAL-RESERVE NOT EQUAL +0
000840        ADD WS-AMOUNT-PAID       TO AT-CURRENT-MANUAL-RESERVE.
000841
000842 2010-CHECK-OPEN-CLOSE.
000843
000844     IF (PI-PAY-TYPE IS EQUAL TO '5' OR '6')
000845         GO TO 2010-REWRITE-ZERO-TRAILER.
000846
000847     IF (PI-PAY-TYPE IS EQUAL TO '1' OR '4' OR '7') AND
000848        CLAIM-IS-OPEN
000849         GO TO 2010-REWRITE-ZERO-TRAILER.
000850
000851     IF (PI-PAY-TYPE IS EQUAL TO '2' OR '3') AND
000852        CLAIM-IS-CLOSED
000853         GO TO 2010-REWRITE-ZERO-TRAILER.
000854
000855     MOVE +1                     TO  SUB-1.
000856
000857 2010-OPEN-CLOSE-LOOP.
000858
000859     IF AT-OPEN-CLOSE-TYPE (SUB-1) IS EQUAL TO SPACES
000860         MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB-1)
000861         IF (PI-PAY-TYPE IS EQUAL TO '1' OR '4' OR '7')
000862             MOVE 'O'            TO  AT-OPEN-CLOSE-TYPE (SUB-1)
000863             MOVE 'FORCE'        TO  AT-OPEN-CLOSE-REASON (SUB-1)
000864             GO TO 2010-REWRITE-ZERO-TRAILER
000865         ELSE
000866             MOVE 'C'            TO  AT-OPEN-CLOSE-TYPE   (SUB-1)
000867             MOVE 'FINAL'        TO  AT-OPEN-CLOSE-REASON (SUB-1)
000868             GO TO 2010-REWRITE-ZERO-TRAILER.
000869
000870     IF SUB-1 IS EQUAL TO 6
000871       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)
000872       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)
000873       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)
000874       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)
000875       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)
000876       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)
000877       GO TO 2010-OPEN-CLOSE-LOOP.
000878
000879     ADD +1                      TO  SUB-1.
000880     GO TO 2010-OPEN-CLOSE-LOOP.
000881
000882 2010-REWRITE-ZERO-TRAILER.
000883
000884     MOVE PI-PROCESSOR-ID        TO AT-RESERVES-LAST-UPDATED-BY.
000885     MOVE SAVE-BIN-DATE          TO AT-RESERVES-LAST-MAINT-DT.
000886     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
000887
000888     
      * EXEC CICS REWRITE
000889*         DATASET  (ELTRLR-FILE-ID)
000890*         FROM     (ACTIVITY-TRAILERS)
000891*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006026' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036303236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000892
000893 2020-UPDATE-ELCERT.
000894
000895     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
000896         GO TO 2050-UPDATE-EMPLCY.
000897
000898     MOVE PI-COMPANY-CD    TO ELCERT-COMPANY-CD.
000899     MOVE CL-CERT-CARRIER  TO ELCERT-CARRIER.
000900     MOVE CL-CERT-GROUPING TO ELCERT-GROUP.
000901     MOVE CL-CERT-STATE    TO ELCERT-STATE.
000902     MOVE CL-CERT-ACCOUNT  TO ELCERT-ACCOUNT.
000903     MOVE CL-CERT-EFF-DT   TO ELCERT-EFF-DATE.
000904     MOVE CL-CERT-NO       TO ELCERT-CERT-NO.
000905
000906     
      * EXEC CICS READ
000907*         DATASET     (ELCERT-FILE-ID)
000908*         RIDFLD      (ELCERT-KEY)
000909*         SET         (ADDRESS OF CERTIFICATE-MASTER)
000910*         UPDATE
000911*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006044' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036303434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000912
000913     move cm-ah-benefit-amt      to ws-cm-ah-benefit-amt
000914     move cm-ah-orig-term        to ws-cm-ah-orig-term
000915
000916     IF CL-CLAIM-TYPE NOT EQUAL PI-LIFE-OVERRIDE-L1 AND 'O'
000917        GO TO 2020-AH-VOID.
000918
000919     MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
000920     MOVE WS-ACCESS              TO  ELCNTL-ACCESS.
000921     MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
000922     MOVE '4'                    TO  ELCNTL-REC-TYPE.
000923     MOVE ZEROS                  TO  ELCNTL-SEQ-NO.
000924     PERFORM 7500-FIND-BENEFIT THRU 7500-EXIT.
000925     IF NO-BENEFIT-FOUND
000926         GO TO 8500-NOT-FOUND.
000927     MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.
000928
000929     IF PI-LIFE-OVERRIDE-L1 IS EQUAL TO 'P' OR
000930        WS-LF-COVERAGE-TYPE IS EQUAL TO 'P'
000931         IF WS-PAY-TYPE IS EQUAL TO '4'
000932             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
000933             IF CM-LF-CURRENT-STATUS IS EQUAL TO '1' OR '2'
000934                 GO TO 2020-REWRITE-CERT
000935             ELSE
000936                 MOVE CM-LF-STATUS-AT-DEATH
000937                                  TO CM-LF-CURRENT-STATUS
000938                 MOVE SPACES      TO CM-LF-STATUS-AT-DEATH
000939                 MOVE LOW-VALUES  TO CM-LF-DEATH-EXIT-DT
000940                                     CM-LF-DEATH-DT
000941                 GO TO 2020-REWRITE-CERT.
000942
000943     IF WS-PAY-TYPE EQUAL '4'
000944        SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
000945        GO TO 2020-REWRITE-CERT.
000946
000947     IF WS-PAY-TYPE EQUAL '2'
000948        IF CM-LF-CURRENT-STATUS IS EQUAL TO '1' OR '2'
000949            SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
000950            GO TO 2020-REWRITE-CERT
000951        ELSE
000952            MOVE CM-LF-STATUS-AT-DEATH TO CM-LF-CURRENT-STATUS
000953            MOVE SPACES        TO CM-LF-STATUS-AT-DEATH
000954            SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
000955            MOVE LOW-VALUES  TO CM-LF-DEATH-EXIT-DT CM-LF-DEATH-DT
000956            GO TO 2020-REWRITE-CERT
000957     ELSE
000958         GO TO 2030-UNLOCK-CERT.
000959
000960 2020-AH-VOID.
000961
000962     IF WS-PAY-TYPE EQUAL '4'
000963        SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
000964        GO TO 2020-REWRITE-CERT.
000965
000966     IF WS-PAY-TYPE EQUAL '3'
000967        MOVE CM-AH-STATUS-AT-SETTLEMENT TO CM-AH-CURRENT-STATUS
000968        MOVE SPACES        TO CM-AH-STATUS-AT-SETTLEMENT
000969        SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
000970        MOVE LOW-VALUES TO CM-AH-SETTLEMENT-DT
000971                           CM-AH-SETTLEMENT-EXIT-DT
000972        GO TO 2020-REWRITE-CERT
000973     ELSE
000974        GO TO 2030-UNLOCK-CERT.
000975
000976 2020-REWRITE-CERT.
000977
000978     
      * EXEC CICS REWRITE
000979*         DATASET  (ELCERT-FILE-ID)
000980*         FROM     (CERTIFICATE-MASTER)
000981*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006116' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036313136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000982
000983     GO TO 2090-UPDATE-ELACTQ.
000984
000985 2030-UNLOCK-CERT.
000986
000987     
      * EXEC CICS UNLOCK
000988*         DATASET  (ELCERT-FILE-ID)
000989*    END-EXEC.
      *    MOVE '&*                    #   #00006125' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036313235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000990
000991     GO TO 2090-UPDATE-ELACTQ.
000992
000993     EJECT
000994 2050-UPDATE-EMPLCY.
000995
000996     MOVE PI-COMPANY-CD          TO  EMPLCY-COMPANY-CD.
000997     MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER.
000998     MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.
000999     MOVE CL-CERT-STATE          TO  EMPLCY-STATE.
001000     MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.
001001     MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DATE.
001002     MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO.
001003
001004     
      * EXEC CICS READ
001005*        DATASET   (EMPLCY-FILE-ID)
001006*        RIDFLD    (EMPLCY-KEY)
001007*        SET       (ADDRESS OF POLICY-MASTER)
001008*    END-EXEC.
      *    MOVE '&"S        E          (   #00006142' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036313432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPLCY-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001009
001010     MOVE LOW-VALUES             TO WS-POLICY-UPDATE-WORKING-GRPS.
001011     MOVE PM-COMPANY-CD          TO  WS-COMPANY-CD.
001012     MOVE PM-CARRIER             TO  WS-CARRIER.
001013     MOVE PM-GROUPING            TO  WS-GROUPING.
001014     MOVE PM-STATE               TO  WS-STATE.
001015     MOVE PM-PRODUCER            TO  WS-PRODUCER.
001016     MOVE PM-POLICY-EFF-DT       TO  WS-POLICY-EFF-DT.
001017     MOVE PM-REFERENCE-NUMBER    TO  WS-REFERENCE-NUMBER.
001018
001019     MOVE 'RW'                   TO  WS-EMPLCY-FUNCTION.
001020     MOVE PI-PROCESSOR-ID        TO  WS-LAST-CHANGE-PROCESSOR.
001021     MOVE SAVE-BIN-DATE          TO  WS-LAST-CHANGE-DT.
001022     MOVE EIBTIME                TO  WS-LAST-CHANGE-TIME.
001023
001024     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F' OR 'B' OR 'H'
001025         GO TO 2050-UPDATE-AH-POLICY-DATA.
001026
001027 2050-UPDATE-LF-POLICY-DATA.
001028
001029     IF WS-PAY-TYPE IS EQUAL TO '2'
001030*** FROM AT-PAYMENT-TYPE - TYPES 1-6 IN CONVENIENCE VALUES
001031       IF (WS-CV-PMT-CODE IS EQUAL TO '1' OR '2' OR '3' OR '4')
001032*** LIFE / HALF LIFE / ADD / HALF ADD - CONV VALUES
001033         COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
001034                                         WS-AMOUNT-PAID
001035         COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -
001036                                     WS-AMOUNT-PAID
001037         COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
001038
001039     IF WS-PAY-TYPE IS EQUAL TO '2'
001040       IF (WS-CV-PMT-CODE IS EQUAL TO '5' OR '6')
001041*** RIDER AND HALF RIDER - CONV VALUES
001042         COMPUTE WS-CLAIM-RIDER-ITD = PM-CLAIM-RIDER-ITD -
001043                                      WS-AMOUNT-PAID.
001044
001045     IF WS-PAY-TYPE IS EQUAL TO '4'
001046*** ADDITIONAL PAYMENT - TYPE 8 CONV VALUES
001047         COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
001048                                         WS-AMOUNT-PAID
001049         COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -
001050                                     WS-AMOUNT-PAID
001051         COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
001052
001053     IF WS-PAY-TYPE = '6'
001054*** NON CHARGEABLE EXPENSE - TYPE 7 IN CONVENIENCE
001055         COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -
001056                                         WS-AMOUNT-PAID.
001057
001058     IF  PM-CLAIM-SETTLEMENT
001059
001060         IF  WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'
001061             MOVE '6'            TO WS-CURRENT-STATUS
001062
001063             IF  PM-EXIT-DT GREATER THAN LOW-VALUES
001064                 MOVE HIGH-VALUES
001065                                 TO WS-EXIT-DT
001066             END-IF.
001067
001068     GO TO 2050-UPDATE-CLAIM-HISTORY.
001069
001070 2050-UPDATE-AH-POLICY-DATA.
001071
001072     IF (WS-PAY-TYPE IS EQUAL TO '1' OR '4')
001073         COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
001074                                         WS-AMOUNT-PAID
001075         COMPUTE WS-CLAIM-AH-ITD = PM-CLAIM-AH-ITD -
001076                                   WS-AMOUNT-PAID
001077         COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
001078
001079     IF WS-PAY-TYPE = '6'
001080         COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -
001081                                         WS-AMOUNT-PAID.
001082
001083     IF  PM-CLAIM-SETTLEMENT
001084
001085         IF  WS-CV-PMT-CODE = '2'
001086             MOVE '6'            TO WS-CURRENT-STATUS
001087
001088         ELSE
001089             IF  WS-CV-PMT-CODE = '1'
001090                     AND
001091                 WS-CLAIM-PAYMENTS-ITD LESS THAN
001092                     PM-INS-TOTAL-BENEFIT
001093                 MOVE '6'        TO WS-CURRENT-STATUS.
001094
001095 2050-UPDATE-CLAIM-HISTORY.
001096
001097     IF PM-CLAIM-ATTACH-CNT IS EQUAL TO +1 OR
001098        PM-CLAIM-INCURRED-DT IS EQUAL TO CL-INCURRED-DT
001099         NEXT SENTENCE
001100     ELSE
001101         GO TO 2050-FINISH-POLICY-UPDATE.
001102
001103     IF (PM-CLAIM-ATTACH-CNT IS EQUAL TO +1) AND
001104        (CL-NO-OF-PMTS-MADE IS EQUAL TO +0)
001105         OR
001106        (PM-CLAIM-PAYMENT-CNT IS EQUAL TO +0)
001107         MOVE +0                 TO  WS-CLAIM-LAST-PAYMENT-AMT
001108         MOVE '1'                TO  WS-CLAIM-INTERFACE-SW
001109         MOVE HIGH-VALUES        TO  WS-CLAIM-INCURRED-DT
001110                                     WS-CLAIM-PAID-TO-DT
001111     ELSE
001112         MOVE CL-PAID-THRU-DT    TO  WS-CLAIM-PAID-TO-DT
001113         MOVE CL-LAST-PMT-AMT    TO  WS-CLAIM-LAST-PAYMENT-AMT.
001114
001115 2050-FINISH-POLICY-UPDATE.
001116
001117     IF WS-CLAIM-PAYMENT-CNT IS NEGATIVE
001118         MOVE +0                 TO  WS-CLAIM-PAYMENT-CNT.
001119
001120     
      * EXEC CICS LINK
001121*        PROGRAM    ('EMPLCY')
001122*        COMMAREA   (WS-POLICY-MASTER-UPDATE-AREA)
001123*        LENGTH     (WS-PM-COMM-LNGTH)
001124*    END-EXEC.
           MOVE 'EMPLCY' TO DFHEIV1
      *    MOVE '."C                   (   #00006258' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036323538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-POLICY-MASTER-UPDATE-AREA, 
                 WS-PM-COMM-LNGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001125
001126     IF WS-EMPLCY-RETURN-CODE IS EQUAL TO LOW-VALUES
001127         NEXT SENTENCE
001128     ELSE
001129         MOVE ER-9211            TO  EMI-ERROR
001130         MOVE -1                 TO  MAINTO
001131         MOVE AL-UABON           TO  MAINTA
001132         PERFORM 9900-ERROR-FORMAT
001133         
      * EXEC CICS SYNCPOINT
001134*            ROLLBACK
001135*        END-EXEC
      *    MOVE '6"R                   !   #00006271' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303036323731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001136         GO TO 8200-SEND-DATAONLY.
001137
001138 2090-UPDATE-ELACTQ.
001139
001140     
      * EXEC CICS READ
001141*         DATASET    (ELACTQ-FILE-ID)
001142*         RIDFLD     (ELACTQ-KEY)
001143*         SET        (ADDRESS OF ACTIVITY-QUE)
001144*         UPDATE
001145*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006278' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036323738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001146
001147     IF AQ-PAYMENT-COUNTER IS NOT NUMERIC
001148         MOVE +0                 TO AQ-PAYMENT-COUNTER.
001149
001150     IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC
001151         MOVE +0                 TO  AQ-PMT-UNAPPROVED-COUNT.
001152
001153     IF AQ-PMT-UNAPPROVED-COUNT GREATER THAN +0
001154        SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT.
001155
001156     SUBTRACT +1    FROM PI-UNAPPROVED-COUNT.
001157     MOVE PI-UNAPPROVED-COUNT    TO UCOUNTO.
001158
001159     IF AQ-PAYMENT-COUNTER IS GREATER THAN +0
001160         SUBTRACT +1 FROM AQ-PAYMENT-COUNTER.
001161
001162     IF AQ-PAYMENT-COUNTER IS EQUAL TO +0
001163         MOVE SPACE               TO AQ-PENDING-PAYMENT-FLAG.
001164
001165     IF AQ-PENDING-ACTIVITY-FLAGS IS EQUAL TO SPACES
001166         
      * EXEC CICS DELETE
001167*            DATASET   (ELACTQ-FILE-ID)
001168*        END-EXEC
      *    MOVE '&(                    &   #00006304' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303036333034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001169     ELSE
001170         
      * EXEC CICS REWRITE
001171*            DATASET  (ELACTQ-FILE-ID)
001172*            FROM     (ACTIVITY-QUE)
001173*        END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006308' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036333038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001174
001175 2090-REWRITE-ELMSTR.
001176
001177     IF (WS-PAY-TYPE IS EQUAL TO '4' OR '5' OR '6')
001178         GO TO 2092-CONT-REWRITE.
001179
001180     IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
001181         IF CL-NO-OF-PMTS-MADE IS GREATER THAN +0
001182             GO TO 2092-CONT-REWRITE.
001183
001184     MOVE 'O'                    TO  CL-CLAIM-STATUS.
001185
001186 2092-CONT-REWRITE.
001187
001188     if ws-at-payment-type not = '5' and '6' and 'I'
001189        perform 2100-upd-cert-trlr thru 2100-exit
001190     end-if
001191
001192     
      * EXEC CICS REWRITE
001193*         DATASET  (ELMSTR-FILE-ID)
001194*         FROM     (CLAIM-MASTER)
001195*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006330' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036333330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001196
001197     MOVE ZEROS                  TO EMI-ERROR
001198     MOVE -1                     TO MAINTL.
001199     MOVE SPACES                 TO MAINTO.
001200     MOVE AL-UANOF               TO MAINTA.
001201     PERFORM 9900-ERROR-FORMAT
001202     GO TO 8200-SEND-DATAONLY.
001203
001204 2095-NOT-FOUND.
001205
001206     MOVE  ER-0142               TO EMI-ERROR.
001207     MOVE AL-UNBON               TO CLAIMA
001208                                    CERTA.
001209     MOVE -1                     TO CARRL.
001210     MOVE AL-UNBON               TO CARRA.
001211
001212     PERFORM 9900-ERROR-FORMAT.
001213     GO TO 8200-SEND-DATAONLY.
001214
001215 2100-UPD-CERT-TRLR.
001216
001217     MOVE CL-COMPANY-CD          TO CTRLR-COMP-CD
001218     MOVE CL-CERT-KEY-DATA       TO ELCRTT-KEY (2:21)
001219     MOVE CL-CERT-NO             TO CTRLR-CERT-NO
001220     MOVE 'B'                    TO CTRLR-REC-TYPE
001221
001222     
      * EXEC CICS READ
001223*       UPDATE
001224*       DATASET   ('ELCRTT')
001225*       RIDFLD    (ELCRTT-KEY)
001226*       set       (address of CERTIFICATE-TRAILERS)
001227*       RESP      (WS-RESPONSE)
001228*    END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00006360' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303036333630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001229
001230     IF RESP-NORMAL
001231        perform varying s1 from +1 by +1 until
001232           (s1 > +24)
001233           or (cl-claim-no = cs-claim-no (s1))
001234        end-perform
001235
001236     if s1 < +25
001237        subtract ws-at-amount-paid from cs-total-paid (s1)
001238        if cs-total-paid (s1) < zeros
001239           move zeros            to cs-total-paid (s1)
001240        end-if
001241        subtract at-days-in-period from cs-days-paid (s1)
001242        if cs-days-paid (s1) < zeros
001243           move zeros            to cs-days-paid (s1)
001244        end-if
001245        if cl-claim-type not = 'L' and 'P'
001246           perform 2110-calc-rem-bens
001247                                 thru 2110-exit
001248        end-if
001249        
      * exec cics rewrite
001250*          dataset    ('ELCRTT')
001251*          from       (certificate-trailers)
001252*          resp       (ws-response)
001253*       end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00006387' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303036333837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001254     end-if
001255
001256     .
001257 2100-EXIT.
001258     EXIT.
001259
001260 2110-calc-rem-bens.
001261
001262     move cm-ah-orig-term        to ws-max-bens
001263     if cl-critical-period not = zeros and spaces
001264        move cl-critical-period  to ws-max-bens
001265     end-if
001266
001267     move zeros to ws-tot-days-paid ws-tot-amt-paid
001268     perform varying s2 from +1 by +1 until
001269        (s2 > +24)
001270        or (cs-claim-no (s2) = spaces)
001271        if (cs-benefit-period (s2) = cl-benefit-period)
001272           and (cs-insured-type (s2) = cl-insured-type)
001273           and (cs-claim-type (s2) = cl-claim-type)
001274           compute ws-tot-days-paid =
001275              ws-tot-days-paid + cs-days-paid (s2)
001276           compute ws-tot-amt-paid =
001277              ws-tot-amt-paid + cs-total-paid (s2)
001278        end-if
001279     end-perform
001280     compute cs-remaining-bens (s1) =
001281        ws-max-bens / cm-ah-benefit-amt
001282     if cs-remaining-bens (s1) < zeros
001283        move zeros            to cs-remaining-bens (s1)
001284     end-if
001285
001286     .
001287 2110-exit.
001288     exit.
001289
001290 3000-SHOW-CLAIM-PAYMENT.
001291
001292     IF MAINTI EQUAL 'S'
001293        IF CLAIML GREATER THAN +0 AND
001294           CERTL  GREATER THAN +0
001295           NEXT SENTENCE
001296        ELSE
001297           MOVE ER-0005     TO EMI-ERROR
001298           MOVE -1          TO MAINTL
001299           MOVE AL-UNBON    TO MAINTA
001300           PERFORM 9900-ERROR-FORMAT
001301           GO TO 8200-SEND-DATAONLY.
001302
001303     IF MAINTI EQUAL 'S'
001304        MOVE SPACES              TO ELACTQ-KEY
001305        MOVE PI-COMPANY-CD       TO ELACTQ-COMPANY-CD
001306        MOVE CARRI               TO ELACTQ-CARRIER
001307        MOVE CLAIMI              TO ELACTQ-CLAIM-NO
001308        MOVE CERTI               TO ELACTQ-CERT-PRIME
001309        MOVE SUFFIXI             TO ELACTQ-CERT-SFX.
001310
001311     
      * EXEC CICS HANDLE CONDITION
001312*         NOTFND     (3100-NOT-FOUND)
001313*         ENDFILE    (3100-NOT-FOUND)
001314*    END-EXEC.
      *    MOVE '"$I''                  ! & #00006449' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303036343439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001315
001316     
      * EXEC CICS READ
001317*         DATASET     (ELACTQ-FILE-ID)
001318*         RIDFLD      (ELACTQ-KEY)
001319*         SET         (ADDRESS OF ACTIVITY-QUE)
001320*    END-EXEC.
      *    MOVE '&"S        E          (   #00006454' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036343534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001321
001322     IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
001323        AQ-PAYMENT-COUNTER IS EQUAL TO +0
001324         GO TO 3100-NOT-FOUND.
001325
001326     IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC  OR
001327        AQ-PMT-UNAPPROVED-COUNT EQUAL +0
001328          GO TO 3100-NOT-FOUND.
001329
001330     MOVE AQ-CONTROL-PRIMARY           TO PI-LAST-ELACTQ-KEY
001331                                          ELMSTR-KEY
001332                                          ELTRLR-KEY.
001333
001334     IF MAINTI EQUAL 'S'
001335        MOVE +90                          TO ELTRLR-SEQ-NO
001336        MOVE SPACES                       TO  PI-DIAGNOSIS
001337     ELSE
001338        MOVE PI-LAST-TRLR-SEQ-NO          TO ELTRLR-SEQ-NO.
001339
001340     
      * EXEC CICS READ
001341*         DATASET    (ELMSTR-FILE-ID)
001342*         RIDFLD     (ELMSTR-KEY)
001343*         SET        (ADDRESS OF CLAIM-MASTER)
001344*    END-EXEC.
      *    MOVE '&"S        E          (   #00006478' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036343738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001345
001346     MOVE CL-CARRIER                 TO  PI-CARRIER.
001347     MOVE CL-CLAIM-NO                TO  PI-CLAIM-NO.
001348     MOVE CL-CERT-NO                 TO  PI-CERT-NO.
001349     MOVE CL-CERT-GROUPING           TO  PI-GROUPING.
001350     MOVE CL-CERT-STATE              TO  PI-STATE.
001351     MOVE CL-CERT-ACCOUNT            TO  PI-ACCOUNT.
001352     MOVE CL-CERT-EFF-DT             TO  PI-CERT-EFF-DT.
001353
001354
001355 3010-READ-NEXT-ELTRLR.
001356
001357     
      * EXEC CICS READ
001358*         DATASET    (ELTRLR-FILE-ID)
001359*         RIDFLD     (ELTRLR-KEY)
001360*         SET        (ADDRESS OF ACTIVITY-TRAILERS)
001361*         GTEQ
001362*    END-EXEC.
      *    MOVE '&"S        G          (   #00006495' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303036343935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001363
001364     MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.
001365     IF WS-HOLD-ELTRLR-KEY NOT EQUAL CL-CONTROL-PRIMARY
001366        GO TO 3100-NOT-FOUND.
001367
001368     IF AT-TRAILER-TYPE EQUAL '6'
001369        IF AT-SEQUENCE-NO EQUAL +90
001370           MOVE AT-INFO-LINE-1   TO  PI-DIAGNOSIS.
001371
001372     IF (AT-TRAILER-TYPE NOT EQUAL '2')
001373             OR
001374        (AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U')
001375        MOVE AT-CONTROL-PRIMARY TO ELTRLR-KEY
001376        ADD +1 TO ELTRLR-SEQ-NO
001377        GO TO 3010-READ-NEXT-ELTRLR.
001378
001379     MOVE LOW-VALUES                   TO EL143AI.
001380
001381     MOVE PI-DIAGNOSIS          TO DIAGNO.
001382     MOVE CL-CARRIER            TO CARRO.
001383     MOVE CL-CLAIM-NO           TO CLAIMO.
001384     MOVE CL-CERT-PRIME         TO CERTO.
001385     MOVE CL-CERT-SFX           TO SUFFIXO.
001386
001387     IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC
001388         MOVE +0                       TO  PI-UNAPPROVED-COUNT
001389                                           UCOUNTO
001390     ELSE
001391         MOVE AQ-PMT-UNAPPROVED-COUNT  TO  PI-UNAPPROVED-COUNT
001392                                           UCOUNTO.
001393
001394
001395     EVALUATE TRUE
001396
001397     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
001398        MOVE PI-AH-OVERRIDE-L6   TO TYPEO
001399
001400     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
001401        MOVE PI-LIFE-OVERRIDE-L6 TO TYPEO
001402
001403     WHEN CL-CLAIM-TYPE = 'I'
001404        MOVE '  IU  '            TO TYPEO
001405
001406     WHEN CL-CLAIM-TYPE = 'G'
001407        MOVE ' GAP  '            TO TYPEO
001408
001409     WHEN CL-CLAIM-TYPE = 'F'
001410        MOVE ' FAM  '            TO TYPEO
001411     WHEN CL-CLAIM-TYPE = 'B'
001412        MOVE ' BRV  '            TO TYPEO
001413     WHEN CL-CLAIM-TYPE = 'H'
001414        MOVE ' HSP '             TO TYPEO
001415
001416     WHEN CL-CLAIM-TYPE = 'O'
001417        MOVE ' OTH  '            TO TYPEO
001418
001419     END-EVALUATE
001420
001421     IF CL-CLAIM-STATUS  EQUAL 'O'
001422        MOVE ' OPEN '           TO STATUSO
001423     ELSE
001424        MOVE 'CLOSED'           TO STATUSO.
001425
001426     MOVE CL-INCURRED-DT        TO DC-BIN-DATE-1.
001427     MOVE ' '                   TO DC-OPTION-CODE.
001428     PERFORM 9700-DATE-LINK.
001429     IF NO-CONVERSION-ERROR
001430        MOVE DC-GREG-DATE-1-EDIT     TO INCDTEO.
001431
001432     MOVE CL-REPORTED-DT        TO DC-BIN-DATE-1.
001433     MOVE ' '                   TO DC-OPTION-CODE.
001434     PERFORM 9700-DATE-LINK.
001435     IF NO-CONVERSION-ERROR
001436        MOVE DC-GREG-DATE-1-EDIT     TO REPDTEO.
001437
001438     MOVE CL-PAID-THRU-DT       TO DC-BIN-DATE-1.
001439     MOVE ' '                   TO DC-OPTION-CODE.
001440     PERFORM 9700-DATE-LINK.
001441
001442     IF NO-CONVERSION-ERROR
001443        MOVE DC-GREG-DATE-1-EDIT     TO PAYTHRO
001444        IF PI-USES-PAID-TO
001445           MOVE CL-PAID-THRU-DT      TO DC-BIN-DATE-1
001446           MOVE '6'                  TO DC-OPTION-CODE
001447           MOVE +1                   TO DC-ELAPSED-DAYS
001448           MOVE +0                   TO DC-ELAPSED-MONTHS
001449           PERFORM 9700-DATE-LINK
001450           IF NO-CONVERSION-ERROR
001451              MOVE DC-GREG-DATE-1-EDIT     TO PAYTHRO.
001452
001453     MOVE CL-LAST-PMT-DT        TO DC-BIN-DATE-1.
001454     MOVE ' '                   TO DC-OPTION-CODE.
001455     PERFORM 9700-DATE-LINK
001456     IF NO-CONVERSION-ERROR
001457        MOVE DC-GREG-DATE-1-EDIT     TO LSTPAIDO.
001458
001459     IF CL-NO-OF-PMTS-MADE NUMERIC
001460        MOVE CL-NO-OF-PMTS-MADE      TO NOPMTSO
001461     ELSE
001462        MOVE ZEROS                   TO NOPMTSO.
001463
001464     MOVE CL-NO-OF-DAYS-PAID         TO DAYPAIDO.
001465
001466     MOVE CL-INSURED-LAST-NAME       TO LNAMEO.
001467     MOVE CL-PROCESSOR-ID            TO PROCO.
001468
001469     IF AT-PAYMENT-TYPE EQUAL '1'
001470        MOVE 'PARTIAL'               TO PMTTYPO
001471     ELSE
001472     IF AT-PAYMENT-TYPE EQUAL '2'
001473        MOVE 'FINAL'                 TO PMTTYPO
001474     ELSE
001475     IF AT-PAYMENT-TYPE EQUAL '3'
001476        MOVE 'SETTLEMENT'            TO PMTTYPO
001477     ELSE
001478     IF AT-PAYMENT-TYPE EQUAL '4'
001479        MOVE 'ADDITIONAL'            TO PMTTYPO
001480     ELSE
001481     IF AT-PAYMENT-TYPE EQUAL '5'
001482        MOVE 'CHG EXP'               TO PMTTYPO
001483     ELSE
001484     IF AT-PAYMENT-TYPE EQUAL '6'
001485        MOVE 'N-CHG EXP'             TO PMTTYPO
001486     ELSE
001487        MOVE AT-PAYMENT-TYPE         TO PMTTYPO.
001488
001489     MOVE AT-CHECK-NO                TO CHECKO.
001490     MOVE AT-AMOUNT-PAID             TO AMTPDO.
001491
001492     MOVE AT-PMT-SELECT-DT           TO DC-BIN-DATE-1.
001493     MOVE ' '                        TO DC-OPTION-CODE.
001494     PERFORM 9700-DATE-LINK.
001495     IF NO-CONVERSION-ERROR
001496        MOVE DC-GREG-DATE-1-EDIT     TO SELDTEO.
001497
001498     MOVE AT-PAID-FROM-DT            TO DC-BIN-DATE-1.
001499     MOVE ' '                        TO DC-OPTION-CODE.
001500     PERFORM 9700-DATE-LINK.
001501     IF NO-CONVERSION-ERROR
001502        MOVE DC-GREG-DATE-1-EDIT     TO PDFROMO.
001503
001504     MOVE AT-PAID-THRU-DT            TO DC-BIN-DATE-1.
001505     MOVE ' '                        TO DC-OPTION-CODE.
001506     PERFORM 9700-DATE-LINK.
001507     IF NO-CONVERSION-ERROR
001508        MOVE DC-GREG-DATE-1-EDIT     TO PDTHRUO
001509        IF PI-USES-PAID-TO
001510           MOVE AT-PAID-THRU-DT            TO DC-BIN-DATE-1
001511           MOVE '6'                        TO DC-OPTION-CODE
001512           MOVE +1                         TO DC-ELAPSED-DAYS
001513           MOVE +0                         TO DC-ELAPSED-MONTHS
001514           PERFORM 9700-DATE-LINK
001515           IF NO-CONVERSION-ERROR
001516              MOVE DC-GREG-DATE-1-EDIT     TO PDTHRUO.
001517
001518     MOVE AT-VOID-REASON             TO COMMENTO.
001519     MOVE AT-RECORDED-BY             TO PAIDBYO.
001520     MOVE AT-SEQUENCE-NO             TO SEQO.
001521
001522     MOVE AT-APPROVAL-LEVEL-REQD     TO AREQO.
001523     MOVE AT-APPROVED-LEVEL          TO ALEVO.
001524
001525     IF AT-PAYEE-TYPE EQUAL 'I'
001526        MOVE 'INSURED   '            TO PDTOO
001527     ELSE
001528     IF AT-PAYEE-TYPE EQUAL 'B'
001529        MOVE 'BENEFICARY'            TO PDTOO
001530     ELSE
001531     IF AT-PAYEE-TYPE EQUAL 'A'
001532        MOVE 'ACCOUNT   '            TO PDTOO
001533     ELSE
001534     IF AT-PAYEE-TYPE EQUAL 'O'
001535        MOVE 'OTHER-1   '            TO PDTOO
001536     ELSE
001537     IF AT-PAYEE-TYPE EQUAL 'Q'
001538        MOVE 'OTHER-2   '            TO PDTOO
001539     ELSE
001540     IF AT-PAYEE-TYPE EQUAL 'P'
001541        MOVE 'PHYSICIAN '            TO PDTOO
001542     ELSE
001543        MOVE AT-PAYEE-TYPE-CD        TO PDTOO.
001544
001545     MOVE AT-RECORDED-DT             TO DC-BIN-DATE-1.
001546     MOVE ' '                        TO DC-OPTION-CODE.
001547     PERFORM 9700-DATE-LINK.
001548     IF NO-CONVERSION-ERROR
001549        MOVE DC-GREG-DATE-1-EDIT     TO RECDTEO.
001550
001551     IF AT-FORCE-CONTROL EQUAL '1'
001552        MOVE 'Y'                     TO FORCEO
001553     ELSE
001554        MOVE 'N'                     TO FORCEO.
001555
001556     MOVE AT-SEQUENCE-NO              TO PI-LAST-TRLR-SEQ-NO.
001557     MOVE AT-PAYMENT-LAST-UPDATED-BY  TO PI-ELTRLR-UPDATE-BY.
001558     MOVE AT-LAST-MAINT-HHMMSS        TO PI-ELTRLR-UPDATE-HHMMSS.
001559     MOVE AT-CONTROL-PRIMARY          TO  PI-LAST-ELTRLR-KEY.
001560
001561     GO TO 8100-SEND-INITIAL-MAP.
001562
001563 3100-NOT-FOUND.
001564
001565     IF EIBAID EQUAL DFHPF1
001566        MOVE WS-HOLD-KEY TO ELACTQ-KEY
001567        GO TO 7005-CONTINUE-BROWSE.
001568
001569     IF EIBAID EQUAL DFHPF2
001570        MOVE WS-HOLD-KEY TO ELACTQ-KEY
001571        GO TO 7105-CONTINUE-BROWSE.
001572
001573     MOVE  ER-0142               TO EMI-ERROR.
001574     MOVE -1                     TO CARRL.
001575     MOVE AL-UNBON               TO CARRA
001576                                    CLAIMA
001577                                    CERTA.
001578     PERFORM 9900-ERROR-FORMAT.
001579     GO TO 8100-SEND-INITIAL-MAP.
001580
001581     EJECT
001582******************************************************************
001583*      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *
001584*      ACTIVITY QUE FILE.  WHEN AN ACTIVITY QUE RECORD WITH AN   *
001585*      UNAPPROVED PAYMENT COUNTER GREATER THAN +0 IS FOUND THE   *
001586*      FIRST UNAPPROVED PAYMENT FOR THAT CLAIM IS DISPLAYED.     *
001587******************************************************************
001588 7000-BROWSE-FWRD-NEXT-CLAIM.
001589
001590     MOVE PI-COMPANY-CD     TO ELACTQ-COMPANY-CD.
001591     MOVE CARRI             TO ELACTQ-CARRIER.
001592     MOVE CLAIMI            TO ELACTQ-CLAIM-NO.
001593     MOVE CERTI             TO ELACTQ-CERT-PRIME.
001594     MOVE SUFFIXI           TO ELACTQ-CERT-SFX.
001595
001596 7005-CONTINUE-BROWSE.
001597
001598     
      * EXEC CICS HANDLE CONDITION
001599*         NOTFND    (7030-END-FILE)
001600*         ENDFILE   (7030-END-FILE)
001601*    END-EXEC.
      *    MOVE '"$I''                  ! '' #00006736' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303036373336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001602
001603     
      * EXEC CICS STARTBR
001604*         DATASET   (ELACTQ-FILE-ID)
001605*         RIDFLD    (ELACTQ-KEY)
001606*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006741' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036373431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001607
001608     MOVE 'Y' TO WS-BROWSE-SW.
001609
001610     MOVE ELACTQ-KEY     TO  WS-HOLD-KEY.
001611
001612 7010-READ-NEXT-ELACTQ.
001613
001614     
      * EXEC CICS READNEXT
001615*         DATASET   (ELACTQ-FILE-ID)
001616*         RIDFLD    (ELACTQ-KEY)
001617*         SET       (ADDRESS OF ACTIVITY-QUE)
001618*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006752' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036373532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001619
001620     IF ELACTQ-KEY EQUAL WS-HOLD-KEY
001621        GO TO 7010-READ-NEXT-ELACTQ.
001622
001623     IF AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD
001624        GO TO 7030-END-FILE.
001625
001626     IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
001627        AQ-PAYMENT-COUNTER IS EQUAL TO +0
001628         GO TO 7010-READ-NEXT-ELACTQ.
001629
001630     IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC OR
001631        AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
001632         GO TO 7010-READ-NEXT-ELACTQ.
001633
001634     MOVE ELACTQ-KEY     TO  WS-HOLD-KEY.
001635
001636     MOVE AQ-CONTROL-PRIMARY  TO ELACTQ-KEY.
001637     MOVE +1                  TO PI-LAST-TRLR-SEQ-NO.
001638
001639     IF WS-BROWSE-SW EQUAL 'Y'
001640         PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
001641
001642     GO TO 3000-SHOW-CLAIM-PAYMENT.
001643
001644 7030-END-FILE.
001645
001646     IF WS-BROWSE-SW EQUAL 'Y'
001647         PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
001648
001649     MOVE -1                     TO MAINTL.
001650     MOVE  ER-2237               TO EMI-ERROR.
001651     PERFORM 9900-ERROR-FORMAT.
001652     GO TO 8200-SEND-DATAONLY.
001653
001654     EJECT
001655******************************************************************
001656*      THIS ROUTINE BROWSES BACKWARD SEQUENTIALLY THROUGH THE    *
001657*      ACTIVITY QUE FILE.  WHEN AN ACTIVITY QUE RECORD WITH AN   *
001658*      UNAPPROVED PAYMENT COUNTER GREATER THAN +0 IS FOUND THE   *
001659*      FIRST UNAPPROVED PAYMENT FOR THAT CLAIM IS DISPLAYED.     *
001660******************************************************************
001661 7100-BROWSE-BWRD-NEXT-CLAIM.
001662
001663     MOVE PI-COMPANY-CD     TO ELACTQ-COMPANY-CD.
001664     MOVE CARRI             TO ELACTQ-CARRIER.
001665     MOVE CLAIMI            TO ELACTQ-CLAIM-NO.
001666     MOVE CERTI             TO ELACTQ-CERT-PRIME.
001667     MOVE SUFFIXI           TO ELACTQ-CERT-SFX.
001668
001669 7105-CONTINUE-BROWSE.
001670
001671     
      * EXEC CICS HANDLE CONDITION
001672*         NOTFND    (7130-END-FILE)
001673*         ENDFILE   (7130-END-FILE)
001674*    END-EXEC.
      *    MOVE '"$I''                  ! ( #00006809' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303036383039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001675
001676     
      * EXEC CICS STARTBR
001677*         DATASET   (ELACTQ-FILE-ID)
001678*         RIDFLD    (ELACTQ-KEY)
001679*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006814' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036383134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001680
001681     MOVE 'Y'     TO WS-BROWSE-SW.
001682
001683 7110-READ-NEXT-ELACTQ.
001684
001685     
      * EXEC CICS READNEXT
001686*         DATASET   (ELACTQ-FILE-ID)
001687*         RIDFLD    (ELACTQ-KEY)
001688*         SET       (ADDRESS OF ACTIVITY-QUE)
001689*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006823' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001690
001691     
      * EXEC CICS READPREV
001692*         DATASET   (ELACTQ-FILE-ID)
001693*         RIDFLD    (ELACTQ-KEY)
001694*         SET       (ADDRESS OF ACTIVITY-QUE)
001695*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006829' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036383239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001696
001697 7120-READ-PREV-ELACTQ.
001698
001699     
      * EXEC CICS READPREV
001700*         DATASET   (ELACTQ-FILE-ID)
001701*         RIDFLD    (ELACTQ-KEY)
001702*         SET       (ADDRESS OF ACTIVITY-QUE)
001703*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006837' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036383337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001704
001705     IF AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD
001706        GO TO 7130-END-FILE.
001707
001708     MOVE ELACTQ-KEY             TO  WS-HOLD-KEY.
001709
001710     IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
001711        AQ-PAYMENT-COUNTER IS EQUAL TO +0
001712         GO TO 7120-READ-PREV-ELACTQ.
001713
001714     IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC OR
001715        AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
001716         GO TO 7120-READ-PREV-ELACTQ.
001717
001718     MOVE AQ-CONTROL-PRIMARY  TO ELACTQ-KEY.
001719     MOVE +1                  TO PI-LAST-TRLR-SEQ-NO.
001720
001721     IF WS-BROWSE-SW EQUAL 'Y'
001722        MOVE ' ' TO WS-BROWSE-SW
001723        PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
001724
001725     GO TO 3000-SHOW-CLAIM-PAYMENT.
001726
001727 7130-END-FILE.
001728
001729     IF WS-BROWSE-SW EQUAL 'Y'
001730        MOVE ' ' TO WS-BROWSE-SW
001731        PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
001732
001733     MOVE -1                     TO MAINTL.
001734     MOVE  ER-2238               TO EMI-ERROR.
001735     PERFORM 9900-ERROR-FORMAT.
001736     GO TO 8200-SEND-DATAONLY.
001737
001738     EJECT
001739******************************************************************
001740*      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *
001741*      ACTIVITY TRAILER FILE SEARCHING FOR AND DISPLAYING        *
001742*      UNAPPROVED PAYMENT TRAILER DATA ASSOCIATED WITH A         *
001743*      PARTICULAR CLAIM.                                         *
001744******************************************************************
001745 7200-BROWSE-FWRD-NEXT-PAYMENT.
001746
001747     MOVE CARRI         TO WS-HOLD-CARR.
001748     MOVE CLAIMI        TO WS-HOLD-CLAIM.
001749     MOVE CERTI         TO WS-HOLD-CERT-PRIME.
001750     MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
001751     MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
001752
001753     IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
001754        MOVE ER-0628     TO EMI-ERROR
001755        MOVE -1          TO MAINTL
001756        MOVE AL-UNBON    TO MAINTA
001757        PERFORM 9900-ERROR-FORMAT
001758        GO TO 8200-SEND-DATAONLY.
001759
001760     MOVE PI-LAST-ELACTQ-KEY  TO ELTRLR-KEY.
001761     MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.
001762     ADD +1   TO ELTRLR-SEQ-NO.
001763
001764     
      * EXEC CICS HANDLE CONDITION
001765*         NOTFND    (7230-END-FILE)
001766*         ENDFILE   (7230-END-FILE)
001767*    END-EXEC.
      *    MOVE '"$I''                  ! ) #00006902' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303036393032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001768
001769 7210-READ-NEXT-ELTRLR.
001770
001771     
      * EXEC CICS READ
001772*         DATASET   (ELTRLR-FILE-ID)
001773*         RIDFLD    (ELTRLR-KEY)
001774*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001775*         GTEQ
001776*    END-EXEC.
      *    MOVE '&"S        G          (   #00006909' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303036393039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001777
001778     MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.
001779     IF WS-HOLD-ELTRLR-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
001780        GO TO 7230-END-FILE.
001781
001782     IF AT-TRAILER-TYPE NOT EQUAL '2'
001783        ADD +1 TO ELTRLR-SEQ-NO
001784        GO TO 7210-READ-NEXT-ELTRLR.
001785
001786     IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'
001787        ADD +1 TO ELTRLR-SEQ-NO
001788        GO TO 7210-READ-NEXT-ELTRLR.
001789
001790     MOVE AT-CONTROL-PRIMARY  TO ELACTQ-KEY.
001791     MOVE AT-SEQUENCE-NO      TO PI-LAST-TRLR-SEQ-NO.
001792
001793     GO TO 3000-SHOW-CLAIM-PAYMENT.
001794
001795 7230-END-FILE.
001796
001797     MOVE -1                     TO MAINTL.
001798     MOVE  ER-0303               TO EMI-ERROR.
001799     PERFORM 9900-ERROR-FORMAT.
001800     GO TO 8200-SEND-DATAONLY.
001801
001802     EJECT
001803******************************************************************
001804*      THIS ROUTINE BROWSES BACKWARD SEQUENTIALLY THROUGH THE    *
001805*      ACTIVITY TRAILER FILE SEARCHING FOR AND DISPLAYING        *
001806*      UNAPPROVED PAYMENT TRAILER DATA ASSOCIATED WITH A         *
001807*      PARTICULAR CLAIM.                                         *
001808******************************************************************
001809 7300-BROWSE-BWRD-NEXT-PAYMENT.
001810
001811     MOVE CARRI         TO WS-HOLD-CARR.
001812     MOVE CLAIMI        TO WS-HOLD-CLAIM.
001813     MOVE CERTI         TO WS-HOLD-CERT-PRIME.
001814     MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
001815     MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
001816
001817     IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
001818        MOVE ER-0628     TO EMI-ERROR
001819        MOVE -1          TO MAINTL
001820        MOVE AL-UNBON    TO MAINTA
001821        PERFORM 9900-ERROR-FORMAT
001822        GO TO 8200-SEND-DATAONLY.
001823
001824     MOVE PI-LAST-ELACTQ-KEY  TO ELTRLR-KEY.
001825     MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.
001826
001827     
      * EXEC CICS HANDLE CONDITION
001828*         NOTFND    (7330-END-FILE)
001829*         ENDFILE   (7330-END-FILE)
001830*    END-EXEC.
      *    MOVE '"$I''                  ! * #00006965' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303036393635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001831
001832     
      * EXEC CICS STARTBR
001833*         DATASET   (ELTRLR-FILE-ID)
001834*         RIDFLD    (ELTRLR-KEY)
001835*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006970' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036393730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001836
001837 7310-READ-NEXT-ELTRLR.
001838
001839     
      * EXEC CICS READNEXT
001840*         DATASET   (ELTRLR-FILE-ID)
001841*         RIDFLD    (ELTRLR-KEY)
001842*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001843*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006977' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036393737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001844
001845     
      * EXEC CICS READPREV
001846*         DATASET   (ELTRLR-FILE-ID)
001847*         RIDFLD    (ELTRLR-KEY)
001848*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001849*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006983' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036393833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001850
001851 7320-READ-PREV-ELTRLR.
001852
001853     
      * EXEC CICS READPREV
001854*         DATASET   (ELTRLR-FILE-ID)
001855*         RIDFLD    (ELTRLR-KEY)
001856*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
001857*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006991' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036393931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001858
001859     MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.
001860     IF WS-HOLD-ELTRLR-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
001861        GO TO 7330-END-FILE.
001862
001863     IF AT-TRAILER-TYPE NOT EQUAL '2'
001864        GO TO 7320-READ-PREV-ELTRLR.
001865
001866     IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'
001867        GO TO 7320-READ-PREV-ELTRLR.
001868
001869     MOVE AT-CONTROL-PRIMARY  TO ELACTQ-KEY.
001870     MOVE AT-SEQUENCE-NO      TO PI-LAST-TRLR-SEQ-NO.
001871
001872     GO TO 3000-SHOW-CLAIM-PAYMENT.
001873
001874 7330-END-FILE.
001875
001876     MOVE -1                     TO MAINTL.
001877     MOVE  ER-0303               TO EMI-ERROR.
001878     PERFORM 9900-ERROR-FORMAT.
001879     GO TO 8200-SEND-DATAONLY.
001880
001881     EJECT
001882 7500-FIND-BENEFIT.
001883
001884     
      * EXEC CICS HANDLE CONDITION
001885*        ENDFILE   (7500-EXIT)
001886*        NOTFND    (7500-EXIT)
001887*    END-EXEC.
      *    MOVE '"$''I                  ! + #00007022' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303037303232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001888
001889     
      * EXEC CICS READ
001890*        DATASET   ('ELCNTL')
001891*        RIDFLD    (ELCNTL-KEY)
001892*        SET       (ADDRESS OF CONTROL-FILE)
001893*        GTEQ
001894*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00007027' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303037303237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001895
001896     IF ELCNTL-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR
001897        ELCNTL-REC-TYPE   IS NOT EQUAL TO CF-RECORD-TYPE
001898         GO TO 7500-EXIT.
001899
001900     PERFORM 7500-BENEFIT-DUMMY THRU 7500-DUMMY-EXIT
001901         VARYING SUB FROM 1 BY 1 UNTIL
001902             ((SUB IS GREATER THAN 8) OR
001903             (CF-BENEFIT-CODE (SUB) IS EQUAL TO WS-BEN-CD)).
001904
001905     IF SUB IS NOT EQUAL TO 9
001906         MOVE 'Y'                TO  WS-BEN-SEARCH-SW.
001907
001908     GO TO 7500-EXIT.
001909
001910 7500-BENEFIT-DUMMY.
001911 7500-DUMMY-EXIT.
001912     EXIT.
001913
001914 7500-EXIT.
001915     EXIT.
001916     EJECT
001917 7600-READ-COMPANY-REC.
001918     
      * EXEC CICS HANDLE CONDITION
001919*        ENDFILE   (7600-EXIT)
001920*        NOTFND    (7600-EXIT)
001921*    END-EXEC.
      *    MOVE '"$''I                  ! , #00007056' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303037303536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001922
001923     MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
001924     MOVE '1'                    TO  ELCNTL-REC-TYPE.
001925     MOVE SPACES                 TO  ELCNTL-ACCESS.
001926     MOVE +0                     TO  ELCNTL-SEQ-NO.
001927
001928     
      * EXEC CICS READ
001929*        DATASET   ('ELCNTL')
001930*        RIDFLD    (ELCNTL-KEY)
001931*        SET       (ADDRESS OF CONTROL-FILE)
001932*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007066' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037303636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001933
001934     MOVE CF-PAYMENT-APPROVAL-SW TO WS-PAYMENT-APPROVAL-SW.
001935
001936 7600-EXIT.
001937     EXIT.
001938
001939 7700-READ-USER-REC.
001940     IF PI-PROCESSOR-ID = 'LGXX'
001941        GO TO 7700-EXIT.
001942
001943     
      * EXEC CICS HANDLE CONDITION
001944*        ENDFILE   (7700-EXIT)
001945*        NOTFND    (7700-EXIT)
001946*    END-EXEC.
      *    MOVE '"$''I                  ! - #00007081' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303037303831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001947
001948     MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
001949     MOVE PI-PROCESSOR-ID        TO  ELCNTL-ACCESS.
001950     MOVE '2'                    TO  ELCNTL-REC-TYPE.
001951
001952     
      * EXEC CICS READ
001953*        DATASET   ('ELCNTL')
001954*        RIDFLD    (ELCNTL-KEY)
001955*        SET       (ADDRESS OF CONTROL-FILE)
001956*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007090' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037303930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001957
001958     MOVE CF-APPROVAL-LEVEL TO WS-APPROVAL-LEVEL.
001959
001960 7700-EXIT.
001961     EXIT.
001962     EJECT
001963******************************************************************
001964*      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *
001965*      ACTIVITY QUE AND ACTIVITY TRAILER FILES SEARCHING FOR     *
001966*      AND DISPLAYING UNAPPROVED PAYMENT DATA RELATED TO A       *
001967*      USERS SPECIFIC APPROVAL LEVEL.                            *
001968******************************************************************
001969 8000-BROWSE-FWRD-NEXT-APPROVAL.
001970
001971     IF PI-UNAPPROVED-COUNT IS GREATER THAN +0
001972         MOVE PI-LAST-ELTRLR-KEY     TO  ELTRLR-KEY
001973         MOVE PI-LAST-ELACTQ-KEY     TO  ELACTQ-KEY
001974         SUBTRACT +1 FROM PI-UNAPPROVED-COUNT
001975         GO TO 8020-BROWSE-ACTIVITY-TRAILERS.
001976
001977     IF PI-FIRST-TIME-SW IS EQUAL TO 'Y'
001978         MOVE LOW-VALUES             TO  ELACTQ-KEY
001979         MOVE PI-COMPANY-CD          TO  ELACTQ-COMPANY-CD
001980         MOVE 'N'                    TO  PI-FIRST-TIME-SW
001981     ELSE
001982         MOVE PI-LAST-ELACTQ-KEY     TO  ELACTQ-KEY.
001983
001984     MOVE SPACES                     TO  PI-DIAGNOSIS.
001985
001986     
      * EXEC CICS HANDLE CONDITION
001987*        NOTFND    (8040-END-FILE)
001988*        ENDFILE   (8040-END-FILE)
001989*    END-EXEC.
      *    MOVE '"$I''                  ! . #00007124' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303037313234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001990
001991     
      * EXEC CICS STARTBR
001992*        DATASET   (ELACTQ-FILE-ID)
001993*        RIDFLD    (ELACTQ-KEY)
001994*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007129' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303037313239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001995
001996 8010-READNEXT.
001997     
      * EXEC CICS READNEXT
001998*        DATASET   (ELACTQ-FILE-ID)
001999*        RIDFLD    (ELACTQ-KEY)
002000*        SET       (ADDRESS OF ACTIVITY-QUE)
002001*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007135' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037313335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002002
002003     IF ELACTQ-KEY IS EQUAL TO PI-LAST-ELACTQ-KEY
002004         GO TO 8010-READNEXT.
002005
002006     IF AQ-COMPANY-CD IS NOT EQUAL TO PI-COMPANY-CD
002007         GO TO 8040-END-FILE.
002008
002009     MOVE ELACTQ-KEY             TO  PI-LAST-ELACTQ-KEY.
002010
002011     IF AQ-PENDING-PAYMENT-FLAG IS EQUAL TO '1'
002012         NEXT SENTENCE
002013     ELSE
002014         GO TO 8010-READNEXT.
002015
002016     IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
002017        AQ-PAYMENT-COUNTER IS EQUAL TO +0
002018         GO TO 8010-READNEXT.
002019
002020     IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC OR
002021        AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
002022         GO TO 8010-READNEXT.
002023
002024     MOVE AQ-PMT-UNAPPROVED-COUNT    TO  PI-UNAPPROVED-COUNT.
002025     MOVE ELACTQ-KEY                 TO  ELTRLR-KEY.
002026     MOVE +0                         TO  ELTRLR-SEQ-NO.
002027
002028     PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
002029
002030 8020-BROWSE-ACTIVITY-TRAILERS.
002031
002032     
      * EXEC CICS STARTBR
002033*        DATASET   (ELTRLR-FILE-ID)
002034*        RIDFLD    (ELTRLR-KEY)
002035*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007170' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303037313730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002036
002037 8030-READNEXT-ELTRLR.
002038
002039     
      * EXEC CICS HANDLE CONDITION
002040*        NOTFND    (8045-END-FILE)
002041*        ENDFILE   (8045-END-FILE)
002042*    END-EXEC.
      *    MOVE '"$I''                  ! / #00007177' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303037313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002043
002044     
      * EXEC CICS READNEXT
002045*        DATASET   (ELTRLR-FILE-ID)
002046*        RIDFLD    (ELTRLR-KEY)
002047*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
002048*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007182' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037313832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002049
002050     IF ELTRLR-COMPANY-CD IS NOT EQUAL TO PI-COMPANY-CD
002051         PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT
002052         GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
002053
002054     IF ELTRLR-CLAIM-NO IS NOT EQUAL TO ELACTQ-CLAIM-NO
002055         PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT
002056         GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
002057
002058     IF ELTRLR-CERT-NO IS NOT EQUAL TO ELACTQ-CERT-NO
002059         PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT
002060         GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
002061
002062     IF ELTRLR-KEY IS EQUAL TO PI-LAST-ELTRLR-KEY
002063         GO TO 8030-READNEXT-ELTRLR.
002064
002065     IF AT-TRAILER-TYPE EQUAL '6'
002066        IF AT-SEQUENCE-NO EQUAL +90
002067           MOVE AT-INFO-LINE-1   TO  PI-DIAGNOSIS.
002068
002069     IF AT-TRAILER-TYPE IS EQUAL TO '2'
002070         NEXT SENTENCE
002071     ELSE
002072         GO TO 8030-READNEXT-ELTRLR.
002073
002074     IF AT-PAYMENT-APPROVAL-SW IS NOT EQUAL TO 'U'
002075         GO TO 8030-READNEXT-ELTRLR.
002076
002077*01886      IF AT-APPROVED-LEVEL IS NOT EQUAL TO WS-APPROVAL-LEVEL
002078     IF AT-APPROVED-LEVEL IS NOT LESS THAN WS-APPROVAL-LEVEL
002079         GO TO 8030-READNEXT-ELTRLR.
002080
002081     MOVE AT-SEQUENCE-NO         TO  PI-LAST-TRLR-SEQ-NO.
002082     MOVE AT-CONTROL-PRIMARY     TO  PI-LAST-ELTRLR-KEY.
002083
002084     PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT.
002085
002086     GO TO 3000-SHOW-CLAIM-PAYMENT.
002087
002088 8040-END-FILE.
002089     MOVE -1                     TO MAINTL.
002090     MOVE  ER-0303               TO EMI-ERROR.
002091     PERFORM 9900-ERROR-FORMAT.
002092     GO TO 8200-SEND-DATAONLY.
002093
002094 8045-END-FILE.
002095
002096     PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT.
002097     GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
002098
002099     EJECT
002100 8050-ENDBR-ELACTQ.
002101
002102     
      * EXEC CICS ENDBR
002103*        DATASET   (ELACTQ-FILE-ID)
002104*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007240' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037323430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002105
002106 8050-EXIT.
002107     EXIT.
002108
002109 8060-ENDBR-ELTRLR.
002110
002111     
      * EXEC CICS ENDBR
002112*        DATASET   (ELTRLR-FILE-ID)
002113*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007249' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037323439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002114
002115 8060-EXIT.
002116     EXIT.
002117
002118     EJECT
002119 8100-SEND-INITIAL-MAP.
002120     MOVE SAVE-DATE              TO DATEO.
002121     MOVE EIBTIME                TO TIME-IN.
002122     MOVE TIME-OUT               TO TIMEO.
002123     MOVE LOW-VALUES             TO MAINTO.
002124     MOVE -1                     TO MAINTL.
002125     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
002126
002127 8150-SEND-INITIAL-MAP.
002128
002129     IF PI-USES-PAID-TO
002130        MOVE 'PAID  TO :'       TO PDTHHD1O
002131        MOVE 'PAID  TO       :' TO PDTHHD2O.
002132
002133     
      * EXEC CICS SEND
002134*        MAP      (MAP-NAME)
002135*        MAPSET   (MAPSET-NAME)
002136*        FROM     (EL143AO)
002137*        ERASE
002138*        CURSOR
002139*    END-EXEC.
           MOVE LENGTH OF
            EL143AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007271' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303037323731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL143AO, 
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
           
002140
002141     GO TO 9100-RETURN-TRAN.
002142
002143 8200-SEND-DATAONLY.
002144
002145     MOVE SAVE-DATE              TO DATEO.
002146     MOVE EIBTIME                TO TIME-IN.
002147     MOVE TIME-OUT               TO TIMEO.
002148     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
002149
002150     IF PI-USES-PAID-TO
002151        MOVE 'PAID  TO :'       TO PDTHHD1O
002152        MOVE 'PAID  TO       :' TO PDTHHD2O.
002153
002154     
      * EXEC CICS SEND
002155*        MAP      (MAP-NAME)
002156*        MAPSET   (MAPSET-NAME)
002157*        FROM     (EL143AO)
002158*        DATAONLY
002159*        CURSOR
002160*    END-EXEC.
           MOVE LENGTH OF
            EL143AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007292' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303037323932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL143AO, 
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
           
002161
002162     GO TO 9100-RETURN-TRAN.
002163
002164     EJECT
002165 8300-SEND-TEXT.
002166
002167     
      * EXEC CICS SEND TEXT
002168*        FROM     (LOGOFF-TEXT)
002169*        LENGTH   (LOGOFF-LENGTH)
002170*        ERASE
002171*        FREEKB
002172*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007305' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303037333035' TO DFHEIV0
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
           
002173
002174     
      * EXEC CICS RETURN
002175*    END-EXEC.
      *    MOVE '.(                    ''   #00007312' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037333132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002176
002177     EJECT
002178 8500-NOT-FOUND.
002179
002180     MOVE ER-0282                TO  EMI-ERROR.
002181     MOVE -1                     TO  MAINTL.
002182     PERFORM 9900-ERROR-FORMAT.
002183     GO TO 8200-SEND-DATAONLY.
002184
002185 8800-UNAUTHORIZED-ACCESS.
002186
002187     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
002188     GO TO 8300-SEND-TEXT.
002189
002190 8810-PF23.
002191
002192     MOVE EIBAID                 TO PI-ENTRY-CD-1.
002193     MOVE XCTL-005               TO PGM-NAME.
002194     GO TO 9300-XCTL.
002195
002196 9100-RETURN-TRAN.
002197
002198     MOVE EMI-ERROR-NUMBER (1)      TO PI-LAST-ERROR-NO.
002199     MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.
002200     
      * EXEC CICS RETURN
002201*        TRANSID    (TRANS-ID)
002202*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
002203*        LENGTH     (PI-COMM-LENGTH)
002204*    END-EXEC.
      *    MOVE '.(CT                  ''   #00007338' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037333338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002205
002206 9200-RETURN-MAIN-MENU.
002207
002208     MOVE XCTL-126               TO PGM-NAME.
002209     GO TO 9300-XCTL.
002210
002211 9300-XCTL.
002212
002213     
      * EXEC CICS XCTL
002214*        PROGRAM    (PGM-NAME)
002215*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
002216*        LENGTH     (PI-COMM-LENGTH)
002217*    END-EXEC.
      *    MOVE '.$C                   %   #00007351' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037333531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002218
002219 9400-CLEAR.
002220
002221     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
002222     GO TO 9300-XCTL.
002223
002224 9500-PF12.
002225
002226     MOVE XCTL-010               TO PGM-NAME.
002227     GO TO 9300-XCTL.
002228
002229 9600-PGMID-ERROR.
002230
002231     
      * EXEC CICS HANDLE CONDITION
002232*        PGMIDERR    (8300-SEND-TEXT)
002233*    END-EXEC.
      *    MOVE '"$L                   ! 0 #00007369' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303037333639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002234
002235     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
002236     MOVE ' '                    TO PI-ENTRY-CD-1.
002237     MOVE XCTL-005               TO PGM-NAME.
002238     MOVE PGM-NAME               TO LOGOFF-PGM.
002239     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
002240     GO TO 9300-XCTL.
002241
002242 9700-DATE-LINK.
002243
002244     MOVE LINK-ELDATCV           TO PGM-NAME.
002245     
      * EXEC CICS LINK
002246*        PROGRAM    (PGM-NAME)
002247*        COMMAREA   (DATE-CONVERSION-DATA)
002248*        LENGTH     (DC-COMM-LENGTH)
002249*    END-EXEC.
      *    MOVE '."C                   (   #00007383' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037333833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002250
002251 9870-OUTPUT-ACTIVITY-RECORD.
002252
002253     
      * EXEC CICS GETMAIN
002254*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
002255*        LENGTH (25)
002256*        INITIMG (WS-BLANK)
002257*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007391' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037333931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002258
002259     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
002260     MOVE ELMSTR-KEY             TO DA-KEY.
002261     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.
002262     IF WS-PAY-TYPE EQUAL '7'
002263         MOVE 'V'                TO DA-RECORD-TYPE
002264     ELSE
002265         MOVE 'P'                TO DA-RECORD-TYPE.
002266
002267     
      * EXEC CICS HANDLE CONDITION
002268*        NOTOPEN (9870-NOTOPEN)
002269*        DUPREC (9870-EXIT)
002270*    END-EXEC.
      *    MOVE '"$J%                  ! 1 #00007405' TO DFHEIV0
           MOVE X'22244A252020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303037343035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002271
002272     
      * EXEC CICS WRITE
002273*        DATASET ('DLYACTV')
002274*        RIDFLD (DA-KEY)
002275*        FROM (DAILY-ACTIVITY-RECORD)
002276*        LENGTH (25)
002277*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007410' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037343130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002278
002279     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.
002280     GO TO 9870-EXIT.
002281
002282 9870-NOTOPEN.
002283
002284     MOVE '2955'                 TO EMI-ERROR.
002285     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.
002286
002287 9870-EXIT.
002288     EXIT.
002289
002290 9900-ERROR-FORMAT.
002291
002292     IF NOT EMI-ERRORS-COMPLETE
002293         MOVE LINK-001           TO PGM-NAME
002294         
      * EXEC CICS LINK
002295*            PROGRAM    (PGM-NAME)
002296*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
002297*            LENGTH     (EMI-COMM-LENGTH)
002298*        END-EXEC.
      *    MOVE '."C                   (   #00007432' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037343332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002299
002300 9990-ABEND.
002301
002302     MOVE LINK-004               TO PGM-NAME.
002303     MOVE DFHEIBLK               TO EMI-LINE1.
002304
002305     
      * EXEC CICS LINK
002306*        PROGRAM   (PGM-NAME)
002307*        COMMAREA  (EMI-LINE1)
002308*        LENGTH    (72)
002309*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007443' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037343433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002310
002311     MOVE -1                     TO MAINTL.
002312     GO TO 8200-SEND-DATAONLY.
002313
002314 9995-SECURITY-VIOLATION.
002315*    COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00007471' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037343731' TO DFHEIV0
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
002316
002317 9995-EXIT.
002318      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL143' TO DFHEIV1
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
               GO TO 0100-SEND-NEW,
                     0100-SEND-NEW
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1010-NOT-FOUND,
                     1010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2095-NOT-FOUND,
                     2095-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3100-NOT-FOUND,
                     3100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7030-END-FILE,
                     7030-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7130-END-FILE,
                     7130-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7230-END-FILE,
                     7230-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7330-END-FILE,
                     7330-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7500-EXIT,
                     7500-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7600-EXIT,
                     7600-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7700-EXIT,
                     7700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8040-END-FILE,
                     8040-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8045-END-FILE,
                     8045-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 9870-NOTOPEN,
                     9870-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL143' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
