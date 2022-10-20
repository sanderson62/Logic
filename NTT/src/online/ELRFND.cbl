      *((program: ELRFND.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. ELRFND.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 03/05/96 16:23:50.
000007*                            VMOD=2.011
000008*
000009*AUTHOR.       LOGIC, INC.
000010*              DALLAS, TEXAS.
000011
000012*DATE-COMPILED.
000013
000014*SECURITY.   *****************************************************
000015*            *                                                   *
000016*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000017*            *                                                   *
000018*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000019*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000020*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000021*            *                                                   *
000022*            *****************************************************
000023
000024*REMARKS.    *****************************************************
000025*            *                                                   *
000026*            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
000027*            *    OPTION SPECIFIED, COMPUTE PREMIUM REFUNDS      *
000028*            *****************************************************
000029
000030
000031******************************************************************
000032*                                                                *
000033*              INPUT FIELDS USED                                 *
000034*                                                                *
000035******************************************************************
000036*  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
000037*  REFUND DATE      - CP-VALUATION-DT                            *
000038*  ORIGINAL TERM    - CP-ORIGINAL-TERM                           *
000039*  LOAN TERM        - CP-LOAN-TERM                               *
000040*  REMAINING TERM   - CP-REMAINING-TERM                          *
000041*  TERM OR EXT DAYS - CP-TERM-OR-EXT-DAYS                        *
000042*  STATE CODE       - CP-STATE                                   *
000043*  STATE CODE       - CP-STATE-STD-ABBV                          *
000044*  CLASS CODE       - CP-CLASS-CODE                              *
000045*  DEVIATION CODE   - CP-DEVIATION-CODE                          *
000046*  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
000047*  PROCESS TYPE     - CP-PROCESS-TYPE                            *
000048*  BENEFIT KIND     - CP-BENEFIT-TYPE                            *
000049*  A.P.R.           - CP-LOAN-APR                                *
000050*  METHOD           - CP-EARNING-METHOD                          *
000051*  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
000052*  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
000053*  COMPANY I.D.     - CP-COMPANY-ID                              *
000054*  BENEFIT CODE     - CP-BENEFIT-CD                              *
000055*  INSURED AGE      - CP-ISSUE-AGE                               *
000056******************************************************************
000057 ENVIRONMENT DIVISION.
000058
000059 DATA DIVISION.
000060     EJECT
000061 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000062 77  FILLER   PIC X(32) VALUE '********************************'.
000063 77  FILLER   PIC X(32) VALUE '**  ELRFND  WORKING STORAGE   **'.
000064 77  FILLER   PIC X(32) VALUE '********VMOD=2.011 *************'.
000065 77  WS-UE-CLP                   PIC S9(5)V99 COMP-3 VALUE +0.
000066 77  WS-GFR3                     PIC S9(5)V99 COMP-3 VALUE +0.
000067 77  WS-MONTH                    PIC S999     COMP-3 VALUE +0.
000068
000069 01  WS-WORK-MISC.
000070     12  WS-REMAINING-TERM   PIC S9(4)V9      COMP-3.
000071     12  WS-TEMP-RESULT      PIC S9(7)V9(6)   COMP-3.
000072     12  WS-REMAINING-AMT    PIC S9(7)V9(6)   COMP-3.
000073     12  WS-ORIG-PREM        PIC S9(7)V9(6)   COMP-3.
000074     12  WS-REMAIN-PREM      PIC S9(7)V9(6)   COMP-3.
000075     12  WS-NP-REF-FACTOR    PIC S9(4)V9(11)  COMP-3.
000076     12  WS-SAVE-TERM        PIC S9(3)        COMP-3.
000077     12  WS-SAVE-LOAN-TERM   PIC S9(3)        COMP-3.
000078     12  WS-SAVE-BENEFIT     PIC S9(9)V99     COMP-3.
000079     12  WS-SAVE-EARN-METHOD PIC X.
000080     12  ws-save-ext-days    pic s9(5)        comp-3.
000081     12  CAL-RFND            PIC S9(5)V99     COMP-3.
000082     12  FACTOR-1            PIC S9(4)V9(11)  COMP-3.
000083     12  FACTOR-2            PIC S9(4)V9(11)  COMP-3.
000084     12  FACTOR-3            PIC S9(4)V9(11)  COMP-3.
000085     12  FACTOR-4            PIC S9(4)V9(11)  COMP-3.
000086     12  FACTOR-5            PIC S9(7)        COMP-3.
000087     12  FACTOR-6            PIC S9(7)        COMP-3.
000088     12  RATE-FACTOR-1       PIC S9V99        COMP-3.
000089     12  RATE-FACTOR-2       PIC S999         COMP-3.
000090     12  R78-FACTOR          PIC S9(4)V9(11)  COMP-3.
000091     12  PR-FACTOR           PIC S9(4)V9(11)  COMP-3.
000092     12  RSUM-FACTOR         PIC S9(4)V9(11)  COMP-3.
000093     12  RSUM-REMAINING-TERM PIC S9(3)V99     COMP-3.
000094
000095 01  TEXAS-REG-WORK-AREAS.
000096     12  TEX-FACT-1          PIC S9(7)V9(2)  COMP-3.
000097     12  TEX-FACT-2          PIC S9(3)       COMP-3.
000098     12  TEX-FACT-3          PIC S9(3)       COMP-3.
000099     12  TEX-FACT-4          PIC S9(7)       COMP-3.
000100     12  TEX-FACT-5          PIC S9(3)       COMP-3.
000101     12  TEX-FACT-6          PIC S9(3)       COMP-3.
000102     12  TEX-FACT-7          PIC S9(7)       COMP-3.
000103     12  TEX-FACT-8          PIC S9V9(6)     COMP-3.
000104     12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.
000105
000106 01  NET-PAY-INTERFACE.
000107     12  N-P-APR             PIC S9(3)V9(4)  COMP-3.
000108     12  N-P-ORIG            PIC S9(3)       COMP-3.
000109     12  N-P-REM             PIC S9(3)       COMP-3.
000110     12  N-P-OPT             PIC X.
000111     12  N-P-LOAN            PIC S9(3)       COMP-3.
000112     12  N-P-FACTOR          PIC S9(4)V9(9)  COMP-3.
000113
000114*                        COPY ERCNETWS.
      *>>((file: ERCNETWS))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ERCNETWS                           *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.014                         *
000007*                                                               *
000008*    WORKING STORAGE TO USE WITH ERCNETP - NET PAY CALCULATIONS *
000009*                                                               *
000010*****************************************************************.
000011*                   C H A N G E   L O G
000012*
000013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000014*-----------------------------------------------------------------
000015*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000016* EFFECTIVE    NUMBER
000017*-----------------------------------------------------------------
000018* 060909  CR2008042300002  PEMA  ADD NEW FARM PLAN CALC
000019* 101509  IR2009100700002  PEMA  INCREASE DECIMAL POSITIONS
000020* 071910  IR2010062900001  PEMA  ADD MN NET BALLOON REFUND CALC
000021* 071511  CR2011021500001  PEMA  ADD CODE FOR DCC 10C
000022* 050713  CR2008052200001  PEMA  ADD CODE FOR 0 PCT APR
000023******************************************************************
000024
000025 01  NET-PAY-WORK-AREA.
000026     12  OPTION-SW               PIC X   VALUE 'X'.
000027        88  NPO-STD         VALUE SPACE.
000028        88  NPO-ALT         VALUE 'A'.
000029        88  NPO-SIMPLE      VALUE 'S'.
000030        88  NPO-2MO         VALUE 'I'.
000031        88  NPO-3MO         VALUE 'J'.
000032        88  NPO-4MO         VALUE 'K'.
000033        88  NPO-TRUNC       VALUE 'T' 'U' 'V' 'W' 'X'.
000034        88  NPO-TRUNC-0     VALUE 'T'.
000035        88  NPO-TRUNC-1     VALUE 'U'.
000036        88  NPO-TRUNC-2     VALUE 'V'.
000037        88  NPO-TRUNC-3     VALUE 'W'.
000038        88  NPO-TRUNC-4     VALUE 'X'.
000039        88  NPO-REFUND      VALUE 'R'.
000040        88  NPO-PRUDENTIAL  VALUE 'P'.
000041
000042     12  TYPE-SW             PIC X   VALUE 'N'.
000043        88  NET-STD             VALUE 'N'.
000044        88  NET-SMP             VALUE 'S'.
000045
000046     12  NP-PROCESS-SW       PIC X   VALUE '1'.
000047        88  NP-RATING           VALUE '1'.
000048        88  NP-REFUND           VALUE '2'.
000049        88  NP-REMAIN-AMT       VALUE '3'.
000050
000051     12  COMP-3-WORK-AREA         COMP-3.
000052         16  WS-WORK-DIS-RATE PIC S99V9(06)    VALUE +0.
000053         16  V               PIC SV9(13)       VALUE +.0.
000054         16  J               PIC SV9(9)        VALUE +0.
000055         16  K               PIC SV9(9)        VALUE +0.
000056         16  I               PIC SV9(10)       VALUE +.0.
000057         16  D               PIC S9(5)         VALUE +.0.
000058         16  APR100          PIC SV9(9)        VALUE +.0.
000059         16  RA              PIC S9(6)V9(9)    VALUE +.0.
000060         16  WK1             PIC S9(4)V9(13)   VALUE +.0.
000061         16  WK2             PIC S9(4)V9(13)   VALUE +.0.
000062         16  pema            pic s9(2)v9(15)   value +0.
000063         16  WK3             PIC S9(7)V9(10)   VALUE +.0.
000064         16  WK4             PIC S9(7)V9(8)    VALUE +.0.
000065         16  WK5             PIC S9(5)V9(13)   VALUE +.0.
000066         16  WK5-CSL         PIC S9(9)V9(6)    VALUE +.0.
000067         16  WK6             PIC S9(7)V9(10)   VALUE +.0.
000068         16  WK7             PIC S9(7)V9(10)   VALUE +.0.
000069         16  WK8             PIC S9(3)V9(10)   VALUE +.0.
000070         16  WK9             PIC S9(3)V9(10)   VALUE +.0.
000071         16  GR              PIC S9(7)V9(10)   VALUE +.0.
000072         16  ONE-PLUS-I      PIC S9(3)V9(10)   VALUE +.0.
000073         16  ODF             PIC S9(3)V9(10)   VALUE +.0.
000074         16  OD              PIC S9(3)V9(10)   VALUE +.0.
000075         16  FM              PIC S9(3)V9(10)   VALUE +.0.
000076         16  FN              PIC S9(3)V9(10)   VALUE +.0.
000077         16  FNM             PIC S9(3)V9(10)   VALUE +.0.
000078         16  ANGLEM          PIC S9(5)V9(13)   VALUE +.0.
000079         16  ANGLEMY         PIC S9(5)V9(13)   VALUE +.0.
000080         16  ANGLEMP1        PIC S9(5)V9(13)   VALUE +.0.
000081         16  ANGLEN          PIC S9(5)V9(13)   VALUE +.0.
000082         16  ANGLENY         PIC S9(5)V9(13)   VALUE +.0.
000083         16  ANGLEN-M        PIC S9(5)V9(13)   VALUE +.0.
000084         16  ANGLEN-MP1      PIC S9(5)V9(13)   VALUE +.0.
000085         16  ANGLEN-MP1Y     PIC S9(5)V9(13)   VALUE +.0.
000086         16  ANGLEN-1        PIC S9(7)V9(10)   VALUE +.0.
000087         16  ANGLEM-1        PIC S9(7)V9(10)   VALUE +.0.
000088         16  ANGLEM-1Y       PIC S9(7)V9(10)   VALUE +.0.
000089         16  ANGLEM-T        PIC S9(5)V9(13)   VALUE +.0.
000090         16  PVBALLOON       PIC S9(7)V99      VALUE +.0.
000091         16  TRUNC-PMT       PIC S9(7)V99      VALUE +.0.
000092         16  K1              PIC S999          VALUE +1.
000093         16  K12             PIC S999          VALUE +12.
000094         16  K100            PIC S999          VALUE +100.
000095         16  K1000           PIC S9(7)         VALUE +1000.
000096         16  NC-LR           PIC S9(2)V9(5)    VALUE +.0.
000097         16  Y REDEFINES NC-LR
000098                             PIC S99V9(5).
000099         16  TI              PIC S999V9(5)     VALUE +0.
000100         16  NC-R            PIC S9(7)V9(10)   VALUE +.0.
000101         16  NC-OB           PIC S9(7)V9(10)   VALUE +.0.
000102         16  NC-P            PIC S9(7)V9(10)   VALUE +.0.
000103         16  NC-D            PIC S9(7)V9(10)   VALUE +.0.
000104         16  NC-BIG-D        PIC S9(3)         VALUE +030.
000105         16  NC-LITTLE-D     PIC S9(2)V9(7)    VALUE +.0.
000106         16  NC-MP           PIC S9(5)V9(2)    VALUE +.0.
000107         16  NC-LP           PIC S9(5)V9(2)    VALUE +.0.
000108
000109         16  CA-MP           PIC S9(5)V9(2)    VALUE +.0.
000110         16  CA-API          PIC S9(2)V9(8)    VALUE +.0.
000111         16  CA-APJ          PIC S9(2)V9(8)    VALUE +.0.
000112         16  CA-VI           PIC S9(2)V9(8)    VALUE +.0.
000113         16  CA-J            PIC S9(2)V9(8)    VALUE +.0.
000114         16  CA-DISCOUNT     PIC S9(2)V9(8)    VALUE +.042000.
000115         16  GTL-IA-DIVIDEND PIC S9(5)V9(8)    VALUE +.0.
000116         16  GTL-IA-DIVISOR  PIC S9(5)V9(8)    VALUE +.0.
000117         16  GTL-IA-FACTOR   PIC S9(5)V9(8)    VALUE +.0.
000118         16  GTL-IA-ODF      PIC S9(5)V9(8)    VALUE +.0.
000119         16  GTL-IA-ODD-DAYS PIC S999          VALUE +0.
000120         16  GTL-IA-SGN      PIC S9            VALUE +0.
000121         16  GTL-IA-ABS      PIC 999           VALUE ZEROS.
000122
000123         16  ANNUAL-INT-RATE PIC S9(3)V9(4)    VALUE +.0.
000124
000125         16  ORIGINAL-TERM   PIC S999.
000126         16  M              REDEFINES ORIGINAL-TERM
000127                             PIC S999.
000128
000129         16  REMAINING-TERM  PIC S999.
000130         16  R              REDEFINES REMAINING-TERM
000131                             PIC S999.
000132
000133         16  LOAN-TERM       PIC S999.
000134         16  N              REDEFINES LOAN-TERM
000135                             PIC S999.
000136
000137         16  EXPIRED-TERM    PIC S999.
000138         16  E              REDEFINES EXPIRED-TERM
000139                             PIC S999.
000140
000141         16  PRUDENTIAL-WORK-AMT
000142                             PIC S9(7)V9(8).
000143         16  PWK            REDEFINES PRUDENTIAL-WORK-AMT
000144                             PIC S9(7)V9(8).
000145
000146         16  FACTOR          PIC S9(4)V9(9)    VALUE +0.0.
000147         16  VX              PIC S9V9(13)      VALUE +0.0.
000148         16  SV              PIC S9V9(13)      VALUE +0.0.
000149         16  SV-MINUS-ONE    PIC S999V9(09)    VALUE +0.0.
000150         16  SX              PIC S9V9(13)      VALUE +0.0.
000151         16  SE              PIC S9V9(10)      VALUE +0.0.
000152         16  N2              PIC S9(7)         VALUE +0.
000153         16  N3              PIC S9(7)         VALUE +0.
000154         16  K-I             PIC S9V9(8)       VALUE +0.0.
000155
000156     12  BINARY-WORK-AREA        COMP.
000157         16  X1              PIC S999        VALUE +0.
000158         16  X2              PIC S999        VALUE +0.
000159         16  MAX-X           PIC S9(5)       VALUE +0.
000160         16  B1              PIC S9(5)       VALUE +1.
000161         16  LINDX           PIC S999        VALUE ZEROS.
000162         16  AHNDX           PIC S999        VALUE ZEROS.
000163
000164     12  CSO-RATE-WORK-AREAS.
000165         16  C-PMTS-PER-YEAR     PIC S999V99 COMP-3 VALUE ZEROS.
000166         16  C-DAYS-IN-PMT-PER   PIC S999 COMP-3 VALUE ZEROS.
000167         16  C-TOT-PMTS          PIC S999 COMP-3 VALUE ZEROS.
000168
000169     12  WS-WORK-MISC.
000170         16  ws-extra-pmts       pic s999         comp-3.
000171         16  NET-BEN             PIC S9(9)V9999   COMP-3.
000172         16  WS-RATE-TERM        PIC S9(3)        COMP-3.
000173*        16  WS-AGE              PIC 99           VALUE ZEROS.
000174         16  WS-AGE              PIC S9(3)V99 COMP-3 VALUE +0.
000175         16  WS-AH-FACE-BENEFIT  PIC S9(7)        COMP-3.
000176         16  WS-COUNTER          PIC S9(3)    COMP-3  VALUE +0.
000177         16  WS-WORK-RATE        PIC S9(2)V9(8) COMP-3 VALUE +0.
000178         16  WS-DISCOUNT-OPTION  PIC X VALUE SPACES.
000179         16  WS-DUE-DATE         PIC S9(7) COMP-3 VALUE +0.
000180         16  WS-INT-BEGIN-DATE   PIC S9(7) COMP-3 VALUE +0.
000181         16  WS-X-Y-FACTOR       PIC S9(3)V9(15) COMP-3 VALUE +0.
000182         16  WS-Y-X-FACTOR       PIC S9(3)V9(15) COMP-3 VALUE +0.
000183         16  WS-ODD-DAY-UNITS    PIC S9(3)V9(15) COMP-3 VALUE +0.
000184         16  WS-CALC-FREQ-RATE   PIC S9(2)V9(15) COMP-3 VALUE +0.
000185         16  WS-NP-FACTOR        PIC S9(5)V9(10) COMP-3 VALUE +0.
000186******************************************************************
      *<<((file: ERCNETWS))
000115 EJECT
000116*                        COPY ELCDATE.
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
000117 EJECT
000118*                        COPY ELCCALC.
      *>>((file: ELCCALC))
000001******************************************************************
000002*                                                                *
000003*                           ELCCALC.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.025                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
000008*                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
000009*                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
000010*                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
000011*                                                                *
000012*  PASSED TO ELRTRM                                              *
000013*  -----------------                                             *
000014*  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
000015*  ORIGINAL TERM                                                 *
000016*  BEGINNING DATE                                                *
000017*  ENDING DATE                                                   *
000018*  COMPANY I.D.                                                  *
000019*  ACCOUNT MASTER USER FIELD                                     *
000020*  PROCESS SWITCH (CANCEL, CLAIM)                                *
000021*  FREE LOOK DAYS                                                *
000022*                                                                *
000023*  RETURNED FROM ELRTRM                                          *
000024*  ---------------------                                         *
000025*  REMAINING TERM 1 - USED FOR EARNINGS                          *
000026*  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
000027*  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
000028*  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
000029*----------------------------------------------------------------*
000030*  PASSED TO ELRAMT                                              *
000031*  ----------------                                              *
000032*  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
000033*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000034*  ORIGINAL AMOUNT                                               *
000035*  ALTERNATE BENEFIT (BALLON)                                    *
000036*  A.P.R. - NET PAY ONLY                                         *
000037*  METHOD
000038*  PAYMENT FREQUENCY - FOR FARM PLAN                             *
000039*  COMPANY I.D.                                                  *
000040*  BENEFIT TYPE                                                  *
000041*                                                                *
000042*  RETURNED FROM ELRAMT                                          *
000043*  --------------------                                          *
000044*  REMAINING AMOUNT 1 - CURRENT                                  *
000045*  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
000046*  REMAINING AMOUNT FACTOR
000047*----------------------------------------------------------------*
000048*  PASSED TO ELRESV                                              *
000049*  -----------------                                             *
000050*  CERTIFICATE EFFECTIVE DATE                                    *
000051*  VALUATION DATE                                                *
000052*  PAID THRU DATE                                                *
000053*  BENEFIT                                                       *
000054*  INCURRED DATE                                                 *
000055*  REPORTED DATE                                                 *
000056*  ISSUE AGE                                                     *
000057*  TERM                                                          *
000058*  CDT PERCENT                                                   *
000059*  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
000060* *CLAIM TYPE (LIFE, A/H)                                        *
000061* *REMAINING BENEFIT (FROM ELRAMT)                               *
000062* *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
000063*                                                                *
000064*  RETURNED FROM ELRESV                                          *
000065*  --------------------                                          *
000066*  CDT TABLE USED                                                *
000067*  CDT FACTOR USED                                               *
000068*  PAY TO CURRENT RESERVE                                        *
000069*  I.B.N.R. - A/H ONLY                                           *
000070*  FUTURE (ACCRUED) AH ONLY                                      *
000071*----------------------------------------------------------------*
000072*  PASSED TO ELRATE                                              *
000073*  ----------------                                              *
000074*  CERT ISSUE DATE                                               *
000075*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000076*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000077*  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
000078*  STATE CODE (CLIENT DEFINED)                                   *
000079*  STATE CODE (STANDARD P.O. ABBRV)                              *
000080*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000081*  DEVIATION CODE                                                *
000082*  ISSUE AGE                                                     *
000083*  ORIGINAL BENEFIT AMOUNT                                       *
000084*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000085*  PROCESS TYPE (ISSUE OR CANCEL)                                *
000086*  BENEFIT KIND (LIFE OR A/H)                                    *
000087*  A.P.R.                                                        *
000088*  METHOD
000089*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000090*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000091*  COMPANY I.D. (3 CHARACTER)                                    *
000092*  BENEFIT CODE                                                  *
000093*  BENEFIT OVERRIDE CODE                                         *
000094*  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
000095*  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
000096*  JOINT INDICATOR (CSL ONLY)                                    *
000097*  FIRST PAYMENT DATE (CSL ONLY)                                 *
000098*  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
000099*                                                                *
000100*  RETURNED FROM ELRATE                                          *
000101*  --------------------                                          *
000102*  CALCULATED PREMIUM                                            *
000103*  PREMIUM RATE                                                  *
000104*  MORTALITY CODE                                                *
000105*  MAX ATTAINED AGE                                              *
000106*  MAX AGE                                                       *
000107*  MAX TERM                                                      *
000108*  MAX MONTHLY BENEFIT                                           *
000109*  MAX TOTAL BENIFIT                                             *
000110*  COMPOSITE RATE (OPEN-END ONLY)                                *
000111*----------------------------------------------------------------*
000112*  PASSED TO ELRFND                                              *
000113*  ----------------                                              *
000114*  CERT ISSUE DATE                                               *
000115*  REFUND DATE                                                   *
000116*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000117*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000118*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000119*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000120*  STATE CODE (CLIENT DEFINED)                                   *
000121*  STATE CODE (STANDARD P.O. ABBRV)                              *
000122*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000123*  DEVIATION CODE                                                *
000124*  ISSUE AGE                                                     *
000125*  ORIGINAL BENEFIT AMOUNT                                       *
000126*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000127*  PROCESS TYPE (CANCEL)                                         *
000128*  BENEFIT KIND (LIFE OR A/H)                                    *
000129*  A.P.R.                                                        *
000130*  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
000131*  RATING METHOD -  (CODE FROM BENEFIT)                          *
000132*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000133*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000134*  COMPANY I.D. (3 CHARACTER)                                    *
000135*  BENEFIT CODE                                                  *
000136*  BENEFIT OVERRIDE CODE                                         *
000137*                                                                *
000138*  RETURNED FROM ELRFND                                          *
000139*  --------------------                                          *
000140*  CALCULATED REFUND                                             *
000141*----------------------------------------------------------------*
000142*  PASSED TO ELEARN                                              *
000143*  ----------------                                              *
000144*  CERT ISSUE DATE                                               *
000145*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000146*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000147*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000148*  STATE CODE (CLIENT DEFINED)                                   *
000149*  STATE CODE (STANDARD P.O. ABBRV)                              *
000150*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000151*  DEVIATION CODE                                                *
000152*  ISSUE AGE                                                     *
000153*  ORIGINAL BENEFIT AMOUNT                                       *
000154*  BENEFIT KIND (LIFE OR A/H)                                    *
000155*  A.P.R.                                                        *
000156*  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
000157*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000158*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000159*  COMPANY I.D. (3 CHARACTER)                                    *
000160*  BENEFIT CODE                                                  *
000161*  BENEFIT OVERRIDE CODE                                         *
000162*                                                                *
000163*  RETURNED FROM ELEARN                                          *
000164*  --------------------                                          *
000165*  INDICATED  EARNINGS                                           *
000166*----------------------------------------------------------------*
000167*                 LENGTH = 450                                   *
000168*                                                                *
000169******************************************************************
000170******************************************************************
000171*                   C H A N G E   L O G
000172*
000173* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000174*-----------------------------------------------------------------
000175*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000176* EFFECTIVE    NUMBER
000177*-----------------------------------------------------------------
000178* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
000179* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000180* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
000181* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
000182* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
000183* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
000184* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
000185* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000186* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000187* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
000188* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000189* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
000190******************************************************************
000191
000192 01  CALCULATION-PASS-AREA.
000193     12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
000194                                     COMP.
000195
000196     12  CP-RETURN-CODE            PIC X             VALUE ZERO.
000197       88  NO-CP-ERROR                             VALUE ZERO.
000198       88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
000199                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
000200       88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
000201       88  CP-ERROR-IN-DATES                       VALUE '2'.
000202       88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
000203       88  CP-ERROR-IN-TERMS                       VALUE '4'.
000204       88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
000205       88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
000206       88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
000207       88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
000208       88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
000209       88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
000210       88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
000211       88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
000212       88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
000213       88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
000214       88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
000215       88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
000216       88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
000217       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
000218
000219     12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
000220       88  NO-CP-ERROR-2                           VALUE ZERO.
000221***********************  INPUT AREAS ****************************
000222
000223     12  CP-CALCULATION-AREA.
000224         16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
000225         16  CP-CERT-EFF-DT        PIC XX.
000226         16  CP-VALUATION-DT       PIC XX.
000227         16  CP-PAID-THRU-DT       PIC XX.
000228         16  CP-BENEFIT-TYPE       PIC X.
000229           88  CP-AH                               VALUE 'A' 'D'
000230                                                   'I' 'U'.
000231           88  CP-REDUCING-LIFE                    VALUE 'R'.
000232           88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
000233         16  CP-INCURRED-DT        PIC XX.
000234         16  CP-REPORTED-DT        PIC XX.
000235         16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
000236         16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
000237         16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
000238                                     COMP-3.
000239         16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
000240                                     COMP-3.
000241         16  CP-CDT-METHOD         PIC X.
000242           88  CP-CDT-ROUND-NEAR                   VALUE '1'.
000243           88  CP-CDT-ROUND-HIGH                   VALUE '2'.
000244           88  CP-CDT-INTERPOLATED                 VALUE '3'.
000245         16  CP-CLAIM-TYPE         PIC X.
000246           88  CP-AH-CLAIM                         VALUE 'A'.
000247           88  CP-LIFE-CLAIM                       VALUE 'L'.
000248         16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
000249                                     COMP-3.
000250         16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
000251                                     COMP-3.
000252         16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
000253                                     COMP-3.
000254         16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
000255                                     COMP-3.
000256         16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
000257                                     COMP-3.
000258         16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
000259                                     COMP-3.
000260         16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
000261                                     COMP-3.
000262         16  CP-REM-TERM-METHOD    PIC X.
000263           88  CP-EARN-AFTER-15TH                  VALUE '1'.
000264           88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
000265           88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
000266           88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
000267           88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
000268           88  CP-EARN-AFTER-14TH                  VALUE '6'.
000269           88  CP-EARN-AFTER-16TH                  VALUE '7'.
000270         16  CP-EARNING-METHOD     PIC X.
000271           88  CP-EARN-BY-R78                      VALUE '1' 'R'.
000272           88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
000273           88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
000274           88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
000275           88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
000276           88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
000277           88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
000278           88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
000279           88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
000280           88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
000281           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
000282           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
000283           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
000284           88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
000285         16  CP-PROCESS-TYPE       PIC X.
000286           88  CP-CLAIM                            VALUE '1'.
000287           88  CP-CANCEL                           VALUE '2'.
000288           88  CP-ISSUE                            VALUE '3'.
000289         16  CP-SPECIAL-CALC-CD    PIC X.
000290           88  CP-OUTSTANDING-BAL              VALUE 'O'.
000291           88  CP-1-MTH-INTEREST               VALUE ' '.
000292           88  CP-0-MTH-INTEREST               VALUE 'A'.
000293           88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
000294           88  CP-CRITICAL-PERIOD              VALUE 'C'.
000295           88  CP-TERM-IS-DAYS                 VALUE 'D'.
000296           88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
000297           88  CP-FARM-PLAN                    VALUE 'F'.
000298           88  CP-RATE-AS-STANDARD             VALUE 'G'.
000299           88  CP-2-MTH-INTEREST               VALUE 'I'.
000300           88  CP-3-MTH-INTEREST               VALUE 'J'.
000301           88  CP-4-MTH-INTEREST               VALUE 'K'.
000302           88  CP-BALLOON-LAST-PMT             VALUE 'L'.
000303           88  CP-MORTGAGE-REC                 VALUE 'M'.
000304           88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
000305           88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
000306           88  CP-NET-PAY-SIMPLE               VALUE 'S'.
000307           88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
000308                                                     'W' 'X'.
000309           88  CP-TRUNCATE-0-MTH               VALUE 'T'.
000310           88  CP-TRUNCATE-1-MTH               VALUE 'U'.
000311           88  CP-TRUNCATE-2-MTH               VALUE 'V'.
000312           88  CP-TRUNCATE-3-MTH               VALUE 'W'.
000313           88  CP-TRUNCATE-4-MTH               VALUE 'X'.
000314           88  CP-SUMMARY-REC                  VALUE 'Z'.
000315           88  CP-PROPERTY-BENEFIT             VALUE '2'.
000316           88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
000317           88  CP-AD-D-BENEFIT                 VALUE '4'.
000318           88  CP-CSL-METH-1                   VALUE '5'.
000319           88  CP-CSL-METH-2                   VALUE '6'.
000320           88  CP-CSL-METH-3                   VALUE '7'.
000321           88  CP-CSL-METH-4                   VALUE '8'.
000322
000323         16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
000324                                     COMP-3.
000325         16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
000326         16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
000327         16  CP-STATE              PIC XX          VALUE SPACE.
000328         16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
000329         16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
000330           88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
000331               '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
000332         16  CP-R78-OPTION         PIC X.
000333           88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
000334           88  CP-TERM-TIMES-TERM                  VALUE '1'.
000335
000336         16  CP-COMPANY-CD         PIC X             VALUE SPACE.
000337         16  CP-IBNR-RESERVE-SW    PIC X.
000338         16  CP-CLAIM-STATUS       PIC X.
000339         16  CP-RATE-FILE          PIC X.
000340         16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
000341                                     COMP-3.
000342
000343         16  CP-LIFE-OVERRIDE-CODE PIC X.
000344         16  CP-AH-OVERRIDE-CODE   PIC X.
000345
000346         16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
000347                                     COMP-3.
000348         16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
000349                                   PIC S9(5)V99 COMP-3.
000350         16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
000351                                     COMP-3.
000352         16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
000353                                     COMP-3.
000354         16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
000355                                     COMP-3.
000356         16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
000357                                  PIC S9(7)V99 COMP-3.
000358
000359         16  CP-PAID-FROM-DATE     PIC X(02).
000360         16  CP-CLAIM-CALC-METHOD  PIC X(01).
000361         16  CP-EXT-DAYS-CALC      PIC X.
000362           88  CP-EXT-NO-CHG                   VALUE ' '.
000363           88  CP-EXT-CHG-LF                   VALUE '1'.
000364           88  CP-EXT-CHG-AH                   VALUE '2'.
000365           88  CP-EXT-CHG-LF-AH                VALUE '3'.
000366         16  CP-DOMICILE-STATE     PIC XX.
000367         16  CP-CARRIER            PIC X.
000368         16  CP-REIN-FLAG          PIC X.
000369         16  CP-REM-TRM-CALC-OPTION PIC X.
000370           88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
000371                      '2' '3' '4' '5'.
000372           88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
000373           88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
000374           88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
000375           88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
000376           88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
000377           88  CP-EXT-30-DAY-MONTH          VALUE '3'.
000378           88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
000379           88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
000380         16  CP-SIG-SWITCH         PIC X.
000381         16  CP-RATING-METHOD      PIC X.
000382           88  CP-RATE-AS-R78                      VALUE '1' 'R'.
000383           88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
000384           88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
000385           88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
000386           88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
000387           88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
000388           88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
000389           88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
000390           88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
000391         16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
000392                                     COMP-3.
000393         16  CP-BEN-CATEGORY       PIC X.
000394         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000395         16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
000396                                   PIC S99V9(5) COMP-3.
000397         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000398         16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
000399                                   PIC S99V9(5) COMP-3.
000400         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
000401         16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
000402         16  CP-EXPIRE-DT          PIC XX.
000403         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
000404         16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
000405         16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
000406         16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
000407         16  CP-DDF-SPEC-CALC      PIC X.
000408             88  CP-CALC-GROSS-FEE        VALUE 'G'.
000409             88  CP-CALC-CLP              VALUE 'C'.
000410         16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
000411         16  CP-CANCEL-REASON      PIC X.
000412         16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
000413         16  CP-PMT-MODE           PIC X.
000414         16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
000415         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
000416         16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
000417         16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
000418         16  FILLER                PIC X.
000419
000420***************    OUTPUT FROM ELRESV   ************************
000421
000422         16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
000423
000424         16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
000425                                     COMP-3.
000426         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
000427                                     COMP-3.
000428         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
000429                                     COMP-3.
000430         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
000431                                     COMP-3.
000432         16  FILLER                PIC X(09).
000433***************    OUTPUT FROM ELRTRM   *************************
000434
000435         16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
000436                                     COMP-3.
000437         16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
000438                                     COMP-3.
000439         16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
000440                                     COMP-3.
000441         16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
000442                                     COMP-3.
000443         16  FILLER                PIC X(12).
000444
000445***************    OUTPUT FROM ELRAMT   *************************
000446
000447         16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
000448                                     COMP-3.
000449         16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
000450                                     COMP-3.
000451         16  FILLER                PIC X(12).
000452
000453***************    OUTPUT FROM ELRATE   *************************
000454
000455         16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
000456                                     COMP-3.
000457         16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
000458                                     COMP-3.
000459         16  CP-MORTALITY-CODE     PIC X(4).
000460         16  CP-RATE-EDIT-FLAG     PIC X.
000461             88  CP-RATES-NEED-APR                  VALUE '1'.
000462         16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
000463                                     COMP-3.
000464         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
000465         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
000466         16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
000467                                   PIC S9(7)V99 COMP-3.
000468         16  FILLER                PIC X(07).
000469
000470***************    OUTPUT FROM ELRFND   *************************
000471
000472         16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
000473                                     COMP-3.
000474         16  CP-REFUND-TYPE-USED   PIC X.
000475           88  CP-R-AS-R78                         VALUE '1'.
000476           88  CP-R-AS-PRORATA                     VALUE '2'.
000477           88  CP-R-AS-CALIF                       VALUE '3'.
000478           88  CP-R-AS-TEXAS                       VALUE '4'.
000479           88  CP-R-AS-FARM-PLAN                   VALUE '4'.
000480           88  CP-R-AS-NET-PAY                     VALUE '5'.
000481           88  CP-R-AS-ANTICIPATION                VALUE '6'.
000482           88  CP-R-AS-MEAN                        VALUE '8'.
000483           88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
000484           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
000485           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
000486           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
000487           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
000488           88  CP-R-AS-REPOSSESSION                VALUE 'R'.
000489         16  FILLER                PIC X(12).
000490
000491***************    OUTPUT FROM ELEARN   *************************
000492
000493         16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
000494                                     COMP-3.
000495         16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
000496                                     COMP-3.
000497         16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
000498                                     COMP-3.
000499         16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
000500                                     COMP-3.
000501         16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
000502                                     COMP-3.
000503         16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
000504                                     COMP-3.
000505         16  CP-EARNING-TYPE-USED  PIC X.
000506           88  CP-E-AS-SPECIAL                     VALUE 'S'.
000507           88  CP-E-AS-R78                         VALUE '1'.
000508           88  CP-E-AS-PRORATA                     VALUE '2'.
000509           88  CP-E-AS-TEXAS                       VALUE '4'.
000510           88  CP-E-AS-FARM-PLAN                   VALUE '4'.
000511           88  CP-E-AS-NET-PAY                     VALUE '5'.
000512           88  CP-E-AS-ANTICIPATION                VALUE '6'.
000513           88  CP-E-AS-MEAN                        VALUE '8'.
000514           88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
000515         16  FILLER                PIC X(12).
000516
000517***************    OUTPUT FROM ELPMNT   *************************
000518
000519         16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
000520                                     COMP-3.
000521         16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
000522                                     COMP-3.
000523         16  FILLER                PIC X(12).
000524
000525***************   MISC WORK AREAS    *****************************
000526         16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
000527                                     COMP-3.
000528         16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
000529                                     COMP-3.
000530         16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
000531                                     COMP-3.
000532         16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
000533                                     COMP-3.
000534         16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
000535                                     COMP-3.
000536         16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
000537                                     COMP-3.
000538         16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
000539             88  OPEN-RATE-FILE                   VALUE 'O'.
000540             88  CLOSE-RATE-FILE                  VALUE 'C'.
000541             88  IO-ERROR                         VALUE 'E'.
000542
000543         16  CP-FIRST-PAY-DATE     PIC XX.
000544
000545         16  CP-JOINT-INDICATOR    PIC X.
000546
000547         16  CP-RESERVE-REMAINING-TERM
000548                                   PIC S9(4)V9    VALUE ZERO
000549                                     COMP-3.
000550
000551         16  CP-INSURED-BIRTH-DT   PIC XX.
000552
000553         16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
000554                                     COMP-3.
000555
000556         16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
000557                                     COMP-3.
000558
000559         16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
000560                                     COMP-3.
000561
000562         16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
000563                                     COMP-3.
000564
000565         16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
000566                                     COMP-3.
000567
000568         16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
000569                                     COMP-3.
000570
000571         16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
000572                                     COMP-3.
000573
000574         16  CP-ROA-REFUND         PIC X          VALUE 'N'.
000575             88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
000576
000577         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
000578                                     COMP-3.
000579         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
000580                                     COMP-3.
000581         16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
000582         16  cp-extra-periods      pic 9 value zeros.
000583         16  cp-net-only-state     pic x value spaces.
000584         16  FILLER                PIC X(13).
000585******************************************************************
      *<<((file: ELCCALC))
000119
000120
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
000122 01  DFHCOMMAREA                PIC X(450).
000123
000124     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'ELRFND' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000125 VCOBOL-DUMMY-PROCEDURE.
000126     MOVE DFHCOMMAREA    TO  CALCULATION-PASS-AREA.
000127
000128 000-START-REFUND-CALC.
000129*                      COPY ELCRFNDP.
      *>>((file: ELCRFNDP))
000001****************************************************************
000002*                                                              *
000003*                            ELCRFNDP.                         *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.028                        *
000006*                                                              *
000007****************************************************************.
000008*                   C H A N G E   L O G
000009*
000010* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000011*-----------------------------------------------------------------
000012*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000013* EFFECTIVE    NUMBER
000014*-----------------------------------------------------------------
000015* 042203                   PEMA ADD SPECIAL IN CALC FOR CID
000016* 033104    2003080800002  PEMA ADD GAP NON REFUNDABLE OPTION
000017* 042904    2003080800002  PEMA ADD ACTUARIAL EARNING METHOD
000018* 110609  CR2009092300002  PEMA CHANGE MN REFUNDS
000019* 000000  CR2008042200001  PEMA ADD 0 APR PROCESSING
000020* 071910  IR2010062900001  PEMA ADD MN NET BALLOON REFUND CALC
000021* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000022* 032612  CR2011110200001  PEMA AHL CHANGES
000023* 050713  CR2008042200001  PEMA  ADD CODE FOR ZERO APR NET PAY
000024* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000025* 011317  IR2017011000002  PEMA  REMOVE 'CT' HARDCODING
000026******************************************************************
000027
000028     MOVE ZERO                   TO  CP-RETURN-CODE
000029                                     CP-CALC-REFUND.
000030     MOVE SPACE                  TO  CP-REFUND-TYPE-USED.
000031
000032 0001-GET-CANCEL-TYPE.
000033     IF CP-CERT-EFF-DT = CP-VALUATION-DT
000034        MOVE CP-ORIGINAL-PREMIUM TO CP-CALC-REFUND
000035        GO TO 1999-CALC-REF-AMT-X.
000036
000037     IF CP-CANCEL-REASON = 'R'
000038        IF CP-CALC-CLP
000039           MOVE CP-ORIGINAL-PREMIUM
000040                                 TO CP-CALC-REFUND
000041        ELSE
000042           COMPUTE CP-CALC-REFUND = CP-ORIGINAL-PREMIUM -
000043              CP-DDF-ADMIN-FEES
000044        END-IF
000045        MOVE 'R'                 TO CP-REFUND-TYPE-USED
000046        GO TO 1999-CALC-REF-AMT-X
000047     END-IF
000048
000049     IF CP-REMAINING-TERM = ZERO   OR
000050        CP-REMAINING-TERM NEGATIVE
000051          MOVE ZERO TO CP-CALC-REFUND
000052          GO TO 1999-CALC-REF-AMT-X.
000053
000054     MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1.
000055     MOVE SPACE              TO DC-OPTION-CODE.
000056     PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
000057
000058     IF DATE-CONVERSION-ERROR
000059         MOVE '2' TO CP-RETURN-CODE
000060         GO TO 1999-CALC-REF-AMT-X.
000061
000062     IF CP-AH
000063        GO TO 1030-GET-AH-REFUND.
000064
000065********************    LIFE REFUNDS    **************************
000066
000067     IF CP-TERM-OR-EXT-DAYS NOT NUMERIC
000068         MOVE +0                TO CP-TERM-OR-EXT-DAYS.
000069
000070     IF CP-LOAN-TERM NOT NUMERIC
000071         MOVE +0                TO CP-LOAN-TERM.
000072
000073     COMPUTE PR-FACTOR = CP-REMAINING-TERM / CP-ORIGINAL-TERM.
000074
000075     COMPUTE R78-FACTOR =
000076            (CP-REMAINING-TERM * (CP-REMAINING-TERM + 1)) /
000077              (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1)).
000078************************************************************
000079***       ADDED IF CONDITION FOR TESTING PURPOSES.       ***
000080***---------------------------------------------------------
000081     IF CP-TRUNCATED-LIFE
000082        MOVE  CP-LOAN-TERM         TO  WS-RATE-TERM
000083     ELSE
000084        MOVE  CP-ORIGINAL-TERM     TO  WS-RATE-TERM.
000085***
000086************************************************************
000087**** THE FOLLOWING IF STATEMENT FOR NCL ONLY SHOULD DO THE
000088**** FOLLOWING.
000089****       FOR NC. LIFE..
000090****  LEVEL LIFE COVERAGES   --- REFUND AS PRO RATA.
000091****  TRUNCATED NET COVERAGE --- TERM > 60 USE RULE OF ANTIC.
000092****                             TERM <= 60 USE SUM OF DIGITS.
000093****  DECREASING COVERAGE    --- EFF DATE > 093081 AND
000094****                             TERM > 60 USE RULE OF ANTIC
000095****                             OTHERWISE USE RULE OF 78.
000096****
000097****
000098
000099     IF (CP-COMPANY-ID = 'NCL') AND
000100        (CP-STATE-STD-ABBRV = 'NC')
000101        IF CP-LEVEL-LIFE
000102           GO TO 1023-REF-PR
000103        ELSE
000104        IF CP-EARN-AS-NET-PAY AND
000105           CP-TRUNCATED-LIFE
000106           IF CP-ORIGINAL-TERM GREATER THAN 60
000107              GO TO 1021-ANTICIPATION-REF
000108           ELSE
000109              GO TO 1026-REF-R78
000110        ELSE
000111        IF CP-REDUCING-LIFE
000112           IF (DC-GREG-DATE-CYMD  GREATER 19810930 AND
000113              CP-ORIGINAL-TERM GREATER THAN 60)
000114              GO TO 1021-ANTICIPATION-REF
000115           ELSE
000116              GO TO 1075-SUM-OF-DIGITS.
000117
000118****
000119**** THE FOLLOWING IF STATEMENT FOR CID ONLY SHOULD DO THE
000120**** FOLLOWING.
000121****       FOR OH. LIFE..
000122****  LEVEL LIFE COVERAGES   --- REFUND AS PRO RATA.
000123****  NET/NET TRUNCATED      --- TERM > 60 USE RULE OF ANTIC.
000124****  GROSS DECREASING       --- RULE OF 78
000125****
000126****
000127
000128     IF (CP-COMPANY-ID = 'CID') AND
000129        (CP-STATE-STD-ABBRV = 'OH') AND
000130        (CP-CLASS-CODE NOT = 'L ')
000131        IF CP-LEVEL-LIFE
000132           GO TO 1023-REF-PR
000133        ELSE
000134           IF (CP-ORIGINAL-TERM > 60) OR
000135              (CP-EARN-AS-NET-PAY)
000136              GO TO 1028-NET-PAY-REFUND
000137           ELSE
000138              GO TO 1026-REF-R78
000139           END-IF
000140        END-IF
000141     END-IF
000142
000143**** THE FOLLOWING IF STATEMENT FOR NCL ONLY SHOULD DO THE
000144**** FOLLOWING.
000145****       FOR VA. LIFE..
000146****  LEVEL LIFE COVERAGES   --- REFUND AS PRO RATA.
000147****           NET COVERAGES --- USE RULE OF ANTIC.
000148****  DECREASING COVERAGE    --- EFF DATE < 010193 USE R78.
000149****                         --- EFF DATE > 123194 USE ANTIC.
000150****                         --- EFF DATE BETWEEN 010193 AND 12319
000151****                             WITH TERM > 61 USE RULE OF ANTIC
000152****                             OTHERWISE USE RULE OF 78.
000153****
000154****    NOTE... PER NCL WE SHOULD NOT INCLUDE THEM IN ANY
000155****            OF THE SYSTEM DEFAULT STATUTORY REFUND RULES.
000156****            NCL WILL SUPPLY US WITH THE STATUTORY RULES THEY
000157****            WILL FOLLOW.
000158****
000159****
000160****
000161
000162     IF (CP-COMPANY-ID = 'NCL') AND
000163        (CP-STATE-STD-ABBRV = 'VA')
000164        IF CP-LEVEL-LIFE
000165           GO TO 1023-REF-PR
000166        ELSE
000167        IF CP-EARN-AS-NET-PAY
000168           GO TO 1021-ANTICIPATION-REF
000169        ELSE
000170        IF CP-REDUCING-LIFE
000171           IF DC-GREG-DATE-CYMD LESS THAN 19930101
000172              GO TO 1026-REF-R78
000173           ELSE
000174           IF DC-GREG-DATE-CYMD GREATER THAN 19941231
000175              GO TO 1021-ANTICIPATION-REF
000176           ELSE
000177           IF CP-ORIGINAL-TERM GREATER THAN 61
000178              GO TO 1021-ANTICIPATION-REF
000179           ELSE
000180              GO TO 1026-REF-R78.
000181
000182     IF (CP-COMPANY-ID = 'CID')
000183        AND (CP-STATE-STD-ABBRV = 'IN')
000184        AND (DC-GREG-DATE-CYMD < 20030401)
000185        IF CP-REDUCING-LIFE
000186           GO TO 1026-REF-R78
000187        ELSE
000188           IF CP-LEVEL-LIFE
000189              GO TO 1023-REF-PR
000190           END-IF
000191        END-IF
000192     END-IF
000193
000194     IF (CP-COMPANY-ID = 'CID')
000195        AND (CP-STATE-STD-ABBRV = 'FL')
000196        AND (DC-GREG-DATE-CYMD > 20030630)
000197        IF CP-RATE-AS-NET-PAY
000198           GO TO 1028-NET-PAY-REFUND
000199        END-IF
000200     END-IF
000201
000202     IF (CP-COMPANY-ID = 'CID')
000203        AND (CP-STATE-STD-ABBRV = 'MN')
000204        AND (CP-EARN-AS-NET-PAY)
000205        AND (CP-RATE-AS-REG-BALLOON)
000206        AND (DC-GREG-DATE-CYMD > 20091231)
000207        GO TO 1028-NET-PAY-REFUND
000208     END-IF
000209
000210     IF (CP-COMPANY-ID = 'CID')
000211        AND (CP-STATE-STD-ABBRV = 'MN')
000212        AND (DC-GREG-DATE-CYMD > 20091231)
000213        AND (CP-RATE-AS-NET-PAY)
000214        GO TO 1021-ANTICIPATION-REF
000215     END-IF
000216
000217     IF (CP-COMPANY-ID = 'CID')
000218        AND (CP-STATE-STD-ABBRV = 'MO')
000219        AND (DC-GREG-DATE-CYMD > 20030630)
000220        AND (CP-LOAN-TERM < +121)
000221        IF CP-RATE-AS-NET-PAY
000222           GO TO 1028-NET-PAY-REFUND
000223        END-IF
000224     END-IF
000225
000226     IF (CP-COMPANY-ID = 'CID')
000227        AND (CP-STATE-STD-ABBRV = 'ND')
000228        AND (DC-GREG-DATE-CYMD > 20011231)
000229        AND (CP-REDUCING-LIFE)
000230        GO TO 1021-ANTICIPATION-REF
000231     END-IF
000232
000233     IF CP-CRITICAL-PERIOD
000234        IF CP-COMPANY-ID = 'NCL'
000235           NEXT SENTENCE
000236        ELSE
000237           GO TO 1022-REF-CRITICAL-PERIOD-LF.
000238
000239     IF CP-TERM-IS-DAYS  AND
000240        CP-TERM-OR-EXT-DAYS GREATER ZERO
000241         GO TO 1024-REF-PR-DAYS.
000242
000243     IF CP-STATE-STD-ABBRV = 'NM'
000244       IF CP-REDUCING-LIFE
000245*        IF DC-GREG-DATE-1-YMD LESS THAN '980901'
000246         IF DC-GREG-DATE-CYMD LESS THAN 19980901
000247             GO TO 1026-REF-R78.
000248
000249     IF CP-EARN-ANTICIPATION
000250         GO TO 1021-ANTICIPATION-REF.
000251
000252     IF CP-STATE-STD-ABBRV = 'ME'
000253         IF CP-COMPANY-ID = 'NCL' OR 'CVL'
000254            NEXT SENTENCE
000255         ELSE
000256            IF CP-REDUCING-LIFE
000257               GO TO 1021-ANTICIPATION-REF.
000258
000259     IF CP-COMPANY-ID EQUAL 'CID' OR 'CSO'
000260        IF CP-STATE-STD-ABBRV = 'TX'
000261            GO TO SKIP-CSO-TEXAS-LIFE-PROCESSING
000262        END-IF
000263     END-IF.
000264
000265     IF CP-STATE-STD-ABBRV = 'TX'
000266       IF CP-COMPANY-ID EQUAL 'HER' OR 'NCL'
000267          NEXT SENTENCE
000268       END-IF
000269        IF DC-GREG-DATE-CYMD GREATER 19920629  AND
000270           CP-ORIGINAL-TERM NOT GREATER THAN 120
000271           GO TO 1021-ANTICIPATION-REF.
000272
000273 SKIP-CSO-TEXAS-LIFE-PROCESSING.
000274
000275     IF CP-EARN-BY-PRORATA
000276         GO TO 1023-REF-PR.
000277
000278     IF CP-EARN-AS-MEAN
000279         GO TO 1025-REF-MEAN.
000280
000281     IF CP-EARN-AS-TEXAS
000282         GO TO 1027-TEXAS-REFUND.
000283
000284     IF CP-EARN-AS-NET-PAY
000285         GO TO 1028-NET-PAY-REFUND.
000286
000287     IF CP-LIFE-OVERRIDE-CODE NOT = 'L'
000288         GO TO 1000-NO-NET-PAY.
000289
000290     IF  CP-COMPANY-ID IS EQUAL TO 'CID' OR 'CSO'
000291       IF CP-STATE-STD-ABBRV = 'NC'
000292         IF CP-REDUCING-LIFE
000293           IF CP-ORIGINAL-TERM GREATER 60  AND
000294              DC-GREG-DATE-CYMD GREATER 19810930
000295                GO TO 1028-NET-PAY-REFUND
000296           ELSE
000297              GO TO 1026-REF-R78.
000298
000299
000300     IF CP-STATE-STD-ABBRV = 'NC'
000301       IF CP-COMPANY-ID EQUAL 'NCL'
000302          NEXT SENTENCE
000303       END-IF
000304       IF CP-REDUCING-LIFE
000305         IF CP-ORIGINAL-TERM GREATER 60  AND
000306            DC-GREG-DATE-CYMD GREATER 19810930
000307             GO TO 1021-ANTICIPATION-REF
000308         ELSE
000309             GO TO 1026-REF-R78.
000310
000311     IF CP-STATE-STD-ABBRV = 'UT'
000312       IF CP-COMPANY-ID EQUAL 'NCL'
000313          NEXT SENTENCE
000314       END-IF
000315         IF CP-ORIGINAL-TERM GREATER 62         AND
000316            CP-LOAN-APR GREATER ZERO            AND
000317            DC-GREG-DATE-CYMD GREATER 19810831 AND
000318            DC-GREG-DATE-CYMD LESS    19830901
000319             GO TO 1028-NET-PAY-REFUND
000320         ELSE
000321             GO TO 1026-REF-R78.
000322
000323     IF CP-STATE-STD-ABBRV = 'OH'
000324       IF CP-COMPANY-ID EQUAL 'NCL' OR 'HER'
000325          NEXT SENTENCE
000326       END-IF
000327           IF CP-ORIGINAL-TERM GREATER 60         AND
000328              CP-CLASS-CODE NOT = 'L '            AND
000329              CP-LOAN-APR GREATER ZERO            AND
000330              DC-GREG-DATE-CYMD GREATER 19831031
000331              GO TO 1028-NET-PAY-REFUND
000332           ELSE
000333              GO TO 1026-REF-R78.
000334
000335     IF CP-COMPANY-ID EQUAL 'CID' OR 'CSO'
000336       IF CP-STATE-STD-ABBRV = 'MT'
000337         GO TO SKIP-CSO-MT-LF-PROCESSING.
000338
000339
000340     IF CP-STATE-STD-ABBRV = 'MT'
000341       IF CP-COMPANY-ID EQUAL 'NCL'
000342          NEXT SENTENCE
000343       END-IF
000344         IF CP-ORIGINAL-TERM GREATER 61         AND
000345            CP-LOAN-APR GREATER ZERO            AND
000346            DC-GREG-DATE-CYMD GREATER 19830318
000347             GO TO 1028-NET-PAY-REFUND
000348         ELSE
000349             GO TO 1026-REF-R78.
000350
000351 SKIP-CSO-MT-LF-PROCESSING.
000352
000353     IF CP-STATE-STD-ABBRV = 'RI'
000354       IF CP-COMPANY-ID EQUAL 'NCL'
000355          NEXT SENTENCE
000356       END-IF
000357         IF CP-ORIGINAL-TERM GREATER 60         AND
000358            CP-LOAN-APR GREATER ZERO            AND
000359            DC-GREG-DATE-CYMD GREATER 19831231
000360             GO TO 1028-NET-PAY-REFUND
000361         ELSE
000362             GO TO 1026-REF-R78.
000363
000364
000365 1000-NO-NET-PAY.
000366
000367     IF CP-STATE-STD-ABBRV = 'WY'
000368       IF CP-COMPANY-ID EQUAL 'NCL'
000369          NEXT SENTENCE
000370       END-IF
000371        IF CP-COMPANY-ID = 'TMS'
000372           GO TO 1026-REF-R78
000373        ELSE
000374           GO TO 1023-REF-PR.
000375
000376     IF CP-EARN-BY-R78
000377         GO TO 1026-REF-R78.
000378
000379     IF CP-EARN-AS-SUM-OF-DIGITS
000380         GO TO 1075-SUM-OF-DIGITS.
000381
000382     IF CP-LEVEL-LIFE
000383         GO TO 1023-REF-PR
000384     ELSE
000385         GO TO 1026-REF-R78.
000386
000387*-----------------------------------------------------------------
000388 1021-ANTICIPATION-REF.
000389     MOVE '6'                  TO CP-REFUND-TYPE-USED.
000390
000391     MOVE CP-LOAN-TERM         TO WS-SAVE-LOAN-TERM.
000392     IF CP-LOAN-TERM = ZEROS
000393        MOVE CP-ORIGINAL-TERM TO CP-LOAN-TERM
000394     END-IF
000395
000396     COMPUTE CP-LOAN-TERM = CP-LOAN-TERM - CP-ORIGINAL-TERM +
000397                            CP-REMAINING-TERM.
000398
000399     MOVE CP-ORIGINAL-BENEFIT  TO WS-SAVE-BENEFIT.
000400     MOVE CP-ORIGINAL-TERM     TO WS-SAVE-TERM.
000401     MOVE CP-EARNING-METHOD    TO WS-SAVE-EARN-METHOD.
000402     move cp-term-or-ext-days  to ws-save-ext-days
000403
000404     MOVE CP-REMAINING-BENEFIT TO CP-ORIGINAL-BENEFIT.
000405     MOVE CP-REMAINING-TERM    TO CP-ORIGINAL-TERM.
000406     MOVE CP-RATING-METHOD     TO CP-EARNING-METHOD.
000407
000408     IF CP-REMAINING-TERM = WS-SAVE-TERM
000409         MOVE 'N'              TO CP-ROA-REFUND
000410     ELSE
000411         MOVE 'Y'              TO CP-ROA-REFUND.
000412
000413******    TO COMPUTE PREMIUM AT REFUND DATE
000414
000415     PERFORM 2500-GET-RATE.
000416
000417     MOVE 'N'                  TO CP-ROA-REFUND.
000418     MOVE WS-SAVE-BENEFIT      TO CP-ORIGINAL-BENEFIT.
000419     MOVE WS-SAVE-TERM         TO CP-ORIGINAL-TERM.
000420     MOVE WS-SAVE-LOAN-TERM    TO CP-LOAN-TERM.
000421     move ws-save-ext-days     to cp-term-or-ext-days
000422
000423     IF CP-PREMIUM-RATE = ZEROS
000424        MOVE '7'                 TO CP-RETURN-CODE
000425        MOVE WS-SAVE-EARN-METHOD TO CP-EARNING-METHOD
000426        GO TO 1999-CALC-REF-AMT-X.
000427
000428     MOVE CP-CALC-PREMIUM         TO WS-REMAIN-PREM.
000429
000430******    TO COMPUTE PREMIUM AT ORIGINAL ISSUE DATE
000431
000432     PERFORM 2500-GET-RATE.
000433
000434     MOVE WS-SAVE-EARN-METHOD     TO CP-EARNING-METHOD.
000435
000436     IF CP-PREMIUM-RATE = ZEROS
000437        MOVE '7'            TO CP-RETURN-CODE
000438        GO TO 1999-CALC-REF-AMT-X.
000439
000440     MOVE CP-CALC-PREMIUM         TO WS-ORIG-PREM.
000441
000442     IF  WS-ORIG-PREM = ZEROS
000443         MOVE ZEROS TO WS-NP-REF-FACTOR
000444        ELSE
000445         COMPUTE WS-NP-REF-FACTOR ROUNDED = WS-REMAIN-PREM /
000446                                            WS-ORIG-PREM.
000447
000448     COMPUTE CP-CALC-REFUND ROUNDED = WS-NP-REF-FACTOR *
000449                                        CP-ORIGINAL-PREMIUM.
000450
000451     GO TO 1999-CALC-REF-AMT-X.
000452
000453*---------------------------------------------------------------
000454 1022-REF-CRITICAL-PERIOD-LF.
000455     GO TO 1023-REF-PR.
000456
000457*---------------------------------------------------------------
000458 1023-REF-PR.
000459     MOVE '2'               TO CP-REFUND-TYPE-USED.
000460     COMPUTE CP-CALC-REFUND ROUNDED =
000461            CP-ORIGINAL-PREMIUM * PR-FACTOR.
000462
000463     GO TO 1999-CALC-REF-AMT-X.
000464
000465*---------------------------------------------------------------
000466 1024-REF-PR-DAYS.
000467     MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1.
000468     MOVE CP-VALUATION-DT    TO DC-BIN-DATE-2.
000469     MOVE '1'                TO DC-OPTION-CODE.
000470     PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
000471
000472     IF DATE-CONVERSION-ERROR
000473         MOVE '2' TO CP-RETURN-CODE
000474         GO TO 1999-CALC-REF-AMT-X.
000475
000476     COMPUTE CP-CALC-REFUND ROUNDED =
000477         (CP-ORIGINAL-PREMIUM / CP-TERM-OR-EXT-DAYS) *
000478                 (CP-TERM-OR-EXT-DAYS - DC-ELAPSED-DAYS).
000479
000480     GO TO 1999-CALC-REF-AMT-X.
000481
000482*---------------------------------------------------------------
000483 1025-REF-MEAN.
000484     MOVE '8'               TO CP-REFUND-TYPE-USED.
000485     COMPUTE CP-CALC-REFUND ROUNDED =
000486          ((CP-ORIGINAL-PREMIUM * PR-FACTOR) +
000487           (CP-ORIGINAL-PREMIUM * R78-FACTOR)) * +.5.
000488
000489     GO TO 1999-CALC-REF-AMT-X.
000490
000491*---------------------------------------------------------------
000492 1026-REF-R78.
000493     MOVE '1'               TO CP-REFUND-TYPE-USED.
000494*    IF CP-TERM-TIMES-TERM
000495*        COMPUTE R78-FACTOR =
000496*            (CP-REMAINING-TERM * CP-REMAINING-TERM) /
000497*               (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1)).
000498*
000499     COMPUTE CP-CALC-REFUND ROUNDED =
000500        CP-ORIGINAL-PREMIUM * R78-FACTOR.
000501
000502     GO TO 1999-CALC-REF-AMT-X.
000503
000504*---------------------------------------------------------------
000505 1027-TEXAS-REFUND.
000506     MOVE '4'               TO CP-REFUND-TYPE-USED.
000507     IF CP-PAY-FREQUENCY = ZERO
000508        MOVE ZERO TO CP-CALC-REFUND
000509        MOVE '5'  TO CP-RETURN-CODE
000510        GO TO 1999-CALC-REF-AMT-X.
000511
000512     COMPUTE TEX-FACT-4 = (CP-ORIGINAL-TERM * CP-ORIGINAL-TERM) +
000513         (CP-PAY-FREQUENCY * CP-ORIGINAL-TERM).
000514
000515     DIVIDE CP-REMAINING-TERM BY CP-PAY-FREQUENCY
000516         GIVING TEX-FACT-5
000517             REMAINDER TEX-FACT-6.
000518
000519     COMPUTE TEX-FACT-5 = TEX-FACT-5 * CP-PAY-FREQUENCY.
000520
000521     COMPUTE TEX-FACT-7 = (TEX-FACT-5 * TEX-FACT-5) +
000522         (TEX-FACT-5 * CP-PAY-FREQUENCY) +
000523         (2 * (TEX-FACT-6 * (TEX-FACT-5 + CP-PAY-FREQUENCY))).
000524
000525     COMPUTE TEX-FACT-8 ROUNDED = TEX-FACT-7 / TEX-FACT-4.
000526
000527     COMPUTE CP-CALC-REFUND ROUNDED = CP-ORIGINAL-PREMIUM
000528                                        * TEX-FACT-8.
000529
000530     GO TO 1999-CALC-REF-AMT-X.
000531
000532*---------------------------------------------------------------
000533 1028-NET-PAY-REFUND.
000534
000535     MOVE '5'               TO CP-REFUND-TYPE-USED.
000536     IF CP-LOAN-APR = ZERO
000537        move '1'                 to cp-refund-type-used
000538        compute cp-calc-refund rounded =
000539           cp-original-premium * r78-factor
000540        go to 1999-calc-ref-amt-x
000541     end-if
000542
000543*       MOVE ZERO TO CP-CALC-REFUND
000544*       MOVE '1'  TO CP-RETURN-CODE
000545*       GO TO 1999-CALC-REF-AMT-X.
000546
000547     MOVE CP-LOAN-APR           TO N-P-APR.
000548     MOVE CP-ORIGINAL-TERM      TO N-P-ORIG  N-P-LOAN.
000549     MOVE CP-REMAINING-TERM     TO N-P-REM.
000550
000551     IF CP-TRUNCATED-LIFE
000552        MOVE CP-LOAN-TERM     TO N-P-LOAN.
000553
000554     MOVE 'R'                   TO N-P-OPT.
000555
000556     PERFORM 10000-NET-TERM THRU 99999-EXIT.
000557
000558     COMPUTE CP-CALC-REFUND ROUNDED = N-P-FACTOR *
000559                                        CP-ORIGINAL-PREMIUM.
000560
000561 1029-UTAH-REFUND.
000562*
000563*---------------------------------------------------------------
000564* FOLLOWING IS FOR TRISH - 10/26/98  (FOR UTAH ONLY).
000565*---------------------------------------------------------------
000566*
000567     GO TO 1999-CALC-REF-AMT-X.
000568*
000569*---------------------------------------------------------------
000570     IF (CP-STATE-STD-ABBRV NOT = 'UT')
000571                      OR
000572        (CP-COMPANY-ID EQUAL 'NCL')
000573         GO TO 1999-CALC-REF-AMT-X.
000574     MOVE '3'               TO CP-REFUND-TYPE-USED.
000575
000576     IF (CP-ORIGINAL-TERM - CP-REMAINING-TERM)
000577        GREATER ZERO AND LESS 13
000578         COMPUTE CP-CALC-REFUND = CP-CALC-REFUND -
000579                 (CP-CALC-REFUND * .05).
000580
000581     IF (CP-ORIGINAL-TERM - CP-REMAINING-TERM)
000582        GREATER 12 AND LESS 25
000583         COMPUTE CP-CALC-REFUND = CP-CALC-REFUND -
000584                 (CP-CALC-REFUND * .025).
000585
000586     GO TO 1999-CALC-REF-AMT-X.
000587*---------------------------------------------------------------
000588     EJECT
000589*---------------------------------------------------------------
000590 1030-GET-AH-REFUND.
000591     COMPUTE PR-FACTOR = CP-REMAINING-TERM / CP-ORIGINAL-TERM.
000592
000593     COMPUTE R78-FACTOR =
000594            ((CP-REMAINING-TERM * (CP-REMAINING-TERM + 1)) /
000595                (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1))).
000596
000597*    IF CP-TERM-TIMES-TERM
000598*        COMPUTE R78-FACTOR =
000599*            (CP-REMAINING-TERM * CP-REMAINING-TERM) /
000600*            (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1)).
000601*
000602
000603**** THE FOLLOWING IF STATEMENT FOR NCL ONLY SHOULD DO THE
000604**** FOLLOWING.
000605****       FOR VA. DISABILITY
000606****                         --- EFF DATE > 123192 AND
000607****                             TERM > 61 USE RULE OF ANTIC
000608****                             OTHERWISE USE RULE OF 78.
000609****
000610****    NOTE... PER NCL WE SHOULD NOT INCLUDE THEM IN ANY
000611****            OF THE SYSTEM DEFAULT STATUTORY REFUND RULES.
000612****            NCL WILL SUPPLY US WITH THE STATUTORY RULES THEY
000613****            WILL FOLLOW.
000614****
000615****
000616****
000617     IF CP-COMPANY-ID = 'NCL' or 'AHL'
000618        IF CP-STATE-STD-ABBRV = 'VA'
000619           IF (DC-GREG-DATE-CYMD GREATER 19921231) AND
000620              (CP-ORIGINAL-TERM GREATER THAN 61)
000621               MOVE '6'        TO CP-EARNING-METHOD
000622               GO TO 1060-ANTICIPATION-REF
000623            ELSE
000624               MOVE '1'        TO CP-EARNING-METHOD
000625               GO TO 1050-R78-REF.
000626
000627     IF CP-STATE-STD-ABBRV = 'WY'
000628        IF CP-COMPANY-ID = 'TMS'
000629           GO TO 1050-R78-REF.
000630
000631     IF CP-COMPANY-ID = 'NCL' OR 'WDS' OR 'CID' OR 'DCC'
000632                     OR 'AHL' OR 'VPP'
000633         NEXT SENTENCE
000634     ELSE
000635         IF CP-CRITICAL-PERIOD
000636             GO TO 1035-REF-AH-CRITICAL-PERIOD.
000637
000638
000639*      THE FOLLOWING HAPPENS WHEN THE REFUND METHOD FOR THE
000640*    BENEFIT CODE IS A NON REFUNDABLE METHOD
000641*    I AM CHECKING THE TERM AND REM TERM BECAUSE OF THE 30
000642*    DAY FREE LOOK PERIOD. ALSO, 55 AND 56 ARE THE BIU CODES
000643
000644     IF (CP-GAP-NON-REFUNDABLE)
000645        IF (CP-ORIGINAL-TERM NOT = CP-REMAINING-TERM)
000646           OR (CP-BENEFIT-CD = '55' OR '56')
000647           MOVE ZEROS            TO CP-CALC-REFUND
000648           MOVE 'G'              TO CP-REFUND-TYPE-USED
000649           GO TO 1999-CALC-REF-AMT-X
000650        END-IF
000651     END-IF
000652
000653     IF CP-GAP-ACTUARIAL
000654         GO TO 1070-NET-PAY-REFUND
000655     END-IF
000656
000657     IF (CP-COMPANY-ID = 'CID')
000658        AND (CP-STATE-STD-ABBRV = 'MN')
000659        AND (DC-GREG-DATE-CYMD > 20091231)
000660        IF (CP-CRITICAL-PERIOD)
000661           GO TO 1040-PR-REF
000662        ELSE
000663           GO TO 1055-MEAN-REF
000664     END-IF
000665
000666     IF CP-COMPANY-ID EQUAL 'CID' OR 'DCC' OR 'VPP'
000667        IF CP-STATE-STD-ABBRV = 'NC'
000668           GO TO SKIP-CSO-NC-AH-PROCESSING.
000669
000670     IF (CP-COMPANY-ID = 'CID')
000671        AND (CP-STATE-STD-ABBRV = 'IN')
000672        AND (DC-GREG-DATE-CYMD < 20030401)
000673        GO TO 1026-REF-R78
000674     END-IF
000675
000676     IF (CP-COMPANY-ID = 'CID')
000677        AND (CP-STATE-STD-ABBRV = 'FL')
000678        AND (DC-GREG-DATE-CYMD > 20030630)
000679        AND (CP-CRITICAL-PERIOD)
000680        GO TO 1040-PR-REF
000681     END-IF
000682
000683     IF (CP-COMPANY-ID = 'CID')
000684        AND (CP-STATE-STD-ABBRV = 'MO')
000685        AND (DC-GREG-DATE-CYMD > 20030630)
000686        AND (CP-CRITICAL-PERIOD)
000687        GO TO 1040-PR-REF
000688     END-IF
000689
000690     IF (CP-COMPANY-ID = 'CID')
000691        AND (CP-STATE-STD-ABBRV = 'ND')
000692        AND (CP-CRITICAL-PERIOD)
000693        AND (DC-GREG-DATE-CYMD > 20011231)
000694        GO TO 1040-PR-REF
000695     END-IF
000696
000697     IF (CP-COMPANY-ID = 'CID')
000698        AND (CP-STATE-STD-ABBRV = 'ND')
000699        AND (DC-GREG-DATE-CYMD > 20011231)
000700        GO TO 1060-ANTICIPATION-REF
000701     END-IF
000702
000703     IF CP-STATE-STD-ABBRV = 'NC'
000704       IF CP-COMPANY-ID = 'NCL' OR 'NCB'
000705          NEXT SENTENCE
000706       END-IF
000707         IF DC-GREG-DATE-CYMD GREATER 19810930
000708            IF CP-EARN-ANTICIPATION
000709                GO TO 1060-ANTICIPATION-REF
000710            ELSE
000711                GO TO 1055-MEAN-REF.
000712
000713 SKIP-CSO-NC-AH-PROCESSING.
000714
000715     IF CP-STATE-STD-ABBRV = 'NM'
000716       IF CP-COMPANY-ID = 'NCL' OR 'DCC' OR 'AHL' OR 'VPP'
000717          NEXT SENTENCE
000718       END-IF
000719         IF DC-GREG-DATE-CYMD GREATER THAN 19871231
000720            IF CP-COMPANY-ID = 'MON'
000721              MOVE '6'             TO CP-EARNING-METHOD
000722              GO TO 1060-ANTICIPATION-REF
000723             ELSE
000724              GO TO 1055-MEAN-REF.
000725
000726*    IF CP-STATE-STD-ABBRV = 'CT'
000727*      IF CP-COMPANY-ID EQUAL 'NCL' OR 'DCC' OR 'VPP'
000728*         NEXT SENTENCE
000729*      END-IF
000730*         MOVE '6'              TO CP-EARNING-METHOD
000731*         GO TO 1060-ANTICIPATION-REF.
000732
000733     IF CP-STATE-STD-ABBRV = 'ME'
000734       IF CP-COMPANY-ID = 'NCL' OR 'CVL' OR 'DCC' OR 'VPP'
000735          NEXT SENTENCE
000736       END-IF
000737           IF CP-EARN-BY-PRORATA
000738              GO TO 1040-PR-REF
000739           ELSE
000740              MOVE '6'           TO CP-EARNING-METHOD
000741              GO TO 1060-ANTICIPATION-REF.
000742
000743
000744     IF CP-COMPANY-ID = 'CID' OR 'DCC' OR 'VPP'
000745        IF CP-STATE-STD-ABBRV = 'TX' OR 'VA'
000746            GO TO SKIP-CSO-TX-VA-AH-PROCESSING.
000747
000748     IF CP-STATE-STD-ABBRV = 'TX'
000749       IF CP-COMPANY-ID = 'NCL' OR 'HER'
000750          NEXT SENTENCE
000751       END-IF
000752        IF DC-GREG-DATE-CYMD GREATER 19920629  AND
000753           CP-ORIGINAL-TERM NOT GREATER THAN 120
000754           MOVE '6'              TO CP-EARNING-METHOD
000755           GO TO 1060-ANTICIPATION-REF.
000756
000757     IF CP-STATE-STD-ABBRV = 'VA'
000758       IF CP-COMPANY-ID EQUAL 'NCL'
000759          NEXT SENTENCE
000760       END-IF
000761       IF CP-COMPANY-ID = 'NCB'
000762           MOVE '6'                TO CP-EARNING-METHOD
000763           GO TO 1060-ANTICIPATION-REF
000764       ELSE
000765           IF DC-GREG-DATE-CYMD GREATER 19921231
000766               IF CP-ORIGINAL-TERM GREATER THAN 61
000767                   MOVE '6'        TO CP-EARNING-METHOD
000768                   GO TO 1060-ANTICIPATION-REF
000769               ELSE
000770                   MOVE '1'        TO CP-EARNING-METHOD
000771                   GO TO 1050-R78-REF.
000772
000773 SKIP-CSO-TX-VA-AH-PROCESSING.
000774
000775     IF CP-EARN-BY-PRORATA
000776         GO TO 1040-PR-REF.
000777
000778     IF CP-EARN-BY-R78
000779         GO TO 1050-R78-REF.
000780
000781     IF CP-EARN-AS-MEAN
000782        GO TO 1055-MEAN-REF.
000783
000784     IF CP-EARN-AS-SUM-OF-DIGITS
000785         GO TO 1075-SUM-OF-DIGITS.
000786
000787     IF CP-DCC-SPP-DDF
000788        GO TO 1080-DCC-DDF-SPECIAL
000789     END-IF
000790
000791     IF CP-EARN-AS-CALIF
000792         GO TO 1060-ANTICIPATION-REF.
000793
000794     IF CP-EARN-ANTICIPATION
000795         GO TO 1060-ANTICIPATION-REF.
000796
000797     IF CP-EARN-AS-NET-PAY
000798         GO TO 1070-NET-PAY-REFUND.
000799
000800     GO TO 1050-R78-REF.
000801
000802*---------------------------------------------------------------
000803 1035-REF-AH-CRITICAL-PERIOD.
000804     IF CP-REMAINING-TERM GREATER +6
000805        COMPUTE CP-CALC-REFUND ROUNDED =
000806          CP-ORIGINAL-PREMIUM * (((CP-REMAINING-TERM * +2) - +5) /
000807                       ((CP-ORIGINAL-TERM * +2) - + 5))
000808      ELSE
000809        COMPUTE CP-CALC-REFUND ROUNDED =
000810          CP-ORIGINAL-PREMIUM * ((CP-REMAINING-TERM *
000811                                           CP-REMAINING-TERM + 1)
000812                              /  (+12 * CP-ORIGINAL-TERM) - +30).
000813
000814     IF CP-CALC-REFUND LESS ZERO
000815         MOVE ZERO TO CP-CALC-REFUND.
000816
000817     GO TO 1999-CALC-REF-AMT-X.
000818
000819*---------------------------------------------------------------
000820 1040-PR-REF.
000821     MOVE '2'               TO CP-REFUND-TYPE-USED.
000822     COMPUTE CP-CALC-REFUND = CP-ORIGINAL-PREMIUM * PR-FACTOR.
000823
000824     GO TO 1999-CALC-REF-AMT-X.
000825
000826*---------------------------------------------------------------
000827 1050-R78-REF.
000828     MOVE '1'               TO CP-REFUND-TYPE-USED.
000829     COMPUTE CP-CALC-REFUND ROUNDED =
000830           CP-ORIGINAL-PREMIUM * R78-FACTOR.
000831
000832     GO TO 1999-CALC-REF-AMT-X.
000833
000834*---------------------------------------------------------------
000835 1055-MEAN-REF.
000836     MOVE '8'               TO CP-REFUND-TYPE-USED.
000837     COMPUTE CP-CALC-REFUND ROUNDED =
000838         ((CP-ORIGINAL-PREMIUM * R78-FACTOR) +
000839          (CP-ORIGINAL-PREMIUM * PR-FACTOR)) * .5.
000840
000841     GO TO 1999-CALC-REF-AMT-X.
000842
000843*---------------------------------------------------------------
000844 1060-ANTICIPATION-REF.
000845     MOVE '6'               TO CP-REFUND-TYPE-USED.
000846
000847     MOVE CP-ORIGINAL-TERM  TO WS-SAVE-TERM.
000848     MOVE CP-LOAN-TERM      TO WS-SAVE-LOAN-TERM.
000849     MOVE CP-EARNING-METHOD TO WS-SAVE-EARN-METHOD.
000850
000851     MOVE CP-REMAINING-TERM TO CP-ORIGINAL-TERM
000852                               CP-LOAN-TERM.
000853     MOVE CP-RATING-METHOD  TO CP-EARNING-METHOD.
000854
000855     PERFORM 2500-GET-RATE.
000856
000857     MOVE WS-SAVE-TERM      TO CP-ORIGINAL-TERM.
000858     MOVE WS-SAVE-LOAN-TERM TO CP-LOAN-TERM.
000859
000860     IF CP-PREMIUM-RATE = ZEROS
000861        MOVE '7'                 TO CP-RETURN-CODE
000862        MOVE WS-SAVE-EARN-METHOD TO CP-EARNING-METHOD
000863        GO TO 1999-CALC-REF-AMT-X.
000864
000865     MOVE CP-CALC-PREMIUM         TO WS-REMAIN-PREM.
000866
000867     PERFORM 2500-GET-RATE.
000868
000869     MOVE WS-SAVE-EARN-METHOD     TO CP-EARNING-METHOD.
000870
000871     IF CP-PREMIUM-RATE = ZEROS
000872        MOVE '7'            TO CP-RETURN-CODE
000873        GO TO 1999-CALC-REF-AMT-X.
000874
000875     MOVE CP-CALC-PREMIUM         TO WS-ORIG-PREM.
000876
000877     IF  WS-ORIG-PREM = ZERO
000878         MOVE ZERO                TO WS-NP-REF-FACTOR
000879        ELSE
000880         COMPUTE WS-NP-REF-FACTOR ROUNDED = WS-REMAIN-PREM /
000881                                            WS-ORIG-PREM.
000882
000883     COMPUTE CP-CALC-REFUND ROUNDED = WS-NP-REF-FACTOR *
000884                                        CP-ORIGINAL-PREMIUM.
000885
000886     IF NOT CP-EARN-AS-CALIF
000887         GO TO 1999-CALC-REF-AMT-X.
000888
000889*---------------------------------------------------------------
000890 1065-CALIF-REGS.
000891     MOVE '3'               TO CP-REFUND-TYPE-USED.
000892     SUBTRACT CP-REMAINING-TERM FROM CP-ORIGINAL-TERM
000893         GIVING FACTOR-5.
000894
000895     IF FACTOR-5 NOT GREATER +6  AND
000896        CP-CLASS-CODE = 'A '
000897         GO TO 1050-R78-REF.
000898
000899     IF FACTOR-5 = ZERO
000900         MOVE CP-ORIGINAL-PREMIUM   TO CP-CALC-REFUND
000901         GO TO 1999-CALC-REF-AMT-X.
000902
000903     IF CP-ORIGINAL-TERM LESS +014
000904         GO TO 1999-CALC-REF-AMT-X.
000905
000906     MULTIPLY CP-ORIGINAL-TERM BY CP-ORIGINAL-TERM
000907                                   GIVING FACTOR-5 ROUNDED.
000908
000909     MULTIPLY FACTOR-5 BY +0.0000045573 GIVING FACTOR-1 ROUNDED.
000910
000911     MULTIPLY +0.001125 BY CP-ORIGINAL-TERM
000912                                   GIVING FACTOR-2 ROUNDED.
000913
000914     SUBTRACT FACTOR-2 FROM FACTOR-1.
000915     ADD +0.099375 TO FACTOR-1.
000916
000917     SUBTRACT CP-REMAINING-TERM FROM CP-ORIGINAL-TERM
000918                                   GIVING FACTOR-5 ROUNDED.
000919
000920     SUBTRACT +1.00 FROM FACTOR-5.
000921
000922     IF CP-ORIGINAL-TERM LESS +049
000923         SUBTRACT +013 FROM CP-ORIGINAL-TERM
000924                                   GIVING FACTOR-6 ROUNDED
000925     ELSE
000926         MOVE +035 TO FACTOR-6.
000927
000928     MOVE +001 TO FACTOR-2.
000929
000930     DIVIDE FACTOR-5 BY FACTOR-6 GIVING FACTOR-4 ROUNDED.
000931     SUBTRACT FACTOR-4 FROM FACTOR-2.
000932     MULTIPLY FACTOR-1 BY FACTOR-2 GIVING FACTOR-3 ROUNDED.
000933     MOVE +001 TO FACTOR-2.
000934     SUBTRACT FACTOR-3 FROM FACTOR-2 GIVING FACTOR-1 ROUNDED.
000935
000936     MULTIPLY FACTOR-1 BY CP-CALC-REFUND
000937         GIVING CAL-RFND ROUNDED.
000938
000939     IF CAL-RFND LESS +0.01
000940         MOVE ZEROS TO CAL-RFND.
000941
000942     IF CAL-RFND LESS CP-CALC-REFUND
000943         MOVE CAL-RFND TO CP-CALC-REFUND.
000944
000945     GO TO 1999-CALC-REF-AMT-X.
000946
000947*-----------------------------------------------------------------
000948 1070-NET-PAY-REFUND.
000949     MOVE '5'                    TO  CP-REFUND-TYPE-USED.
000950     IF CP-LOAN-APR = ZERO
000951         MOVE ZEROS              TO  CP-CALC-REFUND
000952         MOVE '1'                TO  CP-RETURN-CODE
000953         GO TO 1999-CALC-REF-AMT-X.
000954
000955     MOVE CP-LOAN-APR            TO  N-P-APR.
000956     MOVE CP-ORIGINAL-TERM       TO  N-P-ORIG  N-P-LOAN.
000957     MOVE CP-REMAINING-TERM      TO  N-P-REM.
000958
000959     MOVE 'R'                    TO  N-P-OPT.
000960
000961     PERFORM 10000-NET-TERM THRU 99999-EXIT.
000962
000963     COMPUTE CP-CALC-REFUND ROUNDED = N-P-FACTOR *
000964                                        CP-ORIGINAL-PREMIUM.
000965
000966     GO TO 1999-CALC-REF-AMT-X.
000967*---------------------------------------------------------------
000968
000969 1075-SUM-OF-DIGITS.
000970
000971     MOVE '9'               TO CP-REFUND-TYPE-USED.
000972
000973     COMPUTE RSUM-REMAINING-TERM =
000974             CP-LOAN-TERM -
000975                  (CP-ORIGINAL-TERM - (CP-REMAINING-TERM + 1))
000976
000977     COMPUTE RSUM-FACTOR =
000978        ((RSUM-REMAINING-TERM * (RSUM-REMAINING-TERM + 1)) -
000979               ((CP-LOAN-TERM - CP-ORIGINAL-TERM)          *
000980                 (CP-LOAN-TERM - CP-ORIGINAL-TERM + 1)))   /
000981            ((CP-LOAN-TERM * (CP-LOAN-TERM + 1))           -
000982               ((CP-LOAN-TERM - CP-ORIGINAL-TERM)          *
000983                 (CP-LOAN-TERM - CP-ORIGINAL-TERM  + 1))).
000984
000985     COMPUTE CP-CALC-REFUND ROUNDED =
000986           CP-ORIGINAL-PREMIUM * RSUM-FACTOR.
000987
000988     GO TO 1999-CALC-REF-AMT-X
000989
000990     .
000991 1080-DCC-DDF-SPECIAL.
000992
000993     COMPUTE CP-MONTH =
000994        FUNCTION REM(CP-ORIGINAL-TERM - CP-REMAINING-TERM, 12)
000995*    DISPLAY ' BEGIN ELCRFNDP ************** '
000996*    DISPLAY ' SPEC       ' CP-DDF-SPEC-CALC
000997*    DISPLAY ' MONTH      ' CP-MONTH
000998*    DISPLAY ' ORIG TERM  ' CP-ORIGINAL-TERM
000999*    DISPLAY ' REM TERM   ' CP-REMAINING-TERM
001000*    DISPLAY ' ORIG PREM  ' CP-ORIGINAL-PREMIUM
001001*    DISPLAY ' 1YR EXP AL ' CP-1ST-YR-ALLOW
001002*    DISPLAY ' HI FACT    ' CP-DDF-HI-FACT
001003*    DISPLAY ' LO FACT    ' CP-DDF-LO-FACT
001004*    DISPLAY ' RATE UP    ' CP-IU-RATE-UP
001005*    DISPLAY ' CLP RAT UP ' CP-CLP-RATE-UP
001006*    DISPLAY ' EARM METH  ' CP-EARNING-METHOD
001007*    DISPLAY '   END ELCRFNDP ************** '
001008
001009     IF CP-MONTH = 0
001010        MOVE 12                  TO CP-MONTH
001011     END-IF
001012     MOVE CP-MONTH               TO WS-MONTH
001013
001014     MOVE 'D'                    TO CP-REFUND-TYPE-USED
001015
001016     IF CP-DCC-SPP-DDF-IU
001017        GO TO 1090-DCC-DDF-IU
001018     END-IF
001019
001020     IF CP-CALC-CLP
001021        GO TO 1085-DCC-DDF-SPECIAL-CLP
001022     END-IF
001023
001024     EVALUATE TRUE
001025        WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
001026           OR (CP-ORIGINAL-PREMIUM = ZEROS)
001027           MOVE CP-ORIGINAL-PREMIUM
001028                                 TO CP-CALC-REFUND
001029
001030        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
001031           MOVE CP-MONTH         TO WS-MONTH
001032           PERFORM 1150-UE-GROSS-LT-4
001033           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001034
001035        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
001036           MOVE CP-MONTH         TO WS-MONTH
001037           PERFORM 1160-UE-GROSS-LT-13
001038           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001039
001040        WHEN OTHER
001041           MOVE CP-MONTH         TO WS-MONTH
001042           PERFORM 1170-UE-GROSS-GT-12
001043           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001044     END-EVALUATE
001045
001046     GO TO 1999-CALC-REF-AMT-X
001047
001048     .
001049 1085-DCC-DDF-SPECIAL-CLP.
001050
001051     EVALUATE TRUE
001052        WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
001053           OR (CP-ORIGINAL-PREMIUM = ZEROS)
001054           MOVE CP-ORIGINAL-PREMIUM
001055                                 TO CP-CALC-REFUND
001056
001057        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
001058           MOVE CP-MONTH         TO WS-MONTH
001059           PERFORM 1100-UE-CLP-LT-4
001060           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001061
001062        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
001063           MOVE CP-MONTH         TO WS-MONTH
001064           PERFORM 1110-UE-CLP-LT-13
001065           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001066
001067        WHEN OTHER
001068           MOVE CP-MONTH         TO WS-MONTH
001069           PERFORM 1120-UE-CLP-GT-12
001070           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001071     END-EVALUATE
001072
001073     GO TO 1999-CALC-REF-AMT-X
001074
001075     .
001076 1090-DCC-DDF-IU.
001077
001078     MOVE 'I'                    TO CP-REFUND-TYPE-USED
001079
001080     IF CP-CALC-CLP
001081        GO TO 1095-DCC-DDF-IU-SPECIAL-CLP
001082     END-IF
001083
001084     EVALUATE TRUE
001085        WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
001086           OR (CP-ORIGINAL-PREMIUM = ZEROS)
001087           MOVE CP-ORIGINAL-PREMIUM
001088                                 TO CP-CALC-REFUND
001089
001090        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
001091           MOVE CP-MONTH         TO WS-MONTH
001092           PERFORM 1250-IU-UE-GROSS-LT-4
001093           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001094
001095        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
001096           MOVE CP-MONTH         TO WS-MONTH
001097           PERFORM 1260-IU-UE-GROSS-LT-13
001098           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001099
001100        WHEN OTHER
001101           MOVE CP-MONTH         TO WS-MONTH
001102           PERFORM 1270-IU-UE-GROSS-GT-12
001103           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001104
001105     END-EVALUATE
001106
001107     GO TO 1999-CALC-REF-AMT-X
001108
001109     .
001110 1095-DCC-DDF-IU-SPECIAL-CLP.
001111
001112     EVALUATE TRUE
001113        WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
001114           OR (CP-DDF-CLP = ZEROS)
001115           MOVE CP-DDF-CLP       TO CP-CALC-REFUND
001116
001117        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
001118           MOVE CP-MONTH         TO WS-MONTH
001119           PERFORM 1200-IU-UE-CLP-LT-4
001120           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001121
001122        WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
001123           MOVE CP-MONTH         TO WS-MONTH
001124           PERFORM 1210-IU-UE-CLP-LT-13
001125           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001126
001127        WHEN OTHER
001128           MOVE CP-MONTH         TO WS-MONTH
001129           PERFORM 1220-IU-UE-CLP-GT-12
001130           COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
001131
001132     END-EVALUATE
001133
001134     GO TO 1999-CALC-REF-AMT-X
001135
001136     .
001137 1100-UE-CLP-LT-4.
001138
001139*****  (CLP-Yr1 Exp)-CLP*{((CLP-Yr1 Exp)/CLP - UEF1)*mo/12}
001140
001141     COMPUTE WS-TEMP-RESULT ROUNDED = (CP-DDF-CLP - 0)
001142        - CP-DDF-CLP * (((CP-DDF-CLP - 0)
001143        / CP-DDF-CLP - CP-DDF-HI-FACT)
001144        * WS-MONTH / 12)
001145
001146     .
001147 1110-UE-CLP-LT-13.
001148
001149*****  (CLP-Yr1 Exp)-CLP*{((CLP-Yr1 Exp)/CLP - UEF1)*3/12}
001150*****  -[(CLP-Yr1 Exp)-CLP*{((CLP-Yr1 Exp)/CLP - UEF1)*3/12
001151*****  -CLP*UEF1}*(mo-3)/9]
001152
001153     COMPUTE WS-TEMP-RESULT ROUNDED = (CP-DDF-CLP - 0)
001154        - CP-DDF-CLP * (((CP-DDF-CLP - 0)
001155        / CP-DDF-CLP - CP-DDF-HI-FACT) * 3 / 12)
001156        -  ((CP-DDF-CLP - 0) - CP-DDF-CLP
001157        * (((CP-DDF-CLP - 0) / CP-DDF-CLP
001158        - CP-DDF-HI-FACT) * 3 / 12) - CP-DDF-CLP
001159        * CP-DDF-HI-FACT) * (WS-MONTH - 3) / 9
001160
001161     .
001162 1120-UE-CLP-GT-12.
001163
001164*****  CLP * {UEF1 - (UEF1 -UEF2)*mo/12}
001165
001166     COMPUTE WS-TEMP-RESULT ROUNDED = CP-DDF-CLP *
001167        (CP-DDF-HI-FACT - (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
001168        * WS-MONTH / 12)
001169
001170     .
001171 1150-UE-GROSS-LT-4.
001172
001173     COMPUTE CP-1ST-YR-ALLOW = CP-1ST-YR-ALLOW - CP-DDF-YR1AF
001174     COMPUTE WS-TEMP-RESULT ROUNDED =
001175        (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW - CP-DDF-YR1AF
001176        * WS-MONTH / 3) - (CP-ORIGINAL-PREMIUM -
001177        CP-1ST-YR-ALLOW - CP-DDF-YR1AF) * (1 - CP-DDF-HI-FACT)
001178        * WS-MONTH / 12
001179
001180     .
001181 1160-UE-GROSS-LT-13.
001182
001183     MOVE 3                      TO WS-MONTH
001184     PERFORM 1150-UE-GROSS-LT-4
001185     COMPUTE WS-GFR3 ROUNDED = WS-TEMP-RESULT * 1
001186     MOVE CP-MONTH               TO WS-MONTH
001187     COMPUTE WS-TEMP-RESULT ROUNDED = WS-GFR3 - (WS-GFR3
001188        - (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW - CP-DDF-YR1AF)
001189        * (CP-DDF-HI-FACT)) * ((WS-MONTH - 3) / 9)
001190
001191     .
001192 1170-UE-GROSS-GT-12.
001193
001194*****  (GF-Yr1 Texp) * {UEF1 - (UEF1 -UEF2)*mo/12}
001195
001196     COMPUTE WS-TEMP-RESULT ROUNDED =
001197        (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW)
001198        * (CP-DDF-HI-FACT - (CP-DDF-HI-FACT
001199        - CP-DDF-LO-FACT) * WS-MONTH / 12)
001200
001201     .
001202 1200-IU-UE-CLP-LT-4.
001203
001204*****  no changes with < 4  pema 08/03/11
001205*****  (CLPCounty-Yr1 Exp)-(CLP*(UEF1 - UEF2)*mo/12)
001206
001207     COMPUTE WS-TEMP-RESULT ROUNDED =
001208        (CP-DDF-CLP - 0)
001209        - (CP-DDF-CLP - CP-CLP-RATE-UP)
001210        * (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
001211        * WS-MONTH / 12
001212
001213     .
001214 1210-IU-UE-CLP-LT-13.
001215
001216
001217*****  (CLPCounty-Yr1 Exp)-(CLP*(UEF1 - UEF2)*3/12)
001218*****  -[(CLPCounty-Yr1 Exp)-(CLP*(UEF1 - UEF2)*3/12)
001219*****  - CLP*UEF1]*(mo-3)/9
001220
001221     MOVE 3                      TO WS-MONTH
001222     PERFORM 1200-IU-UE-CLP-LT-4
001223     COMPUTE WS-UE-CLP ROUNDED = WS-TEMP-RESULT * 1
001224     MOVE CP-MONTH               TO WS-MONTH
001225     COMPUTE WS-TEMP-RESULT ROUNDED =
001226        WS-UE-CLP - (WS-UE-CLP - (CP-DDF-CLP - CP-CLP-RATE-UP)
001227          * CP-DDF-HI-FACT) * (WS-MONTH - 3) / 9
001228
001229     .
001230 1220-IU-UE-CLP-GT-12.
001231
001232*****  CLP*{UEF1 - (UEF1 -UEF2)*mo/12}
001233
001234     COMPUTE WS-TEMP-RESULT ROUNDED =
001235        (CP-DDF-CLP - CP-CLP-RATE-UP)
001236        * (CP-DDF-HI-FACT - (CP-DDF-HI-FACT
001237        - CP-DDF-LO-FACT) * WS-MONTH / 12)
001238
001239     .
001240 1250-IU-UE-GROSS-LT-4.
001241
001242*****  (Comm+MngtFee)*UECLPmo/CLPCounty+CLPCounty+AdminFee-
001243*****  Yr1 AF*(mo/3)-[CLP*(UEF1-UEF2)+(AdminFee-Yr1AF)*
001244*****  (1-UEF1)]*mo/12
001245
001246     PERFORM 1200-IU-UE-CLP-LT-4
001247     COMPUTE WS-UE-CLP ROUNDED = WS-TEMP-RESULT * 1
001248
001249     COMPUTE WS-TEMP-RESULT = CP-DDF-COMM-AND-MFEE *
001250        WS-UE-CLP / CP-DDF-CLP + CP-DDF-CLP +
001251        CP-DDF-CSO-ADMIN-FEE - CP-DDF-YR1AF *
001252        WS-MONTH / 3 - ((CP-DDF-CLP - CP-CLP-RATE-UP)
001253        * (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
001254        + (CP-DDF-CSO-ADMIN-FEE - CP-DDF-YR1AF)
001255        * (1 - CP-DDF-HI-FACT)) * WS-MONTH / 12
001256
001257     .
001258 1260-IU-UE-GROSS-LT-13.
001259
001260***** uegf3-(uegf3-(gf - 1st yr - clprateu)*hi))*(mo-3)/9
001261
001262     MOVE 3                      TO WS-MONTH
001263     PERFORM 1250-IU-UE-GROSS-LT-4
001264     move cp-month               to ws-month
001265     COMPUTE WS-GFR3 ROUNDED = WS-TEMP-RESULT * 1
001266     COMPUTE WS-TEMP-RESULT ROUNDED =
001267        WS-GFR3 - (WS-GFR3 - ((CP-ORIGINAL-PREMIUM
001268        - CP-1ST-YR-ALLOW - cp-clp-rate-up))
001269        * CP-DDF-HI-FACT) * (WS-MONTH - 3) / 9
001270
001271     .
001272 1270-IU-UE-GROSS-GT-12.
001273
001274***** (gf - 1st yr - clprateu)*(hi-(hi-lo)*mo/12)
001275
001276     COMPUTE WS-TEMP-RESULT ROUNDED =
001277        (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW
001278        - CP-CLP-RATE-UP) *
001279        (CP-DDF-HI-FACT - (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
001280        * WS-MONTH / 12)
001281
001282     .
001283 1299-END-DDF-CALCS.
001284     .
001285 1999-CALC-REF-AMT-X.
      *<<((file: ELCRFNDP))
000130
000131     MOVE CALCULATION-PASS-AREA TO DFHCOMMAREA.
000132
000133     
      * EXEC CICS RETURN
000134*    END-EXEC.
      *    MOVE '.(                    ''   #00002497' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303032343937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000135     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRFND' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
000136
000137     EJECT
000138  2500-GET-RATE.
000139     
      * EXEC CICS LINK
000140*        PROGRAM    ('ELRATE')
000141*        COMMAREA   (CALCULATION-PASS-AREA)
000142*        LENGTH     (CP-COMM-LENGTH)
000143*        END-EXEC.
           MOVE 'ELRATE' TO DFHEIV1
      *    MOVE '."C                   (   #00002503' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032353033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000144
000145  2500-EXIT.
000146       EXIT.
000147
000148  9100-CONVERT-DATE.
000149     
      * EXEC CICS LINK
000150*         PROGRAM   ('ELDATCV')
000151*         COMMAREA  (DATE-CONVERSION-DATA)
000152*         LENGTH    (DC-COMM-LENGTH)
000153*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002513' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032353133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000154
000155  9100-EXIT.
000156       EXIT.
000157
000158     EJECT
000159  10000-NET-TERM  SECTION.
000160*                      COPY ERCNETP.
      *>>((file: ERCNETP))
000001*****************************************************************
000002*                                                               *
000003*                            ERCNETP                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.029                         *
000006*                                                               *
000007*****************************************************************.
000008*
000009*    FIVE PARAMETERS ARE PASSED TO THIS MODULE AND A FACTOR IS
000010*    RETURNED. PARAMETERS PASSED  - A.P.R. (S999V9999)
000011*                                   ORIGINAL TERM (S999)
000012*                                   REMAINING TERM (S999)
000013*                                   NET PAY OPTION (X)
000014*                                   LOAN TERM (S999)
000015*              FACTOR RETURNED IS - FACTOR (S9(4)V9(9))
000016*
000017*    FACTOR RETURNED IS MULTIPLIED BY ORIG. FACE TO GET REMAINING
000018*    FACE. IF ORIGNAL TERM = REMAINING TERM, FACTOR WOULD BE 1,
000019*    THEREFORE MODULE ASSUMES RATING IS DESIRED AND FACTOR THAT
000020*    IS RETURNED MAY BE MULTIPLIED BY THOUSANDS OF ORIGINAL FACE
000021*    AND REGULAR PREMIUM PER $100 PER MONTH TO GET PREMIUM TO BE
000022*    CHARGED.
000023*
000024*    OPTIONS - S = NET SIMPLE
000025*          SPACE = NET PAY  (1 MO. INTEREST)
000026*              A = NET PAY  (0 MO. INTEREST)
000027*              I = NET PAY  (2 MO. INTEREST)
000028*              J = NET PAY  (3 MO. INTEREST)
000029*              K = NET PAY  (4 MO. INTEREST)
000030*              T = TRUNCATED  (0 MO. INTEREST)
000031*              U = TRUNCATED  (1 MO. INTEREST)
000032*              V = TRUNCATED  (2 MO. INTEREST)
000033*              W = TRUNCATED  (3 MO. INTEREST)
000034*              X = TRUNCATED  (4 MO. INTEREST)
000035*              R = REFUNDS (REGULAR OR TRUNCATED)
000036******************************************************************
000037*                   C H A N G E   L O G
000038*
000039* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000040*-----------------------------------------------------------------
000041*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000042* EFFECTIVE    NUMBER
000043*-----------------------------------------------------------------
000044* 120202    2001061800003  PEMA  ADD DCC PROCESSING
000045* 120502                   PEMA  ADD SPECIAL IN CALC FOR CID
000046* 120403                   PEMA  ADD SPECIAL TX CALC
000047* 011904                   PEMA  ADD TOTAL FEE PROCESSING
000048* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000049* 081205                   PEMA  ADD DAYS TO 1ST PMT ON 'S'
000050* 083105                   PEMA  ADD DAYS TO 1ST PMT ON 'N'
000051* 011707                   PEMA  ADD SC NP PLUS 6 PMTS
000052* 021207                   PEMA  ADD PA NP
000053* 022107                   PEMA  ADD NH NP AND NP TRUNC
000054* 032807                   PEMA  ADD VT NP PLUS 2
000055* 091307  IR2007090600001  PEMA  REMOVE LOW APR CHECK
000056* 110207    2007010300001  PEMA  FIX PROBLEM WHERE DIFF OF LOAN
000057* 110207      LOAN TERM AND INS TERM IS 1. ALSO ADD TRUNC
000058* 110207      TO 'AL' NET PAY CALC
000059* 052808    2008032000002  PEMA  ADD CODE FOR AK NET PAY
000060* 052808    2008050800003  PEMA  ADD CODE FOR NV NET +2
000061* 060308  IR2008052800002  PEMA  ADD TNP FOR ME
000062* 060308  CR2008032700002  PEMA  ADD EXT DAYS FOR ME
000063* 0600909 CR2008042300002  PEMA  ADD NEW FARM PLAN CALC
000064* 010410  CR2009092300002  PEMA  ADD MN NET CALC
000065* 010410  CR2008021200005  PEMA  ADD MN NET BALLOON
000066* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
000067* 071910  IR2010062900001  PEMA ADD MN NET BALLOON REFUND CALC
000068* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
000069* 092311  IR2011092100001  PEMA  ID 10C MONTHLY
000070* 032612  CR2011110200001  PEMA  AHL CHANGES
000071* 111212  CR2012013100001  PEMA  CHANGES FOR MN AND WA
000072* 022613  CR2011021500001  PEMA  ADD CODE 4 TRUNC BALLOON DCC 10C
000073* 050713  CR2008042200001  PEMA  ADD CODE FOR ZERO APR
000074* 030515  IR2015022300001  PEMA  ADD MN AND WA FARMPLAN CODING
000075* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS FOR NET BALLOON
000076* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000077* 021519  CR2019021100001  PEMA  non-monthly AL calcs
000078* 012820  CR2020010600001  PEMA  MOD TX NP FORMULA TO HANDLE TRUNC
000079* 012820    CONTINUED   AND TO ADJ AF FOR EXTRA DAYS.
000080******************************************************************
000081
000082     MOVE N-P-APR     TO ANNUAL-INT-RATE.
000083     MOVE N-P-ORIG    TO ORIGINAL-TERM.
000084     MOVE N-P-LOAN    TO LOAN-TERM.
000085     MOVE N-P-REM     TO REMAINING-TERM.
000086     MOVE N-P-OPT     TO OPTION-SW.
000087
000088     MOVE +0 TO FACTOR.
000089
000090     if cp-company-id = 'CID'
000091        if annual-int-rate = +99.9999
000092           go to 99000-error
000093        end-if
000094     else
000095        if annual-int-rate = zero
000096           go to 99000-error
000097        end-if
000098     end-if
000099
000100     IF ORIGINAL-TERM = ZERO
000101         GO TO 99999-RETURN.
000102     IF REMAINING-TERM = ZERO
000103         GO TO 99999-RETURN.
000104     IF REMAINING-TERM GREATER ORIGINAL-TERM
000105         GO TO 99000-ERROR.
000106     IF LOAN-TERM = ZERO
000107         GO TO 99999-RETURN.
000108     IF LOAN-TERM LESS ORIGINAL-TERM
000109         GO TO 99000-ERROR.
000110
000111*    IF ANNUAL-INT-RATE LESS +2
000112*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
000113*    IF ANNUAL-INT-RATE LESS +2
000114*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
000115*    IF ANNUAL-INT-RATE LESS +2
000116*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
000117
000118     IF NPO-REFUND
000119         MOVE '2' TO NP-PROCESS-SW
000120       ELSE
000121         IF ORIGINAL-TERM = REMAINING-TERM
000122            MOVE '1'   TO NP-PROCESS-SW
000123           ELSE
000124            MOVE '3'   TO NP-PROCESS-SW.
000125
000126     IF NPO-SIMPLE
000127         MOVE 'S' TO TYPE-SW
000128     ELSE
000129         MOVE 'N' TO TYPE-SW.
000130
000131     COMPUTE I ROUNDED = (ANNUAL-INT-RATE / K100) / K12.
000132     if i = zeros
000133        move .0000000001         to i
000134     end-if
000135
000136     COMPUTE V ROUNDED = K1 / (K1 + I).
000137
000138     MOVE V     TO VX
000139                   SV.
000140
000141     MOVE +1    TO X1
000142                   SX.
000143
000144     MOVE LOAN-TERM TO MAX-X.
000145
000146     COMPUTE X2 = MAX-X - ORIGINAL-TERM.
000147
000148     IF MAX-X = +1
000149        GO TO 10700-COMPUTE-REMAINING-FACTOR.
000150
000151     COMPUTE EXPIRED-TERM = ORIGINAL-TERM - REMAINING-TERM.
000152
000153     IF (CP-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'CID' OR 'MON'
000154         OR 'DCC' or 'VPP' OR 'NCL') AND
000155        (NOT NP-REFUND)
000156        NEXT SENTENCE
000157     ELSE
000158     IF LOAN-TERM NOT = ORIGINAL-TERM
000159        COMPUTE REMAINING-TERM = LOAN-TERM - EXPIRED-TERM.
000160
000161     COMPUTE SV-MINUS-ONE EQUAL ORIGINAL-TERM - +1.
000162
000163 10500-VX-LOOP.
000164
000165***  VX = AMORT. LOAN TERM.
000166***  SV = AMORT. INSURANCE TERM.
000167***  SV = AMORT. REMAINING TERM FOR REFUND CALCS.
000168***  SX = AMORT. LOAN TERM - INSURANCE TERM. (USED FOR TRUNCATED.
000169***  SV-MINUS-ONE = INSURANCE TERM - +1... (TMS ONLY)
000170
000171     IF X2 = 1
000172        MOVE V         TO SX
000173     END-IF
000174     COMPUTE VX ROUNDED = VX * V.
000175
000176     ADD B1 TO X1.
000177
000178     IF X1 = REMAINING-TERM
000179          MOVE VX    TO SV.
000180
000181     IF X1 EQUAL SV-MINUS-ONE
000182        MOVE VX      TO SV-MINUS-ONE.
000183
000184     IF X1 = X2
000185          MOVE VX    TO SX.
000186
000187     IF X1 NOT = MAX-X
000188          GO TO 10500-VX-LOOP.
000189
000190 10700-COMPUTE-REMAINING-FACTOR.
000191*****  WK1 = LOAN TERM AMORT
000192     COMPUTE WK1 = K1 - VX.
000193*****  WK2 = INS TERM AMORT
000194     COMPUTE WK2 = K1 - SV.
000195*****  WK5 = LOAN TERM - INS TERM AMORT (TRUNCATED)
000196     COMPUTE WK5 = K1 - SX.
000197*****  WK6 = INS TERM - 1
000198     COMPUTE WK6 = K1 - SV-MINUS-ONE.
000199
000200     IF NP-RATING
000201         GO TO 12000-PREMIUM-RATE.
000202
000203     IF NP-REFUND
000204         GO TO 11000-REFUND-CALC.
000205
000206     IF NET-STD
000207         COMPUTE WK3 ROUNDED = (WK2 * K1000) / WK1.
000208
000209     IF NET-SMP
000210         COMPUTE WK3 ROUNDED = ((R + 1) / (N + 1)) * (R / N)
000211         COMPUTE WK3 ROUNDED = (1 - WK3) * ((I * N / WK1) - 1)
000212         COMPUTE WK3 ROUNDED = WK3 + 1 - ((N - R) * I / WK1)
000213         COMPUTE WK3 ROUNDED = WK3 * 1000.
000214
000215     IF REMAINING-TERM LESS THAN X2
000216         MOVE +0 TO WK3.
000217
000218     MOVE WK3 TO FACTOR.
000219
000220     GO TO 99999-RETURN.
000221
000222 11000-REFUND-CALC.
000223     IF REMAINING-TERM NOT LESS MAX-X
000224        MOVE +1 TO FACTOR
000225        GO TO 99999-RETURN.
000226
000227     IF REMAINING-TERM LESS +1
000228        MOVE 0 TO FACTOR
000229        GO TO 99999-RETURN.
000230
000231     COMPUTE WK2 ROUNDED = WK2 / I.
000232     COMPUTE WK5 ROUNDED = WK5 / I.
000233     COMPUTE WK1 ROUNDED = WK1 / I.
000234
000235     IF (CP-COMPANY-ID EQUAL 'CID')
000236        AND (CP-STATE-STD-ABBRV = 'MN')
000237        AND (CP-CERT-EFF-DT > X'A4FF')
000238        AND (CP-RATE-AS-REG-BALLOON)
000239        AND (CP-EARN-AS-NET-PAY)
000240        AND (CP-DEVIATION-CODE NOT = 'LEV')
000241        CONTINUE
000242     ELSE
000243        GO TO 11000-CHECK-MN-LEV-BALLOON
000244     END-IF
000245
000246     COMPUTE R = M - E
000247     COMPUTE FACTOR = (R - WK2 + WK5) /
000248        (M - WK1 + WK5)
000249
000250     GO TO 99999-RETURN
000251
000252      .
000253 11000-CHECK-MN-LEV-BALLOON.
000254
000255     IF (CP-COMPANY-ID EQUAL 'CID')
000256        AND (CP-STATE-STD-ABBRV = 'MN')
000257        AND (CP-CERT-EFF-DT > X'A4FF')
000258        AND (CP-RATE-AS-REG-BALLOON)
000259        AND (CP-EARN-AS-NET-PAY)
000260        AND (CP-DEVIATION-CODE = 'LEV')
000261        CONTINUE
000262     ELSE
000263        GO TO 11000-CONTINUE-REFUND
000264     END-IF
000265
000266     COMPUTE ANGLEM = (1 - (1 / (1 + I)) ** M) / I
000267     COMPUTE ANGLEM-T = (1 - (1 / (1 + I))
000268        ** (M - E)) / I
000269
000270     COMPUTE FACTOR = ANGLEM-T / ANGLEM
000271
000272     GO TO 99999-RETURN
000273
000274     .
000275 11000-CONTINUE-REFUND.
000276
000277*    IF CP-GAP-ACTUARIAL
000278*       COMPUTE ANGLEN = WK1 / ((1 + I) **
000279*          (CP-TERM-OR-EXT-DAYS * 12 / 365))
000280*       COMPUTE WK3 = WK2 / ANGLEN
000281*       MOVE 'S'                 TO CP-REFUND-TYPE-USED
000282
000283     IF CP-GAP-ACTUARIAL
000284        COMPUTE ANGLEN = WK1 / ((1 + I) **
000285           (CP-TERM-OR-EXT-DAYS * 12 / 365))
000286        COMPUTE WK3 = (R - WK2) / (M - ANGLEN)
000287     ELSE
000288        COMPUTE WK3 ROUNDED =
000289          (N-P-REM - WK2 + WK5) / (ORIGINAL-TERM - WK1 + WK5)
000290     END-IF
000291
000292     MOVE WK3  TO FACTOR.
000293
000294     GO TO 99999-RETURN.
000295
000296 12000-PREMIUM-RATE.
000297*    K-I IS ADJUSTMENT FACTOR FOR NO. MONTHS ADD'L. INTEREST
000298*      OPTION - N OR U OR SPACE  = 1 MO,  SO K-I = 1 + I
000299*      OPTION - A OR T           = 0 MO,  SO K-I = 1
000300*      OPTION - I OR V           = 2 MO,  SO K-I = 1 + 2I
000301*      OPTION - J OR W           = 3 MO,  SO K-I = 1 + 3I
000302*      OPTION - K OR X           = 4 MO,  SO K-I = 1 + 4I
000303
000304     COMPUTE K-I = K1 + I.
000305     MOVE K-I TO ONE-PLUS-I.
000306
000307     IF NPO-ALT OR NPO-TRUNC-0
000308         MOVE K1 TO K-I.
000309
000310     IF NPO-2MO OR NPO-TRUNC-2
000311         COMPUTE K-I = K1 + (2 * I).
000312
000313     IF NPO-3MO OR NPO-TRUNC-3
000314         COMPUTE K-I = K1 + (3 * I).
000315
000316     IF NPO-4MO OR NPO-TRUNC-4
000317         COMPUTE K-I = K1 + (4 * I).
000318
000319     COMPUTE RA ROUNDED = 1 -
000320             ((X2 * (X2 + 1)) /
000321              (N *  (N  + 1))).
000322
000323     IF (CP-COMPANY-ID EQUAL 'CSL') AND
000324        (CP-CSL-VALID-NP-BENEFIT-CD)
000325        NEXT SENTENCE
000326     ELSE
000327        GO TO XXXX-CHECK-DISCOUNT-5.
000328
000329     MOVE CP-CERT-EFF-DT TO DC-BIN-DATE-1.
000330     MOVE ' ' TO DC-OPTION-CODE.
000331     PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
000332
000333     IF DATE-CONVERSION-ERROR
000334        MOVE '2'                 TO CP-RETURN-CODE
000335        MOVE ZEROS               TO FACTOR
000336                                    CP-CALC-PREMIUM
000337         GO TO 99999-RETURN.
000338
000339     MOVE DC-JULIAN-DATE-1 TO WS-INT-BEGIN-DATE
000340     MOVE CP-FIRST-PAY-DATE TO DC-BIN-DATE-1
000341     MOVE ' ' TO DC-OPTION-CODE
000342     PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
000343
000344     IF DATE-CONVERSION-ERROR
000345        MOVE '2'                 TO CP-RETURN-CODE
000346        MOVE ZEROS               TO FACTOR
000347                                    CP-CALC-PREMIUM
000348         GO TO 99999-RETURN.
000349
000350     MOVE DC-JULIAN-DATE-1 TO WS-DUE-DATE.
000351
000352     COMPUTE ANGLEN-1 EQUAL 1 + (WK6 / I).
000353
000354     IF (WS-DUE-DATE - WS-INT-BEGIN-DATE) GREATER THAN 30
000355        COMPUTE WK3 EQUAL 1 + ((30 + CP-TERM-OR-EXT-DAYS) *
000356        CP-PAY-FREQUENCY / 365) * I
000357     ELSE
000358        COMPUTE WK3 EQUAL 1 + ((30 - CP-TERM-OR-EXT-DAYS) *
000359        CP-PAY-FREQUENCY / 365) * I.
000360
000361     COMPUTE WS-WORK-RATE EQUAL ((2 * CP-PREMIUM-RATE) /
000362     13).
000363
000364     COMPUTE WK5-CSL EQUAL WS-WORK-RATE * ((1 + I) / I) *
000365     (N - ( ANGLEN-1 / WK3)).
000366
000367     MOVE ZEROS TO WS-Y-X-FACTOR WS-X-Y-FACTOR.
000368
000369     COMPUTE WS-CALC-FREQ-RATE EQUAL
000370       (CP-LOAN-APR * .01) / CP-PAY-FREQUENCY.
000371
000372     COMPUTE WS-Y-X-FACTOR EQUAL
000373       1 + ((1 - (1 + WS-CALC-FREQ-RATE) ** -(N - 1)) /
000374       WS-CALC-FREQ-RATE).
000375
000376     COMPUTE WS-ODD-DAY-UNITS EQUAL
000377       (WS-DUE-DATE - WS-INT-BEGIN-DATE) * CP-PAY-FREQUENCY / 365.
000378
000379     COMPUTE WS-X-Y-FACTOR EQUAL
000380       1 + (WS-ODD-DAY-UNITS * WS-CALC-FREQ-RATE).
000381
000382     COMPUTE WS-NP-FACTOR EQUAL WS-WORK-RATE *
000383       ((1 + WS-CALC-FREQ-RATE) / WS-CALC-FREQ-RATE) *
000384       (N - (WS-Y-X-FACTOR / WS-X-Y-FACTOR)).
000385
000386     COMPUTE CP-CALC-PREMIUM EQUAL CP-MONTHLY-PAYMENT * WK5-CSL.
000387
000388     GO TO 99999-RETURN.
000389
000390 XXXX-CHECK-DISCOUNT-5.
000391
000392     IF WS-DISCOUNT-OPTION EQUAL '5'
000393        NEXT SENTENCE
000394     ELSE
000395        GO TO 12000-CHECK-CLIENT.
000396
000397      COMPUTE ANGLEN EQUAL WK1 / I.
000398
000399      COMPUTE APR100 EQUAL ANNUAL-INT-RATE / +100.
000400
000401      COMPUTE WK3 EQUAL
000402        (12 *
000403        (((CP-ORIGINAL-BENEFIT / ANGLEN) * N) -
000404        CP-ORIGINAL-BENEFIT)) /
000405        APR100.
000406
000407     COMPUTE WK3 EQUAL
000408        (WK3 * CP-PREMIUM-RATE) / +1000.
000409
000410     COMPUTE WK3 EQUAL
000411        (24 * WK3) / (24 + (WS-WORK-DIS-RATE * N)).
000412
000413      MOVE WK3 TO CP-CALC-PREMIUM.
000414
000415     GO TO 99999-RETURN.
000416
000417 12000-CHECK-CLIENT.
000418
000419     IF (CP-COMPANY-ID EQUAL 'MON') AND
000420        (CP-STATE-STD-ABBRV EQUAL 'MD') AND
000421        (CP-TRUNCATED-LIFE)
000422        NEXT SENTENCE
000423     ELSE
000424        GO TO 12000-CHECK-NEXT.
000425
000426     COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
000427
000428     COMPUTE ANGLEN EQUAL WK1 / I.
000429     COMPUTE ANGLEM EQUAL WK2 / I.
000430
000431     COMPUTE FM EQUAL
000432                    (((ONE-PLUS-I ** M) * I * M) /
000433                    ((ONE-PLUS-I ** M) - 1))
000434                    - 1.
000435
000436     COMPUTE FN EQUAL
000437                    (((ONE-PLUS-I ** N) * I * N) /
000438                    ((ONE-PLUS-I ** N) - 1))
000439                    - 1.
000440
000441     COMPUTE FNM EQUAL
000442                    (((ONE-PLUS-I ** (N - M)) * I * (N - M)) /
000443                    ((ONE-PLUS-I ** (N - M)) - 1))
000444                    - 1.
000445
000446     COMPUTE GR EQUAL
000447                    (WS-WORK-RATE * ANGLEM * I) /
000448                    (M - ANGLEM).
000449
000450     COMPUTE ODF EQUAL
000451                     (1 +
000452                     ((12 * I * (CP-TERM-OR-EXT-DAYS + 30)) /
000453                     360)) /
000454                     ONE-PLUS-I.
000455
000456     COMPUTE OD EQUAL
000457                    (1 + (ODF * (FN / I - 1))) +
000458                    ((12 * CP-TERM-OR-EXT-DAYS) / +360).
000459
000460     COMPUTE WK3 EQUAL
000461*                   ((1 + FN) * ODF * CP-ORIGINAL-BENEFIT) /
000462                    ((1 + FN) * ODF *
000463                    (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM)) /
000464                    (1 - (GR * (OD - ((ONE-PLUS-I ** M) / I) *
000465                    FNM * ODF * (1 - ANGLEM / ANGLEN)))).
000466
000467     COMPUTE TRUNC-PMT EQUAL WK3 / N.
000468     COMPUTE WK3 EQUAL TRUNC-PMT * N.
000469
000470     COMPUTE WK3 EQUAL
000471                    (WK3 / (ODF * (1 + FN))) -
000472*                   CP-ORIGINAL-BENEFIT.
000473                    (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM).
000474
000475      MOVE WK3 TO CP-CALC-PREMIUM.
000476
000477     GO TO 99999-RETURN.
000478
000479 12000-CHECK-NEXT.
000480
000481     IF WS-DISCOUNT-OPTION EQUAL '3' OR '4'
000482        NEXT SENTENCE
000483     ELSE
000484        GO TO 12000-CHECK-PA-TRUNC.
000485
000486      IF WS-WORK-DIS-RATE EQUAL +0
000487         MOVE '7'                TO CP-RETURN-CODE
000488         MOVE ZEROS              TO FACTOR
000489                                    CP-CALC-PREMIUM
000490         GO TO 99999-RETURN.
000491
000492      COMPUTE ANGLEN EQUAL WK1 / I.
000493
000494      COMPUTE APR100 EQUAL ANNUAL-INT-RATE / +100.
000495
000496      IF WS-WORK-DIS-RATE EQUAL APR100
000497         ADD .00001 TO APR100.
000498
000499      COMPUTE WK8 EQUAL 12 / (12 + WS-WORK-DIS-RATE).
000500      COMPUTE WK9 EQUAL 12 / (12 + APR100).
000501
000502
000503
000504     COMPUTE Y = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
000505        / (1 + I)
000506     COMPUTE ANGLEN-1 = ANGLEN / Y
000507
000508      IF CP-COMPANY-ID EQUAL 'CDL'
000509         COMPUTE WK3 EQUAL
000510
000511          (144 * CP-ORIGINAL-BENEFIT / ANGLEN) *
000512          ((1 + WS-WORK-DIS-RATE / 12) ** .5) *
000513
000514          ((1 / (WS-WORK-DIS-RATE * APR100)) -
000515
000516          ((WK8 ** N) /
000517          (WS-WORK-DIS-RATE * (APR100 - WS-WORK-DIS-RATE))) +
000518
000519          ((WK9 ** N) /
000520          (APR100 * (APR100 - WS-WORK-DIS-RATE))))
000521
000522      ELSE
000523         COMPUTE WK3 EQUAL
000524
000525          ((CP-ORIGINAL-BENEFIT / ANGLEN-1) *
000526          (12 + WS-WORK-DIS-RATE) * 12) *
000527
000528          ((1 / (WS-WORK-DIS-RATE * APR100)) -
000529
000530          ((WK8 ** N) /
000531          (WS-WORK-DIS-RATE * (APR100 - WS-WORK-DIS-RATE))) +
000532
000533          ((WK9 ** N) /
000534          (APR100 * (APR100 - WS-WORK-DIS-RATE)))).
000535
000536      COMPUTE WK3 EQUAL
000537          (WK3 * CP-PREMIUM-RATE) / +1000.
000538
000539      MOVE WK3 TO CP-CALC-PREMIUM.
000540
000541     GO TO 99999-RETURN.
000542
000543 12000-CHECK-PA-TRUNC.
000544
000545     IF (CP-COMPANY-ID EQUAL 'NCL') AND
000546        (CP-STATE-STD-ABBRV EQUAL 'PA') AND
000547        (CP-EARN-AS-NET-PAY) AND
000548        (CP-TRUNCATED-LIFE)
000549        NEXT SENTENCE
000550     ELSE
000551        GO TO 12000-CHECK-MD-TRUNC.
000552
000553     COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
000554
000555     COMPUTE ANGLEN-M EQUAL WK5 / I.
000556     COMPUTE ANGLEN EQUAL WK1 / I.
000557
000558     COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
000559
000560     COMPUTE WK3 EQUAL
000561        (CP-ORIGINAL-BENEFIT * 2 * (M - (ANGLEN-M - ANGLEN)) *
000562        WS-WORK-RATE) / (ANGLEN-1 * (N + 1) * I).
000563
000564     MOVE WK3 TO CP-CALC-PREMIUM.
000565
000566     GO TO 99999-RETURN.
000567
000568 12000-CHECK-MD-TRUNC.
000569
000570     IF (CP-COMPANY-ID EQUAL 'NCL') AND
000571        (CP-STATE-STD-ABBRV EQUAL 'MD') AND
000572        (CP-EARN-AS-NET-PAY) AND
000573        (CP-TRUNCATED-LIFE)
000574        NEXT SENTENCE
000575     ELSE
000576        GO TO 12000-CHECK-MD.
000577
000578     COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
000579
000580     COMPUTE ANGLEN-M EQUAL WK5 / I.
000581     COMPUTE ANGLEM EQUAL WK2 / I.
000582     COMPUTE ANGLEN EQUAL WK1 / I.
000583
000584     COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
000585
000586     COMPUTE WK3 EQUAL
000587        CP-ORIGINAL-BENEFIT * WS-WORK-RATE *
000588        ((ANGLEM * (M - (ANGLEN - ANGLEN-M))) /
000589        (ANGLEN-1 * (M - ANGLEM))).
000590
000591     MOVE WK3 TO CP-CALC-PREMIUM.
000592
000593     GO TO 99999-RETURN.
000594
000595 12000-CHECK-MD.
000596
000597     IF (CP-COMPANY-ID EQUAL 'NCL') AND
000598        (CP-STATE-STD-ABBRV EQUAL 'MD') AND
000599        (CP-EARN-AS-NET-PAY)
000600        NEXT SENTENCE
000601     ELSE
000602        GO TO 12000-CHECK-ME-TRUNC.
000603
000604     COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
000605
000606     COMPUTE WK3 EQUAL
000607        CP-ORIGINAL-BENEFIT * WS-WORK-RATE.
000608
000609     MOVE WK3 TO CP-CALC-PREMIUM.
000610
000611     GO TO 99999-RETURN.
000612
000613 12000-CHECK-ME-TRUNC.
000614
000615     IF (CP-COMPANY-ID EQUAL 'NCL' OR 'CID')
000616        AND (CP-STATE-STD-ABBRV EQUAL 'ME')
000617        AND (CP-EARN-AS-NET-PAY)
000618        AND (CP-TRUNCATED-LIFE)
000619        CONTINUE
000620     ELSE
000621        GO TO 12000-CHECK-CID-WA-FARMPLAN
000622     END-IF
000623
000624     COMPUTE WS-WORK-RATE = CP-PREMIUM-RATE / +1000
000625
000626     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
000627
000628     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
000629
000630     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
000631
000632     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
000633     COMPUTE ANGLEN-1 = ANGLEN / Y
000634
000635     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000636
000637     COMPUTE WK5 = 1 / (1 + (WS-WORK-DIS-RATE * N / 24))
000638
000639     COMPUTE WK9 = 1 / (1 + (WS-WORK-DIS-RATE * (N - M) / 24))
000640
000641     COMPUTE WK3
000642        = (WK4 * ((N - ANGLEN) / I) * WS-WORK-RATE * WK5)
000643        - (WK4 * (((N - M) - ANGLEN-M) / I) * WS-WORK-RATE * WK9
000644            * (1 / (1.0635 ** (M / 12))))
000645
000646*    COMPUTE WK8 EQUAL 1 / (1 + (.045 * N / 24)).
000647*    COMPUTE WK9 EQUAL
000648*       1 / (1 + (.045 * (N - M) / 24))
000649*    COMPUTE WK7 EQUAL
000650*       WS-WORK-RATE * 20 / (M + 1).
000651*    COMPUTE WK3 EQUAL
000652*       ((( N - ANGLEN) * WK7 * WK8) /
000653*       (10 * ANGLEN-1 * I)) -
000654*       (((N - M - ANGLEN-M) * WK7 * WK9 * 1) /
000655*       (10 * ANGLEN-1 * I * (1.0635 ** (M / 12)))).
000656*    COMPUTE WK3 EQUAL WK3 * CP-ORIGINAL-BENEFIT.
000657*
000658     MOVE WK3 TO CP-CALC-PREMIUM.
000659
000660     GO TO 99999-RETURN
000661
000662      .
000663 12000-CHECK-CID-WA-FARMPLAN.
000664
000665     IF (CP-COMPANY-ID EQUAL 'CID')
000666        AND (CP-STATE-STD-ABBRV = 'WA')
000667        AND (CP-FARM-PLAN)
000668        CONTINUE
000669     ELSE
000670        GO TO 12000-CHECK-CID-FARMPLAN
000671     END-IF
000672
000673**  This was removed due to IR 2015022300001
000674    GO TO 12000-CHECK-CID-FARMPLAN
000675
000676     move cp-original-term       to n
000677     move cp-no-of-pmts          TO C-TOT-PMTS
000678     compute c-pmts-per-year = 12 / cp-pay-frequency
000679     move cp-pay-frequency       to m
000680     compute c-days-in-pmt-per = 365 / c-pmts-per-year
000681     COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
000682     if i = zeros
000683        move .0000000001         to i
000684     end-if
000685     COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** c-tot-pmts) / I
000686
000687     COMPUTE Y = (1 + ((CP-DAYS-TO-1ST-PMT * I) /
000688        (m * 30))) / (1 + I)
000689     COMPUTE J = ((1 + WS-WORK-DIS-RATE)
000690        ** CP-PAY-FREQUENCY) - 1
000691     COMPUTE TI = (cp-days-to-1st-pmt - (M * 30)) / 30
000692     COMPUTE WK1 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
000693        ** TI) / WS-WORK-DIS-RATE
000694     COMPUTE WK2 = (1 - (1 / (1 + J)) ** c-tot-pmts) / J
000695     COMPUTE WK3 = (1 - (1 / (1 + J)) ** (c-tot-pmts - 1)) / J
000696     COMPUTE WK4 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
000697        ** m) / WS-WORK-DIS-RATE
000698     COMPUTE WK2 = WK2 * (1 + J)
000699     COMPUTE WK4 = WK4 * (1 + WS-WORK-DIS-RATE)
000700     COMPUTE WK1 = WK1 * (1 + WS-WORK-DIS-RATE)
000701     COMPUTE WK3 = WK3 * (1 + J)
000702     COMPUTE WK5 = (WK3 - (c-tot-pmts - 1) * (1 / (1 + J)
000703        ** (c-tot-pmts - 1))) / J
000704     COMPUTE GR = (((1 / (1 + WS-WORK-DIS-RATE) ** TI) * WK4 *
000705        ((c-tot-pmts * WK2) - WK5)) + (c-tot-pmts * WK1)) *
000706        (CP-PREMIUM-RATE / 1000)
000707     COMPUTE WK7 = CP-ORIGINAL-BENEFIT / (anglen / y)
000708     COMPUTE WK6 = (WK7 * GR)
000709
000710     MOVE WK6 TO CP-CALC-PREMIUM
000711
000712     GO TO 99999-RETURN
000713
000714     .
000715 12000-CHECK-CID-FARMPLAN.
000716
000717     IF (CP-COMPANY-ID EQUAL 'CID')
000718        AND (CP-STATE-STD-ABBRV EQUAL 'ND' OR 'MT' OR 'NJ'
000719           OR 'IN' OR 'AZ')
000720        AND (CP-FARM-PLAN)
000721        CONTINUE
000722     ELSE
000723        GO TO 12000-CHECK-CID-AL-FARMPLAN
000724     END-IF
000725
000726     COMPUTE N = M / CP-PAY-FREQUENCY
000727     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
000728     COMPUTE J = ((1 + WS-WORK-DIS-RATE)
000729        ** CP-PAY-FREQUENCY) - 1
000730     COMPUTE TI = (D - (CP-PAY-FREQUENCY * 30)) / 30
000731     COMPUTE WK1 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
000732        ** TI) / WS-WORK-DIS-RATE
000733     COMPUTE WK2 = (1 - (1 / (1 + J)) ** N) / J
000734     COMPUTE WK3 = (1 - (1 / (1 + J)) ** (N - 1)) / J
000735     COMPUTE WK4 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
000736        ** CP-PAY-FREQUENCY) / WS-WORK-DIS-RATE
000737     COMPUTE WK2 = WK2 * (1 + J)
000738     COMPUTE WK4 = WK4 * (1 + WS-WORK-DIS-RATE)
000739     COMPUTE WK1 = WK1 * (1 + WS-WORK-DIS-RATE)
000740     COMPUTE WK3 = WK3 * (1 + J)
000741     COMPUTE WK5 = (WK3 - (N - 1) * (1 / (1 + J)
000742        ** (N - 1))) / J
000743     COMPUTE GR = (((1 / (1 + WS-WORK-DIS-RATE) ** TI) * WK4 *
000744        ((N * WK2) - WK5)) + (N * WK1)) *
000745        (CP-PREMIUM-RATE / 1000)
000746     COMPUTE WK7 = CP-ORIGINAL-BENEFIT / N
000747     COMPUTE WK6 = (WK7 * GR)
000748
000749     MOVE WK6 TO CP-CALC-PREMIUM
000750
000751     GO TO 99999-RETURN
000752
000753     .
000754 12000-CHECK-CID-AL-FARMPLAN.
000755
000756     IF (CP-COMPANY-ID EQUAL 'CID')
000757        AND (CP-STATE-STD-ABBRV = 'AL')
000758        AND (CP-FARM-PLAN)
000759        CONTINUE
000760     ELSE
000761        GO TO 12000-CHECK-SC-CID
000762     END-IF
000763
000764     display ' made AL Farm Plan calc '
000765
000766     evaluate true
000767        when cp-pmt-mode = 'B'
000768           move 26               to c-pmts-per-year
000769           move 14               to c-days-in-pmt-per
000770        when cp-pmt-mode = 'W'
000771           move 52               to c-pmts-per-year
000772           move 07               to c-days-in-pmt-per
000773        when cp-pmt-mode = 'S'
000774           move 24               to c-pmts-per-year
000775           move 15               to c-days-in-pmt-per
000776        when cp-pmt-mode = 'A'
000777           move 1                to c-pmts-per-year
000778           move 360              to c-days-in-pmt-per
000779        when cp-pmt-mode = 'T'
000780           move 2                to c-pmts-per-year
000781           move 180              to c-days-in-pmt-per
000782        when cp-pmt-mode = 'Q'
000783           move 4                to c-pmts-per-year
000784           move 90               to c-days-in-pmt-per
000785        when cp-pmt-mode = 'N' *> Bi-Monthly ??
000786           move 6                to c-pmts-per-year
000787           move 60               to c-days-in-pmt-per
000788        when other
000789           move 12               to c-pmts-per-year
000790           move 30               to c-days-in-pmt-per
000791     end-evaluate
000792
000793     compute c-tot-pmts = n / 12 * c-pmts-per-year
000794     compute i = cp-loan-apr / c-pmts-per-year / 100
000795     if i = zeros
000796        move .0000000001         to i
000797     end-if
000798     compute anglen  =
000799        (1 - (1 / (1 + I)) ** c-tot-pmts) / I
000800
000801     compute y = (1 + ((CP-DAYS-TO-1ST-PMT * I) /
000802        c-days-in-pmt-per)) / (1 + I)
000803
000804     compute angleny = anglen / y
000805
000806     compute wk3 rounded =   *> tv1
000807        ((c-tot-pmts - anglen + (i * c-tot-pmts))) / i
000808
000809     compute wk6 =           *> tv2
000810        cp-premium-rate / 10 * 12 / c-pmts-per-year / 100
000811
000812     compute wk7 =    *> tv3
000813        (cp-original-benefit / anglen)
000814
000815     compute wk5 rounded = wk7 *wk3 * wk6
000816
000817     move wk5                    to cp-calc-premium
000818     GO TO 99999-RETURN
000819
000820     .
000821 12000-CHECK-SC-CID.
000822
000823     IF (CP-COMPANY-ID = 'CID')
000824        AND (CP-STATE-STD-ABBRV = 'SC')
000825        AND (CP-EARN-AS-NET-PAY)
000826        AND (CP-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
000827        move 6                   to ws-extra-pmts
000828     ELSE
000829        GO TO 12000-CHECK-AL-CID
000830     END-IF
000831
000832     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
000833     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
000834     COMPUTE WK2 = (CP-PREMIUM-RATE * ((M + ((D - 30) / 30))
000835        / 12)) / ((M + 1) / 2)
000836
000837     if i = .0000000001
000838        compute wk4 = cp-original-benefit / n
000839        compute wk3 = wk4 * (((n * (n + 1)) / 2 - ((n - m) *
000840            (n - m + 1)) / 2 + ws-extra-pmts * m) * wk2 / 100)
000841        display ' hit sc wo apr - wk2 ' wk2
000842     else
000843        COMPUTE ANGLEN rounded = (1 - (1 / (1 + I)) ** N) / I
000844        COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
000845        COMPUTE ANGLEN-1 = ANGLEN / Y
000846        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000847        COMPUTE WK3 = WK4 * (((M - (ANGLEN - ANGLEN-M)) / I)
000848           + WS-EXTRA-PMTS * M) * (WK2 / 100) * (1 + I)
000849     end-if
000850
000851*    COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
000852*    COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
000853*    COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
000854*    COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
000855*    COMPUTE ANGLEN-1 = ANGLEN / Y
000856*    COMPUTE WK2 = (CP-PREMIUM-RATE * ((M + ((D - 30) / 30))
000857*       / 12)) / ((M + 1) / 2)
000858*    COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000859*    COMPUTE WK3 = WK4 * (((M - (ANGLEN - ANGLEN-M)) / I)
000860*       + WS-EXTRA-PMTS * M) * (WK2 / 100) * (1 + I)
000861
000862     MOVE WK3 TO CP-CALC-PREMIUM
000863
000864     GO TO 99999-RETURN
000865
000866     .
000867 12000-CHECK-AL-CID.
000868
000869     IF (CP-COMPANY-ID EQUAL 'CID')
000870        AND (CP-BENEFIT-CD = '49' OR '50' OR '53' OR '54')
000871        AND (CP-EARN-AS-NET-PAY)
000872        CONTINUE
000873     ELSE
000874        GO TO 12000-CHECK-NH-CID
000875     END-IF
000876
000877     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
000878     COMPUTE ANGLEM = (1 - (1 / (1 + I)) ** M) / I
000879     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
000880
000881     COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30)* I) / +30)
000882                   / (1 + I)
000883
000884     COMPUTE ANGLEN-1 = ANGLEN / WK8
000885
000886     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000887
000888     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M) + I * M) / I) *
000889        (CP-PREMIUM-RATE / 1000)
000890
000891     MOVE WK3 TO CP-CALC-PREMIUM
000892
000893     GO TO 99999-RETURN
000894
000895     .
000896 12000-CHECK-NH-CID.
000897
000898     IF (CP-COMPANY-ID EQUAL 'CID')
000899        AND (CP-STATE-STD-ABBRV = 'NH' OR 'AK')
000900        AND (CP-EARN-AS-NET-PAY)
000901        CONTINUE
000902     ELSE
000903        GO TO 12000-CHECK-PA-CID
000904     END-IF
000905
000906     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
000907     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
000908
000909     COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30)* I) / +30)
000910                   / (1 + I)
000911
000912     COMPUTE ANGLEN-1 = ANGLEN / WK8
000913
000914     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000915
000916     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
000917        (1 + (2 * I)) * (CP-PREMIUM-RATE / 1000)
000918
000919     MOVE WK3 TO CP-CALC-PREMIUM
000920
000921     GO TO 99999-RETURN
000922
000923     .
000924 12000-CHECK-PA-CID.
000925
000926     IF (CP-COMPANY-ID EQUAL 'CID')
000927        AND (CP-STATE-STD-ABBRV EQUAL 'PA')
000928        AND (CP-EARN-AS-NET-PAY)
000929        CONTINUE
000930     ELSE
000931        GO TO 12000-CHECK-ME-CID
000932     END-IF
000933
000934
000935     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
000936     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
000937     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
000938     COMPUTE ANGLEN-1 = ANGLEN / Y
000939     COMPUTE WK5 = 1 / (10 * (1 + (WS-WORK-DIS-RATE / 24) * N))
000940     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000941     COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100
000942     COMPUTE WK3 = WK4 * ((N - ANGLEN) / I) * WS-WORK-RATE
000943        * WK5
000944
000945     MOVE WK3 TO CP-CALC-PREMIUM
000946
000947     GO TO 99999-RETURN
000948
000949     .
000950 12000-CHECK-ME-CID.
000951
000952     IF (CP-COMPANY-ID EQUAL 'CID')
000953        AND (CP-STATE-STD-ABBRV EQUAL 'ME')
000954        AND (CP-EARN-AS-NET-PAY)
000955        CONTINUE
000956     ELSE
000957        GO TO 12000-CHECK-VT-TRUNC
000958     END-IF
000959
000960     COMPUTE WS-WORK-RATE = CP-PREMIUM-RATE / +1000
000961
000962     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
000963
000964     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
000965     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
000966     COMPUTE ANGLEN-1 = ANGLEN / Y
000967
000968     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
000969
000970     COMPUTE WK5 = 1 / (1 + (WS-WORK-DIS-RATE * N / 24))
000971
000972     COMPUTE WK3 = WK4 * ((N - ANGLEN) / I) * WS-WORK-RATE
000973        * WK5
000974
000975     MOVE WK3 TO CP-CALC-PREMIUM
000976
000977     GO TO 99999-RETURN
000978
000979     .
000980 12000-CHECK-VT-TRUNC.
000981
000982     IF (CP-COMPANY-ID EQUAL 'NCL') AND
000983        (CP-STATE-STD-ABBRV EQUAL 'VT') AND
000984        (CP-EARN-AS-NET-PAY) AND
000985        (CP-TRUNCATED-LIFE)
000986        NEXT SENTENCE
000987     ELSE
000988        GO TO 12000-CHECK-VT.
000989
000990      COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +1000.
000991
000992      COMPUTE ANGLEN EQUAL WK1 / I.
000993
000994      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
000995
000996      COMPUTE WK8 EQUAL (1 / (1 + I)) ** N.
000997      COMPUTE WK9 EQUAL
000998        (1 - ((1 / (1 + ((.0054 - I) / (1 + I)))) ** (M - 1))) /
000999        ((.0054 - I) / (1 + I)).
001000      COMPUTE WK7 EQUAL
001001        (1 - (1 / 1.0054) ** (M - 1)) / .0054.
001002      COMPUTE WK3 EQUAL  CP-ORIGINAL-BENEFIT * WS-WORK-RATE *
001003        ((WK7 + 1) - (WK8 * (WK9 + 1))) / (ANGLEN-1 * I).
001004
001005      MOVE WK3 TO CP-CALC-PREMIUM.
001006
001007     GO TO 99999-RETURN.
001008
001009 12000-CHECK-VT.
001010
001011     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001012        (CP-STATE-STD-ABBRV EQUAL 'VT') AND
001013        (CP-EARN-AS-NET-PAY)
001014        NEXT SENTENCE
001015     ELSE
001016        GO TO 12000-CHECK-MINN-WDS.
001017
001018      COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +1000.
001019
001020      COMPUTE ANGLEN EQUAL WK1 / I.
001021
001022      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001023
001024      COMPUTE WK3 EQUAL ((CP-ORIGINAL-BENEFIT * WS-WORK-RATE) /
001025                        (ANGLEN-1 * I)) *
001026
001027                        (((1.0054 ** N - 1) /
001028                        (1.0054 ** (N - 1) * .0054)) -
001029
001030                        ((V * (1 - 1.0054 ** N * VX)) /
001031                        (1.0054 ** (N - 1) * (1 - 1.0054 * V)))).
001032
001033      MOVE WK3 TO CP-CALC-PREMIUM.
001034
001035     GO TO 99999-RETURN.
001036
001037 12000-CHECK-MINN-WDS.
001038
001039     IF (CP-COMPANY-ID EQUAL 'WDS') AND
001040        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
001041        (CP-EARN-AS-NET-PAY)
001042        NEXT SENTENCE
001043     ELSE
001044        GO TO 12000-CHECK-MINN-BALL-LEV.
001045
001046     COMPUTE ANGLEN EQUAL WK1 / I.
001047     COMPUTE WK3 EQUAL (CP-PREMIUM-RATE / +100) *
001048              (N / +12) * (2 / (N + 1)).
001049     COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT / ANGLEN) *
001050         WK3 * ((N - ANGLEN) / I).
001051
001052      MOVE WK3 TO CP-CALC-PREMIUM.
001053
001054     GO TO 99999-RETURN.
001055
001056 12000-CHECK-MINN-BALL-LEV.
001057
001058     IF (CP-COMPANY-ID EQUAL 'WDS') AND
001059        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
001060        (CP-DEVIATION-CODE EQUAL 'LEV') AND
001061        (CP-EARN-AS-REG-BALLOON)
001062        NEXT SENTENCE
001063     ELSE
001064        GO TO 12000-CHECK-MINN-BALL.
001065
001066     COMPUTE ANGLEN EQUAL WK1 / I.
001067
001068     COMPUTE WK3 EQUAL 1 / (1 + I) ** N.
001069
001070     COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE /
001071        +100 * N / 12 * 2 / (N + 1) * (ANGLEN / WK3).
001072
001073     MOVE WK3 TO CP-CALC-PREMIUM.
001074
001075     GO TO 99999-RETURN.
001076
001077 12000-CHECK-MINN-BALL.
001078
001079     IF (CP-COMPANY-ID EQUAL 'WDS') AND
001080        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
001081        (CP-EARN-AS-REG-BALLOON)
001082        NEXT SENTENCE
001083     ELSE
001084        GO TO 12000-CHECK-CID-MN-BALL.
001085
001086     COMPUTE ANGLEN EQUAL WK1 / I.
001087     COMPUTE ANGLEN-1 EQUAL WK6 / I.
001088
001089     COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE
001090     / +100 * (N - 1) / 12 * 2 / N *
001091     (((N - 1) - ANGLEN-1) / (I * ANGLEN-1)).
001092
001093      MOVE WK3 TO CP-CALC-PREMIUM.
001094
001095     GO TO 99999-RETURN.
001096
001097 12000-CHECK-CID-MN-BALL.
001098
001099     IF (CP-COMPANY-ID EQUAL 'CID')
001100        AND (CP-STATE-STD-ABBRV = 'MN')
001101        AND (CP-CERT-EFF-DT > X'A4FF')
001102        AND (CP-EARN-AS-REG-BALLOON)
001103        CONTINUE
001104     ELSE
001105        GO TO 12000-CHECK-CID-MN-FARM
001106     END-IF
001107
001108     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
001109     COMPUTE ANGLEM = (1 - (1 / (1 + I)) ** M) / I
001110     COMPUTE ANGLEMP1 = (1 - (1 / (1 + I)) ** (M + 1)) / I
001111     COMPUTE ANGLEM-1 = (1 - (1 / (1 + I)) ** (M - 1)) / I
001112     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M + 1)) / I
001113     COMPUTE V = 1 / (1 + I)
001114     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
001115     COMPUTE Y = (1 + (D * I) / +30)
001116        / (1 + I)
001117
001118
001119     COMPUTE CP-LF-BALLOON-PREM = CP-ALTERNATE-BENEFIT * (M + 1)
001120        * CP-PREMIUM-RATE / 1000
001121
001122*    IF CP-MONTHLY-PAYMENT NOT = ZEROS
001123*       MOVE CP-MONTHLY-PAYMENT  TO GR
001124*    ELSE
001125*       COMPUTE WK6 = CP-ORIGINAL-BENEFIT + CP-ALTERNATE-BENEFIT
001126*          * (ANGLEM / Y) * (CP-PREMIUM-RATE / 1000) -
001127*          CP-ALTERNATE-BENEFIT * V ** (M + ((D - 30) / 30))
001128*       DISPLAY ' BALLOON TOP ' WK6
001129*       COMPUTE WK7 = (ANGLEM-1 / Y) - ((M - 1) - ANGLEM-1) / I
001130*          * (CP-PREMIUM-RATE / 1000) * (1 + I) - ((M - 1) * 6.65)
001131*          / 100
001132*       DISPLAY ' BALLOON BOTTOM ' WK7
001133*       COMPUTE GR = WK6 / WK7
001134*       DISPLAY ' BALLOON PAYMENT ' GR
001135*    END-IF
001136
001137     COMPUTE WK8 = (1 + I) ** ((M + 1) * -1)
001138     COMPUTE CP-ORIGINAL-BENEFIT ROUNDED = (CP-ORIGINAL-BENEFIT
001139        + CP-ALTERNATE-BENEFIT) - (CP-ALTERNATE-BENEFIT * WK8)
001140     COMPUTE GR = CP-ORIGINAL-BENEFIT / ANGLEM
001141
001142     COMPUTE WK3 = (GR * ((M - ANGLEM) / I) * (1 + I)
001143        + (CP-ALTERNATE-BENEFIT * ANGLEMP1) -
001144           (CP-ALTERNATE-BENEFIT * (M + 1)))
001145           * CP-PREMIUM-RATE / 1000
001146
001147     MOVE WK3                    TO CP-CALC-PREMIUM
001148
001149     GO TO 99999-RETURN.
001150
001151     .
001152 12000-CHECK-CID-MN-FARM.
001153
001154     IF (CP-COMPANY-ID EQUAL 'CID')
001155        AND (CP-STATE-STD-ABBRV = 'MN' or 'WA')
001156        AND (CP-CERT-EFF-DT > X'A4FF')
001157        AND (CP-EARN-AS-FARM-PLAN)
001158        CONTINUE
001159     ELSE
001160        GO TO 12000-CHECK-CID-NET-BALLoons
001161     END-IF
001162
001163     EVALUATE CP-PMT-MODE
001164        WHEN 'B'
001165           MOVE +14              TO C-DAYS-IN-PMT-PER
001166           MOVE +26              TO C-PMTS-PER-YEAR
001167           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
001168        WHEN 'S'
001169           MOVE +15              TO C-DAYS-IN-PMT-PER
001170           MOVE +24              TO C-PMTS-PER-YEAR
001171           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
001172        WHEN 'T'
001173           MOVE +360             TO C-DAYS-IN-PMT-PER
001174           MOVE +1               TO C-PMTS-PER-YEAR
001175           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
001176        WHEN 'W'
001177           MOVE +7               TO C-DAYS-IN-PMT-PER
001178           MOVE +52              TO C-PMTS-PER-YEAR
001179           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
001180        WHEN OTHER
001181           move cp-no-of-pmts    to c-tot-pmts
001182           if cp-pay-frequency > zeros
001183              compute c-pmts-per-year =
001184                 12 / cp-pay-frequency
001185              compute c-days-in-pmt-per = 360 / c-pmts-per-year
001186           else
001187              MOVE +30           TO C-DAYS-IN-PMT-PER
001188              MOVE +12           TO C-PMTS-PER-YEAR
001189              MOVE CP-LOAN-TERM  TO C-TOT-PMTS
001190           end-if
001191     END-EVALUATE
001192
001193     COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
001194     if i = zeros
001195        move .0000000001         to i
001196     end-if
001197
001198     COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * I /
001199        C-DAYS-IN-PMT-PER)) / (1 + I)
001200
001201     compute cp-premium-rate =
001202        cp-premium-rate * 12 / c-pmts-per-year / 10
001203     move c-tot-pmts to n m
001204     COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** N) / I
001205     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
001206     COMPUTE ANGLEN-1 = ANGLEN / Y
001207     COMPUTE WK3 = CP-ORIGINAL-BENEFIT / ANGLEN-1
001208
001209     compute wk3 = wk3 * ((m - (anglen - anglen-m)) / i) *
001210        (cp-premium-rate / 100)
001211
001212
001213     MOVE WK3                    TO CP-CALC-PREMIUM
001214
001215     GO TO 99999-RETURN.
001216
001217     .
001218 12000-check-cid-net-balloons.
001219
001220     IF (CP-COMPANY-ID EQUAL 'CID')
001221        AND (CP-EARN-AS-REG-BALLOON)
001222        and (CP-NET-ONLY-STATE not = 'Y')
001223        AND (CP-BEN-CATEGORY = 'P' OR 'I')
001224        CONTINUE
001225     ELSE
001226        GO TO 12000-check-cid-net-only-balls
001227     END-IF
001228
001229     display ' *** NOT NET ONLY STATE BALLOONS *** '
001230     COMPUTE Y =
001231        (1 + (CP-DAYS-TO-1ST-PMT * I / 30)) / (1 + I)
001232
001233     COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** N) / I
001234     COMPUTE ANGLEM   = (1 - (1 / (1 + I)) ** M) / I
001235     COMPUTE ANGLEM-1   = (1 - (1 / (1 + I)) ** (M - 1)) / I
001236     COMPUTE ANGLEN-MP1 = (1 - (1 / (1 + I)) ** (N - M + 1)) / I
001237     COMPUTE ANGLEMY = ANGLEM / Y
001238     COMPUTE ANGLEM-1Y = ANGLEM-1 / Y
001239     COMPUTE ANGLENY = ANGLEN / Y
001240     COMPUTE ANGLEN-MP1Y = ANGLEN-MP1 / Y
001241
001242     compute cp-original-benefit =
001243        cp-original-benefit + cp-alternate-benefit
001244     COMPUTE PVBALLOON rounded =
001245        CP-ALTERNATE-BENEFIT / ((1 + I) ** (N + 1))
001246     compute wk4 rounded =
001247        cp-original-benefit - pvballoon
001248     compute wk6 rounded =
001249        (cp-original-benefit - cp-alternate-benefit)
001250
001251***  compute ra = wk4 / anglem-1y
001252     compute ra = wk4 / anglemy
001253     compute wk6 rounded = wk6 / anglemy
001254     display ' rate before ' cp-premium-rate
001255
001256
001257     compute cp-premium-rate =
001258        (cp-premium-rate * ((m + (cp-days-to-1st-pmt - 30)
001259            / 30) / 12))     /
001260           ((m + 1) / 2)
001261
001262     display '************************************'
001263     display ' orign ben    ' cp-original-benefit
001264     display ' balloon      ' cp-alternate-benefit
001265     display ' ra           ' ra
001266     display ' wk6          ' wk6
001267     display ' m            ' m
001268     display ' anglemy      ' anglemy
001269     display ' anglem-1     ' anglem-1y
001270     display ' i            ' i
001271     display ' extra per    ' cp-extra-periods
001272     display ' prem rate    ' cp-premium-rate
001273
001274
001275***  compute wk3 = ra * ((m - 1) - anglem-1y) / i * (1 +
001276***     (cp-extra-periods * i)) * cp-premium-rate / 100
001277
001278     compute wk3 = wk6 * (m - anglem) / i * (1 +
001279        (cp-extra-periods * i)) * cp-premium-rate / 100
001280
001281     move wk3                    to cp-calc-premium
001282     display ' wk3          ' wk3
001283     display '************************************'
001284*    compute wk3 rounded =
001285*       cp-alternate-benefit * (m + 1) * cp-premium-rate / 100
001286*    move wk3                    to cp-lf-balloon-prem
001287
001288     GO TO 99999-RETURN.
001289
001290     .
001291 12000-check-cid-net-only-balls.
001292
001293     IF (CP-COMPANY-ID EQUAL 'CID')
001294        AND (CP-EARN-AS-REG-BALLOON)
001295        AND (CP-BEN-CATEGORY = 'P' OR 'I')
001296        CONTINUE
001297     ELSE
001298        GO TO 12000-CHECK-NC-WDS
001299     END-IF
001300
001301     display ' *** NET ONLY STATE BALLOONS *** '
001302
001303     COMPUTE Y =
001304        (1 + (CP-DAYS-TO-1ST-PMT * I / 30)) / (1 + I)
001305
001306     COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** N) / I
001307     COMPUTE ANGLEM   = (1 - (1 / (1 + I)) ** M) / I
001308     COMPUTE ANGLEM-1   = (1 - (1 / (1 + I)) ** (M - 1)) / I
001309     COMPUTE ANGLEN-MP1 = (1 - (1 / (1 + I)) ** (N - M + 1)) / I
001310     COMPUTE ANGLEMY = ANGLEM / Y
001311     COMPUTE ANGLEM-1Y = ANGLEM-1 / Y
001312     COMPUTE ANGLENY = ANGLEN / Y
001313     COMPUTE ANGLEN-MP1Y = ANGLEN-MP1 / Y
001314
001315
001316     COMPUTE PVBALLOON rounded =
001317        CP-ALTERNATE-BENEFIT / ((1 + I) ** (N + 1))
001318     compute wk4 rounded =
001319        cp-original-benefit - pvballoon
001320
001321
001322*    compute cp-premium-rate =
001323*       (cp-premium-rate * (m + 1) / 12)  /
001324*          ((m + 2) / 2)
001325
001326
001327***  compute ra = wk4 / anglem-1y
001328     compute ra = (cp-original-benefit + cp-alternate-benefit -
001329        pvballoon) / anglemy
001330
001331     display '************************************'
001332     display ' ra           ' ra
001333     display ' m            ' m
001334     display ' anglemy      ' anglemy
001335     display ' anglem-1     ' anglem-1y
001336     display ' i            ' i
001337     display ' extra per    ' cp-extra-periods
001338     display ' prem rate    ' cp-premium-rate
001339     display '************************************'
001340
001341***  compute wk3 = ra * ((m - 1) - anglem-1y) / i * (1 +
001342***     (cp-extra-periods * i)) * cp-premium-rate / 100
001343
001344     compute wk3 = ra * (m - anglemy) / i * (1 +
001345        (cp-extra-periods * i)) * cp-premium-rate / 100
001346
001347     move wk3                    to cp-calc-premium
001348     compute wk3 rounded =
001349        cp-alternate-benefit * (m + 1) * cp-premium-rate / 100
001350     move wk3                    to cp-lf-balloon-prem
001351
001352     GO TO 99999-RETURN.
001353
001354     .
001355 12000-CHECK-NC-WDS.
001356
001357     IF (CP-COMPANY-ID EQUAL 'WDS') AND
001358        (CP-STATE-STD-ABBRV EQUAL 'NC') AND
001359        (CP-EARN-AS-NET-PAY)
001360        NEXT SENTENCE
001361     ELSE
001362        GO TO 12000-CHECK-NC-BALL-LEV.
001363
001364     COMPUTE WK4 EQUAL N * (1 + I * 2).
001365     COMPUTE ANGLEN EQUAL WK1 / I.
001366     COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT *
001367              CP-PREMIUM-RATE / +100 *
001368              N / +12 * 2 / (N + 1) *
001369              (WK4 - ANGLEN) / (I * ANGLEN).
001370
001371      MOVE WK3 TO CP-CALC-PREMIUM.
001372
001373     GO TO 99999-RETURN.
001374
001375 12000-CHECK-NC-BALL-LEV.
001376
001377     IF (CP-COMPANY-ID EQUAL 'WDS') AND
001378        (CP-STATE-STD-ABBRV EQUAL 'NC') AND
001379        (CP-DEVIATION-CODE EQUAL 'LEV') AND
001380        (CP-EARN-AS-REG-BALLOON)
001381        NEXT SENTENCE
001382     ELSE
001383        GO TO 12000-CHECK-NC-BALL.
001384
001385     COMPUTE ANGLEN EQUAL WK1 / I.
001386
001387**** VX = 1 / (1 + I)**N.
001388
001389     COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE /
001390        +100 * N / 12 * 2 / (N + 1) * (ANGLEN / VX).
001391
001392     MOVE WK3 TO CP-CALC-PREMIUM.
001393
001394     GO TO 99999-RETURN.
001395
001396 12000-CHECK-NC-BALL.
001397
001398     IF (CP-COMPANY-ID EQUAL 'WDS') AND
001399        (CP-STATE-STD-ABBRV EQUAL 'NC') AND
001400        (CP-EARN-AS-REG-BALLOON)
001401        NEXT SENTENCE
001402     ELSE
001403        GO TO 12000-CHECK-MINN-TRUNC.
001404
001405     COMPUTE WK4 EQUAL (N - 1) * (1 + I * 2).
001406     COMPUTE ANGLEN EQUAL WK1 / I.
001407     COMPUTE ANGLEN-1 EQUAL WK6 / I.
001408**** VX = 1 / (1 + I)**N.
001409
001410     COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE
001411     / +100 * (N - 1) / 12 * 2 / N *
001412     (WK4 - ANGLEN-1) / (I * ANGLEN-1).
001413
001414      MOVE WK3 TO CP-CALC-PREMIUM.
001415
001416     GO TO 99999-RETURN.
001417
001418 12000-CHECK-MINN-TRUNC.
001419
001420     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001421        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
001422        (M GREATER THAN +63) AND
001423        (CP-EARN-AS-NET-PAY) AND
001424        (CP-TRUNCATED-LIFE)
001425        NEXT SENTENCE
001426     ELSE
001427        GO TO 12000-CHECK-MINN.
001428
001429      COMPUTE ANGLEN EQUAL WK1 / I.
001430      COMPUTE ANGLEN-M EQUAL WK5 / I.
001431
001432      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001433
001434      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
001435       (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
001436       (((M - ANGLEN + ANGLEN-M) / I) + (2 * M)).
001437
001438      MOVE WK3 TO CP-CALC-PREMIUM.
001439
001440     GO TO 99999-RETURN.
001441
001442 12000-CHECK-MINN.
001443
001444     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001445        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
001446        (M GREATER THAN +63) AND
001447        (CP-EARN-AS-NET-PAY)
001448        NEXT SENTENCE
001449     ELSE
001450        GO TO 12000-CHECK-MONTANA-TRUNC.
001451
001452      COMPUTE ANGLEN EQUAL WK1 / I.
001453
001454      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001455
001456      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
001457       (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
001458       (((N - ANGLEN) / I) + (2 * N)).
001459
001460      MOVE WK3 TO CP-CALC-PREMIUM.
001461
001462     GO TO 99999-RETURN.
001463
001464 12000-CHECK-MONTANA-TRUNC.
001465
001466     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001467        (CP-STATE-STD-ABBRV EQUAL 'MT') AND
001468        (M GREATER THAN +63) AND
001469        (CP-EARN-AS-NET-PAY) AND
001470        (CP-TRUNCATED-LIFE)
001471        NEXT SENTENCE
001472     ELSE
001473        GO TO 12000-CHECK-MONTANA.
001474
001475      COMPUTE ANGLEN EQUAL WK1 / I.
001476      COMPUTE ANGLEN-M EQUAL WK5 / I.
001477
001478      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001479
001480      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT *
001481      ((CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
001482       (((M - ANGLEN + ANGLEN-M) / I) + (4 * M)).
001483
001484      MOVE WK3 TO CP-CALC-PREMIUM.
001485
001486     GO TO 99999-RETURN.
001487
001488 12000-CHECK-MONTANA.
001489
001490     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001491        (CP-STATE-STD-ABBRV EQUAL 'MT') AND
001492        (M GREATER THAN +63) AND
001493        (CP-EARN-AS-NET-PAY)
001494        NEXT SENTENCE
001495     ELSE
001496        GO TO 12001-CONTINUE-NCL-TRUNC.
001497
001498      COMPUTE ANGLEN EQUAL WK1 / I.
001499
001500      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001501
001502      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
001503       (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
001504       (((N - ANGLEN) / I) + (4 * N)).
001505
001506      MOVE WK3 TO CP-CALC-PREMIUM.
001507
001508     GO TO 99999-RETURN.
001509
001510 12001-CONTINUE-NCL-TRUNC.
001511
001512     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001513        (CP-EARN-AS-NET-PAY) AND
001514        (CP-TRUNCATED-LIFE)
001515        NEXT SENTENCE
001516     ELSE
001517        GO TO 12001-CONTINUE-NCL.
001518
001519      COMPUTE ANGLEN EQUAL WK1 / I.
001520      COMPUTE ANGLEN-M EQUAL WK5 / I.
001521
001522      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001523
001524      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT * 2 *
001525      (M - (ANGLEN - ANGLEN-M)) * (CP-PREMIUM-RATE / +100)) /
001526      (ANGLEN-1 * (N + 1) * I).
001527
001528      MOVE WK3 TO CP-CALC-PREMIUM.
001529
001530     GO TO 99999-RETURN.
001531
001532 12001-CONTINUE-NCL.
001533
001534     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001535        (CP-STATE-STD-ABBRV EQUAL 'NC') AND
001536        (CP-EARN-AS-NET-PAY)
001537        GO TO 12001-CHECK-NC-NET-PLUS3.
001538
001539     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001540        (CP-STATE-STD-ABBRV EQUAL 'CA') AND
001541        (CP-EARN-AS-NET-PAY)
001542        GO TO 12001-CHECK-CA.
001543
001544     IF (CP-COMPANY-ID EQUAL 'NCL') AND
001545        (CP-EARN-AS-NET-PAY)
001546        NEXT SENTENCE
001547     ELSE
001548        GO TO 12000-CONTINUE-NET.
001549
001550      COMPUTE ANGLEN EQUAL WK1 / I.
001551      MOVE ANGLEN                TO ANGLEN-1.
001552
001553      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
001554
001555      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
001556       (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
001557       ((N - ANGLEN) / I).
001558
001559      MOVE WK3 TO CP-CALC-PREMIUM.
001560
001561     GO TO 99999-RETURN.
001562
001563 12000-CONTINUE-NET.
001564
001565     IF (CP-COMPANY-ID EQUAL 'TMS') AND
001566        (CP-STATE-STD-ABBRV EQUAL 'MN' OR 'CA' OR 'WA') AND
001567        (CP-EARN-AS-NET-PAY)
001568        NEXT SENTENCE
001569     ELSE
001570        GO TO 12001-CONTINUE-MINN.
001571
001572      COMPUTE ANGLEN EQUAL WK6 / I.
001573      COMPUTE ODF EQUAL 1 + (12 * CP-TERM-OR-EXT-DAYS / +365).
001574
001575      COMPUTE WK7 EQUAL (1 + ODF * I) / (1 + ANGLEN).
001576      COMPUTE WK3 EQUAL (M  * WK7) - 1.
001577      COMPUTE WK3 EQUAL (WK3 / I) *
001578       ((M * (CP-PREMIUM-RATE / +100)) / (+6 * (M + 1))).
001579
001580      IF CP-R-MAX-TOT-BEN NOT NUMERIC
001581         MOVE +0 TO CP-R-MAX-TOT-BEN.
001582
001583      COMPUTE WK3 EQUAL (WK3 * CP-ORIGINAL-BENEFIT)
001584
001585      IF (CP-ORIGINAL-BENEFIT GREATER THAN CP-R-MAX-TOT-BEN) AND
001586         (CP-R-MAX-TOT-BEN NOT EQUAL +0)
001587         COMPUTE WK3 EQUAL (WK3 * CP-R-MAX-TOT-BEN).
001588
001589      MOVE WK3 TO CP-CALC-PREMIUM.
001590
001591     GO TO 99999-RETURN.
001592
001593 12001-CONTINUE-MINN.
001594
001595     IF (CP-COMPANY-ID EQUAL 'GTL') AND
001596        (CP-STATE-STD-ABBRV EQUAL 'NJ') AND
001597        (CP-TRUNCATED-LIFE)
001598        NEXT SENTENCE
001599     ELSE
001600        GO TO 12001-CHECK-RI-GTL.
001601
001602      COMPUTE ANGLEN EQUAL WK1 / I.
001603      COMPUTE ANGLEM EQUAL WK2 / I.
001604
001605      COMPUTE WK3 EQUAL ((M - ANGLEM * SX) /
001606       (I * ANGLEN) * CP-PREMIUM-RATE / (1 + WS-WORK-DIS-RATE
001607       * M / 2400) * 1000 + .5) / 1000000.
001608
001609      COMPUTE CP-CALC-PREMIUM EQUAL WK3 * (CP-ORIGINAL-BENEFIT
001610       - CP-ORIGINAL-PREMIUM) / (1 - WK3).
001611
001612     GO TO 99999-RETURN.
001613
001614 12001-CHECK-RI-GTL.
001615
001616     IF (CP-COMPANY-ID EQUAL 'GTL' ) AND
001617        (CP-STATE-STD-ABBRV EQUAL 'RI') AND
001618        (CP-EARN-AS-NET-PAY)
001619        NEXT SENTENCE
001620     ELSE
001621        GO TO 12001-CHECK-RI-LEASE-GTL.
001622
001623      COMPUTE ANGLEN EQUAL WK1 / I.
001624*     COMPUTE ANGLEM EQUAL WK2 / I.
001625
001626      COMPUTE WK3 EQUAL (1000 * I * ANGLEN) *
001627               (1 + WS-WORK-DIS-RATE * N).
001628      COMPUTE CP-CALC-PREMIUM EQUAL CP-ORIGINAL-BENEFIT *
001629         ((N - ANGLEN) * (1 + 2 * I) * CP-PREMIUM-RATE) / WK3.
001630
001631
001632     GO TO 99999-RETURN.
001633
001634 12001-CHECK-RI-LEASE-GTL.
001635
001636     IF (CP-COMPANY-ID EQUAL 'GTL' ) AND
001637        (CP-STATE-STD-ABBRV EQUAL 'RI') AND
001638        (CP-SPECIAL-CALC-CD EQUAL 'H')
001639        NEXT SENTENCE
001640     ELSE
001641        GO TO 12001-CHECK-OR-GTL.
001642
001643     COMPUTE ANGLEN EQUAL WK1 / I.
001644
001645     COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT / ANGLEN *
001646        (N - ANGLEN) * CP-PREMIUM-RATE * (1 + 2 * I))
001647                        /
001648        (1000 * I * (1 + .0021 * N)).
001649     COMPUTE CP-CALC-PREMIUM EQUAL WK3 +
001650       (CP-ALTERNATE-BENEFIT * CP-PREMIUM-RATE * (N + 1))
001651                            /
001652       (1000 * (1 + .0027 * (N + 1))).
001653
001654     GO TO 99999-RETURN.
001655
001656 12001-CHECK-OR-GTL.
001657
001658     IF (CP-COMPANY-ID EQUAL 'GTL' ) AND
001659        (CP-STATE-STD-ABBRV EQUAL 'OR') AND
001660        (CP-EARN-AS-NET-PAY)
001661        NEXT SENTENCE
001662     ELSE
001663        GO TO 12001-CHECK-GA-GTL.
001664
001665     COMPUTE ANGLEN-M EQUAL WK5 / I.
001666     COMPUTE ANGLEN EQUAL WK1 / I.
001667
001668     IF M LESS THAN +64
001669        COMPUTE WK3 EQUAL (((N - (N - M)) - (ANGLEN - ANGLEN-M)) *
001670         (1 + I) * CP-PREMIUM-RATE * (N - (N - M)))
001671                   /
001672         (I * ANGLEN * (N - (N - M) + 1) * 600)
001673        COMPUTE CP-CALC-PREMIUM EQUAL WK3 * CP-ORIGINAL-BENEFIT
001674     ELSE
001675        COMPUTE WK3 EQUAL (((N - (N - M)) - (ANGLEN - ANGLEN-M)) *
001676        CP-PREMIUM-RATE * (1 + I))
001677                  /
001678        (+1000 * I * ANGLEN)
001679        COMPUTE CP-CALC-PREMIUM EQUAL WK3 * CP-ORIGINAL-BENEFIT.
001680*    MOVE WK3 TO CP-CALC-PREMIUM.
001681     GO TO 99999-RETURN.
001682
001683 12001-CHECK-GA-GTL.
001684
001685     IF (CP-COMPANY-ID EQUAL 'GTL') AND
001686        (CP-STATE-STD-ABBRV EQUAL 'GA') AND
001687        (CP-EARN-AS-NET-PAY)
001688*       (CP-TRUNCATED-LIFE)
001689        NEXT SENTENCE
001690     ELSE
001691        GO TO 12001-CHECK-MA-GTL.
001692
001693     COMPUTE ANGLEN-M EQUAL WK5 / I.
001694     COMPUTE ANGLEN EQUAL WK1 / I.
001695
001696     COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT / ANGLEN.
001697     COMPUTE WK3 EQUAL (CP-PREMIUM-RATE / +1000) * WK3 *
001698      ((N - (ANGLEN - ANGLEN-M)) / I) * (1 + 2 * I).
001699     MOVE WK3 TO CP-CALC-PREMIUM.
001700
001701     GO TO 99999-RETURN.
001702
001703 12001-CHECK-MA-GTL.
001704
001705     IF (CP-COMPANY-ID EQUAL 'GTL') AND
001706        (CP-STATE-STD-ABBRV EQUAL 'MA') AND
001707        (CP-EARN-AS-NET-PAY) AND
001708        (CP-SPECIAL-CALC-CD EQUAL 'H')
001709        NEXT SENTENCE
001710     ELSE
001711        GO TO 12001-CHECK-NV-NET.
001712
001713     COMPUTE ANGLEN EQUAL WK1 / I.
001714
001715     COMPUTE WK3 EQUAL (CP-PREMIUM-RATE / +1000) *
001716      CP-ALTERNATE-BENEFIT * N.
001717
001718     COMPUTE WK7 EQUAL (CP-ORIGINAL-BENEFIT + WK3) /
001719     (ANGLEN - (CP-PREMIUM-RATE / +1000) * (N - ANGLEN) / I).
001720
001721     COMPUTE WK3 EQUAL WK3 + (CP-PREMIUM-RATE / +1000) *
001722     WK7 * (N - ANGLEN) / I.
001723     MOVE WK3 TO CP-CALC-PREMIUM.
001724
001725     GO TO 99999-RETURN.
001726
001727 12001-CHECK-NV-NET.
001728
001729     IF (CP-COMPANY-ID EQUAL 'CID')
001730        AND (CP-STATE-STD-ABBRV = 'NV' OR 'WA')
001731        AND (CP-EARN-AS-NET-PAY)
001732        CONTINUE
001733     ELSE
001734        GO TO 12001-CHECK-MN-NET
001735     END-IF
001736
001737     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
001738     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
001739
001740     COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
001741                   / (1 + I)
001742
001743     COMPUTE ANGLEN-1 = ANGLEN / WK8
001744
001745     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
001746
001747     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
001748        (CP-PREMIUM-RATE * K-I / 1000)
001749
001750     MOVE WK3                    TO CP-CALC-PREMIUM
001751
001752     GO TO 99999-RETURN
001753
001754      .
001755 12001-CHECK-MN-NET.
001756
001757     IF (CP-COMPANY-ID EQUAL 'CID')
001758        AND (CP-STATE-STD-ABBRV = 'MN')
001759        AND (CP-EARN-AS-NET-PAY)
001760        AND (NOT CP-LEVEL-LIFE)
001761        AND (WS-DISCOUNT-OPTION = '1')
001762        CONTINUE
001763     ELSE
001764        GO TO 12001-CHECK-NC-NET
001765     END-IF
001766
001767     if i = .0000000001
001768        compute wk4 = cp-original-benefit / n
001769        compute wk3 = wk4 * ((n * (n + 1) / 2) - ((n - m) *
001770           (n - m + 1) / 2))*(cp-premium-rate / 1000)
001771        move wk3 to cp-calc-premium
001772        go to 99999-return
001773     end-if
001774
001775     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
001776     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
001777
001778     COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
001779                   / (1 + I)
001780
001781     COMPUTE ANGLEN-1 = ANGLEN / WK8
001782
001783     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
001784
001785     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
001786        (CP-PREMIUM-RATE / 1000) * (1 + I)
001787
001788     MOVE WK3                    TO CP-CALC-PREMIUM
001789
001790     GO TO 99999-RETURN
001791
001792      .
001793 12001-CHECK-NC-NET.
001794
001795     IF (CP-COMPANY-ID EQUAL 'NCB' ) AND
001796        (CP-STATE-STD-ABBRV EQUAL 'NC') AND
001797        (CP-EARN-AS-NET-PAY)
001798        NEXT SENTENCE
001799     ELSE
001800        GO TO 12001-CHECK-NC-NET-PLUS3.
001801
001802      COMPUTE ANGLEN EQUAL WK1 / I.
001803      COMPUTE ANGLEN-M EQUAL WK5 / I.
001804
001805      COMPUTE WK6 ROUNDED = CP-ORIGINAL-BENEFIT /
001806          (ANGLEN - (CP-PREMIUM-RATE / 1000) *
001807          ((N - (ANGLEN - ANGLEN-M)) / I + 3 * N) -
001808          N * (CP-PREMIUM-RATE / 1000)).
001809
001810      COMPUTE WK3 ROUNDED = (CP-PREMIUM-RATE / 1000) * WK6 *
001811          ((N - (ANGLEN - ANGLEN-M)) / I + 3 * N).
001812
001813      COMPUTE WK7 ROUNDED = (1 - (CP-PREMIUM-RATE / 1000) *
001814          ((N - ANGLEN) / I + 3) / ANGLEN) /
001815          (1 - (CP-PREMIUM-RATE / 1000) * N * (N + 1) /
001816          (ANGLEN * 2))
001817
001818      COMPUTE WK7 ROUNDED = WK7 * N - 3.
001819
001820      IF ANGLEN IS NOT LESS THAN WK7
001821          COMPUTE CP-PREMIUM-RATE ROUNDED =
001822              (CP-PREMIUM-RATE / 1000) * +6.5 * (N / 12)
001823          COMPUTE WK3 ROUNDED =
001824              (CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE).
001825
001826     MOVE WK3 TO CP-CALC-PREMIUM.
001827
001828     GO TO 99999-RETURN.
001829
001830 12001-CHECK-NC-NET-PLUS3.
001831
001832     IF (CP-STATE-STD-ABBRV EQUAL 'NC') AND
001833        (CP-EARN-AS-NET-PAY) AND
001834        (CP-COMPANY-ID NOT = 'CID' AND 'DCC' AND 'AHL' AND 'VPP')
001835        NEXT SENTENCE
001836     ELSE
001837        GO TO 12001-CHECK-CA.
001838
001839     MOVE CP-CERT-EFF-DT TO DC-BIN-DATE-1.
001840     MOVE ' ' TO DC-OPTION-CODE.
001841     PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
001842
001843     IF DATE-CONVERSION-ERROR
001844        MOVE '2'                 TO CP-RETURN-CODE
001845        MOVE ZEROS               TO FACTOR
001846                                    CP-CALC-PREMIUM
001847         GO TO 99999-RETURN.
001848
001849     IF DC-GREG-DATE-CYMD GREATER THAN 19931231
001850        NEXT SENTENCE
001851     ELSE
001852        GO TO 12001-CONTINUE-NET.
001853
001854     COMPUTE ANGLEN EQUAL WK1 / I.
001855     COMPUTE ANGLEN-M EQUAL WK5 / I.
001856
001857     COMPUTE NC-LR = CP-PREMIUM-RATE / ( ORIGINAL-TERM / 12).
001858
001859     COMPUTE NC-R = 20 *
001860                    ((NC-LR * ORIGINAL-TERM) / 12)
001861                          / (ORIGINAL-TERM + 1).
001862
001863     COMPUTE NC-OB = NC-R / 1000.
001864
001865     COMPUTE NC-P = (((ORIGINAL-TERM - ANGLEN) / I) * NC-OB) +
001866                    (NC-OB * ORIGINAL-TERM * 3).
001867
001868     IF CP-EXT-CHG-LF
001869         COMPUTE NC-BIG-D = CP-TERM-OR-EXT-DAYS + 30
001870     ELSE
001871         MOVE 30                 TO  NC-BIG-D.
001872
001873     COMPUTE NC-LITTLE-D = (1 + I) /
001874             ((( 1 + ((NC-BIG-D / 30.4166666)) * I))).
001875
001876     COMPUTE NC-MP ROUNDED = CP-ORIGINAL-BENEFIT  /
001877             (ANGLEN * NC-LITTLE-D ).
001878
001879     COMPUTE CP-CALC-PREMIUM ROUNDED = NC-MP * NC-P.
001880
001881     GO TO 99999-RETURN.
001882
001883 12001-CHECK-CA.
001884
001885     IF (CP-STATE-STD-ABBRV EQUAL 'CA') AND
001886        (WS-DISCOUNT-OPTION = '6')
001887        NEXT SENTENCE
001888     ELSE
001889        GO TO 12001-CHECK-GTL-IA.
001890
001891     COMPUTE CA-J = CA-DISCOUNT / 12.
001892     COMPUTE ANGLEN EQUAL WK1 / I.
001893     COMPUTE CA-MP = CP-ORIGINAL-BENEFIT  / ANGLEN.
001894     COMPUTE CA-API = (1 - (1 + I)** - ORIGINAL-TERM) / I.
001895     COMPUTE CA-APJ = (1 - (1 + CA-J)** - ORIGINAL-TERM) / CA-J.
001896     COMPUTE CA-VI = (1 + I)** -1.
001897     COMPUTE CP-CALC-PREMIUM =
001898         ((CP-PREMIUM-RATE * 2) / (100 * (ORIGINAL-TERM + 1)) *
001899         ((1 + CA-J) / I) *
001900         (CA-APJ + (CA-J * CA-APJ - I * CA-API) *
001901         ((CA-VI ** (LOAN-TERM - ORIGINAL-TERM)) /
001902         (I - CA-J))) * (1 + CP-ODD-DAYS-TO-PMT * I) * CA-MP).
001903
001904     GO TO 99999-RETURN.
001905
001906 12001-CHECK-GTL-IA.
001907
001908     IF (CP-COMPANY-ID EQUAL 'GTL')     AND
001909        (CP-STATE-STD-ABBRV EQUAL 'IA') AND
001910        (CP-EARN-AS-NET-PAY)
001911        NEXT SENTENCE
001912     ELSE
001913        GO TO 12001-CHECK-CID-TRN-VA.
001914
001915***************************************************************
001916
001917     COMPUTE GTL-IA-ODD-DAYS = CP-ODD-DAYS-TO-PMT - 30.
001918
001919     IF GTL-IA-ODD-DAYS = ZEROS
001920         MOVE ZERO               TO  GTL-IA-SGN
001921     ELSE
001922         IF GTL-IA-ODD-DAYS GREATER THAN ZERO
001923             MOVE +1             TO  GTL-IA-SGN
001924         ELSE
001925             MOVE -1             TO  GTL-IA-SGN.
001926
001927     MOVE GTL-IA-ODD-DAYS        TO  GTL-IA-ABS.
001928     COMPUTE GTL-IA-ABS = GTL-IA-ABS - 30.
001929
001930     COMPUTE GTL-IA-ODF = (1 + I * GTL-IA-ABS / 30) **
001931                          GTL-IA-SGN.
001932
001933     COMPUTE ANGLEN EQUAL WK1 / I.
001934     COMPUTE ANGLEN-M EQUAL WK5 / I.
001935
001936     IF GTL-IA-ODF NOT = ZERO
001937         COMPUTE ANGLEN EQUAL ANGLEN / GTL-IA-ODF
001938         COMPUTE ANGLEN-M EQUAL ANGLEN-M / GTL-IA-ODF.
001939
001940     COMPUTE GTL-IA-DIVIDEND =
001941           (((N-P-LOAN - (N-P-LOAN - N-P-ORIG)) -
001942                             (ANGLEN - ANGLEN-M))) *
001943           ((K1 + I) * CP-PREMIUM-RATE *
001944                     (N-P-LOAN - (N-P-LOAN - N-P-ORIG))).
001945
001946     COMPUTE GTL-IA-DIVISOR =
001947      I * ANGLEN * (N-P-LOAN - (N-P-LOAN - N-P-ORIG) + 1) * +600.
001948
001949     COMPUTE GTL-IA-FACTOR = GTL-IA-DIVIDEND / GTL-IA-DIVISOR.
001950
001951     COMPUTE CP-CALC-PREMIUM =
001952                           GTL-IA-FACTOR * CP-ORIGINAL-BENEFIT.
001953     GO TO 99999-RETURN.
001954
001955
001956 12001-CHECK-CID-TRN-VA.
001957
001958     IF (CP-COMPANY-ID EQUAL 'CID')     AND
001959        (CP-STATE-STD-ABBRV EQUAL 'VA') AND
001960        (CP-EARN-AS-NET-PAY) AND
001961        (CP-TRUNCATED-LIFE) AND
001962        (CP-STATE-STD-ABBRV = CP-STATE)
001963        CONTINUE
001964     ELSE
001965        GO TO 12001-CHECK-CID-TX
001966     END-IF
001967
001968     COMPUTE ANGLEN EQUAL WK1 / I
001969     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
001970     IF WS-WORK-DIS-RATE = ZEROS
001971        CONTINUE
001972     ELSE
001973        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
001974        COMPUTE WK7 = (1 / I) *
001975              (((WK4 ** M - 1) / (WK4 - 1)) -
001976              ((WK4 ** M * SX - VX) /
001977              (WK4 * (1 + I) - 1))) *
001978              (CP-PREMIUM-RATE / 1000)
001979        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN
001980        COMPUTE WK8 = WK7 * 100 / ANGLEN
001981        COMPUTE CP-CALC-PREMIUM =
001982           WK8 *  ((WK4 * ANGLEN) / 100)
001983     END-IF
001984
001985     GO TO 99999-RETURN
001986
001987     .
001988
001989 12001-CHECK-CID-TX.
001990
001991     IF (CP-COMPANY-ID EQUAL 'CID')     AND
001992        (CP-STATE-STD-ABBRV EQUAL 'TX') AND
001993        (CP-EARN-AS-NET-PAY) AND
001994        (WS-WORK-DIS-RATE > ZEROS) AND
001995        (CP-STATE-STD-ABBRV = CP-STATE)
001996        CONTINUE
001997     ELSE
001998        GO TO 12001-CHECK-CID-LEV-MN-NEW
001999     END-IF
002000* THE FOLLOWING STMT FORCES ONE MONTH EXTRA INT PERIOD
002001     COMPUTE K-I = 1 + (1 * I)
002002     COMPUTE ANGLEm EQUAL WK1 / I   *>   Loan term
002003     COMPUTE ANGLEn EQUAL WK2 / I   *>   Ins  term
002004
002005     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
002006
002007     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
002008     COMPUTE ANGLEmy = ANGLEm / Y
002009
002010     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
002011     IF WS-WORK-DIS-RATE = ZEROS
002012        CONTINUE
002013     ELSE
002014        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
002015        COMPUTE WK7 = (K-I / I) *
002016              (((WK4 ** M - 1) / (WK4 - 1)) -
002017*             ((WK4 ** M * SX - VX) /
002018              ((WK4 ** M * SX) - VX) /
002019*             (WK4 * (1 + I) - 1))) *
002020              (WK4 * (1 + I) - 1)) *
002021              (CP-PREMIUM-RATE / 1000)
002022        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEmy
002023        COMPUTE WK8 = (WK7 * 100) / (ANGLEmy * K-I)
002024        COMPUTE CP-CALC-PREMIUM =
002025           WK8 *  ((WK4 * ANGLEmy * K-I) / 100)
002026     END-IF
002027
002028     GO TO 99999-RETURN
002029
002030     .
002031 12001-CHECK-CID-LEV-MN-NEW.
002032
002033*    IF (CP-COMPANY-ID EQUAL 'CID')
002034*       AND (CP-STATE-STD-ABBRV = 'MN')
002035*       AND (CP-EARN-AS-NET-PAY)
002036*       AND (CP-LEVEL-LIFE)
002037*       AND (CP-CERT-EFF-DT > X'A4FF')
002038*       CONTINUE
002039*    ELSE
002040*       GO TO 12001-CHECK-CID-LEV-MN
002041*    END-IF
002042*
002043*    MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1
002044*    MOVE CP-EXPIRE-DT           TO DC-BIN-DATE-2
002045*    MOVE '1'                    TO DC-OPTION-CODE
002046*    PERFORM 9100-CONVERT-DATE THRU 9100-EXIT
002047*
002048*    IF DATE-CONVERSION-ERROR
002049*       MOVE '2'                 TO CP-RETURN-CODE
002050*       MOVE ZEROS               TO FACTOR
002051*                                   CP-CALC-PREMIUM
002052*       GO TO 99999-RETURN
002053*    END-IF
002054*
002055*    COMPUTE I = CP-LOAN-APR / 100
002056*    COMPUTE D = DC-ELAPSED-DAYS + CP-TERM-OR-EXT-DAYS
002057*
002058*    COMPUTE WK3 = (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM) /
002059*      (1 / (1 + (I * D / 360)) - CP-PREMIUM-RATE / 1000
002060*      * 12 / 360 * D)
002061*
002062*    COMPUTE CP-CALC-PREMIUM = WK3 * CP-PREMIUM-RATE / 1000
002063*       * 12 / 360 * D
002064*
002065*    GO TO 99999-RETURN
002066
002067      .
002068 12001-CHECK-CID-LEV-MN.
002069
002070     IF (CP-COMPANY-ID EQUAL 'CID')     AND
002071        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
002072        (CP-EARN-AS-NET-PAY) AND
002073        (CP-LEVEL-LIFE) AND
002074        (CP-STATE-STD-ABBRV = CP-STATE)
002075        CONTINUE
002076     ELSE
002077        GO TO 12001-CHECK-CID-LEV-CA
002078     END-IF
002079
002080     COMPUTE CP-CALC-PREMIUM = (CP-ORIGINAL-BENEFIT *
002081         (N / +12) * (CP-PREMIUM-RATE / +100)) /
002082         (1 - (N / +12) * (CP-PREMIUM-RATE / 100))
002083
002084     GO TO 99999-RETURN
002085
002086      .
002087 12001-CHECK-CID-LEV-CA.
002088
002089     IF (CP-COMPANY-ID EQUAL 'CID')     AND
002090        (CP-STATE-STD-ABBRV EQUAL 'CA') AND
002091        (CP-EARN-AS-NET-PAY) AND
002092        (CP-LEVEL-LIFE) AND
002093        (CP-STATE-STD-ABBRV = CP-STATE)
002094        CONTINUE
002095     ELSE
002096        GO TO 12001-CHECK-CID-TRN-CA
002097     END-IF
002098
002099     COMPUTE WK4 = WS-WORK-DIS-RATE / 12
002100     COMPUTE WK7 = (1 / (1 + WK4)) ** N
002101     COMPUTE WK8 = (1 / (1 + WK4)) ** (N - 1)
002102     COMPUTE ANGLEN-1 = (1 - WK8) / WK4
002103     COMPUTE CP-CALC-PREMIUM =
002104          (CP-ORIGINAL-BENEFIT * (1 + ANGLEN-1) *
002105          (CP-PREMIUM-RATE / 1000)) /
002106          (1 - ((1 + ANGLEN-1) * (CP-PREMIUM-RATE / 1000)))
002107
002108     GO TO 99999-RETURN
002109
002110      .
002111 12001-CHECK-CID-TRN-CA.
002112
002113     IF (CP-COMPANY-ID EQUAL 'CID')     AND
002114        (CP-STATE-STD-ABBRV EQUAL 'CA') AND
002115        (CP-EARN-AS-NET-PAY) AND
002116        (CP-TRUNCATED-LIFE) AND
002117        (CP-STATE-STD-ABBRV = CP-STATE) AND
002118        (WS-DISCOUNT-OPTION NOT = ' ')
002119        CONTINUE
002120     ELSE
002121        GO TO 12001-CHECK-CID-TRN-MT
002122     END-IF
002123
002124     COMPUTE ANGLEN EQUAL WK1 / I
002125     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
002126     IF WS-WORK-DIS-RATE = ZEROS
002127        CONTINUE
002128     ELSE
002129        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
002130        COMPUTE WK7 = (1 / I) *
002131              (((WK4 ** M - 1) / (WK4 - 1)) -
002132              ((WK4 ** M * SX - VX) /
002133              (WK4 * (1 + I) - 1))) *
002134              (CP-PREMIUM-RATE / 1000)
002135        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN
002136        COMPUTE CP-CALC-PREMIUM =
002137           WK4 *  WK7
002138     END-IF
002139
002140     GO TO 99999-RETURN
002141
002142      .
002143 12001-CHECK-CID-TRN-MT.
002144
002145     IF (CP-COMPANY-ID EQUAL 'CID') AND
002146        ((CP-STATE-STD-ABBRV EQUAL 'MT' OR 'VT')
002147                   OR
002148        ((CP-STATE-STD-ABBRV = 'IN') AND
002149        (CP-CERT-EFF-DT > X'9A7F'))
002150                   OR
002151        ((CP-STATE-STD-ABBRV = 'AZ' OR 'ND') AND
002152        (CP-CERT-EFF-DT > X'9ADF'))) AND
002153        (CP-EARN-AS-NET-PAY) AND
002154        (CP-TRUNCATED-LIFE) AND
002155        (CP-STATE-STD-ABBRV = CP-STATE)
002156        CONTINUE
002157     ELSE
002158        GO TO 12001-CHECK-CID-MT
002159     END-IF
002160
002161* K-I WILL BE USED FOR EXTRA INT. PERIODS
002162     IF CP-STATE-STD-ABBRV = 'IN' OR 'ND'
002163        MOVE 0                   TO K-I
002164     ELSE
002165        MOVE 2                   TO K-I
002166     END-IF
002167     COMPUTE CP-TERM-OR-EXT-DAYS =
002168           CP-TERM-OR-EXT-DAYS + 30
002169
002170     COMPUTE ANGLEN EQUAL WK1 / I
002171     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
002172     COMPUTE WK8 = (1 + (CP-TERM-OR-EXT-DAYS * I) / +30)
002173                   / (1 + I)
002174     COMPUTE ANGLEN-1 = ANGLEN / WK8
002175     IF WS-WORK-DIS-RATE = ZEROS
002176        CONTINUE
002177     ELSE
002178        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
002179*       COMPUTE WK7 = ((1 + (2 * I)) / I) *
002180        COMPUTE WK7 = ((1 + (K-I * I)) / I) *
002181              (((WK4 ** M - 1) / (WK4 - 1)) -
002182              ((WK4 ** M * SX - VX) /
002183              (WK4 * (1 + I) - 1))) *
002184              (CP-PREMIUM-RATE / 1000)
002185        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
002186*       COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + 2 * I))
002187        COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + K-I * I))
002188        COMPUTE CP-CALC-PREMIUM =
002189           WK8 *  ((WK4 * ANGLEN * (1 + K-I * I)) / 100)
002190*          WK8 *  ((WK4 * ANGLEN * (1 + 2 * I)) / 100)
002191     END-IF
002192
002193     GO TO 99999-RETURN
002194
002195     .
002196 12001-CHECK-CID-MT.
002197
002198     IF (CP-COMPANY-ID EQUAL 'CID')     AND
002199        ((CP-STATE-STD-ABBRV EQUAL 'MT' OR 'NJ'
002200                                OR 'VT')
002201                   OR
002202        ((CP-STATE-STD-ABBRV = 'IN') AND
002203        (CP-CERT-EFF-DT > X'9A7F'))
002204                   OR
002205        ((CP-STATE-STD-ABBRV = 'AZ' OR 'ND') AND
002206        (CP-CERT-EFF-DT > X'9ADF'))) AND
002207        (CP-EARN-AS-NET-PAY) AND
002208        (CP-STATE-STD-ABBRV = CP-STATE)
002209        CONTINUE
002210     ELSE
002211        GO TO 12001-CHECK-DCC-DD
002212     END-IF
002213
002214     COMPUTE CP-TERM-OR-EXT-DAYS =
002215           CP-TERM-OR-EXT-DAYS + 30
002216
002217* K-I WILL BE USED FOR EXTRA INT. PERIODS
002218     IF CP-STATE-STD-ABBRV = 'IN' OR 'ND'
002219        MOVE 0                   TO K-I
002220     ELSE
002221        IF CP-STATE-STD-ABBRV = 'NJ'
002222           MOVE 1                TO K-I
002223        ELSE
002224           MOVE 2                TO K-I
002225        END-IF
002226     END-IF
002227     COMPUTE ANGLEN EQUAL WK1 / I
002228     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
002229     COMPUTE WK8 = (1 + (CP-TERM-OR-EXT-DAYS * I) / +30)
002230                   / (1 + I)
002231     COMPUTE ANGLEN-1 = ANGLEN / WK8
002232     IF WS-WORK-DIS-RATE = ZEROS
002233        CONTINUE
002234     ELSE
002235        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
002236*       COMPUTE WK7 = ((1 + (2 * I)) / I) *
002237        COMPUTE WK7 = ((1 + (K-I * I)) / I) *
002238              (((WK4 ** N - 1) / (WK4 - 1)) -
002239              ((WK4 ** N - SV) /
002240              (WK4 * (1 + I) - 1))) *
002241              (CP-PREMIUM-RATE / 1000)
002242        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
002243*       COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + 2 * I))
002244        COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + K-I * I))
002245        COMPUTE CP-CALC-PREMIUM =
002246           WK8 *  ((WK4 * ANGLEN * (1 + K-I * I)) / 100)
002247*          WK8 *  ((WK4 * ANGLEN * (1 + 2 * I)) / 100)
002248     END-IF
002249
002250     GO TO 99999-RETURN
002251
002252     .
002253
002254 12001-CHECK-DCC-DD.
002255
002256     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002257        AND (CP-EARN-AS-NET-PAY)
002258        AND (CP-BEN-CATEGORY = 'D')
002259        CONTINUE
002260     ELSE
002261        GO TO 12001-CHECK-DCC-NET
002262     END-IF
002263
002264     COMPUTE CP-TERM-OR-EXT-DAYS =
002265           CP-TERM-OR-EXT-DAYS + 30
002266
002267     COMPUTE ANGLEN EQUAL WK1 / I.
002268     COMPUTE ANGLEM EQUAL WK2 / I.
002269     COMPUTE ANGLEN-M EQUAL WK5 / I.
002270
002271     COMPUTE ODF = (1+ (CP-TERM-OR-EXT-DAYS * I / 30))
002272           / (1 + I)
002273
002274     COMPUTE GR     = ANGLEN / ODF
002275*    COMPUTE CP-PREMIUM-RATE = CP-PREMIUM-RATE *
002276*      CP-RATE-DEV-PCT
002277
002278     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / GR
002279
002280     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
002281       (CP-PREMIUM-RATE / 100) * (1 + (0 * (N-P-APR / 12)))
002282     MOVE ZEROS                  TO CP-RATE-DEV-PCT
002283     MOVE WK3                    TO CP-CALC-PREMIUM
002284
002285     GO TO 99999-RETURN
002286
002287     .
002288
002289 12001-CHECK-DCC-NET.
002290
002291     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002292        AND (CP-EARN-AS-NET-PAY)
002293        AND (CP-BENEFIT-CD (1:1) NOT = 'M' AND 'N'
002294             AND 'H' AND 'S' AND 'P')
002295        CONTINUE
002296     ELSE
002297        GO TO 12001-CHECK-DCC-PNET
002298     END-IF
002299
002300     COMPUTE CP-TERM-OR-EXT-DAYS =
002301           CP-TERM-OR-EXT-DAYS + 30
002302
002303     COMPUTE ANGLEN EQUAL WK1 / I.
002304     COMPUTE ANGLEM EQUAL WK2 / I.
002305     COMPUTE ANGLEN-M EQUAL WK5 / I.
002306
002307     COMPUTE ODF = (1+ (CP-TERM-OR-EXT-DAYS * I / 30))
002308           / (1 + I)
002309
002310     COMPUTE GR     = ANGLEN / ODF
002311     COMPUTE CP-PREMIUM-RATE = CP-PREMIUM-RATE / M * 12
002312     COMPUTE CP-CALC-PREMIUM ROUNDED = CP-PREMIUM-RATE *
002313       CP-RATE-DEV-PCT
002314     MOVE CP-CALC-PREMIUM        TO CP-PREMIUM-RATE
002315*    COMPUTE WK8 = CP-PREMIUM-RATE * M / 12
002316
002317     COMPUTE WK8 = CP-PREMIUM-RATE *
002318        ((M + ((CP-TERM-OR-EXT-DAYS - 30) / 30)) / 12)
002319     COMPUTE WK8 = WK8 / ((M + (12 / 12)) / 2)
002320     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / GR
002321     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
002322       (WK8 / 100) * (1 + (0 * (N-P-APR / 12)))
002323     MOVE ZEROS                  TO CP-RATE-DEV-PCT
002324     MOVE WK3                    TO CP-CALC-PREMIUM
002325
002326     GO TO 99999-RETURN
002327
002328     .
002329 12001-CHECK-DCC-PNET.
002330
002331     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002332        AND (CP-EARN-AS-NET-PAY)
002333        AND (CP-BENEFIT-CD (1:1) = 'P')
002334        CONTINUE
002335     ELSE
002336        GO TO 12001-CHECK-DCC-MNET
002337     END-IF
002338
002339     IF CP-NO-OF-PMTS NOT NUMERIC
002340        MOVE +0                  TO CP-NO-OF-PMTS
002341     END-IF
002342     IF CP-PMT-MODE NOT = 'W' AND 'S' AND 'B' AND 'T'
002343        AND ' ' AND 'M'
002344        MOVE ' '                 TO CP-PMT-MODE
002345     END-IF
002346
002347******************************************************************
002348***   Default is monthly, use loan term for rate and amort     ***
002349******************************************************************
002350     move cp-loan-term         to c-tot-pmts
002351     MOVE +30                  TO C-DAYS-IN-PMT-PER
002352     MOVE +12                  TO C-PMTS-PER-YEAR
002353******************************************************************
002354
002355     evaluate true
002356        when cp-pmt-mode = 'W'
002357           MOVE +52              TO C-PMTS-PER-YEAR
002358           compute c-tot-pmts = cp-loan-term /
002359              12 * c-pmts-per-year
002360           MOVE +7               TO C-DAYS-IN-PMT-PER
002361        when cp-pmt-mode = 'B'
002362           MOVE +26              TO C-PMTS-PER-YEAR
002363           compute c-tot-pmts = cp-loan-term /
002364              12 * c-pmts-per-year
002365           MOVE +14              TO C-DAYS-IN-PMT-PER
002366     end-evaluate
002367
002368     COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
002369
002370     COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * I /
002371        C-DAYS-IN-PMT-PER)) / (1 + I)
002372
002373     compute anglen = (1 - (1 / (1 + i)) ** c-tot-pmts) / i
002374*    COMPUTE ANGLEN = (1 - ((1 + I) ** (C-TOT-PMTS * -1))) / I
002375     COMPUTE ANGLEN-1 = ANGLEN / Y
002376
002377     COMPUTE WK3 = CP-ORIGINAL-BENEFIT / ANGLEN-1
002378
002379     IF CP-RATE-DEV-PCT NOT = ZEROS
002380        COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
002381           CP-PREMIUM-RATE
002382     END-IF
002383
002384     COMPUTE CP-CALC-PREMIUM = (WK3 / 100) * CP-PREMIUM-RATE
002385
002386     GO TO 99999-RETURN
002387
002388     .
002389 12001-CHECK-DCC-MNET.
002390
002391     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002392        AND (CP-EARN-AS-NET-PAY)
002393        AND (CP-BENEFIT-CD (1:1) = 'M' OR 'H')
002394        CONTINUE
002395     ELSE
002396        GO TO 12001-CHECK-DCC-MNET-LF-BAL
002397     END-IF
002398
002399*    COMPUTE ANGLEN EQUAL WK1 / I.
002400*    COMPUTE ANGLEM EQUAL WK2 / I.
002401*    COMPUTE ANGLEN-M EQUAL WK5 / I.
002402
002403     COMPUTE WK3 = CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE / 1000
002404     MOVE WK3                    TO CP-CALC-PREMIUM
002405
002406     GO TO 99999-RETURN
002407
002408     .
002409 12001-CHECK-DCC-MNET-LF-BAL.
002410
002411     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002412        AND (CP-EARN-AS-NET-PAY)
002413        AND (CP-BENEFIT-CD (1:1) = 'N')
002414        AND (CP-BEN-CATEGORY NOT = 'N')
002415        AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
002416        CONTINUE
002417     ELSE
002418        GO TO 12001-CHECK-DCC-MNET-LFSUM-BAL
002419     END-IF
002420
002421     IF CP-RATE-DEV-PCT NOT = ZEROS
002422        COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
002423           CP-PREMIUM-RATE
002424     END-IF
002425
002426*    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
002427     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
002428        / (1 + I)
002429     COMPUTE ANGLEN-M = (WK5 / I) / OD
002430     COMPUTE J = I + (CP-PREMIUM-RATE / +1000)
002431     COMPUTE WK2 = (1 + J) ** (M * -1)
002432     COMPUTE ANGLEN-M = ANGLEN-M * WK2
002433
002434     COMPUTE ANGLEM-1 = ((1 - ((1 + J)
002435          ** ((M - 1) * -1))) / J)
002436
002437     COMPUTE ANGLEM = ((1 - ((1 + J)
002438          ** (M * -1))) / J) / OD
002439
002440     COMPUTE RA = (CP-ORIGINAL-BENEFIT * OD -
002441        (CP-ALTERNATE-BENEFIT * (1 + J) ** (M * -1)))
002442        / ANGLEM-1
002443
002444     COMPUTE ANGLEN-M = ((1 - ((1 + I)
002445          ** ((N - M) * -1))) / I)
002446
002447     MOVE +0                     TO WK3
002448     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
002449        (WS-COUNTER > M)
002450        COMPUTE ANGLEM = (1 - ((1 + J)
002451          ** ((M - WS-COUNTER) * -1))) / J
002452        COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
002453        COMPUTE WK5-CSL = (RA * ANGLEM)
002454        + (CP-ALTERNATE-BENEFIT * WK2)
002455        COMPUTE WK3 = WK3 + (WK5-CSL * CP-PREMIUM-RATE / +1000)
002456     END-PERFORM
002457
002458     MOVE WK3                    TO CP-CALC-PREMIUM
002459
002460     GO TO 99999-RETURN
002461
002462     .
002463 12001-CHECK-DCC-MNET-LFSUM-BAL.
002464
002465     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002466        AND (CP-EARN-AS-NET-PAY)
002467        AND (CP-BENEFIT-CD (1:1) = 'S')
002468        AND (CP-BEN-CATEGORY NOT = 'N')
002469        AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
002470        CONTINUE
002471     ELSE
002472        GO TO 12001-CHECK-DCC-MNET-LF
002473     END-IF
002474
002475     IF CP-RATE-DEV-PCT NOT = ZEROS
002476        COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
002477           CP-PREMIUM-RATE
002478     END-IF
002479
002480     COMPUTE K = I + (CP-PREMIUM-RATE / +1000)
002481
002482     COMPUTE ANGLEM-1 = ((1 - ((1 + K)
002483          ** ((M - 1) * -1))) / K)
002484
002485     COMPUTE ANGLEM = ((1 - ((1 + K)
002486          ** (M * -1))) / K)
002487
002488     MOVE CP-MONTHLY-PAYMENT     TO RA
002489
002490     COMPUTE WK3 = (((((M - 1) - ANGLEM-1) / K) - ((((M - 1)
002491        * M / 2 - (((M - 1) - ANGLEM-1) / K)) / K) * 0 / 1000))
002492        * RA + (CP-ALTERNATE-BENEFIT * ANGLEM)) *
002493        CP-PREMIUM-RATE / 1000
002494
002495     MOVE WK3                    TO CP-CALC-PREMIUM
002496
002497
002498     GO TO 99999-RETURN
002499
002500     .
002501 12001-CHECK-DCC-MNET-LF.
002502
002503     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002504        AND (CP-EARN-AS-NET-PAY)
002505        AND (CP-BENEFIT-CD (1:1) = 'N')
002506        AND (CP-BEN-CATEGORY NOT = 'N')
002507        CONTINUE
002508     ELSE
002509        GO TO 12001-CHECK-DCC-MNET-SUMLF
002510     END-IF
002511
002512     IF CP-RATE-DEV-PCT NOT = ZEROS
002513        COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
002514           CP-PREMIUM-RATE
002515     END-IF
002516
002517*    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
002518     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
002519        / (1 + I)
002520     COMPUTE ANGLEN-M = (WK5 / I) / OD
002521     COMPUTE J = I + (CP-PREMIUM-RATE / +1000)
002522     COMPUTE WK2 = (1 + J) ** (M * -1)
002523     COMPUTE ANGLEN-M = ANGLEN-M * WK2
002524
002525
002526     COMPUTE ANGLEM = ((1 - ((1 + J)
002527          ** (M * -1))) / J) / OD
002528     COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
002529
002530     COMPUTE ANGLEN-M = ((1 - ((1 + I)
002531          ** ((N - M) * -1))) / I)
002532
002533     MOVE +0                     TO WK3
002534     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
002535        (WS-COUNTER > M)
002536        COMPUTE ANGLEM = (1 - ((1 + J)
002537          ** ((M - WS-COUNTER + 1) * -1))) / J
002538        COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
002539        COMPUTE WK5-CSL = RA * (ANGLEM + (ANGLEN-M * WK2))
002540        COMPUTE WK3 = WK3 + (WK5-CSL * CP-PREMIUM-RATE / +1000)
002541     END-PERFORM
002542
002543     MOVE WK3                    TO CP-CALC-PREMIUM
002544
002545     GO TO 99999-RETURN
002546
002547     .
002548 12001-CHECK-DCC-MNET-SUMLF.
002549
002550     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002551        AND (CP-EARN-AS-NET-PAY)
002552        AND (CP-BENEFIT-CD (1:1) = 'S')
002553        AND (CP-BEN-CATEGORY NOT = 'N')
002554        CONTINUE
002555     ELSE
002556        GO TO 12001-CHECK-DCC-MNET-TOT-BAL-N
002557     END-IF
002558
002559     IF CP-RATE-DEV-PCT NOT = ZEROS
002560        COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
002561           CP-PREMIUM-RATE
002562     END-IF
002563
002564     COMPUTE K = I + (CP-PREMIUM-RATE / +1000)
002565     COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * K / 30))
002566        / (1 + K)
002567
002568     COMPUTE ANGLEN-M = (WK5 / I)
002569     COMPUTE WK2 = (1 + K) ** (M * -1)
002570     COMPUTE ANGLEN-M = ANGLEN-M * WK2
002571     COMPUTE ANGLEN-M = ANGLEN-M / Y
002572
002573
002574     COMPUTE ANGLEM = ((1 - ((1 + K)
002575          ** (M * -1))) / K) / Y
002576     COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
002577
002578     COMPUTE WK5-CSL = (((M - ANGLEM) / K) + (ANGLEN-M *
002579        ((1 + K) ** (M + 1 / Y * (CP-DAYS-TO-1ST-PMT - +30) / 30)
002580        - 1)) / K) * RA
002581     COMPUTE WK7 = (((((M * (M + 1) / 2)
002582        + (CP-DAYS-TO-1ST-PMT - +30)
002583        / 30 * M - (M - ANGLEM) / K) / K)) * RA *
002584        +0 / 1000 * 12 / 12)
002585     COMPUTE WK3 = (WK5-CSL - WK7) * (CP-PREMIUM-RATE / 1000)
002586        * (12 / 12)
002587
002588
002589     MOVE WK3                    TO CP-CALC-PREMIUM
002590
002591     GO TO 99999-RETURN
002592
002593     .
002594 12001-CHECK-DCC-MNET-TOT-BAL-N.
002595
002596     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002597        AND (CP-EARN-AS-NET-PAY)
002598        AND (CP-BENEFIT-CD (1:1) = 'N')
002599        AND (CP-BEN-CATEGORY = 'N')
002600        AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
002601        CONTINUE
002602     ELSE
002603        GO TO 12001-CHECK-DCC-MNET-TOT-BAL-S
002604     END-IF
002605
002606*    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
002607     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
002608        / (1 + I)
002609     COMPUTE ANGLEN-M = (WK5 / I) / OD
002610     COMPUTE J = I + ((CP-DCC-LF-RATE + CP-DCC-AH-RATE) / +1000)
002611     COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
002612     COMPUTE WK2 = (1 + J) ** (M * -1)
002613     COMPUTE ANGLEN-M = ANGLEN-M * WK2
002614
002615     COMPUTE ANGLEM-1 = ((1 - ((1 + J)
002616          ** ((M - 1) * -1))) / J)
002617
002618     COMPUTE ANGLEM = ((1 - ((1 + J)
002619          ** (M * -1))) / J) / OD
002620
002621     COMPUTE RA = (CP-ORIGINAL-BENEFIT * OD -
002622        (CP-ALTERNATE-BENEFIT * (1 + K) ** (M * -1)))
002623        / ANGLEM-1
002624
002625     COMPUTE ANGLEN-M = ((1 - ((1 + I)
002626          ** ((N - M) * -1))) / I)
002627
002628     MOVE +0                     TO WK3
002629     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
002630        (WS-COUNTER > M)
002631        COMPUTE ANGLEM = (1 - ((1 + J)
002632          ** ((M - WS-COUNTER) * -1))) / J
002633        COMPUTE WK2 = (1 / (1 + K)) ** (M - WS-COUNTER + 1)
002634        COMPUTE WK5-CSL = (RA * ANGLEM)
002635        + (CP-ALTERNATE-BENEFIT * WK2)
002636        COMPUTE WK3 = WK3 +
002637         (WK5-CSL * (CP-DCC-LF-RATE / +1000))
002638        COMPUTE WK3 = WK3 + (WK5-CSL - (CP-ALTERNATE-BENEFIT *
002639          (1 / (1 + K) ** (M - WS-COUNTER + 1)))) *
002640          (CP-DCC-AH-RATE / 1000)
002641     END-PERFORM
002642
002643     MOVE WK3                    TO CP-CALC-PREMIUM
002644
002645     GO TO 99999-RETURN
002646
002647     .
002648 12001-CHECK-DCC-MNET-TOT-BAL-S.
002649
002650     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002651        AND (CP-EARN-AS-NET-PAY)
002652        AND (CP-BENEFIT-CD (1:1) = 'S')
002653        AND (CP-BEN-CATEGORY = 'N')
002654        AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
002655        CONTINUE
002656     ELSE
002657        GO TO 12001-CHECK-DCC-MNET-TOT-N
002658     END-IF
002659
002660
002661     COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
002662
002663     COMPUTE ANGLEM-1 = ((1 - ((1 + K)
002664          ** ((M - 1) * -1))) / K)
002665
002666     COMPUTE ANGLEM = ((1 - ((1 + K)
002667          ** (M * -1))) / K)
002668
002669     MOVE CP-MONTHLY-PAYMENT     TO RA
002670
002671     COMPUTE WK3 = (((((M - 1) - ANGLEM-1) / K) - ((((M - 1)
002672        * M / 2 - (((M - 1) - ANGLEM-1) / K)) / K)
002673        * CP-DCC-AH-RATE / 1000))
002674        * RA + (CP-ALTERNATE-BENEFIT * ANGLEM)) *
002675        CP-DCC-LF-RATE / 1000
002676
002677     MOVE WK3                    TO CP-LF-PREM
002678
002679     COMPUTE WK5-CSL = RA * ((M - 1) * M / 2
002680        + (M - 1) * (CP-DAYS-TO-1ST-PMT - 30) / 30)
002681        * CP-DCC-AH-RATE / 1000
002682
002683     COMPUTE WK3 = WK3 + WK5-CSL
002684
002685
002686     MOVE WK3                    TO CP-CALC-PREMIUM
002687
002688     GO TO 99999-RETURN
002689
002690      .
002691 12001-CHECK-DCC-MNET-TOT-N.
002692
002693     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002694        AND (CP-EARN-AS-NET-PAY)
002695        AND (CP-BENEFIT-CD (1:1) = 'N')
002696        AND (CP-BEN-CATEGORY = 'N')
002697        CONTINUE
002698     ELSE
002699        GO TO 12001-CHECK-DCC-MNET-TOT-S
002700     END-IF
002701
002702*    I = APR/1200
002703*    J = APR/1200 THEN ADJUSTED FOR THE LIFE AND DIS RATE
002704*    RA = PAYMENT AMOUNT
002705*    WK5-CSL = THE BALANCE (PAY OFF )
002706*    WK3 = THE ACCUMULATED PREMIUM
002707*    WK2 = (1 + I) ^ (m * -1)
002708*    OD  = GAMMA
002709*
002710*    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
002711     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
002712        / (1 + I)
002713     COMPUTE ANGLEN-M = (WK5 / I) / OD
002714     COMPUTE J = I + (CP-DCC-LF-RATE / +1000) +
002715        (CP-DCC-AH-RATE / +1000)
002716     COMPUTE WK2 = (1 + J) ** (M * -1)
002717     COMPUTE ANGLEN-M = ANGLEN-M * WK2
002718
002719
002720     COMPUTE ANGLEM = ((1 - ((1 + J)
002721          ** (M * -1))) / J) / OD
002722     COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
002723
002724     COMPUTE ANGLEN-M = ((1 - ((1 + I)
002725          ** ((N - M) * -1))) / I)
002726
002727     MOVE +0                     TO WK3
002728     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
002729        (WS-COUNTER > M)
002730        COMPUTE ANGLEM = (1 - ((1 + J)
002731          ** ((M - WS-COUNTER + 1) * -1))) / J
002732        COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
002733        COMPUTE WK5-CSL = RA * (ANGLEM + (ANGLEN-M * WK2))
002734        COMPUTE WK3 = WK3 + (WK5-CSL * CP-DCC-LF-RATE / +1000)
002735           + (WK5-CSL * CP-DCC-AH-RATE / +1000)
002736     END-PERFORM
002737
002738     MOVE WK3                    TO CP-CALC-PREMIUM
002739
002740     GO TO 99999-RETURN
002741
002742     .
002743 12001-CHECK-DCC-MNET-TOT-S.
002744
002745     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
002746        AND (CP-EARN-AS-NET-PAY)
002747        AND (CP-BENEFIT-CD (1:1) = 'S')
002748        AND (CP-BEN-CATEGORY = 'N')
002749        CONTINUE
002750     ELSE
002751        GO TO 12001-CONTINUE-NET
002752     END-IF
002753
002754*    I = APR/1200
002755*    J = APR/1200 THEN ADJUSTED FOR THE LIFE RATE (K)
002756*    RA = PAYMENT AMOUNT
002757*    WK5-CSL = THE BALANCE (PAY OFF )
002758*    WK3 = THE ACCUMULATED PREMIUM
002759*    WK2 = (1 + I) ^ (m * -1)
002760*    OD  = GAMMA
002761*
002762     COMPUTE J = I + (CP-DCC-LF-RATE / +1000)
002763
002764     COMPUTE OD = ((1 + (CP-DAYS-TO-1ST-PMT * J / 30)))
002765        / (1 + J)
002766
002767     COMPUTE ANGLEM = ((1 - ((1 + J)
002768          ** (M * -1))) / J) / OD
002769
002770     COMPUTE ANGLEN-M = ((1 - ((1 + I)
002771          ** ((N - M) * -1))) / I)
002772          * (1 + J) ** (-1 * M)
002773
002774     COMPUTE ANGLEN-M = ANGLEN-M / OD
002775
002776     MOVE CP-MONTHLY-PAYMENT     TO RA
002777
002778     COMPUTE WK1 = (M + 1 / OD * (CP-DAYS-TO-1ST-PMT - +30)
002779        / +30)
002780     COMPUTE WK4 = (((M - ANGLEM) / J) + (ANGLEN-M * ((1 + J)
002781        ** (WK1) - 1)) / J) * RA
002782
002783     COMPUTE WK5 = (((((M * (M + 1) / 2)
002784        + (CP-DAYS-TO-1ST-PMT - +30)
002785        / +30 * M - (M - ANGLEM) / J) / J)) * RA *
002786        CP-DCC-AH-RATE / +1000 * 12 / 12)
002787
002788     COMPUTE WK3 = (WK4 - WK5) * (CP-DCC-LF-RATE / +1000) *
002789        (12 / 12)
002790
002791     COMPUTE WK5-CSL = RA * (M * (M + 1) / 2 + M *
002792        (CP-DAYS-TO-1ST-PMT - +30) / +30) *
002793        (CP-DCC-AH-RATE / +1000) * 12 / 12
002794
002795     COMPUTE WK3 = WK3 + WK5-CSL
002796     MOVE WK3                    TO CP-CALC-PREMIUM
002797
002798     COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
002799     COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * K / 30))
002800        / (1 + K)
002801
002802     COMPUTE ANGLEN-M = ((1 - ((1 + I)
002803          ** ((N - M) * -1))) / I)
002804     COMPUTE WK2 = (1 + K) ** (M * -1)
002805     COMPUTE ANGLEN-M = ANGLEN-M * WK2
002806     COMPUTE ANGLEN-M = ANGLEN-M / Y
002807
002808
002809     COMPUTE ANGLEM = ((1 - ((1 + K)
002810          ** (M * -1))) / K) / Y
002811
002812*    COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
002813
002814     COMPUTE WK6 = (((M - ANGLEM) / K) + (ANGLEN-M *
002815        ((1 + K) ** (M + 1 / Y
002816        * (CP-DAYS-TO-1ST-PMT - +30) / 30)
002817        - 1)) / K) * RA
002818     COMPUTE WK7 = (((((M * (M + 1) / 2)
002819        + (CP-DAYS-TO-1ST-PMT - +30)
002820        / 30 * M - (M - ANGLEM) / K) / K)) * RA *
002821        CP-DCC-AH-RATE / 1000 * 12 / 12)
002822     COMPUTE WK3 = (WK6 - WK7) * (CP-DCC-LF-RATE / 1000)
002823        * (12 / 12)
002824
002825
002826     MOVE WK3                    TO CP-LF-PREM
002827
002828
002829
002830     GO TO 99999-RETURN
002831
002832     .
002833 12001-CONTINUE-NET.
002834
002835     IF NET-STD
002836        IF CP-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'CID' OR 'MON'
002837*          OR 'DCC'
002838         COMPUTE WK3 ROUNDED = ((I * M) + VX - SX) * 2 * N
002839         COMPUTE WK3 ROUNDED = WK3 / ((1 - VX) * M * I)
002840         COMPUTE WK3 ROUNDED = WK3 / ((2 * N) - M + 1)
002841         COMPUTE WK3 ROUNDED = (WK3 * RA * K-I)
002842                               * ((1 + N) / (1 + M))
002843         if (i = .0000000001)
002844            and (wk3 = zeros)
002845            move 1               to wk3
002846         end-if
002847        ELSE
002848         COMPUTE WK3 ROUNDED = ((I * M) + VX - SX) * 2 * N
002849         COMPUTE WK3 ROUNDED = WK3 / ((1 - VX) * M * I)
002850         COMPUTE WK3 ROUNDED = WK3 / ((2 * N) - M + 1)
002851         COMPUTE WK3 ROUNDED = WK3 * RA * K-I.
002852
002853     IF NET-SMP
002854         COMPUTE N2 = N * N
002855         COMPUTE N3 = N2 * N
002856         COMPUTE WK3 ROUNDED = 2 * N2 * WK1
002857         COMPUTE WK3 ROUNDED = WK3 + (N3 * I) - (N2 * I)
002858         COMPUTE WK3 ROUNDED = WK3 + (4 * N * WK1)
002859         COMPUTE WK3 ROUNDED = WK3 * (1 + I) * 10
002860         COMPUTE WK3 ROUNDED = WK3 / (36 * (N + 1) * WK1).
002861
002862     IF NPO-PRUDENTIAL
002863       PERFORM 12100-PRU-CALC THRU 12199-EXIT
002864           VARYING WS-COUNTER FROM 1 BY 1
002865               UNTIL WS-COUNTER = N
002866       COMPUTE WK3 ROUNDED = (N * (I * PWK / (PWK - 1))) - 1
002867       COMPUTE WK3 ROUNDED = WK3 / (I * 12).
002868
002869     MOVE WK3 TO FACTOR.
002870
002871
002872     GO TO 99999-RETURN.
002873
002874 12100-PRU-CALC.
002875     IF WS-COUNTER = 1
002876         COMPUTE PWK = (I + 1)
002877     ELSE
002878         COMPUTE PWK = PWK * (I + 1).
002879
002880 12199-EXIT.
002881      EXIT.
002882
002883 99000-ERROR.
002884     MOVE +9999.999999999 TO N-P-FACTOR.
002885     MOVE '4'             TO CP-RETURN-CODE.
002886
002887     GO TO 99999-EXIT.
002888
002889 99999-RETURN.
002890     MOVE FACTOR          TO N-P-FACTOR.
002891
002892 99999-EXIT.
002893      EXIT.
      *<<((file: ERCNETP))

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRFND' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRFND' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
