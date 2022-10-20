      *((program: NSAASBL.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. NSAASBL.
000004
000005*AUTHOR.     PABLO
000006*            COLLEYVILLE, TEXAS.
000007
000008*REMARKS.    EXECUTED FROM NSAASLTR.cl2
000009
000010******************************************************************
000011*                   C H A N G E   L O G
000012*
000013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000014*-----------------------------------------------------------------
000015*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000016* EFFECTIVE    NUMBER
000017*-----------------------------------------------------------------
000018* 071111    2011022800001  PEMA  NEW PROGRAM
000019* 102212    2012101700002  AJRA  EXPAND PASSED AREA
000020* 012413    2013012100001  AJRA  ADD AUTO BILLING NOTE
000021* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000022* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
000023* 061421  CR2017031500001  PEMA  Update to CCM8
000024******************************************************************
000025 ENVIRONMENT DIVISION.
000026
000027 DATA DIVISION.
000028
000029 working-storage section.
       01  DFH-START PIC X(04).
000030
000031 77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000032 77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000033 77  S1                          PIC S999 COMP-3 VALUE +0.
000034 77  WS-ARCHIVE-NO               PIC S9(8)  COMP VALUE +0.
000035 77  WS-FOLLOW-UP-DT             PIC XX  VALUE LOW-VALUES.
000036 77  WS-RESEND-DT                PIC XX  VALUE LOW-VALUES.
000037 77  NOTE-SUB                    PIC S9(5) COMP-3 VALUE +0.
000038 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
000039     88  NO-CERT-RW                 VALUE 'N'.
000040     88  CERT-RW                    VALUE 'Y'.
000041
000042 01 response-code         pic s9(8) comp.
000043 01 display-response      pic 9(8).
000044 01 bl-index              pic 9(8) comp.
000045 01 max-last-name         pic x(18).
000046 01 first-initial         pic x.
000047 01 name-in-range-flag    pic 9.
000048 01 max-entries           pic s9(8) comp value 100.
000049
000050 01 lower-case    pic x(26) value
000051            "abcdefghijklmnopqrstuvwxyz".
000052 01 upper-case    pic x(26) value
000053            "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
000054 01  MISC.
000055     12  WS-RESPONSE             PIC S9(8)   COMP.
000056         88  RESP-NORMAL                  VALUE +00.
000057         88  RESP-NOTFND                  VALUE +13.
000058         88  RESP-DUPREC                  VALUE +14.
000059         88  RESP-DUPKEY                  VALUE +15.
000060         88  RESP-NOTOPEN                 VALUE +19.
000061         88  RESP-ENDFILE                 VALUE +20.
000062
000063*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
000064*                         COPY ELCZREC.
      *>>((file: ELCZREC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCZREC.                            *
000005*                                                                *
000006*   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
000007*                                                                *
000008******************************************************************
000009*-----------------------------------------------------------------
000010*                   C H A N G E   L O G
000011*
000012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000013*-----------------------------------------------------------------
000014*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000015* EFFECTIVE    NUMBER
000016*-----------------------------------------------------------------
000017* 122011    2011022800001  AJRA  NEW FILE
000018* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
000019* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
000020* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
000021* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
000022*-----------------------------------------------------------------
000023 01  W-Z-CONTROL-DATA.
000024     05  W-NUMBER-OF-COPIES      PIC  9.
000025     05  FILLER                  PIC  X.
000026     05  W-DAYS-TO-FOLLOW-UP     PIC  999.
000027     05  FILLER                  PIC  X.
000028     05  W-DAYS-TO-RESEND        PIC  999.
000029     05  FILLER                  PIC  X.
000030     05  W-FORM-TO-RESEND        PIC  X(4).
000031     05  W-ADD-BAR-CODE          PIC  X.
000032     05  W-PROMPT-LETTER         PIC  X.
000033     05  W-HAS-RETURN-ENV        PIC  X.
000034     05  W-ENCLOSURE-CD          PIC  XXX.
000035     05  W-SIG-FLAG-DEFAULT      PIC  X.
000036     05  W-AUTO-CLOSE-IND        PIC  X.
000037     05  FILLER                  PIC  X.
000038     05  W-LETTER-TO-BENE        PIC  X.
000039     05  FILLER                  PIC  X.
000040     05  W-LETTER-TO-ACCT        PIC  X.
000041     05  FILLER                  PIC  X.
000042     05  W-LETTER-TYPE           PIC  X.
000043     05  FILLER                  PIC  X.
000044     05  W-PRINT-CERTIFICATE     PIC  X.
000045     05  FILLER                  PIC  X.
000046     05  W-REFUND-REQUIRED       PIC  X.
000047     05  FILLER                  PIC  X.
000048     05  W-ONBASE-CODE           PIC  XX.
000049     05  FILLER                  PIC  X.
000050     05  W-ACCT-SUMM             PIC  X.
000051     05  FILLER                  PIC  X.
000052     05  W-CSO-SUMM              PIC  X.
000053     05  FILLER                  PIC  X.
000054     05  W-REASONS-REQUIRED      PIC  X.
000055     05  FILLER                  PIC  X(29).
      *<<((file: ELCZREC))
000065
000066
000067 01  WS-ERARCH-KEY.
000068     05  WS-ERARCH-COMPANY-CD    PIC X.
000069     05  WS-ERARCH-ARCH-NO       PIC S9(8) BINARY.
000070
000071 01  ws-erpndb-key.
000072     05  ws-erpndb-company-cd    pic x.
000073     05  ws-erpndb-batch-no      pic x(6).
000074     05  ws-erpndb-batch-seq-no  pic s9(4) COMP.
000075     05  ws-erpndb-chg-seq-no    pic s9(4) COMP.
000076
000077 01  WS-ERENDT2-KEY.
000078     05  WS-ERENDT-COMPANY-CD    PIC X.
000079     05  WS-ERENDT-ARCH-NO       PIC 9(8) BINARY.
000080
000081 01  WS-NSASEXTR-KEY.
000082     05  WS-EXTR-COMPANY-CD    PIC X.
000083     05  WS-EXTR-ARCH-NO       PIC 9(8) BINARY.
000084     05  WS-EXTR-SEQ-NO        PIC 9(4) BINARY.
000085
000086 01  WS-ELLETR-KEY.
000087     05  WS-ELLETR-COMPANY-CD    PIC X.
000088     05  WS-ELLETR-LETTER-ID     PIC X(12).
000089     05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
000090
000091 01  BILL-NOTE-AREA.
000092     12  ws-from-issue-or-cancel pic x.
000093     12  WS-ELEOBC-KEY.
000094         16  WS-EOBC-COMPANY-CD  PIC X.
000095         16  WS-EOBC-REC-TYPE    PIC X.
000096         16  WS-EOBC-CODE        PIC X(4).
000097         16  FILLER              PIC X(9).
000098     12  WS-ERNOTE-KEY.
000099         16  WS-NOTE-COMPANY-CD  PIC X.
000100         16  WS-NOTE-CARRIER     PIC X.
000101         16  WS-NOTE-GROUPING    PIC X(6).
000102         16  WS-NOTE-STATE       PIC XX.
000103         16  WS-NOTE-ACCOUNT     PIC X(10).
000104         16  WS-NOTE-CERT-EFF-DT PIC XX.
000105         16  WS-NOTE-CERT-PRIME  PIC X(10).
000106         16  WS-NOTE-CERT-SFX    PIC X.
000107         16  ws-note-record-type pic x.
000108     12  WS-BILLING-NOTE.
000109         16  WS-BN-NOTE          PIC X(25).
000110         16  WS-BN-LTRID         PIC X(4).
000111         16  FILLER              PIC X(3).
000112         16  WS-BN-DATE          PIC X(8).
000113         16  FILLER              PIC X(3).
000114         16  WS-BN-USERID        PIC X(4).
000115         16  FILLER              PIC X(30).
000116     12  WS-LEN                  PIC S9(5) COMP-3 VALUE +0.
000117     12  WS-ELCERT-KEY.
000118         16  WS-CERT-COMPANY-CD  PIC X.
000119         16  WS-CERT-CARRIER     PIC X.
000120         16  WS-CERT-GROUPING    PIC X(6).
000121         16  WS-CERT-STATE       PIC XX.
000122         16  WS-CERT-ACCOUNT     PIC X(10).
000123         16  WS-CERT-CERT-EFF-DT PIC XX.
000124         16  WS-CERT-CERT-PRIME  PIC X(10).
000125         16  WS-CERT-CERT-SFX    PIC X.
000126     12  WS-SAVE-KEY.
000127         16  WS-SV-COMPANY-CD    PIC X.
000128         16  WS-SV-CARRIER       PIC X.
000129         16  WS-SV-GROUPING      PIC X(6).
000130         16  WS-SV-STATE         PIC XX.
000131         16  WS-SV-ACCOUNT       PIC X(10).
000132         16  WS-SV-CERT-EFF-DT   PIC XX.
000133         16  WS-SV-CERT-PRIME    PIC X(10).
000134         16  WS-SV-CERT-SFX      PIC X.
000135     12  WS-SAVE-DATA.
000136         16  WS-SV-LETTER-ID     PIC X(4).
000137         16  WS-SV-PROC-ID       PIC X(4).
000138
000139
000140 01 srch-commarea.
000141*                                copy ELCADLTRSPI.
      *>>((file: ELCADLTRSPI))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 060611    2011022800001  PEMA  NEW COPYBOOK
000010* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
000011* 110612    2012101700002  AJRA  EXPAND PASSED DATA
000012******************************************************************
000013****************************************
000014*  commarea for NaperSoft On Demand Admin services letters
000015*  (business logic input & output)
000016****************************************
000017
000018     03  BL-INPUT.
000019         05  BL-DATA-SRCE        PIC X.
000020         05  BL-LETTER-ID        PIC XXXX.
000021         05  BL-CARRIER          PIC X.
000022         05  BL-GROUP            PIC X(6).
000023         05  BL-STATE            PIC XX.
000024         05  BL-ACCOUNT          PIC X(10).
000025         05  BL-EFF-DT           PIC X(10).
000026         05  BL-CERT-NO          PIC X(11).
000027         05  BL-BATCH-NO         PIC X(6).
000028         05  BL-BATCH-SEQ        PIC 9(8).
000029         05  BL-RESP-NO          PIC X(10).
000030         05  BL-NO-OF-COPIES     PIC 99.
000031         05  BL-PROC-ID          PIC XXXX.
000032         05  BL-COMP-ID          PIC XXX.
000033         05  BL-PRINT-NOW-SW     PIC X.
000034         05  BL-ENC-CD           PIC XXX.
000035         05  BL-RESEND-DT        PIC X(10).
000036         05  BL-FOLLOW-UP-DT     PIC X(10).
000037         05  BL-ARCHIVE-NO       PIC 9(8).
000038         05  BL-FUNC             PIC X(8).
000039         05  BL-COMMENTS         PIC X(100).
000040         05  FILLER REDEFINES BL-COMMENTS.
000041             10  BL-REASON-CODE OCCURS 12 PIC X(4).
000042             10  BL-LETTER-TO-ACCT PIC X.
000043             10  BL-LETTER-TO-BENE PIC X.
000044             10  BL-WRITE-ERARCH   PIC X.
000045                 88  ERARCH-QWS      VALUE 'Q'.
000046                 88  ERARCH-BATCH    VALUE 'B'.
000047                 88  ERARCH-TEMP     VALUE 'T'.
000048             10  BL-PROCESS-TYPE PIC X(07).
000049             10  BL-CERT-FORM-ID PIC X(05).
000050             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
000051             10  BL-SOURCE-SCREEN PIC X(8).
000052             10  FILLER          PIC X(25).
000053
000054     03  BL-OUTPUT.
000055         05  BL-STATUS                   PIC X.
000056             88  BL-OK                      VALUE "P".
000057             88  BL-FAIL                  VALUE "F".
000058         05  BL-MESSAGE          PIC X(50).
000059     03  BL-RECORD-PASSED-DATA   PIC X(6200).
000060     03  FILLER                  PIC X(31).
      *<<((file: ELCADLTRSPI))
000142
000143
000144*                                COPY ELCDATE.
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
000145*                                copy ERCPNDB.
      *>>((file: ERCPNDB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.025                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
000009*                                                                *
000010******************************************************************
000011*   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
000012*         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
000013******************************************************************
000014*                                                                *
000015*                                                                *
000016*   FILE TYPE = VSAM,KSDS                                        *
000017*   RECORD SIZE = 585  RECFORM = FIXED                           *
000018*                                                                *
000019*   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
000020*       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
000021*                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
000022*                                                 RKP=13,LEN=36  *
000023*       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
000024*                                      AND CHG-SEQ.)             *
000025*                                                RKP=49,LEN=11   *
000026*       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
000027*                                      AND CHG-SEQ.)             *
000028*                                                RKP=60,LEN=15   *
000029*                                                                *
000030*   LOG = NO                                                     *
000031*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000032******************************************************************
000033******************************************************************
000034*                   C H A N G E   L O G
000035*
000036* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000037*-----------------------------------------------------------------
000038*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000039* EFFECTIVE    NUMBER
000040*-----------------------------------------------------------------
000041* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000042* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000043* 011904                   PEMA  ADD TOTAL FEE PROCESSING
000044* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000045* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
000046* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000047* 032306                   PEMA  ADD BOW LOAN NUMBER
000048* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
000049* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
000050* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000051* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000052* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
000053* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
000054* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000055* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
000056* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
000057* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000058* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
000059* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000060* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
000061******************************************************************
000062
000063 01  PENDING-BUSINESS.
000064     12  PB-RECORD-ID                     PIC XX.
000065         88  VALID-PB-ID                        VALUE 'PB'.
000066
000067     12  PB-CONTROL-PRIMARY.
000068         16  PB-COMPANY-CD                PIC X.
000069         16  PB-ENTRY-BATCH               PIC X(6).
000070         16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
000071         16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
000072
000073     12  PB-CONTROL-BY-ACCOUNT.
000074         16  PB-COMPANY-CD-A1             PIC X.
000075         16  PB-CARRIER                   PIC X.
000076         16  PB-GROUPING.
000077             20  PB-GROUPING-PREFIX       PIC XXX.
000078             20  PB-GROUPING-PRIME        PIC XXX.
000079         16  PB-STATE                     PIC XX.
000080         16  PB-ACCOUNT.
000081             20  PB-ACCOUNT-PREFIX        PIC X(4).
000082             20  PB-ACCOUNT-PRIME         PIC X(6).
000083         16  PB-CERT-EFF-DT               PIC XX.
000084         16  PB-CERT-NO.
000085             20  PB-CERT-PRIME            PIC X(10).
000086             20  PB-CERT-SFX              PIC X.
000087         16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
000088
000089         16  PB-RECORD-TYPE               PIC X.
000090             88  PB-MAILING-DATA                VALUE '0'.
000091             88  PB-ISSUE                       VALUE '1'.
000092             88  PB-CANCELLATION                VALUE '2'.
000093             88  PB-BATCH-TRAILER               VALUE '9'.
000094
000095     12  PB-CONTROL-BY-ORIG-BATCH.
000096         16  PB-ORIGINAL-COMPANY-CD       PIC X.
000097         16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
000098         16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
000099         16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
000100
000101     12  PB-CONTROL-BY-CSR.
000102         16  PB-CSR-COMPANY-CD            PIC X.
000103         16  PB-CSR-ID                    PIC X(4).
000104         16  PB-CSR-ENTRY-BATCH           PIC X(6).
000105         16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
000106         16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
000107******************************************************************
000108*    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
000109******************************************************************
000110
000111     12  PB-LAST-MAINT-DT                 PIC XX.
000112     12  PB-LAST-MAINT-BY                 PIC X(4).
000113     12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000114
000115     12  PB-RECORD-BODY                   PIC X(375).
000116
000117     12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
000118         16  PB-CERT-ORIGIN               PIC X.
000119             88  CLASIC-CREATED-CERT         VALUE '1'.
000120         16  PB-I-NAME.
000121             20  PB-I-INSURED-LAST-NAME   PIC X(15).
000122             20  PB-I-INSURED-FIRST-NAME.
000123                 24  PB-I-INSURED-1ST-INIT PIC X.
000124                 24  FILLER                PIC X(9).
000125             20  PB-I-INSURED-MIDDLE-INIT PIC X.
000126         16  PB-I-AGE                     PIC S99   COMP-3.
000127         16  PB-I-JOINT-AGE               PIC S99   COMP-3.
000128         16  PB-I-BIRTHDAY                PIC XX.
000129         16  PB-I-INSURED-SEX             PIC X.
000130             88  PB-SEX-MALE     VALUE 'M'.
000131             88  PB-SEX-FEMALE   VALUE 'F'.
000132
000133         16  PB-I-LF-TERM                 PIC S999   COMP-3.
000134         16  PB-I-AH-TERM                 PIC S999   COMP-3.
000135         16  PB-I-LOAN-TERM               PIC S999   COMP-3.
000136         16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
000137         16  PB-I-SKIP-CODE               PIC X.
000138             88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
000139             88  PB-SKIP-JULY              VALUE '1'.
000140             88  PB-SKIP-AUGUST            VALUE '2'.
000141             88  PB-SKIP-SEPTEMBER         VALUE '3'.
000142             88  PB-SKIP-JULY-AUG          VALUE '4'.
000143             88  PB-SKIP-AUG-SEPT          VALUE '5'.
000144             88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
000145             88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
000146             88  PB-SKIP-JUNE              VALUE '8'.
000147             88  PB-SKIP-JUNE-JULY         VALUE '9'.
000148             88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
000149             88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
000150         16  PB-I-TERM-TYPE               PIC X.
000151             88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
000152             88  PB-PAID-WEEKLY            VALUE 'W'.
000153             88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
000154             88  PB-PAID-BI-WEEKLY         VALUE 'B'.
000155             88  PB-PAID-13-YEARLY         VALUE 'T'.
000156         16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
000157         16  PB-I-POLICY-FORM-NO          PIC X(12).
000158         16  PB-I-DATA-ENTRY-SW           PIC X.
000159             88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
000160             88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
000161             88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
000162             88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
000163         16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
000164         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
000165*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
000166         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
000167         16  PB-I-LETTER-REQD             PIC X.
000168
000169         16  PB-I-LIFE-BENEFIT-CD         PIC XX.
000170             88  PB-VALID-LIFE               VALUE '01' THRU '89'.
000171             88  PB-INVALID-LIFE             VALUE '  ' '00'
000172                                                   '90' THRU '99'.
000173         16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
000174                                          PIC XX.
000175         16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
000176         16  PB-I-AMOUNT-FINANCED REDEFINES
000177                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
000178         16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
000179         16  PB-I-UNPAID-CASH-PRICE REDEFINES
000180                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
000181         16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000182         16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
000183         16  PB-I-CLP-AMOUNT REDEFINES
000184                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
000185         16  PB-I-LF-CALC-FLAG            PIC X.
000186             88 PB-COMP-LF-PREM               VALUE '?'.
000187         16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
000188         16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
000189         16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
000190         16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
000191         16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
000192         16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
000193         16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
000194         16  PB-I-LF-ABBR                 PIC XXX.
000195         16  PB-I-LF-INPUT-CD             PIC XX.
000196
000197         16  PB-I-AH-BENEFIT-CD           PIC XX.
000198             88  PB-VALID-AH                 VALUE '01' THRU '89'.
000199             88  PB-INVALID-AH               VALUE '  ' '00'
000200                                                   '90' THRU '99'.
000201         16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
000202         16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000203         16  PB-I-AH-CALC-FLAG            PIC X.
000204             88 PB-COMP-AH-PREM                  VALUE '?'.
000205         16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
000206         16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
000207         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
000208         16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
000209         16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
000210         16  PB-I-AH-ABBR                 PIC XXX.
000211         16  PB-I-AH-INPUT-CD             PIC XXX.
000212
000213         16  PB-I-SPECIAL-REIN-CODE       PIC X.
000214         16  PB-I-REIN-TABLE              PIC XXX.
000215         16  PB-I-BUSINESS-TYPE           PIC 99.
000216         16  PB-I-INDV-GRP-CD             PIC X.
000217         16  PB-I-MORT-CODE.
000218             20  PB-I-TABLE               PIC X.
000219             20  PB-I-INTEREST            PIC XX.
000220             20  PB-I-MORT-TYP            PIC X.
000221         16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
000222         16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
000223         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
000224         16  PB-I-INDV-GRP-OVRD           PIC X.
000225         16  PB-I-RATE-CLASS-OVRD         PIC XX.
000226         16  PB-I-SIG-SW                  PIC X.
000227             88  PB-POLICY-SIGNED             VALUE 'Y'.
000228         16  PB-I-RATE-CLASS              PIC XX.
000229         16  PB-I-RATE-DEVIATION-LF       PIC XXX.
000230         16  PB-I-RATE-DEVIATION-AH       PIC XXX.
000231         16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
000232         16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
000233         16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
000234         16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
000235         16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
000236         16  PB-I-BENEFIT-TYPE            PIC XXX.
000237         16  PB-I-OB-FLAG                 PIC X.
000238             88  PB-I-OB                      VALUE 'B'.
000239             88  PB-I-SUMMARY                 VALUE 'Z'.
000240         16  PB-I-ENTRY-STATUS            PIC X.
000241             88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
000242                                              'M' '5' '9' '2'.
000243             88  PB-I-NORMAL-ENTRY            VALUE '1'.
000244             88  PB-I-POLICY-PENDING          VALUE '2'.
000245             88  PB-I-CONVERSION-ENTRY        VALUE '4'.
000246             88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
000247             88  PB-I-POLICY-IS-CASH          VALUE 'C'.
000248             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
000249             88  PB-I-REIN-ONLY               VALUE '9'.
000250             88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
000251             88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
000252             88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
000253             88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
000254         16  PB-I-INT-CODE                PIC X.
000255             88  PB-ADD-ON-INTEREST           VALUE 'A'.
000256             88  PB-SIMPLE-INTEREST           VALUE 'S'.
000257         16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
000258         16  PB-I-SOC-SEC-NO              PIC X(11).
000259         16  PB-I-MEMBER-NO               PIC X(12).
000260         16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
000261*        16  PB-I-LOAN-OFFICER            PIC XXX.
000262         16  PB-I-OLD-LOF                 PIC XXX.
000263         16  PB-I-LF-EXPIRE-DT            PIC XX.
000264         16  PB-I-AH-EXPIRE-DT            PIC XX.
000265         16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
000266         16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
000267         16  PB-I-LIFE-INDICATOR          PIC X.
000268             88  PB-I-JOINT-COVERAGE         VALUE 'J'.
000269         16  PB-I-LIVES                   PIC S9(7)       COMP-3.
000270         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
000271                                          PIC S9(5)V99    COMP-3.
000272         16  PB-I-MAIL-ADDRS-SW           PIC X.
000273             88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
000274             88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
000275         16  PB-I-1ST-PMT-DT              PIC XX.
000276         16  PB-I-JOINT-INSURED.
000277             20 PB-I-JOINT-LAST-NAME      PIC X(15).
000278             20 PB-I-JOINT-FIRST-NAME.
000279                24  PB-I-JOINT-FIRST-INIT PIC X.
000280                24  FILLER                PIC X(9).
000281             20 PB-I-JOINT-MIDDLE-INIT    PIC X.
000282*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
000283         16  PB-I-BENEFICIARY-NAME.
000284             20  PB-I-BANK-NUMBER         PIC X(10).
000285             20  FILLER                   PIC X(15).
000286         16  PB-I-LAST-ADD-ON-DT          PIC XX.
000287         16  PB-I-REFERENCE               PIC X(12).
000288         16  FILLER REDEFINES PB-I-REFERENCE.
000289             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
000290             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
000291             20  PB-I-CLP-STATE           PIC XX.
000292         16  PB-I-UNDERWRITING-STATUS     PIC X.
000293             88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
000294             88  PB-I-POLICY-DECLINED         VALUE 'D'.
000295             88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
000296         16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
000297         16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
000298         16  PB-I-RESIDENT-STATE          PIC XX.
000299         16  PB-I-RATE-CODE               PIC X(4).
000300         16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
000301         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000302         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000303         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
000304         16  PB-I-BANK-NOCHRGB            PIC 99.
000305         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
000306         16  PB-I-JOINT-BIRTHDAY          PIC XX.
000307
000308     12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
000309         16  PB-C-LF-CANCEL-VOID-SW       PIC X.
000310             88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
000311         16  PB-C-CANCEL-ORIGIN           PIC X.
000312             88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
000313         16  PB-C-LF-CANCEL-DT            PIC XX.
000314         16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000315         16  PB-C-LF-CALC-REQ             PIC X.
000316             88 PB-COMP-LF-CANCEL            VALUE '?'.
000317         16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
000318         16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
000319         16  PB-C-AH-CANCEL-VOID-SW       PIC X.
000320             88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
000321         16  PB-C-AH-CANCEL-DT            PIC XX.
000322         16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000323         16  PB-C-AH-CALC-REQ             PIC X.
000324             88 PB-COMP-AH-CANCEL            VALUE '?'.
000325         16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
000326         16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
000327         16  PB-C-LAST-NAME               PIC X(15).
000328         16  PB-C-REFUND-SW               PIC X.
000329             88  PB-C-REFUND-CREATED          VALUE 'Y'.
000330             88  PB-C-REFUND-REQUESTED        VALUE 'R'.
000331         16  PB-C-LIVES                   PIC S9(3)       COMP-3.
000332         16  PB-C-PAYEE-CODE              PIC X(6).
000333         16  PB-C-LF-REFUND-OVERRIDE      PIC X.
000334         16  PB-C-AH-REFUND-OVERRIDE      PIC X.
000335         16  PB-C-LF-COMM-CHARGEBACK      PIC X.
000336         16  PB-C-AH-COMM-CHARGEBACK      PIC X.
000337         16  PB-C-REFERENCE               PIC X(12).
000338         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000339         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000340         16  PB-C-POST-CARD-IND           PIC X.
000341         16  PB-C-CANCEL-REASON           PIC X.
000342         16  PB-C-REF-INTERFACE-SW        PIC X.
000343         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
000344         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
000345         16  FILLER                       PIC X(01).
000346*        16  FILLER                       PIC X(18).
000347         16  PB-C-POLICY-FORM-NO          PIC X(12).
000348*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
000349         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
000350         16  PB-CANCELED-CERT-DATA.
000351             20  PB-CI-INSURED-NAME.
000352                 24  PB-CI-LAST-NAME      PIC X(15).
000353                 24  PB-CI-INITIALS       PIC XX.
000354             20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
000355             20  PB-CI-INSURED-SEX        PIC X.
000356             20  PB-CI-LF-TERM            PIC S999        COMP-3.
000357             20  PB-CI-LF-BENEFIT-CD      PIC XX.
000358             20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
000359             20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
000360             20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000361             20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
000362             20  PB-CI-AH-TERM            PIC S999        COMP-3.
000363             20  PB-CI-AH-BENEFIT-CD      PIC XX.
000364             20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
000365             20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000366             20  PB-CI-RATE-CLASS         PIC XX.
000367             20  PB-CI-RATE-DEV-LF        PIC XXX.
000368             20  PB-CI-RATE-DEV-AH        PIC XXX.
000369             20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
000370             20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
000371             20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
000372             20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
000373             20  PB-CI-LF-ABBR            PIC X(3).
000374             20  PB-CI-AH-ABBR            PIC X(3).
000375             20  PB-CI-OB-FLAG            PIC X.
000376                 88  PB-CI-OB                VALUE 'B'.
000377             20  PB-CI-LF-POLICY-STATUS   PIC X.
000378                 88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
000379                                           'M' '4' '5' '9' '2'.
000380                 88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
000381                 88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
000382                 88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
000383                 88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
000384                 88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
000385                 88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
000386                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000387                 88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
000388                 88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000389                 88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
000390                 88  PB-CI-LF-REIN-ONLY              VALUE '9'.
000391                 88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
000392                 88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
000393             20  PB-CI-AH-POLICY-STATUS   PIC X.
000394                 88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
000395                                           'M' '4' '5' '9' '2'.
000396                 88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
000397                 88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
000398                 88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
000399                 88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
000400                 88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
000401                 88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
000402                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000403                 88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
000404                 88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000405                 88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
000406                 88  PB-CI-AH-REIN-ONLY              VALUE '9'.
000407                 88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
000408                 88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
000409             20  PB-CI-PAY-FREQUENCY      PIC 99.
000410             20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
000411             20  PB-CI-SOC-SEC-NO         PIC X(11).
000412             20  PB-CI-MEMBER-NO          PIC X(12).
000413             20  PB-CI-INT-CODE           PIC X.
000414                 88  PB-CI-ADD-ON                  VALUE 'A'.
000415                 88  PB-CI-SIMPLE                  VALUE 'S'.
000416             20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
000417             20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
000418             20  PB-CI-COMP-EXCP-SW       PIC X.
000419                 88  PB-CI-NO-COMP-EXCP            VALUE ' '.
000420                 88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
000421             20  PB-CI-ENTRY-STATUS       PIC X.
000422             20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
000423             20  PB-CI-AH-PAID-THRU-DT    PIC XX.
000424             20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
000425             20  PB-CI-DEATH-DT           PIC XX.
000426             20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
000427             20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
000428             20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000429             20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000430             20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
000431             20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
000432             20  PB-CI-ENTRY-DT              PIC XX.
000433             20  PB-CI-ENTRY-BATCH           PIC X(6).
000434             20  PB-CI-LF-EXPIRE-DT          PIC XX.
000435             20  PB-CI-AH-EXPIRE-DT          PIC XX.
000436             20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
000437             20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
000438             20  PB-CI-OLD-LOF               PIC XXX.
000439*            20  PB-CI-LOAN-OFFICER          PIC XXX.
000440             20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
000441             20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
000442             20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
000443             20  PB-CI-INDV-GRP-CD           PIC X.
000444             20  PB-CI-BENEFICIARY-NAME.
000445                 24  PB-CI-BANK-NUMBER       PIC X(10).
000446                 24  FILLER                  PIC X(15).
000447             20  PB-CI-NOTE-SW               PIC X.
000448             20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
000449             20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
000450             20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
000451             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
000452             20  PB-CI-LOAN-OFFICER          PIC X(5).
000453             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
000454             20  PB-CI-FIRST-NAME            PIC X(10).
000455             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
000456
000457         16  FILLER                       PIC X(13).
000458*032306  16  FILLER                       PIC X(27).
000459*        16  FILLER                       PIC X(46).
000460
000461     12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
000462         16  FILLER                       PIC X(10).
000463         16  PB-M-INSURED-LAST-NAME       PIC X(15).
000464         16  PB-M-INSURED-FIRST-NAME      PIC X(10).
000465         16  PB-M-INSURED-MID-INIT        PIC X.
000466         16  PB-M-INSURED-AGE             PIC 99.
000467         16  PB-M-INSURED-BIRTHDAY        PIC XX.
000468         16  PB-M-INSURED-SEX             PIC X.
000469         16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
000470         16  PB-M-INSURED-ADDRESS-1       PIC X(30).
000471         16  PB-M-INSURED-ADDRESS-2       PIC X(30).
000472         16  PB-M-INSURED-CITY-STATE.
000473             20  PB-M-INSURED-CITY        PIC X(28).
000474             20  PB-M-INSURED-STATE       PIC XX.
000475         16  PB-M-INSURED-ZIP-CODE.
000476             20  PB-M-INSURED-ZIP-PRIME.
000477                 24  PB-M-INSURED-ZIP-1   PIC X.
000478                     88  PB-M-CANADIAN-POST-CODE
000479                                             VALUE 'A' THRU 'Z'.
000480                 24  FILLER               PIC X(4).
000481             20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
000482         16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
000483                                        PB-M-INSURED-ZIP-CODE.
000484             20  PM-M-INS-CAN-POST1       PIC XXX.
000485             20  PM-M-INS-CAN-POST2       PIC XXX.
000486             20  FILLER                   PIC XXX.
000487         16  PB-M-INSURED-PHONE-NO        PIC 9(10).
000488         16  PB-M-JOINT-BIRTHDAY          PIC XX.
000489         16  PB-M-CRED-BENE-NAME          PIC X(30).
000490         16  PB-M-CRED-BENE-ADDR1         PIC X(30).
000491         16  PB-M-CRED-BENE-ADDR2         PIC X(30).
000492         16  PB-M-CRED-BENE-CITYST.
000493             20  PB-M-CRED-BENE-CITY      PIC X(28).
000494             20  PB-M-CRED-BENE-STATE     PIC XX.
000495
000496         16  FILLER                       PIC X(92).
000497
000498     12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
000499         16  FILLER                       PIC X(10).
000500         16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000501         16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000502         16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000503         16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000504         16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000505         16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000506         16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000507         16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000508         16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000509         16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000510         16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000511         16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000512         16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
000513         16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
000514         16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
000515         16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
000516         16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
000517         16  PB-ACCOUNT-NAME              PIC X(30).
000518         16  PB-PREM-REF-RPT-FLAG         PIC X.
000519         16  PB-REFERENCE                 PIC X(12).
000520         16  PB-B-RECEIVED-DT             PIC XX.
000521         16  FILLER                       PIC X(234).
000522
000523     12  PB-RECORD-STATUS.
000524         16  PB-CREDIT-SELECT-DT          PIC XX.
000525         16  PB-CREDIT-ACCEPT-DT          PIC XX.
000526         16  PB-BILLED-DT                 PIC XX.
000527         16  PB-BILLING-STATUS            PIC X.
000528             88  PB-ENTRY-REVERSED            VALUE 'R'.
000529             88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
000530             88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
000531         16  PB-RECORD-BILL               PIC X.
000532             88  PB-RECORD-ON-HOLD            VALUE 'H'.
000533             88  PB-RECORD-RETURNED           VALUE 'R'.
000534             88  PB-RECORD-ENDORSED           VALUE 'E'.
000535             88  PB-OVERRIDE-LIFE             VALUE 'L'.
000536             88  PB-OVERRIDE-AH               VALUE 'A'.
000537             88  PB-OVERRIDE-BOTH             VALUE 'B'.
000538         16  PB-BATCH-ENTRY               PIC X.
000539             88  PB-POLICY-IS-DECLINED        VALUE 'D'.
000540             88  PB-REIN-ONLY-CERT            VALUE 'R'.
000541             88  PB-REISSUED-CERT             VALUE 'E'.
000542             88  PB-CASH-CERT                 VALUE 'C'.
000543             88  PB-MONTHLY-CERT              VALUE 'M'.
000544             88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
000545             88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
000546             88  PB-POLICY-IS-VOIDED          VALUE 'V'.
000547         16  PB-FORCE-CODE                PIC X.
000548             88  PB-FORCE-OFF                 VALUE ' ' '0'.
000549             88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
000550             88  PB-CANCEL-FORCE              VALUE '8'.
000551             88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
000552             88  PB-ALL-CANCEL-FORCED         VALUE '8'.
000553             88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
000554             88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
000555             88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
000556             88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
000557             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
000558             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
000559         16  PB-FATAL-FLAG                PIC X.
000560             88  PB-FATAL-ERRORS              VALUE 'X'.
000561         16  PB-FORCE-ER-CD               PIC X.
000562             88  PB-FORCE-ERRORS              VALUE 'F'.
000563             88  PB-UNFORCED-ERRORS           VALUE 'X'.
000564         16  PB-WARN-ER-CD                PIC X.
000565             88  PB-WARNING-ERRORS            VALUE 'W'.
000566         16  FILLER                       PIC X.
000567         16  PB-OUT-BAL-CD                PIC X.
000568             88  PB-OUT-OF-BAL                VALUE 'O'.
000569         16  PB-LIFE-OVERRIDE-L1          PIC X.
000570         16  PB-AH-OVERRIDE-L1            PIC X.
000571         16  PB-INPUT-DT                  PIC XX.
000572         16  PB-INPUT-BY                  PIC X(4).
000573         16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
000574         16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
000575         16  PB-TOLERANCE-REJECT-SW       PIC X.
000576         16  PB-LF-EARNING-METHOD         PIC X.
000577         16  PB-AH-EARNING-METHOD         PIC X.
000578         16  PB-LF-TERM-CALC-METHOD       PIC X.
000579         16  PB-AH-TERM-CALC-METHOD       PIC X.
000580         16  PB-REIN-CD                   PIC XXX.
000581         16  PB-LF-REFUND-TYPE            PIC X.
000582         16  PB-AH-REFUND-TYPE            PIC X.
000583         16  PB-ACCT-EFF-DT               PIC XX.
000584         16  PB-ACCT-EXP-DT               PIC XX.
000585         16  PB-COMPANY-ID                PIC X(3).
000586         16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000587         16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000588         16  PB-SV-CARRIER                PIC X.
000589         16  PB-SV-GROUPING               PIC X(6).
000590         16  PB-SV-STATE                  PIC XX.
000591         16  PB-CONFIRMATION-REPT-DT      PIC XX.
000592         16  PB-GA-BILLING-INFO.
000593             20  PB-GA-BILL-DT OCCURS 5 TIMES
000594                                          PIC XX.
000595         16  PB-SV-REMIT-TO  REDEFINES
000596             PB-GA-BILLING-INFO           PIC X(10).
000597         16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
000598         16  PB-I-LOAN-OFFICER            PIC X(5).
000599         16  PB-I-VIN                     PIC X(17).
000600
000601         16  FILLER                       PIC X(04).
000602         16  IMNET-BYPASS-SW              PIC X.
000603
000604******************************************************************
000605*                COMMON EDIT ERRORS                              *
000606******************************************************************
000607
000608     12  PB-COMMON-ERRORS.
000609         16  PB-COMMON-ERROR    OCCURS 10 TIMES
000610                                           PIC S9(4)     COMP.
000611
000612******************************************************************
      *<<((file: ERCPNDB))
000146*                                COPY ERCARCH.
      *>>((file: ERCARCH))
000001******************************************************************
000002*                                                                *
000003*                            ERCARCH.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 250  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
000013*     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
000014*     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
000015*     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
000016*     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
000017*     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
000018*                                                                *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
000029* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
000030* 110612    2012101700002  AJRA  ADD NEW FIELDS
000031* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
000032******************************************************************
000033 01  LETTER-ARCHIVE.
000034     12  LA-RECORD-ID                PIC  X(02).
000035         88  LA-VALID-ID                VALUE 'LA'.
000036
000037     12  LA-CONTROL-PRIMARY.
000038         16  LA-COMPANY-CD           PIC  X(01).
000039         16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
000040
000041     12  LA-CONTROL-BY-CERT-RESP.
000042         16  LA-COMPANY-CD-A2        PIC  X(01).
000043         16  LA-CERT-NO-A2.
000044             20  LA-CERT-PRIME-A2    PIC  X(10).
000045             20  LA-CERT-SUFFIX-A2   PIC  X(01).
000046         16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
000047             20  LA-RESP-PERSON-A2   PIC  X(10).
000048             20  LA-TYPE-A2          PIC  X(01).
000049         16  LA-CARRIER-A2           PIC  X(01).
000050         16  LA-GROUPING-A2          PIC  X(06).
000051         16  LA-STATE-A2             PIC  X(02).
000052         16  LA-ACCOUNT-A2           PIC  X(10).
000053         16  LA-EFFECT-DATE-A2       PIC  X(02).
000054         16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
000055
000056     12  LA-CONTROL-BY-FORM.
000057         16  LA-COMPANY-CD-A3        PIC  X(01).
000058         16  LA-FORM-A3              PIC  X(04).
000059         16  LA-CARRIER-A3           PIC  X(01).
000060         16  LA-GROUPING-A3          PIC  X(06).
000061         16  LA-STATE-A3             PIC  X(02).
000062         16  LA-ACCOUNT-A3           PIC  X(10).
000063         16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
000064
000065     12  LA-CONTROL-BY-PROCESSOR.
000066         16  LA-COMPANY-CD-A4        PIC  X(01).
000067         16  LA-PROCESSOR-CD         PIC  X(04).
000068         16  LA-CARRIER-A4           PIC  X(01).
000069         16  LA-GROUPING-A4          PIC  X(06).
000070         16  LA-STATE-A4             PIC  X(02).
000071         16  LA-ACCOUNT-A4           PIC  X(10).
000072         16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
000073
000074     12  LA-CONTROL-BY-KEY-FIELDS.
000075         16  LA-COMPANY-CD-A5        PIC  X(01).
000076         16  LA-CARRIER-A5           PIC  X(01).
000077         16  LA-GROUPING-A5          PIC  X(06).
000078         16  LA-STATE-A5             PIC  X(02).
000079         16  LA-ACCOUNT-A5           PIC  X(10).
000080         16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
000081
000082     12  LA-CONTROL-BY-GROUP-CODE.
000083         16  LA-COMPANY-CD-A6        PIC  X(01).
000084         16  LA-ENTRY-A6.
000085             20  LA-FILLER           PIC  X(02).
000086             20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
000087         16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
000088
000089     12  FILLER                      PIC  X(09).
000090
000091     12  LA-HEADER-RECORD.
000092         16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
000093         16  LA-CREATION-DATE        PIC  X(02).
000094         16  LA-FOLLOW-UP-DATE       PIC  X(02).
000095         16  LA-FINAL-ACT-DATE       REDEFINES
000096               LA-FOLLOW-UP-DATE     PIC  X(02).
000097         16  LA-INITIAL-PRINT-DATE   PIC  X(02).
000098         16  LA-NO-OF-COPIES         PIC S9(01).
000099         16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
000100         16  LA-REPLY-DATE           PIC  X(02).
000101         16  LA-RESEND-DATES.
000102             20  LA-RESEND-DATE      PIC  X(02).
000103             20  LA-SENT-DATE        PIC  X(02).
000104             20  FILLER              PIC  X(08).
000105         16  LA-SOURCE-INFORMATION.
000106             20  LA-DATA-SOURCE      PIC  X(01).
000107             20  LA-ADDR-SOURCE      PIC  X(01).
000108         16  LA-STATUS               PIC  X(01).
000109             88  LA-STATUS-ACTIVE         VALUE 'A'.
000110             88  LA-STATUS-COMPLETED      VALUE 'C'.
000111             88  LA-STATUS-ON-HOLD        VALUE 'H'.
000112             88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
000113             88  LA-STATUS-PURGED         VALUE 'P'.
000114             88  LA-STATUS-VOIDED         VALUE 'V'.
000115         16  LA-LAST-RESENT-PRINT-DATE
000116                                     PIC  X(02).
000117         16  LA-PRINT-RESTRICTION    PIC  X(01).
000118             88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
000119                                          VALUE 'C'.
000120             88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
000121                                          VALUE 'F'.
000122             88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
000123                                          VALUE 'P'.
000124         16  LA-PURGED-DATE          PIC  X(02).
000125         16  LA-VOIDED-DATE          PIC  X(02).
000126         16  LA-RESEND-LETR          PIC  X(4).
000127         16  LA-VOID-ONBASE-YN       PIC  X.
000128         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
000129         16  FILLER                  PIC  X(04).
000130*        16  LA-RESEND-LETR-2        PIC  X(4).
000131*        16  LA-RESEND-LETR-3        PIC  X(4).
000132*        16  FILLER                  PIC  X(59).
000133         16  LA-ARCHIVE-STATUS       PIC  X.
000134             88  LA-TEMP                VALUE 'T'.
000135             88  LA-QWS                 VALUE 'Q'.
000136             88  LA-BATCH               VALUE 'B'.
000137         16  LA-FINAL-ACT-IND        PIC  X(1).
000138         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
000139         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
000140         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
000141                                     PIC X(4).
000142         16  FILLER                  PIC  X(42).
000143*        16  FILLER                  PIC  X(71).
000144         16  LA-LAST-MAINT-DATE      PIC  X(2).
000145         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
000146         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
000147                                     PIC  X(4).
000148         16  LA-LAST-UPDATED-BY      PIC  X(4).
000149
000150******************************************************************
      *<<((file: ERCARCH))
000147*                                COPY ERCENDT.
      *>>((file: ERCENDT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCENDT.                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ENDORSEMENT FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 579  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ERENDT                         RKP=02,LEN=36  *
000013*       ALTERNATE PATH1 = ERENDT2 (BY ARCH NO)    RKP=38,LEN=05  *
000014*                                                                *
000015*   LOG = NO                                                     *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018*-----------------------------------------------------------------
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
000027*                                WITH OPEN CLAIMS
000028* 072312    2011022800001  AJRA  ADDED BATCH NUMBER
000029* 110612    2012101700002  AJRA  ADD NEW FIELDS
000030* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000031* 010616  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
000032*-----------------------------------------------------------------
000033
000034 01  ENDORSEMENT-RECORD.
000035     12  EN-RECORD-ID                PIC XX.
000036         88  VALID-EN-ID                VALUE 'EN'.
000037
000038     12  EN-CONTROL-PRIMARY.
000039         16  EN-COMPANY-CD           PIC X.
000040         16  EN-CARRIER              PIC X.
000041         16  EN-GROUPING             PIC X(6).
000042         16  EN-STATE                PIC XX.
000043         16  EN-ACCOUNT              PIC X(10).
000044         16  EN-CERT-EFF-DT          PIC XX.
000045         16  EN-CERT-NO.
000046             20  EN-CERT-PRIME       PIC X(10).
000047             20  EN-CERT-SFX         PIC X.
000048         16  EN-REC-TYPE             PIC X.
000049             88  EN-ISSUE               VALUE 'I'.
000050             88  EN-CANCELLATION        VALUE 'C'.
000051         16  EN-SEQ-NO               PIC 9(04) BINARY.
000052
000053     12  EN-CONTROL-BY-ARCH-NO.
000054         16  EN-COMPANY-CD-A1              PIC X.
000055         16  EN-ARCHIVE-NO                 PIC 9(8) BINARY.
000056
000057     12  EN-LAST-MAINT-DT            PIC XX.
000058     12  EN-LAST-MAINT-BY            PIC X(4).
000059     12  EN-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
000060
000061     12  EN-ENDORSEMENT-RECORD       PIC X(329).
000062
000063     12  EN-ISSUE-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
000064         16  EN-INS-ORIG-LAST-NAME   PIC X(15).
000065         16  EN-INS-ORIG-FIRST-NAME  PIC X(10).
000066         16  EN-INS-ORIG-MIDDLE-INIT PIC X.
000067         16  EN-INS-ORIG-AGE         PIC S999     COMP-3.
000068         16  EN-JNT-ORIG-LAST-NAME   PIC X(15).
000069         16  EN-JNT-ORIG-FIRST-NAME  PIC X(10).
000070         16  EN-JNT-ORIG-MIDDLE-INIT PIC X.
000071         16  EN-JNT-ORIG-AGE         PIC S999     COMP-3.
000072         16  EN-LF-ORIG-BENCD        PIC XX.
000073         16  EN-LF-ORIG-TERM         PIC S999      COMP-3.
000074         16  EN-LF-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
000075         16  EN-LF-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
000076         16  EN-LF-ORIG-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
000077         16  EN-LF-ORIG-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
000078         16  EN-LF-ORIG-EXP-DT       PIC XX.
000079*        16  EN-LF-ORIG-COV-TYPE     PIC X(10).
000080         16  EN-ORIG-CRED-BENE       PIC X(25).
000081         16  EN-LF-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
000082         16  FILLER                  PIC X.
000083         16  EN-AH-ORIG-BENCD        PIC XX.
000084         16  EN-AH-ORIG-TERM         PIC S999      COMP-3.
000085         16  EN-AH-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
000086         16  EN-AH-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
000087         16  EN-AH-ORIG-EXP-DT       PIC XX.
000088*        16  EN-AH-ORIG-COV-TYPE     PIC X(10).
000089*        16  EN-AH-ORIG-WAIT-PER     PIC 99.
000090         16  EN-AH-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
000091         16  F                       PIC X(09).
000092         16  EN-AH-ORIG-CP           PIC 99.
000093
000094         16  EN-INS-NEW-LAST-NAME    PIC X(15).
000095         16  EN-INS-NEW-FIRST-NAME   PIC X(10).
000096         16  EN-INS-NEW-MIDDLE-INIT  PIC X.
000097         16  EN-INS-NEW-AGE          PIC S999     COMP-3.
000098         16  EN-JNT-NEW-LAST-NAME    PIC X(15).
000099         16  EN-JNT-NEW-FIRST-NAME   PIC X(10).
000100         16  EN-JNT-NEW-MIDDLE-INIT  PIC X.
000101         16  EN-JNT-NEW-AGE          PIC S999     COMP-3.
000102         16  EN-LF-NEW-BENCD         PIC XX.
000103         16  EN-LF-NEW-TERM          PIC S999      COMP-3.
000104         16  EN-LF-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
000105         16  EN-LF-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
000106         16  EN-LF-NEW-ALT-BEN-AMT   PIC S9(9)V99  COMP-3.
000107         16  EN-LF-NEW-ALT-PRM-AMT   PIC S9(7)V99  COMP-3.
000108         16  EN-LF-NEW-EXP-DT        PIC XX.
000109         16  EN-NEW-CRED-BENE        PIC X(25).
000110         16  EN-LF-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
000111         16  FILLER                  PIC X.
000112         16  EN-AH-NEW-BENCD         PIC XX.
000113         16  EN-AH-NEW-TERM          PIC S999      COMP-3.
000114         16  EN-AH-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
000115         16  EN-AH-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
000116         16  EN-AH-NEW-EXP-DT        PIC XX.
000117*        16  EN-AH-NEW-COV-TYPE      PIC X(10).
000118*        16  EN-AH-NEW-WAIT-PER      PIC 99.
000119*        16  F                       PIC X(12).
000120         16  EN-AH-NEW-CP            PIC 99.
000121         16  EN-SIG-SW               PIC X.
000122         16  EN-AH-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
000123         16  EN-INS-ORIG-AGE-DEF-FLAG PIC X.
000124         16  EN-JNT-ORIG-AGE-DEF-FLAG PIC X.
000125         16  EN-INS-NEW-AGE-DEF-FLAG PIC X.
000126         16  EN-JNT-NEW-AGE-DEF-FLAG PIC X.
000127         16  FILLER                  PIC X(33).
000128     12  EN-CANCEL-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
000129         16  EN-LF-ORIG-REF-DT       PIC XX.
000130         16  EN-LF-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
000131         16  EN-AH-ORIG-REF-DT       PIC XX.
000132         16  EN-AH-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
000133         16  EN-LF-NEW-REF-DT        PIC XX.
000134         16  EN-LF-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
000135         16  EN-AH-NEW-REF-DT        PIC XX.
000136         16  EN-AH-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
000137         16  EN-FLAG-CERT            PIC X.
000138         16  EN-INS-LAST-NAME        PIC X(15).
000139         16  EN-INS-FIRST-NAME       PIC X(10).
000140         16  EN-INS-MIDDLE-INIT      PIC X.
000141         16  EN-LF-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
000142         16  EN-AH-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
000143         16  EN-LF-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
000144         16  EN-AH-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
000145         16  FILLER                  PIC X(262).
000146
000147     12  EN-MONEY-SW             PIC X.
000148     12  EN-HEALTH-APP           PIC X.
000149     12  EN-VOUCHER-SW           PIC X.
000150     12  EN-PAYEE                PIC X(14).
000151     12  EN-INPUT-DT             PIC XX.
000152     12  EN-PROCESS-DT           PIC XX.
000153     12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
000154     12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
000155
000156     12  EN-REASON-CODES.
000157         16  F OCCURS 12.
000158             20  EN-REASON-CODE  PIC X(4).
000159     12  EN-TEMPLATE-USED        PIC X(8).
000160     12  EN-DOCU-TYPE            PIC X.
000161         88  EN-VERI-DOCU          VALUE 'V'.
000162         88  EN-GCE-DOCU           VALUE 'G'.
000163         88  EN-CANC-DOCU          VALUE 'C'.
000164     12  EN-COMMENTS1            PIC X(13).
000165     12  EN-COMMENTS2            PIC X(70).
000166     12  EN-COMM-CHGBK           PIC X.
000167         88  EN-DO-NOT-CHG-ACCT    VALUE 'N'.
000168         88  EN-CHG-ACCT           VALUE 'Y'.
000169     12  EN-CSO-PORTION          PIC S9(5)V99  COMP-3.
000170     12  EN-ACCT-PORTION         PIC S9(5)V99  COMP-3.
000171     12  EN-BATCH-NUMBER         PIC X(6).
000172     12  EN-ACCT-SUMM            PIC X.
000173     12  EN-CSO-SUMM             PIC X.
000174     12  en-check-type           pic x.
000175     12  FILLER                  PIC X(12).
000176
000177******************************************************************
000178
      *<<((file: ERCENDT))
000148*                                COPY NSCASEXTR.
      *>>((file: NSCASEXTR))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            NSCASEXTR.                          *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = NAPERSOFT ADMIN SERVICES EXTRACT FILE     *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 4500 RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = NSASEXTR                       RKP=0,LEN=07   *
000013*                                                                *
000014*   LOG = NO                                                     *
000015*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000016******************************************************************
000017*-----------------------------------------------------------------
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 072211    2011022800001  PEMA  NEW FILE
000026*-----------------------------------------------------------------
000027 01 NSAS-EXTRACT-RECORD.
000028    05  NSAS-CONTROL-PRIMARY.
000029        10  NSAS-COMPANY-CD      PIC X.
000030        10  NSAS-ARCHIVE-NO      PIC 9(8) BINARY.
000031        10  NSAS-SEQ-NO          PIC 9(4) BINARY.
000032    05  NSAS-LETTER-VARIABLES    PIC X(4350).
000033    05  FILLER                   PIC X(143).
      *<<((file: NSCASEXTR))
000149*                                COPY ERCNOTE.
      *>>((file: ERCNOTE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCNOTE                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
000009*                                                                *
000010*        FILE TYPE= VSAM,KSDS                                    *
000011*        RECORD SIZE = 825    RECFORM = FIXED                    *
000012*                                                                *
000013*        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
000014*                                                                *
000015*        LOG = YES                                               *
000016*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000017*                                                                *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
000027*                                FILE WILL CONTAIN BILLING NOTES O
000028* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
000029******************************************************************
000030
000031 01  CERTIFICATE-NOTE.
000032     12  CN-RECORD-ID                PIC  XX.
000033         88  VALID-CN-ID                  VALUE 'CN'.
000034
000035     12  CN-CONTROL-PRIMARY.
000036         16  CN-COMPANY-CD           PIC X.
000037         16  CN-CARRIER              PIC X.
000038         16  CN-GROUPING.
000039             20 CN-GROUPING-PREFIX   PIC XXX.
000040             20 CN-GROUPING-PRIME    PIC XXX.
000041         16  CN-STATE                PIC XX.
000042         16  CN-ACCOUNT.
000043             20 CN-ACCOUNT-PREFIX    PIC X(4).
000044             20 CN-ACCOUNT-PRIME     PIC X(6).
000045         16  CN-CERT-EFF-DT          PIC XX.
000046         16  CN-CERT-NO.
000047             20  CN-CERT-PRIME       PIC X(10).
000048             20  CN-CERT-SFX         PIC X.
000049         16  CN-RECORD-TYPE          PIC X.
000050             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
000051             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
000052     12  CN-BILLING-START-LINE-NO    PIC 99.
000053     12  CN-BILLING-END-LINE-NO      PIC 99.
000054
000055     12  CN-LINES.
000056         16  CN-LINE OCCURS 10       PIC X(77).
000057
000058     12  CN-LAST-MAINT-DT            PIC XX.
000059     12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
000060     12  CN-LAST-MAINT-USER          PIC X(4).
000061     12  FILLER                      PIC X(5).
000062******************************************************************
      *<<((file: ERCNOTE))
000150*                                COPY ELCEOBC.
      *>>((file: ELCEOBC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEOBC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM EOB CODE TABLE                                  *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000010*   VSAM EOB CODE TABLE                                          *
000011*                                                                *
000012*   FILE DESCRIPTION = EOB CODE TABLE                            *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 350   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELEOBC                    RKP=2,LEN=15   *
000018*                                                                *
000019*   LOG = NO                                                     *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021*                                                                *
000022*                                                                *
000023******************************************************************
000024*                   C H A N G E   L O G
000025*
000026* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000027*-----------------------------------------------------------------
000028*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000029* EFFECTIVE    NUMBER
000030*-----------------------------------------------------------------
000031* 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
000032* 081511    2011022800001  PEMA  CHG FOR ADMIN SERV NAPERSOFT
000033* 091913    2013090300001  AJRA  ADDITIONAL RECORD TYPES
000034******************************************************************
000035
000036 01  EOB-CODES.
000037     12  EO-RECORD-ID                      PIC XX.
000038         88  VALID-DN-ID                      VALUE 'EO'.
000039
000040     12  EO-CONTROL-PRIMARY.
000041         16  EO-COMPANY-CD                 PIC X.
000042         16  EO-RECORD-TYPE                PIC X.
000043             88  EO-EOB-RECS                  VALUE '1'.
000044             88  EO-VERIF-RECS                VALUE '2'.
000045             88  EO-GCE-RECS                  VALUE '3'.
000046             88  EO-CANC-RECS                 VALUE '4'.
000047             88  EO-BILL-NOTE-RECS            VALUE '5'.
000048         16  EO-EOB-CODE                   PIC X(4).
000049         16  FILLER                        PIC X(9).
000050
000051     12  EO-MAINT-INFORMATION.
000052         16  EO-LAST-MAINT-DT              PIC XX.
000053         16  EO-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000054         16  EO-LAST-MAINT-USER            PIC X(4).
000055         16  FILLER                        PIC XX.
000056
000057     12  EO-DESCRIPTION                    PIC X(275).
000058     12  FILLER                            PIC X(46).
000059******************************************************************
      *<<((file: ELCEOBC))
000151*                                COPY ELCCERT.
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
000152
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
000154
000155 01 dfhcommarea                  pic x(218).
000156
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'NSAASBL' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000157 VCOBOL-DUMMY-PROCEDURE.
000158
000159*    display ' entering nsaasbl '
000160
000161     MOVE EIBDATE                TO DC-JULIAN-YYDDD
000162     MOVE '5'                    TO DC-OPTION-CODE
000163     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
000164     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
000165     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
000166     MOVE DFHCOMMAREA            TO BL-INPUT
000167*****************************************************
000168* Using the information passed to this program,
000169* it will update the erarch record with a "Q"
000170* in the status field
000171*****************************************************
000172
000173     evaluate bl-comp-id
000174        when 'DCC'
000175           move X'05'            to ws-erarch-company-cd
000176        when 'AHL'
000177           move X'06'            to ws-erarch-company-cd
000178        when 'VPP'
000179           move X'07'            to ws-erarch-company-cd
000180        when 'FNL'
000181           move X'08'            to ws-erarch-company-cd
000182        when other
000183           move X'04'            to ws-erarch-company-cd
000184     end-evaluate
000185
000186     MOVE WS-ERARCH-COMPANY-CD   TO WS-ERENDT-COMPANY-CD
000187                                    WS-EXTR-COMPANY-CD
000188                                    ws-erpndb-company-cd
000189     MOVE BL-ARCHIVE-NO          TO WS-ERARCH-ARCH-NO
000190                                    WS-ERENDT-ARCH-NO
000191                                    WS-EXTR-ARCH-NO
000192
000193     SET BL-FAIL TO TRUE
000194
000195     
      * EXEC CICS READ
000196*         DATASET    ('ERARCH')
000197*         INTO       (LETTER-ARCHIVE)
000198*         RIDFLD     (WS-ERARCH-KEY)
000199*         UPDATE
000200*         RESP       (WS-RESPONSE)
000201*    END-EXEC
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002057' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032303537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 WS-ERARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000202
000203     IF RESP-NORMAL
000204        SET BL-OK TO TRUE
000205        IF (BL-PRINT-NOW-SW = 'P')
000206           or (bl-process-type = 'cancel')
000207           PERFORM 0050-DELETE-RECORDS
000208                                 THRU 0050-EXIT
000209        ELSE
000210           IF BL-PROCESS-TYPE = 'process'
000211              MOVE LA-COMPANY-CD-A2 TO WS-SV-COMPANY-CD
000212              MOVE LA-CARRIER-A2    TO WS-SV-CARRIER
000213              MOVE LA-GROUPING-A2   TO WS-SV-GROUPING
000214              MOVE LA-STATE-A2      TO WS-SV-STATE
000215              MOVE LA-ACCOUNT-A2    TO WS-SV-ACCOUNT
000216              MOVE LA-EFFECT-DATE-A2 TO WS-SV-CERT-EFF-DT
000217              MOVE LA-CERT-PRIME-A2 TO WS-SV-CERT-PRIME
000218              MOVE LA-CERT-SUFFIX-A2 TO WS-SV-CERT-SFX
000219              MOVE LA-FORM-A3       TO WS-SV-LETTER-ID
000220              MOVE LA-PROCESSOR-CD  TO WS-SV-PROC-ID
000221              MOVE 'Q'           TO LA-ARCHIVE-STATUS
000222              IF BL-PRINT-NOW-SW = 'Y'
000223                 MOVE SAVE-BIN-DATE TO LA-INITIAL-PRINT-DATE
000224                 IF (LA-RESEND-DATE EQUAL LOW-VALUES OR SPACES)
000225                  AND (LA-FOLLOW-UP-DATE EQUAL
000226                                 LOW-VALUES OR SPACES)
000227                    MOVE 'C'             TO LA-STATUS
000228                 END-IF
000229              END-IF
000230           ELSE
000231              MOVE 'T'           TO LA-ARCHIVE-STATUS
000232           END-IF
000233        END-IF
000234        
      * EXEC CICS REWRITE
000235*          FROM      (LETTER-ARCHIVE)
000236*          DATASET   ('ERARCH')
000237*          RESP       (WS-RESPONSE)
000238*       END-EXEC
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002096' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303032303936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000239
000240        IF BL-PROCESS-TYPE = 'process'
000241           PERFORM 0440-ADD-BILLING-NOTE THRU 0440-EXIT
000242        END-IF
000243     ELSE
000244        MOVE ' BAD READ UPDATE ON ERARCH '
000245                                 TO BL-MESSAGE
000246        display ' bad read on erarch ' ws-response
000247          ' ' bl-archive-no
000248     END-IF
000249     MOVE BL-INPUT               TO DFHCOMMAREA
000250
000251     
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00002113' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303032313133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000252
000253 0050-DELETE-RECORDS.
000254
000255     IF BL-FUNC = 'ClmResc' or 'ClmRefo'
000256        go to 0050-exit
000257     end-if
000258
000259     
      * EXEC CICS DELETE
000260*       DATASET    ('ERARCH')
000261*       RESP       (WS-RESPONSE)
000262*    END-EXEC
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&(                    &  N#00002121' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303032313231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ERARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000263     IF NOT RESP-NORMAL
000264        DISPLAY ' BAD DELETE ERARCH ' WS-RESPONSE
000265        GO TO 0050-EXIT
000266     END-IF
000267
000268     
      * EXEC CICS READ
000269*         DATASET    ('ERENDT2')
000270*         INTO       (ENDORSEMENT-RECORD)
000271*         RIDFLD     (WS-ERENDT2-KEY)
000272*         UPDATE
000273*         RESP       (WS-RESPONSE)
000274*    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
           MOVE 'ERENDT2' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002130' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032313330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 WS-ERENDT2-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000275
000276     IF NOT RESP-NORMAL
000277        DISPLAY ' BAD READ ON ERENDT2 ' WS-RESPONSE
000278     ELSE
000279        
      * EXEC CICS DELETE
000280*          DATASET    ('ERENDT2')
000281*          RESP       (WS-RESPONSE)
000282*       END-EXEC
           MOVE 'ERENDT2' TO DFHEIV1
      *    MOVE '&(                    &  N#00002141' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303032313431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ERENDT2-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000283        IF NOT RESP-NORMAL
000284           DISPLAY ' BAD DELETE ON ERENDT2 ' WS-RESPONSE
000285        END-IF
000286     END-IF
000287
000288     MOVE 0                      TO WS-EXTR-SEQ-NO
000289     
      * EXEC CICS READ
000290*         DATASET    ('NSASEXTR')
000291*         INTO       (NSAS-EXTRACT-RECORD)
000292*         RIDFLD     (WS-NSASEXTR-KEY)
000293*         UPDATE
000294*         RESP       (WS-RESPONSE)
000295*    END-EXEC
           MOVE LENGTH OF
            NSAS-EXTRACT-RECORD
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002151' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032313531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NSAS-EXTRACT-RECORD, 
                 DFHEIV11, 
                 WS-NSASEXTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000296
000297     IF NOT RESP-NORMAL
000298        DISPLAY ' BAD READ ON 0 NSASEXTR ' WS-RESPONSE
000299        GO TO 0050-EXIT
000300     ELSE
000301        
      * EXEC CICS DELETE
000302*          DATASET    ('NSASEXTR')
000303*          RESP       (WS-RESPONSE)
000304*       END-EXEC
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&(                    &  N#00002163' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303032313633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-NSASEXTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000305        IF NOT RESP-NORMAL
000306           DISPLAY ' BAD DELETE ON 0 NSASEXTR ' WS-RESPONSE
000307           GO TO 0050-EXIT
000308        END-IF
000309     END-IF
000310
000311     MOVE 1                      TO WS-EXTR-SEQ-NO
000312     
      * EXEC CICS READ
000313*         DATASET    ('NSASEXTR')
000314*         INTO       (NSAS-EXTRACT-RECORD)
000315*         RIDFLD     (WS-NSASEXTR-KEY)
000316*         UPDATE
000317*         RESP       (WS-RESPONSE)
000318*    END-EXEC
           MOVE LENGTH OF
            NSAS-EXTRACT-RECORD
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002174' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032313734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NSAS-EXTRACT-RECORD, 
                 DFHEIV11, 
                 WS-NSASEXTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000319
000320     IF NOT RESP-NORMAL
000321        DISPLAY ' BAD READ ON 1 NSASEXTR ' WS-RESPONSE
000322     ELSE
000323        
      * EXEC CICS DELETE
000324*          DATASET    ('NSASEXTR')
000325*          RESP       (WS-RESPONSE)
000326*       END-EXEC
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&(                    &  N#00002185' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303032313835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-NSASEXTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000327        IF NOT RESP-NORMAL
000328           DISPLAY ' BAD DELETE ON 1 NSASEXTR ' WS-RESPONSE
000329        END-IF
000330     END-IF
000331
000332     MOVE 2                      TO WS-EXTR-SEQ-NO
000333     
      * EXEC CICS READ
000334*         DATASET    ('NSASEXTR')
000335*         INTO       (NSAS-EXTRACT-RECORD)
000336*         RIDFLD     (WS-NSASEXTR-KEY)
000337*         UPDATE
000338*         RESP       (WS-RESPONSE)
000339*    END-EXEC
           MOVE LENGTH OF
            NSAS-EXTRACT-RECORD
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002195' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032313935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NSAS-EXTRACT-RECORD, 
                 DFHEIV11, 
                 WS-NSASEXTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000340
000341     IF NOT RESP-NORMAL
000342        DISPLAY ' BAD READ ON 2 NSASEXTR ' WS-RESPONSE
000343     ELSE
000344        
      * EXEC CICS DELETE
000345*          DATASET    ('NSASEXTR')
000346*          RESP       (WS-RESPONSE)
000347*       END-EXEC
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&(                    &  N#00002206' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'204E233030303032323036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-NSASEXTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000348        IF NOT RESP-NORMAL
000349           DISPLAY ' BAD DELETE ON 2 NSASEXTR ' WS-RESPONSE
000350        END-IF
000351     END-IF
000352
000353     .
000354 0050-EXIT.
000355     EXIT.
000356
000357
000358 0440-ADD-BILLING-NOTE.
000359
000360     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
000361     MOVE WS-ERARCH-COMPANY-CD   TO WS-EOBC-COMPANY-CD
000362     MOVE '5'                    TO WS-EOBC-REC-TYPE
000363
000364     
      * EXEC CICS STARTBR
000365*        DATASET   ('ELEOBC')
000366*        RIDFLD    (WS-ELEOBC-KEY)
000367*        GTEQ
000368*        RESP      (WS-RESPONSE)
000369*    END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002226' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303032323236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000370
000371     IF NOT RESP-NORMAL
000372        GO TO 0440-EXIT
000373     END-IF
000374      .
000375 0440-READNEXT-ELEOBC.
000376
000377     
      * EXEC CICS READNEXT
000378*       INTO    (EOB-CODES)
000379*       DATASET ('ELEOBC')
000380*       RIDFLD  (WS-ELEOBC-KEY)
000381*       RESP    (WS-RESPONSE)
000382*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002239' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303032323339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000383
000384     IF RESP-NORMAL
000385         IF EO-RECORD-TYPE NOT = '5'
000386             GO TO 0440-EXIT
000387         END-IF
000388     ELSE
000389         GO TO 0440-EXIT
000390     END-IF
000391
000392     IF EO-RECORD-TYPE = '5' AND
000393        EO-EOB-CODE = WS-SV-LETTER-ID
000394           CONTINUE
000395     ELSE
000396         GO TO 0440-READNEXT-ELEOBC
000397     END-IF
000398
000399     MOVE SPACES         TO WS-BILLING-NOTE
000400     MOVE EO-DESCRIPTION TO WS-BN-NOTE
000401     MOVE WS-SV-LETTER-ID TO WS-BN-LTRID
000402     MOVE SAVE-DATE      TO WS-BN-DATE
000403     MOVE WS-SV-PROC-ID  TO WS-BN-USERID
000404     MOVE +25            TO WS-LEN
000405
000406     PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
000407     .
000408 0440-EXIT.
000409     EXIT.
000410
000411 0441-UPDATE-BILLING-NOTE.
000412
000413     move bl-batch-no            to ws-erpndb-batch-no
000414     move bl-batch-seq           to ws-erpndb-batch-seq-no
000415     move +0                     to ws-erpndb-chg-seq-no
000416     
      * exec cics read
000417*       dataset   ('ERPNDB')
000418*       ridfld    (ws-erpndb-key)
000419*       into      (pending-business)
000420*       resp      (ws-response)
000421*    end-exec
           MOVE LENGTH OF
            pending-business
             TO DFHEIV11
           MOVE 'ERPNDB' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002278' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303032323738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 pending-business, 
                 DFHEIV11, 
                 ws-erpndb-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000422     if resp-normal and pb-cancellation
000423        move '2'                 to ws-from-issue-or-cancel
000424*       display ' must be from cancel nsaasbl '
000425     else
000426        move '1'                 to ws-from-issue-or-cancel
000427     end-if
000428
000429     MOVE WS-SAVE-KEY            TO WS-ERNOTE-KEY
000430     move ws-from-issue-or-cancel to ws-note-record-type
000431
000432*    display ' rid fle b4 ' ws-ernote-key(2:33)
000433
000434     
      * EXEC CICS READ
000435*       DATASET    ('ERNOTE')
000436*       RIDFLD     (WS-ERNOTE-KEY)
000437*       INTO       (CERTIFICATE-NOTE)
000438*       RESP       (WS-RESPONSE)
000439*       UPDATE
000440*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002296' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032323936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 WS-ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000441
000442     IF RESP-NORMAL
000443       IF CN-BILLING-START-LINE-NO NOT NUMERIC
000444          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
000445       END-IF
000446       IF CN-BILLING-END-LINE-NO NOT NUMERIC
000447          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
000448       END-IF
000449       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
000450           (NOTE-SUB > +10) OR
000451           (CN-LINE (NOTE-SUB) (1:WS-LEN) =
000452                             WS-BILLING-NOTE (1:WS-LEN))
000453       END-PERFORM
000454       IF CN-LINE (NOTE-SUB) (1:WS-LEN) =
000455                              WS-BILLING-NOTE (1:WS-LEN)
000456         
      * EXEC CICS UNLOCK
000457*           DATASET    ('ERNOTE')
000458*        END-EXEC
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&*                    #   #00002318' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032333138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000459       ELSE
000460         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
000461           (NOTE-SUB > +10) OR
000462           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES)
000463         END-PERFORM
000464         IF (NOTE-SUB < +11)
000465           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
000466              NOTE-SUB <= CN-BILLING-END-LINE-NO
000467                MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
000468           ELSE
000469             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
000470              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
000471                MOVE WS-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
000472                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
000473             ELSE
000474               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
000475                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
000476                     MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
000477                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
000478               ELSE
000479                 IF (CN-BILLING-END-LINE-NO = ZEROS)
000480                   MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
000481                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
000482                                       CN-BILLING-START-LINE-NO
000483                 ELSE
000484                    PERFORM 0442-SQUEEZE-IT-IN THRU 0442-EXIT
000485                 END-IF
000486               END-IF
000487             END-IF
000488           END-IF
000489           MOVE WS-SV-PROC-ID       TO CN-LAST-MAINT-USER
000490           MOVE SAVE-BIN-DATE       TO CN-LAST-MAINT-DT
000491           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
000492           
      * EXEC CICS REWRITE
000493*             DATASET    ('ERNOTE')
000494*             FROM       (CERTIFICATE-NOTE)
000495*             RESP       (WS-RESPONSE)
000496*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002354' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303032333534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000497           PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
000498         END-IF
000499       END-IF
000500     ELSE
000501        MOVE SPACES              TO CERTIFICATE-NOTE
000502        MOVE 'CN'                TO CN-RECORD-ID
000503        MOVE WS-SAVE-KEY         TO CN-CONTROL-PRIMARY
000504                                    WS-ERNOTE-KEY
000505
000506        move ws-from-issue-or-cancel
000507                                 to cn-record-type
000508                                    ws-note-record-type
000509
000510        MOVE 01                  TO CN-BILLING-START-LINE-NO
000511                                    CN-BILLING-END-LINE-NO
000512        MOVE WS-BILLING-NOTE     TO CN-LINE (01)
000513        MOVE WS-SV-PROC-ID       TO CN-LAST-MAINT-USER
000514        MOVE SAVE-BIN-DATE       TO CN-LAST-MAINT-DT
000515        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
000516        
      * EXEC CICS WRITE
000517*          DATASET    ('ERNOTE')
000518*          FROM       (CERTIFICATE-NOTE)
000519*          RIDFLD     (cn-control-primary)
000520*          RESP       (WS-RESPONSE)
000521*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00002378' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303032333738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 cn-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000522
000523        PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
000524     END-IF
000525
000526     .
000527 0441-EXIT.
000528     EXIT.
000529
000530
000531 0442-SQUEEZE-IT-IN.
000532
000533     IF NOTE-SUB < CN-BILLING-START-LINE-NO
000534        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
000535           NOTE-SUB = +10
000536           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
000537           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
000538             MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
000539             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
000540             MOVE +9 TO NOTE-SUB
000541           END-IF
000542        END-PERFORM
000543     ELSE
000544        IF NOTE-SUB > CN-BILLING-END-LINE-NO
000545           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1
000546             UNTIL NOTE-SUB = +1
000547             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
000548             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
000549                MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
000550                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
000551                MOVE +2          TO NOTE-SUB
000552             END-IF
000553           END-PERFORM
000554        END-IF
000555     END-IF
000556
000557     .
000558 0442-EXIT.
000559     EXIT.
000560
000561 0445-CERTIFICATE-UPDATE.
000562
000563     MOVE WS-SAVE-KEY     TO WS-ELCERT-KEY
000564     
      * EXEC CICS READ
000565*        DATASET  ('ELCERT')
000566*        RIDFLD   (WS-ELCERT-KEY)
000567*        INTO     (CERTIFICATE-MASTER)
000568*        RESP     (WS-RESPONSE)
000569*        UPDATE
000570*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002426' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303032343236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 WS-ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000571
000572     IF RESP-NORMAL
000573        EVALUATE CM-NOTE-SW
000574           WHEN '2'
000575           WHEN '3'
000576           WHEN '6'
000577           WHEN '7'
000578              SET NO-CERT-RW     TO TRUE
000579           WHEN ' '
000580              MOVE '2'           TO CM-NOTE-SW
000581           WHEN '1'
000582              MOVE '3'           TO CM-NOTE-SW
000583           WHEN '4'
000584              MOVE '6'           TO CM-NOTE-SW
000585           WHEN '5'
000586              MOVE '7'           TO CM-NOTE-SW
000587        END-EVALUATE
000588     END-IF
000589
000590     IF NOT NO-CERT-RW
000591        
      * EXEC CICS REWRITE
000592*          FROM     (CERTIFICATE-MASTER)
000593*          DATASET  ('ELCERT')
000594*          RESP     (WS-RESPONSE)
000595*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002453' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303032343533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000596     ELSE
000597        
      * EXEC CICS UNLOCK
000598*          DATASET  ('ELCERT')
000599*       END-EXEC
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&*                    #   #00002459' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032343539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000600     END-IF
000601
000602     .
000603 0445-EXIT.
000604     EXIT.
000605
000606
000607 9700-DATE-LINK.
000608
000609     
      * EXEC CICS LINK
000610*        PROGRAM   ('ELDATCV')
000611*        COMMAREA  (DATE-CONVERSION-DATA)
000612*        LENGTH    (DC-COMM-LENGTH)
000613*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002471' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032343731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000614
000615
000616 9700-EXIT.
000617      EXIT.
000618
000619

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSAASBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSAASBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
