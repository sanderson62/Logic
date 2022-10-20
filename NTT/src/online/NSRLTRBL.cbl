      *((program: NSRLTRBL.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. NSRLTRBL.
000004
000005*AUTHOR.     PABLO
000006*            COLLEYVILLE, TEXAS.
000007
000008*REMARKS.    EXECUTED FROM NSREQLTR
000009
000010******************************************************************
000011*                   C H A N G E   L O G
000012*
000013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000014*-----------------------------------------------------------------
000015*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000016* EFFECTIVE    NUMBER
000017*-----------------------------------------------------------------
000018* 121802    2009122800001  PEMA  NEW PROGRAM
000019* 022212    2011120900003  AJRA  ADD AHL
000020* 031912    2011120900003  AJRA  ADD AHL CLAIM NO TO EXTRACT
000021* 020513    2011090100001  PEMA  CORRECT ISS WITH MULT &
000022* 020613    2012110800002  AJRA  ADD BUS TYPE (GPCD) TO NAPERSOFT
000023* 031116    2015110400001  TANA  ADD EL150D FIELDS
000024* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000025* 031319  IR2019031100002  TANA  Correct iss w/ letters with ???
000026* 071719    2019011600010  TANA  ADD VERIFICATION CODE
000027* 061421  CR2017031500001  PEMA  Update to CCM8
000028******************************************************************
000029 ENVIRONMENT DIVISION.
000030
000031 DATA DIVISION.
000032
000033 working-storage section.
       01  DFH-START PIC X(04).
000034
000035 77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000036 77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
000037 77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000038 77  S1                          PIC S999 COMP-3 VALUE +0.
000039 77  S2                          PIC S999 COMP-3 VALUE +0.
000040 77  S3                          PIC S999 COMP-3 VALUE +0.
000041 77  E1                          PIC S999 COMP-3 VALUE +0.
000042 77  A1                          PIC S9(5) COMP-3 VALUE +0.
000043 77  M1                          PIC S999 COMP-3 VALUE +0.
000044 77  P1                          PIC S999 COMP-3 VALUE +0.
000045 77  WS-HOLD-KEY                 PIC X(20).
000046 77  WS-HOLD-S1                  PIC S999 COMP-3 VALUE +0.
000047 77  WS-WORK-FIELD               PIC X(80)    VALUE SPACES.
000048 77  WS-ARCHIVE-NO               PIC S9(8)  COMP VALUE +0.
000049 77  WS-FOLLOW-UP-DT             PIC XX  VALUE LOW-VALUES.
000050 77  WS-RESEND-DT                PIC XX  VALUE LOW-VALUES.
000051 77  WS-AUTO-LAST-SCHED-DT       PIC XX  VALUE LOW-VALUES.
000052 77  WS-LAST-ACT-DT              PIC XX  VALUE LOW-VALUES.
000053 77  WS-ACTIVITY-DT              PIC XX  VALUE LOW-VALUES.
000054 77  WS-FORM                     PIC XXXX  VALUE SPACES.
000055 77  WS-LAST-ACT-TYPE            PIC X   VALUE ' '.
000056 77  WS-FOUND-BENE-SW            PIC X   VALUE ' '.
000057     88  FOUND-BENE                  VALUE 'Y'.
000058 77  WS-TALLY                    PIC S999 COMP-3 VALUE +0.
000059 77  WS-TALLY1                   PIC S999 COMP-3 VALUE +0.
000060 77  NS-LEN                      PIC S9(5) COMP-3 VALUE +0.
000061 77  WS-CNTR                     pic s999 comp-3 value +0.
000062 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
000063 77  WS-ACCUM-DAYS               PIC S9(5) COMP-3 VALUE +0.
000064 77  WS-ACCUM-AMT                PIC S9(9)V99 COMP-3 VALUE +0.
000065 77  WS-ACCUM-PD-BENS            PIC S999 COMP-3 VALUE +0.
000066 77  WS-PREV-CLM-TYPE            PIC X   VALUE ' '.
000067 77  WS-PREV-INS-TYPE            PIC X   VALUE ' '.
000068 77  WS-PREV-BEN-PER             PIC 99 VALUE ZEROS.
000069 77  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
000070     88  PDEF-FOUND                   VALUE 'Y'.
000071 77  wk1                         pic 999 value zeros.
000072 77  wk2                         pic 999 value zeros.
000073 77  WS-WORK-BEN-PCT             PIC S9V999 COMP-3 VALUE +0.
000074 77  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.
000075
000076 01  OUTPUT-SCREEN-WORK-AREA.
000077     05  OS-PREV-KEY.
000078         10  OS-PREV-CLM-TYPE        PIC X.
000079         10  OS-PREV-INS-TYPE        PIC X.
000080     05  WS-PD-BENS                  PIC 999 VALUE ZEROS.
000081     05  WS-COV-REM-BENS             PIC S999 VALUE ZEROS.
000082
000083
000084 01  WS-XML-WORK                 PIC X(2500)  VALUE SPACES.
000085 01 response-code         pic s9(8) comp.
000086 01 display-response      pic 9(8).
000087 01 bl-index              pic 9(8) comp.
000088 01 max-last-name         pic x(18).
000089 01 first-initial         pic x.
000090 01 name-in-range-flag    pic 9.
000091 01 max-entries           pic s9(8) comp value 100.
000092
000093 01 lower-case    pic x(26) value
000094            "abcdefghijklmnopqrstuvwxyz".
000095 01 upper-case    pic x(26) value
000096            "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
000097
000098 01  W-Z-CONTROL-DATA.
000099     05  W-NUMBER-OF-COPIES  PIC  9(01).
000100     05  FILLER              PIC  X(01).
000101     05  W-DAYS-TO-FOLLOW-UP PIC  9(03).
000102     05  FILLER              PIC  X(01).
000103     05  W-DAYS-TO-RESEND-1  PIC  9(03).
000104     05  FILLER              PIC  X(01).
000105     05  W-FORM-TO-RESEND    PIC  X(04).
000106     05  FILLER              PIC  X(01).
000107     05  W-PROMPT-LETTER     PIC  X(01).
000108     05  FILLER              PIC  X(01).
000109     05  W-ENCLOSURE-CD      PIC  X(03).
000110     05  FILLER              PIC  X(1).
000111     05  W-AUTO-CLOSE-IND    PIC  X(1).
000112     05  FILLER              PIC  X(1).
000113     05  W-LETTER-TO-BENE    PIC  X(1).
000114
000115 01  MISC.
000116     12  WS-RESPONSE             PIC S9(8)   COMP.
000117         88  RESP-NORMAL                  VALUE +00.
000118         88  RESP-NOTFND                  VALUE +13.
000119         88  RESP-DUPREC                  VALUE +14.
000120         88  RESP-DUPKEY                  VALUE +15.
000121         88  RESP-NOTOPEN                 VALUE +19.
000122         88  RESP-ENDFILE                 VALUE +20.
000123
000124 01  WS-ELENCC-KEY.
000125     05  WS-ELENCC-COMPANY-CD    PIC X.
000126     05  WS-ELENCC-REC-TYPE      PIC X.
000127     05  WS-ELENCC-ENC-CODE      PIC X(5).
000128     05  F                       PIC X(09).
000129
000130 01  WS-ELMSTR-KEY.
000131     05  WS-ELMSTR-COMPANY-CD    PIC X.
000132     05  WS-ELMSTR-CARRIER       PIC X.
000133     05  WS-ELMSTR-CLAIM-NO      PIC X(7).
000134     05  WS-ELMSTR-CERT-NO       PIC X(11).
000135 01  WS-ELTRLR-KEY.
000136     05  WS-ELTRLR-COMPANY-CD    PIC X.
000137     05  WS-ELTRLR-CARRIER       PIC X.
000138     05  WS-ELTRLR-CLAIM-NO      PIC X(7).
000139     05  WS-ELTRLR-CERT-NO       PIC X(11).
000140     05  WS-ELTRLR-SEQ-NO        PIC S9(4) COMP VALUE +0.
000141 01  WS-ELCERT-KEY.
000142     05  WS-ELCERT-COMPANY-CD    PIC X.
000143     05  WS-ELCERT-CARRIER       PIC X.
000144     05  WS-ELCERT-GROUP         PIC X(6).
000145     05  WS-ELCERT-STATE         PIC XX.
000146     05  WS-ELCERT-ACCOUNT       PIC X(10).
000147     05  WS-ELCERT-EFF-DT        PIC XX.
000148     05  WS-ELCERT-CERT-NO       PIC X(11).
000149 01  WS-ERACCT-KEY.
000150     05  WS-ERACCT-COMPANY-CD    PIC X.
000151     05  WS-ERACCT-CARRIER       PIC X.
000152     05  WS-ERACCT-GROUP         PIC X(6).
000153     05  WS-ERACCT-STATE         PIC XX.
000154     05  WS-ERACCT-ACCOUNT       PIC X(10).
000155     05  WS-ERACCT-EXP-DT        PIC XX.
000156     05  FILLER                  PIC XXXX.
000157 01  WS-ELBENE-KEY.
000158     05  WS-ELBENE-COMPANY-CD    PIC X.
000159     05  WS-ELBENE-REC-TYPE      PIC X.
000160     05  WS-ELBENE-BENE          PIC X(10).
000161 01  WS-ELLETR-KEY.
000162     05  WS-ELLETR-COMPANY-CD    PIC X.
000163     05  WS-ELLETR-LETTER-ID     PIC X(12).
000164     05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
000165 01  WS-ELCNTL-KEY.
000166     05  WS-ELCNTL-COMPANY-ID    PIC XXX.
000167     05  WS-ELCNTL-REC-TYPE      PIC X.
000168     05  WS-ELCNTL-GENL.
000169         10  FILLER              PIC XX  VALUE SPACES.
000170         10  WS-ELCNTL-BEN-CD    PIC XX.
000171     05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.
000172 01  ACCESS-KEYS.
000173     12  ELCRTT-KEY.
000174         16  CRTT-COMP-CD            PIC X.
000175         16  CRTT-CARRIER            PIC X.
000176         16  CRTT-GROUPING           PIC X(6).
000177         16  CRTT-STATE              PIC XX.
000178         16  CRTT-ACCOUNT            PIC X(10).
000179         16  CRTT-EFF-DT             PIC XX.
000180         16  CRTT-CERT-NO            PIC X(11).
000181         16  CRTT-REC-TYPE           PIC X.
000182
000183 01  ERPDEF-KEY-SAVE             PIC X(18).
000184 01  ERPDEF-KEY.
000185     12  ERPDEF-COMPANY-CD       PIC X.
000186     12  ERPDEF-STATE            PIC XX.
000187     12  ERPDEF-PROD-CD          PIC XXX.
000188     12  F                       PIC X(7).
000189     12  ERPDEF-BEN-TYPE         PIC X.
000190     12  ERPDEF-BEN-CODE         PIC XX.
000191     12  ERPDEF-EXP-DT           PIC XX.
000192 01  TEXT-WORK-AREAS.
000193     05  WS-COV-TYPE             PIC X(4) VALUE SPACES.
000194
000195 01  WS-UNSORTED-TABLE.
000196     12  WS-UNSRTD-TABLE   OCCURS 25 TIMES.
000197         16  WS-KEY.
000198             20  WS-CLM-TYPE     PIC X.
000199             20  WS-INS-TYPE     PIC X.
000200             20  WS-BEN-PER      PIC 99.
000201             20  WS-INC-DT       PIC XX.
000202         16  WS-EXCL-PER         PIC 999.
000203         16  WS-COV-ENDS         PIC 999.
000204         16  WS-ACC-PER          PIC 999.
000205         16  WS-MAX-BENS         PIC 999.
000206         16  WS-REC-MOS          PIC 99.
000207         16  WS-MAX-EXTEN        PIC 99.
000208         16  WS-STATUS           PIC X.
000209         16  WS-PD-THRU-DT       PIC XX.
000210         16  WS-CLAIM-NO         PIC X(7).
000211         16  WS-MAX-MOBEN        PIC S9(7)V99 COMP-3.
000212         16  WS-TOTAL-PAID       PIC S9(7)V99 COMP-3.
000213         16  WS-REM-BENS         PIC 999.
000214         16  WS-SORTED-SW        PIC X.
000215
000216 01  WS-SORTED-TABLE.
000217     12  WS-SRTD-TABLE OCCURS 25 TIMES.
000218         16  WS-SRTD-KEY.
000219             20  WS-SRTD-CLM-TYPE PIC X.
000220             20  WS-SRTD-INS-TYPE PIC X.
000221             20  WS-SRTD-BEN-PER PIC 99.
000222             20  WS-SRTD-INC-DT  PIC XX.
000223         16  WS-SRTD-EXCL-PER    PIC 999.
000224         16  WS-SRTD-COV-ENDS    PIC 999.
000225         16  WS-SRTD-ACC-PER     PIC 999.
000226         16  WS-SRTD-MAX-BENS    PIC 999.
000227         16  WS-SRTD-REC-MOS     PIC 99.
000228         16  WS-SRTD-MAX-EXTEN   PIC 99.
000229         16  WS-SRTD-STATUS      PIC X.
000230         16  WS-SRTD-PD-THRU-DT  PIC XX.
000231         16  WS-SRTD-CLAIM-NO    PIC X(7).
000232         16  WS-SRTD-MAX-MOBEN   PIC S9(7)V99 COMP-3.
000233         16  WS-SRTD-TOTAL-PAID  PIC S9(7)V99 COMP-3.
000234         16  WS-SRTD-REM-BENS    PIC 999.
000235         16  WS-SRTD-SW          PIC X.
000236
000237 01  SAVE-ACCOUNT-MASTER         PIC X(2000) VALUE SPACES.
000238
000239*                                COPY ELCDATE.
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
000240*                                COPY ELCMSTR.
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
000241*                                COPY ELCTRLR.
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
000242*                                COPY ELCCERT.
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
000243*                                COPY ERCACCT.
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
000244*                                COPY ELCBENE.
      *>>((file: ELCBENE))
000001******************************************************************
000002*                                                                *
000003*                            ELCBENE.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.006                          *
000006*                                                                *
000007*   FILE DESCRIPTION = BENEFICIARY FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 500   RECFORM = FIX                            *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
000013*     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 013017  CR2016053100001  PEMA  ACH PROCESSING
000028* 082317  CR2017082100003  PEMA  Add sub type
000029* 032019  CR2019011400002  PEMA  Add email address for ach report
000030******************************************************************
000031
000032 01  BENEFICIARY-MASTER.
000033     12  BE-RECORD-ID                PIC XX.
000034         88  VALID-BE-ID                VALUE 'BE'.
000035
000036     12  BE-CONTROL-PRIMARY.
000037         16  BE-COMPANY-CD           PIC X.
000038         16  BE-RECORD-TYPE          PIC X.
000039             88  BENEFICIARY-RECORD  VALUE 'B'.
000040             88  ADJUSTOR-RECORD     VALUE 'A'.
000041         16  BE-BENEFICIARY          PIC X(10).
000042     12  BE-CONTROL-BY-NAME.
000043         16  BE-COMPANY-CD-A1        PIC X.
000044         16  BE-RECORD-TYPE-A1       PIC X.
000045         16  BE-MAIL-TO-NAME-A1      PIC X(30).
000046         16  BE-ALTERNATE-PRIME-A1   PIC X(10).
000047
000048     12  BE-LAST-MAINT-DT            PIC XX.
000049     12  BE-LAST-MAINT-BY            PIC X(4).
000050     12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
000051
000052     12  BE-ADDRESS-DATA.
000053         16  BE-MAIL-TO-NAME         PIC X(30).
000054         16  BE-ADDRESS-LINE-1       PIC X(30).
000055         16  BE-ADDRESS-LINE-2       PIC X(30).
000056         16  BE-ADDRESS-LINE-3       PIC X(30).
000057         16  BE-CITY-STATE.
000058             20  BE-CITY             PIC X(28).
000059             20  BE-STATE            PIC XX.
000060         16  BE-ZIP-CODE.
000061             20  BE-ZIP-PRIME.
000062                 24  BE-ZIP-1ST      PIC X.
000063                     88  BE-CANADIAN-POST-CODE
000064                                         VALUE 'A' THRU 'Z'.
000065                 24  FILLER          PIC X(4).
000066             20  BE-ZIP-PLUS4        PIC X(4).
000067         16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
000068             20  BE-CAN-POSTAL-1     PIC XXX.
000069             20  BE-CAN-POSTAL-2     PIC XXX.
000070             20  FILLER              PIC XXX.
000071         16  BE-PHONE-NO             PIC 9(11)     COMP-3.
000072         16  BE-GROUP-CHECKS-Y-N     PIC X.
000073
000074******************************************************************
000075*    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
000076*    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
000077******************************************************************
000078     12  BE-CARRIER                  PIC X.
000079
000080     12  BE-ADDRESS-DATA2.
000081         16  BE-MAIL-TO-NAME2        PIC X(30).
000082         16  BE-ADDRESS-LINE-12      PIC X(30).
000083         16  BE-ADDRESS-LINE-22      PIC X(30).
000084         16  BE-ADDRESS-LINE-32      PIC X(30).
000085         16  BE-CITY-STATE2.
000086             20  BE-CITY2            PIC X(28).
000087             20  BE-STATE2           PIC XX.
000088         16  BE-ZIP-CODE2.
000089             20  BE-ZIP-PRIME2.
000090                 24  BE-ZIP-1ST2     PIC X.
000091                     88  BE-CANADIAN-POST-CODE2
000092                                         VALUE 'A' THRU 'Z'.
000093                 24  FILLER          PIC X(4).
000094             20  BE-ZIP-PLUS42       PIC X(4).
000095         16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
000096             20  BE-CAN-POSTAL-12    PIC XXX.
000097             20  BE-CAN-POSTAL-22    PIC XXX.
000098             20  FILLER              PIC XXX.
000099         16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
000100         16  BE-ACH-DATA.
000101             20  BE-ACH-YES-OR-NO    PIC X.
000102                 88  BE-ON-ACH       VALUE 'Y'.
000103                 88  BE-NOT-ON-ACH   VALUE 'N' ' '.
000104             20  BE-ACH-ABA-ROUTING-NUMBER
000105                                     PIC X(15).
000106             20  BE-ACH-BANK-ACCOUNT-NUMBER
000107                                     PIC X(20).
000108             20  BE-ACH-SUB-TYPE     PIC XX.
000109             20  BE-ACH-EMAIL-YN     PIC X.
000110                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
000111             20  be-ach-email-addr   PIC X(40).
000112         16  BE-BILLING-STMT-DATA.
000113*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
000114             20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
000115             20  BE-OUTPUT-TYPE      PIC X.
000116                 88  BE-FAX-OUTPUT         VALUE 'F'.
000117                 88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
000118
000119     12  filler                      PIC X(16).
000120******************************************************************
      *<<((file: ELCBENE))
000245*                                COPY ELCTEXT.
      *>>((file: ELCTEXT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCTEXT.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.008                          *
000007*                                                                *
000008*   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
000009*                                     FORM LETTERS,              *
000010*                                     CERT FORM DISPLAY.
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 100   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
000016*       ALTERNATE INDEX = NONE                                   *
000017*                                                                *
000018*   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
000019*       ALTERNATE INDEX = NONE                                   *
000020*                                                                *
000021*   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
000022*       ALTERNATE INDEX = NONE                                   *
000023*                                                                *
000024*   LOG = NO                                                     *
000025*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000026******************************************************************
000027 01  TEXT-FILES.
000028     12  TEXT-FILE-ID                PIC XX.
000029         88  FORMS-FILE-TEXT            VALUE 'TF'.
000030         88  LETTER-FILE-TEXT           VALUE 'TL'.
000031         88  HELP-FILE-TEXT             VALUE 'TH'.
000032
000033     12  TX-CONTROL-PRIMARY.
000034         16  TX-COMPANY-CD           PIC X.
000035             88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
000036         16  TX-ACCESS-CD-GENL       PIC X(12).
000037
000038         16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000039             20  TX-LETTER-NO        PIC X(4).
000040             20  FILLER              PIC X(8).
000041
000042         16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000043             20  TX-FORM-NO          PIC X(12).
000044
000045         16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
000046             20  TX-HELP-TYPE        PIC X.
000047                 88  HELP-FOR-GENERAL   VALUE ' '.
000048                 88  HELP-BY-SCREEN     VALUE 'S'.
000049                 88  HELP-BY-ERROR      VALUE 'E'.
000050             20  TX-SCREEN-OR-ERROR  PIC X(4).
000051                 88  GENERAL-INFO-HELP  VALUE '0000'.
000052             20  TX-HELP-FOR-COMPANY  PIC XXX.
000053                 88  NOT-COMPANY-SPECIFIC VALUE '   '.
000054             20  FILLER              PIC X(4).
000055
000056         16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
000057
000058     12  TX-PROCESS-CONTROL          PIC XX.
000059         88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
000060
000061     12  TX-TEXT-LINE                PIC X(70).
000062
000063     12  TX-FORM-SQUEEZE-CONTROL     PIC X.
000064         88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
000065         88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
000066         88  TX-VALID-FORM-SQUEEZE-VALUE
000067                                        VALUE 'Y' ' '.
000068
000069     12  TX-LINE-SQUEEZE-CONTROL     PIC X.
000070         88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
000071         88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
000072         88  TX-DO-NOT-ADJUST           VALUE 'N'.
000073         88  TX-FORM-CONTROL-LINE       VALUE 'K'.
000074         88  TX-NEW-PARAGRAPH           VALUE 'P'.
000075         88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
000076         88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
000077                                              'K' 'N' ' '
000078                                              'Z'.
000079
000080     12  TX-ARCHIVE-SW               PIC X.
000081         88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
000082         88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
000083         88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
000084
000085     12  TX-LAST-MAINTENANCED-BY     PIC X(4).
000086     12  TX-LAST-MAINTENANCED-DT     PIC X(2).
000087
000088     12  TX-BSR-CODE                 PIC X.
000089         88  TX-BSR-LETTER              VALUE 'B'.
000090         88  TX-NON-BSR-LETTER          VALUE ' '.
000091
000092     12  FILLER                      PIC X.
000093*****************************************************************
      *<<((file: ELCTEXT))
000246*                                COPY ELCCNTL.
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
000247*                                COPY NSCVARS.
      *>>((file: NSCVARS))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 031912    2011110200002  AJRA  AHL CLAIM NUM
000010* 013013    2012110080002  AJRA  ADD ACCOUNT GPCD
000011* 031116    2015110400001  TANA  ADD EL150D FIELDS
000012* 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
000013* 071719    2019011600010  TANA  ADD VERIF CODE
000014******************************************************************
000015 01 NAPER-OUTPUT-DATA.
000016    05 FILLER              PIC X(6) VALUE "TEMPL=".
000017    05 OUT-TEMP            PIC X(4).
000018    05 FILLER              PIC X(8) VALUE "&LETTER=".
000019    05 OUT-LETTER          PIC X(4).
000020    05 FILLER              PIC X(6) VALUE "&CARR=".
000021    05 OUT-CARR            PIC X.
000022    05 FILLER              PIC X(7) VALUE "&CLMNO=".
000023    05 OUT-CLMNO           PIC X(7).
000024    05 FILLER              PIC X(7) VALUE "&CRTNO=".
000025    05 OUT-CRTNO           PIC X(11).
000026    05 FILLER              PIC X(7) VALUE "&ACTNO=".
000027    05 OUT-ACTNO           PIC X(10).
000028    05 FILLER              PIC X(8) VALUE "&ILNAME=".
000029    05 OUT-ILNAME          PIC X(15).
000030    05 FILLER              PIC X(8) VALUE "&IFNAME=".
000031    05 OUT-IFNAME          PIC X(10).
000032    05 FILLER              PIC X(7) VALUE "&SEXCD=".
000033    05 OUT-SEX-CD          PIC X.
000034    05 FILLER              PIC X(5) VALUE "&SSN=".
000035    05 OUT-SSN             PIC X(11).
000036    05 FILLER              PIC X(9) VALUE "&CLMSTAT=".
000037    05 OUT-CLMSTAT         PIC X.
000038    05 FILLER              PIC X(8) VALUE "&CLMTYP=".
000039    05 OUT-CLMTYPE         PIC X.
000040    05 FILLER              PIC X(7) VALUE "&ESTDT=".
000041    05 OUT-EST-DT          PIC X(21).
000042    05 FILLER              PIC X(7) VALUE "&INCDT=".
000043    05 OUT-INC-DT          PIC X(21).
000044    05 FILLER              PIC X(7) VALUE "&RPTDT=".
000045    05 OUT-RPT-DT          PIC X(21).
000046    05 FILLER              PIC X(10) VALUE "&PDTHRUDT=".
000047    05 OUT-PD-THRU-DT      PIC X(21).
000048    05 FILLER              PIC X(11) VALUE "&FORMDUEDT=".
000049    05 OUT-FORM-DUE-DT     PIC X(21).
000050    05 FILLER              PIC X(8) VALUE "&LPMTDT=".
000051    05 OUT-LST-PMT-DT      PIC X(21).
000052    05 FILLER              PIC X(8) VALUE "&NOPMTS=".
000053    05 OUT-NO-OF-PMTS      PIC X(10).
000054    05 FILLER              PIC X(9) VALUE "&TOTPAID=".
000055    05 OUT-TOT-PAID        PIC 9999999.99.
000056    05 FILLER              PIC X(8) VALUE "&LPDAMT=".
000057    05 OUT-LST-PD-AMT      PIC 9999999.99.
000058    05 FILLER              PIC X(9) VALUE "&ADDRCNT=".
000059    05 OUT-ADDR-CNT        PIC 9.
000060    05 FILLER              PIC X(7) VALUE "&CRTST=".
000061    05 OUT-CRT-ST          PIC XX.
000062    05 FILLER              PIC X(10) VALUE "&CERTACCT=".
000063    05 OUT-CERTACCT        PIC X(10).
000064    05 FILLER              PIC X(7) VALUE "&EFFDT=".
000065    05 OUT-EFF-DT          PIC X(21).
000066    05 FILLER              PIC X(8) VALUE "&LMNTDT=".
000067    05 OUT-LST-MAINT-DT    PIC X(21).
000068    05 FILLER              PIC X(10) VALUE "&LMNTUSER=".
000069    05 OUT-LST-MAINT-USER  PIC X(10).
000070    05 FILLER              PIC X(10) VALUE "&LMNTTYPE=".
000071    05 OUT-LST-MAINT-TYPE  PIC X(10).
000072    05 FILLER              PIC X(6) VALUE "&DIAG=".
000073    05 OUT-DIAG            PIC X(66).
000074    05 FILLER              PIC X(7) VALUE "&EXPDT=".
000075    05 OUT-EXP-DT          PIC X(21).
000076    05 FILLER              PIC X(12) VALUE "&LCLSREASON=".
000077    05 OUT-LST-CLS-REA     PIC X(10).
000078    05 FILLER              PIC X(8) VALUE "&BRTHDT=".
000079    05 OUT-BIRTH-DT        PIC X(21).
000080    05 FILLER              PIC X(6) VALUE "&TERM=".
000081    05 OUT-TERM            PIC 999.
000082    05 FILLER              PIC X(7) VALUE "&BENCD=".
000083    05 OUT-BEN-CD          PIC XX.
000084    05 FILLER              PIC X(8) VALUE "&BENAMT=".
000085    05 OUT-BENAMT          PIC 9999999.99.
000086    05 FILLER              PIC X(9) VALUE "&CRITPER=".
000087    05 OUT-CRIT-PER        PIC 999.
000088    05 FILLER              PIC X(9) VALUE "&WAITPER=".
000089    05 OUT-WAIT-PER        PIC XX.
000090    05 FILLER              PIC X(11)  VALUE "&LFINTRATE=".
000091    05 OUT-LF-INT-RATE     PIC 99.99999.
000092    05 FILLER              PIC X(10) VALUE "&AUTOPYDT=".
000093    05 OUT-AUTO-PY-DT      PIC X(21).
000094    05 FILLER              PIC X(12) VALUE "&AUTOSCHEND=".
000095    05 OUT-AUTO-SCHD-END   PIC X(21).
000096    05 FILLER              PIC X(8) VALUE "&LOANNO=".
000097    05 OUT-LOAN-NO         PIC X(25).
000098    05 FILLER              PIC X(5) VALUE "&APR=".
000099    05 OUT-APR             PIC 99.99999.
000100    05 FILLER              PIC X(8) VALUE "&LACTDT=".
000101    05 OUT-LST-ACT-DT      PIC X(21).
000102    05 FILLER              PIC X(7) VALUE "&ACTDT=".
000103    05 OUT-ACT-DT          PIC X(21).
000104    05 FILLER              PIC X(10) VALUE "&LACTTYPE=".
000105    05 OUT-LST-ACT-TYPE    PIC X(15).
000106    05 FILLER              PIC X(9) VALUE "&ACTTYPE=".
000107    05 OUT-ACT-TYPE        PIC X(15).
000108    05 FILLER              PIC X(6) VALUE "&FORM=".
000109    05 OUT-FORM            PIC X(4).
000110    05 FILLER              PIC X(9) VALUE "&INSNAME=".
000111    05 OUT-INS-NAME        PIC X(36).
000112    05 FILLER              PIC X(10) VALUE "&INSADDR1=".
000113    05 OUT-INS-ADDR1       PIC X(36).
000114    05 FILLER              PIC X(10) VALUE "&INSADDR2=".
000115    05 OUT-INS-ADDR2       PIC X(36).
000116    05 FILLER              PIC X(9) VALUE "&INSCITY=".
000117    05 OUT-INS-CITY        PIC X(28).
000118    05 FILLER              PIC X(10) VALUE "&INSSTATE=".
000119    05 OUT-INS-STATE       PIC XX.
000120    05 FILLER              PIC X(8) VALUE "&INSZIP=".
000121    05 OUT-INS-ZIP         PIC X(10).
000122    05 FILLER              PIC X(10) VALUE "&INSPHONE=".
000123    05 OUT-INS-PHONE       PIC X(13).
000124    05 FILLER              PIC X(10) VALUE "&BENENAME=".
000125    05 OUT-BEN-NAME        PIC X(32).
000126    05 FILLER              PIC X(11) VALUE "&BENEADDR1=".
000127    05 OUT-BEN-ADDR1       PIC X(36).
000128    05 FILLER              PIC X(11) VALUE "&BENEADDR2=".
000129    05 OUT-BEN-ADDR2       PIC X(32).
000130    05 FILLER              PIC X(10) VALUE "&BENECITY=".
000131    05 OUT-BEN-CITY        PIC X(28).
000132    05 FILLER              PIC X(11) VALUE "&BENESTATE=".
000133    05 OUT-BEN-STATE       PIC XX.
000134    05 FILLER              PIC X(9) VALUE "&BENEZIP=".
000135    05 OUT-BEN-ZIP         PIC X(10).
000136    05 FILLER              PIC X(11) VALUE "&BENEPHONE=".
000137    05 OUT-BEN-PHONE       PIC X(13).
000138    05 FILLER              PIC X(8) VALUE "&OANAME=".
000139    05 OUT-ORIG-NAME       PIC X(32).
000140    05 FILLER              PIC X(10) VALUE "&ACCTNAME=".
000141    05 OUT-ACCT-NAME       PIC X(32).
000142    05 FILLER              PIC X(11) VALUE "&ACCTADDR1=".
000143    05 OUT-ACCT-ADDR1      PIC X(36).
000144    05 FILLER              PIC X(11) VALUE "&ACCTADDR2=".
000145    05 OUT-ACCT-ADDR2      PIC X(32).
000146    05 FILLER              PIC X(10) VALUE "&ACCTCITY=".
000147    05 OUT-ACCT-CITY       PIC X(30).
000148    05 FILLER              PIC X(11) VALUE "&ACCTSTATE=".
000149    05 OUT-ACCT-STATE      PIC XX.
000150    05 FILLER              PIC X(9)  VALUE "&ACCTZIP=".
000151    05 OUT-ACCT-ZIP        PIC X(10).
000152    05 FILLER              PIC X(11) VALUE "&ACCTPHONE=".
000153    05 OUT-ACCT-PHONE      PIC X(13).
000154    05 FILLER              PIC X(7) VALUE "&ENCCD=".
000155    05 OUT-ENC-CODE        PIC XXX.
000156    05 FILLER              PIC X(7) VALUE "&ENCST=".
000157    05 OUT-ENC-ST          PIC XX.
000158    05 FILLER              PIC X(8) VALUE "&ARCHNO=".
000159    05 OUT-ARCHNO          PIC 999999999.
000160    05 FILLER              PIC X(11) VALUE "&FLTRPRTDT=".
000161    05 OUT-1ST-LTR-PRT-DT  PIC X(21).
000162    05 FILLER              PIC X(11) VALUE "&NEXTDUEDT=".
000163    05 OUT-NEXT-DUE-DT     PIC X(21).
000164    05 FILLER              PIC X(8) VALUE "&JLNAME=".
000165    05 OUT-JLNAME          PIC X(15).
000166    05 FILLER              PIC X(8) VALUE "&JFNAME=".
000167    05 OUT-JFNAME          PIC X(10).
000168    05 FILLER              PIC X(9)  VALUE "&ENCLINE=".
000169    05 OUT-ENCLINE         PIC X(100).
000170    05 FILLER              PIC X(9) VALUE "&ENCATTS=".
000171    05 OUT-ENCATTS         PIC X(255).
000172    05 FILLER              PIC X(10) VALUE "&OUTSTACK=".
000173    05 OUT-OUTSTACK        PIC X.
000174    05 FILLER              PIC X(8) VALUE "&PROCID=".
000175    05 OUT-PROCID          PIC XXXX.
000176    05 FILLER              PIC X(8) VALUE "&COMPID=".
000177    05 OUT-COMPID          PIC XXX.
000178    05 FILLER              PIC X(8) VALUE "&PRTNOW=".
000179    05 OUT-PRINT-NOW-SW    PIC X.
000180    05 FILLER              PIC X(11) VALUE "&CYCLEDATE=".
000181    05 OUT-CYCLE-DATE      PIC X(8).
000182    05 FILLER              PIC X(11) VALUE "&ENCSTNAME=".
000183    05 OUT-ENC-ST-NAME     PIC XX.
000184    05 FILLER              PIC X(10) VALUE "&CARRIDLU=".
000185    05 OUT-CARR-LU         PIC X.
000186    05 FILLER              PIC X(5)  VALUE "&CCN=".
000187    05 OUT-AHL-CLAIM-NO    PIC X(9).
000188    05 FILLER              PIC X(4) VALUE "&GP=".
000189    05 OUT-ACCT-GPCD       PIC 99.
000190    05 FILLER              PIC X(9) VALUE "&MAXBENS=".
000191    05 OUT-MAX-BENS        PIC 9(3).
000192    05 FILLER              PIC X(10) VALUE "&PAIDBENS=".
000193    05 OUT-PAID-BENS       PIC 9(3).
000194    05 FILLER              PIC X(9) VALUE "&REMBENS=".
000195    05 OUT-REM-BENS        PIC 9(3).
000196    05 FILLER              PIC X(9) VALUE "&EXCLPER=".
000197    05 OUT-EXCL-PER        PIC 9(3).
000198    05 FILLER              PIC X(8) VALUE "&RECMOS=".
000199    05 OUT-REC-MOS         PIC 9(2).
000200    05 FILLER              PIC X(8) VALUE "&INSTYP=".
000201    05 OUT-INS-TYP         PIC X(4).
000202    05 FILLER              PIC X(8) VALUE "&BENPER=".
000203    05 OUT-BEN-PER         PIC 9(2).
000204    05 FILLER              PIC X(7) VALUE "&ACCSW=".
000205    05 OUT-ACC-SW          PIC X.
000206    05 FILLER              PIC X(7) VALUE "&VERCD=".
000207    05 OUT-VER-CD          PIC X(5).
      *<<((file: NSCVARS))
000248 01  FILLER                      PIC X(500)  VALUE SPACES.
000249*                                COPY ELCNAPS.
      *>>((file: ELCNAPS))
000001******************************************************************
000002*                                                                *
000003*                            ELCNAPS                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*        FILE DESCRIPTION = NAPERSOFT LETTER FILE                *
000008*                                                                *
000009*        FILE TYPE= VSAM,KSDS                                    *
000010*        RECORD SIZE = 150    RECFORM = FIXED                    *
000011*                                                                *
000012*        BASE CLUSTER = ELNAPS        RKP=2,LEN=28               *
000013*                                                                *
000014*        LOG = YES                                               *
000015*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000016*                                                                *
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 033110  CR2009122800001  AJRA  NEW FILE FOR NAPERSOFT.
000026******************************************************************
000027
000028 01  NAPERSOFT-FILE.
000029     12  NA-RECORD-ID                PIC  XX.
000030         88  VALID-NA-ID                  VALUE 'NA'.
000031
000032     12  NA-CONTROL-PRIMARY.
000033         16  NA-COMPANY-CD           PIC X.
000034         16  NA-CARRIER              PIC X.
000035         16  NA-CLAIM-NO             PIC X(7).
000036         16  NA-CERT-NO.
000037             20  NA-CERT-PRIME       PIC X(10).
000038             20  NA-CERT-SFX         PIC X.
000039         16  NA-ARCHIVE-NO           PIC 9(8).
000040
000041     12  NA-LETTER-INFORMATION.
000042         16  NA-LETTER-ID            PIC X(4).
000043         16  NA-PROCESSOR-ID         PIC X(4).
000044         16  NA-CREATION-DT          PIC X(2).
000045         16  NA-INITIAL-PRINT-DT     PIC X(2).
000046         16  NA-FOLLOW-UP-DT         PIC X(2).
000047         16  NA-RESEND-DT            PIC X(2).
000048         16  NA-RESEND-LETTER-ID     PIC X(4).
000049         16  NA-NO-OF-COPIES         PIC 9(2).
000050         16  NA-ADDRESS-TYPE         PIC X(2).
000051         16  NA-CORR-TRLR-SEQ        PIC 9(4).
000052         16  NA-RESEND-PRINT-DT      PIC X(2).
000053         16  NA-1ST-LTR-PRINT-DT     PIC X(2).
000054         16  NA-NEXT-DUE-DT          PIC X(2).
000055         16  NA-AUTOPYDT             PIC X(2).
000056         16  NA-ENCLOSURE-CD         PIC X(3).
000057         16  NA-CREATED-IN-NAPERSOFT PIC X(1).
000058         16  NA-ORIG-ARCHIVE-NO      PIC 9(9).
000059         16  NA-RESEND-PROMPT-IND    PIC X(1).
000060         16  NA-ORIG-ENCLOSURE-CD    PIC X(3).
000061         16  FILLER                  PIC X(67).
000062******************************************************************
      *<<((file: ELCNAPS))
000250*                                COPY ELCENCC.
      *>>((file: ELCENCC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCENCC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
000008*                                                                *
000009*   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
000010*   A NAPERSOFT DOCUMENT                                         *
000011*                                                                *
000012*   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 400   RECFORM = FIX                            *
000016*                                                                *
000017*   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
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
000031* 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
000032* 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
000033******************************************************************
000034
000035 01  ENCLOSURE-CODES.
000036     12  NC-RECORD-ID                      PIC XX.
000037         88  VALID-NC-ID                      VALUE 'NC'.
000038
000039     12  NC-CONTROL-PRIMARY.
000040         16  NC-COMPANY-CD                 PIC X.
000041         16  NC-REC-TYPE                   PIC X.
000042             88  NC-CLAIMS                   VALUE '1'.
000043             88  NC-ADMIN                    VALUE '2'.
000044         16  NC-ENC-CODE                   PIC X(5).
000045         16  FILLER                        PIC X(09).
000046
000047     12  NC-MAINT-INFORMATION.
000048         16  NC-LAST-MAINT-DT              PIC XX.
000049         16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000050         16  NC-LAST-MAINT-USER            PIC X(4).
000051         16  FILLER                        PIC XX.
000052
000053     12  NC-OUTPUT-STACK                   PIC XXX.
000054     12  NC-ENCLOSURE-LINE                 PIC X(100).
000055     12  NC-ATTACHMENTS                    PIC X(255).
000056     12  NC-FUTURE                         PIC X(12).
000057******************************************************************
      *<<((file: ELCENCC))
000251*                                COPY ELCCRTT.
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
000252*                                COPY ERCPDEF.
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
000254
000255 01 dfhcommarea.
000256*                                copy ELCLTRSPI.
      *>>((file: ELCLTRSPI))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 121802    2009122800001  PEMA  NEW COPYBOOK
000010******************************************************************
000011****************************************
000012*  commarea for NaperSoft On Demand Claim letters
000013*  (business logic input & output)
000014****************************************
000015
000016     03  BL-INPUT.
000017         05  BL-CARRIER          PIC X.
000018         05  BL-CLAIM-NO         PIC X(7).
000019         05  BL-CERT-NO          PIC X(11).
000020         05  BL-LETTER-ID        PIC XXXX.
000021         05  BL-FOLLOW-UP-DT     PIC X(10).
000022         05  BL-RESEND-DT        PIC X(10).
000023         05  BL-NO-OF-COPIES     PIC 99.
000024         05  BL-PROC-ID          PIC XXXX.
000025         05  BL-COMP-ID          PIC XXX.
000026         05  BL-PRINT-NOW-SW     PIC X.
000027         05  BL-ENC-CD           PIC XXX.
000028         05  BL-ARCHIVE-NO       PIC 9(8).
000029         05  BL-REGARDING        PIC X(70).
000030
000031     03  BL-OUTPUT.
000032         05  BL-STATUS                   PIC X.
000033             88  BL-OK                      VALUE "P".
000034             88  BL-FAIL                  VALUE "F".
000035         05  BL-MESSAGE          PIC X(50).
000036     03  BL-RECORD-PASSED-DATA   PIC X(2500).
      *<<((file: ELCLTRSPI))
000257
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'NSRLTRBL' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000258 VCOBOL-DUMMY-PROCEDURE.
000259
000260     MOVE EIBDATE                TO DC-JULIAN-YYDDD
000261     MOVE '5'                    TO DC-OPTION-CODE
000262     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
000263     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
000264     MOVE DC-GREG-DATE-A-EDIT    TO WS-SAVE-EDIT-A-DATE
000265     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
000266
000267*****************************************************
000268* The full claim key has been established and passed
000269* to this program via NSREQLTR
000270*****************************************************
000271
000272     INITIALIZE NAPER-OUTPUT-DATA
000273     evaluate bl-comp-id
000274        when 'DCC'
000275           MOVE X'05'            TO WS-ELMSTR-COMPANY-CD
000276        when 'AHL'
000277           MOVE X'06'            TO WS-ELMSTR-COMPANY-CD
000278        when 'VPP'
000279           MOVE X'07'            TO WS-ELMSTR-COMPANY-CD
000280        when 'FNL'
000281           MOVE X'08'            TO WS-ELMSTR-COMPANY-CD
000282        when other
000283           MOVE X'04'            TO WS-ELMSTR-COMPANY-CD
000284     end-evaluate
000285
000286     MOVE BL-CARRIER             TO WS-ELMSTR-CARRIER
000287     MOVE BL-CLAIM-NO            TO WS-ELMSTR-CLAIM-NO
000288     MOVE BL-CERT-NO             TO WS-ELMSTR-CERT-NO
000289     SET BL-FAIL TO TRUE
000290
000291     
      * EXEC CICS READ
000292*         DATASET    ('ELMSTR')
000293*         INTO       (CLAIM-MASTER)
000294*         RIDFLD     (WS-ELMSTR-KEY)
000295*         RESP       (WS-RESPONSE)
000296*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00004561' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034353631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 WS-ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000297
000298     IF RESP-NORMAL
000299        MOVE BL-LETTER-ID        TO OUT-TEMP
000300                                    OUT-LETTER
000301        MOVE BL-PROC-ID          TO OUT-PROCID
000302        MOVE BL-COMP-ID          TO OUT-COMPID
000303        MOVE BL-PRINT-NOW-SW     TO OUT-PRINT-NOW-SW
000304        MOVE CL-CARRIER          TO OUT-CARR
000305                                    OUT-CARR-LU
000306        MOVE CL-INSURED-LAST-NAME
000307                                 TO OUT-ILNAME
000308        MOVE CL-INSURED-1ST-NAME TO OUT-IFNAME
000309        MOVE CL-CLAIM-NO         TO OUT-CLMNO
000310        MOVE CL-CERT-NO          TO OUT-CRTNO
000311        MOVE CL-CERT-ACCOUNT     TO OUT-ACTNO
000312        MOVE CL-SOC-SEC-NO       TO OUT-SSN
000313        MOVE CL-CLAIM-STATUS     TO OUT-CLMSTAT
000314        MOVE CL-CCN (1:9)        TO OUT-AHL-CLAIM-NO
000315
000316        MOVE WS-SAVE-EDIT-A-DATE TO DC-GREG-DATE-A-EDIT
000317        STRING DC-EDITA-CCYY  DC-EDITA-MONTH DC-EDITA-DAY
000318          DELIMITED BY SIZE INTO OUT-CYCLE-DATE
000319        END-STRING
000320
000321*       IF CLAIM-IS-CLOSED
000322*          MOVE 'CLOSED'         TO OUT-CLMSTAT
000323*       ELSE
000324*          MOVE 'OPEN'           TO OUT-CLMSTAT
000325*       END-IF
000326        MOVE CL-CLAIM-TYPE       TO OUT-CLMTYPE
000327        MOVE CL-FILE-ESTABLISH-DT
000328                                 TO DC-BIN-DATE-1
000329        MOVE ' '                 TO DC-OPTION-CODE
000330        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000331        IF NO-CONVERSION-ERROR
000332           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000333              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000334              INTO OUT-EST-DT
000335           END-STRING
000336        ELSE
000337           MOVE SPACES           TO OUT-EST-DT
000338        END-IF
000339        MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
000340        MOVE ' '                 TO DC-OPTION-CODE
000341        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000342        IF NO-CONVERSION-ERROR
000343           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000344              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000345              INTO OUT-PD-THRU-DT
000346           END-STRING
000347        ELSE
000348           MOVE SPACES           TO OUT-PD-THRU-DT
000349        END-IF
000350
000351        MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
000352        MOVE +0                  TO DC-ELAPSED-MONTHS
000353        MOVE +30                 TO DC-ELAPSED-DAYS
000354        MOVE '6'                 TO DC-OPTION-CODE
000355        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000356        IF NO-CONVERSION-ERROR
000357           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000358              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000359              INTO OUT-FORM-DUE-DT
000360           END-STRING
000361        ELSE
000362           MOVE SPACES           TO OUT-FORM-DUE-DT
000363        END-IF
000364
000365        MOVE CL-TOTAL-PAID-AMT   TO OUT-TOT-PAID
000366        MOVE CL-INSURED-SEX-CD   TO OUT-SEX-CD
000367        MOVE CL-CERT-STATE       TO OUT-CRT-ST
000368                                    OUT-ENC-ST
000369                                    OUT-ENC-ST-NAME
000370
000371        MOVE CL-CERT-EFF-DT      TO DC-BIN-DATE-1
000372        MOVE ' '                 TO DC-OPTION-CODE
000373        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000374        IF NO-CONVERSION-ERROR
000375           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000376              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000377              INTO OUT-EFF-DT
000378           END-STRING
000379        ELSE
000380           MOVE SPACES           TO OUT-EFF-DT
000381        END-IF
000382        MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
000383        MOVE ' '                 TO DC-OPTION-CODE
000384        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000385        IF NO-CONVERSION-ERROR
000386           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000387              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000388              INTO OUT-INC-DT
000389           END-STRING
000390        ELSE
000391           MOVE SPACES           TO OUT-INC-DT
000392        END-IF
000393        MOVE CL-REPORTED-DT      TO DC-BIN-DATE-1
000394        MOVE ' '                 TO DC-OPTION-CODE
000395        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000396        IF NO-CONVERSION-ERROR
000397           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000398              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000399              INTO OUT-RPT-DT
000400           END-STRING
000401        ELSE
000402           MOVE SPACES           TO OUT-RPT-DT
000403        END-IF
000404        MOVE CL-LAST-PMT-DT      TO DC-BIN-DATE-1
000405        MOVE ' '                 TO DC-OPTION-CODE
000406        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000407        IF NO-CONVERSION-ERROR
000408           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000409              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000410              INTO OUT-LST-PMT-DT
000411           END-STRING
000412        ELSE
000413           MOVE SPACES           TO OUT-LST-PMT-DT
000414        END-IF
000415        MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
000416        MOVE ' '                 TO DC-OPTION-CODE
000417        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000418        IF NO-CONVERSION-ERROR
000419           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000420              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000421              INTO OUT-BIRTH-DT
000422           END-STRING
000423        ELSE
000424           MOVE SPACES           TO OUT-BIRTH-DT
000425        END-IF
000426        MOVE CL-LAST-MAINT-DT    TO DC-BIN-DATE-1
000427        MOVE ' '                 TO DC-OPTION-CODE
000428        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
000429        IF NO-CONVERSION-ERROR
000430           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000431              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000432              INTO OUT-LST-MAINT-DT
000433           END-STRING
000434        ELSE
000435           MOVE SPACES           TO OUT-LST-MAINT-DT
000436        END-IF
000437        MOVE CL-LAST-MAINT-USER  TO OUT-LST-MAINT-USER
000438        MOVE CL-LAST-MAINT-TYPE  TO OUT-LST-MAINT-TYPE
000439        MOVE CL-NO-OF-PMTS-MADE  TO OUT-NO-OF-PMTS
000440        MOVE CL-LAST-PMT-AMT     TO OUT-LST-PD-AMT
000441        MOVE CL-ACCOUNT-ADDR-CNT TO OUT-ADDR-CNT
000442        MOVE CL-LAST-CLOSE-REASON TO OUT-LST-CLS-REA
000443        MOVE BL-ENC-CD           TO OUT-ENC-CODE
000444        PERFORM 1000-GET-FILES   THRU 1000-EXIT
000445        PERFORM 1800-SET-EL150D-FIELDS THRU 1800-EXIT
000446        MOVE FUNCTION REVERSE(CL-CLAIM-NO(3:5)) TO OUT-VER-CD
000447        SET BL-OK TO TRUE
000448     END-IF
000449
000450     IF BL-OK
000451        PERFORM 1500-GET-ARCH-NO THRU 1500-EXIT
000452        MOVE WS-ARCHIVE-NO       TO OUT-ARCHNO
000453        PERFORM 0500-SCRUB-DATA  THRU 0500-EXIT
000454        MOVE NAPER-OUTPUT-DATA   TO BL-RECORD-PASSED-DATA
000455*       PERFORM 1600-ADD-CORR-TRLR
000456*                                THRU 1600-EXIT
000457     END-IF
000458
000459     
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00004729' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034373239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000460
000461 0500-SCRUB-DATA.
000462
000463*   THE DECODER DOES NOT LIKE & AND I TRIED TO USE
000464*   A DIFFERENT DELIMITER IN PROGRAM NSREQLTR BUT
000465*   COULD NOT GET IT TO WORK SO I AM CONVERTING IT
000466*   TO A HEX 26 TO GET BY THE DECODER
000467
000468     MOVE FUNCTION LENGTH(OUT-ILNAME)
000469                                 TO M1
000470     MOVE SPACES                 TO WS-WORK-FIELD
000471     perform varying a1 from +1 by +1 until a1 > m1
000472        if out-ilname (a1:1) = '&'
000473           MOVE OUT-ILNAME (1:A1 - 1)
000474                                 TO WS-WORK-FIELD (1:A1 - 1)
000475           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000476           MOVE OUT-ILNAME (A1 + 1:M1 - A1)
000477                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000478           MOVE WS-WORK-FIELD    TO OUT-ILNAME
000479           add +2 to a1
000480        end-if
000481     end-perform
000482
000483     MOVE FUNCTION LENGTH(OUT-ACCT-ADDR1)
000484                                 TO M1
000485     MOVE SPACES                 TO WS-WORK-FIELD
000486
000487     perform varying a1 from +1 by +1 until a1 > m1
000488        if out-acct-addr1 (a1:1) = '&'
000489           MOVE out-acct-addr1 (1:A1 - 1)
000490                                 TO WS-WORK-FIELD (1:A1 - 1)
000491           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000492           MOVE out-acct-addr1 (A1 + 1:M1 - A1)
000493                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000494           MOVE WS-WORK-FIELD    TO out-acct-addr1
000495           add +2 to a1
000496        end-if
000497     end-perform
000498
000499     MOVE FUNCTION LENGTH(OUT-ACCT-ADDR2)
000500                                 TO M1
000501     MOVE SPACES                 TO WS-WORK-FIELD
000502
000503     perform varying a1 from +1 by +1 until a1 > m1
000504        if out-acct-addr2 (a1:1) = '&'
000505           MOVE out-acct-addr2 (1:A1 - 1)
000506                                 TO WS-WORK-FIELD (1:A1 - 1)
000507           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000508           MOVE out-acct-addr2 (A1 + 1:M1 - A1)
000509                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000510           MOVE WS-WORK-FIELD    TO out-acct-addr2
000511           add +2 to a1
000512        end-if
000513     end-perform
000514
000515     MOVE FUNCTION LENGTH(OUT-ACCT-CITY)
000516                                 TO M1
000517     MOVE SPACES                 TO WS-WORK-FIELD
000518
000519     perform varying a1 from +1 by +1 until a1 > m1
000520        if out-acct-city (a1:1) = '&'
000521           MOVE out-acct-city (1:A1 - 1)
000522                                 TO WS-WORK-FIELD (1:A1 - 1)
000523           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000524           MOVE out-acct-city (A1 + 1:M1 - A1)
000525                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000526           MOVE WS-WORK-FIELD    TO out-acct-city
000527           add +2 to a1
000528        end-if
000529     end-perform
000530
000531     MOVE FUNCTION LENGTH(OUT-ACCT-NAME)
000532                                 TO M1
000533     MOVE SPACES                 TO WS-WORK-FIELD
000534
000535     perform varying a1 from +1 by +1 until a1 > m1
000536        if out-acct-name (a1:1) = '&'
000537           MOVE out-acct-name (1:A1 - 1)
000538                                 TO WS-WORK-FIELD (1:A1 - 1)
000539           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000540           MOVE out-acct-name (A1 + 1:M1 - A1)
000541                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000542           MOVE WS-WORK-FIELD    TO out-acct-name
000543           add +2 to a1
000544        end-if
000545     end-perform
000546
000547     MOVE FUNCTION LENGTH(OUT-ORIG-NAME)
000548                                 TO M1
000549     MOVE SPACES                 TO WS-WORK-FIELD
000550
000551     perform varying a1 from +1 by +1 until a1 > m1
000552        if out-orig-name (a1:1) = '&'
000553           MOVE out-orig-name (1:A1 - 1)
000554                                 TO WS-WORK-FIELD (1:A1 - 1)
000555           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000556           MOVE out-orig-name (A1 + 1:M1 - A1)
000557                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000558           MOVE WS-WORK-FIELD    TO out-orig-name
000559           add +2 to a1
000560        end-if
000561     end-perform
000562
000563     MOVE FUNCTION LENGTH(OUT-INS-NAME)
000564                                 TO M1
000565     MOVE SPACES                 TO WS-WORK-FIELD
000566
000567     perform varying a1 from +1 by +1 until a1 > m1
000568        if out-ins-name (a1:1) = '&'
000569           MOVE out-ins-name (1:A1 - 1)
000570                                 TO WS-WORK-FIELD (1:A1 - 1)
000571           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000572           MOVE out-ins-name (A1 + 1:M1 - A1)
000573                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000574           MOVE WS-WORK-FIELD    TO out-ins-name
000575           add +2 to a1
000576        end-if
000577     end-perform
000578
000579     MOVE FUNCTION LENGTH(OUT-INS-ADDR1)
000580                                 TO M1
000581     MOVE SPACES                 TO WS-WORK-FIELD
000582
000583     perform varying a1 from +1 by +1 until a1 > m1
000584        if out-ins-addr1 (a1:1) = '&'
000585           MOVE out-ins-addr1 (1:A1 - 1)
000586                                 TO WS-WORK-FIELD (1:A1 - 1)
000587           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000588           MOVE out-ins-addr1 (A1 + 1:M1 - A1)
000589                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000590           MOVE WS-WORK-FIELD    TO out-ins-addr1
000591           add +2 to a1
000592        end-if
000593     end-perform
000594
000595     MOVE FUNCTION LENGTH(OUT-INS-ADDR2)
000596                                 TO M1
000597     MOVE SPACES                 TO WS-WORK-FIELD
000598
000599     perform varying a1 from +1 by +1 until a1 > m1
000600        if out-ins-addr2 (a1:1) = '&'
000601           MOVE out-ins-addr2 (1:A1 - 1)
000602                                 TO WS-WORK-FIELD (1:A1 - 1)
000603           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000604           MOVE out-ins-addr2 (A1 + 1:M1 - A1)
000605                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000606           MOVE WS-WORK-FIELD    TO out-ins-addr2
000607           add +2 to a1
000608        end-if
000609     end-perform
000610
000611     MOVE FUNCTION LENGTH(OUT-BEN-NAME)   TO M1
000612     MOVE SPACES                 TO WS-WORK-FIELD
000613
000614     perform varying a1 from +1 by +1 until a1 > m1
000615        if out-ben-name (a1:1) = '&'
000616           MOVE out-ben-name (1:A1 - 1)
000617                                 TO WS-WORK-FIELD (1:A1 - 1)
000618           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000619           MOVE out-ben-name (A1 + 1:M1 - A1)
000620                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000621           MOVE WS-WORK-FIELD    TO out-ben-name
000622           add +2 to a1
000623        end-if
000624     end-perform
000625
000626     MOVE FUNCTION LENGTH(OUT-BEN-ADDR1)
000627                                 TO M1
000628     MOVE SPACES                 TO WS-WORK-FIELD
000629
000630     perform varying a1 from +1 by +1 until a1 > m1
000631        if out-ben-addr1 (a1:1) = '&'
000632           MOVE out-ben-addr1 (1:A1 - 1)
000633                                 TO WS-WORK-FIELD (1:A1 - 1)
000634           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000635           MOVE out-ben-addr1 (A1 + 1:M1 - A1)
000636                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000637           MOVE WS-WORK-FIELD    TO out-ben-addr1
000638           add +2 to a1
000639        end-if
000640     end-perform
000641
000642     MOVE FUNCTION LENGTH(OUT-BEN-ADDR2)
000643                                 TO M1
000644     MOVE SPACES                 TO WS-WORK-FIELD
000645
000646     perform varying a1 from +1 by +1 until a1 > m1
000647        if out-ben-addr2 (a1:1) = '&'
000648           MOVE out-ben-addr2 (1:A1 - 1)
000649                                 TO WS-WORK-FIELD (1:A1 - 1)
000650           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000651           MOVE out-ben-addr2 (A1 + 1:M1 - A1)
000652                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000653           MOVE WS-WORK-FIELD    TO out-ben-addr2
000654           add +2 to a1
000655        end-if
000656     end-perform
000657
000658     MOVE FUNCTION LENGTH(OUT-DIAG)       TO M1
000659     MOVE SPACES                 TO WS-WORK-FIELD
000660
000661     perform varying a1 from +1 by +1 until a1 > m1
000662        if out-diag (a1:1) = '&'
000663           MOVE out-diag (1:A1 - 1)
000664                                 TO WS-WORK-FIELD (1:A1 - 1)
000665           MOVE '%26'            TO WS-WORK-FIELD (A1:3)
000666           MOVE out-diag (A1 + 1:M1 - A1)
000667                                 TO WS-WORK-FIELD (A1 + 3:M1 - A1)
000668           MOVE WS-WORK-FIELD    TO out-diag
000669           add +2 to a1
000670        end-if
000671     end-perform
000672
000673     MOVE ZEROS TO WS-TALLY WS-TALLY1
000674     INSPECT NAPER-OUTPUT-DATA TALLYING WS-TALLY
000675        FOR ALL '%'
000676     INSPECT NAPER-OUTPUT-DATA TALLYING WS-TALLY1
000677        FOR ALL '%26'
000678     IF (WS-TALLY > 0)
000679        AND (WS-TALLY NOT = WS-TALLY1)
000680        MOVE +1                  TO A1
000681        MOVE FUNCTION LENGTH(NAPER-OUTPUT-DATA)
000682                                 TO NS-LEN
000683        PERFORM WS-TALLY TIMES
000684           PERFORM VARYING A1 FROM A1 BY +1 UNTIL
000685              (A1 > NS-LEN)
000686              OR (NAPER-OUTPUT-DATA (A1:1) = '%')
000687           END-PERFORM
000688           IF A1 > NS-LEN
000689              CONTINUE
000690           ELSE
000691              IF NAPER-OUTPUT-DATA (A1:3) = '%26'
000692                 ADD +1 TO A1
000693              ELSE
000694                 MOVE NAPER-OUTPUT-DATA (1:A1 - 1)
000695                                 TO WS-XML-WORK
000696                 MOVE '%25'      TO WS-XML-WORK (A1:3)
000697                 MOVE NAPER-OUTPUT-DATA (A1 + 1:NS-LEN - A1)
000698                              TO WS-XML-WORK (A1 + 3:NS-LEN - A1)
000699                 MOVE WS-XML-WORK TO NAPER-OUTPUT-DATA
000700                 ADD +3 TO A1
000701              END-IF
000702           END-IF
000703        END-PERFORM
000704     END-IF
000705
000706     .
000707 0500-EXIT.
000708     EXIT.
000709
000710 1000-GET-FILES.
000711
000712     PERFORM 1100-GET-ELCERT     THRU 1100-EXIT
000713     PERFORM 1200-GET-ERACCT     THRU 1200-EXIT
000714
000715     MOVE ' '                    TO WS-FOUND-BENE-SW
000716     PERFORM 1250-GET-ELLETR     THRU 1250-EXIT
000717     PERFORM 1300-GET-ELTRLRS    THRU 1300-EXIT
000718     PERFORM 1700-GET-ELENCC     THRU 1700-EXIT
000719
000720     IF CL-CLAIM-TYPE NOT = 'L' AND 'P' AND 'O'
000721        PERFORM 1400-GET-WAIT-PER THRU 1400-EXIT
000722     END-IF
000723
000724     IF NOT FOUND-BENE
000725        MOVE CL-COMPANY-CD       TO WS-ELBENE-COMPANY-CD
000726        MOVE 'B'                 TO WS-ELBENE-REC-TYPE
000727        MOVE CL-BENEFICIARY      TO WS-ELBENE-BENE
000728        
      * EXEC CICS READ
000729*            DATASET    ('ELBENE')
000730*            INTO       (BENEFICIARY-MASTER)
000731*            RIDFLD     (WS-ELBENE-KEY)
000732*            RESP       (WS-RESPONSE)
000733*       END-EXEC
           MOVE LENGTH OF
            BENEFICIARY-MASTER
             TO DFHEIV11
           MOVE 'ELBENE' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00004998' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303034393938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 BENEFICIARY-MASTER, 
                 DFHEIV11, 
                 WS-ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000734        IF RESP-NORMAL
000735           MOVE BE-MAIL-TO-NAME   TO OUT-BEN-NAME
000736           MOVE BE-ADDRESS-LINE-1 TO OUT-BEN-ADDR1
000737           MOVE BE-ADDRESS-LINE-2 TO OUT-BEN-ADDR2
000738           MOVE BE-CITY           TO OUT-BEN-CITY
000739           MOVE BE-STATE          TO OUT-BEN-STATE
000740           MOVE BE-ZIP-PRIME      TO OUT-BEN-ZIP
000741           IF BE-ZIP-PLUS4 NOT = SPACES AND ZEROS
000742              STRING '-' BE-ZIP-PLUS4 DELIMITED BY SIZE
000743                 INTO OUT-BEN-ZIP (6:5)
000744              END-STRING
000745           END-IF
000746           MOVE BE-PHONE-NO       TO OUT-BEN-PHONE
000747        END-IF
000748     END-IF
000749
000750     .
000751 1000-EXIT.
000752     EXIT.
000753
000754 1100-GET-ELCERT.
000755
000756     MOVE CL-COMPANY-CD          TO WS-ELCERT-COMPANY-CD
000757     MOVE CL-CERT-KEY-DATA       TO WS-ELCERT-KEY (2:21)
000758     MOVE CL-CERT-NO             TO WS-ELCERT-CERT-NO
000759
000760     
      * EXEC CICS READ
000761*         DATASET    ('ELCERT')
000762*         INTO       (CERTIFICATE-MASTER)
000763*         RIDFLD     (WS-ELCERT-KEY)
000764*         RESP       (WS-RESPONSE)
000765*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005030' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035303330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 WS-ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000766
000767     IF RESP-NORMAL
000768        IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
000769           MOVE CM-LF-BENEFIT-AMT TO OUT-BENAMT
000770           MOVE CM-LF-ORIG-TERM  TO OUT-TERM
000771           MOVE CM-LF-BENEFIT-CD TO OUT-BEN-CD
000772           MOVE CM-LF-CRITICAL-PERIOD
000773                                 TO OUT-CRIT-PER
000774           MOVE CM-LF-LOAN-EXPIRE-DT
000775                                 TO DC-BIN-DATE-1
000776           MOVE ' '              TO DC-OPTION-CODE
000777           PERFORM 9700-DATE-LINK
000778                                 THRU 9700-EXIT
000779           IF NO-CONVERSION-ERROR
000780              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000781                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000782                 INTO OUT-EXP-DT
000783              END-STRING
000784           ELSE
000785              MOVE SPACES        TO OUT-EXP-DT
000786           END-IF
000787        ELSE
000788           MOVE CM-AH-BENEFIT-AMT TO OUT-BENAMT
000789           MOVE CM-AH-ORIG-TERM  TO OUT-TERM
000790           MOVE CM-AH-BENEFIT-CD TO OUT-BEN-CD
000791           MOVE CM-AH-CRITICAL-PERIOD
000792                                 TO OUT-CRIT-PER
000793           MOVE CM-AH-LOAN-EXPIRE-DT
000794                                 TO DC-BIN-DATE-1
000795           MOVE ' '              TO DC-OPTION-CODE
000796           PERFORM 9700-DATE-LINK
000797                                 THRU 9700-EXIT
000798           IF NO-CONVERSION-ERROR
000799              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
000800                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
000801                 INTO OUT-EXP-DT
000802              END-STRING
000803           ELSE
000804              MOVE SPACES        TO OUT-EXP-DT
000805           END-IF
000806        END-IF
000807        MOVE CM-ACCOUNT          TO OUT-CERTACCT
000808        IF CL-INSURED-1ST-NAME = CM-INSURED-FIRST-NAME
000809            MOVE CM-JT-FIRST-NAME TO OUT-JFNAME
000810            MOVE CM-JT-LAST-NAME  TO OUT-JLNAME
000811        ELSE
000812            MOVE CM-INSURED-FIRST-NAME TO OUT-JFNAME
000813            MOVE CM-INSURED-LAST-NAME TO OUT-JLNAME
000814        END-IF
000815
000816
000817        MOVE CM-LOAN-APR         TO OUT-APR
000818     END-IF
000819
000820     .
000821 1100-EXIT.
000822     EXIT.
000823
000824 1200-GET-ERACCT.
000825
000826    MOVE LOW-VALUES              TO WS-ERACCT-KEY
000827    MOVE WS-ELCERT-KEY (1:22)    TO WS-ERACCT-KEY (1:22)
000828
000829     
      * EXEC CICS STARTBR
000830*        DATASET   ('ERACCT')
000831*        RIDFLD    (WS-ERACCT-KEY)
000832*        GTEQ
000833*        RESP      (WS-RESPONSE)
000834*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005099' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035303939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000835
000836     IF RESP-NORMAL
000837        
      * EXEC CICS READNEXT
000838*          INTO    (ACCOUNT-MASTER)
000839*          DATASET ('ERACCT')
000840*          RIDFLD  (WS-ERACCT-KEY)
000841*          RESP    (WS-RESPONSE)
000842*       END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005107' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035313037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000843     END-IF
000844
000845     IF RESP-NORMAL
000846        PERFORM WITH TEST AFTER UNTIL
000847           NOT (RESP-NORMAL)
000848           OR (CM-CONTROL-PRIMARY (1:20) NOT =
000849                        AM-CONTROL-PRIMARY (1:20))
000850           IF (CM-CONTROL-PRIMARY (1:20)
000851              = AM-CONTROL-PRIMARY (1:20))
000852              IF (CL-CERT-EFF-DT >= AM-EFFECTIVE-DT)
000853                 AND (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
000854                 MOVE AM-NAME    TO OUT-ORIG-NAME
000855                 MOVE ACCOUNT-MASTER TO SAVE-ACCOUNT-MASTER
000856              END-IF
000857              MOVE AM-NAME       TO OUT-ACCT-NAME
000858              MOVE AM-ADDRS      TO OUT-ACCT-ADDR1
000859              MOVE SPACES        TO OUT-ACCT-ADDR2
000860              MOVE AM-ADDR-CITY  TO OUT-ACCT-CITY
000861              MOVE AM-ADDR-STATE TO OUT-ACCT-STATE
000862              MOVE AM-ZIP-PRIME  TO OUT-ACCT-ZIP
000863              IF AM-ZIP-PLUS4 NOT = SPACES AND ZEROS
000864                 STRING '-' AM-ZIP-PLUS4 DELIMITED BY SIZE
000865                    INTO OUT-ACCT-ZIP (6:5)
000866                 END-STRING
000867              END-IF
000868              MOVE AM-TEL-NO     TO OUT-ACCT-PHONE
000869              MOVE AM-GPCD       TO OUT-ACCT-GPCD
000870              
      * EXEC CICS READNEXT
000871*                INTO    (ACCOUNT-MASTER)
000872*                DATASET ('ERACCT')
000873*                RIDFLD  (WS-ERACCT-KEY)
000874*                RESP    (WS-RESPONSE)
000875*             END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005140' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035313430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000876           END-IF
000877        END-PERFORM
000878     END-IF
000879
000880     MOVE SAVE-ACCOUNT-MASTER  TO ACCOUNT-MASTER
000881
000882    .
000883 1200-EXIT.
000884     EXIT.
000885
000886 1250-GET-ELLETR.
000887
000888     MOVE CL-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
000889     MOVE BL-LETTER-ID           TO WS-ELLETR-LETTER-ID
000890     MOVE +0                     TO WS-ELLETR-SEQ-NO
000891
000892     
      * EXEC CICS READ
000893*         DATASET    ('ELLETR')
000894*         INTO       (TEXT-FILES)
000895*         RIDFLD     (WS-ELLETR-KEY)
000896*         RESP       (WS-RESPONSE)
000897*         GTEQ
000898*    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
           MOVE 'ELLETR' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005162' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303035313632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 WS-ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000899
000900     IF RESP-NORMAL
000901        IF (LETTER-FILE-TEXT)
000902           AND (BL-LETTER-ID = TX-LETTER-NO)
000903           AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
000904           PERFORM 1280-PROCESS-Z-CONTROLS
000905                                 THRU 1280-EXIT
000906        END-IF
000907     END-IF
000908
000909     .
000910 1250-EXIT.
000911     EXIT.
000912
000913 1280-PROCESS-Z-CONTROLS.
000914
000915     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
000916
000917     IF W-FORM-TO-RESEND > SPACES
000918        MOVE W-FORM-TO-RESEND    TO OUT-FORM
000919     ELSE
000920        MOVE SPACES              TO OUT-FORM
000921     END-IF.
000922
000923*    MOVE W-ENCLOSURE-CD         TO OUT-ENC-CODE
000924
000925     .
000926 1280-EXIT.
000927     EXIT.
000928
000929
000930 1300-GET-ELTRLRS.
000931
000932     MOVE CL-CONTROL-PRIMARY     TO WS-ELTRLR-KEY
000933     MOVE +0                     TO WS-ELTRLR-SEQ-NO
000934
000935     
      * EXEC CICS STARTBR
000936*        DATASET   ('ELTRLR')
000937*        RIDFLD    (WS-ELTRLR-KEY)
000938*        GTEQ
000939*        RESP      (WS-RESPONSE)
000940*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005205' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035323035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000941
000942    IF RESP-NORMAL
000943       
      * EXEC CICS READNEXT
000944*         INTO    (ACTIVITY-TRAILERS)
000945*         DATASET ('ELTRLR')
000946*         RIDFLD  (WS-ELTRLR-KEY)
000947*         RESP    (WS-RESPONSE)
000948*      END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV12
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005213' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035323133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV12, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000949       PERFORM 1310-GET-ELTRLRS THRU 1310-EXIT
000950    END-IF
000951
000952     .
000953 1300-EXIT.
000954     EXIT.
000955
000956 1310-GET-ELTRLRS.
000957
000958     IF RESP-NORMAL
000959
000960     IF CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20)
000961        EVALUATE TRUE
000962           WHEN AT-INSURED-ADDR-TRL
000963              MOVE AT-MAIL-TO-NAME   TO OUT-INS-NAME
000964              MOVE AT-ADDRESS-LINE-1 TO OUT-INS-ADDR1
000965              MOVE AT-ADDRESS-LINE-2 TO OUT-INS-ADDR2
000966              MOVE AT-CITY           TO OUT-INS-CITY
000967              MOVE AT-STATE          TO OUT-INS-STATE
000968              MOVE AT-ZIP-CODE       TO OUT-INS-ZIP
000969              IF AT-ZIP-PLUS4 NOT = SPACES AND ZEROS
000970                 STRING '-' AT-ZIP-PLUS4 DELIMITED BY SIZE
000971                    INTO OUT-INS-ZIP (6:5)
000972                 END-STRING
000973              END-IF
000974              MOVE AT-PHONE-NO       TO OUT-INS-PHONE
000975           WHEN AT-BENEFICIARY-ADDR-TRL
000976              SET FOUND-BENE TO TRUE
000977              MOVE AT-MAIL-TO-NAME   TO OUT-BEN-NAME
000978              MOVE AT-ADDRESS-LINE-1 TO OUT-BEN-ADDR1
000979              MOVE AT-ADDRESS-LINE-2 TO OUT-BEN-ADDR2
000980              MOVE AT-CITY           TO OUT-BEN-CITY
000981              MOVE AT-STATE          TO OUT-BEN-STATE
000982              MOVE AT-ZIP-CODE       TO OUT-BEN-ZIP
000983              IF AT-ZIP-PLUS4 NOT = SPACES AND ZEROS
000984                 STRING '-' AT-ZIP-PLUS4 DELIMITED BY SIZE
000985                    INTO OUT-BEN-ZIP (6:5)
000986                 END-STRING
000987              END-IF
000988              MOVE AT-PHONE-NO       TO OUT-BEN-PHONE
000989           WHEN AT-ACCOUNT-ADDR-TRL
000990              MOVE AT-MAIL-TO-NAME   TO OUT-ACCT-NAME
000991              MOVE AT-ADDRESS-LINE-1 TO OUT-ACCT-ADDR1
000992              MOVE AT-ADDRESS-LINE-2 TO OUT-ACCT-ADDR2
000993              MOVE AT-CITY           TO OUT-ACCT-CITY
000994              MOVE AT-STATE          TO OUT-ACCT-STATE
000995              MOVE AT-ZIP-CODE       TO OUT-ACCT-ZIP
000996              IF AT-ZIP-PLUS4 NOT = SPACES AND ZEROS
000997                 STRING '-' AT-ZIP-PLUS4 DELIMITED BY SIZE
000998                    INTO OUT-ACCT-ZIP (6:5)
000999                 END-STRING
001000              END-IF
001001              MOVE AT-PHONE-NO       TO OUT-ACCT-PHONE
001002           WHEN AT-SEQUENCE-NO = +90
001003              MOVE AT-INFO-LINE-1   TO OUT-DIAG
001004           WHEN AT-SEQUENCE-NO = +91
001005              MOVE AT-INFO-LINE-1 TO OUT-LOAN-NO
001006           WHEN AT-TRAILER-TYPE = '2' AND AT-PAYMENT-TYPE = 'I'
001007              IF AT-INT-RATE NUMERIC AND
001008                 AT-INT-RATE NOT EQUAL ZEROS
001009                    COMPUTE OUT-LF-INT-RATE = AT-INT-RATE * 100
001010              END-IF
001011
001012           WHEN AT-TRAILER-TYPE = '3'
001013              IF (SAVE-BIN-DATE >= AT-SCHEDULE-START-DT)
001014                 AND (SAVE-BIN-DATE <= AT-SCHEDULE-END-DT)
001015                 AND (AT-TERMINATED-DT = LOW-VALUES OR SPACES)
001016                 MOVE AT-SCHEDULE-END-DT TO WS-AUTO-LAST-SCHED-DT
001017              END-IF
001018           WHEN AT-TRAILER-TYPE = '8'
001019              IF AT-RECORDED-DT > WS-LAST-ACT-DT
001020                 MOVE AT-RECORDED-DT
001021                              TO WS-LAST-ACT-DT
001022                 MOVE 'DENIAL'
001023                              TO WS-LAST-ACT-TYPE
001024              END-IF
001025           WHEN AT-TRAILER-TYPE = 'A'
001026              IF AT-RECORDED-DT > WS-LAST-ACT-DT
001027                 MOVE AT-RECORDED-DT
001028                              TO WS-LAST-ACT-DT
001029                 MOVE 'FORMS' TO WS-LAST-ACT-TYPE
001030                 IF INITIAL-FORM
001031                    MOVE 'INIT'
001032                              TO WS-FORM
001033                 END-IF
001034                 IF PROGRESS-FORM
001035                    MOVE 'PROG'
001036                              TO WS-FORM
001037                 END-IF
001038              END-IF
001039              IF AT-FORM-SEND-ON-DT > WS-ACTIVITY-DT
001040                 MOVE AT-FORM-SEND-ON-DT
001041                              TO WS-ACTIVITY-DT
001042              END-IF
001043              IF AT-FORM-REPRINT-DT > WS-ACTIVITY-DT
001044                 MOVE AT-FORM-REPRINT-DT
001045                              TO WS-ACTIVITY-DT
001046              END-IF
001047              IF AT-FORM-ANSWERED-DT > WS-ACTIVITY-DT
001048                 MOVE AT-FORM-ANSWERED-DT
001049                              TO WS-ACTIVITY-DT
001050              END-IF
001051        END-EVALUATE
001052        
      * EXEC CICS READNEXT
001053*         INTO    (ACTIVITY-TRAILERS)
001054*         DATASET ('ELTRLR')
001055*         RIDFLD  (WS-ELTRLR-KEY)
001056*         RESP    (WS-RESPONSE)
001057*       END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV12
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005322' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035333232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV12, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001058
001059        GO TO 1310-GET-ELTRLRS
001060     END-IF
001061
001062     END-IF
001063     .
001064 1310-EXIT.
001065     EXIT.
001066
001067 1400-GET-WAIT-PER.
001068
001069     MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
001070     MOVE CM-AH-BENEFIT-CD       TO WS-ELCNTL-BEN-CD
001071     MOVE '5'                    TO WS-ELCNTL-REC-TYPE
001072     MOVE +0                     TO WS-ELCNTL-SEQ-NO
001073
001074     
      * EXEC CICS READ
001075*       INTO    (CONTROL-FILE)
001076*       DATASET ('ELCNTL')
001077*       RIDFLD  (WS-ELCNTL-KEY)
001078*       GTEQ
001079*       RESP    (WS-RESPONSE)
001080*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005344' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303035333434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001081
001082     IF RESP-NORMAL
001083        AND (CF-COMPANY-ID  = BL-COMP-ID)
001084        AND (CF-RECORD-TYPE = '5')
001085        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001086           (S1 > +8)
001087           OR (CM-AH-BENEFIT-CD = CF-BENEFIT-CODE (S1))
001088        END-PERFORM
001089        IF S1 > +8
001090           MOVE '**'             TO OUT-WAIT-PER
001091        ELSE
001092           MOVE CF-BENEFIT-ALPHA (S1) (1:2)
001093                                 TO OUT-WAIT-PER
001094        END-IF
001095     ELSE
001096        MOVE '**'                TO OUT-WAIT-PER
001097     END-IF
001098
001099    .
001100 1400-EXIT.
001101     EXIT.
001102
001103 1500-GET-ARCH-NO.
001104
001105*    DISPLAY ' MADE IT TO 1500 '
001106     MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
001107     MOVE '1'                    TO WS-ELCNTL-REC-TYPE
001108     MOVE SPACES                 TO WS-ELCNTL-GENL
001109     MOVE +0                     TO WS-ELCNTL-SEQ-NO
001110
001111     
      * EXEC CICS READ
001112*       INTO    (CONTROL-FILE)
001113*       DATASET ('ELCNTL')
001114*       RIDFLD  (WS-ELCNTL-KEY)
001115*       UPDATE
001116*       RESP    (WS-RESPONSE)
001117*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00005381' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303035333831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001118
001119     IF RESP-NORMAL
001120        AND (CF-COMPANY-ID  = BL-COMP-ID)
001121        AND (CF-RECORD-TYPE = '1')
001122        ADD +1                   TO CF-CO-ARCHIVE-COUNTER
001123        MOVE CF-CO-ARCHIVE-COUNTER TO WS-ARCHIVE-NO
001124        MOVE WS-ARCHIVE-NO       TO BL-ARCHIVE-NO
001125        
      * EXEC CICS REWRITE
001126*          FROM    (CONTROL-FILE)
001127*          DATASET ('ELCNTL')
001128*       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00005395' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035333935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001129     ELSE
001130        MOVE +0                  TO WS-ARCHIVE-NO
001131     END-IF
001132
001133    .
001134 1500-EXIT.
001135     EXIT.
001136
001137 1600-ADD-CORR-TRLR.
001138
001139     
      * EXEC CICS READ
001140*         DATASET    ('ELMSTR')
001141*         INTO       (CLAIM-MASTER)
001142*         RIDFLD     (WS-ELMSTR-KEY)
001143*         UPDATE
001144*         RESP       (WS-RESPONSE)
001145*    END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00005409' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303035343039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 WS-ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001146
001147     IF NOT RESP-NORMAL
001148        DISPLAY ' 1600 READ UPD MSTR ' WS-RESPONSE
001149        GO TO 1600-EXIT
001150     END-IF
001151
001152     SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT
001153
001154     PERFORM 2000-BUILD-ELNAPS   THRU 2000-EXIT
001155
001156     IF WS-FOLLOW-UP-DT > CL-NEXT-FOLLOWUP-DT
001157        MOVE WS-FOLLOW-UP-DT     TO CL-NEXT-FOLLOWUP-DT
001158     END-IF
001159
001160     IF WS-RESEND-DT > CL-NEXT-FOLLOWUP-DT
001161        MOVE WS-RESEND-DT        TO CL-NEXT-FOLLOWUP-DT
001162     END-IF
001163     MOVE '2'                    TO CL-LAST-MAINT-TYPE
001164
001165     MOVE 'AT'                   TO ACTIVITY-TRAILERS
001166     MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
001167     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
001168     MOVE  4                     TO AT-TRAILER-TYPE
001169     MOVE SAVE-BIN-DATE          TO AT-RECORDED-DT
001170                                    CL-LAST-MAINT-DT
001171                                    AT-CORR-LAST-MAINT-DT
001172     MOVE BL-PROC-ID             TO AT-RECORDED-BY
001173                                    CL-LAST-MAINT-USER
001174                                    AT-CORR-LAST-UPDATED-BY
001175     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
001176                                    CL-LAST-MAINT-HHMMSS.
001177     MOVE SAVE-BIN-DATE          TO AT-LETTER-SENT-DT.
001178     MOVE WS-FOLLOW-UP-DT        TO AT-RECEIPT-FOLLOW-UP.
001179     MOVE WS-RESEND-DT           TO AT-AUTO-RE-SEND-DT.
001180     MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
001181                                    AT-LETTER-PURGED-DT.
001182     MOVE WS-ARCHIVE-NO          TO AT-LETTER-ARCHIVE-NO.
001183     MOVE '3'                    TO AT-LETTER-ORIGIN
001184
001185     MOVE BL-LETTER-ID           TO AT-STD-LETTER-FORM
001186
001187     MOVE SAVE-BIN-DATE          TO AT-INITIAL-PRINT-DATE
001188
001189     MOVE LOW-VALUES             TO AT-RESEND-PRINT-DATE
001190
001191     
      * EXEC CICS WRITE
001192*         DATASET    ('ELTRLR')
001193*         FROM       (ACTIVITY-TRAILERS)
001194*         RIDFLD     (AT-CONTROL-PRIMARY)
001195*         RESP       (WS-RESPONSE)
001196*    END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00005461' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303035343631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001197
001198     IF NOT RESP-NORMAL
001199        DISPLAY ' 1600 WRITE TRLR ' WS-RESPONSE
001200        GO TO 1600-EXIT
001201     END-IF
001202
001203     
      * EXEC CICS REWRITE
001204*         DATASET    ('ELMSTR')
001205*         FROM       (CLAIM-MASTER)
001206*         RESP       (WS-RESPONSE)
001207*    END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %  N#00005473' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303035343733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001208
001209     IF NOT RESP-NORMAL
001210        DISPLAY ' 1600 REWRITE MSTR ' WS-RESPONSE
001211     END-IF
001212
001213     .
001214 1600-EXIT.
001215     EXIT.
001216
001217 1700-GET-ELENCC.
001218
001219     MOVE CL-COMPANY-CD          TO WS-ELENCC-KEY
001220     MOVE '1'                    TO WS-ELENCC-REC-TYPE
001221     MOVE BL-ENC-CD              TO WS-ELENCC-ENC-CODE
001222     PERFORM VARYING E1 FROM +1 BY +1 UNTIL
001223        (WS-ELENCC-ENC-CODE (E1:1) = ' ')
001224        OR (E1 > +5)
001225     END-PERFORM
001226
001227     IF E1 < +5
001228        MOVE CL-CERT-STATE       TO WS-ELENCC-ENC-CODE (E1:2)
001229     END-IF
001230
001231     
      * EXEC CICS READ
001232*        DATASET   ('ELENCC')
001233*        INTO      (ENCLOSURE-CODES)
001234*        RIDFLD    (WS-ELENCC-KEY)
001235*        RESP      (WS-RESPONSE)
001236*    END-EXEC
           MOVE LENGTH OF
            ENCLOSURE-CODES
             TO DFHEIV11
           MOVE 'ELENCC' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005501' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035353031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENCLOSURE-CODES, 
                 DFHEIV11, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001237
001238     IF NOT RESP-NORMAL
001239        MOVE BL-ENC-CD           TO WS-ELENCC-ENC-CODE
001240        
      * EXEC CICS READ
001241*           DATASET   ('ELENCC')
001242*           INTO      (ENCLOSURE-CODES)
001243*           RIDFLD    (WS-ELENCC-KEY)
001244*           RESP      (WS-RESPONSE)
001245*       END-EXEC
           MOVE LENGTH OF
            ENCLOSURE-CODES
             TO DFHEIV11
           MOVE 'ELENCC' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005510' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035353130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENCLOSURE-CODES, 
                 DFHEIV11, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001246     END-IF
001247
001248     IF RESP-NORMAL
001249        MOVE NC-OUTPUT-STACK     TO OUT-OUTSTACK
001250        MOVE NC-ENCLOSURE-LINE   TO OUT-ENCLINE
001251        MOVE NC-ATTACHMENTS      TO OUT-ENCATTS
001252     END-IF
001253
001254     .
001255 1700-EXIT.
001256     EXIT.
001257 1800-SET-EL150D-FIELDS.
001258     MOVE SPACES TO WS-UNSORTED-TABLE
001259                    WS-SORTED-TABLE
001260
001261     MOVE ' '                    TO WS-PDEF-RECORD-SW
001262
001263     MOVE CM-CONTROL-PRIMARY     TO ELCRTT-KEY
001264     MOVE 'B'                    TO CRTT-REC-TYPE
001265
001266     
      * EXEC CICS READ
001267*       DATASET   ('ELCRTT')
001268*       INTO      (CERTIFICATE-TRAILERS)
001269*       RIDFLD    (ELCRTT-KEY)
001270*       RESP      (WS-RESPONSE)
001271*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005536' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035353336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001272
001273     IF NOT RESP-NORMAL
001274        DISPLAY ' NSRLTRBL- NO TRLRS ' CL-CERT-NO
001275        GO TO 1800-EXIT
001276     END-IF
001277
001278     DISPLAY ' NSRLTRBL- GOOD READ ON ELCRTT '
001279     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001280       (S1 > +24)
001281       OR (CS-CLAIM-NO (S1) = SPACES)
001282        MOVE CS-CLAIM-NO (S1)    TO WS-CLAIM-NO (S1)
001283        MOVE CS-CLAIM-TYPE (S1)  TO WS-CLM-TYPE (S1)
001284        IF CS-INSURED-TYPE (S1) = 'C'
001285           MOVE '2'              TO WS-INS-TYPE (S1)
001286        ELSE
001287           MOVE '1'              TO WS-INS-TYPE (S1)
001288        END-IF
001289        MOVE CS-BENEFIT-PERIOD (S1)
001290                                 TO WS-BEN-PER (S1)
001291*       MOVE CS-DAYS-PAID (S1)   TO WS-DAYS-PAID (S1)
001292        MOVE CS-TOTAL-PAID (S1)  TO WS-TOTAL-PAID (S1)
001293        MOVE ' '                 TO WS-SORTED-SW (S1)
001294
001295        MOVE CS-COMPANY-CD          TO WS-ELMSTR-COMPANY-CD
001296        MOVE CS-CARRIER             TO WS-ELMSTR-CARRIER
001297        MOVE CS-CLAIM-NO (S1)       TO WS-ELMSTR-CLAIM-NO
001298        MOVE CS-CERT-NO             TO WS-ELMSTR-CERT-NO
001299
001300        
      * EXEC CICS READ
001301*          DATASET        ('ELMSTR')
001302*          RIDFLD         (WS-ELMSTR-KEY)
001303*          INTO           (CLAIM-MASTER)
001304*          RESP           (WS-RESPONSE)
001305*       END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005570' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035353730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 WS-ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001306        IF RESP-NORMAL
001307           MOVE CL-INCURRED-DT   TO WS-INC-DT (S1)
001308           MOVE CL-PAID-THRU-DT  TO WS-PD-THRU-DT (S1)
001309           MOVE CL-CLAIM-STATUS  TO WS-STATUS (S1)
001310           IF CL-CRITICAL-PERIOD NOT NUMERIC
001311              MOVE ZEROS TO CL-CRITICAL-PERIOD
001312           END-IF
001313           MOVE CL-CRITICAL-PERIOD
001314                                 TO WS-MAX-BENS (S1)
001315           IF CL-CRITICAL-PERIOD = ZEROS
001316              IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
001317                 MOVE 01         TO WS-MAX-BENS (S1)
001318              ELSE
001319                 MOVE CM-AH-ORIG-TERM
001320                                 TO WS-MAX-BENS (S1)
001321              END-IF
001322           END-IF
001323           IF CL-DENIAL-TYPE = '1' OR '2' OR '3' OR '4'
001324              MOVE 'D'           TO WS-STATUS (S1)
001325           END-IF
001326           IF CS-CLAIM-NO (S1) = BL-CLAIM-NO
001327              MOVE CL-ACCIDENT-CLAIM-SW TO OUT-ACC-SW
001328           END-IF
001329
001330           MOVE ' '                    TO WS-PDEF-RECORD-SW
001331           IF (AM-DCC-PRODUCT-CODE NOT = SPACES)
001332              PERFORM 1850-GET-DDF-LIMITS
001333                                 THRU 1850-EXIT
001334           END-IF
001335           IF PDEF-FOUND
001336              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
001337                 (PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
001338                 OR (P1 > +8)
001339              END-PERFORM
001340              IF P1 < +9
001341                 IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
001342                    IF (PD-MAX-AMT (P1) NOT = ZEROS)
001343                       AND (PD-MAX-AMT (P1) < CM-LF-BENEFIT-AMT)
001344                       MOVE PD-MAX-AMT (P1)
001345                                 TO WS-MAX-MOBEN (S1)
001346                    ELSE
001347                       MOVE CM-LF-BENEFIT-AMT
001348                                 TO WS-MAX-MOBEN (S1)
001349                    END-IF
001350                 ELSE
001351                    IF PD-BEN-PCT (P1) NOT NUMERIC
001352                       MOVE ZEROS   TO PD-BEN-PCT (P1)
001353                    END-IF
001354                    IF PD-BEN-PCT (P1) = ZEROS
001355                       MOVE +1      TO WS-WORK-BEN-PCT
001356                    ELSE
001357                       MOVE PD-BEN-PCT (P1)
001358                                    TO WS-WORK-BEN-PCT
001359                    END-IF
001360                    COMPUTE WS-MAX-MOBEN (S1) =
001361                       CM-AH-BENEFIT-AMT * WS-WORK-BEN-PCT
001362                    IF (PD-MAX-AMT (P1) NOT = ZEROS)
001363                       AND (PD-MAX-AMT (P1) < WS-MAX-MOBEN (S1))
001364                       MOVE PD-MAX-AMT (P1)
001365                                 TO WS-MAX-MOBEN (S1)
001366                    END-IF
001367                 END-IF
001368*                IF (PD-MAX-AMT (P1) NOT = ZEROS)
001369*                   AND (PD-MAX-AMT (P1) < CM-AH-BENEFIT-AMT)
001370*                   MOVE PD-MAX-AMT (P1) TO WS-MAX-MOBEN (S1)
001371*                ELSE
001372*                   MOVE CM-AH-BENEFIT-AMT TO WS-MAX-MOBEN (S1)
001373*                END-IF
001374                 MOVE PD-EXCLUSION-PERIOD-DAYS (P1)
001375                                 TO WS-EXCL-PER (S1)
001376                 MOVE PD-COVERAGE-ENDS-MOS (P1)
001377                                 TO WS-COV-ENDS (S1)
001378                 MOVE PD-ACCIDENT-ONLY-MOS (P1)
001379                                 TO WS-ACC-PER (S1)
001380                 MOVE PD-MAX-EXTENSION (P1)
001381                                 TO WS-MAX-EXTEN (S1)
001382                 EVALUATE TRUE
001383                    WHEN PD-RECURRING-YN (P1) = 'N'
001384                       MOVE 00   TO WS-REC-MOS (S1)
001385                    WHEN PD-RECURRING-YN (P1) = 'Y'
001386                       MOVE 99   TO WS-REC-MOS (S1)
001387                    WHEN PD-REC-CRIT-PERIOD (P1) NUMERIC
001388                       MOVE PD-REC-CRIT-PERIOD (P1)
001389                                 TO WS-REC-MOS (S1)
001390                    WHEN OTHER
001391                       MOVE ZEROS TO WS-REC-MOS (S1)
001392                 END-EVALUATE
001393                 IF WS-REC-MOS (S1) = ZEROS
001394                    MOVE 01      TO WS-REC-MOS (S1)
001395                 END-IF
001396                 DISPLAY ' NSRLTRBL- PDEF FOUND ' WS-EXCL-PER (S1)
001397              END-IF
001398           ELSE
001399              IF CL-CLAIM-TYPE NOT = 'L' AND 'P' AND 'O'
001400                 MOVE CM-AH-BENEFIT-AMT
001401                                 TO WS-MAX-MOBEN (S1)
001402              ELSE
001403                 MOVE ZEROS      TO WS-MAX-MOBEN (S1)
001404              END-IF
001405              MOVE ZEROS         TO WS-EXCL-PER (S1)
001406                                    WS-MAX-EXTEN (S1)
001407                                    WS-ACC-PER  (S1)
001408              MOVE 999           TO WS-COV-ENDS (S1)
001409              MOVE 01            TO WS-REC-MOS  (S1)
001410           END-IF
001411        END-IF
001412     END-PERFORM
001413
001414*      SORT THE TABLE
001415     MOVE HIGH-VALUES            TO WS-KEY (S1)
001416     COMPUTE WS-MAX-SUB = S1 - +1
001417     DISPLAY ' NSRLTRBL- JUST BUILT TABLE '   WS-MAX-SUB
001418
001419     MOVE WS-MAX-SUB             TO WS-CNTR
001420
001421     MOVE +1                     TO S2
001422     MOVE +0                     TO S3
001423     PERFORM UNTIL (WS-CNTR = ZERO) OR (S3 > 700)
001424        MOVE +0 TO WS-HOLD-S1
001425        MOVE HIGH-VALUES TO WS-HOLD-KEY
001426        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001427           (S1 > WS-MAX-SUB)
001428           IF (WS-SORTED-SW (S1) NOT = 'Y')
001429              AND (WS-KEY (S1) <= WS-HOLD-KEY)
001430              MOVE WS-KEY (S1)   TO WS-HOLD-KEY
001431              MOVE S1            TO WS-HOLD-S1
001432           END-IF
001433        END-PERFORM
001434        IF WS-HOLD-S1 NOT = ZEROS
001435           MOVE WS-HOLD-S1         TO S1
001436           MOVE WS-KEY        (S1) TO WS-SRTD-KEY        (S2)
001437           MOVE WS-STATUS     (S1) TO WS-SRTD-STATUS     (S2)
001438           MOVE WS-MAX-BENS   (S1) TO WS-SRTD-MAX-BENS   (S2)
001439           MOVE WS-EXCL-PER   (S1) TO WS-SRTD-EXCL-PER   (S2)
001440           MOVE WS-COV-ENDS   (S1) TO WS-SRTD-COV-ENDS   (S2)
001441           MOVE WS-ACC-PER    (S1) TO WS-SRTD-ACC-PER    (S2)
001442           MOVE WS-REC-MOS    (S1) TO WS-SRTD-REC-MOS    (S2)
001443           MOVE WS-MAX-EXTEN  (S1) TO WS-SRTD-MAX-EXTEN  (S2)
001444           MOVE WS-PD-THRU-DT (S1) TO WS-SRTD-PD-THRU-DT (S2)
001445           MOVE WS-CLAIM-NO   (S1) TO WS-SRTD-CLAIM-NO   (S2)
001446           MOVE WS-MAX-MOBEN  (S1) TO WS-SRTD-MAX-MOBEN  (S2)
001447           MOVE WS-TOTAL-PAID (S1) TO WS-SRTD-TOTAL-PAID (S2)
001448*          MOVE WS-REM-BENS   (S1) TO WS-SRTD-REM-BENS   (S2)
001449           MOVE 'Y'                TO WS-SORTED-SW       (S1)
001450           SUBTRACT 1 FROM WS-CNTR
001451           ADD +1 TO S2
001452        END-IF
001453        ADD 1 TO S3
001454     END-PERFORM
001455
001456     DISPLAY ' NSRLTRBL- JUST SORTED TABLE '
001457
001458***  -----------------------
001459     IF BL-COMP-ID NOT = 'DCC' AND 'VPP'
001460        PERFORM 1900-BUILD-NON-DCC
001461        GO TO 1800-EXIT
001462     END-IF
001463
001464     MOVE SPACES                 TO WS-PREV-CLM-TYPE
001465                                    WS-PREV-INS-TYPE
001466     MOVE ZEROS                  TO WS-PREV-BEN-PER
001467                                    WS-ACCUM-DAYS
001468                                    WS-ACCUM-AMT
001469                                    WS-ACCUM-PD-BENS
001470
001471     MOVE +1                     TO M1
001472     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001473        S1 > WS-MAX-SUB
001474        IF (WS-SRTD-CLM-TYPE (S1) = WS-PREV-CLM-TYPE)
001475           AND (WS-SRTD-BEN-PER (S1) = WS-PREV-BEN-PER)
001476           AND (WS-SRTD-INS-TYPE (S1) NOT = WS-PREV-INS-TYPE)
001477           MOVE ZEROS            TO WS-ACCUM-DAYS
001478                                    WS-ACCUM-AMT
001479                                    WS-ACCUM-PD-BENS
001480        END-IF
001481        MOVE WS-SRTD-INS-TYPE (S1) TO WS-PREV-INS-TYPE
001482        IF (WS-SRTD-CLM-TYPE (S1) = WS-PREV-CLM-TYPE)
001483           AND (WS-SRTD-BEN-PER (S1) NOT = WS-PREV-BEN-PER)
001484           MOVE ZEROS            TO WS-ACCUM-DAYS
001485                                    WS-ACCUM-AMT
001486                                    WS-ACCUM-PD-BENS
001487        END-IF
001488        MOVE WS-SRTD-BEN-PER (S1) TO WS-PREV-BEN-PER
001489        IF WS-SRTD-CLM-TYPE (S1) NOT = WS-PREV-CLM-TYPE
001490           PERFORM 1810-SET-NEW-HEAD
001491                                 THRU 1810-EXIT
001492           MOVE WS-SRTD-CLM-TYPE (S1)
001493                                 TO WS-PREV-CLM-TYPE
001494           ADD +1                TO M1
001495        END-IF
001496
001497        MOVE WS-SRTD-BEN-PER (S1)  TO OUT-BEN-PER
001498        IF WS-SRTD-INS-TYPE (S1) = '1'
001499           MOVE 'PRIM'             TO OUT-INS-TYP
001500        ELSE
001501           MOVE 'COBO'             TO OUT-INS-TYP
001502        END-IF
001503*       EVALUATE WS-SRTD-STATUS (S1)
001504*          WHEN 'C'
001505*             MOVE 'CLOSED'        TO WSM-STATUS (M1)
001506*          WHEN 'D'
001507*             MOVE 'DENIED'        TO WSM-STATUS (M1)
001508*          WHEN 'O'
001509*             MOVE 'OPEN'          TO WSM-STATUS (M1)
001510*          WHEN OTHER
001511*             MOVE 'OTHER'         TO WSM-STATUS (M1)
001512*       END-EVALUATE
001513*       MOVE WS-SRTD-CLAIM-NO (S1) TO WSM-CLAIM-NO (M1)
001514*                                     PI-WSM-CLAIM-NOS (S1)
001515*       MOVE WS-SRTD-TOTAL-PAID (S1) TO WSM-TOTAL-PAID (M1)
001516
001517        COMPUTE OUT-MAX-BENS =
001518           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
001519
001520        DISPLAY ' NSRLTRBL- MAX BENS 1 ' OUT-MAX-BENS ' '
001521           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
001522
001523        MOVE ZEROS TO WK1 WK2
001524        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
001525           COMPUTE WS-PD-BENS ROUNDED =
001526              WS-SRTD-TOTAL-PAID (S1) / WS-SRTD-MAX-MOBEN (S1)
001527           IF WS-SRTD-CLM-TYPE (S1) NOT = 'L' AND 'P'
001528              DIVIDE WS-SRTD-TOTAL-PAID (S1) BY
001529                 WS-SRTD-MAX-MOBEN(S1) GIVING WK1
001530                 REMAINDER WK2
001531           END-IF
001532        ELSE
001533           MOVE ZEROS            TO WS-PD-BENS
001534        END-IF
001535        IF (WS-PD-BENS = ZEROS)
001536           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
001537           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
001538           MOVE 1                TO WS-PD-BENS
001539        END-IF
001540        MOVE WS-PD-BENS          TO OUT-PAID-BENS
001541*       if wk2 not = zeros
001542*          move '*'              to wsm-part (m1)
001543*       end-if
001544*       compute ws-accum-days =
001545*          ws-accum-days + ws-srtd-days-paid (s1)
001546        COMPUTE WS-ACCUM-AMT =
001547           WS-ACCUM-AMT + WS-SRTD-TOTAL-PAID (S1)
001548        DISPLAY ' NSRLTRBL- COMPUTE A PD BENS ' M1 ' '
001549           WS-ACCUM-AMT ' ' WS-SRTD-MAX-MOBEN (S1)
001550        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
001551           COMPUTE WS-ACCUM-PD-BENS ROUNDED =
001552              WS-ACCUM-AMT / WS-SRTD-MAX-MOBEN (S1)
001553        ELSE
001554           MOVE ZEROS            TO WS-ACCUM-PD-BENS
001555        END-IF
001556        IF (WS-ACCUM-PD-BENS = ZEROS)
001557           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
001558           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
001559           MOVE 1                TO WS-ACCUM-PD-BENS
001560        END-IF
001561        DISPLAY ' NSRLTRBL- COMPUTE REM BENS ' M1 ' '
001562           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
001563        COMPUTE OUT-REM-BENS =
001564           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
001565        PERFORM 1820-FIND-QUALIFY THRU 1820-EXIT
001566
001567*       MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
001568*       MOVE ' '                    TO DC-OPTION-CODE
001569*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001570*       IF NO-CONVERSION-ERROR
001571*          MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
001572*       ELSE
001573*          MOVE SPACES              TO wsm-inc-date (m1)
001574*       END-IF
001575*       MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
001576*       MOVE ' '                     TO DC-OPTION-CODE
001577*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001578*       IF NO-CONVERSION-ERROR
001579*          MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
001580*       ELSE
001581*          MOVE SPACES              TO wsm-pd-thru-dt (m1)
001582*       END-IF
001583        ADD +1                   TO M1
001584        IF WS-SRTD-CLAIM-NO (S1) = BL-CLAIM-NO
001585           MOVE WS-SRTD-EXCL-PER (S1) TO OUT-EXCL-PER
001586           MOVE 26 TO S1
001587        END-IF
001588
001589     END-PERFORM
001590
001591*   DISPLAY ' NSRLTRBL- 1800 end MAX BENS '  OUT-MAX-BENS
001592*   display  ' PAID BENS ' OUT-PAID-BENS ' REM BENS ' OUT-REM-BENS
001593*   display  ' EXCL PER ' OUT-EXCL-PER ' REC MOS ' OUT-REC-MOS
001594*   display  ' INS TYP ' OUT-INS-TYP ' BEN PER ' OUT-BEN-PER
001595*   display  ' ACC SW ' OUT-ACC-SW
001596
001597     .
001598 1800-EXIT.
001599     EXIT.
001600
001601     EJECT
001602
001603 1810-SET-NEW-HEAD.
001604     MOVE ZEROS                  TO WS-ACCUM-DAYS
001605                                    WS-ACCUM-AMT
001606                                    WS-ACCUM-PD-BENS
001607     EVALUATE WS-SRTD-CLM-TYPE (S1)
001608        WHEN 'A'
001609           MOVE 'A&H'            TO WS-COV-TYPE
001610        WHEN 'F'
001611           MOVE 'FAM '           TO WS-COV-TYPE
001612        WHEN 'I'
001613           MOVE ' IU '           TO WS-COV-TYPE
001614        WHEN 'L'
001615           MOVE 'LIFE'           TO WS-COV-TYPE
001616        WHEN OTHER
001617           MOVE WS-SRTD-CLM-TYPE (S1)
001618                                 TO WS-COV-TYPE
001619     END-EVALUATE
001620
001621     MOVE WS-SRTD-REC-MOS (S1) TO OUT-REC-MOS
001622     MOVE WS-SRTD-EXCL-PER (S1) TO OUT-EXCL-PER
001623
001624*    string '  ' ws-cov-type ' ExPer ' OUT-EXCL-PER
001625*       ' CovEnd ' ws-srtd-cov-ends (s1)
001626*       ' MaxBens ' ws-srtd-max-bens (s1)
001627*       ' Recurring ' OUT-REC-MOS
001628*       delimited by size into ws-map-output (m1)
001629*    end-string
001630
001631     .
001632 1810-EXIT.
001633     EXIT.
001634
001635 1820-FIND-QUALIFY.
001636     IF NOT PDEF-FOUND
001637        GO TO 1820-EXIT
001638     END-IF
001639
001640*    DISPLAY ' NSRLTRBL- MAX BENS 2 ' OUT-MAX-BENS ' '
001641*       WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
001642
001643     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
001644     MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-2
001645     MOVE '1'                    TO DC-OPTION-CODE
001646     MOVE +0                     TO DC-ELAPSED-MONTHS
001647                                    DC-ELAPSED-DAYS
001648     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
001649
001650     IF NO-CONVERSION-ERROR
001651        DISPLAY ' NSRLTRBL- GOOD DATE CONVERT '
001652        MOVE DC-ELAPSED-MONTHS   TO WS-MONTHS-BETWEEN
001653        IF DC-ELAPSED-DAYS > 1
001654           ADD 1 TO WS-MONTHS-BETWEEN
001655        END-IF
001656     ELSE
001657        MOVE ZEROS               TO WS-MONTHS-BETWEEN
001658     END-IF
001659
001660*    DISPLAY ' NSRLTRBL- DISPLAY 3 ' OUT-EXCL-PER
001661*       ' ' WS-SRTD-COV-ENDS (S1) ' ' WS-MONTHS-BETWEEN
001662
001663     EVALUATE TRUE
001664        WHEN (OUT-EXCL-PER NOT = ZEROS)
001665           AND (WS-MONTHS-BETWEEN <= OUT-EXCL-PER)
001666           DISPLAY ' NSRLTRBL- ZERO 1 '
001667           MOVE ZEROS TO OUT-MAX-BENS
001668                         OUT-REM-BENS
001669        WHEN (WS-SRTD-COV-ENDS (S1) NOT = ZEROS)
001670           AND (WS-MONTHS-BETWEEN > WS-SRTD-COV-ENDS (S1))
001671           DISPLAY ' NSRLTRBL- ZERO 2 '
001672           MOVE ZEROS TO OUT-MAX-BENS
001673     END-EVALUATE
001674     IF OUT-REC-MOS < WS-SRTD-BEN-PER (S1)
001675        MOVE ZEROS               TO OUT-REM-BENS
001676     END-IF
001677
001678     .
001679 1820-EXIT.
001680     EXIT.
001681
001682 1850-GET-DDF-LIMITS.
001683
001684     IF CM-CLP-STATE = SPACES OR LOW-VALUES OR ZEROS
001685        MOVE CM-STATE            TO CM-CLP-STATE
001686     END-IF
001687
001688     MOVE CL-COMPANY-CD          TO ERPDEF-KEY
001689     MOVE CM-CLP-STATE           TO ERPDEF-STATE
001690     MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
001691
001692     IF (CL-CLAIM-TYPE = 'L' OR 'P' OR 'O')
001693        AND (CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD')
001694        MOVE 'L'                 TO ERPDEF-BEN-TYPE
001695        MOVE CM-LF-BENEFIT-CD    TO ERPDEF-BEN-CODE
001696     ELSE
001697        MOVE 'A'                 TO ERPDEF-BEN-TYPE
001698        MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
001699     END-IF
001700
001701     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
001702
001703*    DISPLAY ' NSRLTRBL- MADE 1850 ' ERPDEF-KEY (2:15)
001704     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
001705
001706     
      * EXEC CICS STARTBR
001707*        DATASET  ('ERPDEF')
001708*        RIDFLD   (ERPDEF-KEY)
001709*        GTEQ
001710*        RESP     (WS-RESPONSE)
001711*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005976' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035393736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001712
001713     IF NOT RESP-NORMAL
001714        GO TO 1850-EXIT
001715     END-IF
001716
001717     .
001718 1850-READNEXT.
001719
001720     
      * EXEC CICS READNEXT
001721*       DATASET  ('ERPDEF')
001722*       INTO     (PRODUCT-MASTER)
001723*       RIDFLD   (ERPDEF-KEY)
001724*       RESP     (WS-RESPONSE)
001725*    END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005990' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035393930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PRODUCT-MASTER, 
                 DFHEIV12, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001726
001727     IF NOT RESP-NORMAL
001728        GO TO 1850-ENDBR
001729     END-IF
001730
001731     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
001732        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
001733           MOVE 'Y'              TO WS-PDEF-RECORD-SW
001734           DISPLAY ' NSRLTRBL- SETTING PDEF FOUND TO TRUE '
001735        ELSE
001736           GO TO 1850-READNEXT
001737        END-IF
001738     ELSE
001739        GO TO 1850-ENDBR
001740     END-IF
001741
001742     .
001743 1850-ENDBR.
001744
001745     
      * EXEC CICS ENDBR
001746*       DATASET  ('ERPDEF')
001747*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006015' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036303135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001748
001749     .
001750 1850-EXIT.
001751     EXIT.
001752
001753 1900-BUILD-NON-DCC.
001754
001755     MOVE SPACES                 TO WS-PREV-CLM-TYPE
001756                                    WS-PREV-INS-TYPE
001757     MOVE ZEROS                  TO WS-PREV-BEN-PER
001758                                    WS-ACCUM-DAYS
001759                                    WS-ACCUM-AMT
001760                                    WS-ACCUM-PD-BENS
001761
001762     MOVE +1                     TO M1
001763     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
001764        S1 > WS-MAX-SUB
001765        IF WS-SRTD-CLM-TYPE (S1) NOT = WS-PREV-CLM-TYPE
001766           PERFORM 1810-SET-NEW-HEAD
001767                                 THRU 1810-EXIT
001768           MOVE WS-SRTD-CLM-TYPE (S1)
001769                                 TO WS-PREV-CLM-TYPE
001770           ADD +1                TO M1
001771        END-IF
001772        MOVE WS-SRTD-BEN-PER (S1)  TO OUT-BEN-PER
001773        IF WS-SRTD-INS-TYPE (S1) = '1'
001774           MOVE 'PRIM'             TO OUT-INS-TYP
001775        ELSE
001776           MOVE 'COBO'             TO OUT-INS-TYP
001777        END-IF
001778*       evaluate ws-srtd-status (s1)
001779*          when 'C'
001780*             MOVE 'CLOSED'        TO WSM-STATUS (M1)
001781*          WHEN 'D'
001782*             MOVE 'DENIED'        TO WSM-STATUS (M1)
001783*          WHEN 'O'
001784*             MOVE 'OPEN'          TO WSM-STATUS (M1)
001785*          WHEN OTHER
001786*             MOVE 'OTHER'         TO WSM-STATUS (M1)
001787*       END-EVALUATE
001788*       move ws-srtd-claim-no (s1) to wsm-claim-no (m1)
001789*                                     pi-wsm-claim-nos (s1)
001790*       move ws-srtd-total-paid (s1) to wsm-total-paid (m1)
001791
001792        COMPUTE OUT-MAX-BENS =
001793           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
001794
001795        DISPLAY ' NSRLTRBL- MAX BENS 1 ' OUT-MAX-BENS ' '
001796           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
001797
001798        MOVE ZEROS TO WK1 WK2
001799        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
001800           COMPUTE WS-PD-BENS ROUNDED =
001801              WS-SRTD-TOTAL-PAID (S1) / WS-SRTD-MAX-MOBEN (S1)
001802           IF WS-SRTD-CLM-TYPE (S1) NOT = 'L' AND 'P'
001803              DIVIDE WS-SRTD-TOTAL-PAID (S1) BY
001804                 WS-SRTD-MAX-MOBEN(S1) GIVING WK1
001805                 REMAINDER WK2
001806           END-IF
001807        ELSE
001808           MOVE ZEROS            TO WS-PD-BENS
001809        END-IF
001810        IF (WS-PD-BENS = ZEROS)
001811           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
001812           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
001813           MOVE 1                TO WS-PD-BENS
001814        END-IF
001815        MOVE WS-PD-BENS          TO OUT-PAID-BENS
001816*       IF WK2 NOT = ZEROS
001817*          MOVE '*'              TO WSM-PART (M1)
001818*       END-IF
001819        COMPUTE WS-ACCUM-AMT =
001820           WS-ACCUM-AMT + WS-SRTD-TOTAL-PAID (S1)
001821        DISPLAY ' NSRLTRBL- COMPUTE A PD BENS ' M1 ' '
001822           WS-ACCUM-AMT ' ' WS-SRTD-MAX-MOBEN (S1)
001823        IF WS-SRTD-MAX-MOBEN (S1) NOT = ZEROS
001824           COMPUTE WS-ACCUM-PD-BENS ROUNDED =
001825              WS-ACCUM-AMT / WS-SRTD-MAX-MOBEN (S1)
001826        ELSE
001827           MOVE ZEROS            TO WS-ACCUM-PD-BENS
001828        END-IF
001829        IF (WS-ACCUM-PD-BENS = ZEROS)
001830           AND (WS-SRTD-CLM-TYPE (S1) = 'L' OR 'P')
001831           AND (WS-SRTD-TOTAL-PAID (S1) > ZEROS)
001832           MOVE 1                TO WS-ACCUM-PD-BENS
001833        END-IF
001834        DISPLAY ' NSRLTRBL- COMPUTE REM BENS ' M1 ' '
001835           WS-SRTD-MAX-BENS (S1) ' ' WS-ACCUM-PD-BENS
001836        COMPUTE OUT-REM-BENS =
001837           WS-SRTD-MAX-BENS (S1) - WS-ACCUM-PD-BENS
001838
001839*       MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
001840*       MOVE ' '                    TO DC-OPTION-CODE
001841*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001842*       IF NO-CONVERSION-ERROR
001843*          MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
001844*       ELSE
001845*          MOVE SPACES              TO wsm-inc-date (m1)
001846*       END-IF
001847*       MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
001848*       MOVE ' '                     TO DC-OPTION-CODE
001849*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001850*       IF NO-CONVERSION-ERROR
001851*          MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
001852*       ELSE
001853*          MOVE SPACES              TO wsm-pd-thru-dt (m1)
001854*       END-IF
001855*       move WS-MAP-OUTPUT (m1)  to replineo (m1)
001856        IF WS-SRTD-CLAIM-NO (S1) = BL-CLAIM-NO
001857           MOVE WS-SRTD-EXCL-PER (S1) TO OUT-EXCL-PER
001858           MOVE 26 TO S1
001859        END-IF
001860
001861        ADD +1                   TO M1
001862     END-PERFORM
001863*    DISPLAY ' NSRLTRBL- 1900 end MAX BENS '  OUT-MAX-BENS
001864*    display ' PAID BENS ' OUT-PAID-BENS ' REM BENS ' OUT-REM-BENS
001865*    display ' EXCL PER ' OUT-EXCL-PER ' REC MOS ' OUT-REC-MOS
001866*    display ' INS TYP ' OUT-INS-TYP ' BEN PER ' OUT-BEN-PER
001867*    display ' ACC SW ' OUT-ACC-SW
001868
001869     .
001870 2000-BUILD-ELNAPS.
001871
001872     PERFORM 1500-GET-ARCH-NO    THRU 1500-EXIT
001873
001874     MOVE 'NA'                   TO NAPERSOFT-FILE
001875     MOVE CL-COMPANY-CD          TO NA-COMPANY-CD
001876     MOVE BL-CARRIER             TO NA-CARRIER
001877     MOVE BL-CLAIM-NO            TO NA-CLAIM-NO
001878     MOVE BL-CERT-NO             TO NA-CERT-NO
001879     MOVE WS-ARCHIVE-NO          TO NA-ARCHIVE-NO
001880                                    BL-ARCHIVE-NO
001881     MOVE BL-LETTER-ID           TO NA-LETTER-ID
001882     MOVE BL-NO-OF-COPIES        TO NA-NO-OF-COPIES
001883     MOVE SAVE-BIN-DATE          TO NA-CREATION-DT
001884                                    NA-INITIAL-PRINT-DT
001885
001886     MOVE LOW-VALUES             TO NA-FOLLOW-UP-DT
001887                                    NA-RESEND-DT
001888                                    NA-RESEND-PRINT-DT
001889                                    NA-1ST-LTR-PRINT-DT
001890                                    NA-NEXT-DUE-DT
001891                                    NA-AUTOPYDT
001892
001893     MOVE CL-TRAILER-SEQ-CNT     TO NA-CORR-TRLR-SEQ
001894
001895     IF BL-FOLLOW-UP-DT NOT = SPACES AND LOW-VALUES
001896        STRING BL-FOLLOW-UP-DT (7:2) BL-FOLLOW-UP-DT (1:2)
001897           BL-FOLLOW-UP-DT (4:2) DELIMITED BY SIZE
001898             INTO DC-GREG-DATE-1-YMD-R
001899        END-STRING
001900        MOVE '3'                 TO DC-OPTION-CODE
001901        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001902        IF NO-CONVERSION-ERROR
001903           MOVE DC-BIN-DATE-1    TO NA-FOLLOW-UP-DT
001904                                    WS-FOLLOW-UP-DT
001905        ELSE
001906           MOVE LOW-VALUES       TO NA-FOLLOW-UP-DT
001907                                    WS-FOLLOW-UP-DT
001908        END-IF
001909     END-IF
001910
001911     IF BL-RESEND-DT NOT = SPACES AND LOW-VALUES
001912        STRING BL-RESEND-DT (7:2) BL-RESEND-DT (1:2)
001913           BL-RESEND-DT (4:2) DELIMITED BY SIZE
001914             INTO DC-GREG-DATE-1-YMD-R
001915        END-STRING
001916        MOVE '3'                 TO DC-OPTION-CODE
001917        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
001918        IF NO-CONVERSION-ERROR
001919           MOVE DC-BIN-DATE-1    TO NA-RESEND-DT
001920                                    WS-RESEND-DT
001921        ELSE
001922           MOVE LOW-VALUES       TO NA-RESEND-DT
001923                                    WS-RESEND-DT
001924        END-IF
001925     END-IF
001926
001927     PERFORM WITH TEST AFTER UNTIL RESP-NORMAL
001928        
      * EXEC CICS WRITE
001929*          DATASET   ('ELNAPS')
001930*          FROM      (NAPERSOFT-FILE)
001931*          RIDFLD    (NA-CONTROL-PRIMARY)
001932*          RESP      (WS-RESPONSE)
001933*       END-EXEC
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
           MOVE 'ELNAPS' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00006198' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303036313938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 NA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001934        IF RESP-NORMAL
001935           CONTINUE
001936        ELSE
001937           IF RESP-DUPREC OR RESP-DUPKEY
001938              ADD +1 TO NA-ARCHIVE-NO
001939           ELSE
001940              DISPLAY ' BAD WRITE ELNAPS ' WS-RESPONSE
001941                 ' ' NA-CONTROL-PRIMARY (2:19)
001942              SET RESP-NORMAL TO TRUE
001943           END-IF
001944        END-IF
001945     END-PERFORM
001946
001947    .
001948 2000-EXIT.
001949     EXIT.
001950
001951 9700-DATE-LINK.
001952
001953     
      * EXEC CICS LINK
001954*        PROGRAM   ('ELDATCV')
001955*        COMMAREA  (DATE-CONVERSION-DATA)
001956*        LENGTH    (DC-COMM-LENGTH)
001957*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006223' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036323233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001958
001959
001960 9700-EXIT.
001961      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSRLTRBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'NSRLTRBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
