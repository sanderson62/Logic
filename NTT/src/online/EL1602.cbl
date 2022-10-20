      *((program: EL1602.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL1602.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 05/16/95 15:31:54.
000007*                            VMOD=2.007.
000008*
000009*
000010*AUTHOR.        LOGIC, INC.
000011*               DALLAS, TEXAS.
000012
000013*REMARKS. TRANSACTION EX34 - CLAIM DISPLAY.
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000023* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000024* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000025* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000026* 080322  CR2021100800003  TANA  Add B and H claim types
000027******************************************************************
000028     EJECT
000029 ENVIRONMENT DIVISION.
000030 DATA DIVISION.
000031 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000032 77  FILLER  PIC X(32)  VALUE '********************************'.
000033 77  FILLER  PIC X(32)  VALUE '*   EL1602 WORKING STORAGE     *'.
000034 77  FILLER  PIC X(32)  VALUE '********** VMOD=2.007 **********'.
000035
000036 01  LCP-TIME-OF-DAY-XX.
000037     05  LCP-TIME-OF-DAY-68        PIC 9(6).
000038     05  FILLER                    PIC 99.
000039 01  LCP-CICS-TIME                 PIC 9(15).
000040
000041 01  WS-TS-AREA.
000042     12  FILLER                  PIC X(145).
000043     12  WS-TS-CLAIM             PIC X(7).
000044     12  FILLER                  PIC X(3).
000045     12  WS-TS-TYPE              PIC X.
000046     12  FILLER                  PIC X(3).
000047     12  WS-TS-CERT              PIC X(10).
000048     12  FILLER                  PIC X(3).
000049     12  WS-TS-CERT-SFX          PIC X.
000050     12  FILLER                  PIC X(3).
000051     12  WS-TS-CARR              PIC X.
000052     12  FILLER                  PIC X(3).
000053     12  WS-TS-STATUS            PIC X.
000054     12  FILLER                  PIC X(10).
000055     12  WS-TS-FILE              PIC X(4).
000056     12  FILLER                  PIC X(3).
000057     12  WS-TS-CCN               PIC X(16).
000058     12  FILLER                  PIC X(3).
000059     12  WS-TS-LNAME             PIC X(15).
000060     12  FILLER                  PIC X(3).
000061     12  WS-TS-FNAME             PIC X(15).
000062     12  FILLER                  PIC X(3).
000063     12  WS-TS-MINIT             PIC X.
000064     12  FILLER                  PIC X(608).
000065
000066     EJECT
000067 01  WS-HEADING1.
000068     12  FILLER                  PIC X(24)   VALUE SPACES.
000069     12  WS-H1-TITLE             PIC X(18)   VALUE
000070         'CLAIM AUDIT REPORT'.
000071     12  FILLER                  PIC X(12)   VALUE SPACES.
000072     12  WS-H1-REPORT-NUMBER     PIC X(7)    VALUE 'EL -160'.
000073     12  FILLER                  PIC X(71)   VALUE SPACES.
000074
000075 01  WS-HEADING2.
000076     12  FILLER                  PIC X(5)    VALUE SPACES.
000077     12  FILLER                  PIC X(50)   VALUE
000078         'CAR CLAIM     CERT      TYPE   STATUS   FILE  NAME'.
000079     12  FILLER                  PIC X(77)   VALUE SPACES.
000080
000081 01  WS-DETAIL1.
000082     12  FILLER                  PIC X(6)    VALUE SPACES.
000083     12  WS-D1-CARR              PIC X       VALUE SPACES.
000084     12  FILLER                  PIC X       VALUE SPACES.
000085     12  WS-D1-CLAIM             PIC X(7)    VALUE SPACES.
000086     12  FILLER                  PIC X       VALUE SPACES.
000087     12  WS-D1-CERT.
000088         16  WS-D1-CERT-PRIME    PIC X(10)   VALUE SPACES.
000089         16  WS-D1-CERT-SFX      PIC X       VALUE SPACES.
000090     12  FILLER                  PIC X       VALUE SPACES.
000091     12  WS-D1-TYPE              PIC X(6)    VALUE SPACES.
000092     12  FILLER                  PIC XX      VALUE SPACES.
000093     12  WS-D1-STATUS            PIC X(6)    VALUE SPACES.
000094     12  FILLER                  PIC X(3)    VALUE SPACES.
000095     12  WS-D1-FILE              PIC X(4)   VALUE SPACES.
000096     12  FILLER                  PIC XX      VALUE SPACES.
000097     12  WS-D1-NAME              PIC X(30)   VALUE SPACES.
000098     12  FILLER                  PIC X(54)   VALUE SPACES.
000099
000100     EJECT
000101*                                COPY ELCREPT.
      *>>((file: ELCREPT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCREPT.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = REPORT STORAGE                            *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 146   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ELREPT                 RKP=2,LEN=11      *
000014*       ALTERNATE PATH  = NOT USED                               *
000015*                                                                *
000016*   LOG = NO                                                     *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019 01  REPORT-SAVE-FILE.
000020     12  RF-RECORD-ID                PIC XX.
000021         88  VALID-RF-ID                VALUE 'RF'.
000022
000023     12  RF-CONTROL-PRIMARY.
000024         16  RF-COMPANY-CD           PIC X.
000025         16  RF-RECORD-TYPE          PIC X.
000026             88  REPORT-DETAIL-RECORD   VALUE '1'.
000027             88  REPORT-TRAILER-RECORD  VALUE '2'.
000028         16  RF-REPORT-ID.
000029             20  RF-SYSTEM-CODE      PIC XX.
000030                 88  CLAS-IC-ONLINE     VALUE 'EL'.
000031                 88  CLAS-SCS-BATCH     VALUE 'EC'.
000032             20  RF-PROGRAM-SEQUENCE PIC 999.
000033         16  RF-LINE-NUMBER          PIC S9(8)       COMP.
000034     12  RF-REPORT-LINE-133.
000035         16  RF-CTL-CHAR-133         PIC X.
000036         16  RF-DATA-133             PIC X(132).
000037         16  RF-DATA-FIRST  REDEFINES RF-DATA-133.
000038             20  RF-DATA-2-81        PIC X(80).
000039             20  FILLER              PIC X(52).
000040         16  RF-DATA-LAST   REDEFINES RF-DATA-133.
000041             20  FILLER              PIC X(53).
000042             20  RF-DATA-55-133      PIC X(79).
000043     12  RF-TRAILER-RECORD  REDEFINES RF-REPORT-LINE-133.
000044         16  FILLER                  PIC X.
000045         16  RF-CURRENT-DATE         PIC X(8).
000046         16  RF-PRINT-HH-MM-SS       PIC X(6).
000047         16  FILLER                  PIC X(115).
000048         16  RF-COMPANY-ID           PIC XXX.
000049******************************************************************
      *<<((file: ELCREPT))
000102     EJECT
000103 01  WS-DATE-AREA.
000104     12  SAVE-DATE               PIC X(8)   VALUE SPACES.
000105     12  SAVE-BIN-DATE           PIC XX     VALUE SPACES.
000106
000107 01  LITERALS-NUMBERS.
000108     12  LIT-SP                  PIC XX      VALUE 'SP'.
000109     12  LIT-OB                  PIC XX      VALUE 'OB'.
000110     12  LIT-OE                  PIC XX      VALUE 'OE'.
000111     12  LIT-TYPE-L              PIC X(4)    VALUE 'LIFE'.
000112     12  LIT-TYPE-A              PIC X(4)    VALUE 'A/H'.
000113     12  LIT-CLOSED              PIC X(6)    VALUE 'CLOSED'.
000114     12  LIT-OPEN                PIC X(6)    VALUE ' OPEN'.
000115     12  LIT-SET-UP              PIC X(6)    VALUE 'SET UP'.
000116     12  LIT-PMT                 PIC X(6)    VALUE 'PAYMNT'.
000117     12  LIT-LETTER              PIC X(6)    VALUE 'LETTER'.
000118     12  LIT-UPDATE              PIC X(6)    VALUE 'UPDATE'.
000119     12  LIT-RESTORE             PIC X(6)    VALUE 'RESTOR'.
000120     12  LIT-INC-CHG             PIC X(6)    VALUE 'INC DT'.
000121     12  LIT-CONV                PIC X(6)    VALUE ' CONV'.
000122     12  LIT-SIGN-OFF            PIC X(8)    VALUE 'EL005'.
000123     12  LIT-HELP                PIC X(8)    VALUE 'EL010'.
000124     12  LIT-MASTER              PIC X(8)    VALUE 'EL126'.
000125     12  LIT-ACT                 PIC X(8)    VALUE 'EL142'.
000126     12  LIT-PROG                PIC X(8)    VALUE 'EL1602'.
000127     12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
000128     12  LIT-TRAN                PIC X(4)    VALUE 'EX34'.
000129     12  LIT-MAP                 PIC X(4)    VALUE '160B'.
000130     12  LIT-SCREEN              PIC X(4)    VALUE '160C'.
000131     12  NUM-ONE                 PIC 99      VALUE 1.
000132     12  NUM-TWENTY-FOUR         PIC 99      VALUE 24.
000133     12  START-TRANS-ID          PIC X(4)    VALUE 'EX58'.
000134     12  FILE-SWITCH             PIC X(4)    VALUE SPACES.
000135     12  WS-PRINT-SW             PIC S9      VALUE +0.
000136         88  PRINT-IN-PROCESS                VALUE +1.
000137     12  WS-FIRST-TIME-SW        PIC XX      VALUE LOW-VALUES.
000138         88  FIRST-TIME-THRU                 VALUE LOW-VALUES.
000139     12  REPT-FILE-ID            PIC X(8)   VALUE 'ELREPT  '.
000140     12  GETMAIN-SPACE           PIC X      VALUE SPACE.
000141     12  MAX-TS-PAGES            PIC 9999    VALUE 251.
000142     12  W-FILE-ID               PIC X(8)    VALUE 'ELMSTR'.
000143
000144 01  FILLER          COMP-3.
000145     12  WS-LINE-COUNT           PIC S9(3)       VALUE +99.
000146     12  WS-RECORD-COUNT         PIC S9(9)       VALUE ZERO.
000147     12  WS-LINE-NUMBER          PIC S9(4)       VALUE +0.
000148
000149     EJECT
000150 01  EDIT-WORK-AREA.
000151     12  CALL-PGM                PIC X(8).
000152     12  TRANS-ID                PIC X(4).
000153     12  CHECK-PFKEYS            PIC 99.
000154     12  CONV-COUNT              PIC 9(4).
000155     12  EDIT-COUNT              PIC ZZZ9.
000156     12  PI-KEY.
000157         16  CLAS-TERM           PIC X(4).
000158         16  CLAS-QUAL           PIC X(4).
000159     12  HOLD-PROC               PIC X(4).
000160     12  HOLD-PRI                PIC X.
000161     12  HOLD-SUPV               PIC X.
000162     12  HOLD-FILE               PIC X(4).
000163     12  DAYS-PAID               PIC ZZZZ9.
000164     12  PMTS-MADE               PIC ZZZZZ9.
000165     12  EDIT-DOLLARS-9          PIC ZZZZZZ.99.
000166
000167 01  WS-OLD-CLAIM-RECORD         PIC X(350).
000168
000169 01  CNTL-KEY.
000170     12  COMPANY-ID              PIC X(3).
000171     12  RECORD-TYPE             PIC X.
000172     12  CNTL-PROC               PIC X(4).
000173     12  SEQ-NO                  PIC 9(4)    COMP.
000174
000175 01  MSTR-KEY.
000176     12  MSTR-COMPANY-CODE       PIC X.
000177     12  MSTR-CARRIER            PIC X.
000178     12  MSTR-CLAIM-NO           PIC X(7).
000179     12  MSTR-CERT-NO            PIC X(11).
000180
000181 01  ELACTQ-KEY.
000182     12  ACTQ-COMP-CD            PIC X.
000183     12  ACTQ-CARRIER            PIC X.
000184     12  ACTQ-CLAIM-NO           PIC X(7).
000185     12  ACTQ-CERT-NO            PIC X(11).
000186
000187 01  TIME-UNFORMATTED.
000188     12  UN-HOURS                PIC XX.
000189     12  UN-MINUTES              PIC XX.
000190     12  FILLER                  PIC X(4).
000191
000192 01  TIME-FORMATTED.
000193     12  FOR-HOURS               PIC XX.
000194     12  FILLER                  PIC X       VALUE '.'.
000195     12  FOR-MINUTES             PIC XX.
000196     EJECT
000197 01  ERROR-NUMBERS.
000198     12  ER-0000                 PIC X(4)    VALUE '0000'.
000199     12  ER-0008                 PIC X(4)    VALUE '0008'.
000200     12  ER-0029                 PIC X(4)    VALUE '0029'.
000201     12  ER-0042                 PIC X(4)    VALUE '0042'.
000202     12  ER-0068                 PIC X(4)    VALUE '0068'.
000203     12  ER-0070                 PIC X(4)    VALUE '0070'.
000204     12  ER-0190                 PIC X(4)    VALUE '0190'.
000205     12  ER-0609                 PIC X(4)    VALUE '0609'.
000206     12  ER-0130                 PIC X(4)    VALUE '0130'.
000207     12  ER-0131                 PIC X(4)    VALUE '0131'.
000208     12  ER-0192                 PIC X(4)    VALUE '0192'.
000209     12  ER-0230                 PIC X(4)    VALUE '0230'.
000210     12  ER-0273                 PIC X(4)    VALUE '0273'.
000211     12  ER-0274                 PIC X(4)    VALUE '0274'.
000212     12  ER-0276                 PIC X(4)    VALUE '0276'.
000213     12  ER-0337                 PIC X(4)    VALUE '0337'.
000214     12  ER-0412                 PIC X(4)    VALUE '0412'.
000215     12  ER-0413                 PIC X(4)    VALUE '0413'.
000216     12  ER-0515                 PIC X(4)    VALUE '0515'.
000217     12  ER-0971                 PIC X(4)    VALUE '0971'.
000218     12  ER-0972                 PIC X(4)    VALUE '0972'.
000219     12  ER-2379                 PIC X(4)    VALUE '2379'.
000220
000221 01  ERROR-SWITCHES.
000222     12  ERROR-SWITCH            PIC X.
000223         88  SCREEN-ERROR                    VALUE 'X'.
000224     12  UPDATE-SWITCH           PIC X.
000225         88  NO-UPDATES                      VALUE SPACE.
000226     12  KEY-SWITCH              PIC X.
000227         88  KEY-CHANGE                      VALUE 'X'.
000228     12  WS-SAVE-PRINT-OPTION    PIC X       VALUE SPACE.
000229
000230 01  COMP-LENGTHS.
000231     12  LIT-IC                  PIC S9(4)   COMP VALUE -1.
000232     12  EL1602-LENGTH           PIC S9(4)   COMP VALUE +881.
000233     12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.
000234     12  WS-SAVE-TS-COUNT        PIC S9(4)   COMP VALUE +0.
000235     12  WS-TS-ITEM-NO           PIC S9(4)   COMP VALUE +0.
000236     EJECT
000237*                                COPY ELCATTR.
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
000238     EJECT
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
000240     EJECT
000241*                                COPY ELCLOGOF.
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
000242     EJECT
000243*                                COPY ELCMSTR.
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
000244     EJECT
000245*                                COPY ELCAID.
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
000246 01  FILLER REDEFINES DFHAID.
000247     12  FILLER                  PIC X(8).
000248     12  AID-KEYS OCCURS 24 TIMES.
000249         16  FILLER              PIC X.
000250     EJECT
000251*                                COPY EL160S.
      *>>((file: EL160S))
000001 01  EL160AI.
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
000016     05  CFILEIDL PIC S9(0004) COMP.
000017     05  CFILEIDF PIC  X(0001).
000018     05  FILLER REDEFINES CFILEIDF.
000019         10  CFILEIDA PIC  X(0001).
000020     05  CFILEIDI PIC  X(0001).
000021*    -------------------------------
000022     05  CARRSL PIC S9(0004) COMP.
000023     05  CARRSF PIC  X(0001).
000024     05  FILLER REDEFINES CARRSF.
000025         10  CARRSA PIC  X(0001).
000026     05  CARRSI PIC  X(0001).
000027*    -------------------------------
000028     05  INCLSL PIC S9(0004) COMP.
000029     05  INCLSF PIC  X(0001).
000030     05  FILLER REDEFINES INCLSF.
000031         10  INCLSA PIC  X(0001).
000032     05  INCLSI PIC  X(0008).
000033*    -------------------------------
000034     05  INCHSL PIC S9(0004) COMP.
000035     05  INCHSF PIC  X(0001).
000036     05  FILLER REDEFINES INCHSF.
000037         10  INCHSA PIC  X(0001).
000038     05  INCHSI PIC  X(0008).
000039*    -------------------------------
000040     05  GRPSL PIC S9(0004) COMP.
000041     05  GRPSF PIC  X(0001).
000042     05  FILLER REDEFINES GRPSF.
000043         10  GRPSA PIC  X(0001).
000044     05  GRPSI PIC  X(0006).
000045*    -------------------------------
000046     05  PMTDLSL PIC S9(0004) COMP.
000047     05  PMTDLSF PIC  X(0001).
000048     05  FILLER REDEFINES PMTDLSF.
000049         10  PMTDLSA PIC  X(0001).
000050     05  PMTDLSI PIC  X(0008).
000051*    -------------------------------
000052     05  PMTDHSL PIC S9(0004) COMP.
000053     05  PMTDHSF PIC  X(0001).
000054     05  FILLER REDEFINES PMTDHSF.
000055         10  PMTDHSA PIC  X(0001).
000056     05  PMTDHSI PIC  X(0008).
000057*    -------------------------------
000058     05  STATESL PIC S9(0004) COMP.
000059     05  STATESF PIC  X(0001).
000060     05  FILLER REDEFINES STATESF.
000061         10  STATESA PIC  X(0001).
000062     05  STATESI PIC  X(0002).
000063*    -------------------------------
000064     05  OPENLSL PIC S9(0004) COMP.
000065     05  OPENLSF PIC  X(0001).
000066     05  FILLER REDEFINES OPENLSF.
000067         10  OPENLSA PIC  X(0001).
000068     05  OPENLSI PIC  X(0003).
000069*    -------------------------------
000070     05  OPENHSL PIC S9(0004) COMP.
000071     05  OPENHSF PIC  X(0001).
000072     05  FILLER REDEFINES OPENHSF.
000073         10  OPENHSA PIC  X(0001).
000074     05  OPENHSI PIC  X(0003).
000075*    -------------------------------
000076     05  ACCTSL PIC S9(0004) COMP.
000077     05  ACCTSF PIC  X(0001).
000078     05  FILLER REDEFINES ACCTSF.
000079         10  ACCTSA PIC  X(0001).
000080     05  ACCTSI PIC  X(0010).
000081*    -------------------------------
000082     05  AMTLSL PIC S9(0004) COMP.
000083     05  AMTLSF PIC  X(0001).
000084     05  FILLER REDEFINES AMTLSF.
000085         10  AMTLSA PIC  X(0001).
000086     05  AMTLSI PIC  X(0010).
000087*    -------------------------------
000088     05  AMTHSL PIC S9(0004) COMP.
000089     05  AMTHSF PIC  X(0001).
000090     05  FILLER REDEFINES AMTHSF.
000091         10  AMTHSA PIC  X(0001).
000092     05  AMTHSI PIC  X(0010).
000093*    -------------------------------
000094     05  TYPESL PIC S9(0004) COMP.
000095     05  TYPESF PIC  X(0001).
000096     05  FILLER REDEFINES TYPESF.
000097         10  TYPESA PIC  X(0001).
000098     05  TYPESI PIC  X(0001).
000099*    -------------------------------
000100     05  CAUSELSL PIC S9(0004) COMP.
000101     05  CAUSELSF PIC  X(0001).
000102     05  FILLER REDEFINES CAUSELSF.
000103         10  CAUSELSA PIC  X(0001).
000104     05  CAUSELSI PIC  X(0006).
000105*    -------------------------------
000106     05  CAUSEHSL PIC S9(0004) COMP.
000107     05  CAUSEHSF PIC  X(0001).
000108     05  FILLER REDEFINES CAUSEHSF.
000109         10  CAUSEHSA PIC  X(0001).
000110     05  CAUSEHSI PIC  X(0006).
000111*    -------------------------------
000112     05  DENSL PIC S9(0004) COMP.
000113     05  DENSF PIC  X(0001).
000114     05  FILLER REDEFINES DENSF.
000115         10  DENSA PIC  X(0001).
000116     05  DENSI PIC  X(0001).
000117*    -------------------------------
000118     05  REPLSL PIC S9(0004) COMP.
000119     05  REPLSF PIC  X(0001).
000120     05  FILLER REDEFINES REPLSF.
000121         10  REPLSA PIC  X(0001).
000122     05  REPLSI PIC  X(0008).
000123*    -------------------------------
000124     05  REPHSL PIC S9(0004) COMP.
000125     05  REPHSF PIC  X(0001).
000126     05  FILLER REDEFINES REPHSF.
000127         10  REPHSA PIC  X(0001).
000128     05  REPHSI PIC  X(0008).
000129*    -------------------------------
000130     05  PROCSL PIC S9(0004) COMP.
000131     05  PROCSF PIC  X(0001).
000132     05  FILLER REDEFINES PROCSF.
000133         10  PROCSA PIC  X(0001).
000134     05  PROCSI PIC  X(0004).
000135*    -------------------------------
000136     05  PMTLSL PIC S9(0004) COMP.
000137     05  PMTLSF PIC  X(0001).
000138     05  FILLER REDEFINES PMTLSF.
000139         10  PMTLSA PIC  X(0001).
000140     05  PMTLSI PIC  X(0010).
000141*    -------------------------------
000142     05  PMTHSL PIC S9(0004) COMP.
000143     05  PMTHSF PIC  X(0001).
000144     05  FILLER REDEFINES PMTHSF.
000145         10  PMTHSA PIC  X(0001).
000146     05  PMTHSI PIC  X(0010).
000147*    -------------------------------
000148     05  PREMSL PIC S9(0004) COMP.
000149     05  PREMSF PIC  X(0001).
000150     05  FILLER REDEFINES PREMSF.
000151         10  PREMSA PIC  X(0001).
000152     05  PREMSI PIC  X(0001).
000153*    -------------------------------
000154     05  MNTLSL PIC S9(0004) COMP.
000155     05  MNTLSF PIC  X(0001).
000156     05  FILLER REDEFINES MNTLSF.
000157         10  MNTLSA PIC  X(0001).
000158     05  MNTLSI PIC  X(0008).
000159*    -------------------------------
000160     05  MNTHSL PIC S9(0004) COMP.
000161     05  MNTHSF PIC  X(0001).
000162     05  FILLER REDEFINES MNTHSF.
000163         10  MNTHSA PIC  X(0001).
000164     05  MNTHSI PIC  X(0008).
000165*    -------------------------------
000166     05  REQSL PIC S9(0004) COMP.
000167     05  REQSF PIC  X(0001).
000168     05  FILLER REDEFINES REQSF.
000169         10  REQSA PIC  X(0001).
000170     05  REQSI PIC  X(0001).
000171*    -------------------------------
000172     05  ESTLSL PIC S9(0004) COMP.
000173     05  ESTLSF PIC  X(0001).
000174     05  FILLER REDEFINES ESTLSF.
000175         10  ESTLSA PIC  X(0001).
000176     05  ESTLSI PIC  X(0008).
000177*    -------------------------------
000178     05  ESTHSL PIC S9(0004) COMP.
000179     05  ESTHSF PIC  X(0001).
000180     05  FILLER REDEFINES ESTHSF.
000181         10  ESTHSA PIC  X(0001).
000182     05  ESTHSI PIC  X(0008).
000183*    -------------------------------
000184     05  SUPRSL PIC S9(0004) COMP.
000185     05  SUPRSF PIC  X(0001).
000186     05  FILLER REDEFINES SUPRSF.
000187         10  SUPRSA PIC  X(0001).
000188     05  SUPRSI PIC  X(0001).
000189*    -------------------------------
000190     05  FOLLSL PIC S9(0004) COMP.
000191     05  FOLLSF PIC  X(0001).
000192     05  FILLER REDEFINES FOLLSF.
000193         10  FOLLSA PIC  X(0001).
000194     05  FOLLSI PIC  X(0008).
000195*    -------------------------------
000196     05  FOLHSL PIC S9(0004) COMP.
000197     05  FOLHSF PIC  X(0001).
000198     05  FILLER REDEFINES FOLHSF.
000199         10  FOLHSA PIC  X(0001).
000200     05  FOLHSI PIC  X(0008).
000201*    -------------------------------
000202     05  CERTSL PIC S9(0004) COMP.
000203     05  CERTSF PIC  X(0001).
000204     05  FILLER REDEFINES CERTSF.
000205         10  CERTSA PIC  X(0001).
000206     05  CERTSI PIC  X(0001).
000207*    -------------------------------
000208     05  DAYSLSL PIC S9(0004) COMP.
000209     05  DAYSLSF PIC  X(0001).
000210     05  FILLER REDEFINES DAYSLSF.
000211         10  DAYSLSA PIC  X(0001).
000212     05  DAYSLSI PIC  X(0003).
000213*    -------------------------------
000214     05  DAYSHSL PIC S9(0004) COMP.
000215     05  DAYSHSF PIC  X(0001).
000216     05  FILLER REDEFINES DAYSHSF.
000217         10  DAYSHSA PIC  X(0001).
000218     05  DAYSHSI PIC  X(0003).
000219*    -------------------------------
000220     05  PRISL PIC S9(0004) COMP.
000221     05  PRISF PIC  X(0001).
000222     05  FILLER REDEFINES PRISF.
000223         10  PRISA PIC  X(0001).
000224     05  PRISI PIC  X(0001).
000225*    -------------------------------
000226     05  AUTOSL PIC S9(0004) COMP.
000227     05  AUTOSF PIC  X(0001).
000228     05  FILLER REDEFINES AUTOSF.
000229         10  AUTOSA PIC  X(0001).
000230     05  AUTOSI PIC  X(0001).
000231*    -------------------------------
000232     05  PRTOPTL PIC S9(0004) COMP.
000233     05  PRTOPTF PIC  X(0001).
000234     05  FILLER REDEFINES PRTOPTF.
000235         10  PRTOPTA PIC  X(0001).
000236     05  PRTOPTI PIC  X(0001).
000237*    -------------------------------
000238     05  OPCLSL PIC S9(0004) COMP.
000239     05  OPCLSF PIC  X(0001).
000240     05  FILLER REDEFINES OPCLSF.
000241         10  OPCLSA PIC  X(0001).
000242     05  OPCLSI PIC  X(0001).
000243*    -------------------------------
000244     05  FMTOPTL PIC S9(0004) COMP.
000245     05  FMTOPTF PIC  X(0001).
000246     05  FILLER REDEFINES FMTOPTF.
000247         10  FMTOPTA PIC  X(0001).
000248     05  FMTOPTI PIC  X(0001).
000249*    -------------------------------
000250     05  ASEXL PIC S9(0004) COMP.
000251     05  ASEXF PIC  X(0001).
000252     05  FILLER REDEFINES ASEXF.
000253         10  ASEXA PIC  X(0001).
000254     05  ASEXI PIC  X(0001).
000255*    -------------------------------
000256     05  ALTPRTL PIC S9(0004) COMP.
000257     05  ALTPRTF PIC  X(0001).
000258     05  FILLER REDEFINES ALTPRTF.
000259         10  ALTPRTA PIC  X(0001).
000260     05  ALTPRTI PIC  X(0004).
000261*    -------------------------------
000262     05  MSG1L PIC S9(0004) COMP.
000263     05  MSG1F PIC  X(0001).
000264     05  FILLER REDEFINES MSG1F.
000265         10  MSG1A PIC  X(0001).
000266     05  MSG1I PIC  X(0075).
000267*    -------------------------------
000268     05  MSG2L PIC S9(0004) COMP.
000269     05  MSG2F PIC  X(0001).
000270     05  FILLER REDEFINES MSG2F.
000271         10  MSG2A PIC  X(0001).
000272     05  MSG2I PIC  X(0075).
000273*    -------------------------------
000274     05  PFKEYL PIC S9(0004) COMP.
000275     05  PFKEYF PIC  X(0001).
000276     05  FILLER REDEFINES PFKEYF.
000277         10  PFKEYA PIC  X(0001).
000278     05  PFKEYI PIC  X(0002).
000279 01  EL160AO REDEFINES EL160AI.
000280     05  FILLER            PIC  X(0012).
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  DATEO PIC  X(0008).
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  TIMEO PIC  X(0005).
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  CFILEIDO PIC  X(0001).
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  CARRSO PIC  X(0001).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  INCLSO PIC  X(0008).
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  INCHSO PIC  X(0008).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  GRPSO PIC  X(0006).
000302*    -------------------------------
000303     05  FILLER            PIC  X(0003).
000304     05  PMTDLSO PIC  X(0008).
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  PMTDHSO PIC  X(0008).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  STATESO PIC  X(0002).
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  OPENLSO PIC  X(0003).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  OPENHSO PIC  X(0003).
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  ACCTSO PIC  X(0010).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  AMTLSO PIC  Z(7).99.
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  AMTHSO PIC  Z(7).99.
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  TYPESO PIC  X(0001).
000329*    -------------------------------
000330     05  FILLER            PIC  X(0003).
000331     05  CAUSELSO PIC  X(0006).
000332*    -------------------------------
000333     05  FILLER            PIC  X(0003).
000334     05  CAUSEHSO PIC  X(0006).
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  DENSO PIC  X(0001).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  REPLSO PIC  X(0008).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  REPHSO PIC  X(0008).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  PROCSO PIC  X(0004).
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  PMTLSO PIC  Z(7).99.
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  PMTHSO PIC  Z(7).99.
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  PREMSO PIC  X(0001).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  MNTLSO PIC  X(0008).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  MNTHSO PIC  X(0008).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  REQSO PIC  X(0001).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  ESTLSO PIC  X(0008).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  ESTHSO PIC  X(0008).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  SUPRSO PIC  X(0001).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  FOLLSO PIC  X(0008).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  FOLHSO PIC  X(0008).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  CERTSO PIC  X(0001).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  DAYSLSO PIC  X(0003).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  DAYSHSO PIC  X(0003).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  PRISO PIC  X(0001).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  AUTOSO PIC  X(0001).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  PRTOPTO PIC  X(0001).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  OPCLSO PIC  X(0001).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  FMTOPTO PIC  X(0001).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  ASEXO PIC  X(0001).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  ALTPRTO PIC  X(0004).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  MSG1O PIC  X(0075).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  MSG2O PIC  X(0075).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  PFKEYO PIC  X(0002).
000419*    -------------------------------
000420 01  EL160BI.
000421     05  FILLER            PIC  X(0012).
000422*    -------------------------------
000423     05  DATEBL PIC S9(0004) COMP.
000424     05  DATEBF PIC  X(0001).
000425     05  FILLER REDEFINES DATEBF.
000426         10  DATEBA PIC  X(0001).
000427     05  DATEBI PIC  X(0008).
000428*    -------------------------------
000429     05  TIMEBL PIC S9(0004) COMP.
000430     05  TIMEBF PIC  X(0001).
000431     05  FILLER REDEFINES TIMEBF.
000432         10  TIMEBA PIC  X(0001).
000433     05  TIMEBI PIC  X(0005).
000434*    -------------------------------
000435     05  TITLEL PIC S9(0004) COMP.
000436     05  TITLEF PIC  X(0001).
000437     05  FILLER REDEFINES TITLEF.
000438         10  TITLEA PIC  X(0001).
000439     05  TITLEI PIC  X(0028).
000440*    -------------------------------
000441     05  PIKEYL PIC S9(0004) COMP.
000442     05  PIKEYF PIC  X(0001).
000443     05  FILLER REDEFINES PIKEYF.
000444         10  PIKEYA PIC  X(0001).
000445     05  PIKEYI PIC  X(0039).
000446*    -------------------------------
000447     05  SCNERRL PIC S9(0004) COMP.
000448     05  SCNERRF PIC  X(0001).
000449     05  FILLER REDEFINES SCNERRF.
000450         10  SCNERRA PIC  X(0001).
000451     05  SCNERRI PIC  X(0004).
000452*    -------------------------------
000453     05  USERSAVL PIC S9(0004) COMP.
000454     05  USERSAVF PIC  X(0001).
000455     05  FILLER REDEFINES USERSAVF.
000456         10  USERSAVA PIC  X(0001).
000457     05  USERSAVI PIC  X(0004).
000458*    -------------------------------
000459     05  TIMESAVL PIC S9(0004) COMP.
000460     05  TIMESAVF PIC  X(0001).
000461     05  FILLER REDEFINES TIMESAVF.
000462         10  TIMESAVA PIC  X(0001).
000463     05  TIMESAVI PIC  9(07).
000464*    -------------------------------
000465     05  NOSCRNL PIC S9(0004) COMP.
000466     05  NOSCRNF PIC  X(0001).
000467     05  FILLER REDEFINES NOSCRNF.
000468         10  NOSCRNA PIC  X(0001).
000469     05  NOSCRNI PIC  9999.
000470*    -------------------------------
000471     05  TOTSCRNL PIC S9(0004) COMP.
000472     05  TOTSCRNF PIC  X(0001).
000473     05  FILLER REDEFINES TOTSCRNF.
000474         10  TOTSCRNA PIC  X(0001).
000475     05  TOTSCRNI PIC  X(0004).
000476*    -------------------------------
000477     05  CLAIML PIC S9(0004) COMP.
000478     05  CLAIMF PIC  X(0001).
000479     05  FILLER REDEFINES CLAIMF.
000480         10  CLAIMA PIC  X(0001).
000481     05  CLAIMI PIC  X(0007).
000482*    -------------------------------
000483     05  TYPEL PIC S9(0004) COMP.
000484     05  TYPEF PIC  X(0001).
000485     05  FILLER REDEFINES TYPEF.
000486         10  TYPEA PIC  X(0001).
000487     05  TYPEI PIC  X(0001).
000488*    -------------------------------
000489     05  CERTL PIC S9(0004) COMP.
000490     05  CERTF PIC  X(0001).
000491     05  FILLER REDEFINES CERTF.
000492         10  CERTA PIC  X(0001).
000493     05  CERTI PIC  X(0010).
000494*    -------------------------------
000495     05  CERTSXL PIC S9(0004) COMP.
000496     05  CERTSXF PIC  X(0001).
000497     05  FILLER REDEFINES CERTSXF.
000498         10  CERTSXA PIC  X(0001).
000499     05  CERTSXI PIC  X(0001).
000500*    -------------------------------
000501     05  CARRL PIC S9(0004) COMP.
000502     05  CARRF PIC  X(0001).
000503     05  FILLER REDEFINES CARRF.
000504         10  CARRA PIC  X(0001).
000505     05  CARRI PIC  X(0001).
000506*    -------------------------------
000507     05  STATUSL PIC S9(0004) COMP.
000508     05  STATUSF PIC  X(0001).
000509     05  FILLER REDEFINES STATUSF.
000510         10  STATUSA PIC  X(0001).
000511     05  STATUSI PIC  X(0001).
000512*    -------------------------------
000513     05  PROCL PIC S9(0004) COMP.
000514     05  PROCF PIC  X(0001).
000515     05  FILLER REDEFINES PROCF.
000516         10  PROCA PIC  X(0001).
000517     05  PROCI PIC  X(0004).
000518*    -------------------------------
000519     05  FILEL PIC S9(0004) COMP.
000520     05  FILEF PIC  X(0001).
000521     05  FILLER REDEFINES FILEF.
000522         10  FILEA PIC  X(0001).
000523     05  FILEI PIC  X(0004).
000524*    -------------------------------
000525     05  CREDCDL PIC S9(0004) COMP.
000526     05  CREDCDF PIC  X(0001).
000527     05  FILLER REDEFINES CREDCDF.
000528         10  CREDCDA PIC  X(0001).
000529     05  CREDCDI PIC  X(0016).
000530*    -------------------------------
000531     05  MLNAMEL PIC S9(0004) COMP.
000532     05  MLNAMEF PIC  X(0001).
000533     05  FILLER REDEFINES MLNAMEF.
000534         10  MLNAMEA PIC  X(0001).
000535     05  MLNAMEI PIC  X(0015).
000536*    -------------------------------
000537     05  MFNAMEL PIC S9(0004) COMP.
000538     05  MFNAMEF PIC  X(0001).
000539     05  FILLER REDEFINES MFNAMEF.
000540         10  MFNAMEA PIC  X(0001).
000541     05  MFNAMEI PIC  X(0015).
000542*    -------------------------------
000543     05  MMINITL PIC S9(0004) COMP.
000544     05  MMINITF PIC  X(0001).
000545     05  FILLER REDEFINES MMINITF.
000546         10  MMINITA PIC  X(0001).
000547     05  MMINITI PIC  X(0001).
000548*    -------------------------------
000549     05  SEXL PIC S9(0004) COMP.
000550     05  SEXF PIC  X(0001).
000551     05  FILLER REDEFINES SEXF.
000552         10  SEXA PIC  X(0001).
000553     05  SEXI PIC  X(0001).
000554*    -------------------------------
000555     05  BIRTHL PIC S9(0004) COMP.
000556     05  BIRTHF PIC  X(0001).
000557     05  FILLER REDEFINES BIRTHF.
000558         10  BIRTHA PIC  X(0001).
000559     05  BIRTHI PIC  X(0008).
000560*    -------------------------------
000561     05  SOCIALL PIC S9(0004) COMP.
000562     05  SOCIALF PIC  X(0001).
000563     05  FILLER REDEFINES SOCIALF.
000564         10  SOCIALA PIC  X(0001).
000565     05  SOCIALI PIC  X(0011).
000566*    -------------------------------
000567     05  OCCL PIC S9(0004) COMP.
000568     05  OCCF PIC  X(0001).
000569     05  FILLER REDEFINES OCCF.
000570         10  OCCA PIC  X(0001).
000571     05  OCCI PIC  X(0006).
000572*    -------------------------------
000573     05  CBENEL PIC S9(0004) COMP.
000574     05  CBENEF PIC  X(0001).
000575     05  FILLER REDEFINES CBENEF.
000576         10  CBENEA PIC  X(0001).
000577     05  CBENEI PIC  X(0010).
000578*    -------------------------------
000579     05  BHEADL PIC S9(0004) COMP.
000580     05  BHEADF PIC  X(0001).
000581     05  FILLER REDEFINES BHEADF.
000582         10  BHEADA PIC  X(0001).
000583     05  BHEADI PIC  X(0029).
000584*    -------------------------------
000585     05  CAUSEL PIC S9(0004) COMP.
000586     05  CAUSEF PIC  X(0001).
000587     05  FILLER REDEFINES CAUSEF.
000588         10  CAUSEA PIC  X(0001).
000589     05  CAUSEI PIC  X(0026).
000590*    -------------------------------
000591     05  CCAUSCDL PIC S9(0004) COMP.
000592     05  CCAUSCDF PIC  X(0001).
000593     05  FILLER REDEFINES CCAUSCDF.
000594         10  CCAUSCDA PIC  X(0001).
000595     05  CCAUSCDI PIC  X(0006).
000596*    -------------------------------
000597     05  ENDL PIC S9(0004) COMP.
000598     05  ENDF PIC  X(0001).
000599     05  FILLER REDEFINES ENDF.
000600         10  ENDA PIC  X(0001).
000601     05  ENDI PIC  X(0008).
000602*    -------------------------------
000603     05  PDTHRUL PIC S9(0004) COMP.
000604     05  PDTHRUF PIC  X(0001).
000605     05  FILLER REDEFINES PDTHRUF.
000606         10  PDTHRUA PIC  X(0001).
000607     05  PDTHRUI PIC  X(0008).
000608*    -------------------------------
000609     05  PDAMTL PIC S9(0004) COMP.
000610     05  PDAMTF PIC  X(0001).
000611     05  FILLER REDEFINES PDAMTF.
000612         10  PDAMTA PIC  X(0001).
000613     05  PDAMTI PIC  9(7)V99.
000614*    -------------------------------
000615     05  NODAYSL PIC S9(0004) COMP.
000616     05  NODAYSF PIC  X(0001).
000617     05  FILLER REDEFINES NODAYSF.
000618         10  NODAYSA PIC  X(0001).
000619     05  NODAYSI PIC  9(5).
000620*    -------------------------------
000621     05  NOPMTSL PIC S9(0004) COMP.
000622     05  NOPMTSF PIC  X(0001).
000623     05  FILLER REDEFINES NOPMTSF.
000624         10  NOPMTSA PIC  X(0001).
000625     05  NOPMTSI PIC  9(4).
000626*    -------------------------------
000627     05  INCL PIC S9(0004) COMP.
000628     05  INCF PIC  X(0001).
000629     05  FILLER REDEFINES INCF.
000630         10  INCA PIC  X(0001).
000631     05  INCI PIC  X(0008).
000632*    -------------------------------
000633     05  REPL PIC S9(0004) COMP.
000634     05  REPF PIC  X(0001).
000635     05  FILLER REDEFINES REPF.
000636         10  REPA PIC  X(0001).
000637     05  REPI PIC  X(0008).
000638*    -------------------------------
000639     05  ESTL PIC S9(0004) COMP.
000640     05  ESTF PIC  X(0001).
000641     05  FILLER REDEFINES ESTF.
000642         10  ESTA PIC  X(0001).
000643     05  ESTI PIC  X(0008).
000644*    -------------------------------
000645     05  MNTDTL PIC S9(0004) COMP.
000646     05  MNTDTF PIC  X(0001).
000647     05  FILLER REDEFINES MNTDTF.
000648         10  MNTDTA PIC  X(0001).
000649     05  MNTDTI PIC  X(0008).
000650*    -------------------------------
000651     05  MNTTYPEL PIC S9(0004) COMP.
000652     05  MNTTYPEF PIC  X(0001).
000653     05  FILLER REDEFINES MNTTYPEF.
000654         10  MNTTYPEA PIC  X(0001).
000655     05  MNTTYPEI PIC  X(0006).
000656*    -------------------------------
000657     05  PRICDL PIC S9(0004) COMP.
000658     05  PRICDF PIC  X(0001).
000659     05  FILLER REDEFINES PRICDF.
000660         10  PRICDA PIC  X(0001).
000661     05  PRICDI PIC  X(0001).
000662*    -------------------------------
000663     05  SUPVL PIC S9(0004) COMP.
000664     05  SUPVF PIC  X(0001).
000665     05  FILLER REDEFINES SUPVF.
000666         10  SUPVA PIC  X(0001).
000667     05  SUPVI PIC  X(0001).
000668*    -------------------------------
000669     05  LOANNOL PIC S9(0004) COMP.
000670     05  LOANNOF PIC  X(0001).
000671     05  FILLER REDEFINES LOANNOF.
000672         10  LOANNOA PIC  X(0001).
000673     05  LOANNOI PIC  X(0008).
000674*    -------------------------------
000675     05  LOANBALL PIC S9(0004) COMP.
000676     05  LOANBALF PIC  X(0001).
000677     05  FILLER REDEFINES LOANBALF.
000678         10  LOANBALA PIC  X(0001).
000679     05  LOANBALI PIC  9(10)V99.
000680*    -------------------------------
000681     05  CERTEFFL PIC S9(0004) COMP.
000682     05  CERTEFFF PIC  X(0001).
000683     05  FILLER REDEFINES CERTEFFF.
000684         10  CERTEFFA PIC  X(0001).
000685     05  CERTEFFI PIC  X(0008).
000686*    -------------------------------
000687     05  CERTACTL PIC S9(0004) COMP.
000688     05  CERTACTF PIC  X(0001).
000689     05  FILLER REDEFINES CERTACTF.
000690         10  CERTACTA PIC  X(0001).
000691     05  CERTACTI PIC  X(0010).
000692*    -------------------------------
000693     05  CERTSTL PIC S9(0004) COMP.
000694     05  CERTSTF PIC  X(0001).
000695     05  FILLER REDEFINES CERTSTF.
000696         10  CERTSTA PIC  X(0001).
000697     05  CERTSTI PIC  X(0002).
000698*    -------------------------------
000699     05  CERTCARL PIC S9(0004) COMP.
000700     05  CERTCARF PIC  X(0001).
000701     05  FILLER REDEFINES CERTCARF.
000702         10  CERTCARA PIC  X(0001).
000703     05  CERTCARI PIC  X(0001).
000704*    -------------------------------
000705     05  CERTGRPL PIC S9(0004) COMP.
000706     05  CERTGRPF PIC  X(0001).
000707     05  FILLER REDEFINES CERTGRPF.
000708         10  CERTGRPA PIC  X(0001).
000709     05  CERTGRPI PIC  X(0006).
000710*    -------------------------------
000711     05  SOCSECL PIC S9(0004) COMP.
000712     05  SOCSECF PIC  X(0001).
000713     05  FILLER REDEFINES SOCSECF.
000714         10  SOCSECA PIC  X(0001).
000715     05  SOCSECI PIC  X(0011).
000716*    -------------------------------
000717     05  CLNAMEL PIC S9(0004) COMP.
000718     05  CLNAMEF PIC  X(0001).
000719     05  FILLER REDEFINES CLNAMEF.
000720         10  CLNAMEA PIC  X(0001).
000721     05  CLNAMEI PIC  X(0015).
000722*    -------------------------------
000723     05  CFNAMEL PIC S9(0004) COMP.
000724     05  CFNAMEF PIC  X(0001).
000725     05  FILLER REDEFINES CFNAMEF.
000726         10  CFNAMEA PIC  X(0001).
000727     05  CFNAMEI PIC  X(0010).
000728*    -------------------------------
000729     05  CINITL PIC S9(0004) COMP.
000730     05  CINITF PIC  X(0001).
000731     05  FILLER REDEFINES CINITF.
000732         10  CINITA PIC  X(0001).
000733     05  CINITI PIC  X(0001).
000734*    -------------------------------
000735     05  INSAGEL PIC S9(0004) COMP.
000736     05  INSAGEF PIC  X(0001).
000737     05  FILLER REDEFINES INSAGEF.
000738         10  INSAGEA PIC  X(0001).
000739     05  INSAGEI PIC  X(0002).
000740*    -------------------------------
000741     05  CJLNAMEL PIC S9(0004) COMP.
000742     05  CJLNAMEF PIC  X(0001).
000743     05  FILLER REDEFINES CJLNAMEF.
000744         10  CJLNAMEA PIC  X(0001).
000745     05  CJLNAMEI PIC  X(0015).
000746*    -------------------------------
000747     05  CJFAMEL PIC S9(0004) COMP.
000748     05  CJFAMEF PIC  X(0001).
000749     05  FILLER REDEFINES CJFAMEF.
000750         10  CJFAMEA PIC  X(0001).
000751     05  CJFAMEI PIC  X(0010).
000752*    -------------------------------
000753     05  CJINITL PIC S9(0004) COMP.
000754     05  CJINITF PIC  X(0001).
000755     05  FILLER REDEFINES CJINITF.
000756         10  CJINITA PIC  X(0001).
000757     05  CJINITI PIC  X(0001).
000758*    -------------------------------
000759     05  JAGEL PIC S9(0004) COMP.
000760     05  JAGEF PIC  X(0001).
000761     05  FILLER REDEFINES JAGEF.
000762         10  JAGEA PIC  X(0001).
000763     05  JAGEI PIC  X(0002).
000764*    -------------------------------
000765     05  CVDESCRL PIC S9(0004) COMP.
000766     05  CVDESCRF PIC  X(0001).
000767     05  FILLER REDEFINES CVDESCRF.
000768         10  CVDESCRA PIC  X(0001).
000769     05  CVDESCRI PIC  X(0006).
000770*    -------------------------------
000771     05  CVKINDL PIC S9(0004) COMP.
000772     05  CVKINDF PIC  X(0001).
000773     05  FILLER REDEFINES CVKINDF.
000774         10  CVKINDA PIC  X(0001).
000775     05  CVKINDI PIC  X(0003).
000776*    -------------------------------
000777     05  CVCDL PIC S9(0004) COMP.
000778     05  CVCDF PIC  X(0001).
000779     05  FILLER REDEFINES CVCDF.
000780         10  CVCDA PIC  X(0001).
000781     05  CVCDI PIC  X(0002).
000782*    -------------------------------
000783     05  CVOTRML PIC S9(0004) COMP.
000784     05  CVOTRMF PIC  X(0001).
000785     05  FILLER REDEFINES CVOTRMF.
000786         10  CVOTRMA PIC  X(0001).
000787     05  CVOTRMI PIC  X(0003).
000788*    -------------------------------
000789     05  CVRTRML PIC S9(0004) COMP.
000790     05  CVRTRMF PIC  X(0001).
000791     05  FILLER REDEFINES CVRTRMF.
000792         10  CVRTRMA PIC  X(0001).
000793     05  CVRTRMI PIC  X(0003).
000794*    -------------------------------
000795     05  CVOBENEL PIC S9(0004) COMP.
000796     05  CVOBENEF PIC  X(0001).
000797     05  FILLER REDEFINES CVOBENEF.
000798         10  CVOBENEA PIC  X(0001).
000799     05  CVOBENEI PIC  9(9)V99.
000800*    -------------------------------
000801     05  CVFORML PIC S9(0004) COMP.
000802     05  CVFORMF PIC  X(0001).
000803     05  FILLER REDEFINES CVFORMF.
000804         10  CVFORMA PIC  X(0001).
000805     05  CVFORMI PIC  X(0012).
000806*    -------------------------------
000807     05  CVCNCDTL PIC S9(0004) COMP.
000808     05  CVCNCDTF PIC  X(0001).
000809     05  FILLER REDEFINES CVCNCDTF.
000810         10  CVCNCDTA PIC  X(0001).
000811     05  CVCNCDTI PIC  X(0008).
000812*    -------------------------------
000813     05  CVEXITL PIC S9(0004) COMP.
000814     05  CVEXITF PIC  X(0001).
000815     05  FILLER REDEFINES CVEXITF.
000816         10  CVEXITA PIC  X(0001).
000817     05  CVEXITI PIC  X(0008).
000818*    -------------------------------
000819     05  CVSTATL PIC S9(0004) COMP.
000820     05  CVSTATF PIC  X(0001).
000821     05  FILLER REDEFINES CVSTATF.
000822         10  CVSTATA PIC  X(0001).
000823     05  CVSTATI PIC  X(0006).
000824*    -------------------------------
000825     05  CMEMCAPL PIC S9(0004) COMP.
000826     05  CMEMCAPF PIC  X(0001).
000827     05  FILLER REDEFINES CMEMCAPF.
000828         10  CMEMCAPA PIC  X(0001).
000829     05  CMEMCAPI PIC  X(0010).
000830*    -------------------------------
000831     05  CAPRL PIC S9(0004) COMP.
000832     05  CAPRF PIC  X(0001).
000833     05  FILLER REDEFINES CAPRF.
000834         10  CAPRA PIC  X(0001).
000835     05  CAPRI PIC  9(4)V9(4).
000836*    -------------------------------
000837     05  CPFREQL PIC S9(0004) COMP.
000838     05  CPFREQF PIC  X(0001).
000839     05  FILLER REDEFINES CPFREQF.
000840         10  CPFREQA PIC  X(0001).
000841     05  CPFREQI PIC  99.
000842*    -------------------------------
000843     05  CINDGRPL PIC S9(0004) COMP.
000844     05  CINDGRPF PIC  X(0001).
000845     05  FILLER REDEFINES CINDGRPF.
000846         10  CINDGRPA PIC  X(0001).
000847     05  CINDGRPI PIC  X(0001).
000848*    -------------------------------
000849     05  CPREMTPL PIC S9(0004) COMP.
000850     05  CPREMTPF PIC  X(0001).
000851     05  FILLER REDEFINES CPREMTPF.
000852         10  CPREMTPA PIC  X(0001).
000853     05  CPREMTPI PIC  X(0002).
000854*    -------------------------------
000855     05  CREINCDL PIC S9(0004) COMP.
000856     05  CREINCDF PIC  X(0001).
000857     05  FILLER REDEFINES CREINCDF.
000858         10  CREINCDA PIC  X(0001).
000859     05  CREINCDI PIC  X(0003).
000860*    -------------------------------
000861     05  CMEMBERL PIC S9(0004) COMP.
000862     05  CMEMBERF PIC  X(0001).
000863     05  FILLER REDEFINES CMEMBERF.
000864         10  CMEMBERA PIC  X(0001).
000865     05  CMEMBERI PIC  X(0012).
000866*    -------------------------------
000867     05  MSGBL PIC S9(0004) COMP.
000868     05  MSGBF PIC  X(0001).
000869     05  FILLER REDEFINES MSGBF.
000870         10  MSGBA PIC  X(0001).
000871     05  MSGBI PIC  X(0075).
000872*    -------------------------------
000873     05  PFKEYBL PIC S9(0004) COMP.
000874     05  PFKEYBF PIC  X(0001).
000875     05  FILLER REDEFINES PFKEYBF.
000876         10  PFKEYBA PIC  X(0001).
000877     05  PFKEYBI PIC  X(0002).
000878 01  EL160BO REDEFINES EL160BI.
000879     05  FILLER            PIC  X(0012).
000880*    -------------------------------
000881     05  FILLER            PIC  X(0003).
000882     05  DATEBO PIC  X(0008).
000883*    -------------------------------
000884     05  FILLER            PIC  X(0003).
000885     05  TIMEBO PIC  X(0005).
000886*    -------------------------------
000887     05  FILLER            PIC  X(0003).
000888     05  TITLEO PIC  X(0028).
000889*    -------------------------------
000890     05  FILLER            PIC  X(0003).
000891     05  PIKEYO PIC  X(0039).
000892*    -------------------------------
000893     05  FILLER            PIC  X(0003).
000894     05  SCNERRO PIC  X(0004).
000895*    -------------------------------
000896     05  FILLER            PIC  X(0003).
000897     05  USERSAVO PIC  X(0004).
000898*    -------------------------------
000899     05  FILLER            PIC  X(0003).
000900     05  TIMESAVO PIC  9(07).
000901*    -------------------------------
000902     05  FILLER            PIC  X(0003).
000903     05  NOSCRNO PIC  X(0004).
000904*    -------------------------------
000905     05  FILLER            PIC  X(0003).
000906     05  TOTSCRNO PIC  X(0004).
000907*    -------------------------------
000908     05  FILLER            PIC  X(0003).
000909     05  CLAIMO PIC  X(0007).
000910*    -------------------------------
000911     05  FILLER            PIC  X(0003).
000912     05  TYPEO PIC  X(0001).
000913*    -------------------------------
000914     05  FILLER            PIC  X(0003).
000915     05  CERTO PIC  X(0010).
000916*    -------------------------------
000917     05  FILLER            PIC  X(0003).
000918     05  CERTSXO PIC  X(0001).
000919*    -------------------------------
000920     05  FILLER            PIC  X(0003).
000921     05  CARRO PIC  X(0001).
000922*    -------------------------------
000923     05  FILLER            PIC  X(0003).
000924     05  STATUSO PIC  X(0001).
000925*    -------------------------------
000926     05  FILLER            PIC  X(0003).
000927     05  PROCO PIC  X(0004).
000928*    -------------------------------
000929     05  FILLER            PIC  X(0003).
000930     05  FILEO PIC  X(0004).
000931*    -------------------------------
000932     05  FILLER            PIC  X(0003).
000933     05  CREDCDO PIC  X(0016).
000934*    -------------------------------
000935     05  FILLER            PIC  X(0003).
000936     05  MLNAMEO PIC  X(0015).
000937*    -------------------------------
000938     05  FILLER            PIC  X(0003).
000939     05  MFNAMEO PIC  X(0015).
000940*    -------------------------------
000941     05  FILLER            PIC  X(0003).
000942     05  MMINITO PIC  X(0001).
000943*    -------------------------------
000944     05  FILLER            PIC  X(0003).
000945     05  SEXO PIC  X(0001).
000946*    -------------------------------
000947     05  FILLER            PIC  X(0003).
000948     05  BIRTHO PIC  X(0008).
000949*    -------------------------------
000950     05  FILLER            PIC  X(0003).
000951     05  SOCIALO PIC  X(0011).
000952*    -------------------------------
000953     05  FILLER            PIC  X(0003).
000954     05  OCCO PIC  X(0006).
000955*    -------------------------------
000956     05  FILLER            PIC  X(0003).
000957     05  CBENEO PIC  X(0010).
000958*    -------------------------------
000959     05  FILLER            PIC  X(0003).
000960     05  BHEADO PIC  X(0029).
000961*    -------------------------------
000962     05  FILLER            PIC  X(0003).
000963     05  CAUSEO PIC  X(0026).
000964*    -------------------------------
000965     05  FILLER            PIC  X(0003).
000966     05  CCAUSCDO PIC  X(0006).
000967*    -------------------------------
000968     05  FILLER            PIC  X(0003).
000969     05  ENDO PIC  X(0008).
000970*    -------------------------------
000971     05  FILLER            PIC  X(0003).
000972     05  PDTHRUO PIC  X(0008).
000973*    -------------------------------
000974     05  FILLER            PIC  X(0003).
000975     05  PDAMTO PIC  Z(06).99.
000976*    -------------------------------
000977     05  FILLER            PIC  X(0003).
000978     05  NODAYSO PIC  ZZZ99.
000979*    -------------------------------
000980     05  FILLER            PIC  X(0003).
000981     05  NOPMTSO PIC  ZZ99.
000982*    -------------------------------
000983     05  FILLER            PIC  X(0003).
000984     05  INCO PIC  X(0008).
000985*    -------------------------------
000986     05  FILLER            PIC  X(0003).
000987     05  REPO PIC  X(0008).
000988*    -------------------------------
000989     05  FILLER            PIC  X(0003).
000990     05  ESTO PIC  X(0008).
000991*    -------------------------------
000992     05  FILLER            PIC  X(0003).
000993     05  MNTDTO PIC  X(0008).
000994*    -------------------------------
000995     05  FILLER            PIC  X(0003).
000996     05  MNTTYPEO PIC  X(0006).
000997*    -------------------------------
000998     05  FILLER            PIC  X(0003).
000999     05  PRICDO PIC  X(0001).
001000*    -------------------------------
001001     05  FILLER            PIC  X(0003).
001002     05  SUPVO PIC  X(0001).
001003*    -------------------------------
001004     05  FILLER            PIC  X(0003).
001005     05  LOANNOO PIC  X(0008).
001006*    -------------------------------
001007     05  FILLER            PIC  X(0003).
001008     05  LOANBALO PIC  Z,ZZZ,Z99.99.
001009*    -------------------------------
001010     05  FILLER            PIC  X(0003).
001011     05  CERTEFFO PIC  X(0008).
001012*    -------------------------------
001013     05  FILLER            PIC  X(0003).
001014     05  CERTACTO PIC  X(0010).
001015*    -------------------------------
001016     05  FILLER            PIC  X(0003).
001017     05  CERTSTO PIC  X(0002).
001018*    -------------------------------
001019     05  FILLER            PIC  X(0003).
001020     05  CERTCARO PIC  X(0001).
001021*    -------------------------------
001022     05  FILLER            PIC  X(0003).
001023     05  CERTGRPO PIC  X(0006).
001024*    -------------------------------
001025     05  FILLER            PIC  X(0003).
001026     05  SOCSECO PIC  X(0011).
001027*    -------------------------------
001028     05  FILLER            PIC  X(0003).
001029     05  CLNAMEO PIC  X(0015).
001030*    -------------------------------
001031     05  FILLER            PIC  X(0003).
001032     05  CFNAMEO PIC  X(0010).
001033*    -------------------------------
001034     05  FILLER            PIC  X(0003).
001035     05  CINITO PIC  X(0001).
001036*    -------------------------------
001037     05  FILLER            PIC  X(0003).
001038     05  INSAGEO PIC  X(0002).
001039*    -------------------------------
001040     05  FILLER            PIC  X(0003).
001041     05  CJLNAMEO PIC  X(0015).
001042*    -------------------------------
001043     05  FILLER            PIC  X(0003).
001044     05  CJFAMEO PIC  X(0010).
001045*    -------------------------------
001046     05  FILLER            PIC  X(0003).
001047     05  CJINITO PIC  X(0001).
001048*    -------------------------------
001049     05  FILLER            PIC  X(0003).
001050     05  JAGEO PIC  X(0002).
001051*    -------------------------------
001052     05  FILLER            PIC  X(0003).
001053     05  CVDESCRO PIC  X(0006).
001054*    -------------------------------
001055     05  FILLER            PIC  X(0003).
001056     05  CVKINDO PIC  X(0003).
001057*    -------------------------------
001058     05  FILLER            PIC  X(0003).
001059     05  CVCDO PIC  X(0002).
001060*    -------------------------------
001061     05  FILLER            PIC  X(0003).
001062     05  CVOTRMO PIC  999.
001063*    -------------------------------
001064     05  FILLER            PIC  X(0003).
001065     05  CVRTRMO PIC  999.
001066*    -------------------------------
001067     05  FILLER            PIC  X(0003).
001068     05  CVOBENEO PIC  ZZZZZZZZ.ZZ.
001069*    -------------------------------
001070     05  FILLER            PIC  X(0003).
001071     05  CVFORMO PIC  X(0012).
001072*    -------------------------------
001073     05  FILLER            PIC  X(0003).
001074     05  CVCNCDTO PIC  X(0008).
001075*    -------------------------------
001076     05  FILLER            PIC  X(0003).
001077     05  CVEXITO PIC  X(0008).
001078*    -------------------------------
001079     05  FILLER            PIC  X(0003).
001080     05  CVSTATO PIC  X(0006).
001081*    -------------------------------
001082     05  FILLER            PIC  X(0003).
001083     05  CMEMCAPO PIC  X(0010).
001084*    -------------------------------
001085     05  FILLER            PIC  X(0003).
001086     05  CAPRO PIC  9(3).9(4).
001087*    -------------------------------
001088     05  FILLER            PIC  X(0003).
001089     05  CPFREQO PIC  99.
001090*    -------------------------------
001091     05  FILLER            PIC  X(0003).
001092     05  CINDGRPO PIC  X(0001).
001093*    -------------------------------
001094     05  FILLER            PIC  X(0003).
001095     05  CPREMTPO PIC  X(0002).
001096*    -------------------------------
001097     05  FILLER            PIC  X(0003).
001098     05  CREINCDO PIC  X(0003).
001099*    -------------------------------
001100     05  FILLER            PIC  X(0003).
001101     05  CMEMBERO PIC  X(0012).
001102*    -------------------------------
001103     05  FILLER            PIC  X(0003).
001104     05  MSGBO PIC  X(0075).
001105*    -------------------------------
001106     05  FILLER            PIC  X(0003).
001107     05  PFKEYBO PIC  X(0002).
001108*    -------------------------------
      *<<((file: EL160S))
000252     EJECT
000253*                                COPY ELCINTF.
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
000254     12  EL160-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
000255         16  PI-TS-COUNT         PIC S9(4)   COMP.
000256         16  PI-TS-COUNT-1       PIC S9(4)   COMP.
000257         16  PI-EL160-KEY        PIC X(8).
000258         16  PI-EL1602-KEY       PIC X(8).
000259         16  PI-PRINT-OPTION     PIC X.
000260         16  PI-FORMAT-OPTION    PIC X.
000261         16  PI-PRINT-ID         PIC X(4).
000262         16  PI-ALT-PRINT-ID     PIC X(4).
000263         16  PI-FILE-ID-IND      PIC X(1).
000264             88  PI-RETRIEVAL-FILE           VALUE 'R'.
000265             88  PI-MASTER-FILE              VALUE 'M'.
000266         16  FILLER              PIC X(609).
000267     EJECT
000268*                                COPY ELCEMIB.
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
000271 01  DFHCOMMAREA                 PIC X(1024).
000272
000273 01  CLAIM-MASTER-L              PIC X(0350).
000274     EJECT
000275*                                COPY ELCCNTL.
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
000276     EJECT
000277*                                COPY ELCACTQ.
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
000278     EJECT
000279*                                COPY ELCRETR.
      *>>((file: ELCRETR))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCRETR.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
000009*                                                                *
000010*   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
000011*   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
000012*   ****      DUPLICATED IN THE OTHER.                           *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 350  RECFORM = FIXED                           *
000016*                                                                *
000017*   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
000018*       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
000019*       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000020*       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
000021*       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
000022*                                                 RKP=75,LEN=21  *
000023*                                                                *
000024*   LOG = YES                                                    *
000025*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000026******************************************************************
000027 01  RETRIEVE-MASTER.
000028     12  RL-RECORD-ID                PIC XX.
000029         88  VALID-RL-ID         VALUE 'RL'.
000030
000031     12  RL-CONTROL-PRIMARY.
000032         16  RL-COMPANY-CD           PIC X.
000033         16  RL-CARRIER              PIC X.
000034         16  RL-CLAIM-NO             PIC X(7).
000035         16  RL-CERT-NO.
000036             20  RL-CERT-PRIME       PIC X(10).
000037             20  RL-CERT-SFX         PIC X.
000038
000039     12  RL-CONTROL-BY-NAME.
000040         16  RL-COMPANY-CD-A1        PIC X.
000041         16  RL-INSURED-LAST-NAME    PIC X(15).
000042         16  RL-INSURED-NAME.
000043             20  RL-INSURED-1ST-NAME PIC X(12).
000044             20  RL-INSURED-MID-INIT PIC X.
000045
000046     12  RL-CONTROL-BY-SSN.
000047         16  RL-COMPANY-CD-A2        PIC X.
000048         16  RL-SOC-SEC-NO.
000049             20  RL-SSN-STATE        PIC XX.
000050             20  RL-SSN-ACCOUNT      PIC X(6).
000051             20  RL-SSN-LN3          PIC X(3).
000052
000053     12  RL-CONTROL-BY-CERT-NO.
000054         16  RL-COMPANY-CD-A4        PIC X.
000055         16  RL-CERT-NO-A4.
000056             20  RL-CERT-A4-PRIME    PIC X(10).
000057             20  RL-CERT-A4-SFX      PIC X.
000058
000059     12  RL-CONTROL-BY-CCN.
000060         16  RL-COMPANY-CD-A5        PIC X.
000061         16  RL-CCN-A5.
000062             20  RL-CCN-NO.
000063                 24  RL-CCN-PREFIX-A5 PIC X(4).
000064                 24  RL-CCN-PRIME-A5 PIC X(12).
000065             20  RL-CCN-FILLER-A5    PIC X(4).
000066
000067     12  RL-INSURED-PROFILE-DATA.
000068         16  RL-INSURED-BIRTH-DT     PIC XX.
000069         16  RL-INSURED-SEX-CD       PIC X.
000070             88  RL-INSURED-IS-MALE     VALUE 'M'.
000071             88  RL-INSURED-IS-FEMALE   VALUE 'F'.
000072             88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
000073         16  RL-INSURED-OCC-CD       PIC X(6).
000074         16  FILLER                  PIC X(5).
000075
000076     12  RL-PROCESSING-INFO.
000077         16  RL-PROCESSOR-ID         PIC X(4).
000078         16  RL-CLAIM-STATUS         PIC X.
000079             88  RL-CLAIM-IS-OPEN       VALUE 'O'.
000080             88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
000081         16  RL-CLAIM-TYPE           PIC X.
000082*            88  RL-AH-CLAIM            VALUE 'A'.
000083*            88  RL-LIFE-CLAIM          VALUE 'L'.
000084*            88  RL-PROPERTY-CLAIM      VALUE 'P'.
000085*            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
000086         16  RL-CLAIM-PREM-TYPE      PIC X.
000087             88  RL-SINGLE-PREMIUM         VALUE '1'.
000088             88  RL-O-B-COVERAGE           VALUE '2'.
000089             88  RL-OPEN-END-COVERAGE      VALUE '3'.
000090         16  RL-INCURRED-DT          PIC XX.
000091         16  RL-REPORTED-DT          PIC XX.
000092         16  RL-FILE-ESTABLISH-DT    PIC XX.
000093         16  RL-EST-END-OF-DISAB-DT  PIC XX.
000094         16  RL-LAST-PMT-DT          PIC XX.
000095         16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000096         16  RL-PAID-THRU-DT         PIC XX.
000097         16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000098         16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000099         16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000100         16  RL-PMT-CALC-METHOD      PIC X.
000101             88  RL-360-DAY-YR          VALUE '1'.
000102             88  RL-365-DAY-YR          VALUE '2'.
000103             88  RL-FULL-MONTHS         VALUE '3'.
000104         16  RL-CAUSE-CD             PIC X(6).
000105
000106         16  RL-PRIME-CERT-NO.
000107             20  RL-PRIME-CERT-PRIME PIC X(10).
000108             20  RL-PRIME-CERT-SFX   PIC X.
000109
000110         16  RL-SYSTEM-IDENTIFIER    PIC XX.
000111             88  RL-CREDIT-CLAIM        VALUE 'CR'.
000112             88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
000113
000114         16  RL-MICROFILM-NO         PIC X(10).
000115         16  RL-PROG-FORM-TYPE       PIC X.
000116         16  RL-LAST-ADD-ON-DT       PIC XX.
000117
000118         16  RL-LAST-REOPEN-DT       PIC XX.
000119         16  RL-LAST-CLOSE-DT        PIC XX.
000120         16  RL-LAST-CLOSE-REASON    PIC X.
000121             88  RL-FINAL-PAID          VALUE '1'.
000122             88  RL-CLAIM-DENIED        VALUE '2'.
000123             88  RL-AUTO-CLOSE          VALUE '3'.
000124             88  RL-MANUAL-CLOSE        VALUE '4'.
000125         16  RL-ASSOC-CERT-SEQU      PIC S99.
000126         16  RL-ASSOC-CERT-TOTAL     PIC S99.
000127         16  RL-CLAIM-PAYMENT-STATUS PIC 9.
000128             88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
000129         16  FILLER                  PIC X(5).
000130
000131     12  RL-CERTIFICATE-DATA.
000132         16  RL-CERT-ORIGIN          PIC X.
000133             88  RL-CERT-WAS-ONLINE     VALUE '1'.
000134             88  RL-CERT-WAS-CREATED    VALUE '2'.
000135             88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
000136         16  RL-CERT-KEY-DATA.
000137             20  RL-CERT-CARRIER     PIC X.
000138             20  RL-CERT-GROUPING    PIC X(6).
000139             20  RL-CERT-STATE       PIC XX.
000140             20  RL-CERT-ACCOUNT.
000141                 24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
000142                 24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
000143             20  RL-CERT-EFF-DT      PIC XX.
000144
000145     12  RL-STATUS-CONTROLS.
000146         16  RL-PRIORITY-CD          PIC X.
000147             88  RL-HIGHEST-PRIORITY    VALUE '9'.
000148         16  RL-SUPV-ATTN-CD         PIC X.
000149             88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
000150             88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
000151         16  RL-PURGED-DT            PIC XX.
000152         16  RL-RESTORED-DT          PIC XX.
000153         16  RL-NEXT-AUTO-PAY-DT     PIC XX.
000154         16  RL-NEXT-RESEND-DT       PIC XX.
000155         16  RL-NEXT-FOLLOWUP-DT     PIC XX.
000156         16  FILLER                  PIC XX.
000157         16  RL-LAST-MAINT-DT        PIC XX.
000158         16  RL-LAST-MAINT-USER      PIC X(4).
000159         16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000160         16  RL-LAST-MAINT-TYPE      PIC X.
000161             88  RL-CLAIM-SET-UP           VALUE ' '.
000162             88  RL-PAYMENT-MADE           VALUE '1'.
000163             88  RL-LETTER-SENT            VALUE '2'.
000164             88  RL-MASTER-WAS-ALTERED     VALUE '3'.
000165             88  RL-MASTER-WAS-RESTORED    VALUE '4'.
000166             88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
000167             88  RL-FILE-CONVERTED         VALUE '6'.
000168         16  RL-RELATED-CLAIM-NO     PIC X(7).
000169         16  RL-HISTORY-ARCHIVE-DT   PIC XX.
000170         16  RL-BENEFICIARY          PIC X(10).
000171         16  RL-FILE-ESTABLISHED-BY  PIC X(4).
000172         16  FILLER                  PIC X(6).
000173
000174     12  RL-TRAILER-CONTROLS.
000175         16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000176             88  RL-1ST-TRL-AVAIL       VALUE +4095.
000177             88  RL-LAST-TRL-AVAIL      VALUE +100.
000178             88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
000179         16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000180         16  FILLER                  PIC XX.
000181         16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000182         16  RL-ADDRESS-TRAILER-CNT.
000183             20  RL-INSURED-ADDR-CNT  PIC S9.
000184                 88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
000185             20  RL-ACCOUNT-ADDR-CNT  PIC S9.
000186                 88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
000187             20  RL-BENIF-ADDR-CNT    PIC S9.
000188                 88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
000189             20  RL-EMPLOYER-ADDR-CNT PIC S9.
000190                 88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
000191             20  RL-DOCTOR-ADDR-CNT   PIC S9.
000192                 88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
000193             20  RL-OTHER-1-ADDR-CNT  PIC S9.
000194                 88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
000195             20  RL-OTHER-2-ADDR-CNT  PIC S9.
000196                 88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
000197
000198     12  RL-CV-REFERENCE-NO.
000199         16  RL-CV-REFNO-PRIME       PIC X(18).
000200         16  RL-CV-REFNO-SFX         PIC XX.
000201
000202     12  RL-FILE-LOCATION            PIC X(4).
000203
000204     12  RL-PROCESS-ERRORS.
000205         16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000206             88  RL-NO-FATAL-ERRORS     VALUE ZERO.
000207         16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000208             88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
000209
000210     12  RL-PRODUCT-CD               PIC X.
000211
000212     12  RL-CURRENT-KEY-DATA.
000213         16  RL-CURRENT-CARRIER      PIC X.
000214         16  RL-CURRENT-GROUPING     PIC X(6).
000215         16  RL-CURRENT-STATE        PIC XX.
000216         16  RL-CURRENT-ACCOUNT      PIC X(10).
000217
000218     12  RL-ASSOCIATES               PIC X.
000219         88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
000220         88  RL-ASSOC-INTERFACE         VALUE 'I'.
000221         88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000222         88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
000223
000224     12  RL-ACTIVITY-CODE            PIC 99.
000225     12  RL-ACTIVITY-MAINT-DT        PIC XX.
000226     12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
000227
000228     12  RL-LAPSE-REPORT-CODE        PIC 9.
000229     12  RL-LAG-REPORT-CODE          PIC 9.
000230     12  RL-LOAN-TYPE                PIC XX.
000231     12  RL-LEGAL-STATE              PIC XX.
000232
000233     12  FILLER                      PIC X(5).
      *<<((file: ELCRETR))
000280     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1602' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000281 VCOBOL-DUMMY-PROCEDURE.
000282
000283     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000284     MOVE '5'                   TO DC-OPTION-CODE.
000285     PERFORM 9800-CONVERT-DATE THRU 9800-CONVERT-DATE-EXIT.
000286     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000287     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
000288
000289     IF EIBCALEN = ZERO
000290         GO TO 8800-UNAUTHORIZED-ACCESS.
000291
000292     
      * EXEC CICS HANDLE CONDITION
000293*        PGMIDERR (8820-XCTL-ERROR)
000294*        ERROR (9990-ABEND)
000295*    END-EXEC.
      *    MOVE '"$L.                  ! " #00004333' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303034333333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000296
000297     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000298     MOVE LIT-TRAN               TO TRANS-ID.
000299
000300     IF PI-RETRIEVAL-FILE
000301         MOVE 'ELRETR'           TO W-FILE-ID.
000302
000303     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000304         MOVE LOW-VALUES         TO EL160BO
000305         MOVE ER-0008            TO EMI-ERROR
000306         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000307         MOVE LIT-IC             TO NOSCRNL
000308         GO TO 8110-SEND-DATA.
000309
000310     MOVE EIBTRMID   TO CLAS-TERM.
000311     MOVE LIT-SCREEN TO CLAS-QUAL.
000312
000313     IF LIT-PROG NOT = PI-CALLING-PROGRAM
000314         GO TO 0100-UPDATE-PI.
000315
000316     IF EIBAID = DFHCLEAR
000317         GO TO 8200-RETURN-PRIOR.
000318
000319     
      * EXEC CICS RECEIVE
000320*        MAP ('EL160B')
000321*        MAPSET ('EL160S')
000322*    END-EXEC.
           MOVE 'EL160B' TO DFHEIV1
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00004360' TO DFHEIV0
           MOVE X'382254202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034333630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160BI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000323
000324     MOVE SPACES TO ERROR-SWITCHES.
000325
000326     IF PFKEYBL GREATER THAN ZERO
000327         PERFORM 0200-TRANS-PF THRU 0210-TRANS-PF-EXIT.
000328
000329     IF SCREEN-ERROR
000330         MOVE ER-0008            TO EMI-ERROR
000331         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000332         MOVE AL-UNBON           TO PFKEYBA
000333         MOVE LIT-IC             TO PFKEYBL
000334         GO TO 8110-SEND-DATA.
000335
000336     IF EIBAID = DFHPF1 OR DFHPF2
000337         GO TO 1100-CHECK-PFKEYS.
000338     IF EIBAID = DFHPF5 OR DFHPF6
000339         GO TO 1100-CHECK-PFKEYS.
000340     IF EIBAID = DFHPF3
000341         GO TO 8200-RETURN-PRIOR.
000342     IF EIBAID = DFHPF4
000343         GO TO 8500-GET-ACT.
000344     IF EIBAID = DFHPF7
000345         GO TO 0500-CHECK-IN-PROGRESS.
000346     IF EIBAID = DFHPF12
000347         GO TO 8300-GET-HELP.
000348     IF EIBAID = DFHPF23
000349         GO TO 8810-PF23-ENTERED.
000350     IF EIBAID = DFHPF24
000351         GO TO 8400-RETURN-MASTER.
000352
000353     IF EIBAID = DFHENTER
000354         NEXT SENTENCE
000355     ELSE
000356         MOVE ER-0029            TO EMI-ERROR
000357         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000358         MOVE AL-UNBON           TO PFKEYBA
000359         MOVE LIT-IC             TO PFKEYBL
000360         GO TO 8110-SEND-DATA.
000361
000362     PERFORM 4000-CHECK-UPDATE THRU 4000-CHECK-UPDATE-EXIT.
000363     IF SCREEN-ERROR
000364         GO TO 8110-SEND-DATA.
000365
000366     IF NOSCRNL NOT = ZEROS
000367        GO TO 1000-BROWSE.
000368
000369     IF EIBAID = DFHPF1 OR DFHPF2
000370         GO TO 1100-CHECK-PFKEYS.
000371
000372     MOVE LIT-IC TO NOSCRNL.
000373     MOVE ER-0000 TO EMI-ERROR.
000374     PERFORM 9900-ERROR-FORMAT
000375             THRU 9900-ERROR-FORMAT-EXIT.
000376     GO TO 8110-SEND-DATA.
000377     EJECT
000378 0100-UPDATE-PI.
000379     IF PI-RETURN-TO-PROGRAM = LIT-PROG
000380         GO TO 0110-UPDATE-UP.
000381
000382     MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.
000383     MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.
000384     MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.
000385     MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.
000386     MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.
000387     MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.
000388     MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.
000389     MOVE LIT-PROG               TO PI-CALLING-PROGRAM.
000390     MOVE ZEROS                  TO PI-TS-COUNT.
000391     GO TO 1100-CHECK-PFKEYS.
000392
000393 0110-UPDATE-UP.
000394     MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM.
000395     MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM.
000396     MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1.
000397     MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2.
000398     MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3.
000399     MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4.
000400     MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5.
000401     MOVE SPACES                 TO PI-SAVED-PROGRAM-6.
000402
000403     
      * EXEC CICS HANDLE CONDITION
000404*        QIDERR       (0130-TS-ERROR)
000405*        ITEMERR      (0130-TS-ERROR)
000406*    END-EXEC.
      *    MOVE '"$N<                  ! # #00004444' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303034343434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000407
000408     
      * EXEC CICS READQ TS
000409*        QUEUE      (PI-KEY)
000410*        INTO       (PROGRAM-INTERFACE-BLOCK)
000411*        LENGTH     (PI-COMM-LENGTH)
000412*    END-EXEC.
      *    MOVE '*$I    L              ''   #00004449' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034343439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000413
000414     
      * EXEC CICS DELETEQ TS
000415*        QUEUE      (PI-KEY)
000416*    END-EXEC.
      *    MOVE '*&                    #   #00004455' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034343535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000417
000418     PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
000419     MOVE LIT-IC                 TO NOSCRNL.
000420     GO TO 8100-SEND-MAP.
000421
000422 0130-TS-ERROR.
000423     MOVE LOW-VALUES             TO EL160BO.
000424     MOVE ER-0192                TO EMI-ERROR.
000425     PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
000426     MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
000427     MOVE EMI-MESSAGE-AREA (1)   TO MSGBO.
000428     GO TO 8100-SEND-MAP.
000429
000430 0200-TRANS-PF.
000431     IF EIBAID NOT = DFHENTER
000432         MOVE 'X'                TO ERROR-SWITCH
000433         GO TO 0210-TRANS-PF-EXIT.
000434
000435     IF PFKEYBI NOT NUMERIC
000436         MOVE 'X'                TO ERROR-SWITCH
000437         GO TO 0210-TRANS-PF-EXIT.
000438
000439     MOVE PFKEYBI TO CHECK-PFKEYS.
000440
000441     IF CHECK-PFKEYS LESS THAN NUM-ONE
000442       OR
000443        CHECK-PFKEYS GREATER THAN NUM-TWENTY-FOUR
000444         MOVE 'X'                TO ERROR-SWITCH
000445         GO TO 0210-TRANS-PF-EXIT.
000446
000447     MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
000448
000449 0210-TRANS-PF-EXIT.
000450     EXIT.
000451     EJECT
000452 0500-CHECK-IN-PROGRESS.
000453     
      * EXEC CICS  HANDLE CONDITION
000454*           NOTFND   (0510-WRITE-INITIAL-TRAILER)
000455*    END-EXEC.
      *    MOVE '"$I                   ! $ #00004494' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303034343934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000456
000457     MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
000458     MOVE 'RF'                   TO  RF-RECORD-ID.
000459     MOVE '2'                    TO  RF-RECORD-TYPE.
000460     MOVE 'EL160'                TO  RF-REPORT-ID.
000461     MOVE ZEROS                  TO  RF-LINE-NUMBER.
000462
000463     
      * EXEC CICS READ
000464*        DATASET    (REPT-FILE-ID)
000465*        INTO       (REPORT-SAVE-FILE)
000466*        RIDFLD     (RF-CONTROL-PRIMARY)
000467*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00004504' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303034353034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000468
000469     MOVE ER-0000                TO EMI-ERROR
000470     PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000471     MOVE -1                     TO PFKEYBL
000472     GO TO 8110-SEND-DATA.
000473
000474 0510-WRITE-INITIAL-TRAILER.
000475     MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
000476     MOVE 'RF'                   TO  RF-RECORD-ID.
000477     MOVE '2'                    TO  RF-RECORD-TYPE.
000478     MOVE 'EL160'                TO  RF-REPORT-ID.
000479     MOVE ZEROS                  TO  RF-LINE-NUMBER.
000480
000481     MOVE SPACES                 TO  RF-TRAILER-RECORD.
000482     
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
000483*    END-EXEC
      *    MOVE '0"A                   "   #00004523' TO DFHEIV0
           MOVE X'302241202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303034353233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000484     
      * EXEC CICS FORMATTIME
000485*              ABSTIME(LCP-CICS-TIME)
000486*              TIME(LCP-TIME-OF-DAY-XX)
000487*    END-EXEC
      *    MOVE 'j$(     (             #   #00004525' TO DFHEIV0
           MOVE X'6A2428202020202028202020' &
                X'202020202020202020202320' &
                X'2020233030303034353235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000488     MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.
000489     MOVE 'STARTED'              TO  RF-CURRENT-DATE.
000490
000491     
      * EXEC CICS WRITE
000492*        DATASET (REPT-FILE-ID)
000493*        FROM    (REPORT-SAVE-FILE)
000494*        RIDFLD  (RF-CONTROL-PRIMARY)
000495*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004532' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034353332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000496
000497 0520-DELETE-REC.
000498     MOVE 1                      TO RF-LINE-NUMBER.
000499     
      * EXEC CICS  HANDLE CONDITION
000500*           NOTFND   (0540-DELETE-REC)
000501*    END-EXEC.
      *    MOVE '"$I                   ! % #00004540' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303034353430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000502
000503 0530-DELETE-1.
000504     MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
000505     MOVE 'RF'                   TO RF-RECORD-ID.
000506     MOVE '1'                    TO RF-RECORD-TYPE.
000507     MOVE 'EL160'                TO RF-REPORT-ID.
000508
000509     
      * EXEC CICS DELETE
000510*        DATASET (REPT-FILE-ID)
000511*        RIDFLD  (RF-CONTROL-PRIMARY)
000512*        KEYLENGTH (11)
000513*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004550' TO DFHEIV0
           MOVE X'26282020524B202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034353530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000514
000515     ADD 1 TO RF-LINE-NUMBER.
000516     GO TO 0530-DELETE-1.
000517
000518 0540-DELETE-REC.
000519     
      * EXEC CICS  HANDLE CONDITION
000520*           NOTFND   (0560-READ-TEMP-STORAGE)
000521*    END-EXEC.
      *    MOVE '"$I                   ! & #00004560' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303034353630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000522
000523 0550-DELETE-2.
000524     MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
000525     MOVE 'RF'                   TO RF-RECORD-ID.
000526     MOVE '2'                    TO RF-RECORD-TYPE.
000527     MOVE 'EL160'                TO RF-REPORT-ID.
000528
000529     
      * EXEC CICS DELETE
000530*        DATASET (REPT-FILE-ID)
000531*        RIDFLD  (RF-CONTROL-PRIMARY)
000532*        KEYLENGTH (11)
000533*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004570' TO DFHEIV0
           MOVE X'26282020524B202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034353730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000534
000535     ADD 1 TO RF-LINE-NUMBER.
000536     GO TO 0550-DELETE-2.
000537
000538 EJECT
000539 0560-READ-TEMP-STORAGE.
000540     
      * EXEC CICS HANDLE CONDITION
000541*         ITEMERR (0570-DELETE-INITIAL-2)
000542*    END-EXEC.
      *    MOVE '"$<                   ! '' #00004581' TO DFHEIV0
           MOVE X'22243C202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303034353831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000543
000544     MOVE +0 TO WS-TS-ITEM-NO.
000545
000546 0565-READ-NEXT-TEMP.
000547     ADD +1 TO WS-TS-ITEM-NO.
000548
000549     
      * EXEC CICS READQ TS
000550*         QUEUE    (PI-EL1602-KEY)
000551*         INTO     (WS-TS-AREA)
000552*         LENGTH   (EL1602-LENGTH)
000553*         ITEM     (WS-TS-ITEM-NO)
000554*    END-EXEC.
      *    MOVE '*$II   L              ''   #00004590' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034353930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 WS-TS-AREA, 
                 EL1602-LENGTH, 
                 WS-TS-ITEM-NO, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000555
000556     IF WS-LINE-COUNT GREATER THAN +55
000557        PERFORM 0800-PRINT-HEADINGS THRU 0800-EXIT.
000558
000559     MOVE WS-TS-CARR             TO WS-D1-CARR.
000560     MOVE WS-TS-CLAIM            TO WS-D1-CLAIM.
000561     MOVE WS-TS-CERT             TO WS-D1-CERT-PRIME.
000562     MOVE WS-TS-CERT-SFX         TO WS-D1-CERT-SFX.
000563
000564     EVALUATE TRUE
000565
000566        WHEN WS-TS-TYPE = PI-LIFE-OVERRIDE-L1
000567           MOVE PI-LIFE-OVERRIDE-L6
000568                                 TO WS-D1-TYPE
000569
000570        WHEN WS-TS-TYPE = PI-AH-OVERRIDE-L1
000571           MOVE PI-AH-OVERRIDE-L6
000572                                 TO WS-D1-TYPE
000573
000574        WHEN WS-TS-TYPE = 'I'
000575           MOVE '  IU  '         TO WS-D1-TYPE
000576
000577        WHEN WS-TS-TYPE = 'G'
000578           MOVE ' GAP  '         TO WS-D1-TYPE
000579
000580        WHEN WS-TS-TYPE = 'F'
000581           MOVE ' FAM  '         TO WS-D1-TYPE
000582        WHEN WS-TS-TYPE = 'B'
000583           MOVE ' BRV  '         TO WS-D1-TYPE
000584        WHEN WS-TS-TYPE = 'H'
000585           MOVE ' HOSP '         TO WS-D1-TYPE
000586
000587        WHEN WS-TS-TYPE = 'O'
000588           MOVE ' OTH  '         TO WS-D1-TYPE
000589
000590     END-EVALUATE
000591
000592     IF WS-TS-STATUS = 'O'
000593        MOVE ' OPEN '            TO WS-D1-STATUS
000594     ELSE
000595        MOVE 'CLOSED'            TO WS-D1-STATUS.
000596
000597     MOVE WS-TS-FILE             TO WS-D1-FILE.
000598
000599     MOVE WS-TS-LNAME            TO WS-D1-NAME.
000600
000601     MOVE WS-DETAIL1             TO RF-DATA-133.
000602     MOVE ' '                    TO RF-CTL-CHAR-133.
000603     PERFORM 0600-PRT-LINE THRU 0600-EXIT.
000604
000605     GO TO 0565-READ-NEXT-TEMP.
000606
000607 0570-DELETE-INITIAL-2.
000608     MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
000609     MOVE 'RF'                   TO RF-RECORD-ID.
000610     MOVE '2'                    TO RF-RECORD-TYPE.
000611     MOVE 'EL160'                TO RF-REPORT-ID.
000612     MOVE ZEROS                  TO RF-LINE-NUMBER.
000613
000614     
      * EXEC CICS DELETE
000615*         DATASET     (REPT-FILE-ID)
000616*         RIDFLD      (RF-CONTROL-PRIMARY)
000617*         KEYLENGTH   (11)
000618*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004655' TO DFHEIV0
           MOVE X'26282020524B202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034363535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000619
000620 0580-WRITE-TRAILER.
000621     MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
000622     MOVE 'RF'                   TO RF-RECORD-ID.
000623     MOVE 'EL160'                TO RF-REPORT-ID.
000624     MOVE '2'                    TO RF-RECORD-TYPE.
000625     ADD +1                      TO WS-LINE-NUMBER.
000626     MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
000627     MOVE SPACES                 TO RF-TRAILER-RECORD.
000628
000629     
      * EXEC CICS ASKTIME
000630*        ABSTIME(LCP-CICS-TIME)
000631*    END-EXEC.
      *    MOVE '0"A                   "   #00004670' TO DFHEIV0
           MOVE X'302241202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303034363730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000632
000633     
      * EXEC CICS FORMATTIME
000634*        ABSTIME(LCP-CICS-TIME)
000635*        TIME(LCP-TIME-OF-DAY-XX)
000636*    END-EXEC.
      *    MOVE 'j$(     (             #   #00004674' TO DFHEIV0
           MOVE X'6A2428202020202028202020' &
                X'202020202020202020202320' &
                X'2020233030303034363734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000637
000638     MOVE LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.
000639     MOVE SAVE-DATE              TO RF-CURRENT-DATE.
000640
000641     
      * EXEC CICS WRITE
000642*        DATASET (REPT-FILE-ID)
000643*        FROM    (REPORT-SAVE-FILE)
000644*        RIDFLD  (RF-CONTROL-PRIMARY)
000645*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004682' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034363832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000646
000647     MOVE ER-0000                TO EMI-ERROR.
000648     PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
000649     MOVE -1                     TO PFKEYBL.
000650     GO TO 8110-SEND-DATA.
000651
000652 0600-PRT-LINE.
000653     MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
000654     MOVE 'RF'                   TO RF-RECORD-ID.
000655     MOVE '1'                    TO RF-RECORD-TYPE.
000656     MOVE 'EL160'                TO RF-REPORT-ID.
000657     ADD +1                      TO WS-LINE-NUMBER.
000658     MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
000659
000660     
      * EXEC CICS WRITE
000661*        DATASET     (REPT-FILE-ID)
000662*        FROM        (REPORT-SAVE-FILE)
000663*        RIDFLD      (RF-CONTROL-PRIMARY)
000664*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004701' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034373031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000665
000666     ADD +1 TO WS-LINE-COUNT.
000667 0600-EXIT.
000668     EXIT.
000669
000670 0800-PRINT-HEADINGS.
000671     MOVE WS-HEADING1            TO RF-DATA-133.
000672     MOVE '1'                    TO RF-CTL-CHAR-133.
000673     PERFORM 0600-PRT-LINE THRU 0600-EXIT.
000674
000675     MOVE WS-HEADING2            TO RF-DATA-133.
000676     MOVE '-'                    TO RF-CTL-CHAR-133.
000677     PERFORM 0600-PRT-LINE THRU 0600-EXIT.
000678
000679     MOVE SPACES                 TO RF-DATA-133.
000680     MOVE '0'                    TO RF-CTL-CHAR-133.
000681     PERFORM 0600-PRT-LINE THRU 0600-EXIT.
000682
000683     MOVE +6                     TO WS-LINE-COUNT.
000684
000685 0800-EXIT.
000686     EXIT.
000687     EJECT
000688
000689 1000-BROWSE.
000690
000691     IF NOSCRNL = ZEROS
000692        GO TO 1100-CHECK-PFKEYS.
000693
000694     
      * EXEC CICS BIF DEEDIT
000695*        FIELD   (NOSCRNI)
000696*        LENGTH  (4)
000697*    END-EXEC.
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004735' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034373335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NOSCRNI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000698
000699     IF (PI-PROCESSOR-ID = 'LGXX') OR
000700        (PI-COMPANY-ID   = 'AIG' OR 'AUK')
000701         MOVE 2501               TO  MAX-TS-PAGES.
000702
000703     IF (NOSCRNI GREATER '00' AND LESS MAX-TS-PAGES)
000704        NEXT SENTENCE
000705     ELSE
000706        MOVE ER-0515             TO EMI-ERROR
000707        MOVE AL-UNBON            TO NOSCRNA
000708        MOVE -1                  TO NOSCRNL
000709        PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000710        GO TO 8110-SEND-DATA.
000711
000712     IF NOSCRNI GREATER THAN PI-TS-COUNT-1
000713        MOVE ER-0515             TO EMI-ERROR
000714        MOVE AL-UNBON            TO NOSCRNA
000715        MOVE -1                  TO NOSCRNL
000716        PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000717        GO TO 8110-SEND-DATA.
000718
000719     MOVE NOSCRNI                TO PI-TS-COUNT
000720     GO TO 1150-GET-TEMP-STOR.
000721
000722 1100-CHECK-PFKEYS.
000723********MODIFICATION TO ENABLE PRINTING OF AUDIT*******
000724*
000725******************************************************************
000726*       PI-PRINT-OPTION MUST BE 'N' FOR CARRIER AND ACCOUNT      *
000727*       AND MUST HAVE AN ALTERNATE PRINTER-ID.                   *
000728******************************************************************
000729
000730     IF PI-RETRIEVAL-FILE
000731         IF EIBAID = DFHPF5
000732             MOVE ER-0971        TO EMI-ERROR
000733             PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000734             MOVE -1             TO PFKEYBL
000735             GO TO 8110-SEND-DATA
000736         ELSE
000737             IF EIBAID = DFHPF6
000738                 MOVE ER-0972    TO EMI-ERROR
000739                 PERFORM 9900-ERROR-FORMAT
000740                     THRU 9900-ERROR-FORMAT-EXIT
000741                 MOVE -1         TO PFKEYBL
000742                 GO TO 8110-SEND-DATA.
000743
000744     IF PI-CARRIER-SECURITY GREATER THAN SPACES OR
000745        PI-ACCOUNT-SECURITY GREATER THAN SPACES
000746           IF EIBAID = DFHPF5
000747              IF PI-ALT-PRINT-ID GREATER THAN SPACES
000748                 NEXT SENTENCE
000749              ELSE
000750                 MOVE ER-2379    TO EMI-ERROR
000751                 PERFORM 9900-ERROR-FORMAT THRU
000752                 9900-ERROR-FORMAT-EXIT
000753                 MOVE -1         TO PFKEYBL
000754                 GO TO 8110-SEND-DATA.
000755
000756     IF EIBAID = DFHPF5
000757         PERFORM 1200-PROCESS-OPTIONS
000758            THRU 1299-EXIT
000759         GO TO 1150-GET-TEMP-STOR.
000760
000761     IF EIBAID = DFHPF6 AND
000762        PI-PRINT-OPTION = 'N'
000763         MOVE ER-0609            TO EMI-ERROR
000764         PERFORM 9900-ERROR-FORMAT
000765                THRU 9900-ERROR-FORMAT-EXIT
000766         MOVE -1                 TO PFKEYBL
000767         GO TO 8110-SEND-DATA.
000768
000769     IF EIBAID = DFHPF6
000770         MOVE PI-TS-COUNT        TO WS-SAVE-TS-COUNT
000771         MOVE +1                 TO PI-TS-COUNT  WS-PRINT-SW
000772         PERFORM 1200-PROCESS-OPTIONS THRU 1299-EXIT
000773            UNTIL PI-TS-COUNT GREATER PI-TS-COUNT-1
000774         MOVE WS-SAVE-TS-COUNT   TO PI-TS-COUNT
000775         GO TO 1150-GET-TEMP-STOR.
000776*******************************************************
000777
000778     IF EIBAID = DFHPF2
000779        SUBTRACT +1 FROM PI-TS-COUNT
000780     ELSE
000781        ADD +1        TO PI-TS-COUNT.
000782
000783 1150-GET-TEMP-STOR.
000784     MOVE SPACE TO ERROR-SWITCH.
000785
000786     IF PI-TS-COUNT LESS THAN NUM-ONE
000787         MOVE 'X'                TO ERROR-SWITCH
000788         MOVE ER-0131         TO EMI-ERROR
000789         PERFORM 9900-ERROR-FORMAT
000790             THRU 9900-ERROR-FORMAT-EXIT
000791         MOVE NUM-ONE            TO PI-TS-COUNT.
000792
000793     PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
000794
000795     IF SCREEN-ERROR
000796         MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO
000797         MOVE EMI-MESSAGE-AREA (1) TO MSGBO
000798     ELSE
000799         IF SCNERRI = SPACES OR LOW-VALUES
000800             MOVE SCNERRI TO PI-LAST-ERROR-NO
000801             MOVE SPACES TO MSGBO
000802         ELSE
000803             MOVE SCNERRI TO EMI-ERROR PI-LAST-ERROR-NO
000804             PERFORM 9900-ERROR-FORMAT
000805                 THRU 9900-ERROR-FORMAT-EXIT
000806             MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
000807
000808     MOVE LIT-IC                 TO NOSCRNL.
000809     GO TO 8100-SEND-MAP.
000810     EJECT
000811
000812 1200-PROCESS-OPTIONS.
000813     MOVE SPACE TO ERROR-SWITCH.
000814     PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
000815
000816     IF SCREEN-ERROR
000817         MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO
000818         MOVE EMI-MESSAGE-AREA (1) TO MSGBO
000819     ELSE
000820         IF SCNERRI = SPACES OR LOW-VALUES
000821             MOVE SCNERRI TO PI-LAST-ERROR-NO
000822             MOVE SPACES TO MSGBO
000823         ELSE
000824             MOVE SCNERRI TO EMI-ERROR PI-LAST-ERROR-NO
000825             PERFORM 9900-ERROR-FORMAT
000826                 THRU 9900-ERROR-FORMAT-EXIT
000827             MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
000828
000829     MOVE PIKEYI                 TO PI-CONTROL-IN-PROGRESS.
000830
000831     IF PI-PRINT-OPTION = SPACE
000832         MOVE 'N' TO PI-PRINT-OPTION.
000833
000834     IF PI-FORMAT-OPTION = SPACE
000835         MOVE 'P' TO PI-FORMAT-OPTION.
000836
000837     IF PI-PRINT-OPTION = 'N'
000838       AND EIBAID NOT = DFHPF6
000839         GO TO 1260-START-PRINTER.
000840
000841     
      * EXEC CICS HANDLE CONDITION
000842*        NOTFND(1230-CREATE-NEW-ACTQ)
000843*        END-EXEC.
      *    MOVE '"$I                   ! ( #00004882' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303034383832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000844
000845     MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.
000846     MOVE PI-CARRIER             TO ACTQ-CARRIER.
000847     MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.
000848     MOVE PI-CERT-NO             TO ACTQ-CERT-NO.
000849
000850     MOVE 'ACTQ'                 TO FILE-SWITCH.
000851
000852     
      * EXEC CICS READ
000853*        UPDATE
000854*        DATASET  ('ELACTQ')
000855*        SET      (ADDRESS OF ACTIVITY-QUE)
000856*        RIDFLD   (ELACTQ-KEY)
000857*    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00004893' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034383933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000858
000859     IF PI-FORMAT-OPTION = 'F'
000860         MOVE '1'                TO AQ-PENDING-STATUS-FLAG
000861     ELSE
000862         MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
000863
000864     
      * EXEC CICS REWRITE
000865*        DATASET ('ELACTQ')
000866*        FROM    (ACTIVITY-QUE)
000867*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00004905' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034393035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000868
000869     IF PRINT-IN-PROCESS
000870         ADD +1 TO PI-TS-COUNT.
000871
000872     GO TO 1299-EXIT.
000873
000874 1230-CREATE-NEW-ACTQ.
000875     
      * EXEC CICS GETMAIN
000876*        SET     (ADDRESS OF ACTIVITY-QUE)
000877*        LENGTH  (60)
000878*        INITIMG (GETMAIN-SPACE)
000879*    END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004916' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034393136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000880
000881     MOVE 'AQ'                   TO AQ-RECORD-ID.
000882     MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.
000883     MOVE PI-CARRIER             TO AQ-CARRIER.
000884     MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.
000885     MOVE PI-CERT-NO             TO AQ-CERT-NO.
000886
000887     IF PI-FORMAT-OPTION = 'F'
000888         MOVE '1'                TO AQ-PENDING-STATUS-FLAG
000889     ELSE
000890         MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
000891
000892     MOVE +0                     TO AQ-PAYMENT-COUNTER.
000893     MOVE 'ACTQ'                 TO FILE-SWITCH.
000894
000895     
      * EXEC CICS WRITE
000896*        DATASET ('ELACTQ')
000897*        FROM    (ACTIVITY-QUE)
000898*        RIDFLD  (AQ-CONTROL-PRIMARY)
000899*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00004936' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034393336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000900
000901     IF PRINT-IN-PROCESS
000902         ADD +1 TO PI-TS-COUNT.
000903
000904     GO TO 1299-EXIT.
000905
000906 1260-START-PRINTER.
000907     IF PI-FORMAT-OPTION = 'F'
000908         MOVE '2' TO PI-ENTRY-CD-1
000909     ELSE
000910         MOVE '1' TO PI-ENTRY-CD-1.
000911
000912 1265-START-PRINTER.
000913     IF FIRST-TIME-THRU
000914         IF PI-ALT-PRINT-ID = SPACES
000915             GO TO 1270-GET-PRINT-ID
000916         ELSE
000917             MOVE PI-ALT-PRINT-ID TO PI-PRINT-ID.
000918
000919     
      * EXEC CICS HANDLE CONDITION
000920*        NOTOPEN (1275-CNTL-NOT-OPEN)
000921*        NOTFND  (1280-NOT-FOUND)
000922*        TERMIDERR  (1270-GET-PRINT-ID)
000923*        TRANSIDERR (1285-TRANS-ERROR)
000924*    END-EXEC.
      *    MOVE '"$JI[\                ! ) #00004960' TO DFHEIV0
           MOVE X'22244A495B5C202020202020' &
                X'202020202020202020202120' &
                X'2920233030303034393630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000925
000926     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'
000927*        MOVE EIBTRMID       TO PI-PRINT-ID
000928         
      * EXEC CICS START
000929*            TRANSID (START-TRANS-ID)
000930*            TERMID  (PI-PRINT-ID)
000931*            FROM    (PROGRAM-INTERFACE-BLOCK)
000932*            LENGTH  (PI-COMM-LENGTH)
000933*        END-EXEC
      *    MOVE '0( LF                 1   #00004969' TO DFHEIV0
           MOVE X'3028204C4620202020202020' &
                X'202020202020202020203120' &
                X'2020233030303034393639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000934     ELSE
000935         
      * EXEC CICS START
000936*            TRANSID (START-TRANS-ID)
000937*            TERMID  (PI-PRINT-ID)
000938*            FROM    (PROGRAM-INTERFACE-BLOCK)
000939*            LENGTH  (PI-COMM-LENGTH)
000940*        END-EXEC.
      *    MOVE '0( LFT                1   #00004976' TO DFHEIV0
           MOVE X'3028204C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303034393736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 PI-PRINT-ID, 
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
           
000941
000942     GO TO 1299-EXIT.
000943
000944 1270-GET-PRINT-ID.
000945     IF FIRST-TIME-THRU
000946         MOVE HIGH-VALUES TO WS-FIRST-TIME-SW
000947     ELSE
000948         MOVE ER-0412 TO EMI-ERROR
000949         PERFORM 9900-ERROR-FORMAT
000950            THRU 9900-ERROR-FORMAT-EXIT
000951         GO TO 8110-SEND-DATA.
000952
000953     MOVE PI-COMPANY-ID          TO COMPANY-ID.
000954     MOVE '1'                    TO RECORD-TYPE.
000955     MOVE SPACES                 TO CNTL-PROC.
000956     MOVE +0                     TO SEQ-NO.
000957     MOVE 'CNTL'                 TO FILE-SWITCH.
000958
000959     
      * EXEC CICS READ
000960*        DATASET ('ELCNTL')
000961*        SET     (ADDRESS OF CONTROL-FILE)
000962*        RIDFLD  (CNTL-KEY)
000963*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005000' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035303030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000964
000965     IF CF-FORMS-PRINTER-ID = SPACES
000966         MOVE ER-0337 TO EMI-ERROR
000967         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
000968         GO TO 8110-SEND-DATA
000969     ELSE
000970         MOVE CF-FORMS-PRINTER-ID TO PI-PRINT-ID
000971         GO TO 1265-START-PRINTER.
000972
000973 1275-CNTL-NOT-OPEN.
000974     MOVE ER-0042 TO EMI-ERROR.
000975     PERFORM 9900-ERROR-FORMAT
000976        THRU 9900-ERROR-FORMAT-EXIT.
000977     GO TO 8110-SEND-DATA.
000978
000979 1280-NOT-FOUND.
000980     MOVE ER-0190 TO EMI-ERROR.
000981     PERFORM 9900-ERROR-FORMAT
000982        THRU 9900-ERROR-FORMAT-EXIT.
000983
000984     GO TO 8110-SEND-DATA.
000985
000986 1285-TRANS-ERROR.
000987     MOVE ER-0413 TO EMI-ERROR.
000988     PERFORM 9900-ERROR-FORMAT
000989        THRU 9900-ERROR-FORMAT-EXIT.
000990
000991     GO TO 8110-SEND-DATA.
000992
000993 1299-EXIT.
000994     EXIT.
000995     EJECT
000996
000997 3000-GET-RECORD.
000998     IF PI-TS-COUNT GREATER THAN PI-TS-COUNT-1
000999        GO TO 3010-RECORD-NOT-FOUND.
001000
001001     
      * EXEC CICS HANDLE CONDITION
001002*        ITEMERR (3010-RECORD-NOT-FOUND)
001003*    END-EXEC.
      *    MOVE '"$<                   ! * #00005042' TO DFHEIV0
           MOVE X'22243C202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303035303432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001004
001005     
      * EXEC CICS READQ TS
001006*        QUEUE (PI-EL1602-KEY)
001007*        INTO (EL160BO)
001008*        LENGTH (EL1602-LENGTH)
001009*        ITEM (PI-TS-COUNT)
001010*    END-EXEC.
      *    MOVE '*$II   L              ''   #00005046' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035303436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 EL160BO, 
                 EL1602-LENGTH, 
                 PI-TS-COUNT, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001011
001012     GO TO 3020-GET-RECORD-EXIT.
001013
001014 3010-RECORD-NOT-FOUND.
001015     MOVE 'X'                    TO ERROR-SWITCH.
001016     MOVE ER-0130             TO EMI-ERROR.
001017     PERFORM 9900-ERROR-FORMAT
001018             THRU 9900-ERROR-FORMAT-EXIT.
001019     SUBTRACT NUM-ONE            FROM PI-TS-COUNT.
001020     GO TO 3000-GET-RECORD.
001021
001022 3020-GET-RECORD-EXIT.
001023     EXIT.
001024     EJECT
001025 4000-CHECK-UPDATE.
001026     MOVE SPACE                  TO UPDATE-SWITCH.
001027
001028     IF PRICDL GREATER THAN ZEROES
001029         PERFORM 4100-CHECK-PRI
001030             THRU 4100-CHECK-PRI-EXIT
001031     ELSE
001032         MOVE AL-UANOF           TO PRICDA.
001033
001034     IF SUPVL GREATER THAN ZEROES
001035         PERFORM 4110-CHECK-SUPR
001036             THRU 4110-CHECK-SUPR-EXIT
001037     ELSE
001038         MOVE AL-UANOF           TO SUPVA.
001039
001040     IF FILEL GREATER THAN ZEROES
001041         PERFORM 4120-CHECK-FILE
001042             THRU 4120-CHECK-FILE-EXIT
001043     ELSE
001044         MOVE AL-UANOF TO FILEA.
001045
001046     IF PROCL GREATER THAN ZEROES
001047         PERFORM 4130-CHECK-PROC
001048             THRU 4150-CHECK-PROC-EXIT
001049     ELSE
001050         MOVE AL-UANOF TO PROCA.
001051
001052     IF NOT NO-UPDATES
001053        GO TO 4000-CHECK-CAP.
001054
001055     IF NOSCRNL NOT = ZEROS
001056        GO TO 1000-BROWSE.
001057
001058     IF EIBAID = DFHPF1 OR DFHPF2 OR
001059                 DFHPF5 OR DFHPF6
001060        MOVE NUM-ONE TO PI-TS-COUNT
001061        GO TO 1100-CHECK-PFKEYS.
001062
001063     IF  EIBAID = DFHENTER
001064         MOVE 'X'                TO ERROR-SWITCH
001065         MOVE ER-0276         TO EMI-ERROR
001066         PERFORM 9900-ERROR-FORMAT
001067             THRU 9900-ERROR-FORMAT-EXIT
001068         MOVE LIT-IC             TO NOSCRNL
001069         GO TO 4000-CHECK-UPDATE-EXIT.
001070
001071 4000-CHECK-CAP.
001072     IF NOT MODIFY-CAP
001073         MOVE 'X'                TO ERROR-SWITCH
001074         MOVE ER-0070         TO EMI-ERROR
001075         PERFORM 9900-ERROR-FORMAT
001076             THRU 9900-ERROR-FORMAT-EXIT
001077         MOVE LIT-IC             TO NOSCRNL
001078         GO TO 4000-CHECK-UPDATE-EXIT.
001079
001080     PERFORM 4200-UPDATE-MSTR THRU 4210-UPDATE-MSTR-EXIT.
001081
001082     IF SCREEN-ERROR
001083         PERFORM 5000-UPDATE-TS
001084             THRU 5000-UPDATE-TS-EXIT
001085         MOVE LIT-IC             TO NOSCRNL
001086         MOVE ER-0068         TO EMI-ERROR
001087         PERFORM 9900-ERROR-FORMAT
001088             THRU 9900-ERROR-FORMAT-EXIT.
001089
001090 4000-CHECK-UPDATE-EXIT.
001091     EXIT.
001092     EJECT
001093 4100-CHECK-PRI.
001094     IF PRICDI GREATER THAN ZERO
001095       AND
001096        PRICDI NOT GREATER THAN '9'
001097         MOVE AL-UANON TO PRICDA
001098         MOVE 'X'                TO UPDATE-SWITCH
001099         GO TO 4100-CHECK-PRI-EXIT.
001100
001101     MOVE LIT-IC                 TO PRICDL.
001102     MOVE AL-UABON               TO PRICDA.
001103     MOVE ER-0274             TO EMI-ERROR.
001104     PERFORM 9900-ERROR-FORMAT
001105         THRU 9900-ERROR-FORMAT-EXIT.
001106
001107 4100-CHECK-PRI-EXIT.
001108     EXIT.
001109
001110 4110-CHECK-SUPR.
001111     IF SUPVI = 'Y' OR 'N' OR SPACES
001112         MOVE 'X'                TO UPDATE-SWITCH
001113         MOVE AL-UANON           TO SUPVA
001114         GO TO 4110-CHECK-SUPR-EXIT.
001115
001116     MOVE LIT-IC                 TO SUPVL.
001117     MOVE AL-UABON               TO SUPVA.
001118     MOVE ER-0230             TO EMI-ERROR.
001119     PERFORM 9900-ERROR-FORMAT
001120         THRU 9900-ERROR-FORMAT-EXIT.
001121
001122 4110-CHECK-SUPR-EXIT.
001123     EXIT.
001124
001125 4120-CHECK-FILE.
001126     IF FILEI NOT = SPACES
001127         MOVE 'X'                TO UPDATE-SWITCH
001128         GO TO 4120-CHECK-FILE-EXIT.
001129
001130 4120-CHECK-FILE-EXIT.
001131     EXIT.
001132
001133 4130-CHECK-PROC.
001134     
      * EXEC CICS HANDLE CONDITION
001135*        NOTFND (4140-PROC-ERROR)
001136*    END-EXEC.
      *    MOVE '"$I                   ! + #00005175' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303035313735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001137
001138     MOVE PI-COMPANY-ID          TO COMPANY-ID.
001139     MOVE '2'                    TO RECORD-TYPE.
001140     MOVE PROCI                  TO CNTL-PROC.
001141     MOVE ZEROES                 TO SEQ-NO.
001142
001143     
      * EXEC CICS READ
001144*        SET (ADDRESS OF CONTROL-FILE)
001145*        DATASET ('ELCNTL')
001146*        RIDFLD (CNTL-KEY)
001147*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005184' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001148
001149     MOVE 'X'                    TO KEY-SWITCH UPDATE-SWITCH.
001150     GO TO 4150-CHECK-PROC-EXIT.
001151
001152 4140-PROC-ERROR.
001153
001154     MOVE ER-0273 TO EMI-ERROR.
001155     MOVE LIT-IC                 TO PROCL.
001156     MOVE AL-UABON               TO PROCA.
001157     PERFORM 9900-ERROR-FORMAT
001158         THRU 9900-ERROR-FORMAT-EXIT.
001159
001160 4150-CHECK-PROC-EXIT.
001161     EXIT.
001162     EJECT
001163 4200-UPDATE-MSTR.
001164     MOVE PIKEYI         TO PI-CONTROL-IN-PROGRESS.
001165     MOVE SPACES         TO MSTR-KEY.
001166     MOVE PI-COMPANY-CD  TO MSTR-COMPANY-CODE.
001167
001168     MOVE PI-CARRIER     TO MSTR-CARRIER.
001169
001170     MOVE PI-CLAIM-NO    TO MSTR-CLAIM-NO.
001171     MOVE PI-CERT-NO     TO MSTR-CERT-NO.
001172
001173     
      * EXEC CICS READ
001174*        INTO    (CLAIM-MASTER)
001175*        DATASET (W-FILE-ID)
001176*        RIDFLD  (MSTR-KEY)
001177*        UPDATE
001178*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00005214' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303035323134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001179
001180     MOVE USERSAVI TO PI-UPDATE-BY.
001181     MOVE TIMESAVI TO PI-UPDATE-HHMMSS.
001182
001183     IF PI-UPDATE-BY NOT = CL-LAST-MAINT-USER
001184         PERFORM 4220-UNLOCK-MSTR
001185             THRU 4230-UNLOCK-MSTR-EXIT
001186         MOVE 'X'                TO ERROR-SWITCH
001187         GO TO 4210-UPDATE-MSTR-EXIT.
001188
001189     IF PI-UPDATE-HHMMSS NOT = CL-LAST-MAINT-HHMMSS
001190         PERFORM 4220-UNLOCK-MSTR
001191             THRU 4230-UNLOCK-MSTR-EXIT
001192         MOVE 'X'                TO ERROR-SWITCH
001193         GO TO 4210-UPDATE-MSTR-EXIT.
001194
001195     IF KEY-CHANGE
001196         PERFORM 4300-CHANGE-KEY
001197             THRU 4310-CHANGE-KEY-EXIT
001198     ELSE
001199         PERFORM 4320-UPDATE-RECORD
001200             THRU 4330-UPDATE-RECORD-EXIT.
001201
001202     MOVE PROCI                  TO HOLD-PROC.
001203     MOVE PRICDI                 TO HOLD-PRI.
001204     MOVE SUPVI                  TO HOLD-SUPV.
001205     MOVE FILEI                  TO HOLD-FILE.
001206
001207     PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
001208
001209     IF HOLD-PROC NOT = LOW-VALUES
001210         MOVE HOLD-PROC     TO PROCO.
001211     IF HOLD-PRI NOT = LOW-VALUES
001212         MOVE HOLD-PRI      TO PRICDO.
001213     IF HOLD-SUPV NOT = LOW-VALUES
001214         MOVE HOLD-SUPV     TO SUPVO.
001215     IF HOLD-FILE NOT = LOW-VALUES
001216         MOVE HOLD-FILE     TO FILEO.
001217
001218     MOVE SAVE-DATE         TO MNTDTO.
001219     MOVE LIT-UPDATE        TO MNTTYPEO.
001220     MOVE EIBTIME           TO TIMESAVO.
001221     MOVE PI-PROCESSOR-ID   TO USERSAVO.
001222
001223     PERFORM 4400-REWRITE-TS THRU 4410-REWRITE-TS-EXIT.
001224
001225     MOVE AL-UANOF TO PROCA
001226                      PRICDA
001227                      SUPVA
001228                      FILEA.
001229
001230 4210-UPDATE-MSTR-EXIT.
001231     EXIT.
001232
001233 4220-UNLOCK-MSTR.
001234     
      * EXEC CICS UNLOCK
001235*        DATASET (W-FILE-ID)
001236*    END-EXEC.
      *    MOVE '&*                    #   #00005275' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035323735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001237
001238 4230-UNLOCK-MSTR-EXIT.
001239     EXIT.
001240     EJECT
001241 4300-CHANGE-KEY.
001242     MOVE CLAIM-MASTER           TO  WS-OLD-CLAIM-RECORD.
001243
001244     
      * EXEC CICS DELETE
001245*        DATASET (W-FILE-ID)
001246*    END-EXEC.
      *    MOVE '&(                    &   #00005285' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303035323835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001247
001248*    EXEC CICS GETMAIN
001249*        SET (ADDRESS OF CLAIM-MASTER)
001250*        LENGTH (MSTR-LENGTH)
001251*        INITIMG (GETMAIN-SPACE)
001252*    END-EXEC.
001253
001254     MOVE WS-OLD-CLAIM-RECORD    TO  CLAIM-MASTER.
001255
001256     IF PROCI NOT = LOW-VALUES
001257         MOVE PROCI          TO CL-PROCESSOR-ID.
001258     IF PRICDI NOT = LOW-VALUES
001259         MOVE PRICDI         TO CL-PRIORITY-CD.
001260     IF SUPVI NOT = LOW-VALUES
001261         MOVE SUPVI          TO CL-SUPV-ATTN-CD.
001262     IF FILEI NOT = LOW-VALUES
001263         MOVE FILEI          TO CL-FILE-LOCATION.
001264
001265     MOVE SAVE-BIN-DATE      TO CL-LAST-MAINT-DT.
001266     MOVE PI-PROCESSOR-ID    TO CL-LAST-MAINT-USER.
001267     MOVE EIBTIME            TO CL-LAST-MAINT-HHMMSS.
001268     MOVE '3'                TO CL-LAST-MAINT-TYPE.
001269
001270     
      * EXEC CICS HANDLE CONDITION
001271*        DUPKEY (4310-CHANGE-KEY-EXIT)
001272*    END-EXEC.
      *    MOVE '"$$                   ! , #00005311' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303035333131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001273
001274     
      * EXEC CICS WRITE
001275*        FROM    (CLAIM-MASTER)
001276*        DATASET (W-FILE-ID)
001277*        RIDFLD  (MSTR-KEY)
001278*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005315' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035333135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001279
001280 4310-CHANGE-KEY-EXIT.
001281     EXIT.
001282
001283 4320-UPDATE-RECORD.
001284     
      * EXEC CICS HANDLE CONDITION
001285*        DUPKEY (4330-UPDATE-RECORD-EXIT)
001286*    END-EXEC.
      *    MOVE '"$$                   ! - #00005325' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303035333235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001287
001288     IF PRICDI NOT = LOW-VALUES
001289         MOVE PRICDI TO CL-PRIORITY-CD.
001290     IF SUPVI NOT = LOW-VALUES
001291         MOVE SUPVI TO CL-SUPV-ATTN-CD.
001292     IF FILEI NOT = LOW-VALUES
001293         MOVE FILEI TO CL-FILE-LOCATION.
001294
001295     MOVE SAVE-BIN-DATE          TO CL-LAST-MAINT-DT.
001296     MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER.
001297     MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.
001298     MOVE '3'                    TO CL-LAST-MAINT-TYPE.
001299
001300     
      * EXEC CICS REWRITE
001301*        FROM    (CLAIM-MASTER)
001302*        DATASET (W-FILE-ID)
001303*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005341' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035333431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001304
001305 4330-UPDATE-RECORD-EXIT.
001306     EXIT.
001307     EJECT
001308 4400-REWRITE-TS.
001309     
      * EXEC CICS WRITEQ TS
001310*        QUEUE (PI-EL1602-KEY)
001311*        FROM (EL160BO)
001312*        LENGTH (EL1602-LENGTH)
001313*        ITEM (PI-TS-COUNT)
001314*        REWRITE
001315*    END-EXEC.
      *    MOVE '*" IR  L              ''   #00005350' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035333530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 EL160BO, 
                 EL1602-LENGTH, 
                 PI-TS-COUNT, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001316
001317 4410-REWRITE-TS-EXIT.
001318     EXIT.
001319
001320     EJECT
001321 5000-UPDATE-TS.
001322     PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
001323     PERFORM 5010-MOVE-MSTR THRU 5010-MOVE-MSTR-EXIT.
001324     PERFORM 4400-REWRITE-TS THRU 4410-REWRITE-TS-EXIT.
001325
001326 5000-UPDATE-TS-EXIT.
001327     EXIT.
001328
001329 5010-MOVE-MSTR.
001330     MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.
001331     MOVE CL-CLAIM-TYPE          TO TYPEO.
001332     MOVE CL-CERT-PRIME          TO CERTO.
001333     MOVE CL-CERT-SFX            TO CERTSXO.
001334
001335     MOVE CL-CCN                 TO CREDCDO.
001336
001337     MOVE CL-CERT-CARRIER        TO CARRO.
001338     MOVE CL-CLAIM-STATUS        TO STATUSO.
001339     MOVE CL-PROCESSOR-ID        TO PROCO.
001340     MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.
001341     MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.
001342     MOVE CL-INSURED-MID-INIT    TO MMINITO.
001343     MOVE CL-INSURED-SEX-CD      TO SEXO.
001344
001345     IF CL-INSURED-BIRTH-DT GREATER THAN LOW-VALUES
001346         MOVE CL-INSURED-BIRTH-DT   TO DC-BIN-DATE-1
001347         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001348         PERFORM 9800-CONVERT-DATE
001349             THRU 9800-CONVERT-DATE-EXIT
001350         MOVE DC-GREG-DATE-1-EDIT TO BIRTHO
001351     ELSE
001352         MOVE SPACES             TO BIRTHO.
001353
001354     MOVE CL-SOC-SEC-NO          TO SOCIALO.
001355     MOVE CL-INSURED-OCC-CD      TO OCCO.
001356
001357     IF SINGLE-PREMIUM
001358         MOVE LIT-SP             TO PREMSO
001359     ELSE
001360         IF O-B-COVERAGE
001361             MOVE LIT-OB         TO PREMSO
001362         ELSE
001363             IF OPEN-END-COVERAGE
001364                 MOVE LIT-OE     TO PREMSO
001365             ELSE
001366                 MOVE SPACES     TO PREMSO.
001367
001368     MOVE CL-CAUSE-CD            TO CAUSEO.
001369
001370     IF CL-EST-END-OF-DISAB-DT GREATER THAN LOW-VALUES
001371         MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
001372         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001373         PERFORM 9800-CONVERT-DATE
001374             THRU 9800-CONVERT-DATE-EXIT
001375         MOVE DC-GREG-DATE-1-EDIT TO ENDO
001376     ELSE
001377         MOVE SPACES             TO ENDO.
001378
001379     IF CL-PAID-THRU-DT GREATER THAN LOW-VALUES
001380         MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
001381         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001382         PERFORM 9800-CONVERT-DATE
001383             THRU 9800-CONVERT-DATE-EXIT
001384         MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
001385     ELSE
001386         MOVE SPACES TO PDTHRUO.
001387
001388     MOVE CL-TOTAL-PAID-AMT      TO PDAMTO.
001389     MOVE CL-NO-OF-DAYS-PAID     TO NODAYSO.
001390     MOVE CL-NO-OF-PMTS-MADE     TO NOPMTSO.
001391
001392     IF CL-INCURRED-DT GREATER THAN LOW-VALUES
001393         MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
001394         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001395         PERFORM 9800-CONVERT-DATE
001396             THRU 9800-CONVERT-DATE-EXIT
001397         MOVE DC-GREG-DATE-1-EDIT TO INCO
001398     ELSE
001399         MOVE SPACES             TO INCO.
001400
001401     IF CL-REPORTED-DT GREATER THAN LOW-VALUES
001402         MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1
001403         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001404         PERFORM 9800-CONVERT-DATE
001405             THRU 9800-CONVERT-DATE-EXIT
001406         MOVE DC-GREG-DATE-1-EDIT TO REPO
001407     ELSE
001408         MOVE SPACES             TO REPO.
001409
001410     IF CL-FILE-ESTABLISH-DT GREATER THAN LOW-VALUES
001411         MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
001412         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001413         PERFORM 9800-CONVERT-DATE
001414             THRU 9800-CONVERT-DATE-EXIT
001415         MOVE DC-GREG-DATE-1-EDIT TO ESTO
001416     ELSE
001417         MOVE SPACES             TO ESTO.
001418
001419*    IF CL-LAST-PMT-DT GREATER THAN LOW-VALUES
001420*        MOVE CL-LAST-PMT-DT TO DC-BIN-DATE-1
001421*        MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001422*        PERFORM 9800-CONVERT-DATE
001423*            THRU 9800-CONVERT-DATE-EXIT
001424*        MOVE DC-GREG-DATE-1-EDIT TO LSTPMTO
001425*    ELSE
001426*        MOVE SPACES             TO LSTPMTO.
001427*    MOVE CL-LAST-PMT-AMT        TO LSTAMTO.
001428
001429     IF CL-LAST-MAINT-DT GREATER THAN LOW-VALUES
001430         MOVE CL-LAST-MAINT-DT TO DC-BIN-DATE-1
001431         MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
001432         PERFORM 9800-CONVERT-DATE
001433             THRU 9800-CONVERT-DATE-EXIT
001434         MOVE DC-GREG-DATE-1-EDIT TO MNTDTO
001435     ELSE
001436         MOVE SPACES             TO MNTDTO.
001437
001438     IF CL-LAST-MAINT-TYPE = SPACE
001439        MOVE LIT-SET-UP         TO MNTTYPEO
001440     ELSE
001441     IF CL-LAST-MAINT-TYPE = '1'
001442        MOVE LIT-PMT             TO MNTTYPEO
001443     ELSE
001444     IF CL-LAST-MAINT-TYPE = '2'
001445        MOVE LIT-LETTER          TO MNTTYPEO
001446     ELSE
001447     IF CL-LAST-MAINT-TYPE = '3'
001448        MOVE LIT-UPDATE          TO MNTTYPEO
001449     ELSE
001450     IF CL-LAST-MAINT-TYPE = '4'
001451        MOVE LIT-RESTORE         TO MNTTYPEO
001452     ELSE
001453     IF CL-LAST-MAINT-TYPE = '5'
001454        MOVE LIT-INC-CHG         TO MNTTYPEO
001455     ELSE
001456     IF CL-LAST-MAINT-TYPE = '6'
001457        MOVE LIT-CONV            TO MNTTYPEO
001458     ELSE
001459        MOVE SPACES              TO MNTTYPEO.
001460
001461     MOVE CL-PRIORITY-CD         TO PRICDO.
001462     MOVE CL-SUPV-ATTN-CD        TO SUPVO.
001463     MOVE CL-FILE-LOCATION       TO FILEO.
001464     MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
001465     MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
001466     MOVE PI-CONTROL-IN-PROGRESS TO PIKEYO.
001467     MOVE PI-UPDATE-BY           TO USERSAVO.
001468     MOVE PI-UPDATE-HHMMSS       TO TIMESAVO.
001469
001470 5010-MOVE-MSTR-EXIT.
001471     EXIT.
001472     EJECT
001473 8100-SEND-MAP.
001474     PERFORM 8120-FORMAT-TIME-DATE
001475             THRU 8130-FORMAT-TIME-DATE-EXIT.
001476
001477     IF PI-RETRIEVAL-FILE
001478         MOVE '- CLAIM MASTER (RETRIEVAL) -'
001479                                 TO TITLEO
001480         MOVE AL-SANOF           TO PROCA
001481                                    FILEA
001482                                    PRICDA
001483                                    SUPVA.
001484
001485     MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
001486     MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
001487
001488     
      * EXEC CICS SEND
001489*        MAP ('EL160B')
001490*        MAPSET ('EL160S')
001491*        ERASE
001492*        FREEKB
001493*        CURSOR
001494*    END-EXEC.
           MOVE 'EL160B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00005529' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'204620204820202020202C20' &
                X'2020233030303035353239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160BO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001495
001496     GO TO 9000-RETURN-TRANS.
001497
001498 8110-SEND-DATA.
001499     PERFORM 8120-FORMAT-TIME-DATE
001500             THRU 8130-FORMAT-TIME-DATE-EXIT.
001501
001502     IF PI-RETRIEVAL-FILE
001503         MOVE '- CLAIM MASTER (RETRIEVAL) -'
001504                                 TO TITLEO
001505         MOVE AL-SANOF           TO PROCA
001506                                    FILEA
001507                                    PRICDA
001508                                    SUPVA.
001509
001510     MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
001511     MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
001512
001513     
      * EXEC CICS SEND
001514*        MAP ('EL160B')
001515*        MAPSET ('EL160S')
001516*        DATAONLY
001517*        FREEKB
001518*        CURSOR
001519*    END-EXEC.
           MOVE 'EL160B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00005554' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'204620204820202020202C20' &
                X'2020233030303035353534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160BO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001520
001521     GO TO 9000-RETURN-TRANS.
001522
001523 8120-FORMAT-TIME-DATE.
001524     MOVE SAVE-DATE      TO DATEBO.
001525
001526     
      * EXEC CICS ASKTIME
001527*        ABSTIME(LCP-CICS-TIME)
001528*    END-EXEC.
      *    MOVE '0"A                   "   #00005567' TO DFHEIV0
           MOVE X'302241202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303035353637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001529
001530     
      * EXEC CICS FORMATTIME
001531*        ABSTIME(LCP-CICS-TIME)
001532*        TIME(LCP-TIME-OF-DAY-XX)
001533*    END-EXEC.
      *    MOVE 'j$(     (             #   #00005571' TO DFHEIV0
           MOVE X'6A2428202020202028202020' &
                X'202020202020202020202320' &
                X'2020233030303035353731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001534
001535     MOVE LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
001536     MOVE UN-HOURS       TO FOR-HOURS.
001537     MOVE UN-MINUTES     TO FOR-MINUTES.
001538     MOVE TIME-FORMATTED TO TIMEBO.
001539     MOVE LIT-MAP        TO PI-CURRENT-SCREEN-NO.
001540
001541     IF EMI-ERROR NOT = 0515
001542        MOVE PI-TS-COUNT         TO CONV-COUNT
001543        MOVE CONV-COUNT          TO EDIT-COUNT
001544        MOVE EDIT-COUNT          TO NOSCRNO.
001545
001546     MOVE PI-TS-COUNT-1 TO CONV-COUNT.
001547     MOVE CONV-COUNT    TO EDIT-COUNT.
001548     MOVE EDIT-COUNT    TO TOTSCRNO.
001549
001550 8130-FORMAT-TIME-DATE-EXIT.
001551     EXIT.
001552
001553 8200-RETURN-PRIOR.
001554     MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.
001555     GO TO 9200-XCTL.
001556
001557 8300-GET-HELP.
001558     MOVE LIT-HELP TO CALL-PGM.
001559     GO TO 9200-XCTL.
001560
001561 8400-RETURN-MASTER.
001562     PERFORM 8700-DELETEQ THRU 8700-DELETEQ-EXIT.
001563     MOVE LIT-MASTER TO CALL-PGM.
001564     GO TO 9200-XCTL.
001565
001566 8500-GET-ACT.
001567     MOVE PIKEYI TO PI-CONTROL-IN-PROGRESS.
001568
001569     
      * EXEC CICS WRITEQ TS
001570*        QUEUE (PI-KEY)
001571*        FROM (PROGRAM-INTERFACE-BLOCK)
001572*        LENGTH (PI-COMM-LENGTH)
001573*    END-EXEC.
      *    MOVE '*"     L              ''   #00005610' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035363130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001574
001575     MOVE LIT-ACT TO CALL-PGM.
001576     GO TO 9200-XCTL.
001577
001578 8700-DELETEQ.
001579      
      * EXEC CICS DELETEQ
001580*        QUEUE (PI-EL160-KEY)
001581*    END-EXEC.
      *    MOVE '*&                    #   #00005620' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035363230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001582      
      * EXEC CICS DELETEQ
001583*        QUEUE (PI-EL1602-KEY)
001584*    END-EXEC.
      *    MOVE '*&                    #   #00005623' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035363233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001585
001586 8700-DELETEQ-EXIT.
001587     EXIT.
001588
001589 8800-UNAUTHORIZED-ACCESS.
001590     MOVE UNACCESS-MSG TO LOGOFF-MSG.
001591     GO TO 8990-SEND-TEXT.
001592
001593 8810-PF23-ENTERED.
001594     PERFORM 8700-DELETEQ THRU 8700-DELETEQ-EXIT.
001595     MOVE EIBAID TO PI-ENTRY-CD-1.
001596     MOVE LIT-SIGN-OFF TO CALL-PGM.
001597     GO TO 9200-XCTL.
001598
001599 8820-XCTL-ERROR.
001600     
      * EXEC CICS HANDLE CONDITION
001601*        PGMIDERR (8990-SEND-TEXT)
001602*    END-EXEC.
      *    MOVE '"$L                   ! . #00005641' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303035363431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001603
001604     MOVE SPACE        TO PI-ENTRY-CD-1.
001605     MOVE CALL-PGM     TO PI-CALLING-PROGRAM   LOGOFF-PGM
001606     MOVE PGMIDERR-MSG TO LOGOFF-FILL.
001607     MOVE LIT-SIGN-OFF TO CALL-PGM.
001608     GO TO 9200-XCTL.
001609
001610 8990-SEND-TEXT.
001611     
      * EXEC CICS SEND TEXT
001612*        FROM (LOGOFF-TEXT)
001613*        LENGTH (LOGOFF-LENGTH)
001614*        ERASE
001615*        FREEKB
001616*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005652' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303035363532' TO DFHEIV0
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
           
001617
001618     GO TO 9100-RETURN-CICS.
001619     EJECT
001620 9000-RETURN-TRANS.
001621     
      * EXEC CICS RETURN
001622*        TRANSID (TRANS-ID)
001623*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001624*        LENGTH (PI-COMM-LENGTH)
001625*    END-EXEC.
      *    MOVE '.(CT                  ''   #00005662' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035363632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001626
001627     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
001628
001629 9100-RETURN-CICS.
001630     
      * EXEC CICS RETURN
001631*    END-EXEC.
      *    MOVE '.(                    ''   #00005671' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035363731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001632
001633     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
001634
001635 9200-XCTL.
001636     
      * EXEC CICS XCTL
001637*        PROGRAM (CALL-PGM)
001638*        COMMAREA (PROGRAM-INTERFACE-BLOCK)
001639*        LENGTH (PI-COMM-LENGTH)
001640*    END-EXEC.
      *    MOVE '.$C                   %   #00005677' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035363737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001641
001642 9800-CONVERT-DATE.
001643     MOVE SPACE TO DC-ERROR-CODE.
001644
001645     
      * EXEC CICS LINK
001646*        PROGRAM   (DATE-CONV)
001647*        COMMAREA  (DATE-CONVERSION-DATA)
001648*        LENGTH    (DC-COMM-LENGTH)
001649*    END-EXEC.
      *    MOVE '."C                   (   #00005686' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035363836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-CONV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001650
001651 9800-CONVERT-DATE-EXIT.
001652     EXIT.
001653
001654 9900-ERROR-FORMAT.
001655     IF EMI-ERRORS-COMPLETE
001656         GO TO 9900-ERROR-FORMAT-EXIT.
001657
001658     
      * EXEC CICS LINK
001659*        PROGRAM ('EL001')
001660*        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
001661*        LENGTH (EMI-COMM-LENGTH)
001662*    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00005699' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035363939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001663
001664 9900-ERROR-FORMAT-EXIT.
001665     EXIT.
001666
001667 9990-ABEND.
001668     
      * EXEC CICS LINK
001669*        PROGRAM('EL004')
001670*        COMMAREA(DFHEIBLK)
001671*        LENGTH(64)
001672*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00005709' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035373039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001673
001674     GO TO 9100-RETURN-CICS.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0130-TS-ERROR,
                     0130-TS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0510-WRITE-INITIAL-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0540-DELETE-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0560-READ-TEMP-STORAGE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0570-DELETE-INITIAL-2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1230-CREATE-NEW-ACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1275-CNTL-NOT-OPEN,
                     1280-NOT-FOUND,
                     1270-GET-PRINT-ID,
                     1285-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3010-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4140-PROC-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4310-CHANGE-KEY-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 4330-UPDATE-RECORD-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
