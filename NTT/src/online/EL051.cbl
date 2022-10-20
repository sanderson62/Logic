      *((program: EL051.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL051 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 03/13/95 13:23:45.
000007*                            VMOD=2.013
000008*
000009*
000010*AUTHOR.     LOGIC INC.
000011*            DALLAS, TEXAS.
000012
000013*DATE-COMPILED.
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
000024*REMARKS.    TRANSACTION - EXSE AND EXEB
000025*         THIS PROGRAM IS STARTED  FROM EL630 AND CICS.  IT'S
000026*         FUNCTION IS TO FEED PENDING BUSINESS RECORDS TO
000027*         THE EDIT PROGRAM.
000028
000029     EJECT
000030******************************************************************
000031*                   C H A N G E   L O G
000032*
000033* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000034*-----------------------------------------------------------------
000035*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000036* EFFECTIVE    NUMBER
000037*-----------------------------------------------------------------
000038* 110921  CR2021051200001  PEMA  Onbase Workflow project
000039******************************************************************
000040 ENVIRONMENT DIVISION.
000041 DATA DIVISION.
000042 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000043 77  FILLER  PIC X(32) VALUE '********************************'.
000044 77  FILLER  PIC X(32) VALUE '*     EL051  WORKING-STORAGE   *'.
000045 77  FILLER  PIC X(32) VALUE '*********** VMOD=2.013 *********'.
000046
000047 77  ELEN                        PIC S9(4)  COMP    VALUE +16.
000048 77  ws-sql-code                 pic s9(7) value zeros.
000049 77  ws-dis-sql-code             pic -9999999 value zeros.
000050
000051 01  P pointer.
000052 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
000053 01  var-ptr pointer.
000054 01  env-var-len                 pic 9(4)  binary.
000055
000056 01  WS-KIXSYS.
000057     05  WS-KIX-FIL1             PIC X(10).
000058     05  WS-KIX-APPS             PIC X(10).
000059     05  WS-KIX-ENV              PIC X(10).
000060     05  WS-KIX-MYENV            PIC X(10).
000061     05  WS-KIX-SYS              PIC X(10).
000062
000063*EXEC SQL
000064*   INCLUDE SQLDA
000065*END-EXEC
000066
000069*EXEC SQL
      *   INCLUDE SQLCA
      *END-EXEC
      *>>((file: SQLCA))
000001****************************************************************<*
000002* Copyright (c) 2016-2021 NTT DATA, Inc. All rights reserved.  *<*
000003* Users of NTT DATA Enterprise COBOL may freely                *<*
000004* redistribute this copybook.                                  *<*
000005****************************************************************<*
000006
000007 01  SQLCA GLOBAL.
000008     05  SQLCAID                PIC X(8).
000009     05  SQLCABC                PIC S9(9) COMP-5.
000010     05  SQLCODE                PIC S9(9) COMP-5.
000011     05  SQLERRM.
000012         49  SQLERRML           PIC S9(4) COMP-5.
000013         49  SQLERRMC           PIC X(254).
000014     05  SQLERRP                PIC X(8).
000015     05  SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
000016     05  SQLWARN.
000017         10 SQLWARN0            PIC X(1).
000018         10 SQLWARN1            PIC X(1).
000019         10 SQLWARN2            PIC X(1).
000020         10 SQLWARN3            PIC X(1).
000021         10 SQLWARN4            PIC X(1).
000022         10 SQLWARN5            PIC X(1).
000023         10 SQLWARN6            PIC X(1).
000024         10 SQLWARN7            PIC X(1).
000025     05  SQLSTATE               PIC X(5).
000026     05  SQLEXT                 PIC S9(5) COMP-3 VALUE 1.
      *<<((file: SQLCA))
000070
000072 EXEC SQL
          BEGIN DECLARE SECTION
000073 END-EXEC
000074
000075 01  svr                         pic x(32).
000076 01  database                    pic x(32).
000077 01  usr                         pic x(32).
000078 01  pass                        pic x(32).
000079 01  usr-pass                    pic x(64).
000080 01  ws-disp-code                pic s9(11).
000081
000082***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
000083***                                                            ***
000084***  These indicators are used to determine if a variable      ***
000085***  is passed nulls from sql. The indicator will be -1        ***
000086***  if the value on sql is nulls and +0 if the value is       ***
000087***  something other than nulls. Here is an example on how     ***
000088***  to use the indicator variables.                           ***
000089***                                                            ***
000090***     EXEC SQL                                               ***
000091***        fetch checkapp into                                 ***
000092***           :db-app-status :nu-app-status,                   ***
000093***           :db-app-by     :nu-app-by,                       ***
000094***           :db-app-date   :nu-app-date,                     ***
000095***           :db-app-batch  :nu-app-batch                     ***
000096***     END-EXEC                                               ***
000097***                                                            ***
000098***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
000099
000100 01  indicator-vaiables-for-nulls.
000101     05  nu-claim-no             pic s9(4) comp value +0.
000102     05  nu-app-by               pic s9(4) comp value +0.
000103     05  nu-exp-date             pic s9(4) comp value +0.
000104     05  nu-app-batch            pic s9(4) comp value +0.
000105
000107 EXEC SQL
          END DECLARE SECTION
000108 END-EXEC
000109
000110
000111 01  WORK-AREAS.
000112     12  ws-connect-sw               pic x  value ' '.
000113         88  connected-to-db             value 'Y'.
000114     12  ER-2617                 PIC 9(4)    VALUE 2617.
000115     12  WS-CSR-ID               PIC X(4)    VALUE SPACES.
000116     12  WS-PREV-BATCH           PIC X(6)    VALUE SPACES.
000117     12  LIT-2600                PIC 9(4)    VALUE 2600.
000118     12  LIT-2625                PIC 9(4)    VALUE 2625.
000119     12  LIT-2725                PIC 9(4)    VALUE 2725.
000120     12  LIT-2800                PIC 9(4)    VALUE 2800.
000121     12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
000122     12  LINK-001                PIC X(8)    VALUE 'EL001'.
000123     12  ELREPT-FILE-ID          PIC X(8)    VALUE 'ELREPT'.
000124     12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.
000125     12  ERPNDC-FILE-ID          PIC X(8)    VALUE 'ERPNDC'.
000126     12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.
000127     12  CERT-ID                 PIC X(8)    VALUE 'ELCERT'.
000128     12  PNDB-EDIT-PGM           PIC X(8)    VALUE 'EL050'.
000129     12  PNDC-EDIT-PGM           PIC X(8)    VALUE 'EL053'.
000130     12  PGM-NAME                PIC X(8)    VALUE SPACES.
000131     12  THIS-PGM                PIC X(8)    VALUE 'EL051'.
000132     12  ERPNDB-LENGTH           PIC S9(4)   COMP VALUE +585.
000133     12  ERPNDC-LENGTH           PIC S9(4)   COMP VALUE +500.
000134     12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.
000135     12  TRANS-DATA-LENGTH       PIC S9(4)   COMP VALUE +100.
000136     12  REC-COUNT               PIC S9(6)   COMP-3 VALUE +0.
000137     12  WS-WORK                 PIC S9(3)   COMP-3 VALUE +0.
000138     12  WS-REM                  PIC S9(6)   COMP-3 VALUE +0.
000139     12  PRT-CNT                 PIC 9       VALUE 4.
000140     12  WS-LINE-NUMBER          PIC 9(5)    COMP-3 VALUE ZERO.
000141     12  WS-PAGE                 PIC 9(5)    COMP-3 VALUE ZERO.
000142     12  WS-CURRENT-DATE         PIC X(8).
000143     12  SUB                     PIC 999     COMP-3 VALUE ZEROS.
000144     12  SUB1                    PIC 999     COMP-3 VALUE ZEROS.
000145     12  WS-SYNC-CNTR            PIC 999     COMP-3 VALUE ZEROS.
000146     12  WS-TIME-WORK            PIC S9(11)  COMP-3 VALUE ZERO.
000147     12  WS-START-TIME           PIC S9(11)  COMP-3 VALUE ZERO.
000148     12  WS-STOP-TIME            PIC S9(11)  COMP-3 VALUE ZERO.
000149     12  WS-LAST-TIME            PIC S9(11)  COMP-3 VALUE ZERO.
000150     12  WS-TIME                 PIC 9(7).
000151     12  FILLER    REDEFINES WS-TIME.
000152         16  FILLER              PIC X.
000153         16  WS-TIME-6           PIC X(6).
000154     12  FILLER    REDEFINES WS-TIME.
000155         16  FILLER              PIC X.
000156         16  WS-HOURS            PIC 99.
000157         16  WS-MINUTES          PIC 99.
000158         16  WS-SECONDS          PIC 99.
000159     12  ABEND-AREA              PIC X(72).
000160     12  ERPNDB-FILE-SW          PIC X     VALUE SPACE.
000161         88  ERPNDB-EOF      VALUE 'Y'.
000162         88  ERPNDB-EOF-COMP VALUE 'X'.
000163     12  ERPNDC-FILE-SW          PIC X     VALUE SPACE.
000164         88  ERPNDC-EOF      VALUE 'Y'.
000165         88  ERPNDC-EOF-COMP VALUE 'X'.
000166     12  DATA-EDITED-SW          PIC X     VALUE SPACE.
000167         88  DATA-EDITED     VALUE 'Y'.
000168     12  EDIT-PROCESS-SW         PIC X     VALUE SPACE.
000169         88  PROCESS-ENTIRE-FILE     VALUE 'F'.
000170         88  PROCESS-COMPANY         VALUE 'C'.
000171         88  PROCESS-BATCH           VALUE 'B'.
000172         88  BATCH-PROCESS-COMPLETE  VALUE 'Y'.
000173     12  FIRST-TIME-SW           PIC X     VALUE 'Y'.
000174         88  FIRST-TIME              VALUE 'Y'.
000175     12  WS-ERR-CODE.
000176         16  FILLER              PIC 99.
000177         16  WS-ERROR-SUB        PIC 99.
000178     12  PNDC-MSG PIC X(22)   VALUE '1PENDING CLAIMS EDIT  '.
000179     12  PRINT-CONTROL.
000180         16  SINGLE-SPACE        PIC X     VALUE SPACE.
000181         16  DOUBLE-SPACE        PIC X     VALUE ZERO.
000182         16  TRIPLE-SPACE        PIC X     VALUE '-'.
000183         16  SUPPRESS-SPACE      PIC X     VALUE '+'.
000184         16  TOP-OF-PAGE         PIC X     VALUE '1'.
000185     12  WS-COMPUTED-TOTALS      COMP-3.
000186         16  WS-LF-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
000187         16  WS-LF-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
000188         16  WS-AH-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
000189         16  WS-AH-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
000190         16  WS-LF-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
000191         16  WS-LF-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
000192         16  WS-AH-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
000193         16  WS-AH-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
000194         16  WS-ISSUE-CNT        PIC 9(5)      VALUE ZEROS COMP-3.
000195         16  WS-CANCEL-CNT       PIC 9(5)      VALUE ZEROS COMP-3.
000196     12  WS-LGX-CLAIM-USER       PIC X         VALUE SPACES.
000197         88  CO-HAS-CLAIMS                     VALUE 'Y'.
000198     12  TYPE-OF-EDIT            PIC X       VALUE SPACES.
000199         88  REEDIT-OR-RESTART-OR-FULL       VALUE 'X'.
000200
000201     12  WS-FILES-OPEN-SW        PIC X       VALUE SPACES.
000202         88  FILES-NOT-OPEN                  VALUE 'N'.
000203
000204     12  WS-DELAY-INTERVAL       PIC S9(7)     VALUE +2   COMP-3.
000205     12  PNDB-REC-COUNT          PIC S9(4)     VALUE +0.
000206
000207     12  WS-BATCH-NO.
000208         16  WS-BATCH-PREFIX         PIC XXX         VALUE SPACES.
000209         16  FILLER                  PIC XXX         VALUE SPACES.
000210
000211 01  ws-misc-work                pic x(1024) value spaces.
000212 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000213     88  RESP-NORMAL                    VALUE +0.
000214     88  resp-file-notfnd               value +12.
000215     88  RESP-NOTFND                    VALUE +13.
000216     88  resp-duprec                    value +14.
000217     88  resp-dupkey                    value +15.
000218     88  resp-invreq                    value +16.
000219     88  RESP-NOTOPEN                   VALUE +19.
000220     88  RESP-ENDFILE                   VALUE +20.
000221     88  resp-lengtherr                 value +22.
000222
000223 01  WS-RESPONSE2                PIC S9(8) COMP VALUE +0.
000224
000225 01  TEMPORARY-STORAGE-AREA.
000226     12  TS-QUEUE-ID.
000227         16  FILLER                  PIC XX          VALUE 'SE'.
000228         16  TS-QUEUE-COMPANY-ID     PIC X(3)        VALUE SPACES.
000229         16  FILLER                  PIC X(3)        VALUE SPACES.
000230
000231     12  TS-ITEM     COMP            PIC S9(4)       VALUE ZERO.
000232
000233     12  TS-RECORD.
000234         16  TS-COMPANY-CD           PIC X.
000235         16  TS-BATCH-NO             PIC X(6).
000236         16  TS-COMPANY-ID           PIC X(3).
000237         16  TS-RESTART-BATCH-NO     PIC X(6).
000238
000239     12  TS-RECORD-LENGTH  COMP      PIC S9(4) VALUE +16.
000240
000241     EJECT
000242 01  TRANS-DATA-MSG.
000243     12  TD-EDIT-TYPE                PIC X(7) VALUE 'BATCH'.
000244     12  TD-MSG      PIC X(20)  VALUE 'EDIT HAS COMPLETED  '.
000245     12  TD-REC-CNT              PIC ZZZ,ZZ9.
000246     12  FILLER                  PIC X(14) VALUE '  RECORDS FOR '.
000247     12  TD-COMPANY-ID           PIC X(3)  VALUE SPACES.
000248     12  FILLER      PIC X(20) VALUE ' LAST BATCH NO. WAS '.
000249     12  TD-LAST-BATCH           PIC X(6)  VALUE SPACES.
000250     12  FILLER      PIC X(09) VALUE ' TASK NO'.
000251     12  TD-TASK-NO              PIC 9(7)  VALUE ZERO.
000252     12  FILLER                      PIC X(14)       VALUE SPACES.
000253
000254 01  TRANS-DATA-MSG2.
000255     12  FILLER                      PIC X(10)    VALUE SPACES.
000256     12  FILLER      PIC X(9) VALUE ' STARTED '.
000257     12  TD-START-TIME.
000258         16  TD-START-HOURS          PIC 99 VALUE ZERO.
000259         16  FILLER                  PIC X VALUE '.'.
000260         16  TD-START-MINUTES        PIC 99 VALUE ZERO.
000261         16  FILLER                  PIC X VALUE '.'.
000262         16  TD-START-SECONDS        PIC 99 VALUE ZERO.
000263     12  FILLER      PIC X(9) VALUE ' ELAPSED '.
000264     12  TD-ELAPSED-TIME.
000265         16  TD-ELAPSED-HOURS        PIC 99 VALUE ZERO.
000266         16  FILLER                  PIC X VALUE '.'.
000267         16  TD-ELAPSED-MINUTES      PIC 99 VALUE ZERO.
000268         16  FILLER                  PIC X VALUE '.'.
000269         16  TD-ELAPSED-SECONDS      PIC 99 VALUE ZERO.
000270     12  FILLER      PIC X(10) VALUE ' LAST MSG '.
000271     12  TD-ELAPSED2-TIME.
000272         16  TD-ELAPSED2-HOURS       PIC 99 VALUE ZERO.
000273         16  FILLER                  PIC X VALUE '.'.
000274         16  TD-ELAPSED2-MINUTES     PIC 99 VALUE ZERO.
000275         16  FILLER                  PIC X VALUE '.'.
000276         16  TD-ELAPSED2-SECONDS     PIC 99 VALUE ZERO.
000277     12  FILLER  PIC X(38)       VALUE SPACES.
000278
000279     EJECT
000280 01  ACCESS-KEYS.
000281     12  ELCNTL-KEY.
000282         16  ELCNTL-COMPANY-ID   PIC XXX   VALUE SPACES.
000283         16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
000284         16  ELCNTL-FILLER       PIC X(3)  VALUE SPACES.
000285         16  ELCNTL-CARRIER      PIC X     VALUE SPACES.
000286         16  ELCNTL-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.
000287     12  ERPNDB-KEY.
000288         16  ERPNDB-COMPANY-CD   PIC X     VALUE SPACES.
000289         16  ERPNDB-BATCH        PIC X(6)  VALUE SPACES.
000290         16  ERPNDB-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.
000291         16  ERPNDB-SEQ-XX REDEFINES ERPNDB-SEQ-NO PIC XX.
000292         16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP  VALUE ZEROS.
000293     12  ERPNDC-KEY.
000294         16  ERPNDC-COMPANY-CD   PIC X     VALUE SPACES.
000295         16  ERPNDC-CARRIER      PIC X     VALUE SPACES.
000296         16  ERPNDC-GROUPING     PIC X(6)  VALUE SPACES.
000297         16  ERPNDC-STATE        PIC XX    VALUE SPACES.
000298         16  ERPNDC-ACCOUNT      PIC X(10) VALUE SPACES.
000299         16  ERPNDC-CERT-EFF-DT  PIC XX    VALUE SPACES.
000300         16  ERPNDC-CERT-NO      PIC X(11) VALUE SPACES.
000301         16  ERPNDC-CLAIM-NO     PIC X(7)  VALUE SPACES.
000302         16  ERPNDC-CHECK-NO     PIC X(7)  VALUE SPACES.
000303         16  ERPNDC-REC-TYPE     PIC X     VALUE SPACES.
000304         16  ERPNDC-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.
000305     12  CERT-KEY.
000306         16  CERT-COMPANY-CD             PIC X.
000307         16  CERT-CARRIER                PIC X.
000308         16  CERT-GROUPING               PIC X(6).
000309         16  CERT-STATE                  PIC XX.
000310         16  CERT-ACCOUNT                PIC X(10).
000311         16  CERT-CERT-EFF-DT            PIC XX.
000312         16  CERT-CERT-NO.
000313             20  CERT-CERT-PRIME         PIC X(10).
000314             20  CERT-CERT-SFX           PIC X.
000315
000316     EJECT
000317*    COPY ELCREPT.
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
000318     EJECT
000319
000320 01  PRT-LINE.
000321     12  PRT-MSG  PIC X(22)   VALUE '1PENDING BUSINESS EDIT'.
000322     12  FILLER   PIC X(11)   VALUE ' COMPLETED-'.
000323     12  PRT-CO   PIC X(31)   VALUE SPACES.
000324     12  PRT-DATE PIC X(8)    VALUE SPACES.
000325     12  FILLER   PIC X       VALUE SPACES.
000326     12  PRT-TIME PIC X(6)    VALUE SPACES.
000327
000328 01  START-LINE.
000329     12  START-MSG  PIC X(22)  VALUE '1PENDING BUSINESS EDIT'.
000330     12  FILLER     PIC X(11)  VALUE ' STARTED - '.
000331     12  START-CO   PIC X(31)  VALUE SPACES.
000332     12  START-DATE PIC X(8)   VALUE SPACES.
000333     12  FILLER     PIC X      VALUE SPACES.
000334     12  START-TIME PIC X(6)   VALUE SPACES.
000335
000336 01  TIME-UNFORMATTED.
000337     12  UN-HOURS                PIC XX.
000338     12  UN-MINUTES              PIC XX.
000339     12  FILLER                  PIC XX.
000340
000341 01  TIME-FORMATTED.
000342     12  FOR-HOURS               PIC XX.
000343     12  FILLER                  PIC X       VALUE ':'.
000344     12  FOR-MINUTES             PIC XX.
000345     EJECT
000346*    COPY ELCDATE.
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
000347     EJECT
000348*    COPY ELCAID.
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
000349 01  FILLER    REDEFINES DFHAID.
000350     12  FILLER              PIC X(8).
000351     12  PF-VALUES           PIC X       OCCURS 2.
000352     EJECT
000353*    COPY ELCEMIB.
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
000354     EJECT
000355*    COPY ELCJPFX.
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
000356                                PIC X(585).
000357     EJECT
000358 01  BATCH-TO-PROCESS            VALUE SPACES.
000359     05  EDIT-COMPANY-CD         PIC X.
000360     05  EDIT-BATCH              PIC X(6).
000361     05  EDIT-COMPANY-ID         PIC XXX.
000362     05  EDIT-RESTART-BATCH      PIC X(6).
000363     EJECT
000364 01  EDIT-WORK-AREAS.
000365     05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
000366     05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
000367     05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
000368     05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
000369     EJECT
000370*    COPY ERCPNDB REPLACING
000371*    PENDING-BUSINESS       BY PNDB-EDIT-PASS-AREA.
000372*                               COPY ERCPNDB.
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
000373 01  PNDB-EDIT-PASS-AREA.
000374     12  WK-PENDING-BUSINESS          PIC X(585) VALUE SPACES.
000375* COPYBOOK FOR ADDITIONAL DFHCOMMAREA WK-WORK-AREA.
000377*    COPY ELC50W1 REPLACING WK-CANCEL-EXIT-DT
      *                     BY  WK-CURRENT-DATE.
      *>>((file: ELC50W1))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                             ELC50W1                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.004                          *
000007*                                                                *
000008*       THIS COPYBOOK IS USED BY EL050, EL051, AND EL517.        *
000009*                                                                *
000010*                                                                *
000011******************************************************************
000012
000013     12  WK-WORK-AREA.
000014         16  WK-CNTL-RECORD-FOUND-SW  PIC X   VALUE ' '.
000015         16  WK-LAST-CARRIER          PIC X   VALUE ' '.
000016         16  WK-CR-REM-TERM-CALC      PIC X   VALUE ' '.
000017         16  WK-CR-R78-METHOD         PIC X   VALUE ' '.
000018         16  WK-CO-MAX-CAP          PIC S9(3)V99  COMP-3 VALUE +0.
000019         16  WK-CO-TOL-CLAIM        PIC S9(3)V99  COMP-3 VALUE +0.
000020         16  WK-CO-TOL-PREM         PIC S9(3)V99  COMP-3 VALUE +0.
000021         16  WK-CO-TOL-REFUND       PIC S9(3)V99  COMP-3 VALUE +0.
000022         16  WK-CO-TOL-PREM-PCT     PIC S9V9(4)   COMP-3 VALUE +0.
000023         16  WK-CO-TOL-REFUND-PCT   PIC S9V9(4)   COMP-3 VALUE +0.
000024         16  WK-CO-PREM-REJECT-SW     PIC X   VALUE ' '.
000025         16  WK-CO-REF-REJECT-SW      PIC X   VALUE ' '.
000026         16  WK-BIRTH-DATE-INPUT      PIC X   VALUE ' '.
000027         16  WK-JOINT-AGE-INPUT       PIC X   VALUE ' '.
000028         16  WK-CURRENT-MONTH-END     PIC XX     VALUE LOW-VALUE.
000029         16  WK-CREDIT-EDIT-CONTROLS.
000030             20  WK-MIN-PREMIUM       PIC S9(3)V99  COMP-3.
000031             20  WK-MIN-AGE           PIC 99  VALUE 00.
000032             20  WK-DEFAULT-AGE       PIC 99  VALUE 00.
000033             20  WK-MIN-TERM          PIC S9(3) COMP-3 VALUE +0.
000034             20  WK-MAX-TERM          PIC S9(3) COMP-3 VALUE +0.
000035             20  WK-DEFAULT-SEX       PIC X  VALUE ' '.
000036         16  WK-CURRENT-DATE        PIC XX VALUE LOW-VALUE.
000037         16  WK-SAVE-REIN-DATA.
000038             20  WK-REIN-TABLE        PIC X(3) VALUE SPACES.
000039             20  WK-REIN-ST-AH        PIC X(2) VALUE SPACES.
000040             20  WK-REIN-ST-LF        PIC X(2) VALUE SPACES.
000041         16  WK-ENTRY-SW              PIC X    VALUE ' '.
000042             88  ENTRY-FROM-EL6311       VALUE '6'.
000043         16  WK-REM-TRM-CALC-OPTION   PIC X    VALUE ' '.
000044         16  WK-DEFAULT-APR           PIC S9(3)V9(4) COMP-3.
000045     12  WK-RECORD-ADDRESSES.
000046         16  WK-ACCT-ADDR             PIC S9(8) COMP VALUE ZEROS.
000047         16  WK-LIFE-EDIT-ADDR        PIC S9(8) COMP VALUE ZEROS.
000048         16  WK-AH-EDIT-ADDR          PIC S9(8) COMP VALUE ZEROS.
000049         16  WK-LIFE-BEN-ADDR         PIC S9(8) COMP VALUE ZEROS.
000050         16  WK-AH-BEN-ADDR           PIC S9(8) COMP VALUE ZEROS.
000051         16  WK-STATE-ADDR            PIC S9(8) COMP VALUE ZEROS.
000052         16  WK-PLAN-ADDR             PIC S9(8) COMP VALUE ZEROS.
000053         16  WK-FORM-ADDR             PIC S9(8) COMP VALUE ZEROS.
000054     12  WK-OVER-SHORT-AREA.
000055         16  WK-REFUND-OVS-AMT        PIC S999V99 COMP-3 VALUE +0.
000056         16  WK-REFUND-OVS-PCT        PIC S9V9(4) COMP-3 VALUE +0.
000057         16  WK-CSR-SESSION-SW        PIC X  VALUE ' '.
000058             88  WK-CSR-EDIT-SESSION       VALUE 'Y'.
000059         16  FILLER                   PIC X(5) VALUE SPACES.
      *<<((file: ELC50W1))
000378     12  EDIT-CRITERIA-WORK-AREA      PIC X(352) VALUE SPACES.
000379     EJECT
000381*    COPY ERCPNDC REPLACING
      *    PENDING-CLAIMS          BY PNDC-EDIT-PASS-AREA.
      *>>((file: ERCPNDC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPNDC                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.004                          *
000007*                                                                *
000008*   FILE DESCRIPTION = PENDING CLAIM TRANSACTIONS                *
000009*                      PAYMENTS, RESERVES, EXPENSES              *
000010*                                                                *
000011*   FILE TYPE = VSAM,KSDS                                        *
000012*   RECORD SIZE = 500  RECFORM = FIXED                           *
000013*                                                                *
000014*   BASE CLUSTER = ERPNDC                         RKP=2,LEN=50   *
000015*       ALTERNATE PATHS = NONE                                   *
000016*                                                                *
000017*   LOG = YES                                                    *
000018*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000019******************************************************************
000020
000021 01  PNDC-EDIT-PASS-AREA.
000022     12  PC-RECORD-ID                     PIC XX.
000023         88  VALID-PC-ID                      VALUE 'PC'.
000024
000025     12  PC-CONTROL-PRIMARY.
000026         16  PC-COMPANY-CD                PIC X.
000027         16  PC-CARRIER                   PIC X.
000028         16  PC-GROUPING.
000029             20  PC-GROUPING-PREFIX       PIC XXX.
000030             20  PC-GROUPING-PRIME        PIC XXX.
000031         16  PC-STATE                     PIC XX.
000032         16  PC-ACCOUNT.
000033             20  PC-ACCOUNT-PREFIX        PIC X(4).
000034             20  PC-ACCOUNT-PRIME         PIC X(6).
000035         16  PC-CERT-EFF-DT               PIC XX.
000036         16  PC-CERT-NO.
000037             20  PC-CERT-PRIME            PIC X(10).
000038             20  PC-CERT-SFX              PIC X.
000039         16  PC-CLAIM-NO                  PIC X(7).
000040         16  PC-CHECK-NO                  PIC X(7).
000041
000042         16  PC-RECORD-TYPE               PIC X.
000043             88  PC-CLAIMS                    VALUE '1'.
000044             88  PC-RESERVES                  VALUE '2'.
000045         16  PC-RECORD-SEQUENCE           PIC S9(4)     COMP.
000046
000047     12  PC-LAST-MAINT-DT                 PIC XX.
000048     12  PC-LAST-MAINT-BY                 PIC X(4).
000049     12  PC-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000050
000051     12  PC-CLAIM-RECORD.
000052         16  PC-CLAIM-TYPE                PIC X.
000053             88  PC-LF-CLAIM                  VALUE '1'.
000054             88  PC-AH-CLAIM                  VALUE '2'.
000055             88  PC-OB-LF-CLAIM               VALUE '3'.
000056             88  PC-OB-AH-CLAIM               VALUE '4'.
000057         16  PC-PAYMENT-DT                PIC XX.
000058         16  PC-PAID-THRU-DT              PIC XX.
000059         16  PC-REPORTED-DT               PIC XX.
000060         16  PC-INCURRED-DT               PIC XX.
000061         16  PC-NO-OF-DAYS-PAID           PIC S9(3)     COMP-3.
000062         16  PC-CLAIM-PAYMENT             PIC S9(7)V99  COMP-3.
000063         16  PC-AGE-AT-CLAIM              PIC 99.
000064         16  FILLER                       PIC XX.
000065         16  PC-PAYMENT-TYPE              PIC X.
000066             88  PC-PARTIAL-PAYMENT           VALUE '1'.
000067             88  PC-FINAL-PAYMENT             VALUE '2'.
000068             88  PC-LUMP-SUM-PAYMENT          VALUE '3'.
000069             88  PC-ADDITIONAL-PAYMENT        VALUE '4'.
000070             88  PC-CHARGEBLE-EXPENSE         VALUE '5'.
000071             88  PC-NON-CHARGEBLE-EXPENSE     VALUE '6'.
000072             88  PC-VOIDED-PAYMENT            VALUE '9'.
000073
000074         16  PC-FUTURE-RESERVE-AMT        PIC S9(7)V99  COMP-3.
000075         16  PC-IBNR-RESERVE-AMT          PIC S9(7)V99  COMP-3.
000076         16  PC-PTC-RESERVE-AMT           PIC S9(7)V99  COMP-3.
000077         16  PC-MANUAL-RESERVE-AMT        PIC S9(7)V99  COMP-3.
000078
000079         16  PC-SV-CARRIER                PIC X.
000080         16  PC-SV-GROUPING               PIC X(6).
000081         16  PC-SV-STATE                  PIC XX.
000082
000083         16  PC-VOID-SW                   PIC X.
000084             88  PC-PUT-CERT-INFORCE          VALUE '1'.
000085
000086         16  PC-CAUSE-CODE                PIC X(6).
000087         16  FILLER                       PIC X(48).
000088
000089         16  PC-CLAIMED-CERT-DATA.
000090             20  PC-CC-INSURED-NAME.
000091                 24  PC-CC-LAST-NAME      PIC X(15).
000092                 24  PC-CC-INITIALS       PIC XX.
000093             20  PC-CC-INSURED-AGE        PIC S99.
000094             20  PC-CC-INSURED-SEX        PIC X.
000095             20  PC-CC-ORIG-TERM          PIC S999        COMP-3.
000096             20  PC-CC-LF-BENEFIT-CD      PIC XX.
000097             20  PC-CC-LIFE-BENEFIT-AMT   PIC S9(9)V99    COMP-3.
000098             20  PC-CC-ALT-LF-BENEFIT-AMT PIC S9(9)V99    COMP-3.
000099             20  PC-CC-LIFE-PREMIUM       PIC S9(7)V99    COMP-3.
000100             20  PC-CC-AH-BENEFIT-CD      PIC XX.
000101             20  PC-CC-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
000102             20  PC-CC-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000103             20  PC-CC-RATE-CLASS         PIC XX.
000104             20  PC-CC-RATE-DEV           PIC XXX.
000105             20  PC-CC-OB-FLAG            PIC X.
000106                 88  PC-CC-OB                VALUE 'B'.
000107             20  PC-CC-AH-POLICY-STATUS   PIC X.
000108                 88  PC-CCA-POLICY-IS-ACTIVE        VALUE '1' '3'
000109                                               '4' '5' '9' '2'.
000110                 88  PC-CCA-NORMAL-ENTRY            VALUE '1'.
000111                 88  PC-CCA-POLICY-PENDING          VALUE '2'.
000112                 88  PC-CCA-POLICY-IS-RESTORE       VALUE '3'.
000113                 88  PC-CCA-CONVERSION-ENTRY        VALUE '4'.
000114                 88  PC-CCA-POLICY-IS-REISSUE       VALUE '5'.
000115                 88  PC-CCA-LUMP-SUM-DISAB          VALUE '6'.
000116                 88  PC-CCA-DEATH-CLAIM-APPLIED     VALUE '7'.
000117                 88  PC-CCA-CANCEL-APPLIED          VALUE '8'.
000118                 88  PC-CCA-REIN-ONLY               VALUE '9'.
000119             20  PC-CC-LF-POLICY-STATUS   PIC X.
000120                 88  PC-CCL-POLICY-IS-ACTIVE        VALUE '1' '3'
000121                                               '4' '5' '9' '2'.
000122                 88  PC-CCL-NORMAL-ENTRY            VALUE '1'.
000123                 88  PC-CCL-POLICY-PENDING          VALUE '2'.
000124                 88  PC-CCL-POLICY-IS-RESTORE       VALUE '3'.
000125                 88  PC-CCL-CONVERSION-ENTRY        VALUE '4'.
000126                 88  PC-CCL-POLICY-IS-REISSUE       VALUE '5'.
000127                 88  PC-CCL-LUMP-SUM-DISAB          VALUE '6'.
000128                 88  PC-CCL-DEATH-CLAIM-APPLIED     VALUE '7'.
000129                 88  PC-CCL-CANCEL-APPLIED          VALUE '8'.
000130                 88  PC-CCL-REIN-ONLY               VALUE '9'.
000131             20  PC-CC-PAY-FREQUENCY      PIC 99.
000132             20  PC-CC-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
000133             20  PC-CC-SOC-SEC-NO         PIC X(11).
000134             20  PC-CC-MEMBER-NO          PIC X(12).
000135             20  PC-CC-INT-CODE           PIC X.
000136                 88  PC-CC-ADD-ON                  VALUE 'A'.
000137                 88  PC-CC-SIMPLE                  VALUE 'S'.
000138             20  PC-CC-CAPPED-TERM        PIC 999.
000139             20  PC-CC-PRIOR-LUMP-PMT     PIC S9(7)V99  COMP-3.
000140             20  PC-CC-PRIOR-DEATH-AMT    PIC S9(9)V99  COMP-3.
000141             20  PC-CC-CANCEL-DT          PIC XX.
000142             20  PC-CC-DEATH-DT           PIC XX.
000143             20  PC-CC-SETTLEMENT-DT      PIC XX.
000144             20  PC-CC-PRIOR-STATUS       PIC X.
000145             20  PC-CC-CERT-ENTRY-STATUS  PIC X.
000146         16  PC-TRLR-SEQ-NO               PIC S9(4)     COMP.
000147         16  PC-CC-CLP-STATE              PIC XX.
000148         16  FILLER                       PIC X(14).
000149         16  PC-REMAINING-BENEFIT         PIC S9(9)V99  COMP-3.
000150         16  PC-REMAINING-TERM            PIC S9(3)     COMP-3.
000151         16  FILLER                       PIC X(34).
000152
000153     12  PC-RECORD-STATUS.
000154         16  PC-CREDIT-SELECT-DT          PIC XX.
000155         16  PC-CREDIT-ACCEPT-DT          PIC XX.
000156         16  FILLER                       PIC XX.
000157         16  PC-FATAL-FLAG                PIC X.
000158             88  PC-FATAL-ERRORS             VALUE 'X'.
000159         16  PC-FORCE-CODE                PIC X.
000160             88  PC-FORCE-OFF                VALUE ' ' '0'.
000161             88  PC-CLAIM-FORCE              VALUE '6' '7'
000162                                                     '8'.
000163         16  PC-FORCE-ER-CD               PIC X.
000164             88  PC-FORCE-ERRORS             VALUE 'F'.
000165             88  PC-UNFORCED-ERRORS          VALUE 'X'.
000166         16  PC-WARN-ER-CD                PIC X.
000167             88  PC-WARNING-ERRORS           VALUE 'W'.
000168         16  PC-LF-OVERRIDE-L1            PIC X.
000169         16  PC-AH-OVERRIDE-L1            PIC X.
000170         16  FILLER                       PIC X(17).
000171         16  PC-CERT-UPDATE-SW            PIC X.
000172             88  PC-CERT-DATA-CAPTURED       VALUE '1'.
000173         16  PC-COMPANY-ID                PIC XXX.
000174         16  PC-INPUT-DT                  PIC XX.
000175
000176     12  PC-ERROR-FLAGS.
000177         16  PC-STANDARD-ERRORS.
000178             20  PC-STD-ERROR-FLAGS   OCCURS 25 TIMES PIC X.
000179         16  PC-TRANSACTION-ERRORS.
000180             20  PC-TRN-ERROR-FLAGS   OCCURS 75 TIMES PIC X.
000181
000182     12  PC-ERR-FLAGS-R REDEFINES  PC-ERROR-FLAGS.
000183         16  PC-ERR-FLAG              OCCURS 100 TIMES PIC X.
000184
000185     12  FILLER                           PIC X(25).
000186
000187******************************************************************
      *<<((file: ERCPNDC))
000382     12  WK-PC-WORK-AREA.
000383         16  WK-PC-CNTL-RECORD-FOUND-SW  PIC X.
000384         16  WK-PC-LAST-CARRIER          PIC X.
000385         16  WK-PC-CERT-ACCESS-CNTL      PIC X.
000386         16  WK-PC-CO-CLAIM-REJECT-SW    PIC X.
000387         16  WK-PC-CLAIM-SYSTEM-SW       PIC X.
000388         16  WK-PC-CO-TOL-CLAIM          PIC S9(3)V99  COMP-3.
000389         16  WK-PC-RESERVE-CONTROLS      PIC X(4).
000390         16  WK-PC-CREDIT-EDIT-CONTROLS  PIC X(12).
000391     12  WK-PC-RECORD-ADDRESSES.
000392         16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.
000393         16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.
000394     12  WK-MISC.
000395         16  WK-PC-REM-TRM-CALC-OPTION   PIC X.
000396         16  FILLER                      PIC X(20).
000397     EJECT
000398
000399
000400*    COPY ELCCNTL.
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
000401     EJECT
000402*    COPY ELCCERT.
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
000403     EJECT
000404
000405 01  work-flow-pass-area.
000406     05  pa-rec-type             pic x(4).
000407     05  pa-company-id           pic xxx.
000408     05  pa-rest-of-record       pic x(600).
000409
000410 01  ERROR-SEVERITY-CODES.
000411     12  STD-ERR-SEVERITY            OCCURS 25 TIMES
000412                                     INDEXED BY SINDEX PIC X.
000413     12  TRN-ERR-SEVERITY            OCCURS 150 TIMES
000414                                     INDEXED BY TINDEX PIC X.
000415
000416 01  connect-string              pic x(60) value spaces.
000417 01  sqlconnect-parms.
000418     05  p-sql-server            PIC X(30).
000419     05  p-sql-database          PIC X(30).
000420     05  p-connect-return-code   pic s9(5) comp-5.
000421     05  p-sql-return-message    pic x(256).
000422
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
000424 01  DFHCOMMAREA                     PIC X(1024).
000425 01  var  pic x(30).
000426*01 PARM-LIST .
000427*    02  FILLER              PIC S9(8)   COMP.
000428*    02  ELCNTL-POINTER      PIC S9(8)   COMP.
000429*    02  CERT-POINTER        PIC S9(8)   COMP.
000430     EJECT
000431
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL051' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000432 VCOBOL-DUMMY-PROCEDURE.
000433
000434     display ' Entering Program EL051 '
000435
000436     
      * EXEC CICS HANDLE CONDITION
000437*        ERROR    (8300-ABEND)
000438*        PGMIDERR (9900-ERROR-FORMAT)
000439*        NOTFND   (0210-PROCESS-FILE)
000440*        ENDDATA  (0210-PROCESS-FILE)
000441*    END-EXEC.
      *    MOVE '"$.LI&                ! " #00003695' TO DFHEIV0
           MOVE X'22242E4C4926202020202020' &
                X'202020202020202020202120' &
                X'2220233030303033363935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000442
000443     
      * EXEC CICS RETRIEVE
000444*        INTO   (BATCH-TO-PROCESS)
000445*        LENGTH (ELEN)
000446*        resp   (ws-response)
000447*        resp2  (ws-response2)
000448*    END-EXEC
      *    MOVE '0*I L                 &  N#00003702' TO DFHEIV0
           MOVE X'302A49204C20202020202020' &
                X'202020202020202020202620' &
                X'204E233030303033373032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BATCH-TO-PROCESS, 
                 ELEN, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           MOVE EIBRESP2 TO ws-response2
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000449
000450     display ' retrieve response ' ws-response ' ' ws-response2
000451
000452     display ' Pass area **' batch-to-process '**'
000453
000454     if not resp-normal
000455        display ' not normal, returning '
000456        go to 9999-return-cics
000457     end-if
000458
000459     set P to address of KIXSYS
000460     CALL "getenv" using by value P returning var-ptr
000461     if var-ptr = null then
000462        display ' kixsys not set '
000463     else
000464        set address of var to var-ptr
000465        move 0 to env-var-len
000466        inspect var tallying env-var-len
000467          for characters before X'00'
000468        unstring var (1:env-var-len) delimited by '/'
000469           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
000470              WS-KIX-SYS
000471        end-unstring
000472     end-if
000473
000474     display ' ENVIRONMENT ' ws-kix-myenv
000475     MOVE BATCH-TO-PROCESS       TO  TS-RECORD.
000476
000477     MOVE EDIT-COMPANY-ID        TO  TS-QUEUE-COMPANY-ID.
000478
000479     IF TS-RESTART-BATCH-NO NOT NUMERIC
000480         MOVE LOW-VALUES         TO  TS-RESTART-BATCH-NO.
000481
000482     
      * EXEC CICS WRITEQ TS
000483*        QUEUE  (TS-QUEUE-ID)
000484*        ITEM   (TS-ITEM)
000485*        FROM   (TS-RECORD)
000486*        LENGTH (TS-RECORD-LENGTH)
000487*    END-EXEC.
      *    MOVE '*" I   L              ''   #00003741' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033373431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000488
000489     
      * EXEC CICS SYNCPOINT
000490*    END-EXEC.
      *    MOVE '6"                    !   #00003748' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303033373438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000491
000492     IF EDIT-BATCH = SPACES
000493         MOVE 'C'                TO EDIT-PROCESS-SW
000494       ELSE
000495         MOVE 'B'                TO EDIT-PROCESS-SW.
000496
000497     GO TO 1000-PROCESS-FILE.
000498
000499 0210-PROCESS-FILE.
000500     MOVE 'F'                    TO EDIT-PROCESS-SW.
000501
000502     EJECT
000503 1000-PROCESS-FILE.
000504
000505     display ' EIBTRNID = ' EIBTRNID
000506
000507     IF EIBTRNID = 'EXEB' OR 'XXEB'
000508         MOVE 'BATCH'          TO  TD-EDIT-TYPE
000509         
      * EXEC CICS ENQ
000510*            RESOURCE (EIBTRNID)
000511*            LENGTH   (4)
000512*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '2$L                   $   #00003768' TO DFHEIV0
           MOVE X'32244C202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033373638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EIBTRNID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000513      ELSE
000514       
      * EXEC CICS ENQ
000515*           RESOURCE   (EDIT-COMPANY-CD)
000516*           LENGTH     (1)
000517*      END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '2$L                   $   #00003773' TO DFHEIV0
           MOVE X'32244C202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033373733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EDIT-COMPANY-CD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000518       MOVE 'SYSTEM '            TO  TD-EDIT-TYPE.
000519
000520     MOVE REC-COUNT              TO  TD-REC-CNT.
000521     MOVE EDIT-COMPANY-ID        TO  TD-COMPANY-ID.
000522     MOVE EDIT-BATCH             TO  TD-LAST-BATCH.
000523     MOVE EIBTASKN               TO  TD-TASK-NO.
000524
000525     
      * EXEC CICS ASKTIME
000526*    END-EXEC.
      *    MOVE '0"                    "   #00003784' TO DFHEIV0
           MOVE X'302220202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303033373834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000527
000528     MOVE EIBTIME                TO  WS-TIME.
000529     COMPUTE WS-START-TIME = (WS-HOURS * 3600) +
000530                             (WS-MINUTES * 60) + WS-SECONDS.
000531
000532     MOVE WS-HOURS               TO  TD-START-HOURS.
000533     MOVE WS-MINUTES             TO  TD-START-MINUTES.
000534     MOVE WS-SECONDS             TO  TD-START-SECONDS.
000535
000536     
      * EXEC CICS WRITEQ TD
000537*        QUEUE  ('CSMT')
000538*        FROM   (TRANS-DATA-MSG)
000539*        LENGTH (TRANS-DATA-LENGTH)
000540*    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00003795' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303033373935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000541
000542     
      * EXEC CICS WRITEQ TD
000543*        QUEUE  ('CSMT')
000544*        FROM   (TRANS-DATA-MSG2)
000545*        LENGTH (TRANS-DATA-LENGTH)
000546*    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00003801' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303033383031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG2, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000547
000548     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000549     MOVE '5'                    TO DC-OPTION-CODE.
000550     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
000551     MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.
000552     MOVE DC-BIN-DATE-1          TO WK-CURRENT-DATE.
000553     MOVE WS-CURRENT-DATE        TO PRT-DATE
000554                                    START-DATE.
000555     MOVE ZEROS                  TO WK-ACCT-ADDR
000556                                    WK-LIFE-EDIT-ADDR
000557                                    WK-AH-EDIT-ADDR
000558                                    WK-LIFE-BEN-ADDR
000559                                    WK-AH-BEN-ADDR
000560                                    WK-STATE-ADDR
000561                                    WK-PC-ACCT-ADDR
000562                                    WK-PC-STATE-ADDR
000563                                    WK-PLAN-ADDR
000564                                    WK-FORM-ADDR.
000565
000566     MOVE SPACE                  TO WK-CNTL-RECORD-FOUND-SW
000567                                    WK-PC-CNTL-RECORD-FOUND-SW
000568                                    ERROR-SEVERITY-CODES.
000569
000570     IF PROCESS-ENTIRE-FILE
000571         MOVE LOW-VALUES         TO ERPNDB-KEY
000572                                    ERPNDC-KEY
000573         MOVE ZEROS              TO ERPNDB-SEQ-NO
000574                                    ERPNDC-SEQ-NO
000575     ELSE
000576         MOVE EDIT-COMPANY-CD    TO ERPNDB-COMPANY-CD
000577                                    ERPNDC-COMPANY-CD
000578         IF EDIT-RESTART-BATCH = SPACES OR 'REEDIT'
000579            MOVE EDIT-BATCH      TO ERPNDB-BATCH
000580         ELSE
000581            MOVE EDIT-RESTART-BATCH TO ERPNDB-BATCH.
000582
000583     IF EDIT-BATCH         = SPACES  OR
000584        EDIT-RESTART-BATCH = 'REEDIT'
000585        MOVE 'X'                 TO TYPE-OF-EDIT
000586        PERFORM 6000-WRITE-EL051-MESSAGE THRU 6999-EXIT.
000587
000588 1100-PROCESS-PNDB-LOOP.
000589     MOVE ERPNDB-BATCH           TO WS-PREV-BATCH.
000590     PERFORM 3200-READ-NEXT-RECORD THRU 3290-EXIT.
000591
000592     IF ERPNDB-EOF
000593       OR ERPNDB-EOF-COMP
000594         GO TO 1200-WRITE-REPORT.
000595
000596     IF NOT PB-BATCH-TRAILER
000597         ADD 1                  TO WS-SYNC-CNTR
000598                                   REC-COUNT
000599         PERFORM 2000-CALL-PNDB-EDIT-PROGRAM THRU  2090-EXIT.
000600
000601     if pb-issue
000602        perform 1800-work-flow   thru 1800-exit
000603     end-if
000604
000605     PERFORM 4000-SET-PNDB-ERROR-FLAGS THRU 4900-EXIT.
000606
000607     IF WS-SYNC-CNTR = 32
000608        MOVE ZEROS               TO WS-SYNC-CNTR
000609        
      * EXEC CICS SYNCPOINT
000610*       END-EXEC.
      *    MOVE '6"                    !   #00003868' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303033383638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000611
000612     IF PROCESS-COMPANY AND NOT PB-BATCH-TRAILER
000613         DIVIDE REC-COUNT BY 500 GIVING WS-WORK REMAINDER WS-REM
000614         IF WS-REM = ZEROS
000615            MOVE REC-COUNT           TO TD-REC-CNT
000616            MOVE EDIT-COMPANY-ID TO TD-COMPANY-ID
000617            MOVE ERPNDB-BATCH TO TD-LAST-BATCH
000618                                 TS-RESTART-BATCH-NO
000619            PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT
000620            ADD +1 TO WS-LINE-NUMBER
000621            MOVE ERPNDB-COMPANY-CD  TO RF-COMPANY-CD
000622            MOVE '1'             TO RF-RECORD-TYPE
000623            MOVE 'EL051'         TO RF-REPORT-ID
000624            MOVE TRANS-DATA-MSG  TO RF-DATA-133
000625            PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT
000626
000627            PERFORM 5950-UPDATE-TYPE-2 THRU 5959-EXIT
000628            
      * EXEC CICS WRITEQ TS REWRITE
000629*                QUEUE  (TS-QUEUE-ID)
000630*                ITEM   (TS-ITEM)
000631*                FROM   (TS-RECORD)
000632*                LENGTH (TS-RECORD-LENGTH)
000633*           END-EXEC.
      *    MOVE '*" IR  L              ''   #00003887' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033383837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000634
000635     IF BATCH-PROCESS-COMPLETE
000636         MOVE 'EDIT TERMINATED'  TO TD-MSG
000637         PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT
000638         MOVE HIGH-VALUES        TO  TS-RESTART-BATCH-NO
000639         
      * EXEC CICS WRITEQ TS REWRITE
000640*            QUEUE  (TS-QUEUE-ID)
000641*            ITEM   (TS-ITEM)
000642*            FROM   (TS-RECORD)
000643*            LENGTH (TS-RECORD-LENGTH)
000644*        END-EXEC
      *    MOVE '*" IR  L              ''   #00003898' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033383938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000645         GO TO 9999-RETURN-CICS.
000646
000647     GO TO 1100-PROCESS-PNDB-LOOP.
000648
000649     EJECT
000650 1200-WRITE-REPORT.
000651     MOVE REC-COUNT              TO TD-REC-CNT.
000652     MOVE EDIT-COMPANY-ID        TO TD-COMPANY-ID.
000653     MOVE 'EDIT TERMINATED'      TO TD-MSG.
000654     MOVE WS-PREV-BATCH          TO TD-LAST-BATCH.
000655
000656     PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT.
000657
000658     MOVE HIGH-VALUES            TO  TS-RESTART-BATCH-NO.
000659
000660     
      * EXEC CICS WRITEQ TS REWRITE
000661*        QUEUE  (TS-QUEUE-ID)
000662*        ITEM   (TS-ITEM)
000663*        FROM   (TS-RECORD)
000664*        LENGTH (TS-RECORD-LENGTH)
000665*    END-EXEC.
      *    MOVE '*" IR  L              ''   #00003919' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033393139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000666
000667     IF NOT REEDIT-OR-RESTART-OR-FULL
000668        GO TO 1260-NO-REPORT.
000669
000670     IF EDIT-PROCESS-SW = 'B' OR 'Y'
000671         GO TO 1260-NO-REPORT.
000672
000673     ADD +1 TO WS-LINE-NUMBER.
000674     MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
000675     MOVE '1'                    TO RF-RECORD-TYPE.
000676     MOVE 'EL051'                TO RF-REPORT-ID.
000677     MOVE TRANS-DATA-MSG         TO RF-DATA-133.
000678     PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
000679
000680     IF DATA-EDITED
000681         MOVE SPACE              TO DATA-EDITED-SW
000682     ELSE
000683         GO TO 1260-NO-REPORT.
000684
000685 1250-CONTINUE.
000686
000687     MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.
000688     MOVE 'RF'                   TO RF-RECORD-ID.
000689     MOVE '2'                    TO RF-RECORD-TYPE.
000690     MOVE 'EL051'                TO RF-REPORT-ID.
000691
000692     
      * EXEC CICS DELETE
000693*        DATASET (ELREPT-FILE-ID)
000694*        RIDFLD  (RF-CONTROL-PRIMARY)
000695*        KEYLENGTH (7)
000696*        GENERIC
000697*    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00003951' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'2020233030303033393531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000698
000699     MOVE SPACES                 TO RF-TRAILER-RECORD.
000700     MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE
000701     MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
000702     MOVE '2'                    TO RF-RECORD-TYPE.
000703     MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
000704     MOVE 'EL051'                TO RF-REPORT-ID.
000705     MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
000706     PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
000707
000708 1260-NO-REPORT.
000709     IF ERPNDB-EOF
000710         MOVE 'Y'                TO FIRST-TIME-SW
000711         MOVE PNDC-MSG           TO PRT-MSG
000712         GO TO 1300-START-CLAIMS-EDIT.
000713
000714     PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.
000715     MOVE CF-CL-MAIL-TO-NAME     TO PRT-CO.
000716     MOVE LOW-VALUES             TO ERPNDB-KEY.
000717     MOVE PB-COMPANY-CD          TO ERPNDB-COMPANY-CD.
000718     MOVE ZEROS                  TO ERPNDB-SEQ-NO.
000719     MOVE SPACE                  TO ERPNDB-FILE-SW
000720                                    WK-CNTL-RECORD-FOUND-SW.
000721     GO TO 1100-PROCESS-PNDB-LOOP.
000722     EJECT
000723 1300-START-CLAIMS-EDIT.
000724
000725     PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.
000726
000727     IF CO-HAS-CLAIMS
000728        GO TO 9999-RETURN-CICS.
000729
000730     PERFORM 7000-WRITE-EL053-MESSAGE THRU 7999-EXIT.
000731
000732 1300-PROCESS-PNDC-LOOP.
000733     PERFORM 3300-READ-NEXT-RECORD THRU 3390-EXIT.
000734
000735     IF ERPNDC-EOF  OR
000736        ERPNDC-EOF-COMP
000737         GO TO 1400-WRITE-REPORT.
000738
000739     PERFORM 2100-CALL-PNDC-EDIT-PROGRAM THRU  2190-EXIT.
000740     PERFORM 5000-SET-PNDC-ERROR-FLAGS THRU 5900-EXIT.
000741
000742     GO TO 1300-PROCESS-PNDC-LOOP.
000743
000744     EJECT
000745
000746 1400-WRITE-REPORT.
000747     IF DATA-EDITED
000748         MOVE SPACE              TO DATA-EDITED-SW
000749     ELSE
000750         GO TO 1460-NO-REPORT.
000751
000752     IF EDIT-PROCESS-SW = 'B' OR 'Y'
000753         GO TO 1460-NO-REPORT.
000754
000755 1450-CONTINUE.
000756     MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.
000757     MOVE 'EL053'                TO RF-REPORT-ID.
000758     MOVE +1                     TO RF-LINE-NUMBER.
000759     MOVE '2'                    TO RF-RECORD-TYPE.
000760     
      * EXEC CICS READ
000761*         DATASET  ('ELREPT')
000762*         INTO     (REPORT-SAVE-FILE)
000763*         RIDFLD   (RF-CONTROL-PRIMARY)
000764*         UPDATE
000765*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00004019' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034303139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000766
000767     MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.
000768
000769     
      * EXEC CICS REWRITE
000770*         DATASET  ('ELREPT')
000771*         FROM     (REPORT-SAVE-FILE)
000772*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&& L                  %   #00004028' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034303238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000773
000774 1460-NO-REPORT.
000775     IF ERPNDC-EOF
000776         GO TO 9999-RETURN-CICS.
000777
000778     PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.
000779     MOVE CF-CL-MAIL-TO-NAME     TO PRT-CO.
000780     MOVE LOW-VALUES             TO ERPNDC-KEY.
000781     MOVE PC-COMPANY-CD          TO ERPNDC-COMPANY-CD.
000782     MOVE ZEROS                  TO ERPNDC-SEQ-NO.
000783     MOVE SPACE                  TO ERPNDC-FILE-SW
000784                                    WK-PC-CNTL-RECORD-FOUND-SW.
000785     GO TO 1300-PROCESS-PNDC-LOOP.
000786     EJECT
000787 1500-PRINT-MESSAGE.
000788     MOVE REC-COUNT              TO  TD-REC-CNT.
000789
000790
000791     
      * EXEC CICS ASKTIME
000792*    END-EXEC.
      *    MOVE '0"                    "   #00004050' TO DFHEIV0
           MOVE X'302220202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303034303530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000793
000794
000795
000796     MOVE EIBTIME                TO  WS-TIME.
000797
000798     COMPUTE WS-STOP-TIME = (WS-HOURS * 3600) +
000799                                (WS-MINUTES * 60) + WS-SECONDS.
000800
000801     MOVE WS-STOP-TIME           TO  WS-LAST-TIME.
000802     SUBTRACT WS-START-TIME FROM WS-STOP-TIME
000803                                 GIVING WS-TIME-WORK.
000804     DIVIDE WS-TIME-WORK BY +3600 GIVING TD-ELAPSED-HOURS
000805                                 REMAINDER WS-TIME-WORK.
000806     DIVIDE WS-TIME-WORK BY +60 GIVING TD-ELAPSED-MINUTES
000807                                 REMAINDER TD-ELAPSED-SECONDS.
000808
000809     SUBTRACT WS-LAST-TIME FROM WS-STOP-TIME
000810                                 GIVING WS-TIME-WORK.
000811     DIVIDE WS-TIME-WORK BY +3600 GIVING TD-ELAPSED2-HOURS
000812                                 REMAINDER WS-TIME-WORK.
000813     DIVIDE WS-TIME-WORK BY +60 GIVING TD-ELAPSED2-MINUTES
000814                                 REMAINDER TD-ELAPSED2-SECONDS.
000815
000816
000817
000818
000819     
      * EXEC CICS ASKTIME
000820*    END-EXEC.
      *    MOVE '0"                    "   #00004078' TO DFHEIV0
           MOVE X'302220202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303034303738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000821
000822     MOVE EIBTIME                TO  WS-TIME.
000823     COMPUTE WS-START-TIME = (WS-HOURS * 3600) +
000824                             (WS-MINUTES * 60) + WS-SECONDS.
000825
000826     MOVE WS-HOURS               TO  TD-START-HOURS.
000827     MOVE WS-MINUTES             TO  TD-START-MINUTES.
000828     MOVE WS-SECONDS             TO  TD-START-SECONDS.
000829
000830     if connected-to-db
000832        exec sql
                 set connection 'WFCERT'
000833        end-exec
000834
000836        EXEC SQL
                  commit work
000837        END-EXEC
000838        if sqlcode not = 0
000839           display "Error: commit release "
000840           display ' sql return code ' sqlcode
000841           display ' sql err mess    ' sqlerrmc
000842        end-if
000843
000845        EXEC SQL
                 DISCONNECT 'WFCERT'
000846        END-EXEC
000847
000848        if sqlcode not = 0
000849           display "Error: cannot disconnect pdbnk "
000850           display ' sql return code ' sqlcode
000851           display ' sql err mess    ' sqlerrmc
000852        end-if
000853     end-if
000854
000855     
      * EXEC CICS WRITEQ TD
000856*         QUEUE     ('CSMT')
000857*         FROM      (TRANS-DATA-MSG)
000858*         LENGTH    (TRANS-DATA-LENGTH)
000859*    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00004114' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303034313134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000860
000861     
      * EXEC CICS WRITEQ TD
000862*         QUEUE     ('CSMT')
000863*         FROM      (TRANS-DATA-MSG2)
000864*         LENGTH    (TRANS-DATA-LENGTH)
000865*    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00004120' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' &
                X'202020202020202020202620' &
                X'2020233030303034313230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG2, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000866
000867 1599-EXIT.
000868     EXIT.
000869
000870 1800-work-flow.
000871
000872     if not connected-to-db
000873        evaluate ws-kix-myenv
000874           when 'cid1p'
000875              move '//sdv-db01.cso.local:1433;'
000876                                 to p-sql-server
000877           when 'mdoff'
000878              move '//hov-tstdb01.cso.local:55330;'
000879                                 to p-sql-server
000880           when other
000881              move '//hov-tstdb01.cso.local:1433;'
000882                                 to p-sql-server
000883        end-evaluate
000884
000885        move spaces              to connect-string
000886        string
000887           'jdbc:sqlserver:' delimited size
000888           p-sql-server delimited space
000889           'databaseName=' delimited size
000890           into connect-string
000891        end-string
000892        display ' connection string = **' connect-string '**'
000893
000894        SET ENVIRONMENT "jdbc.url"
000895                                 to connect-string
000896
000897        move 'appuser'           to usr
000898        move 'appuser@cso'       to pass
000899        move 'CSO_Workflow'      to database
000900
000902        exec sql
                 connect to :database AS 'WFCERT'
000903            user      :usr
000904            using     :pass
000905        end-exec
000906        if sqlcode not = 0
000907           display "Error: cannot connect "
000908           display sqlcode
000909           display sqlerrmc
000910           
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL051' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
000911        end-if
000912        set connected-to-db to true
000913     end-if
000914
000915     move 'CRTS'                 to pa-rec-type
000916     move edit-company-id        to pa-company-id
000917     move pending-business       to pa-rest-of-record
000918
000919     
      * EXEC CICS LINK
000920*        PROGRAM  ('WF001')
000921*        COMMAREA (work-flow-pass-area)
000922*        LENGTH   (604)
000923*    END-EXEC
           MOVE 'WF001' TO DFHEIV1
           MOVE 604
             TO DFHEIV11
      *    MOVE '."C                   (   #00004178' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034313738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 work-flow-pass-area, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000924
000925     .
000926 1800-exit.
000927     exit.
000928
000929 2000-CALL-PNDB-EDIT-PROGRAM.
000930
000931     
      * EXEC CICS  HANDLE CONDITION
000932*           NOTFND   (2080-WRITE-REPORT)
000933*    END-EXEC.
      *    MOVE '"$I                   ! # #00004190' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303034313930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000934
000935     MOVE PENDING-BUSINESS TO WK-PENDING-BUSINESS.
000936     
      * EXEC CICS LINK
000937*        PROGRAM    (PNDB-EDIT-PGM)
000938*        COMMAREA   (PNDB-EDIT-PASS-AREA)
000939*        LENGTH     (1036)
000940*    END-EXEC.
           MOVE 1036
             TO DFHEIV11
      *    MOVE '."C                   (   #00004195' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034313935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PNDB-EDIT-PGM, 
                 PNDB-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000941
000942***********   IF VALID RECORD HAS NOT BEEN PASSED BACK TO THIS
000943***********   PROGRAM, THEN AN ABEND HAS OCCURRED.
000944***********   IF ERROR 2617 WAS SENT BACK THEN A FILE IS NOT OPEN
000945***********   AND THE EDIT SHOULD NOT CONTINUE.
000946
000947     MOVE WK-PENDING-BUSINESS TO PENDING-BUSINESS.
000948
000949        IF ER-2617               = PB-COMMON-ERROR (1)
000950                                OR PB-COMMON-ERROR (2)
000951                                OR PB-COMMON-ERROR (3)
000952                                OR PB-COMMON-ERROR (4)
000953                                OR PB-COMMON-ERROR (5)
000954                                OR PB-COMMON-ERROR (6)
000955                                OR PB-COMMON-ERROR (7)
000956                                OR PB-COMMON-ERROR (8)
000957                                OR PB-COMMON-ERROR (9)
000958                                OR PB-COMMON-ERROR (10)
000959
000960        GO TO 9999-RETURN-CICS.
000961
000962     IF NOT VALID-PB-ID
000963        MOVE PNDB-EDIT-PASS-AREA TO RF-DATA-133
000964        MOVE '1'                 TO RF-CTL-CHAR-133
000965        MOVE EDIT-COMPANY-CD     TO RF-COMPANY-CD
000966        MOVE '1'                 TO RF-RECORD-TYPE
000967        MOVE 'EL051'             TO RF-REPORT-ID
000968        ADD +1                   TO WS-LINE-NUMBER
000969        PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT
000970        MOVE '2'                 TO RF-RECORD-TYPE
000971        MOVE +1                  TO RF-LINE-NUMBER
000972        MOVE 'ABEND'             TO RF-CURRENT-DATE
000973        
      * EXEC CICS READ
000974*            DATASET  ('ELREPT')
000975*            INTO     (REPORT-SAVE-FILE)
000976*            RIDFLD   (RF-CONTROL-PRIMARY)
000977*            UPDATE
000978*       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00004232' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034323332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000979        MOVE 'ABEND'             TO RF-CURRENT-DATE
000980        
      * EXEC CICS REWRITE
000981*            DATASET  ('ELREPT')
000982*            FROM     (REPORT-SAVE-FILE)
000983*       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&& L                  %   #00004239' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034323339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000984        GO TO 9999-RETURN-CICS.
000985
000986     GO TO 2090-EXIT.
000987
000988 2080-WRITE-REPORT.
000989
000990     
      * EXEC CICS WRITE
000991*         DATASET  ('ELREPT')
000992*         RIDFLD   (RF-CONTROL-PRIMARY)
000993*         FROM     (REPORT-SAVE-FILE)
000994*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00004249' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034323439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000995
000996     GO TO 9999-RETURN-CICS.
000997
000998 2090-EXIT.
000999     EXIT.
001000
001001     EJECT
001002
001003 2100-CALL-PNDC-EDIT-PROGRAM.
001004     
      * EXEC CICS LINK
001005*        PROGRAM    (PNDC-EDIT-PGM)
001006*        COMMAREA   (PNDC-EDIT-PASS-AREA)
001007*        LENGTH     (553)
001008*    END-EXEC.
           MOVE 553
             TO DFHEIV11
      *    MOVE '."C                   (   #00004263' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034323633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PNDC-EDIT-PGM, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001009
001010     IF NOT VALID-PC-ID
001011        MOVE PNDC-EDIT-PASS-AREA TO RF-DATA-133
001012        MOVE '1'                 TO RF-CTL-CHAR-133
001013        MOVE EDIT-COMPANY-CD     TO RF-COMPANY-CD
001014        MOVE '1'                 TO RF-RECORD-TYPE
001015        MOVE 'EL053'             TO RF-REPORT-ID
001016        ADD +1 TO RF-LINE-NUMBER
001017        PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT
001018        MOVE '2'                 TO RF-RECORD-TYPE
001019        MOVE +1                  TO RF-LINE-NUMBER
001020        
      * EXEC CICS READ
001021*            DATASET  ('ELREPT')
001022*            INTO     (REPORT-SAVE-FILE)
001023*            RIDFLD   (RF-CONTROL-PRIMARY)
001024*            UPDATE
001025*       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00004279' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034323739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001026        MOVE 'ABEND'   TO RF-CURRENT-DATE
001027        
      * EXEC CICS REWRITE
001028*            DATASET  ('ELREPT')
001029*            FROM     (REPORT-SAVE-FILE)
001030*       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&& L                  %   #00004286' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034323836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001031        GO TO 9999-RETURN-CICS.
001032
001033 2190-EXIT.
001034     EXIT.
001035     EJECT
001036 3000-READ-CONTROL-FILE.
001037     
      * EXEC CICS HANDLE CONDITION
001038*        NOTFND (9999-RETURN-CICS)
001039*    END-EXEC.
      *    MOVE '"$I                   ! $ #00004296' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303034323936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001040
001041     MOVE SPACES                 TO ELCNTL-KEY.
001042     MOVE '1'                    TO ELCNTL-REC-TYPE.
001043     MOVE EDIT-COMPANY-ID        TO ELCNTL-COMPANY-ID.
001044
001045     MOVE +0                     TO ELCNTL-SEQ-NO.
001046
001047     
      * EXEC CICS READ
001048*        DATASET (ELCNTL-FILE-ID)
001049*        into (control-file)
001050*        SET (ADDRESS OF CONTROL-FILE)
001051*        RIDFLD (ELCNTL-KEY)
001052*    END-EXEC.
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00004306' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303034333036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 control-file, 
                 DFHEIV11, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001053
001054     MOVE CF-LGX-CLAIM-USER      TO WS-LGX-CLAIM-USER.
001055 3090-EXIT.
001056     EXIT.
001057     EJECT
001058 3200-READ-NEXT-RECORD.
001059     IF ERPNDB-SEQ-NO = 9999
001060        MOVE HIGH-VALUES         TO ERPNDB-SEQ-XX
001061       ELSE
001062        ADD +1                   TO ERPNDB-SEQ-NO.
001063
001064     
      * EXEC CICS HANDLE CONDITION
001065*        NOTFND (3210-END-OF-FILE)
001066*    END-EXEC.
      *    MOVE '"$I                   ! % #00004323' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303034333233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001067
001068     
      * EXEC CICS READ
001069*        INTO    (PENDING-BUSINESS)
001070*        INTO    (PNDB-EDIT-PASS-AREA)
001071*        DATASET (ERPNDB-FILE-ID)
001072*        RIDFLD  (ERPNDB-KEY)
001073*        GTEQ
001074*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&"IL       G          (   #00004327' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303034333237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001075
001076     IF PB-COMPANY-CD NOT = ERPNDB-COMPANY-CD
001077         IF PROCESS-COMPANY
001078             MOVE 'Y'            TO ERPNDB-FILE-SW
001079             GO TO 3290-EXIT
001080         ELSE
001081             MOVE 'X'            TO ERPNDB-FILE-SW
001082             GO TO 3290-EXIT.
001083
001084     ADD +1                      TO PNDB-REC-COUNT.
001085     IF PNDB-REC-COUNT IS GREATER THAN +30
001086         MOVE +0                 TO PNDB-REC-COUNT
001087*        EXEC CICS DELAY
001088*            INTERVAL  (WS-DELAY-INTERVAL)
001089*        END-EXEC
001090     end-if
001091
001092     IF FIRST-TIME
001093         MOVE PB-CONTROL-PRIMARY TO ERPNDB-KEY
001094         MOVE SPACE              TO FIRST-TIME-SW.
001095
001096     MOVE PB-CONTROL-PRIMARY     TO ERPNDB-KEY.
001097
001098     MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.
001099
001100     IF  WS-BATCH-PREFIX = '#CL'
001101         MOVE 9999               TO ERPNDB-SEQ-NO
001102         GO TO 3200-READ-NEXT-RECORD.
001103
001104     IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES
001105        IF EDIT-RESTART-BATCH NOT = 'REEDIT'
001106           GO TO 3200-READ-NEXT-RECORD.
001107
001108     IF PB-BILLED-DT NOT EQUAL LOW-VALUES
001109        GO TO 3200-READ-NEXT-RECORD.
001110
001111     MOVE 'Y'                    TO DATA-EDITED-SW.
001112     
      * EXEC CICS READ
001113*        DATASET (ERPNDB-FILE-ID)
001114*        INTO    (PENDING-BUSINESS)
001115*        INTO    (PNDB-EDIT-PASS-AREA)
001116*        RIDFLD  (ERPNDB-KEY)
001117*        UPDATE
001118*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00004371' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034333731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001119
001120     GO TO 3290-EXIT.
001121
001122 3210-END-OF-FILE.
001123     MOVE 'Y'                    TO ERPNDB-FILE-SW.
001124
001125 3290-EXIT.
001126     EXIT.
001127     EJECT
001128 3300-READ-NEXT-RECORD.
001129     ADD +1                      TO ERPNDC-SEQ-NO.
001130     
      * EXEC CICS HANDLE CONDITION
001131*        NOTFND (3310-END-OF-FILE)
001132*    END-EXEC.
      *    MOVE '"$I                   ! & #00004389' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303034333839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001133
001134     
      * EXEC CICS READ
001135*        INTO    (PNDC-EDIT-PASS-AREA)
001136*        DATASET (ERPNDC-FILE-ID)
001137*        RIDFLD  (ERPNDC-KEY)
001138*        GTEQ
001139*    END-EXEC.
           MOVE LENGTH OF
            PNDC-EDIT-PASS-AREA
             TO DFHEIV11
      *    MOVE '&"IL       G          (   #00004393' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303034333933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001140
001141     IF FIRST-TIME
001142         MOVE PC-CONTROL-PRIMARY TO ERPNDC-KEY
001143         MOVE SPACE              TO FIRST-TIME-SW.
001144
001145     IF PC-COMPANY-CD NOT = ERPNDC-COMPANY-CD
001146         IF PROCESS-COMPANY
001147             MOVE 'Y'            TO ERPNDC-FILE-SW
001148             GO TO 3390-EXIT
001149         ELSE
001150             MOVE 'X'            TO ERPNDC-FILE-SW
001151             GO TO 3390-EXIT.
001152
001153     ADD +1                      TO PNDB-REC-COUNT.
001154     IF PNDB-REC-COUNT IS GREATER THAN +30
001155         MOVE +0                 TO PNDB-REC-COUNT
001156         
      * EXEC CICS DELAY
001157*            INTERVAL  (WS-DELAY-INTERVAL)
001158*        END-EXEC.
      *    MOVE '0$I                   &   #00004415' TO DFHEIV0
           MOVE X'302449202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034343135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001159
001160     MOVE PC-CONTROL-PRIMARY     TO ERPNDC-KEY.
001161
001162     IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES
001163         GO TO 3300-READ-NEXT-RECORD.
001164
001165     MOVE 'Y'                    TO DATA-EDITED-SW.
001166     
      * EXEC CICS READ
001167*        DATASET (ERPNDC-FILE-ID)
001168*        INTO    (PNDC-EDIT-PASS-AREA)
001169*        RIDFLD  (ERPNDC-KEY)
001170*        UPDATE
001171*    END-EXEC.
           MOVE LENGTH OF
            PNDC-EDIT-PASS-AREA
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00004425' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034343235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001172
001173     GO TO 3390-EXIT.
001174
001175 3310-END-OF-FILE.
001176     MOVE 'Y'                    TO ERPNDC-FILE-SW.
001177
001178 3390-EXIT.
001179     EXIT.
001180     EJECT
001181 3400-WRITE-REPORT-RECORD.
001182
001183     IF FILES-NOT-OPEN
001184        NEXT SENTENCE
001185     ELSE
001186       IF NOT REEDIT-OR-RESTART-OR-FULL
001187          GO TO 3490-EXIT.
001188
001189     MOVE 'RF'                   TO RF-RECORD-ID.
001190     MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
001191
001192     
      * EXEC CICS WRITE
001193*        DATASET (ELREPT-FILE-ID)
001194*        FROM    (REPORT-SAVE-FILE)
001195*        RIDFLD  (RF-CONTROL-PRIMARY)
001196*    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004451' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034343531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001197
001198 3490-EXIT.
001199      EXIT.
001200     EJECT
001201 3500-INCREMENT-ERROR-COUNTERS.
001202     IF EMI-SEVERITY-SAVE = 'W'
001203         ADD 1                   TO EMI-WARNING-CTR
001204     ELSE
001205         IF EMI-SEVERITY-SAVE = 'F'
001206             ADD 1               TO EMI-FORCABLE-CTR
001207         ELSE
001208             IF EMI-SEVERITY-SAVE = 'X'
001209                 ADD 1           TO EMI-FATAL-CTR.
001210
001211 3500-EXIT.
001212     EXIT.
001213     EJECT
001214 4000-SET-PNDB-ERROR-FLAGS.
001215
001216     IF PB-COMMON-ERRORS = LOW-VALUES OR
001217        PB-BATCH-TRAILER
001218        GO TO 4500-REWRITE-RECORD.
001219
001220 4010-FORMAT-ERRORS.
001221
001222     MOVE +0                     TO SUB1.
001223
001224 4100-ERROR-LOOP.
001225
001226     ADD +1                      TO SUB1.
001227
001228     IF SUB1 GREATER THAN PB-NO-OF-ERRORS
001229        GO TO 4200-SET-ERROR-FLAGS.
001230
001231     MOVE PB-COMMON-ERROR (SUB1) TO EMI-ERROR.
001232     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001233
001234     GO TO 4100-ERROR-LOOP.
001235
001236
001237 4200-SET-ERROR-FLAGS.
001238
001239     IF EMI-FATAL-CTR NOT = ZEROS
001240        MOVE 'X'                 TO PB-FATAL-FLAG
001241        GO TO 4300-SET-ERROR-FLAGS.
001242
001243     IF EMI-FORCABLE-CTR NOT = ZEROS
001244        IF PB-ISSUE
001245           IF PB-ISSUE-FORCE
001246              MOVE 'F'           TO PB-FORCE-ER-CD
001247             ELSE
001248              MOVE 'X'           TO PB-FORCE-ER-CD
001249          ELSE
001250           IF PB-CANCEL-FORCE
001251              MOVE 'F'           TO PB-FORCE-ER-CD
001252             ELSE
001253              MOVE 'X'           TO PB-FORCE-ER-CD.
001254
001255 4300-SET-ERROR-FLAGS.
001256
001257     IF EMI-WARNING-CTR NOT = ZEROS
001258        MOVE 'W'                 TO PB-WARN-ER-CD.
001259
001260     IF PB-UNFORCED-ERRORS OR
001261        PB-FATAL-ERRORS    OR
001262        PB-RECORD-ON-HOLD  OR
001263        PB-RECORD-RETURNED OR
001264        PB-CANCELLATION
001265          NEXT SENTENCE
001266        ELSE
001267          GO TO 4500-REWRITE-RECORD.
001268
001269
001270     
      * EXEC CICS  HANDLE CONDITION
001271*          NOTFND    (4500-REWRITE-RECORD)
001272*    END-EXEC.
      *    MOVE '"$I                   ! '' #00004529' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303034353239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001273
001274     MOVE PB-CONTROL-BY-ACCOUNT  TO CERT-KEY.
001275     MOVE PB-SV-CARRIER          TO CERT-CARRIER.
001276     MOVE PB-SV-GROUPING         TO CERT-GROUPING.
001277     MOVE PB-SV-STATE            TO CERT-STATE.
001278     
      * EXEC CICS READ
001279*        into    (certificate-master)
001280*        SET     (ADDRESS OF CERTIFICATE-MASTER)
001281*        DATASET (CERT-ID)
001282*        RIDFLD  (CERT-KEY)
001283*        UPDATE
001284*    END-EXEC.
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00004537' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034353337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 certificate-master, 
                 DFHEIV11, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001285
001286     IF CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE
001287        GO TO 4490-REWRITE-CERT-MASTER.
001288
001289     IF PB-ISSUE
001290        IF  PB-RECORD-RETURNED
001291            MOVE '4'             TO CM-CREDIT-INTERFACE-SW-1
001292        ELSE
001293             MOVE '2'            TO CM-CREDIT-INTERFACE-SW-1.
001294
001295     IF PB-CANCELLATION
001296        IF  PB-RECORD-RETURNED
001297            MOVE '7'             TO CM-CREDIT-INTERFACE-SW-2
001298        ELSE
001299            IF PB-C-LF-CANCEL-VOIDED OR PB-C-AH-CANCEL-VOIDED
001300               MOVE '6'          TO CM-CREDIT-INTERFACE-SW-2
001301            ELSE
001302               MOVE '4'          TO CM-CREDIT-INTERFACE-SW-2.
001303
001304 4490-REWRITE-CERT-MASTER.
001305     
      * EXEC CICS REWRITE
001306*         DATASET    (CERT-ID)
001307*         FROM       (CERTIFICATE-MASTER)
001308*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004564' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034353634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001309
001310 4500-REWRITE-RECORD.
001311     IF NOT PB-BATCH-TRAILER
001312        MOVE PB-CSR-ID               TO WS-CSR-ID
001313         IF PB-ISSUE
001314             ADD PB-I-LF-PREM-CALC   TO WS-LF-ISS-COMPUTED
001315             ADD PB-I-LF-ALT-PREM-CALC   TO WS-LF-ISS-COMPUTED
001316             ADD PB-I-LF-PREMIUM-AMT TO WS-LF-ISS-ENTERED
001317             ADD PB-I-LF-ALT-PREMIUM-AMT TO WS-LF-ISS-ENTERED
001318             ADD PB-I-AH-PREM-CALC   TO WS-AH-ISS-COMPUTED
001319             ADD PB-I-AH-PREMIUM-AMT TO WS-AH-ISS-ENTERED
001320             ADD 1                   TO WS-ISSUE-CNT
001321         ELSE
001322             ADD PB-C-LF-REF-CALC    TO WS-LF-CAN-COMPUTED
001323             ADD PB-C-LF-CANCEL-AMT  TO WS-LF-CAN-ENTERED
001324             ADD PB-C-AH-REF-CALC    TO WS-AH-CAN-COMPUTED
001325             ADD PB-C-AH-CANCEL-AMT  TO WS-AH-CAN-ENTERED
001326             ADD 1                   TO WS-CANCEL-CNT
001327     ELSE
001328         MOVE WS-CSR-ID              TO PB-CSR-ID
001329         MOVE WS-LF-ISS-COMPUTED     TO PB-B-LF-ISS-PRM-COMPUTED
001330         MOVE WS-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED
001331         MOVE WS-AH-ISS-COMPUTED     TO PB-B-AH-ISS-PRM-COMPUTED
001332         MOVE WS-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED
001333         MOVE WS-LF-CAN-COMPUTED     TO PB-B-LF-CAN-PRM-COMPUTED
001334         MOVE WS-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED
001335         MOVE WS-AH-CAN-COMPUTED     TO PB-B-AH-CAN-PRM-COMPUTED
001336         MOVE WS-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED
001337         MOVE WS-ISSUE-CNT           TO PB-B-ISSUE-CNT-ENTERED
001338         MOVE WS-CANCEL-CNT          TO PB-B-CANCEL-CNT-ENTERED
001339         MOVE ZEROS                  TO WS-LF-ISS-COMPUTED
001340                                        WS-LF-ISS-ENTERED
001341                                        WS-AH-ISS-COMPUTED
001342                                        WS-AH-ISS-ENTERED
001343                                        WS-LF-CAN-COMPUTED
001344                                        WS-LF-CAN-ENTERED
001345                                        WS-AH-CAN-COMPUTED
001346                                        WS-AH-CAN-ENTERED
001347                                        WS-ISSUE-CNT
001348                                        WS-CANCEL-CNT
001349         MOVE SPACE                  TO PB-FATAL-FLAG
001350         IF PROCESS-BATCH
001351             MOVE 'Y'            TO EDIT-PROCESS-SW.
001352
001353     IF PB-BATCH-TRAILER
001354         IF PB-B-LF-ISS-PRM-REMITTED =
001355            PB-B-LF-ISS-PRM-ENTERED   AND
001356            PB-B-LF-CAN-PRM-REMITTED =
001357            PB-B-LF-CAN-PRM-ENTERED   AND
001358            PB-B-AH-ISS-PRM-REMITTED =
001359            PB-B-AH-ISS-PRM-ENTERED   AND
001360            PB-B-AH-CAN-PRM-REMITTED =
001361            PB-B-AH-CAN-PRM-ENTERED   AND
001362            PB-B-ISSUE-CNT-REMITTED  =
001363            PB-B-ISSUE-CNT-ENTERED    AND
001364            PB-B-CANCEL-CNT-REMITTED =
001365            PB-B-CANCEL-CNT-ENTERED
001366             MOVE SPACE          TO PB-OUT-BAL-CD
001367     ELSE
001368         MOVE 'O'                TO PB-OUT-BAL-CD.
001369
001370     MOVE 'C'                    TO JP-RECORD-TYPE.
001371     MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.
001372*    MOVE PNDB-EDIT-PASS-AREA    TO JP-RECORD-AREA.
001373     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
001374     COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.
001375
001376     
      * EXEC CICS REWRITE
001377*        DATASET (ERPNDB-FILE-ID)
001378*        FROM    (PNDB-EDIT-PASS-AREA)
001379*        FROM    (PENDING-BUSINESS)
001380*    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004635' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034363335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001381
001382     PERFORM 8400-LOG-JOURNAL-RECORD.
001383     MOVE ZEROS                  TO EMI-WARNING-CTR
001384                                    EMI-FORCABLE-CTR
001385                                    EMI-FATAL-CTR.
001386
001387 4900-EXIT.
001388     EXIT.
001389     EJECT
001390 5000-SET-PNDC-ERROR-FLAGS.
001391     IF PC-ERROR-FLAGS = SPACES
001392         GO TO 5500-REWRITE-RECORD.
001393
001394     MOVE LIT-2800               TO WS-ERR-CODE.
001395     MOVE 1  TO SUB.
001396
001397 5100-ERR-LOOP.
001398     IF PC-ERR-FLAG (SUB) NOT = SPACES
001399         MOVE SUB                TO WS-ERROR-SUB
001400         MOVE WS-ERR-CODE        TO EMI-ERROR
001401         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001402         PERFORM 3500-INCREMENT-ERROR-COUNTERS THRU 3500-EXIT.
001403
001404     IF EMI-FATAL-CTR NOT = ZEROS
001405         GO TO 5200-SET-ERROR-FLAGS.
001406
001407     ADD 1   TO SUB.
001408     IF SUB LESS THAN 101
001409         GO TO 5100-ERR-LOOP.
001410
001411 5200-SET-ERROR-FLAGS.
001412     IF EMI-FATAL-CTR NOT = ZEROS
001413         MOVE 'X'                TO PC-FATAL-FLAG.
001414
001415     IF EMI-FORCABLE-CTR NOT = ZEROS
001416         IF PC-CLAIM-FORCE
001417             MOVE 'F'            TO PC-FORCE-ER-CD
001418         ELSE
001419             MOVE 'X'            TO PC-FORCE-ER-CD.
001420
001421     IF EMI-WARNING-CTR NOT = ZEROS
001422         MOVE 'W'                TO PC-WARN-ER-CD.
001423
001424 5500-REWRITE-RECORD.
001425     MOVE 'C'                    TO JP-RECORD-TYPE.
001426     MOVE ERPNDC-FILE-ID         TO JP-FILE-ID.
001427     MOVE PNDC-EDIT-PASS-AREA    TO JP-RECORD-AREA.
001428     COMPUTE JOURNAL-LENGTH = ERPNDC-LENGTH + 23.
001429     
      * EXEC CICS REWRITE
001430*        DATASET (ERPNDC-FILE-ID)
001431*        FROM    (PNDC-EDIT-PASS-AREA)
001432*    END-EXEC.
           MOVE LENGTH OF
            PNDC-EDIT-PASS-AREA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004688' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034363838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001433
001434     PERFORM 8400-LOG-JOURNAL-RECORD.
001435
001436     MOVE ZEROS                  TO EMI-WARNING-CTR
001437                                    EMI-FORCABLE-CTR
001438                                    EMI-FATAL-CTR.
001439
001440 5900-EXIT.
001441     EXIT.
001442     EJECT
001443 5950-UPDATE-TYPE-2.
001444     IF NOT REEDIT-OR-RESTART-OR-FULL
001445        GO TO 5959-EXIT.
001446
001447     
      * EXEC CICS  HANDLE CONDITION
001448*           NOTFND   (5955-CONTINUE)
001449*    END-EXEC.
      *    MOVE '"$I                   ! ( #00004706' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303034373036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001450
001451     MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.
001452     MOVE 'RF'                   TO RF-RECORD-ID.
001453     MOVE '2'                    TO RF-RECORD-TYPE.
001454     MOVE 'EL051'                TO RF-REPORT-ID.
001455     
      * EXEC CICS DELETE
001456*        DATASET (ELREPT-FILE-ID)
001457*        RIDFLD  (RF-CONTROL-PRIMARY)
001458*        KEYLENGTH (7)
001459*        GENERIC
001460*    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004714' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'2020233030303034373134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001461
001462 5955-CONTINUE.
001463     MOVE EIBTIME                TO WS-TIME.
001464     MOVE SPACES                 TO RF-TRAILER-RECORD.
001465     MOVE 'STARTED'              TO RF-CURRENT-DATE.
001466     MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
001467     MOVE '2'                    TO RF-RECORD-TYPE.
001468     MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
001469     MOVE 'EL051'                TO RF-REPORT-ID.
001470     PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
001471
001472 5959-EXIT.
001473     EXIT.
001474     EJECT
001475 6000-WRITE-EL051-MESSAGE.
001476     
      * EXEC CICS  HANDLE CONDITION
001477*           NOTFND   (6010-DELETE-TYPE-2)
001478*    END-EXEC.
      *    MOVE '"$I                   ! ) #00004735' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303034373335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001479
001480     MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.
001481     MOVE 'RF'                   TO RF-RECORD-ID.
001482     MOVE '1'                    TO RF-RECORD-TYPE.
001483     MOVE 'EL051'                TO RF-REPORT-ID.
001484     
      * EXEC CICS DELETE
001485*        DATASET (ELREPT-FILE-ID)
001486*        RIDFLD  (RF-CONTROL-PRIMARY)
001487*        KEYLENGTH (7)
001488*        GENERIC
001489*    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004743' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'2020233030303034373433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001490
001491 6010-DELETE-TYPE-2.
001492     
      * EXEC CICS  HANDLE CONDITION
001493*           NOTFND   (6020-CONTINUE)
001494*    END-EXEC.
      *    MOVE '"$I                   ! * #00004751' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303034373531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001495
001496     MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.
001497     MOVE 'RF'                   TO RF-RECORD-ID.
001498     MOVE '2'                    TO RF-RECORD-TYPE.
001499     MOVE 'EL051'                TO RF-REPORT-ID.
001500     
      * EXEC CICS DELETE
001501*        DATASET (ELREPT-FILE-ID)
001502*        RIDFLD  (RF-CONTROL-PRIMARY)
001503*        KEYLENGTH (7)
001504*        GENERIC
001505*    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004759' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'2020233030303034373539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001506
001507 6020-CONTINUE.
001508     MOVE EIBTIME                TO WS-TIME.
001509     MOVE SPACES                 TO RF-TRAILER-RECORD.
001510     MOVE 'STARTED'              TO RF-CURRENT-DATE.
001511     MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
001512     MOVE '2'                    TO RF-RECORD-TYPE.
001513     MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
001514     MOVE 'EL051'                TO RF-REPORT-ID.
001515     MOVE +1                     TO WS-LINE-NUMBER.
001516     PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
001517
001518 6999-EXIT.
001519     EXIT.
001520     EJECT
001521 7000-WRITE-EL053-MESSAGE.
001522     
      * EXEC CICS  HANDLE CONDITION
001523*           NOTFND   (7010-DELETE-TYPE-2)
001524*    END-EXEC.
      *    MOVE '"$I                   ! + #00004781' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303034373831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001525
001526     MOVE ERPNDC-COMPANY-CD TO RF-COMPANY-CD.
001527     MOVE 'RF'                   TO RF-RECORD-ID.
001528     MOVE '1'                    TO RF-RECORD-TYPE.
001529     MOVE 'EL053'                TO RF-REPORT-ID.
001530     
      * EXEC CICS DELETE
001531*        DATASET (ELREPT-FILE-ID)
001532*        RIDFLD  (RF-CONTROL-PRIMARY)
001533*        KEYLENGTH (7)
001534*        GENERIC
001535*    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004789' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'2020233030303034373839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001536
001537 7010-DELETE-TYPE-2.
001538     
      * EXEC CICS  HANDLE CONDITION
001539*           NOTFND   (7020-CONTINUE)
001540*    END-EXEC.
      *    MOVE '"$I                   ! , #00004797' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303034373937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001541
001542     MOVE ERPNDC-COMPANY-CD TO RF-COMPANY-CD.
001543     MOVE 'RF'                   TO RF-RECORD-ID.
001544     MOVE '2'                    TO RF-RECORD-TYPE.
001545     MOVE 'EL053'                TO RF-REPORT-ID.
001546     
      * EXEC CICS DELETE
001547*        DATASET (ELREPT-FILE-ID)
001548*        RIDFLD  (RF-CONTROL-PRIMARY)
001549*        KEYLENGTH (7)
001550*        GENERIC
001551*    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004805' TO DFHEIV0
           MOVE X'26282020524B472020202020' &
                X'202020202020202020202620' &
                X'2020233030303034383035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001552
001553 7020-CONTINUE.
001554     MOVE EIBTIME                TO WS-TIME.
001555     MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.
001556     MOVE SPACES                 TO RF-TRAILER-RECORD.
001557     MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.
001558     MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
001559     MOVE '2'                    TO RF-RECORD-TYPE.
001560     MOVE 'EL053'                TO RF-REPORT-ID.
001561     MOVE +1                     TO WS-LINE-NUMBER.
001562     PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
001563
001564 7999-EXIT.
001565     EXIT.
001566     EJECT
001567 8300-ABEND.
001568     MOVE DFHEIBLK TO EMI-LINE1.
001569     
      * EXEC CICS LINK
001570*        PROGRAM   ('EL004')
001571*        COMMAREA  (EMI-LINE1)
001572*        LENGTH    (72)
001573*    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004828' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001574
001575     GO TO 9999-RETURN-CICS.
001576
001577 8400-LOG-JOURNAL-RECORD.
001578     MOVE 'EDIT'                 TO JP-USER-ID.
001579     MOVE THIS-PGM               TO JP-PROGRAM-ID.
001580*    EXEC CICS JOURNAL
001581*        JFILEID     (1)
001582*        JTYPEID     ('EL')
001583*        FROM        (JOURNAL-RECORD)
001584*        LENGTH      (JOURNAL-LENGTH)
001585*        END-EXEC.
001586
001587 8500-DATE-CONVERT.
001588     MOVE LINK-ELDATCV           TO PGM-NAME.
001589     
      * EXEC CICS LINK
001590*        PROGRAM    (PGM-NAME)
001591*        COMMAREA   (DATE-CONVERSION-DATA)
001592*        LENGTH     (DC-COMM-LENGTH)
001593*    END-EXEC.
      *    MOVE '."C                   (   #00004848' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001594 8500-EXIT.
001595     EXIT.
001596
001597     EJECT
001598
001599 9900-ERROR-FORMAT.
001600     MOVE EDIT-COMPANY-ID        TO EMI-CLIENT-ID.
001601     MOVE LINK-001               TO PGM-NAME
001602     
      * EXEC CICS LINK
001603*        PROGRAM    (PGM-NAME)
001604*        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
001605*        LENGTH     (EMI-COMM-LENGTH)
001606*    END-EXEC.
      *    MOVE '."C                   (   #00004861' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001607 9900-EXIT.
001608     EXIT.
001609
001610 9999-RETURN-CICS.
001611     
      * EXEC CICS DEQ
001612*         RESOURCE   (EDIT-COMPANY-CD)
001613*         LENGTH     (1)
001614*    END-EXEC.
           MOVE 1
             TO DFHEIV11
      *    MOVE '2&L                   $   #00004870' TO DFHEIV0
           MOVE X'32264C202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034383730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EDIT-COMPANY-CD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001615
001616     
      * EXEC CICS  RETURN
001617*    END-EXEC.
      *    MOVE '.(                    ''   #00004875' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034383735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001618
001619 9999-EXIT.
001620      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL051' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8300-ABEND,
                     9900-ERROR-FORMAT,
                     0210-PROCESS-FILE,
                     0210-PROCESS-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2080-WRITE-REPORT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9999-RETURN-CICS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3210-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3310-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4500-REWRITE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5955-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 6010-DELETE-TYPE-2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6020-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7010-DELETE-TYPE-2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7020-CONTINUE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL051' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
