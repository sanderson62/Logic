      *****************************************************************
      *                                                               *
      * Copyright (c) 2013 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. BSSRCH.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.
      ********************************************
      *   Balance sheet for month end balancing
      ********************************************
082013******************************************************************
082013*                   C H A N G E   L O G
082013*
082013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082013*-----------------------------------------------------------------
082013*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082013* EFFECTIVE    NUMBER
082013*-----------------------------------------------------------------
082013* 082013  CR2013060600001  PEMA  ADJ TOL TO 10% FOR NON-PROC
100813* 100813  IR2013100100001  PEMA INCREASE NON-PROC REF TOL
101713* 101713  CR2013060600001  PEMA AUTOMATE AHL MORT RESERVE CALC
031714* 031714  IR2014030200002  PEMA INCREASE NON-PROC REF TOL
040214* 040214  IR2014040200002  PEMA FIX NON-PROC REF TOL
123014* 123014  CR2014123000001  PEMA CHG AHL MORT RESV TOL
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
071816* 071816  CR2016062900001  PEMA CHG AHL MORT RESV TOL
041417* 041417  CR2016022400002  PEMA  TPE/BPE Upgrade
100220* 100220  CR2020092400001  PEMA  Add calc of est mort resv for CID
110220* 110220  IR2020110200001  PEMA  Correct mort estimate
082013******************************************************************
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
100220 SELECT ECS010-IN ASSIGN TO dynamic ws-ecs010-in
          FILE STATUS IS ECS010-STATUS.
100220 SELECT ECS080-IN ASSIGN TO dynamic ws-ecs080-in
          FILE STATUS IS ECS080-STATUS
                                  ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CHKPOINT-OUT ASSIGN TO dynamic ws-checkpoint-out
          FILE STATUS IS CHKPNT-STATUS
                                  ORGANIZATION IS LINE SEQUENTIAL.
       data division.
       FILE SECTION.
100220 FD  ECS010-IN
100220     BLOCK CONTAINS 0
100220     RECORDING MODE F.
100220 01  ECS010-IN-REC               pic x(94).
100220 FD  ECS080-IN
100220     BLOCK CONTAINS 0
100220     RECORDING MODE F.
100220 01  ECS080-IN-REC               pic x(83).
100220 FD  CHKPOINT-OUT
100220     BLOCK CONTAINS 0
100220     RECORDING MODE F.
100220 01  CHKPOINT-OUT-REC            pic x(94).
       working-storage section.
       01  DFH-START PIC X(04).
       77  s1 pic s999 comp-3 value +0.
       77  s2 pic s999 comp-3 value +0.
       77  ws-pass-amount              pic s9(11) comp-3 value +0.
       77  ws-low-amount               pic s9(11) value zeros.
       77  ws-hi-amount                pic s9(11) value zeros.
082013 77  ws-low-pct                  pic s9v9(4) value zeros comp-3.
082013 77  ws-high-pct                 pic s9v9(4) value zeros comp-3.
      ************************************************
      * commarea passed to the business logic
      ************************************************
       01  srch-commarea.
      *                                copy BSSRCH-COMMAREA.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2013 by CSO.                                    *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
      ****************************************
      *  commarea for QUERY by eom date and company id
      *  (business logic input & output)
      ****************************************
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
100220* 100220  CR2020092400001  PEMA  Add calc of est mort resv for CID
      ******************************************************************
           03  bl-input.
               05  bl-input-eom-date   pic x(10).
               05  bl-input-comp-id    pic xxx.
               05  bl-input-lo-resv    pic 9(9).
               05  bl-input-hi-resv    pic 9(9).
               05  bl-input-bld-file   pic xxx.
           03  bl-output.
               05  bl-status           pic x.
                   88  bl-ok                 value "p".
                   88  bl-fail               value "f".
               05  bl-output-message   pic x(50).
               05  bl-output-record.
                  07  bl-out-company-name
                                       pic x(30).
                  07  bl-out-formated-date
                                       pic x(20).
                  07  filler occurs 3.
                      09  bl-out-lf-prem  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ah-prem  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-lf-ref   pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ah-ref   pic s9(9)v99 comp-3 value +0.
                      09  bl-out-lf-net   pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ah-net   pic s9(9)v99 comp-3 value +0.
                      09  bl-out-lf-iss-com
                                          pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ah-iss-com
                                          pic s9(9)v99 comp-3 value +0.
                      09  bl-out-lf-ref-com
                                          pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ah-ref-com
                                          pic s9(9)v99 comp-3 value +0.
                      09  bl-out-lf-net-com
                                          pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ah-net-com
                                          pic s9(9)v99 comp-3 value +0.
                      09  bl-out-iss-com  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-ref-com  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-net-com  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-iss-cnt  pic s9(9)    comp-3 value +0.
                      09  bl-out-can-cnt  pic s9(9)    comp-3 value +0.
                      09  bl-out-net-cnt  pic s9(9)    comp-3 value +0.
                      09  bl-out-tot-prm  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-tot-ref  pic s9(9)v99 comp-3 value +0.
                      09  bl-out-net-tot  pic s9(9)v99 comp-3 value +0.
                  07  bl-out-cm-cnt    pic s9(9)     comp-3 value +0.
                  07  bl-out-lf-clms-pd
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-lf-clms-void
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-lf-tot-clms
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-ah-clms-pd
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-ah-clms-void
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-ah-tot-clms
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-clms-pd   pic s9(9)v99  comp-3 value +0.
                  07  bl-out-clms-void pic s9(9)v99  comp-3 value +0.
                  07  bl-out-tot-clms  pic s9(9)v99  comp-3 value +0.
                  07  bl-out-lf-clms-cur
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-ah-clms-cur
                                       pic s9(9)v99  comp-3 value +0.
                  07  bl-out-tot-clms-cur
                                       pic s9(9)v99  comp-3 value +0.
100220            07  bl-tot-inforce   pic s9(11)v99 comp-3 value +0.
      *                                copy ELCFUNDT.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELCFUNDT.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE *
00005 *                            VMOD=2.001                         *
00006 *                                                               *
00007 *           COPYBOOK FOR THE FUNCTION DATE FORMAT               *
00008 *                                                               *
00009 *****************************************************************
00010
00011
00012  01  FUNCTION-DATE.
00013      05  WS-FN-DATE                PIC 9(8)    VALUE ZEROS.
00014      05  WS-FN-CYMD  REDEFINES  WS-FN-DATE.
00015          10  WS-FN-CCYR            PIC 9(4).
00016          10  WS-FN-CCYY  REDEFINES  WS-FN-CCYR.
00017              15  WS-FN-CC          PIC 99.
00018              15  WS-FN-YR          PIC 99.
00019          10  WS-FN-MO              PIC 99.
00020          10  WS-FN-DA              PIC 99.
00021      05  WS-FN-HOURS               PIC 99      VALUE ZEROS.
00022      05  WS-FN-MINUTES             PIC 99      VALUE ZEROS.
00023      05  WS-FN-SECONDS             PIC 99      VALUE ZEROS.
00024      05  WS-FN-HUNDSECS            PIC 99      VALUE ZEROS.
00025      05  WS-FN-GMT-IND             PIC X       VALUE SPACES.
00026          88  WS-BEHIND-GMT                     VALUE '-'.
00027          88  WS-AFTER-GMT                      VALUE '+'.
00028          88  WS-NO-GMT                         VALUE ZERO.
00029      05  WS-FN-GMT-HOURS           PIC 99      VALUE ZEROS.
00030      05  WS-FN-GMT-MINUTES         PIC 99      VALUE ZEROS.
      *                                COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
      * 01  TRAN-DATA-LINE1             PIC X(80)    VALUE
      *     'BEGINJOB mode=''MVS'''.
      * 01  TRAN-DATA-LINE2             PIC X(80)    VALUE
      *     'LABEL name=PERL1'.
      * 01  TRAN-DATA-LINE3             PIC X(80)    VALUE
      *     'ASSGNDD ddname=''SYSIN'' type=''INSTREAM''  << !'.
      * 01  TRAN-DATA-LINE4             PIC X(80)    VALUE
      *     'perl /export/home/pema/bin/bal_sheet_ss.pl $SEQFILES/a1.txt
      *-    '$SEQFILES/a2.xlsx'
      * 01  TRAN-DATA-LINE5             PIC X(80)    VALUE '!'.
      * 01  TRAN-DATA-LINE6             PIC X(80)    VALUE
      *    'EXECPGM pgmname=''BPXBATCH'' stepname=''PERL1'' parm=''SH'''
      * 01  TRAN-DATA-LINE7             PIC X(80)    VALUE
      *     'LABEL name=EMAIL'.
      * 01  TRAN-DATA-LINE8             PIC X(80)    VALUE
      *     'ASSGNDD ddname=''SYSIN'' type=''INSTREAM''  << !'.
      * 01  TRAN-DATA-LINE9             PIC X(80)    VALUE
      *     'mutt -s ''AHL chkpts'' -a $SEQFILES/ahlmechkpts.xlsx pema@c
      *-    'o.com < /dev/null'.
      * 01  TRAN-DATA-LINE10            PIC X(80)    VALUE '!'.
      * 01  TRAN-DATA-LINE11            PIC X(80)    VALUE
      *    'EXECPGM pgmname=''BPXBATCH'' stepname=''EMAIL'' parm=''SH'''
      * 01  TRAN-DATA-LINE12            PIC X(80)    VALUE
      *     'ENDJOB'.
100220 01  ws-work-dynamic.
100220     05  ws-seqfiles-dir         pic x(27) value spaces.
100220     05  ws-jcllib-dir           pic x(27) value spaces.
100220     05  ws-work-env             pic x(8)  value spaces.
100220     05  ws-work-comp-id         pic xxx   value spaces.
100220     05  ws-work-file-in         pic x(16) value spaces.
100220     05  ws-work-rpt-out         pic x(13) value spaces.
100220     05  ws-ecs010-jobname       pic x(10)  value spaces.
100220     05  ws-ecs080-jobname       pic x(10)  value spaces.
100220
100220 01  ws-ecs010-in                pic x(58).
100220 01  ws-ecs080-in                pic x(58).
100220 01  ws-checkpoint-out           pic x(58).
100220
100220 01  P pointer.
100220 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
100220 01  var-ptr pointer.
100220 01  env-var-len                 pic 9(4)  binary.
100220 01  rc                          pic 9(9)  binary.
100220
100220 01  WS-KIXSYS.
100220     05  WS-KIX-FIL1             PIC X(10).
100220     05  WS-KIX-APPS             PIC X(10).
100220     05  WS-KIX-ENV              PIC X(10).
100220     05  WS-KIX-MYENV            PIC X(10).
100220     05  WS-KIX-SYS              PIC X(10).
       01  TRAN-DATA-LINE1             PIC X(80)    VALUE
           'cd /apps/prod/cid1p/jcl'.
       01  TRAN-DATA-LINE2.
           05  filler                  pic x(10) value 'unikixjob '.
           05  tran-comp-id            pic xxx   value '   '.
           05  filler                 pic x(15) value 'chkpts -k cid1p'.
           05  filler                  pic x(52) value spaces.
041417 01  filler.
041417     12  WS-RESPONSE2            PIC S9(8)   COMP.
041417     12  WS-RESPONSE             PIC S9(8)   COMP.
041417         88  RESP-NORMAL                  VALUE +00.
041417         88  RESP-NOTFND                  VALUE +13.
041417         88  RESP-DUPREC                  VALUE +14.
041417         88  RESP-DUPKEY                  VALUE +15.
041417         88  RESP-NOTOPEN                 VALUE +19.
041417         88  RESP-ENDFILE                 VALUE +20.
100220 01  ws-ECS010-IN-REC.
           05  filler                  pic x(21).
           05  ecs010-cert-cnt         pic x(11).
           05  filler                  pic x(62).
100220 01  ws-ECS080-IN-REC.
101713     05  filler                  pic x(21).
100220     05  ecs080-mort-resv        pic x(13).
101713     05  filler                  pic x(49).
       01  cpo-out-record.
           05  cpo-jobname             pic x(10)  value spaces.
           05  f                       pic x value ';'.
           05  cpo-stepname            pic x(6)   value spaces.
           05  f                       pic x value ';'.
           05  cpo-low-amount          pic -9(9)  value zeros.
           05  f                       pic x value ';'.
           05  cpo-hi-amount           pic -9(9)  value zeros.
           05  f                       pic x value ';'.
           05  cpo-desc                pic x(50)  value spaces.
       01  ahl-table.
           05 f                        pic x(68) value
               'AHLGM10   ;EL524 ;Total Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Processable Written & Outstanding Bala
      -        'nce         '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Non-Processable Written & Outstanding
      -        'Balance     '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Total Written & Outstanding Balance
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Processable Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Non-Processable Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Total Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Processable Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Non-Processable Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Total Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Processable Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Non-Processable Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM10   ;EL523 ;Total Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM15   ;ECS010;Life Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM15   ;ECS010;A&H Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'AHLGM15   ;ECS010;Output Certificate Master Records Prio
      -        'r Month     '.
           05 f                        pic x(68) value
               'AHLGM17   ;ECS080;Gross Reserve
      -        '            '.
       01  dcc-table.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL524 ;Total Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Processable Written & Outstanding Bala
      -        'nce         '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Non-Processable Written & Outstanding
      -        'Balance     '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Total Written & Outstanding Balance
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Processable Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Non-Processable Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Total Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Processable Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Non-Processable Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Total Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Processable Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Non-Processable Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM10 ;EL523 ;Total Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM15 ;ECS010;Life Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM15 ;ECS010;A&H Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'CIDCLGM15 ;ECS010;Output Certificate Master Records Prio
      -        'r Month     '.
           05 f                        pic x(68) value
               'CIDCLGM17 ;ECS080;Gross Reserve
      -        '            '.
020816 01  VPP-table.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL524 ;Total Claims Paid
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Processable Written & Outstanding Bala
020816-        'nce         '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Non-Processable Written & Outstanding
020816-        'Balance     '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Total Written & Outstanding Balance
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Processable Cancelled
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Non-Processable Cancelled
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Total Cancelled
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Processable Net Premium
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Non-Processable Net Premium
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Total Net Premium
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Processable Net Commission
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Non-Processable Net Commission
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM10 ;EL523 ;Total Net Commission
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM15 ;ECS010;Life Claims Paid
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM15 ;ECS010;A&H Claims Paid
020816-        '            '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM15 ;ECS010;Output Certificate Master Records Prio
020816-        'r Month     '.
020816     05 f                        pic x(68) value
020816         'CIVPLGM17 ;ECS080;Gross Reserve
020816-        '            '.
       01  cid-table.
           05 f                        pic x(68) value
               'CILGM10   ;EL524 ;Total Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Processable Written & Outstanding Bala
      -        'nce         '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Non-Processable Written & Outstanding
      -        'Balance     '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Total Written & Outstanding Balance
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Processable Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Non-Processable Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Total Cancelled
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Processable Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Non-Processable Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Total Net Premium
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Processable Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Non-Processable Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'CILGM10   ;EL523 ;Total Net Commission
      -        '            '.
           05 f                        pic x(68) value
               'CILGM15   ;ECS010;Life Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'CILGM15   ;ECS010;A&H Claims Paid
      -        '            '.
           05 f                        pic x(68) value
               'CILGM15   ;ECS010;Output Certificate Master Records Prio
      -        'r Month     '.
           05 f                        pic x(68) value
               'CILGM17   ;ECS080;Gross Reserve
      -        '            '.
       01  filler redefines cid-table.
           05  filler occurs 17.
               10  tbl-jobname             pic x(10).
               10  f                       pic x.
               10  tbl-stepname            pic x(6).
               10  f                       pic x.
               10  tbl-desc                pic x(50).
       01  filler.
           05  filler occurs 17.
               10  tbl-low-amount      pic s9(9).
               10  tbl-hi-amount       pic s9(9).
      ************************************
      * fields used to read web data
      ************************************
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(80).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).
041417 01  w-template-name   pic x(48) value spaces.
100220 01  ws-res-per-thousand         pic 9(5)v9(4) value zeros comp-3.
100220 01  ws-new-mort-reserve         pic 9(9) value zeros comp-3.
       01  filler.
           05  ECS010-STATUS           PIC XX VALUE ZEROS.
101713     05  ECS080-STATUS           PIC XX VALUE ZEROS.
           05  CHKPNT-STATUS           pic xx value zeros.
           05  work-cert-cnt           pic x(11).
           05  numeric-cert-cnt        pic 9(9).
100220     05  work-mort-resv          pic x(13).
           05  work-mort-resv-lo       pic x(11).
           05  work-mort-resv-hi       pic x(11).
           05  numeric-mort-resv       pic 9(11).
100220     05  work-in-force           pic x(13).
100220     05  numeric-in-force        pic 9(11).
      *****************************************
      * symbol list for the BSDETAIL template
      *****************************************
       01 output-data.
          05  filler                   pic x(7) value"COMPID=".
          05  out-comp-id              pic xxx.
          05  filler                   pic x(8) value"&EOMDTE=".
          05  out-eom-date             pic x(10).
          05  filler                   pic x(9) value"&COMPNME=".
          05  out-comp-name            pic x(30).
          05  filler                   pic x(7) value"&FDATE=".
          05  out-format-date          pic x(20).
          05  filler                   pic x(10) value"&LFISSPRM=".
          05  out-lf-iss-prem          pic zz,z99,999.99.
          05  filler                   pic x(10) value"&LFREFPRM=".
          05  out-lf-ref-prem          pic zz,z99,999.99.
          05  filler                   pic x(10) value"&LFNETPRM=".
          05  out-lf-net-prem          pic -zz,zz9,999.99.
          05  filler                   pic x(10) value"&AHISSPRM=".
          05  out-ah-iss-prem          pic zz,zz9,999.99.
          05  filler                   pic x(10) value"&AHREFPRM=".
          05  out-ah-ref-prem          pic zz,z99,999.99.
          05  filler                   pic x(10) value"&AHNETPRM=".
          05  out-ah-net-prem          pic -zz,z99,999.99.
          05  filler                   pic x(10) value"&LFISSCOM=".
          05  out-lf-iss-comm          pic zz,zz9,999.99.
          05  filler                   pic x(10) value"&LFREFCOM=".
          05  out-lf-ref-comm          pic zz,zz9,999.99.
          05  filler                   pic x(10) value"&LFNETCOM=".
          05  out-lf-net-comm          pic -zz,zz9,999.99.
          05  filler                   pic x(10) value"&AHISSCOM=".
          05  out-ah-iss-comm          pic zz,zzz,999.99.
          05  filler                   pic x(10) value"&AHREFCOM=".
          05  out-ah-ref-comm          pic zz,z99,999.99.
          05  filler                   pic x(10) value"&AHNETCOM=".
          05  out-ah-net-comm          pic -zz,z99,999.99.
          05  filler                   pic x(9) value "&TOTPREM=".
          05  out-tot-prem             pic zz,z99,999.99.
          05  filler                   pic x(9) value "&TOTREFS=".
          05  out-tot-refs             pic zz,z99,999.99.
          05  filler                   pic x(9) value"&TOTNETP=".
          05  out-tot-netp             pic -zz,z99,999.99.
          05  filler                   pic x(8) value"&ISSCOM=".
          05  out-iss-comm             pic zz,zz9,999.99.
          05  filler                   pic x(8) value"&REFCOM=".
          05  out-ref-comm             pic zz,z99,999.99.
          05  filler                   pic x(9) value"&TOTNCOM=".
          05  out-net-comm             pic -zz,z99,999.99.
          05  filler                   pic x(8) value "&ISSCNT=".
          05  out-iss-cnt              pic zzzz,z99.
          05  filler                   pic x(8) value "&REFCNT=".
          05  out-ref-cnt              pic zzzz,999.
          05  filler                   pic x(8) value "&NETCNT=".
          05  out-net-cnt              pic -zzzz,999.
          05  filler                   pic x(8) value"&LFCLMS=".
          05  out-lf-clms-pd           pic zz,z99,999.99.
          05  filler                   pic x(9) value"&LFVOIDS=".
          05  out-lf-clms-void         pic zz,zz9,999.99.
          05  filler                   pic x(11) value"&TOTLFCLMS=".
          05  out-lf-tot-clms          pic -zz,z99,999.99.
          05  filler                   pic x(8) value"&AHCLMS=".
          05  out-ah-clms-pd           pic zz,z99,999.99.
          05  filler                   pic x(9) value"&AHVOIDS=".
          05  out-ah-clms-void         pic zz,zz9,999.99.
          05  filler                   pic x(11) value"&TOTAHCLMS=".
          05  out-ah-tot-clms          pic -zz,z99,999.99.
          05  filler                   pic x(8) value"&PDCLMS=".
          05  out-clms-pd              pic zz,z99,999.99.
          05  filler                   pic x(8) value"&VDCLMS=".
          05  out-clms-void            pic zz,z99,999.99.
          05  filler                   pic x(9) value"&TOTCLMS=".
          05  out-tot-clms             pic -zz,z99,999.99.
          05  filler                   pic x(8) value"&LORESV=".
          05  out-lo-resv              pic zzz,z99,999.
          05  filler                   pic x(8) value"&HIRESV=".
          05  out-hi-resv              pic zzz,z99,999.
          05  filler                   pic x(8) value"&CRTCNT=".
          05  out-crt-cnt              pic z,zz9,999.
          05  filler                   pic x(5) value "&MSG=".
          05  output-msg               pic x(50).
       01 bl-index               pic s9(8) comp.
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
       01  DFHCOMMAREA       PIC X(01).
100220 01  var                         pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'BSSRCH' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
100220     set P to address of KIXSYS
100220     CALL "getenv" using by value P returning var-ptr
100220     if var-ptr = null then
100220        display ' kixsys not set '
100220     else
100220        set address of var to var-ptr
100220        move 0 to env-var-len
100220        inspect var tallying env-var-len
100220          for characters before X'00'
100220        unstring var (1:env-var-len) delimited by '/'
100220           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
100220              WS-KIX-SYS
100220        end-unstring
100220     end-if
100220     display ' ws-kixsys ' ws-kixsys
100220     display ' my env    ' ws-kix-myenv
           .
       0000-begin.
           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           
      * exec cics web
      *       startbr formfield resp(w-resp)
      *    end-exec
      *    MOVE 'X(f                   &  N#00000921' TO DFHEIV0
           MOVE X'582866202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           perform 0200-read-form      thru 0200-exit until
              w-resp not = 0 
      *   dfhresp(normal)
           
      * exec cics web
      *       endbr formfield
      *    end-exec
      *    MOVE 'X,f                   #   #00000926' TO DFHEIV0
           MOVE X'582C66202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           display ' input date   ' bl-input-eom-date
           display ' comp id      ' bl-input-comp-id
           display ' mort resv lo ' work-mort-resv-lo
           display ' mort resv hi ' work-mort-resv-hi
           display ' build file   ' bl-input-bld-file
100220     evaluate bl-input-comp-id
100220        when 'CID'
100220           move 'cilgm15'        to ws-ecs010-jobname
100220           move 'cilgm17'        to ws-ecs080-jobname
100220        when 'AHL'
100220           move 'ahlgm15'        to ws-ecs010-jobname
100220           move 'ahlgm17'        to ws-ecs080-jobname
100220        when 'DCC'
100220           move 'cidclgm15'      to ws-ecs010-jobname
100220        when 'VPP'
100220           move 'vplgm15'        to ws-ecs010-jobname
100220     end-evaluate
100220     move bl-input-comp-id       to ws-work-comp-id
100220     move ws-kix-myenv           to ws-work-env
100220     if ws-kix-myenv = 'cid1p'
100220        move '/data/seqfiles/'   to ws-seqfiles-dir
110220        move ' cd /apps/prod/cid1p/jcl'
100220                                 to tran-data-line1
100220     else
100220        string
100220           '/data/test/'  delimited by size
100220           ws-work-env    delimited by space
100220           '/seqfiles/'   delimited by size
100220              into ws-seqfiles-dir
100220        end-string
100220        string
100220           ' cd /apps/test/'  delimited by size
100220           ws-work-env        delimited by space
100220           '/jcl'             delimited by size
100220              into tran-data-line1
100220        end-string
100220     end-if
100220     move spaces to tran-data-line2
100220     string
100220         'unikixjob '     delimited by size
100220         function lower-case(bl-input-comp-id) delimited by size
100220         'chkpts -k '     delimited by size
100220         ws-work-env      delimited by space
100220            into tran-data-line2
100220      end-string
100220
100220     string
100220        ws-seqfiles-dir delimited by space
100220        ws-ecs010-jobname delimited by space
100220        '.ECS010.ME50.BAL.AMTS' delimited by size
100220           into ws-ecs010-in
100220     end-string
100220     string
100220        ws-seqfiles-dir delimited by space
100220        ws-ecs080-jobname delimited by space
100220        '.ECS080.ME.BAL.AMTS' delimited by size
100220           into ws-ecs080-in
100220     end-string
100220     string
100220        ws-seqfiles-dir delimited by space
100220        function lower-case(bl-input-comp-id) delimited by size
100220        'mechkpts.txt' delimited by size
100220           into ws-checkpoint-out
100220     end-string
           open input ecs010-in
           if ecs010-status not = '00'
              move ' cant open ecs010 file '
                                       to bl-output-message
              go to 0100-send-document
           end-if
           read ecs010-in into ws-ecs010-in-rec
           move ecs010-cert-cnt        to work-cert-cnt
           close ecs010-in
           if bl-input-comp-id = 'CID' or 'AHL'
              open input ecs080-in
              if ecs080-status not = '00'
                 move ' cant open ecs080 file '
                                       to bl-output-message
                 go to 0100-send-document
              end-if
              read ecs080-in into ws-ecs080-in-rec
              move ecs080-mort-resv       to work-mort-resv
              if bl-input-comp-id = 'CID'
                 read ecs080-in into ws-ecs080-in-rec
                 move ecs080-mort-resv    to work-in-force
              end-if
              close ecs080-in
           end-if
           move zeros                  to numeric-cert-cnt
           inspect work-cert-cnt replacing all spaces by zeros
           move +9 to s2
           perform varying s1 from +11 by -1 until s1 < +1
              if work-cert-cnt (s1:1) numeric
                 move work-cert-cnt (s1:1)
                                       to numeric-cert-cnt (s2:1)
                 subtract +1 from s2
              end-if
           end-perform
           move numeric-cert-cnt       to bl-out-cm-cnt
100220     if bl-input-comp-id = 'CID'
100220        move zeros               to numeric-mort-resv
100220        inspect work-mort-resv replacing all spaces by zeros
100220        display ' work mort reserve  **' work-mort-resv '**'
100220        move +11 to s2
100220        perform varying s1 from +13 by -1 until s1 < +1
100220           if work-mort-resv (s1:1) numeric
100220              move work-mort-resv (s1:1)
100220                                 to numeric-mort-resv (s2:1)
100220              subtract +1 from s2
100220           end-if
100220        end-perform
100220        move zeros               to numeric-in-force
100220        inspect work-in-force  replacing all spaces by zeros
100220        move +11 to s2
100220        perform varying s1 from +13 by -1 until s1 < +1
100220           if work-in-force  (s1:1) numeric
100220              move work-in-force  (s1:1)
100220                                 to numeric-in-force (s2:1)
100220              subtract +1 from s2
100220           end-if
100220        end-perform
100220     else
101713        if bl-input-comp-id = 'AHL'
101713           move zeros            to numeric-mort-resv
101713           inspect work-mort-resv replacing all spaces by zeros
101713           move +11 to s2
101713           perform varying s1 from +13 by -1 until s1 < +1
101713              if work-mort-resv (s1:1) numeric
101713                 move work-mort-resv (s1:1)
101713                                 to numeric-mort-resv (s2:1)
101713                 subtract +1 from s2
101713              end-if
101713           end-perform
101713           move numeric-mort-resv to bl-input-hi-resv
123014           if bl-input-hi-resv < 100000
101713              move zeros         to bl-input-lo-resv
101713           else
123014              compute bl-input-lo-resv =
                        bl-input-hi-resv - 100000
101713           end-if
101713        else
                 move zeros            to numeric-mort-resv
      ****       inspect work-mort-resv replacing all spaces by zeros
                 move +9 to s2
                 perform varying s1 from +11 by -1 until s1 < +1
                    if work-mort-resv-lo (s1:1) numeric
                       move work-mort-resv-lo (s1:1)
                                       to numeric-mort-resv (s2:1)
                       subtract +1 from s2
                    end-if
                 end-perform
                 move numeric-mort-resv to bl-input-lo-resv
                 move zeros            to numeric-mort-resv
      ****       inspect work-mort-resv replacing all spaces by zeros
                 move +9 to s2
                 perform varying s1 from +11 by -1 until s1 < +1
                    if work-mort-resv-hi (s1:1) numeric
                       move work-mort-resv-hi (s1:1)
                                       to numeric-mort-resv (s2:1)
                       subtract +1 from s2
                    end-if
                 end-perform
                 move numeric-mort-resv to bl-input-hi-resv
              end-if
101713     end-if
100220     if bl-input-comp-id = 'CID'
100220        
      * exec cics link
100220*          program  ('BSRAMTBL')
100220*          commarea (srch-commarea)
100220*       end-exec
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'BSRAMTBL' TO DFHEIV1
      *    MOVE '."C                   (   #00001095' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100220     end-if
100220     DISPLAY ' REM AMOUNT ' BL-tot-inforce
100220     if bl-input-comp-id = 'CID'
100220        compute ws-res-per-thousand = numeric-mort-resv /
100220           (numeric-in-force / 1000)
100220        compute ws-new-mort-reserve =
100220           ws-res-per-thousand * (bl-tot-inforce / 1000)
100220        compute bl-input-lo-resv = ws-new-mort-reserve * .95
100220        compute bl-input-hi-resv = ws-new-mort-reserve * 1.05
100220     end-if
           
      * exec cics link
      *       program  ('BSSRCHBL')
      *       commarea (srch-commarea)
      *    end-exec
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'BSSRCHBL' TO DFHEIV1
      *    MOVE '."C                   (   #00001109' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0100-send-document.
      ***********************************************************
      * Build output document.
      ***********************************************************
           move bl-input-comp-id       to out-comp-id
           move bl-input-eom-date      to out-eom-date
           move bl-input-lo-resv       to out-lo-resv
           move bl-input-hi-resv       to out-hi-resv
           move bl-out-company-name    to out-comp-name
           move bl-out-formated-date   to out-format-date
           move bl-out-lf-prem (1)     to out-lf-iss-prem
           move bl-out-ah-prem (1)     to out-ah-iss-prem
           move bl-out-tot-prm (1)     to out-tot-prem
           move bl-out-lf-ref  (1)     to out-lf-ref-prem
           move bl-out-ah-ref  (1)     to out-ah-ref-prem
           move bl-out-tot-ref (1)     to out-tot-refs
           move bl-out-lf-net  (1)     to out-lf-net-prem
           move bl-out-ah-net  (1)     to out-ah-net-prem
           move bl-out-lf-iss-com (1)  to out-lf-iss-comm
           move bl-out-ah-iss-com (1)  to out-ah-iss-comm
           move bl-out-lf-ref-com (1)  to out-lf-ref-comm
           move bl-out-ah-ref-com (1)  to out-ah-ref-comm
           move bl-out-lf-net-com (1)  to out-lf-net-comm
           move bl-out-ah-net-com (1)  to out-ah-net-comm
           move bl-out-iss-com    (1)  to out-iss-comm
           move bl-out-ref-com    (1)  to out-ref-comm
           move bl-out-net-com    (1)  to out-net-comm
           move bl-out-iss-cnt    (1)  to out-iss-cnt
           move bl-out-can-cnt    (1)  to out-ref-cnt
           move bl-out-net-cnt    (1)  to out-net-cnt
           move bl-out-net-tot    (1)  to out-tot-netp
           move bl-out-cm-cnt          to out-crt-cnt
           move bl-out-lf-clms-pd      to out-lf-clms-pd
           move bl-out-lf-clms-void    to out-lf-clms-void
           move bl-out-lf-tot-clms     to out-lf-tot-clms
           move bl-out-ah-clms-pd      to out-ah-clms-pd
           move bl-out-ah-clms-void    to out-ah-clms-void
           move bl-out-ah-tot-clms     to out-ah-tot-clms
           move bl-out-clms-pd         to out-clms-pd
           move bl-out-clms-void       to out-clms-void
           move bl-out-tot-clms        to out-tot-clms
           move bl-output-message      to output-msg
           if bl-input-bld-file = 'YES'
              move 'Balance File Created'
                                       to output-msg
           end-if
041417     move 'BSDETAIL'             to w-template-name
           
      * exec cics document create
      *       doctoken   (w-doctoken)
041417*       template   (w-template-name)
041417*       template   ('BSDETAIL')
      *       symbollist (output-data)
041417*       resp       (ws-response)
041417*       resp2      (ws-response2)
      *       listlength (length of output-data)
      *    end-exec
           MOVE LENGTH OF
            output-data TO DFHEIV16
      *    MOVE '\"D tSL               )  N#00001161' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 w-template-name, 
                 output-data, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           MOVE EIBRESP2 TO ws-response2
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
041417*    display ' af doc create ' ws-response ' ' ws-response2
           move 1                      to bl-index
      *    exec cics document insert
      *       doctoken (w-doctoken)
      *       template ('BSFTR')
      *    end-exec
          if bl-fail
             
      * exec cics syncpoint rollback
      *      end-exec
      *    MOVE '6"R                   !   #00001177' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
          end-if
      ****************************************
      * Send the document and return.
      ****************************************
           
      * exec cics web send
      *       doctoken(w-doctoken)
      *    end-exec
      *    MOVE 'X$D                   *   #00001183' TO DFHEIV0
           MOVE X'582444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202A20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if bl-input-bld-file = 'YES'
              evaluate bl-input-comp-id
                 when 'AHL'
                    move ahl-table     to cid-table
                    perform 0300-build-cid
                                       thru 0300-exit
                    perform 0400-write-ahl
                                       thru 0400-exit
                 when 'CID'
                    perform 0300-build-cid
                                       thru 0300-exit
                    perform 0500-write-cid
                                       thru 0500-exit
                 when 'DCC'
                    move dcc-table     to cid-table
                    perform 0300-build-cid
                                       thru 0300-exit
                    perform 0600-write-dcc
                                       thru 0600-exit
020816           when 'VPP'
020816              move VPP-table     to cid-table
020816              perform 0300-build-cid
020816                                 thru 0300-exit
020816              perform 0650-write-vpp
020816                                 thru 0650-exit
              end-evaluate
           end-if
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    ''   #00001213' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0200-read-form.
           move spaces                 to w-form-name
           move length of w-form-name  to w-form-name-len
           move spaces                 to w-form-value
           move length of w-form-value to w-form-value-len
           
      * exec cics web readnext
      *       formfield   (w-form-name)
      *       namelength  (w-form-name-len)
      *       value       (w-form-value)
      *       valuelength (w-form-value-len)
      *       resp        (w-resp)
      *    end-exec
      *    MOVE 'X*FLVL                &  N#00001221' TO DFHEIV0
           MOVE X'582A464C564C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           evaluate w-resp
              when 0 
      *   dfhresp(normal)
                 evaluate w-form-name(1:w-form-name-len)
                    when 'eom_dte'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to bl-input-eom-date
                       else
                          move spaces  to bl-input-eom-date
                      end-if
                    when 'comp_id'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to bl-input-comp-id
                       else
                          move spaces  to bl-input-comp-id
                      end-if
                    when 'lo_resv'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to work-mort-resv-lo
                       else
                          move zeros   to work-mort-resv-lo
                      end-if
                    when 'hi_resv'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to work-mort-resv-hi
                       else
                          move zeros   to work-mort-resv-hi
                      end-if
                    when 'bld_file'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to bl-input-bld-file
                       else
                          move spaces  to bl-input-bld-file
                      end-if
                 end-evaluate
              when other
                 continue
           end-evaluate
           .
       0200-exit.
           exit.
       0300-build-cid.
082013     move +.98                   to ws-low-pct
082013     move +1.02                  to ws-high-pct
           perform varying s1 from +1 by +1 until s1 > +17
              move -999999999          to tbl-low-amount (s1)
              move +999999999          to tbl-hi-amount  (s1)
           end-perform
           move bl-out-tot-clms        to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (1)
           move ws-hi-amount           to tbl-hi-amount (1)
      *** processable gross premium
           move bl-out-tot-prm (1)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (2)
           move ws-hi-amount           to tbl-hi-amount (2)
      *** non-processable gross premium
082013     move +.90                   to ws-low-pct
082013     move +1.10                  to ws-high-pct
           move bl-out-tot-prm (2)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (3)
           move ws-hi-amount           to tbl-hi-amount (3)
      *** total gross premium
082013     move +.98                   to ws-low-pct
082013     move +1.02                  to ws-high-pct
082013     compute tbl-low-amount (4) =
082013        tbl-low-amount (2) + tbl-low-amount (3)
082013     compute tbl-hi-amount (4) =
082013        tbl-hi-amount (2) + tbl-hi-amount (3)
      *** processable refunded premium
           move bl-out-tot-ref (1)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (5)
           move ws-hi-amount           to tbl-hi-amount (5)
      *** non-processable refunded premium
031714     move +.60                   to ws-low-pct
031714     move +1.40                  to ws-high-pct
           move bl-out-tot-ref (2)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
040214     if (ws-pass-amount - ws-low-amount) < 5000
040214        compute ws-low-amount = ws-low-amount - 5000
040214        compute ws-hi-amount = ws-hi-amount + 5000
040214     end-if
           move ws-low-amount          to tbl-low-amount (6)
           move ws-hi-amount           to tbl-hi-amount (6)
      *** total refunded premium
082013     move +.98                   to ws-low-pct
082013     move +1.02                  to ws-high-pct
082013     compute tbl-low-amount (7) =
082013        tbl-low-amount (5) + tbl-low-amount (6)
082013     compute tbl-hi-amount (7) =
082013        tbl-hi-amount (5) + tbl-hi-amount (6)
      *** processable net premium
           move bl-out-net-tot (1)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (8)
           move ws-hi-amount           to tbl-hi-amount (8)
      *** non-processable net premium
082013     move +.90                   to ws-low-pct
082013     move +1.10                  to ws-high-pct
           move bl-out-net-tot (2)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (9)
           move ws-hi-amount           to tbl-hi-amount (9)
      *** total net premium
082013     move +.98                   to ws-low-pct
082013     move +1.02                  to ws-high-pct
082013     compute tbl-low-amount (10) =
082013        tbl-low-amount (8) + tbl-low-amount (9)
082013     compute tbl-hi-amount (10) =
082013        tbl-hi-amount (8) + tbl-hi-amount (9)
      *** processable commission
           move bl-out-net-com (1)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (11)
           move ws-hi-amount           to tbl-hi-amount (11)
      *** non-processable commission
082013     move +.90                   to ws-low-pct
082013     move +1.10                  to ws-high-pct
           move bl-out-net-com (2)     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (12)
           move ws-hi-amount           to tbl-hi-amount (12)
      *** total commission
082013     move +.98                   to ws-low-pct
082013     move +1.02                  to ws-high-pct
082013     compute tbl-low-amount (13) =
082013        tbl-low-amount (11) + tbl-low-amount (12)
082013     compute tbl-hi-amount (13) =
082013        tbl-hi-amount (11) + tbl-hi-amount (12)
           move bl-out-lf-tot-clms     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (14)
           move ws-hi-amount           to tbl-hi-amount (14)
           move bl-out-ah-tot-clms     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move ws-low-amount          to tbl-low-amount (15)
           move ws-hi-amount           to tbl-hi-amount (15)
           move bl-out-cm-cnt          to tbl-low-amount (16)
                                          tbl-hi-amount  (16)
           move bl-input-lo-resv       to tbl-low-amount (17)
           move bl-input-hi-resv       to tbl-hi-amount (17)
           .
       0300-exit.
           exit.
       0310-calc-tol.
           if ws-pass-amount > zeros
082013        compute ws-low-amount = ws-pass-amount * ws-low-pct
082013        compute ws-hi-amount  = ws-pass-amount * ws-high-pct
           else
082013        compute ws-low-amount = ws-pass-amount * ws-high-pct
082013        compute ws-hi-amount  = ws-pass-amount * ws-low-pct
           end-if
           .
       0310-exit.
           exit.
       0400-write-ahl.
100220     open output chkpoint-out
           if chkpnt-status not = '00'
              move ' Cant open AHL CHKPNT file '
                 to bl-output-message
              go to 0100-send-document
           end-if
           move bl-input-eom-date      to cpo-jobname
           move 'AHL'                  to cpo-stepname
           string ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
              ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds
              delimited by size into cpo-desc
           end-string
100220     write chkpoint-out-rec  from cpo-out-record
           move spaces                 to cpo-jobname
                                          cpo-stepname
           move 'Proc Cnt'             to cpo-desc
           move bl-out-iss-cnt (1)     to cpo-low-amount
           move bl-out-can-cnt (1)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move 'NonProcCnt'           to cpo-desc
           move bl-out-iss-cnt (2)     to cpo-low-amount
           move bl-out-can-cnt (2)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move 'Total Cnt'            to cpo-desc
           move bl-out-iss-cnt (3)     to cpo-low-amount
           move bl-out-can-cnt (3)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-lf-clms-cur     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'LF Claims Paid Curr'  to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-ah-clms-cur     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'AH Claims Paid Curr'  to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-tot-clms-cur    to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'TOT Claims Paid Curr' to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           perform varying s1 from 1 by 1 until s1 > 17
              move tbl-jobname    (s1) to cpo-jobname
              move tbl-stepname   (s1) to cpo-stepname
              move tbl-desc       (s1) to cpo-desc
              move tbl-low-amount (s1) to cpo-low-amount
              move tbl-hi-amount  (s1) to cpo-hi-amount
100220        write chkpoint-out-rec from cpo-out-record
           end-perform
100220     close chkpoint-out
           perform 0700-submit-job     thru 0700-exit
           .
       0400-exit.
           exit.
       0500-write-cid.
100220     open output chkpoint-out
           if chkpnt-status not = '00'
              move ' Cant open CID CHKPNT file '
                 to bl-output-message
              go to 0100-send-document
           end-if
           move bl-input-eom-date      to cpo-jobname
           move 'CID'                  to cpo-stepname
           string ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
              ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds
              delimited by size into cpo-desc
           end-string
100220     write chkpoint-out-rec  from cpo-out-record
           move spaces                 to cpo-jobname
                                          cpo-stepname
           move 'Proc Cnt'             to cpo-desc
           move bl-out-iss-cnt (1)     to cpo-low-amount
           move bl-out-can-cnt (1)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move 'NonProcCnt'           to cpo-desc
           move bl-out-iss-cnt (2)     to cpo-low-amount
           move bl-out-can-cnt (2)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move 'Total Cnt'            to cpo-desc
           move bl-out-iss-cnt (3)     to cpo-low-amount
           move bl-out-can-cnt (3)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-lf-clms-cur     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'LF Claims Paid Curr'  to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-ah-clms-cur     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'AH Claims Paid Curr'  to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-tot-clms-cur    to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'TOT Claims Paid Curr' to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           perform varying s1 from 1 by 1 until s1 > 17
              move tbl-jobname    (s1) to cpo-jobname
              move tbl-stepname   (s1) to cpo-stepname
              move tbl-desc       (s1) to cpo-desc
              move tbl-low-amount (s1) to cpo-low-amount
              move tbl-hi-amount  (s1) to cpo-hi-amount
100220        write chkpoint-out-rec   from cpo-out-record
           end-perform
100220     close chkpoint-out
           perform 0700-submit-job     thru 0700-exit
           .
       0500-exit.
           exit.
       0600-write-dcc.
100220     open output chkpoint-out
           if chkpnt-status not = '00'
              move ' Cant open DCC CHKPNT file '
                 to bl-output-message
              go to 0100-send-document
           end-if
           move bl-input-eom-date      to cpo-jobname
           move 'DCC'                  to cpo-stepname
           string ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
              ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds
              delimited by size into cpo-desc
           end-string
100220     write chkpoint-out-rec  from cpo-out-record
           move spaces                 to cpo-jobname
                                          cpo-stepname
           move 'Proc Cnt'             to cpo-desc
           move bl-out-iss-cnt (1)     to cpo-low-amount
           move bl-out-can-cnt (1)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move 'NonProcCnt'           to cpo-desc
           move bl-out-iss-cnt (2)     to cpo-low-amount
           move bl-out-can-cnt (2)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move 'Total Cnt'            to cpo-desc
           move bl-out-iss-cnt (3)     to cpo-low-amount
           move bl-out-can-cnt (3)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-lf-clms-cur     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'LF Claims Paid Curr'  to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-ah-clms-cur     to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'AH Claims Paid Curr'  to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           move bl-out-tot-clms-cur    to ws-pass-amount
           perform 0310-calc-tol       thru 0310-exit
           move 'TOT Claims Paid Curr' to cpo-desc
           move ws-low-amount          to cpo-low-amount
           move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
           perform varying s1 from 1 by 1 until s1 > 16
              move tbl-jobname    (s1) to cpo-jobname
              move tbl-stepname   (s1) to cpo-stepname
              move tbl-desc       (s1) to cpo-desc
              move tbl-low-amount (s1) to cpo-low-amount
              move tbl-hi-amount  (s1) to cpo-hi-amount
100220        write chkpoint-out-rec from cpo-out-record
           end-perform
100220     close chkpoint-out
           perform 0700-submit-job     thru 0700-exit
           .
       0600-exit.
           exit.
020816 0650-write-vpp.
020816
100220     open output chkpoint-out
020816
020816     if chkpnt-status not = '00'
020816        move ' Cant open vpp CHKPNT file '
020816           to bl-output-message
020816        go to 0100-send-document
020816     end-if
020816
020816     move bl-input-eom-date      to cpo-jobname
020816     move 'VPP'                  to cpo-stepname
020816     string ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
020816        ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds
020816        delimited by size into cpo-desc
020816     end-string
100220     write chkpoint-out-rec  from cpo-out-record
020816
020816     move spaces                 to cpo-jobname
020816                                    cpo-stepname
020816
020816     move 'Proc Cnt'             to cpo-desc
020816     move bl-out-iss-cnt (1)     to cpo-low-amount
020816     move bl-out-can-cnt (1)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
020816     move 'NonProcCnt'           to cpo-desc
020816     move bl-out-iss-cnt (2)     to cpo-low-amount
020816     move bl-out-can-cnt (2)     to cpo-hi-amount
020816     write chkpoint-out-rec  from cpo-out-record
020816     move 'Total Cnt'            to cpo-desc
020816     move bl-out-iss-cnt (3)     to cpo-low-amount
020816     move bl-out-can-cnt (3)     to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
020816
020816
020816     move bl-out-lf-clms-cur     to ws-pass-amount
020816     perform 0310-calc-tol       thru 0310-exit
020816     move 'LF Claims Paid Curr'  to cpo-desc
020816     move ws-low-amount          to cpo-low-amount
020816     move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
020816
020816     move bl-out-ah-clms-cur     to ws-pass-amount
020816     perform 0310-calc-tol       thru 0310-exit
020816     move 'AH Claims Paid Curr'  to cpo-desc
020816     move ws-low-amount          to cpo-low-amount
020816     move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
020816
020816     move bl-out-tot-clms-cur    to ws-pass-amount
020816     perform 0310-calc-tol       thru 0310-exit
020816     move 'TOT Claims Paid Curr' to cpo-desc
020816     move ws-low-amount          to cpo-low-amount
020816     move ws-hi-amount           to cpo-hi-amount
100220     write chkpoint-out-rec  from cpo-out-record
020816
020816     perform varying s1 from 1 by 1 until s1 > 16
020816        move tbl-jobname    (s1) to cpo-jobname
020816        move tbl-stepname   (s1) to cpo-stepname
020816        move tbl-desc       (s1) to cpo-desc
020816        move tbl-low-amount (s1) to cpo-low-amount
020816        move tbl-hi-amount  (s1) to cpo-hi-amount
100220        write chkpoint-out-rec from cpo-out-record
020816     end-perform
020816
100220     close chkpoint-out
020816
020816     perform 0700-submit-job     thru 0700-exit
020816
020816     .
020816 0650-exit.
020816     exit.
       0700-submit-job.
           move function lower-case(bl-input-comp-id)
                                       to tran-comp-id
           
      * EXEC CICS WRITEQ TD
      *       QUEUE ('BTCH')
      *       FROM (TRAN-DATA-LINE1)
      *       LENGTH (80)
      *    END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00001642' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS WRITEQ TD
      *       QUEUE ('BTCH')
      *       FROM (TRAN-DATA-LINE2)
      *       LENGTH (80)
      *    END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00001647' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE2, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0700-exit.
           exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'BSSRCH' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'BSSRCH' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
