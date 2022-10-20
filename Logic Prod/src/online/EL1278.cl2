      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1278.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/21/94 14:09:26.
00007 *                            VMOD=2.030.
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.
00025 *        TRANSACTION - EXXA - CANCELLATION QUOTE.
00023 *
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
070105* 070105    2005063000003  PEMA  COMMENT OUT CODE THAT ADDS 1
070104*         TO THE TERM AND REMAINING TERM
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
081606* 081606  CR2006051800002  PEMA  ADD POST CARD PROCESSING
111907* 111907  CR2004020600011  PEMA  FORCE FP TO REFUND AS FP
022608* 022608  CR2008010200006  PEMA  CHANGE MO RTERM MTHOD
050508* 050508  CR2008010200004  PEMA  FIX MO REFUNDING
090408* 090408  CR2008081900002  PEMA  UPDATE MO REFUNDING
021710* 021710  IR2010021000002  PEMA  ADD CANCEL DATE EDIT
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
042011* 042011  CR2010030900001  PEMA ADD CLM PROCESSING ON QUOTES
112911* 112911  CR2011083000003  PEMA  ADD SPPDDF TRUNCATED
010412* 010412  CR2011022800001  AJRA  NAPERSOFT CANCEL PROCESSING
032912* 032912  CR2011110200001  PEMA  AHL CHANGES
062712* 062712  CR2011022800001  AJRA NAPERSOFT
071712* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
102212* 102212  CR2012013100001  PEMA  ALLOW MN OVERRIDE RF
112612* 112612  CR2012101700002  AJRA  FIX CANCEL WHEN DEATH CLAIM
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
050713* 050713  CR2011062300001  PEMA  REM CHG FOR EXT DYS FOR ROA REF
041514* 041514  CR2012110200003  AJRA  REMOVE PF11 FROM SCREEN, 
041514*                                CANCEL QUOTE SCRIPT WILL DO PF11
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
072616* 072616  CR2016020400001  PEMA  ADD PF10 CONFIRMATION
032117* 032117  CR2017030300002  PEMA  ADD COMM % AND COMM AMT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc notes.
070622* 070622  CR2020061200002  TANA  Add cancel reason logic
091322* 091322  IR2022081700001  SJAA  Set fld len of can rea upon error
101201******************************************************************

00026  EJECT
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
00030  77  FILLER  PIC  X(32) VALUE '********************************'.
00031  77  FILLER  PIC  X(32) VALUE '*    EL1278 WORKING STORAGE    *'.
00032  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.030 *********'.
081606 77  S1                          PIC S999 COMP-3 VALUE +0.
       77  P1                          PIC S999 COMP-3 VALUE +0.
       77  P2                          PIC S999 COMP-3 VALUE +0.
       77  C0                          PIC S999 COMP-3 VALUE +0.
       77  C1                          PIC S999 COMP-3 VALUE +0.
       77  C2                          PIC S999 COMP-3 VALUE +0.
       77  C3                          PIC S999 COMP-3 VALUE +0.
       77  WS-STOP-SW                  PIC X  VALUE ' '.
           88  I-SAY-TO-STOP      VALUE 'Y'.
       77  WS-OPEN-LF-CLAIM            PIC X  VALUE ' '.
           88  OPEN-LF-CLAIM              VALUE 'Y'.
       77  WS-CLOSED-LF-CLAIM          PIC X  VALUE ' '.
           88  CLOSED-LF-CLAIM            VALUE 'Y'.
       77  WS-OPEN-AH-CLAIM            PIC X  VALUE ' '.
           88  OPEN-AH-CLAIM              VALUE 'Y'.
       77  WS-CLOSED-AH-CLAIM          PIC X  VALUE ' '.
           88  CLOSED-AH-CLAIM            VALUE 'Y'.
       77  WS-FOUND-A-CLAIM            PIC X  VALUE ' '.
           88  FOUND-A-CLAIM              VALUE 'Y'.
100518 77  WS-CLAIM-TYPE               PIC X  VALUE ' '.
100518     88  LIFE-CLAIM                 VALUE 'L'.
100518     88  OTHER-CLAIM                VALUE 'O'.
100518 77  WS-CLAIM-LIT                PIC X(06)  VALUE SPACES.

       77  WS-ST-REF-IND               PIC X.
       77  WS-LF-INCUR-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-AH-INCUR-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-AH-PAID-THRU-DT          PIC XX  VALUE LOW-VALUES.
       77  WS-LF-LAST-CLOSE-DT         PIC XX  VALUE LOW-VALUES.
       77  WS-AH-LAST-CLOSE-DT         PIC XX  VALUE LOW-VALUES.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
       77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
       77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
       77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
       77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
           88  NO-CERT-RW                 VALUE 'N'.
           88  CERT-RW                    VALUE 'Y'.
062712 77  WS-ERMAIL-SW                PIC X  VALUE ' '.
062712     88  ERMAIL-FOUND                 VALUE 'Y'.
121712 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE ZERO.
070622 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
070622 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.

070622 EXEC SQL
070622    INCLUDE SQLDA
070622 END-EXEC
070622
070622 EXEC SQL
070622    INCLUDE SQLCA
070622 END-EXEC
070622
070622 EXEC SQL
070622    BEGIN DECLARE SECTION
070622 END-EXEC
070622
070622 01  SQLCMD                      PIC X(1024).
070622 01  SVR                         PIC X(32).
070622 01  USR                         PIC X(32).
070622 01  PASS                        PIC X(32).
070622 01  USR-PASS                    PIC X(64).
070622
070622 01  WS-SQL-DATA.
070622     05  WS-CYCLE-DATE           PIC X(10).
070622     05  WS-NEXT-BUS-DT          PIC X(10).
070622     05  WS-LOOKUPID             PIC X(4).
070622     05  WS-LOOKUPNAME           PIC X(4).
070622     05  WS-LOOKUP-VALUE         PIC X(100).
070622     05  WS-CARRIER              PIC X.
070622     05  WS-GROUP                PIC X(6).
070622     05  WS-STATE                PIC XX.
070622     05  WS-ACCOUNT              PIC X(10).
070622     05  WS-EFF-DT               PIC XX.
070622     05  WS-CERT-NO              PIC X(10).
070622     05  WS-CERT-NO-SUF          PIC X(01).
070622
070622 EXEC SQL
070622    END DECLARE SECTION
070622 END-EXEC
011514 01  P pointer.
011514 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
011514 01  KIXHOST             pic x(9) value Z"HOSTNAME".
011514 01  var-ptr pointer.
011514 01  env-var-len                 pic 9(4)  binary.
011514 01  rc                          pic 9(9)  binary.
011514
011514 01  WS-KIXHOST                  PIC X(10).
011514 01  WS-KIXSYS.
011514     05  WS-KIX-FIL1             PIC X(10).
011514     05  WS-KIX-APPS             PIC X(10).
011514     05  WS-KIX-ENV              PIC X(10).
011514     05  WS-KIX-MYENV            PIC X(10).
011514     05  WS-KIX-SYS              PIC X(10).

00033
00034                              COPY ELCSCTM.
00035
00036                              COPY ELCSCRTY.
00037
00038  01  FILLER          COMP-3.
00039      12  WS-BALLOON-RTRM         PIC S9(03)      VALUE ZERO.
00039      12  WS-ELAPSED-MONTHS       PIC S9(03)      VALUE ZERO.
00040      12  WS-TOT-AH-PREM          PIC S9(9)V99    VALUE ZERO.
00041      12  WS-TOT-LF-PREM          PIC S9(9)V99    VALUE ZERO.
00042      12  WS-TOT-AH-RFND          PIC S9(9)V99    VALUE ZERO.
00043      12  WS-TOT-LF-RFND          PIC S9(9)V99    VALUE ZERO.
00044      12  WS-TOT-PREM             PIC S9(9)V99    VALUE ZERO.
00045      12  WS-TOT-RFND             PIC S9(9)V99    VALUE ZERO.
00046      12  WS-TOT-ITDR             PIC S9(9)V99    VALUE ZERO.
00047
00048  01  WS-DATE-AREA.
00049      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.
00050      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.
00051      12  WS-LF-CANCEL-DATE       PIC  X(02)      VALUE SPACES.
00052      12  WS-AH-CANCEL-DATE       PIC  X(02)      VALUE SPACES.
00053      12  WS-LF-CANCEL-DATE-ED    PIC  X(08)      VALUE SPACES.
00054      12  WS-AH-CANCEL-DATE-ED    PIC  X(08)      VALUE SPACES.
00055
       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.

       01  ELMSTR-KEY.
           12  ELMSTR-COMP-CD          PIC X.
           12  ELMSTR-CERT-NO          PIC X(11).

121712 01  ELCRTT-KEY.
121712     12  ELCRTT-PRIMARY          PIC X(33).
121712     12  ELCRTT-REC-TYPE         PIC X(1).
121712
062712 01  ELCRTO-KEY.                                    
062712     05  ELCRTO-COMPANY-CD       PIC X.             
062712     05  ELCRTO-CARRIER          PIC X.             
062712     05  ELCRTO-GROUPING         PIC X(6).          
062712     05  ELCRTO-STATE            PIC XX.            
062712     05  ELCRTO-ACCOUNT          PIC X(10).         
062712     05  ELCRTO-CERT-EFF-DT      PIC XX.            
062712     05  ELCRTO-CERT-NO.                            
062712         10  ELCRTO-CERT-PRIME   PIC X(10).         
062712         10  ELCRTO-CERT-SFX     PIC X.             
062712     05  ELCRTO-RECORD-TYPE      PIC X.             
062712     05  ELCRTO-SEQ-NO           PIC 9(4)  BINARY.  
062712
00056  01  STANDARD-AREAS.
00057      12  SC-ITEM                 PIC S9(04) COMP VALUE +0001.
00058      12  SUB3                    PIC S9(04) COMP VALUE +0.
00059      12  SUB4                    PIC S9(04) COMP VALUE +0.
00060      12  GETMAIN-SPACE           PIC  X(01)      VALUE SPACE.
00061      12  MAP-NAME                PIC  X(08)      VALUE 'EL127H'.
00062      12  MAPSET-NAME             PIC  X(08)      VALUE 'EL1278S'.
00063      12  SCREEN-NUMBER           PIC  X(04)      VALUE '127H'.
00064      12  TRANS-ID                PIC  X(04)      VALUE 'EXXA'.
00065      12  THIS-PGM                PIC  X(08)      VALUE 'EL1278'.
00066      12  PGM-NAME                PIC  X(08).
00067      12  TIME-IN                 PIC S9(07).
00068      12  TIME-OUT-R  REDEFINES  TIME-IN.
00069          16  FILLER              PIC  X(01).
00070          16  TIME-OUT            PIC  9(02)V99.
00071          16  FILLER              PIC  X(02).
00072      12  XCTL-005                PIC  X(08)      VALUE 'EL005'.
00073      12  XCTL-010                PIC  X(08)      VALUE 'EL010'.
00074      12  XCTL-126                PIC  X(08)      VALUE 'EL126'.
00075      12  LINK-001                PIC  X(08)      VALUE 'EL001'.
00076      12  LINK-004                PIC  X(08)      VALUE 'EL004'.
00077      12  LINK-ELDATCV            PIC  X(08)      VALUE 'ELDATCV'.
00078      12  ELRTRM-ID               PIC  X(08)      VALUE 'ELRTRM'.
00079      12  ELRAMT-ID               PIC  X(08)      VALUE 'ELRAMT'.
00080      12  ELRFND-ID               PIC  X(08)      VALUE 'ELRFND'.
00081      12  ELCNTL-ID               PIC  X(08)      VALUE 'ELCNTL'.
00082      12  ERACCT-ID               PIC  X(08)      VALUE 'ERACCT'.
00083      12  ELCERT-ID               PIC  X(08)      VALUE 'ELCERT'.
081606     12  ERMAIL-ID               PIC  X(08)      VALUE 'ERMAIL'.
00084      12  ERFORM-ID               PIC  X(08)      VALUE 'ERFORM'.
00085      12  ERNOTE-ID               PIC  X(08)      VALUE 'ERCNOT'.
121712     12  ELCRTT-ID               PIC  X(08)      VALUE 'ELCRTT'.
00086      12  DATE-RANGE-SW           PIC  X(01)      VALUE SPACE.
00087      12  WS-REFUND-SEARCH-SW     PIC  X(01)      VALUE ' '.
00088          88  LIFE-REFUND-SEARCH                  VALUE 'L'.
00089      12  WS-BROWSE-STARTED-SW    PIC  X(01)      VALUE SPACE.
00090          88  BROWSE-STARTED                      VALUE 'Y'.
00091      12  WS-FORM-RECORD-SW       PIC  X(01)      VALUE SPACE.
00092          88  FORM-FOUND                          VALUE 'Y'.
00093      12  BEN-SEARCH-SW           PIC  X(01)      VALUE SPACE.
00094          88  NO-BENEFIT-FOUND                    VALUE 'N'.
00095      12  WS-CF-LF-COVERAGE-TYPE  PIC  X(01)      VALUE SPACE.
00096          88  WS-REDUCING                         VALUE 'R'.
00097          88  WS-LEVEL                            VALUE 'L'  'P'.
00098      12  WS-NEW-LF-CANCEL-SW     PIC  X(01)      VALUE 'N'.
00099          88  WS-NEW-LF-CANCEL                    VALUE 'Y'.
00100      12  WS-NEW-AH-CANCEL-SW     PIC  X(01)      VALUE 'N'.
00101          88  WS-NEW-AH-CANCEL                    VALUE 'Y'.
00102      12  WS-BENEFIT-DESCRIP      PIC  X(10)      VALUE SPACES.
00103      12  WS-PMT-DESCRIP          PIC  X(10)      VALUE
00104          'CLAIM PMTS'.
00105      12  WS-REFUND-DESCRIP       PIC  X(10)      VALUE
00106          'ITD REFUND'.
00107      12  WS-ACCESS.
00108          16  FILLER              PIC  X(02).
00109          16  WS-BEN-CD           PIC  X(02).
00110      12  WS-KIND                 PIC  X(03)      VALUE SPACES.
           12  WS-LF-KIND              PIC  X(03)      VALUE SPACES.
           12  WS-AH-KIND              PIC  X(03)      VALUE SPACES.
00111      12  WS-LOOKUP-TYPE          PIC  X(01).
00112      12  WS-BENEFIT-NO           PIC  X(02)      VALUE ZERO.
00113      12  WS-STATE-ABBREVIATION   PIC  X(02)      VALUE SPACES.
00114      12  WS-CALC-CD              PIC  X(01).
00115      12  DEEDIT-FIELD            PIC  X(15).
00116      12  DEEDIT-FIELD-V0  REDEFINES
00117          DEEDIT-FIELD            PIC S9(15).
00118      12  WS-CURRENT-DATE         PIC  X(02)  VALUE LOW-VALUES.
00119      12  RETURN-FROM             PIC  X(08).
00120      12  QID.
00121          16  QID-TERM            PIC  X(04).
00122          16  FILLER              PIC  X(04)      VALUE '127H'.
00123      12  WS-FIRST-ENTRY-SW       PIC  X(01)      VALUE SPACE.
00124          88  FIRST-ENTRY                         VALUE 'Y'.
           12  WS-PDEF-RECORD-SW       PIC X           VALUE ' '.
               88  PDEF-FOUND                          VALUE 'Y'.
00125      12  WS-CERT-RECORD-SW       PIC  X(01)      VALUE SPACE.
00126          88  CERT-FOUND                          VALUE 'Y'.
00127          88  CERT-NOT-FOUND                      VALUE 'N'.
00128      12  WS-CNTL-RECORD-SW       PIC  X(01)      VALUE SPACE.
00129          88  CNTL-FOUND                          VALUE 'Y'.
00130          88  CNTL-NOT-FOUND                      VALUE 'N'.
00131      12  WS-BEN-RECORD-SW        PIC  X(01)      VALUE SPACE.
00132          88  BEN-FOUND                           VALUE 'Y'.
00133      12  WS-STATE-RECORD-SW      PIC  X(01)      VALUE SPACE.
00134          88  STATE-FOUND                         VALUE 'Y'.
00135      12  WS-ST-BEN-RECORD-SW     PIC  X(01)      VALUE SPACE.
00136          88  ST-BEN-FOUND                        VALUE 'Y'.
00137      12  WS-ACCT-RECORD-SW       PIC  X(01)      VALUE SPACE.
00138          88  ACCT-FOUND                          VALUE 'Y'.
00139      12  WS-DUPREC-SW            PIC  X(01)      VALUE SPACE.
00140          88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
00141      12  WS-NOT-FOUND            PIC S9(01) COMP-3
00142                                                  VALUE ZERO.
00143          88  BENEFIT-FOUND                       VALUE +1.
00144      12  WS-CALC-REFUND          PIC S9(07)V99 COMP-3
00145                                                  VALUE ZERO.
00146      12  WS-LIFE-PREMIUM         PIC S9(07)V99 COMP-3
00147                                                  VALUE ZERO.
00148      12  WS-INDEX                PIC S9(04) COMP VALUE ZERO.
00149      12  WS-SUB1                 PIC S99    COMP.
00150      12  WS-ACCT-USER-FLD-5      PIC  X(02).
00151      12  WS-CF-DEFAULT-APR       PIC  S9(03)V9(04) COMP-3.
00152      12  WS-CF-CR-R78-METHOD     PIC  X(01)      VALUE SPACE.
00153      12  WS-CF-CR-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00154      12  WS-AH-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
           12  WS-AH-BEN-CATEGORY      PIC  X          VALUE SPACE.
00155      12  WS-LF-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
00156      12  WS-AH-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00157      12  WS-LF-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00158      12  WS-AH-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
00159      12  WS-LF-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
00160      12  WS-AH-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00161      12  WS-LF-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00162      12  WS-AH-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00163      12  WS-LF-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00164      12  WS-AH-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00165      12  WS-LF-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00166      12  WS-AH-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00167      12  WS-LF-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00168      12  WS-AH-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00169      12  WS-LF-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00170      12  WS-AM-EARN-METHOD-A     PIC  X(01)      VALUE SPACE.
00171      12  WS-AM-EARN-METHOD-L     PIC  X(01)      VALUE SPACE.
00172      12  WS-AM-EARN-METHOD-R     PIC  X(01)      VALUE SPACE.
00173      12  ERROR-MESSAGES.
00174          16  ER-0000             PIC  X(04)      VALUE '0000'.
00175          16  ER-0004             PIC  X(04)      VALUE '0004'.
00176          16  ER-0008             PIC  X(04)      VALUE '0008'.
00177          16  ER-0028             PIC  X(04)      VALUE '0028'.
00178          16  ER-0029             PIC  X(04)      VALUE '0029'.
00179          16  ER-0070             PIC  X(04)      VALUE '0070'.
00180          16  ER-0142             PIC  X(04)      VALUE '0142'.
00180          16  ER-0214             PIC  X(04)      VALUE '0214'.
00181          16  ER-0681             PIC  X(04)      VALUE '0681'.
00182          16  ER-0682             PIC  X(04)      VALUE '0682'.
00183          16  ER-0683             PIC  X(04)      VALUE '0683'.
00184          16  ER-0684             PIC  X(04)      VALUE '0684'.
070622         16  ER-1590             PIC  X(04)      VALUE '1590'.
00185          16  ER-2227             PIC  X(04)      VALUE '2227'.
00186          16  ER-2601             PIC  X(04)      VALUE '2601'.
00187          16  ER-2616             PIC  X(04)      VALUE '2616'.
00188          16  ER-2617             PIC  X(04)      VALUE '2617'.
00189          16  ER-2619             PIC  X(04)      VALUE '2619'.
00190          16  ER-2740             PIC  X(04)      VALUE '2740'.
               16  ER-2756             PIC  X(04)      VALUE '2756'.
               16  ER-2768             PIC  X(04)      VALUE '2768'.
               16  ER-2774             PIC  X(04)      VALUE '2774'.
               16  ER-2775             PIC  X(04)      VALUE '2775'.
               16  ER-2965             PIC  X(04)      VALUE '2965'.
               16  ER-2966             PIC  X(04)      VALUE '2966'.
               16  ER-2967             PIC  X(04)      VALUE '2967'.
               16  ER-2968             PIC  X(04)      VALUE '2968'.
               16  ER-2969             PIC  X(04)      VALUE '2969'.
               16  ER-2998             PIC  X(04)      VALUE '2998'.
               16  ER-3037             PIC  X(04)      VALUE '3037'.
               16  ER-3038             PIC  X(04)      VALUE '3038'.
               16  ER-3039             PIC  X(04)      VALUE '3039'.
00191          16  ER-3780             PIC  X(04)      VALUE '3780'.
00192          16  ER-3781             PIC  X(04)      VALUE '3781'.
062712         16  ER-3830             PIC  X(04)      VALUE '3830'.
072616         16  er-3847             pic  x(04)      value '3847'.
00193          16  ER-8160             PIC  X(04)      VALUE '8160'.
00194          16  ER-8161             PIC  X(04)      VALUE '8161'.
               16  ER-9999             PIC  X(04)      VALUE '9999'.
00195      12  WS-CF-LIFE-OVERRIDE-L1  PIC  X(01).
00196      12  WS-CF-LIFE-OVERRIDE-L2  PIC  X(02).
00197      12  WS-CF-AH-OVERRIDE-L1    PIC  X(01).
00198      12  WS-CF-AH-OVERRIDE-L2    PIC  X(02).
00199
00200      12  WS-RESPONSE             PIC S9(8)       COMP.
00201          88  WS-RESP-NORMAL                      VALUE +00.
00202          88  WS-RESP-NOTFND                      VALUE +13.
               88  WS-RESP-DUPREC                      VALUE +14.
               88  WS-RESP-DUPKEY                      VALUE +15.
00203          88  WS-RESP-NOTOPEN                     VALUE +19.
               88  WS-RESP-ENDFILE                     VALUE +20.
010412
010412     12  WS-01-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL DATE ERROR'.
010412     12  WS-02-CANCEL-ERR        PIC X(25)
010412         VALUE 'CERT NOT FOUND'.
010412     12  WS-04-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL AMOUNT ERROR'.
010412     12  WS-05-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL OPTION ERROR'.
010412     12  WS-06-CANCEL-ERR        PIC X(25)
010412         VALUE 'PREVIOUSLY CANCELLED'.
010412     12  WS-07-CANCEL-ERR        PIC X(25)
010412         VALUE 'INVALID DATA'.
010412     12  WS-08-CANCEL-ERR        PIC X(25)
010412         VALUE 'NO ACCOUNT MASTER'.
010412     12  WS-09-CANCEL-ERR        PIC X(25)
010412         VALUE 'SUFFIX ALREADY EXISTS'.
010412     12  WS-99-CANCEL-ERR        PIC X(25)
010412         VALUE 'MISC CANCEL ERROR'.
00204
00205  01  WS-SAVE-ALLOWABLE-BENEFIT.
00206      16  WS-SAVE-BENEFIT-CODE      PIC XX.
00207      16  WS-SAVE-BENEFIT-TYPE      PIC X.
00208      16  WS-SAVE-BENEFIT-REVISION  PIC XXX.
00209      16  WS-SAVE-BENEFIT-REM-TERM  PIC X.
00210      16  WS-SAVE-BENEFIT-RETRO-Y-N PIC X.
00211      16  FILLER                    PIC XX.

       01  CTBL-KEY-SAVE               PIC X(5).
       01  CTBL-KEY.
           05  CTBL-COMPANY-CD         PIC X.
           05  CTBL-TABLE              PIC XXX.
           05  CTBL-BEN-TYPE           PIC X.
           05  CTBL-BEN-CODE           PIC XX.

       01  CANCEL-GEN-PASS-AREA.
           05  CG-OPTION-CODE          PIC X.
               88  CG-VALID-OPTION       VALUE '1' '2' '3'.
               88  CG-FLAT-CANCEL        VALUE '1'.
               88  CG-CANCEL             VALUE '2'.
               88  CG-CANCEL-REISSUE     VALUE '3'.
           05  CG-ERROR-CODE           PIC 99.
               88  CG-SUCCESS            VALUE 00.
               88  CG-DATE-ERROR         VALUE 01.
               88  CG-CERT-NOT-FOUND     VALUE 02.
               88  CG-AMOUNT-ERROR       VALUE 04.
               88  CG-OPTION-ERROR       VALUE 05.
               88  CG-PREV-CAN           VALUE 06.
               88  CG-INVALID-DATA       VALUE 07.
               88  CG-NO-ACCT-MSTR       VALUE 08.
               88  CG-SFX-A-EXIST        VALUE 09.
               88  CG-MISC-ERROR         VALUE 99.
           05  CG-COMPANY-ID           PIC XXX.
           05  CG-PROC-ID              PIC XXXX.
           05  CG-CURRENT-DT           PIC XX.
           05  CG-MONTH-END-DT         PIC XX.
           05  CG-CERT-KEY.
               10  CG-CERT-COMPANY-CD  PIC X.
               10  CG-CERT-CARRIER     PIC X.
               10  CG-CERT-GROUP       PIC X(6).
               10  CG-CERT-STATE       PIC XX.
               10  CG-CERT-ACCOUNT     PIC X(10).
               10  CG-CERT-EFF-DT      PIC XX.
               10  CG-CERT-CERT-NO     PIC X(11).
           05  CG-LF-CAN-DATA.
               10  CG-LF-CAN-DT        PIC XX.
               10  CG-LF-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-AH-CAN-DATA.
               10  CG-AH-CAN-DT        PIC XX.
               10  CG-AH-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-CERT-PROFILE-DATA.
               10  CG-INS-LNAME        PIC X(15).
               10  CG-INS-FNAME        PIC X(10).
               10  CG-INS-MID-INIT     PIC X.
               10  CG-INS-AGE          PIC 99.
               10  CG-JNT-LNAME        PIC X(15).
               10  CG-JNT-FNAME        PIC X(10).
               10  CG-JNT-MID-INIT     PIC X.
               10  CG-JNT-AGE          PIC 99.
           05  CG-LF-ISS-DATA.
               10  CG-LF-BENCD         PIC XX.
               10  CG-LF-PREM-AMT      PIC S9(7)V99 COMP-3.
072312         10  CG-LF-ALT-PREM-AMT  PIC S9(7)V99 COMP-3.
           05  CG-AH-ISS-DATA.
               10  CG-AH-BENCD         PIC XX.
               10  CG-AH-PREM-AMT      PIC S9(7)V99 COMP-3.
           05  CG-BATCH-NO             PIC X(6).
010412     05  CG-BATCH-SEQ-NO         PIC 9(4)  COMP.
072312     05  CG-DCC-REASON-CD        PIC X.
122712     05  CG-COMM-PCT-ZERO        PIC X.
100917     05  cg-vin                  pic x(17).
101918     05  cg-from-where           pic x(6).
100917     05  FILLER                  PIC X(281).


00213  01  WS-CAN-QUOTE-NOTE.
041514     12  WS-CN-CANCEL-DT         PIC  X(09).
00215      12  FILLER                  PIC  X(10)      VALUE
00216          'RF QUOTE: '.
00217      12  WS-CN-LIFE-AMT-RF       PIC  Z,ZZ9.99.
00218      12  FILLER                  PIC  X(4)       VALUE
00219          ' LF '.
00220      12  WS-CN-LF-REF-METH       PIC  X.
00221      12  FILLER                  PIC  X          VALUE ','.
00222      12  WS-CN-AH-AMT-RF         PIC  Z,ZZ9.99.
00223      12  FILLER                  PIC  X(5)       VALUE
00224          ' AH '.
00225      12  WS-CN-AH-REF-METH       PIC  X.
00226      12  FILLER                  PIC  XX         VALUE ','.
00227      12  FILLER                  PIC  X(6)       VALUE
00228          'TOTAL '.
00229      12  WS-CN-TOTAL-RF          PIC  Z,ZZ9.99.
00230      12  FILLER                  PIC  X(01)      VALUE ' '.
041514     12  WS-CN-DT-QUOTED         PIC  X(08).
00232      12  FILLER                  PIC  X(01)      VALUE ';'.
00233      12  WS-CN-PROCESSOR-ID      PIC  X(04).
00234
00235  01  WS-CM-CONTROL-PRIMARY.
00236      12  WS-CM-COMPANY-CD        PIC  X(01).
00237      12  WS-CM-CARRIER           PIC  X(01).
00238      12  WS-CM-GROUPING          PIC  X(06).
00239      12  WS-CM-STATE             PIC  X(02).
00240      12  WS-CM-ACCOUNT           PIC  X(10).
00241      12  WS-CM-CERT-EFF-DT       PIC  X(02).
00242      12  WS-CM-CERT-NO.
00243          16  WS-CM-CERT-PRIME    PIC  X(10).
00244          16  WS-CM-CERT-SFX      PIC  X(01).
00245
081606 01  WS-MA-CONTROL-PRIMARY.
081606     12  WS-MA-COMPANY-CD        PIC  X(01).
081606     12  WS-MA-CARRIER           PIC  X(01).
081606     12  WS-MA-GROUPING          PIC  X(06).
081606     12  WS-MA-STATE             PIC  X(02).
081606     12  WS-MA-ACCOUNT           PIC  X(10).
081606     12  WS-MA-CERT-EFF-DT       PIC  X(02).
081606     12  WS-MA-CERT-NO.
081606         16  WS-MA-CERT-PRIME    PIC  X(10).
081606         16  WS-MA-CERT-SFX      PIC  X(01).

00246  01  WS-CF-CONTROL-PRIMARY.
00247      12  WS-CF-COMPANY-ID        PIC  X(03)      VALUE SPACES.
00248      12  WS-CF-RECORD-TYPE       PIC  X(01)      VALUE ZERO.
00249 *        88  COMPANY-MASTER                      VALUE '1'.
00250 *        88  STATE-MASTER                        VALUE '3'.
00251 *        88  LF-BENEFIT-MASTER                   VALUE '4'.
00252 *        88  AH-BENEFIT-MASTER                   VALUE '5'.
00253      12  WS-CF-ACCESS.
00254          16  WS-CF-STATE         PIC  X(02)      VALUE SPACES.
00255          16  WS-CF-BENEFIT-NO                    VALUE SPACES.
00256              20  FILLER          PIC  X(01).
00257              20  WS-CF-CARRIER   PIC  X(01).
00258      12  WS-CF-SEQUENCE-NO       PIC S9(04) COMP VALUE ZERO.
00259
00260  01  WS-AM-CONTROL-PRIMARY.
00261      12  WS-AM-COMPANY-CD        PIC  X(01).
00262      12  WS-AM-CARRIER           PIC  X(01).
00263      12  WS-AM-GROUPING          PIC  X(06).
00264      12  WS-AM-STATE             PIC  X(02).
00265      12  WS-AM-ACCOUNT           PIC  X(10).
00266      12  WS-AM-EXPIRATION-DT     PIC  X(02).
00267      12  WS-AM-FILLER            PIC  X(04).
00268
00269  01  WS-CN-CONTROL-PRIMARY.
00270      12  WS-CN-COMPANY-CD        PIC  X(01).
00271      12  WS-CN-CARRIER           PIC  X(01).
00272      12  WS-CN-GROUPING          PIC  X(06).
00273      12  WS-CN-STATE             PIC  X(02).
00274      12  WS-CN-ACCOUNT           PIC  X(10).
00275      12  WS-CN-CERT-EFF-DT       PIC  X(02).
00276      12  WS-CN-CERT-NO.
00277          16  WS-CN-CERT-PRIME    PIC  X(10).
00278          16  WS-CN-CERT-SFX      PIC  X.
           12  WS-CN-REC-TYPE          PIC X.
           12  WS-CN-SEQ-NO            PIC 9(4) BINARY.
00279
00280  01  WS-FO-CONTROL-PRIMARY.
00281      12  WS-FO-COMPANY-CD        PIC  X(01).
00282      12  WS-FO-STATE             PIC  X(02).
00283      12  WS-FO-FORM-ID           PIC  X(12).
00284      12  WS-FO-EXP-DT            PIC  X(02).
00285  EJECT


                                   COPY ERCCNOT.

00286                              COPY ELCDATE.
00287  EJECT
00288                              COPY ELCLOGOF.
00289  EJECT
00290                              COPY ELCATTR.
00291  EJECT
00292                              COPY ELCEMIB.
00293  EJECT
010412 01  WS-PASS-631.
010412     12  WS-PASS-WORK-AREA         PIC X(384).
010412     12  WS-PASS-PROGRAM-WORK-AREA PIC X(640).
010412     12  FILLER REDEFINES WS-PASS-PROGRAM-WORK-AREA.
010412         COPY ELC631PI.
010412         16  FILLER                PIC X(94).
010412
010412
00294                              COPY ELCINTF.
00295      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00296          16  FILLER                PIC  X(500).
00297          16  PI-PEND-SW            PIC  X(01).
00298          16  PI-LF-CANCEL-DATE     PIC  X(02).
00299          16  PI-AH-CANCEL-DATE     PIC  X(02).
00300          16  PI-LF-CANCEL-DATE-ED  PIC  X(08).
00301          16  PI-AH-CANCEL-DATE-ED  PIC  X(08).
00302          16  PI-EARNING-METHOD-LF  PIC  X.
00303          16  PI-EARNING-METHOD-AH  PIC  X.
00304          16  PI-LF-REFUND-AMT      PIC  S9(7)V99 COMP-3.
00305          16  PI-LF-REFUND-METH     PIC  X.
00306          16  PI-AH-REFUND-AMT      PIC  S9(7)V99 COMP-3.
00307          16  PI-AH-REFUND-METH     PIC  X.
00308          16  PI-TOTAL-REFUND-AMT   PIC  S9(7)V99 COMP-3.
00309          16  PI-MODIFY-CERT-CAP    PIC  X.
00310              88 MODIFY-CERT-CAP    VALUE 'Y'.
               16  PI-CLP-YN             PIC X.
               16  PI-CANCEL-REASON      PIC X.
072616         16  PI-PF10-OK            PIC X.
072616         16  FILLER                PIC  X(96).
00312  EJECT
00313                              COPY ELCAID.
00314
00315  01  FILLER  REDEFINES  DFHAID.
00316      12  FILLER                  PIC  X(08).
00317      12  PF-VALUES               PIC  X(01)      OCCURS 24 TIMES.
00318  EJECT
00319                              COPY EL1278S.
00320  EJECT
00321                              COPY ELCCALC.
00322  EJECT
                                   COPY ERCCTBL.
00324
00325  LINKAGE SECTION.
00326
00327  01  DFHCOMMAREA                 PIC  X(1024).
00328
00329 *01 PARMLIST .
00330 *    12  FILLER                  PIC S9(08)      COMP.
00331 *    12  ELCNTL-POINTER          PIC S9(08)      COMP.
00332 *    12  ELACCT-POINTER          PIC S9(08)      COMP.
00333 *    12  ELCERT-POINTER          PIC S9(08)      COMP.

00335                              COPY ELCCNTL.

00337                              COPY ERCACCT.

00339                              COPY ELCCERT.

081606                             COPY ERCMAIL.
062712                             COPY ELCCRTO.

00341                              COPY ERCFORM.

                                   COPY ELCMSTR.
                                   COPY ERCPDEF.
121712                             COPY ELCCRTT.
011514 01  var  pic x(30).

00343  PROCEDURE DIVISION.
00344 *    SERVICE RELOAD PARMLIST.
00345
00346      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00347      MOVE '5'                    TO  DC-OPTION-CODE.
00348
00349      PERFORM 9600-DATE-LINK.
00350
00351      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00352      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00353      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00354      MOVE 2                      TO  EMI-NUMBER-OF-LINES.

           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6

00355      MOVE EIBTRMID               TO  QID-TERM.
00356
00357      IF EIBCALEN  IS EQUAL TO  ZERO
00358          GO TO 8700-UNAUTHORIZED-ACCESS.
00359
00360      IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00361          IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00362              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00363              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00364              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00365              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00366              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00367              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00368              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00369              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00370          ELSE
00371              MOVE PI-CALLING-PROGRAM    TO  RETURN-FROM
00372              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00373              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00374              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00375              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00376              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00377              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00378              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00379              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00380
00381      IF EIBAID  IS EQUAL TO  DFHCLEAR
00382          GO TO 9300-CLEAR.
00383
00384      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'
00385          NEXT SENTENCE
00386      ELSE
00387          EXEC CICS READQ TS
00388              QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00389              INTO    (SECURITY-CONTROL)
00390              LENGTH  (SC-COMM-LENGTH)
00391              ITEM    (SC-ITEM)
00392          END-EXEC
00393          MOVE SC-CREDIT-DISPLAY (33)
00394                                  TO  PI-DISPLAY-CAP
00395          MOVE SC-CREDIT-UPDATE  (33)
00396                                  TO  PI-MODIFY-CAP
00397          MOVE SC-CREDIT-UPDATE  (32)
00398                                  TO  PI-MODIFY-CERT-CAP
00399          IF NOT  DISPLAY-CAP
00400              MOVE 'READ'         TO  SM-READ
00401              PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
00402              MOVE ER-0070        TO  EMI-ERROR
00403              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00404              GO TO 8100-SEND-INITIAL-MAP.
00405
00406      IF EIBTRNID  IS NOT EQUAL TO  TRANS-ID
00407          MOVE 'Y' TO WS-FIRST-ENTRY-SW
00408          MOVE LOW-VALUES         TO  EL127HI
00409          MOVE SAVE-BIN-DATE      TO  WS-LF-CANCEL-DATE
00410                                      PI-LF-CANCEL-DATE
00411                                      WS-AH-CANCEL-DATE
00412                                      PI-AH-CANCEL-DATE
00413          MOVE SAVE-DATE          TO  WS-LF-CANCEL-DATE-ED
00414                                      PI-LF-CANCEL-DATE-ED
00415                                      WS-AH-CANCEL-DATE-ED
00416                                      PI-AH-CANCEL-DATE-ED
00417          MOVE SPACES             TO  PI-EARNING-METHOD-LF
00418                                      PI-EARNING-METHOD-AH
00419          PERFORM 0500-FORMAT-SCREEN
00420          GO TO 8100-SEND-INITIAL-MAP.
00421
00422      IF NOT  MODIFY-CAP
00423          MOVE 'UPDATE'           TO  SM-READ
00424          PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
00425          MOVE ER-0070            TO  EMI-ERROR
00426          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00427          GO TO 8100-SEND-INITIAL-MAP.
00428
00429      EXEC CICS HANDLE CONDITION
00430          PGMIDERR  (9500-PGMID-ERROR)
00431          ERROR     (9900-ABEND)
00432      END-EXEC.
070622
070622     set P to address of KIXSYS
070622     CALL "getenv" using by value P returning var-ptr
070622     if var-ptr = null then
070622        display ' kixsys not set '
070622     else
070622        set address of var to var-ptr
070622        move 0 to env-var-len
070622        inspect var tallying env-var-len
070622          for characters before X'00'
070622        unstring var (1:env-var-len) delimited by '/'
070622        into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
070622              WS-KIX-SYS
070622        end-unstring
070622     end-if
070622
070622     set P to address of KIXHOST
070622     CALL "getenv" using by value P returning var-ptr
070622     if var-ptr = null then
070622        display ' kixhost not set '
070622     else
070622        set address of var to var-ptr
070622        move 0 to env-var-len
070622        inspect var tallying env-var-len
070622          for characters before X'00'
070622        MOVE var(1:env-var-len)  to ws-kixhost
070622        DISPLAY ' WS KIX HOST ' WS-KIXHOST
070622     end-if.

00433  EJECT
00434  0200-RECEIVE.
00435      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00436          MOVE ER-0008            TO  EMI-ERROR
00437          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00438          MOVE -1                 TO  HLCANCL
00439          GO TO 8200-SEND-DATAONLY.
00440
00441      EXEC CICS RECEIVE
00442          MAP     (MAP-NAME)
00443          MAPSET  (MAPSET-NAME)
00444          INTO    (EL127HI)
00445      END-EXEC.
00446
00447      IF HPFKEYL  IS EQUAL TO  ZERO
00448          GO TO 0300-CHECK-PFKEYS.
00449
00450      IF EIBAID  IS NOT EQUAL TO  DFHENTER
00451          MOVE ER-0004            TO  EMI-ERROR
00452          GO TO 0320-INPUT-ERROR.
00453
00454      IF HPFKEYI NUMERIC
00455          IF HPFKEYI  IS EQUAL TO  '12'  OR  '23'  OR  '24' OR '11'
00456              MOVE PF-VALUES (HPFKEYI)
00457                                  TO  EIBAID
00458          ELSE
00459              MOVE ER-0029        TO  EMI-ERROR
00460              GO TO 0320-INPUT-ERROR.
00461
00462  0300-CHECK-PFKEYS.
00463      IF EIBAID  IS EQUAL TO  DFHPF23
00464          GO TO 8800-PF23.
00465
00466      IF EIBAID  IS EQUAL TO  DFHPF24
00467          GO TO 9100-RETURN-MAIN-MENU.

072616     if eibaid not = dfhpf10
072616        move 'N'                 to pi-pf10-ok
072616     end-if

00469      IF EIBAID = DFHPF11
00471         GO TO 1700-WRITE-CERT-NOTE
           END-IF
00472
00473      IF EIBAID  IS EQUAL TO  DFHPF12
00474          GO TO 9400-PF12.
00475
00476      IF EIBAID = DFHENTER OR DFHPF10
00477         GO TO 0400-EDIT-INPUT-DATA
           END-IF
00478
00479      MOVE ER-0029                TO  EMI-ERROR.
00480
00481  0320-INPUT-ERROR.
00482      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
00483
00484      MOVE AL-UNBON               TO  HPFKEYA.
00485
00486      IF HPFKEYL  IS EQUAL TO  ZERO
00487          MOVE -1                 TO  HLCANCL
00488      ELSE
00489          MOVE -1                 TO  HPFKEYL.
00490
00491      GO TO 8200-SEND-DATAONLY.
00492  EJECT
00493  0400-EDIT-INPUT-DATA.
00494      MOVE 'N'                    TO  WS-NEW-AH-CANCEL-SW
00495                                      WS-NEW-LF-CANCEL-SW.
00496

081606     IF (HPCYNL > 0)
081606        AND (HPCYNI = 'Y' OR 'N')
081606        PERFORM 1800-READ-ELCERT-UPDATE
081606                                 THRU 1800-EXIT
              IF WS-RESP-NORMAL
081606           MOVE HPCYNI           TO CM-POST-CARD-IND
081606           PERFORM 1810-REWRITE-ELCERT
081606                                 THRU 1810-EXIT
              END-IF
081606     END-IF

00497      IF HLCAL1L GREATER ZERO
00498          MOVE HLCAL1I              TO  PI-EARNING-METHOD-LF.
           IF CLPYNI = 'Y'
              MOVE 'Y'                   TO PI-CLP-YN
              MOVE AL-UANON              TO CLPYNA
           ELSE
              MOVE ' '                   TO PI-CLP-YN
              MOVE AL-UANON              TO CLPYNA
           END-IF
           IF CANREAI = 'R'
              MOVE 'R'                 TO PI-CANCEL-REASON
              MOVE AL-UANON            TO CANREAA
           ELSE
070622        IF CANREAI > SPACES
070622           MOVE CANREAI             TO PI-CANCEL-REASON
                 MOVE AL-UANON            TO CANREAA
              END-IF
           END-IF

070622     PERFORM 0450-CHECK-CANCEL-REASON THRU 0450-EXIT.
00499
00500      IF HLCANCL  IS EQUAL TO  ZERO
00501        AND HACANCL  IS EQUAL TO  ZERO
00502          MOVE PI-LF-CANCEL-DATE    TO  WS-LF-CANCEL-DATE
00503          MOVE PI-AH-CANCEL-DATE    TO  WS-AH-CANCEL-DATE
00504          MOVE PI-LF-CANCEL-DATE-ED TO  WS-LF-CANCEL-DATE-ED
00505          MOVE PI-AH-CANCEL-DATE-ED TO  WS-AH-CANCEL-DATE-ED
00506          GO TO 0420-CHECK-ERRORS.
00507
00508      IF HLCANCL  IS EQUAL TO  ZERO
00509          MOVE PI-LF-CANCEL-DATE    TO  WS-LF-CANCEL-DATE
00510          MOVE PI-LF-CANCEL-DATE-ED TO  WS-LF-CANCEL-DATE-ED
00511          GO TO 0410-CHECK-AH-DATE.
00512
00508      IF HLCANCI = SPACES
              MOVE LOW-VALUES            TO PI-LF-CANCEL-DATE
                                            WS-LF-CANCEL-DATE
              MOVE SPACES                TO PI-LF-CANCEL-DATE-ED
                                            WS-LF-CANCEL-DATE-ED
                                            PI-LF-REFUND-METH
              MOVE +0                    TO PI-LF-REFUND-AMT
00511         GO TO 0410-CHECK-AH-DATE
           END-IF
00512
00513      MOVE AL-UNNON               TO  HLCANCA.
00514      MOVE HLCANCI                TO  DEEDIT-FIELD.
00515      MOVE 'Y'                    TO  WS-NEW-LF-CANCEL-SW.
00516
00517      PERFORM 8600-DEEDIT.
00518
00519      MOVE DEEDIT-FIELD           TO  DC-GREG-DATE-1-MDY.
00520      MOVE '4'                    TO  DC-OPTION-CODE.
00521
00522      PERFORM 9600-DATE-LINK.
00523
00524      IF DATE-CONVERSION-ERROR
00525          MOVE -1                 TO  HLCANCL
00526          MOVE AL-UABON           TO  HLCANCA
00527          MOVE ER-2227            TO  EMI-ERROR
00528          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00529      ELSE
              IF PI-CERT-EFF-DT > DC-BIN-DATE-1
                 MOVE -1               TO  HACANCL
                 MOVE AL-UABON         TO  HACANCA
                 MOVE ER-2774          TO  EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              ELSE
00530            MOVE DC-BIN-DATE-1    TO WS-LF-CANCEL-DATE
00531                                     PI-LF-CANCEL-DATE
00532            MOVE DC-GREG-DATE-1-EDIT
00533                                  TO HLCANCO
00534                                     WS-LF-CANCEL-DATE-ED
00535                                     PI-LF-CANCEL-DATE-ED
              END-IF
           END-IF

           .
00537  0410-CHECK-AH-DATE.
00538      IF HACAL1L GREATER ZERO
00539          MOVE HACAL1I              TO  PI-EARNING-METHOD-AH.
00540
00541      IF HACANCL  IS EQUAL TO  ZERO
00542          MOVE PI-AH-CANCEL-DATE    TO  WS-AH-CANCEL-DATE
00543          MOVE PI-AH-CANCEL-DATE-ED TO  WS-AH-CANCEL-DATE-ED
00544          GO TO 0420-CHECK-ERRORS.

00541      IF HACANCI = SPACES
00542         MOVE LOW-VALUES            TO PI-AH-CANCEL-DATE
                                            WS-AH-CANCEL-DATE
00543         MOVE SPACES                TO PI-AH-CANCEL-DATE-ED
                                            WS-AH-CANCEL-DATE-ED
                                            PI-AH-REFUND-METH
              MOVE +0                    TO PI-AH-REFUND-AMT
00544         GO TO 0420-CHECK-ERRORS
           END-IF

00545
00546      MOVE AL-UNNON               TO  HACANCA.
00547      MOVE HACANCI                TO  DEEDIT-FIELD.
00548      MOVE 'Y'                    TO  WS-NEW-AH-CANCEL-SW.
00549
00550      PERFORM 8600-DEEDIT.
00551
00552      MOVE DEEDIT-FIELD           TO  DC-GREG-DATE-1-MDY.
00553      MOVE '4'                    TO  DC-OPTION-CODE.
00554
00555      PERFORM 9600-DATE-LINK.
00556
00557      IF DATE-CONVERSION-ERROR
00558          MOVE -1                 TO  HACANCL
00559          MOVE AL-UABON           TO  HACANCA
00560          MOVE ER-2227            TO  EMI-ERROR
00561          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00562      ELSE
              IF PI-CERT-EFF-DT > DC-BIN-DATE-1
                 MOVE -1               TO  HACANCL
                 MOVE AL-UABON         TO  HACANCA
                 MOVE ER-2775          TO  EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              ELSE
00563            MOVE DC-BIN-DATE-1    TO WS-AH-CANCEL-DATE
00564                                     PI-AH-CANCEL-DATE
00565            MOVE DC-GREG-DATE-1-EDIT
00566                                  TO HACANCO
00567                                     WS-AH-CANCEL-DATE-ED
00568                                     PI-AH-CANCEL-DATE-ED
              END-IF
           END-IF

           .
00570  0420-CHECK-ERRORS.

00571      IF EMI-ERROR  IS EQUAL TO  ZEROS
00572          NEXT SENTENCE
00573      ELSE
00574          GO TO 8200-SEND-DATAONLY.
00575


00576      PERFORM 0500-FORMAT-SCREEN.

072616     if eibaid = dfhpf10
072616        if pi-pf10-ok not = 'Y'
072616           move 'Y'              to pi-pf10-ok
072616           move er-3847          to emi-error
072616           move -1               to hlcancl
072616           perform 9700-error-format
072616                                 thru 9799-exit
072616           go to 8200-send-dataonly
072616        end-if
072616     end-if

           IF EIBAID = DFHPF10
              perform 4000-GEN-CANCEL-TRANS
                                       thru 4000-exit
010412        MOVE PROGRAM-INTERFACE-BLOCK TO WS-PASS-631
010412        MOVE LOW-VALUES     TO WS-PASS-PROGRAM-WORK-AREA
010412        MOVE PI-COMPANY-CD  TO PI-PB-COMPANY-CD
010412        MOVE CG-BATCH-NO    TO PI-PB-ENTRY-BATCH
010412        MOVE 1              TO PI-PB-BATCH-SEQ-NO
010412        MOVE 'Y'            TO PI-ALL-ISSUES-SW
010412                               PI-ALL-CANCELS-SW
062712        IF PI-PROCESSOR-IS-CSR
062712            MOVE 'Y'        TO PI-CSR-SESSION-SW
062712        ELSE
062712            MOVE ' '        TO PI-CSR-SESSION-SW
062712        END-IF
010412        MOVE 'N'            TO PI-ISSUES-IN-ERROR-SW
010412                               PI-CANCELS-IN-ERROR-SW
010412                               PI-ONLY-BATCH-HEADERS-SW
010412                               PI-ALL-OUT-OF-BAL-SW
010412                               PI-HOLD-REC-SW
010412                               PI-CHANGE-REC-SW
010412                               PI-CHK-REQ-REC-SW
010412                               PI-ISSUE-WARNING-SW
010412                               PI-CANCEL-WARNING-SW
010412        MOVE '1'            TO PI-BROWSE-TYPE
010412        MOVE DFHENTER       TO  EIBAID
070622        MOVE PI-CANCEL-REASON TO PI-CANCEL-REASON-631
010412
010412        EXEC CICS XCTL
010412            PROGRAM    ('EL6311')
010412            COMMAREA   (WS-PASS-631)
010412            LENGTH     (1300)
010412        END-EXEC
010412
010412*        PERFORM 0500-FORMAT-SCREEN
           END-IF

00578      GO TO 8100-SEND-INITIAL-MAP.
070622 0450-CHECK-CANCEL-REASON.
070622
070622*      Read Napersoft CancelReasons Lookup Table
070622     MOVE 'naperadmin'           TO USR
070622     MOVE 'cCm8naper'            TO PASS
070622     IF WS-KIXHOST = 'logictest'
070622        MOVE 'HOVTSTDB01_NaperRepo'
070622                                 TO SVR
070622        MOVE '1029'              TO WS-LOOKUPID
070622     ELSE
070622        MOVE 'SDVDB01_NaperRepo' TO SVR
070622        MOVE '1029'              TO WS-LOOKUPID
070622     END-IF
070622
070622     MOVE SPACES                 TO WS-LOOKUP-VALUE
070622
070622     PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
070622     MOVE PI-CANCEL-REASON TO WS-LOOKUPNAME
070622
070622     EXEC SQL
070622           SELECT LOOKUPVALUE
070622             INTO :WS-LOOKUP-VALUE
070622             FROM LOOKUPVALUES
070622               WHERE LOOKUPID = :WS-LOOKUPID
070622                 AND LOOKUPNAME = :WS-LOOKUPNAME
070622     END-EXEC
070622
070622     IF SQLCODE = 0
070622        CONTINUE
070622     ELSE
070622     IF SQLCODE = 100
070622         MOVE -1                    TO CANREAL
070622         MOVE AL-UANON              TO CANREAA
070622         MOVE ER-1590               TO EMI-ERROR
070622         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
070622     ELSE
070622        DISPLAY "ERROR: INVALID CancelReasons Lookup Table SELECT"
070622        DISPLAY ' SQL RETURN CODE ' SQLCODE
070622        DISPLAY ' SQL ERR MESS    ' SQLERRMC
070622     END-IF.
070622
070622     PERFORM 4300-DISCONNECT THRU 4300-EXIT.
070622
070622 0450-EXIT.
070622     EXIT.

00579  EJECT
00580  0500-FORMAT-SCREEN  SECTION.
00581      MOVE LOW-VALUES             TO  EL127HO.
00582      MOVE PI-CARRIER             TO  HCARIERO.
00583      MOVE PI-GROUPING            TO  HGROUPO.
00584      MOVE PI-STATE               TO  HSTATEO.
00585      MOVE PI-ACCOUNT             TO  HACCTNOO.
00586      MOVE ' '                    TO  DC-OPTION-CODE.
00587      MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00588
00589      PERFORM 9600-DATE-LINK.
00590
00591      MOVE DC-GREG-DATE-1-EDIT    TO  HEFFDTO.
00592      MOVE PI-CERT-PRIME          TO  HCERTNOO.
00593      MOVE PI-CERT-SFX            TO  HCRTSFXO.
           MOVE PI-CLP-YN              TO  CLPYNO

           .
00595  0510-GET-CERT.
00596      MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
00597      MOVE PI-CARRIER             TO  WS-CM-CARRIER.
00598      MOVE PI-GROUPING            TO  WS-CM-GROUPING.
00599      MOVE PI-STATE               TO  WS-CM-STATE.
00600      MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
00601      MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
00602      MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
00603      MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
00604
00605      PERFORM 0900-READ-CERT-FILE.
00606
00607      IF CERT-NOT-FOUND
00608          MOVE -1                 TO  HLCANCL
00609          MOVE AL-SANON           TO  HLCANCA
00610          MOVE ER-0214            TO  EMI-ERROR
00611          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00612          GO TO 8100-SEND-INITIAL-MAP.
00613
           IF CM-CLAIM-ATTACHED-COUNT = +0
              GO TO 0510-SKIP-CLAIM-CHECK
           END-IF

      *    DISPLAY ' STARTING CLAIM TEST '

           MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
           MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
           EXEC CICS STARTBR
              DATASET     ('ELMSTR5')
              RIDFLD      (ELMSTR-KEY)
              RESP        (WS-RESPONSE)
           END-EXEC

           IF WS-RESP-NORMAL

           PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP

              EXEC CICS READNEXT
                 DATASET   ('ELMSTR5')
                 RIDFLD    (ELMSTR-KEY)
                 SET       (ADDRESS OF CLAIM-MASTER)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF (WS-RESP-NORMAL OR WS-RESP-DUPKEY)
                 AND (CM-COMPANY-CD = CL-COMPANY-CD)
                 AND (CM-CERT-NO = CL-CERT-NO)
                 IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
                    AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
                    SET FOUND-A-CLAIM  TO TRUE
100518              MOVE CL-CLAIM-TYPE TO WS-CLAIM-TYPE
100518              IF CL-CLAIM-TYPE = 'L' OR 'O'
                       IF CL-INCURRED-DT > WS-LF-INCUR-DT
                          MOVE CL-INCURRED-DT TO WS-LF-INCUR-DT
                       END-IF
                       IF CLAIM-IS-OPEN
                          SET OPEN-LF-CLAIM TO TRUE
                       ELSE
                          IF CL-LAST-CLOSE-DT > WS-LF-LAST-CLOSE-DT
                             MOVE CL-LAST-CLOSE-DT
                                       TO WS-LF-LAST-CLOSE-DT
                          END-IF
                          SET CLOSED-LF-CLAIM TO TRUE
                       END-IF
                    ELSE
                       IF CLAIM-IS-OPEN
                          MOVE CL-PAID-THRU-DT
                                       TO WS-AH-PAID-THRU-DT
                          SET OPEN-AH-CLAIM TO TRUE
                       ELSE
                          IF CL-PAID-THRU-DT > WS-AH-PAID-THRU-DT
                             MOVE CL-PAID-THRU-DT
                                       TO WS-AH-PAID-THRU-DT
                          END-IF
                          IF CL-LAST-CLOSE-DT > WS-AH-LAST-CLOSE-DT
                             MOVE CL-LAST-CLOSE-DT
                                       TO WS-AH-LAST-CLOSE-DT
                          END-IF
                          SET CLOSED-AH-CLAIM TO TRUE
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 SET I-SAY-TO-STOP TO TRUE
              END-IF
           END-PERFORM

           END-IF

           IF FOUND-A-CLAIM
              IF WS-LF-INCUR-DT NOT = LOW-VALUES
                 MOVE WS-LF-INCUR-DT   TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 9600-DATE-LINK
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-1-EDIT TO LCLMDTO
                    MOVE AL-SANOF      TO LCLMDTHA
                 END-IF
              END-IF
100518        IF WS-CLAIM-TYPE = 'O'
100518           MOVE 'CLAIM TYPE O INCURRED' TO LCLMDTHO
100518           MOVE 'OTHER' TO WS-CLAIM-LIT
100518        ELSE
100518           MOVE 'LIFE ' TO WS-CLAIM-LIT
100518        END-IF

              IF WS-AH-PAID-THRU-DT NOT = LOW-VALUES
                 MOVE WS-AH-PAID-THRU-DT TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 9600-DATE-LINK
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-1-EDIT TO ACLMDTO
                    MOVE AL-SANOF      TO ACLMDTHA
                 END-IF
              END-IF
              MOVE PI-COMPANY-ID       TO WS-CF-COMPANY-ID
              MOVE '3'                 TO WS-CF-RECORD-TYPE
              MOVE CM-STATE            TO WS-CF-ACCESS
              MOVE +0                  TO WS-CF-SEQUENCE-NO
              
              EXEC CICS READ
                 DATASET  (ELCNTL-ID)
                 SET      (ADDRESS OF CONTROL-FILE)
                 RIDFLD   (WS-CF-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC
              
              IF WS-RESP-NORMAL
                 MOVE CF-ST-REF-AH-DEATH-IND
                                       TO WS-ST-REF-IND
              END-IF
           END-IF

           .
       0510-SKIP-CLAIM-CHECK.

00614      MOVE CM-INSURED-LAST-NAME   TO  HLNAMEO.
00615      MOVE CM-INSURED-FIRST-NAME  TO  HFNAMEO.
00616      MOVE CM-INSURED-INITIAL2    TO  HINITO.

081606     MOVE WS-CM-CONTROL-PRIMARY  TO WS-MA-CONTROL-PRIMARY

081606     EXEC CICS READ
081606         DATASET  (ERMAIL-ID)
081606         RIDFLD   (WS-MA-CONTROL-PRIMARY)
081606         SET      (ADDRESS OF MAILING-DATA)
081606         RESP     (WS-RESPONSE)
081606     END-EXEC.

081606     IF WS-RESP-NORMAL
081606        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
081606           (S1 > +7)
081606           OR (MA-MAIL-STATUS (S1) = '1')
081606        END-PERFORM
081606        IF S1 > +7
081606           MOVE AL-SANOF         TO HPCYNA
081606        END-IF
081606     END-IF

           IF CM-POST-CARD-IND = 'Y'
              MOVE 'Y'                 TO HPCYNO
           ELSE
              MOVE 'N'                 TO HPCYNO
           END-IF
081606     IF ('8' NOT = CM-LF-CURRENT-STATUS AND CM-AH-CURRENT-STATUS)
081606        OR (NOT CERT-AS-LOADED)
081606        MOVE AL-SANOF            TO HPCYNA
081606     END-IF

081606*    IF (CERT-ADDED-BATCH)
081606*       CONTINUE
      *    ELSE
      *       MOVE AL-SANOF            TO HPCYNA
      *    END-IF
00617      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00618      MOVE '5'                    TO  DC-OPTION-CODE.
00619
00620      PERFORM 9600-DATE-LINK.
00621
00622      MOVE DC-BIN-DATE-1          TO  DC-BIN-DATE-2.
00623      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00624      MOVE '1'                    TO  DC-OPTION-CODE.
00625
00626      PERFORM 9600-DATE-LINK.
00627
00628      MOVE DC-ELAPSED-MONTHS      TO  WS-ELAPSED-MONTHS.
00629
00630      MOVE ZEROS TO WS-TOT-ITDR.
00631      MOVE WS-REFUND-DESCRIP TO PMTHDO.
00632
00633  0510-CHECK-LF-STATUS.
00634
00635      IF (CM-LF-BENEFIT-CD IS EQUAL TO '00' OR '  ')
00636          MOVE LOW-VALUES         TO WS-LF-CANCEL-DATE
00637                                     PI-LF-CANCEL-DATE
00638          MOVE SPACES             TO WS-LF-CANCEL-DATE-ED
00639                                     PI-LF-CANCEL-DATE-ED
00640                                     PI-LF-REFUND-METH
00641          MOVE ZEROS              TO PI-LF-REFUND-AMT
00642          GO TO 0510-CHECK-AH-STATUS.
00643
00644      IF (CM-LF-CURRENT-STATUS  IS EQUAL TO  '8') AND
00645         (CM-LF-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES)
00646           MOVE CM-LF-ITD-CANCEL-AMT TO HLITDRO
00647           ADD  CM-LF-ITD-CANCEL-AMT TO WS-TOT-ITDR
00648           IF FIRST-ENTRY
00649               MOVE CM-LF-CANCEL-DT      TO DC-BIN-DATE-1
00650               MOVE SPACES               TO DC-OPTION-CODE
00651               PERFORM 9600-DATE-LINK
00652               IF NOT DATE-CONVERSION-ERROR
00653                    MOVE DC-GREG-DATE-1-EDIT TO HLCANCO
00654                                             PI-LF-CANCEL-DATE-ED
00655                                             WS-LF-CANCEL-DATE-ED
00656                    MOVE CM-LF-CANCEL-DT TO  PI-LF-CANCEL-DATE
00657                                             WS-LF-CANCEL-DATE.
00658
00659      IF (CM-LF-CURRENT-STATUS  IS EQUAL TO  '7')  AND
00660         (CM-LF-DEATH-DT  IS NOT EQUAL TO  LOW-VALUES)
00661           MOVE WS-PMT-DESCRIP       TO PMTHDO
00662           MOVE CM-LF-ITD-DEATH-AMT  TO HLITDRO
00663           ADD  CM-LF-ITD-DEATH-AMT TO WS-TOT-ITDR
00664         IF FIRST-ENTRY
00665             MOVE CM-LF-DEATH-DT       TO DC-BIN-DATE-1
00666             MOVE SPACES               TO DC-OPTION-CODE
00667             PERFORM 9600-DATE-LINK
00668             IF NOT DATE-CONVERSION-ERROR
00669                  MOVE DC-GREG-DATE-1-EDIT TO HLCANCO
00670                                           PI-LF-CANCEL-DATE-ED
00671                                           WS-LF-CANCEL-DATE-ED
00672                  MOVE CM-LF-DEATH-DT  TO  PI-LF-CANCEL-DATE
00673                                           WS-LF-CANCEL-DATE.
00674
00675  0510-CHECK-AH-STATUS.
00676
00677      IF (CM-AH-BENEFIT-CD IS EQUAL TO '00' OR '  ')
00678          MOVE LOW-VALUES         TO WS-AH-CANCEL-DATE
00679                                     PI-AH-CANCEL-DATE
00680          MOVE SPACES             TO WS-AH-CANCEL-DATE-ED
00681                                     PI-AH-CANCEL-DATE-ED
00682                                     PI-AH-REFUND-METH
00683          MOVE ZEROS              TO PI-AH-REFUND-AMT
00684          GO TO 0510-CONT.
00685
00686      IF (CM-AH-CURRENT-STATUS  IS EQUAL TO  '8')  AND
00687         (CM-AH-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES)
00688           MOVE CM-AH-ITD-CANCEL-AMT TO HAITDRO
00689           ADD  CM-AH-ITD-CANCEL-AMT TO WS-TOT-ITDR
00690         IF FIRST-ENTRY
00691             MOVE CM-AH-CANCEL-DT      TO DC-BIN-DATE-1
00692             MOVE SPACES               TO DC-OPTION-CODE
00693             PERFORM 9600-DATE-LINK
00694             IF NOT DATE-CONVERSION-ERROR
00695                 MOVE DC-GREG-DATE-1-EDIT TO HACANCO
00696                                             PI-AH-CANCEL-DATE-ED
00697                                             WS-AH-CANCEL-DATE-ED
00698                  MOVE CM-AH-CANCEL-DT  TO   PI-AH-CANCEL-DATE
00699                                             WS-AH-CANCEL-DATE.
00700
00701      IF (CM-AH-CURRENT-STATUS  IS EQUAL TO  '6' OR '7')   AND
00702         (CM-AH-SETTLEMENT-DT  IS NOT EQUAL TO  LOW-VALUES)
00703           MOVE WS-PMT-DESCRIP       TO PMTHDO
00704           MOVE CM-AH-ITD-LUMP-PMT   TO HAITDRO
00705           ADD  CM-AH-ITD-LUMP-PMT   TO WS-TOT-ITDR
00706          IF FIRST-ENTRY
00707              MOVE CM-AH-SETTLEMENT-DT  TO DC-BIN-DATE-1
00708              MOVE SPACES               TO DC-OPTION-CODE
00709              PERFORM 9600-DATE-LINK
00710              IF NOT DATE-CONVERSION-ERROR
00711                 MOVE DC-GREG-DATE-1-EDIT TO HACANCO
00712                                             PI-AH-CANCEL-DATE-ED
00713                                             WS-AH-CANCEL-DATE-ED
00714                 MOVE CM-AH-SETTLEMENT-DT TO PI-AH-CANCEL-DATE
00715                                             WS-AH-CANCEL-DATE.
00716
00717      IF CM-AH-CURRENT-STATUS IS EQUAL TO '8'
00718          MOVE 'CANC DT'              TO  CNCDTHDO
00719          GO TO 0510-CONT.
00720
00721      IF CM-LF-CURRENT-STATUS IS EQUAL TO '7'
00722          MOVE 'DTH DT '              TO  CNCDTHDO
00723          IF FIRST-ENTRY
00724              MOVE CM-LF-DEATH-DT     TO  DC-BIN-DATE-1
00725              MOVE ' '                TO  DC-OPTION-CODE
00726              PERFORM 9600-DATE-LINK
00727              IF NOT DATE-CONVERSION-ERROR
00728                  MOVE DC-GREG-DATE-1-EDIT TO HACANCO
00729                                              PI-AH-CANCEL-DATE-ED
00730                                              WS-AH-CANCEL-DATE-ED
00731                  MOVE CM-LF-DEATH-DT      TO PI-AH-CANCEL-DATE
00732                                              WS-AH-CANCEL-DATE.
00733
00734  0510-CONT.
00735
00736      IF WS-TOT-ITDR GREATER THAN ZEROS
00737          MOVE WS-TOT-ITDR TO TOITDRO.
00738
00739  0520-GET-COMPANY-RECORD.
00740      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
00741      MOVE '1'                    TO  WS-CF-RECORD-TYPE.
00742      MOVE SPACES                 TO  WS-CF-ACCESS.
00743      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
00744
00745      PERFORM 1000-READ-CONTROL  THRU  1099-EXIT.
00746
00747      IF CNTL-NOT-FOUND
00748          MOVE -1                 TO  HLCANCL
00749          MOVE AL-SANON           TO  HLCANCA
00750          MOVE ER-2616            TO  EMI-ERROR
00751          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00752          GO TO 8100-SEND-INITIAL-MAP.
00753
00754      IF WS-CF-COMPANY-ID  IS EQUAL TO  CF-COMPANY-ID
00755        AND WS-CF-RECORD-TYPE  IS EQUAL TO  CF-RECORD-TYPE
00756          MOVE CF-LIFE-OVERRIDE-L1
00757                                  TO  WS-CF-LIFE-OVERRIDE-L1
00758          MOVE CF-LIFE-OVERRIDE-L2
00759                                  TO  WS-CF-LIFE-OVERRIDE-L2
00760          MOVE CF-AH-OVERRIDE-L1  TO  WS-CF-AH-OVERRIDE-L1
00761          MOVE CF-AH-OVERRIDE-L2  TO  WS-CF-AH-OVERRIDE-L2
00762          MOVE CF-CR-REM-TERM-CALC
00763                                  TO  WS-CF-CR-REM-TERM-CALC
00764          MOVE CF-CR-R78-METHOD   TO  WS-CF-CR-R78-METHOD
00765          IF CF-DEFAULT-APR NUMERIC
00766              MOVE CF-DEFAULT-APR     TO  WS-CF-DEFAULT-APR
00767              GO TO 0525-GET-LF-BENEFIT
00768          ELSE
00769              MOVE ZEROS              TO  WS-CF-DEFAULT-APR
00770              GO TO 0525-GET-LF-BENEFIT.
00771
00772      MOVE -1                     TO  HLCANCL.
00773      MOVE AL-SANON               TO  HLCANCA.
00774      MOVE ER-2616                TO  EMI-ERROR.
00775
00776      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
00777
00778      GO TO 8100-SEND-INITIAL-MAP.
00779
PEMMOD 0525-GET-LF-BENEFIT.
PEMMOD
PEMMOD     IF CM-LF-BENEFIT-CD = '  ' OR '00'
PEMMOD        GO TO 0527-GET-AH-BENEFIT
PEMMOD     END-IF
PEMMOD


00995      MOVE '4'                    TO  WS-CF-RECORD-TYPE.
00996      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO
00997                                      HLCDO.
00998      MOVE WS-CF-LIFE-OVERRIDE-L2
00999                                  TO  HLKINDO.
01000
01001      PERFORM 1600-LOCATE-BENEFIT THRU 1699-EXIT.
01002
01003      IF BENEFIT-FOUND
01004          NEXT SENTENCE
01005      ELSE
01006          MOVE -1                 TO  HLCDL
01007          MOVE AL-SANON           TO  HLCDA
01008          MOVE ER-0028            TO  EMI-ERROR
01009          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01010          GO TO 8100-SEND-INITIAL-MAP.
01011
01016      IF CF-SUMMARY-PROCESSING (WS-INDEX)
01017          MOVE AL-UNNOF           TO  HLTERMA.
01018
01019 *****MOVE WS-BENEFIT-DESCRIP     TO  HLDESCO.
           MOVE WS-KIND                TO WS-LF-KIND
01020      MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)
01021                                  TO  WS-CF-LF-COVERAGE-TYPE.
01022
01023      IF CF-CO-REM-TERM-CALC (WS-INDEX)  IS GREATER THAN  '0'
01024          MOVE CF-CO-REM-TERM-CALC (WS-INDEX)
01025                                  TO  WS-LF-CO-REM-TERM-CALC.
01026
01027      IF CF-CO-EARNINGS-CALC (WS-INDEX)  IS GREATER THAN  ' '
01028          MOVE CF-CO-EARNINGS-CALC (WS-INDEX)
01029                                  TO  WS-LF-CO-EARNINGS-CALC
01030                                      WS-LF-CO-REFUND-CALC.
01031
01032      IF CF-SPECIAL-CALC-CD (WS-INDEX)  IS GREATER THAN  ' '
01033          MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
01034                                  TO  WS-LF-SPECIAL-CALC-CD.
01035
01036      IF CF-CO-REFUND-CALC (WS-INDEX)  IS GREATER THAN  '0'
01037          MOVE CF-CO-REFUND-CALC (WS-INDEX)
01038                                  TO  WS-LF-CO-REFUND-CALC.
01039
PEMMOD 0527-GET-AH-BENEFIT.
PEMMOD
PEMMOD     IF CM-AH-BENEFIT-CD = '00' OR '  '
PEMMOD        GO TO 0530-GET-STATE-RECORD
PEMMOD     END-IF
PEMMOD
01265      MOVE '5'                    TO  WS-CF-RECORD-TYPE.
01266      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO
01267                                      HACDO.
01268      MOVE WS-CF-AH-OVERRIDE-L2   TO  HAKINDO.
01269
01270      PERFORM 1600-LOCATE-BENEFIT THRU 1699-EXIT.
01271
01272      IF BENEFIT-FOUND
01273          NEXT SENTENCE
01274      ELSE
01275          MOVE -1                 TO  HACDL
01276          MOVE AL-SANON           TO  HACDA
01277          MOVE ER-0028            TO  EMI-ERROR
01278          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01279          GO TO 8100-SEND-INITIAL-MAP.
01280
01285      IF CF-SUMMARY-PROCESSING (WS-INDEX)
01286          MOVE AL-UNNOF           TO  HATERMA.
01287
01288 *****MOVE WS-BENEFIT-DESCRIP     TO  HADESCO.
           MOVE WS-KIND                TO  WS-AH-KIND
01289
01290      IF CF-CO-REM-TERM-CALC (WS-INDEX)  IS GREATER THAN  '0'
01291          MOVE CF-CO-REM-TERM-CALC (WS-INDEX)
01292                                  TO  WS-AH-CO-REM-TERM-CALC.
01293
01294      IF CF-CO-EARNINGS-CALC (WS-INDEX)  IS GREATER THAN  ' '
01295          MOVE CF-CO-EARNINGS-CALC (WS-INDEX)
01296                                  TO  WS-AH-CO-EARNINGS-CALC
01297                                      WS-AH-CO-REFUND-CALC.
01298
01299      IF CF-SPECIAL-CALC-CD (WS-INDEX)  IS GREATER THAN  ' '
01300          MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
01301                                  TO  WS-AH-SPECIAL-CALC-CD.
01302
01303      IF CF-CO-REFUND-CALC (WS-INDEX)  IS GREATER THAN  '0'
01304          MOVE CF-CO-REFUND-CALC (WS-INDEX)
01305                                  TO  WS-AH-CO-REFUND-CALC.

           IF CF-BENEFIT-CATEGORY (WS-INDEX) > ' '
              MOVE CF-BENEFIT-CATEGORY (WS-INDEX)
                                       TO WS-AH-BEN-CATEGORY
           END-IF

01306      .
00780  0530-GET-STATE-RECORD.
00781      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
00782      MOVE '3'                    TO  WS-CF-RECORD-TYPE.
00783      MOVE CM-STATE               TO  WS-CF-ACCESS.
00784      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
00785
00786      PERFORM 1000-READ-CONTROL  THRU  1099-EXIT.
00787
00788      IF CNTL-NOT-FOUND
00789          MOVE '0'                TO  WS-LF-ST-REFUND-CALC
00790                                      WS-LF-ST-REM-TERM-CALC
00791          MOVE 'N'                TO  WS-STATE-RECORD-SW
00792          GO TO 0560-FIND-ACCOUNT.
00793
00794      IF WS-CF-COMPANY-ID  IS EQUAL TO  CF-COMPANY-ID
00795        AND WS-CF-RECORD-TYPE  IS EQUAL TO  CF-RECORD-TYPE
00796        AND CM-STATE  IS EQUAL TO  CF-STATE-CODE
00797          MOVE 'Y'                TO  WS-STATE-RECORD-SW
00798      ELSE
00799          MOVE '0'                TO  WS-LF-ST-REFUND-CALC
00800                                      WS-LF-ST-REM-TERM-CALC
00801          MOVE 'N'                TO  WS-STATE-RECORD-SW
00802          GO TO 0560-FIND-ACCOUNT.
00803
00804      MOVE CF-STATE-ABBREVIATION  TO  WS-STATE-ABBREVIATION.
           MOVE CF-ST-FST-PMT-DAYS-CHG TO  WS-STATE-EXT-DAYS-CHG
00805
00806      IF CM-LF-BENEFIT-CD  = '00'
00807          GO TO 0550-GET-AH-BEN-IN-STATE.
00808
00809  0540-GET-LF-BEN-IN-STATE.
00810      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
00811      MOVE PI-LIFE-OVERRIDE-L1    TO  WS-LOOKUP-TYPE.
00812
CIDMOD     MOVE '0'                    TO WS-LF-ST-REM-TERM-CALC
CIDMOD     IF CF-ST-RT-CALC NOT = SPACES
CIDMOD        MOVE CF-ST-RT-CALC       TO WS-LF-ST-REM-TERM-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF WS-CF-LF-COVERAGE-TYPE = 'R'
PEMMOD        IF CF-ST-RF-LR-CALC > '0'
PEMMOD           MOVE CF-ST-RF-LR-CALC TO WS-LF-ST-REFUND-CALC
PEMMOD        END-IF
PEMMOD        IF WS-LF-CO-EARNINGS-CALC = 'N' OR '5'
PEMMOD           IF CF-ST-RF-LN-CALC > '0'
PEMMOD              MOVE CF-ST-RF-LN-CALC
PEMMOD                                 TO WS-LF-ST-REFUND-CALC
PEMMOD           END-IF
PEMMOD        END-IF
PEMMOD     ELSE
PEMMOD        IF CF-ST-RF-LL-CALC > '0'
PEMMOD           MOVE CF-ST-RF-LL-CALC TO WS-LF-ST-REFUND-CALC
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
00813      PERFORM 1100-FIND-BENEFIT-IN-STATE  THRU  1199-EXIT.
00814
00815      IF NO-BENEFIT-FOUND
PEMMOD*        MOVE '0'                TO  WS-LF-ST-REFUND-CALC
00818          GO TO 0550-GET-AH-BEN-IN-STATE.
00819
00820      IF CF-ST-REM-TERM-CALC (SUB3)  IS GREATER THAN  '0'
00821          MOVE CF-ST-REM-TERM-CALC (SUB3)
00822                                  TO  WS-LF-ST-REM-TERM-CALC.
00823
00824      IF CF-ST-REFUND-CALC (SUB3)  IS GREATER THAN  '0'
00825          MOVE CF-ST-REFUND-CALC (SUB3)
00826                                  TO  WS-LF-ST-REFUND-CALC.
00827
00828  0550-GET-AH-BEN-IN-STATE.
00829      IF CM-AH-BENEFIT-CD  = '00'
00830          GO TO 0560-FIND-ACCOUNT.
00831
CIDMOD     MOVE '0'                    TO WS-AH-ST-REM-TERM-CALC
CIDMOD     IF CF-ST-RT-CALC NOT = SPACES
CIDMOD        MOVE CF-ST-RT-CALC       TO WS-AH-ST-REM-TERM-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF CF-ST-RF-AH-CALC > '0'
PEMMOD        MOVE CF-ST-RF-AH-CALC    TO WS-AH-ST-REFUND-CALC
PEMMOD     END-IF
PEMMOD     IF WS-AH-SPECIAL-CALC-CD = 'C'
PEMMOD        IF CF-ST-RF-CP-CALC > '0'
PEMMOD           MOVE CF-ST-RF-CP-CALC TO WS-AH-ST-REFUND-CALC
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
00832      MOVE CM-AH-BENEFIT-CD       TO  WS-BEN-CD.
00833      MOVE PI-AH-OVERRIDE-L1      TO  WS-LOOKUP-TYPE.
00834
00835      PERFORM 1100-FIND-BENEFIT-IN-STATE  THRU  1199-EXIT.
00836
00837      IF NO-BENEFIT-FOUND
PEMMOD*        MOVE '0'                TO  WS-AH-ST-REFUND-CALC
00840          GO TO 0560-FIND-ACCOUNT.
00841
00842      IF CF-ST-REM-TERM-CALC (SUB3)  IS GREATER THAN  '0'
00843          MOVE CF-ST-REM-TERM-CALC (SUB3)
00844                                  TO  WS-AH-ST-REM-TERM-CALC.
00845
00846      IF CF-ST-REFUND-CALC (SUB3)  IS GREATER THAN  '0'
00847          MOVE CF-ST-REFUND-CALC (SUB3)
00848                                  TO  WS-AH-ST-REFUND-CALC.
00849
00850  0560-FIND-ACCOUNT.
00851      MOVE CM-COMPANY-CD          TO  WS-AM-COMPANY-CD.
00852      MOVE CM-CARRIER             TO  WS-AM-CARRIER.
00853      MOVE CM-GROUPING            TO  WS-AM-GROUPING.
00854      MOVE CM-STATE               TO  WS-AM-STATE.
00855      MOVE CM-ACCOUNT             TO  WS-AM-ACCOUNT.
00856      MOVE CM-CERT-EFF-DT         TO  WS-AM-EXPIRATION-DT.
00857      MOVE LOW-VALUES             TO  WS-AM-FILLER.
00858
00859      PERFORM 1200-START-ACCOUNT-MASTER  THRU  1299-EXIT.
00860
00861      IF ACCT-FOUND
00862          NEXT SENTENCE
00863      ELSE
00864          GO TO 0585-NO-ACCT.
00865
00866  0570-READ-NEXT-RANGE.
00867      PERFORM 1300-READ-ACCOUNT-MASTER  THRU  1399-EXIT.
00868
00869      IF ACCT-FOUND
00870          NEXT SENTENCE
00871      ELSE
00872          GO TO 0580-ACCT-NOT-FOUND.
00873
00874      IF CM-COMPANY-CD  IS EQUAL TO  AM-COMPANY-CD
00875        AND CM-CARRIER  IS EQUAL TO  AM-CARRIER
00876        AND CM-GROUPING  IS EQUAL TO  AM-GROUPING
00877        AND CM-STATE  IS EQUAL TO  AM-STATE
00878        AND CM-ACCOUNT  IS EQUAL TO  AM-ACCOUNT
00879          NEXT SENTENCE
00880      ELSE
00881          GO TO 0580-ACCT-NOT-FOUND.
00882
00883      MOVE 'X'                    TO  DATE-RANGE-SW.
00884
00885      IF CM-CERT-EFF-DT  IS LESS THAN  AM-EFFECTIVE-DT
00886          MOVE 'X'                TO  DATE-RANGE-SW
00887          GO TO 0580-ACCT-NOT-FOUND.
00888
00889      IF CM-CERT-EFF-DT  IS NOT LESS THAN  AM-EXPIRATION-DT
00890          GO TO 0570-READ-NEXT-RANGE.
00891
           IF WS-BROWSE-STARTED-SW = 'Y'
              EXEC CICS ENDBR
                 DATASET    ('ERACCT')
              END-EXEC
              MOVE SPACES             TO  WS-BROWSE-STARTED-SW
           END-IF

00892      MOVE ' '                    TO  DATE-RANGE-SW.
00893      MOVE 'Y'                    TO  WS-ACCT-RECORD-SW.
00894
00895      GO TO 0590-SET-ACCOUNT.
00896
00897  0580-ACCT-NOT-FOUND.
00898      IF DATE-RANGE-SW  IS NOT EQUAL TO  SPACES
00899          MOVE ER-2601            TO  EMI-ERROR
00900      ELSE
00901          MOVE ER-2619            TO  EMI-ERROR.
00902
00903      MOVE 'N'                    TO  WS-ACCT-RECORD-SW.
00904
00905      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
00906
00907  0585-NO-ACCT.
00908      MOVE SPACES                 TO  WS-ACCT-USER-FLD-5.
00909      MOVE '0'                    TO  WS-AM-EARN-METHOD-L
00910                                      WS-AM-EARN-METHOD-R
00911                                      WS-AM-EARN-METHOD-A
00912                                      WS-LF-AM-REM-TERM-CALC
00913                                      WS-AH-AM-REM-TERM-CALC.
00914
00915      GO TO 0600-CHECK-FOR-LIFE.
00916
00917  0590-SET-ACCOUNT.
00918 ******************************************************************
00919 ******* THIS CODE ALLOWS THE FORM MASTER BENEFIT CODES TERM
00920 ******* AND REFUND METHOD TO OVERRIDE COMPANY AND STATE REFUND
00921 ******* METHODS.
00922 ******************************************************************
00923      MOVE SPACES              TO WS-LF-FO-REFUND-CALC.
00924      MOVE SPACES              TO WS-AH-FO-REFUND-CALC.
00925      IF PI-COMPANY-ID EQUAL TO 'NCL'
00926           IF ((CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES)
00927                  OR (CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES))
00928                   PERFORM 7000-GET-ERFORM THRU 7000-EXIT
00929                   IF FORM-FOUND
00930                       PERFORM 7100-BENEFIT-SEARCH THRU 7100-EXIT.
00931 ******************************************************************
00932
00933      MOVE AM-FLD-5               TO  WS-ACCT-USER-FLD-5.
00934      MOVE '0'                    TO  WS-AM-EARN-METHOD-L
00935                                      WS-AM-EARN-METHOD-R
00936                                      WS-AM-EARN-METHOD-A.
070622     IF AM-GPCD > 1
070622       AND AM-GPCD < 6
070622       AND PI-CANCEL-REASON NOT > SPACES
070622        MOVE 'Y'                 TO PI-CANCEL-REASON
070622     END-IF.
           MOVE PI-CANCEL-REASON       TO  CANREAO
00937
PEMMOD*    EVALUATE AM-EARN-METHOD-R
PEMMOD*       WHEN 'R'
PEMMOD*           MOVE '1'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'P'
PEMMOD*           MOVE '2'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'C'
PEMMOD*           MOVE '3'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'A'
PEMMOD*           MOVE '6'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'M'
PEMMOD*           MOVE '8'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'S'
PEMMOD*           MOVE '9'                TO  WS-AM-EARN-METHOD-R
PEMMOD*    END-EVALUATE.
PEMMOD
PEMMOD*    EVALUATE AM-EARN-METHOD-L
PEMMOD*       WHEN 'R'
PEMMOD*           MOVE '1'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'P'
PEMMOD*           MOVE '2'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'C'
PEMMOD*           MOVE '3'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'A'
PEMMOD*           MOVE '6'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'M'
PEMMOD*           MOVE '8'                 TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'S'
PEMMOD*           MOVE '9'                 TO  WS-AM-EARN-METHOD-L
PEMMOD*    END-EVALUATE.
PEMMOD
PEMMOD*    EVALUATE AM-EARN-METHOD-A
PEMMOD*       WHEN 'R'
PEMMOD*           MOVE '1'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'P'
PEMMOD*           MOVE '2'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'C'
PEMMOD*           MOVE '3'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'A'
PEMMOD*           MOVE '6'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'M'
PEMMOD*           MOVE '8'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'N'
PEMMOD*           MOVE '5'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'S'
PEMMOD*           MOVE '9'                TO  WS-AM-EARN-METHOD-A
PEMMOD*    END-EVALUATE.
00984
00985      MOVE ZEROS                  TO  WS-TOT-AH-PREM
00986                                      WS-TOT-LF-PREM
00987                                      WS-TOT-AH-RFND
00988                                      WS-TOT-LF-RFND.
00989
00990  0600-CHECK-FOR-LIFE.
00991      IF CM-LF-BENEFIT-CD  = '00'
00992          GO TO 0640-AH-BENEFIT.
00993
00994  0610-GET-LIFE-BENEFIT.
00995
01012      IF ACCT-FOUND
01013          MOVE +1                 TO SUB3
01014          PERFORM 5000-GET-LF-AM-REM-TERM THRU 5900-EXIT.

01046      MOVE WS-LF-CANCEL-DATE-ED   TO  HLCANCO.
01051      MOVE WS-LF-KIND             TO  HLEDESCO.
01075      MOVE CM-LF-ORIG-TERM        TO  HLTERMO
           IF WS-LF-CO-EARNINGS-CALC = 'B'
01146         COMPUTE WS-LIFE-PREMIUM = CM-LF-PREMIUM-AMT
01147                              + CM-LF-ALT-PREMIUM-AMT
           ELSE
              MOVE CM-LF-PREMIUM-AMT TO WS-LIFE-PREMIUM
           END-IF
01148
01149      MOVE WS-LIFE-PREMIUM        TO  HLPREMO


           EVALUATE TRUE
              WHEN ((OPEN-LF-CLAIM)
                 OR (CLOSED-LF-CLAIM))
                 AND (WS-LF-CANCEL-DATE NOT = WS-LF-INCUR-DT)
                 MOVE ZEROS            TO HLREFNDO  WS-CALC-REFUND     
                                          WS-TOT-LF-RFND               
                                          PI-LF-REFUND-AMT
                 IF CM-AH-BENEFIT-CD NOT = '00'
                    MOVE ZEROS         TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                    MOVE SPACES        TO PI-AH-REFUND-METH
                 END-IF
100518           IF OTHER-CLAIM
100518              MOVE ER-2998          TO EMI-ERROR
100518           ELSE
                    MOVE ER-2965          TO EMI-ERROR
100518           END-IF
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0640-AH-BENEFIT
              WHEN OPEN-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND = '4'
                 MOVE ER-2966          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              WHEN OPEN-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND NOT = '4'
                 MOVE ER-2967          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0640-AH-BENEFIT
              WHEN CLOSED-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND = '4'
                 MOVE ER-2968          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              WHEN CLOSED-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND NOT = '4'
                 MOVE ER-2969          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0640-AH-BENEFIT
              WHEN OTHER
                 CONTINUE
           END-EVALUATE

           .
01040  0620-GET-REM-TERM.

01041      MOVE WS-CF-LIFE-OVERRIDE-L1
01042                                  TO  CP-LIFE-OVERRIDE-CODE.
01043      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
01044      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01045      MOVE WS-LF-CANCEL-DATE      TO  CP-VALUATION-DT.
01046      MOVE WS-LF-CANCEL-DATE-ED   TO  HLCANCO.
01047      MOVE CM-STATE               TO  CP-STATE.
01048      MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
01049      MOVE WS-CF-LF-COVERAGE-TYPE
01050                                  TO  CP-BENEFIT-TYPE.
01051      MOVE WS-LF-KIND             TO  HLEDESCO.
01052      MOVE WS-LF-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
01053      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
01054      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
01055      MOVE '2'                    TO  CP-PROCESS-TYPE.
01056      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01057      MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
01058      MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
01059      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
01060      MOVE WS-CF-CR-REM-TERM-CALC
01061                                  TO  CP-REM-TERM-METHOD.
01062
01063      IF WS-LF-CO-REM-TERM-CALC  IS GREATER THAN  '0'
01064          MOVE WS-LF-CO-REM-TERM-CALC
01065                                  TO  CP-REM-TERM-METHOD.
01066
01067      IF WS-LF-ST-REM-TERM-CALC  IS GREATER THAN  '0'
01068          MOVE WS-LF-ST-REM-TERM-CALC
01069                                  TO  CP-REM-TERM-METHOD.
01070
01071      IF WS-LF-AM-REM-TERM-CALC  IS GREATER THAN  '0'
01072          MOVE WS-LF-AM-REM-TERM-CALC
01073                                  TO  CP-REM-TERM-METHOD.
01074
083005     IF (PI-COMPANY-ID = 'CID')
022608        AND (CP-STATE-STD-ABBRV = 'WI')
083005        MOVE '7'                 TO CP-REM-TERM-METHOD
022608        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
083005     END-IF

           IF (PI-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MO')
              AND (CM-CERT-EFF-DT >= X'9B41')
              AND (CM-CERT-EFF-DT <= X'A2FB')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF

01075      MOVE CM-LF-ORIG-TERM        TO  HLTERMO
01076                                      CP-ORIGINAL-TERM
01077                                      CP-LOAN-TERM.
01078
01079      IF CP-TRUNCATED-LIFE
01080          MOVE CM-LOAN-TERM       TO  CP-LOAN-TERM.
01081
01082      IF CP-TERM-IS-DAYS
01083          IF CM-LF-TERM-IN-DAYS  IS NUMERIC
01084              MOVE CM-LF-TERM-IN-DAYS
01085                                  TO  CP-TERM-OR-EXT-DAYS
01086          ELSE
01087              MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS
01088      ELSE
01089          IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
01090              MOVE CM-PMT-EXTENSION-DAYS
01091                                  TO  CP-TERM-OR-EXT-DAYS
01092          ELSE
01093              MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
01094
01095 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
01096      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
01097      MOVE '3'                    TO  WS-CF-RECORD-TYPE.
01098      MOVE CM-STATE               TO  WS-CF-ACCESS.
01099      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
01100
01101      EXEC CICS READ
01102          DATASET  (ELCNTL-ID)
01103          SET      (ADDRESS OF CONTROL-FILE)
01104          RIDFLD   (WS-CF-CONTROL-PRIMARY)
01105          RESP     (WS-RESPONSE)
01106      END-EXEC.
01107
01108      IF WS-RESP-NOTOPEN
01109          MOVE ER-2617            TO  EMI-ERROR
01110          GO TO 0320-INPUT-ERROR.
01111
01112      IF WS-RESP-NOTFND
01113          MOVE ZERO               TO  CP-FREE-LOOK
01114      ELSE
01115          MOVE CF-ST-FREE-LOOK-PERIOD
01116                                  TO CP-FREE-LOOK.
01117
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        ADD +1   TO CP-ORIGINAL-TERM CP-LOAN-TERM
CIDMOD     END-IF
CIDMOD
01118      PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT.
01119
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        MOVE CP-REMAINING-TERM-1   TO WS-BALLOON-RTRM
CIDMOD        COMPUTE CP-REMAINING-TERM-1 =
CIDMOD                CP-REMAINING-TERM-1 - +1
CIDMOD        COMPUTE CP-REMAINING-TERM-2 =
CIDMOD                CP-REMAINING-TERM-2 - +1
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CP-REMAINING-TERM-1 NEGATIVE
CIDMOD        MOVE ZEROS               TO CP-REMAINING-TERM-1
CIDMOD     END-IF
CIDMOD     IF CP-REMAINING-TERM-2 NEGATIVE
CIDMOD        MOVE ZEROS               TO CP-REMAINING-TERM-2
CIDMOD     END-IF
01120      MOVE CP-REMAINING-TERM-1    TO  HLREMO.
01121
00991      IF PI-LF-CANCEL-DATE = LOW-VALUES
00992          GO TO 0640-AH-BENEFIT.

01122 *    IF CP-REMAINING-TERM-1  IS GREATER THAN  CM-LF-ORIG-TERM
01123 *        MOVE CM-LF-ORIG-TERM    TO  HLREMO.
01124
01125      IF CM-LF-CURRENT-STATUS  IS EQUAL TO  '8'
01126          IF CM-LF-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES
01127              MOVE ER-0681        TO  EMI-ERROR
01128              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01129
01130      IF CM-LF-CURRENT-STATUS  IS EQUAL TO  '7'
01131          IF CM-LF-DEATH-DT  IS NOT EQUAL TO  LOW-VALUES
01132              MOVE ER-0682        TO  EMI-ERROR
01133              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01134
01135  0630-CONTINUE-LIFE.
01136
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
                                          FREMTRMO
CIDMOD     ELSE
              MOVE CP-REMAINING-TERM-2 TO  CP-REMAINING-TERM
                                           FREMTRMO
CIDMOD     END-IF

01139      MOVE WS-LF-CO-EARNINGS-CALC TO  CP-EARNING-METHOD.
01140      MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT.
032912     if pi-company-id = 'AHL'
032912        MOVE CM-LF-CLASS-CD      TO CP-CLASS-CODE
032912        if cp-class-code = spaces
032912           move zeros            to cp-class-code
032912        end-if
032912     ELSE
CIDMOD        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
032912     END-IF

           IF (PI-COMPANY-ID = 'DCC')
              AND (AM-DCC-PRODUCT-CODE = 'DDF')
              MOVE ' '                 TO WS-PDEF-RECORD-SW
              PERFORM 0730-GET-DDF-FACTORS
                                       THRU 0730-EXIT
              IF PDEF-FOUND
070622           IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
070622               MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
070622           ELSE
070622               MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
070622           END-IF
070622           COMPUTE WS-ATT-AGE = WS-EDIT-AGE
070622              + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
                 PERFORM VARYING P1 FROM +1 BY +1 UNTIL
PEMTST              (P1 > +11)
                    OR ((PD-PROD-CODE (P1) = 'L' OR 'O')
070622                AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
                 END-PERFORM
PEMTST           IF P1 < +12
                    MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
                 END-IF
              END-IF
           END-IF

01142      PERFORM 0710-LINK-REM-AMOUNT THRU 0710-EXIT.
01143
01144      MOVE CP-REMAINING-AMT       TO CP-REMAINING-BENEFIT.
01145
           IF WS-LF-CO-EARNINGS-CALC = 'B'
01146         COMPUTE WS-LIFE-PREMIUM = CM-LF-PREMIUM-AMT
01147                              + CM-LF-ALT-PREMIUM-AMT
           ELSE
              MOVE CM-LF-PREMIUM-AMT TO WS-LIFE-PREMIUM
           END-IF
01148
01149      MOVE WS-LIFE-PREMIUM        TO  HLPREMO
01150                                      WS-TOT-LF-PREM.
01151
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
CIDMOD     ELSE
CIDMOD        MOVE CP-REMAINING-TERM-1 TO  CP-REMAINING-TERM
CIDMOD     END-IF
01153
01154      MOVE WS-LF-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
01155      MOVE WS-LF-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
01156
01157      IF WS-LF-ST-REFUND-CALC  IS GREATER THAN  ZERO
01158          MOVE WS-LF-ST-REFUND-CALC
01159                                  TO  CP-EARNING-METHOD.
01160
01161      IF WS-LF-FO-REFUND-CALC  IS GREATER THAN  ZERO
01162          MOVE WS-LF-FO-REFUND-CALC
01163                                  TO  CP-EARNING-METHOD.
01164
102212     if (pi-company-id = 'CID')
102212        and (cp-state-std-abbrv = 'MN' OR 'WA')
102212        continue
102212     else
111907        IF CP-RATING-METHOD = '4'
111907           MOVE '4'              TO CP-EARNING-METHOD
111907        END-IF
102212     end-if

PEMMOD*    IF WS-CF-LF-COVERAGE-TYPE = 'R'
PEMMOD*        IF WS-AM-EARN-METHOD-R  IS GREATER THAN  ZERO
PEMMOD*            MOVE WS-AM-EARN-METHOD-R
PEMMOD*                                TO  CP-EARNING-METHOD
PEMMOD*        END-IF
PEMMOD*    ELSE
PEMMOD*        IF WS-AM-EARN-METHOD-L  IS GREATER THAN  ZERO
PEMMOD*            MOVE WS-AM-EARN-METHOD-L
PEMMOD*                                TO  CP-EARNING-METHOD.
01174
032912     if pi-company-id = 'AHL'
032912        MOVE CM-LF-CLASS-CD      TO CP-CLASS-CODE
032912        if cp-class-code = spaces
032912           move zeros            to cp-class-code
032912        end-if
032912     ELSE
CIDMOD        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
032912     END-IF
01176      MOVE CM-LF-BENEFIT-CD       TO  CP-BENEFIT-CD.
01177      MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
01178                                      CP-RATING-BENEFIT-AMT.
01179      IF CP-STATE-STD-ABBRV = 'OR'
01180          COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
01181                                          CM-LF-ALT-BENEFIT-AMT.
CIDMOD****   N O T E   ****
CIDMOD*      CID DOES NOT WANT THE REFUND METHOD TO OVERRIDE
CIDMOD*      THE OH HARD CODING IN ELCRFNDP
CIDMOD
CIDMOD     IF PI-COMPANY-ID = 'CID'
CIDMOD        IF CP-STATE-STD-ABBRV = 'OH'
CIDMOD           MOVE WS-LF-CO-EARNINGS-CALC
CIDMOD                                 TO CP-EARNING-METHOD
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD****   N O T E   ****
CIDMOD
      ****   MN   NOTE  ****  CID HAS THE STATE REFUND METHOD SET AS
      ****      R78 BUT THEY WANT THE BALLOONS REFUNDED WITH A 
      ****      CUSTOM FORMULA THAT IS IN ELRFND SO I AM SETTING
      ****     THE APPROPRIATE OPTIONS HERE

      *    IF PI-COMPANY-ID = 'CID'
      *       IF (CP-STATE-STD-ABBRV = 'MN')
      *          AND (CM-CERT-EFF-DT > X'A4FF')
      *          AND (WS-LF-CO-EARNINGS-CALC = 'B')
      *          MOVE '5'              TO CP-EARNING-METHOD
      *       END-IF
      *    END-IF


01182      MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
01183      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
01184      MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
01185      MOVE CM-LF-DEV-PCT          TO  CP-RATE-DEV-PCT.
01186      MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
           MOVE PI-CANCEL-REASON       TO CP-CANCEL-REASON
01187
102808*    IF CP-EARN-AS-NET-PAY
01189 *        IF CP-LOAN-APR NOT GREATER THAN ZEROS
01190 *           IF WS-CF-DEFAULT-APR GREATER THAN ZEROS
01191 *               MOVE WS-CF-DEFAULT-APR
01192 *                                TO  CP-LOAN-APR
01193 *               MOVE ER-3780     TO  EMI-ERROR
01194 *               PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01195 *           ELSE
01196 *               MOVE ER-3781     TO  EMI-ERROR
01197 *               PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01198

      *    DISPLAY ' 1278 REM AMT      ' CP-REMAINING-BENEFIT
      *    DISPLAY ' 1278 REM TRM      ' CP-REMAINING-TERM
      *    DISPLAY ' 1278 EARN METH    ' CP-EARNING-METHOD
      *    DISPLAY ' 1278 RATE METH    ' CP-RATING-METHOD
      *    DISPLAY ' 1278 CLASS        ' CP-CLASS-CODE
      *    DISPLAY ' 1278 BENE CODE    ' CP-BENEFIT-CD
      *    DISPLAY ' 1278 ORIG BENE    ' CP-ORIGINAL-BENEFIT
      *    DISPLAY ' 1278 RATE BENE    ' CP-RATING-BENEFIT-AMT
      *    DISPLAY ' 1278 ORIG PREM    ' CP-ORIGINAL-PREMIUM
      *    DISPLAY ' 1278 ISS AGE      ' CP-ISSUE-AGE
      *    DISPLAY ' 1278 DEV CODE     ' CP-DEVIATION-CODE
      *    DISPLAY ' 1278 DEV PCT      ' CP-RATE-DEV-PCT
      *    DISPLAY ' 1278 APR          ' CP-LOAN-APR
      *    DISPLAY ' 1278 EXT DAYS     ' CP-TERM-OR-EXT-DAYS
      *    DISPLAY ' 1278 LOAN TERM    ' CP-LOAN-TERM
      *    DISPLAY ' 1278 SPEC CALC    ' CP-SPECIAL-CALC-CD
      *    DISPLAY ' 1278 ST STD ABB   ' CP-STATE-STD-ABBRV

050713*    MOVE WS-STATE-EXT-DAYS-CHG  TO CP-EXT-DAYS-CALC

01199      PERFORM 0800-LINK-REFUND  THRU  0899-EXIT.
01200
01201      MOVE 'L'                    TO  WS-REFUND-SEARCH-SW.
01202
01203      PERFORM 1400-GET-REFUND-TYPE  THRU  1499-EXIT.
01204
01205      IF CP-ERROR-RATE-IS-ZERO
01206        OR CP-ERROR-RATE-NOT-FOUND
01207          MOVE ER-2740            TO  EMI-ERROR
01208          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01209
01210      IF CP-ERROR-RATE-FILE-NOTOPEN
01211          MOVE ER-2617            TO  EMI-ERROR
01212          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01213
01214      MOVE ZEROS                  TO  WS-CALC-REFUND.
01215      MOVE CP-CALC-REFUND         TO  HLREFNDO  WS-CALC-REFUND
01216                                      WS-TOT-LF-RFND
01217                                      PI-LF-REFUND-AMT.
01218      MOVE CP-REMAINING-AMT       TO  HLREDO.
01219
01220      MOVE CP-REFUND-TYPE-USED    TO  PI-LF-REFUND-METH.

032117     move cm-life-comm-pct       to hlcpcto
032117     compute hlcamto rounded =
032117        cp-calc-refund * cm-life-comm-pct

01222      IF WS-LF-CO-EARNINGS-CALC  IS NOT EQUAL TO  'B'
01223          GO TO 0640-AH-BENEFIT.
01224
070105*    IF WS-LF-SPECIAL-CALC-CD = 'L'
070105*        ADD +1                  TO  CP-REMAINING-TERM
070105*                                    CP-ORIGINAL-TERM.
01228
01229      MOVE 'L'                    TO  CP-BENEFIT-TYPE.
01230      MOVE '2'                    TO  CP-EARNING-METHOD
01231                                      CP-RATING-METHOD.

           IF PI-COMPANY-ID = 'CID'
              IF (CP-STATE-STD-ABBRV = 'MN')
                 AND (CM-CERT-EFF-DT > X'A4FF')
                 AND (WS-LF-CO-EARNINGS-CALC = 'B')
                 MOVE '5'              TO CP-EARNING-METHOD
                 MOVE WS-LF-CO-EARNINGS-CALC TO CP-RATING-METHOD
              END-IF
           END-IF

01232      MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ORIGINAL-BENEFIT
01233                                      CP-REMAINING-BENEFIT
01234                                      CP-RATING-BENEFIT-AMT.
01235      IF CP-STATE-STD-ABBRV = 'OR'
01236          COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
01237                                          CM-LF-ALT-BENEFIT-AMT.
01238      MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ORIGINAL-PREMIUM.
01239      MOVE 'LEV'                  TO  CP-DEVIATION-CODE.
01240
01241      PERFORM 0800-LINK-REFUND  THRU  0899-EXIT.
01242
01243      IF CP-ERROR-RATE-IS-ZERO
01244        OR CP-ERROR-RATE-NOT-FOUND
01245          MOVE ER-2740            TO  EMI-ERROR
01246          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01247
01248      IF CP-ERROR-RATE-FILE-NOTOPEN
01249          MOVE ER-2617            TO  EMI-ERROR
01250          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01251
01252      ADD CP-CALC-REFUND          TO  WS-CALC-REFUND.
01253
01254      MOVE WS-CALC-REFUND         TO  HLREFNDO
01255                                      WS-TOT-LF-RFND
01256                                      PI-LF-REFUND-AMT.
01257
01258      MOVE CP-REFUND-TYPE-USED    TO  PI-LF-REFUND-METH.
01259
01260  0640-AH-BENEFIT.
01261      IF CM-AH-BENEFIT-CD = '00'
01262          GO TO 0680-CONTINUE.
01263
01264  0650-GET-AH-BENEFIT.
01281      IF ACCT-FOUND
01282          MOVE +1                 TO SUB3
01283          PERFORM 6000-GET-AH-AM-REM-TERM THRU 6900-EXIT.

01388      MOVE CM-AH-PREMIUM-AMT      TO  HAPREMO
01312      MOVE WS-AH-CANCEL-DATE-ED   TO  HACANCO.
01339      MOVE CM-AH-ORIG-TERM        TO  HATERMO

01314      MOVE WS-AH-KIND             TO  HAEDESCO.

           EVALUATE TRUE

              WHEN ((OPEN-LF-CLAIM)
                 OR (CLOSED-LF-CLAIM))
                 AND (WS-AH-CANCEL-DATE NOT = WS-LF-INCUR-DT)
                 MOVE ZEROS            TO HAREFNDO  WS-CALC-REFUND     
                                          WS-TOT-AH-RFND               
                                          PI-AH-REFUND-AMT
                 MOVE SPACES           TO PI-AH-REFUND-METH
100518           IF OTHER-CLAIM
100518              MOVE ER-2998          TO EMI-ERROR
100518           ELSE
                    MOVE ER-2965          TO EMI-ERROR
100518           END-IF

                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0680-CONTINUE


              WHEN (CM-LF-BENEFIT-CD NOT = '00')
                 AND ((OPEN-LF-CLAIM) OR (CLOSED-LF-CLAIM))
100518           AND ((EMI-ERROR = ER-2965) OR (EMI-ERROR = ER-2998))
                 AND (WS-LF-CANCEL-DATE NOT = WS-LF-INCUR-DT)
                 MOVE ZEROS            TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE SPACES           TO PI-AH-REFUND-METH
                 GO TO 0680-CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = '1')
                 MOVE ZEROS            TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE ER-3039          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0680-CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = '2' OR '3' OR '4')
                 MOVE ER-3037          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
      *          CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = '5')
                 MOVE ER-3038          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
      *          CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = ' ')
                 MOVE ER-2768          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              WHEN (CLOSED-AH-CLAIM)
                 AND (WS-AH-CANCEL-DATE <= WS-AH-PAID-THRU-DT)
                 MOVE ZEROS            TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE ER-2756          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 IF WS-TOT-LF-RFND > 0
                    MOVE ZEROS         TO HLREFNDO
                                          WS-TOT-LF-RFND
                                          PI-LF-REFUND-AMT
                    MOVE SPACES        TO PI-LF-REFUND-METH
                 END-IF
                 GO TO 0680-CONTINUE
      *       WHEN OTHER
      *          CONTINUE
           END-EVALUATE

           .
01307  0660-GET-REM-TERM.

01308      MOVE WS-CF-AH-OVERRIDE-L1   TO  CP-AH-OVERRIDE-CODE.
01309      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
01310      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01311      MOVE WS-AH-CANCEL-DATE      TO  CP-VALUATION-DT.
01312      MOVE WS-AH-CANCEL-DATE-ED   TO  HACANCO.
01313 *****MOVE WS-BENEFIT-DESCRIP     TO  HADESCO.
01314      MOVE WS-AH-KIND             TO  HAEDESCO.
01315      MOVE CM-STATE               TO  CP-STATE.
01316      MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
01317      MOVE 'A'                    TO  CP-BENEFIT-TYPE.
01318      MOVE WS-AH-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
01319      MOVE '2'                    TO  CP-PROCESS-TYPE.
01320      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01321      MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
01322      MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
01323      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
01324      MOVE WS-CF-CR-REM-TERM-CALC
01325                                  TO  CP-REM-TERM-METHOD.
01326
01327      IF WS-AH-CO-REM-TERM-CALC  IS GREATER THAN  '0'
01328          MOVE WS-AH-CO-REM-TERM-CALC
01329                                  TO  CP-REM-TERM-METHOD.
01330
01331      IF WS-AH-ST-REM-TERM-CALC  IS GREATER THAN  '0'
01332          MOVE WS-AH-ST-REM-TERM-CALC
01333                                  TO  CP-REM-TERM-METHOD.
01334
01335      IF WS-AH-AM-REM-TERM-CALC  IS GREATER THAN  '0'
01336          MOVE WS-AH-AM-REM-TERM-CALC
01337                                  TO  CP-REM-TERM-METHOD.
083005     IF (PI-COMPANY-ID = 'CID')
022608        AND (CP-STATE-STD-ABBRV = 'WI')
083005        MOVE '7'                 TO CP-REM-TERM-METHOD
022608        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
083005     END-IF

           IF (PI-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MO')
              AND (CM-CERT-EFF-DT >= X'9B41')
              AND (CM-CERT-EFF-DT <= X'A2FB')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
01338
01339      MOVE CM-AH-ORIG-TERM        TO  HATERMO
01340                                      CP-ORIGINAL-TERM
01341                                      CP-LOAN-TERM.
01342
01343      IF  NOT  CP-TERM-IS-DAYS
01344          IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
01345              MOVE CM-PMT-EXTENSION-DAYS
01346                                  TO  CP-TERM-OR-EXT-DAYS
01347          ELSE
01348              MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
01349
01350 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
01351      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
01352      MOVE '3'                    TO  WS-CF-RECORD-TYPE.
01353      MOVE CM-STATE               TO  WS-CF-ACCESS.
01354      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
01355
01356      EXEC CICS READ
01357          DATASET  (ELCNTL-ID)
01358          SET      (ADDRESS OF CONTROL-FILE)
01359          RIDFLD   (WS-CF-CONTROL-PRIMARY)
01360          RESP     (WS-RESPONSE)
01361      END-EXEC.
01362
01363      IF WS-RESP-NOTOPEN
01364          MOVE ER-2617            TO  EMI-ERROR
01365          GO TO 0320-INPUT-ERROR.
01366
01367      IF WS-RESP-NOTFND
01368          MOVE ZERO               TO CP-FREE-LOOK
01369      ELSE
01370          MOVE CF-ST-FREE-LOOK-PERIOD
01371                                  TO CP-FREE-LOOK.
01372
01373      PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT.
01374
01375      MOVE CP-REMAINING-TERM-1    TO  HAREMO.
01376
01377      IF CM-AH-CURRENT-STATUS  IS EQUAL TO  '8'
01378          IF CM-AH-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES
01379              MOVE ER-0683        TO  EMI-ERROR
01380              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01381
01382      IF CM-AH-CURRENT-STATUS  IS EQUAL TO  '6'  OR  '7'
01383          IF CM-AH-SETTLEMENT-DT  IS NOT EQUAL TO  LOW-VALUES
01384              MOVE ER-0684        TO  EMI-ERROR
01385              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01386
01387  0670-CONTINUE-AH.
01388      MOVE CM-AH-PREMIUM-AMT      TO  HAPREMO
01389                                      WS-TOT-AH-PREM
01390      MOVE CP-REMAINING-TERM-1    TO  HAREMO.
01391
01392      IF CP-REMAINING-TERM-1  IS GREATER THAN  CM-AH-ORIG-TERM
01393          MOVE CM-AH-ORIG-TERM    TO  HAREMO.
01394

01261      IF PI-AH-CANCEL-DATE = LOW-VALUES
01262         GO TO 0680-CONTINUE.


01395      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
01396      MOVE WS-AH-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
01397      MOVE WS-AH-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
01398
01399      IF WS-AH-ST-REFUND-CALC  IS GREATER THAN  ZERO
01400          MOVE WS-AH-ST-REFUND-CALC
01401                                  TO  CP-EARNING-METHOD.
01402
01403      IF WS-AH-FO-REFUND-CALC  IS GREATER THAN  ZERO
01404          MOVE WS-AH-FO-REFUND-CALC
01405                                  TO  CP-EARNING-METHOD.
01406
PEMMOD*    IF WS-AM-EARN-METHOD-A  IS GREATER THAN  ZERO
PEMMOD*        MOVE WS-AM-EARN-METHOD-A
PEMMOD*                                TO  CP-EARNING-METHOD.
01410
032912     if pi-company-id = 'AHL'
032912        MOVE CM-AH-CLASS-CD      TO CP-CLASS-CODE
032912        if cp-class-code = spaces
032912           move zeros            to cp-class-code
032912        end-if
032912     ELSE
CIDMOD        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
032912     END-IF
01412      MOVE CM-AH-BENEFIT-CD       TO  CP-BENEFIT-CD.
01413      MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
01414                                      CP-RATING-BENEFIT-AMT.
01415      IF CP-STATE-STD-ABBRV = 'OR'
01416          COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
01417                                          CM-AH-ORIG-TERM.
01418      MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
01419      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
01420      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
01421      MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
01422      MOVE CM-AH-DEV-PCT          TO  CP-RATE-DEV-PCT.
01423      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
01424
01425      MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
01426      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
01427      MOVE ' '                    TO  DC-OPTION-CODE.
01428
01429      PERFORM 9600-DATE-LINK.
01430
01431      IF CP-STATE-STD-ABBRV = 'OH'
CIDMOD        IF PI-COMPANY-ID NOT EQUAL 'NCL' AND 'CID'
01433          IF (CP-ORIGINAL-TERM  IS GREATER THAN  60)
01434            AND (DC-GREG-DATE-1-YMD  IS GREATER THAN  '831101')
01435            AND (CM-LF-BENEFIT-CD  IS NOT EQUAL TO  ZERO)
CIDMOD             MOVE '6'            TO  CP-EARNING-METHOD
CIDMOD         END-IF
CIDMOD        END-IF
CIDMOD        IF PI-COMPANY-ID = 'CID'
CIDMOD           IF CM-LF-BENEFIT-CD = (SPACES OR ZEROS OR
CIDMOD                           LOW-VALUES)
CIDMOD              IF CP-CRITICAL-PERIOD
CIDMOD                 MOVE '2'       TO CP-EARNING-METHOD
CIDMOD              ELSE
CIDMOD                 MOVE '6'       TO CP-EARNING-METHOD
CIDMOD              END-IF
CIDMOD           ELSE
CIDMOD              IF WS-CF-LF-COVERAGE-TYPE = 'L'
CIDMOD                 MOVE '2'       TO CP-EARNING-METHOD
CIDMOD              ELSE
CIDMOD                 IF ((CM-LF-ORIG-TERM > 60) AND
CIDMOD                    (CM-RATE-CLASS NOT = 'L '))
CIDMOD                               OR
CIDMOD                    (WS-LF-CO-EARNINGS-CALC = '5')
CIDMOD                    IF CP-CRITICAL-PERIOD
CIDMOD                       MOVE '2'  TO CP-EARNING-METHOD
CIDMOD                    ELSE
CIDMOD                       MOVE '6'  TO CP-EARNING-METHOD
CIDMOD                    END-IF
CIDMOD                 ELSE
CIDMOD                    MOVE '1'     TO CP-EARNING-METHOD
CIDMOD                 END-IF
CIDMOD              END-IF
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
01437
01438      IF CP-STATE-STD-ABBRV = 'VA'
01439        IF PI-COMPANY-ID NOT EQUAL 'NCL' AND 'CID'
01440          IF DC-GREG-DATE-1-YMD  IS GREATER THAN  '921231'
01441             IF CP-ORIGINAL-TERM  IS GREATER THAN  61
01442                 MOVE '6'            TO  CP-EARNING-METHOD
01443             ELSE
01444                 MOVE '1'            TO  CP-EARNING-METHOD.
01445
01446      IF CP-EARN-AS-NET-PAY
01447          IF CP-LOAN-APR NOT GREATER THAN ZEROS
01448             IF WS-CF-DEFAULT-APR GREATER THAN ZEROS
01449                 MOVE WS-CF-DEFAULT-APR
01450                                  TO  CP-LOAN-APR
01451                 MOVE ER-3780     TO  EMI-ERROR
01452                 PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01453             ELSE
01454                 MOVE ER-3781     TO  EMI-ERROR
01455                 PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
           IF PI-COMPANY-ID = 'DCC'
092705        IF (WS-AH-BEN-CATEGORY = 'G' OR 'L')
                 AND (CP-EARNING-METHOD NOT = 'G' AND 'D')
                 MOVE 'S'              TO CP-EARNING-METHOD
              END-IF
           END-IF

           MOVE PI-CANCEL-REASON       TO CP-CANCEL-REASON

           IF (PI-COMPANY-ID = 'DCC')
              AND (CP-EARNING-METHOD = 'D')
              MOVE +0                  TO WS-DDF-ADMIN-FEES
                                          WS-DDF-CSO-ADMIN-FEE
                                          WS-DDF-1ST-YR-TOT-EXP
                                          WS-DDF-COMM-AND-MFEE
              PERFORM VARYING S1 FROM +2 BY +1 UNTIL
                 S1 > +10
                 IF AM-COM-TYP (S1) = 'L' OR 'N' OR 'J' OR 'I'
                    IF (AM-A-COM (S1) NUMERIC)
                       AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M')
                       COMPUTE WS-COMM-PCT = (AM-A-COM (S1) * +1000)
                    ELSE
                       MOVE +0         TO WS-COMM-PCT C0
                       PERFORM 0740-GET-ERCTBL THRU 0740-EXIT
                       COMPUTE WS-COMM-PCT = WS-COMM-PCT * +1000
                    END-IF
                    IF AM-COM-TYP (S1) = 'L' OR 'N'
                       COMPUTE WS-DDF-ADMIN-FEES = WS-DDF-ADMIN-FEES
                          + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'N'
                       COMPUTE WS-DDF-CSO-ADMIN-FEE =
                          WS-DDF-CSO-ADMIN-FEE + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'J' OR 'L'
                       COMPUTE WS-DDF-1ST-YR-TOT-EXP
                          = WS-DDF-1ST-YR-TOT-EXP + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'I'
                       COMPUTE WS-DDF-COMM-AND-MFEE
                          = WS-DDF-COMM-AND-MFEE + WS-COMM-PCT
                    END-IF
                 END-IF
              END-PERFORM
              MOVE WS-DDF-CSO-ADMIN-FEE TO CP-DDF-CSO-ADMIN-FEE
              MOVE WS-DDF-ADMIN-FEES   TO CP-DDF-ADMIN-FEES
              COMPUTE WS-DDF-COMM-AND-MFEE = WS-DDF-COMM-AND-MFEE +
                 (CM-AH-PREMIUM-AMT - CM-AH-CLP - CM-ADDL-CLP)
           END-IF

           IF (PI-COMPANY-ID = 'DCC')
              AND (CP-EARNING-METHOD = 'D')
              AND (PI-CANCEL-REASON NOT = 'R')
              PERFORM 0730-GET-DDF-FACTORS
                                       THRU 0730-EXIT
              IF NOT PDEF-FOUND
                 MOVE ER-9999          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU 9799-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF

              MOVE PD-UEP-FACTOR (P1 P2 + 1)
                                       TO CP-DDF-LO-FACT
              MOVE PD-UEP-FACTOR (P1 P2)
                                       TO CP-DDF-HI-FACT
              MOVE WS-DDF-COMM-AND-MFEE TO CP-DDF-COMM-AND-MFEE
              MOVE CM-AH-CLP           TO CP-DDF-CLP
              MOVE PD-1ST-YR-ADMIN-ALLOW TO CP-DDF-YR1AF
              COMPUTE CP-1ST-YR-ALLOW = WS-DDF-1ST-YR-TOT-EXP
                 + PD-1ST-YR-ADMIN-ALLOW

              MOVE 'G'                 TO CP-DDF-SPEC-CALC
              IF PI-CLP-YN = 'Y'
PEMTST           MOVE 'C'              TO CP-DDF-SPEC-CALC
PEMTST           MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
                                          CP-DDF-CLP
PEMTST           MOVE ZEROS            TO CP-1ST-YR-ALLOW
              END-IF

              IF DD-IU-PRESENT
                 MOVE 'I'              TO CP-EARNING-METHOD
              END-IF

              MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
                                          CP-CLP-RATE-UP

              IF (CP-CALC-GROSS-FEE)
                 AND (CP-IU-RATE-UP NOT = ZEROS)
                 COMPUTE TEX-FACT-8 = 1 - ((CM-ADDL-CLP + CM-AH-CLP)
                    / CP-ORIGINAL-PREMIUM)
                 COMPUTE CP-IU-RATE-UP ROUNDED = CP-IU-RATE-UP
                    / (1 - TEX-FACT-8)
              END-IF

           END-IF

020816     if pi-company-id = 'VPP'
020816        IF PI-CLP-YN = 'Y'
020816           MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
020816                                    hapremo
020816                                    CP-DDF-CLP
020816        END-IF
020816     end-if

01457      PERFORM 0800-LINK-REFUND  THRU  0899-EXIT
01458
01459      MOVE ' '                    TO  WS-REFUND-SEARCH-SW.
01460
01461      PERFORM 1400-GET-REFUND-TYPE  THRU  1499-EXIT.
01462
01463      IF CP-ERROR-RATE-IS-ZERO
01464        OR CP-ERROR-RATE-NOT-FOUND
01465          MOVE ER-2740            TO  EMI-ERROR
01466          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01467
01468      IF CP-ERROR-RATE-FILE-NOTOPEN
01469          MOVE ER-2617            TO  EMI-ERROR
01470          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01471
01472      MOVE ZEROS                  TO  WS-CALC-REFUND.
01473      MOVE CP-CALC-REFUND         TO  HAREFNDO  WS-CALC-REFUND
01474                                      WS-TOT-AH-RFND
01475                                      PI-AH-REFUND-AMT.

032117     move cm-ah-comm-pct         to hacpcto
032117     compute hacamto rounded =
032117        cp-calc-refund * cm-ah-comm-pct

020816     if pi-company-id = 'VPP'
020816        if cp-calc-refund < cm-cancel-fee
020816           move cp-calc-refund   to cm-cancel-fee
020816        end-if
020816        if cp-calc-refund = cm-ah-premium-amt
020816           move zeros            to cm-cancel-fee
020816        end-if
020816        move cm-cancel-fee       to canfeeo
020816        compute refdueo =
020816           cp-calc-refund - cm-cancel-fee
020816     end-if

01476
01477      MOVE CP-REFUND-TYPE-USED    TO  PI-AH-REFUND-METH.
01478
01479      MOVE AL-UANON               TO  HACANCA.
01480
01481  0680-CONTINUE.
01482      IF BROWSE-STARTED
01483          PERFORM 1500-END-BROWSE  THRU  1599-EXIT
01484          MOVE SPACES             TO  WS-BROWSE-STARTED-SW.
01485
01486      IF CM-LF-BENEFIT-CD = '00'
01487          MOVE -1                 TO  HACANCL
01488      ELSE
01489          MOVE -1                 TO  HLCANCL.
01490
01491      ADD WS-TOT-LF-PREM    WS-TOT-AH-PREM GIVING
01492                            WS-TOT-PREM.
01493      ADD WS-TOT-LF-RFND    WS-TOT-AH-RFND GIVING
01494                            WS-TOT-RFND.
01495
01496      MOVE WS-TOT-PREM            TO  TOPREMO.
01497      MOVE WS-TOT-RFND            TO  TORFNDO.
01498
01499  0680-EXIT.
01500      EXIT.
01501  EJECT
01502  0700-LINK-REM-TERM SECTION.
01503      EXEC CICS LINK
01504          PROGRAM   (ELRTRM-ID)
01505          COMMAREA  (CALCULATION-PASS-AREA)
01506          LENGTH    (CP-COMM-LENGTH)
01507      END-EXEC.
01508
01509  0700-EXIT.
01510      EXIT.
01511  EJECT
01512  0710-LINK-REM-AMOUNT SECTION.
01513      EXEC CICS LINK
01514          PROGRAM   (ELRAMT-ID)
01515          COMMAREA  (CALCULATION-PASS-AREA)
01516          LENGTH    (CP-COMM-LENGTH)
01517      END-EXEC.
01518
01519  0710-EXIT.
01520      EXIT.

       0730-GET-DDF-FACTORS.

           MOVE ' '                    TO WS-PDEF-RECORD-SW

           MOVE PI-COMPANY-CD          TO ERPDEF-KEY
           MOVE CM-STATE               TO ERPDEF-STATE
010816     if cm-clp-state <> cm-state and spaces
010816                  and low-values and zeros
010816        move cm-clp-state        to erpdef-state
010816     end-if
           MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
           MOVE 'A'                    TO ERPDEF-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
           MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
           MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE

           EXEC CICS STARTBR
               DATASET  ('ERPDEF')
               RIDFLD   (ERPDEF-KEY)
               GTEQ
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT WS-RESP-NORMAL
              GO TO 0730-EXIT
           END-IF

           .
       0730-READNEXT.

           EXEC CICS READNEXT
              DATASET  ('ERPDEF')
              SET      (ADDRESS OF PRODUCT-MASTER)
              RIDFLD   (ERPDEF-KEY)
              RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT WS-RESP-NORMAL
              GO TO 0730-ENDBR
           END-IF

           IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
              IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
                 MOVE 'Y'              TO WS-PDEF-RECORD-SW
              ELSE
                 GO TO 0730-READNEXT
              END-IF
           ELSE
              GO TO 0730-ENDBR
           END-IF

           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
              (P1 > +8)
              OR (PD-PROD-CODE (P1) = 'I')
           END-PERFORM
           IF P1 < +9
              SET DD-IU-PRESENT        TO TRUE
           END-IF

           IF CM-LOAN-TERM = ZEROS
              MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
           END-IF

           IF PD-TRUNCATED = 'Y'
              MOVE CM-LOAN-TERM        TO WS-TERM
           ELSE
              MOVE CP-ORIGINAL-TERM    TO WS-TERM
           END-IF

           EVALUATE TRUE
              WHEN WS-TERM > +168
                 MOVE 15               TO P1
              WHEN WS-TERM > +156
                 MOVE 14               TO P1
              WHEN WS-TERM > +144
                 MOVE 13               TO P1
              WHEN WS-TERM > +132
                 MOVE 12               TO P1
              WHEN WS-TERM > +120
                 MOVE 11               TO P1
              WHEN WS-TERM > +108
                 MOVE 10               TO P1
              WHEN WS-TERM > +96
                 MOVE 9                TO P1
              WHEN WS-TERM > +84
                 MOVE 8                TO P1
              WHEN WS-TERM > +72
                 MOVE 7                TO P1
              WHEN WS-TERM > +60
                 MOVE 6                TO P1
              WHEN WS-TERM > +48
                 MOVE 5                TO P1
              WHEN WS-TERM > +36
                 MOVE 4                TO P1
              WHEN WS-TERM > +24
                 MOVE 3                TO P1
              WHEN WS-TERM > +12
                 MOVE 2                TO P1
              WHEN OTHER
                 MOVE 1                TO P1
           END-EVALUATE

           EVALUATE TRUE
      *       WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13)
      *          AND (DD-IU-PRESENT)
      *          MOVE 2                TO P2
      *       WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13
      *          MOVE 1                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +25
                 MOVE 2                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +37
                 MOVE 3                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +49
                 MOVE 4                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +61
                 MOVE 5                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +73
                 MOVE 6                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +85
                 MOVE 7                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +97
                 MOVE 8                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +109
                 MOVE 9                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +121
                 MOVE 10               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +133
                 MOVE 11               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +145
                 MOVE 12               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +157
                 MOVE 13               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +169
                 MOVE 14               TO P2
              WHEN OTHER
                 MOVE 15               TO P2
           END-EVALUATE

           .
       0730-ENDBR.

           EXEC CICS ENDBR
              DATASET  ('ERPDEF')
           END-EXEC

           .
       0730-EXIT.
           EXIT.

       0740-GET-ERCTBL.

           MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
           MOVE AM-A-COMA (S1)         TO CTBL-TABLE
           MOVE 'A'                    TO CTBL-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO CTBL-BEN-CODE
           MOVE CTBL-KEY               TO CTBL-KEY-SAVE

           PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
           IF WS-RESP-NORMAL
              PERFORM 0760-FIND-COMM   THRU 0760-EXIT
           ELSE
              MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
              MOVE AM-A-COMA (S1)         TO CTBL-TABLE
              MOVE 'A'                    TO CTBL-BEN-TYPE
              MOVE 'AA'                   TO CTBL-BEN-CODE
              MOVE CTBL-KEY               TO CTBL-KEY-SAVE
              
              PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
              IF WS-RESP-NORMAL
                 PERFORM 0760-FIND-COMM   THRU 0760-EXIT
              END-IF
           END-IF

           .
       0740-EXIT.
           EXIT.

       0750-READ-ERCTBL.

           EXEC CICS READ
                INTO    (COMM-TABLE-RECORD)
                DATASET ('ERCTBL')
                RIDFLD  (CTBL-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       0750-EXIT.
           EXIT.

       0760-FIND-COMM.

           PERFORM VARYING C1 FROM +1 BY +1 UNTIL
              ((CM-AH-BENEFIT-AMT * CM-AH-ORIG-TERM) <= CT-TBF (C1))
              OR (C1 > +3)
           END-PERFORM

           PERFORM VARYING C2 FROM +1 BY +1 UNTIL
              (CM-INSURED-ISSUE-AGE <= CT-AGE (C2))
              OR (C2 > +3)
           END-PERFORM

           PERFORM VARYING C3 FROM +1 BY +1 UNTIL
              (CM-AH-ORIG-TERM <= CT-TRM (C3))
              OR (C3 > +3)
           END-PERFORM

           IF C1 > +3
              MOVE +1                  TO C1
           END-IF
           IF C2 > +3
              MOVE +1                  TO C2
           END-IF
           IF C3 > +3
              MOVE +1                  TO C3
           END-IF

           IF C1 = +3
              MOVE +18                 TO C0
           ELSE
              IF C1 = +2
                 MOVE +9               TO C0
              END-IF
           END-IF

           IF C2 = +3
              ADD +6                   TO C0
           ELSE
              IF C2 = +2
                 ADD +3                TO C0
              END-IF
           END-IF

           ADD C3                      TO C0

           MOVE CT-RT (C0)             TO WS-COMM-PCT

           .
       0760-EXIT.
           EXIT.

01522  0800-LINK-REFUND SECTION.
01523      IF CP-AH
01524        IF PI-EARNING-METHOD-AH NOT = SPACES
01525          MOVE PI-EARNING-METHOD-AH   TO CP-EARNING-METHOD
01526         ELSE
01527          NEXT SENTENCE
01528       ELSE
01529        IF PI-EARNING-METHOD-LF NOT = SPACES
01530          MOVE PI-EARNING-METHOD-LF   TO CP-EARNING-METHOD.
01531
01532      EXEC CICS LINK
01533          PROGRAM   (ELRFND-ID)
01534          COMMAREA  (CALCULATION-PASS-AREA)
01535          LENGTH    (CP-COMM-LENGTH)
01536      END-EXEC.
01537
01538  0899-EXIT.
01539      EXIT.
01540  EJECT
01541  0900-READ-CERT-FILE  SECTION.
01542      EXEC CICS HANDLE CONDITION
01543          NOTFND  (0910-NOT-FOUND)
01544      END-EXEC.
01545
01546      EXEC CICS READ
01547          DATASET  (ELCERT-ID)
01548          RIDFLD   (WS-CM-CONTROL-PRIMARY)
01549          SET      (ADDRESS OF CERTIFICATE-MASTER)
01550      END-EXEC.
01551
01552 *    SERVICE RELOAD CERTIFICATE-MASTER.
01553
01554      GO TO 0999-EXIT.
01555
01556  0910-NOT-FOUND.
01557      MOVE 'N'                    TO  WS-CERT-RECORD-SW.
01558
01559  0999-EXIT.
01560      EXIT.
01561  EJECT
01562  1000-READ-CONTROL  SECTION.
01563      EXEC CICS HANDLE CONDITION
01564          NOTFND   (1010-NOTFND)
01565          NOTOPEN  (1020-NOTOPEN)
01566      END-EXEC.
01567
01568      EXEC CICS READ
01569          DATASET  (ELCNTL-ID)
01570          SET      (ADDRESS OF CONTROL-FILE)
01571          RIDFLD   (WS-CF-CONTROL-PRIMARY)
01572          GTEQ
01573      END-EXEC.
01574
01575 *    SERVICE RELOAD CONTROL-FILE.
01576
01577      GO TO 1099-EXIT.
01578
01579  1010-NOTFND.
01580      MOVE 'N'                    TO  WS-CNTL-RECORD-SW.
01581
01582      GO TO 1099-EXIT.
01583
01584  1020-NOTOPEN.
01585      MOVE ER-2617                TO  EMI-ERROR.
01586
01587      GO TO 0320-INPUT-ERROR.
01588
01589  1099-EXIT.
01590      EXIT.
01591  EJECT
01592  1100-FIND-BENEFIT-IN-STATE  SECTION.
01593      MOVE 'N'                    TO  BEN-SEARCH-SW.
01594
01595      PERFORM 1110-BENEFIT-DUMMY  THRU  1119-EXIT
01596          VARYING  SUB3  FROM  1  BY  1
01597              UNTIL  ((SUB3  IS GREATER THAN  50)
01598                OR ((CF-ST-BENEFIT-CD (SUB3)
01599                       IS EQUAL TO  WS-BEN-CD)
01600                AND (WS-LOOKUP-TYPE
01601                       IS EQUAL TO  CF-ST-BENEFIT-KIND (SUB3)))).
01602
01603      IF SUB3  IS NOT EQUAL TO  51
01604          MOVE 'Y'                TO  BEN-SEARCH-SW.
01605
01606      GO TO 1199-EXIT.
01607
01608  1110-BENEFIT-DUMMY.
01609
01610  1119-EXIT.
01611      EXIT.
01612
01613  1199-EXIT.
01614      EXIT.
01615  EJECT
01616  1200-START-ACCOUNT-MASTER  SECTION.
01617      EXEC CICS HANDLE CONDITION
01618          NOTFND   (1290-ACCT-NOT-FOUND)
01619          ENDFILE  (1290-ACCT-NOT-FOUND)
01620      END-EXEC.
01621
01622      MOVE ' '                    TO  WS-ACCT-RECORD-SW.
01623
01624      EXEC CICS STARTBR
01625          DATASET  (ERACCT-ID)
01626          RIDFLD   (WS-AM-CONTROL-PRIMARY)
01627          GTEQ
01628      END-EXEC.
01629
01630      MOVE 'Y'                    TO  WS-BROWSE-STARTED-SW.
01631      MOVE 'Y'                    TO  WS-ACCT-RECORD-SW.
01632
01633      GO TO 1299-EXIT.
01634
01635  1290-ACCT-NOT-FOUND.
01636      MOVE ' '                    TO  WS-ACCT-RECORD-SW.
01637
01638  1299-EXIT.
01639      EXIT.
01640  EJECT
01641  1300-READ-ACCOUNT-MASTER  SECTION.
01642      EXEC CICS HANDLE CONDITION
01643          NOTFND   (0580-ACCT-NOT-FOUND)
01644          ENDFILE  (0580-ACCT-NOT-FOUND)
01645      END-EXEC.
01646
01647      EXEC CICS READNEXT
01648          DATASET  (ERACCT-ID)
01649          SET      (ADDRESS OF ACCOUNT-MASTER)
01650          RIDFLD   (WS-AM-CONTROL-PRIMARY)
01651      END-EXEC.
01652
01653 *    SERVICE RELOAD ACCOUNT-MASTER.
01654
01655  1399-EXIT.
01656      EXIT.
01657  EJECT
01658  1400-GET-REFUND-TYPE  SECTION.
01659      IF LIFE-REFUND-SEARCH
01660          NEXT SENTENCE
01661      ELSE
01662          GO TO 1410-AH-REFUND-TYPE.
01663
01664      IF CP-REFUND-TYPE-USED  IS EQUAL TO  SPACE
01665          MOVE SPACES             TO  HLCALCO
01666                                      HLCAL1O
01667          GO TO 1499-EXIT.
01668
01669      MOVE CP-REFUND-TYPE-USED    TO  HLCAL1O.
01670
01671      EVALUATE TRUE
              WHEN CP-R-AS-REPOSSESSION
                 MOVE 'REPOSSESSION'     TO  HLCALCO
01672         WHEN CP-R-AS-R78
01673            MOVE 'RULE 78'          TO  HLCALCO
01674         WHEN  CP-R-AS-PRORATA
01675            MOVE 'PRO RATA'         TO  HLCALCO
01676         WHEN CP-R-AS-CALIF
01677            MOVE 'CALIF'            TO  HLCALCO
01678         WHEN CP-R-AS-TEXAS
01679            MOVE 'IRREG'            TO  HLCALCO
01680         WHEN CP-REFUND-TYPE-USED IS EQUAL TO 'S'
01681            MOVE 'UTAH'             TO  HLCALCO
01682         WHEN CP-R-AS-FARM-PLAN
01683            MOVE 'FARM PLAN'        TO  HLCALCO
01684         WHEN CP-R-AS-NET-PAY
01685            MOVE 'NET PAY'          TO  HLCALCO
01686         WHEN CP-R-AS-ANTICIPATION
01687            MOVE 'ANTICIPATION'     TO  HLCALCO
01688         WHEN CP-R-AS-MEAN
01689            MOVE 'MEAN'             TO  HLCALCO
01690         WHEN CP-R-AS-SUM-OF-DIGITS
01691            MOVE 'SUM OF DIGIT'     TO  HLCALCO
              WHEN OTHER
                 MOVE 'UNDEFINED'        TO  HLCALCO
01692      END-EVALUATE.
01693
01694      GO TO 1499-EXIT.
01695
01696  1410-AH-REFUND-TYPE.
01697      IF CP-REFUND-TYPE-USED  IS EQUAL TO  SPACE
01698          MOVE SPACES             TO  HACALCO
01699                                      HACAL1O
01700          GO TO 1499-EXIT.
01701
01702      MOVE CP-REFUND-TYPE-USED    TO  HACAL1O.
01703
01704      EVALUATE TRUE
              WHEN CP-R-AS-REPOSSESSION
                 MOVE 'REPOSSESSION'     TO HACALCO
01705         WHEN CP-R-AS-R78
01706            MOVE 'RULE 78'          TO HACALCO
01707         WHEN CP-R-AS-PRORATA
01708            MOVE 'PRO RATA'         TO HACALCO
01709         WHEN CP-REFUND-TYPE-USED = '3'
01710            MOVE 'CALIF'            TO HACALCO
01711         WHEN CP-R-AS-TEXAS
01712            MOVE 'IRREG'            TO HACALCO
01713         WHEN CP-R-AS-FARM-PLAN
01714            MOVE 'FARM PLAN'        TO HACALCO
01715         WHEN CP-R-AS-NET-PAY
01716            MOVE 'NET PAY'          TO HACALCO
01717         WHEN CP-R-AS-ANTICIPATION
01718            MOVE 'ANTICIPATION'     TO HACALCO
01719         WHEN CP-R-AS-MEAN
01720            MOVE 'MEAN'             TO HACALCO
01721         WHEN CP-R-AS-SUM-OF-DIGITS
01722            MOVE 'SUM OF DIGIT'     TO HACALCO
              WHEN CP-GAP-ACTUARIAL
                 MOVE 'SP ACTUARIAL'     TO HACALCO
              WHEN CP-R-AS-SPP-DDF
                 MOVE 'SPEC DDF'         TO HACALCO
              WHEN CP-R-AS-SPP-DDF-IU
                 MOVE 'SPEC DDF IU'      TO HACALCO
              WHEN OTHER
                 MOVE 'UNKNOWN'          TO HLCALCO
01723      END-EVALUATE

           .
01726  1499-EXIT.
01727      EXIT.
01728  EJECT
01729  1500-END-BROWSE  SECTION.
01730      IF BROWSE-STARTED
01731          EXEC CICS ENDBR
01732              DATASET  (ERACCT-ID)
01733          END-EXEC.
01734
01735  1599-EXIT.
01736      EXIT.
01737  EJECT
01738  1600-LOCATE-BENEFIT  SECTION.
01739      EXEC CICS HANDLE CONDITION
01740          NOTFND  (1699-EXIT)
01741      END-EXEC.
01742
01743      MOVE SPACES                 TO  WS-KIND.
01744      MOVE ZERO                   TO  WS-NOT-FOUND.
01745      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
01746      MOVE SPACES                 TO  WS-CF-STATE.
01747      MOVE WS-BENEFIT-NO          TO  WS-CF-BENEFIT-NO.
01748
01749      EXEC CICS READ
01750          DATASET  (ELCNTL-ID)
01751          RIDFLD   (WS-CF-CONTROL-PRIMARY)
01752          SET      (ADDRESS OF CONTROL-FILE)
01753          GTEQ
01754      END-EXEC.
01755
01756 *    SERVICE RELOAD CONTROL-FILE.
01757
01758      IF WS-CF-COMPANY-ID  IS NOT EQUAL TO  CF-COMPANY-ID
01759        OR WS-CF-RECORD-TYPE  IS NOT EQUAL TO  CF-RECORD-TYPE
01760          GO TO 1699-EXIT.
01761
01762      MOVE +1                     TO  WS-INDEX.
01763
01764  1610-LOOKUP-BENEFIT.
01765      IF WS-BENEFIT-NO  IS EQUAL TO  CF-BENEFIT-CODE (WS-INDEX)
01766          MOVE CF-BENEFIT-ALPHA (WS-INDEX)
01767                                  TO  WS-KIND
01768          MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
01769                                  TO  WS-CALC-CD
01770          MOVE CF-BENEFIT-DESCRIP (WS-INDEX)
01771                                  TO  WS-BENEFIT-DESCRIP
01772          MOVE +1                 TO  WS-NOT-FOUND
01773          GO TO 1699-EXIT.
01774
01775      IF CF-BENEFIT-CODE (WS-INDEX)
01776              IS NOT LESS THAN  CF-HI-BEN-IN-REC
01777          GO TO 1699-EXIT.
01778
01779      IF WS-INDEX  IS LESS THAN  +8
01780          ADD +1                  TO  WS-INDEX
01781          GO TO 1610-LOOKUP-BENEFIT.
01782
01783  1699-EXIT.
01784      EXIT.
01785  EJECT
01786  1700-WRITE-CERT-NOTE      SECTION.

01788      IF NOT MODIFY-CERT-CAP
01789          MOVE 'UPDATE'           TO  SM-READ
01790          PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
01791          MOVE ER-0070            TO  EMI-ERROR
01792          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01793          GO TO 8200-SEND-DATAONLY.
01794
01795 **MOVE CANCEL QUOTE NOTE INFO
01796      IF PI-LF-REFUND-AMT NUMERIC
01797         MOVE PI-LF-REFUND-AMT    TO WS-CN-LIFE-AMT-RF
01798         MOVE PI-LF-REFUND-METH   TO WS-CN-LF-REF-METH
01799      ELSE
01800         MOVE ZERO                TO WS-CN-LIFE-AMT-RF
01801                                     PI-LF-REFUND-AMT
01802         MOVE SPACE               TO WS-CN-LF-REF-METH
01803                                     PI-LF-REFUND-METH.
01804
01805      IF PI-AH-REFUND-AMT NUMERIC
01806         MOVE PI-AH-REFUND-AMT    TO WS-CN-AH-AMT-RF
01807         MOVE PI-AH-REFUND-METH   TO WS-CN-AH-REF-METH
01808      ELSE
01809         MOVE ZERO                TO WS-CN-AH-AMT-RF
01810                                     PI-AH-REFUND-AMT
01811         MOVE SPACE               TO WS-CN-AH-REF-METH
01812                                     PI-AH-REFUND-METH.
01813
01814      COMPUTE PI-TOTAL-REFUND-AMT = PI-LF-REFUND-AMT +
01815                                    PI-AH-REFUND-AMT.
01816
01817      MOVE PI-TOTAL-REFUND-AMT    TO WS-CN-TOTAL-RF.
01818
01819      MOVE SAVE-DATE              TO WS-CN-DT-QUOTED.
01820      MOVE PI-PROCESSOR-ID        TO WS-CN-PROCESSOR-ID.
01821      IF PI-LF-CANCEL-DATE-ED NOT EQUAL SPACE
01822         MOVE PI-LF-CANCEL-DATE-ED
01823                                  TO WS-CN-CANCEL-DT
01824      ELSE
01825         MOVE PI-AH-CANCEL-DATE-ED
01826                                  TO WS-CN-CANCEL-DT
           END-IF

           MOVE 'CZ'                   TO CERT-NOTE-FILE
           MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD
           MOVE PI-CARRIER             TO CZ-CARRIER
           MOVE PI-GROUPING            TO CZ-GROUPING
           MOVE PI-STATE               TO CZ-STATE
           MOVE PI-ACCOUNT             TO CZ-ACCOUNT
           MOVE PI-CERT-EFF-DT         TO CZ-CERT-EFF-DT
           MOVE PI-CERT-PRIME          TO CZ-CERT-PRIME
           MOVE PI-CERT-SFX            TO CZ-CERT-SFX
           MOVE '1'                    TO CZ-RECORD-TYPE
           MOVE +1                     TO CZ-NOTE-SEQUENCE
     
           MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER
           MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO CZ-LAST-MAINT-DT
           MOVE WS-CAN-QUOTE-NOTE      TO CZ-NOTE-INFORMATION

           MOVE ' '                    TO WS-STOP-SW
           PERFORM UNTIL I-SAY-TO-STOP
              EXEC CICS WRITE
                  FROM      (CERT-NOTE-FILE)
                  DATASET   (ERNOTE-ID)
                  RIDFLD    (CZ-CONTROL-PRIMARY)
                  RESP      (WS-RESPONSE)
              END-EXEC
              IF WS-RESP-DUPREC
                 ADD +1                TO CZ-NOTE-SEQUENCE
              ELSE
                 SET I-SAY-TO-STOP     TO TRUE
              END-IF
           END-PERFORM

           PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
           IF WS-RESP-NORMAL
              EVALUATE CM-NOTE-SW
                 WHEN '1'
                 WHEN '3'
                 WHEN '5'
                 WHEN '7'
                    SET NO-CERT-RW     TO TRUE
                 WHEN ' '
                    MOVE '1'           TO CM-NOTE-SW
                 WHEN '2'
                    MOVE '3'           TO CM-NOTE-SW
                 WHEN '4'
                    MOVE '5'           TO CM-NOTE-SW
                 WHEN '6'
                    MOVE '7'           TO CM-NOTE-SW
              END-EVALUATE
           END-IF
           IF NOT NO-CERT-RW
              PERFORM 1810-REWRITE-ELCERT
                                       THRU 1810-EXIT
           ELSE
              EXEC CICS UNLOCK
                 DATASET    (ELCERT-ID)
              END-EXEC
           END-IF

01919      MOVE ER-8160                TO EMI-ERROR.
01920      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01921      GO TO 8200-SEND-DATAONLY.
01922
01923  1799-EXIT.
01924      EXIT.

081606 1800-READ-ELCERT-UPDATE.

081606     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
081606     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
081606     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
081606     MOVE PI-STATE               TO  WS-CM-STATE.
081606     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
081606     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
081606     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
081606     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.

081606     EXEC CICS READ
081606         UPDATE
081606         DATASET  (ELCERT-ID)
081606         RIDFLD   (WS-CM-CONTROL-PRIMARY)
081606         SET      (ADDRESS OF CERTIFICATE-MASTER)
081606         RESP     (WS-RESPONSE)
081606     END-EXEC

081606     .
081606 1800-EXIT.
081606     EXIT.

081606 1810-REWRITE-ELCERT.

081606     EXEC CICS REWRITE
081606         FROM      (CERTIFICATE-MASTER)
081606         DATASET   (ELCERT-ID)
081606         RESP     (WS-RESPONSE)
081606     END-EXEC.

081606     .
081606 1810-EXIT.
081606     EXIT.


       4000-GEN-CANCEL-TRANS.
       
112612     IF PI-LF-REFUND-AMT NOT NUMERIC
112612         MOVE +0 TO PI-LF-REFUND-AMT
112612     END-IF
112612     IF PI-AH-REFUND-AMT NOT NUMERIC
112612         MOVE +0 TO PI-AH-REFUND-AMT
112612     END-IF
112612       
062712     PERFORM 4400-ADD-ORIG-REC THRU 4400-EXIT
062712
           MOVE '2'                    TO CG-OPTION-CODE
           MOVE PI-COMPANY-ID          TO CG-COMPANY-ID
           MOVE PI-PROCESSOR-ID        TO CG-PROC-ID
           MOVE PI-CR-MONTH-END-DT     TO CG-MONTH-END-DT
           MOVE PI-COMPANY-CD          TO CG-CERT-COMPANY-CD
           MOVE PI-CARRIER             TO CG-CERT-CARRIER
           MOVE PI-GROUPING            TO CG-CERT-GROUP
           MOVE PI-STATE               TO CG-CERT-STATE
           MOVE PI-ACCOUNT             TO CG-CERT-ACCOUNT
           MOVE PI-CERT-EFF-DT         TO CG-CERT-EFF-DT
           MOVE PI-CERT-NO             TO CG-CERT-CERT-NO
           MOVE SAVE-BIN-DATE          TO CG-CURRENT-DT
                                       
           MOVE ZEROS                  TO CG-LF-BENCD
                                          CG-AH-BENCD
                                          CG-LF-CAN-AMT
                                          CG-AH-CAN-AMT
           MOVE LOW-VALUES             TO CG-LF-CAN-DT
                                          CG-AH-CAN-DT
           MOVE SPACES                 TO CG-BATCH-NO
070622     MOVE PI-CANCEL-REASON       TO CG-DCC-REASON-CD

062712     IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
062712       AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
062712        IF (PI-LF-CANCEL-DATE NOT = LOW-VALUES AND SPACES)
062712           MOVE CM-LF-BENEFIT-CD  TO CG-LF-BENCD
062712           MOVE PI-LF-REFUND-AMT  TO CG-LF-CAN-AMT
062712           MOVE PI-LF-CANCEL-DATE TO CG-LF-CAN-DT
062712        END-IF
062712     END-IF
062712     IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
062712       AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
062712        IF (PI-AH-CANCEL-DATE NOT = LOW-VALUES AND SPACES)
062712           MOVE CM-AH-BENEFIT-CD  TO CG-AH-BENCD
062712           MOVE PI-AH-REFUND-AMT  TO CG-AH-CAN-AMT
062712           MOVE PI-AH-CANCEL-DATE TO CG-AH-CAN-DT
062712        END-IF
062712     END-IF
           
           EXEC CICS LINK
               PROGRAM  ('ELCANC')
               COMMAREA (CANCEL-GEN-PASS-AREA)
           END-EXEC
           IF CG-SUCCESS
              CONTINUE
           ELSE
010412        MOVE SPACES              TO EMI-MESSAGE-AREA (1)
              move '3'                 to emi-switch1
              MOVE CG-ERROR-CODE       TO EMI-TEXT-VARIABLE (1)
010412        EVALUATE TRUE
010412           WHEN CG-DATE-ERROR
010412             MOVE WS-01-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-CERT-NOT-FOUND
010412             MOVE WS-02-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-AMOUNT-ERROR
010412             MOVE WS-04-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-OPTION-ERROR
010412             MOVE WS-05-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-PREV-CAN
010412             MOVE WS-06-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-INVALID-DATA
010412             MOVE WS-07-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-NO-ACCT-MSTR
010412             MOVE WS-08-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-SFX-A-EXIST
010412             MOVE WS-09-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-MISC-ERROR
010412             MOVE WS-99-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN OTHER
010412             move ' error - elcanc - return '
                                       to emi-error-text (1) (12:25)
010412        END-EVALUATE
              go to 8200-send-dataonly
           end-if

           .
       4000-exit.
           exit.
070622 4100-CONNECT-TO-DB.
070622
070622     IF SVR > SPACES
070622        CONTINUE
070622     ELSE
070622        MOVE 'NTCSO2_LOGIC'         TO SVR
070622        MOVE 'sa'                   TO USR
070622        MOVE 'ntcso2'               TO PASS
070622     END-IF
070622
070622     STRING
070622         USR DELIMITED SPACE
070622         "." DELIMITED SIZE
070622         PASS DELIMITED SPACE INTO USR-PASS
070622     END-STRING
070622
070622     EXEC SQL
070622        CONNECT TO :SVR USER :USR-PASS
070622     END-EXEC
070622
070622     IF SQLCODE NOT = 0
070622        DISPLAY "ERROR: CANNOT CONNECT "
070622        DISPLAY SQLCODE
070622        DISPLAY SQLERRMC
070622        GO TO 4100-EXIT
070622     END-IF
070622
070622     .
070622 4100-EXIT.
070622     EXIT.
070622 4300-DISCONNECT.
070622
070622     EXEC SQL
070622        DISCONNECT
070622     END-EXEC
070622     .
070622 4300-EXIT.
070622     EXIT.


062712
062712 4400-ADD-ORIG-REC.
062712
062712     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
062712     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
062712     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
062712     MOVE PI-STATE               TO  WS-CM-STATE.
062712     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
062712     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
062712     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
062712     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
062712
062712     EXEC CICS READ
062712         DATASET  (ELCERT-ID)
062712         RIDFLD   (WS-CM-CONTROL-PRIMARY)
062712         SET      (ADDRESS OF CERTIFICATE-MASTER)
062712         RESP     (WS-RESPONSE)
062712     END-EXEC
062712     IF WS-RESP-NORMAL
062712         CONTINUE
062712     ELSE
062712        MOVE ER-0142                TO EMI-ERROR
062712        PERFORM 9700-ERROR-FORMAT THRU 9799-EXIT
062712        GO TO 8100-SEND-INITIAL-MAP
062712     END-IF
062712
062712******************************************************************
062712*            A D D   O R I G   C E R T   I N F O                 *
062712******************************************************************
062712
062712     display ' made it to add orig cert ' WS-CM-CONTROL-PRIMARY
062712
062712     MOVE WS-CM-CONTROL-PRIMARY  TO WS-MA-CONTROL-PRIMARY
062712     MOVE ' '                    TO WS-ERMAIL-SW
062712
062712     EXEC CICS READ
062712        DATASET   ('ERMAIL')
062712        SET       (ADDRESS OF MAILING-DATA)
062712        RIDFLD    (WS-MA-CONTROL-PRIMARY)
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712
062712     IF WS-RESP-NORMAL
062712        SET ERMAIL-FOUND TO TRUE
062712     END-IF
121712
121712     MOVE WS-CM-CONTROL-PRIMARY TO ELCRTT-PRIMARY
121712     MOVE 'C'                   TO ELCRTT-REC-TYPE
121712     MOVE +0                    TO WS-CERT-TRL-REC-NOT-FOUND
121712
121712     EXEC CICS READ
121712          DATASET  (ELCRTT-ID)
121712          RIDFLD   (ELCRTT-KEY)
121712          SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121712          RESP     (WS-RESPONSE)
121712     END-EXEC
121712
121712     IF NOT WS-RESP-NORMAL
121712        MOVE +1               TO WS-CERT-TRL-REC-NOT-FOUND
121712     END-IF
062712
062712     MOVE WS-CM-CONTROL-PRIMARY TO ELCRTO-KEY (1:33)
062712     MOVE 'I'                 TO ELCRTO-RECORD-TYPE
062712     MOVE +0                  TO ELCRTO-SEQ-NO
062712
062712     EXEC CICS READ
062712        DATASET   ('ELCRTO')
062712        SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712        RIDFLD    (ELCRTO-KEY)
062712        GTEQ
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712     
062712     IF WS-RESP-NORMAL
062712        AND (OC-CONTROL-PRIMARY (1:33) =
062712                 WS-CM-CONTROL-PRIMARY)
062712        AND (OC-RECORD-TYPE = 'I')
062712        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
062712           EXEC CICS READ
062712              DATASET   ('ELCRTO')
062712              SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712              RIDFLD    (OC-CONTROL-PRIMARY)
062712              UPDATE
062712              RESP      (WS-RESPONSE)
062712           END-EXEC
062712           MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
062712           MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
062712           MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
062712           MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
062712           MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
062712           MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
062712           MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
062712           MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
072312           IF ((PI-LF-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312            (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES)) 
072312               MOVE SPACES             TO OC-LF-BENCD
072312               MOVE ZEROS              TO OC-LF-TERM         
072312                                          OC-LF-BEN-AMT      
072312                                          OC-LF-PRM-AMT      
072312                                          OC-LF-ALT-BEN-AMT  
072312                                          OC-LF-ALT-PRM-AMT  
072312                                          OC-LF-COMM-PCT     
072312                                          OC-LF-CANCEL-AMT
072312               MOVE LOW-VALUES         TO OC-LF-EXP-DT       
072312               MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
072312           ELSE
062712               MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD        
062712               MOVE CM-LF-ORIG-TERM        TO OC-LF-TERM         
062712               MOVE CM-LF-BENEFIT-AMT      TO OC-LF-BEN-AMT      
062712               MOVE CM-LF-PREMIUM-AMT      TO OC-LF-PRM-AMT      
062712               MOVE CM-LF-ALT-BENEFIT-AMT  TO OC-LF-ALT-BEN-AMT  
062712               MOVE CM-LF-ALT-PREMIUM-AMT  TO OC-LF-ALT-PRM-AMT  
062712               MOVE CM-LF-LOAN-EXPIRE-DT   TO OC-LF-EXP-DT       
062712               MOVE CM-LIFE-COMM-PCT       TO OC-LF-COMM-PCT     
071712               MOVE PI-LF-REFUND-AMT       TO OC-LF-CANCEL-AMT
071712               MOVE PI-LF-CANCEL-DATE      TO OC-LF-CANCEL-DT
072312           END-IF
071712           MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT   
072312           IF ((PI-AH-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312            (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES)) 
072312               MOVE SPACES             TO OC-AH-BENCD
072312               MOVE ZEROS              TO OC-AH-TERM         
072312                                          OC-AH-BEN-AMT      
072312                                          OC-AH-PRM-AMT      
072312                                          OC-AH-COMM-PCT     
072312                                          OC-AH-CANCEL-AMT
072312                                          OC-AH-CP
072312               MOVE LOW-VALUES         TO OC-AH-EXP-DT       
072312               MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
072312           ELSE
062712               MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD        
062712               MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM         
062712               MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT      
062712               MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT      
062712               MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
062712               MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT     
062712               MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP           
071712               MOVE PI-AH-REFUND-AMT   TO OC-AH-CANCEL-AMT
071712               MOVE PI-AH-CANCEL-DATE  TO OC-AH-CANCEL-DT
072312           END-IF
071712           MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712           MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413           MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712           MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712           MOVE EIBDATE                TO DC-JULIAN-YYDDD
062712           MOVE '5'                    TO DC-OPTION-CODE
062712           PERFORM 9600-DATE-LINK
062712           MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
062712           IF ERMAIL-FOUND
062712               MOVE MA-CRED-BENE-NAME
062712                           TO OC-CRED-BENE-NAME
062712           END-IF
121712           IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
121712               MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                                  OC-INS-AGE-DEFAULT-FLAG
121712               MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                                  OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
121712
062712           EXEC CICS REWRITE
062712              DATASET   ('ELCRTO')
062712              FROM      (ORIGINAL-CERTIFICATE)
062712              RESP      (WS-RESPONSE)
062712           END-EXEC
062712           IF NOT WS-RESP-NORMAL
062712              MOVE ER-3830          TO EMI-ERROR
062712              PERFORM 9700-ERROR-FORMAT
062712                                 THRU 9799-EXIT
062712              GO TO 8200-SEND-DATAONLY
062712           END-IF
062712           
062712           GO TO 4400-EXIT
062712
062712        ELSE
062712           SUBTRACT +1 FROM OC-KEY-SEQ-NO
062712        END-IF
062712     ELSE
062712        MOVE SPACES              TO ORIGINAL-CERTIFICATE
062712        MOVE 'OC'                TO OC-RECORD-ID
062712        MOVE WS-CM-CONTROL-PRIMARY TO OC-CONTROL-PRIMARY (1:33)
062712        MOVE 'I'                 TO OC-RECORD-TYPE
062712        MOVE +4096               TO OC-KEY-SEQ-NO
062712     END-IF
062712
062712     MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
062712     MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
062712     MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
062712     MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
062712     MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
062712     MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
062712     MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
062712     MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
072312     IF ((PI-LF-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312      (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES)) 
072312         MOVE SPACES             TO OC-LF-BENCD
072312         MOVE ZEROS              TO OC-LF-TERM         
072312                                    OC-LF-BEN-AMT      
072312                                    OC-LF-PRM-AMT      
072312                                    OC-LF-ALT-BEN-AMT  
072312                                    OC-LF-ALT-PRM-AMT  
072312                                    OC-LF-COMM-PCT     
072312                                    OC-LF-CANCEL-AMT
072312         MOVE LOW-VALUES         TO OC-LF-EXP-DT       
072312         MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
072312     ELSE
062712         MOVE CM-LF-BENEFIT-CD   TO OC-LF-BENCD        
062712         MOVE CM-LF-ORIG-TERM    TO OC-LF-TERM         
062712         MOVE CM-LF-BENEFIT-AMT  TO OC-LF-BEN-AMT      
062712         MOVE CM-LF-PREMIUM-AMT  TO OC-LF-PRM-AMT      
062712         MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
062712         MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
062712         MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT       
062712         MOVE CM-LIFE-COMM-PCT   TO OC-LF-COMM-PCT     
071712         MOVE PI-LF-REFUND-AMT   TO OC-LF-CANCEL-AMT
071712         MOVE PI-LF-CANCEL-DATE  TO OC-LF-CANCEL-DT
072312     END-IF
071712     MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
072312     IF ((PI-AH-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312      (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES)) 
072312         MOVE SPACES             TO OC-AH-BENCD
072312         MOVE ZEROS              TO OC-AH-TERM         
072312                                    OC-AH-BEN-AMT      
072312                                    OC-AH-PRM-AMT      
072312                                    OC-AH-COMM-PCT     
072312                                    OC-AH-CANCEL-AMT
072312                                    OC-AH-CP
072312         MOVE LOW-VALUES         TO OC-AH-EXP-DT       
072312         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
072312     ELSE
062712         MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD        
062712         MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM         
062712         MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT      
062712         MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT      
062712         MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
062712         MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT     
062712         MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP           
062712         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT    
071712         MOVE PI-AH-REFUND-AMT   TO OC-AH-CANCEL-AMT
071712         MOVE PI-AH-CANCEL-DATE  TO OC-AH-CANCEL-DT
072312     END-IF
071712     MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712     MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413     MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712     MOVE EIBDATE                TO DC-JULIAN-YYDDD
062712     MOVE '5'                    TO DC-OPTION-CODE
062712     PERFORM 9600-DATE-LINK
062712     MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
062712     IF ERMAIL-FOUND
062712         MOVE MA-CRED-BENE-NAME
062712                     TO OC-CRED-BENE-NAME
062712     END-IF
121712     IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
121712         MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                            OC-INS-AGE-DEFAULT-FLAG
121712         MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                            OC-JNT-AGE-DEFAULT-FLAG
121712     END-IF
121712
062712     MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
062712     .
062712 4400-WRITE-ELCRTO.
062712
062712     EXEC CICS WRITE
062712        DATASET   ('ELCRTO')
062712        FROM      (ORIGINAL-CERTIFICATE)
062712        RIDFLD    (OC-CONTROL-PRIMARY)
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712
062712    IF WS-RESP-DUPKEY OR WS-RESP-DUPREC
062712        SUBTRACT +1    FROM OC-KEY-SEQ-NO
062712        GO TO 4400-WRITE-ELCRTO
062712    ELSE
062712        IF NOT WS-RESP-NORMAL
062712           MOVE ER-3830          TO EMI-ERROR
062712           PERFORM 9700-ERROR-FORMAT
062712                                 THRU 9799-EXIT
062712           GO TO 8200-SEND-DATAONLY
062712        END-IF
062712    END-IF
062712
062712     .
062712 4400-EXIT.
062712    EXIT.
062712
062712

01926  5000-GET-LF-AM-REM-TERM.
01927      MOVE +1                     TO SUB3.
01928
01929  5100-GET-LF-AM-REM-TERM.
01930      IF AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1
01931         IF CM-LF-BENEFIT-CD = AM-BENEFIT-CODE (SUB3)
01932            MOVE AM-ALLOWABLE-BENEFITS (SUB3)
01933                          TO WS-SAVE-ALLOWABLE-BENEFIT
01934            GO TO 5300-MOVE-LF-REM-TERM.
01935
01936      IF SUB3 LESS THAN +20
01937         ADD +1                   TO SUB3
01938         GO TO 5100-GET-LF-AM-REM-TERM.
01939
01940      MOVE +1                     TO SUB3.
01941
01942      IF PI-COMPANY-ID NOT EQUAL 'DMD'
01943          GO TO 5200-EDIT-FOR-NINETY-LF.
01944
01945  5100-GET-LF-DMD-AM-REM-TERM.
01946
01947      IF AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1
01948         IF CM-LF-BENEFIT-CD = AM-BENEFIT-DMD-CODE (SUB3)
01949            MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
01950                          TO WS-SAVE-ALLOWABLE-BENEFIT
01951            GO TO 5300-MOVE-LF-REM-TERM.
01952
01953      IF SUB3 LESS THAN +30
01954         ADD +1                   TO SUB3
01955         GO TO 5100-GET-LF-DMD-AM-REM-TERM.
01956
01957      MOVE +1                     TO SUB3.
01958
01959  5200-EDIT-FOR-NINETY-LF.
01960
01961 ***************************************************************
01962 *                                                             *
01963 *    NINETY BENEFIT PLAN TYPES:                               *
01964 *                                                             *
01965 *      91 = ALL INDIVIDUAL REDUCING BENEFITS.                 *
01966 *      92 = ALL GROUP REDUCING BENEFITS                       *
01967 *      93 = ALL INDIVIDUAL LEVEL BENEFITS                     *
01968 *      94 = ALL GROUP LEVEL BENEFITS                          *
01969 *      98 = ALL BENEFITS EXCEPT THOSE CODED IN ACCOUNT        *
01970 *           BENEFIT CONTROLS.                                 *
01971 *      99 = ALL BENEFITS                                      *
01972 *                                                             *
01973 ***************************************************************
01974
01975      MOVE AM-ALLOWABLE-BENEFITS (SUB3)
01976                          TO WS-SAVE-ALLOWABLE-BENEFIT.
01977
01978      IF (AM-BENEFIT-CODE (SUB3) = '99')
01979        AND
01980         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
01981            GO TO 5300-MOVE-LF-REM-TERM.
01982
01983      IF (AM-BENEFIT-CODE (SUB3) = '91')
01984        AND
01985         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
01986         IF CF-REDUCING (WS-INDEX)
01987            IF CM-INDIVIDUAL
01988                GO TO 5300-MOVE-LF-REM-TERM.
01989
01990      IF (AM-BENEFIT-CODE (SUB3) = '92')
01991        AND
01992         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
01993         IF CF-REDUCING (WS-INDEX)
01994            IF CM-GROUP
01995                GO TO 5300-MOVE-LF-REM-TERM.
01996
01997      IF (AM-BENEFIT-CODE (SUB3) = '93')
01998        AND
01999         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02000         IF CF-LEVEL (WS-INDEX)
02001            IF CM-INDIVIDUAL
02002                GO TO 5300-MOVE-LF-REM-TERM.
02003
02004      IF (AM-BENEFIT-CODE (SUB3) = '94')
02005        AND
02006         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02007         IF CF-LEVEL (WS-INDEX)
02008            IF CM-GROUP
02009                GO TO 5300-MOVE-LF-REM-TERM.
02010
02011      IF (AM-BENEFIT-CODE (SUB3) = '98')
02012        AND
02013         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02014             GO TO 5300-MOVE-LF-REM-TERM.
02015
02016      IF SUB3 LESS THAN +20
02017         ADD +1           TO SUB3
02018           GO TO 5200-EDIT-FOR-NINETY-LF.
02019
02020      MOVE SPACES         TO WS-SAVE-ALLOWABLE-BENEFIT.
02021
02022      IF PI-COMPANY-ID NOT EQUAL 'DMD'
02023          MOVE '0'                 TO WS-LF-AM-REM-TERM-CALC
02024          GO TO 5900-EXIT.
02025
02026      MOVE 1                       TO SUB3.
02027
02028  5200-EDIT-FOR-DMD-NINETY-LF.
02029
02030      MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
02031                          TO WS-SAVE-ALLOWABLE-BENEFIT.
02032
02033      IF (AM-BENEFIT-DMD-CODE (SUB3) = '99')
02034        AND
02035         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02036            GO TO 5300-MOVE-LF-REM-TERM.
02037
02038      IF (AM-BENEFIT-DMD-CODE (SUB3) = '91')
02039        AND
02040         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02041         IF CF-REDUCING (WS-INDEX)
02042            IF CM-INDIVIDUAL
02043                GO TO 5300-MOVE-LF-REM-TERM.
02044
02045      IF (AM-BENEFIT-DMD-CODE (SUB3) = '92')
02046        AND
02047         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02048         IF CF-REDUCING (WS-INDEX)
02049            IF CM-GROUP
02050                GO TO 5300-MOVE-LF-REM-TERM.
02051
02052      IF (AM-BENEFIT-DMD-CODE (SUB3) = '93')
02053        AND
02054         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02055         IF CF-LEVEL (WS-INDEX)
02056            IF CM-INDIVIDUAL
02057                GO TO 5300-MOVE-LF-REM-TERM.
02058
02059      IF (AM-BENEFIT-DMD-CODE (SUB3) = '94')
02060        AND
02061         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02062         IF CF-LEVEL (WS-INDEX)
02063            IF CM-GROUP
02064                GO TO 5300-MOVE-LF-REM-TERM.
02065
02066      IF (AM-BENEFIT-DMD-CODE (SUB3) = '98')
02067        AND
02068         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02069             GO TO 5300-MOVE-LF-REM-TERM.
02070
02071      IF SUB3 LESS THAN +30
02072         ADD +1                   TO SUB3
02073           GO TO 5200-EDIT-FOR-DMD-NINETY-LF.
02074
02075      MOVE '0'                    TO WS-LF-AM-REM-TERM-CALC.
02076
02077      GO TO 5900-EXIT.
02078
02079  5300-MOVE-LF-REM-TERM.
02080      MOVE WS-SAVE-BENEFIT-REM-TERM
02081                                  TO WS-LF-AM-REM-TERM-CALC.
02082
02083  5900-EXIT.
02084       EXIT.
02085
02086  EJECT
02087
02088  6000-GET-AH-AM-REM-TERM.
02089      MOVE +1                     TO SUB3.
02090
02091  6100-GET-AH-AM-REM-TERM.
02092
02093      IF AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02094         IF WS-BEN-CD = AM-BENEFIT-CODE (SUB3)
02095            MOVE AM-ALLOWABLE-BENEFITS (SUB3)
02096                          TO WS-SAVE-ALLOWABLE-BENEFIT
02097            GO TO 6300-MOVE-AH-REM-TERM.
02098
02099      IF SUB3 LESS THAN +20
02100         ADD +1                   TO SUB3
02101         GO TO 6100-GET-AH-AM-REM-TERM.
02102
02103      MOVE +1                     TO SUB3.
02104
02105      IF PI-COMPANY-ID NOT EQUAL 'DMD'
02106          GO TO 6200-EDIT-FOR-NINETY-AH.
02107
02108  6100-GET-AH-AM-DMD-REM-TERM.
02109      IF AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02110         IF WS-BEN-CD = AM-BENEFIT-DMD-CODE (SUB3)
02111            MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
02112                          TO WS-SAVE-ALLOWABLE-BENEFIT
02113            GO TO 6300-MOVE-AH-REM-TERM.
02114
02115      IF SUB3 LESS THAN +30
02116         ADD +1                   TO SUB3
02117         GO TO 6100-GET-AH-AM-DMD-REM-TERM.
02118
02119      MOVE +1                     TO SUB3.
02120
02121  6200-EDIT-FOR-NINETY-AH.
02122
02123 ***************************************************************
02124 *                                                             *
02125 *    NINETY BENEFIT PLAN TYPES:                               *
02126 *                                                             *
02127 *      91 = ALL INDIVIDUAL REDUCING BENEFITS.                 *
02128 *      92 = ALL GROUP REDUCING BENEFITS.                      *
02129 *      98 = ALL BENEFITS EXCEPT THOSE CODED IN ACCOUNT        *
02130 *           BENEFIT CONTROLS.                                 *
02131 *      99 = ALL BENEFITS.                                     *
02132 *                                                             *
02133 ***************************************************************
02134
02135      MOVE AM-ALLOWABLE-BENEFITS (SUB3)
02136                          TO WS-SAVE-ALLOWABLE-BENEFIT.
02137
02138      IF AM-BENEFIT-CODE (SUB3) = '99'
02139        AND
02140         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02141            GO TO 6300-MOVE-AH-REM-TERM.
02142
02143      IF AM-BENEFIT-CODE (SUB3) = '91'
02144        AND
02145         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02146            IF CM-INDIVIDUAL
02147                GO TO 6300-MOVE-AH-REM-TERM.
02148
02149      IF AM-BENEFIT-CODE (SUB3) = '92'
02150        AND
02151         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02152            IF CM-GROUP
02153                GO TO 6300-MOVE-AH-REM-TERM.
02154
02155      IF AM-BENEFIT-CODE (SUB3) = '98'
02156        AND
02157         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02158            GO TO 6300-MOVE-AH-REM-TERM.
02159
02160      IF SUB3 LESS THAN +20
02161         ADD +1                    TO SUB3
02162         GO TO 6200-EDIT-FOR-NINETY-AH.
02163
02164      MOVE +1                      TO SUB3.
02165
02166      IF PI-COMPANY-ID NOT EQUAL 'DMD'
02167          MOVE '0'                 TO WS-AH-AM-REM-TERM-CALC
02168          GO TO 6900-EXIT.
02169
02170  6200-EDIT-FOR-DMD-NINETY-AH.
02171
02172      MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
02173                          TO WS-SAVE-ALLOWABLE-BENEFIT.
02174
02175      IF AM-BENEFIT-DMD-CODE (SUB3) = '99'
02176        AND
02177         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02178            GO TO 6300-MOVE-AH-REM-TERM.
02179
02180      IF AM-BENEFIT-DMD-CODE (SUB3) = '91'
02181        AND
02182         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02183            IF CM-INDIVIDUAL
02184                GO TO 6300-MOVE-AH-REM-TERM.
02185
02186      IF AM-BENEFIT-DMD-CODE (SUB3) = '92'
02187        AND
02188         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02189            IF CM-GROUP
02190                GO TO 6300-MOVE-AH-REM-TERM.
02191
02192      IF AM-BENEFIT-DMD-CODE (SUB3) = '98'
02193        AND
02194         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02195            GO TO 6300-MOVE-AH-REM-TERM.
02196
02197      IF SUB3 LESS THAN +30
02198         ADD +1                   TO SUB3
02199         GO TO 6200-EDIT-FOR-DMD-NINETY-AH.
02200
02201      MOVE '0'                    TO WS-AH-AM-REM-TERM-CALC.
02202
02203      GO TO 6900-EXIT.
02204
02205  6300-MOVE-AH-REM-TERM.
02206
02207      MOVE WS-SAVE-BENEFIT-REM-TERM
02208                                  TO WS-AH-AM-REM-TERM-CALC.
02209
02210  6900-EXIT.
02211       EXIT.
02212
02213  EJECT
02214  7000-GET-ERFORM     SECTION.
02215      EXEC CICS HANDLE CONDITION
02216          NOTFND  (7000-EXIT)
02217      END-EXEC.
02218
02219      MOVE ' '                    TO  WS-FORM-RECORD-SW.
02220      MOVE PI-COMPANY-CD          TO  WS-FO-COMPANY-CD.
02221      MOVE PI-STATE               TO  WS-FO-STATE.
02222      MOVE CM-POLICY-FORM-NO      TO  WS-FO-FORM-ID.
02223      MOVE CM-CERT-EFF-DT         TO  WS-FO-EXP-DT.
02224
02225      EXEC CICS READ
02226          DATASET  (ERFORM-ID)
02227          RIDFLD   (WS-FO-CONTROL-PRIMARY)
02228          SET      (ADDRESS OF FORM-MASTER)
02229          GTEQ
02230      END-EXEC.
02231
02232      IF PI-COMPANY-CD     EQUAL FO-COMPANY-CD AND
02233         PI-STATE          EQUAL FO-STATE      AND
02234         CM-POLICY-FORM-NO EQUAL FO-FORM-ID
02235          MOVE 'Y'  TO WS-FORM-RECORD-SW
02236          GO TO 7000-EXIT.
02237  7000-EXIT.
02238      EXIT.
02239  7100-BENEFIT-SEARCH SECTION.
02240
02241      IF CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES
02242         MOVE +1                  TO  SUB4
02243         PERFORM 7150-BENEFIT-LIFE-SEARCH THRU 7150-EXIT.
02244
02245      IF CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES
02246         MOVE +1                  TO  SUB4
02247         PERFORM 7250-BENEFIT-AH-SEARCH THRU 7250-EXIT.
02248  7100-EXIT.
02249      EXIT.
02250  7150-BENEFIT-LIFE-SEARCH.
02251      IF PI-LIFE-OVERRIDE-L1 EQUAL TO FO-PLAN-TYPE (SUB4)
02252        IF CM-LF-BENEFIT-CD EQUAL TO FO-PLAN-ID (SUB4)
02253          IF (CM-LF-ORIG-TERM IS LESS THAN FO-PLAN-TERM (SUB4)
02254           OR CM-LF-ORIG-TERM IS EQUAL TO FO-PLAN-TERM (SUB4))
02255             IF FO-PLAN-REFUND-METHOD (SUB4)
02256                     IS GREATER THAN SPACES
02257                MOVE FO-PLAN-REFUND-METHOD (SUB4) TO
02258                                     WS-LF-FO-REFUND-CALC
02259                GO TO 7150-EXIT.
02260
02261      IF SUB4 LESS THAN +40
02262          ADD +1              TO SUB4
02263          GO TO 7150-BENEFIT-LIFE-SEARCH.
02264
02265  7150-EXIT.
02266      EXIT.
02267  7250-BENEFIT-AH-SEARCH.
02268      IF PI-AH-OVERRIDE-L1 EQUAL TO FO-PLAN-TYPE (SUB4)
02269        IF CM-AH-BENEFIT-CD EQUAL TO FO-PLAN-ID (SUB4)
02270          IF (CM-AH-ORIG-TERM IS LESS THAN FO-PLAN-TERM (SUB4)
02271           OR CM-AH-ORIG-TERM IS EQUAL TO FO-PLAN-TERM (SUB4))
02272             IF FO-PLAN-REFUND-METHOD (SUB4)
02273                     IS GREATER THAN SPACES
02274                MOVE FO-PLAN-REFUND-METHOD (SUB4) TO
02275                                     WS-AH-FO-REFUND-CALC
02276                GO TO 7250-EXIT.
02277
02278      IF SUB4 LESS THAN +40
02279          ADD +1              TO SUB4
02280          GO TO 7250-BENEFIT-AH-SEARCH.
02281
02282  7250-EXIT.
02283      EXIT.
02284  EJECT
02285
02286  8100-SEND-INITIAL-MAP  SECTION.
02287      MOVE SAVE-DATE              TO  HDATEO.
02288      MOVE EIBTIME                TO  TIME-IN.
02289      MOVE TIME-OUT               TO  HTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
02290      MOVE EMI-MESSAGE-AREA (1)   TO  HERMSG1O.
02291      MOVE EMI-MESSAGE-AREA (2)   TO  HERMSG2O.
02292
02293      IF CM-LF-BENEFIT-CD = '00'
02294          MOVE -1                 TO  HACANCL
02295      ELSE
02296          MOVE -1                 TO  HLCANCL.
02297

020816     if pi-company-id = 'VPP'
020816        move al-sanof            to refdueha
020816                                    canfeeha
020816        move al-uanof            to refduea
020816                                    canfeea
020816     end-if

020816     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
070622*       MOVE AL-SADOF            TO CANREAHA
020816        MOVE AL-SADOF            TO CLPYNHA
070622*                                   CANREAA
020816                                    CLPYNA
020816     END-IF
02300
02301      EXEC CICS SEND
02302          FROM    (EL127HO)
02303          MAPSET  (MAPSET-NAME)
02304          MAP     (MAP-NAME)
02305          CURSOR
02306          ERASE
02307      END-EXEC.
02308
02309      GO TO 9000-RETURN-TRAN.
02310
02311  8200-SEND-DATAONLY  SECTION.
02312      IF FIRST-ENTRY
02313          GO TO 8100-SEND-INITIAL-MAP.
02314
02315      MOVE SAVE-DATE              TO  HDATEO.
02316      MOVE EIBTIME                TO  TIME-IN.
02317      MOVE TIME-OUT               TO  HTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
02318      MOVE EMI-MESSAGE-AREA (1)   TO  HERMSG1O.
02319      MOVE EMI-MESSAGE-AREA (2)   TO  HERMSG2O.
091322*    MOVE -1                     TO  HLCANCL.
02321
02322      EXEC CICS SEND DATAONLY
02323          FROM    (EL127HO)
02324          MAPSET  (MAPSET-NAME)
02325          MAP     (MAP-NAME)
02326          CURSOR
02327      END-EXEC.
02328
02329      GO TO 9000-RETURN-TRAN.
02330  EJECT
02331  8300-SEND-TEXT  SECTION.
02332      EXEC CICS SEND TEXT
02333          FROM    (LOGOFF-TEXT)
02334          LENGTH  (LOGOFF-LENGTH)
02335          ERASE
02336          FREEKB
02337      END-EXEC.
02338
02339      EXEC CICS RETURN
02340      END-EXEC.
02341  EJECT
02342  8600-DEEDIT  SECTION.
02343      EXEC CICS BIF DEEDIT
02344          FIELD   (DEEDIT-FIELD)
02345          LENGTH  (15)
02346      END-EXEC.
02347  EJECT
02348  8700-UNAUTHORIZED-ACCESS  SECTION.
02349      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
02350
02351      GO TO 8300-SEND-TEXT.
02352
02353  8800-PF23  SECTION.
02354      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02355      MOVE XCTL-005               TO  PGM-NAME.
02356
02357      GO TO 9200-XCTL.
02358
02359  8900-RETURN-CICS  SECTION.
02360      EXEC CICS RETURN
02361      END-EXEC.
02362
02363  9000-RETURN-TRAN  SECTION.
02364      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02365      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02366
02367      EXEC CICS RETURN
02368          TRANSID   (TRANS-ID)
02369          COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02370          LENGTH    (PI-COMM-LENGTH)
02371      END-EXEC.
02372
02373  9100-RETURN-MAIN-MENU  SECTION.
02374      MOVE XCTL-126               TO  PGM-NAME.
02375
02376      GO TO 9200-XCTL.
02377
02378  9200-XCTL  SECTION.
02379      EXEC CICS XCTL
02380          PROGRAM   (PGM-NAME)
02381          COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02382          LENGTH    (PI-COMM-LENGTH)
02383      END-EXEC.
02384
02385  9300-CLEAR  SECTION.
02386      MOVE ' '                    TO  PI-PEND-SW.
02387      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
02388
02389      GO TO 9200-XCTL.
02390
02391  9400-PF12  SECTION.
02392      MOVE XCTL-010               TO  PGM-NAME.
02393
02394      GO TO 9200-XCTL.
02395
02396  9500-PGMID-ERROR  SECTION.
02397      EXEC CICS HANDLE CONDITION
02398          PGMIDERR  (8300-SEND-TEXT)
02399      END-EXEC.
02400
02401      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
02402      MOVE ' '                    TO  PI-ENTRY-CD-1.
02403      MOVE XCTL-005               TO  PGM-NAME.
02404      MOVE PGM-NAME               TO  LOGOFF-PGM.
02405      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02406
02407      GO TO 9200-XCTL.
02408
02409  9600-DATE-LINK  SECTION.
02410      MOVE LINK-ELDATCV           TO  PGM-NAME
02411
02412      EXEC CICS LINK
02413          PROGRAM   (PGM-NAME)
02414          COMMAREA  (DATE-CONVERSION-DATA)
02415          LENGTH    (DC-COMM-LENGTH)
02416      END-EXEC.
02417
02418
02419  9700-ERROR-FORMAT  SECTION.
02420      IF NOT EMI-ERRORS-COMPLETE
02421          MOVE LINK-001               TO  PGM-NAME
02422          EXEC CICS LINK
02423              PROGRAM   (PGM-NAME)
02424              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
02425              LENGTH    (EMI-COMM-LENGTH)
02426          END-EXEC.
02427
02428  9799-EXIT.
02429      EXIT.
02430
02431  9800-SECURITY-VIOLATION  SECTION.
02432                              COPY ELCSCTP.
02433
02434  9899-EXIT.
02435      EXIT.
02436
02437  9900-ABEND  SECTION.
02438      MOVE LINK-004               TO  PGM-NAME.
02439      MOVE DFHEIBLK               TO  EMI-LINE1.
02440
02441      EXEC CICS LINK
02442          PROGRAM   (PGM-NAME)
02443          COMMAREA  (EMI-LINE1)
02444          LENGTH    (72)
02445      END-EXEC.
02446
02447      MOVE -1                     TO  HLCANCL.
02448
02449      GO TO 8200-SEND-DATAONLY.
02450
02451  9999-LAST-PARAGRAPH.
02452      GOBACK.
