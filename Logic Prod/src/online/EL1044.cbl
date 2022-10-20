       ID DIVISION.
       PROGRAM-ID.                 EL1044.
      *
      *AUTHOR.     PABLO
      *            OMAHA NE
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CENTRAL STATES IS EXPRESSLY PROHIBITED       *
      *            *   WITHOUT THE PRIOR WRITTEN PERMISSION OF         *
      *            *   CENTRAL STATES                                  *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
      *    FOR THE Z LETTER CONTROLS FOR ACCOUNT SERVICES
      *    SCREENS     - EL1044Z - LETTER (Z) CONTROL MAINTENANCE
      *    ENTERED BY  - EL1042 - TEXT MAINT
      *    EXIT TO     - EL1042 - TEXT MAINT
      *    INPUT FILE  - ELLETR -              - LETTER TEXT
      *    OUTPUT FILE - ELLETR -              - LETTER TEXT
      *    COMMAREA    - PASSED
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL1042. ON
      *                  FIRST ENTRY, WE READ THE ELLETR FILE USING THE
      *                  LETTER ID PASSED FROM EL1042.     ON SUBSEQUENT
      *                  ENTRIES (XCTL FROM CICS VIA     ) THE SCREEN
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE
      *                  MAINTENANCE TYPE INDICATED.
122011******************************************************************
122011*                   C H A N G E   L O G
122011*
122011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122011*-----------------------------------------------------------------
122011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122011* EFFECTIVE    NUMBER
122011*-----------------------------------------------------------------
122011* 122011    2011022800001  AJRA  ADD ELCZREC COPYBOOK
073112* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
122712* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
080113* 080113    2013062000003  AJRA  ADD BARCODE,RETURN ENV FLAG
091913* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
010914* 010914    2013090300001  AJRA  VALIDATE ENC CODE AGAINST ELENCC
031516* 030516  CR2016012700004  PEMA  Add acct summ code of 5
092419* 092419  CR2019030500002  TANA  Add letter type of A
122712******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*   EL1044 WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
       77  M1                          PIC S999 COMP-3 VALUE +0.
       77  M2                          PIC S999 COMP-3 VALUE +0.
       77  WS-STOP-SW                  PIC X  VALUE ' '.
           88  I-SAY-TO-STOP              VALUE 'Y'.
       77  WS-FIND-SW                  PIC X  VALUE ' '.
           88  WE-FOUND-IT                VALUE 'Y'.
010914 77  WS-DONE-WITH-ENCC           PIC X  VALUE ' '.
010914     88  DONE-WITH-ENCC             VALUE 'Y'.
       77  WS-SEQ-NO-TO-USE            PIC S9(4) COMP VALUE +0.
       01  ACCESS-KEYS.
           12  ELLETR-KEY.
               16  ELLETR-COMPANY-CD   PIC X.
               16  ELLETR-ACCESS-CD    PIC X(12).
               16  ELLETR-SEQ-NO       PIC S9(4) COMP.
010914
010914     12  ELENCC-KEY.
010914         16  ELENCC-COMPANY-CD    PIC X.
010914         16  ELENCC-REC-TYPE      PIC X.
010914         16  ELENCC-ENC-CODE      PIC X(5).
010914         16  F                    PIC X(09).
       01  WS-DATE-AREA.
           05  SAVE-DATE                   PIC X(08)   VALUE SPACES.
           05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.
       01  MISC-WORK-AREAS.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-9V999-OF-DEEDIT REDEFINES
                   WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
           12  DEEDIT-FIELD                PIC X(11).
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD
                                           PIC S9(11).
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD
                                           PIC S9(09)V99.
010914
010914     12  WS-TEST-STATE               PIC X(2) VALUE SPACES.
010914         88 STATE-SPECIFIC    VALUES 'AK','AL','AR','AS','AZ',
010914            'CA','CO','CT','DC','DE','FL','GA','GU','HI','IA',
010914            'ID','IL','IN','KS','KY','LA','MA','MD','ME','MI',
010914            'MN','MO','MS','MT','NC','ND','NE','NH','NJ','NM',
010914            'NV','NY','OH','OK','OR','PA','PR','RI','SC','SD',
010914            'TN','TX','UT','VA','VI','VT','WA','WI','WV','WY'.
       01  STANDARD-AREAS.
           12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.
           12  TRANS-ID                    PIC X(04)   VALUE 'EX1F'.
           12  PGM-NAME                    PIC X(08).
           12  TIME-IN                     PIC S9(07).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER                  PIC X(01).
               16  TIME-OUT                PIC 99V99.
               16  FILLER                  PIC X(02).
           12  XCTL-005                    PIC X(08)   VALUE 'EL005'.
           12  XCTL-010                    PIC X(08)   VALUE 'EL010'.
           12  XCTL-155                    PIC X(08)   VALUE 'EL155'.
           12  XCTL-626                    PIC X(08)   VALUE 'EL626'.
           12  LINK-001                    PIC X(08)   VALUE 'EL001'.
           12  LINK-004                    PIC X(08)   VALUE 'EL004'.
           12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.
           12  THIS-PGM                    PIC X(08)   VALUE 'EL1044'.
           12  ELLETR-FILE-ID              PIC X(08)   VALUE 'ELLETR'.
           12  ELLETR-LENGTH               PIC S9(04)  VALUE +100  COMP.
010914     12  ELENCC-FILE-ID              PIC X(08)   VALUE 'ELENCC'.
010914     12  ELENCC-LENGTH               PIC S9(04)  VALUE +400  COMP.
           12  SUB                         PIC 9(02).
           12  SUB-1                       PIC 9(02).
           12  SUB2                        PIC 9(02).
           12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.
           12  MAPSET-NAME                 PIC X(08)   VALUE 'EL1044S'.
           12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL1044Z'.
           12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.
010914     12  HOLD-ENC-CODES              PIC X(120)  VALUE SPACES.
010914     12  WS-SUB1                     PIC S9(03).
010914     12  WS-SUB2                     PIC S9(03).
010914     12  WS-SUB3                     PIC S9(03).
122011****  Z RECORD LAYOUT MOVED TO COPYBOOK ELCZREC
122011*                                    COPY ELCZREC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCZREC.                            *
      *                                                                *
      *   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
      *                                                                *
      ******************************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122011    2011022800001  AJRA  NEW FILE
073112* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
122712* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
072313* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
091913* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
      *-----------------------------------------------------------------
       01  W-Z-CONTROL-DATA.
           05  W-NUMBER-OF-COPIES      PIC  9.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-FOLLOW-UP     PIC  999.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-RESEND        PIC  999.
           05  FILLER                  PIC  X.
           05  W-FORM-TO-RESEND        PIC  X(4).
072313     05  W-ADD-BAR-CODE          PIC  X.
           05  W-PROMPT-LETTER         PIC  X.
072313     05  W-HAS-RETURN-ENV        PIC  X.
           05  W-ENCLOSURE-CD          PIC  XXX.
091913     05  W-SIG-FLAG-DEFAULT      PIC  X.
           05  W-AUTO-CLOSE-IND        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-BENE        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-ACCT        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TYPE           PIC  X.
           05  FILLER                  PIC  X.
           05  W-PRINT-CERTIFICATE     PIC  X.
           05  FILLER                  PIC  X.
           05  W-REFUND-REQUIRED       PIC  X.
           05  FILLER                  PIC  X.
           05  W-ONBASE-CODE           PIC  XX.
073112     05  FILLER                  PIC  X.
073112     05  W-ACCT-SUMM             PIC  X.
073112     05  FILLER                  PIC  X.
073112     05  W-CSO-SUMM              PIC  X.
122712     05  FILLER                  PIC  X.
122712     05  W-REASONS-REQUIRED      PIC  X.
122712     05  FILLER                  PIC  X(29).
      *                                    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
      *                                    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
       01  ERROR-MESSAGES.
           12  ER-0000                     PIC X(04)   VALUE '0000'.
           12  ER-0005                     PIC X(04)   VALUE '0005'.
           12  ER-0023                     PIC X(04)   VALUE '0023'.
           12  ER-0029                     PIC X(04)   VALUE '0029'.
           12  ER-0050                     PIC X(04)   VALUE '0050'.
           12  ER-0068                     PIC X(04)   VALUE '0068'.
           12  ER-0070                     PIC X(04)   VALUE '0070'.
           12  ER-0130                     PIC X(04)   VALUE '0130'.
           12  ER-0131                     PIC X(04)   VALUE '0131'.
           12  ER-0132                     PIC X(04)   VALUE '0132'.
           12  ER-0138                     PIC X(04)   VALUE '0138'.
           12  ER-0144                     PIC X(04)   VALUE '0144'.
           12  ER-0145                     PIC X(04)   VALUE '0145'.
           12  ER-0184                     PIC X(04)   VALUE '0184'.
           12  ER-0418                     PIC X(04)   VALUE '0418'.
           12  ER-0491                     PIC X(04)   VALUE '0491'.
           12  ER-0582                     PIC X(04)   VALUE '0582'.
           12  ER-0627                     PIC X(04)   VALUE '0627'.
           12  ER-0701                     PIC X(04)   VALUE '0701'.
           12  ER-0702                     PIC X(04)   VALUE '0702'.
           12  ER-0703                     PIC X(04)   VALUE '0703'.
           12  ER-0704                     PIC X(04)   VALUE '0704'.
           12  ER-0705                     PIC X(04)   VALUE '0705'.
           12  ER-0706                     PIC X(04)   VALUE '0706'.
           12  ER-0707                     PIC X(04)   VALUE '0707'.
           12  ER-0708                     PIC X(04)   VALUE '0708'.
           12  ER-0709                     PIC X(04)   VALUE '0709'.
           12  ER-0710                     PIC X(04)   VALUE '0710'.
           12  ER-0711                     PIC X(04)   VALUE '0711'.
           12  ER-0712                     PIC X(04)   VALUE '0712'.
           12  ER-0713                     PIC X(04)   VALUE '0713'.
           12  ER-0717                     PIC X(04)   VALUE '0717'.
           12  ER-0718                     PIC X(04)   VALUE '0718'.
           12  ER-0719                     PIC X(04)   VALUE '0719'.
           12  ER-0720                     PIC X(04)   VALUE '0720'.
           12  ER-0721                     PIC X(04)   VALUE '0721'.
           12  ER-0722                     PIC X(04)   VALUE '0722'.
           12  ER-0723                     PIC X(04)   VALUE '0723'.
           12  ER-0724                     PIC X(04)   VALUE '0724'.
           12  ER-0725                     PIC X(04)   VALUE '0725'.
           12  ER-0726                     PIC X(04)   VALUE '0726'.
           12  ER-0727                     PIC X(04)   VALUE '0727'.
           12  ER-0729                     PIC X(04)   VALUE '0729'.
           12  ER-0754                     PIC X(04)   VALUE '0754'.
           12  ER-1560                     PIC X(04)   VALUE '1560'.
           12  ER-1562                     PIC X(04)   VALUE '1562'.
           12  ER-1564                     PIC X(04)   VALUE '1564'.
           12  ER-2241                     PIC X(04)   VALUE '2241'.
           12  ER-2276                     PIC X(04)   VALUE '2276'.
           12  ER-3804                     PIC X(04)   VALUE '3804'.
073112     12  ER-3820                     PIC X(04)   VALUE '3820'.
073112     12  ER-3821                     PIC X(04)   VALUE '3821'.
           12  ER-7008                     PIC X(04)   VALUE '7008'.
           12  ER-7031                     PIC X(04)   VALUE '7031'.
           12  ER-7123                     PIC X(04)   VALUE '7123'.
           12  ER-8150                     PIC X(04)   VALUE '8150'.
           12  ER-9999                     PIC XXXX    VALUE '9999'.
      *                                    COPY ELCDATE.
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
      *                                    COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
      *                                    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
      *                                    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *                                    COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
                                           PIC X(530).
      *                                    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
           12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
      *                                    COPY ELC1042.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELC1042                              *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *    NOTE                                                        *
00007 *        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
00008 *        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
00009 *        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
00010 *        TO THESE PROGRAMS.                                      *
00011 *                                                                *
00012 *    NOTE                                                        *
00013 *        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
00014 *        USE ONLY!                                               *
00015 *                                                                *
00016 ******************************************************************
00017
00018          16  PI-1042-WA.
00019              20  PI-ACTION       PIC  X(01).
00020                  88 PI-SHOW-MODE           VALUE '1'.
00021                  88 PI-CLEAR-MODE          VALUE '2'.
00022                  88 PI-CREATE-MODE         VALUE '3'.
00023              20  PI-COMM-CONTROL PIC  X(12).
00024              20  PI-CURRENT-LINE PIC S9(03) COMP-3.
00025              20  PI-EOF-SW       PIC  X(01).
00026                  88  PI-FILE-EOF           VALUE 'Y'.
00027              20  PI-FILETYP      PIC  X(01).
00028              20  PI-FORM-SQUEEZE-CONTROL
00029                                  PIC  X(01).
00030                  88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
00031                  88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
00032              20  PI-LAST-CONTROL PIC  X(12).
00033              20  PI-TEMP-STOR-ITEMS
00034                                  PIC S9(04) COMP.
00035              20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
00036              20  PI-UPDATE-SW    PIC  9(01).
00037                  88 ANY-UPDATES            VALUE 1.
00038              20  PI-104-SCREEN-SENT-IND
00039                                  PIC  X(01).
00040                  88  PI-104-SCREEN-SENT    VALUE 'Y'.
00041                  88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
00042              20  PI-1042-SCREEN-SENT-IND
00043                                  PIC  X(01).
00044                  88  PI-1042-SCREEN-SENT    VALUE 'Y'.
00045                  88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
00046              20  PI-1042-ARCHIVE-IND
00047                                  PIC  X(01).
00048                  88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
00049              20  FILLER          PIC  X(29).
               16  PI-PREV-LETR-KEY        PIC X(15).
               16  PI-CURR-LETR-KEY        PIC X(15).
               16  PI-UPDATE-DT            PIC XX.
010914         16  PI-ENC-CODES            PIC X(120).
010914         16  PI-SPLIT-SUB            PIC S9(3)  COMP.
010914         16  FILLER                  PIC X(434).
      *                                    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
       01  FILLER    REDEFINES DFHAID.
           12  FILLER                      PIC X(08).
           12  PF-VALUES                   PIC X         OCCURS 24.
      *                                    COPY EL1044S.
       01  EL1044ZI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  ZLETRL PIC S9(0004) COMP.
           05  ZLETRF PIC  X(0001).
           05  FILLER REDEFINES ZLETRF.
               10  ZLETRA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZLETRI PIC  X(0004).
      *    -------------------------------
           05  ZCOPIESL PIC S9(0004) COMP.
           05  ZCOPIESF PIC  X(0001).
           05  FILLER REDEFINES ZCOPIESF.
               10  ZCOPIESA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZCOPIESI PIC  X(0001).
      *    -------------------------------
           05  ZPROMPTL PIC S9(0004) COMP.
           05  ZPROMPTF PIC  X(0001).
           05  FILLER REDEFINES ZPROMPTF.
               10  ZPROMPTA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZPROMPTI PIC  X(0001).
      *    -------------------------------
           05  ZENCCDL PIC S9(0004) COMP.
           05  ZENCCDF PIC  X(0001).
           05  FILLER REDEFINES ZENCCDF.
               10  ZENCCDA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZENCCDI PIC  X(0003).
      *    -------------------------------
           05  ZRDAYSL PIC S9(0004) COMP.
           05  ZRDAYSF PIC  X(0001).
           05  FILLER REDEFINES ZRDAYSF.
               10  ZRDAYSA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZRDAYSI PIC  X(0003).
      *    -------------------------------
           05  ZFORML PIC S9(0004) COMP.
           05  ZFORMF PIC  X(0001).
           05  FILLER REDEFINES ZFORMF.
               10  ZFORMA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZFORMI PIC  X(0004).
      *    -------------------------------
           05  ZBENEL PIC S9(0004) COMP.
           05  ZBENEF PIC  X(0001).
           05  FILLER REDEFINES ZBENEF.
               10  ZBENEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZBENEI PIC  X(0001).
      *    -------------------------------
           05  ZACCTL PIC S9(0004) COMP.
           05  ZACCTF PIC  X(0001).
           05  FILLER REDEFINES ZACCTF.
               10  ZACCTA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZACCTI PIC  X(0001).
      *    -------------------------------
           05  ZTYPEL PIC S9(0004) COMP.
           05  ZTYPEF PIC  X(0001).
           05  FILLER REDEFINES ZTYPEF.
               10  ZTYPEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZTYPEI PIC  X(0001).
      *    -------------------------------
           05  ZPRTCRTL PIC S9(0004) COMP.
           05  ZPRTCRTF PIC  X(0001).
           05  FILLER REDEFINES ZPRTCRTF.
               10  ZPRTCRTA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZPRTCRTI PIC  X(0001).
      *    -------------------------------
           05  ZREFREQL PIC S9(0004) COMP.
           05  ZREFREQF PIC  X(0001).
           05  FILLER REDEFINES ZREFREQF.
               10  ZREFREQA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZREFREQI PIC  X(0001).
      *    -------------------------------
           05  ZONBASEL PIC S9(0004) COMP.
           05  ZONBASEF PIC  X(0001).
           05  FILLER REDEFINES ZONBASEF.
               10  ZONBASEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZONBASEI PIC  X(0002).
      *    -------------------------------
           05  ZACTSUML PIC S9(0004) COMP.
           05  ZACTSUMF PIC  X(0001).
           05  FILLER REDEFINES ZACTSUMF.
               10  ZACTSUMA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZACTSUMI PIC  X(0001).
      *    -------------------------------
           05  ZCSOSUML PIC S9(0004) COMP.
           05  ZCSOSUMF PIC  X(0001).
           05  FILLER REDEFINES ZCSOSUMF.
               10  ZCSOSUMA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZCSOSUMI PIC  X(0001).
      *    -------------------------------
           05  ZREASONL PIC S9(0004) COMP.
           05  ZREASONF PIC  X(0001).
           05  FILLER REDEFINES ZREASONF.
               10  ZREASONA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZREASONI PIC  X(0001).
      *    -------------------------------
           05  ZBARCODL PIC S9(0004) COMP.
           05  ZBARCODF PIC  X(0001).
           05  FILLER REDEFINES ZBARCODF.
               10  ZBARCODA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZBARCODI PIC  X(0001).
      *    -------------------------------
           05  ZRETENVL PIC S9(0004) COMP.
           05  ZRETENVF PIC  X(0001).
           05  FILLER REDEFINES ZRETENVF.
               10  ZRETENVA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZRETENVI PIC  X(0001).
      *    -------------------------------
           05  ZSIGFLGL PIC S9(0004) COMP.
           05  ZSIGFLGF PIC  X(0001).
           05  FILLER REDEFINES ZSIGFLGF.
               10  ZSIGFLGA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZSIGFLGI PIC  X(0001).
      *    -------------------------------
           05  ZCODES1L PIC S9(0004) COMP.
           05  ZCODES1F PIC  X(0001).
           05  FILLER REDEFINES ZCODES1F.
               10  ZCODES1A PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZCODES1I PIC  X(0053).
      *    -------------------------------
           05  ZCODES2L PIC S9(0004) COMP.
           05  ZCODES2F PIC  X(0001).
           05  FILLER REDEFINES ZCODES2F.
               10  ZCODES2A PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZCODES2I PIC  X(0078).
      *    -------------------------------
           05  ZFADAYSL PIC S9(0004) COMP.
           05  ZFADAYSF PIC  X(0001).
           05  FILLER REDEFINES ZFADAYSF.
               10  ZFADAYSA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZFADAYSI PIC  X(0003).
      *    -------------------------------
           05  ZACLOSEL PIC S9(0004) COMP.
           05  ZACLOSEF PIC  X(0001).
           05  FILLER REDEFINES ZACLOSEF.
               10  ZACLOSEA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ZACLOSEI PIC  X(0001).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  ERRMSGI PIC  X(0072).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  FILLER            PIC  X(3).
           05  PFKEYI PIC  99.
       01  EL1044ZO REDEFINES EL1044ZI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  DATEC PIC  X(0001).
           05  DATEH PIC  X(0001).
           05  DATEU PIC  X(0001).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  TIMEC PIC  X(0001).
           05  TIMEH PIC  X(0001).
           05  TIMEU PIC  X(0001).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  CMPNYIDC PIC  X(0001).
           05  CMPNYIDH PIC  X(0001).
           05  CMPNYIDU PIC  X(0001).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  USERIDC PIC  X(0001).
           05  USERIDH PIC  X(0001).
           05  USERIDU PIC  X(0001).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  MAINTC PIC  X(0001).
           05  MAINTH PIC  X(0001).
           05  MAINTU PIC  X(0001).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  LSTUSRC PIC  X(0001).
           05  LSTUSRH PIC  X(0001).
           05  LSTUSRU PIC  X(0001).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  LSTDTEC PIC  X(0001).
           05  LSTDTEH PIC  X(0001).
           05  LSTDTEU PIC  X(0001).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZLETRC PIC  X(0001).
           05  ZLETRH PIC  X(0001).
           05  ZLETRU PIC  X(0001).
           05  ZLETRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZCOPIESC PIC  X(0001).
           05  ZCOPIESH PIC  X(0001).
           05  ZCOPIESU PIC  X(0001).
           05  ZCOPIESO PIC  9.
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZPROMPTC PIC  X(0001).
           05  ZPROMPTH PIC  X(0001).
           05  ZPROMPTU PIC  X(0001).
           05  ZPROMPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZENCCDC PIC  X(0001).
           05  ZENCCDH PIC  X(0001).
           05  ZENCCDU PIC  X(0001).
           05  ZENCCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZRDAYSC PIC  X(0001).
           05  ZRDAYSH PIC  X(0001).
           05  ZRDAYSU PIC  X(0001).
           05  ZRDAYSO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZFORMC PIC  X(0001).
           05  ZFORMH PIC  X(0001).
           05  ZFORMU PIC  X(0001).
           05  ZFORMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZBENEC PIC  X(0001).
           05  ZBENEH PIC  X(0001).
           05  ZBENEU PIC  X(0001).
           05  ZBENEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZACCTC PIC  X(0001).
           05  ZACCTH PIC  X(0001).
           05  ZACCTU PIC  X(0001).
           05  ZACCTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZTYPEC PIC  X(0001).
           05  ZTYPEH PIC  X(0001).
           05  ZTYPEU PIC  X(0001).
           05  ZTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZPRTCRTC PIC  X(0001).
           05  ZPRTCRTH PIC  X(0001).
           05  ZPRTCRTU PIC  X(0001).
           05  ZPRTCRTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZREFREQC PIC  X(0001).
           05  ZREFREQH PIC  X(0001).
           05  ZREFREQU PIC  X(0001).
           05  ZREFREQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZONBASEC PIC  X(0001).
           05  ZONBASEH PIC  X(0001).
           05  ZONBASEU PIC  X(0001).
           05  ZONBASEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZACTSUMC PIC  X(0001).
           05  ZACTSUMH PIC  X(0001).
           05  ZACTSUMU PIC  X(0001).
           05  ZACTSUMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZCSOSUMC PIC  X(0001).
           05  ZCSOSUMH PIC  X(0001).
           05  ZCSOSUMU PIC  X(0001).
           05  ZCSOSUMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZREASONC PIC  X(0001).
           05  ZREASONH PIC  X(0001).
           05  ZREASONU PIC  X(0001).
           05  ZREASONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZBARCODC PIC  X(0001).
           05  ZBARCODH PIC  X(0001).
           05  ZBARCODU PIC  X(0001).
           05  ZBARCODO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZRETENVC PIC  X(0001).
           05  ZRETENVH PIC  X(0001).
           05  ZRETENVU PIC  X(0001).
           05  ZRETENVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZSIGFLGC PIC  X(0001).
           05  ZSIGFLGH PIC  X(0001).
           05  ZSIGFLGU PIC  X(0001).
           05  ZSIGFLGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZCODES1C PIC  X(0001).
           05  ZCODES1H PIC  X(0001).
           05  ZCODES1U PIC  X(0001).
           05  ZCODES1O PIC  X(0053).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZCODES2C PIC  X(0001).
           05  ZCODES2H PIC  X(0001).
           05  ZCODES2U PIC  X(0001).
           05  ZCODES2O PIC  X(0078).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZFADAYSC PIC  X(0001).
           05  ZFADAYSH PIC  X(0001).
           05  ZFADAYSU PIC  X(0001).
           05  ZFADAYSO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ZACLOSEC PIC  X(0001).
           05  ZACLOSEH PIC  X(0001).
           05  ZACLOSEU PIC  X(0001).
           05  ZACLOSEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  ERRMSGC PIC  X(0001).
           05  ERRMSGH PIC  X(0001).
           05  ERRMSGU PIC  X(0001).
           05  ERRMSGO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(3).
           05  PFKEYC PIC  X(0001).
           05  PFKEYH PIC  X(0001).
           05  PFKEYU PIC  X(0001).
           05  PFKEYO PIC  99.
      *    -------------------------------
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
       01  DFHCOMMAREA                     PIC X(1024).
      *                                    COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
010914*                                    COPY ELCENCC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCENCC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
      *   A NAPERSOFT DOCUMENT                                         *
      *                                                                *
      *   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 400   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
      ******************************************************************
       01  ENCLOSURE-CODES.
           12  NC-RECORD-ID                      PIC XX.
               88  VALID-NC-ID                      VALUE 'NC'.
           12  NC-CONTROL-PRIMARY.
               16  NC-COMPANY-CD                 PIC X.
               16  NC-REC-TYPE                   PIC X.
                   88  NC-CLAIMS                   VALUE '1'.
                   88  NC-ADMIN                    VALUE '2'.
               16  NC-ENC-CODE                   PIC X(5).
               16  FILLER                        PIC X(09).
           12  NC-MAINT-INFORMATION.
               16  NC-LAST-MAINT-DT              PIC XX.
               16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  NC-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  NC-OUTPUT-STACK                   PIC XXX.
           12  NC-ENCLOSURE-LINE                 PIC X(100).
           12  NC-ATTACHMENTS                    PIC X(255).
           12  NC-FUTURE                         PIC X(12).
      ******************************************************************
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA TEXT-FILES
                                ENCLOSURE-CODES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1044' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS.
           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
           MOVE 1                      TO EMI-NUMBER-OF-LINES
           
      * EXEC CICS HANDLE CONDITION
      *        DUPREC     (8850-DUPREC)
      *        NOTOPEN    (8870-NOTOPEN)
      *        NOTFND     (8880-NOT-FOUND)
      *        PGMIDERR   (9600-PGMID-ERROR)
      *        ERROR      (9990-ABEND)
      *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00001753' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031373533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           .
       0150-SET-PROGRAM-SAVES.
           IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
               IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM
               ELSE
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6
           ELSE
               GO TO 0200-RECEIVE.
      *    DISPLAY ' COMM KEY PASSED ' PI-COMM-CONTROL
           MOVE PI-COMPANY-CD          TO ELLETR-KEY
           MOVE PI-COMM-CONTROL        TO ELLETR-KEY (2:12)
           MOVE ZEROS                  TO ELLETR-SEQ-NO
           MOVE ELLETR-KEY             TO PI-CURR-LETR-KEY
                                          PI-PREV-LETR-KEY
010914
010914     PERFORM 6500-BUILD-CURRENT-CODES THRU 6500-EXIT
010914
           GO TO 1000-SHOW-LETR-RECORD
           .
       0200-RECEIVE.
           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR.
           IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
               MOVE LOW-VALUES             TO  EL1044ZI
               MOVE ER-7008                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                     TO  MAINTL
               GO TO 8200-SEND-DATAONLY.
           IF PI-PROCESSOR-ID EQUAL 'LGXX'
               NEXT SENTENCE
           ELSE
               
      * EXEC CICS READQ TS
      *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
      *            INTO   (SECURITY-CONTROL)
      *            LENGTH (SC-COMM-LENGTH)
      *            ITEM   (SC-ITEM)
      *        END-EXEC
      *    MOVE '*$II   L              ''   #00001806' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE SC-CREDIT-DISPLAY (3)  TO  PI-DISPLAY-CAP
               MOVE SC-CREDIT-UPDATE  (3)  TO  PI-MODIFY-CAP.
           
      * EXEC CICS RECEIVE
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        INTO     (EL1044ZI)
      *    END-EXEC
           MOVE LENGTH OF
            EL1044ZI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001814' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1044ZI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PFKEYL = +0
               GO TO 0300-CHECK-PFKEYS.
           IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)
               MOVE PF-VALUES (PFKEYI)     TO  EIBAID
           ELSE
               MOVE ER-0029                TO  EMI-ERROR
               GO TO 0320-INPUT-ERROR.
       0300-CHECK-PFKEYS.
           IF EIBAID EQUAL DFHPF23
               GO TO 8810-PF23.
           IF EIBAID EQUAL DFHPF24
               GO TO 9200-RETURN-MAIN-MENU.
           IF EIBAID EQUAL DFHPF12
               GO TO 9500-PF12.
           IF (MAINTL <> 0) AND (EIBAID <> DFHENTER)
               MOVE ER-0050            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR.
           IF EIBAID EQUAL DFHENTER
               GO TO 0330-EDIT-DATA.
           MOVE ER-0029                    TO  EMI-ERROR.
       0320-INPUT-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE AL-UNBON                   TO  PFKEYA.
           MOVE -1                         TO  PFKEYL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       0330-EDIT-DATA.
           IF NOT DISPLAY-CAP
               MOVE 'READ'                 TO  SM-READ
               PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
               MOVE ER-0070                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                     TO  MAINTL
               GO TO 8100-SEND-INITIAL-MAP.
           IF MAINTI EQUAL 'S'
              GO TO 1000-SHOW-LETR-RECORD.
           IF MAINTI = 'C' OR 'A'
              IF NOT MODIFY-CAP
                 PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
                 MOVE ER-0070          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE LOW-VALUES       TO EL1044ZO
                 MOVE -1               TO MAINTL
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF
           IF MAINTI = 'C'
              GO TO 2000-CHANGE-LETR-RECORD
           END-IF
           IF MAINTI = 'A'
              GO TO 3000-ADD-LETR-RECORD
           END-IF
           MOVE ER-0023                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  MAINTL.
           MOVE AL-UABON                   TO  MAINTA.
           GO TO 8200-SEND-DATAONLY.
       1000-SHOW-LETR-RECORD.
           MOVE PI-CURR-LETR-KEY       TO ELLETR-KEY
           
      * EXEC CICS STARTBR
      *        DATASET   (ELLETR-FILE-ID)
      *        RIDFLD    (ELLETR-KEY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001882' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              PERFORM UNTIL I-SAY-TO-STOP
                 OR WE-FOUND-IT
                 
      * EXEC CICS READNEXT
      *              DATASET    (ELLETR-FILE-ID)
      *              SET        (ADDRESS OF TEXT-FILES)
      *              RIDFLD     (ELLETR-KEY)
      *              RESP       (WS-RESPONSE)
      *          END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00001891' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF (RESP-NORMAL)
                    AND (TX-LETTER-NO = PI-COMM-CONTROL (1:4))
                    IF (TX-LINE-SQUEEZE-CONTROL = 'Z')
                       SET WE-FOUND-IT   TO TRUE
                    END-IF
                 ELSE
                    SET I-SAY-TO-STOP  TO TRUE
                 END-IF
              END-PERFORM
           END-IF
           IF WE-FOUND-IT
              MOVE TX-CONTROL-PRIMARY  TO PI-CURR-LETR-KEY
                                          PI-PREV-LETR-KEY
              GO TO 7000-BUILD-OUTPUT-MAP
           END-IF
           MOVE ER-0418                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE -1                     TO MAINTL
           MOVE 'A'                    TO MAINTI
           MOVE AL-UANON               TO MAINTA
           MOVE PI-COMM-CONTROL (1:4)  TO ZLETRI
           MOVE +4                     TO ZLETRL
           MOVE AL-UANON               TO ZLETRA
           GO TO 8100-SEND-INITIAL-MAP
           .
       2000-CHANGE-LETR-RECORD.
      *    display ' curr   ' pi-curr-letr-key
      *    display ' prev   ' pi-prev-letr-key
           IF PI-CURR-LETR-KEY NOT = PI-PREV-LETR-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.
           MOVE PI-CURR-LETR-KEY       TO ELLETR-KEY
           .
       2000-CHANGE-CONTINUE.
           
      * EXEC CICS READ
      *        DATASET    (ELLETR-FILE-ID)
      *        SET        (ADDRESS OF TEXT-FILES)
      *        RIDFLD     (ELLETR-KEY)
      *        UPDATE
      *        RESP       (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00001938' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031393338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NOTFND
              IF ELLETR-SEQ-NO = +0
                 GO TO 2000-CONTINUE-CHANGE
              END-IF
           END-IF
           IF TX-LAST-MAINTENANCED-BY NOT = PI-UPDATE-BY OR
              TX-LAST-MAINTENANCED-DT NOT = PI-UPDATE-DT
               
      * EXEC CICS UNLOCK
      *            DATASET   (ELLETR-FILE-ID)
      *        END-EXEC
      *    MOVE '&*                    #   #00001952' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE ER-0068                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 1000-SHOW-LETR-RECORD.
           MOVE PI-PROCESSOR-ID        TO TX-LAST-MAINTENANCED-BY
           MOVE SAVE-BIN-DATE          TO TX-LAST-MAINTENANCED-DT
           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
           IF ZCOPIESL > +0
              MOVE ZCOPIESO            TO W-NUMBER-OF-COPIES
           END-IF
           IF ZFADAYSL > +0
              MOVE ZFADAYSO            TO W-DAYS-TO-FOLLOW-UP
           END-IF
           IF ZRDAYSL > +0
122011        MOVE ZRDAYSO             TO W-DAYS-TO-RESEND
           END-IF
           IF ZFORML > +0
              MOVE ZFORMI              TO W-FORM-TO-RESEND
           END-IF
           IF ZPROMPTL > +0
              MOVE ZPROMPTI            TO W-PROMPT-LETTER
           END-IF
           IF ZENCCDL > +0
              MOVE ZENCCDI             TO W-ENCLOSURE-CD
           END-IF
           IF ZACLOSEL > +0
              MOVE ZACLOSEI            TO W-AUTO-CLOSE-IND
           END-IF
           IF ZBENEL > +0
              MOVE ZBENEI              TO W-LETTER-TO-BENE
           END-IF
           IF ZACCTL > +0
              MOVE ZACCTI              TO W-LETTER-TO-ACCT
           END-IF
           IF ZTYPEL > +0
              MOVE ZTYPEI              TO W-LETTER-TYPE
           END-IF
           IF ZPRTCRTL > +0
              MOVE ZPRTCRTI            TO W-PRINT-CERTIFICATE
           END-IF
           IF ZREFREQL > +0
              MOVE ZREFREQI            TO W-REFUND-REQUIRED
           END-IF
           IF ZONBASEL > +0
               MOVE ZONBASEI           TO W-ONBASE-CODE
           END-IF
073112
073112     IF ZACTSUML > +0
073112         MOVE ZACTSUMI           TO W-ACCT-SUMM
073112     END-IF
073112
073112     IF ZCSOSUML > +0
073112         MOVE ZCSOSUMI           TO W-CSO-SUMM
073112     END-IF
122712
122712     IF ZREASONL > +0
122712        MOVE ZREASONI            TO W-REASONS-REQUIRED
122712     END-IF
080113
080113     IF ZBARCODL > +0
080113        MOVE ZBARCODI            TO W-ADD-BAR-CODE
080113     END-IF
080113
080113     IF ZRETENVL > +0
080113        MOVE ZRETENVI            TO W-HAS-RETURN-ENV
080113     END-IF
091913
091913     IF ZSIGFLGL > +0
091913        MOVE ZSIGFLGI            TO W-SIG-FLAG-DEFAULT
091913     END-IF
           MOVE W-Z-CONTROL-DATA       TO TX-TEXT-LINE
           .
       2000-CONTINUE-CHANGE.
           
      * EXEC CICS REWRITE
      *        DATASET   (ELLETR-FILE-ID)
      *        FROM      (TEXT-FILES)
      *    END-EXEC.
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002028' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ER-0000                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 1000-SHOW-LETR-RECORD
           .
       3000-ADD-LETR-RECORD.
      *    display ' curr   ' pi-curr-letr-key
      *    display ' prev   ' pi-prev-letr-key
           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
           .
       3000-CONTINUE-ADD.
           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE SPACES                 TO ELLETR-KEY
           MOVE PI-COMPANY-CD          TO ELLETR-COMPANY-CD
           MOVE ZLETRI                 TO ELLETR-ACCESS-CD
           MOVE +0                     TO ELLETR-SEQ-NO
           
      * EXEC CICS READ
      *        DATASET    (ELLETR-FILE-ID)
      *        SET        (ADDRESS OF TEXT-FILES)
      *        RIDFLD     (ELLETR-KEY)
      *        GTEQ
      *        RESP       (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        G          (  N#00002050' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              IF TX-LETTER-NO = ZLETRI
                 IF TX-LINE-SQUEEZE-CONTROL = 'Z'
                    MOVE ER-0132       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    MOVE -1            TO ZLETRL
                    MOVE AL-UABON      TO ZLETRA
                    GO TO 8200-SEND-DATAONLY
                 ELSE
                    MOVE +0            TO WS-SEQ-NO-TO-USE
                 END-IF
              ELSE
                 MOVE +1               TO WS-SEQ-NO-TO-USE
              END-IF
           ELSE
              MOVE +1                  TO WS-SEQ-NO-TO-USE
           END-IF
           
      * EXEC CICS GETMAIN
      *       SET(ADDRESS OF TEXT-FILES)
      *       LENGTH(ELLETR-LENGTH)
      *    END-EXEC
      *    MOVE '," L                  $   #00002075' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELLETR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE SPACES                 TO TEXT-FILES
           MOVE 'TL'                   TO TEXT-FILE-ID
           MOVE PI-COMPANY-CD          TO TX-COMPANY-CD
           MOVE ZLETRI                 TO TX-LETTER-NO
           MOVE WS-SEQ-NO-TO-USE       TO TX-LINE-SEQUENCE
           MOVE 'Z'                    TO TX-LINE-SQUEEZE-CONTROL
           MOVE PI-PROCESSOR-ID        TO TX-LAST-MAINTENANCED-BY
           MOVE SAVE-BIN-DATE          TO TX-LAST-MAINTENANCED-DT
           MOVE SPACES                 TO W-Z-CONTROL-DATA
           IF ZCOPIESL > +0
              MOVE ZCOPIESO            TO W-NUMBER-OF-COPIES
           ELSE
              MOVE 1                   TO W-NUMBER-OF-COPIES
           END-IF
           IF ZFADAYSL > +0
              MOVE ZFADAYSO            TO W-DAYS-TO-FOLLOW-UP
           END-IF
           IF ZRDAYSL > +0
122011        MOVE ZRDAYSO             TO W-DAYS-TO-RESEND
           ELSE
122011        MOVE ZEROS               TO W-DAYS-TO-RESEND
           END-IF
           IF ZFORML > +0
              MOVE ZFORMI              TO W-FORM-TO-RESEND
           END-IF
           IF ZPROMPTL > +0
              MOVE ZPROMPTI            TO W-PROMPT-LETTER
           END-IF
           IF ZENCCDL > +0
              MOVE ZENCCDI             TO W-ENCLOSURE-CD
           END-IF
           IF ZACLOSEL > +0
              MOVE ZACLOSEI            TO W-AUTO-CLOSE-IND
           END-IF
           IF ZBENEL > +0
              MOVE ZBENEI              TO W-LETTER-TO-BENE
           END-IF
           IF ZACCTL > +0
              MOVE ZACCTI              TO W-LETTER-TO-ACCT
           END-IF
           IF ZTYPEL > +0
              MOVE ZTYPEI              TO W-LETTER-TYPE
           END-IF
           IF ZPRTCRTL > +0
              MOVE ZPRTCRTI            TO W-PRINT-CERTIFICATE
           END-IF
           IF ZREFREQL > +0
              MOVE ZREFREQI            TO W-REFUND-REQUIRED
           END-IF
           IF ZONBASEL > +0
              MOVE ZONBASEI            TO W-ONBASE-CODE
           END-IF
073112
073112     IF ZACTSUML > +0
073112         MOVE ZACTSUMI           TO W-ACCT-SUMM
073112     END-IF
073112
073112     IF ZCSOSUML > +0
073112         MOVE ZCSOSUMI           TO W-CSO-SUMM
073112     END-IF
122712
122712     IF ZREASONL > +0
122712        MOVE ZREASONI            TO W-REASONS-REQUIRED
122712     END-IF
080113
080113     IF ZBARCODL > +0
080113        MOVE ZBARCODI            TO W-ADD-BAR-CODE
080113     END-IF
080113
080113     IF ZRETENVL > +0
080113        MOVE ZRETENVI            TO W-HAS-RETURN-ENV
080113     END-IF
091913
091913     IF ZSIGFLGL > +0
091913        MOVE ZSIGFLGI            TO W-SIG-FLAG-DEFAULT
091913     END-IF
           MOVE W-Z-CONTROL-DATA       TO TX-TEXT-LINE
           MOVE TX-CONTROL-PRIMARY     TO ELLETR-KEY
                                          PI-CURR-LETR-KEY
                                          PI-PREV-LETR-KEY
           
      * EXEC CICS WRITE
      *       DATASET (ELLETR-FILE-ID)
      *       FROM    (TEXT-FILES)
      *       RIDFLD  (ELLETR-KEY)
      *    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00002160' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE ER-0000                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 1000-SHOW-LETR-RECORD
           .
       5000-FIND-NEXT-PROD-RECORD.
      *    MOVE PI-PROD-KEY            TO ELLETR-KEY
      *
      *    EXEC CICS STARTBR
      *        DATASET   (ELLETR-FILE-ID)
      *        RIDFLD    (ELLETR-KEY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *
      *    IF NOT RESP-NORMAL
      *       MOVE -1                  TO PFKEYL
      *       MOVE ER-0130             TO EMI-ERROR
      *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
      *       GO TO 8200-SEND-DATAONLY
      *    END-IF
           .
       5000-READNEXT-LOOP.
      *    EXEC CICS READNEXT
      *        DATASET   (ELLETR-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ELLETR-KEY)
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *
      *    IF (NOT RESP-NORMAL)
      *       OR (PD-COMPANY-CD NOT = PI-COMPANY-CD)
      *       PERFORM 5000-END-BROWSE
      *       MOVE -1                  TO PFKEYL
      *       MOVE ER-0130             TO EMI-ERROR
      *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
      *       GO TO 1000-SHOW-LETR-RECORD
      *    END-IF
      *
      *    IF ELLETR-KEY = PI-PREV-LETR-KEY
      *       GO TO 5000-READNEXT-LOOP
      *    END-IF
      *
      *    MOVE ELLETR-KEY             TO PI-PROD-KEY
      *
      *    PERFORM 5000-END-BROWSE
      *    GO TO 7000-BUILD-OUTPUT-MAP
           .
       5000-END-BROWSE.
      *    EXEC CICS ENDBR
      *        DATASET   (ELLETR-FILE-ID)
      *    END-EXEC
           .
       5100-FIND-PREV-PROD-RECORD.
      *    MOVE PI-PREV-LETR-KEY       TO ELLETR-KEY
      *    IF STATEL > +0
      *       MOVE STATEI              TO ELLETR-STATE
      *    END-IF
      *    IF PRODCDL > +0
      *       MOVE PRODCDI             TO ELLETR-PROD-CD
      *    END-IF
      *    IF BENTYPL > +0
      *        MOVE BENTYPI                TO  ELLETR-BEN-TYPE.
      *    IF BENCODEL > +0
      *        MOVE BENCODEI               TO  ELLETR-BEN-CODE.
      *    IF EXPDTL IS GREATER THAN +0
      *        MOVE EXPDTI                 TO  DEEDIT-FIELD
      *        PERFORM 8600-DEEDIT THRU 8600-EXIT
      *        IF WS-NUMVAL-OF-DEEDIT >= 999999
      *            MOVE HIGH-VALUES        TO  ELLETR-EXP-DT
      *        ELSE
      *          STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:12)
      *             DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
      *          END-STRING
      *           MOVE 'L'                    TO  DC-OPTION-CODE
      *            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
      *            IF NO-CONVERSION-ERROR
      *                MOVE DC-BIN-DATE-1  TO  ELLETR-EXP-DT
      *            ELSE
      *                MOVE LOW-VALUES     TO  ELLETR-EXP-DT.
      *
      *    EXEC CICS HANDLE CONDITION
      *        ENDFILE (5100-UNSUCCESSFUL-SEARCH)
      *    END-EXEC.
      *
      *    EXEC CICS STARTBR
      *        DATASET   (ELLETR-FILE-ID)
      *        RIDFLD    (ELLETR-KEY)
      *        GTEQ
      *    END-EXEC.
       5100-READPREV-LOOP.
      *    EXEC CICS READPREV
      *        DATASET   (ELLETR-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ELLETR-KEY)
      *    END-EXEC.
      *
      *    IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
      *        GO TO 5100-UNSUCCESSFUL-SEARCH.
      *
      *    IF ELLETR-KEY IS EQUAL TO PI-PREV-LETR-KEY
      *        GO TO 5100-READPREV-LOOP.
      *
      *    MOVE ELLETR-KEY             TO PI-PROD-KEY
      *
      *    GO TO 7000-BUILD-OUTPUT-MAP.
       5100-END-BROWSE.
      *    EXEC CICS ENDBR
      *        DATASET   (ELLETR-FILE-ID)
      *    END-EXEC.
       5100-UNSUCCESSFUL-SEARCH.
           PERFORM 5100-END-BROWSE.
           MOVE ER-0131                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 1000-SHOW-LETR-RECORD.
           .
       6000-EDIT-INPUT-DATA.
           IF MAINTI = 'A'
              IF ZLETRL > +0
                 AND ZLETRI NOT = SPACES
                 CONTINUE
              ELSE
                 MOVE AL-UNBON         TO ZCOPIESA
                 MOVE -1               TO ZCOPIESL
                 MOVE ER-0005          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF ZCOPIESL > +0
              IF ZCOPIESI NUMERIC
                 CONTINUE
              ELSE
                 MOVE AL-UNBON         TO ZCOPIESA
                 MOVE -1               TO ZCOPIESL
                 MOVE ER-0184          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF ZFADAYSL > +0
             IF ZFADAYSI NOT EQUAL SPACES
                IF ZFADAYSI NUMERIC
                   CONTINUE
                ELSE
                   MOVE AL-UNBON         TO ZFADAYSA
                   MOVE -1               TO ZFADAYSL
                   MOVE ER-0491          TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                END-IF
             END-IF
           END-IF
           IF ZRDAYSL > +0
             IF ZRDAYSI NOT EQUAL SPACES
                IF ZRDAYSI NUMERIC
                   CONTINUE
                ELSE
                   MOVE AL-UNBON         TO ZRDAYSA
                   MOVE -1               TO ZRDAYSL
                   MOVE ER-0491          TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                END-IF
             END-IF
           END-IF
           IF ZPROMPTL > +0
              IF ZPROMPTI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE AL-UABON         TO ZPROMPTA
                 MOVE -1               TO ZPROMPTL
                 MOVE ER-0627          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF MAINTI = 'A'
              AND ZENCCDL <= 0
              MOVE AL-UABON            TO ZENCCDA
              MOVE -1                  TO ZENCCDL
              MOVE ER-1560             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF
           IF ZENCCDL > +0
010914        IF ZENCCDI = '@'
010914            CONTINUE
010914        ELSE
010914           MOVE SPACES             TO ELENCC-KEY
010914           MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
010914           MOVE '2'                TO ELENCC-REC-TYPE
010914           MOVE ZENCCDI            TO ELENCC-ENC-CODE
010914
010914           
      * EXEC CICS READ
010914*              DATASET    (ELENCC-FILE-ID)
010914*              SET        (ADDRESS OF ENCLOSURE-CODES)
010914*              RIDFLD     (ELENCC-KEY)
010914*              RESP       (WS-RESPONSE)
010914*          END-EXEC
      *    MOVE '&"S        E          (  N#00002352' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELENCC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
010914
010914           IF RESP-NORMAL
010914              CONTINUE
010914           ELSE
010914              MOVE AL-UABON         TO ZENCCDA
010914              MOVE -1               TO ZENCCDL
010914              MOVE ER-1560          TO EMI-ERROR
010914              PERFORM 9900-ERROR-FORMAT
010914                                 THRU 9900-EXIT
010914           END-IF
              END-IF
           END-IF
           IF MAINTI = 'A'
              AND ZTYPEL <= 0
              MOVE AL-UABON            TO ZTYPEA
              MOVE -1                  TO ZTYPEL
              MOVE ER-1564             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF
           IF ZTYPEL > +0
092419        IF ZTYPEI = 'C' OR 'P' OR 'U' OR 'A'
                 CONTINUE
              ELSE
                 MOVE -1               TO ZTYPEL
                 MOVE AL-UABON         TO ZTYPEA
                 MOVE ER-1564          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF ZACLOSEL > +0
              IF ZACLOSEI = 'S' OR 'C' OR 'B' OR ' '
                 CONTINUE
              ELSE
                 MOVE -1               TO ZACLOSEL
                 MOVE AL-UABON         TO ZACLOSEA
                 MOVE ER-1562          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF ZPRTCRTL > +0
              IF ZPRTCRTI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE AL-UABON         TO ZPRTCRTA
                 MOVE -1               TO ZPRTCRTL
                 MOVE ER-0627          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF ZREFREQL > +0
              IF ZREFREQI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE AL-UABON         TO ZREFREQA
                 MOVE -1               TO ZREFREQL
                 MOVE ER-0627          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF MAINTI = 'A'
              AND ZONBASEL <= 0
              MOVE AL-UABON            TO ZONBASEA
              MOVE -1                  TO ZONBASEL
              MOVE ER-3804             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF
           IF ZONBASEL > +0
              IF ZONBASEI = ' '
                 MOVE AL-UABON            TO ZONBASEA
                 MOVE -1                  TO ZONBASEL
                 MOVE ER-3804             TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
073112
073112     IF ZACTSUML > +0
031516        IF ZACTSUMI = '1' OR '2' OR '5' or ' '
073112           CONTINUE
073112        ELSE
073112           MOVE AL-UABON         TO ZACTSUMA
073112           MOVE -1               TO ZACTSUML
073112           MOVE ER-3820          TO EMI-ERROR
073112           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
073112        END-IF
073112     END-IF
073112
073112     IF ZCSOSUML > +0
073112        IF ZCSOSUMI = '3' OR '4' OR ' '
073112           CONTINUE
073112        ELSE
073112           MOVE AL-UABON         TO ZCSOSUMA
073112           MOVE -1               TO ZCSOSUML
073112           MOVE ER-3821          TO EMI-ERROR
073112           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
073112        END-IF
073112     END-IF
073112
122712     IF ZREASONL > +0
122712        IF ZREASONI = 'Y' OR 'N' OR ' '
122712           CONTINUE
122712        ELSE
122712           MOVE AL-UABON         TO ZREASONA
122712           MOVE -1               TO ZREASONL
122712           MOVE ER-0627          TO EMI-ERROR
122712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122712        END-IF
122712     END-IF
122712
080113     IF ZBARCODL > +0
080113        IF ZBARCODI = 'Y' OR 'N' OR ' '
080113           CONTINUE
080113        ELSE
080113           MOVE AL-UABON         TO ZBARCODA
080113           MOVE -1               TO ZBARCODL
080113           MOVE ER-0627          TO EMI-ERROR
080113           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080113        END-IF
080113     END-IF
080113
080113     IF ZRETENVL > +0
080113        IF ZRETENVI = 'Y' OR 'N' OR ' '
080113           CONTINUE
080113        ELSE
080113           MOVE AL-UABON         TO ZRETENVA
080113           MOVE -1               TO ZRETENVL
080113           MOVE ER-0627          TO EMI-ERROR
080113           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080113        END-IF
080113     END-IF
080113
091913     IF ZSIGFLGL > +0
091913        IF ZSIGFLGI = 'Y' OR 'N' OR ' '
091913           CONTINUE
091913        ELSE
091913           MOVE AL-UABON         TO ZSIGFLGA
091913           MOVE -1               TO ZSIGFLGL
091913           MOVE ER-0627          TO EMI-ERROR
091913           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
091913        END-IF
091913     END-IF
091913
           .
       6000-EXIT.
           EXIT.
010914
010914
010914 6500-BUILD-CURRENT-CODES.
010914      MOVE SPACES             TO ELENCC-KEY
010914      MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
010914      MOVE '2'                TO ELENCC-REC-TYPE
010914      MOVE LOW-VALUES         TO ELENCC-ENC-CODE
010914      MOVE SPACES             TO HOLD-ENC-CODES
010914      MOVE '@'                TO HOLD-ENC-CODES (1:1)
010914      MOVE HOLD-ENC-CODES     TO PI-ENC-CODES
010914
010914      
      * EXEC CICS STARTBR
010914*        DATASET   (ELENCC-FILE-ID)
010914*        RIDFLD    (ELENCC-KEY)
010914*        GTEQ
010914*        RESP      (WS-RESPONSE)
010914*     END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002513' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELENCC-FILE-ID, 
                 ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
010914
010914      IF RESP-NORMAL
010914        PERFORM UNTIL DONE-WITH-ENCC
010914           MOVE PI-ENC-CODES TO HOLD-ENC-CODES
010914           
      * EXEC CICS READNEXT
010914*              DATASET    (ELENCC-FILE-ID)
010914*              SET        (ADDRESS OF ENCLOSURE-CODES)
010914*              RIDFLD     (ELENCC-KEY)
010914*              RESP       (WS-RESPONSE)
010914*          END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00002523' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELENCC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
010914           IF (RESP-NORMAL)
010914              AND (ELENCC-COMPANY-CD = PI-COMPANY-CD)
010914              AND (ELENCC-REC-TYPE = '2')
010914                 PERFORM VARYING WS-SUB1 FROM 5 BY -1
010914                      UNTIL ELENCC-ENC-CODE (WS-SUB1:1) <> ' '
010914                 END-PERFORM
010914                 IF WS-SUB1 > 2
010914                     SUBTRACT +1 FROM WS-SUB1
010914                 END-IF
010914                 MOVE ELENCC-ENC-CODE (WS-SUB1:2) TO
010914                             WS-TEST-STATE
010914                 IF STATE-SPECIFIC AND
010914                    ELENCC-ENC-CODE NOT = 'ENV'
010914                    CONTINUE
010914                 ELSE
010914                    STRING HOLD-ENC-CODES DELIMITED BY ' '
010914                         ','
010914                         ELENCC-ENC-CODE DELIMITED BY ' '
010914                    INTO PI-ENC-CODES
010914                 END-IF
010914           ELSE
010914              SET DONE-WITH-ENCC  TO TRUE
010914           END-IF
010914        END-PERFORM
010914     END-IF
010914
010914     MOVE +53 TO PI-SPLIT-SUB
010914     PERFORM UNTIL PI-ENC-CODES (PI-SPLIT-SUB:1) = ' ' OR ','
010914         SUBTRACT +1 FROM PI-SPLIT-SUB
010914     END-PERFORM
010914
010914     .
010914 6500-EXIT.
010914     EXIT.
           EJECT
       7000-BUILD-OUTPUT-MAP.
           MOVE LOW-VALUES             TO EL1044ZO
           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
           MOVE TX-LETTER-NO           TO ZLETRO
           MOVE W-NUMBER-OF-COPIES     TO ZCOPIESO
           MOVE W-DAYS-TO-FOLLOW-UP    TO ZFADAYSO
122011     MOVE W-DAYS-TO-RESEND       TO ZRDAYSO
           MOVE W-FORM-TO-RESEND       TO ZFORMO
           MOVE W-PROMPT-LETTER        TO ZPROMPTO
           MOVE W-ENCLOSURE-CD         TO ZENCCDO
           MOVE W-AUTO-CLOSE-IND       TO ZACLOSEO
           MOVE W-LETTER-TO-BENE       TO ZBENEO
           MOVE W-LETTER-TO-ACCT       TO ZACCTO
           MOVE W-LETTER-TYPE          TO ZTYPEO
           MOVE W-PRINT-CERTIFICATE    TO ZPRTCRTO
           MOVE W-REFUND-REQUIRED      TO ZREFREQO
           MOVE W-ONBASE-CODE          TO ZONBASEO
073112     MOVE W-ACCT-SUMM            TO ZACTSUMO
073112     MOVE W-CSO-SUMM             TO ZCSOSUMO
122712     MOVE W-REASONS-REQUIRED     TO ZREASONO
080113     MOVE W-ADD-BAR-CODE         TO ZBARCODO
080113     MOVE W-HAS-RETURN-ENV       TO ZRETENVO
091913     MOVE W-SIG-FLAG-DEFAULT     TO ZSIGFLGO
010914     MOVE PI-SPLIT-SUB           TO WS-SUB1
010914     COMPUTE WS-SUB2 = PI-SPLIT-SUB + 1
010914     COMPUTE WS-SUB3 = 120 - PI-SPLIT-SUB
010914     MOVE PI-ENC-CODES (1:WS-SUB1) TO ZCODES1O
010914     MOVE PI-ENC-CODES (WS-SUB2:WS-SUB3) TO ZCODES2O
           MOVE TX-LAST-MAINTENANCED-BY TO LSTUSRO
                                          PI-UPDATE-BY
           MOVE TX-LAST-MAINTENANCED-DT TO PI-UPDATE-DT
                                          DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-DAYS
                                           DC-ELAPSED-MONTHS.
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO
           ELSE
              MOVE LOW-VALUES          TO LSTDTEO
           END-IF
           MOVE -1                     TO MAINTL
           .
       8100-SEND-INITIAL-MAP.
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSGO.
           MOVE EIBTIME                    TO  TIME-IN.
           MOVE SAVE-DATE                  TO  DATEO.
           MOVE TIME-OUT                   TO  TIMEO
           MOVE PI-COMPANY-ID          TO CMPNYIDO
           MOVE PI-PROCESSOR-ID        TO USERIDO
           
      * EXEC CICS SEND
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL1044ZO)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL1044ZO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002614' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1044ZO, 
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
           
           MOVE '044Z'                 TO PI-CURRENT-SCREEN-NO
           GO TO 9100-RETURN-TRAN
           .
       8200-SEND-DATAONLY.
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSGO.
           MOVE EIBTIME                    TO  TIME-IN.
           MOVE SAVE-DATE                  TO  DATEO.
           MOVE TIME-OUT                   TO  TIMEO.
           MOVE PI-COMPANY-ID          TO CMPNYIDO
           MOVE PI-PROCESSOR-ID        TO USERIDO
           
      * EXEC CICS SEND
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL1044ZO)
      *        DATAONLY
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL1044ZO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002632' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1044ZO, 
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
           
           GO TO 9100-RETURN-TRAN
           .
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM  (LOGOFF-TEXT)
      *        LENGTH(LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002642' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363432' TO DFHEIV0(25:11)
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
           
           
      * EXEC CICS RETURN
      *        END-EXEC.
      *    MOVE '.(                    ''   #00002648' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           .
       8500-DEEDIT.
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-9V999-OF-DEEDIT
           .
       8500-EXIT.
           EXIT.
       8600-DEEDIT.
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-NUMVAL-OF-DEEDIT
           .
       8600-EXIT.
           EXIT.
       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG               TO  LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
       8810-PF23.
           MOVE EIBAID                     TO  PI-ENTRY-CD-1.
           MOVE XCTL-005                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       8850-DUPREC.
           MOVE ER-0132                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  MAINTL
           MOVE AL-UABON                   TO  ZLETRA
           GO TO 8100-SEND-INITIAL-MAP.
       8870-NOTOPEN.
           MOVE LOW-VALUES                 TO  EL1044ZO.
           MOVE -1                         TO  MAINTL.
           MOVE ER-0701                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 8100-SEND-INITIAL-MAP.
       8880-NOT-FOUND.
           MOVE ER-0702                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  MAINTL
           MOVE AL-UABON                   TO  ZLETRA
           GO TO 8100-SEND-INITIAL-MAP.
           .
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.
           
      * EXEC CICS RETURN
      *        TRANSID    (TRANS-ID)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.(CT                  ''   #00002691' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   %   #00002700' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.
           GO TO 9300-XCTL.
       9500-PF12.
           MOVE XCTL-010                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       9600-PGMID-ERROR.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR   (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! # #00002712' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.
           MOVE ' '                        TO  PI-ENTRY-CD-1.
           MOVE XCTL-005                   TO  PGM-NAME.
           MOVE PGM-NAME                   TO  LOGOFF-PGM.
           MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.
           GO TO 9300-XCTL.
           EJECT
       9700-LINK-DATE-CONVERT.
           
      * EXEC CICS LINK
      *        PROGRAM    ('ELDATCV')
      *        COMMAREA   (DATE-CONVERSION-DATA)
      *        LENGTH     (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002723' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
           EXIT.
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
               MOVE LINK-001               TO  PGM-NAME
               
      * EXEC CICS LINK
      *            PROGRAM    (PGM-NAME)
      *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
      *            LENGTH     (EMI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '."C                   (   #00002733' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-EXIT.
           EXIT.
       9990-ABEND.
           MOVE LINK-004                   TO  PGM-NAME.
           MOVE DFHEIBLK                   TO  EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00002743' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       9995-SECURITY-VIOLATION.
      *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00002768' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
       9995-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1044' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8850-DUPREC,
                     8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1044' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
