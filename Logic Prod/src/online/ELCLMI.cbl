       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 ELCLMI.
      *                            VMOD=2.001
      *AUTHOR.     CSO
      *            OMAHA, NEBRASKA
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      ******************************************************************
      *  REMARKS   *                                                   *
      *            *    THIS 'SUBROUTINE' WILL, CALCULATE THE LIFE     *
      *            *    CLAIM INTEREST DUE DEPENDING ON THE STATE      *
060608******************************************************************
060608*                   C H A N G E   L O G
060608*
060608* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060608*-----------------------------------------------------------------
060608*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060608* EFFECTIVE    NUMBER
060608*-----------------------------------------------------------------
060608* 060608    2008040300002  PEMA  ADD SPEC CALC FOR UT AND WA
082814* 082814    2014082800001  PEMA  CORRECT UT CLAIM IN CALC
060608******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           EJECT
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER   PIC X(32) VALUE '********************************'.
       77  FILLER   PIC X(32) VALUE '**  ELCLMI  WORKING STORAGE   **'.
       77  FILLER   PIC X(32) VALUE '***********VMOD 2.001 **********'.
       77  SAVE-DATE                   PIC X(8)     VALUE SPACES.
       77  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
       77  WS-SELECT-DATE              PIC XX VALUE LOW-VALUES.
       77  WS-REQ-FROM-DT              PIC XX VALUE LOW-VALUES.
       77  WS-REQ-END-DT               PIC XX VALUE LOW-VALUES.
       77  WS-PAY-FROM-DT              PIC XX VALUE LOW-VALUES.
       77  WS-PAY-TO-DT                PIC XX VALUE LOW-VALUES.
       77  WS-SUB-FROM-DT              PIC XX VALUE LOW-VALUES.
       77  WS-SUB-TO-DT                PIC XX VALUE LOW-VALUES.
       77  WS-CALC-START-DT            PIC XX VALUE LOW-VALUES.
       77  WS-CALC-END-DT              PIC XX VALUE LOW-VALUES.
       77  WS-INTEREST                 PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-TOTAL-INTEREST           PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-DAYS                     PIC S9(5)    COMP-3 VALUE +0.
       77  WS-TOTAL-DAYS               PIC S9(5)    COMP-3 VALUE +0.
       77  WS-ELCISC-BROWSE-SW         PIC X  VALUE SPACES.
           88  ELCISC-BROWSE-STARTED          VALUE 'Y'.
       77  WS-ELCISC-SW                PIC X  VALUE SPACES.
           88  ELCISC-FOUND                   VALUE 'Y'.
       77  WS-ELCISB-SW                PIC X  VALUE SPACES.
           88  END-OF-BREAKOUT                VALUE 'Y'.
       77  WS-ELCIST-SW                PIC X  VALUE SPACES.
           88  END-OF-SCHED-TABLE             VALUE 'Y'.
       77  WS-CO-HLD-DAYS              PIC S9(5) COMP-3 VALUE +0.
       77  WS-CO-TOTAL-DAYS            PIC S9(5) COMP-3 VALUE +0.
       77  WS-CO-SW                    PIC X VALUE SPACES.
           88  CO-CHECKED                    VALUE 'Y'.
       77  WS-WA-DAYSA                 PIC S9(5) COMP-3 VALUE +0.
       77  WS-WA-DAYSB                 PIC S9(5) COMP-3 VALUE +0.
082814 01  GETMAIN-SPACE           PIC  X      VALUE SPACE.
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-ERROR                   VALUE +01.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
       01  WS-HOLD-ELCISC              PIC X(100) VALUE SPACES.
       01  WS-SC-SAVE-KEY              PIC X(10)  VALUE LOW-VALUES.
       01  WS-SB-SAVE-KEY              PIC X(12)  VALUE LOW-VALUES.
       01  WS-ST-SAVE-KEY              PIC X(6)   VALUE LOW-VALUES.
       01  WS-SC-CONTROL-PRIMARY.
           10  WS-SC-COMPANY-CD        PIC X      VALUE SPACES.
           10  WS-SC-STATE             PIC XX     VALUE SPACES.
           10  WS-SC-PRODUCT           PIC XX     VALUE SPACES.
           10  WS-SC-COVERAGE          PIC XX     VALUE SPACES.
           10  WS-SC-EXCESS-DAYS       PIC 999    VALUE ZEROS.
       01  WS-SB-CONTROL-PRIMARY.
           10  WS-SB-COMPANY-CD        PIC X      VALUE SPACES.
           10  WS-SB-STATE             PIC XX     VALUE SPACES.
           10  WS-SB-PRODUCT           PIC XX     VALUE SPACES.
           10  WS-SB-COVERAGE          PIC XX     VALUE SPACES.
           10  WS-SB-BREAKOUT-CODE     PIC XX     VALUE SPACES.
           10  WS-SB-CALC-END          PIC 999    VALUE ZEROS.
       01  WS-ST-CONTROL-PRIMARY.
           10  WS-ST-COMPANY-CD        PIC X      VALUE SPACES.
           10  WS-ST-SCHED-CODE        PIC XXX    VALUE SPACES.
           10  WS-ST-END-DT            PIC XX     VALUE LOW-VALUES.
       01  WS-IR-CONTROL-PRIMARY.
           10  WS-IR-COMPANY-CD        PIC X      VALUE SPACES.
           10  WS-IR-INT-RATE-CODE     PIC XXX    VALUE SPACES.
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
      *                                COPY ELCICALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCICALC.                           *
00005 *                            VMOD=2.001                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO THE CLAIM INTEREST        *
00008 *                 CALCULATION MODULE                             *
00011 *                                                                *
00012 *  PASSED TO ELCLMI                                              *
00013 *  -----------------                                             *
00014 *  STATE                                                         *
00015 *  PRODUCT                                                       *
00016 *  COVERAGE                                                      *
      *  EFFECTIVE DATE                                                *
00017 *  INCURRED DATE                                                 *
00018 *  ESTABLISHED DATE                                              *
00019 *  LAST PAID DATE                                                *
00020 *  REPORTED DATE                                                 *
00021 *  CLAIM PAYMENT AMOUNT                                          *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  RETURN CODE                                                   *
00026 *  CLAIM INTEREST DUE SWITCH Y/N                                 *
00027 *  CLAIM INTEREST AMOUNT                                         *
00029 *----------------------------------------------------------------*
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 100                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
120905* 120905    2004040700004  PEMA  NEW COPYBOOK
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
010303******************************************************************
00170
00171  01  CLAIM-INT-PASS-AREA.
00172      12  CP-CLAIM-LENGTH           PIC S9(4)         VALUE +100
00173                                      COMP.
00174
00175      12  CI-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CI-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'.
00179        88  CP-ERROR-IN-SEL-CRITERIA                VALUE '1'.
00180        88  CP-NOT-QUAL-LEVEL-1                     VALUE '2'.
00181        88  CP-NOT-QUAL-LEVEL-2                     VALUE '3'.
00182        88  CP-AMT-LT-5000                          VALUE '4'.
00183        88  CP-ERROR-IN-DATES                       VALUE '5'.
00184        88  CP-ERROR-IN-ST-BREAKOUT                 VALUE '6'.
00185        88  CP-ERROR-IN-SCHEDULES                   VALUE '7'.
00185        88  CP-INTEREST-RATE-IS-ZERO                VALUE '8'.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
               16  CP-COMPANY-CD         PIC X.
00202          16  CP-STATE              PIC XX.
00203          16  CP-PRODUCT            PIC XX.
00204          16  CP-COVERAGE           PIC XX.
00205          16  CP-INC-DT             PIC XX.
00205          16  CP-EST-DT             PIC XX.
00205          16  CP-LSTPD-DT           PIC XX.
00205          16  CP-RPT-DT             PIC XX.
               16  CP-PRF-DT             PIC XX.
               16  CP-EFF-DT             PIC XX.
               16  CP-CLAIM-AMT          PIC S9(7)V99 COMP-3.
               16  CP-INT-RATE           PIC S99V9(5) COMP-3.
090803         16  FILLER                PIC X(37).
00363
00364 ***************    OUTPUT FROM ELCLMI   ************************
00365
00366          16  CP-CLM-INT-SW         PIC X         VALUE SPACES.
00367
00368          16  CP-CLM-INT-AMT        PIC S9(7)V99  VALUE +0 COMP-3.
               16  CP-CLM-INT-RATE       PIC S99V9(5)  VALUE +0 COMP-3.
               16  CP-CLM-INT-NODAYS     PIC S9(5)     VALUE +0 COMP-3.
010303         16  FILLER                PIC X(19).
00514 ******************************************************************
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
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
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
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
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA                 PIC X(100).
      *                                COPY ELCCISC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCCISC.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = LIFE CLAIM INTEREST SELECTION CRITERIA    *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 100  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCISC                        RKP=2,LEN=10    *
      *       ALTERNATE INDEX = NONE                                   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 053105                   PEMA  NEW FILE AND COPYBOOK
      ******************************************************************
      *
       01  CLAIM-INTEREST-SC.
           12  SC-RECORD-ID                       PIC XX.
               88  VALID-SC-ID                        VALUE 'SC'.
           12  SC-CONTROL-PRIMARY.
               16  SC-COMPANY-CD                  PIC X.
               16  SC-STATE                       PIC XX.
               16  SC-PRODUCT                     PIC XX.
               16  SC-COVERAGE                    PIC XX.
092706         16  SC-EXCESS-DAYS                 PIC 999.
           12  SC-LAST-MAINT-BY                   PIC X(4).
           12  SC-LAST-MAINT-DT                   PIC XX.
           12  SC-LAST-MAINT-HHMMSS               PIC S9(6) COMP-3.
           12  SC-S-TYPE                          PIC X.
           12  SC-SA-DATE                         PIC XX.
           12  SC-R-TYPE                          PIC X.
           12  SC-P-DAYS                          PIC S999.
           12  SC-CS-TYPE                         PIC X.
           12  SC-CS-DAYS                         PIC S999.
           12  SC-CE-TYPE                         PIC X.
           12  SC-BREAKOUT-CODE                   PIC XX.
           12  FILLER                             PIC X(64).
      *                                COPY ELCCISB.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCCISB.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = LIFE CLAIM INTEREST STATE BREAK OUT TABLE *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 100  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCISB                        RKP=2,LEN=12    *
      *       ALTERNATE INDEX = NONE                                   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 053105                   PEMA  NEW FILE AND COPYBOOK
      ******************************************************************
      *
       01  CLAIM-INTEREST-SB.
           12  SB-RECORD-ID                       PIC XX.
               88  VALID-SB-ID                        VALUE 'SB'.
           12  SB-CONTROL-PRIMARY.
               16  SB-COMPANY-CD                  PIC X.
               16  SB-STATE                       PIC XX.
               16  SB-PRODUCT                     PIC XX.
               16  SB-COVERAGE                    PIC XX.
               16  SB-BREAKOUT-CODE               PIC XX.
               16  SB-CALC-END                    PIC 999.
           12  SB-LAST-MAINT-BY                   PIC X(4).
           12  SB-LAST-MAINT-DT                   PIC XX.
           12  SB-LAST-MAINT-HHMMSS               PIC S9(6) COMP-3.
           12  SB-CALC-START                      PIC 999.
           12  SB-SCHED-CODE                      PIC XXX.
           12  FILLER                             PIC X(70).
      *                                COPY ELCCIST.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCCIST.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = LIFE CLAIM INTEREST SCHEDULE TABLE        *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 100  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCIST                        RKP=2,LEN=06    *
      *       ALTERNATE INDEX = NONE                                   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 053105                   PEMA  NEW FILE AND COPYBOOK
      ******************************************************************
      *
       01  CLAIM-INTEREST-ST.
           12  ST-RECORD-ID                       PIC XX.
               88  VALID-ST-ID                        VALUE 'ST'.
           12  ST-CONTROL-PRIMARY.
               16  ST-COMPANY-CD                  PIC X.
               16  ST-SCHED-CODE                  PIC XXX.
               16  ST-END-DATE                    PIC XX.
           12  ST-LAST-MAINT-BY                   PIC X(4).
           12  ST-LAST-MAINT-DT                   PIC XX.
           12  ST-LAST-MAINT-HHMMSS               PIC S9(6) COMP-3.
           12  ST-START-DATE                      PIC XX.
           12  ST-INT-RATE-CODE                   PIC XXX.
           12  FILLER                             PIC X(77).
      *                                COPY ELCCIIR.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCCIIR.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = LIFE CLAIM INTEREST RATES                 *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 100  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCIIR                        RKP=2,LEN=04    *
      *       ALTERNATE INDEX = NONE                                   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 053105                   PEMA  NEW FILE AND COPYBOOK
      ******************************************************************
      *
       01  CLAIM-INTEREST-IR.
           12  IR-RECORD-ID                       PIC XX.
               88  VALID-IR-ID                        VALUE 'IR'.
           12  IR-CONTROL-PRIMARY.
               16  IR-COMPANY-CD                  PIC X.
               16  IR-INT-RATE-CODE               PIC XXX.
           12  IR-LAST-MAINT-BY                   PIC X(4).
           12  IR-LAST-MAINT-DT                   PIC XX.
           12  IR-LAST-MAINT-HHMMSS               PIC S9(6) COMP-3.
           12  IR-DESCRIPTION                     PIC X(25).
           12  IR-INT-RATE                        PIC S99V9(5).
           12  FILLER                             PIC X(52).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-INTEREST-SC
                                CLAIM-INTEREST-SB CLAIM-INTEREST-ST
                                CLAIM-INTEREST-IR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELCLMI' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE DFHCOMMAREA            TO CLAIM-INT-PASS-AREA
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9100-CONVERT-DATE   THRU 9100-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
           .
       0000-START-INTEREST-CALC.
           MOVE ZERO                   TO CI-RETURN-CODE
                                          CP-CLM-INT-AMT
                                          CP-CLM-INT-RATE
                                          CP-CLM-INT-NODAYS
                                          WS-INTEREST
                                          WS-TOTAL-INTEREST
                                          WS-TOTAL-DAYS
      *****************************************************************
      *
      * IF THEY HAVEN'T MADE A PAYMENT YET THEN USE THE CURRENT DATE
      *
      *****************************************************************
           IF CP-LSTPD-DT = ZEROS OR SPACES OR LOW-VALUES
              MOVE SAVE-BIN-DATE       TO CP-LSTPD-DT
           END-IF
      *****************************************************************
           MOVE 'Y'                    TO CP-CLM-INT-SW
           MOVE CP-COMPANY-CD          TO WS-SC-COMPANY-CD
           MOVE CP-STATE               TO WS-SC-STATE
           MOVE CP-PRODUCT             TO WS-SC-PRODUCT
           MOVE CP-COVERAGE            TO WS-SC-COVERAGE
           MOVE ZEROS                  TO WS-SC-EXCESS-DAYS
           MOVE WS-SC-CONTROL-PRIMARY  TO WS-SC-SAVE-KEY
           PERFORM 0005-FIND-ELCISC    THRU 0005-EXIT
           IF (NOT RESP-NORMAL)
                    OR
              (WS-SC-SAVE-KEY (1:7) NOT =
                                       WS-SC-CONTROL-PRIMARY (1:7))
              MOVE WS-SC-SAVE-KEY      TO WS-SC-CONTROL-PRIMARY
              MOVE '**'                TO WS-SC-PRODUCT
                                          CP-PRODUCT
              MOVE WS-SC-CONTROL-PRIMARY
                                       TO WS-SC-SAVE-KEY
              PERFORM 0010-FIND-ELCISC THRU 0010-EXIT
           END-IF
           IF (RESP-NORMAL)
              AND (WS-SC-SAVE-KEY (1:7) =
                           WS-SC-CONTROL-PRIMARY (1:7))
              PERFORM 0030-CALC-CLM-INT THRU 0030-EXIT
              MOVE WS-TOTAL-INTEREST   TO CP-CLM-INT-AMT
              MOVE IR-INT-RATE         TO CP-CLM-INT-RATE
              MOVE WS-TOTAL-DAYS       TO CP-CLM-INT-NODAYS
           ELSE
              MOVE '1'                 TO CI-RETURN-CODE
              MOVE 'N'                 TO CP-CLM-INT-SW
           END-IF
           MOVE CLAIM-INT-PASS-AREA    TO DFHCOMMAREA
           
      * EXEC CICS RETURN
      *    END-EXEC
      *    MOVE '.(                    ''   #00000706' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      *****************************************************************
      *
      * FIND THE SELECTION CRITERIA RECORD FOR THE CLAIM
      * IF NOT FOUND THE FIRST TIME THEN TRY USING THE WILD KEY **
      *
      *****************************************************************
           .
       0005-FIND-ELCISC.
           PERFORM 0012-ELCISC-STARTBR THRU 0012-EXIT
           IF RESP-NORMAL
              PERFORM 0020-ELCISC-READNEXT
                                       THRU 0020-EXIT
           END-IF
           .
       0005-EXIT.
           EXIT.
       0010-FIND-ELCISC.
           IF ELCISC-BROWSE-STARTED
              PERFORM 0015-ELCISC-RESETBR
                                       THRU 0015-EXIT
           ELSE
              PERFORM 0012-ELCISC-STARTBR
                                       THRU 0012-EXIT
           END-IF
           IF RESP-NORMAL
              PERFORM 0020-ELCISC-READNEXT
                                       THRU 0020-EXIT
           END-IF
           .
       0010-EXIT.
           EXIT.
       0012-ELCISC-STARTBR.
           
      * EXEC CICS STARTBR
      *        DATASET   ('ELCISC')
      *        RIDFLD    (WS-SC-CONTROL-PRIMARY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCISC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00000740' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-SC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              SET ELCISC-BROWSE-STARTED
                                       TO TRUE
           END-IF
           .
       0012-EXIT.
           EXIT.
       0015-ELCISC-RESETBR.
           
      * EXEC CICS RESETBR
      *        DATASET   ('ELCISC')
      *        RIDFLD    (WS-SC-CONTROL-PRIMARY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCISC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &  N#00000754' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-SC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              SET ELCISC-BROWSE-STARTED
                                       TO TRUE
           END-IF
           .
       0015-EXIT.
           EXIT.
       0017-ELCISC-ENDBR.
           
      * EXEC CICS ENDBR
      *        DATASET   ('ELCISC')
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCISC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $  N#00000768' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'204E233030303030373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0017-EXIT.
           EXIT.
       0020-ELCISC-READNEXT.
           
      * EXEC CICS READNEXT
      *        DATASET  ('ELCISC')
      *        SET      (ADDRESS OF CLAIM-INTEREST-SC)
      *        RIDFLD   (WS-SC-CONTROL-PRIMARY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCISC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00000776' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303030373736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-SC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-INTEREST-SC TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0020-EXIT.
           EXIT.
      *****************************************************************
      *
      * FIND OUT IF THE INCURRED DATE OR THE ESTABLISH DATE IS BEYOND
      * THE SELECT DATE, IF NOT THEN THERE IS NO REQUIREMENT TO
      * CALCULATE THE INTEREST
      *
      *****************************************************************
       0030-CALC-CLM-INT.
           IF SC-S-TYPE = 'I'
              MOVE CP-INC-DT           TO WS-SELECT-DATE
           ELSE
              IF SC-S-TYPE = 'R'
                 MOVE CP-RPT-DT        TO WS-SELECT-DATE
              ELSE
                 MOVE CP-PRF-DT        TO WS-SELECT-DATE
              END-IF
           END-IF
           IF WS-SELECT-DATE >= SC-SA-DATE
              PERFORM 0040-INT-TYPE    THRU 0040-EXIT
           ELSE
              MOVE 'N'                 TO CP-CLM-INT-SW
              MOVE '2'                 TO CI-RETURN-CODE
           END-IF
           .
       0030-EXIT.
           EXIT.
      *****************************************************************
      *
      * ESTABLISH THE EARLIEST REQUIRED DATE BASED ON THE REQUIRE TYPE
      * I = INCURRED DATE, R = REPORTED DATE.
      * ESTABLISH THE LATEST REQUIRE DATE USING THE PAID DATE.
      * CALCULATE THE DURATION BETWEEN THE EARLIEST DATE AND THE
      * LATEST.  IF THE DURATION IS LESS THAN THE PLUS DAYS
      * THEN THE CLAIM IS NOT REQUIRED TO CALCULATE INTEREST
      *
      *****************************************************************
       0040-INT-TYPE.
           IF SC-R-TYPE = 'I'
              MOVE CP-INC-DT           TO WS-REQ-FROM-DT
           ELSE
              IF SC-R-TYPE = 'R'
                 MOVE CP-RPT-DT        TO WS-REQ-FROM-DT
              ELSE
                 MOVE CP-PRF-DT        TO WS-REQ-FROM-DT
              END-IF
           END-IF
           MOVE CP-LSTPD-DT            TO WS-REQ-END-DT
           MOVE WS-REQ-FROM-DT         TO DC-BIN-DATE-1
           MOVE WS-REQ-END-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9100-CONVERT-DATE   THRU 9100-EXIT
           IF NO-CONVERSION-ERROR
              IF (DC-ELAPSED-DAYS > SC-P-DAYS)
                             OR
                 ((SC-STATE = 'UT')
                  AND (CP-EFF-DT >= X'A285'))
                 CONTINUE
              ELSE
                 MOVE 'N'              TO CP-CLM-INT-SW
                 MOVE '3'              TO CI-RETURN-CODE
                 GO TO 0040-EXIT
              END-IF
           END-IF
      *****************************************************************
      *
      * FIGURE OUT IF I NEED TO READ ANOTHER ELCISC RECORD.
      * I NEED TO READ ANOTHER ONE IF THE STATE IS LA.
      * I ALSO NEED TO READ ANOTHER ONE IF THE STATE IS ID, MD,
      *        ND OR PA
      *
      *****************************************************************
           IF SC-STATE = 'MS'
              IF CP-CLAIM-AMT < 5000
                 MOVE 'N'              TO CP-CLM-INT-SW
                 MOVE '4'              TO CI-RETURN-CODE
                 GO TO 0040-EXIT
              END-IF
           END-IF
           IF (SC-STATE = 'UT')
              AND (CP-EFF-DT >= X'A285')
              PERFORM 0070-UT-SPEC     THRU 0070-EXIT
              GO TO 0040-EXIT
           END-IF
           IF SC-STATE = 'WA'
              MOVE DC-ELAPSED-DAYS     TO WS-WA-DAYSA
              MOVE +0                  TO WS-WA-DAYSB
              MOVE CP-INC-DT           TO DC-BIN-DATE-1
              MOVE CP-LSTPD-DT         TO DC-BIN-DATE-2
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-ELAPSED-DAYS  TO WS-WA-DAYSB
              ELSE
                 MOVE 'N'              TO CP-CLM-INT-SW
                 MOVE '3'              TO CI-RETURN-CODE
                 GO TO 0040-EXIT
              END-IF
           END-IF
           IF SC-STATE = 'CO'
              MOVE ' '                 TO WS-CO-SW
              MOVE +0                  TO WS-CO-HLD-DAYS
              MOVE CP-PRF-DT           TO DC-BIN-DATE-1
              MOVE CP-LSTPD-DT         TO DC-BIN-DATE-2
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 IF DC-ELAPSED-DAYS > +30
                    COMPUTE WS-CO-HLD-DAYS = DC-ELAPSED-DAYS - +30
                 ELSE
                    MOVE +99999        TO WS-CO-HLD-DAYS
                 END-IF
              END-IF
           END-IF
           IF SC-STATE = 'LA' OR 'MN'
              MOVE CLAIM-INTEREST-SC   TO WS-HOLD-ELCISC
              PERFORM 0020-ELCISC-READNEXT
                                       THRU 0020-EXIT
              IF (RESP-NORMAL)
                 AND (WS-SC-SAVE-KEY (1:7) =
                           WS-SC-CONTROL-PRIMARY (1:7))
                 IF DC-ELAPSED-DAYS > SC-P-DAYS
                    CONTINUE
                 ELSE
                    MOVE WS-HOLD-ELCISC
                                       TO CLAIM-INTEREST-SC
                    MOVE SC-CONTROL-PRIMARY
                                       TO WS-SC-CONTROL-PRIMARY
                 END-IF
              ELSE
                 MOVE WS-HOLD-ELCISC   TO CLAIM-INTEREST-SC
                 MOVE SC-CONTROL-PRIMARY
                                       TO WS-SC-CONTROL-PRIMARY
              END-IF
           ELSE
              IF SC-STATE = 'ID' OR 'MD' OR 'ND' OR 'PA'
                 MOVE CP-INC-DT        TO DC-BIN-DATE-1
                 MOVE CP-PRF-DT        TO DC-BIN-DATE-2
                 MOVE '1'              TO DC-OPTION-CODE
                 PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
                 IF NO-CONVERSION-ERROR
                    IF DC-ELAPSED-DAYS < SC-EXCESS-DAYS
                       CONTINUE
                    ELSE
                       MOVE CLAIM-INTEREST-SC
                                       TO WS-HOLD-ELCISC
                       PERFORM 0020-ELCISC-READNEXT
                                       THRU 0020-EXIT
                       IF (RESP-NORMAL)
                          AND (WS-SC-SAVE-KEY (1:7) =
                                  WS-SC-CONTROL-PRIMARY (1:7))
                          CONTINUE
                       ELSE
                          MOVE WS-HOLD-ELCISC
                                       TO CLAIM-INTEREST-SC
                          MOVE SC-CONTROL-PRIMARY
                                       TO WS-SC-CONTROL-PRIMARY
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
      *****************************************************************
      *
      * DETERMINE THE PAY FROM DATE BY THE CALC START TYPE,
      * I = INCURRED DATE, R = REPORTED DATE.
      * NEXT, ADD THE CALC START DAYS ONTO THE PAY FROM  DATE.
      * AS OF 12/15/2005 THE ONLY VALID CALC END TYPE IS 'P' SO
      * USE THE PAID DATE AS THE PAY TO DATE.
      *
      *****************************************************************
           IF SC-CS-TYPE = 'I'
              MOVE CP-INC-DT           TO WS-PAY-FROM-DT
           ELSE
              IF SC-CS-TYPE = 'R'
                 MOVE CP-RPT-DT        TO WS-PAY-FROM-DT
              ELSE
                 MOVE CP-PRF-DT        TO WS-PAY-FROM-DT
              END-IF
           END-IF
           IF SC-CS-DAYS > ZEROS
              MOVE WS-PAY-FROM-DT      TO DC-BIN-DATE-1
              MOVE SC-CS-DAYS          TO DC-ELAPSED-DAYS
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO WS-PAY-FROM-DT
              ELSE
                 MOVE '5'              TO CI-RETURN-CODE
                 GO TO 0040-EXIT
              END-IF
           END-IF
           MOVE CP-LSTPD-DT            TO WS-PAY-TO-DT
      *****************************************************************
      *
      * BROWSE ALL THE STATE BREAKOUT RECORDS FOR THE STATE.
      * WHEN FINISHED BROWSING SET END OF BREAKOUT TO TRUE.
      *
      *****************************************************************
      ******************************************************************
           IF SC-STATE = 'CO'
              MOVE WS-PAY-FROM-DT      TO DC-BIN-DATE-1
              MOVE WS-PAY-TO-DT        TO DC-BIN-DATE-2
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-ELAPSED-DAYS  TO WS-CO-TOTAL-DAYS
              END-IF
              COMPUTE WS-CO-HLD-DAYS = WS-CO-TOTAL-DAYS - WS-CO-HLD-DAYS
           END-IF
      ******************************************************************
           MOVE WS-SC-CONTROL-PRIMARY (1:7)
                                       TO WS-SB-CONTROL-PRIMARY (1:7)
           MOVE SC-BREAKOUT-CODE       TO WS-SB-BREAKOUT-CODE
           MOVE ZEROS                  TO WS-SB-CALC-END
           MOVE WS-SB-CONTROL-PRIMARY  TO WS-SB-SAVE-KEY
           PERFORM 0100-ELCISB-STARTBR THRU 0100-EXIT
           IF NOT RESP-NORMAL
              MOVE 'N'                 TO CP-CLM-INT-SW
              MOVE '6'                 TO CI-RETURN-CODE
              GO TO 0040-EXIT
           END-IF
           PERFORM UNTIL END-OF-BREAKOUT
              PERFORM 0110-ELCISB-READNEXT
                                       THRU 0110-EXIT
              IF RESP-NORMAL
                 IF (SB-CONTROL-PRIMARY (1:9) = WS-SB-SAVE-KEY (1:9))
                    PERFORM 0050-PROCESS-BREAKOUT
                                       THRU 0050-EXIT
                 ELSE
                    SET END-OF-BREAKOUT TO TRUE
                 END-IF
              ELSE
                 SET END-OF-BREAKOUT   TO TRUE
              END-IF
           END-PERFORM
           .
       0040-EXIT.
           EXIT.
      *****************************************************************
      *
      * THIS IS A PAIN. HERE WE GO... IF THE CALC START IS 001 AND THE
      * CALC END IS 999 THEN IT IS A PIECE OF CAKE. OTHERWISE WE HAVE
      * TO SPLIT THE PAYMENTS APART BY THE NUMBER OF DAYS IN THE CALC
      * END, THEN READ THE NEXT RECORD AND IF THE END IS 999 THEN THAT
      * IS THE LAST ONE.
      * NEXT, ADD THE CALC START DAYS ONTO THE CALC START DATE.
      * AS OF 12/15/2005 THE ONLY VALID CALC END TYPE IS 'P' SO
      * USE THE PAID DATE AS THE CALC END DATE.
      *
      *****************************************************************
       0050-PROCESS-BREAKOUT.
           IF SB-STATE = 'CO'
              IF CO-CHECKED
                 COMPUTE SB-CALC-START = WS-CO-HLD-DAYS + +1
              ELSE
                 MOVE WS-CO-HLD-DAYS   TO SB-CALC-END
                 SET CO-CHECKED        TO TRUE
              END-IF
           END-IF
           IF SB-CALC-START = 001
              MOVE WS-PAY-FROM-DT      TO WS-SUB-FROM-DT
              IF (SC-STATE = 'WA')
                 AND (WS-WA-DAYSA < SB-CALC-END)
                 SET END-OF-BREAKOUT   TO TRUE
              END-IF
           ELSE
              IF SC-STATE = 'WA'
                 MOVE CP-PRF-DT        TO WS-PAY-FROM-DT
              END-IF
              MOVE WS-PAY-FROM-DT      TO DC-BIN-DATE-1
              IF SB-CALC-START > 1
                 COMPUTE DC-ELAPSED-DAYS = SB-CALC-START - 1
              ELSE
                 MOVE 1                TO DC-ELAPSED-DAYS
              END-IF
              MOVE +0                  TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO WS-SUB-FROM-DT
              ELSE
                 MOVE '5'              TO CI-RETURN-CODE
                 GO TO 0050-EXIT
              END-IF
           END-IF
           IF SB-CALC-END = 999
              SET END-OF-BREAKOUT TO TRUE
              MOVE WS-PAY-TO-DT        TO WS-SUB-TO-DT
           ELSE
              IF (SC-STATE = 'WA')
                 AND (END-OF-BREAKOUT)
                 MOVE WS-WA-DAYSB      TO SB-CALC-END
              ELSE
                 IF SC-STATE = 'WA'
                    COMPUTE SB-CALC-END = WS-WA-DAYSB -
                       (WS-WA-DAYSA - 90)
                 END-IF
              END-IF
              MOVE WS-PAY-FROM-DT      TO DC-BIN-DATE-1
              MOVE SB-CALC-END         TO DC-ELAPSED-DAYS
              MOVE +0                  TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO WS-SUB-TO-DT
                 IF WS-PAY-TO-DT < WS-SUB-TO-DT
                    MOVE WS-PAY-TO-DT  TO WS-SUB-TO-DT
PEMTST              SET END-OF-BREAKOUT TO TRUE
                 END-IF
              ELSE
                 MOVE '5'              TO CI-RETURN-CODE
                 GO TO 0050-EXIT
              END-IF
           END-IF
           MOVE CP-COMPANY-CD          TO WS-ST-COMPANY-CD
           MOVE SB-SCHED-CODE          TO WS-ST-SCHED-CODE
           MOVE LOW-VALUES             TO WS-ST-END-DT
           PERFORM 0200-ELCIST-STARTBR THRU 0200-EXIT
           IF NOT RESP-NORMAL
              MOVE 'N'                 TO CP-CLM-INT-SW
              MOVE '7'                 TO CI-RETURN-CODE
              GO TO 0050-EXIT
           END-IF
           MOVE WS-ST-CONTROL-PRIMARY  TO WS-ST-SAVE-KEY
           MOVE SPACES                 TO WS-ELCIST-SW
           PERFORM UNTIL END-OF-SCHED-TABLE
              PERFORM 0210-ELCIST-READNEXT
                                       THRU 0210-EXIT
              IF RESP-NORMAL
                 IF ST-CONTROL-PRIMARY (1:4) = WS-ST-SAVE-KEY (1:4)
                    PERFORM 0060-ACCUMULATE THRU 0060-EXIT
                 ELSE
                    SET END-OF-SCHED-TABLE TO TRUE
                 END-IF
              ELSE
                 SET END-OF-SCHED-TABLE TO TRUE
              END-IF
           END-PERFORM
           
      * EXEC CICS ENDBR
      *       DATASET ('ELCIST')
      *    END-EXEC
           MOVE 'ELCIST' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001131' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0050-EXIT.
           EXIT.
       0060-ACCUMULATE.
           IF WS-SUB-TO-DT < ST-START-DATE
              SET END-OF-SCHED-TABLE   TO TRUE
              GO TO 0060-EXIT
           ELSE
              IF WS-SUB-FROM-DT > ST-END-DATE
                 GO TO 0060-EXIT
              END-IF
           END-IF
           IF (WS-SUB-FROM-DT >= ST-START-DATE)
              AND (WS-SUB-FROM-DT <= ST-END-DATE)
              MOVE WS-SUB-FROM-DT      TO WS-CALC-START-DT
              IF (WS-SUB-TO-DT >= ST-START-DATE)
                 AND (WS-SUB-TO-DT <= ST-END-DATE)
                 MOVE WS-SUB-TO-DT     TO WS-CALC-END-DT
                 SET END-OF-SCHED-TABLE TO TRUE
              ELSE
                 MOVE ST-END-DATE        TO WS-CALC-END-DT
              END-IF
           ELSE
              MOVE ST-START-DATE        TO WS-CALC-START-DT
              IF (WS-SUB-TO-DT >= ST-START-DATE)
                 AND (WS-SUB-TO-DT <= ST-END-DATE)
                 MOVE WS-SUB-TO-DT     TO WS-CALC-END-DT
                 SET END-OF-SCHED-TABLE TO TRUE
              ELSE
                 MOVE ST-END-DATE        TO WS-CALC-END-DT
              END-IF
           END-IF
           IF WS-CALC-START-DT >= WS-CALC-END-DT
              GO TO 0060-EXIT
           END-IF
           MOVE WS-CALC-START-DT       TO DC-BIN-DATE-1
           MOVE WS-CALC-END-DT         TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9100-CONVERT-DATE   THRU 9100-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-DAYS     TO WS-DAYS
           ELSE
              MOVE 'N'                 TO CP-CLM-INT-SW
              MOVE '5'                 TO CI-RETURN-CODE
              GO TO 0060-EXIT
           END-IF
           MOVE CP-COMPANY-CD          TO WS-IR-COMPANY-CD
           MOVE ST-INT-RATE-CODE       TO WS-IR-INT-RATE-CODE
           PERFORM 0300-ELCIIR-READ    THRU 0300-EXIT
           IF RESP-NORMAL
              IF IR-INT-RATE-CODE = 'ZZZ'
                 IF CP-INT-RATE = ZEROS
                    MOVE 'N'           TO CP-CLM-INT-SW
                    MOVE '8'           TO CI-RETURN-CODE
                    GO TO 0060-EXIT
                 ELSE
                    MOVE CP-INT-RATE   TO IR-INT-RATE
                 END-IF
              END-IF
              COMPUTE WS-TOTAL-DAYS = WS-TOTAL-DAYS + WS-DAYS
              COMPUTE WS-INTEREST ROUNDED = (CP-CLAIM-AMT *
                 ((1 + IR-INT-RATE) ** (WS-DAYS / 365) - 1))
              ADD WS-INTEREST          TO WS-TOTAL-INTEREST
           END-IF
           .
       0060-EXIT.
           EXIT.
       0070-UT-SPEC.
      *    X'A285' = 05/05/2008   USED FOR UT ONLY
      *    FOR UT ONLY THE CP-RPT-DT IS REALLY THE EFF DATE
           MOVE CP-INC-DT              TO DC-BIN-DATE-1
           MOVE CP-LSTPD-DT            TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9100-CONVERT-DATE   THRU 9100-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-DAYS     TO WS-WA-DAYSA
           ELSE
              MOVE 'N'                 TO CP-CLM-INT-SW
              MOVE '5'                 TO CI-RETURN-CODE
              GO TO 0070-EXIT
           END-IF
           MOVE CP-PRF-DT              TO DC-BIN-DATE-1
           MOVE CP-LSTPD-DT            TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9100-CONVERT-DATE   THRU 9100-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-DAYS     TO WS-WA-DAYSB
           ELSE
              MOVE 'N'                 TO CP-CLM-INT-SW
              MOVE '5'                 TO CI-RETURN-CODE
              GO TO 0070-EXIT
           END-IF
082814     
      * EXEC CICS GETMAIN
082814*        LENGTH   (100)
082814*        SET      (ADDRESS OF claim-interest-ir)
082814*        INITIMG  (getmain-space)
082814*    END-EXEC
           MOVE 100
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00001226' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 getmain-space
           SET ADDRESS OF claim-interest-ir TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
082814
082814     move +0                     to ir-int-rate
           IF WS-WA-DAYSB < 31
              COMPUTE WS-TOTAL-INTEREST ROUNDED = (CP-CLAIM-AMT *
              ((1.035) ** (WS-WA-DAYSA / 365) - 1))
              MOVE .035                TO IR-INT-RATE
              MOVE WS-WA-DAYSA         TO WS-TOTAL-DAYS
           ELSE
              MOVE CP-INC-DT           TO DC-BIN-DATE-1
              MOVE CP-PRF-DT           TO DC-BIN-DATE-2
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 9100-CONVERT-DATE
                                       THRU 9100-EXIT
              IF NO-CONVERSION-ERROR
                 COMPUTE WS-DAYS = DC-ELAPSED-DAYS + 30
              ELSE
                 MOVE 'N'              TO CP-CLM-INT-SW
                 MOVE '5'              TO CI-RETURN-CODE
                 GO TO 0070-EXIT
              END-IF
              COMPUTE WS-INTEREST ROUNDED = (CP-CLAIM-AMT *
                 ((1.035) ** (WS-DAYS / 365) - 1))
              MOVE .035                TO IR-INT-RATE
              MOVE WS-DAYS             TO WS-TOTAL-DAYS
              SUBTRACT +30             FROM WS-WA-DAYSB
              IF WS-WA-DAYSB > +0
                 COMPUTE WS-TOTAL-INTEREST ROUNDED = (CP-CLAIM-AMT *
                    ((1.135) ** (WS-WA-DAYSB / 365) - 1))
                 COMPUTE WS-TOTAL-INTEREST = WS-TOTAL-INTEREST +
                    WS-INTEREST
                 MOVE .135                TO IR-INT-RATE
                 COMPUTE WS-TOTAL-DAYS = WS-TOTAL-DAYS + WS-WA-DAYSB
              END-IF
           END-IF
           .
       0070-EXIT.
           EXIT.
       0100-ELCISB-STARTBR.
           
      * EXEC CICS STARTBR
      *        DATASET   ('ELCISB')
      *        RIDFLD    (WS-SB-CONTROL-PRIMARY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCISB' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001269' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-SB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0100-EXIT.
           EXIT.
       0110-ELCISB-READNEXT.
           
      * EXEC CICS READNEXT
      *        DATASET  ('ELCISB')
      *        SET      (ADDRESS OF CLAIM-INTEREST-SB)
      *        RIDFLD   (WS-SB-CONTROL-PRIMARY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCISB' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00001279' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-SB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-INTEREST-SB TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0110-EXIT.
           EXIT.
       0200-ELCIST-STARTBR.
           
      * EXEC CICS STARTBR
      *        DATASET   ('ELCIST')
      *        RIDFLD    (WS-ST-CONTROL-PRIMARY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCIST' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001289' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ST-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0200-EXIT.
           EXIT.
       0210-ELCIST-READNEXT.
           
      * EXEC CICS READNEXT
      *        DATASET  ('ELCIST')
      *        SET      (ADDRESS OF CLAIM-INTEREST-ST)
      *        RIDFLD   (WS-ST-CONTROL-PRIMARY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCIST' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00001299' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ST-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-INTEREST-ST TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0210-EXIT.
           EXIT.
       0300-ELCIIR-READ.
           
      * EXEC CICS READ
      *        DATASET  ('ELCIIR')
      *        SET      (ADDRESS OF CLAIM-INTEREST-IR)
      *        RIDFLD   (WS-IR-CONTROL-PRIMARY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELCIIR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00001309' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031333039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-IR-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-INTEREST-IR TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0300-EXIT.
           EXIT.
       9100-CONVERT-DATE.
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELDATCV')
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001319' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9100-EXIT.
           EXIT.
       99999-DUMMY-STOP-RUN.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELCLMI' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELCLMI' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
