00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ELPMNT
00003  PROGRAM-ID.                 ELPMNT.                                 LV006
00004 *                            VMOD=2.006                           ELPMNT
00005                                                                   ELPMNT
00006 *AUTHOR.     LOGIC,INC.                                           ELPMNT
00007 *            DALLAS, TEXAS.                                       ELPMNT
00008 *                                                                 ELPMNT
00009 *DATE-COMPILED.                                                   ELPMNT
00010                                                                   ELPMNT
00011 *SECURITY.   *****************************************************ELPMNT
00012 *            *                                                   *ELPMNT
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELPMNT
00014 *            *                                                   *ELPMNT
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELPMNT
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELPMNT
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ELPMNT
00018 *            *                                                   *ELPMNT
00019 *            *****************************************************ELPMNT
00020                                                                   ELPMNT
00021 *REMARKS.    THIS SUBROUTINE CALCULATES A CLAIM PAYMENT           ELPMNT
00022 *            FROM THE INFORMATION PASSED VIA THE ELCCALC          ELPMNT
00023 *            COPYBOOK.                                            ELPMNT
00024                                                                   ELPMNT
00025      EJECT                                                        ELPMNT
00026  ENVIRONMENT DIVISION.                                            ELPMNT
00027                                                                   ELPMNT
00028  DATA DIVISION.                                                   ELPMNT
00029                                                                   ELPMNT
00030  WORKING-STORAGE SECTION.                                         ELPMNT
00031                                                                   ELPMNT
00032  77  FILLER  PIC X(32)  VALUE '********************************'. ELPMNT
00033  77  FILLER  PIC X(32)  VALUE '*   ELPMNT WORKING STORAGE     *'. ELPMNT
00034  77  FILLER  PIC X(32)  VALUE '******* VMOD=2.006 *************'. ELPMNT
00035                                                                   ELPMNT
00036                                     COPY ELCDATE.                    CL**6
00037                                                                   ELPMNT
00038  EJECT                                                               CL**4
00039                                     COPY ELCCALC.                    CL**4
00040                                                                   ELPMNT
00041  EJECT                                                               CL**4
00042  01  MISC-WORK-AREAS.                                             ELPMNT
00043      12  WS-DAYS-TOL         PIC S9(5)     VALUE +0      COMP-3.  ELPMNT
00044      12  WS-PMT-TOL          PIC S9(7)V99  VALUE +0      COMP-3.  ELPMNT
00045      12  WS-MAX-DAYS-TOL     PIC S9(5)     VALUE +0      COMP-3.  ELPMNT
00046      12  WS-MAX-PMT-TOL      PIC S9(7)V99  VALUE +0      COMP-3.  ELPMNT
00047      12  WS-MAX-LF-PMT-TOL   PIC S9(7)V99  VALUE +0      COMP-3.  ELPMNT
00048      12  WS-AH-BEN-AMT       PIC S9(7)V999 VALUE +0      COMP-3.  ELPMNT
00049      12  WS-COMPUTE-AMT      PIC S9(9)V999 VALUE +0      COMP-3.  ELPMNT
00050      12  WS-TOL-SEVERITY     PIC X         VALUE ' '.             ELPMNT
00051      12  WS-EARNING-METHOD   PIC X         VALUE ' '.             ELPMNT
00052      12  WS-CALC-METHOD      PIC X         VALUE ' '.             ELPMNT
00053      12  WS-PAY-TYPE         PIC X         VALUE ' '.             ELPMNT
00054      12  WORK-DATE-FROM      PIC 9(8)      VALUE ZERO.               CL**4
00055      12  WORK-DATE-FROM-R  REDEFINES  WORK-DATE-FROM.                CL**4
00056          16  WDF-CCYY        PIC 9(4).                               CL**4
00057          16  WDF-CCYR  REDEFINES  WDF-CCYY.                          CL**4
00058              20  WDF-CC      PIC 99.                                 CL**4
00059              20  WDF-YR      PIC 99.                                 CL**4
00060          16  WDF-MONTH       PIC 99.                                 CL**4
00061          16  WDF-DAY         PIC 99.                                 CL**4
00062      12  WORK-DATE-THRU      PIC 9(8)      VALUE ZERO.               CL**4
00063      12  WORK-DATE-THRU-R  REDEFINES  WORK-DATE-THRU.                CL**4
00064          16  WDT-CCYY        PIC 9(4).                               CL**4
00065          16  WDT-CCYR  REDEFINES  WDT-CCYY.                          CL**5
00066              20  WDT-CC      PIC 99.                                 CL**4
00067              20  WDT-YR      PIC 99.                                 CL**4
00068          16  WDT-MONTH       PIC 99.                                 CL**4
00069          16  WDT-DAY         PIC 99.                                 CL**4
00070      12  FROM-DAYS-IN-MONTH  PIC 99        VALUE ZERO.            ELPMNT
00071      12  THRU-DAYS-IN-MONTH  PIC 99        VALUE ZERO.            ELPMNT
00072      12  FROM-PMT-DAYS       PIC 99        VALUE ZERO.            ELPMNT
00073      12  THRU-PMT-DAYS       PIC 99        VALUE ZERO.            ELPMNT
00074      12  SAVE-ELAPSED-MONTHS PIC 999       VALUE ZERO.            ELPMNT
00075      12  SAVE-ELAPSED-DAYS   PIC 9(04)     VALUE ZERO.            ELPMNT
00076      12  WS-SAVE-ODD-DAYS    PIC S9(04)    VALUE ZERO.            ELPMNT
00077      12  SAVE-ODD-DAYS-OVER  PIC 9(03)     VALUE ZERO.            ELPMNT
00078      12  WS-DAILY-RATE       PIC S9(3)V99.                        ELPMNT
00079      12  WST-DAILY-RATE      PIC S9(3)V99999.                     ELPMNT
00080      12  WS-PY-WORK          PIC S9(8)V99.                        ELPMNT
00081      12  WS-PY-WK REDEFINES WS-PY-WORK.                           ELPMNT
00082          16  WS-PY-NUM       PIC S9(7)V99.                        ELPMNT
00083          16  WS-PY-SIGN      PIC X.                               ELPMNT
00084      12  WS-DY-WORK          PIC S9(5).                           ELPMNT
00085      12  WS-DY-WK REDEFINES WS-DY-WORK.                           ELPMNT
00086          16  WS-DY-NUM       PIC S9(4).                           ELPMNT
00087          16  WS-DY-SIGN      PIC X.                               ELPMNT
00088      12  WS-ACCESS.                                               ELPMNT
00089          16  FILLER          PIC XX      VALUE SPACES.            ELPMNT
00090          16  WS-BEN-CD       PIC 99.                              ELPMNT
00091      12  DATE-WORK.                                               ELPMNT
00092          16  FILLER          PIC X(2).                            ELPMNT
00093          16  NUM-WORK        PIC X(6).                            ELPMNT
00094      12  WS-SAVE-INPUT.                                           ELPMNT
00095          16  WS-PMTTYPE      PIC X       VALUE SPACE.             ELPMNT
00096          16  WS-PAYEE        PIC X(02)   VALUE SPACE.             ELPMNT
00097          16  WS-PMTNOTE1     PIC X(40)   VALUE SPACES.            ELPMNT
00098          16  WS-PMTNOTE2     PIC X(40)   VALUE SPACES.            ELPMNT
00099          16  WS-OFFLINE      PIC X       VALUE SPACE.             ELPMNT
00100          16  WS-CHECKNO      PIC X(7)    VALUE SPACES.            ELPMNT
00101          16  WS-HOLDTIL      PIC 9(6)    VALUE 0.                 ELPMNT
00102          16  WS-EDAYS        PIC S9(5)    VALUE +0.               ELPMNT
00103          16  WS-EPYAMT       PIC S9(7)V99 VALUE +0.               ELPMNT
00104          16  WS-ERESV        PIC S9(7)V99 VALUE 0.                ELPMNT
00105          16  WS-EEXPENS      PIC 9(6)V99  VALUE 0.                ELPMNT
00106          16  WS-ETYPE        PIC X       VALUE SPACE.             ELPMNT
00107          16  WS-CASH         PIC X       VALUE SPACE.             ELPMNT
00108          16  WS-GROUPED      PIC X       VALUE SPACE.             ELPMNT
00109      12  LAST-ERROR-LINE.                                         ELPMNT
00110          16  WS-FATAL-CTR        PIC ZZ9.                         ELPMNT
00111          16  FILLER              PIC X(30)                        ELPMNT
00112              VALUE ' FATAL ERRORS ENCOUNTERED'.                   ELPMNT
00113          16  WS-FORCE-CTR        PIC ZZ9.                         ELPMNT
00114          16  FILLER              PIC X(28)                        ELPMNT
00115              VALUE ' FORCABLE ERRORS ENCOUNTERED'.                ELPMNT
00116      12  WS-CHECK-AREA.                                           ELPMNT
00117          16  WS-CHECK-CARR       PIC X.                           ELPMNT
00118          16  WS-CHECK-NUM        PIC 9(6).                        ELPMNT
00119      12  WS-CHECK-NUMERIC REDEFINES WS-CHECK-AREA PIC 9(7).       ELPMNT
00120      12  OPTION-1-SAVE-AREA.                                      ELPMNT
00121          16  SAVE1-FROM-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.    ELPMNT
00122          16  SAVE1-THRU-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.    ELPMNT
00123                                                                   ELPMNT
00124      EJECT                                                        ELPMNT
00125  LINKAGE SECTION.                                                 ELPMNT
00126  01  DFHCOMMAREA             PIC X(450).                          ELPMNT
00127                                                                   ELPMNT
00128      EJECT                                                        ELPMNT
00129  PROCEDURE DIVISION.                                              ELPMNT
00130                                                                   ELPMNT
00131      MOVE DFHCOMMAREA       TO CALCULATION-PASS-AREA.             ELPMNT
00132                                                                   ELPMNT
00133      MOVE ZERO              TO CP-RETURN-CODE.                       CL**2
00134                                                                   ELPMNT
00135 *    SAMPLE DATE RANGES AND DAYS PAID FOR OPTION 1 AND 6          ELPMNT
00136                                                                   ELPMNT
00137 ****************************************************************  ELPMNT
00138 ****************************************************************  ELPMNT
00139 *                  CALCULATED FROM      PAYMENT      PAYMENT      ELPMNT
00140 *   DATES           DATE ROUTINE        OPT  1       OPT  6       ELPMNT
00141 * FROM    THRU    MONTHS ODD ELAPSED   DAYS-PAID    DAYS-PAID     ELPMNT
00142 *1-01-83  1-15-83    0    15     15        15           15        ELPMNT
00143 *1-01-83  1-30-83    0    30     30        30           30        ELPMNT
00144 *1-01-83  1-31-83    1     0     31        30           30        ELPMNT
00145 *1-01-83  2-01-83    1     1     32        31           31        ELPMNT
00146 *1-01-83  2-15-83    1    15     46        45           45        ELPMNT
00147 *1-01-83  2-28-83    2     0     59        60           60        ELPMNT
00148 *1-01-83  3-01-83    2     1     60        61           61        ELPMNT
00149 *1-01-83  3-31-83    3     0     90        90           90        ELPMNT
00150 *1-15-83  1-31-83    0    17     17        17           16        ELPMNT
00151 *1-15-83  2-01-83    0    18     18        18           17        ELPMNT
00152 *1-15-83  2-28-83    1    14     45        44           46        ELPMNT
00153 *1-21-83  3-09-83    1    20     48        50           49        ELPMNT
00154 *2-05-83  3-06-83    1     2     30        32           32        ELPMNT
00155 *2-05-83  3-23-83    1    19     47        49           49        ELPMNT
00156 *2-15-83  2-28-83    0    14     14        14           16        ELPMNT
00157 *2-15-83  3-01-83    0    15     15        15           17        ELPMNT
00158 *2-21-83  3-19-83    0    27     27        27           29        ELPMNT
00159 *2-21-83  3-20-83    1     0     28        30           30        ELPMNT
00160 *2-21-83  4-10-83    1    18     49        30           50        ELPMNT
00161 *2-21-83  5-19-83    2    27     88        87           89        ELPMNT
00162 *2-21-83  5-22-83    3     2     91        92           92        ELPMNT
00163 *3-21-83  5-19-83    1    30     60        60           59        ELPMNT
00164 ****************************************************************  ELPMNT
00165      EJECT                                                        ELPMNT
00166  0000-CALC-PAYMENT.                                               ELPMNT
00167                                                                   ELPMNT
00168      MOVE CP-PAID-FROM-DATE      TO DC-BIN-DATE-1                 ELPMNT
00169      MOVE ' '                    TO DC-OPTION-CODE                ELPMNT
00170      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                ELPMNT
00171      IF NO-CONVERSION-ERROR                                       ELPMNT
00172         MOVE DC-DAYS-IN-MONTH    TO FROM-DAYS-IN-MONTH            ELPMNT
00173         MOVE DC-GREG-DATE-CYMD   TO WORK-DATE-FROM                   CL**4
00174      ELSE                                                         ELPMNT
00175         MOVE '2'                 TO CP-RETURN-CODE                ELPMNT
00176         GO TO 0100-RETURN.                                        ELPMNT
00177                                                                   ELPMNT
00178      MOVE CP-PAID-THRU-DT        TO DC-BIN-DATE-1                 ELPMNT
00179      MOVE ' '                    TO DC-OPTION-CODE                ELPMNT
00180      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                ELPMNT
00181      IF NO-CONVERSION-ERROR                                       ELPMNT
00182         MOVE DC-GREG-DATE-CYMD      TO WORK-DATE-THRU                CL**4
00183         MOVE DC-DAYS-IN-MONTH       TO THRU-DAYS-IN-MONTH         ELPMNT
00184      ELSE                                                         ELPMNT
00185         MOVE '2'                 TO CP-RETURN-CODE                ELPMNT
00186         GO TO 0100-RETURN.                                        ELPMNT
00187                                                                   ELPMNT
00188      MOVE CP-PAID-FROM-DATE      TO DC-BIN-DATE-1                 ELPMNT
00189      MOVE -1                     TO DC-ELAPSED-DAYS               ELPMNT
00190      MOVE ZEROS                  TO DC-ELAPSED-MONTHS             ELPMNT
00191      MOVE '6'                    TO DC-OPTION-CODE                ELPMNT
00192      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                ELPMNT
00193      MOVE DC-DAYS-IN-MONTH       TO SAVE1-FROM-DAYS-IN-MONTH      ELPMNT
00194                                                                   ELPMNT
00195      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1                 ELPMNT
00196      MOVE CP-PAID-THRU-DT        TO DC-BIN-DATE-2                 ELPMNT
00197      MOVE '1'                    TO DC-OPTION-CODE                ELPMNT
00198      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                ELPMNT
00199                                                                   ELPMNT
00200      MOVE DC-ELAPSED-MONTHS      TO SAVE-ELAPSED-MONTHS           ELPMNT
00201      MOVE DC-ODD-DAYS-OVER       TO SAVE-ODD-DAYS-OVER            ELPMNT
00202      MOVE DC-ELAPSED-DAYS        TO SAVE-ELAPSED-DAYS             ELPMNT
00203                                     CP-ACTUAL-DAYS                ELPMNT
00204                                                                   ELPMNT
00205 ******************************************************************ELPMNT
00206 *   THE FOLLOWING STATEMENT ADJUSTS THE ODD DAYS OVER BY THE     *ELPMNT
00207 *     DIFFERENCE BETWEEN DAYS IN MONTH (FROM MONTH) AND          *ELPMNT
00208 *     DAYS IN MONTH (ONE MONTH PRIOR TO THRU MONTH)              *ELPMNT
00209 ******************************************************************ELPMNT
00210                                                                   ELPMNT
00211      IF CP-CLAIM-CALC-METHOD EQUAL '1'  AND                       ELPMNT
00212         DC-ELAPSED-MONTHS GREATER ZERO AND                        ELPMNT
00213         DC-ODD-DAYS-OVER NOT = ZEROS   AND                        ELPMNT
00214         WDF-DAY GREATER THAN WDT-DAY                              ELPMNT
00215         MOVE CP-PAID-THRU-DT     TO DC-BIN-DATE-1                 ELPMNT
00216         MOVE ZEROS               TO DC-ELAPSED-DAYS               ELPMNT
00217         MOVE -1                  TO DC-ELAPSED-MONTHS             ELPMNT
00218         MOVE '6'                 TO DC-OPTION-CODE                ELPMNT
00219         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             ELPMNT
00220         MOVE DC-DAYS-IN-MONTH    TO SAVE1-THRU-DAYS-IN-MONTH      ELPMNT
00221         COMPUTE WS-SAVE-ODD-DAYS =                                ELPMNT
00222          (SAVE1-THRU-DAYS-IN-MONTH - SAVE1-FROM-DAYS-IN-MONTH)    ELPMNT
00223         ADD WS-SAVE-ODD-DAYS     TO SAVE-ODD-DAYS-OVER.           ELPMNT
00224                                                                   ELPMNT
00225      MOVE SAVE-ELAPSED-MONTHS    TO DC-ELAPSED-MONTHS.            ELPMNT
00226      MOVE SAVE-ODD-DAYS-OVER     TO DC-ODD-DAYS-OVER              ELPMNT
00227      MOVE SAVE-ELAPSED-DAYS      TO DC-ELAPSED-DAYS.              ELPMNT
00228                                                                   ELPMNT
00229      IF (CP-COMPANY-ID EQUAL 'FIA' AND                            ELPMNT
00230         CP-ACCOUNT-NUMBER EQUAL '0000011043')                     ELPMNT
00231         COMPUTE WS-DAILY-RATE ROUNDED =                           ELPMNT
00232                     (CP-ORIGINAL-BENEFIT * 13) / 365              ELPMNT
00233          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                   ELPMNT
00234                            WS-DAILY-RATE * DC-ELAPSED-DAYS        ELPMNT
00235          GO TO 0100-RETURN.                                       ELPMNT
00236                                                                   ELPMNT
00237      IF CP-CLAIM-CALC-METHOD EQUAL '1'                            ELPMNT
00238          COMPUTE WST-DAILY-RATE ROUNDED EQUAL                     ELPMNT
00239                   CP-ORIGINAL-BENEFIT / 30                        ELPMNT
00240          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                   ELPMNT
00241              (CP-ORIGINAL-BENEFIT * DC-ELAPSED-MONTHS) +          ELPMNT
00242              (WST-DAILY-RATE     * DC-ODD-DAYS-OVER)              ELPMNT
00243               MOVE WST-DAILY-RATE TO WS-DAILY-RATE                ELPMNT
00244          GO TO 0100-RETURN.                                       ELPMNT
00245                                                                   ELPMNT
00246      IF CP-CLAIM-CALC-METHOD EQUAL '2'                            ELPMNT
00247          COMPUTE WS-DAILY-RATE ROUNDED =                          ELPMNT
00248                    (CP-ORIGINAL-BENEFIT * 12) / 365               ELPMNT
00249          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                   ELPMNT
00250            (CP-ORIGINAL-BENEFIT * DC-ELAPSED-MONTHS) +            ELPMNT
00251              (WS-DAILY-RATE     * DC-ODD-DAYS-OVER)               ELPMNT
00252          GO TO 0100-RETURN.                                       ELPMNT
00253                                                                   ELPMNT
00254      IF CP-CLAIM-CALC-METHOD EQUAL '4'                            ELPMNT
00255          MOVE CP-ORIGINAL-BENEFIT     TO  WS-AH-BEN-AMT           ELPMNT
00256          COMPUTE WST-DAILY-RATE ROUNDED = (WS-AH-BEN-AMT / 30)    ELPMNT
00257          MOVE WST-DAILY-RATE TO WS-DAILY-RATE                     ELPMNT
00258          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                   ELPMNT
00259                            WST-DAILY-RATE * DC-ELAPSED-DAYS       ELPMNT
00260          GO TO 0100-RETURN.                                       ELPMNT
00261                                                                   ELPMNT
00262      IF CP-CLAIM-CALC-METHOD EQUAL '5'                            ELPMNT
00263          COMPUTE WS-DAILY-RATE ROUNDED =                          ELPMNT
00264                    (CP-ORIGINAL-BENEFIT * 12) / 365               ELPMNT
00265          COMPUTE CP-CLAIM-PAYMENT ROUNDED =                       ELPMNT
00266                            WS-DAILY-RATE * DC-ELAPSED-DAYS        ELPMNT
00267          GO TO 0100-RETURN.                                       ELPMNT
00268                                                                   ELPMNT
00269      IF CP-CLAIM-CALC-METHOD NOT EQUAL '3'                        ELPMNT
00270         GO TO 0010-CHECK-PAYMENT.                                 ELPMNT
00271                                                                   ELPMNT
00272 ******   CALCULATION METHOD  3  ROUTINE                           ELPMNT
00273      COMPUTE WS-DAILY-RATE =                                      ELPMNT
00274              CP-ORIGINAL-BENEFIT / FROM-DAYS-IN-MONTH.            ELPMNT
00275                                                                   ELPMNT
00276 ******CHECK TO SEE IF WITHIN SAME MONTH                           ELPMNT
00277      IF (WDF-MONTH = WDT-MONTH) AND (WDF-CCYY = WDT-CCYY)            CL**4
00278         COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                    ELPMNT
00279               SAVE-ELAPSED-DAYS *                                 ELPMNT
00280               (CP-ORIGINAL-BENEFIT / FROM-DAYS-IN-MONTH)          ELPMNT
00281          GO TO 0100-RETURN.                                       ELPMNT
00282                                                                   ELPMNT
00283 ******CHECK TO SEE IF EVEN MONTHS AND NO ODD DAYS                 ELPMNT
00284      IF SAVE-ELAPSED-MONTHS NOT = ZEROS   AND                     ELPMNT
00285              SAVE-ODD-DAYS-OVER = ZEROS                           ELPMNT
00286       COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                      ELPMNT
00287             CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS             ELPMNT
00288       GO TO 0100-RETURN.                                          ELPMNT
00289                                                                   ELPMNT
00290 ******BUMP THE FROM DATE WITH THE NUMBER OF MONTHS                ELPMNT
00291 ******AND FIND THE NUMBER OF DAYS LEFT IN THIS MONTH.             ELPMNT
00292      IF SAVE-ELAPSED-MONTHS NOT = ZERO                            ELPMNT
00293         ADD SAVE-ELAPSED-MONTHS  TO WDF-MONTH                     ELPMNT
00294         IF WDF-MONTH GREATER THAN 12                              ELPMNT
00295            ADD 1                 TO WDF-CCYY                         CL**4
00296            SUBTRACT 12         FROM WDF-MONTH.                       CL**4
00297 ******CONVERT THE NEW FROM MONTH TO BINARY                        ELPMNT
00298      IF SAVE-ELAPSED-MONTHS NOT = ZERO                            ELPMNT
00299         MOVE WORK-DATE-FROM TO DC-GREG-DATE-CYMD                     CL**4
00300         MOVE 'L' TO DC-OPTION-CODE                                   CL**4
00301         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             ELPMNT
00302         MOVE DC-DAYS-IN-MONTH    TO FROM-DAYS-IN-MONTH.           ELPMNT
00303                                                                   ELPMNT
00304      IF (WDF-MONTH = WDT-MONTH) AND (WDF-CCYY = WDT-CCYY)            CL**4
00305        COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                     ELPMNT
00306                       ((WDT-DAY  -  WDF-DAY)  *                   ELPMNT
00307              (CP-ORIGINAL-BENEFIT / FROM-DAYS-IN-MONTH)) +        ELPMNT
00308                 (CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS)       ELPMNT
00309         ELSE                                                      ELPMNT
00310        COMPUTE FROM-PMT-DAYS = FROM-DAYS-IN-MONTH - WDF-DAY       ELPMNT
00311        MOVE WDT-DAY              TO THRU-PMT-DAYS                 ELPMNT
00312        COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                     ELPMNT
00313        (FROM-PMT-DAYS * (CP-ORIGINAL-BENEFIT /                    ELPMNT
00314                          FROM-DAYS-IN-MONTH))                     ELPMNT
00315        +                                                          ELPMNT
00316        (THRU-PMT-DAYS * (CP-ORIGINAL-BENEFIT /                    ELPMNT
00317                          THRU-DAYS-IN-MONTH))                     ELPMNT
00318        +                                                          ELPMNT
00319        (CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS).               ELPMNT
00320      GO TO 0100-RETURN.                                           ELPMNT
00321      EJECT                                                        ELPMNT
00322  0010-CHECK-PAYMENT.                                              ELPMNT
00323 ******PAYMENT OPTION 6                                            ELPMNT
00324 ******CHECK TO SEE IF LESS THAN 1 MONTH AND WITHIN SAME MONTH     ELPMNT
00325      IF SAVE-ELAPSED-MONTHS GREATER THAN ZERO                     ELPMNT
00326         IF SAVE-ODD-DAYS-OVER = ZEROS                             ELPMNT
00327            GO TO 0020-CHECK-TO-DAY                                ELPMNT
00328         ELSE                                                      ELPMNT
00329            IF WDF-DAY GREATER THAN WDT-DAY                        ELPMNT
00330               IF WDF-MONTH = 02                                   ELPMNT
00331                   PERFORM 0050-CHECK-LEAP-YEAR THRU 0050-EXIT     ELPMNT
00332                   ADD 2          TO  SAVE-ODD-DAYS-OVER           ELPMNT
00333                   GO TO 0020-CHECK-TO-DAY                         ELPMNT
00334               ELSE                                                ELPMNT
00335                   IF FROM-DAYS-IN-MONTH = 31 AND WDF-DAY NOT = 31 ELPMNT
00336                      SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER           ELPMNT
00337                      GO TO 0020-CHECK-TO-DAY                      ELPMNT
00338                   ELSE                                            ELPMNT
00339                      GO TO 0020-CHECK-TO-DAY                      ELPMNT
00340            ELSE                                                   ELPMNT
00341                GO TO 0020-CHECK-TO-DAY.                           ELPMNT
00342                                                                   ELPMNT
00343      IF WDF-MONTH NOT = WDT-MONTH                                 ELPMNT
00344         IF WDF-MONTH = 02                                         ELPMNT
00345            PERFORM 0050-CHECK-LEAP-YEAR THRU 0050-EXIT            ELPMNT
00346            IF WDF-DAY = 29                                        ELPMNT
00347               ADD 1              TO SAVE-ODD-DAYS-OVER            ELPMNT
00348            ELSE                                                   ELPMNT
00349               ADD 2              TO SAVE-ODD-DAYS-OVER            ELPMNT
00350         ELSE                                                      ELPMNT
00351            IF FROM-DAYS-IN-MONTH = 31 AND WDF-DAY NOT = 31        ELPMNT
00352               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.         ELPMNT
00353                                                                   ELPMNT
00354  0020-CHECK-TO-DAY.                                               ELPMNT
00355         IF WDT-MONTH = 02 AND SAVE-ODD-DAYS-OVER NOT = 0          ELPMNT
00356            IF WDT-DAY = 28                                        ELPMNT
00357               ADD 2              TO SAVE-ODD-DAYS-OVER            ELPMNT
00358            ELSE                                                   ELPMNT
00359               IF WDT-DAY = 29                                     ELPMNT
00360                  ADD 1           TO SAVE-ODD-DAYS-OVER            ELPMNT
00361               ELSE                                                ELPMNT
00362                  NEXT SENTENCE                                    ELPMNT
00363         ELSE                                                      ELPMNT
00364            IF WDT-DAY = 31 AND SAVE-ODD-DAYS-OVER NOT = 0         ELPMNT
00365               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.         ELPMNT
00366                                                                   ELPMNT
00367      COMPUTE WST-DAILY-RATE ROUNDED EQUAL                         ELPMNT
00368                             CP-ORIGINAL-BENEFIT / 30              ELPMNT
00369      MOVE WST-DAILY-RATE          TO WS-DAILY-RATE                ELPMNT
00370      COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL                       ELPMNT
00371          (CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS) +            ELPMNT
00372          (WST-DAILY-RATE     * SAVE-ODD-DAYS-OVER).               ELPMNT
00373                                                                   ELPMNT
00374      GO TO 0100-RETURN.                                           ELPMNT
00375                                                                   ELPMNT
00376  0050-CHECK-LEAP-YEAR.                                            ELPMNT
00377                                                                   ELPMNT
00378      IF FROM-DAYS-IN-MONTH = 29                                   ELPMNT
00379         SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER.                       ELPMNT
00380                                                                   ELPMNT
00381  0050-EXIT.                                                       ELPMNT
00382      EXIT.                                                        ELPMNT
00383                                                                   ELPMNT
00384      EJECT                                                        ELPMNT
00385  0100-RETURN.                                                     ELPMNT
00386                                                                   ELPMNT
00387      MOVE CALCULATION-PASS-AREA TO DFHCOMMAREA.                   ELPMNT
00388                                                                   ELPMNT
00389      EXEC CICS RETURN                                             ELPMNT
00390      END-EXEC.                                                    ELPMNT
00391                                                                      CL**4
00392      GOBACK.                                                      ELPMNT
00393                                                                      CL**4
00394  9700-LINK-DATE-CONVERT.                                          ELPMNT
00395                                                                   ELPMNT
00396      EXEC CICS LINK                                               ELPMNT
00397           PROGRAM    ('ELDATCV')                                  ELPMNT
00398           COMMAREA   (DATE-CONVERSION-DATA)                       ELPMNT
00399           LENGTH     (DC-COMM-LENGTH)                             ELPMNT
00400      END-EXEC.                                                    ELPMNT
00401                                                                   ELPMNT
00402  9700-EXIT.                                                       ELPMNT
00403      EXIT.                                                        ELPMNT
00404      EJECT                                                        ELPMNT
