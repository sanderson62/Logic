00001  IDENTIFICATION DIVISION.                                         06/01/98
00002                                                                   EL009
00003  PROGRAM-ID.                 EL009 .                                 LV058
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 09:25:21.                    CL**5
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*36
00008 *                            VMOD=2.006                              CL*36
00009 *                                                                 EL009
00009 *                                                                 EL009
00010 *AUTHOR.     LOGIC,INC.                                              CL**5
00011 *            DALLAS, TEXAS.                                          CL**5
00012                                                                   EL009
00013 *DATE-COMPILED.                                                      CL**5
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00021 *            *                                                   *   CL**5
00022 *            *****************************************************   CL**5
00023                                                                   EL009
00024 *REMARKS.    TRANSACTION - EXDT - DATE CONVERSION WORKSHEET          CL**5
00025 *        THIS PROGRAM IS USED TO REVIEW THE DATE CONVERSIONS         CL**5
00026 *        DONE BY THE SUBROUTINE ELDATCV.                             CL**5
00027                                                                   EL009
00028                                                                   EL009
00029      EJECT                                                        EL009
00030  ENVIRONMENT DIVISION.                                            EL009
00031  DATA DIVISION.                                                   EL009
00032  WORKING-STORAGE SECTION.                                         EL009
00033  01  LCP-TIME-OF-DAY-XX.                                             CL**5
00034      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**5
00035      05  FILLER                    PIC 99.                           CL**5
00036  01  LCP-CICS-TIME                 PIC 9(15).                        CL**5
00037  77  FILLER  PIC X(32)  VALUE '********************************'. EL009
00038  77  FILLER  PIC X(32)  VALUE '*    EL009 WORKING STORAGE     *'. EL009
00039  77  FILLER  PIC X(32)  VALUE '************ VMOD=2.006 ********'.    CL*36
00040                                                                   EL009
00041  01  WS-DATE-AREA.                                                EL009
00042      12  SAVE-DATE           PIC X(8).                               CL**2
00043                                                                      CL**2
00044      12  W-CURRENT-DATE      PIC 9(7)      VALUE 0.                  CL*18
00045      12  FILLER REDEFINES W-CURRENT-DATE.                            CL**2
00046          16 FILLER           PIC 9(1).                               CL*44
00047          16 W-CD-CCYY        PIC 9(3).                               CL*54
00048          16 W-CD-CCYY-R REDEFINES W-CD-CCYY.                         CL*54
00049             20 W-JULIAN-CEN  PIC 9(1).                               CL*54
00050             20 W-CD-YEAR     PIC 9(2).                               CL*54
00051          16 FILLER           PIC 9(3).                               CL**3
00052      12  W-CURRENT-DT REDEFINES W-CURRENT-DATE                       CL*16
00053                              PIC 9(07).                              CL*16
00054      12  W-CURRENT-DT-R REDEFINES W-CURRENT-DATE.                    CL*16
00055          16  W-CD-YEAR-CD        PIC  9.                             CL*16
00056          16  W-CURRENT-YEAR-YY   PIC  99.                            CL*16
00057          16  W-CURRENT-MONTH     PIC  99.                            CL*45
00058          16  W-CURRENT-DAY       PIC  99.                            CL*45
00059                                                                      CL**2
00060      12  W-FLOAT-YEAR        PIC  9(3).                              CL*17
00061      12  W-FLOAT-YY REDEFINES W-FLOAT-YEAR.                          CL*17
00062          16  W-FLOAT-YEAR-CD         PIC  9.                         CL*17
00063          16  W-FLOAT-YEAR-YY         PIC  99.                        CL*17
00064      12  SAVE-BIN-DATE       PIC  X(2)   VALUE SPACES.               CL**2
00065                                                                   EL009
00066      COPY ELCDATE.                                                   CL*19
00067                                                                   EL009
00068      EJECT                                                        EL009
00069      COPY ELCDATW1.                                                  CL**3
00070                                                                   EL009
00071      EJECT                                                        EL009
00072  01  MISC-SWITCHS.                                                EL009
00073      12  EDIT-ERROR-FLAG     PIC X       VALUE 'N'.               EL009
00074          88  EDIT-ERROR          VALUE 'Y'.                       EL009
00075                                                                   EL009
00076  01  FILLER.                                                      EL009
00077      05  WS-DAY-OF-WEEK-ARRAY.                                    EL009
00078          10  FILLER              PICTURE X(9) VALUE 'SUNDAY'.     EL009
00079          10  FILLER              PICTURE X(9) VALUE 'MONDAY'.     EL009
00080          10  FILLER              PICTURE X(9) VALUE 'TUESDAY'.    EL009
00081          10  FILLER              PICTURE X(9) VALUE 'WEDNESDAY'.  EL009
00082          10  FILLER              PICTURE X(9) VALUE 'THURSDAY'.   EL009
00083          10  FILLER              PICTURE X(9) VALUE 'FRIDAY'.     EL009
00084          10  FILLER              PICTURE X(9) VALUE 'SATURDAY'.   EL009
00085                                                                   EL009
00086      05  WS-DAY-OF-WEEK          REDEFINES                        EL009
00087          WS-DAY-OF-WEEK-ARRAY    PICTURE X(9)                     EL009
00088          OCCURS 7 TIMES.                                          EL009
00089                                                                   EL009
00090  01  BINARY-WORK             PIC 9(8)     COMP.                   EL009
00091  01  WORK-BINARY  REDEFINES BINARY-WORK.                          EL009
00092      12  FILLER              PIC XX.                              EL009
00093      12  BINARY-DTE          PIC XX.                              EL009
00094                                                                   EL009
00095  01  HEXADECIMAL-DAYS.                                            EL009
00096      05  HEX-CHARACTER               PIC X                        EL009
00097                        OCCURS 4 TIMES  INDEXED BY U.              EL009
00098                                                                   EL009
00099  01  CONVERSION-TABLE.                                            EL009
00100      05  FILLER          PIC X(21)  VALUE '104096002560001600001'.EL009
00101      05  FILLER          PIC X(21)  VALUE '208192005120003200002'.EL009
00102      05  FILLER          PIC X(21)  VALUE '312288007680004800003'.EL009
00103      05  FILLER          PIC X(21)  VALUE '416384010240006400004'.EL009
00104      05  FILLER          PIC X(21)  VALUE '520480012800008000005'.EL009
00105      05  FILLER          PIC X(21)  VALUE '624576015360009600006'.EL009
00106      05  FILLER          PIC X(21)  VALUE '728672017920011200007'.EL009
00107      05  FILLER          PIC X(21)  VALUE '832768020480012800008'.EL009
00108      05  FILLER          PIC X(21)  VALUE '936864023040014400009'.EL009
00109      05  FILLER          PIC X(21)  VALUE 'A40960025600016000010'.EL009
00110      05  FILLER          PIC X(21)  VALUE 'B45056028160017600011'.EL009
00111      05  FILLER          PIC X(21)  VALUE 'C49152030720019200012'.EL009
00112      05  FILLER          PIC X(21)  VALUE 'D53248033280020800013'.EL009
00113      05  FILLER          PIC X(21)  VALUE 'E57344035840022400014'.EL009
00114      05  FILLER          PIC X(21)  VALUE 'F61440038400024000015'.EL009
00115                                                                   EL009
00116  01  HEXADECIMAL-TABLE  REDEFINES  CONVERSION-TABLE.              EL009
00117      05  HEXADECIMAL-VALUES  OCCURS 15 TIMES  INDEXED BY V.       EL009
00118          10  HEX-VALUE               PIC X.                       EL009
00119          10  DECIMAL-NUMBERS  OCCURS 4 TIMES  INDEXED BY W.       EL009
00120              15  DECIMAL-VALUE       PIC 9(5).                    EL009
00121                                                                   EL009
00122  01  MISC-WORK-AREAS.                                             EL009
00123      12  TIME-IN             PIC 9(6).                            EL009
00124      12  TIME-WRK  REDEFINES TIME-IN.                             EL009
00125          16  TIME-HHMM       PIC 99V99.                           EL009
00126          16  FILLER          PIC XX.                              EL009
00127      12  TIME-OUT            PIC 99.99.                           EL009
00128      12  TEXT-AREA           PIC X(66).                           EL009
00129                                                                   EL009
00130  01  MISC-COMP        COMP.                                       EL009
00131      12  WS-IC               PIC S9(4)   VALUE -1.                EL009
00132      12  TEXT-LENGTH         PIC S9(4)   VALUE +66.               EL009
00133      12  BLANK-TEXT          PIC S9(4)   VALUE +1.                EL009
00134                                                                   EL009
00135  01  TEXT-MESSAGES.                                               EL009
00136      12  MAPFAIL-MSG.                                             EL009
00137          16  FILLER          PIC X(40)                            EL009
00138            VALUE '     MAPFAIL ENCOUNTERED - TRANSACTION A'.      EL009
00139          16  FILLER          PIC X(6)                             EL009
00140            VALUE 'BORTED'.                                        EL009
00141      12  TRAN-COMPLETE-MSG.                                       EL009
00142          16  FILLER          PIC X(45)                            EL009
00143            VALUE '     CLEAR ENTERED - SESSION ENDED          '.  EL009
00144                                                                   EL009
00145      EJECT                                                        EL009
00146      COPY ELCDATE REPLACING                                          CL**3
00147      DATE-CONVERSION-DATA        BY  DATE-CONVERSION-DATA-INIT    EL009
00148      DC-COMM-LENGTH              BY  INIT-DC-COMM-LENGTH             CL*16
00149      DC-OPTION-CODE              BY  INIT-OPTION-CODE             EL009
00150      BIN-TO-GREG                 BY  INIT-BIN-TO-GREG             EL009
00151      ELAPSED-BETWEEN-BIN         BY  INIT-ELAPSED-BETWEEN-BIN     EL009
00152      ELAPSED-BETWEEN-BIN-30      BY  INIT-ELAPSED-BETWEEN-BIN-30  EL009
00153      EDIT-GREG-TO-BIN            BY  INIT-EDIT-GREG-TO-BIN        EL009
00154      YMD-GREG-TO-BIN             BY  INIT-YMD-GREG-TO-BIN         EL009
00155      MDY-GREG-TO-BIN             BY  INIT-MDY-GREG-TO-BIN         EL009
00156      JULIAN-TO-BIN               BY  INIT-JULIAN-TO-BIN           EL009
00157      BIN-PLUS-ELAPSED            BY  INIT-BIN-PLUS-ELAPSED        EL009
00158      FIND-CENTURY                BY  INIT-FIND-CENTURY               CL**7
00159      ELAPSED-BETWEEN-BIN-3       BY  INIT-ELAPSED-BETWEEN-BIN-3      CL**7
00160      EDIT-GREG-TO-BIN-3          BY  INIT-EDIT-GREG-TO-BIN-3         CL**7
00161      YMD-GREG-TO-BIN-3           BY  INIT-YMD-GREG-TO-BIN-3          CL**7
00162      MDY-GREG-TO-BIN-3           BY  INIT-MDY-GREG-TO-BIN-3          CL**7
00163      JULIAN-TO-BIN-3             BY  INIT-JULIAN-TO-BIN-3            CL**7
00164      BIN-PLUS-ELAPSED-3          BY  INIT-BIN-PLUS-ELAPSED-3         CL**7
00165      JULIAN-EXPANDED-TO-BIN      BY  INIT-JULIAN-EXPANDED-TO-BIN     CL**7
00166      JULIAN-EXPANDED-TO-BIN-3    BY  INIT-JULIAN-EXPANDED-TO-BIN-3   CL**7
00167      BIN-TO-JULIAN-EXPANDED      BY  INIT-BIN-TO-JULIAN-EXPANDED     CL**7
00168      JULIAN-EXPANDED             BY  INIT-JULIAN-EXPANDED            CL**7
00169      CHECK-LEAP-YEAR             BY  INIT-CHECK-LEAP-YEAR            CL**7
00170      BIN-3-TO-GREG               BY  INIT-BIN-3-TO-GREG              CL**7
00171      CYMD-GREG-TO-BIN-3          BY  INIT-CYMD-GREG-TO-BIN-3         CL**7
00172      MDCY-GREG-TO-BIN-3          BY  INIT-MDCY-GREG-TO-BIN-3         CL**7
00173      CYMD-GREG-TO-BIN            BY  INIT-CYMD-GREG-TO-BIN           CL**7
00174      MDCY-GREG-TO-BIN            BY  INIT-MDCY-GREG-TO-BIN           CL**7
00175      MDY-GREG-TO-JULIAN          BY  INIT-MDY-GREG-TO-JULIAN         CL**7
00176      MDCY-GREG-TO-JULIAN         BY  INIT-MDCY-GREG-TO-JULIAN        CL**7
00177      YMD-GREG-TO-JULIAN          BY  INIT-YMD-GREG-TO-JULIAN         CL**7
00178      CYMD-GREG-TO-JULIAN         BY  INIT-CYMD-GREG-TO-JULIAN        CL**7
00179      THREE-CHARACTER-BIN         BY  INIT-THREE-CHARACTER-BIN        CL**7
00180      GREGORIAN-TO-BIN            BY  INIT-GREGORIAN-TO-BIN           CL**7
00181      BIN-TO-GREGORIAN            BY  INIT-BIN-TO-GREGORIAN           CL**7
00182      JULIAN-TO-BINARY            BY  INIT-JULIAN-TO-BINARY           CL**7
00183      DC-ERROR-CODE               BY  INIT-DC-ERROR-CODE           EL009
00184      NO-CONVERSION-ERROR         BY  INIT-NO-CONVERSION-ERROR     EL009
00185      DATE-CONVERSION-ERROR       BY  INIT-DATE-CONVERSION-ERROR   EL009
00186      DATE-IS-ZERO                BY  INIT-DATE-IS-ZERO            EL009
00187      DATE-IS-NON-NUMERIC         BY  INIT-DATE-IS-NON-NUMERIC     EL009
00188      DATE-IS-INVALID             BY  INIT-DATE-IS-INVALID         EL009
00189      DATE1-GREATER-DATE2         BY  INIT-DATE1-GREATER-DATE2     EL009
00190      ELAPSED-PLUS-NEGATIVE       BY  INIT-ELAPSED-PLUS-NEGATIVE      CL*13
00191      DATE-INVALID-OPTION         BY  INIT-DATE-INVALID-OPTION     EL009
00192      INVALID-CENTURY             BY  INIT-INVALID-CENTURY            CL**9
00193      ONLY-CENTURY                BY  INIT-ONLY-CENTURY               CL**9
00194      ONLY-LEAP-YEAR              BY  INIT-ONLY-LEAP-YEAR             CL**9
00195      VALID-CENTURY-LEAP-YEAR     BY  INIT-VALID-CENTURY-LEAP-YEAR    CL**9
00196      DC-END-OF-MONTH             BY  INIT-DC-END-OF-MONTH         EL009
00197      CALCULATE-END-OF-MONTH      BY  INIT-CALCULATE-END-OF-MONTH  EL009
00198      DC-CENTURY-ADJUSTMENT       BY  INIT-DC-CENTURY-ADJUSTMENT      CL**4
00199      USE-NORMAL-PROCESS          BY  INIT-USE-NORMAL-PROCESS         CL*13
00200      ADJUST-DOWN-100-YRS         BY  INIT-ADJUST-DOWN-100-YRS        CL**4
00201      ADJUST-UP-100-YRS           BY  INIT-ADJUST-UP-100-YRS          CL**4
00202      DC-CONVERSION-DATES         BY  INIT-DC-CONVERSION-DATES     EL009
00203      DC-BIN-DATE-1               BY  INIT-DC-BIN-DATE-1           EL009
00204      DC-BIN-DATE-2               BY  INIT-DC-BIN-DATE-2           EL009
00205      DC-GREG-DATE-1-EDIT         BY  INIT-DC-GREG-DATE-1-EDIT     EL009
00206      DC-GREG-DATE-1-EDIT-R       BY  INIT-DC-GREG-DATE-1-EDIT-R      CL*13
00207      DC-EDIT1-MONTH              BY  INIT-DC-EDIT1-MONTH             CL*15
00208      SLASH1-1                    BY  INIT-SLASH1-1                   CL*15
00209      DC-EDIT1-DAY                BY  INIT-DC-EDIT1-DAY               CL*15
00210      SLASH1-2                    BY  INIT-SLASH1-2                   CL*15
00211      DC-EDIT1-YEAR               BY  INIT-DC-EDIT1-YEAR              CL*15
00212      DC-GREG-DATE-2-EDIT         BY  INIT-DC-GREG-DATE-2-EDIT     EL009
00213      DC-GREG-DATE-2-EDIT-R       BY  INIT-DC-GREG-DATE-2-EDIT-R      CL*13
00214      DC-EDIT2-MONTH              BY  INIT-DC-EDIT2-MONTH             CL*13
00215      SLASH2-1                    BY  INIT-SLASH2-1                   CL*13
00216      DC-EDIT2-DAY                BY  INIT-DC-EDIT2-DAY               CL*13
00217      SLASH2-2                    BY  INIT-SLASH2-2                   CL*13
00218      DC-EDIT2-YEAR               BY  INIT-DC-EDIT2-YEAR              CL*13
00219      DC-GREG-DATE-1-YMD          BY  INIT-DC-GREG-DATE-1-YMD      EL009
00220      DC-GREG-DATE-1-YMD-R        BY  INIT-DC-GREG-DATE-1-YMD-R       CL*13
00221      DC-YMD-YEAR                 BY  INIT-DC-YMD-YEAR                CL*13
00222      DC-YMD-MONTH                BY  INIT-DC-YMD-MONTH               CL*13
00223      DC-YMD-DAY                  BY  INIT-DC-YMD-DAY                 CL*13
00224      DC-GREG-DATE-1-MDY          BY  INIT-DC-GREG-DATE-1-MDY      EL009
00225      DC-GREG-DATE-1-MDY-R        BY  INIT-DC-GREG-DATE-1-MDY-R       CL*13
00226      DC-MDY-MONTH                BY  INIT-DC-MDY-MONTH               CL*13
00227      DC-MDY-DAY                  BY  INIT-DC-MDY-DAY                 CL*13
00228      DC-MDY-YEAR                 BY  INIT-DC-MDY-YEAR                CL*13
00229      DC-GREG-DATE-1-ALPHA        BY  INIT-DC-GREG-DATE-1-ALPHA    EL009
00230      DC-ALPHA-MONTH              BY  INIT-DC-ALPHA-MONTH             CL*13
00231      DC-ALPHA-DAY                BY  INIT-DC-ALPHA-DAY               CL*13
00232      DC-ALPHA-CENTURY            BY  INIT-DC-ALPHA-CENTURY           CL*15
00233      DC-ALPHA-CEN-N              BY  INIT-DC-ALPHA-CEN-N             CL*13
00234      DC-ALPHA-YEAR               BY  INIT-DC-ALPHA-YEAR              CL*15
00235      DC-ELAPSED-MONTHS           BY  INIT-DC-ELAPSED-MONTHS       EL009
00236      DC-ODD-DAYS-OVER            BY  INIT-DC-ODD-DAYS-OVER        EL009
00237      DC-ELAPSED-DAYS             BY  INIT-DC-ELAPSED-DAYS         EL009
00238      DC-JULIAN-YYDDD             BY  INIT-DC-JULIAN-YYDDD         EL009
00239      DC-JULIAN-DT                BY  INIT-DC-JULIAN-DT               CL*13
00240      DC-JULIAN-YEAR              BY  INIT-DC-JULIAN-YEAR             CL*13
00241      DC-JULIAN-DAYS              BY  INIT-DC-JULIAN-DAYS             CL*13
00242      DC-DAYS-IN-MONTH            BY  INIT-DC-DAYS-IN-MONTH        EL009
00243      DC-DAY-OF-WEEK              BY  INIT-DC-DAY-OF-WEEK          EL009
00244      DC-DAY-OF-WEEK2             BY  INIT-DC-DAY-OF-WEEK2            CL**8
00245      DATE-CONVERSION-VARIBLES                                        CL**9
00246                         BY  INIT-DATE-CONVERSION-VARIBLES            CL**9
00247      HOLD-CENTURY-1              BY  INIT-HOLD-CENTURY-1             CL**8
00248      HOLD-CENTURY-1-SPLIT        BY  INIT-HOLD-CENTURY-1-SPLIT       CL**8
00249      HOLD-CEN-1-CCYY             BY  INIT-HOLD-CEN-1-CCYY            CL*12
00250      HOLD-CEN-1-CC               BY  INIT-HOLD-CEN-1-CC              CL*12
00251      HOLD-CEN-1-YY               BY  INIT-HOLD-CEN-1-YY              CL*12
00252      HOLD-CEN-1-MO               BY  INIT-HOLD-CEN-1-MO              CL*12
00253      HOLD-CEN-1-DA               BY  INIT-HOLD-CEN-1-DA              CL*12
00254      HOLD-CENTURY-1-R            BY  INIT-HOLD-CENTURY-1-R           CL**8
00255      HOLD-CEN-1-R-MO             BY  INIT-HOLD-CEN-1-R-MO            CL*12
00256      HOLD-CEN-1-R-DA             BY  INIT-HOLD-CEN-1-R-DA            CL*12
00257      HOLD-CEN-1-R-CCYY           BY  INIT-HOLD-CEN-1-R-CCYY          CL*12
00258      HOLD-CEN-1-R-C              BY  INIT-HOLD-CEN-1-R-C             CL*12
00259      HOLD-CEN-1-R-Y              BY  INIT-HOLD-CEN-1-R-Y             CL*12
00260      HOLD-CENTURY-1-X            BY  INIT-HOLD-CENTURY-1-X           CL**8
00261      HOLD-CEN-1-X-CCYY           BY  INIT-HOLD-CEN-1-X-CCYY          CL*12
00262      HOLD-CEN-1-X-CC             BY  INIT-HOLD-CEN-1-X-CC            CL*12
00263      HOLD-CEN-1-X-YY             BY  INIT-HOLD-CEN-1-X-YY            CL*12
00264      HOLD-CEN-1-X-MO             BY  INIT-HOLD-CEN-1-X-MO            CL*12
00265      HOLD-CEN-1-X-DA             BY  INIT-HOLD-CEN-1-X-DA            CL*12
00266      HOLD-CENTURY-1-R-X          BY  INIT-HOLD-CENTURY-1-R-X         CL**8
00267      HOLD-CEN-1-R-X-MO           BY  INIT-HOLD-CEN-1-R-X-MO          CL*12
00268      HOLD-CEN-1-R-X-DA           BY  INIT-HOLD-CEN-1-R-X-DA          CL*12
00269      HOLD-CEN-1-R-X-CCYY         BY  INIT-HOLD-CEN-1-R-X-CCYY        CL*12
00270      HOLD-CEN-1-R-X-CC           BY  INIT-HOLD-CEN-1-R-X-CC          CL*12
00271      HOLD-CEN-1-R-X-YY           BY  INIT-HOLD-CEN-1-R-X-YY          CL*12
00272      DC-BIN-DATE-EXPAND-1        BY  INIT-DC-BIN-DATE-EXPAND-1       CL**8
00273      DC-BIN-DATE-EXPAND-2        BY  INIT-DC-BIN-DATE-EXPAND-2       CL**8
00274      DC-JULIAN-DATE-1            BY  INIT-DC-JULIAN-DATE-1           CL**8
00275      DC-JULIAN-DATE-1-R          BY  INIT-DC-JULIAN-DATE-1-R         CL*12
00276      DC-JULIAN-1-CCYY            BY  INIT-DC-JULIAN-1-CCYY           CL*12
00277      DC-JULIAN-1-CC              BY  INIT-DC-JULIAN-1-CC             CL*12
00278      DC-JULIAN-1-YR              BY  INIT-DC-JULIAN-1-YR             CL*12
00279      DC-JULIAN-DA-1              BY  INIT-DC-JULIAN-DA-1             CL*12
00280      DC-JULIAN-DATE-2            BY  INIT-DC-JULIAN-DATE-2           CL**7
00281      DC-JULIAN-DATE-2-R          BY  INIT-DC-JULIAN-DATE-2-R         CL*12
00282      DC-JULIAN-2-CCYY            BY  INIT-DC-JULIAN-2-CCYY           CL*12
00283      DC-JULIAN-2-CC              BY  INIT-DC-JULIAN-2-CC             CL*12
00284      DC-JULIAN-2-YR              BY  INIT-DC-JULIAN-2-YR             CL*12
00285      DC-JULIAN-DA-2              BY  INIT-DC-JULIAN-DA-2             CL*12
00286      DC-GREG-DATE-A-EDIT         BY  INIT-DC-GREG-DATE-A-EDIT        CL**7
00287      DC-EDITA-MONTH              BY  INIT-DC-EDITA-MONTH             CL*12
00288      SLASHA-1                    BY  INIT-SLASHA-1                   CL*12
00289      DC-EDITA-DAY                BY  INIT-DC-EDITA-DAY               CL*12
00290      SLASHA-2                    BY  INIT-SLASHA-2                   CL*12
00291      DC-EDITA-CCYY               BY  INIT-DC-EDITA-CCYY              CL*12
00292      DC-EDITA-CENT               BY  INIT-DC-EDITA-CENT              CL*12
00293      DC-EDITA-YEAR               BY  INIT-DC-EDITA-YEAR              CL*12
00294      DC-GREG-DATE-B-EDIT         BY  INIT-DC-GREG-DATE-B-EDIT        CL**7
00295      DC-EDITB-MONTH              BY  INIT-DC-EDITB-MONTH             CL*12
00296      SLASHB-1                    BY  INIT-SLASHA-1                   CL*12
00297      DC-EDITB-DAY                BY  INIT-DC-EDITA-DAY               CL*12
00298      SLASHB-2                    BY  INIT-SLASHB-2                   CL*12
00299      DC-EDITB-CCYY               BY  INIT-DC-EDITB-CCYY              CL*12
00300      DC-EDITB-CENT               BY  INIT-DC-EDITB-CENT              CL*12
00301      DC-EDITB-YEAR               BY  INIT-DC-EDITB-YEAR              CL*12
00302      DC-GREG-DATE-CYMD           BY  INIT-DC-GREG-DATE-CYMD          CL**7
00303      DC-GREG-DATE-CYMD-R         BY  INIT-DC-GREG-DATE-CYMD-R        CL*12
00304      DC-CYMD-CEN                 BY  INIT-DC-CYMD-CEN                CL*12
00305      DC-CYMD-YEAR                BY  INIT-DC-CYMD-YEAR               CL*12
00306      DC-CYMD-MONTH               BY  INIT-DC-CYMD-MONTH              CL*12
00307      DC-CYMD-DAY                 BY  INIT-DC-CYMD-DAY                CL*12
00308      DC-GREG-DATE-MDCY           BY  INIT-DC-GREG-DATE-MDCY          CL*13
00309      DC-GREG-DATE-MDCY-R         BY  INIT-DC-GREG-DATE-MDCY-R        CL*12
00310      DC-MDCY-MONTH               BY  INIT-DC-MDCY-MONTH              CL*16
00311      DC-MDCY-DAY                 BY  INIT-DC-MDCY-DAY                CL*12
00312      DC-MDCY-CEN                 BY  INIT-DC-MDCY-CEN                CL*12
00313      DC-MDCY-YEAR                BY  INIT-DC-MDCY-YEAR.              CL*13
00314                                                                   EL009
00315      EJECT                                                        EL009
00316      COPY ELCATTR.                                                   CL**3
00317      EJECT                                                        EL009
00318      COPY ELCAID.                                                    CL**3
00319      EJECT                                                           CL**3
00320      COPY EL009S.                                                    CL**3
00321                                                                   EL009
00322 /                                                                    CL*43
00323  PROCEDURE DIVISION.                                              EL009
00324 ***  Y2K, PROJ 7744                                                  CL*22
00325      INITIALIZE  DATE-CONVERSION-DATA-INIT                           CL*20
00326                  BINARY-WORK                                         CL*21
00327                  HEXADECIMAL-DAYS                                    CL*23
00328                                                                      CL*21
00329      MOVE +200                  TO INIT-DC-COMM-LENGTH               CL*20
00330                                                                      CL*22
00331      MOVE SPACES                TO DC-ERROR-CODE.                    CL*58
00332                                                                      CL*58
00333      MOVE ZEROS                 TO DC-ELAPSED-MONTHS                 CL*58
00334                                    DC-ODD-DAYS-OVER               EL009
00335                                    DC-ELAPSED-DAYS                EL009
00336                                    DC-JULIAN-YEAR                 EL009
00337                                    DC-JULIAN-DAYS                 EL009
00338                                    DC-DAYS-IN-MONTH.              EL009
00339                                                                      CL*28
00340      MOVE LOW-VALUES            TO DC-BIN-DATE-1                     CL*28
00341                                    DC-BIN-DATE-2                     CL*28
00342                                    INIT-DC-BIN-DATE-1                CL*28
00343                                    INIT-DC-BIN-DATE-2.               CL*31
00344 ***  Y2K, PROJ 7744                                                  CL*24
00345                                                                   EL009
00346      MOVE EIBDATE               TO W-CURRENT-DATE.                   CL**3
00347 *    ACCEPT W-CURRENT-DATE FROM DATE.                                CL**3
00348 ***  Y2K, PROJ 7744                                                  CL*52
00349      MOVE '1'                  TO FIND-CENTURY-FLAG                  CL*52
00350      MOVE W-JULIAN-CEN         TO SEARCH-CENTURY-CD-N                CL*52
00351                                   W-FLOAT-YEAR-CD.                   CL*53
00352      PERFORM 0800-FIND-CENTURY                                       CL*52
00353      IF NO-CONVERSION-ERROR                                          CL*52
00354          MOVE FOUND-CENTURY-N  TO CURRENT-CENTURY-1-N                CL*52
00355      END-IF.                                                         CL*52
00356 ***  Y2K, PROJ 7744                                                  CL*52
00357      COMPUTE W-FLOAT-YEAR = W-CD-CCYY -                              CL*57
00358                            ((W-JULIAN-CEN*100)+ 70)                  CL*57
00359                                                                      CL*47
00360      MOVE W-CURRENT-DATE        TO DC-JULIAN-YYDDD.                  CL**3
00361      MOVE '5'                   TO DC-OPTION-CODE.                EL009
00362      PERFORM 8500-DATE-CONVERSION.                                EL009
00363      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.                        CL**2
00364      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.                    CL**2
00365                                                                      CL*45
00366      MOVE DATE-CONVERSION-DATA-INIT  TO  DATE-CONVERSION-DATA     EL009
00367                                                                   EL009
00368      EXEC CICS HANDLE AID                                         EL009
00369          CLEAR   (8950-CLEAR-RETURN)                              EL009
00370          PF1     (4010-BIN-TO-GREG)                               EL009
00371          PF2     (4020-COMPUTE-ELAPSED)                           EL009
00372          PF6     (4020-COMPUTE-ELAPSED)                           EL009
00373          PF3     (4030-GREG-TO-BIN)                               EL009
00374          PF4     (4040-JULIAN-TO-OTHER)                           EL009
00375          PF5     (4050-BINARY-PLUS-ELAPSED)                       EL009
00376          ANYKEY  (4060-UNSUPPORTED)                               EL009
00377          END-EXEC.                                                EL009
00378                                                                   EL009
00379      EXEC CICS HANDLE CONDITION                                   EL009
00380          MAPFAIL (8900-MAPFAIL)                                   EL009
00381          END-EXEC.                                                EL009
00382                                                                   EL009
00383      IF EIBCALEN GREATER THAN ZERO                                EL009
00384          GO TO 0300-RECEIVE-MAP.                                  EL009
00385                                                                   EL009
00386      MOVE LOW-VALUES             TO EL009AO.                      EL009
00387      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**5
00388      END-EXEC                                                        CL**5
00389      EXEC CICS FORMATTIME                                            CL**5
00390                ABSTIME(LCP-CICS-TIME)                                CL**5
00391                TIME(LCP-TIME-OF-DAY-XX)                              CL**5
00392      END-EXEC                                                        CL**5
00393      MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.                            CL**5
00394      MOVE TIME-HHMM              TO TIME-OUT.                     EL009
00395      MOVE TIME-OUT               TO STIMEO.                       EL009
00396      MOVE SAVE-DATE              TO SDATEO.                       EL009
00397                                                                   EL009
00398  0100-SEND-INITIAL-MAP.                                           EL009
00399      MOVE WS-IC                  TO BSTRINL.                      EL009
00400                                                                   EL009
00401      EXEC CICS SEND                                               EL009
00402          MAP     ('EL009A')                                       EL009
00403          MAPSET  ('EL009S')                                       EL009
00404          ERASE                                                    EL009
00405          FREEKB                                                   EL009
00406          CURSOR                                                   EL009
00407          END-EXEC.                                                EL009
00408                                                                   EL009
00409      GO TO 0250-RETURN.                                           EL009
00410                                                                   EL009
00411  0200-SEND-MAP-DATAONLY.                                          EL009
00412      MOVE WS-IC                  TO BSTRINL.                      EL009
00413                                                                   EL009
00414      EXEC CICS SEND                                               EL009
00415          MAP     ('EL009A')                                       EL009
00416          MAPSET  ('EL009S')                                       EL009
00417          DATAONLY                                                 EL009
00418          FREEKB                                                   EL009
00419          CURSOR                                                   EL009
00420          END-EXEC.                                                EL009
00421      MOVE LOW-VALUES             TO EL009AO.                      EL009
00422                                                                   EL009
00423  0250-RETURN.                                                     EL009
00424      EXEC CICS RETURN                                             EL009
00425          TRANSID  (EIBTRNID)                                      EL009
00426          COMMAREA (DATE-CONVERSION-DATA)                          EL009
00427          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL009
00428                                                                   EL009
00429                                                                   EL009
00430  0300-RECEIVE-MAP.                                                EL009
00431      EXEC CICS RECEIVE                                            EL009
00432          MAP     ('EL009A')                                       EL009
00433          MAPSET  ('EL009S')                                       EL009
00434          END-EXEC.                                                EL009
00435                                                                   EL009
00436      EJECT                                                        EL009
00437  4010-BIN-TO-GREG.                                                EL009
00438                                                                      CL**4
00439      IF  CENTURYL GREATER THAN ZERO                                  CL**4
00440          IF  CENTURYI EQUAL '1' OR '2' OR ' '                        CL**4
00441              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT           CL**4
00442          ELSE                                                        CL**4
00443              GO TO 6000-INPUT-ERROR.                                 CL**4
00444                                                                      CL**4
00445      IF BSTRINL NOT = ZERO                                        EL009
00446          MOVE BSTRINI            TO HEXADECIMAL-DAYS              EL009
00447          MOVE ZERO               TO BINARY-WORK                   EL009
00448          SET U, V, W TO 1                                         EL009
00449          PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT                EL009
00450          MOVE BINARY-DTE         TO DC-BIN-DATE-1                 EL009
00451          MOVE SPACE              TO DC-OPTION-CODE                EL009
00452          GO TO 5000-FORMAT-OUTPUT                                 EL009
00453      ELSE                                                         EL009
00454          GO TO 6000-INPUT-ERROR.                                  EL009
00455                                                                   EL009
00456  4020-COMPUTE-ELAPSED.                                            EL009
00457      IF BSTRINL = ZERO OR                                         EL009
00458         BENDINL = ZERO                                            EL009
00459          GO TO 6000-INPUT-ERROR.                                  EL009
00460                                                                      CL**4
00461      IF  CENTURYL GREATER THAN ZERO                                  CL**4
00462                                                                      CL**4
00463          IF  CENTURYI EQUAL '1' OR '2' OR ' '                        CL**4
00464              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT           CL**4
00465                                                                      CL**4
00466          ELSE                                                        CL**4
00467              GO TO 6000-INPUT-ERROR.                                 CL**4
00468                                                                   EL009
00469      MOVE BSTRINI                TO HEXADECIMAL-DAYS.             EL009
00470      MOVE ZERO                   TO BINARY-WORK.                  EL009
00471      SET U, V, W TO 1.                                            EL009
00472      PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT.                   EL009
00473      MOVE BINARY-DTE             TO DC-BIN-DATE-1.                EL009
00474      MOVE BENDINI                TO HEXADECIMAL-DAYS.             EL009
00475      MOVE ZERO                   TO BINARY-WORK.                  EL009
00476      SET U, V, W TO 1.                                            EL009
00477      PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT.                   EL009
00478      MOVE BINARY-DTE             TO DC-BIN-DATE-2.                EL009
00479                                                                   EL009
00480      IF EIBAID = DFHPF6                                           EL009
00481          MOVE '7'                TO DC-OPTION-CODE                EL009
00482      ELSE                                                         EL009
00483          MOVE '1'                TO DC-OPTION-CODE.               EL009
00484                                                                   EL009
00485      GO TO 5000-FORMAT-OUTPUT.                                    EL009
00486                                                                   EL009
00487  4030-GREG-TO-BIN.                                                EL009
00488                                                                      CL**4
00489      IF  CENTURYL GREATER THAN ZERO                                  CL**4
00490                                                                      CL**4
00491          IF  CENTURYI EQUAL '1' OR '2' OR ' '                        CL**4
00492              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT           CL**4
00493                                                                      CL**4
00494          ELSE                                                        CL**4
00495              GO TO 6000-INPUT-ERROR.                                 CL**4
00496                                                                      CL**4
00497      IF GEDTINL = 8                                               EL009
00498          MOVE GEDTINI            TO DC-GREG-DATE-1-EDIT           EL009
00499          MOVE '2'                TO DC-OPTION-CODE                EL009
00500          GO TO 5000-FORMAT-OUTPUT.                                EL009
00501                                                                   EL009
00502      IF GMDYINL = 6                                               EL009
00503          MOVE GMDYINI            TO DC-GREG-DATE-1-MDY            EL009
00504          MOVE '4'                TO DC-OPTION-CODE                EL009
00505          GO TO 5000-FORMAT-OUTPUT.                                EL009
00506                                                                   EL009
00507      IF GYMDINL = 6                                               EL009
00508          MOVE GYMDINI            TO DC-GREG-DATE-1-YMD            EL009
00509          MOVE '3'                TO DC-OPTION-CODE                EL009
00510          GO TO 5000-FORMAT-OUTPUT.                                EL009
00511                                                                   EL009
00512      GO TO 6000-INPUT-ERROR.                                      EL009
00513                                                                   EL009
00514  4040-JULIAN-TO-OTHER.                                            EL009
00515      IF JINL = ZERO                                               EL009
00516          GO TO 6000-INPUT-ERROR                                   EL009
00517      ELSE                                                         EL009
00518          MOVE JINI               TO DC-JULIAN-YYDDD               EL009
00519          MOVE '5'                TO DC-OPTION-CODE.                  CL**4
00520                                                                      CL**4
00521      IF  CENTURYL GREATER THAN ZERO                                  CL**4
00522                                                                      CL**4
00523          IF  CENTURYI EQUAL '1' OR '2' OR ' '                        CL**4
00524              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT           CL**4
00525                                                                      CL**4
00526          ELSE                                                        CL**4
00527              GO TO 6000-INPUT-ERROR.                                 CL**4
00528                                                                      CL**4
00529      GO TO 5000-FORMAT-OUTPUT.                                       CL**4
00530                                                                   EL009
00531  4050-BINARY-PLUS-ELAPSED.                                        EL009
00532      IF BSTRINL = ZERO                                            EL009
00533          GO TO 6000-INPUT-ERROR.                                  EL009
00534                                                                   EL009
00535      IF MONENDL GREATER THAN ZERO                                 EL009
00536          MOVE '1'                TO  DC-END-OF-MONTH.             EL009
00537                                                                      CL**4
00538      IF  CENTURYL GREATER THAN ZERO                                  CL**4
00539          MOVE '1'                TO  DC-CENTURY-ADJUSTMENT.          CL**4
00540                                                                   EL009
00541      IF EMOSINL GREATER THAN ZERO                                 EL009
00542          EXEC CICS BIF DEEDIT                                     EL009
00543              FIELD  (EMOSINI)                                     EL009
00544              LENGTH (6) END-EXEC                                  EL009
00545          IF EMOSINI IS NOT NUMERIC                                EL009
00546              GO TO 6200-NON-NUMERIC-DATA                          EL009
00547          ELSE                                                     EL009
00548              MOVE EMOSINI        TO  DC-ELAPSED-MONTHS            EL009
00549                                      EMOSINO.                     EL009
00550                                                                   EL009
00551      IF EDAYINL GREATER THAN ZERO                                 EL009
00552          EXEC CICS BIF DEEDIT                                     EL009
00553              FIELD  (EDAYINI)                                     EL009
00554              LENGTH (6) END-EXEC                                  EL009
00555          IF EDAYINI IS NOT NUMERIC                                EL009
00556              GO TO 6200-NON-NUMERIC-DATA                          EL009
00557          ELSE                                                     EL009
00558              MOVE EDAYINI        TO  DC-ELAPSED-DAYS              EL009
00559                                      EDAYINO.                     EL009
00560                                                                   EL009
00561      MOVE BSTRINI                TO HEXADECIMAL-DAYS.             EL009
00562      MOVE ZERO                   TO BINARY-WORK.                  EL009
00563      SET U V W TO 1.                                              EL009
00564      PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT.                   EL009
00565      MOVE BINARY-DTE             TO DC-BIN-DATE-1.                EL009
00566      MOVE '6'                    TO DC-OPTION-CODE.               EL009
00567      GO TO 5000-FORMAT-OUTPUT.                                    EL009
00568                                                                   EL009
00569  4060-UNSUPPORTED.                                                EL009
00570      MOVE 'KEY PRESSED IS NOT SUPPORTED - PF1 THRU PF4 OR CLEAR'  EL009
00571                                  TO ERROUTO.                      EL009
00572      GO TO 0200-SEND-MAP-DATAONLY.                                EL009
00573                                                                   EL009
00574      EJECT                                                        EL009
00575  5000-FORMAT-OUTPUT.                                              EL009
00576      PERFORM 8500-DATE-CONVERSION                                 EL009
00577                                                                   EL009
00578 ***  Y2K, PROJ 7744                                                  CL*31
00579      MOVE DC-GREG-DATE-1-EDIT    TO GEDTOUTO.                        CL*40
00580      MOVE DC-GREG-DATE-2-EDIT    TO GEDOUT2O.                        CL*40
00581                                                                   EL009
00582      IF GEDOUT2O = '  /  /  ' OR '00/00/00'                          CL*41
00583          MOVE '        '         TO GEDOUT2O                         CL*40
00584      END-IF.                                                         CL*37
00585                                                                      CL*40
00586      MOVE DC-JULIAN-YYDDD        TO JOUTO.                        EL009
00587      MOVE DC-GREG-DATE-1-ALPHA   TO ALFOUTO.                      EL009
00588                                                                   EL009
00589      MOVE DC-ELAPSED-MONTHS      TO EMOSOUTO.                     EL009
00590      MOVE DC-ODD-DAYS-OVER       TO ODDOUTO.                      EL009
00591      MOVE DC-ELAPSED-DAYS        TO EDAYOUTO.                     EL009
00592      MOVE DC-DAYS-IN-MONTH       TO EDAYMONO.                     EL009
00593                                                                   EL009
00594      IF DC-DAY-OF-WEEK NOT = ZERO                                 EL009
00595          MOVE WS-DAY-OF-WEEK (DC-DAY-OF-WEEK)  TO  DAY1O             CL*30
00596      ELSE                                                            CL*30
00597          MOVE SPACES                           TO  DAY1O             CL*30
00598      END-IF                                                          CL*30
00599                                                                   EL009
00600      IF DC-DAY-OF-WEEK2 NOT = ZERO                                EL009
00601          MOVE WS-DAY-OF-WEEK (DC-DAY-OF-WEEK2) TO  DAY2O             CL*30
00602      ELSE                                                            CL*30
00603          MOVE SPACES                           TO  DAY2O             CL*30
00604      END-IF                                                          CL*30
00605                                                                   EL009
00606      MOVE ZERO                   TO HEXADECIMAL-DAYS BINARY-WORK. EL009
00607      MOVE DC-BIN-DATE-1          TO BINARY-DTE.                   EL009
00608      SET V W TO 1.                                                EL009
00609      PERFORM 7000-GENERATE-HEX-CHARACTERS THRU 7499-EXIT.         EL009
00610                                                                      CL*33
00611      IF HEXADECIMAL-DAYS NOT = ZEROS                                 CL*35
00612          MOVE HEXADECIMAL-DAYS   TO BOUTO                            CL*33
00613      ELSE                                                            CL*33
00614          MOVE SPACES             TO BOUTO                            CL*33
00615      END-IF                                                          CL*33
00616                                                                   EL009
00617      MOVE ZERO                   TO HEXADECIMAL-DAYS BINARY-WORK. EL009
00618      MOVE DC-BIN-DATE-2          TO BINARY-DTE.                   EL009
00619      SET V W TO 1.                                                EL009
00620      PERFORM 7000-GENERATE-HEX-CHARACTERS THRU 7499-EXIT.         EL009
00621                                                                      CL*33
00622      IF HEXADECIMAL-DAYS NOT = ZEROS                                 CL*35
00623          MOVE HEXADECIMAL-DAYS   TO BOUT2O                           CL*33
00624      ELSE                                                            CL*33
00625          MOVE SPACES             TO BOUT2O                           CL*33
00626      END-IF                                                          CL*33
00627 ***  Y2K, PROJ 7744                                                  CL*31
00628                                                                   EL009
00629      IF DC-ERROR-CODE = SPACE                                     EL009
00630          MOVE 'DATE CONVERSION SUCCESSFUL   RETURN CODE SPACE'    EL009
00631                                  TO ERROUTO.                      EL009
00632                                                                   EL009
00633      IF DC-ERROR-CODE = '1'                                       EL009
00634          MOVE 'DATE CONVERSION ERROR (DATE IS ZEROCODE = 1)'      EL009
00635                                  TO ERROUTO.                      EL009
00636                                                                   EL009
00637      IF DC-ERROR-CODE = '2'                                       EL009
00638          MOVE 'DATE CONVERSION ERROR (DATE NOT NUMERICCODE = 2)'  EL009
00639                                  TO ERROUTO.                      EL009
00640                                                                   EL009
00641      IF DC-ERROR-CODE = '3'                                       EL009
00642          MOVE 'DATE CONVERSION ERROR (DATE IS INVALIDCODE = 3)'   EL009
00643                                  TO ERROUTO.                      EL009
00644                                                                   EL009
00645      IF DC-ERROR-CODE = '4'                                       EL009
00646          MOVE 'DATE CONVERSION ERROR (DATE1 IS AFTER DATE2 = 4)'  EL009
00647                                  TO ERROUTO.                      EL009
00648                                                                   EL009
00649      IF DC-ERROR-CODE = '5'                                       EL009
00650          MOVE 'DATE CONVERSION ERROR (BIN PLUS ELAPSED NEG = 5)'  EL009
00651                                  TO ERROUTO.                      EL009
00652                                                                   EL009
00653      IF DC-ERROR-CODE = '9'                                       EL009
00654          MOVE 'DATE CONVERSION ERROR = 9)'                        EL009
00655                                  TO ERROUTO.                      EL009
00656                                                                   EL009
00657      GO TO 0200-SEND-MAP-DATAONLY.                                EL009
00658                                                                   EL009
00659      EJECT                                                        EL009
00660  6000-INPUT-ERROR.                                                EL009
00661      MOVE 'DATA GIVEN IS NOT CONSISTENT WITH REQUEST - RE-ENTER'  EL009
00662          TO ERROUTO.                                              EL009
00663      INSPECT GEDTINI CONVERTING ' ' TO LOW-VALUES                    CL**5
00664      INSPECT GMDYINI CONVERTING ' ' TO LOW-VALUES                    CL**5
00665      INSPECT GYMDINI CONVERTING ' ' TO LOW-VALUES                    CL**5
00666      GO TO 0200-SEND-MAP-DATAONLY.                                EL009
00667                                                                   EL009
00668                                                                   EL009
00669  6200-NON-NUMERIC-DATA.                                           EL009
00670      MOVE 'ELAPSED MONTHS OR DAYS IS NON-NUMERIC - RE-ENTER'      EL009
00671          TO ERROUTO.                                              EL009
00672      GO TO 0200-SEND-MAP-DATAONLY.                                EL009
00673      EJECT                                                        EL009
00674                                                                   EL009
00675  7000-GENERATE-HEX-CHARACTERS.                                    EL009
00676      IF BINARY-WORK LESS THAN DECIMAL-VALUE (V, W)                EL009
00677          IF W LESS THAN 4                                         EL009
00678              SET W UP BY 1                                        EL009
00679              GO TO 7000-GENERATE-HEX-CHARACTERS                   EL009
00680          ELSE                                                     EL009
00681              GO TO 7499-EXIT.                                     EL009
00682                                                                   EL009
00683      SET V TO 15.                                                 EL009
00684                                                                   EL009
00685  7050-GET-HEXADECIMAL-DIGIT.                                      EL009
00686      IF BINARY-WORK NOT LESS THAN DECIMAL-VALUE (V, W)            EL009
00687          SET U TO W                                               EL009
00688          MOVE HEX-VALUE (V)      TO HEX-CHARACTER (U)             EL009
00689          COMPUTE BINARY-WORK =                                    EL009
00690                        BINARY-WORK - DECIMAL-VALUE (V, W)         EL009
00691          SET V TO 1                                               EL009
00692          GO TO 7000-GENERATE-HEX-CHARACTERS.                      EL009
00693                                                                   EL009
00694      SET V DOWN BY 1.                                             EL009
00695      GO TO 7050-GET-HEXADECIMAL-DIGIT.                            EL009
00696                                                                   EL009
00697  7499-EXIT.                                                       EL009
00698      EXIT.                                                        EL009
00699                                                                   EL009
00700  7500-FIND-HEX-CHAR.                                              EL009
00701      IF U = 5                                                     EL009
00702          GO TO 7999-EXIT.                                         EL009
00703                                                                   EL009
00704      IF HEX-CHARACTER (U) = HEX-VALUE (V)                         EL009
00705          GO TO 7550-ADD-VALUE-IN.                                 EL009
00706                                                                   EL009
00707      IF V LESS 15                                                 EL009
00708          SET V UP BY 1                                            EL009
00709          GO TO 7500-FIND-HEX-CHAR                                 EL009
00710      ELSE                                                         EL009
00711          SET U, W UP BY 1                                         EL009
00712          SET V TO 1                                               EL009
00713          GO TO 7500-FIND-HEX-CHAR.                                EL009
00714                                                                   EL009
00715  7550-ADD-VALUE-IN.                                               EL009
00716      COMPUTE BINARY-WORK = BINARY-WORK + DECIMAL-VALUE (V, W).    EL009
00717      SET U, W UP BY 1.                                            EL009
00718      SET V TO 1.                                                  EL009
00719      GO TO 7500-FIND-HEX-CHAR.                                    EL009
00720                                                                   EL009
00721  7999-EXIT.                                                       EL009
00722      EXIT.                                                        EL009
00723                                                                   EL009
00724  8500-DATE-CONVERSION SECTION.                                    EL009
00725      EXEC CICS ASKTIME                                            EL009
00726          END-EXEC.                                                EL009
00727                                                                   EL009
uktdel*8510-DATE-CONVERSION. COPY ELCDATP1.                             EL009
uktins 8510-DATE-CONVERSION.
uktins     COPY ELCDATP1.
00729      EXEC CICS ASKTIME                                            EL009
00730          END-EXEC.                                                EL009
00731                                                                   EL009
00732  8590-EXIT.                                                       EL009
00733      EXIT.                                                        EL009
00734                                                                   EL009
00735      EJECT                                                        EL009
uktdel*0200-DATE-CONVERSION-ROUTINES SECTION. COPY ELCDATP2 REPLACING   EL009
uktins 0200-DATE-CONVERSION-ROUTINES SECTION.
uktins     COPY ELCDATP2 REPLACING
00737      DC-JULIAN-DATE              BY  DC-JULIAN-YYDDD.             EL009
00738                                                                   EL009
00739      EJECT                                                        EL009
00740  8900-MAPFAIL SECTION.                                            EL009
00741      MOVE MAPFAIL-MSG            TO TEXT-AREA.                    EL009
00742      GO TO 8990-SEND-TEXT.                                        EL009
00743                                                                   EL009
00744  8950-CLEAR-RETURN.                                               EL009
00745      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.                    EL009
00746                                                                   EL009
00747  8990-SEND-TEXT.                                                  EL009
00748      EXEC CICS SEND TEXT                                          EL009
00749          FROM    (TEXT-AREA)                                      EL009
00750          LENGTH  (TEXT-LENGTH)                                    EL009
00751          ERASE                                                    EL009
00752          FREEKB                                                   EL009
00753          END-EXEC.                                                EL009
00754                                                                   EL009
00755  9000-RETURN-TO-CICS.                                             EL009
00756      EXEC CICS RETURN                                             EL009
00757          END-EXEC.                                                EL009
00758                                                                   EL009
00759      GOBACK.                                                      EL009
00760                                                                   EL009
