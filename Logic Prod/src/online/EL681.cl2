00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL681
00003  PROGRAM-ID.                 EL681 .                                 LV009
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 02/14/96 07:59:26.                    CL**8
00007 *                            VMOD=2.009                              CL**9
00008 *                                                                 EL681
00009 *AUTHOR.     LOGIC INC.                                              CL**8
00010 *            DALLAS, TEXAS.                                          CL**8
00011                                                                   EL681
00012 *DATE-COMPILED.                                                      CL**8
00013 *SECURITY.   *****************************************************   CL**8
00014 *            *                                                   *   CL**8
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00016 *            *                                                   *   CL**8
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00020 *            *                                                   *   CL**8
00021 *            *****************************************************   CL**8
00022                                                                   EL681
00023 *REMARKS.    TRANSACTION - EXF7 - PAYMENT CALCULATION PRINT.         CL**7
00024 *         THIS PROGRAM IS STARTED  FROM EL680  TO PRINT              CL**7
00025 *         THE RESULTS OF THE PAYMENT CALCULATION PROGRAM.            CL**7
00026                                                                   EL681
00027      EJECT                                                        EL681
00028  ENVIRONMENT DIVISION.                                            EL681
00029  DATA DIVISION.                                                   EL681
00030  WORKING-STORAGE SECTION.                                         EL681
00031  77  FILLER  PIC X(32) VALUE '********************************'.  EL681
00032  77  FILLER  PIC X(32) VALUE '     EL681  WORKING-STORAGE     '.  EL681
00033  77  FILLER  PIC X(32) VALUE '***********VMOD=2.009 **********'.     CL**9
00034                                                                   EL681
00035  77  ELEN    PIC S9(4)  COMP    VALUE +426.                          CL**3
00036                                                                   EL681
00037  77  WS-SAVED-PI-CARD-ID         PIC XXX.                            CL**9
00038                                                                      CL**9
00039  01  WORK-AREAS.                                                  EL681
00040      03  WS-NEXT-TRAN            PIC X(4).                           CL**9
00041      03  WS-TERMINAL-ID.                                          EL681
00042          05  WS-TERM-PREFIX      PIC XX.                          EL681
00043          05  FILLER              PIC XX.                          EL681
00044                                                                   EL681
00045  01  PRT-LINES.                                                   EL681
00046     05  PRT-LN-1.                                                 EL681
00047       07  P1-DATE   PIC X(8).                                     EL681
00048       07  FILLER    PIC X(15) VALUE SPACES.                       EL681
00049       07  FILLER    PIC X(20) VALUE 'PAYMENT CALCULATIONS'.       EL681
00050       07  FILLER    PIC X(20) VALUE SPACES.                       EL681
00051       07  THIS-PGM  PIC X(6)  VALUE 'EL681'.                         CL**9
00052     05  PRT-LN-2.                                                 EL681
00053       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00054       07  FILLER    PIC X(33)                                        CL**4
00055                        VALUE 'NAME............................ '.    CL**4
00056       07  P2-NAME   PIC X(29).                                    EL681
00057     05  PRT-LN-3.                                                 EL681
00058       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00059       07  FILLER    PIC X(33)                                        CL**4
00060                        VALUE 'AMOUNT REQUESTED................ '.    CL**4
00061       07  P3-AMT    PIC X(13).                                    EL681
00062     05  PRT-LN-4.                                                 EL681
00063       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00064       07  FILLER    PIC X(33)                                        CL**4
00065                        VALUE 'ANNUAL PERCENTAGE RATE.......... '.    CL**4
00066       07  P4-INT    PIC X(8).                                        CL**3
00067       07  FILLER    PIC X(33)                                        CL**4
00068                        VALUE '    ****************** RESULTS **'.    CL**4
00069     05  PRT-LN-5.                                                 EL681
00070       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00071       07  FILLER    PIC X(33)                                        CL**4
00072                        VALUE 'PMT FREQ. (MO,SM,BW,WK,SA,AN,13).'.    CL**6
00073       07  P5-FRQ    PIC XX.                                       EL681
00074       07  FILLER    PIC X(5)  VALUE SPACES.                          CL**3
00075       07  FILLER    PIC X(25) VALUE '    * TOTAL FINANCED.... '.     CL**3
00076       07  P5-AMT    PIC X(13).                                    EL681
00077     05  PRT-LN-6.                                                 EL681
00078       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00079       07  FILLER    PIC X(33)                                        CL**4
00080                        VALUE 'NUMBER OF PAYMENTS.............. '.    CL**4
00081       07  P6-PMTS   PIC X(3).                                     EL681
00082       07  FILLER    PIC X(4)  VALUE SPACES.                          CL**3
00083       07  FILLER    PIC X(25) VALUE '    * LIFE INSURANCE.... '.     CL**3
00084       07  P6-LIFE   PIC X(13).                                    EL681
00085     05  PRT-LN-7.                                                 EL681
00086       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00087       07  FILLER    PIC X(33)                                        CL**4
00088                        VALUE 'ADDTL DAYS TO 1ST PMT / CHG CODE '.    CL**4
00089       07  P7-DAYS   PIC X(3).                                     EL681
00090       07  FILLER    PIC X     VALUE SPACE.                           CL**3
00091       07  FILLER    PIC X     VALUE '/'.                             CL**3
00092       07  P7-ADD    PIC X     VALUE SPACE.                           CL**3
00093       07  FILLER    PIC X     VALUE SPACE.                           CL**3
00094       07  FILLER    PIC X(25) VALUE '    * A & H   INS....... '.     CL**3
00095       07  P7-DIS    PIC X(13).                                    EL681
00096     05  PRT-LN-8.                                                 EL681
00097       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00098       07  FILLER    PIC X(33)                                        CL**4
00099                        VALUE 'LF CALC.BASIS (AF,TP,NP)........ '.    CL**4
00100       07  P8-BASI   PIC X(2).                                     EL681
00101       07  FILLER    PIC X(5)  VALUE SPACES.                          CL**3
00102       07  FILLER    PIC X(25) VALUE '    * PAYMENT........... '.     CL**3
00103       07  P8-PMT    PIC X(13).                                    EL681
00104     05  PRT-LN-9.                                                 EL681
00105       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00106       07  FILLER    PIC X(33)                                        CL**4
00107                        VALUE 'LF RATES USED ARE O/B? (Y/N).... '.    CL**4
00108       07  P9-OBYN   PIC X.                                           CL**2
00109       07  FILLER    PIC X(6)  VALUE SPACES.                          CL**3
00110       07  FILLER    PIC X(25) VALUE '    * BALLOON PAYMENT... '.     CL**3
00111       07  P9-BPY    PIC X(13).                                    EL681
00112     05  PRT-LN-10.                                                EL681
00113       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00114       07  FILLER    PIC X(33)                                        CL**4
00115                        VALUE 'NP BENEFIT INCLUDES PREM. (Y/N). '.    CL**4
00116       07  P10-NPRE  PIC X.                                           CL**2
00117       07  FILLER    PIC X(6)  VALUE SPACES.                          CL**3
00118       07  FILLER    PIC X(25) VALUE '    * TOTAL PAYMENT..... '.     CL**3
00119       07  P10-TPY   PIC X(13).                                    EL681
00120     05  PRT-LN-11.                                                EL681
00121       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00122       07  FILLER    PIC X(33)                                        CL**4
00123                        VALUE '# PMTS COVERED (TRUNCATED LIFE). '.    CL**4
00124       07  P11-TRNC  PIC X(3).                                        CL**2
00125       07  FILLER    PIC X(4)  VALUE SPACES.                          CL**3
00126       07  FILLER    PIC X(25) VALUE '    * TOTAL INTEREST.... '.     CL**3
00127       07  P11-INT   PIC X(13).                                    EL681
00128     05  PRT-LN-12.                                                EL681
00129       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00130       07  FILLER    PIC X(33)                                        CL**4
00131                        VALUE 'EXTRA INTEREST PERIODS.......... '.    CL**4
00132       07  P12-EXTR  PIC X(7).                                        CL**2
00133       07  FILLER    PIC X(34)                                        CL**4
00134                        VALUE '     *****************************'.   CL**4
00135     05  PRT-LN-13.                                                EL681
00136       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00137       07  FILLER    PIC X(33)                                        CL**4
00138                        VALUE 'LIFE RATE PER $100/YR........... '.    CL**4
00139       07  P13-LRAT  PIC X(13).                                       CL**2
00140     05  PRT-LN-14.                                                EL681
00141       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00142       07  FILLER    PIC X(33)                                        CL**4
00143                        VALUE 'LIFE DEVIATION (100 = NO DEV.).. '.    CL**4
00144       07  P14-LDEV  PIC X(13).                                       CL**2
00145     05  PRT-LN-15.                                                EL681
00146       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00147       07  FILLER    PIC X(33)                                        CL**4
00148                        VALUE 'A&H  RATE PER $100.............. '.    CL**4
00149       07  P15-DRAT  PIC X(13).                                       CL**2
00150     05  PRT-LN-16.                                                EL681
00151       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00152       07  FILLER    PIC X(33)                                        CL**4
00153                        VALUE 'A&H. DEVIATION (100 = NO DEV.).. '.    CL**4
00154       07  P16-DDEV  PIC X(13).                                       CL**2
00155     05  PRT-LN-17.                                                EL681
00156       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00157       07  FILLER    PIC X(33)                                        CL**4
00158                        VALUE 'PMTS. COVERED IF NOT SAME AS LF. '.    CL**4
00159       07  P17-PCOV  PIC XXX.                                         CL**2
00160     05  PRT-LN-18.                                                EL681
00161       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00162       07  FILLER    PIC X(33)                                        CL**4
00163                        VALUE 'BALLOON AMOUNT.................. '.    CL**4
00164       07  P18-BAMT  PIC X(13).                                       CL**2
00165     05  PRT-LN-19.                                                EL681
00166       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00167       07  FILLER    PIC X(33)                                        CL**4
00168                        VALUE 'BALLOON LEVEL PORTION LIFE RATE. '.    CL**4
00169       07  P19-LLRT  PIC X.                                           CL**2
00170     05  PRT-LN-20.                                                   CL**2
00171       07  FILLER    PIC XX    VALUE SPACES.                          CL**4
00172       07  FILLER    PIC X(33)                                        CL**4
00173                        VALUE 'BALLOON COINCIDES W/LAST P(Y/N). '.    CL**4
00174       07  P20-BCON  PIC X.                                           CL**2
00175       07  FILLER    PIC X(40)                                        CL**4
00176                  VALUE '    *ALL RATES ASSUME 4 DECIMAL PLACES*'.    CL**4
00177                                                                   EL681
00178                                      COPY ELCDMD34.                  CL**9
00179      EJECT                                                        EL681
00180                                      COPY EL680S.                    CL**7
00181      EJECT                                                        EL681
00182                                      COPY ELCAID.                    CL**7
00183  01  PF-AID REDEFINES DFHAID.                                     EL681
00184      05  FILLER                      PIC X(8).                    EL681
00185      05  PF-VALUES  OCCURS 24        PIC X.                       EL681
00186      EJECT                                                        EL681
00187                                      COPY ELPRTCVD.                  CL**7
00188                                                                      CL**3
00189                                      COPY ELCINTF SUPPRESS.          CL**7
00190      EJECT                                                        EL681
00191  LINKAGE SECTION.                                                 EL681
00192  01  DFHCOMMAREA                     PIC X(1024).                    CL**3
00193 *01 PARM-LIST .                                                      CL**8
00194 *    05  FILLER                      PIC S9(8) COMP.                 CL**8
00195 *    05  CNTL-POINTER                PIC S9(8) COMP.                 CL**8
00196                                      COPY ELCCNTL SUPPRESS.          CL**8
00197      EJECT                                                        EL681
00198                                                                   EL681
00199  PROCEDURE DIVISION.                                              EL681
00200                                                                   EL681
00201       MOVE SPACES                 TO DL34-PROCESS-TYPE.              CL**9
00202                                                                      CL**9
00203  0000-START.                                                      EL681
00204      EXEC CICS  HANDLE CONDITION                                  EL681
00205          ERROR    (8300-ABEND)                                       CL**3
00206          PGMIDERR (8900-PGMIDERR)                                    CL**3
00207          ENDDATA  (9999-FINALIZE)                                    CL**3
00208      END-EXEC.                                                    EL681
00209                                                                   EL681
00210      EXEC CICS  RETRIEVE                                          EL681
00211                 INTO    (PROGRAM-INTERFACE-BLOCK)                    CL**9
00212                 LENGTH  (PI-COMM-LENGTH)                             CL**9
00213      END-EXEC.                                                    EL681
00214                                                                      CL**9
00215      MOVE PI-PROGRAM-WORK-AREA       TO EL680AI.                     CL**9
00216                                                                      CL**9
00217 * DLO034 OPEN WHEN DMD OR CID                                        CL**9
00218      MOVE PI-COMPANY-ID              TO WS-SAVED-PI-CARD-ID.         CL**9
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**9
00220          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**9
00221              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**9
00222              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**9
00223              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**9
00224              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**9
00225              MOVE SPACES             TO DL34-PRINT-LINE              CL**9
00226              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**9
00227              EXEC CICS LINK                                          CL**9
00228                  PROGRAM    ('DLO034')                               CL**9
00229                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**9
00230                  LENGTH     (DLO034-REC-LENGTH)                      CL**9
00231              END-EXEC                                                CL**9
00232              IF DL34-RETURN-CODE NOT = 'OK'                          CL**9
00233                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**9
00234                                      TO WS-PASSED-DATA               CL**9
00235 *                PERFORM 400-SEND-TEXT                               CL**9
00236                  EXEC CICS RETURN                                    CL**9
00237                  END-EXEC.                                           CL**9
00238                                                                   EL681
CIDMOD*    MOVE 'CID1'   TO CSO-PRINT-ID.                                    000
CIDMOD*    MOVE 'F'      TO DRS-SW.                                          000
CIDMOD*    PERFORM  ELPRTCVP  THRU  ELPRTCVP-EXIT.                           000
CIDMOD*    MOVE ' '      TO DRS-SW.                                          000
CIDMOD*                                                                      000
00239 **********   PRINT THE PAYMENT CALCULATION RESULTS **********     EL681
00240                                                                   EL681
00241      MOVE ADATEO        TO P1-DATE.                               EL681
00242      MOVE '1'           TO WS-PASSED-CNTL-CHAR.                   EL681
00243      MOVE  PRT-LN-1     TO WS-PASSED-DATA.                        EL681
00244      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00245                                                                   EL681
00246      MOVE ANAMEO        TO P2-NAME.                               EL681
00247      MOVE '-'           TO WS-PASSED-CNTL-CHAR.                   EL681
00248      MOVE  PRT-LN-2     TO WS-PASSED-DATA.                        EL681
00249      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00250                                                                   EL681
00251      MOVE AAMOUNTO      TO P3-AMT.                                EL681
00252      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00253      MOVE  PRT-LN-3     TO WS-PASSED-DATA.                        EL681
00254      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00255                                                                   EL681
00256      MOVE AINTRATO      TO P4-INT.                                EL681
00257      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00258      MOVE  PRT-LN-4     TO WS-PASSED-DATA.                        EL681
00259      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00260                                                                   EL681
00261      MOVE AFREQO        TO P5-FRQ.                                   CL**6
00262      MOVE BAMOUNTO      TO P5-AMT.                                EL681
00263      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00264      MOVE  PRT-LN-5     TO WS-PASSED-DATA.                        EL681
00265      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00266                                                                   EL681
00267      MOVE ANOPMTSO      TO P6-PMTS.                                  CL**6
00268      MOVE BLAMTO        TO P6-LIFE.                               EL681
00269      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00270      MOVE  PRT-LN-6     TO WS-PASSED-DATA.                        EL681
00271      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00272                                                                   EL681
00273      MOVE AADDAYSO      TO P7-DAYS.                               EL681
00274      MOVE BDAMTO        TO P7-DIS.                                EL681
00275      MOVE ADDCHGO       TO P7-ADD.                                   CL**3
00276      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00277      MOVE  PRT-LN-7     TO WS-PASSED-DATA.                        EL681
00278      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00279                                                                   EL681
00280      MOVE ABASISO       TO P8-BASI.                               EL681
00281      MOVE BMOPMTO       TO P8-PMT.                                EL681
00282      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00283      MOVE  PRT-LN-8     TO WS-PASSED-DATA.                        EL681
00284      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00285                                                                   EL681
00286      MOVE OBYORNO       TO P9-OBYN.                                  CL**2
00287      MOVE BALAMTO       TO P9-BPY.                                EL681
00288      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00289      MOVE  PRT-LN-9     TO WS-PASSED-DATA.                        EL681
00290      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00291                                                                   EL681
00292      MOVE ANPREMO       TO P10-NPRE.                                 CL**2
00293      MOVE BTOTPMTO      TO P10-TPY.                               EL681
00294      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00295      MOVE  PRT-LN-10    TO WS-PASSED-DATA.                        EL681
00296      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00297                                                                   EL681
00298      MOVE ATRUNCO       TO P11-TRNC.                                 CL**2
00299      MOVE BTOTINTO      TO P11-INT.                               EL681
00300      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00301      MOVE  PRT-LN-11    TO WS-PASSED-DATA.                        EL681
00302      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00303                                                                   EL681
00304      MOVE AEXTRAO       TO P12-EXTR.                                 CL**2
00305      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00306      MOVE  PRT-LN-12    TO WS-PASSED-DATA.                        EL681
00307      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00308                                                                   EL681
00309      MOVE ALRATEO       TO P13-LRAT.                                 CL**2
00310      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00311      MOVE  PRT-LN-13    TO WS-PASSED-DATA.                        EL681
00312      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00313                                                                   EL681
00314      MOVE ALFDEVO       TO P14-LDEV.                                 CL**2
00315      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00316      MOVE  PRT-LN-14    TO WS-PASSED-DATA.                        EL681
00317      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00318                                                                   EL681
00319      MOVE ADRATEO       TO P15-DRAT.                                 CL**2
00320      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00321      MOVE  PRT-LN-15    TO WS-PASSED-DATA.                        EL681
00322      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00323                                                                   EL681
00324      MOVE AAHDEVO       TO P16-DDEV.                                 CL**2
00325      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00326      MOVE  PRT-LN-16    TO WS-PASSED-DATA.                        EL681
00327      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00328                                                                   EL681
00329      MOVE ANOTLFO       TO P17-PCOV.                                 CL**2
00330      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00331      MOVE  PRT-LN-17    TO WS-PASSED-DATA.                        EL681
00332      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00333                                                                   EL681
00334      MOVE BALAMTO       TO P18-BAMT.                                 CL**2
00335      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00336      MOVE  PRT-LN-18    TO WS-PASSED-DATA.                        EL681
00337      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00338                                                                   EL681
00339      MOVE BLRATEO       TO P19-LLRT.                                 CL**2
00340      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                   EL681
00341      MOVE  PRT-LN-19    TO WS-PASSED-DATA.                        EL681
00342      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                           CL**2
00343                                                                      CL**2
00344      MOVE BALYNO        TO P20-BCON.                                 CL**2
00345      MOVE ' '           TO WS-PASSED-CNTL-CHAR.                      CL**2
00346      MOVE  PRT-LN-20    TO WS-PASSED-DATA.                           CL**2
00347      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00348                                                                   EL681
00349      GO TO 9999-FINALIZE.                                         EL681
00350                                                                   EL681
00351 ***************************************************************** EL681
uktdel*PRINT-RTN.  COPY ELPRTCVP.                                       EL681
uktins PRINT-RTN.
uktins     COPY ELPRTCVP.
00353                                                                      CL**9
00354 ***************************************************************** EL681
00355                                                                   EL681
00356  8300-ABEND.                                                      EL681
00357      MOVE DFHEIBLK               TO WS-PASSED-DATA.               EL681
00358      EXEC CICS LINK                                               EL681
00359          PROGRAM   ('EL004')                                      EL681
00360          COMMAREA  (WS-PASSED-DATA)                               EL681
00361          LENGTH    (72)                                           EL681
00362      END-EXEC.                                                    EL681
00363                                                                   EL681
00364      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00365      GO TO 9999-FINALIZE.                                         EL681
00366                                                                   EL681
00367  8900-PGMIDERR.                                                   EL681
00368      MOVE '* PROG NOT FOUND, NOTIFY DATA PROCESSING *'            EL681
00369                           TO WS-PASSED-DATA.                      EL681
00370      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00371      GO TO 9999-FINALIZE.                                         EL681
CIDMOD                                                                       000
CIDMOD     GOBACK.                                                           000
00372                                                                   EL681
00373  8900-EXIT.                                                       EL681
00374      EXIT.                                                           CL**9
00375      EJECT                                                        EL681
00376                                                                   EL681
00377  8999-RETURN-CICS.                                                EL681
00378                                                                      CL**9
00379 * DLO034 CLOSE                                                       CL**9
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**9
00381          IF DL34-PROCESS-TYPE IS NOT EQUAL TO SPACES                 CL**9
00382              MOVE 'C'                TO DL34-PROCESS-TYPE            CL**9
00383              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**9
00384              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**9
00385              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**9
00386              MOVE SPACES             TO DL34-PRINT-LINE              CL**9
00387                                         DL34-OVERRIDE-PRINTER-ID     CL**9
00388              EXEC CICS LINK                                          CL**9
00389                  PROGRAM    ('DLO034')                               CL**9
00390                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**9
00391                  LENGTH     (DLO034-REC-LENGTH)                      CL**9
00392              END-EXEC                                                CL**9
00393              IF DL34-RETURN-CODE NOT = 'OK'                          CL**9
00394                  MOVE  '**DLO034 CLOSE ERROR - ABORT**'              CL**9
00395                                      TO WS-PASSED-DATA.              CL**9
00396 *                PERFORM 400-SEND-TEXT.                              CL**9
00397                                                                      CL**9
00398      MOVE 'EXF6'    TO WS-NEXT-TRAN.                              EL681
00399                                                                   EL681
00400      MOVE EIBTRMID  TO WS-TERMINAL-ID.                            EL681
00401                                                                   EL681
00402      IF WS-TERM-PREFIX = 'DU'                                     EL681
00403          EXEC CICS RETURN                                            CL**3
00404              TRANSID  (WS-NEXT-TRAN)                              EL681
00405              COMMAREA (PROGRAM-INTERFACE-BLOCK)                   EL681
00406              LENGTH   (PI-COMM-LENGTH)                            EL681
00407          END-EXEC                                                    CL**3
00408        ELSE                                                       EL681
00409          EXEC CICS  RETURN                                           CL**3
00410          END-EXEC.                                                   CL**3
00411                                                                   EL681
00412  8999-EXIT.                                                          CL**3
00413       EXIT.                                                          CL**3
00414                                                                   EL681
00415  9999-FINALIZE.                                                   EL681
00416      MOVE SPACES TO WS-PASSED-DATA.                               EL681
CIDMOD*    MOVE 'L'    TO DRS-SW.                                            000
00417      MOVE 'X'    TO WS-PROG-END.                                  EL681
00418      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL681
00419      GO TO 8999-RETURN-CICS.                                      EL681
00420                                                                   EL681
00421  9999-EXIT.                                                       EL681
