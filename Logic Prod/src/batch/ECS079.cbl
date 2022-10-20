00001  IDENTIFICATION DIVISION.                                         04/17/98
00002                                                                   ECS079
00003  PROGRAM-ID.                ECS079.                                  LV003
00004 *              PROGRAM CONVERTED BY                               ECS079
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS079
00006 *              CONVERSION DATE 02/08/96 18:38:47.                 ECS079
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS079
00008 *                           VMOD=2.005                            ECS079
00009 *AUTHOR.     LOGIC, INC.                                          ECS079
00010 *            DALLAS, TEXAS.                                       ECS079
00011                                                                   ECS079
00012 *DATE-COMPILED.                                                   ECS079
00013                                                                   ECS079
00014 *SECURITY.   *****************************************************ECS079
00015 *            *                                                   *ECS079
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS079
00017 *            *                                                   *ECS079
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS079
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS079
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS079
00021 *            *                                                   *ECS079
00022 *            *****************************************************ECS079
00023                                                                   ECS079
00024 *REMARKS.                                                         ECS079
00025                                                                   ECS079
00026 *REMARKS.                                                         ECS079
00027 *            THIS PROGRAM WILL PRINT 3 X 5 CARDS FROM THE COMMENT ECS079
00028 *            AREA OF THE ACCOUNT MASTER.                          ECS079
102004******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
022808* 022808  CR2007083100002  PEMA  ADD 'F' ACCT STATUS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
102004******************************************************************
00029  ENVIRONMENT DIVISION.                                            ECS079
00030  CONFIGURATION SECTION.                                           ECS079
00031  SPECIAL-NAMES.                                                   ECS079
00032      C02 IS LCP-CH2                                               ECS079
00033      C03 IS LCP-CH3                                               ECS079
00034      C04 IS LCP-CH4                                               ECS079
00035      C05 IS LCP-CH5                                               ECS079
00036      C06 IS LCP-CH6                                               ECS079
00037      C07 IS LCP-CH7                                               ECS079
00038      C08 IS LCP-CH8                                               ECS079
00039      C09 IS LCP-CH9                                               ECS079
00040      C10 IS LCP-CH10                                              ECS079
00041      C11 IS LCP-CH11                                              ECS079
00042      C12 IS LCP-CH12                                              ECS079
00043      S01 IS LCP-P01                                               ECS079
00044      S02 IS LCP-P02.                                              ECS079
00045  INPUT-OUTPUT SECTION.                                            ECS079
00046  FILE-CONTROL.                                                    ECS079
00047                                                                   ECS079
00048      SELECT ERACCTT    ASSIGN TO SYS010-FBA1-ERACCTT              ECS079
00049              ORGANIZATION IS INDEXED                              ECS079
00050              ACCESS IS SEQUENTIAL                                 ECS079
00051              RECORD KEY IS AM-CONTROL-PRIMARY                     ECS079
00052              FILE STATUS IS ERACCTT-FILE-STATUS.                  ECS079
00053      SELECT ACCT-PRT   ASSIGN TO SYS008-UR-1403-S-SYS008.         ECS079
00054      SELECT DISK-DATE  ASSIGN TO SYS019-UT-FBA1-S-SYS019.         ECS079
00055                                                                   ECS079
00056      EJECT                                                        ECS079
00057  DATA DIVISION.                                                   ECS079
00058  FILE SECTION.                                                    ECS079
00059                                                                   ECS079
00060  FD  ERACCTT.                                                     ECS079
00061                                                                   ECS079
00062      COPY ERCACCT.                                                ECS079
00063                                                                   ECS079
00064      EJECT                                                        ECS079
00065  FD  ACCT-PRT                     COPY ELCPRTFD.                  ECS079
00066                                                                   ECS079
00067      EJECT                                                        ECS079
00068  FD  DISK-DATE                    COPY ELCDTEFD.                  ECS079
00069                                                                   ECS079
00070      EJECT                                                        ECS079
00071  WORKING-STORAGE SECTION.                                         ECS079
00072  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS079
00073  77  LCP-ASA                       PIC X.                         ECS079
00074  77  FILLER  PIC X(32) VALUE '********************************'.  ECS079
00075  77  FILLER  PIC X(32) VALUE '     ECS079 WORKING-STORAGE     '.  ECS079
00076  77  FILLER  PIC X(32) VALUE '********** VMOD=2.005 **********'.  ECS079
00077                                                                   ECS079
00078  77  SUB                     PIC S999    COMP    VALUE +1.        ECS079
00079  77  SUBA                    PIC S999    COMP    VALUE +1.        ECS079
00080  77  X                       PIC X.                               ECS079
00081  77  PGM-SUB                 PIC S999    COMP-3  VALUE +079.      ECS079
00082  77  CTR                     PIC X       VALUE '1'.               ECS079
00083  77  WS-LINEUP-SW            PIC X       VALUE SPACE.             ECS079
00084  01  WS.                                                          ECS079
00085      12  ABEND-CODE          PIC X(4)    VALUE ZEROS.             ECS079
00086      12  ABEND-OPTION        PIC X       VALUE 'Y'.               ECS079
00087      12  WS-ABEND-FILE-STATUS                                     ECS079
00088                              PIC XX      VALUE ZEROS.             ECS079
00089      12  WS-ABEND-MESSAGE    PIC X(80).                           ECS079
00090      12  ERACCTT-FILE-STATUS PIC XX      VALUE ZEROS.             ECS079
00091      12  WS-RETURN-CODE      PIC X       VALUE SPACE.             ECS079
00092      12  WS-ZERO             PIC S9      VALUE ZERO COMP-3.       ECS079
00093  01  WS-ACCT.                                                     ECS079
00094      12  WS-AGT-A            PIC X.                               ECS079
00095      12  WS-AGT-B            PIC X(4).                            ECS079
00096      12  WS-AGT-C            PIC X.                               ECS079
00097                                                                   ECS079
00098      COPY ELCDTECX.                                               ECS079
00099                                                                   ECS079
00100      COPY ELCDTEVR.                                               ECS079
00101                                                                   ECS079
00102  01  DTL-LINES-FILL.                                              ECS079
00103      12  DTL-0.                                                   ECS079
00104          16  FILLER          PIC X(51)       VALUE SPACES.        ECS079
00105      12  DTL-1.                                                   ECS079
00106          16  FILLER          PIC X           VALUE ' '.           ECS079
00107          16  FILLER          PIC X           VALUE SPACES.        ECS079
00108          16  D1-NAME         PIC X(30).                           ECS079
00109          16  FILLER          PIC X           VALUE SPACES.        ECS079
00110          16  D1-STATUS       PIC X(5).                            ECS079
00111          16  FILLER          PIC X           VALUE SPACES.        ECS079
00112          16  D1-AGT          PIC X(10).                           ECS079
00113          16  FILLER          PIC XX          VALUE SPACES.        ECS079
00114      12  DTL-2.                                                   ECS079
00115          16  FILLER          PIC X           VALUE ' '.           ECS079
00116          16  FILLER          PIC X           VALUE SPACES.        ECS079
00117          16  D2-ADDRESS-1    PIC X(20).                           ECS079
00118          16  FILLER          PIC XX          VALUE SPACES.        ECS079
00119          16  D2-COVERAGE     PIC X(11).                           ECS079
00120          16  FILLER          PIC X(7)        VALUE '   ST. '.     ECS079
00121          16  D2-STATE        PIC XX.                              ECS079
00122          16  FILLER          PIC X(4)        VALUE ' TC '.        ECS079
00123          16  D2-CLS          PIC XX.                              ECS079
00124          16  FILLER          PIC X           VALUE SPACES.        ECS079
00125      12  DTL-3.                                                   ECS079
00126          16  FILLER          PIC X           VALUE ' '.           ECS079
00127          16  FILLER          PIC X           VALUE SPACES.        ECS079
00128          16  D3-ADDRESS-2    PIC X(20).                           ECS079
00129          16  FILLER          PIC X(11)       VALUE '  BIZ TYPE '. ECS079
00130          16  D3-BIZ-TYPE     PIC XX.                              ECS079
00131          16  FILLER          PIC X(7)        VALUE '   EFF '.     ECS079
00132          16  D3-EFF-MO       PIC XX.                              ECS079
00133          16  FILLER          PIC X           VALUE '-'.           ECS079
00134          16  D3-EFF-DA       PIC XX.                              ECS079
00135          16  FILLER          PIC X           VALUE '-'.           ECS079
00136          16  D3-EFF-YR       PIC XX.                              ECS079
00137          16  FILLER          PIC X           VALUE SPACES.        ECS079
00138      12  DTL-4.                                                   ECS079
00139          16  FILLER          PIC X           VALUE ' '.           ECS079
00140          16  FILLER          PIC X           VALUE SPACES.        ECS079
00141          16  D4-ADDRESS-3    PIC X(20).                           ECS079
00142          16  FILLER          PIC X(8)        VALUE ' PM DEV '.    ECS079
00143          16  D4-DEV-LF       PIC X(3).                            ECS079
00144          16  FILLER          PIC X           VALUE SPACES.        ECS079
00145          16  D4-DEV-AH       PIC X(3).                            ECS079
00146          16  FILLER          PIC X(5)        VALUE ' EXP '.       ECS079
00147          16  D3-EXP-MO       PIC XX.                              ECS079
00148          16  FILLER          PIC X           VALUE '-'.           ECS079
00149          16  D3-EXP-DA       PIC XX.                              ECS079
00150          16  FILLER          PIC X           VALUE '-'.           ECS079
00151          16  D3-EXP-YR       PIC XX.                              ECS079
00152          16  FILLER          PIC X           VALUE SPACES.        ECS079
00153      12  DTL-5.                                                   ECS079
00154          16  FILLER          PIC X           VALUE ' '.           ECS079
00155          16  FILLER          PIC X           VALUE SPACES.        ECS079
00156          16  D5-AMER-ZIP.                                         ECS079
00157              24  D5-ZIP1         PIC X(5).                        ECS079
00158              24  D5-DASH-AMER    PIC X.                           ECS079
00159              24  D5-ZIP2         PIC X(4).                        ECS079
00160          16  D5-CANADIAN-POSTAL-CODE REDEFINES D5-AMER-ZIP.       ECS079
00161              24  D5-CAN-POSTAL-CODE-1                             ECS079
00162                                  PIC X(3).                        ECS079
00163              24  D5-DASH-CAN     PIC X.                           ECS079
00164              24  D5-CAN-POSTAL-CODE-2                             ECS079
00165                                  PIC X(3).                        ECS079
00166              24  D5-CAN-FILLER   PIC X(3).                        ECS079
00167          16  FILLER          PIC XX          VALUE SPACES.        ECS079
00168          16  D5-TEL-LOC      PIC XXX.                             ECS079
00169          16  FILLER          PIC X(4)        VALUE ' PH '.        ECS079
00170          16  D5-TELEPHONE.                                        ECS079
00171              20  D5-LP       PIC X.                               ECS079
00172              20  D5-AREA-CDE PIC XXX.                             ECS079
00173              20  D5-RP       PIC X.                               ECS079
00174              20  FILLER      PIC X.                               ECS079
00175              20  D5-TEL-PRE  PIC XXX.                             ECS079
00176              20  D5-DASH     PIC X.                               ECS079
00177              20  D5-TEL-NBR  PIC XXXX.                            ECS079
00178          16  FILLER          PIC X(4)        VALUE ' CC '.        ECS079
00179          16  D5-CARRIER      PIC X.                               ECS079
00180          16  FILLER          PIC X(4)        VALUE ' RT '.        ECS079
00181          16  D5-REIN.                                             ECS079
00182              20  FILLER      PIC X(6).                            ECS079
00183          16  FILLER          PIC X           VALUE SPACES.        ECS079
00184      12  DTL-6.                                                   ECS079
00185          16  FILLER          PIC X           VALUE '0'.           ECS079
00186          16  FILLER          PIC X(50)       VALUE                ECS079
00187              ' * * * * * *  COMMISSION STRUCTURE  * * * * * * * '.ECS079
00188      12  DTL-7.                                                   ECS079
00189          16  FILLER          PIC X           VALUE ' '.           ECS079
00190          16  FILLER          PIC X(50)       VALUE                ECS079
00191              '  LEVEL        NO.    S.L.    J.L.    DISAB    '.   ECS079
00192      12  DTL-8               OCCURS 5.                            ECS079
00193          16  D8-CTL          PIC X.                               ECS079
00194          16  FILLER          PIC X.                               ECS079
00195          16  D8-LEVEL        PIC 9.                               ECS079
00196          16  FILLER          PIC X.                               ECS079
00197          16  D8-DESC         PIC X(10).                           ECS079
00198          16  FILLER          PIC X.                               ECS079
00199          16  D8-AGENT        PIC X(6).                            ECS079
00200          16  FILLER          PIC X.                               ECS079
00201          16  D8-SL           PIC ZZ.999.                          ECS079
00202          16  D8-SLR          REDEFINES D8-SL.                     ECS079
00203              20  D8-SL-R     PIC XXX.                             ECS079
00204              20  FILLER      PIC XXX.                             ECS079
00205          16  D8-PCT-1        PIC X.                               ECS079
00206          16  FILLER          PIC X.                               ECS079
00207          16  D8-JL           PIC ZZ.999.                          ECS079
00208          16  D8-JLR          REDEFINES D8-JL.                     ECS079
00209              20  D8-JL-R     PIC XXX.                             ECS079
00210              20  FILLER      PIC XXX.                             ECS079
00211          16  D8-PCT-2        PIC X.                               ECS079
00212          16  FILLER          PIC X.                               ECS079
00213          16  D8-DISAB        PIC ZZ.999.                          ECS079
00214          16  D8-DISABR       REDEFINES D8-DISAB.                  ECS079
00215              20  D8-DISAB-R  PIC XXX.                             ECS079
00216              20  FILLER      PIC XXX.                             ECS079
00217          16  D8-PCT-3        PIC X.                               ECS079
00218          16  FILLER          PIC X(6).                            ECS079
00219      12  DTL-9.                                                   ECS079
00220          16  FILLER          PIC X           VALUE '0'.           ECS079
00221          16  FILLER          PIC X(50)       VALUE                ECS079
00222              ' * * * * * *  COMMENTS AND REMARKS  * * * * * * * '.ECS079
00223      12  DTL-10              OCCURS 5.                            ECS079
00224          16  D10-CTL         PIC X.                               ECS079
00225          16  D10-COMMENT     PIC X(50).                           ECS079
00226      12  DTL-11.                                                  ECS079
00227          16  FILLER          PIC X           VALUE ' '.           ECS079
00228          16  FILLER          PIC X           VALUE ' '.           ECS079
00229          16  FILLER          PIC X(7)        VALUE                ECS079
00230              'CONTROL'.                                           ECS079
00231          16  FILLER          PIC X           VALUE ' '.           ECS079
00232          16  D11-CARR        PIC X.                               ECS079
00233          16  FILLER          PIC X           VALUE '-'.           ECS079
00234          16  D11-COMP        PIC X(6).                            ECS079
00235          16  FILLER          PIC X(5)        VALUE '    -'.       ECS079
00236          16  D11-STATE       PIC XX.                              ECS079
00237          16  FILLER          PIC X           VALUE '-'.           ECS079
00238          16  D11-ACCT        PIC X(10).                           ECS079
00239          16  FILLER          PIC X(6)        VALUE '     -'.      ECS079
00240          16  D11-EXP         PIC X(6).                            ECS079
00241          16  FILLER          PIC X(3)        VALUE '   '.         ECS079
00242      12  DTL-12.                                                  ECS079
00243          16  FILLER          PIC X           VALUE ' '.           ECS079
00244          16  FILLER          PIC X(50)       VALUE SPACES.        ECS079
00245      12  DTL-FILL-1.                                              ECS079
00246          16  FILLER          PIC X(51)       VALUE SPACES.        ECS079
00247      12  DTL-FILL-2.                                              ECS079
00248          16  FILLER          PIC X(51)       VALUE SPACES.        ECS079
00249      12  DTL-FILL-3.                                              ECS079
00250          16  FILLER          PIC X(51)       VALUE SPACES.        ECS079
00251  01  DTL-LINES               REDEFINES DTL-LINES-FILL.            ECS079
00252      12  DTL-LINE            OCCURS 24.                           ECS079
00253          16  FILLER          PIC X.                               ECS079
00254          16  D-LINE          PIC X(50).                           ECS079
00255  01  P-LINES.                                                     ECS079
00256      12  PRT-LINES           OCCURS 24.                           ECS079
00257          16  PRT-LINE        PIC X(51).                           ECS079
00258          16  P-LINE          PIC X(50).                           ECS079
00259      EJECT                                                        ECS079
00260   COPY ELCACCTV.                                                  ECS079
00261      EJECT                                                        ECS079
00262  PROCEDURE DIVISION.                                              ECS079
00263                                                                   ECS079
00264  OPEN-INIT.                                                       ECS079
00265                              COPY ELCDTERX.                       ECS079
00266      EJECT                                                        ECS079
00267      OPEN INPUT  ERACCTT                                          ECS079
00268           OUTPUT ACCT-PRT.                                        ECS079
00269                                                                   ECS079
00270      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       ECS079
00271          NEXT SENTENCE                                            ECS079
00272        ELSE                                                       ECS079
00273          MOVE ERACCTT-FILE-STATUS                                 ECS079
00274                                  TO WS-ABEND-FILE-STATUS          ECS079
00275          MOVE ' ERACCTT OPEN ERROR- '                             ECS079
00276                                  TO WS-ABEND-MESSAGE              ECS079
00277          GO TO ABEND-PGM.                                         ECS079
00278                                                                   ECS079
00279      MOVE +1                     TO SUB.                          ECS079
00280      PERFORM BLANK-PRINT 24 TIMES.                                ECS079
00281                                                                   ECS079
00282      EJECT                                                        ECS079
00283  ALIGN-ROUTINE.                                                   ECS079
00284      MOVE '  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'    ECS079
00285                                  TO PRT-LINE (2) PRT-LINE (3)     ECS079
00286                                     PRT-LINE (4) PRT-LINE (5).    ECS079
00287      MOVE '0 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'    ECS079
00288                                  TO PRT-LINE (8) PRT-LINE (14).   ECS079
00289      MOVE '  XXXXXXXXXXXXXXX   XXXXXXXXXXXX   XXXXXXXXXXXXXXX'    ECS079
00290                                  TO PRT-LINE (20) PRT-LINE (21).  ECS079
00291      PERFORM PRINT-CARDS THRU P-C-EXIT.                           ECS079
00292      ADD +1 TO SUBA.                                              ECS079
00293      IF SUBA LESS THAN +5                                         ECS079
00294          GO TO ALIGN-ROUTINE.                                     ECS079
00295                                                                   ECS079
00296      EJECT                                                        ECS079
00297  READ-ACCT.                                                       ECS079
00298      READ ERACCTT.                                                ECS079
00299                                                                   ECS079
00300      IF ERACCTT-FILE-STATUS = '10'                                ECS079
00301          GO TO E-O-J.                                             ECS079
00302                                                                   ECS079
00303      IF ERACCTT-FILE-STATUS NOT = ZERO                            ECS079
00304          MOVE ERACCTT-FILE-STATUS                                 ECS079
00305                                  TO WS-ABEND-FILE-STATUS          ECS079
00306          MOVE ' ERACCTT READ ERROR- '                             ECS079
00307                                  TO WS-ABEND-MESSAGE              ECS079
00308          GO TO ABEND-PGM.                                         ECS079
00309                                                                   ECS079
00310      COPY ELCACCTI.                                               ECS079
00311                                                                   ECS079
00312      MOVE AM-NAME                TO D1-NAME.                      ECS079
00313                                                                   ECS079
102004
102004     EVALUATE AM-STATUS
102004        WHEN '0'
102004           MOVE 'ACT  '          TO D1-STATUS
102004        WHEN '1'
102004           MOVE 'INACT'          TO D1-STATUS
102004        WHEN '2'
102004           MOVE 'XFER '          TO D1-STATUS
102004        WHEN '3'
102004           MOVE 'CANCL'          TO D1-STATUS
022808        WHEN '4'
022808           MOVE 'INACT'          TO D1-STATUS
031811        WHEN '5'
031811           MOVE 'SUSPD'          TO D1-STATUS
021916        WHEN '6'
021916           MOVE 'DROPD'          TO D1-STATUS
021916        WHEN '7'
021916           MOVE 'LPSED'          TO D1-STATUS
021916        WHEN '8'
021916           MOVE 'RUNOF'          TO D1-STATUS
021916        WHEN '9'
021916           MOVE 'PENDG'          TO D1-STATUS
102004        WHEN OTHER
102004           MOVE 'UNKNW'          TO D1-STATUS
102004     END-EVALUATE
102004
00314 *    IF AM-STATUS = '0'                                           ECS079
00315 *        MOVE 'ACT  '            TO D1-STATUS                     ECS079
00316 *    ELSE                                                         ECS079
00317 *        MOVE 'INACT'            TO D1-STATUS.                    ECS079

00341 *    IF AM-EXPIRE-DT NOT = 99999999999  AND  00099999999             CL**3
00342 *        MOVE 'EXP  '            TO D1-STATUS.                    ECS079
00343                                                                   ECS079
00319      MOVE AM-ACCOUNT             TO D1-AGT.                       ECS079
00320      MOVE AM-PERSON              TO D2-ADDRESS-1.                 ECS079
00321                                                                   ECS079
00322      IF AM-IG = '1'                                               ECS079
00323          MOVE 'INDIVIDUAL'       TO D2-COVERAGE                   ECS079
00324      ELSE                                                         ECS079
00325          MOVE 'GROUP'            TO D2-COVERAGE.                  ECS079
00326                                                                   ECS079
00327      MOVE AM-STATE               TO D2-STATE.                     ECS079
00328      MOVE AM-CAL-TABLE           TO D2-CLS.                       ECS079
00329      MOVE AM-ADDRS               TO D3-ADDRESS-2.                 ECS079
00330      MOVE AM-GPCD                TO D3-BIZ-TYPE.                  ECS079
00331      MOVE AM-EFF-MO              TO D3-EFF-MO.                    ECS079
00332      MOVE AM-EFF-DA              TO D3-EFF-DA.                    ECS079
00333      MOVE AM-EFF-YR              TO D3-EFF-YR.                    ECS079
00334      MOVE AM-CITY                TO D4-ADDRESS-3.                 ECS079
00335      MOVE AM-LF-DEVIATION        TO D4-DEV-LF.                    ECS079
00336      MOVE AM-AH-DEVIATION        TO D4-DEV-AH.                    ECS079
00337      MOVE AM-EXP-MO              TO D3-EXP-MO.                    ECS079
00338      MOVE AM-EXP-DA              TO D3-EXP-DA.                    ECS079
00339      MOVE AM-EXP-YR              TO D3-EXP-YR.                    ECS079
00340                                                                   ECS079
00341      IF AM-EXPIRE-DT NOT = 99999999999  AND  00099999999             CL**3
00342          MOVE 'EXP  '            TO D1-STATUS.                    ECS079
00343                                                                   ECS079
00344      IF  AM-CANADIAN-POST-CODE                                    ECS079
00345          MOVE AM-CAN-POSTAL-1    TO D5-CAN-POSTAL-CODE-1          ECS079
00346          MOVE AM-CAN-POSTAL-2    TO D5-CAN-POSTAL-CODE-2          ECS079
00347          MOVE SPACES             TO D5-DASH-CAN                   ECS079
00348          MOVE SPACES             TO D5-CAN-FILLER                 ECS079
00349      ELSE                                                         ECS079
00350          MOVE AM-ZIP             TO D5-ZIP1                       ECS079
00351          MOVE SPACES             TO D5-DASH-AMER                  ECS079
00352          IF  AM-ZIP-PLUS4 = SPACES OR ZEROS                       ECS079
00353              MOVE SPACES         TO D5-ZIP2                       ECS079
00354          ELSE                                                     ECS079
00355              MOVE AM-ZIP-PLUS4   TO D5-ZIP2.                      ECS079
00356                                                                   ECS079
00357      IF AM-TEL-LOC = 'O'                                          ECS079
00358          MOVE 'OFC'              TO D5-TEL-LOC                    ECS079
00359      ELSE                                                         ECS079
00360          IF AM-TEL-LOC = 'R'                                      ECS079
00361              MOVE 'RES'          TO D5-TEL-LOC                    ECS079
00362          ELSE                                                     ECS079
00363              MOVE SPACES         TO D5-TEL-LOC.                   ECS079
00364                                                                      CL**2
00365      MOVE SPACES                 TO D5-TELEPHONE.                 ECS079
00366                                                                   ECS079
00367      IF AM-AREA-CODE NOT = ZEROS                                  ECS079
00368          MOVE '('                TO D5-LP                         ECS079
00369          MOVE ')'                TO D5-RP                         ECS079
00370          MOVE AM-AREA-CODE       TO D5-AREA-CDE.                  ECS079
00371                                                                   ECS079
00372      IF AM-TEL-PRE NOT = ZEROS                                    ECS079
00373          OR AM-TEL-NBR NOT = ZEROS                                ECS079
00374              MOVE AM-TEL-PRE     TO D5-TEL-PRE                    ECS079
00375              MOVE AM-TEL-NBR     TO D5-TEL-NBR                    ECS079
00376              MOVE '-'            TO D5-DASH.                      ECS079
00377                                                                   ECS079
00378      MOVE AM-CARRIER             TO D5-CARRIER.                   ECS079
00379      MOVE AM-REI-TABLE           TO D5-REIN.                      ECS079
00380      MOVE SPACES                 TO DTL-8 (1) DTL-8 (2) DTL-8 (3) ECS079
00381                                     DTL-8 (4) DTL-8 (5).          ECS079
00382      MOVE +1                     TO SUB                           ECS079
00383                                     SUBA.                         ECS079
00384      PERFORM FILL-D8 THRU F-D-EXIT                                ECS079
00385         VARYING SUB FROM 1 BY 1 UNTIL (SUB GREATER +10)           ECS079
00386                         OR (SUBA GREATER +5).                     ECS079
00387      MOVE +1                     TO SUB.                          ECS079
00388      PERFORM FILL-DTL-10 5 TIMES.                                 ECS079
00389      MOVE AM-CARRIER             TO D11-CARR.                     ECS079
00390      MOVE AM-GROUPING            TO D11-COMP.                     ECS079
00391      MOVE AM-STATE               TO D11-STATE.                    ECS079
00392      MOVE AM-ACCOUNT             TO D11-ACCT.                     ECS079
00393      MOVE AM-EXPIRE-DT           TO D11-EXP.                      ECS079
00394      MOVE +1                     TO SUB.                          ECS079
00395                                                                   ECS079
00396  SAVE-PRT-LOOP.                                                   ECS079
00397      IF CTR = '1'                                                 ECS079
00398          MOVE DTL-LINE (SUB)     TO PRT-LINE (SUB)                ECS079
00399      ELSE                                                         ECS079
00400          MOVE D-LINE (SUB)       TO P-LINE (SUB).                 ECS079
00401                                                                   ECS079
00402      ADD +1 TO SUB.                                               ECS079
00403                                                                   ECS079
00404      IF SUB LESS +25                                              ECS079
00405          GO TO SAVE-PRT-LOOP.                                     ECS079
00406                                                                   ECS079
00407      IF CTR = '1'                                                 ECS079
00408          MOVE '2'                TO CTR                           ECS079
00409      ELSE                                                         ECS079
00410          PERFORM PRINT-CARDS THRU P-C-EXIT.                       ECS079
00411                                                                   ECS079
00412      GO TO READ-ACCT.                                             ECS079
00413                                                                   ECS079
00414      EJECT                                                        ECS079
00415  PRINT-CARDS.                                                     ECS079
00416      MOVE +1                     TO SUB.                          ECS079
00417                                                                   ECS079
00418  PRINT-LOOP.                                                      ECS079
00419      MOVE PRT-LINES (SUB)        TO PRT.                          ECS079
00420      MOVE P-CTL                  TO X.                            ECS079
00421      PERFORM PRT-RTN.                                             ECS079
00422      ADD +1 TO SUB.                                               ECS079
00423                                                                   ECS079
00424      IF SUB LESS +23                                              ECS079
00425          GO TO PRINT-LOOP.                                        ECS079
00426                                                                   ECS079
00427      MOVE +1                     TO SUB.                          ECS079
00428      PERFORM BLANK-PRINT 24 TIMES.                                ECS079
00429      MOVE '1'                    TO CTR.                          ECS079
00430                                                                   ECS079
00431  P-C-EXIT.                                                        ECS079
00432      EXIT.                                                        ECS079
00433                                                                   ECS079
00434      EJECT                                                        ECS079
00435  BLANK-PRINT.                                                     ECS079
00436      MOVE SPACES                 TO PRT-LINES (SUB).              ECS079
00437      ADD +1 TO SUB.                                               ECS079
00438                                                                   ECS079
00439  FILL-D8.                                                         ECS079
00440      IF AM-AGT (SUB) = (SPACES OR ZEROS)                          ECS079
00441              GO TO F-D-EXIT.                                      ECS079
00442      MOVE ' '                    TO D8-CTL (SUBA).                ECS079
00443      MOVE SUB                    TO D8-LEVEL (SUBA).              ECS079
00444                                                                   ECS079
00445      IF AM-COM-TYP (SUB) = 'C'                                    ECS079
00446          MOVE 'AGENT ONLY'       TO D8-DESC (SUBA).               ECS079
00447                                                                   ECS079
00448      IF AM-COM-TYP (SUB) = 'R'                                    ECS079
00449          MOVE 'AGT   REIN'       TO D8-DESC (SUBA).               ECS079
00450                                                                   ECS079
00451      IF AM-COM-TYP (SUB) = 'D'                                    ECS079
00452          MOVE 'AGT + REI '       TO D8-DESC (SUBA).               ECS079
00453                                                                   ECS079
00454      IF AM-COM-TYP (SUB) = 'O'                                    ECS079
00455          MOVE 'G.A. ONLY '       TO D8-DESC (SUBA).               ECS079
00456                                                                   ECS079
00457      IF AM-COM-TYP (SUB) = 'T'                                    ECS079
00458          MOVE 'GA FOR REI'       TO D8-DESC (SUBA).               ECS079
00459                                                                   ECS079
00460      IF AM-COM-TYP (SUB) = 'P'                                    ECS079
00461          MOVE 'GA + REI  '       TO D8-DESC (SUBA).               ECS079
00462                                                                   ECS079
00463      IF AM-COM-TYP (SUB) = 'W'                                    ECS079
00464          MOVE 'SPCL REIN '       TO D8-DESC (SUBA).               ECS079
00465                                                                   ECS079
00466      MOVE AM-AGT-PRIME (SUB)     TO D8-AGENT (SUBA).              ECS079
00467                                                                   ECS079
00468      IF AM-L-COM (SUB) NUMERIC                                    ECS079
00469          MULTIPLY AM-L-COM (SUB) BY +100 GIVING D8-SL (SUBA)      ECS079
00470          MOVE '%'                TO D8-PCT-1 (SUBA)               ECS079
00471      ELSE                                                         ECS079
00472          MOVE AM-L-COMA (SUB)    TO D8-SL-R (SUBA).               ECS079
00473                                                                   ECS079
00474      IF AM-J-COM (SUB) NUMERIC                                    ECS079
00475          MULTIPLY AM-J-COM (SUB) BY +100 GIVING D8-JL (SUBA)      ECS079
00476          MOVE '%'                TO D8-PCT-2 (SUBA)               ECS079
00477      ELSE                                                         ECS079
00478          MOVE AM-J-COMA (SUB)    TO D8-JL-R (SUBA).               ECS079
00479                                                                   ECS079
00480      IF AM-A-COM (SUB) NUMERIC                                    ECS079
00481          MULTIPLY AM-A-COM (SUB) BY +100 GIVING D8-DISAB (SUBA)   ECS079
00482          MOVE '%'                TO D8-PCT-3 (SUBA)               ECS079
00483      ELSE                                                         ECS079
00484          MOVE AM-A-COMA (SUB)    TO D8-DISAB-R (SUBA).            ECS079
00485                                                                   ECS079
00486      ADD +1 TO SUBA.                                              ECS079
00487                                                                   ECS079
00488  F-D-EXIT.                                                        ECS079
00489      EXIT.                                                        ECS079
00490                                                                   ECS079
00491      EJECT                                                        ECS079
00492  FILL-DTL-10.                                                     ECS079
00493      MOVE AM-COMMENT-LINE (SUB)  TO D10-COMMENT (SUB).            ECS079
00494      MOVE ' '                    TO D10-CTL (SUB).                ECS079
00495      ADD +1 TO SUB.                                               ECS079
00496                                                                   ECS079
00497  PRT-RTN.                                                         ECS079
00498      MOVE X TO LCP-ASA                                            ECS079
00499      PERFORM LCP-WRITE-POS-PRT                                    ECS079
00500          THRU LCP-WRITE-END-PRT.                                  ECS079
00501                                                                   ECS079
00502  E-O-J.                                                           ECS079
00503      IF CTR = '2'                                                 ECS079
00504          PERFORM PRINT-CARDS THRU P-C-EXIT.                       ECS079
00505                                                                   ECS079
00506      CLOSE ERACCTT  ACCT-PRT.                                     ECS079
00507                                                                   ECS079
00508      GOBACK.                                                      ECS079
00509                                                                   ECS079
00510                                                                   ECS079
00511  ABEND-PGM SECTION.                                               ECS079
00512                                   COPY ELCABEND SUPPRESS.         ECS079
00513 /                                                                 ECS079
00514  LCP-WRITE-POS-PRT SECTION.                                       ECS079
00515      IF LCP-ASA = '+'                                             ECS079
00516          WRITE PRT AFTER 0 LINE                                   ECS079
00517      ELSE                                                         ECS079
00518      IF LCP-ASA = ' '                                             ECS079
00519          WRITE PRT AFTER ADVANCING 1 LINE                         ECS079
00520      ELSE                                                         ECS079
00521      IF LCP-ASA = '0'                                             ECS079
00522          WRITE PRT AFTER ADVANCING 2 LINE                         ECS079
00523      ELSE                                                         ECS079
00524      IF LCP-ASA = '-'                                             ECS079
00525          WRITE PRT AFTER ADVANCING 3 LINE                         ECS079
00526      ELSE                                                         ECS079
00527      IF LCP-ASA = '1'                                             ECS079
00528          WRITE PRT AFTER ADVANCING PAGE                           ECS079
00529      ELSE                                                         ECS079
00530      IF LCP-ASA = '2'                                             ECS079
00531          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS079
00532      ELSE                                                         ECS079
00533      IF LCP-ASA = '3'                                             ECS079
00534          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS079
00535      ELSE                                                         ECS079
00536      IF LCP-ASA = '4'                                             ECS079
00537          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS079
00538      ELSE                                                         ECS079
00539      IF LCP-ASA = '5'                                             ECS079
00540          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS079
00541      ELSE                                                         ECS079
00542      IF LCP-ASA = '6'                                             ECS079
00543          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS079
00544      ELSE                                                         ECS079
00545      IF LCP-ASA = '7'                                             ECS079
00546          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS079
00547      ELSE                                                         ECS079
00548      IF LCP-ASA = '8'                                             ECS079
00549          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS079
00550      ELSE                                                         ECS079
00551      IF LCP-ASA = '9'                                             ECS079
00552          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS079
00553      ELSE                                                         ECS079
00554      IF LCP-ASA = 'A'                                             ECS079
00555          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS079
00556      ELSE                                                         ECS079
00557      IF LCP-ASA = 'B'                                             ECS079
00558          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS079
00559      ELSE                                                         ECS079
00560      IF LCP-ASA = 'C'                                             ECS079
00561          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS079
00562      ELSE                                                         ECS079
00563      IF LCP-ASA = 'V'                                             ECS079
00564          WRITE PRT AFTER ADVANCING LCP-P01                        ECS079
00565      ELSE                                                         ECS079
00566      IF LCP-ASA = 'W'                                             ECS079
00567          WRITE PRT AFTER ADVANCING LCP-P02                        ECS079
00568      ELSE                                                         ECS079
00569      DISPLAY 'ASA CODE ERROR'.                                    ECS079
00570  LCP-WRITE-END-PRT.                                               ECS079
00571      EXIT.                                                        ECS079
00572                                                                   ECS079
