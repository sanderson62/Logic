00001  IDENTIFICATION DIVISION.                                         01/14/97
00002                                                                   EL105
00003  PROGRAM-ID.                 EL105 .                                 LV017
00004 *              PROGRAM CONVERTED BY                                  CL*13
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*13
00006 *              CONVERSION DATE 12/13/94 09:22:24.                    CL*13
00007 *                            VMOD=2.017                              CL*17
00008 *                                                                 EL105
00008 *                                                                 EL105
00009                                                                   EL105
00010 *AUTHOR.    LOGIC, INC.                                              CL*13
00011 *           DALLAS, TEXAS.                                           CL*13
00012                                                                   EL105
00013 *DATE-COMPILED.                                                      CL*13
00014                                                                   EL105
00015 *SECURITY.   *****************************************************   CL*13
00016 *            *                                                   *   CL*13
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*13
00018 *            *                                                   *   CL*13
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*13
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*13
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*13
00022 *            *                                                   *   CL*13
00023 *            *****************************************************   CL*13
00024                                                                   EL105
00025                                                                   EL105
00026 *REMARKS.    TRANSACTION - EX12                                      CL*10
00027                                                                   EL105
00028 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED      CL*10
00029 *    FOR THE CARRIER CONTROL RECORDS.                                CL*10
00030                                                                   EL105
00031 *    SCREENS     - EL105A - CARRIER MAINTENANCE                      CL*10
00032                                                                   EL105
00033 *    ENTERED BY  - EL101 OR EL601 - MAINTENANCE MENU                 CL*10
00034                                                                   EL105
00035 *    EXIT TO     - EL101 OR EL601 - MAINTENANCE MENU                 CL*10
00036                                                                   EL105
00037 *    INPUT FILE  - ELCNTL - CONTROL FILE - CARRIER RECORDS           CL*10
00038                                                                   EL105
00039 *    OUTPUT FILE - ELCNTL - CONTROL FILE - CARRIER RECORDS           CL*10
00040                                                                   EL105
00041 *    COMMAREA    - PASSED                                            CL*10
00042                                                                   EL105
00043 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101 OR EL601    CL*10
00044 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE    CL*10
00045 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE   CL*10
00046 *                  ENTRIES (XCTL FROM CICS VIA EX12) THE SCREEN      CL*10
00047 *                  WILL BE READ AND ACTION WILL BE BASED ON THE      CL*10
00048 *                  MAINTENANCE TYPE INDICATED.                       CL*10
112103******************************************************************
112103*                   C H A N G E   L O G
112103*
112103* Changes are marked by the Change Effective date.
112103*-----------------------------------------------------------------
112103*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
112103* EFFECTIVE    NUMBER
112103*-----------------------------------------------------------------
112103* 112103    2003080800002  SMVA  ADD CLP TOLERANCE % FOR SECURE PAY 
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
032813* 032813  CR2012051000001  AJRA  DISPLAY NEXT REAUDIT CHECK NUMBER
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
112103******************************************************************
00049                                                                   EL105
00050      EJECT                                                        EL105
00051  ENVIRONMENT DIVISION.                                            EL105
00052                                                                   EL105
00053  DATA DIVISION.                                                   EL105
00054                                                                   EL105
00055  WORKING-STORAGE SECTION.                                         EL105
00056  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*13
00057                                                                   EL105
00058  77  FILLER  PIC X(32)  VALUE '********************************'. EL105
00059  77  FILLER  PIC X(32)  VALUE '*    EL105 WORKING STORAGE     *'. EL105
00060  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.017 **********'.    CL*17
00061                                                                   EL105
00062                              COPY ELCSCTM.                           CL*10
00063                              COPY ELCSCRTY.                          CL*10
00064                                                                   EL105
00065  01  WS-DATE-AREA.                                                EL105
00066      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL105
00067      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL105
00068                                                                      CL**7
00069  01  WS-ZIP-CODE-AREA.                                               CL**7
00070      12  WS-ZIP-CODE.                                                CL**7
00071          16  WS-ZIP-1            PIC X.                              CL**7
00072              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.              CL**7
00073          16  WS-ZIP-2-3          PIC XX.                             CL**7
00074          16  WS-ZIP-4            PIC X.                              CL**7
00075          16  WS-ZIP-5            PIC X.                              CL**7
00076          16  WS-ZIP-6            PIC X.                              CL**7
00077          16  FILLER              PIC X(4).                           CL**7
00078      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                        CL**7
00079          16  WS-ZIP-AM-1-CODE    PIC X(5).                           CL**7
00080          16  WS-ZIP-AM-1-PLUS4   PIC X(4).                           CL**7
00081          16  FILLER              PIC X.                              CL**7
00082      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                        CL**7
00083          16  WS-ZIP-AM-2-CODE    PIC X(5).                           CL**7
00084          16  WS-ZIP-AM-2-DASH    PIC X.                              CL**7
00085          16  WS-ZIP-AM-2-PLUS4   PIC X(4).                           CL**7
00086      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                       CL**7
00087          16  WS-ZIP-CAN-1-POST1  PIC XXX.                            CL**7
00088          16  WS-ZIP-CAN-1-POST2  PIC XXX.                            CL**7
00089          16  FILLER              PIC X(4).                           CL**7
00090      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                       CL**7
00091          16  WS-ZIP-CAN-2-POST1  PIC XXX.                            CL**7
00092          16  FILLER              PIC X.                              CL**7
00093          16  WS-ZIP-CAN-2-POST2  PIC XXX.                            CL**7
00094          16  FILLER              PIC XXX.                            CL**7
00095                                                                      CL**7
00096      12  WS-ZIP-CODE-NUM         PIC 9(9).                           CL**7
00097                                                                   EL105
00098  01  FILLER                          COMP-3.                      EL105
00099      05  TIME-IN                     PIC S9(7)       VALUE ZERO.  EL105
00100      05  TIME-OUT                    REDEFINES                    EL105
00101          TIME-IN                     PIC S9(3)V9(4).              EL105
00102                                                                   EL105
00103      05  WS-EXPENSE-DOLLAR           PIC S9(3)V99 VALUE ZERO.     EL105
00104      05  WS-EXPENSE-PERCENT          PIC S9(3)V99 VALUE ZERO.     EL105
00105                                                                      CL**5
092705     05  WS-SPP-LEASE-COMM           PIC S9(5)V99    VALUE +0.
112103     05  WS-CLP-TOL-PCT              PIC S9(1)V9(4)  VALUE ZEROS.    CL**5
00106      05  WS-TOL-PREM-PCT             PIC S9(1)V9(4)  VALUE ZEROS.    CL**5
00107      05  WS-TOL-REF-PCT              PIC S9(1)V9(4)  VALUE ZEROS.    CL**5
00108      05  WS-CR-OVR-SHT-PCT           PIC S9(1)V9(4)  VALUE ZEROS.    CL*17
00109                                                                   EL105
00110      05  WS-IBNR-UEP-PCT             PIC S9(3)V9(4)  VALUE ZEROS.    CL*11
00111      05  WS-IBNR-R78-PCT             PIC S9(3)V9(4)  VALUE ZEROS.    CL*11
00112      05  WS-IBNR-PRO-PCT             PIC S9(3)V9(4)  VALUE ZEROS.    CL*11
00113                                                                      CL*11
00114  01  FILLER                          COMP  SYNC.                  EL105
00115      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.    EL105
00116      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.     CL**9
00117                                                                   EL105
00118      05  APHONE-LENGTH               PIC S9(4)       VALUE +12.   EL105
00119      05  AEXPCA-LENGTH               PIC S9(4)       VALUE +7.    EL105
00120      05  AEXPCP-LENGTH               PIC S9(4)       VALUE +7.    EL105
00121      05  ALQCA-LENGTH                PIC S9(4)       VALUE +7.    EL105
00122      05  ALMRP-LENGTH                PIC S9(4)       VALUE +13.   EL105
00123      05  ALQCD-LENGTH                PIC S9(4)       VALUE +4.    EL105
00124      05  ALMDPP-LENGTH               PIC S9(4)       VALUE +4.    EL105
00125      05  ALDBC-LENGTH                PIC S9(4)       VALUE +4.    EL105
00126      05  ALMAP-LENGTH                PIC S9(4)       VALUE +13.   EL105
00127      05  ALMBP-LENGTH                PIC S9(4)       VALUE +4.    EL105
00128      05  ALMAPM-LENGTH               PIC S9(4)       VALUE +4.    EL105
00129      05  APCTCDT-LENGTH              PIC S9(4)       VALUE +7.    EL105
00130      05  IBNRPCT-LENGTH              PIC S9(4)       VALUE +7.    EL105
00131      05  AUEPPCT-LENGTH              PIC S9(4)       VALUE +7.       CL*11
00132      05  AR78PCT-LENGTH              PIC S9(4)       VALUE +7.       CL*11
00133      05  APROPCT-LENGTH              PIC S9(4)       VALUE +7.       CL*11
00134                                                                   EL105
00135  01  FILLER.                                                      EL105
00136      05  XCTL-EL126                  PIC X(5)    VALUE 'EL126'.      CL**4
00137      05  XCTL-EL626                  PIC X(5)    VALUE 'EL626'.      CL**4
00138      05  XCTL-EM626                  PIC X(5)    VALUE 'EM626'.      CL**4
00139      05  XCTL-GL800                  PIC X(5)    VALUE 'GL800'.      CL**4
00140      05  GETMAIN-SPACE               PIC X  VALUE SPACE.          EL105
00141      05  WS-CONTROL-FILE-KEY.                                     EL105
00142          10  WS-CFK-COMPANY-ID       PIC X(3).                    EL105
00143          10  WS-CFK-RECORD-TYPE      PIC X.                       EL105
00144 *          88  LF-BENEFIT-MASTER                     VALUE '4'.   EL105
00145 *          88  AH-BENEFIT-MASTER                     VALUE '5'.   EL105
00146          10  FILLER                  PIC XXX.                     EL105
00147          10  WS-CFK-CARRIER-NO       PIC X.                       EL105
00148          10  WS-CFK-SEQUENCE-NO      PIC S9(4)  COMP.             EL105
00149                                                                   EL105
00150      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL105S'.EL105
00151      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL105A'.EL105
00152                                                                   EL105
00153      05  FILLER                      REDEFINES                    EL105
00154          WS-MAP-NAME.                                             EL105
00155          10  FILLER                  PIC XX.                      EL105
00156          10  WS-MAP-NUMBER           PIC X(4).                    EL105
00157          10  FILLER                  PIC XX.                         CL*13
00158                                                                   EL105
00159      05  THIS-PGM                    PIC X(8)      VALUE 'EL105'. EL105
00160                                                                   EL105
00161      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     EL105
00162                                                                   EL105
00163      05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.  EL105
00164                                                                   EL105
00165      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX12'.EL105
00166                                                                   EL105
00167      EJECT                                                        EL105
00168  01  ERROR-MESSAGES.                                              EL105
00169      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL105
00170      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL105
00171      12  ER-0006                 PIC X(4)  VALUE '0006'.          EL105
00172      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL105
00173      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL105
00174      12  ER-0042                 PIC X(4)  VALUE '0042'.          EL105
00175      12  ER-0050                 PIC X(4)  VALUE '0050'.          EL105
00176      12  ER-0052                 PIC X(4)  VALUE '0052'.          EL105
00177      12  ER-0053                 PIC X(4)  VALUE '0053'.          EL105
00178      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL105
00179      12  ER-0090                 PIC X(4)  VALUE '0090'.          EL105
00180      12  ER-0091                 PIC X(4)  VALUE '0091'.          EL105
00181      12  ER-0092                 PIC X(4)  VALUE '0092'.          EL105
00182      12  ER-0093                 PIC X(4)  VALUE '0093'.          EL105
00183      12  ER-0094                 PIC X(4)  VALUE '0094'.          EL105
00184      12  ER-0095                 PIC X(4)  VALUE '0095'.          EL105
00185      12  ER-0096                 PIC X(4)  VALUE '0096'.          EL105
00186      12  ER-0097                 PIC X(4)  VALUE '0097'.          EL105
00187      12  ER-0098                 PIC X(4)  VALUE '0098'.          EL105
00188      12  ER-0099                 PIC X(4)  VALUE '0099'.          EL105
00189      12  ER-0100                 PIC X(4)  VALUE '0100'.          EL105
00190      12  ER-0101                 PIC X(4)  VALUE '0101'.          EL105
00191      12  ER-0102                 PIC X(4)  VALUE '0102'.          EL105
00192      12  ER-0103                 PIC X(4)  VALUE '0103'.          EL105
00193      12  ER-0104                 PIC X(4)  VALUE '0104'.          EL105
00194      12  ER-0105                 PIC X(4)  VALUE '0105'.             CL**8
00195      12  ER-0106                 PIC X(4)  VALUE '0106'.          EL105
00196      12  ER-0107                 PIC X(4)  VALUE '0107'.          EL105
00197      12  ER-0108                 PIC X(4)  VALUE '0108'.          EL105
00198      12  ER-0109                 PIC X(4)  VALUE '0109'.          EL105
00199      12  ER-0110                 PIC X(4)  VALUE '0110'.          EL105
00200      12  ER-0111                 PIC X(4)  VALUE '0111'.          EL105
00201      12  ER-0112                 PIC X(4)  VALUE '0112'.          EL105
00202      12  ER-0113                 PIC X(4)  VALUE '0113'.          EL105
00203      12  ER-0114                 PIC X(4)  VALUE '0114'.          EL105
00204      12  ER-0117                 PIC X(4)  VALUE '0117'.          EL105
00205      12  ER-0118                 PIC X(4)  VALUE '0118'.          EL105
00206      12  ER-0119                 PIC X(4)  VALUE '0119'.          EL105
00207      12  ER-0120                 PIC X(4)  VALUE '0120'.          EL105
00208      12  ER-0121                 PIC X(4)  VALUE '0121'.          EL105
00209      12  ER-0122                 PIC X(4)  VALUE '0122'.          EL105
00210      12  ER-0123                 PIC X(4)  VALUE '0123'.          EL105
00211      12  ER-0124                 PIC X(4)  VALUE '0124'.          EL105
00212      12  ER-0125                 PIC X(4)  VALUE '0125'.          EL105
00213      12  ER-0145                 PIC X(4)  VALUE '0145'.             CL*12
00214      12  ER-0173                 PIC X(4)  VALUE '0173'.          EL105
00215      12  ER-0193                 PIC X(4)  VALUE '0193'.          EL105
00216      12  ER-0497                 PIC X(4)  VALUE '0497'.          EL105
00217      12  ER-0529                 PIC X(4)  VALUE '0529'.          EL105
00218      12  ER-0637                 PIC X(4)  VALUE '0637'.          EL105
00219      12  ER-0638                 PIC X(4)  VALUE '0638'.          EL105
112103     12  ER-1778                 PIC X(4)  VALUE '1778'.          EL105
00220      12  ER-2010                 PIC X(4)  VALUE '2010'.             CL**5
00221      12  ER-2014                 PIC X(4)  VALUE '2014'.             CL**5
00222      12  ER-2308                 PIC X(4)  VALUE '2308'.             CL*11
112103     12  ER-3270                 PIC X(4)  VALUE '3270'.             CL*11
00223      12  ER-7008                 PIC X(4)  VALUE '7008'.          EL105
00224      12  ER-7532                 PIC X(4)  VALUE '7532'.             CL**5
00225      12  ER-8017                 PIC X(4)  VALUE '8017'.             CL*13
00226      12  ER-8127                 PIC X(4)  VALUE '8127'.             CL*13
00227      12  ER-8128                 PIC X(4)  VALUE '8128'.             CL*13
00228                                                                   EL105
00229      EJECT                                                        EL105
00230                                      COPY ELCINTF.                   CL*10
00231      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL105
00232          16  PI-1ST-TIME-SW          PIC S9    COMP-3.            EL105
00233          16  PI-MODE                 PIC X.                       EL105
00234          16  PI-CARRIER-NUMBER       PIC X.                       EL105
00235          16  PI-NEXT-CARRIER-NUMBER  PIC X.                       EL105
00236          16  PI-LINE-COUNT           PIC S9(3) COMP-3.            EL105
00237          16  PI-BROWSE-SW            PIC S9    COMP-3.            EL105
00238          16  PI-END-OF-FILE          PIC S9    COMP-3.            EL105
00239          16  PI-PREV-MODE            PIC X.                          CL*12
00240          16  PI-PREV-CARRIER         PIC X.                          CL*12
00241          16  FILLER                  PIC X(630).                     CL*15
00242                                                                   EL105
00243      EJECT                                                        EL105
00244                                      COPY ELCJPFX.                   CL*10
00245                                      PIC X(750).                     CL**9
00246      EJECT                                                        EL105
00247                                      COPY ELCEMIB.                   CL*10
00248      EJECT                                                        EL105
00249                                      COPY ELCDATE.                   CL*10
00250      EJECT                                                        EL105
00251                                      COPY EL105S.                    CL*10
00252      EJECT                                                        EL105
00253                                      COPY ELCLOGOF.                  CL*10
00254      EJECT                                                        EL105
00255                                      COPY ELCATTR.                   CL*10
00256      EJECT                                                        EL105
00257                                      COPY ELCAID.                    CL*10
00258  01  FILLER  REDEFINES DFHAID.                                    EL105
00259      05  FILLER                      PIC X(8).                    EL105
00260      05  PF-VALUES                   PIC X   OCCURS 24.           EL105
00261                                                                   EL105
00262      EJECT                                                        EL105
00263  LINKAGE SECTION.                                                 EL105
00264  01  DFHCOMMAREA                     PIC X(1024).                 EL105
00265                                                                   EL105
00266      EJECT                                                        EL105
00267                                      COPY ELCCNTL.                   CL*10
00268      EJECT                                                        EL105
00269  PROCEDURE DIVISION.                                              EL105
00270                                                                   EL105
00271  0000-MAINLINE SECTION.                                              CL*13
00272                                                                   EL105
00273      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL105
00274      MOVE '5'                   TO DC-OPTION-CODE.                EL105
00275      PERFORM 8500-DATE-CONVERSION.                                EL105
00276      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL105
00277      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL105
00278                                                                   EL105
00279      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL105
00280                                                                      CL**5
00281      IF CREDIT-SESSION                                               CL**5
00282          MOVE 'EL105B  '  TO  WS-MAP-NAME                            CL**5
00283      ELSE                                                            CL**5
00284          MOVE 'EL105A  '  TO  WS-MAP-NAME.                           CL**5
00285                                                                   EL105
00286 *    NOTE ******************************************************* EL105
00287 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL105
00288 *         *  FROM ANOTHER MODULE.                               * EL105
00289 *         *******************************************************.EL105
00290                                                                   EL105
00291      IF EIBCALEN NOT GREATER ZERO                                 EL105
00292          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL105
00293          GO TO 8300-SEND-TEXT.                                    EL105
00294                                                                   EL105
00295      EXEC CICS HANDLE CONDITION                                   EL105
00296          PGMIDERR (9600-PGMIDERR)                                 EL105
00297          NOTOPEN  (8700-NOT-OPEN)                                 EL105
00298          DUPREC   (8800-DUPREC)                                   EL105
00299          ERROR    (9990-ERROR)                                    EL105
00300      END-EXEC.                                                    EL105
00301                                                                   EL105
00302      EJECT                                                        EL105
00303  0010-MAIN-LOGIC.                                                 EL105
00304      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL105
00305          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL105
00306              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL105
00307              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL105
00308              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL105
00309              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL105
00310              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL105
00311              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL105
00312              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL105
00313              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL105
00314            ELSE                                                   EL105
00315              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL105
00316              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL105
00317              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL105
00318              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL105
00319              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL105
00320              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL105
00321              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL105
00322              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL105
00323        ELSE                                                       EL105
00324          GO TO 0020-MAIN-LOGIC.                                   EL105
00325                                                                   EL105
00326 *    NOTE ******************************************************* EL105
00327 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL105
00328 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL105
00329 *         *******************************************************.EL105
00330      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA            CL*12
00331                                      PI-PREV-MODE                    CL*12
00332                                      PI-PREV-CARRIER.                CL*12
00333      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL105
00334                                      PI-LINE-COUNT                EL105
00335                                      PI-BROWSE-SW                 EL105
00336                                      PI-END-OF-FILE.              EL105
00337                                                                   EL105
00338 *    NOTE ******************************************************* EL105
00339 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   * EL105
00340 *         *  FOR EL105.                                         * EL105
00341 *         *******************************************************.EL105
00342      MOVE LOW-VALUES             TO  EL105AI.                     EL105
00343      MOVE -1                     TO  AMAINTL.                     EL105
00344      PERFORM 8100-SEND-INITIAL-MAP.                               EL105
00345                                                                   EL105
00346      GO TO 9100-RETURN-TRAN.                                      EL105
00347                                                                   EL105
00348      EJECT                                                        EL105
00349  0020-MAIN-LOGIC.                                                 EL105
00350 *    NOTE ******************************************************* EL105
00351 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL105
00352 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL105
00353 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL105
00354 *         *******************************************************.EL105
00355      IF EIBAID = DFHCLEAR                                         EL105
00356          GO TO 9400-CLEAR.                                        EL105
00357                                                                   EL105
00358      IF NOT SYSTEM-DISPLAY-CAP                                    EL105
00359          MOVE 'READ'         TO SM-READ                           EL105
00360          PERFORM 9995-SECURITY-VIOLATION                          EL105
00361          MOVE ER-0070        TO EMI-ERROR                         EL105
00362          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL105
00363          PERFORM 8100-SEND-INITIAL-MAP                            EL105
00364          GO TO 9100-RETURN-TRAN.                                  EL105
00365                                                                   EL105
00366      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL105
00367          MOVE LOW-VALUES         TO  EL105AI                      EL105
00368          MOVE ER-7008            TO  EMI-ERROR                    EL105
00369          IF CREDIT-SESSION                                           CL**5
00370              MOVE -1                 TO  BPFKL                       CL**5
00371              PERFORM 9900-ERROR-FORMAT                               CL**5
00372              PERFORM 8200-SEND-DATAONLY                              CL**5
00373              GO TO 9100-RETURN-TRAN                                  CL**5
00374          ELSE                                                        CL**5
00375              MOVE -1                 TO  APFKL                       CL**5
00376              PERFORM 9900-ERROR-FORMAT                               CL**5
00377              PERFORM 8200-SEND-DATAONLY                              CL**5
00378              GO TO 9100-RETURN-TRAN.                                 CL**5
00379                                                                   EL105
00380      EXEC CICS RECEIVE                                            EL105
00381          INTO   (EL105AI)                                         EL105
00382          MAPSET (WS-MAPSET-NAME)                                  EL105
00383          MAP    (WS-MAP-NAME)                                     EL105
00384      END-EXEC.                                                    EL105
00385                                                                   EL105
00386      IF NOT CREDIT-SESSION                                           CL**5
00387          IF APFKL GREATER ZERO                                       CL**5
00388             IF EIBAID NOT = DFHENTER                                 CL*13
00389                 MOVE ER-0004           TO  EMI-ERROR                 CL**5
00390                 MOVE AL-UNBON          TO  APFKA                     CL**5
00391                 MOVE -1                TO  APFKL                     CL**5
00392                 PERFORM 9900-ERROR-FORMAT                            CL**5
00393                 PERFORM 8200-SEND-DATAONLY                           CL**5
00394                 GO TO 9100-RETURN-TRAN                               CL**5
00395             ELSE                                                     CL**5
00396              IF APFKO GREATER ZERO AND LESS '25'                  EL105
00397                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL105
00398                ELSE                                               EL105
00399                  MOVE ER-0029            TO  EMI-ERROR            EL105
00400                  MOVE AL-UNBOF           TO  APFKA                EL105
00401                  MOVE -1                 TO  APFKL                EL105
00402                  PERFORM 9900-ERROR-FORMAT                        EL105
00403                  PERFORM 8200-SEND-DATAONLY                       EL105
00404                  GO TO 9100-RETURN-TRAN                           EL105
00405          ELSE                                                        CL**5
00406             MOVE AL-UNNOF            TO APFKA.                       CL**5
00407                                                                      CL**5
00408      IF CREDIT-SESSION                                               CL**5
00409          IF BPFKL GREATER ZERO                                       CL**5
00410             IF EIBAID NOT = DFHENTER                                 CL*13
00411                 MOVE ER-0004           TO  EMI-ERROR                 CL**5
00412                 MOVE AL-UNBON          TO  BPFKA                     CL**5
00413                 MOVE -1                TO  BPFKL                     CL**5
00414                 PERFORM 9900-ERROR-FORMAT                            CL**5
00415                 PERFORM 8200-SEND-DATAONLY                           CL**5
00416                 GO TO 9100-RETURN-TRAN                               CL**5
00417             ELSE                                                     CL**5
00418              IF BPFKO GREATER ZERO AND LESS '25'                     CL**5
00419                  MOVE PF-VALUES (BPFKI)  TO  EIBAID                  CL**5
00420                ELSE                                                  CL**5
00421                  MOVE ER-0029            TO  EMI-ERROR               CL**5
00422                  MOVE AL-UNBOF           TO  BPFKA                   CL**5
00423                  MOVE -1                 TO  BPFKL                   CL**5
00424                  PERFORM 9900-ERROR-FORMAT                           CL**5
00425                  PERFORM 8200-SEND-DATAONLY                          CL**5
00426                  GO TO 9100-RETURN-TRAN                              CL**5
00427          ELSE                                                        CL**5
00428             MOVE AL-UNNOF            TO BPFKA.                       CL**5
00429                                                                   EL105
00430      IF EIBAID = DFHPF12                                          EL105
00431          MOVE 'EL010   '         TO  THIS-PGM                     EL105
00432          GO TO 9300-XCTL.                                         EL105
00433                                                                   EL105
00434      IF EIBAID = DFHPF23                                          EL105
00435          GO TO 9000-RETURN-CICS.                                  EL105
00436                                                                   EL105
00437      IF EIBAID = DFHPF24                                          EL105
00438          IF  CREDIT-SESSION                                          CL**4
00439              MOVE XCTL-EL626     TO THIS-PGM                         CL**4
00440              GO TO 9300-XCTL                                      EL105
00441          ELSE                                                     EL105
00442              IF  CLAIM-SESSION                                       CL**4
00443                  MOVE XCTL-EL126 TO THIS-PGM                         CL**4
00444                  GO TO 9300-XCTL                                     CL**4
00445              ELSE                                                    CL**4
00446                  IF  MORTGAGE-SESSION                                CL**4
00447                      MOVE XCTL-EM626                                 CL**4
00448                                  TO THIS-PGM                         CL**4
00449                      GO TO 9300-XCTL                                 CL**4
00450                  ELSE                                                CL**4
00451                      IF  GENERAL-LEDGER-SESSION                      CL**4
00452                          MOVE XCTL-GL800                             CL**4
00453                                  TO THIS-PGM                         CL**4
00454                          GO TO 9300-XCTL.                            CL**4
00455                                                                   EL105
00456      IF EIBAID = DFHENTER OR DFHPF1                               EL105
00457          NEXT SENTENCE                                            EL105
00458        ELSE                                                       EL105
00459          MOVE ER-7008               TO  EMI-ERROR                 EL105
00460         IF CREDIT-SESSION                                            CL**5
00461             MOVE -1                    TO  BPFKL                     CL**5
00462             PERFORM 9900-ERROR-FORMAT                                CL**5
00463             PERFORM 8200-SEND-DATAONLY                               CL**5
00464             GO TO 9100-RETURN-TRAN                                   CL**5
00465         ELSE                                                         CL**5
00466             MOVE -1                    TO  APFKL                     CL**5
00467             PERFORM 9900-ERROR-FORMAT                                CL**5
00468             PERFORM 8200-SEND-DATAONLY                               CL**5
00469             GO TO 9100-RETURN-TRAN.                                  CL**5
00470                                                                   EL105
00471      IF EIBAID = DFHPF1                                           EL105
00472        AND PI-END-OF-FILE = +1                                    EL105
00473          MOVE LOW-VALUES         TO  PI-NEXT-CARRIER-NUMBER       EL105
00474          MOVE ZERO               TO  PI-BROWSE-SW                 EL105
00475                                      PI-END-OF-FILE               EL105
00476          GO TO 8000-DISPLAY-RECORDS.                              EL105
00477                                                                   EL105
00478      EJECT                                                        EL105
00479  0025-MAIN-LOGIC.                                                 EL105
00480      IF AMAINTL NOT GREATER ZERO                                  EL105
00481        AND EIBAID = DFHPF1                                        EL105
00482          MOVE 'S'                TO  PI-MODE                      EL105
00483                                      AMAINTO                      EL105
00484          MOVE AL-UANON           TO  AMAINTA                      EL105
00485          MOVE SPACES             TO  WS-CONTROL-FILE-KEY          EL105
00486          MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID            EL105
00487          MOVE PI-CARRIER-NUMBER  TO  WS-CFK-CARRIER-NO            EL105
00488          MOVE '6'                TO  WS-CFK-RECORD-TYPE           EL105
00489          GO TO 8000-DISPLAY-RECORDS.                              EL105
00490                                                                      CL*12
00491      MOVE PI-MODE                TO PI-PREV-MODE.                    CL*12
00492      MOVE PI-CARRIER-NUMBER      TO PI-PREV-CARRIER.                 CL*12
00493                                                                   EL105
00494      IF AMAINTI = 'S' OR ' '                                      EL105
00495          NEXT SENTENCE                                            EL105
00496         ELSE                                                      EL105
00497          IF EIBAID = DFHPF1                                       EL105
00498             MOVE ER-0050               TO  EMI-ERROR              EL105
00499             IF CREDIT-SESSION                                        CL**5
00500                 MOVE -1                TO  BPFKL                     CL**5
00501                 PERFORM 9900-ERROR-FORMAT                            CL**5
00502             ELSE                                                     CL**5
00503                 MOVE -1                TO  APFKL                     CL**5
00504                 PERFORM 9900-ERROR-FORMAT.                           CL**5
00505                                                                   EL105
00506      IF AMAINTL GREATER ZERO                                      EL105
00507          IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S'                    EL105
00508              MOVE AMAINTI        TO  PI-MODE                      EL105
00509              MOVE AL-UANON       TO  AMAINTA                      EL105
00510            ELSE                                                   EL105
00511              MOVE AL-UABOF       TO  AMAINTA                      EL105
00512              MOVE -1             TO  AMAINTL                      EL105
00513              MOVE ER-0023        TO EMI-ERROR                     EL105
00514              PERFORM 9900-ERROR-FORMAT                            EL105
00515        ELSE                                                       EL105
00516          IF PI-1ST-TIME-SW NOT = ZERO                             EL105
00517              NEXT SENTENCE                                        EL105
00518            ELSE                                                   EL105
00519              IF EIBAID = DFHPF1                                   EL105
00520                  MOVE 'S'        TO PI-MODE                       EL105
00521                ELSE                                               EL105
00522                  MOVE AL-UABOF   TO  AMAINTA                      EL105
00523                  MOVE -1         TO  AMAINTL                      EL105
00524                  MOVE ER-0023    TO EMI-ERROR                     EL105
00525                  PERFORM 9900-ERROR-FORMAT.                       EL105
00526                                                                   EL105
00527      IF ACARIERL GREATER  ZERO                                    EL105
00528          IF ACARIERI NOT = SPACES                                 EL105
00529              MOVE AL-UANON       TO  ACARIERA                     EL105
00530              MOVE ACARIERI       TO  PI-CARRIER-NUMBER            EL105
00531            ELSE                                                   EL105
00532              MOVE AL-UABOF       TO  ACARIERA                     EL105
00533              MOVE -1             TO  ACARIERL                     EL105
00534              MOVE ER-0193        TO EMI-ERROR                     EL105
00535              PERFORM 9900-ERROR-FORMAT                            EL105
00536        ELSE                                                       EL105
00537          IF PI-1ST-TIME-SW NOT = ZERO                             EL105
00538              NEXT SENTENCE                                        EL105
00539            ELSE                                                   EL105
00540              IF EIBAID = DFHPF1                                   EL105
00541                  MOVE LOW-VALUES     TO  PI-NEXT-CARRIER-NUMBER   EL105
00542                ELSE                                               EL105
00543                  MOVE AL-UABOF       TO  ACARIERA                 EL105
00544                  MOVE -1             TO  ACARIERL                 EL105
00545                  MOVE ER-0193    TO EMI-ERROR                     EL105
00546                  PERFORM 9900-ERROR-FORMAT.                       EL105
00547                                                                   EL105
00548      IF EMI-FATAL-CTR GREATER ZERO                                EL105
00549          PERFORM 8200-SEND-DATAONLY                               EL105
00550          GO TO 9100-RETURN-TRAN.                                  EL105
00551                                                                   EL105
00552      MOVE +1                     TO  PI-1ST-TIME-SW.              EL105
00553                                                                   EL105
00554      IF  PI-MODE NOT = 'S'                                        EL105
00555          MOVE +1                 TO  PI-BROWSE-SW.                EL105
00556                                                                   EL105
00557      EJECT                                                        EL105
00558  0100-MAIN-LOGIC.                                                 EL105
00559      IF PI-MODE NOT = 'S'                                         EL105
00560          GO TO 0200-MAIN-LOGIC.                                   EL105
00561                                                                   EL105
00562      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL105
00563      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL105
00564                                                                   EL105
00565      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL105
00566      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.          EL105
00567                                                                   EL105
00568      IF EIBAID = DFHPF1                                           EL105
00569          MOVE PI-NEXT-CARRIER-NUMBER  TO  WS-CFK-CARRIER-NO       EL105
00570          GO TO 8000-DISPLAY-RECORDS.                              EL105
00571                                                                   EL105
00572      GO TO 6000-SHOW-RECORD.                                      EL105
00573                                                                   EL105
00574      EJECT                                                        EL105
00575  0200-MAIN-LOGIC.                                                 EL105
00576      IF SYSTEM-MODIFY-CAP                                         EL105
00577         NEXT SENTENCE                                             EL105
00578        ELSE                                                       EL105
00579         IF PI-MODE = 'A' OR 'C' OR 'D'                               CL*15
00580            MOVE 'UPDATE'              TO SM-READ                     CL*15
00581            PERFORM 9995-SECURITY-VIOLATION                           CL*15
00582            MOVE ER-0070               TO EMI-ERROR                   CL*15
00583            MOVE -1                    TO  AMAINTL                    CL*15
00584            MOVE AL-UABON              TO  AMAINTA                    CL*15
00585            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*15
00586            PERFORM 8200-SEND-DATAONLY                                CL*15
00587            GO TO 9100-RETURN-TRAN.                                   CL*15
00588                                                                   EL105
00589      IF PI-MODE NOT = 'C'                                         EL105
00590          GO TO 0300-MAIN-LOGIC.                                   EL105
00591                                                                   EL105
00592      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL105
00593                                                                   EL105
00594      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL105
00595      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.          EL105
00596      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.           EL105
00597      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL105
00598                                                                   EL105
00599      EXEC CICS READ                                               EL105
00600          DATASET (WS-CONTROL-FILE-DSID)                           EL105
00601          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL105
00602          SET     (ADDRESS OF CONTROL-FILE)                           CL*13
00603     END-EXEC.                                                     EL105
00604                                                                   EL105
00605      MOVE CF-EXPENSE-DOLLAR      TO  WS-EXPENSE-DOLLAR.           EL105
00606      MOVE CF-EXPENSE-PERCENT     TO  WS-EXPENSE-PERCENT.          EL105
00607                                                                   EL105
00608      IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC                            CL*11
00609          MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.          CL*11
00610      IF CF-IBNR-R78-PERCENT NOT NUMERIC                              CL*11
00611          MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.            CL*11
00612      IF CF-IBNR-PRO-PERCENT NOT NUMERIC                              CL*11
00613          MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.            CL*11
00614                                                                      CL*11
00615      MOVE CF-IBNR-UEPRM-PERCENT  TO  WS-IBNR-UEP-PCT.                CL*11
00616      MOVE CF-IBNR-R78-PERCENT    TO  WS-IBNR-R78-PCT.                CL*11
00617      MOVE CF-IBNR-PRO-PERCENT    TO  WS-IBNR-PRO-PCT.                CL*11
00618                                                                      CL*11
00619      PERFORM 4000-EDIT-MAP-FIELDS.                                EL105
00620                                                                   EL105
00621      IF EMI-FATAL-CTR  GREATER ZERO                               EL105
00622          PERFORM 8200-SEND-DATAONLY                               EL105
00623          GO TO 9100-RETURN-TRAN.                                  EL105
00624                                                                   EL105
00625      PERFORM 5100-CHANGE-RECORD.                                  EL105
00626                                                                   EL105
00627      MOVE ER-0000                TO EMI-ERROR.                    EL105
00628      PERFORM 9900-ERROR-FORMAT.                                   EL105
00629      MOVE LOW-VALUES             TO  EL105AO.                     EL105
00630      MOVE -1                     TO  AMAINTL.                     EL105
00631      MOVE ZERO                   TO  PI-1ST-TIME-SW.              EL105
00632      GO TO 6000-SHOW-RECORD.                                         CL**5
00633                                                                   EL105
00634      EJECT                                                        EL105
00635  0300-MAIN-LOGIC.                                                 EL105
00636 *    NOTE ******************************************************* EL105
00637 *         *          P R O C E S S   T H E   A D D S            * EL105
00638 *         *******************************************************.EL105
00639      IF PI-MODE NOT = 'A'                                         EL105
00640          GO TO 0400-MAIN-LOGIC.                                   EL105
00641                                                                      CL*11
00642      MOVE ZEROS                  TO  WS-IBNR-UEP-PCT                 CL*11
00643                                      WS-IBNR-R78-PCT                 CL*11
00644                                      WS-IBNR-PRO-PCT.                CL*11
00645                                                                   EL105
00646      PERFORM 4000-EDIT-MAP-FIELDS.                                EL105
00647                                                                   EL105
00648      IF EMI-FATAL-CTR GREATER ZERO                                EL105
00649          PERFORM 8200-SEND-DATAONLY                               EL105
00650          GO TO 9100-RETURN-TRAN.                                  EL105
00651                                                                   EL105
00652      PERFORM 5000-ADD-RECORD.                                     EL105
00653                                                                   EL105
00654      MOVE ER-0000                TO EMI-ERROR.                    EL105
00655      PERFORM 9900-ERROR-FORMAT.                                   EL105
00656      MOVE LOW-VALUES             TO  EL105AO.                     EL105
00657      MOVE -1                     TO  AMAINTL.                     EL105
00658      PERFORM 8100-SEND-INITIAL-MAP.                               EL105
00659      MOVE ZERO                   TO  PI-1ST-TIME-SW.              EL105
00660      GO TO 9100-RETURN-TRAN.                                      EL105
00661                                                                   EL105
00662      EJECT                                                        EL105
00663  0400-MAIN-LOGIC.                                                 EL105
00664 *    NOTE ******************************************************* EL105
00665 *         *         P R O C E S S   T H E   D E L E T E S       * EL105
00666 *         *******************************************************.EL105
00667      IF PI-PREV-MODE    = 'S' AND                                    CL*12
00668         PI-PREV-CARRIER = PI-CARRIER-NUMBER                          CL*12
00669            PERFORM 5200-DELETE-RECORD                                CL*12
00670         ELSE                                                         CL*12
00671            MOVE AL-UABOF       TO AMAINTA                            CL*12
00672            MOVE -1             TO AMAINTL                            CL*12
00673            MOVE ER-0145        TO EMI-ERROR                          CL*12
00674            PERFORM 9900-ERROR-FORMAT                                 CL*12
00675            PERFORM 8200-SEND-DATAONLY                                CL*12
00676            GO TO 9100-RETURN-TRAN.                                   CL*12
00677                                                                   EL105
00678      MOVE ER-0000                TO EMI-ERROR.                    EL105
00679      PERFORM 9900-ERROR-FORMAT.                                   EL105
00680      MOVE LOW-VALUES             TO  EL105AO.                     EL105
00681      PERFORM 8100-SEND-INITIAL-MAP.                               EL105
00682      MOVE ZERO                   TO  PI-1ST-TIME-SW.              EL105
00683      GO TO 9100-RETURN-TRAN.                                      EL105
00684      EJECT                                                        EL105
00685  4000-EDIT-MAP-FIELDS SECTION.                                    EL105
112103*    NOTE ******************************************************* EL105
112103*         *              EDIT SECURE PAY SWITCH.                * EL105
112103*         *******************************************************.EL105
112103     IF CREDIT-SESSION                                               CL**5
062121         IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
112103             MOVE AL-SADOF           TO BSPLABLA
112103             MOVE AL-SADOF           TO BSECPAYA
112103         ELSE
112103             IF BSECPAYL GREATER ZERO     
112103                 IF BSECPAYI = ' ' OR 'Y' OR 'N' 
112103                     MOVE AL-UANON       TO  BSECPAYA    
112103                     MOVE BSECPAYI       TO  BSECPAYO 
112103                 ELSE                                
112103                     MOVE ER-3270        TO  EMI-ERROR  
112103                     PERFORM 9900-ERROR-FORMAT         
112103                     MOVE AL-UNBON       TO  BSECPAYA 
112103                     MOVE -1             TO  BSECPAYL
112103                 END-IF
112103             END-IF
112103         END-IF
112103     END-IF.
112103
112103*    IF NOT CREDIT-SESSION    
112103*        IF PI-COMPANY-ID = 'CID'
112103*            MOVE AL-SADOF           TO ASPLABLA
112103*            MOVE AL-SADOF           TO ASECPAYA
112103*        ELSE
112103*            IF ASECPAYL GREATER ZERO        
112103*                IF ASECPAYI = ' ' OR 'Y' OR 'N' 
112103*                    MOVE AL-UANON       TO  ASECPAYA      
112103*                    MOVE ASECPAYI       TO  ASECPAYO 
112103*                ELSE                                
112103*                    MOVE ER-3270        TO  EMI-ERROR  
112103*                    PERFORM 9900-ERROR-FORMAT         
112103*                    MOVE AL-UNBON       TO  ASECPAYA 
112103*                    MOVE -1             TO  ASECPAYL
112103*                END-IF
112103*            END-IF
112103*        END-IF
112103*    END-IF.
00704                                                                      CL**5
00686 *    NOTE ******************************************************* EL105
00687 *         *              EDIT THE PHONE NUMBER.                 * EL105
00688 *         *******************************************************.EL105
00689      IF CREDIT-SESSION                                               CL**5
00690          IF BPHONEL GREATER ZERO                                     CL**5
00691              EXEC CICS BIF DEEDIT                                    CL**5
00692                  FIELD  (BPHONEI)                                    CL**5
00693                  LENGTH (APHONE-LENGTH)                              CL**5
00694              END-EXEC                                                CL**5
00695              IF BPHONEI NUMERIC                                      CL**5
00696                  MOVE AL-UNNON       TO  BPHONEA                     CL**5
00697                  MOVE BPHONEI        TO  BPHONEO                     CL**5
00698                  INSPECT BPHONEO CONVERTING ' '    TO '-'            CL*13
00699              ELSE                                                    CL**5
00700                  MOVE ER-0053           TO  EMI-ERROR                CL**5
00701                  PERFORM 9900-ERROR-FORMAT                           CL**5
00702                  MOVE AL-UNBON       TO  BPHONEA                     CL**5
00703                  MOVE -1             TO  BPHONEL.                    CL**5
00704                                                                      CL**5
00705      IF NOT CREDIT-SESSION                                           CL**5
00706          IF APHONEL GREATER ZERO                                     CL**5
00707              EXEC CICS BIF DEEDIT                                    CL**5
00708                  FIELD  (APHONEI)                                    CL**5
00709                  LENGTH (APHONE-LENGTH)                              CL**5
00710              END-EXEC                                                CL**5
00711              IF APHONEI NUMERIC                                      CL**5
00712                  MOVE AL-UNNON       TO  APHONEA                     CL**5
00713                  MOVE APHONEI        TO  APHONEO                     CL**5
00714                  INSPECT APHONEO CONVERTING SPACES TO '-'            CL*13
00715              ELSE                                                    CL**5
00716                  MOVE ER-0053           TO  EMI-ERROR                CL**5
00717                  PERFORM 9900-ERROR-FORMAT                           CL**5
00718                  MOVE AL-UNBON       TO  APHONEA                     CL**5
00719                  MOVE -1             TO  APHONEL.                    CL**5
00720                                                                   EL105
00721 *    NOTE ******************************************************* EL105
00722 *         *              EDIT THE ZIP CODE.                     * EL105
00723 *         *******************************************************.EL105
00724      IF CREDIT-SESSION                                               CL**5
00725          IF BZIPL GREATER ZERO                                       CL**5
00726              MOVE AL-UANON          TO  BZIPA.                       CL**7
00727                                                                      CL**5
00728      IF NOT CREDIT-SESSION                                           CL**5
00729          IF AZIPL GREATER ZERO                                       CL**5
00730              MOVE AL-UANON          TO  AZIPA.                       CL**7
00731                                                                   EL105
00732 *    NOTE ******************************************************* EL105
00733 *         *              EDIT THE DOMICILE STATE CODE.          * EL105
00734 *         *******************************************************.EL105
00735      IF CREDIT-SESSION                                               CL**5
00736          IF BDOMSTL GREATER ZERO                                     CL**5
00737             IF BDOMSTI ALPHABETIC-UPPER                              CL*13
00738                 MOVE AL-UNNON       TO  BDOMSTA                      CL**5
00739              ELSE                                                    CL**5
00740                 MOVE AL-UNBON       TO  BDOMSTA                      CL**5
00741                 MOVE -1             TO  BDOMSTL                      CL**5
00742                 MOVE ER-0529 TO  EMI-ERROR                           CL**5
00743                 PERFORM 9900-ERROR-FORMAT.                           CL**5
00744                                                                      CL**5
112103*    NOTE ******************************************************* EL105
112103*         *              EDIT THE CLP PERCENT.                  * EL105
112103*         *******************************************************.EL105
112103     IF CREDIT-SESSION                                               CL**5
062121         IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103             MOVE AL-SADOF          TO  BCTLABLA
112103             MOVE AL-SADOF          TO  BCLPTOLA
112103         ELSE
112103             IF BCLPTOLL GREATER ZERO               
112103                 EXEC CICS BIF
112103                     DEEDIT  
112103                     FIELD  (BCLPTOLI)       
112103                     LENGTH (7)             
112103                 END-EXEC                  
112103                 IF BCLPTOLI NUMERIC    
112103                     MOVE AL-UNNON      TO  BCLPTOLA     
112103                     MOVE BCLPTOLI      TO  WS-CLP-TOL-PCT
112103                 ELSE                                    
112103                     MOVE AL-UNBON      TO  BCLPTOLA    
112103                     MOVE -1            TO  BCLPTOLL   
112103                     MOVE ER-1778       TO  EMI-ERROR 
112103                     PERFORM 9900-ERROR-FORMAT
112103                 END-IF
112103             END-IF
112103         END-IF
112103     END-IF.
112103                                                                     CL**5
092705*    NOTE *******************************************************
092705*         *              EDIT THE LEASE COMM AMOUNT             *
092705*         *******************************************************
092705     IF CREDIT-SESSION
092705        IF BLCOMML > ZERO
092705           EXEC CICS BIF
092705                DEEDIT
092705                FIELD  (BLCOMMI)
092705                LENGTH (8)
092705           END-EXEC
092705           IF BLCOMMI NUMERIC
092705              MOVE AL-UNNON      TO BLCOMMA
092705              MOVE BLCOMMI       TO WS-SPP-LEASE-COMM
092705           ELSE
092705              MOVE AL-UNBON      TO BLCOMMA
092705              MOVE -1            TO BLCOMML
092705              MOVE ER-1778       TO EMI-ERROR
092705              PERFORM 9900-ERROR-FORMAT
092705           END-IF
092705        END-IF
092705     END-IF
092705                                                                     CL**5
00745 *    NOTE *******************************************************    CL**5
00746 *         *              EDIT THE PREMIUM TOLERANCE PERCENTAGE  *    CL**5
00747 *         *******************************************************.   CL**5
00748      IF CREDIT-SESSION                                               CL**5
00749          IF BPRMTOLL GREATER ZERO                                    CL**5
00750              EXEC CICS BIF DEEDIT                                    CL**5
00751                  FIELD  (BPRMTOLI)                                   CL**5
00752                  LENGTH (6)                                          CL**5
00753              END-EXEC                                                CL**5
00754              IF BPRMTOLI NOT NUMERIC                                 CL**5
00755                  MOVE -1             TO  BPRMTOLL                    CL**5
00756                  MOVE AL-UNBON       TO  BPRMTOLA                    CL**5
00757                  MOVE ER-2010           TO  EMI-ERROR                CL**5
00758                  PERFORM 9900-ERROR-FORMAT                           CL**5
00759              ELSE                                                    CL**5
00760              IF BPRMTOLI GREATER THAN 9999                           CL**5
00761                  MOVE -1             TO  BPRMTOLL                    CL**5
00762                  MOVE AL-UNBON       TO  BPRMTOLA                    CL**5
00763                  MOVE ER-2010           TO  EMI-ERROR                CL**5
00764                  PERFORM 9900-ERROR-FORMAT                           CL**5
00765              ELSE                                                    CL**5
00766                  MOVE AL-UNNON       TO  BPRMTOLA                    CL**5
00767                  MOVE BPRMTOLI       TO  BPRMTOLO.                   CL**5
00768                                                                      CL**5
00769      IF CREDIT-SESSION                                               CL**5
00770          IF BREFTOLL GREATER ZERO                                    CL**5
00771              EXEC CICS BIF DEEDIT                                    CL**5
00772                  FIELD  (BREFTOLI)                                   CL**5
00773                  LENGTH (6)                                          CL**5
00774              END-EXEC                                                CL**5
00775              IF BREFTOLI NOT NUMERIC                                 CL**5
00776                  MOVE -1             TO  BREFTOLL                    CL**5
00777                  MOVE AL-UNBON       TO  BREFTOLA                    CL**5
00778                  MOVE ER-2014           TO  EMI-ERROR                CL**5
00779                  PERFORM 9900-ERROR-FORMAT                           CL**5
00780              ELSE                                                    CL**5
00781              IF BREFTOLI GREATER THAN 9999                           CL**5
00782                  MOVE -1             TO  BREFTOLL                    CL**5
00783                  MOVE AL-UNBON       TO  BREFTOLA                    CL**5
00784                  MOVE ER-2014           TO  EMI-ERROR                CL**5
00785                  PERFORM 9900-ERROR-FORMAT                           CL**5
00786              ELSE                                                    CL**5
00787                  MOVE AL-UNNON       TO  BREFTOLA                    CL**5
00788                  MOVE BREFTOLI       TO  BREFTOLO.                   CL**5
00789                                                                      CL**5
00790      IF CREDIT-SESSION                                               CL**5
00791          IF BPRMPCTL GREATER ZERO                                    CL**5
00792              EXEC CICS BIF DEEDIT                                    CL**5
00793                  FIELD  (BPRMPCTI)                                   CL**5
00794                  LENGTH (5)                                          CL**5
00795              END-EXEC                                                CL**5
00796              IF BPRMPCTI NUMERIC                                     CL**5
00797                  MOVE AL-UNNON       TO  BPRMPCTA                    CL**5
00798                  MOVE BPRMPCTI       TO  WS-TOL-PREM-PCT             CL**5
00799              ELSE                                                    CL**5
00800                  MOVE -1             TO  BPRMPCTL                    CL**5
00801                  MOVE AL-UNBON       TO  BPRMPCTA                    CL**5
00802                  MOVE ER-7532           TO  EMI-ERROR                CL**5
00803                  PERFORM 9900-ERROR-FORMAT.                          CL**5
00804                                                                      CL**5
00805      IF CREDIT-SESSION                                               CL**5
00806          IF BREFPCTL GREATER ZERO                                    CL**5
00807              EXEC CICS BIF DEEDIT                                    CL**5
00808                  FIELD  (BREFPCTI)                                   CL**5
00809                  LENGTH (5)                                          CL**5
00810              END-EXEC                                                CL**5
00811              IF BREFPCTI NUMERIC                                     CL**5
00812                  MOVE AL-UNNON       TO  BREFPCTA                    CL**5
00813                  MOVE BREFPCTI       TO  WS-TOL-REF-PCT              CL**5
00814              ELSE                                                    CL**5
00815                  MOVE -1             TO  BREFPCTL                    CL**5
00816                  MOVE AL-UNBON       TO  BREFPCTA                    CL**5
00817                  MOVE ER-7532           TO  EMI-ERROR                CL**5
00818                  PERFORM 9900-ERROR-FORMAT.                          CL**5
00819                                                                      CL*17
00820      IF CREDIT-SESSION                                               CL*17
00821          IF BOVSPCTL GREATER ZERO                                    CL*17
00822              EXEC CICS BIF DEEDIT                                    CL*17
00823                  FIELD  (BOVSPCTI)                                   CL*17
00824                  LENGTH (5)                                          CL*17
00825              END-EXEC                                                CL*17
00826              IF BOVSPCTI NUMERIC                                     CL*17
00827                  MOVE AL-UNNON       TO  BOVSPCTA                    CL*17
00828                  MOVE BOVSPCTI       TO  WS-CR-OVR-SHT-PCT           CL*17
00829              ELSE                                                    CL*17
00830                  MOVE -1             TO  BOVSPCTL                    CL*17
00831                  MOVE AL-UNBON       TO  BOVSPCTA                    CL*17
00832                  MOVE ER-7532           TO  EMI-ERROR                CL*17
00833                  PERFORM 9900-ERROR-FORMAT                           CL*17
00834              END-IF                                                  CL*17
00835          END-IF.                                                     CL*17
00836                                                                      CL**5
00837      IF CREDIT-SESSION                                               CL**5
00838          GO TO 4900-EXIT.                                            CL**5
00839                                                                      CL**5
00840      IF CLAIM-SESSION                                                CL*13
00841         IF ALPHCHL GREATER ZERO                                      CL*13
00842            IF ALPHCHI ALPHABETIC                                     CL*14
00843               MOVE AL-UANON       TO  ALPHCHA                        CL*14
00844            ELSE                                                      CL*14
00845               MOVE AL-UNBON       TO  ALPHCHA                        CL*14
00846               MOVE -1             TO  ALPHCHL                        CL*14
00847               MOVE ER-8128        TO  EMI-ERROR                      CL*14
00848               PERFORM 9900-ERROR-FORMAT.                             CL*14
00849                                                                      CL*13
00850      IF ADOMSTL GREATER ZERO                                      EL105
00851          IF ADOMSTI ALPHABETIC-UPPER                                 CL*13
00852              MOVE AL-UNNON       TO  ADOMSTA                      EL105
00853          ELSE                                                        CL**5
00854              MOVE AL-UNBON       TO  ADOMSTA                      EL105
00855              MOVE -1             TO  ADOMSTL                      EL105
00856              MOVE ER-0529 TO  EMI-ERROR                              CL**5
00857              PERFORM 9900-ERROR-FORMAT.                           EL105
00858                                                                      CL**5
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF           TO ACTLABLA
112103*        MOVE AL-SADOF           TO ACLPTOLA
112103*    ELSE
112103*        IF ACLPTOLL GREATER ZERO      
112103*            EXEC CICS BIF
112103*                DEEDIT  
112103*                FIELD  (ACLPTOLI)    
112103*                LENGTH (6)          
112103*            END-EXEC               
112103*            IF ACLPTOLI NUMERIC   
112103*                MOVE AL-UNNON       TO  ACLPTOLA    
112103*                MOVE ACLPTOLI       TO  WS-CLP-TOL-PCT 
112103*            ELSE  
112103*                MOVE AL-UNBON       TO  ACLPTOLA
112103*                MOVE -1             TO  ACLPTOLL
112103*                MOVE ER-1778 TO  EMI-ERROR         
112103*                PERFORM 9900-ERROR-FORMAT
112103*            END-IF
112103*        END-IF
112103*    END-IF.
00860                                                                   EL105
00861 *    NOTE ******************************************************* EL105
00862 *         *      EDIT THE CLAIM NUMBER ASSIGNMENT METHOD.       * EL105
00863 *         *******************************************************.EL105
00864      IF ACLNAML GREATER ZERO                                      EL105
00865          IF (ACLNAMI = '1' OR '2' OR '3' OR '4' OR '5')              CL*13
00866              MOVE AL-UNNON       TO  ACLNAMA                      EL105
00867            ELSE                                                   EL105
00868              MOVE AL-UNBON       TO  ACLNAMA                      EL105
00869              MOVE -1             TO  ACLNAML                      EL105
00870              MOVE ER-0090        TO EMI-ERROR                     EL105
00871              PERFORM 9900-ERROR-FORMAT                            EL105
00872        ELSE                                                       EL105
00873          IF PI-MODE = 'A'                                         EL105
00874              MOVE AL-UNBOF       TO  ACLNAMA                      EL105
00875              MOVE -1             TO  ACLNAML                      EL105
00876              MOVE ER-0090        TO EMI-ERROR                     EL105
00877              PERFORM 9900-ERROR-FORMAT.                           EL105
00878                                                                   EL105
00879 *    NOTE ******************************************************* EL105
00880 *         *      EDIT THE CHECK NUMBER ASSIGNMENT METHOD.       * EL105
00881 *         *******************************************************.EL105
00882      IF ACKNAML GREATER ZERO                                      EL105
00883          IF ACKNAMI GREATER ZERO                                  EL105
00884            AND ACKNAMI LESS '5'                                   EL105
00885              MOVE AL-UNNON       TO  ACKNAMA                      EL105
00886            ELSE                                                   EL105
00887              MOVE AL-UNBON       TO  ACKNAMA                      EL105
00888              MOVE -1             TO  ACKNAML                      EL105
00889              MOVE ER-0091        TO EMI-ERROR                     EL105
00890              PERFORM 9900-ERROR-FORMAT                            EL105
00891        ELSE                                                       EL105
00892          IF PI-MODE = 'A'                                         EL105
00893              MOVE AL-UNBOF       TO  ACKNAMA                      EL105
00894              MOVE -1             TO  ACKNAML                      EL105
00895              MOVE ER-0091        TO EMI-ERROR                     EL105
00896              PERFORM 9900-ERROR-FORMAT.                           EL105
00897                                                                   EL105
00898 *    NOTE ******************************************************* EL105
00899 *         *            EDIT THE CLAIM STARTING NUMBER.          * EL105
00900 *         *******************************************************.EL105
00901      IF ACLAIML GREATER ZERO                                      EL105
00902          EXEC CICS BIF DEEDIT                                     EL105
00903              FIELD  (ACLAIMI)                                     EL105
00904              LENGTH (8)                                           EL105
00905          END-EXEC                                                 EL105
00906          IF ACLAIMI NUMERIC                                       EL105
00907              MOVE AL-UNNON       TO  ACLAIMA                      EL105
00908            ELSE                                                   EL105
00909              MOVE AL-UNBON       TO  ACLAIMA                      EL105
00910              MOVE -1             TO  ACLAIML                      EL105
00911              MOVE ER-0637        TO EMI-ERROR                     EL105
00912              PERFORM 9900-ERROR-FORMAT                            EL105
00913                                                                   EL105
00914 *    NOTE ******************************************************* EL105
00915 *         *            EDIT THE CHECK STARTING NUMBER.          * EL105
00916 *         *******************************************************.EL105
00917      IF ACHECKL GREATER ZERO                                      EL105
00918          EXEC CICS BIF DEEDIT                                     EL105
00919              FIELD  (ACHECKI)                                     EL105
00920              LENGTH (8)                                           EL105
00921          END-EXEC                                                 EL105
00922          IF ACHECKI NUMERIC                                       EL105
00923              MOVE AL-UNNON       TO  ACHECKA                      EL105
00924            ELSE                                                   EL105
00925              MOVE AL-UNBON       TO  ACHECKA                      EL105
00926              MOVE -1             TO  ACHECKL                      EL105
00927              MOVE ER-0638           TO  EMI-ERROR                 EL105
00928              PERFORM 9900-ERROR-FORMAT                            EL105
00929                                                                   EL105
00930 *    NOTE ******************************************************* EL105
00931 *         *              EDIT THE LETTER ARCHIVE.               * EL105
00932 *         *******************************************************.EL105
00933      IF ALAL GREATER ZERO                                         EL105
00934          IF ALAI = 'Y' OR 'N'                                     EL105
00935              MOVE AL-UANON       TO  ALAA                         EL105
00936            ELSE                                                   EL105
00937              MOVE AL-UABON       TO  ALAA                         EL105
00938              MOVE -1             TO  ALAL                         EL105
00939              MOVE ER-0092        TO EMI-ERROR                     EL105
00940              PERFORM 9900-ERROR-FORMAT                            EL105
00941        ELSE                                                       EL105
00942          IF PI-MODE = 'A'                                         EL105
00943              MOVE AL-UABOF       TO  ALAA                         EL105
00944              MOVE -1             TO  ALAL                         EL105
00945              MOVE ER-0092        TO EMI-ERROR                     EL105
00946              PERFORM 9900-ERROR-FORMAT.                           EL105
00947                                                                   EL105
00948      IF AEXPCML NOT GREATER ZERO                                  EL105
00949          GO TO 4200-EDIT-MAP-FIELDS.                              EL105
00950                                                                   EL105
00951      IF AEXPCML GREATER ZERO                                      EL105
00952          IF AEXPCMI GREATER ZERO                                  EL105
00953            AND AEXPCMI LESS '5'                                   EL105
00954              MOVE AL-UNNON           TO  AEXPCMA                  EL105
00955            ELSE                                                   EL105
00956              MOVE AL-UNBON           TO  AEXPCMA                  EL105
00957              MOVE -1                 TO  AEXPCML                  EL105
00958              MOVE ER-0093            TO EMI-ERROR                 EL105
00959              PERFORM 9900-ERROR-FORMAT.                           EL105
00960                                                                   EL105
00961      IF AEXPCAL GREATER ZERO                                      EL105
00962          EXEC CICS BIF DEEDIT                                     EL105
00963              FIELD  (AEXPCAI)                                     EL105
00964              LENGTH (AEXPCA-LENGTH)                               EL105
00965          END-EXEC                                                 EL105
00966          IF AEXPCAI NUMERIC                                       EL105
00967              MOVE AL-UNNON       TO  AEXPCAA                      EL105
00968              MOVE AEXPCAI        TO  WS-EXPENSE-DOLLAR            EL105
00969                                      AEXPCAO                      EL105
00970            ELSE                                                   EL105
00971              MOVE AL-UNBON       TO  AEXPCAA                      EL105
00972              MOVE -1             TO  AEXPCAL                      EL105
00973              MOVE ER-0097        TO EMI-ERROR                     EL105
00974              PERFORM 9900-ERROR-FORMAT.                           EL105
00975                                                                   EL105
00976      IF AEXPCPL GREATER ZERO                                      EL105
00977          EXEC CICS BIF DEEDIT                                     EL105
00978              FIELD  (AEXPCPI)                                     EL105
00979              LENGTH (AEXPCP-LENGTH)                               EL105
00980          END-EXEC                                                 EL105
00981          IF AEXPCPI NUMERIC                                       EL105
00982              MOVE AEXPCPI        TO  WS-EXPENSE-PERCENT           EL105
00983                                      AEXPCPO                      EL105
00984              MOVE AL-UNNON       TO  AEXPCPA                      EL105
00985            ELSE                                                   EL105
00986              MOVE AL-UNBON       TO  AEXPCPA                      EL105
00987              MOVE -1             TO  AEXPCPL                      EL105
00988              MOVE ER-0099        TO EMI-ERROR                     EL105
00989              PERFORM 9900-ERROR-FORMAT.                           EL105
00990                                                                      CL*13
00991      IF CLAIM-SESSION                                                CL*13
00992         IF ABRETRL GREATER ZERO                                      CL*15
00993            IF ABRETRI NUMERIC                                        CL*15
00994               MOVE ABRETRI   TO CF-BUILD-RETRIEVE-AFTER-MONTHS       CL*15
00995            ELSE                                                      CL*14
00996               MOVE AL-UNBON           TO ABRETRA                     CL*15
00997               MOVE -1                 TO ABRETRL                     CL*15
00998               MOVE ER-8127            TO EMI-ERROR                   CL*14
00999               PERFORM 9900-ERROR-FORMAT.                             CL*14
01000                                                                   EL105
01001      IF AEXPCMI NOT = '1'                                         EL105
01002          GO TO 4120-EDIT-MAP-FIELDS.                              EL105
01003                                                                   EL105
01004      IF WS-EXPENSE-PERCENT NOT = ZERO                             EL105
01005          MOVE AL-UNBON           TO  AEXPCPA                      EL105
01006          MOVE -1                 TO  AEXPCPL                      EL105
01007          MOVE ER-0094            TO EMI-ERROR                     EL105
01008          PERFORM 9900-ERROR-FORMAT.                               EL105
01009                                                                   EL105
01010      IF WS-EXPENSE-DOLLAR NOT = ZERO                              EL105
01011          MOVE AL-UNBON           TO  AEXPCAA                      EL105
01012          MOVE -1                 TO  AEXPCAL                      EL105
01013          MOVE ER-0095            TO EMI-ERROR                     EL105
01014          PERFORM 9900-ERROR-FORMAT.                               EL105
01015                                                                   EL105
01016      GO TO 4200-EDIT-MAP-FIELDS.                                  EL105
01017                                                                   EL105
01018  4120-EDIT-MAP-FIELDS.                                            EL105
01019      IF AEXPCMI NOT = '2'                                         EL105
01020          GO TO 4130-EDIT-MAP-FIELDS.                              EL105
01021                                                                   EL105
01022      IF WS-EXPENSE-PERCENT NOT = ZERO                             EL105
01023          MOVE AL-UNBON           TO  AEXPCPA                      EL105
01024          MOVE -1                 TO  AEXPCPL                      EL105
01025          MOVE ER-0096            TO EMI-ERROR                     EL105
01026          PERFORM 9900-ERROR-FORMAT.                               EL105
01027                                                                   EL105
01028      IF WS-EXPENSE-DOLLAR = ZERO                                  EL105
01029          MOVE AL-UNBON           TO  AEXPCAA                      EL105
01030          MOVE -1                 TO  AEXPCAL                      EL105
01031          MOVE ER-0098            TO EMI-ERROR                     EL105
01032          PERFORM 9900-ERROR-FORMAT.                               EL105
01033                                                                   EL105
01034      GO TO 4200-EDIT-MAP-FIELDS.                                  EL105
01035                                                                   EL105
01036  4130-EDIT-MAP-FIELDS.                                            EL105
01037      IF AEXPCMI NOT = '3'                                         EL105
01038          GO TO 4140-EDIT-MAP-FIELDS.                              EL105
01039                                                                   EL105
01040      IF WS-EXPENSE-PERCENT = ZERO                                 EL105
01041          MOVE AL-UNBON           TO  AEXPCPA                      EL105
01042          MOVE -1                 TO  AEXPCPL                      EL105
01043          MOVE ER-0100            TO EMI-ERROR                     EL105
01044          PERFORM 9900-ERROR-FORMAT.                               EL105
01045                                                                   EL105
01046      IF WS-EXPENSE-DOLLAR NOT = ZERO                              EL105
01047          MOVE AL-UNBON           TO  AEXPCAA                      EL105
01048          MOVE -1                 TO  AEXPCAL                      EL105
01049          MOVE ER-0117            TO EMI-ERROR                     EL105
01050          PERFORM 9900-ERROR-FORMAT.                               EL105
01051                                                                   EL105
01052      GO TO 4200-EDIT-MAP-FIELDS.                                  EL105
01053                                                                   EL105
01054  4140-EDIT-MAP-FIELDS.                                            EL105
01055      IF WS-EXPENSE-PERCENT NOT = ZERO                             EL105
01056          MOVE AL-UNBON           TO  AEXPCPA                      EL105
01057          MOVE -1                 TO  AEXPCPL                      EL105
01058          MOVE ER-0101            TO EMI-ERROR                     EL105
01059          PERFORM 9900-ERROR-FORMAT.                               EL105
01060                                                                   EL105
01061      IF WS-EXPENSE-DOLLAR = ZERO                                  EL105
01062          MOVE AL-UABON           TO  AEXPCAA                      EL105
01063          MOVE -1                 TO  AEXPCAL                      EL105
01064          MOVE ER-0102            TO EMI-ERROR                     EL105
01065          PERFORM 9900-ERROR-FORMAT.                               EL105
01066                                                                   EL105
01067      GO TO 4200-EDIT-MAP-FIELDS.                                  EL105
01068                                                                   EL105
01069  4200-EDIT-MAP-FIELDS.                                            EL105
01070 *    NOTE ******************************************************* EL105
01071 *         *          EDIT THE CLAIM CALCULATION METHOD.         * EL105
01072 *         *******************************************************.EL105
01073      IF ACLCML GREATER ZERO                                       EL105
01074          IF ACLCMI GREATER ZERO                                   EL105
01075            AND ACLCMI LESS '7'                                    EL105
01076              MOVE AL-UNNON       TO  ACLCMA                       EL105
01077            ELSE                                                   EL105
01078              MOVE AL-UNBON       TO  ACLCMA                       EL105
01079              MOVE -1             TO  ACLCML                       EL105
01080              MOVE ER-0103        TO EMI-ERROR                     EL105
01081              PERFORM 9900-ERROR-FORMAT.                           EL105
01082                                                                   EL105
01083 *    NOTE ******************************************************* EL105
01084 *         *      EDIT THE MANUAL RESERVES INDICATOR.            * EL105
01085 *         *******************************************************.EL105
01086      IF ARESMANL GREATER ZERO                                     EL105
01087          IF ARESMANI = 'Y' OR 'N'                                 EL105
01088              MOVE AL-UANON       TO  ARESMANA                     EL105
01089            ELSE                                                   EL105
01090              MOVE AL-UABON       TO  ARESMANA                     EL105
01091              MOVE -1             TO  ARESMANL                     EL105
01092              MOVE ER-0107        TO EMI-ERROR                     EL105
01093              PERFORM 9900-ERROR-FORMAT                            EL105
01094        ELSE                                                       EL105
01095          IF PI-MODE = 'A'                                         EL105
01096              MOVE AL-UABOF       TO  ARESMANA                     EL105
01097              MOVE -1             TO  ARESMANL                     EL105
01098              MOVE ER-0108        TO EMI-ERROR                     EL105
01099              PERFORM 9900-ERROR-FORMAT.                           EL105
01100                                                                   EL105
01101 *    NOTE ******************************************************* EL105
01102 *         *      EDIT THE CDT/FUTURE RESERVES INDICATOR.        * EL105
01103 *         *******************************************************.EL105
01104      IF ARESCDTL GREATER ZERO                                     EL105
01105          IF ARESCDTI = 'Y' OR 'N'                                 EL105
01106              MOVE AL-UANON       TO  ARESCDTA                     EL105
01107            ELSE                                                   EL105
01108              MOVE AL-UABON       TO  ARESCDTA                     EL105
01109              MOVE -1             TO  ARESCDTL                     EL105
01110              MOVE ER-0109        TO EMI-ERROR                     EL105
01111              PERFORM 9900-ERROR-FORMAT                            EL105
01112        ELSE                                                       EL105
01113          IF PI-MODE = 'A'                                         EL105
01114              MOVE AL-UABOF       TO  ARESCDTA                     EL105
01115              MOVE -1             TO  ARESCDTL                     EL105
01116              MOVE ER-0110        TO EMI-ERROR                     EL105
01117              PERFORM 9900-ERROR-FORMAT.                           EL105
01118                                                                   EL105
01119 *    NOTE ******************************************************* EL105
01120 *         *        EDIT THE IBN RESERVES INDICATOR.             * EL105
01121 *         *******************************************************.EL105
01122      IF ARESIBNL GREATER ZERO                                     EL105
01123          IF ARESIBNI = 'Y' OR 'N' OR '1' OR '2'                   EL105
01124              MOVE AL-UANON       TO  ARESIBNA                     EL105
01125            ELSE                                                   EL105
01126              MOVE AL-UABON       TO  ARESIBNA                     EL105
01127              MOVE -1             TO  ARESIBNL                     EL105
01128              MOVE ER-0111        TO EMI-ERROR                     EL105
01129              PERFORM 9900-ERROR-FORMAT                            EL105
01130        ELSE                                                       EL105
01131          IF PI-MODE = 'A'                                         EL105
01132              MOVE AL-UABOF       TO  ARESIBNA                     EL105
01133              MOVE -1             TO  ARESIBNL                     EL105
01134              MOVE ER-0112        TO EMI-ERROR                     EL105
01135              PERFORM 9900-ERROR-FORMAT.                           EL105
01136                                                                   EL105
01137 *    NOTE ******************************************************* EL105
01138 *         *      EDIT THE PAY-TO-CURRENT RESERVES INDICATOR.    * EL105
01139 *         *******************************************************.EL105
01140      IF ARESPTCL GREATER ZERO                                     EL105
01141          IF ARESPTCI = 'Y' OR 'N'                                 EL105
01142              MOVE AL-UANON       TO  ARESPTCA                     EL105
01143            ELSE                                                   EL105
01144              MOVE AL-UABON       TO  ARESPTCA                     EL105
01145              MOVE -1             TO  ARESPTCL                     EL105
01146              MOVE ER-0113        TO EMI-ERROR                     EL105
01147              PERFORM 9900-ERROR-FORMAT                            EL105
01148        ELSE                                                       EL105
01149          IF PI-MODE = 'A'                                         EL105
01150              MOVE AL-UABOF       TO  ARESPTCA                     EL105
01151              MOVE -1             TO  ARESPTCL                     EL105
01152              MOVE ER-0114        TO EMI-ERROR                     EL105
01153              PERFORM 9900-ERROR-FORMAT.                           EL105
01154                                                                   EL105
01155 *    NOTE ******************************************************* EL105
01156 *         *      IF THE PERCENT OF CDT IS ENTERED THE CDT ACCESS* EL105
01157 *         *  MUST ALSO BE ENTERED ON ADD.                       * EL105
01158 *         *******************************************************.EL105
01159      IF PI-MODE = 'A'                                             EL105
01160          IF ACDTAL NOT GREATER ZERO                               EL105
01161            AND APCTCDTL GREATER ZERO                              EL105
01162              MOVE -1             TO  ACDTAL                       EL105
01163              MOVE AL-UNBOF       TO  ACDTAA                       EL105
01164              MOVE AL-UNBON       TO  APCTCDTA                     EL105
01165              MOVE ER-0104        TO EMI-ERROR                     EL105
01166              PERFORM 9900-ERROR-FORMAT.                           EL105
01167                                                                   EL105
01168 *    NOTE ******************************************************* EL105
01169 *         *      EDIT THE PERCENT OF CDT.                       * EL105
01170 *         *******************************************************.EL105
01171      IF APCTCDTL GREATER ZERO                                     EL105
01172          EXEC CICS BIF DEEDIT                                     EL105
01173              FIELD  (APCTCDTI)                                    EL105
01174              LENGTH (APCTCDT-LENGTH)                              EL105
01175          END-EXEC                                                 EL105
01176          IF APCTCDTI NUMERIC                                      EL105
01177              MOVE AL-UNNON       TO  APCTCDTA                     EL105
01178              MOVE APCTCDTI         TO  APCTCDTO                   EL105
01179            ELSE                                                   EL105
01180              MOVE AL-UNBON       TO  APCTCDTA                     EL105
01181              MOVE -1             TO  APCTCDTL                     EL105
01182              MOVE ER-0106           TO  EMI-ERROR                 EL105
01183              PERFORM 9900-ERROR-FORMAT.                              CL**8
01184                                                                      CL**8
01185 *    NOTE *******************************************************    CL**8
01186 *         *      EDIT THE CDT ACCESS METHOD                     *    CL**8
01187 *         *******************************************************.   CL**8
01188      IF ACDTAL GREATER THAN +0                                       CL**8
01189          IF (ACDTAI = '1' OR '2' OR '3')                             CL**8
01190             OR                                                       CL**8
01191             (PI-COMPANY-ID EQUAL 'FLA' AND                           CL**8
01192              ACDTAI EQUAL '1' OR '2' OR '3' OR '4')                  CL**8
01193              MOVE AL-UANON       TO  ARESIBNA                        CL**8
01194            ELSE                                                      CL**8
01195              MOVE AL-UABON       TO  ACDTAA                          CL**8
01196              MOVE -1             TO  ACDTAL                          CL**8
01197              MOVE ER-0105        TO EMI-ERROR                        CL**8
01198              PERFORM 9900-ERROR-FORMAT.                           EL105
01199                                                                   EL105
01200 *    NOTE ******************************************************* EL105
01201 *         *      EDIT THE IBNR PERCENT.                         * EL105
01202 *         *******************************************************.EL105
01203      IF IBNRPCTL GREATER ZERO                                     EL105
01204          EXEC CICS BIF DEEDIT                                     EL105
01205              FIELD  (IBNRPCTI)                                    EL105
01206              LENGTH (IBNRPCT-LENGTH)                              EL105
01207          END-EXEC                                                 EL105
01208          IF IBNRPCTI NUMERIC                                      EL105
01209              MOVE AL-UNNON       TO  IBNRPCTA                     EL105
01210              MOVE IBNRPCTI       TO  IBNRPCTO                     EL105
01211            ELSE                                                   EL105
01212              MOVE AL-UNBON       TO  IBNRPCTA                     EL105
01213              MOVE -1             TO  IBNRPCTL                     EL105
01214              MOVE ER-0106        TO  EMI-ERROR                    EL105
01215              PERFORM 9900-ERROR-FORMAT.                           EL105
01216                                                                   EL105
01217 *    NOTE ******************************************************* EL105
01218 *         *      EDIT THE IBNR UNEARNED PREMIUM PERCENT         *    CL*11
01219 *         *******************************************************.   CL*11
01220      IF AUEPPCTL GREATER ZERO                                        CL*11
01221          EXEC CICS BIF DEEDIT                                        CL*11
01222              FIELD  (AUEPPCTI)                                       CL*11
01223              LENGTH (AUEPPCT-LENGTH)                                 CL*11
01224          END-EXEC                                                    CL*11
01225          IF AUEPPCTI NUMERIC                                         CL*11
01226              MOVE AL-UNNON       TO  AUEPPCTA                        CL*11
01227              MOVE AUEPPCTI       TO  WS-IBNR-UEP-PCT                 CL*11
01228                                      AUEPPCTO                        CL*11
01229            ELSE                                                      CL*11
01230              MOVE AL-UNBON       TO  AUEPPCTA                        CL*11
01231              MOVE -1             TO  AUEPPCTL                        CL*11
01232              MOVE ER-0106        TO  EMI-ERROR                       CL*11
01233              PERFORM 9900-ERROR-FORMAT.                              CL*11
01234                                                                      CL*11
01235 *    NOTE *******************************************************    CL*11
01236 *         *      EDIT THE IBNR UNEARNED RULE 78 PERCENT         *    CL*11
01237 *         *******************************************************.   CL*11
01238      IF AR78PCTL GREATER ZERO                                        CL*11
01239          EXEC CICS BIF DEEDIT                                        CL*11
01240              FIELD  (AR78PCTI)                                       CL*11
01241              LENGTH (AR78PCT-LENGTH)                                 CL*11
01242          END-EXEC                                                    CL*11
01243          IF AR78PCTI NUMERIC                                         CL*11
01244              MOVE AL-UNNON       TO  AR78PCTA                        CL*11
01245              MOVE AR78PCTI       TO  WS-IBNR-R78-PCT                 CL*11
01246                                      AR78PCTO                        CL*11
01247            ELSE                                                      CL*11
01248              MOVE AL-UNBON       TO  AR78PCTA                        CL*11
01249              MOVE -1             TO  AR78PCTL                        CL*11
01250              MOVE ER-0106        TO  EMI-ERROR                       CL*11
01251              PERFORM 9900-ERROR-FORMAT.                              CL*11
01252                                                                      CL*11
01253 *    NOTE *******************************************************    CL*11
01254 *         *      EDIT THE IBNR UNEARNED PRORATA PERCENT         *    CL*11
01255 *         *******************************************************.   CL*11
01256      IF APROPCTL GREATER ZERO                                        CL*11
01257          EXEC CICS BIF DEEDIT                                        CL*11
01258              FIELD  (APROPCTI)                                       CL*11
01259              LENGTH (APROPCT-LENGTH)                                 CL*11
01260          END-EXEC                                                    CL*11
01261          IF APROPCTI NUMERIC                                         CL*11
01262              MOVE AL-UNNON       TO  APROPCTA                        CL*11
01263              MOVE APROPCTI       TO  WS-IBNR-PRO-PCT                 CL*11
01264                                      APROPCTO                        CL*11
01265            ELSE                                                      CL*11
01266              MOVE AL-UNBON       TO  APROPCTA                        CL*11
01267              MOVE -1             TO  APROPCTL                        CL*11
01268              MOVE ER-0106        TO  EMI-ERROR                       CL*11
01269              PERFORM 9900-ERROR-FORMAT.                              CL*11
01270                                                                      CL*11
01271 *    NOTE *******************************************************    CL*11
01272 *         *      EDIT THE IBNR R78 AND PRO TOTAL PERCENT        *    CL*11
01273 *         *******************************************************.   CL*11
01274      IF WS-IBNR-UEP-PCT NOT = ZEROS                                  CL*11
01275          IF (WS-IBNR-R78-PCT + WS-IBNR-PRO-PCT) NOT = +1.0           CL*11
01276              MOVE AL-UNBON       TO  AR78PCTA                        CL*11
01277                                      APROPCTA                        CL*11
01278              MOVE -1             TO  AR78PCTL                        CL*11
01279                                      APROPCTL                        CL*11
01280              MOVE ER-2308        TO  EMI-ERROR                       CL*11
01281              PERFORM 9900-ERROR-FORMAT.                              CL*11
01282                                                                      CL*11
01283 *    NOTE *******************************************************    CL*11
01284 *         *      EDIT THE LIMITS - QUOTED/CALCULATED AMOUNT.    * EL105
01285 *         *******************************************************.EL105
01286      IF ALQCAL GREATER ZERO                                       EL105
01287          EXEC CICS BIF DEEDIT                                     EL105
01288              FIELD  (ALQCAI)                                      EL105
01289              LENGTH (ALQCA-LENGTH)                                EL105
01290          END-EXEC                                                 EL105
01291          IF ALQCAI NUMERIC                                        EL105
01292              MOVE AL-UNNON       TO  ALQCAA                       EL105
01293              MOVE ALQCAI         TO  ALQCAO                       EL105
01294            ELSE                                                   EL105
01295              MOVE AL-UNBON       TO  ALQCAA                       EL105
01296              MOVE -1             TO  ALQCAL                       EL105
01297              MOVE ER-0118        TO EMI-ERROR                     EL105
01298              PERFORM 9900-ERROR-FORMAT.                           EL105
01299                                                                   EL105
01300 *    NOTE ******************************************************* EL105
01301 *         *      EDIT THE LIMITS - MAXIMUM REGULAR PAYMENT.     * EL105
01302 *         *******************************************************.EL105
01303      IF ALMRPL GREATER ZERO                                       EL105
01304          EXEC CICS BIF DEEDIT                                     EL105
01305              FIELD  (ALMRPI)                                      EL105
01306              LENGTH (ALMRP-LENGTH)                                EL105
01307          END-EXEC                                                 EL105
01308          IF ALMRPI NUMERIC                                        EL105
01309              MOVE AL-UNNON       TO  ALMRPA                       EL105
01310              MOVE ALMRPI         TO  ALMRPO                       EL105
01311            ELSE                                                   EL105
01312              MOVE AL-UNBON       TO  ALMRPA                       EL105
01313              MOVE -1             TO  ALMRPL                       EL105
01314              MOVE ER-0119        TO EMI-ERROR                     EL105
01315              PERFORM 9900-ERROR-FORMAT.                           EL105
01316                                                                   EL105
01317 *    NOTE ******************************************************* EL105
01318 *         *      EDIT THE LIMITS - QUOTED/CALCULATION DAYS.     * EL105
01319 *         *******************************************************.EL105
01320      IF ALQCDL GREATER ZERO                                       EL105
01321          EXEC CICS BIF DEEDIT                                     EL105
01322              FIELD  (ALQCDI)                                      EL105
01323              LENGTH (ALQCD-LENGTH)                                EL105
01324          END-EXEC                                                 EL105
01325          IF ALQCDI NUMERIC                                        EL105
01326              MOVE AL-UNNON       TO  ALQCDA                       EL105
01327              MOVE ALQCDI         TO  ALQCDO                       EL105
01328            ELSE                                                   EL105
01329              MOVE AL-UNBON       TO  ALQCDA                       EL105
01330              MOVE -1             TO  ALQCDL                       EL105
01331              MOVE ER-0120        TO EMI-ERROR                     EL105
01332              PERFORM 9900-ERROR-FORMAT.                           EL105
01333                                                                   EL105
01334 *    NOTE ******************************************************* EL105
01335 *         *      EDIT THE LIMITS - MAXIMUM DAYS PER PAYMENT.    * EL105
01336 *         *******************************************************.EL105
01337      IF ALMDPPL GREATER ZERO                                      EL105
01338          EXEC CICS BIF DEEDIT                                     EL105
01339              FIELD  (ALMDPPI)                                     EL105
01340              LENGTH (ALMDPP-LENGTH)                               EL105
01341          END-EXEC                                                 EL105
01342          IF ALMDPPI NUMERIC                                       EL105
01343              MOVE AL-UNNON       TO  ALMDPPA                      EL105
01344              MOVE ALMDPPI        TO  ALMDPPO                      EL105
01345            ELSE                                                   EL105
01346              MOVE AL-UNBON       TO  ALMDPPA                      EL105
01347              MOVE -1             TO  ALMDPPL                      EL105
01348              MOVE ER-0121        TO EMI-ERROR                     EL105
01349              PERFORM 9900-ERROR-FORMAT.                           EL105
01350                                                                   EL105
01351 *    NOTE ******************************************************* EL105
01352 *         *      EDIT THE LIMITS - DAYS BEFORE CLOSED.          * EL105
01353 *         *******************************************************.EL105
01354      IF ALDBCL GREATER ZERO                                       EL105
01355          EXEC CICS BIF DEEDIT                                     EL105
01356              FIELD  (ALDBCI)                                      EL105
01357              LENGTH (ALDBC-LENGTH)                                EL105
01358          END-EXEC                                                 EL105
01359          IF ALDBCI NUMERIC                                        EL105
01360              MOVE AL-UNNON       TO  ALDBCA                       EL105
01361              MOVE ALDBCI         TO  ALDBCO                       EL105
01362            ELSE                                                   EL105
01363              MOVE AL-UNBON       TO  ALDBCA                       EL105
01364              MOVE -1             TO  ALDBCL                       EL105
01365              MOVE ER-0122        TO EMI-ERROR                     EL105
01366              PERFORM 9900-ERROR-FORMAT.                           EL105
01367                                                                   EL105
01368 *    NOTE ******************************************************* EL105
01369 *         *      EDIT THE LIMITS - MAXIMUM AUTOMATIC PAYMENT.   * EL105
01370 *         *******************************************************.EL105
01371      IF ALMAPL GREATER ZERO                                       EL105
01372          EXEC CICS BIF DEEDIT                                     EL105
01373              FIELD  (ALMAPI)                                      EL105
01374              LENGTH (ALMAP-LENGTH)                                EL105
01375          END-EXEC                                                 EL105
01376          IF ALMAPI NUMERIC                                        EL105
01377              MOVE AL-UNNON       TO  ALMAPA                       EL105
01378              MOVE ALMAPI         TO  ALMAPO                       EL105
01379            ELSE                                                   EL105
01380              MOVE AL-UNBON       TO  ALMAPA                       EL105
01381              MOVE -1             TO  ALMAPL                       EL105
01382              MOVE ER-0123            TO EMI-ERROR                 EL105
01383              PERFORM 9900-ERROR-FORMAT.                           EL105
01384                                                                   EL105
01385 *    NOTE ******************************************************* EL105
01386 *         *      EDIT THE LIMITS - MONTHS BEFORE PURGED.        * EL105
01387 *         *******************************************************.EL105
01388      IF ALMBPL GREATER ZERO                                       EL105
01389          EXEC CICS BIF DEEDIT                                     EL105
01390              FIELD  (ALMBPI)                                      EL105
01391              LENGTH (ALMBP-LENGTH)                                EL105
01392          END-EXEC                                                 EL105
01393          IF ALMBPI NUMERIC                                        EL105
01394              MOVE AL-UNNON       TO  ALMBPA                       EL105
01395              MOVE ALMBPI         TO  ALMBPO                       EL105
01396            ELSE                                                   EL105
01397              MOVE AL-UNBON       TO  ALMBPA                       EL105
01398              MOVE -1             TO  ALMBPL                       EL105
01399              MOVE ER-0124        TO EMI-ERROR                     EL105
01400              PERFORM 9900-ERROR-FORMAT.                           EL105
01401                                                                   EL105
01402 *    NOTE ******************************************************* EL105
01403 *         *      EDIT THE LIMITS - MAXIMUM AUTO-PAY MONTHS.     * EL105
01404 *         *******************************************************.EL105
01405      IF ALMAPML GREATER ZERO                                      EL105
01406          EXEC CICS BIF DEEDIT                                     EL105
01407              FIELD  (ALMAPMI)                                     EL105
01408              LENGTH (ALMAPM-LENGTH)                               EL105
01409          END-EXEC                                                 EL105
01410          IF ALMAPMI NUMERIC                                       EL105
01411              MOVE AL-UNNON       TO  ALMAPMA                      EL105
01412              MOVE ALMAPMI        TO  ALMAPMO                      EL105
01413            ELSE                                                   EL105
01414              MOVE AL-UNBON       TO  ALMAPMA                      EL105
01415              MOVE -1             TO  ALMAPML                      EL105
01416              MOVE ER-0125        TO EMI-ERROR                     EL105
01417              PERFORM 9900-ERROR-FORMAT.                           EL105
01418                                                                   EL105
01419  4900-EXIT.                                                       EL105
01420      EXIT.                                                        EL105
01421                                                                   EL105
01422      EJECT                                                        EL105
01423  5000-ADD-RECORD SECTION.                                         EL105
01424      EXEC CICS GETMAIN                                            EL105
01425          SET     (ADDRESS OF CONTROL-FILE)                           CL*13
01426          LENGTH  (750)                                               CL**9
01427          INITIMG (GETMAIN-SPACE)                                  EL105
01428      END-EXEC.                                                    EL105
01429                                                                   EL105
01430      MOVE 'CF'                   TO  CF-RECORD-ID.                EL105
01431                                                                   EL105
01432      MOVE PI-COMPANY-ID          TO  CF-COMPANY-ID.               EL105
01433      MOVE '6'                    TO  CF-RECORD-TYPE.              EL105
01434      MOVE PI-CARRIER-NUMBER      TO  CF-CARRIER-CNTL.             EL105
01435      MOVE ZERO                   TO  CF-SEQUENCE-NO.              EL105
01436                                                                   EL105
01437      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.        EL105
01438      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL105
01439      MOVE '5'                    TO  DC-OPTION-CODE.              EL105
01440      PERFORM 8500-DATE-CONVERSION.                                EL105
01441      MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.            EL105
01442      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL105
01443                                                                   EL105
01444      MOVE ZERO                   TO  CF-ZIP-CODE                  EL105
01445                                      CF-ZIP-CODE-NUM                 CL**7
01446                                      CF-PHONE-NO                  EL105
01447                                      CF-CLAIM-COUNTER             EL105
01448                                      CF-CHECK-COUNTER             EL105
01449                                      CF-EXPENSE-PERCENT           EL105
01450                                      CF-EXPENSE-DOLLAR            EL105
01451                                      CF-PERCENT-OF-CDT            EL105
01452                                      CF-IBNR-PERCENT              EL105
01453                                      CF-IBNR-UEPRM-PERCENT           CL*11
01454                                      CF-IBNR-R78-PERCENT             CL*11
01455                                      CF-IBNR-PRO-PERCENT             CL*11
01456                                      CF-CALC-AMT-TOL              EL105
01457                                      CF-MAX-REG-PMT               EL105
01458                                      CF-MAX-REG-DAYS              EL105
01459                                      CF-MAX-AUTO-PMT              EL105
01460                                      CF-MAX-AUTO-MOS              EL105
01461                                      CF-CALC-DAYS-TOL             EL105
01462                                      CF-DAYS-BEFORE-CLOSED        EL105
01463                                      CF-MONTHS-BEFORE-PURGED         CL**5
01464                                      CF-CR-TOL-PREM                  CL**5
01465                                      CF-CR-TOL-REFUND                CL**5
01466                                      CF-CR-TOL-PREM-PCT              CL**5
01467                                      CF-CR-TOL-REFUND-PCT
092705                                     CF-CARRIER-CLP-TOL-PCT
092705                                     CF-CARRIER-LEASE-COMM
032813                                     CF-CARRIER-NEXT-AUDIT-CHK-NO
01468                                                                   EL105
01469      MOVE '1'                    TO  CF-CLAIM-NO-METHOD           EL105
01470                                      CF-CHECK-NO-METHOD           EL105
01471                                      CF-EXPENSE-METHOD            EL105
01472                                      CF-CDT-ACCESS-METHOD         EL105
01473                                      CF-CLAIM-CALC-METHOD.        EL105
01474                                                                   EL105
01475      PERFORM 5900-MOVE-MAP-DATA.                                  EL105
01476                                                                   EL105
01477      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL105
01478      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL105
01479                                                                   EL105
01480      EXEC CICS WRITE                                              EL105
01481          DATASET (WS-CONTROL-FILE-DSID)                           EL105
01482          RIDFLD  (CF-CONTROL-PRIMARY)                             EL105
01483          FROM    (CONTROL-FILE)                                   EL105
01484      END-EXEC.                                                    EL105
01485                                                                   EL105
01486      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL105
01487                                                                   EL105
01488  5090-EXIT.                                                       EL105
01489      EXIT.                                                        EL105
01490                                                                   EL105
01491      EJECT                                                        EL105
01492  5100-CHANGE-RECORD SECTION.                                      EL105
01493      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL105
01494                                                                   EL105
01495      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL105
01496      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.          EL105
01497      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.           EL105
01498      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL105
01499                                                                   EL105
01500      EXEC CICS READ UPDATE                                        EL105
01501          DATASET (WS-CONTROL-FILE-DSID)                           EL105
01502          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL105
01503          SET     (ADDRESS OF CONTROL-FILE)                           CL*13
01504      END-EXEC.                                                    EL105
01505                                                                   EL105
01506      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL105
01507      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL105
01508                                                                   EL105
01509      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL105
01510                                                                   EL105
01511      PERFORM 5900-MOVE-MAP-DATA.                                  EL105
01512                                                                   EL105
01513      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL105
01514      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL105
01515                                                                   EL105
01516      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.        EL105
01517      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL105
01518      MOVE '5'                    TO  DC-OPTION-CODE.              EL105
01519      PERFORM 8500-DATE-CONVERSION.                                EL105
01520      MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.            EL105
01521      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL105
01522                                                                   EL105
01523      EXEC CICS REWRITE                                            EL105
01524          DATASET (WS-CONTROL-FILE-DSID)                           EL105
01525          FROM    (CONTROL-FILE)                                   EL105
01526      END-EXEC.                                                    EL105
01527                                                                   EL105
01528      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL105
01529                                                                   EL105
01530  5190-EXIT.                                                       EL105
01531      EXIT.                                                        EL105
01532                                                                   EL105
01533      EJECT                                                        EL105
01534  5200-DELETE-RECORD SECTION.                                      EL105
01535      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL105
01536                                                                   EL105
01537      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL105
01538      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.          EL105
01539      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.           EL105
01540      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL105
01541                                                                   EL105
01542      EXEC CICS READ UPDATE                                        EL105
01543          DATASET (WS-CONTROL-FILE-DSID)                           EL105
01544          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL105
01545          SET     (ADDRESS OF CONTROL-FILE)                           CL*13
01546      END-EXEC.                                                    EL105
01547                                                                   EL105
01548      MOVE 'D'                    TO  JP-RECORD-TYPE.              EL105
01549      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL105
01550                                                                   EL105
01551      EXEC CICS DELETE                                             EL105
01552          DATASET (WS-CONTROL-FILE-DSID)                           EL105
01553      END-EXEC.                                                    EL105
01554                                                                   EL105
01555      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL105
01556                                                                   EL105
01557  5290-EXIT.                                                       EL105
01558      EXIT.                                                        EL105
01559                                                                   EL105
01560      EJECT                                                        EL105
01561  5900-MOVE-MAP-DATA SECTION.                                      EL105
01562                                                                      CL**5
01563      IF NOT CREDIT-SESSION                                           CL**5
01564          GO TO 5910-NOT-CREDIT-SESSION.                              CL*15
01565                                                                      CL**5
112103     IF BSECPAYL GREATER ZERO  
112103         MOVE BSECPAYI       TO  CF-SECPAY-SWITCH
112103     END-IF.

112103
01566      IF BCONAMEL GREATER ZERO                                        CL**5
01567          MOVE BCONAMEI           TO  CF-MAIL-TO-NAME.                CL**5
01568                                                                      CL**5
01569      IF BCAREOFL GREATER ZERO                                        CL**5
01570          MOVE BCAREOFI           TO  CF-IN-CARE-OF.                  CL**5
01571                                                                      CL**5
01572      IF BADDR1L GREATER ZERO                                         CL**5
01573          MOVE BADDR1I            TO  CF-ADDRESS-LINE-1.              CL**5
01574                                                                      CL**5
01575      IF BADDR2L GREATER ZERO                                         CL**5
01576          MOVE BADDR2I            TO  CF-ADDRESS-LINE-2.              CL**5
01577                                                                      CL**5
01578      IF BCITYSTL GREATER ZERO                                        CL**5
01579          MOVE BCITYSTI           TO  CF-CITY-STATE.                  CL**5
01580                                                                      CL**5
01581      IF BPHONEL GREATER ZERO                                         CL**5
01582          EXEC CICS BIF DEEDIT                                        CL**5
01583              FIELD  (BPHONEI)                                        CL**5
01584              LENGTH (APHONE-LENGTH)                                  CL**5
01585          END-EXEC                                                    CL**5
01586          MOVE BPHONEI            TO  CF-PHONE-NO                     CL**5
01587                                      BPHONEO                         CL**5
01588          INSPECT BPHONEO CONVERTING SPACES TO '-'.                   CL*14
01589                                                                      CL**5
01590      IF CF-ZIP-CODE-NUM NUMERIC  AND                                 CL**7
01591         CF-ZIP-CODE-NUM NOT = ZEROS                                  CL**7
01592          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM                  CL**7
01593          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.                     CL**7
01594                                                                      CL**7
01595      IF BZIPL NOT = ZERO                                             CL**7
01596          MOVE BZIPI              TO  WS-ZIP-CODE                     CL**7
01597          MOVE ZEROS              TO  CF-ZIP-CODE-NUM                 CL**7
01598      ELSE                                                            CL**7
01599          GO TO 5905-CONTINUE.                                        CL**7
01600                                                                      CL**7
01601      IF WS-CANADIAN-ZIP                                              CL**7
01602          IF WS-ZIP-4 = SPACE  OR  '-'                                CL**7
01603              MOVE WS-ZIP-CAN-2-POST1   TO CF-CAN-POSTAL-1            CL**7
01604              MOVE WS-ZIP-CAN-2-POST2   TO CF-CAN-POSTAL-2            CL**7
01605          ELSE                                                        CL**7
01606              MOVE WS-ZIP-CAN-1-POST1   TO CF-CAN-POSTAL-1            CL**7
01607              MOVE WS-ZIP-CAN-1-POST2   TO CF-CAN-POSTAL-2            CL**7
01608      ELSE                                                            CL**7
01609          IF WS-ZIP-6 = SPACE  OR  '-'                                CL**7
01610              MOVE WS-ZIP-AM-2-CODE     TO CF-ZIP-PRIME               CL**7
01611              MOVE WS-ZIP-AM-2-PLUS4    TO CF-ZIP-PLUS4               CL**7
01612          ELSE                                                        CL**7
01613              MOVE WS-ZIP-AM-1-CODE     TO CF-ZIP-PRIME               CL**7
01614              MOVE WS-ZIP-AM-1-PLUS4    TO CF-ZIP-PLUS4.              CL**7
01615                                                                      CL**7
01616  5905-CONTINUE.                                                      CL**7
01617                                                                      CL**5
01618      IF BPRMTOLL GREATER ZERO                                        CL**5
01619          EXEC CICS BIF DEEDIT                                        CL**5
01620              FIELD  (BPRMTOLI)                                       CL**5
01621              LENGTH (6)                                              CL**5
01622          END-EXEC                                                    CL**5
01623          MOVE BPRMTOLI           TO  CF-CR-TOL-PREM.                 CL**5
01624                                                                      CL**5
01625      IF BREFTOLL GREATER ZERO                                        CL**5
01626          EXEC CICS BIF DEEDIT                                        CL**5
01627              FIELD  (BREFTOLI)                                       CL**5
01628              LENGTH (6)                                              CL**5
01629          END-EXEC                                                    CL**5
01630          MOVE BREFTOLI           TO  CF-CR-TOL-REFUND.               CL**5
01631                                                                      CL**5
01632      IF BOVSAMTL GREATER ZERO                                        CL*17
01633          EXEC CICS BIF DEEDIT                                        CL*17
01634              FIELD  (BOVSAMTI)                                       CL*17
01635              LENGTH (6)                                              CL*17
01636          END-EXEC                                                    CL*17
01637          MOVE BOVSAMTI           TO  CF-CR-OVR-SHT-AMT               CL*17
01638      END-IF.                                                         CL*17
01639                                                                      CL*17
01640      IF BPRMPCTL GREATER ZERO                                        CL**5
01641          MOVE WS-TOL-PREM-PCT    TO  CF-CR-TOL-PREM-PCT.             CL**5
01642                                                                      CL**5
01643      IF BREFPCTL GREATER ZERO                                        CL**5
01644          MOVE WS-TOL-REF-PCT     TO  CF-CR-TOL-REFUND-PCT.           CL**5
01645                                                                      CL*17
01646      IF BOVSPCTL > 0                                                 CL*17
01647          MOVE WS-CR-OVR-SHT-PCT  TO  CF-CR-OVR-SHT-PCT               CL*17
01648      END-IF.                                                         CL*17
01649                                                                      CL**6
01650      IF BDOMSTL GREATER ZERO                                         CL**6
01651          MOVE BDOMSTI            TO  CF-DOMICILE-STATE.              CL**6
112103                                                                     CL**6
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE ZERO               TO  CF-CARRIER-CLP-TOL-PCT
092705                                     CF-CARRIER-LEASE-COMM
112103     ELSE
112103         IF BCLPTOLL GREATER ZERO
112103             MOVE WS-CLP-TOL-PCT TO  CF-CARRIER-CLP-TOL-PCT
112103*        ELSE
112103*            MOVE ZERO           TO  CF-CARRIER-CLP-TOL-PCT
112103         END-IF
092705         IF BLCOMML > ZEROS
092705            MOVE WS-SPP-LEASE-COMM TO CF-CARRIER-LEASE-COMM
092705         END-IF
112103     END-IF
01652                                                                      CL**5
01653 *    NOTE *******************************************************    CL*13
01654 * DMD CUSTOM CODE        EDIT CALCULATE PREMIUM FLAG            *    CL*13
01655 *         *******************************************************.   CL*13
01656      IF CREDIT-SESSION                                               CL*13
01657       IF PI-COMPANY-ID NOT = 'DMD'                                   CL*13
01658         MOVE 'Y'                 TO BCLCPRMI                         CL*14
01659         MOVE +1                  TO BCLCPRML.                        CL*14
01660                                                                      CL*13
01661      IF CREDIT-SESSION                                               CL*13
01662         IF BCLCPRMI = 'Y' OR 'N'                                     CL*16
01663            MOVE BCLCPRMI       TO CF-RATING-SWITCH                   CL*16
01664         ELSE                                                         CL*16
01665            MOVE AL-UABON       TO BCLCPRMA                           CL*16
01666            MOVE -1             TO BCLCPRML                           CL*16
01667            MOVE ER-8017        TO EMI-ERROR                          CL*16
01668            PERFORM 9900-ERROR-FORMAT                                 CL*16
01669            PERFORM 8200-SEND-DATAONLY                                CL*16
01670            GO TO 9100-RETURN-TRAN.                                   CL*16
01671                                                                      CL*13
01672      GO TO 5990-EXIT.                                                CL**5
01673                                                                      CL**5
01674  5910-NOT-CREDIT-SESSION.                                            CL*15

112103*    IF ASECPAYL GREATER ZERO                                        CL**5
112103*        MOVE ASECPAYI           TO  CF-SECPAY-SWITCH.               CL**5
112103
01675      IF ACONAMEL GREATER ZERO                                     EL105
01676          MOVE ACONAMEI           TO  CF-MAIL-TO-NAME.             EL105
01677                                                                   EL105
01678      IF ACAREOFL GREATER ZERO                                     EL105
01679          MOVE ACAREOFI           TO  CF-IN-CARE-OF.               EL105
01680                                                                   EL105
01681      IF AADDR1L GREATER ZERO                                      EL105
01682          MOVE AADDR1I            TO  CF-ADDRESS-LINE-1.           EL105
01683                                                                   EL105
01684      IF AADDR2L GREATER ZERO                                      EL105
01685          MOVE AADDR2I            TO  CF-ADDRESS-LINE-2.           EL105
01686                                                                   EL105
01687      IF ACITYSTL GREATER ZERO                                     EL105
01688          MOVE ACITYSTI           TO  CF-CITY-STATE.               EL105
01689                                                                   EL105
01690      IF APHONEL GREATER ZERO                                      EL105
01691          EXEC CICS BIF DEEDIT                                     EL105
01692              FIELD  (APHONEI)                                     EL105
01693              LENGTH (APHONE-LENGTH)                               EL105
01694          END-EXEC                                                 EL105
01695          MOVE APHONEI            TO  CF-PHONE-NO                  EL105
01696                                      APHONEO                      EL105
01697          INSPECT APHONEO CONVERTING SPACES TO '-'.                   CL*14
01698                                                                   EL105
01699      IF CF-ZIP-CODE-NUM NUMERIC  AND                                 CL**7
01700         CF-ZIP-CODE-NUM NOT = ZEROS                                  CL**7
01701          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM                  CL**7
01702          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.                     CL**7
01703                                                                      CL**7
01704      IF AZIPL NOT = ZERO                                             CL**7
01705          MOVE AZIPI              TO  WS-ZIP-CODE                     CL**7
01706          MOVE ZEROS              TO  CF-ZIP-CODE-NUM                 CL**7
01707      ELSE                                                            CL**7
01708          GO TO 5915-CONTINUE.                                        CL**7
01709                                                                      CL**7
01710      IF WS-CANADIAN-ZIP                                              CL**7
01711          IF WS-ZIP-4 = SPACE  OR  '-'                                CL**7
01712              MOVE WS-ZIP-CAN-2-POST1   TO CF-CAN-POSTAL-1            CL**7
01713              MOVE WS-ZIP-CAN-2-POST2   TO CF-CAN-POSTAL-2            CL**7
01714          ELSE                                                        CL**7
01715              MOVE WS-ZIP-CAN-1-POST1   TO CF-CAN-POSTAL-1            CL**7
01716              MOVE WS-ZIP-CAN-1-POST2   TO CF-CAN-POSTAL-2            CL**7
01717      ELSE                                                            CL**7
01718          IF WS-ZIP-6 = SPACE  OR  '-'                                CL**7
01719              MOVE WS-ZIP-AM-2-CODE     TO CF-ZIP-PRIME               CL**7
01720              MOVE WS-ZIP-AM-2-PLUS4    TO CF-ZIP-PLUS4               CL**7
01721          ELSE                                                        CL**7
01722              MOVE WS-ZIP-AM-1-CODE     TO CF-ZIP-PRIME               CL**7
01723              MOVE WS-ZIP-AM-1-PLUS4    TO CF-ZIP-PLUS4.              CL**7
01724                                                                      CL**7
01725  5915-CONTINUE.                                                      CL**7
01726                                                                   EL105
01727      IF ADOMSTL GREATER ZERO                                      EL105
01728          MOVE ADOMSTI            TO  CF-DOMICILE-STATE.           EL105
112103                                                                  EL105
112103*    IF PI-COMPANY-ID = 'CID' 
112103*        MOVE ZERO               TO  CF-CARRIER-CLP-TOL-PCT
112103*    ELSE
112103*        IF ACLPTOLL GREATER ZERO  
112103*            MOVE ACLPTOLI       TO  CF-CARRIER-CLP-TOL-PCT
112103*        ELSE
112103*            MOVE ZERO           TO  CF-CARRIER-CLP-TOL-PCT
112103*        END-IF
112103*    END-IF.
01729                                                                   EL105
01730      IF CLAIM-SESSION                                                CL*13
01731         IF ALPHCHL GREATER ZERO                                      CL*13
01732             IF ALPHCHI ALPHABETIC                                    CL*13
01733                MOVE AL-UANON       TO ALPHCHA                        CL*13
01734                MOVE  ALPHCHI       TO CF-LAST-ALPHA-CHARACTER        CL*13
01735             ELSE                                                     CL*13
01736                MOVE AL-UNBON       TO ALPHCHA                        CL*13
01737                MOVE -1             TO ALPHCHL                        CL*13
01738                MOVE ER-8128        TO EMI-ERROR                      CL*13
01739                PERFORM 9900-ERROR-FORMAT.                            CL*13
01740                                                                      CL*13
01741      EJECT                                                        EL105
01742      IF ACLNAML GREATER ZERO                                      EL105
01743          MOVE ACLNAMI            TO  CF-CLAIM-NO-METHOD.          EL105
01744                                                                   EL105
01745      IF ACKNAML GREATER ZERO                                      EL105
01746          MOVE ACKNAMI            TO  CF-CHECK-NO-METHOD.          EL105
01747                                                                   EL105
01748      IF ACLAIML GREATER ZERO                                      EL105
01749          EXEC CICS BIF DEEDIT                                     EL105
01750              FIELD  (ACLAIMI)                                     EL105
01751              LENGTH (8)                                           EL105
01752          END-EXEC                                                 EL105
01753          MOVE ACLAIMO            TO  CF-CLAIM-COUNTER.            EL105
01754                                                                   EL105
01755      IF ACHECKL GREATER ZERO                                      EL105
01756          EXEC CICS BIF DEEDIT                                     EL105
01757              FIELD  (ACHECKI)                                     EL105
01758              LENGTH (8)                                           EL105
01759          END-EXEC                                                 EL105
01760          MOVE ACHECKO            TO  CF-CHECK-COUNTER.            EL105
01761                                                                   EL105
01762      IF ALAL GREATER ZERO                                         EL105
01763          MOVE ALAI               TO  CF-LETTER-RESEND-OPT         EL105
01764          INSPECT CF-LETTER-RESEND-OPT CONVERTING 'YN' TO '1 '.       CL*13
01765                                                                   EL105
01766      IF AEXPCML GREATER ZERO                                      EL105
01767          MOVE AEXPCMI            TO  CF-EXPENSE-METHOD.           EL105
01768                                                                   EL105
01769      IF AEXPCPL GREATER ZERO                                      EL105
01770          EXEC CICS BIF DEEDIT                                     EL105
01771              FIELD  (AEXPCPI)                                     EL105
01772              LENGTH (AEXPCP-LENGTH)                               EL105
01773          END-EXEC                                                 EL105
01774          MOVE AEXPCPI            TO  CF-EXPENSE-PERCENT           EL105
01775                                      AEXPCPO.                     EL105
01776                                                                   EL105
01777      IF AEXPCAL GREATER ZERO                                      EL105
01778          EXEC CICS BIF DEEDIT                                     EL105
01779              FIELD  (AEXPCAI)                                     EL105
01780              LENGTH (AEXPCA-LENGTH)                               EL105
01781          END-EXEC                                                 EL105
01782          MOVE AEXPCAI            TO  CF-EXPENSE-DOLLAR            EL105
01783                                      AEXPCAO.                     EL105
01784      IF CLAIM-SESSION                                                CL*13
01785         IF ABRETRL GREATER ZERO                                      CL*15
01786            IF ABRETRI NUMERIC                                        CL*15
01787               MOVE ABRETRI   TO CF-BUILD-RETRIEVE-AFTER-MONTHS       CL*15
01788            ELSE                                                      CL*14
01789               MOVE AL-UNBON           TO ABRETRA                     CL*15
01790               MOVE -1                 TO ABRETRL                     CL*15
01791               MOVE ER-8127            TO EMI-ERROR                   CL*14
01792               PERFORM 9900-ERROR-FORMAT.                             CL*14
01793                                                                   EL105
01794      IF ACLCML GREATER ZERO                                       EL105
01795          MOVE ACLCMI             TO  CF-CLAIM-CALC-METHOD.        EL105
01796                                                                   EL105
01797      IF ARESMANL GREATER ZERO                                     EL105
01798          MOVE ARESMANI           TO  CF-MANUAL-SW                 EL105
01799          INSPECT CF-MANUAL-SW CONVERTING 'YN' TO '1 '.               CL*13
01800                                                                   EL105
01801      IF ARESCDTL GREATER ZERO                                     EL105
01802          MOVE ARESCDTI           TO  CF-FUTURE-SW                 EL105
01803          INSPECT CF-FUTURE-SW CONVERTING 'YN' TO '1 '.               CL*13
01804                                                                   EL105
01805      IF ARESIBNL GREATER ZERO                                     EL105
01806          MOVE ARESIBNI           TO  CF-IBNR-SW                   EL105
01807          INSPECT CF-IBNR-SW CONVERTING 'YN' TO '1 '.                 CL*13
01808                                                                   EL105
01809      IF ARESPTCL GREATER ZERO                                     EL105
01810          MOVE ARESPTCI           TO  CF-PTC-SW                    EL105
01811                                      CF-PTC-LF-SW                    CL**3
01812          INSPECT CF-PTC-SW CONVERTING 'YN' TO '1 '                   CL*13
01813          INSPECT CF-PTC-LF-SW CONVERTING 'YN' TO '1 '.               CL*13
01814                                                                   EL105
01815      IF ACDTAL GREATER ZERO                                       EL105
01816          MOVE ACDTAI             TO  CF-CDT-ACCESS-METHOD.        EL105
01817                                                                   EL105
01818      IF APCTCDTL GREATER ZERO                                     EL105
01819          EXEC CICS BIF DEEDIT                                     EL105
01820              FIELD  (APCTCDTI)                                    EL105
01821              LENGTH (APCTCDT-LENGTH)                              EL105
01822          END-EXEC                                                 EL105
01823          MOVE APCTCDTI           TO  CF-PERCENT-OF-CDT            EL105
01824                                      APCTCDTO.                    EL105
01825                                                                   EL105
01826      IF IBNRPCTL GREATER ZERO                                     EL105
01827          EXEC CICS BIF DEEDIT                                     EL105
01828              FIELD  (IBNRPCTI)                                    EL105
01829              LENGTH (IBNRPCT-LENGTH)                              EL105
01830          END-EXEC                                                 EL105
01831          MOVE IBNRPCTI           TO  CF-IBNR-PERCENT              EL105
01832                                      IBNRPCTO.                    EL105
01833                                                                   EL105
01834      IF AUEPPCTL GREATER ZERO                                        CL*11
01835          EXEC CICS BIF DEEDIT                                        CL*11
01836              FIELD  (AUEPPCTI)                                       CL*11
01837              LENGTH (AUEPPCT-LENGTH)                                 CL*11
01838          END-EXEC                                                    CL*11
01839          MOVE AUEPPCTI           TO  CF-IBNR-UEPRM-PERCENT           CL*11
01840                                      AUEPPCTO.                       CL*11
01841                                                                      CL*11
01842      IF AR78PCTL GREATER ZERO                                        CL*11
01843          EXEC CICS BIF DEEDIT                                        CL*11
01844              FIELD  (AR78PCTI)                                       CL*11
01845              LENGTH (AR78PCT-LENGTH)                                 CL*11
01846          END-EXEC                                                    CL*11
01847          MOVE AR78PCTI           TO  CF-IBNR-R78-PERCENT             CL*11
01848                                      AR78PCTO.                       CL*11
01849                                                                      CL*11
01850      IF APROPCTL GREATER ZERO                                        CL*11
01851          EXEC CICS BIF DEEDIT                                        CL*11
01852              FIELD  (APROPCTI)                                       CL*11
01853              LENGTH (APROPCT-LENGTH)                                 CL*11
01854          END-EXEC                                                    CL*11
01855          MOVE APROPCTI           TO  CF-IBNR-PRO-PERCENT             CL*11
01856                                      APROPCTO.                       CL*11
01857                                                                      CL*11
01858      IF ALQCAL GREATER ZERO                                       EL105
01859          EXEC CICS BIF DEEDIT                                     EL105
01860              FIELD  (ALQCAI)                                      EL105
01861              LENGTH (ALQCA-LENGTH)                                EL105
01862          END-EXEC                                                 EL105
01863          MOVE ALQCAI             TO  CF-CALC-AMT-TOL              EL105
01864                                      ALQCAO.                      EL105
01865                                                                   EL105
01866      IF ALMRPL GREATER ZERO                                       EL105
01867          EXEC CICS BIF DEEDIT                                     EL105
01868              FIELD  (ALMRPI)                                      EL105
01869              LENGTH (ALMRP-LENGTH)                                EL105
01870          END-EXEC                                                 EL105
01871          MOVE ALMRPI             TO  CF-MAX-REG-PMT               EL105
01872                                      ALMRPO.                      EL105
01873                                                                   EL105
01874      IF ALQCDL GREATER ZERO                                       EL105
01875          EXEC CICS BIF DEEDIT                                     EL105
01876              FIELD  (ALQCDI)                                      EL105
01877              LENGTH (ALQCD-LENGTH)                                EL105
01878          END-EXEC                                                 EL105
01879          MOVE ALQCDI             TO  CF-CALC-DAYS-TOL             EL105
01880                                      ALQCDO.                      EL105
01881      IF ALMDPPL GREATER ZERO                                      EL105
01882          EXEC CICS BIF DEEDIT                                     EL105
01883              FIELD  (ALMDPPI)                                     EL105
01884              LENGTH (ALMDPP-LENGTH)                               EL105
01885          END-EXEC                                                 EL105
01886          MOVE ALMDPPI            TO  CF-MAX-REG-DAYS              EL105
01887                                      ALMDPPO.                     EL105
01888                                                                   EL105
01889      IF ALDBCL GREATER ZERO                                       EL105
01890          EXEC CICS BIF DEEDIT                                     EL105
01891              FIELD  (ALDBCI)                                      EL105
01892              LENGTH (ALDBC-LENGTH)                                EL105
01893          END-EXEC                                                 EL105
01894          MOVE ALDBCI             TO  CF-DAYS-BEFORE-CLOSED        EL105
01895                                      ALDBCO.                      EL105
01896                                                                   EL105
01897      IF ALMAPL GREATER ZERO                                       EL105
01898          EXEC CICS BIF DEEDIT                                     EL105
01899              FIELD  (ALMAPI)                                      EL105
01900              LENGTH (ALMAP-LENGTH)                                EL105
01901          END-EXEC                                                 EL105
01902          MOVE ALMAPI             TO  CF-MAX-AUTO-PMT              EL105
01903                                      ALMAPO.                      EL105
01904                                                                   EL105
01905      IF ALMBPL GREATER ZERO                                       EL105
01906          EXEC CICS BIF DEEDIT                                     EL105
01907              FIELD  (ALMBPI)                                      EL105
01908              LENGTH (ALMBP-LENGTH)                                EL105
01909          END-EXEC                                                 EL105
01910          MOVE ALMBPI             TO  CF-MONTHS-BEFORE-PURGED      EL105
01911                                      ALMBPO.                      EL105
01912                                                                   EL105
01913      IF ALMAPML GREATER ZERO                                      EL105
01914          EXEC CICS BIF DEEDIT                                     EL105
01915              FIELD  (ALMAPMI)                                     EL105
01916              LENGTH (ALMAPM-LENGTH)                               EL105
01917          END-EXEC                                                 EL105
01918          MOVE ALMAPMI            TO  CF-MAX-AUTO-MOS              EL105
01919                                      ALMAPMO.                     EL105
01920                                                                   EL105
01921  5990-EXIT.                                                       EL105
01922      EXIT.                                                        EL105
01923  EJECT                                                               CL*11
01924  6000-SHOW-RECORD SECTION.                                        EL105
01925      EXEC CICS HANDLE CONDITION                                   EL105
01926          NOTFND  (6060-NOT-FOUND)                                 EL105
01927      END-EXEC.                                                    EL105
01928                                                                   EL105
01929      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL105
01930                                                                   EL105
01931      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL105
01932      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.          EL105
01933      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.           EL105
01934      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL105
01935                                                                   EL105
01936      EXEC CICS READ                                               EL105
01937          DATASET (WS-CONTROL-FILE-DSID)                           EL105
01938          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL105
01939          SET     (ADDRESS OF CONTROL-FILE)                           CL*13
01940      END-EXEC.                                                    EL105
01941                                                                   EL105
01942      IF NOT CREDIT-SESSION                                           CL**5
01943          GO TO 6010-NOT-CREDIT-SESSION.                              CL*15
01944                                                                      CL**5
01945      MOVE CF-CARRIER-CNTL        TO  BCARIERO.                       CL**5
01946      MOVE AL-UANON               TO  BCARIERA.                       CL**5
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF           TO  BSPLABLA
112103         MOVE AL-SADOF           TO  BSECPAYA
112103     ELSE
112103         MOVE CF-SECPAY-SWITCH   TO  BSECPAYO
112103     END-IF.
112103     
01947      MOVE CF-MAIL-TO-NAME        TO  BCONAMEO.                       CL**5
01948      MOVE CF-IN-CARE-OF          TO  BCAREOFO.                       CL**5
01949      MOVE CF-ADDRESS-LINE-1      TO  BADDR1O.                        CL*15
01950      MOVE CF-ADDRESS-LINE-2      TO  BADDR2O.                        CL*15
01951      MOVE CF-CITY-STATE          TO  BCITYSTO.                       CL**5
01952                                                                      CL*13
01953      IF CREDIT-SESSION                                               CL*13
01954         MOVE CF-RATING-SWITCH    TO  BCLCPRMO.                       CL*13
01955                                                                      CL**7
01956      IF CF-ZIP-CODE-NUM NUMERIC  AND                                 CL**7
01957         CF-ZIP-CODE-NUM NOT = ZEROS                                  CL**7
01958          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM                  CL**7
01959          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.                     CL**7
01960                                                                      CL**7
01961      MOVE SPACES                   TO WS-ZIP-CODE.                   CL**7
01962      IF CF-CANADIAN-POST-CODE                                        CL**7
01963          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1             CL**7
01964          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2             CL**7
01965      ELSE                                                            CL**7
01966          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE               CL**7
01967          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL**7
01968              MOVE '-'              TO WS-ZIP-AM-2-DASH               CL**7
01969              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.             CL**7
01970                                                                      CL**7
01971      MOVE WS-ZIP-CODE            TO  BZIPO.                          CL**7
01972                                                                      CL**7
01973      MOVE CF-DOMICILE-STATE      TO  BDOMSTO.                        CL**5
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF               TO  BCTLABLA
112103                                         BCLPTOLA
092705                                         BLCOMMA
112103     ELSE
112103         MOVE CF-CARRIER-CLP-TOL-PCT TO  BCLPTOLO
092705         MOVE CF-CARRIER-LEASE-COMM  TO  BLCOMMO
112103     END-IF.
01974      MOVE CF-PHONE-NO            TO  BPHONEO.                        CL**5
01975      INSPECT BPHONEO CONVERTING SPACES TO '-'.                       CL*14
01976                                                                      CL**5
01977      IF CF-CR-TOL-PREM NUMERIC                                       CL**5
01978          IF CF-CR-TOL-PREM NOT = ZEROS                               CL**5
01979              MOVE CF-CR-TOL-PREM        TO  BPRMTOLO.                CL**5
01980                                                                      CL**5
01981      IF CF-CR-TOL-REFUND NUMERIC                                     CL**5
01982          IF CF-CR-TOL-REFUND NOT = ZEROS                             CL**5
01983              MOVE CF-CR-TOL-REFUND      TO  BREFTOLO.                CL**5
01984                                                                      CL**5
01985      IF CF-CR-OVR-SHT-AMT  NUMERIC                                   CL*17
01986           IF CF-CR-OVR-SHT-AMT > +0                                  CL*17
01987               MOVE CF-CR-OVR-SHT-AMT TO BOVSAMTO                     CL*17
01988           END-IF                                                     CL*17
01989      END-IF.                                                         CL*17
01990                                                                      CL*17
01991      IF CF-CR-TOL-PREM-PCT NUMERIC                                   CL**5
01992          IF CF-CR-TOL-PREM-PCT NOT = ZEROS                           CL**5
01993              MOVE CF-CR-TOL-PREM-PCT    TO  BPRMPCTO.                CL**5
01994                                                                      CL**5
01995      IF CF-CR-TOL-REFUND-PCT NUMERIC                                 CL**5
01996          IF CF-CR-TOL-REFUND-PCT NOT = ZEROS                         CL**5
01997              MOVE CF-CR-TOL-REFUND-PCT  TO  BREFPCTO.                CL**5
01998                                                                      CL*17
01999      IF CF-CR-OVR-SHT-PCT  NUMERIC                                   CL*17
02000           IF CF-CR-OVR-SHT-PCT > +0                                  CL*17
02001              MOVE CF-CR-OVR-SHT-PCT TO  BOVSPCTO                     CL*17
02002           END-IF                                                     CL*17
02003      END-IF.                                                         CL*17
02004                                                                      CL**5
02005      IF CREDIT-SESSION                                               CL*13
02006        MOVE CF-RATING-SWITCH            TO  BCLCPRMO.                CL*13
02007                                                                      CL*13
02008      MOVE AL-UNNON                      TO  BPRMTOLA  BREFTOLA       CL**5
02009                                             BPRMPCTA  BREFPCTA.      CL**5
02010                                                                      CL**5
02011      GO TO 6030-DISPLAY-MAINT.                                       CL**5
02012                                                                      CL**5
02013  6010-NOT-CREDIT-SESSION.                                            CL*15
02014      MOVE CF-CARRIER-CNTL        TO  ACARIERO.                    EL105
02015      MOVE AL-UANON               TO  ACARIERA.                    EL105
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF           TO  ASPLABLA
112103*        MOVE AL-SADOF           TO  ASECPAYA
112103*    ELSE
112103*        MOVE CF-SECPAY-SWITCH   TO  ASECPAYO
112103*    END-IF.
02016      MOVE CF-MAIL-TO-NAME        TO  ACONAMEO.                    EL105
02017      MOVE CF-IN-CARE-OF          TO  ACAREOFO.                    EL105
02018      MOVE CF-ADDRESS-LINE-1      TO  AADDR1O.                     EL105
02019      MOVE CF-ADDRESS-LINE-2      TO  AADDR2O.                     EL105
02020      MOVE CF-CITY-STATE          TO  ACITYSTO.                    EL105
02021                                                                      CL**7
02022      IF CF-ZIP-CODE-NUM NUMERIC  AND                                 CL**7
02023         CF-ZIP-CODE-NUM NOT = ZEROS                                  CL**7
02024          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM                  CL**7
02025          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.                     CL**7
02026                                                                      CL**7
02027      MOVE SPACES                   TO WS-ZIP-CODE.                   CL**7
02028      IF CF-CANADIAN-POST-CODE                                        CL**7
02029          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1             CL**7
02030          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2             CL**7
02031      ELSE                                                            CL**7
02032          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE               CL**7
02033          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL**7
02034              MOVE '-'              TO WS-ZIP-AM-2-DASH               CL**7
02035              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.             CL**7
02036                                                                      CL**7
02037      MOVE WS-ZIP-CODE            TO  AZIPO.                          CL**7
02039      MOVE CF-DOMICILE-STATE      TO  ADOMSTO.                     EL105
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF               TO  ACTLABLA
112103*        MOVE AL-SADOF               TO  ACLPTOLA 
112103*    ELSE
112103*        MOVE CF-CARRIER-CLP-TOL-PCT TO  ACLPTOLO
112103*    END-IF.
02040                                                                      CL*13
02041      IF CLAIM-SESSION                                                CL*13
02042         MOVE CF-LAST-ALPHA-CHARACTER TO ALPHCHO.                     CL*13
02043                                                                      CL*13
02044      MOVE CF-PHONE-NO            TO  APHONEO.                     EL105
02045 *    INSPECT APHONEI CONVERTING SPACES TO '-'.                       CL*14
02046      INSPECT APHONEO CONVERTING SPACES TO '-'.                       CL*14
02047      EJECT                                                        EL105
02048                                                                   EL105
02049      MOVE CF-CLAIM-NO-METHOD     TO  ACLNAMO.                     EL105
02050      MOVE CF-CLAIM-COUNTER       TO  ACLAIMO.                     EL105
02051                                                                   EL105
02052      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*13
02053          MOVE AL-UNNOF           TO  ACLAIMA.                     EL105
02054                                                                   EL105
02055      MOVE CF-CHECK-NO-CONTROL    TO  ACKNAMO.                     EL105
02056                                                                   EL105
02057      MOVE CF-CHECK-COUNTER       TO  ACHECKO.                     EL105
02058                                                                   EL105
02059      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*13
02060          MOVE AL-UNNOF           TO  ACHECKA.                     EL105
02061                                                                   EL105
02062      MOVE CF-EXPENSE-METHOD      TO  AEXPCMO.                     EL105
02063      MOVE CF-EXPENSE-PERCENT     TO  AEXPCPO.                     EL105
02064      MOVE CF-EXPENSE-DOLLAR      TO  AEXPCAO.                     EL105
02065                                                                   EL105
02066      IF CLAIM-SESSION                                                CL*13
02067         MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS TO ABRETRO.              CL*15
02068                                                                      CL*13
02069      MOVE CF-LETTER-RESEND-OPT   TO  ALAO.                        EL105
02070      INSPECT ALAO CONVERTING ' 1' TO 'NY'.                           CL*13
02071                                                                   EL105
02072      MOVE CF-MANUAL-SW           TO  ARESMANO.                    EL105
02073      INSPECT ARESMANO CONVERTING ' 1' TO 'NY'.                       CL*13
02074                                                                   EL105
02075      MOVE CF-FUTURE-SW           TO  ARESCDTO.                    EL105
02076      INSPECT ARESCDTO CONVERTING ' 1' TO 'NY'.                       CL*13
02077                                                                   EL105
02078      MOVE CF-PTC-SW              TO  ARESPTCO.                    EL105
02079      INSPECT ARESPTCO CONVERTING ' 1' TO 'NY'.                       CL*13
02080                                                                   EL105
02081      MOVE CF-IBNR-SW             TO  ARESIBNO.                    EL105
02082      INSPECT ARESIBNO CONVERTING ' 1' TO 'NY'.                       CL*13
02083                                                                   EL105
02084      MOVE CF-CDT-ACCESS-METHOD   TO  ACDTAO.                      EL105
02085      MOVE CF-PERCENT-OF-CDT      TO  APCTCDTO.                    EL105
02086      MOVE CF-IBNR-PERCENT        TO  IBNRPCTO.                    EL105
02087                                                                      CL*11
02088      IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC                            CL*11
02089          MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.          CL*11
02090      IF CF-IBNR-R78-PERCENT NOT NUMERIC                              CL*11
02091          MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.            CL*11
02092      IF CF-IBNR-PRO-PERCENT NOT NUMERIC                              CL*11
02093          MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.            CL*11
02094                                                                      CL*11
02095      MOVE CF-IBNR-UEPRM-PERCENT  TO  AUEPPCTO.                       CL*11
02096      MOVE CF-IBNR-R78-PERCENT    TO  AR78PCTO.                       CL*11
02097      MOVE CF-IBNR-PRO-PERCENT    TO  APROPCTO.                       CL*11
02098                                                                   EL105
02099      MOVE CF-CLAIM-CALC-METHOD   TO  ACLCMO.                      EL105
02100                                                                   EL105
02101      MOVE CF-CALC-AMT-TOL        TO  ALQCAO.                      EL105
02102      MOVE CF-MAX-REG-PMT         TO  ALMRPO.                      EL105
02103      MOVE CF-MAX-REG-DAYS        TO  ALMDPPO.                     EL105
02104      MOVE CF-MAX-AUTO-PMT        TO  ALMAPO.                      EL105
02105      MOVE CF-MAX-AUTO-MOS        TO  ALMAPMO.                     EL105
02106      MOVE CF-CALC-DAYS-TOL       TO  ALQCDO.                      EL105
02107                                                                   EL105
02108      MOVE CF-DAYS-BEFORE-CLOSED  TO  ALDBCO.                      EL105
02109      MOVE CF-MONTHS-BEFORE-PURGED  TO  ALMBPO.                    EL105
032813
032813     MOVE CF-CARRIER-NEXT-AUDIT-CHK-NO TO ANXTAUDO.
02110                                                                   EL105
02111  6030-DISPLAY-MAINT.                                              EL105
02112      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL105
02113      MOVE SPACES                 TO  DC-OPTION-CODE.              EL105
02114      PERFORM 8500-DATE-CONVERSION.                                EL105
02115                                                                   EL105
02116      IF CREDIT-SESSION                                               CL**5
02117          MOVE DC-GREG-DATE-1-EDIT    TO  BLUDATEO                    CL**5
02118          MOVE CF-LAST-MAINT-HHMMSS   TO  BLUTIMEO                    CL**5
02119          INSPECT BLUTIMEI CONVERTING SPACES TO '.'                   CL*13
02120          MOVE CF-LAST-MAINT-BY       TO  BLUBYO                      CL**5
02121          MOVE -1                     TO  BMAINTL                     CL**5
02122      ELSE                                                            CL**5
02123          MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO                    CL**5
02124          MOVE CF-LAST-MAINT-HHMMSS   TO  ALUTIMEO                    CL**5
02125          INSPECT ALUTIMEI CONVERTING SPACES TO '.'                   CL*13
02126          MOVE CF-LAST-MAINT-BY       TO  ALUBYO                      CL**5
02127          MOVE -1                     TO  AMAINTL.                    CL**5
02128                                                                   EL105
02129      MOVE +1                     TO  PI-BROWSE-SW.                EL105
02130      ADD  +1                     TO  WS-CFK-SEQUENCE-NO.          EL105
02131                                                                   EL105
02132      PERFORM 8000-DISPLAY-RECORDS THRU 8010-DISPLAY-RECORDS.      EL105
02133                                                                   EL105
02134      EXEC CICS ENDBR                                              EL105
02135          DATASET (WS-CONTROL-FILE-DSID)                           EL105
02136      END-EXEC.                                                    EL105
02137                                                                   EL105
02138      MOVE WS-CFK-CARRIER-NO      TO  PI-NEXT-CARRIER-NUMBER.      EL105
02139                                                                   EL105
02140      PERFORM 8100-SEND-INITIAL-MAP.                               EL105
02141      GO TO 9100-RETURN-TRAN.                                      EL105
02142                                                                   EL105
02143  6060-NOT-FOUND.                                                  EL105
02144      MOVE AL-UNBON               TO  ACARIERA.                    EL105
02145      MOVE -1                     TO  ACARIERL.                    EL105
02146      MOVE ER-0006                TO  EMI-ERROR.                   EL105
02147      PERFORM 9900-ERROR-FORMAT.                                   EL105
02148                                                                   EL105
02149      PERFORM 8200-SEND-DATAONLY.                                  EL105
02150      GO TO 9100-RETURN-TRAN.                                      EL105
02151                                                                   EL105
02152      EJECT                                                        EL105
02153                                                                   EL105
02154  8000-DISPLAY-RECORDS SECTION.                                    EL105
02155      EXEC CICS HANDLE CONDITION                                   EL105
02156          NOTFND  (8060-DISPLAY-RECORDS)                           EL105
02157          ENDFILE (8040-DISPLAY-RECORDS)                           EL105
02158      END-EXEC.                                                    EL105
02159                                                                   EL105
02160      EXEC CICS STARTBR                                            EL105
02161           DATASET (WS-CONTROL-FILE-DSID)                          EL105
02162           RIDFLD  (WS-CONTROL-FILE-KEY)                           EL105
02163           GTEQ                                                    EL105
02164      END-EXEC.                                                    EL105
02165                                                                   EL105
02166  8010-DISPLAY-RECORDS.                                            EL105
02167      EXEC CICS READNEXT                                           EL105
02168          DATASET (WS-CONTROL-FILE-DSID)                           EL105
02169          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL105
02170          SET     (ADDRESS OF CONTROL-FILE)                           CL*13
02171      END-EXEC.                                                    EL105
02172                                                                   EL105
02173      IF WS-CFK-COMPANY-ID NOT = PI-COMPANY-ID                     EL105
02174          GO TO 8040-DISPLAY-RECORDS.                              EL105
02175                                                                   EL105
02176      IF CF-RECORD-TYPE NOT = '6'                                  EL105
02177          MOVE ZERO               TO  PI-1ST-TIME-SW               EL105
02178          MOVE SPACES             TO  PI-NEXT-CARRIER-NUMBER       EL105
02179          MOVE ER-0173            TO  EMI-ERROR                    EL105
02180          PERFORM 9900-ERROR-FORMAT                                EL105
02181          GO TO 8050-DISPLAY-RECORDS.                              EL105
02182                                                                   EL105
02183  8015-DISPLAY-RECORDS.                                            EL105
02184      IF LCP-ONCTR-01 =  0                                            CL*13
02185          ADD 1 TO LCP-ONCTR-01                                       CL*13
02186         GO TO 8020-DISPLAY-RECORDS.                               EL105
02187                                                                   EL105
02188      MOVE WS-CFK-CARRIER-NO      TO  PI-NEXT-CARRIER-NUMBER.      EL105
02189      MOVE +1                     TO  PI-BROWSE-SW.                EL105
02190      GO TO 8050-DISPLAY-RECORDS.                                  EL105
02191                                                                   EL105
02192  8020-DISPLAY-RECORDS.                                            EL105
02193      IF NOT CREDIT-SESSION                                           CL**5
02194          GO TO 8025-NOT-CREDIT-SESSION.                              CL*15
02195                                                                      CL**5
02196      MOVE LOW-VALUES             TO  EL105BO.                        CL*15
02197                                                                      CL*15
02198      MOVE 'S'                    TO  BMAINTO.                        CL**5
02199      MOVE AL-UANON               TO  BMAINTA.                        CL**5
02200      MOVE -1                     TO  BMAINTL.                        CL**5
02201      MOVE CF-CARRIER-CNTL        TO  BCARIERO                        CL*12
02202                                      PI-CARRIER-NUMBER.              CL*12
02203      MOVE AL-UANON               TO  BCARIERA.                       CL**5
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF           TO  BSPLABLA
112103         MOVE AL-SADOF           TO  BSECPAYA
112103     ELSE 
112103         MOVE CF-SECPAY-SWITCH   TO  BSECPAYO
112103     END-IF.

02204      MOVE CF-MAIL-TO-NAME        TO  BCONAMEO.                       CL**5
02205      MOVE CF-IN-CARE-OF          TO  BCAREOFO.                       CL**5
02206      MOVE CF-ADDRESS-LINE-1      TO  BADDR1O.                        CL**5
02207      MOVE CF-ADDRESS-LINE-2      TO  BADDR2O.                        CL**5
02208      MOVE CF-CITY-STATE          TO  BCITYSTO.                       CL**5
02209                                                                      CL**7
02210      MOVE CF-RATING-SWITCH       TO  BCLCPRMO.                       CL*15
02211                                                                      CL*13
02212      IF CF-ZIP-CODE-NUM NUMERIC  AND                                 CL**7
02213         CF-ZIP-CODE-NUM NOT = ZEROS                                  CL**7
02214          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM                  CL**7
02215          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.                     CL**7
02216                                                                      CL**7
02217      MOVE SPACES                   TO WS-ZIP-CODE.                   CL**7
02218      IF CF-CANADIAN-POST-CODE                                        CL**7
02219          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1             CL**7
02220          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2             CL**7
02221      ELSE                                                            CL**7
02222          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE               CL**7
02223          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL**7
02224              MOVE '-'              TO WS-ZIP-AM-2-DASH               CL**7
02225              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.             CL**7
02226                                                                      CL**7
02227      MOVE WS-ZIP-CODE            TO  BZIPO.                          CL**7
02228                                                                      CL**7
02229      MOVE CF-DOMICILE-STATE      TO  BDOMSTO.                        CL**5
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF               TO  BCTLABLA 
112103                                         BCLPTOLA
092705                                         BLCOMMA
112103     ELSE
112103         MOVE CF-CARRIER-CLP-TOL-PCT TO  BCLPTOLO
092705         MOVE CF-CARRIER-LEASE-COMM  TO  BLCOMMO
112103     END-IF.
02230      MOVE CF-PHONE-NO            TO  BPHONEO.                        CL**5
02231      INSPECT BPHONEO CONVERTING SPACES TO '-'.                       CL*14
02232                                                                      CL**5
02233      IF CF-CR-TOL-PREM NUMERIC                                       CL**5
02234          IF CF-CR-TOL-PREM NOT = ZEROS                               CL**5
02235              MOVE CF-CR-TOL-PREM        TO  BPRMTOLO.                CL**5
02236                                                                      CL**5
02237      IF CF-CR-TOL-REFUND NUMERIC                                     CL**5
02238          IF CF-CR-TOL-REFUND NOT = ZEROS                             CL**5
02239              MOVE CF-CR-TOL-REFUND      TO  BREFTOLO.                CL**5
02240                                                                      CL**5
02241      IF CF-CR-OVR-SHT-AMT  NUMERIC AND                               CL*17
02242             CF-CR-OVR-SHT-AMT  > 0                                   CL*17
02243         MOVE CF-CR-OVR-SHT-AMT TO  BOVSAMTO                          CL*17
02244      END-IF.                                                         CL*17
02245                                                                      CL*17
02246      IF CF-CR-TOL-PREM-PCT NUMERIC                                   CL**5
02247          IF CF-CR-TOL-PREM-PCT NOT = ZEROS                           CL**5
02248              MOVE CF-CR-TOL-PREM-PCT    TO  BPRMPCTO.                CL**5
02249                                                                      CL**5
02250      IF CF-CR-TOL-REFUND-PCT NUMERIC                                 CL**5
02251          IF CF-CR-TOL-REFUND-PCT NOT = ZEROS                         CL**5
02252              MOVE CF-CR-TOL-REFUND-PCT  TO  BREFPCTO.                CL**5
02253                                                                      CL*17
02254      IF CF-CR-OVR-SHT-PCT NUMERIC AND                                CL*17
02255           CF-CR-OVR-SHT-PCT > 0                                      CL*17
02256              MOVE CF-CR-OVR-SHT-PCT TO BOVSPCTO                      CL*17
02257      END-IF.                                                         CL*17
02258                                                                      CL**5
02259      MOVE AL-UNNON                      TO  BPRMTOLA  BREFTOLA       CL**5
02260                                             BPRMPCTA  BREFPCTA.      CL**5
02261                                                                      CL**5
02262      GO TO 8030-DISPLAY-MAINT.                                       CL**5
02263                                                                      CL**5
02264  8025-NOT-CREDIT-SESSION.                                            CL*15
02265                                                                      CL*13
02266      MOVE CF-CARRIER-CNTL        TO  ACARIERO.                    EL105
02267      MOVE AL-UANON               TO  ACARIERA.                    EL105
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF           TO  ASPLABLA
112103*        MOVE AL-SADOF           TO  ASECPAYA
112103*    ELSE
112103*        MOVE CF-SECPAY-SWITCH   TO  ASECPAYO
112103*    END-IF.
02268      MOVE CF-MAIL-TO-NAME        TO  ACONAMEO.                    EL105
02269      MOVE CF-IN-CARE-OF          TO  ACAREOFO.                    EL105
02270      MOVE CF-ADDRESS-LINE-1      TO  AADDR1O.                     EL105
02271      MOVE CF-ADDRESS-LINE-2      TO  AADDR2O.                     EL105
02272      MOVE CF-CITY-STATE          TO  ACITYSTO.                    EL105
02273                                                                      CL**7
02274      IF CF-ZIP-CODE-NUM NUMERIC  AND                                 CL**7
02275         CF-ZIP-CODE-NUM NOT = ZEROS                                  CL**7
02276          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM                  CL**7
02277          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.                     CL**7
02278                                                                      CL**7
02279      MOVE SPACES                   TO WS-ZIP-CODE.                   CL**7
02280      IF CF-CANADIAN-POST-CODE                                        CL**7
02281          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1             CL**7
02282          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2             CL**7
02283      ELSE                                                            CL**7
02284          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE               CL**7
02285          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL**7
02286              MOVE '-'              TO WS-ZIP-AM-2-DASH               CL**7
02287              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.             CL**7
02288                                                                      CL**7
02289      MOVE WS-ZIP-CODE            TO  AZIPO.                          CL**7
02291      MOVE CF-DOMICILE-STATE      TO  ADOMSTO.                     EL105
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF               TO  ACTLABLA
112103*        MOVE AL-SADOF               TO  ACLPTOLA
112103*    ELSE
112103*        MOVE CF-CARRIER-CLP-TOL-PCT TO  ACLPTOLO
112103*    END-IF.
02292                                                                      CL*13
02293      IF CLAIM-SESSION                                                CL*13
02294         MOVE CF-LAST-ALPHA-CHARACTER TO ALPHCHO.                     CL*13
02295                                                                      CL*13
02296      MOVE CF-PHONE-NO            TO  APHONEO.                     EL105
02297      INSPECT APHONEO CONVERTING SPACES TO '-'.                       CL*14
02298                                                                   EL105
02299      EJECT                                                        EL105
02300                                                                   EL105
02301      MOVE CF-CLAIM-NO-METHOD     TO  ACLNAMO.                     EL105
02302      MOVE CF-CLAIM-COUNTER       TO  ACLAIMO.                     EL105
02303                                                                   EL105
02304      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*13
02305          MOVE AL-UNNOF           TO  ACLAIMA.                     EL105
02306                                                                   EL105
02307      MOVE CF-CHECK-NO-CONTROL    TO  ACKNAMO.                     EL105
02308                                                                   EL105
02309      MOVE CF-CHECK-COUNTER       TO  ACHECKO.                     EL105
02310                                                                   EL105
02311      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*13
02312          MOVE AL-UNNOF           TO  ACHECKA.                     EL105
02313                                                                   EL105
02314      MOVE CF-EXPENSE-METHOD      TO  AEXPCMO.                     EL105
02315      MOVE CF-EXPENSE-PERCENT     TO  AEXPCPO.                     EL105
02316      MOVE CF-EXPENSE-DOLLAR      TO  AEXPCAO.                     EL105
02317                                                                   EL105
02318      IF CLAIM-SESSION                                                CL*13
02319         MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS TO ABRETRO.              CL*15
032813
032813     MOVE CF-CARRIER-NEXT-AUDIT-CHK-NO TO ANXTAUDO.
02320                                                                      CL*13
02321      MOVE CF-LETTER-RESEND-OPT   TO  ALAO.                        EL105
02322      INSPECT ALAO CONVERTING ' 1' TO 'NY'.                           CL*13
02323                                                                   EL105
02324      MOVE CF-MANUAL-SW           TO  ARESMANO.                    EL105
02325      INSPECT ARESMANO CONVERTING ' 1' TO 'NY'.                       CL*13
02326                                                                   EL105
02327      MOVE CF-FUTURE-SW           TO  ARESCDTO.                    EL105
02328      INSPECT ARESCDTO CONVERTING ' 1' TO 'NY'.                       CL*13
02329                                                                   EL105
02330      MOVE CF-PTC-SW              TO  ARESPTCO.                    EL105
02331      INSPECT ARESPTCO CONVERTING ' 1' TO 'NY'.                       CL*13
02332                                                                   EL105
02333      MOVE CF-IBNR-SW             TO  ARESIBNO.                    EL105
02334      INSPECT ARESIBNO CONVERTING ' 1' TO 'NY'.                       CL*13
02335                                                                   EL105
02336      MOVE CF-CDT-ACCESS-METHOD   TO  ACDTAO.                      EL105
02337      MOVE CF-PERCENT-OF-CDT      TO  APCTCDTO.                    EL105
02338      MOVE CF-IBNR-PERCENT        TO  IBNRPCTO.                    EL105
02339                                                                      CL*11
02340      IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC                            CL*11
02341          MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.          CL*11
02342      IF CF-IBNR-R78-PERCENT NOT NUMERIC                              CL*11
02343          MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.            CL*11
02344      IF CF-IBNR-PRO-PERCENT NOT NUMERIC                              CL*11
02345          MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.            CL*11
02346                                                                      CL*11
02347      MOVE CF-IBNR-UEPRM-PERCENT  TO  AUEPPCTO.                       CL*11
02348      MOVE CF-IBNR-R78-PERCENT    TO  AR78PCTO.                       CL*11
02349      MOVE CF-IBNR-PRO-PERCENT    TO  APROPCTO.                       CL*11
02350                                                                   EL105
02351      MOVE CF-CLAIM-CALC-METHOD   TO  ACLCMO.                      EL105
02352                                                                   EL105
02353      MOVE CF-CALC-AMT-TOL        TO  ALQCAO.                      EL105
02354      MOVE CF-MAX-REG-PMT         TO  ALMRPO.                      EL105
02355      MOVE CF-MAX-REG-DAYS        TO  ALMDPPO.                     EL105
02356      MOVE CF-MAX-AUTO-PMT        TO  ALMAPO.                      EL105
02357      MOVE CF-MAX-AUTO-MOS        TO  ALMAPMO.                     EL105
02358      MOVE CF-CALC-DAYS-TOL       TO  ALQCDO.                      EL105
02359                                                                   EL105
02360      MOVE CF-DAYS-BEFORE-CLOSED  TO  ALDBCO.                      EL105
02361      MOVE CF-MONTHS-BEFORE-PURGED  TO  ALMBPO.                    EL105
02362                                                                   EL105
02363  8030-DISPLAY-MAINT.                                              EL105
02364      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL105
02365      MOVE SPACES                 TO  DC-OPTION-CODE.              EL105
02366      PERFORM 8500-DATE-CONVERSION.                                EL105
02367                                                                   EL105
02368      IF CREDIT-SESSION                                               CL**5
02369          MOVE DC-GREG-DATE-1-EDIT    TO  BLUDATEO                    CL**5
02370          MOVE CF-LAST-MAINT-HHMMSS   TO  BLUTIMEO                    CL**5
02371          INSPECT BLUTIMEI CONVERTING SPACES TO '.'                   CL*13
02372          MOVE CF-LAST-MAINT-BY       TO  BLUBYO                      CL**5
02373      ELSE                                                            CL**5
02374          MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO                    CL**5
02375          MOVE CF-LAST-MAINT-HHMMSS   TO  ALUTIMEO                    CL**5
02376          INSPECT ALUTIMEI CONVERTING SPACES TO '.'                   CL*13
02377          MOVE CF-LAST-MAINT-BY       TO  ALUBYO.                     CL**5
02378                                                                   EL105
02379      MOVE -1                     TO  AMAINTL.                     EL105
02380                                                                   EL105
02381      GO TO 8010-DISPLAY-RECORDS.                                  EL105
02382                                                                   EL105
02383  8040-DISPLAY-RECORDS.                                            EL105
02384      MOVE +1                     TO  PI-END-OF-FILE.              EL105
02385      MOVE ER-0173                TO  EMI-ERROR.                   EL105
02386      MOVE SPACES                 TO  PI-NEXT-CARRIER-NUMBER.      EL105
02387      PERFORM 9900-ERROR-FORMAT.                                   EL105
02388                                                                   EL105
02389  8050-DISPLAY-RECORDS.                                            EL105
02390      EXEC CICS ENDBR                                              EL105
02391          DATASET (WS-CONTROL-FILE-DSID)                           EL105
02392      END-EXEC.                                                    EL105
02393                                                                   EL105
02394      PERFORM 8100-SEND-INITIAL-MAP.                               EL105
02395      GO TO 9100-RETURN-TRAN.                                      EL105
02396                                                                   EL105
02397  8060-DISPLAY-RECORDS.                                            EL105
02398      MOVE AL-UNBON               TO  ACARIERA.                    EL105
02399      MOVE -1                     TO  ACARIERL.                    EL105
02400      MOVE ER-0006                TO  EMI-ERROR.                   EL105
02401      PERFORM 9900-ERROR-FORMAT.                                   EL105
02402                                                                   EL105
02403      PERFORM 8200-SEND-DATAONLY.                                  EL105
02404      GO TO 9100-RETURN-TRAN.                                      EL105
02405                                                                   EL105
02406      EJECT                                                        EL105
02407  8100-SEND-INITIAL-MAP SECTION.                                   EL105
02408      MOVE SAVE-DATE              TO  ADATEO.                      EL105
02409      MOVE EIBTIME                TO  TIME-IN.                     EL105
02410      MOVE TIME-OUT               TO  ATIMEO.                      EL105
02411      MOVE -1                     TO  AMAINTL                      EL105
02412                                                                   EL105
02413 ****DMD CUSTOM  CODE******************                               CL*13
02414      IF PI-COMPANY-ID = 'DMD'                                        CL*13
02415      IF CREDIT-SESSION                                               CL*15
02416         MOVE AL-SANOF           TO DMDSW2A                           CL*13
02417         MOVE AL-UANON           TO BCLCPRMA.                         CL*14
02418 ****DMD CUSTOM  CODE******************                               CL*13
02419                                                                      CL*13
02420      EJECT                                                        EL105
02421      IF CREDIT-SESSION                                            EL105
02422          MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O                    CL**5
02423      ELSE                                                            CL**5
02424          MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                   CL**5
02425                                                                      CL*13
02426      IF CLAIM-SESSION                                                CL*13
02427         MOVE 'ALPHA'             TO ALPHLO.                          CL*13
02428                                                                   EL105
02429      EXEC CICS SEND                                               EL105
02430          FROM   (EL105AI)                                         EL105
02431          MAPSET (WS-MAPSET-NAME)                                  EL105
02432          MAP    (WS-MAP-NAME)                                     EL105
02433          CURSOR ERASE                                             EL105
02434      END-EXEC.                                                    EL105
02435                                                                   EL105
02436  8100-EXIT.                                                       EL105
02437      EXIT.                                                        EL105
02438                                                                   EL105
02439      EJECT                                                        EL105
02440  8200-SEND-DATAONLY SECTION.                                      EL105
02441      MOVE SAVE-DATE              TO  ADATEO.                      EL105
02442      MOVE EIBTIME                TO  TIME-IN.                     EL105
02443      MOVE TIME-OUT               TO  ATIMEO.                      EL105
02444                                                                   EL105
02445      IF CREDIT-SESSION                                            EL105
02446          MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O                     CL**5
02447      ELSE                                                            CL**5
02448          MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                    CL**5
02449                                                                      CL*13
02450 ****DMD CUSTOM  CODE******************                               CL*13
02451      IF PI-COMPANY-ID = 'DMD'                                        CL*13
02452      IF CREDIT-SESSION                                               CL*15
02453         MOVE AL-SANOF           TO DMDSW2A                           CL*13
02454         MOVE AL-UANON           TO BCLCPRMA.                         CL*14
02455 ****DMD CUSTOM  CODE******************                               CL*13
02456                                                                      CL*13
02457      IF CLAIM-SESSION                                                CL*13
02458         MOVE 'ALPHA'             TO ALPHLO.                          CL*13
02459                                                                   EL105
02460      EXEC CICS SEND DATAONLY                                      EL105
02461          FROM   (EL105AI)                                         EL105
02462          MAPSET (WS-MAPSET-NAME)                                  EL105
02463          MAP    (WS-MAP-NAME)                                     EL105
02464          CURSOR                                                   EL105
02465      END-EXEC.                                                    EL105
02466                                                                   EL105
02467  8200-EXIT.                                                       EL105
02468      EXIT.                                                        EL105
02469                                                                   EL105
02470      EJECT                                                        EL105
02471  8300-SEND-TEXT SECTION.                                          EL105
02472      EXEC CICS SEND TEXT                                          EL105
02473          FROM   (LOGOFF-TEXT)                                     EL105
02474          LENGTH (LOGOFF-LENGTH)                                   EL105
02475          ERASE  FREEKB                                            EL105
02476      END-EXEC.                                                    EL105
02477                                                                   EL105
02478      EXEC CICS RETURN                                             EL105
02479      END-EXEC.                                                    EL105
02480                                                                   EL105
02481  8300-EXIT.                                                       EL105
02482      EXIT.                                                        EL105
02483                                                                   EL105
02484      EJECT                                                        EL105
02485  8400-LOG-JOURNAL-RECORD SECTION.                                 EL105
02486      IF PI-JOURNAL-FILE-ID = ZERO                                 EL105
02487          GO TO 8400-EXIT.                                         EL105
02488                                                                   EL105
02489      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL105
02490      MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.                  EL105
02491      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL105
02492                                                                   EL105
pemuni*    EXEC CICS JOURNAL                                            EL105
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)                             EL105
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)                             EL105
pemuni*        FROM    (JOURNAL-RECORD)                                 EL105
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH)                       EL105
pemuni*    END-EXEC.                                                    EL105
02499                                                                   EL105
02500  8400-EXIT.                                                       EL105
02501      EXIT.                                                        EL105
02502                                                                   EL105
02503  8500-DATE-CONVERSION SECTION.                                    EL105
02504      EXEC CICS LINK                                               EL105
02505          PROGRAM  ('ELDATCV')                                     EL105
02506          COMMAREA (DATE-CONVERSION-DATA)                          EL105
02507          LENGTH   (DC-COMM-LENGTH)                                EL105
02508      END-EXEC.                                                    EL105
02509                                                                   EL105
02510  8500-EXIT.                                                       EL105
02511      EXIT.                                                        EL105
02512                                                                   EL105
02513      EJECT                                                        EL105
02514  8700-NOT-OPEN SECTION.                                           EL105
02515      MOVE ER-0042                TO EMI-ERROR.                    EL105
02516      MOVE -1                     TO ACARIERL.                     EL105
02517      PERFORM 9900-ERROR-FORMAT.                                   EL105
02518      PERFORM 8200-SEND-DATAONLY.                                  EL105
02519      GO TO 9100-RETURN-TRAN.                                      EL105
02520                                                                   EL105
02521  8700-EXIT.                                                       EL105
02522       EXIT.                                                       EL105
02523                                                                   EL105
02524  8800-DUPREC SECTION.                                             EL105
02525      MOVE ER-0497                TO EMI-ERROR.                    EL105
02526      MOVE -1                     TO ACARIERL.                     EL105
02527      PERFORM 9900-ERROR-FORMAT.                                   EL105
02528      PERFORM 8200-SEND-DATAONLY.                                  EL105
02529      GO TO 9100-RETURN-TRAN.                                      EL105
02530                                                                   EL105
02531  8800-EXIT.                                                       EL105
02532       EXIT.                                                       EL105
02533                                                                   EL105
02534      EJECT                                                        EL105
02535  9000-RETURN-CICS SECTION.                                        EL105
02536      MOVE 'EL005   '             TO  THIS-PGM.                    EL105
02537      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL105
02538      PERFORM 9300-XCTL.                                           EL105
02539                                                                   EL105
02540  9000-EXIT.                                                       EL105
02541      EXIT.                                                        EL105
02542                                                                   EL105
02543  9100-RETURN-TRAN SECTION.                                        EL105
02544      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL105
02545      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL105
02546                                                                   EL105
02547      EXEC CICS RETURN                                             EL105
02548          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL105
02549          LENGTH   (PI-COMM-LENGTH)                                EL105
02550          TRANSID  (WS-TRANS-ID)                                   EL105
02551      END-EXEC.                                                    EL105
02552      GOBACK.                                                         CL*13
02553                                                                   EL105
02554  9100-EXIT.                                                       EL105
02555      EXIT.                                                        EL105
02556                                                                   EL105
02557  9300-XCTL SECTION.                                               EL105
02558      MOVE DFHENTER               TO  EIBAID                       EL105
02559                                                                   EL105
02560      EXEC CICS XCTL                                               EL105
02561          PROGRAM  (THIS-PGM)                                      EL105
02562          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL105
02563          LENGTH   (PI-COMM-LENGTH)                                EL105
02564      END-EXEC.                                                    EL105
02565                                                                   EL105
02566  9300-EXIT.                                                       EL105
02567      EXIT.                                                        EL105
02568                                                                   EL105
02569      EJECT                                                        EL105
02570  9400-CLEAR SECTION.                                              EL105
02571      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL105
02572      PERFORM 9300-XCTL.                                           EL105
02573                                                                   EL105
02574  9400-EXIT.                                                       EL105
02575      EXIT.                                                        EL105
02576                                                                   EL105
02577  9600-PGMIDERR SECTION.                                           EL105
02578      EXEC CICS HANDLE CONDITION                                   EL105
02579          PGMIDERR (8300-SEND-TEXT)                                EL105
02580      END-EXEC.                                                    EL105
02581                                                                   EL105
02582      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL105
02583                                      LOGOFF-PGM.                  EL105
02584                                                                   EL105
02585      MOVE 'EL005   '             TO  THIS-PGM.                    EL105
02586      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL105
02587      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL105
02588      PERFORM 9300-XCTL.                                           EL105
02589                                                                   EL105
02590  9600-EXIT.                                                       EL105
02591      EXIT.                                                        EL105
02592                                                                   EL105
02593                                                                   EL105
02594      EJECT                                                        EL105
02595  9900-ERROR-FORMAT SECTION.                                       EL105
02596      EXEC CICS LINK                                               EL105
02597          PROGRAM  ('EL001')                                       EL105
02598          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL105
02599          LENGTH   (EMI-COMM-LENGTH)                               EL105
02600      END-EXEC.                                                    EL105
02601                                                                   EL105
02602  9900-EXIT.                                                       EL105
02603      EXIT.                                                        EL105
02604                                                                   EL105
02605      EJECT                                                        EL105
02606  9990-ERROR SECTION.                                              EL105
02607      MOVE DFHEIBLK               TO EMI-LINE1.                    EL105
02608                                                                   EL105
02609      EXEC CICS LINK                                               EL105
02610          PROGRAM  ('EL004')                                       EL105
02611          COMMAREA (EMI-LINE1)                                     EL105
02612          LENGTH   (72)                                            EL105
02613      END-EXEC.                                                    EL105
02614                                                                   EL105
02615      PERFORM 8200-SEND-DATAONLY.                                  EL105
02616      GO TO 9100-RETURN-TRAN.                                      EL105
02617                                                                   EL105
02618  9990-EXIT.                                                       EL105
02619      EXIT.                                                        EL105
02620                                                                   EL105
02621  9995-SECURITY-VIOLATION.                                         EL105
02622             COPY ELCSCTP.                                         EL105
