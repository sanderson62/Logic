00001  ID DIVISION.                                                     05/12/98
00002                                                                   EL178
00003  PROGRAM-ID.                 EL178.                                  LV010
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 02/13/96 09:35:26.                    CL**8
00007 *                            VMOD=2.009                              CL**9
00008 *                                                                 EL178
00009 *AUTHOR.     LOGIC,INC.                                              CL**8
00010 *            DALLAS, TEXAS.                                          CL**8
00011                                                                   EL178
00024 *REMARKS.    TRANSACTION - EX48 - LETTER RELEASE.                    CL**3
00025 *        THIS FUNCTION IS USED TO START THE PRINTING OF STORED       CL**3
00026 *        LETTERS AND/OR ADDRESS LABELS.  IT MAY ALSO BE USED         CL**3
00027 *        TO OBTAIN COUNTS OF OUTSTANDING LETTERS.                    CL**3
00011 *#################################################################
110402*
110402*                        C H A N G E   L O G
110402*-----------------------------------------------------------------
110402*  CHANGE   CHANGE REQUEST  PGMR  DESCRIPTION OF CHANGE
110402* EFFECTIVE    NUMBER
110402*-----------------------------------------------------------------
110402* 110402    2001031200008   SMVA  ADD OPTION 8,CLAIM NO, & CERT NO
110402*                                 TO EL178A SCREEN TO REPRINT ALL
110402*                                 LETTERS FOR A SPECIFIC CLAIM
110402******************************************************************
00028                                                                   EL178
00029      EJECT                                                        EL178
00030  ENVIRONMENT DIVISION.                                            EL178
00031  DATA DIVISION.                                                   EL178
00032  WORKING-STORAGE SECTION.                                         EL178
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL178
00034  77  FILLER  PIC X(32)  VALUE '*    EL178 WORKING STORAGE     *'. EL178
00035  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.009 *********'.    CL**9
00036                                                                   EL178
00037      COPY ELCSCTM.                                                   CL**7
00038                                                                   EL178
00039      COPY ELCSCRTY.                                                  CL**7
00040                                                                   EL178
00041  01  STANDARD-AREAS.                                              EL178
00042      12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.   EL178
00043      12  MAP-NAME.                                                EL178
00044          16  MAP-PREFIX              PIC X(2)    VALUE 'EL'.      EL178
00045          16  MAP-NUMBER              PIC X(4)    VALUE '178A'.    EL178
00046          16  MAP-FILLER              PIC X(2)    VALUE '  '.      EL178
00047      12  MAPSET-NAME             PIC X(8)    VALUE 'EL178S'.      EL178
00048      12  TRANS-ID                PIC X(4)    VALUE 'EX48'.        EL178
00049      12  PRINT-TRANS             PIC X(4)    VALUE 'EX56'.        EL178
00050      12  W-EL1524-TRANS          PIC X(4)    VALUE 'EXL2'.           CL**7
00051      12  PGM-NAME                PIC X(8).                        EL178
00052      12  TIME-IN                 PIC S9(7).                       EL178
00053      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL178
00054          16  FILLER              PIC X.                           EL178
00055          16  TIME-OUT            PIC 99V99.                       EL178
00056          16  FILLER              PIC X(2).                        EL178
00057      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       EL178
00058      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       EL178
00059      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       EL178
00060      12  LINK-001                PIC X(5)    VALUE 'EL001'.       EL178
00061      12  LINK-004                PIC X(5)    VALUE 'EL004'.       EL178
00062      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.        CL*10
00063      12  THIS-PGM                PIC X(8)    VALUE 'EL178'.       EL178
00064      12  CURRENT-SAVE            PIC XX.                          EL178
00065      12  WS-CURRENT-DATE-EDIT    PIC X(8)            VALUE SPACES.EL178
00066      12  DEEDIT-FIELD            PIC X(15).                       EL178
00067      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       EL178
00068      12  WS-COUNTER              PIC 9(5)  COMP-3 VALUE 0.        EL178
00069      12  SUB                     PIC 9  COMP-3.                   EL178
00070      12  ARCH-ID                 PIC X(8)    VALUE 'ELARCH2'.     EL178
00071      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL178
00072      12  ACTV-ID                 PIC X(8)    VALUE 'ELTRLR'.      EL178
00073      12  WS-COUNT-BY-CARR        PIC X       VALUE SPACE.            CL**2
00074          88  COUNT-BY-CARR                   VALUE 'Y'.              CL**2
00075      12  WS-CARRIER              PIC X       VALUE SPACE.            CL**2
00076                                                                      CL**3
00077      12  WS-DELAY-INTERVAL       PIC S9(7)   VALUE +2   COMP-3.      CL**3
00078                                                                      CL**3
00079      12  WS-RECORD-COUNT         PIC S9(4)   VALUE +0.               CL**3
00080                                                                   EL178
00081      12  CNTL-KEY.                                                EL178
00082          16  CNTL-CO             PIC X(3).                        EL178
00083          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL178
00084          16  CNTL-GENL.                                           EL178
00085            18 CNTL-GEN1          PIC X(2)    VALUE SPACES.        EL178
00086            18 CNTL-GEN2.                                          EL178
00087              20 CNTL-GEN3         PIC X       VALUE SPACES.       EL178
00088              20 CNTL-GEN4         PIC X       VALUE SPACES.       EL178
00089          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.   EL178
00090      12  ARCH-KEY.                                                EL178
00091          16  ARCH-PARTIAL-KEY.                                    EL178
00092              20  ARCH-CO          PIC X.                          EL178
00093              20  ARCH-REC-TYPE    PIC X.                          EL178
00094          16  ARCH-NUMBER          PIC S9(8)      COMP.            EL178
00095          16  ARCH-SEQ             PIC S9(4)      COMP VALUE +0.   EL178
00096      12  ACTV-KEY.                                                EL178
00097          16  ACTV-CO              PIC X.                          EL178
00098          16  ACTV-CARRIER         PIC X.                          EL178
00099          16  ACTV-CLAIM           PIC X(7).                       EL178
00100          16  ACTV-CERT-NO         PIC X(11).                      EL178
00101          16  ACTV-SEQ             PIC S9(4)   COMP.               EL178
00102      EJECT                                                        EL178
00103      12  ARCH-SAVE-KEY           PIC X(5).                        EL178
00104      12  ARCH-LENGTH             PIC S9(4)  COMP  VALUE +90.      EL178
00105                                                                   EL178
00106  01  ERROR-MESSAGES.                                              EL178
00107      12  ER-ZEROS                PIC 9(4)    VALUE 0000.          EL178
00108      12  ER-0000                 PIC 9(4)    VALUE 0000.          EL178
00109      12  ER-0004                 PIC 9(4)    VALUE 0004.          EL178
00110      12  ER-0008                 PIC 9(4)    VALUE 0008.          EL178
00111      12  ER-0029                 PIC 9(4)    VALUE 0029.          EL178
00112      12  ER-0042                 PIC 9(4)    VALUE 0042.          EL178
00113      12  ER-0070                 PIC 9(4)    VALUE 0070.          EL178
00114      12  ER-0172                 PIC 9(4)    VALUE 0172.          EL178
00115      12  ER-0182                 PIC 9(4)    VALUE 0182.          EL178
00116      12  ER-0189                 PIC 9(4)    VALUE 0189.          EL178
00117      12  ER-0190                 PIC 9(4)    VALUE 0190.          EL178
110402     12  ER-0194                 PIC 9(4)    VALUE 0194.          EL178
110402     12  ER-0203                 PIC 9(4)    VALUE 0203.          EL178
110402     12  ER-0209                 PIC 9(4)    VALUE 0209.          EL178
00118      12  ER-0408                 PIC 9(4)    VALUE 0408.          EL178
00119      12  ER-0409                 PIC 9(4)    VALUE 0409.          EL178
00120      12  ER-0410                 PIC 9(4)    VALUE 0410.          EL178
00121      12  ER-0411                 PIC 9(4)    VALUE 0411.          EL178
00122      12  ER-0412                 PIC 9(4)    VALUE 0412.          EL178
00123      12  ER-0413                 PIC 9(4)    VALUE 0413.          EL178
00124      12  ER-0669                 PIC 9(4)    VALUE 0669.             CL**5
00125      12  ER-3767                 PIC 9(4)    VALUE 3767.             CL**7
00126                                                                   EL178
00127      EJECT                                                        EL178
00128      COPY ELCDATE.                                                   CL**7
00129                                                                   EL178
00130      EJECT                                                        EL178
00131      COPY ELCLOGOF.                                                  CL**7
00132                                                                   EL178
00133      EJECT                                                        EL178
00134      COPY ELCATTR.                                                   CL**7
00135      EJECT                                                        EL178
00136      COPY ELCEMIB.                                                   CL**7
00137      EJECT                                                        EL178
00138      COPY ELCINTF.                                                   CL**7
00139      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL178
00140 **********************************************************        EL178
00141 *    NOTE                                                *        EL178
00142 *        THE WORK AREA IS USED BY EL178 AND EL1782       *        EL178
00143 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *        EL178
00144 *        BOTH PROGRAMS.                                  *        EL178
00145 **********************************************************        EL178
00146          16  PI-PRINT-DATE       PIC X(8).                        EL178
00147          16  PI-PRINT-DATE-BIN   PIC XX.                          EL178
00148          16  PI-PRINT-ID         PIC X(4).                        EL178
00149          16  PI-STARTING-ARCH-NO PIC S9(08) COMP.                 EL178
00150          16  PI-PRINT-BY-CARR    PIC X.                              CL**2
00151              88  PRINT-BY-CARR              VALUE 'Y'.               CL**2
00152          16  PI-PRINT-CARRIER    PIC X.                              CL**2
00153          16  PI-LETTER-TYPE      PIC X.                              CL**5
00154          16  FILLER              PIC X(619).                         CL**8
00155      EJECT                                                        EL178
00156      COPY ELCAID.                                                    CL**7
00157  01  FILLER    REDEFINES DFHAID.                                  EL178
00158      12  FILLER              PIC X(8).                            EL178
00159      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL178
00160      EJECT                                                        EL178
00161      COPY EL178S.                                                    CL**7
00162      EJECT                                                        EL178
00163  LINKAGE SECTION.                                                 EL178
00164  01  DFHCOMMAREA             PIC X(1024).                         EL178
00165                                                                   EL178
00166      EJECT                                                        EL178
00167      COPY ELCCNTL.                                                   CL**7
00168      EJECT                                                        EL178
00169      COPY ELCARCH.                                                   CL**7
00170      EJECT                                                        EL178
00171      COPY ELCTRLR.                                                   CL**7
00172      EJECT                                                        EL178
00173  PROCEDURE DIVISION.                                              EL178
00174                                                                      CL**8
00175      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL178
00176                                                                   EL178
00177      MOVE 1 TO EMI-NUMBER-OF-LINES.                               EL178
00178                                                                   EL178
00179      IF EIBCALEN = 0                                              EL178
00180          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL178
00181                                                                   EL178
00182      MOVE EIBDATE             TO DC-JULIAN-YYDDD.                 EL178
00183      MOVE '5'                 TO DC-OPTION-CODE.                  EL178
00184      PERFORM 9700-DATE-LINK  THRU  9700-EXIT.                     EL178
00185      MOVE DC-BIN-DATE-1          TO CURRENT-SAVE.                 EL178
00186      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE-EDIT.         EL178
00187                                                                   EL178
00188      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL178
00189          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL178
00190              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL178
00191              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL178
00192              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL178
00193              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL178
00194              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL178
00195              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL178
00196              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL178
00197              MOVE THIS-PGM  TO PI-CALLING-PROGRAM                 EL178
00198              MOVE ZEROS               TO PI-PRINT-DATE            EL178
00199              MOVE LOW-VALUES TO EL178AO                           EL178
00200              GO TO 8100-SEND-INITIAL-MAP.                         EL178
00201                                                                   EL178
00202      EXEC CICS HANDLE CONDITION                                   EL178
00203          PGMIDERR(9600-PGMID-ERROR)                               EL178
00204          ERROR(9990-ABEND)                                        EL178
00205          END-EXEC.                                                EL178
00206                                                                   EL178
00207      IF EIBAID = DFHCLEAR                                         EL178
00208          GO TO 9400-CLEAR.                                        EL178
00209                                                                   EL178
00210      IF PI-PROCESSOR-ID = 'LGXX'                                  EL178
00211          GO TO 0200-RECEIVE.                                      EL178
00212                                                                   EL178
00213      EXEC CICS READQ TS                                           EL178
00214          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL178
00215          INTO    (SECURITY-CONTROL)                               EL178
00216          LENGTH  (SC-COMM-LENGTH)                                 EL178
00217          ITEM    (SC-ITEM)                                        EL178
00218      END-EXEC.                                                    EL178
00219                                                                   EL178
00220      MOVE SC-CLAIMS-DISPLAY (20)  TO  PI-DISPLAY-CAP.             EL178
00221      MOVE SC-CLAIMS-UPDATE  (20)  TO  PI-MODIFY-CAP.              EL178
00222                                                                   EL178
00223      IF NOT DISPLAY-CAP                                           EL178
00224          MOVE 'READ'              TO  SM-READ                        CL**7
00225          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL**8
00226          MOVE ER-0070             TO  EMI-ERROR                   EL178
00227          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL178
00228          GO TO 8100-SEND-INITIAL-MAP.                             EL178
00229                                                                   EL178
00230      EJECT                                                        EL178
00231  0200-RECEIVE.                                                    EL178
00232      MOVE LOW-VALUES TO EL178AI.                                  EL178
00233      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL178
00234          MOVE ER-0008 TO EMI-ERROR                                EL178
00235          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL178
00236          MOVE -1 TO OPTIONL                                       EL178
00237          GO TO 8200-SEND-DATAONLY.                                EL178
00238                                                                   EL178
00239      EXEC CICS RECEIVE                                            EL178
00240          MAP(MAP-NAME)                                            EL178
00241          MAPSET(MAPSET-NAME)                                      EL178
00242          INTO(EL178AI)                                            EL178
00243          END-EXEC.                                                EL178
00244                                                                   EL178
00245      IF ENTERPFL = 0                                              EL178
00246          GO TO 0300-CHECK-PFKEYS.                                 EL178
00247      IF EIBAID NOT = DFHENTER                                     EL178
00248          MOVE ER-0004 TO EMI-ERROR                                EL178
00249          GO TO 0320-INPUT-ERROR.                                  EL178
00250      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)      CL**3
00251          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL178
00252      ELSE                                                         EL178
00253          MOVE ER-0029 TO EMI-ERROR                                EL178
00254          GO TO 0320-INPUT-ERROR.                                  EL178
00255                                                                   EL178
00256  0300-CHECK-PFKEYS.                                               EL178
00257      IF EIBAID = DFHPF23                                          EL178
00258          GO TO 8810-PF23.                                         EL178
00259      IF EIBAID = DFHPF24                                          EL178
00260          GO TO 9200-RETURN-MAIN-MENU.                             EL178
00261      IF EIBAID = DFHPF12                                          EL178
00262          GO TO 9500-PF12.                                         EL178
00263      IF EIBAID = DFHENTER                                         EL178
00264          GO TO 0330-FUNCTION-CHECK.                               EL178
00265                                                                   EL178
00266      MOVE ER-0029 TO EMI-ERROR.                                   EL178
00267  0320-INPUT-ERROR.                                                EL178
00268      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL178
00269      MOVE AL-UNBON TO ENTERPFA.                                   EL178
00270      IF ENTERPFL = 0                                              EL178
00271          MOVE -1 TO OPTIONL                                       EL178
00272      ELSE                                                         EL178
00273          MOVE -1 TO ENTERPFL.                                     EL178
00274      GO TO 8200-SEND-DATAONLY.                                    EL178
00275                                                                   EL178
00276      EJECT                                                        EL178

00277  0330-FUNCTION-CHECK.                                             EL178

00278      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.                    EL178
00279                                                                      CL**7
00280      IF NOT EMI-NO-ERRORS                                         EL178
00281         GO TO 8200-SEND-DATAONLY.                                 EL178
00282                                                                   EL178
00283      IF NOT MODIFY-CAP                                            EL178
00284          IF OPTIONI = '3' OR '4'                                  EL178
00285              NEXT SENTENCE                                        EL178
00286          ELSE                                                     EL178
00287              MOVE 'UPDATE'        TO  SM-READ                     EL178
00288              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**8
00289              MOVE ER-0070         TO  EMI-ERROR                   EL178
00290              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL178
00291              GO TO 8100-SEND-INITIAL-MAP.                         EL178
00292                                                                      CL**7
00293      IF OPTIONI = '7'                                                CL**7
00294          GO TO 7000-START-EL1524.                                    CL**7
00295                                                                   EL178
00296      IF CARRL IS GREATER THAN 0  
00297          MOVE 'Y'                 TO  PI-PRINT-BY-CARR               CL**2
00298                                       WS-COUNT-BY-CARR               CL**2
00299          MOVE CARRI               TO  PI-PRINT-CARRIER               CL**2
00300                                       WS-CARRIER                     CL**2
00301      ELSE                                                            CL**2
00302          MOVE 'N'                 TO  PI-PRINT-BY-CARR               CL**2
00303                                       WS-COUNT-BY-CARR               CL**2
00304          MOVE ' '                 TO  PI-PRINT-CARRIER               CL**2
00305                                       WS-CARRIER.                    CL**2
00306                                                                      CL**2
00307      IF OPTIONI = '1'                                             EL178
00308          GO TO 1000-PRINT-INITIAL-LETTER.                         EL178

00309      IF OPTIONI = '2'                                             EL178
00310          GO TO 2000-PRINT-FOLLOW-UP-LETTERS.                      EL178

00311      IF OPTIONI = '3'                                             EL178
00312          GO TO 3000-SHOW-COUNT-OF-INITIAL.                        EL178

00313      IF OPTIONI = '4'                                             EL178
00314          GO TO 4000-SHOW-COUNT-OF-FOLLOW-UP.                      EL178

00315      IF OPTIONI = '5'                                             EL178
00316          GO TO 5000-PRINT-ADDRESS-LABELS.                         EL178

00317      IF OPTIONI = '6'                                             EL178
00318          GO TO 6000-REPRINT-LETTERS-BY-DATE.                      EL178

110402     IF OPTIONI = '8'                                             EL178
110402         GO TO 6700-REPRINT-LETTERS-BY-CLAIM.                     EL178

00319      EJECT                                                        EL178

00320  0350-EDIT-ROUTINE.                                               EL178

110402     IF OPTIONI LESS THAN '1' OR GREATER THAN '8'                    CL**7
00322         MOVE -1                  TO OPTIONL                       EL178
00323         MOVE ER-0409             TO EMI-ERROR                     EL178
00324         MOVE AL-UNBON            TO OPTIONA                       EL178
00325         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL178
00326      ELSE                                                         EL178
00327         MOVE AL-UNNON            TO OPTIONA.                      EL178

110402     IF (OPTIONI = '8')  AND 
110402        CARRL = ZEROS      
110402        MOVE -1                  TO CARRL 
110402        MOVE ER-0194             TO EMI-ERROR                     EL178
110402        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL178
00328                                                                      CL**7
110402     IF (OPTIONI = '8')  AND 
110402        CLAIML = ZEROS    
110402        MOVE -1                  TO CLAIML
110402        MOVE ER-0209             TO EMI-ERROR                     EL178
110402        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL178

110402     IF (OPTIONI = '8')  AND 
110402        CERTL = ZEROS    
110402        MOVE -1                  TO CERTL
110402        MOVE ER-0203             TO EMI-ERROR                     EL178
110402        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL178

00329      IF OPTIONI = '7'                                                CL**7
00330         GO TO 0350-CONTINUE.                                         CL**7
00331                                                                   EL178
00332      IF (OPTIONI = '5' OR = '6')  AND                             EL178
00333         DATEINL = ZEROS                                           EL178
00334         MOVE -1                  TO DATEINL                       EL178
00335         MOVE ER-0410             TO EMI-ERROR                     EL178
00336         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL178
00337                                                                   EL178
110402     IF DATEINL NOT = ZEROS  AND
110402        (OPTIONI = '5' OR = '6')       
00339         MOVE DATEINI             TO DEEDIT-FIELD                  EL178
00340         PERFORM 8600-DEEDIT                                       EL178
00341         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL178
00342         MOVE '4'                 TO DC-OPTION-CODE                EL178
00343         PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   EL178
00344         IF DATE-CONVERSION-ERROR                                  EL178
00345            MOVE ER-0182          TO EMI-ERROR                     EL178
00346            MOVE -1               TO DATEINL                       EL178
00347            MOVE AL-UABON         TO DATEINA                       EL178
00348            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             EL178
00349            ELSE                                                   EL178
00350            MOVE AL-UANON         TO DATEINA                       EL178
00351            MOVE DC-BIN-DATE-1    TO PI-PRINT-DATE-BIN             EL178
00352            MOVE DC-GREG-DATE-1-EDIT   TO PI-PRINT-DATE            EL178
00353                                          DATEINI                  EL178
00354         ELSE                                                      EL178
00355         MOVE LOW-VALUE           TO PI-PRINT-DATE-BIN.            EL178
00356                                                                      CL**5
110402     IF LETRTYPL NOT = ZEROS AND
110402        (OPTIONI = '1' OR = '2') 
00358          IF LETRTYPI = 'P'  OR  'I'                                  CL**5
00359              MOVE LETRTYPI  TO  PI-LETTER-TYPE                       CL**5
00360              MOVE AL-UANON  TO  LETRTYPA                             CL**5
00361          ELSE                                                        CL**5
00362              MOVE ER-0669   TO  EMI-ERROR                            CL**5
00363              MOVE -1        TO  LETRTYPL                             CL**5
00364              MOVE AL-UABON  TO  LETRTYPA                             CL**5
00365              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**5
00366                                                                      CL**5
00367      IF NOT EMI-NO-ERRORS                                         EL178
00368         GO TO 0350-EXIT.                                          EL178
00369                                                                      CL**7
00370  0350-CONTINUE.                                                      CL**7
00371                                                                   EL178
00372      PERFORM 7900-READ-CONTROL-FILE THRU 7999-EXIT.               EL178
00373                                                                   EL178
00374  0350-EXIT.                                                       EL178
00375       EXIT.                                                       EL178
00376                                                                   EL178
00377  400-SET-CODES.                                                   EL178
00378      MOVE PI-COMPANY-CD TO ARCH-CO                                EL178
00379                            ACTV-CO.                               EL178
00380      EJECT                                                        EL178
00381  1000-PRINT-INITIAL-LETTER.                                       EL178
00382      MOVE '1'                    TO PI-ENTRY-CD-1                 EL178
00383                                     PI-ENTRY-CD-2.                EL178
00384      GO TO 7800-START-PRINT.                                      EL178
00385                                                                   EL178
00386  2000-PRINT-FOLLOW-UP-LETTERS.                                    EL178
00387      MOVE '1'                    TO PI-ENTRY-CD-1.                EL178
00388      MOVE '2'                    TO PI-ENTRY-CD-2.                EL178
00389      GO TO 7800-START-PRINT.                                      EL178
00390                                                                   EL178
00391  3000-SHOW-COUNT-OF-INITIAL.                                      EL178
00392      PERFORM 7500-BROWSE-ARCHIVE THRU 7599-EXIT.                  EL178
00393      GO TO 6500-COMPLETE-COUNT.                                   EL178
00394                                                                   EL178
00395  4000-SHOW-COUNT-OF-FOLLOW-UP.                                    EL178
00396      PERFORM 7500-BROWSE-ARCHIVE THRU 7599-EXIT.                  EL178
00397      GO TO 6500-COMPLETE-COUNT.                                   EL178
00398                                                                   EL178
00399  5000-PRINT-ADDRESS-LABELS.                                       EL178
00400      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL178
00401      MOVE '2'                    TO PI-ENTRY-CD-2.                EL178
00402      GO TO 7800-START-PRINT.                                      EL178
00403                                                                   EL178
00404  6000-REPRINT-LETTERS-BY-DATE.                                       CL**7
00405      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL178
00406      MOVE '3'                    TO PI-ENTRY-CD-2.                EL178
00407      GO TO 7800-START-PRINT.                                      EL178
00408                                                                   EL178
00409  6500-COMPLETE-COUNT.                                             EL178
00410      MOVE WS-COUNTER             TO COUNTO.                       EL178
00411      MOVE -1                     TO OPTIONL.                      EL178
00412      MOVE ER-ZEROS               TO EMI-ERROR.                    EL178
00413      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL178
00414      GO TO 8200-SEND-DATAONLY.                                    EL178
00415      EJECT                                                           CL**7

110402 6700-REPRINT-LETTERS-BY-CLAIM.                                      CL**7
110402     MOVE SPACE                  TO PI-ENTRY-CD-1.                EL178
110402     MOVE '4'                    TO PI-ENTRY-CD-2.                EL178
110402     MOVE CLAIMI                 TO PI-CLAIM-NO. 
110402     MOVE CERTI                  TO PI-CERT-NO.
110402     GO TO 7800-START-PRINT.                                      EL178

00416  7000-START-EL1524.                                                  CL**7
00417                                                                      CL**7
CIDMOD 7010-START.                                                         CL**7
CIDMOD                                                                     CL**7
00418      EXEC CICS HANDLE CONDITION                                      CL**7
00419           TERMIDERR   (8820-TERMID-ERROR)                            CL**7
00420           TRANSIDERR  (8830-TRANS-ERROR)                             CL**7
00421           END-EXEC.                                                  CL**7
00422                                                                      CL**7
00423  7020-START.                                                         CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**9
00425 *        MOVE EIBTRMID       TO  PI-PRINT-ID                         CL**9
00426          EXEC CICS START                                             CL**9
00427               INTERVAL    (0)                                        CL**9
00428               TRANSID     (W-EL1524-TRANS)                           CL**9
00429               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**9
00430               LENGTH      (PI-COMM-LENGTH)                           CL**9
00431 *             TERMID      (PI-PRINT-ID)                              CL**9
00432               END-EXEC                                               CL**9
00433      ELSE                                                            CL**9
00434          EXEC CICS START                                             CL**9
00435               INTERVAL    (0)                                        CL**9
00436               TRANSID     (W-EL1524-TRANS)                           CL**9
00437               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**9
00438               LENGTH      (PI-COMM-LENGTH)                           CL**9
00439               TERMID      (PI-PRINT-ID)                              CL**9
00440               END-EXEC.                                              CL**9
00441                                                                      CL**7
00442      MOVE -1                     TO OPTIONL.                         CL**7
00443      MOVE ER-3767                TO EMI-ERROR.                       CL**7
00444      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**7
00445      GO TO 8200-SEND-DATAONLY.                                       CL**7
00446                                                                      CL**7
00447      EJECT                                                        EL178
00448  7500-BROWSE-ARCHIVE.                                             EL178
00449      MOVE LOW-VALUES             TO ARCH-KEY.                     EL178
00450      MOVE ZEROS                  TO ARCH-SEQ.                     EL178
00451      MOVE PI-STARTING-ARCH-NO TO ARCH-NUMBER.                     EL178
00452      PERFORM 400-SET-CODES.                                       EL178
00453      MOVE '1'                    TO ARCH-REC-TYPE.                EL178
00454      EXEC CICS HANDLE CONDITION                                   EL178
00455           NOTFND(7599-EXIT)                                       EL178
00456           ENDFILE(7599-EXIT)                                      EL178
00457           NOTOPEN(8850-ARCH-NOT-OPEN)                             EL178
00458           END-EXEC.                                               EL178
00459                                                                   EL178
00460      EXEC CICS STARTBR                                            EL178
00461           DATASET(ARCH-ID)                                        EL178
00462           RIDFLD(ARCH-KEY)                                        EL178
00463           END-EXEC.                                               EL178
00464                                                                   EL178
00465  7505-RESET-HANDLE.                                               EL178
00466      EXEC CICS HANDLE CONDITION                                   EL178
00467           NOTFND(7599-EXIT)                                       EL178
00468           ENDFILE(7599-EXIT)                                      EL178
00469           NOTOPEN(8850-ARCH-NOT-OPEN)                             EL178
00470           END-EXEC.                                               EL178
00471                                                                   EL178
00472  7510-READ-NEXT.                                                  EL178
00473      EXEC CICS READNEXT                                           EL178
00474           DATASET(ARCH-ID)                                        EL178
00475           RIDFLD (ARCH-KEY)                                       EL178
00476           SET    (ADDRESS OF LETTER-ARCHIVE)                         CL**8
00477           END-EXEC                                                EL178
00478                                                                   EL178
00479      IF ARCH-CO NOT = PI-COMPANY-CD OR ARCH-REC-TYPE NOT = '1'    EL178
00480         GO TO 7599-EXIT.                                          EL178
00481                                                                      CL**3
00482      ADD +1                      TO  WS-RECORD-COUNT.                CL**3
00483      IF WS-RECORD-COUNT IS GREATER THAN +100                         CL**3
00484          MOVE +0                 TO  WS-RECORD-COUNT                 CL**3
00485          EXEC CICS DELAY                                             CL**3
00486              INTERVAL  (WS-DELAY-INTERVAL)                           CL**3
00487          END-EXEC.                                                   CL**3
00488                                                                      CL**2
00489      IF COUNT-BY-CARR                                                CL**2
00490          IF LA-CARRIER IS EQUAL TO WS-CARRIER                        CL**2
00491              NEXT SENTENCE                                           CL**2
00492          ELSE                                                        CL**2
00493              GO TO 7510-READ-NEXT.                                   CL**2
00494                                                                   EL178
00495      IF OPTIONI = '3'                                             EL178
00496         IF LA-INITIAL-PRINT-DATE = LOW-VALUES                     EL178
00497            ADD 1                 TO WS-COUNTER                    EL178
00498            GO TO 7510-READ-NEXT                                   EL178
00499         ELSE                                                      EL178
00500            GO TO 7510-READ-NEXT.                                  EL178
00501                                                                   EL178
00502      IF LA-RESEND-DATE = LOW-VALUES                               EL178
00503         GO TO 7510-READ-NEXT.                                     EL178
00504                                                                   EL178
00505      IF LA-RESEND-DATE GREATER THAN CURRENT-SAVE                  EL178
00506         GO TO 7510-READ-NEXT.                                     EL178
00507                                                                   EL178
00508      IF LA-RESEND-PRINT-DATE NOT EQUAL TO LOW-VALUES              EL178
00509          GO TO 7510-READ-NEXT.                                    EL178
00510                                                                   EL178
00511      MOVE LA-CARRIER             TO ACTV-CARRIER.                 EL178
00512      MOVE LA-CLAIM-NO            TO ACTV-CLAIM.                   EL178
00513      MOVE LA-CERT-NO             TO ACTV-CERT-NO.                 EL178
00514      MOVE LA-CORR-TRLR-SEQ       TO ACTV-SEQ.                     EL178
00515      EXEC CICS HANDLE CONDITION                                   EL178
00516           NOTFND(7510-READ-NEXT)                                  EL178
00517           NOTOPEN(8870-ACTV-NOT-OPEN)                             EL178
00518           END-EXEC.                                               EL178
00519                                                                   EL178
00520      EXEC CICS READ                                               EL178
00521           DATASET(ACTV-ID)                                        EL178
00522           RIDFLD(ACTV-KEY)                                        EL178
00523           SET(ADDRESS OF ACTIVITY-TRAILERS)                          CL**8
00524           END-EXEC.                                               EL178
00525                                                                   EL178
00526                                                                   EL178
00527      IF NOT CORRESPONDENCE-TR                                     EL178
00528        GO TO 7505-RESET-HANDLE.                                   EL178
00529                                                                   EL178
00530      IF AT-LETTER-ANSWERED-DT  = LOW-VALUES  AND                  EL178
00531         AT-AUTO-RE-SEND-DT NOT = LOW-VALUES                       EL178
00532         ADD 1                    TO WS-COUNTER                    EL178
00533        GO TO 7505-RESET-HANDLE.                                   EL178
00534                                                                   EL178
00535      EXEC CICS HANDLE CONDITION                                   EL178
00536           NOTOPEN(8850-ARCH-NOT-OPEN)                             EL178
00537           END-EXEC.                                               EL178
00538                                                                   EL178
00539      EXEC CICS READ                                               EL178
00540           DATASET('ELARCH  ')                                     EL178
00541           RIDFLD(LA-CONTROL-PRIMARY)                              EL178
00542           SET(ADDRESS OF LETTER-ARCHIVE)                             CL**8
00543           UPDATE                                                  EL178
00544           END-EXEC.                                               EL178
00545                                                                   EL178
00546      MOVE LOW-VALUES             TO LA-RESEND-DATE.               EL178
00547      EXEC CICS HANDLE CONDITION                                   EL178
00548          DUPKEY (7505-RESET-HANDLE)                               EL178
00549          END-EXEC.                                                EL178
00550                                                                   EL178
00551      EXEC CICS REWRITE                                            EL178
00552           DATASET('ELARCH  ')                                     EL178
00553           FROM(LETTER-ARCHIVE)                                    EL178
00554           END-EXEC.                                               EL178
00555                                                                   EL178
00556        GO TO 7505-RESET-HANDLE.                                   EL178
00557                                                                   EL178
00558  7599-EXIT.                                                       EL178
00559       EXIT.                                                       EL178
00560      EJECT                                                        EL178

00561  7800-START-PRINT.                                                EL178

00562      PERFORM 400-SET-CODES.                                       EL178
00563      EXEC CICS HANDLE CONDITION                                   EL178
00564           TERMIDERR   (8820-TERMID-ERROR)                         EL178
00565           TRANSIDERR  (8830-TRANS-ERROR)                          EL178
00566           END-EXEC.                                               EL178
00567                                                                   EL178
00568  7820-START.                                                      EL178
CIDMOD*    MOVE PI-PRINT-ID TO PI-PROCESSOR-PRINTER.                         000
CIDMOD*                                                                      000
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**9
CIDMOD         MOVE EIBTRMID       TO  PI-PRINT-ID                         CL**9
00571          EXEC CICS START                                             CL**9
00572               INTERVAL    (0)                                        CL**9
00573               TRANSID     (PRINT-TRANS)                              CL**9
00574               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**9
00575               LENGTH      (PI-COMM-LENGTH)                           CL**9
CIDMOD*             TERMID      (PI-PRINT-ID)                              CL**9
CIDMOD              TERMID      (PI-PRINT-ID)                              CL**9
00577          END-EXEC                                                    CL**9
00578      ELSE                                                            CL**9
00579          EXEC CICS START                                             CL**9
00580               INTERVAL    (0)                                        CL**9
00581               TRANSID     (PRINT-TRANS)                              CL**9
00582               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**9
00583               LENGTH      (PI-COMM-LENGTH)                           CL**9
00584               TERMID      (PI-PRINT-ID)                              CL**9
00585          END-EXEC.                                                   CL**9
00586                                                                   EL178
00587      IF OPTIONI = '5'                                             EL178
00588         MOVE ER-0411             TO EMI-ERROR                     EL178
00589      ELSE                                                         EL178
00590         MOVE ER-0189             TO EMI-ERROR.                    EL178
00591                                                                   EL178
00592      MOVE -1                     TO OPTIONL.                      EL178
00593      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL178
00594      GO TO 8200-SEND-DATAONLY.                                    EL178
00595      EJECT                                                        EL178

00596  7900-READ-CONTROL-FILE.                                          EL178

00597      MOVE PI-COMPANY-ID          TO CNTL-CO.                      EL178
00598      EXEC CICS HANDLE CONDITION                                   EL178
00599           NOTOPEN     (8840-CNTL-NOT-OPEN)                        EL178
00600           NOTFND      (7990-NOT-FOUND)                            EL178
00601           END-EXEC.                                               EL178
00602                                                                   EL178
00603      MOVE '1'                    TO CNTL-RECORD-TYPE.             EL178
00604      MOVE SPACES                 TO CNTL-GENL.                    EL178
00605      MOVE ZEROS                  TO CNTL-SEQ.                     EL178
00606      EXEC CICS READ                                               EL178
00607           DATASET(CNTL-ID)                                        EL178
00608           SET(ADDRESS OF CONTROL-FILE)                               CL**8
00609           RIDFLD(CNTL-KEY)                                        EL178
00610           END-EXEC.                                               EL178
00611                                                                   EL178
00612      MOVE CF-FORMS-PRINTER-ID    TO PI-PRINT-ID.                  EL178
00613                                                                      CL**6
00614      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES                  CL**6
00615          MOVE PI-PROCESSOR-PRINTER   TO  PI-PRINT-ID.                CL**6
00616                                                                      CL**6
00617      IF PRINTERL NOT = ZEROS                                      EL178
00618         MOVE PRINTERI            TO PI-PRINT-ID                      CL**9
00619                                     PI-ALT-DMD-PRT-ID.               CL**9
00620                                                                   EL178
00621      MOVE CF-STARTING-ARCH-NO    TO PI-STARTING-ARCH-NO.          EL178
00622                                                                   EL178
00623      GO TO 7999-EXIT.                                             EL178
00624                                                                   EL178
00625  7990-NOT-FOUND.                                                  EL178
00626      MOVE ER-0190                TO EMI-ERROR.                    EL178
00627      MOVE -1                     TO OPTIONL.                      EL178
00628      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   EL178
00629                                                                   EL178
00630  7999-EXIT.                                                       EL178
00631      EXIT.                                                        EL178
00632      EJECT                                                        EL178
00633  8100-SEND-INITIAL-MAP.                                           EL178
00634      MOVE WS-CURRENT-DATE-EDIT   TO DATEAO.                       EL178
00635      MOVE EIBTIME                TO TIME-IN.                      EL178
00636      MOVE TIME-OUT               TO TIMEAO.                       EL178
00637      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGO                         EL178
00638      MOVE -1                     TO OPTIONL.                      EL178
00639      EXEC CICS SEND                                               EL178
00640          MAP(MAP-NAME)                                            EL178
00641          MAPSET(MAPSET-NAME)                                      EL178
00642          FROM(EL178AO)                                            EL178
00643          ERASE                                                    EL178
00644          CURSOR                                                   EL178
00645          END-EXEC.                                                EL178
00646                                                                   EL178
00647      GO TO 9100-RETURN-TRAN.                                      EL178
00648                                                                   EL178
00649  8200-SEND-DATAONLY.                                              EL178
00650      MOVE WS-CURRENT-DATE-EDIT   TO DATEAO.                       EL178
00651      MOVE EIBTIME                TO TIME-IN.                      EL178
00652      MOVE TIME-OUT               TO TIMEAO.                       EL178
00653      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGO                         EL178
00654      EXEC CICS SEND                                               EL178
00655          MAP(MAP-NAME)                                            EL178
00656          MAPSET(MAPSET-NAME)                                      EL178
00657          FROM(EL178AO)                                            EL178
00658          DATAONLY                                                 EL178
00659          ERASEAUP                                                 EL178
00660          CURSOR                                                   EL178
00661          END-EXEC.                                                EL178
00662                                                                   EL178
00663      GO TO 9100-RETURN-TRAN.                                      EL178
00664                                                                   EL178
00665  8300-SEND-TEXT.                                                  EL178
00666      EXEC CICS SEND TEXT                                          EL178
00667          FROM(LOGOFF-TEXT)                                        EL178
00668          LENGTH(LOGOFF-LENGTH)                                    EL178
00669          ERASE                                                    EL178
00670          FREEKB                                                   EL178
00671          END-EXEC.                                                EL178
00672      EXEC CICS RETURN                                             EL178
00673          END-EXEC.                                                EL178
00674                                                                   EL178
00675  8600-DEEDIT.                                                     EL178
00676      EXEC CICS BIF DEEDIT                                         EL178
00677           FIELD(DEEDIT-FIELD)                                     EL178
00678           LENGTH(15)                                              EL178
00679           END-EXEC.                                               EL178
00680      EJECT                                                        EL178
00681  8800-UNAUTHORIZED-ACCESS.                                        EL178
00682      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL178
00683      GO TO 8300-SEND-TEXT.                                        EL178
00684                                                                   EL178
00685  8810-PF23.                                                       EL178
00686      MOVE EIBAID TO PI-ENTRY-CD-1.                                EL178
00687      MOVE XCTL-005 TO PGM-NAME.                                   EL178
00688      GO TO 9300-XCTL.                                             EL178
00689                                                                   EL178
00690  8820-TERMID-ERROR.                                               EL178
00691      MOVE ER-0412                TO EMI-ERROR                     EL178
00692      GO TO 8999-OPEN-ERROR.                                       EL178
00693                                                                   EL178
00694  8830-TRANS-ERROR.                                                EL178
00695      MOVE ER-0413                TO EMI-ERROR                     EL178
00696      GO TO 8999-OPEN-ERROR.                                       EL178
00697                                                                   EL178
00698  8840-CNTL-NOT-OPEN.                                              EL178
00699      MOVE ER-0042 TO EMI-ERROR.                                   EL178
00700      GO TO 8999-OPEN-ERROR.                                       EL178
00701                                                                   EL178
00702  8850-ARCH-NOT-OPEN.                                              EL178
00703      MOVE ER-0408 TO EMI-ERROR.                                   EL178
00704      GO TO 8999-OPEN-ERROR.                                       EL178
00705                                                                   EL178
00706  8870-ACTV-NOT-OPEN.                                              EL178
00707      MOVE ER-0172 TO EMI-ERROR.                                   EL178
00708      GO TO 8999-OPEN-ERROR.                                       EL178
00709                                                                   EL178
00710  8999-OPEN-ERROR.                                                 EL178
00711      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL178
00712      MOVE -1 TO OPTIONL.                                          EL178
00713      GO TO 8200-SEND-DATAONLY.                                    EL178
00714                                                                   EL178
00715  9100-RETURN-TRAN.                                                EL178
00716      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL178
00717      MOVE MAP-NUMBER             TO PI-CURRENT-SCREEN-NO.         EL178
00718      EXEC CICS RETURN                                             EL178
00719          TRANSID(TRANS-ID)                                        EL178
00720          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL178
00721          LENGTH(PI-COMM-LENGTH)                                   EL178
00722          END-EXEC.                                                EL178
00723                                                                   EL178
00724  9200-RETURN-MAIN-MENU.                                           EL178
00725      MOVE XCTL-126 TO PGM-NAME.                                   EL178
00726      GO TO 9300-XCTL.                                             EL178
00727                                                                   EL178
00728  9300-XCTL.                                                       EL178
00729      EXEC CICS XCTL                                               EL178
00730          PROGRAM(PGM-NAME)                                        EL178
00731          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL178
00732          LENGTH(PI-COMM-LENGTH)                                   EL178
00733          END-EXEC.                                                EL178
00734                                                                   EL178
00735  9400-CLEAR.                                                      EL178
00736      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL178
00737      GO TO 9300-XCTL.                                             EL178
00738                                                                   EL178
00739  9500-PF12.                                                       EL178
00740      MOVE XCTL-010 TO PGM-NAME.                                   EL178
00741      GO TO 9300-XCTL.                                             EL178
00742                                                                   EL178
00743  9600-PGMID-ERROR.                                                EL178
00744      EXEC CICS HANDLE CONDITION                                   EL178
00745          PGMIDERR(8300-SEND-TEXT)                                 EL178
00746          END-EXEC.                                                EL178
00747                                                                   EL178
00748      MOVE ' '          TO PI-ENTRY-CD-1.                          EL178
00749      MOVE XCTL-005     TO PGM-NAME.                               EL178
00750      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL178
00751      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL178
00752      GO TO 9300-XCTL.                                             EL178
00753                                                                   EL178
00754  9700-DATE-LINK.                                                  EL178
00755      MOVE LINK-ELDATCV TO PGM-NAME.                                  CL*10
00756      EXEC CICS LINK                                               EL178
00757          PROGRAM(PGM-NAME)                                        EL178
00758          COMMAREA(DATE-CONVERSION-DATA)                           EL178
00759          LENGTH(DC-COMM-LENGTH)                                   EL178
00760          END-EXEC.                                                EL178
00761                                                                   EL178
00762  9700-EXIT.                                                       EL178
00763       EXIT.                                                       EL178
00764                                                                   EL178
00765  9900-ERROR-FORMAT.                                               EL178
00766      IF NOT EMI-ERRORS-COMPLETE                                   EL178
00767          MOVE LINK-001 TO PGM-NAME                                EL178
00768          EXEC CICS LINK                                           EL178
00769              PROGRAM(PGM-NAME)                                    EL178
00770              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL178
00771              LENGTH(EMI-COMM-LENGTH)                              EL178
00772              END-EXEC.                                            EL178
00773  9900-EXIT.                                                       EL178
00774      EXIT.                                                        EL178
00775                                                                   EL178
00776  9990-ABEND.                                                      EL178
00777      MOVE LINK-004 TO PGM-NAME.                                   EL178
00778      MOVE DFHEIBLK TO EMI-LINE1.                                  EL178
00779      EXEC CICS LINK                                               EL178
00780          PROGRAM(PGM-NAME)                                        EL178
00781          COMMAREA(EMI-LINE1)                                      EL178
00782          LENGTH(72)                                               EL178
00783          END-EXEC.                                                EL178
00784                                                                   EL178
00785      GO TO 8200-SEND-DATAONLY.                                    EL178
00786                                                                   EL178
00787  9995-SECURITY-VIOLATION.                                         EL178
00788                              COPY ELCSCTP.                        EL178
00789                                                                   EL178
00790  9995-EXIT.                                                       EL178
00791      EXIT.                                                        EL178
00792                                                                   EL178
