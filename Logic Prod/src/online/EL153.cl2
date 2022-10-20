00001  IDENTIFICATION DIVISION.                                         10/07/97
00002                                                                   EL153
00003  PROGRAM-ID.                 EL153 .                                 LV019
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 06/20/94 09:17:09.                    CL*11
00007 *                            VMOD=2.019                              CL*19
00008 *                                                                 EL153
00008 *                                                                 EL153
00009 *AUTHOR.    LOGIC, INC.                                              CL*11
00010 *           DALLAS, TEXAS.                                           CL*11
00011                                                                   EL153
00012 *DATE-COMPILED.                                                      CL*11
00013                                                                   EL153
00014 *SECURITY.   *****************************************************   CL*11
00015 *            *                                                   *   CL*11
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00017 *            *                                                   *   CL*11
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *   CL*11
00021 *            *                                                   *   CL*11
00022 *            *****************************************************   CL*11
00023                                                                   EL153
00024 *REMARKS.                                                            CL**2
00025 *    SCREENS     - EL153S - NOTES AND REMINDERS                      CL**2
00026                                                                   EL153
00027 *    ENTERED BY  - EL150 - STATUS AND DISPOSITION                    CL**2
00028                                                                   EL153
00029 *    EXIT TO     - EL150 - CALLING PROGRAM                           CL**2
00030                                                                   EL153
00031 *    INPUT FILE  - ELMSTR - CLAIM MASTER                             CL**2
00032 *                - ELTRLR - ACTIVITY TRAILERS                        CL**2
00033                                                                   EL153
00034 *    OUTPUT FILE - ELMSTR - CLAIM MASTER                             CL**2
00035 *                - ELTRLR - ACTIVITY TRAILERS                        CL**2
00036                                                                   EL153
00037 *    COMMAREA    - PASSED CLAIM NUMBER FROM PROG INTERFACE BLK       CL*16
00038                                                                   EL153
00039 *    ERROR-CODES ACCESSED - 132, 314, 133, 137, 29, 50, 315,         CL**2
00040 *                            316, 317, 08                            CL*11
00041 *    NARRATIVE   - PROVIDE CREATION OF AUTO-PROMPT-TRAILER           CL**2
00042 *                  AND GENERAL-INFO-TRAILER                          CL**2
00043 *                  IF THE AUTO PROMPT DATE GIVEN  IS LOWER           CL**2
00044 *                  THAN THE NEXT-REMINDER DATE OF THE CLAIM MASTER   CL**2
00045 *                  THIS FIELD IS UPDATED IN THE CLAIM MASTER         CL**2
00046                                                                   EL153
102901******************************************************************
102901*                   C H A N G E   L O G
102901*
102901* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102901*-----------------------------------------------------------------
102901*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102901* EFFECTIVE    NUMBER
102901*-----------------------------------------------------------------
102901* 031102    2002022100003  SMVA  ADD CERT# TO EL153A SCREEN HEADER
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
080106* 080106    2006052500001  AJRA  ADD NOTE TYPE 'N'(NOTE AND FILE)
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
102418* 102418  CR2018083000001  TANA  ADD NEW CALL TYPE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
102901******************************************************************

00047      EJECT                                                        EL153
00048  ENVIRONMENT DIVISION.                                            EL153
00049                                                                   EL153
00050  DATA DIVISION.                                                   EL153
00051                                                                   EL153
00052  WORKING-STORAGE SECTION.                                         EL153
00053                                                                   EL153
00054  77  FILLER  PIC X(32)  VALUE '********************************'. EL153
00055  77  FILLER  PIC X(32)  VALUE '*   EL153  WORKING STORAGE     *'. EL153
00056  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.019 *********'.    CL*19
00057                                                                   EL153
00058                                      COPY ELCSCTM.                   CL**8
00059                                                                   EL153
00060                                      COPY ELCSCRTY.                  CL**8
00061                                                                   EL153
00062  01  WS-DATE-AREA.                                                EL153
00063      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.       CL*16
00064      12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL*16
00065      12  CURRENT-PLUS3-SAVE          PIC XX      VALUE SPACES.       CL*17
00066                                                                   EL153
00067  01  WS-SCRATCH-AREA.                                             EL153
00068      12  GETMAIN-SPACE               PIC X       VALUE SPACE.        CL*16
00069      12  WS-TRLR-LENGTH              PIC S9(4)   VALUE +200  COMP.   CL**8
00070      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.   CL**8
00071                                                                   EL153
00072      12  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.         CL*16
00073                                                                   EL153
00074      12  WS-CURSOR                   PIC S9(4)   VALUE -1    COMP.   CL**8
00075                                                                      CL**4
00076      12  WS-MAP-NAME                 PIC X(8)    VALUE 'EL153A'.     CL*16
00077      12  WS-MAPSET-NAME              PIC X(8)    VALUE 'EL153S'.     CL*16
00078                                                                   EL153
00079      12  WS-TRANS-ID                 PIC X(4)    VALUE 'EX26'.       CL*16
00080      12  THIS-PGM                    PIC X(8)    VALUE  'EL153'.     CL*16
00081                                                                   EL153
00082      12  TIME-OUT.                                                   CL**8
00083          16  WS-TRANS-HOUR           PIC XX      VALUE SPACE.        CL*16
00084          16  FILLER                  PIC X       VALUE '.'.          CL*16
00085          16  WS-TRANS-MINUTE         PIC XX      VALUE SPACE.        CL*16
00086                                                                   EL153
00087      12  WS-DATE-UNEDIT.                                             CL**8
00088          16  FILLER                  PIC XX.                         CL*16
00089          16  WS-DATE-MDY.                                            CL**8
00090              20  WS-DATE-MM          PIC XX.                         CL*16
00091              20  WS-DATE-DD          PIC XX.                         CL*16
00092              20  WS-DATE-YY          PIC XX.                         CL*16
00093                                                                   EL153
00094      12  WS-STR-DATE-BIN             PIC XX      VALUE LOW-VALUES.   CL*16
00095      12  WS-END-DATE-BIN             PIC XX      VALUE LOW-VALUES.   CL*16
00096                                                                   EL153
00097      12  SUB                         PIC S99     VALUE ZEROS.        CL*16
00098      12  SUB-1                       PIC S99     VALUE ZEROS.        CL*16
00099                                                                      CL**4
00100      12  WS-FIRST-TIME-SW            PIC X       VALUE 'Y'.          CL*16
00101                                                                   EL153
00102      12  WS-EDIT-NOTE.                                               CL**8
00103          16  WS-EDIT-NOTE-1-4.                                       CL**8
00104              20  WS-EDIT-NOTE-1-3    PIC X(3).                       CL*16
00105              20  FILLER              PIC X.                          CL*16
00106          16  FILLER                  PIC X(56).                      CL**8
00107                                                                   EL153
00108      12  WS-DATE-ERROR-SW            PIC X       VALUE SPACE.        CL*16
00109          88  DATE-ERROR                          VALUE 'X'.          CL**8
00110                                                                   EL153
062602     12  W-NOTE-TYPE                 PIC X       VALUE SPACE.        CL*16
00111      12  W-CALL-TYPE                 PIC X       VALUE SPACE.        CL*16
00112      12  W-NOTE-TYPE-IND             PIC X       VALUE SPACE.        CL*16
00113                                                                      CL*12
00114      12  TIME-IN                     PIC S9(7).                      CL*16
00115      12  WS-TIME  REDEFINES TIME-IN.                                 CL**8
00116          16  FILLER                  PIC 9.                          CL*16
00117          16  WS-HOUR                 PIC 99.                         CL*16
00118          16  WS-MINUTE               PIC 99.                         CL*16
00119          16  FILLER                  PIC 99.                         CL*16
00120                                                                      CL**8
00121      12  WS-TRAILER-KEY.                                             CL**8
00122          16  WS-CLAIM-KEY.                                           CL**8
00123              20  WS-KEY-COMPANY-CD       PIC X.                      CL*16
00124              20  WS-KEY-CARRIER          PIC X.                      CL*16
00125              20  WS-KEY-CLAIM-NO         PIC X(7).                   CL*16
00126              20  WS-KEY-CERT-NO.                                     CL**8
00127                  24  WS-KEY-CERT-PRIME   PIC X(10).                  CL**8
00128                  24  WS-KEY-CERT-SFX     PIC X.                      CL*16
00129          16  WS-KEY-SEQUENCE-NO          PIC S9(4) COMP.             CL*16
00130                                                                   EL153
00131      EJECT                                                        EL153
00132  01  ERROR-MESSAGES.                                              EL153
00133      12  ER-0000                     PIC X(4)    VALUE '0000'.       CL*16
00134      12  ER-0004                     PIC X(4)    VALUE '0004'.       CL*16
00135      12  ER-0008                     PIC X(4)    VALUE '0008'.       CL*16
00136      12  ER-0029                     PIC X(4)    VALUE '0029'.       CL*16
00137      12  ER-0050                     PIC X(4)    VALUE '0050'.       CL*16
00138      12  ER-0070                     PIC X(4)    VALUE '0070'.       CL*16
00139      12  ER-0132                     PIC X(4)    VALUE '0132'.       CL*16
00140      12  ER-0133                     PIC X(4)    VALUE '0133'.       CL*16
00141      12  ER-0137                     PIC X(4)    VALUE '0137'.       CL*16
00142      12  ER-0154                     PIC X(4)    VALUE '0154'.       CL*16
00143      12  ER-0172                     PIC X(4)    VALUE '0172'.       CL*16
00144      12  ER-0314                     PIC X(4)    VALUE '0314'.       CL*16
00145      12  ER-0316                     PIC X(4)    VALUE '0316'.       CL*16
00146      12  ER-0317                     PIC X(4)    VALUE '0317'.       CL*16
00147      12  ER-0483                     PIC X(4)    VALUE '0483'.       CL*16
00148      12  ER-0694                     PIC X(4)    VALUE '0694'.       CL*16
00149      12  ER-0914                     PIC X(4)    VALUE '0914'.       CL*16
00150      12  ER-0915                     PIC X(4)    VALUE '0915'.       CL*16
00151      12  ER-0916                     PIC X(4)    VALUE '0916'.       CL*16
00152      12  ER-0917                     PIC X(4)    VALUE '0917'.       CL*16
00153      12  ER-0924                     PIC X(4)    VALUE '0924'.       CL*16
00154      12  ER-7840                     PIC X(4)    VALUE '7840'.       CL*17
062602     12  ER-7846                     PIC X(4)    VALUE '7846'.       CL*17
00155      EJECT                                                        EL153
00156                                      COPY ELCAID.                    CL**4
00157  01  PF-AID REDEFINES DFHAID.                                     EL153
00158      05  FILLER                      PIC X(8).                       CL*16
00159      05  PF-VALUES  OCCURS 24        PIC X.                          CL*16
00160      EJECT                                                        EL153
00161                                      COPY ELCINTF.                   CL**4
00162      EJECT                                                        EL153
00163                                      COPY ELCATTR.                   CL**4
00164      EJECT                                                        EL153
00165                                      COPY ELCLOGOF.                  CL**4
00166      EJECT                                                        EL153
00167                                      COPY ELCDATE.                   CL**4
00168      EJECT                                                        EL153
00169                                      COPY ELCEMIB.                   CL**4
00170      EJECT                                                        EL153
00171                                      COPY EL153S.                    CL*11
00172  01  EL153AI-R REDEFINES EL153AI.                                    CL**4
062602*    12  FILLER                      PIC X(70).                      CL*13
062602     12  FILLER                      PIC X(74).                      CL*13
00174      12  EL153AI-OCCURS OCCURS 14 TIMES.                             CL**4
00175          16  EL153A-NOTE-LENGTH      PIC S9(4)     COMP.             CL*16
00176          16  EL153A-NOTE-ATTRB       PIC X.                          CL*16
00177          16  EL153A-NOTE             PIC X(60).                      CL**4
00178                                                                      CL**4
00179      EJECT                                                        EL153
00180  LINKAGE SECTION.                                                 EL153
00181                                                                   EL153
00182  01  DFHCOMMAREA                     PIC X(1024).                    CL**4
00183      EJECT                                                        EL153
00184                                      COPY ELCMSTR.                   CL**4
00185      EJECT                                                        EL153
00186                                      COPY ELCTRLR.                   CL**4
00187      EJECT                                                        EL153
00188  PROCEDURE DIVISION.                                              EL153
00189                                                                   EL153
00190      MOVE EIBDATE               TO  DC-JULIAN-YYDDD.                 CL**4
00191      MOVE '5'                   TO  DC-OPTION-CODE.                  CL**4
00192      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.                    CL**4
00193      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL153
00194      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL153
00195                                                                      CL*17
00196      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.         CL*19
00197                                                                      CL*19
00198      IF EIBCALEN NOT GREATER THAN ZEROS                              CL*19
00199        GO TO 9000-UNAUTHERR.                                         CL*19
00200                                                                      CL*19
00201      IF PI-COMPANY-ID = 'DMD'                                        CL*17
00202          MOVE +3                TO  DC-ELAPSED-MONTHS                CL*17
00203          MOVE +0                TO  DC-ELAPSED-DAYS                  CL*17
00204          MOVE '6'               TO  DC-OPTION-CODE                   CL*17
00205          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL*17
00206          MOVE DC-BIN-DATE-2     TO  CURRENT-PLUS3-SAVE.              CL*17
00207                                                                   EL153
00208      EXEC CICS  HANDLE CONDITION                                  EL153
00209             ERROR    (9990-ERROR)                                    CL**4
00210             PGMIDERR (9600-PGMIDERR)                                 CL**4
00211      END-EXEC.                                                    EL153
00212                                                                   EL153
00213      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL153
00214          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL153
00215              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL153
00216              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL153
00217              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL153
00218              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL153
00219              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL153
00220              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL153
00221              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL153
00222              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL153
00223          ELSE                                                     EL153
00224              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL153
00225              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL153
00226              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL153
00227              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL153
00228              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL153
00229              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL153
00230              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL153
00231              MOVE SPACES               TO PI-SAVED-PROGRAM-6         CL**4
00232      ELSE                                                            CL**4
00233          GO TO 0100-RECEIVE.                                         CL**4
00234                                                                   EL153
00235      MOVE LOW-VALUES             TO  EL153AO.                        CL**4
00236                                                                      CL*15
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' or 'FNL'
00238         MOVE -1                  TO CALLTPL                          CL*16
00239      ELSE                                                            CL*15
CIDMOD*       IF PI-COMPANY-ID = 'CID'
CIDMOD*          MOVE -1               TO MLINE1L
CIDMOD*       ELSE
00240            MOVE -1               TO MSTRDTL                          CL*15
CIDMOD*       END-IF
CIDMOD     END-IF
00241                                                                      CL*15
00242      MOVE +2                     TO  EMI-NUMBER-OF-LINES.            CL**4
00243      MOVE SPACES                 TO  MERMSG1O                        CL**4
00244                                      MERMSG2O.                       CL**4
00245      MOVE '153A'                 TO  PI-CURRENT-SCREEN-NO.           CL**4
00246      MOVE ER-0694                TO  EMI-ERROR.                      CL**4
00247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00248      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
00249                                                                   EL153
00250      EJECT                                                        EL153
00251  0100-RECEIVE.                                                       CL**4
00252                                                                   EL153
00253      IF EIBAID = DFHCLEAR                                            CL*16
00254          GO TO 9400-CLEAR.                                           CL**4
00255                                                                   EL153
00256      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                          CL*16
00257          MOVE LOW-VALUES         TO  EL153AO                         CL**4
00258          MOVE ER-0008            TO  EMI-ERROR                       CL**4
00259          MOVE -1                 TO  MLINE1L                         CL**4
00260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00261          GO TO 8200-SEND-DATAONLY.                                   CL**4
00262                                                                   EL153
00263      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*16
00264          NEXT SENTENCE                                               CL**4
00265      ELSE                                                            CL**4
00266          EXEC CICS READQ TS                                          CL**4
00267              QUEUE    (PI-SECURITY-TEMP-STORE-ID)                    CL**4
00268              INTO     (SECURITY-CONTROL)                             CL**4
00269              LENGTH   (SC-COMM-LENGTH)                               CL**4
00270              ITEM     (SC-ITEM)                                      CL**4
00271          END-EXEC                                                    CL**4
00272          MOVE SC-CLAIMS-DISPLAY (8)  TO  PI-DISPLAY-CAP              CL**4
00273          MOVE SC-CLAIMS-UPDATE  (8)  TO  PI-MODIFY-CAP.              CL**4
00274                                                                   EL153
00275      EXEC CICS RECEIVE                                               CL**4
00276          MAP      (WS-MAP-NAME)                                      CL**4
00277          MAPSET   (WS-MAPSET-NAME)                                   CL**4
00278          INTO     (EL153AI)                                          CL**4
00279      END-EXEC.                                                       CL**4
00280                                                                      CL**4
00281      IF MPFNUMBL > +0                                                CL*17
00282          IF EIBAID NOT = DFHENTER                                    CL*17
00283              MOVE ER-0004        TO  EMI-ERROR                       CL**4
00284              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00285              MOVE AL-UNBOF       TO  MPFNUMBA                        CL**4
00286              MOVE -1             TO  MPFNUMBL                        CL**4
00287              GO TO 8200-SEND-DATAONLY.                               CL**4
00288                                                                   EL153
00289      IF MPFNUMBI IS NUMERIC                                          CL**4
00290          IF MPFNUMBO > ZERO AND                                      CL*17
00291             MPFNUMBO < 25                                            CL*17
00292              MOVE PF-VALUES (MPFNUMBI)   TO  EIBAID                  CL**4
00293          ELSE                                                        CL**4
00294              MOVE ER-0029                TO  EMI-ERROR               CL**4
00295              MOVE AL-UNBOF               TO  MPFNUMBA                CL**4
00296              MOVE -1                     TO  MPFNUMBL                CL**4
00297              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00298              GO TO 8200-SEND-DATAONLY.                               CL**4
00299                                                                   EL153
00300      IF EIBAID = DFHPF12                                             CL*16
00301          MOVE 'EL010   '         TO  THIS-PGM                        CL**4
00302          GO TO 9300-XCTL.                                            CL**4
00303                                                                   EL153
00304      IF EIBAID = DFHPF23                                             CL*16
00305          MOVE EIBAID             TO  PI-ENTRY-CD-1                   CL**4
00306          MOVE 'EL005   '         TO  THIS-PGM                        CL**4
00307          GO TO 9300-XCTL.                                            CL**4
00308                                                                   EL153
00309      IF EIBAID = DFHPF24                                             CL*16
00310          MOVE 'EL126   '         TO  THIS-PGM                        CL**4
00311          GO TO 9300-XCTL.                                            CL**4
00312                                                                   EL153
00313      IF (EIBAID = DFHENTER)                                          CL*18
00314                OR                                                    CL*18
00315         (EIBAID = DFHPF4 AND                                         CL*18
00316          PI-COMPANY-ID = 'DMD')                                      CL*18
00317          NEXT SENTENCE                                               CL**4
00318      ELSE                                                         EL153
00319          MOVE ER-0008            TO  EMI-ERROR                       CL**4
00320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00321          MOVE -1                 TO  MPFNUMBL                        CL**4
00322          MOVE AL-UNBON           TO  MPFNUMBA                        CL**4
00323          GO TO 8200-SEND-DATAONLY.                                   CL**4
00324                                                                   EL153
00325  0200-PROCESSING-MAINLINE.                                           CL**4
00326                                                                   EL153
00327      IF NOT MODIFY-CAP                                               CL**4
00328          MOVE 'UPDATE'           TO  SM-READ                         CL**4
00329          PERFORM 9995-SECURITY-VIOLATION                             CL**4
00330          MOVE ER-0070            TO  EMI-ERROR                       CL**4
00331          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00332          MOVE -1                 TO  MPFNUMBL                        CL**4
00333          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00334                                                                   EL153
00335      MOVE +0                     TO  SUB.                            CL**4
00336      PERFORM 4000-CHECK-INPUT-LOOP THRU 4000-CHECK-EXIT.             CL**4
00337                                                                   EL153
00338      IF NOT EMI-NO-ERRORS                                            CL**4
00339          GO TO 8200-SEND-DATAONLY.                                   CL**4
00340                                                                   EL153
00341      MOVE PI-COMPANY-CD          TO  WS-KEY-COMPANY-CD.              CL**4
00342      MOVE PI-CARRIER             TO  WS-KEY-CARRIER.                 CL**4
00343      MOVE PI-CLAIM-NO            TO  WS-KEY-CLAIM-NO.                CL**4
00344      MOVE PI-CERT-NO             TO  WS-KEY-CERT-NO.                 CL**4
00345                                                                   EL153
062602     IF (NOTETPL > +0)
062602*       AND (NOTETPI = 'S')
080106        IF (NOTETPI = 'S')
062602          GO TO 1200-BUILD-SPEC-REVIEW
080106        ELSE
080106          GO TO 1000-BUILD-NOTES
080106        END-IF
062602     ELSE
00346        IF MSTRDTI = (LOW-VALUES OR SPACES) AND                       CL*16
00347          MENDDTI = (LOW-VALUES OR SPACES)                            CL*16
00348          GO TO 1000-BUILD-NOTES                                      CL**4
00349        ELSE                                                          CL**4
00350           GO TO 1500-BUILD-REMINDERS
062602       END-IF
062602     END-IF
00351      .                                                            EL153
00352      EJECT                                                           CL**4
00353  1000-BUILD-NOTES.                                                   CL**4
00354                                                                      CL*12
00355      MOVE SPACES TO W-CALL-TYPE W-NOTE-TYPE-IND W-NOTE-TYPE.         CL*13
00356                                                                      CL*12
00357      IF CALLTPI NOT = SPACES AND LOW-VALUES                          CL*14
102418         IF CALLTPI = 'I' OR 'O' OR 'N'                              CL*16
00359              MOVE CALLTPI    TO W-CALL-TYPE                          CL*13
00360              MOVE 'C'        TO W-NOTE-TYPE-IND                      CL*13
00361          ELSE                                                        CL*12
00362              MOVE ER-0915    TO EMI-ERROR                            CL*13
00363              MOVE AL-UABON   TO CALLTPA                              CL*13
00364              MOVE -1         TO CALLTPL                              CL*13
00365              MOVE 'X'        TO WS-DATE-ERROR-SW                     CL*13
00366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*12
00367              GO TO 8200-SEND-DATAONLY.                               CL*12
080106
080106     IF NOTETPI NOT = SPACES AND LOW-VALUES         
080106         IF NOTETPI = 'N'
080106             MOVE NOTETPI    TO W-NOTE-TYPE         
080106             MOVE 'N'        TO W-NOTE-TYPE-IND     
080106         ELSE
080106             MOVE ER-7846    TO EMI-ERROR           
080106             MOVE AL-UABON   TO NOTETPA             
080106             MOVE -1         TO NOTETPL             
080106             MOVE 'X'        TO WS-DATE-ERROR-SW    
080106             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080106             GO TO 8200-SEND-DATAONLY
080106         END-IF               
080106     END-IF.
00368
00369      MOVE +1                     TO  SUB   SUB-1.                    CL**4
00370      PERFORM 4100-SQUASH-SCREEN THRU 4100-EXIT.                      CL**4
00371                                                                   EL153
00372      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*16
00373          MOVE +0                 TO  SUB                             CL**8
00374          PERFORM 6000-EXPAND-AIG-NOTES THRU 6000-EXIT.               CL**8
00375                                                                      CL**8
00376      PERFORM 7000-READ-UP-CLAIM THRU 7050-EXIT.                      CL**4
00377      PERFORM 7100-GETMAIN-TRLR  THRU 7100-EXIT.                      CL*17
00378                                                                      CL*17
00379      MOVE +15                    TO  SUB.                            CL**4
00380      PERFORM 3200-BUILD-GI-TRLR-LOOP THRU 3200-EXIT.                 CL**4
00381      MOVE +0                     TO  SUB   SUB-1.                    CL**4
00382                                                                      CL**6
00383      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.             CL**6
00384      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.               CL**6
00385      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.           CL**6
00386      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.             CL**6
00387                                                                      CL**4
00388      PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.                      CL**4
00389                                                                      CL**4
00390      MOVE ER-0000                TO  EMI-ERROR.                      CL**4
00391      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00392      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).         CL**4
00393                                                                      CL*17
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
00395          MOVE -1                 TO  CALLTPL.                        CL*16
00396                                                                      CL*17
00397      GO TO 8200-SEND-DATAONLY.                                       CL*14
00398                                                                      CL**4
00399      EJECT                                                        EL153
062602 1200-BUILD-SPEC-REVIEW.                                             CL**4
062602                                                                     CL**4
062602     IF NOTETPI NOT = SPACES AND LOW-VALUES                          CL*14
062602         IF NOTETPI = 'S'
00359              MOVE NOTETPI    TO W-NOTE-TYPE                          CL*13
00360              MOVE 'S'        TO W-NOTE-TYPE-IND                      CL*13
00361          ELSE                                                        CL*12
00362              MOVE ER-7846    TO EMI-ERROR                            CL*13
00363              MOVE AL-UABON   TO NOTETPA                              CL*13
00364              MOVE -1         TO NOTETPL                              CL*13
00365              MOVE 'X'        TO WS-DATE-ERROR-SW                     CL*13
00366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*12
062602             GO TO 8200-SEND-DATAONLY.                               CL*12
062602                                                                  EL153
00369      MOVE +1                     TO  SUB   SUB-1.                    CL**4
00370      PERFORM 4100-SQUASH-SCREEN THRU 4100-EXIT.                      CL**4
00371                                                                   EL153
00376      PERFORM 7000-READ-UP-CLAIM THRU 7050-EXIT.                      CL**4
00409      PERFORM 7100-GETMAIN-TRLR     THRU 7100-EXIT.                   CL*16
00411      PERFORM 3150-BUILD-TRAILER-SR THRU 3150-EXIT.                   CL**4
00412                                                                      CL**4
00413      PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.                      CL**4
00423                                                                   EL153
00383      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.             CL**6
00384      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.               CL**6
00385      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.           CL**6
00386      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.             CL**6
00387                                                                      CL**4
00388      PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.                      CL**4
00389                                                                      CL**4
00424      MOVE ER-0000                TO  EMI-ERROR.                      CL**4
00425      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00426      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).         CL**4
00427                                                                      CL*17
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
00429          MOVE -1                 TO  CALLTPL.                        CL*16
00430                                                                      CL*17
062602     GO TO 8200-SEND-DATAONLY.                                       CL**4
062602                                                                  EL153
062602     EJECT                                                        EL153
00400  1500-BUILD-REMINDERS.                                               CL**4
00401                                                                      CL**4
00402      PERFORM 2000-EDIT-SCREEN THRU 2000-EXIT.                        CL**4
00403                                                                      CL**4
00404      IF NOT EMI-NO-ERRORS                                            CL**4
00405          GO TO 8200-SEND-DATAONLY.                                   CL**4
00406                                                                      CL**4
00407      PERFORM 7000-READ-UP-CLAIM    THRU 7050-EXIT.                   CL*16
00408      PERFORM 2200-CHECK-DATES      THRU 2200-EXIT.                   CL*16
00409      PERFORM 7100-GETMAIN-TRLR     THRU 7100-EXIT.                   CL*16
00410      PERFORM 3000-REDUCE-SEQ       THRU 3000-EXIT.                   CL*16
00411      PERFORM 3100-BUILD-TRAILER-AP THRU 3100-EXIT.                   CL**4
00412                                                                      CL**4
00413      PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.                      CL**4
00414                                                                      CL**4
00415      IF CL-NEXT-FOLLOWUP-DT = LOW-VALUES                             CL*16
00416          MOVE WS-STR-DATE-BIN        TO  CL-NEXT-FOLLOWUP-DT         CL**4
00417      ELSE                                                            CL**4
00418          IF WS-STR-DATE-BIN < CL-NEXT-FOLLOWUP-DT AND                CL*17
00419             WS-STR-DATE-BIN NOT = LOW-VALUES                         CL*17
00420              MOVE WS-STR-DATE-BIN    TO  CL-NEXT-FOLLOWUP-DT.        CL**4
00421                                                                      CL**4
00422      PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.                      CL**4
00423                                                                   EL153
00424      MOVE ER-0000                TO  EMI-ERROR.                      CL**4
00425      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00426      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).         CL**4
00427                                                                      CL*17
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
00429          MOVE -1                 TO  CALLTPL.                        CL*16
00430                                                                      CL*17
00431      GO TO 8200-SEND-DATAONLY.                                       CL**4
00432                                                                   EL153
00433      EJECT                                                        EL153
00434  2000-EDIT-SCREEN.                                                EL153
00435      MOVE SPACES                 TO  WS-DATE-ERROR-SW.               CL**4
00436                                                                   EL153
00437      IF MSTRDTI = SPACES OR LOW-VALUES                            EL153
00438          MOVE LOW-VALUES         TO  WS-STR-DATE-BIN                 CL**4
00439      ELSE                                                            CL**4
00440          MOVE MSTRDTI            TO  WS-DATE-UNEDIT                  CL**4
00441          EXEC CICS  BIF  DEEDIT                                      CL**4
00442              FIELD (WS-DATE-UNEDIT)                                  CL**4
00443              LENGTH (8)                                              CL**4
00444          END-EXEC                                                    CL**4
00445          MOVE '4'                TO  DC-OPTION-CODE                  CL**4
00446          MOVE WS-DATE-MDY        TO  DC-GREG-DATE-1-MDY              CL**4
00447          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**4
00448          IF DATE-CONVERSION-ERROR                                    CL*17
00449              MOVE ER-0314        TO  EMI-ERROR                       CL**4
00450              MOVE AL-UABON       TO  MSTRDTA                         CL**4
00451              MOVE -1             TO  MSTRDTL                         CL**4
00452              MOVE 'X'            TO  WS-DATE-ERROR-SW                CL**4
00453              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00454          ELSE                                                        CL**4
00455              IF (PI-COMPANY-ID = 'DMD') AND                          CL*18
00456                 (EIBAID NOT = DFHPF4)   AND                          CL*18
00457                 (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)                 CL*18
00458                  MOVE 'PF4=FORCE 7840' TO PFKEY4O                    CL*18
00459                  MOVE AL-SABON         TO PFKEY4A                    CL*18
00460                  MOVE ER-7840          TO EMI-ERROR                  CL*19
00461                  MOVE AL-UABON         TO MSTRDTA                    CL*19
00462                  MOVE -1               TO MSTRDTL                    CL*19
00463                  MOVE 'X'              TO WS-DATE-ERROR-SW           CL*19
00464                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
00465              ELSE                                                    CL*17
00466                MOVE SPACES               TO PFKEY4O                  CL*18
00467                MOVE AL-SADON             TO PFKEY4A                  CL*18
00468                MOVE AL-UANON             TO MSTRDTA                  CL*17
00469                MOVE DC-BIN-DATE-1        TO WS-STR-DATE-BIN          CL*17
00470                MOVE DC-GREG-DATE-1-EDIT  TO MSTRDTI.                 CL*17
00471                                                                   EL153
00472      IF MENDDTI = SPACES OR = LOW-VALUES                          EL153
00473          MOVE LOW-VALUES         TO  WS-END-DATE-BIN                 CL**4
00474      ELSE                                                            CL**4
00475          MOVE MENDDTI            TO  WS-DATE-UNEDIT                  CL**4
00476          EXEC CICS  BIF  DEEDIT                                      CL**4
00477              FIELD (WS-DATE-UNEDIT)                                  CL**4
00478              LENGTH (8)                                              CL**4
00479          END-EXEC                                                    CL**4
00480          MOVE '4'                TO  DC-OPTION-CODE                  CL**4
00481          MOVE WS-DATE-MDY        TO  DC-GREG-DATE-1-MDY              CL**4
00482          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**4
00483          IF  DATE-CONVERSION-ERROR                                EL153
00484              MOVE ER-0314        TO  EMI-ERROR                       CL**4
00485              MOVE AL-UABON       TO  MENDDTA                         CL**4
00486              MOVE -1             TO  MENDDTL                         CL**4
00487              MOVE 'X'            TO  WS-DATE-ERROR-SW                CL**4
00488              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00489          ELSE                                                        CL**4
00490              MOVE AL-UANON             TO  MENDDTA                   CL*16
00491              MOVE DC-BIN-DATE-1        TO  WS-END-DATE-BIN           CL*16
00492              MOVE DC-GREG-DATE-1-EDIT  TO  MENDDTI.                  CL**4
00493  2000-EXIT.                                                       EL153
00494       EXIT.                                                       EL153
00495                                                                      CL**4
00496      EJECT                                                        EL153
00497  2200-CHECK-DATES.                                                EL153
00498      IF WS-STR-DATE-BIN  LESS THAN SAVE-BIN-DATE AND                 CL**4
00499         WS-END-DATE-BIN  LESS THAN SAVE-BIN-DATE                     CL**4
00500          MOVE ER-0316            TO  EMI-ERROR                       CL**4
00501          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00502          MOVE -1                 TO  MSTRDTL                         CL**4
00503          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00504                                                                   EL153
00505      IF WS-STR-DATE-BIN  GREATER THAN WS-END-DATE-BIN                CL**4
00506          MOVE ER-0317            TO  EMI-ERROR                       CL**4
00507          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00508          MOVE -1                 TO  MSTRDTL                         CL**4
00509          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00510                                                                   EL153
00511  2200-EXIT.                                                       EL153
00512       EXIT.                                                       EL153
00513                                                                   EL153
00514      EJECT                                                        EL153
00515  3000-REDUCE-SEQ.                                                    CL**4
00516      IF CL-LAST-TRL-AVAIL                                            CL**4
00517          MOVE ER-0137            TO  EMI-ERROR                       CL**4
00518          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00519          MOVE -1                 TO  EL153A-NOTE-LENGTH (1)          CL**4
00520          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
00521                                                                      CL**4
00522      SUBTRACT  1 FROM CL-TRAILER-SEQ-CNT.                            CL**4
00523      MOVE CL-TRAILER-SEQ-CNT     TO  WS-KEY-SEQUENCE-NO              CL**7
00524                                      AT-SEQUENCE-NO.                 CL**7
00525                                                                   EL153
00526  3000-EXIT.                                                       EL153
00527       EXIT.                                                       EL153
00528                                                                   EL153
00529  3100-BUILD-TRAILER-AP.                                              CL**4
00530      MOVE 'AT'                   TO  AT-RECORD-ID.                   CL**4
00531      MOVE '7'                    TO  AT-TRAILER-TYPE.                CL**4
00532                                                                   EL153
00533      IF EL153A-NOTE-LENGTH (1) NOT > +0                              CL*17
00534          MOVE SPACES             TO  AT-PROMPT-LINE-1                CL**4
00535      ELSE                                                            CL**4
00536          MOVE EL153A-NOTE  (1)   TO  AT-PROMPT-LINE-1.               CL**4
00537                                                                      CL**4
00538      IF EL153A-NOTE-LENGTH (2) NOT > +0                              CL*17
00539          MOVE SPACES             TO  AT-PROMPT-LINE-2                CL**4
00540      ELSE                                                            CL**4
00541          MOVE EL153A-NOTE  (2)   TO  AT-PROMPT-LINE-2.               CL**4
00542                                                                      CL**4
00543      MOVE WS-STR-DATE-BIN        TO  AT-PROMPT-START-DT.             CL**4
00544      MOVE WS-END-DATE-BIN        TO  AT-PROMPT-END-DT.               CL**4
00545      MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.             CL**4
00546      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY                  CL**4
00547                                      AT-PROMPT-LAST-UPDATED-BY.      CL**4
00548      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.           CL**4
00549      MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT                  CL**4
00550                                      AT-PROMPT-LAST-MAINT-DT.        CL**4
00551                                                                      CL**4
00552  3100-EXIT.                                                          CL**4
00553       EXIT.                                                          CL**4
00554                                                                      CL**4
00555      EJECT                                                           CL**4
062602 3150-BUILD-TRAILER-SR.                                              CL**4
062602     MOVE 'AT'                   TO  AT-RECORD-ID.                   CL**4
00531      MOVE '6'                    TO  AT-TRAILER-TYPE.                CL**4
00532                                                                   EL153
00533      IF EL153A-NOTE-LENGTH (1) NOT > +0                              CL*17
00534          MOVE SPACES             TO  AT-info-line-1                  CL**4
00535      ELSE                                                            CL**4
00536          MOVE EL153A-NOTE  (1)   TO  AT-info-line-1.                 CL**4
00537                                                                      CL**4
00538      IF EL153A-NOTE-LENGTH (2) NOT > +0                              CL*17
00539          MOVE SPACES             TO  AT-info-line-2                  CL**4
00540      ELSE                                                            CL**4
00541          MOVE EL153A-NOTE  (2)   TO  AT-info-line-2.                 CL**4
00542                                                                      CL**4
062602     MOVE W-NOTE-TYPE-IND        TO  AT-INFO-TRAILER-TYPE.           CL*12
062602     move +92                    to  ws-key-sequence-no
00545      MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
00546      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY                  CL**4
00572                                      AT-GEN-INFO-LAST-UPDATED-BY.    CL**4
00548      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.           CL**4
00549      MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT                  CL**4
00577                                      AT-GEN-INFO-LAST-MAINT-DT       CL**4
00551      .                                                               CL**4
00552  3150-EXIT.                                                          CL**4
00553       EXIT.                                                          CL**4
00554                                                                      CL**4
00555      EJECT                                                           CL**4
00556  3200-BUILD-GI-TRLR-LOOP.                                            CL**4
00557                                                                      CL**4
00558      SUBTRACT +1 FROM SUB.                                           CL**4
00559                                                                      CL**4
00560      IF SUB < +1                                                     CL*17
00561          GO TO 3200-EXIT.                                            CL**4
00562                                                                      CL**4
00563      IF EL153A-NOTE-LENGTH (SUB) = +0                                CL*16
00564          GO TO 3200-BUILD-GI-TRLR-LOOP.                              CL**4
00565                                                                      CL**4
00566      MOVE SPACES                 TO ACTIVITY-TRAILERS.               CL*12
00567      MOVE 'AT'                   TO  AT-RECORD-ID.                   CL**4
00568      MOVE '6'                    TO  AT-TRAILER-TYPE.                CL**4
00569                                                                      CL**4
00570      MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.             CL**4
00571      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY                  CL**4
00572                                      AT-GEN-INFO-LAST-UPDATED-BY.    CL**4
00573      MOVE W-NOTE-TYPE-IND        TO  AT-INFO-TRAILER-TYPE.           CL*12
00574      MOVE W-CALL-TYPE            TO  AT-CALL-TYPE.                   CL*12
00575      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.           CL**4
00576      MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT                  CL**4
00577                                      AT-GEN-INFO-LAST-MAINT-DT.      CL**4
00578                                                                      CL**4
00579      IF WS-FIRST-TIME-SW = 'Y'                                       CL*16
00580          MOVE 'N'                TO  WS-FIRST-TIME-SW                CL**4
00581          IF SUB NOT = 1 AND 2                                        CL*16
00582              MOVE 'X'            TO AT-NOTE-CONTINUATION             CL*12
00583          END-IF                                                      CL*12
00584          IF SUB = 1 OR 3 OR 5 OR 7 OR 9 OR 11 OR 13                  CL*16
00585              MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-1              CL**4
00586              PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT                  CL**4
00587              MOVE CL-TRAILER-SEQ-CNT TO  AT-SEQUENCE-NO              CL**4
00588              PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT               CL**4
00589              GO TO 3200-BUILD-GI-TRLR-LOOP                           CL**4
00590          ELSE                                                        CL**4
00591              MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-2              CL**4
00592              SUBTRACT +1             FROM SUB                        CL**4
00593              MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-1              CL**4
00594              PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT                  CL**4
00595              MOVE CL-TRAILER-SEQ-CNT TO  AT-SEQUENCE-NO              CL**4
00596              PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT               CL**4
00597              GO TO 3200-BUILD-GI-TRLR-LOOP.                          CL**4
00598                                                                      CL**4
00599      MOVE EL153A-NOTE (SUB)      TO  AT-INFO-LINE-2.                 CL**4
00600      SUBTRACT +1 FROM SUB.                                           CL**4
00601      MOVE EL153A-NOTE (SUB)      TO  AT-INFO-LINE-1.                 CL**4
00602                                                                      CL*12
00603      IF SUB NOT = 1                                                  CL*17
00604          MOVE 'X'                TO AT-NOTE-CONTINUATION.            CL*12
00605                                                                      CL*12
00606      PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT.                         CL**4
00607      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.                 CL**4
00608      PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.                      CL**4
00609                                                                      CL*16
00610      GO TO 3200-BUILD-GI-TRLR-LOOP.                                  CL**4
00611                                                                   EL153
00612  3200-EXIT.                                                       EL153
00613       EXIT.                                                       EL153
00614                                                                   EL153
00615      EJECT                                                           CL**4
00616  4000-CHECK-INPUT-LOOP.                                              CL**4
00617                                                                      CL**4
00618       ADD +1 TO SUB.                                                 CL*17
00619                                                                      CL*17
00620       IF SUB > +14                                                   CL*17
00621           MOVE ER-0483           TO  EMI-ERROR                       CL**4
00622           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                   CL**4
00623           MOVE -1                TO  EL153A-NOTE-LENGTH (1)          CL**4
00624           GO TO 4000-CHECK-EXIT.                                     CL**4
00625                                                                      CL**4
00626       IF EL153A-NOTE-LENGTH (SUB) NOT > +0                           CL*17
00627           GO TO 4000-CHECK-INPUT-LOOP.                               CL**4
00628                                                                   EL153
00629  4000-CHECK-EXIT.                                                    CL**4
00630      EXIT.                                                           CL**4
00631                                                                   EL153
00632  4100-SQUASH-SCREEN.                                                 CL**4
00633                                                                   EL153
00634      IF SUB > +14                                                    CL*17
00635          GO TO 4100-EXIT.                                            CL**4
00636                                                                   EL153
00637      IF EL153A-NOTE-LENGTH (SUB) > +0                                CL*17
00638          IF SUB = SUB-1                                              CL*16
00639              ADD +1              TO  SUB   SUB-1                     CL**4
00640              GO TO 4100-SQUASH-SCREEN.                               CL**4
00641                                                                   EL153
00642      IF EL153A-NOTE-LENGTH (SUB) > +0                                CL*17
00643          MOVE EL153A-NOTE-LENGTH (SUB)   TO                          CL**4
00644                                       EL153A-NOTE-LENGTH (SUB-1)     CL**4
00645          MOVE EL153A-NOTE-ATTRB  (SUB)   TO                          CL**4
00646                                       EL153A-NOTE-ATTRB  (SUB-1)     CL**4
00647          MOVE EL153A-NOTE        (SUB)   TO  EL153A-NOTE (SUB-1)     CL**4
00648          MOVE +0                         TO                          CL**4
00649                                       EL153A-NOTE-LENGTH (SUB)       CL**4
00650          MOVE LOW-VALUES                 TO  EL153A-NOTE (SUB)       CL**4
00651          MOVE AL-UANOF                   TO                          CL**4
00652                                       EL153A-NOTE-ATTRB  (SUB)       CL**4
00653          ADD +1                          TO  SUB   SUB-1             CL**4
00654      ELSE                                                         EL153
00655          ADD +1                          TO  SUB.                    CL**4
00656                                                                   EL153
00657      GO TO 4100-SQUASH-SCREEN.                                       CL**4
00658                                                                   EL153
00659  4100-EXIT.                                                          CL**4
00660      EXIT.                                                           CL**4
00661                                                                   EL153
00662      EJECT                                                           CL**4
00663  6000-EXPAND-AIG-NOTES.                                              CL**8
00664                                                                      CL**8
00665      ADD +1 TO SUB.                                                  CL*17
00666                                                                      CL*17
00667      IF SUB > +14                                                    CL*17
00668          GO TO 6000-EXIT.                                            CL**8
00669                                                                      CL**8
00670      IF EL153A-NOTE-LENGTH (SUB) > +0                                CL*17
00671          MOVE EL153A-NOTE (SUB)          TO  WS-EDIT-NOTE            CL**8
00672          INSPECT WS-EDIT-NOTE REPLACING ALL LOW-VALUES BY SPACES     CL**8
00673          IF WS-EDIT-NOTE-1-4 = 'APC '                                CL*16
00674              MOVE 'AUTO-PAY CLAIM'       TO  EL153A-NOTE (SUB)       CL**8
00675          ELSE                                                        CL**8
00676          IF WS-EDIT-NOTE-1-4 = 'MGR '                                CL*16
00677              MOVE 'MANAGEMENT REVIEW'    TO  EL153A-NOTE (SUB)       CL**8
00678          ELSE                                                        CL**8
00679          IF WS-EDIT-NOTE-1-4 = 'SVR '                                CL*16
00680              MOVE 'SUPERVISOR REVIEW'    TO  EL153A-NOTE (SUB)       CL**8
00681          ELSE                                                        CL**8
00682          IF WS-EDIT-NOTE-1-4 = 'OIO '                                CL*16
00683              MOVE 'OUTSIDE INVESTIGATION ORDERED'                    CL**8
00684                                          TO  EL153A-NOTE (SUB)       CL**8
00685          ELSE                                                        CL**8
00686          IF WS-EDIT-NOTE-1-4 = 'LGR '                                CL*16
00687              MOVE 'LEGAL REVIEW'         TO  EL153A-NOTE (SUB)       CL**8
00688          ELSE                                                        CL**8
00689          IF WS-EDIT-NOTE-1-4 = 'FRN '                                CL*16
00690              MOVE 'FORM REVIEWED/NO ADDITIONAL PAYMENT DUE AT THIS   CL**8
00691 -                 ' TIME'                TO  EL153A-NOTE (SUB)       CL**8
00692          ELSE                                                        CL**8
00693          IF WS-EDIT-NOTE-1-4 = 'PRB '                                CL*16
00694              MOVE 'PHONE CALL RECEIVED--BRANCH'                      CL**8
00695                                          TO  EL153A-NOTE (SUB)       CL**8
00696          ELSE                                                        CL**8
00697          IF WS-EDIT-NOTE-1-4 = 'PRG '                                CL*16
00698              MOVE 'PHONE CALL RECEIVED--GROUP'                       CL**8
00699                                          TO  EL153A-NOTE (SUB)       CL**8
00700          ELSE                                                        CL**8
00701          IF WS-EDIT-NOTE-1-4 = 'PRH '                                CL*16
00702              MOVE 'PHONE CALL RECEIVED--HOSPITAL'                    CL**8
00703                                          TO  EL153A-NOTE (SUB)       CL**8
00704          ELSE                                                        CL**8
00705          IF WS-EDIT-NOTE-1-4 = 'PRI '                                CL*16
00706              MOVE 'PHONE CALL RECEIVED--INSURED'                     CL**8
00707                                          TO  EL153A-NOTE (SUB)       CL**8
00708          ELSE                                                        CL**8
00709          IF WS-EDIT-NOTE-1-4 = 'PRO '                                CL*16
00710              MOVE 'PHONE CALL RECEIVED--OTHER'                       CL**8
00711                                          TO  EL153A-NOTE (SUB)       CL**8
00712          ELSE                                                        CL**8
00713          IF WS-EDIT-NOTE-1-4 = 'PRP '                                CL*16
00714              MOVE 'PHONE CALL RECEIVED--PHYSICIAN'                   CL**8
00715                                          TO  EL153A-NOTE (SUB)       CL**8
00716          ELSE                                                        CL**8
00717          IF WS-EDIT-NOTE-1-4 = 'PMB '                                CL*16
00718              MOVE 'PHONE CALL MADE--BRANCH'                          CL**8
00719                                          TO  EL153A-NOTE (SUB)       CL**8
00720          ELSE                                                        CL**8
00721          IF WS-EDIT-NOTE-1-4 = 'PMG '                                CL*16
00722              MOVE 'PHONE CALL MADE--GROUP'                           CL**8
00723                                          TO  EL153A-NOTE (SUB)       CL**8
00724          ELSE                                                        CL**8
00725          IF WS-EDIT-NOTE-1-4 = 'PMH '                                CL*16
00726              MOVE 'PHONE CALL MADE--HOSPITAL'                        CL**8
00727                                          TO  EL153A-NOTE (SUB)       CL**8
00728          ELSE                                                        CL**8
00729          IF WS-EDIT-NOTE-1-4 = 'PMI '                                CL*16
00730              MOVE 'PHONE CALL MADE--INSURED'                         CL**8
00731                                          TO  EL153A-NOTE (SUB)       CL**8
00732          ELSE                                                        CL**8
00733          IF WS-EDIT-NOTE-1-4 = 'PMO '                                CL*16
00734              MOVE 'PHONE CALL MADE--OTHER'                           CL**8
00735                                          TO  EL153A-NOTE (SUB)       CL**8
00736          ELSE                                                        CL**8
00737          IF WS-EDIT-NOTE-1-4 = 'PMP '                                CL*16
00738              MOVE 'PHONE CALL MADE--PHYSICIAN'                       CL**8
00739                                          TO  EL153A-NOTE (SUB)       CL**8
00740          ELSE                                                        CL**8
00741          IF WS-EDIT-NOTE-1-3 = '01 '                                 CL*16
00742              MOVE 'CONTINUING CLAIM FORM RCVD'                       CL**8
00743                                          TO  EL153A-NOTE (SUB)       CL**8
00744          ELSE                                                        CL**8
00745          IF WS-EDIT-NOTE-1-3 = '02 '                                 CL*16
00746              MOVE 'ACCOUNT INFORMATION RCVD'                         CL**8
00747                                          TO  EL153A-NOTE (SUB)       CL**8
00748          ELSE                                                        CL**8
00749          IF WS-EDIT-NOTE-1-3 = '03 '                                 CL*16
00750              MOVE 'MEDICAL HISTORY RCVD'                             CL**8
00751                                          TO  EL153A-NOTE (SUB)       CL**8
00752          ELSE                                                        CL**8
00753          IF WS-EDIT-NOTE-1-3 = '04 '                                 CL*16
00754              MOVE 'INSURED INFO RCVD'                                CL**8
00755                                          TO  EL153A-NOTE (SUB)       CL**8
00756          ELSE                                                        CL**8
00757          IF WS-EDIT-NOTE-1-3 = '05 '                                 CL*16
00758              MOVE 'EMPLOYER INFO RCVD'                               CL**8
00759                                          TO  EL153A-NOTE (SUB)       CL**8
00760          ELSE                                                        CL**8
00761          IF WS-EDIT-NOTE-1-3 = '06 '                                 CL*16
00762              MOVE 'PARTIAL INVESTIGATION RCVD.  CLAIM IS STILL PEN   CL*11
00763 -                 'DING'                 TO  EL153A-NOTE (SUB)       CL*11
00764          ELSE                                                        CL**8
00765          IF WS-EDIT-NOTE-1-3 = '07 '                                 CL*16
00766              MOVE 'DEATH CERTIFICATE RCVD'                           CL**8
00767                                          TO  EL153A-NOTE (SUB)       CL**8
00768          ELSE                                                        CL**8
00769          IF WS-EDIT-NOTE-1-3 = '08 '                                 CL*16
00770              MOVE 'ATTORNEY LTR RCVD'                                CL**8
00771                                          TO  EL153A-NOTE (SUB)       CL**8
00772          ELSE                                                        CL**8
00773          IF WS-EDIT-NOTE-1-3 = '09 '                                 CL*16
00774              MOVE 'EXAM RESULTS RCVD'                                CL**8
00775                                          TO  EL153A-NOTE (SUB)       CL**8
00776          ELSE                                                        CL**8
00777          IF WS-EDIT-NOTE-1-3 = '10 '                                 CL*16
00778              MOVE 'INS DEPT INQUIRY RCVD'                            CL**8
00779                                          TO  EL153A-NOTE (SUB)       CL**8
00780          ELSE                                                        CL**8
00781          IF WS-EDIT-NOTE-1-3 = '11 '                                 CL*16
00782              MOVE 'RETURNED MAIL RCVD'                               CL**8
00783                                          TO  EL153A-NOTE (SUB)       CL**9
00784          ELSE                                                        CL**9
00785          IF WS-EDIT-NOTE-1-3 = '12 '                                 CL*16
00786              MOVE 'INITIAL CLAIM RCVD'                               CL**9
00787                                          TO  EL153A-NOTE (SUB).      CL**8
00788                                                                      CL**8
00789      GO TO 6000-EXPAND-AIG-NOTES.                                    CL**8
00790                                                                      CL**8
00791  6000-EXIT.                                                          CL**8
00792      EXIT.                                                           CL**8
00793                                                                      CL**8
00794      EJECT                                                           CL**8
00795  7000-READ-UP-CLAIM.                                                 CL**4
00796                                                                      CL**4
00797      EXEC CICS HANDLE CONDITION                                      CL**4
00798          NOTFND    (7040-NOTFND)                                     CL**4
00799          NOTOPEN   (9981-NOTOPEN-MSTR)                               CL**4
00800      END-EXEC.                                                       CL**4
00801                                                                      CL**4
00802      EXEC CICS READ                                                  CL**4
00803          DATASET   ('ELMSTR')                                        CL**4
00804          RIDFLD    (WS-CLAIM-KEY)                                    CL**4
00805          SET       (ADDRESS OF CLAIM-MASTER)                         CL*11
00806          UPDATE                                                      CL**4
00807      END-EXEC.                                                       CL**4
00808                                                                      CL*16
00809      GO TO 7050-EXIT.                                                CL**4
00810                                                                      CL**4
00811  7040-NOTFND.                                                        CL**4
00812                                                                      CL**4
00813      MOVE ER-0133                TO  EMI-ERROR.                      CL**4
00814      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00815      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).         CL**4
00816      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
00817                                                                      CL**4
00818  7050-EXIT.                                                          CL**4
00819      EXIT.                                                           CL**4
00820                                                                      CL**4
00821  7055-REWRITE-CLAIM.                                                 CL**4
00822                                                                      CL**4
00823      EXEC CICS HANDLE CONDITION                                      CL**4
00824          DUPKEY   (7055-EXIT)                                        CL**4
00825      END-EXEC.                                                       CL**4
00826                                                                      CL**4
00827      EXEC CICS REWRITE                                               CL**4
00828          DATASET   ('ELMSTR')                                        CL**4
00829          FROM      (CLAIM-MASTER)                                    CL**4
00830      END-EXEC.                                                       CL**4
00831                                                                      CL**4
00832  7055-EXIT.                                                          CL**4
00833      EXIT.                                                           CL**4
00834                                                                      CL**4
00835  7100-GETMAIN-TRLR.                                                  CL**4
00836      EXEC CICS GETMAIN                                               CL**4
00837          SET      (ADDRESS OF ACTIVITY-TRAILERS)                     CL*11
00838          INITIMG  (GETMAIN-SPACE)                                    CL**4
00839          LENGTH   (WS-TRLR-LENGTH)                                   CL**4
00840      END-EXEC.                                                       CL**4
00841                                                                      CL**4
00842  7100-EXIT.                                                          CL**4
00843       EXIT.                                                       EL153
00844                                                                      CL**4
00845  7150-WRITE-TRAILER.                                                 CL**4
00846                                                                      CL**4
00847      EXEC CICS HANDLE CONDITION                                      CL**4
00848          DUPREC    (7190-DUPREC)                                     CL**4
00849          NOTOPEN   (9982-NOTOPEN-TRLR)                               CL**4
00850      END-EXEC.                                                       CL**4
00851                                                                      CL**4
00852      EXEC CICS WRITE                                                 CL**4
00853          DATASET   ('ELTRLR')                                        CL**4
00854          RIDFLD    (WS-TRAILER-KEY)                                  CL**4
00855          FROM      (ACTIVITY-TRAILERS)                               CL**4
00856      END-EXEC.                                                       CL**4
00857                                                                      CL**4
00858      GO TO 7199-EXIT.                                                CL**4
00859                                                                      CL**4
00860  7190-DUPREC.
062602
062602     if w-note-type = 'S'
062602        MOVE ER-7846             TO EMI-ERROR
062602        MOVE -1                  TO notetpl
062602        PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
062602        GO TO 8200-SEND-DATAONLY
062602     else
00861         PERFORM 3000-REDUCE-SEQ  THRU 3000-EXIT
00862         GO TO 7150-WRITE-TRAILER
           end-if

00863      .                                                                CL*16
00864  7199-EXIT.                                                          CL**4
00865      EXIT.                                                           CL**4
00866                                                                   EL153
00867       EJECT                                                       EL153
00868  8100-SEND-INITIAL-MAP.                                              CL**4
00869      MOVE EIBTIME                TO  TIME-IN.                        CL**4
00870      MOVE WS-HOUR                TO  WS-TRANS-HOUR.                  CL**4
00871      MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.                CL**4
00872      MOVE TIME-OUT               TO  MRNTIMEO.                       CL**4
00873      MOVE SAVE-DATE              TO  MRNDATEO.
031102     MOVE PI-CERT-NO             TO  MCERTO.                         CL**4
00874                                                                      CL**4
00875      EXEC CICS SEND                                                  CL**4
00876          MAP      (WS-MAP-NAME)                                      CL**4
00877          MAPSET   (WS-MAPSET-NAME)                                   CL**4
00878          FROM     (EL153AI)                                          CL**4
00879          FREEKB                                                      CL**4
00880          ERASE                                                       CL**4
00881          CURSOR                                                      CL**4
00882      END-EXEC.                                                       CL**4
00883                                                                      CL**4
00884      GO TO 9100-RETURN-TRANS.                                        CL**4
00885  8100-EXIT.                                                          CL**4
00886      EXIT.                                                           CL**4
00887                                                                      CL**4
00888  8200-SEND-DATAONLY.                                                 CL**4
00889      MOVE EIBTIME                TO  TIME-IN.                        CL**4
00890      MOVE WS-HOUR                TO  WS-TRANS-HOUR.                  CL**4
00891      MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.                CL**4
00892      MOVE TIME-OUT               TO  MRNTIMEO.                       CL**4
00893      MOVE SAVE-DATE              TO  MRNDATEO.
031102     MOVE PI-CERT-NO             TO  MCERTO.                         CL**4
00894                                                                      CL**4
00895      EXEC CICS SEND                                                  CL**4
00896          MAP      (WS-MAP-NAME)                                      CL**4
00897          MAPSET   (WS-MAPSET-NAME)                                   CL**4
00898          FROM     (EL153AI)                                          CL**4
00899          FREEKB                                                      CL**4
00900          DATAONLY                                                    CL**4
00901          CURSOR                                                      CL**4
00902      END-EXEC.                                                       CL**4
00903                                                                      CL**4
00904      GO TO 9100-RETURN-TRANS.                                        CL**4
00905                                                                      CL*16
00906  8200-EXIT.                                                          CL**4
00907      EXIT.                                                           CL**4
00908                                                                      CL**4
00909      EJECT                                                           CL**4
00910  8300-SEND-TEXT.                                                     CL**4
00911      EXEC CICS SEND TEXT                                             CL**4
00912          FROM     (LOGOFF-TEXT)                                      CL**4
00913          ERASE                                                       CL**4
00914          FREEKB                                                      CL**4
00915          LENGTH   (LOGOFF-LENGTH)                                    CL**4
00916      END-EXEC.                                                       CL**4
00917                                                                      CL**4
00918      EXEC CICS RETURN                                                CL**4
00919      END-EXEC.                                                       CL*16
00920                                                                      CL**4
00921  8300-EXIT.                                                          CL**4
00922       EXIT.                                                          CL**4
00923                                                                      CL**4
00924  8500-DATE-CONVERSION.                                               CL**4
00925      EXEC CICS LINK                                               EL153
00926             PROGRAM  ('ELDATCV')                                  EL153
00927             COMMAREA (DATE-CONVERSION-DATA)                       EL153
00928             LENGTH   (DC-COMM-LENGTH)                             EL153
00929      END-EXEC.                                                    EL153
00930                                                                   EL153
00931  8500-EXIT.                                                          CL**4
00932       EXIT.                                                       EL153
00933                                                                   EL153
00934      EJECT                                                           CL**4
00935  9000-UNAUTHERR.                                                     CL**4
00936      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                      CL**4
00937      GO TO 8300-SEND-TEXT.                                           CL**4
00938                                                                   EL153
00939  9100-RETURN-TRANS.                                                  CL**4
00940      EXEC CICS RETURN                                                CL**4
00941          TRANSID  (WS-TRANS-ID)                                      CL**4
00942          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL**4
00943          LENGTH   (PI-COMM-LENGTH)                                   CL**4
00944      END-EXEC.                                                       CL**4
00945                                                                      CL**4
00946  9100-EXIT.                                                          CL**4
00947       EXIT.                                                          CL**4
00948                                                                      CL**4
00949  9300-XCTL.                                                          CL**4
00950      EXEC CICS XCTL                                                  CL**4
00951          PROGRAM  (THIS-PGM)                                         CL**4
00952          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL**4
00953          LENGTH   (PI-COMM-LENGTH)                                   CL**4
00954      END-EXEC.                                                       CL**4
00955                                                                      CL**4
00956  9300-EXIT.                                                          CL**4
00957       EXIT.                                                          CL**4
00958                                                                      CL**4
00959  9400-CLEAR.                                                         CL**4
00960      MOVE DFHENTER              TO  EIBAID.                          CL**4
00961      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                        CL**4
00962      GO TO 9300-XCTL.                                                CL**4
00963                                                                      CL**4
00964  9400-EXIT.                                                          CL**4
00965      EXIT.                                                           CL**4
00966                                                                      CL**4
00967      EJECT                                                        EL153
00968  9600-PGMIDERR.                                                      CL**4
00969      EXEC CICS HANDLE CONDITION                                      CL**4
00970          PGMIDERR (8300-SEND-TEXT)                                   CL**4
00971      END-EXEC.                                                       CL**4
00972                                                                      CL**4
00973      MOVE THIS-PGM               TO  LOGOFF-PGM                      CL**4
00974                                      PI-CALLING-PROGRAM.             CL**4
00975      MOVE SPACES                 TO  PI-ENTRY-CD-1.                  CL**4
00976      MOVE 'EL005'                TO  THIS-PGM.                       CL**4
00977      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                    CL**4
00978      GO TO 9300-XCTL.                                                CL**4
00979                                                                      CL**4
00980  9600-EXIT.                                                          CL**4
00981       EXIT.                                                          CL**4
00982                                                                      CL**4
00983      EJECT                                                           CL**4
00984  9900-ERROR-FORMAT.                                                  CL**4
00985      EXEC CICS LINK                                                  CL**4
00986          PROGRAM  ('EL001')                                          CL**4
00987          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                    CL**4
00988          LENGTH   (EMI-COMM-LENGTH)                                  CL**4
00989      END-EXEC.                                                       CL**4
00990                                                                      CL**4
00991      MOVE EMI-LINE1          TO MERMSG1O.                            CL**4
00992      MOVE EMI-LINE2          TO MERMSG2O.                            CL**4
00993                                                                   EL153
00994  9900-EXIT.                                                       EL153
00995       EXIT.                                                       EL153
00996                                                                   EL153
00997  9981-NOTOPEN-MSTR.                                               EL153
00998      MOVE ER-0154                TO  EMI-ERROR.                      CL**4
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01000      MOVE -1                     TO  MPFNUMBL.                       CL**4
01001      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
01002                                                                   EL153
01003  9981-EXIT.                                                       EL153
01004       EXIT.                                                       EL153
01005                                                                   EL153
01006  9982-NOTOPEN-TRLR.                                               EL153
01007      MOVE ER-0172                TO  EMI-ERROR.                      CL**4
01008      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01009      MOVE -1                     TO  MPFNUMBL.                       CL**4
01010      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
01011                                                                   EL153
01012  9990-ERROR.                                                         CL**4
01013      EXEC CICS LINK                                                  CL**4
01014          PROGRAM  ('EL004')                                          CL**4
01015          COMMAREA (DFHEIBLK)                                         CL**4
01016          LENGTH   (64)                                               CL**4
01017      END-EXEC.                                                       CL**4
01018                                                                   EL153
01019      GO TO 8200-SEND-DATAONLY.                                       CL**4
01020                                                                   EL153
01021  9995-SECURITY-VIOLATION.                                         EL153
01022                              COPY ELCSCTP SUPPRESS.                  CL**4
01023                                                                   EL153
