00001  IDENTIFICATION DIVISION.                                         03/08/96
00002                                                                   EL127
00003  PROGRAM-ID.                 EL127 .                                 LV014
00004 *              PROGRAM CONVERTED BY                                  CL*13
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*13
00006 *              CONVERSION DATE 02/07/95 10:52:52.                    CL*13
00007 *                            VMOD=2.014                              CL*14
00008 *                                                                 EL127
00008 *                                                                 EL127
00009 *AUTHOR.    LOGIC, INC.                                              CL*13
00010 *           DALLAS, TEXAS.                                           CL*13
00011                                                                   EL127
00012 *DATE-COMPILED.                                                      CL*13
00013                                                                   EL127
00014 *SECURITY.   *****************************************************   CL*13
00015 *            *                                                   *   CL*13
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*13
00017 *            *                                                   *   CL*13
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*13
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*13
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*13
00021 *            *                                                   *   CL*13
00022 *            *****************************************************   CL*13
00023                                                                   EL127
00024 *REMARKS.                                                            CL**3
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR       CL**3
00026 *    THE CERTIFICATE LOOK-UP.                                        CL**3
00027                                                                   EL127
00028 *    SCREENS     - EL127A - CERT LOOK-UP QUALIFICATION               CL**3
00029                                                                   EL127
00030 *    ENTERED BY  - EL126 - MASTER MENU                               CL**3
00031 *                  EL130 - NEW CLAIM SET-UP                          CL**3
00032 *                  EL150 - STATUS DISPLAY (ENTERS EL1273 ONLY)       CL**3
00033                                                                   EL127
00034 *    EXIT TO     - CALLING PROGRAM                                   CL**3
00035 *                  EL130 - NEW CLAIM SETUP                           CL**3
00036                                                                   EL127
00037 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE                 CL**3
00038 *                  ERACCT2 - CREDIT ACCOUNT MASTER FILE              CL**3
00039                                                                   EL127
00040 *    OUTPUT FILE - NONE                                              CL**3
00041                                                                   EL127
00042 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE        CL**3
00043 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE      CL**3
00044 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR           CL**3
00045 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM    CL**3
00046 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE     CL**3
00047 *                  RECORD KEY INFORMATION NEEDED BY EL1272 TO        CL**3
00048 *                  LOCATE THE CERTIFICATE.                           CL**3
00049                                                                   EL127
00050                                                                   EL127
00051 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON        CL**3
00052 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE    CL**3
00053 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE   CL**3
00054 *                  ENTRIES (XCTL FROM CICS VIA EX15) THE SCREEN      CL**3
00055 *                  WILL BE READ AND ACTION WILL BE BASED ON THE      CL**3
00056 *                  MAINTENANCE TYPE INDICATED.                       CL**3
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID, POPULATE ACOMPO
101501*                              COMMENT OUT ORIGINAL CO ID CODE
110106* 110106  CR2005050500006  PEMA  ADD STATE TO OPTION 3 
101501******************************************************************

00057                                                                   EL127
00058      EJECT                                                        EL127
00059  ENVIRONMENT DIVISION.                                            EL127
00060                                                                   EL127
00061  DATA DIVISION.                                                   EL127
00062                                                                   EL127
00063  WORKING-STORAGE SECTION.                                         EL127
00064                                                                   EL127
00065                                                                   EL127
00066  77  FILLER  PIC X(32)  VALUE '********************************'. EL127
00067  77  FILLER  PIC X(32)  VALUE '*    EL127 WORKING STORAGE     *'. EL127
00068  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.014 *********'.    CL*14
00069                                                                   EL127
00070                                      COPY ELCSCTM.                   CL**8
00071                                                                   EL127
00072                                      COPY ELCSCRTY.                  CL**8
00073                                                                   EL127
00074  01  WS-DATE-AREA.                                                EL127
00075      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.   EL127
00076      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.   EL127
00077                                                                   EL127
00078  01  FILLER                          COMP-3.                      EL127
00079      05  WS-READNEXT-SW              PIC S9       VALUE ZERO.     EL127
00080      05  TIME-IN                     PIC S9(7)    VALUE ZERO.     EL127
00081      05  TIME-OUT                    REDEFINES                    EL127
00082          TIME-IN                     PIC S9(3)V9(4).              EL127
00083                                                                   EL127
00084  01  FILLER         COMP SYNC.                                       CL**5
00085      05  SC-ITEM                     PIC S9(4)    VALUE +0001.    EL127
00086                                                                   EL127
00087  01  FILLER.                                                      EL127
00088      05  XCTL-725                    PIC X(8)     VALUE 'EL725'.     CL**5
00089      05  QID.                                                     EL127
00090          10  QID-TERM                PIC X(4).                    EL127
00091          10  FILLER                  PIC X(4)     VALUE '127A'.   EL127
00092      05  QID-ITEM                    PIC S9(4)    VALUE +1 COMP.  EL127
00093      05  WS-KEY-LENGTH               PIC S9(4)    VALUE +0 COMP.     CL*13
00094                                                                      CL*13
00095      05  PART-KEY-ON-SW              PIC X(01)    VALUE 'N'.         CL*13
00096          88  PART-KEY-ON                          VALUE 'Y'.         CL*13
00097                                                                      CL*13
00098      05  PART-FIELD-ON-SW            PIC X(01)    VALUE ' '.         CL*13
00099          88  PART-FIELD-ACCT                      VALUE 'A'.         CL*13
00100          88  PART-FIELD-STATE                     VALUE 'S'.         CL*13
00101          88  PART-FIELD-CERT                      VALUE 'C'.         CL*13
00102                                                                   EL127
00103      05  WS-CNTL-KEY.                                             EL127
00104          10  WS-CNTL-ID              PIC X(3).                    EL127
00105          10  WS-CNTL-TYPE            PIC X.                       EL127
00106          10  WS-CNTL-USER            PIC X(4)     VALUE SPACES.   EL127
00107          10  WS-CNTL-SEQ             PIC S9(4)    VALUE +0 COMP.  EL127
00108                                                                   EL127
00109      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL127S'. EL127
00110      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127A'. EL127
00111                                                                   EL127
00112      05  FILLER                      REDEFINES                    EL127
00113          WS-MAP-NAME.                                             EL127
00114          10  FILLER                  PIC XX.                      EL127
00115          10  WS-MAP-NUMBER           PIC X(4).                    EL127
00116          10  FILLER                  PIC XX.                         CL*14
00117                                                                   EL127
00118      05  THIS-PGM                    PIC X(8)     VALUE 'EL127'.  EL127
00119                                                                   EL127
00120      05  WS-CNTL-REC-FOUND-SW        PIC X(01)    VALUE SPACE.       CL**8
00121      05  WS-NEXT-COMPANY-ID          PIC X(03)    VALUE SPACES.      CL**8
00122                                                                      CL**8
00123      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'. EL127
00124      05  WS-ACCOUNT-MASTER-DSID      PIC X(8)     VALUE 'ERACCT2'.EL127
00125      05  WS-CERT-MASTER-DSID         PIC X(8)     VALUE 'ELCERT'. EL127
00126      05  WS-CERT-AIX01-DSID          PIC X(8)     VALUE 'ELCERT2'.EL127
00127      05  WS-CERT-AIX02-DSID          PIC X(8)     VALUE 'ELCERT3'.EL127
00128      05  WS-CERT-AIX03-DSID          PIC X(8)     VALUE 'ELCERT4'.EL127
00129      05  WS-CERT-AIX04-DSID          PIC X(8)     VALUE 'ELCERT5'.EL127
00130      05  WS-CERT-AIX05-DSID          PIC X(8)     VALUE 'ELCERT6'.EL127
00131                                                                   EL127
00132      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX1'.   EL127
00133                                                                      CL*13
00134      05  WK-SC-STATE.                                                CL*13
00135          12  WK-SC-STATE-1           PIC X.                          CL*13
00136          12  WK-SC-STATE-2           PIC X.                          CL*13
00137                                                                      CL*13
00138      05  WK-SC-CERT.                                                 CL*13
00139          12  WK-SC-CERT-1            PIC X.                          CL*13
00140          12  WK-SC-CERT-2            PIC X.                          CL*13
00141          12  WK-SC-CERT-3            PIC X.                          CL*13
00142          12  WK-SC-CERT-4            PIC X.                          CL*13
00143          12  WK-SC-CERT-5            PIC X.                          CL*13
00144          12  WK-SC-CERT-6            PIC X.                          CL*13
00145          12  WK-SC-CERT-7            PIC X.                          CL*13
00146          12  WK-SC-CERT-8            PIC X.                          CL*13
00147          12  WK-SC-CERT-9            PIC X.                          CL*13
00148          12  WK-SC-CERT-10           PIC X.                          CL*13
00149                                                                   EL127
00150      05  WS-DEEDIT-FIELD             PIC X(15)    VALUE ZERO.     EL127
00151                                                                   EL127
00152      05  WS-DEEDIT-FIELD-V0          REDEFINES                    EL127
00153          WS-DEEDIT-FIELD             PIC S9(15).                  EL127
00154                                                                   EL127
00155      05  WS-INPUT-FIELD              PIC X(50)    VALUE SPACES.   EL127
00156                                                                   EL127
00157      05  WS-INPUT-CHAR               REDEFINES                    EL127
00158          WS-INPUT-FIELD              PIC X                        EL127
00159          OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.      EL127
00160                                                                   EL127
00161  01  WS-FIRST-NAME.                                                  CL*10
00162      05  WS-FIRST-INITIAL            PIC X        VALUE SPACES.      CL**9
00163      05  WS-FIRST-REST               PIC X(14)    VALUE SPACES.      CL**9
00164                                                                      CL**9
00165  01  WS-INITIALS.                                                    CL**9
00166      05  WS-INITIAL-FIRST            PIC X        VALUE SPACES.      CL**9
00167      05  WS-INITIAL-MIDDLE           PIC X        VALUE SPACES.      CL**9
00168                                                                      CL**9
00169      05  PI-ACCOUNT-KEY.                                          EL127
00170          10  PI-AK-COMPANY-CD        PIC X.                       EL127
00171          10  PI-AK-CARRIER           PIC X.                       EL127
00172          10  PI-AK-GROUP             PIC X(06).                      CL**6
00173          10  PI-AK-STATE             PIC XX.                      EL127
00174          10  PI-AK-ACCOUNT           PIC X(10).                      CL**6
00175          10  PI-AK-EXPIRE-DATE       PIC XX.                      EL127
00176                                                                   EL127
00177      EJECT                                                        EL127
00178      05  ERROR-MESSAGES.                                          EL127
00179          10  ER-0004                 PIC X(4)     VALUE '0004'.   EL127
00180          10  ER-0008                 PIC X(4)     VALUE '0008'.   EL127
00181          10  ER-0019                 PIC X(4)     VALUE '0019'.      CL**8
00182          10  ER-0022                 PIC X(4)     VALUE '0022'.   EL127
00183          10  ER-0029                 PIC X(4)     VALUE '0029'.   EL127
00184          10  ER-0070                 PIC X(4)     VALUE '0070'.   EL127
00185          10  ER-0089                 PIC X(4)     VALUE '0089'.      CL**8
00186          10  ER-0194                 PIC X(4)     VALUE '0194'.   EL127
00187          10  ER-0195                 PIC X(4)     VALUE '0195'.   EL127
00188          10  ER-0196                 PIC X(4)     VALUE '0196'.   EL127
00189          10  ER-0197                 PIC X(4)     VALUE '0197'.   EL127
00190          10  ER-0198                 PIC X(4)     VALUE '0198'.   EL127
00191          10  ER-0201                 PIC X(4)     VALUE '0201'.   EL127
00192          10  ER-0210                 PIC X(4)     VALUE '0210'.   EL127
00193          10  ER-0215                 PIC X(4)     VALUE '0215'.   EL127
00194          10  ER-0216                 PIC X(4)     VALUE '0216'.   EL127
00195          10  ER-0228                 PIC X(4)     VALUE '0228'.      CL**8
00196          10  ER-0488                 PIC X(4)     VALUE '0488'.   EL127
00197          10  ER-0671                 PIC X(4)     VALUE '0671'.      CL**4
00198          10  ER-0764                 PIC X(4)     VALUE '0764'.      CL**9
00199          10  ER-0765                 PIC X(4)     VALUE '0765'.      CL*10
00200          10  ER-2370                 PIC X(4)     VALUE '2370'.   EL127
00201          10  ER-2371                 PIC X(4)     VALUE '2371'.   EL127
00202          10  ER-2373                 PIC X(4)     VALUE '2373'.   EL127
00203          10  ER-8100                 PIC X(4)     VALUE '8100'.      CL*13
00204          10  ER-8101                 PIC X(4)     VALUE '8101'.      CL*13
00205          10  ER-8102                 PIC X(4)     VALUE '8102'.      CL*13
00206          10  ER-8103                 PIC X(4)     VALUE '8103'.      CL*13
00207          10  ER-8104                 PIC X(4)     VALUE '8104'.      CL*13
00208          10  ER-8105                 PIC X(4)     VALUE '8105'.      CL*13
00209          10  ER-8106                 PIC X(4)     VALUE '8106'.      CL*13
00210          10  ER-8107                 PIC X(4)     VALUE '8107'.      CL*13
00211                                                                   EL127
00212      EJECT                                                        EL127
00213                                      COPY ELCINTF.                   CL**8
00214                                                                   EL127
00215                                      COPY ELC127PI.                  CL**8
00216          16  FILLER                  PIC X(167).                     CL*13
00217          16  PI-PART-KEY-SW          PIC X(01).                      CL*13
00218          16  PI-PART-FIELD-SW        PIC X(01).                      CL*13
00219          16  FILLER                  PIC X(138).                     CL*14
00220                                                                      CL*13
00221                                                                   EL127
00222      EJECT                                                        EL127
00223                                      COPY ELCEMIB.                   CL**8
00224                                                                   EL127
00225      EJECT                                                        EL127
00226                                      COPY ELCDATE.                   CL**8
00227                                                                   EL127
00228      EJECT                                                        EL127
00229                                      COPY ELCLOGOF.                  CL**8
00230                                                                   EL127
00231      EJECT                                                        EL127
00232                                      COPY EL127S.                    CL**8
00233                                                                   EL127
00234      EJECT                                                        EL127
00235                                      COPY ELCATTR.                   CL**8
00236                                                                   EL127
00237      EJECT                                                        EL127
00238                                      COPY ELCAID.                    CL**8
00239                                                                   EL127
00240  01  FILLER                      REDEFINES                        EL127
00241      DFHAID.                                                      EL127
00242      05  FILLER                      PIC X(8).                    EL127
00243                                                                   EL127
00244      05  PF-VALUES                   PIC X                        EL127
00245          OCCURS 24 TIMES.                                         EL127
00246                                                                   EL127
00247      EJECT                                                        EL127
00248  LINKAGE SECTION.                                                 EL127
00249  01  DFHCOMMAREA                     PIC X(1024).                 EL127
00250                                                                   EL127
00251 *01 PARMLIST                         COMP                            CL*13
00252 *                                    SYNC.                           CL*13
00253 *    05  FILLER                      PIC S9(9).                      CL*13
00254 *    05  ELCERT-POINTER              PIC S9(9).                      CL*13
00255 *    05  ERACCT-POINTER              PIC S9(9).                      CL*13
00256 *    05  ELCNTL-POINTER              PIC S9(9).                      CL*13
00257                                                                   EL127
00258      EJECT                                                        EL127
00259                                      COPY ELCCERT.                   CL**8
00260                                                                   EL127
00261      EJECT                                                        EL127
00262                                      COPY ERCACCT.                   CL**8
00263                                                                   EL127
00264      EJECT                                                        EL127
00265                                      COPY ELCCNTL.                   CL*13
00266                                                                   EL127
00267      EJECT                                                        EL127
00268  PROCEDURE DIVISION.                                              EL127
00269                                                                   EL127
00270      CONTINUE.                                                       CL*13
00271                                                                   EL127
00272      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL127
00273      MOVE '5'                    TO  DC-OPTION-CODE.              EL127
00274      PERFORM 8500-DATE-CONVERSION.                                EL127
00275      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL127
00276      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL127
00277                                                                   EL127
00278      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL127
00279                                                                   EL127
00280      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL127
00281                                      EMI-SWITCH2.                 EL127
00282                                                                   EL127
00283 *    NOTE ******************************************************* EL127
00284 *         *                                                     * EL127
00285 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL127
00286 *         *  FROM ANOTHER MODULE.                               * EL127
00287 *         *                                                     * EL127
00288 *         *******************************************************.EL127
00289                                                                   EL127
00290      IF EIBCALEN NOT GREATER THAN ZERO                            EL127
00291          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL127
00292          GO TO 8300-SEND-TEXT.                                    EL127
00293                                                                   EL127
00294      EXEC CICS HANDLE CONDITION                                   EL127
00295          PGMIDERR (9600-PGMIDERR)                                 EL127
00296          NOTFND   (0030-MAIN-LOGIC)                               EL127
00297          ENDFILE  (0030-MAIN-LOGIC)                               EL127
00298          ERROR    (9990-ERROR)                                    EL127
00299      END-EXEC.                                                    EL127
00300                                                                   EL127
00301      IF PI-CALLING-PROGRAM NOT = 'EL1272'                            CL*10
00302          MOVE ZERO                TO PI-ALT-NAME-COUNT.              CL*10
00303                                                                      CL*10
00304      EJECT                                                        EL127
00305  0010-MAIN-LOGIC.                                                 EL127
00306      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL127
00307          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL127
00308              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL127
00309              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL127
00310              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL127
00311              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL127
00312              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL127
00313              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL127
00314              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL127
00315              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL127
00316            ELSE                                                   EL127
00317              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL127
00318              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL127
00319              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL127
00320              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL127
00321              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL127
00322              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL127
00323              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL127
00324              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL127
00325              PERFORM 7000-BUILD-SCREEN                            EL127
00326        ELSE                                                       EL127
00327          GO TO 0020-CONTINUE-PROCESSING.                             CL*10
00328                                                                   EL127
00329  0015-INITIALIZE.                                                    CL*10
00330 *    NOTE ******************************************************* EL127
00331 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL127
00332 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL127
00333 *         *******************************************************.EL127
00334      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL127
00335                                      PI-CONTROL-IN-PROGRESS.      EL127
00336                                                                   EL127
00337      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL127
00338                                      PI-LINE-COUNT                EL127
00339                                      PI-BROWSE-SW                 EL127
00340                                      PI-KEY-LENGTH                EL127
00341                                      PI-TS-ITEM                   EL127
00342                                      PI-END-OF-FILE               EL127
00343                                      PI-START-SW                  EL127
00344                                      PI-AIX-RECORD-COUNT.         EL127
00345                                                                   EL127
00346 *    NOTE ******************************************************* EL127
00347 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   * EL127
00348 *         *  FOR EL127.                                         * EL127
00349 *         *******************************************************.EL127
00350                                                                   EL127
00351      MOVE LOW-VALUES             TO  EL127AO.                     EL127
00352                                                                   EL127
00353      GO TO 8100-SEND-INITIAL-MAP.                                    CL**8
00354                                                                   EL127
00355      EJECT                                                        EL127
00356  0020-CONTINUE-PROCESSING.                                           CL*10
00357      IF PI-1ST-TIME-SW NOT = ZERO                                 EL127
00358          GO TO 0015-INITIALIZE.                                      CL*10
00359                                                                   EL127
00360 *    NOTE ******************************************************* EL127
00361 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL127
00362 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL127
00363 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL127
00364 *         *******************************************************.EL127
00365                                                                   EL127
00366      IF EIBAID = DFHCLEAR                                         EL127
00367          GO TO 9400-CLEAR.                                        EL127
00368                                                                   EL127
00369      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL127
00370          MOVE LOW-VALUES         TO  EL127AO                      EL127
00371          MOVE -1                 TO  APFKL                        EL127
00372          MOVE ER-0008            TO  EMI-ERROR                    EL127
00373          GO TO 8200-SEND-DATAONLY.                                   CL**8
00374                                                                   EL127
00375      EXEC CICS RECEIVE                                            EL127
00376          INTO   (EL127AI)                                         EL127
00377          MAPSET (WS-MAPSET-NAME)                                  EL127
00378          MAP    (WS-MAP-NAME)                                     EL127
00379      END-EXEC.                                                    EL127
00380                                                                   EL127
00381      IF APFKL GREATER ZERO                                        EL127
00382          IF EIBAID NOT = DFHENTER                                 EL127
00383              MOVE ER-0004        TO  EMI-ERROR                    EL127
00384              MOVE AL-UNBOF       TO  APFKA                        EL127
00385              MOVE -1             TO  APFKL                        EL127
00386              GO TO 8200-SEND-DATAONLY                                CL**8
00387          ELSE                                                        CL**2
00388              IF APFKO IS NUMERIC                                     CL**2
00389              IF APFKO GREATER 0 AND LESS 25                       EL127
00390                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL127
00391                ELSE                                               EL127
00392                  MOVE ER-0029        TO  EMI-ERROR                EL127
00393                  MOVE AL-UNBOF       TO  APFKA                    EL127
00394                  MOVE -1             TO  APFKL                    EL127
00395                  GO TO 8200-SEND-DATAONLY.                           CL**8
00396                                                                   EL127
00397      IF EIBAID = DFHPF12                                          EL127
00398          MOVE 'EL010'         TO  THIS-PGM                        EL127
00399          GO TO 9300-XCTL.                                         EL127
00400                                                                   EL127
00401      IF EIBAID = DFHPF23                                          EL127
00402          GO TO 9000-RETURN-CICS.                                  EL127
00403                                                                   EL127
00404      IF EIBAID = DFHPF24                                          EL127
00405          MOVE 'EL126'         TO  THIS-PGM                        EL127
00406          GO TO 9300-XCTL.                                         EL127
00407                                                                   EL127
00408      IF EIBAID NOT = DFHENTER AND DFHPF6 AND DFHPF7 AND DFHPF9       CL**5
00409          MOVE ER-0008            TO  EMI-ERROR                    EL127
00410          MOVE -1                 TO  APFKL                        EL127
00411          GO TO 8200-SEND-DATAONLY.                                   CL**8
00412                                                                   EL127
00413      EJECT                                                        EL127
00414  0025-MAIN-LOGIC.                                                 EL127
00415      IF EIBAID = DFHPF6                                           EL127
00416         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                    EL127
00417            PERFORM 5000-NEXT-COMPANY THRU 5000-EXIT               EL127
00418            PERFORM 5500-WRITE-SECURITY-TEMP-STORE THRU 5500-EXIT  EL127
00419         ELSE                                                      EL127
00420            MOVE -1               TO  APFKL                        EL127
00421            MOVE ER-0008          TO  EMI-ERROR                    EL127
00422            GO TO 8200-SEND-DATAONLY.                                 CL**8
00423                                                                   EL127
00424      IF EIBAID = DFHPF7                                           EL127
00425         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                    EL127
00426            PERFORM 5000-NEXT-COMPANY THRU 5000-EXIT               EL127
00427            PERFORM 5500-WRITE-SECURITY-TEMP-STORE THRU 5500-EXIT  EL127
00428            MOVE PI-ORIGINAL-COMPANY-CD   TO  PI-COMPANY-CD        EL127
00429            MOVE PI-ORIGINAL-COMPANY-ID   TO  PI-COMPANY-ID        EL127
00430         ELSE                                                      EL127
00431            MOVE -1               TO  APFKL                        EL127
00432            MOVE ER-0008          TO  EMI-ERROR                    EL127
00433            GO TO 8200-SEND-DATAONLY.                                 CL**8
00434                                                                      CL**5
00435      IF EIBAID = DFHPF9                                              CL**5
00436        IF PI-HAS-CLAS-IC-CRDTCRD                                     CL**5
00437           MOVE XCTL-725            TO THIS-PGM                       CL**5
00438           GO TO 9300-XCTL.                                           CL**5
00439                                                                   EL127
00440      MOVE SPACES                 TO  PI-SELECTION-CRITERIA        EL127
00441                                      PI-CERTIFICATE-KEY.          EL127
00442                                                                   EL127
00443      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD             EL127
00444                                      PI-CK-COMPANY-CD.            EL127
00445                                                                   EL127
00446      MOVE 'EL1272'               TO  THIS-PGM.                    EL127
00447                                                                   EL127
00448      IF PI-PROCESSOR-ID = 'LGXX'                                  EL127
00449          NEXT SENTENCE                                            EL127
00450      ELSE                                                         EL127
00451          EXEC CICS READQ TS                                       EL127
00452              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL127
00453              INTO    (SECURITY-CONTROL)                           EL127
00454              LENGTH  (SC-COMM-LENGTH)                             EL127
00455              ITEM    (SC-ITEM)                                    EL127
00456          END-EXEC                                                 EL127
00457          MOVE SC-CREDIT-DISPLAY (31)  TO  PI-DISPLAY-CAP          EL127
00458          MOVE SC-CREDIT-UPDATE  (31)  TO  PI-MODIFY-CAP           EL127
00459          IF NOT DISPLAY-CAP                                       EL127
00460              MOVE 'READ'              TO  SM-READ                 EL127
00461              PERFORM 9995-SECURITY-VIOLATION                      EL127
00462              MOVE ER-0070             TO  EMI-ERROR               EL127
00463              GO TO 8100-SEND-INITIAL-MAP.                            CL**8
00464                                                                   EL127
00465      EJECT                                                        EL127
00466 ******************************************************************EL127
00467 *           O P T I O N  1  P R O C E S S I N G                  *EL127
00468 ******************************************************************EL127
00469                                                                   EL127
00470      IF ACRTNO4L GREATER ZERO  OR                                 EL127
00471         ACRTSX4L GREATER ZERO                                     EL127
00472          NEXT SENTENCE                                            EL127
00473      ELSE                                                         EL127
00474          GO TO 0027-MAIN-LOGIC.                                   EL127
00475                                                                   EL127
00476      MOVE WS-CERT-AIX04-DSID     TO  PI-DSID.                     EL127
00477      MOVE '1'                    TO  PI-OPTION.                   EL127
00478                                                                   EL127
00479      IF ACRTNO4L NOT GREATER ZERO  AND                            EL127
00480         ACRTSX4L GREATER ZERO                                     EL127
00481          MOVE ER-0210            TO  EMI-ERROR                    EL127
00482          MOVE -1                 TO  ACRTNO4L                     EL127
00483          GO TO 8200-SEND-DATAONLY.                                   CL**8
00484                                                                   EL127
00485      MOVE ACRTNO4I               TO  PI-SC-CERT-PRIME-A4.         EL127
00486      MOVE +11                    TO  PI-KEY-LENGTH.               EL127
00487                                                                   EL127
00488      IF ACRTSX4L GREATER ZERO                                     EL127
00489          MOVE ACRTSX4I           TO  PI-SC-CERT-SFX-A4            EL127
00490          MOVE +12                TO  PI-KEY-LENGTH.               EL127
00491                                                                   EL127
00492      MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.          EL127
00493                                                                   EL127
00494      MOVE -1                     TO  ACRTNO4L.                    EL127
00495      PERFORM 4000-READ-CERT-FILE.                                 EL127
00496                                                                   EL127
00497      EJECT                                                        EL127
00498  0027-MAIN-LOGIC.                                                 EL127
00499 ******************************************************************EL127
00500 *           O P T I O N  2  P R O C E S S I N G                  *EL127
00501 ******************************************************************EL127
00502                                                                   EL127
00503      IF ACERTNOL GREATER ZERO  OR                                 EL127
00504         ACERTSXL GREATER ZERO  OR                                 EL127
00505         AACCTNOL GREATER ZERO  OR                                 EL127
00506         ASTATEL  GREATER ZERO  OR                                 EL127
00507         ACARIERL GREATER ZERO  OR                                 EL127
00508         AGROUPL  GREATER ZERO  OR                                 EL127
00509         AEDATEL  GREATER ZERO                                     EL127
00510          NEXT SENTENCE                                            EL127
00511        ELSE                                                       EL127
00512          GO TO 0100-MAIN-LOGIC.                                   EL127
00513                                                                   EL127
00514       ADD +1                    TO  WS-KEY-LENGTH.                   CL*13
00515 ************************************************************      EL127
00516 *        SECURITY CHECK FOR ACCOUNT AND CARRIER NO         *      EL127
00517 *                      03/29/84                            *      EL127
00518 ************************************************************      EL127
00519                                                                   EL127
00520      IF  PI-NO-ACCOUNT-SECURITY AND PI-NO-CARRIER-SECURITY        EL127
00521          GO TO 0028-PROCESS-OPTION-2.                             EL127
00522                                                                   EL127
00523      IF  PI-NO-ACCOUNT-SECURITY                                   EL127
00524          GO TO 0028-CHECK-CARRIER-SECURITY.                       EL127
00525                                                                   EL127
00526      IF  AACCTNOL GREATER ZERO                                    EL127
00527          NEXT SENTENCE                                            EL127
00528      ELSE                                                         EL127
00529          GO TO 0028-CHECK-CARRIER-SECURITY.                       EL127
00530                                                                   EL127
00531      IF  AACCTNOI = PI-ACCOUNT-SECURITY                           EL127
00532          MOVE AL-UANON           TO  AACCTNOA                     EL127
00533      ELSE                                                         EL127
00534          MOVE -1                 TO  AACCTNOL                     EL127
00535          MOVE AL-UABON           TO  AACCTNOA                     EL127
00536          MOVE ER-2371            TO  EMI-ERROR                    EL127
00537          PERFORM 9900-ERROR-FORMAT.                               EL127
00538                                                                   EL127
00539  0028-CHECK-CARRIER-SECURITY.                                     EL127
00540      IF  PI-NO-CARRIER-SECURITY                                   EL127
00541          GO TO  0028-PROCESS-OPTION-2.                            EL127
00542                                                                   EL127
00543      IF ACARIERL GREATER ZERO                                     EL127
00544          NEXT SENTENCE                                            EL127
00545      ELSE                                                         EL127
00546          GO TO  0028-ERROR-CHECK.                                 EL127
00547                                                                   EL127
00548      IF  ACARIERI = PI-CARRIER-SECURITY                           EL127
00549          MOVE AL-UANON            TO  ACARIERA                    EL127
00550      ELSE                                                         EL127
00551          MOVE -1                  TO  ACARIERL                    EL127
00552          MOVE ER-2370             TO  EMI-ERROR                   EL127
00553          MOVE AL-UABON            TO  ACARIERA                    EL127
00554          PERFORM 9900-ERROR-FORMAT.                               EL127
00555                                                                   EL127
00556  0028-ERROR-CHECK.                                                EL127
00557      IF  EMI-FATAL-CTR GREATER ZERO                               EL127
00558          GO TO 8200-SEND-DATAONLY.                                   CL**8
00559                                                                   EL127
00560  0028-PROCESS-OPTION-2.                                           EL127
00561      MOVE WS-CERT-MASTER-DSID    TO  PI-DSID.                     EL127
00562      MOVE '2'                    TO  PI-OPTION.                   EL127
00563                                                                   EL127
00564      IF ACARIERL GREATER ZERO                                     EL127
00565          MOVE ACARIERI           TO  PI-SC-CARRIER                EL127
00566      ELSE                                                         EL127
00567          IF PI-CERT-ACCESS-CONTROL = ('1' OR '2' OR '4')          EL127
00568              MOVE ER-0194        TO  EMI-ERROR                    EL127
00569              PERFORM 9900-ERROR-FORMAT                            EL127
00570              MOVE -1             TO  ACARIERL.                    EL127
00571                                                                   EL127
00572      IF ACARIERL GREATER ZERO  AND                                   CL*13
00573         AGROUPL  = ZERO  AND                                         CL*13
00574         ASTATEL  = ZERO  AND                                         CL*13
00575         AACCTNOL = ZERO  AND                                         CL*13
00576         AEDATEL  = ZERO  AND                                         CL*13
00577         ACERTNOL = ZERO  AND                                         CL*13
00578         ACERTSXL = ZERO                                              CL*13
00579         MOVE SPACES                  TO  PI-SC-GROUP                 CL*13
00580                                          PI-SC-STATE                 CL*13
00581                                          PI-SC-ACCOUNT               CL*13
00582                                          PI-SC-EFF-DATE              CL*13
00583                                          PI-SC-CERT-NO               CL*13
00584         ADD +1                    TO  WS-KEY-LENGTH                  CL*13
00585         MOVE 'Y'                  TO  PART-KEY-ON-SW                 CL*13
00586         GO TO 0400-MAIN-LOGIC.                                       CL*13
00587 ****    GO TO 0028-PROCESS-OPTION-2-CONT.                            CL*13
00588                                                                      CL*13
00589      IF  AGROUPL  > ZERO  AND                                        CL*13
00590          ACARIERL = ZERO                                             CL*13
00591          MOVE -1                 TO  AGROUPL                         CL*13
00592          MOVE AL-UABON           TO  AGROUPA                         CL*13
00593          MOVE ER-8101            TO  EMI-ERROR                       CL*13
00594          PERFORM 9900-ERROR-FORMAT.                                  CL*13
00595                                                                      CL*13
00596      IF AGROUPL GREATER ZERO                                      EL127
00597          MOVE AGROUPI            TO  PI-SC-GROUP                  EL127
00598      ELSE                                                         EL127
00599          IF PI-CERT-ACCESS-CONTROL = '1'                          EL127
00600              MOVE ER-0195        TO  EMI-ERROR                    EL127
00601              PERFORM 9900-ERROR-FORMAT                            EL127
00602              MOVE -1             TO  AGROUPL.                     EL127
00603                                                                   EL127
00604      IF AGROUPL  > ZERO  AND                                         CL*13
00605         ASTATEL  = ZERO  AND                                         CL*13
00606         AACCTNOL = ZERO  AND                                         CL*13
00607         AEDATEL  = ZERO  AND                                         CL*13
00608         ACERTNOL = ZERO  AND                                         CL*13
00609         ACERTSXL = ZERO                                              CL*13
00610         MOVE SPACES                  TO  PI-SC-STATE                 CL*13
00611                                          PI-SC-ACCOUNT               CL*13
00612                                          PI-SC-EFF-DATE              CL*13
00613                                          PI-SC-CERT-NO               CL*13
00614         ADD +7                    TO  WS-KEY-LENGTH                  CL*13
00615         MOVE 'Y'                  TO  PART-KEY-ON-SW                 CL*13
00616         GO TO 0400-MAIN-LOGIC.                                       CL*13
00617 ****    GO TO 0028-PROCESS-OPTION-2-CONT.                            CL*13
00618                                                                      CL*13
00619      IF ASTATEL GREATER ZERO AND                                     CL*13
00620          AGROUPL  = ZERO                                             CL*13
00621          MOVE -1                 TO  ASTATEL                         CL*13
00622          MOVE AL-UABON           TO  ASTATEA                         CL*13
00623          MOVE ER-8102            TO  EMI-ERROR                       CL*13
00624          PERFORM 9900-ERROR-FORMAT.                                  CL*13
00625                                                                      CL*13
00626      IF ASTATEL GREATER ZERO                                      EL127
00627          MOVE ASTATEI            TO  PI-SC-STATE                  EL127
00628                                      WK-SC-STATE                     CL*13
00629      ELSE                                                         EL127
00630          IF PI-CERT-ACCESS-CONTROL = (SPACES OR '1' OR '2')       EL127
00631              MOVE ER-0196        TO  EMI-ERROR                    EL127
00632              PERFORM 9900-ERROR-FORMAT                            EL127
00633              MOVE -1             TO  ASTATEL.                     EL127
00634                                                                      CL*13
00635      IF ASTATEL GREATER ZERO                                         CL*13
00636          IF WK-SC-STATE-1 = SPACES OR LOW-VALUES                     CL*13
00637              MOVE ER-8106        TO  EMI-ERROR                       CL*13
00638              MOVE -1             TO  ASTATEL                         CL*13
00639              GO TO 8200-SEND-DATAONLY                                CL*13
00640          ELSE                                                        CL*13
00641              IF WK-SC-STATE-2 = SPACES OR LOW-VALUES                 CL*13
00642                 MOVE 'S'         TO  PART-FIELD-ON-SW                CL*13
00643                 MOVE LOW-VALUES  TO  WK-SC-STATE-2                   CL*13
00644                 MOVE WK-SC-STATE TO  PI-SC-STATE.                    CL*13
00645                                                                      CL*13
00646      IF ASTATEL  > ZERO  AND                                         CL*13
00647         AACCTNOL = ZERO  AND                                         CL*13
00648         AEDATEL  = ZERO  AND                                         CL*13
00649         ACERTNOL = ZERO  AND                                         CL*13
00650         ACERTSXL = ZERO                                              CL*13
00651         MOVE SPACES                  TO  PI-SC-ACCOUNT               CL*13
00652                                          PI-SC-EFF-DATE              CL*13
00653                                          PI-SC-CERT-NO               CL*13
00654         MOVE 'Y'                  TO  PART-KEY-ON-SW                 CL*13
00655         IF PART-FIELD-STATE                                          CL*13
00656            ADD +8                 TO  WS-KEY-LENGTH                  CL*13
00657            GO TO 0400-MAIN-LOGIC                                     CL*13
00658         ELSE                                                         CL*13
00659            ADD +9                 TO  WS-KEY-LENGTH                  CL*13
00660            GO TO 0400-MAIN-LOGIC.                                    CL*13
00661 ****    GO TO 0028-PROCESS-OPTION-2-CONT.                            CL*13
00662                                                                      CL*13
00663      IF  AACCTNOL GREATER ZERO  AND                                  CL*13
00664          ASTATEL  = ZERO                                             CL*13
00665          MOVE -1                 TO  AACCTNOL                        CL*13
00666          MOVE AL-UABON           TO  AACCTNOA                        CL*13
00667          MOVE ER-8100            TO  EMI-ERROR                       CL*13
00668          PERFORM 9900-ERROR-FORMAT.                                  CL*13
00669                                                                   EL127
00670      IF AACCTNOL GREATER ZERO                                     EL127
00671          MOVE AACCTNOI           TO  PI-SC-ACCOUNT                EL127
00672      ELSE                                                         EL127
00673          MOVE ER-0197            TO  EMI-ERROR                    EL127
00674          PERFORM 9900-ERROR-FORMAT                                EL127
00675          MOVE -1                 TO  AACCTNOL.                    EL127
00676                                                                   EL127
00677      IF AACCTNOL > ZERO  AND                                         CL*13
00678         AEDATEL  = ZERO  AND                                         CL*13
00679         ACERTNOL = ZERO  AND                                         CL*13
00680         ACERTSXL = ZERO                                              CL*13
00681         MOVE SPACES                  TO  PI-SC-EFF-DATE              CL*13
00682                                          PI-SC-CERT-NO               CL*13
00683         ADD +19                   TO  WS-KEY-LENGTH                  CL*13
00684         MOVE 'Y'                  TO  PART-KEY-ON-SW                 CL*13
00685         GO TO 0400-MAIN-LOGIC.                                       CL*13
00686 ****    GO TO 0028-PROCESS-OPTION-2-CONT.                            CL*13
00687                                                                      CL*13
00688      IF  AEDATEL GREATER ZERO   AND                                  CL*13
00689          AACCTNOL = ZERO                                             CL*13
00690          MOVE -1                 TO  AEDATEL                         CL*13
00691          MOVE AL-UABON           TO  AEDATEA                         CL*13
00692          MOVE ER-8103            TO  EMI-ERROR                       CL*13
00693          PERFORM 9900-ERROR-FORMAT.                                  CL*13
00694                                                                      CL*13
00695      IF  AEDATEL GREATER   ZERO   AND                                CL*13
00696          AEDATEL LESS THAN +6                                        CL*13
00697          MOVE -1                 TO  AEDATEL                         CL*13
00698          MOVE AL-UABON           TO  AEDATEA                         CL*13
00699          MOVE ER-8105            TO  EMI-ERROR                       CL*13
00700          PERFORM 9900-ERROR-FORMAT.                                  CL*13
00701                                                                      CL*13
00702      IF AEDATEL GREATER ZERO                                      EL127
00703          MOVE AEDATEI            TO  WS-DEEDIT-FIELD              EL127
00704          PERFORM 8600-DEEDIT                                      EL127
00705          IF WS-DEEDIT-FIELD-V0 NUMERIC                            EL127
00706              MOVE WS-DEEDIT-FIELD-V0  TO  AEDATEO                 EL127
00707              INSPECT AEDATEI CONVERTING SPACES TO '/'                CL*13
00708              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       EL127
00709              MOVE '4'                TO  DC-OPTION-CODE           EL127
00710              PERFORM 8500-DATE-CONVERSION                         EL127
00711              IF DC-ERROR-CODE NOT = SPACES                        EL127
00712                  MOVE ER-0215        TO  EMI-ERROR                EL127
00713                  PERFORM 9900-ERROR-FORMAT                        EL127
00714                  MOVE -1             TO  AEDATEL                  EL127
00715                  MOVE AL-UABON       TO  AEDATEA                  EL127
00716              ELSE                                                 EL127
00717                  MOVE AL-UANON       TO  AEDATEA                  EL127
00718                  MOVE DC-BIN-DATE-1  TO  PI-SC-EFF-DATE           EL127
00719          ELSE                                                     EL127
00720              MOVE ER-0215        TO  EMI-ERROR                    EL127
00721              PERFORM 9900-ERROR-FORMAT                            EL127
00722              MOVE -1             TO  AEDATEL                      EL127
00723              MOVE AL-UABON       TO  AEDATEA                      EL127
00724      ELSE                                                         EL127
00725          MOVE ER-0216            TO  EMI-ERROR                    EL127
00726          PERFORM 9900-ERROR-FORMAT                                EL127
00727          MOVE -1                 TO  AEDATEL                      EL127
00728          MOVE AL-UABOF           TO  AEDATEA.                     EL127
00729                                                                   EL127
00730      IF AEDATEL  > ZERO  AND                                         CL*13
00731         ACERTNOL = ZERO  AND                                         CL*13
00732         ACERTSXL = ZERO                                              CL*13
00733         ADD +21                   TO  WS-KEY-LENGTH                  CL*13
00734         MOVE 'Y'                  TO  PART-KEY-ON-SW                 CL*13
00735         MOVE SPACES               TO  PI-SC-CERT-NO                  CL*13
00736         GO TO 0400-MAIN-LOGIC.                                       CL*13
00737                                                                      CL*13
00738  0028-PROCESS-OPTION-2-CONT.                                         CL*13
00739      MOVE +22                    TO  PI-KEY-LENGTH                   CL*13
00740                                      WS-KEY-LENGTH.                  CL*13
00741                                                                      CL*13
00742      IF  ACERTNOL GREATER ZERO  AND                                  CL*13
00743          AEDATEL  = ZERO                                             CL*13
00744          MOVE -1                 TO  ACERTNOL                        CL*13
00745          MOVE AL-UABON           TO  ACERTNOA                        CL*13
00746          MOVE ER-8104            TO  EMI-ERROR                       CL*13
00747          PERFORM 9900-ERROR-FORMAT.                                  CL*13
00748                                                                   EL127
00749      IF ACERTNOL GREATER ZERO                                     EL127
00750          MOVE ACERTNOI           TO  PI-SC-CERT-PRIME             EL127
00751                                      WK-SC-CERT                      CL*13
00752          MOVE +32                TO  PI-KEY-LENGTH.               EL127
00753                                                                      CL*13
00754      PERFORM 0029-TEST-CERT-LENGTH THRU 0029-EXIT.                   CL*13
00755                                                                   EL127
00756      IF ACERTSXL GREATER ZERO                                     EL127
00757          MOVE ACERTSXI           TO  PI-SC-CERT-SFX               EL127
00758          MOVE +33                TO  PI-KEY-LENGTH.               EL127
00759                                                                   EL127
00760      IF EMI-FATAL-CTR GREATER ZERO                                EL127
00761          GO TO 0040-MAIN-LOGIC.                                   EL127
00762                                                                   EL127
00763      IF PI-SC-CARRIER  NOT = SPACES  AND                          EL127
00764         PI-SC-GROUP    NOT = SPACES  AND                          EL127
00765         PI-SC-STATE    NOT = SPACES  AND                          EL127
00766         PI-SC-ACCOUNT  NOT = SPACES  AND                          EL127
00767         PI-SC-EFF-DATE NOT = SPACES  AND                          EL127
00768         PI-SC-CERT-NO  NOT = SPACES                               EL127
00769          MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY       EL127
00770          MOVE -1                     TO  ACERTNOL                 EL127
00771          MOVE WS-CERT-MASTER-DSID    TO  PI-DSID                  EL127
00772          MOVE +33                    TO  PI-KEY-LENGTH            EL127
00773          PERFORM 4000-READ-CERT-FILE.                             EL127
00774                                                                      CL*13
00775      IF PI-SC-CARRIER  = SPACES                                      CL*13
00776         MOVE LOW-VALUES              TO  PI-SC-CARRIER.              CL*13
00777      IF PI-SC-GROUP    = SPACES                                      CL*13
00778         MOVE LOW-VALUES              TO  PI-SC-GROUP.                CL*13
00779      IF PI-SC-STATE    = SPACES                                      CL*13
00780         MOVE LOW-VALUES              TO  PI-SC-STATE.                CL*13
00781      IF PI-SC-ACCOUNT  = SPACES                                      CL*13
00782         MOVE LOW-VALUES              TO  PI-SC-ACCOUNT.              CL*13
00783      IF PI-SC-EFF-DATE = SPACES                                      CL*13
00784         MOVE LOW-VALUES              TO  PI-SC-EFF-DATE.             CL*13
00785      IF PI-SC-CERT-NO  = SPACES                                      CL*13
00786         MOVE LOW-VALUES              TO  PI-SC-CERT-NO.              CL*13
00787                                                                      CL*13
00788 *       MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY           CL*13
00789 *       MOVE -1                     TO  ACERTNOL                     CL*13
00790 *       MOVE WS-CERT-MASTER-DSID    TO  PI-DSID                      CL*13
00791 *       PERFORM 3900-READNEXT-CERT-FILE.                             CL*13
00792                                                                   EL127
00793      MOVE SPACES                 TO  PI-ACCOUNT-KEY.              EL127
00794      MOVE PI-COMPANY-CD          TO  PI-AK-COMPANY-CD.            EL127
00795                                                                   EL127
00796      IF PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4'                EL127
00797          MOVE PI-SC-CARRIER      TO  PI-AK-CARRIER.               EL127
00798                                                                   EL127
00799      IF PI-CERT-ACCESS-CONTROL = '1'                              EL127
00800          MOVE PI-SC-GROUP        TO  PI-AK-GROUP.                 EL127
00801                                                                   EL127
00802      IF PI-CERT-ACCESS-CONTROL = SPACES OR '1' OR '2'             EL127
00803          MOVE PI-SC-STATE        TO  PI-AK-STATE.                 EL127
00804                                                                   EL127
00805      MOVE PI-SC-EFF-DATE         TO  PI-AK-EXPIRE-DATE.           EL127
00806                                                                   EL127
00807      MOVE PI-SC-ACCOUNT          TO  PI-AK-ACCOUNT.               EL127
00808                                                                   EL127
00809      EXEC CICS HANDLE CONDITION                                   EL127
00810          NOTFND   (0030-MAIN-LOGIC)                               EL127
00811      END-EXEC.                                                    EL127
00812                                                                   EL127
00813      EXEC CICS STARTBR                                            EL127
00814          DATASET   (WS-ACCOUNT-MASTER-DSID)                       EL127
00815          RIDFLD    (PI-ACCOUNT-KEY)                               EL127
00816          GTEQ                                                     EL127
00817      END-EXEC.                                                    EL127
00818                                                                   EL127
00819      MOVE +1                     TO  WS-READNEXT-SW.              EL127
00820                                                                   EL127
00821  0028-MAIN-LOGIC.                                                 EL127
00822      EXEC CICS READNEXT                                           EL127
00823          DATASET   (WS-ACCOUNT-MASTER-DSID)                       EL127
00824          RIDFLD    (PI-ACCOUNT-KEY)                               EL127
00825          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL*13
00826      END-EXEC.                                                    EL127
00827                                                                   EL127
00828      CONTINUE.                                                       CL*13
00829                                                                   EL127
00830      IF PI-SC-COMPANY-CD = PI-AK-COMPANY-CD  AND                  EL127
00831         PI-SC-CARRIER    = PI-AK-CARRIER     AND                  EL127
00832         PI-SC-GROUP      = PI-AK-GROUP       AND                  EL127
00833         PI-SC-STATE      = PI-AK-STATE       AND                  EL127
00834         PI-SC-ACCOUNT    = PI-AK-ACCOUNT                          EL127
00835          NEXT SENTENCE                                            EL127
00836        ELSE                                                       EL127
00837          GO TO 0030-MAIN-LOGIC.                                   EL127
00838                                                                   EL127
00839      IF PI-SC-EFF-DATE LESS AM-EFFECTIVE-DT                       EL127
00840          GO TO 0030-MAIN-LOGIC.                                   EL127
00841                                                                   EL127
00842      IF PI-SC-EFF-DATE LESS AM-EXPIRATION-DT                      EL127
00843          GO TO 0040-MAIN-LOGIC.                                   EL127
00844                                                                   EL127
00845      GO TO 0028-MAIN-LOGIC.                                       EL127
00846                                                                   EL127
00847  0029-TEST-CERT-LENGTH.                                              CL*13
00848      IF ACERTNOL NOT GREATER ZERO                                    CL*13
00849         GO TO 0029-EXIT.                                             CL*13
00850                                                                      CL*13
00851      IF WK-SC-CERT-1 = LOW-VALUES OR SPACES                          CL*13
00852         MOVE ER-8107             TO  EMI-ERROR                       CL*13
00853         MOVE -1                  TO  ACERTNOL                        CL*13
00854         GO TO 8200-SEND-DATAONLY.                                    CL*13
00855                                                                      CL*13
00856      IF WK-SC-CERT-2     = SPACES OR LOW-VALUES                      CL*13
00857         ADD +1                   TO  WS-KEY-LENGTH                   CL*13
00858         MOVE LOW-VALUES          TO  WK-SC-CERT-3                    CL*13
00859                                      WK-SC-CERT-4                    CL*13
00860                                      WK-SC-CERT-5                    CL*13
00861                                      WK-SC-CERT-6                    CL*13
00862                                      WK-SC-CERT-7                    CL*13
00863                                      WK-SC-CERT-8                    CL*13
00864                                      WK-SC-CERT-9                    CL*13
00865                                      WK-SC-CERT-10                   CL*13
00866      ELSE                                                            CL*13
00867        IF (WK-SC-CERT-3     = SPACES OR LOW-VALUES)                  CL*13
00868           ADD +2                 TO  WS-KEY-LENGTH                   CL*13
00869           MOVE LOW-VALUES        TO  WK-SC-CERT-4                    CL*13
00870                                      WK-SC-CERT-5                    CL*13
00871                                      WK-SC-CERT-6                    CL*13
00872                                      WK-SC-CERT-7                    CL*13
00873                                      WK-SC-CERT-8                    CL*13
00874                                      WK-SC-CERT-9                    CL*13
00875                                      WK-SC-CERT-10                   CL*13
00876        ELSE                                                          CL*13
00877          IF (WK-SC-CERT-4     = SPACES OR LOW-VALUES)                CL*13
00878             ADD +3               TO  WS-KEY-LENGTH                   CL*13
00879             MOVE LOW-VALUES      TO  WK-SC-CERT-5                    CL*13
00880                                      WK-SC-CERT-6                    CL*13
00881                                      WK-SC-CERT-7                    CL*13
00882                                      WK-SC-CERT-8                    CL*13
00883                                      WK-SC-CERT-9                    CL*13
00884                                      WK-SC-CERT-10                   CL*13
00885          ELSE                                                        CL*13
00886            IF (WK-SC-CERT-5     = SPACES OR LOW-VALUES)              CL*13
00887               ADD +4                 TO  WS-KEY-LENGTH               CL*13
00888               MOVE LOW-VALUES        TO  WK-SC-CERT-6                CL*13
00889                                          WK-SC-CERT-7                CL*13
00890                                          WK-SC-CERT-8                CL*13
00891                                          WK-SC-CERT-9                CL*13
00892                                          WK-SC-CERT-10               CL*13
00893            ELSE                                                      CL*13
00894              IF (WK-SC-CERT-6     = SPACES OR LOW-VALUES)            CL*13
00895                 ADD +5               TO  WS-KEY-LENGTH               CL*13
00896                 MOVE LOW-VALUES      TO  WK-SC-CERT-7                CL*13
00897                                          WK-SC-CERT-8                CL*13
00898                                          WK-SC-CERT-9                CL*13
00899                                          WK-SC-CERT-10               CL*13
00900              ELSE                                                    CL*13
00901                IF (WK-SC-CERT-7     = SPACES OR LOW-VALUES)          CL*13
00902                   ADD +6             TO  WS-KEY-LENGTH               CL*13
00903                   MOVE LOW-VALUES    TO  WK-SC-CERT-8                CL*13
00904                                          WK-SC-CERT-9                CL*13
00905                                          WK-SC-CERT-10               CL*13
00906                ELSE                                                  CL*13
00907                  IF (WK-SC-CERT-8     = SPACES OR LOW-VALUES)        CL*13
00908                     ADD +7               TO  WS-KEY-LENGTH           CL*13
00909                     MOVE LOW-VALUES      TO  WK-SC-CERT-9            CL*13
00910                                              WK-SC-CERT-10           CL*13
00911                  ELSE                                                CL*13
00912                    IF (WK-SC-CERT-9     = SPACES OR LOW-VALUES)      CL*13
00913                       ADD +8             TO  WS-KEY-LENGTH           CL*13
00914                       MOVE LOW-VALUES    TO  WK-SC-CERT-10           CL*13
00915                    ELSE                                              CL*13
00916                      IF (WK-SC-CERT-10    = SPACES OR LOW-VALUES)    CL*13
00917                         ADD +9           TO  WS-KEY-LENGTH           CL*13
00918                         MOVE LOW-VALUES  TO  WK-SC-CERT-10           CL*13
00919                      ELSE                                            CL*13
00920                         GO TO 0029-EXIT.                             CL*13
00921                                                                      CL*13
00922      MOVE WK-SC-CERT          TO  PI-SC-CERT-PRIME                   CL*13
00923      MOVE 'Y'                 TO  PART-KEY-ON-SW                     CL*13
00924      MOVE 'C'                 TO  PART-FIELD-ON-SW                   CL*13
00925      GO TO 0400-MAIN-LOGIC.                                          CL*13
00926  0029-EXIT. EXIT.                                                    CL*13
00927                                                                      CL*13
00928  0030-MAIN-LOGIC.                                                 EL127
00929 *    MOVE ER-0198                TO  EMI-ERROR.                      CL*13
00930 *    PERFORM 9900-ERROR-FORMAT.                                      CL*13
00931 *    MOVE -1                     TO  AACCTNOL.                       CL*13
00932                                                                   EL127
00933      IF PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4'                EL127
00934          IF ACARIERL GREATER ZERO                                 EL127
00935              MOVE AL-UABON           TO  ACARIERA                 EL127
00936          ELSE                                                     EL127
00937              MOVE AL-UABOF           TO  ACARIERA.                EL127
00938                                                                   EL127
00939      IF PI-CERT-ACCESS-CONTROL = '1'                              EL127
00940          IF AGROUPL GREATER ZERO                                  EL127
00941              MOVE AL-UABON           TO  AGROUPA                  EL127
00942          ELSE                                                     EL127
00943              MOVE AL-UABOF           TO  AGROUPA.                 EL127
00944                                                                   EL127
00945      IF PI-CERT-ACCESS-CONTROL = SPACES OR '1' OR '2'             EL127
00946          IF ASTATEL GREATER ZERO                                  EL127
00947              MOVE AL-UABON           TO  ASTATEA                  EL127
00948          ELSE                                                     EL127
00949              MOVE AL-UABOF           TO  ASTATEA.                 EL127
00950                                                                   EL127
00951      IF AACCTNOL GREATER ZERO                                     EL127
00952          MOVE AL-UABON           TO  AACCTNOA                     EL127
00953      ELSE                                                         EL127
00954          MOVE AL-UABOF           TO  AACCTNOA.                    EL127
00955                                                                   EL127
00956      IF AEDATEL GREATER ZERO                                      EL127
00957          MOVE AL-UNBON           TO  AEDATEA                      EL127
00958      ELSE                                                         EL127
00959          MOVE AL-UNBOF           TO  AEDATEA.                     EL127
00960                                                                   EL127
00961  0040-MAIN-LOGIC.                                                 EL127
00962      IF EMI-FATAL-CTR GREATER ZERO                                EL127
00963          GO TO 8200-SEND-DATAONLY.                                   CL**8
00964                                                                   EL127
00965      MOVE AM-CONTROL-PRIMARY     TO  PI-ACCOUNT-KEY.              EL127
00966                                                                   EL127
00967      MOVE AM-CARRIER             TO  PI-SC-CARRIER.               EL127
00968      MOVE AM-GROUPING            TO  PI-SC-GROUP.                 EL127
00969      MOVE AM-STATE               TO  PI-SC-STATE.                 EL127
00970                                                                   EL127
00971      MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.          EL127
00972                                                                   EL127
00973      IF PI-KEY-LENGTH NOT GREATER +22                             EL127
00974          PERFORM 4000-READ-CERT-FILE.                             EL127
00975                                                                   EL127
00976      IF WS-READNEXT-SW NOT = ZERO                                    CL*13
00977          EXEC CICS ENDBR                                             CL*13
00978              DATASET (WS-ACCOUNT-MASTER-DSID)                        CL*13
00979          END-EXEC.                                                   CL*13
00980                                                                      CL*13
00981      EXEC CICS HANDLE CONDITION                                   EL127
00982          NOTFND (0050-MAIN-LOGIC)                                 EL127
00983      END-EXEC.                                                    EL127
00984                                                                   EL127
00985      EXEC CICS READ                                               EL127
00986          DATASET (WS-CERT-MASTER-DSID)                            EL127
00987          RIDFLD  (PI-CERTIFICATE-KEY)                             EL127
00988          SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL*13
00989      END-EXEC.                                                    EL127
00990                                                                   EL127
00991      CONTINUE.                                                       CL*13
00992                                                                   EL127
00993      MOVE PI-CK-CARRIER          TO  PI-CARRIER.                  EL127
00994      MOVE PI-CK-GROUPING         TO  PI-GROUPING.                 EL127
00995      MOVE PI-CK-STATE            TO  PI-STATE.                    EL127
00996      MOVE PI-CK-ACCOUNT          TO  PI-ACCOUNT.                  EL127
00997      MOVE PI-CK-CERT-EFF-DT      TO  PI-CERT-EFF-DT.              EL127
00998      MOVE PI-CK-CERT-NO          TO  PI-CERT-NO.                  EL127
00999                                                                   EL127
01000      MOVE 'EL1273'               TO  THIS-PGM.                    EL127
01001      MOVE +1                     TO  PI-1ST-TIME-SW.              EL127
01002      GO TO 9300-XCTL.                                             EL127
01003                                                                   EL127
01004  0050-MAIN-LOGIC.                                                 EL127
01005      MOVE ER-0201                TO  EMI-ERROR.                   EL127
01006      MOVE -1                     TO  ACERTNOL.                    EL127
01007      GO TO 8200-SEND-DATAONLY.                                       CL**8
01008                                                                   EL127
01009      EJECT                                                        EL127
01010  0100-MAIN-LOGIC.                                                 EL127
01011 ******************************************************************EL127
01012 *           O P T I O N  3  P R O C E S S I N G                  *EL127
01013 ******************************************************************EL127
01014                                                                   EL127
01015      IF ALNAMEL  > ZERO  OR
01016         AFNAMEL  > ZERO  OR
01017         AINITALL > ZERO  OR
01018         AACCT2L  > ZERO  OR
01019         ACARRL   > ZERO  OR
110106        ASTL     > ZERO  OR
110106        ASTATUSL > ZERO
01020         CONTINUE
01021      ELSE
01022         GO TO 0200-MAIN-LOGIC
           END-IF

01024      IF (AFNAMEL  > ZERO OR
01025          AINITALL > ZERO OR
01026          AACCT2L  > ZERO OR
01027          ACARRL   > ZERO OR
110106         ASTL     > ZERO OR
110106         ASTATUSL > ZERO)
01028                 AND
01029          ALNAMEL NOT > ZERO
01030          MOVE ER-0488            TO EMI-ERROR
01031          MOVE -1                 TO ALNAMEL
01032          GO TO 8200-SEND-DATAONLY
           END-IF

01034      IF AINITALL > ZERO   AND
01035         AFNAMEL NOT > ZERO
01036         MOVE ER-0764             TO EMI-ERROR
01037         MOVE -1                  TO AFNAMEL
01038         GO TO 8200-SEND-DATAONLY
           END-IF

01040 ************************************************************      EL127
01041 *           SECURITY CHECK FOR ACCOUNT NUMBER              *      EL127
01042 *                      03/29/84                            *      EL127
01043 ************************************************************      EL127
01044                                                                   EL127
01045      IF  PI-NO-ACCOUNT-SECURITY                                   EL127
01046          GO TO 0110-CHECK-EDITS.                                  EL127
01047                                                                   EL127
01048      IF  AACCT2L GREATER ZERO                                     EL127
01049          IF  AACCT2I = PI-ACCOUNT-SECURITY                        EL127
01050             MOVE AL-UANON        TO  AACCT2A                      EL127
01051          ELSE                                                     EL127
01052             MOVE -1              TO  AACCT2L                      EL127
01053             MOVE ER-2371         TO  EMI-ERROR                    EL127
01054             MOVE AL-UABON        TO  AACCT2A                      EL127
01055             GO TO 8200-SEND-DATAONLY.                                CL**8
01056                                                                   EL127
01057  0110-CHECK-EDITS.                                                EL127
01058      MOVE WS-CERT-AIX01-DSID     TO  PI-DSID.                     EL127
01059      MOVE '3'                    TO  PI-OPTION.                   EL127
01060      MOVE PI-COMPANY-CD          TO  PI-SELECTION-CRITERIA.          CL*10
01061                                                                   EL127
01062      MOVE +1                     TO  PI-KEY-LENGTH.               EL127
01063                                                                   EL127
01064      IF ALNAMEL GREATER ZERO                                      EL127
01065          MOVE ALNAMEI            TO  PI-SC-LAST-NAME                 CL*10
01066                                      WS-INPUT-FIELD               EL127
01067          PERFORM 0120-MAIN-LOGIC                                     CL**9
01068             THRU 0120-MAIN-LOGIC-EXIT                                CL**9
01069               VARYING INPUT-INDEX FROM ALNAMEL BY -1                 CL**9
01070                 UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE        CL**9
01071          ADD ALNAMEL  TO  PI-KEY-LENGTH.                          EL127
01072                                                                   EL127
01073      IF AFNAMEL GREATER ZERO                                         CL**9
01074          MOVE +17                TO  PI-KEY-LENGTH                   CL*10
01075          MOVE AFNAMEI            TO  WS-FIRST-NAME                   CL**9
01076          MOVE WS-FIRST-INITIAL   TO  WS-INITIAL-FIRST.               CL**9
01077                                                                      CL**9
01078      IF AFNAMEL GREATER +1                                           CL**9
01079          MOVE AFNAMEI            TO PI-SC-FIRST-NAME.                CL**9
01080                                                                      CL**9
01081      IF AINITALL GREATER ZERO                                     EL127
01082          MOVE +18                TO  PI-KEY-LENGTH                   CL*10
01083          MOVE AINITALI           TO  WS-INITIAL-MIDDLE.              CL**9
01084                                                                      CL**9
01085      IF WS-INITIALS GREATER SPACES                                   CL**9
01086          MOVE WS-INITIALS        TO  PI-SC-INITIALS.                 CL*10
01087                                                                   EL127
01088      IF AACCT2L GREATER ZERO                                      EL127
01089          MOVE AACCT2I            TO  PI-SC-ACCT-NO.               EL127
01090                                                                      CL*12
01091      IF ACARRL GREATER ZERO                                          CL*12
01092          MOVE ACARRI             TO  PI-SC-CARR.

110106     IF ASTL > ZEROS
110106        MOVE ASTI                TO PI-SC-ST
110106     END-IF

110106     IF ASTATUSL > ZEROS
110106        MOVE ASTATUSI            TO PI-SC-STATUS
110106     END-IF

01094      MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY.          EL127
01095                                                                   EL127
01096      MOVE -1                     TO  ALNAMEL.                        CL**3
01097                                                                      CL**3
01098      PERFORM 4000-READ-CERT-FILE.                                 EL127
01099                                                                   EL127
01100  0120-MAIN-LOGIC.                                                 EL127
01101      SUBTRACT +1 FROM ALNAMEL.                                    EL127
01102                                                                   EL127
01103  0120-MAIN-LOGIC-EXIT.                                            EL127
01104      EXIT.                                                        EL127
01105                                                                   EL127
01106      EJECT                                                        EL127
01107  0200-MAIN-LOGIC.                                                 EL127
01108 ******************************************************************EL127
01109 *           O P T I O N  4  P R O C E S S I N G                  *EL127
01110 ******************************************************************EL127
01111      IF ASSNL GREATER ZERO                                        EL127
01112          NEXT SENTENCE                                            EL127
01113      ELSE                                                         EL127
01114          GO TO 0300-MAIN-LOGIC.                                   EL127
01115                                                                   EL127
01116      MOVE '4'                    TO  PI-OPTION.                   EL127
01117                                                                   EL127
01118      MOVE WS-CERT-AIX02-DSID     TO  PI-DSID.                     EL127
01119                                                                   EL127
01120      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD             EL127
01121                                      PI-CK-COMPANY-CD.            EL127
01122      MOVE ASSNI                  TO  PI-CK-SOC-SEC-NO             EL127
01123                                      PI-SC-SOC-SEC-NO             EL127
01124                                      WS-INPUT-FIELD.              EL127
01125                                                                   EL127
01126      PERFORM 0220-MAIN-LOGIC THRU 0220-MAIN-LOGIC-EXIT            EL127
01127          VARYING INPUT-INDEX FROM ASSNL BY -1                     EL127
01128              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.       EL127
01129      ADD +1  ASSNL  GIVING  PI-KEY-LENGTH.                        EL127
01130      MOVE -1                     TO  ASSNL.                       EL127
01131      PERFORM 4000-READ-CERT-FILE.                                 EL127
01132                                                                   EL127
01133  0220-MAIN-LOGIC.                                                 EL127
01134      SUBTRACT +1 FROM ASSNL.                                      EL127
01135                                                                   EL127
01136  0220-MAIN-LOGIC-EXIT.                                            EL127
01137      EXIT.                                                        EL127
01138                                                                   EL127
01139      EJECT                                                        EL127
01140  0300-MAIN-LOGIC.                                                 EL127
01141 ******************************************************************EL127
01142 *           O P T I O N  5  P R O C E S S I N G                  *EL127
01143 ******************************************************************EL127
01144      IF AMEMBERL GREATER ZERO                                     EL127
01145          NEXT SENTENCE                                            EL127
01146      ELSE                                                         EL127
01147          GO TO 0400-MAIN-LOGIC.                                   EL127
01148                                                                   EL127
01149      MOVE '5'                    TO  PI-OPTION.                   EL127
01150                                                                   EL127
01151      MOVE WS-CERT-AIX05-DSID     TO  PI-DSID.                     EL127
01152                                                                   EL127
01153      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD             EL127
01154                                      PI-SC-COMPANY-CD.            EL127
01155      MOVE AMEMBERI               TO  PI-CK-MEMBER-NO              EL127
01156                                      PI-SC-MEMBER-NO              EL127
01157                                      WS-INPUT-FIELD.              EL127
01158                                                                      CL*10
01159      PERFORM 0320-MAIN-LOGIC THRU 0320-MAIN-LOGIC-EXIT            EL127
01160          VARYING INPUT-INDEX FROM AMEMBERL BY -1                  EL127
01161              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.       EL127
01162      ADD +1  AMEMBERL  GIVING  PI-KEY-LENGTH.                     EL127
01163      MOVE -1                     TO  AMEMBERL.                    EL127
01164      PERFORM 4000-READ-CERT-FILE.                                 EL127
01165                                                                   EL127
01166  0320-MAIN-LOGIC.                                                 EL127
01167      SUBTRACT +1 FROM AMEMBERL.                                   EL127
01168                                                                   EL127
01169  0320-MAIN-LOGIC-EXIT.                                            EL127
01170      EXIT.                                                        EL127
01171                                                                   EL127
01172  0400-MAIN-LOGIC.                                                 EL127
01173      MOVE +1                     TO  PI-KEY-LENGTH.               EL127
01174      IF  PART-KEY-ON                                                 CL*13
01175          MOVE PI-SELECTION-CRITERIA  TO  PI-CERTIFICATE-KEY          CL*13
01176          MOVE WS-KEY-LENGTH      TO  PI-KEY-LENGTH.                  CL*13
01177      MOVE WS-CERT-MASTER-DSID    TO  PI-DSID.                     EL127
01178      MOVE ZERO                   TO  PI-OPTION.                   EL127
01179      MOVE -1                     TO  ACRTNO4L.                    EL127
01180      PERFORM 4000-READ-CERT-FILE.                                 EL127
01181                                                                   EL127
01182      EJECT                                                        EL127
01183  3900-READNEXT-CERT-FILE SECTION.                                    CL*13
01184      EXEC CICS HANDLE CONDITION                                      CL*13
01185          DUPKEY (9300-XCTL)                                          CL*13
01186          NOTFND (3980-NOTFND)                                        CL*13
01187          DSIDERR (3970-DSIDERR)                                      CL*13
01188      END-EXEC.                                                       CL*13
01189                                                                      CL*13
01190      EXEC CICS ENDBR                                                 CL*13
01191           DATASET (WS-ACCOUNT-MASTER-DSID)                           CL*13
01192      END-EXEC.                                                       CL*13
01193                                                                      CL*13
01194      EXEC CICS STARTBR                                               CL*13
01195          DATASET   (PI-DSID)                                         CL*13
01196          RIDFLD    (PI-CERTIFICATE-KEY)                              CL*13
01197          KEYLENGTH (PI-KEY-LENGTH)                                   CL*13
01198          GENERIC                                                     CL*13
01199          GTEQ                                                        CL*13
01200      END-EXEC.                                                       CL*13
01201                                                                      CL*13
01202  3900-READNEXT-CONTINUE.                                             CL*13
01203      EXEC CICS READNEXT                                              CL*13
01204          DATASET   (PI-DSID)                                         CL*13
01205          RIDFLD    (PI-CERTIFICATE-KEY)                              CL*13
01206          SET       (ADDRESS OF CERTIFICATE-MASTER)                   CL*13
01207      END-EXEC.                                                       CL*13
01208                                                                      CL*13
01209      GO TO 3999-EXIT.                                                CL*13
01210                                                                      CL*13
01211  3970-DSIDERR.                                                       CL*13
01212      MOVE ER-0671                TO  EMI-ERROR.                      CL*13
01213      MOVE -1                     TO  APFKL.                          CL*13
01214      GO TO 8200-SEND-DATAONLY.                                       CL*13
01215                                                                      CL*13
01216  3980-NOTFND.                                                        CL*13
01217      GO TO 3900-READNEXT-CONTINUE.                                   CL*13
01218                                                                      CL*13
01219  3999-EXIT. EXIT.                                                    CL*13
01220                                                                      CL*13
01221  4000-READ-CERT-FILE SECTION.                                     EL127
01222      EXEC CICS HANDLE CONDITION                                   EL127
01223          DUPKEY (9300-XCTL)                                       EL127
01224          NOTFND (4080-NOTFND)                                     EL127
01225          DSIDERR (4070-DSIDERR)                                      CL**4
01226      END-EXEC.                                                    EL127
01227                                                                      CL*13
01228 **   IF  PART-KEY-ON                                                 CL*13
01229 *        MOVE WS-CERT-MASTER-DSID    TO  PI-DSID                     CL*13
01230 *        PERFORM 3900-READNEXT-CERT-FILE THRU 3999-EXIT              CL*13
01231 *        MOVE 'N'          TO  PART-KEY-ON-SW                        CL*13
01232 *        EXEC CICS ENDBR                                             CL*13
01233 *           DATASET   (PI-DSID)                                      CL*13
01234 *        END-EXEC                                                    CL*13
01235 *        GO TO 4070-CONTINUE.                                        CL*13
01236                                                                   EL127
01237      IF (PI-DSID = WS-CERT-MASTER-DSID AND                        EL127
01238          PI-KEY-LENGTH LESS +33)                                  EL127
01239        OR                                                         EL127
01240         (PI-DSID = WS-CERT-AIX01-DSID AND                         EL127
01241          PI-KEY-LENGTH LESS +18)                                     CL*10
01242        OR                                                         EL127
01243         (PI-DSID = WS-CERT-AIX02-DSID AND                         EL127
01244          PI-KEY-LENGTH LESS +12)                                  EL127
01245        OR                                                         EL127
01246         (PI-DSID = WS-CERT-AIX04-DSID AND                         EL127
01247          PI-KEY-LENGTH LESS +12)                                  EL127
01248        OR                                                         EL127
01249         (PI-DSID = WS-CERT-AIX05-DSID AND                         EL127
01250          PI-KEY-LENGTH LESS +13)                                  EL127
01251              MOVE +1             TO  PI-START-SW                  EL127
01252              EXEC CICS READ                                       EL127
01253                  DATASET   (PI-DSID)                              EL127
01254                  RIDFLD    (PI-CERTIFICATE-KEY)                   EL127
01255                  SET       (ADDRESS OF CERTIFICATE-MASTER)           CL*13
01256                  GENERIC                                          EL127
01257                  EQUAL                                            EL127
01258                  KEYLENGTH (PI-KEY-LENGTH)                        EL127
01259              END-EXEC                                             EL127
01260          ELSE                                                     EL127
01261              MOVE ZERO           TO  PI-START-SW                  EL127
01262              EXEC CICS READ                                       EL127
01263                  DATASET   (PI-DSID)                              EL127
01264                  RIDFLD    (PI-CERTIFICATE-KEY)                   EL127
01265                  SET       (ADDRESS OF CERTIFICATE-MASTER)           CL*13
01266              END-EXEC.                                            EL127
01267                                                                   EL127
01268  4070-CONTINUE.                                                      CL*13
01269                                                                   EL127
01270      GO TO 9300-XCTL.                                             EL127
01271                                                                      CL**4
01272  4070-DSIDERR.                                                       CL**4
01273      MOVE ER-0671                TO  EMI-ERROR.                      CL**4
01274      MOVE -1                     TO  APFKL.                          CL*13
01275      GO TO 8200-SEND-DATAONLY.                                       CL**8
01276                                                                   EL127
01277  4080-NOTFND.                                                     EL127
01278 *    MOVE -1                     TO  ACRTNO4L.                    EL127
01279      MOVE -1                     TO  APFKL.                          CL*13
01280      MOVE ER-0201                TO  EMI-ERROR.                   EL127
01281      GO TO 8200-SEND-DATAONLY.                                       CL**8
01282                                                                   EL127
01283  4090-EXIT.                                                       EL127
01284      EXIT.                                                        EL127
01285      EJECT                                                        EL127
01286                                                                   EL127
01287  5000-NEXT-COMPANY SECTION.                                       EL127
01288 ******************************************************************   CL**8
01289 ****      READ CURRENT COMPANY RECORD TO OBTAIN THE NEXT      ****   CL**8
01290 ****      COMPANY ID.                                         ****   CL**8
01291 ******************************************************************   CL**8
01292                                                                   EL127
01293      MOVE PI-COMPANY-ID              TO  WS-CNTL-ID.                 CL**8
01294      MOVE '1'                        TO  WS-CNTL-TYPE.               CL**8
01295      MOVE SPACES                     TO  WS-CNTL-USER.               CL**8
01296      MOVE +0                         TO  WS-CNTL-SEQ.                CL**8
01297                                                                   EL127
01298      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**8
01299                                                                   EL127
01300      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**8
01301          MOVE ER-0022                TO  EMI-ERROR                   CL**8
01302          MOVE -1                     TO  APFKL                       CL**8
01303          GO TO 8200-SEND-DATAONLY.                                   CL**8
01304                                                                   EL127
01305      IF EIBAID = DFHPF6                                           EL127
01306          MOVE CF-NEXT-COMPANY-ID     TO  WS-NEXT-COMPANY-ID.         CL**8
01307                                                                   EL127
01308      IF EIBAID = DFHPF7                                           EL127
01309          MOVE PI-ORIGINAL-COMPANY-ID TO  WS-NEXT-COMPANY-ID.         CL**8
01310                                                                   EL127
01311      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                           CL**8
01312          GO TO 5000-CONTINUE-NEXT-COMPANY.                           CL**8
01313                                                                   EL127
01314 ******************************************************************   CL**8
01315 ****      READ CURRENT USER RECORD FOR UPDATE AND REMOVE      ****   CL**8
01316 ****      THE TERMINAL ID FROM THE RECORD.                    ****   CL**8
01317 ******************************************************************   CL**8
01318                                                                   EL127
01319      MOVE PI-COMPANY-ID              TO  WS-CNTL-ID.                 CL**8
01320      MOVE '2'                        TO  WS-CNTL-TYPE.               CL**8
01321      MOVE PI-PROCESSOR-ID            TO  WS-CNTL-USER.               CL**8
01322      MOVE +0                         TO  WS-CNTL-SEQ.                CL**8
01323                                                                   EL127
01324      PERFORM 6010-READ-CONTROL-UPDATE THRU 6010-EXIT.                CL**8
01325                                                                   EL127
01326      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**8
01327          MOVE ER-0019                TO  EMI-ERROR                   CL**8
01328          MOVE -1                     TO  APFKL                       CL**8
01329          GO TO 8200-SEND-DATAONLY.                                   CL**8
01330                                                                      CL**8
01331      MOVE SPACES                     TO  CF-CURRENT-TERM-ON.         CL**8
01332                                                                      CL**8
01333      PERFORM 6020-REWRITE-CONTROL THRU 6020-EXIT.                    CL**8
01334                                                                      CL**8
01335 ******************************************************************   CL**8
01336 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****   CL**8
01337 ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****   CL**8
01338 ****        1.  MOVE USER ACCOUNT/CARRIER SECURITY TO PI-AREA ****   CL**8
01339 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CONTROL ****   CL**8
01340 ****            IN WORKING STORAGE                            ****   CL**8
01341 ******************************************************************   CL**8
01342                                                                      CL**8
01343      MOVE WS-NEXT-COMPANY-ID         TO  WS-CNTL-ID.                 CL**8
01344      MOVE '2'                        TO  WS-CNTL-TYPE.               CL**8
01345      MOVE PI-PROCESSOR-ID            TO  WS-CNTL-USER.               CL**8
01346      MOVE +0                         TO  WS-CNTL-SEQ.                CL**8
01347                                                                      CL**8
01348      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**8
01349                                                                      CL**8
01350      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**8
01351          MOVE ER-0228                TO  EMI-ERROR                   CL**8
01352          MOVE -1                     TO  APFKL                       CL**8
01353          GO TO 8200-SEND-DATAONLY.                                   CL**8
01354                                                                      CL**8
01355      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.        CL**8
01356      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.        CL**8
01357      MOVE CF-INDIVIDUAL-APP (1)      TO  SC-CREDIT-CODES.            CL**8
01358      MOVE CF-INDIVIDUAL-APP (2)      TO  SC-CLAIMS-CODES.            CL**8
01359                                                                      CL**8
01360 ******************************************************************   CL**8
01361 ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****   CL**8
01362 ****      UPDATE AND MOVE THE TERMINAL ID TO THE RECORD.      ****   CL**8
01363 ******************************************************************   CL**8
01364                                                                      CL**8
01365      MOVE WS-NEXT-COMPANY-ID         TO  WS-CNTL-ID.                 CL**8
01366      MOVE '2'                        TO  WS-CNTL-TYPE.               CL**8
01367      MOVE PI-PROCESSOR-ID            TO  WS-CNTL-USER.               CL**8
01368      MOVE +0                         TO  WS-CNTL-SEQ.                CL**8
01369                                                                      CL**8
01370      PERFORM 6010-READ-CONTROL-UPDATE THRU 6010-EXIT.                CL**8
01371                                                                      CL**8
01372      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**8
01373          MOVE ER-0228                TO  EMI-ERROR                   CL**8
01374          MOVE -1                     TO  APFKL                       CL**8
01375          GO TO 8200-SEND-DATAONLY.                                   CL**8
01376                                                                      CL**8
01377      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.         CL**8
01378                                                                      CL**8
01379      PERFORM 6020-REWRITE-CONTROL THRU 6020-EXIT.                    CL**8
01380                                                                      CL**8
01381  5000-CONTINUE-NEXT-COMPANY.                                         CL**8
01382 ******************************************************************   CL**8
01383 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****   CL**8
01384 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO THE PI-AREA.  ****   CL**8
01385 ******************************************************************   CL**8
01386                                                                      CL**8
01387      MOVE WS-NEXT-COMPANY-ID         TO  WS-CNTL-ID.                 CL**8
01388      MOVE '1'                        TO  WS-CNTL-TYPE.               CL**8
01389      MOVE SPACES                     TO  WS-CNTL-USER.               CL**8
01390      MOVE +0                         TO  WS-CNTL-SEQ.                CL**8
01391                                                                      CL**8
01392      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**8
01393                                                                      CL**8
01394      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**8
01395          MOVE ER-0089                TO  EMI-ERROR                   CL**8
01396          MOVE -1                     TO  APFKL                       CL**8
01397          GO TO 8200-SEND-DATAONLY.                                   CL**8
01398                                                                   EL127
01399      MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD.           EL127
01400      MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.           EL127
01401      MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.        CL**8
01402      MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.          EL127
01403      MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.           EL127
01404      MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.  EL127
01405      MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.EL127
01406      MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.      EL127
01407      MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.      CL**8
01408      MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.      CL**8
01409                                                                   EL127
01410      MOVE CF-LIFE-OVERRIDE-L1        TO  PI-LIFE-OVERRIDE-L1.        CL**5
01411      MOVE CF-LIFE-OVERRIDE-L2        TO  PI-LIFE-OVERRIDE-L2.        CL**5
01412      MOVE CF-LIFE-OVERRIDE-L6        TO  PI-LIFE-OVERRIDE-L6.        CL**5
01413      MOVE CF-LIFE-OVERRIDE-L12       TO  PI-LIFE-OVERRIDE-L12.       CL**5
01414                                                                   EL127
01415      MOVE CF-AH-OVERRIDE-L1          TO  PI-AH-OVERRIDE-L1.          CL**5
01416      MOVE CF-AH-OVERRIDE-L2          TO  PI-AH-OVERRIDE-L2.          CL**5
01417      MOVE CF-AH-OVERRIDE-L6          TO  PI-AH-OVERRIDE-L6.          CL**5
01418      MOVE CF-AH-OVERRIDE-L12         TO  PI-AH-OVERRIDE-L12.         CL**5
01419                                                                   EL127
01420      IF CREDIT-SESSION                                            EL127
01421          MOVE CF-CURRENT-MONTH-END   TO  PI-CR-MONTH-END-DT       EL127
01422          MOVE CF-CAR-GROUP-ACCESS-CNTL                               CL*11
01423                                      TO  PI-CAR-GROUP-ACCESS-CNTL    CL*11
01424          MOVE CF-CR-PRINT-ADDRESS-LABELS                             CL*11
01425                                      TO  PI-LABEL-CONTROL            CL*11
01426                                                                      CL*11
01427      ELSE                                                            CL*11
01428          MOVE CF-PRINT-ADDRESS-LABELS                                CL*11
01429                                      TO  PI-LABEL-CONTROL.           CL*11
01430                                                                   EL127
01431  5000-EXIT.                                                       EL127
01432      EXIT.                                                        EL127
01433      EJECT                                                        EL127
01434  5500-WRITE-SECURITY-TEMP-STORE  SECTION.                         EL127
01435      EXEC CICS HANDLE CONDITION                                   EL127
01436          QIDERR   (5501-WRITE-SECURITY)                           EL127
01437      END-EXEC.                                                    EL127
01438                                                                   EL127
01439      MOVE EIBTRMID               TO  QID.                         EL127
01440                                                                   EL127
01441      EXEC CICS DELETEQ TS                                         EL127
01442          QUEUE   (QID)                                            EL127
01443      END-EXEC.                                                    EL127
01444                                                                   EL127
01445  5501-WRITE-SECURITY.                                             EL127
01446                                                                   EL127
01447      EXEC CICS WRITEQ TS                                          EL127
01448          QUEUE   (QID)                                            EL127
01449          FROM    (SECURITY-CONTROL)                               EL127
01450          LENGTH  (SC-COMM-LENGTH)                                 EL127
01451          ITEM    (QID-ITEM)                                       EL127
01452      END-EXEC.                                                    EL127
01453                                                                   EL127
01454      MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.   EL127
01455                                                                   EL127
01456      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL127
01457          MOVE ALL 'Y'            TO  SC-CREDIT-CODES              EL127
01458                                      SC-CLAIMS-CODES              EL127
01459                                      PI-PROCESSOR-USER-ALMIGHTY.  EL127
01460                                                                   EL127
01461  5500-EXIT.                                                       EL127
01462      EXIT.                                                        EL127
01463                                                                   EL127
01464  EJECT                                                            EL127
01465  6000-READ-CONTROL SECTION.                                       EL127
01466      EXEC CICS HANDLE CONDITION                                   EL127
01467           NOTFND (6000-NOT-FOUND)                                 EL127
01468      END-EXEC.                                                    EL127
01469                                                                   EL127
01470      EXEC CICS READ                                               EL127
01471           DATASET  (WS-CONTROL-FILE-DSID)                            CL**8
01472           RIDFLD   (WS-CNTL-KEY)                                  EL127
01473           SET      (ADDRESS OF CONTROL-FILE)                         CL*13
01474      END-EXEC.                                                    EL127
01475                                                                   EL127
01476      CONTINUE.                                                       CL*13
01477      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.           CL**8
01478      GO TO 6000-EXIT.                                             EL127
01479                                                                   EL127
01480  6000-NOT-FOUND.                                                  EL127
01481                                                                      CL**8
01482      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.           CL**8
01483                                                                   EL127
01484  6000-EXIT.                                                       EL127
01485      EXIT.                                                           CL**8
01486                                                                      CL**8
01487  6010-READ-CONTROL-UPDATE.                                           CL**8
01488                                                                      CL**8
01489       EXEC CICS HANDLE CONDITION                                     CL**8
01490           NOTFND    (6010-NOTFND)                                    CL**8
01491       END-EXEC.                                                      CL**8
01492                                                                      CL**8
01493       EXEC CICS READ                                                 CL**8
01494           DATASET   (WS-CONTROL-FILE-DSID)                           CL**8
01495           RIDFLD    (WS-CNTL-KEY)                                    CL**8
01496           SET       (ADDRESS OF CONTROL-FILE)                        CL*13
01497           UPDATE                                                     CL**8
01498       END-EXEC.                                                      CL**8
01499                                                                      CL**8
01500       CONTINUE.                                                      CL*13
01501                                                                      CL**8
01502       MOVE 'Y'                       TO  WS-CNTL-REC-FOUND-SW.       CL**8
01503       GO TO 6010-EXIT.                                               CL**8
01504                                                                      CL**8
01505  6010-NOTFND.                                                        CL**8
01506                                                                      CL**8
01507       MOVE 'N'                       TO  WS-CNTL-REC-FOUND-SW.       CL**8
01508                                                                      CL**8
01509  6010-EXIT.                                                          CL**8
01510      EXIT.                                                           CL**8
01511                                                                      CL**8
01512  6020-REWRITE-CONTROL.                                               CL**8
01513                                                                      CL**8
01514      EXEC CICS REWRITE                                               CL**8
01515          DATASET   (WS-CONTROL-FILE-DSID)                            CL**8
01516          FROM      (CONTROL-FILE)                                    CL**8
01517      END-EXEC.                                                       CL**8
01518                                                                      CL**8
01519  6020-EXIT.                                                          CL**8
01520      EXIT.                                                        EL127
01521                                                                   EL127
01522      EJECT                                                        EL127
01523  7000-BUILD-SCREEN     SECTION.                                   EL127
01524 ******************************************************************EL127
01525 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1272    *EL127
01526 *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *EL127
01527 ******************************************************************EL127
01528      IF EIBTRNID = WS-TRANS-ID                                    EL127
01529         NEXT SENTENCE                                             EL127
01530        ELSE                                                       EL127
01531         GO TO 7099-EXIT.                                          EL127
01532                                                                   EL127
01533 ***                                                                  CL*10
01534 ******** PI-BROWSE-SW = 9  MEANS NO RECORDS FOUND (SET IN EL1272)    CL*10
01535 ***                                                                  CL*10
01536      IF  PI-BROWSE-SW = +9                                        EL127
01537          NEXT SENTENCE                                            EL127
01538         ELSE                                                      EL127
01539          GO TO 7099-EXIT.                                         EL127
01540                                                                   EL127
01541       IF OPTION-TWO-SELECTED                                      EL127
01542          GO TO 7099-EXIT.                                         EL127
01543                                                                   EL127
01544      MOVE LOW-VALUES             TO  EL127AO.                     EL127
01545                                                                   EL127
01546      MOVE ER-2373                TO  EMI-ERROR.                   EL127
01547      PERFORM 9900-ERROR-FORMAT.                                      CL*10
01548                                                                      CL*10
01549      IF PI-ALT-NAME-COUNT GREATER 140                                CL*10
01550          MOVE ER-0765            TO  EMI-ERROR.                      CL*10
01551                                                                   EL127
01552  7010-OPTION-ONE.                                                 EL127
01553       IF OPTION-ONE-SELECTED                                      EL127
01554          NEXT SENTENCE                                            EL127
01555         ELSE                                                      EL127
01556          GO TO 7030-OPTION-THREE.                                 EL127
01557                                                                   EL127
01558       MOVE -1                       TO  ACRTNO4L.                 EL127
01559                                                                   EL127
01560       IF  PI-SC-CERT-PRIME-A4 GREATER SPACES                      EL127
01561           MOVE PI-SC-CERT-PRIME-A4  TO  ACRTNO4O                  EL127
01562           MOVE AL-UANON             TO  ACRTNO4A.                 EL127
01563                                                                   EL127
01564       IF  PI-SC-CERT-SFX-A4   GREATER SPACES                      EL127
01565           MOVE PI-SC-CERT-SFX-A4    TO  ACRTSX4O                  EL127
01566           MOVE AL-UANON             TO  ACRTSX4A.                 EL127
01567                                                                   EL127
01568      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL127
01569                                                                   EL127
01570  7030-OPTION-THREE.                                               EL127
01571      IF OPTION-THREE-SELECTED                                     EL127
01572          NEXT SENTENCE                                            EL127
01573        ELSE                                                       EL127
01574         GO TO 7040-OPTION-FOUR.                                   EL127
01575                                                                   EL127
01576      MOVE -1                          TO  ALNAMEL.                EL127
01577                                                                   EL127
01578      IF  PI-SC-LAST-NAME GREATER SPACES                           EL127
01579          MOVE PI-SC-LAST-NAME         TO  ALNAMEO                 EL127
01580          MOVE AL-UANON                TO  ALNAMEA.                EL127
01581      IF  PI-SC-FIRST-NAME GREATER SPACES                             CL**9
01582          MOVE PI-SC-FIRST-NAME        TO  AFNAMEO                    CL**9
01583          MOVE AL-UANON                TO  AFNAMEA.                   CL**9
01584      IF  PI-SC-INITIALS  GREATER SPACES                           EL127
01585          MOVE PI-SC-INITIALS          TO  WS-INITIALS                CL**9
01586          IF  WS-INITIAL-MIDDLE GREATER SPACES                        CL**9
01587              MOVE WS-INITIAL-MIDDLE   TO  AINITALO                   CL**9
01588              MOVE AL-UANON            TO  AINITALA.                  CL**9
01589      IF  PI-SC-ACCT-NO   GREATER SPACES                           EL127
01590          MOVE PI-SC-ACCT-NO           TO  AACCT2O                 EL127
01591          MOVE AL-UANON                TO  AACCT2A.                EL127

110106     IF PI-SC-ST > SPACES
              MOVE PI-SC-ST            TO ASTO
              MOVE AL-UANON            TO ASTA
           END-IF

110106     IF PI-SC-STATUS > SPACES
              MOVE PI-SC-STATUS        TO ASTATUSO
              MOVE AL-UANON            TO ASTATUSA
           END-IF

01593      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL127
01594                                                                   EL127
01595  7040-OPTION-FOUR.                                                EL127
01596      IF OPTION-FOUR-SELECTED                                      EL127
01597          NEXT SENTENCE                                            EL127
01598      ELSE                                                         EL127
01599          GO TO 7050-OPTION-FIVE.                                  EL127
01600                                                                   EL127
01601      MOVE -1                          TO  ASSNL.                  EL127
01602                                                                   EL127
01603      IF PI-SC-SOC-SEC-NO GREATER SPACES                           EL127
01604         MOVE PI-SC-SOC-SEC-NO         TO  ASSNO                   EL127
01605         MOVE AL-UANON                 TO  ASSNA.                  EL127
01606                                                                   EL127
01607      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL127
01608                                                                   EL127
01609  7050-OPTION-FIVE.                                                EL127
01610      MOVE -1                     TO  AMEMBERL.                    EL127
01611                                                                   EL127
01612      IF PI-SC-MEMBER-NO GREATER SPACES                            EL127
01613         MOVE PI-SC-MEMBER-NO     TO  AMEMBERO                     EL127
01614         MOVE AL-UANON            TO  AMEMBERA.                    EL127
01615                                                                   EL127
01616                                                                   EL127
01617  7090-INITIALIZE-WORK-AREAS.                                      EL127
01618      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL127
01619                                      PI-CONTROL-IN-PROGRESS.      EL127
01620                                                                   EL127
01621      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL127
01622                                      PI-LINE-COUNT                EL127
01623                                      PI-BROWSE-SW                 EL127
01624                                      PI-KEY-LENGTH                EL127
01625                                      PI-TS-ITEM                   EL127
01626                                      PI-END-OF-FILE               EL127
01627                                      PI-START-SW                  EL127
01628                                      PI-AIX-RECORD-COUNT.         EL127
01629                                                                   EL127
01630      GO TO 8200-SEND-DATAONLY.                                       CL**8
01631                                                                   EL127
01632  7099-EXIT.                                                       EL127
01633      EXIT.                                                        EL127
01634      EJECT                                                        EL127
01635  8100-SEND-INITIAL-MAP SECTION.                                   EL127
01636                                                                      CL**8
01637      MOVE PI-COMPANY-ID          TO  WS-CNTL-ID.                     CL**8
01638      MOVE '1'                    TO  WS-CNTL-TYPE.                   CL**8
01639      MOVE SPACES                 TO  WS-CNTL-USER.                   CL**8
01640      MOVE +0                     TO  WS-CNTL-SEQ.                    CL**8
01641                                                                      CL**8
01642      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**8
01643                                                                      CL**8
01644      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**8
01645          MOVE ER-0022            TO  EMI-ERROR                       CL**8
01646          MOVE -1                 TO  APFKL                           CL**8
01647          GO TO 8200-SEND-DATAONLY.                                   CL**8
01648                                                                   EL127
01649      IF SOC-SEC-NO-USED                                           EL127
01650         MOVE AL-SANOF            TO  AOPT4A ASSOPTA               EL127
01651         MOVE AL-UANOF            TO  ASSNA                        EL127
01652         MOVE 'SOCIAL SECURITY NUMBER ' TO  ASSOPTO                EL127
01653         MOVE '** OPTION 4 **'    TO  AOPT4O                       EL127
01654      ELSE                                                         EL127
01655         MOVE AL-SANOF            TO  AOPT4A ASSOPTA ASSNA.        EL127
01656                                                                   EL127
01657      IF MEMBER-NO-USED                                            EL127
01658         MOVE AL-SANOF            TO  AOPT5A AMEOPTA               EL127
01659         MOVE AL-UANOF            TO  AMEMBERA                     EL127
01660         MOVE '** OPTION 5 **'    TO  AOPT5O                       EL127
01661         IF CF-MEMBER-CAPTION = SPACES                             EL127
01662            MOVE 'MEMBER NUMBER ' TO  AMEOPTO                      EL127
01663         ELSE                                                      EL127
01664            MOVE CF-MEMBER-CAPTION TO AMEOPTO                      EL127
01665      ELSE                                                         EL127
01666         MOVE AL-SANOF            TO  AOPT5A AMEOPTA AMEMBERA.     EL127
01667                                                                   EL127
01668      MOVE -1                     TO  ACRTNO4L                     EL127
01669                                                                   EL127
01670      MOVE SAVE-DATE              TO  ADATEO.                      EL127
01671      MOVE EIBTIME                TO  TIME-IN.                     EL127
01672      MOVE TIME-OUT               TO  ATIMEO.                      EL127
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01673                                                                   EL127
01674      IF EMI-ERROR NOT = ZERO                                      EL127
01675          PERFORM 9900-ERROR-FORMAT.                                  CL*10
01676                                                                   EL127
01677      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL127
01678      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    EL127
01679                                                                   EL127
01680      IF PI-NOT-CRDTCRD-USER                                          CL**5
01681          MOVE SPACES              TO APFK9O.                         CL**5
01682                                                                      CL**5
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL127
101501*       MOVE PI-COMPANY-ID       TO  ACOMPO                       EL127
101501*       MOVE AL-PANOF            TO  APFK6A                       EL127
101501*                                    APFK7A                       EL1CL**5
101501*    ELSE                                                         EL127
101501*       MOVE SPACES              TO  ACOMPO                       EL127
101501*       MOVE AL-PADOF            TO  APFK6A                       EL127
101501*                                    APFK7A.                      EL1CL**5
01691                                                                   EL127
01692      EXEC CICS SEND                                               EL127
01693          FROM   (EL127AO)                                         EL127
01694          MAPSET (WS-MAPSET-NAME)                                  EL127
01695          MAP    (WS-MAP-NAME)                                     EL127
01696          CURSOR                                                   EL127
01697          ERASE                                                    EL127
01698      END-EXEC.                                                    EL127
01699                                                                   EL127
01700      GO TO 9100-RETURN-TRAN.                                         CL**8
01701                                                                      CL**8
01702  8100-EXIT.                                                       EL127
01703      EXIT.                                                        EL127
01704                                                                   EL127
01705      EJECT                                                        EL127
01706  8200-SEND-DATAONLY SECTION.                                      EL127
01707      MOVE SAVE-DATE              TO  ADATEO.                      EL127
01708      MOVE EIBTIME                TO  TIME-IN.                     EL127
01709      MOVE TIME-OUT               TO  ATIMEO.                      EL127
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01710                                                                   EL127
01711      IF EMI-ERROR NOT = ZERO                                      EL127
01712          PERFORM 9900-ERROR-FORMAT.                               EL127
01713                                                                   EL127
01714      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL127
01715      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    EL127
01716                                                                   EL127
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL127
101501*       MOVE PI-COMPANY-ID       TO  ACOMPO                       EL127
101501*       MOVE AL-PANOF            TO  APFK6A                       EL127
101501*                                    APFK7A                          CL**5
101501*    ELSE                            APFK7A                       EL127**5
101501*       MOVE SPACES              TO  ACOMPO                       EL127**5
101501*       MOVE AL-PADOF            TO  APFK6A                       EL127**5
101501*                                    APFK7A.                         CL**5
01725                                                                   EL127
01726      EXEC CICS SEND DATAONLY                                      EL127
01727          FROM   (EL127AO)                                         EL127
01728          MAPSET (WS-MAPSET-NAME)                                  EL127
01729          MAP    (WS-MAP-NAME)                                     EL127
01730          CURSOR                                                   EL127
01731      END-EXEC.                                                    EL127
01732                                                                      CL**8
01733      GO TO 9100-RETURN-TRAN.                                         CL**8
01734                                                                   EL127
01735  8200-EXIT.                                                       EL127
01736      EXIT.                                                        EL127
01737                                                                   EL127
01738      EJECT                                                        EL127
01739  8300-SEND-TEXT SECTION.                                          EL127
01740      EXEC CICS SEND TEXT                                          EL127
01741          FROM   (LOGOFF-TEXT)                                     EL127
01742          LENGTH (LOGOFF-LENGTH)                                   EL127
01743          ERASE                                                    EL127
01744          FREEKB                                                   EL127
01745      END-EXEC.                                                    EL127
01746                                                                   EL127
01747      EXEC CICS RETURN                                             EL127
01748      END-EXEC.                                                    EL127
01749                                                                   EL127
01750  8300-EXIT.                                                       EL127
01751      EXIT.                                                        EL127
01752                                                                   EL127
01753      EJECT                                                        EL127
01754  8500-DATE-CONVERSION SECTION.                                    EL127
01755      EXEC CICS LINK                                               EL127
01756          PROGRAM  ('ELDATCV')                                     EL127
01757          COMMAREA (DATE-CONVERSION-DATA)                          EL127
01758          LENGTH   (DC-COMM-LENGTH)                                EL127
01759      END-EXEC.                                                    EL127
01760                                                                   EL127
01761  8500-EXIT.                                                       EL127
01762      EXIT.                                                        EL127
01763                                                                   EL127
01764  8600-DEEDIT SECTION.                                             EL127
01765      EXEC CICS BIF DEEDIT                                         EL127
01766          FIELD  (WS-DEEDIT-FIELD)                                 EL127
01767          LENGTH (15)                                              EL127
01768      END-EXEC.                                                    EL127
01769                                                                   EL127
01770  8600-EXIT.                                                       EL127
01771      EXIT.                                                        EL127
01772                                                                   EL127
01773      EJECT                                                        EL127
01774  9000-RETURN-CICS SECTION.                                        EL127
01775      MOVE 'EL005'                TO  THIS-PGM.                    EL127
01776      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL127
01777      GO TO 9300-XCTL.                                             EL127
01778                                                                   EL127
01779  9000-EXIT.                                                       EL127
01780      EXIT.                                                        EL127
01781                                                                   EL127
01782  9100-RETURN-TRAN SECTION.                                        EL127
01783      MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO.             EL127
01784      MOVE WS-MAP-NUMBER         TO  PI-CURRENT-SCREEN-NO.         EL127
01785                                                                   EL127
01786      EXEC CICS RETURN                                             EL127
01787          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL127
01788          LENGTH   (PI-COMM-LENGTH)                                EL127
01789          TRANSID  (WS-TRANS-ID)                                   EL127
01790      END-EXEC.                                                    EL127
01791                                                                   EL127
01792  9100-EXIT.                                                       EL127
01793      EXIT.                                                        EL127
01794                                                                   EL127
01795  9300-XCTL SECTION.                                               EL127
01796      MOVE DFHENTER               TO  EIBAID.                      EL127
01797      MOVE PART-KEY-ON-SW         TO  PI-PART-KEY-SW.                 CL*13
01798      MOVE PART-FIELD-ON-SW       TO  PI-PART-FIELD-SW.               CL*13
01799      MOVE ' '                    TO  PART-FIELD-ON-SW.               CL*13
01800                                                                   EL127
01801      EXEC CICS XCTL                                               EL127
01802          PROGRAM  (THIS-PGM)                                      EL127
01803          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL127
01804          LENGTH   (PI-COMM-LENGTH)                                EL127
01805      END-EXEC.                                                    EL127
01806                                                                   EL127
01807  9300-EXIT.                                                       EL127
01808      EXIT.                                                        EL127
01809                                                                   EL127
01810      EJECT                                                        EL127
01811  9400-CLEAR SECTION.                                              EL127
01812      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL127
01813      GO TO 9300-XCTL.                                             EL127
01814                                                                   EL127
01815  9400-EXIT.                                                       EL127
01816      EXIT.                                                        EL127
01817                                                                   EL127
01818  9600-PGMIDERR SECTION.                                           EL127
01819      EXEC CICS HANDLE CONDITION                                   EL127
01820          PGMIDERR (8300-SEND-TEXT)                                EL127
01821      END-EXEC.                                                    EL127
01822                                                                   EL127
01823      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL127
01824                                      LOGOFF-PGM.                  EL127
01825                                                                   EL127
01826      MOVE 'EL005'                TO  THIS-PGM.                    EL127
01827      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL127
01828      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL127
01829      GO TO 9300-XCTL.                                             EL127
01830                                                                   EL127
01831  9600-EXIT.                                                       EL127
01832      EXIT.                                                        EL127
01833                                                                   EL127
01834      EJECT                                                        EL127
01835  9900-ERROR-FORMAT SECTION.                                       EL127
01836      EXEC CICS LINK                                               EL127
01837          PROGRAM  ('EL001')                                       EL127
01838          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL127
01839          LENGTH   (EMI-COMM-LENGTH)                               EL127
01840      END-EXEC.                                                    EL127
01841                                                                   EL127
01842  9900-EXIT.                                                       EL127
01843      EXIT.                                                        EL127
01844                                                                   EL127
01845  9990-ERROR SECTION.                                              EL127
01846      MOVE DFHEIBLK               TO EMI-LINE1.                    EL127
01847      EXEC CICS LINK                                               EL127
01848          PROGRAM  ('EL004')                                       EL127
01849          COMMAREA (EMI-LINE1)                                     EL127
01850          LENGTH   (72)                                            EL127
01851      END-EXEC.                                                    EL127
01852                                                                   EL127
01853      GO TO 8200-SEND-DATAONLY.                                       CL**8
01854                                                                   EL127
01855  9990-EXIT.                                                       EL127
01856      EXIT.                                                        EL127
01857                                                                   EL127
01858  9995-SECURITY-VIOLATION.                                         EL127
01859                              COPY ELCSCTP.                        EL127
01860                                                                   EL127
01861  9995-EXIT.                                                       EL127
01862      EXIT.                                                        EL127
01863                                                                   EL127
01864  9999-LAST-PARAGRAPH SECTION.                                     EL127
01865      GOBACK.                                                      EL127
