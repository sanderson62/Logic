00001  ID DIVISION.                                                     06/26/96
00002                                                                   EL694
00003  PROGRAM-ID.                 EL694.                                  LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 10:02:23.                    CL**3
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL694
00008 *                                                                 EL694
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL694
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL694
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL694
00024 *REMARKS.    TRANSACTION - EXM4 - LETTER PRINTING.                EL694
00025 *        THIS FUNCTION IS USED TO START THE PRINTING OF STORED    EL694
00026 *        LETTERS AND/OR ADDRESS LABELS.  MAY ALSO BE USED         EL694
00027 *        TO OBTAIN COUNTS OF OUTSTANDING LETTERS.                 EL694

031011******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
031011******************************************************************

00030  ENVIRONMENT DIVISION.                                            EL694
00031  DATA DIVISION.                                                   EL694
00032  WORKING-STORAGE SECTION.                                         EL694
00033  77  FILLER  PIC  X(32) VALUE '********************************'. EL694
00034  77  FILLER  PIC  X(32) VALUE '*    EL694 WORKING STORAGE     *'. EL694
00035  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.004 *********'.    CL**4
00036                                                                   EL694
00037  01  W-PROGRAM-CONSTANTS.                                         EL694
00038      12  FILLER                  PIC  X(18)                       EL694
00039                                  VALUE 'PROGRAM CONSTANTS:'.      EL694
00040      12  W-ARCH-LENGTH           PIC S9(04) COMP  VALUE +250.     EL694
00041      12  W-APPL-SCRTY-NDX        PIC S9(04) COMP VALUE +03.          CL**3
00042                                                                   EL694
00043      12  W-ARCH-ID               PIC  X(08)  VALUE 'ERARCH'.      EL694
00044      12  W-ARCH3-ID              PIC  X(08)  VALUE 'ERARCH3'.     EL694
00045      12  W-ARCH-TRANS            PIC  X(04)  VALUE 'EXM6'.        EL694
00046      12  W-CNTL-ID               PIC  X(08)  VALUE 'ELCNTL'.      EL694
00047      12  W-LINK-001              PIC  X(05)  VALUE 'EL001'.       EL694
00048      12  W-LINK-004              PIC  X(05)  VALUE 'EL004'.       EL694
00049      12  W-MAP.                                                   EL694
00050          16  W-MAP-PREFIX        PIC  X(02)  VALUE 'EL'.          EL694
00051          16  W-MAP-NUMBER        PIC  X(04)  VALUE '694A'.        EL694
00052          16  W-MAP-FILLER        PIC  X(02)  VALUE '  '.          EL694
00053      12  W-MAPSET                PIC  X(08)  VALUE 'EL694S'.      EL694
00054      12  W-PRINT-TRANS           PIC  X(04)  VALUE 'EXM5'.        EL694
00055      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL694'.       EL694
00056      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXM4'.        EL694
00057      12  W-XCTL-005              PIC  X(05)  VALUE 'EL005'.       EL694
00058      12  W-XCTL-010              PIC  X(05)  VALUE 'EL010'.       EL694
00059      12  W-XCTL-626              PIC  X(05)  VALUE 'EL626'.       EL694
00060                                  EJECT                            EL694
00061  01  W-PROGRAM-WORK-AREA.                                         EL694
00062      12  FILLER                  PIC  X(18)                       EL694
00063                                  VALUE 'PROGRAM WORK AREA:'.      EL694
00064      12  W-CHECK-CONTROL         PIC S9(08) COMP   VALUE +0.      EL694
00065      12  W-RECORD-COUNT          PIC S9(04) COMP   VALUE +0.      EL694
00066                                                                   EL694
00067      12  W-ALIGNMENT-COPIES      PIC S9(01) COMP-3 VALUE +0.      EL694
00068      12  W-COPIES-CTR            PIC S9(05) COMP-3 VALUE +0.      EL694
00069      12  W-DELAY-INTERVAL        PIC S9(07) COMP-3 VALUE +2.      EL694
00070      12  W-HOLD-PENDING-CTR      PIC S9(05) COMP-3 VALUE +0.      EL694
00071      12  W-INITIAL-PENDING-CTR   PIC S9(05) COMP-3 VALUE +0.      EL694
00072      12  W-RESEND-PENDING-CTR-TOTAL                               EL694
00073                                  PIC S9(05) COMP-3 VALUE +0.      EL694
00074      12  W-SUB                   PIC  9(01) COMP-3.               EL694
00075                                                                   EL694
00076      12  W-LAST-ERROR            PIC  9(05)  VALUE 9999.          EL694
00077                                                                   EL694
00078      12  W-CALL-PGM              PIC  X(08).                      EL694
00079      12  W-CARRIER               PIC  X(01)  VALUE SPACE.         EL694
00080      12  W-CURRENT-DATE-SAVE     PIC  X(02).                      EL694
00081      12  W-CURRENT-DATE-EDIT     PIC  X(08)  VALUE SPACES.        EL694
00082      12  W-DEEDIT-FIELD          PIC  X(15).                      EL694
00083      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD               EL694
00084                                  PIC S9(15).                      EL694
00085      12  W-ENTRY                 PIC  X(06)  VALUE SPACES.        EL694
00086      12  W-FORM                  PIC  X(04)  VALUE SPACES.        EL694
00087      12  W-GROUPING              PIC  X(06)  VALUE SPACES.        EL694
00088      12  W-ACCOUNT               PIC  X(10)  VALUE SPACES.        EL694
00089      12  W-STATE                 PIC  X(02)  VALUE SPACES.        EL694
00090                                                                   EL694
00091      12  W-TIME-IN               PIC S9(07).                      EL694
00092      12  W-TIME-OUT-R REDEFINES W-TIME-IN.                        EL694
00093          16  FILLER              PIC  X(01).                      EL694
00094          16  W-TIME-OUT          PIC  9(01)9V99.                  EL694
00095          16  FILLER              PIC  X(02).                      EL694
00096                                                                   EL694
00097  01  W-PROGRAM-SWITCHES-INDICATORS.                               EL694
00098      12  FILLER                  PIC  X(17)                       EL694
00099                                  VALUE 'PROGRAM SWITCHES:'.       EL694
00100      12  W-COUNT-BY-KEY-IND      PIC  X(01)  VALUE SPACE.         EL694
00101          88  W-COUNT-BY-KEY              VALUE 'A' 'C' 'G'        EL694
00102                                                'P' 'S'.           EL694
00103          88  W-COUNT-BY-CARRIER          VALUE 'C'.               EL694
00104          88  W-COUNT-BY-GROUPING         VALUE 'G'.               EL694
00105          88  W-COUNT-BY-STATE            VALUE 'S'.               EL694
00106          88  W-COUNT-BY-ACCOUNT          VALUE 'A'.               EL694
00107          88  W-COUNT-BY-PROCESSOR        VALUE 'P'.               EL694
00108          88  W-COUNT-BY-QUE-CONTROL      VALUE 'Q'.               EL694
00109          88  W-COUNT-BY-ENTRY            VALUE 'E'.               EL694
00110      12  W-DATE-FOUND-IND        PIC  X(01)  VALUE SPACE.         EL694
00111          88  W-DATE-FOUND                    VALUE 'Y'.           EL694
00112                                                                   EL694
00113  01  W-PROGRAM-KEY-FIELDS.                                        EL694
00114      12  FILLER                  PIC  X(19)                       EL694
00115                                  VALUE 'PROGRAM KEY FIELDS:'.     EL694
00116      12  W-ARCH-KEY.                                              EL694
00117          16  W-ARCH-CO           PIC  X(01).                      EL694
00118          16  W-ARCH-NUMBER       PIC S9(08)     COMP.             EL694
00119          16  W-ARCH-SEQ          PIC S9(04)     COMP VALUE +0.    EL694
00120                                                                   EL694
00121      12  W-ARCH-SAVE-KEY         PIC  X(05).                      EL694
00122                                                                   EL694
00123      12  W-CNTL-KEY.                                              EL694
00124          16  W-CNTL-COMPANY-ID   PIC  X(03).                      EL694
00125          16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '1'.           EL694
00126          16  W-CNTL-GENL.                                         EL694
00127              20  W-CNTL-GEN1     PIC  X(02)  VALUE SPACES.        EL694
00128              20  W-CNTL-GEN2.                                     EL694
00129                  24   W-CNTL-GEN3                                 EL694
00130                                  PIC  X(01)  VALUE SPACES.        EL694
00131                  24   W-CNTL-GEN4                                 EL694
00132                                  PIC  X(01)  VALUE SPACES.        EL694
00133          16  W-CNTL-SEQ          PIC S9(04)  VALUE +0    COMP.    EL694
00134                                                                   EL694
00135      12  W-SC-QUID-KEY.                                           EL694
00136          16  W-SC-QUID-TERMINAL  PIC  X(04).                      EL694
00137          16  W-SC-QUID-SYSTEM    PIC  X(04).                      EL694
00138                                                                   EL694
00139  01  W-PROGRAM-TABLES.                                            EL694
00140      12  FILLER                  PIC  X(15)                       EL694
00141                                  VALUE 'PROGRAM TABLES:'.         EL694
00142      12  W-RESEND-COUNTER-TABLE.                                  EL694
00143          16  W-RESEND-PENDING-CTR PIC S9(07) COMP-3.

00147  01  FILLER                      PIC  X(23)                       EL694
00148                                  VALUE 'INTERFACE AREA STARTS:'.  EL694
00149      COPY ELCINTF.                                                EL694
00150      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL694
00151 **********************************************************        EL694
00152 *    NOTE                                                *        EL694
00153 *        THE WORK AREA IS USED BY EL694 AND EL6942       *        EL694
00154 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *        EL694
00155 *        BOTH PROGRAMS.                                  *        EL694
00156 **********************************************************        EL694
00157          16  PI-694-ALIGNMENT-COPIES                              EL694
00158                                  PIC S9(01) COMP-3.               EL694
00159          16  PI-694-PRINT-DATE   PIC  X(08).                      EL694
00160          16  PI-694-PRINT-DATE-BIN                                EL694
00161                                  PIC  X(02).                      EL694
00162          16  PI-694-PRINT-BY-KEY-IND                              EL694
00163                                  PIC  X(01).                      EL694
00164              88  PI-694-PRINT-BY-KEY       VALUE 'C' 'G' 'S' 'A'. EL694
00165              88  PI-694-PRINT-BY-CARRIER   VALUE 'C'.             EL694
00166              88  PI-694-PRINT-BY-GROUPING  VALUE 'G'.             EL694
00167              88  PI-694-PRINT-BY-STATE     VALUE 'S'.             EL694
00168              88  PI-694-PRINT-BY-ACCOUNT   VALUE 'A'.             EL694
00169          16  PI-694-PRINT-BY-PROCESSOR-IND                        EL694
00170                                  PIC  X(01).                      EL694
00171              88  PI-694-PRINT-BY-PROCESSOR                        EL694
00172                                            VALUE 'Y'.             EL694
00173          16  PI-694-PRINT-ID     PIC  X(04).                      EL694
00174          16  PI-694-PRINT-KEY.                                    EL694
00175              20  PI-694-PRINT-CARRIER                             EL694
00176                                  PIC  X(01).                      EL694
00177              20  PI-694-PRINT-GROUPING                            EL694
00178                                  PIC  X(06).                      EL694
00179              20  PI-694-PRINT-STATE                               EL694
00180                                  PIC  X(02).                      EL694
00181              20  PI-694-PRINT-ACCOUNT                             EL694
00182                                  PIC  X(10).                      EL694
00183          16  PI-694-PRINT-PROCESSOR                               EL694
00184                                  PIC  X(04).                      EL694
00185          16  PI-694-LETTER-FORM  PIC  X(04).                      EL694
00186          16  PI-694-LETTER-TYPE  PIC  X(01).                      EL694
00187          16  PI-694-STARTING-ARCH-NO                              EL694
00188                                  PIC S9(08) COMP.                 EL694
00189          16  PI-694-ENTRY.                                        EL694
00190              20  PI-694-FILLER   PIC  X(02).                      EL694
00191              20  PI-694-QUE-CONTROL                               EL694
00192                                  PIC S9(08) COMP.                 EL694
00193          16  FILLER              PIC  X(585).                        CL**3
00194                                  EJECT                            EL694
00195  01  FILLER                      PIC  X(15)                       EL694
00196                                  VALUE 'MAP AREA START:'.         EL694
00197      COPY EL694S.                                                 EL694
00198  01  FILLER                      PIC  X(14)                       EL694
00199                                  VALUE 'MAP AREA ENDS:'.          EL694
00200                                  EJECT                            EL694
00201  01  ERROR-MESSAGES.                                              EL694
00202      12  ER-ZEROS                PIC  9(05) VALUE 0000.           EL694
00203      12  ER-0000                 PIC  9(05) VALUE 0000.           EL694
00204      12  ER-0004                 PIC  9(05) VALUE 0004.           EL694
00205      12  ER-0008                 PIC  9(05) VALUE 0008.           EL694
00206      12  ER-0029                 PIC  9(05) VALUE 0029.           EL694
00207      12  ER-0042                 PIC  9(05) VALUE 0042.           EL694
00208      12  ER-0070                 PIC  9(05) VALUE 0070.           EL694
00209      12  ER-0172                 PIC  9(05) VALUE 0172.           EL694
00210      12  ER-0182                 PIC  9(05) VALUE 0182.           EL694
00211      12  ER-0189                 PIC  9(05) VALUE 0189.           EL694
00212      12  ER-0190                 PIC  9(05) VALUE 0190.           EL694
00213      12  ER-0409                 PIC  9(05) VALUE 0409.           EL694
00214      12  ER-0410                 PIC  9(05) VALUE 0410.           EL694
00215      12  ER-0411                 PIC  9(05) VALUE 0411.           EL694
00216      12  ER-0412                 PIC  9(05) VALUE 0412.           EL694
00217      12  ER-0413                 PIC  9(05) VALUE 0413.           EL694
00218      12  ER-0669                 PIC  9(05) VALUE 0669.           EL694
00219      12  ER-7357                 PIC  9(05) VALUE 7357.           EL694
00220      12  ER-7362                 PIC  9(05) VALUE 7362.           EL694
00221      12  ER-7382                 PIC  9(05) VALUE 7382.           EL694
00222      12  ER-7383                 PIC  9(05) VALUE 7383.           EL694
00223      12  ER-7397                 PIC  9(05) VALUE 7397.           EL694
00224      12  ER-9067                 PIC  9(05) VALUE 9067.           EL694
00225      12  ER-9097                 PIC  9(05) VALUE 9097.           EL694
00226      12  ER-9409                 PIC  9(05) VALUE 9409.           EL694
00227                                                                   EL694
00228                                  EJECT                            EL694
00229      COPY ELCAID.                                                 EL694
00230  01  FILLER    REDEFINES DFHAID.                                  EL694
00231      12  FILLER              PIC  X(08).                          EL694
00232      12  PF-VALUES           PIC  X(01)  OCCURS 24 TIMES.         EL694
00233                                  EJECT                            EL694
00234      COPY ELCATTR.                                                EL694
00235                                  EJECT                            EL694
00236      COPY ELCDATE.                                                EL694
00237                                  EJECT                            EL694
00238      COPY ELCLOGOF.                                               EL694
00239                                  EJECT                            EL694
00240      COPY ELCEMIB.                                                EL694
00241                                  EJECT                            EL694
00242      COPY ELCSCTM.                                                EL694
00243                                  EJECT                            EL694
00244      COPY ELCSCRTY.                                               EL694
00245                                                                   EL694
00246  LINKAGE SECTION.                                                 EL694
00247  01  DFHCOMMAREA             PIC  X(1024).                        EL694
00248                                                                   EL694
00249 *01 PARMLIST .                                                       CL**3
00250 *    02  FILLER              PIC S9(08)  COMP.                       CL**3
00251 *    02  W-ARCH-POINTER      PIC S9(08)  COMP.                       CL**3
00252 *    02  W-CNTL-POINTER      PIC S9(08)  COMP.                       CL**3
00253                                  EJECT                            EL694
00254      COPY ERCARCH.                                                EL694
00255                                  EJECT                            EL694
00256      COPY ELCCNTL.                                                EL694
00257                                  EJECT                            EL694
00258  PROCEDURE DIVISION.                                              EL694
00259                                                                   EL694
00260      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL694
00261                                                                   EL694
00262      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL694
00263                                                                   EL694
00264      IF  EIBCALEN = 0                                             EL694
00265          MOVE UNACCESS-MSG       TO LOGOFF-MSG                    EL694
00266          GO TO 8300-SEND-TEXT.                                    EL694
00267                                                                   EL694
00268      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL694
00269      MOVE '5'                    TO DC-OPTION-CODE.               EL694
00270      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.               EL694
00271      MOVE DC-BIN-DATE-1          TO W-CURRENT-DATE-SAVE.          EL694
00272      MOVE DC-GREG-DATE-1-EDIT    TO W-CURRENT-DATE-EDIT.          EL694
00273                                                                   EL694
00274      IF  PI-CALLING-PROGRAM NOT = W-THIS-PGM                      EL694
00275                                                                   EL694
00276          IF  PI-RETURN-TO-PROGRAM NOT = W-THIS-PGM                EL694
00277              MOVE PI-SAVED-PROGRAM-5                              EL694
00278                                  TO PI-SAVED-PROGRAM-6            EL694
00279              MOVE PI-SAVED-PROGRAM-4                              EL694
00280                                  TO PI-SAVED-PROGRAM-5            EL694
00281              MOVE PI-SAVED-PROGRAM-3                              EL694
00282                                  TO PI-SAVED-PROGRAM-4            EL694
00283              MOVE PI-SAVED-PROGRAM-2                              EL694
00284                                  TO PI-SAVED-PROGRAM-3            EL694
00285              MOVE PI-SAVED-PROGRAM-1                              EL694
00286                                  TO PI-SAVED-PROGRAM-2            EL694
00287              MOVE PI-RETURN-TO-PROGRAM                            EL694
00288                                  TO PI-SAVED-PROGRAM-1            EL694
00289              MOVE PI-CALLING-PROGRAM                              EL694
00290                                  TO PI-RETURN-TO-PROGRAM          EL694
00291              MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM            EL694
00292              MOVE LOW-VALUES     TO EL694AO                       EL694
00293                                     PI-PROGRAM-WORK-AREA          EL694
00294              MOVE ZEROS          TO PI-694-PRINT-DATE             EL694
00295                                     PI-694-ALIGNMENT-COPIES       EL694
00296                                     PI-694-STARTING-ARCH-NO       EL694
00297              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT      EL694
00298              GO TO 8100-SEND-INITIAL-MAP.                         EL694
00299                                                                   EL694
00300      EXEC CICS HANDLE CONDITION                                   EL694
00301          PGMIDERR (9700-PGMID-ERROR)                              EL694
00302          ERROR    (9800-ABEND)                                    EL694
00303      END-EXEC.                                                    EL694
00304                                                                   EL694
00305      IF  EIBAID = DFHCLEAR                                        EL694
00306              OR                                                   EL694
00307          NOT DISPLAY-CAP                                          EL694
00308          MOVE PI-RETURN-TO-PROGRAM                                EL694
00309                                  TO W-CALL-PGM                    EL694
00310          GO TO 9400-XCTL.                                         EL694
00311                                  EJECT                            EL694
00312  0200-RECEIVE.                                                    EL694
00313                                                                   EL694
00314      MOVE LOW-VALUES             TO EL694AI.                      EL694
00315                                                                   EL694
00316      IF  EIBAID = DFHPA1                                          EL694
00317              OR                                                   EL694
00318          EIBAID = DFHPA2                                          EL694
00319              OR                                                   EL694
00320          EIBAID = DFHPA3                                          EL694
00321          MOVE ER-0008            TO EMI-ERROR                     EL694
00322          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00323          MOVE -1                 TO OPTIONL                       EL694
00324          GO TO 8200-SEND-DATAONLY.                                EL694
00325                                                                   EL694
00326      EXEC CICS RECEIVE                                            EL694
00327          MAP       (W-MAP)                                        EL694
00328          MAPSET    (W-MAPSET)                                     EL694
00329          INTO      (EL694AI)                                      EL694
00330      END-EXEC.                                                    EL694
00331                                                                   EL694
00332      IF  ENTERPFL = 0                                             EL694
00333          GO TO 0300-CHECK-PFKEYS.                                 EL694
00334                                                                   EL694
00335      IF  EIBAID NOT = DFHENTER                                    EL694
00336          MOVE ER-0004            TO EMI-ERROR                     EL694
00337          GO TO 0320-INPUT-ERROR.                                  EL694
00338                                                                   EL694
00339      IF  ENTERPFI NUMERIC                                         EL694
00340              AND                                                  EL694
00341          ENTERPFI GREATER 0                                       EL694
00342              AND                                                  EL694
00343          ENTERPFI LESS 25                                         EL694
00344          MOVE PF-VALUES (ENTERPFI)                                EL694
00345                                  TO EIBAID                        EL694
00346                                                                   EL694
00347      ELSE                                                         EL694
00348          MOVE ER-0029            TO EMI-ERROR                     EL694
00349          GO TO 0320-INPUT-ERROR.                                  EL694
00350                                  EJECT                            EL694
00351  0300-CHECK-PFKEYS.                                               EL694
00352                                                                   EL694
00353      IF  EIBAID = DFHPF23                                         EL694
00354          MOVE EIBAID             TO PI-ENTRY-CD-1                 EL694
00355          MOVE W-XCTL-005         TO W-CALL-PGM                    EL694
00356          GO TO 9400-XCTL.                                         EL694
00357                                                                   EL694
00358      IF  EIBAID = DFHPF24                                         EL694
00359          MOVE W-XCTL-626         TO W-CALL-PGM                    EL694
00360          GO TO 9400-XCTL.                                         EL694
00361                                                                   EL694
00362      IF  EIBAID = DFHPF12                                         EL694
00363          MOVE W-XCTL-010         TO W-CALL-PGM                    EL694
00364          GO TO 9400-XCTL.                                         EL694
00365                                                                   EL694
00366      IF  EIBAID = DFHENTER                                        EL694
00367          GO TO 0330-FUNCTION-CHECK.                               EL694
00368                                                                   EL694
00369      MOVE ER-0029                TO EMI-ERROR.                    EL694
00370                                                                   EL694
00371  0320-INPUT-ERROR.                                                EL694
00372                                                                   EL694
00373      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL694
00374      MOVE AL-UNBON               TO ENTERPFA.                     EL694
00375                                                                   EL694
00376      IF  ENTERPFL = 0                                             EL694
00377          MOVE -1                 TO OPTIONL                       EL694
00378                                                                   EL694
00379      ELSE                                                         EL694
00380          MOVE -1                 TO ENTERPFL.                     EL694
00381                                                                   EL694
00382      GO TO 8200-SEND-DATAONLY.                                    EL694
00383                                                                   EL694
00384                                  EJECT                            EL694
00385  0330-FUNCTION-CHECK.                                             EL694
00386                                                                   EL694
00387      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.                    EL694
00388                                                                   EL694
00389      IF  NOT EMI-NO-ERRORS                                        EL694
00390          GO TO 8200-SEND-DATAONLY.                                EL694
00391                                                                   EL694
00392      IF  NOT MODIFY-CAP                                           EL694
00393                                                                   EL694
00394          IF  OPTIONI = '3'                                        EL694
00395                  OR                                               EL694
00396              OPTIONI = '4'                                        EL694
00397              NEXT SENTENCE                                        EL694
00398                                                                   EL694
00399          ELSE                                                     EL694
00400              MOVE 'UPDATE'       TO SM-READ                       EL694
00401              PERFORM 9995-SECURITY-VIOLATION                      EL694
00402              MOVE ER-0070        TO EMI-ERROR                     EL694
00403              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL694
00404              GO TO 8100-SEND-INITIAL-MAP.                         EL694
00405                                                                   EL694
00406      MOVE 'N'                    TO PI-694-PRINT-BY-KEY-IND       EL694
00407                                     W-COUNT-BY-KEY-IND.           EL694
00408                                                                   EL694
00409      IF  CARRL IS NOT EQUAL 0                                     EL694
00410          MOVE 'C'                TO PI-694-PRINT-BY-KEY-IND       EL694
00411                                     W-COUNT-BY-KEY-IND            EL694
00412          MOVE CARRI              TO PI-694-PRINT-CARRIER          EL694
00413                                     W-CARRIER.                    EL694
00414                                                                   EL694
00415      IF  GROUPL IS NOT EQUAL 0                                    EL694
00416          MOVE 'G'                TO PI-694-PRINT-BY-KEY-IND       EL694
00417                                     W-COUNT-BY-KEY-IND            EL694
00418          MOVE GROUPI             TO PI-694-PRINT-GROUPING         EL694
00419                                     W-GROUPING.                   EL694
00420                                                                   EL694
00421      IF  STATEL IS NOT EQUAL 0                                    EL694
00422          MOVE 'S'                TO PI-694-PRINT-BY-KEY-IND       EL694
00423                                     W-COUNT-BY-KEY-IND            EL694
00424          MOVE STATEI             TO PI-694-PRINT-STATE            EL694
00425                                     W-STATE.                      EL694
00426                                                                   EL694
00427      IF  ACCTL IS NOT EQUAL 0                                     EL694
00428          MOVE 'A'                TO PI-694-PRINT-BY-KEY-IND       EL694
00429                                     W-COUNT-BY-KEY-IND            EL694
00430          MOVE ACCTI              TO PI-694-PRINT-ACCOUNT          EL694
00431                                     W-ACCOUNT.                    EL694
00432                                                                   EL694
00433      IF  PI-694-PRINT-ACCOUNT GREATER THAN SPACES                 EL694
00434              AND                                                  EL694
00435          (PI-694-PRINT-STATE NOT GREATER THAN SPACES              EL694
00436                  OR                                               EL694
00437              PI-694-PRINT-GROUPING NOT GREATER THAN SPACES        EL694
00438                  OR                                               EL694
00439              PI-694-PRINT-CARRIER NOT GREATER THAN SPACES)        EL694
00440          MOVE -1                 TO ACCTL                         EL694
00441          MOVE ER-7397            TO EMI-ERROR                     EL694
00442          MOVE AL-UNBON           TO ACCTA                         EL694
00443          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00444          GO TO 0330-SKIP-KEY-EDIT.                                EL694
00445                                                                   EL694
00446      IF  PI-694-PRINT-STATE GREATER THAN SPACES                   EL694
00447              AND                                                  EL694
00448          (PI-694-PRINT-GROUPING NOT GREATER THAN SPACES           EL694
00449                  OR                                               EL694
00450              PI-694-PRINT-CARRIER NOT GREATER THAN SPACES)        EL694
00451          MOVE -1                 TO STATEL                        EL694
00452          MOVE ER-7397            TO EMI-ERROR                     EL694
00453          MOVE AL-UNBON           TO STATEA                        EL694
00454          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00455          GO TO 0330-SKIP-KEY-EDIT.                                EL694
00456                                                                   EL694
00457      IF  PI-694-PRINT-GROUPING GREATER THAN SPACES                EL694
00458              AND                                                  EL694
00459          PI-694-PRINT-CARRIER NOT GREATER THAN SPACES             EL694
00460          MOVE -1                 TO GROUPL                        EL694
00461          MOVE ER-7397            TO EMI-ERROR                     EL694
00462          MOVE AL-UNBON           TO GROUPA                        EL694
00463          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00464          GO TO 0330-SKIP-KEY-EDIT.                                EL694
00465                                                                   EL694
00466  0330-SKIP-KEY-EDIT.                                              EL694
00467                                                                   EL694
00468      IF  OVRPRCL NOT EQUAL ZEROS                                  EL694
00469          MOVE 'Y'                TO PI-694-PRINT-BY-PROCESSOR-IND EL694
00470          MOVE OVRPRCI            TO PI-694-PRINT-PROCESSOR.       EL694
00471                                                                   EL694
00472      MOVE AL-SADOF               TO TITCNTA                       EL694
00473                                     TITCNTCA                      EL694
00474                                     TITCNTHA                      EL694
00475                                     TITCNT1A                      EL694
00476                                     TITCNT2A                      EL694
00477                                     TITCNT3A                      EL694
00478                                     COUNTA                        EL694
00479                                     COUNTCA                       EL694
00480                                     COUNTHA                       EL694
00481                                     COUNT1A                       EL694
00482                                     COUNT2A                       EL694
00483                                     COUNT3A.                      EL694
00484                                                                   EL694
00485      IF  OPTIONI EQUAL '1'                                        EL694
00486          GO TO 1000-PRINT-INITIAL-LETTER.                         EL694
00487                                                                   EL694
00488      IF  OPTIONI EQUAL '2'                                        EL694
00489          GO TO 2000-PRINT-FOLLOW-UP-LETTERS.                      EL694
00490                                                                   EL694
00491      IF  OPTIONI EQUAL '3'                                        EL694
00492          GO TO 3000-SHOW-COUNT-OF-INITIAL.                        EL694
00493                                                                   EL694
00494      IF  OPTIONI EQUAL '4'                                        EL694
00495          GO TO 4000-SHOW-COUNT-OF-FOLLOW-UP.                      EL694
00496                                                                   EL694
00497      IF  OPTIONI EQUAL '5'                                        EL694
00498          GO TO 5000-PRINT-ADDRESS-LABELS.                         EL694
00499                                                                   EL694
00500      IF  OPTIONI EQUAL '6'                                        EL694
00501          GO TO 6000-REPRINT-LETTERS-BY-DATE.                      EL694
00502                                                                   EL694
00503      IF  OPTIONI EQUAL '7'                                        EL694
00504          GO TO 7000-CREATE-ARCHIVE-LETTERS.                       EL694
00505                                  EJECT                            EL694
00506  0350-EDIT-ROUTINE.                                               EL694
00507                                                                   EL694
00508      IF  OPTIONI LESS THAN '1'                                    EL694
00509              OR                                                   EL694
00510          OPTIONI GREATER THAN '7'                                 EL694
00511          MOVE -1                 TO OPTIONL                       EL694
00512          MOVE ER-0409            TO EMI-ERROR                     EL694
00513          MOVE AL-UNBON           TO OPTIONA                       EL694
00514          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00515          GO TO 0350-EXIT                                          EL694
00516                                                                   EL694
00517      ELSE                                                         EL694
00518          MOVE AL-UNNON           TO OPTIONA.                      EL694
00519                                                                   EL694
00520      IF  PI-BYPASS-LABELS                                         EL694
00521              AND                                                  EL694
00522          OPTIONI EQUAL '5'                                        EL694
00523          MOVE -1                 TO OPTIONL                       EL694
00524          MOVE ER-9409            TO EMI-ERROR                     EL694
00525          MOVE AL-UNBON           TO OPTIONA                       EL694
00526          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00527          GO TO 0350-EXIT.                                         EL694
00528                                                                   EL694
00529      IF  (OPTIONI = '5'                                           EL694
00530              OR                                                   EL694
00531          OPTIONI = '6')                                           EL694
00532              AND                                                  EL694
00533          DATEINL = ZEROS                                          EL694
00534          MOVE -1                 TO DATEINL                       EL694
00535          MOVE ER-0410            TO EMI-ERROR                     EL694
00536          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL694
00537                                                                   EL694
00538      IF  DATEINL NOT = ZEROS                                      EL694
00539          MOVE DATEINI            TO W-DEEDIT-FIELD                EL694
00540          PERFORM 8000-DEEDIT                                      EL694
00541          MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY            EL694
00542          MOVE '4'                TO DC-OPTION-CODE                EL694
00543          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL694
00544                                                                   EL694
00545          IF  DATE-CONVERSION-ERROR                                EL694
00546              MOVE ER-0182        TO EMI-ERROR                     EL694
00547              MOVE -1             TO DATEINL                       EL694
00548              MOVE AL-UABON       TO DATEINA                       EL694
00549              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL694
00550                                                                   EL694
00551          ELSE                                                     EL694
00552              MOVE AL-UANON       TO DATEINA                       EL694
00553              MOVE DC-BIN-DATE-1  TO PI-694-PRINT-DATE-BIN         EL694
00554              MOVE DC-GREG-DATE-1-EDIT                             EL694
00555                                  TO PI-694-PRINT-DATE             EL694
00556                                     DATEINI                       EL694
00557      ELSE                                                         EL694
00558          MOVE LOW-VALUE          TO PI-694-PRINT-DATE-BIN.        EL694
00559                                                                   EL694
00560      IF  LETRTYPL NOT = ZEROS                                     EL694
00561                                                                   EL694
00562          IF  LETRTYPI = 'R' OR  'I'                               EL694
00563              MOVE LETRTYPI       TO PI-694-LETTER-TYPE            EL694
00564              MOVE AL-UANON       TO LETRTYPA                      EL694
00565                                                                   EL694
00566          ELSE                                                     EL694
00567              MOVE ER-0669        TO EMI-ERROR                     EL694
00568              MOVE -1             TO LETRTYPL                      EL694
00569              MOVE AL-UABON       TO LETRTYPA                      EL694
00570              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL694
00571                                                                   EL694
00572      IF  CKCNTLL GREATER THAN +0                                  EL694
00573              AND                                                  EL694
00574          ENTRYL GREATER THAN +0                                   EL694
00575          MOVE ER-7362            TO EMI-ERROR                     EL694
00576          MOVE -1                 TO CKCNTLL                       EL694
00577          MOVE AL-UABON           TO CKCNTLA                       EL694
00578          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00579          GO TO 8200-SEND-DATAONLY.                                EL694
00580                                                                   EL694
00581      IF  CKCNTLL GREATER THAN +0                                  EL694
00582          MOVE CKCNTLI            TO PI-694-QUE-CONTROL            EL694
00583                                     W-CHECK-CONTROL               EL694
00584          MOVE 'CK'               TO PI-694-FILLER                 EL694
00585          MOVE AL-UNNON           TO CKCNTLA.                      EL694
00586                                                                   EL694
00587      IF  ENTRYL GREATER THAN +0                                   EL694
00588          MOVE ENTRYI             TO PI-694-ENTRY                  EL694
00589                                     W-ENTRY                       EL694
00590          MOVE AL-UANON           TO ENTRYA.                       EL694
00591                                                                   EL694
00592      IF  FORML GREATER THAN +0                                    EL694
00593          MOVE FORMI              TO PI-694-LETTER-FORM            EL694
00594                                     W-FORM                        EL694
00595          MOVE AL-UANON           TO FORMA.                        EL694
00596                                                                   EL694
00597      IF  ALIGNL GREATER THAN +0                                   EL694
00598                                                                   EL694
00599          IF  ALIGNI NUMERIC                                       EL694
00600              MOVE ALIGNI         TO PI-694-ALIGNMENT-COPIES       EL694
00601                                     W-ALIGNMENT-COPIES            EL694
00602              MOVE AL-UNNON       TO ALIGNA                        EL694
00603                                                                   EL694
00604          ELSE                                                     EL694
00605              MOVE ER-9067        TO EMI-ERROR                     EL694
00606              MOVE -1             TO ALIGNL                        EL694
00607              MOVE AL-UABON       TO ALIGNA                        EL694
00608              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL694
00609                                                                   EL694
00610      IF  NOT EMI-NO-ERRORS                                        EL694
00611          GO TO 0350-EXIT.                                         EL694
00612                                                                   EL694
00613      PERFORM 7900-GET-CNTL-INFO THRU 7999-EXIT.                   EL694
00614                                                                   EL694
00615  0350-EXIT.                                                       EL694
00616       EXIT.                                                       EL694
00617                                  EJECT                            EL694
00618  1000-PRINT-INITIAL-LETTER.                                       EL694
00619                                                                   EL694
00620      MOVE '1'                    TO PI-ENTRY-CD-1                 EL694
00621                                     PI-ENTRY-CD-2.                EL694
00622      GO TO 7800-START-PRINT.                                      EL694
00623                                                                   EL694
00624  2000-PRINT-FOLLOW-UP-LETTERS.                                    EL694
00625                                                                   EL694
00626      MOVE '1'                    TO PI-ENTRY-CD-1.                EL694
00627      MOVE '2'                    TO PI-ENTRY-CD-2.                EL694
00628      GO TO 7800-START-PRINT.                                      EL694
00629                                                                   EL694
00630  3000-SHOW-COUNT-OF-INITIAL.                                      EL694
00631                                                                   EL694
00632      PERFORM 7500-BROWSE-ARCHIVE THRU 7599-EXIT.                  EL694
00633      GO TO 6500-COMPLETE-COUNT.                                   EL694
00634                                                                   EL694
00635  4000-SHOW-COUNT-OF-FOLLOW-UP.                                    EL694
00636                                                                   EL694
00637      PERFORM 7500-BROWSE-ARCHIVE THRU 7599-EXIT.                  EL694
00638      GO TO 6500-COMPLETE-COUNT.                                   EL694
00639                                                                   EL694
00640  5000-PRINT-ADDRESS-LABELS.                                       EL694
00641                                                                   EL694
00642      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL694
00643      MOVE '2'                    TO PI-ENTRY-CD-2.                EL694
00644      GO TO 7800-START-PRINT.                                      EL694
00645                                                                   EL694
00646  6000-REPRINT-LETTERS-BY-DATE.                                    EL694
00647                                                                   EL694
00648      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL694
00649      MOVE '3'                    TO PI-ENTRY-CD-2.                EL694
00650      GO TO 7800-START-PRINT.                                      EL694
00651                                                                   EL694
00652  6500-COMPLETE-COUNT.                                             EL694
00653                                                                   EL694
00654      IF  OPTIONI EQUAL '3'                                        EL694
00655          MOVE W-INITIAL-PENDING-CTR                               EL694
00656                                  TO COUNTO                        EL694
00657 *        MOVE 'COUNT:'           TO TITCNTO                       EL694
00658          MOVE W-COPIES-CTR       TO COUNTCO                       EL694
00659          MOVE W-HOLD-PENDING-CTR TO COUNTHO                       EL694
00660          MOVE AL-SANON           TO COUNTA                        EL694
00661                                     COUNTCA                       EL694
00662                                     COUNTHA                       EL694
00663                                     TITCNTCA                      EL694
00664                                     TITCNTA                       EL694
00665                                     TITCNTHA                      EL694
00666                                                                   EL694
00667      ELSE                                                         EL694
00668          IF  OPTIONI EQUAL '4'                                    EL694
00669              MOVE W-RESEND-PENDING-CTR
00670                                  TO COUNT1O                       EL694
00675              MOVE W-RESEND-PENDING-CTR-TOTAL                      EL694
00676                                  TO COUNTO                        EL694
00677              MOVE W-COPIES-CTR   TO COUNTCO                       EL694
00678              MOVE W-HOLD-PENDING-CTR                              EL694
00679                                  TO COUNTHO                       EL694
00680 *            MOVE 'TOTAL:'       TO TITCNTO                       EL694
00681              MOVE AL-SANON       TO COUNTA                        EL694
00682                                     COUNTCA                       EL694
00683                                     COUNTHA                       EL694
00684                                     COUNT1A                       EL694
00685                                     COUNT2A                       EL694
00686                                     COUNT3A                       EL694
00687                                     TITCNTA                       EL694
00688                                     TITCNTCA                      EL694
00689                                     TITCNTHA                      EL694
00690                                     TITCNT1A                      EL694
00691                                     TITCNT2A                      EL694
00692                                     TITCNT3A.                     EL694
00693                                                                   EL694
00694      MOVE -1                     TO OPTIONL.                      EL694
00695      MOVE ER-ZEROS               TO EMI-ERROR.                    EL694
00696      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL694
00697      GO TO 8200-SEND-DATAONLY.                                    EL694
00698                                  EJECT                            EL694
00699  7000-CREATE-ARCHIVE-LETTERS.                                     EL694
00700                                                                   EL694
00701      IF  CKCNTLL NOT EQUAL ZEROS                                  EL694
00702          MOVE 'CK'               TO PI-694-FILLER                 EL694
00703          MOVE CKCNTLI            TO PI-694-QUE-CONTROL            EL694
00704          MOVE PI-694-ENTRY       TO PI-CR-BATCH-NUMBER            EL694
00705                                                                   EL694
00706      ELSE                                                         EL694
00707          MOVE ER-7383            TO EMI-ERROR                     EL694
00708          MOVE -1                 TO CKCNTLL                       EL694
00709          MOVE AL-UABON           TO CKCNTLA                       EL694
00710          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL694
00711          GO TO 8200-SEND-DATAONLY.                                EL694
00712                                                                   EL694
00713      MOVE PI-COMPANY-CD          TO W-ARCH-CO.                    EL694
00714                                                                   EL694
00715      EXEC CICS HANDLE CONDITION                                   EL694
00716           TERMIDERR   (8020-TERMID-ERROR)                         EL694
00717           TRANSIDERR  (8030-TRANS-ERROR)                          EL694
00718           END-EXEC.                                               EL694
00719                                                                   EL694
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**4
00721 *        MOVE EIBTRMID       TO PI-694-PRINT-ID                      CL**4
00722          EXEC CICS START                                             CL**4
00723               INTERVAL    (0)                                        CL**4
00724               TRANSID     (W-ARCH-TRANS)                             CL**4
00725               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**4
00726               LENGTH      (PI-COMM-LENGTH)                           CL**4
00727 *             TERMID      (PI-694-PRINT-ID)                          CL**4
00728          END-EXEC                                                    CL**4
00729      ELSE                                                            CL**4
00730          EXEC CICS START                                             CL**4
00731               INTERVAL    (0)                                        CL**4
00732               TRANSID     (W-ARCH-TRANS)                             CL**4
00733               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**4
00734               LENGTH      (PI-COMM-LENGTH)                           CL**4
00735               TERMID      (PI-694-PRINT-ID)                          CL**4
00736          END-EXEC.                                                   CL**4
00737                                                                   EL694
00738      MOVE ER-7382               TO EMI-ERROR.                     EL694
00739      MOVE -1                    TO OPTIONL.                       EL694
00740      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL694
00741      GO TO 8200-SEND-DATAONLY.                                    EL694
00742                                                                   EL694
00743  7000-EXIT.                                                       EL694
00744      EXIT.                                                        EL694
00745                                  EJECT                            EL694
00746  7500-BROWSE-ARCHIVE.                                             EL694
00747                                                                   EL694
00748      MOVE LOW-VALUES             TO W-ARCH-KEY.                   EL694
00749      MOVE PI-694-STARTING-ARCH-NO                                 EL694
00750                                  TO W-ARCH-NUMBER.                EL694
00751      MOVE PI-COMPANY-CD          TO W-ARCH-CO.                    EL694
00752      MOVE ZEROS                  TO W-RESEND-PENDING-CTR
00755                                     W-RESEND-PENDING-CTR-TOTAL    EL694
00756                                     W-HOLD-PENDING-CTR            EL694
00757                                     W-COPIES-CTR                  EL694
00758                                     W-INITIAL-PENDING-CTR.        EL694
00759                                                                   EL694
00760      EXEC CICS HANDLE CONDITION                                   EL694
00761           NOTFND  (7599-EXIT)                                     EL694
00762           ENDFILE (7599-EXIT)                                     EL694
00763           NOTOPEN (8050-ARCH-NOT-OPEN)                            EL694
00764      END-EXEC.                                                    EL694
00765                                                                   EL694
00766      EXEC CICS STARTBR                                            EL694
00767           DATASET (W-ARCH-ID)                                     EL694
00768           RIDFLD  (W-ARCH-KEY)                                    EL694
00769           GTEQ                                                    EL694
00770      END-EXEC.                                                    EL694
00771                                                                   EL694
00772 *7505-RESET-HANDLE.                                               EL694
00773 *                                                                 EL694
00774 *    EXEC CICS HANDLE CONDITION                                   EL694
00775 *         NOTFND  (7599-EXIT)                                     EL694
00776 *         ENDFILE (7599-EXIT)                                     EL694
00777 *         NOTOPEN (8050-ARCH-NOT-OPEN)                            EL694
00778 *    END-EXEC.                                                    EL694
00779 *                                                                 EL694
00780  7510-READ-NEXT.                                                  EL694
00781                                                                   EL694
00782      EXEC CICS READNEXT                                           EL694
00783           DATASET (W-ARCH-ID)                                     EL694
00784           RIDFLD  (W-ARCH-KEY)                                    EL694
00785           SET     (ADDRESS OF LETTER-ARCHIVE)                        CL**3
00786      END-EXEC.                                                    EL694
00787                                                                   EL694
00788      IF  W-ARCH-CO NOT = PI-COMPANY-CD                            EL694
00789          GO TO 7599-EXIT.                                         EL694
00790                                                                   EL694
00791      ADD +1                      TO W-RECORD-COUNT.               EL694
00792                                                                   EL694
00793      IF  W-RECORD-COUNT IS GREATER THAN +100                      EL694
00794          MOVE +0                 TO W-RECORD-COUNT                EL694
00795                                                                   EL694
00796          EXEC CICS DELAY                                          EL694
00797              INTERVAL  (W-DELAY-INTERVAL)                         EL694
00798          END-EXEC.                                                EL694
00799                                                                   EL694
00800      IF  LA-STATUS-VOIDED                                         EL694
00801              OR                                                   EL694
00802          LA-STATUS-PURGED                                         EL694
00803              OR                                                   EL694
00804          LA-STATUS-COMPLETED                                      EL694
00805              OR                                                   EL694
00806          LA-STATUS-TO-BE-PURGED                                   EL694
00807              OR                                                   EL694
00808          LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL694
00809          GO TO 7510-READ-NEXT.                                    EL694
00810                                                                   EL694
00811      IF  PI-694-PRINT-BY-PROCESSOR                                EL694
00812              AND                                                  EL694
00813          LA-PROCESSOR-CD NOT EQUAL PI-694-PRINT-PROCESSOR         EL694
00814          GO TO 7510-READ-NEXT.                                    EL694
00815                                                                   EL694
00816      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL694
00817              AND                                                  EL694
00818          LA-PROCESSOR-CD NOT EQUAL PI-694-PRINT-PROCESSOR         EL694
00819          GO TO 7510-READ-NEXT.                                    EL694
00820                                                                   EL694
00821      IF  W-CHECK-CONTROL GREATER THAN ZEROS                       EL694
00822          IF  LA-FILLER NOT EQUAL 'CK'                                CL**2
00823              GO TO 7510-READ-NEXT                                    CL**2
00824          ELSE                                                        CL**2
00825              IF  W-CHECK-CONTROL NOT EQUAL LA-QUE-CONTROL-A6         CL**2
00826                  GO TO 7510-READ-NEXT.                               CL**2
00827                                                                   EL694
00828      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL694
00829          IF  LA-FILLER EQUAL 'CK'                                    CL**2
00830              IF  W-CHECK-CONTROL NOT GREATER THAN ZEROS              CL**2
00831                  GO TO 7510-READ-NEXT                                CL**2
00832              ELSE                                                    CL**2
00833                  IF  W-CHECK-CONTROL NOT EQUAL LA-QUE-CONTROL-A6     CL**2
00834                      GO TO 7510-READ-NEXT                            CL**2
00835                  ELSE                                                CL**2
00836                      NEXT SENTENCE                                   CL**2
00837          ELSE                                                        CL**2
00838              GO TO 7510-READ-NEXT.                                   CL**2
00839                                                                   EL694
00840      IF  W-ENTRY GREATER THAN SPACES                              EL694
00841          IF  LA-FILLER EQUAL 'CK'                                    CL**2
00842              GO TO 7510-READ-NEXT                                    CL**2
00843          ELSE                                                        CL**2
00844              IF  W-ENTRY NOT EQUAL LA-ENTRY-A6                       CL**2
00845                  GO TO 7510-READ-NEXT.                               CL**2
00846                                                                   EL694
00847      IF  W-FORM GREATER THAN SPACES                               EL694
00848              AND                                                  EL694
00849          W-FORM NOT EQUAL LA-FORM-A3                              EL694
00850          GO TO 7510-READ-NEXT.                                    EL694
00851                                                                   EL694
00852      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL694
00853              AND                                                  EL694
00854          W-FORM NOT EQUAL LA-FORM-A3                              EL694
00855          GO TO 7510-READ-NEXT.                                    EL694
00856                                                                   EL694
00857      IF  W-COUNT-BY-KEY                                           EL694
00858          IF  W-COUNT-BY-CARRIER                                   EL694
00859              IF  LA-CARRIER-A2 EQUAL W-CARRIER                    EL694
00860                  NEXT SENTENCE                                    EL694
00861              ELSE                                                 EL694
00862                  GO TO 7510-READ-NEXT                             EL694
00863          ELSE                                                     EL694
00864              IF  W-COUNT-BY-GROUPING                              EL694
00865                                                                   EL694
00866                  IF  LA-CARRIER-A2 EQUAL W-CARRIER                EL694
00867                          AND                                      EL694
00868                      LA-GROUPING-A2 EQUAL W-GROUPING              EL694
00869                      NEXT SENTENCE                                EL694
00870                  ELSE                                             EL694
00871                      GO TO 7510-READ-NEXT                         EL694
00872              ELSE                                                 EL694
00873                  IF  W-COUNT-BY-STATE                             EL694
00874                                                                   EL694
00875                      IF  LA-CARRIER-A2 EQUAL W-CARRIER            EL694
00876                              AND                                  EL694
00877                          LA-GROUPING-A2 EQUAL W-GROUPING          EL694
00878                              AND                                  EL694
00879                          LA-STATE-A2 EQUAL W-STATE                EL694
00880                          NEXT SENTENCE                            EL694
00881                      ELSE                                         EL694
00882                          GO TO 7510-READ-NEXT                     EL694
00883                  ELSE                                             EL694
00884                      IF  W-COUNT-BY-ACCOUNT                       EL694
00885                                                                   EL694
00886                          IF  LA-CARRIER-A2 EQUAL W-CARRIER        EL694
00887                                  AND                              EL694
00888                              LA-GROUPING-A2 EQUAL W-GROUPING      EL694
00889                                  AND                              EL694
00890                              LA-STATE-A2 EQUAL W-STATE            EL694
00891                                  AND                              EL694
00892                              LA-ACCOUNT-A2 EQUAL W-ACCOUNT        EL694
00893                              NEXT SENTENCE                        EL694
00894                          ELSE                                     EL694
00895                              GO  TO 7510-READ-NEXT.               EL694
00896                                                                   EL694
00897      IF  OPTIONI = '3'                                            EL694
00898          IF  PI-694-PRINT-DATE-BIN = LOW-VALUES                   EL694
00899              IF  LA-INITIAL-PRINT-DATE = LOW-VALUES               EL694
00900                  IF  LA-STATUS-ON-HOLD                            EL694
00901                      ADD 1       TO W-HOLD-PENDING-CTR            EL694
00902                      ADD LA-NO-OF-COPIES                          EL694
00903                                  TO W-COPIES-CTR                  EL694
00904                      GO TO 7510-READ-NEXT                         EL694
00905                  ELSE                                             EL694
00906                      ADD 1       TO W-INITIAL-PENDING-CTR         EL694
00907                      ADD LA-NO-OF-COPIES                          EL694
00908                                  TO W-COPIES-CTR                  EL694
00909                      GO TO 7510-READ-NEXT                         EL694
00910              ELSE                                                 EL694
00911                  GO TO 7510-READ-NEXT                             EL694
00912          ELSE                                                     EL694
00913              IF  LA-CREATION-DATE EQUAL PI-694-PRINT-DATE-BIN     EL694
00914                  IF  LA-INITIAL-PRINT-DATE = LOW-VALUES           EL694
00915                      IF  LA-STATUS-ON-HOLD                        EL694
00916                          ADD 1   TO W-HOLD-PENDING-CTR            EL694
00917                          ADD LA-NO-OF-COPIES                      EL694
00918                                  TO W-COPIES-CTR                  EL694
00919                          GO TO 7510-READ-NEXT                     EL694
00920                      ELSE                                         EL694
00921                          ADD 1   TO W-INITIAL-PENDING-CTR         EL694
00922                          ADD LA-NO-OF-COPIES                      EL694
00923                                  TO W-COPIES-CTR                  EL694
00924                          GO TO 7510-READ-NEXT                     EL694
00925                  ELSE                                             EL694
00926                      GO TO 7510-READ-NEXT                         EL694
00927              ELSE                                                 EL694
00928                  GO TO 7510-READ-NEXT.                            EL694
00929                                                                   EL694
00930      IF  LA-RESEND-DATE   EQUAL LOW-VALUES                        EL694
00931              OR                                                   EL694
00932          LA-INITIAL-PRINT-DATE = LOW-VALUES                       EL694
00933          GO TO 7510-READ-NEXT.                                    EL694
00934                                                                   EL694
00935      MOVE SPACES                 TO W-DATE-FOUND-IND.             EL694
00936                                                                   EL694
9          PERFORM 7600-CHECK-RESEND-DATES THRU 7600-EXIT
00948                                                                   EL694
00949      GO TO 7510-READ-NEXT.                                        EL694
00950 *    GO TO 7505-RESET-HANDLE.                                     EL694
00951                                                                   EL694
00952  7599-EXIT.                                                       EL694
00953       EXIT.                                                       EL694
00954                                  EJECT                            EL694
00955  7600-CHECK-RESEND-DATES.                                         EL694
00956                                                                   EL694
00957      IF  LA-RESEND-DATE = LOW-VALUES
00958          GO TO 7600-EXIT.                                         EL694
00959                                                                   EL694
00960      IF  PI-694-PRINT-DATE-BIN GREATER THAN LOW-VALUES            EL694
00961          IF  LA-RESEND-DATE =
00962                  PI-694-PRINT-DATE-BIN                            EL694
00963              MOVE 'Y'            TO W-DATE-FOUND-IND              EL694
00964              IF  LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE       EL694
00967                                                                   EL694
00968                  IF  LA-STATUS-ON-HOLD                            EL694
00969                      ADD +1      TO W-HOLD-PENDING-CTR            EL694
00970                                     W-RESEND-PENDING-CTR-TOTAL    EL694
00971                      ADD LA-NO-OF-COPIES                          EL694
00972                                  TO W-COPIES-CTR                  EL694
00973                  ELSE                                             EL694
00974                      ADD +1  TO W-RESEND-PENDING-CTR
00975                                 W-RESEND-PENDING-CTR-TOTAL        EL694
00976                      ADD LA-NO-OF-COPIES                          EL694
00977                                  TO W-COPIES-CTR                  EL694
00978              ELSE                                                 EL694
00979                  GO TO 7600-EXIT                                  EL694
00980          ELSE                                                     EL694
00981              GO TO 7600-EXIT                                      EL694
00982      ELSE                                                         EL694
00983          IF  LA-RESEND-DATE NOT GREATER THAN
00984                  W-CURRENT-DATE-SAVE                              EL694
00985              MOVE 'Y'            TO W-DATE-FOUND-IND              EL694
00986                                                                   EL694
00987              IF  LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE       EL694
00990                  IF  LA-STATUS-ON-HOLD                            EL694
00991                      ADD +1      TO W-HOLD-PENDING-CTR            EL694
00992                                     W-RESEND-PENDING-CTR-TOTAL    EL694
00993                      ADD LA-NO-OF-COPIES                          EL694
00994                                  TO W-COPIES-CTR                  EL694
00995                  ELSE                                             EL694
00996                      ADD LA-NO-OF-COPIES                          EL694
00997                                  TO W-COPIES-CTR                  EL694
00998                      ADD +1 TO W-RESEND-PENDING-CTR
00999                                W-RESEND-PENDING-CTR-TOTAL.        EL694
01000                                                                   EL694
01001  7600-EXIT.                                                       EL694
01002       EXIT.                                                       EL694
01003                                  EJECT                            EL694
01004  7800-START-PRINT.                                                EL694
01005                                                                   EL694
01006      MOVE PI-COMPANY-CD          TO W-ARCH-CO.                    EL694
01007                                                                   EL694
01008      EXEC CICS HANDLE CONDITION                                   EL694
01009           TERMIDERR   (8020-TERMID-ERROR)                         EL694
01010           TRANSIDERR  (8030-TRANS-ERROR)                          EL694
01011           END-EXEC.                                               EL694
01012                                                                   EL694
01013  7820-START.                                                      EL694
01014                                                                   EL694
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**4
01016 *        MOVE EIBTRMID       TO PI-694-PRINT-ID                      CL**4
01017          EXEC CICS START                                             CL**4
01018               INTERVAL    (0)                                        CL**4
01019               TRANSID     (W-PRINT-TRANS)                            CL**4
01020               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**4
01021               LENGTH      (PI-COMM-LENGTH)                           CL**4
01022 *             TERMID      (PI-694-PRINT-ID)                          CL**4
01023          END-EXEC                                                    CL**4
01024      ELSE                                                            CL**4
01025          EXEC CICS START                                             CL**4
01026               INTERVAL    (0)                                        CL**4
01027               TRANSID     (W-PRINT-TRANS)                            CL**4
01028               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**4
01029               LENGTH      (PI-COMM-LENGTH)                           CL**4
01030               TERMID      (PI-694-PRINT-ID)                          CL**4
01031          END-EXEC.                                                   CL**4
01032                                                                   EL694
01033      IF  OPTIONI = '5'                                            EL694
01034          MOVE ER-0411            TO EMI-ERROR                     EL694
01035      ELSE                                                         EL694
01036          MOVE ER-0189            TO EMI-ERROR.                    EL694
01037                                                                   EL694
01038      MOVE -1                     TO OPTIONL.                      EL694
01039      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL694
01040      GO TO 8200-SEND-DATAONLY.                                    EL694
01041                                  EJECT                            EL694
01042  7900-GET-CNTL-INFO.                                              EL694
01043                                                                   EL694
01044      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL**4
01045      IF  PRINTERL NOT = ZEROS                                     EL694
01046          MOVE PRINTERI           TO PI-694-PRINT-ID               EL694
01047                                     PI-ALT-DMD-PRT-ID                CL**4
01048      ELSE                                                         EL694
01049          IF  PI-PROCESSOR-PRINTER NOT EQUAL SPACES                EL694
01050                  AND                                              EL694
01051              PI-PROCESSOR-PRINTER NOT EQUAL LOW-VALUES            EL694
01052              MOVE PI-PROCESSOR-PRINTER                            EL694
01053                                  TO PI-694-PRINT-ID               EL694
01054          ELSE                                                     EL694
01055              EXEC CICS HANDLE CONDITION                           EL694
01056                   NOTOPEN (8040-CNTL-NOT-OPEN)                    EL694
01057                   NOTFND  (7990-NOT-FOUND)                        EL694
01058              END-EXEC                                             EL694
01059              MOVE SPACES         TO W-CNTL-KEY                    EL694
01060              MOVE PI-COMPANY-ID  TO W-CNTL-COMPANY-ID             EL694
01061              MOVE '1'            TO W-CNTL-RECORD-TYPE            EL694
01062              MOVE ZEROS          TO W-CNTL-SEQ                    EL694
01063              PERFORM 7910-READ-CNTL-INFO THRU 7910-EXIT           EL694
01064              MOVE CF-FORMS-PRINTER-ID                             EL694
01065                                  TO PI-694-PRINT-ID               EL694
01066              GO TO 7900-CONTINUE.                                 EL694
01067                                                                   EL694
01068      MOVE SPACES                 TO W-CNTL-KEY.                   EL694
01069      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.            EL694
01070      MOVE '1'                    TO W-CNTL-RECORD-TYPE.           EL694
01071      MOVE ZEROS                  TO W-CNTL-SEQ.                   EL694
01072                                                                   EL694
01073      PERFORM 7910-READ-CNTL-INFO THRU 7910-EXIT.                  EL694
01074                                                                   EL694
01075  7900-CONTINUE.                                                   EL694
01076                                                                   EL694
01077      MOVE CF-CREDIT-START-ARCH-NUM                                EL694
01078                                  TO PI-694-STARTING-ARCH-NO.      EL694
01079                                                                   EL694
01080      GO TO 7999-EXIT.                                             EL694
01081                                                                   EL694
01082  7910-READ-CNTL-INFO.                                             EL694
01083                                                                   EL694
01084      EXEC CICS READ                                               EL694
01085           DATASET (W-CNTL-ID)                                     EL694
01086           SET     (ADDRESS OF CONTROL-FILE)                          CL**3
01087           RIDFLD  (W-CNTL-KEY)                                    EL694
01088      END-EXEC.                                                    EL694
01089                                                                   EL694
01090  7910-EXIT.                                                       EL694
01091      EXIT.                                                        EL694
01092                                                                   EL694
01093  7990-NOT-FOUND.                                                  EL694
01094                                                                   EL694
01095      MOVE ER-0190                TO EMI-ERROR.                    EL694
01096      MOVE -1                     TO OPTIONL.                      EL694
01097      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   EL694
01098                                                                   EL694
01099  7999-EXIT.                                                       EL694
01100      EXIT.                                                        EL694
01101                                  EJECT                            EL694
01102  8000-DEEDIT.                                                     EL694
01103                                                                   EL694
01104      EXEC CICS BIF  DEEDIT                                        EL694
01105           FIELD(W-DEEDIT-FIELD)                                   EL694
01106           LENGTH(15)                                              EL694
01107      END-EXEC.                                                    EL694
01108                                  EJECT                            EL694
01109  8020-TERMID-ERROR.                                               EL694
01110                                                                   EL694
01111      MOVE ER-0412                TO EMI-ERROR                     EL694
01112      GO TO 8080-OPEN-ERROR.                                       EL694
01113                                                                   EL694
01114  8030-TRANS-ERROR.                                                EL694
01115                                                                   EL694
01116      MOVE ER-0413                TO EMI-ERROR                     EL694
01117      GO TO 8080-OPEN-ERROR.                                       EL694
01118                                                                   EL694
01119  8040-CNTL-NOT-OPEN.                                              EL694
01120                                                                   EL694
01121      MOVE ER-0042                TO EMI-ERROR.                    EL694
01122      GO TO 8080-OPEN-ERROR.                                       EL694
01123                                                                   EL694
01124  8050-ARCH-NOT-OPEN.                                              EL694
01125                                                                   EL694
01126      MOVE ER-7357                TO EMI-ERROR.                    EL694
01127      GO TO 8080-OPEN-ERROR.                                       EL694
01128                                                                   EL694
01129  8080-OPEN-ERROR.                                                 EL694
01130                                                                   EL694
01131      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL694
01132      MOVE -1                     TO OPTIONL.                      EL694
01133      GO TO 8200-SEND-DATAONLY.                                    EL694
01134                                  EJECT                            EL694
01135  8100-SEND-INITIAL-MAP.                                           EL694
01136                                                                   EL694
01137      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.                EL694
01138                                                                   EL694
01139      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL694
01140      MOVE -1                     TO OPTIONL.                      EL694
01141                                                                   EL694
01142      EXEC CICS SEND                                               EL694
01143          MAP    (W-MAP)                                           EL694
01144          MAPSET (W-MAPSET)                                        EL694
01145          FROM   (EL694AO)                                         EL694
01146          ERASE                                                    EL694
01147          FREEKB                                                   EL694
01148          CURSOR                                                   EL694
01149      END-EXEC.                                                    EL694
01150                                                                   EL694
01151      GO TO 9000-RETURN-TRANS.                                     EL694
01152                                                                   EL694
01153  8100-EXIT.                                                       EL694
01154      EXIT.                                                        EL694
01155                                  EJECT                            EL694
01156  8200-SEND-DATAONLY.                                              EL694
01157                                                                   EL694
01158      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.                EL694
01159                                                                   EL694
01160      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL694
01161                                                                   EL694
01162      EXEC CICS SEND                                               EL694
01163          MAP    (W-MAP)                                           EL694
01164          MAPSET (W-MAPSET)                                        EL694
01165          FROM   (EL694AO)                                         EL694
01166          DATAONLY                                                 EL694
01167          FREEKB                                                   EL694
01168          CURSOR                                                   EL694
01169          ERASEAUP                                                 EL694
01170      END-EXEC.                                                    EL694
01171                                                                   EL694
01172      GO TO 9000-RETURN-TRANS.                                     EL694
01173                                                                   EL694
01174  8200-EXIT.                                                       EL694
01175      EXIT.                                                        EL694
01176                                  EJECT                            EL694
01177  8300-SEND-TEXT.                                                  EL694
01178                                                                   EL694
01179      EXEC CICS SEND TEXT                                          EL694
01180           FROM    (LOGOFF-TEXT)                                   EL694
01181           LENGTH  (LOGOFF-LENGTH)                                 EL694
01182           ERASE                                                   EL694
01183           FREEKB                                                  EL694
01184      END-EXEC.                                                    EL694
01185                                                                   EL694
01186      EXEC CICS RETURN                                             EL694
01187      END-EXEC.                                                    EL694
01188                                  EJECT                            EL694
01189  9000-RETURN-TRANS.                                               EL694
01190                                                                   EL694
01191      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL694
01192      MOVE W-MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.         EL694
01193                                                                   EL694
01194      EXEC CICS RETURN                                             EL694
01195          TRANSID  (W-TRANSACTION)                                 EL694
01196          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL694
01197          LENGTH   (PI-COMM-LENGTH)                                EL694
01198      END-EXEC.                                                    EL694
01199                                                                   EL694
01200  9000-EXIT.                                                       EL694
01201      EXIT.                                                        EL694
01202                                  EJECT                            EL694
01203  9400-XCTL.                                                       EL694
01204                                                                   EL694
01205      EXEC CICS XCTL                                               EL694
01206          PROGRAM  (W-CALL-PGM)                                    EL694
01207          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL694
01208          LENGTH   (PI-COMM-LENGTH)                                EL694
01209      END-EXEC.                                                    EL694
01210                                                                   EL694
01211  9400-EXIT.                                                       EL694
01212      EXIT.                                                        EL694
01213                                  EJECT                            EL694
01214  9500-LINK-DATE-CONVERT.                                          EL694
01215                                                                   EL694
01216      EXEC CICS LINK                                               EL694
01217           PROGRAM    ('ELDATCV')                                  EL694
01218           COMMAREA   (DATE-CONVERSION-DATA)                       EL694
01219           LENGTH     (DC-COMM-LENGTH)                             EL694
01220      END-EXEC.                                                    EL694
01221                                                                   EL694
01222  9500-EXIT.                                                       EL694
01223      EXIT.                                                        EL694
01224                                  EJECT                            EL694
01225  9600-FORMAT-DATE-TIME.                                           EL694
01226                                                                   EL694
01227      MOVE PI-COMPANY-ID          TO COMPANYO.                     EL694
01228      MOVE W-CURRENT-DATE-EDIT    TO DATEO.                        EL694
01229                                                                   EL694
01230      EXEC CICS ASKTIME                                            EL694
01231      END-EXEC.                                                    EL694
01232                                                                   EL694
01233      MOVE EIBTIME                TO W-TIME-IN.                    EL694
01234      MOVE W-TIME-OUT             TO TIMEO.                        EL694
01235      MOVE W-MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.         EL694
01236                                                                   EL694
01237  9600-EXIT.                                                       EL694
01238      EXIT.                                                        EL694
01239                                  EJECT                            EL694
01240  9700-PGMID-ERROR.                                                EL694
01241                                                                   EL694
01242      EXEC CICS  HANDLE CONDITION                                  EL694
01243          PGMIDERR  (8300-SEND-TEXT)                               EL694
01244          END-EXEC.                                                EL694
01245                                                                   EL694
01246      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.           EL694
01247      MOVE ' '                    TO PI-ENTRY-CD-1.                EL694
01248      MOVE W-XCTL-005             TO W-CALL-PGM                    EL694
01249                                     LOGOFF-PGM.                   EL694
01250      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL694
01251                                                                   EL694
01252      PERFORM 9400-XCTL THRU 9400-EXIT.                            EL694
01253                                                                   EL694
01254  9700-EXIT.                                                       EL694
01255      EXIT.                                                        EL694
01256                                  EJECT                            EL694
01257  9900-ERROR-FORMAT.                                               EL694
01258                                                                   EL694
01259      IF  NOT EMI-ERRORS-COMPLETE                                  EL694
01260              AND                                                  EL694
01261          EMI-ERROR NOT EQUAL W-LAST-ERROR                         EL694
01262          MOVE W-LINK-001          TO W-CALL-PGM                   EL694
01263          EXEC CICS LINK                                           EL694
01264              PROGRAM    (W-CALL-PGM)                              EL694
01265              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL694
01266              LENGTH     (EMI-COMM-LENGTH)                         EL694
01267          END-EXEC                                                 EL694
01268          MOVE EMI-ERROR TO W-LAST-ERROR.                          EL694
01269                                                                   EL694
01270  9900-EXIT.                                                       EL694
01271      EXIT.                                                        EL694
01272                                  EJECT                            EL694
01273  9800-ABEND.                                                      EL694
01274                                                                   EL694
01275      MOVE W-LINK-004             TO W-CALL-PGM.                   EL694
01276      MOVE DFHEIBLK               TO EMI-LINE1                     EL694
01277                                                                   EL694
01278      EXEC CICS  LINK                                              EL694
01279          PROGRAM   (W-CALL-PGM)                                   EL694
01280          COMMAREA  (EMI-LINE1)                                    EL694
01281          LENGTH    (72)                                           EL694
01282          END-EXEC.                                                EL694
01283                                                                   EL694
01284      PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT.                   EL694
01285      PERFORM 9000-RETURN-TRANS  THRU 9000-EXIT.                   EL694
01286                                                                   EL694
01287  9800-EXIT.                                                       EL694
01288      EXIT.                                                        EL694
01289                                  EJECT                            EL694
01290  9910-INITIALIZE-SECURITY.                                        EL694
01291                                                                   EL694
01292      IF  PI-PROCESSOR-ID EQUAL 'LGXX'                             EL694
01293          MOVE 'Y'                TO PI-DISPLAY-CAP                EL694
01294                                         PI-MODIFY-CAP             EL694
01295      ELSE                                                         EL694
01296          EXEC CICS READQ TS                                       EL694
01297              QUEUE  (PI-SECURITY-TEMP-STORE-ID)                   EL694
01298              INTO   (SECURITY-CONTROL)                            EL694
01299              LENGTH (SC-COMM-LENGTH)                              EL694
01300              ITEM   (1)                                           EL694
01301          END-EXEC                                                 EL694
01302          MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)                EL694
01303                                  TO PI-DISPLAY-CAP                EL694
01304          MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)                 EL694
01305                                  TO PI-MODIFY-CAP                 EL694
01306          IF  NOT DISPLAY-CAP                                      EL694
01307              MOVE 'READ'         TO SM-READ                       EL694
01308              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL694
01309              MOVE ER-9097        TO EMI-ERROR                     EL694
01310              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL694
01311              PERFORM 8100-SEND-INITIAL-MAP THRU 8100-EXIT         EL694
01312              PERFORM 9000-RETURN-TRANS THRU 9000-EXIT.            EL694
01313                                                                   EL694
01314  9910-EXIT.                                                       EL694
01315      EXIT.                                                        EL694
01316                                  EJECT                            EL694
01317  9995-SECURITY-VIOLATION.                                         EL694
01318                                                                   EL694
01319      MOVE EIBDATE          TO SM-JUL-DATE.                        EL694
01320      MOVE EIBTRMID         TO SM-TERMID.                          EL694
01321      MOVE W-THIS-PGM       TO SM-PGM.                             EL694
01322      MOVE EIBTIME          TO W-TIME-IN.                          EL694
01323      MOVE W-TIME-OUT       TO SM-TIME.                            EL694
01324      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.                    EL694
01325                                                                   EL694
01326      EXEC CICS LINK                                               EL694
01327           PROGRAM  ('EL003')                                      EL694
01328           COMMAREA (SECURITY-MESSAGE)                             EL694
01329           LENGTH   (80)                                           EL694
01330      END-EXEC.                                                    EL694
01331                                                                   EL694
01332  9995-EXIT.                                                       EL694
01333      EXIT.                                                        EL694
01334                                  EJECT                            EL694
01335  9999-GOBACK.                                                     EL694
01336                                                                   EL694
01337      GOBACK.                                                      EL694
01338                                                                   EL694
01339  9999-EXIT.                                                       EL694
01340      EXIT.                                                        EL694
01341                                                                   EL694
