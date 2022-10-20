00001  IDENTIFICATION DIVISION.                                         07/11/96
00002                                                                   EL6942
00003  PROGRAM-ID.                 EL6942.                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 02/12/96 10:03:45.                    CL**6
00007 *                            VMOD=2.007                              CL**7
00008 *                                                                 EL6942
00008 *                                                                 EL6942
00009 *AUTHOR.           LOGIC,INC.                                        CL**6
00010 *                  DALLAS,TEXAS.                                     CL**6
00011                                                                   EL6942
00012 *DATE-COMPILED.                                                      CL**6
00013                                                                   EL6942
00014 *SECURITY.   *****************************************************   CL**6
00015 *            *                                                   *   CL**6
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOCIC, INC.     *   CL**7
00017 *            *                                                   *   CL**6
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00021 *            *                                                   *   CL**6
00022 *            *****************************************************   CL**6
00023                                                                   EL6942
00024 *REMARKS. TRANSACTION EXM5 - LETTER PRINTER                       EL6942
00025 *        THIS PROGRAM IS USED TO PRINT THE STORED LETTERS AND     EL6942
00026 *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.    EL6942
00027                                                                   EL6942
00028 *        PRINT INITIAL LETTERS   CODE-1 = 1                       EL6942
00029 *                                CODE-2 = 1                       EL6942
00030                                                                   EL6942
00031 *        PRINT FOLLOW-UP LETTERS CODE-1 = 1                       EL6942
00032 *                                CODE-2 = 2                       EL6942
00033                                                                   EL6942
00034 *        RE-PRINT LETTERS        CODE-1 = 0                       EL6942
00035 *                                CODE-2 = 3                       EL6942
00036                                                                   EL6942
00037 *        PRINT ADDRESS LABELS    CODE-1 = 0                       EL6942
00038 *                                CODE-2 = 2                       EL6942

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

00040  ENVIRONMENT DIVISION.                                            EL6942
00041  DATA DIVISION.                                                   EL6942
00042  WORKING-STORAGE SECTION.                                         EL6942
00043  77  FILLER  PIC  X(32) VALUE '********************************'. EL6942
00044  77  FILLER  PIC  X(32) VALUE '*   EL6942 WORKING STORAGE     *'. EL6942
00045  77  FILLER  PIC  X(32) VALUE '********* V/M 2.007 ************'.    CL**7
00046                                                                   EL6942
00047  01  W-PROGRAM-CONSTANTS.                                         EL6942
00048      12  FILLER                  PIC  X(18)                       EL6942
00049                                  VALUE 'PROGRAM CONSTANTS:'.      EL6942
00050                                                                   EL6942
00051      12  W-ZEROS                 PIC S9(04)  COMP VALUE +0.       EL6942
00052                                                                   EL6942
00053      12  W-ARCH-ID               PIC  X(08)  VALUE 'ERARCH'.      EL6942
00054      12  W-ARCH3-ID              PIC  X(08)  VALUE 'ERARCH3'.     EL6942
00055      12  W-ARCH4-ID              PIC  X(08)  VALUE 'ERARCH4'.     EL6942
00056      12  W-ARCH5-ID              PIC  X(08)  VALUE 'ERARCH5'.     EL6942
00057      12  W-ARCH6-ID              PIC  X(08)  VALUE 'ERARCH6'.     EL6942
00058      12  W-ARCT-ID               PIC  X(08)  VALUE 'ERARCT'.      EL6942
00059      12  W-CNTL-ID               PIC  X(08)  VALUE 'ELCNTL'.      EL6942
00060                                                                   EL6942
00061  01  W-PROGRAM-WORK-AREA.                                         EL6942
00062      12  THIS-PGM                PIC  X(8)  VALUE 'EL6942'.          CL**7
00063      12  FILLER                  PIC  X(18)                       EL6942
00064                                  VALUE 'PROGRAM WORK AREA:'.      EL6942
00065                                                                   EL6942
00066      12  W-ASKTIME-CTR           PIC S9(04)  COMP.                EL6942
00067      12  W-ARCHIVE-SAVE          PIC S9(08)  COMP.                EL6942
00068      12  W-COPIES                PIC  9.                          EL6942
00069      12  W-DELAY-INTERVAL        PIC S9(07)  COMP-3 VALUE +2.     EL6942
00070      12  W-NDX                   PIC S9(04)  COMP   VALUE +0.     EL6942
00071      12  W-NUM-OF-TEXT-RECORDS   PIC S9(04)  COMP   VALUE +0.     EL6942
00072      12  W-NUMBER-OF-LINES       PIC S9(04)  COMP.                EL6942
00073      12  W-RECORD-COUNT          PIC S9(04)         VALUE +0.     EL6942
00074      12  W-SAVE-ARCH-NO          PIC S9(08)  COMP   VALUE +0.     EL6942
00075      12  W-SKIP                  PIC  9(02).                      EL6942
00076      12  W-SUB                   PIC S9(04)  COMP.                EL6942
00077      12  W-LETTER-TOTALS         PIC  9(07)  COMP-3 VALUE 0.      EL6942
00078                                                                   EL6942
00079      12  W-ASTERISK-LINE1.                                           CL**6
00080          16  FILLER              PIC  X(78)  VALUE ALL '*'.       EL6942
00081      12  W-ASTERISK-LINE.                                         EL6942
00082          16  FILLER              PIC  X(01)  VALUE SPACES.        EL6942
00083          16  FILLER              PIC  X(78)  VALUE ALL '*'.       EL6942
00084      12  W-CALL-PGM              PIC  X(08).                      EL6942
00085      12  W-CURRENT-SAVE          PIC  X(02).                      EL6942
00086      12  W-ERROR-LINE            PIC  X(80).                      EL6942
00087      12  W-LAST-RESENT-PRINT-DATE                                 EL6942
00088                                  PIC  X(02)  VALUE SPACES.        EL6942
00089                                                                   EL6942
00090      12  W-LABEL-HOLD-AREA.                                       EL6942
00091          16  W-LABEL-LINES OCCURS 6 TIMES INDEXED BY W-L-NDX.     EL6942
00092              20  W-LABEL-ZIP.                                     EL6942
00093                  24  W-LABEL-1ST-ZIP                              EL6942
00094                                  PIC  X(04).                      EL6942
00095                  24  W-LABEL-2ND-ZIP                              EL6942
00096                                  PIC  X(05).                      EL6942
00097              20  FILLER          PIC  X(12).                      EL6942
00098              20  WS-LAST-ZIP.                                     EL6942
00099                  24  WS-LAST-1ST-ZIP                              EL6942
00100                                  PIC  X(04).                      EL6942
00101                  24  WS-LAST-2ND-ZIP                              EL6942
00102                                  PIC  X(05).                      EL6942
00103                                                                   EL6942
00104      12  W-SAVE-CURRENT-DATE     PIC  X(08)  VALUE SPACES.        EL6942
00105      12  W-SAVE-CURRENT-BIN-DATE PIC  X(02)  VALUE SPACES.        EL6942
00106      12  W-SAVE-LETTER-ARCHIVE   PIC X(250)  VALUE SPACES.        EL6942
00107      12  W-TOTAL-LINE.                                            EL6942
00108          20  FILLER              PIC  X(01)  VALUE SPACES.        EL6942
00109          20  FILLER              PIC  X(20)                       EL6942
00110              VALUE 'PROCESS COMPLETED.  '.                        EL6942
00111          20  W-TOTAL-LINE-DESC   PIC  X(26)                          CL**3
00112              VALUE 'LETTERS PRINTED TOTAL   - '.                     CL**3
00113          20  W-TOTAL-LETTERS     PIC Z,ZZZ,ZZ9.                   EL6942
00114      12  W-WORKING-RESEND-DATE   PIC  X(02)  VALUE SPACES.        EL6942
00115      12  W-LABEL-LINE-DESC   PIC  X(26)                              CL**3
00116              VALUE 'LABELS PRINTED TOTAL    - '.                     CL**3
00117                                                                   EL6942
00118  01  W-PROGRAM-KEYS.                                              EL6942
00119      12  FILLER                  PIC  X(13)                       EL6942
00120                                  VALUE 'PROGRAM KEYS:'.           EL6942
00121      12  W-ARCH-KEY.                                              EL6942
00122          20  W-ARCH-COMPANY-CD   PIC  X(01).                      EL6942
00123          20  W-ARCH-NUMBER       PIC S9(08)     COMP.             EL6942
00124                                                                   EL6942
00125      12  W-ARCH3-KEY.                                             EL6942
00126          16  W-ARCH3-COMPANY-CD  PIC  X(01).                      EL6942
00127          16  W-ARCH3-FORM        PIC  X(04).                      EL6942
00128          16  W-ARCH3-CARRIER     PIC  X(01).                      EL6942
00129          16  W-ARCH3-GROUPING    PIC  X(06).                      EL6942
00130          16  W-ARCH3-STATE       PIC  X(02).                      EL6942
00131          16  W-ARCH3-ACCOUNT     PIC  X(10).                      EL6942
00132          16  W-ARCH3-ARCHIVE-NO  PIC S9(08)  COMP.                EL6942
00133                                                                   EL6942
00134      12  W-ARCH4-KEY.                                             EL6942
00135          16  W-ARCH4-COMPANY-CD  PIC  X(01).                      EL6942
00136          16  W-ARCH4-PROCESSOR-CD                                 EL6942
00137                                  PIC  X(04).                      EL6942
00138          16  W-ARCH4-CARRIER     PIC  X(01).                      EL6942
00139          16  W-ARCH4-GROUPING    PIC  X(06).                      EL6942
00140          16  W-ARCH4-STATE       PIC  X(02).                      EL6942
00141          16  W-ARCH4-ACCOUNT     PIC  X(10).                      EL6942
00142          16  W-ARCH4-ARCHIVE-NO  PIC S9(08)    COMP.              EL6942
00143                                                                   EL6942
00144      12  W-ARCH5-KEY.                                             EL6942
00145          16  W-ARCH5-COMPANY-CD  PIC  X(01).                      EL6942
00146          16  W-ARCH5-CARRIER     PIC  X(01).                      EL6942
00147          16  W-ARCH5-GROUPING    PIC  X(06).                      EL6942
00148          16  W-ARCH5-STATE       PIC  X(02).                      EL6942
00149          16  W-ARCH5-ACCOUNT     PIC  X(10).                      EL6942
00150          16  W-ARCH5-ARCHIVE-NO  PIC S9(08)  COMP.                EL6942
00151                                                                   EL6942
00152      12  W-ARCH6-KEY.                                             EL6942
00153          16  W-ARCH6-COMPANY-CD  PIC  X(01).                      EL6942
00154          16  W-ARCH6-ENTRY.                                       EL6942
00155              20  W-ARCH6-FILLER  PIC  X(02).                      EL6942
00156              20  W-ARCH6-CONTROL PIC S9(08)  COMP.                EL6942
00157          16  W-ARCH6-ARCHIVE-NO  PIC S9(08)  COMP.                EL6942
00158                                                                   EL6942
00159      12  W-ARCT-KEY.                                              EL6942
00160          16  W-ARCT-PARTIAL-KEY.                                  EL6942
00161              20  W-ARCT-COMPANY-CD                                EL6942
00162                                  PIC  X(01).                      EL6942
00163              20  W-ARCT-NUMBER   PIC S9(08)     COMP.             EL6942
00164          16  W-ARCT-REC-ID       PIC  X(01).                      EL6942
00165          16  W-ARCT-SEQ          PIC S9(04)     COMP VALUE +0.    EL6942
00166                                                                   EL6942
00167      12  W-CNTL-KEY.                                              EL6942
00168          16  W-CNTL-COMPANY-ID   PIC  X(03).                      EL6942
00169          16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '1'.           EL6942
00170          16  W-CNTL-GENL.                                         EL6942
00171              20  W-CNTL-GEN1     PIC  X(02)  VALUE SPACES.        EL6942
00172              20  W-CNTL-GEN2.                                     EL6942
00173                  24 W-CNTL-GEN3  PIC  X(01)  VALUE SPACES.        EL6942
00174                  24 W-CNTL-GEN4  PIC  X(01)  VALUE SPACES.        EL6942
00175          16  W-CNTL-SEQ          PIC S9(04)  VALUE +0    COMP.    EL6942
00176                                                                   EL6942
00177  01  W-PROGRAM-SWITCES.                                           EL6942
00178      12  FILLER                  PIC  X(17)                       EL6942
00179                                  VALUE 'PROGRAM SWITCHES:'.       EL6942
00180                                                                   EL6942
00181      12  W-ENDBR-SW              PIC  X(01)          VALUE ' '.   EL6942
00182          88  W-ENDBR                                 VALUE 'Y'.   EL6942
00183      12  W-FIRST-FORM-SW         PIC  X(01)          VALUE ' '.   EL6942
00184          88  W-THIS-IS-FIRST-FORM                    VALUE ' '.   EL6942
00185          88  W-THIS-IS-NOT-FIRST-FORM                VALUE 'Y'.   EL6942
00186      12  W-HEADER-BROWSE-STARTED PIC  X(01)          VALUE 'N'.   EL6942
00187      12  W-HEADER-SW             PIC  X(01)          VALUE SPACE. EL6942
00188          88  W-HEADER-REC-FOUND                      VALUE SPACE. EL6942
00189      12  W-PRINT-SW              PIC S9(01) COMP-3   VALUE ZERO.  EL6942
00190      12  W-PROCESSING-SW         PIC S9(01) COMP-3   VALUE ZERO.  EL6942
00191          88  W-PROCESS-BY-KEY                        VALUE +3.    EL6942
00192      12  W-TEXT-BROWSE-STARTED   PIC  X(01)          VALUE 'N'.   EL6942
00193      12  W-TOP-FORM-SW           PIC  X(01)          VALUE SPACE. EL6942
00194          88  W-TOP-FORM-SET                          VALUE 'T'.   EL6942
00195      12  W-OPTION-CODES          PIC  X(02).                      EL6942
00196          88  W-PRINT-INITIAL                         VALUE '11'.  EL6942
00197          88  W-PRINT-FOLLOW-UP                       VALUE '12'.  EL6942
00198          88  W-PRINT-LABELS                          VALUE ' 2'.  EL6942
00199          88  W-REPRINT-LETTERS                       VALUE ' 3'.  EL6942
00200                                  EJECT                            EL6942
00201  01  FILLER                      PIC  X(25)                       EL6942
00202                              VALUE 'PROGRAM INTERFACE STARTS:'.   EL6942
00203                                  COPY ELCINTF.                    EL6942
00204      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL6942
00205 **********************************************************        EL6942
00206          16  PI-6942-ALIGNMENT-COPIES                             EL6942
00207                                  PIC S9(01) COMP-3.               EL6942
00208          16  PI-6942-PRINT-DATE  PIC  X(08).                      EL6942
00209          16  PI-6942-PRINT-DATE-BIN                               EL6942
00210                                  PIC  X(02).                      EL6942
00211          16  PI-6942-PRINT-BY-KEY-IND                             EL6942
00212                                  PIC  X(01).                      EL6942
00213              88  PI-6942-PRINT-BY-KEY      VALUE 'C' 'G' 'S' 'A'. EL6942
00214              88  PI-6942-PRINT-BY-CARRIER  VALUE 'C'.             EL6942
00215              88  PI-6942-PRINT-BY-GROUPING VALUE 'G'.             EL6942
00216              88  PI-6942-PRINT-BY-STATE    VALUE 'S'.             EL6942
00217              88  PI-6942-PRINT-BY-ACCOUNT  VALUE 'A'.             EL6942
00218          16  PI-6942-PRINT-BY-PROCESSOR-IND                       EL6942
00219                                  PIC  X(01).                      EL6942
00220              88  PI-6942-PRINT-BY-PROCESSOR                       EL6942
00221                                            VALUE 'Y'.             EL6942
00222          16  PI-6942-PRINT-ID    PIC  X(04).                      EL6942
00223          16  PI-6942-PRINT-KEY.                                   EL6942
00224              20  PI-6942-PRINT-CARRIER                            EL6942
00225                                  PIC  X(01).                      EL6942
00226              20  PI-6942-PRINT-GROUPING                           EL6942
00227                                  PIC  X(06).                      EL6942
00228              20  PI-6942-PRINT-STATE                              EL6942
00229                                  PIC  X(02).                      EL6942
00230              20  PI-6942-PRINT-ACCOUNT                            EL6942
00231                                  PIC  X(10).                      EL6942
00232          16  PI-6942-PRINT-PROCESSOR                              EL6942
00233                                  PIC  X(04).                      EL6942
00234          16  PI-6942-LETTER-FORM PIC  X(04).                      EL6942
00235          16  PI-6942-LETTER-TYPE PIC  X(01).                      EL6942
00236          16  PI-6942-STARTING-ARCH-NO                             EL6942
00237                                  PIC S9(08) COMP.                 EL6942
00238          16  PI-6942-ENTRY.                                       EL6942
00239              20  PI-6942-FILLER  PIC  X(02).                      EL6942
00240              20  PI-6942-QUE-CONTROL                              EL6942
00241                                  PIC S9(08) COMP.                 EL6942
00242          16  FILLER              PIC X(585).                         CL**6
00243                                  EJECT                            EL6942
00244      COPY ELPRTCVD.                                               EL6942
00245  01  FILLER.                                                      EL6942
00246      16  FILLER                  PIC  X(200)                      EL6942
00247          VALUE 'THIS IS PART OF THE BUFFER ZONE'.                 EL6942
00248                                                                   EL6942
00249                                  EJECT                            EL6942
00250  01  FILLER                      PIC  X(18)                       EL6942
00251                              VALUE 'WORK TABLE STARTS:'.          EL6942
00252                                                                   EL6942
00253  01  W-ADJUST-AREA.                                               EL6942
00254      12  FILLER                  PIC  X(07).                         CL**6
00255      12  W-AD-PRINT-AREA         PIC  X(70).                      EL6942
00256      12  FILLER                  PIC  X(03).                      EL6942
00257                                                                   EL6942
00258  01  W-WORK-TABLE.                                                EL6942
00259      12  W-WORK-LINE OCCURS 300 TIMES                             EL6942
00260                           INDEXED BY W-WK-NDX.                    EL6942
00261          16  W-TEXT-LINE         PIC  X(70).                      EL6942
00262          16  W-SKIP-CONTROL      PIC  X(02).                      EL6942
00263              88  W-NO-LINES-SKIPPED            VALUE SPACES.      EL6942
00264              88  W-SKIP-TO-NEXT-PAGE           VALUE '99'.        EL6942
00265                                  EJECT                            EL6942
00266                                                                   EL6942
00267  01  FILLER                      PIC  X(16)                       EL6942
00268                              VALUE 'WORK TABLE ENDS:'.            EL6942
00269                                  EJECT                            EL6942
00270      COPY ELCDATE.                                                EL6942
00271                                  EJECT                            EL6942
00272      COPY ERCARCH.                                                EL6942
00273                                  EJECT                            EL6942
00274      COPY ELCDMD34.                                                  CL**7
00275                                                                      CL**7
00276  LINKAGE SECTION.                                                 EL6942
00277 *01 PARMLIST .                                                       CL**6
00278 *    02  FILLER                  PIC S9(08) COMP.                    CL**6
00279 *    02  L-ARCH-POINTER          PIC S9(08) COMP.                    CL**6
00280 *    02  L-ARCT-POINTER          PIC S9(08) COMP.                    CL**6
00281 *    02  L-CNTL-POINTER          PIC S9(08) COMP.                    CL**6
00282                                  EJECT                            EL6942
00283  01  L-LETTER-ARCHIVE            PIC X(250).                      EL6942
00284                                  EJECT                            EL6942
00285      COPY ERCARCT.                                                EL6942
00286                                  EJECT                            EL6942
00287      COPY ELCCNTL.                                                EL6942
00288                                  EJECT                            EL6942
00289  PROCEDURE DIVISION.                                              EL6942
00290                                                                   EL6942
00291      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6942
00292      MOVE '5'                    TO DC-OPTION-CODE.               EL6942
00293      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       EL6942
00294      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-CURRENT-DATE.          EL6942
00295      MOVE DC-BIN-DATE-1          TO W-SAVE-CURRENT-BIN-DATE.      EL6942
00296      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**7
00297                                                                   EL6942
00298  0100-RETRIEVE-LOOP.                                              EL6942
00299                                                                   EL6942
00300      EXEC CICS HANDLE CONDITION                                   EL6942
00301           ENDDATA (0200-END-DATA)                                 EL6942
00302           NOTFND  (0300-NOT-FOUND)                                EL6942
00303      END-EXEC.                                                    EL6942
00304                                                                   EL6942
00305      EXEC CICS RETRIEVE                                           EL6942
00306           INTO    (PROGRAM-INTERFACE-BLOCK)                       EL6942
00307           LENGTH  (PI-COMM-LENGTH)                                EL6942
00308      END-EXEC.                                                    EL6942
00309                                                                      CL**7
00310                                                                      CL**7
00311 * DLO034 OPEN WHEN DMD OR CID                                        CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00313          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**7
00314              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**7
00315              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**7
00316              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**7
00317              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**7
00318              MOVE SPACES             TO DL34-PRINT-LINE              CL**7
00319              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**7
00320              EXEC CICS LINK                                          CL**7
00321                  PROGRAM    ('DLO034')                               CL**7
00322                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**7
00323                  LENGTH     (DLO034-REC-LENGTH)                      CL**7
00324              END-EXEC                                                CL**7
00325              IF DL34-RETURN-CODE NOT = 'OK'                          CL**7
00326                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**7
00327                                      TO W-ERROR-LINE                 CL**7
00328                  PERFORM 0400-SEND-TEXT                              CL**7
00329                  EXEC CICS RETURN                                    CL**7
00330                  END-EXEC.                                           CL**7
00331                                                                   EL6942
00332      PERFORM 1000-INITIALIZE THRU 1000-EXIT.                      EL6942
00333      PERFORM 2000-PROCESS-ARCHIVES THRU 2999-EXIT.                EL6942
00334                                                                   EL6942
00335      IF  W-SAVE-ARCH-NO EQUAL ZEROS                               EL6942
00336              OR                                                   EL6942
00337          PI-6942-STARTING-ARCH-NO EQUAL W-SAVE-ARCH-NO            EL6942
00338          GO TO 0200-END-DATA.                                     EL6942
00339                                                                   EL6942
00340  0150-UPDATE-CONTROL-FILE.                                        EL6942
00341                                                                   EL6942
00342      EXEC CICS HANDLE CONDITION                                   EL6942
00343           NOTOPEN     (0200-END-DATA)                             EL6942
00344           NOTFND      (0200-END-DATA)                             EL6942
00345      END-EXEC.                                                    EL6942
00346                                                                   EL6942
00347      MOVE SPACES                 TO W-CNTL-KEY.                   EL6942
00348      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.            EL6942
00349      MOVE '1'                    TO W-CNTL-RECORD-TYPE.           EL6942
00350      MOVE ZEROS                  TO W-CNTL-SEQ.                   EL6942
00351                                                                   EL6942
00352      EXEC CICS READ                                               EL6942
00353           UPDATE                                                  EL6942
00354           DATASET (W-CNTL-ID)                                     EL6942
00355           SET     (ADDRESS OF CONTROL-FILE)                          CL**6
00356           RIDFLD  (W-CNTL-KEY)                                    EL6942
00357      END-EXEC.                                                    EL6942
00358                                                                   EL6942
00359      MOVE W-SAVE-ARCH-NO         TO CF-CREDIT-START-ARCH-NUM.     EL6942
00360                                                                   EL6942
00361      EXEC CICS REWRITE                                            EL6942
00362           DATASET (W-CNTL-ID)                                     EL6942
00363           FROM    (CONTROL-FILE)                                  EL6942
00364      END-EXEC.                                                    EL6942
00365                                                                   EL6942
00366  0200-END-DATA.                                                   EL6942
00367                                                                   EL6942
00368      MOVE '1'                    TO WS-PRINT-AREA.                EL6942
00369      MOVE W-ASTERISK-LINE1       TO WS-PASSED-DATA.               EL6942
00370      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00371      MOVE SPACES                 TO WS-PRINT-AREA.                EL6942
00372      MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.               EL6942
00373      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00374                                                                   EL6942
00375      MOVE '0'                    TO WS-PRINT-AREA.                EL6942
00376                                                                      CL**3
00377      IF W-PRINT-LABELS                                               CL**3
00378          MOVE W-LABEL-LINE-DESC  TO W-TOTAL-LINE-DESC.               CL**3
00379                                                                      CL**3
00380      MOVE W-LETTER-TOTALS        TO W-TOTAL-LETTERS.              EL6942
00381      MOVE W-TOTAL-LINE           TO WS-PASSED-DATA.               EL6942
00382      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00383                                                                   EL6942
00384      MOVE '0'                    TO WS-PRINT-AREA.                EL6942
00385      MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.               EL6942
00386      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00387      MOVE SPACES                 TO WS-PRINT-AREA.                EL6942
00388      MOVE W-ASTERISK-LINE        TO WS-PASSED-DATA.               EL6942
00389      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00390      MOVE '1'                    TO WS-PRINT-AREA.                EL6942
00391      MOVE SPACES                 TO WS-PASSED-DATA.               EL6942
00392      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00393                                                                   EL6942
00394      MOVE 'X'                    TO WS-PROG-END.                  EL6942
00395      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
00396                                                                   EL6942
00397 * DLO034 CLOSE                                                       CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00399          MOVE 'C'                TO DL34-PROCESS-TYPE                CL**7
00400          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL**7
00401          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL**7
00402          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL**7
00403          MOVE SPACES             TO DL34-PRINT-LINE                  CL**7
00404                                     DL34-OVERRIDE-PRINTER-ID         CL**7
00405          EXEC CICS LINK                                              CL**7
00406              PROGRAM    ('DLO034')                                   CL**7
00407              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL**7
00408              LENGTH     (DLO034-REC-LENGTH)                          CL**7
00409          END-EXEC                                                    CL**7
00410          IF DL34-RETURN-CODE NOT = 'OK'                              CL**7
00411              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL**7
00412                                  TO W-ERROR-LINE                     CL**7
00413              PERFORM 0400-SEND-TEXT.                                 CL**7
00414                                                                      CL**7
00415      EXEC CICS RETURN                                             EL6942
00416      END-EXEC.                                                    EL6942
00417                                                                   EL6942
00418  0300-NOT-FOUND.                                                  EL6942
00419                                                                   EL6942
00420      MOVE 'NO COMMUNICATION AREA FOUND'                           EL6942
00421                                  TO W-ERROR-LINE.                 EL6942
00422      PERFORM 0400-SEND-TEXT.                                      EL6942
00423      GO TO 0200-END-DATA.                                         EL6942
00424                                                                   EL6942
00425  0400-SEND-TEXT.                                                  EL6942
00426                                                                   EL6942
00427      EXEC CICS SEND TEXT                                          EL6942
00428          FROM   (W-ERROR-LINE)                                    EL6942
00429          LENGTH (70)                                              EL6942
00430      END-EXEC.                                                    EL6942
00431                                  EJECT                            EL6942
00432  1000-INITIALIZE.                                                 EL6942
00433                                                                   EL6942
00434      MOVE SPACES                 TO W-ADJUST-AREA.                EL6942
00435      MOVE W-SAVE-CURRENT-BIN-DATE                                 EL6942
00436                                  TO W-CURRENT-SAVE.               EL6942
00437                                                                   EL6942
00438      MOVE PI-ENTRY-CODES         TO W-OPTION-CODES.               EL6942
00439      MOVE PI-6942-STARTING-ARCH-NO                                EL6942
00440                                  TO W-SAVE-ARCH-NO.               EL6942
00441                                                                   EL6942
00442 ***************************************************************** EL6942
00443 *  THE FOLLOWING 'IF' LOGIC SETS THE PRIORITIES WHEN THE USER   * EL6942
00444 *  ENTERS MORE THAN ONE PRINT RESTRICTION.  THE LOGIC USING     * EL6942
00445 *  W-PROCESSING-SW MAKES DECISIONS BASED ON THIS PRIORITY.      * EL6942
00446 *  THE PRIORITY IS 1. BRANCH ENTRY/ CHECK QUE CONTROL           * EL6942
00447 *                  2. PROCESSOR ID                              * EL6942
00448 *                  3. LETTER FORM CODE                          * EL6942
00449 *                  4. KEY FIELDS                                * EL6942
00450 *                  5. NO RESTRICTIONS                           * EL6942
00451 ***************************************************************** EL6942
00452                                                                   EL6942
00453      IF  PI-6942-ENTRY GREATER THAN LOW-VALUES                    EL6942
00454          MOVE 5                  TO W-PROCESSING-SW               EL6942
00455      ELSE                                                         EL6942
00456          IF  PI-6942-PRINT-BY-PROCESSOR                           EL6942
00457              MOVE 1              TO W-PROCESSING-SW               EL6942
00458          ELSE                                                     EL6942
00459              IF  PI-6942-LETTER-FORM GREATER THAN SPACES          EL6942
00460                  MOVE 2          TO W-PROCESSING-SW               EL6942
00461              ELSE                                                 EL6942
00462                  IF  PI-6942-PRINT-BY-KEY                         EL6942
00463                      MOVE 3      TO W-PROCESSING-SW               EL6942
00464                  ELSE                                             EL6942
00465                      MOVE 4      TO W-PROCESSING-SW.              EL6942
00466                                                                   EL6942
00467  1000-EXIT.                                                       EL6942
00468      EXIT.                                                        EL6942
00469                                  EJECT                            EL6942
00470  2000-PROCESS-ARCHIVES.                                           EL6942
00471                                                                   EL6942
00472      GO TO 2100-BY-PROCESSOR                                      EL6942
00473            2200-BY-LETTER                                         EL6942
00474            2300-BY-KEY                                            EL6942
00475            2400-BY-ARCHIVE                                        EL6942
00476            2500-BY-CONTROL-ENTRY                                  EL6942
00477                                  DEPENDING ON W-PROCESSING-SW.    EL6942
00478                                                                   EL6942
00479                                  EJECT                            EL6942
00480  2100-BY-PROCESSOR.                                               EL6942
00481                                                                   EL6942
00482      MOVE LOW-VALUES             TO W-ARCH4-KEY.                  EL6942
00483      MOVE PI-COMPANY-CD          TO W-ARCH4-COMPANY-CD.           EL6942
00484      MOVE PI-6942-PRINT-PROCESSOR                                 EL6942
00485                                  TO W-ARCH4-PROCESSOR-CD.         EL6942
00486      MOVE PI-6942-PRINT-CARRIER  TO W-ARCH4-CARRIER.              EL6942
00487      MOVE PI-6942-PRINT-GROUPING TO W-ARCH4-GROUPING.             EL6942
00488      MOVE PI-6942-PRINT-STATE    TO W-ARCH4-STATE.                EL6942
00489      MOVE PI-6942-PRINT-ACCOUNT  TO W-ARCH4-ACCOUNT.              EL6942
00490                                                                   EL6942
00491      IF  W-PRINT-INITIAL                                          EL6942
00492              OR                                                   EL6942
00493          W-PRINT-FOLLOW-UP                                        EL6942
00494          COMPUTE W-ARCH4-ARCHIVE-NO                               EL6942
00495              = PI-6942-STARTING-ARCH-NO - 1.                      EL6942
00496                                                                   EL6942
00497      EXEC CICS HANDLE CONDITION                                   EL6942
00498           NOTOPEN  (8800-ARCH4-NOT-OPEN)                          EL6942
00499           NOTFND   (2999-EXIT)                                    EL6942
00500           ENDFILE  (2999-EXIT)                                    EL6942
00501      END-EXEC.                                                    EL6942
00502                                                                   EL6942
00503  2100-READ-NEXT.                                                  EL6942
00504                                                                   EL6942
00505      ADD +1                      TO W-ARCH4-ARCHIVE-NO.           EL6942
00506                                                                   EL6942
00507      EXEC CICS READ                                               EL6942
00508           DATASET (W-ARCH4-ID)                                    EL6942
00509           RIDFLD  (W-ARCH4-KEY)                                   EL6942
00510           SET     (ADDRESS OF L-LETTER-ARCHIVE)                      CL**6
00511           GTEQ                                                    EL6942
00512      END-EXEC.                                                    EL6942
00513                                                                   EL6942
00514      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.               EL6942
00515                                                                   EL6942
00516      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD                    EL6942
00517          GO TO 2999-EXIT.                                         EL6942
00518                                                                   EL6942
00519      IF  LA-PROCESSOR-CD NOT EQUAL PI-6942-PRINT-PROCESSOR        EL6942
00520          GO TO 2999-EXIT.                                         EL6942
00521                                                                   EL6942
00522      MOVE LA-CONTROL-BY-PROCESSOR                                 EL6942
00523                                  TO W-ARCH4-KEY.                  EL6942
00524                                                                   EL6942
00525      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL6942
00526              OR                                                   EL6942
00527          LA-STATUS-VOIDED                                         EL6942
00528              OR                                                   EL6942
00529          LA-STATUS-PURGED                                         EL6942
00530              OR                                                   EL6942
00531          LA-STATUS-TO-BE-PURGED                                   EL6942
00532              OR                                                   EL6942
00533          LA-STATUS-ON-HOLD                                        EL6942
00534          GO TO 2100-READ-NEXT.                                    EL6942
00535                                                                   EL6942
00536      ADD +1                      TO W-RECORD-COUNT.               EL6942
00537                                                                   EL6942
00538      IF  W-RECORD-COUNT IS GREATER THAN +50                       EL6942
00539          MOVE +0                 TO W-RECORD-COUNT                EL6942
00540                                                                   EL6942
00541          EXEC CICS DELAY                                          EL6942
00542              INTERVAL  (W-DELAY-INTERVAL)                         EL6942
00543          END-EXEC.                                                EL6942
00544                                                                   EL6942
00545      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.               EL6942
00546                                                                   EL6942
00547      IF  W-REPRINT-LETTERS                                        EL6942
00548          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT               EL6942
00549          GO TO 2100-READ-NEXT.                                    EL6942
00550                                                                   EL6942
00551      IF  W-PRINT-LABELS                                              CL**2
00552          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT                 EL6942
00553          GO TO 2100-READ-NEXT.                                    EL6942
00554                                                                   EL6942
00555      IF  LA-STATUS-COMPLETED                                      EL6942
00556          GO TO 2100-READ-NEXT.                                    EL6942
00557                                                                   EL6942
00558      IF  W-PRINT-INITIAL                                          EL6942
00559          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT               EL6942
00560          GO TO 2100-READ-NEXT.                                    EL6942
00561                                                                   EL6942
00562      IF  W-PRINT-FOLLOW-UP                                        EL6942
00563          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT             EL6942
00564          GO TO 2100-READ-NEXT.                                    EL6942
00565                                                                   EL6942
00566      GO TO 2100-READ-NEXT.                                        EL6942
00567                                                                   EL6942
00568  2100-EXIT.                                                       EL6942
00569      EXIT.                                                        EL6942
00570                                  EJECT                            EL6942
00571  2200-BY-LETTER.                                                  EL6942
00572                                                                   EL6942
00573      MOVE LOW-VALUES             TO W-ARCH3-KEY.                  EL6942
00574      MOVE PI-COMPANY-CD          TO W-ARCH3-COMPANY-CD.           EL6942
00575      MOVE PI-6942-LETTER-FORM    TO W-ARCH3-FORM.                 EL6942
00576      MOVE PI-6942-PRINT-CARRIER  TO W-ARCH3-CARRIER.              EL6942
00577      MOVE PI-6942-PRINT-GROUPING TO W-ARCH3-GROUPING.             EL6942
00578      MOVE PI-6942-PRINT-STATE    TO W-ARCH3-STATE.                EL6942
00579      MOVE PI-6942-PRINT-ACCOUNT  TO W-ARCH3-ACCOUNT.              EL6942
00580                                                                   EL6942
00581      IF  W-PRINT-INITIAL                                          EL6942
00582              OR                                                   EL6942
00583          W-PRINT-FOLLOW-UP                                        EL6942
00584          COMPUTE W-ARCH3-ARCHIVE-NO                               EL6942
00585              = PI-6942-STARTING-ARCH-NO - 1.                      EL6942
00586                                                                   EL6942
00587      EXEC CICS HANDLE CONDITION                                   EL6942
00588           NOTOPEN  (8810-ARCH3-NOT-OPEN)                          EL6942
00589           NOTFND   (2999-EXIT)                                    EL6942
00590           ENDFILE  (2999-EXIT)                                    EL6942
00591      END-EXEC.                                                    EL6942
00592                                                                   EL6942
00593  2200-READ-NEXT.                                                  EL6942
00594                                                                   EL6942
00595      ADD +1                      TO W-ARCH3-ARCHIVE-NO.           EL6942
00596                                                                   EL6942
00597      EXEC CICS READ                                               EL6942
00598           DATASET (W-ARCH3-ID)                                    EL6942
00599           RIDFLD  (W-ARCH3-KEY)                                   EL6942
00600           SET     (ADDRESS OF L-LETTER-ARCHIVE)                      CL**6
00601           GTEQ                                                    EL6942
00602      END-EXEC.                                                    EL6942
00603                                                                   EL6942
00604      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.               EL6942
00605                                                                   EL6942
00606      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD                    EL6942
00607          GO TO 2999-EXIT.                                         EL6942
00608                                                                   EL6942
00609      IF  LA-FORM-A3 NOT EQUAL PI-6942-LETTER-FORM                 EL6942
00610          GO TO 2999-EXIT.                                         EL6942
00611                                                                   EL6942
00612      MOVE LA-CONTROL-BY-FORM     TO W-ARCH3-KEY.                  EL6942
00613                                                                   EL6942
00614      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL6942
00615              OR                                                   EL6942
00616          LA-STATUS-VOIDED                                         EL6942
00617              OR                                                   EL6942
00618          LA-STATUS-PURGED                                         EL6942
00619              OR                                                   EL6942
00620          LA-STATUS-TO-BE-PURGED                                   EL6942
00621              OR                                                   EL6942
00622          LA-STATUS-ON-HOLD                                        EL6942
00623          GO TO 2200-READ-NEXT.                                    EL6942
00624                                                                   EL6942
00625      ADD +1                      TO W-RECORD-COUNT.               EL6942
00626                                                                   EL6942
00627      IF  W-RECORD-COUNT IS GREATER THAN +50                       EL6942
00628          MOVE +0                 TO W-RECORD-COUNT                EL6942
00629                                                                   EL6942
00630          EXEC CICS DELAY                                          EL6942
00631              INTERVAL  (W-DELAY-INTERVAL)                         EL6942
00632          END-EXEC.                                                EL6942
00633                                                                   EL6942
00634      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.               EL6942
00635                                                                   EL6942
00636      IF  W-REPRINT-LETTERS                                        EL6942
00637          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT               EL6942
00638          GO TO 2200-READ-NEXT.                                    EL6942
00639                                                                   EL6942
00640      IF  W-PRINT-LABELS                                              CL**2
00641          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT                 EL6942
00642          GO TO 2200-READ-NEXT.                                    EL6942
00643                                                                   EL6942
00644      IF  LA-STATUS-COMPLETED                                      EL6942
00645          GO TO 2200-READ-NEXT.                                    EL6942
00646                                                                   EL6942
00647      IF  W-PRINT-INITIAL                                          EL6942
00648          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT               EL6942
00649          GO TO 2200-READ-NEXT.                                    EL6942
00650                                                                   EL6942
00651      IF  W-PRINT-FOLLOW-UP                                        EL6942
00652          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT             EL6942
00653          GO TO 2200-READ-NEXT.                                    EL6942
00654                                                                   EL6942
00655      GO TO 2200-READ-NEXT.                                        EL6942
00656                                  EJECT                            EL6942
00657  2300-BY-KEY.                                                     EL6942
00658                                                                   EL6942
00659      MOVE LOW-VALUES             TO W-ARCH5-KEY.                  EL6942
00660      MOVE PI-COMPANY-CD          TO W-ARCH5-COMPANY-CD.           EL6942
00661      MOVE PI-6942-PRINT-CARRIER  TO W-ARCH5-CARRIER.              EL6942
00662      MOVE PI-6942-PRINT-GROUPING TO W-ARCH5-GROUPING.             EL6942
00663      MOVE PI-6942-PRINT-STATE    TO W-ARCH5-STATE.                EL6942
00664      MOVE PI-6942-PRINT-ACCOUNT  TO W-ARCH5-ACCOUNT.              EL6942
00665                                                                   EL6942
00666      IF  W-PRINT-INITIAL                                          EL6942
00667              OR                                                   EL6942
00668          W-PRINT-FOLLOW-UP                                        EL6942
00669          COMPUTE W-ARCH5-ARCHIVE-NO                               EL6942
00670              = PI-6942-STARTING-ARCH-NO - 1.                      EL6942
00671                                                                   EL6942
00672      EXEC CICS HANDLE CONDITION                                   EL6942
00673           NOTOPEN  (8860-ARCH5-NOT-OPEN)                          EL6942
00674           NOTFND   (2999-EXIT)                                    EL6942
00675           ENDFILE  (2999-EXIT)                                    EL6942
00676      END-EXEC.                                                    EL6942
00677                                                                   EL6942
00678  2300-READ-NEXT.                                                  EL6942
00679                                                                   EL6942
00680      ADD +1                      TO W-ARCH5-ARCHIVE-NO.           EL6942
00681                                                                   EL6942
00682      EXEC CICS READ                                               EL6942
00683           DATASET (W-ARCH5-ID)                                    EL6942
00684           RIDFLD  (W-ARCH5-KEY)                                   EL6942
00685           SET     (ADDRESS OF L-LETTER-ARCHIVE)                      CL**6
00686           GTEQ                                                    EL6942
00687      END-EXEC.                                                    EL6942
00688                                                                   EL6942
00689      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.               EL6942
00690                                                                   EL6942
00691      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD                    EL6942
00692          GO TO 2999-EXIT.                                         EL6942
00693                                                                   EL6942
00694      MOVE LA-CONTROL-BY-KEY-FIELDS                                EL6942
00695                                  TO W-ARCH5-KEY.                  EL6942
00696                                                                   EL6942
00697      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL6942
00698              OR                                                   EL6942
00699          LA-STATUS-VOIDED                                         EL6942
00700              OR                                                   EL6942
00701          LA-STATUS-PURGED                                         EL6942
00702              OR                                                   EL6942
00703          LA-STATUS-TO-BE-PURGED                                   EL6942
00704              OR                                                   EL6942
00705          LA-STATUS-ON-HOLD                                        EL6942
00706          GO TO 2300-READ-NEXT.                                    EL6942
00707                                                                   EL6942
00708      ADD +1                      TO W-RECORD-COUNT.               EL6942
00709                                                                   EL6942
00710      IF  W-RECORD-COUNT IS GREATER THAN +50                       EL6942
00711          MOVE +0                 TO W-RECORD-COUNT                EL6942
00712          EXEC CICS DELAY                                          EL6942
00713              INTERVAL  (W-DELAY-INTERVAL)                         EL6942
00714          END-EXEC.                                                EL6942
00715                                                                   EL6942
00716      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.               EL6942
00717                                                                   EL6942
00718      IF  W-REPRINT-LETTERS                                        EL6942
00719          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT               EL6942
00720          GO TO 2300-READ-NEXT.                                    EL6942
00721                                                                   EL6942
00722      IF  W-PRINT-LABELS                                              CL**2
00723          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT                 EL6942
00724          GO TO 2300-READ-NEXT.                                    EL6942
00725                                                                   EL6942
00726      IF  LA-STATUS-COMPLETED                                      EL6942
00727          GO TO 2300-READ-NEXT.                                    EL6942
00728                                                                   EL6942
00729      IF  W-PRINT-INITIAL                                          EL6942
00730          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT               EL6942
00731          GO TO 2300-READ-NEXT.                                    EL6942
00732                                                                   EL6942
00733      IF  W-PRINT-FOLLOW-UP                                        EL6942
00734          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT             EL6942
00735          GO TO 2300-READ-NEXT.                                    EL6942
00736                                                                   EL6942
00737      GO TO 2300-READ-NEXT.                                        EL6942
00738                                  EJECT                            EL6942
00739  2400-BY-ARCHIVE.                                                 EL6942
00740                                                                   EL6942
00741      MOVE LOW-VALUES             TO W-ARCH-KEY.                   EL6942
00742      MOVE PI-COMPANY-CD          TO W-ARCH-COMPANY-CD.            EL6942
00743                                                                   EL6942
00744      IF  W-PRINT-INITIAL                                          EL6942
00745              OR                                                   EL6942
00746          W-PRINT-FOLLOW-UP                                        EL6942
00747          COMPUTE W-ARCH-NUMBER                                    EL6942
00748              = PI-6942-STARTING-ARCH-NO - 1.                      EL6942
00749                                                                   EL6942
00750      EXEC CICS HANDLE CONDITION                                   EL6942
00751           NOTOPEN  (8870-ARCH-NOT-OPEN)                           EL6942
00752           NOTFND   (2999-EXIT)                                    EL6942
00753           ENDFILE  (2999-EXIT)                                    EL6942
00754      END-EXEC.                                                    EL6942
00755                                                                   EL6942
00756  2400-READ-NEXT.                                                  EL6942
00757                                                                   EL6942
00758      ADD +1                      TO W-ARCH-NUMBER.                EL6942
00759                                                                   EL6942
00760      EXEC CICS READ                                               EL6942
00761           DATASET (W-ARCH-ID)                                     EL6942
00762           RIDFLD  (W-ARCH-KEY)                                    EL6942
00763           SET     (ADDRESS OF L-LETTER-ARCHIVE)                      CL**6
00764           GTEQ                                                    EL6942
00765      END-EXEC.                                                    EL6942
00766                                                                   EL6942
00767      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.               EL6942
00768                                                                   EL6942
00769      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD                    EL6942
00770          GO TO 2999-EXIT.                                         EL6942
00771                                                                   EL6942
00772      MOVE LA-CONTROL-PRIMARY     TO W-ARCH-KEY.                   EL6942
00773                                                                   EL6942
00774      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL6942
00775              OR                                                   EL6942
00776          LA-STATUS-VOIDED                                         EL6942
00777              OR                                                   EL6942
00778          LA-STATUS-PURGED                                         EL6942
00779              OR                                                   EL6942
00780          LA-STATUS-TO-BE-PURGED                                   EL6942
00781              OR                                                   EL6942
00782          LA-STATUS-ON-HOLD                                        EL6942
00783          GO TO 2400-READ-NEXT.                                    EL6942
00784                                                                   EL6942
00785      ADD +1                      TO W-RECORD-COUNT.               EL6942
00786                                                                   EL6942
00787      IF  W-RECORD-COUNT IS GREATER THAN +50                       EL6942
00788          MOVE +0                 TO W-RECORD-COUNT                EL6942
00789          EXEC CICS DELAY                                          EL6942
00790              INTERVAL  (W-DELAY-INTERVAL)                         EL6942
00791          END-EXEC.                                                EL6942
00792                                                                   EL6942
00793      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.               EL6942
00794                                                                   EL6942
00795      IF  W-REPRINT-LETTERS                                        EL6942
00796          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT               EL6942
00797          GO TO 2400-READ-NEXT.                                    EL6942
00798                                                                   EL6942
00799      IF  W-PRINT-LABELS                                              CL**2
00800          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT                 EL6942
00801          GO TO 2400-READ-NEXT.                                    EL6942
00802                                                                   EL6942
00803      IF  LA-STATUS-COMPLETED                                      EL6942
00804          GO TO 2400-READ-NEXT.                                    EL6942
00805                                                                   EL6942
00806      IF  W-PRINT-INITIAL                                          EL6942
00807          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT               EL6942
00808          GO TO 2400-READ-NEXT.                                    EL6942
00809                                                                   EL6942
00810      IF  W-PRINT-FOLLOW-UP                                        EL6942
00811          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT             EL6942
00812          GO TO 2400-READ-NEXT.                                    EL6942
00813                                                                   EL6942
00814      GO TO 2400-READ-NEXT.                                        EL6942
00815                                  EJECT                            EL6942
00816  2500-BY-CONTROL-ENTRY.                                           EL6942
00817                                                                   EL6942
00818      MOVE LOW-VALUES             TO W-ARCH6-KEY.                  EL6942
00819      MOVE PI-COMPANY-CD          TO W-ARCH6-COMPANY-CD.           EL6942
00820      MOVE PI-6942-ENTRY          TO W-ARCH6-ENTRY.                EL6942
00821                                                                   EL6942
00822      IF  W-PRINT-INITIAL                                          EL6942
00823              OR                                                   EL6942
00824          W-PRINT-FOLLOW-UP                                        EL6942
00825          COMPUTE W-ARCH6-ARCHIVE-NO                               EL6942
00826              = PI-6942-STARTING-ARCH-NO - 1.                      EL6942
00827                                                                   EL6942
00828      EXEC CICS HANDLE CONDITION                                   EL6942
00829           NOTOPEN  (8860-ARCH5-NOT-OPEN)                          EL6942
00830      END-EXEC.                                                    EL6942
00831                                                                   EL6942
00832  2500-READ-NEXT.                                                  EL6942
00833                                                                   EL6942
00834      EXEC CICS HANDLE CONDITION                                   EL6942
00835           ENDFILE  (2999-EXIT)                                    EL6942
00836           NOTFND   (2999-EXIT)                                    EL6942
00837      END-EXEC.                                                    EL6942
00838                                                                   EL6942
00839      ADD +1                      TO W-ARCH6-ARCHIVE-NO.           EL6942
00840                                                                   EL6942
00841      EXEC CICS READ                                               EL6942
00842           DATASET (W-ARCH6-ID)                                    EL6942
00843           RIDFLD  (W-ARCH6-KEY)                                   EL6942
00844           SET     (ADDRESS OF L-LETTER-ARCHIVE)                      CL**6
00845           GTEQ                                                    EL6942
00846      END-EXEC.                                                    EL6942
00847                                                                   EL6942
00848      MOVE L-LETTER-ARCHIVE       TO LETTER-ARCHIVE.               EL6942
00849                                                                   EL6942
00850      IF  PI-COMPANY-CD NOT EQUAL LA-COMPANY-CD                    EL6942
00851          GO TO 2999-EXIT.                                         EL6942
00852                                                                   EL6942
00853      IF  PI-6942-ENTRY NOT EQUAL LA-ENTRY-A6                      EL6942
00854          GO TO 2999-EXIT.                                         EL6942
00855                                                                   EL6942
00856      MOVE LA-CONTROL-BY-GROUP-CODE                                EL6942
00857                                  TO W-ARCH6-KEY.                  EL6942
00858                                                                   EL6942
00859      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL6942
00860              OR                                                   EL6942
00861          LA-STATUS-VOIDED                                         EL6942
00862              OR                                                   EL6942
00863          LA-STATUS-PURGED                                         EL6942
00864              OR                                                   EL6942
00865          LA-STATUS-TO-BE-PURGED                                   EL6942
00866              OR                                                   EL6942
00867          LA-STATUS-ON-HOLD                                        EL6942
00868          GO TO 2500-READ-NEXT.                                    EL6942
00869                                                                   EL6942
00870      ADD +1                      TO W-RECORD-COUNT.               EL6942
00871                                                                   EL6942
00872      IF  W-RECORD-COUNT IS GREATER THAN +50                       EL6942
00873          MOVE +0                 TO W-RECORD-COUNT                EL6942
00874          EXEC CICS DELAY                                          EL6942
00875              INTERVAL  (W-DELAY-INTERVAL)                         EL6942
00876          END-EXEC.                                                EL6942
00877                                                                   EL6942
00878      MOVE LA-ARCHIVE-NO          TO W-ARCHIVE-SAVE.               EL6942
00879                                                                   EL6942
00880      IF  W-REPRINT-LETTERS                                        EL6942
00881          PERFORM 6000-REPRINT-CHECKS THRU 6299-EXIT               EL6942
00882          GO TO 2500-READ-NEXT.                                    EL6942
00883                                                                   EL6942
00884      IF  W-PRINT-LABELS                                              CL**2
00885          PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT                 EL6942
00886          GO TO 2500-READ-NEXT.                                    EL6942
00887                                                                   EL6942
00888      IF  LA-STATUS-COMPLETED                                      EL6942
00889          GO TO 2500-READ-NEXT.                                    EL6942
00890                                                                   EL6942
00891      IF  W-PRINT-INITIAL                                          EL6942
00892          PERFORM 3000-INITIAL-CHECKS THRU 3999-EXIT               EL6942
00893          GO TO 2500-READ-NEXT.                                    EL6942
00894                                                                   EL6942
00895      IF  W-PRINT-FOLLOW-UP                                        EL6942
00896          PERFORM 4000-FOLLOW-UP-CHECKS THRU 4199-EXIT             EL6942
00897          GO TO 2500-READ-NEXT.                                    EL6942
00898                                                                   EL6942
00899      GO TO 2500-READ-NEXT.                                        EL6942
00900                                                                   EL6942
00901  2999-EXIT.                                                       EL6942
00902      EXIT.                                                        EL6942
00903                                  EJECT                            EL6942
00904  3000-INITIAL-CHECKS.                                             EL6942
00905                                                                   EL6942
00906      IF  LA-INITIAL-PRINT-DATE NOT EQUAL LOW-VALUES               EL6942
00907          GO TO 3999-EXIT.                                         EL6942
00908                                                                   EL6942
00909      PERFORM 3900-CHECK-FOR-ACTIVE-RESENDS THRU 3900-EXIT         EL6942

031011     IF PI-6942-PRINT-DATE-BIN NOT = LOW-VALUES
031011        IF LA-CREATION-DATE NOT = PI-6942-PRINT-DATE-BIN
031011           GO TO 3999-EXIT
031011        END-IF
031011     END-IF

00923      GO TO 3100-BY-PROCESSOR                                      EL6942
00924            3200-BY-LETTER                                         EL6942
00925            3300-BY-KEY                                            EL6942
00926            3400-BY-ARCHIVE                                        EL6942
00927            3500-BY-CONTROL-ENTRY                                  EL6942
00928                                  DEPENDING ON W-PROCESSING-SW.    EL6942
00929      GO TO 3999-EXIT.                                             EL6942
00930                                  EJECT                            EL6942
00931  3100-BY-PROCESSOR.                                               EL6942
00932                                                                   EL6942
00933      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
00934          GO TO 3999-EXIT.                                         EL6942
00935                                                                   EL6942
00936      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
00937              AND                                                  EL6942
00938          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
00939          GO TO 3999-EXIT.                                         EL6942
00940                                                                   EL6942
00941      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
00942              AND                                                  EL6942
00943          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
00944          GO TO 3999-EXIT.                                         EL6942
00945                                                                   EL6942
00946      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
00947          GO TO 3100-CONTINUE.                                     EL6942
00948                                                                   EL6942
00949      IF  LA-CARRIER-A4 GREATER THAN PI-6942-PRINT-CARRIER         EL6942
00950          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
00951          GO TO 3999-EXIT.                                         EL6942
00952                                                                   EL6942
00953      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
00954          GO TO 3100-CONTINUE.                                     EL6942
00955                                                                   EL6942
00956      IF  LA-GROUPING-A4 GREATER THAN PI-6942-PRINT-GROUPING       EL6942
00957          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
00958          GO TO 3999-EXIT.                                         EL6942
00959                                                                   EL6942
00960      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
00961          GO TO 3100-CONTINUE.                                     EL6942
00962                                                                   EL6942
00963      IF  LA-STATE-A4 GREATER THAN PI-6942-PRINT-STATE             EL6942
00964          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
00965          GO TO 3999-EXIT.                                         EL6942
00966                                                                   EL6942
00967      IF  PI-6942-PRINT-BY-STATE                                   EL6942
00968          GO TO 3100-CONTINUE.                                     EL6942
00969                                                                   EL6942
00970      IF  LA-ACCOUNT-A4 GREATER THAN PI-6942-PRINT-ACCOUNT         EL6942
00971          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
00972          GO TO 3999-EXIT.                                         EL6942
00973                                                                   EL6942
00974  3100-CONTINUE.                                                   EL6942
00975                                                                   EL6942
00976      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
00977                                                                   EL6942
00978      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
00979                                                                   EL6942
00980      IF  LA-RESEND-DATE = LOW-VALUES
00981          MOVE 'C'                TO LA-STATUS.                    EL6942
00982                                                                   EL6942
00983      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
00984                                                                   EL6942
00985      IF  W-THIS-IS-FIRST-FORM                                     EL6942
00986          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
00987              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
00988          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
00989                                                                   EL6942
00990      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
00991          W-COPIES TIMES.                                          EL6942
00992                                                                      CL**7
00993      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
00994                                                                      CL**7
00995      GO TO 3999-EXIT.                                             EL6942
00996                                                                   EL6942
00997  3100-EXIT.                                                       EL6942
00998       EXIT.                                                       EL6942
00999                                  EJECT                            EL6942
01000  3200-BY-LETTER.                                                  EL6942
01001                                                                   EL6942
01002      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01003              OR                                                   EL6942
01004          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01005          GO TO 3999-EXIT.                                         EL6942
01006                                                                   EL6942
01007      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
01008          GO TO 3200-CONTINUE.                                     EL6942
01009                                                                   EL6942
01010      IF  LA-CARRIER-A3 GREATER THAN PI-6942-PRINT-CARRIER         EL6942
01011          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01012          GO TO 3999-EXIT.                                         EL6942
01013                                                                   EL6942
01014      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01015          GO TO 3200-CONTINUE.                                     EL6942
01016                                                                   EL6942
01017      IF  LA-GROUPING-A3 GREATER THAN PI-6942-PRINT-GROUPING       EL6942
01018          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01019          GO TO 3999-EXIT.                                         EL6942
01020                                                                   EL6942
01021      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01022          GO TO 3200-CONTINUE.                                     EL6942
01023                                                                   EL6942
01024      IF  LA-STATE-A3 GREATER THAN PI-6942-PRINT-STATE             EL6942
01025          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01026          GO TO 3999-EXIT.                                         EL6942
01027                                                                   EL6942
01028      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01029          GO TO 3200-CONTINUE.                                     EL6942
01030                                                                   EL6942
01031      IF  LA-ACCOUNT-A3 GREATER THAN PI-6942-PRINT-ACCOUNT         EL6942
01032          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01033          GO TO 3999-EXIT.                                         EL6942
01034                                                                   EL6942
01035  3200-CONTINUE.                                                   EL6942
01036                                                                   EL6942
01037      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01038                                                                   EL6942
01039      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01040                                                                   EL6942
01041      IF  LA-RESEND-DATE   EQUAL LOW-VALUES                        EL6942
01042          MOVE 'C'                TO LA-STATUS.                    EL6942
01043                                                                   EL6942
01044      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01045                                                                   EL6942
01046      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01047          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01048              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01049          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01050                                                                   EL6942
01051      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01052          W-COPIES TIMES.                                          EL6942
01053                                                                      CL**7
01054      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01055                                                                      CL**7
01056      GO TO 3999-EXIT.                                             EL6942
01057                                                                   EL6942
01058  3200-EXIT.                                                       EL6942
01059       EXIT.                                                       EL6942
01060                                  EJECT                            EL6942
01061  3300-BY-KEY.                                                     EL6942
01062                                                                   EL6942
01063      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01064              OR                                                   EL6942
01065          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01066              OR                                                   EL6942
01067          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01068          GO TO 3999-EXIT.                                         EL6942
01069                                                                   EL6942
01070      IF  LA-CARRIER-A5 GREATER THAN PI-6942-PRINT-CARRIER         EL6942
01071          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01072          GO TO 3999-EXIT.                                         EL6942
01073                                                                   EL6942
01074      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01075          GO TO 3300-CONTINUE.                                     EL6942
01076                                                                   EL6942
01077      IF  LA-GROUPING-A5 GREATER THAN PI-6942-PRINT-GROUPING       EL6942
01078          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01079          GO TO 3999-EXIT.                                         EL6942
01080                                                                   EL6942
01081      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01082          GO TO 3300-CONTINUE.                                     EL6942
01083                                                                   EL6942
01084      IF  LA-STATE-A5 GREATER THAN PI-6942-PRINT-STATE             EL6942
01085          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01086          GO TO 3999-EXIT.                                         EL6942
01087                                                                   EL6942
01088      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01089          GO TO 3300-CONTINUE.                                     EL6942
01090                                                                   EL6942
01091      IF  LA-ACCOUNT-A5 GREATER THAN PI-6942-PRINT-ACCOUNT         EL6942
01092          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01093          GO TO 3999-EXIT.                                         EL6942
01094                                                                   EL6942
01095  3300-CONTINUE.                                                   EL6942
01096                                                                   EL6942
01097      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01098                                                                   EL6942
01099      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01100      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01101                                                                   EL6942
01102      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01103          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01104              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01105          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01106                                                                   EL6942
01107      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01108          W-COPIES TIMES.                                          EL6942
01109                                                                      CL**7
01110      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01111                                                                      CL**7
01112      GO TO 3999-EXIT.                                             EL6942
01113                                                                   EL6942
01114  3300-EXIT.                                                       EL6942
01115       EXIT.                                                       EL6942
01116                                  EJECT                            EL6942
01117  3400-BY-ARCHIVE.                                                 EL6942
01118                                                                   EL6942
01119      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01120              OR                                                   EL6942
01121          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01122              OR                                                   EL6942
01123          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01124          GO TO 3999-EXIT.                                         EL6942
01125                                                                   EL6942
01126      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01127                                                                   EL6942
01128      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01129                                                                   EL6942
01130      IF  LA-RESEND-DATE   EQUAL LOW-VALUES                        EL6942
01131          MOVE 'C'                TO LA-STATUS.                    EL6942
01132                                                                   EL6942
01133      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01134                                                                   EL6942
01135      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01136          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01137              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01138          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01139                                                                   EL6942
01140      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01141          W-COPIES TIMES.                                          EL6942
01142                                                                      CL**7
01143      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01144                                                                      CL**7
01145      GO TO 3999-EXIT.                                             EL6942
01146                                                                   EL6942
01147  3400-EXIT.                                                       EL6942
01148       EXIT.                                                       EL6942
01149                                  EJECT                            EL6942
01150  3500-BY-CONTROL-ENTRY.                                           EL6942
01151                                                                   EL6942
01152      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES              EL6942
01153              AND                                                  EL6942
01154          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
01155          GO TO 3999-EXIT.                                         EL6942
01156                                                                   EL6942
01157      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01158              AND                                                  EL6942
01159          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
01160          GO TO 3999-EXIT.                                         EL6942
01161                                                                   EL6942
01162      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
01163              AND                                                  EL6942
01164          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01165          GO TO 3999-EXIT.                                         EL6942
01166                                                                   EL6942
01167      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01168              AND                                                  EL6942
01169          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01170          GO TO 3999-EXIT.                                         EL6942
01171                                                                   EL6942
01172      IF  NOT W-PROCESS-BY-KEY                                     EL6942
01173          GO TO 3500-CONTINUE.                                     EL6942
01174                                                                   EL6942
01175      IF  PI-6942-PRINT-BY-KEY                                     EL6942
01176          IF  PI-6942-PRINT-BY-CARRIER                             EL6942
01177              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER     EL6942
01178                  NEXT SENTENCE                                    EL6942
01179              ELSE                                                 EL6942
01180                  GO TO 3999-EXIT                                  EL6942
01181          ELSE                                                     EL6942
01182              IF  PI-6942-PRINT-BY-GROUPING                        EL6942
01183                  IF  LA-CARRIER-A3 IS EQUAL                       EL6942
01184                          PI-6942-PRINT-CARRIER                    EL6942
01185                          AND                                      EL6942
01186                      LA-GROUPING-A3 IS EQUAL                      EL6942
01187                          PI-6942-PRINT-GROUPING                   EL6942
01188                      NEXT SENTENCE                                EL6942
01189                  ELSE                                             EL6942
01190                      GO TO 3999-EXIT                              EL6942
01191              ELSE                                                 EL6942
01192                  IF  PI-6942-PRINT-BY-STATE                       EL6942
01193                      IF  LA-CARRIER-A3 IS EQUAL                   EL6942
01194                              PI-6942-PRINT-CARRIER                EL6942
01195                              AND                                  EL6942
01196                          LA-GROUPING-A3 IS EQUAL                  EL6942
01197                              PI-6942-PRINT-GROUPING               EL6942
01198                              AND                                  EL6942
01199                          LA-STATE-A3 IS EQUAL                     EL6942
01200                              PI-6942-PRINT-STATE                  EL6942
01201                          NEXT SENTENCE                            EL6942
01202                      ELSE                                         EL6942
01203                          GO TO 3999-EXIT                          EL6942
01204                  ELSE                                             EL6942
01205                      IF  PI-6942-PRINT-BY-ACCOUNT                 EL6942
01206                          IF  LA-CARRIER-A3 IS EQUAL               EL6942
01207                                  PI-6942-PRINT-CARRIER            EL6942
01208                                  AND                              EL6942
01209                              LA-GROUPING-A3 IS EQUAL              EL6942
01210                                  PI-6942-PRINT-GROUPING           EL6942
01211                                  AND                              EL6942
01212                              LA-STATE-A3 IS EQUAL                 EL6942
01213                                  PI-6942-PRINT-STATE              EL6942
01214                                  AND                              EL6942
01215                              LA-ACCOUNT-A3 IS EQUAL               EL6942
01216                                  PI-6942-PRINT-ACCOUNT            EL6942
01217                              NEXT SENTENCE                        EL6942
01218                          ELSE                                     EL6942
01219                              GO TO 3999-EXIT.                     EL6942
01220                                                                   EL6942
01221  3500-CONTINUE.                                                   EL6942
01222                                                                   EL6942
01223      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01224                                                                   EL6942
01225      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01226                                                                   EL6942
01227      IF  LA-RESEND-DATE = LOW-VALUES
01228          MOVE 'C'                TO LA-STATUS.                    EL6942
01229                                                                   EL6942
01230      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01231                                                                   EL6942
01232      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01233          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01234              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01235          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01236                                                                   EL6942
01237      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01238          W-COPIES TIMES.                                          EL6942
01239                                                                      CL**7
01240      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01241                                                                      CL**7
01242      GO TO 3999-EXIT.                                             EL6942
01243                                                                   EL6942
01244  3500-EXIT.                                                       EL6942
01245       EXIT.                                                       EL6942
01246                                  EJECT                            EL6942
01247  3900-CHECK-FOR-ACTIVE-RESENDS.                                   EL6942
01248                                                                   EL6942
01249      IF LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE
01251         MOVE LA-ARCHIVE-NO       TO W-SAVE-ARCH-NO
           END-IF

           .
01253  3900-EXIT.                                                       EL6942
01254       EXIT.                                                       EL6942
01255  3999-EXIT.                                                       EL6942
01256       EXIT.                                                       EL6942
01257                                  EJECT                            EL6942
01258  4000-FOLLOW-UP-CHECKS.                                           EL6942
01259                                                                   EL6942
01260      IF  LA-INITIAL-PRINT-DATE = LOW-VALUES                       EL6942
01261          IF  W-SAVE-ARCH-NO = ZEROS                               EL6942
01262                  OR                                               EL6942
01263              LA-ARCHIVE-NO LESS THAN W-SAVE-ARCH-NO               EL6942
01264              MOVE LA-ARCHIVE-NO  TO W-SAVE-ARCH-NO                EL6942
01265              GO TO 4199-EXIT                                      EL6942
01266          ELSE                                                     EL6942
01267              GO TO 4199-EXIT.                                     EL6942
01268                                                                   EL6942
01269      IF  LA-RESEND-DATE = LOW-VALUES
01270          GO TO 4199-EXIT.                                         EL6942
01271                                                                   EL6942
01272      MOVE LOW-VALUES             TO W-WORKING-RESEND-DATE         EL6942
01273                                     W-LAST-RESENT-PRINT-DATE.     EL6942
01274                                                                   EL6942
01275      IF  PI-6942-PRINT-DATE-BIN NOT = LOW-VALUES                  EL6942
01276          PERFORM 4600-GET-APP-RSND-DT-DT-GIVEN THRU 4600-EXIT     EL6942
01283      ELSE                                                         EL6942
01284          PERFORM 4500-GET-APP-RSND-DT-CURR-DT THRU 4500-EXIT      EL6942
           END-IF
01291                                                                   EL6942
01292      IF  W-WORKING-RESEND-DATE EQUAL LOW-VALUES                   EL6942
01293          IF  W-SAVE-ARCH-NO EQUAL ZEROS                           EL6942
01294              GO TO 4100-CHECK-ACTIVITY                            EL6942
01295          ELSE                                                     EL6942
01296              GO TO 4199-EXIT                                      EL6942
01297      ELSE                                                         EL6942
01298          IF  W-SAVE-ARCH-NO EQUAL ZEROS                           EL6942
01299              MOVE LA-ARCHIVE-NO  TO W-SAVE-ARCH-NO.               EL6942
01300                                                                   EL6942
01301      GO TO 4010-BY-PROCESSOR                                      EL6942
01302            4020-BY-LETTER                                         EL6942
01303            4030-BY-KEY                                            EL6942
01304            4040-BY-ARCHIVE                                        EL6942
01305            4050-BY-CONTROL-ENTRY                                  EL6942
01306                                  DEPENDING ON W-PROCESSING-SW.    EL6942
01307      GO TO 4199-EXIT.                                             EL6942
01308                                  EJECT                            EL6942
01309  4010-BY-PROCESSOR.                                               EL6942
01310                                                                   EL6942
01311      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01312          GO TO 4199-EXIT.                                         EL6942
01313                                                                   EL6942
01314      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
01315              AND                                                  EL6942
01316          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01317          GO TO 4199-EXIT.                                         EL6942
01318                                                                   EL6942
01319      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01320              AND                                                  EL6942
01321          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01322          GO TO 4199-EXIT.                                         EL6942
01323                                                                   EL6942
01324      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
01325          GO TO 4010-CONTINUE.                                     EL6942
01326                                                                   EL6942
01327      IF  LA-CARRIER-A4 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01328          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01329          GO TO 4199-EXIT.                                         EL6942
01330                                                                   EL6942
01331      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01332          GO TO 4010-CONTINUE.                                     EL6942
01333                                                                   EL6942
01334      IF  LA-GROUPING-A4 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01335          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01336          GO TO 4199-EXIT.                                         EL6942
01337                                                                   EL6942
01338      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01339          GO TO 4010-CONTINUE.                                     EL6942
01340                                                                   EL6942
01341      IF  LA-STATE-A4 LESS THAN PI-6942-PRINT-STATE                EL6942
01342          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01343          GO TO 4199-EXIT.                                         EL6942
01344                                                                   EL6942
01345      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01346          GO TO 4010-CONTINUE.                                     EL6942
01347                                                                   EL6942
01348      IF  LA-ACCOUNT-A4 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01349          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01350          GO TO 4199-EXIT.                                         EL6942
01351                                                                   EL6942
01352  4010-CONTINUE.                                                   EL6942
01353                                                                   EL6942
01354      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01355                                                                   EL6942
01356      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01357                                                                   EL6942
01358      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01359          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01360              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01361          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01362                                                                   EL6942
01363      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01364          W-COPIES TIMES.                                          EL6942
01365                                                                   EL6942
01366      MOVE W-LAST-RESENT-PRINT-DATE                                EL6942
01367                                  TO LA-LAST-RESENT-PRINT-DATE     EL6942
01368                                     LA-SENT-DATE
01369                                                                   EL6942

01371      MOVE 'C'                TO LA-STATUS
01372                                                                   EL6942
01373      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01374      GO TO 4199-EXIT.                                             EL6942
01375                                                                   EL6942
01376  4010-EXIT.                                                       EL6942
01377       EXIT.                                                       EL6942
01378                                  EJECT                            EL6942
01379  4020-BY-LETTER.                                                  EL6942
01380                                                                   EL6942
01381      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01382              OR                                                   EL6942
01383          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01384          GO TO 4199-EXIT.                                         EL6942
01385                                                                   EL6942
01386      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
01387          GO TO 4020-CONTINUE.                                     EL6942
01388                                                                   EL6942
01389      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01390          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01391          GO TO 4199-EXIT.                                         EL6942
01392                                                                   EL6942
01393      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01394          GO TO 4020-CONTINUE.                                     EL6942
01395                                                                   EL6942
01396      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01397          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01398          GO TO 4199-EXIT.                                         EL6942
01399                                                                   EL6942
01400      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01401          GO TO 4020-CONTINUE.                                     EL6942
01402                                                                   EL6942
01403      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE                EL6942
01404          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01405          GO TO 4199-EXIT.                                         EL6942
01406                                                                   EL6942
01407      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01408          GO TO 4020-CONTINUE.                                     EL6942
01409                                                                   EL6942
01410      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01411          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01412          GO TO 4199-EXIT.                                         EL6942
01413                                                                   EL6942
01414  4020-CONTINUE.                                                   EL6942
01415                                                                   EL6942
01416      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01417                                                                   EL6942
01418      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01419                                                                   EL6942
01420      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01421          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01422              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01423          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01424                                                                   EL6942
01425      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01426          W-COPIES TIMES.                                          EL6942
01427                                                                   EL6942
01428      MOVE W-LAST-RESENT-PRINT-DATE                                EL6942
01429                                  TO LA-LAST-RESENT-PRINT-DATE     EL6942
01430                                     LA-SENT-DATE
01431                                                                   EL6942
01433          MOVE 'C'                TO LA-STATUS.                    EL6942
01434                                                                   EL6942
01435      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01436      GO TO 4199-EXIT.                                             EL6942
01437                                                                   EL6942
01438  4020-EXIT.                                                       EL6942
01439       EXIT.                                                       EL6942
01440                                  EJECT                            EL6942
01441  4030-BY-KEY.                                                     EL6942
01442                                                                   EL6942
01443      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01444              OR                                                   EL6942
01445          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01446              OR                                                   EL6942
01447          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01448          GO TO 4199-EXIT.                                         EL6942
01449                                                                   EL6942
01450      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01451          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01452          GO TO 4199-EXIT.                                         EL6942
01453                                                                   EL6942
01454      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01455          GO TO 4030-CONTINUE.                                     EL6942
01456                                                                   EL6942
01457      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01458          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01459          GO TO 4199-EXIT.                                         EL6942
01460                                                                   EL6942
01461      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01462          GO TO 4030-CONTINUE.                                     EL6942
01463                                                                   EL6942
01464      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE                EL6942
01465          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01466          GO TO 4199-EXIT.                                         EL6942
01467                                                                   EL6942
01468      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01469          GO TO 4030-CONTINUE.                                     EL6942
01470                                                                   EL6942
01471      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01472          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01473          GO TO 4199-EXIT.                                         EL6942
01474                                                                   EL6942
01475  4030-CONTINUE.                                                   EL6942
01476                                                                   EL6942
01477      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01478                                                                   EL6942
01479      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01480                                                                   EL6942
01481      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01482          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01483              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01484          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01485                                                                   EL6942
01486      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01487          W-COPIES TIMES.                                          EL6942
01488                                                                   EL6942
01489      MOVE W-LAST-RESENT-PRINT-DATE                                EL6942
01490                                  TO LA-LAST-RESENT-PRINT-DATE     EL6942
01491                                     LA-SENT-DATE
01492                                                                   EL6942
01494          MOVE 'C'                TO LA-STATUS.                    EL6942
01495                                                                   EL6942
01496      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01497      GO TO 4199-EXIT.                                             EL6942
01498                                                                   EL6942
01499  4030-EXIT.                                                       EL6942
01500       EXIT.                                                       EL6942
01501                                  EJECT                            EL6942
01502  4040-BY-ARCHIVE.                                                 EL6942
01503                                                                   EL6942
01504      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01505              OR                                                   EL6942
01506          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01507              OR                                                   EL6942
01508          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01509          GO TO 4199-EXIT.                                         EL6942
01510                                                                   EL6942
01511      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01512                                                                   EL6942
01513      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01514                                                                   EL6942
01515      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01516          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01517              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01518          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01519                                                                   EL6942
01520      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01521          W-COPIES TIMES.                                          EL6942
01522                                                                   EL6942
01523      MOVE W-LAST-RESENT-PRINT-DATE                                EL6942
01524                                  TO LA-LAST-RESENT-PRINT-DATE     EL6942
01525                                     LA-SENT-DATE
01526                                                                   EL6942
01528          MOVE 'C'                TO LA-STATUS.                    EL6942
01529                                                                   EL6942
01530      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01531      GO TO 4199-EXIT.                                             EL6942
01532                                                                   EL6942
01533  4040-EXIT.                                                       EL6942
01534       EXIT.                                                       EL6942
01535                                  EJECT                            EL6942
01536  4050-BY-CONTROL-ENTRY.                                           EL6942
01537                                                                   EL6942
01538      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES              EL6942
01539              AND                                                  EL6942
01540          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
01541          GO TO 4199-EXIT.                                         EL6942
01542                                                                   EL6942
01543      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01544              AND                                                  EL6942
01545          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
01546          GO TO 4199-EXIT.                                         EL6942
01547                                                                   EL6942
01548      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
01549              AND                                                  EL6942
01550          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01551          GO TO 4199-EXIT.                                         EL6942
01552                                                                   EL6942
01553      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01554              AND                                                  EL6942
01555          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01556          GO TO 3999-EXIT.                                         EL6942
01557                                                                   EL6942
01558      IF  W-PROCESS-BY-KEY                                         EL6942
01559          GO TO 4050-CONTINUE.                                     EL6942
01560                                                                   EL6942
01561      IF  PI-6942-PRINT-BY-KEY                                     EL6942
01562          IF  PI-6942-PRINT-BY-CARRIER                             EL6942
01563              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER     EL6942
01564                  NEXT SENTENCE                                    EL6942
01565              ELSE                                                 EL6942
01566                  GO TO 4199-EXIT                                  EL6942
01567          ELSE                                                     EL6942
01568              IF  PI-6942-PRINT-BY-GROUPING                        EL6942
01569                  IF  LA-CARRIER-A3 IS EQUAL                       EL6942
01570                          PI-6942-PRINT-CARRIER                    EL6942
01571                          AND                                      EL6942
01572                      LA-GROUPING-A3 IS EQUAL                      EL6942
01573                          PI-6942-PRINT-GROUPING                   EL6942
01574                      NEXT SENTENCE                                EL6942
01575                  ELSE                                             EL6942
01576                      GO TO 4199-EXIT                              EL6942
01577              ELSE                                                 EL6942
01578                  IF  PI-6942-PRINT-BY-STATE                       EL6942
01579                      IF  LA-CARRIER-A3 IS EQUAL                   EL6942
01580                              PI-6942-PRINT-CARRIER                EL6942
01581                              AND                                  EL6942
01582                          LA-GROUPING-A3 IS EQUAL                  EL6942
01583                              PI-6942-PRINT-GROUPING               EL6942
01584                              AND                                  EL6942
01585                          LA-STATE-A3 IS EQUAL                     EL6942
01586                              PI-6942-PRINT-STATE                  EL6942
01587                          NEXT SENTENCE                            EL6942
01588                      ELSE                                         EL6942
01589                          GO TO 4199-EXIT                          EL6942
01590                  ELSE                                             EL6942
01591                      IF  PI-6942-PRINT-BY-ACCOUNT                 EL6942
01592                          IF  LA-CARRIER-A3 IS EQUAL               EL6942
01593                                  PI-6942-PRINT-CARRIER            EL6942
01594                                  AND                              EL6942
01595                              LA-GROUPING-A3 IS EQUAL              EL6942
01596                                  PI-6942-PRINT-GROUPING           EL6942
01597                                  AND                              EL6942
01598                              LA-STATE-A3 IS EQUAL                 EL6942
01599                                  PI-6942-PRINT-STATE              EL6942
01600                                  AND                              EL6942
01601                              LA-ACCOUNT-A3 IS EQUAL               EL6942
01602                                  PI-6942-PRINT-ACCOUNT            EL6942
01603                              NEXT SENTENCE                        EL6942
01604                          ELSE                                     EL6942
01605                              GO TO 4199-EXIT.                     EL6942
01606                                                                   EL6942
01607  4050-CONTINUE.                                                   EL6942
01608                                                                   EL6942
01609      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01610                                                                   EL6942
01611      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01612                                                                   EL6942
01613      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01614          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01615              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01616          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01617                                                                   EL6942
01618      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01619          W-COPIES TIMES.                                          EL6942
01620                                                                   EL6942
01622      MOVE W-LAST-RESENT-PRINT-DATE                                EL6942
01623                                  TO LA-LAST-RESENT-PRINT-DATE     EL6942
01624                                     LA-SENT-DATE
01625                                                                   EL6942
01627          MOVE 'C'                TO LA-STATUS.                    EL6942
01628                                                                   EL6942
01629      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01630      GO TO 4199-EXIT.                                             EL6942
01631                                                                   EL6942
01632  4050-EXIT.                                                       EL6942
01633       EXIT.                                                       EL6942
01634                                  EJECT                            EL6942
01635  4100-CHECK-ACTIVITY.                                             EL6942
01636                                                                   EL6942
01637      PERFORM 3900-CHECK-FOR-ACTIVE-RESENDS THRU 3900-EXIT         EL6942

           .
01647  4199-EXIT.                                                       EL6942
01648      EXIT.                                                        EL6942
01649                                                                   EL6942
01650  4500-GET-APP-RSND-DT-CURR-DT.                                    EL6942
01651                                                                   EL6942
01652      IF LA-RESEND-DATE = LOW-VALUE
01653          GO TO 4500-EXIT.                                         EL6942
01654                                                                   EL6942
01655      IF (LA-RESEND-DATE NOT > W-CURRENT-SAVE)
01657         AND (LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE)
01661          MOVE W-CURRENT-SAVE     TO W-LAST-RESENT-PRINT-DATE      EL6942
01662          MOVE LA-RESEND-DATE     TO W-WORKING-RESEND-DATE.        EL6942
01664                                                                   EL6942
01665  4500-EXIT.                                                       EL6942
01666       EXIT.                                                       EL6942
01667                                                                   EL6942
01668  4600-GET-APP-RSND-DT-DT-GIVEN.                                   EL6942
01669                                                                   EL6942
01670      IF  LA-RESEND-DATE EQUAL LOW-VALUE
01671          GO TO 4600-EXIT.                                         EL6942
01672                                                                   EL6942
01673      IF  (LA-RESEND-DATE = PI-6942-PRINT-DATE-BIN)
01675          AND (LA-RESEND-DATE > LA-LAST-RESENT-PRINT-DATE)
01679          MOVE PI-6942-PRINT-DATE-BIN                              EL6942
01680                                  TO W-LAST-RESENT-PRINT-DATE      EL6942
01681          MOVE LA-RESEND-DATE     TO W-WORKING-RESEND-DATE.        EL6942
01683                                                                   EL6942
01684  4600-EXIT.                                                       EL6942
01685       EXIT.                                                       EL6942
01686                                  EJECT                            EL6942
01687  6000-REPRINT-CHECKS.                                             EL6942
01688                                                                   EL6942
01689      IF  PI-6942-LETTER-TYPE = 'I'                                EL6942
01690          IF  LA-LAST-RESENT-PRINT-DATE GREATER THAN LOW-VALUES    EL6942
01691                  OR                                               EL6942
01692              LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES               EL6942
01693              GO TO 6299-EXIT                                      EL6942
01694          ELSE                                                     EL6942
01695              IF  LA-INITIAL-PRINT-DATE NOT EQUAL                  EL6942
01696                      PI-6942-PRINT-DATE-BIN                       EL6942
01697                  GO TO 6299-EXIT                                  EL6942
01698              ELSE                                                 EL6942
01699                  NEXT SENTENCE                                    EL6942
01700      ELSE                                                         EL6942
01701          IF  PI-6942-LETTER-TYPE = 'R'                            EL6942
01702              IF  LA-LAST-RESENT-PRINT-DATE = LOW-VALUES           EL6942
01703                  GO TO 6299-EXIT                                  EL6942
01704              ELSE                                                 EL6942
01705                  IF  LA-LAST-RESENT-PRINT-DATE NOT EQUAL          EL6942
01706                          PI-6942-PRINT-DATE-BIN                   EL6942
01707                      GO TO 6299-EXIT                              EL6942
01708                  ELSE                                             EL6942
01709                      NEXT SENTENCE                                EL6942
01710          ELSE                                                     EL6942
01711              IF  LA-LAST-RESENT-PRINT-DATE = LOW-VALUES           EL6942
01712                  IF  LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES       EL6942
01713                      GO TO 6299-EXIT                              EL6942
01714                  ELSE                                             EL6942
01715                      IF  LA-INITIAL-PRINT-DATE NOT EQUAL          EL6942
01716                              PI-6942-PRINT-DATE-BIN               EL6942
01717                          GO TO 6299-EXIT                          EL6942
01718              ELSE                                                 EL6942
01719                  IF  LA-LAST-RESENT-PRINT-DATE NOT EQUAL          EL6942
01720                          PI-6942-PRINT-DATE-BIN                   EL6942
01721                      GO TO 6299-EXIT.                             EL6942
01722                                                                   EL6942
01723      GO TO 6010-BY-PROCESSOR                                      EL6942
01724            6020-BY-LETTER                                         EL6942
01725            6030-BY-KEY                                            EL6942
01726            6040-BY-ARCHIVE                                        EL6942
01727            6050-BY-CONTROL-ENTRY                                  EL6942
01728                                  DEPENDING ON W-PROCESSING-SW.    EL6942
01729      GO TO 6299-EXIT.                                             EL6942
01730                                  EJECT                            EL6942
01731  6010-BY-PROCESSOR.                                               EL6942
01732                                                                   EL6942
01733      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01734          GO TO 6299-EXIT.                                         EL6942
01735                                                                   EL6942
01736      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
01737              AND                                                  EL6942
01738          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01739          GO TO 6299-EXIT.                                         EL6942
01740                                                                   EL6942
01741      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01742              AND                                                  EL6942
01743          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01744          GO TO 6299-EXIT.                                         EL6942
01745                                                                   EL6942
01746      IF  LA-CARRIER-A4 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01747          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01748          GO TO 6299-EXIT.                                         EL6942
01749                                                                   EL6942
01750      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01751          GO TO 6010-CONTINUE.                                     EL6942
01752                                                                   EL6942
01753      IF  LA-GROUPING-A4 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01754          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01755          GO TO 6299-EXIT.                                         EL6942
01756                                                                   EL6942
01757      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01758          GO TO 6010-CONTINUE.                                     EL6942
01759                                                                   EL6942
01760      IF  LA-STATE-A4 LESS THAN PI-6942-PRINT-STATE                EL6942
01761          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01762          GO TO 6299-EXIT.                                         EL6942
01763                                                                   EL6942
01764      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01765          GO TO 6010-CONTINUE.                                     EL6942
01766                                                                   EL6942
01767      IF  LA-ACCOUNT-A4 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01768          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01769          GO TO 6299-EXIT.                                         EL6942
01770                                                                   EL6942
01771  6010-CONTINUE.                                                   EL6942
01772                                                                   EL6942
01773      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01774                                                                   EL6942
01775      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01776      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01777                                                                   EL6942
01778      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01779          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01780              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01781          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01782                                                                   EL6942
01783      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01784          W-COPIES TIMES.                                          EL6942
01785                                                                      CL**7
01786      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01787                                                                      CL**7
01788      GO TO 6299-EXIT.                                             EL6942
01789                                                                   EL6942
01790  6010-EXIT.                                                       EL6942
01791       EXIT.                                                       EL6942
01792                                  EJECT                            EL6942
01793  6020-BY-LETTER.                                                  EL6942
01794                                                                   EL6942
01795      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01796              OR                                                   EL6942
01797          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01798          GO TO 6299-EXIT.                                         EL6942
01799                                                                   EL6942
01800      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
01801          GO TO 6020-CONTINUE.                                     EL6942
01802                                                                   EL6942
01803      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01804          GO TO 6299-EXIT.                                         EL6942
01805                                                                   EL6942
01806      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01807          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01808          GO TO 6299-EXIT.                                         EL6942
01809                                                                   EL6942
01810      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01811          GO TO 6020-CONTINUE.                                     EL6942
01812                                                                   EL6942
01813      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01814          GO TO 6299-EXIT.                                         EL6942
01815                                                                   EL6942
01816      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01817          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01818          GO TO 6299-EXIT.                                         EL6942
01819                                                                   EL6942
01820      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01821          GO TO 6020-CONTINUE.                                     EL6942
01822                                                                   EL6942
01823      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE                EL6942
01824          GO TO 6299-EXIT.                                         EL6942
01825                                                                   EL6942
01826      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE                EL6942
01827          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01828          GO TO 6299-EXIT.                                         EL6942
01829                                                                   EL6942
01830      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01831          GO TO 6020-CONTINUE.                                     EL6942
01832                                                                   EL6942
01833      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01834          GO TO 6299-EXIT.                                         EL6942
01835                                                                   EL6942
01836      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01837          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01838          GO TO 6299-EXIT.                                         EL6942
01839                                                                   EL6942
01840  6020-CONTINUE.                                                   EL6942
01841                                                                   EL6942
01842      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01843                                                                   EL6942
01844      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01845      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01846                                                                   EL6942
01847      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01848          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01849              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01850          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01851                                                                   EL6942
01852      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01853          W-COPIES TIMES.                                          EL6942
01854                                                                      CL**7
01855      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01856                                                                      CL**7
01857      GO TO 6299-EXIT.                                             EL6942
01858                                                                   EL6942
01859  6020-EXIT.                                                       EL6942
01860       EXIT.                                                       EL6942
01861                                  EJECT                            EL6942
01862  6030-BY-KEY.                                                     EL6942
01863                                                                   EL6942
01864      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01865              OR                                                   EL6942
01866          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01867              OR                                                   EL6942
01868          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01869          GO TO 6299-EXIT.                                         EL6942
01870                                                                   EL6942
01871      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER            EL6942
01872          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01873          GO TO 6299-EXIT.                                         EL6942
01874                                                                   EL6942
01875      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
01876          GO TO 6030-CONTINUE.                                     EL6942
01877                                                                   EL6942
01878      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING          EL6942
01879          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01880          GO TO 6299-EXIT.                                         EL6942
01881                                                                   EL6942
01882      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
01883          GO TO 6030-CONTINUE.                                     EL6942
01884                                                                   EL6942
01885      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE                EL6942
01886          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01887          GO TO 6299-EXIT.                                         EL6942
01888                                                                   EL6942
01889      IF  PI-6942-PRINT-BY-STATE                                   EL6942
01890          GO TO 6030-CONTINUE.                                     EL6942
01891                                                                   EL6942
01892      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
01893          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
01894          GO TO 6299-EXIT.                                         EL6942
01895                                                                   EL6942
01896  6030-CONTINUE.                                                   EL6942
01897                                                                   EL6942
01898      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01899                                                                   EL6942
01900      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01901      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01902                                                                   EL6942
01903      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01904          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01905              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01906          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01907                                                                   EL6942
01908      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01909          W-COPIES TIMES.                                          EL6942
01910                                                                      CL**7
01911      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01912                                                                      CL**7
01913      GO TO 6299-EXIT.                                             EL6942
01914                                                                   EL6942
01915  6030-EXIT.                                                       EL6942
01916       EXIT.                                                       EL6942
01917                                  EJECT                            EL6942
01918  6040-BY-ARCHIVE.                                                 EL6942
01919                                                                   EL6942
01920      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01921              OR                                                   EL6942
01922          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
01923              OR                                                   EL6942
01924          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01925          GO TO 6299-EXIT.                                         EL6942
01926                                                                   EL6942
01927      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
01928                                                                   EL6942
01929      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
01930      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
01931                                                                   EL6942
01932      IF  W-THIS-IS-FIRST-FORM                                     EL6942
01933          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
01934              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
01935          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
01936                                                                   EL6942
01937      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
01938          W-COPIES TIMES.                                          EL6942
01939                                                                      CL**7
01940      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
01941                                                                      CL**7
01942      GO TO 6299-EXIT.                                             EL6942
01943                                                                   EL6942
01944  6040-EXIT.                                                       EL6942
01945       EXIT.                                                       EL6942
01946                                  EJECT                            EL6942
01947  6050-BY-CONTROL-ENTRY.                                           EL6942
01948                                                                   EL6942
01949      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES              EL6942
01950              AND                                                  EL6942
01951          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
01952          GO TO 6299-EXIT.                                         EL6942
01953                                                                   EL6942
01954      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
01955              AND                                                  EL6942
01956          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
01957          GO TO 6299-EXIT.                                         EL6942
01958                                                                   EL6942
01959      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
01960              AND                                                  EL6942
01961          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01962          GO TO 6299-EXIT.                                         EL6942
01963                                                                   EL6942
01964      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
01965              AND                                                  EL6942
01966          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
01967          GO TO 6299-EXIT.                                         EL6942
01968                                                                   EL6942
01969      IF  W-PROCESS-BY-KEY                                         EL6942
01970          GO TO 6050-CONTINUE.                                     EL6942
01971                                                                   EL6942
01972      IF  PI-6942-PRINT-BY-KEY                                     EL6942
01973          IF  PI-6942-PRINT-BY-CARRIER                             EL6942
01974              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER     EL6942
01975                  NEXT SENTENCE                                    EL6942
01976              ELSE                                                 EL6942
01977                  GO TO 6299-EXIT                                  EL6942
01978          ELSE                                                     EL6942
01979              IF  PI-6942-PRINT-BY-GROUPING                        EL6942
01980                  IF  LA-CARRIER-A3 IS EQUAL                       EL6942
01981                          PI-6942-PRINT-CARRIER                    EL6942
01982                          AND                                      EL6942
01983                      LA-GROUPING-A3 IS EQUAL                      EL6942
01984                          PI-6942-PRINT-GROUPING                   EL6942
01985                      NEXT SENTENCE                                EL6942
01986                  ELSE                                             EL6942
01987                      GO TO 6299-EXIT                              EL6942
01988              ELSE                                                 EL6942
01989                  IF  PI-6942-PRINT-BY-STATE                       EL6942
01990                                                                   EL6942
01991                      IF  LA-CARRIER-A3 IS EQUAL                   EL6942
01992                              PI-6942-PRINT-CARRIER                EL6942
01993                              AND                                  EL6942
01994                          LA-GROUPING-A3 IS EQUAL                  EL6942
01995                              PI-6942-PRINT-GROUPING               EL6942
01996                              AND                                  EL6942
01997                          LA-STATE-A3 IS EQUAL                     EL6942
01998                              PI-6942-PRINT-STATE                  EL6942
01999                          NEXT SENTENCE                            EL6942
02000                      ELSE                                         EL6942
02001                          GO TO 6299-EXIT                          EL6942
02002                  ELSE                                             EL6942
02003                      IF  PI-6942-PRINT-BY-ACCOUNT                 EL6942
02004                          IF  LA-CARRIER-A3 IS EQUAL               EL6942
02005                                  PI-6942-PRINT-CARRIER            EL6942
02006                                  AND                              EL6942
02007                              LA-GROUPING-A3 IS EQUAL              EL6942
02008                                  PI-6942-PRINT-GROUPING           EL6942
02009                                  AND                              EL6942
02010                              LA-STATE-A3 IS EQUAL                 EL6942
02011                                  PI-6942-PRINT-STATE              EL6942
02012                                  AND                              EL6942
02013                              LA-ACCOUNT-A3 IS EQUAL               EL6942
02014                                  PI-6942-PRINT-ACCOUNT            EL6942
02015                              NEXT SENTENCE                        EL6942
02016                          ELSE                                     EL6942
02017                              GO TO 6299-EXIT.                     EL6942
02018                                                                   EL6942
02019  6050-CONTINUE.                                                   EL6942
02020                                                                   EL6942
02021      MOVE LA-NO-OF-COPIES        TO W-COPIES.                     EL6942
02022                                                                   EL6942
02023      MOVE W-CURRENT-SAVE         TO LA-INITIAL-PRINT-DATE.        EL6942
02024      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.             EL6942
02025                                                                   EL6942
02026      IF  W-THIS-IS-FIRST-FORM                                     EL6942
02027          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT        EL6942
02028              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
02029          MOVE 'Y'                TO W-FIRST-FORM-SW.              EL6942
02030                                                                   EL6942
02031      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT            EL6942
02032          W-COPIES TIMES.                                          EL6942
02033      PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT.           EL6942
02034      GO TO 6299-EXIT.                                             EL6942
02035                                                                   EL6942
02036  6050-EXIT.                                                       EL6942
02037       EXIT.                                                       EL6942
02038                                                                   EL6942
02039  6299-EXIT.                                                       EL6942
02040       EXIT.                                                       EL6942
02041                                  EJECT                            EL6942
02042  6300-LABEL-CHECKS.                                               EL6942
02043                                                                   EL6942
02044      IF  FIRST-TIME                                               EL6942
02045          PERFORM 6400-ALIGNMENT-PRINT THRU 6450-EXIT.             EL6942
02046                                                                   EL6942
02047      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            EL6942
02048                                                                   EL6942
02049      IF  LA-INITIAL-PRINT-DATE NOT EQUAL PI-6942-PRINT-DATE-BIN   EL6942
02050              AND                                                  EL6942
02051          LA-LAST-RESENT-PRINT-DATE NOT EQUAL                      EL6942
02052              PI-6942-PRINT-DATE-BIN                               EL6942
02053          GO TO 6399-EXIT.                                         EL6942
02054                                                                   EL6942
02055      IF  PI-6942-LETTER-TYPE = 'I'                                EL6942
02056          IF  LA-LAST-RESENT-PRINT-DATE GREATER THAN LOW-VALUES    EL6942
02057              GO TO 6399-EXIT                                      EL6942
02058          ELSE                                                     EL6942
02059              IF  LA-INITIAL-PRINT-DATE NOT EQUAL                  EL6942
02060                      PI-6942-PRINT-DATE-BIN                       EL6942
02061                  GO TO 6399-EXIT                                  EL6942
02062              ELSE                                                 EL6942
02063                  NEXT SENTENCE                                    EL6942
02064      ELSE                                                         EL6942
02065          IF  PI-6942-LETTER-TYPE = 'R'                            EL6942
02066              IF  LA-LAST-RESENT-PRINT-DATE = LOW-VALUES           EL6942
02067                  GO TO 6399-EXIT                                  EL6942
02068              ELSE                                                 EL6942
02069                  IF  LA-LAST-RESENT-PRINT-DATE NOT EQUAL          EL6942
02070                          PI-6942-PRINT-DATE-BIN                   EL6942
02071                      GO TO 6399-EXIT.                             EL6942
02072                                                                   EL6942
02073      GO TO 6310-BY-PROCESSOR                                      EL6942
02074            6320-BY-LETTER                                         EL6942
02075            6330-BY-KEY                                            EL6942
02076            6340-BY-ARCHIVE                                        EL6942
02077            6350-BY-CONTROL-ENTRY                                  EL6942
02078                                  DEPENDING ON W-PROCESSING-SW.    EL6942
02079      GO TO 6399-EXIT.                                             EL6942
02080                                  EJECT                            EL6942
02081  6310-BY-PROCESSOR.                                               EL6942
02082                                                                   EL6942
02083      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
02084          GO TO 6399-EXIT.                                         EL6942
02085                                                                   EL6942
02086      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
02087              AND                                                  EL6942
02088          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
02089          GO TO 6399-EXIT.                                         EL6942
02090                                                                   EL6942
02091      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
02092              AND                                                  EL6942
02093          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
02094          GO TO 6399-EXIT.                                         EL6942
02095                                                                   EL6942
02096      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
02097          GO TO 6310-CONTINUE.                                     EL6942
02098                                                                   EL6942
02099      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER            EL6942
02100          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02101          GO TO 6399-EXIT.                                         EL6942
02102                                                                   EL6942
02103      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
02104          GO TO 6310-CONTINUE.                                     EL6942
02105                                                                   EL6942
02106      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING          EL6942
02107          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02108          GO TO 6399-EXIT.                                         EL6942
02109                                                                   EL6942
02110      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
02111          GO TO 6310-CONTINUE.                                     EL6942
02112                                                                   EL6942
02113      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE                EL6942
02114          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02115          GO TO 6399-EXIT.                                         EL6942
02116                                                                   EL6942
02117      IF  PI-6942-PRINT-BY-STATE                                   EL6942
02118          GO TO 6310-CONTINUE.                                     EL6942
02119                                                                   EL6942
02120      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
02121          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02122          GO TO 6399-EXIT.                                         EL6942
02123                                                                   EL6942
02124  6310-CONTINUE.                                                   EL6942
02125                                                                   EL6942
02126      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.                CL**3
02127      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.           EL6942
02128      GO TO 6399-EXIT.                                             EL6942
02129                                                                   EL6942
02130  6310-EXIT.                                                       EL6942
02131       EXIT.                                                       EL6942
02132                                  EJECT                            EL6942
02133  6320-BY-LETTER.                                                  EL6942
02134                                                                   EL6942
02135      IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
02136              OR                                                   EL6942
02137          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
02138          GO TO 6399-EXIT.                                         EL6942
02139                                                                   EL6942
02140      IF  NOT PI-6942-PRINT-BY-KEY                                 EL6942
02141          GO TO 6320-CONTINUE.                                     EL6942
02142                                                                   EL6942
02143      IF  LA-CARRIER-A3 LESS THAN PI-6942-PRINT-CARRIER            EL6942
02144          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02145          GO TO 6399-EXIT.                                         EL6942
02146                                                                   EL6942
02147      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
02148          GO TO 6320-CONTINUE.                                     EL6942
02149                                                                   EL6942
02150      IF  LA-GROUPING-A3 LESS THAN PI-6942-PRINT-GROUPING          EL6942
02151          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02152          GO TO 6399-EXIT.                                         EL6942
02153                                                                   EL6942
02154      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
02155          GO TO 6320-CONTINUE.                                     EL6942
02156                                                                   EL6942
02157      IF  LA-STATE-A3 LESS THAN PI-6942-PRINT-STATE                EL6942
02158          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02159          GO TO 6399-EXIT.                                         EL6942
02160                                                                   EL6942
02161      IF  PI-6942-PRINT-BY-STATE                                   EL6942
02162          GO TO 6320-CONTINUE.                                     EL6942
02163                                                                   EL6942
02164      IF  LA-ACCOUNT-A3 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
02165          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02166          GO TO 6399-EXIT.                                         EL6942
02167                                                                   EL6942
02168  6320-CONTINUE.                                                   EL6942
02169                                                                   EL6942
02170      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.                CL**3
02171      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.           EL6942
02172      GO TO 6399-EXIT.                                             EL6942
02173                                                                   EL6942
02174  6320-EXIT.                                                       EL6942
02175       EXIT.                                                       EL6942
02176                                  EJECT                            EL6942
02177  6330-BY-KEY.                                                     EL6942
02178                                                                   EL6942
02179      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
02180              OR                                                   EL6942
02181          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
02182              OR                                                   EL6942
02183          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
02184          GO TO 6399-EXIT.                                         EL6942
02185                                                                   EL6942
02186      IF  LA-CARRIER-A5 LESS THAN PI-6942-PRINT-CARRIER            EL6942
02187          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02188          GO TO 6399-EXIT.                                         EL6942
02189                                                                   EL6942
02190      IF  PI-6942-PRINT-BY-CARRIER                                 EL6942
02191          GO TO 6330-CONTINUE.                                     EL6942
02192                                                                   EL6942
02193      IF  LA-GROUPING-A5 LESS THAN PI-6942-PRINT-GROUPING          EL6942
02194          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02195          GO TO 6399-EXIT.                                         EL6942
02196                                                                   EL6942
02197      IF  PI-6942-PRINT-BY-GROUPING                                EL6942
02198          GO TO 6330-CONTINUE.                                     EL6942
02199                                                                   EL6942
02200      IF  LA-STATE-A5 LESS THAN PI-6942-PRINT-STATE                EL6942
02201          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02202          GO TO 6399-EXIT.                                         EL6942
02203                                                                   EL6942
02204      IF  PI-6942-PRINT-BY-STATE                                   EL6942
02205          GO TO 6330-CONTINUE.                                     EL6942
02206                                                                   EL6942
02207      IF  LA-ACCOUNT-A5 LESS THAN PI-6942-PRINT-ACCOUNT            EL6942
02208          MOVE 'Y'                TO W-ENDBR-SW                    EL6942
02209          GO TO 6399-EXIT.                                         EL6942
02210                                                                   EL6942
02211  6330-CONTINUE.                                                   EL6942
02212                                                                   EL6942
02213      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.                CL**3
02214      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.           EL6942
02215      GO TO 6399-EXIT.                                             EL6942
02216                                                                   EL6942
02217  6330-EXIT.                                                       EL6942
02218       EXIT.                                                       EL6942
02219                                  EJECT                            EL6942
02220  6340-BY-ARCHIVE.                                                 EL6942
02221                                                                   EL6942
02222      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
02223              OR                                                   EL6942
02224          LA-PRINT-ONLY-WHEN-CNTL-GIVEN                            EL6942
02225              OR                                                   EL6942
02226          LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
02227          GO TO 6399-EXIT.                                         EL6942
02228                                                                   EL6942
02229      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.                CL**3
02230      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.           EL6942
02231      GO TO 6399-EXIT.                                             EL6942
02232                                                                   EL6942
02233  6340-EXIT.                                                       EL6942
02234       EXIT.                                                       EL6942
02235                                  EJECT                            EL6942
02236  6350-BY-CONTROL-ENTRY.                                           EL6942
02237                                                                   EL6942
02238      IF  PI-6942-PRINT-PROCESSOR GREATER THAN SPACES              EL6942
02239              AND                                                  EL6942
02240          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
02241          GO TO 6399-EXIT.                                         EL6942
02242                                                                   EL6942
02243      IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN                            EL6942
02244              AND                                                  EL6942
02245          PI-6942-PRINT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD        EL6942
02246          GO TO 6399-EXIT.                                         EL6942
02247                                                                   EL6942
02248      IF  PI-6942-LETTER-FORM GREATER THAN SPACES                  EL6942
02249              AND                                                  EL6942
02250          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
02251          GO TO 6399-EXIT.                                         EL6942
02252                                                                   EL6942
02253      IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL6942
02254              AND                                                  EL6942
02255          PI-6942-LETTER-FORM NOT EQUAL LA-FORM-A3                 EL6942
02256          GO TO 6399-EXIT.                                         EL6942
02257                                                                   EL6942
02258      IF  W-PROCESS-BY-KEY                                         EL6942
02259          GO TO 6350-CONTINUE.                                     EL6942
02260                                                                   EL6942
02261      IF  PI-6942-PRINT-BY-KEY                                     EL6942
02262          IF  PI-6942-PRINT-BY-CARRIER                             EL6942
02263              IF  LA-CARRIER-A3 IS EQUAL PI-6942-PRINT-CARRIER     EL6942
02264                  NEXT SENTENCE                                    EL6942
02265              ELSE                                                 EL6942
02266                  GO TO 6399-EXIT                                  EL6942
02267          ELSE                                                     EL6942
02268              IF  PI-6942-PRINT-BY-GROUPING                        EL6942
02269                  IF  LA-CARRIER-A3 IS EQUAL                       EL6942
02270                          PI-6942-PRINT-CARRIER                    EL6942
02271                          AND                                      EL6942
02272                      LA-GROUPING-A3 IS EQUAL                      EL6942
02273                          PI-6942-PRINT-GROUPING                   EL6942
02274                      NEXT SENTENCE                                EL6942
02275                  ELSE                                             EL6942
02276                      GO TO 6399-EXIT                              EL6942
02277              ELSE                                                 EL6942
02278                  IF  PI-6942-PRINT-BY-STATE                       EL6942
02279                      IF  LA-CARRIER-A3 IS EQUAL                   EL6942
02280                              PI-6942-PRINT-CARRIER                EL6942
02281                              AND                                  EL6942
02282                          LA-GROUPING-A3 IS EQUAL                  EL6942
02283                              PI-6942-PRINT-GROUPING               EL6942
02284                              AND                                  EL6942
02285                          LA-STATE-A3 IS EQUAL                     EL6942
02286                              PI-6942-PRINT-STATE                  EL6942
02287                          NEXT SENTENCE                            EL6942
02288                      ELSE                                         EL6942
02289                          GO TO 6399-EXIT                          EL6942
02290                  ELSE                                             EL6942
02291                      IF  PI-6942-PRINT-BY-ACCOUNT                 EL6942
02292                          IF  LA-CARRIER-A3 IS EQUAL               EL6942
02293                                  PI-6942-PRINT-CARRIER            EL6942
02294                                  AND                              EL6942
02295                              LA-GROUPING-A3 IS EQUAL              EL6942
02296                                  PI-6942-PRINT-GROUPING           EL6942
02297                                  AND                              EL6942
02298                              LA-STATE-A3 IS EQUAL                 EL6942
02299                                  PI-6942-PRINT-STATE              EL6942
02300                                  AND                              EL6942
02301                              LA-ACCOUNT-A3 IS EQUAL               EL6942
02302                                  PI-6942-PRINT-ACCOUNT            EL6942
02303                              NEXT SENTENCE                        EL6942
02304                          ELSE                                     EL6942
02305                              GO TO 6399-EXIT.                     EL6942
02306                                                                   EL6942
02307  6350-CONTINUE.                                                   EL6942
02308                                                                   EL6942
02309      PERFORM 7100-CREATE-PRINT-TABLES THRU 7100-EXIT.                CL**3
02310      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7200-EXIT.           EL6942
02311      GO TO 6399-EXIT.                                             EL6942
02312                                                                   EL6942
02313  6350-EXIT.                                                       EL6942
02314       EXIT.                                                       EL6942
02315                                                                   EL6942
02316  6399-EXIT.                                                       EL6942
02317       EXIT.                                                       EL6942
02318                                  EJECT                            EL6942
02319  6400-ALIGNMENT-PRINT.                                            EL6942
02320                                                                   EL6942
02321      MOVE ALL '*'                TO W-LABEL-LINES (1)                CL**3
02322                                     W-LABEL-LINES (2)                CL**3
02323                                     W-LABEL-LINES (3).               CL**3
02324                                                                      CL**3
02325      MOVE SPACES                 TO W-LABEL-LINES (4)                CL**3
02326                                     W-LABEL-LINES (5)                CL**3
02327                                     W-LABEL-LINES (6).               CL**3
02328                                                                   EL6942
02329      IF  PI-6942-ALIGNMENT-COPIES GREATER THAN +0                 EL6942
02330          PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT                EL6942
02331              PI-6942-ALIGNMENT-COPIES TIMES                       EL6942
02332      ELSE                                                         EL6942
02333          PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT 6 TIMES.       EL6942
02334                                                                   EL6942
02335  6450-EXIT.                                                       EL6942
02336       EXIT.                                                       EL6942
02337                                                                   EL6942
02338  6480-MOVE-TO-PRINT.                                              EL6942
02339                                                                   EL6942
02340      MOVE SPACES                 TO WS-PASSED-CNTL-CHAR.          EL6942
02341      MOVE W-LABEL-LINES (1)      TO WS-PASSED-DATA.               EL6942
02342      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
02343      MOVE W-LABEL-LINES (2)      TO WS-PASSED-DATA.               EL6942
02344      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
02345      MOVE W-LABEL-LINES (3)      TO WS-PASSED-DATA.               EL6942
02346      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
02347      MOVE W-LABEL-LINES (4)      TO WS-PASSED-DATA.               EL6942
02348      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
02349      MOVE W-LABEL-LINES (5)      TO WS-PASSED-DATA.                  CL**2
02350      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
02351      MOVE W-LABEL-LINES (6)      TO WS-PASSED-DATA.               EL6942
02352      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6942
02353                                                                   EL6942
02354  6499-EXIT.                                                       EL6942
02355       EXIT.                                                       EL6942
02356                                  EJECT                            EL6942
02357  7100-CREATE-PRINT-TABLES.                                        EL6942
02358                                                                   EL6942
02359      MOVE ZEROS                  TO W-NUM-OF-TEXT-RECORDS.        EL6942
02360      MOVE LOW-VALUES             TO W-ARCT-KEY.                   EL6942
02361      MOVE PI-COMPANY-CD          TO W-ARCT-COMPANY-CD.            EL6942
02362      MOVE SPACES                 TO WS-PROG-END                   EL6942
02363      MOVE SPACES                 TO W-WORK-TABLE.                 EL6942
02364      SET W-WK-NDX                TO W-ZEROS.                      EL6942
02365      MOVE LA-ARCHIVE-NO          TO W-ARCT-NUMBER.                EL6942
02366      MOVE +1                     TO W-ARCT-SEQ.                   EL6942
02367                                                                   EL6942
02368      IF  W-PRINT-LABELS                                           EL6942
02369          MOVE +0                 TO W-ARCT-SEQ                       CL**3
02370          MOVE '1'                TO W-ARCT-REC-ID                 EL6942
02371          SET W-L-NDX             TO 1                             EL6942
02372      ELSE                                                         EL6942
02373          MOVE '2'                TO W-ARCT-REC-ID.                EL6942
02374                                                                   EL6942
02375      EXEC CICS HANDLE CONDITION                                   EL6942
02376           NOTFND  (7100-EXIT)                                     EL6942
02377           ENDFILE (7100-EXIT)                                     EL6942
02378           NOTOPEN (8890-ARCT-NOT-OPEN)                            EL6942
02379      END-EXEC.                                                    EL6942
02380                                                                   EL6942
02381      IF  W-PRINT-LABELS                                              CL**3
02382           PERFORM 7110-READ-NEXT THRU 7110-EXIT                      CL**3
02383      ELSE                                                            CL**3
02384           ADD +1 TO W-LETTER-TOTALS                                  CL**3
02385           PERFORM 7110-READ-NEXT THRU 7110-EXIT                      CL**3
02386             LA-NO-OF-TEXT-RECORDS TIMES.                             CL**3
02387                                                                   EL6942
02388  7100-EXIT.                                                       EL6942
02389      EXIT.                                                        EL6942
02390                                                                   EL6942
02391  7110-READ-NEXT.                                                  EL6942
02392                                                                   EL6942
02393      EXEC CICS READ                                               EL6942
02394           DATASET (W-ARCT-ID)                                     EL6942
02395           RIDFLD  (W-ARCT-KEY)                                    EL6942
02396           SET     (ADDRESS OF LETTER-ARCHIVE-TEXT)                   CL**6
02397      END-EXEC.                                                    EL6942
02398                                                                   EL6942
02399      IF  W-PRINT-LABELS                                           EL6942
02400          IF (LT-LETTER-TEXT (1) EQUAL SPACES OR LOW-VALUES)  AND     CL**5
02401             (LT-LETTER-TEXT (2) EQUAL SPACES OR LOW-VALUES)  AND     CL**5
02402             (LT-LETTER-TEXT (3) EQUAL SPACES OR LOW-VALUES)  AND     CL**5
02403             (LT-LETTER-TEXT (4) EQUAL SPACES OR LOW-VALUES)  AND     CL**5
02404             (LT-LETTER-TEXT (5) EQUAL SPACES OR LOW-VALUES)  AND     CL**5
02405             (LT-LETTER-TEXT (6) EQUAL SPACES OR LOW-VALUES)          CL**5
02406              GO TO 7110-EXIT                                         CL**5
02407          ELSE                                                        CL**5
02408              ADD +1 TO W-LETTER-TOTALS                               CL**5
02409              MOVE LT-LETTER-TEXT (1) TO W-LABEL-LINES (1)            CL**5
02410              MOVE LT-LETTER-TEXT (2) TO W-LABEL-LINES (2)            CL**5
02411              MOVE LT-LETTER-TEXT (3) TO W-LABEL-LINES (3)            CL**5
02412              MOVE LT-LETTER-TEXT (4) TO W-LABEL-LINES (4)            CL**5
02413              MOVE LT-LETTER-TEXT (5) TO W-LABEL-LINES (5)            CL**5
02414              MOVE LT-LETTER-TEXT (6) TO W-LABEL-LINES (6)            CL**5
02415              PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT               CL**5
02416              GO TO 7110-EXIT.                                        CL**5
02417                                                                   EL6942
02418      PERFORM 7120-PROCESS-RECORD THRU 7120-EXIT                   EL6942
02419              VARYING                                              EL6942
02420          LT-NDX FROM +1 BY +1                                     EL6942
02421              UNTIL                                                EL6942
02422          LT-NDX GREATER THAN LT-NUM-LINES-ON-RECORD.              EL6942
02423                                                                   EL6942
02424      ADD +1                      TO W-ARCT-SEQ.                   EL6942
02425                                                                   EL6942
02426  7110-EXIT.                                                       EL6942
02427      EXIT.                                                        EL6942
02428                                  EJECT                            EL6942
02429  7120-PROCESS-RECORD.                                             EL6942
02430                                                                   EL6942
02431      SET W-WK-NDX UP BY +1.                                       EL6942
02432      ADD +1                      TO W-NUM-OF-TEXT-RECORDS.        EL6942
02433      MOVE LT-LETTER-TEXT (LT-NDX)                                 EL6942
02434                                  TO W-WORK-LINE (W-WK-NDX).       EL6942
02435                                                                   EL6942
02436  7120-EXIT.                                                       EL6942
02437      EXIT.                                                        EL6942
02438                                  EJECT                            EL6942
02439  7200-PRINT-ARCHIVE-RECORDS.                                      EL6942
02440                                                                   EL6942
02441      PERFORM 7220-PROCESS-TABLE THRU 7220-EXIT                    EL6942
02442              VARYING                                              EL6942
02443          W-WK-NDX FROM 1 BY 1                                     EL6942
02444              UNTIL                                                EL6942
02445          W-WK-NDX GREATER THAN W-NUM-OF-TEXT-RECORDS.             EL6942
02446                                                                   EL6942
02447  7200-EXIT.                                                       EL6942
02448      EXIT.                                                        EL6942
02449                                  EJECT                            EL6942
02450  7220-PROCESS-TABLE.                                              EL6942
02451                                                                   EL6942
02452      IF  W-SKIP-CONTROL (W-WK-NDX) GREATER THAN '00'              EL6942
02453              AND                                                  EL6942
02454          W-SKIP-CONTROL (W-WK-NDX) LESS THAN '99'                 EL6942
02455          MOVE SPACES             TO WS-PRINT-AREA                 EL6942
02456          MOVE W-SKIP-CONTROL (W-WK-NDX)                           EL6942
02457                                  TO W-SKIP                        EL6942
02458          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT W-SKIP TIMES         EL6942
02459      ELSE                                                         EL6942
02460          IF  W-SKIP-TO-NEXT-PAGE (W-WK-NDX)                       EL6942
02461              IF  W-TEXT-LINE (W-WK-NDX) EQUAL SPACES              EL6942
02462                  MOVE '1'        TO WS-PRINT-AREA                 EL6942
02463              ELSE                                                 EL6942
02464                  MOVE '1'        TO WS-PRINT-AREA                 EL6942
02465                  PERFORM ELPRTCVP THRU ELPRTCVP-EXIT              EL6942
02466                  MOVE SPACES     TO WS-PRINT-AREA                 EL6942
02467          ELSE                                                     EL6942
02468              MOVE SPACES         TO WS-PRINT-AREA.                EL6942

031011     IF W-TEXT-LINE (W-WK-NDX) (1:6) = '&&&&&&'
031011        CONTINUE
031011     ELSE
031011        MOVE W-TEXT-LINE (W-WK-NDX) TO W-AD-PRINT-AREA
031011        MOVE W-ADJUST-AREA          TO WS-PASSED-DATA
031011        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
031011     END-IF

           .
02475  7220-EXIT.                                                       EL6942
02476      EXIT.                                                        EL6942
02477                                 EJECT                             EL6942
02478  7300-UPDATE-ARCHIVE-HEADER.                                      EL6942
02479                                                                   EL6942
02480      EXEC CICS HANDLE CONDITION                                   EL6942
02481          DUPKEY (7399-EXIT)                                       EL6942
02482      END-EXEC.                                                    EL6942
02483                                                                   EL6942
02484      EXEC CICS READ                                               EL6942
02485           DATASET (W-ARCH-ID)                                     EL6942
02486           RIDFLD  (LA-CONTROL-PRIMARY)                            EL6942
02487           SET     (ADDRESS OF L-LETTER-ARCHIVE)                      CL**6
02488           UPDATE                                                  EL6942
02489      END-EXEC.                                                    EL6942
02490                                                                   EL6942
02491      EXEC CICS REWRITE                                            EL6942
02492          DATASET (W-ARCH-ID)                                      EL6942
02493          FROM    (LETTER-ARCHIVE)                                 EL6942
02494      END-EXEC.                                                    EL6942
02495                                                                   EL6942
02496  7399-EXIT.                                                       EL6942
02497      EXIT.                                                        EL6942
02498                                  EJECT                            EL6942
02499  8800-ARCH4-NOT-OPEN.                                             EL6942
02500                                                                   EL6942
02501      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH4'                EL6942
02502                                  TO W-ERROR-LINE.                 EL6942
02503      PERFORM 0400-SEND-TEXT.                                      EL6942
02504      GO TO 0200-END-DATA.                                         EL6942
02505                                  EJECT                            EL6942
02506  8810-ARCH3-NOT-OPEN.                                             EL6942
02507                                                                   EL6942
02508      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH3'                EL6942
02509                                  TO W-ERROR-LINE.                 EL6942
02510      PERFORM 0400-SEND-TEXT.                                      EL6942
02511      GO TO 0200-END-DATA.                                         EL6942
02512                                  EJECT                            EL6942
02513  8860-ARCH5-NOT-OPEN.                                             EL6942
02514                                                                   EL6942
02515      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH5'                EL6942
02516                                  TO W-ERROR-LINE.                 EL6942
02517      PERFORM 0400-SEND-TEXT.                                      EL6942
02518      GO TO 0200-END-DATA.                                         EL6942
02519                                                                   EL6942
02520  8870-ARCH-NOT-OPEN.                                              EL6942
02521                                                                   EL6942
02522      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ERARCH'                 EL6942
02523                                  TO W-ERROR-LINE.                 EL6942
02524      PERFORM 0400-SEND-TEXT.                                      EL6942
02525      GO TO 0200-END-DATA.                                         EL6942
02526                                                                   EL6942
02527  8890-ARCT-NOT-OPEN.                                              EL6942
02528                                                                   EL6942
02529      MOVE 'LETTER ARCHIVE TEXT FILE NOT OPEN -ERARCTT'            EL6942
02530                                  TO W-ERROR-LINE.                 EL6942
02531      PERFORM 0400-SEND-TEXT.                                      EL6942
02532      GO TO 0200-END-DATA.                                         EL6942
02533                                  EJECT                            EL6942
02534  9700-DATE-LINK.                                                  EL6942
02535                                                                   EL6942
02536      EXEC CICS LINK                                               EL6942
02537           PROGRAM  ('ELDATCV')                                       CL**4
02538           COMMAREA (DATE-CONVERSION-DATA)                         EL6942
02539           LENGTH   (DC-COMM-LENGTH)                               EL6942
02540      END-EXEC.                                                    EL6942
02541                                                                   EL6942
02542  9700-EXIT.                                                       EL6942
02543       EXIT.                                                       EL6942
02544                                  EJECT                            EL6942
uktdel*9800-PRINT-ROUTINE.             COPY ELPRTCVP.                   EL6942
uktins 9800-PRINT-ROUTINE.
uktins     COPY ELPRTCVP.
02546                                                                      CL**7
02547  9999-GOBACK.                                                     EL6942
02548                                                                   EL6942
02549      GOBACK.                                                      EL6942
02550                                                                   EL6942
02551  9999-EXIT.                                                       EL6942
02552      EXIT.                                                        EL6942
