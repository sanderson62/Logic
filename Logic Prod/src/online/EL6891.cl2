00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL6891
00003  PROGRAM-ID.                 EL6891.                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 09:58:47.                    CL**3
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL6891
00008 *                                                                 EL6891
00009 *AUTHOR.           LOGIC,INC.                                        CL**3
00010 *                  DALLAS,TEXAS.                                     CL**3
00011                                                                   EL6891
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL6891
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOCIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL6891
00024 *REMARKS. TRANSACTION EXM6 - CHECK FILE LETTER ARCHIVER           EL6891
00025 *       THIS PROGRAM IS ACTIVATED THROUGH A START COMMMAND        EL6891
00026 *       FROM EL690 AND WILL HAVE THE PROGRAM INTERFACE            EL6891
00027 *       BLOCK PASSED.                                             EL6891
00028 *       THIS PROGRAM WILL READ THE CREDIT CHECK QUEUE (ERCHKQ)    EL6891
00029 *       AND THE CREDIT CHECK MASTER (ERCHEK), SELECTING THOSE     EL6891
00030 *       RECORDS CORRESPONDING TO THE COMPANY BEING PROCESSED         CL**3
00031 *       AND CALL EL689 TO CREATE ARCHIVE ENTRIES FOR EACH CHECK   EL6891
00032 *       FOR WHOM LETTERS HAVE NOT ALREADY BEEN GENERATED.         EL6891
00033                                                                   EL6891
00034                                  EJECT                            EL6891
00035  ENVIRONMENT DIVISION.                                            EL6891
00036  DATA DIVISION.                                                   EL6891
00037  WORKING-STORAGE SECTION.                                         EL6891
00038  77  FILLER  PIC X(32) VALUE '********************************'.  EL6891
00039  77  FILLER  PIC X(32) VALUE '*   EL6891 WORKING STORAGE     *'.  EL6891
00040  77  FILLER  PIC X(32) VALUE '******* VMOD=2.004 *************'.     CL**4
00041                                                                   EL6891
00042  01  W-PROGRAM-WORK-AREA.                                         EL6891
00043      12  FILLER                  PIC  X(17)                       EL6891
00044                                  VALUE 'PROGRAM WORK AREA'.       EL6891
00045      12  W-PGM-ID                PIC S9(04)  COMP VALUE +6891.    EL6891
00046      12  W-INCOMING-LINES        PIC S9(04)  COMP VALUE +56.      EL6891
00047      12  W-LET-NDX               PIC S9(04)  COMP VALUE +0.          CL**2
00048      12  W-LINE-COUNT            PIC S9(04)  COMP VALUE +56.      EL6891
00049                                                                   EL6891
00050      12  W-CHKQ-RCRD-CHANGED     PIC S9(07)  COMP-3 VALUE +0.     EL6891
00051      12  W-CHKQ-RCRD-READ        PIC S9(07)  COMP-3 VALUE +0.     EL6891
00052      12  W-CHKQ-RCRD-USED        PIC S9(07)  COMP-3 VALUE +0.     EL6891
00053      12  W-COMBINED-SPACES       PIC S9(07)  COMP-3 VALUE +0.     EL6891
00054      12  W-FATAL-ERRORS          PIC S9(07)  COMP-3 VALUE +0.     EL6891
00055      12  W-LETTERS-CREATED       PIC S9(07)  COMP-3 VALUE +0.     EL6891
00056      12  W-LETTERS-WITH-ERRORS   PIC S9(07)  COMP-3 VALUE +0.     EL6891
00057      12  W-PAGE                  PIC S9(04)  COMP-3 VALUE +0.     EL6891
00058                                                                   EL6891
00059      12  W-CHECK-CONTROL.                                         EL6891
00060          16  FILLER              PIC  X(02).                      EL6891
00061              88  W-CONTROL-NUMBER-PROVIDED   VALUE 'CK'.          EL6891
00062          16  W-CONTROL-NUMBER    PIC S9(08)  COMP.                EL6891
00063      12  W-CHEK-FILE-ID          PIC  X(08)  VALUE 'ERCHEK'.      EL6891
00064      12  W-CHEK-KEY.                                              EL6891
00065          16  W-CHEK-COMPANY-CD   PIC  X(01).                      EL6891
00066          16  W-CHEK-CGSAETC.                                      EL6891
00067              20  W-CHEK-CARRIER  PIC  X(01).                      EL6891
00068              20  W-CHEK-GROUPING PIC  X(06).                      EL6891
00069              20  W-CHEK-STATE    PIC  X(02).                      EL6891
00070              20  W-CHEK-ACCOUNT  PIC  X(10).                      EL6891
00071              20  W-CHEK-CERT-EFF-DT PIC X(02).                    EL6891
00072              20  W-CHEK-CERT-NO.                                  EL6891
00073                  24  W-CHEK-CERT-PRIME                            EL6891
00074                                  PIC  X(10).                      EL6891
00075                  24  W-CHEK-CERT-SFX                              EL6891
00076                                  PIC  X(01).                      EL6891
00077              20  W-CHEK-SEQUENCE-NO                               EL6891
00078                                  PIC S9(04)  COMP.                EL6891
00079                                                                   EL6891
00080      12  W-CHKQ-FILE-ID          PIC  X(08)  VALUE 'ERCHKQ'.      EL6891
00081      12  W-CHKQ-KEY.                                              EL6891
00082          16  W-CHKQ-COMPANY-CD   PIC  X(01).                      EL6891
00083          16  W-CHKQ-CONTROL-NUMBER                                EL6891
00084                                  PIC S9(08)       COMP.           EL6891
00085          16  W-CHKQ-SEQUENCE-NUMBER                               EL6891
00086                                  PIC S9(04)       COMP.           EL6891
00087                                                                   EL6891
00088      12  W-COMPANY.                                               EL6891
00089          16  W-CO-CHAR OCCURS 30 TIMES                            EL6891
00090                        INDEXED BY W-CO-NDX                        EL6891
00091                                  PIC  X(01).                      EL6891
00092                                                                   EL6891
00093      12  W-CNTL-FILE-ID          PIC  X(08)  VALUE 'ELCNTL'.      EL6891
00094      12  W-CNTL-KEY.                                              EL6891
00095          16  W-CNTL-COMPANY-ID   PIC  X(03).                      EL6891
00096          16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '1'.           EL6891
00097          16  W-CNTL-GENL.                                         EL6891
00098              20  W-CNTL-GEN1     PIC  X(02)  VALUE SPACES.        EL6891
00099              20  W-CNTL-GEN2.                                     EL6891
00100                  24   W-CNTL-GEN3                                 EL6891
00101                                  PIC  X(01)  VALUE SPACES.        EL6891
00102                  24   W-CNTL-GEN4                                 EL6891
00103                                  PIC  X(01)  VALUE SPACES.        EL6891
00104          16  W-CNTL-SEQ          PIC S9(04)  VALUE +0    COMP.    EL6891
00105                                                                   EL6891
00106      12  W-LETTERS.                                               EL6891
00107          16  W-LETTER     OCCURS 3 TIMES                             CL**2
00108                                  PIC  X(04).                      EL6891
00109                                                                   EL6891
00110      12  W-MESSAGES.                                              EL6891
00111          16  W-REPORT-TITLE.                                      EL6891
00112              20  FILLER          PIC  X(01)                       EL6891
00113                  VALUE '1'.                                       EL6891
00114              20  W-DATE          PIC  X(08)                       EL6891
00115                  VALUE 'XX/XX/XX'.                                EL6891
00116              20  FILLER          PIC  X(18)                       EL6891
00117                  VALUE SPACES.                                    EL6891
00118              20  FILLER          PIC  X(26)                       EL6891
00119                  VALUE 'CHECK LETTERS CREATED LIST'.              EL6891
00120              20  FILLER          PIC  X(16)                       EL6891
00121                  VALUE SPACES.                                    EL6891
00122              20  THIS-PGM        PIC  X(09)                          CL**4
00123                  VALUE 'EL6891'.                                  EL6891
00124          16  W-COMPANY-TITLE.                                     EL6891
00125              20  FILLER          PIC  X(26)                       EL6891
00126                  VALUE SPACES.                                    EL6891
00127              20  W-CT-COMPANY.                                    EL6891
00128                  24  W-CT-CHAR OCCURS 30 TIMES                    EL6891
00129                                INDEXED BY W-CT-NDX                EL6891
00130                                  PIC  X(01).                      EL6891
00131              20  FILLER          PIC  X(14)  VALUE SPACES.        EL6891
00132              20  FILLER          PIC  X(05)                       EL6891
00133                  VALUE 'PAGE '.                                   EL6891
00134              20  W-CT-PAGE       PIC  ZZZ9.                       EL6891
00135          16  W-FATAL-ERROR-MSG.                                   EL6891
00136              20  FILLER          PIC  X(12)                       EL6891
00137                  VALUE 'FATAL ERROR '.                            EL6891
00138              20  W-FATAL-ERROR   PIC  9(04).                      EL6891
00139              20  FILLER          PIC  X(18)                       EL6891
00140                                  VALUE ' DETECTED FOR CHK '.      EL6891
00141              20  W-CHECK-NO-F    PIC  X(07) VALUE 'XXXXXXX'.      EL6891
00142              20  FILLER          PIC  X(07) VALUE ', CERT '.      EL6891
00143              20  W-CERT-NO-F     PIC  X(11) VALUE 'XXXXXXXXXXX'.  EL6891
00144              20  FILLER          PIC  X(20)                       EL6891
00145                  VALUE ', LETTER NOT CREATED'.                    EL6891
00146          16  W-CONTROL.                                              CL**2
00147              20  FILLER          PIC  X(17)                          CL**2
00148                  VALUE '0CONTROL NUMBER: '.                          CL**2
00149              20  W-CNTL-NUMBER   PIC  9(09).                         CL**2
00150          16  W-GOOD-LETTERS.                                         CL**2
00151              20  FILLER          PIC  X(35)                          CL**2
00152                  VALUE '* THERE WERE NO ERRORS DETECTED IN '.        CL**2
00153              20  FILLER          PIC  X(45)                          CL**2
00154                  VALUE 'THESE LETTERS.'.                             CL**2
00155          16  W-REVERSED.                                             CL**2
00156              20  FILLER          PIC  X(35)                          CL**2
00157                  VALUE '* ALL FILE UPDATES REVERSED DUE TO '.        CL**2
00158              20  FILLER          PIC  X(45)                          CL**2
00159                  VALUE 'THE DETECTION OF FATAL ERROR(S).'.           CL**2
00160          16  W-CLEANUP.                                              CL**2
00161              20  FILLER          PIC  X(33)                          CL**2
00162                  VALUE '  ALL ARCHIVED RECORDS HAVE BEEN '.          CL**2
00163              20  FILLER          PIC  X(08)                          CL**2
00164                  VALUE 'DELETED '.                                   CL**2
00165              20  FILLER          PIC  X(38)                          CL**2
00166                  VALUE 'DUE TO THE DETECTION OF A FATAL ERROR.'.     CL**2
00167          16  W-LETTER-SENT.                                       EL6891
00168              20  FILLER          PIC  X(26)                       EL6891
00169                  VALUE 'LETTER ARCHIVED FOR CHECK '.              EL6891
00170              20  W-CHECK-X       PIC  X(07) VALUE 'XXXXXXX'.      EL6891
00171              20  FILLER          PIC  X(10) VALUE ' AND CERT '.   EL6891
00172              20  W-CERT-X        PIC  X(11) VALUE 'XXXXXXXXXXX'.  EL6891
00173              20  FILLER          PIC  X(10)                       EL6891
00174                                  VALUE ', ARCHIVE '.              EL6891
00175              20  W-ARCHIVE       PIC  9(08) VALUE 99999999.       EL6891
00176              20  W-ASTERISKS REDEFINES W-ARCHIVE                     CL**2
00177                                  PIC  X(08).                         CL**2
00178          16  W-SEVERE-ERROR.                                      EL6891
00179              20  FILLER          PIC  X(10)                       EL6891
00180                  VALUE ' ******** '.                              EL6891
00181              20  FILLER          PIC  X(26)                       EL6891
00182                  VALUE 'A SEVERE ERROR DETECTED.'.                EL6891
00183              20  FILLER          PIC  X(26)                       EL6891
00184                  VALUE 'ALL PROCESSING STOPPED.'.                 EL6891
00185          16  W-TOTAL1.                                            EL6891
00186              20  FILLER          PIC  X(35)                       EL6891
00187                  VALUE 'CHECK RCRDS READ - '.                     EL6891
00188              20  W-TOTAL-AMT1    PIC  ZZZ,ZZ9.                    EL6891
00189          16  W-TOTAL2.                                            EL6891
00190              20  FILLER          PIC  X(35)                       EL6891
00191                  VALUE 'CHECK RCRDS QUALIFIED - '.                EL6891
00192              20  W-TOTAL-AMT2    PIC  ZZZ,ZZ9.                    EL6891
00193          16  W-TOTAL3.                                            EL6891
00194              20  FILLER          PIC  X(35)                       EL6891
00195                  VALUE 'RCRDS WITH FATAL ERRORS DETECTED - '.     EL6891
00196              20  W-TOTAL-AMT3    PIC  ZZZ,ZZ9.                    EL6891
00197          16  W-TOTAL4.                                            EL6891
00198              20  FILLER          PIC  X(35)                       EL6891
00199                  VALUE 'LETTERS ARCHIVED - '.                     EL6891
00200              20  W-TOTAL-AMT4    PIC  ZZZ,ZZ9.                    EL6891
00201              20  W-ASTERISK4     PIC  X(01).                         CL**2
00202          16  W-TOTAL6.                                            EL6891
00203              20  FILLER          PIC  X(35)                       EL6891
00204                  VALUE 'CHECK RCRDS CHANGED - '.                  EL6891
00205              20  W-TOTAL-AMT6    PIC  ZZZ,ZZ9.                    EL6891
00206              20  W-ASTERISK6     PIC  X(01).                         CL**2
00207          16  W-TOTAL7.                                            EL6891
00208              20  FILLER          PIC  X(35)                       EL6891
00209                  VALUE 'LETTERS WITH ERRORS - '.                  EL6891
00210              20  W-TOTAL-AMT7    PIC  ZZZ,ZZ9.                    EL6891
00211                                                                   EL6891
00212      12  W-MESSAGE-LINE.                                          EL6891
00213          16  W-ML-CC             PIC  X(01).                      EL6891
00214          16  W-ML-DATA           PIC  X(79).                      EL6891
00215                                                                   EL6891
00216      12  W-NEXT-TRAN             PIC  X(04).                      EL6891
00217                                                                   EL6891
00218      12  W-PYAJ-FILE-ID          PIC  X(08)  VALUE 'ERPYAJ'.      EL6891
00219      12  W-PYAJ-KEY.                                              EL6891
00220          16  W-PYAJ-COMPANY-CD   PIC  X(01).                      EL6891
00221          16  W-PYAJ-CARRIER      PIC  X(01).                      EL6891
00222          16  W-PYAJ-GROUPING     PIC  X(06).                      EL6891
00223          16  W-PYAJ-FIN-RESP     PIC  X(10).                      EL6891
00224          16  W-PYAJ-ACCOUNT      PIC  X(10).                      EL6891
00225          16  W-PYAJ-FILE-SEQ-NO  PIC S9(8)     COMP.              EL6891
00226          16  W-PYAJ-RECORD-TYPE  PIC  X(01).                      EL6891
00227              88  W-PYAJ-REMIT-RECEIVED        VALUE 'R'.          EL6891
00228              88  W-PYAJ-DEPOSIT               VALUE 'D'.          EL6891
00229              88  W-PYAJ-CHARGE-TO-AGENT       VALUE 'C'.          EL6891
00230              88  W-PYAJ-ADJ-REM-RECEIVED      VALUE 'S'.          EL6891
00231              88  W-PYAJ-ADJ-DEPOSIT           VALUE 'T'.          EL6891
00232              88  W-PYAJ-ADJ-CHG-TO-AGT        VALUE 'U'.          EL6891
00233              88  W-PYAJ-ADD-TO-YTD-COMP       VALUE 'X'.          EL6891
00234              88  W-PYAJ-SUBTRACT-YTD-COMP     VALUE 'Y'.          EL6891
00235              88  W-PYAJ-ADD-TO-BALANCE        VALUE 'Z'.          EL6891
00236              88  W-PYAJ-FICA-ENTRY            VALUE 'F'.          EL6891
00237              88  W-PYAJ-REMIT-IND-GROUPING    VALUE 'G'.          EL6891
00238              88  W-PYAJ-POLICY-FEE            VALUE 'W'.          EL6891
00239                                                                   EL6891
00240      12  W-TERMINAL-ID.                                           EL6891
00241          18  W-TERM-PREFIX       PIC  X(02).                      EL6891
00242          18  FILLER              PIC  X(02).                      EL6891
00243                                                                   EL6891
00244  01  W-PROGRAM-CONSTANTS.                                         EL6891
00245      12  FILLER                  PIC  X(17)                       EL6891
00246                                  VALUE 'PROGRAM CONSTANTS'.       EL6891
00247      12  W-PGM-EL694             PIC  X(08)  VALUE 'EL694'.       EL6891
00248                                  EJECT                            EL6891
00249                                  COPY ELCINTF.                    EL6891
00250                                  EJECT                            EL6891
00251      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL6891
00252                                  COPY ELC1042.                    EL6891
00253                                  COPY ELC689PI.                   EL6891
00254          16  PI-6891-CHECK-NO    PIC  X(07).                      EL6891
00255          16  FILLER              PIC  X(273).                        CL**3
00256                                  EJECT                            EL6891
00257                                  COPY ELCDATE.                    EL6891
00258                                  EJECT                            EL6891
00259                                  COPY ELCEMIB.                    EL6891
00260                                                                   EL6891
00261  01  EMI-SAVE-AREA               PIC  X(400).                     EL6891
00262                                  EJECT                            EL6891
00263                                  COPY ELPRTCVD.                   EL6891
00264                                  EJECT                            EL6891
00265                                  COPY ELCDMD34.                      CL**4
00266                                                                      CL**4
00267  LINKAGE SECTION.                                                 EL6891
00268  01  DFHCOMMAREA                 PIC  X(1024).                    EL6891
00269 *01 PARM-LIST .                                                      CL**3
00270 *    12  FILLER                  PIC S9(08)  COMP.                   CL**3
00271 *    12  L-CHEK-POINTER          PIC S9(08)  COMP.                   CL**3
00272 *    12  L-CHKQ-POINTER          PIC S9(08)  COMP.                   CL**3
00273 *    12  L-CNTL-POINTER          PIC S9(08)  COMP.                   CL**3
00274 *    12  L-PYAJ-POINTER          PIC S9(08)  COMP.                   CL**3
00275                                                                   EL6891
00276      COPY ERCCHEK.                                                EL6891
00277                                  EJECT                            EL6891
00278      COPY ERCCHKQ.                                                EL6891
00279                                  EJECT                            EL6891
00280      COPY ELCCNTL.                                                EL6891
00281                                  EJECT                            EL6891
00282      COPY ERCPYAJ.                                                EL6891
00283                                  EJECT                            EL6891
00284  PROCEDURE DIVISION.                                              EL6891
00285                                                                   EL6891
00286      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6891
00287      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL6891
00288      MOVE ERROR-MESSAGE-INTERFACE-BLOCK                           EL6891
00289                                  TO EMI-SAVE-AREA.                EL6891
00290                                                                   EL6891
00291      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**4
00292                                                                      CL**4
00293  0100-RETRIEVE-LOOP.                                                 CL**4
00294                                                                      CL**4
00295      EXEC CICS HANDLE CONDITION                                   EL6891
00296           ENDDATA (0200-END-DATA)                                 EL6891
00297           NOTFND  (0300-NOT-FOUND)                                EL6891
00298           ERROR   (1000-ERROR-ABEND)                              EL6891
00299           INVREQ  (1000-ERROR-ABEND)                              EL6891
00300      END-EXEC.                                                    EL6891
00301                                                                   EL6891
00302      EXEC CICS RETRIEVE                                           EL6891
00303           INTO   (PROGRAM-INTERFACE-BLOCK)                        EL6891
00304           LENGTH (PI-COMM-LENGTH)                                 EL6891
00305      END-EXEC.                                                    EL6891
00306                                                                      CL**4
00307                                                                      CL**4
00308 *DLO034  OPEN WHEN DMD OR CID                                        CL**4
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**4
00310          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**4
00311              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**4
00312              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**4
00313              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**4
00314              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**4
00315              MOVE SPACES             TO DL34-PRINT-LINE              CL**4
00316              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**4
00317              EXEC CICS LINK                                          CL**4
00318                  PROGRAM    ('DLO034')                               CL**4
00319                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**4
00320                  LENGTH     (DLO034-REC-LENGTH)                      CL**4
00321              END-EXEC                                                CL**4
00322              IF DL34-RETURN-CODE NOT = 'OK'                          CL**4
00323                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**4
00324                                      TO W-MESSAGE-LINE               CL**4
00325                  PERFORM 0400-SEND-TEXT                              CL**4
00326                  EXEC CICS RETURN                                    CL**4
00327                  END-EXEC.                                           CL**4
00328                                                                   EL6891
00329      EXEC CICS SYNCPOINT                                             CL**2
00330      END-EXEC.                                                       CL**2
00331                                                                      CL**2
00332      IF  PI-CALLING-PROGRAM = W-PGM-EL694                         EL6891
00333          MOVE PI-CR-BATCH-NUMBER TO W-CHECK-CONTROL               EL6891
00334          MOVE W-CONTROL-NUMBER   TO W-CNTL-NUMBER                    CL**2
00335          PERFORM 5000-GET-COMPANY-NAME THRU 5000-EXIT             EL6891
00336          PERFORM 1000-PROCESS-CHKQ-FILE THRU 1000-EXIT            EL6891
00337          PERFORM 3000-CLEAN-UP-ARCHS THRU 3000-EXIT                  CL**2
00338          PERFORM 4000-PRINT-TOTALS THRU 4000-EXIT                 EL6891
00339          MOVE 'X'                TO WS-PROG-END                   EL6891
00340          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL6891
00341          GO TO 0100-RETRIEVE-LOOP.                                EL6891
00342                                                                   EL6891
00343      GO TO 0100-RETRIEVE-LOOP.                                    EL6891
00344                                  EJECT                            EL6891
00345  0200-END-DATA.                                                   EL6891
00346                                                                   EL6891
00347      MOVE 'EXM3'                 TO W-NEXT-TRAN.                  EL6891
00348                                                                   EL6891
00349      MOVE EIBTRMID               TO W-TERMINAL-ID.                EL6891
00350                                                                   EL6891
00351 * DLO034 CLOSE                                                       CL**4
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**4
00353         IF DL34-PROCESS-TYPE NOT EQUAL TO SPACES                     CL**4
00354             MOVE 'C'                TO DL34-PROCESS-TYPE             CL**4
00355             MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID               CL**4
00356             MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID         CL**4
00357             MOVE PI-PROCESSOR-ID    TO DL34-USERID                   CL**4
00358             MOVE SPACES             TO DL34-PRINT-LINE               CL**4
00359                                        DL34-OVERRIDE-PRINTER-ID      CL**4
00360             EXEC CICS LINK                                           CL**4
00361                 PROGRAM    ('DLO034')                                CL**4
00362                 COMMAREA   (DLO034-COMMUNICATION-AREA)               CL**4
00363                 LENGTH     (DLO034-REC-LENGTH)                       CL**4
00364             END-EXEC                                                 CL**4
00365                                                                      CL**4
00366             IF DL34-RETURN-CODE NOT = 'OK'                           CL**4
00367                 MOVE  '**DLO034 CLOSE ERROR - ABORT**'               CL**4
00368                                     TO W-MESSAGE-LINE                CL**4
00369                 PERFORM 0400-SEND-TEXT                               CL**4
00370                 EXEC CICS RETURN                                     CL**4
00371                      TRANSID  (W-NEXT-TRAN)                          CL**4
00372                      COMMAREA (PROGRAM-INTERFACE-BLOCK)              CL**4
00373                      LENGTH   (PI-COMM-LENGTH)                       CL**4
00374                 END-EXEC.                                            CL**4
00375                                                                      CL**4
00376      IF  W-TERM-PREFIX = 'DU'                                     EL6891
00377          EXEC CICS RETURN                                         EL6891
00378               TRANSID  (W-NEXT-TRAN)                              EL6891
00379               COMMAREA (PROGRAM-INTERFACE-BLOCK)                  EL6891
00380               LENGTH   (PI-COMM-LENGTH)                           EL6891
00381          END-EXEC                                                 EL6891
00382      ELSE                                                         EL6891
00383          EXEC CICS RETURN                                         EL6891
00384          END-EXEC.                                                EL6891
00385                                                                   EL6891
00386  0300-NOT-FOUND.                                                  EL6891
00387                                                                   EL6891
00388      IF  PI-COMPANY-ID NOT = 'MON'                                EL6891
00389          MOVE 'NO COMMUNICATION AREA FOUND' TO W-MESSAGE-LINE     EL6891
00390          PERFORM 0400-SEND-TEXT.                                  EL6891
00391                                                                   EL6891
00392      GO TO 0200-END-DATA.                                         EL6891
00393                                                                   EL6891
00394  0400-SEND-TEXT.                                                  EL6891
00395                                                                   EL6891
00396      EXEC CICS SEND TEXT                                          EL6891
00397           FROM   (W-MESSAGE-LINE)                                 EL6891
00398           LENGTH (70)                                             EL6891
00399      END-EXEC.                                                    EL6891
00400                                  EJECT                            EL6891
00401  1000-PROCESS-CHKQ-FILE.                                          EL6891
00402                                                                   EL6891
00403      MOVE +80                    TO WS-LINE-LEN.                  EL6891
00404      MOVE '1'                    TO W-ML-CC.                      EL6891
00405                                                                   EL6891
00406      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6891
00407      MOVE '5'                    TO DC-OPTION-CODE.               EL6891
00408                                                                   EL6891
00409      EXEC CICS LINK                                               EL6891
00410          PROGRAM   ('ELDATCV')                                    EL6891
00411          COMMAREA  (DATE-CONVERSION-DATA)                         EL6891
00412          LENGTH    (DC-COMM-LENGTH)                               EL6891
00413      END-EXEC.                                                    EL6891
00414                                                                   EL6891
00415      MOVE DC-GREG-DATE-1-EDIT    TO W-DATE.                       EL6891
00416      MOVE +56                    TO W-LINE-COUNT.                 EL6891
00417                                                                   EL6891
00418      MOVE LOW-VALUES             TO W-CHKQ-KEY                       CL**2
00419                                     PI-689-WORK-AREA.                CL**2
00420                                                                   EL6891
00421      MOVE PI-COMPANY-CD          TO W-CHKQ-COMPANY-CD.            EL6891
00422                                                                   EL6891
00423      IF  W-CONTROL-NUMBER-PROVIDED                                EL6891
00424          MOVE W-CONTROL-NUMBER   TO W-CHKQ-CONTROL-NUMBER.        EL6891
00425                                                                   EL6891
00426  1000-READ-CHKQ.                                                  EL6891
00427                                                                   EL6891
00428      EXEC CICS HANDLE CONDITION                                   EL6891
00429           NOTOPEN (1000-NOT-OPEN)                                 EL6891
00430           NOTFND  (1000-EXIT)                                     EL6891
00431           ENDFILE (1000-EXIT)                                     EL6891
00432      END-EXEC.                                                    EL6891
00433                                                                   EL6891
00434      EXEC CICS READ                                               EL6891
00435          SET     (ADDRESS OF CHECK-QUE)                              CL**3
00436          DATASET (W-CHKQ-FILE-ID)                                 EL6891
00437          RIDFLD  (W-CHKQ-KEY)                                     EL6891
00438          GTEQ                                                     EL6891
00439      END-EXEC.                                                    EL6891
00440                                                                   EL6891
00441      MOVE EMI-SAVE-AREA          TO ERROR-MESSAGE-INTERFACE-BLOCK.EL6891
00442                                                                   EL6891
00443      IF  CQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD                    EL6891
00444          GO TO 1000-EXIT.                                         EL6891
00445                                                                   EL6891
00446      IF  W-CONTROL-NUMBER-PROVIDED                                EL6891
00447          IF  W-CONTROL-NUMBER NOT EQUAL CQ-CONTROL-NUMBER         EL6891
00448              GO TO 1000-EXIT.                                     EL6891
00449                                                                   EL6891
00450      ADD +1                      TO W-CHKQ-RCRD-READ.             EL6891
00451      MOVE CQ-CONTROL-PRIMARY     TO W-CHKQ-KEY.                   EL6891
00452                                                                   EL6891
00453      IF  CQ-LETTERS-IND NOT EQUAL 'Y'                             EL6891
00454          ADD +1                  TO W-CHKQ-SEQUENCE-NUMBER        EL6891
00455          GO TO 1000-READ-CHKQ.                                    EL6891
00456                                                                   EL6891
00457      IF  NOT CHECK-ON-QUE                                         EL6891
00458              AND                                                  EL6891
00459          NOT MANUAL-CHECK                                         EL6891
00460          ADD +1                  TO W-CHKQ-SEQUENCE-NUMBER        EL6891
00461          GO TO 1000-READ-CHKQ.                                    EL6891
00462                                                                   EL6891
00463      ADD +1                      TO W-CHKQ-RCRD-USED.             EL6891
00464                                                                   EL6891
00465      MOVE CQ-CONTROL-NUMBER      TO PI-689-CONTROL.               EL6891
00466      MOVE 'Y'                    TO PI-689-ARCHIVE-SW.               CL**2
00467      MOVE '3'                    TO PI-689-PRINT-ORDER-SW.           CL**2
00468      MOVE '1'                    TO PI-689-USE-SCREEN-IND.           CL**2
00469      MOVE ZEROS                  TO PI-689-ERROR.                    CL**2
00470                                                                   EL6891
00471      IF  CQ-BILLING-CREDIT                                        EL6891
00472          PERFORM 1300-PROCESS-PYAJ-RECORD THRU 1300-EXIT          EL6891
00473      ELSE                                                         EL6891
00474          PERFORM 1200-GET-CHECK-MASTER THRU 1200-EXIT.            EL6891
00475                                                                   EL6891
00476      IF  NOT PI-689-FATAL-ERROR                                      CL**2
00477          PERFORM 1400-LINK-TO-EL689 THRU 1400-EXIT                   CL**2
00478                  VARYING                                             CL**2
00479              W-LET-NDX FROM 1 BY 1                                   CL**2
00480                  UNTIL                                               CL**2
00481              W-LET-NDX GREATER THAN 3                                CL**2
00482                  OR                                                  CL**2
00483              W-LETTER (W-LET-NDX) NOT GREATER THAN SPACES.           CL**2
00484                                                                   EL6891
00485      IF  PI-689-STOP-ERROR                                        EL6891
00486          MOVE W-SEVERE-ERROR     TO W-ML-DATA                     EL6891
00487          MOVE SPACES             TO W-ML-CC                       EL6891
00488          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                 EL6891
00489          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL6891
00490          GO TO 1000-EXIT                                          EL6891
00491      ELSE                                                         EL6891
00492          IF  PI-689-ERR-DETECTED-PREV                                CL**2
00493              NEXT SENTENCE                                        EL6891
00494          ELSE                                                     EL6891
00495              PERFORM 2000-UPDATE-CHKQ-RCRD THRU 2000-EXIT.        EL6891
00496                                                                   EL6891
00497      ADD +1                      TO W-CHKQ-SEQUENCE-NUMBER.       EL6891
00498      GO TO 1000-READ-CHKQ.                                        EL6891
00499                                                                   EL6891
00500  1000-NOT-OPEN.                                                   EL6891
00501                                                                   EL6891
00502      MOVE +56                    TO W-INCOMING-LINES              EL6891
00503      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00504                                                                   EL6891
00505      MOVE 'ELCHKQ FILE IS NOT OPENED'                             EL6891
00506                                  TO W-ML-DATA.                    EL6891
00507      MOVE '0'                    TO W-ML-CC.                      EL6891
00508      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00509      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00510      GO TO 1000-EXIT.                                             EL6891
00511                                  EJECT                            EL6891
00512  1000-ERROR-ABEND.                                                EL6891
00513                                                                   EL6891
00514      MOVE +2                     TO W-INCOMING-LINES              EL6891
00515      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00516                                                                   EL6891
00517      MOVE 'UNACCEPTABLE ERROR DETECTED DURING PROCESSING.'        EL6891
00518                                  TO W-ML-DATA.                    EL6891
00519      MOVE '0'                    TO W-ML-CC.                      EL6891
00520      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00521      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00522      GO TO 1000-EXIT.                                             EL6891
00523                                                                   EL6891
00524  1000-EXIT.                                                       EL6891
00525      EXIT.                                                        EL6891
00526                                  EJECT                            EL6891
00527  1200-GET-CHECK-MASTER.                                           EL6891
00528                                                                   EL6891
00529      MOVE '5'                    TO PI-689-DATA-SOURCE.           EL6891
00530      MOVE '6'                    TO PI-689-LABEL-SOURCE.          EL6891
00531      MOVE CQ-COMPANY-CD          TO W-CHEK-COMPANY-CD.            EL6891
00532      MOVE CQ-CREDIT-CHEK-CNTL    TO W-CHEK-CGSAETC.               EL6891
00533                                                                   EL6891
00534      EXEC CICS HANDLE CONDITION                                   EL6891
00535           NOTOPEN (1200-CHEK-NOT-OPEN)                            EL6891
00536           NOTFND  (1200-CHEK-NOT-FOUND)                           EL6891
00537           ENDFILE (1200-CHEK-NOT-FOUND)                           EL6891
00538      END-EXEC.                                                    EL6891
00539                                                                   EL6891
00540      EXEC CICS READ                                               EL6891
00541          SET      (ADDRESS OF CHECK-RECORDS)                         CL**3
00542          DATASET  (W-CHEK-FILE-ID)                                EL6891
00543          RIDFLD   (W-CHEK-KEY)                                    EL6891
00544      END-EXEC.                                                    EL6891
00545                                                                   EL6891
00546      MOVE CH-CARRIER             TO PI-689-CARRIER.               EL6891
00547      MOVE CH-GROUPING            TO PI-689-GROUPING.              EL6891
00548      MOVE CH-STATE               TO PI-689-STATE.                 EL6891
00549      MOVE CH-ACCOUNT             TO PI-689-ACCOUNT.               EL6891
00550      MOVE CH-CERT-EFF-DT         TO PI-689-EFF-DATE.              EL6891
00551      MOVE CH-CERT-NO             TO W-CERT-X                      EL6891
00552                                     PI-689-CERT-NO.               EL6891
00553      MOVE CH-SEQUENCE-NO         TO PI-689-SEQ-NO.                EL6891
00554      MOVE CH-CHECK-NO            TO PI-6891-CHECK-NO              EL6891
00555                                     W-CHECK-X.                    EL6891
00556      MOVE CH-LETTER-TABLE        TO W-LETTERS.                    EL6891
00557      GO TO 1200-EXIT.                                             EL6891
00558                                  EJECT                            EL6891
00559  1200-CHEK-NOT-OPEN.                                              EL6891
00560                                                                   EL6891
00561      MOVE +56                    TO W-INCOMING-LINES              EL6891
00562      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00563                                                                   EL6891
00564      MOVE 'ELCHEK FILE IS NOT OPENED'                             EL6891
00565                                  TO W-ML-DATA.                    EL6891
00566      MOVE '0'                    TO W-ML-CC.                      EL6891
00567      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00568      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00569      MOVE '3775'                 TO PI-689-ERROR.                    CL**2
00570      GO TO 1200-EXIT.                                             EL6891
00571                                                                   EL6891
00572  1200-CHEK-NOT-FOUND.                                             EL6891
00573                                                                   EL6891
00574      MOVE +2                     TO W-INCOMING-LINES              EL6891
00575      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00576                                                                   EL6891
00577      MOVE 'UNACCEPTABLE ERROR DETECTED WHILE READING CHEK.'       EL6891
00578                                  TO W-ML-DATA.                    EL6891
00579      MOVE '0'                    TO W-ML-CC.                      EL6891
00580      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00581      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00582      MOVE '2908'                 TO PI-689-ERROR.                    CL**2
00583      GO TO 1200-EXIT.                                             EL6891
00584                                                                   EL6891
00585  1200-EXIT.                                                       EL6891
00586      EXIT.                                                        EL6891
00587                                  EJECT                            EL6891
00588  1300-PROCESS-PYAJ-RECORD.                                        EL6891
00589                                                                   EL6891
00590      MOVE '6'                    TO PI-689-DATA-SOURCE.           EL6891
00591      MOVE '4'                    TO PI-689-LABEL-SOURCE.          EL6891
00592                                                                   EL6891
00593      EXEC CICS HANDLE CONDITION                                   EL6891
00594           NOTOPEN (1300-PYAJ-NOT-OPEN)                            EL6891
00595           NOTFND  (1300-PYAJ-NOT-FOUND)                           EL6891
00596           ENDFILE (1300-PYAJ-NOT-FOUND)                           EL6891
00597      END-EXEC.                                                    EL6891
00598                                                                   EL6891
00599      MOVE LOW-VALUES             TO W-PYAJ-KEY.                   EL6891
00600      MOVE CQ-COMPANY-CD          TO W-PYAJ-COMPANY-CD.            EL6891
00601      MOVE CQ-PYAJ-CARRIER        TO W-PYAJ-CARRIER.               EL6891
00602      MOVE CQ-PYAJ-GROUPING       TO W-PYAJ-GROUPING.              EL6891
00603      MOVE CQ-PYAJ-FIN-RESP       TO W-PYAJ-FIN-RESP.              EL6891
00604      MOVE CQ-PYAJ-ACCOUNT        TO W-PYAJ-ACCOUNT.               EL6891
00605      MOVE CQ-PYAJ-SEQ            TO W-PYAJ-FILE-SEQ-NO.           EL6891
00606      MOVE 'C'                    TO W-PYAJ-RECORD-TYPE.           EL6891
00607                                                                   EL6891
00608      EXEC CICS READ                                               EL6891
00609          DATASET (W-PYAJ-FILE-ID)                                 EL6891
00610          RIDFLD  (W-PYAJ-KEY)                                     EL6891
00611          SET     (ADDRESS OF PENDING-PAY-ADJ)                        CL**3
00612      END-EXEC.                                                    EL6891
00613                                                                   EL6891
00614      MOVE PY-CARRIER             TO PI-689-CARRIER.               EL6891
00615      MOVE PY-GROUPING            TO PI-689-GROUPING.              EL6891
00616      MOVE PY-ACCOUNT             TO PI-689-ACCOUNT.               EL6891
00617      MOVE PY-FILE-SEQ-NO         TO PI-689-SEQ-NO.                EL6891
00618      MOVE PY-FIN-RESP            TO PI-689-RESP-PERSON            EL6891
00619                                     W-CERT-X.                     EL6891
00620      MOVE 'C'                    TO PI-689-TYPE.                  EL6891
00621      MOVE PY-CHECK-NUMBER        TO PI-6891-CHECK-NO              EL6891
00622                                     W-CHECK-X.                    EL6891
00623      MOVE PY-LETTERS             TO W-LETTERS.                    EL6891
00624      GO TO 1300-EXIT.                                             EL6891
00625                                  EJECT                            EL6891
00626  1300-PYAJ-NOT-OPEN.                                              EL6891
00627                                                                   EL6891
00628      MOVE +56                    TO W-INCOMING-LINES.             EL6891
00629      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00630                                                                   EL6891
00631      MOVE 'ERPYAJ FILE IS NOT OPENED'                             EL6891
00632                                  TO W-ML-DATA.                    EL6891
00633      MOVE '0'                    TO W-ML-CC.                      EL6891
00634      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00635      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00636      MOVE '2232'                 TO PI-689-ERROR.                    CL**2
00637      GO TO 1300-EXIT.                                             EL6891
00638                                                                   EL6891
00639  1300-PYAJ-NOT-FOUND.                                             EL6891
00640                                                                   EL6891
00641      MOVE +2                     TO W-INCOMING-LINES.             EL6891
00642      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00643                                                                   EL6891
00644      MOVE 'UNACCEPTABLE ERROR DETECTED WHILE READING PYAJ.'       EL6891
00645                                  TO W-ML-DATA.                    EL6891
00646      MOVE '0'                    TO W-ML-CC.                      EL6891
00647      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00648      MOVE '7395'                 TO PI-689-ERROR.                    CL**2
00649      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00650      GO TO 1300-EXIT.                                             EL6891
00651                                                                   EL6891
00652  1300-EXIT.                                                       EL6891
00653      EXIT.                                                        EL6891
00654                                  EJECT                            EL6891
00655  1400-LINK-TO-EL689.                                              EL6891
00656                                                                   EL6891
00657      MOVE W-LETTER (W-LET-NDX)   TO PI-689-FORM-NUMBER.           EL6891
00658      MOVE LOW-VALUES             TO PI-689-FOLLOW-UP-DATE         EL6891
00659                                     PI-689-RESEND-DATE-1          EL6891
00662      MOVE ZEROS                  TO PI-689-NUMBER-COPIES.         EL6891
00663                                                                   EL6891
00664      EXEC CICS LINK                                               EL6891
00665           PROGRAM   ('EL689')                                     EL6891
00666           COMMAREA  (PROGRAM-INTERFACE-BLOCK)                     EL6891
00667           LENGTH    (PI-COMM-LENGTH)                              EL6891
00668      END-EXEC.                                                    EL6891
00669                                                                   EL6891
00670      IF  PI-689-FATAL-ERROR                                       EL6891
00671          MOVE 'Y'                TO PI-689-ERROR-IND                 CL**2
00672          ADD +1                  TO W-FATAL-ERRORS                EL6891
00673          MOVE PI-689-ERROR       TO W-FATAL-ERROR                 EL6891
00674          MOVE PI-6891-CHECK-NO   TO W-CHECK-NO-F                  EL6891
00675          MOVE PI-689-CERT-NO     TO W-CERT-NO-F                   EL6891
00676          MOVE W-FATAL-ERROR-MSG  TO W-ML-DATA                     EL6891
00677          MOVE '0'                TO W-ML-CC                       EL6891
00678          MOVE +3                 TO W-INCOMING-LINES              EL6891
00679          PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT                  EL6891
00680          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                 EL6891
00681          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL6891
00682          MOVE PI-689-ERROR       TO EMI-ERROR                     EL6891
00683          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6891
00684          MOVE EMI-ERROR-TEXT (1) TO W-ML-DATA                     EL6891
00685          MOVE SPACES             TO W-ML-CC                       EL6891
00686          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                 EL6891
00687          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL6891
00688          GO TO 1400-EXIT.                                            CL**2
00689                                                                   EL6891
00690      ADD +1                      TO W-LETTERS-CREATED.            EL6891
00691                                                                   EL6891
00692      IF  PI-689-ERR-DETECTED-PREV                                    CL**2
00693          MOVE '********'         TO W-ASTERISKS                      CL**2
00694      ELSE                                                            CL**2
00695          MOVE PI-689-ARCHIVE-NUMBER                                  CL**2
00696                                  TO W-ARCHIVE.                       CL**2
00697                                                                      CL**2
00698      MOVE W-LETTER-SENT          TO W-ML-DATA.                    EL6891
00699                                                                   EL6891
00700      MOVE '0'                    TO W-ML-CC.                         CL**2
00701      MOVE +2                     TO W-INCOMING-LINES.                CL**2
00702      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                        CL**2
00703                                                                   EL6891
00704      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00705      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00706                                                                   EL6891
00707  1400-EXIT.                                                       EL6891
00708      EXIT.                                                        EL6891
00709                                  EJECT                            EL6891
00710  2000-UPDATE-CHKQ-RCRD.                                           EL6891
00711                                                                   EL6891
00712      EXEC CICS READ                                               EL6891
00713          SET      (ADDRESS OF CHECK-QUE)                             CL**3
00714          DATASET  (W-CHKQ-FILE-ID)                                EL6891
00715          RIDFLD   (W-CHKQ-KEY)                                    EL6891
00716          UPDATE                                                   EL6891
00717      END-EXEC.                                                    EL6891
00718                                                                   EL6891
00719      MOVE W-PGM-ID               TO CQ-LAST-UPDATED-BY.           EL6891
00720      MOVE SPACE                  TO CQ-LETTERS-IND.               EL6891
00721                                                                   EL6891
00722      EXEC CICS REWRITE                                            EL6891
00723          DATASET  (W-CHKQ-FILE-ID)                                EL6891
00724          FROM     (CHECK-QUE)                                     EL6891
00725      END-EXEC.                                                    EL6891
00726                                                                   EL6891
00727      ADD +1                      TO W-CHKQ-RCRD-CHANGED.          EL6891
00728                                                                   EL6891
00729  2000-EXIT.                                                       EL6891
00730      EXIT.                                                        EL6891
00731                                  EJECT                            EL6891
00732  3000-CLEAN-UP-ARCHS.                                                CL**2
00733                                                                      CL**2
00734      IF  PI-689-ERR-DETECTED-PREV                                    CL**2
00735          MOVE W-GOOD-LETTERS     TO W-ML-DATA                        CL**2
00736          MOVE '0'                TO W-ML-CC                          CL**2
00737          MOVE +3                 TO W-INCOMING-LINES                 CL**2
00738          PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT                     CL**2
00739          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                    CL**2
00740          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**2
00741          MOVE 'NOTE: '           TO W-ML-DATA                        CL**2
00742          MOVE 0                  TO W-ML-CC                          CL**2
00743          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                    CL**2
00744          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**2
00745          MOVE W-CLEANUP          TO W-ML-DATA                        CL**2
00746          MOVE SPACES             TO W-ML-CC                          CL**2
00747          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                    CL**2
00748          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**2
00749                                                                      CL**2
00750          EXEC CICS SYNCPOINT                                         CL**2
00751              ROLLBACK                                                CL**2
00752          END-EXEC.                                                   CL**2
00753                                                                      CL**2
00754  3000-EXIT.                                                          CL**2
00755      EXIT.                                                           CL**2
00756                                  EJECT                               CL**2
00757  4000-PRINT-TOTALS.                                               EL6891
00758                                                                      CL**2
00759      IF  PI-689-ERR-DETECTED-PREV                                    CL**2
00760          MOVE '*'                TO W-ASTERISK4                      CL**2
00761                                     W-ASTERISK6.                     CL**2
00762                                                                   EL6891
00763      MOVE +56                    TO W-INCOMING-LINES.             EL6891
00764      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00765                                                                   EL6891
00766      MOVE W-CHKQ-RCRD-READ       TO W-TOTAL-AMT1.                 EL6891
00767      MOVE 0                      TO W-ML-CC.                         CL**2
00768      MOVE W-TOTAL1               TO W-ML-DATA.                    EL6891
00769      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00770      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00771                                                                   EL6891
00772      MOVE W-CHKQ-RCRD-USED       TO W-TOTAL-AMT2.                 EL6891
00773      MOVE '0'                    TO W-ML-CC.                      EL6891
00774      MOVE W-TOTAL2               TO W-ML-DATA.                    EL6891
00775      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00776      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00777                                                                   EL6891
00778      MOVE W-CHKQ-RCRD-CHANGED    TO W-TOTAL-AMT6.                 EL6891
00779      MOVE '0'                    TO W-ML-CC.                      EL6891
00780      MOVE W-TOTAL6               TO W-ML-DATA.                    EL6891
00781      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00782      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00783                                                                   EL6891
00784      MOVE W-FATAL-ERRORS         TO W-TOTAL-AMT3.                 EL6891
00785      MOVE '0'                    TO W-ML-CC.                      EL6891
00786      MOVE W-TOTAL3               TO W-ML-DATA.                    EL6891
00787      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00788      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00789                                                                   EL6891
00790      MOVE W-LETTERS-CREATED      TO W-TOTAL-AMT4.                 EL6891
00791      MOVE '0'                    TO W-ML-CC.                      EL6891
00792      MOVE W-TOTAL4               TO W-ML-DATA.                    EL6891
00793      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00794      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00795                                                                   EL6891
00796      MOVE W-LETTERS-WITH-ERRORS  TO W-TOTAL-AMT7.                 EL6891
00797      MOVE '0'                    TO W-ML-CC.                      EL6891
00798      MOVE W-TOTAL7               TO W-ML-DATA.                    EL6891
00799      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00800      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00801                                                                   EL6891
00802      IF  PI-689-ERR-DETECTED-PREV                                    CL**2
00803          MOVE W-REVERSED         TO W-ML-DATA                        CL**2
00804          MOVE '0'                TO W-ML-CC                          CL**2
00805          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                    CL**2
00806          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                        CL**2
00807                                                                      CL**2
00808      MOVE '1'                    TO W-ML-CC.                      EL6891
00809      MOVE SPACES                 TO W-ML-DATA.                    EL6891
00810      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00811      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00812                                                                   EL6891
00813  4000-EXIT.                                                       EL6891
00814      EXIT.                                                        EL6891
00815                                  EJECT                            EL6891
00816  5000-GET-COMPANY-NAME.                                           EL6891
00817                                                                   EL6891
00818      EXEC CICS HANDLE CONDITION                                   EL6891
00819           NOTOPEN (5000-NOTOPEN)                                     CL**2
00820           NOTFND  (5000-NOTFND)                                      CL**2
00821      END-EXEC.                                                    EL6891
00822                                                                   EL6891
00823      MOVE SPACES                 TO W-CNTL-KEY.                   EL6891
00824      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.            EL6891
00825      MOVE '1'                    TO W-CNTL-RECORD-TYPE.           EL6891
00826      MOVE ZEROS                  TO W-CNTL-SEQ.                   EL6891
00827                                                                   EL6891
00828      EXEC CICS READ                                               EL6891
00829           DATASET (W-CNTL-FILE-ID)                                EL6891
00830           SET     (ADDRESS OF CONTROL-FILE)                          CL**3
00831           RIDFLD  (W-CNTL-KEY)                                    EL6891
00832      END-EXEC.                                                    EL6891
00833                                                                   EL6891
00834      MOVE CF-CL-MAIL-TO-NAME     TO W-COMPANY.                    EL6891
00835      MOVE SPACES                 TO W-CT-COMPANY.                 EL6891
00836      PERFORM 5100-CENTER-NAME THRU 5100-EXIT.                     EL6891
00837      GO TO 5000-EXIT.                                                CL**2
00838                                                                      CL**2
00839  5000-NOTOPEN.                                                       CL**2
00840                                                                      CL**2
00841      MOVE '0042'                 TO EMI-ERROR                        CL**2
00842                                     PI-689-ERROR.                    CL**2
00843      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**2
00844      MOVE EMI-ERROR-TEXT (1)     TO W-ML-DATA.                       CL**2
00845      MOVE SPACES                 TO W-ML-CC.                         CL**2
00846      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                   CL**2
00847      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**2
00848      GO TO 5000-EXIT.                                                CL**2
00849                                                                      CL**2
00850  5000-NOTFND.                                                        CL**2
00851                                                                      CL**2
00852      MOVE '9299'                 TO EMI-ERROR                        CL**2
00853                                     PI-689-ERROR.                    CL**2
00854      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**2
00855      MOVE EMI-ERROR-TEXT (1)     TO W-ML-DATA.                       CL**2
00856      MOVE SPACES                 TO W-ML-CC.                         CL**2
00857      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                   CL**2
00858      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**2
00859                                                                   EL6891
00860  5000-EXIT.                                                       EL6891
00861      EXIT.                                                        EL6891
00862                                  EJECT                            EL6891
00863  5100-CENTER-NAME.                                                EL6891
00864                                                                   EL6891
00865      MOVE ZEROS                  TO W-COMBINED-SPACES.            EL6891
00866                                                                   EL6891
00867      PERFORM 5120-FIND-LAST-CHAR THRU 5120-EXIT                   EL6891
00868              VARYING                                              EL6891
00869          W-CO-NDX FROM 30 BY -1                                   EL6891
00870              UNTIL                                                EL6891
00871          W-CO-NDX LESS THAN +1                                    EL6891
00872              OR                                                   EL6891
00873          W-CO-CHAR (W-CO-NDX) NOT EQUAL SPACES.                   EL6891
00874                                                                   EL6891
00875      COMPUTE W-COMBINED-SPACES = W-COMBINED-SPACES / 2.           EL6891
00876                                                                   EL6891
00877      SET W-CT-NDX                TO W-COMBINED-SPACES.            EL6891
00878                                                                   EL6891
00879      PERFORM 5140-MOVE-NAME-CHAR THRU 5140-EXIT                   EL6891
00880              VARYING                                              EL6891
00881          W-CO-NDX FROM +1 BY +1                                   EL6891
00882              UNTIL                                                EL6891
00883          W-CO-NDX GREATER THAN +30                                EL6891
00884              OR                                                   EL6891
00885          W-CT-NDX EQUAL +30.                                      EL6891
00886                                                                   EL6891
00887  5100-EXIT.                                                       EL6891
00888      EXIT.                                                        EL6891
00889                                  EJECT                            EL6891
00890  5120-FIND-LAST-CHAR.                                             EL6891
00891                                                                   EL6891
00892      IF  W-CO-CHAR (W-CO-NDX) EQUAL SPACE                         EL6891
00893          ADD +1                  TO W-COMBINED-SPACES.            EL6891
00894                                                                   EL6891
00895  5120-EXIT.                                                       EL6891
00896      EXIT.                                                        EL6891
00897                                                                   EL6891
00898  5140-MOVE-NAME-CHAR.                                                CL**4
00899                                                                   EL6891
00900      SET W-CT-NDX UP BY +1.                                       EL6891
00901      MOVE W-CO-CHAR (W-CO-NDX)   TO W-CT-CHAR (W-CT-NDX).         EL6891
00902                                                                   EL6891
00903  5140-EXIT.                                                       EL6891
00904      EXIT.                                                        EL6891
00905                                  EJECT                            EL6891
00906  8000-CHKQ-NOT-OPEN.                                              EL6891
00907                                                                   EL6891
00908      MOVE +56                    TO W-INCOMING-LINES              EL6891
00909      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00910                                                                   EL6891
00911      MOVE 'ELCHKQ FILE IS NOT OPENED'                             EL6891
00912                                  TO W-ML-DATA.                    EL6891
00913      MOVE '0'                    TO W-ML-CC.                      EL6891
00914      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00915      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00916      GO TO 1000-EXIT.                                             EL6891
00917                                  EJECT                            EL6891
00918  8010-CHKQ-NOT-FOUND.                                             EL6891
00919                                                                   EL6891
00920      MOVE +2                     TO W-INCOMING-LINES              EL6891
00921      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL6891
00922                                                                   EL6891
00923      MOVE 'UNACCEPTABLE ERROR DETECTED WHILE READING CHKQ.'       EL6891
00924                                  TO W-ML-DATA.                    EL6891
00925      MOVE '0'                    TO W-ML-CC.                      EL6891
00926      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL6891
00927      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6891
00928      GO TO 1000-EXIT.                                             EL6891
00929                                  EJECT                            EL6891
00930  8100-TOP-OF-PAGE.                                                EL6891
00931                                                                   EL6891
00932      COMPUTE W-LINE-COUNT = W-LINE-COUNT + W-INCOMING-LINES.      EL6891
00933                                                                   EL6891
00934      IF  W-LINE-COUNT GREATER THAN +56                            EL6891
00935          COMPUTE W-LINE-COUNT = W-INCOMING-LINES + 2              EL6891
00936          ADD +1                  TO W-PAGE                        EL6891
00937          MOVE +6                 TO W-LINE-COUNT                     CL**2
00938          MOVE W-PAGE             TO W-CT-PAGE                     EL6891
00939          MOVE W-REPORT-TITLE     TO WS-PRINT-AREA                 EL6891
00940          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL6891
00941          MOVE W-COMPANY-TITLE    TO WS-PRINT-AREA                 EL6891
00942          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**2
00943          MOVE SPACES             TO WS-PRINT-AREA                    CL**2
00944          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**2
00945          MOVE W-CONTROL          TO WS-PRINT-AREA                    CL**2
00946          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL6891
00947          MOVE SPACES             TO WS-PRINT-AREA                 EL6891
00948          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                     EL6891
00949                                                                   EL6891
00950  8100-EXIT.                                                       EL6891
00951      EXIT.                                                        EL6891
00952                                  EJECT                            EL6891
00953  9900-ERROR-FORMAT.                                               EL6891
00954                                                                   EL6891
00955      EXEC CICS LINK                                               EL6891
00956          PROGRAM  ('EL001')                                       EL6891
00957          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL6891
00958          LENGTH   (EMI-COMM-LENGTH)                               EL6891
00959      END-EXEC.                                                    EL6891
00960                                                                   EL6891
00961  9900-EXIT.                                                       EL6891
00962      EXIT.                                                        EL6891
00963                                  COPY ELPRTCVP.                   EL6891
00964                                                                      CL**4
00965                                                                      CL**4
