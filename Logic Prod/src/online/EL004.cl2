00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL004
00003  PROGRAM-ID.                 EL004.                                  LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 09:23:08.                    CL**4
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL004
00008 *                                                                 EL004
00009 *AUTHOR.           LOGIC,INC.                                        CL**4
00010 *                  DALLAS,TEXAS.                                     CL**4
00011                                                                   EL004
00012 *DATE-COMPILED.                                                      CL**4
00013 *SECURITY.   *****************************************************   CL**4
00014 *            *                                                   *   CL**4
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00016 *            *                                                   *   CL**4
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00020 *            *                                                   *   CL**4
00021 *            *****************************************************   CL**4
00022                                                                   EL004
00023 *REMARKS.  COMMON ABEND ROUTINE.                                     CL**3
00024 *        THIS PROGRAM IS LINK'D TO WHENEVER AN ERROR OCCURS THAT     CL**3
00025 *        IS NOT CONTROLLED THROUGH A HANDLE CONDITION.               CL**4
00026                                                                   EL004
00027                                                                   EL004
00028      EJECT                                                        EL004
00029  ENVIRONMENT DIVISION.                                            EL004
00030                                                                   EL004
00031  DATA DIVISION.                                                   EL004
00032                                                                   EL004
00033  WORKING-STORAGE SECTION.                                         EL004
00034                                                                   EL004
00035  77  FILLER  PIC X(32)  VALUE '********************************'. EL004
00036  77  FILLER  PIC X(32)  VALUE '*    EL004 WORKING STORAGE     *'. EL004
00037  77  FILLER  PIC X(32)  VALUE '********* V/M 2.004 ************'.    CL**4
00038                                                                   EL004
00039  77  WS-DUMP-SW    COMP-3        PIC S9              VALUE ZERO.  EL004
00040  77  WS-INDEX      COMP-3        PIC S9(3)           VALUE ZERO.  EL004
00041  77  WS-INDEX2     COMP-3        PIC S9(3)           VALUE ZERO.  EL004
00042                                                                   EL004
00043  01  TERMINAL-MESSAGE.                                            EL004
00044      05  TM-SBA                  PIC X.                           EL004
00045      05  TM-FADDR                PIC XX.                          EL004
00046      05  TM-SF                   PIC X.                           EL004
00047      05  TM-ATTRB                PIC X.                           EL004
00048      05  TM-MESG                 PIC X(72)   VALUE SPACES.        EL004
00049      05  FILLER REDEFINES TM-MESG.                                EL004
00050          10  TMR1-A              PIC X(19).                       EL004
00051          10  TMR1-B              PIC X(53).                       EL004
00052      05  FILLER REDEFINES TM-MESG.                                EL004
00053          10  TMR2-A              PIC X(9).                        EL004
00054          10  TMR2-B              PIC X(10).                       EL004
00055          10  TMR2-C              PIC X(53).                       EL004
00056      05  FILLER REDEFINES TM-MESG.                                EL004
00057          10  FILLER              PIC X(68).                       EL004
00058          10  TMR3-A              PIC X(4).                        EL004
00059                                                                   EL004
00060      05  FILLER                  PIC X               VALUE SPACES.EL004
00061                                                                   EL004
00062      05  TM-EIBFN                PIC X(4)            VALUE SPACES.EL004
00063      05  TM-EIBFN-CHAR           REDEFINES                        EL004
00064          TM-EIBFN                PIC X           OCCURS 4 TIMES.  EL004
00065                                                                   EL004
00066      05  FILLER                  PIC X               VALUE SPACES.EL004
00067                                                                   EL004
00068      05  TM-EIBRCODE             PIC X(12)           VALUE SPACES.EL004
00069      05  TM-EIBRCODE-CHAR        REDEFINES                        EL004
00070          TM-EIBRCODE             PIC X           OCCURS 12 TIMES. EL004
00071                                                                   EL004
00072                                                                   EL004
00073  01  EIB-BREAK-DOWN.                                              EL004
00074      05  EBD-FN.                                                  EL004
00075          10  EBD-FN1             PIC X.                           EL004
00076          10  EBD-FN2             PIC X.                           EL004
00077      05  EIBFN-CHAR              REDEFINES                        EL004
00078          EBD-FN                  PIC X    OCCURS 2 TIMES.         EL004
00079                                                                   EL004
00080      05  EBD-RCODE.                                               EL004
00081          10  EBD-RC1             PIC X.                           EL004
00082          10  EBD-RC2A.                                            EL004
00083              15  EBD-RC2         PIC X.                           EL004
00084              15  EBD-RC3         PIC X.                           EL004
00085              15  EBD-RC4         PIC X.                           EL004
00086              15  EBD-RC5         PIC X.                           EL004
00087              15  EBD-RC6         PIC X.                           EL004
00088 *                                                                 EL004
00089      05  EIBRCODE-CHAR           REDEFINES                        EL004
00090          EBD-RCODE               PIC X    OCCURS 6 TIMES.         EL004
00091                                                                   EL004
00092      05  WS-NUMBER                   PIC S9(4)       VALUE ZERO   EL004
00093                                      COMP.                        EL004
00094                                                                   EL004
00095      05  FILLER                      REDEFINES                    EL004
00096          WS-NUMBER.                                               EL004
00097          10  FILLER                  PIC X.                       EL004
00098          10  WS-CHAR                 PIC X.                       EL004
00099                                                                   EL004
00100      05  WS-REMAINDER                PIC S9(3)       VALUE ZERO   EL004
00101                                      COMP-3.                      EL004
00102                                                                   EL004
00103      05  WS-HEX-VALUES               PIC X(15)       VALUE        EL004
00104          '123456789ABCDEF'.                                       EL004
00105                                                                   EL004
00106      05  WS-HEX-CHAR                 REDEFINES                    EL004
00107          WS-HEX-VALUES               PIC X                        EL004
00108          OCCURS 15 TIMES.                                         EL004
00109                                                                   EL004
00110      05  WS-CHARACTERS               PIC X(10)       VALUE SPACES.EL004
00111      05  WS-CHARACTER                REDEFINES                    EL004
00112          WS-CHARACTERS               PIC X                        EL004
00113          OCCURS 10 TIMES.                                         EL004
00114                                                                   EL004
00115      05  WS-ZONE                     PIC X                        EL004
00116          OCCURS 10 TIMES.                                         EL004
00117                                                                   EL004
00118      05  WS-DIGIT                    PIC X                        EL004
00119          OCCURS 10 TIMES.                                         EL004
00120                                                                   EL004
00121  01  HEX-BREAK-DOWN.                                              EL004
00122      05  HEX-BD                  PIC 9(4)    COMP VALUE ZEROS.    EL004
00123      05  HEX-1  REDEFINES HEX-BD.                                 EL004
00124          10  FILLER              PIC X.                           EL004
00125          10  HEX-NBR             PIC X.                           EL004
00126                                                                   EL004
00127  01  HEX-NX                      PIC XX      VALUE '*0'.          EL004
00128 *    HEX-NX IS FOR LINE 24                                        EL004
00129                                                                   EL004
00130  01  HEX-L21                     PIC XX      VALUE 'R '.          EL004
00131 *    HEX-L21 IS FOR LINE 21                                       EL004
00132                                                                   EL004
00133  01  SAVE-DS                     PIC X(8).                        EL004
00134                                                                   EL004
00135  01  WORK-EIB                    PIC X(80).                       EL004
00136                                                                   EL004
00137  01  FILLER  REDEFINES  WORK-EIB.                                 EL004
00138      05  FILLER                  PIC S9(7)   COMP-3.              EL004
00139      05  FILLER                  PIC S9(7)   COMP-3.              EL004
00140      05  FILLER                  PIC X(4).                        EL004
00141      05  FILLER                  PIC S9(7)   COMP-3.              EL004
00142      05  FILLER                  PIC X(4).                        EL004
00143      05  FILLER                  PIC S9(4)   COMP.                EL004
00144      05  FILLER                  PIC S9(4)   COMP.                EL004
00145      05  FILLER                  PIC S9(4)   COMP.                EL004
00146      05  FILLER                  PIC XX.                          EL004
00147      05  W-EIBFN                 PIC XX.                          EL004
PEMUNI     05  FILLER                  PIC XX.
00148      05  W-EIBRCODE              PIC X(6).                        EL004
PEMUNI     05  FILLER                  PIC XX.
00149      05  W-EIBDS                 PIC X(8).                        EL004
00150      05  W-EIBREQID              PIC X(8).                           CL**4
00151      05  W-EIBRSRCE              PIC X(8).                           CL**4
00152                                                                   EL004
00153  LINKAGE SECTION.                                                 EL004
00154                                                                   EL004
00155  01  DFHCOMMAREA                 PIC X(72).                       EL004
00156                                                                   EL004
00157      EJECT                                                        EL004
00158  PROCEDURE DIVISION.                                              EL004
00159                                                                   EL004
00160  000-MAIN-LOGIC SECTION.                                          EL004
00161                                                                   EL004
00162  000-MLS-10-NOTES.                                                EL004
00163 *                                                                 EL004
00164 *    THIS SECTION CONTROLS ALL OTHER SECTIONS                     EL004
00165 *                                                                 EL004
00166 *    CHECK EIBFN CODES TO PERFORM CORRECT                         EL004
00167 *    OPERATION                                                    EL004
00168 *                                                                 EL004
00169      MOVE DFHCOMMAREA            TO WORK-EIB.                     EL004
00170      MOVE W-EIBFN                TO EBD-FN                        EL004
00171                                  WS-CHARACTERS.                   EL004
00172                                                                   EL004
00173      PERFORM 1000-CONVERT-HEX-CHARACTERS                          EL004
00174          VARYING WS-INDEX FROM +1 BY +1                           EL004
00175              UNTIL WS-INDEX GREATER THAN +2.                      EL004
00176                                                                   EL004
00177      MOVE WS-ZONE (1)            TO  TM-EIBFN-CHAR (1).           EL004
00178      MOVE WS-DIGIT (1)           TO  TM-EIBFN-CHAR (2).           EL004
00179      MOVE WS-ZONE (2)            TO  TM-EIBFN-CHAR (3).           EL004
00180      MOVE WS-DIGIT (2)           TO  TM-EIBFN-CHAR (4).           EL004
00181                                                                   EL004
00182      MOVE W-EIBRCODE             TO EBD-RCODE                     EL004
00183                                  WS-CHARACTERS.                   EL004
00184                                                                   EL004
00185      PERFORM 1000-CONVERT-HEX-CHARACTERS                          EL004
00186          VARYING WS-INDEX FROM +1 BY +1                           EL004
00187              UNTIL WS-INDEX GREATER THAN +6.                      EL004
00188                                                                   EL004
00189      MOVE WS-ZONE  (1)           TO  TM-EIBRCODE-CHAR (1).        EL004
00190      MOVE WS-DIGIT (1)           TO  TM-EIBRCODE-CHAR (2).        EL004
00191      MOVE WS-ZONE  (2)           TO  TM-EIBRCODE-CHAR (3).        EL004
00192      MOVE WS-DIGIT (2)           TO  TM-EIBRCODE-CHAR (4).        EL004
00193      MOVE WS-ZONE  (3)           TO  TM-EIBRCODE-CHAR (5).        EL004
00194      MOVE WS-DIGIT (3)           TO  TM-EIBRCODE-CHAR (6).        EL004
00195      MOVE WS-ZONE  (4)           TO  TM-EIBRCODE-CHAR (7).        EL004
00196      MOVE WS-DIGIT (4)           TO  TM-EIBRCODE-CHAR (8).        EL004
00197      MOVE WS-ZONE  (5)           TO  TM-EIBRCODE-CHAR (9).        EL004
00198      MOVE WS-DIGIT (5)           TO  TM-EIBRCODE-CHAR (10).       EL004
00199      MOVE WS-ZONE  (6)           TO  TM-EIBRCODE-CHAR (11).       EL004
00200      MOVE WS-DIGIT (6)           TO  TM-EIBRCODE-CHAR (12).       EL004
00201                                                                   EL004
00202      MOVE W-EIBDS                TO SAVE-DS.                      EL004
00203      MOVE DFHEIBLK               TO WORK-EIB.                     EL004
00204                                                                   EL004
00205      MOVE 02 TO HEX-BD.                                           EL004
00206      IF HEX-NBR = EBD-FN1                                         EL004
00207          PERFORM 100-CICS-ERROR                                   EL004
00208          GO TO 000-MLS-100.                                       EL004
00209                                                                   EL004
00210      MOVE 04 TO HEX-BD.                                           EL004
00211      IF HEX-NBR = EBD-FN1                                         EL004
00212          PERFORM 125-ISSUE                                        EL004
00213          GO TO 000-MLS-100.                                       EL004
00214                                                                   EL004
00215      MOVE 06 TO HEX-BD.                                           EL004
00216      IF HEX-NBR = EBD-FN1                                         EL004
00217          PERFORM 150-FILE                                         EL004
00218          GO TO 000-MLS-100.                                       EL004
00219                                                                   EL004
00220      MOVE 08 TO HEX-BD.                                           EL004
00221      IF HEX-NBR = EBD-FN1                                         EL004
00222          PERFORM 175-TD                                           EL004
00223          GO TO 000-MLS-100.                                       EL004
00224                                                                   EL004
00225      MOVE 10 TO HEX-BD.                                           EL004
00226      IF HEX-NBR = EBD-FN1                                         EL004
00227          PERFORM 200-TS                                           EL004
00228          GO TO 000-MLS-100.                                       EL004
00229                                                                   EL004
00230      MOVE 12 TO HEX-BD.                                           EL004
00231      IF HEX-NBR = EBD-FN1                                         EL004
00232          PERFORM 225-CORE                                         EL004
00233          GO TO 000-MLS-100.                                       EL004
00234                                                                   EL004
00235      MOVE 14 TO HEX-BD.                                           EL004
00236      IF HEX-NBR = EBD-FN1                                         EL004
00237          PERFORM 250-ABEND                                        EL004
00238          GO TO 000-MLS-100.                                       EL004
00239                                                                   EL004
00240      MOVE 16 TO HEX-BD.                                           EL004
00241      IF HEX-NBR = EBD-FN1                                         EL004
00242          PERFORM 275-POST                                         EL004
00243          GO TO 000-MLS-100.                                       EL004
00244                                                                   EL004
00245      MOVE 18 TO HEX-BD.                                           EL004
00246      IF HEX-NBR = EBD-FN1                                         EL004
00247          PERFORM 300-ENQ                                          EL004
00248          GO TO 000-MLS-100.                                       EL004
00249                                                                   EL004
00250      MOVE 20 TO HEX-BD.                                           EL004
00251      IF HEX-NBR = EBD-FN1                                         EL004
00252          PERFORM 325-JOURNAL                                      EL004
00253          GO TO 000-MLS-100.                                       EL004
00254                                                                   EL004
00255      MOVE 24 TO HEX-BD.                                           EL004
00256      IF HEX-NBR = EBD-FN1                                         EL004
00257          PERFORM 350-MAP                                          EL004
00258          GO TO 000-MLS-100.                                       EL004
00259                                                                   EL004
00260      MOVE 30 TO HEX-BD.                                           EL004
00261      IF HEX-NBR = EBD-FN1                                         EL004
00262          PERFORM 375-ISS-QUERY                                    EL004
00263          GO TO 000-MLS-100.                                       EL004
00264                                                                   EL004
00265      MOVE 'UNDETERMINED ERROR' TO TM-MESG.                        EL004
00266                                                                   EL004
00267  000-MLS-100.                                                     EL004
00268      MOVE 17                     TO HEX-BD.                       EL004
00269      MOVE HEX-NBR                TO TM-SBA.                       EL004
00270 *    MOVE HEX-NX                 TO HEX-1.                        EL004
00271      MOVE HEX-L21                TO HEX-1.                        EL004
00272      MOVE HEX-1                  TO TM-FADDR.                     EL004
00273      MOVE 29                     TO HEX-BD.                       EL004
00274      MOVE HEX-NBR                TO TM-SF.                        EL004
00275      MOVE '8'                    TO TM-ATTRB.                     EL004
00276                                                                   EL004
00277      MOVE EIBTRMID               TO TMR3-A.                       EL004
00278                                                                   EL004
00279      EXEC CICS WRITEQ TD                                          EL004
00280           QUEUE   ('CSMT')                                        EL004
00281           FROM    (TM-MESG)                                       EL004
00282           LENGTH  (90)                                            EL004
00283           END-EXEC.                                               EL004
00284                                                                   EL004
00285      EXEC CICS SYNCPOINT                                             CL**3
00286           ROLLBACK                                                   CL**3
00287      END-EXEC.                                                       CL**3
00288                                                                      CL**3
00289      MOVE W-EIBFN                TO EBD-FN.                       EL004
00290      MOVE 14                     TO HEX-BD.                       EL004
00291      IF HEX-NBR = EBD-FN1                                         EL004
00292         PERFORM 050-CK-SEND.                                      EL004
00293                                                                   EL004
00294      EXEC CICS SEND                                               EL004
00295          FROM   (TERMINAL-MESSAGE)                                EL004
00296          LENGTH (77) END-EXEC.                                    EL004
00297                                                                   EL004
pemuni*    IF WS-DUMP-SW NOT = ZERO                                     EL004
pemuni*        EXEC CICS DUMP                                           EL004
pemuni*            DUMPCODE  ('LGXX') TASK END-EXEC.                    EL004
00301                                                                   EL004
00302      EXEC CICS RETURN END-EXEC.                                   EL004
00303      GOBACK.                                                      EL004
00304                                                                   EL004
00305  000-MLS-900-EXIT.                                                EL004
00306      EXIT.                                                        EL004
00307                                                                   EL004
00308      EJECT                                                        EL004
00309  050-CK-SEND SECTION.                                             EL004
00310                                                                   EL004
00311  050-CKS-10-NOTES.                                                EL004
00312      MOVE 02                     TO HEX-BD.                       EL004
00313                                                                   EL004
pemuni*    IF WS-DUMP-SW NOT = ZERO                                     EL004
pemuni*        MOVE ZERO               TO WS-DUMP-SW                    EL004
pemuni*        EXEC CICS DUMP DUMPCODE('LGXX') TASK END-EXEC.           EL004
00317                                                                   EL004
00318      IF HEX-NBR = EBD-FN2                                         EL004
00319         MOVE TM-MESG             TO DFHCOMMAREA                   EL004
00320         EXEC CICS RETURN END-EXEC.                                EL004
00321                                                                   EL004
00322  050-CKS-900-EXIT.                                                EL004
00323      EXIT.                                                        EL004
00324                                                                   EL004
00325      EJECT                                                        EL004
00326  100-CICS-ERROR SECTION.                                          EL004
00327                                                                   EL004
00328  100-CES-10-NOTES.                                                EL004
00329 *                                                                 EL004
00330 *    THIS SECTION HANDLES THE ADDRESS OR ASSIGN ERROR             EL004
00331 *                                                                 EL004
00332      MOVE 'INVREQ FOR ADDRESS ASSIGN OR HANDLE CONDITION'         EL004
00333          TO TM-MESG.                                              EL004
00334                                                                   EL004
00335  100-CES-900-EXIT.                                                EL004
00336      EXIT.                                                        EL004
00337                                                                   EL004
00338      EJECT                                                        EL004
00339  125-ISSUE SECTION.                                               EL004
00340                                                                   EL004
00341  125-ISS-10-NOTES.                                                EL004
00342                                                                   EL004
00343 *                                                                 EL004
00344 *    THIS SECTION HANDLES CLASS 04 ERRORS                         EL004
00345 *                                                                 EL004
00346      MOVE 02                     TO HEX-BD.                       EL004
00347      IF HEX-NBR = EBD-FN2                                         EL004
00348          MOVE 'RECEIVE'          TO TMR1-A.                       EL004
00349                                                                   EL004
00350      MOVE 04                     TO HEX-BD.                       EL004
00351      IF HEX-NBR = EBD-FN2                                         EL004
00352          MOVE 'SEND'             TO TMR1-A.                       EL004
00353                                                                   EL004
00354      MOVE 06                     TO HEX-BD.                       EL004
00355      IF HEX-NBR = EBD-FN2                                         EL004
00356          MOVE 'CONVERSE'         TO TMR1-A.                       EL004
00357                                                                   EL004
00358      MOVE 08                     TO HEX-BD.                       EL004
00359      IF HEX-NBR = EBD-FN2                                         EL004
00360          MOVE 'ISSUE EODS'       TO TMR1-A.                       EL004
00361                                                                   EL004
00362      MOVE 10                     TO HEX-BD.                       EL004
00363      IF HEX-NBR = EBD-FN2                                         EL004
00364          MOVE 'ISSUE COPY'       TO TMR1-A.                       EL004
00365                                                                   EL004
00366      MOVE 12                     TO HEX-BD.                       EL004
00367      IF HEX-NBR = EBD-FN2                                         EL004
00368          MOVE 'WAIT TERMINAL'    TO TMR1-A.                       EL004
00369                                                                   EL004
00370      MOVE 14                     TO HEX-BD.                       EL004
00371      IF HEX-NBR = EBD-FN2                                         EL004
00372          MOVE 'ISSUE LOAD'       TO TMR1-A.                       EL004
00373                                                                   EL004
00374      MOVE 16                     TO HEX-BD.                       EL004
00375      IF HEX-NBR = EBD-FN2                                         EL004
00376          MOVE 'WAIT SIGNAL'      TO TMR1-A.                       EL004
00377                                                                   EL004
00378      MOVE 18                     TO HEX-BD.                       EL004
00379      IF HEX-NBR = EBD-FN2                                         EL004
00380          MOVE 'ISSUE RESET'      TO TMR1-A.                       EL004
00381                                                                   EL004
00382      MOVE 20                     TO HEX-BD.                       EL004
00383      IF HEX-NBR = EBD-FN2                                         EL004
00384          MOVE 'ISSUE DISCONNECT' TO TMR1-A.                       EL004
00385                                                                   EL004
00386      MOVE 22                     TO HEX-BD.                       EL004
00387      IF HEX-NBR = EBD-FN2                                         EL004
00388          MOVE 'ISSUE ENDOUTPUT'  TO TMR1-A.                       EL004
00389                                                                   EL004
00390      MOVE 24                     TO HEX-BD.                       EL004
00391      IF HEX-NBR = EBD-FN2                                         EL004
00392          MOVE 'ISSUE ERASEAUP'   TO TMR1-A.                       EL004
00393                                                                   EL004
00394      MOVE 26                     TO HEX-BD.                       EL004
00395      IF HEX-NBR = EBD-FN2                                         EL004
00396          MOVE 'ISSUE ENDFILE'    TO TMR1-A.                       EL004
00397                                                                   EL004
00398      MOVE 28                     TO HEX-BD.                       EL004
00399      IF HEX-NBR = EBD-FN2                                         EL004
00400          MOVE 'ISSUE PRINT'      TO TMR1-A.                       EL004
00401                                                                   EL004
00402      MOVE 04                     TO HEX-BD.                       EL004
00403      IF HEX-NBR = EBD-RC1                                         EL004
00404          MOVE 'EOF ERROR'        TO TMR1-B.                       EL004
00405                                                                   EL004
00406      MOVE 16                     TO HEX-BD.                       EL004
00407      IF HEX-NBR = EBD-RC1                                         EL004
00408          MOVE 'EODS ERROR'       TO TMR1-B.                       EL004
00409                                                                   EL004
00410      MOVE 193                    TO HEX-BD.                       EL004
00411      IF HEX-NBR = EBD-RC1                                         EL004
00412          MOVE 'EOF ERROR'        TO TMR1-B.                       EL004
00413                                                                   EL004
00414      MOVE 194                    TO HEX-BD.                       EL004
00415      IF HEX-NBR = EBD-RC1                                         EL004
00416          MOVE 'ENDINPT'          TO TMR1-B.                       EL004
00417                                                                   EL004
00418      MOVE 225                    TO HEX-BD.                       EL004
00419      IF HEX-NBR = EBD-RC1                                         EL004
00420          MOVE 'LENGERR'          TO TMR1-B.                       EL004
00421                                                                   EL004
00422      MOVE 227                    TO HEX-BD.                       EL004
00423      IF HEX-NBR = EBD-RC1                                         EL004
00424          MOVE 'WRBRK ERROR'      TO TMR1-B.                       EL004
00425                                                                   EL004
00426      MOVE 228                    TO HEX-BD.                       EL004
00427      IF HEX-NBR = EBD-RC1                                         EL004
00428          MOVE 'RDATT ERROR'      TO TMR1-B.                       EL004
00429                                                                   EL004
00430      MOVE 229                    TO HEX-BD.                       EL004
00431      IF HEX-NBR = EBD-RC1                                         EL004
00432          MOVE 'SIGNAL ERROR'     TO TMR1-B.                       EL004
00433                                                                   EL004
00434      MOVE 230                    TO HEX-BD.                       EL004
00435      IF HEX-NBR = EBD-RC1                                         EL004
00436          MOVE 'TERMIDERR'        TO TMR1-B.                       EL004
00437                                                                   EL004
00438      MOVE 231                    TO HEX-BD.                       EL004
00439      IF HEX-NBR = EBD-RC1                                         EL004
00440          MOVE 'NOPASSBKRD'       TO TMR1-B.                       EL004
00441                                                                   EL004
00442      MOVE 232                    TO HEX-BD.                       EL004
00443      IF HEX-NBR = EBD-RC1                                         EL004
00444          MOVE 'NOPASSBKWR'       TO TMR1-B.                       EL004
00445                                                                   EL004
00446      MOVE 32                     TO HEX-BD.                       EL004
00447      IF HEX-NBR = EBD-RC1                                         EL004
00448          MOVE 'EOC ERROR'        TO TMR1-B.                       EL004
00449                                                                   EL004
00450      MOVE 64                     TO HEX-BD.                       EL004
00451      IF HEX-NBR = EBD-RC1                                         EL004
00452          MOVE 'INBFMH ERROR'     TO TMR1-B.                       EL004
00453                                                                   EL004
00454      MOVE 246                    TO HEX-BD.                       EL004
00455      IF HEX-NBR = EBD-RC1                                         EL004
00456          MOVE 'NOSTART ERROR'    TO TMR1-B.                       EL004
00457                                                                   EL004
00458      MOVE 247                    TO HEX-BD.                       EL004
00459      IF HEX-NBR = EBD-RC1                                         EL004
00460          MOVE 'NONVAL ERROR'     TO TMR1-B.                       EL004
00461                                                                   EL004
00462  125-ISS-900-EXIT.                                                EL004
00463                                                                   EL004
00464      EXIT.                                                        EL004
00465                                                                   EL004
00466      EJECT                                                        EL004
00467  150-FILE SECTION.                                                EL004
00468                                                                   EL004
00469  150-FLE-10-NOTES.                                                EL004
00470 *                                                                 EL004
00471 *    THIS SECTION HANDLES FILE ERRORS                             EL004
00472 *                                                                 EL004
00473      MOVE SAVE-DS                TO TMR2-A.                       EL004
00474                                                                   EL004
00475      MOVE 02                     TO HEX-BD.                       EL004
00476      IF HEX-NBR = EBD-FN2 MOVE 'READ' TO TMR2-B.                  EL004
00477                                                                   EL004
00478      MOVE 04                     TO HEX-BD.                       EL004
00479      IF HEX-NBR = EBD-FN2 MOVE 'WRITE' TO TMR2-B.                 EL004
00480                                                                   EL004
00481      MOVE 06                     TO HEX-BD.                       EL004
00482      IF HEX-NBR = EBD-FN2 MOVE 'REWRITE' TO TMR2-B.               EL004
00483                                                                   EL004
00484      MOVE 08                     TO HEX-BD.                       EL004
00485      IF HEX-NBR = EBD-FN2 MOVE 'DELETE' TO TMR2-B.                EL004
00486                                                                   EL004
00487      MOVE 10                     TO HEX-BD.                       EL004
00488      IF HEX-NBR = EBD-FN2 MOVE 'UNLOCK' TO TMR2-B.                EL004
00489                                                                   EL004
00490      MOVE 12                     TO HEX-BD.                       EL004
00491      IF HEX-NBR = EBD-FN2 MOVE 'STARTBR' TO TMR2-B.               EL004
00492                                                                   EL004
00493      MOVE 14                     TO HEX-BD.                       EL004
00494      IF HEX-NBR = EBD-FN2 MOVE 'READNEXT' TO TMR2-B.                 CL**2
00495                                                                   EL004
00496      MOVE 16                     TO HEX-BD.                       EL004
00497      IF HEX-NBR = EBD-FN2 MOVE 'READPREV' TO TMR2-B.              EL004
00498                                                                   EL004
00499      MOVE 18                     TO HEX-BD.                       EL004
00500      IF HEX-NBR = EBD-FN2 MOVE 'ENDBR' TO TMR2-B.                 EL004
00501                                                                   EL004
00502      MOVE 20                     TO HEX-BD.                       EL004
00503      IF HEX-NBR = EBD-FN2 MOVE 'RESETBR' TO TMR2-B.               EL004
00504                                                                   EL004
00505      MOVE 01                     TO HEX-BD.                       EL004
00506      IF HEX-NBR = EBD-RC1 MOVE 'DSIDERR' TO TMR2-C.               EL004
00507                                                                   EL004
00508      MOVE 02                     TO HEX-BD.                       EL004
00509      IF HEX-NBR = EBD-RC1 MOVE 'ILLOGIC' TO TMR2-C.               EL004
00510                                                                   EL004
00511      MOVE 04                     TO HEX-BD.                       EL004
00512      IF HEX-NBR = EBD-RC1 MOVE 'SEGIDERR' TO TMR2-C.              EL004
00513                                                                   EL004
00514      MOVE 08                     TO HEX-BD.                       EL004
00515      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ'  TO TMR2-C.               EL004
00516                                                                   EL004
00517      MOVE 12                     TO HEX-BD.                       EL004
00518      IF HEX-NBR = EBD-RC1 MOVE 'NOTOPEN' TO TMR2-C                EL004
00519         GO TO 150-FLE-900-EXIT.                                   EL004
00520                                                                   EL004
00521      MOVE 13                     TO HEX-BD.                          CL**2
00522      IF HEX-NBR = EBD-RC1 MOVE 'DISABLED' TO TMR2-C                  CL**2
00523         GO TO 150-FLE-900-EXIT.                                      CL**2
00524                                                                      CL**2
00525      MOVE 15                     TO HEX-BD.                       EL004
00526      IF HEX-NBR = EBD-RC1 MOVE 'ENDFILE' TO TMR2-C.               EL004
00527                                                                   EL004
00528      MOVE 128                    TO HEX-BD.                       EL004
00529      IF HEX-NBR = EBD-RC1 MOVE 'IOERR'   TO TMR2-C.               EL004
00530                                                                   EL004
00531      MOVE 129                    TO HEX-BD.                       EL004
00532      IF HEX-NBR = EBD-RC1 MOVE 'NOTFND'  TO TMR2-C.               EL004
00533                                                                   EL004
00534      MOVE 130                    TO HEX-BD.                       EL004
00535      IF HEX-NBR = EBD-RC1 MOVE 'DUPREC'  TO TMR2-C.               EL004
00536                                                                   EL004
00537      MOVE 131                    TO HEX-BD.                       EL004
00538      IF HEX-NBR = EBD-RC1 MOVE 'NOSPACE' TO TMR2-C.               EL004
00539                                                                   EL004
00540      MOVE 132                    TO HEX-BD.                       EL004
00541      IF HEX-NBR = EBD-RC1 MOVE 'DUPREC'  TO TMR2-C.               EL004
00542                                                                   EL004
00543      MOVE 208                    TO HEX-BD.                       EL004
00544      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR2-C.              EL004
00545                                                                   EL004
00546      MOVE 209                    TO HEX-BD.                       EL004
00547      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVREQ' TO TMR2-C.             EL004
00548                                                                   EL004
00549      MOVE 225                    TO HEX-BD.                       EL004
00550      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR2-C.               EL004
00551                                                                   EL004
00552      MOVE +1                     TO  WS-DUMP-SW.                  EL004
00553                                                                   EL004
00554  150-FLE-900-EXIT.                                                EL004
00555      EXIT.                                                        EL004
00556                                                                   EL004
00557      EJECT                                                        EL004
00558  175-TD SECTION.                                                  EL004
00559                                                                   EL004
00560  175-TD-10-NOTES.                                                 EL004
00561 *                                                                 EL004
00562 *    THIS SECTION HANDLES TRANSIENT DATA ERRORS                   EL004
00563 *                                                                 EL004
00564      MOVE 02                     TO HEX-BD.                       EL004
00565      IF HEX-NBR = EBD-FN2 MOVE 'WRITEQ TD' TO TMR1-A.             EL004
00566      MOVE 04                     TO HEX-BD.                       EL004
00567      IF HEX-NBR = EBD-FN2 MOVE 'READQ TD' TO TMR1-A.              EL004
00568                                                                   EL004
00569      MOVE 06                     TO HEX-BD.                       EL004
00570      IF HEX-NBR = EBD-FN2 MOVE 'DELETEQ TD' TO TMR1-A.            EL004
00571                                                                   EL004
00572      MOVE 01                     TO HEX-BD.                       EL004
00573      IF HEX-NBR = EBD-RC1 MOVE 'QZERO' TO TMR1-B.                 EL004
00574                                                                   EL004
00575      MOVE 02                     TO HEX-BD.                       EL004
00576      IF HEX-NBR = EBD-RC1 MOVE 'QIDERR' TO TMR1-B.                EL004
00577                                                                   EL004
00578      MOVE 04                     TO HEX-BD.                       EL004
00579      IF HEX-NBR = EBD-RC1 MOVE 'IOERR' TO TMR1-B.                 EL004
00580                                                                   EL004
00581      MOVE 08                     TO HEX-BD.                       EL004
00582      IF HEX-NBR = EBD-RC1 MOVE 'NOTOPEN' TO TMR1-B.               EL004
00583                                                                   EL004
00584      MOVE 16                     TO HEX-BD.                       EL004
00585      IF HEX-NBR = EBD-RC1 MOVE 'NOSPACE' TO TMR1-B.               EL004
00586                                                                   EL004
00587      MOVE 192                    TO HEX-BD.                       EL004
00588      IF HEX-NBR = EBD-RC1 MOVE 'QBUSY' TO TMR1-B.                 EL004
00589                                                                   EL004
00590      MOVE 208                    TO HEX-BD.                       EL004
00591      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR1-B.              EL004
00592                                                                   EL004
00593      MOVE 209                    TO HEX-BD.                       EL004
00594      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVREQ' TO TMR1-B.             EL004
00595                                                                   EL004
00596      MOVE 225                    TO HEX-BD.                       EL004
00597      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.               EL004
00598                                                                   EL004
00599  175-TDS-900-EXIT.                                                EL004
00600      EXIT.                                                        EL004
00601                                                                   EL004
00602      EJECT                                                        EL004
00603  200-TS SECTION.                                                  EL004
00604                                                                   EL004
00605  200-TSS-10-NOTES.                                                EL004
00606 *                                                                 EL004
00607 *    THIS SECTION HANDLES TEMP STORAGE                            EL004
00608 *                                                                 EL004
00609      MOVE 02                     TO HEX-BD.                       EL004
00610      IF HEX-NBR = EBD-FN2 MOVE 'WRITEQ TS' TO TMR1-A.             EL004
00611                                                                   EL004
00612      MOVE 04                     TO HEX-BD.                       EL004
00613      IF HEX-NBR = EBD-FN2 MOVE 'READQ TS' TO TMR1-A.              EL004
00614                                                                   EL004
00615      MOVE 06                     TO HEX-BD.                       EL004
00616      IF HEX-NBR = EBD-FN2 MOVE 'DELETEQ TS' TO TMR1-A.            EL004
00617                                                                   EL004
00618      MOVE 01                     TO HEX-BD.                       EL004
00619      IF HEX-NBR = EBD-RC1 MOVE 'ITEMERR' TO TMR1-B.               EL004
00620                                                                   EL004
00621      MOVE 02                     TO HEX-BD.                       EL004
00622      IF HEX-NBR = EBD-RC1 MOVE 'QIDERR'  TO TMR1-B.               EL004
00623                                                                   EL004
00624      MOVE 04                     TO HEX-BD.                       EL004
00625      IF HEX-NBR = EBD-RC1 MOVE 'IOERR'   TO TMR1-B.               EL004
00626                                                                   EL004
00627      MOVE 08                     TO HEX-BD.                       EL004
00628      IF HEX-NBR = EBD-RC1 MOVE 'NOSPACE' TO TMR1-B.               EL004
00629                                                                   EL004
00630      MOVE 32                     TO HEX-BD.                       EL004
00631      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ'  TO TMR1-B.               EL004
00632                                                                   EL004
00633      MOVE 208                    TO HEX-BD.                       EL004
00634      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR1-B.              EL004
00635                                                                   EL004
00636      MOVE 209                    TO HEX-BD.                       EL004
00637      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVREQ' TO TMR1-B.             EL004
00638                                                                   EL004
00639      MOVE 225                    TO HEX-BD.                       EL004
00640      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.               EL004
00641                                                                   EL004
00642  200-TSS-900-EXIT.                                                EL004
00643      EXIT.                                                        EL004
00644                                                                   EL004
00645      EJECT                                                        EL004
00646  225-CORE SECTION.                                                EL004
00647                                                                   EL004
00648  225-CES-10-NOTES.                                                EL004
00649 *                                                                 EL004
00650 *    THIS SECTION HANDLES GETMAIN AND FREEMAIN                    EL004
00651 *                                                                 EL004
00652      MOVE 02                     TO HEX-BD.                       EL004
00653      IF HEX-NBR = EBD-FN2 MOVE 'GETMAIN' TO TMR1-A.               EL004
00654                                                                   EL004
00655      MOVE 04                     TO HEX-BD.                       EL004
00656      IF HEX-NBR = EBD-FN2 MOVE 'FREEMAIN' TO TMR1-A.              EL004
00657                                                                   EL004
00658      MOVE 226                    TO HEX-BD.                       EL004
00659      IF HEX-NBR = EBD-RC1 MOVE 'NOSTG  ' TO TMR1-B.               EL004
00660                                                                   EL004
00661  225-CDS-900-EXIT.                                                EL004
00662      EXIT.                                                        EL004
00663                                                                   EL004
00664      EJECT                                                        EL004
00665  250-ABEND SECTION.                                               EL004
00666                                                                   EL004
00667  250-ABS-10-NOTES.                                                EL004
00668 *                                                                 EL004
00669 *    THIS SECTION HANDLES ABEND CODES                             EL004
00670 *                                                                 EL004
00671      MOVE 02                     TO HEX-BD.                       EL004
00672      IF HEX-NBR = EBD-FN2 MOVE 'LINK' TO TMR1-A                      CL**4
00673      MOVE W-EIBRSRCE TO TMR2-B.                                      CL**4
00674                                                                   EL004
00675      MOVE 04                     TO HEX-BD.                       EL004
00676      IF HEX-NBR = EBD-FN2 MOVE 'XCTL' TO TMR1-A.                  EL004
00677                                                                   EL004
00678      MOVE 06                     TO HEX-BD.                       EL004
00679      IF HEX-NBR = EBD-FN2 MOVE 'LOAD' TO TMR1-A.                  EL004
00680                                                                   EL004
00681      MOVE 08                     TO HEX-BD.                       EL004
00682      IF HEX-NBR = EBD-FN2 MOVE 'RETURN' TO TMR1-A.                EL004
00683                                                                   EL004
00684      MOVE 10                     TO HEX-BD.                       EL004
00685      IF HEX-NBR = EBD-FN2 MOVE 'RELEASE' TO TMR1-A.               EL004
00686                                                                   EL004
00687      MOVE 12                     TO HEX-BD.                       EL004
00688      IF HEX-NBR = EBD-FN2 MOVE 'ABEND' TO TMR1-A.                 EL004
00689                                                                   EL004
00690      MOVE 14                     TO HEX-BD.                       EL004
00691      IF HEX-NBR = EBD-FN2 MOVE 'HANDLE ABEND' TO TMR1-A.          EL004
00692                                                                   EL004
00693      MOVE 01                     TO HEX-BD.                       EL004
00694      IF HEX-NBR = EBD-RC1 MOVE 'PGMIDERR' TO TMR1-B.              EL004
00695                                                                   EL004
00696      MOVE 214                    TO HEX-BD.                          CL**4
00697      IF HEX-NBR = EBD-RC1 MOVE 'NOTAUTH ' TO TMR1-B.                 CL**4
00698                                                                      CL**4
00699      MOVE 224                    TO HEX-BD.                       EL004
00700      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ' TO TMR1-B.                EL004
00701                                                                   EL004
00702  250-ABS-900-EXIT.                                                EL004
00703      EXIT.                                                        EL004
00704                                                                   EL004
00705      EJECT                                                        EL004
00706  275-POST SECTION.                                                EL004
00707                                                                   EL004
00708  275-PTS-10-NOTES.                                                EL004
00709 *                                                                 EL004
00710 *    THIS SECTION HANDLES TIME RELATED CONDITIONS                 EL004
00711 *                                                                 EL004
00712      MOVE 02                     TO HEX-BD.                       EL004
00713      IF HEX-NBR = EBD-FN2 MOVE 'ASKTIME' TO TMR1-A.               EL004
00714                                                                   EL004
00715      MOVE 04                     TO HEX-BD.                       EL004
00716      IF HEX-NBR = EBD-FN2 MOVE 'DELAY' TO TMR1-A.                 EL004
00717                                                                   EL004
00718      MOVE 06                     TO HEX-BD.                       EL004
00719      IF HEX-NBR = EBD-FN2 MOVE 'POST' TO TMR1-A.                  EL004
00720                                                                   EL004
00721      MOVE 08                     TO HEX-BD.                       EL004
00722      IF HEX-NBR = EBD-FN2 MOVE 'START' TO TMR1-A.                 EL004
00723                                                                   EL004
00724      MOVE 10                     TO HEX-BD.                       EL004
00725      IF HEX-NBR = EBD-FN2 MOVE 'RETRIEVE' TO TMR1-A.              EL004
00726                                                                   EL004
00727      MOVE 12                     TO HEX-BD.                       EL004
00728      IF HEX-NBR = EBD-FN2 MOVE 'CANCEL'  TO TMR1-A.               EL004
00729                                                                   EL004
00730      MOVE 01                     TO HEX-BD.                       EL004
00731      IF HEX-NBR = EBD-RC1 MOVE 'ENDDATA' TO TMR1-B.               EL004
00732                                                                   EL004
00733      MOVE 04                     TO HEX-BD.                       EL004
00734      IF HEX-NBR = EBD-RC1 MOVE 'IOERR'   TO TMR1-B.               EL004
00735                                                                   EL004
00736      MOVE 17                     TO HEX-BD.                       EL004
00737      IF HEX-NBR = EBD-RC1 MOVE 'TRANSIDERR' TO TMR1-B.            EL004
00738                                                                   EL004
00739      MOVE 18                     TO HEX-BD.                       EL004
00740      IF HEX-NBR = EBD-RC1 MOVE 'TERMIDERR' TO TMR1-B.             EL004
00741                                                                   EL004
00742      MOVE 20                     TO HEX-BD.                       EL004
00743      IF HEX-NBR = EBD-RC1 MOVE 'INVTSREQ' TO TMR1-B.              EL004
00744                                                                   EL004
00745      MOVE 32                     TO HEX-BD.                       EL004
00746      IF HEX-NBR = EBD-RC1 MOVE 'EXPIRED' TO TMR1-B.               EL004
00747                                                                   EL004
00748      MOVE 129                    TO HEX-BD.                       EL004
00749      IF HEX-NBR = EBD-RC1 MOVE 'NOTFND'  TO TMR1-B.               EL004
00750                                                                   EL004
00751      MOVE 208                    TO HEX-BD.                       EL004
00752      IF HEX-NBR = EBD-RC1 MOVE 'SYSIDERR' TO TMR1-B.              EL004
00753                                                                   EL004
00754      MOVE 209                    TO HEX-BD.                       EL004
00755      IF HEX-NBR = EBD-RC1 MOVE 'ISCINVRER' TO TMR1-B.             EL004
00756                                                                   EL004
00757      MOVE 225                    TO HEX-BD.                       EL004
00758      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.               EL004
00759                                                                   EL004
00760      MOVE 233                    TO HEX-BD.                       EL004
00761      IF HEX-NBR = EBD-RC1 MOVE 'ENVDEFERR' TO TMR1-B.             EL004
00762                                                                   EL004
00763      MOVE 255                    TO HEX-BD.                       EL004
00764      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ'  TO TMR1-B.               EL004
00765                                                                   EL004
00766  275-PTS-900-EXIT.                                                EL004
00767      EXIT.                                                        EL004
00768                                                                   EL004
00769      EJECT                                                        EL004
00770  300-ENQ SECTION.                                                 EL004
00771                                                                   EL004
00772  300-ENQ-10-NOTES.                                                EL004
00773 *                                                                 EL004
00774 *    THIS SECTION HANDLES WAIT EVENT, ENQ, DEQ, AND SUSPEND       EL004
00775 *                                                                 EL004
00776      MOVE 02                     TO HEX-BD.                       EL004
00777      IF HEX-NBR = EBD-FN2 MOVE 'WAIT EVENT' TO TMR1-A.            EL004
00778                                                                   EL004
00779      MOVE 04                     TO HEX-BD.                       EL004
00780      IF HEX-NBR = EBD-FN2 MOVE 'ENQ' TO TMR1-A.                   EL004
00781                                                                   EL004
00782      MOVE 06                     TO HEX-BD.                       EL004
00783      IF HEX-NBR = EBD-FN2 MOVE 'DEQ' TO TMR1-A.                   EL004
00784                                                                   EL004
00785      MOVE 08                     TO HEX-BD.                       EL004
00786      IF HEX-NBR = EBD-FN2 MOVE 'SUSPEND' TO TMR1-A.               EL004
00787                                                                   EL004
00788      MOVE 50                     TO HEX-BD.                       EL004
00789      IF HEX-NBR = EBD-RC1 MOVE 'ENQBUSY' TO TMR1-B.               EL004
00790                                                                   EL004
00791  300-ENQ-900-EXIT.                                                EL004
00792      EXIT.                                                        EL004
00793                                                                   EL004
00794      EJECT                                                        EL004
00795  325-JOURNAL SECTION.                                             EL004
00796                                                                   EL004
00797  325-JUS-10-NOTES.                                                EL004
00798 *                                                                 EL004
00799 *    THIS SECTION HANDLES ALL JOURNAL ABENDS                      EL004
00800 *                                                                 EL004
00801      MOVE 02                     TO HEX-BD.                       EL004
00802      IF HEX-NBR = EBD-FN2 MOVE 'JOURNAL' TO TMR1-A.               EL004
00803                                                                   EL004
00804      MOVE 04                     TO HEX-BD.                       EL004
00805      IF HEX-NBR = EBD-FN2 MOVE 'WAIT JOURNAL' TO TMR1-A.          EL004
00806                                                                   EL004
00807      MOVE 01                     TO HEX-BD.                       EL004
00808      IF HEX-NBR = EBD-RC1 MOVE 'JIDERR' TO TMR1-B.                EL004
00809                                                                   EL004
00810      MOVE 02                     TO HEX-BD.                       EL004
00811      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ' TO TMR1-B.                EL004
00812                                                                   EL004
00813      MOVE 05                     TO HEX-BD.                       EL004
00814      IF HEX-NBR = EBD-RC1 MOVE 'NOTOPEN' TO TMR1-B.               EL004
00815                                                                   EL004
00816      MOVE 06                     TO HEX-BD.                       EL004
00817      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.               EL004
00818                                                                   EL004
00819      MOVE 07                     TO HEX-BD.                       EL004
00820      IF HEX-NBR = EBD-RC1 MOVE 'IOERR' TO TMR1-B.                 EL004
00821                                                                   EL004
00822      MOVE 09                     TO HEX-BD.                       EL004
00823      IF HEX-NBR = EBD-RC1 MOVE 'NOJBUFSP' TO TMR1-B.              EL004
00824                                                                   EL004
00825  325-JUS-900-EXIT.                                                EL004
00826      EXIT.                                                        EL004
00827                                                                   EL004
00828      EJECT                                                        EL004
00829  350-MAP SECTION.                                                 EL004
00830                                                                   EL004
00831  350-MPS-10-NOTES.                                                EL004
00832 *                                                                 EL004
00833 *    THIS SECTION HANDLES ALL MAP ERRORS                          EL004
00834 *                                                                 EL004
00835      MOVE 02                     TO HEX-BD.                       EL004
00836      IF HEX-NBR = EBD-FN2 MOVE 'RECEIVE MAP' TO TMR1-A.           EL004
00837                                                                   EL004
00838      MOVE 04                     TO HEX-BD.                       EL004
00839      IF HEX-NBR = EBD-FN2 MOVE 'SEND MAP' TO TMR1-A.              EL004
00840                                                                   EL004
00841      MOVE 06                     TO HEX-BD.                       EL004
00842      IF HEX-NBR = EBD-FN2 MOVE 'SEND TEXT' TO TMR1-A.             EL004
00843                                                                   EL004
00844      MOVE 08                     TO HEX-BD.                       EL004
00845      IF HEX-NBR = EBD-FN2 MOVE 'SEND PAGE' TO TMR1-A.             EL004
00846                                                                   EL004
00847      MOVE 10                     TO HEX-BD.                       EL004
00848      IF HEX-NBR = EBD-FN2 MOVE 'PURGE MESSAGE' TO TMR1-A.         EL004
00849                                                                   EL004
00850      MOVE 12                     TO HEX-BD.                       EL004
00851      IF HEX-NBR = EBD-FN2 MOVE 'ROUTE' TO TMR1-A.                 EL004
00852                                                                   EL004
00853      MOVE 01                     TO HEX-BD.                       EL004
00854      IF HEX-NBR = EBD-RC1 MOVE 'INVREQ' TO TMR1-B.                EL004
00855                                                                   EL004
00856      MOVE 02                     TO HEX-BD.                       EL004
00857      IF HEX-NBR = EBD-RC1 MOVE 'RETPAGE' TO TMR1-B.               EL004
00858                                                                   EL004
00859      MOVE 04                     TO HEX-BD.                       EL004
00860      IF HEX-NBR = EBD-RC1 MOVE 'MAPFAIL' TO TMR1-B.               EL004
00861                                                                   EL004
00862      MOVE 08                     TO HEX-BD.                       EL004
00863      IF HEX-NBR = EBD-RC1 MOVE 'INVMPSZ' TO TMR1-B.               EL004
00864                                                                   EL004
00865      MOVE 32                     TO HEX-BD.                       EL004
00866      IF HEX-NBR = EBD-RC1 MOVE 'INVERRTERM' TO TMR1-B.            EL004
00867                                                                   EL004
00868      MOVE 64                     TO HEX-BD.                       EL004
00869      IF HEX-NBR = EBD-RC1 MOVE 'RTESOME' TO TMR1-B.               EL004
00870                                                                   EL004
00871      MOVE 128                    TO HEX-BD.                       EL004
00872      IF HEX-NBR = EBD-RC1 MOVE 'RTEFAIL' TO TMR1-B.               EL004
00873                                                                   EL004
00874      MOVE 227                    TO HEX-BD.                       EL004
00875      IF HEX-NBR = EBD-RC1 MOVE 'WRBRK'  TO TMR1-B.                EL004
00876                                                                   EL004
00877      MOVE 228                    TO HEX-BD.                       EL004
00878      IF HEX-NBR = EBD-RC1 MOVE 'RDATT' TO TMR1-B.                 EL004
00879                                                                   EL004
00880      MOVE 16                     TO HEX-BD.                       EL004
00881      IF HEX-NBR = EBD-RC2 MOVE 'INVLDC' TO TMR1-B.                EL004
00882                                                                   EL004
00883      MOVE 128                    TO HEX-BD.                       EL004
00884      IF HEX-NBR = EBD-RC2 MOVE 'TSIOERR' TO TMR1-B.               EL004
00885                                                                   EL004
00886      MOVE 01                     TO HEX-BD.                       EL004
00887      IF HEX-NBR = EBD-RC3 MOVE 'OVERFLOW' TO TMR1-B.              EL004
00888                                                                   EL004
00889  350-MPS-900-EXIT.                                                EL004
00890      EXIT.                                                        EL004
00891                                                                   EL004
00892      EJECT                                                        EL004
00893  375-ISS-QUERY SECTION.                                           EL004
00894                                                                   EL004
00895  375-ISS-10-NOTES.                                                EL004
00896 *                                                                 EL004
00897 *    THIS SECTION HANDLES ALL ISSUE ERRORS                        EL004
00898 *                                                                 EL004
00899      MOVE 02                     TO HEX-BD.                       EL004
00900      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE ADD' TO TMR1-A.             EL004
00901                                                                   EL004
00902      MOVE 04                     TO HEX-BD.                       EL004
00903      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE ERASE' TO TMR1-A.           EL004
00904                                                                   EL004
00905      MOVE 06                     TO HEX-BD.                       EL004
00906      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE REPLACE' TO TMR1-A.         EL004
00907                                                                   EL004
00908      MOVE 08                     TO HEX-BD.                       EL004
00909      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE ABORT' TO TMR1-A.           EL004
00910                                                                   EL004
00911      MOVE 10                     TO HEX-BD.                       EL004
00912      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE QUERY' TO TMR1-A.           EL004
00913                                                                   EL004
00914      MOVE 12                     TO HEX-BD.                       EL004
00915      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE END' TO TMR1-A.             EL004
00916                                                                   EL004
00917      MOVE 14                     TO HEX-BD.                       EL004
00918      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE RECEIVE' TO TMR1-A.         EL004
00919                                                                   EL004
00920      MOVE 16                     TO HEX-BD.                       EL004
00921      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE NOTE' TO TMR1-A.            EL004
00922                                                                   EL004
00923      MOVE 18                     TO HEX-BD.                       EL004
00924      IF HEX-NBR = EBD-FN2 MOVE 'ISSUE WAIT' TO TMR1-A.            EL004
00925                                                                   EL004
00926      MOVE 04                     TO HEX-BD.                       EL004
00927      IF HEX-NBR = EBD-RC3 MOVE 'EDOS' TO TMR1-B.                  EL004
00928                                                                   EL004
00929      MOVE 08                     TO HEX-BD.                       EL004
00930      IF HEX-NBR = EBD-RC3 MOVE 'EOC' TO TMR1-B.                   EL004
00931                                                                   EL004
00932      MOVE 16                     TO HEX-BD.                       EL004
00933      IF HEX-NBR = EBD-RC3 MOVE 'IGREQID' TO TMR1-B.               EL004
00934                                                                   EL004
00935      MOVE 04                     TO HEX-BD.                       EL004
00936      IF HEX-NBR = EBD-RC1 MOVE 'DSSTAT' TO TMR1-B.                EL004
00937                                                                   EL004
00938      MOVE 08                     TO HEX-BD.                       EL004
00939      IF HEX-NBR = EBD-RC1 MOVE 'FUNCERR' TO TMR1-B.               EL004
00940                                                                   EL004
00941      MOVE 12                     TO HEX-BD.                       EL004
00942      IF HEX-NBR = EBD-RC1 MOVE 'SELNERR' TO TMR1-B.               EL004
00943                                                                   EL004
00944      MOVE 16                     TO HEX-BD.                       EL004
00945      IF HEX-NBR = EBD-RC1 MOVE 'UNEXPIN' TO TMR1-B.               EL004
00946                                                                   EL004
00947      MOVE 225                    TO HEX-BD.                       EL004
00948      IF HEX-NBR = EBD-RC1 MOVE 'LENGERR' TO TMR1-B.               EL004
00949                                                                   EL004
00950      MOVE 17                     TO HEX-BD.                       EL004
00951      IF HEX-NBR = EBD-RC2 MOVE 'EDOS' TO TMR1-B.                  EL004
00952                                                                   EL004
00953      MOVE 32                     TO HEX-BD.                       EL004
00954      IF HEX-NBR = EBD-RC3 MOVE 'EOC' TO TMR1-B.                   EL004
00955                                                                   EL004
00956  375-ISS-900-EXIT.                                                EL004
00957      EXIT.                                                        EL004
00958                                                                   EL004
00959      EJECT                                                        EL004
00960  1000-CONVERT-HEX-CHARACTERS SECTION.                             EL004
00961      IF WS-INDEX GREATER THAN +1                                  EL004
00962          MULTIPLY WS-INDEX BY +2 GIVING WS-INDEX2                 EL004
00963        ELSE                                                       EL004
00964          MOVE +1                 TO  WS-INDEX2.                   EL004
00965                                                                   EL004
00966      MOVE WS-CHARACTER (WS-INDEX) TO WS-CHAR                      EL004
00967                                                                   EL004
00968      IF WS-NUMBER = ZERO                                          EL004
00969          MOVE ZERO               TO  WS-ZONE (WS-INDEX)           EL004
00970                                      WS-DIGIT (WS-INDEX)          EL004
00971          GO TO 1099-EXIT.                                         EL004
00972                                                                   EL004
00973      DIVIDE WS-NUMBER BY +16 GIVING WS-NUMBER                     EL004
00974          REMAINDER WS-REMAINDER                                   EL004
00975                                                                   EL004
00976      IF WS-NUMBER NOT = ZERO                                      EL004
00977          MOVE WS-HEX-CHAR (WS-NUMBER) TO  WS-ZONE (WS-INDEX)      EL004
00978        ELSE                                                       EL004
00979          MOVE ZERO               TO  WS-ZONE (WS-INDEX).          EL004
00980                                                                   EL004
00981      IF WS-REMAINDER NOT = ZERO                                   EL004
00982          MOVE WS-HEX-CHAR (WS-REMAINDER) TO  WS-DIGIT (WS-INDEX)  EL004
00983        ELSE                                                       EL004
00984          MOVE ZERO               TO  WS-DIGIT (WS-INDEX).         EL004
00985                                                                   EL004
00986  1099-EXIT.                                                       EL004
00987      EXIT.                                                        EL004
00988                                                                   EL004
