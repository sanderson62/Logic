00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL6892
00003  PROGRAM-ID.                 EL6892.                                 LV010
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 02/12/96 09:59:40.                    CL**8
00007 *                            VMOD=2.010.                             CL*10
00008 *                                                                 EL6892
00009 *AUTHOR.           LOGIC,INC.                                        CL**8
00010 *                  DALLAS,TEXAS.                                     CL**8
00011                                                                   EL6892
00012 *DATE-COMPILED.                                                      CL**8
00013                                                                      CL**7
00014 *SECURITY.   *****************************************************   CL**8
00015 *            *                                                   *   CL**8
00016 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *         CL**8
00017 *            *                                                   *   CL**8
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00021 *            *                                                   *   CL**8
00022 *            *****************************************************   CL**8
00023                                                                   EL6892
00024 *REMARKS. TRANSACTION EXH5 - HARDCOPY PRINT PROGRAM                  CL**7
00025 *        THIS PROGRAM IS USED TO PRINT LETTERS TO THE COMPANY        CL**7
00026 *        SPECIFIED PRINTER. IT IS ACTIVATED THROUGH THE START        CL**7
00027 *        COMMAND AND WILL HAVE THE INTERFACE BLOCK PASSED.           CL**7
00028 *        DATA IN THE INTERFACE BLOCK WILL INDICATE THE NUMBER OF     CL**7
00029 *        LINES TO PRINT AND THE NUMBER OF COPIES.                    CL**7
00030 *        IT WILL ALSO CONTAIN THE TEMP STORAGE ID TO USE IN ORDER    CL**7
00031 *        TO RETRIEVE THE DATA TO BE PRINTED.                         CL**7

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

00035  ENVIRONMENT DIVISION.                                            EL6892
00036  DATA DIVISION.                                                   EL6892
00037  WORKING-STORAGE SECTION.                                         EL6892
00038  77  THIS-PGM PIC X(8) VALUE 'EL6892'.                               CL*10
00039  77  FILLER  PIC X(32) VALUE '********************************'.     CL**7
00040  77  FILLER  PIC X(32) VALUE '*   EL6892 WORKING STORAGE     *'.     CL**7
00041  77  FILLER  PIC X(32) VALUE '******* VMOD=2.010 *************'.     CL*10
00042                                                                   EL6892
00043  01  W-CONSTANTS.                                                    CL**7
00044      12  FILLER                  PIC  X(18)                          CL**7
00045                              VALUE 'PROGRAM CONSTANTS:'.             CL**7
00046                                                                      CL**7
00047      12  W-ENTRIES-PER-RECORD    PIC  9(03) COMP-3 VALUE 50.         CL**7
00048      12  W-TS-LENGTH             PIC S9(04) COMP VALUE +3650.        CL**7
00049                                                                      CL**7
00050      12  W-PGM-EL689             PIC  X(08) VALUE 'EL689'.           CL**7
00051      12  W-TOP-OF-FORM-MESSAGE   PIC  X(08) VALUE '*****TOP'.        CL**7
00052      12  W-TOP-FORM              PIC  X(01) VALUE '1'.               CL**7
00053                                                                      CL**7
00054  01  W-WORK-AREAS.                                                   CL**7
00055      12  FILLER                  PIC  X(18)                          CL**7
00056                              VALUE 'PROGRAM WORK AREA:'.             CL**7
00057                                                                      CL**7
00058      12  W-ASKTIME-CTR           PIC S9(04)  COMP.                   CL**7
00059      12  W-NDX                   PIC  9(02).                         CL**7
00060      12  W-RECORDS-PRINTED       PIC S9(03) COMP-3 VALUE 0.          CL**7
00061      12  W-TS-ITEM               PIC  S9(04) COMP.                   CL**7
00062                                                                      CL**7
00063      12  W-ERROR-LINE            PIC  X(70).                         CL**7
00064      12  W-NEXT-TRAN             PIC  X(04).                         CL**7
00065      12  W-TERMINAL-ID.                                              CL**7
00066          18  W-TERM-PREFIX       PIC  X(02).                         CL**7
00067          18  FILLER              PIC  X(02).                         CL**7
00068                                                                      CL**7
00069      12  W-ADJUST-AREA.                                              CL**7
00070          16  FILLER              PIC  X(07).                         CL**8
00071          16  W-AD-PRINT-AREA     PIC  X(70).                         CL**7
00072          16  FILLER              PIC  X(08).                         CL**7
00073                                                                      CL**7
00074      12  W-TS-WORK-AREA          PIC X(3650).                        CL**7
00075      12  W-REC-ENTRIES REDEFINES W-TS-WORK-AREA.                     CL**7
00076          16  W-REC-ENT OCCURS 50 TIMES INDEXED BY W-TB-NDX.          CL**7
00077              20  W-REC-TEXT.                                         CL**7
00078                  24  W-REC-TEXT-TOP                                  CL**7
00079                                  PIC  X(08).                         CL**7
00080                  24  FILLER      PIC  X(62).                         CL**7
00081              20  W-REC-PC        PIC  9(02).                         CL**7
00082              20  W-REC-SC        PIC  X(01).                         CL**7
00083                                   EJECT                              CL**7
00084  01  FILLER                      PIC  X(25)                          CL**7
00085                              VALUE 'PROGRAM INTERFACE STARTS:'.      CL**7
00086      COPY ELCINTF.                                                   CL**7
00087      12  PI-WA  REDEFINES PI-PROGRAM-WORK-AREA.                      CL**7
00088      COPY ELC1042.                                                   CL**7
00089      COPY ELC689PI.                                                  CL**7
00090      16  FILLER                  PIC X(280).                         CL**8
00091                                   EJECT                              CL**7
00092  01  FILLER                      PIC  X(23)                          CL**7
00093                              VALUE 'PRINT WORK AREA STARTS:'.        CL**7
00094      COPY ELPRTCVD.                                                  CL**6
00095  01  FILLER                      PIC  X(21)                          CL**7
00096                              VALUE 'PRINT WORK AREA ENDS:'.          CL**7
00097                                   EJECT                              CL**7
00098      COPY ELCDMD34.                                                  CL*10
00099                                                                      CL*10
00100  PROCEDURE DIVISION.                                              EL6892
00101                                                                      CL*10
00102 *    MOVE +132                   TO  WS-LINE-LEN.                    CL*10
00103      MOVE +85                    TO  WS-LINE-LEN.                    CL*10
00104      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL*10
00105                                                                      CL*10
00106  0100-RETRIEVE-LOOP.                                                 CL*10
00107                                                                      CL**7
00108      EXEC CICS HANDLE CONDITION                                   EL6892
00109           ENDDATA  (0200-END-DATA)                                   CL**7
00110           NOTFND   (0300-NOT-FOUND)                                  CL**7
00111      END-EXEC.                                                    EL6892
00112                                                                   EL6892
00113      EXEC CICS RETRIEVE                                           EL6892
00114           INTO      (PROGRAM-INTERFACE-BLOCK)                        CL**7
00115           LENGTH    (PI-COMM-LENGTH)                                 CL**7
00116      END-EXEC.                                                    EL6892
00117                                                                      CL*10
00118 * DLO034 OPEN WHEN DMD OR CID                                        CL*10
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*10
00120          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL*10
00121              MOVE 'O'                TO DL34-PROCESS-TYPE            CL*10
00122              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL*10
00123              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL*10
00124              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL*10
00125              MOVE SPACES             TO DL34-PRINT-LINE              CL*10
00126              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL*10
00127              EXEC CICS LINK                                          CL*10
00128                  PROGRAM    ('DLO034')                               CL*10
00129                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL*10
00130                  LENGTH     (DLO034-REC-LENGTH)                      CL*10
00131              END-EXEC                                                CL*10
00132              IF DL34-RETURN-CODE NOT = 'OK'                          CL*10
00133                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL*10
00134                                      TO W-ERROR-LINE                 CL*10
00135                  PERFORM 0400-SEND-TEXT                              CL*10
00136                  EXEC CICS RETURN                                    CL*10
00137                  END-EXEC.                                           CL*10
00138                                                                   EL6892
00139      IF  PI-CALLING-PROGRAM = W-PGM-EL689                            CL**7
00140          GO TO 1000-PRINT-EL689.                                     CL**7
00141                                                                   EL6892
00142      GO TO 0100-RETRIEVE-LOOP.                                    EL6892
00143                                   EJECT                              CL**7
00144  0200-END-DATA.                                                      CL**7
CIDMOD*    MOVE 'L'                    TO DRS-SW.                            000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
00146      MOVE 'MX27'                 TO W-NEXT-TRAN.                     CL**7
00147                                                                   EL6892
00148      MOVE EIBTRMID               TO W-TERMINAL-ID                    CL**7
00149                                                                   EL6892
00150 * DLO034 CLOSE                                                       CL*10
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*10
00152          MOVE 'C'                TO DL34-PROCESS-TYPE                CL*10
00153          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL*10
00154          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL*10
00155          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL*10
00156          MOVE SPACES             TO DL34-PRINT-LINE                  CL*10
00157                                     DL34-OVERRIDE-PRINTER-ID         CL*10
00158          EXEC CICS LINK                                              CL*10
00159              PROGRAM    ('DLO034')                                   CL*10
00160              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL*10
00161              LENGTH     (DLO034-REC-LENGTH)                          CL*10
00162          END-EXEC                                                    CL*10
00163          IF DL34-RETURN-CODE NOT = 'OK'                              CL*10
00164              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL*10
00165                                  TO W-ERROR-LINE                     CL*10
00166              PERFORM 0400-SEND-TEXT                                  CL*10
00167              EXEC CICS RETURN                                        CL*10
00168                   TRANSID  (W-NEXT-TRAN)                             CL*10
00169                   COMMAREA (PROGRAM-INTERFACE-BLOCK)                 CL*10
00170                   LENGTH   (PI-COMM-LENGTH)                          CL*10
00171              END-EXEC.                                               CL*10
00172                                                                      CL*10
00173      IF  W-TERM-PREFIX = 'DU'                                        CL**7
00174          EXEC CICS RETURN                                            CL**7
00175               TRANSID  (W-NEXT-TRAN)                                 CL**7
00176               COMMAREA (PROGRAM-INTERFACE-BLOCK)                     CL**7
00177               LENGTH   (PI-COMM-LENGTH)                              CL**7
00178          END-EXEC                                                    CL**7
00179      ELSE                                                            CL**7
00180          EXEC CICS RETURN                                            CL**7
00181          END-EXEC.                                                   CL**7
00182                                                                      CL**7
00183  0300-NOT-FOUND.                                                     CL**7
00184                                                                      CL**7
00185      MOVE 'NO COMMUNICATION AREA FOUND' TO W-ERROR-LINE              CL**7
00186      PERFORM 0400-SEND-TEXT                                          CL**7
00187      GO TO 0200-END-DATA.                                            CL**7
00188                                                                      CL**7
00189  0400-SEND-TEXT.                                                     CL**7
00190                                                                      CL**7
00191      EXEC CICS SEND TEXT                                          EL6892
00192           FROM    (W-ERROR-LINE)                                     CL**7
00193           LENGTH  (70)                                               CL**7
00194      END-EXEC.                                                    EL6892
00195                                   EJECT                              CL**7
00196  1000-PRINT-EL689.                                                EL6892
CIDMOD*    MOVE PI-PROCESSOR-PRINTER TO  CSO-PRINT-ID.                       000
CIDMOD*    MOVE 'F'       TO  DRS-SW.                                        000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
CIDMOD*    MOVE ' '       TO  DRS-SW.                                        000
00197                                                                      CL**7
00198      EXEC CICS HANDLE CONDITION                                   EL6892
00199           QIDERR  (1090-TS-QIDERR)                                   CL**7
00200           ITEMERR (1089-TS-ITEMERR)                                  CL**7
00201      END-EXEC.                                                    EL6892
00202                                                                   EL6892
CIDMOD     IF  PI-CREATE-LABELS                                            CL**7
CIDMOD         SUBTRACT PI-689-NUMBER-LABEL-LINES                          CL**7
CIDMOD             FROM PI-TOTAL-LINES.                                    CL**7
CIDMOD*    SUBTRACT 7 FROM PI-TOTAL-LINES.                                   000
00206                                                                   EL6892
00207  1005-READ-TEMP.                                                  EL6892
00208                                                                      CL**7
00209      PERFORM 1010-TEMP-READ THRU 1019-EXIT                        EL6892
00210              VARYING                                                 CL**7
00211          W-TS-ITEM FROM 1 BY 1                                       CL**7
00212              UNTIL                                                   CL**7
00213          W-TS-ITEM GREATER THAN PI-TEMP-STOR-ITEMS.                  CL**7
00214                                                                   EL6892
00215      MOVE 'X'                    TO WS-PROG-END.                  EL6892
CIDMOD*    MOVE SPACES                 TO WS-PASSED-DATA                     000
CIDMOD*                                   WS-PASSED-CNTL-CHAR.               000
00216      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6892
00217      MOVE ZEROS                  TO W-RECORDS-PRINTED.               CL**7
00218      SUBTRACT 1 FROM PI-689-NUMBER-COPIES.                           CL**7
00219                                                                   EL6892
00220      IF  PI-689-NUMBER-COPIES = 0                                    CL**7
00221          PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT                CL**7
00222          GO TO 0100-RETRIEVE-LOOP.                                   CL**7
00223                                                                   EL6892
00224      GO TO 1005-READ-TEMP.                                        EL6892
00225                                   EJECT                              CL**7
00226  1010-TEMP-READ.                                                  EL6892
00227                                                                      CL**7
00228      EXEC CICS READQ TS                                           EL6892
00229           INTO    (W-TS-WORK-AREA)                                   CL**7
00230           QUEUE   (PI-689-TEMP-STOR-ID)                              CL**7
00231           LENGTH  (W-TS-LENGTH)                                      CL**7
00232           ITEM    (W-TS-ITEM)                                        CL**7
00233      END-EXEC.                                                    EL6892
00234                                                                   EL6892
00235                                                                   EL6892
00236      IF  W-TS-ITEM = 1                                               CL**7
00237              AND                                                     CL**7
00238          W-RECORDS-PRINTED EQUAL ZEROS                               CL**7
00239          SET W-TB-NDX            TO PI-689-NUMBER-LABEL-LINES        CL**7
00240          SET W-TB-NDX UP BY +1                                       CL**7
00241                                                                      CL**7
00242          IF  W-REC-TEXT (W-TB-NDX) EQUAL SPACES                      CL**7
00243              MOVE W-TOP-FORM     TO WS-PASSED-CNTL-CHAR              CL**7
00244              PERFORM 1020-RECORD-PRINT THRU 1029-EXIT                CL**7
00245                      VARYING                                         CL**7
00246                  W-TB-NDX FROM W-TB-NDX BY +1                        CL**7
00247                      UNTIL                                           CL**7
00248                  W-TB-NDX GREATER THAN W-ENTRIES-PER-RECORD          CL**7
00249                                                                      CL**7
00250          ELSE                                                        CL**7
00251              IF  W-REC-TEXT-TOP (W-TB-NDX) EQUAL                     CL**7
00252                      W-TOP-OF-FORM-MESSAGE                           CL**7
00253                  SET W-TB-NDX UP BY +1                               CL**7
00254                  MOVE W-TOP-FORM TO WS-PASSED-CNTL-CHAR              CL**7
00255                                                                      CL**7
00256                  IF  W-REC-TEXT (W-TB-NDX) EQUAL SPACES              CL**7
00257                      PERFORM 1020-RECORD-PRINT THRU 1029-EXIT        CL**7
00258                              VARYING                                 CL**7
00259                          W-TB-NDX FROM W-TB-NDX BY +1                CL**7
00260                              UNTIL                                   CL**7
00261                          W-TB-NDX GREATER THAN                       CL**7
00262                              W-ENTRIES-PER-RECORD                    CL**7
00263                                                                      CL**7
00264                  ELSE                                                CL**7
00265                      MOVE SPACES TO WS-PASSED-DATA                   CL**7
00266                      MOVE -1     TO W-RECORDS-PRINTED                CL**7
00267                      PERFORM 1028-PRINT THRU 1028-EXIT               CL**7
00268                      PERFORM 1020-RECORD-PRINT THRU 1029-EXIT        CL**7
00269                              VARYING                                 CL**7
00270                          W-TB-NDX FROM W-TB-NDX BY +1                CL**7
00271                              UNTIL                                   CL**7
00272                          W-TB-NDX GREATER THAN                       CL**7
00273                              W-ENTRIES-PER-RECORD                    CL**7
00274                                                                      CL**7
00275              ELSE                                                    CL**7
00276                  MOVE W-TOP-FORM TO WS-PASSED-CNTL-CHAR              CL**7
00277                  MOVE SPACES     TO WS-PASSED-DATA                   CL**7
00278                  MOVE -1         TO W-RECORDS-PRINTED                CL**7
00279                  PERFORM 1028-PRINT THRU 1028-EXIT                   CL**7
00280                  PERFORM 1020-RECORD-PRINT THRU 1029-EXIT            CL**7
00281                          VARYING                                     CL**7
00282                      W-TB-NDX FROM W-TB-NDX BY +1                    CL**7
00283                          UNTIL                                       CL**7
00284                      W-TB-NDX GREATER THAN                           CL**7
00285                          W-ENTRIES-PER-RECORD                        CL**7
00286                                                                      CL**7
00287      ELSE                                                            CL**7
00288          PERFORM 1020-RECORD-PRINT THRU 1029-EXIT                    CL**7
00289                  VARYING                                             CL**7
00290              W-TB-NDX FROM 1 BY 1                                    CL**7
00291                  UNTIL                                               CL**7
00292              W-TB-NDX GREATER THAN W-ENTRIES-PER-RECORD.             CL**7
00293                                                                   EL6892
00294  1019-EXIT.                                                       EL6892
00295       EXIT.                                                       EL6892
00296                                   EJECT                              CL**7
00297  1020-RECORD-PRINT.                                               EL6892
00298                                                                   EL6892
00299      IF  W-REC-TEXT-TOP (W-TB-NDX) = W-TOP-OF-FORM-MESSAGE           CL**7
00300          MOVE W-TOP-FORM         TO WS-PASSED-CNTL-CHAR              CL**7
00301          MOVE SPACES             TO WS-PASSED-DATA                   CL**7
00302          MOVE -1                 TO W-RECORDS-PRINTED                CL**7
00303          GO TO 1028-PRINT.                                           CL**7
00304                                                                      CL**7
00305      IF  W-TB-NDX GREATER THAN 50                                    CL**7
00306          MOVE 'INPUT RECORDS GREATER THAN 50'                        CL**7
00307                                  TO W-ERROR-LINE                     CL**7
00308          PERFORM 0400-SEND-TEXT                                      CL**7
00309          SET W-TB-NDX DOWN BY 1                                      CL**7
00310          MOVE W-REC-TEXT (W-TB-NDX)                                  CL**7
00311                                  TO W-ERROR-LINE                     CL**7
00312          PERFORM 0400-SEND-TEXT                                      CL**7
00313          GO TO 0200-END-DATA.                                        CL**7

031011     IF W-REC-TEXT (W-TB-NDX) (1:6) = '&&&&&&'
031011        GO TO 1029-EXIT
031011     END-IF

00315      MOVE SPACES                 TO W-ADJUST-AREA.                   CL**9
00316                                                                      CL**7
00317      MOVE W-REC-TEXT (W-TB-NDX)  TO W-AD-PRINT-AREA.                 CL**7
00318      MOVE W-ADJUST-AREA          TO WS-PASSED-DATA.                  CL**7
00319                                                                      CL**7
00320  1028-PRINT.                                                         CL**7
00321                                                                      CL**7
00322      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6892
00323      MOVE SPACE                  TO WS-PASSED-CNTL-CHAR.          EL6892
00324      ADD 1                       TO W-RECORDS-PRINTED.               CL**7
00325                                                                   EL6892
00326  1028-EXIT.                                                          CL**7
00327       EXIT.                                                          CL**7
00328                                                                   EL6892
00329  1028-CONTINUE.                                                      CL**7
00330                                                                      CL**7
00331      IF  W-RECORDS-PRINTED = PI-TOTAL-LINES                          CL**7
00332          SET W-TB-NDX            TO W-ENTRIES-PER-RECORD             CL**7
00333          SET W-TB-NDX UP BY 1                                        CL**7
00334          GO TO 1029-EXIT.                                            CL**7
00335                                                                   EL6892
00336  1029-EXIT.                                                       EL6892
00337       EXIT.                                                       EL6892
00338                                   EJECT                              CL**7
00339  1089-TS-ITEMERR.                                                 EL6892
00340                                                                      CL**7
00341      MOVE 'TEMP STORAGE RECORD NOT FOUND'                            CL**7
00342                                   TO W-ERROR-LINE.                   CL**7
00343      PERFORM 0400-SEND-TEXT.                                         CL**7
00344      GO TO 0100-RETRIEVE-LOOP.                                    EL6892
00345                                                                   EL6892
00346  1090-TS-QIDERR.                                                  EL6892
00347                                                                      CL**7
00348      MOVE 'NO TEMP STORAGE RECORDS NOT FOUND'                        CL**7
00349                                   TO W-ERROR-LINE.                   CL**7
00350      PERFORM 0400-SEND-TEXT.                                         CL**7
00351      GO TO 0100-RETRIEVE-LOOP.                                    EL6892
00352                                   EJECT                              CL**7
00353  7750-DELETE-TEMP-STOR.                                           EL6892
00354                                                                      CL**7
00355      EXEC CICS HANDLE CONDITION                                   EL6892
00356           QIDERR (7750-EXIT)                                         CL**7
00357      END-EXEC.                                                    EL6892
00358                                                                   EL6892
00359      EXEC CICS DELETEQ TS                                         EL6892
00360           QUEUE  (PI-689-TEMP-STOR-ID)                               CL**7
00361      END-EXEC.                                                    EL6892
00362                                                                   EL6892
00363  7750-EXIT.                                                       EL6892
00364      EXIT.                                                        EL6892
00365                                   EJECT                              CL**7
00366      COPY ELPRTCVP.                                                  CL**7
00367                                                                      CL*10
00368                                                                   EL6892
00369  9999-GOBACK.                                                        CL**7
00370                                                                   EL6892
00371      GOBACK.                                                         CL**7
00372                                                                      CL**7
00373  9999-EXIT.                                                          CL**7
00374      EXIT.                                                           CL**7
