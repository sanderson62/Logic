00001  IDENTIFICATION DIVISION.                                         04/03/97
00002                                                                   EL1792
00003  PROGRAM-ID.                 EL1792.                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/13/96 09:39:33.                    CL**5
00007 *                            VMOD=2.008.                             CL**8
00008 *AUTHOR.        LOGIC, INC.                                          CL**5
00009 *               DALLAS, TEXAS.                                       CL**5
00010                                                                   EL1792
00011 *DATE-COMPILED.                                                      CL**5
00012                                                                   EL1792
00013 *SECURITY.   *****************************************************   CL**5
00014 *            *                                                   *   CL**5
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00016 *            *                                                   *   CL**5
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00020 *            *                                                   *   CL**5
00021 *            *****************************************************   CL**5
00022                                                                   EL1792
00023 *REMARKS. TRANSACTION EX59 - PRINT REPORT.                           CL**2
00024      EJECT                                                        EL1792
00025  ENVIRONMENT DIVISION.                                            EL1792
00026  DATA DIVISION.                                                   EL1792
00027  WORKING-STORAGE SECTION.                                         EL1792
00028 *77  THIS-PGM PIC X(6)  VALUE 'EL1792'.                              CL**7
CIDMOD 77  THIS-PGM PIC X(5)  VALUE SPACES.                                CL**7
00029  77  FILLER  PIC X(32)  VALUE '********************************'. EL1792
00030  77  FILLER  PIC X(32)  VALUE '*   EL1792 WORKING STORAGE     *'. EL1792
00031  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.    CL**8
00032                                                                   EL1792
00033  01  LITERALS-NUMBERS.                                            EL1792
00034      12  LIT-X                   PIC X       VALUE 'X'.           EL1792
00035      12  LIT-1                   PIC X       VALUE '1'.           EL1792
00036      SKIP1                                                        EL1792
00037  01  REPT-KEY.                                                    EL1792
00038      12  REPT-COMPANY-CODE       PIC X.                           EL1792
00039      12  REPT-RECORD-TYPE        PIC X.                           EL1792
00040      12  REPT-REPORT-ID          PIC X(5).                        EL1792
00041      12  REPT-LINE-NUMBER        PIC S9(8)   COMP.                EL1792
00042      SKIP1                                                        EL1792
00043  01  WS-TERMINAL-ID.                                              EL1792
00044      12  WS-TERM-PREFIX          PIC XX.                          EL1792
00045      12  FILLER                  PIC XX.                          EL1792
00046  01  REMOTE-PRINTER-CTRL.                                            CL**3
00047      12  CTRL-BUFFER-LENGTH      PIC S9(04) COMP.                    CL**3
00048      12  CTRL-BUFFER-AREA        PIC X(20).                          CL**3
00049      SKIP1                                                        EL1792
00050  01  ERROR-NUMBERS.                                               EL1792
00051      12  ER-0275                 PIC X(4)    VALUE '0275'.        EL1792
00052      12  ER-0424                 PIC X(4)    VALUE '0424'.        EL1792
00053      12  ER-8332                 PIC X(4)    VALUE '8332'.           CL**7
00054      12  ER-8333                 PIC X(4)    VALUE '8333'.           CL**7
00055      SKIP1                                                        EL1792
00056  01  COMP-LENGTHS.                                                EL1792
00057      12  COMP-132                PIC S9(4)   COMP VALUE +132.     EL1792
00058                                  COPY ELCDMD34.                      CL**7
00059      EJECT                                                        EL1792
00060                                  COPY ELCLOGOF.                      CL**2
00061      EJECT                                                        EL1792
00062                                  COPY ELCINTF.                       CL**2
00063      12  EL1792-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.         EL1792
00064          16  PI-PRINT-REPORT     PIC X(5).                        EL1792
00065          16  PI-START-COUNT      PIC S9(8)  COMP.                 EL1792
00066          16  PI-TOTAL-COUNT      PIC S9(8)  COMP.                 EL1792
00067          16  PI-START-PAGE-NO    PIC S9(4).                       EL1792
00068          16  PI-ACCUM-PAGE-NO    PIC S9(4).                       EL1792
00069          16  PI-END-PAGE-NO      PIC S9(4).                       EL1792
00070 *        16  PI-FORMS-PRINTER-ID PIC X(4).                        EL1792
00070          16  PI-PRINT-ID         PIC X(4).                        EL1792
00071          16  FILLER              PIC X(611).                         CL**6
00072      EJECT                                                        EL1792
00073                                  COPY ELCEMIB.                       CL**2
00074      EJECT                                                        EL1792
00075                                  COPY ELPRTCVD.                      CL**2
00076      EJECT                                                        EL1792
00077  LINKAGE SECTION.                                                 EL1792
00078  01  DFHCOMMAREA                 PIC X(1024).                     EL1792
00079                                                                      CL**5
00080                                  COPY ELCREPT.                       CL**2
00081      EJECT                                                        EL1792
00082  PROCEDURE DIVISION.                                              EL1792
00083                                                                   EL1792
00084      MOVE COMP-132               TO WS-LINE-LEN.                  EL1792
00085      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**7
CIDMOD*    MOVE 'N'                    TO CSO-PRINT-STARTED-SW.              000
00086                                                                   EL1792
00087  0010-RETREIVE-REQUEST.                                           EL1792
00088                                                                   EL1792
00089      EXEC CICS HANDLE CONDITION                                   EL1792
00090          NOTOPEN  (0100-FILE-NOT-OPEN)                            EL1792
00091          ENDDATA  (9100-RETURN-CICS)                              EL1792
00092          NOTFND   (0200-COMM-AREA-NOT-FOUND)                      EL1792
00093          ERROR    (9990-ABEND)                                    EL1792
00094      END-EXEC.                                                    EL1792
00095                                                                   EL1792
00096      EXEC CICS RETRIEVE                                           EL1792
00097          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL1792
00098          LENGTH  (PI-COMM-LENGTH)                                 EL1792
00099      END-EXEC.                                                    EL1792
00100                                                                      CL**7
CIDMOD     MOVE PI-PRINT-REPORT TO THIS-PGM.                               CL**7
CIDMOD                                                                     CL**7
00101 * DLO034 OPEN WHEN DMD OR CID                                        CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00103          IF DL34-PROCESS-TYPE = SPACES                               CL**8
00104              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**7
00105              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**7
00106              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**7
00107              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**7
00108              MOVE SPACES             TO DL34-PRINT-LINE              CL**7
00109              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**7
00110              EXEC CICS LINK                                          CL**7
00111                  PROGRAM    ('DLO034')                               CL**7
00112                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**7
00113                  LENGTH     (DLO034-REC-LENGTH)                      CL**7
00114              END-EXEC                                                CL**7
00115              IF DL34-RETURN-CODE NOT = 'OK'                          CL**7
00116                  MOVE '8332'         TO EMI-ERROR                    CL**7
00117                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**8
00118                  EXEC CICS RETURN                                    CL**7
00119                  END-EXEC.                                           CL**7
00120                                                                   EL1792
CIDMOD*    MOVE 'F' TO DRS-SW.                                               000
CIDMOD*    MOVE PI-PRINT-ID TO CSO-PRINT-ID.                                 000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
CIDMOD*    MOVE ' ' TO DRS-SW.                                               000
CIDMOD*
00121      MOVE SPACE                  TO WS-PROG-END.                  EL1792
00122      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL1792
00123      MOVE LIT-1                  TO REPT-RECORD-TYPE.             EL1792
00124      MOVE PI-PRINT-REPORT        TO REPT-REPORT-ID.               EL1792
00125      MOVE PI-START-COUNT         TO REPT-LINE-NUMBER.             EL1792
00126                                                                   EL1792
00127  0020-START-BROWSE.                                               EL1792
00128                                                                   EL1792
00129      EXEC CICS STARTBR                                            EL1792
00130          DATASET  ('ELREPT')                                      EL1792
00131          RIDFLD   (REPT-KEY)                                      EL1792
00132      END-EXEC.                                                    EL1792
00133                                                                   EL1792
00143      PERFORM 1000-PRINT-REPORT THRU 1000-PRINT-REPORT-EXIT.       EL1792
00144                                                                      CL**3
00154      EXEC CICS ENDBR                                              EL1792
00155          DATASET ('ELREPT')                                       EL1792
00156      END-EXEC.                                                    EL1792
00157                                                                   EL1792
CIDMOD     GO TO 0010-RETREIVE-REQUEST.                                 EL1792
CIDMOD*    GO TO 9100-RETURN-CICS.                                           000
00159      EJECT                                                        EL1792
00160  0100-FILE-NOT-OPEN.                                              EL1792
00161      MOVE ER-0275                TO EMI-ERROR.                    EL1792
00162      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
00163      MOVE EMI-MESSAGE-AREA (1)   TO WS-PRINT-AREA.                EL1792
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1792
00165      MOVE LIT-X                  TO WS-PROG-END.                  EL1792
CIDMOD*                                                                      000
CIDMOD*    MOVE 'L' TO DRS-SW.                                               000
CIDMOD*                                                                      000
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1792
00167      GO TO 9100-RETURN-CICS.                                      EL1792
00168      EJECT                                                        EL1792
00169  0200-COMM-AREA-NOT-FOUND.                                        EL1792
00170      MOVE ER-0424                TO EMI-ERROR.                    EL1792
00171      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
00172      MOVE EMI-MESSAGE-AREA (1)   TO WS-PRINT-AREA.                EL1792
00173      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1792
00174      MOVE LIT-X                  TO WS-PROG-END.                  EL1792
CIDMOD*                                                                      000
CIDMOD*    MOVE 'L' TO DRS-SW.                                               000
CIDMOD*                                                                      000
00175      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1792
00176      GO TO 9100-RETURN-CICS.                                      EL1792
00177      EJECT                                                        EL1792
00178  1000-PRINT-REPORT.                                               EL1792
00179                                                                   EL1792
00180      EXEC CICS HANDLE CONDITION                                   EL1792
00181          ENDFILE (1000-END-OF-FILE)                               EL1792
00182      END-EXEC.                                                    EL1792
00183                                                                   EL1792
00184      EXEC CICS READNEXT                                           EL1792
00185          SET      (ADDRESS OF REPORT-SAVE-FILE)                      CL**5
00186          DATASET  ('ELREPT')                                      EL1792
00187          RIDFLD   (REPT-KEY)                                      EL1792
00188      END-EXEC.                                                    EL1792
00189                                                                   EL1792
00190      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD  OR                       CL**8
00191         RF-RECORD-TYPE NOT = LIT-1          OR                       CL**8
00192         RF-REPORT-ID   NOT = PI-PRINT-REPORT                         CL**8
00193          GO TO 1000-END-OF-FILE.                                  EL1792
00194                                                                   EL1792
00195      IF PI-END-PAGE-NO GREATER ZERO                               EL1792
00196          IF RF-CTL-CHAR-133 = '1'                                 EL1792
00197              ADD +1 TO PI-ACCUM-PAGE-NO                           EL1792
00198              IF PI-ACCUM-PAGE-NO GREATER PI-END-PAGE-NO           EL1792
00199                  GO TO 1000-END-OF-FILE.                          EL1792
00200                                                                   EL1792
00201      MOVE RF-REPORT-LINE-133     TO WS-PRINT-AREA.                EL1792
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1792
00203      GO TO 1000-PRINT-REPORT.                                     EL1792
00204                                                                   EL1792
00205  1000-END-OF-FILE.                                                EL1792
00206      MOVE LIT-X                  TO WS-PROG-END.                  EL1792
00207                                                                      CL**8
CIDMOD     IF PI-COMPANY-ID NOT = 'DMD' OR 'CID'                           CL**8
00209          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                        CL**8
00210                                                                      CL**6
00211  1000-PRINT-REPORT-EXIT.                                          EL1792
00212      EXIT.                                                        EL1792
00213      EJECT                                                        EL1792
uktdel*8000-STANDARD-PRINT-ROUTINE.    COPY ELPRTCVP.                   EL1792
uktins 8000-STANDARD-PRINT-ROUTINE.
uktins     COPY ELPRTCVP.
00215                                                                      CL**7
00216      EJECT                                                        EL1792
00217  9100-RETURN-CICS.                                                EL1792
00218 ******   WHEN PRINTING TO A 3275 DIAL-UP TERMINAL                 EL1792
00219 ******   THE TRANSID AND COMMAREA MUST BE RESET                   EL1792
00220 ******   UPON COMPLETION OF THE TASK.                             EL1792
CIDMOD*                                                                      000
CIDMOD*    MOVE 'L' TO DRS-SW.                                               000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
CIDMOD*                                                                      000
00221      MOVE EIBTRMID               TO WS-TERMINAL-ID.               EL1792
00222                                                                      CL**7
00223 * DLO034 CLOSE                                                       CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00225          MOVE 'C'                TO DL34-PROCESS-TYPE                CL**7
00226          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL**7
00227          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL**7
00228          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL**7
00229          MOVE SPACES             TO DL34-PRINT-LINE                  CL**7
00230                                     DL34-OVERRIDE-PRINTER-ID         CL**7
00231          EXEC CICS LINK                                              CL**7
00232              PROGRAM    ('DLO034')                                   CL**7
00233              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL**7
00234              LENGTH     (DLO034-REC-LENGTH)                          CL**7
00235          END-EXEC                                                    CL**7
00236          IF DL34-RETURN-CODE NOT = 'OK'                              CL**7
00237              MOVE '8333'         TO EMI-ERROR                        CL**7
00238              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**8
00239              EXEC CICS RETURN                                        CL**7
00240              END-EXEC.                                               CL**7
00241                                                                   EL1792
00242      IF WS-TERM-PREFIX = 'DU'                                     EL1792
00243         EXEC CICS RETURN                                          EL1792
00244              TRANSID  ('EX49')                                    EL1792
00245              COMMAREA (PROGRAM-INTERFACE-BLOCK)                   EL1792
00246              LENGTH   (PI-COMM-LENGTH)                            EL1792
00247         END-EXEC                                                     CL**8
00248      ELSE                                                         EL1792
00249         EXEC CICS  RETURN                                         EL1792
00250         END-EXEC.                                                    CL**8
00251                                                                   EL1792
CIDMOD     MOVE ZEROS     TO RETURN-CODE.
00252      GOBACK.                                                      EL1792
00253                                                                   EL1792
00254  9900-ERROR-FORMAT.                                               EL1792
00255                                                                   EL1792
00256      IF EMI-ERRORS-COMPLETE                                       EL1792
00257          GO TO 9900-EXIT.                                            CL**8
00258                                                                   EL1792
00259      EXEC CICS LINK                                               EL1792
00260          PROGRAM   ('EL001')                                      EL1792
00261          COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)                EL1792
00262          LENGTH    (EMI-COMM-LENGTH)                              EL1792
00263      END-EXEC.                                                    EL1792
00264                                                                      CL**8
00265  9900-EXIT.                                                          CL**8
00266      EXIT.                                                        EL1792
00267                                                                   EL1792
00268  9990-ABEND.                                                      EL1792
00269      MOVE DFHEIBLK               TO LOGOFF-FILL.                  EL1792
00270                                                                   EL1792
00271      EXEC CICS LINK                                               EL1792
00272          PROGRAM   ('EL004')                                      EL1792
00273          COMMAREA  (LOGOFF-FILL)                                  EL1792
00274          LENGTH    (66)                                           EL1792
00275      END-EXEC.                                                    EL1792
00276                                                                   EL1792
00277      GO TO 9100-RETURN-CICS.                                      EL1792
