00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL1524
00003  PROGRAM-ID.                 EL1524.                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/13/96 09:53:52.                    CL**2
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL1524
00009 *AUTHOR.           LOGIC,INC.                                        CL**2
00010 *                  DALLAS,TEXAS.                                     CL**2
00011                                                                   EL1524
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   EL1524
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *         CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   EL1524
00024 *REMARKS. TRANSACTION EXL2 - ACTIVITY QUEUE LETTER WRITER.        EL1524
00025 *        THIS PROGRAM IS USED TO PRINT LETTERS TO THE COMPANY     EL1524
00026 *        SPECIFIED PRINTER.  IT IS ACTIVATED THROUGH A START      EL1524
00027 *        COMMAND FROM EL178 AND WILL HAVE THE PROGRAM INTERFACE   EL1524
00028 *        BLOCK PASSED.                                            EL1524
00029 *        THIS PROGRAM WILL READ THE ACTIVITY QUEUE FILE ELACTQ    EL1524
00030 *        SELECTING THOSE RECORDS CORRESPONDING TO THE COMPANY     EL1524
00031 *        BEING PROCESSED AND CALL EL1523 TO CREATE ARCHIVE ENTRIESEL1524
00032 *        FOR EACH ACTIVITY RECORD THAT CONTAINS A VALID FORM NUMBEEL1524
00033                                                                   EL1524
00034                                  EJECT                            EL1524
00035  ENVIRONMENT DIVISION.                                            EL1524
00036  DATA DIVISION.                                                   EL1524
00037  WORKING-STORAGE SECTION.                                         EL1524
00038  77  FILLER  PIC X(32) VALUE '********************************'.  EL1524
00039  77  FILLER  PIC X(32) VALUE '*   EL1524 WORKING STORAGE     *'.  EL1524
00040  77  FILLER  PIC X(32) VALUE '******* VMOD=2.003 *************'.     CL**3
00041                                                                   EL1524
00042  01  W-PROGRAM-WORK-AREA.                                         EL1524
00043      12  THIS-PGM                PIC  X(06) VALUE 'EL1524'.          CL**3
00044      12  FILLER                  PIC  X(17)                       EL1524
00045                                  VALUE 'PROGRAM WORK AREA'.       EL1524
00046      12  W-ACTQ-EXIT-SW          PIC X.                              CL**2
00047          88  W-ACTQ-EXIT-YES                      VALUE 'Y'.         CL**2
00048      12  W-ASKTIME-CTR           PIC S9(04)  COMP.                EL1524
00049      12  W-PGM-ID                PIC S9(04)  COMP VALUE +1524.    EL1524
00050      12  W-INCOMING-LINES        PIC S9(04)  COMP VALUE +56.      EL1524
00051      12  W-LINE-COUNT            PIC S9(04)  COMP VALUE +56.      EL1524
00052                                                                   EL1524
00053      12  W-ACTQ-RCRD-CHANGED     PIC S9(07)  COMP-3 VALUE +0.     EL1524
00054      12  W-ACTQ-RCRD-DELETED     PIC S9(07)  COMP-3 VALUE +0.     EL1524
00055      12  W-ACTQ-RCRD-READ        PIC S9(07)  COMP-3 VALUE +0.     EL1524
00056      12  W-ACTQ-RCRD-USED        PIC S9(07)  COMP-3 VALUE +0.     EL1524
00057      12  W-COMBINED-SPACES       PIC S9(07)  COMP-3 VALUE +0.     EL1524
00058      12  W-FATAL-ERRORS          PIC S9(07)  COMP-3 VALUE +0.     EL1524
00059      12  W-LETTERS-CREATED       PIC S9(07)  COMP-3 VALUE +0.     EL1524
00060      12  W-LETTERS-WITH-ERRORS   PIC S9(07)  COMP-3 VALUE +0.     EL1524
00061      12  W-PAGE                  PIC S9(04)  COMP-3 VALUE +0.     EL1524
00062                                                                   EL1524
00063      12  W-ACTQ-FILE-ID          PIC  X(08)  VALUE 'ELACTQ'.      EL1524
00064      12  W-ACTQ-KEY.                                              EL1524
00065          16  W-ACTQ-COMPANY-CD   PIC  X(01).                      EL1524
00066          16  W-ACTQ-CARRIER      PIC  X(01).                      EL1524
00067          16  W-ACTQ-CLAIM-NO     PIC  X(07).                      EL1524
00068          16  W-ACTQ-CERT-NO      PIC  X(11).                      EL1524
00069                                                                   EL1524
00070      12  W-CNTL-FILE-ID          PIC  X(08)  VALUE 'ELCNTL'.      EL1524
00071      12  W-CNTL-KEY.                                              EL1524
00072          16  W-CNTL-COMPANY-ID   PIC  X(03).                      EL1524
00073          16  W-CNTL-RECORD-TYPE  PIC  X(01).                      EL1524
00074          16  FILLER              PIC  X(04).                      EL1524
00075          16  W-CNTL-SEQ          PIC S9(04)  COMP.                EL1524
00076                                                                   EL1524
00077      12  W-COMPANY.                                               EL1524
00078          16  W-CO-CHAR OCCURS 30 TIMES                            EL1524
00079                        INDEXED BY W-CO-NDX                        EL1524
00080                                  PIC  X(01).                      EL1524
00081                                                                   EL1524
00082      12  W-FLAGS.                                                 EL1524
00083          88  W-NO-PENDING-ACTIVITY         VALUE SPACES.          EL1524
00084          16  FILLER              PIC  X(02).                      EL1524
00085          16  W-PENDING-LETTER-FLAG                                EL1524
00086                                  PIC  X(01).                      EL1524
00087                                                                   EL1524
00088      12  W-MESSAGES.                                              EL1524
00089          16  W-REPORT-TITLE.                                      EL1524
00090              20  FILLER          PIC  X(01)                       EL1524
00091                  VALUE '1'.                                       EL1524
00092              20  W-DATE          PIC  X(08)                       EL1524
00093                  VALUE 'XX/XX/XX'.                                EL1524
00094              20  FILLER          PIC  X(14)                       EL1524
00095                  VALUE SPACES.                                    EL1524
00096              20  FILLER          PIC  X(35)                       EL1524
00097                  VALUE 'ACTIVITY QUEUE LETTERS CREATED LIST'.     EL1524
00098              20  FILLER          PIC  X(11)                       EL1524
00099                  VALUE SPACES.                                    EL1524
00100              20  FILLER          PIC  X(09)                       EL1524
00101                  VALUE 'EL1524'.                                  EL1524
00102          16  W-COMPANY-TITLE.                                     EL1524
00103              20  FILLER          PIC  X(26)                       EL1524
00104                  VALUE SPACES.                                    EL1524
00105              20  W-CT-COMPANY.                                    EL1524
00106                  24  W-CT-CHAR OCCURS 30 TIMES                    EL1524
00107                                INDEXED BY W-CT-NDX                EL1524
00108                                  PIC  X(01).                      EL1524
00109              20  FILLER          PIC  X(14)  VALUE SPACES.        EL1524
00110              20  FILLER          PIC  X(05)                       EL1524
00111                  VALUE 'PAGE '.                                   EL1524
00112              20  W-CT-PAGE       PIC  ZZZ9.                       EL1524
00113          16  W-FATAL-ERROR-MSG.                                   EL1524
00114              20  FILLER          PIC  X(12)                       EL1524
00115                  VALUE 'FATAL ERROR '.                            EL1524
00116              20  W-FATAL-ERROR   PIC  9(04).                      EL1524
00117              20  FILLER          PIC  X(18)                       EL1524
00118                                  VALUE ' DETECTED FOR CLM '.      EL1524
00119              20  W-CLAIM-NO-F    PIC  X(07) VALUE 'XXXXXXX'.      EL1524
00120              20  FILLER          PIC  X(07) VALUE ', CERT '.      EL1524
00121              20  W-CERT-NO-F     PIC  X(11) VALUE 'XXXXXXXXXXX'.  EL1524
00122              20  FILLER          PIC  X(20)                       EL1524
00123                  VALUE ', LETTER NOT CREATED'.                    EL1524
00124          16  W-GENERAL-ERROR.                                     EL1524
00125              20  FILLER          PIC  X(06) VALUE 'CLAIM '.       EL1524
00126              20  W-CLAIM-NO      PIC  X(07) VALUE 'XXXXXXX'.      EL1524
00127              20  FILLER          PIC  X(10) VALUE ' FOR CERT '.   EL1524
00128              20  W-CERT-NO       PIC  X(11) VALUE 'XXXXXXXXXXX'.  EL1524
00129              20  FILLER          PIC  X(11) VALUE ' HAD ERROR '.  EL1524
00130              20  W-ERROR         PIC  9(04) VALUE 9999.           EL1524
00131          16  W-LETTER-SENT.                                       EL1524
00132              20  FILLER          PIC  X(26)                       EL1524
00133                  VALUE 'LETTER ARCHIVED FOR CLAIM '.              EL1524
00134              20  W-CLAIM-X       PIC  X(07) VALUE 'XXXXXXX'.      EL1524
00135              20  FILLER          PIC  X(10) VALUE ' AND CERT '.   EL1524
00136              20  W-CERT-X        PIC  X(11) VALUE 'XXXXXXXXXXX'.  EL1524
00137              20  FILLER          PIC  X(10)                       EL1524
00138                                  VALUE ', ARCHIVE '.              EL1524
00139              20  W-ARCHIVE       PIC  9(08) VALUE 99999999.       EL1524
00140          16  W-SEVERE-ERROR.                                      EL1524
00141              20  FILLER          PIC  X(10)                       EL1524
00142                  VALUE ' ******** '.                              EL1524
00143              20  FILLER          PIC  X(26)                       EL1524
00144                  VALUE 'A SEVERE ERROR DETECTED.'.                EL1524
00145              20  FILLER          PIC  X(26)                       EL1524
00146                  VALUE 'ALL PROCESSING STOPPED.'.                 EL1524
00147          16  W-TOTAL1.                                            EL1524
00148              20  FILLER          PIC  X(35)                       EL1524
00149                  VALUE 'ACTQ RCRDS READ - '.                      EL1524
00150              20  W-TOTAL-AMT1    PIC  ZZZ,ZZ9.                    EL1524
00151          16  W-TOTAL2.                                            EL1524
00152              20  FILLER          PIC  X(35)                       EL1524
00153                  VALUE 'ACTQ RCRDS QUALIFIED - '.                 EL1524
00154              20  W-TOTAL-AMT2    PIC  ZZZ,ZZ9.                    EL1524
00155          16  W-TOTAL3.                                            EL1524
00156              20  FILLER          PIC  X(35)                       EL1524
00157                  VALUE 'RCRDS WITH FATAL ERRORS DETECTED - '.     EL1524
00158              20  W-TOTAL-AMT3    PIC  ZZZ,ZZ9.                    EL1524
00159          16  W-TOTAL4.                                            EL1524
00160              20  FILLER          PIC  X(35)                       EL1524
00161                  VALUE 'LETTERS ARCHIVED - '.                     EL1524
00162              20  W-TOTAL-AMT4    PIC  ZZZ,ZZ9.                    EL1524
00163          16  W-TOTAL5.                                            EL1524
00164              20  FILLER          PIC  X(35)                       EL1524
00165                  VALUE 'ACTQ RCRDS DELETED - '.                   EL1524
00166              20  W-TOTAL-AMT5    PIC  ZZZ,ZZ9.                    EL1524
00167          16  W-TOTAL6.                                            EL1524
00168              20  FILLER          PIC  X(35)                       EL1524
00169                  VALUE 'ACTQ RCRDS CHANGED - '.                   EL1524
00170              20  W-TOTAL-AMT6    PIC  ZZZ,ZZ9.                    EL1524
00171          16  W-TOTAL7.                                            EL1524
00172              20  FILLER          PIC  X(35)                       EL1524
00173                  VALUE 'LETTERS WITH ERRORS - '.                  EL1524
00174              20  W-TOTAL-AMT7    PIC  ZZZ,ZZ9.                    EL1524
00175                                                                   EL1524
00176      12  W-MESSAGE-LINE.                                          EL1524
00177          16  W-ML-CC             PIC  X(01).                      EL1524
00178          16  W-ML-DATA           PIC  X(79).                      EL1524
00179                                                                   EL1524
00180      12  W-NEXT-TRAN             PIC  X(04).                      EL1524
00181                                                                   EL1524
00182      12  W-TERMINAL-ID.                                           EL1524
00183          18  W-TERM-PREFIX       PIC  X(02).                      EL1524
00184          18  FILLER              PIC  X(02).                      EL1524
00185                                                                   EL1524
00186  01  W-PROGRAM-CONSTANTS.                                         EL1524
00187      12  FILLER                  PIC  X(17)                       EL1524
00188                                  VALUE 'PROGRAM CONSTANTS'.       EL1524
00189      12  W-PGM-EL178             PIC  X(08)  VALUE 'EL178'.       EL1524
00190                                  COPY ELCDMD34.                      CL**3
00191                                  EJECT                            EL1524
00192                                  COPY ELCINTF.                    EL1524
CIDMOD     12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                         EL1
CIDMOD         16  PI-PRINT-DATE       PIC X(08).                            EL1
CIDMOD         16  PI-PRINT-DATE-BIN   PIC X(02).                            EL1
CIDMOD         16  PI-PRINT-ID         PIC X(04).                            EL1
CIDMOD         16  PI-STARTING-ARCH-NO PIC S9(08) COMP.                      EL1
CIDMOD         16  PI-PRINT-BY-CARR    PIC X.                                EL1
CIDMOD             88  PRINT-BY-CARR              VALUE 'Y'.                 EL1
CIDMOD         16  PI-PRINT-CARRIER    PIC X.                                EL1
CIDMOD         16  PI-LETTER-TYPE      PIC X.                                EL1
CIDMOD         16  PI-PRINT-REPORT     PIC X(05).                            EL1
00193                                  EJECT                            EL1524
00194                                  COPY ELCLNKLT.                   EL1524
00195                                  EJECT                            EL1524
00196                                  COPY ELCDATE.                    EL1524
00197                                  EJECT                            EL1524
00198                                  COPY ELCEMIB.                    EL1524
00199                                                                   EL1524
00200  01  EMI-SAVE-AREA               PIC X(400).                      EL1524
00201                                  EJECT                            EL1524
00202                                  COPY ELPRTCVD.                   EL1524
00203                                  EJECT                            EL1524
00204  LINKAGE SECTION.                                                 EL1524
00205  01  DFHCOMMAREA                 PIC X(1024).                     EL1524
00206                                                                   EL1524
00207      COPY ELCACTQ.                                                EL1524
00208                                  EJECT                            EL1524
00209      COPY ELCCNTL.                                                EL1524
00210                                  EJECT                            EL1524
00211  PROCEDURE DIVISION.                                              EL1524
00212                                                                   EL1524
00213      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL1524
00214      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL1524
00215      MOVE ERROR-MESSAGE-INTERFACE-BLOCK                           EL1524
00216                                  TO EMI-SAVE-AREA.                EL1524
00217                                                                   EL1524
00218      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**3
00219                                                                      CL**3
00220      EXEC CICS HANDLE CONDITION                                   EL1524
00221           ENDDATA (0200-END-DATA)                                 EL1524
00222           NOTFND  (0300-NOT-FOUND)                                EL1524
00223           ERROR   (8020-ERROR-ABEND)                              EL1524
00224           INVREQ  (8020-ERROR-ABEND)                              EL1524
00225      END-EXEC.                                                    EL1524
00226                                                                   EL1524
00227  0100-RETRIEVE-LOOP.                                              EL1524
00228                                                                   EL1524
00229      EXEC CICS RETRIEVE                                           EL1524
00230           INTO   (PROGRAM-INTERFACE-BLOCK)                        EL1524
00231           LENGTH (PI-COMM-LENGTH)                                 EL1524
00232      END-EXEC.                                                    EL1524
00233                                                                      CL**3
00234 * DLO034 OPEN WHEN DMD OR CID                                        CL**3
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**3
00236          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**3
00237              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**3
00238              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**3
00239              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**3
00240              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**3
00241              MOVE SPACES             TO DL34-PRINT-LINE              CL**3
00242              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**3
00243              EXEC CICS LINK                                          CL**3
00244                  PROGRAM    ('DLO034')                               CL**3
00245                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**3
00246                  LENGTH     (DLO034-REC-LENGTH)                      CL**3
00247              END-EXEC                                                CL**3
00248              IF DL34-RETURN-CODE NOT = 'OK'                          CL**3
00249                  MOVE '8332'             TO EMI-ERROR                CL**3
00250                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**3
00251                  MOVE 'EX48'  TO W-NEXT-TRAN                         CL**3
00252                  EXEC CICS RETURN                                    CL**3
00253                       TRANSID  (W-NEXT-TRAN)                         CL**3
00254                       COMMAREA (PROGRAM-INTERFACE-BLOCK)             CL**3
00255                       LENGTH   (PI-COMM-LENGTH)                      CL**3
00256                  END-EXEC.                                           CL**3
00257                                                                   EL1524
00258      IF  PI-CALLING-PROGRAM = W-PGM-EL178                         EL1524
CIDMOD*        MOVE 'F'                TO DRS-SW                             EL1
CIDMOD*        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                           EL1
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00259          MOVE LOW-VALUES         TO W-1523-LINKDATA               EL1524
00260          PERFORM 5000-GET-COMPANY-NAME THRU 5000-EXIT             EL1524
00261          PERFORM 1000-PROCESS-ACTQ-FILE THRU 1000-EXIT            EL1524
00262          PERFORM 4000-PRINT-TOTALS THRU 4000-EXIT                 EL1524
00263          MOVE 'X'                TO WS-PROG-END                   EL1524
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00264          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL1524
00265          GO TO 0100-RETRIEVE-LOOP.                                EL1524
00266                                                                   EL1524
00267      GO TO 0100-RETRIEVE-LOOP.                                    EL1524
00268                                  EJECT                            EL1524
00269  0200-END-DATA.                                                   EL1524
CIDMOD*    MOVE 'L'                TO DRS-SW.                                EL1
CIDMOD*      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            EL1
CIDMOD                                                                       EL1
00270                                                                   EL1524
00271      MOVE 'EX48'                 TO W-NEXT-TRAN.                  EL1524
00272                                                                   EL1524
00273      MOVE EIBTRMID               TO W-TERMINAL-ID.                EL1524
00274                                                                   EL1524
00275 * DLO034 CLOSE                                                       CL**3
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**3
00277          MOVE 'C'                TO DL34-PROCESS-TYPE                CL**3
00278          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL**3
00279          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL**3
00280          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL**3
00281          MOVE SPACES             TO DL34-PRINT-LINE                  CL**3
00282                                     DL34-OVERRIDE-PRINTER-ID         CL**3
00283          EXEC CICS LINK                                              CL**3
00284              PROGRAM    ('DLO034')                                   CL**3
00285              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL**3
00286              LENGTH     (DLO034-REC-LENGTH)                          CL**3
00287          END-EXEC                                                    CL**3
00288          IF DL34-RETURN-CODE NOT = 'OK'                              CL**3
00289              MOVE '8333'             TO EMI-ERROR                    CL**3
00290              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**3
00291              EXEC CICS RETURN                                        CL**3
00292                   TRANSID  (W-NEXT-TRAN)                             CL**3
00293                   COMMAREA (PROGRAM-INTERFACE-BLOCK)                 CL**3
00294                   LENGTH   (PI-COMM-LENGTH)                          CL**3
00295              END-EXEC.                                               CL**3
00296                                                                      CL**3
00297      IF  W-TERM-PREFIX = 'DU'                                     EL1524
00298          EXEC CICS RETURN                                         EL1524
00299               TRANSID  (W-NEXT-TRAN)                              EL1524
00300               COMMAREA (PROGRAM-INTERFACE-BLOCK)                  EL1524
00301               LENGTH   (PI-COMM-LENGTH)                           EL1524
00302          END-EXEC                                                 EL1524
00303      ELSE                                                         EL1524
00304          EXEC CICS RETURN                                         EL1524
00305          END-EXEC.                                                EL1524
00306                                                                   EL1524
00307  0300-NOT-FOUND.                                                  EL1524
00308                                                                   EL1524
00309      IF  PI-COMPANY-ID NOT = 'MON'                                EL1524
00310          MOVE 'NO COMMUNICATION AREA FOUND' TO W-MESSAGE-LINE     EL1524
00311          PERFORM 0400-SEND-TEXT.                                  EL1524
00312                                                                   EL1524
00313      GO TO 0200-END-DATA.                                         EL1524
00314                                                                   EL1524
00315  0400-SEND-TEXT.                                                  EL1524
00316                                                                   EL1524
00317      EXEC CICS SEND TEXT                                          EL1524
00318           FROM   (W-MESSAGE-LINE)                                 EL1524
00319           LENGTH (70)                                             EL1524
00320      END-EXEC.                                                    EL1524
00321                                  EJECT                            EL1524
00322  1000-PROCESS-ACTQ-FILE.                                          EL1524
00323                                                                   EL1524
00324      MOVE +80                    TO WS-LINE-LEN.                  EL1524
00325      MOVE '1'                    TO W-ML-CC.                      EL1524
00326                                                                   EL1524
00327      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1524
00328      MOVE '5'                    TO DC-OPTION-CODE.               EL1524
00329                                                                   EL1524
00330      EXEC CICS LINK                                               EL1524
00331          PROGRAM   ('ELDATCV')                                    EL1524
00332          COMMAREA  (DATE-CONVERSION-DATA)                         EL1524
00333          LENGTH    (DC-COMM-LENGTH)                               EL1524
00334      END-EXEC.                                                    EL1524
00335                                                                   EL1524
00336      MOVE DC-GREG-DATE-1-EDIT    TO W-DATE.                       EL1524
00337      MOVE +56                    TO W-LINE-COUNT.                 EL1524
00338                                                                   EL1524
00339      MOVE LOW-VALUES             TO W-ACTQ-KEY.                   EL1524
00340                                                                   EL1524
00341      MOVE PI-COMPANY-CD          TO W-ACTQ-COMPANY-CD.            EL1524
00342                                                                   EL1524
00343  1000-READ-ACTQ.                                                  EL1524
00344                                                                   EL1524
00345      EXEC CICS HANDLE CONDITION                                      CL**2
00346           NOTOPEN (8000-ACTQ-NOT-OPEN)                               CL**2
00347           NOTFND  (8010-ACTQ-NOT-FOUND)                              CL**2
00348           ENDFILE (1000-EXIT)                                        CL**2
00349      END-EXEC.                                                       CL**2
00350                                                                      CL**2
00351      PERFORM 3400-STARTBR-ACTQ THRU 3400-EXIT.                    EL1524
00352                                                                   EL1524
00353  1000-READ-NEXT.                                                  EL1524
00354                                                                   EL1524
00355      PERFORM 3500-READNEXT-ACTQ THRU 3500-EXIT.                   EL1524
00356                                                                   EL1524
00357      MOVE EMI-SAVE-AREA          TO ERROR-MESSAGE-INTERFACE-BLOCK.EL1524
00358                                                                   EL1524
00359      IF  AQ-COMPANY-CD LESS THAN PI-COMPANY-CD                    EL1524
00360          GO TO 1000-READ-ACTQ                                     EL1524
00361                                                                   EL1524
00362      ELSE                                                         EL1524
00363          IF  AQ-COMPANY-CD GREATER THAN PI-COMPANY-CD             EL1524
00364              GO TO 1000-ENDBR.                                    EL1524
00365                                                                   EL1524
00366      ADD +1                      TO W-ACTQ-RCRD-READ.             EL1524
00367                                                                   EL1524
00368      IF  NOT PENDING-LETTERS                                      EL1524
00369              OR                                                   EL1524
00370          AQ-AUTO-LETTER NOT GREATER THAN SPACES                   EL1524
00371          GO TO 1000-READ-NEXT.                                    EL1524
00372                                                                   EL1524
00373      ADD +1                      TO W-ACTQ-RCRD-USED.             EL1524
00374                                                                   EL1524
00375      MOVE LOW-VALUES             TO W-1523-WORK-AREA.             EL1524
00376      MOVE PROGRAM-INTERFACE-BLOCK                                 EL1524
00377                                  TO W-1523-COMMON-PI-DATA.        EL1524
00378      MOVE AQ-CARRIER             TO W-1523-CARRIER.               EL1524
00379      MOVE AQ-CLAIM-NO            TO W-CLAIM-X                     EL1524
00380                                     W-1523-CLAIM-NO.              EL1524
00381      MOVE AQ-CERT-NO             TO W-CERT-X                      EL1524
00382                                     W-1523-CERT-NO.               EL1524
00383      MOVE AQ-AUTO-LETTER         TO W-1523-FORM-NUMBER.           EL1524
00384      MOVE AQ-RESEND-DATE         TO W-1523-RESEND-DATE.           EL1524
00385      MOVE AQ-FOLLOWUP-DATE       TO W-1523-FOLLOW-UP-DATE.        EL1524
00386      MOVE AQ-PENDING-ACTIVITY-FLAGS                               EL1524
00387                                  TO W-FLAGS.                      EL1524
00388                                                                   EL1524
00389      EXEC CICS LINK                                               EL1524
00390           PROGRAM   ('EL1523')                                    EL1524
00391           COMMAREA  (W-1523-LINKDATA)                             EL1524
00392           LENGTH    (W-1523-COMM-LENGTH)                          EL1524
00393      END-EXEC.                                                    EL1524
00394                                                                   EL1524
00395      IF  W-1523-FATAL-ERROR                                       EL1524
00396          ADD +1                  TO W-FATAL-ERRORS                EL1524
00397          MOVE W-1523-ERROR-CODE  TO W-FATAL-ERROR                 EL1524
00398          MOVE W-1523-CLAIM-NO    TO W-CLAIM-NO-F                  EL1524
00399          MOVE W-1523-CERT-NO     TO W-CERT-NO-F                   EL1524
00400          MOVE W-FATAL-ERROR-MSG  TO W-ML-DATA                     EL1524
00401          MOVE '0'                TO W-ML-CC                       EL1524
00402          MOVE +3                 TO W-INCOMING-LINES              EL1524
00403          PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT                  EL1524
00404          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                 EL1524
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00405          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL1524
00406          MOVE W-1523-ERROR-CODE  TO EMI-ERROR                     EL1524
00407          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1524
00408          MOVE EMI-ERROR-TEXT (1) TO W-ML-DATA                     EL1524
00409          MOVE SPACES             TO W-ML-CC                       EL1524
00410          MOVE W-MESSAGE-LINE     TO WS-PRINT-AREA                 EL1524
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00411          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL1524
00412                                                                   EL1524
00413          IF  W-1523-STOP-ERRORS                                   EL1524
00414              MOVE W-SEVERE-ERROR TO W-ML-DATA                     EL1524
00415              MOVE SPACES         TO W-ML-CC                       EL1524
00416              MOVE W-MESSAGE-LINE TO WS-PRINT-AREA                 EL1524
CIDMOD*            MOVE ' '                TO DRS-SW                         EL1
00417              PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                  EL1524
00418              GO TO 1000-EXIT                                      EL1524
00419                                                                   EL1524
00420          ELSE                                                     EL1524
00421              PERFORM 3400-STARTBR-ACTQ THRU 3400-EXIT             EL1524
00422              PERFORM 3500-READNEXT-ACTQ THRU 3500-EXIT            EL1524
00423              GO TO 1000-READ-NEXT                                 EL1524
00424                                                                   EL1524
00425      ELSE                                                         EL1524
00426          IF  W-1523-ERROR-CODE EQUAL 0191                         EL1524
00427              ADD +1              TO W-LETTERS-WITH-ERRORS         EL1524
00428              MOVE W-1523-CLAIM-NO                                 EL1524
00429                                  TO W-CLAIM-NO                    EL1524
00430              MOVE W-1523-CERT-NO TO W-CERT-NO                     EL1524
00431              MOVE W-1523-ERROR-CODE                               EL1524
00432                                  TO W-ERROR                       EL1524
00433              MOVE W-GENERAL-ERROR                                 EL1524
00434                                  TO W-ML-DATA                     EL1524
00435              MOVE '0'            TO W-ML-CC                       EL1524
00436              MOVE +2             TO W-INCOMING-LINES              EL1524
00437              PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT              EL1524
00438              MOVE W-MESSAGE-LINE TO WS-PRINT-AREA                 EL1524
CIDMOD*            MOVE ' '                TO DRS-SW
00439              PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                 EL1524
00440                                                                   EL1524
00441      ADD +1                      TO W-LETTERS-CREATED.            EL1524
00442      MOVE SPACE                  TO W-PENDING-LETTER-FLAG.        EL1524
00443                                                                   EL1524
00444      MOVE W-1523-ARCHIVE-NUMBER  TO W-ARCHIVE.                    EL1524
00445      MOVE W-LETTER-SENT          TO W-ML-DATA.                    EL1524
00446                                                                   EL1524
00447      IF  W-1523-ERROR-CODE EQUAL 0191                             EL1524
00448          MOVE SPACES             TO W-ML-CC                       EL1524
00449          MOVE +1                 TO W-INCOMING-LINES              EL1524
00450          PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT                  EL1524
00451                                                                   EL1524
00452      ELSE                                                         EL1524
00453          MOVE '0'                TO W-ML-CC                       EL1524
00454          MOVE +2                 TO W-INCOMING-LINES              EL1524
00455          PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                 EL1524
00456                                                                   EL1524
00457      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00458      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00459                                                                   EL1524
00460      IF  W-NO-PENDING-ACTIVITY                                    EL1524
00461          PERFORM 3000-DELETE-ACTQ-RCRD THRU 3000-EXIT             EL1524
00462          GO TO 1000-READ-ACTQ                                     EL1524
00463                                                                   EL1524
00464      ELSE                                                         EL1524
00465          PERFORM 2000-UPDATE-ACTQ-RCRD THRU 2000-EXIT             EL1524
00466          PERFORM 3400-STARTBR-ACTQ THRU 3400-EXIT                 EL1524
00467          PERFORM 3500-READNEXT-ACTQ THRU 3500-EXIT                EL1524
00468          GO TO 1000-READ-NEXT.                                    EL1524
00469                                                                   EL1524
00470  1000-ENDBR.                                                      EL1524
00471                                                                   EL1524
00472      EXEC CICS ENDBR                                              EL1524
00473           DATASET (W-ACTQ-FILE-ID)                                EL1524
00474      END-EXEC.                                                    EL1524
00475                                                                   EL1524
00476  1000-EXIT.                                                       EL1524
00477      EXIT.                                                        EL1524
00478                                  EJECT                            EL1524
00479  2000-UPDATE-ACTQ-RCRD.                                           EL1524
00480                                                                   EL1524
00481      EXEC CICS READ                                               EL1524
00482          SET      (ADDRESS OF ACTIVITY-QUE)                          CL**2
00483          DATASET  (W-ACTQ-FILE-ID)                                EL1524
00484          RIDFLD   (W-ACTQ-KEY)                                    EL1524
00485          UPDATE                                                   EL1524
00486      END-EXEC.                                                    EL1524
00487                                                                   EL1524
00488      MOVE W-PGM-ID               TO AQ-LAST-UPDATED-BY.           EL1524
00489      MOVE SPACE                  TO AQ-PENDING-LETTER-FLAG        EL1524
00490                                     AQ-AUTO-LETTER.               EL1524
00491                                                                   EL1524
00492      EXEC CICS REWRITE                                            EL1524
00493          DATASET  (W-ACTQ-FILE-ID)                                EL1524
00494          FROM     (ACTIVITY-QUE)                                  EL1524
00495      END-EXEC.                                                    EL1524
00496                                                                   EL1524
00497      ADD +1                      TO W-ACTQ-RCRD-CHANGED.          EL1524
00498                                                                   EL1524
00499  2000-EXIT.                                                       EL1524
00500      EXIT.                                                        EL1524
00501                                  EJECT                            EL1524
00502  3000-DELETE-ACTQ-RCRD.                                           EL1524
00503                                                                   EL1524
00504      MOVE 'N' TO W-ACTQ-EXIT-SW.                                     CL**2
00505      EXEC CICS HANDLE CONDITION                                   EL1524
00506          ENDFILE (3000-EXIT)                                         CL**2
00507          NOTFND  (3000-EXIT)                                         CL**2
00508      END-EXEC.                                                    EL1524
00509                                                                   EL1524
00510      EXEC CICS READ                                               EL1524
00511          SET      (ADDRESS OF ACTIVITY-QUE)                          CL**2
00512          DATASET  (W-ACTQ-FILE-ID)                                EL1524
00513          RIDFLD   (W-ACTQ-KEY)                                    EL1524
00514          UPDATE                                                   EL1524
00515      END-EXEC.                                                    EL1524
00516                                                                   EL1524
00517      EXEC CICS DELETE                                             EL1524
00518          DATASET  (W-ACTQ-FILE-ID)                                EL1524
00519      END-EXEC.                                                    EL1524
00520                                                                   EL1524
00521      ADD +1                      TO W-ACTQ-RCRD-DELETED.          EL1524
00522                                                                   EL1524
00523  3000-EXIT.                                                       EL1524
00524      EXIT.                                                        EL1524
00525                                  EJECT                            EL1524
00526  3400-STARTBR-ACTQ.                                               EL1524
00527                                                                   EL1524
00528      EXEC CICS STARTBR                                            EL1524
00529          DATASET (W-ACTQ-FILE-ID)                                 EL1524
00530          RIDFLD  (W-ACTQ-KEY)                                     EL1524
00531          GTEQ                                                     EL1524
00532      END-EXEC.                                                    EL1524
00533                                                                   EL1524
00534  3400-EXIT.                                                       EL1524
00535      EXIT.                                                        EL1524
00536                                  EJECT                            EL1524
00537  3500-READNEXT-ACTQ.                                              EL1524
00538                                                                   EL1524
00539      EXEC CICS READNEXT                                           EL1524
00540          DATASET (W-ACTQ-FILE-ID)                                 EL1524
00541          RIDFLD  (W-ACTQ-KEY)                                     EL1524
00542          SET     (ADDRESS OF ACTIVITY-QUE)                           CL**2
00543      END-EXEC.                                                    EL1524
00544                                                                   EL1524
00545  3500-EXIT.                                                       EL1524
00546      EXIT.                                                        EL1524
00547                                  EJECT                            EL1524
00548  4000-PRINT-TOTALS.                                               EL1524
00549                                                                   EL1524
00550      MOVE +56                    TO W-INCOMING-LINES.             EL1524
00551      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL1524
00552 *                                                                 EL1524
00553 *    MOVE '1'                    TO W-ML-CC.                      EL1524
00554 *    MOVE SPACES                 TO W-ML-DATA.                    EL1524
00555 *    MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
00556 *    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00557                                                                   EL1524
00558      MOVE W-ACTQ-RCRD-READ       TO W-TOTAL-AMT1.                 EL1524
00559      MOVE SPACES                 TO W-ML-CC.                      EL1524
00560      MOVE W-TOTAL1               TO W-ML-DATA.                    EL1524
00561      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00562      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00563                                                                   EL1524
00564      MOVE W-ACTQ-RCRD-USED       TO W-TOTAL-AMT2.                 EL1524
00565      MOVE '0'                    TO W-ML-CC.                      EL1524
00566      MOVE W-TOTAL2               TO W-ML-DATA.                    EL1524
00567      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00568      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00569                                                                   EL1524
00570      MOVE W-ACTQ-RCRD-CHANGED    TO W-TOTAL-AMT6.                 EL1524
00571      MOVE '0'                    TO W-ML-CC.                      EL1524
00572      MOVE W-TOTAL6               TO W-ML-DATA.                    EL1524
00573      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00574      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00575                                                                   EL1524
00576      MOVE W-ACTQ-RCRD-DELETED    TO W-TOTAL-AMT5.                 EL1524
00577      MOVE '0'                    TO W-ML-CC.                      EL1524
00578      MOVE W-TOTAL5               TO W-ML-DATA.                    EL1524
00579      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00580      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00581                                                                   EL1524
00582      MOVE W-FATAL-ERRORS         TO W-TOTAL-AMT3.                 EL1524
00583      MOVE '0'                    TO W-ML-CC.                      EL1524
00584      MOVE W-TOTAL3               TO W-ML-DATA.                    EL1524
00585      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00586      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00587                                                                   EL1524
00588      MOVE W-LETTERS-CREATED      TO W-TOTAL-AMT4.                 EL1524
00589      MOVE '0'                    TO W-ML-CC.                      EL1524
00590      MOVE W-TOTAL4               TO W-ML-DATA.                    EL1524
00591      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00592      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00593                                                                   EL1524
00594      MOVE W-LETTERS-WITH-ERRORS  TO W-TOTAL-AMT7.                 EL1524
00595      MOVE '0'                    TO W-ML-CC.                      EL1524
00596      MOVE W-TOTAL7               TO W-ML-DATA.                    EL1524
00597      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00598      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00599                                                                   EL1524
00600      MOVE '1'                    TO W-ML-CC.                      EL1524
00601      MOVE SPACES                 TO W-ML-DATA.                    EL1524
00602      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00603      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00604                                                                   EL1524
00605  4000-EXIT.                                                       EL1524
00606      EXIT.                                                        EL1524
00607                                  EJECT                            EL1524
00608  5000-GET-COMPANY-NAME.                                           EL1524
00609                                                                   EL1524
00610      EXEC CICS HANDLE CONDITION                                   EL1524
00611           NOTOPEN (5000-EXIT)                                     EL1524
00612           NOTFND  (5000-EXIT)                                     EL1524
00613      END-EXEC.                                                    EL1524
00614                                                                   EL1524
00615      MOVE SPACES                 TO W-CNTL-KEY.                   EL1524
00616      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.            EL1524
00617      MOVE '1'                    TO W-CNTL-RECORD-TYPE.           EL1524
00618      MOVE ZEROS                  TO W-CNTL-SEQ.                   EL1524
00619                                                                   EL1524
00620      EXEC CICS READ                                               EL1524
00621           DATASET (W-CNTL-FILE-ID)                                EL1524
00622           SET     (ADDRESS OF CONTROL-FILE)                          CL**2
00623           RIDFLD  (W-CNTL-KEY)                                    EL1524
00624      END-EXEC.                                                    EL1524
00625                                                                   EL1524
00626      MOVE CF-CL-MAIL-TO-NAME     TO W-COMPANY.                    EL1524
00627      MOVE SPACES                 TO W-CT-COMPANY.                 EL1524
00628      PERFORM 5100-CENTER-NAME THRU 5100-EXIT.                     EL1524
00629                                                                   EL1524
00630  5000-EXIT.                                                       EL1524
00631      EXIT.                                                        EL1524
00632                                  EJECT                            EL1524
00633  5100-CENTER-NAME.                                                EL1524
00634                                                                   EL1524
00635      MOVE ZEROS                  TO W-COMBINED-SPACES.            EL1524
00636                                                                   EL1524
00637      PERFORM 5120-FIND-LAST-CHAR THRU 5120-EXIT                   EL1524
00638              VARYING                                              EL1524
00639          W-CO-NDX FROM 30 BY -1                                   EL1524
00640              UNTIL                                                EL1524
00641          W-CO-NDX LESS THAN +1                                    EL1524
00642              OR                                                   EL1524
00643          W-CO-CHAR (W-CO-NDX) NOT EQUAL SPACES.                   EL1524
00644                                                                   EL1524
00645      COMPUTE W-COMBINED-SPACES = W-COMBINED-SPACES / 2.           EL1524
00646                                                                   EL1524
00647      SET W-CT-NDX                TO W-COMBINED-SPACES.            EL1524
00648                                                                   EL1524
00649      PERFORM 5140-MOVE-NAME-CHAR THRU 5140-EXIT                   EL1524
00650              VARYING                                              EL1524
00651          W-CO-NDX FROM +1 BY +1                                   EL1524
00652              UNTIL                                                EL1524
00653          W-CO-NDX GREATER THAN +30                                EL1524
00654              OR                                                   EL1524
00655          W-CT-NDX EQUAL +30.                                      EL1524
00656                                                                   EL1524
00657  5100-EXIT.                                                       EL1524
00658      EXIT.                                                        EL1524
00659                                  EJECT                            EL1524
00660  5120-FIND-LAST-CHAR.                                             EL1524
00661                                                                   EL1524
00662      IF  W-CO-CHAR (W-CO-NDX) EQUAL SPACE                         EL1524
00663          ADD +1                  TO W-COMBINED-SPACES.            EL1524
00664                                                                   EL1524
00665  5120-EXIT.                                                       EL1524
00666      EXIT.                                                        EL1524
00667                                                                   EL1524
00668  5140-MOVE-NAME-CHAR.                                                CL**2
00669                                                                   EL1524
00670      SET W-CT-NDX UP BY +1.                                       EL1524
00671      MOVE W-CO-CHAR (W-CO-NDX)   TO W-CT-CHAR (W-CT-NDX).         EL1524
00672                                                                   EL1524
00673  5140-EXIT.                                                       EL1524
00674      EXIT.                                                        EL1524
00675                                  EJECT                            EL1524
00676  8000-ACTQ-NOT-OPEN.                                              EL1524
00677                                                                   EL1524
00678      MOVE +56                    TO W-INCOMING-LINES              EL1524
00679      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL1524
00680                                                                   EL1524
00681      MOVE 'ELACTQ FILE IS NOT OPENED'                             EL1524
00682                                  TO W-ML-DATA.                    EL1524
00683      MOVE '0'                    TO W-ML-CC.                      EL1524
00684      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00685      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00686      GO TO 1000-EXIT.                                             EL1524
00687                                  EJECT                            EL1524
00688  8010-ACTQ-NOT-FOUND.                                             EL1524
00689                                                                   EL1524
00690      MOVE +2                     TO W-INCOMING-LINES              EL1524
00691      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL1524
00692                                                                   EL1524
00693      MOVE 'UNACCEPTABLE ERROR DETECTED WHILE READING ACTQ.'       EL1524
00694                                  TO W-ML-DATA.                    EL1524
00695      MOVE '0'                    TO W-ML-CC.                      EL1524
00696      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00697      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00698      GO TO 1000-EXIT.                                             EL1524
00699                                  EJECT                            EL1524
00700  8020-ERROR-ABEND.                                                EL1524
00701                                                                   EL1524
00702      MOVE +2                     TO W-INCOMING-LINES              EL1524
00703      PERFORM 8100-TOP-OF-PAGE THRU 8100-EXIT.                     EL1524
00704                                                                   EL1524
00705      MOVE 'UNACCEPTABLE ERROR DETECTED DURING PROCESSING.'        EL1524
00706                                  TO W-ML-DATA.                    EL1524
00707      MOVE '0'                    TO W-ML-CC.                      EL1524
00708      MOVE W-MESSAGE-LINE         TO WS-PRINT-AREA.                EL1524
CIDMOD*    MOVE ' '                TO DRS-SW                                 EL1
00709      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1524
00710      GO TO 1000-EXIT.                                             EL1524
00711                                  EJECT                            EL1524
00712  8100-TOP-OF-PAGE.                                                EL1524
00713                                                                   EL1524
00714      COMPUTE W-LINE-COUNT = W-LINE-COUNT + W-INCOMING-LINES.      EL1524
00715                                                                   EL1524
00716      IF  W-LINE-COUNT GREATER THAN +56                            EL1524
00717          COMPUTE W-LINE-COUNT = W-INCOMING-LINES + 3              EL1524
00718          ADD +1                  TO W-PAGE                        EL1524
00719          MOVE W-PAGE             TO W-CT-PAGE                     EL1524
00720          MOVE W-REPORT-TITLE     TO WS-PRINT-AREA                 EL1524
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00721          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL1524
00722          MOVE W-COMPANY-TITLE    TO WS-PRINT-AREA                 EL1524
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00723          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL1524
00724          MOVE SPACES             TO WS-PRINT-AREA                 EL1524
CIDMOD*        MOVE ' '                TO DRS-SW                             EL1
00725          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                     EL1524
00726                                                                   EL1524
00727  8100-EXIT.                                                       EL1524
00728      EXIT.                                                        EL1524
00729                                  EJECT                            EL1524
00730  9900-ERROR-FORMAT.                                               EL1524
00731                                                                   EL1524
00732      EXEC CICS LINK                                               EL1524
00733          PROGRAM  ('EL001')                                       EL1524
00734          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1524
00735          LENGTH   (EMI-COMM-LENGTH)                               EL1524
00736      END-EXEC.                                                    EL1524
00737                                                                   EL1524
00738  9900-EXIT.                                                       EL1524
00739      EXIT.                                                        EL1524
00740                                  COPY ELPRTCVP.                   EL1524
00741                                                                      CL**3
00742                                                                      CL**3
