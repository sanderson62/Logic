00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL052
00003  PROGRAM-ID.                 EL052 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 09:35:30.                    CL**3
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL052
00008 *                                                                 EL052
00009 *AUTHOR.     LOGIC INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL052
00012 *DATE-COMPILED.                                                      CL**3
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00020 *            *                                                   *   CL**3
00021 *            *****************************************************   CL**3
00022                                                                   EL052
00023 *REMARKS.   TRANSACTION - EXED                                       CL**3
00024 *         THIS PROGRAM IS STARTED  FROM CICS BY ENTERING THE         CL**3
00025 *         TRANSACTION I.D.  'EXED'. FUNCTION   IS TO                 CL**3
00026 *         START EL051 AND RETURN CONTROL TO CICS.                    CL**3
00027                                                                   EL052
00028      EJECT                                                        EL052
00029  ENVIRONMENT DIVISION.                                            EL052
00030  DATA DIVISION.                                                   EL052
00031  WORKING-STORAGE SECTION.                                         EL052
00032  77  FILLER  PIC X(32) VALUE '********************************'.  EL052
00033  77  FILLER  PIC X(32) VALUE '*     EL052  WORKING-STORAGE   *'.  EL052
00034  77  FILLER  PIC X(32) VALUE '************ V/M 2.003 *********'.     CL**3
00035                                                                   EL052
00036  01  WORK-AREAS.                                                  EL052
00037      12  EDIT-TRANS              PIC X(4)    VALUE 'XXSE'.        EL052
00038      12  TEXT-AREA.                                                  CL**3
00039          16  FILLER              PIC X(38).                       EL052
00040          16  TEXT-BATCH          PIC X(7).                        EL052
00041      12  TEXT-LENGTH             PIC S9(4)   VALUE +45 COMP.      EL052
00042      12  BATCH-LENGTH            PIC S9(4)   VALUE +16   COMP.    EL052
00043                                                                   EL052
00044      12  EL052-COMM-AREA         PIC X(10)   VALUE 'EL052 COMM'.  EL052
00045      12  COMM-LENGTH             PIC S9(4)   COMP  VALUE +10.     EL052
00046                                                                   EL052
00047  01  FILLER                      COMP-3.                          EL052
00048      05  WS-TIME-WORK            PIC S9(7) VALUE ZEROS.           EL052
00049      05  WS-TIME REDEFINES WS-TIME-WORK                           EL052
00050                                  PIC S9(3)V9(4).                  EL052
00051                                                                   EL052
00052  01  BATCH-TO-PROCESS.                                            EL052
00053      05  EDIT-COMPANY-CD         PIC X.                           EL052
00054      05  EDIT-BATCH              PIC X(6).                        EL052
00055      05  EDIT-COMPANY-ID         PIC XXX.                         EL052
00056      05  EDIT-RESTART-BATCH      PIC X(6).                        EL052
00057                                                                   EL052
00058  01  ACCESS-KEYS.                                                 EL052
00059      12  ELCNTL-KEY.                                              EL052
00060          16  CNTL-COMP-ID         PIC X(3).                       EL052
00061          16  CNTL-REC-TYPE        PIC X.                          EL052
00062          16  CNTL-ACCESS          PIC X(4).                       EL052
00063          16  CNTL-SEQ-NO          PIC S9(4)    COMP.              EL052
00064                                                                   EL052
00065  01  ERROR-MESSAGES.                                              EL052
00066      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL052
00067                                                                   EL052
00068      EJECT                                                        EL052
00069      COPY ELCEMIB.                                                   CL**3
00070      EJECT                                                        EL052
00071      COPY ELCDATE.                                                   CL**3
00072      EJECT                                                        EL052
00073      COPY ELCATTR.                                                   CL**3
00074      EJECT                                                        EL052
00075      COPY ELCAID.                                                    CL**3
00076      EJECT                                                        EL052
00077      COPY EL052S.                                                    CL**3
00078      EJECT                                                        EL052
00079  LINKAGE SECTION.                                                 EL052
00080  01  DFHCOMMAREA                     PIC X(1).                    EL052
00081 *01 PARMLIST .                                                       CL**3
00082 *    02  FILLER                    PIC S9(8)   COMP.                 CL**3
00083 *    02  ELCNTL-POINTER            PIC S9(8)   COMP.                 CL**3
00084      EJECT                                                        EL052
00085      COPY ELCCNTL.                                                   CL**3
00086      EJECT                                                        EL052
00087  PROCEDURE DIVISION.                                              EL052
00088                                                                   EL052
00089  0000-BEGIN-PROCESS.                                              EL052
00090      MOVE LOW-VALUES TO EL052AI.                                  EL052
00091      IF EIBCALEN = ZEROS                                          EL052
00092         PERFORM 8100-SEND-INITIAL-MAP THRU 8100-EXIT              EL052
00093         GO TO 9100-RETURN-TRAN.                                   EL052
00094                                                                   EL052
00095      IF EIBAID = DFHCLEAR                                         EL052
00096         MOVE ' CLEAR KEY ENTERED , EDIT PROCEDURE CONCLUDED'      EL052
00097         TO  TEXT-AREA                                             EL052
00098         PERFORM 8300-SEND-TEXT THRU 8300-EXIT                     EL052
00099         GO TO 9000-RETURN-CICS.                                   EL052
00100                                                                   EL052
00101      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL052
00102         MOVE ER-0029              TO EMI-ERROR                    EL052
00103         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL052
00104         MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                      EL052
00105         MOVE -1                   TO ATYPEL                       EL052
00106         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00107         GO TO 9100-RETURN-TRAN.                                   EL052
00108                                                                   EL052
00109      EXEC CICS HANDLE CONDITION                                   EL052
00110          NOTFND    (0099-COMPANY-NOT-FOUND)                       EL052
00111          ERROR     (9990-ERROR)                                   EL052
00112          PGMIDERR  (9990-ERROR)                                   EL052
00113          TERMIDERR (8900-TERMIDERR)                               EL052
00114          END-EXEC.                                                EL052
00115                                                                   EL052
00116  0010-RECEIVE-MAP.                                                EL052
00117      EXEC CICS RECEIVE                                            EL052
00118           MAP      ('EL052A')                                     EL052
00119           MAPSET   ('EL052S')                                     EL052
00120           END-EXEC.                                               EL052
00121                                                                   EL052
00122      EJECT                                                        EL052
00123  0020-EDIT-INPUT.                                                 EL052
00124      IF EIBAID NOT = DFHENTER                                     EL052
00125         MOVE ER-0029 TO EMI-ERROR                                 EL052
00126         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL052
00127         MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                      EL052
00128         MOVE -1                   TO ATYPEL                       EL052
00129         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00130         GO TO 9100-RETURN-TRAN.                                   EL052
00131                                                                   EL052
00132      IF ATYPEL NOT GREATER THAN +0                                EL052
00133         MOVE -1       TO ATYPEL                                   EL052
00134         MOVE AL-UABON TO ATYPEA                                   EL052
00135         MOVE ' EDIT TYPE IS REQUIRED' TO AEMSG1O                  EL052
00136         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00137         GO TO 9100-RETURN-TRAN                                    EL052
00138      ELSE                                                         EL052
00139         IF ATYPEI NOT = 'F' AND 'B' AND 'R'                       EL052
00140            MOVE -1       TO ATYPEL                                EL052
00141            MOVE AL-UABON TO ATYPEA                                EL052
00142            MOVE ' INVALID EDIT TYPE, MUST BE F,B OR R' TO AEMSG1O EL052
00143            PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT              EL052
00144            GO TO 9100-RETURN-TRAN.                                EL052
00145                                                                   EL052
00146      IF ACOMPIDL NOT GREATER THAN +0                              EL052
00147         MOVE -1       TO ACOMPIDL                                 EL052
00148         MOVE AL-UABON TO ACOMPIDA                                 EL052
00149         MOVE ' COMPANY ID IS  REQUIRED' TO AEMSG1O                EL052
00150         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00151         GO TO 9100-RETURN-TRAN.                                   EL052
00152                                                                   EL052
00153      IF (ATYPEI = 'R' OR 'B') AND                                 EL052
00154         (ABATCHL NOT GREATER THAN +0)                             EL052
00155         MOVE -1       TO ABATCHL                                  EL052
00156         MOVE AL-UABON TO ABATCHA                                  EL052
00157         MOVE ' BATCH IS REQUIRED IF TYPE IS R OR B' TO AEMSG1O    EL052
00158         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00159         GO TO 9100-RETURN-TRAN.                                   EL052
00160                                                                   EL052
00161      IF (ATYPEI = 'F') AND                                        EL052
00162         (ABATCHL GREATER THAN +0)                                 EL052
00163         MOVE -1       TO ABATCHL                                  EL052
00164         MOVE AL-UABON TO ABATCHA                                  EL052
00165         MOVE ' BATCH IS INVALID IF TYPE IS F      ' TO AEMSG1O    EL052
00166         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00167         GO TO 9100-RETURN-TRAN.                                   EL052
00168                                                                   EL052
00169      IF AWHENL NOT GREATER THAN +0                                EL052
00170         MOVE -1       TO AWHENL                                   EL052
00171         MOVE AL-UABON TO AWHENA                                   EL052
00172         MOVE ' WHEN IS REQUIRED        ' TO AEMSG1O               EL052
00173         PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT                 EL052
00174         GO TO 9100-RETURN-TRAN                                    EL052
00175      ELSE                                                         EL052
00176         IF AWHENI NOT = 'N' AND 'T'                               EL052
00177            MOVE -1       TO AWHENL                                EL052
00178            MOVE AL-UABON TO AWHENA                                EL052
00179            MOVE ' INVALID TIME, MUST BE N OR T' TO AEMSG1O        EL052
00180            PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT              EL052
00181            GO TO 9100-RETURN-TRAN                                 EL052
00182         ELSE                                                      EL052
00183            IF (AWHENI = 'T') AND                                  EL052
00184               (ATYPEI = 'B')                                      EL052
00185               MOVE -1       TO AWHENL                             EL052
00186               MOVE AL-UABON TO AWHENA                             EL052
00187               MOVE ' TONIGHT NOT AVAILABLE IF TYPE IS B' TO       EL052
00188               AEMSG1O                                             EL052
00189               PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT           EL052
00190               GO TO 9100-RETURN-TRAN.                             EL052
00191                                                                   EL052
00192      EJECT                                                        EL052
00193  0030-READ-CONTROL-FILE.                                          EL052
00194      MOVE ACOMPIDI TO CNTL-COMP-ID.                               EL052
00195      MOVE SPACES   TO CNTL-ACCESS.                                EL052
00196      MOVE '1'      TO CNTL-REC-TYPE.                              EL052
00197      MOVE +0       TO CNTL-SEQ-NO.                                EL052
00198                                                                   EL052
00199      EXEC CICS READ                                               EL052
00200           DATASET     ('ELCNTL')                                  EL052
00201           SET         (ADDRESS OF CONTROL-FILE)                      CL**3
00202           RIDFLD      (ELCNTL-KEY)                                EL052
00203           GTEQ                                                    EL052
00204      END-EXEC.                                                    EL052
00205                                                                   EL052
00206      IF CF-COMPANY-ID NOT = ACOMPIDI                              EL052
00207         GO TO 0099-COMPANY-NOT-FOUND.                             EL052
00208                                                                   EL052
00209      IF CF-COMPANY-MASTER                                         EL052
00210         IF CF-LGX-CREDIT-USER NOT = 'Y'                              CL**2
00211            MOVE -1       TO ACOMPIDL                              EL052
00212            MOVE AL-UABON TO ACOMPIDA                              EL052
00213            MOVE ' COMPANY IS NOT A CREDIT USER' TO AEMSG1O        EL052
00214            PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT              EL052
00215            GO TO 9100-RETURN-TRAN                                 EL052
00216        ELSE                                                       EL052
00217         IF (CF-EDIT-SW = '1') OR                                  EL052
00218            (CF-EDIT-RESTART-BATCH NOT = SPACES AND                EL052
00219             LOW-VALUES AND ZEROS) AND (AWHENI = 'T')              EL052
00220            MOVE -1       TO AWHENL                                EL052
00221            MOVE AL-UABON TO AWHENA                                EL052
00222            MOVE ' EDIT ALREADY SCHEDULED TO RUN TONIGHT' TO       EL052
00223            AEMSG1O                                                EL052
00224            PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT              EL052
00225            GO TO 9100-RETURN-TRAN                                 EL052
00226        ELSE                                                       EL052
00227         IF (ATYPEI = 'F' OR 'R') AND (AWHENI = 'T')               EL052
00228            MOVE CF-CONTROL-PRIMARY TO ELCNTL-KEY                  EL052
00229            EXEC CICS READ                                         EL052
00230                 DATASET    ('ELCNTL')                             EL052
00231                 SET        (ADDRESS OF CONTROL-FILE)                 CL**3
00232                 RIDFLD     (ELCNTL-KEY)                           EL052
00233                 UPDATE                                            EL052
00234            END-EXEC                                               EL052
00235            MOVE '1'     TO CF-EDIT-SW                             EL052
00236            MOVE ABATCHI TO CF-EDIT-RESTART-BATCH                  EL052
00237            EXEC CICS REWRITE                                      EL052
00238                 DATASET    ('ELCNTL')                             EL052
00239                 FROM       (CONTROL-FILE)                         EL052
00240            END-EXEC                                               EL052
00241            MOVE LOW-VALUES TO EL052AI                             EL052
00242            MOVE -1         TO ATYPEL                              EL052
00243            MOVE ' EDIT WILL BE STARTED TONIGHT' TO AEMSG1O        EL052
00244            PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT              EL052
00245            GO TO 9100-RETURN-TRAN                                 EL052
00246         ELSE                                                      EL052
00247            GO TO 0100-START.                                      EL052
00248                                                                   EL052
00249  0099-COMPANY-NOT-FOUND.                                          EL052
00250      MOVE -1       TO ACOMPIDL.                                   EL052
00251      MOVE AL-UABON TO ACOMPIDA.                                   EL052
00252      MOVE ' INVALID COMPANY ID'  TO AEMSG1O.                      EL052
00253      PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT.                   EL052
00254      GO TO 9100-RETURN-TRAN.                                      EL052
00255                                                                   EL052
00256      EJECT                                                        EL052
00257  0100-START.                                                      EL052
00258      MOVE SPACES        TO BATCH-TO-PROCESS.                      EL052
00259      MOVE CF-COMPANY-CD TO EDIT-COMPANY-CD                        EL052
00260      MOVE CF-COMPANY-ID TO EDIT-COMPANY-ID                        EL052
00261                                                                   EL052
00262      IF ABATCHL GREATER THAN ZERO                                 EL052
00263         IF ATYPEI = 'B'                                           EL052
00264            MOVE ABATCHI TO EDIT-BATCH                             EL052
00265            MOVE SPACES  TO EDIT-RESTART-BATCH                     EL052
00266            MOVE 'EXEB'  TO EDIT-TRANS                             EL052
00267         ELSE                                                      EL052
00268            MOVE ABATCHI TO EDIT-RESTART-BATCH                     EL052
00269            MOVE SPACES TO EDIT-BATCH.                             EL052
00270                                                                   EL052
00271      IF ATERMIDL GREATER THAN +0                                  EL052
00272         EXEC CICS START                                           EL052
00273              TRANSID     (EDIT-TRANS)                             EL052
00274              FROM        (BATCH-TO-PROCESS)                       EL052
00275              LENGTH      (BATCH-LENGTH)                           EL052
00276              TERMID      (ATERMIDI)                               EL052
00277         END-EXEC                                                  EL052
00278      ELSE                                                         EL052
00279         EXEC CICS START                                           EL052
00280              TRANSID     (EDIT-TRANS)                             EL052
00281              FROM        (BATCH-TO-PROCESS)                       EL052
00282              LENGTH      (BATCH-LENGTH)                           EL052
00283         END-EXEC.                                                 EL052
00284                                                                   EL052
00285      MOVE ' EDIT HAS BEEN STARTED SUCESSFULLY' TO AEMSG1O.        EL052
00286      MOVE -1 TO ATYPEL.                                           EL052
00287      PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT.                   EL052
00288      GO TO 9100-RETURN-TRAN.                                      EL052
00289                                                                   EL052
00290      EJECT                                                        EL052
00291                                                                   EL052
00292  8000-FORMAT-DATE-TIME.                                           EL052
00293      MOVE EIBDATE TO DC-JULIAN-YYDDD.                             EL052
00294      MOVE '5'     TO DC-OPTION-CODE.                              EL052
00295      EXEC CICS LINK                                               EL052
00296           PROGRAM   ('ELDATCV')                                   EL052
00297           COMMAREA  (DATE-CONVERSION-DATA)                        EL052
00298           LENGTH    (DC-COMM-LENGTH)                              EL052
00299      END-EXEC.                                                    EL052
00300                                                                   EL052
00301      MOVE DC-GREG-DATE-1-EDIT TO ADATEO.                          EL052
00302      MOVE EIBTIME             TO WS-TIME-WORK.                    EL052
00303      MOVE WS-TIME             TO ATIMEO.                          EL052
00304                                                                   EL052
00305  8000-EXIT.                                                       EL052
00306      EXIT.                                                        EL052
00307                                                                   EL052
00308  8100-SEND-INITIAL-MAP.                                           EL052
00309      MOVE -1 TO ATYPEL.                                           EL052
00310      PERFORM 8000-FORMAT-DATE-TIME THRU 8000-EXIT.                EL052
00311      EXEC CICS SEND                                               EL052
00312           MAP      ('EL052A')                                     EL052
00313           MAPSET   ('EL052S')                                     EL052
00314           ERASE                                                   EL052
00315           FREEKB                                                  EL052
00316           CURSOR                                                  EL052
00317      END-EXEC.                                                    EL052
00318                                                                   EL052
00319  8100-EXIT.                                                       EL052
00320      EXIT.                                                        EL052
00321      EJECT                                                        EL052
00322  8200-SEND-DATAONLY.                                              EL052
00323      PERFORM 8000-FORMAT-DATE-TIME THRU 8000-EXIT.                EL052
00324      EXEC CICS SEND                                               EL052
00325           MAP      ('EL052A')                                     EL052
00326           MAPSET   ('EL052S')                                     EL052
00327           DATAONLY                                                EL052
00328           FREEKB                                                  EL052
00329           CURSOR                                                  EL052
00330      END-EXEC.                                                    EL052
00331                                                                   EL052
00332  8200-EXIT.                                                       EL052
00333      EXIT.                                                        EL052
00334                                                                   EL052
00335  8300-SEND-TEXT.                                                  EL052
00336      EXEC CICS SEND TEXT                                          EL052
00337          FROM    (TEXT-AREA)                                      EL052
00338          LENGTH  (TEXT-LENGTH)                                    EL052
00339          ERASE                                                    EL052
00340          FREEKB                                                   EL052
00341          END-EXEC.                                                EL052
00342                                                                   EL052
00343  8300-EXIT.                                                       EL052
00344      EXIT.                                                        EL052
00345      EJECT                                                        EL052
00346  8900-TERMIDERR.                                                  EL052
00347      MOVE -1       TO ATERMIDL.                                   EL052
00348      MOVE AL-UABON TO ATERMIDA.                                   EL052
00349      MOVE ' INVALID TERMINAL ID RE-ENTER ' TO AEMSG1O.            EL052
00350      PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT.                   EL052
00351      GO TO 9100-RETURN-TRAN.                                      EL052
00352                                                                   EL052
00353  8900-EXIT.                                                       EL052
00354      EXIT.                                                        EL052
00355                                                                   EL052
00356  9000-RETURN-CICS.                                                EL052
00357      EXEC CICS RETURN                                             EL052
00358      END-EXEC.                                                    EL052
00359                                                                   EL052
00360      GOBACK.                                                         CL**2
00361                                                                   EL052
00362  9000-EXIT.                                                       EL052
00363      EXIT.                                                        EL052
00364                                                                   EL052
00365  9100-RETURN-TRAN.                                                EL052
00366      EXEC CICS RETURN                                             EL052
00367           TRANSID   (EIBTRNID)                                    EL052
00368           COMMAREA  (EL052-COMM-AREA)                             EL052
00369           LENGTH    (COMM-LENGTH)                                 EL052
00370      END-EXEC.                                                    EL052
00371                                                                   EL052
00372  9100-EXIT.                                                       EL052
00373      EXIT.                                                        EL052
00374      EJECT                                                        EL052
00375  9900-ERROR-FORMAT.                                               EL052
00376      EXEC CICS LINK                                               EL052
00377          PROGRAM    ('EL001')                                     EL052
00378          COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)               EL052
00379          LENGTH     (EMI-COMM-LENGTH)                             EL052
00380          END-EXEC.                                                EL052
00381                                                                   EL052
00382  9900-EXIT.                                                       EL052
00383      EXIT.                                                        EL052
00384                                                                   EL052
00385  9990-ERROR.                                                      EL052
00386      MOVE DFHEIBLK TO EMI-LINE1.                                  EL052
00387      EXEC CICS LINK                                               EL052
00388          PROGRAM   ('EL004')                                      EL052
00389          COMMAREA  (EMI-LINE1)                                    EL052
00390          LENGTH    (72)                                           EL052
00391      END-EXEC.                                                    EL052
00392                                                                   EL052
00393      MOVE -1 TO ATYPEL.                                           EL052
00394      PERFORM 8200-SEND-DATAONLY THRU 8200-EXIT.                   EL052
00395      GO TO 9100-RETURN-TRAN.                                      EL052
00396                                                                   EL052
00397  9990-EXIT.                                                       EL052
00398      EXIT.                                                        EL052
00399                                                                   EL052
00400  9999-DUMMY-PARA.                                                 EL052
00401      EJECT                                                        EL052
