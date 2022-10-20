00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL053
00003  PROGRAM-ID.                 EL053 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 09:37:16.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL053
00008 *                                                                 EL053
00009 *AUTHOR.     LOGIC, INC.                                             CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL053
00012 *DATE-COMPILED.                                                      CL**5
00013                                                                   EL053
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00021 *            *                                                   *   CL**5
00022 *            *****************************************************   CL**5
00023                                                                   EL053
00024 *REMARKS.                                                            CL**5
00025 *          THIS SUBROUTINE IS CALLED TO EDIT THE CLAIMS              CL**5
00026 *           RECORDS FOR THE ONLINE CREDIT SYSTEM.                    CL**5
00027                                                                   EL053
00028  ENVIRONMENT DIVISION.                                            EL053
00029                                                                   EL053
00030  DATA DIVISION.                                                   EL053
00031      EJECT                                                        EL053
00032  WORKING-STORAGE SECTION.                                         EL053
00033  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.                   CL**5
00034  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP        CL**5
00035                                    USAGE POINTER.                    CL**5
00036  77  FILLER   PIC X(32) VALUE '********************************'. EL053
00037  77  FILLER   PIC X(32) VALUE '**  EL053   WORKING STORAGE   **'. EL053
00038  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.005 *********'.    CL**5
00039                                                                   EL053
00040      COPY ELC53WS.                                                   CL**4
00041      COPY ELCJPFX.                                                   CL**4
00042                                PIC X(450).                        EL053
00043                                                                   EL053
00044      EJECT                                                        EL053
00045      COPY ELCDATE.                                                   CL**4
00046      EJECT                                                        EL053
00047      COPY ELCCALC.                                                   CL**4
00048      EJECT                                                        EL053
00049      COPY ERCPNDC.                                                   CL**4
00050      12  WK-PC-WORK-AREA.                                         EL053
00051          16  WK-PC-CNTL-RECORD-FOUND-SW  PIC X.                   EL053
00052          16  WK-PC-LAST-CARRIER          PIC X.                   EL053
00053          16  WK-PC-CERT-ACCESS-CNTL      PIC X.                   EL053
00054          16  WK-PC-CO-CLAIM-REJECT-SW    PIC X.                   EL053
00055          16  WK-PC-CLAIM-SYSTEM-SW       PIC X.                   EL053
00056          16  WK-PC-CO-TOL-CLAIM          PIC S9(3)V99  COMP-3.    EL053
00057          16  WK-PC-RESERVE-CONTROLS      PIC X(4).                EL053
00058          16  WK-PC-CREDIT-EDIT-CONTROLS  PIC X(12).               EL053
00059      12  WK-PC-RECORD-ADDRESSES.                                  EL053
00060          16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.      EL053
00061          16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.      EL053
00062      12  WK-MISC.                                                    CL**3
00063          16  WK-PC-REM-TRM-CALC-OPTION   PIC X.                      CL**3
00064          16  FILLER                      PIC X(20).                  CL**3
00065                                                                   EL053
00066  LINKAGE SECTION.                                                 EL053
00067                                                                   EL053
00068  01  DFHCOMMAREA                 PIC X(553).                         CL**5
00069                                                                   EL053
00070 *01 PARMLIST .                                                       CL**5
00071 *    02  FILLER                  PIC S9(8)   COMP.                   CL**5
00072 *    02  ELCNTL-POINTER          PIC S9(8)   COMP.                   CL**5
00073 *    02  ERACCT-POINTER          PIC S9(8)   COMP.                   CL**5
00074 *    02  ELCERT-POINTER          PIC S9(8)   COMP.                   CL**5
00075      EJECT                                                        EL053
00076      COPY ELCCNTL.                                                   CL**4
00077      EJECT                                                        EL053
00078      COPY ERCACCT.                                                   CL**4
00079      EJECT                                                        EL053
00080      COPY ELCCERT.                                                   CL**4
00081      EJECT                                                        EL053
00082                                                                   EL053
00083  PROCEDURE DIVISION.                                              EL053
00084                                                                   EL053
00085      MOVE DFHCOMMAREA            TO  PENDING-CLAIMS.              EL053
00086                                                                   EL053
00087      EXEC CICS  HANDLE CONDITION                                  EL053
00088             ERROR    (9300-ABEND)                                 EL053
00089             END-EXEC.                                             EL053
00090                                                                   EL053
uktdel*0000-ELC53PD.         COPY ELC53PD.                              EL053
uktins 0000-ELC53PD.
uktins     COPY ELC53PD.
00092                                                                   EL053
00093  7200-FIND-BENEFIT.                                               EL053
00094      MOVE 'N'                    TO BEN-SEARCH-SW.                EL053
00095                                                                   EL053
00096      EXEC CICS HANDLE CONDITION                                   EL053
00097          ENDFILE   (7200-EXIT)                                    EL053
00098          NOTFND    (7200-EXIT)                                    EL053
00099          END-EXEC.                                                EL053
00100                                                                   EL053
00101      EXEC CICS READ                                               EL053
00102          DATASET     (ELCNTL-FILE-ID)                             EL053
00103          SET         (ADDRESS OF CONTROL-FILE)                       CL**5
00104          RIDFLD      (ELCNTL-KEY)                                 EL053
00105          GTEQ                                                     EL053
00106          END-EXEC.                                                EL053
00107                                                                   EL053
00108      IF (CNTL-COMP-ID NOT = CF-COMPANY-ID) OR                     EL053
00109         (CNTL-REC-TYPE NOT = CF-RECORD-TYPE)                      EL053
00110            GO TO 7200-EXIT.                                       EL053
00111                                                                   EL053
00112      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT              EL053
00113          VARYING SUB3 FROM 1 BY 1 UNTIL                           EL053
00114             ((SUB3 GREATER 8) OR                                  EL053
00115             (CF-BENEFIT-CODE (SUB3) = WS-BEN-CD)).                   CL**2
00116                                                                   EL053
00117      IF SUB3 NOT = 9                                              EL053
00118          MOVE 'Y'                TO BEN-SEARCH-SW.                EL053
00119                                                                   EL053
00120      GO TO 7200-EXIT.                                             EL053
00121                                                                   EL053
00122  7200-BENEFIT-DUMMY.                                              EL053
00123                                                                   EL053
00124  7200-DUMMY-EXIT.                                                 EL053
00125      EXIT.                                                        EL053
00126                                                                   EL053
00127  7200-EXIT.                                                       EL053
00128      EXIT.                                                        EL053
00129      EJECT                                                        EL053
00130                                                                   EL053
00131  7600-GET-REMAIN-TERM.                                            EL053
00132      EXEC CICS LINK                                               EL053
00133          PROGRAM    (LINK-ELRTRM)                                 EL053
00134          COMMAREA   (CALCULATION-PASS-AREA)                       EL053
00135          LENGTH     (CP-COMM-LENGTH)                              EL053
00136          END-EXEC.                                                EL053
00137                                                                   EL053
00138  7600-EXIT.                                                       EL053
00139      EXIT.                                                        EL053
00140      EJECT                                                        EL053
00141                                                                   EL053
00142  7700-GET-REMAIN-AMT.                                             EL053
00143      EXEC CICS LINK                                               EL053
00144          PROGRAM    (LINK-ELRAMT)                                 EL053
00145          COMMAREA   (CALCULATION-PASS-AREA)                       EL053
00146          LENGTH     (CP-COMM-LENGTH)                              EL053
00147          END-EXEC.                                                EL053
00148                                                                   EL053
00149  7700-EXIT.                                                       EL053
00150      EXIT.                                                        EL053
00151      EJECT                                                        EL053
00152                                                                   EL053
00153  7800-REWRITE-CERT.                                               EL053
00154      EXEC CICS REWRITE                                            EL053
00155          DATASET     (ELCERT-FILE-ID)                             EL053
00156          FROM        (CERTIFICATE-MASTER)                         EL053
00157          END-EXEC.                                                EL053
00158                                                                   EL053
00159  7800-EXIT.                                                       EL053
00160       EXIT.                                                       EL053
00161                                                                   EL053
00162  8100-JOURNAL-CERT.                                               EL053
00163      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.               EL053
00164      MOVE PC-LAST-MAINT-BY       TO JP-USER-ID.                   EL053
00165      MOVE 'ELCERT  '             TO JP-FILE-ID.                   EL053
00166      MOVE ZERO                   TO JP-GENERIC-KEY-LENGTH.        EL053
00167      COMPUTE JOURNAL-LENGTH = ELCERT-LENGTH + 23.                 EL053
00168                                                                   EL053
00169 *    EXEC CICS JOURNAL                                            EL053
00170 *        JFILEID     (WS-JOURNAL-FILE-ID)                         EL053
00171 *        JTYPEID     ('EL')                                       EL053
00172 *        FROM        (JOURNAL-RECORD)                             EL053
00173 *        LENGTH      (JOURNAL-LENGTH)                             EL053
00174 *        END-EXEC.                                                EL053
00175                                                                   EL053
00176  8100-EXIT.                                                       EL053
00177       EXIT.                                                       EL053
00178                                                                   EL053
00179       EJECT                                                       EL053
00180  8200-START-BR.                                                   EL053
00181      EXEC CICS HANDLE CONDITION                                   EL053
00182          NOTFND    (0199-ACCT-NOT-FOUND)                          EL053
00183          NOTOPEN   (0198-ACCT-NOT-OPEN)                           EL053
00184          ENDFILE   (0199-ACCT-NOT-FOUND)                          EL053
00185          END-EXEC.                                                EL053
00186      EXEC CICS STARTBR                                            EL053
00187          DATASET     (ERACCT-FILE-ID)                             EL053
00188          RIDFLD      (ERACCT-KEY)                                 EL053
00189          END-EXEC.                                                EL053
00190                                                                   EL053
00191  8200-EXIT.                                                       EL053
00192      EXIT.                                                        EL053
00193                                                                   EL053
00194  8300-READ-ACCT.                                                  EL053
00195      EXEC CICS READNEXT                                           EL053
00196          DATASET     (ERACCT-FILE-ID)                             EL053
00197          INTO        (ACCOUNT-MASTER)                             EL053
00198          RIDFLD      (ERACCT-KEY)                                 EL053
00199          END-EXEC.                                                EL053
00200                                                                   EL053
00201  8300-EXIT.                                                       EL053
00202      EXIT.                                                        EL053
00203                                                                   EL053
00204  8500-DATE-CONVERT.                                               EL053
00205      EXEC CICS LINK                                               EL053
00206          PROGRAM    (LINK-ELDATCV)                                EL053
00207          COMMAREA   (DATE-CONVERSION-DATA)                        EL053
00208          LENGTH     (DC-COMM-LENGTH)                              EL053
00209          END-EXEC.                                                EL053
00210                                                                   EL053
00211  8500-EXIT.                                                       EL053
00212      EXIT.                                                        EL053
00213      EJECT                                                        EL053
00214                                                                   EL053
00215  8600-SRL-ACCOUNT-MASTER.                                         EL053
00216      CONTINUE.                                                       CL**5
00217                                                                   EL053
00218  8600-EXIT.                                                       EL053
00219      EXIT.                                                        EL053
00220                                                                   EL053
00221  8700-SRL-CERT-MASTER.                                            EL053
00222      EXEC CICS HANDLE CONDITION                                   EL053
00223           NOTOPEN   (0298-CERT-NOT-OPEN)                          EL053
00224           NOTFND    (0299-CERT-NOT-FOUND)                         EL053
00225           END-EXEC.                                               EL053
00226                                                                   EL053
00227                                                                   EL053
00228      EXEC CICS READ                                               EL053
00229          DATASET     (ELCERT-FILE-ID)                             EL053
00230          SET         (ADDRESS OF CERTIFICATE-MASTER)                 CL**5
00231          RIDFLD      (ELCERT-KEY)                                 EL053
00232          UPDATE                                                   EL053
00233          END-EXEC.                                                EL053
00234                                                                   EL053
00235  8700-EXIT.                                                       EL053
00236      EXIT.                                                        EL053
00237                                                                   EL053
00238  8800-SRL-CONT-FILE.                                              EL053
00239      CONTINUE.                                                       CL**5
00240                                                                   EL053
00241  8800-EXIT.                                                       EL053
00242      EXIT.                                                        EL053
00243                                                                   EL053
00244  8900-GETMAIN.                                                    EL053
00245      EXEC CICS GETMAIN                                            EL053
00246          LENGTH (2000)                                            EL053
00247          SET    (ADDRESS OF ACCOUNT-MASTER)                          CL**5
00248          END-EXEC.                                                EL053
00249                                                                   EL053
00250  8900-EXIT.                                                       EL053
00251      EXIT.                                                        EL053
00252                                                                   EL053
00253  9000-READ-CONTROL.                                               EL053
00254      EXEC CICS HANDLE CONDITION                                   EL053
00255          NOTFND    (9000-NOT-FOUND)                               EL053
00256          NOTOPEN   (9000-NOT-OPEN)                                EL053
00257          END-EXEC.                                                EL053
00258                                                                   EL053
00259      EXEC CICS READ                                               EL053
00260          DATASET     (ELCNTL-FILE-ID)                             EL053
00261          SET         (ADDRESS OF CONTROL-FILE)                       CL**5
00262          RIDFLD      (ELCNTL-KEY)                                 EL053
00263          GTEQ                                                     EL053
00264          END-EXEC.                                                EL053
00265                                                                   EL053
00266      GO TO 9000-EXIT.                                             EL053
00267                                                                   EL053
00268  9000-NOT-FOUND.                                                  EL053
00269      IF CTL-READ = 'C'                                            EL053
00270          GO TO 0099-COMPANY-NOT-FOUND.                            EL053
00271                                                                   EL053
00272      IF CTL-READ = 'M'                                            EL053
00273          GO TO 0301-CARRIER-NOT-FOUND.                            EL053
00274                                                                   EL053
00275      GO TO 9000-EXIT.                                             EL053
00276                                                                   EL053
00277  9000-NOT-OPEN.                                                   EL053
00278      IF CTL-READ = 'C'                                            EL053
00279          GO TO 0098-COMPANY-NOT-OPEN.                             EL053
00280      GO TO 9000-EXIT.                                             EL053
00281                                                                   EL053
00282  9000-EXIT.                                                       EL053
00283       EXIT.                                                       EL053
00284      EJECT                                                        EL053
00285  9100-READ-CONTROL.                                               EL053
00286      EXEC CICS HANDLE CONDITION                                   EL053
00287          NOTFND    (0310-STATE-NOT-FOUND)                         EL053
00288          END-EXEC.                                                EL053
00289                                                                   EL053
00290      EXEC CICS READ                                               EL053
00291          DATASET     (ELCNTL-FILE-ID)                             EL053
00292          INTO        (CONTROL-FILE)                               EL053
00293          RIDFLD      (ELCNTL-KEY)                                 EL053
00294          GTEQ                                                     EL053
00295          END-EXEC.                                                EL053
00296                                                                   EL053
00297  9100-EXIT.                                                       EL053
00298       EXIT.                                                       EL053
00299                                                                   EL053
00300  9200-CONTROL-GETMAIN.                                            EL053
00301      EXEC CICS GETMAIN                                            EL053
00302           LENGTH   (750)                                             CL**4
00303           SET      (ADDRESS OF CONTROL-FILE)                         CL**5
00304      END-EXEC.                                                       CL**5
00305                                                                   EL053
00306  9200-EXIT.                                                       EL053
00307      EXIT.                                                        EL053
00308      EJECT                                                        EL053
00309                                                                   EL053
00310  9250-UNLOCK.                                                     EL053
00311      EXEC CICS UNLOCK                                             EL053
00312          DATASET (ELCERT-FILE-ID)                                 EL053
00313          END-EXEC                                                 EL053
00314      GO TO 9990-RETURN.                                           EL053
00315                                                                   EL053
00316  9300-ABEND.                                                      EL053
00317      MOVE DFHEIBLK TO WS-ERROR-LINE                               EL053
00318      EXEC CICS LINK                                               EL053
00319          PROGRAM   (LINK-004)                                     EL053
00320          COMMAREA  (WS-ERROR-LINE)                                EL053
00321          LENGTH    (72)                                           EL053
00322      END-EXEC.                                                    EL053
00323                                                                   EL053
00324  9990-RETURN.                                                     EL053
00325      IF ACCOUNT-BROWSE-STARTED                                    EL053
00326          EXEC CICS ENDBR                                          EL053
00327              DATASET  (ERACCT-FILE-ID)                            EL053
00328              END-EXEC.                                            EL053
00329                                                                   EL053
00330      MOVE PENDING-CLAIMS         TO  DFHCOMMAREA.                 EL053
00331                                                                   EL053
00332      EXEC CICS RETURN                                             EL053
00333           END-EXEC.                                               EL053
00334      GOBACK.                                                      EL053
00335                                                                   EL053
