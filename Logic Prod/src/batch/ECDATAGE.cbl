00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ECDATAGE
00003  PROGRAM-ID.                 ECDATAGE.                               LV028
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 05/09/94 09:52:36.                    CL**3
00007 *                            VMOD=2.001                              CL**3
00008 *                                                                    CL**3
00009 *AUTHOR.     LOGIC INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                      CL**3
00012 *SECURITY.   *****************************************************   CL**3
00013 *            *                                                   *   CL**3
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00019 *            *                                                   *   CL**3
00020 *            *****************************************************   CL**3
00021                                                                      CL**3
00022 *REMARKS.                                                            CL**3
00023 ************ THIS PROGRAM RUNS BY CLIENT (DATE-CARD DRIVEN)          CL**3
00024 *                                                                    CL**3
00025 *         THIS PROGRAM IS USED TO AGE THE DATES BY A SPECIFIC        CL**3
00026 *       NUMBER OF MONTHS SUPPLIED BY THE CARD FILE.                  CL**3
00027                                                                      CL**3
00028  ENVIRONMENT DIVISION.                                               CL**3
00029  INPUT-OUTPUT SECTION.                                               CL**3
00030  FILE-CONTROL.                                                       CL**3
00031                                                                      CL**3
00032      SELECT CARD-FILE ASSIGN S-SYS006.                               CL**3
00033                                                                      CL**3
00034      SELECT ECSCERT-IN ASSIGN S-CERTIN.                              CL**3
00035      SELECT ECSEPEC-IN ASSIGN S-EPECIN.                              CL**3
00036      SELECT ECSCLMS-IN ASSIGN S-CLMSIN.                              CL**3
00037      SELECT ECSRESV-IN ASSIGN S-RESVIN.                              CL**3
00038      SELECT ELEXTR-IN  ASSIGN S-EXTRIN.                              CL*19
00039                                                                      CL**3
00040      SELECT ECSCERT-OT ASSIGN S-CERTOT.                              CL**3
00041      SELECT ECSEPEC-OT ASSIGN S-EPECOT.                              CL**3
00042      SELECT ECSCLMS-OT ASSIGN S-CLMSOT.                              CL**3
00043      SELECT ECSRESV-OT ASSIGN S-RESVOT.                              CL**3
00044      SELECT ELEXTR-OT  ASSIGN S-EXTROT.                              CL*19
00045                                                                      CL**3
00046      SELECT DISK-DATE  ASSIGN S-SYS019  .                            CL*14
00047                                                                      CL**3
00048                                                                      CL**3
00049      EJECT                                                           CL**3
00050  DATA DIVISION.                                                      CL**3
00051                                                                      CL**3
00052  FILE SECTION.                                                       CL**3
00053                                                                      CL**3
00054  FD  CARD-FILE                                                       CL**3
00055      RECORDING MODE F.                                               CL**3
00056                                                                      CL**3
00057  01  CARD-RECORD.                                                    CL**3
00058      05  ER-FILE-NAME                PIC X(8).                       CL**3
00059      05  ER-ACTION                   PIC X(6).                       CL**3
00060      05  ER-NUM-MONTHS               PIC 9999.                       CL**3
00061      05  FILLER                      PIC X(62).                      CL**3
00062                                                                      CL**3
00063      EJECT                                                           CL**3
00064  FD  ECSCERT-IN                                                      CL**3
00065      RECORDING MODE F.                                               CL**3
00066                                                                      CL**3
00067  01  ECSCERT-IN-REC                    PIC X(1056).                  CL*13
00068                                                                      CL**3
00069      EJECT                                                           CL**3
00070  FD  ECSEPEC-IN                                                      CL**3
00071      RECORDING MODE F.                                               CL**3
00072                                                                      CL**3
00073  01  ECSEPEC-IN-REC                    PIC X(0325).                  CL*13
00074                                                                      CL**3
00075      EJECT                                                           CL**3
00076  FD  ECSCLMS-IN                                                      CL**3
00077      RECORDING MODE F.                                               CL**3
00078                                                                      CL**3
00079  01  ECSCLMS-IN-REC                    PIC X(0510).                  CL*13
00080                                                                      CL*13
00081                                                                      CL**3
00082      EJECT                                                           CL**3
00083  FD  ECSRESV-IN                                                      CL**3
00084      RECORDING MODE F.                                               CL**3
00085                                                                      CL**3
00086  01  ECSRESV-IN-REC                    PIC X(0510).                  CL*13
00087                                                                      CL*13
00088  FD  ELEXTR-IN                                                       CL*19
00089      RECORDING MODE F.                                               CL*19
00090                                                                      CL*19
00091  01  ELEXTR-IN-REC                    PIC X(0314).                   CL*19
00092                                                                      CL*19
00093                                                                      CL**3
00094      EJECT                                                           CL**3
00095  FD  ECSCERT-OT                                                      CL**3
00096      RECORDING MODE F.                                               CL**3
00097                                                                      CL**3
00098  01  ECSCERT-OUT-RECORD            PIC X(1056).                      CL*12
00099                                                                      CL**3
00100      EJECT                                                           CL**3
00101  FD  ECSEPEC-OT                                                      CL**3
00102      RECORDING MODE F.                                               CL**3
00103                                                                      CL**3
00104  01  ECSEPEC-OUT-RECORD            PIC X(0325).                      CL*12
00105                                                                      CL**3
00106      EJECT                                                           CL**3
00107  FD  ECSCLMS-OT                                                      CL**3
00108      RECORDING MODE F.                                               CL**3
00109                                                                      CL**3
00110  01  ECSCLMS-OUT-RECORD            PIC X(0510).                      CL*12
00111                                                                      CL**3
00112      EJECT                                                           CL**3
00113  FD  ECSRESV-OT                                                      CL**3
00114      RECORDING MODE F.                                               CL**3
00115                                                                      CL**3
00116  01  ECSRESV-OUT-RECORD            PIC X(0510).                      CL*12
00117                                                                      CL**3
00118  FD  ELEXTR-OT                                                       CL*19
00119      RECORDING MODE F.                                               CL*19
00120                                                                      CL*19
00121  01  ELEXTR-OUT-RECORD            PIC X(0314).                       CL*19
00122                                                                      CL*19
00123      EJECT                                                           CL**3
00124  FD  DISK-DATE                                                       CL**3
00125                                        COPY ELCDTEFD.                CL**3
00126                                                                      CL**3
00127      EJECT                                                           CL**3
00128  WORKING-STORAGE SECTION.                                            CL**3
00129                                                                      CL**3
00130  77  FILLER  PIC X(33)  VALUE '*********************************'.   CL**3
00131  77  FILLER  PIC X(33)  VALUE '*   ECDATAGE  WORKING STORAGE   *'.   CL**3
00132  77  FILLER  PIC X(33)  VALUE '********* VMOD=2.014 ************'.   CL**3
00133                                                                      CL**3
00134  77  PGM-SUB                    PIC S999  COMP-3 VALUE +329.         CL**3
00135                                                                      CL**3
00136                                                                      CL**3
00137  01  WS-ABEND-STORAGE.                                               CL**3
00138      12  WS-CARD-SWITCH          PIC X(3).                           CL**3
00139          88  THERE-ARE-NO-MORE-CARDS  VALUE 'YES'.                   CL**3
00140      12  WS-FILE-SWITCH          PIC X(3).                           CL**3
00141          88  THERE-ARE-NO-MORE-RECORDS VALUE 'YES'.                  CL**3
00142                                                                      CL**3
00143      12  WS-SUB                  PIC S9(4) VALUE +0 COMP.            CL**3
00144      12  WS-RETURN-CODE          PIC S9(4) VALUE +0 COMP.            CL**3
00145      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.            CL**3
00146      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.             CL**3
00147      12  WS-ZERO                 PIC S9     VALUE ZERO COMP-3.       CL**3
00148      12  WS-ABEND-CODE           PIC S9(4).                          CL**3
00149      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.                  CL**3
00150          16  WAC-1               PIC X.                              CL**3
00151          16  WAC-2               PIC X.                              CL**3
00152          16  WAC-3-4             PIC 99.                             CL**3
00153                                                                      CL**3
00154      EJECT                                                           CL**3
00155                                                                      CL**3
00156      COPY ELCDTECX.                                                  CL**3
00157                                                                      CL**3
00158                           COPY ECSCRT01.                             CL*13
00159                           COPY ECSEPC01.                             CL*13
00160                           COPY ECSEXT01.                             CL*13
00161                           COPY ELCEXTR.                              CL*19
00162      COPY ELCDTEVR.                                                  CL**3
00163                                                                      CL**3
00164      COPY ELCDATE.                                                   CL*27
00165                                                                      CL**3
00166      EJECT                                                           CL**3
00167                                                                      CL**3
00168  01  FILLER.                                                         CL**3
00169      05  WS-COMPANY-CODE             PIC S9(4)  COMP.                CL**3
00170      05  FILLER                      REDEFINES                       CL**3
00171          WS-COMPANY-CODE.                                            CL**3
00172          10  FILLER                  PIC X.                          CL**3
00173          10  WS-COMPANY              PIC X.                          CL**3
00174                                                                      CL**3
00175  01  FILLER.                                                         CL**3
00176      05  WS-FIELD-NAME               PIC X(35) VALUE SPACES.         CL**3
00177      05  X                           PIC X.                          CL**3
00178      05  ABEND-CODE                  PIC X(4).                       CL**3
00179      05  ABEND-OPTION                PIC X.                          CL**3
00180      05  OLC-REPORT-NAME             PIC X(8) VALUE 'ECDATAGE'.      CL**3
00181      05  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.              CL**3
00182      05  WS-DISPLAY-COUNT            PIC Z,ZZZ,ZZ9-.                 CL**3
00183                                                                      CL**3
00184      05  WS-FILE-ERROR-MESSAGE.                                      CL**3
00185          10  FILLER                  PIC X(24)       VALUE SPACES.   CL**3
00186          10  WS-FEM-FILE-NAME        PIC X(8).                       CL**3
00187                                                                      CL**3
00188      05  WS-RECORD-COUNT          PIC S9(7)   COMP-3 VALUE +0.       CL**3
00189      05  WS-EDITED-COUNT          PIC Z,ZZZ,ZZ9.                     CL**3
00190                                                                      CL**3
00191      EJECT                                                           CL**3
00192  PROCEDURE DIVISION.                                                 CL**3
00193                                                                      CL**3
00194  0000-DATE-CARD-READ SECTION.                                        CL**3
00195                               COPY ELCDTERX.                         CL**3
00196      EJECT                                                           CL**3
00197                                                                      CL**3
00198      PERFORM 0000-OPEN                                               CL**5
00199      PERFORM 0000-INITIALIZE                                         CL**5
00200      PERFORM 0000-PROCESS         THRU 0000-EXIT UNTIL               CL**5
00201          THERE-ARE-NO-MORE-CARDS                                     CL**3
00202                                                                      CL**3
00203      GOBACK                                                          CL**5
00204                .                                                     CL**5
00205  0000-OPEN.                                                          CL**5
00206                                                                      CL**3
00207      OPEN INPUT CARD-FILE.                                           CL**3
00208                                                                      CL**3
00209  0000-INITIALIZE.                                                    CL**5
00210                                                                      CL**3
00211      MOVE 'NO ' TO WS-CARD-SWITCH                                    CL**3
00212                    WS-FILE-SWITCH                                    CL**5
00213                                                                      CL**3
00214      PERFORM 0010-READ-ECSCARD THRU 0019-EXIT                        CL**5
00215                  .                                                   CL**5
00216  0000-PROCESS.                                                       CL**5
00217                                                                      CL**3
00218      DISPLAY ' '.                                                    CL**3
00219      DISPLAY CARD-RECORD.                                            CL**3
00220                                                                      CL**3
00221      EVALUATE ER-FILE-NAME                                           CL**3
00222         WHEN 'ECSCERT '                                              CL**3
00223            EVALUATE ER-ACTION                                        CL**3
00224               WHEN 'AGE   '                                          CL**3
00225                  MOVE 'NO ' TO WS-FILE-SWITCH                        CL**5
00226                  MOVE ZEROS TO WS-RECORD-COUNT                       CL**5
00227                  OPEN INPUT ECSCERT-IN                               CL*13
00228                  OPEN OUTPUT ECSCERT-OT                              CL*15
00229                  PERFORM 0020-READ-ECSCERT THRU 0029-EXIT            CL**5
00230                  PERFORM 0100-AGE-ECSCERT THRU 0199-EXIT             CL**3
00231                  UNTIL THERE-ARE-NO-MORE-RECORDS                     CL**3
00232                  CLOSE ECSCERT-OT                                    CL*16
00233            END-EVALUATE                                              CL**3
00234         WHEN 'ECSEPEC '                                              CL**3
00235            EVALUATE ER-ACTION                                        CL**3
00236               WHEN 'AGE   '                                          CL**3
00237                  MOVE 'NO ' TO WS-FILE-SWITCH                        CL**5
00238                  MOVE ZEROS TO WS-RECORD-COUNT                       CL**5
00239                  OPEN INPUT ECSEPEC-IN                               CL*13
00240                  OPEN OUTPUT ECSEPEC-OT                              CL*15
00241                  PERFORM 0030-READ-ECSEPEC THRU 0039-EXIT            CL**5
00242                  PERFORM 0200-AGE-ECSEPEC THRU 0299-EXIT             CL**3
00243                  UNTIL THERE-ARE-NO-MORE-RECORDS                     CL**3
00244                  CLOSE ECSEPEC-OT                                    CL*16
00245            END-EVALUATE                                              CL**3
00246         WHEN 'ECSCLMS '                                              CL**3
00247            EVALUATE ER-ACTION                                        CL**3
00248               WHEN 'AGE   '                                          CL**3
00249                  MOVE 'NO ' TO WS-FILE-SWITCH                        CL**6
00250                  MOVE ZEROS TO WS-RECORD-COUNT                       CL**6
00251                  OPEN INPUT ECSCLMS-IN                               CL*13
00252                  OPEN OUTPUT ECSCLMS-OT                              CL*15
00253                  PERFORM 0040-READ-ECSCLMS THRU 0049-EXIT            CL**6
00254                  PERFORM 0300-AGE-ECSCLMS THRU 0399-EXIT             CL**3
00255                  UNTIL THERE-ARE-NO-MORE-RECORDS                     CL**3
00256                  CLOSE ECSCLMS-OT                                    CL*16
00257            END-EVALUATE                                              CL**3
00258         WHEN 'ECSRESV '                                              CL**3
00259            EVALUATE ER-ACTION                                        CL**3
00260               WHEN 'AGE   '                                          CL**3
00261                  MOVE 'NO ' TO WS-FILE-SWITCH                        CL*13
00262                  MOVE ZEROS TO WS-RECORD-COUNT                       CL*13
00263                  OPEN INPUT ECSRESV-IN                               CL*13
00264                  OPEN OUTPUT ECSRESV-OT                              CL*15
00265                  PERFORM 0050-READ-ECSRESV THRU 0059-EXIT            CL*13
00266                  PERFORM 0400-AGE-ECSRESV THRU 0499-EXIT             CL**3
00267                  UNTIL THERE-ARE-NO-MORE-RECORDS                     CL**3
00268                  CLOSE ECSRESV-OT                                    CL*16
00269            END-EVALUATE                                              CL**3
00270         WHEN 'ELEXTR  '                                              CL*19
00271            EVALUATE ER-ACTION                                        CL*19
00272               WHEN 'AGE   '                                          CL*19
00273                  MOVE 'NO ' TO WS-FILE-SWITCH                        CL*19
00274                  MOVE ZEROS TO WS-RECORD-COUNT                       CL*19
00275                  OPEN INPUT ELEXTR-IN                                CL*19
00276                  OPEN OUTPUT ELEXTR-OT                               CL*19
00277                  PERFORM 0060-READ-ELEXTR THRU 0069-EXIT             CL*19
00278                  PERFORM 0500-AGE-ELEXTR THRU 0599-EXIT              CL*19
00279                  UNTIL THERE-ARE-NO-MORE-RECORDS                     CL*19
00280                  CLOSE ELEXTR-OT                                     CL*19
00281            END-EVALUATE                                              CL*19
00282      END-EVALUATE                                                    CL**3
00283                                                                      CL**3
00284      PERFORM 0010-READ-ECSCARD THRU 0019-EXIT                        CL**5
00285                  .                                                   CL**5
00286  0000-EXIT.                                                          CL**5
00287      EXIT.                                                           CL**5
00288 /                                                                    CL**5
00289  0010-READ-ECSCARD.                                                  CL**5
00290                                                                      CL**5
00291      READ CARD-FILE AT END                                           CL**5
00292          CLOSE CARD-FILE                                             CL**5
00293          MOVE 'YES' TO WS-CARD-SWITCH.                               CL**5
00294                                                                      CL**5
00295  0019-EXIT.                                                          CL**5
00296      EXIT.                                                           CL**5
00297                                                                      CL**5
00298  0020-READ-ECSCERT.                                                  CL**5
00299                                                                      CL**5
00300      READ ECSCERT-IN INTO CERTIFICATE-RECORD AT END                  CL*14
00301           CLOSE ECSCERT-IN                                           CL**5
00302           MOVE 'YES' TO WS-FILE-SWITCH.                              CL**5
00303                                                                      CL**5
00304      IF THERE-ARE-NO-MORE-RECORDS                                    CL**5
00305         MOVE WS-RECORD-COUNT    TO WS-EDITED-COUNT                   CL**5
00306         DISPLAY 'CERT   - RECORDS AGED ' WS-EDITED-COUNT             CL**5
00307         MOVE ZEROS TO WS-RECORD-COUNT                                CL**5
00308      END-IF                                                          CL**5
00309                       .                                              CL**5
00310  0029-EXIT.                                                          CL**5
00311      EXIT.                                                           CL**5
00312                                                                      CL**5
00313  0030-READ-ECSEPEC.                                                  CL**5
00314                                                                      CL**5
00315      READ ECSEPEC-IN INTO EP-RECORD AT END                           CL*14
00316           CLOSE ECSEPEC-IN                                           CL**5
00317           MOVE 'YES' TO WS-FILE-SWITCH.                              CL**5
00318                                                                      CL**5
00319      IF THERE-ARE-NO-MORE-RECORDS                                    CL**5
00320         MOVE WS-RECORD-COUNT    TO WS-EDITED-COUNT                   CL**5
00321         DISPLAY 'EPEC   - RECORDS AGED ' WS-EDITED-COUNT             CL**5
00322         MOVE ZEROS TO WS-RECORD-COUNT                                CL**5
00323      END-IF                                                          CL**5
00324                       .                                              CL**5
00325  0039-EXIT.                                                          CL**5
00326      EXIT.                                                           CL**5
00327                                                                      CL**5
00328  0040-READ-ECSCLMS.                                                  CL**5
00329                                                                      CL**5
00330      READ ECSCLMS-IN INTO DETAIL-EXTRACT AT END                      CL*14
00331           CLOSE ECSCLMS-IN                                           CL**5
00332           MOVE 'YES' TO WS-FILE-SWITCH.                              CL**5
00333                                                                      CL**5
00334      IF THERE-ARE-NO-MORE-RECORDS                                    CL**5
00335         MOVE WS-RECORD-COUNT    TO WS-EDITED-COUNT                   CL**5
00336         DISPLAY 'CLMS   - RECORDS AGED ' WS-EDITED-COUNT             CL**5
00337         MOVE ZEROS TO WS-RECORD-COUNT                                CL**5
00338      END-IF                                                          CL**5
00339                       .                                              CL**5
00340  0049-EXIT.                                                          CL**5
00341      EXIT.                                                           CL**5
00342                                                                      CL**5
00343  0050-READ-ECSRESV.                                                  CL**5
00344                                                                      CL**5
00345      READ ECSRESV-IN INTO DETAIL-EXTRACT AT END                      CL*14
00346           CLOSE ECSRESV-IN                                           CL**5
00347           MOVE 'YES' TO WS-FILE-SWITCH.                              CL**5
00348                                                                      CL**5
00349      IF THERE-ARE-NO-MORE-RECORDS                                    CL**5
00350         MOVE WS-RECORD-COUNT    TO WS-EDITED-COUNT                   CL**5
00351         DISPLAY 'RESV   - RECORDS AGED ' WS-EDITED-COUNT             CL**5
00352         MOVE ZEROS TO WS-RECORD-COUNT                                CL**5
00353      END-IF                                                          CL**5
00354                       .                                              CL**5
00355  0059-EXIT.                                                          CL**5
00356      EXIT.                                                           CL**5
00357                                                                      CL**5
00358  0060-READ-ELEXTR.                                                   CL*19
00359                                                                      CL*19
00360      READ ELEXTR-IN INTO REPORTS-EXTRACT-RECORD AT END               CL*19
00361           CLOSE ELEXTR-IN                                            CL*19
00362           MOVE 'YES' TO WS-FILE-SWITCH.                              CL*19
00363                                                                      CL*19
00364      IF THERE-ARE-NO-MORE-RECORDS                                    CL*19
00365         MOVE WS-RECORD-COUNT    TO WS-EDITED-COUNT                   CL*19
00366         DISPLAY 'ELEXTR - RECORDS AGED ' WS-EDITED-COUNT             CL*19
00367         MOVE ZEROS TO WS-RECORD-COUNT                                CL*19
00368      END-IF                                                          CL*19
00369                       .                                              CL*19
00370  0069-EXIT.                                                          CL*19
00371      EXIT.                                                           CL*19
00372                                                                      CL*19
00373  0100-AGE-ECSCERT.                                                   CL**3
00374                                                                      CL**3
00375      ADD +1 TO WS-RECORD-COUNT                                       CL**5
00376                                                                      CL**5
00377      IF CR-DT                 EQUAL ZEROS                            CL**3
00378         CONTINUE                                                     CL**3
00379      ELSE                                                            CL**3
00380         MOVE CR-DT        TO DC-GREG-DATE-CYMD                       CL**3
00381         MOVE 'CR-DT        ' TO WS-FIELD-NAME                        CL**3
00382         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00383                    8199-DATE-CONVERT-X                               CL**3
00384         IF NO-CONVERSION-ERROR                                       CL**3
00385            MOVE DC-GREG-DATE-CYMD TO CR-DT                           CL**3
00386         END-IF                                                       CL**3
00387      END-IF                                                          CL**3
00388                                                                      CL**3
00389      IF CR-LF-EXPIRE-DATE     EQUAL ZEROS                            CL**3
00390         CONTINUE                                                     CL**3
00391      ELSE                                                            CL**3
00392         MOVE CR-LF-EXPIRE-DATE TO DC-GREG-DATE-CYMD                  CL**3
00393         MOVE 'CR-LF-EXPIRE-DATE        ' TO WS-FIELD-NAME            CL**3
00394         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00395                    8199-DATE-CONVERT-X                               CL**3
00396         IF NO-CONVERSION-ERROR                                       CL**3
00397            MOVE DC-GREG-DATE-CYMD TO CR-LF-EXPIRE-DATE               CL**3
00398         END-IF                                                       CL**3
00399      END-IF                                                          CL**3
00400                                                                      CL**3
00401      IF CR-AH-EXPIRE-DATE     EQUAL ZEROS                            CL*23
00402         CONTINUE                                                     CL*23
00403      ELSE                                                            CL*23
00404         MOVE CR-AH-EXPIRE-DATE TO DC-GREG-DATE-CYMD                  CL*23
00405         MOVE 'CR-AH-EXPIRE-DATE        ' TO WS-FIELD-NAME            CL*23
00406         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL*23
00407                    8199-DATE-CONVERT-X                               CL*23
00408         IF NO-CONVERSION-ERROR                                       CL*23
00409            MOVE DC-GREG-DATE-CYMD TO CR-AH-EXPIRE-DATE               CL*23
00410         END-IF                                                       CL*23
00411      END-IF                                                          CL*23
00412                                                                      CL*23
00413      IF CR-LOAN-1ST-PMT-DT = ZEROS  OR  SPACES  OR  LOW-VALUES       CL*23
00414         CONTINUE                                                     CL**3
00415      ELSE                                                            CL**3
00416         MOVE CR-LOAN-1ST-PMT-DT TO DC-GREG-DATE-1-YMD                CL*23
00417         MOVE 'CR-LOAN-1ST-PMT-DT       ' TO WS-FIELD-NAME            CL*23
00418         PERFORM 8200-DATE-CONVERT-ROUTINE THRU                       CL*23
00419                    8299-DATE-CONVERT-X                               CL*23
00420         IF NO-CONVERSION-ERROR                                       CL**3
00421            MOVE DC-GREG-DATE-1-YMD  TO CR-LOAN-1ST-PMT-DT            CL*24
00422         END-IF                                                       CL**3
00423      END-IF                                                          CL**3
00424                                                                      CL**3
00425      IF CR-ENTRY-DATE         EQUAL ZEROS                            CL**3
00426         CONTINUE                                                     CL**3
00427      ELSE                                                            CL**3
00428         MOVE CR-ENTRY-DATE TO DC-GREG-DATE-CYMD                      CL**3
00429         MOVE 'CR-ENTRY-DATE        ' TO WS-FIELD-NAME                CL**3
00430         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00431                    8199-DATE-CONVERT-X                               CL**3
00432         IF NO-CONVERSION-ERROR                                       CL**3
00433            MOVE DC-GREG-DATE-CYMD TO CR-ENTRY-DATE                   CL**3
00434         END-IF                                                       CL**3
00435      END-IF                                                          CL**3
00436                                                                      CL**3
00437      IF CR-LF-CANC-DT         EQUAL ZEROS                            CL**3
00438         CONTINUE                                                     CL**3
00439      ELSE                                                            CL**3
00440         MOVE CR-LF-CANC-DT TO DC-GREG-DATE-CYMD                      CL**3
00441         MOVE 'CR-LF-CANC-DT        ' TO WS-FIELD-NAME                CL**3
00442         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00443                    8199-DATE-CONVERT-X                               CL**3
00444         IF NO-CONVERSION-ERROR                                       CL**3
00445            MOVE DC-GREG-DATE-CYMD TO CR-LF-CANC-DT                   CL**3
00446         END-IF                                                       CL**3
00447      END-IF                                                          CL**3
00448                                                                      CL**3
00449      IF CR-LF-CANCEL-EXIT-DATE EQUAL ZEROS                           CL**3
00450         CONTINUE                                                     CL**3
00451      ELSE                                                            CL**3
00452         MOVE CR-LF-CANCEL-EXIT-DATE TO DC-GREG-DATE-CYMD             CL**3
00453         MOVE 'CR-LF-CANCEL-EXIT-DATE        ' TO WS-FIELD-NAME       CL**3
00454         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00455                    8199-DATE-CONVERT-X                               CL**3
00456         IF NO-CONVERSION-ERROR                                       CL**3
00457            MOVE DC-GREG-DATE-CYMD TO CR-LF-CANCEL-EXIT-DATE          CL**3
00458         END-IF                                                       CL**3
00459      END-IF                                                          CL**3
00460                                                                      CL**3
00461      IF CR-LF-CLAIM-EXIT-DATE EQUAL ZEROS                            CL**3
00462         CONTINUE                                                     CL**3
00463      ELSE                                                            CL**3
00464         MOVE CR-LF-CLAIM-EXIT-DATE TO DC-GREG-DATE-CYMD              CL**3
00465         MOVE 'CR-LF-CLAIM-EXIT-DATE        ' TO WS-FIELD-NAME        CL**3
00466         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00467                    8199-DATE-CONVERT-X                               CL**3
00468         IF NO-CONVERSION-ERROR                                       CL**3
00469            MOVE DC-GREG-DATE-CYMD TO CR-LF-CLAIM-EXIT-DATE           CL**3
00470         END-IF                                                       CL**3
00471      END-IF                                                          CL**3
00472                                                                      CL**3
00473      IF CR-AH-CANC-DT EQUAL ZEROS                                    CL**3
00474         CONTINUE                                                     CL**3
00475      ELSE                                                            CL**3
00476         MOVE CR-AH-CANC-DT TO DC-GREG-DATE-CYMD                      CL**3
00477         MOVE 'CR-AH-CANC-DT        ' TO WS-FIELD-NAME                CL**3
00478         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00479                    8199-DATE-CONVERT-X                               CL**3
00480         IF NO-CONVERSION-ERROR                                       CL**3
00481            MOVE DC-GREG-DATE-CYMD TO CR-AH-CANC-DT                   CL**3
00482         END-IF                                                       CL**3
00483      END-IF                                                          CL**3
00484                                                                      CL**3
00485      IF CR-AH-CANCEL-EXIT-DATE EQUAL ZEROS                           CL**3
00486         CONTINUE                                                     CL**3
00487      ELSE                                                            CL**3
00488         MOVE CR-AH-CANCEL-EXIT-DATE TO DC-GREG-DATE-CYMD             CL**3
00489         MOVE 'CR-AH-CANCEL-EXIT-DATE        ' TO WS-FIELD-NAME       CL**3
00490         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00491                    8199-DATE-CONVERT-X                               CL**3
00492         IF NO-CONVERSION-ERROR                                       CL**3
00493            MOVE DC-GREG-DATE-CYMD TO CR-AH-CANCEL-EXIT-DATE          CL**3
00494         END-IF                                                       CL**3
00495      END-IF                                                          CL**3
00496                                                                      CL**3
00497      IF CR-AH-SETTLEMENT-EXIT-DATE EQUAL ZEROS                       CL**3
00498         CONTINUE                                                     CL**3
00499      ELSE                                                            CL**3
00500         MOVE CR-AH-SETTLEMENT-EXIT-DATE TO DC-GREG-DATE-CYMD         CL**3
00501         MOVE 'CR-AH-SETTLEMENT-EXIT-DATE        ' TO WS-FIELD-NAME   CL**3
00502         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**3
00503                    8199-DATE-CONVERT-X                               CL**3
00504         IF NO-CONVERSION-ERROR                                       CL**3
00505            MOVE DC-GREG-DATE-CYMD TO CR-AH-SETTLEMENT-EXIT-DATE      CL**3
00506         END-IF                                                       CL**3
00507      END-IF                                                          CL**3
00508                                                                      CL**3
00509      IF CR-DTH-DT EQUAL ZEROS                                        CL**4
00510         CONTINUE                                                     CL**4
00511      ELSE                                                            CL**4
00512         MOVE CR-DTH-DT TO DC-GREG-DATE-CYMD                          CL**4
00513         MOVE 'CR-DTH-DT        ' TO WS-FIELD-NAME                    CL**4
00514         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**4
00515                    8199-DATE-CONVERT-X                               CL**4
00516         IF NO-CONVERSION-ERROR                                       CL**4
00517            MOVE DC-GREG-DATE-CYMD TO CR-DTH-DT                       CL**4
00518         END-IF                                                       CL**4
00519      END-IF                                                          CL**4
00520                                                                      CL**4
00521      IF CR-DTH-RPT-DT = ZEROS  OR  SPACES  OR  LOW-VALUES            CL*23
00522         CONTINUE                                                     CL*23
00523      ELSE                                                            CL*23
00524         MOVE CR-DTH-RPT-DT      TO DC-GREG-DATE-1-YMD                CL*23
00525         MOVE 'CR-DTH-RPT-DT            ' TO WS-FIELD-NAME            CL*23
00526         PERFORM 8200-DATE-CONVERT-ROUTINE THRU                       CL*23
00527                    8299-DATE-CONVERT-X                               CL*23
00528         IF NO-CONVERSION-ERROR                                       CL*23
00529            MOVE DC-GREG-DATE-1-YMD   TO CR-DTH-RPT-DT                CL*24
00530         END-IF                                                       CL*23
00531      END-IF                                                          CL*23
00532                                                                      CL*23
00533      IF CR-DTH-PAY-DT EQUAL ZEROS                                    CL**4
00534         CONTINUE                                                     CL**4
00535      ELSE                                                            CL**4
00536         MOVE CR-DTH-PAY-DT TO DC-GREG-DATE-CYMD                      CL**4
00537         MOVE 'CR-DTH-PAY-DT        ' TO WS-FIELD-NAME                CL**4
00538         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**4
00539                    8199-DATE-CONVERT-X                               CL**4
00540         IF NO-CONVERSION-ERROR                                       CL**4
00541            MOVE DC-GREG-DATE-CYMD TO CR-DTH-PAY-DT                   CL**4
00542         END-IF                                                       CL**4
00543      END-IF                                                          CL**4
00544                                                                      CL**4
00545      IF CR-DIS-DT EQUAL ZEROS                                        CL**4
00546         CONTINUE                                                     CL**4
00547      ELSE                                                            CL**4
00548         MOVE CR-DIS-DT TO DC-GREG-DATE-CYMD                          CL**4
00549         MOVE 'CR-DIS-DT        ' TO WS-FIELD-NAME                    CL**4
00550         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**4
00551                    8199-DATE-CONVERT-X                               CL**4
00552         IF NO-CONVERSION-ERROR                                       CL**4
00553            MOVE DC-GREG-DATE-CYMD TO CR-DIS-DT                       CL**4
00554         END-IF                                                       CL**4
00555      END-IF                                                          CL**4
00556                                                                      CL**4
00557      IF CR-DIS-RPT-DT = ZEROS  OR  SPACES  OR  LOW-VALUES            CL*23
00558         CONTINUE                                                     CL*23
00559      ELSE                                                            CL*23
00560         MOVE CR-DIS-RPT-DT      TO DC-GREG-DATE-1-YMD                CL*23
00561         MOVE 'CR-DIS-RPT-DT            ' TO WS-FIELD-NAME            CL*23
00562         PERFORM 8200-DATE-CONVERT-ROUTINE THRU                       CL*23
00563                    8299-DATE-CONVERT-X                               CL*23
00564         IF NO-CONVERSION-ERROR                                       CL*23
00565            MOVE DC-GREG-DATE-1-YMD   TO CR-DIS-RPT-DT                CL*24
00566         END-IF                                                       CL*23
00567      END-IF                                                          CL*23
00568                                                                      CL*23
00569      IF CR-DIS-PAY-DT EQUAL ZEROS                                    CL**4
00570         CONTINUE                                                     CL**4
00571      ELSE                                                            CL**4
00572         MOVE CR-DIS-PAY-DT TO DC-GREG-DATE-CYMD                      CL**4
00573         MOVE 'CR-DIS-PAY-DT        ' TO WS-FIELD-NAME                CL**4
00574         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**4
00575                    8199-DATE-CONVERT-X                               CL**4
00576         IF NO-CONVERSION-ERROR                                       CL**4
00577            MOVE DC-GREG-DATE-CYMD TO CR-DIS-PAY-DT                   CL**4
00578         END-IF                                                       CL**4
00579      END-IF                                                          CL**4
00580                                                                      CL**4
00581      IF CR-DIS-PTO-DT EQUAL ZEROS                                    CL**4
00582         CONTINUE                                                     CL**4
00583      ELSE                                                            CL**4
00584         MOVE CR-DIS-PTO-DT TO DC-GREG-DATE-CYMD                      CL**4
00585         MOVE 'CR-DIS-PTO-DT        ' TO WS-FIELD-NAME                CL**4
00586         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**4
00587                    8199-DATE-CONVERT-X                               CL**4
00588         IF NO-CONVERSION-ERROR                                       CL**4
00589            MOVE DC-GREG-DATE-CYMD TO CR-DIS-PTO-DT                   CL**4
00590         END-IF                                                       CL**4
00591      END-IF                                                          CL**4
00592                                                                      CL**4
00593      PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB                 CL**4
00594            GREATER THAN +5                                           CL**4
00595         IF CR-DIS-INCUR-DT (WS-SUB) EQUAL ZEROS                      CL**4
00596            CONTINUE                                                  CL**4
00597         ELSE                                                         CL**4
00598            MOVE CR-DIS-INCUR-DT (WS-SUB) TO DC-GREG-DATE-CYMD        CL**4
00599            MOVE 'CR-DIS-INCUR-DT        ' TO WS-FIELD-NAME           CL**4
00600            PERFORM 8100-DATE-CONVERT-ROUTINE THRU                    CL**4
00601                       8199-DATE-CONVERT-X                            CL**4
00602            IF NO-CONVERSION-ERROR                                    CL**4
00603               MOVE DC-GREG-DATE-CYMD TO CR-DIS-INCUR-DT (WS-SUB)     CL**4
00604            END-IF                                                    CL**4
00605         END-IF                                                       CL**4
00606      END-PERFORM                                                     CL**4
00607                                                                      CL**4
00608      IF CR-LAST-ADD-ON-DT EQUAL LOW-VALUES OR SPACES OR ZEROS        CL**5
00609         CONTINUE                                                     CL**1
00610      ELSE                                                            CL**1
00611         MOVE CR-LAST-ADD-ON-DT    TO DC-BIN-DATE-1                   CL**5
00612         MOVE 'CR-LAST-ADD-ON-DT'  TO WS-FIELD-NAME                   CL**5
00613         PERFORM 8000-DATE-CONVERT-ROUTINE THRU                       CL**1
00614                    8099-DATE-CONVERT-X                               CL**1
00615         IF NO-CONVERSION-ERROR                                       CL**1
00616            MOVE DC-BIN-DATE-2     TO CR-LAST-ADD-ON-DT               CL**5
00617         END-IF                                                       CL**1
00618      END-IF                                                          CL**1
00619                                                                      CL**3
00620      WRITE ECSCERT-OUT-RECORD FROM CERTIFICATE-RECORD                CL**9
00621                                                                      CL**5
00622      PERFORM 0020-READ-ECSCERT THRU 0029-EXIT                        CL**9
00623                                                                      CL**3
00624                 .                                                    CL**9
00625  0199-EXIT.                                                          CL**5
00626                                                                      CL**3
00627      EXIT.                                                           CL**3
00628                                                                      CL**3
00629      EJECT                                                           CL**3
00630                                                                      CL**3
00631  0200-AGE-ECSEPEC.                                                   CL**5
00632                                                                      CL**5
00633      ADD +1 TO WS-RECORD-COUNT                                       CL**5
00634                                                                      CL**5
00635      IF EP-EXP-DTE            EQUAL ZEROS  OR  99999999999           CL*26
00636         CONTINUE                                                     CL**5
00637      ELSE                                                            CL**5
00638         MOVE EP-EXP-DTE   TO DC-GREG-DATE-CYMD                       CL**5
00639         MOVE 'EP-EXP-DTE        ' TO WS-FIELD-NAME                   CL**5
00640         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**5
00641                    8199-DATE-CONVERT-X                               CL**5
00642         IF NO-CONVERSION-ERROR                                       CL**5
00643            MOVE DC-GREG-DATE-CYMD TO EP-EXP-DTE                      CL**5
00644         END-IF                                                       CL**5
00645      END-IF                                                          CL**5
00646                                                                      CL**5
00647      IF EP-EFF-DTE            EQUAL ZEROS                            CL**5
00648         CONTINUE                                                     CL**5
00649      ELSE                                                            CL**5
00650         MOVE EP-EFF-DTE   TO DC-GREG-DATE-CYMD                       CL**5
00651         MOVE 'EP-EFF-DTE        ' TO WS-FIELD-NAME                   CL**5
00652         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**5
00653                    8199-DATE-CONVERT-X                               CL**5
00654         IF NO-CONVERSION-ERROR                                       CL**5
00655            MOVE DC-GREG-DATE-CYMD TO EP-EFF-DTE                      CL**5
00656         END-IF                                                       CL**5
00657      END-IF                                                          CL**5
00658                                                                      CL**5
00659      IF EP-RECORD-ID EQUAL 'EP'                                      CL**5
00660         IF EP-HI-COV-DT          EQUAL ZEROS                         CL**5
00661            CONTINUE                                                  CL**5
00662         ELSE                                                         CL**5
00663            MOVE EP-HI-COV-DT TO DC-GREG-DATE-CYMD                    CL**5
00664            MOVE 'EP-HI-COV-DT        ' TO WS-FIELD-NAME              CL**5
00665            PERFORM 8100-DATE-CONVERT-ROUTINE THRU                    CL**5
00666                       8199-DATE-CONVERT-X                            CL**5
00667            IF NO-CONVERSION-ERROR                                    CL**5
00668               MOVE DC-GREG-DATE-CYMD TO EP-HI-COV-DT                 CL**5
00669            END-IF                                                    CL**5
00670         END-IF                                                       CL**5
00671                                                                      CL**5
00672         IF EP-HI-CERT            EQUAL ZEROS                         CL**5
00673            CONTINUE                                                  CL**5
00674         ELSE                                                         CL**5
00675            MOVE EP-HI-CERT TO DC-GREG-DATE-CYMD                      CL**5
00676            MOVE 'EP-HI-CERT        ' TO WS-FIELD-NAME                CL**5
00677            PERFORM 8100-DATE-CONVERT-ROUTINE THRU                    CL**5
00678                       8199-DATE-CONVERT-X                            CL**5
00679            IF NO-CONVERSION-ERROR                                    CL**5
00680               MOVE DC-GREG-DATE-CYMD TO EP-HI-CERT                   CL**5
00681            END-IF                                                    CL**5
00682         END-IF                                                       CL**5
00683                                                                      CL**5
00684         IF EP-LO-CERT            EQUAL ZEROS                         CL**5
00685            CONTINUE                                                  CL**5
00686         ELSE                                                         CL**5
00687            MOVE EP-LO-CERT TO DC-GREG-DATE-CYMD                      CL**5
00688            MOVE 'EP-LO-CERT        ' TO WS-FIELD-NAME                CL**5
00689            PERFORM 8100-DATE-CONVERT-ROUTINE THRU                    CL**5
00690                       8199-DATE-CONVERT-X                            CL**5
00691            IF NO-CONVERSION-ERROR                                    CL**5
00692               MOVE DC-GREG-DATE-CYMD TO EP-LO-CERT                   CL**5
00693            END-IF                                                    CL**5
00694         END-IF                                                       CL**5
00695      END-IF                                                          CL**5
00696                                                                      CL**5
00697      IF EP-RUN-DTE            EQUAL ZEROS                            CL**5
00698         CONTINUE                                                     CL**5
00699      ELSE                                                            CL**5
00700         MOVE EP-RUN-DTE TO DC-GREG-DATE-CYMD                         CL**5
00701         MOVE 'EP-RUN-DTE        ' TO WS-FIELD-NAME                   CL**5
00702         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**5
00703                    8199-DATE-CONVERT-X                               CL**5
00704         IF NO-CONVERSION-ERROR                                       CL**5
00705            MOVE DC-GREG-DATE-CYMD TO EP-RUN-DTE                      CL**5
00706         END-IF                                                       CL**5
00707      END-IF                                                          CL**5
00708                                                                      CL**5
00709      WRITE ECSEPEC-OUT-RECORD FROM EP-RECORD                         CL**9
00710                                                                      CL**5
00711      PERFORM 0030-READ-ECSEPEC THRU 0039-EXIT                        CL**9
00712                                                                      CL**5
00713                      .                                               CL**9
00714  0299-EXIT.                                                          CL**5
00715                                                                      CL**5
00716      EXIT.                                                           CL**5
00717                                                                      CL**5
00718      EJECT                                                           CL**5
00719                                                                      CL**5
00720  0300-AGE-ECSCLMS.                                                   CL**9
00721                                                                      CL**6
00722      ADD +1 TO WS-RECORD-COUNT                                       CL**6
00723                                                                      CL**6
00724      IF DE-EFF                EQUAL ZEROS                            CL**9
00725         CONTINUE                                                     CL**6
00726      ELSE                                                            CL**6
00727         MOVE DE-EFF       TO DC-GREG-DATE-CYMD                       CL**9
00728         MOVE 'DE-EFF        ' TO WS-FIELD-NAME                       CL**9
00729         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**6
00730                    8199-DATE-CONVERT-X                               CL**6
00731         IF NO-CONVERSION-ERROR                                       CL**6
00732            MOVE DC-GREG-DATE-CYMD TO DE-EFF                          CL**9
00733         END-IF                                                       CL**6
00734      END-IF                                                          CL**6
00735                                                                      CL**6
00736      IF DE-LF-CANC-DTE        EQUAL ZEROS                            CL**9
00737         CONTINUE                                                     CL**9
00738      ELSE                                                            CL**9
00739         MOVE DE-LF-CANC-DTE TO DC-GREG-DATE-CYMD                     CL**9
00740         MOVE 'DE-LF-CANC-DTE        ' TO WS-FIELD-NAME               CL**9
00741         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00742                    8199-DATE-CONVERT-X                               CL**9
00743         IF NO-CONVERSION-ERROR                                       CL**9
00744            MOVE DC-GREG-DATE-CYMD TO DE-LF-CANC-DTE                  CL**9
00745         END-IF                                                       CL**9
00746      END-IF                                                          CL**9
00747                                                                      CL**9
00748      IF DE-LF-CANC-EXIT-DT    EQUAL ZEROS                            CL**9
00749         CONTINUE                                                     CL**9
00750      ELSE                                                            CL**9
00751         MOVE DE-LF-CANC-EXIT-DT TO DC-GREG-DATE-CYMD                 CL**9
00752         MOVE 'DE-LF-CANC-EXIT-DT        ' TO WS-FIELD-NAME           CL**9
00753         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00754                    8199-DATE-CONVERT-X                               CL**9
00755         IF NO-CONVERSION-ERROR                                       CL**9
00756            MOVE DC-GREG-DATE-CYMD TO DE-LF-CANC-EXIT-DT              CL**9
00757         END-IF                                                       CL**9
00758      END-IF                                                          CL**9
00759                                                                      CL**9
00760      IF DE-AH-CANC-DTE        EQUAL ZEROS                            CL**9
00761         CONTINUE                                                     CL**9
00762      ELSE                                                            CL**9
00763         MOVE DE-AH-CANC-DTE TO DC-GREG-DATE-CYMD                     CL**9
00764         MOVE 'DE-AH-CANC-DTE        ' TO WS-FIELD-NAME               CL**9
00765         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00766                    8199-DATE-CONVERT-X                               CL**9
00767         IF NO-CONVERSION-ERROR                                       CL**9
00768            MOVE DC-GREG-DATE-CYMD TO DE-AH-CANC-DTE                  CL**9
00769         END-IF                                                       CL**9
00770      END-IF                                                          CL**9
00771                                                                      CL**9
00772      IF DE-AH-CANC-EXIT-DT    EQUAL ZEROS                            CL**9
00773         CONTINUE                                                     CL**9
00774      ELSE                                                            CL**9
00775         MOVE DE-AH-CANC-EXIT-DT TO DC-GREG-DATE-CYMD                 CL**9
00776         MOVE 'DE-AH-CANC-EXIT-DT        ' TO WS-FIELD-NAME           CL**9
00777         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00778                    8199-DATE-CONVERT-X                               CL**9
00779         IF NO-CONVERSION-ERROR                                       CL**9
00780            MOVE DC-GREG-DATE-CYMD TO DE-AH-CANC-EXIT-DT              CL**9
00781         END-IF                                                       CL**9
00782      END-IF                                                          CL**9
00783                                                                      CL**9
00784      IF DE-ENTRY-DTE          EQUAL ZEROS                            CL**9
00785         CONTINUE                                                     CL**9
00786      ELSE                                                            CL**9
00787         MOVE DE-ENTRY-DTE TO DC-GREG-DATE-CYMD                       CL**9
00788         MOVE 'DE-ENTRY-DTE        ' TO WS-FIELD-NAME                 CL**9
00789         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00790                    8199-DATE-CONVERT-X                               CL**9
00791         IF NO-CONVERSION-ERROR                                       CL**9
00792            MOVE DC-GREG-DATE-CYMD TO DE-ENTRY-DTE                    CL**9
00793         END-IF                                                       CL**9
00794      END-IF                                                          CL**9
00795                                                                      CL**9
00796      IF DE-INCUR              EQUAL ZEROS                            CL**9
00797         CONTINUE                                                     CL**9
00798      ELSE                                                            CL**9
00799         MOVE DE-INCUR TO DC-GREG-DATE-CYMD                           CL**9
00800         MOVE 'DE-INCUR        ' TO WS-FIELD-NAME                     CL**9
00801         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00802                    8199-DATE-CONVERT-X                               CL**9
00803         IF NO-CONVERSION-ERROR                                       CL**9
00804            MOVE DC-GREG-DATE-CYMD TO DE-INCUR                        CL**9
00805         END-IF                                                       CL**9
00806      END-IF                                                          CL**9
00807                                                                      CL**9
00808      IF DE-REPORTED = ZEROS  OR  SPACES  OR  LOW-VALUES              CL*25
00809         CONTINUE                                                     CL*25
00810      ELSE                                                            CL*25
00811         MOVE DE-REPORTED        TO DC-GREG-DATE-1-YMD                CL*25
00812         MOVE 'DE-REPORTED              ' TO WS-FIELD-NAME            CL*25
00813         PERFORM 8200-DATE-CONVERT-ROUTINE THRU                       CL*25
00814                    8299-DATE-CONVERT-X                               CL*25
00815         IF NO-CONVERSION-ERROR                                       CL*25
00816            MOVE DC-GREG-DATE-1-YMD   TO DE-REPORTED                  CL*25
00817         END-IF                                                       CL*25
00818      END-IF                                                          CL*25
00819                                                                      CL*25
00820      IF DE-PAY                EQUAL ZEROS                            CL**9
00821         CONTINUE                                                     CL**9
00822      ELSE                                                            CL**9
00823         MOVE DE-PAY TO DC-GREG-DATE-CYMD                             CL**9
00824         MOVE 'DE-PAY        ' TO WS-FIELD-NAME                       CL**9
00825         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00826                    8199-DATE-CONVERT-X                               CL**9
00827         IF NO-CONVERSION-ERROR                                       CL**9
00828            MOVE DC-GREG-DATE-CYMD TO DE-PAY                          CL**9
00829         END-IF                                                       CL**9
00830      END-IF                                                          CL**9
00831                                                                      CL**9
00832      IF DE-PAID-TO            EQUAL ZEROS                            CL**9
00833         CONTINUE                                                     CL**9
00834      ELSE                                                            CL**9
00835         MOVE DE-PAID-TO TO DC-GREG-DATE-CYMD                         CL**9
00836         MOVE 'DE-PAID-TO        ' TO WS-FIELD-NAME                   CL**9
00837         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00838                    8199-DATE-CONVERT-X                               CL**9
00839         IF NO-CONVERSION-ERROR                                       CL**9
00840            MOVE DC-GREG-DATE-CYMD TO DE-PAID-TO                      CL**9
00841         END-IF                                                       CL**9
00842      END-IF                                                          CL**9
00843                                                                      CL**9
00844      IF DE-ACC-EXP-DTE        EQUAL ZEROS OR 99999999999             CL*26
00845         CONTINUE                                                     CL**9
00846      ELSE                                                            CL**9
00847         MOVE DE-ACC-EXP-DTE TO DC-GREG-DATE-CYMD                     CL**9
00848         MOVE 'DE-ACC-EXP-DTE        ' TO WS-FIELD-NAME               CL**9
00849         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00850                    8199-DATE-CONVERT-X                               CL**9
00851         IF NO-CONVERSION-ERROR                                       CL**9
00852            MOVE DC-GREG-DATE-CYMD TO DE-ACC-EXP-DTE                  CL**9
00853         END-IF                                                       CL**9
00854      END-IF                                                          CL**9
00855                                                                      CL**9
00856      IF DE-ACC-EFF-DTE        EQUAL ZEROS                            CL**9
00857         CONTINUE                                                     CL**9
00858      ELSE                                                            CL**9
00859         MOVE DE-ACC-EFF-DTE TO DC-GREG-DATE-CYMD                     CL**9
00860         MOVE 'DE-ACC-EFF-DTE        ' TO WS-FIELD-NAME               CL**9
00861         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00862                    8199-DATE-CONVERT-X                               CL**9
00863         IF NO-CONVERSION-ERROR                                       CL**9
00864            MOVE DC-GREG-DATE-CYMD TO DE-ACC-EFF-DTE                  CL**9
00865         END-IF                                                       CL**9
00866      END-IF                                                          CL**9
00867                                                                      CL**9
00868      IF DE-CLM-PROC-DT        EQUAL ZEROS                            CL**9
00869         CONTINUE                                                     CL**9
00870      ELSE                                                            CL**9
00871         MOVE DE-CLM-PROC-DT TO DC-GREG-DATE-CYMD                     CL**9
00872         MOVE 'DE-CLM-PROC-DT        ' TO WS-FIELD-NAME               CL**9
00873         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00874                    8199-DATE-CONVERT-X                               CL**9
00875         IF NO-CONVERSION-ERROR                                       CL**9
00876            MOVE DC-GREG-DATE-CYMD TO DE-CLM-PROC-DT                  CL**9
00877         END-IF                                                       CL**9
00878      END-IF                                                          CL**9
00879                                                                      CL**9
00880                                                                      CL**6
00881      WRITE ECSCLMS-OUT-RECORD FROM DETAIL-EXTRACT                    CL**9
00882                                                                      CL**6
00883      PERFORM 0040-READ-ECSCLMS THRU 0049-EXIT                        CL*14
00884                                                                      CL**6
00885               .                                                      CL**9
00886  0399-EXIT.                                                          CL**9
00887                                                                      CL**6
00888      EXIT.                                                           CL**6
00889                                                                      CL**6
00890      EJECT                                                           CL**6
00891                                                                      CL**6
00892  0400-AGE-ECSRESV.                                                   CL**9
00893                                                                      CL**9
00894      ADD +1 TO WS-RECORD-COUNT                                       CL**9
00895                                                                      CL**9
00896      IF DE-EFF                EQUAL ZEROS                            CL**9
00897         CONTINUE                                                     CL**9
00898      ELSE                                                            CL**9
00899         MOVE DE-EFF       TO DC-GREG-DATE-CYMD                       CL**9
00900         MOVE 'DE-EFF        ' TO WS-FIELD-NAME                       CL**9
00901         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00902                    8199-DATE-CONVERT-X                               CL**9
00903         IF NO-CONVERSION-ERROR                                       CL**9
00904            MOVE DC-GREG-DATE-CYMD TO DE-EFF                          CL**9
00905         END-IF                                                       CL**9
00906      END-IF                                                          CL**9
00907                                                                      CL**9
00908      IF DE-LF-CANC-DTE        EQUAL ZEROS                            CL**9
00909         CONTINUE                                                     CL**9
00910      ELSE                                                            CL**9
00911         MOVE DE-LF-CANC-DTE TO DC-GREG-DATE-CYMD                     CL**9
00912         MOVE 'DE-LF-CANC-DTE        ' TO WS-FIELD-NAME               CL**9
00913         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00914                    8199-DATE-CONVERT-X                               CL**9
00915         IF NO-CONVERSION-ERROR                                       CL**9
00916            MOVE DC-GREG-DATE-CYMD TO DE-LF-CANC-DTE                  CL**9
00917         END-IF                                                       CL**9
00918      END-IF                                                          CL**9
00919                                                                      CL**9
00920      IF DE-LF-CANC-EXIT-DT    EQUAL ZEROS                            CL**9
00921         CONTINUE                                                     CL**9
00922      ELSE                                                            CL**9
00923         MOVE DE-LF-CANC-EXIT-DT TO DC-GREG-DATE-CYMD                 CL**9
00924         MOVE 'DE-LF-CANC-EXIT-DT        ' TO WS-FIELD-NAME           CL**9
00925         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00926                    8199-DATE-CONVERT-X                               CL**9
00927         IF NO-CONVERSION-ERROR                                       CL**9
00928            MOVE DC-GREG-DATE-CYMD TO DE-LF-CANC-EXIT-DT              CL**9
00929         END-IF                                                       CL**9
00930      END-IF                                                          CL**9
00931                                                                      CL**9
00932      IF DE-AH-CANC-DTE        EQUAL ZEROS                            CL**9
00933         CONTINUE                                                     CL**9
00934      ELSE                                                            CL**9
00935         MOVE DE-AH-CANC-DTE TO DC-GREG-DATE-CYMD                     CL**9
00936         MOVE 'DE-AH-CANC-DTE        ' TO WS-FIELD-NAME               CL**9
00937         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00938                    8199-DATE-CONVERT-X                               CL**9
00939         IF NO-CONVERSION-ERROR                                       CL**9
00940            MOVE DC-GREG-DATE-CYMD TO DE-AH-CANC-DTE                  CL**9
00941         END-IF                                                       CL**9
00942      END-IF                                                          CL**9
00943                                                                      CL**9
00944      IF DE-AH-CANC-EXIT-DT    EQUAL ZEROS                            CL**9
00945         CONTINUE                                                     CL**9
00946      ELSE                                                            CL**9
00947         MOVE DE-AH-CANC-EXIT-DT TO DC-GREG-DATE-CYMD                 CL**9
00948         MOVE 'DE-AH-CANC-EXIT-DT        ' TO WS-FIELD-NAME           CL**9
00949         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00950                    8199-DATE-CONVERT-X                               CL**9
00951         IF NO-CONVERSION-ERROR                                       CL**9
00952            MOVE DC-GREG-DATE-CYMD TO DE-AH-CANC-EXIT-DT              CL**9
00953         END-IF                                                       CL**9
00954      END-IF                                                          CL**9
00955                                                                      CL**9
00956      IF DE-ENTRY-DTE          EQUAL ZEROS                            CL**9
00957         CONTINUE                                                     CL**9
00958      ELSE                                                            CL**9
00959         MOVE DE-ENTRY-DTE TO DC-GREG-DATE-CYMD                       CL**9
00960         MOVE 'DE-ENTRY-DTE        ' TO WS-FIELD-NAME                 CL**9
00961         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00962                    8199-DATE-CONVERT-X                               CL**9
00963         IF NO-CONVERSION-ERROR                                       CL**9
00964            MOVE DC-GREG-DATE-CYMD TO DE-ENTRY-DTE                    CL**9
00965         END-IF                                                       CL**9
00966      END-IF                                                          CL**9
00967                                                                      CL**9
00968      IF DE-RSV-INCUR          EQUAL ZEROS                            CL*10
00969         CONTINUE                                                     CL**9
00970      ELSE                                                            CL**9
00971         MOVE DE-RSV-INCUR TO DC-GREG-DATE-CYMD                       CL*10
00972         MOVE 'DE-RSV-INCUR        ' TO WS-FIELD-NAME                 CL*10
00973         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00974                    8199-DATE-CONVERT-X                               CL**9
00975         IF NO-CONVERSION-ERROR                                       CL**9
00976            MOVE DC-GREG-DATE-CYMD TO DE-RSV-INCUR                    CL*10
00977         END-IF                                                       CL**9
00978      END-IF                                                          CL**9
00979                                                                      CL**9
00980      IF DE-RSV-REPORTED = ZEROS  OR  SPACES  OR  LOW-VALUES          CL*25
00981         CONTINUE                                                     CL*25
00982      ELSE                                                            CL*25
00983         MOVE DE-RSV-REPORTED    TO DC-GREG-DATE-1-YMD                CL*25
00984         MOVE 'DE-RSV-REPORTED          ' TO WS-FIELD-NAME            CL*25
00985         PERFORM 8200-DATE-CONVERT-ROUTINE THRU                       CL*25
00986                    8299-DATE-CONVERT-X                               CL*25
00987         IF NO-CONVERSION-ERROR                                       CL*25
00988            MOVE DC-GREG-DATE-1-YMD   TO DE-RSV-REPORTED              CL*25
00989         END-IF                                                       CL*25
00990      END-IF                                                          CL*25
00991                                                                      CL*25
00992      IF DE-RSV-PAYTO          EQUAL ZEROS                            CL*10
00993         CONTINUE                                                     CL**9
00994      ELSE                                                            CL**9
00995         MOVE DE-RSV-PAYTO TO DC-GREG-DATE-CYMD                       CL*10
00996         MOVE 'DE-RSV-PAYTO        ' TO WS-FIELD-NAME                 CL*10
00997         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
00998                    8199-DATE-CONVERT-X                               CL**9
00999         IF NO-CONVERSION-ERROR                                       CL**9
01000            MOVE DC-GREG-DATE-CYMD TO DE-RSV-PAYTO                    CL*10
01001         END-IF                                                       CL**9
01002      END-IF                                                          CL**9
01003                                                                      CL**9
01004      IF DE-ACC-EXP-DTE-RSV    EQUAL ZEROS OR 99999999999             CL*26
01005         CONTINUE                                                     CL**9
01006      ELSE                                                            CL**9
01007         MOVE DE-ACC-EXP-DTE-RSV TO DC-GREG-DATE-CYMD                 CL*10
01008         MOVE 'DE-ACC-EXP-DTE-RSV        ' TO WS-FIELD-NAME           CL*10
01009         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
01010                    8199-DATE-CONVERT-X                               CL**9
01011         IF NO-CONVERSION-ERROR                                       CL**9
01012            MOVE DC-GREG-DATE-CYMD TO DE-ACC-EXP-DTE-RSV              CL*10
01013         END-IF                                                       CL**9
01014      END-IF                                                          CL**9
01015                                                                      CL**9
01016      IF DE-ACC-EFF-DTE-RSV    EQUAL ZEROS                            CL*10
01017         CONTINUE                                                     CL**9
01018      ELSE                                                            CL**9
01019         MOVE DE-ACC-EFF-DTE-RSV TO DC-GREG-DATE-CYMD                 CL*10
01020         MOVE 'DE-ACC-EFF-DTE-RSV        ' TO WS-FIELD-NAME           CL*10
01021         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
01022                    8199-DATE-CONVERT-X                               CL**9
01023         IF NO-CONVERSION-ERROR                                       CL**9
01024            MOVE DC-GREG-DATE-CYMD TO DE-ACC-EFF-DTE-RSV              CL*10
01025         END-IF                                                       CL**9
01026      END-IF                                                          CL**9
01027                                                                      CL**9
01028      IF DE-RSV-PROC-DT        EQUAL ZEROS                            CL*10
01029         CONTINUE                                                     CL**9
01030      ELSE                                                            CL**9
01031         MOVE DE-RSV-PROC-DT TO DC-GREG-DATE-CYMD                     CL*10
01032         MOVE 'DE-RSV-PROC-DT        ' TO WS-FIELD-NAME               CL*10
01033         PERFORM 8100-DATE-CONVERT-ROUTINE THRU                       CL**9
01034                    8199-DATE-CONVERT-X                               CL**9
01035         IF NO-CONVERSION-ERROR                                       CL**9
01036            MOVE DC-GREG-DATE-CYMD TO DE-RSV-PROC-DT                  CL*10
01037         END-IF                                                       CL**9
01038      END-IF                                                          CL**9
01039                                                                      CL**9
01040      WRITE ECSRESV-OUT-RECORD FROM DETAIL-EXTRACT                    CL*10
01041                                                                      CL**9
01042      PERFORM 0050-READ-ECSRESV THRU 0059-EXIT                        CL*14
01043                                                                      CL**9
01044               .                                                      CL**9
01045  0499-EXIT.                                                          CL*10
01046                                                                      CL**9
01047      EXIT.                                                           CL**9
01048                                                                      CL**9
01049  0500-AGE-ELEXTR.                                                    CL*19
01050                                                                      CL*19
01051      ADD +1 TO WS-RECORD-COUNT                                       CL*19
01052                                                                      CL*19
01053      EVALUATE EX-EXTRACT-CODE                                        CL*19
01054         WHEN 'A'                                                     CL*19
01055           EVALUATE EX-RECORD-TYPE                                    CL*19
01056             WHEN 'A'                                                 CL*19
01057               IF EX-AA-CERT-EFF-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01058                                       OR SPACES                      CL*19
01059                  CONTINUE                                            CL*19
01060               ELSE                                                   CL*19
01061                  MOVE EX-AA-CERT-EFF-DT  TO DC-BIN-DATE-1            CL*19
01062                  MOVE 'EX-AA-CERT-EFF-DT' TO WS-FIELD-NAME           CL*19
01063                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01064                             8099-DATE-CONVERT-X                      CL*19
01065                  IF NO-CONVERSION-ERROR                              CL*19
01066                     MOVE DC-BIN-DATE-2     TO EX-AA-CERT-EFF-DT      CL*19
01067                  END-IF                                              CL*19
01068               END-IF                                                 CL*19
01069               IF EX-AA-INSURED-BIRTH-DT EQUAL LOW-VALUES OR ZEROS    CL*19
01070                                       OR SPACES                      CL*19
01071                  CONTINUE                                            CL*19
01072               ELSE                                                   CL*19
01073                  MOVE EX-AA-INSURED-BIRTH-DT TO DC-BIN-DATE-1        CL*19
01074                  MOVE 'EX-AA-INSURED-BIRTH-DT' TO WS-FIELD-NAME      CL*19
01075                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01076                             8099-DATE-CONVERT-X                      CL*19
01077                  IF NO-CONVERSION-ERROR                              CL*19
01078                     MOVE DC-BIN-DATE-2     TO                        CL*21
01079                                        EX-AA-INSURED-BIRTH-DT        CL*21
01080                  END-IF                                              CL*19
01081               END-IF                                                 CL*19
01082               IF EX-AA-INCURRED-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01083                                       OR SPACES                      CL*19
01084                  CONTINUE                                            CL*19
01085               ELSE                                                   CL*19
01086                  MOVE EX-AA-INCURRED-DT TO DC-BIN-DATE-1             CL*19
01087                  MOVE 'EX-AA-INCURRED-DT' TO WS-FIELD-NAME           CL*19
01088                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01089                             8099-DATE-CONVERT-X                      CL*19
01090                  IF NO-CONVERSION-ERROR                              CL*19
01091                     MOVE DC-BIN-DATE-2     TO EX-AA-INCURRED-DT      CL*19
01092                  END-IF                                              CL*19
01093               END-IF                                                 CL*19
01094               IF EX-AA-REPORTED-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01095                                       OR SPACES                      CL*19
01096                  CONTINUE                                            CL*19
01097               ELSE                                                   CL*19
01098                  MOVE EX-AA-REPORTED-DT TO DC-BIN-DATE-1             CL*19
01099                  MOVE 'EX-AA-REPORTED-DT' TO WS-FIELD-NAME           CL*19
01100                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01101                             8099-DATE-CONVERT-X                      CL*19
01102                  IF NO-CONVERSION-ERROR                              CL*19
01103                     MOVE DC-BIN-DATE-2     TO EX-AA-REPORTED-DT      CL*19
01104                  END-IF                                              CL*19
01105               END-IF                                                 CL*19
01106               IF EX-AA-PAID-THRU-DT EQUAL LOW-VALUES OR ZEROS        CL*19
01107                                       OR SPACES                      CL*19
01108                  CONTINUE                                            CL*19
01109               ELSE                                                   CL*19
01110                  MOVE EX-AA-PAID-THRU-DT TO DC-BIN-DATE-1            CL*19
01111                  MOVE 'EX-AA-PAID-THRU-DT' TO WS-FIELD-NAME          CL*19
01112                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01113                             8099-DATE-CONVERT-X                      CL*19
01114                  IF NO-CONVERSION-ERROR                              CL*19
01115                     MOVE DC-BIN-DATE-2     TO EX-AA-PAID-THRU-DT     CL*19
01116                  END-IF                                              CL*19
01117               END-IF                                                 CL*19
01118               IF EX-AA-LAST-MAINT-DT EQUAL LOW-VALUES OR ZEROS       CL*19
01119                                       OR SPACES                      CL*19
01120                  CONTINUE                                            CL*19
01121               ELSE                                                   CL*19
01122                  MOVE EX-AA-LAST-MAINT-DT TO DC-BIN-DATE-1           CL*19
01123                  MOVE 'EX-AA-LAST-MAINT-DT' TO WS-FIELD-NAME         CL*19
01124                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01125                             8099-DATE-CONVERT-X                      CL*19
01126                  IF NO-CONVERSION-ERROR                              CL*19
01127                     MOVE DC-BIN-DATE-2     TO EX-AA-LAST-MAINT-DT    CL*19
01128                  END-IF                                              CL*19
01129               END-IF                                                 CL*19
01130               IF EX-AA-PROJECT-CLOSE-DT EQUAL LOW-VALUES OR ZEROS    CL*19
01131                                       OR SPACES                      CL*19
01132                  CONTINUE                                            CL*19
01133               ELSE                                                   CL*19
01134                  MOVE EX-AA-PROJECT-CLOSE-DT TO DC-BIN-DATE-1        CL*19
01135                  MOVE 'EX-AA-PROJECT-CLOSE-DT' TO WS-FIELD-NAME      CL*19
01136                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01137                             8099-DATE-CONVERT-X                      CL*19
01138                  IF NO-CONVERSION-ERROR                              CL*19
01139                     MOVE DC-BIN-DATE-2     TO                        CL*21
01140                                      EX-AA-PROJECT-CLOSE-DT          CL*21
01141                  END-IF                                              CL*19
01142               END-IF                                                 CL*19
01143               IF EX-AA-LAST-PMT-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01144                                       OR SPACES                      CL*19
01145                  CONTINUE                                            CL*19
01146               ELSE                                                   CL*19
01147                  MOVE EX-AA-LAST-PMT-DT TO DC-BIN-DATE-1             CL*19
01148                  MOVE 'EX-AA-LAST-PMT-DT' TO WS-FIELD-NAME           CL*19
01149                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01150                             8099-DATE-CONVERT-X                      CL*19
01151                  IF NO-CONVERSION-ERROR                              CL*19
01152                     MOVE DC-BIN-DATE-2     TO EX-AA-LAST-PMT-DT      CL*19
01153                  END-IF                                              CL*19
01154               END-IF                                                 CL*19
01155             WHEN 'B'                                                 CL*19
01156               IF EX-AB-CERT-EFF-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01157                                       OR SPACES                      CL*19
01158                  CONTINUE                                            CL*19
01159               ELSE                                                   CL*19
01160                  MOVE EX-AB-CERT-EFF-DT TO DC-BIN-DATE-1             CL*19
01161                  MOVE 'EX-AB-CERT-EFF-DT' TO WS-FIELD-NAME           CL*19
01162                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01163                             8099-DATE-CONVERT-X                      CL*19
01164                  IF NO-CONVERSION-ERROR                              CL*19
01165                     MOVE DC-BIN-DATE-2     TO EX-AB-CERT-EFF-DT      CL*19
01166                  END-IF                                              CL*19
01167               END-IF                                                 CL*19
01168               IF EX-AB-PAID-FROM-DT EQUAL LOW-VALUES OR ZEROS        CL*19
01169                                       OR SPACES                      CL*19
01170                  CONTINUE                                            CL*19
01171               ELSE                                                   CL*19
01172                  MOVE EX-AB-PAID-FROM-DT TO DC-BIN-DATE-1            CL*19
01173                  MOVE 'EX-AB-PAID-FROM-DT' TO WS-FIELD-NAME          CL*19
01174                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01175                             8099-DATE-CONVERT-X                      CL*19
01176                  IF NO-CONVERSION-ERROR                              CL*19
01177                     MOVE DC-BIN-DATE-2     TO EX-AB-PAID-FROM-DT     CL*19
01178                  END-IF                                              CL*19
01179               END-IF                                                 CL*19
01180               IF EX-AB-PAID-THRU-DT EQUAL LOW-VALUES OR ZEROS        CL*19
01181                                       OR SPACES                      CL*19
01182                  CONTINUE                                            CL*19
01183               ELSE                                                   CL*19
01184                  MOVE EX-AB-PAID-THRU-DT TO DC-BIN-DATE-1            CL*19
01185                  MOVE 'EX-AB-PAID-THRU-DT' TO WS-FIELD-NAME          CL*19
01186                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01187                             8099-DATE-CONVERT-X                      CL*19
01188                  IF NO-CONVERSION-ERROR                              CL*19
01189                     MOVE DC-BIN-DATE-2     TO EX-AB-PAID-THRU-DT     CL*19
01190                  END-IF                                              CL*19
01191               END-IF                                                 CL*19
01192               IF EX-AB-INCURRED-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01193                                       OR SPACES                      CL*19
01194                  CONTINUE                                            CL*19
01195               ELSE                                                   CL*19
01196                  MOVE EX-AB-INCURRED-DT TO DC-BIN-DATE-1             CL*19
01197                  MOVE 'EX-AB-INCURRED-DT' TO WS-FIELD-NAME           CL*19
01198                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01199                             8099-DATE-CONVERT-X                      CL*19
01200                  IF NO-CONVERSION-ERROR                              CL*19
01201                     MOVE DC-BIN-DATE-2     TO EX-AB-INCURRED-DT      CL*19
01202                  END-IF                                              CL*19
01203               END-IF                                                 CL*19
01204               IF EX-AB-REPORTED-DT EQUAL LOW-VALUES OR ZEROS         CL*19
01205                                       OR SPACES                      CL*19
01206                  CONTINUE                                            CL*19
01207               ELSE                                                   CL*19
01208                  MOVE EX-AB-REPORTED-DT TO DC-BIN-DATE-1             CL*19
01209                  MOVE 'EX-AB-REPORTED-DT' TO WS-FIELD-NAME           CL*19
01210                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01211                             8099-DATE-CONVERT-X                      CL*19
01212                  IF NO-CONVERSION-ERROR                              CL*19
01213                     MOVE DC-BIN-DATE-2     TO EX-AB-REPORTED-DT      CL*19
01214                  END-IF                                              CL*19
01215               END-IF                                                 CL*19
01216               IF EX-AB-CHECK-WRITTEN-DT EQUAL LOW-VALUES OR ZEROS    CL*19
01217                                       OR SPACES                      CL*19
01218                  CONTINUE                                            CL*19
01219               ELSE                                                   CL*19
01220                  MOVE EX-AB-CHECK-WRITTEN-DT TO DC-BIN-DATE-1        CL*19
01221                  MOVE 'EX-AB-CHECK-WRITTEN-DT' TO WS-FIELD-NAME      CL*19
01222                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01223                             8099-DATE-CONVERT-X                      CL*19
01224                  IF NO-CONVERSION-ERROR                              CL*19
01225                     MOVE DC-BIN-DATE-2     TO                        CL*21
01226                                        EX-AB-CHECK-WRITTEN-DT        CL*21
01227                  END-IF                                              CL*19
01228               END-IF                                                 CL*19
01229               IF EX-AB-VOID-DT EQUAL LOW-VALUES OR ZEROS             CL*19
01230                                       OR SPACES                      CL*19
01231                  CONTINUE                                            CL*19
01232               ELSE                                                   CL*19
01233                  MOVE EX-AB-VOID-DT TO DC-BIN-DATE-1                 CL*19
01234                  MOVE 'EX-AB-VOID-DT' TO WS-FIELD-NAME               CL*19
01235                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*19
01236                             8099-DATE-CONVERT-X                      CL*19
01237                  IF NO-CONVERSION-ERROR                              CL*19
01238                     MOVE DC-BIN-DATE-2     TO EX-AB-VOID-DT          CL*19
01239                  END-IF                                              CL*19
01240               END-IF                                                 CL*19
01241               IF EX-AB-VOID-SELECT-DT EQUAL LOW-VALUES OR ZEROS      CL*28
01242                                       OR SPACES                      CL*28
01243                  CONTINUE                                            CL*28
01244               ELSE                                                   CL*28
01245                  MOVE EX-AB-VOID-SELECT-DT TO DC-BIN-DATE-1          CL*28
01246                  MOVE 'EX-AB-VOID-SELECT-DT' TO WS-FIELD-NAME        CL*28
01247                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*28
01248                             8099-DATE-CONVERT-X                      CL*28
01249                  IF NO-CONVERSION-ERROR                              CL*28
01250                     MOVE DC-BIN-DATE-2     TO EX-AB-VOID-SELECT-DT   CL*28
01251                  END-IF                                              CL*28
01252               END-IF                                                 CL*28
01253               IF EX-AB-PMT-SELECT-DT EQUAL LOW-VALUES OR ZEROS       CL*28
01254                                       OR SPACES                      CL*28
01255                  CONTINUE                                            CL*28
01256               ELSE                                                   CL*28
01257                  MOVE EX-AB-PMT-SELECT-DT TO DC-BIN-DATE-1           CL*28
01258                  MOVE 'EX-AB-PMT-SELECT-DT' TO WS-FIELD-NAME         CL*28
01259                  PERFORM 8000-DATE-CONVERT-ROUTINE THRU              CL*28
01260                             8099-DATE-CONVERT-X                      CL*28
01261                  IF NO-CONVERSION-ERROR                              CL*28
01262                     MOVE DC-BIN-DATE-2     TO EX-AB-PMT-SELECT-DT    CL*28
01263                  END-IF                                              CL*28
01264               END-IF                                                 CL*28
01265             WHEN OTHER                                               CL*19
01266             CONTINUE                                                 CL*19
01267           END-EVALUATE                                               CL*19
01268         WHEN OTHER                                                   CL*19
01269           CONTINUE                                                   CL*19
01270      END-EVALUATE                                                    CL*19
01271                                                                      CL*19
01272                                                                      CL*19
01273      WRITE ELEXTR-OUT-RECORD FROM REPORTS-EXTRACT-RECORD             CL*19
01274                                                                      CL*19
01275      PERFORM 0060-READ-ELEXTR THRU 0069-EXIT                         CL*19
01276                                                                      CL*19
01277               .                                                      CL*19
01278  0599-EXIT.                                                          CL*19
01279                                                                      CL*19
01280      EXIT.                                                           CL*19
01281                                                                      CL*19
01282      EJECT                                                           CL**9
01283                                                                      CL**9
01284      EJECT                                                           CL**3
01285  8000-DATE-CONVERT-ROUTINE.                                          CL**3
01286                                                                      CL**3
01287      MOVE '6'                    TO DC-OPTION-CODE                   CL**3
01288      MOVE +0                     TO DC-ELAPSED-DAYS                  CL**3
01289      MOVE ER-NUM-MONTHS          TO DC-ELAPSED-MONTHS                CL**3
01290      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                      CL**3
01291      IF DATE-CONVERSION-ERROR                                        CL**3
01292         DISPLAY ' DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL**3
01293         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME.                  CL**3
01294                                                                      CL**3
01295                                                                      CL**3
01296  8099-DATE-CONVERT-X.                                                CL**3
01297      EXIT.                                                           CL**3
01298                                                                      CL*23
01299      EJECT                                                           CL*23
01300  8100-DATE-CONVERT-ROUTINE.                                          CL*23
01301                                                                      CL*23
01302 *** CONVERT TO BIN                                                   CL*23
01303      MOVE 'L'                    TO DC-OPTION-CODE                   CL*23
01304      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                      CL*23
01305      IF DATE-CONVERSION-ERROR                                        CL*23
01306         DISPLAY '1DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL*23
01307         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME                   CL*23
01308      ELSE                                                            CL*23
01309 *** BIN PLUS ELAPSED                                                 CL*23
01310      MOVE '6'                    TO DC-OPTION-CODE                   CL*23
01311      MOVE +0                     TO DC-ELAPSED-DAYS                  CL*23
01312      MOVE ER-NUM-MONTHS          TO DC-ELAPSED-MONTHS                CL*23
01313      CALL 'ELDATCX' USING DATE-CONVERSION-DATA                       CL*23
01314      IF DATE-CONVERSION-ERROR                                        CL*23
01315         DISPLAY '2DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL*23
01316         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME                   CL*23
01317      ELSE                                                            CL*23
01318 *** BIN TO CYMD                                                      CL*23
01319      MOVE ' '                    TO DC-OPTION-CODE                   CL*23
01320      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1                    CL*23
01321      CALL 'ELDATCX' USING DATE-CONVERSION-DATA                       CL*23
01322      IF DATE-CONVERSION-ERROR                                        CL*23
01323         DISPLAY '3DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL*23
01324         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME.                  CL*23
01325                                                                      CL*23
01326                                                                      CL*23
01327  8199-DATE-CONVERT-X.                                                CL*23
01328      EXIT.                                                           CL*23
01329                                                                      CL*23
01330      EJECT                                                           CL*23
01331  8200-DATE-CONVERT-ROUTINE.                                          CL*23
01332                                                                      CL**3
01333 *** CONVERT TO BIN                                                   CL**3
01334      MOVE '3'                    TO DC-OPTION-CODE                   CL*23
01335      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                      CL**3
01336      IF DATE-CONVERSION-ERROR                                        CL**3
01337         DISPLAY '4DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL*23
01338         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME                   CL**3
01339      ELSE                                                            CL**3
01340 *** BIN PLUS ELAPSED                                                 CL**3
01341      MOVE '6'                    TO DC-OPTION-CODE                   CL**3
01342      MOVE +0                     TO DC-ELAPSED-DAYS                  CL**3
01343      MOVE ER-NUM-MONTHS          TO DC-ELAPSED-MONTHS                CL**3
01344      CALL 'ELDATCX' USING DATE-CONVERSION-DATA                       CL**3
01345      IF DATE-CONVERSION-ERROR                                        CL**3
01346         DISPLAY '5DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL*23
01347         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME                   CL**3
01348      ELSE                                                            CL**3
01349 *** BIN TO YMD                                                       CL*23
01350      MOVE ' '                    TO DC-OPTION-CODE                   CL**3
01351      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1                    CL*18
01352      CALL 'ELDATCX' USING DATE-CONVERSION-DATA                       CL**3
01353      IF DATE-CONVERSION-ERROR                                        CL**3
01354         DISPLAY '6DATE CONVERT ERROR, CODE = ' DC-ERROR-CODE         CL*23
01355         '  ' WS-FEM-FILE-NAME '    ' WS-FIELD-NAME.                  CL**3
01356                                                                      CL**3
01357                                                                      CL**3
01358  8299-DATE-CONVERT-X.                                                CL*23
01359      EXIT.                                                           CL**3
01360                                                                      CL**3
01361  ABEND-PGM SECTION.                                                  CL**3
01362                    COPY ELCABEND.                                    CL**3
01363                                                                      CL**3
