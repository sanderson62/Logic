00001  IDENTIFICATION DIVISION.                                         10/11/97
00002                                                                   EL339
00003  PROGRAM-ID.                 EL339 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL339
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL339
00006 *              CONVERSION DATE 02/15/96 18:56:34.                 EL339
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL339
00008 *                            VMOD=2.006.                          EL339
00009 *AUTHOR.     LOGIC INC.                                           EL339
00010 *            DALLAS, TEXAS.                                       EL339
00011                                                                   EL339
00011                                                                   EL339
00012 *DATE-COMPILED.                                                   EL339
00013                                                                   EL339
00014 *SECURITY.   *****************************************************EL339
00015 *            *                                                   *EL339
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL339
00017 *            *                                                   *EL339
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL339
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL339
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL339
00021 *            *                                                   *EL339
00022 *            *****************************************************EL339
00023                                                                   EL339
00024 *REMARKS.                                                         EL339
00025 *         THIS PROGRAM READS A COMPANIES ELLETR FILE AND          EL339
00026 *       COPIES ALL LETTERS TO ANOTHER COMPANIES ELLETR FILE.      EL339
00027 *       THE INPUT COMPANY IS DETERMINED BY THE DATE CARD,         EL339
00028 *       THE OUPUT IS DETERMINED BY CONTROL CARDS.                 EL339
00029 *                                                                 EL339
00030 *       CC 01-03  = CLIENT ID                                     EL339
00031 *       CC 04-80  = SPACES                                        EL339
00032                                                                   EL339
00033  ENVIRONMENT DIVISION.                                            EL339
00034                                                                   EL339
00035  INPUT-OUTPUT SECTION.                                            EL339
00036                                                                   EL339
00037  FILE-CONTROL.                                                    EL339
00038                                                                   EL339
00039      SELECT CARD-FILE                                             EL339
00040          ASSIGN TO SYS006-UR-2540R-S-SYS006.                      EL339
00041                                                                   EL339
00042      SELECT PRNTR              ASSIGN TO SYS008-UR-1403-S-SYS008. EL339
00043                                                                   EL339
00044      SELECT DISK-DATE                                             EL339
00045          ASSIGN TO SYS019-UT-FBA1-S-SYS019.                       EL339
00046                                                                   EL339
00047      SELECT ELCNTL                                                EL339
00048          ASSIGN TO SYS018-3380-ELCNTL                             EL339
00049          ORGANIZATION IS INDEXED                                  EL339
00050          ACCESS IS DYNAMIC                                        EL339
00051          RECORD KEY IS CF-CONTROL-PRIMARY                         EL339
00052          FILE STATUS IS ELCNTL-FILE-STATUS.                       EL339
00053                                                                   EL339
00054      SELECT ELLETRI                                               EL339
00055          ASSIGN TO SYS018-3380-ELLETRI                            EL339
00056          ORGANIZATION IS INDEXED                                  EL339
00057          ACCESS IS DYNAMIC                                        EL339
00058          RECORD KEY IS LI-CONTROL-PRIMARY                         EL339
00059          FILE STATUS IS ELLETRI-FILE-STATUS.                      EL339
00060                                                                   EL339
00061      SELECT ELLETRO                                               EL339
00062          ASSIGN TO SYS018-3380-ELLETRO                            EL339
00063          ORGANIZATION IS INDEXED                                  EL339
00064          ACCESS IS DYNAMIC                                        EL339
00065          RECORD KEY IS TX-CONTROL-PRIMARY                         EL339
00066          FILE STATUS IS ELLETRO-FILE-STATUS.                      EL339
00067                                                                   EL339
00068      EJECT                                                        EL339
00069  DATA DIVISION.                                                   EL339
00070                                                                   EL339
00071  FILE SECTION.                                                    EL339
00072                                                                   EL339
00073  FD  CARD-FILE                                                    EL339
00074      BLOCK CONTAINS 0 RECORDS
00075      RECORDING MODE F.                                            EL339
00076                                                                   EL339
00077  01  CARD-RECORD.                                                 EL339
00078      12  CARD-CLIENT             PIC X(03).                       EL339
00079      12  FILLER                  PIC X(77).                       EL339
00080                                                                   EL339
00081      EJECT                                                        EL339
00082  FD  PRNTR                       COPY ELCPRTFD.                   EL339
00083                                                                   EL339
00084  FD  ELCNTL.                                                      EL339
00085                                                                   EL339
00086                                  COPY ELCCNTL.                    EL339
00087                                                                   EL339
00088      EJECT                                                        EL339
00089  FD  ELLETRI.                                                     EL339
00090                                                                   EL339
00091  01  LETTER-INPUT-REC.                                            EL339
00092      12  LI-RECORD-ID            PIC X(02).                       EL339
00093      12  LI-CONTROL-PRIMARY.                                      EL339
00094          16  LI-COMPANY-CD       PIC X(01).                       EL339
00095          16  LI-LETTER-ACCESS    PIC X(04).                       EL339
00096          16  FILLER              PIC X(08).                       EL339
00097          16  LI-LINE-SEQUENCE    PIC S9(04) COMP.                 EL339
00098      12  FILLER                  PIC X(83).                       EL339
00099                                                                   EL339
00100      EJECT                                                        EL339
00101  FD  ELLETRO.                                                     EL339
00102                                                                   EL339
00103                                  COPY ELCTEXT.                    EL339
00104                                                                   EL339
00105      EJECT                                                        EL339
00106  FD  DISK-DATE                   COPY ELCDTEFD.                   EL339
00107                                                                   EL339
00108  WORKING-STORAGE SECTION.                                         EL339
00109  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL339
00110  77  FILLER  PIC X(32) VALUE '********************************'.  EL339
00111  77  FILLER  PIC X(32) VALUE '*    EL339  WORKING-STORAGE    *'.  EL339
00112  77  FILLER  PIC X(32) VALUE '*********VMOD=2.006 ************'.  EL339
00113                                                                   EL339
00114  77  PGM-SUB                     PIC S9(4)  COMP     VALUE +339.  EL339
00115  77  ABEND-CODE                  PIC X(4).                        EL339
00116  77  ABEND-OPTION                PIC X.                           EL339
00117  77  OLC-REPORT-NAME             PIC X(5)           VALUE 'EL339'.EL339
00118  77  LINE-COUNT                  PIC S9(03)  VALUE +99 COMP-3.    EL339
00119  77  PAGE-COUNT                  PIC S9(03)  VALUE +0 COMP-3.     EL339
00120                                                                   EL339
00121  01  FILLER.                                                      EL339
00122      12  WS-LETTER-COUNT         PIC S9(07)  VALUE +0 COMP-3.     EL339
00123      12  WS-SAVE-LETTER          PIC X(04)   VALUE LOW-VALUES.    EL339
00124      12  ELCNTL-FILE-STATUS      PIC X(02)   VALUE SPACES.        EL339
00125      12  ELLETRI-FILE-STATUS     PIC X(02)   VALUE SPACES.        EL339
00126      12  ELLETRO-FILE-STATUS     PIC X(02)   VALUE SPACES.        EL339
00127      12  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.        EL339
00128      12  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.          EL339
00129      12  WS-RETURN-CODE          PIC S9(4)   VALUE +0 COMP.       EL339
00130      12  WS-ZERO                 PIC S9(01)  VALUE +0 COMP-3.     EL339
00131                                                                   EL339
00132                                                                   EL339
00133  01  HEADING-1.                                                   EL339
00134      12  FILLER                  PIC X(40)  VALUE SPACES.         EL339
00135      12  FILLER                  PIC X(48)  VALUE                 EL339
00136          'COPY CLAIM LETTERS FROM ONE COMPANY TO ANOTHER'.        EL339
00137      12  FILLER                  PIC X(30)  VALUE SPACES.         EL339
00138      12  FILLER                  PIC X(8)   VALUE 'EL339  '.      EL339
00139                                                                   EL339
00140                                                                   EL339
00141  01  HEADING-2.                                                   EL339
00142      12  FILLER                  PIC X(47)  VALUE SPACES.         EL339
00143      12  HD-CLIENT               PIC X(30).                       EL339
00144      12  FILLER                  PIC X(47)  VALUE SPACES.         EL339
00145      12  HD-RUN                  PIC X(8).                        EL339
00146                                                                   EL339
00147  01  HEADING-3.                                                   EL339
00148      12  FILLER                  PIC X(52)  VALUE SPACES.         EL339
00149      12  HD-DATE                 PIC X(18).                       EL339
00150      12  FILLER                  PIC X(42)  VALUE SPACES.         EL339
00151      12  FILLER                  PIC X(5)   VALUE 'PAGE '.        EL339
00152      12  HD-PAGE                 PIC ZZ,ZZZ.                      EL339
00153                                                                   EL339
00154  01  HEADING-4.                                                   EL339
00155      12  FILLER                  PIC X(49)  VALUE                 EL339
00156          '   COPY  FROM    TO   LETR             MESSAGE   '.     EL339
00157                                                                   EL339
00158  01  DETAIL-LINE.                                                 EL339
00159      12  FILLER                  PIC X(10).                       EL339
00160      12  DE-CLIENT-FROM          PIC X(03).                       EL339
00161      12  FILLER                  PIC X(03)  VALUE SPACES.         EL339
00162      12  DE-CLIENT-TO            PIC X(03).                       EL339
00163      12  FILLER                  PIC X(03)  VALUE SPACES.         EL339
00164      12  DE-LETTER               PIC X(04).                       EL339
00165      12  FILLER                  PIC X(03)  VALUE SPACES.         EL339
00166      12  DE-MESSAGE              PIC X(20)  VALUE SPACES.         EL339
00167      12  FILLER                  PIC X(03)  VALUE SPACES.         EL339
00168      12  DE-LETTER-COUNT         PIC ZZZ,ZZZ,ZZZ.                 EL339
00169      12  FILLER                  PIC X(08)  VALUE ' RECORDS'.     EL339
00170      12  FILLER                  PIC XX     VALUE SPACES.         EL339
00171                                                                   EL339
00172                                  COPY ELCDATE.                    EL339
00173                                                                   EL339
00174      EJECT                                                        EL339
00175                                  COPY ELCDTECX.                   EL339
00176                                                                   EL339
00177                                  COPY ELCDTEVR.                      CL**2
00178                                                                      CL**2
00179      EJECT                                                        EL339
00180  PROCEDURE DIVISION.                                              EL339
00181                                                                   EL339
00182  0000-LOAD-DATE-CARD.            COPY ELCDTERX.                   EL339
00183                                                                   EL339
00184      EJECT                                                        EL339
00185  0010-OPEN-FILES.                                                 EL339
00186                                                                   EL339
00187      OPEN INPUT ELLETRI                                           EL339
00188                 ELCNTL                                            EL339
00189                 CARD-FILE                                         EL339
00190          I-O    ELLETRO                                           EL339
00191          OUTPUT PRNTR                                             EL339
00192                                                                   EL339
00193      IF ELLETRI-FILE-STATUS EQUAL '00' OR '97'                    EL339
00194         NEXT SENTENCE                                             EL339
00195      ELSE                                                         EL339
00196         MOVE ' ERROR OCURRED OPEN - ELLETRI'                      EL339
00197                               TO WS-ABEND-MESSAGE                 EL339
00198         MOVE ELLETRI-FILE-STATUS                                  EL339
00199                               TO WS-ABEND-FILE-STATUS             EL339
00200        GO TO ABEND-PGM.                                           EL339
00201                                                                   EL339
00202      IF ELLETRO-FILE-STATUS EQUAL '00' OR '97'                    EL339
00203         NEXT SENTENCE                                             EL339
00204      ELSE                                                         EL339
00205         MOVE ' ERROR OCURRED OPEN - ELLETRO'                      EL339
00206                               TO WS-ABEND-MESSAGE                 EL339
00207         MOVE ELLETRO-FILE-STATUS                                  EL339
00208                               TO WS-ABEND-FILE-STATUS             EL339
00209        GO TO ABEND-PGM.                                           EL339
00210                                                                   EL339
00211      IF ELCNTL-FILE-STATUS EQUAL '00' OR '97'                     EL339
00212         NEXT SENTENCE                                             EL339
00213      ELSE                                                         EL339
00214         MOVE ' ERROR OCURRED OPEN - ELCNTL '                      EL339
00215                               TO WS-ABEND-MESSAGE                 EL339
00216         MOVE ELCNTL-FILE-STATUS                                   EL339
00217                               TO WS-ABEND-FILE-STATUS             EL339
00218        GO TO ABEND-PGM.                                           EL339
00219                                                                   EL339
00220      MOVE COMPANY-NAME        TO HD-CLIENT                        EL339
00221      MOVE ALPH-DATE           TO HD-DATE                          EL339
00222      MOVE BIN-RUN-DATE        TO DC-BIN-DATE-1.                   EL339
00223      MOVE ' '                 TO DC-OPTION-CODE                   EL339
00224      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT                  EL339
00225      MOVE DC-GREG-DATE-1-EDIT TO HD-RUN.                          EL339
00226                                                                   EL339
00227  0020-READ-CARD-FILE.                                             EL339
00228                                                                   EL339
00229      READ CARD-FILE AT END                                        EL339
00230           GO TO 0100-END-JOB.                                     EL339
00231                                                                   EL339
00232  0030-READ-ELCNTL.                                                EL339
00233                                                                   EL339
00234      MOVE CARD-CLIENT            TO CF-COMPANY-ID                 EL339
00235      MOVE '1'                    TO CF-RECORD-TYPE                EL339
00236      MOVE SPACES                 TO CF-ACCESS-CD-GENL             EL339
00237                                                                   EL339
00238      READ ELCNTL.                                                 EL339
00239                                                                   EL339
00240      IF ELCNTL-FILE-STATUS EQUAL '23' OR '10'                     EL339
00241         GO TO 0040-CLIENT-NOT-FOUND.                              EL339
00242                                                                   EL339
00243      IF ELCNTL-FILE-STATUS EQUAL '00'                             EL339
00244         GO TO 0050-START-ELLETRI                                  EL339
00245      ELSE                                                         EL339
00246         MOVE 'ERROR ON READ - ELCNTL '                            EL339
00247                                  TO WS-ABEND-MESSAGE              EL339
00248         MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS          EL339
00249         GO TO ABEND-PGM.                                          EL339
00250                                                                   EL339
00251  0040-CLIENT-NOT-FOUND.                                           EL339
00252                                                                   EL339
00253      MOVE CARD-CLIENT            TO DE-CLIENT-TO                  EL339
00254      MOVE 'COMPANY NOT FOUND'    TO DE-MESSAGE                    EL339
00255      PERFORM 7000-WRITE-PRINT THRU 7099-EXIT                      EL339
00256      GO TO 0020-READ-CARD-FILE.                                   EL339
00257                                                                   EL339
00258  0050-START-ELLETRI.                                              EL339
00259                                                                   EL339
00260      MOVE LOW-VALUES             TO LI-CONTROL-PRIMARY            EL339
00261      MOVE DTE-CLASIC-COMPANY-CD  TO LI-COMPANY-CD                 EL339
00262                                                                   EL339
00263      START ELLETRI KEY IS NOT LESS THAN                           EL339
00264            LI-CONTROL-PRIMARY.                                    EL339
00265                                                                   EL339
00266      IF ELLETRI-FILE-STATUS EQUAL '23' OR '10'                    EL339
00267         GO TO 0080-END-COPY-LETTER.                               EL339
00268                                                                   EL339
00269      IF ELLETRI-FILE-STATUS EQUAL '00'                            EL339
00270         NEXT SENTENCE                                             EL339
00271      ELSE                                                         EL339
00272         MOVE 'ERROR ON START - ELLETRI '                          EL339
00273                                  TO WS-ABEND-MESSAGE              EL339
00274         MOVE ELLETRI-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL339
00275         GO TO ABEND-PGM.                                          EL339
00276                                                                   EL339
00277      MOVE LOW-VALUES             TO WS-SAVE-LETTER.               EL339
00278                                                                   EL339
00279  0060-READ-ELLETRI.                                               EL339
00280                                                                   EL339
00281      READ ELLETRI NEXT RECORD.                                    EL339
00282                                                                   EL339
00283      IF ELLETRI-FILE-STATUS EQUAL '23' OR '10'                    EL339
00284         GO TO 0080-END-COPY-LETTER.                               EL339
00285                                                                   EL339
00286      IF ELLETRI-FILE-STATUS EQUAL '00'                            EL339
00287         NEXT SENTENCE                                             EL339
00288      ELSE                                                         EL339
00289         MOVE 'ERROR ON READ - ELLETRI '                           EL339
00290                                  TO WS-ABEND-MESSAGE              EL339
00291         MOVE ELLETRI-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL339
00292         GO TO ABEND-PGM.                                          EL339
00293                                                                   EL339
00294      IF LI-COMPANY-CD NOT EQUAL DTE-CLASIC-COMPANY-CD             EL339
00295         GO TO 0080-END-COPY-LETTER.                               EL339
00296                                                                   EL339
00297      IF LI-RECORD-ID NOT EQUAL 'TL'                               EL339
00298         GO TO 0060-READ-ELLETRI.                                  EL339
00299                                                                   EL339
00300      IF LI-LETTER-ACCESS NOT EQUAL WS-SAVE-LETTER                 EL339
00301         IF WS-SAVE-LETTER NOT EQUAL LOW-VALUES                    EL339
00302            PERFORM 0090-LETTER-BREAK THRU 0099-EXIT.              EL339
00303                                                                   EL339
00304  0070-WRITE-ELLETRO.                                              EL339
00305                                                                   EL339
00306      MOVE LETTER-INPUT-REC       TO TEXT-FILES                    EL339
00307      MOVE CF-COMPANY-CD          TO TX-COMPANY-CD                 EL339
00308      MOVE LI-LETTER-ACCESS       TO WS-SAVE-LETTER                EL339
00309                                                                   EL339
00310      WRITE TEXT-FILES.                                            EL339
00311                                                                   EL339
00312      IF ELLETRO-FILE-STATUS EQUAL '22'                            EL339
00313         GO TO 0060-READ-ELLETRI.                                  EL339
00314                                                                   EL339
00315      IF ELLETRO-FILE-STATUS EQUAL '00'                            EL339
00316         NEXT SENTENCE                                             EL339
00317      ELSE                                                         EL339
00318         MOVE 'ERROR ON WRITE - ELLETRO '                          EL339
00319                                  TO WS-ABEND-MESSAGE              EL339
00320         MOVE ELLETRO-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL339
00321         GO TO ABEND-PGM.                                          EL339
00322                                                                   EL339
00323      ADD +1        TO WS-LETTER-COUNT                             EL339
00324                                                                   EL339
00325      GO TO 0060-READ-ELLETRI.                                     EL339
00326                                                                   EL339
00327  0080-END-COPY-LETTER.                                            EL339
00328                                                                   EL339
00329      MOVE WS-SAVE-LETTER         TO DE-LETTER                     EL339
00330      MOVE 'SUCCESSFULLY COPIED'  TO DE-MESSAGE                    EL339
00331      MOVE WS-LETTER-COUNT        TO DE-LETTER-COUNT               EL339
00332      MOVE +0                     TO WS-LETTER-COUNT               EL339
00333      MOVE DTE-CLIENT             TO DE-CLIENT-FROM                EL339
00334      MOVE CF-COMPANY-ID          TO DE-CLIENT-TO                  EL339
00335      PERFORM 7000-WRITE-PRINT THRU 7099-EXIT                      EL339
00336                                                                   EL339
00337      GO TO 0020-READ-CARD-FILE.                                   EL339
00338                                                                   EL339
00339      EJECT                                                        EL339
00340  0090-LETTER-BREAK.                                               EL339
00341                                                                   EL339
00342      MOVE WS-SAVE-LETTER         TO DE-LETTER                     EL339
00343      MOVE 'SUCCESSFULLY COPIED'  TO DE-MESSAGE                    EL339
00344      MOVE WS-LETTER-COUNT        TO DE-LETTER-COUNT               EL339
00345      MOVE +0                     TO WS-LETTER-COUNT               EL339
00346      MOVE DTE-CLIENT             TO DE-CLIENT-FROM                EL339
00347      MOVE CF-COMPANY-ID          TO DE-CLIENT-TO                  EL339
00348      PERFORM 7000-WRITE-PRINT THRU 7099-EXIT.                     EL339
00349                                                                   EL339
00350  0099-EXIT.                                                       EL339
00351      EXIT.                                                        EL339
00352                                                                   EL339
00353  0100-END-JOB.                                                    EL339
00354                                                                   EL339
00355      CLOSE ELLETRI                                                EL339
00356            ELLETRO                                                EL339
00357            ELCNTL                                                 EL339
00358            CARD-FILE                                              EL339
00359            PRNTR.                                                 EL339
00360                                                                   EL339
00361      IF ELLETRI-FILE-STATUS EQUAL '00'                            EL339
00362         NEXT SENTENCE                                             EL339
00363      ELSE                                                         EL339
00364         MOVE ' ERROR OCURRED CLOSE - ELLETRI'                     EL339
00365                               TO WS-ABEND-MESSAGE                 EL339
00366         MOVE ELLETRI-FILE-STATUS                                  EL339
00367                               TO WS-ABEND-FILE-STATUS             EL339
00368        GO TO ABEND-PGM.                                           EL339
00369                                                                   EL339
00370      IF ELLETRO-FILE-STATUS EQUAL '00'                            EL339
00371         NEXT SENTENCE                                             EL339
00372      ELSE                                                         EL339
00373         MOVE ' ERROR OCURRED CLOSE - ELLETRO'                     EL339
00374                               TO WS-ABEND-MESSAGE                 EL339
00375         MOVE ELLETRO-FILE-STATUS                                  EL339
00376                               TO WS-ABEND-FILE-STATUS             EL339
00377        GO TO ABEND-PGM.                                           EL339
00378                                                                   EL339
00379      IF ELCNTL-FILE-STATUS EQUAL '00'                             EL339
00380         NEXT SENTENCE                                             EL339
00381      ELSE                                                         EL339
00382         MOVE ' ERROR OCURRED CLOSE - ELCNTL '                     EL339
00383                               TO WS-ABEND-MESSAGE                 EL339
00384         MOVE ELCNTL-FILE-STATUS                                   EL339
00385                               TO WS-ABEND-FILE-STATUS             EL339
00386        GO TO ABEND-PGM.                                           EL339
00387                                                                   EL339
00388      GOBACK.                                                      EL339
00389                                                                   EL339
00390      EJECT                                                        EL339
00391  7000-WRITE-PRINT.                                                EL339
00392                                                                   EL339
00393      IF LINE-COUNT GREATER +60                                    EL339
00394         PERFORM 7200-PRINT-HEADINGS THRU 7299-EXIT.               EL339
00395                                                                   EL339
00396      MOVE DETAIL-LINE            TO P-DATA                        EL339
00397      MOVE 1                      TO P-CTL                         EL339
00398      PERFORM 7300-PRINT-RTN THRU 7399-EXIT                        EL339
00399                                                                   EL339
00400      ADD +1 TO LINE-COUNT.                                        EL339
00401                                                                   EL339
00402  7099-EXIT.                                                       EL339
00403      EXIT.                                                        EL339
00404                                                                   EL339
00405      EJECT                                                        EL339
00406  7200-PRINT-HEADINGS.                                             EL339
00407                                                                   EL339
00408      ADD +1 TO PAGE-COUNT                                         EL339
00409      MOVE PAGE-COUNT TO HD-PAGE                                   EL339
00410                                                                   EL339
00411      MOVE +7   TO LINE-COUNT                                      EL339
00412                                                                   EL339
00413      MOVE  1  TO P-CTL                                               CL**2
00414      MOVE HEADING-1 TO P-DATA                                     EL339
00415      PERFORM 7300-PRINT-RTN THRU 7399-EXIT                        EL339
00416                                                                   EL339
00417      MOVE 2                 TO P-CTL                              EL339
00418      MOVE HEADING-2 TO P-DATA                                     EL339
00419      PERFORM 7300-PRINT-RTN THRU 7399-EXIT                        EL339
00420                                                                   EL339
00421      MOVE 1                 TO P-CTL                              EL339
00422      MOVE HEADING-3 TO P-DATA                                     EL339
00423      PERFORM 7300-PRINT-RTN THRU 7399-EXIT                        EL339
00424                                                                   EL339
00425      MOVE  3                     TO P-CTL                            CL**2
00426      MOVE HEADING-4 TO P-DATA                                     EL339
00427      PERFORM 7300-PRINT-RTN THRU 7399-EXIT                        EL339
00428                                                                   EL339
00429      MOVE  0                     TO P-CTL                            CL**2
00430      MOVE SPACES TO P-DATA                                        EL339
00431      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL339
00432                                                                   EL339
00433  7299-EXIT.                                                       EL339
00434      EXIT.                                                        EL339
00435                                                                   EL339
00436  7300-PRINT-RTN.                                                  EL339
00437                                                                   EL339
00438      WRITE PRT.                                                   EL339
00439                                                                   EL339
00440  7399-EXIT.                                                       EL339
00441      EXIT.                                                        EL339
00442                                                                   EL339
00443  8500-DATE-CONVERSION.           COPY ELCDCS.                     EL339
00444                                                                   EL339
00445                                                                   EL339
00446  ABEND-PGM.                      COPY ELCABEND.                   EL339
00447                                                                   EL339
