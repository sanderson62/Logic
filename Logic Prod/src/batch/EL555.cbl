00001  IDENTIFICATION DIVISION.                                         03/10/98
00002                                                                   EL555
00003  PROGRAM-ID.                 EL555 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL555
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL555
00006 *              CONVERSION DATE 02/12/96 16:49:31.                 EL555
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL555
00008 *                            VMOD=2.003                           EL555
00009                                                                   EL555
00010 *AUTHOR.     LOGIC, INC.                                          EL555
00011 *            DALLAS, TEXAS.                                       EL555
00012                                                                   EL555
00013 *DATE-COMPILED.                                                   EL555
00014                                                                   EL555
00015 *SECURITY.   *****************************************************EL555
00016 *            *                                                   *EL555
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL555
00018 *            *                                                   *EL555
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL555
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL555
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL555
00022 *            *                                                   *EL555
00023 *            *****************************************************EL555
00024                                                                   EL555
00025 *REMARKS.                                                         EL555
00026                                                                   EL555
00027 *    THIS PROGRAM WILL DELETE ANY REPORTS THAT HAVE BEEN ON THE   EL555
00028 *         FILE LONGER THAN 15 DAYS.  ALSO DELETES ANY DETAIL      EL555
00029 *         RECORDS THAT HAVE NO TRAILER RECORD.                    EL555
00030                                                                   EL555
00031  EJECT                                                            EL555
00032                                                                   EL555
00033  ENVIRONMENT DIVISION.                                            EL555
00034                                                                   EL555
00035  INPUT-OUTPUT SECTION.                                            EL555
00036                                                                   EL555
00037  FILE-CONTROL.                                                    EL555
00038                                                                   EL555
00039      SELECT SORT-WORK    ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      EL555
00040                                                                   EL555
00041      SELECT RPTFILE      ASSIGN TO SYS014-UT-FBA1-S-RPTFILE.      EL555
00042                                                                   EL555
00043      SELECT NEWFILE      ASSIGN TO SYS015-UT-FBA1-S-NEWFILE.      EL555
00044                                                                   EL555
00045  EJECT                                                            EL555
00046  DATA DIVISION.                                                   EL555
00047  FILE SECTION.                                                    EL555
00048                                                                   EL555
00049  SD  SORT-WORK.                                                   EL555
00050  01  SORT-RECORD.                                                 EL555
00051      12  FILLER                      PIC XX.                      EL555
00052      12  SORT-COMPANY                PIC X.                       EL555
00053      12  SORT-TYPE                   PIC X.                       EL555
00054      12  SORT-ID                     PIC X(5).                    EL555
00055      12  SORT-LINE                   PIC S9(8) COMP.              EL555
00056      12  FILLER                      PIC X.                       EL555
00057      12  SORT-CURRENT-DATE           PIC X(8).                    EL555
00058      12  FILLER                      PIC X(124).                  EL555
00059      EJECT                                                        EL555
00060  FD  RPTFILE.                                                     EL555
00061                                                                   EL555
00062  COPY ELCREPT.                                                    EL555
00063      EJECT                                                        EL555
00064  FD  NEWFILE.                                                     EL555
00065                                                                   EL555
00066  01  OUT-REC                PIC X(146).                           EL555
00067      EJECT                                                        EL555
00068  WORKING-STORAGE SECTION.                                         EL555
00069  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL555
00070  77  FILLER  PIC X(32)  VALUE '********************************'. EL555
00071  77  FILLER  PIC X(32)  VALUE '*    EL555 WORKING STORAGE     *'. EL555
00072  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.003 ************'. EL555
00073                                                                   EL555
00074  77  IN-CNT          PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00075  77  DEL-CNT         PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00076  77  OUT-CNT         PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00077  77  STR-CNT         PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00078  77  ELREPT-RECS-DEL PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00079  77  ELREPT-RPTS-DEL PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00080  77  WS-PAGE         PIC S9(7)       COMP-3  VALUE ZERO.          EL555
00081  77  PRT-CNT         PIC S9(3)       COMP-3  VALUE ZERO.          EL555
00082  77  X               PIC X.                                       EL555
00083                                                                   EL555
00084  01  WS-ACCEPT-DATE.                                              EL555
00085      12  WS-AD-YY                    PIC 99.                      EL555
00086      12  WS-AD-MM                    PIC 99.                      EL555
00087      12  WS-AD-DD                    PIC 99.                      EL555
00088                                                                   EL555
00089  01  WS-CURRENT-DATE.                                             EL555
00090      12  WS-CD-MM                    PIC 99.                      EL555
00091      12  FILLER                      PIC X      VALUE '/'.        EL555
00092      12  WS-CD-DD                    PIC 99.                      EL555
00093      12  FILLER                      PIC X      VALUE '/'.        EL555
00094      12  WS-CD-YY                    PIC 99.                      EL555
00095                                                                   EL555
00096  01  WS-TIME-OF-DAY.                                              EL555
00097      12  WS-TIME                     PIC 9(6).                    EL555
00098      12  WS-HUN-SEC                  PIC 99.                      EL555
00099                                                                   EL555
00100  01  FILLER.                                                      EL555
00101      12  SAVE-ID                PIC X(5)  VALUE SPACES.           EL555
00102      12  DATE-NOW               PIC XX.                           EL555
00103      12  DATE-THEN              PIC XX.                           EL555
00104      12  WS-REPORT-ID           PIC X(8).                         EL555
00105      12  WS-COMPANY-CD          PIC X.                            EL555
00106      12  WS-LINE-NUMBER         PIC S9(8)  COMP.                  EL555
00107                                                                   EL555
00108                                                                   EL555
00109      EJECT                                                        EL555
00110  COPY ELCDATE.                                                    EL555
00111      EJECT                                                        EL555
00112  PROCEDURE DIVISION.                                              EL555
00113                                                                   EL555
00114  0100-OPEN-EM.                                                    EL555
00115      ACCEPT WS-ACCEPT-DATE FROM DATE.                             EL555
00116      MOVE WS-AD-YY               TO WS-CD-YY.                     EL555
00117      MOVE WS-AD-MM               TO WS-CD-MM.                     EL555
00118      MOVE WS-AD-DD               TO WS-CD-DD.                     EL555
00119                                                                   EL555
00120      MOVE WS-CURRENT-DATE TO DC-GREG-DATE-1-EDIT.                 EL555
00121      MOVE '2'             TO DC-OPTION-CODE.                      EL555
00122      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.                    EL555
00123      MOVE DC-BIN-DATE-1   TO  DATE-NOW.                           EL555
00124                                                                   EL555
00125                                                                   EL555
00126      OPEN INPUT RPTFILE                                           EL555
00127          OUTPUT NEWFILE.                                          EL555
00128  SORT-INPUT.                                                      EL555
00129         SORT SORT-WORK      ASCENDING KEY                         EL555
00130                                 SORT-COMPANY                      EL555
00131                                 SORT-ID                           EL555
00132                             DESCENDING KEY                        EL555
00133                                 SORT-TYPE                         EL555
00134                                                                   EL555
00135                INPUT PROCEDURE  1000-RD-INPUT                     EL555
00136                OUTPUT PROCEDURE 5000-READ-SORTED.                 EL555
00137                                                                   EL555
00138      CLOSE RPTFILE                                                EL555
00139            NEWFILE.                                               EL555
00140                                                                   EL555
00141      SORT SORT-WORK      ASCENDING KEY                            EL555
00142                              SORT-COMPANY                         EL555
00143                              SORT-TYPE                            EL555
00144                              SORT-ID                              EL555
00145                              SORT-LINE                            EL555
00146                  USING  NEWFILE                                   EL555
00147                  GIVING RPTFILE.                                  EL555
00148                                                                   EL555
00149      DISPLAY '**ELREPT RECORDS IN   ' IN-CNT.                     EL555
00150      DISPLAY '**ELREPT RECS DELETED ' DEL-CNT.                    EL555
00151      DISPLAY '**ELREPT RECS STARTED ' STR-CNT.                    EL555
00152      DISPLAY '**ELREPT RECORDS OUT  ' OUT-CNT.                    EL555
00153      GOBACK.                                                      EL555
00154                                                                   EL555
00155  EJECT                                                            EL555
00156  1000-RD-INPUT          SECTION.                                  EL555
00157  2000-READ-TAPE.                                                  EL555
00158      READ RPTFILE  AT END  GO TO 4000-E-INPUT-SECT.               EL555
00159                                                                   EL555
00160      IF RF-LINE-NUMBER = ZERO                                     EL555
00161         ADD 1 TO STR-CNT                                          EL555
00162         GO TO 2000-READ-TAPE.                                     EL555
00163                                                                   EL555
00164      MOVE REPORT-SAVE-FILE TO SORT-RECORD.                        EL555
00165                                                                   EL555
00166      RELEASE SORT-RECORD.                                         EL555
00167                                                                   EL555
00168      GO TO 2000-READ-TAPE.                                        EL555
00169                                                                   EL555
00170  4000-E-INPUT-SECT.                                               EL555
00171      EXIT.                                                        EL555
00172                                                                   EL555
00173      EJECT                                                        EL555
00174  5000-READ-SORTED     SECTION.                                    EL555
00175  5000-READ-IT.                                                    EL555
00176      RETURN SORT-WORK                                             EL555
00177           AT END GO TO 9000-CLOSE-EM.                             EL555
00178                                                                   EL555
00179      ADD 1 TO IN-CNT.                                             EL555
00180                                                                   EL555
00181      IF SORT-TYPE NOT = '2'                                       EL555
00182         GO TO 6000-CHECK-DETAIL.                                  EL555
00183                                                                   EL555
00184 ******  TRAILER RECORD ******                                     EL555
00185      MOVE SORT-ID   TO SAVE-ID.                                   EL555
00186      MOVE SORT-CURRENT-DATE TO DC-GREG-DATE-1-EDIT.               EL555
00187      MOVE '2'               TO DC-OPTION-CODE.                    EL555
00188      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.                    EL555
00189      MOVE DC-BIN-DATE-1     TO  DATE-THEN.                        EL555
00190                                                                   EL555
00191      MOVE DATE-THEN       TO DC-BIN-DATE-1.                       EL555
00192      MOVE DATE-NOW        TO DC-BIN-DATE-2.                       EL555
00193      MOVE '1'             TO DC-OPTION-CODE.                      EL555
00194      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.                    EL555
00195                                                                   EL555
00196      IF DC-ELAPSED-DAYS GREATER 15                                EL555
00197         MOVE SPACE TO SAVE-ID.                                    EL555
00198                                                                   EL555
00199  6000-CHECK-DETAIL.                                               EL555
00200      IF SORT-ID NOT = SAVE-ID                                     EL555
00201         ADD 1 TO DEL-CNT                                          EL555
00202         GO TO 5000-READ-IT.                                       EL555
00203                                                                   EL555
00204  7000-WRITE-RECORD.                                               EL555
00205      WRITE OUT-REC FROM SORT-RECORD.                              EL555
00206                                                                   EL555
00207      ADD 1 TO OUT-CNT.                                            EL555
00208      GO TO 5000-READ-IT.                                          EL555
00209                                                                   EL555
00210      EJECT                                                        EL555
00211  8000-DATE-CONVERT.                                               EL555
00212      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL555
00213      IF DC-ERROR-CODE NOT = SPACE                                 EL555
00214         MOVE ZEROS TO DC-CONVERSION-DATES.                        EL555
00215  8000-EXIT.                                                       EL555
00216      EXIT.                                                        EL555
00217                                                                   EL555
00218  9000-CLOSE-EM.                                                   EL555
00219                                                                   EL555
00220  9999-EOJ    SECTION.                                             EL555
