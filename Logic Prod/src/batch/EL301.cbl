00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL301
00003  PROGRAM-ID.                 EL301 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL301
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL301
00006 *              CONVERSION DATE 02/13/96 07:44:39.                 EL301
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL301
00008 *                            VMOD=2.003                           EL301
00009                                                                   EL301
00009                                                                   EL301
00010 *AUTHOR.     LOGIC, INC.                                          EL301
00011 *            DALLAS, TEXAS.                                       EL301
00012                                                                   EL301
00013 *DATE-COMPILED.                                                   EL301
00014                                                                   EL301
00015 *SECURITY.   *****************************************************EL301
00016 *            *                                                   *EL301
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL301
00018 *            *                                                   *EL301
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL301
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL301
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL301
00022 *            *                                                   *EL301
00023 *            *****************************************************EL301
00024                                                                   EL301
00025 *REMARKS.    *****************************************************EL301
00026 *            *                                                   *EL301
00027 *            *    THIS PROGRAM GENERATES A CONVERSION CALENDAR   *EL301
00028 *            *    FOR A 100 YEAR PERIOD FROM 1916 TO 2015.  THE  *EL301
00029 *            *    DATES FOR EACH DAY OF THE SPECIFIED YEAR WILL  *EL301
00030 *            *    APPEAR AS FOLLOWS:  I. GREGORIAN   (MM/DD/YY)  *EL301
00031 *            *                       II. NUMBER OF DAYS SINCE    *EL301
00032 *            *                           1900 IN HEX FORMAT.     *EL301
00033 *            *                      III. JULIAN   (YY/DDD)       *EL301
00034 *            *                                                   *EL301
00035 *            *****************************************************EL301
00036                                                                   EL301
00037  ENVIRONMENT DIVISION.                                            EL301
00038  INPUT-OUTPUT SECTION.                                            EL301
00039  FILE-CONTROL.                                                    EL301
00040                                                                   EL301
00041      SELECT PRINTER ASSIGN TO SYS008-UR-1403-S-SYS008.            EL301
00042                                                                   EL301
00043  DATA DIVISION.                                                   EL301
00044  FILE SECTION.                                                    EL301
00045                                                                   EL301
00046  FD  PRINTER                                                      EL301
00047      RECORDING MODE IS F.                                         EL301
00048  01  PRINT-RECORD                    PIC X(133).                  EL301
00049                                                                   EL301
00050      EJECT                                                        EL301
00051  WORKING-STORAGE SECTION.                                         EL301
00052  77  FILLER   PIC X(32) VALUE '********************************'. EL301
00053  77  FILLER   PIC X(32) VALUE '**  EL301   WORKING STORAGE   **'. EL301
00054  77  FILLER   PIC X(32) VALUE '**** V/M 2.003 *****************'. EL301
00055                                                                   EL301
00056  77  DAYS-IN-YEAR                    PIC 999.                     EL301
00057  77  DIVIDE-RESULT                   PIC 99.                      EL301
00058  77  DIVIDE-REMAINDER                PIC 9.                       EL301
00059          88  A-LEAP-YEAR                VALUE ZERO.               EL301
00060                                                                   EL301
00061  77  BLANK-ONE                       PIC X(133)  VALUE SPACES.    EL301
00062  77  BEGINNING-YEAR-DAY              PIC 9(5)    VALUE 16001.     EL301
00063                                                                   EL301
00064  01  WS-ABEND-STORAGE.                                            EL301
00065      12  WS-RETURN-CODE          PIC S9(4)  VALUE ZERO COMP.      EL301
00066      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         EL301
00067      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZERO.           EL301
00068      12  WS-ZERO                 PIC S9     VALUE ZERO COMP-3.    EL301
00069                                                                   EL301
00070  01  BINARY-WORK                     PIC 9(8)   COMP.             EL301
00071  01  WORK-BINARY  REDEFINES  BINARY-WORK.                         EL301
00072      05  FILLER                      PIC XX.                      EL301
00073      05  BINARY-DTE                  PIC XX.                      EL301
00074                                                                   EL301
00075  01  CALENDAR-DATE.                                               EL301
00076      05  CALENDAR-YEAR               PIC 99.                      EL301
00077      05  CALENDAR-DAY                PIC 999.                     EL301
00078                                                                   EL301
00079                                      COPY ELCDATE.                   CL**4
00080                                                                   EL301
00081  01  JULIAN-EDITED.                                               EL301
00082      05  JULIAN-YEAR                 PIC 99.                      EL301
00083      05  FILLER                      PIC X       VALUE '/'.       EL301
00084      05  JULIAN-DAY                  PIC 999.                     EL301
00085                                                                   EL301
00086  01  HEXADECIMAL-DAYS.                                            EL301
00087      05  HEX-CHARACTER               PIC X                        EL301
00088                        OCCURS 4 TIMES  INDEXED BY U.              EL301
00089                                                                   EL301
00090  01  CONVERSION-TABLE.                                            EL301
00091      05  FILLER          PIC X(21)  VALUE '104096002560001600001'.EL301
00092      05  FILLER          PIC X(21)  VALUE '208192005120003200002'.EL301
00093      05  FILLER          PIC X(21)  VALUE '312288007680004800003'.EL301
00094      05  FILLER          PIC X(21)  VALUE '416384010240006400004'.EL301
00095      05  FILLER          PIC X(21)  VALUE '520480012800008000005'.EL301
00096      05  FILLER          PIC X(21)  VALUE '624576015360009600006'.EL301
00097      05  FILLER          PIC X(21)  VALUE '728672017920011200007'.EL301
00098      05  FILLER          PIC X(21)  VALUE '832768020480012800008'.EL301
00099      05  FILLER          PIC X(21)  VALUE '936864023040014400009'.EL301
00100      05  FILLER          PIC X(21)  VALUE 'A40960025600016000010'.EL301
00101      05  FILLER          PIC X(21)  VALUE 'B45056028160017600011'.EL301
00102      05  FILLER          PIC X(21)  VALUE 'C49152030720019200012'.EL301
00103      05  FILLER          PIC X(21)  VALUE 'D53248033280020800013'.EL301
00104      05  FILLER          PIC X(21)  VALUE 'E57344035840022400014'.EL301
00105      05  FILLER          PIC X(21)  VALUE 'F61440038400024000015'.EL301
00106                                                                   EL301
00107  01  HEXADECIMAL-TABLE  REDEFINES  CONVERSION-TABLE.              EL301
00108      05  HEXADECIMAL-VALUES  OCCURS 15 TIMES  INDEXED BY V.       EL301
00109          10  HEX-VALUE               PIC X.                       EL301
00110          10  DECIMAL-NUMBERS  OCCURS 4 TIMES  INDEXED BY W.       EL301
00111              15  DECIMAL-VALUE       PIC 9(5).                    EL301
00112                                                                   EL301
00113  01  CALENDAR-TABLE.                                              EL301
00114      05  CALENDAR-ROWS  OCCURS 61 TIMES  INDEXED BY X.            EL301
00115          10  CALENDAR-COLUMNS  OCCURS 6 TIMES  INDEXED BY Y.      EL301
00116              15  GREGORIAN           PIC X(8).                    EL301
00117              15  BINARY-74 PIC X(4).                              EL301
00118              15  JULIAN              PIC 9(6).                    EL301
00119                                                                   EL301
00120  01  CALENDAR-LINE.                                               EL301
00121      05  FILLER                      PIC X       VALUE SPACE.     EL301
00122      05  CALENDAR-DATES  OCCURS 6 TIMES  INDEXED BY Z.            EL301
00123          10  GREGORIAN-DATE          PIC X(8).                    EL301
00124          10  FILLER                  PIC X.                       EL301
00125          10  BINARY-DATE             PIC X(4).                    EL301
00126          10  FILLER                  PIC X.                       EL301
00127          10  JULIAN-DATE             PIC X(6).                    EL301
00128          10  FILLER                  PIC XX.                      EL301
00129                                                                   EL301
00130  01  HEADING-1.                                                   EL301
00131      05  FILLER            PIC X(56) VALUE SPACES.                EL301
00132      05  FILLER            PIC X(19) VALUE 'CONVERSION CALENDER'. EL301
00133      05  FILLER            PIC X(57) VALUE SPACES.                EL301
00134                                                                   EL301
00135  01  HEADING-2.                                                   EL301
00136      05  FILLER            PIC X(59) VALUE SPACES.                EL301
00137      05  FILLER            PIC X(9)  VALUE 'FOR YEAR '.           EL301
00138      05  HDG-CENTURY       PIC XX    VALUE '19'.                  EL301
00139      05  HDG-YEAR          PIC XX.                                EL301
00140      05  FILLER            PIC X(60) VALUE SPACES.                EL301
00141                                                                   EL301
00142  01  HEADING-3.                                                   EL301
00143      05  FILLER        PIC X(23) VALUE '   GREG    HEX   JUL   '. EL301
00144      05  FILLER        PIC X(22) VALUE  '  GREG    HEX   JUL   '. EL301
00145      05  FILLER        PIC X(22) VALUE  '  GREG    HEX   JUL   '. EL301
00146      05  FILLER        PIC X(22) VALUE  '  GREG    HEX   JUL   '. EL301
00147      05  FILLER        PIC X(22) VALUE  '  GREG    HEX   JUL   '. EL301
00148      05  FILLER        PIC X(22) VALUE  '  GREG    HEX   JUL   '. EL301
00149      EJECT                                                        EL301
00150  PROCEDURE DIVISION.                                              EL301
00151                                                                   EL301
00152  0100-PROGRAM-PROCESSING.                                         EL301
00153      OPEN OUTPUT PRINTER.                                         EL301
00154                                                                   EL301
00155      MOVE '5'                TO DC-OPTION-CODE.                   EL301
00156      MOVE BEGINNING-YEAR-DAY TO CALENDAR-DATE.                    EL301
00157                                                                   EL301
00158      PERFORM 0200-GENERATE-CALENDAR THRU 0200-EXIT 100 TIMES.     EL301
00159                                                                   EL301
00160      CLOSE PRINTER.                                               EL301
00161                                                                   EL301
00162      GOBACK.                                                      EL301
00163                                                                   EL301
00164      EJECT                                                        EL301
00165  0200-GENERATE-CALENDAR.                                          EL301
00166      PERFORM 0300-INITIALIZE-CALENDAR       THRU 0300-EXIT.       EL301
00167                                                                   EL301
00168      PERFORM 0400-DATE-CONVERSION-ROUTINE   THRU 0400-EXIT.       EL301
00169                                                                   EL301
00170      PERFORM 0500-PRINT-CONVERSION-CALENDAR THRU 0500-EXIT.       EL301
00171                                                                   EL301
00172      IF CALENDAR-YEAR = 99                                        EL301
00173          MOVE 00 TO CALENDAR-YEAR                                 EL301
00174      ELSE                                                         EL301
00175          ADD  1  TO CALENDAR-YEAR.                                EL301
00176                                                                   EL301
00177      MOVE 001    TO CALENDAR-DAY.                                 EL301
00178                                                                   EL301
00179  0200-EXIT.                                                       EL301
00180      EXIT.                                                        EL301
00181                                                                   EL301
00182      EJECT                                                        EL301
00183  0300-INITIALIZE-CALENDAR.                                        EL301
00184      MOVE SPACES TO CALENDAR-TABLE  CALENDAR-LINE.                EL301
00185                                                                   EL301
00186      MOVE CALENDAR-YEAR                  TO DC-ALPHA-YEAR.           CL**3
00187      MOVE 7                              TO DC-OPTION-CODE.       EL301
00188      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0400-EXIT.         EL301
00189      IF DATE-CONVERSION-ERROR                                        CL**2
00190         IF ONLY-CENTURY                                              CL**2
00191            CONTINUE                                                  CL**2
00192         ELSE                                                         CL**2
00193            MOVE 'CENTURY NOT FOUND'  TO WS-ABEND-MESSAGE             CL**2
00194            MOVE DC-ERROR-CODE        TO WS-ABEND-FILE-STATUS         CL**2
00195            GO TO ABEND-PGM                                           CL**3
00196         END-IF                                                       CL**2
00197      ELSE                                                            CL**2
00198         MOVE 'ERROR FINDING CENTURY'  TO WS-ABEND-MESSAGE         EL301
00199         MOVE DC-ERROR-CODE            TO WS-ABEND-FILE-STATUS     EL301
00200         GO TO ABEND-PGM.                                             CL**3
00201                                                                   EL301
00202      MOVE 'H'  TO  DC-OPTION-CODE.                                   CL**3
00203      MOVE DC-GREG-DATE-CYMD TO HOLD-CENTURY-1.                       CL**2
00204      PERFORM 0400-DATE-CONVERSION-ROUTINE.                           CL**3
00205                                                                   EL301
00206      IF DATE-CONVERSION-ERROR                                     EL301
00207         IF ONLY-LEAP-YEAR                                         EL301
00208            IF HOLD-CEN-1-DA = 28                                     CL**2
00209               MOVE 365 TO DAYS-IN-YEAR                               CL**2
00210            ELSE                                                   EL301
00211               MOVE 366 TO DAYS-IN-YEAR                               CL**2
00212            END-IF                                                 EL301
00213         ELSE                                                      EL301
00214            MOVE 'LEAP YEAR ERROR'  TO  WS-ABEND-MESSAGE           EL301
00215            MOVE DC-ERROR-CODE      TO  WS-ABEND-FILE-STATUS       EL301
00216            GO TO ABEND-PGM.                                          CL**3
00217                                                                   EL301
00218      SET X Y  TO  1.                                              EL301
00219                                                                   EL301
00220  0300-EXIT.                                                       EL301
00221      EXIT.                                                        EL301
00222                                                                   EL301
00223      EJECT                                                        EL301
00224  0400-DATE-CONVERSION-ROUTINE.                                    EL301
00225      MOVE CALENDAR-DATE       TO DC-JULIAN-DATE.                  EL301
00226                                                                   EL301
00227      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL301
00228                                                                   EL301
00229      MOVE DC-GREG-DATE-1-EDIT TO GREGORIAN (X, Y).                EL301
00230                                                                   EL301
00231      MOVE ZEROS TO HEXADECIMAL-DAYS  BINARY-WORK.                 EL301
00232      MOVE DC-BIN-DATE-1 TO BINARY-DTE.                            EL301
00233      SET V W  TO  1.                                              EL301
00234                                                                   EL301
00235      PERFORM 0900-GENERATE-HEX-CHARACTERS THRU 0999-EXIT.         EL301
00236                                                                   EL301
00237      MOVE HEXADECIMAL-DAYS TO BINARY-74 (X, Y).                   EL301
00238                                                                   EL301
00239      MOVE CALENDAR-YEAR    TO JULIAN-YEAR.                        EL301
00240      MOVE CALENDAR-DAY     TO JULIAN-DAY.                         EL301
00241      MOVE JULIAN-EDITED    TO JULIAN (X, Y).                      EL301
00242                                                                   EL301
00243      IF X = 61                                                    EL301
00244          SET X TO 1                                               EL301
00245          SET Y UP BY 1                                            EL301
00246      ELSE                                                         EL301
00247          SET X UP BY 1.                                           EL301
00248                                                                   EL301
00249      ADD 1 TO CALENDAR-DAY.                                       EL301
00250                                                                   EL301
00251      IF CALENDAR-DAY NOT GREATER THAN DAYS-IN-YEAR                EL301
00252          GO TO 0400-DATE-CONVERSION-ROUTINE.                      EL301
00253                                                                   EL301
00254  0400-EXIT.                                                       EL301
00255      EXIT.                                                        EL301
00256                                                                   EL301
00257      EJECT                                                        EL301
00258  0500-PRINT-CONVERSION-CALENDAR.                                  EL301
00259      MOVE CALENDAR-YEAR TO HDG-YEAR                               EL301
00260                            DC-ALPHA-YEAR.                            CL**3
00261      MOVE '7'           TO DC-OPTION-CODE.                           CL**3
00262      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0400-EXIT.            CL**3
00263      IF ONLY-CENTURY                                              EL301
00264         MOVE  DC-ALPHA-CEN-N  TO  HDG-CENTURY                     EL301
00265      ELSE                                                         EL301
00266         MOVE  'ERROR FINDING HDG-CENTURY'  TO WS-ABEND-MESSAGE    EL301
00267         MOVE  DC-ERROR-CODE            TO WS-ABEND-FILE-STATUS    EL301
00268         GO TO ABEND-PGM.                                             CL**3
00269                                                                   EL301
00270      WRITE PRINT-RECORD FROM HEADING-1 AFTER ADVANCING PAGE.      EL301
00271      WRITE PRINT-RECORD FROM HEADING-2 AFTER ADVANCING 1 LINES.   EL301
00272      WRITE PRINT-RECORD FROM HEADING-3 AFTER ADVANCING 2 LINES.   EL301
00273      WRITE PRINT-RECORD FROM BLANK-ONE AFTER ADVANCING 1 LINES.   EL301
00274                                                                   EL301
00275      SET X Y Z  TO  1.                                            EL301
00276      PERFORM 0550-PRINT-CALENDAR-LINES THRU 0550-EXIT.            EL301
00277                                                                   EL301
00278  0500-EXIT.                                                       EL301
00279      EXIT.                                                        EL301
00280                                                                   EL301
00281  0550-PRINT-CALENDAR-LINES.                                       EL301
00282      PERFORM 0575-GET-CALENDAR-DATES THRU 0575-EXIT  6 TIMES.     EL301
00283                                                                   EL301
00284      WRITE PRINT-RECORD FROM CALENDAR-LINE AFTER ADVANCING 1.     EL301
00285                                                                   EL301
00286      IF X LESS THAN 61                                            EL301
00287          SET X UP BY 1                                            EL301
00288          SET Y Z TO 1                                             EL301
00289            GO TO 0550-PRINT-CALENDAR-LINES.                       EL301
00290                                                                   EL301
00291  0550-EXIT.                                                       EL301
00292      EXIT.                                                        EL301
00293                                                                   EL301
00294  0575-GET-CALENDAR-DATES.                                         EL301
00295      MOVE GREGORIAN (X, Y) TO GREGORIAN-DATE (Z).                 EL301
00296      MOVE BINARY-74 (X, Y) TO BINARY-DATE (Z).                    EL301
00297      MOVE JULIAN    (X, Y) TO JULIAN-DATE    (Z).                 EL301
00298                                                                   EL301
00299      SET Y Z UP BY 1.                                             EL301
00300                                                                   EL301
00301  0575-EXIT.                                                       EL301
00302      EXIT.                                                        EL301
00303                                                                   EL301
00304      EJECT                                                        EL301
00305  0900-GENERATE-HEX-CHARACTERS.                                    EL301
00306      IF BINARY-WORK LESS THAN DECIMAL-VALUE (V, W)                EL301
00307          IF W LESS THAN 4                                         EL301
00308              SET W UP BY 1                                        EL301
00309              GO TO 0900-GENERATE-HEX-CHARACTERS                   EL301
00310          ELSE                                                     EL301
00311              GO TO 0999-EXIT.                                     EL301
00312                                                                   EL301
00313      SET V TO 15.                                                 EL301
00314                                                                   EL301
00315  0950-GET-HEXADECIMAL-DIGIT.                                      EL301
00316      IF BINARY-WORK NOT LESS THAN DECIMAL-VALUE (V, W)            EL301
00317          SET U TO W                                               EL301
00318          MOVE HEX-VALUE (V) TO HEX-CHARACTER (U)                  EL301
00319          COMPUTE BINARY-WORK =                                    EL301
00320                        BINARY-WORK - DECIMAL-VALUE (V, W)         EL301
00321          SET V TO 1                                               EL301
00322          GO TO 0900-GENERATE-HEX-CHARACTERS.                      EL301
00323                                                                   EL301
00324      SET V DOWN BY 1.                                             EL301
00325      GO TO 0950-GET-HEXADECIMAL-DIGIT.                            EL301
00326                                                                   EL301
00327  0999-EXIT.                                                       EL301
00328      EXIT.                                                        EL301
00329  ABEND-PGM.                                                       EL301
00330  COPY ELCABEND.                                                   EL301
