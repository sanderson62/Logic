00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL199
00003  PROGRAM-ID.                 EL199 .                                 LV006
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 10:28:05.                    CL**4
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL199
00008 *                                                                 EL199
00009 *AUTHOR.    LOGIC, INC.                                              CL**4
00010 *           DALLAS, TEXAS.                                           CL**4
00011                                                                   EL199
00012 *SECURITY.   *****************************************************   CL**4
00013 *            *                                                   *   CL**4
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00019 *            *                                                   *   CL**4
00020 *            *****************************************************   CL**4
00021 *                                                                    CL**4
00022 *REMARKS.                                                         EL199
00023 *                                                                    CL**4
00024 *        THIS MODULE RESETS THE TERMINAL USER HAS BEEN LOGGED ON     CL**4
00025 *    IN THE USER RECORD OF THE CONTROL FILE.                         CL**4
00026 *                                                                    CL**4
00027 *        THIS MODULE EXECUTES AS PART OF THE POST INITILIZATION      CL**4
00028 *    PROCESSING FOR CICS/VS.                                         CL**4
00029                                                                   EL199
00030      EJECT                                                        EL199
00031  ENVIRONMENT DIVISION.                                            EL199
00032                                                                   EL199
00033  DATA DIVISION.                                                   EL199
00034                                                                   EL199
00035  WORKING-STORAGE SECTION.                                         EL199
00036                                                                   EL199
00037  77  FILLER  PIC X(32)  VALUE '********************************'. EL199
00038  77  FILLER  PIC X(32)  VALUE '*    EL199 WORKING STORAGE     *'. EL199
00039  77  FILLER  PIC X(32)  VALUE '************ V/M 2.004 *********'.    CL**4
00040                                                                   EL199
00041  01  FILLER                          COMP-3.                         CL**6
00042      05  WS-QIDERR-SW                PIC S9          VALUE ZERO.     CL**6
00043      05  WS-BROWSE-SW                PIC S9          VALUE ZERO.     CL**6
00044                                                                   EL199
00045  01  FILLER                          COMP.                           CL**6
00046      05  WS-SIXTEEN                  PIC S9(4)       VALUE +16.      CL**6
00047                                                                   EL199
00048  01  FILLER.                                                      EL199
00049      05  ELCNTL                      PIC X(8) VALUE 'ELCNTL'.        CL**6
00050      05  EL004                       PIC X(8) VALUE 'EL004'.         CL**6
00051      05  E199                        PIC X(4) VALUE 'E199'.          CL**6
00052      05  CSMT                        PIC X(4) VALUE 'CSMT'.          CL**6
00053      05  EXSE                        PIC X(4) VALUE 'EXSE'.          CL**6
00054      05  EXEB                        PIC X(4) VALUE 'EXEB'.          CL**6
00055      05  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.       CL**6
00056                                                                   EL199
00057      05  WS-LAST-ELCNTL-KEY    PIC X(10) VALUE SPACES.               CL**6
00058                                                                   EL199
00059      05  WS-TEMP-STORAGE-QUEUE.                                   EL199
00060          10  FILLER                  PIC XX          VALUE 'SE'.     CL**6
00061          10  WS-TS-COMPANY-ID        PIC XXX         VALUE SPACES.   CL**6
00062          10  FILLER                  PIC X(3)        VALUE SPACES.   CL**6
00063                                                                   EL199
00064      05  WS-EDIT-DATA.                                            EL199
00065          10  WS-EDIT-COMPANY-CODE    PIC X.                          CL**6
00066          10  WS-EDIT-BATCH-NUMBER    PIC X(6).                       CL**6
00067          10  WS-EDIT-COMPANY-ID      PIC X(3).                       CL**6
00068          10  WS-EDIT-RESTART-BATCH   PIC X(6).                       CL**6
00069                                                                   EL199
00070      05  WS-MESSAGE            PIC X(80) VALUE SPACES.               CL**6
00071                                                                   EL199
00072      05  WS-RESTART-MESSAGE          PIC X(80)       VALUE           CL**6
00073          'EL199 - RESTARTING EDIT FOR XXX'.                       EL199
00074                                                                   EL199
00075      05  FILLER  REDEFINES  WS-RESTART-MESSAGE.                   EL199
00076          10  FILLER                  PIC X(28).                      CL**6
00077          10  WS-RESTART-COMPANY-ID   PIC X(3).                       CL**6
00078          10  FILLER                  PIC X(49).                      CL**6
00079                                                                   EL199
00080      05  WS-LOGOFF-MESSAGE           PIC X(80)       VALUE           CL**6
00081          'EL199 - CO=XXX USER=XXXX LOGGED OFF TERMINAL XXXX'.     EL199
00082                                                                   EL199
00083      05  FILLER  REDEFINES  WS-LOGOFF-MESSAGE.                    EL199
00084          10  FILLER                  PIC X(11).                      CL**6
00085          10  WS-LOGOFF-COMPANY-ID    PIC X(3).                       CL**6
00086          10  FILLER                  PIC X(6).                       CL**6
00087          10  WS-LOGOFF-USER-ID       PIC X(4).                       CL**6
00088          10  FILLER                  PIC X(21).                      CL**6
00089          10  WS-LOGOFF-TERMINAL-ID   PIC X(4).                       CL**6
00090          10  FILLER                  PIC X(31).                      CL**6
00091                                                                   EL199
00092      05  WS-START-MESSAGE          PIC X(80)       VALUE             CL**6
00093          'EL199 - STARTING EDIT FOR XXX'.                         EL199
00094                                                                   EL199
00095      05  FILLER  REDEFINES  WS-START-MESSAGE.                     EL199
00096          10  FILLER                  PIC X(26).                      CL**6
00097          10  WS-START-COMPANY-ID     PIC X(3).                       CL**6
00098          10  FILLER                  PIC X(51).                      CL**6
00099                                                                   EL199
00100      EJECT                                                        EL199
00101                           COPY ELCINTF.                           EL199
00102                                                                   EL199
00103      EJECT                                                        EL199
00104                           COPY ELCDATE.                           EL199
00105                                                                   EL199
00106      EJECT                                                        EL199
00107                           COPY ELCCNTL.                           EL199
00108                                                                   EL199
00109      EJECT                                                        EL199
00110  PROCEDURE DIVISION.                                              EL199
00111                                                                   EL199
00112      EJECT                                                        EL199
00113  MAIN-LOGIC SECTION.                                              EL199
00114                                                                   EL199
00115 *    NOTE ******************************************************* EL199
00116 *         *                                                     * EL199
00117 *         *      THIS SECTION CONTROLS THE LOGIC TO BROWSE THE  * EL199
00118 *         *   CREDIT CONTROL FILE CHECKING ALL OF THE USER      * EL199
00119 *         *   RECORDS TO SEE IF THE USER IS STILL LOGGED ON TO  * EL199
00120 *         *   A TERMINAL, AND RESET THE USER SO THEY CAN LOGON  * EL199
00121 *         *   TO A DIFFERENT TERMINAL AFTER CICS COMES UP.      * EL199
00122 *         *                                                     * EL199
00123 *         *       THIS PROGRAM EXECUTES AS PART OF THE CICS/VS  * EL199
00124 *         *   POST INITIALIZATION PROCESSING (IT IS INCLUDED    * EL199
00125 *         *   IN THE PLTPI).                                    * EL199
00126 *         *                                                     * EL199
00127 *         *       UPON ENTRY IT STARTS A TASK TO BE INVOKED     * EL199
00128 *         *   AFTER INITILIZATION IS COMPLETE SO IF ANY ERRORS  *    CL**6
00129 *         *   OCCUR DURING EXECUTION CICS WILL NOT TERMINATE.   * EL199
00130 *         *                                                     * EL199
00131 *         *******************************************************.EL199
00132                                                                   EL199
00133  MLS-010.                                                         EL199
00134                                                                   EL199
00135      EXEC CICS HANDLE CONDITION                                   EL199
00136          ENDDATA (MLS-020)                                        EL199
00137          QIDERR  (MLS-170)                                        EL199
00138          ITEMERR (MLS-180)                                        EL199
00139          NOTFND  (MLS-190)                                        EL199
00140          ENDFILE (MLS-190)                                        EL199
00141          ERROR   (ERROR-OCCURED)                                     CL**6
00142      END-EXEC.                                                       CL**6
00143                                                                   EL199
00144      MOVE +1024 TO  PI-COMM-LENGTH.                                  CL**6
00145                                                                   EL199
00146      EXEC CICS RETRIEVE                                           EL199
00147          INTO  (PROGRAM-INTERFACE-BLOCK)                          EL199
00148          LENGTH (PI-COMM-LENGTH)                                     CL**6
00149      END-EXEC.                                                       CL**6
00150                                                                   EL199
00151      GO TO MLS-030.                                               EL199
00152                                                                   EL199
00153  MLS-020.                                                         EL199
00154                                                                   EL199
00155      MOVE SPACES TO  PROGRAM-INTERFACE-BLOCK                      EL199
00156      MOVE +1024 TO  PI-COMM-LENGTH                                EL199
00157                                                                   EL199
00158      EXEC CICS START                                              EL199
00159          TRANSID (E199)                                           EL199
00160          FROM    (PROGRAM-INTERFACE-BLOCK)                        EL199
00161          LENGTH  (PI-COMM-LENGTH) END-EXEC                        EL199
00162                                                                   EL199
00163      EXEC CICS RETURN                                             EL199
00164          END-EXEC.                                                EL199
00165                                                                   EL199
00166  MLS-030.                                                         EL199
00167                                                                   EL199
00168      MOVE 'EL199 - STARTING TO CHECK FOR EDITS TO RESTART'        EL199
00169                                  TO  WS-MESSAGE                   EL199
00170      PERFORM WRITE-A-LINE.                                        EL199
00171                                                                   EL199
00172      MOVE EIBDATE                TO  DC-JULIAN-YYDDD              EL199
00173      MOVE '5'                    TO  DC-OPTION-CODE               EL199
00174      PERFORM DATE-CONVERSION.                                     EL199
00175                                                                   EL199
00176      EJECT                                                        EL199
00177  MLS-100.                                                         EL199
00178                                                                   EL199
00179 *    NOTE ******************************************************* EL199
00180 *         *                                                     * EL199
00181 *         *      FIND THE COMPANY RECORDS FOR THE COMPANIES     * EL199
00182 *         *  PROCESSING CLAS-IC CREDIT THEN DETERMINE IF THERE  * EL199
00183 *         *  WERE ANY EDITS IN FLIGHT WHEN THE CICS/VS FAILURE  * EL199
00184 *         *  OCCURED.                                           * EL199
00185 *         *                                                     * EL199
00186 *         *******************************************************.EL199
00187                                                                   EL199
00188      MOVE LOW-VALUES  TO  CF-CONTROL-PRIMARY.                     EL199
00189                                                                   EL199
00190  MLS-110.                                                         EL199
00191                                                                   EL199
00192      EXEC CICS STARTBR GTEQ                                       EL199
00193          DATASET (ELCNTL)                                         EL199
00194          RIDFLD  (CF-CONTROL-PRIMARY) END-EXEC.                   EL199
00195                                                                   EL199
00196      MOVE +1 TO  WS-BROWSE-SW.                                    EL199
00197                                                                   EL199
00198  MLS-140.                                                         EL199
00199                                                                   EL199
00200      EXEC CICS READNEXT                                           EL199
00201          DATASET (ELCNTL)                                         EL199
00202          RIDFLD  (CF-CONTROL-PRIMARY)                             EL199
00203          INTO    (CONTROL-FILE) END-EXEC                          EL199
00204                                                                   EL199
00205      IF CF-CONTROL-PRIMARY EQUAL TO WS-LAST-ELCNTL-KEY            EL199
00206          GO TO MLS-140.                                           EL199
00207                                                                   EL199
00208      MOVE CF-CONTROL-PRIMARY TO  WS-LAST-ELCNTL-KEY               EL199
00209                                                                   EL199
00210      IF CF-RECORD-TYPE LESS THAN '1'                              EL199
00211          MOVE '1'                TO  CF-RECORD-TYPE               EL199
00212          MOVE LOW-VALUES         TO CF-ACCESS-CD-GENL                CL**3
00213          GO TO MLS-140.                                           EL199
00214                                                                   EL199
00215      IF CF-RECORD-TYPE GREATER THAN '2'                           EL199
00216          MOVE HIGH-VALUES        TO  CF-RECORD-TYPE               EL199
00217          MOVE LOW-VALUES         TO CF-ACCESS-CD-GENL                CL**3
00218          GO TO MLS-140.                                           EL199
00219                                                                   EL199
00220      IF CF-RECORD-TYPE EQUAL TO '2'                               EL199
00221        AND CF-CURRENT-TERM-ON EQUAL TO SPACES                     EL199
00222          GO TO MLS-140.                                           EL199
00223                                                                   EL199
00224      IF CF-RECORD-TYPE EQUAL TO '1'                               EL199
00225        AND (CF-LGX-CREDIT-USER EQUAL TO 'N' AND                      CL**2
00226             CF-LGX-CLAIM-USER EQUAL TO 'N')                          CL**2
00227          GO TO MLS-140.                                           EL199
00228                                                                   EL199
00229      EXEC CICS ENDBR                                              EL199
00230          DATASET (ELCNTL) END-EXEC                                EL199
00231                                                                   EL199
00232      MOVE ZERO TO  WS-BROWSE-SW.                                  EL199
00233                                                                   EL199
00234      IF CF-RECORD-TYPE NOT EQUAL TO '2'                           EL199
00235          GO TO MLS-150.                                           EL199
00236                                                                   EL199
00237      EXEC CICS READ UPDATE                                        EL199
00238          DATASET (ELCNTL)                                         EL199
00239          RIDFLD  (CF-CONTROL-PRIMARY)                             EL199
00240          INTO    (CONTROL-FILE) END-EXEC                          EL199
00241                                                                   EL199
00242      MOVE CF-COMPANY-ID        TO  WS-LOGOFF-COMPANY-ID           EL199
00243      MOVE CF-PROCESSOR         TO  WS-LOGOFF-USER-ID              EL199
00244      MOVE CF-CURRENT-TERM-ON   TO  WS-LOGOFF-TERMINAL-ID          EL199
00245      MOVE WS-LOGOFF-MESSAGE    TO  WS-MESSAGE                     EL199
00246      PERFORM WRITE-A-LINE                                         EL199
00247                                                                   EL199
00248      MOVE SPACES      TO  CF-CURRENT-TERM-ON                      EL199
00249                                                                   EL199
00250      EXEC CICS REWRITE                                            EL199
00251          DATASET (ELCNTL)                                         EL199
00252          FROM    (CONTROL-FILE) END-EXEC                          EL199
00253                                                                   EL199
00254      GO TO MLS-110.                                               EL199
00255                                                                   EL199
00256  MLS-150.                                                         EL199
00257                                                                   EL199
00258      MOVE CF-COMPANY-ID TO  WS-TS-COMPANY-ID                      EL199
00259                                                                   EL199
00260      MOVE ZERO                   TO  WS-QIDERR-SW.                EL199
00261                                                                   EL199
00262      EJECT                                                        EL199
00263  MLS-160.                                                         EL199
00264                                                                   EL199
00265 *    NOTE ******************************************************* EL199
00266 *         *                                                     * EL199
00267 *         *      FIND ALL OF THE EDITS THAT HAD NOT COMPLETED   * EL199
00268 *         *   PROCESSING AND RESTART THEM.                      * EL199
00269 *         *                                                     * EL199
00270 *         *******************************************************.EL199
00271                                                                   EL199
00272      EXEC CICS READQ TS NEXT                                      EL199
00273          QUEUE  (WS-TEMP-STORAGE-QUEUE)                           EL199
00274          INTO   (WS-EDIT-DATA)                                    EL199
00275          LENGTH (WS-SIXTEEN) END-EXEC                             EL199
00276                                                                   EL199
00277      IF WS-EDIT-RESTART-BATCH EQUAL TO HIGH-VALUES                EL199
00278          GO TO MLS-160.                                           EL199
00279                                                                   EL199
00280      IF WS-EDIT-BATCH-NUMBER EQUAL TO SPACES                      EL199
00281          EXEC CICS START                                          EL199
00282              INTERVAL (0)                                         EL199
00283              TRANSID (EXSE)                                       EL199
00284              FROM    (WS-EDIT-DATA)                               EL199
00285              LENGTH  (16) END-EXEC                                EL199
00286        ELSE                                                       EL199
00287          EXEC CICS START                                          EL199
00288              INTERVAL (0)                                         EL199
00289              TRANSID (EXEB)                                       EL199
00290              FROM    (WS-EDIT-DATA)                               EL199
00291              LENGTH  (16) END-EXEC.                               EL199
00292                                                                   EL199
00293      MOVE CF-COMPANY-ID      TO  WS-RESTART-COMPANY-ID            EL199
00294      MOVE WS-RESTART-MESSAGE TO  WS-MESSAGE                       EL199
00295                                                                   EL199
00296      PERFORM WRITE-A-LINE                                         EL199
00297                                                                   EL199
00298      GO TO MLS-160.                                               EL199
00299                                                                   EL199
00300  MLS-170.                                                         EL199
00301                                                                   EL199
00302      MOVE +1                     TO  WS-QIDERR-SW.                EL199
00303                                                                   EL199
00304      EJECT                                                        EL199
00305  MLS-180.                                                         EL199
00306                                                                   EL199
00307 *    NOTE ******************************************************* EL199
00308 *         *                                                     * EL199
00309 *         *      AFTER ALL OF THE EDITS HAVE BEEN SUCCESSFULLY  * EL199
00310 *         *  RESTARTED DELETE THE TEMPORARY STORAGE QUEUE SO    * EL199
00311 *         *  IF A FAILURE OCCURS AGAIN THE SAME EDITS WILL NOT  * EL199
00312 *         *  BE RESTARTED IN ERROR.                             * EL199
00313 *         *                                                     * EL199
00314 *         *******************************************************.EL199
00315                                                                   EL199
00316      IF WS-QIDERR-SW EQUAL TO ZERO                                EL199
00317          EXEC CICS DELETEQ TS                                     EL199
00318              QUEUE (WS-TEMP-STORAGE-QUEUE) END-EXEC.              EL199
00319                                                                   EL199
00320      EXEC CICS SYNCPOINT                                          EL199
00321          END-EXEC.                                                EL199
00322                                                                   EL199
00323      GO TO MLS-110.                                               EL199
00324                                                                   EL199
00325  MLS-190.                                                         EL199
00326                                                                   EL199
00327      IF WS-BROWSE-SW IS NOT EQUAL TO ZERO                         EL199
00328          EXEC CICS ENDBR                                          EL199
00329              DATASET (ELCNTL) END-EXEC.                           EL199
00330                                                                   EL199
00331      MOVE 'EL199 - ENDED'         TO  WS-MESSAGE                  EL199
00332      PERFORM WRITE-A-LINE.                                        EL199
00333                                                                   EL199
00334      EXEC CICS RETURN                                             EL199
00335          END-EXEC.                                                EL199
00336                                                                   EL199
00337      EJECT                                                        EL199
00338  WRITE-A-LINE SECTION.                                            EL199
00339                                                                   EL199
00340      EXEC CICS WRITEQ TD                                          EL199
00341          QUEUE (CSMT)                                             EL199
00342          FROM  (WS-MESSAGE)                                       EL199
00343          LENGTH (80) END-EXEC.                                    EL199
00344                                                                   EL199
00345  WAL-EXIT.                                                        EL199
00346      EXIT.                                                        EL199
00347                                                                   EL199
00348  DATE-CONVERSION SECTION.                                         EL199
00349                                                                   EL199
00350      EXEC CICS LINK                                               EL199
00351          PROGRAM (ELDATCV)                                        EL199
00352          COMMAREA (DATE-CONVERSION-DATA)                          EL199
00353          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL199
00354                                                                   EL199
00355  DCS-EXIT.                                                        EL199
00356      EXIT.                                                        EL199
00357                                                                   EL199
00358      EJECT                                                        EL199
00359  ERROR-OCCURED SECTION.                                           EL199
00360                                                                   EL199
00361      MOVE DFHEIBLK               TO  WS-MESSAGE                   EL199
00362                                                                   EL199
00363      EXEC CICS LINK                                               EL199
00364          PROGRAM ('EL004')                                        EL199
00365          COMMAREA (WS-MESSAGE)                                    EL199
00366          LENGTH  (80) END-EXEC                                    EL199
00367                                                                   EL199
00368      PERFORM WRITE-A-LINE                                         EL199
00369                                                                   EL199
00370      EXEC CICS RETURN                                             EL199
00371          END-EXEC.                                                EL199
00372                                                                   EL199
00373      GOBACK.                                                      EL199
00374                                                                   EL199
