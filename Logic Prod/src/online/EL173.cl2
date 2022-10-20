00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL173
00003  PROGRAM-ID.                 EL173 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/13/96 09:32:25.                    CL**3
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL173
00008 *                                                                 EL173
00009 *AUTHOR.    LOGIC, INC.                                              CL**3
00010 *           DALLAS, TEXAS.                                           CL**3
00011                                                                   EL173
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL173
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *                                                                *   CL**3
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00022 *            *                                                   *   CL**3
00023 *            *****************************************************   CL**3
00024                                                                   EL173
00025 *REMARKS.                                                            CL**2
00026 *        THIS PROGRAM PRODUCES A REPORT SHOWING ALL CLAIMS THAT      CL**2
00027 *    HAVE BEEN FLAGGED AS NEEDING A SUPERVISOR'S ATTENTION.          CL**2
00028                                                                   EL173
00029 *    SCREENS     - EL173A - REQUEST FOR REVIEW                       CL**2
00030                                                                   EL173
00031 *    ENTERED BY  - EL171  - REPORT MENU                              CL**2
00032                                                                   EL173
00033 *    EXIT TO     - EL171  - RESULT OF CLEAR OR END OF JOB            CL**2
00034                                                                   EL173
00035 *    INPUT FILES - ELMSTR - CLAIM MASTER                             CL**2
00036                                                                   EL173
00037 *    OUTPUT FILES - NONE                                             CL**2
00038                                                                   EL173
00039 *    COMMAREA    - PASSED.                                           CL**2
00040                                                                   EL173
00041                                                                   EL173
00042 *    NARRATIVE   - ALL CLAIM RECORDS ARE READ TO DETERMINE WHETHER   CL**2
00043 *                  ATTENTION HAS BEEN REQUESTED (CL-SUPV-ATTN-CD =   CL**2
00044 *                  "Y").  AS PAGES ARE COMPLETED THEY ARE WRITTEN    CL**2
00045 *                  TO TEMPORARY STORAGE.                             CL**2
00046                                                                   EL173
00047 *                  IN THE BROWSING MODE, PAGE NUMBER IS USED AS A    CL**2
00048 *                  REFERENCE FOR SETTING THE ITEM NUMBER IN THE      CL**2
00049 *                  T/S READ.                                         CL**2
00050                                                                   EL173
00051      EJECT                                                        EL173
00052  ENVIRONMENT DIVISION.                                            EL173
00053                                                                   EL173
00054  DATA DIVISION.                                                   EL173
00055                                                                   EL173
00056  WORKING-STORAGE SECTION.                                         EL173
00057                                                                   EL173
00058  77  FILLER  PIC X(32)  VALUE '********************************'. EL173
00059  77  FILLER  PIC X(32)  VALUE '*   EL173  WORKING STORAGE     *'. EL173
00060  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.    CL**3
00061                                                                   EL173
00062      EJECT                                                           CL**2
00063                              COPY ELCSCTM.                           CL**2
00064                                                                   EL173
00065                              COPY ELCSCRTY.                          CL**2
00066                                                                   EL173
00067      EJECT                                                           CL**2
00068  01  WS-DATE-AREA.                                                EL173
00069      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL173
00070      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL173
00071                                                                   EL173
00072  01  FILLER                          COMP-3.                      EL173
00073      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  EL173
00074                                                                   EL173
00075      05  TIME-IN                     PIC S9(7)       VALUE ZERO.  EL173
00076      05  TIME-OUT                    REDEFINES                    EL173
00077          TIME-IN                     PIC S9(3)V9(4).              EL173
00078                                                                   EL173
00079  01  FILLER                          COMP SYNC.                   EL173
00080      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1349. EL173
00081      05  SC-ITEM                     PIC S9(4)       VALUE +0001. EL173
00082                                                                   EL173
00083                                                                   EL173
00084  01  FILLER.                                                      EL173
00085      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL173S'.EL173
00086      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL173A'.EL173
00087                                                                   EL173
00088      05  FILLER                      REDEFINES                    EL173
00089          WS-MAP-NAME.                                             EL173
00090          20  FILLER                  PIC XX.                      EL173
00091          20  WS-MAP-NUMBER           PIC X(6).                    EL173
00092                                                                   EL173
00093      05  THIS-PGM                    PIC X(8)      VALUE 'EL173'. EL173
00094                                                                   EL173
00095      05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.     EL173
00096                                                                   EL173
00097      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX03'.EL173
00098                                                                   EL173
00099      05  ER-0004                     PIC 9(4)        VALUE 0004.  EL173
00100      05  ER-0008                     PIC 9(4)        VALUE 0008.  EL173
00101      05  ER-0029                     PIC 9(4)        VALUE 0029.  EL173
00102      05  ER-0047                     PIC 9(4)        VALUE 0047.     CL**3
00103      05  ER-0070                     PIC 9(4)        VALUE 0070.  EL173
00104      05  ER-0313                     PIC 9(4)        VALUE 0313.  EL173
00105                                                                   EL173
00106      05  WS-TEMP-STORAGE-KEY.                                     EL173
00107          10  WS-TS-TERM-ID           PIC X(4).                    EL173
00108          10  FILLER                  PIC X(4)        VALUE '173'. EL173
00109                                                                   EL173
00110      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO   EL173
00111                                      COMP SYNC.                   EL173
00112                                                                   EL173
00113      EJECT                                                        EL173
00114                                      COPY ELCINTF.                   CL**2
00115      12  FILLER                      REDEFINES                    EL173
00116          PI-PROGRAM-WORK-AREA.                                    EL173
00117          16  PI-CLAIM-KEY.                                        EL173
00118              20  PI-CK-COMPANY-CODE     PIC X.                    EL173
00119              20  PI-CK-CARRIER          PIC X.                    EL173
00120              20  PI-CK-CLAIM-NO         PIC X(7).                 EL173
00121              20  PI-CK-CERT-NO          PIC X(11).                EL173
00122                                                                   EL173
00123          16  PI-PREV-CLAIM-KEY.                                   EL173
00124              20  PI-PREV-CK-COMPANY-CODE     PIC X.               EL173
00125              20  PI-PREV-CK-CARRIER          PIC X.               EL173
00126              20  PI-PREV-CK-CLAIM-NO         PIC X(7).            EL173
00127              20  PI-PREV-CK-CERT-NO          PIC X(11).           EL173
00128                                                                   EL173
00129          16  PI-TEMP-STORAGE-ITEM    PIC S9(4)                    EL173
00130                                      COMP SYNC.                   EL173
00131                                                                   EL173
00132          16  PI-LAST-ITEM-NO         PIC S9(4)                       CL**3
00133                                      COMP SYNC.                      CL**3
00134                                                                      CL**3
00135          16  PI-END-OF-FILE          PIC S9                       EL173
00136                                      COMP-3.                      EL173
00137          16  FILLER                  PIC X(595).                     CL**3
00138                                                                   EL173
00139      EJECT                                                        EL173
00140                                      COPY EL173S SUPPRESS.           CL**2
00141                                                                   EL173
00142  01  FILLER                          REDEFINES                    EL173
00143      EL173AI.                                                     EL173
00144                                                                   EL173
00145      05  FILLER                      PIC X(38).                   EL173
00146                                                                   EL173
00147      05  FILLER                      OCCURS 18 TIMES              EL173
00148                                      INDEXED BY EL173A-INDEX.     EL173
00149                                                                   EL173
00150          15  EL173A-CLAIM-LENGTH     PIC S9(4)                    EL173
00151                                      COMP.                        EL173
00152          15  EL173A-CLAIM-ATTRB      PIC X.                       EL173
00153          15  EL173A-CLAIM            PIC X(7).                    EL173
00154                                                                   EL173
00155          15  EL173A-CARRIER-LENGTH   PIC S9(4)                    EL173
00156                                      COMP.                        EL173
00157          15  EL173A-CARRIER-ATTRB    PIC X.                       EL173
00158          15  EL173A-CARRIER          PIC X.                       EL173
00159                                                                   EL173
00160          15  EL173A-CERT-NO-LENGTH   PIC S9(4)                    EL173
00161                                      COMP.                        EL173
00162          15  EL173A-CERT-NO-ATTRB    PIC X.                       EL173
00163          15  EL173A-CERT-NO          PIC X(11).                   EL173
00164                                                                   EL173
00165          15  EL173A-TYPE-LENGTH      PIC S9(4)                    EL173
00166                                      COMP.                        EL173
00167          15  EL173A-TYPE-ATTRB       PIC X.                       EL173
00168          15  EL173A-TYPE             PIC X(4).                    EL173
00169                                                                   EL173
00170          15  EL173A-BY-LENGTH        PIC S9(4)                    EL173
00171                                      COMP.                        EL173
00172          15  EL173A-BY-ATTRB         PIC X.                       EL173
00173          15  EL173A-BY               PIC X(4).                    EL173
00174                                                                   EL173
00175          15  EL173A-EDATE-LENGTH     PIC S9(4)                    EL173
00176                                      COMP.                        EL173
00177          15  EL173A-EDATE-ATTRB      PIC X.                       EL173
00178          15  EL173A-EDATE            PIC X(8).                    EL173
00179                                                                   EL173
00180          15  EL173A-IDATE-LENGTH     PIC S9(4)                    EL173
00181                                      COMP.                        EL173
00182          15  EL173A-IDATE-ATTRB      PIC X.                       EL173
00183          15  EL173A-IDATE            PIC X(8).                    EL173
00184                                                                   EL173
00185          15  EL173A-FILE-LENGTH      PIC S9(4)                    EL173
00186                                      COMP.                        EL173
00187          15  EL173A-FILE-ATTRB       PIC X.                       EL173
00188          15  EL173A-FILE             PIC X(4).                    EL173
00189                                                                   EL173
00190      EJECT                                                        EL173
00191                                      COPY ELCEMIB.                   CL**2
00192      EJECT                                                        EL173
00193                                      COPY ELCDATE.                   CL**2
00194      EJECT                                                        EL173
00195                                      COPY ELCLOGOF.                  CL**2
00196      EJECT                                                           CL**2
00197                                      COPY ELCATTR.                   CL**2
00198      EJECT                                                           CL**2
00199                                      COPY ELCAID.                    CL**2
00200                                                                   EL173
00201  01  FILLER                      REDEFINES                        EL173
00202      DFHAID.                                                      EL173
00203                                                                   EL173
00204      05  FILLER                      PIC X(8).                    EL173
00205                                                                   EL173
00206      05  PF-VALUES                   PIC X                        EL173
00207          OCCURS 24 TIMES.                                         EL173
00208      EJECT                                                        EL173
00209  LINKAGE SECTION.                                                 EL173
00210                                                                   EL173
00211  01  DFHCOMMAREA                     PIC X(1024).                 EL173
00212                                                                   EL173
00213                                      COPY ELCMSTR.                   CL**2
00214      EJECT                                                        EL173
00215  PROCEDURE DIVISION.                                              EL173
00216                                                                   EL173
00217      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL173
00218      MOVE '5'                   TO DC-OPTION-CODE.                EL173
00219      PERFORM 8500-DATE-CONVERSION.                                EL173
00220      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL173
00221      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL173
00222                                                                   EL173
00223      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      EL173
00224                                                                   EL173
00225 *    NOTE ******************************************************* EL173
00226 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL173
00227 *         *  FROM ANOTHER MODULE.                               * EL173
00228 *         *******************************************************.EL173
00229                                                                   EL173
00230      IF EIBCALEN NOT GREATER THAN ZERO                            EL173
00231          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL173
00232          GO TO 8300-SEND-TEXT.                                    EL173
00233                                                                   EL173
00234      EXEC CICS HANDLE CONDITION                                   EL173
00235          PGMIDERR (9600-PGMIDERR)                                 EL173
00236          ENDFILE  (4800-END-OF-FILE)                              EL173
00237          ERROR    (9990-ERROR) END-EXEC.                          EL173
00238                                                                   EL173
00239      EJECT                                                        EL173
00240  0010-MAIN-LOGIC.                                                 EL173
00241      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL173
00242          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL173
00243              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL173
00244              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL173
00245              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL173
00246              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL173
00247              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL173
00248              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL173
00249              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL173
00250              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL173
00251            ELSE                                                   EL173
00252              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL173
00253              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL173
00254              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL173
00255              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL173
00256              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL173
00257              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL173
00258              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL173
00259              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL173
00260        ELSE                                                       EL173
00261          GO TO 0020-MAIN-LOGIC.                                   EL173
00262                                                                   EL173
00263  0015-MAIN-LOGIC.                                                 EL173
00264 *    NOTE ******************************************************* EL173
00265 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL173
00266 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL173
00267 *         *******************************************************.EL173
00268                                                                   EL173
00269      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL173
00270                                                                   EL173
00271      MOVE LOW-VALUES             TO  PI-CLAIM-KEY                 EL173
00272                                      PI-PREV-CLAIM-KEY.           EL173
00273      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CODE.          EL173
00274                                                                   EL173
00275      MOVE ZERO                   TO  PI-END-OF-FILE               EL173
00276                                      PI-TEMP-STORAGE-ITEM            CL**3
00277                                      PI-LAST-ITEM-NO.                CL**3
00278                                                                   EL173
00279      IF EIBAID = DFHCLEAR                                         EL173
00280          GO TO 9400-CLEAR.                                        EL173
00281                                                                   EL173
00282      IF PI-PROCESSOR-ID = 'LGXX'                                  EL173
00283          NEXT SENTENCE                                            EL173
00284      ELSE                                                         EL173
00285          EXEC CICS READQ TS                                       EL173
00286              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL173
00287              INTO    (SECURITY-CONTROL)                           EL173
00288              LENGTH  (SC-COMM-LENGTH)                             EL173
00289              ITEM    (SC-ITEM)                                    EL173
00290          END-EXEC                                                 EL173
00291          MOVE SC-CLAIMS-DISPLAY (17)  TO  PI-DISPLAY-CAP          EL173
00292          MOVE SC-CLAIMS-UPDATE  (17)  TO  PI-MODIFY-CAP           EL173
00293          IF NOT DISPLAY-CAP                                       EL173
00294              MOVE 'READ'              TO  SM-READ                 EL173
00295              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**3
00296              MOVE ER-0070             TO  EMI-ERROR               EL173
00297              GO TO 8100-SEND-INITIAL-MAP.                         EL173
00298                                                                   EL173
00299      GO TO 4000-BROWSE-CLAIM-MASTER.                                 CL**3
00300                                                                   EL173
00301      EJECT                                                        EL173
00302  0020-MAIN-LOGIC.                                                 EL173
00303 *    NOTE ******************************************************* EL173
00304 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL173
00305 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL173
00306 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL173
00307 *         *******************************************************.EL173
00308                                                                   EL173
00309      IF EIBAID = DFHCLEAR                                         EL173
00310          GO TO 9400-CLEAR.                                        EL173
00311                                                                   EL173
00312      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL173
00313          MOVE LOW-VALUES         TO  EL173AI                      EL173
00314          MOVE ER-0008            TO  EMI-ERROR                    EL173
00315          MOVE -1                 TO  APFKL                        EL173
00316          GO TO 8200-SEND-DATAONLY.                                   CL**3
00317                                                                   EL173
00318      EXEC CICS RECEIVE                                            EL173
00319          INTO   (EL173AI)                                         EL173
00320          MAPSET (WS-MAPSET-NAME)                                  EL173
00321          MAP    (WS-MAP-NAME)                                     EL173
00322      END-EXEC.                                                    EL173
00323                                                                   EL173
00324      IF APFKL GREATER ZERO                                        EL173
00325          IF EIBAID NOT = DFHENTER                                 EL173
00326              MOVE ER-0004           TO  EMI-ERROR                 EL173
00327              MOVE AL-UNBOF       TO  APFKA                        EL173
00328              MOVE -1             TO  APFKL                        EL173
00329              GO TO 8200-SEND-DATAONLY                                CL**3
00330            ELSE                                                   EL173
00331              IF APFKO IS NUMERIC                                  EL173
00332                AND APFKO IS GREATER THAN ZERO                     EL173
00333                AND APFKO IS LESS THAN '25'                        EL173
00334                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL173
00335                ELSE                                               EL173
00336                  MOVE ER-0029           TO  EMI-ERROR             EL173
00337                  MOVE AL-UNBOF       TO  APFKA                    EL173
00338                  MOVE -1             TO  APFKL                    EL173
00339                  GO TO 8200-SEND-DATAONLY.                           CL**3
00340                                                                   EL173
00341      IF EIBAID = DFHPF12                                          EL173
00342          MOVE 'EL010   '         TO  THIS-PGM                     EL173
00343          GO TO 9300-XCTL.                                         EL173
00344                                                                   EL173
00345      IF EIBAID = DFHPF23                                          EL173
00346          GO TO 9000-RETURN-CICS.                                  EL173
00347                                                                   EL173
00348      IF EIBAID = DFHPF24                                          EL173
00349          MOVE 'EL126   '         TO  THIS-PGM                     EL173
00350          GO TO 9300-XCTL.                                         EL173
00351                                                                   EL173
00352      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2                     EL173
00353          NEXT SENTENCE                                            EL173
00354        ELSE                                                       EL173
00355          MOVE ER-0008               TO  EMI-ERROR                 EL173
00356          MOVE -1                 TO  APFKL                        EL173
00357          GO TO 8200-SEND-DATAONLY.                                   CL**3
00358                                                                   EL173
00359      EJECT                                                        EL173
00360                                                                   EL173
00361      IF EIBAID = DFHPF1 OR DFHPF2                                 EL173
00362          GO TO 0110-MAIN-LOGIC.                                   EL173
00363                                                                   EL173
00364      IF PI-END-OF-FILE NOT = ZERO                                 EL173
00365          PERFORM 9400-CLEAR.                                      EL173
00366                                                                   EL173
00367      GO TO 4000-BROWSE-CLAIM-MASTER.                                 CL**3
00368                                                                   EL173
00369  0110-MAIN-LOGIC.                                                 EL173
00370      MOVE APAGEI                 TO  WS-TEMP-STORAGE-ITEM.        EL173
00371                                                                   EL173
00372      IF EIBAID = DFHPF1                                           EL173
00373        IF (PI-LAST-ITEM-NO GREATER THAN ZEROS) AND                   CL**3
00374           (WS-TEMP-STORAGE-ITEM GREATER THAN PI-LAST-ITEM-NO)        CL**3
00375          MOVE ER-0313                TO  EMI-ERROR                   CL**3
00376          MOVE -1                     TO  APFKL                       CL**3
00377          GO TO 8200-SEND-DATAONLY                                    CL**3
00378        ELSE                                                          CL**3
00379          ADD +1  TO  WS-TEMP-STORAGE-ITEM                         EL173
00380          GO TO 0120-MAIN-LOGIC.                                   EL173
00381                                                                   EL173
00382      IF EIBAID = DFHPF2                                           EL173
00383        IF WS-TEMP-STORAGE-ITEM GREATER THAN +1                       CL**3
00384          SUBTRACT +1 FROM WS-TEMP-STORAGE-ITEM                    EL173
00385          GO TO 0120-MAIN-LOGIC                                       CL**3
00386        ELSE                                                          CL**3
00387          MOVE ER-0047                TO  EMI-ERROR.                  CL**3
00388          MOVE -1                     TO  APFKL                       CL**3
00389          GO TO 8200-SEND-DATAONLY.                                   CL**3
00390                                                                   EL173
00391                                                                   EL173
00392  0120-MAIN-LOGIC.                                                 EL173
00393      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL173
00394                                                                      CL**3
00395      EXEC CICS HANDLE CONDITION                                      CL**3
00396          ITEMERR (0130-BROWSE-CLAIM-MASTER)                          CL**3
00397      END-EXEC.                                                       CL**3
00398                                                                   EL173
00399      EXEC CICS READQ TS                                           EL173
00400          QUEUE  (WS-TEMP-STORAGE-KEY)                             EL173
00401          ITEM   (WS-TEMP-STORAGE-ITEM)                            EL173
00402          INTO   (EL173AI)                                         EL173
00403          LENGTH (WS-TS-LENGTH) END-EXEC                           EL173
00404                                                                   EL173
00405      MOVE WS-TEMP-STORAGE-ITEM  TO  APAGEO.                       EL173
00406                                                                   EL173
00407      GO TO 8100-SEND-INITIAL-MAP.                                    CL**3
00408                                                                      CL**3
00409  0130-BROWSE-CLAIM-MASTER.                                           CL**3
00410                                                                      CL**3
00411      GO TO 4000-BROWSE-CLAIM-MASTER.                                 CL**3
00412                                                                   EL173
00413      EJECT                                                        EL173
00414  4000-BROWSE-CLAIM-MASTER SECTION.                                EL173
00415      MOVE LOW-VALUES             TO  EL173AI.                     EL173
00416                                                                   EL173
00417      EXEC CICS STARTBR                                            EL173
00418          DATASET (WS-CLAIM-MASTER-DSID)                           EL173
00419          RIDFLD  (PI-CLAIM-KEY)                                   EL173
00420          GTEQ    END-EXEC                                         EL173
00421                                                                   EL173
00422      SET EL173A-INDEX TO +1.                                      EL173
00423                                                                   EL173
00424  4100-READNEXT.                                                   EL173
00425      MOVE PI-CLAIM-KEY           TO  PI-PREV-CLAIM-KEY.           EL173
00426                                                                   EL173
00427      EXEC CICS READNEXT                                           EL173
00428          DATASET (WS-CLAIM-MASTER-DSID)                           EL173
00429          RIDFLD  (PI-CLAIM-KEY)                                   EL173
00430          SET     (ADDRESS OF CLAIM-MASTER) END-EXEC                  CL**3
00431                                                                   EL173
00432      IF PI-CK-COMPANY-CODE NOT = PI-COMPANY-CD                    EL173
00433          GO TO 4800-END-OF-FILE.                                  EL173
00434                                                                   EL173
00435      IF WS-READNEXT-SW GREATER THAN ZERO                          EL173
00436          GO TO 4900-ENDBROWSE.                                    EL173
00437                                                                   EL173
00438      IF CL-SUPV-ATTN-CD NOT = 'Y'                                 EL173
00439          GO TO 4100-READNEXT.                                     EL173
00440                                                                   EL173
00441      IF NOT PI-NO-CARRIER-SECURITY                                EL173
00442          IF PI-CK-CARRIER NOT = PI-CARRIER-SECURITY               EL173
00443             GO TO 4100-READNEXT.                                  EL173
00444                                                                   EL173
00445      MOVE CL-CLAIM-NO    TO  EL173A-CLAIM   (EL173A-INDEX).       EL173
00446      MOVE CL-CARRIER     TO  EL173A-CARRIER (EL173A-INDEX).       EL173
00447      MOVE CL-CERT-NO     TO  EL173A-CERT-NO (EL173A-INDEX).       EL173
00448      MOVE CL-CLAIM-TYPE  TO  EL173A-TYPE    (EL173A-INDEX).       EL173
00449      MOVE CL-PROCESSOR-ID  TO  EL173A-BY    (EL173A-INDEX).       EL173
00450                                                                   EL173
00451      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES                     EL173
00452          MOVE CL-FILE-ESTABLISH-DT  TO  DC-BIN-DATE-1             EL173
00453          MOVE SPACES             TO  DC-OPTION-CODE               EL173
00454          PERFORM 8500-DATE-CONVERSION                             EL173
00455          MOVE DC-GREG-DATE-1-EDIT TO EL173A-EDATE (EL173A-INDEX). EL173
00456                                                                   EL173
00457      IF CL-INCURRED-DT NOT = LOW-VALUES                           EL173
00458          MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1                EL173
00459          MOVE SPACES             TO  DC-OPTION-CODE               EL173
00460          PERFORM 8500-DATE-CONVERSION                             EL173
00461          MOVE DC-GREG-DATE-1-EDIT TO EL173A-IDATE (EL173A-INDEX). EL173
00462                                                                   EL173
00463      MOVE CL-FILE-LOCATION  TO  EL173A-FILE (EL173A-INDEX).       EL173
00464                                                                   EL173
00465      IF EL173A-INDEX LESS THAN +18                                EL173
00466          SET EL173A-INDEX UP BY +1                                EL173
00467          GO TO 4100-READNEXT.                                     EL173
00468                                                                   EL173
00469      MOVE +1                     TO  WS-READNEXT-SW.              EL173
00470      GO TO 4100-READNEXT.                                         EL173
00471                                                                   EL173
00472  4800-END-OF-FILE.                                                EL173
00473      MOVE +1                     TO  PI-END-OF-FILE.              EL173
00474      MOVE PI-TEMP-STORAGE-ITEM   TO  PI-LAST-ITEM-NO.                CL**3
00475                                                                   EL173
00476      MOVE ER-0313                TO  EMI-ERROR.                   EL173
00477                                                                   EL173
00478  4900-ENDBROWSE.                                                  EL173
00479      EXEC CICS ENDBR                                              EL173
00480          DATASET (WS-CLAIM-MASTER-DSID) END-EXEC                  EL173
00481                                                                   EL173
00482      MOVE -1                     TO  APFKL.                       EL173
00483                                                                   EL173
00484      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL173
00485                                                                   EL173
00486      EXEC CICS WRITEQ TS                                          EL173
00487          QUEUE  (WS-TEMP-STORAGE-KEY)                             EL173
00488          ITEM   (PI-TEMP-STORAGE-ITEM)                            EL173
00489          FROM   (EL173AI)                                         EL173
00490          LENGTH (WS-TS-LENGTH) END-EXEC                           EL173
00491                                                                   EL173
00492      MOVE PI-TEMP-STORAGE-ITEM  TO  APAGEO.                       EL173
00493                                                                   EL173
00494      GO TO 8100-SEND-INITIAL-MAP.                                    CL**3
00495                                                                   EL173
00496      EJECT                                                        EL173
00497  8100-SEND-INITIAL-MAP SECTION.                                   EL173
00498      IF EMI-ERROR NOT = ZERO                                      EL173
00499          PERFORM 9900-ERROR-FORMAT.                               EL173
00500                                                                   EL173
00501      MOVE -1                     TO  APFKL.                          CL**3
00502      MOVE EIBTIME                TO  TIME-IN.                     EL173
00503      MOVE SAVE-DATE              TO  ADATEO.                      EL173
00504      MOVE TIME-OUT               TO  ATIMEO.                      EL173
00505      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     EL173
00506                                                                   EL173
00507      EXEC CICS SEND                                               EL173
00508          FROM   (EL173AI)                                         EL173
00509          MAPSET (WS-MAPSET-NAME)                                  EL173
00510          MAP    (WS-MAP-NAME)                                     EL173
00511          CURSOR ERASE END-EXEC.                                   EL173
00512                                                                   EL173
00513      GO TO 9100-RETURN-TRAN.                                         CL**3
00514                                                                   EL173
00515      EJECT                                                        EL173
00516  8200-SEND-DATAONLY SECTION.                                      EL173
00517      IF EMI-ERROR NOT = ZERO                                      EL173
00518          PERFORM 9900-ERROR-FORMAT.                               EL173
00519                                                                   EL173
00520      MOVE EIBTIME                TO  TIME-IN.                     EL173
00521                                                                   EL173
00522          MOVE SAVE-DATE          TO  ADATEO                       EL173
00523          MOVE TIME-OUT           TO  ATIMEO                       EL173
00524          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                     EL173
00525                                                                   EL173
00526      EXEC CICS SEND DATAONLY                                      EL173
00527          FROM   (EL173AI)                                         EL173
00528          MAPSET (WS-MAPSET-NAME)                                  EL173
00529          MAP    (WS-MAP-NAME)                                     EL173
00530          CURSOR END-EXEC.                                         EL173
00531                                                                   EL173
00532      GO TO 9100-RETURN-TRAN.                                         CL**3
00533                                                                   EL173
00534      EJECT                                                        EL173
00535  8300-SEND-TEXT SECTION.                                          EL173
00536      EXEC CICS SEND TEXT                                          EL173
00537          FROM   (LOGOFF-TEXT)                                     EL173
00538          LENGTH (LOGOFF-LENGTH)                                   EL173
00539          ERASE  FREEKB END-EXEC.                                  EL173
00540                                                                   EL173
00541      EXEC CICS RETURN                                             EL173
00542          END-EXEC.                                                EL173
00543                                                                   EL173
00544  8300-EXIT.                                                       EL173
00545      EXIT.                                                        EL173
00546                                                                   EL173
00547      EJECT                                                        EL173
00548  8500-DATE-CONVERSION SECTION.                                    EL173
00549      EXEC CICS LINK                                               EL173
00550          PROGRAM  ('ELDATCV')                                     EL173
00551          COMMAREA (DATE-CONVERSION-DATA)                          EL173
00552          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL173
00553                                                                   EL173
00554  8500-EXIT.                                                       EL173
00555      EXIT.                                                        EL173
00556                                                                   EL173
00557      EJECT                                                        EL173
00558  9000-RETURN-CICS SECTION.                                        EL173
00559      MOVE 'EL005'                TO  THIS-PGM.                    EL173
00560      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL173
00561      PERFORM 9300-XCTL.                                           EL173
00562                                                                   EL173
00563  9000-EXIT.                                                       EL173
00564      EXIT.                                                        EL173
00565                                                                   EL173
00566  9100-RETURN-TRAN SECTION.                                        EL173
00567      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL173
00568      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL173
00569                                                                   EL173
00570      EXEC CICS RETURN                                             EL173
00571          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL173
00572          LENGTH   (PI-COMM-LENGTH)                                EL173
00573          TRANSID  (WS-TRANS-ID) END-EXEC.                         EL173
00574                                                                   EL173
00575  9100-EXIT.                                                       EL173
00576      EXIT.                                                        EL173
00577                                                                   EL173
00578  9300-XCTL SECTION.                                               EL173
00579      EXEC CICS HANDLE CONDITION                                   EL173
00580          QIDERR (9300-NEXT-SENTENCE) END-EXEC.                    EL173
00581                                                                   EL173
00582      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL173
00583                                                                   EL173
00584      EXEC CICS DELETEQ TS                                         EL173
00585          QUEUE (WS-TEMP-STORAGE-KEY) END-EXEC.                    EL173
00586                                                                   EL173
00587  9300-NEXT-SENTENCE.                                              EL173
00588      MOVE DFHENTER               TO  EIBAID.                      EL173
00589                                                                   EL173
00590      EXEC CICS XCTL                                               EL173
00591          PROGRAM  (THIS-PGM)                                      EL173
00592          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL173
00593          LENGTH   (PI-COMM-LENGTH) END-EXEC.                      EL173
00594                                                                   EL173
00595  9300-EXIT.                                                       EL173
00596      EXIT.                                                        EL173
00597                                                                   EL173
00598      EJECT                                                        EL173
00599  9400-CLEAR SECTION.                                              EL173
00600      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL173
00601      PERFORM 9300-XCTL.                                           EL173
00602                                                                   EL173
00603  9400-EXIT.                                                       EL173
00604      EXIT.                                                        EL173
00605                                                                   EL173
00606  9600-PGMIDERR SECTION.                                           EL173
00607      EXEC CICS HANDLE CONDITION                                   EL173
00608          PGMIDERR (8300-SEND-TEXT) END-EXEC.                      EL173
00609                                                                   EL173
00610      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL173
00611                                      LOGOFF-PGM.                  EL173
00612                                                                   EL173
00613      MOVE 'EL005'                TO  THIS-PGM.                    EL173
00614      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL173
00615      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL173
00616      PERFORM 9300-XCTL.                                           EL173
00617                                                                   EL173
00618  9600-EXIT.                                                       EL173
00619      EXIT.                                                        EL173
00620                                                                   EL173
00621      EJECT                                                        EL173
00622  9900-ERROR-FORMAT SECTION.                                       EL173
00623      EXEC CICS LINK                                               EL173
00624          PROGRAM  ('EL001')                                       EL173
00625          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL173
00626          LENGTH   (EMI-COMM-LENGTH) END-EXEC.                     EL173
00627                                                                   EL173
00628  9900-EXIT.                                                       EL173
00629      EXIT.                                                        EL173
00630                                                                   EL173
00631      EJECT                                                        EL173
00632  9990-ERROR SECTION.                                              EL173
00633      MOVE DFHEIBLK TO EMI-LINE1.                                  EL173
00634      EXEC CICS LINK                                               EL173
00635          PROGRAM  ('EL004')                                       EL173
00636          COMMAREA (EMI-LINE1)                                     EL173
00637          LENGTH   (72) END-EXEC.                                  EL173
00638                                                                   EL173
00639      GO TO 8200-SEND-DATAONLY.                                       CL**3
00640                                                                   EL173
00641  9990-EXIT.                                                       EL173
00642      EXIT.                                                        EL173
00643                                                                   EL173
00644  9995-SECURITY-VIOLATION.                                         EL173
00645                              COPY ELCSCTP.                        EL173
00646                                                                   EL173
00647  9995-EXIT.                                                       EL173
00648      EXIT.                                                        EL173
00649                                                                   EL173
