00001  ID DIVISION.                                                     05/06/97
00002                                                                   EL601
00003  PROGRAM-ID.                 EL601.                                  LV008
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 12/06/94 08:32:30.                    CL**6
00007 *                            VMOD=2.008.                             CL**8
00008 *                                                                 EL601
00008 *                                                                 EL601
00009 *AUTHOR.     LOGIC,INC.                                              CL**6
00010 *            DALLAS, TEXAS.                                          CL**6
00011                                                                   EL601
00012 *DATE-COMPILED.                                                      CL**6
00013                                                                   EL601
00014 *SECURITY.   *****************************************************   CL**6
00015 *            *                                                   *   CL**6
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**6
00017 *            *                                                   *   CL**6
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00021 *            *                                                   *   CL**6
00022 *            *****************************************************   CL**6
00023                                                                   EL601
00024 *REMARKS.    TRANSACTION - EG1A - SYSTEM MAINTENANCE MENU.           CL**4
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
030612******************************************************************
00025                                                                   EL601
00026  ENVIRONMENT DIVISION.                                            EL601
00027                                                                   EL601
00028      EJECT                                                        EL601
00029  DATA DIVISION.                                                   EL601
00030  WORKING-STORAGE SECTION.                                         EL601
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL601
00032  77  FILLER  PIC X(32)  VALUE '*    EL601 WORKING STORAGE     *'. EL601
00033  77  FILLER  PIC X(32)  VALUE '*************V/M 2.008 *********'.    CL**8
00034                                                                   EL601
00035  01  WS-DATE-AREA.                                                EL601
00036      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL601
00037      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL601
00038                                                                   EL601
00039  01  MISC-WORK-AREAS.                                             EL601
00040                                                                      CL**7
00041      12  SCREEN-SELECT-CHANGES.                                      CL**4
00042          16  SCREEN-RESERVE-10   PIC X(32) VALUE                     CL**8
00043                  ' 10. RESERVED'.                                    CL**8
00044          16  SCREEN-RESERVE-11   PIC X(32) VALUE                     CL**8
00045                  ' 11. RESERVED'.                                    CL**8
00046          16  SCREEN-RESERVE-13   PIC X(32) VALUE                     CL**8
00047                  ' 13. RESERVED'.                                    CL**8
00048          16  SCREEN-RESERVE-14   PIC X(32) VALUE                     CL**8
00049                  ' 14. RESERVED'.                                    CL**8
00050          16  SCREEN-RESERVE-15   PIC X(32) VALUE                     CL**8
00051                  ' 15. RESERVED'.                                    CL**8
00052          16  SCREEN-RESERVE-16   PIC X(32) VALUE                     CL**8
00053                  ' 16. RESERVED'.                                    CL**8
00054          16  SCREEN-RESERVE-17   PIC X(32) VALUE                     CL**8
00055                  ' 17. RESERVED'.                                    CL**8
00056          16  SCREEN-SELECT-10    PIC X(32) VALUE                     CL**8
00057                  ' 10. GENERAL LEDGER TABLE LOOKUP'.                 CL**4
00058          16  SCREEN-SELECT-11    PIC X(32) VALUE                     CL**8
00059                  ' 11. RETRO HISTORY MAINTENANCE'.                   CL**8
00060          16  SCREEN-SELECT-12    PIC X(32) VALUE                     CL**8
00061                  ' 12. BANK MASTER MAINTENANCE'.                     CL**8
00062 *        16  SCREEN-SELECT-13    PIC X(32) VALUE                     CL**8
00063 *                ' 13. RETRO MASTER MAINTENANCE'.                    CL**8
051805         16  SCREEN-SELECT-13    PIC X(32) VALUE                     CL**8
051805                 ' 13. LIFE CLAIM INT. MAINT   '.                    CL**8
00064          16  SCREEN-SELECT-14    PIC X(29) VALUE                     CL**8
00065                  ' 14. RESIDENT STATE TAX MAINT'.                    CL**6
00066          16  SCREEN-SELECT-15    PIC X(38) VALUE                     CL**8
00067                  ' 15. ACCOUNT RESIDENT STATE COMM MAINT'.           CL**6
00068          16  SCREEN-SELECT-16    PIC X(37) VALUE                     CL**8
00069                  ' 16. RESIDENT ST COMMISSION CAP MAINT'.            CL**6
00070          16  SCREEN-SELECT-17    PIC X(37) VALUE                     CL**8
00071                  ' 17. CODES TABLE MAINTENANCE'.                     CL**8
00072          16  SCREEN-SELECT-18    PIC X(37) VALUE                     CL**8
00073                  ' 18. HIERARCHY TABLE'.                             CL**8
00074                                                                      CL**8
00075      12  MAP-EL601A          PIC X(8)    VALUE 'EL601A'.          EL601
00076      12  MAPSET-EL601S       PIC X(8)    VALUE 'EL601S'.          EL601
00077      12  TRANS-EXA1          PIC X(4)    VALUE 'EXA1'.            EL601
00078      12  PGM-EL601           PIC X(8)    VALUE 'EL601'.           EL601
00079      12  PGM-NAME            PIC X(8).                            EL601
00080      12  TIME-IN             PIC S9(7).                           EL601
00081      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL601
00082          16  FILLER          PIC X.                               EL601
00083          16  TIME-OUT        PIC 99V99.                           EL601
00084          16  FILLER          PIC X(2).                            EL601
00085      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL601
00086      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           EL601
00087      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           EL601
00088                                                                      CL**8
00089      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.           EL601
00090      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.           EL601
00091      12  XCTL-EL102          PIC X(8)    VALUE 'EL102'.           EL601
00092      12  XCTL-EL103          PIC X(8)    VALUE 'EL103'.           EL601
00093      12  XCTL-EL104          PIC X(8)    VALUE 'EL104'.           EL601
00094      12  XCTL-EL105          PIC X(8)    VALUE 'EL105'.           EL601
00095      12  XCTL-EL106          PIC X(8)    VALUE 'EL106'.           EL601
00096      12  XCTL-EL107          PIC X(8)    VALUE 'EL107'.           EL601
00097      12  XCTL-EL108          PIC X(8)    VALUE 'EL108'.           EL601
00098      12  XCTL-EL110          PIC X(8)    VALUE 'EL110'.           EL601
00099      12  XCTL-EL111          PIC X(8)    VALUE 'EL111'.           EL601
00100      12  XCTL-EL112          PIC X(8)    VALUE 'EL112'.           EL601
           12  XCTL-EL115          PIC X(8)    VALUE 'EL115'.
00101      12  XCTL-EL119          PIC X(8)    VALUE 'EL119'.           EL601
00102      12  XCTL-EL126          PIC X(8)    VALUE 'EL800'.           EL601
00103      12  XCTL-EL158          PIC X(8)    VALUE 'EL158'.              CL**4
00103      12  XCTL-EL159          PIC X(8)    VALUE 'EL159'.              CL**4
00104      12  XCTL-EL400          PIC X(8)    VALUE 'EL400NCB'.           CL**6
00105      12  XCTL-EL602          PIC X(8)    VALUE 'EL602'.           EL601
00106      12  XCTL-EL603          PIC X(8)    VALUE 'EL603'.           EL601
00107      12  XCTL-EL604          PIC X(8)    VALUE 'EL604'.           EL601
00108      12  XCTL-EL605          PIC X(8)    VALUE 'EL605'.              CL**4
00109      12  XCTL-EL606          PIC X(8)    VALUE 'EL606'.              CL**4
00110      12  XCTL-EL607          PIC X(8)    VALUE 'EL607'.              CL**8
00111      12  XCTL-EL608          PIC X(8)    VALUE 'EL608 '.             CL**8
00112      12  XCTL-EL610          PIC X(8)    VALUE 'EL610'.           EL601
00113      12  XCTL-EL611          PIC X(8)    VALUE 'EL611'.              CL**7
00114      12  XCTL-EL613          PIC X(8)    VALUE 'EL613'.              CL**7
00115      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           EL601
00116      12  XCTL-EL650          PIC X(8)    VALUE 'EL650'.           EL601
00117      12  XCTL-EL651          PIC X(8)    VALUE 'EL651'.           EL601
00118      12  XCTL-EL652          PIC X(8)    VALUE 'EL652'.           EL601
00119      12  XCTL-EL653          PIC X(8)    VALUE 'EL653'.           EL601
00120      12  XCTL-EL654          PIC X(8)    VALUE 'EL654'.           EL601
00121      12  XCTL-EL656          PIC X(8)    VALUE 'EL656'.           EL601
00122      12  XCTL-EL658          PIC X(8)    VALUE 'EL658'.           EL601
00123      12  XCTL-EL659          PIC X(8)    VALUE 'EL659'.              CL**3
00124      12  XCTL-EL1062         PIC X(8)    VALUE 'EL1062'.             CL**6
00125      12  XCTL-EL1064         PIC X(8)    VALUE 'EL1064'.             CL**6
00126                                                                      CL**8
00127      12  ER-0002             PIC X(4)    VALUE '0002'.            EL601
00128      12  ER-2370             PIC X(4)    VALUE '2370'.            EL601
00129      12  ER-2371             PIC X(4)    VALUE '2371'.            EL601
00130      12  ER-7000             PIC X(4)    VALUE '7000'.            EL601
00131      12  ER-7001             PIC X(4)    VALUE '7001'.            EL601
00132      12  ER-7002             PIC X(4)    VALUE '7002'.            EL601
00133      12  ER-7003             PIC X(4)    VALUE '7003'.            EL601
00134      12  ER-7008             PIC X(4)    VALUE '7008'.            EL601
00135      12  ER-7448             PIC X(4)    VALUE '7448'.               CL**7
00136                                                                   EL601
00137      EJECT                                                        EL601
00138                              COPY ELCLOGOF.                          CL**4
00139                                                                   EL601
00140      EJECT                                                        EL601
00141                              COPY ELCDATE.                           CL**4
00142                                                                   EL601
00143      EJECT                                                        EL601
00144                              COPY ELCATTR.                           CL**4
00145                                                                   EL601
00146      EJECT                                                        EL601
00147                              COPY ELCEMIB.                           CL**4
00148                                                                   EL601
00149      EJECT                                                        EL601
00150                              COPY ELCINTF.                           CL**4
00151      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL601
00152          16  PI-FILE-ID                    PIC XX.                EL601
00153          16  FILLER                        PIC X(638).               CL**6
00154                                                                   EL601
00155      EJECT                                                        EL601
00156                              COPY ELCAID.                            CL**4
00157  01  FILLER    REDEFINES DFHAID.                                  EL601
00158      12  FILLER              PIC X(8).                            EL601
00159      12  PF-VALUES           PIC X       OCCURS 2.                EL601
00160                                                                   EL601
00161      EJECT                                                        EL601
00162                              COPY EL601S.                            CL**4
00163                                                                   EL601
00164      EJECT                                                        EL601
00165  LINKAGE SECTION.                                                 EL601
00166  01  DFHCOMMAREA             PIC X(1024).                         EL601
00167                                                                   EL601
00168      EJECT                                                        EL601
00169  PROCEDURE DIVISION.                                              EL601
00170                                                                   EL601
00171      MOVE DFHCOMMAREA   TO PROGRAM-INTERFACE-BLOCK.               EL601
00172                                                                   EL601
00173      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL601
00174      MOVE '5'                    TO DC-OPTION-CODE.               EL601
00175      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL601
00176      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL601
00177      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL601
00178                                                                   EL601
00179      IF EIBCALEN = 0                                              EL601
00180          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL601
00181                                                                   EL601
00182      IF PI-CALLING-PROGRAM NOT = PGM-EL601                        EL601
00183          IF PI-RETURN-TO-PROGRAM NOT = PGM-EL601                  EL601
00184              MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6    EL601
00185              MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5    EL601
00186              MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4    EL601
00187              MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3    EL601
00188              MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2    EL601
00189              MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1    EL601
00190              MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM  EL601
00191              MOVE PGM-EL601              TO PI-CALLING-PROGRAM       CL**8
00192          ELSE                                                     EL601
00193              MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM    EL601
00194              MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM  EL601
00195              MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1    EL601
00196              MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2    EL601
00197              MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3    EL601
00198              MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4    EL601
00199              MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5    EL601
00200              MOVE SPACES                 TO PI-SAVED-PROGRAM-6.   EL601
00201                                                                   EL601
00202      EXEC CICS HANDLE CONDITION                                   EL601
00203          PGMIDERR  (9600-PGMID-ERROR)                             EL601
00204          ERROR     (9990-ABEND)                                   EL601
00205      END-EXEC.                                                       CL**6
00206                                                                   EL601
00207      IF EIBTRNID  = TRANS-EXA1                                    EL601
00208          GO TO 0100-SAME-TRAN.                                       CL**4
00209                                                                   EL601
00210      GO TO 8100-SEND-INITIAL-MAP.                                 EL601
00211                                                                   EL601
00212  0100-SAME-TRAN.                                                     CL**4
00213      IF EIBAID = DFHCLEAR                                         EL601
00214          GO TO 9400-CLEAR.                                        EL601
00215                                                                   EL601
00216      EJECT                                                        EL601
00217  0200-RECEIVE.                                                    EL601
00218      MOVE LOW-VALUES   TO EL601AI.                                EL601
00219                                                                   EL601
00220      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL601
00221          MOVE -1                 TO SELECTL                       EL601
00222          MOVE ER-7008         TO EMI-ERROR                        EL601
00223          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL601
00224          GO TO 8200-SEND-DATAONLY.                                EL601
00225                                                                   EL601
00226      EXEC CICS RECEIVE                                            EL601
00227          MAP      (MAP-EL601A)                                    EL601
00228          MAPSET   (MAPSET-EL601S)                                 EL601
00229          INTO     (EL601AI)                                       EL601
00230      END-EXEC.                                                       CL**6
00231                                                                   EL601
00232      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL601
00233                                                                   EL601
00234      IF EIBAID = DFHPF12                                             CL**5
00235         GO TO 9500-HELP                                              CL**5
00236      ELSE                                                         EL601
00237         IF EIBAID = DFHPF23                                          CL**5
00238            GO TO 8810-PF23                                           CL**5
00239         ELSE                                                         CL**5
00240            IF EIBAID = DFHPF24                                       CL**5
00241               GO TO 9200-PF24.                                       CL**5
00242                                                                   EL601
00243      IF PFKEYL = ZEROS                                            EL601
00244          NEXT SENTENCE                                            EL601
00245      ELSE                                                         EL601
00246          IF PFKEYI = '12'                                            CL**5
00247             GO TO 9500-HELP                                          CL**5
00248          ELSE                                                     EL601
00249             IF PFKEYI = '23'                                         CL**5
00250                GO TO 8810-PF23                                       CL**5
00251             ELSE                                                     CL**5
00252                IF PFKEYI = '24'                                      CL**5
00253                   GO TO 9200-PF24.                                   CL**5
00254                                                                   EL601
00255      IF EIBAID NOT = DFHENTER                                     EL601
00256          MOVE ER-0002         TO EMI-ERROR                        EL601
00257          GO TO 0320-INPUT-ERROR.                                  EL601
00258                                                                   EL601
00259      IF SELECTL = 0                                               EL601
00260          MOVE ER-0002         TO EMI-ERROR                        EL601
00261          GO TO 0320-INPUT-ERROR.                                  EL601
00262                                                                   EL601
00263  0310-CHECK-PFKEYS.                                               EL601
00264      IF SELECTI = '01'                                            EL601
00265          MOVE XCTL-EL650 TO PGM-NAME                              EL601
00266          GO TO 9300-XCTL.                                         EL601
00267                                                                   EL601
00268      IF SELECTI = '02'                                            EL601
00269          MOVE XCTL-EL652 TO PGM-NAME                              EL601
00270          GO TO 9300-XCTL.                                         EL601
00271                                                                   EL601
00272      IF SELECTI = '03'                                            EL601
00273          MOVE XCTL-EL656 TO PGM-NAME                              EL601
00274          GO TO 9300-XCTL.                                         EL601
00275                                                                   EL601
00276      IF SELECTI = '04'                                            EL601
00277          MOVE XCTL-EL651 TO PGM-NAME                              EL601
00278          GO TO 9300-XCTL.                                         EL601
00279                                                                   EL601
00280      IF SELECTI = '05'                                            EL601
00281          MOVE XCTL-EL653 TO PGM-NAME                              EL601
00282          GO TO 9300-XCTL.                                         EL601
00283                                                                   EL601
00284      IF SELECTI = '06'                                            EL601
00285          MOVE XCTL-EL658 TO PGM-NAME                              EL601
00286          GO TO 9300-XCTL.                                         EL601
00287                                                                   EL601
00288      IF SELECTI = '07'                                            EL601
00289          MOVE XCTL-EL610 TO PGM-NAME                              EL601
00290          GO TO 9300-XCTL.                                            CL**3
00291                                                                      CL**3
00292      IF SELECTI = '08'                                               CL**3
00293          MOVE XCTL-EL659 TO PGM-NAME                                 CL**3
00294          GO TO 9300-XCTL.                                         EL601
00295                                                                   EL601
00296      IF SELECTI = '09'                                               CL**4
00297          MOVE XCTL-EL158 TO PGM-NAME                                 CL**4
00298          GO TO 9300-XCTL.                                            CL**4
00299                                                                      CL**4
00300      IF SELECTI = '10'                                               CL**4
              MOVE XCTL-EL159 TO PGM-NAME
00303              GO TO 9300-XCTL.                                        CL**4
00304                                                                      CL**4
00305      IF SELECTI = '11'                                               CL**4
00306          IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                         CL**4
00307             MOVE XCTL-EL607 TO PGM-NAME                              CL**4
00308             GO TO 9300-XCTL.                                         CL**4
00309                                                                      CL**4
00310      IF SELECTI = '12'                                               CL**7
00311        IF PI-AR-PROCESSING                                           CL**7
00312           MOVE XCTL-EL611 TO PGM-NAME                                CL**7
00313           GO TO 9300-XCTL                                            CL**8
00314        ELSE                                                          CL**7
00315           MOVE ER-7448   TO EMI-ERROR                                CL**7
00316           GO TO 0320-INPUT-ERROR.                                    CL**8
00317                                                                      CL**7
051805     IF SELECTI = '13'
062121        IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
051805           MOVE XCTL-EL605       TO PGM-NAME
051805           GO TO 9300-XCTL
051805        END-IF
051805     END-IF
00322                                                                      CL**4
00318      IF SELECTI = '13'                                               CL**4
00319          IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                         CL**4
00320              MOVE XCTL-EL606 TO PGM-NAME                             CL**4
00321              GO TO 9300-XCTL.                                        CL**4
00322                                                                      CL**4
00323      IF SELECTI = '14'                                               CL**6
00324          IF PI-COMPANY-ID  =  'DMD'                                  CL**6
00325              MOVE XCTL-EL1062 TO PGM-NAME                            CL**6
00326              GO TO 9300-XCTL.                                        CL**6
00327                                                                      CL**6
00328      IF SELECTI = '15'                                               CL**6
00329          IF PI-COMPANY-ID  =  'DMD'                                  CL**6
00330              MOVE XCTL-EL608 TO PGM-NAME                             CL**6
00331              GO TO 9300-XCTL.                                        CL**6
00332                                                                      CL**6
00333      IF SELECTI = '16'                                               CL**6
00334          IF PI-COMPANY-ID  =  'DMD'                                  CL**6
00335              MOVE XCTL-EL1064 TO PGM-NAME                            CL**6
00336              GO TO 9300-XCTL.                                        CL**6
00337                                                                      CL**6
00338 *    IF SELECTI = '17'                                               CL**6
00339 *        IF PI-COMPANY-ID  =  'DMD'                                  CL**6
00340 *            MOVE XCTL-ELXXXX TO PGM-NAME (CODES TABLE MAINT)        CL**6
00341 *            GO TO 9300-XCTL.                                        CL**6
00342 *                                                                    CL**6
00343 *CODE ABOVE COMMENTED OUT PENDING SPECS APPROVAL                     CL**6
00344 *                                                                    CL**6
00345                                                                      CL**6
00346      IF SELECTI = '18'                                               CL**6
00347          IF PI-COMPANY-ID  =  'NCB'                                  CL**6
00348              MOVE XCTL-EL400 TO PGM-NAME                             CL**6
00349              GO TO 9300-XCTL.                                        CL**6
00350                                                                      CL**6
00351      IF SELECTI = '21'                                            EL601
00352          MOVE XCTL-EL102 TO PGM-NAME                              EL601
00353          GO TO 9300-XCTL.                                         EL601
00354                                                                   EL601
00355      IF SELECTI = '22'                                            EL601
00356          MOVE XCTL-EL105 TO PGM-NAME                              EL601
00357          GO TO 9300-XCTL.                                         EL601
00358                                                                      CL**7
00359      IF SELECTI = '23'                                               CL**7
00360        IF PI-AR-PROCESSING                                           CL**7
00361           MOVE XCTL-EL613 TO PGM-NAME                                CL**7
00362           GO TO 9300-XCTL                                            CL**8
00363        ELSE                                                          CL**7
00364           MOVE ER-7448   TO EMI-ERROR                                CL**7
00365           GO TO 0320-INPUT-ERROR.                                    CL**8
00366                                                                   EL601
00367      IF SELECTI = '24'                                            EL601
00368          MOVE XCTL-EL106 TO PGM-NAME                              EL601
00369          GO TO 9300-XCTL.                                         EL601

           IF SELECTI = '25'
              MOVE XCTL-EL115          TO PGM-NAME
              GO TO 9300-XCTL
           END-IF

00371      IF SELECTI = '26'                                            EL601
00372          MOVE XCTL-EL107 TO PGM-NAME                              EL601
00373          GO TO 9300-XCTL.                                         EL601
00374                                                                   EL601
00375      IF SELECTI = '27'                                            EL601
00376          MOVE XCTL-EL604 TO PGM-NAME                              EL601
00377          GO TO 9300-XCTL.                                         EL601
00378                                                                   EL601
00379      IF SELECTI = '28'                                            EL601
00380          MOVE XCTL-EL602 TO PGM-NAME                              EL601
00381          GO TO 9300-XCTL.                                         EL601
00382                                                                   EL601
00383      IF SELECTI = '29'                                            EL601
00384          MOVE XCTL-EL603 TO PGM-NAME                              EL601
00385          GO TO 9300-XCTL.                                         EL601
00386                                                                   EL601
00387      IF SELECTI = '30'                                            EL601
00388          MOVE XCTL-EL654 TO PGM-NAME                              EL601
00389          GO TO 9300-XCTL.                                         EL601
00390                                                                   EL601
00391      IF SELECTI = '31'                                            EL601
00392          MOVE XCTL-EL103 TO PGM-NAME                              EL601
00393          GO TO 9300-XCTL.                                         EL601
00394                                                                   EL601
00395      IF SELECTI = '32'                                            EL601
00396          MOVE XCTL-EL119 TO PGM-NAME                              EL601
00397          GO TO 9300-XCTL.                                         EL601
00398                                                                   EL601
00399      IF SELECTI = '33'                                            EL601
00400          MOVE XCTL-EL108 TO PGM-NAME                              EL601
00401          GO TO 9300-XCTL.                                         EL601
00402                                                                   EL601
00403      IF SELECTI = '34'                                            EL601
00404          MOVE XCTL-EL104 TO PGM-NAME                              EL601
00405          GO TO 9300-XCTL.                                         EL601
00406                                                                   EL601
00407      IF SELECTI = '35'                                            EL601
00408          MOVE XCTL-EL110 TO PGM-NAME                              EL601
00409          GO TO 9300-XCTL.                                         EL601
00410                                                                   EL601
00411      IF SELECTI = '36'                                            EL601
00412          MOVE XCTL-EL111 TO PGM-NAME                              EL601
00413          GO TO 9300-XCTL.                                         EL601
00414                                                                   EL601
00415      IF SELECTI = '37'                                            EL601
00416          MOVE XCTL-EL112 TO PGM-NAME                              EL601
00417          GO TO 9300-XCTL.                                         EL601
00418                                                                   EL601
00419      MOVE ER-7002                TO EMI-ERROR.                    EL601
00420                                                                   EL601
00421  0320-INPUT-ERROR.                                                EL601
00422      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL601
00423      MOVE AL-UNBON               TO SELECTA.                      EL601
00424      MOVE -1                     TO SELECTL.                      EL601
00425      GO TO 8200-SEND-DATAONLY.                                    EL601
00426                                                                   EL601
00427      EJECT                                                        EL601
00428  8100-SEND-INITIAL-MAP.                                           EL601
00429      MOVE LOW-VALUES             TO EL601AO.                      EL601
00430      MOVE EIBTIME                TO TIME-IN.                      EL601
00431      MOVE TIME-OUT               TO TIMEO.                        EL601
00432      MOVE SAVE-DATE              TO DATEO.                        EL601
00433                                                                      CL**4
00435      MOVE SCREEN-RESERVE-11      TO ITEM11O.                         CL**7
00436                                                                      CL**7
00437      MOVE SCREEN-SELECT-12       TO ITEM13O.                         CL**7
00438                                                                      CL**7
062121     IF PI-COMPANY-ID  =  'CID' OR 'AHL' OR 'FNL'
051805        MOVE SCREEN-SELECT-13    TO ITEM14O
051805     END-IF
00445                                                                      CL**6
00439      IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                             CL**4
00441          MOVE SCREEN-SELECT-11   TO ITEM11O                          CL**7
00442          MOVE SCREEN-SELECT-13   TO ITEM14O                          CL**7
00443          MOVE SPACES             TO ITEM15O ITEM16O                  CL**7
00444                                     ITEM17O ITEM18O.                 CL**7
00445                                                                      CL**6
00446      IF PI-COMPANY-ID  =  'DMD'                                      CL**6
00447          MOVE SCREEN-RESERVE-13  TO  ITEM14O                         CL**7
00448          MOVE SCREEN-SELECT-14   TO  ITEM15O                         CL**7
00449          MOVE SCREEN-SELECT-15   TO  ITEM16O                         CL**7
00450          MOVE SCREEN-SELECT-16   TO  ITEM17O                         CL**7
00451          MOVE SPACES             TO  ITEM18O.                        CL**7
00452 *        MOVE SCREEN-SELECT-17   TO  ITEM17O.                        CL**6
00453                                                                      CL**6
00454      IF PI-COMPANY-ID  =  'NCB'                                      CL**6
00455          MOVE SCREEN-RESERVE-13  TO ITEM14O                          CL**7
00456          MOVE SCREEN-RESERVE-14  TO ITEM15O                          CL**7
00457          MOVE SCREEN-RESERVE-15  TO ITEM16O                          CL**7
00458          MOVE SCREEN-RESERVE-16  TO ITEM17O                          CL**7
00459          MOVE SCREEN-SELECT-18   TO ITEM18O.                         CL**8
00460                                                                      CL**4
00461      MOVE -1                     TO SELECTL.                      EL601
00462                                                                   EL601
00463      EXEC CICS SEND                                               EL601
00464          MAP     (MAP-EL601A)                                     EL601
00465          MAPSET  (MAPSET-EL601S)                                  EL601
00466          FROM    (EL601AO)                                        EL601
00467          ERASE                                                    EL601
00468          CURSOR                                                   EL601
00469      END-EXEC.                                                       CL**6
00470                                                                   EL601
00471      GO TO 9100-RETURN-TRAN.                                      EL601
00472                                                                   EL601
00473  8200-SEND-DATAONLY.                                              EL601
00474      MOVE SAVE-DATE              TO DATEO.                        EL601
00475      MOVE EIBTIME                TO TIME-IN.                      EL601
00476      MOVE TIME-OUT               TO TIMEO.                        EL601
00477      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL601
00478                                                                   EL601
00480      MOVE SCREEN-RESERVE-11      TO ITEM11O.                         CL**7
00481                                                                      CL**7
00482      MOVE SCREEN-SELECT-12       TO ITEM13O.                         CL**7
00483                                                                      CL**7
00484      IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                             CL**7
00486          MOVE SCREEN-SELECT-11   TO  ITEM11O                         CL**7
00487          MOVE SCREEN-SELECT-13   TO  ITEM14O                         CL**7
00488          MOVE SPACES             TO ITEM15O ITEM16O                  CL**7
00489                                     ITEM17O ITEM18O.                 CL**7
00490                                                                      CL**7
00491      IF PI-COMPANY-ID  =  'DMD'                                      CL**6
00492          MOVE SCREEN-RESERVE-13  TO  ITEM14O                         CL**7
00493          MOVE SCREEN-SELECT-14   TO  ITEM15O                         CL**7
00494          MOVE SCREEN-SELECT-15   TO  ITEM16O                         CL**7
00495          MOVE SCREEN-SELECT-16   TO  ITEM17O                         CL**7
00496          MOVE SPACES             TO  ITEM18O.                        CL**7
00497 *        MOVE SCREEN-SELECT-17   TO  ITEM17O.                        CL**7
00498                                                                      CL**6
00499      IF PI-COMPANY-ID  =  'NCB'                                      CL**6
00500          MOVE SCREEN-RESERVE-13  TO ITEM14O                          CL**7
00501          MOVE SCREEN-RESERVE-14  TO ITEM15O                          CL**7
00502          MOVE SCREEN-RESERVE-15  TO ITEM16O                          CL**7
00503          MOVE SCREEN-RESERVE-16  TO ITEM17O                          CL**7
00504          MOVE SCREEN-SELECT-18   TO ITEM18O.                         CL**8
00505                                                                      CL**6
00506      EXEC CICS SEND                                               EL601
00507          MAP      (MAP-EL601A)                                    EL601
00508          MAPSET   (MAPSET-EL601S)                                 EL601
00509          FROM     (EL601AO)                                       EL601
00510          DATAONLY                                                 EL601
00511          CURSOR                                                   EL601
00512      END-EXEC.                                                       CL**6
00513                                                                   EL601
00514      GO TO 9100-RETURN-TRAN.                                      EL601
00515                                                                   EL601
00516  8300-SEND-TEXT.                                                  EL601
00517      EXEC CICS SEND TEXT                                          EL601
00518          FROM     (LOGOFF-TEXT)                                   EL601
00519          LENGTH   (LOGOFF-LENGTH)                                 EL601
00520          ERASE                                                    EL601
00521          FREEKB                                                   EL601
00522      END-EXEC.                                                       CL**6
00523                                                                   EL601
00524      EXEC CICS RETURN                                             EL601
00525      END-EXEC.                                                       CL**6
00526                                                                   EL601
00527  8800-UNAUTHORIZED-ACCESS.                                        EL601
00528      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL601
00529      GO TO 8300-SEND-TEXT.                                        EL601
00530                                                                   EL601
00531  8810-PF23.                                                       EL601
00532      MOVE DFHPF23                TO PI-ENTRY-CD-1.                EL601
00533      MOVE XCTL-EL005             TO PGM-NAME.                     EL601
00534      GO TO 9300-XCTL.                                             EL601
00535                                                                   EL601
00536  9100-RETURN-TRAN.                                                EL601
00537      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL601
00538      MOVE '601A'                 TO PI-CURRENT-SCREEN-NO.            CL**2
00539                                                                   EL601
00540      EXEC CICS RETURN                                             EL601
00541          TRANSID   (TRANS-EXA1)                                   EL601
00542          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL601
00543          LENGTH    (PI-COMM-LENGTH)                               EL601
00544      END-EXEC.                                                       CL**6
00545                                                                   EL601
00546  9200-PF24.                                                          CL**5
00547      MOVE XCTL-EL626             TO PGM-NAME.                     EL601
00548      GO TO 9300-XCTL.                                             EL601
00549                                                                   EL601
00550  9300-XCTL.                                                       EL601
00551      MOVE SPACES                 TO PI-ENTRY-CD-2                 EL601
00552                                     PI-RETURN-CODES               EL601
00553                                     PI-UPDATE-BY                  EL601
00554                                     PI-PROGRAM-WORK-AREA.         EL601
00555      MOVE ZEROS                  TO PI-UPDATE-HHMMSS.             EL601
00556                                                                   EL601
00557      EXEC CICS XCTL                                               EL601
00558          PROGRAM    (PGM-NAME)                                    EL601
00559          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL601
00560          LENGTH     (PI-COMM-LENGTH)                              EL601
00561      END-EXEC.                                                       CL**6
00562                                                                   EL601
00563  9400-CLEAR.                                                      EL601
00564      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL601
00565      GO TO 9300-XCTL.                                             EL601
00566                                                                   EL601
00567  9500-HELP.                                                       EL601
00568      MOVE XCTL-EL010             TO PGM-NAME.                     EL601
00569      GO TO 9300-XCTL.                                             EL601
00570                                                                   EL601
00571  9600-PGMID-ERROR.                                                EL601
00572      MOVE PGM-NAME               TO PROGO.                        EL601
00573      MOVE AL-UNBON               TO SELECTA.                         CL**8
00574      MOVE ER-7003                TO EMI-ERROR.                       CL**8
00575                                                                   EL601
00576      EXEC CICS HANDLE CONDITION                                   EL601
00577          PGMIDERR  (8300-SEND-TEXT)                               EL601
00578      END-EXEC.                                                       CL**6
00579                                                                   EL601
00580      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL601
00581      MOVE ' '                    TO PI-ENTRY-CD-1.                EL601
00582      MOVE XCTL-EL005             TO PGM-NAME.                     EL601
00583      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL601
00584      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL601
00585      GO TO 9300-XCTL.                                             EL601
00586                                                                   EL601
00587  9700-LINK-DATE-CONVERT.                                          EL601
00588      EXEC CICS LINK                                               EL601
00589          PROGRAM    (LINK-ELDATCV)                                EL601
00590          COMMAREA   (DATE-CONVERSION-DATA)                        EL601
00591          LENGTH     (DC-COMM-LENGTH)                              EL601
00592      END-EXEC.                                                       CL**6
00593                                                                   EL601
00594  9700-EXIT.                                                       EL601
00595      EXIT.                                                        EL601
00596                                                                   EL601
00597  9900-ERROR-FORMAT.                                               EL601
00598      IF NOT EMI-ERRORS-COMPLETE                                   EL601
00599          MOVE LINK-EL001         TO PGM-NAME                      EL601
00600          EXEC CICS LINK                                           EL601
00601              PROGRAM    (PGM-NAME)                                EL601
00602              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL601
00603              LENGTH     (EMI-COMM-LENGTH)                         EL601
00604          END-EXEC.                                                   CL**6
00605                                                                   EL601
00606  9900-EXIT.                                                       EL601
00607      EXIT.                                                        EL601
00608                                                                   EL601
00609  9990-ABEND.                                                      EL601
00610      MOVE LINK-EL004             TO PGM-NAME.                     EL601
00611      MOVE DFHEIBLK               TO EMI-LINE1.                    EL601
00612                                                                   EL601
00613      EXEC CICS LINK                                               EL601
00614          PROGRAM   (PGM-NAME)                                     EL601
00615          COMMAREA  (EMI-LINE1)                                    EL601
00616          LENGTH    (72)                                           EL601
00617      END-EXEC.                                                       CL**6
00618                                                                   EL601
00619      GO TO 8200-SEND-DATAONLY.                                    EL601
00620                                                                   EL601
