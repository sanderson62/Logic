00001  IDENTIFICATION DIVISION.                                         11/24/98
00002                                                                   ECS157
00003  PROGRAM-ID.                 ECS157.                                 LV027
00004 *              PROGRAM CONVERTED BY                                  CL**1
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**1
00006 *              CONVERSION DATE 10/17/97 08:55:52.                    CL**1
00007 *                            VMOD=2.012                              CL**1
00008 *                                                                    CL**1
00009 *AUTHOR.        LOGIC, INC.                                          CL**1
00010 *               DALLAS, TEXAS.                                       CL**1
00011                                                                      CL**1
00012 *DATE-COMPILED.                                                      CL**1
00013                                                                      CL**1
00014 *SECURITY.   *****************************************************   CL**1
00015 *            *                                                   *   CL**1
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**1
00017 *            *                                                   *   CL**1
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**1
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**1
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**1
00021 *            *                                                   *   CL**1
00022 *            *****************************************************   CL**1
00023                                                                      CL**1
00024 *REMARKS.                                                            CL**1
00025 *        THIS PROGRAM GENERATES RETRO PAYMENT EXTRACTS               CL**1
00026 *        AND CLAIM RESERVES FOR THE STATE CALL.                      CL**1
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
120803* 120803                   PEMA  REMOVE SUPPLEMENTAL IBNR CALC
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
122002******************************************************************
00027                                                                      CL**1
00028  ENVIRONMENT DIVISION.                                               CL**1
00029  CONFIGURATION SECTION.                                              CL**1
00030  SPECIAL-NAMES.                                                      CL**1
00031      C02 IS LCP-CH2                                                  CL**1
00032      C03 IS LCP-CH3                                                  CL**1
00033      C04 IS LCP-CH4                                                  CL**1
00034      C05 IS LCP-CH5                                                  CL**1
00035      C06 IS LCP-CH6                                                  CL**1
00036      C07 IS LCP-CH7                                                  CL**1
00037      C08 IS LCP-CH8                                                  CL**1
00038      C09 IS LCP-CH9                                                  CL**1
00039      C10 IS LCP-CH10                                                 CL**1
00040      C11 IS LCP-CH11                                                 CL**1
00041      C12 IS LCP-CH12                                                 CL**1
00042      S01 IS LCP-P01                                                  CL**1
00043      S02 IS LCP-P02.                                                 CL**1
00044  INPUT-OUTPUT SECTION.                                               CL**1
00045  FILE-CONTROL.                                                       CL**1
00046                                                                      CL**1
00047      SELECT  EPEC-IN         ASSIGN TO SYS010-UT-2400-S-SYS010.      CL**1
00048                                                                      CL**1
00049      SELECT  ERACCTT         ASSIGN TO SYS015-FBA1-ERACCTT           CL**1
00050                              ACCESS IS SEQUENTIAL                    CL**1
00051                              ORGANIZATION IS INDEXED                 CL**1
00052                              FILE STATUS IS ERACCTT-FILE-STATUS      CL**1
00053                              RECORD KEY IS AM-CONTROL-PRIMARY.       CL**1
00054                                                                      CL**1
00055      SELECT  CALL-EXTRACTS   ASSIGN TO SYS011-UT-2400-S-SYS011.      CL**1
00056      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.      CL**1
00057      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.      CL**1
00058      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.      CL**1
00059  EJECT                                                               CL**1
00060  DATA DIVISION.                                                      CL**1
00061  FILE SECTION.                                                       CL**1
00062                                                                      CL**1
00063  FD  PRNTR                                                           CL**1
00064                              COPY ELCPRTFD.                          CL**1
00065  EJECT                                                               CL**1
00066                                                                      CL**1
00067  FD  EPEC-IN                                                         CL**1
00068                              COPY ECSEPCFD.                          CL**1
00069                                                                      CL**1
00070                              COPY ECSEPC01.                          CL**1
00071  EJECT                                                               CL**1
00072  FD  ERACCTT.                                                        CL**1
00073                                                                      CL**1
00074                              COPY ERCACCT.                           CL**1
00075      EJECT                                                           CL**1
00076  FD  CALL-EXTRACTS                                                   CL**1
00077      BLOCK CONTAINS 0 RECORDS
00078      RECORDING MODE F.                                               CL**1
00079                                                                      CL**1
00080  01  CALL-RECORD                 PIC X(666).                         CL**1
00081                                                                      CL**1
00082                                                                      CL**1
00083  FD  FICH                                                            CL**1
00084                              COPY ELCFCHFD.                          CL**1
00085  EJECT                                                               CL**1
00086  FD  DISK-DATE                                                       CL**1
00087                              COPY ELCDTEFD.                          CL**1
00088  EJECT                                                               CL**1
00089  WORKING-STORAGE SECTION.                                            CL**1
00090  77  FILLER  PIC X(32) VALUE '********************************'.     CL**1
00091  77  FILLER  PIC X(32) VALUE '    ECS157  WORKING STORAGE     '.     CL**1
00092  77  FILLER  PIC X(32) VALUE '*********VMOD=2.012 ************'.     CL**1
00093                                                                      CL**1
00094  77  X                             PIC X     VALUE SPACE.            CL**1
00095  77  LCP-ASA                       PIC X.                            CL**1
00096                                                                      CL**1
00097  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**1
00098      EJECT                                                           CL**1
00099                                  COPY ECSEXTCL.                      CL**1
00100      EJECT                                                           CL**1
00101                                                                      CL**1
00102  01  MISC-WORK-AREAS.                                                CL**1
00103      12  X-RECORDS                  PIC 9(7)     VALUE ZEROS.        CL**1
00104      12  TEX-RECORDS                PIC 9(7)     VALUE ZEROS.        CL**1
00105      12  EP-RECORDS                 PIC 9(7)     VALUE ZEROS.        CL**1
00106      12  ERACCTT-FILE-STATUS PIC X(02)           VALUE SPACES.       CL**1
00107      12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.       CL**1
00108      12  WS-ABEND-FILE-STATUS PIC X(02)          VALUE SPACES.       CL**1
00109      12  WS-RETURN-CODE      PIC S9(04)  COMP    VALUE +0.           CL**1
00110      12  WS-ZERO             PIC S9(01)  COMP-3  VALUE +0.           CL**1
00111      12  PGM-SUB             PIC S999    COMP-3  VALUE +158.         CL**1
00112      12  STATE-OF-TEXAS      PIC XX.                                 CL**1
00113      12  WS-REPORT-SW        PIC X               VALUE SPACES.       CL**1
00114          88  STATE-IS-EXCLUDED     VALUE 'X'.                        CL**1
00115          88  REPORT-OVER-UNDER-60  VALUES '4' '5'.                   CL**1
00116      12  WS-EXCL-BUSC-SW     PIC X               VALUE SPACES.       CL**1
00117          88  BUSC-TYPE-IS-EXCLUDED VALUE 'X'.                        CL**1
00118      12  SAVE-STATE          PIC XX              VALUE SPACES.       CL**1
00119      12  SAVE-BUSC-TYPE      PIC 99              VALUE ZEROS.        CL**1
00120      12  WS-THRU-DATE        PIC 9(8).                               CL*17
00121      12  WS-THRU-DATE-R  REDEFINES  WS-THRU-DATE.                    CL*18
00122          16  WS-THRU-CCYY    PIC 9(4).                               CL*17
00123          16  WS-THRU-CCYR REDEFINES WS-THRU-CCYY.                    CL*17
00124              20  WS-THRU-CC  PIC 99.                                 CL*17
00125              20  WS-THRU-YR  PIC 99.                                 CL*17
00126          16  WS-THRU-MO      PIC 99.                                 CL*17
00127          16  WS-THRU-DA      PIC 99.                                 CL*17
00128      12  PRIOR-DAT-1         PIC 9(8).                               CL**1
00129      12  PRIOR-DAT-1-R  REDEFINES  PRIOR-DAT-1.                      CL**1
00130          16  PRIOR-CCYY-1    PIC 9(4).                               CL**1
00131          16  PRIOR-CCYR-1  REDEFINES  PRIOR-CCYY-1.                  CL**1
00132              20  PRIOR-CC-1  PIC 99.                                 CL**1
00133              20  PRIOR-YR-1  PIC 99.                                 CL**1
00134          16  PRIOR-MO-1      PIC 99.                                 CL**4
00135          16  PRIOR-DA-1      PIC 99.                                 CL**4
00136      12  PRIOR-DAT-2         PIC 9(8).                               CL**1
00137      12  PRIOR-DAT-2-R  REDEFINES  PRIOR-DAT-2.                      CL**1
00138          16  PRIOR-CCYY-2    PIC 9(4).                               CL**1
00139          16  PRIOR-CCYR-2  REDEFINES  PRIOR-CCYY-2.                  CL**1
00140              20  PRIOR-CC-2  PIC 99.                                 CL**1
00141              20  PRIOR-YR-2  PIC 99.                                 CL**1
00142          16  PRIOR-MO-2      PIC 99.                                 CL**4
00143          16  PRIOR-DA-2      PIC 99.                                 CL**4
00144      12  PRIOR-DAT-3         PIC 9(8).                               CL**1
00145      12  PRIOR-DAT-3-R  REDEFINES  PRIOR-DAT-3.                      CL**1
00146          16  PRIOR-CCYY-3    PIC 9(4).                               CL**1
00147          16  PRIOR-CCYR-3  REDEFINES  PRIOR-CCYY-3.                  CL**1
00148              20  PRIOR-CC-3  PIC 99.                                 CL**1
00149              20  PRIOR-YR-3  PIC 99.                                 CL**1
00150          16  PRIOR-MO-3      PIC 99.                                 CL**4
00151          16  PRIOR-DA-3      PIC 99.                                 CL**4
00152                                                                      CL**1
00153      12  END-DAT-1           PIC 9(8).                               CL**3
00154      12  END-DAT-1-R  REDEFINES  END-DAT-1.                          CL**1
00155          16  END-CCYY-1      PIC 9(4).                               CL**1
00156          16  END-CCYR-1  REDEFINES  END-CCYY-1.                      CL**5
00157              20  END-CC-1    PIC 99.                                 CL**1
00158              20  END-YR-1    PIC 99.                                 CL**1
00159          16  END-MO-1        PIC 99.                                 CL**4
00160          16  END-DA-1        PIC 99.                                 CL**4
00161      12  END-DAT-2           PIC 9(8).                               CL**3
00162      12  END-DAT-2-R  REDEFINES  END-DAT-2.                          CL**1
00163          16  END-CCYY-2      PIC 9(4).                               CL**1
00164          16  END-CCYR-2  REDEFINES  END-CCYY-2.                      CL**5
00165              20  END-CC-2    PIC 99.                                 CL**1
00166              20  END-YR-2    PIC 99.                                 CL**1
00167          16  END-MO-2        PIC 99.                                 CL**4
00168          16  END-DA-2        PIC 99.                                 CL**4
00169      12  END-DAT-3           PIC 9(8).                               CL**3
00170      12  END-DAT-3-R  REDEFINES  END-DAT-3.                          CL**1
00171          16  END-CCYY-3      PIC 9(4).                               CL**1
00172          16  END-CCYR-3  REDEFINES  END-CCYY-3.                      CL**5
00173              20  END-CC-3    PIC 99.                                 CL**1
00174              20  END-YR-3    PIC 99.                                 CL**1
00175          16  END-MO-3        PIC 99.                                 CL**4
00176          16  END-DA-3        PIC 99.                                 CL**8
00177      12  WS-EP-RUN-DATE          PIC 9(11).                          CL*14
00178      12  WS-EP-RUN-DATE-R  REDEFINES  WS-EP-RUN-DATE.                CL*13
00179          16  FILLER              PIC 999.                            CL*10
00180          16  WS-EP-RUN-CCYY      PIC 9(4).                           CL**9
00181          16  WS-EP-RUN-CCYR  REDEFINES  WS-EP-RUN-CCYY.              CL**8
00182              20  WS-EP-RUN-CC    PIC 99.                             CL**8
00183              20  WS-EP-RUN-YR    PIC 99.                             CL**8
00184          16  WS-EP-RUN-MO        PIC 99.                             CL**8
00185          16  WS-EP-RUN-DA        PIC 99.                             CL**8
00186                                                                      CL**1
00187      12  MATCH-SWITCH        PIC X           VALUE '0'.              CL**1
00188          88  MATCHED-TO-ACCOUNT    VALUE '0'.                        CL**1
00189                                                                      CL**1
00190      12  WS-PROCESS-SW       PIC X           VALUE SPACES.           CL**1
00191          88  SINGLE-PERIOD-PROCESS    VALUE 'S'.                     CL**1
00192                                                                      CL**1
00193      EJECT                                                           CL**1
00194  01  INITIALIZED-FRONT.                                              CL**1
00195      12  FILLER              PIC X(35)      VALUE SPACES.            CL**1
00196      12  FILLER              PIC X(37)      VALUE SPACES.            CL**1
00197      12  FILLER              PIC X(02)      VALUE ZEROS.             CL**1
00198      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00199      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00200      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00201      12  FILLER              PIC X(26)      VALUE SPACES.            CL**1
00202      12  FILLER              PIC 9(02)      VALUE ZEROS.             CL**1
00203      12  FILLER              PIC X(12)      VALUE SPACES.            CL**1
00204      12  FILLER              PIC 9(02)      VALUE ZEROS.             CL**1
00205      12  FILLER              PIC S9(3)V9(4) VALUE +0 COMP-3.         CL**1
00206      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00207      12  FILLER              PIC X(01)      VALUE SPACES.            CL**1
00208      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00209      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00210      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00211      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00212  01  INITIALIZED-BACK.                                               CL**1
00213      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00214      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00215      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00216      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00217      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00218      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00219      12  FILLER              PIC S9(03)V9   VALUE +0 COMP-3.         CL**1
00220      12  FILLER              PIC S9(03)V9   VALUE +0 COMP-3.         CL**1
00221      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00222      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.         CL**1
00223      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00224      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00225      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00226      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00227      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00228      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00229      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00230      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00231      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00232      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00233      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00234      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00235      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00236      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00237      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00238      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00239      12  FILLER              PIC S9(11)V99  VALUE +0 COMP-3.         CL**1
00240      12  FILLER              PIC S9(5)V99   VALUE +0 COMP-3.         CL**1
00241      12  FILLER              PIC S9(5)V99   VALUE +0 COMP-3.         CL**1
00242      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.         CL**1
00243      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00244      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00245      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00246      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00247      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.         CL**1
00248      12  FILLER              PIC X(08)      VALUE SPACES.            CL*26
00249                                                                      CL**1
00250 ******************************************************************   CL**1
00251      EJECT                                                           CL**1
00252                                                                      CL**1
00253  01  HEAD-1.                                                         CL**1
00254      12  FILLER      PIC X(41) VALUE SPACES.                         CL**1
00255      12  FILLER      PIC X(21) VALUE 'STATE CALL   RESERVES'.        CL**1
00256      12  FILLER      PIC X(21) VALUE ' AND RETRO PAYMENTS  '.        CL**1
00257      12  FILLER      PIC X(41) VALUE SPACES.                         CL**1
00258      12  FILLER      PIC X(07) VALUE 'ECS-157'.                      CL**1
00259                                                                      CL**1
00260  01  HEAD-2.                                                         CL**1
00261      12  FILLER      PIC X(47) VALUE SPACES.                         CL**1
00262      12  HD-CLIENT   PIC X(30) VALUE SPACES.                         CL**1
00263      12  FILLER      PIC X(47) VALUE SPACES.                         CL**1
00264      12  HD-DATE     PIC X(08) VALUE SPACES.                         CL**1
00265                                                                      CL**1
00266  01  HEAD-3.                                                         CL**1
00267      12  FILLER      PIC X(53) VALUE SPACES.                         CL**1
00268      12  HD-ALF-DTE  PIC X(18) VALUE SPACES.                         CL**1
00269      12  FILLER      PIC X(41) VALUE SPACES.                         CL**1
00270      12  FILLER      PIC X(05) VALUE 'PAGE'.                         CL**1
00271      12  HD-PAGE     PIC ZZ,ZZZ.                                     CL**1
00272                                                                      CL**1
00273  01  DISPLAY-LINE.                                                   CL**1
00274      12  FILLER          PIC X(10) VALUE SPACES.                     CL**1
00275      12  DISPLAY-MESS    PIC X(35) VALUE SPACES.                     CL**1
00276      12  FILLER          PIC X(03) VALUE SPACES.                     CL**1
00277      12  DISPLAY-COUNT   PIC Z,ZZZ,ZZZ VALUE ZEROS.                  CL**1
00278                                                                      CL**1
00279      EJECT                                                           CL**1
00280                              COPY ELCDATE.                           CL**1
00281      EJECT                                                           CL**1
00282                              COPY ELCDTECX.                          CL**1
00283      EJECT                                                           CL**1
00284                              COPY ELCDTEVR.                          CL**1
00285  EJECT                                                               CL**1
00286                                                                      CL**1
00287  PROCEDURE DIVISION.                                                 CL**1
00288                                                                      CL**1
00289  0001-READ-DATE.                                                     CL**1
00290                              COPY ELCDTERX.                          CL**1
00291                                                                      CL**1
00292      IF EP-DT = RUN-DATE                                             CL**1
00293         MOVE ALPH-DATE           TO HD-ALF-DTE                       CL**1
00294         SUBTRACT 1 FROM RUN-CCYY                                     CL**1
00295      ELSE                                                            CL**1
00296         MOVE EP-DT               TO DC-GREG-DATE-CYMD                CL**7
00297         MOVE 'L'                 TO DC-OPTION-CODE                   CL**1
00298         CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    CL**1
00299         MOVE DC-GREG-DATE-1-ALPHA                                    CL**1
00300                                  TO HD-ALF-DTE.                      CL**1
00301                                                                      CL**1
00302      MOVE EP-DT                  TO WS-THRU-DATE.                    CL*17
00303                                                                      CL*17
00304      MOVE EP-CCYY                TO END-CCYY-1.                      CL*17
00305      MOVE EP-MO                  TO END-MO-1.                        CL*17
00306      MOVE EP-DA                  TO END-DA-1.                        CL*17
00307      MOVE END-DAT-1              TO END-DAT-2                        CL**1
00308                                     END-DAT-3.                       CL**1
00309      SUBTRACT 1 FROM END-CCYY-2.                                     CL**1
00310      SUBTRACT 2 FROM END-CCYY-3.                                     CL**1
00311                                                                      CL**1
00312      MOVE WS-RUN-DATE-N          TO PRIOR-DAT-1                      CL*20
00313                                     PRIOR-DAT-2                      CL*15
00314                                     PRIOR-DAT-3.                     CL*15
pemuni*    SUBTRACT 1 FROM PRIOR-YR-2                                      CL**1
pemuni*    SUBTRACT 2 FROM PRIOR-YR-3                                      CL**1
pemuni     SUBTRACT 1 FROM PRIOR-ccyy-2                                    CL**1
pemuni     SUBTRACT 2 FROM PRIOR-ccyy-3                                    CL**1
00317                                                                      CL**1
00318      IF EP-SW EQUAL '1'                                              CL**1
00319          MOVE 'S' TO WS-PROCESS-SW.                                  CL**1
00320                                                                      CL**1
00321  0080-FIND-TEXAS.                                                    CL**1
00322                                                                      CL**1
00323      ADD +1 TO CLAS-INDEXS.                                          CL**1
00324      IF CLAS-INDEXS GREATER CLAS-MAXS                                CL**1
00325         MOVE SPACE TO STATE-OF-TEXAS                                 CL**1
00326         GO TO 0090-OPEN-FILES.                                       CL**1
00327                                                                      CL**1
00328      IF STATE-ABBR (CLAS-INDEXS) EQUAL 'TX'                          CL**1
00329         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-OF-TEXAS               CL**1
00330         GO TO 0090-OPEN-FILES.                                       CL**1
00331                                                                      CL**1
00332      GO TO 0080-FIND-TEXAS.                                          CL**1
00333  EJECT                                                               CL**1
00334  0090-OPEN-FILES.                                                    CL**1
00335                                                                      CL**1
00336      OPEN INPUT EPEC-IN  ERACCTT                                     CL**1
00337           OUTPUT CALL-EXTRACTS PRNTR.                                CL**1
00338                                                                      CL**1
00339      IF ERACCTT-FILE-STATUS NOT EQUAL '00' AND '97'                  CL**1
00340         MOVE 'ERROR ON OPEN ERACCTT ' TO WS-ABEND-MESSAGE            CL**1
00341         MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS             CL**1
00342         GO TO ABEND-PGM.                                             CL**1
00343                                                                      CL**1
00344  0100-READ-ACCT.                                                     CL**1
00345                                                                      CL**1
00346      READ ERACCTT NEXT RECORD.                                       CL**1
00347                                                                      CL**1
00348      IF ERACCTT-FILE-STATUS EQUAL '10'                               CL**9
00349         MOVE HIGH-VALUE          TO AM-CONTROL-PRIMARY               CL**9
00350         MOVE 99999999999         TO AM-EFFECT-DT.                    CL*12
00351                                                                      CL**1
00352      IF ERACCTT-FILE-STATUS NOT EQUAL '00' AND '10'                  CL**1
00353         MOVE ' ERROR ON READ ERACCTT ' TO WS-ABEND-MESSAGE           CL**1
00354         MOVE ERACCTT-FILE-STATUS       TO WS-ABEND-FILE-STATUS       CL**1
00355         GO TO ABEND-PGM.                                             CL**1
00356                                                                      CL**1
00357  0200-READ-EPEC.                                                     CL**1
00358                                                                      CL**1
00359      READ EPEC-IN AT END                                             CL**1
00360           GO TO 0800-PRINT-HEADINGS.                                 CL**1
00361                                                                      CL*23
00362      ADD 1 TO EP-RECORDS.                                            CL**1
00363                                                                      CL**1
00364      IF EP-RECORD-ID NOT EQUAL 'EP'                                  CL**1
00365         GO TO 0200-READ-EPEC.                                        CL**1
00366                                                                      CL**1
00367      IF EP-PURGE EQUAL 'P'                                           CL**1
00368         GO TO 0200-READ-EPEC.                                        CL**1
00369                                                                      CL**1
00370      IF (EP-REIN NOT EQUAL ' ') AND                                  CL**1
00371         (DTE-PGM-OPT EQUAL '1')                                      CL**1
00372         GO TO 0200-READ-EPEC.                                        CL**1
00373                                                                      CL**1
00374      MOVE ZEROS TO  MATCH-SWITCH.                                    CL**1
00375                                                                      CL**1
00376      MOVE EP-RUN-DTE             TO WS-EP-RUN-DATE.                  CL**8
00377                                                                      CL**1
00378      IF (WS-EP-RUN-CCYR NOT LESS THAN PRIOR-CCYY-3) AND              CL**7
00379         (WS-EP-RUN-CCYR NOT GREATER THAN WS-THRU-CCYY)               CL*17
00380         NEXT SENTENCE                                                CL**1
00381      ELSE                                                            CL**1
00382         GO TO 0200-READ-EPEC.                                        CL**1
00383                                                                      CL**1
00384      IF EP-STATE NOT EQUAL SAVE-STATE                                CL**1
00385          PERFORM 0700-LOCATE-STATE THRU 0700-EXIT.                   CL**1
00386                                                                      CL**1
00387      IF STATE-IS-EXCLUDED                                            CL**1
00388          GO TO 0200-READ-EPEC.                                       CL**1
00389                                                                      CL**1
00390  0300-MATCH-IT.                                                      CL**1
00391                                                                      CL**1
00392      IF EP-CNTRL-1 GREATER THAN AM-CONTROL-A                         CL**1
00393         PERFORM 0100-READ-ACCT                                       CL**1
00394         GO TO 0300-MATCH-IT.                                         CL**1
00395                                                                      CL**1
00396      IF EP-CNTRL-1 LESS THAN AM-CONTROL-A                            CL**1
00397         MOVE 'X' TO MATCH-SWITCH                                     CL**1
00398         GO TO 0400-BUILD-EXTRACTS.                                   CL**1
00399                                                                      CL**1
00400      IF EP-EXP-DTE GREATER THAN AM-EXPIRE-DT                         CL**1
00401         PERFORM 0100-READ-ACCT                                       CL**1
00402         GO TO 0300-MATCH-IT.                                         CL**1
00403                                                                      CL**1
00404      IF EP-EXP-DTE LESS THAN AM-EXPIRE-DT                            CL*27
00405          MOVE 'X'                TO  MATCH-SWITCH.                   CL*27
00406                                                                      CL*27
00407  0400-BUILD-EXTRACTS.                                                CL**1
00408                                                                      CL**1
00409      IF NOT MATCHED-TO-ACCOUNT                                       CL**1
00410         DISPLAY 'NO ACCOUNT MASTER FOR THIS EXTRACT ' EP-CONTROL     CL**1
00411                 ' EXP DATE ' EP-EXP-DTE ' EFF DATE ' EP-EFF-DTE      CL**1
00412         MOVE 'NO ACCOUNT MASTER FOUND ' TO WS-ABEND-MESSAGE          CL**1
00413         MOVE '0302'                     TO WS-RETURN-CODE            CL**1
00414         GO TO ABEND-PGM.                                             CL**1
00415                                                                      CL**1
00416      IF AM-GPCD NOT NUMERIC                                          CL**1
00417          MOVE ZEROS TO AM-GPCD.                                      CL**1
00418                                                                      CL**1
00419      IF AM-GPCD NOT EQUAL SAVE-BUSC-TYPE                             CL**1
00420          PERFORM 0600-LOCATE-BUSC-TYPE THRU 0699-EXIT.               CL**1
00421                                                                      CL**1
00422      IF BUSC-TYPE-IS-EXCLUDED                                        CL**1
00423          GO TO 0200-READ-EPEC.                                       CL**1
00424                                                                      CL**1
00425      INITIALIZE  CALL-EXTRACT.                                       CL*26
00426                                                                      CL*26
00427      IF EP-CLM-PV NOT NUMERIC                                        CL**1
00428         MOVE +0    TO EP-CLM-PV.                                     CL**1
00429                                                                      CL**1
00430      IF EP-RETRO-PAYMENTS NOT NUMERIC                                CL**1
00431          MOVE +0    TO EP-RETRO-PAYMENTS.                            CL**1
00432                                                                      CL**1
00433      IF EP-RCD-TYPE EQUAL AH-OVERRIDE-L1                             CL**1
00434         MOVE 'A'         TO EX-LIFE-AH                               CL**1
00435      ELSE                                                            CL**1
00436         MOVE 'L'         TO EX-LIFE-AH                               CL**1
030612        IF DTE-CLIENT NOT = 'MON' AND 'CID' AND 'DCC' AND 'AHL'
00438             MOVE +0          TO EP-CLM-IBNR.                         CL**1
00439                                                                      CL**1
00440      MOVE EP-BEN-CODE TO EX-BEN-TYPE.                                CL**1
00441                                                                      CL**1
00442      IF (WS-EP-RUN-YR EQUAL WS-THRU-YR OR PRIOR-YR-1 OR              CL*17
00443                                       PRIOR-YR-2 OR                  CL*11
00444                                       PRIOR-YR-3)                    CL*11
00445          NEXT SENTENCE                                               CL**1
00446       ELSE                                                           CL**1
00447          GO TO 0200-READ-EPEC.                                       CL**1
00448                                                                      CL**1
00449      IF (WS-EP-RUN-YR EQUAL WS-THRU-YR) AND                          CL*17
00450         (EP-REIN EQUAL ' ')                                          CL**1
00451         MOVE EP-RETRO-PAYMENTS   TO EX-RETRO-PMTS (1).               CL**1
00452                                                                      CL**1
00453      IF (WS-EP-RUN-YR EQUAL PRIOR-YR-1) AND                          CL*11
00454         (EP-REIN EQUAL ' ')                                          CL**1
00455         MOVE EP-RETRO-PAYMENTS   TO EX-RETRO-PMTS (2).               CL**1
00456                                                                      CL**1
00457      IF (WS-EP-RUN-YR EQUAL PRIOR-YR-2) AND                          CL*11
00458         (EP-REIN EQUAL ' ')                                          CL**1
00459         MOVE EP-RETRO-PAYMENTS   TO EX-RETRO-PMTS (3).               CL**1
00460                                                                      CL**1
00461      IF WS-EP-RUN-DATE = END-DAT-1                                   CL*22
00462          MOVE EP-CLM-IBNR        TO EX-E-IBNR (1)                    CL**1
00463          MOVE EP-CLM-DU          TO EX-E-LOSS (1)                    CL**1
00464          ADD EP-CLM-PV           TO EX-E-LOSS (1)                    CL**1
00465          ADD EP-LOSS-RESV        TO EX-E-LOSS (1).                   CL**1
00466                                                                      CL**1
00467      IF WS-EP-RUN-DATE = PRIOR-DAT-1                                 CL*21
00468          MOVE EP-CLM-IBNR        TO EX-B-IBNR (1)                    CL**1
00469          MOVE EP-CLM-DU          TO EX-B-LOSS (1)                    CL**1
00470          ADD EP-CLM-PV           TO EX-B-LOSS (1)                    CL**1
00471          ADD EP-LOSS-RESV        TO EX-B-LOSS (1).                   CL**1
00472                                                                      CL**1
00473      IF WS-EP-RUN-DATE = END-DAT-2                                   CL*21
00474          MOVE EP-CLM-IBNR        TO EX-E-IBNR (2)                    CL**1
00475          MOVE EP-CLM-DU          TO EX-E-LOSS (2)                    CL**1
00476          ADD EP-CLM-PV           TO EX-E-LOSS (2)                    CL**1
00477          ADD EP-LOSS-RESV        TO EX-E-LOSS (2).                   CL**1
00478                                                                      CL**1
00479      IF WS-EP-RUN-DATE = PRIOR-DAT-2                                 CL*21
00480          MOVE EP-CLM-IBNR        TO EX-B-IBNR (2)                    CL**1
00481          MOVE EP-CLM-DU          TO EX-B-LOSS (2)                    CL**1
00482          ADD EP-CLM-PV           TO EX-B-LOSS (2)                    CL**1
00483          ADD EP-LOSS-RESV        TO EX-B-LOSS (2).                   CL**1
00484                                                                      CL**1
00485      IF WS-EP-RUN-DATE = END-DAT-3                                   CL*27
00486          MOVE EP-CLM-IBNR        TO EX-E-IBNR (3)                    CL**1
00487          MOVE EP-CLM-DU          TO EX-E-LOSS (3)                    CL**1
00488          ADD EP-CLM-PV           TO EX-E-LOSS (3)                    CL**1
00489          ADD EP-LOSS-RESV        TO EX-E-LOSS (3).                   CL**1
00490                                                                      CL**1
00491      IF WS-EP-RUN-DATE = PRIOR-DAT-3                                 CL*27
00492          MOVE EP-CLM-IBNR     TO EX-B-IBNR (3)                       CL**1
00493          MOVE EP-CLM-DU       TO EX-B-LOSS (3)                       CL**1
00494          ADD EP-CLM-PV        TO EX-B-LOSS (3)                       CL**1
00495          ADD EP-LOSS-RESV     TO EX-B-LOSS (3).                      CL**1
00496                                                                      CL**1
00497      IF (EX-RETRO-PMTS (1) EQUAL ZERO) AND                           CL**1
00498         (EX-RETRO-PMTS (2) EQUAL ZERO) AND                           CL**1
00499         (EX-RETRO-PMTS (3) EQUAL ZERO) AND                           CL**1
00500         (EX-B-IBNR (1) EQUAL ZERO) AND                               CL**1
00501         (EX-B-IBNR (2) EQUAL ZERO) AND                               CL**1
00502         (EX-B-IBNR (3) EQUAL ZERO) AND                               CL**1
00503         (EX-B-LOSS (1) EQUAL ZERO) AND                               CL**1
00504         (EX-B-LOSS (2) EQUAL ZERO) AND                               CL**1
00505         (EX-B-LOSS (3) EQUAL ZERO) AND                               CL**1
00506         (EX-E-IBNR (1) EQUAL ZERO) AND                               CL**1
00507         (EX-E-IBNR (2) EQUAL ZERO) AND                               CL**1
00508         (EX-E-IBNR (3) EQUAL ZERO) AND                               CL**1
00509         (EX-E-LOSS (1) EQUAL ZERO) AND                               CL**1
00510         (EX-E-LOSS (2) EQUAL ZERO) AND                               CL**1
00511         (EX-E-LOSS (3) EQUAL ZERO)                                   CL**1
00512           GO TO 0200-READ-EPEC.                                      CL**1
00513                                                                      CL**1
00514      MOVE EP-CARRIER             TO EX-CARRIER.                      CL**1
00515      MOVE EP-GROUPING            TO EX-GROUPING.                     CL**1
00516      MOVE EP-STATE               TO EX-STATE.                        CL**1
00517      MOVE EP-ACCOUNT             TO EX-ACCOUNT.                      CL**1
00518      MOVE 'G'                    TO EX-I-G.                          CL**1
00519      MOVE '0'                    TO EX-TERM-CD.                      CL**1
00520      MOVE '1'                    TO EX-RECORD-TYPE.                  CL**1
00521                                                                      CL**1
00522      IF REPORT-OVER-UNDER-60   OR                                    CL**1
00523          EP-STATE EQUAL STATE-OF-TEXAS                               CL**1
00524           MOVE '1'                 TO EX-TERM-CD.                    CL**1
00525                                                                      CL**1
00526      IF MATCHED-TO-ACCOUNT                                           CL**1
00527          MOVE AM-CAL-TABLE TO EX-CAL-TABLE                           CL**1
00528          MOVE AM-GPCD TO EX-ACCT-TYPE                                CL**1
00529          MOVE AM-NAME TO EX-NAME                                     CL**1
00530      ELSE                                                            CL**1
00531          MOVE ZEROS TO EX-STATE-DEV EX-CAL-TABLE                     CL**1
00532          MOVE '20' TO EX-ACCT-TYPE                                   CL**1
00533          MOVE 'UNKNOWN' TO EX-NAME.                                  CL**1
00534                                                                      CL**1
00535      IF MATCHED-TO-ACCOUNT                                           CL**1
00536         IF EP-RCD-TYPE EQUAL AH-OVERRIDE-L1                          CL**1
00537            MOVE AM-AH-DEVIATION TO EX-STATE-DEV                      CL**1
00538         ELSE                                                         CL**1
00539            MOVE AM-LF-DEVIATION TO EX-STATE-DEV.                     CL**1
00540                                                                      CL**1
00541      IF (EP-REIN EQUAL 'R') AND                                      CL**1
00542         (EP-REI-CO NOT EQUAL SPACES AND ZEROS)                       CL**1
00543         MOVE EP-REI-CO           TO EX-REIN                          CL**1
00544      ELSE                                                            CL**1
00545         MOVE SPACES              TO EX-REIN.                         CL**1
00546                                                                      CL**1
00547  0500-WRITE-EXTRACTS.                                                CL**1
00548                                                                      CL**1
00549      IF SINGLE-PERIOD-PROCESS                                        CL**1
00550          MOVE ZEROS              TO  EX-INC-CNT          (2)         CL**1
00551                                      EX-PD-CNT           (2)         CL**1
00552                                      EX-C-CNT            (2)         CL**1
00553                                      EX-CLAIM-AMT        (2)         CL**1
00554                                      RR                  (2)         CL**1
00555                                      M1                  (2)         CL**1
00556                                      M2                  (2)         CL**1
00557                                      M3                  (2)         CL**1
00558                                      M5                  (2)         CL**1
00559                                      M6                  (2)         CL**1
00560                                      EX-BEG-ST-RES       (2)         CL**1
00561                                      EX-END-ST-RES       (2)         CL**1
00562                                      EX-BEG-R78-RES      (2)         CL**1
00563                                      EX-END-R78-RES      (2)         CL**1
00564                                      EX-BEG-PRO-RES      (2)         CL**1
00565                                      EX-END-PRO-RES      (2)         CL**1
00566                                      EX-BEG-MEAN-RES     (2)         CL**1
00567                                      EX-END-MEAN-RES     (2)         CL**1
00568                                      EX-PRI-BEG-ST-RES   (2)         CL**1
00569                                      EX-PRI-END-ST-RES   (2)         CL**1
00570                                      EX-PRI-BEG-R78-RES  (2)         CL**1
00571                                      EX-PRI-END-R78-RES  (2)         CL**1
00572                                      EX-PRI-BEG-PRO-RES  (2)         CL**1
00573                                      EX-PRI-END-PRO-RES  (2)         CL**1
00574                                      EX-PRI-BEG-MEAN-RES (2)         CL**1
00575                                      EX-PRI-END-MEAN-RES (2)         CL**1
00576                                      EX-MEAN-INFORCE     (2)         CL**1
00577                                      EX-OVR-COMM         (2)         CL**1
00578                                      EX-AGT-COMM         (2)         CL**1
00579                                      EX-CNC-AMT          (2)         CL**1
00580                                      EX-RETRO-PMTS       (2)         CL**1
00581          MOVE INITIALIZED-BACK   TO  EX-DATA-BY-YEAR   (3).          CL**1
00582                                                                      CL**1
00583                                                                      CL**1
00584      MOVE LOW-VALUES TO SORT-KEY.                                    CL**1
00585      WRITE CALL-RECORD FROM CALL-EXTRACT.                            CL**1
00586      ADD 1 TO X-RECORDS.                                             CL**1
00587                                                                      CL**1
00588      IF EX-TERM-CD EQUAL '1'                                         CL**1
00589         MOVE '2' TO EX-TERM-CD                                       CL**1
00590         MOVE 'X' TO EX-TX-RESV-ADJ-REC                               CL**1
00591         ADD 1 TO TEX-RECORDS                                         CL**1
00592         WRITE CALL-RECORD FROM CALL-EXTRACT.                         CL**1
00593                                                                      CL**1
00594      GO TO 0200-READ-EPEC.                                           CL**1
00595                                                                      CL**1
00596      EJECT                                                           CL**1
00597  0600-LOCATE-BUSC-TYPE.                                              CL**1
00598                                                                      CL**1
00599      MOVE ZEROS TO SAVE-BUSC-TYPE.                                   CL**1
00600      MOVE SPACES TO WS-EXCL-BUSC-SW.                                 CL**1
00601      MOVE +1                     TO CLAS-INDEXB.                     CL**1
00602                                                                      CL**1
00603      IF AM-GPCD NOT NUMERIC                                          CL**1
00604          GO TO 0699-EXIT.                                            CL**1
00605                                                                      CL**1
00606      MOVE AM-GPCD            TO SAVE-BUSC-TYPE.                      CL**1
00607                                                                      CL**1
00608  0600-LOCATE-BUSC-LOOP.                                              CL**1
00609                                                                      CL**1
00610      IF CLAS-INDEXB GREATER THAN CLAS-MAXB                           CL**1
00611          GO TO 0699-EXIT.                                            CL**1
00612                                                                      CL**1
00613      IF CLAS-BUSC-CODE (CLAS-INDEXB) NOT EQUAL SAVE-BUSC-TYPE        CL**1
00614          ADD +1 TO CLAS-INDEXB                                       CL**1
00615          GO TO 0600-LOCATE-BUSC-LOOP.                                CL**1
00616                                                                      CL**1
00617      MOVE CLAS-BUSC-EXCL (CLAS-INDEXB) TO WS-EXCL-BUSC-SW.           CL**1
00618                                                                      CL**1
00619  0699-EXIT.                                                          CL**1
00620      EXIT.                                                           CL**1
00621                                                                      CL**1
00622  0700-LOCATE-STATE.                                                  CL**1
00623                                                                      CL**1
00624      MOVE SPACES TO SAVE-STATE, WS-REPORT-SW.                        CL**1
00625      MOVE +0 TO CLAS-INDEXS.                                         CL**1
00626                                                                      CL**1
00627  0700-LOCATE-STATE-LOOP.                                             CL**1
00628                                                                      CL**1
00629      ADD +1 TO CLAS-INDEXS.                                          CL**1
00630                                                                      CL**1
00631      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                           CL**1
00632         DISPLAY 'NO STATE RECORD FOR THIS STATE ', EP-STATE          CL**1
00633         MOVE 'NO STATE RECORD EXISTS    '  TO WS-ABEND-MESSAGE       CL**1
00634         MOVE '0304'                        TO WS-RETURN-CODE         CL**1
00635         GO TO ABEND-PGM.                                             CL**1
00636                                                                      CL**1
00637      IF STATE-SUB (CLAS-INDEXS) EQUAL EP-STATE                       CL**1
00638          MOVE EP-STATE TO SAVE-STATE                                 CL**1
00639          MOVE STATE-CALL-BREAK (CLAS-INDEXS) TO WS-REPORT-SW         CL**1
00640          GO TO 0700-EXIT.                                            CL**1
00641                                                                      CL**1
00642      GO TO 0700-LOCATE-STATE-LOOP.                                   CL**1
00643  0700-EXIT.                                                          CL**1
00644      EXIT.                                                           CL**1
00645                                                                      CL**1
00646  0800-PRINT-HEADINGS.                                                CL**1
00647                                                                      CL**1
00648      MOVE '1'                    TO X.                               CL**1
00649      MOVE HEAD-1                 TO P-DATA.                          CL**1
00650      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00651                                                                      CL**1
00652      MOVE ' ' TO X.                                                  CL**1
00653      MOVE WS-CURRENT-DATE        TO HD-DATE.                         CL**1
00654      MOVE COMPANY-NAME           TO HD-CLIENT.                       CL**1
00655      MOVE HEAD-2                 TO P-DATA.                          CL**1
00656      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00657                                                                      CL**1
00658      MOVE 1                      TO HD-PAGE.                         CL**2
00659      MOVE ALPH-DATE              TO HD-ALF-DTE.                      CL**2
00660      MOVE HEAD-3                 TO P-DATA.                          CL**2
00661      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00662                                                                      CL**1
00663      MOVE SPACES                 TO P-DATA.                          CL**2
00664      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00665                                                                      CL**1
00666      MOVE '  RESERVE/RETRO EXTRACT COMPLETE'                         CL**2
00667                                  TO P-DATA.                          CL**2
00668      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00669                                                                      CL**1
00670      MOVE SPACES                 TO P-DATA.                          CL**2
00671      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00672                                                                      CL**1
00673      MOVE '  TOTAL EPEC RECORDS READ       '                         CL**2
00674                                  TO DISPLAY-MESS.                    CL**2
00675      MOVE EP-RECORDS             TO DISPLAY-COUNT.                   CL**2
00676      MOVE DISPLAY-LINE           TO P-DATA.                          CL**2
00677      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00678                                                                      CL**1
00679      MOVE '  TOTAL EXTRACT RECORDS WRITTEN '                         CL**2
00680                                  TO DISPLAY-MESS.                    CL**2
00681      MOVE X-RECORDS              TO DISPLAY-COUNT.                   CL**2
00682      MOVE DISPLAY-LINE           TO P-DATA.                          CL**2
00683      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00684                                                                      CL**1
00685      MOVE '  TOTAL TEXAS ADJUSTMENTS       '                         CL**2
00686                                  TO DISPLAY-MESS.                    CL**2
00687      MOVE TEX-RECORDS            TO DISPLAY-COUNT.                   CL**2
00688      MOVE DISPLAY-LINE           TO P-DATA.                          CL**2
00689      PERFORM 1000-PRINT-RTN THRU 1010-PRINT-RTN-EXIT.                CL**1
00690                                                                      CL**1
00691      GO TO 1020-CLOSE-FICH.                                          CL**1
00692                                                                      CL**1
00693  1000-PRINT-RTN.                                                     CL**1
00694                              COPY ELCPRT2.                           CL**1
00695                                                                      CL**1
00696  1010-PRINT-RTN-EXIT.                                                CL**1
00697      EXIT.                                                           CL**1
00698                                                                      CL**1
00699  1020-CLOSE-FICH.                                                    CL**1
00700                              COPY ELCPRTC.                           CL**1
00701                                                                      CL**1
00702                                                                      CL**1
00703  9999-E-O-J.                                                         CL**1
00704                                                                      CL**1
00705      CLOSE EPEC-IN  ERACCTT  CALL-EXTRACTS                           CL**1
00706            PRNTR.                                                    CL**1
00707                                                                      CL**1
00708      IF ERACCTT-FILE-STATUS NOT EQUAL '00'                           CL**1
00709         MOVE ' ERROR ON CLOSE ERACCTT ' TO WS-ABEND-MESSAGE          CL**1
00710         MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS             CL**1
00711         GO TO ABEND-PGM.                                             CL**1
00712                                                                      CL**1
00713      MOVE ZEROS  TO RETURN-CODE.
00713      GOBACK.                                                         CL**1
00714  ABEND-PGM.                                                          CL**1
00715                         COPY ELCABEND.                               CL**1
00716 /                                                                    CL**1
00717  LCP-WRITE-POS-PRT SECTION.                                          CL**1
00718      IF LCP-ASA = '+'                                                CL**1
00719          WRITE PRT AFTER 0 LINE                                      CL**1
00720      ELSE                                                            CL**1
00721      IF LCP-ASA = ' '                                                CL**1
00722          WRITE PRT AFTER ADVANCING 1 LINE                            CL**1
00723      ELSE                                                            CL**1
00724      IF LCP-ASA = '0'                                                CL**1
00725          WRITE PRT AFTER ADVANCING 2 LINE                            CL**1
00726      ELSE                                                            CL**1
00727      IF LCP-ASA = '-'                                                CL**1
00728          WRITE PRT AFTER ADVANCING 3 LINE                            CL**1
00729      ELSE                                                            CL**1
00730      IF LCP-ASA = '1'                                                CL**1
00731          WRITE PRT AFTER ADVANCING PAGE                              CL**1
00732      ELSE                                                            CL**1
00733      IF LCP-ASA = '2'                                                CL**1
00734          WRITE PRT AFTER ADVANCING LCP-CH2                           CL**1
00735      ELSE                                                            CL**1
00736      IF LCP-ASA = '3'                                                CL**1
00737          WRITE PRT AFTER ADVANCING LCP-CH3                           CL**1
00738      ELSE                                                            CL**1
00739      IF LCP-ASA = '4'                                                CL**1
00740          WRITE PRT AFTER ADVANCING LCP-CH4                           CL**1
00741      ELSE                                                            CL**1
00742      IF LCP-ASA = '5'                                                CL**1
00743          WRITE PRT AFTER ADVANCING LCP-CH5                           CL**1
00744      ELSE                                                            CL**1
00745      IF LCP-ASA = '6'                                                CL**1
00746          WRITE PRT AFTER ADVANCING LCP-CH6                           CL**1
00747      ELSE                                                            CL**1
00748      IF LCP-ASA = '7'                                                CL**1
00749          WRITE PRT AFTER ADVANCING LCP-CH7                           CL**1
00750      ELSE                                                            CL**1
00751      IF LCP-ASA = '8'                                                CL**1
00752          WRITE PRT AFTER ADVANCING LCP-CH8                           CL**1
00753      ELSE                                                            CL**1
00754      IF LCP-ASA = '9'                                                CL**1
00755          WRITE PRT AFTER ADVANCING LCP-CH9                           CL**1
00756      ELSE                                                            CL**1
00757      IF LCP-ASA = 'A'                                                CL**1
00758          WRITE PRT AFTER ADVANCING LCP-CH10                          CL**1
00759      ELSE                                                            CL**1
00760      IF LCP-ASA = 'B'                                                CL**1
00761          WRITE PRT AFTER ADVANCING LCP-CH11                          CL**1
00762      ELSE                                                            CL**1
00763      IF LCP-ASA = 'C'                                                CL**1
00764          WRITE PRT AFTER ADVANCING LCP-CH12                          CL**1
00765      ELSE                                                            CL**1
00766      IF LCP-ASA = 'V'                                                CL**1
00767          WRITE PRT AFTER ADVANCING LCP-P01                           CL**1
00768      ELSE                                                            CL**1
00769      IF LCP-ASA = 'W'                                                CL**1
00770          WRITE PRT AFTER ADVANCING LCP-P02                           CL**1
00771      ELSE                                                            CL**1
00772      DISPLAY 'ASA CODE ERROR'.                                       CL**1
00773  LCP-WRITE-END-PRT.                                                  CL**1
00774      EXIT.                                                           CL**1
