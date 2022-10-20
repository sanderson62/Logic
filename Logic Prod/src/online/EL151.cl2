00001  IDENTIFICATION DIVISION.                                         06/18/96
00002                                                                   EL151
00003  PROGRAM-ID.                 EL151 .                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 06/06/95 13:08:11.                    CL**4
00007 *                            VMOD=2.012.                             CL*12
00008 *                                                                 EL151
00009 *AUTHOR.    LOGIC, INC.                                              CL**4
00010 *           DALLAS, TEXAS.                                           CL**4
00011                                                                   EL151
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL151
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL151
050506******************************************************************
050506*                   C H A N G E   L O G
050506*
050506* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
050506*-----------------------------------------------------------------
050506*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
050506* EFFECTIVE    NUMBER
050506*-----------------------------------------------------------------
050506* 050506    2006030600001  AJRA  ADD PROOF DATE TO DENIAL SCREEN
120808* 120808    2008100900001  PEMA  ADD FOR DENIAL CODE
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
043019* 043019  IR2019042300001  PEMA  DISALLOW PROOF DT > REC DT OR < INC DT
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
050506******************************************************************
00024 *REMARKS.                                                            CL**2
00025                                                                   EL151
00026 *    SCREENS     - EL151S - DENIALS                                  CL**2
00027                                                                   EL151
00028 *    ENTERED BY  - EL150 - STATUS AND DISPOSITION                    CL**2
00029                                                                   EL151
00030 *    EXIT TO     - EL150 - CALLING PROGRAM                           CL**2
00031                                                                   EL151
00032 *    INPUT FILE  - ELMSTR - CLAIM MASTER                             CL**2
00033 *                - ELTRLR - ACTIVITY TRAILERS                        CL**2
00034 *                - ERNOTE - CERTIFICATE NOTES                        CL**4
00035                                                                   EL151
00036 *    OUTPUT FILE - ELMSTR - CLAIM MASTER                             CL**2
00037 *                - ELTRLR - ACTIVITY TRAILERS                        CL**2
00038                                                                   EL151
00039 *    COMMAREA    - PASSED                                            CL**2
00040                                                                   EL151
00041 *    ERROR-CODES ACCESSED - 132,  133, 270, 137, 29, 315, 008, 50    CL**2
00042                                                                   EL151
00043 *    NARRATIVE   - 1- PROVIDE CREATION OF DENIAL TRAILERS            CL**2
00044 *                  2- UPDATE CLAIM STATUS TYPE TO CLOSED (ELMSTR)    CL**2
00045 *                  3- UPDATE RESERVE-EXPENSE TRAILER STATUS TO       CL**2
00046 *                     CLOSED, REASON DENIAL                          CL**2
00047                                                                   EL151
00048      EJECT                                                        EL151
00049  ENVIRONMENT DIVISION.                                            EL151
00050                                                                   EL151
00051  DATA DIVISION.                                                   EL151
00052                                                                   EL151
00053  WORKING-STORAGE SECTION.                                         EL151
00054                                                                   EL151
00055  77  FILLER  PIC X(32)  VALUE '********************************'. EL151
00056  77  FILLER  PIC X(32)  VALUE '*   EL151  WORKING STORAGE     *'. EL151
00057  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.012 ********'.     CL*12
00058                                                                   EL151
00059                              COPY ELCSCTM.                           CL**3
00060                                                                   EL151
00061                              COPY ELCSCRTY.                          CL**3
00062                                                                      CL**4
00063                              COPY ELCNWA.                            CL**4
00064                                                                   EL151
00065  01  WS-DATE-AREA.                                                EL151
00066      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL151
00067      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL151
CIDMOD     05  WS-BLANK            PIC X       VALUE ' '.                    000
00068                                                                   EL151
00069  01  WS-SCRATCH-AREA.                                             EL151
00070      05  GETMAIN-SPACE               PIC X     VALUE SPACE.       EL151
00071      05  WS-TRLR-LENGTH              PIC S9(4) VALUE +200  COMP.     CL**2
00072      05  WS-DMO-LENGTH               PIC S9(4) VALUE +108  COMP.     CL**5
00073      05  WS-DCT-LENGTH               PIC S9(4) VALUE +53   COMP.     CL**5
00074      05  SC-ITEM                     PIC S9(4) VALUE +0001 COMP.  EL151
00075                                                                   EL151
00076      05  WS-TRANS-ID                 PIC X(4)   VALUE 'EX32'.     EL151
00077      05  WS-SEQ-NO-SAVED             PIC S9(4)   VALUE ZEROS.     EL151
00078                                                                   EL151
00079      05  WS-ORIG-SEQ-CNT             PIC S9(4)   VALUE ZEROS.     EL151
00080                                                                   EL151
00081      05  TR-SUB                      PIC S9(4)   VALUE ZERO  COMP.EL151
00082      05  TR-SUB-2                    PIC S9(4)   VALUE ZERO  COMP.EL151
00083      05  MISC-SUB                    PIC S9(03)  COMP-3  VALUE +0.EL151
00084                                                                   EL151
00085      05  MAP-NAME                    PIC X(08)   VALUE 'EL151A'.     CL**3
00086      05  MAPSET-NAME                 PIC X(08)   VALUE 'EL151S'.     CL**3
00087                                                                   EL151
00088      05  ELTRLR-FILE-ID              PIC X(08)   VALUE 'ELTRLR'.     CL**3
00089      05  ELMSTR-FILE-ID              PIC X(08)   VALUE 'ELMSTR'.     CL**3
00090      05  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.     CL**3
00091      05  ELARCH-FILE-ID              PIC X(08)   VALUE 'ELARCH'.     CL**3
00092                                                                   EL151
00093      05  THIS-PGM                    PIC X(08)   VALUE  'EL151'.     CL**3
00094                                                                      CL**3
00095      05  WS-OC-LOAD-SW               PIC X(01)   VALUE SPACE.        CL**3
00096          88  OC-HISTORY-LOADED       VALUE 'X'.                   EL151
00097                                                                   EL151
00098      05  WS-CLAIM-SEQ-SW             PIC X(01)   VALUE SPACE.        CL**3
00099          88  NO-SEQ-NUMBER           VALUE 'X'.                   EL151
00100          88  SEQ-NUMBER-EXIST        VALUE ' '.                   EL151
00101          88  NO-ACCT-TRLR            VALUE 'Y'.                   EL151
00102                                                                   EL151
00103      05  WS-RESET-SW                 PIC X(01)   VALUE 'N'.          CL**3
00104      05  WS-REC-FOUND-SW             PIC X(01)   VALUE 'N'.          CL**3
00105      05  WS-BROWSE-SW                PIC X(01)   VALUE 'N'.          CL**3
00106      05  WS-UPDATE-SW                PIC X(01)   VALUE 'N'.          CL**3
00107                                                                      CL**3
00108      05  TIME-IN                     PIC S9(7).                   EL151
00109      05  TIME-OUT-R REDEFINES TIME-IN.                               CL**3
00110          10  FILLER                  PIC X(01).                      CL**3
00111          10  TIME-OUT                PIC 99V99.                      CL**3
00112          10  FILLER                  PIC X(02).                      CL**3
050506
050505     05  DEEDIT-FIELD                PIC X(15).                    
050506     05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15). 
050506
050506     05  WS-PRF-DT                   PIC X(2).
050506     05  WS-MAX-LETTER-ANSWER-DT     PIC X(2)  VALUE LOW-VALUES.
00113                                                                   EL151
00114      EJECT                                                           CL**3
00115      05  DENIAL-TABLE.                                               CL**3
00116          10  FILLER                  PIC X(44)    VALUE              CL**3
00117              'DPE  PRE-EXISTING DISABILITY                '.         CL**3
00118          10  FILLER                  PIC X(44)    VALUE              CL**3
00119              'DDN  DISABILITY NOT COVERED DUE TO NATURE   '.         CL**3
00120          10  FILLER                  PIC X(44)    VALUE              CL**3
00121              'DIR  INSURED RECOVERED FROM DISABILITY      '.         CL**3
00122          10  FILLER                  PIC X(44)    VALUE              CL**3
00123              'DEM  EMPLOYMENT INDICATES OCCUPATION NOT MET'.         CL**3
00124          10  FILLER                  PIC X(44)    VALUE              CL**3
00125              'DWP  DISABILITY WAITING PERIOD NOT SATISFIED'.         CL**3
00126          10  FILLER                  PIC X(44)    VALUE              CL**3
00127              'DNC  NO COVERAGE                            '.         CL**3
00128          10  FILLER                  PIC X(44)    VALUE              CL**3
00129              'DAP  APPLICATION MISREPRESENTATION          '.         CL**3
00130          10  FILLER                  PIC X(44)    VALUE              CL**3
00131              'DMA  MIS-STATEMENT OF AGE                   '.         CL**3
00132          10  FILLER                  PIC X(44)    VALUE              CL**3
00133              'DSU  SUICIDE OR SELF-INFLICTED INJURY       '.         CL**3
00134          10  FILLER                  PIC X(44)    VALUE              CL**3
00135              'DOT  OTHER                                  '.         CL**3
00136          10  FILLER                  PIC X(44)    VALUE              CL**3
00137              'DAR  AWARE OF UNEMPLOY./RED. AT INCEPTION   '.         CL**3
00138          10  FILLER                  PIC X(44)    VALUE              CL**3
00139              'DNE  NOT EMPLOYED SIX MONTHS PRIOR          '.         CL**3
00140          10  FILLER                  PIC X(44)    VALUE              CL**3
00141              'DNR  NOT MADE REDUNDANT/NOT UNEMPLOYED      '.         CL**3
00142          10  FILLER                  PIC X(44)    VALUE              CL**3
00143              'DSE  SELF EMPLOYED                          '.         CL**3
00144          10  FILLER                  PIC X(44)    VALUE              CL**3
00145              'DTW  TEMPORARY WORK                         '.         CL**3
00146          10  FILLER                  PIC X(44)    VALUE              CL**3
00147              'LAP  APPLICATION MISREPRESENTATION          '.         CL**3
00148          10  FILLER                  PIC X(44)    VALUE              CL**3
00149              'LMA  MIS-STATEMENT OF AGE                   '.         CL**3
00150          10  FILLER                  PIC X(44)    VALUE              CL**3
00151              'LNC  NO COVERAGE                            '.         CL**3
00152          10  FILLER                  PIC X(44)    VALUE              CL**3
00153              'LOT  OTHER                                  '.         CL**3
00154      05  DEN-TABLE REDEFINES DENIAL-TABLE OCCURS 19                  CL**3
00155          INDEXED BY DEN-INDEX.                                       CL**3
00156          10  DENIAL-CODE.                                            CL**3
00157              15  DENIAL-1-1          PIC X(01).                      CL**3
00158              15  DENIAL-2-3          PIC X(02).                      CL**3
00159              15  DENIAL-4-4          PIC X(01).                      CL**3
00160          10  FILLER                  PIC X(01).                      CL**3
00161          10  DENIAL-DESC             PIC X(39).                      CL**3
00162                                                                      CL**3
00163      EJECT                                                           CL**3
00164  01  ACCESS-KEYS.                                                    CL**3
00165      05  WS-TRAILER-KEY.                                          EL151
00166          10  WS-CLAIM-KEY.                                        EL151
00167              15  WS-KEY-COMPANY-CD       PIC X.                   EL151
00168              15  WS-KEY-CARRIER          PIC X.                   EL151
00169              15  WS-KEY-CLAIM-NO         PIC X(7).                EL151
00170              15  WS-KEY-CERT-NO.                                  EL151
00171                  20  WS-KEY-CERT-PRIME   PIC X(10).               EL151
00172                  20  WS-KEY-CERT-SFX     PIC X.                   EL151
00173          10  WS-KEY-SEQUENCE-NO      PIC   S9(4) COMP.            EL151
00174                                                                   EL151
00175      05  ELARCH-KEY.                                                 CL**3
00176          10  ELARCH-COMPANY-CD   PIC X(01).                          CL**3
00177          10  ELARCH-ARCHIVE-NO   PIC S9(08)  COMP.                   CL**3
00178          10  ELARCH-RECORD-TYPE  PIC X(01).                          CL**3
00179          10  ELARCH-SEQ-NO       PIC S9(04)  COMP.                   CL**3
00180                                                                      CL**3
120808     05  WS-ELDENY-KEY.
               10  ELDENY-COMPANY-CD   PIC X.
               10  ELDENY-DENIAL-CODE  PIC X(4).
120808         10  FILLER              PIC X(10).

00181      05  ELCNTL-KEY.                                                 CL**3
00182          10  ELCNTL-COMPANY-ID   PIC X(03).                          CL**3
00183          10  ELCNTL-RECORD-TYPE  PIC X(01).                          CL**3
00184          10  ELCNTL-ACCESS       PIC X(04).                          CL**3
00185          10  ELCNTL-SEQ-NO       PIC S9(04)  COMP.                   CL**3
00186                                                                      CL**3
00187      05  W-NOTE-KEY.                                                 CL**4
00188          10  W-NOTE-COMP-CD      PIC X.                              CL**4
00189          10  W-NOTE-CERT-KEY.                                        CL**4
00190              16  W-NOTE-CARRIER  PIC X.                              CL**4
00191              16  W-NOTE-GROUPING PIC X(6).                           CL**4
00192              16  W-NOTE-STATE    PIC XX.                             CL**4
00193              16  W-NOTE-ACCOUNT  PIC X(10).                          CL**4
00194              16  W-NOTE-EFF-DT   PIC XX.                             CL**4
00195              16  W-NOTE-CERT-NO  PIC X(11).                          CL**4
00196                                                                      CL**4
120808 01  WS-RESPONSE             PIC S9(8)   COMP.
           88  RESP-NORMAL              VALUE +00.
           88  RESP-ERROR               VALUE +01.
           88  RESP-NOTFND              VALUE +13.
           88  RESP-NOTOPEN             VALUE +19.
120808     88  RESP-ENDFILE             VALUE +20.

00197  01  ERROR-MESSAGES.                                              EL151
00198      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL151
00199      12  ER-0004                 PIC X(4)  VALUE '0004'.             CL**3
00200      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL151
050506     12  ER-0021                 PIC X(4)  VALUE '0021'.
00201      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL151
00202      12  ER-0050                 PIC X(4)  VALUE '0050'.          EL151
00203      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL151
00204      12  ER-0132                 PIC X(4)  VALUE '0132'.          EL151
00205      12  ER-0133                 PIC X(4)  VALUE '0133'.          EL151
00206      12  ER-0137                 PIC X(4)  VALUE '0137'.          EL151
00207      12  ER-0154                 PIC X(4)  VALUE '0154'.          EL151
00208      12  ER-0172                 PIC X(4)  VALUE '0172'.          EL151
00209      12  ER-0270                 PIC X(4)  VALUE '0270'.          EL151
00210      12  ER-0483                 PIC X(4)  VALUE '0483'.          EL151
050506     12  ER-0872                 PIC X(4)  VALUE '0872'.
050506     12  ER-0873                 PIC X(4)  VALUE '0873'.
00211      12  ER-0884                 PIC X(4)  VALUE '0884'.             CL**6
00212      12  ER-0919                 PIC X(4)  VALUE '0919'.             CL**4
00213      12  ER-0921                 PIC X(4)  VALUE '0921'.             CL**4
00214      12  ER-0946                 PIC X(4)  VALUE '0946'.             CL**4
00215      12  ER-0947                 PIC X(4)  VALUE '0947'.             CL**4
00216      12  ER-0948                 PIC X(4)  VALUE '0948'.             CL**4
00217      12  ER-0949                 PIC X(4)  VALUE '0949'.             CL**4
00218      12  ER-0950                 PIC X(4)  VALUE '0950'.             CL**4
00219      12  ER-0951                 PIC X(4)  VALUE '0951'.             CL**4
00220      12  ER-0954                 PIC X(4)  VALUE '0954'.             CL**4
00221      12  ER-0974                 PIC X(4)  VALUE '0974'.             CL**9
00222      12  ER-0975                 PIC X(4)  VALUE '0975'.             CL**9
00223      12  ER-8051                 PIC X(4)  VALUE '8051'.             CL**5
00224      12  ER-8052                 PIC X(4)  VALUE '8052'.             CL**5
00225      12  ER-8053                 PIC X(4)  VALUE '8053'.             CL**5
00226      12  ER-8054                 PIC X(4)  VALUE '8054'.             CL**5
00227      12  ER-8055                 PIC X(4)  VALUE '8055'.             CL**5
00228      12  ER-8056                 PIC X(4)  VALUE '8056'.             CL**5
00229      12  ER-8057                 PIC X(4)  VALUE '8057'.             CL**5
00230      12  ER-8058                 PIC X(4)  VALUE '8058'.             CL**5
00231      12  ER-8059                 PIC X(4)  VALUE '8059'.             CL**5
00232      12  ER-8060                 PIC X(4)  VALUE '8060'.             CL**5
00233      12  ER-8061                 PIC X(4)  VALUE '8061'.             CL**5
00234      12  ER-8062                 PIC X(4)  VALUE '8062'.             CL**5
00235      12  ER-8063                 PIC X(4)  VALUE '8063'.             CL**5
00236      12  ER-8064                 PIC X(4)  VALUE '8064'.             CL**5
00237      12  ER-8065                 PIC X(4)  VALUE '8065'.             CL**5
00238      12  ER-8066                 PIC X(4)  VALUE '8066'.             CL**5
00239      12  ER-8152                 PIC X(4)  VALUE '8152'.             CL**8
00240      12  ER-8153                 PIC X(4)  VALUE '8153'.             CL**8
00241      12  ER-8154                 PIC X(4)  VALUE '8154'.             CL*12
00242      12  ER-8155                 PIC X(4)  VALUE '8155'.             CL*12
00243      EJECT                                                        EL151
00244                                      COPY ELCAID.                    CL**3
00245  01  PF-AID REDEFINES DFHAID.                                     EL151
00246      05  FILLER                      PIC X(8).                    EL151
00247      05  PF-VALUES  OCCURS 24        PIC X.                       EL151
00248      EJECT                                                        EL151
00249                                      COPY ELCINTF.                   CL**3
00250      12  PI-REDEFINES REDEFINES PI-PROGRAM-WORK-AREA.                CL**3
00251          16  PI-PREV-TRLR-KEY        PIC X(22).                      CL**3
050506*00252          16  FILLER                  PIC X(618).                     CL**4
050506         16  PI-PROOF-DATE           PIC 9(6).
050506         16  FILLER                  PIC X(612).
00253                                                                      CL**6
00254 * DLO023                                                             CL**6
00255  01  DL23-COMM-LENGTH              PIC S9(4) COMP VALUE +132.        CL**6
00256  01  WS-DLO-CODES-TABLE.                                             CL**6
00257      12  DL23-SYSTEM-ID            PIC XX.                           CL**6
00258      12  DL23-RECORD-TYPE          PIC XX.                           CL**6
00259      12  DL23-RECORD-KEY           PIC X(6).                         CL**6
00260      12  DL23-RETURN-CODE          PIC XX.                           CL**6
00261      12  DL23-CODE-DESC.                                             CL**6
00262          16  DL23-CODE-TEXT        PIC X(20).                        CL**6
00263          16  DL23-RET-CODE         PIC X(40).                        CL**6
00264      12  DL23-GEN-DESC-1           PIC X(20).                        CL**6
00265      12  DL23-GEN-DESC-2           PIC X(20).                        CL**6
00266      12  DL23-GEN-DESC-3           PIC X(20).                        CL**6
00267                                                                   EL151
00268  01  DMD-DATE-YYYYMMDD.                                              CL**4
00269      12  DMD-DECADE          PIC XX      VALUE SPACES.               CL**4
00270      12  DMD-YYMMDD.                                                 CL**4
00271          16  DMD-YY          PIC XX      VALUE SPACES.               CL**4
00272          16  DMD-MM          PIC XX      VALUE SPACES.               CL**4
00273          16  DMD-DD          PIC XX      VALUE SPACES.               CL**4
00274                                                                      CL**4
00275      12  W-NAME-LAST             PIC  X(15).                         CL**4
00276      12  W-NAME-FIRST            PIC  X(15).                         CL**4
00277      12  W-NAME-MIDDLE.                                              CL**4
00278          16  FILLER              PIC  X(01).                         CL**4
00279          16  W-NAME-MIDDLE-2     PIC  X(01).                         CL**4
00280          16  FILLER              PIC  X(13).                         CL**4
00281                                                                      CL**4
00282                                      COPY ELCDCTB.                   CL**4
00283      EJECT                                                           CL**3
00284                                      COPY ELCATTR.                   CL**3
00285      EJECT                                                        EL151
00286                                      COPY ELCLOGOF.                  CL**3
00287      EJECT                                                        EL151
00288                                      COPY ELCDATE.                   CL**3
00289      EJECT                                                        EL151
00290                                      COPY ELCEMIB.                   CL**3
00291      EJECT                                                           CL**3
00292                                      COPY EL151S.                    CL**4
00293      EJECT                                                           CL**4
00294                                      COPY ELCDMO.                    CL**4
00295      EJECT                                                        EL151
00296  LINKAGE SECTION.                                                 EL151
00297  01  DFHCOMMAREA                     PICTURE X(1024).             EL151
00298      EJECT                                                        EL151
00299                                      COPY ELCMSTR.                   CL**3
00300      EJECT                                                           CL**3
00301                                      COPY ELCTRLR.                   CL**3
00302      EJECT                                                           CL**3
00303                                      COPY ELCARCH.                   CL**3
00304      EJECT                                                           CL**3
00305                                      COPY ELCCNTL.

120808                                     COPY ELCDENY.

CIDMOD                                     COPY ELCDAR.                    CL**3
CIDMOD     EJECT                                                           CL**4
00307                                      COPY ERCDMDNT.                  CL**4
00308      EJECT                                                           CL**4
00309  PROCEDURE DIVISION.                                              EL151
00310                                                                   EL151
00311      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL151
00312      MOVE '5'                    TO DC-OPTION-CODE.               EL151
00313      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL**3
00314      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL151
00315      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL151
00316                                                                   EL151
00317  0001-PROCESSING-EXITS.                                           EL151
00318      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL151
00319      IF EIBCALEN NOT GREATER THAN ZEROS                           EL151
00320        GO TO 8800-UNAUTHORIZED-ACCESS.                               CL**3
00321                                                                   EL151
00322      EXEC CICS  HANDLE CONDITION                                  EL151
00323             ERROR    (9990-ABEND)                                    CL**3
00324             PGMIDERR (9600-PGMID-ERROR)                              CL**3
00325      END-EXEC.                                                    EL151
00326                                                                   EL151
00327      IF PI-CALLING-PROGRAM  NOT  = THIS-PGM                          CL**3
00328          IF PI-RETURN-TO-PROGRAM NOT  =  THIS-PGM                    CL**3
00329              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6         CL**3
00330              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5         CL**3
00331              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4         CL**3
00332              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3         CL**3
00333              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2         CL**3
00334              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1         CL**3
00335              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM       CL**3
00336              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**3
00337          ELSE                                                        CL**3
00338              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM         CL**3
00339              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM       CL**3
00340              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1         CL**3
00341              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2         CL**3
00342              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3         CL**3
00343              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4         CL**3
00344              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5         CL**3
00345              MOVE SPACES               TO PI-SAVED-PROGRAM-6.        CL**3
00346                                                                      CL**3
00347      IF EIBAID IS EQUAL TO DFHCLEAR                                  CL**3
00348          GO TO 9400-CLEAR.                                           CL**3
00349                                                                   EL151
00350      IF PI-PROCESSOR-ID = 'LGXX'                                  EL151
00351          NEXT SENTENCE                                            EL151
00352      ELSE                                                         EL151
00353          EXEC CICS READQ TS                                       EL151
00354              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL151
00355              INTO    (SECURITY-CONTROL)                           EL151
00356              LENGTH  (SC-COMM-LENGTH)                             EL151
00357              ITEM    (SC-ITEM)                                    EL151
00358          END-EXEC                                                 EL151
00359          MOVE SC-CLAIMS-DISPLAY (6)    TO  PI-DISPLAY-CAP         EL151
00360          MOVE SC-CLAIMS-UPDATE  (6)    TO  PI-MODIFY-CAP.         EL151
00361                                                                   EL151
00362      IF EIBTRNID IS EQUAL TO WS-TRANS-ID                             CL**3
00363          GO TO 0200-RECEIVE.                                         CL**3
00364                                                                   EL151
00365  0100-FIRST-TIME-IN.                                                 CL**3
00366                                                                      CL**3
00367      MOVE LOW-VALUES                 TO  EL151AO                     CL**3
00368      MOVE -1                         TO  MLINE1L                     CL**3
00369      MOVE 1                          TO  EMI-NUMBER-OF-LINES.        CL**3
00370                                                                      CL**3
00371      MOVE SPACE                      TO  DC-ERROR-CODE               CL**3
00372                                          WS-CLAIM-SEQ-SW             CL**3
00373                                          MERMSG2O.                   CL**3
00374                                                                      CL**3
00375      MOVE '153A'                     TO  PI-CURRENT-SCREEN-NO.       CL**3
050506
050506     MOVE 0                          TO  PI-PROOF-DATE.
050506     PERFORM 3550-FIND-LETTER-TRLR THRU 3550-EXIT.
00376                                                                      CL**3
00377      GO TO 8100-SEND-INITIAL-MAP.                                    CL**3
00378                                                                      CL**3
00379  0200-RECEIVE.                                                       CL**3
00380                                                                      CL**3
00381      MOVE LOW-VALUES              TO  EL151AI.                       CL**3
00382                                                                      CL**3
00383      IF (EIBAID IS EQUAL TO DFHPA1 OR DFHPA2 OR DFHPA3)              CL**3
00384          MOVE ER-0008             TO  EMI-ERROR                      CL**3
00385          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**3
00386          MOVE -1                  TO  MPFNUMBL                       CL**3
00387          GO TO 8200-SEND-DATAONLY.                                   CL**3
00388                                                                      CL**3
00389      EXEC CICS RECEIVE                                               CL**3
00390          MAP      (MAP-NAME)                                         CL**3
00391          MAPSET   (MAPSET-NAME)                                      CL**3
00392          INTO     (EL151AI)                                          CL**3
00393      END-EXEC.                                                       CL**3
00394                                                                      CL**3
00395      IF MPFNUMBL IS EQUAL TO +0                                      CL**3
00396          GO TO 0300-CHECK-PFKEYS.                                    CL**3
00397                                                                      CL**3
00398      IF EIBAID IS NOT EQUAL TO DFHENTER                              CL**3
00399          MOVE ER-0004             TO  EMI-ERROR                      CL**3
00400          GO TO 0320-INPUT-ERROR.                                     CL**3
00401                                                                      CL**3
00402      IF (MPFNUMBI NUMERIC) AND (MPFNUMBI GREATER 0 AND LESS 25)      CL**3
00403          MOVE PF-VALUES (MPFNUMBI)    TO  EIBAID                     CL**3
00404      ELSE                                                            CL**3
00405          MOVE ER-0029                 TO  EMI-ERROR                  CL**3
00406          GO TO 0320-INPUT-ERROR.                                     CL**3
00407                                                                      CL**3
00408  0300-CHECK-PFKEYS.                                                  CL**3
00409                                                                      CL**3
00410      IF EIBAID IS EQUAL TO DFHPF12                                   CL**3
00411         MOVE  'EL010'            TO  THIS-PGM                        CL**3
00412         GO TO 9300-XCTL.                                             CL**3
00413                                                                      CL**3
00414      IF EIBAID IS EQUAL TO DFHPF23                                   CL**3
00415         MOVE EIBAID              TO  PI-ENTRY-CD-1                   CL*12
00416         MOVE 'EL005'             TO  THIS-PGM                        CL**3
00417         GO TO 9300-XCTL.                                             CL**3
00418                                                                      CL**3
00419      IF EIBAID IS EQUAL TO DFHPF24                                   CL**3
00420         MOVE EIBAID              TO  PI-ENTRY-CD-1                   CL*12
00421         MOVE  'EL126'            TO  THIS-PGM                        CL**3
00422         GO TO 9300-XCTL.                                             CL**3
00423                                                                      CL**3
00424      IF EIBAID IS EQUAL TO DFHENTER                                  CL**3
00425         GO TO 0330-EDIT-DATA.                                        CL**3
00426                                                                      CL**3
00427      MOVE ER-0029                TO  EMI-ERROR.                      CL**3
00428                                                                      CL**3
00429  0320-INPUT-ERROR.                                                   CL**3
00430      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
00431                                                                      CL**3
00432      MOVE -1                     TO  MPFNUMBL.                       CL**3
00433      GO TO 8200-SEND-DATAONLY.                                       CL**3
00434                                                                      CL**3
00435      EJECT                                                           CL**3
00436  0330-EDIT-DATA.                                                     CL**3
00437      IF NOT MODIFY-CAP                                            EL151
00438         MOVE 'UPDATE'             TO  SM-READ                     EL151
00439         PERFORM 9995-SECURITY-VIOLATION                           EL151
00440         MOVE ER-0070              TO  EMI-ERROR                   EL151
00441         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**3
00442         GO TO 8100-SEND-INITIAL-MAP.                                 CL**3
00443                                                                   EL151
030612     IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CID' OR 'DCC'
062121                     OR 'AHL' or 'VPP' or 'FNL'
00445          IF MLINE1I = (LOW-VALUES OR SPACES) AND                     CL**3
00446             MLINE2I = (LOW-VALUES OR SPACES) AND                     CL**3
00447             MDENCDI = (LOW-VALUES OR SPACES)                         CL**3
00448              MOVE ER-0483        TO  EMI-ERROR                       CL**3
00449              MOVE -1             TO  MLINE1L                         CL**3
00450              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**3
00451              GO TO 8100-SEND-INITIAL-MAP                             CL**3
00452          ELSE                                                        CL**3
00453              NEXT SENTENCE                                           CL**3
00454      ELSE                                                            CL**3
00455          IF MLINE1I  = (LOW-VALUES OR SPACES)  AND                   CL**3
00456             MLINE2I  = (LOW-VALUES OR SPACES)
00457              MOVE ER-0483        TO  EMI-ERROR                       CL**3
00458              MOVE -1             TO  MLINE1L                         CL**3
00459              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**3
00460              GO TO 8100-SEND-INITIAL-MAP.                            CL**3
050506         
050506     IF MPRFDTL > 0
050506         MOVE MPRFDTI TO DEEDIT-FIELD
050506         PERFORM 9650-DEEDIT      THRU 9650-EXIT
050506         MOVE DEEDIT-FIELD-V0  TO PI-PROOF-DATE
050506         MOVE PI-PROOF-DATE    TO MPRFDTO         
050506         INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
050506     END-IF.
050506
050506     IF PI-PROOF-DATE = ZEROS OR LOW-VALUES
050506*     IF  MPRFDTI  = LOW-VALUES OR SPACES
050506         MOVE ER-0872             TO EMI-ERROR
050506         MOVE -1                  TO MPRFDTL
050506         MOVE AL-UABON            TO MPRFDTA
050506         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT  
050506         GO TO 8200-SEND-DATAONLY
050506     END-IF.
050506
050506     IF  PI-PROOF-DATE > 0
050506         MOVE PI-PROOF-DATE       TO DEEDIT-FIELD
050506         PERFORM 9650-DEEDIT      THRU 9650-EXIT
050506         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY 
050506         MOVE '4'                 TO DC-OPTION-CODE
050506         PERFORM 9700-LINK-DATE-CONVERT THRU  9700-EXIT
050506         IF DATE-CONVERSION-ERROR                     
050506            MOVE ER-0021          TO EMI-ERROR        
050506            MOVE -1               TO MPRFDTL         
050506            MOVE AL-UABON         TO MPRFDTA         
050506            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT  
050506            GO TO 8200-SEND-DATAONLY
050506         ELSE 
050506            IF DC-BIN-DATE-1 > SAVE-BIN-DATE
050506                MOVE ER-0873      TO EMI-ERROR
050506                MOVE -1           TO MPRFDTL
050506                MOVE AL-UABON     TO MPRFDTA
050506                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
050506                GO TO 8200-SEND-DATAONLY
050506            ELSE                                        
050506                MOVE DC-BIN-DATE-1    TO WS-PRF-DT     
050506                MOVE DEEDIT-FIELD-V0  TO PI-PROOF-DATE
050506                MOVE PI-PROOF-DATE    TO MPRFDTO         
050506                INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
050506            END-IF
050506         END-IF
050506     END-IF.                  
00461                                                                   EL151
00462      MOVE LOW-VALUES             TO  WS-TRAILER-KEY.                 CL**4
00463      MOVE PI-COMPANY-CD          TO  WS-KEY-COMPANY-CD.              CL**3
00464      MOVE PI-CARRIER             TO  WS-KEY-CARRIER.                 CL**3
00465      MOVE PI-CLAIM-NO            TO  WS-KEY-CLAIM-NO.                CL**3
00466      MOVE PI-CERT-NO             TO  WS-KEY-CERT-NO.                 CL**3
00467                                                                   EL151
00468      PERFORM 7000-READ-ELMSTR-UPDATE THRU 7000-EXIT.                 CL**3
00469                                                                      CL**3
00470      IF CL-LAST-TRL-AVAIL                                            CL**3
00471          MOVE ER-0137            TO  EMI-ERROR                       CL**3
00472          MOVE -1                 TO  MLINE1L                         CL**3
00473          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**3
00474          GO TO 8200-SEND-DATAONLY.                                   CL**3

043019     if ws-prf-dt < cl-incurred-dt
043019        MOVE ER-0873             TO EMI-ERROR
043019        MOVE -1                  TO MPRFDTL
043019        MOVE AL-UABON            TO MPRFDTA
043019        PERFORM 9900-ERROR-FORMAT
043019                                 THRU 9900-EXIT
043019        GO TO 8200-SEND-DATAONLY
043019     end-if

00476      MOVE CL-TRAILER-SEQ-CNT     TO  WS-ORIG-SEQ-CNT.                CL**3
00477      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.                             CL**3
00478      MOVE CL-TRAILER-SEQ-CNT     TO  WS-SEQ-NO-SAVED.                CL**3
00479      MOVE 'C'                    TO  CL-CLAIM-STATUS.                CL**3
00480      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.             CL**3
00481      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.             CL**3
00482      MOVE '2'                    TO  CL-LAST-CLOSE-REASON.           CL**3
00483      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.           CL**3
00484      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT                CL**3
00485                                      CL-LAST-CLOSE-DT.               CL**3
00486                                                                      CL**3
00487      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL**3
00488          IF MDENCDL IS GREATER THAN +0                               CL**3
00489              MOVE MDENCDI        TO  CL-FILE-LOCATION.               CL**3
00490                                                                      CL**3
CIDMOD*************************************************************     ***  000
CIDMOD*****         START BUILDING ACTIVITY RECORD HERE          **     ***  000
CIDMOD*************************************************************     ***  000
062121     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' OR 'FNL'                    000
CIDMOD         PERFORM 2300-GETMAIN-DLYACTV THRU                             000
CIDMOD                 2300-EXIT                                             000
CIDMOD         PERFORM 3500-BUILD-ACTIVITY-RECORD THRU                       000
CIDMOD                 3500-EXIT                                             000
CIDMOD         PERFORM 2350-WRITE-DLYACTV THRU                               000
CIDMOD                 2350-EXIT                                             000
CIDMOD     END-IF.                                                           000
CIDMOD                                                                       000
CIDMOD*************************************************************     ***  000
CIDMOD*****            END OF BUILDING ACTIVITY RECORD           **     ***  000
CIDMOD*************************************************************     ***  000
CIDMOD                                                                       000
00491      PERFORM 7120-READ-ELTRLR-UPDATE THRU 7120-EXIT.                 CL**3
00492                                                                      CL**3
00493      MOVE +1                     TO  MISC-SUB.                       CL**3
00494      MOVE SPACES                 TO  WS-OC-LOAD-SW.                  CL**3
00495      PERFORM 1000-SET-OPEN-CLOSE-HIST THRU 1000-EXIT.                CL**3
00496                                                                      CL**3
00497      MOVE +01                    TO  TR-SUB                          CL**3
00498                                      TR-SUB-2.                       CL**3
00499      PERFORM 1010-UPDATE-TR1-OC   THRU 1010-EXIT                     CL**3
00500              UNTIL  OC-HISTORY-LOADED.                            EL151
00501                                                                   EL151
00502      MOVE PI-PROCESSOR-ID        TO AT-RESERVES-LAST-UPDATED-BY      CL**3
00503      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS             CL**3
00504      MOVE SAVE-BIN-DATE          TO AT-RESERVES-LAST-MAINT-DT.       CL**3
00505                                                                      CL**4
00506      IF PI-COMPANY-ID = 'DMD'                                        CL**4
00507          PERFORM 2000-CREATE-DMD-DMO  THRU  2000-EXIT.               CL**4
00508                                                                   EL151
00509      PERFORM 7130-REWRITE-ELTRLR THRU 7130-EXIT.                     CL**3
00510                                                                   EL151
00511      PERFORM 7100-GETMAIN-ELTRLR  THRU 7100-EXIT.                    CL**3
00512      PERFORM 1050-BUILD-DENIAL-TRLR THRU 1090-EXIT.                  CL**6
00513                                                                   EL151
00514      PERFORM 7110-WRITE-ELTRLR    THRU 7110-EXIT.                    CL**3
00515                                                                      CL**3
00516      PERFORM 6100-CHECK-AUTO-ACTIVITY THRU 6100-EXIT.                CL**3
00517                                                                      CL**3
00518      IF WS-REC-FOUND-SW IS EQUAL TO 'N'                              CL**3
00519          GO TO 0340-FINISH-EDIT-DATA.                                CL**3
00520                                                                      CL**3
00521      IF CL-ACTIVITY-CODE IS EQUAL TO 09                              CL**3
00522          GO TO 0340-FINISH-EDIT-DATA.                                CL**3
00523                                                                      CL**3
00524      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK') AND               CL**3
00525         (CL-ACTIVITY-CODE IS EQUAL TO 11)                            CL**3
00526          GO TO 0340-FINISH-EDIT-DATA.                                CL**3
00527                                                                      CL**3
00528      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL**3
00529          PERFORM 6000-RESET-AUTO-ACTIVITY THRU 6000-EXIT             CL**3
00530          IF WS-UPDATE-SW IS EQUAL TO 'Y'                             CL**3
00531              MOVE LOW-VALUES         TO  CL-NEXT-RESEND-DT           CL**3
00532                                          CL-NEXT-FOLLOWUP-DT.        CL**3
00533                                                                      CL**3
00534      IF WS-RESET-SW IS EQUAL TO 'Y'                                  CL**3
00535          MOVE ZEROS                  TO  CL-ACTIVITY-CODE            CL**3
00536          MOVE SAVE-BIN-DATE          TO  CL-ACTIVITY-MAINT-DT        CL**3
00537          MOVE 'DENY'                 TO  CL-ACTIVITY-MAINT-TYPE.     CL**3
00538                                                                      CL**3
00539  0340-FINISH-EDIT-DATA.                                              CL**3
00540                                                                      CL**3
00541      PERFORM 7010-REWRITE-ELMSTR  THRU 7010-EXIT.                    CL**3
00542                                                                      CL**3
00543      MOVE ER-0000                TO EMI-ERROR.                       CL**3
00544      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
PEMTST     MOVE -1                     TO MLINE1L.                         CL**3
PEMTST*    MOVE LOW-VALUES             TO MLINE1O                          CL**3
PEMTST*                                   MLINE2O                          CL**3
PEMTST*                                   MDENCDO.                         CL**3
00549      GO TO 8200-SEND-DATAONLY.                                       CL**3
00550                                                                      CL**3
00551      EJECT                                                        EL151
00552  1000-SET-OPEN-CLOSE-HIST.                                           CL**3
00553      PERFORM 1000-BUMP-OPEN-CLOSE-HIST UNTIL                         CL**3
00554         (MISC-SUB GREATER THAN +6 OR                              EL151
00555         AT-OPEN-CLOSE-TYPE (MISC-SUB) = SPACES).                  EL151
00556                                                                   EL151
00557      IF MISC-SUB GREATER THAN +1                                  EL151
00558         SUBTRACT +1 FROM MISC-SUB                                 EL151
00559         IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = 'C'                    EL151
00560            MOVE SAVE-BIN-DATE   TO AT-OPEN-CLOSE-DATE (MISC-SUB)     CL**3
00561            MOVE 'C'             TO AT-OPEN-CLOSE-TYPE (MISC-SUB)  EL151
00562            MOVE 'DENIED'        TO AT-OPEN-CLOSE-REASON (MISC-SUB)EL151
00563            MOVE 'X'             TO WS-OC-LOAD-SW.                 EL151
00564                                                                   EL151
00565      GO TO 1000-EXIT.                                                CL**3
00566                                                                   EL151
00567  1000-BUMP-OPEN-CLOSE-HIST.                                          CL**3
00568      ADD +1 TO MISC-SUB.                                          EL151
00569                                                                   EL151
00570  1000-EXIT.                                                          CL**3
00571      EXIT.                                                        EL151
00572                                                                   EL151
00573  1010-UPDATE-TR1-OC.                                                 CL**3
00574      IF  AT-OPEN-CLOSE-DATE (TR-SUB) = (LOW-VALUE OR SPACES)      EL151
00575          MOVE SAVE-BIN-DATE      TO AT-OPEN-CLOSE-DATE (TR-SUB)      CL**3
00576          MOVE 'C'                TO AT-OPEN-CLOSE-TYPE (TR-SUB)   EL151
00577          MOVE 'DENIED'           TO AT-OPEN-CLOSE-REASON (TR-SUB) EL151
00578          MOVE 'X'                TO WS-OC-LOAD-SW                 EL151
00579          GO TO 1010-EXIT.                                            CL**3
00580                                                                   EL151
00581      ADD 1  TO TR-SUB.                                            EL151
00582      IF  TR-SUB  GREATER THAN 6                                   EL151
00583          MOVE +01                TO TR-SUB                        EL151
00584          MOVE +02                TO TR-SUB-2                      EL151
00585          PERFORM 1020-BUMP    THRU 1020-EXIT                         CL**3
00586           UNTIL TR-SUB-2 GREATER THAN 6                           EL151
00587          MOVE SPACES             TO AT-OPEN-CLOSE-DATE (TR-SUB).  EL151
00588                                                                   EL151
00589  1010-EXIT.                                                          CL**3
00590       EXIT.                                                       EL151
00591                                                                   EL151
00592  1020-BUMP.                                                          CL**3
00593      MOVE AT-OPEN-CLOSE-DATE (TR-SUB-2) TO                        EL151
00594                            AT-OPEN-CLOSE-DATE (TR-SUB).           EL151
00595      MOVE AT-OPEN-CLOSE-TYPE (TR-SUB-2) TO                        EL151
00596                            AT-OPEN-CLOSE-TYPE (TR-SUB).           EL151
00597      MOVE AT-OPEN-CLOSE-REASON (TR-SUB-2) TO                      EL151
00598                            AT-OPEN-CLOSE-REASON (TR-SUB).         EL151
00599                                                                   EL151
00600      ADD +1  TO TR-SUB                                            EL151
00601                 TR-SUB-2.                                         EL151
00602                                                                   EL151
00603  1020-EXIT.                                                          CL**3
00604      EXIT.                                                           CL**3
00605                                                                   EL151
00606      EJECT                                                           CL**3
00607  1050-BUILD-DENIAL-TRLR.                                             CL**3
00608                                                                      CL**2
00609      MOVE 'AT'                   TO AT-RECORD-ID.                    CL**3
00610      MOVE '8'                    TO AT-TRAILER-TYPE.                 CL**3
00611                                                                   EL151
00612 *DLO023                                                              CL**6
00613      IF PI-COMPANY-ID NOT = 'DMD'                                    CL**6
00614          GO TO 1055-EDIT-CID-CODE
           END-IF
00615                                                                      CL**6
00616      IF MDENCDL GREATER +0                                           CL**6
00617          MOVE 'CL'               TO DL23-SYSTEM-ID                   CL**6
00618          MOVE 'DN'               TO DL23-RECORD-TYPE                 CL**6
00619          MOVE MDENCDI            TO DL23-RECORD-KEY                  CL**6
00620          EXEC CICS LINK                                              CL**6
00621              PROGRAM    ('DLO023')                                   CL**6
00622              COMMAREA   (WS-DLO-CODES-TABLE)                         CL**6
00623              LENGTH     (DL23-COMM-LENGTH)                           CL**6
00624          END-EXEC                                                    CL**6
00625          IF DL23-RETURN-CODE = 'OK'                                  CL**6
00626              MOVE AL-UANON       TO MDENCDA                          CL**6
00627           ELSE                                                       CL**6
00628              MOVE AL-UABON       TO MDENCDA                          CL**6
00629              MOVE -1             TO MDENCDL                          CL**6
00630              MOVE ER-0884        TO EMI-ERROR                        CL**6
00631              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*11
00632              PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT           CL*11
00633              PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                 CL*11
00634              GO TO 8200-SEND-DATAONLY.                               CL*11
00635                                                                      CL**6
00636      IF MLINE1I = LOW-VALUES OR SPACES                               CL**6
00637          MOVE SPACES         TO AT-DENIAL-INFO-1                     CL**6
00638       ELSE                                                           CL**6
00639          MOVE MLINE1I        TO AT-DENIAL-INFO-1.                    CL**6
00640                                                                      CL**6
00641      GO TO 1070-CONTINUE.                                            CL**6
00642                                                                      CL**6
       1055-EDIT-CID-CODE.

           IF MDENCDL > +0
              MOVE LOW-VALUES          TO WS-ELDENY-KEY
              MOVE PI-COMPANY-CD       TO ELDENY-COMPANY-CD
              MOVE MDENCDI             TO ELDENY-DENIAL-CODE
              EXEC CICS READ
                 DATASET('ELDENY')
                 SET    (ADDRESS OF DENIAL-CODES)
                 RIDFLD (WS-ELDENY-KEY)
                 RESP   (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 IF MLINE1L > +0
                    STRING DN-DESCRIPTION ' ' MLINE1I
                       DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
                    END-STRING
                 ELSE
                    MOVE DN-DESCRIPTION
                                       TO AT-DENIAL-INFO-1
                 END-IF

                 MOVE DN-RECORD-TYPE   TO CL-DENIAL-TYPE
                 MOVE AT-DENIAL-INFO-1 TO MLINE1O
                 MOVE +1               TO MLINE1L
                 MOVE AL-UANON         TO MLINE1A
                 MOVE DN-RECORD-TYPE
                                       TO CL-DENIAL-TYPE
              ELSE
                 MOVE AL-UABON         TO MDENCDA
                 MOVE -1               TO MDENCDL
                 MOVE ER-0884          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
                 PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT  
                 PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT        
                 GO TO 8200-SEND-DATAONLY
              END-IF
           ELSE
              MOVE AL-UABON            TO MDENCDA
              MOVE -1                  TO MDENCDL
              MOVE ER-0884             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT       
              PERFORM 2250-UNLOCK-CLAIM-MSTR
                                       THRU 2250-EXIT  
              PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT        
              GO TO 8200-SEND-DATAONLY
           END-IF

           GO TO 1070-CONTINUE

           .
00643  1060-CONTINUE.                                                      CL**6
00644      IF MDENCDL IS GREATER THAN +0                                   CL**3
00645          SET DEN-INDEX TO +1                                         CL**3
00646          SEARCH DEN-TABLE VARYING DEN-INDEX                          CL**3
00647              AT END                                                  CL**3
00648                  MOVE MLINE1I                TO  AT-DENIAL-INFO-1    CL**3
00649          WHEN MDENCDI IS EQUAL TO DENIAL-CODE (DEN-INDEX)            CL**3
00650              MOVE DENIAL-DESC (DEN-INDEX)    TO  AT-DENIAL-INFO-1
                                                       MLINE1O
00651      ELSE                                                            CL**3
00652          IF MLINE1I = LOW-VALUES OR SPACES                           CL**3
00653              MOVE SPACES         TO AT-DENIAL-INFO-1                 CL**3
00654          ELSE                                                        CL**3
00655              MOVE MLINE1I        TO AT-DENIAL-INFO-1.                CL**3
00656                                                                      CL**3
00657  1070-CONTINUE.                                                      CL**6
00658      IF  MLINE2I  = LOW-VALUES OR SPACES                             CL**3
00659          MOVE  SPACES            TO AT-DENIAL-INFO-2                 CL**3
00660      ELSE                                                            CL**3
00661          MOVE MLINE2I            TO AT-DENIAL-INFO-2.                CL**3
00662                                                                      CL**3
00663      IF (AT-DENIAL-INFO-1 IS EQUAL TO SPACES OR LOW-VALUES)          CL**3
00664          MOVE AT-DENIAL-INFO-2   TO AT-DENIAL-INFO-1                 CL**3
00665          MOVE SPACES             TO AT-DENIAL-INFO-2.                CL**3
00666                                                                      CL**3
050506     MOVE WS-PRF-DT              TO AT-DENIAL-PROOF-DT.    
00667      MOVE WS-SEQ-NO-SAVED        TO WS-KEY-SEQUENCE-NO.              CL**3
00668                                                                      CL**3
00669      MOVE WS-TRAILER-KEY         TO AT-CONTROL-PRIMARY.              CL**3
00670      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                   CL**3
00671                                     AT-DENIAL-LAST-UPDATED-BY.       CL**3
00672      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.            CL**3
00673      MOVE SAVE-BIN-DATE          TO AT-RECORDED-DT                   CL**3
00674                                     AT-DENIAL-DT                     CL**3
00675                                     AT-DENIAL-LAST-MAINT-DT.         CL**3
00676      MOVE MDENCDI                TO AT-DENIAL-REASON-CODE.           CL**3
00677      MOVE LOW-VALUES             TO AT-RETRACTION-DT.                CL**3
00678                                                                      CL**3
00679  1090-EXIT.                                                          CL**6
00680       EXIT.                                                       EL151
00681       EJECT                                                          CL**3
00682 ******************************************************************   CL**4
00683 *    DMD ONLY  -  CREATE THE DIRECT MARKETING OUTPUT FILE            CL**4
00684 ******************************************************************   CL**4
00685  2000-CREATE-DMD-DMO.                                                CL**4
00686                                                                      CL**4
00687      MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.                 CL**4
00688      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.                  CL**4
00689      MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.                  CL**4
00690                                                                      CL**4
00691      EXEC CICS HANDLE CONDITION                                      CL**4
00692           NOTFND   (2000-NOTE-NOT-FOUND)                             CL**4
00693           END-EXEC.                                                  CL**4
00694                                                                      CL**4
00695      EXEC CICS READ                                                  CL**4
00696           DATASET('ERNOTE')                                          CL**4
00697           SET    (ADDRESS OF CERTIFICATE-NOTE)                       CL**4
00698           RIDFLD (W-NOTE-KEY)                                        CL**4
00699           END-EXEC.                                                  CL**4
00700                                                                      CL**4
00701      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.          CL**4
00702      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.        CL**4
00703      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.          CL**4
00704                                                                      CL**4
00705      IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES                     CL**7
00706          MOVE 'CC'               TO DCT-PRODUCT-CODE                 CL**4
00707      ELSE                                                            CL**4
00708          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.            CL**7
00709                                                                      CL**4
00710      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.             CL**5
00711      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.         CL**4
00712                                                                      CL**4
00713      EXEC CICS LINK                                                  CL**5
00714          PROGRAM    ('DLO006')                                       CL**5
00715          COMMAREA   (DCT-COMMUNICATION-AREA)                         CL**5
00716          LENGTH     (WS-DCT-LENGTH)                                  CL**5
00717      END-EXEC.                                                       CL**5
00718                                                                      CL**5
00719      IF  DCT-RETURN-CODE = 'OK'                                      CL**5
00720          GO TO 2000-CONT.                                            CL**5
00721                                                                      CL**4
00722      IF  DCT-RETURN-CODE = '01' OR '02'                              CL**7
00723          GO TO 2000-EXIT.                                            CL**4
00724                                                                      CL**4
00725      IF  DCT-RETURN-CODE = '03'                                      CL**4
00726          MOVE ER-0951            TO EMI-ERROR                        CL**4
00727          MOVE -1                 TO MLINE1L                          CL**4
00728          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00729          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00730              THRU 2250-EXIT                                          CL**4
00731          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00732          GO TO 8200-SEND-DATAONLY.                                   CL**4
00733                                                                      CL**4
00734      IF  DCT-RETURN-CODE = '06'                                      CL**4
00735          MOVE ER-0921            TO EMI-ERROR                        CL**4
00736          MOVE -1                 TO MLINE1L                          CL**4
00737          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00738          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00739              THRU 2250-EXIT                                          CL**4
00740          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00741          GO TO 8200-SEND-DATAONLY.                                   CL**5
00742                                                                      CL**5
00743      IF  DCT-RETURN-CODE = '07'                                      CL**5
00744          MOVE ER-0919            TO EMI-ERROR                        CL**5
00745          MOVE -1                 TO MLINE1L                          CL**5
00746          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00747          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00748              THRU 2250-EXIT                                          CL**5
00749          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00750          GO TO 8200-SEND-DATAONLY.                                   CL**4
00751                                                                      CL**4
00752      IF  DCT-RETURN-CODE = '04'                                      CL**4
00753          MOVE ER-0946            TO EMI-ERROR                        CL**4
00754          MOVE -1                 TO MLINE1L                          CL**4
00755          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00756          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00757              THRU 2250-EXIT                                          CL**4
00758          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00759          GO TO 8200-SEND-DATAONLY.                                   CL**4
00760                                                                      CL**4
00761      IF  DCT-RETURN-CODE = '05'                                      CL**4
00762          MOVE ER-0947            TO EMI-ERROR                        CL**4
00763          MOVE -1                 TO MLINE1L                          CL**4
00764          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00765          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00766              THRU 2250-EXIT                                          CL**4
00767          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00768          GO TO 8200-SEND-DATAONLY.                                   CL**4
00769                                                                      CL**4
00770      IF  DCT-RETURN-CODE = '08'                                      CL**4
00771          MOVE ER-0948            TO EMI-ERROR                        CL**4
00772          MOVE -1                 TO MLINE1L                          CL**4
00773          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00774          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00775              THRU 2250-EXIT                                          CL**4
00776          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00777          GO TO 8200-SEND-DATAONLY.                                   CL**4
00778                                                                      CL**4
00779      IF  DCT-RETURN-CODE = 'N1'                                      CL**4
00780          MOVE ER-0950            TO EMI-ERROR                        CL**4
00781          MOVE -1                 TO MLINE1L                          CL**4
00782          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00783          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00784              THRU 2250-EXIT                                          CL**4
00785          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00786          GO TO 8200-SEND-DATAONLY.                                   CL**4
00787                                                                      CL**4
00788      IF DCT-RETURN-CODE = 'E1'                                       CL**9
00789          MOVE ER-0974            TO EMI-ERROR                        CL**9
00790          MOVE -1                 TO MLINE1L                          CL**9
00791          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**9
00792          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**9
00793              THRU 2250-EXIT                                          CL**9
00794          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**9
00795          GO TO 8200-SEND-DATAONLY.                                   CL**9
00796                                                                      CL**9
00797      IF DCT-RETURN-CODE = 'E2'                                       CL**9
00798          MOVE ER-0975            TO EMI-ERROR                        CL**9
00799          MOVE -1                 TO MLINE1L                          CL**9
00800          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**9
00801          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**9
00802              THRU 2250-EXIT                                          CL**9
00803          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**9
00804          GO TO 8200-SEND-DATAONLY.                                   CL**9
00805                                                                      CL**9
00806      IF DCT-RETURN-CODE NOT = 'OK'                                   CL**9
00807          MOVE ER-0949            TO EMI-ERROR                        CL**4
00808          MOVE -1                 TO MLINE1L                          CL**4
00809          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00810          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00811              THRU 2250-EXIT                                          CL**4
00812          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00813          GO TO 8200-SEND-DATAONLY.                                   CL**4
00814                                                                      CL**5
00815  2000-CONT.                                                          CL**5
00816                                                                      CL**4
00817      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.          CL**4
00818      MOVE 'CS'                   TO DM-RECORD-TYPE.                  CL**4
00819      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.                    CL**4
00820      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.                    CL**4
00821      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.                     CL**4
00822      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.                   CL*12
00823      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.           CL**5
00824                                                                      CL**4
00825      MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1.                  CL*12
00826      MOVE ' '                    TO  DC-OPTION-CODE.                 CL**4
00827      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.                CL**4
00828      MOVE DC-GREG-DATE-1-YMD     TO  DMD-YYMMDD.                     CL**4
00829                                                                      CL**4
00830      IF DMD-YY GREATER THAN 70                                       CL**4
00831          MOVE '19'               TO  DMD-DECADE                      CL**4
00832      ELSE                                                            CL**4
00833          MOVE '20'               TO  DMD-DECADE.                     CL**4
00834                                                                      CL**4
00835      MOVE DMD-DATE-YYYYMMDD      TO  DM-STATUS-DATE.                 CL**4
00836      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.                     CL**4
00837      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.                    CL**4
00838      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.                   CL**4
00839      PERFORM 2100-FORMAT-LAST-NAME-1ST THRU 2100-EXIT.               CL**4
00840      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.                 CL**4
00841                                                                      CL**4
00842      MOVE 'D'                    TO DM-STAT-CHANGE-TYPE.             CL**5
00843      MOVE CL-CARRIER             TO DM-STAT-CARRIER.                 CL*12
00844      MOVE '3'                    TO DM-CLAIM-STATUS.                 CL**5
00845                                                                      CL**4
00846      EXEC CICS LINK                                                  CL**5
00847          PROGRAM    ('DLO025')                                       CL**5
00848          COMMAREA   (DMO-COMMUNICATION-AREA)                         CL**5
00849          LENGTH     (WS-DMO-LENGTH)                                  CL**5
00850      END-EXEC.                                                       CL**5
00851                                                                      CL**4
00852      IF  DM-RETURN-CODE = 'OK'                                       CL**5
00853          GO TO 2000-EXIT.                                            CL**5
00854                                                                      CL**5
00855      IF  DM-RETURN-CODE = '01'                                       CL**5
00856          MOVE ER-8051            TO EMI-ERROR                        CL**5
00857          MOVE -1                 TO MLINE1L                          CL**4
00858          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00859          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**4
00860              THRU 2250-EXIT                                          CL**4
00861          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00862          GO TO 8200-SEND-DATAONLY.                                   CL**4
00863                                                                      CL**4
00864      IF  DM-RETURN-CODE = '02'                                       CL**5
00865          MOVE ER-8052            TO EMI-ERROR                        CL**5
00866          MOVE -1                 TO MLINE1L                          CL**5
00867          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00868          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00869              THRU 2250-EXIT                                          CL**5
00870          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00871          GO TO 8200-SEND-DATAONLY.                                   CL**5
00872                                                                      CL**5
00873      IF  DM-RETURN-CODE = '03'                                       CL**5
00874          MOVE ER-8053            TO EMI-ERROR                        CL**5
00875          MOVE -1                 TO MLINE1L                          CL**5
00876          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00877          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00878              THRU 2250-EXIT                                          CL**5
00879          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00880          GO TO 8200-SEND-DATAONLY.                                   CL**5
00881                                                                      CL**5
00882      IF  DM-RETURN-CODE = '04'                                       CL**5
00883          MOVE ER-8054            TO EMI-ERROR                        CL**5
00884          MOVE -1                 TO MLINE1L                          CL**5
00885          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00886          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00887              THRU 2250-EXIT                                          CL**5
00888          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00889          GO TO 8200-SEND-DATAONLY.                                   CL**5
00890                                                                      CL**5
00891      IF  DM-RETURN-CODE = '05'                                       CL**5
00892          MOVE ER-8055            TO EMI-ERROR                        CL**5
00893          MOVE -1                 TO MLINE1L                          CL**5
00894          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00895          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00896              THRU 2250-EXIT                                          CL**5
00897          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00898          GO TO 8200-SEND-DATAONLY.                                   CL**5
00899                                                                      CL**5
00900      IF  DM-RETURN-CODE = '06'                                       CL**5
00901          MOVE ER-8056            TO EMI-ERROR                        CL**5
00902          MOVE -1                 TO MLINE1L                          CL**5
00903          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00904          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00905              THRU 2250-EXIT                                          CL**5
00906          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00907          GO TO 8200-SEND-DATAONLY.                                   CL**5
00908                                                                      CL**5
00909      IF  DM-RETURN-CODE = '07'                                       CL**5
00910          MOVE ER-8057            TO EMI-ERROR                        CL**5
00911          MOVE -1                 TO MLINE1L                          CL**5
00912          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00913          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00914              THRU 2250-EXIT                                          CL**5
00915          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00916          GO TO 8200-SEND-DATAONLY.                                   CL**5
00917                                                                      CL**5
00918      IF  DM-RETURN-CODE = '08'                                       CL**5
00919          MOVE ER-8058            TO EMI-ERROR                        CL**5
00920          MOVE -1                 TO MLINE1L                          CL**5
00921          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00922          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00923              THRU 2250-EXIT                                          CL**5
00924          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00925          GO TO 8200-SEND-DATAONLY.                                   CL**5
00926                                                                      CL**5
00927      IF  DM-RETURN-CODE = '09'                                       CL**5
00928          MOVE ER-8059            TO EMI-ERROR                        CL**5
00929          MOVE -1                 TO MLINE1L                          CL**5
00930          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00931          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00932              THRU 2250-EXIT                                          CL**5
00933          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00934          GO TO 8200-SEND-DATAONLY.                                   CL**5
00935                                                                      CL**5
00936      IF  DM-RETURN-CODE = '10'                                       CL**5
00937          MOVE ER-8060            TO EMI-ERROR                        CL**5
00938          MOVE -1                 TO MLINE1L                          CL**5
00939          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00940          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00941              THRU 2250-EXIT                                          CL**5
00942          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00943          GO TO 8200-SEND-DATAONLY.                                   CL**5
00944                                                                      CL**5
00945      IF  DM-RETURN-CODE = '11'                                       CL**5
00946          MOVE ER-8061            TO EMI-ERROR                        CL**5
00947          MOVE -1                 TO MLINE1L                          CL**5
00948          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00949          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00950              THRU 2250-EXIT                                          CL**5
00951          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00952          GO TO 8200-SEND-DATAONLY.                                   CL**5
00953                                                                      CL**5
00954      IF  DM-RETURN-CODE = '12'                                       CL**5
00955          MOVE ER-8062            TO EMI-ERROR                        CL**5
00956          MOVE -1                 TO MLINE1L                          CL**5
00957          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00958          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00959              THRU 2250-EXIT                                          CL**5
00960          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00961          GO TO 8200-SEND-DATAONLY.                                   CL**5
00962                                                                      CL**5
00963      IF  DM-RETURN-CODE = '13'                                       CL**5
00964          MOVE ER-8063            TO EMI-ERROR                        CL**5
00965          MOVE -1                 TO MLINE1L                          CL**5
00966          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00967          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00968              THRU 2250-EXIT                                          CL**5
00969          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00970          GO TO 8200-SEND-DATAONLY.                                   CL**5
00971                                                                      CL**5
00972      IF  DM-RETURN-CODE = '14'                                       CL**5
00973          MOVE ER-8064            TO EMI-ERROR                        CL**5
00974          MOVE -1                 TO MLINE1L                          CL**5
00975          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00976          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
00977              THRU 2250-EXIT                                          CL**5
00978          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
00979          GO TO 8200-SEND-DATAONLY.                                   CL**5
00980                                                                      CL**5
00981      IF  DM-RETURN-CODE = '15'                                       CL**5
00982          MOVE ER-8065            TO EMI-ERROR                        CL**5
00983          MOVE -1                 TO MLINE1L                          CL**8
00984          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**8
00985          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**8
00986              THRU 2250-EXIT                                          CL**8
00987          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**8
00988          GO TO 8200-SEND-DATAONLY.                                   CL*12
00989                                                                      CL*12
00990      IF DM-RETURN-CODE = '16'                                        CL*12
00991          MOVE ER-8154            TO EMI-ERROR                        CL*12
00992          MOVE -1                 TO MLINE1L                          CL*12
00993          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT               CL*12
00994          PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT               CL*12
00995          PERFORM 2255-UNLOCK-TRLR       THRU 2255-EXIT               CL*12
00996          GO TO 8200-SEND-DATAONLY.                                   CL*12
00997                                                                      CL*12
00998      IF DM-RETURN-CODE = '17'                                        CL*12
00999          MOVE ER-8155            TO EMI-ERROR                        CL*12
01000          MOVE -1                 TO MLINE1L                          CL*12
01001          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT               CL*12
01002          PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT               CL*12
01003          PERFORM 2255-UNLOCK-TRLR       THRU 2255-EXIT               CL*12
01004          GO TO 8200-SEND-DATAONLY.                                   CL**8
01005                                                                      CL**8
01006      IF DM-RETURN-CODE = 'N1'                                        CL**8
01007          MOVE ER-8152            TO EMI-ERROR                        CL**8
01008          MOVE -1                 TO MLINE1L                          CL**8
01009          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**8
01010          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**8
01011              THRU 2250-EXIT                                          CL**8
01012          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**8
01013          GO TO 8200-SEND-DATAONLY.                                   CL**8
01014                                                                      CL**8
01015      IF DM-RETURN-CODE = 'E1'                                        CL**8
01016          MOVE ER-8153            TO EMI-ERROR                        CL**8
01017          MOVE -1                 TO MLINE1L                          CL**5
01018          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
01019          PERFORM 2250-UNLOCK-CLAIM-MSTR                              CL**5
01020              THRU 2250-EXIT                                          CL**5
01021          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT                     CL**5
01022          GO TO 8200-SEND-DATAONLY.                                   CL**5
01023                                                                      CL**5
01024      MOVE ER-8066                TO EMI-ERROR.                       CL**5
01025      MOVE -1                     TO MLINE1L.                         CL**5
01026      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
01027      PERFORM 2250-UNLOCK-CLAIM-MSTR                                  CL**5
01028          THRU 2250-EXIT.                                             CL**5
01029      PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT.                        CL**5
01030      GO TO 8200-SEND-DATAONLY.                                       CL**5
01031                                                                      CL**4
01032  2000-NOTE-NOT-FOUND.                                                CL**4
01033                                                                      CL**4
01034      MOVE ER-0954                TO EMI-ERROR.                       CL**4
01035      MOVE -1                     TO MLINE1L.                         CL**4
01036      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01037      PERFORM 2250-UNLOCK-CLAIM-MSTR                                  CL**4
01038          THRU 2250-EXIT.                                             CL**4
01039      PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT.                        CL**5
01040      GO TO 8200-SEND-DATAONLY.                                       CL**4
01041                                                                      CL**4
01042  2000-EXIT.                                                          CL**4
01043      EXIT.                                                           CL**4
01044       EJECT                                                          CL**4
01045                                                                      CL**4
01046  2100-FORMAT-LAST-NAME-1ST.                                          CL**4
01047 *****************************************************************    CL**4
01048 *             M O V E   N A M E   R O U T I N E                 *    CL**4
01049 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *    CL**4
01050 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *    CL**4
01051 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *    CL**4
01052 *                  FIELD                   VALUE                *    CL**4
01053 *           W-NAME-LAST    (CL15)      SMITH                    *    CL**4
01054 *           W-NAME-FIRST   (CL15)      JOHN                     *    CL**4
01055 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *    CL**4
01056 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *    CL**4
01057 *     CONTAIN                                                   *    CL**4
01058 *                SMITH, JOHN ALLEN                              *    CL**4
01059 *     OR                                                        *    CL**4
01060 *                SMITH, JOHN A.                                 *    CL**4
01061 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *    CL**4
01062 *     COPYBOOK, ELCNWA.                                         *    CL**4
01063 *****************************************************************.   CL**4
01064                                                                      CL**4
01065      MOVE SPACES                 TO  WS-NAME-WORK-AREA.              CL**4
01066      MOVE ZERO                   TO  WS-NAME-SW.                     CL**4
01067      SET NWA-INDEX               TO +1.                              CL**4
01068                                                                      CL**4
01069      IF  W-NAME-LAST EQUAL SPACES                                    CL**4
01070              AND                                                     CL**4
01071          W-NAME-MIDDLE EQUAL SPACES                                  CL**4
01072          MOVE +1                 TO WS-NAME-SW.                      CL**4
01073                                                                      CL**4
01074      MOVE W-NAME-LAST            TO WS-NAME-WORK2.                   CL**4
01075      PERFORM 2150-MOVE-NAME THRU 2150-EXIT.                          CL**4
01076                                                                      CL**4
01077      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.                   CL**4
01078      PERFORM 2150-MOVE-NAME THRU 2150-EXIT.                          CL**4
01079                                                                      CL**4
01080      SET NWA-INDEX UP BY +1.                                         CL**4
01081                                                                      CL**4
01082      IF  W-NAME-MIDDLE NOT EQUAL SPACES                              CL**4
01083                                                                      CL**4
01084          IF  W-NAME-MIDDLE-2 EQUAL SPACES                            CL**4
01085              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)                CL**4
01086              SET NWA-INDEX UP BY +1                                  CL**4
01087              MOVE '.'            TO WS-NW (NWA-INDEX)                CL**4
01088              SET NWA-INDEX UP BY +2                                  CL**4
01089                                                                      CL**4
01090          ELSE                                                        CL**4
01091              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2                    CL**4
01092              PERFORM 2150-MOVE-NAME THRU 2150-EXIT.                  CL**4
01093                                                                      CL**4
01094  2100-EXIT.                                                          CL**4
01095      EXIT.                                                           CL**4
01096                                  EJECT                               CL**4
01097  2150-MOVE-NAME.                                                     CL**4
01098                                                                      CL**4
01099      IF  WS-NAME-SW GREATER THAN +1                                  CL**4
01100          GO TO 2150-EXIT.                                            CL**4
01101                                                                      CL**4
01102      IF  WS-NAME-WORK2 = SPACES                                      CL**4
01103          GO TO 2150-EXIT.                                            CL**4
01104                                                                      CL**4
01105      SET NWA-INDEX2            TO +1.                                CL**4
01106      SET NWA-INDEX3            TO +2.                                CL**4
01107                                                                      CL**4
01108  2150-MOVE-NAME-CYCLE.                                               CL**4
01109                                                                      CL**4
01110      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).                CL**4
01111                                                                      CL**4
01112      IF  NWA-INDEX LESS THAN +30                                     CL**4
01113          SET NWA-INDEX UP BY +1                                      CL**4
01114                                                                      CL**4
01115      ELSE                                                            CL**4
01116          ADD +2                TO  WS-NAME-SW                        CL**4
01117          GO TO 2150-EXIT.                                            CL**4
01118                                                                      CL**4
01119      IF  NWA-INDEX2 LESS THAN +20                                    CL**4
01120          SET NWA-INDEX3 UP BY +1                                     CL**4
01121          SET NWA-INDEX2 UP BY +1.                                    CL**4
01122                                                                      CL**4
01123      IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES                            CL**4
01124              AND                                                     CL**4
01125          WS-NW2 (NWA-INDEX3) EQUAL SPACES                            CL**4
01126                                                                      CL**4
01127          IF  WS-NAME-SW EQUAL ZERO                                   CL**4
01128              MOVE ','            TO  WS-NW (NWA-INDEX)               CL**4
01129              SET NWA-INDEX UP BY +2                                  CL**4
01130              MOVE +1             TO  WS-NAME-SW                      CL**4
01131              GO TO 2150-EXIT                                         CL**4
01132                                                                      CL**4
01133          ELSE                                                        CL**4
01134              GO TO 2150-EXIT.                                        CL**4
01135                                                                      CL**4
01136      GO TO 2150-MOVE-NAME-CYCLE.                                     CL**4
01137                                                                      CL**4
01138  2150-EXIT.                                                          CL**4
01139      EXIT.                                                           CL**4
01140                                  EJECT                               CL**4
01141  2250-UNLOCK-CLAIM-MSTR.                                             CL**4
01142       EXEC CICS UNLOCK                                               CL**4
01143            DATASET  (ELMSTR-FILE-ID)                                 CL**4
01144            END-EXEC.                                                 CL**4
01145  2250-EXIT.                                                          CL**5
01146      EXIT.                                                           CL**4
01147  2255-UNLOCK-TRLR.                                                   CL**5
01148       EXEC CICS UNLOCK                                               CL**4
01149            DATASET (ELTRLR-FILE-ID)                                  CL**4
01150            END-EXEC.                                                 CL**4
01151  2255-EXIT.                                                          CL**5
01152      EXIT.                                                           CL**4
CIDMOD/                                                                    CL**4
CIDMOD*************************************************************     ****.
CIDMOD*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***     **   000
CIDMOD*************************************************************     ****.
CIDMOD 2300-GETMAIN-DLYACTV.                                                 000
CIDMOD     EXEC CICS GETMAIN                                                 000
CIDMOD         SET (ADDRESS OF DAILY-ACTIVITY-RECORD)                        000
CIDMOD         LENGTH (25)                                                   000
CIDMOD         INITIMG (WS-BLANK)                                            000
CIDMOD     END-EXEC.                                                         000
CIDMOD 2300-EXIT. EXIT.
CIDMOD/                                                                    CL**4
CIDMOD*************************************************************     **   000
CIDMOD*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***     **   000
CIDMOD*************************************************************     **   000
CIDMOD 2350-WRITE-DLYACTV.                                                   000
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD         NOTOPEN (9986-NOTOPEN-DLYACTV)                                000
CIDMOD         DUPREC (2350-EXIT)                                            000
CIDMOD     END-EXEC.
CIDMOD
CIDMOD     EXEC CICS WRITE                                                   000
CIDMOD         DATASET ('DLYACTV')                                           000
CIDMOD         RIDFLD (DA-KEY)                                               000
CIDMOD         FROM (DAILY-ACTIVITY-RECORD)                                  000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD 2350-EXIT.                                                            000
CIDMOD     EXIT.
CIDMOD                                                                       000
CIDMOD/                                                                      000
CIDMOD*************************************************************     **   000
CIDMOD*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***     **   000
CIDMOD*************************************************************     **   000
CIDMOD                                                                       000
CIDMOD 3500-BUILD-ACTIVITY-RECORD.                                           000
CIDMOD                                                                       000
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.             000
CIDMOD     MOVE WS-CLAIM-KEY           TO DA-KEY.                            000
CIDMOD     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT                                000
CIDMOD                                 GIVING DA-TRAILER-SEQ-NO.             000
CIDMOD     MOVE 'D'                    TO DA-RECORD-TYPE.                    000
CIDMOD                                                                       000
CIDMOD 3500-EXIT.                                                            000
CIDMOD     EXIT.                                                             000
CIDMOD/                                                                      000
050506 3550-FIND-LETTER-TRLR.
050506      MOVE PI-COMPANY-CD             TO  WS-KEY-COMPANY-CD.   
050506      MOVE PI-CARRIER                TO  WS-KEY-CARRIER.      
050506      MOVE PI-CLAIM-NO               TO  WS-KEY-CLAIM-NO.     
050506      MOVE PI-CERT-NO                TO  WS-KEY-CERT-NO.      
050506      MOVE +100                      TO  WS-KEY-SEQUENCE-NO.  
050506                                                              
050506 3550-STARTBR-ELTRLR.                                         
050506                                                              
050506      EXEC CICS HANDLE CONDITION                              
050506          ENDFILE   (3550-END)
050506          NOTFND    (3550-END)
050506      END-EXEC.               
050506                              
050506      EXEC CICS STARTBR       
050506          DATASET   (ELTRLR-FILE-ID)  
050506          RIDFLD    (WS-TRAILER-KEY)  
050506          GTEQ                        
050506      END-EXEC.                       
050506
050506      MOVE LOW-VALUES TO WS-MAX-LETTER-ANSWER-DT.
050506                                      
050506 3550-READNEXT-ELTRLR.   
050506                         
050506      EXEC CICS READNEXT 
050506          DATASET   (ELTRLR-FILE-ID)   
050506          RIDFLD    (WS-TRAILER-KEY)   
050506          SET       (ADDRESS OF ACTIVITY-TRAILERS)  
050506      END-EXEC.                                     
050506                                                    
050506      IF (PI-COMPANY-CD IS NOT EQUAL TO  WS-KEY-COMPANY-CD) OR 
050506         (PI-CARRIER    IS NOT EQUAL TO  WS-KEY-CARRIER)    OR 
050506         (PI-CLAIM-NO   IS NOT EQUAL TO  WS-KEY-CLAIM-NO)   OR 
050506         (PI-CERT-NO    IS NOT EQUAL TO  WS-KEY-CERT-NO)       
050506          GO TO 3550-END.                                
050506                   
050506      IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'           
050506          GO TO 3550-READNEXT-ELTRLR.
050506       
050506     IF AT-LETTER-ANSWERED-DT GREATER THAN WS-MAX-LETTER-ANSWER-DT
050506         MOVE AT-LETTER-ANSWERED-DT TO WS-MAX-LETTER-ANSWER-DT
050506     END-IF.
050506          
050506     GO TO 3550-READNEXT-ELTRLR.  
050506                                  
050506 3550-END.
050506
050506     IF WS-MAX-LETTER-ANSWER-DT NOT EQUAL LOW-VALUES AND SPACES
050506         MOVE SPACES             TO  DC-OPTION-CODE               
050506         MOVE WS-MAX-LETTER-ANSWER-DT TO  DC-BIN-DATE-1             
050506         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                            
050506         MOVE DC-GREG-DATE-1-MDY TO PI-PROOF-DATE
050506         MOVE PI-PROOF-DATE TO  MPRFDTO         
050506         INSPECT MPRFDTI REPLACING ALL ' ' BY '/'                  
050506     END-IF.                              
050506                     
050506      EXEC CICS ENDBR
050506          DATASET   (ELTRLR-FILE-ID)                        
050506      END-EXEC.   
050506 
050506 3550-EXIT.                
050506     EXIT.                 
050506/
01154  6000-RESET-AUTO-ACTIVITY.                                           CL**3
01155 ******************************************************************   CL**3
01156 *    RESET ALL FUTURE LETTER ACTIVITY AWAITING FURTHER ACTION    *   CL**3
01157 *    (RESEND, FOLLOW-UP, ETC) BY MOVING LOW-VALUES TO THE        *   CL**3
01158 *    FOLLOW-UP OR RESEND DATES IN THE ACTIVITY TRAILER AND THE   *   CL**3
01159 *    RESEND DATE IS THE LETTER ARCHIVE, IF NECESSARY.            *   CL**3
01160 ******************************************************************   CL**3
01161       MOVE PI-COMPANY-CD             TO  WS-KEY-COMPANY-CD.          CL**3
01162       MOVE PI-CARRIER                TO  WS-KEY-CARRIER.             CL**3
01163       MOVE PI-CLAIM-NO               TO  WS-KEY-CLAIM-NO.            CL**3
01164       MOVE PI-CERT-NO                TO  WS-KEY-CERT-NO.             CL**3
01165       MOVE +100                      TO  WS-KEY-SEQUENCE-NO.         CL**3
01166                                                                   EL151
01167  6000-STARTBR-ELTRLR.                                                CL**3
01168                                                                   EL151
01169       EXEC CICS HANDLE CONDITION                                     CL**3
01170           ENDFILE   (6000-END-RESET)                                 CL**3
01171           NOTFND    (6000-END-RESET)                                 CL**3
01172       END-EXEC.                                                      CL**3
01173                                                                   EL151
01174       EXEC CICS STARTBR                                              CL**3
01175           DATASET   (ELTRLR-FILE-ID)                                 CL**3
01176           RIDFLD    (WS-TRAILER-KEY)                                 CL**3
01177           GTEQ                                                       CL**3
01178       END-EXEC.                                                      CL**3
01179                                                                   EL151
01180       MOVE 'Y'                       TO  WS-BROWSE-SW.               CL**3
01181                                                                   EL151
01182  6000-READNEXT-ELTRLR.                                               CL**3
01183                                                                   EL151
01184       EXEC CICS READNEXT                                             CL**3
01185           DATASET   (ELTRLR-FILE-ID)                                 CL**3
01186           RIDFLD    (WS-TRAILER-KEY)                                 CL**3
01187           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL**4
01188       END-EXEC.                                                      CL**3
01189                                                                   EL151
01190       IF (PI-COMPANY-CD IS NOT EQUAL TO  WS-KEY-COMPANY-CD) OR       CL**3
01191          (PI-CARRIER    IS NOT EQUAL TO  WS-KEY-CARRIER)    OR       CL**3
01192          (PI-CLAIM-NO   IS NOT EQUAL TO  WS-KEY-CLAIM-NO)   OR       CL**3
01193          (PI-CERT-NO    IS NOT EQUAL TO  WS-KEY-CERT-NO)             CL**3
01194           GO TO 6000-END-RESET.                                      CL**3
01195                                                                   EL151
01196       IF WS-TRAILER-KEY IS EQUAL TO PI-PREV-TRLR-KEY                 CL**3
01197           GO TO 6000-READNEXT-ELTRLR.                                CL**3
01198                                                                   EL151
01199       IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'                         CL**3
01200           GO TO 6000-READNEXT-ELTRLR.                                CL**3
01201                                                                   EL151
01202       IF (AT-LETTER-ANSWERED-DT NOT EQUAL LOW-VALUES AND SPACES)     CL**3
01203           GO TO 6000-READNEXT-ELTRLR.                                CL**3
01204                                                                   EL151
01205       IF (AT-AUTO-RE-SEND-DT EQUAL LOW-VALUES OR SPACES)             CL**3
01206         AND                                                          CL**3
01207          (AT-RECEIPT-FOLLOW-UP EQUAL LOW-VALUES OR SPACES)           CL**3
01208           GO TO 6000-READNEXT-ELTRLR.                                CL**3
01209                                                                   EL151
01210       IF AT-RECEIPT-FOLLOW-UP IS LESS THAN SAVE-BIN-DATE AND         CL**3
01211          AT-AUTO-RE-SEND-DT IS LESS THAN SAVE-BIN-DATE   AND         CL**3
01212          AT-RESEND-PRINT-DATE IS LESS THAN SAVE-BIN-DATE             CL**3
01213           GO TO 6000-READNEXT-ELTRLR.                                CL**3
01214                                                                   EL151
01215       IF (AT-AUTO-RE-SEND-DT NOT EQUAL LOW-VALUES AND SPACES)        CL**3
01216         AND                                                          CL**3
01217          (AT-RESEND-PRINT-DATE NOT EQUAL LOW-VALUES AND SPACES)      CL**3
01218         AND                                                          CL**3
01219          (AT-RECEIPT-FOLLOW-UP EQUAL LOW-VALUES)                     CL**3
01220           GO TO 6000-READNEXT-ELTRLR.                                CL**3
01221                                                                   EL151
01222  6000-END-BROWSE.                                                    CL**3
01223                                                                   EL151
01224       MOVE WS-TRAILER-KEY            TO  PI-PREV-TRLR-KEY.           CL**3
01225                                                                   EL151
01226       EXEC CICS ENDBR                                                CL**3
01227           DATASET   (ELTRLR-FILE-ID)                                 CL**3
01228       END-EXEC.                                                      CL**3
01229                                                                   EL151
01230       MOVE 'N'                       TO  WS-BROWSE-SW.               CL**3
01231                                                                   EL151
01232  6000-READ-ELTRLR-UPDATE.                                            CL**3
01233                                                                   EL151
01234       EXEC CICS READ                                                 CL**3
01235           DATASET   (ELTRLR-FILE-ID)                                 CL**3
01236           RIDFLD    (WS-TRAILER-KEY)                                 CL**3
01237           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL**4
01238           UPDATE                                                     CL**3
01239       END-EXEC.                                                      CL**3
01240                                                                   EL151
01241       MOVE 'Y'                       TO  WS-UPDATE-SW.               CL**3
01242                                                                   EL151
01243       IF AT-AUTO-RE-SEND-DT IS NOT LESS THAN SAVE-BIN-DATE           CL**3
01244           MOVE LOW-VALUES            TO  AT-AUTO-RE-SEND-DT          CL**3
01245           MOVE PI-COMPANY-CD         TO  ELARCH-COMPANY-CD           CL**3
01246           MOVE AT-LETTER-ARCHIVE-NO  TO  ELARCH-ARCHIVE-NO           CL**3
01247           MOVE '1'                   TO  ELARCH-RECORD-TYPE          CL**3
01248           MOVE +0                    TO  ELARCH-SEQ-NO               CL**3
01249           PERFORM 6200-READ-ELARCH-UPDATE THRU 6200-EXIT.            CL**3
01250                                                                   EL151
01251       IF AT-RECEIPT-FOLLOW-UP IS NOT LESS THAN SAVE-BIN-DATE         CL**3
01252           MOVE LOW-VALUES            TO  AT-RECEIPT-FOLLOW-UP.       CL**3
01253                                                                   EL151
01254  6000-REWRITE-ELTRLR.                                                CL**3
01255                                                                   EL151
01256       EXEC CICS REWRITE                                              CL**3
01257           DATASET   (ELTRLR-FILE-ID)                                 CL**3
01258           FROM      (ACTIVITY-TRAILERS)                              CL**3
01259       END-EXEC.                                                      CL**3
01260                                                                   EL151
01261       GO TO 6000-STARTBR-ELTRLR.                                     CL**3
01262                                                                      CL**3
01263  6000-END-RESET.                                                     CL**3
01264                                                                      CL**3
01265       IF WS-BROWSE-SW IS EQUAL TO 'Y'                                CL**3
01266           MOVE 'N'                   TO  WS-BROWSE-SW                CL**3
01267           EXEC CICS ENDBR                                            CL**3
01268               DATASET   (ELTRLR-FILE-ID)                             CL**3
01269           END-EXEC.                                                  CL**3
01270                                                                      CL**3
01271  6000-EXIT.                                                          CL**3
01272      EXIT.                                                           CL**3
01273                                                                      CL**3
01274       EJECT                                                          CL**3
01275  6100-CHECK-AUTO-ACTIVITY.                                           CL**3
01276 ******************************************************************   CL**3
01277 *    READ THE AUTOMATIC ACTIVITY RECORD TO DETERMINE IF THE      *   CL**3
01278 *    AUTOMATIC ACTIVITY CODE IN THE CLAIM MASTER IS TO BE RESET. *   CL**3
01279 ******************************************************************   CL**3
01280                                                                      CL**3
01281       EXEC CICS HANDLE CONDITION                                     CL**3
01282           NOTFND   (6100-NOT-FOUND)                                  CL**3
01283       END-EXEC.                                                      CL**3
01284                                                                      CL**3
01285       MOVE PI-COMPANY-ID             TO  ELCNTL-COMPANY-ID.          CL**3
01286       MOVE 'T'                       TO  ELCNTL-RECORD-TYPE.         CL**3
01287       MOVE SPACES                    TO  ELCNTL-ACCESS.              CL**3
01288       MOVE +0                        TO  ELCNTL-SEQ-NO.              CL**3
01289                                                                      CL**3
01290       EXEC CICS READ                                                 CL**3
01291           DATASET   (ELCNTL-FILE-ID)                                 CL**3
01292           RIDFLD    (ELCNTL-KEY)                                     CL**3
01293           SET       (ADDRESS OF CONTROL-FILE)                        CL**4
01294       END-EXEC.                                                      CL**3
01295                                                                      CL**3
01296       IF CL-ACTIVITY-CODE IS NOT EQUAL TO ZEROS                      CL**3
01297           MOVE CL-ACTIVITY-CODE      TO  MISC-SUB                    CL**3
01298           IF MISC-SUB IS GREATER THAN +9                             CL**3
01299               SUBTRACT +9 FROM MISC-SUB                              CL**3
01300               MOVE CF-USER-RESET-SW (MISC-SUB) TO  WS-RESET-SW       CL**3
01301           ELSE                                                       CL**3
01302               MOVE CF-SYS-RESET-SW  (MISC-SUB) TO  WS-RESET-SW.      CL**3
01303                                                                      CL**3
01304       MOVE 'Y'                       TO  WS-REC-FOUND-SW.            CL**3
01305       GO TO 6100-EXIT.                                               CL**3
01306                                                                      CL**3
01307  6100-NOT-FOUND.                                                     CL**3
01308                                                                      CL**3
01309      MOVE 'N'                        TO  WS-RESET-SW                 CL**3
01310                                          WS-REC-FOUND-SW.            CL**3
01311                                                                      CL**3
01312  6100-EXIT.                                                          CL**3
01313      EXIT.                                                           CL**3
01314                                                                      CL**3
01315       EJECT                                                          CL**3
01316  6200-READ-ELARCH-UPDATE.                                            CL**3
01317 ******************************************************************   CL**3
01318 *  READ AND UPDATE THE RESEND DATE IN THE LETTER ARCHIVE RECORD  *   CL**3
01319 ******************************************************************   CL**3
01320                                                                      CL**3
01321       EXEC CICS HANDLE CONDITION                                     CL**3
01322           NOTFND   (6200-EXIT)                                       CL**3
01323       END-EXEC.                                                      CL**3
01324                                                                      CL**3
01325       EXEC CICS READ                                                 CL**3
01326           DATASET   (ELARCH-FILE-ID)                                 CL**3
01327           RIDFLD    (ELARCH-KEY)                                     CL**3
01328           SET       (ADDRESS OF LETTER-ARCHIVE)                      CL**4
01329           UPDATE                                                     CL**3
01330       END-EXEC.                                                      CL**3
01331                                                                      CL**3
01332       MOVE LOW-VALUES                TO  LA-RESEND-DATE.             CL**3
01333                                                                      CL**3
01334  6200-REWRITE-ELARCH.                                                CL**3
01335                                                                      CL**3
01336       EXEC CICS REWRITE                                              CL**3
01337           DATASET   (ELARCH-FILE-ID)                                 CL**3
01338           FROM      (LETTER-ARCHIVE)                                 CL**3
01339       END-EXEC.                                                      CL**3
01340                                                                      CL**3
01341  6200-EXIT.                                                          CL**3
01342      EXIT.                                                           CL**3
01343                                                                      CL**3
01344      EJECT                                                           CL**3
01345  7000-READ-ELMSTR-UPDATE.                                            CL**3
01346                                                                      CL**3
01347      EXEC CICS  HANDLE CONDITION                                  EL151
01348             NOTFND  (7000-NOTFND)                                    CL**3
01349             NOTOPEN (9981-NOTOPEN-MSTR)                           EL151
01350      END-EXEC.                                                    EL151
01351                                                                   EL151
01352      EXEC CICS  READ                                              EL151
01353          DATASET  (ELMSTR-FILE-ID)                                   CL**3
01354          RIDFLD   (WS-CLAIM-KEY)                                     CL**3
01355          SET      (ADDRESS OF CLAIM-MASTER)                          CL**4
01356          UPDATE                                                      CL**3
01357      END-EXEC.                                                    EL151
01358                                                                   EL151
01359      GO TO 7000-EXIT.                                                CL**3
01360                                                                   EL151
01361  7000-NOTFND.                                                        CL**3
01362                                                                   EL151
01363      MOVE ER-0133                TO  EMI-ERROR.                      CL**3
01364      MOVE -1                     TO  MLINE1L.                        CL**3
01365      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
01366      GO TO 8200-SEND-DATAONLY.                                       CL**3
01367                                                                      CL**3
01368  7000-EXIT.                                                          CL**3
01369       EXIT.                                                       EL151
01370                                                                   EL151
01371  7010-REWRITE-ELMSTR.                                                CL**3
01372                                                                      CL**3
01373      EXEC CICS HANDLE CONDITION                                   EL151
01374          DUPKEY (7010-EXIT)                                          CL**3
01375          END-EXEC.                                                EL151
01376                                                                   EL151
01377      EXEC CICS  REWRITE                                           EL151
01378          DATASET  (ELMSTR-FILE-ID)                                   CL**3
01379          FROM     (CLAIM-MASTER)                                     CL**3
01380      END-EXEC.                                                    EL151
01381                                                                   EL151
01382  7010-EXIT.                                                          CL**3
01383       EXIT.                                                       EL151
01384                                                                   EL151
01385      EJECT                                                           CL**3
01386  7100-GETMAIN-ELTRLR.                                                CL**3
01387                                                                   EL151
01388      EXEC CICS GETMAIN                                               CL**3
01389          SET     (ADDRESS OF ACTIVITY-TRAILERS)                      CL**4
01390          INITIMG (GETMAIN-SPACE)                                     CL**3
01391          LENGTH  (WS-TRLR-LENGTH)                                    CL**3
01392      END-EXEC.                                                    EL151
01393                                                                   EL151
01394  7100-EXIT.                                                          CL**3
01395      EXIT.                                                           CL**3
01396                                                                      CL**3
01397  7110-WRITE-ELTRLR.                                                  CL**3
01398      EXEC CICS  HANDLE CONDITION                                     CL**3
01399             DUPREC   (7110-DUPREC)                                   CL**3
01400      END-EXEC.                                                       CL**3
01401                                                                      CL**3
01402      EXEC CICS  WRITE                                                CL**3
01403             DATASET  (ELTRLR-FILE-ID)                                CL**3
01404             RIDFLD   (WS-TRAILER-KEY)                                CL**3
01405             FROM     (ACTIVITY-TRAILERS)                             CL**3
01406      END-EXEC.                                                       CL**3
01407                                                                      CL**3
01408      GO TO 7110-EXIT.                                                CL**3
01409                                                                      CL**3
01410  7110-DUPREC.                                                        CL**3
01411                                                                      CL**3
01412      MOVE ER-0132                TO  EMI-ERROR.                      CL**3
01413      MOVE -1                     TO  MLINE1L.                        CL**3
01414      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
01415      GO TO 8200-SEND-DATAONLY.                                       CL**3
01416                                                                      CL**3
01417  7110-EXIT.                                                          CL**3
01418       EXIT.                                                       EL151
01419                                                                   EL151
01420  7120-READ-ELTRLR-UPDATE.                                            CL**3
01421                                                                      CL**3
01422      EXEC CICS  HANDLE CONDITION                                  EL151
01423             NOTFND  (9965-NO-TRLR-ERROR)                          EL151
01424             NOTOPEN (9982-NOTOPEN-TRLR)                           EL151
01425      END-EXEC.                                                    EL151
01426                                                                   EL151
01427      EXEC CICS  READ                                              EL151
01428          DATASET  (ELTRLR-FILE-ID)                                   CL**3
01429          RIDFLD   (WS-TRAILER-KEY)                                   CL**3
01430          SET      (ADDRESS OF ACTIVITY-TRAILERS)                     CL**4
01431          UPDATE                                                      CL**3
01432      END-EXEC.                                                    EL151
01433                                                                   EL151
01434  7120-EXIT.                                                          CL**3
01435       EXIT.                                                       EL151
01436                                                                   EL151
01437  7130-REWRITE-ELTRLR.                                                CL**3
01438                                                                      CL**3
01439      EXEC CICS  REWRITE                                           EL151
01440          DATASET  (ELTRLR-FILE-ID)                                   CL**3
01441          FROM     (ACTIVITY-TRAILERS)                                CL**3
01442      END-EXEC.                                                       CL**3
01443                                                                      CL**3
01444  7130-EXIT.                                                          CL**3
01445       EXIT.                                                          CL**3
01446                                                                      CL**3
01447      EJECT                                                           CL**3
01448  8100-SEND-INITIAL-MAP.                                              CL**3
01449                                                                      CL**3
01450      MOVE SAVE-DATE                  TO  MRNDATEO.                   CL**3
01451      MOVE EIBTIME                    TO  TIME-IN.                    CL**3
01452      MOVE TIME-OUT                   TO  MRNTIMEO.                   CL**3
01453                                                                      CL**3
01454      EXEC CICS  SEND                                                 CL**3
01455           MAP      (MAP-NAME)                                        CL**3
01456           MAPSET   (MAPSET-NAME)                                     CL**3
01457           FROM     (EL151AO)                                         CL**3
01458           ERASE                                                      CL**3
01459           CURSOR                                                     CL**3
01460      END-EXEC.                                                       CL**3
01461                                                                      CL**3
01462      GO TO 9100-RETURN-TRAN.                                         CL**3
01463                                                                      CL**3
01464  8200-SEND-DATAONLY.                                                 CL**3
01465                                                                      CL**3
01466      MOVE SAVE-DATE                  TO  MRNDATEO.                   CL**3
01467      MOVE EIBTIME                    TO  TIME-IN.                    CL**3
01468      MOVE TIME-OUT                   TO  MRNTIMEO.                   CL**3
01469                                                                      CL**3
01470      EXEC CICS  SEND                                                 CL**3
01471          MAP       (MAP-NAME)                                        CL**3
01472          MAPSET    (MAPSET-NAME)                                     CL**3
01473          FROM      (EL151AO)                                         CL**3
01474          DATAONLY                                                    CL**3
01475          CURSOR                                                      CL**3
01476      END-EXEC.                                                       CL**3
01477                                                                      CL**3
01478      GO TO 9100-RETURN-TRAN.                                         CL**3
01479                                                                      CL**3
01480  8300-SEND-TEXT.                                                     CL**3
01481      EXEC CICS SEND TEXT                                             CL**3
01482          FROM     (LOGOFF-TEXT)                                      CL**3
01483          LENGTH   (LOGOFF-LENGTH)                                    CL**3
01484          ERASE                                                       CL**3
01485          FREEKB                                                      CL**3
01486      END-EXEC.                                                    EL151
01487                                                                   EL151
01488      EXEC CICS RETURN                                                CL**3
01489          END-EXEC.                                                   CL**3
01490                                                                   EL151
01491      EJECT                                                        EL151
01492  9600-PGMID-ERROR.                                                   CL**3
01493      EXEC CICS HANDLE CONDITION                                      CL**3
01494          PGMIDERR (8300-SEND-TEXT)                                   CL**3
01495      END-EXEC.                                                    EL151
01496                                                                   EL151
01497      MOVE THIS-PGM               TO LOGOFF-PGM                    EL151
01498                                     PI-CALLING-PROGRAM.           EL151
01499      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL151
01500      MOVE 'EL005'                TO THIS-PGM.                     EL151
01501      MOVE THIS-PGM               TO LOGOFF-PGM.                      CL**3
01502      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL151
01503      GO TO 9300-XCTL.                                                CL**3
01504                                                                   EL151
01505  8800-UNAUTHORIZED-ACCESS.                                           CL**3
01506                                                                   EL151
01507      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL151
01508      GO TO 8300-SEND-TEXT.                                           CL**3
01509                                                                   EL151
01510  9100-RETURN-TRAN.                                                   CL**3
01511      MOVE EMI-ERROR-NUMBER (1)    TO  PI-LAST-ERROR-NO.              CL**3
01512      MOVE '151A'                  TO  PI-CURRENT-SCREEN-NO.          CL**3
01513                                                                   EL151
01514      EXEC CICS  RETURN                                            EL151
01515          TRANSID  (WS-TRANS-ID)                                      CL**3
01516          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL**3
01517          LENGTH   (PI-COMM-LENGTH)                                   CL**3
01518      END-EXEC.                                                    EL151
01519                                                                   EL151
01520  9300-XCTL.                                                          CL**3
01521      EXEC CICS XCTL                                               EL151
01522          PROGRAM  (THIS-PGM)                                         CL**3
01523          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL**3
01524          LENGTH   (PI-COMM-LENGTH)                                   CL**3
01525      END-EXEC.                                                    EL151
01526                                                                   EL151
01527  9400-CLEAR.                                                         CL**3
01528      MOVE PI-RETURN-TO-PROGRAM       TO  THIS-PGM.                   CL**3
01529      GO TO 9300-XCTL.                                                CL**3
01530                                                                      CL**3
050506 9650-DEEDIT.                          
050506     EXEC CICS BIF DEEDIT              
050506         FIELD (DEEDIT-FIELD)          
050506         LENGTH(15)                    
050506     END-EXEC.                         
050506
050506 9650-EXIT.
050506      EXIT.
050506
01531  9700-LINK-DATE-CONVERT.                                             CL**3
01532      EXEC CICS LINK                                                  CL**3
01533             PROGRAM  ('ELDATCV')                                     CL**3
01534             COMMAREA (DATE-CONVERSION-DATA)                          CL**3
01535             LENGTH   (DC-COMM-LENGTH)                                CL**3
01536      END-EXEC.                                                       CL**3
01537                                                                      CL**3
01538  9700-EXIT.                                                          CL**3
01539       EXIT.                                                       EL151
01540                                                                   EL151
01541  9900-ERROR-FORMAT.                                                  CL**3
01542                                                                   EL151
01543      EXEC CICS LINK                                                  CL**3
01544          PROGRAM  ('EL001')                                          CL**3
01545          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                    CL**3
01546          LENGTH   (EMI-COMM-LENGTH)                                  CL**3
01547      END-EXEC.                                                       CL**3
01548                                                                   EL151
01549      MOVE EMI-LINE1              TO  MERMSG1O.                       CL**3
01550                                                                   EL151
01551  9900-EXIT.                                                       EL151
01552       EXIT.                                                       EL151
01553                                                                   EL151
01554  9965-NO-TRLR-ERROR.                                              EL151
01555      MOVE ER-0270                TO  EMI-ERROR.                      CL**3
01556      MOVE -1                     TO  MLINE1L.                        CL**3
01557      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
01558      GO TO 8200-SEND-DATAONLY.                                       CL**3
01559                                                                   EL151
01560  9981-NOTOPEN-MSTR.                                               EL151
01561      MOVE ER-0154                TO EMI-ERROR                     EL151
01562      MOVE -1                     TO  MLINE1L.                        CL**3
01563      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
01564      GO TO 8200-SEND-DATAONLY.                                       CL**3
01565                                                                   EL151
01566  9982-NOTOPEN-TRLR.                                               EL151
01567      MOVE ER-0172                TO EMI-ERROR                     EL151
01568      MOVE -1                     TO  MLINE1L.                        CL**3
01569      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
01570      GO TO 8200-SEND-DATAONLY.                                       CL**3
01571                                                                   EL151
CIDMOD 9986-NOTOPEN-DLYACTV.                                                 000
CIDMOD     MOVE '2955'                 TO EMI-ERROR.                         000
CIDMOD     MOVE -1                     TO  MLINE1L.                          000
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                         000
CIDMOD     GO TO 8200-SEND-DATAONLY.                                         000
CIDMOD                                                                       000
01572  9995-SECURITY-VIOLATION.                                         EL151
01573                              COPY ELCSCTP.                        EL151
01574                                                                   EL151
01575  9995-EXIT.                                                       EL151
01576      EXIT.                                                        EL151
01577                                                                   EL151
01578  9990-ABEND.                                                         CL**3
01579      MOVE -1                      TO  MPFNUMBL.                      CL**3
01580      MOVE DFHEIBLK                TO  EMI-LINE1.                     CL**3
01581                                                                   EL151
01582      EXEC CICS LINK                                                  CL**3
01583            PROGRAM  ('EL004')                                        CL**3
01584            COMMAREA (EMI-LINE1)                                      CL**3
01585            LENGTH   (72)                                             CL**3
01586      END-EXEC.                                                       CL**3
01587                                                                   EL151
01588      MOVE EMI-LINE1               TO  MERMSG1O.                      CL**3
01589      GO TO 8200-SEND-DATAONLY.                                       CL**3
01590                                                                   EL151
