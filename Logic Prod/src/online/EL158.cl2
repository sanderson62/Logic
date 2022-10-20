00001  ID DIVISION.                                                     09/10/96
00002                                                                   EL158
00003  PROGRAM-ID.                 EL158.                                  LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/13/96 09:56:35.                    CL**3
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL158
00008 *                                                                 EL158
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL158
00012 *DATE-COMPILED.                                                      CL**3
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *                                                                *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL158
00024 *REMARKS.                                                         EL158
00025                                                                   EL158
00026 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED   EL158
00027 *    FOR THE POLICY FORM FILE RECORDS.                            EL158
00028                                                                   EL158
00029 *    SCREENS     - EL158A - FORM MAINTENANCE                      EL158
00030                                                                   EL158
00031 *    ENTERED BY  - EL601 - MAINTENANCE MENU                       EL158
00032                                                                   EL158
00033 *    EXIT TO     - EL601 - MAINTENANCE MENU                       EL158
00034                                                                   EL158
00035 *    INPUT FILE  - ERFORM -              - POLICY FORM FILE       EL158
00036                                                                   EL158
00037 *    OUTPUT FILE - ERFORM -              - POLICY FORM FILE       EL158
00038                                                                   EL158
00039 *    COMMAREA    - PASSED                                         EL158
00040                                                                   EL158
00041 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON     EL158
00042 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE EL158
00043 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVEEL158
00044 *                  ENTRIES (XCTL FROM CICS VIA EX39) THE SCREEN   EL158
00045 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   EL158
00046 *                  MAINTENANCE TYPE INDICATED.                    EL158
00047                                                                   EL158
00048      EJECT                                                        EL158
00049  ENVIRONMENT DIVISION.                                            EL158
00050  DATA DIVISION.                                                   EL158
00051  WORKING-STORAGE SECTION.                                         EL158
00052  77  FILLER  PIC X(32)  VALUE '********************************'. EL158
00053  77  FILLER  PIC X(32)  VALUE '*    EL158 WORKING STORAGE     *'. EL158
00054  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.    CL**4
00055                                                                   EL158
00056  77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.    EL158
00057      88  FIRST-READ-PREV                         VALUE 'Y'.       EL158
00058                                                                   EL158
00059  01  ACCESS-KEYS.                                                 EL158
00060      12  ERFORM-KEY.                                              EL158
00061          16  ERFORM-COMPANY-CD       PIC X(01).                   EL158
00062          16  ERFORM-STATE            PIC X(02).                   EL158
00063          16  ERFORM-FORM-ID          PIC X(12).                   EL158
00064          16  ERFORM-EXP-DT           PIC X(02).                   EL158
00065                                                                   EL158
00066      12  ELCNTL-KEY.                                              EL158
00067          16  ELCNTL-COMPANY-ID       PIC X(03).                   EL158
00068          16  ELCNTL-RECORD-TYPE      PIC X(01).                   EL158
00069          16  ELCNTL-ACCESS           PIC X(04).                   EL158
00070          16  ELCNTL-STATE-ACCESS REDEFINES ELCNTL-ACCESS.         EL158
00071              20  ELCNTL-STATE-CD     PIC  X(02).                  EL158
00072              20  FILLER              PIC  X(02).                  EL158
00073          16  ELCNTL-BENEFIT-ACCESS REDEFINES ELCNTL-ACCESS.       EL158
00074              20  FILLER              PIC  X(02).                  EL158
00075              20  ELCNTL-BENE-CD      PIC  X(02).                  EL158
00076          16  ELCNTL-SEQUENCE-NO      PIC S9(04)      COMP.        EL158
00077                                                                   EL158
00078  01  WS-DATE-AREA.                                                EL158
00079      05  SAVE-DATE                   PIC X(08)   VALUE SPACES.    EL158
00080      05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.    EL158
00081                                                                   EL158
00082  01  MISC-WORK-AREAS.                                             EL158
00083                                                                   EL158
00084      12  DEEDIT-FIELD                PIC X(15).                   EL158
00085      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD                   EL158
00086                                      PIC S9(15).                  EL158
00087      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD                   EL158
00088                                      PIC S9(13)V99.               EL158
00089                                                                   EL158
00090      12  WS-BENEFIT-FIELD.                                        EL158
00091          16  WS-BENE-TYPE            PIC X(01).                   EL158
00092          16  WS-BENE-CODE            PIC X(02).                   EL158
00093                                                                   EL158
00094      12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.EL158
00095                                                                   EL158
00096  01  STANDARD-AREAS.                                              EL158
00097      12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.  EL158
00098      12  TRANS-ID                    PIC X(04)   VALUE 'E030'.    EL158
00099      12  EL150-TRANS-ID              PIC X(04)   VALUE 'EX23'.    EL158
00100      12  PGM-NAME                    PIC X(08).                   EL158
00101      12  TIME-IN                     PIC S9(07).                  EL158
00102      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL158
00103          16  FILLER                  PIC X(01).                   EL158
00104          16  TIME-OUT                PIC 99V99.                   EL158
00105          16  FILLER                  PIC X(02).                   EL158
00106      12  XCTL-005                    PIC X(08)   VALUE 'EL005'.   EL158
00107      12  XCTL-010                    PIC X(08)   VALUE 'EL010'.   EL158
00108      12  XCTL-155                    PIC X(08)   VALUE 'EL155'.   EL158
00109      12  XCTL-626                    PIC X(08)   VALUE 'EL626'.   EL158
00110      12  LINK-001                    PIC X(08)   VALUE 'EL001'.   EL158
00111      12  LINK-004                    PIC X(08)   VALUE 'EL004'.   EL158
00112      12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'. EL158
00113      12  THIS-PGM                    PIC X(08)   VALUE 'EL158'.   EL158
00114      12  ERFORM-FILE-ID              PIC X(08)   VALUE 'ERFORM'.  EL158
00115      12  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.  EL158
00116      12  ERFORM-LENGTH               PIC S9(04)  VALUE +500  COMP.   CL**4
00117      12  SUB                         PIC 9(02).                   EL158
00118      12  SUB-1                       PIC 9(02).                   EL158
00119      12  SUB2                        PIC 9(02).                      CL**4
00120      12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.     EL158
00121      12  MAPSET-NAME                 PIC X(08)   VALUE 'EL158S'.  EL158
00122      12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL158A'.  EL158
00123      12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.     EL158
00124                                                                   EL158
00125      12  WS-CNTL-REC-FOUND-SW        PIC X(01)   VALUE 'N'.       EL158
00126          88  CNTL-RECORD-FOUND                   VALUE 'Y'.       EL158
00127                                                                   EL158
00128      12  WS-PRE-EXIST-CODES          PIC X(02)   VALUE ZEROS.     EL158
00129          88  VALID-PRE-EXIST-CODE                VALUES ARE '00'  EL158
00130                                                        THRU '99'. EL158
00131                                                                   EL158
00132      12  WS-SUICIDE-CODES            PIC X(02)   VALUE ZEROS.     EL158
00133          88  VALID-SUICIDE-CODE                  VALUES ARE '00'  EL158
00134                                                        THRU '99'. EL158
00135                                                                   EL158
00136      12  WS-DISABILITY-CODES         PIC X(02)   VALUE ZEROS.     EL158
00137          88  VALID-DISABILITY-CODE               VALUES ARE '00'  EL158
00138                                                        THRU '99'. EL158
00139                                                                   EL158
00140      12  WS-REFUND-METHOD            PIC X       VALUE ZEROS.        CL**4
00141          88  VALID-REFUND-METHOD                 VALUES ARE ' ',     CL**4
00142                                                    '1' THRU '9'.     CL**4
00143                                                                      CL**4
00144      12  WS-BENEFIT-SW               PIC X       VALUE SPACE.     EL158
00145          88  BENEFIT-FOUND                       VALUE 'Y'.       EL158
00146          88  BENEFIT-NOT-FOUND                   VALUE 'N'.       EL158
00147                                                                   EL158
00148      12  WS-LFAMT                    PIC S9(9)V99 VALUE +0 COMP-3.EL158
00149      12  WS-AHAMT                    PIC S9(9)V99 VALUE +0 COMP-3.EL158
00150      12  WS-LFMIN                    PIC 9(3)     VALUE  0 COMP-3.   CL**3
00151      12  WS-AHMIN                    PIC 9(3)     VALUE  0 COMP-3.   CL**3
00152      12  WS-LFMAX                    PIC 9(3)     VALUE  0 COMP-3.   CL**3
00153      12  WS-AHMAX                    PIC 9(3)     VALUE  0 COMP-3.   CL**3
00154      12  WS-LFTRM                    PIC 9(3)     VALUE  0 COMP-3.   CL**3
00155      12  WS-AHTRM                    PIC 9(3)     VALUE  0 COMP-3.   CL**3
00156      12  WS-DEFTYP                   PIC 99       VALUE  0.          CL**3
00157      12  WS-LFPRE                    PIC 99       VALUE  0.          CL**3
00158      12  WS-AHPRE                    PIC 99       VALUE  0.          CL**3
00159      12  WS-SUICIDE                  PIC 99       VALUE  0.          CL**3
00160      12  WS-INIT-TABLE.                                              CL**4
00161          16  FILLER                  PIC X(03)  VALUE '01.'.         CL**4
00162          16  FILLER                  PIC X(03)  VALUE '02.'.         CL**4
00163          16  FILLER                  PIC X(03)  VALUE '03.'.         CL**4
00164          16  FILLER                  PIC X(03)  VALUE '04.'.         CL**4
00165          16  FILLER                  PIC X(03)  VALUE '05.'.         CL**4
00166          16  FILLER                  PIC X(03)  VALUE '06.'.         CL**4
00167          16  FILLER                  PIC X(03)  VALUE '07.'.         CL**4
00168          16  FILLER                  PIC X(03)  VALUE '08.'.         CL**4
00169          16  FILLER                  PIC X(03)  VALUE '09.'.         CL**4
00170          16  FILLER                  PIC X(03)  VALUE '10.'.         CL**4
00171          16  FILLER                  PIC X(03)  VALUE '11.'.         CL**4
00172          16  FILLER                  PIC X(03)  VALUE '12.'.         CL**4
00173          16  FILLER                  PIC X(03)  VALUE '13.'.         CL**4
00174          16  FILLER                  PIC X(03)  VALUE '14.'.         CL**4
00175          16  FILLER                  PIC X(03)  VALUE '15.'.         CL**4
00176          16  FILLER                  PIC X(03)  VALUE '16.'.         CL**4
00177          16  FILLER                  PIC X(03)  VALUE '17.'.         CL**4
00178          16  FILLER                  PIC X(03)  VALUE '18.'.         CL**4
00179          16  FILLER                  PIC X(03)  VALUE '19.'.         CL**4
00180          16  FILLER                  PIC X(03)  VALUE '20.'.         CL**4
00181          16  FILLER                  PIC X(03)  VALUE '21.'.         CL**4
00182          16  FILLER                  PIC X(03)  VALUE '22.'.         CL**4
00183          16  FILLER                  PIC X(03)  VALUE '23.'.         CL**4
00184          16  FILLER                  PIC X(03)  VALUE '24.'.         CL**4
00185          16  FILLER                  PIC X(03)  VALUE '25.'.         CL**4
00186          16  FILLER                  PIC X(03)  VALUE '26.'.         CL**4
00187          16  FILLER                  PIC X(03)  VALUE '27.'.         CL**4
00188          16  FILLER                  PIC X(03)  VALUE '28.'.         CL**4
00189          16  FILLER                  PIC X(03)  VALUE '29.'.         CL**4
00190          16  FILLER                  PIC X(03)  VALUE '30.'.         CL**4
00191          16  FILLER                  PIC X(03)  VALUE '31.'.         CL**4
00192          16  FILLER                  PIC X(03)  VALUE '32.'.         CL**4
00193          16  FILLER                  PIC X(03)  VALUE '33.'.         CL**4
00194          16  FILLER                  PIC X(03)  VALUE '34.'.         CL**4
00195          16  FILLER                  PIC X(03)  VALUE '35.'.         CL**4
00196          16  FILLER                  PIC X(03)  VALUE '36.'.         CL**4
00197          16  FILLER                  PIC X(03)  VALUE '37.'.         CL**4
00198          16  FILLER                  PIC X(03)  VALUE '38.'.         CL**4
00199          16  FILLER                  PIC X(03)  VALUE '39.'.         CL**4
00200          16  FILLER                  PIC X(03)  VALUE '40.'.         CL**4
00201      12  WS-INIT-VALUE REDEFINES WS-INIT-TABLE OCCURS 40 TIMES       CL**4
00202                                      PIC X(03).                      CL**4
00203      12  WS158-BENE-INIT             PIC X(03) VALUE '01.'.          CL**4
00204      EJECT                                                        EL158
00205                                      COPY ELCSCTM.                EL158
00206                                      COPY ELCSCRTY.               EL158
00207                                                                   EL158
00208      EJECT                                                        EL158
00209  01  ERROR-MESSAGES.                                              EL158
00210      12  ER-0000                     PIC X(04)   VALUE '0000'.    EL158
00211      12  ER-0023                     PIC X(04)   VALUE '0023'.    EL158
00212      12  ER-0029                     PIC X(04)   VALUE '0029'.    EL158
00213      12  ER-0050                     PIC X(04)   VALUE '0050'.    EL158
00214      12  ER-0068                     PIC X(04)   VALUE '0068'.    EL158
00215      12  ER-0070                     PIC X(04)   VALUE '0070'.    EL158
00216      12  ER-0130                     PIC X(04)   VALUE '0130'.    EL158
00217      12  ER-0131                     PIC X(04)   VALUE '0131'.    EL158
00218      12  ER-0132                     PIC X(04)   VALUE '0132'.    EL158
00219      12  ER-0138                     PIC X(04)   VALUE '0138'.    EL158
00220      12  ER-0144                     PIC X(04)   VALUE '0144'.    EL158
00221      12  ER-0418                     PIC X(04)   VALUE '0418'.    EL158
00222      12  ER-0582                     PIC X(04)   VALUE '0582'.       CL**4
00223      12  ER-0701                     PIC X(04)   VALUE '0701'.    EL158
00224      12  ER-0702                     PIC X(04)   VALUE '0702'.    EL158
00225      12  ER-0703                     PIC X(04)   VALUE '0703'.    EL158
00226      12  ER-0704                     PIC X(04)   VALUE '0704'.    EL158
00227      12  ER-0705                     PIC X(04)   VALUE '0705'.    EL158
00228      12  ER-0706                     PIC X(04)   VALUE '0706'.    EL158
00229      12  ER-0707                     PIC X(04)   VALUE '0707'.    EL158
00230      12  ER-0708                     PIC X(04)   VALUE '0708'.    EL158
00231      12  ER-0709                     PIC X(04)   VALUE '0709'.    EL158
00232      12  ER-0710                     PIC X(04)   VALUE '0710'.    EL158
00233      12  ER-0711                     PIC X(04)   VALUE '0711'.    EL158
00234      12  ER-0712                     PIC X(04)   VALUE '0712'.    EL158
00235      12  ER-0713                     PIC X(04)   VALUE '0713'.    EL158
00236      12  ER-0717                     PIC X(04)   VALUE '0717'.    EL158
00237      12  ER-0718                     PIC X(04)   VALUE '0718'.    EL158
00238      12  ER-0719                     PIC X(04)   VALUE '0719'.    EL158
00239      12  ER-0720                     PIC X(04)   VALUE '0720'.    EL158
00240      12  ER-0721                     PIC X(04)   VALUE '0721'.    EL158
00241      12  ER-0722                     PIC X(04)   VALUE '0722'.    EL158
00242      12  ER-0723                     PIC X(04)   VALUE '0723'.    EL158
00243      12  ER-0724                     PIC X(04)   VALUE '0724'.    EL158
00244      12  ER-0725                     PIC X(04)   VALUE '0725'.    EL158
00245      12  ER-0726                     PIC X(04)   VALUE '0726'.    EL158
00246      12  ER-0727                     PIC X(04)   VALUE '0727'.    EL158
00247      12  ER-0729                     PIC X(04)   VALUE '0729'.    EL158
00248      12  ER-0754                     PIC X(04)   VALUE '0754'.    EL158
00249      12  ER-2241                     PIC X(04)   VALUE '2241'.       CL**4
00250      12  ER-2276                     PIC X(04)   VALUE '2276'.       CL**4
00251      12  ER-7008                     PIC X(04)   VALUE '7008'.    EL158
00252      12  ER-7031                     PIC X(04)   VALUE '7031'.    EL158
00253      12  ER-7123                     PIC X(04)   VALUE '7123'.    EL158
00254      12  ER-8150                     PIC X(04)   VALUE '8150'.       CL**4
00255      EJECT                                                        EL158
00256                                      COPY ELCDATE.                EL158
00257      EJECT                                                        EL158
00258                                      COPY ELCLOGOF.               EL158
00259      EJECT                                                        EL158
00260                                      COPY ELCATTR.                EL158
00261      EJECT                                                        EL158
00262                                      COPY ELCEMIB.                EL158
00263      EJECT                                                        EL158
00264                                      COPY ELCJPFX.                EL158
00265                                      PIC X(530).                  EL158
00266                                                                   EL158
00267      EJECT                                                        EL158
00268                                      COPY ELCINTF.                EL158
00269      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL158
00270                                                                   EL158
00271          16  PI-FORM-NUMBER          PIC X(12).                   EL158
00272                                                                   EL158
00273          16  PI-PREV-FORM-KEY.                                    EL158
00274              20  PI-PREV-COMPANY-CD  PIC X(01).                   EL158
00275              20  PI-PREV-STATE       PIC X(02).                   EL158
00276              20  PI-PREV-FORM-ID     PIC X(12).                   EL158
00277              20  PI-PREV-EXP-DT      PIC X(02).                   EL158
00278                                                                   EL158
00279          16  PI-FORM-KEY.                                         EL158
00280              20  PI-STATE-CODE       PIC X(02).                   EL158
00281              20  PI-FORM-ID          PIC X(12).                   EL158
00282              20  PI-EXP-DT           PIC X(08).                   EL158
00283          16  FILLER                  PIC X(589).                     CL**3
00284                                                                   EL158
00285      EJECT                                                        EL158
00286                                      COPY ELCAID.                 EL158
00287  01  FILLER    REDEFINES DFHAID.                                  EL158
00288      12  FILLER                      PIC X(08).                   EL158
00289      12  PF-VALUES                   PIC X         OCCURS 24.     EL158
00290                                                                   EL158
00291      EJECT                                                        EL158
00292                                      COPY EL158S.                 EL158
00293  01  EL158AI-R REDEFINES EL158AI.                                 EL158
00294      12  FILLER                      PIC X(195).                  EL158
00295      12  EL158-BENE-TABLE OCCURS 20 TIMES.                           CL**4
00296          16  EL158-BENE-INIT-LENGTH  PIC S9(04)  COMP.               CL**4
00297          16  EL158-BENE-INIT-ATTRB   PIC X(01).                      CL**4
00298          16  EL158-BENE-INIT         PIC X(03).                      CL**4
00299          16  EL158-BENE-LENGTH       PIC S9(04)  COMP.            EL158
00300          16  EL158-BENE-ATTRB        PIC X(01).                   EL158
00301          16  EL158-BENE.                                             CL**4
00302              20  EL158-BENE-TYPE     PIC X(01).                      CL**4
00303              20  EL158-BENE-ID       PIC X(02).                      CL**4
00304          16  EL158-BENE-TERM-LENGTH  PIC S9(04)  COMP.               CL**4
00305          16  EL158-BENE-TERM-ATTRB   PIC X(01).                      CL**4
00306          16  EL158-BENE-TERM         PIC 9(03).                      CL**4
00307          16  EL158-BENE-TERM-R REDEFINES EL158-BENE-TERM             CL**4
00308                                      PIC X(03).                      CL**4
00309          16  EL158-BENE-REFM-LENGTH  PIC S9(04)  COMP.               CL**4
00310          16  EL158-BENE-REFM-ATTRB   PIC X(01).                      CL**4
00311          16  EL158-BENE-REFM         PIC X.                          CL**4
00312                                                                   EL158
00313      EJECT                                                        EL158
00314  LINKAGE SECTION.                                                 EL158
00315                                                                   EL158
00316  01  DFHCOMMAREA                     PIC X(1024).                 EL158
00317                                                                   EL158
00318      EJECT                                                        EL158
00319                                      COPY ERCFORM.                EL158
00320      EJECT                                                        EL158
00321                                      COPY ELCCNTL.                EL158
00322      EJECT                                                        EL158
00323  PROCEDURE DIVISION.                                              EL158
00324                                                                   EL158
00325      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.         EL158
00326      MOVE '5'                        TO  DC-OPTION-CODE.          EL158
00327      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL158
00328      MOVE DC-GREG-DATE-1-EDIT        TO  SAVE-DATE.               EL158
00329      MOVE DC-BIN-DATE-1              TO  SAVE-BIN-DATE.           EL158
00330                                                                   EL158
00331      MOVE DFHCOMMAREA                TO  PROGRAM-INTERFACE-BLOCK. EL158
00332      IF EIBCALEN EQUAL 0                                          EL158
00333          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL158
00334                                                                   EL158
00335      MOVE PI-LIFE-OVERRIDE-L6        TO  EMI-LIFE-OVERRIDE-L6.    EL158
00336      MOVE PI-AH-OVERRIDE-L6          TO  EMI-AH-OVERRIDE-L6.      EL158
00337                                                                   EL158
00338      EXEC CICS HANDLE CONDITION                                   EL158
00339          DUPREC     (8850-DUPREC)                                 EL158
00340          NOTOPEN    (8870-NOTOPEN)                                EL158
00341          NOTFND     (8880-NOT-FOUND)                              EL158
00342          PGMIDERR   (9600-PGMID-ERROR)                            EL158
00343          ERROR      (9990-ABEND)                                  EL158
00344      END-EXEC.                                                    EL158
00345                                                                   EL158
00346      IF EIBTRNID NOT = EL150-TRANS-ID                             EL158
00347         GO TO 0150-SET-PROGRAM-SAVES.                             EL158
00348                                                                   EL158
00349      MOVE PI-COMPANY-CD              TO ERFORM-COMPANY-CD.        EL158
00350      MOVE PI-FORM-NUMBER             TO ERFORM-FORM-ID.           EL158
00351      MOVE PI-STATE                   TO ERFORM-STATE.             EL158
00352      MOVE PI-CERT-EFF-DT             TO ERFORM-EXP-DT.            EL158
00353                                                                   EL158
00354      EXEC CICS HANDLE CONDITION                                   EL158
00355          NOTFND     (0100-FORM-NOTFND)                            EL158
00356      END-EXEC.                                                    EL158
00357                                                                   EL158
00358      EXEC CICS READ                                               EL158
00359          DATASET    (ERFORM-FILE-ID)                              EL158
00360          SET        (ADDRESS OF FORM-MASTER)                         CL**3
00361          RIDFLD     (ERFORM-KEY)                                  EL158
00362          GTEQ                                                     EL158
00363      END-EXEC.                                                    EL158
00364                                                                   EL158
00365      IF PI-COMPANY-CD       =  FO-COMPANY-CD                      EL158
00366         AND FO-STATE        =  FO-STATE                           EL158
00367         AND FO-FORM-ID      =  ERFORM-FORM-ID                     EL158
00368         GO TO 0150-SET-PROGRAM-SAVES.                             EL158
00369                                                                   EL158
00370  0100-FORM-NOTFND.                                                EL158
00371                                                                   EL158
00372      MOVE XCTL-155                   TO  PGM-NAME.                EL158
00373      GO TO 9300-XCTL.                                             EL158
00374                                                                   EL158
00375  0150-SET-PROGRAM-SAVES.                                          EL158
00376                                                                   EL158
00377      IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM                     EL158
00378          IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM               EL158
00379              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL158
00380              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL158
00381              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL158
00382              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL158
00383              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL158
00384              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL158
00385              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL158
00386              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL158
00387          ELSE                                                     EL158
00388              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL158
00389              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL158
00390              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL158
00391              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL158
00392              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL158
00393              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL158
00394              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL158
00395              MOVE SPACES               TO PI-SAVED-PROGRAM-6      EL158
00396      ELSE                                                         EL158
00397          GO TO 0200-RECEIVE.                                      EL158
00398                                                                   EL158
00399      IF EIBTRNID = EL150-TRANS-ID                                 EL158
00400         MOVE 'I'                     TO MAINTI                    EL158
00401         MOVE +1                      TO MAINTL                    EL158
00402         MOVE DFHENTER                TO EIBAID                    EL158
00403         MOVE PI-STATE                TO STATEI                    EL158
00404         MOVE +2                      TO STATEL                    EL158
00405         MOVE PI-FORM-NUMBER          TO FORMI                     EL158
00406         MOVE +12                     TO FORML                     EL158
00407         MOVE FO-FORM-EXP-DT          TO DC-BIN-DATE-1             EL158
00408         MOVE ' '                     TO DC-OPTION-CODE            EL158
00409         MOVE +0                      TO DC-ELAPSED-DAYS           EL158
00410                                         DC-ELAPSED-MONTHS         EL158
00411         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL158
00412         IF NO-CONVERSION-ERROR                                    EL158
00413            MOVE DC-GREG-DATE-1-EDIT  TO EXPDTO                    EL158
00414            GO TO 1000-SHOW-FORM-RECORD                            EL158
00415         ELSE                                                      EL158
00416            MOVE LOW-VALUES           TO EXPDTO                    EL158
00417            GO TO 1000-SHOW-FORM-RECORD.                           EL158
00418                                                                   EL158
00419      MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.    EL158
00420      MOVE LOW-VALUES                 TO  EL158AO.                 EL158
00421      MOVE -1                         TO  MAINTL.                  EL158
00422      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
00423                                                                   EL158
00424      EJECT                                                        EL158
00425  0200-RECEIVE.                                                    EL158
00426                                                                   EL158
00427      IF EIBAID IS EQUAL TO DFHCLEAR                               EL158
00428          GO TO 9400-CLEAR.                                        EL158
00429                                                                   EL158
00430      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   EL158
00431          MOVE LOW-VALUES             TO  EL158AI                  EL158
00432          MOVE ER-7008                TO  EMI-ERROR                EL158
00433          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
00434          MOVE -1                     TO  MAINTL                   EL158
00435          GO TO 8200-SEND-DATAONLY.                                EL158
00436                                                                   EL158
00437      IF PI-PROCESSOR-ID EQUAL 'LGXX'                              EL158
00438          NEXT SENTENCE                                            EL158
00439      ELSE                                                         EL158
00440          EXEC CICS READQ TS                                       EL158
00441              QUEUE  (PI-SECURITY-TEMP-STORE-ID)                   EL158
00442              INTO   (SECURITY-CONTROL)                            EL158
00443              LENGTH (SC-COMM-LENGTH)                              EL158
00444              ITEM   (SC-ITEM)                                     EL158
00445          END-EXEC                                                 EL158
00446          MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP           EL158
00447          MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.           EL158
00448                                                                   EL158
00449      EXEC CICS RECEIVE                                            EL158
00450          MAP      (WS-MAP-NAME)                                   EL158
00451          MAPSET   (MAPSET-NAME)                                   EL158
00452          INTO     (EL158AI)                                       EL158
00453      END-EXEC.                                                    EL158
00454                                                                   EL158
00455      IF PFKEYL IS EQUAL TO +0                                     EL158
00456          GO TO 0300-CHECK-PFKEYS.                                 EL158
00457                                                                   EL158
00458      IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)       EL158
00459          MOVE PF-VALUES (PFKEYI)     TO  EIBAID                   EL158
00460      ELSE                                                         EL158
00461          MOVE ER-0029                TO  EMI-ERROR                EL158
00462          GO TO 0320-INPUT-ERROR.                                  EL158
00463                                                                   EL158
00464                                                                   EL158
00465  0300-CHECK-PFKEYS.                                               EL158
00466                                                                   EL158
00467      IF EIBAID EQUAL DFHPF23                                      EL158
00468          GO TO 8810-PF23.                                         EL158
00469                                                                   EL158
00470      IF EIBAID EQUAL DFHPF24                                      EL158
00471          GO TO 9200-RETURN-MAIN-MENU.                             EL158
00472                                                                   EL158
00473      IF EIBAID EQUAL DFHPF12                                      EL158
00474          GO TO 9500-PF12.                                         EL158
00475                                                                   EL158
00476      IF (MAINTL NOT EQUAL 0) AND (EIBAID NOT EQUAL DFHENTER)      EL158
00477          MOVE ER-0050            TO EMI-ERROR                     EL158
00478          GO TO 0320-INPUT-ERROR.                                  EL158
00479                                                                   EL158
00480      IF EIBAID EQUAL DFHPF1                                       EL158
00481          GO TO 5000-FIND-NEXT-FORM-RECORD.                        EL158
00482                                                                   EL158
00483      IF EIBAID EQUAL DFHPF2                                       EL158
00484          GO TO 5100-FIND-PREV-FORM-RECORD.                        EL158
00485                                                                      CL**4
00486      IF EIBAID EQUAL DFHPF3                                          CL**4
00487          GO TO 5200-FIND-NEXT-BENEFIT.                               CL**4
00488                                                                      CL**4
00489      IF EIBAID EQUAL DFHPF4                                          CL**4
00490          GO TO 5200-FIND-PREV-BENEFIT.                               CL**4
00491                                                                   EL158
00492      IF EIBAID EQUAL DFHENTER                                     EL158
00493          GO TO 0330-EDIT-DATA.                                    EL158
00494                                                                   EL158
00495      MOVE ER-0029                    TO  EMI-ERROR.               EL158
00496                                                                   EL158
00497  0320-INPUT-ERROR.                                                EL158
00498                                                                   EL158
00499      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
00500      MOVE AL-UNBON                   TO  PFKEYA.                  EL158
00501      MOVE -1                         TO  PFKEYL.                  EL158
00502      GO TO 8200-SEND-DATAONLY.                                    EL158
00503                                                                   EL158
00504      EJECT                                                        EL158
00505  0330-EDIT-DATA.                                                  EL158
00506                                                                   EL158
00507      IF NOT DISPLAY-CAP                                           EL158
00508          MOVE 'READ'                 TO  SM-READ                  EL158
00509          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL**3
00510          MOVE ER-0070                TO  EMI-ERROR                EL158
00511          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
00512          MOVE -1                     TO  MAINTL                   EL158
00513          GO TO 8100-SEND-INITIAL-MAP.                             EL158
00514                                                                   EL158
00515      IF (STATEL GREATER THAN +0 AND                               EL158
00516          FORML  GREATER THAN +0 AND                               EL158
00517          EXPDTL GREATER THAN +0)                                  EL158
00518          NEXT SENTENCE                                            EL158
00519      ELSE                                                         EL158
00520          IF (MAINTI    EQUAL   'S'   AND                             CL**2
00521              EXPDTL    EQUAL   +0)                                   CL**2
00522              GO TO 1000-SHOW-FORM-RECORD                             CL**2
00523          ELSE                                                        CL**2
00524              MOVE ER-0754                TO  EMI-ERROR               CL**2
00525              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**2
00526              MOVE -1                     TO  STATEL                  CL**2
00527              MOVE AL-UABON               TO  STATEA                  CL**2
00528                                          FORMA                    EL158
00529                                          EXPDTA                   EL158
00530              GO TO 8200-SEND-DATAONLY.                               CL**2
00531                                                                   EL158
00532                                                                   EL158
00533      IF MAINTI EQUAL 'S'                                          EL158
00534          GO TO 1000-SHOW-FORM-RECORD.                             EL158
00535                                                                   EL158
00536      IF MAINTI EQUAL 'A' OR 'C' OR 'D'                            EL158
00537         IF NOT MODIFY-CAP                                         EL158
00538             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           CL**3
00539             MOVE ER-0070             TO  EMI-ERROR                EL158
00540             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL158
00541             MOVE LOW-VALUES          TO  EL158AO                  EL158
00542             MOVE -1                  TO  MAINTL                   EL158
00543             GO TO 8100-SEND-INITIAL-MAP.                          EL158
00544                                                                   EL158
00545      IF MAINTI EQUAL 'C'                                          EL158
00546          GO TO 2000-CHANGE-FORM-RECORD.                           EL158
00547                                                                   EL158
00548      IF MAINTI EQUAL 'A'                                          EL158
00549          GO TO 3000-ADD-FORM-RECORD.                              EL158
00550                                                                   EL158
00551      IF MAINTI EQUAL 'D'                                          EL158
00552          GO TO 4000-DELETE-FORM-RECORD.                           EL158
00553                                                                   EL158
00554      MOVE ER-0023                    TO  EMI-ERROR.               EL158
00555      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
00556      MOVE -1                         TO  MAINTL.                  EL158
00557      MOVE AL-UABON                   TO  MAINTA.                  EL158
00558      GO TO 8200-SEND-DATAONLY.                                    EL158
00559                                                                   EL158
00560      EJECT                                                        EL158
00561  1000-SHOW-FORM-RECORD.                                           EL158
00562                                                                   EL158
00563      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.       EL158
00564      MOVE STATEI                     TO  ERFORM-STATE.            EL158
00565      MOVE FORMI                      TO  ERFORM-FORM-ID.          EL158
00566                                                                   EL158
00567      IF (MAINTI    EQUAL   'S'  AND                                  CL**2
00568          EXPDTL    EQUAL   +0)                                       CL**2
00569          MOVE  HIGH-VALUES           TO  ERFORM-EXP-DT               CL**2
00570          GO TO 1000-CONTINUE.                                        CL**2
00571                                                                      CL**2
00572      MOVE EXPDTI                     TO  DEEDIT-FIELD.            EL158
00573      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          EL158
00574      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999                   EL158
00575          MOVE HIGH-VALUES            TO  ERFORM-EXP-DT            EL158
00576      ELSE                                                         EL158
00577          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY       EL158
00578          MOVE '4'                    TO  DC-OPTION-CODE           EL158
00579          MOVE +0                     TO  DC-ELAPSED-DAYS          EL158
00580                                          DC-ELAPSED-MONTHS        EL158
00581          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL158
00582          IF NO-CONVERSION-ERROR                                   EL158
00583              MOVE DC-BIN-DATE-1      TO  ERFORM-EXP-DT            EL158
00584          ELSE                                                     EL158
00585              MOVE LOW-VALUES         TO  ERFORM-EXP-DT.           EL158
00586                                                                      CL**2
00587  1000-CONTINUE.                                                      CL**2
00588                                                                   EL158
00589      EXEC CICS HANDLE CONDITION                                   EL158
00590          NOTFND     (1000-FORM-NOTFND)                            EL158
00591      END-EXEC.                                                    EL158
00592                                                                   EL158
00593      EXEC CICS READ                                               EL158
00594          DATASET    (ERFORM-FILE-ID)                              EL158
00595          SET        (ADDRESS OF FORM-MASTER)                         CL**3
00596          RIDFLD     (ERFORM-KEY)                                  EL158
00597      END-EXEC.                                                    EL158
00598                                                                   EL158
00599      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL158
00600                                                                   EL158
00601  1000-FORM-NOTFND.                                                EL158
00602                                                                   EL158
00603       MOVE ER-0418                   TO  EMI-ERROR.               EL158
00604       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   EL158
00605       MOVE -1                        TO  MAINTL.                  EL158
00606       MOVE AL-UABON                  TO  STATEA                   EL158
00607                                          FORMA                    EL158
00608                                          EXPDTA.                  EL158
00609                                                                   EL158
00610      IF EIBTRNID = EL150-TRANS-ID                                 EL158
00611         GO TO 8100-SEND-INITIAL-MAP                               EL158
00612      ELSE                                                         EL158
00613         GO TO 8200-SEND-DATAONLY.                                 EL158
00614                                                                   EL158
00615      EJECT                                                        EL158
00616  2000-CHANGE-FORM-RECORD.                                         EL158
00617                                                                   EL158
00618      MOVE EXPDTI                     TO  DEEDIT-FIELD.            EL158
00619      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          EL158
00620      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999                   EL158
00621          MOVE HIGH-VALUES            TO  WS-EXP-DT                EL158
00622      ELSE                                                         EL158
00623          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY       EL158
00624          MOVE '4'                    TO  DC-OPTION-CODE           EL158
00625          MOVE +0                     TO  DC-ELAPSED-DAYS          EL158
00626                                          DC-ELAPSED-MONTHS        EL158
00627          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL158
00628          MOVE DC-BIN-DATE-1          TO  WS-EXP-DT.               EL158
00629                                                                   EL158
00630      IF MAINTI = 'C'                                              EL158
00631         IF (STATEI IS EQUAL TO PI-PREV-STATE AND                  EL158
00632            FORMI  IS EQUAL TO PI-PREV-FORM-ID) AND                EL158
00633            (WS-EXP-DT NOT EQUAL TO PI-PREV-EXP-DT)                EL158
00634             MOVE ER-0717                TO  EMI-ERROR             EL158
00635             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL158
00636             MOVE -1                     TO  MAINTL                EL158
00637             MOVE AL-UABON               TO  STATEA                EL158
00638                                          FORMA                    EL158
00639                                          EXPDTA                   EL158
00640             GO TO 8200-SEND-DATAONLY.                             EL158
00641                                                                   EL158
00642      IF STATEI IS EQUAL TO PI-PREV-STATE AND                      EL158
00643         FORMI  IS EQUAL TO PI-PREV-FORM-ID AND                    EL158
00644         WS-EXP-DT IS EQUAL TO PI-PREV-EXP-DT                      EL158
00645          NEXT SENTENCE                                            EL158
00646      ELSE                                                         EL158
00647          MOVE ER-0138                TO  EMI-ERROR                EL158
00648          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
00649          MOVE -1                     TO  MAINTL                   EL158
00650          MOVE AL-UABON               TO  STATEA                   EL158
00651                                          FORMA                    EL158
00652                                          EXPDTA                   EL158
00653          GO TO 8200-SEND-DATAONLY.                                EL158
00654                                                                   EL158
00655      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 EL158
00656                                                                   EL158
00657      IF NOT EMI-NO-ERRORS                                         EL158
00658          GO TO 8200-SEND-DATAONLY.                                EL158
00659                                                                   EL158
00660      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.       EL158
00661      MOVE STATEI                     TO  ERFORM-STATE             EL158
00662                                          PI-STATE-CODE.           EL158
00663      MOVE FORMI                      TO  ERFORM-FORM-ID           EL158
00664                                          PI-FORM-ID.              EL158
00665      MOVE EXPDTI                     TO  DEEDIT-FIELD.            EL158
00666      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          EL158
00667      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999                   EL158
00668          MOVE HIGH-VALUES            TO  ERFORM-EXP-DT            EL158
00669          MOVE '99/99/99'             TO  PI-EXP-DT                EL158
00670      ELSE                                                         EL158
00671          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY       EL158
00672          MOVE '4'                    TO  DC-OPTION-CODE           EL158
00673          MOVE +0                     TO  DC-ELAPSED-DAYS          EL158
00674                                          DC-ELAPSED-MONTHS        EL158
00675          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL158
00676          IF NO-CONVERSION-ERROR                                   EL158
00677              MOVE DC-BIN-DATE-1      TO  ERFORM-EXP-DT            EL158
00678              MOVE DC-GREG-DATE-1-EDIT TO PI-EXP-DT                EL158
00679          ELSE                                                     EL158
00680              MOVE LOW-VALUES         TO  ERFORM-EXP-DT            EL158
00681                                          PI-EXP-DT.               EL158
00682                                                                   EL158
00683      EXEC CICS READ                                               EL158
00684          DATASET    (ERFORM-FILE-ID)                              EL158
00685          SET        (ADDRESS OF FORM-MASTER)                         CL**3
00686          RIDFLD     (ERFORM-KEY)                                  EL158
00687          UPDATE                                                   EL158
00688      END-EXEC.                                                    EL158
00689                                                                   EL158
00690      IF FO-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR            EL158
00691         FO-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS           EL158
00692          EXEC CICS UNLOCK                                         EL158
00693              DATASET   (ERFORM-FILE-ID)                           EL158
00694          END-EXEC                                                 EL158
00695          MOVE ER-0068                TO  EMI-ERROR                EL158
00696          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
00697          GO TO 1000-SHOW-FORM-RECORD.                             EL158
00698                                                                   EL158
00699 *    MOVE 'B'                        TO  JP-RECORD-TYPE.          EL158
00700 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.          EL158
00701 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL158
00702                                                                   EL158
00703      MOVE PI-PROCESSOR-ID            TO  FO-LAST-MAINT-BY.        EL158
00704      MOVE EIBTIME                    TO  FO-LAST-MAINT-HHMMSS.    EL158
00705      MOVE SAVE-BIN-DATE              TO  FO-LAST-MAINT-DT.        EL158
00706                                                                   EL158
00707      IF INDGRPL IS GREATER THAN +0                                EL158
00708          MOVE INDGRPI                TO  FO-IND-GRP-CD.           EL158
00709                                                                   EL158
00710      IF MAXATTL IS GREATER THAN +0                                EL158
00711          MOVE MAXATTI                TO  FO-MAX-ATT-AGE.          EL158
00712                                                                   EL158
00713      IF LFMINL IS GREATER THAN +0                                 EL158
00714          MOVE LFMINI                 TO  FO-LF-MIN-ISSUE-AGE.     EL158
00715                                                                   EL158
00716      IF LFMAXL IS GREATER THAN +0                                 EL158
00717          MOVE LFMAXI                 TO  FO-LF-MAX-ISSUE-AGE.     EL158
00718                                                                   EL158
00719      IF LFTRML IS GREATER THAN +0                                 EL158
00720          MOVE LFTRMI                 TO  FO-LF-MAX-TERM.          EL158
00721                                                                   EL158
00722      IF LFAMTL IS GREATER THAN +0                                 EL158
00723          MOVE LFAMTI                 TO  DEEDIT-FIELD-V2          EL158
00724          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
00725          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-LF-AMT.           EL158
00726                                                                   EL158
00727      IF FO-MAX-LF-AMT = +0                                        EL158
00728         MOVE +0                      TO  FO-LF-MIN-ISSUE-AGE      EL158
00729                                          FO-LF-MAX-ISSUE-AGE      EL158
00730                                          FO-LF-MAX-TERM.          EL158
00731                                                                   EL158
00732      IF LFPREL IS GREATER THAN +0                                 EL158
00733          MOVE LFPREI                 TO FO-LF-PRE-EXIST-EXCL-TYPE.EL158
00734                                                                   EL158
00735      IF SUICIDEL IS GREATER THAN +0                               EL158
00736          MOVE SUICIDEI               TO  FO-SUICIDE-EXCL-TYPE.    EL158
00737                                                                   EL158
00738      IF DEFTYPL IS GREATER THAN +0                                EL158
00739          MOVE DEFTYPI                TO  FO-DIS-DEF-TYPE.         EL158
00740                                                                   EL158
00741      IF DISMCDL IS GREATER THAN +0                                EL158
00742          MOVE DISMCDI                TO  FO-DISMEMBERMENT-CD.     EL158
00743                                                                   EL158
00744      IF CRTAPPL IS GREATER THAN +0                                EL158
00745          MOVE CRTAPPI                TO  FO-APP-CERT-USE-CD.      EL158
00746                                                                   EL158
00747      IF AHMINL IS GREATER THAN +0                                 EL158
00748          MOVE AHMINI                 TO  FO-AH-MIN-ISSUE-AGE.     EL158
00749                                                                   EL158
00750      IF AHMAXL IS GREATER THAN +0                                 EL158
00751          MOVE AHMAXI                 TO  FO-AH-MAX-ISSUE-AGE.     EL158
00752                                                                   EL158
00753      IF AHTRML IS GREATER THAN +0                                 EL158
00754          MOVE AHTRMI                 TO  FO-AH-MAX-TERM.          EL158
00755                                                                   EL158
00756      IF AHAMTL IS GREATER THAN +0                                 EL158
00757          MOVE AHAMTI                 TO  DEEDIT-FIELD-V2          EL158
00758          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
00759          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-AH-AMT.           EL158
00760                                                                   EL158
00761      IF FO-MAX-AH-AMT = +0                                        EL158
00762         MOVE +0                      TO  FO-AH-MIN-ISSUE-AGE      EL158
00763                                          FO-AH-MAX-ISSUE-AGE      EL158
00764                                          FO-AH-MAX-TERM.          EL158
00765                                                                   EL158
00766      IF AHPREL IS GREATER THAN +0                                 EL158
00767          MOVE AHPREI                 TO FO-AH-PRE-EXIST-EXCL-TYPE.EL158
00768                                                                   EL158
00769      IF DESCL IS GREATER THAN +0                                  EL158
00770          MOVE DESCI                  TO  FO-COMMENT-LINE-1.       EL158
00771                                                                   EL158
00772          MOVE +0                     TO  SUB.                     EL158
00773      IF EL158-BENE-INIT (1) EQUAL TO WS-INIT-VALUE (21)              CL**4
00774          MOVE EL158-BENE-INIT (1) TO WS158-BENE-INIT                 CL**4
00775          MOVE +20                    TO SUB2                         CL**4
00776      ELSE                                                            CL**4
00777          MOVE +0                     TO SUB2.                        CL**4
00778                                                                   EL158
00779  2000-CHANGE-BENEFIT-LOOP.                                        EL158
00780                                                                   EL158
00781      ADD +1                          TO  SUB.                     EL158
00782      ADD +1                          TO  SUB2.                       CL**4
00783      IF SUB IS GREATER THAN +20                                      CL**4
00784          GO TO 2000-CONTINUE-CHANGE.                              EL158
00785                                                                   EL158
00786      IF EL158-BENE-LENGTH (SUB) IS GREATER THAN +0                EL158
00787          MOVE EL158-BENE-TYPE (SUB)  TO  FO-PLAN-TYPE (SUB2)         CL**4
00788          MOVE EL158-BENE-ID   (SUB)  TO  FO-PLAN-ID   (SUB2).        CL**4
00789                                                                      CL**4
00790      IF EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0              CL**4
00791          MOVE EL158-BENE-TERM (SUB)  TO  FO-PLAN-TERM (SUB2).        CL**4
00792                                                                      CL**4
00793      IF EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0              CL**4
00794          MOVE EL158-BENE-REFM (SUB)                                  CL**4
00795                               TO FO-PLAN-REFUND-METHOD (SUB2).       CL**4
00796                                                                   EL158
00797      GO TO 2000-CHANGE-BENEFIT-LOOP.                              EL158
00798                                                                   EL158
00799  2000-CONTINUE-CHANGE.                                            EL158
00800 *    MOVE 'C'                        TO  JP-RECORD-TYPE.          EL158
00801 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.          EL158
00802                                                                   EL158
00803      EXEC CICS REWRITE                                            EL158
00804          DATASET   (ERFORM-FILE-ID)                               EL158
00805          FROM      (FORM-MASTER)                                  EL158
00806      END-EXEC.                                                    EL158
00807                                                                   EL158
00808 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL158
00809      MOVE ER-0000                    TO  EMI-ERROR.               EL158
00810      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
00811      MOVE LOW-VALUES                 TO  EL158AO.                 EL158
00812      MOVE -1                         TO  MAINTL.                  EL158
00813      MOVE PI-STATE-CODE              TO  STATEI.                  EL158
00814      MOVE PI-FORM-ID                 TO  FORMI.                   EL158
00815      MOVE PI-EXP-DT                  TO  EXPDTI.                  EL158
00816      GO TO 1000-SHOW-FORM-RECORD.                                 EL158
00817                                                                   EL158
00818      EJECT                                                        EL158
00819  3000-ADD-FORM-RECORD.                                            EL158
00820                                                                   EL158
00821      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 EL158
00822                                                                   EL158
00823      IF NOT EMI-NO-ERRORS                                         EL158
00824          GO TO 8200-SEND-DATAONLY.                                EL158
00825                                                                   EL158
00826      EXEC CICS GETMAIN                                            EL158
00827          SET       (ADDRESS OF FORM-MASTER)                          CL**3
00828          LENGTH    (ERFORM-LENGTH)                                EL158
00829          INITIMG   (GETMAIN-SPACE)                                EL158
00830      END-EXEC.                                                    EL158
00831                                                                   EL158
00832      MOVE SPACES                     TO  FORM-MASTER.             EL158
00833                                                                   EL158
00834      MOVE 'FO'                       TO  FO-RECORD-ID.            EL158
00835      MOVE PI-COMPANY-CD              TO  FO-COMPANY-CD.           EL158
00836      MOVE STATEI                     TO  FO-STATE                 EL158
00837                                          PI-STATE-CODE.           EL158
00838      MOVE FORMI                      TO  FO-FORM-ID               EL158
00839                                          PI-FORM-ID.              EL158
00840                                                                   EL158
00841      MOVE EXPDTI                     TO  DEEDIT-FIELD.            EL158
00842      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          EL158
00843      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999                   EL158
00844          MOVE HIGH-VALUES            TO  FO-FORM-EXP-DT           EL158
00845          MOVE '99/99/99'             TO  PI-EXP-DT                EL158
00846      ELSE                                                         EL158
00847          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY       EL158
00848          MOVE '4'                    TO  DC-OPTION-CODE           EL158
00849          MOVE +0                     TO  DC-ELAPSED-DAYS          EL158
00850                                          DC-ELAPSED-MONTHS        EL158
00851          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL158
00852          IF NO-CONVERSION-ERROR                                   EL158
00853              MOVE DC-BIN-DATE-1      TO  FO-FORM-EXP-DT           EL158
00854              MOVE DC-GREG-DATE-1-EDIT TO PI-EXP-DT                EL158
00855           ELSE                                                    EL158
00856              MOVE LOW-VALUES         TO  FO-FORM-EXP-DT           EL158
00857                                          PI-EXP-DT.               EL158
00858                                                                   EL158
00859      IF INDGRPL IS GREATER THAN +0                                EL158
00860          MOVE INDGRPI                TO  FO-IND-GRP-CD.           EL158
00861                                                                   EL158
00862      IF MAXATTL IS GREATER THAN +0                                EL158
00863          MOVE MAXATTI                TO  FO-MAX-ATT-AGE           EL158
00864      ELSE                                                         EL158
00865          MOVE +0                     TO  FO-MAX-ATT-AGE.          EL158
00866                                                                   EL158
00867      IF LFMINL IS GREATER THAN +0                                 EL158
00868          MOVE LFMINI                 TO  FO-LF-MIN-ISSUE-AGE      EL158
00869      ELSE                                                         EL158
00870          MOVE ZEROS                  TO  FO-LF-MIN-ISSUE-AGE.     EL158
00871                                                                   EL158
00872      IF LFMAXL IS GREATER THAN +0                                 EL158
00873          MOVE LFMAXI                 TO  FO-LF-MAX-ISSUE-AGE      EL158
00874      ELSE                                                         EL158
00875          MOVE +0                     TO  FO-LF-MAX-ISSUE-AGE.     EL158
00876                                                                   EL158
00877      IF LFTRML IS GREATER THAN +0                                 EL158
00878          MOVE LFTRMI                 TO  FO-LF-MAX-TERM           EL158
00879      ELSE                                                         EL158
00880          MOVE +0                     TO  FO-LF-MAX-TERM.          EL158
00881                                                                   EL158
00882      IF LFAMTL IS GREATER THAN +0                                 EL158
00883          MOVE LFAMTI                 TO  DEEDIT-FIELD-V2          EL158
00884          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
00885          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-LF-AMT            EL158
00886      ELSE                                                         EL158
00887          MOVE +0                     TO  FO-MAX-LF-AMT.           EL158
00888                                                                   EL158
00889      IF LFPREL IS GREATER THAN +0                                 EL158
00890          MOVE LFPREI                 TO FO-LF-PRE-EXIST-EXCL-TYPE EL158
00891      ELSE                                                         EL158
00892          MOVE ZEROS                  TO FO-LF-PRE-EXIST-EXCL-TYPE.EL158
00893                                                                   EL158
00894      IF SUICIDEL IS GREATER THAN +0                               EL158
00895          MOVE SUICIDEI               TO  FO-SUICIDE-EXCL-TYPE     EL158
00896      ELSE                                                         EL158
00897          MOVE +0                     TO  FO-SUICIDE-EXCL-TYPE.    EL158
00898                                                                   EL158
00899      IF DEFTYPL IS GREATER THAN +0                                EL158
00900          MOVE DEFTYPI                TO  FO-DIS-DEF-TYPE          EL158
00901      ELSE                                                         EL158
00902          MOVE ZEROS                  TO  FO-DIS-DEF-TYPE.         EL158
00903                                                                   EL158
00904      IF DISMCDL IS GREATER THAN +0                                EL158
00905          MOVE DISMCDI                TO  FO-DISMEMBERMENT-CD      EL158
00906      ELSE                                                         EL158
00907          MOVE 'N'                    TO  FO-DISMEMBERMENT-CD.     EL158
00908                                                                   EL158
00909      IF CRTAPPL IS GREATER THAN +0                                EL158
00910          MOVE CRTAPPI                TO  FO-APP-CERT-USE-CD       EL158
00911      ELSE                                                         EL158
00912          MOVE 'N'                    TO  FO-APP-CERT-USE-CD.      EL158
00913                                                                   EL158
00914      IF AHMINL IS GREATER THAN +0                                 EL158
00915          MOVE AHMINI                 TO  FO-AH-MIN-ISSUE-AGE      EL158
00916      ELSE                                                         EL158
00917          MOVE ZEROS                  TO  FO-AH-MIN-ISSUE-AGE.     EL158
00918                                                                   EL158
00919      IF AHMAXL IS GREATER THAN +0                                 EL158
00920          MOVE AHMAXI                 TO  FO-AH-MAX-ISSUE-AGE      EL158
00921      ELSE                                                         EL158
00922          MOVE +0                     TO  FO-AH-MAX-ISSUE-AGE.     EL158
00923                                                                   EL158
00924      IF AHTRML IS GREATER THAN +0                                 EL158
00925          MOVE AHTRMI                 TO  FO-AH-MAX-TERM           EL158
00926      ELSE                                                         EL158
00927          MOVE +0                     TO  FO-AH-MAX-TERM.          EL158
00928                                                                   EL158
00929      IF AHPREL IS GREATER THAN +0                                 EL158
00930          MOVE AHPREI                 TO FO-AH-PRE-EXIST-EXCL-TYPE EL158
00931      ELSE                                                         EL158
00932          MOVE ZEROS                  TO FO-AH-PRE-EXIST-EXCL-TYPE.EL158
00933                                                                   EL158
00934      IF AHAMTL IS GREATER THAN +0                                 EL158
00935          MOVE AHAMTI                 TO  DEEDIT-FIELD-V2          EL158
00936          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
00937          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-AH-AMT            EL158
00938      ELSE                                                         EL158
00939          MOVE +0                     TO  FO-MAX-AH-AMT.           EL158
00940                                                                   EL158
00941      IF DESCL IS GREATER THAN +0                                  EL158
00942          MOVE DESCI                  TO  FO-COMMENT-LINE-1.       EL158
00943                                                                   EL158
00944      MOVE +0                         TO  SUB.                     EL158
00945                                                                   EL158
00946  3000-ADD-BENEFIT-LOOP.                                           EL158
00947                                                                   EL158
00948      ADD +1                          TO  SUB.                     EL158
00949      IF SUB IS GREATER THAN +20                                      CL**4
00950          PERFORM 3010-ADD-BENEFIT-INIT THRU 3010-ABI-EXIT 20 TIMES   CL**4
00951          GO TO 3000-ADD-CONTINUE.                                 EL158
00952                                                                   EL158
00953      IF EL158-BENE-LENGTH (SUB) IS GREATER THAN +0                EL158
00954          MOVE EL158-BENE-TYPE (SUB)  TO  FO-PLAN-TYPE  (SUB)         CL**4
00955          MOVE EL158-BENE-ID   (SUB)  TO  FO-PLAN-ID    (SUB)         CL**4
00956      ELSE                                                         EL158
00957          MOVE SPACES                 TO  FO-PLAN-TYPE  (SUB)         CL**4
00958          MOVE SPACES                 TO  FO-PLAN-ID    (SUB).        CL**4
00959                                                                      CL**4
00960      IF EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0              CL**4
00961          MOVE EL158-BENE-TERM (SUB)  TO  DEEDIT-FIELD-V0             CL**4
00962          PERFORM 8600-DEEDIT THRU 8600-EXIT                          CL**4
00963          MOVE DEEDIT-FIELD-V0        TO  FO-PLAN-TERM (SUB)          CL**4
00964      ELSE                                                            CL**4
00965          MOVE ZEROES                 TO  FO-PLAN-TERM  (SUB).        CL**4
00966                                                                      CL**4
00967      IF EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0              CL**4
00968          MOVE EL158-BENE-REFM (SUB)                                  CL**4
00969                               TO  FO-PLAN-REFUND-METHOD (SUB)        CL**4
00970      ELSE                                                            CL**4
00971          MOVE SPACES          TO  FO-PLAN-REFUND-METHOD (SUB).       CL**4
00972                                                                      CL**4
00973                                                                   EL158
00974      GO TO 3000-ADD-BENEFIT-LOOP.                                 EL158
00975                                                                   EL158
00976  3000-ADD-CONTINUE.                                               EL158
00977                                                                   EL158
00978 *    IF DATE-CONVERSION-ERROR                                     EL158
00979 *        MOVE LOW-VALUES             TO  FO-LAST-MAINT-DT         EL158
00980 *    ELSE                                                         EL158
00981 *        MOVE DC-BIN-DATE-1          TO  FO-LAST-MAINT-DT.        EL158
00982                                                                   EL158
00983      MOVE SAVE-BIN-DATE              TO  FO-LAST-MAINT-DT.        EL158
00984      MOVE PI-PROCESSOR-ID            TO  FO-LAST-MAINT-BY.        EL158
00985      MOVE EIBTIME                    TO  FO-LAST-MAINT-HHMMSS.    EL158
00986                                                                   EL158
00987 *    MOVE 'A'                        TO  JP-RECORD-TYPE.          EL158
00988 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.          EL158
00989                                                                   EL158
00990  3005-WRITE-ERFORM-FILE.                                          EL158
00991                                                                   EL158
00992      EXEC CICS WRITE                                              EL158
00993          DATASET    (ERFORM-FILE-ID)                              EL158
00994          FROM       (FORM-MASTER)                                 EL158
00995          RIDFLD     (FO-CONTROL-PRIMARY)                          EL158
00996      END-EXEC.                                                    EL158
00997                                                                   EL158
00998 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL158
00999      MOVE ER-0000                    TO  EMI-ERROR.               EL158
01000      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
01001      MOVE LOW-VALUES                 TO  EL158AO.                 EL158
01002      MOVE -1                         TO  MAINTL.                  EL158
01003      MOVE PI-STATE-CODE              TO  STATEI.                  EL158
01004      MOVE PI-FORM-ID                 TO  FORMI.                   EL158
01005      MOVE PI-EXP-DT                  TO  EXPDTI.                  EL158
01006      GO TO 1000-SHOW-FORM-RECORD.                                 EL158
01007                                                                   EL158
01008  3010-ADD-BENEFIT-INIT.                                              CL**4
01009      MOVE ZEROES                 TO  FO-PLAN-TERM  (SUB).            CL**4
01010      ADD +1                      TO  SUB.                            CL**4
01011  3010-ABI-EXIT.                                                      CL**4
01012      EXIT.                                                           CL**4
01013      EJECT                                                        EL158
01014  4000-DELETE-FORM-RECORD.                                         EL158
01015                                                                   EL158
01016      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.       EL158
01017      MOVE STATEI                     TO  ERFORM-STATE.            EL158
01018      MOVE FORMI                      TO  ERFORM-FORM-ID.          EL158
01019                                                                   EL158
01020      MOVE EXPDTI                     TO  DEEDIT-FIELD.            EL158
01021      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          EL158
01022      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999                   EL158
01023          MOVE HIGH-VALUES            TO  ERFORM-EXP-DT            EL158
01024      ELSE                                                         EL158
01025          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY       EL158
01026          MOVE '4'                    TO  DC-OPTION-CODE           EL158
01027          MOVE +0                     TO  DC-ELAPSED-DAYS          EL158
01028                                          DC-ELAPSED-MONTHS        EL158
01029          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL158
01030          IF NO-CONVERSION-ERROR                                   EL158
01031              MOVE DC-BIN-DATE-1      TO  ERFORM-EXP-DT            EL158
01032          ELSE                                                     EL158
01033              MOVE LOW-VALUES         TO  ERFORM-EXP-DT.           EL158
01034                                                                   EL158
01035      EXEC CICS READ                                               EL158
01036          DATASET   (ERFORM-FILE-ID)                               EL158
01037          SET       (ADDRESS OF FORM-MASTER)                          CL**3
01038          RIDFLD    (ERFORM-KEY)                                   EL158
01039          UPDATE                                                   EL158
01040      END-EXEC.                                                    EL158
01041                                                                   EL158
01042      IF FO-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR            EL158
01043         FO-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS           EL158
01044          EXEC CICS UNLOCK                                         EL158
01045              DATASET   (ERFORM-FILE-ID)                           EL158
01046              END-EXEC                                             EL158
01047          MOVE ER-0068                TO  EMI-ERROR                EL158
01048          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01049          GO TO 1000-SHOW-FORM-RECORD.                             EL158
01050                                                                   EL158
01051 *    MOVE 'D'                        TO  JP-RECORD-TYPE.          EL158
01052 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.          EL158
01053      EXEC CICS DELETE                                             EL158
01054          DATASET   (ERFORM-FILE-ID)                               EL158
01055          END-EXEC.                                                EL158
01056                                                                   EL158
01057 *    PERFORM 8400-LOG-JOURNAL-RECORD                              EL158
01058      MOVE ER-0000                    TO  EMI-ERROR.               EL158
01059      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
01060      MOVE LOW-VALUES                 TO  EL158AO.                 EL158
01061      MOVE -1                         TO  MAINTL.                  EL158
01062      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
01063                                                                   EL158
01064      EJECT                                                        EL158
01065  5000-FIND-NEXT-FORM-RECORD.                                      EL158
01066                                                                   EL158
01067      MOVE LOW-VALUES                 TO  ERFORM-KEY.              EL158
01068      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.       EL158
01069                                                                   EL158
01070      IF STATEL IS GREATER THAN +0                                 EL158
01071          MOVE STATEI                 TO  ERFORM-STATE.            EL158
01072      IF FORML IS GREATER THAN +0                                  EL158
01073          MOVE FORMI                  TO  ERFORM-FORM-ID.          EL158
01074      IF EXPDTL IS GREATER THAN +0                                 EL158
01075          MOVE EXPDTI                 TO  DEEDIT-FIELD             EL158
01076          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01077          IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999               EL158
01078              MOVE HIGH-VALUES        TO  ERFORM-EXP-DT            EL158
01079          ELSE                                                     EL158
01080              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY       EL158
01081              MOVE '4'                TO  DC-OPTION-CODE           EL158
01082              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL158
01083              IF NO-CONVERSION-ERROR                               EL158
01084                  MOVE DC-BIN-DATE-1  TO  ERFORM-EXP-DT            EL158
01085              ELSE                                                 EL158
01086                  MOVE LOW-VALUES     TO  ERFORM-EXP-DT.           EL158
01087                                                                   EL158
01088      EXEC CICS HANDLE CONDITION                                   EL158
01089          ENDFILE (5000-UNSUCCESSFUL-SEARCH)                       EL158
01090      END-EXEC.                                                    EL158
01091                                                                   EL158
01092      EXEC CICS STARTBR                                            EL158
01093          DATASET   (ERFORM-FILE-ID)                               EL158
01094          RIDFLD    (ERFORM-KEY)                                   EL158
01095          GTEQ                                                     EL158
01096      END-EXEC.                                                    EL158
01097                                                                   EL158
01098  5000-READNEXT-LOOP.                                              EL158
01099      EXEC CICS READNEXT                                           EL158
01100          DATASET   (ERFORM-FILE-ID)                               EL158
01101          SET       (ADDRESS OF FORM-MASTER)                          CL**3
01102          RIDFLD    (ERFORM-KEY)                                   EL158
01103      END-EXEC.                                                    EL158
01104                                                                   EL158
01105      IF FO-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    EL158
01106          GO TO 5000-UNSUCCESSFUL-SEARCH.                          EL158
01107                                                                   EL158
01108      IF ERFORM-KEY IS EQUAL TO PI-PREV-FORM-KEY                   EL158
01109          GO TO 5000-READNEXT-LOOP.                                EL158
01110                                                                   EL158
01111      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL158
01112                                                                   EL158
01113  5000-END-BROWSE.                                                 EL158
01114                                                                   EL158
01115      EXEC CICS ENDBR                                              EL158
01116          DATASET   (ERFORM-FILE-ID)                               EL158
01117      END-EXEC.                                                    EL158
01118                                                                   EL158
01119  5000-UNSUCCESSFUL-SEARCH.                                        EL158
01120                                                                   EL158
01121      PERFORM 5000-END-BROWSE.                                     EL158
01122      MOVE -1                         TO  PFKEYL.                  EL158
01123      MOVE ER-0130                    TO  EMI-ERROR.               EL158
01124      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
01125      GO TO 1000-SHOW-FORM-RECORD.                                 EL158
01126                                                                   EL158
01127      EJECT                                                        EL158
01128  5100-FIND-PREV-FORM-RECORD.                                      EL158
01129                                                                   EL158
01130      MOVE LOW-VALUES                 TO  ERFORM-KEY.              EL158
01131      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.       EL158
01132      MOVE PI-PREV-FORM-KEY           TO  ERFORM-KEY.              EL158
01133                                                                   EL158
01134      IF STATEL IS GREATER THAN +0                                 EL158
01135          MOVE STATEI                 TO  ERFORM-STATE.            EL158
01136      IF FORML IS GREATER THAN +0                                  EL158
01137          MOVE FORMI                  TO  ERFORM-FORM-ID.          EL158
01138      IF EXPDTL IS GREATER THAN +0                                 EL158
01139          MOVE EXPDTI                 TO  DEEDIT-FIELD             EL158
01140          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01141          IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999               EL158
01142              MOVE HIGH-VALUES        TO  ERFORM-EXP-DT            EL158
01143          ELSE                                                     EL158
01144              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY       EL158
01145              MOVE '4'                TO  DC-OPTION-CODE           EL158
01146              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL158
01147              IF NO-CONVERSION-ERROR                               EL158
01148                  MOVE DC-BIN-DATE-1  TO  ERFORM-EXP-DT            EL158
01149              ELSE                                                 EL158
01150                  MOVE LOW-VALUES     TO  ERFORM-EXP-DT.           EL158
01151                                                                   EL158
01152      EXEC CICS HANDLE CONDITION                                   EL158
01153          ENDFILE (5100-UNSUCCESSFUL-SEARCH)                       EL158
01154      END-EXEC.                                                    EL158
01155                                                                   EL158
01156      EXEC CICS STARTBR                                            EL158
01157          DATASET   (ERFORM-FILE-ID)                               EL158
01158          RIDFLD    (ERFORM-KEY)                                   EL158
01159          GTEQ                                                     EL158
01160      END-EXEC.                                                    EL158
01161                                                                   EL158
01162  5100-READPREV-LOOP.                                              EL158
01163      EXEC CICS READPREV                                           EL158
01164          DATASET   (ERFORM-FILE-ID)                               EL158
01165          SET       (ADDRESS OF FORM-MASTER)                          CL**3
01166          RIDFLD    (ERFORM-KEY)                                   EL158
01167      END-EXEC.                                                    EL158
01168                                                                   EL158
01169      IF FO-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    EL158
01170          GO TO 5100-UNSUCCESSFUL-SEARCH.                          EL158
01171                                                                   EL158
01172      IF ERFORM-KEY IS EQUAL TO PI-PREV-FORM-KEY                   EL158
01173          GO TO 5100-READPREV-LOOP.                                EL158
01174                                                                   EL158
01175      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL158
01176                                                                   EL158
01177  5100-END-BROWSE.                                                 EL158
01178      EXEC CICS ENDBR                                              EL158
01179          DATASET   (ERFORM-FILE-ID)                               EL158
01180      END-EXEC.                                                    EL158
01181                                                                   EL158
01182  5100-UNSUCCESSFUL-SEARCH.                                        EL158
01183                                                                   EL158
01184      PERFORM 5100-END-BROWSE.                                     EL158
01185      MOVE -1                         TO  PFKEYL.                  EL158
01186      MOVE ER-0131                    TO  EMI-ERROR.               EL158
01187      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
01188      GO TO 1000-SHOW-FORM-RECORD.                                    CL**4
01189                                                                      CL**4
01190      EJECT                                                           CL**4
01191  5200-FIND-NEXT-BENEFIT.                                             CL**4
01192                                                                      CL**4
01193      IF EL158-BENE-INIT (1)  EQUAL WS-INIT-VALUE (1)                 CL**4
01194          MOVE WS-INIT-VALUE  (21) TO WS158-BENE-INIT                 CL**4
01195      ELSE                                                            CL**4
01196          MOVE WS-INIT-VALUE  (21) TO WS158-BENE-INIT                 CL**4
01197          MOVE ER-8150             TO EMI-ERROR                       CL**4
01198          MOVE -1                  TO  PFKEYL                         CL**4
01199          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
01200                                                                      CL**4
01201      GO TO 1000-SHOW-FORM-RECORD.                                    CL**4
01202                                                                      CL**4
01203                                                                      CL**4
01204  5200-FIND-PREV-BENEFIT.                                             CL**4
01205                                                                      CL**4
01206      IF EL158-BENE-INIT (1)  EQUAL WS-INIT-VALUE (21)                CL**4
01207          MOVE WS-INIT-VALUE  (1)  TO WS158-BENE-INIT                 CL**4
01208      ELSE                                                            CL**4
01209          MOVE WS-INIT-VALUE  (1)  TO WS158-BENE-INIT                 CL**4
01210          MOVE ER-8150             TO EMI-ERROR                       CL**4
01211          MOVE -1                  TO  PFKEYL                         CL**4
01212          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
01213                                                                      CL**4
01214      GO TO 1000-SHOW-FORM-RECORD.                                 EL158
01215                                                                   EL158
01216      EJECT                                                        EL158
01217  6000-EDIT-INPUT-DATA.                                            EL158
01218                                                                   EL158
01219      IF MAINTI IS EQUAL TO 'A'                                    EL158
01220          IF STATEL IS EQUAL TO +0                                 EL158
01221              MOVE ER-0144            TO  EMI-ERROR                EL158
01222              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01223              MOVE -1                 TO  STATEL                   EL158
01224              MOVE AL-UABON           TO  STATEA                   EL158
01225          ELSE                                                     EL158
01226              MOVE SPACES             TO  ELCNTL-KEY               EL158
01227              MOVE PI-COMPANY-ID      TO  ELCNTL-COMPANY-ID        EL158
01228              MOVE '3'                TO  ELCNTL-RECORD-TYPE       EL158
01229              MOVE STATEI             TO  ELCNTL-STATE-CD          EL158
01230              MOVE +0                 TO  ELCNTL-SEQUENCE-NO       EL158
01231              PERFORM 8000-READ-CNTL THRU 8010-EXIT                EL158
01232              IF CNTL-RECORD-FOUND                                 EL158
01233                  MOVE AL-UANON       TO  STATEA                   EL158
01234              ELSE                                                 EL158
01235                  MOVE ER-0144        TO  EMI-ERROR                EL158
01236                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL158
01237                  MOVE -1             TO  STATEL                   EL158
01238                  MOVE AL-UABON       TO  STATEA.                  EL158
01239                                                                   EL158
01240      IF MAINTI IS EQUAL TO 'A'                                    EL158
01241          IF FORML IS EQUAL TO +0                                  EL158
01242              MOVE ER-0703            TO  EMI-ERROR                EL158
01243              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01244              MOVE -1                 TO  FORML                    EL158
01245              MOVE AL-UABON           TO  FORMA                    EL158
01246          ELSE                                                     EL158
01247              MOVE AL-UANON           TO  FORMA.                   EL158
01248                                                                   EL158
01249      IF MAINTI IS EQUAL TO 'A'                                    EL158
01250          IF EXPDTL IS EQUAL TO +0                                 EL158
01251              MOVE ER-0704            TO  EMI-ERROR                EL158
01252              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01253              MOVE -1                 TO  EXPDTL                   EL158
01254              MOVE AL-UABON           TO  EXPDTA                   EL158
01255          ELSE                                                     EL158
01256              MOVE EXPDTI             TO  DEEDIT-FIELD             EL158
01257              PERFORM 8600-DEEDIT THRU 8600-EXIT                   EL158
01258              IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999           EL158
01259                  MOVE '99/99/99'     TO  EXPDTO                   EL158
01260                  MOVE AL-UANON       TO  EXPDTA                   EL158
01261              ELSE                                                 EL158
01262                  MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY       EL158
01263                  MOVE '4'            TO  DC-OPTION-CODE           EL158
01264                  MOVE +0             TO  DC-ELAPSED-DAYS          EL158
01265                                          DC-ELAPSED-MONTHS        EL158
01266                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT    EL158
01267                  IF NO-CONVERSION-ERROR                           EL158
01268                      MOVE DC-GREG-DATE-1-EDIT TO  EXPDTO          EL158
01269                      MOVE AL-UANON   TO  EXPDTA                   EL158
01270                  ELSE                                             EL158
01271                      MOVE ER-0705    TO  EMI-ERROR                EL158
01272                      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXITEL158
01273                      MOVE -1         TO  EXPDTL                   EL158
01274                      MOVE AL-UABON   TO  EXPDTA.                  EL158
01275                                                                   EL158
01276      IF MAINTI = 'C'                                              EL158
01277         IF INDGRPL IS GREATER THAN +0                             EL158
01278             IF INDGRPI IS EQUAL TO 'I' OR 'G'                     EL158
01279                 MOVE AL-UANON        TO  INDGRPA                  EL158
01280             ELSE                                                  EL158
01281                 MOVE ER-7031         TO  EMI-ERROR                EL158
01282                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          EL158
01283                 MOVE -1              TO  INDGRPL                  EL158
01284                 MOVE AL-UABON        TO  INDGRPA.                 EL158
01285                                                                   EL158
01286      IF MAINTI = 'A'                                              EL158
01287         IF INDGRPL IS GREATER THAN +0                             EL158
01288             IF INDGRPI IS EQUAL TO 'I' OR 'G'                     EL158
01289                 MOVE AL-UANON        TO  INDGRPA                  EL158
01290             ELSE                                                  EL158
01291                 MOVE ER-7031         TO  EMI-ERROR                EL158
01292                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          EL158
01293                 MOVE -1              TO  INDGRPL                  EL158
01294                 MOVE AL-UABON        TO  INDGRPA                  EL158
01295         ELSE                                                      EL158
01296             MOVE ER-7031         TO  EMI-ERROR                    EL158
01297             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL158
01298             MOVE -1              TO  INDGRPL                      EL158
01299             MOVE AL-UABON        TO  INDGRPA.                     EL158
01300                                                                   EL158
01301      IF MAXATTL IS GREATER THAN +0                                EL158
01302          MOVE MAXATTI                TO  DEEDIT-FIELD             EL158
01303          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01304          MOVE DEEDIT-FIELD-V0        TO  MAXATTI                  EL158
01305          MOVE AL-UNNON               TO  MAXATTA.                 EL158
01306                                                                   EL158
01307      IF DEFTYPL IS GREATER THAN +0                                EL158
01308 *       MOVE DEFTYPI                TO  DEEDIT-FIELD              EL158
01309 *       PERFORM 8600-DEEDIT THRU 8600-EXIT                        EL158
01310 *       MOVE DEEDIT-FIELD-V0        TO  WS-DISABILITY-CODES       EL158
01311         MOVE DEFTYPI                TO  WS-DISABILITY-CODES       EL158
01312         IF VALID-DISABILITY-CODE                                  EL158
01313 *          MOVE DEEDIT-FIELD-V0     TO  WS-DEFTYP                 EL158
01314            MOVE AL-UNNON            TO  DEFTYPA                   EL158
01315         ELSE                                                      EL158
01316            MOVE ER-0706             TO  EMI-ERROR                 EL158
01317            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01318            MOVE -1                  TO  DEFTYPL                   EL158
01319            MOVE AL-UNBON            TO  DEFTYPA.                  EL158
01320                                                                   EL158
01321      IF DISMCDL IS GREATER THAN +0                                EL158
01322         IF DISMCDI IS EQUAL TO 'Y' OR 'N'                         EL158
01323            MOVE AL-UANON            TO  DISMCDA                   EL158
01324          ELSE                                                     EL158
01325            MOVE ER-0707             TO  EMI-ERROR                 EL158
01326            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01327            MOVE -1                  TO  DISMCDL                   EL158
01328            MOVE AL-UABON            TO  DISMCDA.                  EL158
01329                                                                   EL158
01330      IF CRTAPPL IS GREATER THAN +0                                EL158
01331        IF CRTAPPI IS EQUAL TO 'Y' OR 'N'                          EL158
01332            MOVE AL-UANON           TO  CRTAPPA                    EL158
01333        ELSE                                                       EL158
01334            MOVE ER-0708            TO  EMI-ERROR                  EL158
01335            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01336            MOVE -1                 TO  CRTAPPL                    EL158
01337            MOVE AL-UABON           TO  CRTAPPA.                   EL158
01338                                                                   EL158
01339      IF LFMINL IS GREATER THAN +0                                 EL158
01340          MOVE LFMINI                 TO  DEEDIT-FIELD             EL158
01341          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01342          MOVE DEEDIT-FIELD-V0        TO  LFMINI                   EL158
01343                                          WS-LFMIN                 EL158
01344          MOVE AL-UNNON               TO  LFMINA.                  EL158
01345                                                                   EL158
01346      IF MAINTI = 'A'                                              EL158
01347         IF MAXATTL NOT GREATER THAN +0                            EL158
01348            MOVE ER-0718              TO  EMI-ERROR                EL158
01349            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01350            MOVE -1                   TO  MAXATTL                  EL158
01351            MOVE AL-UABON             TO  MAXATTA.                 EL158
01352                                                                   EL158
01353      IF LFMAXL IS GREATER THAN +0                                 EL158
01354          MOVE LFMAXI                 TO  DEEDIT-FIELD             EL158
01355          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01356          MOVE DEEDIT-FIELD-V0        TO  LFMAXI                   EL158
01357                                          WS-LFMAX                 EL158
01358          MOVE AL-UNNON               TO  LFMAXA.                  EL158
01359                                                                   EL158
01360      IF LFTRML IS GREATER THAN +0                                 EL158
01361          MOVE LFTRMI                 TO  DEEDIT-FIELD             EL158
01362          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01363          MOVE DEEDIT-FIELD-V0        TO  LFTRMI                   EL158
01364                                          WS-LFTRM                 EL158
01365          MOVE AL-UNNON               TO  LFTRMA.                  EL158
01366                                                                   EL158
01367      IF LFAMTL IS GREATER THAN +0                                 EL158
01368          MOVE LFAMTI                 TO  DEEDIT-FIELD-V2          EL158
01369          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01370          IF DEEDIT-FIELD-V0 IS GREATER THAN +999999999            EL158
01371              MOVE ER-0709            TO  EMI-ERROR                EL158
01372              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01373              MOVE -1                 TO  LFAMTL                   EL158
01374              MOVE AL-UNBON           TO  LFAMTA                   EL158
01375          ELSE                                                     EL158
01376              MOVE DEEDIT-FIELD-V2    TO  LFAMTI                   EL158
01377                                          WS-LFAMT                 EL158
01378              MOVE AL-UNNON           TO  LFAMTA.                  EL158
01379                                                                   EL158
01380      IF LFMINL = +0 AND                                           EL158
01381         LFMAXL = +0 AND                                           EL158
01382         LFTRML = +0 AND                                           EL158
01383         LFAMTL = +0 AND                                           EL158
01384         AHMINL = +0 AND                                           EL158
01385         AHMAXL = +0 AND                                           EL158
01386         AHTRML = +0 AND                                           EL158
01387         AHAMTL = +0                                               EL158
01388         MOVE ER-0729                 TO  EMI-ERROR                EL158
01389         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL158
01390         MOVE -1                      TO  LFMINL.                  EL158
01391                                                                   EL158
01392      IF LFMINL = +0 AND                                           EL158
01393         LFMAXL = +0 AND                                           EL158
01394         LFTRML = +0 AND                                           EL158
01395         LFAMTL = +0                                               EL158
01396         GO TO 6000-EDIT-LF-PREEXS.                                EL158
01397                                                                   EL158
01398      IF MAINTI = 'A'                                              EL158
01399         IF LFMINL NOT GREATER THAN +0                             EL158
01400            MOVE ER-0719              TO  EMI-ERROR                EL158
01401            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01402            MOVE -1                   TO  LFMINL                   EL158
01403            MOVE AL-UABON             TO  LFMINA.                  EL158
01404                                                                   EL158
01405      IF MAINTI = 'A'                                              EL158
01406         IF LFMAXL NOT GREATER THAN +0                             EL158
01407            MOVE ER-0720              TO  EMI-ERROR                EL158
01408            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01409            MOVE -1                   TO  LFMAXL                   EL158
01410            MOVE AL-UABON             TO  LFMAXA.                  EL158
01411                                                                   EL158
01412      IF MAINTI = 'A'                                              EL158
01413         IF LFTRML NOT GREATER THAN +0                             EL158
01414            MOVE ER-0721              TO  EMI-ERROR                EL158
01415            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01416            MOVE -1                   TO  LFTRML                   EL158
01417            MOVE AL-UABON             TO  LFTRMA.                  EL158
01418                                                                   EL158
01419      IF MAINTI = 'A'                                              EL158
01420         IF LFAMTL NOT GREATER THAN +0                             EL158
01421            MOVE ER-0722              TO  EMI-ERROR                EL158
01422            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01423            MOVE -1                   TO  LFAMTL                   EL158
01424            MOVE AL-UABON             TO  LFAMTA.                  EL158
01425                                                                   EL158
01426  6000-EDIT-LF-PREEXS.                                             EL158
01427                                                                   EL158
01428      IF LFPREL IS GREATER THAN +0                                 EL158
01429 *       MOVE LFPREI                 TO  DEEDIT-FIELD              EL158
01430 *       PERFORM 8600-DEEDIT THRU 8600-EXIT                        EL158
01431 *       MOVE DEEDIT-FIELD-V0       TO  WS-PRE-EXIST-CODES         EL158
01432         MOVE LFPREI                TO  WS-PRE-EXIST-CODES         EL158
01433         IF VALID-PRE-EXIST-CODE                                   EL158
01434 *          MOVE DEEDIT-FIELD-V0    TO  WS-LFPRE                   EL158
01435            MOVE AL-UNNON           TO  LFPREA                     EL158
01436         ELSE                                                      EL158
01437            MOVE ER-0710            TO  EMI-ERROR                  EL158
01438            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01439            MOVE -1                 TO  LFPREL                     EL158
01440            MOVE AL-UNBON           TO  LFPREA.                    EL158
01441                                                                   EL158
01442      IF SUICIDEL IS GREATER THAN +0                               EL158
01443 *        MOVE SUICIDEI               TO  DEEDIT-FIELD             EL158
01444 *        PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01445 *        MOVE DEEDIT-FIELD-V0        TO  WS-SUICIDE-CODES         EL158
01446          MOVE SUICIDEI               TO  WS-SUICIDE-CODES         EL158
01447          IF VALID-SUICIDE-CODE                                    EL158
01448              MOVE AL-UANON           TO  SUICIDEA                 EL158
01449 *            MOVE DEEDIT-FIELD-V0    TO  WS-SUICIDE               EL158
01450          ELSE                                                     EL158
01451              MOVE ER-0711            TO  EMI-ERROR                EL158
01452              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01453              MOVE -1                 TO  SUICIDEL                 EL158
01454              MOVE AL-UABON           TO  SUICIDEA.                EL158
01455                                                                   EL158
01456                                                                   EL158
01457      IF AHMINL IS GREATER THAN +0                                 EL158
01458          MOVE AHMINI                 TO  DEEDIT-FIELD             EL158
01459          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01460          MOVE DEEDIT-FIELD-V0        TO  AHMINI                   EL158
01461                                          WS-AHMIN                 EL158
01462          MOVE AL-UNNON               TO  AHMINA.                  EL158
01463                                                                   EL158
01464                                                                   EL158
01465      IF AHMAXL IS GREATER THAN +0                                 EL158
01466          MOVE AHMAXI                 TO  DEEDIT-FIELD             EL158
01467          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01468          MOVE DEEDIT-FIELD-V0        TO  AHMAXI                   EL158
01469                                          WS-AHMAX                 EL158
01470          MOVE AL-UNNON               TO  AHMAXA.                  EL158
01471                                                                   EL158
01472      IF AHTRML IS GREATER THAN +0                                 EL158
01473          MOVE AHTRMI                 TO  DEEDIT-FIELD             EL158
01474          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01475          MOVE DEEDIT-FIELD-V0        TO  AHTRMI                   EL158
01476                                          WS-AHTRM                 EL158
01477          MOVE AL-UNNON               TO  AHTRMA.                  EL158
01478                                                                   EL158
01479      IF AHAMTL IS GREATER THAN +0                                 EL158
01480          MOVE AHAMTI                 TO  DEEDIT-FIELD-V2          EL158
01481          PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01482          IF DEEDIT-FIELD-V0 IS GREATER THAN +9999999              EL158
01483              MOVE ER-0712            TO  EMI-ERROR                EL158
01484              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01485              MOVE -1                 TO  AHAMTL                   EL158
01486              MOVE AL-UNBON           TO  AHAMTA                   EL158
01487          ELSE                                                     EL158
01488              MOVE DEEDIT-FIELD-V2    TO  AHAMTO                   EL158
01489                                          WS-AHAMT                 EL158
01490              MOVE AL-UNNON           TO  AHAMTA.                  EL158
01491                                                                   EL158
01492      IF AHMINL = +0 AND                                           EL158
01493         AHMAXL = +0 AND                                           EL158
01494         AHTRML = +0 AND                                           EL158
01495         AHAMTL = +0                                               EL158
01496         GO TO 6000-EDIT-AH-PREEXS.                                EL158
01497                                                                   EL158
01498      IF MAINTI = 'A'                                              EL158
01499         IF AHMINL NOT GREATER THAN +0                             EL158
01500            MOVE ER-0723              TO  EMI-ERROR                EL158
01501            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01502            MOVE -1                   TO  AHMINL                   EL158
01503            MOVE AL-UABON             TO  AHMINA.                  EL158
01504                                                                   EL158
01505      IF MAINTI = 'A'                                              EL158
01506         IF AHMAXL NOT GREATER THAN +0                             EL158
01507            MOVE ER-0724              TO  EMI-ERROR                EL158
01508            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01509            MOVE -1                   TO  AHMAXL                   EL158
01510            MOVE AL-UABON             TO  AHMAXA.                  EL158
01511                                                                   EL158
01512      IF MAINTI = 'A'                                              EL158
01513         IF AHTRML NOT GREATER THAN +0                             EL158
01514            MOVE ER-0725              TO  EMI-ERROR                EL158
01515            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01516            MOVE -1                   TO  AHTRML                   EL158
01517            MOVE AL-UABON             TO  AHTRMA.                  EL158
01518                                                                   EL158
01519      IF MAINTI = 'A'                                              EL158
01520         IF AHAMTL NOT GREATER THAN +0                             EL158
01521            MOVE ER-0726              TO  EMI-ERROR                EL158
01522            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL158
01523            MOVE -1                   TO  AHAMTL                   EL158
01524            MOVE AL-UABON             TO  AHAMTA.                  EL158
01525                                                                   EL158
01526  6000-EDIT-AH-PREEXS.                                             EL158
01527                                                                   EL158
01528      IF AHPREL IS GREATER THAN +0                                 EL158
01529 *        MOVE AHPREI                 TO  DEEDIT-FIELD             EL158
01530 *        PERFORM 8600-DEEDIT THRU 8600-EXIT                       EL158
01531 *        MOVE DEEDIT-FIELD-V0        TO  WS-PRE-EXIST-CODES       EL158
01532          MOVE AHPREI                 TO  WS-PRE-EXIST-CODES       EL158
01533          IF VALID-PRE-EXIST-CODE                                  EL158
01534              MOVE AL-UNNON           TO  AHPREA                   EL158
01535 *            MOVE DEEDIT-FIELD-V0    TO  WS-AHPRE                 EL158
01536          ELSE                                                     EL158
01537              MOVE ER-0710            TO  EMI-ERROR                EL158
01538              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01539              MOVE -1                 TO  AHPREL                   EL158
01540              MOVE AL-UNBON           TO  AHPREA.                  EL158
01541      MOVE +0                         TO  SUB.                     EL158
01542                                                                   EL158
01543      MOVE 'N'                        TO WS-BENEFIT-SW.            EL158
01544                                                                   EL158
01545      IF EL158-BENE-INIT (1) EQUAL TO '21.'                           CL**4
01546          MOVE 'Y'                    TO  WS-BENEFIT-SW.              CL**4
01547                                                                      CL**4
01548  6000-EDIT-BENEFIT-LOOP.                                          EL158
01549                                                                   EL158
01550                                                                   EL158
01551      ADD +1                          TO  SUB.                     EL158
01552      IF SUB IS GREATER THAN +20                                      CL**4
01553          GO TO 6000-EDIT-FINISHED.                                EL158
01554                                                                   EL158
01555      IF (EL158-BENE-LENGTH (SUB) IS GREATER THAN +0                  CL**4
01556          OR EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0          CL**4
01557          OR EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0)         CL**4
01558          MOVE EL158-BENE (SUB)       TO  WS-BENEFIT-FIELD         EL158
01559          MOVE 'Y'                    TO  WS-BENEFIT-SW            EL158
01560      ELSE                                                         EL158
01561          GO TO 6000-EDIT-BENEFIT-LOOP.                               CL**4
01562                                                                      CL**4
01563      IF EL158-BENE (SUB) IS EQUAL TO SPACES                          CL**4
01564                      AND                                             CL**4
01565        (EL158-BENE-TERM (SUB) IS GREATER THAN +0                     CL**4
01566         OR EL158-BENE-REFM (SUB) IS GREATER THAN +0)                 CL**4
01567          MOVE ER-0713                TO  EMI-ERROR                   CL**4
01568          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01569          MOVE -1               TO  EL158-BENE-LENGTH (SUB)           CL**4
01570          MOVE AL-UABON         TO  EL158-BENE-ATTRB  (SUB)           CL**4
01571          GO TO 6000-EDIT-BENEFIT-LOOP.                            EL158
01572                                                                   EL158
01573      IF MAINTI IS EQUAL TO 'C'                                    EL158
01574          IF EL158-BENE (SUB) IS EQUAL TO SPACES                   EL158
01575              GO TO 6000-EDIT-BENEFIT-LOOP.                        EL158
01576                                                                   EL158
01577      IF WS-BENE-TYPE IS EQUAL TO PI-LIFE-OVERRIDE-L1 OR           EL158
01578         WS-BENE-TYPE IS EQUAL TO PI-AH-OVERRIDE-L1                EL158
01579          NEXT SENTENCE                                            EL158
01580      ELSE                                                         EL158
01581          MOVE ER-0713                TO  EMI-ERROR                EL158
01582          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01583          MOVE -1                     TO  EL158-BENE-LENGTH (SUB)  EL158
01584          MOVE AL-UABON               TO  EL158-BENE-ATTRB  (SUB)  EL158
01585          GO TO 6000-EDIT-BENEFIT-LOOP.                            EL158
01586                                                                   EL158
01587      IF WS-BENE-TYPE IS EQUAL TO PI-LIFE-OVERRIDE-L1              EL158
01588          MOVE SPACES                 TO  ELCNTL-KEY               EL158
01589          MOVE '4'                    TO  ELCNTL-RECORD-TYPE       EL158
01590          MOVE WS-BENE-CODE           TO  ELCNTL-BENE-CD           EL158
01591          MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID        EL158
01592          MOVE ZEROS                  TO  ELCNTL-SEQUENCE-NO       EL158
01593          PERFORM 7100-READ-BENEFIT THRU 7199-EXIT                 EL158
01594          IF CNTL-RECORD-FOUND                                     EL158
01595              MOVE AL-UANON           TO  EL158-BENE-ATTRB (SUB)   EL158
01596          ELSE                                                     EL158
01597              MOVE ER-7123            TO  EMI-ERROR                EL158
01598              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01599              MOVE -1                 TO  EL158-BENE-LENGTH (SUB)  EL158
01600              MOVE AL-UABON           TO  EL158-BENE-ATTRB  (SUB). EL158
01601                                                                   EL158
01602      IF WS-BENE-TYPE IS EQUAL TO PI-AH-OVERRIDE-L1                EL158
01603          MOVE SPACES                 TO  ELCNTL-KEY               EL158
01604          MOVE '5'                    TO  ELCNTL-RECORD-TYPE       EL158
01605          MOVE WS-BENE-CODE           TO  ELCNTL-BENE-CD           EL158
01606          MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID        EL158
01607          MOVE ZEROS                  TO  ELCNTL-SEQUENCE-NO       EL158
01608          PERFORM 7100-READ-BENEFIT THRU 7199-EXIT                 EL158
01609          IF CNTL-RECORD-FOUND                                     EL158
01610              MOVE AL-UANON           TO  EL158-BENE-ATTRB (SUB)   EL158
01611          ELSE                                                     EL158
01612              MOVE ER-7123            TO  EMI-ERROR                EL158
01613              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL158
01614              MOVE -1                 TO  EL158-BENE-LENGTH (SUB)  EL158
01615              MOVE AL-UABON           TO  EL158-BENE-ATTRB  (SUB). EL158
01616                                                                      CL**4
01617      IF EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0              CL**4
01618          MOVE EL158-BENE-TERM (SUB)  TO  DEEDIT-FIELD                CL**4
01619          PERFORM 8600-DEEDIT THRU 8600-EXIT                          CL**4
01620          MOVE DEEDIT-FIELD-V0        TO  EL158-BENE-TERM (SUB)       CL**4
01621          IF (EL158-BENE-TERM (SUB) > 360)  OR                        CL**4
01622             (EL158-BENE-TERM (SUB) < 001)                            CL**4
01623              MOVE ER-2241            TO  EMI-ERROR                   CL**4
01624              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01625              MOVE -1            TO  EL158-BENE-TERM-LENGTH (SUB)     CL**4
01626              MOVE AL-UNBON      TO  EL158-BENE-TERM-ATTRB  (SUB)     CL**4
01627          ELSE                                                        CL**4
01628              MOVE AL-UNNON      TO  EL158-BENE-TERM-ATTRB (SUB).     CL**4
01629                                                                      CL**4
01630      IF EL158-BENE (SUB) NOT EQUAL TO (SPACES AND LOW-VALUES)        CL**4
01631                      AND                                             CL**4
01632         EL158-BENE-TERM (SUB) IS NOT NUMERIC                         CL**4
01633          MOVE ER-2276          TO  EMI-ERROR                         CL**4
01634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01635          MOVE -1            TO  EL158-BENE-TERM-LENGTH (SUB)         CL**4
01636          MOVE AL-UNBON      TO  EL158-BENE-TERM-ATTRB  (SUB).        CL**4
01637                                                                      CL**4
01638      IF EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0              CL**4
01639          MOVE EL158-BENE-REFM (SUB) TO WS-REFUND-METHOD              CL**4
01640          IF VALID-REFUND-METHOD                                      CL**4
01641              MOVE AL-UANON      TO  EL158-BENE-REFM-ATTRB (SUB)      CL**4
01642          ELSE                                                        CL**4
01643              MOVE ER-0582            TO  EMI-ERROR                   CL**4
01644              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01645              MOVE -1            TO  EL158-BENE-REFM-LENGTH (SUB)     CL**4
01646              MOVE AL-UABON      TO  EL158-BENE-REFM-ATTRB  (SUB).    CL**4
01647                                                                   EL158
01648      GO TO 6000-EDIT-BENEFIT-LOOP.                                EL158
01649                                                                   EL158
01650  6000-EDIT-FINISHED.                                              EL158
01651                                                                   EL158
01652      IF LFAMTL GREATER THAN +0 AND                                EL158
01653         WS-LFAMT =  +0         AND                                EL158
01654         AHAMTL GREATER THAN +0 AND                                EL158
01655         WS-AHAMT = +0                                             EL158
01656         MOVE ER-0729                TO  EMI-ERROR                 EL158
01657         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL158
01658         MOVE -1                     TO  LFAMTL                    EL158
01659         MOVE AL-UNBON               TO  LFAMTA.                   EL158
01660                                                                   EL158
01661      IF (WS-LFMIN GREATER THAN +0 OR                              EL158
01662          WS-LFMAX GREATER THAN +0 OR                              EL158
01663          WS-LFTRM GREATER THAN +0 OR                              EL158
01664          WS-LFAMT GREATER THAN +0)                                EL158
01665          AND                                                      EL158
01666          WS-LFMIN = +0                                            EL158
01667          MOVE ER-0729               TO  EMI-ERROR                 EL158
01668          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01669          MOVE -1                    TO  LFMINL                    EL158
01670          MOVE AL-UNBON              TO  LFMINA.                   EL158
01671                                                                   EL158
01672      IF (WS-LFMIN GREATER THAN +0 OR                              EL158
01673          WS-LFMAX GREATER THAN +0 OR                              EL158
01674          WS-LFTRM GREATER THAN +0 OR                              EL158
01675          WS-LFAMT GREATER THAN +0)                                EL158
01676          AND                                                      EL158
01677          WS-LFMAX = +0                                            EL158
01678          MOVE ER-0729               TO  EMI-ERROR                 EL158
01679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01680          MOVE -1                    TO  LFMAXL                    EL158
01681          MOVE AL-UNBON              TO  LFMAXA.                   EL158
01682                                                                   EL158
01683      IF (WS-LFMIN GREATER THAN +0 OR                              EL158
01684          WS-LFMAX GREATER THAN +0 OR                              EL158
01685          WS-LFTRM GREATER THAN +0 OR                              EL158
01686          WS-LFAMT GREATER THAN +0)                                EL158
01687          AND                                                      EL158
01688          WS-LFTRM = +0                                            EL158
01689          MOVE ER-0729               TO  EMI-ERROR                 EL158
01690          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01691          MOVE -1                    TO  LFTRML                    EL158
01692          MOVE AL-UNBON              TO  LFTRMA.                   EL158
01693                                                                   EL158
01694      IF (WS-AHMIN GREATER THAN +0 OR                              EL158
01695          WS-AHMAX GREATER THAN +0 OR                              EL158
01696          WS-AHTRM GREATER THAN +0 OR                              EL158
01697          WS-AHAMT GREATER THAN +0)                                EL158
01698          AND                                                      EL158
01699          WS-AHMIN = +0                                            EL158
01700          MOVE ER-0729               TO  EMI-ERROR                 EL158
01701          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01702          MOVE -1                    TO  AHMINL                    EL158
01703          MOVE AL-UNBON              TO  AHMINA.                   EL158
01704                                                                   EL158
01705      IF (WS-AHMIN GREATER THAN +0 OR                              EL158
01706          WS-AHMAX GREATER THAN +0 OR                              EL158
01707          WS-AHTRM GREATER THAN +0 OR                              EL158
01708          WS-AHAMT GREATER THAN +0)                                EL158
01709          AND                                                      EL158
01710          WS-AHMAX = +0                                            EL158
01711          MOVE ER-0729               TO  EMI-ERROR                 EL158
01712          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01713          MOVE -1                    TO  AHMAXL                    EL158
01714          MOVE AL-UNBON              TO  AHMAXA.                   EL158
01715                                                                   EL158
01716      IF (WS-AHMIN GREATER THAN +0 OR                              EL158
01717          WS-AHMAX GREATER THAN +0 OR                              EL158
01718          WS-AHTRM GREATER THAN +0 OR                              EL158
01719          WS-AHAMT GREATER THAN +0)                                EL158
01720          AND                                                      EL158
01721          WS-AHTRM = +0                                            EL158
01722          MOVE ER-0729               TO  EMI-ERROR                 EL158
01723          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL158
01724          MOVE -1                    TO  AHTRML                    EL158
01725          MOVE AL-UNBON              TO  AHTRMA.                   EL158
01726                                                                   EL158
01727      IF BENEFIT-NOT-FOUND                                         EL158
01728         MOVE ER-0727                 TO  EMI-ERROR                EL158
01729         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL158
01730         MOVE -1                      TO  EL158-BENE-LENGTH (1)    EL158
01731         MOVE AL-UABON                TO  EL158-BENE-ATTRB  (1).   EL158
01732                                                                   EL158
01733  6000-EXIT.                                                       EL158
01734      EXIT.                                                        EL158
01735                                                                   EL158
01736      EJECT                                                        EL158
01737  7000-BUILD-OUTPUT-MAP.                                           EL158
01738                                                                   EL158
01739      MOVE LOW-VALUES                 TO  EL158AO.                 EL158
01740      MOVE PI-COMPANY-CD              TO  PI-PREV-COMPANY-CD.      EL158
01741      MOVE FO-STATE                   TO  STATEO                   EL158
01742                                          PI-PREV-STATE.           EL158
01743      MOVE FO-FORM-ID                 TO  FORMO                    EL158
01744                                          PI-PREV-FORM-ID.         EL158
01745      IF FO-FORM-EXP-DT IS EQUAL TO HIGH-VALUES                    EL158
01746          MOVE '99/99/99'             TO  EXPDTO                   EL158
01747          MOVE HIGH-VALUES            TO  PI-PREV-EXP-DT           EL158
01748      ELSE                                                         EL158
01749          MOVE FO-FORM-EXP-DT         TO  DC-BIN-DATE-1            EL158
01750                                          PI-PREV-EXP-DT           EL158
01751          MOVE ' '                    TO  DC-OPTION-CODE           EL158
01752          MOVE +0                     TO  DC-ELAPSED-DAYS          EL158
01753                                          DC-ELAPSED-MONTHS        EL158
01754          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL158
01755          IF NO-CONVERSION-ERROR                                   EL158
01756              MOVE DC-GREG-DATE-1-EDIT TO EXPDTO                   EL158
01757          ELSE                                                     EL158
01758              MOVE LOW-VALUES         TO  EXPDTO.                  EL158
01759                                                                   EL158
01760      MOVE FO-IND-GRP-CD              TO  INDGRPO.                 EL158
01761      MOVE AL-UANON                   TO  INDGRPA.                 EL158
01762      MOVE FO-MAX-ATT-AGE             TO  MAXATTO.                 EL158
01763      MOVE AL-UNNON                   TO  MAXATTA.                 EL158
01764                                                                   EL158
01765      MOVE FO-LF-MIN-ISSUE-AGE        TO  LFMINO.                  EL158
01766      MOVE AL-UNNON                   TO  LFMINA.                  EL158
01767      MOVE FO-LF-MAX-ISSUE-AGE        TO  LFMAXO.                  EL158
01768      MOVE AL-UNNON                   TO  LFMAXA.                  EL158
01769      MOVE FO-LF-MAX-TERM             TO  LFTRMO.                  EL158
01770      MOVE AL-UNNON                   TO  LFTRMA.                  EL158
01771      MOVE FO-MAX-LF-AMT              TO  LFAMTO.                  EL158
01772      MOVE AL-UNNON                   TO  LFAMTA.                  EL158
01773      MOVE FO-LF-PRE-EXIST-EXCL-TYPE  TO  LFPREO.                  EL158
01774      MOVE AL-UNNON                   TO  LFPREA.                  EL158
01775      MOVE FO-SUICIDE-EXCL-TYPE       TO  SUICIDEO.                EL158
01776      MOVE AL-UNNON                   TO  SUICIDEA.                EL158
01777      MOVE FO-DIS-DEF-TYPE            TO  DEFTYPO.                 EL158
01778      MOVE AL-UANON                   TO  DEFTYPA.                 EL158
01779      MOVE FO-DISMEMBERMENT-CD        TO  DISMCDO.                 EL158
01780      MOVE AL-UANON                   TO  DISMCDA.                 EL158
01781      MOVE FO-APP-CERT-USE-CD         TO  CRTAPPO.                 EL158
01782      MOVE AL-UANON                   TO  CRTAPPA.                 EL158
01783                                                                   EL158
01784      MOVE FO-AH-MIN-ISSUE-AGE        TO  AHMINO.                  EL158
01785      MOVE AL-UNNON                   TO  AHMINA.                  EL158
01786      MOVE FO-AH-MAX-ISSUE-AGE        TO  AHMAXO.                  EL158
01787      MOVE AL-UNNON                   TO  AHMAXA.                  EL158
01788      MOVE FO-AH-MAX-TERM             TO  AHTRMO.                  EL158
01789      MOVE AL-UNNON                   TO  AHTRMA.                  EL158
01790      MOVE FO-MAX-AH-AMT              TO  AHAMTO.                  EL158
01791      MOVE AL-UNNON                   TO  AHAMTA.                  EL158
01792      MOVE FO-AH-PRE-EXIST-EXCL-TYPE  TO  AHPREO.                  EL158
01793      MOVE AL-UNNON                   TO  AHPREA.                  EL158
01794                                                                   EL158
01795      MOVE FO-LAST-MAINT-BY           TO  MAINTBYO                 EL158
01796                                          PI-UPDATE-BY.            EL158
01797      MOVE FO-LAST-MAINT-HHMMSS       TO  TIME-IN                  EL158
01798                                          PI-UPDATE-HHMMSS.        EL158
01799      MOVE TIME-OUT                   TO  MAINTATO.                EL158
01800      MOVE FO-LAST-MAINT-DT           TO  DC-BIN-DATE-1.           EL158
01801      MOVE ' '                        TO  DC-OPTION-CODE.          EL158
01802      MOVE +0                         TO  DC-ELAPSED-DAYS          EL158
01803                                          DC-ELAPSED-MONTHS.       EL158
01804      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL158
01805      IF NO-CONVERSION-ERROR                                       EL158
01806          MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO                 EL158
01807      ELSE                                                         EL158
01808          MOVE LOW-VALUES             TO  MAINTONO.                EL158
01809                                                                   EL158
01810      MOVE +0                         TO  SUB.                     EL158
01811                                                                   EL158
01812      IF WS158-BENE-INIT     EQUAL WS-INIT-VALUE (21)                 CL**4
01813          MOVE +20 TO SUB2                                            CL**4
01814      ELSE                                                            CL**4
01815          MOVE +0  TO SUB2.                                           CL**4
01816                                                                      CL**4
01817  7000-BUILD-BENEFIT-LOOP.                                         EL158
01818                                                                   EL158
01819      ADD +1                          TO  SUB.                     EL158
01820      ADD +1                          TO  SUB2.                       CL**4
01821      IF SUB IS GREATER THAN +20                                      CL**4
01822          GO TO 7000-FINISH-BUILD.                                 EL158
01823                                                                   EL158
01824      MOVE WS-INIT-VALUE (SUB2) TO EL158-BENE-INIT( SUB).             CL**4
01825      MOVE AL-SANON             TO EL158-BENE-INIT-ATTRB (SUB).       CL**4
01826      MOVE FO-PLAN-TYPE  (SUB2)        TO  EL158-BENE-TYPE  (SUB).    CL**4
01827      MOVE FO-PLAN-ID    (SUB2)        TO  EL158-BENE-ID    (SUB).    CL**4
01828      IF FO-PLAN-TYPE (SUB2) GREATER THAN SPACES                      CL**4
01829          MOVE FO-PLAN-TERM  (SUB2)    TO  EL158-BENE-TERM  (SUB)     CL**4
01830      ELSE                                                            CL**4
01831          MOVE SPACES                  TO  EL158-BENE-TERM-R(SUB).    CL**4
01832      MOVE FO-PLAN-REFUND-METHOD(SUB2) TO  EL158-BENE-REFM  (SUB).    CL**4
01833                                                                   EL158
01834      IF FO-PLAN-TYPE   (SUB2) GREATER THAN SPACES                    CL**4
01835         MOVE AL-UANON         TO  EL158-BENE-ATTRB      (SUB)        CL**4
01836         MOVE AL-UNNON         TO  EL158-BENE-TERM-ATTRB (SUB)        CL**4
01837         MOVE AL-UANON         TO  EL158-BENE-REFM-ATTRB (SUB).       CL**4
01838                                                                   EL158
01839      GO TO 7000-BUILD-BENEFIT-LOOP.                               EL158
01840                                                                   EL158
01841  7000-FINISH-BUILD.                                               EL158
01842                                                                   EL158
01843      MOVE FO-COMMENT-LINE-1          TO  DESCO.                   EL158
01844                                                                   EL158
01845      MOVE -1                         TO  MAINTL.                  EL158
01846      MOVE AL-UANON                   TO  STATEA                   EL158
01847                                          FORMA                    EL158
01848                                          EXPDTA.                  EL158
01849      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
01850                                                                   EL158
01851      EJECT                                                        EL158
01852  7100-READ-BENEFIT.                                               EL158
01853                                                                   EL158
01854      EXEC CICS HANDLE CONDITION                                   EL158
01855          NOTFND   (7120-NOT-FOUND)                                EL158
01856      END-EXEC.                                                    EL158
01857                                                                   EL158
01858      EXEC CICS READ                                               EL158
01859          DATASET   (ELCNTL-FILE-ID)                               EL158
01860          RIDFLD    (ELCNTL-KEY)                                   EL158
01861          SET       (ADDRESS OF CONTROL-FILE)                         CL**3
01862          GTEQ                                                     EL158
01863      END-EXEC.                                                    EL158
01864                                                                   EL158
01865      IF PI-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR            EL158
01866         ELCNTL-RECORD-TYPE IS NOT EQUAL TO CF-RECORD-TYPE         EL158
01867          GO TO 7120-NOT-FOUND.                                    EL158
01868                                                                   EL158
01869      MOVE +1                         TO  SUB-1.                   EL158
01870                                                                   EL158
01871  7110-LOOP.                                                       EL158
01872                                                                   EL158
01873      IF SUB-1 IS EQUAL TO +9                                      EL158
01874          GO TO 7120-NOT-FOUND.                                    EL158
01875                                                                   EL158
01876      IF WS-BENE-CODE IS NOT EQUAL TO CF-BENEFIT-CODE (SUB-1)      EL158
01877          ADD +1                      TO  SUB-1                    EL158
01878          GO TO 7110-LOOP.                                         EL158
01879                                                                   EL158
01880      MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.    EL158
01881      GO TO 7199-EXIT.                                             EL158
01882                                                                   EL158
01883  7120-NOT-FOUND.                                                  EL158
01884      MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.    EL158
01885                                                                   EL158
01886  7199-EXIT.                                                       EL158
01887      EXIT.                                                        EL158
01888      EJECT                                                        EL158
01889  8000-READ-CNTL.                                                  EL158
01890                                                                   EL158
01891      EXEC CICS HANDLE CONDITION                                   EL158
01892          NOTFND   (8009-NOTFND)                                   EL158
01893      END-EXEC.                                                    EL158
01894                                                                   EL158
01895      EXEC CICS READ                                               EL158
01896          DATASET   (ELCNTL-FILE-ID)                               EL158
01897          RIDFLD    (ELCNTL-KEY)                                   EL158
01898          SET       (ADDRESS OF CONTROL-FILE)                         CL**3
01899      END-EXEC.                                                    EL158
01900                                                                   EL158
01901      MOVE 'Y'                         TO  WS-CNTL-REC-FOUND-SW.   EL158
01902      GO TO 8010-EXIT.                                             EL158
01903                                                                   EL158
01904  8009-NOTFND.                                                     EL158
01905      MOVE 'N'                         TO  WS-CNTL-REC-FOUND-SW.   EL158
01906                                                                   EL158
01907  8010-EXIT.                                                       EL158
01908      EXIT.                                                        EL158
01909      EJECT                                                        EL158
01910  8100-SEND-INITIAL-MAP.                                           EL158
01911                                                                   EL158
01912      MOVE PI-LIFE-OVERRIDE-L2        TO  LFHDGO.                  EL158
01913      MOVE PI-AH-OVERRIDE-L2          TO  AHHDGO.                  EL158
01914      MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.                EL158
01915      MOVE EIBTIME                    TO  TIME-IN.                 EL158
01916      MOVE SAVE-DATE                  TO  DATEO.                   EL158
01917      MOVE TIME-OUT                   TO  TIMEO.                   EL158
01918                                                                   EL158
01919      EXEC CICS SEND                                               EL158
01920          MAP      (WS-MAP-NAME)                                   EL158
01921          MAPSET   (MAPSET-NAME)                                   EL158
01922          FROM     (EL158AO)                                       EL158
01923          ERASE                                                    EL158
01924          CURSOR                                                   EL158
01925      END-EXEC.                                                    EL158
01926                                                                   EL158
01927      GO TO 9100-RETURN-TRAN.                                      EL158
01928                                                                   EL158
01929      EJECT                                                        EL158
01930  8200-SEND-DATAONLY.                                              EL158
01931                                                                   EL158
01932      MOVE PI-LIFE-OVERRIDE-L2        TO  LFHDGO.                  EL158
01933      MOVE PI-AH-OVERRIDE-L2          TO  AHHDGO.                  EL158
01934      MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.                EL158
01935      MOVE EIBTIME                    TO  TIME-IN.                 EL158
01936      MOVE SAVE-DATE                  TO  DATEO.                   EL158
01937      MOVE TIME-OUT                   TO  TIMEO.                   EL158
01938                                                                   EL158
01939      EXEC CICS SEND                                               EL158
01940          MAP      (WS-MAP-NAME)                                   EL158
01941          MAPSET   (MAPSET-NAME)                                   EL158
01942          FROM     (EL158AO)                                       EL158
01943          DATAONLY                                                 EL158
01944          CURSOR                                                   EL158
01945      END-EXEC.                                                    EL158
01946                                                                   EL158
01947      GO TO 9100-RETURN-TRAN.                                      EL158
01948      EJECT                                                        EL158
01949  8300-SEND-TEXT.                                                  EL158
01950                                                                   EL158
01951      EXEC CICS SEND TEXT                                          EL158
01952          FROM  (LOGOFF-TEXT)                                      EL158
01953          LENGTH(LOGOFF-LENGTH)                                    EL158
01954          ERASE                                                    EL158
01955          FREEKB                                                   EL158
01956      END-EXEC.                                                    EL158
01957                                                                   EL158
01958      EXEC CICS RETURN                                             EL158
01959          END-EXEC.                                                EL158
01960                                                                   EL158
01961      EJECT                                                        EL158
01962 *8400-LOG-JOURNAL-RECORD.                                            CL**3
01963 *    MOVE PI-PROCESSOR-ID            TO  JP-USER-ID.              EL158
01964 *    MOVE ELBENE-FILE-ID             TO  JP-FILE-ID.              EL158
01965 *    MOVE THIS-PGM                   TO  JP-PROGRAM-ID.           EL158
01966 *    EXEC CICS JOURNAL                                            EL158
01967 *         JFILEID   (PI-JOURNAL-FILE-ID)                          EL158
01968 *         JTYPEID   ('EL')                                        EL158
01969 *         FROM      (JOURNAL-RECORD)                              EL158
01970 *         LENGTH    (527)                                         EL158
01971 *         END-EXEC.                                               EL158
01972                                                                   EL158
01973  8600-DEEDIT.                                                     EL158
01974                                                                   EL158
01975      EXEC CICS BIF DEEDIT                                         EL158
01976          FIELD    (DEEDIT-FIELD)                                  EL158
01977          LENGTH   (15)                                            EL158
01978      END-EXEC.                                                    EL158
01979                                                                   EL158
01980  8600-EXIT.                                                       EL158
01981      EXIT.                                                        EL158
01982                                                                   EL158
01983  8800-UNAUTHORIZED-ACCESS.                                        EL158
01984      MOVE UNACCESS-MSG               TO  LOGOFF-MSG.              EL158
01985      GO TO 8300-SEND-TEXT.                                        EL158
01986                                                                   EL158
01987  8810-PF23.                                                       EL158
01988      MOVE EIBAID                     TO  PI-ENTRY-CD-1.           EL158
01989      MOVE XCTL-005                   TO  PGM-NAME.                EL158
01990      GO TO 9300-XCTL.                                             EL158
01991                                                                   EL158
01992  8850-DUPREC.                                                     EL158
01993      MOVE ER-0132                    TO  EMI-ERROR.               EL158
01994      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
01995      MOVE -1                         TO  STATEL.                  EL158
01996      MOVE AL-UABON                   TO  STATEA  FORMA  EXPDTA.   EL158
01997      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
01998                                                                   EL158
01999  8870-NOTOPEN.                                                    EL158
02000                                                                   EL158
02001      MOVE LOW-VALUES                 TO  EL158AO.                 EL158
02002      MOVE -1                         TO  MAINTL.                  EL158
02003      MOVE ER-0701                    TO  EMI-ERROR.               EL158
02004      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
02005      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
02006                                                                   EL158
02007  8880-NOT-FOUND.                                                  EL158
02008      MOVE ER-0702                    TO  EMI-ERROR.               EL158
02009      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL158
02010      MOVE -1                         TO  STATEL.                  EL158
02011      MOVE AL-UABON                   TO  STATEA  FORMA EXPDTA.    EL158
02012      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
02013                                                                   EL158
02014      EJECT                                                        EL158
02015  9100-RETURN-TRAN.                                                EL158
02016      MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.        EL158
02017      MOVE '158A'                     TO  PI-CURRENT-SCREEN-NO.    EL158
02018                                                                   EL158
02019      EXEC CICS RETURN                                             EL158
02020          TRANSID    (TRANS-ID)                                    EL158
02021          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL158
02022          LENGTH     (PI-COMM-LENGTH)                              EL158
02023      END-EXEC.                                                    EL158
02024                                                                   EL158
02025  9200-RETURN-MAIN-MENU.                                           EL158
02026                                                                   EL158
02027      MOVE XCTL-626                   TO  PGM-NAME.                EL158
02028      GO TO 9300-XCTL.                                             EL158
02029                                                                   EL158
02030  9300-XCTL.                                                       EL158
02031      EXEC CICS XCTL                                               EL158
02032          PROGRAM    (PGM-NAME)                                    EL158
02033          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL158
02034          LENGTH     (PI-COMM-LENGTH)                              EL158
02035      END-EXEC.                                                    EL158
02036                                                                   EL158
02037  9400-CLEAR.                                                      EL158
02038      MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.                EL158
02039      GO TO 9300-XCTL.                                             EL158
02040                                                                   EL158
02041  9500-PF12.                                                       EL158
02042      MOVE XCTL-010                   TO  PGM-NAME.                EL158
02043      GO TO 9300-XCTL.                                             EL158
02044                                                                   EL158
02045  9600-PGMID-ERROR.                                                EL158
02046      EXEC CICS HANDLE CONDITION                                   EL158
02047          PGMIDERR   (8300-SEND-TEXT)                              EL158
02048      END-EXEC.                                                    EL158
02049                                                                   EL158
02050      MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.      EL158
02051      MOVE ' '                        TO  PI-ENTRY-CD-1.           EL158
02052      MOVE XCTL-005                   TO  PGM-NAME.                EL158
02053      MOVE PGM-NAME                   TO  LOGOFF-PGM.              EL158
02054      MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.             EL158
02055      GO TO 9300-XCTL.                                             EL158
02056                                                                   EL158
02057      EJECT                                                        EL158
02058  9700-LINK-DATE-CONVERT.                                          EL158
02059      EXEC CICS LINK                                               EL158
02060          PROGRAM    ('ELDATCV')                                   EL158
02061          COMMAREA   (DATE-CONVERSION-DATA)                        EL158
02062          LENGTH     (DC-COMM-LENGTH)                              EL158
02063      END-EXEC.                                                    EL158
02064                                                                   EL158
02065  9700-EXIT.                                                       EL158
02066      EXIT.                                                        EL158
02067                                                                   EL158
02068  9900-ERROR-FORMAT.                                               EL158
02069      IF NOT EMI-ERRORS-COMPLETE                                   EL158
02070          MOVE LINK-001               TO  PGM-NAME                 EL158
02071          EXEC CICS LINK                                           EL158
02072              PROGRAM    (PGM-NAME)                                EL158
02073              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL158
02074              LENGTH     (EMI-COMM-LENGTH)                         EL158
02075          END-EXEC.                                                EL158
02076                                                                   EL158
02077  9900-EXIT.                                                       EL158
02078      EXIT.                                                        EL158
02079                                                                   EL158
02080  9990-ABEND.                                                      EL158
02081      MOVE LINK-004                   TO  PGM-NAME.                EL158
02082      MOVE DFHEIBLK                   TO  EMI-LINE1.               EL158
02083      EXEC CICS LINK                                               EL158
02084          PROGRAM   (PGM-NAME)                                     EL158
02085          COMMAREA  (EMI-LINE1)                                    EL158
02086          LENGTH    (72)                                           EL158
02087      END-EXEC.                                                    EL158
02088                                                                   EL158
02089      GO TO 8100-SEND-INITIAL-MAP.                                 EL158
02090                                                                   EL158
02091      EJECT                                                        EL158
02092  9995-SECURITY-VIOLATION.                                         EL158
02093                              COPY ELCSCTP.                        EL158
02094                                                                   EL158
02095  9995-EXIT.                                                       EL158
02096      EXIT.                                                        EL158
02097                                                                   EL158
