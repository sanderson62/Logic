00001  ID DIVISION.                                                     09/11/98
00002                                                                   EL656
00003  PROGRAM-ID.                 EL656.                                  LV075
00004 *              PROGRAM CONVERTED BY                               EL656
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL656
00006 *              CONVERSION DATE 02/12/96 11:18:33.                 EL656
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL656
00008 *                            VMOD=2.013                              CL*75
00009                                                                   EL656
00010 *AUTHOR.     LOGIC,INC.                                           EL656
00011 *            DALLAS, TEXAS.                                       EL656
00012                                                                   EL656
00013 *DATE-COMPILED.                                                   EL656
00014 *SECURITY.   *****************************************************EL656
00015 *            *                                                   *EL656
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL656
00017 *            *                                                   *EL656
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL656
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL656
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL656
00021 *            *                                                   *EL656
00022 *            *****************************************************EL656
00023 *                                                                 EL656
00024 *REMARKS.    TRANSACTION - EXE1 - RATE MASTER MAINTENANCE.        EL656
00025 *                                                                 EL656
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101501*                              ADJUST REDEFINES EL656AI FILLER
081413* 081413    2013080700002  PEMA ADD JOURNALING OF ERRATE FILE
101501******************************************************************

00027  ENVIRONMENT DIVISION.                                            EL656
00028  DATA DIVISION.                                                   EL656
00029  WORKING-STORAGE SECTION.                                         EL656
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL656
00031  77  FILLER  PIC X(32)  VALUE '*    EL656 WORKING STORAGE     *'. EL656
00032  77  FILLER  PIC X(32)  VALUE '************ V/M 2.013 *********'. EL656
00033                                                                   EL656
00034  01  WS-DATE-AREA.                                                EL656
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL656
00036      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            EL656
00037                                                                   EL656
00038  01  STANDARD-AREAS.                                              EL656
00039      05  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL656
00040      05  WS-MAPNAME          PIC X(8)    VALUE 'EL656A'.          EL656
00041      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL656S'.          EL656
00042      05  TRANS-ID            PIC X(4)    VALUE 'EXE1'.            EL656
00043      05  THIS-PGM            PIC X(8)    VALUE 'EL656'.           EL656
00044      05  PGM-NAME            PIC X(8).                            EL656
00045      05  SUB1                PIC S999    VALUE +0.                EL656
00046      05  SUB2                PIC S99     VALUE +0.                EL656
00047      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.       EL656
00048      05  WS-COMP-CD-R.                                            EL656
00049          10  FILLER          PIC X.                               EL656
00050          10  WS-COMP-CD-X    PIC X.                               EL656
00051      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP. EL656
00052      05  TIME-IN             PIC S9(7).                           EL656
00053      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL656
00054          10  FILLER          PIC X.                               EL656
00055          10  TIME-OUT        PIC 99V99.                           EL656
00056          10  FILLER          PIC XX.                              EL656
00057      05  TIME-MT             PIC S9(7).                           EL656
00058      05  TIME-MT-R  REDEFINES TIME-MT.                            EL656
00059          10  FILLER          PIC X.                               EL656
00060          10  TIME-LMT        PIC 99V99.                           EL656
00061          10  FILLER          PIC XX.                              EL656
00062      05  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL656
00063      05  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL656
00064      05  XCTL-626            PIC X(8)    VALUE 'EL126'.           EL656
00065      05  XCTL-6561           PIC X(8)    VALUE 'EL6561'.          EL656
00066      05  LINK-001            PIC X(8)    VALUE 'EL001'.           EL656
00067      05  LINK-004            PIC X(8)    VALUE 'EL004'.           EL656
00068      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL656
00069                                                                   EL656
00070      05  ER-0000             PIC X(4)    VALUE  '0000'.           EL656
00071      05  ER-0004             PIC X(4)    VALUE  '0004'.           EL656
00072      05  ER-0008             PIC X(4)    VALUE  '0008'.           EL656
00073      05  ER-0029             PIC X(4)    VALUE  '0029'.           EL656
00074      05  ER-0050             PIC X(4)    VALUE  '0050'.           EL656
00075      05  ER-0068             PIC X(4)    VALUE  '0068'.           EL656
00076      05  ER-0070             PIC X(4)    VALUE  '0070'.           EL656
00077      05  ER-0142             PIC X(4)    VALUE  '0142'.           EL656
081413     05  ER-0755             PIC x(4)    value  '0755'.
00078      05  ER-2039             PIC X(4)    VALUE  '2039'.           EL656
00079      05  ER-2055             PIC X(4)    VALUE  '2055'.           EL656
00080      05  ER-2056             PIC X(4)    VALUE  '2056'.           EL656
00081      05  ER-2187             PIC X(4)    VALUE  '2187'.           EL656
00082      05  ER-2189             PIC X(4)    VALUE  '2189'.           EL656
00083      05  ER-2261             PIC X(4)    VALUE  '2261'.           EL656
00084      05  ER-2262             PIC X(4)    VALUE  '2262'.           EL656
00085      05  ER-2263             PIC X(4)    VALUE  '2263'.           EL656
00086      05  ER-2266             PIC X(4)    VALUE  '2266'.           EL656
00087      05  ER-2267             PIC X(4)    VALUE  '2267'.           EL656
00088      05  ER-2268             PIC X(4)    VALUE  '2268'.           EL656
00089      05  ER-2270             PIC X(4)    VALUE  '2270'.           EL656
00090      05  ER-2271             PIC X(4)    VALUE  '2271'.           EL656
00091      05  ER-2272             PIC X(4)    VALUE  '2272'.           EL656
00092      05  ER-2293             PIC X(4)    VALUE  '2293'.           EL656
00093      05  ER-2296             PIC X(4)    VALUE  '2296'.           EL656
00094      05  ER-2395             PIC X(4)    VALUE  '2395'.           EL656
00095      05  ER-2396             PIC X(4)    VALUE  '2396'.           EL656
00096      05  ER-7743             PIC X(4)    VALUE  '7743'.           EL656
00097                                                                   EL656
00098      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.          EL656
00099      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.          EL656
00100      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.          EL656
00101      05  FILE-ID             PIC X(8)    VALUE  SPACES.           EL656
00102      05  BIN-CURRENT-SAVE    PIC XX      VALUE  SPACES.           EL656
00103                                                                   EL656
00104      05  WS-HOLD-RATE-EXP-AL PIC X(11).                           EL656
00105      05  WS-HOLD-RATE-EXP-DT REDEFINES                            EL656
00106          WS-HOLD-RATE-EXP-AL PIC 9(11).                           EL656
00107                                                                   EL656
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.                    
081413         88  WS-RESP-NORMAL              VALUE +00.               
081413         88  WS-RESP-ERROR               VALUE +01.               
081413         88  WS-RESP-NOTFND              VALUE +13.               
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.
081413
00108  01  MISC-WORK-AREAS.                                             EL656
00109      05  WS-PHONE-IN         PIC 9(10).                           EL656
00110      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.                    EL656
00111          10  WSPI-AREA       PIC XXX.                             EL656
00112          10  WSPI-PFX        PIC XXX.                             EL656
00113          10  WSPI-SFX        PIC X(4).                            EL656
00114      05  WS-PHONE-OUT.                                            EL656
00115          10  WSPO-AREA       PIC XXX.                             EL656
00116          10  FILLER          PIC X       VALUE '-'.               EL656
00117          10  WSPO-PFX        PIC XXX.                             EL656
00118          10  FILLER          PIC X       VALUE '-'.               EL656
00119          10  WSPO-SFX        PIC X(4).                            EL656
00120                                                                   EL656
00121      05  DEEDIT-FIELD            PIC X(15).                       EL656
00122      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL656
00123      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9(1). EL656
00124      05  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V9(2). EL656
00125      05  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V9(3). EL656
00126      05  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4). EL656
00127      05  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5). EL656
00128      05  W-FIELD-V0          PIC S9(15).                          EL656
00129      05  W-FIELD-V1          PIC S9(14)V9(1).                     EL656
00130      05  W-FIELD-V2          PIC S9(13)V9(2).                     EL656
00131      05  W-FIELD-V3          PIC S9(12)V9(3).                     EL656
00132      05  W-FIELD-V4          PIC S9(11)V9(4).                     EL656
00133      05  W-FIELD-V5          PIC S9(10)V9(5).                     EL656
00134      05  W-DECIMAL-NUMBER    PIC S9(01).                          EL656
00135                                                                   EL656
00136      05  WS-SAVE-FACTOR      PIC S999V99    VALUE +0.             EL656
00137      05  WS-DEVIATE-FACTOR   PIC S9V9999    VALUE +0.             EL656
00138      05  WS-NEW-RATE         PIC S99V9(5) VALUE +0.               EL656
00139                                                                   EL656
00140      05  ERRATE-LENGTH       PIC S9(4)      VALUE +1765 COMP.     EL656
00141      05  SV-CLMTOL           PIC 999V99     VALUE ZEROS.          EL656
00142      05  DATE-TEST-AREA      PIC 9(6).                            EL656
00143      05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              EL656
00144          10  DATE-TEST-MM    PIC 99.                              EL656
00145          10  DATE-TEST-DD    PIC 99.                              EL656
00146          10  DATE-TEST-YY    PIC 99.                              EL656
00147      05  DIVIDE-RESULT       PIC 99.                              EL656
00148      05  DIVIDE-REMAINDER    PIC 9.                               EL656
00149      05  WS-PREV-AGE         PIC 99       VALUE ZERO.             EL656
00150      05  WS-PREV-TRM         PIC 999      VALUE ZERO.             EL656
00151      05  WS-PREV-AMT         PIC S9(7)V99 VALUE ZERO.             EL656
00152      05  WS-SAVE-KEY         PIC X(28)    VALUE SPACES.           EL656
00153      05  WS-SAVE-STRUCTURE   PIC X(7)     VALUE SPACES.           EL656
00154                                                                   EL656
00155      05  WS-STATE-FOUND-SW   PIC X        VALUE SPACE.            EL656
00156          88  STATE-FOUND                  VALUE 'Y'.              EL656
00157      05  WS-BENEFIT-FOUND-SW PIC X        VALUE SPACE.            EL656
00158          88  BENEFIT-FOUND                VALUE 'Y'.              EL656
00159      05  WS-FIRST-TIME-SW    PIC X        VALUE 'Y'.              EL656
00160          88  FIRST-TIME                   VALUE 'Y'.              EL656
00161      05  WS-FIRST-REC-SW     PIC X        VALUE 'Y'.              EL656
00162          88  FIRST-REC-IN-STRUCTURE       VALUE 'Y'.              EL656
00163      05  WS-SHOW-PLAN-SW     PIC X        VALUE SPACE.            EL656
00164          88  SHOW-AH-PLANS                VALUE '1'.              EL656
00165          88  SHOW-LF-PLANS                VALUE '2'.              EL656
00166      05  WS-EOF-DISPLAY-SW   PIC X        VALUE 'N'.              EL656
00167          88  EOF-HIT-DISPLAY-FIRST        VALUE 'Y'.              EL656
00168                                                                   EL656
00169      05  WS-CURRENT-EXPIRE       PIC 9(6)    VALUE 999999.        EL656
00170                                                                   EL656
00171      05  WS-ACCESS.                                               EL656
00172          10  WS-STATE-CODE       PIC XX      VALUE SPACES.        EL656
00173          10  FILLER              PIC XX      VALUE SPACES.        EL656
00174      05  WS-REC-TYPE             PIC X       VALUE SPACE.         EL656
00175      05  WS-BENEFIT-ACCESS.                                       EL656
00176          10  FILLER              PIC XX      VALUE SPACES.        EL656
00177          10  WS-HI-BENEFIT       PIC XX      VALUE SPACES.        EL656
00178              88  INVALID-PLAN-CODE  VALUE '  ' '00'               EL656
00179                                           '90' THRU '99'.         EL656
00180                                                                   EL656
00181      05  ELCNTL-KEY.                                              EL656
00182          10  CNTL-COMP-ID        PIC XXX     VALUE SPACES.        EL656
00183          10  CNTL-REC-TYPE       PIC X       VALUE SPACES.        EL656
00184          10  CNTL-ACCESS         PIC X(4)    VALUE SPACES.        EL656
00185          10  CNTL-SEQ-NO         PIC S9(4)   VALUE +0  COMP.      EL656
00186                                                                   EL656
00187      05  MISC-SAVE-AREAS.                                         EL656
00188          10  WS-MAX-AGE               PIC 99.                     EL656
00189          10  WS-L-MORT-CODE           PIC X(4).                   EL656
00190          10  WS-EXCEPTIONS            OCCURS 8 TIMES.             EL656
00191              15  WS-LIMIT-AGE         PIC 99.                     EL656
00192              15  WS-LIMIT-TERM        PIC S999 COMP-3.            EL656
00193              15  WS-LIMIT-MO-BEN      PIC S9(7) COMP-3.           EL656
00194              15  WS-LIMIT-TO-BEN      PIC S9(7) COMP-3.           EL656
00195                                                                   EL656
00196  EJECT                                                            EL656
00197                                    COPY ELCSCTM.                  EL656
00198  EJECT                                                            EL656
00199                                    COPY ELCSCRTY.                 EL656
00200      EJECT                                                        EL656
00201                                    COPY ELCDATE.                  EL656
00202      EJECT                                                        EL656
00203                                    COPY ELCLOGOF.                 EL656
00204      EJECT                                                        EL656
00205                                    COPY ELCATTR.                  EL656
00206      EJECT                                                        EL656
00207                                    COPY ELCEMIB.                  EL656
00208      EJECT                                                        EL656
00209                                    COPY ELCINTF.                  EL656
00210      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL656
00211          16  PI-FILE-ID                    PIC XX.                EL656
00212          16  PI-MAINT                      PIC X.                 EL656
00213              88  VALID-MAINT-TYPE                VALUE 'S' 'A' 'C'EL656
00214                                                    'D' 'P' 'E'.   EL656
00215              88  ADD-FUNCTION                    VALUE 'A'.       EL656
00216              88  SHOW-FUNCTION                   VALUE 'S'.       EL656
00217              88  DELETE-FUNCTION                 VALUE 'D'.       EL656
00218              88  CHANGE-FUNCTION                 VALUE 'C'.       EL656
00219              88  COPY-FUNCTION                   VALUE 'P'.       EL656
00220              88  DEVIATE-FUNCTION                VALUE 'E'.       EL656
00221                                                                   EL656
00222          16  PI-ERRATE-KEY.                                       EL656
00223              20  PI-RATE-COMPANY-CD         PIC X.                EL656
00224              20  PI-RATE-STATE-CODE.                              EL656
00225                  24  PI-RATE-CODE           PIC XX.               EL656
00226                  24  PI-RATE-CLASS          PIC XX.               EL656
00227                  24  PI-RATE-DEV            PIC XXX.              EL656
00228              20  PI-RATE-L-AH-CODE.                               EL656
00229                  24  PI-RATE-L-AH           PIC X.                EL656
00230                  24  PI-RATE-LAH-NUM        PIC XX.               EL656
00231              20  PI-RATE-LIMITS.                                  EL656
00232                  24  PI-RATE-HIGH-AGE       PIC 99.               EL656
00233                  24  PI-RATE-HIGH-AMT       PIC 9(6).             EL656
00234                  24  PI-RATE-FUTURE         PIC XX.               EL656
00235                  24  PI-RATE-SEX            PIC X.                EL656
00236              20  PI-RATE-EXPIRY-DATE        PIC 9(11) COMP-3.     EL656
00237                                                                   EL656
00238          16  PI-SAVE-ERRATE-KEY             PIC X(28).            EL656
00239                                                                   EL656
00240          16  PI-BROWSE-SW                  PIC X.                 EL656
00241              88  BROWSE-STARTED                  VALUE 'Y'.       EL656
00242          16  PI-ERRATE-EOF-SW              PIC X.                 EL656
00243              88  ERRATE-EOF                      VALUE 'Y'.       EL656
00244          16  PI-SHOW-SW                    PIC X.                 EL656
00245              88  SHOWN-ONCE                      VALUE 'Y'.       EL656
00246          16  PI-RETURN-SW                  PIC X.                 EL656
00247              88  RETURN-FROM-LIMITS              VALUE 'Y'.       EL656
00248          16  PI-FIRST-TIME-SW              PIC X.                 EL656
00249              88  FIRST-PLAN-SHOWN                VALUE 'Y'.       EL656
081413         16  pi-delete-sw                  pic x.
081413             88  FIRST-REQUEST VALUE ' '.
081413             88  DELETE-OK     VALUE 'Y'.
081413         16  FILLER                        PIC X(575).            EL656
00251                                                                   EL656
00252      EJECT                                                        EL656
00253                              COPY ELCJPFX.                        EL656
081413                             PIC X(2000).                         EL656
00255                                                                   EL656
00256      EJECT                                                        EL656
00257                              COPY ELCAID.                         EL656
00258                                                                   EL656
00259  01  FILLER    REDEFINES DFHAID.                                  EL656
00260      05  FILLER              PIC X(8).                            EL656
00261      05  PF-VALUES           PIC X       OCCURS 2.                EL656
00262                                                                   EL656
00263      EJECT                                                        EL656
00264                             COPY EL656S.                          EL656
00265                                                                   EL656
00266  01  EL656AO-R   REDEFINES EL656AI.                               EL656
101501     05  FILLER             PIC X(221).                           EL656
00268      05  PLAN-TABLE         OCCURS 24 TIMES                       EL656
00269                             INDEXED BY PT-INDX.                   EL656
00270          10  PT-CODE-L      PIC S9(4)         COMP.               EL656
00271          10  PT-CODE-A      PIC X.                                EL656
00272          10  PT-CODE        PIC XX.                               EL656
00273                                                                   EL656
00274          10  PT-DESC-L      PIC S9(4)         COMP.               EL656
00275          10  PT-DESC-A      PIC X.                                EL656
00276          10  PT-DESC        PIC X(10).                            EL656
00277                                                                   EL656
00278          10  PT-AGE-L       PIC S9(4)         COMP.               EL656
00279          10  PT-AGE-A       PIC X.                                EL656
00280          10  PT-AGE         PIC 99.                               EL656
00281                                                                   EL656
00282          10  PT-EXPIRE-L    PIC S9(4)         COMP.               EL656
00283          10  PT-EXPIRE-A    PIC X.                                EL656
00284          10  PT-EXPIRE      PIC 9(8).                                CL*73
00285      05  FILLER             PIC X(128).                           EL656
00286                                                                   EL656
00287      EJECT                                                        EL656
00288                                                                   EL656
00289  LINKAGE SECTION.                                                 EL656
00290                                                                   EL656
00291  01  DFHCOMMAREA             PIC X(1024).                         EL656
00292                                                                   EL656
00293 *01 PARMLIST .                                                    EL656
00294 *    02  FILLER              PIC S9(8)   COMP.                    EL656
00295 *    02  ERRATE-POINTER      PIC S9(8)   COMP.                    EL656
00296 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    EL656
00297      EJECT                                                        EL656
00298                              COPY ERCRATE.                        EL656
00299      EJECT                                                        EL656
00300                              COPY ELCCNTL.                        EL656
00301      EJECT                                                        EL656
00302                                                                   EL656
00303  PROCEDURE DIVISION.                                              EL656
00304                                                                   EL656
00305      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL656
00306      MOVE '5'                   TO DC-OPTION-CODE.                EL656
00307      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL656
00308      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL656
00309      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL656
00310                                                                   EL656
00311      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL656
00312                                                                   EL656
00313  0100-START.                                                      EL656
00314      IF EIBCALEN = 0                                              EL656
00315          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL656
00316                                                                   EL656
00317      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL656
00318          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL656
00319              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL656
00320              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL656
00321              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL656
00322              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL656
00323              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL656
00324              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL656
00325              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL656
00326              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL656
00327          ELSE                                                     EL656
00328              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL656
00329              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL656
00330              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL656
00331              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL656
00332              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL656
00333              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL656
00334              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL656
00335              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL656
00336                                                                   EL656
00337      EXEC CICS HANDLE CONDITION                                   EL656
00338          NOTOPEN  (9990-ABEND)                                    EL656
00339          NOTFND   (8880-NOT-FOUND)                                EL656
00340          PGMIDERR (9600-PGMID-ERROR)                              EL656
00341          ERROR    (9990-ABEND)                                    EL656
00342      END-EXEC.                                                    EL656
00343                                                                   EL656
00344      IF PI-FILE-ID = 'OE'                                         EL656
00345         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.                     EL656
00346                                                                   EL656
00347      IF EIBTRNID NOT = TRANS-ID                                   EL656
00348          MOVE LOW-VALUES TO EL656AI                               EL656
00349          MOVE ZEROS      TO MISC-SAVE-AREAS
081413         move ' '                to pi-delete-sw
00350          IF RETURN-FROM-LIMITS                                    EL656
00351              GO TO 3000-BUILD-SCREEN-A                            EL656
00352          ELSE                                                     EL656
00353              GO TO 8100-SEND-INITIAL-MAP.                         EL656
00354                                                                   EL656
00355      IF EIBAID = DFHCLEAR                                         EL656
00356          GO TO 9400-CLEAR.                                        EL656
00357                                                                   EL656
00358      IF PI-PROCESSOR-ID = 'LGXX'                                  EL656
00359          GO TO 0200-RECEIVE.                                      EL656
00360                                                                   EL656
00361      EXEC CICS  READQ TS                                          EL656
00362          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL656
00363          INTO    (SECURITY-CONTROL)                               EL656
00364          LENGTH  (SC-COMM-LENGTH)                                 EL656
00365          ITEM    (SC-ITEM)                                        EL656
00366      END-EXEC.                                                    EL656
00367                                                                   EL656
00368      MOVE SC-CREDIT-DISPLAY (06)  TO  PI-DISPLAY-CAP.             EL656
00369      MOVE SC-CREDIT-UPDATE  (06)  TO  PI-MODIFY-CAP.              EL656
00370                                                                   EL656
00371      IF NOT DISPLAY-CAP                                           EL656
00372          MOVE 'READ'          TO SM-READ                          EL656
00373          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL656
00374          MOVE ER-0070        TO  EMI-ERROR                        EL656
00375          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
00376          GO TO 8100-SEND-INITIAL-MAP.                             EL656
00377                                                                   EL656
00378      EJECT                                                        EL656
00379                                                                   EL656
00380  0200-RECEIVE.                                                    EL656
00381      MOVE LOW-VALUES TO EL656AI.                                  EL656
00382                                                                   EL656
00383      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL656
00384          MOVE ER-0008 TO EMI-ERROR                                EL656
00385          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
00386          MOVE -1   TO MAINTL                                      EL656
00387          GO TO 8200-SEND-DATAONLY.                                EL656
00388                                                                   EL656
00389      EXEC CICS RECEIVE                                            EL656
00390          MAP    (WS-MAPNAME)                                      EL656
00391          MAPSET (WS-MAPSET-NAME)                                  EL656
00392          INTO   (EL656AI)                                         EL656
00393      END-EXEC.                                                    EL656
00394                                                                   EL656
00395      IF PFENTERL = 0                                              EL656
00396          GO TO 0300-CHECK-PFKEYS.                                 EL656
00397                                                                   EL656
00398      IF EIBAID NOT = DFHENTER                                     EL656
00399          MOVE ER-0004 TO EMI-ERROR                                EL656
00400          GO TO 0310-INPUT-ERROR.                                  EL656
00401                                                                   EL656
00402      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)   EL656
00403          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL656
00404      ELSE                                                         EL656
00405          MOVE ER-0029 TO EMI-ERROR                                EL656
00406          GO TO 0310-INPUT-ERROR.                                  EL656
00407                                                                   EL656
00408  0300-CHECK-PFKEYS.                                               EL656
00409      IF EIBAID = DFHPF23                                          EL656
00410          GO TO 8810-PF23.                                         EL656
00411                                                                   EL656
00412      IF EIBAID = DFHPF24                                          EL656
00413          GO TO 9200-RETURN-MAIN-MENU.                             EL656
00414                                                                   EL656
00415      IF EIBAID = DFHPF12                                          EL656
00416          GO TO 9500-PF12.                                         EL656
00417                                                                   EL656
00418      IF MAINTL GREATER ZERO                                       EL656
00419          IF MAINTI NOT = SPACE                                    EL656
00420              IF EIBAID NOT = DFHENTER                             EL656
00421                  MOVE -1      TO MAINTL                           EL656
00422                  MOVE ER-0050 TO EMI-ERROR                        EL656
00423                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL656
00424                  GO TO 8200-SEND-DATAONLY.                        EL656
00425                                                                   EL656
00426      IF EIBAID = DFHPF1                                           EL656
00427          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL656
00428          GO TO 7100-NEXT-STRUCTURE.                               EL656
00429                                                                   EL656
00430      IF EIBAID = DFHPF2                                           EL656
00431          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL656
00432          GO TO 7200-PRIOR-STRUCTURE.                              EL656
00433                                                                   EL656
00434      IF EIBAID = DFHPF3                                           EL656
00435          GO TO 7100-NEXT-STRUCTURE.                               EL656
00436                                                                   EL656
00437      IF EIBAID = DFHPF4                                           EL656
00438          GO TO 7200-PRIOR-STRUCTURE.                              EL656
00439                                                                   EL656
00440      IF EIBAID = DFHPF5                                           EL656
00441          MOVE '1' TO WS-SHOW-PLAN-SW                              EL656
00442          MOVE 'S' TO MAINTI                                       EL656
00443          MOVE +1  TO MAINTL                                       EL656
00444          GO TO 0400-EDIT-MAINT.                                   EL656
00445                                                                   EL656
00446      IF EIBAID = DFHPF6                                           EL656
00447          MOVE '2' TO WS-SHOW-PLAN-SW                              EL656
00448          MOVE 'S' TO MAINTI                                       EL656
00449          MOVE +1  TO MAINTL                                       EL656
00450          GO TO 0400-EDIT-MAINT.                                   EL656
00451                                                                   EL656
00452      IF EIBAID = DFHENTER                                         EL656
00453          GO TO 0400-EDIT-MAINT.                                   EL656
00454                                                                   EL656
00455      MOVE ER-0029 TO EMI-ERROR.                                   EL656
00456                                                                   EL656
00457  0310-INPUT-ERROR.                                                EL656
00458      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
00459                                                                   EL656
00460      MOVE AL-UNBON TO PFENTERA.                                   EL656
00461      MOVE -1       TO PFENTERL.                                   EL656
00462                                                                   EL656
00463      GO TO 8200-SEND-DATAONLY.                                    EL656
00464                                                                   EL656
00465      EJECT                                                        EL656
00466  0400-EDIT-MAINT.                                                 EL656
00467      MOVE SPACES        TO PI-ERRATE-KEY.                         EL656
00468      MOVE PI-COMPANY-CD TO PI-RATE-COMPANY-CD.                    EL656
00469                                                                   EL656
00470      MOVE ALL  '9'      TO PI-RATE-SEX                            EL656
00471                            PI-RATE-FUTURE.                        EL656
00472                                                                   EL656
00473      MOVE ZEROS         TO PI-RATE-LAH-NUM                        EL656
00474                            PI-RATE-EXPIRY-DATE.                   EL656
00475                                                                   EL656
00476      IF MAINTL GREATER ZERO                                       EL656
00477          MOVE MAINTI TO PI-MAINT                                  EL656
00478          IF VALID-MAINT-TYPE                                      EL656
00479              MOVE AL-UANON TO MAINTA                              EL656
00480          ELSE                                                     EL656
00481              MOVE -1       TO MAINTL                              EL656
00482              MOVE AL-UABON TO MAINTA                              EL656
00483              MOVE ER-2039  TO EMI-ERROR                           EL656
00484              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
00485      ELSE                                                         EL656
00486          MOVE -1           TO MAINTL                              EL656
00487          MOVE AL-UABON     TO MAINTA                              EL656
00488          MOVE ER-2039   TO EMI-ERROR                              EL656
00489          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
00490                                                                   EL656
00491      IF STATE1L GREATER ZERO                                      EL656
00492          MOVE SPACES       TO WS-ACCESS                           EL656
00493          MOVE STATE1I      TO WS-STATE-CODE                       EL656
00494          PERFORM 7400-EDIT-STATE THRU 7499-EXIT                   EL656
00495          IF STATE-FOUND                                           EL656
00496              MOVE AL-UANON TO STATE1A                             EL656
00497              MOVE STATE1I  TO PI-RATE-CODE                        EL656
00498          ELSE                                                     EL656
00499              MOVE -1       TO STATE1L                             EL656
00500              MOVE AL-UABON TO STATE1A                             EL656
00501              MOVE ER-2261  TO EMI-ERROR                           EL656
00502              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
00503      ELSE                                                         EL656
00504          MOVE -1           TO STATE1L                             EL656
00505          MOVE AL-UABON     TO STATE1A                             EL656
00506          MOVE ER-2261   TO EMI-ERROR                              EL656
00507          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
00508                                                                   EL656
00509      IF CLASS1L GREATER ZERO                                      EL656
00510          MOVE AL-UANON     TO CLASS1A                             EL656
00511          MOVE CLASS1I      TO PI-RATE-CLASS                       EL656
00512      ELSE                                                         EL656
00513          IF ADD-FUNCTION                                          EL656
00514              MOVE ZEROS    TO CLASS1O                             EL656
00515                               PI-RATE-CLASS                       EL656
00516              MOVE AL-UANON TO CLASS1A                             EL656
00517          ELSE                                                     EL656
00518              MOVE -1       TO CLASS1L                             EL656
00519              MOVE AL-UABON TO CLASS1A                             EL656
00520              MOVE ER-2262  TO EMI-ERROR                           EL656
00521              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL656
00522                                                                   EL656
00523      IF RTAGE1L GREATER ZERO                                      EL656
00524         MOVE RTAGE1I       TO  DEEDIT-FIELD                       EL656
00525         PERFORM 7500-DEEDIT THRU 7500-EXIT                        EL656
00526         IF DEEDIT-FIELD-V0 GREATER ZERO                           EL656
00527            MOVE AL-UNNON   TO RTAGE1A                             EL656
00528            MOVE RTAGE1I    TO PI-RATE-HIGH-AGE                    EL656
00529         ELSE                                                      EL656
00530            MOVE -1         TO RTAGE1L                             EL656
00531            MOVE AL-UNBON   TO RTAGE1A                             EL656
00532            MOVE ER-2187 TO EMI-ERROR                              EL656
00533            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL656
00534      ELSE                                                         EL656
00535          IF ADD-FUNCTION                                          EL656
00536              MOVE ALL '9'  TO RTAGE1O                             EL656
00537                               PI-RATE-HIGH-AGE                    EL656
00538              MOVE AL-UNNON TO RTAGE1A                             EL656
00539          ELSE                                                     EL656
00540            MOVE -1         TO RTAGE1L                             EL656
00541            MOVE AL-UNBON   TO RTAGE1A                             EL656
00542            MOVE ER-2187 TO EMI-ERROR                              EL656
00543            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL656
00544                                                                   EL656
00545      IF RTAMT1L GREATER ZERO                                      EL656
00546         MOVE RTAMT1I       TO  DEEDIT-FIELD                       EL656
00547         PERFORM 7500-DEEDIT THRU 7500-EXIT                        EL656
00548         IF DEEDIT-FIELD-V0 GREATER ZERO                           EL656
00549            MOVE AL-UNNON   TO RTAMT1A                             EL656
00550            MOVE DEEDIT-FIELD-V0 TO PI-RATE-HIGH-AMT               EL656
00551         ELSE                                                      EL656
00552            MOVE -1         TO RTAMT1L                             EL656
00553            MOVE AL-UNBON   TO RTAMT1A                             EL656
00554            MOVE ER-2189 TO EMI-ERROR                              EL656
00555            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL656
00556      ELSE                                                         EL656
00557          IF ADD-FUNCTION                                          EL656
00558              MOVE ALL '9'  TO RTAMT1O                             EL656
00559                               PI-RATE-HIGH-AMT                    EL656
00560              MOVE AL-UNNON TO RTAMT1A                             EL656
00561          ELSE                                                     EL656
00562            MOVE -1         TO RTAMT1L                             EL656
00563            MOVE AL-UNBON   TO RTAMT1A                             EL656
00564            MOVE ER-2189 TO EMI-ERROR                              EL656
00565            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL656
00566                                                                   EL656
00567      IF DEV1L GREATER ZERO                                        EL656
00568          MOVE AL-UANON     TO DEV1A                               EL656
00569          MOVE DEV1I        TO PI-RATE-DEV                         EL656
00570      ELSE                                                         EL656
00571          IF ADD-FUNCTION                                          EL656
00572              MOVE AL-UANON TO DEV1A                               EL656
00573              MOVE ZEROS    TO DEV1O                               EL656
00574                               PI-RATE-DEV                         EL656
00575          ELSE                                                     EL656
00576              MOVE -1       TO DEV1L                               EL656
00577              MOVE AL-UABON TO DEV1A                               EL656
00578              MOVE ER-2263  TO EMI-ERROR                           EL656
00579              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL656
00580                                                                   EL656
00581      IF TYPE1L GREATER ZERO                                       EL656
00582          IF TYPE1I = PI-LIFE-OVERRIDE-L1 OR PI-AH-OVERRIDE-L1     EL656
00583              MOVE AL-UANON TO TYPE1A                              EL656
00584              MOVE TYPE1I   TO PI-RATE-L-AH                        EL656
00585          ELSE                                                     EL656
00586              MOVE -1       TO TYPE1L                              EL656
00587              MOVE AL-UABON TO TYPE1A                              EL656
00588              MOVE ER-2267  TO EMI-ERROR                           EL656
00589              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
00590      ELSE                                                         EL656
00591          MOVE -1           TO TYPE1L                              EL656
00592          MOVE AL-UABON     TO TYPE1A                              EL656
00593          MOVE ER-2267   TO EMI-ERROR                              EL656
00594          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
00595                                                                   EL656
00596      IF TYPE1I = PI-LIFE-OVERRIDE-L1                              EL656
00597          MOVE '4'          TO WS-REC-TYPE                         EL656
00598      ELSE                                                         EL656
00599          MOVE '5'          TO WS-REC-TYPE.                        EL656
00600                                                                   EL656
00601      IF PLAN1L GREATER ZERO                                       EL656
00602          MOVE PLAN1I       TO WS-HI-BENEFIT                       EL656
00603          PERFORM 7300-EDIT-BENEFIT THRU 7300-EXIT                 EL656
00604      ELSE                                                         EL656
00605          MOVE -1           TO PLAN1L                              EL656
00606          MOVE AL-UABON     TO PLAN1A                              EL656
00607          MOVE ER-2268   TO EMI-ERROR                              EL656
00608          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
00609                                                                   EL656
00610      IF BENEFIT-FOUND                                             EL656
00611          MOVE PLAN1I       TO PI-RATE-LAH-NUM                     EL656
00612          MOVE AL-UANON     TO PLAN1A                              EL656
00613      ELSE                                                         EL656
00614          MOVE -1           TO PLAN1L                              EL656
00615          MOVE AL-UABON     TO PLAN1A                              EL656
00616          MOVE ER-2268   TO EMI-ERROR                              EL656
00617          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
00618                                                                   EL656
00619      IF EXPIRE1L GREATER ZERO                                     EL656
00620          IF EXPIRE1I NUMERIC                                      EL656
00621              IF EXPIRE1I = WS-CURRENT-EXPIRE                      EL656
00622                  MOVE AL-UNNON TO EXPIRE1A                        EL656
00623                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT             EL656
00624                  MOVE EXPIRE1I TO WS-HOLD-RATE-EXP-AL(6:6)        EL656
00625                                   DC-GREG-DATE-1-YMD              EL656
00626                  MOVE '3'      TO DC-OPTION-CODE                  EL656
00627                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT    EL656
00628                  IF NO-CONVERSION-ERROR                           EL656
00629                      MOVE DC-ALPHA-CENTURY TO                     EL656
00630                           WS-HOLD-RATE-EXP-AL(4:2)                EL656
00631                      MOVE WS-HOLD-RATE-EXP-DT TO                  EL656
00632                                               PI-RATE-EXPIRY-DATE EL656
00633                  ELSE                                             EL656
00634                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999         EL656
00635                          MOVE 99999999999         TO              EL656
00636                               WS-HOLD-RATE-EXP-DT                 EL656
00637                          MOVE WS-HOLD-RATE-EXP-DT TO              EL656
00638                                               PI-RATE-EXPIRY-DATE EL656
00639                      ELSE                                         EL656
00640                          MOVE -1       TO EXPIRE1L                EL656
00641                          MOVE AL-UNBON TO EXPIRE1A                EL656
00642                          MOVE ER-2296  TO EMI-ERROR               EL656
00643                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT EL656
00644                      END-IF                                       EL656
00645                  END-IF                                           EL656
00646              ELSE                                                 EL656
00647                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT             EL656
00648                  MOVE EXPIRE1I TO DC-GREG-DATE-1-YMD              EL656
00649                                   WS-HOLD-RATE-EXP-AL(6:6)        EL656
00650                  MOVE '3'      TO DC-OPTION-CODE                  EL656
00651                  PERFORM 9700-LINK-DATE-CONVERT                   EL656
00652                     THRU 9700-EXIT                                EL656
00653                  IF NO-CONVERSION-ERROR                           EL656
00654                      MOVE AL-UNNON TO EXPIRE1A                    EL656
00655                      MOVE EXPIRE1I TO WS-HOLD-RATE-EXP-AL(6:6)    EL656
00656                                       DC-GREG-DATE-1-YMD          EL656
00657                      MOVE '3'      TO DC-OPTION-CODE              EL656
00658                      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXITEL656
00659                      IF NO-CONVERSION-ERROR                       EL656
00660                          MOVE DC-ALPHA-CENTURY TO                 EL656
00661                               WS-HOLD-RATE-EXP-AL(4:2)            EL656
00662                          MOVE WS-HOLD-RATE-EXP-DT TO              EL656
00663                               PI-RATE-EXPIRY-DATE                 EL656
00664                      ELSE                                         EL656
00665                          IF WS-HOLD-RATE-EXP-DT(6:6) = 999999     EL656
00666                              MOVE 99999999999         TO          EL656
00667                                   WS-HOLD-RATE-EXP-DT             EL656
00668                              MOVE WS-HOLD-RATE-EXP-DT TO          EL656
00669                                               PI-RATE-EXPIRY-DATE EL656
00670                          ELSE                                     EL656
00671                              MOVE -1       TO EXPIRE1L            EL656
00672                              MOVE AL-UNBON TO EXPIRE1A            EL656
00673                              MOVE ER-2296  TO EMI-ERROR           EL656
00674                              PERFORM 9900-ERROR-FORMAT THRU       EL656
00675                                                          9900-EXITEL656
00676                          END-IF                                   EL656
00677                      END-IF                                       EL656
00678                  ELSE                                             EL656
00679                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999         EL656
00680                          MOVE 99999999999         TO              EL656
00681                               WS-HOLD-RATE-EXP-DT                 EL656
00682                          MOVE WS-HOLD-RATE-EXP-DT TO              EL656
00683                                               PI-RATE-EXPIRY-DATE EL656
00684                      ELSE                                         EL656
00685                          MOVE -1       TO EXPIRE1L                EL656
00686                          MOVE AL-UNBON TO EXPIRE1A                EL656
00687                          MOVE ER-2296  TO EMI-ERROR               EL656
00688                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT EL656
00689                      END-IF                                       EL656
00690                  END-IF                                           EL656
00691              END-IF                                               EL656
00692          ELSE                                                     EL656
00693              MOVE -1          TO EXPIRE1L                         EL656
00694              MOVE AL-UNBON    TO EXPIRE1A                         EL656
00695              MOVE ER-2266  TO EMI-ERROR                           EL656
00696              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
00697          END-IF                                                   EL656
00698      ELSE                                                         EL656
00699          IF ADD-FUNCTION                                          EL656
00700              MOVE AL-UNNON    TO EXPIRE1A                         EL656
00701              MOVE ALL '9'     TO EXPIRE1O                         EL656
00702              MOVE ZEROS       TO WS-HOLD-RATE-EXP-DT              EL656
00703              MOVE EXPIRE1I    TO WS-HOLD-RATE-EXP-AL(6:6)         EL656
00704                                  DC-GREG-DATE-1-YMD               EL656
00705              MOVE '3'         TO DC-OPTION-CODE                   EL656
00706              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL656
00707              IF NO-CONVERSION-ERROR                               EL656
00708                  MOVE DC-ALPHA-CENTURY TO                         EL656
00709                       WS-HOLD-RATE-EXP-AL(4:2)                    EL656
00710                  MOVE WS-HOLD-RATE-EXP-DT TO                      EL656
00711                       PI-RATE-EXPIRY-DATE                         EL656
00712              ELSE                                                 EL656
00713                  IF WS-HOLD-RATE-EXP-DT(6:6) = 999999             EL656
00714                      MOVE 99999999999         TO                  EL656
00715                           WS-HOLD-RATE-EXP-DT                     EL656
00716                      MOVE WS-HOLD-RATE-EXP-DT TO                  EL656
00717                                           PI-RATE-EXPIRY-DATE     EL656
00718                  ELSE                                             EL656
00719                      MOVE -1       TO EXPIRE1L                    EL656
00720                      MOVE AL-UNBON TO EXPIRE1A                    EL656
00721                      MOVE ER-2296  TO EMI-ERROR                   EL656
00722                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.       CL*74
00723                                                                   EL656
00724      IF EMI-NO-ERRORS                                             EL656
00725          NEXT SENTENCE                                            EL656
00726      ELSE                                                         EL656
00727          GO TO 8200-SEND-DATAONLY.                                EL656
00728                                                                   EL656
00729      IF NOT MODIFY-CAP                                            EL656
00730          IF SHOW-FUNCTION                                         EL656
00731              NEXT SENTENCE                                        EL656
00732          ELSE                                                     EL656
00733              MOVE 'UPDATE'       TO SM-READ                       EL656
00734              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL656
00735              MOVE ER-0070        TO  EMI-ERROR                    EL656
00736              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
00737              GO TO 8100-SEND-INITIAL-MAP.                         EL656
00738                                                                   EL656
00739      IF ADD-FUNCTION                                              EL656
00740          GO TO 1000-ADD.                                          EL656
00741                                                                   EL656
00742      IF CHANGE-FUNCTION                                           EL656
00743          GO TO 1200-CHANGE.                                       EL656
00744                                                                   EL656
00745      IF COPY-FUNCTION                                             EL656
00746          GO TO 1400-COPY.                                         EL656

081413     IF DELETE-FUNCTION
081413        if delete-ok
081413           move ' '              to pi-delete-sw
081413           GO TO 1600-DELETE
081413        else
081413           SET DELETE-OK         TO TRUE
081413           MOVE ER-0755          TO EMI-ERROR
081413           PERFORM 9900-ERROR-FORMAT
081413                                 THRU 9900-EXIT
081413           move ' '              to mainto
081413           move -1               to maintl
081413           go to 8200-send-dataonly
081413        END-IF
081413     END-IF

00751      IF DEVIATE-FUNCTION                                          EL656
00752          GO TO 1800-DEVIATE.                                      EL656
00753                                                                   EL656
00754  0400-SHOW.                                                       EL656
00755      IF SHOW-FUNCTION                                             EL656
00756          IF WS-SHOW-PLAN-SW NOT = SPACE                           EL656
00757              GO TO 3000-BUILD-SCREEN-A                            EL656
00758          ELSE                                                     EL656
00759              IF SHOWN-ONCE                                        EL656
00760                  IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY            EL656
00761                      MOVE XCTL-6561 TO PGM-NAME                   EL656
00762                      GO TO 9300-XCTL                              EL656
00763                  ELSE                                             EL656
00764                      MOVE SPACE TO PI-SHOW-SW                     EL656
00765                      GO TO 3000-BUILD-SCREEN-A                    EL656
00766              ELSE                                                 EL656
00767                  GO TO 3000-BUILD-SCREEN-A.                       EL656
00768                                                                   EL656
00769  0400-EXIT.                                                       EL656
00770      EXIT.                                                        EL656
00771      EJECT                                                        EL656
00772                                                                   EL656
00773  1000-ADD.                                                        EL656
00774      EXEC CICS HANDLE CONDITION                                   EL656
00775          NOTOPEN  (9990-ABEND)                                    EL656
00776          NOTFND   (1025-ADD-REC)                                  EL656
00777      END-EXEC.                                                    EL656
00778                                                                   EL656
00779      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL656
00780                                                                   EL656
00781      MOVE ER-2270   TO EMI-ERROR.                                 EL656
00782      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
00783      MOVE -1     TO MAINTL.                                       EL656
00784      GO TO 8200-SEND-DATAONLY.                                    EL656
00785                                                                   EL656
00786  1025-ADD-REC.                                                    EL656
00787      PERFORM 7700-ERRATE-GETMAIN THRU 7700-EXIT.                  EL656
00788                                                                   EL656
00789      MOVE SPACES              TO RATE-RECORD.                     EL656
00790      MOVE ZEROS               TO RT-HIGH-AGE  RT-HIGH-AMT         EL656
00791                                  RT-LAH-NUM   RT-EXPIRY-DATE      EL656
00792                                  RT-MAX-AGE                       EL656
00793                                  RT-LAST-MAINT-HHMMSS.            EL656
00794                                                                   EL656
00795      PERFORM 4000-UPDATE-SCREEN-A THRU 4099-EXIT.                 EL656
00796                                                                   EL656
00797      MOVE +1 TO SUB1.                                             EL656
00798                                                                   EL656
00799  1050-ZERO-LIMITS.                                                EL656
00800      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL656
00801          MOVE ZEROS TO RT-L-EX-AGE  (SUB1)                        EL656
00802                        RT-L-EX-TERM (SUB1)                        EL656
00803                        RT-L-EX-FACE (SUB1).                       EL656
00804                                                                   EL656
00805      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL656
00806          MOVE ZEROS TO RT-AH-AGE   (SUB1)                         EL656
00807                        RT-AH-TERM  (SUB1)                         EL656
00808                        RT-AH-BEN-M (SUB1)                         EL656
00809                        RT-AH-BEN-F (SUB1).                        EL656
00810                                                                   EL656
00811      ADD +1 TO SUB1.                                              EL656
00812      IF SUB1 GREATER +8                                           EL656
00813          MOVE +1 TO SUB1                                          EL656
00814      ELSE                                                         EL656
00815          GO TO 1050-ZERO-LIMITS.                                  EL656
00816                                                                   EL656
00817  1075-ZERO-RATES.                                                 EL656
00818      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL656
00819          MOVE ZEROS TO RT-L-RATE (SUB1).                          EL656
00820                                                                   EL656
00821      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL656
00822          MOVE ZEROS TO RT-AH-RATE (SUB1).                         EL656
00823                                                                   EL656
00824      ADD +1 TO SUB1.                                              EL656
00825      IF SUB1 GREATER +360                                         EL656
00826          MOVE +1 TO SUB1                                          EL656
00827      ELSE                                                         EL656
00828          GO TO 1075-ZERO-RATES.                                   EL656
00829                                                                   EL656
00830      MOVE PI-PROCESSOR-ID TO RT-LAST-MAINT-USER.                  EL656
00831      MOVE EIBTIME         TO RT-LAST-MAINT-HHMMSS.                EL656
00832                                                                   EL656
00833      MOVE SAVE-BIN-DATE   TO RT-LAST-MAINT-DT                     EL656
00834                              BIN-CURRENT-SAVE.                    EL656
00835      MOVE PI-COMPANY-CD   TO RT-COMPANY-CD.                       EL656
00836      MOVE 'RT'            TO RT-RECORD-ID.                        EL656
00837      MOVE RATE-FILE-ID    TO FILE-ID.                             EL656
00838      MOVE 'A'             TO JP-RECORD-TYPE.                      EL656
00839                                                                   EL656
00840      EXEC CICS WRITE                                              EL656
00841          DATASET (RATE-FILE-ID)                                   EL656
00842          FROM    (RATE-RECORD)                                    EL656
00843          RIDFLD  (RT-CONTROL-PRIMARY)                             EL656
00844      END-EXEC.                                                    EL656
00845                                                                   EL656
00846      MOVE RATE-RECORD TO JP-RECORD-AREA.                          EL656
00847                                                                   EL656
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00849      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL656
00850      MOVE ER-0000 TO EMI-ERROR.                                   EL656
00851      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
00852                                                                   EL656
00853      MOVE XCTL-6561 TO PGM-NAME.                                  EL656
00854      GO TO 9300-XCTL.                                             EL656
00855                                                                   EL656
00856  1099-EXIT.                                                       EL656
00857      EXIT.                                                        EL656
00858      EJECT                                                        EL656
00859                                                                   EL656
00860  1200-CHANGE.                                                     EL656
00861      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL656
00862          NEXT SENTENCE                                            EL656
00863      ELSE                                                         EL656
00864          MOVE ER-2056  TO EMI-ERROR                               EL656
00865          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
00866          MOVE -1    TO MAINTL                                     EL656
00867          GO TO 8200-SEND-DATAONLY.                                EL656
00868                                                                   EL656
00869      EXEC CICS HANDLE CONDITION                                   EL656
00870          NOTFND   (8880-NOT-FOUND)                                EL656
00871      END-EXEC.                                                    EL656
00872                                                                   EL656
00873      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL656
00874                                                                   EL656
00875      IF EMI-NO-ERRORS                                             EL656
00876          NEXT SENTENCE                                            EL656
00877      ELSE                                                         EL656
00878          GO TO 8200-SEND-DATAONLY.                                EL656
00879                                                                   EL656
00880      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.              EL656
00881                                                                   EL656
00882      MOVE RATE-RECORD TO JP-RECORD-AREA.                          EL656
00883                                                                   EL656
00884      IF DEVIATE-FUNCTION                                          EL656
00885          PERFORM 6000-DEVIATE-RATES THRU 6099-EXIT                EL656
00886      ELSE                                                         EL656
00887          PERFORM 4000-UPDATE-SCREEN-A THRU 4099-EXIT.             EL656
00888                                                                   EL656
00889      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR                    EL656
00890         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL656
00891          NEXT SENTENCE                                            EL656
00892      ELSE                                                         EL656
00893          EXEC CICS UNLOCK                                         EL656
00894               DATASET  (RATE-FILE-ID)                             EL656
00895          END-EXEC                                                 EL656
00896          MOVE ER-0068 TO EMI-ERROR                                EL656
00897          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
00898          GO TO 3000-BUILD-SCREEN-A.                               EL656
00899                                                                   EL656
00900      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.              EL656
00901      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.            EL656
00902                                                                   EL656
00903      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT                 EL656
00904                                  BIN-CURRENT-SAVE.                EL656
00905      MOVE 'B'                 TO JP-RECORD-TYPE                   EL656
00906      MOVE RATE-FILE-ID        TO FILE-ID.                         EL656
081413     PERFORM 8400-LOG-JOURNAL-RECORD                              EL656
081413                                 thru 8400-exit
00908                                                                   EL656
00909      EXEC CICS REWRITE                                            EL656
00910          DATASET  (RATE-FILE-ID)                                  EL656
00911          FROM     (RATE-RECORD)                                   EL656
00912      END-EXEC.                                                    EL656
00913                                                                   EL656
00914      MOVE RATE-RECORD         TO JP-RECORD-AREA.                  EL656
00915      MOVE 'C'                 TO JP-RECORD-TYPE                   EL656
00916      MOVE RATE-FILE-ID        TO FILE-ID.                         EL656
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00918      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL656
00919      MOVE ER-0000 TO EMI-ERROR.                                   EL656
00920      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
00921                                                                   EL656
00922      MOVE XCTL-6561 TO PGM-NAME.                                  EL656
00923      GO TO 9300-XCTL.                                             EL656
00924                                                                   EL656
00925  1299-EXIT.                                                       EL656
00926      EXIT.                                                        EL656
00927      EJECT                                                        EL656
00928                                                                   EL656
00929  1400-COPY.                                                       EL656
00930      MOVE PI-ERRATE-KEY TO PI-SAVE-ERRATE-KEY.                    EL656
00931      MOVE SPACES        TO PI-ERRATE-KEY.                         EL656
00932      MOVE ALL '9'       TO PI-RATE-FUTURE                         EL656
00933                            PI-RATE-SEX.                           EL656
00934                                                                   EL656
00935      PERFORM 5000-EDIT-SCREEN-A THRU 5099-EXIT.                   EL656
00936                                                                   EL656
00937      IF EMI-NO-ERRORS                                             EL656
00938          NEXT SENTENCE                                            EL656
00939      ELSE                                                         EL656
00940          GO TO 8200-SEND-DATAONLY.                                EL656
00941                                                                   EL656
00942      EXEC CICS HANDLE CONDITION                                   EL656
00943          NOTOPEN  (9990-ABEND)                                    EL656
00944          NOTFND   (1450-COPY-REC)                                 EL656
00945      END-EXEC.                                                    EL656
00946                                                                   EL656
00947      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL656
00948                                                                   EL656
00949      MOVE -1    TO MAINTL.                                        EL656
00950      MOVE ER-2270  TO EMI-ERROR.                                  EL656
00951      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
00952                                                                   EL656
00953      GO TO 8200-SEND-DATAONLY.                                    EL656
00954                                                                   EL656
00955  1450-COPY-REC.                                                   EL656
00956      MOVE PI-ERRATE-KEY       TO WS-SAVE-KEY.                     EL656
00957      MOVE PI-SAVE-ERRATE-KEY  TO PI-ERRATE-KEY.                   EL656
00958                                                                   EL656
00959      EXEC CICS HANDLE CONDITION                                   EL656
00960          NOTFND   (8880-NOT-FOUND)                                EL656
00961      END-EXEC.                                                    EL656
00962                                                                   EL656
00963      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL656
00964                                                                   EL656
00965      MOVE WS-SAVE-KEY         TO PI-ERRATE-KEY.                   EL656
00966                                                                   EL656
00967      PERFORM 4000-UPDATE-SCREEN-A THRU 4099-EXIT.                 EL656
00968                                                                   EL656
00969      MOVE PI-PROCESSOR-ID TO RT-LAST-MAINT-USER.                  EL656
00970      MOVE EIBTIME         TO RT-LAST-MAINT-HHMMSS.                EL656
00971                                                                   EL656
00972      MOVE SAVE-BIN-DATE   TO RT-LAST-MAINT-DT                     EL656
00973                              BIN-CURRENT-SAVE.                    EL656
00974      MOVE PI-COMPANY-CD   TO RT-COMPANY-CD.                       EL656
00975      MOVE 'RT'            TO RT-RECORD-ID.                        EL656
00976      MOVE RATE-FILE-ID    TO FILE-ID.                             EL656
00977      MOVE 'A'             TO JP-RECORD-TYPE.                      EL656
00978                                                                   EL656
00979      EXEC CICS WRITE                                              EL656
00980          DATASET (RATE-FILE-ID)                                   EL656
00981          FROM    (RATE-RECORD)                                    EL656
00982          RIDFLD  (RT-CONTROL-PRIMARY)                             EL656
00983      END-EXEC.                                                    EL656
00984                                                                   EL656
00985      MOVE RATE-RECORD TO JP-RECORD-AREA.                          EL656
00986                                                                   EL656
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00988      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL656
00989      MOVE ER-0000 TO EMI-ERROR.                                   EL656
00990      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
00991                                                                   EL656
00992      MOVE XCTL-6561 TO PGM-NAME.                                  EL656
00993      GO TO 9300-XCTL.                                             EL656
00994                                                                   EL656
00995  1499-EXIT.                                                       EL656
00996      EXIT.                                                        EL656
00997      EJECT                                                        EL656
00998                                                                   EL656
00999  1600-DELETE.                                                     EL656
01000      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL656
01001          NEXT SENTENCE                                            EL656
01002      ELSE                                                         EL656
01003          MOVE ER-2056  TO EMI-ERROR                               EL656
01004          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01005          MOVE -1    TO MAINTL                                     EL656
01006          GO TO 8200-SEND-DATAONLY.                                EL656
01007                                                                   EL656
01008      EXEC CICS HANDLE CONDITION                                   EL656
01009          NOTFND   (8880-NOT-FOUND)                                EL656
01010      END-EXEC.                                                    EL656
01011                                                                   EL656
01012      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.              EL656
01013                                                                   EL656
01014      MOVE 'D'                 TO JP-RECORD-TYPE.                  EL656
01015      MOVE RATE-RECORD         TO JP-RECORD-AREA.                  EL656
01016      MOVE RATE-FILE-ID        TO FILE-ID.                         EL656
01017                                                                   EL656
01018      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR                    EL656
01019         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL656
01020          NEXT SENTENCE                                            EL656
01021      ELSE                                                         EL656
01022          EXEC CICS UNLOCK                                         EL656
01023               DATASET  (RATE-FILE-ID)                             EL656
01024          END-EXEC                                                 EL656
01025          MOVE ER-0068 TO EMI-ERROR                                EL656
01026          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01027          GO TO 3000-BUILD-SCREEN-A.                               EL656
01028                                                                   EL656
01029      EXEC CICS DELETE                                             EL656
01030           DATASET  (RATE-FILE-ID)                                 EL656
01031      END-EXEC.                                                    EL656
01032                                                                   EL656
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
01034                                                                   EL656
01035      MOVE SAVE-BIN-DATE  TO BIN-CURRENT-SAVE.                     EL656
01036      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL656
01037      MOVE ER-0000     TO EMI-ERROR.                               EL656
01038      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
01039      MOVE ' '                       TO  PI-SHOW-SW.               EL656
01040                                                                   EL656
01041      MOVE LOW-VALUES TO EL656AO.                                  EL656
01042                                                                   EL656
01043      MOVE PI-RATE-CODE              TO  STATE1O.                  EL656
01044      MOVE PI-RATE-CLASS             TO  CLASS1O.                  EL656
01045      MOVE PI-RATE-DEV               TO  DEV1O.                    EL656
01046      MOVE PI-RATE-L-AH              TO  TYPE1O.                   EL656
01047      MOVE PI-RATE-LAH-NUM           TO  PLAN1O.                   EL656
01048      MOVE PI-RATE-HIGH-AGE          TO  RTAGE1O.                  EL656
01049      MOVE PI-RATE-HIGH-AMT          TO  RTAMT1O.                  EL656
01050      MOVE PI-RATE-EXPIRY-DATE       TO  EXPIRE1O.                 EL656
01051                                                                   EL656
01052      MOVE AL-UANON                  TO  CLASS1A                   EL656
01053                                         TYPE1A                    EL656
01054                                         DEV1A                     EL656
01055                                         STATE1A                   EL656
01056                                         PLAN1A.                   EL656
01057      MOVE AL-UNNON                  TO  RTAGE1A                   EL656
01058                                         RTAMT1A                   EL656
01059                                         EXPIRE1A.                 EL656
01060      GO TO 8100-SEND-INITIAL-MAP.                                 EL656
01061                                                                   EL656
01062  1699-EXIT.                                                       EL656
01063      EXIT.                                                        EL656
01064      EJECT                                                        EL656
01065                                                                   EL656
01066  1800-DEVIATE.                                                    EL656
01067      IF DEV1I = ZEROS                                             EL656
01068         IF PI-PROCESSOR-ID NOT = 'LGXX'                           EL656
01069            MOVE -1                  TO MAINTL                     EL656
01070            MOVE SPACE               TO MAINTO                     EL656
01071            MOVE ER-2396          TO EMI-ERROR                     EL656
01072            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL656
01073                                                                   EL656
01074      IF  DECINUML GREATER ZERO                                    EL656
01075          EXEC CICS BIF DEEDIT                                     EL656
01076              FIELD   (DECINUMI)                                   EL656
01077              LENGTH  (1)                                          EL656
01078              END-EXEC                                             EL656
01079                                                                   EL656
01080          IF  DECINUMI NOT NUMERIC                                 EL656
01081                  OR                                               EL656
01082              DECINUMI GREATER THAN 5                              EL656
01083                  OR                                               EL656
01084              DECINUMI LESS THAN 1                                 EL656
01085              MOVE -1             TO DECINUML                      EL656
01086              MOVE AL-UNBON       TO DECINUMA                      EL656
01087              MOVE ER-7743        TO EMI-ERROR                     EL656
01088              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01089                                                                   EL656
01090          ELSE                                                     EL656
01091              MOVE DECINUMI       TO W-DECIMAL-NUMBER              EL656
01092                                                                   EL656
01093      ELSE                                                         EL656
01094          MOVE +5                 TO DECINUMO                      EL656
01095                                     W-DECIMAL-NUMBER.             EL656
01096                                                                   EL656
01097      IF DEVPCTL GREATER ZERO                                      EL656
01098          MOVE DEVPCTI TO DEEDIT-FIELD                             EL656
01099          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL656
01100          IF DEEDIT-FIELD-V2 GREATER ZERO                          EL656
01101              MOVE DEEDIT-FIELD-V2 TO DEVPCTO                      EL656
01102                                      WS-SAVE-FACTOR               EL656
01103              MOVE AL-UNNON        TO DEVPCTA                      EL656
01104          ELSE                                                     EL656
01105              MOVE -1              TO DEVPCTL                      EL656
01106              MOVE AL-UNBON        TO DEVPCTA                      EL656
01107              MOVE ER-2395      TO EMI-ERROR                       EL656
01108              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01109      ELSE                                                         EL656
01110          MOVE -1                  TO DEVPCTL                      EL656
01111          MOVE AL-UNBON            TO DEVPCTA                      EL656
01112          MOVE ER-2395          TO EMI-ERROR                       EL656
01113          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
01114                                                                   EL656
01115      IF EMI-NO-ERRORS                                             EL656
01116          GO TO 1200-CHANGE                                        EL656
01117      ELSE                                                         EL656
01118          GO TO 8200-SEND-DATAONLY.                                EL656
01119                                                                   EL656
01120  1899-EXIT.                                                       EL656
01121      EXIT.                                                        EL656
01122      EJECT                                                        EL656
01123  3000-BUILD-SCREEN-A.                                             EL656
01124      MOVE LOW-VALUES                    TO  EL656AO.              EL656
01125      MOVE ZEROS                         TO  MISC-SAVE-AREAS.      EL656
01126      MOVE PI-COMPANY-CD                 TO  PI-RATE-COMPANY-CD.   EL656
01127                                                                   EL656
01128      EXEC CICS HANDLE CONDITION                                   EL656
01129          NOTOPEN  (9990-ABEND)                                    EL656
01130          NOTFND   (8880-NOT-FOUND)                                EL656
01131          PGMIDERR (9600-PGMID-ERROR)                              EL656
01132          ERROR    (9990-ABEND)                                    EL656
01133      END-EXEC.                                                    EL656
01134                                                                   EL656
01135      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL656
01136                                                                   EL656
01137      MOVE PI-ERRATE-KEY                 TO PI-SAVE-ERRATE-KEY.    EL656
01138                                                                   EL656
01139  3025-SET-UP-SCREEN-A.                                            EL656
01140      MOVE RT-ST-CODE                    TO STATE1O.               EL656
01141      MOVE RT-ST-CLASS                   TO CLASS1O.               EL656
01142      MOVE RT-HIGH-AGE                   TO RTAGE1O.               EL656
01143      MOVE RT-HIGH-AMT                   TO RTAMT1O.               EL656
01144      MOVE RT-ST-DEV                     TO DEV1O.                 EL656
01145                                                                   EL656
01146      MOVE RT-EXPIRY-DATE                TO WS-HOLD-RATE-EXP-DT.      CL*72
01147      MOVE WS-HOLD-RATE-EXP-AL(6:6)      TO EXPIRE1O.                 CL*72
01148                                                                   EL656
01149      MOVE RT-L-AH                       TO TYPE1O.                EL656
01150      MOVE RT-LAH-NUM                    TO PLAN1O.                EL656
01151                                                                   EL656
01152      MOVE RT-STRUCTURE-COMMENT          TO STCOMMO.               EL656
01153                                                                   EL656
01154      MOVE RT-LAST-MAINT-DT   TO DC-BIN-DATE-1.                    EL656
01155      MOVE SPACE              TO DC-OPTION-CODE.                   EL656
01156      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL656
01157      MOVE DC-GREG-DATE-1-EDIT  TO LUDATEO.                        EL656
01158      MOVE RT-LAST-MAINT-USER   TO LUBYO.                          EL656
01159      MOVE RT-LAST-MAINT-HHMMSS TO TIME-MT.                        EL656
01160      MOVE TIME-LMT             TO LUTIMEO.                        EL656
01161                                                                   EL656
01162      MOVE AL-UANON                      TO CLASS1A                EL656
01163                                            STATE1A                EL656
01164                                            TYPE1A                 EL656
01165                                            DEV1A                  EL656
01166                                            PLAN1A.                EL656
01167                                                                   EL656
01168      MOVE AL-UNNON                      TO RTAGE1A                EL656
01169                                            RTAMT1A                EL656
01170                                            EXPIRE1A.              EL656
01171                                                                   EL656
01172      IF EIBAID = DFHPF1 OR DFHPF2                                 EL656
01173          MOVE AL-UADON                  TO TYPE1A                 EL656
01174                                            PLAN1A                 EL656
01175          MOVE AL-UNDON                  TO RTAGE1A                EL656
01176                                            RTAMT1A                EL656
01177                                            EXPIRE1A.              EL656
01178      MOVE 'Y'       TO PI-SHOW-SW.                                EL656
01179                                                                   EL656
01180      IF WS-SHOW-PLAN-SW NOT = SPACE                               EL656
01181          PERFORM 6500-SHOW-PLANS THRU 6599-EXIT.                  EL656
01182                                                                   EL656
01183      IF RETURN-FROM-LIMITS OR                                     EL656
01184         EIBAID NOT = DFHENTER                                     EL656
01185          NEXT SENTENCE                                            EL656
01186       ELSE                                                        EL656
01187      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL656
01188          MOVE XCTL-6561 TO PGM-NAME                               EL656
01189          GO TO 9300-XCTL.                                         EL656
01190                                                                   EL656
01191      MOVE SPACE TO PI-RETURN-SW.                                  EL656
01192                                                                   EL656
01193      MOVE ER-0000 TO EMI-ERROR.                                   EL656
01194      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
01195                                                                   EL656
01196      GO TO 8100-SEND-INITIAL-MAP.                                 EL656
01197                                                                   EL656
01198  3099-EXIT.                                                       EL656
01199      EXIT.                                                        EL656
01200      EJECT                                                        EL656
01201  4000-UPDATE-SCREEN-A.                                            EL656
01202       IF CHANGE-FUNCTION                                          EL656
01203           GO TO 4025-CONT.                                        EL656
01204                                                                   EL656
01205       MOVE PI-RATE-COMPANY-CD   TO RT-COMPANY-CD.                 EL656
01206                                                                   EL656
01207       MOVE PI-RATE-STATE-CODE   TO RT-STATE-CODE                  EL656
01208                                    RTC-1.                         EL656
01209       MOVE PI-RATE-HIGH-AGE     TO RT-HIGH-AGE.                   EL656
01210                                                                   EL656
01211       MOVE PI-RATE-HIGH-AMT     TO RT-HIGH-AMT.                   EL656
01212                                                                   EL656
01213       MOVE PI-RATE-LIMITS       TO RT-LIMITS                      EL656
01214                                    RTC-3.                         EL656
01215                                                                   EL656
01216       MOVE PI-RATE-L-AH-CODE    TO RT-L-AH-CODE                   EL656
01217                                    RTC-2.                         EL656
01218                                                                   EL656
01219       MOVE ZEROS                TO WS-HOLD-RATE-EXP-DT.              CL*72
01220       MOVE PI-RATE-EXPIRY-DATE  TO WS-HOLD-RATE-EXP-AL            EL656
01221                                    RTC-4.                            CL*72
01222                                                                   EL656
01223       MOVE WS-HOLD-RATE-EXP-DT  TO DC-GREG-DATE-1-YMD.               CL*72
01224       MOVE '3'                  TO DC-OPTION-CODE.                   CL*72
01225       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                 CL*72
01226       IF NO-CONVERSION-ERROR                                      EL656
01227           MOVE DC-ALPHA-CENTURY TO WS-HOLD-RATE-EXP-AL(4:2)       EL656
01228       ELSE                                                        EL656
01229           IF WS-HOLD-RATE-EXP-DT(6:6) = 999999                    EL656
01230               CONTINUE                                            EL656
01231           ELSE                                                    EL656
01232               MOVE ZEROS        TO WS-HOLD-RATE-EXP-AL(4:2).         CL*72
01233                                                                   EL656
01234       MOVE WS-HOLD-RATE-EXP-DT  TO RT-EXPIRY-DATE.                   CL*72
01235                                                                   EL656
01236  4025-CONT.                                                       EL656
01237       IF STCOMML GREATER ZERO                                     EL656
01238           MOVE STCOMMI           TO RT-STRUCTURE-COMMENT.         EL656
01239                                                                   EL656
01240  4099-EXIT.                                                       EL656
01241      EXIT.                                                        EL656
01242      EJECT                                                        EL656
01243  5000-EDIT-SCREEN-A.                                              EL656
01244      IF STATE2L GREATER ZERO                                      EL656
01245          MOVE SPACES       TO WS-ACCESS                           EL656
01246          MOVE STATE2I      TO WS-STATE-CODE                       EL656
01247          PERFORM 7400-EDIT-STATE THRU 7499-EXIT                   EL656
01248          IF STATE-FOUND                                           EL656
01249              MOVE AL-UANON TO STATE2A                             EL656
01250              MOVE STATE2I  TO PI-RATE-CODE                        EL656
01251          ELSE                                                     EL656
01252              MOVE -1       TO STATE2L                             EL656
01253              MOVE AL-UABON TO STATE2A                             EL656
01254              MOVE ER-2261  TO EMI-ERROR                           EL656
01255              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01256      ELSE                                                         EL656
01257          IF COPY-FUNCTION                                         EL656
01258              MOVE AL-UANON TO STATE2A                             EL656
01259              MOVE STATE1I  TO STATE2O                             EL656
01260                               PI-RATE-CODE                        EL656
01261          ELSE                                                     EL656
01262              MOVE -1       TO STATE2L                             EL656
01263              MOVE AL-UABON TO STATE2A                             EL656
01264              MOVE ER-2261  TO EMI-ERROR                           EL656
01265              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL656
01266                                                                   EL656
01267      IF CLASS2L GREATER ZERO                                      EL656
01268          MOVE AL-UANON     TO CLASS2A                             EL656
01269          MOVE CLASS2I      TO PI-RATE-CLASS                       EL656
01270      ELSE                                                         EL656
01271          IF COPY-FUNCTION                                         EL656
01272              MOVE AL-UANON TO CLASS2A                             EL656
01273              MOVE ZEROS    TO CLASS2O                             EL656
01274                               PI-RATE-CLASS                       EL656
01275          ELSE                                                     EL656
01276              MOVE -1       TO CLASS2L                             EL656
01277              MOVE AL-UABON TO CLASS2A                             EL656
01278              MOVE ER-2262  TO EMI-ERROR                           EL656
01279              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL656
01280                                                                   EL656
01281      IF RTAGE2L GREATER ZERO                                      EL656
01282         MOVE RTAGE2I       TO  DEEDIT-FIELD                       EL656
01283         PERFORM 7500-DEEDIT THRU 7500-EXIT                        EL656
01284         IF DEEDIT-FIELD-V0 GREATER ZERO                           EL656
01285            MOVE AL-UNNON   TO RTAGE2A                             EL656
01286            MOVE RTAGE2I    TO PI-RATE-HIGH-AGE                    EL656
01287         ELSE                                                      EL656
01288            MOVE -1         TO RTAGE2L                             EL656
01289            MOVE AL-UNBON   TO RTAGE2A                             EL656
01290            MOVE ER-2187 TO EMI-ERROR                              EL656
01291            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL656
01292      ELSE                                                         EL656
01293          IF COPY-FUNCTION                                         EL656
01294              MOVE ALL '9'  TO RTAGE2O                             EL656
01295                               PI-RATE-HIGH-AGE                    EL656
01296              MOVE AL-UNNON TO RTAGE2A                             EL656
01297          ELSE                                                     EL656
01298            MOVE -1         TO RTAGE2L                             EL656
01299            MOVE AL-UNBON   TO RTAGE2A                             EL656
01300            MOVE ER-2187 TO EMI-ERROR                              EL656
01301            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL656
01302                                                                   EL656
01303      IF RTAMT2L GREATER ZERO                                      EL656
01304         MOVE RTAMT2I       TO  DEEDIT-FIELD                       EL656
01305         PERFORM 7500-DEEDIT THRU 7500-EXIT                        EL656
01306         IF DEEDIT-FIELD-V0 GREATER ZERO                           EL656
01307            MOVE AL-UNNON   TO RTAMT2A                             EL656
01308            MOVE DEEDIT-FIELD-V0 TO PI-RATE-HIGH-AMT               EL656
01309         ELSE                                                      EL656
01310            MOVE -1         TO RTAMT2L                             EL656
01311            MOVE AL-UNBON   TO RTAMT2A                             EL656
01312            MOVE ER-2189 TO EMI-ERROR                              EL656
01313            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL656
01314      ELSE                                                         EL656
01315          IF COPY-FUNCTION                                         EL656
01316              MOVE ALL '9'  TO RTAMT2O                             EL656
01317                               PI-RATE-HIGH-AMT                    EL656
01318              MOVE AL-UNNON TO RTAMT2A                             EL656
01319          ELSE                                                     EL656
01320            MOVE -1         TO RTAMT2L                             EL656
01321            MOVE AL-UNBON   TO RTAMT2A                             EL656
01322            MOVE ER-2189 TO EMI-ERROR                              EL656
01323            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL656
01324                                                                   EL656
01325      IF DEV2L GREATER ZERO                                        EL656
01326          MOVE AL-UANON     TO DEV2A                               EL656
01327          MOVE DEV2I        TO PI-RATE-DEV                         EL656
01328      ELSE                                                         EL656
01329          IF COPY-FUNCTION                                         EL656
01330              MOVE AL-UANON TO DEV2A                               EL656
01331              MOVE ZEROS    TO DEV2O                               EL656
01332                               PI-RATE-DEV                         EL656
01333          ELSE                                                     EL656
01334              MOVE -1       TO DEV2L                               EL656
01335              MOVE AL-UABON TO DEV2A                               EL656
01336              MOVE ER-2263  TO EMI-ERROR                           EL656
01337              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL656
01338                                                                   EL656
01339      IF TYPE2L GREATER ZERO                                       EL656
01340          IF TYPE2I = TYPE1I                                       EL656
01341              MOVE AL-UANON TO TYPE2A                              EL656
01342              MOVE TYPE2I   TO PI-RATE-L-AH                        EL656
01343          ELSE                                                     EL656
01344              MOVE -1       TO TYPE2L                              EL656
01345              MOVE AL-UABON TO TYPE2A                              EL656
01346              MOVE ER-2293  TO EMI-ERROR                           EL656
01347              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01348      ELSE                                                         EL656
01349          IF COPY-FUNCTION                                         EL656
01350              MOVE AL-UANON TO TYPE2A                              EL656
01351              MOVE TYPE1I   TO TYPE2O                              EL656
01352                               PI-RATE-L-AH.                       EL656
01353                                                                   EL656
01354      IF TYPE1I = PI-LIFE-OVERRIDE-L1                              EL656
01355          MOVE '4'          TO WS-REC-TYPE                         EL656
01356      ELSE                                                         EL656
01357          MOVE '5'          TO WS-REC-TYPE.                        EL656
01358                                                                   EL656
01359      IF PLAN2L GREATER ZERO                                       EL656
01360          MOVE PLAN2I       TO WS-HI-BENEFIT                       EL656
01361          PERFORM 7300-EDIT-BENEFIT THRU 7300-EXIT                 EL656
01362      ELSE                                                         EL656
01363          IF COPY-FUNCTION                                         EL656
01364              MOVE AL-UNNON TO PLAN2L                              EL656
01365              MOVE PLAN1I   TO PLAN2O                              EL656
01366                               PI-RATE-LAH-NUM                     EL656
01367          ELSE                                                     EL656
01368              MOVE -1       TO PLAN2L                              EL656
01369              MOVE AL-UABON TO PLAN2A                              EL656
01370              MOVE ER-2268  TO EMI-ERROR                           EL656
01371              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL656
01372                                                                   EL656
01373      IF BENEFIT-FOUND                                             EL656
01374          MOVE PLAN2I       TO PI-RATE-LAH-NUM                     EL656
01375          MOVE AL-UANON     TO PLAN2A                              EL656
01376      ELSE                                                         EL656
01377          MOVE -1           TO PLAN2L                              EL656
01378          MOVE AL-UABON     TO PLAN2A                              EL656
01379          MOVE ER-2268   TO EMI-ERROR                              EL656
01380          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL656
01381                                                                   EL656
01382      IF EXPIRE2L GREATER ZERO                                     EL656
01383          IF EXPIRE2I NUMERIC                                      EL656
01384              IF EXPIRE2I = WS-CURRENT-EXPIRE                      EL656
01385                  MOVE AL-UNNON TO EXPIRE2A                        EL656
01386                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT             EL656
01387                  MOVE EXPIRE2I TO WS-HOLD-RATE-EXP-AL(6:6)        EL656
01388                                   DC-GREG-DATE-1-YMD              EL656
01389                  MOVE '3'      TO DC-OPTION-CODE                  EL656
01390                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT    EL656
01391                  IF NO-CONVERSION-ERROR                           EL656
01392                      MOVE DC-ALPHA-CENTURY TO                     EL656
01393                           WS-HOLD-RATE-EXP-AL(4:2)                EL656
01394                      MOVE WS-HOLD-RATE-EXP-DT TO                  EL656
01395                                              PI-RATE-EXPIRY-DATE  EL656
01396                  ELSE                                             EL656
01397                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999         EL656
01398                          MOVE 99999999999         TO              EL656
01399                               WS-HOLD-RATE-EXP-DT                 EL656
01400                          MOVE WS-HOLD-RATE-EXP-DT TO              EL656
01401                                               PI-RATE-EXPIRY-DATE EL656
01402                      ELSE                                         EL656
01403                          MOVE -1       TO EXPIRE1L                EL656
01404                          MOVE AL-UNBON TO EXPIRE1A                EL656
01405                          MOVE ER-2296  TO EMI-ERROR               EL656
01406                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT EL656
01407                      END-IF                                       EL656
01408                  END-IF                                           EL656
01409              ELSE                                                 EL656
01410                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT             EL656
01411                  MOVE EXPIRE2I TO DC-GREG-DATE-1-YMD              EL656
01412                                   WS-HOLD-RATE-EXP-AL(6:6)        EL656
01413                  MOVE '3'      TO DC-OPTION-CODE                  EL656
01414                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT    EL656
01415                  IF NO-CONVERSION-ERROR                           EL656
01416                      MOVE AL-UNNON TO EXPIRE2A                    EL656
01417                      MOVE EXPIRE2I TO WS-HOLD-RATE-EXP-AL(6:6)    EL656
01418                      MOVE DC-ALPHA-CENTURY TO                     EL656
01419                           WS-HOLD-RATE-EXP-AL(4:2)                EL656
01420                      MOVE WS-HOLD-RATE-EXP-DT TO                  EL656
01421                                              PI-RATE-EXPIRY-DATE  EL656
01422                  ELSE                                             EL656
01423                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999         EL656
01424                          MOVE 99999999999         TO              EL656
01425                               WS-HOLD-RATE-EXP-DT                 EL656
01426                          MOVE WS-HOLD-RATE-EXP-DT TO              EL656
01427                                               PI-RATE-EXPIRY-DATE EL656
01428                      ELSE                                         EL656
01429                          MOVE -1 TO EXPIRE2L                      EL656
01430                          MOVE AL-UNBON TO EXPIRE2A                EL656
01431                          MOVE ER-2296  TO EMI-ERROR               EL656
01432                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT EL656
01433                      END-IF                                       EL656
01434                  END-IF                                           EL656
01435              END-IF                                               EL656
01436          ELSE                                                     EL656
01437              MOVE -1       TO EXPIRE2L                            EL656
01438              MOVE AL-UNBON TO EXPIRE2A                            EL656
01439              MOVE ER-2266  TO EMI-ERROR                           EL656
01440              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01441          END-IF                                                   EL656
01442      ELSE                                                         EL656
01443          IF COPY-FUNCTION                                         EL656
01444              MOVE AL-UNNON TO EXPIRE2A                            EL656
01445              MOVE ALL '9'  TO EXPIRE2O                            EL656
01446              MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT                 EL656
01447              MOVE EXPIRE2I TO WS-HOLD-RATE-EXP-AL(6:6)            EL656
01448                               DC-GREG-DATE-1-YMD                  EL656
01449              MOVE '3'      TO DC-OPTION-CODE                      EL656
01450              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL656
01451              IF NO-CONVERSION-ERROR                               EL656
01452                  MOVE DC-ALPHA-CENTURY TO                         EL656
01453                       WS-HOLD-RATE-EXP-AL(4:2)                    EL656
01454                  MOVE WS-HOLD-RATE-EXP-DT TO                      EL656
01455                       PI-RATE-EXPIRY-DATE                         EL656
01456              ELSE                                                 EL656
01457                  IF WS-HOLD-RATE-EXP-DT(6:6) = 999999             EL656
01458                      MOVE 99999999999         TO                  EL656
01459                           WS-HOLD-RATE-EXP-DT                     EL656
01460                      MOVE WS-HOLD-RATE-EXP-DT TO                  EL656
01461                                           PI-RATE-EXPIRY-DATE     EL656
01462                  ELSE                                             EL656
01463                      MOVE -1       TO EXPIRE1L                    EL656
01464                      MOVE AL-UNBON TO EXPIRE1A                    EL656
01465                      MOVE ER-2296  TO EMI-ERROR                   EL656
01466                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.       CL*74
01467                                                                   EL656
01468  5099-EXIT.                                                       EL656
01469      EXIT.                                                        EL656
01470      EJECT                                                        EL656
01471  6000-DEVIATE-RATES.                                              EL656
01472      MOVE +1  TO  SUB1.                                           EL656
01473                                                                   EL656
01474      DIVIDE WS-SAVE-FACTOR BY 100                                 EL656
01475          GIVING WS-DEVIATE-FACTOR.                                EL656
01476                                                                   EL656
01477  6025-CONT.                                                       EL656
01478      MOVE ZEROS                  TO WS-NEW-RATE.                  EL656
01479                                                                   EL656
01480      MULTIPLY  WS-DEVIATE-FACTOR BY RT-L-RATE (SUB1)              EL656
01481          GIVING DEEDIT-FIELD-V5.                                  EL656
01482                                                                   EL656
01483      PERFORM 6100-ROUND-AS-REQUESTED THRU 6100-EXIT.              EL656
01484                                                                   EL656
01485      MOVE DEEDIT-FIELD-V5        TO RT-L-RATE (SUB1).             EL656
01486      ADD +1                      TO SUB1.                         EL656
01487                                                                   EL656
01488      IF SUB1 GREATER 360                                          EL656
01489          GO TO 6099-EXIT                                          EL656
01490      ELSE                                                         EL656
01491          GO TO 6025-CONT.                                         EL656
01492                                                                   EL656
01493  6099-EXIT.                                                       EL656
01494      EXIT.                                                        EL656
01495      EJECT                                                        EL656
01496                                  EJECT                            EL656
01497  6100-ROUND-AS-REQUESTED.                                         EL656
01498                                                                   EL656
01499      IF W-DECIMAL-NUMBER = 5                                         CL*74
01500          GO TO 6100-EXIT.                                         EL656
01501                                                                   EL656
01502      IF W-DECIMAL-NUMBER = 0                                         CL*74
01503          COMPUTE W-FIELD-V0 ROUNDED = DEEDIT-FIELD-V5                CL*74
01504          MOVE W-FIELD-V0           TO DEEDIT-FIELD-V5                CL*74
01505      ELSE                                                         EL656
01506      IF W-DECIMAL-NUMBER = 1                                         CL*74
01507          COMPUTE W-FIELD-V1 ROUNDED = DEEDIT-FIELD-V5                CL*74
01508          MOVE W-FIELD-V1           TO DEEDIT-FIELD-V5                CL*74
01509      ELSE                                                            CL*74
01510      IF W-DECIMAL-NUMBER = 2                                         CL*74
01511          COMPUTE W-FIELD-V2 ROUNDED = DEEDIT-FIELD-V5                CL*74
01512          MOVE W-FIELD-V2           TO DEEDIT-FIELD-V5                CL*74
01513      ELSE                                                            CL*74
01514      IF W-DECIMAL-NUMBER = 3                                         CL*74
01515          COMPUTE W-FIELD-V3 ROUNDED = DEEDIT-FIELD-V5                CL*74
01516          MOVE W-FIELD-V3           TO DEEDIT-FIELD-V5                CL*74
01517      ELSE                                                            CL*74
01518      IF W-DECIMAL-NUMBER = 4                                         CL*74
01519           COMPUTE W-FIELD-V4 ROUNDED = DEEDIT-FIELD-V5               CL*74
01520           MOVE W-FIELD-V4           TO DEEDIT-FIELD-V5.              CL*74
01521                                                                   EL656
01522  6100-EXIT.                                                       EL656
01523      EXIT.                                                        EL656
01524                                  EJECT                            EL656
01525  6500-SHOW-PLANS.                                                 EL656
01526      IF SHOW-AH-PLANS                                             EL656
01527          MOVE PI-AH-OVERRIDE-L6   TO PLANTYPO.                    EL656
01528                                                                   EL656
01529      IF SHOW-LF-PLANS                                             EL656
01530          MOVE PI-LIFE-OVERRIDE-L6 TO PLANTYPO.                    EL656
01531                                                                   EL656
01532      MOVE PI-RATE-STATE-CODE     TO  WS-SAVE-STRUCTURE.           EL656
01533      MOVE LOW-VALUES             TO  PI-RATE-LIMITS               EL656
01534                                      PI-RATE-L-AH-CODE.           EL656
01535      MOVE ZEROS                  TO  PI-RATE-EXPIRY-DATE.         EL656
01536      SET PT-INDX                 TO  +1.                          EL656
01537                                                                   EL656
01538      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL656
01539                                                                   EL656
01540  6525-CONT.                                                       EL656
01541      IF PT-INDX GREATER +24                                       EL656
01542          GO TO 6590-RESET-KEY.                                    EL656
01543                                                                   EL656
01544      EXEC CICS HANDLE CONDITION                                   EL656
01545          ENDFILE  (6590-RESET-KEY)                                EL656
01546      END-EXEC.                                                    EL656
01547                                                                   EL656
01548      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL656
01549                                                                   EL656
01550      IF ERRATE-EOF                                                EL656
01551          MOVE 'N'                TO  PI-ERRATE-EOF-SW             EL656
01552          IF BROWSE-STARTED                                        EL656
01553              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL656
01554              GO TO 6590-RESET-KEY.                                EL656
01555                                                                   EL656
01556      IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE                    EL656
01557          NEXT SENTENCE                                            EL656
01558      ELSE                                                         EL656
01559          GO TO 6590-RESET-KEY.                                    EL656
01560                                                                   EL656
01561      IF SHOW-AH-PLANS                                             EL656
01562          IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                      EL656
01563              MOVE '5'            TO WS-REC-TYPE                   EL656
01564          ELSE                                                     EL656
01565              GO TO 6525-CONT.                                     EL656
01566                                                                   EL656
01567      IF SHOW-LF-PLANS                                             EL656
01568          IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                    EL656
01569              MOVE '4'            TO WS-REC-TYPE                   EL656
01570          ELSE                                                     EL656
01571              GO TO 6525-CONT.                                     EL656
01572                                                                   EL656
01573      IF FIRST-REC-IN-STRUCTURE                                    EL656
01574          MOVE 'N'                  TO WS-FIRST-REC-SW             EL656
01575          MOVE RT-ST-CODE           TO STATE1O                     EL656
01576          MOVE RT-ST-CLASS          TO CLASS1O                     EL656
01577          MOVE RT-HIGH-AGE          TO RTAGE1O                     EL656
01578          MOVE RT-HIGH-AMT          TO RTAMT1O                     EL656
01579          MOVE RT-ST-DEV            TO DEV1O                       EL656
01580          MOVE RT-EXPIRY-DATE       TO WS-HOLD-RATE-EXP-DT         EL656
01581          MOVE WS-HOLD-RATE-EXP-AL  TO EXPIRE1O                    EL656
01582          MOVE RT-L-AH              TO TYPE1O                      EL656
01583          MOVE RT-LAH-NUM           TO PLAN1O                      EL656
01584          MOVE RT-STRUCTURE-COMMENT TO STCOMMO                     EL656
01585          MOVE RT-LAST-MAINT-DT     TO DC-BIN-DATE-1               EL656
01586          MOVE ' '                  TO DC-OPTION-CODE              EL656
01587          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL656
01588          MOVE DC-GREG-DATE-1-EDIT  TO LUDATEO                     EL656
01589          MOVE RT-LAST-MAINT-USER   TO LUBYO                       EL656
01590          MOVE RT-LAST-MAINT-HHMMSS TO TIME-LMT                    EL656
01591          MOVE TIME-LMT             TO LUTIMEO                     EL656
01592          MOVE 'N'                  TO PI-FIRST-TIME-SW            EL656
01593          MOVE PI-ERRATE-KEY        TO PI-SAVE-ERRATE-KEY.         EL656
01594                                                                   EL656
01595      MOVE PI-RATE-LAH-NUM          TO PT-CODE (PT-INDX)           EL656
01596      MOVE PI-RATE-EXPIRY-DATE      TO PT-EXPIRE (PT-INDX)         EL656
01597      MOVE PI-RATE-LAH-NUM          TO WS-HI-BENEFIT.              EL656
01598      MOVE RT-HIGH-AGE              TO PT-AGE (PT-INDX).           EL656
01599                                                                   EL656
01600      PERFORM 7300-EDIT-BENEFIT   THRU 7300-EXIT.                  EL656
01601                                                                   EL656
01602      IF BENEFIT-FOUND                                             EL656
01603          MOVE +1 TO SUB1                                          EL656
01604      ELSE                                                         EL656
01605          MOVE ALL '*' TO PT-DESC (PT-INDX)                        EL656
01606          SET PT-INDX UP BY +1                                     EL656
01607          GO TO 6525-CONT.                                         EL656
01608                                                                   EL656
01609  6575-SEARCH-BENEFIT-TABLE.                                       EL656
01610      IF SUB1 GREATER +8                                           EL656
01611          MOVE ALL '*' TO PT-DESC (PT-INDX)                        EL656
01612          SET PT-INDX UP BY +1                                     EL656
01613          GO TO 6525-CONT.                                         EL656
01614                                                                   EL656
01615      IF CF-BENEFIT-CODE (SUB1) = PI-RATE-LAH-NUM                  EL656
01616          MOVE CF-BENEFIT-DESCRIP (SUB1) TO PT-DESC (PT-INDX)      EL656
01617      ELSE                                                         EL656
01618          ADD +1 TO SUB1                                           EL656
01619          GO TO 6575-SEARCH-BENEFIT-TABLE.                         EL656
01620                                                                   EL656
01621      SET PT-INDX UP BY +1.                                        EL656
01622      GO TO 6525-CONT.                                             EL656
01623                                                                   EL656
01624  6590-RESET-KEY.                                                  EL656
01625      MOVE PI-SAVE-ERRATE-KEY TO PI-ERRATE-KEY.                    EL656
01626                                                                   EL656
01627  6599-EXIT.                                                       EL656
01628      EXIT.                                                        EL656
01629      EJECT                                                        EL656
01630                                                                   EL656
01631  7100-NEXT-STRUCTURE.                                             EL656
01632      MOVE SPACES             TO  PI-ERRATE-EOF-SW.                EL656
01633      MOVE LOW-VALUES         TO  PI-ERRATE-KEY.                   EL656
01634                                                                   EL656
01635      IF STATE1L GREATER ZERO                                      EL656
01636          MOVE AL-UANON   TO STATE1A                               EL656
01637          MOVE STATE1I    TO PI-RATE-CODE                          EL656
01638      ELSE                                                         EL656
01639          MOVE LOW-VALUES TO PI-RATE-CODE.                         EL656
01640                                                                   EL656
01641      IF CLASS1L GREATER ZERO                                      EL656
01642          MOVE AL-UANON   TO CLASS1A                               EL656
01643          MOVE CLASS1I    TO PI-RATE-CLASS                         EL656
01644      ELSE                                                         EL656
01645          MOVE LOW-VALUES TO PI-RATE-CLASS.                        EL656
01646                                                                   EL656
01647      IF DEV1L GREATER ZERO                                        EL656
01648          MOVE AL-UANON   TO DEV1A                                 EL656
01649          MOVE DEV1I      TO PI-RATE-DEV                           EL656
01650      ELSE                                                         EL656
01651          MOVE LOW-VALUES TO PI-RATE-DEV.                          EL656
01652                                                                   EL656
01653      IF EIBAID = DFHPF1                                           EL656
01654          GO TO 7000-START-BROWSE.                                 EL656
01655                                                                   EL656
01656      IF RTAGE1L GREATER ZERO                                      EL656
01657          MOVE AL-UANON   TO RTAGE1A                               EL656
01658          MOVE RTAGE1I    TO PI-RATE-HIGH-AGE                      EL656
01659      ELSE                                                         EL656
01660          MOVE ZEROS      TO PI-RATE-HIGH-AGE.                     EL656
01661                                                                   EL656
01662      IF RTAMT1L GREATER ZERO                                      EL656
01663         MOVE RTAMT1I             TO  DEEDIT-FIELD                 EL656
01664         PERFORM 7500-DEEDIT THRU 7500-EXIT                        EL656
01665         IF DEEDIT-FIELD-V0 GREATER ZERO                           EL656
01666            MOVE AL-UNNON         TO RTAMT1A                       EL656
01667            MOVE DEEDIT-FIELD-V0  TO PI-RATE-HIGH-AMT              EL656
01668         ELSE                                                      EL656
01669            MOVE ZEROS            TO PI-RATE-HIGH-AMT              EL656
01670      ELSE                                                         EL656
01671         MOVE ZEROS               TO PI-RATE-HIGH-AMT.             EL656
01672                                                                   EL656
01673      IF TYPE1L GREATER ZERO                                       EL656
01674          MOVE AL-UANON   TO TYPE1A                                EL656
01675          MOVE TYPE1I     TO PI-RATE-L-AH                          EL656
01676      ELSE                                                         EL656
01677          MOVE LOW-VALUES TO PI-RATE-L-AH.                         EL656
01678                                                                   EL656
01679      IF EXPIRE1L GREATER ZERO                                     EL656
01680          MOVE AL-UNNON   TO EXPIRE1A                              EL656
01681          MOVE ZEROS      TO WS-HOLD-RATE-EXP-DT                   EL656
01682          MOVE EXPIRE1I   TO WS-HOLD-RATE-EXP-AL(6:6)              EL656
01683                             DC-GREG-DATE-1-YMD                    EL656
01684          MOVE '3'        TO DC-OPTION-CODE                        EL656
01685          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL656
01686          IF NO-CONVERSION-ERROR                                   EL656
01687              MOVE DC-ALPHA-CENTURY TO                             EL656
01688                   WS-HOLD-RATE-EXP-AL(4:2)                        EL656
01689              MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE      EL656
01690          ELSE                                                     EL656
01691              IF WS-HOLD-RATE-EXP-DT(6:6) = 999999                 EL656
01692                  MOVE 99999999999         TO                      EL656
01693                       WS-HOLD-RATE-EXP-DT                         EL656
01694                  MOVE WS-HOLD-RATE-EXP-DT TO                      EL656
01695                                       PI-RATE-EXPIRY-DATE         EL656
01696              ELSE                                                 EL656
01697                  MOVE -1       TO EXPIRE1L                        EL656
01698                  MOVE AL-UNBON TO EXPIRE1A                        EL656
01699                  MOVE ER-2296  TO EMI-ERROR                       EL656
01700                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL656
01701              END-IF                                               EL656
01702          END-IF                                                   EL656
01703      ELSE                                                         EL656
01704          MOVE ZEROS               TO WS-HOLD-RATE-EXP-AL          EL656
01705          MOVE 99999999999         TO WS-HOLD-RATE-EXP-AL          EL656
01706          MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE.            CL*74
01707                                                                   EL656
01708      IF PLAN1L GREATER ZERO                                       EL656
01709          MOVE AL-UANON   TO PLAN1A                                EL656
01710          MOVE PLAN1I     TO PI-RATE-LAH-NUM                       EL656
01711      ELSE                                                         EL656
01712          MOVE ZEROS      TO PI-RATE-LAH-NUM.                      EL656
01713                                                                   EL656
01714      MOVE ALL '9'        TO PI-RATE-SEX                           EL656
01715                             PI-RATE-FUTURE.                       EL656
01716                                                                   EL656
01717  7000-START-BROWSE.                                               EL656
01718      MOVE PI-COMPANY-CD      TO  PI-RATE-COMPANY-CD.              EL656
01719      MOVE PI-RATE-STATE-CODE TO  WS-SAVE-STRUCTURE.               EL656
01720                                                                   EL656
01721      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL656
01722                                                                   EL656
01723  7125-READ-NEXT.                                                  EL656
01724      EXEC CICS HANDLE CONDITION                                   EL656
01725          ENDFILE  (7150-ENDFILE)                                  EL656
01726          NOTFND   (7175-NOTFOUND)                                 EL656
01727      END-EXEC.                                                    EL656
01728                                                                   EL656
01729      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL656
01730                                                                   EL656
01731      IF ERRATE-EOF                                                EL656
01732          IF FIRST-TIME                                            EL656
01733              MOVE LOW-VALUES TO EL656AO                           EL656
01734              MOVE ER-2272 TO EMI-ERROR                            EL656
01735              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01736              MOVE -1         TO MAINTL                            EL656
01737              GO TO 8100-SEND-INITIAL-MAP                          EL656
01738          ELSE                                                     EL656
01739              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL656
01740              MOVE LOW-VALUES TO EL656AO                           EL656
01741              MOVE ER-2271 TO EMI-ERROR                            EL656
01742              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01743              MOVE 'Y'        TO WS-FIRST-TIME-SW                  EL656
01744                                 WS-EOF-DISPLAY-SW                 EL656
01745              GO TO 7100-NEXT-STRUCTURE.                           EL656
01746                                                                   EL656
01747      IF EIBAID = DFHPF1                                           EL656
01748          IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE                EL656
01749              MOVE SPACES     TO WS-FIRST-TIME-SW                  EL656
01750              MOVE 'Z'        TO PI-RATE-L-AH                      EL656
01751              GO TO 7125-READ-NEXT.                                EL656
01752                                                                   EL656
01753      IF NOT EOF-HIT-DISPLAY-FIRST                                 EL656
01754          IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                    EL656
01755              IF FIRST-PLAN-SHOWN                                  EL656
01756                 MOVE LOW-VALUES TO PI-RATE-L-AH-CODE              EL656
01757                                    PI-RATE-LIMITS                 EL656
01758                 MOVE ZEROS      TO PI-RATE-EXPIRY-DATE            EL656
01759                 MOVE 'N'        TO PI-FIRST-TIME-SW               EL656
01760                 GO TO 7125-READ-NEXT                              EL656
01761              ELSE                                                 EL656
01762                 MOVE SPACES     TO WS-FIRST-TIME-SW               EL656
01763                 GO TO 7125-READ-NEXT.                             EL656
01764                                                                   EL656
01765      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL656
01766      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL656
01767                                                                   EL656
01768      MOVE PI-ERRATE-KEY          TO PI-SAVE-ERRATE-KEY.           EL656
01769                                                                   EL656
01770      GO TO 3025-SET-UP-SCREEN-A.                                  EL656
01771                                                                   EL656
01772  7150-ENDFILE.                                                    EL656
01773      IF BROWSE-STARTED                                            EL656
01774          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL656
01775                                                                   EL656
01776      IF FIRST-TIME                                                EL656
01777          MOVE LOW-VALUES TO EL656AO                               EL656
01778          MOVE ER-2272 TO EMI-ERROR                                EL656
01779          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01780          MOVE -1         TO MAINTL                                EL656
01781          GO TO 8100-SEND-INITIAL-MAP                              EL656
01782      ELSE                                                         EL656
01783          MOVE LOW-VALUES TO EL656AO                               EL656
01784          MOVE ER-2271 TO EMI-ERROR                                EL656
01785          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01786          MOVE 'Y'        TO WS-FIRST-TIME-SW                      EL656
01787                             WS-EOF-DISPLAY-SW                     EL656
01788          GO TO 7100-NEXT-STRUCTURE.                               EL656
01789                                                                   EL656
01790  7175-NOTFOUND.                                                   EL656
01791      IF BROWSE-STARTED                                            EL656
01792          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL656
01793                                                                   EL656
01794      GO TO 8880-NOT-FOUND.                                        EL656
01795                                                                   EL656
01796  7199-EXIT.                                                       EL656
01797      EXIT.                                                        EL656
01798      EJECT                                                        EL656
01799                                                                   EL656
01800  7200-PRIOR-STRUCTURE.                                            EL656
01801      MOVE SPACES             TO  PI-ERRATE-EOF-SW.                EL656
01802                                                                   EL656
01803      MOVE LOW-VALUES         TO  PI-ERRATE-KEY.                   EL656
01804                                                                   EL656
01805      IF STATE1L GREATER ZERO                                      EL656
01806          MOVE AL-UANON   TO STATE1A                               EL656
01807          MOVE STATE1I    TO PI-RATE-CODE                          EL656
01808      ELSE                                                         EL656
01809          MOVE LOW-VALUES TO PI-RATE-CODE.                         EL656
01810                                                                   EL656
01811      IF CLASS1L GREATER ZERO                                      EL656
01812          MOVE AL-UANON   TO CLASS1A                               EL656
01813          MOVE CLASS1I    TO PI-RATE-CLASS                         EL656
01814      ELSE                                                         EL656
01815          MOVE LOW-VALUES TO PI-RATE-CLASS.                        EL656
01816                                                                   EL656
01817      IF DEV1L GREATER ZERO                                        EL656
01818          MOVE AL-UANON   TO DEV1A                                 EL656
01819          MOVE DEV1I      TO PI-RATE-DEV                           EL656
01820      ELSE                                                         EL656
01821          MOVE LOW-VALUES TO PI-RATE-DEV.                          EL656
01822                                                                   EL656
01823      IF EIBAID = DFHPF2                                           EL656
01824          GO TO 7200-START-BROWSE.                                 EL656
01825                                                                   EL656
01826      IF RTAGE1L GREATER ZERO                                      EL656
01827          MOVE AL-UANON   TO RTAGE1A                               EL656
01828          MOVE RTAGE1I    TO PI-RATE-HIGH-AGE                      EL656
01829      ELSE                                                         EL656
01830          MOVE ZEROS      TO PI-RATE-HIGH-AGE.                     EL656
01831                                                                   EL656
01832      IF RTAMT1L GREATER ZERO                                      EL656
01833         MOVE RTAMT1I             TO  DEEDIT-FIELD                 EL656
01834         PERFORM 7500-DEEDIT THRU 7500-EXIT                        EL656
01835         IF DEEDIT-FIELD-V0 GREATER ZERO                           EL656
01836            MOVE AL-UNNON         TO RTAMT1A                       EL656
01837            MOVE DEEDIT-FIELD-V0  TO PI-RATE-HIGH-AMT              EL656
01838         ELSE                                                      EL656
01839            MOVE ZEROS            TO PI-RATE-HIGH-AMT              EL656
01840      ELSE                                                         EL656
01841         MOVE ZEROS               TO PI-RATE-HIGH-AMT.             EL656
01842                                                                   EL656
01843      IF TYPE1L GREATER ZERO                                       EL656
01844          MOVE AL-UANON   TO TYPE1A                                EL656
01845          MOVE TYPE1I     TO PI-RATE-L-AH                          EL656
01846      ELSE                                                         EL656
01847          MOVE LOW-VALUES TO PI-RATE-L-AH.                         EL656
01848                                                                   EL656
01849      IF EXPIRE1L GREATER ZERO                                     EL656
01850          MOVE AL-UNNON   TO EXPIRE1A                              EL656
01851          MOVE ZEROS      TO WS-HOLD-RATE-EXP-DT                   EL656
01852          MOVE EXPIRE1I   TO WS-HOLD-RATE-EXP-AL(6:6)              EL656
01853                             DC-GREG-DATE-1-YMD                    EL656
01854          MOVE '3'        TO DC-OPTION-CODE                        EL656
01855          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL656
01856          IF NO-CONVERSION-ERROR                                   EL656
01857              MOVE DC-ALPHA-CENTURY TO                             EL656
01858                   WS-HOLD-RATE-EXP-AL(4:2)                        EL656
01859              MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE      EL656
01860          ELSE                                                     EL656
01861              IF WS-HOLD-RATE-EXP-DT(6:6) = 999999                 EL656
01862                  MOVE 99999999999         TO                      EL656
01863                       WS-HOLD-RATE-EXP-DT                         EL656
01864                  MOVE WS-HOLD-RATE-EXP-DT TO                      EL656
01865                                       PI-RATE-EXPIRY-DATE         EL656
01866              ELSE                                                 EL656
01867                  MOVE -1       TO EXPIRE1L                        EL656
01868                  MOVE AL-UNBON TO EXPIRE1A                        EL656
01869                  MOVE ER-2296  TO EMI-ERROR                       EL656
01870                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL656
01871              END-IF                                               EL656
01872          END-IF                                                   EL656
01873      ELSE                                                         EL656
01874          MOVE ZEROS               TO WS-HOLD-RATE-EXP-AL          EL656
01875          MOVE 99999999999         TO WS-HOLD-RATE-EXP-AL          EL656
01876          MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE.            CL*74
01877                                                                   EL656
01878      IF PLAN1L GREATER ZERO                                       EL656
01879          MOVE AL-UANON   TO PLAN1A                                EL656
01880          MOVE PLAN1I     TO PI-RATE-LAH-NUM                       EL656
01881      ELSE                                                         EL656
01882          MOVE ZEROS      TO PI-RATE-LAH-NUM.                      EL656
01883                                                                   EL656
01884  7200-START-BROWSE.                                               EL656
01885      MOVE PI-COMPANY-CD      TO  PI-RATE-COMPANY-CD.              EL656
01886      MOVE PI-RATE-STATE-CODE TO  WS-SAVE-STRUCTURE.               EL656
01887                                                                   EL656
01888      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL656
01889                                                                   EL656
01890      EXEC CICS HANDLE CONDITION                                   EL656
01891          ENDFILE  (7250-ENDFILE)                                  EL656
01892          NOTFND   (7275-NOTFOUND)                                 EL656
01893      END-EXEC.                                                    EL656
01894                                                                   EL656
01895      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL656
01896                                                                   EL656
01897      IF ERRATE-EOF                                                EL656
01898          IF FIRST-TIME                                            EL656
01899              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL656
01900              MOVE LOW-VALUES TO EL656AO                           EL656
01901              MOVE ER-2272 TO EMI-ERROR                            EL656
01902              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01903              GO TO 8100-SEND-INITIAL-MAP                          EL656
01904          ELSE                                                     EL656
01905              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL656
01906              MOVE LOW-VALUES TO EL656AO                           EL656
01907              MOVE ER-2271 TO EMI-ERROR                            EL656
01908              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL656
01909              GO TO 7100-NEXT-STRUCTURE.                           EL656
01910                                                                   EL656
01911      MOVE SPACES TO WS-FIRST-TIME-SW.                             EL656
01912                                                                   EL656
01913  7225-READ-PREV.                                                  EL656
01914      EXEC CICS HANDLE CONDITION                                   EL656
01915          ENDFILE  (7250-ENDFILE)                                  EL656
01916          NOTFND   (7275-NOTFOUND)                                 EL656
01917      END-EXEC.                                                    EL656
01918                                                                   EL656
01919      PERFORM 7900-READPREV THRU 7900-EXIT.                        EL656
01920                                                                   EL656
01921      IF ERRATE-EOF                                                EL656
01922          PERFORM 7950-END-BROWSE THRU 7950-EXIT                   EL656
01923          MOVE LOW-VALUES TO EL656AO                               EL656
01924          MOVE ER-2271 TO EMI-ERROR                                EL656
01925          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01926          MOVE 'Y'        TO WS-FIRST-TIME-SW                      EL656
01927          GO TO 7100-NEXT-STRUCTURE.                               EL656
01928                                                                   EL656
01929      IF EIBAID = DFHPF2                                           EL656
01930          IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE                EL656
01931              MOVE SPACES     TO WS-FIRST-TIME-SW                  EL656
01932              GO TO 7225-READ-PREV.                                EL656
01933                                                                   EL656
01934      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL656
01935          IF FIRST-PLAN-SHOWN                                      EL656
01936              MOVE SPACES         TO PI-FIRST-TIME-SW              EL656
01937          ELSE                                                     EL656
01938              MOVE SPACES         TO WS-FIRST-TIME-SW              EL656
01939              GO TO 7225-READ-PREV.                                EL656
01940                                                                   EL656
01941      IF BROWSE-STARTED                                            EL656
01942          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL656
01943                                                                   EL656
01944      EXEC CICS HANDLE CONDITION                                   EL656
01945          ENDFILE  (7250-ENDFILE)                                  EL656
01946          NOTFND   (7275-NOTFOUND)                                 EL656
01947      END-EXEC.                                                    EL656
01948                                                                   EL656
01949      PERFORM 7600-READ-ERRATE-GTEQ THRU 7600-EXIT.                EL656
01950                                                                   EL656
01951      IF ERRATE-EOF                                                EL656
01952          IF BROWSE-STARTED                                        EL656
01953              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL656
01954              MOVE LOW-VALUES TO EL656AO                           EL656
01955              MOVE 'Y'        TO WS-FIRST-TIME-SW                  EL656
01956              GO TO 7100-NEXT-STRUCTURE.                           EL656
01957                                                                   EL656
01958      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL656
01959      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL656
01960      MOVE RT-CONTROL-PRIMARY     TO PI-ERRATE-KEY                 EL656
01961                                     PI-SAVE-ERRATE-KEY.           EL656
01962                                                                   EL656
01963      GO TO 3025-SET-UP-SCREEN-A.                                  EL656
01964                                                                   EL656
01965  7250-ENDFILE.                                                    EL656
01966      IF BROWSE-STARTED                                            EL656
01967          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL656
01968                                                                   EL656
01969      IF FIRST-TIME                                                EL656
01970          MOVE LOW-VALUES TO EL656AO                               EL656
01971          MOVE ER-2272 TO EMI-ERROR                                EL656
01972          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01973          MOVE -1         TO MAINTL                                EL656
01974          GO TO 8100-SEND-INITIAL-MAP                              EL656
01975      ELSE                                                         EL656
01976          MOVE ER-2271 TO EMI-ERROR                                EL656
01977          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL656
01978          MOVE LOW-VALUES TO EL656AO                               EL656
01979          MOVE 'Y'        TO WS-FIRST-TIME-SW                      EL656
01980          GO TO 7100-NEXT-STRUCTURE.                               EL656
01981                                                                   EL656
01982  7275-NOTFOUND.                                                   EL656
01983      IF BROWSE-STARTED                                            EL656
01984          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL656
01985                                                                   EL656
01986      GO TO 8880-NOT-FOUND.                                        EL656
01987                                                                   EL656
01988  7299-EXIT.                                                       EL656
01989      EXIT.                                                        EL656
01990      EJECT                                                        EL656
01991                                                                   EL656
01992  7300-EDIT-BENEFIT.                                               EL656
01993      MOVE SPACES                 TO WS-BENEFIT-FOUND-SW.          EL656
01994                                                                   EL656
01995      IF INVALID-PLAN-CODE                                         EL656
01996          GO TO 7300-EXIT.                                         EL656
01997                                                                   EL656
01998      MOVE LOW-VALUES             TO ELCNTL-KEY.                   EL656
01999      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL656
02000      MOVE +0                     TO CNTL-SEQ-NO.                  EL656
02001      MOVE WS-BENEFIT-ACCESS      TO CNTL-ACCESS.                  EL656
02002      MOVE WS-REC-TYPE            TO CNTL-REC-TYPE.                EL656
02003                                                                   EL656
02004      EXEC CICS HANDLE CONDITION                                   EL656
02005          NOTFND   (7300-EXIT)                                     EL656
02006      END-EXEC.                                                    EL656
02007                                                                   EL656
02008      EXEC CICS READ                                               EL656
02009          DATASET   (CNTL-FILE-ID)                                 EL656
02010          SET       (ADDRESS OF CONTROL-FILE)                      EL656
02011          RIDFLD    (ELCNTL-KEY)                                   EL656
02012          GTEQ                                                     EL656
02013      END-EXEC.                                                    EL656
02014                                                                   EL656
02015      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL656
02016          IF CF-LF-BENEFIT-MASTER                                  EL656
02017              NEXT SENTENCE                                        EL656
02018          ELSE                                                     EL656
02019              GO TO 7300-EXIT.                                     EL656
02020                                                                   EL656
02021      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL656
02022          IF CF-AH-BENEFIT-MASTER                                  EL656
02023              NEXT SENTENCE                                        EL656
02024          ELSE                                                     EL656
02025              GO TO 7300-EXIT.                                     EL656
02026                                                                   EL656
02027      MOVE 'Y'  TO  WS-BENEFIT-FOUND-SW.                           EL656
02028                                                                   EL656
02029  7300-EXIT.                                                       EL656
02030      EXIT.                                                        EL656
02031      EJECT                                                        EL656
02032                                                                   EL656
02033  7400-EDIT-STATE.                                                 EL656
02034      MOVE SPACES                 TO WS-STATE-FOUND-SW.            EL656
02035      MOVE LOW-VALUES             TO ELCNTL-KEY.                   EL656
02036      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL656
02037      MOVE '3'                    TO CNTL-REC-TYPE.                EL656
02038      MOVE +0                     TO CNTL-SEQ-NO.                  EL656
02039      MOVE WS-ACCESS              TO CNTL-ACCESS.                  EL656
02040                                                                   EL656
02041      EXEC CICS HANDLE CONDITION                                   EL656
02042          NOTFND   (7499-EXIT)                                     EL656
02043      END-EXEC.                                                    EL656
02044                                                                   EL656
02045      EXEC CICS READ                                               EL656
02046          DATASET   (CNTL-FILE-ID)                                 EL656
02047          SET       (ADDRESS OF CONTROL-FILE)                      EL656
02048          RIDFLD    (ELCNTL-KEY)                                   EL656
02049      END-EXEC.                                                    EL656
02050                                                                   EL656
02051      MOVE 'Y' TO WS-STATE-FOUND-SW.                               EL656
02052                                                                   EL656
02053  7499-EXIT.                                                       EL656
02054      EXIT.                                                        EL656
02055      EJECT                                                        EL656
02056                                                                   EL656
02057  7500-DEEDIT.                                                     EL656
02058      EXEC CICS BIF                                                EL656
02059           DEEDIT                                                  EL656
02060           FIELD  (DEEDIT-FIELD)                                   EL656
02061           LENGTH (15)                                             EL656
02062       END-EXEC.                                                   EL656
02063                                                                   EL656
02064  7500-EXIT.                                                       EL656
02065      EXIT.                                                        EL656
02066      EJECT                                                        EL656
02067                                                                   EL656
02068  7600-READ-ERRATE-GTEQ.                                           EL656
02069      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL656
02070                                                                   EL656
02071      EXEC CICS READ                                               EL656
02072           DATASET  (RATE-FILE-ID)                                 EL656
02073           SET      (ADDRESS OF RATE-RECORD)                       EL656
02074           RIDFLD   (PI-ERRATE-KEY)                                EL656
02075           GTEQ                                                    EL656
02076      END-EXEC.                                                    EL656
02077                                                                   EL656
02078      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL656
02079      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL656
02080                                                                   EL656
02081  7600-EXIT.                                                       EL656
02082      EXIT.                                                        EL656
02083      EJECT                                                        EL656
02084                                                                   EL656
02085  7650-READ-ERRATE.                                                EL656
02086      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL656
02087                                                                   EL656
02088      EXEC CICS READ                                               EL656
02089           DATASET  (RATE-FILE-ID)                                 EL656
02090           SET      (ADDRESS OF RATE-RECORD)                       EL656
02091           RIDFLD   (PI-ERRATE-KEY)                                EL656
02092      END-EXEC.                                                    EL656
02093                                                                   EL656
02094      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL656
02095      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL656
02096                                                                   EL656
02097  7650-EXIT.                                                       EL656
02098      EXIT.                                                        EL656
02099      EJECT                                                        EL656
02100                                                                   EL656
02101  7700-ERRATE-GETMAIN.                                             EL656
02102      EXEC CICS GETMAIN                                            EL656
02103           SET     (ADDRESS OF RATE-RECORD)                        EL656
02104           LENGTH  (ERRATE-LENGTH)                                 EL656
02105           INITIMG (GETMAIN-SPACE)                                 EL656
02106      END-EXEC.                                                    EL656
02107                                                                   EL656
02108  7700-EXIT.                                                       EL656
02109      EXIT.                                                        EL656
02110      EJECT                                                        EL656
02111                                                                   EL656
02112  7750-READ-ERRATE-UPDATE.                                         EL656
02113      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL656
02114                                                                   EL656
02115      EXEC CICS READ                                               EL656
02116           DATASET  (RATE-FILE-ID)                                 EL656
02117           SET      (ADDRESS OF RATE-RECORD)                       EL656
02118           RIDFLD   (PI-ERRATE-KEY)                                EL656
02119           UPDATE                                                  EL656
02120      END-EXEC.                                                    EL656
02121                                                                   EL656
02122  7750-EXIT.                                                       EL656
02123      EXIT.                                                        EL656
02124      EJECT                                                        EL656
02125                                                                   EL656
02126  7800-START-BROWSE.                                               EL656
02127      EXEC CICS STARTBR                                            EL656
02128           DATASET  (RATE-FILE-ID)                                 EL656
02129           RIDFLD   (PI-ERRATE-KEY)                                EL656
02130      END-EXEC.                                                    EL656
02131                                                                   EL656
02132      MOVE 'Y' TO PI-BROWSE-SW.                                    EL656
02133                                                                   EL656
02134  7800-EXIT.                                                       EL656
02135      EXIT.                                                        EL656
02136      EJECT                                                        EL656
02137                                                                   EL656
02138  7850-READNEXT.                                                   EL656
02139      EXEC CICS READNEXT                                           EL656
02140           DATASET  (RATE-FILE-ID)                                 EL656
02141           SET      (ADDRESS OF RATE-RECORD)                       EL656
02142           RIDFLD   (PI-ERRATE-KEY)                                EL656
02143      END-EXEC.                                                    EL656
02144                                                                   EL656
02145      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                         EL656
02146          MOVE 'Y'        TO PI-ERRATE-EOF-SW.                     EL656
02147                                                                   EL656
02148  7850-EXIT.                                                       EL656
02149      EXIT.                                                        EL656
02150      EJECT                                                        EL656
02151                                                                   EL656
02152  7900-READPREV.                                                   EL656
02153      EXEC CICS READPREV                                           EL656
02154           DATASET  (RATE-FILE-ID)                                 EL656
02155           SET      (ADDRESS OF RATE-RECORD)                       EL656
02156           RIDFLD   (PI-ERRATE-KEY)                                EL656
02157      END-EXEC.                                                    EL656
02158                                                                   EL656
02159      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                         EL656
02160          MOVE 'Y'        TO PI-ERRATE-EOF-SW.                     EL656
02161                                                                   EL656
02162  7900-EXIT.                                                       EL656
02163      EXIT.                                                        EL656
02164      EJECT                                                        EL656
02165                                                                   EL656
02166  7950-END-BROWSE.                                                 EL656
02167      EXEC CICS ENDBR                                              EL656
02168           DATASET  (RATE-FILE-ID)                                 EL656
02169      END-EXEC.                                                    EL656
02170                                                                   EL656
02171      MOVE SPACE TO PI-BROWSE-SW.                                  EL656
02172                                                                   EL656
02173  7950-EXIT.                                                       EL656
02174      EXIT.                                                        EL656
02175      EJECT                                                        EL656
02176  8000-UPDATE-MAINT-DATE.                                          EL656
02177      MOVE SPACES                 TO ELCNTL-KEY.                   EL656
02178                                                                   EL656
02179      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL656
02180      MOVE '1'                    TO CNTL-REC-TYPE.                EL656
02181      MOVE +0                     TO CNTL-SEQ-NO.                  EL656
02182                                                                   EL656
02183      EXEC CICS HANDLE CONDITION                                   EL656
02184          NOTFND   (8000-EXIT)                                     EL656
02185      END-EXEC.                                                    EL656
02186                                                                   EL656
02187      EXEC CICS READ                                               EL656
02188          UPDATE                                                   EL656
02189          DATASET   (CNTL-FILE-ID)                                 EL656
02190          SET       (ADDRESS OF CONTROL-FILE)                      EL656
02191          RIDFLD    (ELCNTL-KEY)                                   EL656
02192      END-EXEC.                                                    EL656
02193                                                                   EL656
02194      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL656
02195      MOVE 'B'                    TO JP-RECORD-TYPE.               EL656
02196      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL656
081413*    PERFORM 8400-LOG-JOURNAL-RECORD
081413*                                thru 8400-exit
02198                                                                   EL656
02199      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.       EL656
02200                                                                   EL656
02201      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL656
02202      MOVE 'C'                    TO JP-RECORD-TYPE.               EL656
02203      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL656
02204                                                                   EL656
02205      EXEC CICS REWRITE                                            EL656
02206          DATASET   (CNTL-FILE-ID)                                 EL656
02207          FROM      (CONTROL-FILE)                                 EL656
02208      END-EXEC.                                                    EL656
02209                                                                   EL656
081413*    PERFORM 8400-LOG-JOURNAL-RECORD
081413*                                thru 8400-exit

           .
02212  8000-EXIT.                                                       EL656
02213       EXIT.                                                       EL656
02214      EJECT                                                        EL656
02215                                                                   EL656
02216  8100-SEND-INITIAL-MAP.                                           EL656
02217      MOVE EIBTIME              TO TIME-IN.                        EL656
02218      MOVE SAVE-DATE            TO RUNDATEO.                       EL656
02219      MOVE TIME-OUT             TO RUNTIMEO.                       EL656
101501     MOVE PI-COMPANY-ID        TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID      TO USERIDO.
02220      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL656
02221      MOVE -1                   TO MAINTL.                         EL656
02222      MOVE PI-AH-OVERRIDE-L6    TO AHPLANO.                        EL656
02223      MOVE PI-LIFE-OVERRIDE-L6  TO LFPLANO.                        EL656
02224                                                                   EL656
02225      EXEC CICS SEND                                               EL656
02226          MAP   (WS-MAPNAME)                                       EL656
02227          MAPSET(WS-MAPSET-NAME)                                   EL656
02228          FROM  (EL656AO)                                          EL656
02229          ERASE                                                    EL656
02230          CURSOR                                                   EL656
02231      END-EXEC.                                                    EL656
02232                                                                   EL656
02233      GO TO 9100-RETURN-TRAN.                                      EL656
02234                                                                   EL656
02235  8200-SEND-DATAONLY.                                              EL656
02236      MOVE EIBTIME              TO TIME-IN.                        EL656
02237      MOVE SAVE-DATE            TO RUNDATEO.                       EL656
02238      MOVE TIME-OUT             TO RUNTIMEO.                       EL656
101501     MOVE PI-COMPANY-ID        TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID      TO USERIDO.
02239      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL656
02240                                                                   EL656
02241      EXEC CICS SEND                                               EL656
02242          MAP   (WS-MAPNAME)                                       EL656
02243          MAPSET(WS-MAPSET-NAME)                                   EL656
02244          FROM  (EL656AO)                                          EL656
02245          DATAONLY                                                 EL656
02246          CURSOR                                                   EL656
02247      END-EXEC.                                                    EL656
02248                                                                   EL656
02249      GO TO 9100-RETURN-TRAN.                                      EL656
02250                                                                   EL656
02251  8300-SEND-TEXT.                                                  EL656
02252      EXEC CICS SEND TEXT                                          EL656
02253          FROM  (LOGOFF-TEXT)                                      EL656
02254          LENGTH(LOGOFF-LENGTH)                                    EL656
02255          ERASE                                                    EL656
02256          FREEKB                                                   EL656
02257      END-EXEC.                                                    EL656
02258                                                                   EL656
02259      EXEC CICS RETURN                                             EL656
02260      END-EXEC.                                                    EL656

081413 8400-LOG-JOURNAL-RECORD.
081413 
081413     if pi-journal-file-id = 0
081413        go to 8400-exit
081413     end-if
081413 
081413     move eibdate                to jp-date
081413     move eibtime                to jp-time
081413     move RATE-FILE-ID           TO JP-FILE-ID
081413     MOVE PI-PROCESSOR-ID        TO JP-USER-ID
081413     MOVE 02                     TO PI-JOURNAL-FILE-ID
081413     MOVE THIS-PGM               TO JP-PROGRAM-ID
081413 
081413     EXEC CICS JOURNAL
081413        JFILEID   (PI-JOURNAL-FILE-ID)
081413        JTYPEID   ('EL')
081413        FROM      (JOURNAL-RECORD)
081413        LENGTH    (2000)
081413        resp      (ws-response)
081413     END-EXEC
081413 
081413     if ws-resp-normal
081413        continue
081413     else
081413        display ' error-el656-journal ' ws-response
081413     end-if
081413 
081413     .
081413 8400-exit.
081413     exit.

02275  8800-UNAUTHORIZED-ACCESS.                                        EL656
02276      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL656
02277      GO TO 8300-SEND-TEXT.                                        EL656
02278                                                                   EL656
02279  8810-PF23.                                                       EL656
02280      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL656
02281      MOVE XCTL-005 TO PGM-NAME.                                   EL656
02282      GO TO 9300-XCTL.                                             EL656
02283                                                                   EL656
02284  8880-NOT-FOUND.                                                  EL656
02285      MOVE ER-0142 TO EMI-ERROR.                                   EL656
02286      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL656
02287      MOVE -1   TO MAINTL.                                         EL656
02288                                                                   EL656
02289      IF EIBTRNID NOT = TRANS-ID                                   EL656
02290          GO TO 8100-SEND-INITIAL-MAP.                             EL656
02291                                                                   EL656
02292      GO TO 8200-SEND-DATAONLY.                                    EL656
02293                                                                   EL656
02294  9100-RETURN-TRAN.                                                EL656
02295      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL656
02296      MOVE '656A'               TO PI-CURRENT-SCREEN-NO.           EL656
02297      EXEC CICS RETURN                                             EL656
02298          TRANSID (TRANS-ID)                                       EL656
02299          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL656
02300          LENGTH  (PI-COMM-LENGTH)                                 EL656
02301      END-EXEC.                                                    EL656
02302                                                                   EL656
02303  9200-RETURN-MAIN-MENU.                                           EL656
02304      MOVE XCTL-626 TO PGM-NAME.                                   EL656
02305      GO TO 9300-XCTL.                                             EL656
02306                                                                   EL656
02307  9300-XCTL.                                                       EL656
02308      EXEC CICS XCTL                                               EL656
02309          PROGRAM (PGM-NAME)                                       EL656
02310          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL656
02311          LENGTH  (PI-COMM-LENGTH)                                 EL656
02312      END-EXEC.                                                    EL656
02313                                                                   EL656
02314  9400-CLEAR.                                                      EL656
02315      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL656
02316      GO TO 9300-XCTL.                                             EL656
02317                                                                   EL656
02318  9500-PF12.                                                       EL656
02319      MOVE XCTL-010 TO PGM-NAME.                                   EL656
02320      GO TO 9300-XCTL.                                             EL656
02321                                                                   EL656
02322  9600-PGMID-ERROR.                                                EL656
02323      EXEC CICS HANDLE CONDITION                                   EL656
02324          PGMIDERR(8300-SEND-TEXT)                                 EL656
02325      END-EXEC.                                                    EL656
02326                                                                   EL656
02327      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL656
02328      MOVE ' '          TO PI-ENTRY-CD-1.                          EL656
02329      MOVE XCTL-005     TO PGM-NAME.                               EL656
02330      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL656
02331      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL656
02332      GO TO 9300-XCTL.                                             EL656
02333                                                                   EL656
02334  9700-LINK-DATE-CONVERT.                                          EL656
02335      MOVE LINK-ELDATCV TO PGM-NAME.                               EL656
02336                                                                   EL656
02337      EXEC CICS LINK                                               EL656
02338          PROGRAM (PGM-NAME)                                       EL656
02339          COMMAREA(DATE-CONVERSION-DATA)                           EL656
02340          LENGTH  (DC-COMM-LENGTH)                                 EL656
02341      END-EXEC.                                                    EL656
02342                                                                   EL656
02343  9700-EXIT.                                                       EL656
02344      EXIT.                                                        EL656
02345                                                                   EL656
02346  9900-ERROR-FORMAT.                                               EL656
02347      IF NOT EMI-ERRORS-COMPLETE                                   EL656
02348          MOVE LINK-001 TO PGM-NAME                                EL656
02349          EXEC CICS LINK                                           EL656
02350              PROGRAM (PGM-NAME)                                   EL656
02351              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL656
02352              LENGTH  (EMI-COMM-LENGTH)                            EL656
02353          END-EXEC.                                                EL656
02354  9900-EXIT.                                                       EL656
02355      EXIT.                                                        EL656
02356                                                                   EL656
02357  9990-ABEND.                                                      EL656
02358      MOVE LINK-004 TO PGM-NAME.                                   EL656
02359      MOVE DFHEIBLK               TO EMI-LINE1.                    EL656
02360                                                                   EL656
02361      EXEC CICS LINK                                               EL656
02362          PROGRAM   (PGM-NAME)                                     EL656
02363          COMMAREA  (EMI-LINE1)                                    EL656
02364          LENGTH    (72)                                           EL656
02365      END-EXEC.                                                    EL656
02366                                                                   EL656
02367      GO TO 8200-SEND-DATAONLY.                                    EL656
02368                                                                   EL656
02369      MOVE ZEROS  TO RETURN-CODE.
02369      GOBACK.                                                      EL656
02370                                                                   EL656
02371  9995-SECURITY-VIOLATION.                                         EL656
02372                              COPY ELCSCTP.                        EL656
02373                                                                   EL656
02374  9995-EXIT.                                                       EL656
02375       EXIT.                                                       EL656
