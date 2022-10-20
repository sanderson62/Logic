00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS030
00003  PROGRAM-ID.                ECS030.                                  LV006
00004 *              PROGRAM CONVERTED BY                               ECS030
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS030
00006 *              CONVERSION DATE 11/28/95 11:06:51.                 ECS030
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            ECS030
00008 *                            VMOD=2.007.                          ECS030
00009 *AUTHOR.     LOGIC, INC.                                          ECS030
00010 *            DALLAS, TEXAS.                                       ECS030
00011                                                                   ECS030
00012 *DATE-COMPILED.                                                   ECS030
00013                                                                   ECS030
00014 *SECURITY.   *****************************************************ECS030
00015 *            *                                                   *ECS030
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS030
00017 *            *                                                   *ECS030
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS030
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS030
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS030
00021 *            *                                                   *ECS030
00022 *            *****************************************************ECS030
00023                                                                   ECS030
00024 *REMARKS.                                                         ECS030
00025                                                                   ECS030
00026 *    PRINT CLAIM REGISTER.                                        ECS030
00027                                                                   ECS030
00028  ENVIRONMENT DIVISION.                                            ECS030
00029  CONFIGURATION SECTION.                                           ECS030
00030  SPECIAL-NAMES.                                                   ECS030
00031      C02 IS LCP-CH2                                               ECS030
00032      C03 IS LCP-CH3                                               ECS030
00033      C04 IS LCP-CH4                                               ECS030
00034      C05 IS LCP-CH5                                               ECS030
00035      C06 IS LCP-CH6                                               ECS030
00036      C07 IS LCP-CH7                                               ECS030
00037      C08 IS LCP-CH8                                               ECS030
00038      C09 IS LCP-CH9                                               ECS030
00039      C10 IS LCP-CH10                                              ECS030
00040      C11 IS LCP-CH11                                              ECS030
00041      C12 IS LCP-CH12                                              ECS030
00042      S01 IS LCP-P01                                               ECS030
00043      S02 IS LCP-P02.                                              ECS030
00044  INPUT-OUTPUT SECTION.                                            ECS030
00045  FILE-CONTROL.                                                    ECS030
00046                                                                   ECS030
00047      SELECT  EXTRACT     ASSIGN TO SYS018-UT-2400-S-SYS018.       ECS030
00048      SELECT  PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS030
00049      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS030
00050      SELECT  CLM-CARR    ASSIGN TO SYS015-UT-3380-S-SYS015.       ECS030
00051      SELECT  CLM-SRT     ASSIGN TO SYS001-UT-3380-S-SORTWK1.      ECS030
00052      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS030
00053                                                                   ECS030
00054      SELECT ERMEBL                                                ECS030
00055              ASSIGN SYS024-3380-ERMEBL                            ECS030
00056              ORGANIZATION INDEXED                                 ECS030
00057              ACCESS DYNAMIC                                       ECS030
00058              RECORD KEY ME-CONTROL-PRIMARY                        ECS030
00059              FILE STATUS ERMEBL-FILE-STATUS.                      ECS030
00060      EJECT                                                        ECS030
00061  DATA DIVISION.                                                   ECS030
00062  FILE SECTION.                                                    ECS030
00063                                                                   ECS030
00064  FD  EXTRACT                                                      ECS030
00065                              COPY ECSEXTFD.                       ECS030
00066                              COPY ECSEXT01.                       ECS030
00067      EJECT                                                        ECS030
00068  FD  DISK-DATE                                                    ECS030
00069                              COPY ELCDTEFD.                       ECS030
00070      EJECT                                                        ECS030
00071  FD  PRNTR                                                        ECS030
00072                              COPY ELCPRTFD.                       ECS030
00073  01  DTL.                                                         ECS030
00074      12  FILLER      PIC X(3).                                    ECS030
00075      12  P-ST        PIC XX.                                      ECS030
00076      12  FILLER      PIC X(4).                                    ECS030
00077      12  P-ACCT      PIC X(10).                                   ECS030
00078      12  FILLER      PIC X.                                       ECS030
00079      12  P-CERT      PIC X(11).                                   ECS030
00080      12  FILLER      PIC X.                                       ECS030
00081      12  P-EMO       PIC 99.                                      ECS030
00082      12  P-EMOD      PIC X.                                       ECS030
00083      12  P-EDA       PIC 99.                                      ECS030
00084      12  P-EDAD      PIC X.                                       ECS030
00085      12  P-EYR       PIC 99.                                      ECS030
00086      12  FILLER      PIC X.                                       ECS030
00087      12  P-NAME      PIC X(13).                                   ECS030
00088      12  FILLER      PIC X.                                       ECS030
00089      12  P-INL1      PIC X.                                       ECS030
00090      12  P-INL2      PIC X.                                       ECS030
00091      12  FILLER      PIC X.                                       ECS030
00092      12  P-TYPE      PIC X(7).                                    ECS030
00093      12  P-IMO       PIC 99.                                      ECS030
00094      12  P-IMOD      PIC X.                                       ECS030
00095      12  P-IDA       PIC 99.                                      ECS030
00096      12  P-IDAD      PIC X.                                       ECS030
00097      12  P-IYR       PIC 99.                                      ECS030
00098      12  FILLER      PIC XX.                                      ECS030
00099      12  P-PMO       PIC 99.                                      ECS030
00100      12  P-PMOD      PIC X.                                       ECS030
00101      12  P-PDA       PIC 99.                                      ECS030
00102      12  P-PDAD      PIC X.                                       ECS030
00103      12  P-PYR       PIC 99.                                      ECS030
00104      12  FILLER      PIC X.                                       ECS030
00105      12  P-CLMNO     PIC X(7).                                    ECS030
00106      12  FILLER      PIC X.                                       ECS030
00107      12  P-DTH       PIC ZZZZ,ZZZ.99-  BLANK WHEN ZERO.           ECS030
00108      12  FILLER      PIC X.                                       ECS030
00109      12  P-DIS       PIC ZZ,ZZZ.99-   BLANK WHEN ZERO.            ECS030
00110      12  FILLER      PIC X.                                       ECS030
00111      12  P-CAUSE     PIC X(6).                                    ECS030
00112      12  FILLER      PIC X.                                       ECS030
00113      12  P-AGE       PIC XX.                                      ECS030
00114      12  FILLER      PIC X.                                       ECS030
00115      12  P-CHK-NO    PIC X(7).                                    ECS030
00116                                                                   ECS030
00117  01  DTL-C.                                                       ECS030
00118      12  FILL    PIC X.                                           ECS030
00119      12  COPNY   PIC X(6).                                        ECS030
00120      12  FILL    PIC X.                                           ECS030
00121      12  C-ST    PIC XX.                                          ECS030
00122      12  FILL    PIC X.                                           ECS030
00123      12  C-ACCT  PIC X(10).                                       ECS030
00124      12  FILL    PIC X.                                           ECS030
00125      12  C-CERT  PIC X(11).                                       ECS030
00126      12  FILL    PIC X.                                           ECS030
00127      12  C-EMO   PIC XX.                                          ECS030
00128      12  C-EDA   PIC XX.                                          ECS030
00129      12  C-EYR   PIC XX.                                          ECS030
00130      12  FILL    PIC X(93).                                       ECS030
00131      EJECT                                                        ECS030
00132  FD  CLM-CARR                                                     ECS030
00133      BLOCK CONTAINS 0 RECORDS
00134      RECORDING MODE IS F.                                         ECS030
00135                                                                   ECS030
00136  01  CAR-REC.                                                     ECS030
00137      12  FILLER          PIC X(504).                              ECS030
00138      12  C-COPNY         PIC X(6).                                ECS030
00139      EJECT                                                        ECS030
00140  SD  CLM-SRT.                                                     ECS030
00141                                                                   ECS030
00142  01  SRT-REC.                                                     ECS030
00143      12  FILLER          PIC X(4).                                ECS030
00144      12  S-CAR           PIC X.                                   ECS030
00145      12  S-CO            PIC X(6).                                ECS030
00146      12  FILLER          PIC X(360).                              ECS030
00147      12  S-CLM           PIC X(7).                                ECS030
00148      12  S-CHK           PIC X(7).                                ECS030
00149      12  FILLER          PIC X(119).                              ECS030
00150      12  S-COPNY         PIC X(6).                                ECS030
00151      EJECT                                                        ECS030
00152  FD  FICH                                                         ECS030
00153                              COPY ELCFCHFD.                       ECS030
00154      EJECT                                                        ECS030
00155  FD  ERMEBL.                                                      ECS030
00156                                                                   ECS030
00157                         COPY ERCMEBL.                             ECS030
00158      EJECT                                                        ECS030
00159                                                                   ECS030
00160  WORKING-STORAGE SECTION.                                         ECS030
00161  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS030
00162  77  LCP-ASA                       PIC X.                         ECS030
00163  77  FILLER  PIC X(32) VALUE '********************************'.  ECS030
00164  77  FILLER  PIC X(32) VALUE '     ECS030 WORKING STORAGE     '.  ECS030
00165  77  FILLER  PIC X(32) VALUE '*****VMOD=2.007 ****************'.  ECS030
00166                                                                   ECS030
00167  77  CLM-CTR     PIC 9(6)    VALUE 0.                             ECS030
00168  77  WS-DE-COMP  PIC X(7)    VALUE SPACES.                        ECS030
00169  77  DASH-A      PIC X(3)    VALUE '-P '.                         ECS030
00170  77  DASH-E      PIC X(3)    VALUE '-E '.                         ECS030
00171  77  DASH-F      PIC X(3)    VALUE '-F '.                         ECS030
00172  77  DASH-P      PIC X(3)    VALUE '-P '.                         ECS030
00173  77  DASH-V      PIC X(3)    VALUE '-V '.                         ECS030
00174                                                                   ECS030
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Month end balancing work area                           ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00175  01  MONTH-END-DATA.                                              ECS030
00176      12  ME-START-DATE.                                           ECS030
00177          16  ME-START-MO         PIC 99.                          ECS030
00178          16  FILLER              PIC X.                           ECS030
00179          16  ME-START-DA         PIC 99.                          ECS030
00180          16  FILLER              PIC X.                           ECS030
00181          16  ME-START-YR         PIC 99.                          ECS030
00182      12  ME-CNDS-DATE            PIC 9(6).                        ECS030
00183      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS030
00184          16  ME-CNDS-MO          PIC 99.                          ECS030
00185          16  ME-CNDS-DA          PIC 99.                          ECS030
00186          16  ME-CNDS-YR          PIC 99.                          ECS030
00187      12  ME-START-TIME           PIC 9(6).                        ECS030
00188      12 ME-UPDATE-FLAG           PIC X VALUE 'Y'.                 ECS030
00189          88  ME-DO-UPDATE        VALUE 'Y'.                       ECS030
00190          88  ME-NO-UPDATE        VALUE 'N'.                       ECS030
00191      12  ERMEBL-FILE-STATUS      PIC XX.                          ECS030
00192      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                ECS030
070714     12  hld-030-CLMS-L          PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-030-CLMS-AH         PIC S9(9)V99  COMP-3 value +0.          
00193                                                                   ECS030
00194  01  WS.                                                          ECS030
00195      05  WS-ABEND-FILE-STATUS PIC X(4) VALUE SPACE.               ECS030
00196      05  WS-ABEND-MESSAGE     PIC X(80) VALUE ZEROS.              ECS030
00197      05  WS-ZERO              PIC S9    VALUE ZERO.               ECS030
00198      05  WS-RETURN-CODE       PIC X(4)  VALUE ZEROS.              ECS030
00199      05  ABEND-OPTION         PIC X VALUE 'Y'.                    ECS030
00200      05  PGM-SUB              PIC S999    COMP    VALUE +030.     ECS030
00201      EJECT                                                        ECS030
00202                              COPY ELCDTECX.                       ECS030
00203                                                                   ECS030
00204                              COPY ELCDTEVR.                       ECS030
00205                                                                   ECS030
00206  01  HD1.                                                         ECS030
00207      12  FILLER      PIC X(45)   VALUE SPACES.                    ECS030
00208      12  FILLER PIC X(29) VALUE 'CLAIM REGISTER FOR CARRIER - '.  ECS030
00209      12  HD1-CARR    PIC X.                                       ECS030
00210      12  HD-COMP     PIC X(9)    VALUE ' GROUP - '.               ECS030
00211      12  HD1-GROUP   PIC X(6).                                    ECS030
00212      12  FILLER      PIC X(29)    VALUE SPACES.                   ECS030
00213      12  FILLER      PIC X(8) VALUE 'ECS030  '.                   ECS030
00214                                                                   ECS030
00215  01  HD1-A.                                                       ECS030
00216      12  FILL    PIC X(47) VALUE SPACES.                          ECS030
00217      12  FILL    PIC X(29) VALUE 'CLAIM REGISTER FOR CARRIER - '. ECS030
00218      12  HD-CARR PIC X.                                           ECS030
00219      12  FILLER  PIC X(42) VALUE SPACES.                          ECS030
00220      12  FILL    PIC X(8) VALUE 'ECS030  '.                       ECS030
00221                                                                   ECS030
00222  01  HD2.                                                         ECS030
00223      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS030
00224      12  HD-CO       PIC X(30).                                   ECS030
00225      12  FILLER      PIC X(42)   VALUE SPACES.                    ECS030
00226      12  HD-RD       PIC X(8).                                    ECS030
00227                                                                   ECS030
00228  01  HD3.                                                         ECS030
00229      12  FILLER      PIC X(53)   VALUE SPACES.                    ECS030
00230      12  HD-DT       PIC X(18).                                   ECS030
00231      12  FILLER      PIC X(48)   VALUE SPACES.                    ECS030
00232      12  FILLER      PIC X(5)    VALUE 'PAGE '.                   ECS030
00233      12  HD-PG       PIC ZZ,ZZ9.                                  ECS030
00234                                                                   ECS030
00235  01  HD4.                                                         ECS030
00236      12  HD4-1   PIC X(28) VALUE ' STATE   ACCOUNT      CERT. '.  ECS030
00237      12  FILLER  PIC X(9)  VALUE '   EFFECT'.                     ECS030
00238      12  FILLER  PIC X(23)  VALUE  'IVE INSURED         TYP'.     ECS030
00239      12  FILLER  PIC X(23)  VALUE 'E   INCURRED  DATE    C'.      ECS030
00240      12  FILLER  PIC X(25)  VALUE 'LAIM     ---AMOUNT OF CLA'.    ECS030
00241      12  FILLER  PIC X(25)  VALUE 'IM---   CAU   AGE CHECK '.     ECS030
00242                                                                   ECS030
00243  01  HD5.                                                         ECS030
00244      12  FILLER  PIC X(12)  VALUE SPACES.                         ECS030
00245      12  FILLER  PIC X(25)  VALUE '                      DAT'.    ECS030
00246      12  FILLER  PIC X(23)  VALUE 'E                      '.      ECS030
00247      12  FILLER  PIC X(23)  VALUE '      DATE    PAID    N'.      ECS030
00248      12  FILLER  PIC X(13)  VALUE 'UMBER       '.                 ECS030
00249      12  HD4-OV1 PIC X(13)  VALUE SPACE.                          ECS030
00250      12  HD4-OV2 PIC X(7)   VALUE '     CD'.                      ECS030
00251      12  FILLER  PIC X(19)  VALUE '         NUMBER    '.          ECS030
00252                                                                   ECS030
00253  01  HD5G.                                                        ECS030
00254      12  FILLER  PIC X(12)  VALUE SPACES.                         ECS030
00255      12  FILLER  PIC X(25) VALUE '                    DATE '.     ECS030
00256      12  FILLER  PIC X(23)  VALUE '                       '.      ECS030
00257      12  FILLER  PIC X(23)  VALUE '     DATE       PAID   '.      ECS030
00258      12  FILLER  PIC X(14)  VALUE '  NUMBER     '.                ECS030
00259      12  HD5-OV1 PIC X(11)  VALUE SPACE.                          ECS030
00260      12  HD5-OV2 PIC X(10)  VALUE SPACE.                          ECS030
00261      12  FILLER  PIC X(17)  VALUE '           NUMBER'.            ECS030
00262                                                                   ECS030
00263  01  HD6.                                                         ECS030
00264      12  FILL    PIC X(28) VALUE ' GROUP ST    ACCOUNT    CERT'.  ECS030
00265      EJECT                                                        ECS030
00266  01  COMP-3-AREA     COMP-3.                                      ECS030
00267      12  PG-NO       PIC S9(5)       VALUE +0.                    ECS030
00268      12  LN-CT       PIC S9(5)       VALUE +0.                    ECS030
00269      12  CNT-L       PIC S9(6)       VALUE +0.                    ECS030
00270      12  CNT-A       PIC S9(6)       VALUE +0.                    ECS030
00271      12  T-DTH       PIC S9(7)V99    VALUE +0.                    ECS030
00272      12  T-DIS       PIC S9(7)V99    VALUE +0.                    ECS030
00273      12  G-TOTCNT-L  PIC S9(6)       VALUE +0.                    ECS030
00274      12  G-TOTCNT-A  PIC S9(6)       VALUE +0.                    ECS030
00275      12  G-TOTDTH    PIC S9(7)V99    VALUE +0.                    ECS030
00276      12  G-TOTDIS    PIC S9(7)V99    VALUE +0.                    ECS030
00277      12  A-T-CNT-L   PIC S9(6)       VALUE +0.                    ECS030
00278      12  A-T-CNT-A   PIC S9(6)       VALUE +0.                    ECS030
00279      12  A-T-DTH     PIC S9(7)V99    VALUE +0.                    ECS030
00280      12  A-T-DIS     PIC S9(7)V99    VALUE +0.                    ECS030
00281                                                                   ECS030
00282  01  MISC-WORK-AREA.                                              ECS030
00283 *    12  WK-DT.                                                      CL**2
00284 *        16  W-MO     PIC 99.                                        CL**2
00285 *        16  W-DA     PIC 99.                                        CL**2
00286 *        16  W-YR     PIC 99.                                        CL**2
00287      12  WK-DT          PIC 9(11).                                   CL**4
00288      12  WK-DT-RDEF  REDEFINES  WK-DT.                               CL**2
00289          16 FILLER      PIC 999.                                  ECS030
00290          16  W-CCYY     PIC 9(04).                                   CL**2
00291          16  W-CCYR-2 REDEFINES W-CCYY.                              CL**3
00292              20  W-CC   PIC 99.                                      CL**2
00293              20  W-YR   PIC 99.                                      CL**2
00294          16  W-MO       PIC 99.                                      CL**2
00295          16  W-DA       PIC 99.                                      CL**2
00296      12  X           PIC X       VALUE SPACE.                     ECS030
00297      12  CAR-SW      PIC X       VALUE SPACE.                     ECS030
00298      12  SAV-COMP    PIC X(7)    VALUE SPACES.                    ECS030
00299      12  PREV-ACT    PIC X(10)   VALUE SPACES.                    ECS030
00300      12  SAV-PND-SW  PIC X.                                       ECS030
00301                                                                   ECS030
00302  01  TL-LN.                                                       ECS030
00303      12  FILLER      PIC X(8)   VALUE SPACES.                     ECS030
00304      12  TLC         PIC X(24) VALUE 'TOTAL FOR CARRIER/GROUP '.  ECS030
00305      12  TL-CO       PIC X(7).                                    ECS030
00306      12  FILLER      PIC X(15)  VALUE SPACES.                     ECS030
00307      12  TL-CNT-L    PIC ZZZ,ZZ9.                                 ECS030
00308      12  FILLER      PIC X      VALUE SPACE.                      ECS030
00309      12  TL-OV-L     PIC XX     VALUE 'LF'.                       ECS030
00310      12  FILLER      PIC X(5)   VALUE ' AND'.                     ECS030
00311      12  TL-CNT-A    PIC ZZZ,ZZ9.                                 ECS030
00312      12  FILLER      PIC X      VALUE SPACE.                      ECS030
00313      12  TL-OV-A     PIC XX     VALUE 'AH'.                       ECS030
00314      12  FILLER      PIC X(12)  VALUE ' CLAIMS FOR '.             ECS030
00315      12  TL-DTH      PIC $(5),$$$.99-.                            ECS030
00316      12  TL-DIS      PIC $(5),$$$.99-.                            ECS030
00317                                                                   ECS030
00318  01  G-T-LN.                                                      ECS030
00319      12  FILL        PIC X(8)    VALUE SPACE.                     ECS030
00320      12  FILL        PIC X(18)   VALUE 'GRAND TOTALS'.            ECS030
00321      12  FILL        PIC X(23)   VALUE SPACE.                     ECS030
00322      12  GT-CNT-L    PIC ZZZZ,ZZ9.                                ECS030
00323      12  FILLER      PIC X      VALUE SPACE.                      ECS030
00324      12  GT-OV-L     PIC XX     VALUE 'LF'.                       ECS030
00325      12  FILLER      PIC X(5)   VALUE ' AND'.                     ECS030
00326      12  GT-CNT-A    PIC ZZZ,ZZ9.                                 ECS030
00327      12  FILLER      PIC X      VALUE SPACE.                      ECS030
00328      12  GT-OV-A     PIC XX     VALUE 'AH'.                       ECS030
00329      12  FILLNT      PIC X(12)   VALUE ' CLAIMS FOR '.            ECS030
00330      12  GT-DTH      PIC $(5),$$$.99-.                            ECS030
00331      12  FILLER      PIC X(3)    VALUE SPACES.                    ECS030
00332      12  GT-DIS      PIC $(5),$$$.99-.                            ECS030
00333      EJECT                                                        ECS030
00334  PROCEDURE DIVISION.                                              ECS030
00335  CAPTURE-START.                                                   ECS030
00342                                                                   ECS030
00343  0100-SET-START.                                                  ECS030
00344                              COPY ELCDTERX.                       ECS030
00345                                                                   ECS030
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Set up the month-end auto balancing.                    ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00346      MOVE WS-TIME                TO ME-START-TIME.                ECS030
00347      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                ECS030
00348      MOVE ME-START-MO            TO ME-CNDS-MO.                   ECS030
00349      MOVE ME-START-DA            TO ME-CNDS-DA.                   ECS030
00350      MOVE ME-START-YR            TO ME-CNDS-YR.                   ECS030

00351      MOVE LIFE-OVERRIDE-L2       TO TL-OV-L                       ECS030
00352                                     GT-OV-L.                      ECS030
00353      MOVE AH-OVERRIDE-L2         TO TL-OV-A                       ECS030
00354                                     GT-OV-A.                      ECS030
00355                                                                   ECS030
00364      OPEN INPUT EXTRACT                                           ECS030
00365           OUTPUT PRNTR.                                           ECS030
00366                                                                   ECS030
00367      MOVE COMPANY-NAME           TO HD-CO.                        ECS030
00368      MOVE ALPH-DATE              TO HD-DT.                        ECS030
00369      MOVE WS-CURRENT-DATE        TO HD-RD.                        ECS030
00370                                                                   ECS030
00371  0110-SORT-IN SECTION.                                            ECS030
00372      SORT CLM-SRT                                                 ECS030
00373          ASCENDING KEY S-CAR S-CLM S-CHK                          ECS030
00374          INPUT PROCEDURE 0120-GET-CLAIM THRU 0400-STEP-ONE-X      ECS030
00375          GIVING CLM-CARR.                                         ECS030
00376                                                                   ECS030
00377      IF SORT-RETURN NOT = ZEROS                                   ECS030
00378          MOVE 'INTERNAL SORT 01 ABORTED' TO WS-ABEND-MESSAGE      ECS030
00379          MOVE '0101'                     TO WS-RETURN-CODE        ECS030
00380          GO TO ABEND-PGM.                                         ECS030
00381                                                                   ECS030
00382      GO TO 0410-STEP-TWO.                                         ECS030
00383      EJECT                                                        ECS030
00384  0120-GET-CLAIM    SECTION.                                       ECS030
00385      READ EXTRACT                                                 ECS030
00386          AT END  GO TO 0390-END-STEP.                             ECS030
00387                                                                   ECS030
00388      IF DE-RECORD-ID NOT = 'DE'                                   ECS030
00389          GO TO 0120-GET-CLAIM.                                    ECS030
00390                                                                   ECS030
00391      IF DE-REIN NOT = SPACE                                       ECS030
00392          GO TO 0120-GET-CLAIM.                                    ECS030
00393                                                                   ECS030
00394      IF DE-PAY-CODE = 'Y' OR 'Z'                                  ECS030
00395          GO TO 0120-GET-CLAIM.                                    ECS030
00396                                                                   ECS030
00397      IF NOT DE-CLAIM                                              ECS030
00398          GO TO 0120-GET-CLAIM.                                    ECS030
00399                                                                   ECS030
00400      ADD 1 TO CLM-CTR.                                            ECS030
00401                                                                   ECS030
00402      IF PREV-ACT = SPACE                                          ECS030
00403          MOVE DE-ACCOUNT         TO PREV-ACT.                     ECS030
00404                                                                   ECS030
00405      STRING DE-CARRIER  DELIMITED BY SIZE                         ECS030
00406             DE-GROUPING DELIMITED BY SIZE                         ECS030
00407      INTO WS-DE-COMP.                                             ECS030
00408                                                                   ECS030
00409  0130-MAIN-LOOP.                                                  ECS030
00410      IF SAV-COMP = SPACES                                         ECS030
00411          GO TO 0190-SET-NEW.                                      ECS030
00412      IF WS-DE-COMP NOT = SAV-COMP                                 ECS030
00413          GO TO 0180-PRT-TOTALS.                                   ECS030
00414      IF CAR-SW NOT = SPACE                                        ECS030
00415          GO TO 0140-CONT-RPT.                                     ECS030
00416      IF DE-ACCOUNT NOT = PREV-ACT                                 ECS030
00417          PERFORM 0370-ACCT-TOT THRU 0380-ACCT-XIT.                ECS030
00418                                                                   ECS030
00419  0140-CONT-RPT.                                                   ECS030
00420      MOVE SPACES                 TO DTL.                          ECS030
00421      MOVE DE-STATE               TO P-ST.                         ECS030
00422      IF CAR-SW = '9'                                              ECS030
00423          MOVE C-COPNY            TO COPNY                         ECS030
00424          MOVE DE-STATE           TO C-ST                          ECS030
00425          MOVE DE-ACCOUNT         TO C-ACCT                        ECS030
00426          MOVE DE-CERT            TO C-CERT                        ECS030
00427          MOVE DE-EFF             TO WK-DT                            CL**2
00428          MOVE W-MO               TO C-EMO                            CL**5
00429          MOVE W-DA               TO C-EDA                            CL**5
00430          MOVE W-YR               TO C-EYR                            CL**5
00431      ELSE                                                         ECS030
00432          MOVE DE-ACCOUNT         TO P-ACCT                        ECS030
00433          MOVE DE-CERT            TO P-CERT                        ECS030
00434          MOVE DE-EFF             TO WK-DT                            CL**2
00435          MOVE W-MO               TO P-EMO                            CL**5
00436          MOVE W-DA               TO P-EDA                            CL**5
00437          MOVE W-YR               TO P-EYR                            CL**5
00438          MOVE '-'                TO P-EMOD  P-EDAD.               ECS030
00439                                                                   ECS030
00440      MOVE DE-LNAME               TO P-NAME.                       ECS030
00441      MOVE DE-1ST-INIT-FNAME      TO P-INL1.                       ECS030
00442      MOVE DE-INIT                TO P-INL2.                       ECS030
00443      MOVE DE-CNUM                TO P-CLMNO.                      ECS030
00444                                                                   ECS030
00445      MOVE ZEROS                  TO P-DTH  P-DIS.                 ECS030
00446                                                                   ECS030
00447      IF  DE-DTH  OR  DE-OB-DTH                                    ECS030
00448          GO TO 0150-PT-DEATH.                                     ECS030
00449      IF  DE-AH  OR  DE-OB-AH                                      ECS030
00450          GO TO 0160-PT-DISAB.                                     ECS030
00451                                                                   ECS030
00452      MOVE 'UNKNO'                TO  P-TYPE.                      ECS030
00453      GO TO 0170-PT-LST.                                              CL**6
00454                                                                   ECS030
00455  0150-PT-DEATH.                                                   ECS030
00456      MOVE LIFE-OVERRIDE-L6       TO P-TYPE.                       ECS030
00457                                                                   ECS030
00458      IF DE-PAY-CODE = 'E'                                         ECS030
00459         MOVE SPACES              TO P-TYPE                        ECS030
00460         STRING LIFE-OVERRIDE-L2 DELIMITED BY SIZE                 ECS030
00461                DASH-E           DELIMITED BY SIZE                 ECS030
00462                INTO  P-TYPE.                                      ECS030
00463                                                                   ECS030
00464      IF DE-PAY-CODE = 'F'                                         ECS030
00465         MOVE SPACES              TO P-TYPE                        ECS030
00466         STRING LIFE-OVERRIDE-L2 DELIMITED BY SIZE                 ECS030
00467                DASH-F           DELIMITED BY SIZE                 ECS030
00468                INTO  P-TYPE.                                      ECS030
00469                                                                   ECS030
00470      IF DE-PAY-CODE = 'P'                                         ECS030
00471         MOVE SPACES              TO P-TYPE                        ECS030
00472         STRING LIFE-OVERRIDE-L2 DELIMITED BY SIZE                 ECS030
00473                DASH-P           DELIMITED BY SIZE                 ECS030
00474                INTO  P-TYPE.                                      ECS030
00475                                                                   ECS030
00476      IF DE-PAY-CODE = 'V'                                         ECS030
00477         MOVE SPACES              TO P-TYPE                        ECS030
00478         STRING LIFE-OVERRIDE-L2 DELIMITED BY SIZE                 ECS030
00479                DASH-P           DELIMITED BY SIZE                 ECS030
00480                INTO  P-TYPE.                                      ECS030
00481                                                                   ECS030
00482      ADD  DE-CLAIM-AMT  TO  T-DTH  A-T-DTH.                       ECS030
00483      MOVE  DE-CLAIM-AMT          TO  P-DTH.                       ECS030
00484                                                                   ECS030
00485      ADD +1 TO CNT-L  A-T-CNT-L.                                  ECS030
00486                                                                   ECS030
00487      GO TO 0170-PT-LST.                                           ECS030
00488                                                                   ECS030
00489  0160-PT-DISAB.                                                   ECS030
00490      MOVE AH-OVERRIDE-L6         TO P-TYPE.                       ECS030
00491                                                                   ECS030
00492      IF DE-PAY-CODE = 'F'                                         ECS030
00493         MOVE SPACE               TO P-TYPE                        ECS030
00494         STRING AH-OVERRIDE-L2 DELIMITED BY SIZE                   ECS030
00495                DASH-F         DELIMITED BY SIZE                   ECS030
00496                INTO  P-TYPE.                                      ECS030
00497                                                                   ECS030
00498      IF DE-PAY-CODE = 'P'                                         ECS030
00499         MOVE SPACE               TO P-TYPE                        ECS030
00500         STRING AH-OVERRIDE-L2 DELIMITED BY SIZE                   ECS030
00501                DASH-P         DELIMITED BY SIZE                   ECS030
00502                INTO  P-TYPE.                                      ECS030
00503                                                                   ECS030
00504      IF DE-PAY-CODE = 'A'                                         ECS030
00505         MOVE SPACE               TO P-TYPE                        ECS030
00506         STRING AH-OVERRIDE-L2 DELIMITED BY SIZE                   ECS030
00507                DASH-A         DELIMITED BY SIZE                   ECS030
00508                INTO  P-TYPE.                                      ECS030
00509                                                                   ECS030
00510      IF DE-PAY-CODE = 'V'                                         ECS030
00511         MOVE SPACE               TO P-TYPE                        ECS030
00512         STRING AH-OVERRIDE-L2 DELIMITED BY SIZE                   ECS030
00513                DASH-V         DELIMITED BY SIZE                   ECS030
00514                INTO  P-TYPE.                                      ECS030
00515                                                                   ECS030
00516      IF DE-PAY-CODE = 'E'                                         ECS030
00517         MOVE SPACE               TO P-TYPE                        ECS030
00518         STRING AH-OVERRIDE-L2 DELIMITED BY SIZE                   ECS030
00519                DASH-E         DELIMITED BY SIZE                   ECS030
00520                INTO  P-TYPE.                                      ECS030
00521                                                                   ECS030
00522      ADD  DE-CLAIM-AMT  TO  T-DIS  A-T-DIS.                       ECS030
00523      MOVE  DE-CLAIM-AMT          TO  P-DIS.                       ECS030
00524                                                                   ECS030
00525      ADD +1 TO CNT-A  A-T-CNT-A.                                  ECS030
00526                                                                   ECS030
00527  0170-PT-LST.                                                     ECS030
00528      MOVE DE-ACCOUNT             TO PREV-ACT                      ECS030
00529      MOVE DE-INCUR               TO WK-DT.                           CL**2
00530      MOVE W-MO                   TO P-IMO.                           CL**6
00531      MOVE W-DA                   TO P-IDA.                           CL**6
00532      MOVE W-YR                   TO P-IYR.                           CL**6
00533      MOVE '-'                    TO P-IMOD  P-IDAD.               ECS030
00534      MOVE DE-CLM-AGE             TO P-AGE.                        ECS030
00535      MOVE DE-PAY                 TO WK-DT.                        ECS030
00536      MOVE W-MO                   TO P-PMO.                           CL**5
00537      MOVE W-DA                   TO P-PDA.                           CL**5
00538      MOVE W-YR                   TO P-PYR.                           CL**5
00539      MOVE '-'                    TO P-PMOD  P-PDAD.               ECS030
00540      MOVE DE-CHECK               TO P-CHK-NO.                     ECS030
00541      MOVE DE-CLM-CAUSE           TO P-CAUSE.                      ECS030
00542                                                                   ECS030
00543      MOVE ' '                    TO X.                            ECS030
00544      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00545                                                                   ECS030
00546      ADD +1 TO LN-CT.                                             ECS030
00547      IF LN-CT GREATER THAN 49                                     ECS030
00548          PERFORM 0200-PT-HDNG THRU 0230-ALL-HDNG-E.               ECS030
00549                                                                   ECS030
00550      IF CAR-SW = SPACE                                            ECS030
00551          PERFORM 0250-WRITE-CAR THRU 0260-W-CAR-END.              ECS030
00552                                                                   ECS030
00553      IF CAR-SW = '9'                                              ECS030
00554          GO TO 0280-GET-CAR.                                      ECS030
00555                                                                   ECS030
00556      GO TO 0120-GET-CLAIM.                                        ECS030
00557                                                                   ECS030
00558      EJECT                                                        ECS030
00559  0180-PRT-TOTALS.                                                 ECS030
00560      IF CAR-SW NOT = ALL '9'                                      ECS030
00561          PERFORM 0370-ACCT-TOT THRU 0380-ACCT-XIT                 ECS030
00562          MOVE 'TOTAL FOR CARRIER/GROUP' TO TLC.                   ECS030
00563                                                                   ECS030
00564      MOVE CNT-L                  TO TL-CNT-L.                     ECS030
00565      MOVE CNT-A                  TO TL-CNT-A.                     ECS030
00566      MOVE T-DTH                  TO TL-DTH.                       ECS030
00567      MOVE T-DIS                  TO TL-DIS.                       ECS030
00568      ADD T-DTH TO G-TOTDTH.                                       ECS030
00569      ADD T-DIS TO G-TOTDIS.                                       ECS030
00570      ADD CNT-L TO G-TOTCNT-L.                                     ECS030
00571      ADD CNT-A TO G-TOTCNT-A.                                     ECS030
00572      MOVE TL-LN                  TO P-DATA.                       ECS030
00573      MOVE '0'                    TO X.                            ECS030
00574      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00575      MOVE SPACES                 TO SAV-COMP.                     ECS030
00576                                                                   ECS030
00577  0190-SET-NEW.                                                    ECS030
00578      IF CAR-SW = '9'                                              ECS030
00579          MOVE DE-CARRIER         TO HD-CARR  SAV-COMP             ECS030
00580          MOVE HD1-A              TO HD1                           ECS030
00581      ELSE                                                         ECS030
00582          MOVE DE-GROUPING        TO HD1-GROUP                     ECS030
00583          MOVE DE-CARRIER         TO HD1-CARR                      ECS030
00584          MOVE WS-DE-COMP         TO SAV-COMP.                     ECS030
00585                                                                   ECS030
00586      MOVE DE-ACCOUNT             TO PREV-ACT.                     ECS030
00587      MOVE ZEROS                  TO CNT-L CNT-A                   ECS030
00588                                     T-DTH  T-DIS.                 ECS030
00589                                                                   ECS030
00590  0200-PT-HDNG.                                                    ECS030
00591      ADD +1 TO PG-NO.                                             ECS030
00592      MOVE PG-NO                  TO HD-PG                         ECS030
00593      MOVE HD1                    TO P-DATA.                       ECS030
00594      MOVE '1'                    TO X.                            ECS030
00595      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00596                                                                   ECS030
00597      MOVE HD2                    TO P-DATA.                       ECS030
00598      MOVE ' '                    TO X.                            ECS030
00599      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00600                                                                   ECS030
00601      MOVE HD3                    TO P-DATA.                       ECS030
00602      MOVE ' '                    TO X.                            ECS030
00603      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00604                                                                   ECS030
00605  0210-HDNG-EXIT.                                                  ECS030
00606      EXIT.                                                        ECS030
00607                                                                   ECS030
00608  0220-DETAIL-HDNG.                                                ECS030
00609      MOVE LIFE-OVERRIDE-L6       TO HD4-OV1                       ECS030
00610                                     HD5-OV1.                      ECS030
00611      MOVE AH-OVERRIDE-L6         TO HD4-OV2                       ECS030
00612                                     HD5-OV2.                      ECS030
00613      MOVE HD4                    TO P-DATA.                       ECS030
00614      MOVE '0'                    TO X.                            ECS030
00615      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00616                                                                   ECS030
00617      MOVE HD5                    TO P-DATA.                       ECS030
00618      MOVE ' '                    TO X.                            ECS030
00619      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00620                                                                   ECS030
00621      MOVE SPACES                 TO P-DATA.                       ECS030
00622      MOVE ' '                    TO X.                            ECS030
00623      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00624                                                                   ECS030
00625      MOVE SPACES                 TO P-DATA.                       ECS030
00626      MOVE ZERO                   TO LN-CT.                        ECS030
00627                                                                   ECS030
00628  0230-ALL-HDNG-E.                                                 ECS030
00629      EXIT.                                                        ECS030
00630                                                                   ECS030
00631  0240-P-H-X.                                                      ECS030
00632      GO TO 0130-MAIN-LOOP.                                        ECS030
00633                                                                   ECS030
00634  0250-WRITE-CAR.                                                  ECS030
00635      MOVE DETAIL-EXTRACT         TO SRT-REC.                      ECS030
00636      MOVE DE-GROUPING            TO S-COPNY.                      ECS030
00637      MOVE SPACES                 TO S-CO.                         ECS030
00638      RELEASE SRT-REC.                                             ECS030
00639                                                                   ECS030
00640  0260-W-CAR-END.                                                  ECS030
00641      EXIT.                                                        ECS030
00642                                                                   ECS030
00643  0270-PRNT-CARR.                                                  ECS030
00644      MOVE ZEROS                  TO LN-CT                         ECS030
00645                                     T-DIS      T-DTH              ECS030
00646                                     G-TOTDTH   G-TOTDIS           ECS030
00647                                     G-TOTCNT-L G-TOTCNT-A.        ECS030
00648      MOVE 'TOTAL FOR CARRIER'    TO TLC.                          ECS030
00649      MOVE SPACE                  TO TL-CO                         ECS030
00650                                     SAV-COMP                      ECS030
00651                                     WS-DE-COMP.                   ECS030
00652      MOVE HD1-A                  TO HD1.                          ECS030
00653      MOVE HD6                    TO HD4-1.                        ECS030
00654      MOVE HD5G                   TO HD5.                          ECS030
00655                                                                   ECS030
00656  0280-GET-CAR.                                                    ECS030
00657      READ CLM-CARR INTO DETAIL-EXTRACT                            ECS030
00658          AT END  GO TO 0290-PRNT-CARR-XIT.                        ECS030
00659                                                                   ECS030
00660      IF NOT DE-CLAIM                                              ECS030
00661          GO TO 0280-GET-CAR.                                      ECS030
00662                                                                   ECS030
00663      MOVE DE-CARRIER             TO WS-DE-COMP.                   ECS030
00664      GO TO 0130-MAIN-LOOP.                                        ECS030
00665                                                                   ECS030
00666  0290-PRNT-CARR-XIT.                                              ECS030
00667      EXIT.                                                        ECS030
00668                                                                   ECS030
00669  0300-NO-INPUT.                                                   ECS030
00670      PERFORM 0200-PT-HDNG THRU 0210-HDNG-EXIT.                    ECS030
00671      MOVE ' NO CLAIMS INPUT RECEIVED FOR ECS-030' TO P-DATA.      ECS030
00672      MOVE '0'                    TO X.                            ECS030
00673      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00674                                                                   ECS030
00675  0310-N-I-RTURN.                                                  ECS030
00676      EXIT.                                                        ECS030
00677      EJECT                                                        ECS030
00678  0320-PRT-RTN.                                                    ECS030
00679                              COPY ELCPRT2.                        ECS030
00680  0330-PRT-RTN-XIT.    EXIT.                                       ECS030
00681                                                                   ECS030
00682  0350-G-TOTALS.                                                   ECS030
00683      MOVE SPACES                 TO HD-COMP                       ECS030
00684                                     HD1-CARR                      ECS030
00685                                     HD1-GROUP.                    ECS030
00686                                                                   ECS030
00687      PERFORM 0200-PT-HDNG THRU 0230-ALL-HDNG-E.                   ECS030
00688                                                                   ECS030
00689      MOVE G-TOTCNT-L             TO GT-CNT-L.                     ECS030
00690      MOVE G-TOTCNT-A             TO GT-CNT-A.                     ECS030
00691      MOVE G-TOTDTH               TO GT-DTH.                       ECS030
00692      MOVE G-TOTDIS               TO GT-DIS.                       ECS030
00693                                                                   ECS030
00694      IF ME-DO-UPDATE                                              ECS030
070714         MOVE G-TOTDTH           TO hld-030-CLMS-L                ECS030
070714         MOVE G-TOTDIS           TO hld-030-CLMS-AH.              ECS030
00697                                                                   ECS030
00698      MOVE G-T-LN                 TO P-DATA.                       ECS030
00699      MOVE '0'                    TO X.                            ECS030
00700      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00701                                                                   ECS030
00702  0360-END-G-TOTAL.                                                ECS030
00703                                                                   ECS030
00704  0370-ACCT-TOT.                                                   ECS030
00705      MOVE 'TOTAL FOR ACCOUNT'    TO TLC.                          ECS030
00706      MOVE SPACE                  TO TL-CO.                        ECS030
00707      MOVE SPACE                  TO P-DATA.                       ECS030
00708      MOVE A-T-CNT-L              TO TL-CNT-L.                     ECS030
00709      MOVE A-T-CNT-A              TO TL-CNT-A.                     ECS030
00710      MOVE A-T-DTH                TO TL-DTH.                       ECS030
00711      MOVE A-T-DIS                TO TL-DIS.                       ECS030
00712      MOVE TL-LN                  TO P-DATA.                       ECS030
00713      MOVE ' '                    TO X.                            ECS030
00714      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00715                                                                   ECS030
00716      MOVE SPACE                  TO P-DATA.                       ECS030
00717      MOVE ' '                    TO X.                            ECS030
00718      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  ECS030
00719                                                                   ECS030
00720      ADD +2 TO LN-CT.                                             ECS030
00721      MOVE ZEROS                  TO A-T-CNT-L A-T-CNT-A           ECS030
00722                                     A-T-DTH   A-T-DIS.            ECS030
00723                                                                   ECS030
00724  0380-ACCT-XIT.                                                   ECS030
00725      EXIT.                                                        ECS030
00726                                                                   ECS030
00727  0390-END-STEP.                                                   ECS030
00728      MOVE DTE-SYS-E-CLASIC-CLAIMS TO SAV-PND-SW.                  ECS030
00729                                                                   ECS030
00730      IF CLM-CTR = ZERO                                            ECS030
00731          PERFORM 0300-NO-INPUT                                    ECS030
00732          MOVE ALL '9'            TO DETAIL-EXTRACT                ECS030
00733          PERFORM 0250-WRITE-CAR THRU 0260-W-CAR-END               ECS030
00734          GO TO 0400-STEP-ONE-X.                                   ECS030
00735                                                                   ECS030
00736      PERFORM 0180-PRT-TOTALS.                                     ECS030
00737      PERFORM 0350-G-TOTALS.                                       ECS030
00738      MOVE 'N'                    TO DTE-SYS-E-CLASIC-CLAIMS.      ECS030
00739                                                                   ECS030
00740  0400-STEP-ONE-X.                                                 ECS030
00741      EXIT.                                                        ECS030
00742      EJECT                                                        ECS030
00743  0410-STEP-TWO.                                                   ECS030
00744      IF CLM-CTR = ZERO                                            ECS030
00745          GO TO 0420-CLOSE-ALL.                                    ECS030
00746                                                                   ECS030
00747      OPEN INPUT CLM-CARR.                                         ECS030
00748                                                                   ECS030
00749                                                                   ECS030
00750      MOVE '9'                    TO CAR-SW.                       ECS030
00751      PERFORM 0270-PRNT-CARR THRU 0290-PRNT-CARR-XIT.              ECS030
00752      PERFORM 0180-PRT-TOTALS.                                     ECS030
00753      PERFORM 0350-G-TOTALS.                                       ECS030
00754                                                                   ECS030
00755  0420-CLOSE-ALL.                                                  ECS030
00756                              COPY ELCPRTC.                        ECS030
00757      MOVE SAV-PND-SW             TO DTE-SYS-E-CLASIC-CLAIMS.      ECS030
00758                                                                   ECS030
00759      CLOSE PRNTR  EXTRACT.                                        ECS030
00760                                                                   ECS030
070714     OPEN I-O ERMEBL.                                             ECS030
070714                                                                  ECS030
070714     IF ERMEBL-FILE-STATUS  = '00' OR '97'                        ECS030
070714         NEXT SENTENCE                                            ECS030
070714       ELSE                                                       ECS030
070714         MOVE 'N'                TO ME-UPDATE-FLAG.               ECS030
070714
070714     MOVE DTE-CLIENT             TO ME-COMPANY.                   ECS030
070714     COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             ECS030
070714     MOVE MONTH-END-MOYR         TO ME-MOYR.                      ECS030
070714
070714     IF ME-DO-UPDATE                                              ECS030
070714         READ ERMEBL INVALID KEY                                  ECS030
070714         MOVE 'N'                TO ME-UPDATE-FLAG                ECS030
070714         CLOSE ERMEBL.                                            ECS030
070714
070714     IF ME-DO-UPDATE                                              ECS030
070714         move hld-030-clms-l     to me-030-clms-l
070714         move hld-030-clms-ah    to me-030-clms-ah
070714         MOVE ME-CNDS-DATE       TO ME-030-RUN-DT                 ECS030
070714         ADD 1 TO ME-030-RUN-CT                                   ECS030
070714         REWRITE MONTH-END-BALANCES                               ECS030
070714         CLOSE ERMEBL.                                            ECS030
00769                                                                   ECS030
00770      GOBACK.                                                      ECS030
00771                                                                   ECS030
00772                                                                   ECS030
00773  ABEND-PGM SECTION.                                               ECS030
00774                     COPY ELCABEND.                                ECS030
00775 /                                                                 ECS030
00776  LCP-WRITE-POS-PRT SECTION.                                       ECS030
00777      IF LCP-ASA = '+'                                             ECS030
00778          WRITE PRT AFTER 0 LINE                                   ECS030
00779      ELSE                                                         ECS030
00780      IF LCP-ASA = ' '                                             ECS030
00781          WRITE PRT AFTER ADVANCING 1 LINE                         ECS030
00782      ELSE                                                         ECS030
00783      IF LCP-ASA = '0'                                             ECS030
00784          WRITE PRT AFTER ADVANCING 2 LINE                         ECS030
00785      ELSE                                                         ECS030
00786      IF LCP-ASA = '-'                                             ECS030
00787          WRITE PRT AFTER ADVANCING 3 LINE                         ECS030
00788      ELSE                                                         ECS030
00789      IF LCP-ASA = '1'                                             ECS030
00790          WRITE PRT AFTER ADVANCING PAGE                           ECS030
00791      ELSE                                                         ECS030
00792      IF LCP-ASA = '2'                                             ECS030
00793          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS030
00794      ELSE                                                         ECS030
00795      IF LCP-ASA = '3'                                             ECS030
00796          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS030
00797      ELSE                                                         ECS030
00798      IF LCP-ASA = '4'                                             ECS030
00799          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS030
00800      ELSE                                                         ECS030
00801      IF LCP-ASA = '5'                                             ECS030
00802          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS030
00803      ELSE                                                         ECS030
00804      IF LCP-ASA = '6'                                             ECS030
00805          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS030
00806      ELSE                                                         ECS030
00807      IF LCP-ASA = '7'                                             ECS030
00808          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS030
00809      ELSE                                                         ECS030
00810      IF LCP-ASA = '8'                                             ECS030
00811          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS030
00812      ELSE                                                         ECS030
00813      IF LCP-ASA = '9'                                             ECS030
00814          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS030
00815      ELSE                                                         ECS030
00816      IF LCP-ASA = 'A'                                             ECS030
00817          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS030
00818      ELSE                                                         ECS030
00819      IF LCP-ASA = 'B'                                             ECS030
00820          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS030
00821      ELSE                                                         ECS030
00822      IF LCP-ASA = 'C'                                             ECS030
00823          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS030
00824      ELSE                                                         ECS030
00825      IF LCP-ASA = 'V'                                             ECS030
00826          WRITE PRT AFTER ADVANCING LCP-P01                        ECS030
00827      ELSE                                                         ECS030
00828      IF LCP-ASA = 'W'                                             ECS030
00829          WRITE PRT AFTER ADVANCING LCP-P02                        ECS030
00830      ELSE                                                         ECS030
00831      DISPLAY 'ASA CODE ERROR'.                                    ECS030
00832  LCP-WRITE-END-PRT.                                               ECS030
00833      EXIT.                                                        ECS030
