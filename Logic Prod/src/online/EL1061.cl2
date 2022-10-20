00001  ID DIVISION.                                                     11/05/96
00002                                                                   EL1061
00003  PROGRAM-ID.                 EL1061.                                 LV011
00004 *              PROGRAM CONVERTED BY                                  CL*10
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*10
00006 *              CONVERSION DATE 02/12/96 09:43:28.                    CL*10
00007 *                            VMOD=2.011.                             CL*11
00008 *                                                                 EL1061
00008 *                                                                 EL1061
00009 *AUTHOR.     LOGIC,INC.                                              CL*10
00010 *            DALLAS, TEXAS.                                          CL*10
00011                                                                   EL1061
00012 *DATE-COMPILED.                                                      CL*10
00013 *SECURITY.   *****************************************************   CL*10
00014 *            *                                                   *   CL*10
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*10
00016 *            *                                                   *   CL*10
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*10
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*10
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*10
00020 *            *                                                   *   CL*10
00021 *            *****************************************************   CL*10
00022                                                                   EL1061
00023 *REMARKS.    TRANSACTION- EX10 - STATE BENEFIT MAINTENANCE.       EL1061
00024                                                                   EL1061
111219******************************************************************
111219*                   C H A N G E   L O G
111219*
111219* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111219*-----------------------------------------------------------------
111219*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111219* EFFECTIVE    NUMBER
111219*-----------------------------------------------------------------
111219* 111219  CR2019110800001  PEMA  ALLOW VALID BENEFIT CODES

00026  ENVIRONMENT DIVISION.                                            EL1061
00027  DATA DIVISION.                                                   EL1061
00028  WORKING-STORAGE SECTION.                                         EL1061
00029  77  FILLER  PIC X(32)  VALUE '********************************'. EL1061
00030  77  FILLER  PIC X(32)  VALUE '*    EL1061 WORKING STORAGE    *'. EL1061
00031  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.011 *********'.    CL*11
00032                                                                   EL1061
00033                              COPY ELCSCTM.                           CL**7
00034                              COPY ELCSCRTY.                          CL**7
00035                                                                   EL1061
00036  01  WS-DATE-AREA.                                                EL1061
00037      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL1061
00038      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            EL1061
00039                                                                   EL1061
00040  01  WS.                                                          EL1061
00041      05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1500.          EL1061
00042      05  WS-MAP-LENGTH       PIC S9(4) COMP VALUE +1000.          EL1061
00043      05  RETURNED-FROM       PIC X(8)    VALUE SPACES.            EL1061
00044      05  QID.                                                     EL1061
00045          16  QID-TERM        PIC X(4).                            EL1061
00046          16  FILLER          PIC X(4)    VALUE '106B'.            EL1061
00047                                                                   EL1061
00048      05  WS-ST-LF-EXP-PCT    PIC S9(3)V9(4) VALUE +0.             EL1061
00049      05  WS-ST-AH-EXP-PCT    PIC S9(3)V9(4) VALUE +0.             EL1061
00050                                                                   EL1061
00051  01  STANDARD-AREAS.                                              EL1061
00052      12  MAP-NAME            PIC X(8)    VALUE 'EL106B'.          EL1061
00053      12  MAPSET-NAME         PIC X(8)    VALUE 'EL1061S'.         EL1061
00054      12  TRANS-ID            PIC X(4)    VALUE 'EX1A'.            EL1061
00055      12  PGM-NAME            PIC X(8).                            EL1061
00056      12  TIME-IN             PIC S9(7).                           EL1061
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL1061
00058          16  FILLER          PIC X.                               EL1061
00059          16  TIME-OUT        PIC 99V99.                           EL1061
00060          16  FILLER          PIC XX.                              EL1061
00061      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL1061
00062      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL1061
00063      12  XCTL-EL126          PIC X(8)    VALUE 'EL126'.           EL1061
00064      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           EL1061
00065      12  XCTL-EL106          PIC X(8)    VALUE 'EL106'.           EL1061
00066      12  XCTL-6565           PIC X(8)    VALUE 'EL6565'.          EL1061
00067      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.           EL1061
00068      12  XCTL-GL800          PIC X(8)    VALUE 'GL800'.           EL1061
00069      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL1061
00070      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL1061
00071      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL1061
00072      12  THIS-PGM            PIC X(8)    VALUE 'EL1061'.          EL1061
00073      12  ELCNTL-ID           PIC X(8)    VALUE 'ELCNTL'.             CL**8
00074      12  W-LETR-FILE-ID      PIC X(8)    VALUE 'ELLETR'.          EL1061
00075      12  SUB                 PIC 99.                              EL1061
00076      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL1061
00077                                                                   EL1061
00078  01  SWITCH-WORK.                                                 EL1061
00079      12  WS-FIRST-TIME-SW    PIC X       VALUE 'Y'.               EL1061
00080          88  FIRST-TIME                  VALUE 'Y'.               EL1061
00081      12  WS-DISPLAY-SW       PIC X       VALUE 'N'.               EL1061
00082          88  INITIAL-DISPLAY             VALUE 'Y'.               EL1061
00083                                                                   EL1061
00084  01  TBL-WORK.                                                    EL1061
00085      05  TBL-ITEMS OCCURS 50 TIMES INDEXED BY TBL-NDX.               CL**8
00086          10  TBL-CODE        PIC XX.                                 CL**8
00087          10  TBL-KIND        PIC X.                                  CL**8
00088                                                                   EL1061
00089  01  ACCESS-KEYS.                                                 EL1061
00090      12  ELCNTL-KEY.                                              EL1061
00091          16  CK-COMP-ID      PIC X(3).                            EL1061
00092          16  FILLER          PIC X       VALUE '3'.               EL1061
00093          16  CK-STATE-CD     PIC X(4)    VALUE SPACES.            EL1061
00094          16  CK-SEQ          PIC S9(4)   VALUE +0    COMP.        EL1061
00095                                                                   EL1061
00096  01  ERROR-MESSAGES.                                              EL1061
00097      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL1061
00098      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL1061
00099      12  ER-0013                 PIC X(4)  VALUE '0013'.          EL1061
00100      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL1061
00101      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL1061
00102      12  ER-0042                 PIC X(4)  VALUE '0042'.          EL1061
00103      12  ER-0050                 PIC X(4)  VALUE '0050'.          EL1061
00104      12  ER-0068                 PIC X(4)  VALUE '0068'.          EL1061
00105      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL1061
00106      12  ER-0144                 PIC X(4)  VALUE '0144'.          EL1061
00107      12  ER-0145                 PIC X(4)  VALUE '0145'.          EL1061
00108      12  ER-0146                 PIC X(4)  VALUE '0146'.          EL1061
00109      12  ER-0147                 PIC X(4)  VALUE '0147'.          EL1061
00110      12  ER-0148                 PIC X(4)  VALUE '0148'.          EL1061
00111      12  ER-0149                 PIC X(4)  VALUE '0149'.          EL1061
00112      12  ER-0150                 PIC X(4)  VALUE '0150'.          EL1061
00113      12  ER-0151                 PIC X(4)  VALUE '0151'.          EL1061
00114      12  ER-0152                 PIC X(4)  VALUE '0152'.          EL1061
00115      12  ER-0153                 PIC X(4)  VALUE '0153'.          EL1061
00116      12  ER-0159                 PIC X(4)  VALUE '0159'.          EL1061
00117      12  ER-0160                 PIC X(4)  VALUE '0160'.          EL1061
00118      12  ER-0161                 PIC X(4)  VALUE '0161'.          EL1061
00119      12  ER-0582                 PIC X(4)  VALUE '0582'.             CL**3
00120      12  ER-2009                 PIC X(4)  VALUE '2009'.          EL1061
00121      12  ER-2010                 PIC X(4)  VALUE '2010'.          EL1061
00122      12  ER-2012                 PIC X(4)  VALUE '2012'.          EL1061
00123      12  ER-2014                 PIC X(4)  VALUE '2014'.          EL1061
00124      12  ER-2024                 PIC X(4)  VALUE '2024'.          EL1061
00125      12  ER-2028                 PIC X(4)  VALUE '2028'.          EL1061
00126      12  ER-2033                 PIC X(4)  VALUE '2033'.          EL1061
00127      12  ER-2298                 PIC X(4)  VALUE '2298'.          EL1061
00128      12  ER-2299                 PIC X(4)  VALUE '2299'.          EL1061
00129      12  ER-3030                 PIC X(4)  VALUE '3030'.          EL1061
00130      12  ER-3031                 PIC X(4)  VALUE '3031'.          EL1061
00131      12  ER-3032                 PIC X(4)  VALUE '3032'.          EL1061
00132      12  ER-3033                 PIC X(4)  VALUE '3033'.          EL1061
00133      12  ER-3034                 PIC X(4)  VALUE '3034'.          EL1061
00134      12  ER-3035                 PIC X(4)  VALUE '3035'.          EL1061
00135      12  ER-7008                 PIC X(4)  VALUE '7008'.          EL1061
00136      12  ER-7531                 PIC X(4)  VALUE '7531'.          EL1061
00137      12  ER-7534                 PIC X(4)  VALUE '7534'.          EL1061
00138      12  ER-9074                 PIC X(4)  VALUE '9074'.          EL1061
00139      12  ER-9447                 PIC X(4)  VALUE '9447'.          EL1061
00140      12  ER-9448                 PIC X(4)  VALUE '9448'.          EL1061
00141      EJECT                                                        EL1061
00142                              COPY ELCDATE.                           CL**7
00143      EJECT                                                        EL1061
00144                              COPY ELCLOGOF.                          CL**7
00145      EJECT                                                        EL1061
00146                              COPY ELCATTR.                           CL**7
00147      EJECT                                                        EL1061
00148                              COPY ELCEMIB.                           CL**7
00149      EJECT                                                        EL1061
00150                              COPY ELCJPFX.                           CL**7
00151                                   PIC X(530).                     EL1061
00152      EJECT                                                        EL1061
00153                              COPY ELCINTF.                           CL**7
00154      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL1061
00155          16  FILLER              PIC X(101).                      EL1061
00156          16  PI-WS-STATE         PIC XX.                          EL1061
00157          16  PI-WS-CLASS         PIC XX.                          EL1061
00158          16  PI-WS-DEV           PIC XXX.                         EL1061
00159          16  PI-WS-TYPE          PIC X.                           EL1061
00160          16  PI-WS-PLAN          PIC XX.                          EL1061
00161          16  PI-PREV-STATE       PIC X(4).                        EL1061
00162          16  FILLER              PIC X(525).                         CL*10
00163                                                                   EL1061
00164      EJECT                                                        EL1061
00165                              COPY ELCAID.                            CL**7
00166  01  FILLER    REDEFINES DFHAID.                                  EL1061
00167      12  FILLER              PIC X(8).                            EL1061
00168      12  PF-VALUES           PIC X       OCCURS 2.                EL1061
00169                                                                   EL1061
00170      EJECT                                                        EL1061
00171                               COPY EL1061S.                          CL**7
00172  01  MAP-REDEF REDEFINES EL106BI.                                 EL1061
00173      12  FILLER              PIC X(76).                              CL**8
00174      12  BENEFIT-CONTROLS OCCURS 50 TIMES INDEXED BY SC-INDX.        CL**8
00175          16  SC-CDL          PIC S9(4)  COMP.                     EL1061
00176          16  SC-CDA          PIC X.                               EL1061
00177          16  SC-CD           PIC XX.                              EL1061
00178          16  SC-KINDL        PIC S9(4)  COMP.                     EL1061
00179          16  SC-KINDA        PIC X.                               EL1061
00180          16  SC-KIND         PIC X.                               EL1061
00181          16  SC-REFL         PIC S9(4)  COMP.                     EL1061
00182          16  SC-REFA         PIC X.                               EL1061
00183          16  SC-REF          PIC X.                               EL1061
00184          16  SC-REMTERML     PIC S9(4)  COMP.                     EL1061
00185          16  SC-REMTERMA     PIC X.                               EL1061
00186          16  SC-REMTERM      PIC X.                               EL1061
022415         16  sc-extral       pic s9(4)  comp.
022415         16  sc-extraa       pic x.
022415         16  sc-extra        pic x.

00188  LINKAGE SECTION.                                                 EL1061
00189  01  DFHCOMMAREA             PIC X(1500).                         EL1061
00190 *01 PARMLIST .                                                       CL*10
00191 *    02  FILLER              PIC S9(8)   COMP.                       CL*10
00192 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*10
00193 *    02  ELLETR-POINTER      PIC S9(8)   COMP.                       CL*10
00194      EJECT                                                        EL1061
00195                              COPY ELCCNTL.                           CL**7
00196      EJECT                                                        EL1061
00197                              COPY ELCTEXT.                           CL**7
00198      EJECT                                                        EL1061
00199  PROCEDURE DIVISION.                                              EL1061
00200                                                                   EL1061
00201      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1061
00202      MOVE '5'                   TO DC-OPTION-CODE.                EL1061
00203      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1061
00204      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1061
00205      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1061
00206                                                                   EL1061
00207      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.       EL1061
00208      MOVE 2                     TO EMI-NUMBER-OF-LINES            EL1061
00209                                    EMI-SWITCH2.                   EL1061
00210                                                                   EL1061
00211      MOVE EIBTRMID              TO QID-TERM.                      EL1061
00212                                                                   EL1061
00213      IF EIBCALEN = 0                                              EL1061
00214          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL1061
00215                                                                   EL1061
00216      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           EL1061
00217          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM                 EL1061
00218        ELSE                                                       EL1061
00219          MOVE SPACES             TO RETURNED-FROM.                EL1061
00220                                                                   EL1061
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL1061
00222          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL1061
00223              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL1061
00224              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL1061
00225              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL1061
00226              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL1061
00227              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL1061
00228              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL1061
00229              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL1061
00230              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL1061
00231          ELSE                                                     EL1061
00232              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL1061
00233              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL1061
00234              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL1061
00235              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL1061
00236              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL1061
00237              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL1061
00238              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL1061
00239              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL1061
00240                                                                   EL1061
00241      MOVE 'N' TO WS-DISPLAY-SW.                                   EL1061
00242                                                                   EL1061
00243      EXEC CICS HANDLE CONDITION                                   EL1061
00244          DUPREC  (8850-DUPREC)                                    EL1061
00245          NOTOPEN (8870-NOTOPEN)                                   EL1061
00246          NOTFND  (8880-NOT-FOUND)                                 EL1061
00247          PGMIDERR(9600-PGMID-ERROR)                               EL1061
00248          ERROR   (9990-ABEND)                                     EL1061
00249      END-EXEC.                                                    EL1061
00250                                                                   EL1061
00251      IF EIBTRNID NOT = TRANS-ID                                   EL1061
00252         IF RETURNED-FROM = XCTL-6565                              EL1061
00253             MOVE LOW-VALUES           TO EL106BO                  EL1061
00254             PERFORM 6500-RECOVER-TEMP-STORAGE THRU 6500-EXIT      EL1061
00255             GO TO 8100-SEND-INITIAL-MAP                           EL1061
00256         ELSE                                                      EL1061
00257             MOVE LOW-VALUES           TO EL106BO                  EL1061
00258             IF PI-WS-STATE EQUAL LOW-VALUES OR SPACES             EL1061
00259                 GO TO 8100-SEND-INITIAL-MAP                       EL1061
00260             ELSE                                                  EL1061
00261                 MOVE PI-WS-STATE          TO STCDI                EL1061
00262                 MOVE 'Y'                  TO WS-DISPLAY-SW        EL1061
00263                 GO TO 1000-SHOW-STATE.                            EL1061
00264                                                                   EL1061
00265      IF EIBAID = DFHCLEAR                                         EL1061
00266          GO TO 9400-CLEAR.                                        EL1061
00267                                                                   EL1061
00268      IF NOT SYSTEM-DISPLAY-CAP                                    EL1061
00269          MOVE 'READ'         TO SM-READ                           EL1061
00270          PERFORM 9995-SECURITY-VIOLATION                          EL1061
00271          MOVE ER-0070        TO EMI-ERROR                         EL1061
00272          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1061
00273          GO TO 8100-SEND-INITIAL-MAP.                             EL1061
00274                                                                   EL1061
00275      EJECT                                                        EL1061
00276  0200-RECEIVE.                                                    EL1061
00277      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL1061
00278          MOVE LOW-VALUES TO EL106BI                               EL1061
00279          MOVE ER-7008    TO EMI-ERROR                             EL1061
00280          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1061
00281          MOVE -1         TO MAINTL                                   CL**8
00282          GO TO 8200-SEND-DATAONLY.                                EL1061
00283                                                                   EL1061
00284      EXEC CICS RECEIVE                                            EL1061
00285          MAP   (MAP-NAME)                                         EL1061
00286          MAPSET(MAPSET-NAME)                                      EL1061
00287          INTO  (EL106BI)                                          EL1061
00288      END-EXEC.                                                    EL1061
00289                                                                   EL1061
00290      IF ENTERPFL = 0                                              EL1061
00291          GO TO 0300-CHECK-PFKEYS.                                 EL1061
00292                                                                   EL1061
00293      IF EIBAID NOT = DFHENTER                                     EL1061
00294          MOVE ER-0004 TO EMI-ERROR                                EL1061
00295          GO TO 0320-INPUT-ERROR.                                  EL1061
00296                                                                   EL1061
00297      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)   EL1061
00298          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL1061
00299      ELSE                                                         EL1061
00300          MOVE ER-0029 TO EMI-ERROR                                EL1061
00301          GO TO 0320-INPUT-ERROR.                                  EL1061
00302                                                                   EL1061
00303  0300-CHECK-PFKEYS.                                               EL1061
00304      IF EIBAID = DFHPF23                                          EL1061
00305          GO TO 8810-PF23.                                         EL1061
00306                                                                   EL1061
00307      IF EIBAID = DFHPF24                                          EL1061
00308          GO TO 9200-RETURN-MAIN-MENU.                             EL1061
00309                                                                   EL1061
00310      IF EIBAID = DFHPF12                                          EL1061
00311          GO TO 9500-PF12.                                         EL1061
00312                                                                   EL1061
00313      IF MAINTL NOT = 0  AND                                       EL1061
00314         EIBAID NOT = DFHENTER                                     EL1061
00315           MOVE ER-0050 TO EMI-ERROR                               EL1061
00316           GO TO 0320-INPUT-ERROR.                                 EL1061
00317                                                                   EL1061
00318      IF EIBAID = DFHPF1                                           EL1061
00319          GO TO 5000-FIND-NEXT-STATE.                              EL1061
00320                                                                   EL1061
00321      IF EIBAID = DFHPF2                                           EL1061
00322          GO TO 5500-FIND-PREV-STATE.                              EL1061
00323                                                                   EL1061
00324      IF EIBAID = DFHPF3                                           EL1061
00325          MOVE PI-PREV-STATE      TO  PI-WS-STATE                  EL1061
00326          MOVE SPACES             TO  PI-WS-CLASS                  EL1061
00327                                      PI-WS-DEV                    EL1061
00328                                      PI-WS-TYPE                   EL1061
00329                                      PI-WS-PLAN                   EL1061
00330          PERFORM 6400-CREATE-TEMP-STORAGE THRU 6400-EXIT          EL1061
00331          MOVE XCTL-6565          TO  PGM-NAME                     EL1061
00332          GO TO 9300-XCTL.                                         EL1061
00333                                                                   EL1061
00334      IF EIBAID = DFHPF4                                           EL1061
00335          MOVE PI-PREV-STATE      TO  PI-WS-STATE                  EL1061
00336          MOVE SPACES             TO  PI-WS-CLASS                  EL1061
00337                                      PI-WS-DEV                    EL1061
00338                                      PI-WS-TYPE                   EL1061
00339                                      PI-WS-PLAN                   EL1061
00340          MOVE XCTL-EL106         TO  PGM-NAME                     EL1061
00341          GO TO 9300-XCTL.                                         EL1061
00342                                                                   EL1061
00343      IF EIBAID = DFHENTER                                         EL1061
00344          GO TO 0330-EDIT-DATA.                                    EL1061
00345                                                                   EL1061
00346      MOVE ER-0029 TO EMI-ERROR.                                   EL1061
00347  0320-INPUT-ERROR.                                                EL1061
00348      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00349      MOVE AL-UNBON TO ENTERPFA.                                   EL1061
00350                                                                   EL1061
00351      IF ENTERPFL = 0                                              EL1061
00352          MOVE -1 TO MAINTL                                        EL1061
00353      ELSE                                                         EL1061
00354          MOVE -1 TO ENTERPFL.                                     EL1061
00355                                                                   EL1061
00356      GO TO 8200-SEND-DATAONLY.                                    EL1061
00357                                                                   EL1061
00358      EJECT                                                        EL1061
00359  0330-EDIT-DATA.                                                  EL1061
00360      IF STCDL = ZERO AND                                          EL1061
00361         STABRL = ZERO                                             EL1061
00362          MOVE ER-0144       TO EMI-ERROR                          EL1061
00363          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1061
00364          MOVE -1            TO STCDL                              EL1061
00365          MOVE AL-UABON      TO STCDA                              EL1061
00366          GO TO 8200-SEND-DATAONLY.                                EL1061
00367                                                                   EL1061
00368      IF STCDL = ZERO AND                                          EL1061
00369         STABRL NOT = ZERO                                         EL1061
00370          PERFORM 0500-GET-STATE-CD THRU 0600-EXIT.                EL1061
00371                                                                   EL1061
00372      IF MAINTI = 'S'                                              EL1061
00373          GO TO 1000-SHOW-STATE.                                   EL1061
00374                                                                   EL1061
00375      IF SYSTEM-MODIFY-CAP                                         EL1061
00376          NEXT SENTENCE                                               CL**8
00377        ELSE                                                       EL1061
00378          IF MAINTI =  'C'                                            CL**8
00379              MOVE 'UPDATE'       TO SM-READ                          CL**8
00380              PERFORM 9995-SECURITY-VIOLATION                         CL**8
00381              MOVE ER-0070        TO EMI-ERROR                        CL**8
00382              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**8
00383              MOVE LOW-VALUES     TO EL106BO                          CL**8
00384              GO TO 8100-SEND-INITIAL-MAP.                            CL**8
00385                                                                   EL1061
00386      IF MAINTI = 'C'                                              EL1061
00387          GO TO 2000-CHANGE-STATE.                                 EL1061
00388                                                                   EL1061
00389      MOVE ER-0023 TO EMI-ERROR.                                   EL1061
00390      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00391      MOVE -1       TO MAINTL.                                     EL1061
00392      MOVE AL-UABON TO MAINTA.                                     EL1061
00393      GO TO 8200-SEND-DATAONLY.                                    EL1061
00394                                                                   EL1061
00395      EJECT                                                        EL1061
00396  0500-GET-STATE-CD.                                               EL1061
00397      MOVE PI-COMPANY-ID  TO CK-COMP-ID.                           EL1061
00398      MOVE LOW-VALUES     TO CK-STATE-CD.                          EL1061
00399      MOVE +0             TO CK-SEQ.                               EL1061
00400                                                                   EL1061
00401      EXEC CICS HANDLE CONDITION                                   EL1061
00402          ENDFILE(8880-NOT-FOUND)                                  EL1061
00403          NOTFND (8880-NOT-FOUND)                                  EL1061
00404      END-EXEC.                                                    EL1061
00405                                                                   EL1061
00406      EXEC CICS STARTBR                                            EL1061
00407          DATASET  (ELCNTL-ID)                                        CL**8
00408          RIDFLD   (ELCNTL-KEY)                                    EL1061
00409      END-EXEC.                                                    EL1061
00410                                                                   EL1061
00411  0510-GET-NEXT-CD.                                                EL1061
00412      EXEC CICS READNEXT                                           EL1061
00413          DATASET(ELCNTL-ID)                                          CL**8
00414          SET    (ADDRESS OF CONTROL-FILE)                            CL*10
00415          RIDFLD (ELCNTL-KEY)                                      EL1061
00416      END-EXEC.                                                    EL1061
00417                                                                   EL1061
00418      IF CF-COMPANY-ID NOT = PI-COMPANY-ID                         EL1061
00419          EXEC CICS ENDBR                                          EL1061
00420              DATASET  (ELCNTL-ID)                                    CL**8
00421          END-EXEC                                                 EL1061
00422          GO TO 8880-NOT-FOUND.                                    EL1061
00423                                                                   EL1061
00424      IF CF-RECORD-TYPE NOT = '3'                                  EL1061
00425          GO TO 0510-GET-NEXT-CD.                                  EL1061
00426                                                                   EL1061
00427      IF CF-STATE-ABBREVIATION = STABRI                            EL1061
00428          NEXT SENTENCE                                            EL1061
00429        ELSE                                                       EL1061
00430          GO TO 0510-GET-NEXT-CD.                                  EL1061
00431                                                                   EL1061
00432      MOVE CF-STATE-CODE      TO STCDI.                            EL1061
00433      MOVE AL-UANON           TO STCDA.                            EL1061
00434      MOVE +2                 TO STCDL.                            EL1061
00435                                                                   EL1061
00436      EXEC CICS ENDBR                                              EL1061
00437          DATASET  (ELCNTL-ID)                                        CL**8
00438      END-EXEC.                                                    EL1061
00439                                                                   EL1061
00440  0600-EXIT.                                                       EL1061
00441       EXIT.                                                       EL1061
00442      EJECT                                                        EL1061
00443  1000-SHOW-STATE.                                                 EL1061
00444      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            EL1061
00445      MOVE STCDI         TO CK-STATE-CD                            EL1061
00446                            PI-WS-STATE.                           EL1061
00447                                                                   EL1061
00448      EXEC CICS READ                                               EL1061
00449          DATASET(ELCNTL-ID)                                          CL**8
00450          SET    (ADDRESS OF CONTROL-FILE)                            CL*10
00451          RIDFLD (ELCNTL-KEY)                                      EL1061
00452      END-EXEC.                                                    EL1061
00453                                                                   EL1061
00454      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL1061
00455                                                                   EL1061
00456      EJECT                                                        EL1061
00457  2000-CHANGE-STATE.                                               EL1061
00458      IF STCDI NOT = PI-PREV-STATE                                 EL1061
00459          MOVE ER-0145  TO EMI-ERROR                                  CL**8
00460          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1061
00461          MOVE -1       TO STCDL                                   EL1061
00462          MOVE AL-UABON TO STCDA                                   EL1061
00463          GO TO 8200-SEND-DATAONLY.                                EL1061
00464                                                                   EL1061
00465      PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.                 EL1061
00466                                                                   EL1061
00467      IF NOT EMI-NO-ERRORS                                         EL1061
00468          GO TO 8200-SEND-DATAONLY.                                EL1061
00469                                                                   EL1061
00470      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            EL1061
00471      MOVE STCDI         TO CK-STATE-CD                            EL1061
00472                            PI-WS-STATE.                           EL1061
00473                                                                   EL1061
00474      EXEC CICS READ                                               EL1061
00475          UPDATE                                                   EL1061
00476          DATASET(ELCNTL-ID)                                          CL**8
00477          SET    (ADDRESS OF CONTROL-FILE)                            CL*10
00478          RIDFLD (ELCNTL-KEY)                                      EL1061
00479      END-EXEC.                                                    EL1061
00480                                                                   EL1061
00481      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR             EL1061
00482         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS               EL1061
00483          EXEC CICS UNLOCK                                         EL1061
00484              DATASET(ELCNTL-ID)                                      CL**8
00485          END-EXEC                                                 EL1061
00486          MOVE ER-0068 TO EMI-ERROR                                EL1061
00487          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1061
00488          GO TO 1000-SHOW-STATE.                                   EL1061
00489                                                                   EL1061
00490      MOVE 'B'                    TO JP-RECORD-TYPE.               EL1061
00491      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL1061
00492      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL1061
00493                                                                   EL1061
00494      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY.                    EL1061
00495      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS.                EL1061
00496      MOVE EIBDATE         TO DC-JULIAN-YYDDD.                     EL1061
00497      MOVE '5'             TO DC-OPTION-CODE.                      EL1061
00498      MOVE LINK-ELDATCV    TO PGM-NAME.                            EL1061
00499                                                                   EL1061
00500      EXEC CICS LINK                                               EL1061
00501          PROGRAM (PGM-NAME)                                       EL1061
00502          COMMAREA(DATE-CONVERSION-DATA)                           EL1061
00503          LENGTH  (DC-COMM-LENGTH)                                 EL1061
00504      END-EXEC.                                                    EL1061
00505                                                                   EL1061
00506      IF DATE-CONVERSION-ERROR                                     EL1061
00507          MOVE LOW-VALUES          TO CF-LAST-MAINT-DT             EL1061
00508      ELSE                                                         EL1061
00509          MOVE DC-BIN-DATE-1       TO CF-LAST-MAINT-DT.            EL1061
00510                                                                   EL1061
00511      IF STABRL NOT EQUAL ZEROS                                       CL**2
00512          MOVE STABRI              TO CF-STATE-ABBREVIATION.          CL**2
00513                                                                      CL**2
00514      SET SC-INDX TO 1.                                            EL1061
00515      MOVE 1 TO SUB.                                               EL1061
00516  2001-LOOP.                                                       EL1061
00517      IF SC-CDL (SC-INDX) NOT = ZEROS                              EL1061
00518         IF SC-CD  (SC-INDX) NOT = SPACES  AND  ZEROS              EL1061
00519             MOVE SC-CD (SC-INDX) TO CF-ST-BENEFIT-CD (SUB)        EL1061
00520         ELSE                                                      EL1061
00521             MOVE ZEROS           TO CF-ST-BENEFIT-CD (SUB).       EL1061
00522                                                                   EL1061
00523      IF SC-KINDL (SC-INDX) NOT = ZEROS                            EL1061
00524          MOVE SC-KIND (SC-INDX)  TO CF-ST-BENEFIT-KIND (SUB).     EL1061
00525                                                                   EL1061
00526      IF SC-REFL (SC-INDX) NOT = ZEROS                             EL1061
00527          MOVE SC-REF (SC-INDX)   TO CF-ST-REFUND-CALC (SUB).      EL1061
00528                                                                   EL1061
00529      IF SC-REMTERML (SC-INDX) NOT = ZEROS                         EL1061
00530          MOVE SC-REMTERM (SC-INDX)  TO CF-ST-REM-TERM-CALC (SUB). EL1061
022415     if sc-extral (sc-indx) not = zeros
022415        move sc-extra (sc-indx)  to cf-st-extra-periods (sub)
022415     end-if
00531                                                                   EL1061
00532      IF SC-INDX NOT = 50                                             CL**8
00533          ADD 1 TO SUB                                             EL1061
00534          SET SC-INDX UP BY 1                                      EL1061
00535          GO TO 2001-LOOP.                                         EL1061
00536                                                                   EL1061
00537      MOVE 'C'                    TO JP-RECORD-TYPE.               EL1061
00538      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL1061
00539      EXEC CICS REWRITE                                            EL1061
00540          DATASET(ELCNTL-ID)                                          CL**8
00541          FROM   (CONTROL-FILE)                                    EL1061
00542      END-EXEC.                                                    EL1061
00543                                                                   EL1061
00544      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL1061
00545      MOVE ER-0000     TO EMI-ERROR.                                  CL**8
00546      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00547      MOVE LOW-VALUES  TO EL106BO.                                 EL1061
00548      MOVE -1          TO MAINTL.                                  EL1061
00549      MOVE SPACES      TO PI-PREV-STATE.                           EL1061
00550      MOVE CK-STATE-CD TO STCDO.                                   EL1061
00551      MOVE AL-UANON    TO STCDA.                                   EL1061
00552      GO TO 1000-SHOW-STATE.                                       EL1061
00553                                                                   EL1061
00554      EJECT                                                        EL1061
00555  5000-FIND-NEXT-STATE.                                            EL1061
00556      MOVE PI-COMPANY-ID  TO CK-COMP-ID.                           EL1061
00557                                                                   EL1061
00558      IF STCDL = 0                                                 EL1061
00559          MOVE LOW-VALUES TO CK-STATE-CD                           EL1061
00560          MOVE +0         TO CK-SEQ                                EL1061
00561      ELSE                                                         EL1061
00562          MOVE STCDI      TO CK-STATE-CD                           EL1061
00563          MOVE +1         TO CK-SEQ.                               EL1061
00564                                                                   EL1061
00565      MOVE SPACES TO PI-PREV-STATE.                                EL1061
00566                                                                   EL1061
00567      EXEC CICS HANDLE CONDITION                                   EL1061
00568          NOTFND (8860-ENDFILE)                                    EL1061
00569      END-EXEC.                                                    EL1061
00570                                                                   EL1061
00571      EXEC CICS READ                                               EL1061
00572          DATASET(ELCNTL-ID)                                          CL**8
00573          SET    (ADDRESS OF CONTROL-FILE)                            CL*10
00574          RIDFLD (ELCNTL-KEY)                                      EL1061
00575          GTEQ                                                     EL1061
00576      END-EXEC.                                                    EL1061
00577                                                                   EL1061
00578      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR                    EL1061
00579         CF-RECORD-TYPE NOT = '3'                                  EL1061
00580          GO TO 8860-ENDFILE.                                      EL1061
00581                                                                   EL1061
00582      IF STCDL = 0                                                 EL1061
00583          MOVE ER-0146 TO EMI-ERROR                                EL1061
00584          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL1061
00585                                                                   EL1061
00586      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL1061
00587                                                                   EL1061
00588      EJECT                                                        EL1061
00589  5500-FIND-PREV-STATE.                                            EL1061
00590      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  EL1061
00591      MOVE PI-PREV-STATE          TO  CK-STATE-CD.                 EL1061
00592                                                                   EL1061
00593      IF STCDL GREATER +0                                          EL1061
00594          MOVE STCDI              TO  CK-STATE-CD.                 EL1061
00595                                                                   EL1061
00596      MOVE SPACES                 TO  PI-PREV-STATE.               EL1061
00597                                                                   EL1061
00598      EXEC CICS HANDLE CONDITION                                   EL1061
00599          NOTFND(8860-ENDFILE)                                     EL1061
00600      END-EXEC.                                                    EL1061
00601                                                                   EL1061
00602      EXEC CICS STARTBR                                            EL1061
00603          DATASET  (ELCNTL-ID)                                        CL**8
00604          RIDFLD   (ELCNTL-KEY)                                    EL1061
00605      END-EXEC.                                                    EL1061
00606                                                                   EL1061
00607  5600-READ-PREV-STATE-RECORD.                                     EL1061
00608      EXEC CICS READPREV                                           EL1061
00609          DATASET  (ELCNTL-ID)                                        CL**8
00610          SET      (ADDRESS OF CONTROL-FILE)                          CL*10
00611          RIDFLD   (ELCNTL-KEY)                                    EL1061
00612      END-EXEC.                                                    EL1061
00613                                                                   EL1061
00614      IF FIRST-TIME                                                EL1061
00615          MOVE 'N'                TO  WS-FIRST-TIME-SW             EL1061
00616          GO TO 5600-READ-PREV-STATE-RECORD.                       EL1061
00617                                                                   EL1061
00618      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR                    EL1061
00619         CF-RECORD-TYPE NOT = '3'                                  EL1061
00620          GO TO 8860-ENDFILE.                                      EL1061
00621                                                                   EL1061
00622      IF STCDL = 0                                                 EL1061
00623          MOVE ER-0146 TO EMI-ERROR                                EL1061
00624          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL1061
00625                                                                   EL1061
00626      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL1061
00627                                                                   EL1061
00628      EJECT                                                        EL1061
00629  6000-EDIT-INPUT-DATA.                                            EL1061
00630                                                                   EL1061
00631      PERFORM 6100-INITIALIZE-TABLE THRU 6199-EXIT                 EL1061
00632              VARYING TBL-NDX FROM +1 BY +1                        EL1061
00633                UNTIL TBL-NDX GREATER THAN +50.                       CL**8
00634                                                                   EL1061
00635      SET SC-INDX TO 1.                                            EL1061
00636                                                                   EL1061
00637  6010-LOOP.                                                       EL1061
00638      IF SC-CDL (SC-INDX) = ZEROS  OR                              EL1061
00639         (SC-CD (SC-INDX) = SPACES  OR  ZEROS)                     EL1061
00640         GO TO 6020-NO-CD.                                         EL1061
00641                                                                   EL1061
111219     if sc-cd (sc-indx) = '90' OR '91' OR '92' OR '93' OR
111219          '94' OR '95' OR '96' OR '97' OR '98' OR '99'
00642 *    IF SC-CD (SC-INDX) NOT LESS THAN '90'                        EL1061
00643         MOVE ER-0150  TO EMI-ERROR                                EL1061
00644         MOVE -1       TO SC-CDL (SC-INDX)                         EL1061
00645         MOVE AL-UABON TO SC-CDA (SC-INDX)                         EL1061
00646         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1061
00647        ELSE                                                       EL1061
00648         MOVE AL-UANON TO SC-CDA (SC-INDX).                        EL1061
00649                                                                   EL1061
00650      IF SC-KINDL (SC-INDX) = ZEROS OR                                CL**8
00651         SC-KIND  (SC-INDX) = SPACES                                  CL**8
00652           MOVE -1           TO SC-KINDL (SC-INDX)                    CL**8
00653           MOVE ER-0159      TO EMI-ERROR                             CL**8
00654           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                   CL**8
00655        ELSE                                                       EL1061
00656           IF SC-KIND (SC-INDX) = PI-LIFE-OVERRIDE-L1 OR              CL**8
00657                                  PI-AH-OVERRIDE-L1                   CL**8
00658              MOVE AL-UANON TO SC-KINDA (SC-INDX)                     CL**8
00659           ELSE                                                    EL1061
00660              MOVE ER-0151  TO EMI-ERROR                              CL**8
00661              MOVE -1       TO SC-KINDL (SC-INDX)                     CL**8
00662              MOVE AL-UABON TO SC-KINDA (SC-INDX)                     CL**8
00663              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**8
00664                                                                   EL1061
00665      IF CREDIT-SESSION OR CLAIM-SESSION                              CL**5
00666        IF (SC-REFL (SC-INDX) = ZEROS)    OR                          CL**6
00667           (SC-REF  (SC-INDX) = SPACES)                               CL**8
00668           NEXT SENTENCE                                           EL1061
00669          ELSE                                                     EL1061
00670           IF SC-REF (SC-INDX) = ' ' OR '1' OR '2' OR '3' OR          CL**4
00671                                 '4' OR '5' OR '6' OR '8' OR          CL*11
00672                                 '9'                                  CL*11
00673              MOVE AL-UANON        TO SC-REFA (SC-INDX)               CL**8
00674             ELSE                                                  EL1061
00675              MOVE ER-0582         TO EMI-ERROR                       CL**3
00676              MOVE -1              TO SC-REFL (SC-INDX)            EL1061
00677              MOVE AL-UABON        TO SC-REFA (SC-INDX)            EL1061
00678              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL1061
00679                                                                   EL1061
00680      IF SC-REMTERML (SC-INDX) NOT = ZEROS                         EL1061
00681          IF (SC-REMTERM (SC-INDX) = SPACE)                           CL*10
00682                             OR                                       CL*10
00683             (SC-REMTERM (SC-INDX) GREATER '0' AND LESS '8')          CL*10
00684              MOVE AL-UANON        TO SC-REMTERMA (SC-INDX)           CL**8
00685          ELSE                                                     EL1061
00686              MOVE ER-2298         TO EMI-ERROR                    EL1061
00687              MOVE -1              TO SC-REMTERML (SC-INDX)        EL1061
00688              MOVE AL-UABON        TO SC-REMTERMA (SC-INDX)        EL1061
00689              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL1061
00690                                                                   EL1061
00691      GO TO 6040-CHECK-FOR-DUPS.                                   EL1061
00692                                                                   EL1061
00693  6020-NO-CD.                                                      EL1061
00694      IF SC-KINDL (SC-INDX)   NOT = ZEROS                          EL1061
00695         IF SC-KIND (SC-INDX) NOT = SPACES                         EL1061
00696            MOVE -1       TO SC-KINDL (SC-INDX)                    EL1061
00697            MOVE AL-UABON TO SC-KINDA (SC-INDX)                    EL1061
00698            MOVE ER-0160  TO EMI-ERROR                                CL**8
00699            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL1061
00700                                                                   EL1061
00701      IF CREDIT-SESSION OR CLAIM-SESSION                              CL**5
00702         IF SC-REFL (SC-INDX)   NOT = ZEROS                        EL1061
00703            IF SC-REF (SC-INDX) NOT = SPACE                        EL1061
00704               MOVE -1       TO SC-REFL (SC-INDX)                  EL1061
00705               MOVE AL-UABON TO SC-REFA (SC-INDX)                  EL1061
00706               MOVE ER-2033  TO EMI-ERROR                             CL**8
00707               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           EL1061
00708                                                                   EL1061
00709      IF SC-REMTERML (SC-INDX)    NOT = ZEROS                      EL1061
00710          IF SC-REMTERM (SC-INDX) NOT = SPACE                      EL1061
00711              MOVE -1       TO SC-REMTERML (SC-INDX)               EL1061
00712              MOVE AL-UABON TO SC-REMTERMA (SC-INDX)               EL1061
00713              MOVE ER-2299  TO EMI-ERROR                              CL**8
00714              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL1061
00715                                                                   EL1061
00716  6040-CHECK-FOR-DUPS.                                             EL1061
00717                                                                   EL1061
00718      SET TBL-NDX TO 1.                                            EL1061
00719                                                                   EL1061
00720  6050-DUPLICATE-LOOP.                                             EL1061
00721                                                                   EL1061
00722      INSPECT SC-CD (SC-INDX) REPLACING ALL LOW-VALUES             EL1061
00723                                         BY SPACES.                EL1061
00724                                                                   EL1061
00725      IF SC-CD (SC-INDX) EQUAL SPACES                              EL1061
00726         GO TO 6060-CONTINUE.                                      EL1061
00727                                                                   EL1061
00728      IF TBL-CODE (TBL-NDX) = SC-CD   (SC-INDX) AND                   CL**8
00729         TBL-KIND (TBL-NDX) = SC-KIND (SC-INDX)                       CL**8
00730            MOVE -1       TO SC-CDL   (SC-INDX)                    EL1061
00731                             SC-KINDL (SC-INDX)                       CL**8
00732            MOVE AL-UABON TO SC-CDA   (SC-INDX)                    EL1061
00733                             SC-KINDA (SC-INDX)                       CL**8
00734            MOVE ER-7534  TO EMI-ERROR                                CL**8
00735            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1061
00736            GO TO 6099-EXIT.                                       EL1061
00737                                                                   EL1061
00738      IF TBL-CODE (TBL-NDX) = SPACES                               EL1061
00739          MOVE SC-CD   (SC-INDX) TO TBL-CODE (TBL-NDX)                CL**8
00740          MOVE SC-KIND (SC-INDX) TO TBL-KIND (TBL-NDX)             EL1061
00741      ELSE                                                         EL1061
00742          SET TBL-NDX UP BY 1                                      EL1061
00743          GO TO 6050-DUPLICATE-LOOP.                               EL1061
00744                                                                   EL1061
00745  6060-CONTINUE.                                                   EL1061
00746                                                                   EL1061
00747      IF SC-INDX NOT = 50                                             CL**8
00748         SET SC-INDX UP BY 1                                       EL1061
00749         GO TO 6010-LOOP.                                          EL1061
00750                                                                   EL1061
00751  6099-EXIT.                                                       EL1061
00752      EXIT.                                                        EL1061
00753                                                                   EL1061
00754  6100-INITIALIZE-TABLE.                                           EL1061
00755                                                                   EL1061
00756      MOVE SPACES  TO  TBL-CODE (TBL-NDX)                          EL1061
00757                       TBL-KIND (TBL-NDX).                         EL1061
00758  6199-EXIT.                                                       EL1061
00759      EXIT.                                                        EL1061
00760                                                                   EL1061
00761      EJECT                                                        EL1061
00762  6400-CREATE-TEMP-STORAGE.                                        EL1061
00763      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.             EL1061
00764                                                                   EL1061
00765      EXEC CICS WRITEQ TS                                          EL1061
00766          QUEUE   (QID)                                            EL1061
00767          FROM    (EL106BO)                                        EL1061
00768          LENGTH  (WS-MAP-LENGTH)                                  EL1061
00769      END-EXEC.                                                    EL1061
00770                                                                   EL1061
00771  6400-EXIT.                                                       EL1061
00772       EXIT.                                                       EL1061
00773                                                                   EL1061
00774  6500-RECOVER-TEMP-STORAGE.                                       EL1061
00775      EXEC CICS READQ TS                                           EL1061
00776          QUEUE   (QID)                                            EL1061
00777          INTO    (EL106BO)                                        EL1061
00778          LENGTH  (WS-MAP-LENGTH)                                  EL1061
00779      END-EXEC.                                                    EL1061
00780                                                                   EL1061
00781      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.             EL1061
00782                                                                   EL1061
00783  6500-EXIT.                                                       EL1061
00784       EXIT.                                                       EL1061
00785                                                                   EL1061
00786  6600-DELETE-TEMP-STORAGE.                                        EL1061
00787      EXEC CICS HANDLE CONDITION                                   EL1061
00788          QIDERR  (6600-EXIT)                                      EL1061
00789      END-EXEC.                                                    EL1061
00790                                                                   EL1061
00791      EXEC CICS DELETEQ TS                                         EL1061
00792          QUEUE  (QID)                                             EL1061
00793      END-EXEC.                                                    EL1061
00794                                                                   EL1061
00795  6600-EXIT.                                                       EL1061
00796       EXIT.                                                       EL1061
00797                                                                   EL1061
00798      EJECT                                                        EL1061
00799 ***************************************************************   EL1061
00800 *                                                             *   EL1061
00801 *     BUILD THE OUTPUT SCREEN TO BE DISPLAYED                 *   EL1061
00802 *                                                             *   EL1061
00803 ***************************************************************   EL1061
00804  7000-BUILD-OUTPUT-MAP.                                           EL1061
00805      MOVE LOW-VALUES            TO EL106BO.                       EL1061
00806      MOVE CF-STATE-CODE         TO STCDO                          EL1061
00807                                    PI-WS-STATE.                   EL1061
00808      MOVE CF-STATE-ABBREVIATION TO STABRO.                        EL1061
00809      MOVE CF-STATE-NAME         TO STNAMEO.                       EL1061
00810                                                                   EL1061
00811      MOVE CF-LAST-MAINT-BY      TO LSTUSRO.                       EL1061
00812      MOVE ' '                   TO DC-OPTION-CODE.                EL1061
00813      MOVE CF-LAST-MAINT-DT      TO DC-BIN-DATE-1.                 EL1061
00814      MOVE LINK-ELDATCV          TO PGM-NAME.                      EL1061
00815      EXEC CICS LINK                                               EL1061
00816          PROGRAM (PGM-NAME)                                       EL1061
00817          COMMAREA(DATE-CONVERSION-DATA)                           EL1061
00818          LENGTH  (DC-COMM-LENGTH)                                 EL1061
00819      END-EXEC.                                                    EL1061
00820                                                                   EL1061
00821      IF DATE-CONVERSION-ERROR                                     EL1061
00822          MOVE ZEROS            TO LSTDTEO                         EL1061
00823      ELSE                                                         EL1061
00824          MOVE DC-GREG-DATE-1-EDIT                                 EL1061
00825                                TO LSTDTEO.                        EL1061
00826                                                                   EL1061
00827      MOVE CF-LAST-MAINT-HHMMSS TO TIME-IN.                        EL1061
00828      MOVE TIME-OUT             TO LSTTIMEO.                       EL1061
00829      MOVE -1                   TO MAINTL.                         EL1061
00830      MOVE CF-LAST-MAINT-BY     TO PI-UPDATE-BY.                   EL1061
00831      MOVE CF-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.               EL1061
00832      MOVE CF-STATE-CODE        TO PI-PREV-STATE.                  EL1061
00833      MOVE AL-UANOF             TO MAINTA.                         EL1061
00834      MOVE AL-UABON             TO STCDA                              CL**8
00835                                   STABRA.                         EL1061
00836      MOVE AL-PABON             TO STNAMEA.                           CL**8
00837      SET SC-INDX TO 1.                                            EL1061
00838      MOVE 1 TO SUB.                                               EL1061
00839                                                                   EL1061
00840  7100-LOOP.                                                       EL1061
00841      IF CF-ST-BENEFIT-CD (SUB) = ZEROS                            EL1061
00842         MOVE SPACES                   TO  SC-CD    (SC-INDX)      EL1061
00843        ELSE                                                       EL1061
00844         MOVE CF-ST-BENEFIT-CD  (SUB)  TO  SC-CD    (SC-INDX).     EL1061
00845                                                                   EL1061
00846      MOVE AL-UANON                    TO  SC-CDA   (SC-INDX).     EL1061
00847                                                                   EL1061
00848      MOVE CF-ST-BENEFIT-KIND   (SUB)  TO  SC-KIND  (SC-INDX).     EL1061
00849      MOVE AL-UANON                    TO  SC-KINDA (SC-INDX).     EL1061
00850                                                                   EL1061
00851      IF CREDIT-SESSION OR CLAIM-SESSION                              CL**5
00852         MOVE CF-ST-REFUND-CALC (SUB)  TO  SC-REF   (SC-INDX)      EL1061
00853         MOVE AL-UANON                 TO  SC-REFA  (SC-INDX)      EL1061
00854        ELSE                                                       EL1061
00855         MOVE AL-PANOF                 TO  SC-REFA   (SC-INDX).    EL1061
00856                                                                   EL1061
00857      MOVE CF-ST-REM-TERM-CALC  (SUB)  TO  SC-REMTERM  (SC-INDX).  EL1061
00858      MOVE AL-UANON                    TO  SC-REMTERMA (SC-INDX).  EL1061
           if (cf-st-extra-periods (sub) numeric)
              and (cf-st-extra-periods (sub) not = zeros)
              move cf-st-extra-periods (sub)
                                       to sc-extra (sc-indx)
              move al-uanon            to sc-extraa (sc-indx)
           end-if

00860      IF SUB NOT = 50                                                 CL**8
00861         ADD 1 TO SUB                                              EL1061
00862         SET SC-INDX UP BY 1                                       EL1061
00863         GO TO 7100-LOOP.                                          EL1061
00864                                                                   EL1061
00865      IF INITIAL-DISPLAY                                           EL1061
00866          GO TO 8100-SEND-INITIAL-MAP                              EL1061
00867      ELSE                                                         EL1061
00868          GO TO 8200-SEND-DATAONLY.                                EL1061
00869                                                                   EL1061
00870      EJECT                                                        EL1061
00871  8100-SEND-INITIAL-MAP.                                           EL1061
00872      MOVE SAVE-DATE            TO RUNDTEO.                        EL1061
00873      MOVE EIBTIME              TO TIME-IN.                        EL1061
00874      MOVE TIME-OUT             TO RUNTIMEO.                       EL1061
00875      MOVE -1                   TO MAINTL.                         EL1061
00876      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL1061
00877      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.                       EL1061
00878                                                                   EL1061
00879      EXEC CICS SEND                                               EL1061
00880          MAP   (MAP-NAME)                                         EL1061
00881          MAPSET(MAPSET-NAME)                                      EL1061
00882          FROM  (EL106BO)                                          EL1061
00883          ERASE                                                    EL1061
00884          CURSOR                                                   EL1061
00885      END-EXEC.                                                    EL1061
00886                                                                   EL1061
00887      GO TO 9100-RETURN-TRAN.                                      EL1061
00888      EJECT                                                        EL1061
00889  8200-SEND-DATAONLY.                                              EL1061
00890      MOVE SAVE-DATE      TO RUNDTEO.                              EL1061
00891      MOVE EIBTIME        TO TIME-IN.                              EL1061
00892      MOVE TIME-OUT       TO RUNTIMEO.                             EL1061
00893                                                                   EL1061
00894      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL1061
00895      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.                       EL1061
00896                                                                   EL1061
00897      EXEC CICS SEND                                               EL1061
00898          MAP   (MAP-NAME)                                         EL1061
00899          MAPSET(MAPSET-NAME)                                      EL1061
00900          FROM  (EL106BO)                                          EL1061
00901          DATAONLY                                                 EL1061
00902          ERASEAUP                                                 EL1061
00903          CURSOR                                                   EL1061
00904      END-EXEC.                                                    EL1061
00905                                                                   EL1061
00906      GO TO 9100-RETURN-TRAN.                                      EL1061
00907      EJECT                                                        EL1061
00908  8300-SEND-TEXT.                                                  EL1061
00909      EXEC CICS SEND TEXT                                          EL1061
00910          FROM  (LOGOFF-TEXT)                                      EL1061
00911          LENGTH(LOGOFF-LENGTH)                                    EL1061
00912          ERASE                                                    EL1061
00913          FREEKB                                                   EL1061
00914      END-EXEC.                                                    EL1061
00915                                                                   EL1061
00916      EXEC CICS RETURN                                             EL1061
00917      END-EXEC.                                                    EL1061
00918                                                                   EL1061
00919      EJECT                                                        EL1061
00920  8400-LOG-JOURNAL-RECORD.                                         EL1061
00921      IF PI-JOURNAL-FILE-ID = 0                                    EL1061
00922          GO TO 8400-EXIT.                                         EL1061
00923                                                                   EL1061
00924      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL1061
00925      MOVE ELCNTL-ID              TO JP-FILE-ID.                      CL**8
00926      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL1061
pemuni*    EXEC CICS JOURNAL                                            EL1061
pemuni*         JFILEID(PI-JOURNAL-FILE-ID)                             EL1061
pemuni*         JTYPEID('EL')                                           EL1061
pemuni*         FROM   (JOURNAL-RECORD)                                 EL1061
pemuni*         LENGTH (527)                                            EL1061
pemuni*    END-EXEC.                                                    EL1061
00933                                                                   EL1061
00934  8400-EXIT.                                                       EL1061
00935      EXIT.                                                        EL1061
00936                                                                   EL1061
00937  8800-UNAUTHORIZED-ACCESS.                                        EL1061
00938      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL1061
00939      GO TO 8300-SEND-TEXT.                                        EL1061
00940                                                                   EL1061
00941  8810-PF23.                                                       EL1061
00942      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL1061
00943      MOVE XCTL-005 TO PGM-NAME.                                   EL1061
00944      GO TO 9300-XCTL.                                             EL1061
00945                                                                   EL1061
00946  8850-DUPREC.                                                     EL1061
00947      MOVE ER-0147  TO EMI-ERROR.                                     CL**8
00948      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00949      MOVE -1       TO STCDL.                                      EL1061
00950      MOVE AL-UABON TO STCDA.                                      EL1061
00951      GO TO 8200-SEND-DATAONLY.                                    EL1061
00952                                                                   EL1061
00953  8860-ENDFILE.                                                    EL1061
00954      MOVE LOW-VALUES TO EL106BO.                                  EL1061
00955      MOVE ER-0148    TO EMI-ERROR.                                EL1061
00956      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00957      MOVE -1         TO MAINTL.                                      CL**8
00958      GO TO 8100-SEND-INITIAL-MAP.                                 EL1061
00959                                                                   EL1061
00960  8870-NOTOPEN.                                                    EL1061
00961      MOVE ER-0042 TO EMI-ERROR.                                   EL1061
00962      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00963      MOVE -1      TO MAINTL.                                         CL**8
00964      GO TO 8200-SEND-DATAONLY.                                    EL1061
00965                                                                   EL1061
00966  8880-NOT-FOUND.                                                  EL1061
00967      MOVE ER-0149  TO EMI-ERROR.                                     CL**8
00968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1061
00969      MOVE -1       TO STCDL.                                      EL1061
00970      MOVE AL-UABON TO STCDA.                                      EL1061
00971      GO TO 8200-SEND-DATAONLY.                                    EL1061
00972                                                                   EL1061
00973  9000-RETURN-CICS.                                                EL1061
00974      EXEC CICS RETURN                                             EL1061
00975      END-EXEC.                                                    EL1061
00976                                                                   EL1061
00977  9100-RETURN-TRAN.                                                EL1061
00978      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL1061
00979      MOVE '106B'               TO PI-CURRENT-SCREEN-NO.           EL1061
00980                                                                   EL1061
00981      EXEC CICS RETURN                                             EL1061
00982          TRANSID (TRANS-ID)                                       EL1061
00983          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL1061
00984          LENGTH  (WS-COMM-LENGTH)                                 EL1061
00985      END-EXEC.                                                    EL1061
00986                                                                   EL1061
00987  9200-RETURN-MAIN-MENU.                                           EL1061
00988      IF  CREDIT-SESSION                                           EL1061
00989          MOVE XCTL-EL626           TO PGM-NAME                    EL1061
00990      ELSE                                                         EL1061
00991      IF  CLAIM-SESSION                                            EL1061
00992          MOVE XCTL-EL126           TO PGM-NAME                    EL1061
00993      ELSE                                                         EL1061
00994      IF  MORTGAGE-SESSION                                         EL1061
00995          MOVE XCTL-EM626           TO PGM-NAME                    EL1061
00996      ELSE                                                         EL1061
00997      IF  GENERAL-LEDGER-SESSION                                   EL1061
00998          MOVE XCTL-GL800           TO PGM-NAME.                   EL1061
00999                                                                   EL1061
01000  9300-XCTL.                                                       EL1061
01001      EXEC CICS XCTL                                               EL1061
01002          PROGRAM (PGM-NAME)                                       EL1061
01003          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL1061
01004          LENGTH  (WS-COMM-LENGTH)                                 EL1061
01005      END-EXEC.                                                    EL1061
01006                                                                   EL1061
01007  9400-CLEAR.                                                      EL1061
01008      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL1061
01009      GO TO 9300-XCTL.                                             EL1061
01010                                                                   EL1061
01011  9500-PF12.                                                       EL1061
01012      MOVE XCTL-010 TO PGM-NAME.                                   EL1061
01013      GO TO 9300-XCTL.                                             EL1061
01014                                                                   EL1061
01015  9600-PGMID-ERROR.                                                EL1061
01016      EXEC CICS HANDLE CONDITION                                   EL1061
01017          PGMIDERR(8300-SEND-TEXT)                                 EL1061
01018      END-EXEC.                                                    EL1061
01019                                                                   EL1061
01020      MOVE PGM-NAME      TO PI-CALLING-PROGRAM.                    EL1061
01021      MOVE ' '           TO PI-ENTRY-CD-1.                         EL1061
01022      MOVE XCTL-005      TO PGM-NAME.                              EL1061
01023      MOVE PGM-NAME      TO LOGOFF-PGM.                            EL1061
01024      MOVE PGMIDERR-MSG  TO LOGOFF-FILL.                           EL1061
01025      GO TO 9300-XCTL.                                             EL1061
01026                                                                   EL1061
01027  9700-LINK-DATE-CONVERT.                                          EL1061
01028      EXEC CICS LINK                                               EL1061
01029          PROGRAM    ('ELDATCV')                                   EL1061
01030          COMMAREA   (DATE-CONVERSION-DATA)                        EL1061
01031          LENGTH     (DC-COMM-LENGTH)                              EL1061
01032      END-EXEC.                                                    EL1061
01033                                                                   EL1061
01034  9700-EXIT.                                                       EL1061
01035      EXIT.                                                        EL1061
01036                                                                   EL1061
01037  9900-ERROR-FORMAT.                                               EL1061
01038      IF NOT EMI-ERRORS-COMPLETE                                   EL1061
01039          MOVE LINK-001 TO PGM-NAME                                EL1061
01040          EXEC CICS LINK                                           EL1061
01041              PROGRAM (PGM-NAME)                                   EL1061
01042              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL1061
01043              LENGTH  (EMI-COMM-LENGTH)                            EL1061
01044          END-EXEC.                                                EL1061
01045                                                                   EL1061
01046  9900-EXIT.                                                       EL1061
01047      EXIT.                                                        EL1061
01048                                                                   EL1061
01049  9990-ABEND.                                                      EL1061
01050      MOVE LINK-004               TO PGM-NAME.                     EL1061
01051      MOVE DFHEIBLK               TO EMI-LINE1.                    EL1061
01052      EXEC CICS LINK                                               EL1061
01053          PROGRAM   (PGM-NAME)                                     EL1061
01054          COMMAREA  (EMI-LINE1)                                    EL1061
01055          LENGTH    (72)                                           EL1061
01056      END-EXEC.                                                    EL1061
01057                                                                   EL1061
01058      GO TO 8200-SEND-DATAONLY.                                    EL1061
01059                                                                   EL1061
01060  9995-SECURITY-VIOLATION.                                         EL1061
01061             COPY ELCSCTP.                                         EL1061
01062                                                                   EL1061
01063  9995-EXIT.                                                       EL1061
01064       EXIT.                                                       EL1061
01065                                                                   EL1061
