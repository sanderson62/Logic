00001  ID DIVISION.                                                     10/18/96
00002                                                                   EL6521
00003  PROGRAM-ID.                 EL6521.                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 11/18/94 10:26:25.                    CL**3
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL6521
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL6521
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
00023 *                                                                 EL6521
00024 *REMARKS.    TRANSACTION - EXD8 - COMPENSATION GA STATUS DATA.    EL6521
00025 *                                                                 EL6521
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
092707* 092707    2004020600003  PEMA  ADD DELIVER TO MEL SWITCH
030211* 030211    2010012100001  PEMA  ADD RDS EMAILS TO LOGIC
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************

00026  ENVIRONMENT DIVISION.                                            EL6521
00027                                                                   EL6521
00028      EJECT                                                        EL6521
00029  DATA DIVISION.                                                   EL6521
00030  WORKING-STORAGE SECTION.                                         EL6521
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6521
00032  77  FILLER  PIC X(32)  VALUE '*    EL6521 WORKING STORAGE    *'. EL6521
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.    CL**4
00034                                                                   EL6521
00035  01  WS-DATE-AREA.                                                EL6521
00036      05  WS-SAVE-DATE                PIC X(8) VALUE SPACES.          CL**4
00037      05  WS-SAVE-BIN-DT              PIC XX   VALUE SPACES.          CL**4
00038      05  WS-EFF-YMD                  PIC X(6).                    EL6521
00039      05  WS-SAVE-BIN-EFFDT           PIC XX   VALUE SPACES.          CL**4
00040      05  WS-TRM-YMD                  PIC X(6).                    EL6521
00041      05  WS-SAVE-BIN-TRMDT           PIC XX   VALUE SPACES.          CL**4
00042      05  WS-ACH-SW                   PIC X    VALUE 'N'.             CL**4
00043          88  ACH-HAS-CHANGED             VALUE 'Y'.                  CL**4
00044          88  NO-ACH-CHANGE               VALUE 'N'.                  CL**4
00045      05  SUPPRESS-MAP-SW             PIC X    VALUE SPACE.           CL**4
00046          88  DO-NOT-MOVE-TO-MAP          VALUE 'N'.                  CL**4
00047          88  MOVE-TO-MAP                 VALUE 'Y'.                  CL**4
00048      05  MAP-CHANGED-SW              PIC X    VALUE 'N'.             CL**4
00049          88  MAP-NOT-CHANGED             VALUE 'N'.                  CL**4
00050          88  MAP-CHANGED                 VALUE 'Y'.                  CL**4
           05  BILL-INST-SW                PIC X.
               88  BILL-INST-CHANGED           VALUE 'Y'.
00051                                                                   EL6521

       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

00052  01  STANDARD-AREAS.                                              EL6521
00041      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
00053      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1024.     CL**4
00054      12  WS-SUB                      PIC S9(4) COMP VALUE +0.        CL**4
00055      12  MAP-NAME                    PIC X(8) VALUE 'EL6521A'.       CL**4
00056      12  MAPSET-NAME                 PIC X(8) VALUE 'EL6521S'.       CL**4
00057      12  SCREEN-NUMBER               PIC X(4) VALUE '652B'.          CL**4
00058      12  TRANS-ID                    PIC X(4) VALUE 'EXD8'.          CL**4
00059      12  EL611-TRANS-ID              PIC X(4) VALUE 'EXL3'.          CL**4
           12  EL6525-TRANS-ID             PIC X(4) VALUE 'EXDF'.
           12  EL6526-TRANS-ID             PIC X(4) VALUE 'EXDG'.
00060      12  THIS-PGM                    PIC X(8) VALUE 'EL6521'.        CL**4
00061      12  PGM-NAME                    PIC X(8).                    EL6521
00062      12  TIME-IN                     PIC S9(7).                   EL6521
00063      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6521
00064          16  FILLER                  PIC X.                       EL6521
00065          16  TIME-OUT                PIC 99V99.                   EL6521
00066          16  FILLER                  PIC XX.                      EL6521
00067      12  XCTL-005                    PIC X(8) VALUE 'EL005'.         CL**4
00068      12  XCTL-010                    PIC X(8) VALUE 'EL010'.         CL**4
00069      12  XCTL-626                    PIC X(8) VALUE 'EL626'.         CL**4
00070      12  XCTL-611                    PIC X(8) VALUE 'EL611'.         CL**4
           12  XCTL-6525                   PIC X(8) VALUE 'EL6525'.        CL**4
030211     12  XCTL-6526                   PIC X(8) VALUE 'EL6526'.        CL**4
00071      12  XCTL-652                    PIC X(8) VALUE 'EL652'.         CL**4
00072      12  LINK-001                    PIC X(8) VALUE 'EL001'.         CL**4
00073      12  LINK-004                    PIC X(8) VALUE 'EL004'.         CL**4
00074      12  QID.                                                        CL**4
00075          16  QID-TERM                PIC X(4) VALUE SPACES.          CL**4
00076          16  FILLER                  PIC X(4) VALUE '521A'.          CL**4
00077      12  MAP-LENGTH                  PIC S9(4) VALUE +600  COMP.     CL**4
           12  ERCOBI-FILE-ID              PIC X(8) VALUE 'ERCOBI'.        CL**4
00078      12  ERCOMP-FILE-ID              PIC X(8) VALUE 'ERCOMP'.        CL**4
00079      12  ELACHP-FILE-ID              PIC X(8) VALUE 'ELACHP'.        CL**4
00080      12  ELBANK-FILE-ID              PIC X(8) VALUE 'ELBANK'.        CL**4
00081      12  WS-AGENT-BANK-DESC.                                         CL**4
00082          16 FILLER                   PIC X(7) VALUE '  AGENT'.       CL**4
00083          16 FILLER                   PIC X(1) VALUE X'7D'.           CL**4
00084          16 FILLER                   PIC X(15)                       CL**4
00085                                       VALUE 'S BANK ACCOUNT:'.       CL**4
00086                                                                   EL6521
00087      12  DEEDIT-FIELD                PIC X(15).                   EL6521
00088      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6521
00089                                                                   EL6521

           12  ERCOBI-KEY.
               16  ERCOBI-COMPANY-CD      PIC X.
               16  ERCOBI-STMT-OWNER      PIC X(4).
               16  ERCOBI-RGID            PIC X(12).

           12  WS-BILL-INST-SW            PIC X  VALUE SPACES.
               88  BILLING-INSTRUCTIONS-FOUND  VALUE 'Y'.
00091      12  ERROR-MESSAGES.                                          EL6521
00092          16  ER-0000                 PIC X(4) VALUE '0000'.          CL**4
00093          16  ER-0004                 PIC X(4) VALUE '0004'.          CL**4
00094          16  ER-0008                 PIC X(4) VALUE '0008'.          CL**4
00095          16  ER-0029                 PIC X(4) VALUE '0029'.          CL**4
00096          16  ER-0068                 PIC X(4) VALUE '0068'.          CL**4
00097          16  ER-0070                 PIC X(4) VALUE '0070'.          CL**4
00098          16  ER-0348                 PIC X(4) VALUE '0348'.          CL**4
00099          16  ER-0454                 PIC X(4) VALUE '0454'.          CL**4
062907         16  ER-0876                 PIC X(4) VALUE '0876'.
00100          16  ER-1228                 PIC X(4) VALUE '1228'.          CL**4
00101          16  ER-1626                 PIC X(4) VALUE '1626'.          CL**4
00102          16  ER-1629                 PIC X(4) VALUE '1629'.          CL**4
00103          16  ER-2039                 PIC X(4) VALUE '2039'.          CL**4
00104          16  ER-2233                 PIC X(4) VALUE '2233'.          CL**4
               16  ER-2797                 PIC X(4) VALUE '2797'.
00105          16  ER-7430                 PIC X(4) VALUE '7430'.          CL**4
00106          16  ER-7431                 PIC X(4) VALUE '7431'.          CL**4
00107          16  ER-7432                 PIC X(4) VALUE '7432'.          CL**4
00108          16  ER-7434                 PIC X(4) VALUE '7434'.          CL**4
00109          16  ER-7435                 PIC X(4) VALUE '7435'.          CL**4
00110          16  ER-7436                 PIC X(4) VALUE '7436'.          CL**4
00111          16  ER-7438                 PIC X(4) VALUE '7438'.          CL**4
00112          16  ER-7440                 PIC X(4) VALUE '7440'.          CL**4
00113          16  ER-7447                 PIC X(4) VALUE '7447'.          CL**4
00114          16  ER-7449                 PIC X(4) VALUE '7449'.          CL**4
00115          16  ER-7462                 PIC X(4) VALUE '7462'.          CL**4
00116          16  ER-7465                 PIC X(4) VALUE '7465'.          CL**4
00117          16  ER-7468                 PIC X(4) VALUE '7468'.          CL**4
00118          16  ER-7469                 PIC X(4) VALUE '7469'.          CL**4
062907         16  ER-8799                 PIC X(4) VALUE '8799'.
00119          16  ER-9388                 PIC X(4) VALUE '9388'.          CL**4
00120          16  ER-9399                 PIC X(4) VALUE '9399'.          CL**4
062907         16  ER-9999                 PIC X(4) VALUE '9999'.
00121                                                                   EL6521
00122      12  ELCNTL-KEY.                                              EL6521
00123          16  CNTL-COMP-ID            PIC X(3) VALUE SPACES.          CL**4
00124          16  CNTL-REC-TYPE           PIC X    VALUE SPACES.          CL**4
00125          16  CNTL-ACCESS             PIC X(4) VALUE SPACES.          CL**4
00126          16  CNTL-SEQ-NO             PIC S9(4) VALUE +0  COMP.       CL**4
00127                                                                      CL**4
00128      12  WS-BANK-INFORMATION.                                        CL**4
00129          16 WS-BANK-DATA.                                            CL**4
00130              20 WS-TRANSIT1              PIC X(4).                   CL**4
00131              20 WS-TRANSIT2              PIC X(4).                   CL**4
00132              20 WS-BKACCTI               PIC X(17).                  CL**4
00133          16 WS-ACTCDI                    PIC X(1).                   CL**4
00134                                                                   EL6521
00135      EJECT                                                        EL6521
00136      COPY ELCLOGOF.                                               EL6521
00137      EJECT                                                        EL6521
00138      COPY ELCDATE.                                                EL6521
00139      EJECT                                                        EL6521
00140      COPY ELCATTR.                                                EL6521
00141      EJECT                                                        EL6521
00142      COPY ELCEMIB.                                                EL6521
00143      EJECT                                                        EL6521
00144      COPY ELCSCTM.                                                EL6521
00145      EJECT                                                        EL6521
00146      COPY ELCSCRTY.                                               EL6521
00147      EJECT                                                        EL6521
00148      COPY ELCINTF.                                                EL6521
00149      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.           EL6521
00150          16  PI-MAINT                PIC  X.                      EL6521
00151          16  PI-CHECK-TYPE           PIC  X.                      EL6521
00152          16  PI-CHECK-CARRY-BAL      PIC  X.                      EL6521
00153          16  PI-FIRST-TIME-SW        PIC  X.                      EL6521
00154              88  FIRST-TIME                  VALUE 'Y'.           EL6521
00155          16  PI-ERCOMP-EOF-SW        PIC  X.                      EL6521
00156              88  ERCOMP-EOF                  VALUE 'Y'.           EL6521
00157          16  PI-SAVE-PHONE           PIC  X(10).                  EL6521
00158          16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).EL6521
00159          16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.      CL**2
00160          16  PI-ERCOMP-KEY.                                       EL6521
00161              20  PI-ERC-GROUP-CD     PIC  X.                      EL6521
00162              20  PI-ERC-CARRIER      PIC  X.                      EL6521
00163              20  PI-ERC-GROUP        PIC  X(6).                   EL6521
00164              20  PI-ERC-RESP         PIC  X(10).                  EL6521
00165              20  PI-ERC-ACCT         PIC  X(10).                  EL6521
00166              20  PI-ERC-TYPE         PIC  X.                      EL6521
00167          16  PI-SAVE-ERCOMP-KEY      PIC  X(29).                  EL6521
00168          16  PI-BANK-TRANSIT-NUMBER.                                 CL**4
00169              20  PI-BANK-COMPANY-CD  PIC X.                          CL**4
00170              20  PI-FEDERAL-NUMBER   PIC X(4).                       CL**4
00171              20  PI-BANK-NUMBER      PIC X(4).                       CL**4
00172          16  PI-BANK-ACCOUNT-NO      PIC X(17).                      CL**4
00173          16  PI-BANK-ACTION-CODE     PIC X.                          CL**4
               16  PI-ERCOBI-KEY.
                   20  PI-ERCOBI-COMPANY-CD PIC X.
                   20  PI-ERCOBI-STMT-OWNER PIC X(4).
                   20  PI-ERCOBI-RGID      PIC X(12).
               16  PI-SAVE-ERCOBI-KEY      PIC X(17).
00174          16  FILLER                  PIC  X(500).
00175                                                                      CL**4
00176      EJECT                                                        EL6521
00177      COPY ELCAID.                                                 EL6521
00178  01  FILLER    REDEFINES DFHAID.                                  EL6521
00179      12  FILLER                      PIC X(8).                    EL6521
00180      12  PF-VALUES                   PIC X       OCCURS 2.        EL6521
00181                                                                   EL6521
00182      EJECT                                                        EL6521
00183      COPY EL6521S.                                                EL6521
00184                                                                   EL6521
00185      EJECT                                                        EL6521
00186  LINKAGE SECTION.                                                 EL6521
00187  01  DFHCOMMAREA                     PIC X(1024).                    CL**4
00188                                                                   EL6521
00189      COPY ERCCOMP.                                                EL6521
           COPY ERCCOBI.
00190      COPY ELCACHP.                                                   CL**4
00191      COPY ELCBANK.                                                   CL**4

00194  PROCEDURE DIVISION.                                              EL6521
00195                                                                   EL6521
00196      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6521
00197      MOVE '5'                    TO DC-OPTION-CODE.               EL6521
00198      PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.                    EL6521
00199      MOVE DC-GREG-DATE-1-EDIT    TO WS-SAVE-DATE.                 EL6521
00200      MOVE DC-BIN-DATE-1          TO WS-SAVE-BIN-DT.               EL6521
00201                                                                   EL6521
00202      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6521
00203      MOVE EIBTRMID               TO QID-TERM.                        CL**4
00204      MOVE +2                     TO EMI-NUMBER-OF-LINES.          EL6521
00205      MOVE SPACE                  TO SUPPRESS-MAP-SW.                 CL**4
00206                                                                   EL6521
00207      IF EIBCALEN = 0                                              EL6521
00208         GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF

           IF PI-RETURN-TO-PROGRAM = THIS-PGM                           
              MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM
           END-IF
                                                                        
           IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
                 MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6    
                 MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5    
                 MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4    
                 MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3    
                 MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2    
                 MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1    
                 MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM  
                 MOVE THIS-PGM              TO PI-CALLING-PROGRAM    
              ELSE                                                     
                 MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM    
                 MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM  
                 MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1    
                 MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2    
                 MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3    
                 MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4    
                 MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5    
                 MOVE SPACES                TO PI-SAVED-PROGRAM-6
              END-IF
           END-IF

           MOVE LOW-VALUES             TO EL6521AI

00222      IF EIBTRNID = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                 EL6526-TRANS-ID
00223         CONTINUE
00224      ELSE
00225         GO TO 0100-NOT-EL611-RETURN
           END-IF
00226                                                                      CL**4
00227      PERFORM 0600-RECOVER-TS  THRU  0600-EXIT.                       CL**4
00228      MOVE XCTL-652           TO PI-RETURN-TO-PROGRAM.                CL**4
00229                                                                      CL**4
00230      MOVE PI-FEDERAL-NUMBER      TO TRNSIT1O.                        CL**4
00231      MOVE PI-BANK-NUMBER         TO TRNSIT2O.                        CL**4
00232      MOVE PI-BANK-ACCOUNT-NO     TO BKACCTO.                         CL**4
00233                                                                      CL**4
00234      IF PI-BANK-ACTION-CODE EQUAL   'P'                              CL**4
00235         MOVE 'PENDING'           TO CRSTATO.                         CL**4
00236                                                                      CL**4
00237      IF PI-BANK-ACTION-CODE EQUAL   'A'                              CL**4
00238         MOVE 'ACTIVE '           TO CRSTATO.                         CL**4
00239                                                                      CL**4
00240      IF MAINTYPL GREATER THAN ZERO                                   CL**4
00241          MOVE AL-UABON           TO MAINTYPA.                        CL**4
00242                                                                      CL**4
00243      IF STATL    GREATER THAN ZERO                                   CL**4
00244          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00245          MOVE AL-UANON           TO STATA.                           CL**4
00246                                                                      CL**4
062907     IF WTHLDL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UNNON            TO WTHLDA
062907     END-IF

062907     IF GADDL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO GADDA
062907     END-IF

062907     IF MELSWL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MELSWA
062907     END-IF

062907     IF APCHKL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO APCHKA
062907     END-IF

062907     IF MDACTL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MDACTA
062907     END-IF

062907     IF MDDIVL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MDDIVA
062907     END-IF

062907     IF MDCNTRL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MDCNTRA
062907     END-IF

062907     IF MDAMTL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UNNON            TO MDAMTA
062907     END-IF

00247      IF EFFDTL   GREATER THAN ZERO                                   CL**4
00248          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00249          MOVE AL-UANON           TO EFFDTA.                          CL**4
00250                                                                      CL**4
00251      IF TRMDTL   GREATER THAN ZERO                                   CL**4
00252          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00253          MOVE AL-UANON           TO TRMDTA.                          CL**4
00254                                                                      CL**4
00255      IF COMM1L   GREATER THAN ZERO                                   CL**4
00256          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00257          MOVE AL-UANON           TO COMM1A.                          CL**4
00258                                                                      CL**4
00259      IF COMM2L   GREATER THAN ZERO                                   CL**4
00260          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00261          MOVE AL-UANON           TO COMM2A.                          CL**4
00262                                                                      CL**4
00263      IF COMM3L   GREATER THAN ZERO                                   CL**4
00264          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00265          MOVE AL-UANON           TO COMM3A.                          CL**4
00266                                                                      CL**4
00267      IF COMM4L   GREATER THAN ZERO                                   CL**4
00268          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00269          MOVE AL-UANON           TO COMM4A.                          CL**4
00270                                                                      CL**4
00271      IF TRNSIT1L GREATER THAN ZERO                                   CL**4
00272          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00273          MOVE AL-UABON           TO TRNSIT1A.                        CL**4
00274                                                                      CL**4
00275      IF TRNSIT2L GREATER THAN ZERO                                   CL**4
00276          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00277          MOVE AL-UABON           TO TRNSIT2A.                        CL**4
00278                                                                      CL**4
00279      IF BKACCTL  GREATER THAN ZERO                                   CL**4
00280          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00281          MOVE AL-UABON           TO BKACCTA.                         CL**4
00282                                                                      CL**4
00283      IF ACTCDL   GREATER THAN ZERO                                   CL**4
00284          MOVE 'Y'                TO MAP-CHANGED-SW                   CL**4
00285          MOVE AL-UABON           TO ACTCDA.                          CL**4
00286                                                                      CL**4
           IF DELTOL > 0
              MOVE 'Y'                 TO MAP-CHANGED-SW
              MOVE AL-UANON            TO DELTOA
           END-IF

           IF RGIDL > 0
              MOVE 'Y'                 TO MAP-CHANGED-SW
              MOVE AL-UANON            TO RGIDA
           END-IF

00287      IF MAP-NOT-CHANGED                                              CL**4
00288          GO TO 0100-NOT-EL611-RETURN.                                CL**4
00289                                                                      CL**4
00290      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
00291                                                                      CL**4
00292  0100-NOT-EL611-RETURN.                                              CL**4
00293                                                                   EL6521
00294      IF EIBTRNID NOT = TRANS-ID                                   EL6521
00295          MOVE PI-MAINT           TO MAINTYPO                      EL6521
00296          MOVE AL-UANON           TO MAINTYPA                      EL6521
00297          MOVE -1                 TO MAINTYPL                      EL6521
00298          IF PI-MAINT = 'S' OR 'C'                                 EL6521
00299              GO TO 4000-SHOW                                      EL6521
00300          ELSE                                                     EL6521
00301              GO TO 8100-SEND-INITIAL-MAP.                            CL**4
00302                                                                   EL6521
00303      EXEC CICS HANDLE CONDITION                                   EL6521
00304          PGMIDERR  (9600-PGMID-ERROR)                             EL6521
00305          ERROR     (9990-ABEND)                                   EL6521
00306      END-EXEC.                                                    EL6521
00307                                                                   EL6521
00308      IF EIBAID = DFHCLEAR                                         EL6521
00309          GO TO 9400-CLEAR.                                        EL6521
00310                                                                   EL6521
00311      EJECT                                                        EL6521
00312  0200-RECEIVE.                                                    EL6521
00313      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6521
00314          MOVE ER-0008            TO EMI-ERROR                     EL6521
00315          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6521
00316          MOVE -1                 TO PFENTERL                      EL6521
00317          GO TO 8200-SEND-DATAONLY.                                EL6521
00318                                                                   EL6521
00319      EXEC CICS RECEIVE                                            EL6521
00320          MAP      (MAP-NAME)                                      EL6521
00321          MAPSET   (MAPSET-NAME)                                   EL6521
00322          INTO     (EL6521AI)                                      EL6521
00323      END-EXEC.                                                    EL6521
00324                                                                   EL6521
00325      IF PFENTERL = 0                                              EL6521
00326          GO TO 0300-CHECK-PFKEYS.                                 EL6521
00327                                                                   EL6521
00328      IF EIBAID NOT = DFHENTER                                     EL6521
00329          MOVE ER-0004            TO EMI-ERROR                     EL6521
00330          GO TO 0320-INPUT-ERROR.                                  EL6521
00331                                                                   EL6521
00332      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)   EL6521
00333          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6521
00334      ELSE                                                         EL6521
00335          MOVE ER-0029              TO EMI-ERROR                   EL6521
00336          GO TO 0320-INPUT-ERROR.                                  EL6521
00337                                                                   EL6521
00338      EJECT                                                        EL6521
00339                                                                      CL**4
00340  0300-CHECK-PFKEYS.                                               EL6521
00341      IF PI-AR-PROCESSING                                             CL**4
00342         IF EIBAID = DFHPF1                                           CL**4
00343              PERFORM 0500-WRITE-TS  THRU  0500-EXIT                  CL**4
00344              MOVE XCTL-611       TO PGM-NAME                         CL**4
00345              GO TO 9300-XCTL.                                        CL**4

00342      IF EIBAID = DFHPF2
              MOVE PI-COMPANY-CD       TO PI-ERCOBI-COMPANY-CD
              MOVE DELTOI              TO PI-ERCOBI-STMT-OWNER
              MOVE RGIDI               TO PI-ERCOBI-RGID
00343         PERFORM 0500-WRITE-TS    THRU 0500-EXIT
00344         MOVE XCTL-6525           TO PGM-NAME
00345         GO TO 9300-XCTL
           END-IF

030211     IF EIBAID = DFHPF3
030211        PERFORM 0500-WRITE-TS    THRU 0500-EXIT
030211        MOVE XCTL-6526           TO PGM-NAME
030211        GO TO 9300-XCTL
030211     END-IF

00347      IF EIBAID = DFHPF23                                          EL6521
00348          GO TO 8810-PF23.                                         EL6521
00349      IF EIBAID = DFHPF24                                          EL6521
00350          GO TO 9200-RETURN-MAIN-MENU.                             EL6521
00351      IF EIBAID = DFHPF12                                          EL6521
00352          GO TO 9500-PF12.                                         EL6521
00353      IF EIBAID = DFHENTER                                         EL6521
00354          GO TO 0330-CHECK-MAINTYP.                                EL6521
00355                                                                   EL6521
00356      MOVE ER-0029                TO EMI-ERROR.                    EL6521
00357  0320-INPUT-ERROR.                                                EL6521
00358      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6521
00359      MOVE AL-UNBON               TO PFENTERA.                     EL6521
00360      MOVE -1                     TO PFENTERL.                     EL6521
00361      GO TO 8200-SEND-DATAONLY.                                    EL6521
00362                                                                   EL6521
00363  EJECT                                                            EL6521
00364  0330-CHECK-MAINTYP.                                              EL6521
00365                                                                      CL**4
00366      IF MAINTYPL GREATER ZERO                                     EL6521
00367          IF MAINTYPI = 'S' OR 'C'                                 EL6521
00368              MOVE AL-UANON       TO MAINTYPA                      EL6521
00369              MOVE MAINTYPI       TO PI-MAINT                      EL6521
00370          ELSE                                                     EL6521
00371              MOVE -1             TO MAINTYPL                      EL6521
00372              MOVE AL-UABON       TO MAINTYPA                      EL6521
00373              MOVE ER-2039        TO EMI-ERROR                     EL6521
00374              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6521
00375              GO TO 8200-SEND-DATAONLY                             EL6521
00376      ELSE                                                         EL6521
00377          MOVE -1                 TO MAINTYPL                      EL6521
00378          MOVE AL-UABON           TO MAINTYPA                      EL6521
00379          MOVE ER-2039            TO EMI-ERROR                     EL6521
00380          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6521
00381          GO TO 8200-SEND-DATAONLY.                                EL6521
00382                                                                   EL6521
00383      IF PI-MAINT = 'S'                                            EL6521
00384          GO TO 4000-SHOW.                                         EL6521
00385                                                                   EL6521
00386      GO TO 4200-MAINT.                                            EL6521
00387                                                                   EL6521
00388      EJECT                                                        EL6521
00389                                                                   EL6521
00390  0500-WRITE-TS.                                                      CL**4
00391       EXEC CICS WRITEQ TS                                            CL**4
00392           QUEUE  (QID)                                               CL**4
00393           FROM   (EL6521AO)                                          CL**4
00394           LENGTH (MAP-LENGTH)                                        CL**4
00395      END-EXEC.                                                       CL**4
00396                                                                      CL**4
00397  0500-EXIT.                                                          CL**4
00398       EXIT.                                                          CL**4
00399                                                                      CL**4
00400  0600-RECOVER-TS.                                                    CL**4
00401       EXEC CICS READQ TS                                             CL**4
00402           QUEUE  (QID)                                               CL**4
00403           INTO (EL6521AO)                                            CL**4
00404           LENGTH (MAP-LENGTH)                                        CL**4
00405      END-EXEC.                                                       CL**4
00406                                                                      CL**4
00407      PERFORM 0700-DELETE-TS  THRU  0700-EXIT.                        CL**4
00408                                                                      CL**4
00409  0600-EXIT.                                                          CL**4
00410       EXIT.                                                          CL**4
00411                                                                      CL**4
00412  0700-DELETE-TS.                                                     CL**4
00413      EXEC CICS HANDLE CONDITION                                      CL**4
00414          QIDERR (0700-EXIT)                                          CL**4
00415      END-EXEC.                                                       CL**4
00416                                                                      CL**4
00417      EXEC CICS DELETEQ TS                                            CL**4
00418          QUEUE (QID)                                                 CL**4
00419      END-EXEC.                                                       CL**4
00420                                                                      CL**4
00421  0700-EXIT.                                                          CL**4
00422       EXIT.                                                          CL**4
00423       EJECT                                                          CL**4
00424                                                                      CL**4
00425  4000-SHOW.                                                       EL6521
00426                                                                      CL**4
00427      PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.                     EL6521
00428                                                                      CL**4
00429      IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                 EL6526-TRANS-ID
00430          NEXT SENTENCE                                               CL**4
00431      ELSE                                                            CL**4
00432          MOVE LOW-VALUES         TO EL6521AO.                        CL**4
00433                                                                      CL**4
00434      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6521
00435                                                                   EL6521
00436      EJECT                                                        EL6521
00437  4200-MAINT.                                                      EL6521
00438      IF NOT MODIFY-CAP                                            EL6521
00439          MOVE 'UPDATE'           TO SM-READ                       EL6521
00440          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6521
00441          MOVE ER-0070            TO EMI-ERROR                     EL6521
00442          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6521
00443          GO TO 8100-SEND-INITIAL-MAP.                             EL6521
00444                                                                   EL6521
00445      MOVE 'N'                     TO SUPPRESS-MAP-SW.                CL**4
00446      PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.                        CL**4
00447                                                                      CL**4
00448      IF EMI-NO-ERRORS                                                CL**4
00449          NEXT SENTENCE                                               CL**4
00450      ELSE                                                            CL**4
00451        IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                    EL6526-TRANS-ID
00452            GO TO 8100-SEND-INITIAL-MAP                               CL**4
00453        ELSE                                                          CL**4
00454            GO TO 8200-SEND-DATAONLY.                                 CL**4
00455                                                                      CL**4
00456      PERFORM 7000-EDIT THRU 7099-EXIT.                            EL6521
00457                                                                   EL6521
00458      IF EMI-NO-ERRORS                                             EL6521
00459          NEXT SENTENCE                                            EL6521
00460      ELSE                                                         EL6521
00461        IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                   EL6526-TRANS-ID
00462            GO TO 8100-SEND-INITIAL-MAP                               CL**4
00463        ELSE                                                          CL**4
00464            GO TO 8200-SEND-DATAONLY.                                 CL**4
00465                                                                      CL**4
00466      MOVE ER-0000               TO EMI-ERROR                         CL**4
00467      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00468                                                                   EL6521
00469      PERFORM 7300-READ-ERCOMP-UPDATE THRU 7300-EXIT.              EL6521
00470                                                                   EL6521
00471      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.              EL6521
00472                                                                   EL6521
00473      MOVE PI-PROCESSOR-ID        TO CO-LAST-MAINT-USER.           EL6521
00474      MOVE EIBTIME                TO CO-LAST-MAINT-HHMMSS.         EL6521
00475      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6521
00476      MOVE '5'                    TO DC-OPTION-CODE.               EL6521
00477      PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.                    EL6521
00478                                                                   EL6521
00479      MOVE DC-BIN-DATE-1          TO CO-LAST-MAINT-DT.             EL6521
00480                                                                   EL6521
00481      PERFORM 5500-PROCESS-ELACHP THRU 5599-PROCESS-EXIT

           PERFORM 6100-CHECK-BILL-INSTR
                                       THRU 6100-EXIT

00482                                                                      CL**4
00483      EXEC CICS REWRITE                                            EL6521
00484          DATASET  (ERCOMP-FILE-ID)                                   CL**4
00485          FROM     (COMPENSATION-MASTER)                           EL6521
00486      END-EXEC.                                                    EL6521
00487                                                                   EL6521
00488      PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.                     EL6521
00489      MOVE LOW-VALUES             TO EL6521AO.                     EL6521
00490      MOVE 'C'                    TO PI-MAINT.                     EL6521
00491                                                                   EL6521
00492      EJECT                                                        EL6521
00493                                                                   EL6521
00494  5000-BUILD-INITIAL-SCREEN.                                       EL6521
00495      MOVE CO-CARRIER             TO CARRIERO.                     EL6521
00496      MOVE CO-GROUPING            TO GROUPO.                       EL6521
00497      MOVE CO-TYPE                TO TYPEO.                        EL6521
00498      MOVE CO-RESP-NO             TO FINRESPO.                     EL6521
00499      MOVE CO-CTL-2               TO COACCTO.                         CL**4
00500      MOVE CO-GA-STATUS-CODE      TO STATO.                        EL6521

00502      IF CO-GA-STATUS-CODE = 'A' OR 'I' OR 'P'                        CL**4
00503          MOVE CO-GA-STATUS-CODE  TO STATO                            CL**4
00504          MOVE AL-UANON           TO STATA                            CL**4
00505          MOVE +1                 TO STATL                            CL**4
00506      ELSE                                                            CL**4
00507          MOVE AL-UANOF           TO STATA
           END-IF

           IF CO-GA-WITHOLD-PCT NOT NUMERIC
              MOVE ZEROS               TO CO-GA-WITHOLD-PCT
           END-IF
062907     MOVE CO-GA-WITHOLD-PCT      TO WTHLDO
           IF CO-GA-WITHOLD-PCT NOT = ZEROS
              MOVE +3                  TO WTHLDL
              MOVE AL-UANON            TO WTHLDA
           END-IF
062907     IF CO-GA-DIRECT-DEP = 'Y'
              MOVE 'Y'                 TO GADDO
              MOVE AL-UANON            TO GADDA
              MOVE +1                  TO GADDL
           ELSE
              MOVE 'N'                 TO GADDO
              MOVE AL-UANON            TO GADDA
              MOVE +1                  TO GADDL
           END-IF

062907     IF CO-DELIVER-CK-TO-MEL = 'Y'
              MOVE 'Y'                 TO MELSWO
              MOVE AL-UANON            TO MELSWA
              MOVE +1                  TO MELSWL
           ELSE
              MOVE 'N'                 TO MELSWO
              MOVE AL-UANON            TO MELSWA
              MOVE +1                  TO MELSWL
           END-IF

062907     IF CO-CREATE-AP-CHECK = 'Y'
              MOVE 'Y'                 TO APCHKO
              MOVE AL-UANON            TO APCHKA
              MOVE +1                  TO APCHKL
           ELSE
              MOVE 'N'                 TO APCHKO
              MOVE AL-UANON            TO APCHKA
              MOVE +1                  TO APCHKL
           END-IF

           IF CO-MD-GL-ACCT = SPACES OR LOW-VALUES
              MOVE AL-UANOF            TO MDACTA
           ELSE
              MOVE CO-MD-GL-ACCT       TO MDACTO
              MOVE +2                  TO MDACTL
              MOVE AL-UANON            TO MDACTA
           END-IF

           IF CO-MD-DIV = SPACES OR LOW-VALUES
              MOVE AL-UANOF            TO MDDIVA
           ELSE
              MOVE CO-MD-DIV           TO MDDIVO
              MOVE AL-UANON            TO MDDIVA
           END-IF

           IF CO-MD-CENTER = SPACES OR LOW-VALUES
              MOVE AL-UANOF            TO MDCNTRA
           ELSE
              MOVE CO-MD-CENTER        TO MDCNTRO
              MOVE +2                  TO MDCNTRL
              MOVE AL-UANON            TO MDCNTRA
           END-IF

           IF CO-MD-AMT NOT NUMERIC
              MOVE ZEROS               TO CO-MD-AMT
           END-IF
           MOVE CO-MD-AMT              TO MDAMTO
           IF CO-MD-AMT NOT = ZEROS
              MOVE +3                  TO MDAMTL
              MOVE AL-UANON            TO MDAMTA
           END-IF
           MOVE ZEROS                  TO MDLOBO
                                          MDSTO

00509      IF CO-GA-EFFECTIVE-DT = LOW-VALUES OR SPACES                    CL**4
00510          MOVE AL-UANOF           TO EFFDTA                           CL**4
00511      ELSE                                                            CL**4
00512          MOVE CO-GA-EFFECTIVE-DT TO DC-BIN-DATE-1                    CL**4
00513          MOVE ' '                TO DC-OPTION-CODE                   CL**4
00514          PERFORM 9700-DATE-CONVERT THRU 9700-EXIT                 EL6521
00515          MOVE DC-GREG-DATE-1-EDIT                                    CL**4
00516                                  TO EFFDTO                           CL**4
00517          MOVE AL-UANON           TO EFFDTA                           CL**4
00518          MOVE +8                 TO EFFDTL.                          CL**4
00519                                                                   EL6521
           IF CO-GA-TERMINATION-DT = HIGH-VALUES
              MOVE '99/99/99'          TO TRMDTO
              MOVE AL-UANON            TO TRMDTA
           ELSE
00520         IF CO-GA-TERMINATION-DT = LOW-VALUES OR SPACES
00521             MOVE AL-UANOF        TO TRMDTA
00522         ELSE
00523            MOVE CO-GA-TERMINATION-DT
00524                                  TO DC-BIN-DATE-1
00525            MOVE ' '              TO DC-OPTION-CODE
00526            PERFORM 9700-DATE-CONVERT
                                       THRU 9700-EXIT
00527            MOVE DC-GREG-DATE-1-EDIT
00528                                  TO TRMDTO
00529            MOVE AL-UANON         TO TRMDTA
00530            MOVE +8               TO TRMDTL
              END-IF
           END-IF
00531                                                                   EL6521
00532      IF CO-GA-COMMENT-1 = LOW-VALUES OR SPACES                       CL**4
00533          MOVE AL-UANOF            TO COMM1A                          CL**4
00534      ELSE                                                            CL**4
00535          MOVE CO-GA-COMMENT-1    TO COMM1O                           CL**4
00536          MOVE AL-UANON           TO COMM1A                           CL**4
00537          MOVE +40                TO COMM1L.                          CL**4
00538                                                                   EL6521
00539      IF CO-GA-COMMENT-2 = LOW-VALUES OR SPACES                       CL**4
00540          MOVE AL-UANOF            TO COMM2A                          CL**4
00541      ELSE                                                            CL**4
00542          MOVE CO-GA-COMMENT-2    TO COMM2O                           CL**4
00543          MOVE AL-UANON           TO COMM2A                           CL**4
00544          MOVE +40                TO COMM2L.                          CL**4
00545                                                                   EL6521
00546      IF CO-GA-COMMENT-3 = LOW-VALUES OR SPACES                       CL**4
00547          MOVE AL-UANOF           TO COMM3A                           CL**4
00548      ELSE                                                            CL**4
00549          MOVE CO-GA-COMMENT-3    TO COMM3O                           CL**4
00550          MOVE AL-UANON           TO COMM3A                           CL**4
00551          MOVE +40                TO COMM3L.                          CL**4
00552                                                                      CL**4
00553      IF CO-GA-COMMENT-4 = LOW-VALUES OR SPACES                       CL**4
00554          MOVE AL-UANOF           TO COMM4A                           CL**4
00555      ELSE                                                            CL**4
00556          MOVE CO-GA-COMMENT-4    TO COMM4O                           CL**4
00557          MOVE AL-UANON           TO COMM4A                           CL**4
00558          MOVE +40                TO COMM4L.                          CL**4
00559                                                                      CL**4
020816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           or 'FNL'
              CONTINUE
           ELSE
00560         IF (CO-BANK-TRANSIT-NO (1:4) = LOW-VALUES OR SPACES) AND
00561            (CO-BANK-TRANSIT-NO (5:4) = LOW-VALUES OR SPACES)
00562             MOVE AL-UANOF        TO TRNSIT1A
00563             MOVE AL-UANOF        TO TRNSIT2A
00564         ELSE
00565            MOVE CO-BANK-TRANSIT-NO (1:4)
00566                                  TO PI-BANK-TRANSIT-NUMBER (2:4)
00567                                     TRNSIT1O
00568            MOVE CO-BANK-TRANSIT-NO (5:4)
00569                                  TO PI-BANK-TRANSIT-NUMBER (6:4)
00570                                     TRNSIT2O
              END-IF
00572         IF CO-BANK-ACCOUNT-NUMBER = LOW-VALUES OR SPACES
00573            MOVE AL-UANOF         TO BKACCTA
00574         ELSE
00575            MOVE CO-BANK-ACCOUNT-NUMBER
00576                                  TO PI-BANK-ACCOUNT-NO
00577                                     BKACCTO
              END-IF
           END-IF

00579      MOVE    '       '           TO CRSTATO.                         CL**4
00580                                                                      CL**4
00581      MOVE CO-ACH-STATUS          TO PI-BANK-ACTION-CODE.             CL**4
00582                                                                      CL**4
00583      IF CO-ACH-STATUS IS EQUAL      'P'                              CL**4
00584         MOVE 'PENDING'           TO CRSTATO.                         CL**4
00585                                                                      CL**4
00586      IF CO-ACH-STATUS IS EQUAL      'A'                              CL**4
00587         MOVE 'ACTIVE '           TO CRSTATO.                         CL**4

           IF CO-STMT-OWNER NOT = SPACES AND LOW-VALUES
              MOVE CO-STMT-OWNER       TO DELTOO
              MOVE AL-UANON            TO DELTOA
              MOVE +4                  TO DELTOL
           ELSE
              MOVE AL-UANOF            TO DELTOA
           END-IF

           IF CO-REPORT-GROUP-ID NOT = SPACES AND LOW-VALUES
              MOVE CO-REPORT-GROUP-ID  TO RGIDO
              MOVE AL-UANON            TO RGIDA
              MOVE +12                 TO RGIDL
           ELSE
              MOVE AL-UANOF            TO RGIDA
           END-IF

00589      MOVE PI-MAINT               TO MAINTYPO.                     EL6521
00590      MOVE AL-UANOF               TO MAINTYPA.                        CL**4
00591      MOVE -1                     TO MAINTYPL.                     EL6521
00592                                                                   EL6521
00593      GO TO 8100-SEND-INITIAL-MAP.                                 EL6521
00594                                                                   EL6521
00595  5099-EXIT.                                                       EL6521
00596      EXIT.                                                        EL6521
00597      EJECT                                                        EL6521
00598  5500-PROCESS-ELACHP.                                                CL**4
00599                                                                      CL**4
00600      IF ACTCDI = 'D'                                                 CL**4
00601         PERFORM 7400-DELETE-PRE-NOTE THRU 7499-EXIT                  CL**4
00602         MOVE SPACES              TO CO-BANK-INFORMATION              CL**4
00603                                     CO-ACH-STATUS                    CL**4
00604         GO TO 5599-PROCESS-EXIT.                                     CL**4
00605                                                                      CL**4
00606      IF ACTCDI = 'C'                                                 CL**4
00607          PERFORM 7400-DELETE-PRE-NOTE THRU 7499-EXIT                 CL**4
00608          PERFORM 7600-WRITE-ELACHP THRU 7620-WRITE-EXIT              CL**4
00609          MOVE WS-BANK-DATA       TO CO-BANK-INFORMATION              CL**4
00610          MOVE WS-BKACCTI         TO CO-BANK-ACCOUNT-NUMBER           CL**4
00611          MOVE 'P'                TO CO-ACH-STATUS                    CL**4
00612          GO TO 5599-PROCESS-EXIT.                                    CL**4
00613                                                                      CL**4
00614      IF ACTCDI =  'N'                                                CL**4
00615          PERFORM 7600-WRITE-ELACHP THRU 7620-WRITE-EXIT              CL**4
00616          MOVE WS-BANK-DATA       TO CO-BANK-INFORMATION              CL**4
00617          MOVE WS-BKACCTI         TO CO-BANK-ACCOUNT-NUMBER           CL**4
00618          MOVE 'P'                TO CO-ACH-STATUS  WS-ACTCDI         CL**4
00619          GO TO 5599-PROCESS-EXIT.                                    CL**4
00620                                                                      CL**4
00621      IF ACTCDI = 'A'                                                 CL**4
00622          MOVE 'A'                TO CO-ACH-STATUS  WS-ACTCDI         CL**4
00623          GO TO 5599-PROCESS-EXIT.                                    CL**4
00624                                                                      CL**4
00625  5599-PROCESS-EXIT.                                                  CL**4
00626                                                                      CL**4
00627      EXIT.                                                           CL**4
00628  6000-CHECK-FOR-UPDATE.                                           EL6521

00629      IF STATL GREATER ZERO                                        EL6521
00630         MOVE STATI               TO CO-GA-STATUS-CODE.            EL6521
00631                                                                   EL6521
00632      IF EFFDTL GREATER ZERO                                       EL6521
00633         MOVE WS-SAVE-BIN-EFFDT   TO CO-GA-EFFECTIVE-DT.           EL6521
00634                                                                   EL6521
00635      IF TRMDTL GREATER ZERO                                       EL6521
00636         MOVE WS-SAVE-BIN-TRMDT   TO CO-GA-TERMINATION-DT.         EL6521
00637                                                                   EL6521
062907     IF GADDL > ZERO
062907        MOVE GADDI               TO CO-GA-DIRECT-DEP
062907     END-IF

062907     IF MELSWL > ZERO
062907        MOVE MELSWI              TO CO-DELIVER-CK-TO-MEL
062907     END-IF

062907     IF APCHKL > ZERO
062907        MOVE APCHKI              TO CO-CREATE-AP-CHECK
062907     END-IF

062907     IF MDACTL > ZERO
062907        MOVE MDACTI              TO CO-MD-GL-ACCT
062907     END-IF

062907     IF MDDIVL > ZERO
062907        MOVE MDDIVI              TO CO-MD-DIV
062907     END-IF

062907     IF MDCNTRL > ZERO
062907        MOVE MDCNTRI             TO CO-MD-CENTER
062907     END-IF

062907     IF WTHLDL > ZERO
062907        MOVE WTHLDI              TO CO-GA-WITHOLD-PCT
062907     END-IF

062907     IF MDAMTL > ZERO
062907        MOVE MDAMTI              TO CO-MD-AMT
062907     END-IF

           IF DELTOL > ZERO
              MOVE DELTOI              TO CO-STMT-OWNER
           END-IF

           IF RGIDL > ZERO
              MOVE RGIDI               TO CO-REPORT-GROUP-ID
           END-IF

00638      IF COMM1L GREATER ZERO                                       EL6521
00639         MOVE COMM1I              TO CO-GA-COMMENT-1.              EL6521
00640                                                                   EL6521
00641      IF COMM2L GREATER ZERO                                       EL6521
00642         MOVE COMM2I              TO CO-GA-COMMENT-2.              EL6521
00643                                                                   EL6521
00644      IF COMM3L GREATER ZERO                                       EL6521
00645         MOVE COMM3I              TO CO-GA-COMMENT-3.              EL6521
00646                                                                   EL6521
00647      IF COMM4L GREATER ZERO                                       EL6521
00648         MOVE COMM4I              TO CO-GA-COMMENT-4.              EL6521
00649                                                                   EL6521
00650  6049-EXIT.                                                       EL6521
00651      EXIT.                                                        EL6521

       6100-CHECK-BILL-INSTR.

           MOVE PI-COMPANY-CD          TO ERCOBI-COMPANY-CD
           MOVE DELTOI                 TO ERCOBI-STMT-OWNER
           MOVE RGIDI                  TO ERCOBI-RGID

           EXEC CICS READ
              DATASET (ERCOBI-FILE-ID)
              SET     (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
              RIDFLD  (ERCOBI-KEY)
              RESP    (WS-RESPONSE)
              UPDATE
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE AL-UABON            TO DELTOA
                                          RGIDA
              MOVE ER-2797             TO EMI-ERROR
              MOVE -1                  TO DELTOL
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           .
       6100-EXIT.
           EXIT.
00653                                                                   EL6521
00654  7000-EDIT.                                                       EL6521
00655      IF STATL GREATER ZERO                                        EL6521
00656          IF STATI = 'A' OR 'I' OR 'P'                             EL6521
00657              NEXT SENTENCE                                        EL6521
00658           ELSE                                                    EL6521
00659              MOVE -1             TO STATL                         EL6521
00660              MOVE AL-UABON       TO STATA                         EL6521
00661              MOVE ER-7438        TO EMI-ERROR                     EL6521
00662              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6521
00663                                                                   EL6521
00664      MOVE LOW-VALUES             TO WS-SAVE-BIN-EFFDT.               CL**4
00665      MOVE LOW-VALUES             TO WS-SAVE-BIN-TRMDT.               CL**4
00666                                                                      CL**4
00667      IF EFFDTL NOT = ZERO                                         EL6521
00668          MOVE EFFDTI             TO DEEDIT-FIELD                  EL6521
00669          PERFORM 8600-DEEDIT                                      EL6521
00670          MOVE DEEDIT-FIELD-V0    TO DC-GREG-DATE-1-MDY            EL6521
00671          MOVE '4'                TO DC-OPTION-CODE                EL6521
00672          PERFORM 9700-DATE-CONVERT THRU 9700-EXIT                 EL6521
00673          IF DATE-CONVERSION-ERROR                                 EL6521
00674              MOVE ER-0348        TO EMI-ERROR                     EL6521
00675              MOVE -1             TO EFFDTL                        EL6521
00676              MOVE AL-UABON       TO EFFDTA                        EL6521
00677              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6521
00678          ELSE                                                     EL6521
00679              MOVE DC-GREG-DATE-1-EDIT  TO EFFDTI                  EL6521
00680              MOVE AL-UANON             TO EFFDTA                     CL**4
00681              MOVE DC-BIN-DATE-1        TO WS-SAVE-BIN-EFFDT.      EL6521
00682                                                                   EL6521
00683      IF TRMDTL NOT = ZERO                                         EL6521
00684         MOVE TRMDTI              TO DEEDIT-FIELD
00685         PERFORM 8600-DEEDIT
00686         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
              IF DC-GREG-DATE-1-MDY = 999999
                 MOVE HIGH-VALUES      TO WS-SAVE-BIN-TRMDT
              ELSE
00687            MOVE '4'              TO DC-OPTION-CODE
00688            PERFORM 9700-DATE-CONVERT
                                       THRU 9700-EXIT
00689            IF DATE-CONVERSION-ERROR
00690               MOVE ER-0454       TO EMI-ERROR
00691               MOVE -1            TO TRMDTL
00692               MOVE AL-UABON      TO TRMDTA
00693               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
00694            ELSE
00695               MOVE DC-GREG-DATE-1-EDIT
                                       TO TRMDTI
00696               MOVE AL-UANON      TO TRMDTA
00697               MOVE DC-BIN-DATE-1 TO WS-SAVE-BIN-TRMDT
                 END-IF
              END-IF
           END-IF

062907     IF GADDL > ZERO
062907        IF GADDI = 'Y' OR 'N' OR ' '
062907           MOVE AL-UANON         TO GADDA
062907        ELSE
062907           MOVE AL-UABON         TO GADDA
062907           MOVE ER-0876          TO EMI-ERROR
062907           MOVE -1               TO GADDL
062907           PERFORM 9900-ERROR-FORMAT
062907                                 THRU 9900-EXIT
062907        END-IF
062907     END-IF

092707     IF MELSWL > ZERO
092707        IF MELSWI = 'Y' OR 'N' OR ' '
092707           MOVE AL-UANON         TO MELSWA
092707        ELSE
092707           MOVE AL-UABON         TO MELSWA
092707           MOVE ER-0876          TO EMI-ERROR
092707           MOVE -1               TO MELSWL
092707           PERFORM 9900-ERROR-FORMAT
092707                                 THRU 9900-EXIT
092707        END-IF
092707     END-IF

092707     IF APCHKL > ZERO
092707        IF APCHKI = 'Y' OR 'N' OR ' '
092707           MOVE AL-UANON         TO APCHKA
092707        ELSE
092707           MOVE AL-UABON         TO APCHKA
092707           MOVE ER-0876          TO EMI-ERROR
092707           MOVE -1               TO APCHKL
092707           PERFORM 9900-ERROR-FORMAT
092707                                 THRU 9900-EXIT
092707        END-IF
092707     END-IF

062907     IF WTHLDL  > ZERO
062907        EXEC CICS BIF 
062907           DEEDIT
062907           FIELD   (WTHLDI)  
062907           LENGTH  (6)      
062907        END-EXEC      
062907        IF WTHLDI NUMERIC
062907           MOVE AL-UNNON         TO WTHLDA
062907        ELSE
062907           MOVE -1               TO WTHLDL
062907           MOVE AL-UABON         TO WTHLDA
062907           MOVE ER-8799          TO EMI-ERROR  
062907           PERFORM 9900-ERROR-FORMAT
062907                                 THRU  9900-EXIT
062907        END-IF
062907     END-IF

062907     IF MDAMTL  > ZERO
062907        EXEC CICS BIF 
062907           DEEDIT
062907           FIELD   (MDAMTI)  
062907           LENGTH  (8)      
062907        END-EXEC      
062907        IF MDAMTI NUMERIC
062907           MOVE AL-UNNON         TO MDAMTA
062907        ELSE
062907           MOVE -1               TO MDAMTL
062907           MOVE AL-UABON         TO MDAMTA
062907           MOVE ER-8799          TO EMI-ERROR  
062907           PERFORM 9900-ERROR-FORMAT
062907                                 THRU  9900-EXIT
062907        END-IF
062907     END-IF

00699      IF WS-SAVE-BIN-EFFDT = LOW-VALUES OR                            CL**4
00700         WS-SAVE-BIN-TRMDT = LOW-VALUES                               CL**4
00701          NEXT SENTENCE                                               CL**4
00702        ELSE                                                          CL**4
00703          IF WS-SAVE-BIN-TRMDT LESS THAN WS-SAVE-BIN-EFFDT            CL**4
00704              MOVE ER-1228        TO EMI-ERROR                        CL**4
00705              MOVE -1             TO EFFDTL                           CL**4
00706              MOVE AL-UABON       TO EFFDTA                           CL**4
00707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
00708                                                                      CL**4
            IF ((DELTOL > +0)
               OR (RGIDL > +0))
               AND ((DELTOI NOT = SPACES)
               AND (RGIDI NOT = SPACES))
               PERFORM 6100-CHECK-BILL-INSTR
                                       THRU 6100-EXIT
            END-IF

00709      MOVE SPACES                 TO WS-BANK-INFORMATION.             CL**4
00710                                                                      CL**4
00711      IF PI-AR-PROCESSING                                             CL**4
00712         NEXT SENTENCE                                                CL**4
00713      ELSE                                                            CL**4
00714         MOVE ZEROS               TO ACTCDL, TRNSIT1L TRNSIT2L        CL**4
00715                                     BKACCTL                          CL**4
00716         GO TO 7099-EXIT.                                             CL**4
00717                                                                      CL**4
00718      IF (TRNSIT1L NOT = ZERO) OR                                     CL**4
00719         (TRNSIT2L NOT = ZERO) OR                                     CL**4
00720         (BKACCTL  NOT = ZERO) OR                                     CL**4
00721         (ACTCDL   NOT = ZERO)                                        CL**4
00722          MOVE 'Y'                TO WS-ACH-SW                        CL**4
00723      ELSE                                                            CL**4
00724          GO TO 7099-EXIT.                                            CL**4
00725                                                                      CL**4
00726      IF (ACTCDL = ZERO) OR                                           CL**4
00727         (ACTCDI NOT = 'N' AND 'C' AND 'A' AND 'D')                   CL**4
00728          MOVE -1                 TO ACTCDL                           CL**4
00729          MOVE AL-UABON           TO ACTCDA                           CL**4
00730          MOVE ER-7436            TO EMI-ERROR                        CL**4
00731          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00732          GO TO 7099-EXIT.                                            CL**4
00733                                                                      CL**4
00734      IF ACTCDI = 'N'                                                 CL**4
00735          GO TO 7020-NEW-ACH.                                         CL**4
00736                                                                      CL**4
00737      IF ACTCDI = 'C'                                                 CL**4
00738          GO TO 7040-CHANGE-ACH.                                      CL**4
00739                                                                      CL**4
00740      IF ACTCDI = 'A'                                                 CL**4
00741          GO TO 7060-ACTIVATE-ACH.                                    CL**4
00742                                                                      CL**4
00743 ****   ACH DELETE PROCESSING EDITS                                   CL**4
00744                                                                      CL**4
00745      IF TRNSIT1L NOT = ZERO OR                                       CL**4
00746         TRNSIT2L NOT = ZERO OR                                       CL**4
00747         BKACCTL  NOT = ZERO                                          CL**4
00748          MOVE -1                 TO ACTCDL                           CL**4
00749          MOVE AL-UABON           TO ACTCDA                           CL**4
00750          MOVE ER-7430            TO EMI-ERROR                        CL**4
00751          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
00752                                                                      CL**4
00753      GO TO 7099-EXIT.                                                CL**4
00754                                                                      CL**4
00755 ****   ACH NEW ENTRY PROCESSING EDITS                                CL**4
00756  7020-NEW-ACH.                                                       CL**4
00757                                                                      CL**4
00758      IF CO-ACH-STATUS = 'A' OR 'P'                                   CL**4
00759          MOVE -1                 TO ACTCDL                           CL**4
00760          MOVE AL-UABON           TO ACTCDA                           CL**4
00761          MOVE ER-7434            TO EMI-ERROR                        CL**4
00762          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00763          GO TO 7099-EXIT.                                            CL**4
00764                                                                      CL**4
00765      IF TRNSIT1L = ZERO OR                                           CL**4
00766         TRNSIT2L = ZERO OR                                           CL**4
00767         BKACCTL = ZERO                                               CL**4
00768          MOVE -1                 TO TRNSIT1L                         CL**4
00769          MOVE ER-7431            TO EMI-ERROR                        CL**4
00770          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00771          GO TO 7099-EXIT.                                            CL**4
00772                                                                      CL**4
00773      IF TRNSIT1L GREATER THAN ZERO                                   CL**4
00774          IF TRNSIT1I = SPACES                                        CL**4
00775              MOVE -1             TO TRNSIT1L                         CL**4
00776              MOVE ER-7435        TO EMI-ERROR                        CL**4
00777              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00778              GO TO 7099-EXIT                                         CL**4
00779          ELSE                                                        CL**4
00780              MOVE TRNSIT1I       TO WS-TRANSIT1.                     CL**4
00781                                                                      CL**4
00782      IF TRNSIT2L GREATER THAN ZERO                                   CL**4
00783          IF TRNSIT2I = SPACES                                        CL**4
00784              MOVE -1             TO TRNSIT2L                         CL**4
00785              MOVE ER-7435        TO EMI-ERROR                        CL**4
00786              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00787              GO TO 7099-EXIT                                         CL**4
00788          ELSE                                                        CL**4
00789              MOVE TRNSIT2I       TO WS-TRANSIT2.                     CL**4
00790                                                                      CL**4
00791      IF BKACCTL GREATER THAN ZERO                                    CL**4
00792          IF BKACCTI = SPACES                                         CL**4
00793              MOVE -1             TO BKACCTL                          CL**4
00794              MOVE ER-7435        TO EMI-ERROR                        CL**4
00795              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00796              GO TO 7099-EXIT                                         CL**4
00797          ELSE                                                        CL**4
00798              MOVE BKACCTI        TO WS-BKACCTI.                      CL**4
00799          MOVE BKACCTI            TO WS-BKACCTI.                      CL**4
00800                                                                      CL**4
00801      PERFORM 7500-VALIDATE-TRANSIT  THRU  7500-VALIDATE-EXIT.        CL**4
00802                                                                      CL**4
00803      GO TO 7099-EXIT.                                                CL**4
00804                                                                      CL**4
00805 ****   ACH CHANGE ENTRY PROCESSING EDITS                             CL**4
00806  7040-CHANGE-ACH.                                                    CL**4
00807                                                                      CL**4
00808      IF (TRNSIT1L = ZERO) AND                                        CL**4
00809         (TRNSIT2L = ZERO) AND                                        CL**4
00810         (BKACCTL = ZERO)                                             CL**4
00811          MOVE -1                 TO TRNSIT1L                         CL**4
00812          MOVE ER-7435            TO EMI-ERROR                        CL**4
00813          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00814          GO TO 7099-EXIT.                                            CL**4
00815                                                                      CL**4
00816      IF CO-BANK-INFORMATION = SPACES OR LOW-VALUES                   CL**4
00817          MOVE -1                 TO ACTCDL                           CL**4
00818          MOVE ER-7432            TO EMI-ERROR                        CL**4
00819          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00820          GO TO 7099-EXIT.                                            CL**4
00821                                                                      CL**4
00822      MOVE CO-BANK-INFORMATION    TO WS-BANK-INFORMATION.             CL**4
00823                                                                      CL**4
00824      IF TRNSIT1L GREATER THAN ZERO                                   CL**4
00825          IF TRNSIT1I = SPACES                                        CL**4
00826              MOVE -1             TO TRNSIT1L                         CL**4
00827              MOVE ER-7435        TO EMI-ERROR                        CL**4
00828              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00829              GO TO 7099-EXIT                                         CL**4
00830          ELSE                                                        CL**4
00831              MOVE TRNSIT1I       TO WS-TRANSIT1.                     CL**4
00832                                                                      CL**4
00833      IF TRNSIT2L GREATER THAN ZERO                                   CL**4
00834          IF TRNSIT2I = SPACES                                        CL**4
00835              MOVE -1             TO TRNSIT2L                         CL**4
00836              MOVE ER-7435        TO EMI-ERROR                        CL**4
00837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00838              GO TO 7099-EXIT                                         CL**4
00839          ELSE                                                        CL**4
00840              MOVE TRNSIT2I       TO WS-TRANSIT2.                     CL**4
00841                                                                      CL**4
00842      IF BKACCTL GREATER THAN ZERO                                    CL**4
00843          IF BKACCTI = SPACES                                         CL**4
00844              MOVE -1             TO BKACCTL                          CL**4
00845              MOVE ER-7435        TO EMI-ERROR                        CL**4
00846              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00847              GO TO 7099-EXIT                                         CL**4
00848          ELSE                                                        CL**4
00849              MOVE BKACCTI        TO WS-BKACCTI.                      CL**4
00850                                                                      CL**4
00851      IF CO-BANK-INFORMATION = WS-BANK-INFORMATION                    CL**4
00852          MOVE -1                 TO ACTCDL                           CL**4
00853          MOVE ER-7449            TO EMI-ERROR                        CL**4
00854          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00855          GO TO 7099-EXIT.                                            CL**4
00856                                                                      CL**4
00857      IF TRNSIT1L GREATER THAN ZERO OR                                CL**4
00858         TRNSIT2L GREATER THAN ZERO                                   CL**4
00859          PERFORM 7500-VALIDATE-TRANSIT  THRU  7500-VALIDATE-EXIT.    CL**4
00860                                                                      CL**4
00861      GO TO 7099-EXIT.                                                CL**4
00862                                                                      CL**4
00863 ****   ACH ACTIVATE ENTRY PROCESSING EDITS                           CL**4
00864  7060-ACTIVATE-ACH.                                                  CL**4
00865                                                                      CL**4
00866      IF NOT CO-ACH-PENDING                                           CL**4
00867          MOVE -1                 TO ACTCDL                           CL**4
00868          MOVE ER-7465            TO EMI-ERROR                        CL**4
00869          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00870          GO TO 7099-EXIT.                                            CL**4
00871                                                                      CL**4
00872      IF TRNSIT1L NOT = ZERO OR                                       CL**4
00873         TRNSIT2L NOT = ZERO OR                                       CL**4
00874         BKACCTL  NOT = ZERO                                          CL**4
00875          MOVE -1                 TO ACTCDL                           CL**4
00876          MOVE ER-7440            TO EMI-ERROR                        CL**4
00877          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00878          GO TO 7099-EXIT.                                            CL**4
00879                                                                   EL6521
00880  7099-EXIT.                                                       EL6521
00881      EXIT.                                                        EL6521
00882      EJECT                                                        EL6521
00883                                                                   EL6521
00884  7100-READ-ERCOMP.                                                EL6521
00885                                                                      CL**4
00886      EXEC CICS HANDLE CONDITION                                   EL6521
00887          NOTOPEN (7100-NOTOPEN)                                      CL**4
00888          NOTFND  (7100-NOTFND)                                       CL**4
00889      END-EXEC.                                                    EL6521
00890                                                                   EL6521
00891      EXEC CICS READ                                               EL6521
00892           DATASET  (ERCOMP-FILE-ID)                                  CL**4
00893           SET      (ADDRESS OF COMPENSATION-MASTER)                  CL**3
00894           RIDFLD   (PI-ERCOMP-KEY)                                EL6521
00895      END-EXEC.                                                    EL6521
00896                                                                   EL6521
      *    PERFORM 7200-READ-ERCOBI    THRU 7200-EXIT

00897      IF DO-NOT-MOVE-TO-MAP                                           CL**4
00898         MOVE 'Y'                 TO SUPPRESS-MAP-SW                  CL**4
00899         GO TO 7100-EXIT.                                             CL**4
00900                                                                      CL**4
00901      MOVE CO-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL6521
00902      MOVE CO-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL6521
00903      MOVE CO-CONTROL-PRIMARY     TO PI-ERCOMP-KEY.                   CL**4
00904      MOVE CO-CARRIER             TO CARRIERO.                        CL**4
00905      MOVE CO-GROUPING            TO GROUPO.                          CL**4
00906      MOVE CO-TYPE                TO TYPEO.                           CL**4
00907      MOVE CO-RESP-NO             TO FINRESPO.                        CL**4
00908      MOVE CO-CTL-2               TO COACCTO.                         CL**4
00909      MOVE CO-BANK-INFORMATION    TO PI-BANK-TRANSIT-NUMBER (2:8)
           
00910      MOVE PI-FEDERAL-NUMBER      TO TRNSIT1O.                        CL**4
00911      MOVE PI-BANK-NUMBER         TO TRNSIT2O.                        CL**4
00912      GO TO 7100-EXIT.                                                CL**4
00913                                                                      CL**4
00914  7100-NOTFND.                                                        CL**4
00915      MOVE ER-7462               TO EMI-ERROR                         CL**4
00916      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00917      GO TO 7100-EXIT.                                                CL**4
00918                                                                      CL**4
00919  7100-NOTOPEN.                                                       CL**4
00920      MOVE ER-2233               TO EMI-ERROR                         CL**4
00921      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00922      GO TO 7100-EXIT.                                                CL**4
00923                                                                   EL6521
00924  7100-EXIT.                                                       EL6521
00925      EXIT.                                                        EL6521
00926      EJECT                                                        EL6521
00927                                                                   EL6521
       7200-READ-ERCOBI.

           MOVE CO-CONTROL-PRIMARY     TO ERCOBI-KEY

           EXEC CICS READ
              DATASET (ERCOBI-FILE-ID)
              SET     (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
              RIDFLD  (ERCOBI-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET BILLING-INSTRUCTIONS-FOUND TO TRUE
           END-IF

           .
       7200-EXIT.
           EXIT.

00928  7300-READ-ERCOMP-UPDATE.                                         EL6521
00929      EXEC CICS HANDLE CONDITION                                   EL6521
00930          NOTFND  (7300-NOTFND)                                       CL**4
00931          NOTOPEN (7300-NOTOPEN)                                      CL**4
00932      END-EXEC.                                                    EL6521
00933                                                                   EL6521
00934      EXEC CICS READ                                               EL6521
00935           DATASET  (ERCOMP-FILE-ID)                                  CL**4
00936           SET      (ADDRESS OF COMPENSATION-MASTER)                  CL**3
00937           RIDFLD   (PI-ERCOMP-KEY)                                EL6521
00938           UPDATE                                                  EL6521
00939      END-EXEC.                                                    EL6521
00940      GO TO 7300-EXIT.                                                CL**4
00941                                                                      CL**4
00942  7300-NOTOPEN.                                                       CL**4
00943                                                                      CL**4
00944      MOVE ER-2233               TO EMI-ERROR                         CL**4
00945      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00946      GO TO 7300-EXIT.                                                CL**4
00947  7300-NOTFND.                                                        CL**4
00948                                                                      CL**4
00949      MOVE ER-7462               TO EMI-ERROR                         CL**4
00950      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00951      GO TO 7300-EXIT.                                                CL**4
00952                                                                   EL6521
00953  7300-EXIT.                                                       EL6521
00954      EXIT.                                                        EL6521
00955      EJECT                                                        EL6521
00956                                                                      CL**4
00957  7400-DELETE-PRE-NOTE.                                               CL**4
00958                                                                      CL**4
00959      EXEC CICS HANDLE CONDITION                                      CL**4
00960          NOTFND  (7499-EXIT)                                         CL**4
00961          NOTOPEN (7450-NOTOPEN)                                      CL**4
00962      END-EXEC.                                                       CL**4
00963                                                                      CL**4
00964      EXEC CICS READ                                                  CL**4
00965           DATASET  (ELACHP-FILE-ID)                                  CL**4
00966           SET     (ADDRESS OF ACH-PRENOTIFICATION)                   CL**4
00967           RIDFLD   (CO-CONTROL-PRIMARY)                              CL**4
00968           UPDATE                                                     CL**4
00969      END-EXEC.                                                       CL**4
00970                                                                      CL**4
00971      EXEC CICS DELETE                                                CL**4
00972          DATASET   (ELACHP-FILE-ID)                                  CL**4
00973      END-EXEC.                                                       CL**4
00974                                                                      CL**4
00975      GO TO 7499-EXIT.                                                CL**4
00976                                                                      CL**4
00977  7450-NOTOPEN.                                                       CL**4
00978      MOVE ER-7469               TO EMI-ERROR                         CL**4
00979      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00980                                                                      CL**4
00981  7499-EXIT.                                                          CL**4
00982      EXIT.                                                           CL**4
00983      EJECT                                                           CL**4
00984                                                                      CL**4
00985 ******************************************************************   CL**4
00986 *             V A L I D T E  T R A N S I T  N U M B E R              CL**4
00987 ******************************************************************   CL**4
00988  7500-VALIDATE-TRANSIT.                                              CL**4
00989                                                                      CL**4
00990      EXEC CICS HANDLE CONDITION                                      CL**4
00991          NOTFND   (7500-NOT-FOUND)                                   CL**4
00992          NOTOPEN  (7500-NOT-OPEN)                                    CL**4
00993      END-EXEC.                                                       CL**4
00994                                                                      CL**4
00995      MOVE WS-TRANSIT1          TO PI-FEDERAL-NUMBER.                 CL**4
00996      MOVE WS-TRANSIT2          TO PI-BANK-NUMBER.                    CL**4
00997      MOVE PI-COMPANY-CD        TO PI-BANK-COMPANY-CD.                CL**4
00998                                                                      CL**4
00999      EXEC CICS READ                                                  CL**4
01000          EQUAL                                                       CL**4
01001          DATASET   (ELBANK-FILE-ID)                                  CL**4
01002          SET       (ADDRESS OF BANK-MASTER)                          CL**4
01003          RIDFLD    (PI-BANK-TRANSIT-NUMBER)                          CL**4
01004      END-EXEC.                                                       CL**4
01005                                                                      CL**4
01006      GO TO 7500-VALIDATE-EXIT.                                       CL**4
01007                                                                      CL**4
01008  7500-NOT-OPEN.                                                      CL**4
01009                                                                      CL**4
01010      MOVE -1                    TO TRNSIT1L                          CL**4
01011      MOVE AL-UABON              TO TRNSIT1A                          CL**4
01012      MOVE AL-UABON              TO TRNSIT2A                          CL**4
01013      MOVE ER-7468               TO EMI-ERROR .                       CL**4
01014      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**4
01015      GO TO 7500-VALIDATE-EXIT.                                       CL**4
01016                                                                      CL**4
01017  7500-NOT-FOUND.                                                     CL**4
01018                                                                      CL**4
01019      MOVE -1                    TO TRNSIT1L                          CL**4
01020      MOVE AL-UABON              TO TRNSIT1A                          CL**4
01021      MOVE AL-UABON              TO TRNSIT2A                          CL**4
01022      MOVE ER-9388               TO EMI-ERROR .                       CL**4
01023      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**4
01024      GO TO 7500-VALIDATE-EXIT.                                       CL**4
01025                                                                      CL**4
01026  7500-VALIDATE-EXIT.                                                 CL**4
01027      EXIT.                                                           CL**4
01028      EJECT                                                           CL**4
01029                                                                      CL**4
01030  7600-WRITE-ELACHP.                                                  CL**4
01031      EXEC CICS HANDLE CONDITION                                      CL**4
01032          NOTOPEN (7450-NOTOPEN)                                      CL**4
01033          DUPREC   (7610-DUPREC)                                      CL**4
01034      END-EXEC.                                                       CL**4
01035                                                                      CL**4
01036      EXEC CICS GETMAIN                                               CL**4
01037          SET      (ADDRESS OF ACH-PRENOTIFICATION)                   CL**4
01038          LENGTH   (210)                                              CL**4
01039      END-EXEC.                                                       CL**4
01040                                                                      CL**4
01041      INITIALIZE  ACH-PRENOTIFICATION.                                CL**4
01042                                                                      CL**4
01043      MOVE  'AP'                  TO AP-RECORD-ID.                    CL**4
01044                                                                      CL**4
01045      MOVE PI-PROCESSOR-ID        TO AP-LAST-CHANGE-PROCESSOR         CL**4
01046      MOVE CO-CONTROL-PRIMARY     TO AP-CONTROL-PRIMARY               CL**4
01047      MOVE EIBTIME                TO AP-LAST-CHANGE-TIME              CL**4
01048      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                 CL**4
01049      MOVE '5'                    TO DC-OPTION-CODE.                  CL**4
01050      PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.                       CL**4
01051                                                                      CL**4
01052      MOVE DC-BIN-DATE-1          TO AP-LAST-CHANGE-DT.               CL**4
01053      MOVE WS-BANK-DATA           TO AP-BANK-INFORMATION.             CL**4
01054      MOVE BM-NAME                TO AP-BANK-NAME.                    CL**4
01055                                                                      CL**4
01056      EXEC CICS WRITE                                                 CL**4
01057          FROM      (ACH-PRENOTIFICATION)                             CL**4
01058          DATASET   (ELACHP-FILE-ID)                                  CL**4
01059          RIDFLD    (CO-CONTROL-PRIMARY)                              CL**4
01060      END-EXEC.                                                       CL**4
01061                                                                      CL**4
01062      GO TO 7620-WRITE-EXIT.                                          CL**4
01063                                                                      CL**4
01064  7610-DUPREC.                                                        CL**4
01065                                                                      CL**4
01066      MOVE    ER-7447            TO EMI-ERROR                         CL**4
01067      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01068      GO TO 7620-WRITE-EXIT.                                          CL**4
01069                                                                      CL**4
01070  7620-WRITE-EXIT.                                                    CL**4
01071       EXIT.                                                          CL**4
01072      EJECT                                                           CL**4
01073 ******************************************************************   CL**4
01074  8100-SEND-INITIAL-MAP.                                           EL6521
01075      MOVE WS-SAVE-DATE           TO DATEO.                        EL6521
01076      MOVE EIBTIME                TO TIME-IN.                      EL6521
01077      MOVE TIME-OUT               TO TIMEO.                        EL6521
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
01078      MOVE -1                     TO MAINTYPL.                        CL**4
01079      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6521
01080      MOVE WS-AGENT-BANK-DESC    TO BKDESCO.                          CL**4
01081                                                                   EL6521
01082 * HIDE ACH FIELDS IF NOT AR PROCESSING                               CL**4
01083      IF PI-AR-PROCESSING                                             CL**4
01084         NEXT SENTENCE                                                CL**4
01085      ELSE                                                            CL**4
01086         MOVE AL-SADOF             TO BKLITA, BKDESCA  CRSTATA        CL**4
01087                                      ACTLITA CRSLITA ACHPF1A         CL**4
01088                                      TRNSIT1A TRNSIT2A TRDASHA       CL**4
01089                                      BKACCTA, ACTCDA.                CL**4
01090      EXEC CICS SEND                                               EL6521
01091          MAP      (MAP-NAME)                                      EL6521
01092          MAPSET   (MAPSET-NAME)                                   EL6521
01093          FROM     (EL6521AO)                                      EL6521
01094          ERASE                                                    EL6521
01095          CURSOR                                                   EL6521
01096      END-EXEC.                                                    EL6521
01097                                                                   EL6521
01098      GO TO 9100-RETURN-TRAN.                                      EL6521
01099                                                                   EL6521
01100  EJECT                                                            EL6521
01101  8200-SEND-DATAONLY.                                              EL6521
01102      MOVE WS-SAVE-DATE           TO DATEO.                        EL6521
01103      MOVE EIBTIME                TO TIME-IN.                      EL6521
01104      MOVE TIME-OUT               TO TIMEO.                        EL6521
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
01105      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL6521
01106      MOVE WS-AGENT-BANK-DESC     TO BKDESCO.                         CL**4
01107                                                                   EL6521
01108 * HIDE ACH FIELDS IF NOT AR PROCESSING                               CL**4
01109      IF PI-AR-PROCESSING                                             CL**4
01110         NEXT SENTENCE                                                CL**4
01111      ELSE                                                            CL**4
01112         MOVE AL-SADOF             TO BKLITA, BKDESCA  CRSTATA        CL**4
01113                                      ACTLITA CRSLITA ACHPF1A         CL**4
01114                                      TRNSIT1A TRNSIT2A TRDASHA       CL**4
01115                                      BKACCTA, ACTCDA.                CL**4
01116      EXEC CICS SEND                                               EL6521
01117          MAP      (MAP-NAME)                                      EL6521
01118          MAPSET   (MAPSET-NAME)                                   EL6521
01119          FROM     (EL6521AO)                                      EL6521
01120          DATAONLY                                                 EL6521
01121          CURSOR                                                   EL6521
01122      END-EXEC.                                                    EL6521
01123                                                                   EL6521
01124      GO TO 9100-RETURN-TRAN.                                      EL6521
01125                                                                   EL6521
01126  EJECT                                                            EL6521
01127  8300-SEND-TEXT.                                                  EL6521
01128      EXEC CICS SEND TEXT                                          EL6521
01129          FROM     (LOGOFF-TEXT)                                   EL6521
01130          LENGTH   (LOGOFF-LENGTH)                                 EL6521
01131          ERASE                                                    EL6521
01132          FREEKB                                                   EL6521
01133      END-EXEC.                                                    EL6521
01134                                                                   EL6521
01135      EXEC CICS RETURN                                             EL6521
01136      END-EXEC.                                                    EL6521
01137                                                                   EL6521
01138  EJECT                                                            EL6521
01139  8600-DEEDIT.                                                     EL6521
01140      EXEC CICS BIF DEEDIT                                         EL6521
01141           FIELD(DEEDIT-FIELD)                                     EL6521
01142           LENGTH(15)                                              EL6521
01143      END-EXEC.                                                    EL6521
01144                                                                   EL6521
01145  EJECT                                                            EL6521
01146  8800-UNAUTHORIZED-ACCESS.                                        EL6521
01147      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6521
01148      GO TO 8300-SEND-TEXT.                                        EL6521
01149                                                                   EL6521
01150  8810-PF23.                                                       EL6521
01151      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6521
01152      MOVE XCTL-005               TO PGM-NAME.                     EL6521
01153      GO TO 9300-XCTL.                                             EL6521
01154  EJECT                                                            EL6521
01155                                                                   EL6521
01156  9100-RETURN-TRAN.                                                EL6521
01157      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL6521
01158      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL6521
01159      EXEC CICS RETURN                                             EL6521
01160          TRANSID    (TRANS-ID)                                    EL6521
01161          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6521
01162          LENGTH     (WS-COMM-LENGTH)                              EL6521
01163      END-EXEC.                                                    EL6521
01164                                                                   EL6521
01165  9200-RETURN-MAIN-MENU.                                           EL6521
01166      MOVE XCTL-626               TO PGM-NAME.                     EL6521
01167      GO TO 9300-XCTL.                                             EL6521
01168                                                                   EL6521
01169  EJECT                                                            EL6521
01170  9300-XCTL.                                                       EL6521
01171      EXEC CICS XCTL                                               EL6521
01172          PROGRAM    (PGM-NAME)                                    EL6521
01173          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6521
01174          LENGTH     (WS-COMM-LENGTH)                              EL6521
01175      END-EXEC.                                                    EL6521
01176                                                                   EL6521
01177  9400-CLEAR.                                                      EL6521
01178      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL6521
01179      GO TO 9300-XCTL.                                             EL6521
01180                                                                   EL6521
01181  9500-PF12.                                                       EL6521
01182      MOVE XCTL-010               TO PGM-NAME.                     EL6521
01183      GO TO 9300-XCTL.                                             EL6521
01184                                                                   EL6521
01185  9600-PGMID-ERROR.                                                EL6521
01186      EXEC CICS HANDLE CONDITION                                   EL6521
01187          PGMIDERR    (8300-SEND-TEXT)                             EL6521
01188      END-EXEC.                                                    EL6521
01189                                                                   EL6521
01190      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6521
01191      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6521
01192      MOVE XCTL-005               TO PGM-NAME.                     EL6521
01193      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6521
01194      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6521
01195      GO TO 9300-XCTL.                                             EL6521
01196                                                                   EL6521
01197  9700-DATE-CONVERT.                                               EL6521
01198      EXEC CICS LINK                                               EL6521
01199          PROGRAM    ('ELDATCV')                                   EL6521
01200          COMMAREA   (DATE-CONVERSION-DATA)                        EL6521
01201          LENGTH     (DC-COMM-LENGTH)                              EL6521
01202      END-EXEC.                                                    EL6521
01203                                                                   EL6521
01204  9700-EXIT.                                                       EL6521
01205      EXIT.                                                        EL6521
01206                                                                   EL6521
01207  EJECT                                                            EL6521
01208  9900-ERROR-FORMAT.                                               EL6521
01209                                                                      CL**4
01210      IF NOT EMI-ERRORS-COMPLETE                                   EL6521
01211          MOVE LINK-001           TO PGM-NAME                      EL6521
01212          EXEC CICS LINK                                           EL6521
01213              PROGRAM    (PGM-NAME)                                EL6521
01214              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6521
01215              LENGTH     (EMI-COMM-LENGTH)                         EL6521
01216          END-EXEC.                                                EL6521
01217                                                                   EL6521
01218  9900-EXIT.                                                       EL6521
01219      EXIT.                                                        EL6521
01220                                                                   EL6521
01221  9990-ABEND.                                                      EL6521
01222      MOVE LINK-004               TO PGM-NAME.                     EL6521
01223      MOVE DFHEIBLK               TO EMI-LINE1.                    EL6521
01224      EXEC CICS LINK                                               EL6521
01225          PROGRAM   (PGM-NAME)                                     EL6521
01226          COMMAREA  (EMI-LINE1)                                    EL6521
01227          LENGTH    (72)                                           EL6521
01228      END-EXEC.                                                    EL6521
01229                                                                   EL6521
01230      GO TO 8200-SEND-DATAONLY.                                    EL6521
01231                                                                   EL6521
01232  9995-SECURITY-VIOLATION.                                         EL6521
01233             COPY ELCSCTP.                                         EL6521
01234                                                                   EL6521
01235  9995-EXIT.                                                       EL6521
01236       EXIT.                                                       EL6521
