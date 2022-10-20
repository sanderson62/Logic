       IDENTIFICATION DIVISION.                                         03/06/96
                                                                        EL6523
       PROGRAM-ID.                 EL6523.
      *              PROGRAM CONVERTED BY                                  CL**5
      *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
      *              CONVERSION DATE 02/14/96 11:58:19.                    CL**5
      *                            VMOD=2.005                              CL**5
      *                                                                 EL6523
      *AUTHOR.     LOGIC,INC.                                              CL**5
      *            DALLAS, TEXAS.                                          CL**5
                                                                        EL6523
      *DATE-COMPILED.                                                      CL**5
      *SECURITY.   *****************************************************   CL**5
      *            *                                                   *   CL**5
      *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
      *            *                                                   *   CL**5
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
      *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
      *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
      *            *                                                   *   CL**5
      *            *****************************************************   CL**5
                                                                        EL6523
      *REMARKS.    TRANSACTION - EXDD - DISPLAY BANK                       CL**5
      *                                 CROSS REFERENCE RECORDS.           CL**5
                                                                        EL6523
                                                                        EL6523
       ENVIRONMENT DIVISION.                                            EL6523
           EJECT                                                        EL6523
       DATA DIVISION.                                                   EL6523
       WORKING-STORAGE SECTION.                                         EL6523
       77  FILLER  PIC X(32)  VALUE '********************************'. EL6523
       77  FILLER  PIC X(32)  VALUE '*    EL6523 WORKING STORAGE    *'. EL6523
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.    CL**5
                                                                        EL6523
       77  NDX     PIC S9(5)  COMP-3 VALUE +1.
       77  WSUB1   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB2   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB3   PIC S9(5)  COMP-3 VALUE +0.
                                                                        EL6523

       01  WS-WORK-ACCT-TABLE.                                          ERCBXRF
           12  WS-BANKS OCCURS 725.
               16  WS-AM-ACCOUNT       PIC X(10).
               16  WS-AM-EXP-DT        PIC XX.
               16  WS-AM-EFF-DT        PIC XX.
               16  WS-AM-STATE         PIC XX.
               16  FILLER              PIC X(20).

       01  WS-DATE-AREA.                                                EL6523
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6523
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL6523
                                                                        EL6523
       01  WS-CONTROL-PRIMARY.                                          EL6523
           05  WS-COMPANY-CD       PIC X.                               EL6523
           05  WS-CARRIER          PIC X.                               EL6523
           05  WS-GROUPING         PIC X(6).                            EL6523
           05  WS-BANK             PIC X(10).                           EL6523
                                                                        EL6523
           05  WS-OPEN-COUNT       PIC S9(4)    COMP-3 VALUE ZEROS.        CL**5
                                                                        EL6523
                                   COPY ELCATTR SUPPRESS.                  CL**5
                                                                           CL**3
           EJECT                                                        EL6523
                                                                        EL6523
       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0033                 PIC X(4)  VALUE '0033'.
           12  ER-0034                 PIC X(4)  VALUE '0034'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0070                 PIC X(4)  VALUE '0070'.
           12  ER-0130                 PIC X(4)  VALUE '0130'.
           12  ER-0131                 PIC X(4)  VALUE '0131'.
           12  ER-0142                 PIC X(4)  VALUE '0142'.
           12  ER-0193                 PIC X(4)  VALUE '0193'.
           12  ER-0226                 PIC X(4)  VALUE '0226'.
           12  ER-0234                 PIC X(4)  VALUE '0234'.
           12  ER-0235                 PIC X(4)  VALUE '0235'.
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0454                 PIC X(4)  VALUE '0454'.
           12  ER-1162                 PIC X(4)  VALUE '1162'.
           12  ER-1164                 PIC X(4)  VALUE '1164'.
           12  ER-1299                 PIC X(4)  VALUE '1299'.
           12  ER-2039                 PIC X(4)  VALUE '2039'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2057                 PIC X(4)  VALUE '2057'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2339                 PIC X(4)  VALUE '2339'.
           12  ER-2370                 PIC X(4)  VALUE '2370'.
           12  ER-2947                 PIC X(4)  VALUE '2947'.
           12  ER-3112                 PIC X(4)  VALUE '3112'.
           12  ER-5004                 PIC X(4)  VALUE '5004'.
           12  ER-5005                 PIC X(4)  VALUE '5005'.
           12  ER-6508                 PIC X(4)  VALUE '6508'.

                                 COPY ELCSCTM.
                                 COPY ELCSCRTY.

       01  STANDARD-AREAS.
           12  WS-ERACCT-KEY.
               16  WS-ERACCT-COMP-CD   PIC X.
               16  WS-ERACCT-CARRIER   PIC X.
               16  WS-ERACCT-GROUPING  PIC X(6).
               16  WS-ERACCT-STATE     PIC XX.
               16  WS-ERACCT-ACCOUNT   PIC X(10).
               16  WS-ERACCT-EXP-DT    PIC XX.
               16  FILLER              PIC X(4).
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  WS-CHANGE-SW            PIC X  VALUE ' '.
               88  CHANGES-MADE               VALUE 'Y'.
               88  NO-CHANGES-MADE            VALUE 'N'.
           12  WS-LINE-CHANGE-SW       PIC X  VALUE ' '.
               88  LINE-CHANGES               VALUE 'Y'.
               88  NO-LINE-CHANGES            VALUE 'N'.
           12  GETMAIN-SPACE           PIC X     VALUE SPACE.
           12  WS-DEEDIT-FIELD         PIC X(10) VALUE SPACES.
           12  WS-DT-DEEDIT-FIELD REDEFINES WS-DEEDIT-FIELD
                                            PIC X(10).
           12  WS-DEEDIT-FIELD-DATE REDEFINES WS-DT-DEEDIT-FIELD.
               16  FILLER                   PIC X(4).
               16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).
           12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.   
           12  WS-ACCESS.                                               EL652
               16  FILLER          PIC  X(3)         VALUE SPACES.      EL652
               16  WS-ACARRIER     PIC  X.                              EL652
           12  ELCNTL-KEY.                                              EL652
               16  CNTL-COMP-ID    PIC  X(3)         VALUE SPACES.      EL652
               16  CNTL-REC-TYPE   PIC  X            VALUE SPACES.      EL652
               16  CNTL-ACCESS     PIC  X(4)         VALUE SPACES.      EL652
               16  CNTL-SEQ-NO     PIC S9(4)         VALUE +0   COMP.   EL652
           12  QID.                                                     EL6523
               16  QID-TERM        PIC X(4).                            EL6523
               16  FILLER          PIC X(4)    VALUE '652D'.            EL6523
           12  RETURNED-FROM       PIC X(8)    VALUE SPACES.            EL6523
           12  QID-MAP-LENGTH      PIC S9(4)   VALUE +1376   COMP.      EL6523
           12  MAP-NAME                PIC X(8)    VALUE 'EL652D'.      EL6523
           12  MAPSET-NAME             PIC X(8)    VALUE 'EL6523S'.     EL6523
           12  TRANS-ID                PIC X(4)    VALUE 'EXDD'.        EL6523
           12  EL652-TRANS-ID          PIC X(4)    VALUE 'EXD4'.
           12  EL650-TRANS-ID          PIC X(4)    VALUE 'EXC4'.        EL6523
           12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL6523
           12  THIS-PGM                PIC X(8)    VALUE 'EL6523'.      EL6523
           12  XCTL-650                PIC X(8)    VALUE 'EL650'.       EL6523
           12  BXRF-FILE-ID            PIC X(8)    VALUE 'ERBXRF'.      EL6523
           12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.      EL6523
           12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL6523
           12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL6523
           12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL6523
           12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL6523
           12  XCTL-010                PIC X(8)    VALUE 'EL010'.          CL**2
           12  XCTL-626                PIC X(8)    VALUE 'EL626'.       EL6523
           12  TIME-IN                 PIC S9(7).                       EL6523
           12  TIME-OUT-R   REDEFINES TIME-IN.                          EL6523
               16  FILLER              PIC X.                           EL6523
               16  TIME-OUT            PIC 99V99.                       EL6523
               16  FILLER              PIC X(2).                        EL6523
           12  BROWSE-STARTED-SW       PIC X       VALUE ' '.           EL6523
               88  BROWSE-STARTED      VALUE 'Y'.                       EL6523
                                                                        EL6523
       01  WS-RECORD-LENGTH COMP       SYNCHRONIZED.                    EL6523
           12  BXRF-MAX-REC-LENGTH     PIC S9(8)    VALUE +26178.       EL6523
           12  BXRF-REC-LENGTH         PIC S9(4)    VALUE +0.           EL6523
                                                                        EL6523
                                       COPY ELCDATE.
                                       COPY ELCEMIB SUPPRESS.
                                                                        EL6523
                         COPY ELCLOGOF SUPPRESS.                           CL**5
                                                                           CL**3
                                       COPY ELCINTF.
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL6523
               16  PI-ERBXRF-KEY.                                       EL6523
                   20  PI-BXRF-COMP-CD     PIC X.                       EL6523
                   20  PI-BXRF-CARRIER     PIC X.                       EL6523
                   20  PI-BXRF-GROUPING    PIC X(6).                    EL6523
                   20  PI-BXRF-BANK        PIC X(10).
               16  PI-TOP-NDX              PIC S9(5)  COMP-3.
               16  PI-NDX                  PIC S9(5)  COMP-3.           EL6523
               16  PI-EOF-SW               PIC X.                       EL6523
                   88  PI-FILE-EOF             VALUE 'Y'.               EL6523
               16  PI-CHECK-MAINT-TYPE     PIC  X.                      EL652
                   88  VALID-MAINT-TYPE            VALUE 'S' 'A'        EL652
                                                         'C' 'D'.       EL652
                   88  ADD-FUNCTION                VALUE 'A'.           EL652
                   88  SHOW-FUNCTION               VALUE 'S'.           EL652
                   88  DELETE-FUNCTION             VALUE 'D'.           EL652
                   88  CHANGE-FUNCTION             VALUE 'C'.           EL652
               16  PI-ERC-KEY.
                   20  PI-ERC-COMPANY-CD   PIC  X.                         CL*50
                   20  PI-ERC-CARRIER      PIC  X.                      EL652
                   20  PI-ERC-GROUP        PIC  X(6).                      CL**6
                   20  PI-ERC-BANK         PIC  X(10).                  EL652
               16  PI-SAVE-ERBXRF-KEY      PIC  X(18).                  EL652
               16  PI-EL6523-DEL-SW        PIC  X.
               16  FILLER                  PIC X(616).
           EJECT                                                        EL6523
                                   COPY ELCAID SUPPRESS.                   CL**5
       01  FILLER    REDEFINES DFHAID.                                  EL6523
           12  FILLER              PIC X(8).                            EL6523
           12  PF-VALUES           PIC X       OCCURS 24.               EL6523
                                                                        EL6523
                                   COPY EL6523S.                            CL**3
       01  FILLER REDEFINES EL652DI.
           12  FILLER                  PIC X(88).
           12  BLD-LINE OCCURS 13.
               16  BL-ACCOUNTL         PIC S9(4) COMP.
               16  BL-ACCOUNTA         PIC X.
               16  BL-ACCOUNT          PIC X(10).
               16  BL-EFFECTL          PIC S9(4) COMP.
               16  BL-EFFECTA          PIC X.
               16  BL-EFFECT           PIC X(8).
               16  BL-EXPIREL          PIC S9(4) COMP.
               16  BL-EXPIREA          PIC X.
               16  BL-EXPIRE           PIC X(8).
               16  BL-STATEL           PIC S9(4) COMP.
               16  BL-STATEA           PIC X.
               16  BL-STATE            PIC XX.
                                                                        EL6523
           EJECT                                                        EL6523
                                                                        EL6523
       LINKAGE SECTION.                                                 EL6523
       01  DFHCOMMAREA                 PIC X(1024).

                                       COPY ELCCNTL.
                                       COPY ERCBXRF.
                                       COPY ERCACCT.

       PROCEDURE DIVISION.                                              EL6523
           MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL6523
                                                                        EL6523
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6523
           MOVE '5'                   TO DC-OPTION-CODE.                EL6523
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL6523
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL6523
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL6523
                                                                           CL**5
           MOVE EIBTRMID              TO QID-TERM.                         CL**5
                                                                        EL6523
           EXEC CICS HANDLE CONDITION                                   EL6523
               ERROR     (9990-ABEND)                                   EL6523
               MAPFAIL   (8100-SEND-INITIAL-MAP)                        EL6523
           END-EXEC.
                                                                        EL6523
           IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6523
               IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6523
      *  THIS IS THE FIRST TIME THRU
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6523
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6523
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6523
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6523
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6523
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6523
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6523
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6523
                   PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
               ELSE                                                     EL6523
      * THIS IS WHEN I COME BACK FROM WHERE I XCTL'D TO
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6523
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6523
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6523
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6523
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6523
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6523
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6523
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6
               END-IF
      * ALL SUBSEQUENT TIMES
           END-IF

           IF EIBTRNID = TRANS-ID                                       EL6523
               IF EIBAID = DFHCLEAR                                     EL6523
                   GO TO 9400-CLEAR                                     EL6523
               ELSE                                                     EL6523
                   GO TO 0200-RECEIVE-MAP.                              EL6523
                                                                        EL6523
           IF EIBTRNID  = EL652-TRANS-ID
      * THIS IS WHEN I CAME FROM COMP MAINT
               MOVE DFHENTER       TO EIBAID                            EL6523
               MOVE PI-CR-CARRIER  TO CARRIERI                          EL6523
               MOVE PI-CR-GROUPING TO GROUPI                            EL6523
               MOVE PI-CR-FIN-RESP TO BANKI                             EL6523
               MOVE 'S'            TO MAINTYPI
               MOVE 1              TO CARRIERL                          EL6523
               MOVE 6              TO GROUPL                            EL6523
               MOVE 10             TO BANKL                             EL6523
               MOVE 1              TO MAINTYPL
               MOVE AL-UANON       TO CARRIERA GROUPA BANKA MAINTYPA
               GO TO 4000-EDIT-MAINT
           END-IF
                                                                        EL6523
           MOVE LOW-VALUES       TO PI-ERBXRF-KEY    EL652DI.           EL6523
           MOVE PI-COMPANY-CD    TO PI-BXRF-COMP-CD  WS-COMPANY-CD.     EL6523
                                                                        EL6523
           IF EIBTRNID  = EL650-TRANS-ID                                EL6523
      * THIS IS WHEN I CAME FROM ACCOUNT MAINT
               GO TO 0600-RECOVER-TEMP-STORAGE.                         EL6523
                                                                        EL6523
           GO TO 8100-SEND-INITIAL-MAP.                                 EL6523
                                                                        EL6523
           EJECT                                                        EL6523
                                                                        EL6523
       0200-RECEIVE-MAP.

           MOVE LOW-VALUES             TO EL652DI

           EXEC CICS RECEIVE
               MAP (MAP-NAME)
               MAPSET (MAPSET-NAME)
               INTO (EL652DI)
           END-EXEC

           IF PFENTERL = 0
              GO TO 0300-CHECK-PFKEYS
           END-IF

           IF EIBAID NOT = DFHENTER                                     EL6523
              MOVE ER-0004             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF

           IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)            EL6523
              MOVE PF-VALUES (PFENTERI)
                                       TO EIBAID
           ELSE                                                         EL6523
              MOVE ER-0029             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
           .                                                             EL6523
       0300-CHECK-PFKEYS.                                               EL6523

           IF EIBAID = DFHPF23                                          EL6523
               GO TO 8810-PF23.                                         EL6523
                                                                        EL6523
           IF EIBAID = DFHPF24                                          EL6523
               GO TO 9200-RETURN-MAIN-MENU.                             EL6523

           IF EIBAID = DFHPF12                                             CL**2
               GO TO 9500-PF12.                                            CL**2
                                                                           CL**2
           IF (MAINTYPL > ZERO)
              AND (MAINTYPI NOT = 'D')
              MOVE SPACES              TO PI-EL6523-DEL-SW
           END-IF

           IF EIBAID = DFHPF1 OR DFHPF2
              GO TO 5000-BROWSE-FILE
           END-IF
                                                                        EL6523
           IF EIBAID = DFHPF3 OR DFHPF4                                 EL6523
               GO TO 6000-OCCURRENCE.                                   EL6523
                                                                        EL6523
           IF EIBAID = DFHPF5                                           EL6523
               GO TO 4000-ACCT-MAINT.                                   EL6523
                                                                        EL6523
           IF EIBAID = DFHENTER                                         EL6523
              GO TO 4000-EDIT-MAINT
           END-IF
                                                                        EL6523
           MOVE ER-0029                TO EMI-ERROR
           .
       0320-INPUT-ERROR.                                                EL6523
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6523
           MOVE -1                     TO CARRIERL.                     EL6523
           GO TO 8200-SEND-DATAONLY.                                    EL6523
           EJECT                                                        EL6523
       0500-CREATE-TEMP-STORAGE.                                        EL6523
                                                                        EL6523
           EXEC CICS WRITEQ TS                                          EL6523
               QUEUE   (QID)                                            EL6523
               FROM    (EL652DI)                                        EL6523
               LENGTH  (QID-MAP-LENGTH)                                 EL6523
               END-EXEC.                                                EL6523
                                                                        EL6523
       0599-EXIT.                                                       EL6523
            EXIT.                                                       EL6523
                                                                        EL6523
       0600-RECOVER-TEMP-STORAGE.                                       EL6523
                                                                        EL6523
           EXEC CICS HANDLE CONDITION                                   EL6523
               NOTFND  (1500-BXRF-NOT-FOUND)                            EL6523
               QIDERR  (0690-QIDERR)                                    EL6523
               END-EXEC.                                                EL6523
                                                                        EL6523
           EXEC CICS READQ TS                                           EL6523
               QUEUE    (QID)                                           EL6523
               INTO     (EL652DI)                                       EL6523
               LENGTH   (QID-MAP-LENGTH)                                EL6523
               END-EXEC.                                                EL6523
                                                                        EL6523
           EXEC CICS DELETEQ TS                                         EL6523
               QUEUE   (QID)                                            EL6523
           END-EXEC
                                                                        EL6523
           IF CARRIERL NOT = 0                                          EL6523
               MOVE AL-UANON TO CARRIERA.                               EL6523
                                                                        EL6523
           IF GROUPL NOT = 0                                            EL6523
               MOVE AL-UANON TO GROUPA.                                 EL6523
                                                                        EL6523
           IF BANKL NOT = 0
               MOVE AL-UANON TO BANKA.
                                                                        EL6523
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD.                EL6523
           MOVE CARRIERO               TO WS-CARRIER.                   EL6523
           MOVE GROUPO                 TO WS-GROUPING.                  EL6523
           MOVE BANKO                  TO WS-BANK.
                                                                        EL6523
           GO TO 1050-READ-BANK.                                        EL6523

       0690-QIDERR.

           MOVE ER-0033                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 8100-SEND-INITIAL-MAP

           .
       4000-EDIT-MAINT.

           IF MAINTYPL > ZERO
              MOVE MAINTYPI            TO PI-CHECK-MAINT-TYPE
              IF VALID-MAINT-TYPE
                 MOVE AL-UANON         TO MAINTYPA
              ELSE
                 MOVE -1               TO MAINTYPL
                 MOVE AL-UABON         TO MAINTYPA
                 MOVE ER-2039          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              END-IF
           ELSE
              MOVE -1                  TO MAINTYPL
              MOVE AL-UABON            TO MAINTYPA
              MOVE ER-2039             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           MOVE PI-COMPANY-CD          TO PI-ERC-COMPANY-CD
           IF (NOT MODIFY-CAP)
              AND (NOT SHOW-FUNCTION)
              MOVE 'UPDATE'            TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
              MOVE ER-0070             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           IF CARRIERL > ZERO
              IF PI-CARRIER-SECURITY > SPACES
                 IF CARRIERI = PI-CARRIER-SECURITY
                    CONTINUE
                 ELSE
                    MOVE -1            TO CARRIERL
                    MOVE ER-2370       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    MOVE AL-UABON      TO CARRIERA
                    GO TO 8200-SEND-DATAONLY
                 END-IF
              END-IF
           END-IF
                                                                        EL652
           IF CARRIERL > ZERO                                           EL652
               IF ADD-FUNCTION                                          EL652
                   IF PI-ZERO-CARRIER                                   EL652
                     OR PI-ZERO-CAR-GROUP                               EL652
                       MOVE ZEROS      TO  PI-ERC-CARRIER               EL652
                                           CARRIERI                     EL652
                       MOVE AL-UANON   TO  CARRIERA                     EL652
                   ELSE                                                 EL652
                       MOVE CARRIERI   TO  WS-ACARRIER
                                           PI-ERC-CARRIER
                       MOVE '6'        TO  CNTL-REC-TYPE                EL652
                       PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT  EL652
               ELSE                                                     EL652
                   IF PI-ZERO-CARRIER                                   EL652
                     OR PI-ZERO-CAR-GROUP                               EL652
                       MOVE ZEROS      TO  PI-ERC-CARRIER               EL652
                                           CARRIERI                     EL652
                       MOVE AL-UANON   TO  CARRIERA                     EL652
                   ELSE                                                 EL652
                       MOVE AL-UANON   TO  CARRIERA                     EL652
                       MOVE CARRIERI   TO  PI-ERC-CARRIER               EL652
           ELSE                                                         EL652
               IF ADD-FUNCTION                                          EL652
                   IF PI-ZERO-CARRIER                                   EL652
                     OR PI-ZERO-CAR-GROUP                               EL652
                       MOVE ZEROS      TO  PI-ERC-CARRIER               EL652
                                           CARRIERI                     EL652
                       MOVE AL-UANON   TO  CARRIERA                     EL652
                   ELSE                                                 EL652
                       MOVE -1         TO  CARRIERL                     EL652
                       MOVE AL-UABON   TO  CARRIERA                     EL652
                       MOVE ER-0193    TO  EMI-ERROR                    EL652
                       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL652
               ELSE                                                     EL652
                   MOVE -1             TO  CARRIERL                     EL652
                   MOVE AL-UABON       TO  CARRIERA                     EL652
                   MOVE ER-0193        TO  EMI-ERROR                    EL652
                   PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
                                                                        EL652
           IF GROUPL > ZERO                                             EL652
               IF PI-ZERO-GROUPING                                      EL652
                 OR PI-ZERO-CAR-GROUP                                   EL652
                   MOVE ZEROS          TO  PI-ERC-GROUP                    CL**6
                                           GROUPI                       EL652
                   MOVE AL-UANON       TO  GROUPA                       EL652
               ELSE                                                     EL652
                   MOVE AL-UANON       TO  GROUPA                       EL652
                   MOVE GROUPI         TO  PI-ERC-GROUP                    CL**6
           ELSE                                                         EL652
               IF ADD-FUNCTION                                          EL652
                   IF PI-ZERO-GROUPING                                  EL652
                     OR PI-ZERO-CAR-GROUP                               EL652
                       MOVE ZEROS      TO  PI-ERC-GROUP                    CL**6
                                           GROUPI                       EL652
                       MOVE AL-UANON   TO  GROUPA                       EL652
                   ELSE                                                 EL652
                       MOVE LOW-VALUES  TO  PI-ERC-GROUP                   CL**6
               ELSE                                                     EL652
                   MOVE LOW-VALUES     TO  PI-ERC-GROUP.                   CL**6
                                                                        EL652
           IF BANKL > ZERO
               MOVE AL-UANON           TO BANKA
               MOVE BANKI              TO PI-ERC-BANK
           ELSE                                                         EL652
               MOVE LOW-VALUES         TO PI-ERC-BANK
           END-IF
                                                                        EL652
           IF NOT MODIFY-CAP                                            EL652
               IF SHOW-FUNCTION                                         EL652
                   GO TO 1000-EDIT-INPUT
               ELSE                                                     EL652
                   MOVE 'UPDATE'       TO SM-READ                       EL652
                   PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL652
                   MOVE ER-0070        TO  EMI-ERROR                    EL652
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL652
                   GO TO 8100-SEND-INITIAL-MAP.                         EL652
                                                                        EL652
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              IF EIBTRNID NOT = TRANS-ID
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF 
                                                                        EL652
           IF CHANGE-FUNCTION                                           EL652
              GO TO 4400-CHANGE
           END-IF
                                                                        EL652
           IF DELETE-FUNCTION                                           EL652
              GO TO 4600-DELETE
           END-IF
                                                                        EL652
           IF SHOW-FUNCTION                                             EL652
              GO TO 1000-EDIT-INPUT
           END-IF
                                                                        EL652
           IF EMI-NO-ERRORS                                             EL652
              CONTINUE
           ELSE                                                         EL652
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        EL652
           IF ADD-FUNCTION                                              EL652
              GO TO 4200-ADD
           END-IF
                                                                        EL652
           MOVE -1                     TO MAINTYPL
           MOVE ER-2056                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
                                                                        EL652
           GO TO 8200-SEND-DATAONLY
           .
                                                                        EL652
       4000-EXIT.                                                       EL652
           EXIT.                                                        EL652
       EJECT                                                            EL652
       1000-EDIT-INPUT.                                                 EL6523

           IF CARRIERL NOT = ZEROS
              MOVE CARRIERI            TO WS-CARRIER
           ELSE
              MOVE ER-0234             TO EMI-ERROR
              MOVE -1                  TO CARRIERL
              MOVE AL-UABON            TO CARRIERA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF GROUPL NOT = ZEROS
              MOVE GROUPI              TO WS-GROUPING
           ELSE
              MOVE ER-0235             TO EMI-ERROR
              MOVE -1                  TO GROUPL
              MOVE AL-UABON            TO GROUPA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF BANKL  NOT = ZEROS
              MOVE BANKI               TO WS-BANK
           ELSE
              MOVE ER-6508             TO EMI-ERROR
              MOVE -1                  TO BANKL
              MOVE AL-UABON            TO BANKA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           .
       1050-READ-BANK.

           EXEC CICS HANDLE CONDITION
                NOTFND (1500-BXRF-NOT-FOUND)
           END-EXEC

           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT

           MOVE BK-LAST-MAINT-USER     TO PI-UPDATE-BY

           IF BK-LAST-MAINT-HHMMSS NUMERIC
              MOVE BK-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           
           MOVE WS-CONTROL-PRIMARY     TO PI-SAVE-ERBXRF-KEY
           MOVE +1                     TO PI-NDX
                                          PI-TOP-NDX
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           GO TO 5200-FORMAT-SCREEN
           .
                                                                        EL6523
       1500-BXRF-NOT-FOUND.                                             EL6523

           MOVE ER-0142                TO EMI-ERROR
           MOVE -1                     TO CARRIERL
           MOVE AL-UABON               TO CARRIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           IF EIBTRNID  = EL652-TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
                                                                        EL6523
       3000-BLD-LINE.                                                   EL6523

           MOVE 1                      TO NDX
           .
       3000-BLD-LINE-LOOP.                                              EL6523

           IF PI-NDX > BK-BANK-POINTER-CNT
              GO TO 3000-XIT
           END-IF
                                                                        EL6523
           MOVE BK-AM-ACCOUNT (PI-NDX) TO BL-ACCOUNT (NDX)
                                                                        EL6523
           IF BK-AM-EFF-DT (PI-NDX) NOT = SPACES AND LOW-VALUES         EL6523
              MOVE BK-AM-EFF-DT (PI-NDX)
                                       TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO BL-EFFECT (NDX)
           END-IF
                                                                        EL6523
           IF BK-AM-EXP-DT (PI-NDX) NOT = SPACES AND LOW-VALUES
              AND HIGH-VALUES
              MOVE BK-AM-EXP-DT (PI-NDX)
                                       TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO BL-EXPIRE (NDX)
           END-IF
                                                                        EL6523
           IF BK-AM-EXP-DT (PI-NDX) = HIGH-VALUES
              MOVE '99/99/99'          TO BL-EXPIRE (NDX)
            END-IF
                                                                        EL6523
      *    MOVE BK-AM-LEVEL-NO (PI-NDX)       TO BL-LEVEL.              EL6523
                                                                        EL6523
                                                                        EL6523
           MOVE BK-AM-STATE (PI-NDX)   TO BL-STATE (NDX)
                                                                        EL6523
           IF (PI-NDX = BK-BANK-POINTER-CNT)  OR  (NDX = 13)
               GO TO 3000-XIT
           END-IF

           ADD 1                       TO PI-NDX NDX
           GO TO 3000-BLD-LINE-LOOP
           .
                                                                        EL6523
       3000-XIT.                                                        EL6523
           EXIT.                                                        EL6523
           EJECT                                                        EL6523
       4000-ACCT-MAINT.                                                 EL6523

           IF (CARRIERI    NOT = PI-BXRF-CARRIER)
              OR (GROUPI   NOT = PI-BXRF-GROUPING)
              OR (BANKI    NOT = PI-BXRF-BANK)
              MOVE ER-5005             TO EMI-ERROR
              GO TO 4050-ERR
           END-IF
                                                                        EL6523
           IF ACCOCCI NOT NUMERIC                                       EL6523
              GO TO 4010-ERROR
           END-IF
                                                                        EL6523
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE CARRIERI               TO WS-CARRIER
           MOVE GROUPI                 TO WS-GROUPING
           MOVE BANKI                  TO WS-BANK
                                                                        EL6523
           EXEC CICS HANDLE CONDITION
                NOTFND (4010-ERROR)
           END-EXEC

           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT

           MOVE ACCOCCI                TO NDX

           COMPUTE NDX = PI-TOP-NDX + NDX - 1
           IF NDX  NOT > BK-BANK-POINTER-CNT
              MOVE BK-CARRIER          TO PI-CR-CARRIER
              MOVE BK-GROUPING         TO PI-CR-GROUPING
              MOVE BK-AM-STATE (NDX)   TO PI-CR-STATE
              MOVE BK-AM-ACCOUNT (NDX) TO PI-CR-ACCOUNT
              PERFORM 0500-CREATE-TEMP-STORAGE
                                       THRU 0599-EXIT
              MOVE XCTL-650            TO PGM-NAME
              GO TO 9300-XCTL
           END-IF

           .
       4010-ERROR.                                                      EL6523
            MOVE ER-5004 TO EMI-ERROR.                                  EL6523
       4050-ERR.                                                        EL6523
            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   EL6523
            MOVE -1                     TO ACCOCCL.                     EL6523
            MOVE AL-UNBON               TO ACCOCCA.                     EL6523
            GO TO 8200-SEND-DATAONLY.                                   EL6523
                                                                        EL6523
           EJECT                                                        EL6523
       4200-ADD.                                                        EL652

      *    PERFORM 7000-EDIT           THRU 7000-EXIT
                                                                        EL652
           IF EMI-NO-ERRORS                                             EL652
              CONTINUE
           ELSE                                                         EL652
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        EL652
           EXEC CICS HANDLE CONDITION                                   EL652
               NOTOPEN  (9990-ABEND)                                    EL652
               NOTFND   (4250-CONT)                                     EL652
           END-EXEC.                                                    EL652
                                                                        EL652
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT
                                                                        EL652
           MOVE ER-2057                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
                                                                           CL**5
           MOVE LOW-VALUES             TO PI-SAVE-ERBXRF-KEY
                                                                        EL652
           MOVE -1                     TO  MAINTYPL
                                                                        EL652
           GO TO 8200-SEND-DATAONLY
           .
       4250-CONT.                                                       EL652

           EXEC CICS GETMAIN                                            EL652
               SET      (ADDRESS OF BANK-CROSS-REFERENCE)
               LENGTH   (BXRF-REC-LENGTH)
               INITIMG  (GETMAIN-SPACE)                                 EL652
           END-EXEC
                                                                        EL652
           MOVE SPACES                 TO BANK-CROSS-REFERENCE

           MOVE PI-COMPANY-CD          TO BK-COMPANY-CD
           MOVE 'BK'                   TO BK-RECORD-ID

           MOVE PI-ERC-KEY             TO BK-CONTROL-PRIMARY
           MOVE +1                     TO PI-NDX
           SET NO-CHANGES-MADE         TO TRUE
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT

           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (BXRF-FILE-ID)
              END-EXEC
              GO TO 8200-SEND-DATAONLY
           END-IF 

           PERFORM 7325-COMPRESS       THRU 7325-EXIT
           MOVE PI-NDX                 TO BK-BANK-POINTER-CNT
           PERFORM 7510-SORT-ACCOUNTS  THRU 7599-EXIT

           MOVE PI-PROCESSOR-ID        TO BK-LAST-MAINT-USER
           MOVE EIBTIME                TO BK-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO BK-LAST-MAINT-DT

           EXEC CICS WRITE                                              EL652
               DATASET  (BXRF-FILE-ID)                                  EL652
               FROM     (BANK-CROSS-REFERENCE)                          EL652
               RIDFLD   (BK-CONTROL-PRIMARY)                            EL652
           END-EXEC
                                                                           CL*21
           MOVE LOW-VALUES             TO EL652DO
           MOVE 'S'                    TO MAINTYPO
           MOVE +1                     TO MAINTYPL
           MOVE AL-UANON               TO MAINTYPA
           MOVE PI-ERC-CARRIER         TO CARRIERO
           MOVE AL-UANON               TO CARRIERA
           MOVE +1                     TO CARRIERL
           IF PI-ERC-GROUP NOT = SPACES
               MOVE PI-ERC-GROUP       TO GROUPO
               MOVE AL-UANON           TO GROUPA
               MOVE +6                 TO GROUPL
           END-IF
                                                                        EL652
           IF PI-ERC-BANK NOT = SPACES
              MOVE PI-ERC-BANK         TO BANKO
              MOVE AL-UANON            TO BANKA
              MOVE +10                 TO BANKL
           END-IF

           GO TO 1000-EDIT-INPUT
           .
       4200-EXIT.
           EXIT.

       4400-CHANGE.

           IF PI-ERC-KEY = PI-SAVE-ERBXRF-KEY
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERBXRF-UPDATE
                                       THRU 7200-EXIT

           IF (BK-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (BK-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (BXRF-FILE-ID)
              END-EXEC
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           
           MOVE PI-TOP-NDX             TO PI-NDX
           SET NO-CHANGES-MADE         TO TRUE
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT

           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (BXRF-FILE-ID)
              END-EXEC
              GO TO 8200-SEND-DATAONLY
           END-IF 

           IF NO-CHANGES-MADE
              MOVE ER-3112             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF

           COMPUTE PI-NDX = BK-BANK-POINTER-CNT + +13
           PERFORM 7325-COMPRESS       THRU 7325-EXIT

           MOVE PI-NDX                 TO BK-BANK-POINTER-CNT

           PERFORM 7510-SORT-ACCOUNTS  THRU 7599-EXIT

           MOVE PI-PROCESSOR-ID        TO BK-LAST-MAINT-USER
           MOVE EIBTIME                TO BK-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO BK-LAST-MAINT-DT

           EXEC CICS REWRITE
               DATASET  (BXRF-FILE-ID)
               FROM     (BANK-CROSS-REFERENCE)
           END-EXEC

           GO TO 1000-EDIT-INPUT
           .

       4600-DELETE.

           IF PI-ERC-KEY = PI-SAVE-ERBXRF-KEY
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF




031605     IF PI-EL6523-DEL-SW = 'Y'
031605        CONTINUE
031605     ELSE
031605        MOVE 'Y'                 TO PI-EL6523-DEL-SW
031605        MOVE ER-1299             TO EMI-ERROR
031605        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
031605        MOVE SPACES              TO MAINTYPO
031605        MOVE -1                  TO MAINTYPL
031605        GO TO 1000-EDIT-INPUT
031605     END-IF




           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERBXRF-UPDATE
                                       THRU 7200-EXIT

           IF (BK-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (BK-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (BXRF-FILE-ID)
              END-EXEC
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS DELETE
               DATASET  (BXRF-FILE-ID)
           END-EXEC
                                                                        EL652
           MOVE ER-0000                TO  EMI-ERROR                    EL652
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL652
                                                                        EL652
           MOVE LOW-VALUES             TO  EL652DO                      EL652
           MOVE PI-ERC-CARRIER         TO  CARRIERO                     EL652
           MOVE AL-UANON               TO  CARRIERA                     EL652
                                                                        EL652
           IF PI-ERC-GROUP NOT = SPACES                                    CL**6
               MOVE PI-ERC-GROUP       TO  GROUPO                          CL**6
               MOVE AL-UANON           TO  GROUPA                       EL652
           END-IF
                                                                        EL652
           IF PI-ERC-BANK NOT = SPACES                                  EL652
              MOVE PI-ERC-BANK        TO  BANKO
              MOVE AL-UANON           TO  BANKA
           END-IF
                                                                        EL652
           MOVE LOW-VALUES             TO  PI-SAVE-ERBXRF-KEY           EL652
                                                                        EL652
           GO TO 8100-SEND-INITIAL-MAP                                  EL652
           .                                                            EL652

       4600-EXIT.                                                       EL652
           EXIT.                                                        EL652
       5000-BROWSE-FILE.                                                EL6523

           MOVE SPACE                  TO BROWSE-STARTED-SW
           EXEC CICS HANDLE CONDITION
               NOTFND (5800-NO-RECORD)
               ENDFILE (5900-END-OF-FILE)
           END-EXEC
                                                                        EL6523
           MOVE PI-SAVE-ERBXRF-KEY     TO WS-CONTROL-PRIMARY

           EXEC CICS STARTBR
               DATASET (BXRF-FILE-ID)
               RIDFLD (WS-CONTROL-PRIMARY)
           END-EXEC

           MOVE 'Y'                    TO BROWSE-STARTED-SW

           IF EIBAID = DFHPF2
              GO TO 5100-BROWSE-BKWD
           END-IF
           .
       5010-READ-LOOP.                                                  EL6523

           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH

           EXEC CICS READNEXT
               DATASET (BXRF-FILE-ID)
               SET     (ADDRESS OF BANK-CROSS-REFERENCE)
               RIDFLD  (WS-CONTROL-PRIMARY)
               LENGTH  (BXRF-REC-LENGTH)
           END-EXEC

           MOVE BK-BANK-POINTER-CNT    TO BK-BANK-POINTER-CNT

           MOVE +1                     TO PI-NDX

           IF BK-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF

           IF EIBAID = DFHPF1
              IF CARRIERO     = BK-CARRIER
                 AND GROUPO   = BK-GROUPING
                 AND BANKO    = BK-BANK-NO
                 GO TO 5010-READ-LOOP
              END-IF
           END-IF

           MOVE BK-LAST-MAINT-USER     TO PI-UPDATE-BY

           IF BK-LAST-MAINT-HHMMSS NUMERIC
              MOVE BK-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE BK-CONTROL-PRIMARY     TO PI-SAVE-ERBXRF-KEY
           GO TO 5200-FORMAT-SCREEN
           .
       5100-BROWSE-BKWD.

           EXEC CICS HANDLE CONDITION
                NOTFND (5900-END-OF-FILE)
           END-EXEC

           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH

           EXEC CICS READPREV
               DATASET (BXRF-FILE-ID)
               SET     (ADDRESS OF BANK-CROSS-REFERENCE)
               LENGTH  (BXRF-REC-LENGTH)
               RIDFLD  (WS-CONTROL-PRIMARY)
           END-EXEC

           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH

           IF PI-FILE-EOF
              MOVE SPACE               TO PI-EOF-SW
           ELSE
              EXEC CICS READPREV
                   DATASET (BXRF-FILE-ID)
                   SET     (ADDRESS OF BANK-CROSS-REFERENCE)
                   LENGTH  (BXRF-REC-LENGTH)
                   RIDFLD  (WS-CONTROL-PRIMARY)
              END-EXEC
           END-IF

           MOVE BK-BANK-POINTER-CNT TO BK-BANK-POINTER-CNT

           MOVE +1                     TO PI-NDX

           IF BK-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF

           MOVE BK-LAST-MAINT-USER     TO PI-UPDATE-BY

           IF BK-LAST-MAINT-HHMMSS NUMERIC
              MOVE BK-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE BK-CONTROL-PRIMARY     TO PI-SAVE-ERBXRF-KEY
           GO TO 5200-FORMAT-SCREEN
           .
       5200-FORMAT-SCREEN.                                              EL6523

           MOVE LOW-VALUES             TO EL652DI
                                                                        EL6523
           MOVE BK-CONTROL-PRIMARY     TO PI-ERBXRF-KEY
                                                                        EL6523
           MOVE BK-CARRIER             TO CARRIERO
                                                                        EL6523
           MOVE BK-GROUPING            TO GROUPO
                                                                        EL6523
           MOVE BK-BANK-NO             TO BANKO
                                                                        EL6523
           MOVE BK-BANK-POINTER-CNT    TO TOTOCCO
                                                                        EL6523
           PERFORM 7500-COUNT-OPEN-RANGES
                                       THRU 7500-EXIT

           MOVE WS-OPEN-COUNT          TO TOTOPNO

           IF BK-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES AND ZEROS
              MOVE BK-LAST-MAINT-DT    TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO MAINTDTO
           ELSE
              MOVE '00/00/00'          TO MAINTDTO
            END-IF

           PERFORM 3000-BLD-LINE       THRU 3000-XIT

           MOVE AL-UANON               TO CARRIERA GROUPA
                                          BANKA
           GO TO 8100-SEND-INITIAL-MAP
           .
           
       5800-NO-RECORD.                                                  EL6523
           MOVE -1                     TO CARRIERL.                     EL6523
           MOVE AL-UANON               TO CARRIERA   GROUPA             EL6523
                                           BANKA
           MOVE ER-1164                TO EMI-ERROR.                       CL**4
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6523
           IF BROWSE-STARTED                                            EL6523
             EXEC CICS ENDBR                                            EL6523
                  DATASET  (BXRF-FILE-ID)                               EL6523
                  END-EXEC.                                             EL6523
                                                                        EL6523
           GO TO 8200-SEND-DATAONLY.                                    EL6523
                                                                        EL6523
       5900-END-OF-FILE.                                                EL6523
           IF EIBAID = DFHPF1                                           EL6523
               MOVE 'Y'                TO PI-EOF-SW                     EL6523
               MOVE ER-2237            TO EMI-ERROR                     EL6523
           ELSE                                                         EL6523
               MOVE LOW-VALUES         TO PI-ERBXRF-KEY                 EL6523
               MOVE PI-COMPANY-CD      TO PI-BXRF-COMP-CD               EL6523
               MOVE ER-2238            TO EMI-ERROR.                    EL6523
                                                                        EL6523
           MOVE -1            TO CARRIERL.                              EL6523
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6523
           IF BROWSE-STARTED                                            EL6523
             EXEC CICS ENDBR                                            EL6523
                  DATASET  (BXRF-FILE-ID)                               EL6523
                  END-EXEC.                                             EL6523
                                                                        EL6523
           MOVE SPACE TO BROWSE-STARTED-SW.                             EL6523
           GO TO 8200-SEND-DATAONLY.                                    EL6523
           EJECT                                                        EL6523
       6000-OCCURRENCE.

           MOVE PI-ERBXRF-KEY          TO WS-CONTROL-PRIMARY

           EXEC CICS HANDLE CONDITION                                   EL6523
               NOTFND (1500-BXRF-NOT-FOUND)
           END-EXEC
                                                                        EL6523
           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT

           IF EIBAID = DFHPF3
              GO TO 6500-NEXT-OCC
           ELSE
              GO TO 7000-PRIOR-OCC
           END-IF
           .
       6500-NEXT-OCC.

      *    IF PI-NDX = BK-BANK-POINTER-CNT
      *       MOVE -1                  TO CARRIERL
      *       MOVE ER-0130             TO EMI-ERROR
      *       PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *       GO TO 8200-SEND-DATAONLY
      *    END-IF

           ADD +1                      TO PI-NDX
           MOVE PI-NDX                 TO PI-TOP-NDX

           GO TO 5200-FORMAT-SCREEN
           
           .
       7000-PRIOR-OCC.

           IF PI-NDX LESS 14
              MOVE -1                  TO CARRIERL
              MOVE ER-0131             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

           COMPUTE PI-NDX = (PI-NDX - 1) / 13
           IF PI-NDX NOT = 1
              COMPUTE PI-NDX = PI-NDX * 13
              COMPUTE PI-NDX = PI-NDX - 12
           END-IF

           MOVE PI-NDX                 TO PI-TOP-NDX
           GO TO 5200-FORMAT-SCREEN

           .
       7050-READ-ERBXRF.
       
           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH

           EXEC CICS READ
                SET     (ADDRESS OF BANK-CROSS-REFERENCE)
                DATASET ('ERBXRF')
                LENGTH  (BXRF-REC-LENGTH)
                RIDFLD  (WS-CONTROL-PRIMARY)
           END-EXEC

           MOVE BK-BANK-POINTER-CNT    TO BK-BANK-POINTER-CNT
           .
       7050-EXIT.
           EXIT.
           

       7200-READ-ERBXRF-UPDATE.
       
           EXEC CICS HANDLE CONDITION
               NOTFND (7200-BXRF-NOT-FOUND)
           END-EXEC

           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH

           EXEC CICS READ
                SET     (ADDRESS OF BANK-CROSS-REFERENCE)
                DATASET ('ERBXRF')
                LENGTH  (BXRF-REC-LENGTH)
                RIDFLD  (WS-CONTROL-PRIMARY)
                UPDATE
           END-EXEC

           MOVE BK-BANK-POINTER-CNT    TO BK-BANK-POINTER-CNT

           .
       7200-EXIT.
           EXIT.
           .

       7200-BXRF-NOT-FOUND.

           MOVE ER-0142                TO EMI-ERROR
           MOVE -1                     TO CARRIERL
           MOVE AL-UABON               TO CARRIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           IF EIBTRNID  = EL652-TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       7300-EDIT-DATA.
       
           SET NO-LINE-CHANGES         TO TRUE
           PERFORM VARYING NDX FROM +1 BY +1 UNTIL
              (NDX > +13)
              IF BL-ACCOUNTL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-ACCOUNT (NDX) TO BK-AM-ACCOUNT (PI-NDX)
              END-IF
              IF BL-STATEL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-STATE (NDX)   TO BK-AM-STATE (PI-NDX)
              END-IF
              IF BL-EFFECTL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-EFFECT (NDX)  TO WS-DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-DEEDIT-FIELD-DATE-OUT
                                       TO  DC-GREG-DATE-1-MDY
                 MOVE '4'              TO  DC-OPTION-CODE
                 PERFORM 9700-DATE-CONVERSION
                 IF DATE-CONVERSION-ERROR
                    MOVE ER-0348              TO EMI-ERROR
                    MOVE -1                   TO BL-EFFECTL (NDX)
                    MOVE AL-UABON             TO BL-EFFECTA (NDX)
                    PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT
                 ELSE
                    MOVE DC-GREG-DATE-1-EDIT  TO BL-EFFECT (NDX)
                    MOVE AL-UANON             TO BL-EFFECTA (NDX)
                    MOVE DC-BIN-DATE-1        TO BK-AM-EFF-DT (PI-NDX)
                 END-IF
              END-IF
              IF BL-EXPIREL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-EXPIRE (NDX)  TO WS-DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-DEEDIT-FIELD-DATE-OUT = 999999
                    MOVE HIGH-VALUES   TO BK-AM-EXP-DT (PI-NDX)
                    MOVE '99/99/99'    TO BL-EXPIRE (NDX)
                    MOVE AL-UANON      TO BL-EXPIREA (NDX)
                 ELSE
                    MOVE WS-DEEDIT-FIELD-DATE-OUT
                                       TO DC-GREG-DATE-1-MDY
                    MOVE '4'           TO DC-OPTION-CODE
                    PERFORM 9700-DATE-CONVERSION
                    IF DATE-CONVERSION-ERROR
                       MOVE ER-0454    TO EMI-ERROR
                       MOVE -1         TO BL-EXPIREL (NDX)
                       MOVE AL-UABON   TO BL-EXPIREA (NDX)
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    ELSE
                       MOVE DC-GREG-DATE-1-EDIT
                                       TO BL-EXPIRE (NDX)
                       MOVE AL-UANON   TO BL-EXPIREA (NDX)
                       MOVE DC-BIN-DATE-1
                                       TO BK-AM-EXP-DT (PI-NDX)
                    END-IF
                 END-IF
              END-IF
              IF LINE-CHANGES
                 SET CHANGES-MADE      TO TRUE
                 IF BK-AM-EXP-DT (PI-NDX) NOT > BK-AM-EFF-DT (PI-NDX)
                    MOVE ER-2339       TO EMI-ERROR
                    MOVE -1            TO BL-EXPIREL (NDX)
                    MOVE AL-UABON      TO BL-EXPIREA (NDX)
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
                 IF BK-AM-ACCOUNT (PI-NDX) NOT = SPACES
                    PERFORM 7350-VERIFY-ACCOUNT
                                       THRU 7350-EXIT
                 END-IF
              END-IF
              ADD +1                   TO PI-NDX
              SET NO-LINE-CHANGES      TO TRUE
           END-PERFORM
           
           .
       7300-EXIT.
           EXIT.
           
       7325-COMPRESS.
       
           PERFORM VARYING WSUB1 FROM +1 BY +1 UNTIL
              (WSUB1 > PI-NDX)
              IF BK-AM-ACCOUNT (WSUB1) = SPACES OR LOW-VALUES OR ZEROS
                 PERFORM VARYING WSUB2 FROM WSUB1 BY +1 UNTIL
                    (BK-AM-ACCOUNT (WSUB2) NOT = SPACES AND LOW-VALUES
                                             AND ZEROS)
                    OR (WSUB2 > PI-NDX)
                 END-PERFORM
                 IF WSUB2 NOT > PI-NDX
                    MOVE BK-AM-ACCOUNT (WSUB2)
                                       TO BK-AM-ACCOUNT (WSUB1)
                    MOVE BK-AM-EFF-DT (WSUB2)
                                       TO BK-AM-EFF-DT (WSUB1)
                    MOVE BK-AM-EXP-DT (WSUB2)
                                       TO BK-AM-EXP-DT (WSUB1)
                    MOVE BK-AM-STATE (WSUB2)
                                       TO BK-AM-STATE (WSUB1)
                    MOVE LOW-VALUES    TO BK-AM-ACCOUNT (WSUB2)
                                          BK-AM-EFF-DT  (WSUB2)
                                          BK-AM-EXP-DT  (WSUB2)
                                          BK-AM-STATE   (WSUB2)
                 END-IF
              END-IF
           END-PERFORM

           PERFORM VARYING WSUB1 FROM +1 BY +1 UNTIL
              BK-AM-ACCOUNT (WSUB1) = SPACES OR LOW-VALUES
           END-PERFORM

           COMPUTE PI-NDX = WSUB1 - +1
           
           .
       7325-EXIT.
           EXIT.

       7350-VERIFY-ACCOUNT.

           MOVE LOW-VALUES             TO WS-ERACCT-KEY
           MOVE BK-COMPANY-CD          TO WS-ERACCT-COMP-CD
           MOVE BK-CARRIER             TO WS-ERACCT-CARRIER
           MOVE BK-GROUPING            TO WS-ERACCT-GROUPING
           MOVE BK-AM-STATE (PI-NDX)   TO WS-ERACCT-STATE
           MOVE BK-AM-ACCOUNT (PI-NDX) TO WS-ERACCT-ACCOUNT

           EXEC CICS READ
               DATASET ('ERACCT')
               SET     (ADDRESS OF ACCOUNT-MASTER)
               RIDFLD  (WS-ERACCT-KEY)
               GTEQ
               RESP    (WS-RESPONSE)
           END-EXEC
 
           IF RESP-NORMAL
              IF (AM-COMPANY-CD   = PI-COMPANY-CD)
                 AND (AM-CARRIER  = BK-CARRIER)
                 AND (AM-GROUPING = BK-GROUPING)
                 AND (AM-STATE    = BK-AM-STATE (PI-NDX))
                 AND (AM-ACCOUNT  = BK-AM-ACCOUNT (PI-NDX))
                 GO TO 7350-EXIT
              END-IF
           END-IF

           .
       7350-NOT-FOUND.
       
           MOVE ER-0226                TO EMI-ERROR
           MOVE -1                     TO BL-ACCOUNTL (NDX)
           MOVE AL-UABON               TO BL-ACCOUNTA (NDX)
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           .
       7350-EXIT.
           EXIT.

           
       7400-READ-CONTROL-FILE.

           MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
           MOVE WS-ACCESS              TO CNTL-ACCESS

           EXEC CICS HANDLE CONDITION
               NOTFND  (7490-NOT-FOUND)
               ERROR   (9990-ABEND)
           END-EXEC

           EXEC CICS READ
               DATASET  (CNTL-FILE-ID)
               SET      (ADDRESS OF CONTROL-FILE)
               RIDFLD   (ELCNTL-KEY)
           END-EXEC

           IF CNTL-REC-TYPE = '6'
              MOVE AL-UANON           TO CARRIERA
              MOVE CARRIERI           TO PI-ERC-CARRIER
              GO TO 7499-EXIT
           END-IF
           .
       7490-NOT-FOUND.

           IF CNTL-REC-TYPE = '6'
              MOVE -1                  TO CARRIERL
              MOVE AL-UABON            TO CARRIERA
              MOVE ER-0193             TO EMI-ERROR
           END-IF

           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           .
       7499-EXIT.
           EXIT.

       7500-COUNT-OPEN-RANGES.                                             CL**3
                                                                           CL**3
           MOVE +1                     TO NDX
           MOVE +0                     TO WS-OPEN-COUNT.                   CL**3
                                                                           CL**3
       7500-LOOP.                                                          CL**3
                                                                           CL**3
           IF NDX GREATER THAN BK-BANK-POINTER-CNT
                MOVE +1                TO NDX
                GO TO 7500-EXIT.                                           CL**3
                                                                           CL**3
           IF BK-AM-EXP-DT (NDX) = HIGH-VALUES
               ADD +1 TO WS-OPEN-COUNT.                                    CL**3
                                                                           CL**3
           ADD +1 TO NDX
                                                                           CL**3
           GO TO 7500-LOOP.                                                CL**3
                                                                           CL**3
       7500-EXIT.                                                          CL**3
           EXIT.                                                           CL**3
                                                                           CL**3
           EJECT                                                           CL**3
00837  7510-SORT-ACCOUNTS.

00857      IF BK-BANK-POINTER-CNT NOT > +1
00858         GO TO 7599-EXIT
           END-IF

00860      MOVE +1                     TO WSUB1
00861                                     WSUB2
00862      MOVE HIGH-VALUES            TO WS-WORK-ACCT-TABLE
           .
00864  7520-CONTINUE-SORT.

00866      IF WSUB2 > BK-BANK-POINTER-CNT
00867         GO TO 7540-RID-OF-HIGH-VALUES
           END-IF

           IF (BK-AM-ACCOUNT (WSUB1) < WS-AM-ACCOUNT (WSUB2))
              AND (BK-AM-ACCOUNT (WSUB1) NOT = SPACES)
              MOVE BK-BANK-POINTER (WSUB1)
                                       TO WS-BANKS (WSUB2)
              MOVE WSUB1               TO WSUB3
           END-IF

00876      ADD +1 TO WSUB1

00878      IF WSUB1 NOT > BK-BANK-POINTER-CNT
00879         GO TO 7520-CONTINUE-SORT
           END-IF

00890      MOVE HIGH-VALUES            TO BK-BANK-POINTER (WSUB3)
00891      ADD +1                      TO WSUB2
00892      MOVE +1                     TO WSUB1
00893      GO TO 7520-CONTINUE-SORT
00894      .
00897  7540-RID-OF-HIGH-VALUES.

00899      IF WS-BANKS (WSUB1) = HIGH-VALUES
00900         MOVE SPACES              TO WS-BANKS (WSUB1)
           END-IF

00902      ADD +1 TO WSUB1

00904      IF WSUB1 NOT > BK-BANK-POINTER-CNT
00905         GO TO 7540-RID-OF-HIGH-VALUES
           END-IF

00907      MOVE +1                     TO WSUB1
00908      MOVE +2                     TO WSUB2
00909      .
00910  7550-CHECK-DUPS.

00912      IF (WS-AM-ACCOUNT (WSUB1) = WS-AM-ACCOUNT (WSUB2))
00913         AND (WS-AM-STATE (WSUB1) = WS-AM-STATE (WSUB2))
00914         AND (WS-AM-EXP-DT (WSUB1) NOT = SPACES)
00915         MOVE ER-2947             TO  EMI-ERROR
00916         PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
00917         MOVE -1                  TO  MAINTYPL
00918         GO TO 8200-SEND-DATAONLY
           END-IF

00920      ADD +1 TO WSUB2

00922      IF WSUB2 NOT > BK-BANK-POINTER-CNT
00923         GO TO 7550-CHECK-DUPS
           END-IF

00925      ADD +1                      TO WSUB1
00926      MOVE WSUB1                  TO WSUB2
00927      ADD +1                      TO WSUB2

00929      IF WSUB1 NOT > BK-BANK-POINTER-CNT
00930         GO TO 7550-CHECK-DUPS
           END-IF

           PERFORM VARYING WSUB1 FROM +1 BY +1 UNTIL
              (WSUB1 > BK-BANK-POINTER-CNT)
              MOVE WS-BANKS (WSUB1)    TO BK-BANK-POINTER (WSUB1)
           END-PERFORM
           .
00934  7599-EXIT.
00935      EXIT.

       8100-SEND-INITIAL-MAP.                                           EL6523
           MOVE SAVE-DATE              TO DATEO.                        EL6523
           MOVE EIBTIME                TO TIME-IN.                      EL6523
           MOVE TIME-OUT               TO TIMEO.                        EL6523
           MOVE -1                     TO MAINTYPL                      EL6523
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL6523
                                                                        EL6523
           EXEC CICS SEND                                               EL6523
               MAP    (MAP-NAME)                                        EL6523
               MAPSET (MAPSET-NAME)                                     EL6523
               FROM   (EL652DO)                                         EL6523
               ERASE                                                    EL6523
               CURSOR                                                   EL6523
               END-EXEC.                                                EL6523
           GO TO 9100-RETURN-TRAN.                                      EL6523
                                                                        EL6523
       8200-SEND-DATAONLY.                                              EL6523
           MOVE SAVE-DATE              TO DATEO.                        EL6523
           MOVE EIBTIME                TO TIME-IN.                      EL6523
           MOVE TIME-OUT               TO TIMEO.                        EL6523
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL6523
           EXEC CICS SEND                                               EL6523
               MAP    (MAP-NAME)                                        EL6523
               MAPSET (MAPSET-NAME)                                     EL6523
               FROM   (EL652DO)                                         EL6523
               DATAONLY                                                 EL6523
               CURSOR                                                   EL6523
           END-EXEC.
           GO TO 9100-RETURN-TRAN.                                      EL6523
                                                                        EL6523
           EJECT                                                        EL6523
       8500-DATE-CONVERT.                                               EL6523
           MOVE LINK-ELDATCV           TO PGM-NAME.                     EL6523
           EXEC CICS LINK                                               EL6523
               PROGRAM    (PGM-NAME)                                    EL6523
               COMMAREA   (DATE-CONVERSION-DATA)                        EL6523
               LENGTH     (DC-COMM-LENGTH)                              EL6523
               END-EXEC.                                                EL6523
       8500-EXIT.                                                       EL6523
           EXIT.                                                        EL6523
                                                                        EL6523
       8600-DEEDIT.
           EXEC CICS  BIF DEEDIT
               FIELD   (WS-DEEDIT-FIELD)
               LENGTH  (10)
           END-EXEC.
       8600-EXIT.
            EXIT.

       8810-PF23.                                                       EL6523
           MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6523
           MOVE XCTL-005               TO PGM-NAME.                     EL6523
           GO TO 9300-XCTL.                                             EL6523
                                                                        EL6523
       9100-RETURN-TRAN.                                                EL6523
           MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.                  CL**2
           MOVE '652D'               TO PI-CURRENT-SCREEN-NO.              CL**2
           EXEC CICS RETURN                                             EL6523
               TRANSID(TRANS-ID)                                        EL6523
               COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6523
               LENGTH(PI-COMM-LENGTH)                                   EL6523
               END-EXEC.                                                EL6523
                                                                        EL6523
           GOBACK.                                                      EL6523
                                                                        EL6523
       9910-INITIALIZE-SECURITY.                                           CL*19
      ******************************************************************   CL*19
      *                                                                *   CL*19
      *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL*19
      *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *   CL*19
      *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL*19
      *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL*19
      *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL*19
      *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL*19
      *                                                                *   CL*19
      ******************************************************************   CL*19
                                                                           CL*19
           IF PI-PROCESSOR-ID NOT = 'LGXX'

              EXEC CICS    READQ TS
                   QUEUE   (PI-SECURITY-TEMP-STORE-ID)
                   INTO    (SECURITY-CONTROL)
                   LENGTH  (SC-COMM-LENGTH)
                   ITEM    (SC-ITEM-CL-CR)
              END-EXEC

              MOVE SC-CREDIT-DISPLAY (05)
                                       TO PI-DISPLAY-CAP
              MOVE SC-CREDIT-UPDATE  (05)
                                       TO PI-MODIFY-CAP

              IF  NOT DISPLAY-CAP
                  MOVE 'READ'          TO SM-READ
                  PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
                  MOVE ER-0070         TO EMI-ERROR
                  PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                  GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF
           .
       9910-EXIT.
           EXIT.

       9200-RETURN-MAIN-MENU.                                           EL6523
           MOVE XCTL-626               TO PGM-NAME.                     EL6523
           GO TO 9300-XCTL.                                             EL6523
                                                                        EL6523
           EJECT                                                        EL6523
                                                                        EL6523
       9300-XCTL.                                                       EL6523
           EXEC CICS XCTL                                               EL6523
               PROGRAM    (PGM-NAME)                                    EL6523
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6523
               LENGTH     (PI-COMM-LENGTH)                              EL6523
               END-EXEC.                                                EL6523
                                                                        EL6523
       9400-CLEAR.                                                      EL6523
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL6523
           GO TO 9300-XCTL.                                             EL6523
                                                                        EL6523
       9500-PF12.                                                          CL**2
           MOVE XCTL-010               TO PGM-NAME.                        CL**2
           GO TO 9300-XCTL.                                                CL**2

       9700-DATE-CONVERSION.

           EXEC CICS LINK                                               EL652
               PROGRAM   ('ELDATCV')                                    EL652
               COMMAREA  (DATE-CONVERSION-DATA)                         EL652
               LENGTH    (DC-COMM-LENGTH)                               EL652
           END-EXEC.                                                    EL652
                                                                        EL652
       9700-EXIT.                                                       EL652
           EXIT.                                                        EL652
                                                                        EL652
       9900-ERROR-FORMAT.                                               EL6523
           IF NOT EMI-ERRORS-COMPLETE                                   EL6523
               MOVE LINK-001 TO PGM-NAME                                EL6523
               EXEC CICS LINK                                           EL6523
                   PROGRAM(PGM-NAME)                                    EL6523
                   COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL6523
                   LENGTH(EMI-COMM-LENGTH)                              EL6523
                   END-EXEC.                                            EL6523
       9900-EXIT.                                                       EL6523
           EXIT.                                                        EL6523
                                                                        EL6523
       9990-ABEND.                                                      EL6523
           MOVE LINK-004 TO PGM-NAME.                                   EL6523
           MOVE DFHEIBLK TO EMI-LINE1.                                  EL6523
           EXEC CICS LINK                                               EL6523
               PROGRAM   (PGM-NAME)                                     EL6523
               COMMAREA  (EMI-LINE1)                                    EL6523
               LENGTH    (72)                                           EL6523
               END-EXEC.                                                EL6523
           GO TO 8200-SEND-DATAONLY.                                    EL6523

       9995-SECURITY-VIOLATION.                                         EL652
                                       COPY ELCSCTP.
       9995-EXIT.                                                       EL652
            EXIT.                                                       EL652
