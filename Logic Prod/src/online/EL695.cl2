       ID DIVISION.

       PROGRAM-ID.                 EL695.
      *              PROGRAM CONVERTED BY
      *              COBOL CONVERSION AID PO 5785-ABJ
      *              CONVERSION DATE 02/12/96 10:02:23.
      *                            VMOD=2.004
      *
      *
      *AUTHOR.     CENTRAL STATES HEALTH AND LIFE CO. OF OMAHA.
      *            OMAHA, NEBRASKA.

      *DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   HEALTH AND LIFE CO. OF OMAHA                    *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

      *REMARKS.    TRANSACTION - EXM8 - ENDORSEMENT PRINTING.
      *        THIS FUNCTION IS USED TO START THE PRINTING OF THE
      *        ENDORSEMENTS.

                                       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC  X(32) VALUE '********************************'.
       77  FILLER  PIC  X(32) VALUE '*    EL695 WORKING STORAGE     *'.
       77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.001 *********'.

       01  W-PROGRAM-CONSTANTS.
           12  FILLER                  PIC  X(18)
                                       VALUE 'PROGRAM CONSTANTS:'.
           12  W-ENDR-LENGTH           PIC S9(04) COMP  VALUE +185.
           12  W-APPL-SCRTY-NDX        PIC S9(04) COMP VALUE +03.

           12  W-ENDR-ID               PIC  X(08)  VALUE 'ERENDR'.
           12  W-CNTL-ID               PIC  X(08)  VALUE 'ELCNTL'.
           12  W-LINK-001              PIC  X(05)  VALUE 'EL001'.
           12  W-LINK-004              PIC  X(05)  VALUE 'EL004'.
           12  W-MAP.
               16  W-MAP-PREFIX        PIC  X(02)  VALUE 'EL'.
               16  W-MAP-NUMBER        PIC  X(04)  VALUE '695A'.
               16  W-MAP-FILLER        PIC  X(02)  VALUE '  '.
           12  W-MAPSET                PIC  X(08)  VALUE 'EL695S'.
           12  W-PRINT-TRANS           PIC  X(04)  VALUE 'EXM9'.
           12  W-THIS-PGM              PIC  X(08)  VALUE 'EL695'.
           12  W-TRANSACTION           PIC  X(04)  VALUE 'EXM8'.
           12  W-XCTL-005              PIC  X(05)  VALUE 'EL005'.
           12  W-XCTL-010              PIC  X(05)  VALUE 'EL010'.
           12  W-XCTL-626              PIC  X(05)  VALUE 'EL626'.
                                       EJECT
       01  W-PROGRAM-WORK-AREA.
           12  FILLER                  PIC  X(18)
                                       VALUE 'PROGRAM WORK AREA:'.
           12  W-RECORD-COUNT          PIC S9(04) COMP   VALUE +0.

           12  W-COPIES-CTR            PIC S9(05) COMP-3 VALUE +0.
           12  W-DELAY-INTERVAL        PIC S9(07) COMP-3 VALUE +2.
           12  W-SUB                   PIC  9(01) COMP-3.
           12  W-LAST-ERROR            PIC  9(05)  VALUE 9999.
           12  W-CALL-PGM              PIC  X(08).
           12  W-CARRIER               PIC  X(01)  VALUE SPACE.
           12  W-CURRENT-DATE-SAVE     PIC  X(02).
           12  W-CURRENT-DATE-EDIT     PIC  X(08)  VALUE SPACES.
           12  W-DEEDIT-FIELD          PIC  X(15).
           12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD
                                       PIC S9(15).
           12  W-ENTRY                 PIC  X(06)  VALUE SPACES.
           12  W-FORM                  PIC  X(04)  VALUE SPACES.
           12  W-GROUPING              PIC  X(06)  VALUE SPACES.
           12  W-ACCOUNT               PIC  X(10)  VALUE SPACES.
           12  W-STATE                 PIC  X(02)  VALUE SPACES.

           12  W-TIME-IN               PIC S9(07).
           12  W-TIME-OUT-R REDEFINES W-TIME-IN.
               16  FILLER              PIC  X(01).
               16  W-TIME-OUT          PIC  9(01)9V99.
               16  FILLER              PIC  X(02).

       01  W-PROGRAM-SWITCHES-INDICATORS.
           12  FILLER                  PIC  X(17)
                                       VALUE 'PROGRAM SWITCHES:'.
           12  W-COUNT-BY-KEY-IND      PIC  X(01)  VALUE SPACE.
               88  W-COUNT-BY-KEY              VALUE 'A' 'C' 'G'
                                                     'P' 'S'.
               88  W-COUNT-BY-CARRIER          VALUE 'C'.
               88  W-COUNT-BY-GROUPING         VALUE 'G'.
               88  W-COUNT-BY-STATE            VALUE 'S'.
               88  W-COUNT-BY-ACCOUNT          VALUE 'A'.
               88  W-COUNT-BY-PROCESSOR        VALUE 'P'.
               88  W-COUNT-BY-QUE-CONTROL      VALUE 'Q'.
               88  W-COUNT-BY-ENTRY            VALUE 'E'.
           12  W-DATE-FOUND-IND        PIC  X(01)  VALUE SPACE.
               88  W-DATE-FOUND                    VALUE 'Y'.

       01  W-PROGRAM-KEY-FIELDS.
           12  FILLER                  PIC  X(19)
                                       VALUE 'PROGRAM KEY FIELDS:'.
           12  W-ARCH-KEY.
               16  W-ARCH-CO           PIC  X(01).
               16  W-ARCH-NUMBER       PIC S9(08)     COMP.
               16  W-ARCH-SEQ          PIC S9(04)     COMP VALUE +0.

           12  W-ARCH-SAVE-KEY         PIC  X(05).

           12  W-CNTL-KEY.
               16  W-CNTL-COMPANY-ID   PIC  X(03).
               16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '1'.
               16  W-CNTL-GENL.
                   20  W-CNTL-GEN1     PIC  X(02)  VALUE SPACES.
                   20  W-CNTL-GEN2.
                       24   W-CNTL-GEN3
                                       PIC  X(01)  VALUE SPACES.
                       24   W-CNTL-GEN4
                                       PIC  X(01)  VALUE SPACES.
               16  W-CNTL-SEQ          PIC S9(04)  VALUE +0    COMP.

           12  W-SC-QUID-KEY.
               16  W-SC-QUID-TERMINAL  PIC  X(04).
               16  W-SC-QUID-SYSTEM    PIC  X(04).

       01  W-PROGRAM-TABLES.
           12  FILLER                  PIC  X(15)
                                       VALUE 'PROGRAM TABLES:'.
           12  W-RESEND-COUNTER-TABLE.
               16  W-RESEND-PENDING-CTR OCCURS 3 TIMES
                                INDEXED BY W-RS-NDX
                                       PIC S9(07) COMP-3.
                                       EJECT
       01  FILLER                      PIC  X(23)
                                       VALUE 'INTERFACE AREA STARTS:'.
           COPY ELCINTF.
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
      **********************************************************
      *    NOTE                                                *
      *        THE WORK AREA IS USED BY EL695 AND EL6952       *
      *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *
      *        BOTH PROGRAMS.                                  *
      **********************************************************
               16  PI-695-PRINT-DATE   PIC  X(08).
               16  PI-695-PRINT-DATE-BIN
                                       PIC  X(02).
               16  PI-695-PRINT-ID     PIC  X(04).
               16  PI-695-PRINT-KEY.
                   20  PI-695-PRINT-CARRIER
                                       PIC  X(01).
                   20  PI-695-PRINT-GROUPING
                                       PIC  X(06).
                   20  PI-695-PRINT-STATE
                                       PIC  X(02).
                   20  PI-695-PRINT-ACCOUNT
                                       PIC  X(10).
               16  PI-695-PRINT-PROCESSOR
                                       PIC  X(04).
               16  PI-695-ISSREF-TYPE  PIC  X(01).
               16  FILLER              PIC  X(602).
                                       EJECT
       01  FILLER                      PIC  X(15)
                                       VALUE 'MAP AREA START:'.
           COPY EL695S.
       01  FILLER                      PIC  X(14)
                                       VALUE 'MAP AREA ENDS:'.
                                       EJECT
       01  ERROR-MESSAGES.
           12  ER-ZEROS                PIC  9(05) VALUE 0000.
           12  ER-0000                 PIC  9(05) VALUE 0000.
           12  ER-0004                 PIC  9(05) VALUE 0004.
           12  ER-0008                 PIC  9(05) VALUE 0008.
           12  ER-0029                 PIC  9(05) VALUE 0029.
           12  ER-0042                 PIC  9(05) VALUE 0042.
           12  ER-0070                 PIC  9(05) VALUE 0070.
           12  ER-0172                 PIC  9(05) VALUE 0172.
           12  ER-0182                 PIC  9(05) VALUE 0182.
           12  ER-0189                 PIC  9(05) VALUE 0189.
           12  ER-0190                 PIC  9(05) VALUE 0190.
           12  ER-0409                 PIC  9(05) VALUE 0409.
           12  ER-0410                 PIC  9(05) VALUE 0410.
           12  ER-0411                 PIC  9(05) VALUE 0411.
           12  ER-0412                 PIC  9(05) VALUE 0412.
           12  ER-0413                 PIC  9(05) VALUE 0413.
           12  ER-0669                 PIC  9(05) VALUE 0669.
           12  ER-7357                 PIC  9(05) VALUE 7357.
           12  ER-7362                 PIC  9(05) VALUE 7362.
           12  ER-7382                 PIC  9(05) VALUE 7382.
           12  ER-7383                 PIC  9(05) VALUE 7383.
           12  ER-7397                 PIC  9(05) VALUE 7397.
           12  ER-9067                 PIC  9(05) VALUE 9067.
           12  ER-9097                 PIC  9(05) VALUE 9097.
           12  ER-9409                 PIC  9(05) VALUE 9409.

                                       EJECT
           COPY ELCAID.
       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC  X(08).
           12  PF-VALUES           PIC  X(01)  OCCURS 24 TIMES.
                                       EJECT
           COPY ELCATTR.
                                       EJECT
           COPY ELCDATE.
                                       EJECT
           COPY ELCLOGOF.
                                       EJECT
           COPY ELCEMIB.
                                       EJECT
           COPY ELCSCTM.
                                       EJECT
           COPY ELCSCRTY.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC  X(1024).

      *01 PARMLIST .
      *    02  FILLER              PIC S9(08)  COMP.
      *    02  W-ARCH-POINTER      PIC S9(08)  COMP.
      *    02  W-CNTL-POINTER      PIC S9(08)  COMP.
                                       EJECT
           COPY ELCCNTL.
                                       EJECT
       PROCEDURE DIVISION.

           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK

           MOVE 1                      TO EMI-NUMBER-OF-LINES

           IF EIBCALEN = 0
              MOVE UNACCESS-MSG        TO LOGOFF-MSG
              GO TO 8300-SEND-TEXT
           END-IF

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
           MOVE DC-BIN-DATE-1          TO W-CURRENT-DATE-SAVE
           MOVE DC-GREG-DATE-1-EDIT    TO W-CURRENT-DATE-EDIT

           IF PI-CALLING-PROGRAM NOT = W-THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = W-THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5
                                       TO PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4
                                       TO PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3
                                       TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2
                                       TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1
                                       TO PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM
                                       TO PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM
                                       TO PI-RETURN-TO-PROGRAM
                 MOVE W-THIS-PGM       TO PI-CALLING-PROGRAM
                 MOVE LOW-VALUES       TO EL695AO
                                          PI-PROGRAM-WORK-AREA
                 MOVE ZEROS            TO PI-695-PRINT-DATE
                 PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           EXEC CICS HANDLE CONDITION
               PGMIDERR (9700-PGMID-ERROR)
               ERROR    (9800-ABEND)
           END-EXEC.

           IF (EIBAID = DFHCLEAR) OR
              (NOT DISPLAY-CAP)
              MOVE PI-RETURN-TO-PROGRAM
                                       TO W-CALL-PGM
              GO TO 9400-XCTL
           END-IF

           .
       0200-RECEIVE.

           MOVE LOW-VALUES             TO EL695AI

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE ER-0008             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO OPTIONL
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               MAP       (W-MAP)
               MAPSET    (W-MAPSET)
               INTO      (EL695AI)
           END-EXEC

           IF ENTERPFL = 0
              GO TO 0300-CHECK-PFKEYS
           END-IF

           IF EIBAID NOT = DFHENTER
              MOVE ER-0004             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF

           IF (ENTERPFI NUMERIC) AND
              (ENTERPFI > 0) AND
              (ENTERPFI < 25)
              MOVE PF-VALUES (ENTERPFI)
                                       TO EIBAID
           ELSE
              MOVE ER-0029             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF

           .
       0300-CHECK-PFKEYS.

           IF EIBAID = DFHPF23
              MOVE EIBAID              TO PI-ENTRY-CD-1
              MOVE W-XCTL-005          TO W-CALL-PGM
              GO TO 9400-XCTL
           END-IF

           IF EIBAID = DFHPF24
              MOVE W-XCTL-626          TO W-CALL-PGM
              GO TO 9400-XCTL
           END-IF

           IF EIBAID = DFHPF12
              MOVE W-XCTL-010          TO W-CALL-PGM
              GO TO 9400-XCTL
           END-IF

           IF EIBAID = DFHENTER
              GO TO 0330-FUNCTION-CHECK
           END-IF

           MOVE ER-0029                TO EMI-ERROR

           .
       0320-INPUT-ERROR.

           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE AL-UNBON               TO ENTERPFA

           IF ENTERPFL = 0
              MOVE -1                  TO OPTIONL
           ELSE
              MOVE -1                  TO ENTERPFL
           END-IF

           GO TO 8200-SEND-DATAONLY

           .
       0330-FUNCTION-CHECK.

           PERFORM 0350-EDIT-ROUTINE   THRU 0350-EXIT

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE SPACES                 TO PI-695-PRINT-KEY
                                          PI-695-PRINT-PROCESSOR
                                          PI-695-ISSREF-TYPE
           IF  CARRL > +0
               MOVE CARRI              TO PI-695-PRINT-CARRIER
           END-IF

           IF  GROUPL > +0
               MOVE GROUPI             TO PI-695-PRINT-GROUPING
           END-IF

           IF  STATEL > +0
               MOVE STATEI             TO PI-695-PRINT-STATE
           END-IF

           IF  ACCTL > +0
               MOVE ACCTI              TO PI-695-PRINT-ACCOUNT
           END-IF

           IF  OVRPRCL > +0
               MOVE OVRPRCI            TO PI-695-PRINT-PROCESSOR
           END-IF

           IF ISSREFL > +0
              IF ISSREFI = 'I'
                 MOVE '1'              TO PI-695-ISSREF-TYPE
              ELSE
                 IF ISSREFI = 'R'
                    MOVE '2'           TO PI-695-ISSREF-TYPE
                 END-IF
              END-IF
           END-IF

           IF  OPTIONI EQUAL '1'
               GO TO 1000-PRINT
           END-IF

           IF  OPTIONI EQUAL '2'
               GO TO 2000-REPRINT
           END-IF

           .
       0350-EDIT-ROUTINE.

           IF OPTIONI < '1'
                   OR
              OPTIONI > '2'
              MOVE -1                  TO OPTIONL
              MOVE ER-0409             TO EMI-ERROR
              MOVE AL-UNBON            TO OPTIONA
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 0350-EXIT
           ELSE
              MOVE AL-UNNON            TO OPTIONA
           END-IF

           IF OPTIONI = '2'
              IF DATEINL > ZEROS
                 CONTINUE
              ELSE
                 MOVE ER-0410          TO EMI-ERROR
                 MOVE -1               TO DATEINL
                 MOVE AL-UABON         TO DATEINA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF DATEINL > ZEROS
              EXEC CICS BIF  DEEDIT
                   FIELD(DATEINI)
                   LENGTH(8)
              END-EXEC
              IF DATEINI = ZEROS
                 MOVE LOW-VALUES       TO PI-695-PRINT-DATE-BIN
              ELSE
                 MOVE DATEINI          TO DC-GREG-DATE-1-MDY
                 MOVE '4'              TO DC-OPTION-CODE
                 PERFORM 9500-LINK-DATE-CONVERT
                                       THRU 9500-EXIT
                 IF DATE-CONVERSION-ERROR
                    MOVE ER-0182       TO EMI-ERROR
                    MOVE -1            TO DATEINL
                    MOVE AL-UABON      TO DATEINA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 ELSE
                    MOVE AL-UANON      TO DATEINA
                    MOVE DC-BIN-DATE-1 TO PI-695-PRINT-DATE-BIN
                    MOVE DC-GREG-DATE-1-EDIT
                                       TO PI-695-PRINT-DATE
                                          DATEINI
                 END-IF
              END-IF
           ELSE
              MOVE LOW-VALUES          TO PI-695-PRINT-DATE-BIN
           END-IF

           IF ISSREFL > +0
              IF ISSREFI = 'R' OR  'I' OR ' '
                 MOVE AL-UANON         TO ISSREFA
              ELSE
                 MOVE ER-0669          TO EMI-ERROR
                 MOVE -1               TO ISSREFL
                 MOVE AL-UABON         TO ISSREFA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF

           IF NOT EMI-NO-ERRORS
              GO TO 0350-EXIT
           END-IF

           PERFORM 7900-GET-CNTL-INFO THRU 7999-EXIT

           .
       0350-EXIT.
            EXIT.
                                       EJECT
       1000-PRINT.

           MOVE '1'                    TO PI-ENTRY-CD-1
                                          PI-ENTRY-CD-2
           GO TO 7800-START-PRINT

           .
       2000-REPRINT.

           MOVE '1'                    TO PI-ENTRY-CD-1
           MOVE '2'                    TO PI-ENTRY-CD-2

           .
       7800-START-PRINT.

           MOVE PI-COMPANY-CD          TO W-ARCH-CO.

           EXEC CICS HANDLE CONDITION
                TERMIDERR   (8020-TERMID-ERROR)
                TRANSIDERR  (8030-TRANS-ERROR)
                END-EXEC.

       7820-START.

pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
               MOVE EIBTRMID       TO PI-695-PRINT-ID
               EXEC CICS START
                    INTERVAL    (0)
                    TRANSID     (W-PRINT-TRANS)
                    FROM        (PROGRAM-INTERFACE-BLOCK)
                    LENGTH      (PI-COMM-LENGTH)
      *             TERMID      (PI-695-PRINT-ID)
               END-EXEC
           ELSE
               EXEC CICS START
                    INTERVAL    (0)
                    TRANSID     (W-PRINT-TRANS)
                    FROM        (PROGRAM-INTERFACE-BLOCK)
                    LENGTH      (PI-COMM-LENGTH)
                    TERMID      (PI-695-PRINT-ID)
               END-EXEC.

           MOVE ER-0189                TO EMI-ERROR

           MOVE -1                     TO OPTIONL
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           GO TO 8200-SEND-DATAONLY
           .
       7900-GET-CNTL-INFO.

           MOVE SPACES                 TO PI-ALT-DMD-PRT-ID
           IF PRINTERL > +0
              MOVE PRINTERI            TO PI-695-PRINT-ID
                                          PI-ALT-DMD-PRT-ID
           ELSE
               IF (PI-PROCESSOR-PRINTER NOT EQUAL SPACES)
                       AND
                  (PI-PROCESSOR-PRINTER NOT EQUAL LOW-VALUES)
                   MOVE PI-PROCESSOR-PRINTER
                                       TO PI-695-PRINT-ID
               ELSE
                   EXEC CICS HANDLE CONDITION
                        NOTOPEN (8040-CNTL-NOT-OPEN)
                        NOTFND  (7990-NOT-FOUND)
                   END-EXEC
                   MOVE SPACES         TO W-CNTL-KEY
                   MOVE PI-COMPANY-ID  TO W-CNTL-COMPANY-ID
                   MOVE '1'            TO W-CNTL-RECORD-TYPE
                   MOVE ZEROS          TO W-CNTL-SEQ
                   PERFORM 7910-READ-CNTL-INFO THRU 7910-EXIT
                   MOVE CF-FORMS-PRINTER-ID
                                       TO PI-695-PRINT-ID
                   GO TO 7900-CONTINUE
               END-IF
           END-IF

           MOVE SPACES                 TO W-CNTL-KEY.
           MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
           MOVE '1'                    TO W-CNTL-RECORD-TYPE.
           MOVE ZEROS                  TO W-CNTL-SEQ.

           PERFORM 7910-READ-CNTL-INFO THRU 7910-EXIT.

       7900-CONTINUE.


           GO TO 7999-EXIT.

       7910-READ-CNTL-INFO.

           EXEC CICS READ
                DATASET (W-CNTL-ID)
                SET     (ADDRESS OF CONTROL-FILE)
                RIDFLD  (W-CNTL-KEY)
           END-EXEC.

       7910-EXIT.
           EXIT.

       7990-NOT-FOUND.

           MOVE ER-0190                TO EMI-ERROR.
           MOVE -1                     TO OPTIONL.
           PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.

       7999-EXIT.
           EXIT.
                                       EJECT
       8020-TERMID-ERROR.

           MOVE ER-0412                TO EMI-ERROR
           GO TO 8080-OPEN-ERROR.

       8030-TRANS-ERROR.

           MOVE ER-0413                TO EMI-ERROR
           GO TO 8080-OPEN-ERROR.

       8040-CNTL-NOT-OPEN.

           MOVE ER-0042                TO EMI-ERROR.
           GO TO 8080-OPEN-ERROR.


       8080-OPEN-ERROR.

           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                     TO OPTIONL.
           GO TO 8200-SEND-DATAONLY.
                                       EJECT
       8100-SEND-INITIAL-MAP.

           PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.

           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
           MOVE -1                     TO OPTIONL.

           EXEC CICS SEND
               MAP    (W-MAP)
               MAPSET (W-MAPSET)
               FROM   (EL695AO)
               ERASE
               FREEKB
               CURSOR
           END-EXEC.

           GO TO 9000-RETURN-TRANS.

       8100-EXIT.
           EXIT.
                                       EJECT
       8200-SEND-DATAONLY.

           PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.

           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.

           EXEC CICS SEND
               MAP    (W-MAP)
               MAPSET (W-MAPSET)
               FROM   (EL695AO)
               DATAONLY
               FREEKB
               CURSOR
               ERASEAUP
           END-EXEC.

           GO TO 9000-RETURN-TRANS.

       8200-EXIT.
           EXIT.
                                       EJECT
       8300-SEND-TEXT.

           EXEC CICS SEND TEXT
                FROM    (LOGOFF-TEXT)
                LENGTH  (LOGOFF-LENGTH)
                ERASE
                FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
                                       EJECT
       9000-RETURN-TRANS.

           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           MOVE W-MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.

           EXEC CICS RETURN
               TRANSID  (W-TRANSACTION)
               COMMAREA (PROGRAM-INTERFACE-BLOCK)
               LENGTH   (PI-COMM-LENGTH)
           END-EXEC.

       9000-EXIT.
           EXIT.
                                       EJECT
       9400-XCTL.

           EXEC CICS XCTL
               PROGRAM  (W-CALL-PGM)
               COMMAREA (PROGRAM-INTERFACE-BLOCK)
               LENGTH   (PI-COMM-LENGTH)
           END-EXEC.

       9400-EXIT.
           EXIT.
                                       EJECT
       9500-LINK-DATE-CONVERT.

           EXEC CICS LINK
                PROGRAM    ('ELDATCV')
                COMMAREA   (DATE-CONVERSION-DATA)
                LENGTH     (DC-COMM-LENGTH)
           END-EXEC.

       9500-EXIT.
           EXIT.
                                       EJECT
       9600-FORMAT-DATE-TIME.

           MOVE PI-COMPANY-ID          TO COMPANYO.
           MOVE W-CURRENT-DATE-EDIT    TO DATEO.

           EXEC CICS ASKTIME
           END-EXEC.

           MOVE EIBTIME                TO W-TIME-IN.
           MOVE W-TIME-OUT             TO TIMEO.
           MOVE W-MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.

       9600-EXIT.
           EXIT.
                                       EJECT
       9700-PGMID-ERROR.

           EXEC CICS  HANDLE CONDITION
               PGMIDERR  (8300-SEND-TEXT)
               END-EXEC.

           MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE W-XCTL-005             TO W-CALL-PGM
                                          LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.

           PERFORM 9400-XCTL THRU 9400-EXIT.

       9700-EXIT.
           EXIT.
                                       EJECT
       9900-ERROR-FORMAT.

           IF  NOT EMI-ERRORS-COMPLETE
                   AND
               EMI-ERROR NOT EQUAL W-LAST-ERROR
               MOVE W-LINK-001          TO W-CALL-PGM
               EXEC CICS LINK
                   PROGRAM    (W-CALL-PGM)
                   COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
                   LENGTH     (EMI-COMM-LENGTH)
               END-EXEC
               MOVE EMI-ERROR TO W-LAST-ERROR.

       9900-EXIT.
           EXIT.
                                       EJECT
       9800-ABEND.

           MOVE W-LINK-004             TO W-CALL-PGM.
           MOVE DFHEIBLK               TO EMI-LINE1

           EXEC CICS  LINK
               PROGRAM   (W-CALL-PGM)
               COMMAREA  (EMI-LINE1)
               LENGTH    (72)
               END-EXEC.

           GO TO 8200-SEND-DATAONLY

           .
       9800-EXIT.
           EXIT.
                                       EJECT
       9910-INITIALIZE-SECURITY.

           IF  PI-PROCESSOR-ID EQUAL 'LGXX'
               MOVE 'Y'                TO PI-DISPLAY-CAP
                                              PI-MODIFY-CAP
           ELSE
               EXEC CICS READQ TS
                   QUEUE  (PI-SECURITY-TEMP-STORE-ID)
                   INTO   (SECURITY-CONTROL)
                   LENGTH (SC-COMM-LENGTH)
                   ITEM   (1)
               END-EXEC
               MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
                                       TO PI-DISPLAY-CAP
               MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
                                       TO PI-MODIFY-CAP
               IF  NOT DISPLAY-CAP
                   MOVE 'READ'         TO SM-READ
                   PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                   MOVE ER-9097        TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   GO TO 8100-SEND-INITIAL-MAP
               END-IF
           .
       9910-EXIT.
           EXIT.
                                       EJECT
       9995-SECURITY-VIOLATION.

           MOVE EIBDATE          TO SM-JUL-DATE.
           MOVE EIBTRMID         TO SM-TERMID.
           MOVE W-THIS-PGM       TO SM-PGM.
           MOVE EIBTIME          TO W-TIME-IN.
           MOVE W-TIME-OUT       TO SM-TIME.
           MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.

           EXEC CICS LINK
                PROGRAM  ('EL003')
                COMMAREA (SECURITY-MESSAGE)
                LENGTH   (80)
           END-EXEC.

       9995-EXIT.
           EXIT.
                                       EJECT
       9999-GOBACK.

           GOBACK.

       9999-EXIT.
           EXIT.

