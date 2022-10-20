00001  ID DIVISION.                                                     02/26/96
00002                                                                   EL005
00003  PROGRAM-ID.                 EL005.                                  LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 09:24:19.                    CL**3
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL005
00008 *                                                                 EL005
00009 *AUTHOR.     LOGIC INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL005
00012 *DATE-COMPILED.                                                      CL**3
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00020 *            *                                                   *   CL**3
00021 *            *****************************************************   CL**3
00022 *REMARKS. COMMON    EXIT ROUTINE.                                    CL**3
00023      EJECT                                                        EL005
00024  ENVIRONMENT DIVISION.                                            EL005
00025  DATA DIVISION.                                                   EL005
00026  WORKING-STORAGE SECTION.                                         EL005
00027  77  FILLER  PIC X(32)  VALUE '********************************'. EL005
00028  77  FILLER  PIC X(32)  VALUE '*   EL005  WORKING STORAGE     *'. EL005
00029  77  FILLER   PIC X(32) VALUE '******** VMOD=2.003 ************'.    CL**3
00030                                                                   EL005
00031  01  EDIT-WORK-AREA.                                              EL005
00032      12  LIT-TWO                 PIC X       VALUE '2'.           EL005
00033      12  QUEST-MARK              PIC X       VALUE '?'.           EL005
00034      12  USER-LGXX               PIC X(4)    VALUE 'LGXX'.        EL005
00035      12  FILE-ID                 PIC X(8)    VALUE SPACE.         EL005
00036      12  ELCNTL-ID               PIC X(8)    VALUE 'ELCNTL'.      EL005
00037                                                                   EL005
00038  01  FILE-READ-KEY.                                               EL005
00039      12  COMPANY-ID              PIC X(3).                        EL005
00040      12  RECORD-TYPE             PIC X.                           EL005
00041      12  ACCESS-CD-GENL          PIC X(4).                        EL005
00042      12  SEQUENCE-NO             PIC 9(4)    COMP.                EL005
00043                                                                   EL005
00044      COPY ELCLOGOF SUPPRESS.                                         CL**3
00045                                                                   EL005
00046  01  TEXT-MESSAGES.                                               EL005
00047      12  CANCEL-MSG              PIC X(14)                        EL005
00048          VALUE 'LOGON CANCELED'.                                  EL005
00049      12  NOT-OPEN-MSG            PIC X(39)                        EL005
00050          VALUE 'SYSTEM FILES NOT OPEN - LOGON CANCELLED'.         EL005
00051      12  PF23-MSG.                                                EL005
00052          16  FILLER              PIC X(12)                        EL005
00053            VALUE 'PF23 ENTERED'.                                  EL005
CIDMOD     12  PF24-MSG.                                                EL005
CIDMOD         16  FILLER              PIC X(12)                        EL005
CIDMOD           VALUE 'PF24 ENTERED'.                                  EL005
00054      12  CLEAR-MSG.                                               EL005
00055          16  FILLER              PIC X(13)                        EL005
00056            VALUE 'CLEAR ENTERED'.                                 EL005
00057      12  NO-MSG.                                                  EL005
00058          16  FILLER              PIC X(37)                        EL005
00059            VALUE 'LOGOFF REASON CODE MISSING OR INVALID'.         EL005
00060          16  FILLER              PIC X(8)                         EL005
00061            VALUE 'PROGRAM '.                                      EL005
00062      12  LOGON-ERROR-MSG.                                         EL005
00063          16  FILLER              PIC X(35)                        EL005
00064            VALUE 'USER ALREADY SIGNED ON TO TERMINAL '.           EL005
00065          16  TERM-ID             PIC X(4).                        EL005
00066          16  FILLER              PIC X(17)                        EL005
00067              VALUE ' - LOGON CANCELED'.                           EL005
00068      EJECT                                                        EL005
00069      COPY ELCINTF.                                                   CL**3
00070      EJECT                                                        EL005
00071      COPY ELCAID.                                                    CL**3
00072      EJECT                                                        EL005
00073  LINKAGE SECTION.                                                 EL005
00074  01  DFHCOMMAREA                 PIC X(1024).                     EL005
00075                                                                   EL005
00076 *01 PARM-LIST .                                                      CL**3
00077 *    12  FILLER                  PIC S9(8)   COMP.                   CL**3
00078 *    12  CTRL-PNT                PIC S9(8)   COMP.                   CL**3
00079      EJECT                                                        EL005
00080      COPY ELCCNTL.                                                   CL**3
00081      EJECT                                                        EL005
00082  PROCEDURE DIVISION.                                              EL005
00083      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL005
00084                                                                   EL005
00085      IF CREDIT-SESSION                                            EL005
00086          MOVE 'S CREDIT SYSTEM'   TO LOGOFF-SYS-MSG.              EL005
00087                                                                   EL005
00088      IF PI-ENTRY-CD-1 = LOW-VALUES                                EL005
00089          GO TO 8800-CANCEL-LOGON.                                 EL005
00090                                                                   EL005
00091      IF PI-ENTRY-CD-1 = HIGH-VALUES                               EL005
00092          GO TO 8850-NOT-OPEN.                                     EL005
00093                                                                   EL005
00094      IF PI-ENTRY-CD-1 = QUEST-MARK                                EL005
00095          GO TO 8860-LOGON-ERROR.                                  EL005
00096                                                                   EL005
00097      PERFORM 1000-UPDATE-CNTL THRU 1010-UPDATE-CNTL-EXIT.         EL005
00098                                                                   EL005
00099      IF PI-ENTRY-CD-1 = SPACE                                     EL005
00100          GO TO 8810-PGMIDERR.                                     EL005
00101                                                                   EL005
00102      IF PI-ENTRY-CD-1 = DFHPF23                                   EL005
00103          GO TO 8820-PF23-ENTERED.                                 EL005
00104                                                                   EL005
CIDMOD     IF PI-ENTRY-CD-1 = DFHPF24                                   EL005
CIDMOD         GO TO 8825-PF24-ENTERED.                                 EL005
CIDMOD                                                                  EL005
00105      IF PI-ENTRY-CD-1 = DFHCLEAR                                  EL005
00106          GO TO 8830-CLEAR-ENTERED.                                EL005
00107                                                                   EL005
00108      GO TO 8840-NO-MSG.                                           EL005
00109      EJECT                                                        EL005
00110  1000-UPDATE-CNTL.                                                EL005
00111                                                                   EL005
00112      IF PI-PROCESSOR-ID = USER-LGXX                               EL005
00113          GO TO 1010-UPDATE-CNTL-EXIT.                             EL005
00114                                                                   EL005
00115      EXEC CICS HANDLE CONDITION                                   EL005
00116          NOTOPEN (8850-NOT-OPEN)                                  EL005
00117      END-EXEC.                                                       CL**2
00118                                                                   EL005
00119      MOVE PI-COMPANY-ID          TO COMPANY-ID.                   EL005
00120      MOVE LIT-TWO                TO RECORD-TYPE.                  EL005
00121      MOVE PI-PROCESSOR-ID        TO ACCESS-CD-GENL.               EL005
00122      MOVE ZERO                   TO SEQUENCE-NO.                  EL005
00123                                                                   EL005
00124      EXEC CICS READ                                               EL005
00125          DATASET  ('ELCNTL')                                      EL005
00126          RIDFLD   (FILE-READ-KEY)                                 EL005
00127          SET      (ADDRESS OF CONTROL-FILE)                          CL**3
00128          UPDATE                                                   EL005
00129      END-EXEC.                                                    EL005
00130                                                                   EL005
00131      MOVE SPACES                 TO CF-CURRENT-TERM-ON.           EL005
00132                                                                   EL005
00133      EXEC CICS REWRITE                                            EL005
00134          FROM     (CONTROL-FILE)                                  EL005
00135          DATASET  ('ELCNTL')                                      EL005
00136      END-EXEC.                                                    EL005
00137                                                                   EL005
00138      GO TO 1010-UPDATE-CNTL-EXIT.                                 EL005
00139                                                                   EL005
00140                                                                   EL005
00141  1010-UPDATE-CNTL-EXIT.                                           EL005
00142      EXIT.                                                        EL005
00143      EJECT                                                        EL005
00144  8800-CANCEL-LOGON.                                               EL005
00145      MOVE CANCEL-MSG             TO LOGOFF-MSG.                   EL005
00146      GO TO 8990-SEND-TEXT.                                        EL005
00147                                                                   EL005
00148  8810-PGMIDERR.                                                   EL005
00149      MOVE PI-CALLING-PROGRAM     TO LOGOFF-PGM.                   EL005
00150      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL005
00151      GO TO 8990-SEND-TEXT.                                        EL005
00152                                                                   EL005
00153  8820-PF23-ENTERED.                                               EL005
00154      MOVE PF23-MSG               TO LOGOFF-MSG.                   EL005
00155      GO TO 8990-SEND-TEXT.                                        EL005
00156                                                                   EL005
CIDMOD 8825-PF24-ENTERED.                                               EL005
CIDMOD     MOVE PF24-MSG               TO LOGOFF-MSG.                   EL005
CIDMOD     GO TO 8990-SEND-TEXT.                                        EL005
CIDMOD                                                                  EL005
00157  8830-CLEAR-ENTERED.                                              EL005
00158      MOVE CLEAR-MSG              TO LOGOFF-MSG.                   EL005
00159      GO TO 8990-SEND-TEXT.                                        EL005
00160                                                                   EL005
00161  8840-NO-MSG.                                                     EL005
00162      MOVE PI-CALLING-PROGRAM     TO LOGOFF-PGM.                   EL005
00163      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL005
00164      GO TO 8990-SEND-TEXT.                                        EL005
00165                                                                   EL005
00166  8850-NOT-OPEN.                                                   EL005
00167      MOVE NOT-OPEN-MSG           TO LOGOFF-MSG.                   EL005
00168      GO TO 8990-SEND-TEXT.                                        EL005
00169                                                                   EL005
00170  8860-LOGON-ERROR.                                                EL005
00171      MOVE PI-CALLING-PROGRAM     TO TERM-ID.                      EL005
00172      MOVE LOGON-ERROR-MSG        TO LOGOFF-MSG.                   EL005
00173                                                                   EL005
00174  8990-SEND-TEXT.                                                  EL005
00175      EXEC CICS SEND TEXT                                          EL005
00176          FROM    (LOGOFF-TEXT)                                    EL005
00177          LENGTH  (LOGOFF-LENGTH)                                  EL005
00178          ERASE                                                    EL005
00179          FREEKB                                                   EL005
00180      END-EXEC.                                                    EL005
00181                                                                   EL005
00182  9000-RETURN-TO-CICS.                                             EL005
00183      EXEC CICS RETURN                                             EL005
00184      END-EXEC.                                                    EL005
00185      GOBACK.                                                      EL005
