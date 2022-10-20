00001  ID DIVISION.                                                     09/25/97
00002                                                                   EL556
00003  PROGRAM-ID.                 EL556.                                  LV001
00004 *              PROGRAM CONVERTED BY                               EL556
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL556
00006 *              CONVERSION DATE 02/12/96 16:50:21.                 EL556
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL556
00008 *                            VMOD=2.004                           EL556
00009                                                                   EL556
00010 *AUTHOR.     LOGIC, INC.                                          EL556
00011 *            DALLAS, TEXAS.                                       EL556
00012                                                                   EL556
00013 *DATE-COMPILED.                                                   EL556
00014                                                                   EL556
00015 *SECURITY.   *****************************************************EL556
00016 *            *                                                   *EL556
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL556
00018 *            *                                                   *EL556
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL556
00020 *                                                                *EL556
00021 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL556
00022 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL556
00023 *            *                                                   *EL556
00024 *            *****************************************************EL556
00025                                                                   EL556
00026 *REMARKS. MOVE PRIMARY KEY TO NEW ALT KEY.                        EL556
00027                                                                   EL556
00028  ENVIRONMENT DIVISION.                                            EL556
00029                                                                   EL556
00030  INPUT-OUTPUT SECTION.                                            EL556
00031                                                                   EL556
00032  FILE-CONTROL.                                                    EL556
00033                                                                   EL556
00034      SELECT ERPNDB-FILE      ASSIGN SYS024-FBA1-SYS024            EL556
00035                              ORGANIZATION IS INDEXED              EL556
00036                              ACCESS IS SEQUENTIAL                 EL556
00037                              RECORD KEY IS PB-CONTROL-PRIMARY     EL556
00038                              FILE STATUS IS ERPNDB-STATUS.        EL556
00039                                                                   EL556
00040  EJECT                                                            EL556
00041  DATA DIVISION.                                                   EL556
00042                                                                   EL556
00043  FILE SECTION.                                                    EL556
00044                                                                   EL556
00045  FD  ERPNDB-FILE.                                                 EL556
00046                             COPY ERCPNDB.                         EL556
00047  EJECT                                                            EL556
00048  WORKING-STORAGE SECTION.                                         EL556
00049  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL556
00050                                                                   EL556
00051  77  FILLER  PIC X(32)  VALUE '********************************'. EL556
00052  77  FILLER  PIC X(32)  VALUE '* EL556  WORKING STORAGE       *'. EL556
00053  77  FILLER  PIC X(32)  VALUE '*******VMOD=2.004 **************'. EL556
00054                                                                   EL556
00055  77  ERPNDB-STATUS        PIC XX        VALUE 'ZZ'.               EL556
00056  77  WS-TOT-REWRITTEN     PIC S9(7)     COMP-3  VALUE +0.         EL556
00057  77  WS-TOT-READ          PIC S9(7)     COMP-3  VALUE +0.         EL556
00058  77  WS-SUB               PIC S99       COMP-3  VALUE +0.         EL556
00059  77  WS-ZERO              PIC S9        COMP-3  VALUE +0.         EL556
00060  77  WS-RETURN-CODE       PIC S999      COMP-3  VALUE +0.         EL556
00061  77  WS-ABEND-MESSAGE     PIC X(80)             VALUE SPACES.     EL556
00062  77  WS-ABEND-FILE-STATUS PIC XX                VALUE ZERO.       EL556
00063                                                                   EL556
00064  EJECT                                                            EL556
00065  PROCEDURE DIVISION.                                              EL556
00066                                                                   EL556
00067  0100-OPEN-FILES.                                                 EL556
00068      OPEN I-O   ERPNDB-FILE.                                      EL556
00069                                                                   EL556
00070      IF ERPNDB-STATUS NOT = ZERO AND '97'                         EL556
00071          MOVE ERPNDB-STATUS     TO WS-ABEND-FILE-STATUS           EL556
00072          MOVE 'VSAM OPEN ERROR - PNDB FILE' TO WS-ABEND-MESSAGE   EL556
00073          GO TO 9900-ABEND-RTN.                                    EL556
00074                                                                   EL556
00075  0300-PROCESS-PNDB-FILE.                                          EL556
00076      MOVE LOW-VALUES TO PB-CONTROL-PRIMARY.                       EL556
00077                                                                   EL556
00078      START ERPNDB-FILE                                            EL556
00079           KEY IS GREATER THAN PB-CONTROL-PRIMARY.                 EL556
00080                                                                   EL556
00081      IF ERPNDB-STATUS NOT = ZERO                                  EL556
00082          MOVE ERPNDB-STATUS TO WS-ABEND-FILE-STATUS               EL556
00083          MOVE 'VSAM START ERROR - PNDB FILE' TO WS-ABEND-MESSAGE  EL556
00084          GO TO 9900-ABEND-RTN.                                    EL556
00085                                                                   EL556
00086  0400-READ-PNDB-FILE.                                             EL556
00087      READ ERPNDB-FILE NEXT.                                       EL556
00088                                                                   EL556
00089      IF ERPNDB-STATUS = '10'                                      EL556
00090          GO TO 0500-END-OF-JOB.                                   EL556
00091                                                                   EL556
00092      IF ERPNDB-STATUS NOT = ZERO                                  EL556
00093          MOVE ERPNDB-STATUS TO WS-ABEND-FILE-STATUS               EL556
00094          MOVE 'VSAM READ ERROR - PNDB FILE' TO WS-ABEND-MESSAGE   EL556
00095          GO TO 9900-ABEND-RTN.                                    EL556
00096                                                                   EL556
00097      ADD +1 TO WS-TOT-READ.                                       EL556
00098                                                                   EL556
00099      MOVE PB-CONTROL-PRIMARY    TO PB-CONTROL-BY-ORIG-BATCH.      EL556
00100                                                                   EL556
00101  0470-REWRITE.                                                    EL556
00102      REWRITE PENDING-BUSINESS.                                    EL556
00103                                                                   EL556
00104      IF ERPNDB-STATUS NOT = ZERO                                  EL556
00105          MOVE ERPNDB-STATUS TO WS-ABEND-FILE-STATUS               EL556
00106          MOVE 'VSAM REWRITE ERROR- PNDB FILE' TO WS-ABEND-MESSAGE EL556
00107          GO TO 9900-ABEND-RTN.                                    EL556
00108                                                                   EL556
00109      ADD +1 TO WS-TOT-REWRITTEN.                                  EL556
00110                                                                   EL556
00111      GO TO 0400-READ-PNDB-FILE.                                   EL556
00112                                                                   EL556
00113  0500-END-OF-JOB.                                                 EL556
00114      DISPLAY WS-TOT-REWRITTEN.                                    EL556
00115      DISPLAY WS-TOT-READ.                                         EL556
00116                                                                   EL556
00117      CLOSE  ERPNDB-FILE.                                          EL556
00118                                                                   EL556
00119      GOBACK.                                                      EL556
00120                                                                   EL556
00121  9900-ABEND-RTN.                                                  EL556
00122         COPY ELCABEND.                                            EL556
