00001  IDENTIFICATION DIVISION.                                         03/18/96
00002                                                                   ELCMPCV1
00003  PROGRAM-ID.                 ELCMPCV1.                               LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 03/05/96 13:40:52.                    CL**2
00007 *                            VMOD=2.002.                             CL**2
00008 *                                                                 ELCMPCV1
00009 *AUTHOR.        LOGIC, INC.                                          CL**2
00010 *               DALLAS, TEXAS.                                       CL**2
00011 *                                                                 ELCMPCV1
00011                                                                   ELCMPCV1
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   ELCMPCV1
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   ELCMPCV1
00024 *REMARKS.                                                            CL**2
00025 *        CONVERT ERCCOMP TO EXPANDED VERSION (450 TO 700)            CL**2
00026                                                                   ELCMPCV1
00027  EJECT                                                            ELCMPCV1
00028  ENVIRONMENT DIVISION.                                            ELCMPCV1
00029  CONFIGURATION SECTION.                                           ELCMPCV1
00030  INPUT-OUTPUT SECTION.                                            ELCMPCV1
00031  FILE-CONTROL.                                                    ELCMPCV1
00032                                                                   ELCMPCV1
00033      SELECT COMP-TAPE-IN     ASSIGN TO INPUT-S-SYS010.            ELCMPCV1
00034                                                                   ELCMPCV1
00035      SELECT ERCOMP           ASSIGN TO SYS022-FBA1-ERCOMP         ELCMPCV1
00036                              ORGANIZATION IS INDEXED              ELCMPCV1
00037                              ACCESS IS DYNAMIC                    ELCMPCV1
00038                              RECORD KEY IS CO-CONTROL-PRIMARY     ELCMPCV1
00039                              FILE STATUS IS ERCOMP-FILE-STATUS.   ELCMPCV1
00040                                                                   ELCMPCV1
00041  EJECT                                                            ELCMPCV1
00042  DATA DIVISION.                                                   ELCMPCV1
00043  FILE SECTION.                                                    ELCMPCV1
00044                                                                   ELCMPCV1
00045      EJECT                                                        ELCMPCV1
00046  FD  COMP-TAPE-IN                                                 ELCMPCV1
00047      BLOCK CONTAINS 0 RECORDS
00048      RECORDING MODE F.                                               CL**2
00049  01  COMP-IN     PIC X(450).                                      ELCMPCV1
00050                                                                   ELCMPCV1
00051      EJECT                                                        ELCMPCV1
00052  FD  ERCOMP.                                                         CL**2
00053                                                                      CL**2
00054      COPY ERCCOMP.                                                ELCMPCV1
00055                                                                   ELCMPCV1
00056  EJECT                                                            ELCMPCV1
00057  WORKING-STORAGE SECTION.                                         ELCMPCV1
00058  77  FILLER  PIC X(32)  VALUE '********************************'. ELCMPCV1
00059  77  FILLER  PIC X(32)  VALUE '*   ELCMPCV1 WORKING-STORAGE    '. ELCMPCV1
00060  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.002 ************'.    CL**2
00061                                                                   ELCMPCV1
00062  77  COMP-IN-CNT               PIC S9(7)   COMP-3  VALUE +0.      ELCMPCV1
00063  77  COMP-OUT-CNT              PIC S9(7)   COMP-3  VALUE +0.      ELCMPCV1
00064                                                                   ELCMPCV1
00065  01  WS.                                                          ELCMPCV1
00066      12  ERCOMP-FILE-STATUS    PIC XX             VALUE ZERO.     ELCMPCV1
00067      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       ELCMPCV1
00068      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   ELCMPCV1
00069      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    ELCMPCV1
00070      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       ELCMPCV1
00071                                                                   ELCMPCV1
00072      EJECT                                                        ELCMPCV1
00073  PROCEDURE DIVISION.                                              ELCMPCV1
00074                                                                   ELCMPCV1
00075  0100-OPEN-FILES.                                                 ELCMPCV1
00076      OPEN INPUT  COMP-TAPE-IN                                     ELCMPCV1
00077            I-O   ERCOMP.                                          ELCMPCV1
00078                                                                   ELCMPCV1
00079      IF ERCOMP-FILE-STATUS   = '00' OR '97'                       ELCMPCV1
00080          NEXT SENTENCE                                            ELCMPCV1
00081        ELSE                                                       ELCMPCV1
00082          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCMPCV1
00083          MOVE 'ERCOMP OPEN ERR'  TO WS-ABEND-MESSAGE              ELCMPCV1
00084          GO TO ABEND-PGM.                                         ELCMPCV1
00085                                                                   ELCMPCV1
00086  0200-READ-ERCOMP.                                                ELCMPCV1
00087      READ COMP-TAPE-IN                                            ELCMPCV1
00088          AT END GO TO 9000-EOJ.                                   ELCMPCV1
00089                                                                   ELCMPCV1
00090      ADD +1 TO COMP-IN-CNT.                                       ELCMPCV1
00091                                                                   ELCMPCV1
00092      MOVE COMP-IN             TO COMPENSATION-MASTER.             ELCMPCV1
00093      MOVE LOW-VALUES          TO CO-GA-EFFECTIVE-DT               ELCMPCV1
00094                                  CO-GA-TERMINATION-DT.            ELCMPCV1
00095                                                                   ELCMPCV1
00096  0300-WRITE-EXPANDED-RCD.                                         ELCMPCV1
00097      WRITE COMPENSATION-MASTER.                                   ELCMPCV1
00098                                                                   ELCMPCV1
00099      IF ERCOMP-FILE-STATUS   = '00' OR '97'                       ELCMPCV1
00100          NEXT SENTENCE                                            ELCMPCV1
00101        ELSE                                                       ELCMPCV1
00102          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCMPCV1
00103          MOVE 'ERCOMP WRITE ERR' TO WS-ABEND-MESSAGE              ELCMPCV1
00104          GO TO ABEND-PGM.                                         ELCMPCV1
00105                                                                   ELCMPCV1
00106      ADD +1 TO COMP-OUT-CNT.                                      ELCMPCV1
00107                                                                   ELCMPCV1
00108      GO TO 0200-READ-ERCOMP.                                      ELCMPCV1
00109                                                                   ELCMPCV1
00110  1000-EXIT.                                                       ELCMPCV1
00111      EXIT.                                                        ELCMPCV1
00112                                                                   ELCMPCV1
00113      EJECT                                                        ELCMPCV1
00114  ABEND-PGM.                                                       ELCMPCV1
00115      COPY ELCABEND.                                               ELCMPCV1
00116                                                                   ELCMPCV1
00117  9000-EOJ.                                                        ELCMPCV1
00118      CLOSE COMP-TAPE-IN                                           ELCMPCV1
00119            ERCOMP.                                                ELCMPCV1
00120                                                                   ELCMPCV1
00121      IF ERCOMP-FILE-STATUS   = '00' OR '97'                       ELCMPCV1
00122          NEXT SENTENCE                                            ELCMPCV1
00123        ELSE                                                       ELCMPCV1
00124          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCMPCV1
00125          MOVE 'ERCOMP CLOSE ERR' TO WS-ABEND-MESSAGE              ELCMPCV1
00126          GO TO ABEND-PGM.                                         ELCMPCV1
00127                                                                   ELCMPCV1
00128      DISPLAY '*** COMP CONVERSION'.                               ELCMPCV1
00129      DISPLAY ' '.                                                 ELCMPCV1
00130      DISPLAY 'RECORDS IN  ' COMP-IN-CNT.                          ELCMPCV1
00131      DISPLAY 'RECORDS OUT ' COMP-OUT-CNT.                         ELCMPCV1
00132      GOBACK.                                                         CL**2
