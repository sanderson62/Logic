00001  IDENTIFICATION DIVISION.                                         08/12/92
00002                                                                   ELCNTCV1
00003  PROGRAM-ID.                 ELCNTCV1.                               LV001
00004 *                            VMOD=2.001.                          ELCNTCV1
00005                                                                   ELCNTCV1
00006  AUTHOR.        LOGIC, INC.                                       ELCNTCV1
00007                 DALLAS, TEXAS.                                    ELCNTCV1
00008                                                                   ELCNTCV1
00009  DATE-COMPILED.                                                   ELCNTCV1
00010                                                                   ELCNTCV1
00011  SECURITY.   *****************************************************ELCNTCV1
00012              *                                                   *ELCNTCV1
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELCNTCV1
00014              *                                                   *ELCNTCV1
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELCNTCV1
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELCNTCV1
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ELCNTCV1
00018              *                                                   *ELCNTCV1
00019              *****************************************************ELCNTCV1
00020                                                                   ELCNTCV1
00021  REMARKS.                                                         ELCNTCV1
00022          CONVERT ELCCNTL TO EXPANDED VERSION (504 TO 750)         ELCNTCV1
00023                                                                   ELCNTCV1
00024  EJECT                                                            ELCNTCV1
00025  ENVIRONMENT DIVISION.                                            ELCNTCV1
00026  CONFIGURATION SECTION.                                           ELCNTCV1
00027  INPUT-OUTPUT SECTION.                                            ELCNTCV1
00028  FILE-CONTROL.                                                    ELCNTCV1
00029                                                                   ELCNTCV1
00030      SELECT CONTROL-TAPE-IN  ASSIGN TO INPUT-S-SYS010.            ELCNTCV1
00031                                                                   ELCNTCV1
00032      SELECT CONTROL-FILE-OUT ASSIGN TO SYS022-FBA1-ELCNTLO        ELCNTCV1
00033                              ORGANIZATION IS INDEXED              ELCNTCV1
00034                              ACCESS IS DYNAMIC                    ELCNTCV1
00035                              RECORD KEY IS CF-CONTROL-PRIMARY     ELCNTCV1
00036                              FILE STATUS IS ELCNTL-FILE-STATUS.   ELCNTCV1
00037                                                                   ELCNTCV1
00038  EJECT                                                            ELCNTCV1
00039  DATA DIVISION.                                                   ELCNTCV1
00040  FILE SECTION.                                                    ELCNTCV1
00041                                                                   ELCNTCV1
00042      EJECT                                                        ELCNTCV1
00043  FD  CONTROL-TAPE-IN                                              ELCNTCV1
00044      LABEL RECORDS STANDARD                                       ELCNTCV1
00045      BLOCK CONTAINS 0 RECORDS
00046      RECORD CONTAINS 504 CHARACTERS.                              ELCNTCV1
00047  01  CNTL-IN     PIC X(504).                                      ELCNTCV1
00048                                                                   ELCNTCV1
00049      EJECT                                                        ELCNTCV1
00050  FD  CONTROL-FILE-OUT                                             ELCNTCV1
00051      LABEL RECORDS STANDARD                                       ELCNTCV1
00052      BLOCK CONTAINS 0 RECORDS
00053      RECORD CONTAINS 750 CHARACTERS.                              ELCNTCV1
00054      COPY ELCCNTL.                                                ELCNTCV1
00055                                                                   ELCNTCV1
00056  EJECT                                                            ELCNTCV1
00057  WORKING-STORAGE SECTION.                                         ELCNTCV1
00058  77  FILLER  PIC X(32)  VALUE '********************************'. ELCNTCV1
00059  77  FILLER  PIC X(32)  VALUE '*   ELCNTCV1 WORKING-STORAGE    '. ELCNTCV1
00060  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'. ELCNTCV1
00061                                                                   ELCNTCV1
00062  77  CNTL-IN-CNT               PIC S9(7)   COMP-3  VALUE +0.      ELCNTCV1
00063  77  CNTL-OUT-CNT              PIC S9(7)   COMP-3  VALUE +0.      ELCNTCV1
00064  77  PROCESSOR-CNT             PIC S9(7)   COMP-3  VALUE +0.      ELCNTCV1
00065  77  SLOT                      PIC S9(4)   COMP    VALUE +0.      ELCNTCV1
00066  77  MAXSLOT                   PIC S9(4)   COMP    VALUE +44.     ELCNTCV1
00067  77  SYS                       PIC S9(4)   COMP    VALUE +0.      ELCNTCV1
00068  77  MAXSYS                    PIC S9(4)   COMP    VALUE +4.      ELCNTCV1
00069                                                                   ELCNTCV1
00070  01  WS.                                                          ELCNTCV1
00071      12  ELCNTL-FILE-STATUS    PIC XX             VALUE ZERO.     ELCNTCV1
00072      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       ELCNTCV1
00073      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   ELCNTCV1
00074      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    ELCNTCV1
00075      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       ELCNTCV1
00076                                                                   ELCNTCV1
00077                                                                   ELCNTCV1
00078      EJECT                                                        ELCNTCV1
00079  PROCEDURE DIVISION.                                              ELCNTCV1
00080                                                                   ELCNTCV1
00081  0100-OPEN-FILES.                                                 ELCNTCV1
00082      OPEN INPUT  CONTROL-TAPE-IN                                  ELCNTCV1
00083           I-O    CONTROL-FILE-OUT.                                ELCNTCV1
00084                                                                   ELCNTCV1
00085      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       ELCNTCV1
00086          NEXT SENTENCE                                            ELCNTCV1
00087        ELSE                                                       ELCNTCV1
00088          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCNTCV1
00089          MOVE 'ELCNTL OPEN ERR'  TO WS-ABEND-MESSAGE              ELCNTCV1
00090          GO TO ABEND-PGM.                                         ELCNTCV1
00091                                                                   ELCNTCV1
00092  0200-READ-ELCNTL.                                                ELCNTCV1
00093      READ CONTROL-TAPE-IN                                         ELCNTCV1
00094          AT END GO TO 9000-EOJ.                                   ELCNTCV1
00095                                                                   ELCNTCV1
00096      ADD +1 TO CNTL-IN-CNT.                                       ELCNTCV1
00097                                                                   ELCNTCV1
00098      MOVE CNTL-IN             TO CONTROL-FILE.                    ELCNTCV1
00099                                                                   ELCNTCV1
00100  0300-WRITE-EXPANDED-RCD.                                         ELCNTCV1
00101      WRITE CONTROL-FILE.                                          ELCNTCV1
00102                                                                   ELCNTCV1
00103      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       ELCNTCV1
00104          NEXT SENTENCE                                            ELCNTCV1
00105        ELSE                                                       ELCNTCV1
00106          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCNTCV1
00107          MOVE 'ELCNTL WRITE ERR' TO WS-ABEND-MESSAGE              ELCNTCV1
00108          GO TO ABEND-PGM.                                         ELCNTCV1
00109                                                                   ELCNTCV1
00110      ADD +1 TO CNTL-OUT-CNT.                                      ELCNTCV1
00111                                                                   ELCNTCV1
00112      GO TO 0200-READ-ELCNTL.                                      ELCNTCV1
00113                                                                   ELCNTCV1
00114  1000-EXIT.                                                       ELCNTCV1
00115      EXIT.                                                        ELCNTCV1
00116                                                                   ELCNTCV1
00117      EJECT                                                        ELCNTCV1
00118  ABEND-PGM.                                                       ELCNTCV1
00119                                  COPY ELCABEND.                   ELCNTCV1
00120                                                                   ELCNTCV1
00121  9000-EOJ.                                                        ELCNTCV1
00122      CLOSE CONTROL-TAPE-IN                                        ELCNTCV1
00123            CONTROL-FILE-OUT.                                      ELCNTCV1
00124                                                                   ELCNTCV1
00125      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       ELCNTCV1
00126          NEXT SENTENCE                                            ELCNTCV1
00127        ELSE                                                       ELCNTCV1
00128          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCNTCV1
00129          MOVE 'ELCNTL CLOSE ERR' TO WS-ABEND-MESSAGE              ELCNTCV1
00130          GO TO ABEND-PGM.                                         ELCNTCV1
00131                                                                   ELCNTCV1
00132      DISPLAY '*** CNTL CONVERSION'.                               ELCNTCV1
00133      DISPLAY ' '.                                                 ELCNTCV1
00134      DISPLAY 'RECORDS IN  ' CNTL-IN-CNT.                          ELCNTCV1
00135      DISPLAY 'RECORDS OUT ' CNTL-OUT-CNT.                         ELCNTCV1
00136      GOBACK.                                                      ELCNTCV1
