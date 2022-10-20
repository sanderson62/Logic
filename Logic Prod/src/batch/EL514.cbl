00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL514
00003  PROGRAM-ID.                 EL514 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL514
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL514
00006 *              CONVERSION DATE 02/22/96 14:21:17.                 EL514
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL514
00008 *                            VMOD=2.006                           EL514
00009                                                                   EL514
00010 *AUTHOR.     LOGIC INC.                                           EL514
00011 *            DALLAS, TEXAS.                                       EL514
00012                                                                   EL514
00013 *DATE-COMPILED.                                                   EL514
00014                                                                   EL514
00015 *SECURITY.   *****************************************************EL514
00016 *            *                                                   *EL514
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL514
00018 *            *                                                   *EL514
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL514
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL514
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL514
00022 *            *                                                   *EL514
00023 *            *****************************************************EL514
00024                                                                   EL514
00025 *REMARKS.                                                         EL514
00026 *       GENERAL FUNCTION IS TO PRODUCE THE PREMIUM BALANCING      EL514
00027 *       SUMMARY REPORT FROM THE PENDING BUSINESS FILE.            EL514
00028                                                                   EL514
00029      EJECT                                                        EL514
00030  ENVIRONMENT DIVISION.                                            EL514
00031                                                                   EL514
00032  INPUT-OUTPUT SECTION.                                            EL514
00033                                                                   EL514
00034  FILE-CONTROL.                                                    EL514
00035                                                                   EL514
00036      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL514
00037                                                                   EL514
00038      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL514
00039                                                                   EL514
00040      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL514
00041                                                                   EL514
00042      SELECT ERPNDB           ASSIGN TO SYS021-FBA1-ERPNDB2        EL514
00043                              ORGANIZATION IS INDEXED              EL514
00044                              ACCESS IS SEQUENTIAL                 EL514
00045                              RECORD KEY IS PB-ALTERNATE-KEY       EL514
00046                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL514
00047                                                                   EL514
00048      SELECT ELREPT           ASSIGN TO SYS023-FBA1-ELREPT         EL514
00049                              ORGANIZATION IS INDEXED              EL514
00050                              ACCESS IS DYNAMIC                    EL514
00051                              RECORD KEY IS RF-CONTROL-PRIMARY     EL514
00052                              FILE STATUS IS DTE-VSAM-FLAGS.       EL514
00053                                                                   EL514
00054      SELECT SORT-FILE        ASSIGN TO SYS001-UT-2314-S-SORTWK1.  EL514
00055                                                                   EL514
00056  DATA DIVISION.                                                   EL514
00057                                                                   EL514
00058  FILE SECTION.                                                    EL514
00059                                                                   EL514
00060  FD  PRNTR                   COPY ELCPRTFD.                       EL514
00061                                                                   EL514
00062  FD  DISK-DATE               COPY ELCDTEFD.                          CL**2
00063                                                                   EL514
00064  FD  FICH                    COPY ELCFCHFD.                       EL514
00065                                                                   EL514
00066  FD  ERPNDB.                                                      EL514
00067                                                                   EL514
00068  01  PENDING-BUSINESS-RECORD.                                     EL514
00069      16  FILLER                      PIC X(13).                   EL514
00070      16  PB-ALTERNATE-KEY            PIC X(36).                   EL514
00071      16  FILLER                      PIC X(536).                  EL514
00072      EJECT                                                        EL514
00073                                                                   EL514
00074  FD  ELREPT                  COPY ELCRPTFD.                       EL514
00075                                                                   EL514
00076                              COPY ELCREPT.                        EL514
00077                                                                   EL514
00078  SD  SORT-FILE.                                                   EL514
00079                                                                   EL514
00080  01  SORT-RECORD.                                                 EL514
00081      05  SR-CONTROL.                                              EL514
00082          10  SR-COMPARE.                                          EL514
00083              15  SR-COMPANY          PIC X.                       EL514
00084              15  SR-CARRIER          PIC X.                       EL514
00085              15  SR-GROUP            PIC X(6).                    EL514
00086              15  SR-STATE            PIC XX.                      EL514
00087              15  SR-ACCOUNT          PIC X(10).                   EL514
00088              15  SR-BATCH-NO         PIC X(6).                    EL514
00089          10  SR-SEQ-NO               PIC S9(4) COMP.              EL514
00090                                                                   EL514
00091      05  SR-REST-OF-RECORD           PIC X(585).                  EL514
00092                                                                   EL514
00093      EJECT                                                        EL514
00094  WORKING-STORAGE SECTION.                                         EL514
00095  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL514
00096                                                                   EL514
00097  77  FILLER  PIC X(32) VALUE '********************************'.  EL514
00098  77  FILLER  PIC X(32) VALUE '      EL514 WORKING-STORAGE     '.  EL514
00099  77  FILLER  PIC X(32) VALUE '************ V/M 2.006 *********'.  EL514
00100                                                                   EL514
00101  01  FILLER                          COMP-3.                      EL514
00102      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL514
00103      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL514
00104      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +58.       EL514
00105      05  WS-ZERO                     PIC S9      VALUE ZERO.      EL514
00106      05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL514
00107                                                                   EL514
00108  01  FILLER.                                                      EL514
00109      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL514
00110      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL514
00111      05  ERPNDB-FILE-STATUS          PIC XX      VALUE ZERO.      EL514
00112      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL514
00113                                                                   EL514
00114      05  WS-SAVE-PRINT-RECORD        PIC X(133) VALUE SPACES.     EL514
00115                                                                   EL514
00116      05  WS-SAVE-KEY.                                             EL514
00117          10  WS-SAVE-COMPANY-CD      PIC X      VALUE SPACES.     EL514
00118          10  WS-SAVE-CARRIER         PIC X      VALUE SPACES.     EL514
00119          10  WS-SAVE-GROUPING        PIC X(6)   VALUE SPACES.     EL514
00120          10  WS-SAVE-STATE           PIC XX     VALUE SPACES.     EL514
00121          10  WS-SAVE-ACCOUNT         PIC X(10)  VALUE SPACES.     EL514
00122          10  WS-SAVE-BATCH           PIC X(6)   VALUE SPACES.     EL514
00123                                                                   EL514
00124      05  X                           PIC X     VALUE SPACES.      EL514
00125      05  WS-SAVE-DATE                PIC XX.                      EL514
00126      05  WS-SAVE-TIME                PIC 9(6).                    EL514
00127      05  WS-DASH                     PIC XX    VALUE ' -'.        EL514
00128      05  PGM-SUB                     PIC S9(4) COMP  VALUE +514.  EL514
00129      05  ABEND-CODE                  PIC XXXX  VALUE SPACES.      EL514
00130      05  ABEND-OPTION                PIC X     VALUE SPACES.      EL514
00131      05  OLC-REPORT-NAME             PIC X(5)  VALUE 'EL514'.     EL514
00132                                                                   EL514
00133      EJECT                                                        EL514
00134                                      COPY ELCDATE.                   CL**6
00135                                                                   EL514
00136                                      COPY ELCDTECX.                  CL**2
00137                                                                      CL**4
00138                                      COPY ELCDTEVR.                  CL**4
00139      EJECT                                                        EL514
00140                                      COPY ERCPNDB.                EL514
00141      EJECT                                                        EL514
00142                                                                   EL514
00143  01  WS-HEADING1.                                                 EL514
00144      05  FILLER                      PIC X(50) VALUE '1'.         EL514
00145      05  FILLER                      PIC X(25) VALUE              EL514
00146          'PREMIUM BALANCING SUMMARY'.                             EL514
00147      05  FILLER                      PIC X(46) VALUE SPACES.      EL514
00148      05  FILLER                      PIC X(8)  VALUE ' EL514'.    EL514
00149      05  FILLER                      PIC X(4) VALUE SPACES.       EL514
00150                                                                   EL514
00151  01  WS-HEADING2.                                                 EL514
00152      05  FILLER                      PIC X(46) VALUE SPACES.      EL514
00153      05  WS-H2-CLIENT-NAME           PIC X(30) VALUE SPACES.      EL514
00154      05  FILLER                      PIC X(49) VALUE SPACES.      EL514
00155      05  WS-H2-DATE                  PIC X(8)  VALUE SPACES.      EL514
00156                                                                   EL514
00157  01  WS-HEADING3.                                                 EL514
00158      05  FILLER                      PIC X(53) VALUE SPACES.      EL514
00159      05  WS-H3-DATE                  PIC X(58) VALUE SPACES.      EL514
00160      05  FILLER                      PIC X(5)  VALUE 'PAGE'.      EL514
00161      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL514
00162      05  FILLER                      PIC X(11) VALUE SPACES.      EL514
00163                                                                   EL514
00164  01  WS-HEADING4.                                                 EL514
00165      05  FILLER                      PIC X     VALUE  '0'.        EL514
00166      05  FILLER                      PIC X(55) VALUE  SPACE.      EL514
00167      05  FILLER                      PIC X(5)  VALUE  'ISSUE'.    EL514
00168      05  FILLER                      PIC X(6)  VALUE  SPACES.     EL514
00169      05  HD4-LF-OVRD1                PIC X(6)  VALUE  SPACES.     EL514
00170      05  FILLER                      PIC X(8)  VALUE  SPACES.     EL514
00171      05  HD4-AH-OVRD1                PIC X(6)  VALUE  SPACES.     EL514
00172      05  FILLER                      PIC XXX   VALUE  SPACES.     EL514
00173      05  FILLER                      PIC X(6)  VALUE  'CANCEL'.   EL514
00174      05  FILLER                      PIC X(3)  VALUE  SPACES.     EL514
00175      05  HD4-LF-OVRD2                PIC X(6)  VALUE  SPACES.     EL514
00176      05  FILLER                      PIC X(9)  VALUE  SPACES.     EL514
00177      05  HD4-AH-OVRD2                PIC X(6)  VALUE  SPACES.     EL514
00178      05  FILLER                      PIC XXX   VALUE  SPACES.     EL514
00179      05  FILLER                   PIC X(10) VALUE 'COMMISSION'.   EL514
00180                                                                   EL514
00181  01  WS-HEADING5.                                                 EL514
00182      05  FILLER                      PIC X(42)                    EL514
00183          VALUE ' CARR  GROUP ST   ACCOUNT   BATCH  ENTERED'.      EL514
00184      05  FILLER                      PIC X(7)  VALUE SPACES.      EL514
00185      05  FILLER                      PIC X(54) VALUE              EL514
00186         '       COUNT     PREMIUM       PREMIUM    COUNT   REFU'. EL514
00187      05  FILLER                      PIC X(30) VALUE              EL514
00188          'ND        REFUND      ON NET  '.                        EL514
00189      EJECT                                                        EL514
00190  01  WS-DETAIL.                                                   EL514
00191      05  FILLER                      PIC XX    VALUE SPACE.       EL514
00192      05  DTL-CARR                    PIC X.                       EL514
00193      05  FILLER                      PIC XX    VALUE SPACE.       EL514
00194      05  DTL-GRP                     PIC X(6).                    EL514
00195      05  FILLER                      PIC X     VALUE SPACE.       EL514
00196      05  DTL-STATE                   PIC XX.                      EL514
00197      05  FILLER                      PIC X     VALUE SPACE.       EL514
00198      05  DTL-ACCT                    PIC X(10).                   EL514
00199      05  FILLER                      PIC X     VALUE SPACE.       EL514
00200      05  DTL-BATCH                   PIC X(6).                    EL514
00201      05  FILLER                      PIC X     VALUE SPACE.       EL514
00202      05  DTL-DT-ENTERED              PIC X(8).                    EL514
00203      05  FILLER                      PIC X     VALUE SPACE.       EL514
00204      05  DTL-MSG                     PIC X(10).                   EL514
00205      05  DTL-DASH                    PIC XX    VALUE ' -'.        EL514
00206      05  DTL-ISS-CNT                 PIC Z(5).                    EL514
00207      05  FILLER                      PIC X     VALUE SPACE.       EL514
00208      05  DTL-LF-PREM                 PIC ZZ,ZZZ,ZZZ.99-.          EL514
00209      05  DTL-AH-PREM                 PIC ZZ,ZZZ,ZZZ.99-.          EL514
00210      05  DTL-CAN-CNT                 PIC Z(4).                    EL514
00211      05  DTL-LF-REF                  PIC ZZ,ZZZ,ZZZ.99-.          EL514
00212      05  DTL-AH-REF                  PIC ZZ,ZZZ,ZZZ.99-.          EL514
00213      05  FILLER                      PIC X     VALUE SPACE.       EL514
00214      05  DTL-COMM                    PIC ZZZ,ZZZ.99-.             EL514
00215                                                                   EL514
00216  01  WS-ACCOUNT-TOTAL.                                            EL514
00217      05  FILLER                      PIC XXX   VALUE SPACES.      EL514
00218      05  FILLER                      PIC X(8)  VALUE 'ACCOUNT '.  EL514
00219      05  ACCT-NUM                    PIC X(10) VALUE SPACES.      EL514
00220      05  FILLER                      PIC X(6)  VALUE ' TOTAL'.    EL514
00221      EJECT                                                        EL514
00222  01  WS-BATCH-TOTALS.                                             EL514
00223      05  WS-REMIT-LIFE-PREMIUM      PIC S9(9)V99 VALUE ZEROS.     EL514
00224      05  WS-REMIT-LIFE-REFUND       PIC S9(9)V99 VALUE ZEROS.     EL514
00225      05  WS-REMIT-AH-PREMIUM        PIC S9(9)V99 VALUE ZEROS.     EL514
00226      05  WS-REMIT-AH-REFUND         PIC S9(9)V99 VALUE ZEROS.     EL514
00227      05  WS-REMIT-ISSUE-CNT         PIC S9(5)    VALUE ZEROS.     EL514
00228      05  WS-REMIT-CAN-CNT           PIC S9(5)    VALUE ZEROS.     EL514
00229      05  WS-ENTER-LIFE-PREMIUM      PIC S9(9)V99 VALUE ZEROS.     EL514
00230      05  WS-ENTER-LIFE-REFUND       PIC S9(9)V99 VALUE ZEROS.     EL514
00231      05  WS-ENTER-AH-PREMIUM        PIC S9(9)V99 VALUE ZEROS.     EL514
00232      05  WS-ENTER-AH-REFUND         PIC S9(9)V99 VALUE ZEROS.     EL514
00233      05  WS-ENTER-ISSUE-CNT         PIC S9(5)    VALUE ZEROS.     EL514
00234      05  WS-ENTER-CAN-CNT           PIC S9(5)    VALUE ZEROS.     EL514
00235      05  WS-COMM-ON-NET             PIC S9(6)V99 VALUE ZEROS.     EL514
00236                                                                   EL514
00237  01  WS-ACCOUNT-TOTALS.                                           EL514
00238      05  WS-AC-REMIT-LIFE-PREMIUM   PIC S9(9)V99 VALUE ZEROS.     EL514
00239      05  WS-AC-REMIT-LIFE-REFUND    PIC S9(9)V99 VALUE ZEROS.     EL514
00240      05  WS-AC-REMIT-AH-PREMIUM     PIC S9(9)V99 VALUE ZEROS.     EL514
00241      05  WS-AC-REMIT-AH-REFUND      PIC S9(9)V99 VALUE ZEROS.     EL514
00242      05  WS-AC-REMIT-ISSUE-CNT      PIC S9(5)    VALUE ZEROS.     EL514
00243      05  WS-AC-REMIT-CAN-CNT        PIC S9(5)    VALUE ZEROS.     EL514
00244      05  WS-AC-ENTER-LIFE-PREMIUM   PIC S9(9)V99 VALUE ZEROS.     EL514
00245      05  WS-AC-ENTER-LIFE-REFUND    PIC S9(9)V99 VALUE ZEROS.     EL514
00246      05  WS-AC-ENTER-AH-PREMIUM     PIC S9(9)V99 VALUE ZEROS.     EL514
00247      05  WS-AC-ENTER-AH-REFUND      PIC S9(9)V99 VALUE ZEROS.     EL514
00248      05  WS-AC-ENTER-ISSUE-CNT      PIC S9(5)    VALUE ZEROS.     EL514
00249      05  WS-AC-ENTER-CAN-CNT        PIC S9(5)    VALUE ZEROS.     EL514
00250      05  WS-AC-COMM-ON-NET          PIC S9(6)V99 VALUE ZEROS.     EL514
00251      05  WS-BATCH-CNT               PIC 9(5)     VALUE ZEROS.     EL514
00252                                                                   EL514
00253      EJECT                                                        EL514
00254  01  WS-CARRIER-TOTALS.                                           EL514
00255      05  WS-CA-REMIT-LIFE-PREMIUM   PIC S9(9)V99 VALUE ZEROS.     EL514
00256      05  WS-CA-REMIT-LIFE-REFUND    PIC S9(9)V99 VALUE ZEROS.     EL514
00257      05  WS-CA-REMIT-AH-PREMIUM     PIC S9(9)V99 VALUE ZEROS.     EL514
00258      05  WS-CA-REMIT-AH-REFUND      PIC S9(9)V99 VALUE ZEROS.     EL514
00259      05  WS-CA-REMIT-ISSUE-CNT      PIC S9(5)    VALUE ZEROS.     EL514
00260      05  WS-CA-REMIT-CAN-CNT        PIC S9(5)    VALUE ZEROS.     EL514
00261      05  WS-CA-ENTER-LIFE-PREMIUM   PIC S9(9)V99 VALUE ZEROS.     EL514
00262      05  WS-CA-ENTER-LIFE-REFUND    PIC S9(9)V99 VALUE ZEROS.     EL514
00263      05  WS-CA-ENTER-AH-PREMIUM     PIC S9(9)V99 VALUE ZEROS.     EL514
00264      05  WS-CA-ENTER-AH-REFUND      PIC S9(9)V99 VALUE ZEROS.     EL514
00265      05  WS-CA-ENTER-ISSUE-CNT      PIC S9(5)    VALUE ZEROS.     EL514
00266      05  WS-CA-ENTER-CAN-CNT        PIC S9(5)    VALUE ZEROS.     EL514
00267      05  WS-CA-COMM-ON-NET          PIC S9(6)V99 VALUE ZEROS.     EL514
00268                                                                   EL514
00269  01  WS-COMPANY-TOTALS.                                           EL514
00270      05  WS-CO-REMIT-LIFE-PREMIUM   PIC S9(9)V99 VALUE ZEROS.     EL514
00271      05  WS-CO-REMIT-LIFE-REFUND    PIC S9(9)V99 VALUE ZEROS.     EL514
00272      05  WS-CO-REMIT-AH-PREMIUM     PIC S9(9)V99 VALUE ZEROS.     EL514
00273      05  WS-CO-REMIT-AH-REFUND      PIC S9(9)V99 VALUE ZEROS.     EL514
00274      05  WS-CO-REMIT-ISSUE-CNT      PIC S9(5)    VALUE ZEROS.     EL514
00275      05  WS-CO-REMIT-CAN-CNT        PIC S9(5)    VALUE ZEROS.     EL514
00276      05  WS-CO-ENTER-LIFE-PREMIUM   PIC S9(9)V99 VALUE ZEROS.     EL514
00277      05  WS-CO-ENTER-LIFE-REFUND    PIC S9(9)V99 VALUE ZEROS.     EL514
00278      05  WS-CO-ENTER-AH-PREMIUM     PIC S9(9)V99 VALUE ZEROS.     EL514
00279      05  WS-CO-ENTER-AH-REFUND      PIC S9(9)V99 VALUE ZEROS.     EL514
00280      05  WS-CO-ENTER-ISSUE-CNT      PIC S9(5)    VALUE ZEROS.     EL514
00281      05  WS-CO-ENTER-CAN-CNT        PIC S9(5)    VALUE ZEROS.     EL514
00282      05  WS-CO-COMM-ON-NET          PIC S9(6)V99 VALUE ZEROS.     EL514
00283      EJECT                                                        EL514
00284  PROCEDURE DIVISION.                                              EL514
00285                                                                   EL514
00286  0000-LOAD-DATE-CARD.        COPY ELCDTERX.                          CL**2
00287                                                                   EL514
00288  0000-MAIN-LOGIC SECTION.                                         EL514
00289      PERFORM OPEN-FILES.                                          EL514
00290                                                                   EL514
00291      PERFORM GET-DATE.                                            EL514
00292                                                                   EL514
00293      SORT SORT-FILE                                               EL514
00294          ON ASCENDING KEY SR-CONTROL                              EL514
00295              INPUT PROCEDURE   1000-PROCESS-INPUT                 EL514
00296              OUTPUT PROCEDURE  2000-PROCESS-PENDING-OUT.          EL514
00297                                                                   EL514
00298      PERFORM CLOSE-FILES.                                         EL514
00299                                                                   EL514
00300      GOBACK.                                                      EL514
00301                                                                   EL514
00302      EJECT                                                        EL514
00303                                                                   EL514
00304  1000-PROCESS-INPUT SECTION.                                      EL514
00305      MOVE SPACES                 TO PB-CONTROL-BY-ACCOUNT.        EL514
00306      MOVE DTE-CLASIC-COMPANY-CD  TO PB-COMPANY-CD-A1.             EL514
00307      MOVE PB-CONTROL-BY-ACCOUNT  TO PB-ALTERNATE-KEY.             EL514
00308      START ERPNDB                                                 EL514
00309          KEY GREATER THAN PB-ALTERNATE-KEY.                       EL514
00310                                                                   EL514
00311      IF ERPNDB-FILE-STATUS = '23'                                 EL514
00312          GO TO 1900-CLOSE-PENDING-BUSINESS.                       EL514
00313                                                                   EL514
00314      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL514
00315          DISPLAY '*** EL514  ERPNDB START ERROR'                  EL514
00316          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL514
00317          GO TO ABEND-PGM.                                         EL514
00318                                                                   EL514
00319  1100-READ-LOOP.                                                  EL514
00320      READ ERPNDB NEXT RECORD INTO PENDING-BUSINESS.               EL514
00321                                                                   EL514
00322      IF ERPNDB-FILE-STATUS = '10'                                 EL514
00323          GO TO 1900-CLOSE-PENDING-BUSINESS.                       EL514
00324                                                                   EL514
00325      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL514
00326          DISPLAY '*** EL514  ERPNDB READ ERROR'                   EL514
00327          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL514
00328          GO TO ABEND-PGM.                                         EL514
00329                                                                   EL514
00330      IF PB-COMPANY-CD-A1 NOT = DTE-CLASIC-COMPANY-CD              EL514
00331          GO TO 1900-CLOSE-PENDING-BUSINESS.                       EL514
00332                                                                   EL514
00333      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS                           EL514
00334          GO TO 1100-READ-LOOP.                                    EL514
00335                                                                   EL514
00336  1200-PROCESS-PENDING-RECORD.                                     EL514
00337      MOVE PENDING-BUSINESS       TO SR-REST-OF-RECORD.            EL514
00338      MOVE PB-COMPANY-CD-A1       TO SR-COMPANY.                   EL514
00339      MOVE PB-CARRIER             TO SR-CARRIER.                   EL514
00340      MOVE PB-GROUPING            TO SR-GROUP.                     EL514
00341      MOVE PB-STATE               TO SR-STATE.                     EL514
00342      MOVE PB-ACCOUNT             TO SR-ACCOUNT.                   EL514
00343      MOVE PB-ENTRY-BATCH         TO SR-BATCH-NO.                  EL514
00344      MOVE PB-BATCH-SEQ-NO        TO SR-SEQ-NO.                    EL514
00345                                                                   EL514
00346  1300-RELEASE-TO-SORT.                                            EL514
00347      RELEASE SORT-RECORD.                                         EL514
00348                                                                   EL514
00349      GO TO 1100-READ-LOOP.                                        EL514
00350                                                                   EL514
00351  1900-CLOSE-PENDING-BUSINESS.                                     EL514
00352      CLOSE ERPNDB.                                                EL514
00353                                                                   EL514
00354      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL514
00355          DISPLAY '*** EL514  ERPNDB CLOSE ERROR'                  EL514
00356          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL514
00357          GO TO ABEND-PGM.                                         EL514
00358                                                                   EL514
00359  1900-CLOSE-EXIT.                                                 EL514
00360      EXIT.                                                        EL514
00361      EJECT                                                        EL514
00362                                                                   EL514
00363  2000-PROCESS-PENDING-OUT SECTION.                                EL514
00364                                                                   EL514
00365  2100-CONT.                                                       EL514
00366      RETURN SORT-FILE                                             EL514
00367          AT END                                                   EL514
00368              PERFORM 3000-PRINT-BATCH-TOTALS                      EL514
00369              PERFORM 3100-PRINT-ACCOUNT-TOTALS                    EL514
00370              PERFORM 3200-PRINT-CARRIER-TOTALS                    EL514
00371              PERFORM 3300-PRINT-COMPANY-TOTALS                    EL514
00372              GO TO 2900-EXIT.                                     EL514
00373                                                                   EL514
00374      IF WS-SAVE-KEY = SPACES                                      EL514
00375          MOVE SR-COMPARE         TO WS-SAVE-KEY.                  EL514
00376                                                                   EL514
00377      IF SR-CARRIER NOT = WS-SAVE-CARRIER                          EL514
00378          PERFORM 3000-PRINT-BATCH-TOTALS                          EL514
00379          PERFORM 3100-PRINT-ACCOUNT-TOTALS                        EL514
00380          PERFORM 3200-PRINT-CARRIER-TOTALS                        EL514
00381          MOVE ZEROS              TO WS-CARRIER-TOTALS             EL514
00382                                     WS-ACCOUNT-TOTALS             EL514
00383                                     WS-BATCH-TOTALS               EL514
00384          MOVE SR-COMPARE         TO WS-SAVE-KEY.                  EL514
00385                                                                   EL514
00386      IF SR-ACCOUNT NOT = WS-SAVE-ACCOUNT                          EL514
00387          PERFORM 3000-PRINT-BATCH-TOTALS                          EL514
00388          PERFORM 3100-PRINT-ACCOUNT-TOTALS                        EL514
00389          MOVE ZEROS              TO WS-ACCOUNT-TOTALS             EL514
00390                                     WS-BATCH-TOTALS               EL514
00391          MOVE SR-COMPARE         TO WS-SAVE-KEY.                  EL514
00392                                                                   EL514
00393      IF SR-BATCH-NO NOT = WS-SAVE-BATCH                           EL514
00394          PERFORM 3000-PRINT-BATCH-TOTALS                          EL514
00395          MOVE ZEROS              TO WS-BATCH-TOTALS               EL514
00396          MOVE SR-COMPARE         TO WS-SAVE-KEY.                  EL514
00397                                                                   EL514
00398      MOVE SR-REST-OF-RECORD      TO PENDING-BUSINESS.             EL514
00399                                                                   EL514
00400      IF PB-ISSUE                                                  EL514
00401          ADD PB-I-LF-PREMIUM-AMT TO WS-ENTER-LIFE-PREMIUM         EL514
00402          ADD PB-I-AH-PREMIUM-AMT TO WS-ENTER-AH-PREMIUM           EL514
00403          COMPUTE WS-COMM-ON-NET = WS-COMM-ON-NET +                EL514
00404              (PB-I-LF-PREMIUM-AMT * PB-I-LIFE-COMMISSION) +       EL514
00405              (PB-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION)           EL514
00406          ADD +1                  TO WS-ENTER-ISSUE-CNT            EL514
00407          GO TO 2100-CONT.                                         EL514
00408                                                                   EL514
00409      IF PB-CANCELLATION                                           EL514
00410          ADD PB-C-LF-CANCEL-AMT  TO WS-ENTER-LIFE-REFUND          EL514
00411          ADD PB-C-AH-CANCEL-AMT  TO WS-ENTER-AH-REFUND            EL514
00412          COMPUTE WS-COMM-ON-NET = WS-COMM-ON-NET +                EL514
00413              ((PB-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION) +      EL514
00414               (PB-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION) * -1)    EL514
00415          ADD +1                  TO WS-ENTER-CAN-CNT              EL514
00416          GO TO 2100-CONT.                                         EL514
00417                                                                   EL514
00418      IF PB-BATCH-TRAILER                                          EL514
00419          MOVE PB-B-LF-ISS-PRM-REMITTED TO WS-REMIT-LIFE-PREMIUM   EL514
00420          MOVE PB-B-LF-CAN-PRM-REMITTED TO WS-REMIT-LIFE-REFUND    EL514
00421          MOVE PB-B-AH-ISS-PRM-REMITTED TO WS-REMIT-AH-PREMIUM     EL514
00422          MOVE PB-B-AH-CAN-PRM-REMITTED TO WS-REMIT-AH-REFUND      EL514
00423          MOVE PB-B-ISSUE-CNT-REMITTED  TO WS-REMIT-ISSUE-CNT      EL514
00424          MOVE PB-B-CANCEL-CNT-REMITTED TO WS-REMIT-CAN-CNT        EL514
00425          MOVE PB-INPUT-DT              TO DC-BIN-DATE-1           EL514
00426          MOVE SPACE                    TO DC-OPTION-CODE          EL514
00427          PERFORM 8500-DATE-CONVERSION                             EL514
00428          MOVE DC-GREG-DATE-1-EDIT      TO DTL-DT-ENTERED.         EL514
00429                                                                   EL514
00430      GO TO 2100-CONT.                                             EL514
00431                                                                   EL514
00432  2900-EXIT.                                                       EL514
00433      EXIT.                                                        EL514
00434      EJECT                                                        EL514
00435                                                                   EL514
00436  3000-PRINT-BATCH-TOTALS SECTION.                                 EL514
00437      MOVE WS-SAVE-CARRIER        TO DTL-CARR.                     EL514
00438      MOVE WS-SAVE-GROUPING       TO DTL-GRP.                      EL514
00439      MOVE WS-SAVE-STATE          TO DTL-STATE.                    EL514
00440      MOVE WS-SAVE-ACCOUNT        TO DTL-ACCT.                     EL514
00441      MOVE WS-SAVE-BATCH          TO DTL-BATCH.                    EL514
00442      MOVE 'ENTERED'              TO DTL-MSG.                      EL514
00443      MOVE WS-ENTER-LIFE-PREMIUM  TO DTL-LF-PREM.                  EL514
00444      MOVE WS-ENTER-LIFE-REFUND   TO DTL-LF-REF.                   EL514
00445      MOVE WS-ENTER-AH-PREMIUM    TO DTL-AH-PREM.                  EL514
00446      MOVE WS-ENTER-AH-REFUND     TO DTL-AH-REF.                   EL514
00447      MOVE WS-ENTER-ISSUE-CNT     TO DTL-ISS-CNT.                  EL514
00448      MOVE WS-ENTER-CAN-CNT       TO DTL-CAN-CNT.                  EL514
00449      MOVE '0'                    TO P-CTL.                           CL**5
00450      MOVE WS-DETAIL              TO P-DATA.                       EL514
00451      PERFORM WRITE-A-LINE.                                        EL514
00452      MOVE SPACES                 TO WS-DETAIL.                    EL514
00453      MOVE WS-DASH                TO DTL-DASH.                     EL514
00454                                                                   EL514
00455      MOVE 'REMITTED'             TO DTL-MSG.                      EL514
00456      MOVE WS-REMIT-LIFE-PREMIUM  TO DTL-LF-PREM.                  EL514
00457      MOVE WS-REMIT-LIFE-REFUND   TO DTL-LF-REF.                   EL514
00458      MOVE WS-REMIT-AH-PREMIUM    TO DTL-AH-PREM.                  EL514
00459      MOVE WS-REMIT-AH-REFUND     TO DTL-AH-REF.                   EL514
00460      MOVE WS-REMIT-ISSUE-CNT     TO DTL-ISS-CNT.                  EL514
00461      MOVE WS-REMIT-CAN-CNT       TO DTL-CAN-CNT.                  EL514
00462      MOVE SPACE                  TO P-CTL.                           CL**5
00463      MOVE WS-DETAIL              TO P-DATA.                       EL514
00464      PERFORM WRITE-A-LINE.                                        EL514
00465      MOVE SPACES                 TO WS-DETAIL.                    EL514
00466      MOVE WS-DASH                TO DTL-DASH.                     EL514
00467                                                                   EL514
00468      MOVE 'OUT OF BAL'           TO DTL-MSG.                      EL514
00469                                                                   EL514
00470      IF WS-REMIT-LIFE-PREMIUM NOT = ZEROS                         EL514
00471          SUBTRACT WS-ENTER-LIFE-PREMIUM FROM WS-REMIT-LIFE-PREMIUMEL514
00472                   GIVING DTL-LF-PREM.                             EL514
00473                                                                   EL514
00474      IF WS-REMIT-LIFE-REFUND NOT = ZEROS                          EL514
00475          SUBTRACT WS-ENTER-LIFE-REFUND FROM WS-REMIT-LIFE-REFUND  EL514
00476                   GIVING DTL-LF-REF.                              EL514
00477                                                                   EL514
00478      IF WS-REMIT-AH-PREMIUM NOT = ZEROS                           EL514
00479          SUBTRACT WS-ENTER-AH-PREMIUM FROM WS-REMIT-AH-PREMIUM    EL514
00480                   GIVING DTL-AH-PREM.                             EL514
00481                                                                   EL514
00482      IF WS-REMIT-AH-REFUND NOT = ZEROS                            EL514
00483          SUBTRACT WS-ENTER-AH-REFUND FROM WS-REMIT-AH-REFUND      EL514
00484                   GIVING DTL-AH-REF.                              EL514
00485                                                                   EL514
00486      IF WS-REMIT-ISSUE-CNT NOT = ZEROS                            EL514
00487          SUBTRACT WS-ENTER-ISSUE-CNT FROM WS-REMIT-ISSUE-CNT      EL514
00488                   GIVING DTL-ISS-CNT.                             EL514
00489                                                                   EL514
00490      IF WS-REMIT-CAN-CNT NOT = ZEROS                              EL514
00491          SUBTRACT WS-ENTER-CAN-CNT FROM WS-REMIT-CAN-CNT          EL514
00492                   GIVING DTL-CAN-CNT.                             EL514
00493                                                                   EL514
00494      MOVE WS-COMM-ON-NET         TO DTL-COMM.                     EL514
00495      MOVE SPACE                  TO P-CTL.                           CL**5
00496      MOVE WS-DETAIL              TO P-DATA.                       EL514
00497      PERFORM WRITE-A-LINE.                                        EL514
00498      MOVE SPACES                 TO WS-DETAIL.                    EL514
00499      MOVE WS-DASH                TO DTL-DASH.                     EL514
00500      ADD 1   TO WS-BATCH-CNT.                                     EL514
00501                                                                   EL514
00502      ADD WS-REMIT-LIFE-PREMIUM   TO WS-AC-REMIT-LIFE-PREMIUM.     EL514
00503      ADD WS-REMIT-LIFE-REFUND    TO WS-AC-REMIT-LIFE-REFUND.      EL514
00504      ADD WS-REMIT-AH-PREMIUM     TO WS-AC-REMIT-AH-PREMIUM.       EL514
00505      ADD WS-REMIT-AH-REFUND      TO WS-AC-REMIT-AH-REFUND.        EL514
00506      ADD WS-REMIT-ISSUE-CNT      TO WS-AC-REMIT-ISSUE-CNT.        EL514
00507      ADD WS-REMIT-CAN-CNT        TO WS-AC-REMIT-CAN-CNT.          EL514
00508                                                                   EL514
00509      ADD WS-ENTER-LIFE-PREMIUM   TO WS-AC-ENTER-LIFE-PREMIUM.     EL514
00510      ADD WS-ENTER-LIFE-REFUND    TO WS-AC-ENTER-LIFE-REFUND.      EL514
00511      ADD WS-ENTER-AH-PREMIUM     TO WS-AC-ENTER-AH-PREMIUM.       EL514
00512      ADD WS-ENTER-AH-REFUND      TO WS-AC-ENTER-AH-REFUND.        EL514
00513      ADD WS-ENTER-ISSUE-CNT      TO WS-AC-ENTER-ISSUE-CNT.        EL514
00514      ADD WS-ENTER-CAN-CNT        TO WS-AC-ENTER-CAN-CNT.          EL514
00515                                                                   EL514
00516      ADD WS-COMM-ON-NET          TO WS-AC-COMM-ON-NET.            EL514
00517                                                                   EL514
00518  3000-EXIT.                                                       EL514
00519      EXIT.                                                        EL514
00520      EJECT                                                        EL514
00521                                                                   EL514
00522  3100-PRINT-ACCOUNT-TOTALS SECTION.                               EL514
00523      IF WS-BATCH-CNT LESS 2                                       EL514
00524          GO TO 3100-ADD-TOTALS.                                   EL514
00525                                                                   EL514
00526      MOVE WS-SAVE-ACCOUNT           TO ACCT-NUM.                  EL514
00527      MOVE WS-ACCOUNT-TOTAL          TO WS-DETAIL.                 EL514
00528      MOVE 'ENTERED'                 TO DTL-MSG.                   EL514
00529      MOVE WS-AC-ENTER-LIFE-PREMIUM  TO DTL-LF-PREM.               EL514
00530      MOVE WS-AC-ENTER-LIFE-REFUND   TO DTL-LF-REF.                EL514
00531      MOVE WS-AC-ENTER-AH-PREMIUM    TO DTL-AH-PREM.               EL514
00532      MOVE WS-AC-ENTER-AH-REFUND     TO DTL-AH-REF.                EL514
00533      MOVE WS-AC-ENTER-ISSUE-CNT     TO DTL-ISS-CNT.               EL514
00534      MOVE WS-AC-ENTER-CAN-CNT       TO DTL-CAN-CNT.               EL514
00535      MOVE '0'                       TO P-CTL.                        CL**5
00536      MOVE WS-DETAIL                 TO P-DATA.                    EL514
00537      PERFORM WRITE-A-LINE.                                        EL514
00538      MOVE SPACES                    TO WS-DETAIL.                 EL514
00539      MOVE WS-DASH                   TO DTL-DASH.                  EL514
00540                                                                   EL514
00541      MOVE 'REMITTED'                TO DTL-MSG.                   EL514
00542      MOVE WS-AC-REMIT-LIFE-PREMIUM  TO DTL-LF-PREM.               EL514
00543      MOVE WS-AC-REMIT-LIFE-REFUND   TO DTL-LF-REF.                EL514
00544      MOVE WS-AC-REMIT-AH-PREMIUM    TO DTL-AH-PREM.               EL514
00545      MOVE WS-AC-REMIT-AH-REFUND     TO DTL-AH-REF.                EL514
00546      MOVE WS-AC-REMIT-ISSUE-CNT     TO DTL-ISS-CNT.               EL514
00547      MOVE WS-AC-REMIT-CAN-CNT       TO DTL-CAN-CNT.               EL514
00548      MOVE SPACE                     TO P-CTL.                        CL**5
00549      MOVE WS-DETAIL                 TO P-DATA.                    EL514
00550      PERFORM WRITE-A-LINE.                                        EL514
00551      MOVE SPACES                    TO WS-DETAIL.                 EL514
00552      MOVE WS-DASH                   TO DTL-DASH.                  EL514
00553                                                                   EL514
00554      MOVE 'OUT OF BAL'              TO DTL-MSG.                   EL514
00555                                                                   EL514
00556      IF WS-AC-REMIT-LIFE-PREMIUM NOT = ZEROS                      EL514
00557          SUBTRACT WS-AC-ENTER-LIFE-PREMIUM                        EL514
00558                   FROM WS-AC-REMIT-LIFE-PREMIUM                   EL514
00559                   GIVING DTL-LF-PREM.                             EL514
00560                                                                   EL514
00561      IF WS-AC-REMIT-LIFE-REFUND NOT = ZEROS                       EL514
00562          SUBTRACT WS-AC-ENTER-LIFE-REFUND                         EL514
00563                   FROM WS-AC-REMIT-LIFE-REFUND                    EL514
00564                   GIVING DTL-LF-REF.                              EL514
00565                                                                   EL514
00566      IF WS-AC-REMIT-AH-PREMIUM NOT = ZEROS                        EL514
00567          SUBTRACT WS-AC-ENTER-AH-PREMIUM                          EL514
00568                   FROM WS-AC-REMIT-AH-PREMIUM                     EL514
00569                   GIVING DTL-AH-PREM.                             EL514
00570                                                                   EL514
00571      IF WS-AC-REMIT-AH-REFUND NOT = ZEROS                         EL514
00572          SUBTRACT WS-AC-ENTER-AH-REFUND                           EL514
00573                   FROM WS-AC-REMIT-AH-REFUND                      EL514
00574                   GIVING DTL-AH-REF.                              EL514
00575                                                                   EL514
00576      IF WS-AC-REMIT-ISSUE-CNT NOT = ZEROS                         EL514
00577          SUBTRACT WS-AC-ENTER-ISSUE-CNT FROM WS-AC-REMIT-ISSUE-CNTEL514
00578                   GIVING DTL-ISS-CNT.                             EL514
00579                                                                   EL514
00580      IF WS-AC-REMIT-CAN-CNT NOT = ZEROS                           EL514
00581          SUBTRACT WS-AC-ENTER-CAN-CNT FROM WS-AC-REMIT-CAN-CNT    EL514
00582                   GIVING DTL-CAN-CNT.                             EL514
00583                                                                   EL514
00584      MOVE WS-AC-COMM-ON-NET      TO DTL-COMM.                     EL514
00585      MOVE SPACE                  TO P-CTL.                           CL**5
00586      MOVE WS-DETAIL              TO P-DATA.                       EL514
00587      PERFORM WRITE-A-LINE.                                        EL514
00588      MOVE SPACES                 TO WS-DETAIL.                    EL514
00589      MOVE 0                      TO P-CTL.                        EL514
00590      MOVE WS-DETAIL              TO P-DATA.                       EL514
00591      PERFORM WRITE-A-LINE.                                        EL514
00592      MOVE WS-DASH                TO DTL-DASH.                     EL514
00593                                                                   EL514
00594  3100-ADD-TOTALS.                                                 EL514
00595      ADD WS-AC-REMIT-LIFE-PREMIUM   TO WS-CA-REMIT-LIFE-PREMIUM.  EL514
00596      ADD WS-AC-REMIT-LIFE-REFUND    TO WS-CA-REMIT-LIFE-REFUND.   EL514
00597      ADD WS-AC-REMIT-AH-PREMIUM     TO WS-CA-REMIT-AH-PREMIUM.    EL514
00598      ADD WS-AC-REMIT-AH-REFUND      TO WS-CA-REMIT-AH-REFUND.     EL514
00599      ADD WS-AC-REMIT-ISSUE-CNT      TO WS-CA-REMIT-ISSUE-CNT.     EL514
00600      ADD WS-AC-REMIT-CAN-CNT        TO WS-CA-REMIT-CAN-CNT.       EL514
00601                                                                   EL514
00602      ADD WS-AC-ENTER-LIFE-PREMIUM   TO WS-CA-ENTER-LIFE-PREMIUM.  EL514
00603      ADD WS-AC-ENTER-LIFE-REFUND    TO WS-CA-ENTER-LIFE-REFUND.   EL514
00604      ADD WS-AC-ENTER-AH-PREMIUM     TO WS-CA-ENTER-AH-PREMIUM.    EL514
00605      ADD WS-AC-ENTER-AH-REFUND      TO WS-CA-ENTER-AH-REFUND.     EL514
00606      ADD WS-AC-ENTER-ISSUE-CNT      TO WS-CA-ENTER-ISSUE-CNT.     EL514
00607      ADD WS-AC-ENTER-CAN-CNT        TO WS-CA-ENTER-CAN-CNT.       EL514
00608                                                                   EL514
00609      ADD WS-AC-COMM-ON-NET          TO WS-CA-COMM-ON-NET.         EL514
00610                                                                   EL514
00611  3100-EXIT.                                                       EL514
00612      EXIT.                                                        EL514
00613      EJECT                                                        EL514
00614                                                                   EL514
00615  3200-PRINT-CARRIER-TOTALS SECTION.                               EL514
00616      MOVE '  T O T A L   C A R R I E R' TO WS-DETAIL.             EL514
00617      MOVE 'ENTERED'                     TO DTL-MSG.               EL514
00618      MOVE WS-CA-ENTER-LIFE-PREMIUM      TO DTL-LF-PREM.           EL514
00619      MOVE WS-CA-ENTER-LIFE-REFUND       TO DTL-LF-REF.            EL514
00620      MOVE WS-CA-ENTER-AH-PREMIUM        TO DTL-AH-PREM.           EL514
00621      MOVE WS-CA-ENTER-AH-REFUND         TO DTL-AH-REF.            EL514
00622      MOVE WS-CA-ENTER-ISSUE-CNT         TO DTL-ISS-CNT.           EL514
00623      MOVE WS-CA-ENTER-CAN-CNT           TO DTL-CAN-CNT.           EL514
00624      MOVE '0'                           TO P-CTL.                    CL**5
00625      MOVE WS-DETAIL                     TO P-DATA.                EL514
00626      PERFORM WRITE-A-LINE.                                        EL514
00627      MOVE SPACES                        TO WS-DETAIL.             EL514
00628      MOVE WS-DASH                       TO DTL-DASH.              EL514
00629                                                                   EL514
00630      MOVE 'REMITTED'                    TO DTL-MSG.               EL514
00631      MOVE WS-CA-REMIT-LIFE-PREMIUM      TO DTL-LF-PREM.           EL514
00632      MOVE WS-CA-REMIT-LIFE-REFUND       TO DTL-LF-REF.            EL514
00633      MOVE WS-CA-REMIT-AH-PREMIUM        TO DTL-AH-PREM.           EL514
00634      MOVE WS-CA-REMIT-AH-REFUND         TO DTL-AH-REF.            EL514
00635      MOVE WS-CA-REMIT-ISSUE-CNT         TO DTL-ISS-CNT.           EL514
00636      MOVE WS-CA-REMIT-CAN-CNT           TO DTL-CAN-CNT.           EL514
00637      MOVE SPACE                         TO P-CTL.                    CL**5
00638      MOVE WS-DETAIL                     TO P-DATA.                EL514
00639      PERFORM WRITE-A-LINE.                                        EL514
00640      MOVE SPACES                        TO WS-DETAIL.             EL514
00641      MOVE WS-DASH                       TO DTL-DASH.              EL514
00642                                                                   EL514
00643      MOVE 'OUT OF BAL'                  TO DTL-MSG.               EL514
00644                                                                   EL514
00645      IF WS-CA-REMIT-LIFE-PREMIUM NOT = ZEROS                      EL514
00646          SUBTRACT WS-CA-ENTER-LIFE-PREMIUM                        EL514
00647                   FROM WS-CA-REMIT-LIFE-PREMIUM                   EL514
00648                   GIVING DTL-LF-PREM.                             EL514
00649                                                                   EL514
00650      IF WS-CA-REMIT-LIFE-REFUND NOT = ZEROS                       EL514
00651          SUBTRACT WS-CA-ENTER-LIFE-REFUND                         EL514
00652                   FROM WS-CA-REMIT-LIFE-REFUND                    EL514
00653                   GIVING DTL-LF-REF.                              EL514
00654                                                                   EL514
00655      IF WS-CA-REMIT-AH-PREMIUM NOT = ZEROS                        EL514
00656          SUBTRACT WS-CA-ENTER-AH-PREMIUM                          EL514
00657                   FROM WS-CA-REMIT-AH-PREMIUM                     EL514
00658                   GIVING DTL-AH-PREM.                             EL514
00659                                                                   EL514
00660      IF WS-CA-REMIT-AH-REFUND NOT = ZEROS                         EL514
00661          SUBTRACT WS-CA-ENTER-AH-REFUND                           EL514
00662                   FROM WS-CA-REMIT-AH-REFUND                      EL514
00663                   GIVING DTL-AH-REF.                              EL514
00664                                                                   EL514
00665      IF WS-CA-REMIT-ISSUE-CNT NOT = ZEROS                         EL514
00666          SUBTRACT WS-CA-ENTER-ISSUE-CNT FROM WS-CA-REMIT-ISSUE-CNTEL514
00667                   GIVING DTL-ISS-CNT.                             EL514
00668                                                                   EL514
00669      IF WS-CA-REMIT-CAN-CNT NOT = ZEROS                           EL514
00670          SUBTRACT WS-CA-ENTER-CAN-CNT FROM WS-CA-REMIT-CAN-CNT    EL514
00671                   GIVING DTL-CAN-CNT.                             EL514
00672                                                                   EL514
00673      MOVE WS-CA-COMM-ON-NET         TO DTL-COMM.                  EL514
00674      MOVE SPACE                     TO P-CTL.                        CL**5
00675      MOVE WS-DETAIL                 TO P-DATA.                    EL514
00676      PERFORM WRITE-A-LINE.                                        EL514
00677      MOVE SPACES                    TO WS-DETAIL.                 EL514
00678      MOVE '-'                       TO P-CTL.                        CL**5
00679      MOVE WS-DETAIL                 TO P-DATA.                    EL514
00680      PERFORM WRITE-A-LINE.                                        EL514
00681      MOVE WS-DASH                   TO DTL-DASH.                  EL514
00682                                                                   EL514
00683      ADD WS-CA-REMIT-LIFE-PREMIUM   TO WS-CO-REMIT-LIFE-PREMIUM.  EL514
00684      ADD WS-CA-REMIT-LIFE-REFUND    TO WS-CO-REMIT-LIFE-REFUND.   EL514
00685      ADD WS-CA-REMIT-AH-PREMIUM     TO WS-CO-REMIT-AH-PREMIUM.    EL514
00686      ADD WS-CA-REMIT-AH-REFUND      TO WS-CO-REMIT-AH-REFUND.     EL514
00687      ADD WS-CA-REMIT-ISSUE-CNT      TO WS-CO-REMIT-ISSUE-CNT.     EL514
00688      ADD WS-CA-REMIT-CAN-CNT        TO WS-CO-REMIT-CAN-CNT.       EL514
00689                                                                   EL514
00690      ADD WS-CA-ENTER-LIFE-PREMIUM   TO WS-CO-ENTER-LIFE-PREMIUM.  EL514
00691      ADD WS-CA-ENTER-LIFE-REFUND    TO WS-CO-ENTER-LIFE-REFUND.   EL514
00692      ADD WS-CA-ENTER-AH-PREMIUM     TO WS-CO-ENTER-AH-PREMIUM.    EL514
00693      ADD WS-CA-ENTER-AH-REFUND      TO WS-CO-ENTER-AH-REFUND.     EL514
00694      ADD WS-CA-ENTER-ISSUE-CNT      TO WS-CO-ENTER-ISSUE-CNT.     EL514
00695      ADD WS-CA-ENTER-CAN-CNT        TO WS-CO-ENTER-CAN-CNT.       EL514
00696                                                                   EL514
00697      ADD WS-CA-COMM-ON-NET          TO WS-CO-COMM-ON-NET.         EL514
00698                                                                   EL514
00699  3200-EXIT.                                                       EL514
00700      EXIT.                                                        EL514
00701      EJECT                                                        EL514
00702                                                                   EL514
00703  3300-PRINT-COMPANY-TOTALS SECTION.                               EL514
00704      MOVE 99                        TO WS-LINE-COUNT.             EL514
00705      MOVE '  T O T A L   C O M P A N Y' TO WS-DETAIL.             EL514
00706      MOVE 'ENTERED'                 TO DTL-MSG.                   EL514
00707      MOVE WS-CO-ENTER-LIFE-PREMIUM  TO DTL-LF-PREM.               EL514
00708      MOVE WS-CO-ENTER-LIFE-REFUND   TO DTL-LF-REF.                EL514
00709      MOVE WS-CO-ENTER-AH-PREMIUM    TO DTL-AH-PREM.               EL514
00710      MOVE WS-CO-ENTER-AH-REFUND     TO DTL-AH-REF.                EL514
00711      MOVE WS-CO-ENTER-ISSUE-CNT     TO DTL-ISS-CNT.               EL514
00712      MOVE WS-CO-ENTER-CAN-CNT       TO DTL-CAN-CNT.               EL514
00713      MOVE '0'                       TO P-CTL.                        CL**5
00714      MOVE WS-DETAIL                 TO P-DATA.                    EL514
00715      PERFORM WRITE-A-LINE.                                        EL514
00716      MOVE SPACES                    TO WS-DETAIL.                 EL514
00717      MOVE WS-DASH                   TO DTL-DASH.                  EL514
00718                                                                   EL514
00719      MOVE 'REMITTED'                TO DTL-MSG.                   EL514
00720      MOVE WS-CO-REMIT-LIFE-PREMIUM  TO DTL-LF-PREM.               EL514
00721      MOVE WS-CO-REMIT-LIFE-REFUND   TO DTL-LF-REF.                EL514
00722      MOVE WS-CO-REMIT-AH-PREMIUM    TO DTL-AH-PREM.               EL514
00723      MOVE WS-CO-REMIT-AH-REFUND     TO DTL-AH-REF.                EL514
00724      MOVE WS-CO-REMIT-ISSUE-CNT     TO DTL-ISS-CNT.               EL514
00725      MOVE WS-CO-REMIT-CAN-CNT       TO DTL-CAN-CNT.               EL514
00726      MOVE SPACE                     TO P-CTL.                        CL**5
00727      MOVE WS-DETAIL                 TO P-DATA.                    EL514
00728      PERFORM WRITE-A-LINE.                                        EL514
00729      MOVE SPACES                    TO WS-DETAIL.                 EL514
00730      MOVE WS-DASH                   TO DTL-DASH.                  EL514
00731                                                                   EL514
00732      MOVE 'OUT OF BAL'              TO DTL-MSG.                   EL514
00733                                                                   EL514
00734      IF WS-CO-REMIT-LIFE-PREMIUM NOT = ZEROS                      EL514
00735          SUBTRACT WS-CO-ENTER-LIFE-PREMIUM                        EL514
00736                   FROM WS-CO-REMIT-LIFE-PREMIUM                   EL514
00737                   GIVING DTL-LF-PREM.                             EL514
00738                                                                   EL514
00739      IF WS-CO-REMIT-LIFE-REFUND NOT = ZEROS                       EL514
00740          SUBTRACT WS-CO-ENTER-LIFE-REFUND                         EL514
00741                   FROM WS-CO-REMIT-LIFE-REFUND                    EL514
00742                   GIVING DTL-LF-REF.                              EL514
00743                                                                   EL514
00744      IF WS-CO-REMIT-AH-PREMIUM NOT = ZEROS                        EL514
00745          SUBTRACT WS-CO-ENTER-AH-PREMIUM                          EL514
00746                   FROM WS-CO-REMIT-AH-PREMIUM                     EL514
00747                   GIVING DTL-AH-PREM.                             EL514
00748                                                                   EL514
00749      IF WS-CO-REMIT-AH-REFUND NOT = ZEROS                         EL514
00750          SUBTRACT WS-CO-ENTER-AH-REFUND                           EL514
00751                   FROM WS-CO-REMIT-AH-REFUND                      EL514
00752                   GIVING DTL-AH-REF.                              EL514
00753                                                                   EL514
00754      IF WS-CO-REMIT-ISSUE-CNT NOT = ZEROS                         EL514
00755          SUBTRACT WS-CO-ENTER-ISSUE-CNT FROM WS-CO-REMIT-ISSUE-CNTEL514
00756                   GIVING DTL-ISS-CNT.                             EL514
00757                                                                   EL514
00758      IF WS-CO-REMIT-CAN-CNT NOT = ZEROS                           EL514
00759          SUBTRACT WS-CO-ENTER-CAN-CNT FROM WS-CO-REMIT-CAN-CNT    EL514
00760                   GIVING DTL-CAN-CNT.                             EL514
00761                                                                   EL514
00762      MOVE WS-CO-COMM-ON-NET      TO DTL-COMM.                     EL514
00763      MOVE SPACE                  TO P-CTL.                           CL**5
00764      MOVE WS-DETAIL              TO P-DATA.                       EL514
00765      PERFORM WRITE-A-LINE.                                        EL514
00766      MOVE SPACES                 TO WS-DETAIL.                    EL514
00767                                                                   EL514
00768  3300-EXIT.                                                       EL514
00769      EXIT.                                                        EL514
00770      EJECT                                                        EL514
00771  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL514
00772                                                                   EL514
00773  GET-DATE SECTION.                                                EL514
00774  GDS-010.                                                         EL514
00775      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL514
00776      MOVE '2'                    TO  DC-OPTION-CODE.              EL514
00777      PERFORM 8500-DATE-CONVERSION.                                EL514
00778      MOVE DC-BIN-DATE-1          TO  WS-SAVE-DATE.                EL514
00779                                                                   EL514
00780      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL514
00781                                                                   EL514
00782      MOVE WS-TIME                TO  WS-SAVE-TIME.                EL514
00783                                                                   EL514
00784  GDS-EXIT.                                                        EL514
00785      EXIT.                                                        EL514
00786                                                                   EL514
00787  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL514
00788                                                                   EL514
00789  WRITE-HEADINGS SECTION.                                          EL514
00790      IF  WS-H2-DATE EQUAL SPACES                                  EL514
00791          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL514
00792          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL514
00793          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL514
00794                                                                   EL514
00795      ADD +1  TO  WS-PAGE.                                         EL514
00796      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL514
00797      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL514
00798      MOVE ZERO                   TO  WS-LINE-COUNT.               EL514
00799                                                                   EL514
00800      MOVE WS-HEADING1            TO  PRT.                         EL514
00801      MOVE '1'                    TO  X.                           EL514
00802      PERFORM WRITE-PRINTER.                                       EL514
00803                                                                   EL514
00804      MOVE WS-HEADING2            TO  PRT.                         EL514
00805      MOVE ' '                    TO  X.                           EL514
00806      PERFORM WRITE-PRINTER.                                       EL514
00807                                                                   EL514
00808      MOVE WS-HEADING3            TO  PRT.                         EL514
00809      MOVE ' '                    TO  X.                           EL514
00810      PERFORM WRITE-PRINTER.                                       EL514
00811                                                                   EL514
00812      MOVE WS-HEADING4            TO  PRT.                         EL514
00813      MOVE ' '                    TO  X.                           EL514
00814      PERFORM WRITE-PRINTER.                                       EL514
00815                                                                   EL514
00816                                                                   EL514
00817      MOVE WS-HEADING5            TO  PRT.                         EL514
00818      PERFORM WRITE-PRINTER.                                       EL514
00819                                                                   EL514
00820      MOVE +7                     TO  WS-LINE-COUNT.               EL514
00821                                                                   EL514
00822  WHS-020.               COPY ELCWHS2.                             EL514
00823      EJECT                                                        EL514
00824  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL514
00825                                                                   EL514
00826  WPS-020.                                                         EL514
00827                                                                   EL514
00828      IF DTE-FICH NOT = SPACE AND                                  EL514
00829          FICH-OPEN   = SPACE                                      EL514
00830          MOVE 'X' TO FICH-OPEN                                    EL514
00831          OPEN OUTPUT FICH.                                        EL514
00832                                                                   EL514
00833      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL514
00834          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL514
00835              OPEN I-O ELREPT                                      EL514
00836              IF DTE-F-1 NOT = ZERO AND                            EL514
00837                 DTE-VSAM-FLAGS NOT = '97'                         EL514
00838                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL514
00839                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL514
00840                                  TO  WS-ABEND-MESSAGE             EL514
00841                  GO TO ABEND-PGM                                  EL514
00842              ELSE                                                 EL514
00843                  MOVE '1'                   TO REPT-OPEN          EL514
00844                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL514
00845                  MOVE '1'                   TO RF-RECORD-TYPE     EL514
00846                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL514
00847                  MOVE ZERO                  TO RF-LINE-NUMBER     EL514
00848                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL514
00849                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL514
00850                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL514
00851                  MOVE '2'                   TO RF-RECORD-TYPE     EL514
00852                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL514
00853                  MOVE ZERO                  TO RF-LINE-NUMBER     EL514
00854                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL514
00855                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL514
00856                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL514
00857                  MOVE '1'                   TO RF-RECORD-TYPE     EL514
00858                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL514
00859                  MOVE SPACES                TO RF-REPORT-LINE-133.EL514
00860                                                                   EL514
00861      IF DTE-ABEND-CD-1 = '81' AND                                 EL514
00862         DTE-PRT-OPT    = 'S'                                      EL514
00863          MOVE +0302  TO WS-RETURN-CODE                            EL514
00864          GO TO ABEND-PGM.                                         EL514
00865                                                                   EL514
00866      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL514
00867          MOVE X      TO RF-CTL-CHAR-133                           EL514
00868          MOVE P-DATA TO RF-DATA-133                               EL514
00869              IF DTE-ABEND-CD-1 = SPACES                           EL514
00870                  ADD +1 TO DTE-TOT-LINES                          EL514
00871                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL514
00872                  WRITE REPORT-SAVE-FILE                           EL514
00873                      INVALID KEY                                  EL514
00874                          MOVE '88' TO DTE-ABEND-CD-1              EL514
00875                          CLOSE ELREPT                             EL514
00876                          MOVE SPACE TO REPT-OPEN.                 EL514
00877                                                                   EL514
00878      IF DTE-FICH NOT = SPACE                                      EL514
00879          MOVE X TO P-CTL                                          EL514
00880          WRITE FICH-REC FROM PRT.                                 EL514
00881                                                                   EL514
00882      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL514
00883          MOVE X TO P-CTL                                          EL514
00884          WRITE PRT.                                               EL514
00885                                                                   EL514
00886      GO TO DTE-PRINT-EXIT.                                        EL514
00887                                                                   EL514
00888  DTE-REPORT-DELETE.                                               EL514
00889      IF DTE-F-1 NOT = ZERO                                        EL514
00890          MOVE ZERO TO DTE-VSAM-FLAGS                              EL514
00891          GO TO DTE-DELETE-EXIT.                                   EL514
00892                                                                   EL514
00893      READ ELREPT   NEXT RECORD                                    EL514
00894            AT END   GO TO DTE-DELETE-EXIT.                        EL514
00895                                                                   EL514
00896      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL514
00897         OLC-REPORT-NAME       = RF-REPORT-ID                      EL514
00898          DELETE ELREPT RECORD                                     EL514
00899          GO TO DTE-REPORT-DELETE.                                 EL514
00900                                                                   EL514
00901  DTE-DELETE-EXIT.                                                 EL514
00902      EXIT.                                                        EL514
00903                                                                   EL514
00904  DTE-PRINT-EXIT.                                                  EL514
00905      EXIT.                                                        EL514
00906 ******************************************************************EL514
00907      EJECT                                                        EL514
00908  OPEN-FILES SECTION.                                              EL514
00909  OFS-010.                                                         EL514
00910      OPEN INPUT ERPNDB                                            EL514
00911          OUTPUT PRNTR.                                            EL514
00912                                                                   EL514
00913      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL514
00914          DISPLAY '*** EL514  ERPNDB OPEN ERROR'                   EL514
00915          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL514
00916          GO TO ABEND-PGM.                                         EL514
00917                                                                   EL514
00918      MOVE LIFE-OVERRIDE-L6            TO HD4-LF-OVRD1             EL514
00919                                          HD4-LF-OVRD2.            EL514
00920      MOVE AH-OVERRIDE-L6              TO HD4-AH-OVRD1             EL514
00921                                          HD4-AH-OVRD2.            EL514
00922                                                                   EL514
00923  OFS-EXIT.                                                        EL514
00924      EXIT.                                                        EL514
00925                                                                   EL514
00926      EJECT                                                        EL514
00927  CLOSE-FILES SECTION.                                             EL514
00928                                                                   EL514
00929  CFS-050. COPY ELCPRTCX.                                          EL514
00930      CLOSE PRNTR.                                                 EL514
00931                                                                   EL514
00932  CFS-EXIT.                                                        EL514
00933      EXIT.                                                        EL514
00934                                                                   EL514
00935  ABEND-PGM SECTION. COPY ELCABEND.                                   CL**2
00936                                                                   EL514
