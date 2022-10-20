00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ELCNVCH
00002                                                                   ELCNVCH
00003  PROGRAM-ID.                 ELCNVCH.                                LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 03/05/96 14:36:49.                    CL**3
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 ELCNVCH
00009 *AUTHOR.        LOGIC, INC.                                          CL**3
00010 *               DALLAS, TEXAS.                                       CL**3
00011                                                                   ELCNVCH
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   ELCNVCH
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   ELCNVCH
00024 *REMARKS.                                                         ELCNVCH
00025 *       CONVERT OLD CHECK QUE RECORDS TO REVISED CHECK FILE FORMAT   CL**3
00026 *                        * 500 BYTES TO 600 BYTES *                  CL**3
00027                                                                   ELCNVCH
00028  EJECT                                                            ELCNVCH
00029  ENVIRONMENT DIVISION.                                            ELCNVCH
00030  CONFIGURATION SECTION.                                           ELCNVCH
00031  INPUT-OUTPUT SECTION.                                            ELCNVCH
00032  FILE-CONTROL.                                                    ELCNVCH
00033                                                                   ELCNVCH
00034      SELECT CHECK-QUE-IN   ASSIGN TO SYS010-UT-FBA1-SYS010.       ELCNVCH
00035                                                                   ELCNVCH
00036      SELECT CHECK-QUE-OUT  ASSIGN TO SYS011-FBA1-ERCHKOT          ELCNVCH
00037                            ORGANIZATION IS INDEXED                ELCNVCH
00038                            ACCESS IS DYNAMIC                      ELCNVCH
00039                            RECORD KEY IS CH-CONTROL-PRIMARY       ELCNVCH
00040                            FILE STATUS IS CH-FILE-STATUS.         ELCNVCH
00041                                                                   ELCNVCH
00042  EJECT                                                            ELCNVCH
00043  DATA DIVISION.                                                   ELCNVCH
00044  FILE SECTION.                                                    ELCNVCH
00045                                                                   ELCNVCH
00046  FD  CHECK-QUE-IN                                                 ELCNVCH
00047      BLOCK CONTAINS 0 RECORDS
00048      RECORDING MODE F.                                               CL**3
00049                                                                   ELCNVCH
00050 ******************************************************************ELCNVCH
00051 *                                                                *ELCNVCH
00052 *                            ERCCHEK                             *ELCNVCH
00053 *                            VMOD=2.003                          *ELCNVCH
00054 *                                                                *ELCNVCH
00055 *   FILE DESCRIPTION = CHECK RECORDS                             *ELCNVCH
00056 *                                                                *ELCNVCH
00057 *   FILE TYPE = VSAM,KSDS                                        *ELCNVCH
00058 *   RECORD SIZE = 500    RECFORM = FIXED                         *ELCNVCH
00059 *                                                                *ELCNVCH
00060 *   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *ELCNVCH
00061 *       ALTERNATE INDEX = NONE                                   *ELCNVCH
00062 *                                                                *ELCNVCH
00063 *   LOG = YES                                                    *ELCNVCH
00064 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *ELCNVCH
00065 ******************************************************************ELCNVCH
00066  01  CHECK-RECORDS-IN.                                            ELCNVCH
00067      12  OCH-RECORD-ID                      PIC XX.               ELCNVCH
00068          88  VALID-OCH-ID                      VALUE 'CH'.        ELCNVCH
00069                                                                   ELCNVCH
00070      12  OCH-CONTROL-PRIMARY.                                     ELCNVCH
00071          16  OCH-COMPANY-CD                PIC X.                 ELCNVCH
00072          16  OCH-CARRIER                   PIC X.                 ELCNVCH
00073          16  OCH-GROUPING                  PIC X(6).              ELCNVCH
00074          16  OCH-STATE                     PIC XX.                ELCNVCH
00075          16  OCH-ACCOUNT                   PIC X(10).             ELCNVCH
00076          16  OCH-CERT-EFF-DT               PIC XX.                ELCNVCH
00077          16  OCH-CERT-NO.                                         ELCNVCH
00078              20  OCH-CERT-PRIME            PIC X(10).             ELCNVCH
00079              20  OCH-CERT-SFX              PIC X.                 ELCNVCH
00080          16  OCH-SEQUENCE-NO               PIC S9(4)     COMP.    ELCNVCH
00081                                                                   ELCNVCH
00082      12  OCH-RECORDED-DT                   PIC XX.                ELCNVCH
00083      12  OCH-RECORDED-BY                   PIC X(4).              ELCNVCH
00084      12  OCH-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.  ELCNVCH
00085                                                                   ELCNVCH
00086      12  OCH-AMOUNT-PAID                   PIC S9(7)V99  COMP-3.  ELCNVCH
00087      12  OCH-CHECK-NO                      PIC X(7).              ELCNVCH
00088      12  OCH-REASON-FOR-CHECK              PIC X(25).             ELCNVCH
00089      12  OCH-CHECK-WRITTEN-DT              PIC XX.                ELCNVCH
00090      12  OCH-OFFLINE-CHECK-IND             PIC X.                 ELCNVCH
00091          88  MANUAL-CHECK-WRITTEN             VALUE 'Y'.          ELCNVCH
00092                                                                   ELCNVCH
00093      12  OCH-PAYEE-INFO.                                          ELCNVCH
00094          16  OCH-PAYEE-NAME-1              PIC X(30).             ELCNVCH
00095          16  OCH-PAYEE-NAME-2              PIC X(30).             ELCNVCH
00096          16  OCH-PAYEE-STREET-NO           PIC X(30).             ELCNVCH
00097          16  OCH-PAYEE-CITY-ST             PIC X(30).             ELCNVCH
00098          16  OCH-PAYEE-ZIP-CODE.                                  ELCNVCH
00099              20  OCH-PAYEE-ZIP.                                   ELCNVCH
00100                  24  OCH-ZIP-PRI-1ST       PIC X.                 ELCNVCH
00101                      88  OCH-CANADIAN-POST-CODE                   ELCNVCH
00102                                           VALUES 'A' THRU 'Z'.    ELCNVCH
00103                  24  FILLER                PIC X(4).              ELCNVCH
00104              20  OCH-PAYEE-ZIP-EXT         PIC X(4).              ELCNVCH
00105          16  OCH-CANADIAN-RDF REDEFINES OCH-PAYEE-ZIP-CODE.       ELCNVCH
00106              20  OCH-CAN-POSTAL-1          PIC XXX.               ELCNVCH
00107              20  OCH-CAN-POSTAL-2          PIC XXX.               ELCNVCH
00108              20  FILLER                    PIC XXX.               ELCNVCH
00109                                                                   ELCNVCH
00110      12  OCH-CHECK-STUB-TEXT.                                     ELCNVCH
00111          16  OCH-STUB-LINE-1               PIC X(50).             ELCNVCH
00112          16  OCH-STUB-LINE-2               PIC X(50).             ELCNVCH
00113          16  OCH-STUB-LINE-3               PIC X(50).             ELCNVCH
00114          16  FILLER.                                              ELCNVCH
00115            18  OCH-STUB-LINE-4             PIC X(20).             ELCNVCH
00116            18  OCH-LIENHOLDER-NAME         PIC X(30).             ELCNVCH
00117                                                                   ELCNVCH
00118      12  OCH-COMPENSATION-CONTROL.                                ELCNVCH
00119          16  OCH-COMP-CARRIER              PIC X.                 ELCNVCH
00120          16  OCH-COMP-GROUPING             PIC X(6).              ELCNVCH
00121          16  OCH-COMP-FIN-RESP             PIC X(10).             ELCNVCH
00122          16  OCH-COMP-ACCOUNT              PIC X(10).             ELCNVCH
00123      12  OCH-CREDIT-SELECT-DT              PIC XX.                ELCNVCH
00124      12  OCH-CREDIT-ACCEPT-DT              PIC XX.                ELCNVCH
00125      12  OCH-PAYEE-CODE                    PIC X(6).              ELCNVCH
00126      12  FILLER                            PIC X(8).              ELCNVCH
00127                                                                   ELCNVCH
00128      12  OCH-VOID-DATA.                                           ELCNVCH
00129          20  OCH-VOID-DT                   PIC XX.                ELCNVCH
00130          20  OCH-VOID-BY                   PIC X(4).              ELCNVCH
00131          20  OCH-VOID-REASON               PIC X(25).             ELCNVCH
00132                                                                   ELCNVCH
00133      12  OCH-CHECK-QUE-CONTROL             PIC S9(8)     COMP.    ELCNVCH
00134              88  PAYMENT-NOT-QUEUED           VALUE ZERO.         ELCNVCH
00135      12  OCH-CHECK-QUE-SEQUENCE            PIC S9(4)     COMP.    ELCNVCH
00136                                                                   ELCNVCH
00137      12  FILLER                            PIC XX.                ELCNVCH
00138                                                                   ELCNVCH
00139 ******************************************************************ELCNVCH
00140      EJECT                                                        ELCNVCH
00141  FD  CHECK-QUE-OUT.                                                  CL**3
00142                                                                   ELCNVCH
00143                              COPY ERCCHEK.                        ELCNVCH
00144                                                                   ELCNVCH
00145  EJECT                                                            ELCNVCH
00146  WORKING-STORAGE SECTION.                                         ELCNVCH
00147  77  FILLER  PIC X(32)  VALUE '********************************'. ELCNVCH
00148  77  FILLER  PIC X(32)  VALUE '*   ELCNVCH WORKING-STORAGE    '.  ELCNVCH
00149  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.003 ************'.    CL**3
00150                                                                      CL**3
00151  77  CHKQ-IN-CNT             PIC S9(7)   COMP-3  VALUE +0.        ELCNVCH
00152  77  CHKQ-OUT-CNT            PIC S9(7)   COMP-3  VALUE +0.        ELCNVCH
00153                                                                   ELCNVCH
00154  77  CH-FILE-STATUS          PIC XX              VALUE ZEROS.     ELCNVCH
00155                                                                   ELCNVCH
00156  01  WS.                                                          ELCNVCH
00157      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       ELCNVCH
00158      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   ELCNVCH
00159      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    ELCNVCH
00160      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       ELCNVCH
00161                                                                   ELCNVCH
00162      12  WS-ABEND-CODE         PIC 9(4).                          ELCNVCH
00163      12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.                    ELCNVCH
00164          16  ABEND-CODE-1    PIC XX.                              ELCNVCH
00165          16  ABEND-CODE-2    PIC XX.                              ELCNVCH
00166                                                                   ELCNVCH
00167      EJECT                                                        ELCNVCH
00168  PROCEDURE DIVISION.                                              ELCNVCH
00169                                                                   ELCNVCH
00170 ******************************************************************ELCNVCH
00171 ***           O P E N   F I L E S   R O U T I N E              ***ELCNVCH
00172 ******************************************************************ELCNVCH
00173                                                                   ELCNVCH
00174  0080-OPEN-FILES.                                                 ELCNVCH
00175                                                                   ELCNVCH
00176      OPEN INPUT  CHECK-QUE-IN                                     ELCNVCH
00177           I-O    CHECK-QUE-OUT.                                   ELCNVCH
00178                                                                   ELCNVCH
00179      IF CH-FILE-STATUS EQUAL '00' OR '97'                         ELCNVCH
00180          NEXT SENTENCE                                            ELCNVCH
00181      ELSE                                                         ELCNVCH
00182          MOVE 'ERROR OCCURRED OPEN - CHECK OUT '                  ELCNVCH
00183                                       TO  WS-ABEND-MESSAGE        ELCNVCH
00184          MOVE CH-FILE-STATUS          TO  WS-ABEND-FILE-STATUS    ELCNVCH
00185          GO TO ABEND-PGM.                                         ELCNVCH
00186                                                                   ELCNVCH
00187      EJECT                                                        ELCNVCH
00188 ******************************************************************ELCNVCH
00189 ***       R E A D   C H E C K   Q U E  R O U T I N E           ***ELCNVCH
00190 ******************************************************************ELCNVCH
00191                                                                   ELCNVCH
00192  0200-CHECK-QUE-READ-ROUTINE.                                     ELCNVCH
00193                                                                   ELCNVCH
00194      READ CHECK-QUE-IN                                            ELCNVCH
00195          AT END                                                   ELCNVCH
00196          GO TO 9990-FINAL-CLOSE.                                  ELCNVCH
00197                                                                   ELCNVCH
00198      IF OCH-RECORD-ID NOT EQUAL 'CH'                              ELCNVCH
00199          GO TO 0200-CHECK-QUE-READ-ROUTINE.                       ELCNVCH
00200                                                                   ELCNVCH
00201      ADD +1                      TO CHKQ-IN-CNT.                  ELCNVCH
00202                                                                   ELCNVCH
00203      MOVE SPACES                 TO CHECK-RECORDS.                ELCNVCH
00204                                                                   ELCNVCH
00205      MOVE OCH-RECORD-ID          TO CH-RECORD-ID.                 ELCNVCH
00206                                                                   ELCNVCH
00207      MOVE OCH-COMPANY-CD         TO CH-COMPANY-CD.                ELCNVCH
00208      MOVE OCH-CARRIER            TO CH-CARRIER.                   ELCNVCH
00209      MOVE OCH-GROUPING           TO CH-GROUPING.                  ELCNVCH
00210      MOVE OCH-STATE              TO CH-STATE.                     ELCNVCH
00211      MOVE OCH-ACCOUNT            TO CH-ACCOUNT.                   ELCNVCH
00212      MOVE OCH-CERT-EFF-DT        TO CH-CERT-EFF-DT.               ELCNVCH
00213      MOVE OCH-CERT-PRIME         TO CH-CERT-PRIME.                ELCNVCH
00214      MOVE OCH-CERT-SFX           TO CH-CERT-SFX.                  ELCNVCH
00215      MOVE OCH-SEQUENCE-NO        TO CH-SEQUENCE-NO.               ELCNVCH
00216                                                                   ELCNVCH
00217      MOVE OCH-RECORDED-DT        TO CH-RECORDED-DT.               ELCNVCH
00218      MOVE OCH-RECORDED-BY        TO CH-RECORDED-BY.               ELCNVCH
00219      MOVE OCH-LAST-MAINT-HHMMSS  TO CH-LAST-MAINT-HHMMSS.         ELCNVCH
00220                                                                   ELCNVCH
00221      MOVE OCH-AMOUNT-PAID        TO CH-AMOUNT-PAID.               ELCNVCH
00222      MOVE OCH-CHECK-NO           TO CH-CHECK-NO.                  ELCNVCH
00223      MOVE OCH-REASON-FOR-CHECK   TO CH-REASON-FOR-CHECK.          ELCNVCH
00224      MOVE OCH-CHECK-WRITTEN-DT   TO CH-CHECK-WRITTEN-DT.          ELCNVCH
00225      MOVE OCH-OFFLINE-CHECK-IND  TO CH-OFFLINE-CHECK-IND.         ELCNVCH
00226                                                                   ELCNVCH
00227      MOVE OCH-PAYEE-NAME-1       TO CH-PAYEE-NAME-1.              ELCNVCH
00228      MOVE OCH-PAYEE-NAME-2       TO CH-PAYEE-ADDRESS-1.           ELCNVCH
00229      MOVE SPACES                 TO CH-PAYEE-ADDRESS-2.           ELCNVCH
00230      MOVE OCH-PAYEE-STREET-NO    TO CH-PAYEE-CITY-ST.             ELCNVCH
00231      MOVE OCH-PAYEE-ZIP-CODE     TO CH-PAYEE-ZIP-CODE.            ELCNVCH
00232      MOVE OCH-STUB-LINE-1        TO CH-STUB-LINE-1.               ELCNVCH
00233      MOVE OCH-STUB-LINE-2        TO CH-STUB-LINE-2.               ELCNVCH
00234      MOVE OCH-STUB-LINE-3        TO CH-STUB-LINE-3.               ELCNVCH
00235      MOVE OCH-STUB-LINE-4        TO CH-STUB-LINE-4.               ELCNVCH
00236      MOVE OCH-LIENHOLDER-NAME    TO CH-LIENHOLDER-NAME.           ELCNVCH
00237                                                                   ELCNVCH
00238      MOVE OCH-COMP-CARRIER       TO CH-COMP-CARRIER.              ELCNVCH
00239      MOVE OCH-COMP-GROUPING      TO CH-COMP-GROUPING.             ELCNVCH
00240      MOVE OCH-COMP-FIN-RESP      TO CH-COMP-FIN-RESP.             ELCNVCH
00241      MOVE OCH-COMP-ACCOUNT       TO CH-COMP-ACCOUNT.              ELCNVCH
00242                                                                   ELCNVCH
00243      MOVE OCH-CREDIT-SELECT-DT   TO CH-CREDIT-SELECT-DT.          ELCNVCH
00244      MOVE OCH-CREDIT-ACCEPT-DT   TO CH-CREDIT-ACCEPT-DT.          ELCNVCH
00245                                                                   ELCNVCH
00246      MOVE OCH-PAYEE-CODE         TO CH-PAYEE-CODE.                ELCNVCH
00247      MOVE OCH-VOID-DT            TO CH-VOID-DT.                   ELCNVCH
00248      MOVE OCH-VOID-BY            TO CH-VOID-BY.                   ELCNVCH
00249      MOVE OCH-VOID-REASON        TO CH-VOID-REASON.               ELCNVCH
00250                                                                   ELCNVCH
00251      MOVE OCH-CHECK-QUE-CONTROL  TO CH-CHECK-QUE-CONTROL.         ELCNVCH
00252      MOVE OCH-CHECK-QUE-SEQUENCE TO CH-CHECK-QUE-SEQUENCE.        ELCNVCH
00253                                                                   ELCNVCH
00254      MOVE 'M'                    TO CH-CHECK-ORIGIN-SW.           ELCNVCH
00255                                                                      CL**2
00256      MOVE ZEROS                  TO CH-LF-REFUND                     CL**2
00257                                     CH-AH-REFUND                     CL**2
00258                                     CH-DEDUCT-WITHHELD               CL**2
00259                                     CH-ADDITIONAL-CHARGE.            CL**2
00260                                                                   ELCNVCH
00261      WRITE CHECK-RECORDS.                                         ELCNVCH
00262                                                                   ELCNVCH
00263      IF CH-FILE-STATUS NOT EQUAL '00'                             ELCNVCH
00264          MOVE 'ERROR OCCURRED WRITE - CHECK OUT '                 ELCNVCH
00265                                       TO  WS-ABEND-MESSAGE        ELCNVCH
00266          MOVE CH-FILE-STATUS          TO  WS-ABEND-FILE-STATUS    ELCNVCH
00267          GO TO ABEND-PGM.                                         ELCNVCH
00268                                                                   ELCNVCH
00269      ADD +1                      TO CHKQ-OUT-CNT.                 ELCNVCH
00270                                                                   ELCNVCH
00271      GO TO 0200-CHECK-QUE-READ-ROUTINE.                           ELCNVCH
00272                                                                   ELCNVCH
00273  1000-EXIT.                                                       ELCNVCH
00274      EXIT.                                                        ELCNVCH
00275      EJECT                                                        ELCNVCH
00276 ******************************************************************ELCNVCH
00277 ***          E N D   O F   J O B   P R O C E S S I N G         ***ELCNVCH
00278 ******************************************************************ELCNVCH
00279                                                                   ELCNVCH
00280  9990-FINAL-CLOSE.                                                ELCNVCH
00281                                                                   ELCNVCH
00282      DISPLAY '**** CHECK QUE RCDS READ = ' CHKQ-IN-CNT.           ELCNVCH
00283      DISPLAY '**** CHECK RCDS  WRITTEN = ' CHKQ-OUT-CNT.          ELCNVCH
00284                                                                   ELCNVCH
00285      CLOSE CHECK-QUE-IN                                           ELCNVCH
00286            CHECK-QUE-OUT.                                         ELCNVCH
00287                                                                   ELCNVCH
00288      GO TO 9999-END-THE-JOB.                                      ELCNVCH
00289                                                                   ELCNVCH
00290      EJECT                                                        ELCNVCH
00291  ABEND-PGM.                                                       ELCNVCH
00292                                  COPY ELCABEND.                   ELCNVCH
00293                                                                      CL**4
00294  9999-END-THE-JOB.                                                ELCNVCH
00295                                                                   ELCNVCH
00296      GOBACK.                                                      ELCNVCH
