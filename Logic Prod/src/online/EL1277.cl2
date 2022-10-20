00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL1277.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 10:00:12.
00007 *                            VMOD=2.005.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *                                                                *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.    TRANSACTION - EXX7 - CERTIFICATE MAILING DATA.
00025 *
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
080406* 080406    2006051800002  AJRA  ADD POSTCARD MAIL STATUS
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
070313* 070313  CR2013052300001  PEMA  REMOVE SSN FROM MAP.
120513* 120513  IR2013112500001  PEMA  CHANGE EDIT ON ADDRESS
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
042221* 042221  IR2021041300001  PEMA  Fix birth date problems.
101201******************************************************************

00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL1277 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.005 *********'.
00034
00035      COPY ELCSCTM.
00036
00037      COPY ELCSCRTY.
00038
00039  01  WS-DATE-AREA.
00040      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00041      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
00042
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-DUPKEY                  VALUE +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
00043  01  STANDARD-AREAS.
           12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.          
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.
00044      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
00045      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00046      12  MAP-NAME                    PIC X(8)    VALUE 'EL127G'.
00047      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1277S'.
00048      12  SCREEN-NUMBER               PIC X(4)    VALUE '127G'.
00049      12  TRANS-ID                    PIC X(4)    VALUE 'EXX7'.
00050      12  THIS-PGM                    PIC X(8)    VALUE 'EL1277'.
00051      12  PGM-NAME                    PIC X(8).
00052      12  TIME-IN                     PIC S9(7).
00053      12  TIME-OUT-R  REDEFINES TIME-IN.
00054          16  FILLER                  PIC X.
00055          16  TIME-OUT                PIC 99V99.
00056          16  FILLER                  PIC X(2).
00057      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00058      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00059      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
00060      12  XCTL-1272                   PIC X(8)    VALUE 'EL1272'.
00061      12  XCTL-1273                   PIC X(8)    VALUE 'EL1273'.
00062      12  XCTL-1274                   PIC X(8)    VALUE 'EL1274'.
00063      12  XCTL-1275                   PIC X(8)    VALUE 'EL1275'.
00064      12  XCTL-1276                   PIC X(8)    VALUE 'EL1276'.
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00068      12  ERMAIL-ID                   PIC X(8)    VALUE 'ERMAIL'.
00069      12  ELCERT-ID                   PIC X(8)    VALUE 'ELCERT'.
00070      12  WS-RECORD-LENGTHS   COMP.
CIDMOD*        16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)  VALUE +250.
CIDMOD         16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)  VALUE +374.
00072          16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)  VALUE +450.
00073
00074      12  DEEDIT-FIELD                PIC X(15).
00075      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00076
00077      12  WS-BIRTHDT                  PIC XX.
090408     12  WS-JNT-BIRTHDT              PIC XX.
00078
00079      12  RETURN-FROM                 PIC X(8).
00080      12  QID.
00081          16  QID-TERM                PIC X(4).
00082          16  FILLER                  PIC X(4)    VALUE '127G'.
00083      12  WS-RECORD-FOUND-SW          PIC  X      VALUE SPACE.
00084          88  RECORD-FOUND                        VALUE 'Y'.
00085          88  RECORD-NOT-FOUND                    VALUE 'N'.
00086      12  WS-DUPREC-SW                PIC  X      VALUE SPACE.
00087          88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
00088      12  W-ZIP-TEST                  PIC  X(1)   VALUE SPACE.
00089          88  W-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00090
00091      EJECT
00092      12  ERROR-MESSAGES.
00093          16  ER-0000                 PIC  X(4)   VALUE '0000'.
00094          16  ER-0004                 PIC  X(4)   VALUE '0004'.
00095          16  ER-0008                 PIC  X(4)   VALUE '0008'.
00096          16  ER-0023                 PIC  X(4)   VALUE '0023'.
00097          16  ER-0029                 PIC  X(4)   VALUE '0029'.
00098          16  ER-0070                 PIC  X(4)   VALUE '0070'.
00099          16  ER-0142                 PIC  X(4)   VALUE '0142'.
00100          16  ER-0219                 PIC  X(4)   VALUE '0219'.
00101          16  ER-0220                 PIC  X(4)   VALUE '0220'.
100217         16  er-2050                 pic  x(4)   value '2050'.
00102          16  ER-2187                 PIC  X(4)   VALUE '2187'.
00103          16  ER-2199                 PIC  X(4)   VALUE '2199'.
               16  ER-2209                 PIC  X(4)   VALUE '2209'.
100217         16  er-3061                 pic  x(4)   value '3061'.
100217         16  er-3062                 pic  x(4)   value '3062'.
100217         16  er-3063                 pic  x(4)   value '3063'.
00104          16  ER-7049                 PIC  X(4)   VALUE '7049'.
00105          16  ER-7428                 PIC  X(4)   VALUE '7428'.
00106
00107  01  WS-PHONE                        PIC 9(10)   VALUE ZEROS.
00108  01  WS-PHONE-R REDEFINES WS-PHONE.
00109      12  WS-PHONE-AC                 PIC 999.
00110      12  WS-PHONE-EXT                PIC 999.
00111      12  WS-PHONE-NO                 PIC 9999.
00112  01  EDITED-PHONE-NO                 PIC X(12)   VALUE ZEROS.
00113  01  EDITED-PHONE-NO-R REDEFINES EDITED-PHONE-NO.
00114      12  ED-PHONE-AC                 PIC 999.
00115      12  ED-PHONE-DASH1              PIC X.
00116      12  ED-PHONE-EXT                PIC 999.
00117      12  ED-PHONE-DASH2              PIC X.
00118      12  ED-PHONE-NO                 PIC 9999.
00119
00120
00121  01  WS-CM-CONTROL-PRIMARY.
00122      05  WS-CM-COMPANY-CD            PIC  X.
00123      05  WS-CM-CARRIER               PIC  X.
00124      05  WS-CM-GROUPING              PIC  X(6).
00125      05  WS-CM-STATE                 PIC  XX.
00126      05  WS-CM-ACCOUNT               PIC  X(10).
00127      05  WS-CM-CERT-EFF-DT           PIC  XX.
00128      05  WS-CM-CERT-NO.
00129          10  WS-CM-CERT-PRIME        PIC  X(10).
00130          10  WS-CM-CERT-SFX          PIC  X.
00131
100217 01  zipcd-pass-area-len         pic s9(4) comp value +67.
100217 01  zipcd-pass-area.
100217     03  PA-zip                  PIC X(5).
100217     03  PA-ErrorCode-zip        PIC X(10).
100217     03  PA-city                 PIC x(50).
100217     03  PA-state                PIC Xx.

080406 01  WS-MAILED-AREA.
080406     05  WS-SUB                      PIC S9(5) COMP VALUE +0.
080406     05  WS-MAILED-BLD-STATUS.
080406         10  WS-MAILED-STAT          PIC X(5).
080406         10  FILLER                  PIC X(1) VALUE SPACE.
080406         10  WS-MAILED-DATE          PIC X(8).
080406
080406     05  WS-MAILED-DATA OCCURS 7 TIMES.
080406         10  WS-MAILED-TYPE          PIC X(3).
080406         10  WS-MAILED-STATUS        PIC X(14).
00132
00134                                  COPY ELCDATE.
00138                                  COPY ELCLOGOF.
00141                                  COPY ELCATTR.
00144                                  COPY ELCEMIB.
00147                                  COPY ELCINTF.
00148      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00149          16  FILLER                  PIC X(317).
00150          16  PI-PEND-SW              PIC X.
00151          16  FILLER                  PIC X(322).
00152
00153      EJECT
00154
00155      COPY ELCAID.
00156  01  FILLER    REDEFINES DFHAID.
00157      12  FILLER                      PIC X(8).
00158      12  PF-VALUES                   PIC X       OCCURS 24 TIMES.
00159
00160      EJECT
00161      COPY EL1277S.
00162
00163      EJECT
00164
00165  LINKAGE SECTION.
00166  01  DFHCOMMAREA                     PIC X(1024).
00167
                                       COPY ELCCNTL.
00169
00170      COPY ERCMAIL.
00171
00172      EJECT
00173
00174      COPY ELCCERT.
00175
00176      EJECT
00177  PROCEDURE DIVISION.
00178
00179      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00180      MOVE '5'                    TO  DC-OPTION-CODE.
00181      PERFORM 9700-DATE-LINK.
00182      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00183      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00184
00185      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00186      MOVE 1                      TO  EMI-NUMBER-OF-LINES.
00187      MOVE EIBTRMID               TO  QID-TERM.
00188
00189      IF EIBCALEN = 0
00190          GO TO 8800-UNAUTHORIZED-ACCESS.
00191
00192      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00193          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00194              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00195              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00196              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00197              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00198              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00199              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00200              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00201              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00202          ELSE
00203              MOVE PI-CALLING-PROGRAM   TO  RETURN-FROM
00204              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00205              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00206              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00207              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00208              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00209              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00210              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00211              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00212
00213      IF EIBAID = DFHCLEAR
00214          GO TO 9400-CLEAR.
00215
00216      IF PI-PROCESSOR-ID = 'LGXX'
00217          NEXT SENTENCE
00218      ELSE
00219          EXEC CICS READQ TS
00220              QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00221              INTO    (SECURITY-CONTROL)
00222              LENGTH  (SC-COMM-LENGTH)
00223              ITEM    (SC-ITEM)
00224          END-EXEC
00225          MOVE SC-CREDIT-DISPLAY (33)   TO  PI-DISPLAY-CAP
00226          MOVE SC-CREDIT-UPDATE  (33)   TO  PI-MODIFY-CAP
00227          IF NOT DISPLAY-CAP
00228              MOVE 'READ'               TO  SM-READ
00229              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00230              MOVE ER-0070              TO  EMI-ERROR
00231              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00232              GO TO 8100-SEND-INITIAL-MAP.
00233
00234      IF  EIBTRNID NOT = TRANS-ID
00235          MOVE LOW-VALUES         TO  EL127GI
00236          PERFORM 7000-FORMAT-SCREEN
00237          GO TO 8100-SEND-INITIAL-MAP.
00238
00239      IF NOT MODIFY-CAP
00240          MOVE 'UPDATE'            TO  SM-READ
00241          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00242          MOVE ER-0070             TO  EMI-ERROR
00243          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00244          GO TO 8100-SEND-INITIAL-MAP.
00245
00246      EXEC CICS    HANDLE    CONDITION
00247           PGMIDERR          (9600-PGMID-ERROR)
00248           ERROR             (9990-ABEND)
00249           END-EXEC.
00250
00251      EJECT
00252
00253  0200-RECEIVE.
00254      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00255          MOVE ER-0008            TO  EMI-ERROR
00256          PERFORM 9900-ERROR-FORMAT
00257          MOVE -1                 TO  GMAINTL
00258          GO TO 8200-SEND-DATAONLY.
00259
00260      EXEC CICS RECEIVE
00261          MAP      (MAP-NAME)
00262          MAPSET   (MAPSET-NAME)
00263          INTO     (EL127GI)
00264      END-EXEC.
00265
00266      IF  GPFKEYL = 0
00267          GO TO 0300-CHECK-PFKEYS.
00268
00269      IF  EIBAID NOT = DFHENTER
00270          MOVE ER-0004            TO  EMI-ERROR
00271          GO TO 0320-INPUT-ERROR.
00272
00273      IF  GPFKEYI NUMERIC
00274          IF  GPFKEYI = '12' OR '23' OR '24'
00275              MOVE PF-VALUES (GPFKEYI) TO  EIBAID
00276          ELSE
00277              MOVE ER-0029             TO  EMI-ERROR
00278              GO TO 0320-INPUT-ERROR.
00279
00280  0300-CHECK-PFKEYS.
00281      IF EIBAID = DFHPF23
00282          GO TO 8810-PF23.
00283
00284      IF EIBAID = DFHPF24
00285          GO TO 9200-RETURN-MAIN-MENU.
00286
00287      IF EIBAID = DFHPF12
00288          GO TO 9500-PF12.
00289
100217     IF EIBAID = DFHENTER or dfhpf7
00291          GO TO 400-EDIT-INPUT-DATA.
00292
00293      MOVE ER-0029                TO  EMI-ERROR.
00294
00295  0320-INPUT-ERROR.
00296      PERFORM 9900-ERROR-FORMAT.
00297      MOVE AL-UNBON               TO  GPFKEYA.
00298      IF GPFKEYL = 0
00299          MOVE -1                 TO  GMAINTL
00300      ELSE
00301          MOVE -1                 TO  GPFKEYL.
00302      GO TO 8200-SEND-DATAONLY.
00303
00304      EJECT
00305
00306  400-EDIT-INPUT-DATA.
00307      IF GMAINTI = 'A' OR 'C' OR 'S'
00308          NEXT SENTENCE
00309      ELSE
00310          MOVE ER-0023            TO  EMI-ERROR
00311          MOVE -1                 TO  GMAINTL
00312          MOVE AL-UABON           TO  GMAINTA
00313          PERFORM 9900-ERROR-FORMAT
00314          GO TO 8200-SEND-DATAONLY.
00315
00316  410-CHECK-ERRORS.
00317      IF EMI-ERROR = ZEROS
00318          NEXT SENTENCE
00319      ELSE
00320          GO TO 8200-SEND-DATAONLY.
00321
00322      IF GMAINTI = 'A'
00323         GO TO 1000-ADD-RECORD.
00324
00325      IF GMAINTI = 'C'
00326         GO TO 2000-CHANGE-RECORD.
00327
00328      IF GMAINTI = 'S'
00329         PERFORM 7000-FORMAT-SCREEN
00330         GO TO 8200-SEND-DATAONLY.
00331
00332      EJECT
00333
00334  1000-ADD-RECORD       SECTION.
00335      PERFORM 3000-EDIT-MAIL-DATA.
00336
00337      IF NOT EMI-NO-ERRORS
00338          GO TO 8200-SEND-DATAONLY.
00339
00340      EXEC CICS GETMAIN
00341          SET      (ADDRESS OF MAILING-DATA)
00342          LENGTH   (WS-ERMAIL-RECORD-LENGTH)
00343          INITIMG  (GETMAIN-SPACE)
00344      END-EXEC.
00345
00346      MOVE  SPACES                TO  MAILING-DATA.
00347
00348      MOVE  'MA'                  TO  MA-RECORD-ID.
00349
00350      MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
00351      MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
00352      MOVE  PI-STATE              TO  WS-CM-STATE.
00353      MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
00354      MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
00355      MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
00356      MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
00357      MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
00358
00359      MOVE  WS-CM-CONTROL-PRIMARY TO  MA-CONTROL-PRIMARY.
00360
00361      MOVE PI-PROCESSOR-ID        TO  MA-RECORD-ADDED-BY
00362                                      MA-LAST-MAINT-BY.
00363      MOVE EIBTIME                TO  MA-LAST-MAINT-HHMMSS.
00364      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00365      MOVE '5'                    TO  DC-OPTION-CODE.
00366      PERFORM 9700-DATE-LINK.
00367      IF DATE-CONVERSION-ERROR
00368          MOVE LOW-VALUES         TO  MA-RECORD-ADD-DT
00369                                      MA-LAST-MAINT-DT
00370      ELSE
00371          MOVE DC-BIN-DATE-1      TO  MA-RECORD-ADD-DT
00372                                      MA-LAST-MAINT-DT.
00373
00374      IF  GCODE1L GREATER ZERO
00375          MOVE GCODE1I            TO  MA-QUALIFY-CODE-1
00376      ELSE
00377          MOVE SPACES             TO  MA-QUALIFY-CODE-1.
00378
00379      IF  GCODE2L GREATER ZERO
00380          MOVE GCODE2I            TO  MA-QUALIFY-CODE-2
00381      ELSE
00382          MOVE SPACES             TO  MA-QUALIFY-CODE-2.
00383
00384      IF  GCODE3L GREATER ZERO
00385          MOVE GCODE3I            TO  MA-QUALIFY-CODE-3
00386      ELSE
00387          MOVE SPACES             TO  MA-QUALIFY-CODE-3.
00388
00389      IF GCODE4L GREATER ZERO
00390          MOVE GCODE4I            TO  MA-QUALIFY-CODE-4
00391      ELSE
00392          MOVE SPACES             TO  MA-QUALIFY-CODE-4.
00393
00394      IF GCODE5L GREATER ZERO
00395          MOVE GCODE5I            TO  MA-QUALIFY-CODE-5
00396      ELSE
00397          MOVE SPACES             TO  MA-QUALIFY-CODE-5.
00398
00399      IF GLNAMEL GREATER ZERO
00400          MOVE GLNAMEI            TO  MA-INSURED-LAST-NAME
00401      ELSE
00402          MOVE SPACES             TO  MA-INSURED-LAST-NAME.
00403
00404      IF GFNAMEL GREATER ZERO
00405          MOVE GFNAMEI            TO  MA-INSURED-FIRST-NAME
00406      ELSE
00407          MOVE SPACES             TO  MA-INSURED-FIRST-NAME.
00408
00409      IF GINITL GREATER ZERO
00410          MOVE GINITI             TO  MA-INSURED-MIDDLE-INIT
00411      ELSE
00412          MOVE SPACES             TO  MA-INSURED-MIDDLE-INIT.
00413
00414      IF GADDR1L GREATER ZERO
00415          MOVE GADDR1I            TO  MA-ADDRESS-LINE-1
00416      ELSE
00417          MOVE SPACES             TO  MA-ADDRESS-LINE-1.
00418
00419      IF GADDR2L GREATER ZERO
00420          MOVE GADDR2I            TO  MA-ADDRESS-LINE-2
00421      ELSE
00422          MOVE SPACES             TO  MA-ADDRESS-LINE-2.
00423
00424      IF GCITYL GREATER ZERO
00425          MOVE GCITYI           TO  MA-CITY
00426      ELSE
00427          MOVE SPACES             TO  MA-CITY.

00424      IF GISTATEL GREATER ZERO
00425          MOVE GISTATEI           TO  MA-ADDR-STATE
00426      ELSE
00427          MOVE SPACES             TO  MA-ADDR-STATE.

00429      IF GZIP1L GREATER ZERO
00430          MOVE GZIP1I             TO  W-ZIP-TEST
00431
00432          IF  W-CANADIAN-POST-CODE
00433              MOVE GZIP1I         TO  MA-CAN-POSTAL-CODE-1
00434
00435          ELSE
00436              MOVE GZIP1I         TO  MA-ZIP-CODE
00437
00438      ELSE
00439          MOVE ZEROS              TO  MA-ZIP-CODE.
00440
00441      IF GZIP2L GREATER ZERO
00442
00443          IF  MA-CANADIAN-POST-CODE
00444              MOVE GZIP2I         TO  MA-CAN-POSTAL-CODE-2
00445
00446          ELSE
00447              MOVE GZIP2I         TO  MA-ZIP-PLUS4
00448
00449      ELSE
00450          MOVE ZEROS              TO  MA-ZIP-PLUS4.
00451
00452      IF GPHONEL GREATER ZERO
00453          MOVE WS-PHONE           TO  MA-PHONE-NO
00454      ELSE
00455          MOVE ZEROS              TO  MA-PHONE-NO.
00456
070313*    IF GSSNOL GREATER ZERO
070313*        MOVE GSSNOI             TO  MA-INSURED-SOC-SEC-NO
070313*    ELSE
070313*        MOVE ZEROS              TO  MA-INSURED-SOC-SEC-NO.
00461
00462      IF GAGEL GREATER ZERO
00463          MOVE GAGEI              TO  MA-INSURED-ISSUE-AGE
00464      ELSE
00465          MOVE ZEROS              TO  MA-INSURED-ISSUE-AGE.
00466
00467      IF GBDTL GREATER ZERO
00468          MOVE WS-BIRTHDT         TO  MA-INSURED-BIRTH-DT
00469      ELSE
00470          MOVE LOW-VALUES         TO  MA-INSURED-BIRTH-DT.

090408     IF GJDOBL > ZERO
090408        MOVE WS-JNT-BIRTHDT      TO MA-JOINT-BIRTH-DT
090408     ELSE
090408        MOVE LOW-VALUES          TO MA-JOINT-BIRTH-DT
090408     END-IF

00472      IF GSEXL GREATER ZERO
00473          MOVE GSEXI              TO  MA-INSURED-SEX
00474      ELSE
00475          MOVE SPACES             TO  MA-INSURED-SEX.

           IF GBNAMEL > 0
              MOVE GBNAMEI             TO MA-CRED-BENE-NAME
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-NAME
           END-IF
      
           IF GBADD1L > 0
              MOVE GBADD1I             TO MA-CRED-BENE-ADDR
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-ADDR
           END-IF

           IF GBADD2L > 0
              MOVE GBADD2I             TO MA-CRED-BENE-ADDR2
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-ADDR2
           END-IF

           IF GBCITYL > 0
              MOVE GBCITYI            TO MA-CRED-BENE-CITY
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-CITY
           END-IF

           IF GBSTATEL > 0
              MOVE GBSTATEI            TO MA-CRED-BENE-STATE
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-STATE
           END-IF

           IF GBZIP1L > 0
              MOVE GZIP1I              TO W-ZIP-TEST
              IF W-CANADIAN-POST-CODE
                 MOVE GBZIP1I          TO MA-CB-CAN-POSTAL-CODE-1
              ELSE
                 MOVE GBZIP1I          TO MA-CB-ZIP-CODE
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-CODE
           END-IF
      
           IF GBZIP2L > 0
              IF MA-CB-CANADIAN-POST-CODE
                 MOVE GBZIP2I          TO MA-CB-CAN-POSTAL-CODE-2
              ELSE
                 MOVE GBZIP2I          TO MA-CB-ZIP-PLUS4
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-PLUS4
           END-IF

           .
00477  1700-WRITE-RECORD.
00478
00479      PERFORM 6400-WRITE-MAIL-RECORD.
00480
00481      IF  DUPLICATE-RECORD-FOUND
00482          GO TO 1800-DUPLICATE-RECORD.
00483
00484      MOVE ER-0000                TO  EMI-ERROR.
00485      PERFORM 9900-ERROR-FORMAT.
00486      PERFORM 7000-FORMAT-SCREEN.
00487      PERFORM 4000-CERTIFICATE-UPDATE.
00488      GO TO 8100-SEND-INITIAL-MAP.
00489
00490  1800-DUPLICATE-RECORD.
00491      MOVE ER-2199                TO  EMI-ERROR.
00492      PERFORM 9900-ERROR-FORMAT.
00493      PERFORM 7000-FORMAT-SCREEN.
00494      GO TO 8100-SEND-INITIAL-MAP.
00495
00496  1900-EXIT.
00497      EXIT.
00498
00499      EJECT
00500
00501  2000-CHANGE-RECORD   SECTION.
00502      PERFORM 3000-EDIT-MAIL-DATA.

100217     if (emi-fatal-ctr = zeros)
100217        and (emi-forcable-ctr = 1)
100217        and (emi-error = er-3061)
100217        and (eibaid = dfhpf7)
100217        move '2'                 to emi-switch1
100217        continue
100217     else
00504         IF NOT EMI-NO-ERRORS
00505            GO TO 8200-SEND-DATAONLY
100217        end-if
100217     end-if

00507      MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
00508      MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
00509      MOVE  PI-STATE              TO  WS-CM-STATE.
00510      MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
00511      MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
00512      MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
00513      MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
00514      MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
00515
00516      PERFORM 6300-READ-MAIL-FILE-UPDT.
00517
00518      IF  RECORD-NOT-FOUND
00519          GO TO 1000-ADD-RECORD.
00520
00521      MOVE PI-PROCESSOR-ID        TO  MA-LAST-MAINT-BY.
00522      MOVE EIBTIME                TO  MA-LAST-MAINT-HHMMSS.
00523      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00524      MOVE '5'                    TO  DC-OPTION-CODE.
00525
00526      PERFORM 9700-DATE-LINK.
00527      IF DATE-CONVERSION-ERROR
00528          MOVE LOW-VALUES         TO  MA-LAST-MAINT-DT
00529      ELSE
00530          MOVE DC-BIN-DATE-1      TO  MA-LAST-MAINT-DT.
00531
00532      IF  GCODE1L GREATER ZERO
00533          MOVE GCODE1I            TO  MA-QUALIFY-CODE-1.
00534      IF  GCODE2L GREATER ZERO
00535          MOVE GCODE2I            TO  MA-QUALIFY-CODE-2.
00536      IF  GCODE3L GREATER ZERO
00537          MOVE GCODE3I            TO  MA-QUALIFY-CODE-3.
00538      IF  GCODE4L GREATER ZERO
00539          MOVE GCODE4I            TO  MA-QUALIFY-CODE-4.
00540      IF  GCODE5L GREATER ZERO
00541          MOVE GCODE5I            TO  MA-QUALIFY-CODE-5.
00542      IF  GLNAMEL GREATER ZERO
00543          MOVE GLNAMEI            TO  MA-INSURED-LAST-NAME.
00544      IF  GFNAMEL GREATER ZERO
00545          MOVE GFNAMEI            TO  MA-INSURED-FIRST-NAME.
00546      IF  GINITL GREATER ZERO
00547          MOVE GINITI             TO  MA-INSURED-MIDDLE-INIT.
00548      IF  GADDR1L GREATER ZERO
00549          MOVE GADDR1I            TO  MA-ADDRESS-LINE-1.
00550      IF  GADDR2L GREATER ZERO
00551          MOVE GADDR2I            TO  MA-ADDRESS-LINE-2.
00552      IF  GCITYL GREATER ZERO
00553          MOVE GCITYI           TO  MA-CITY.

00552      IF  GISTATEL GREATER ZERO
00553          MOVE GISTATEI          TO  MA-ADDR-STATE.

100217     IF GZIP1L <> 0
00556          MOVE GZIP1I             TO  W-ZIP-TEST
00557
00558          IF  W-CANADIAN-POST-CODE
00559              MOVE GZIP1I         TO  MA-CAN-POSTAL-CODE-1
00560
00561          ELSE
00562              MOVE GZIP1I         TO  MA-ZIP-CODE
00563
00564      ELSE
00565          MOVE ZEROS              TO  MA-ZIP-CODE.
00566
00567      IF GZIP2L GREATER ZERO
00568
00569          IF  MA-CANADIAN-POST-CODE
00570              MOVE GZIP2I         TO  MA-CAN-POSTAL-CODE-2
00571
00572          ELSE
00573              MOVE GZIP2I         TO  MA-ZIP-PLUS4
00574
00575      ELSE
00576          MOVE ZEROS              TO  MA-ZIP-PLUS4.
00577
00578      IF GPHONEL GREATER ZERO
00579          MOVE WS-PHONE           TO  MA-PHONE-NO.
070313*    IF  GSSNOL GREATER ZERO
070313*        MOVE GSSNOI             TO  MA-INSURED-SOC-SEC-NO.
00582      IF  GAGEL GREATER ZERO
00583          MOVE GAGEI              TO  MA-INSURED-ISSUE-AGE.
00584      IF  GBDTL GREATER ZERO
00585          MOVE WS-BIRTHDT         TO  MA-INSURED-BIRTH-DT.
090408     IF GJDOBL > 0
090408        MOVE WS-JNT-BIRTHDT      TO MA-JOINT-BIRTH-DT
090408     END-IF
00586      IF  GSEXL GREATER ZERO
00587          MOVE GSEXI              TO  MA-INSURED-SEX.

           IF GBNAMEL > 0
              MOVE GBNAMEI             TO MA-CRED-BENE-NAME
           END-IF

           IF GBADD1L > 0
              MOVE GBADD1I             TO MA-CRED-BENE-ADDR
           END-IF

           IF GBADD2L > 0
              MOVE GBADD2I             TO MA-CRED-BENE-ADDR2
           END-IF

           IF GBCITYL > 0
              MOVE GBCITYI            TO MA-CRED-BENE-CITY
           END-IF

           IF GBSTATEL > 0
              MOVE GBSTATEI            TO MA-CRED-BENE-STATE
           END-IF

           IF GBZIP1L > 0
              MOVE GBZIP1I             TO W-ZIP-TEST
              IF W-CANADIAN-POST-CODE
                 MOVE GBZIP1I          TO MA-CB-CAN-POSTAL-CODE-1
              ELSE
                 MOVE GBZIP1I          TO MA-CB-ZIP-CODE
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-CODE
           END-IF
      
           IF GBZIP2L > 0
              IF MA-CB-CANADIAN-POST-CODE
                 MOVE GBZIP2I          TO MA-CB-CAN-POSTAL-CODE-2
              ELSE
                 MOVE GBZIP2I          TO MA-CB-ZIP-PLUS4
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-PLUS4
           END-IF

           .
00589  2700-REWRITE-RECORD.

00591      PERFORM 6500-REWRITE-MAIL-RECORD.

020816     IF PI-COMPANY-ID NOT = 'DCC' OR 'VPP'
              IF GBNAMEL > 0
                 EXEC CICS READ
                    EQUAL
                    DATASET   (ELCERT-ID)
                    SET       (ADDRESS OF CERTIFICATE-MASTER)
                    RIDFLD    (WS-CM-CONTROL-PRIMARY)
                    UPDATE
                    RESP       (WS-RESPONSE)
                 END-EXEC
                 IF RESP-NORMAL
                    IF GBNAMEI NOT = CM-BENEFICIARY
                       MOVE GBNAMEI       TO CM-BENEFICIARY
                       EXEC CICS REWRITE
                          FROM    (CERTIFICATE-MASTER)
                          DATASET (ELCERT-ID)
                          RESP    (WS-RESPONSE)
                       END-EXEC
                    ELSE
                       EXEC CICS UNLOCK
                          DATASET  (ELCERT-ID)
                          RESP     (WS-RESPONSE)
                       END-EXEC
                    END-IF
                 END-IF
              END-IF
           END-IF

00593      MOVE ER-0000                TO  EMI-ERROR.
00594      PERFORM 9900-ERROR-FORMAT.
00595      PERFORM 7000-FORMAT-SCREEN.
00596      GO TO 8100-SEND-INITIAL-MAP.
00597
00598  2900-EXIT.
00599      EXIT.
00600
00601      EJECT
00602
00603  3000-EDIT-MAIL-DATA   SECTION.
00604      IF GAGEL NOT = 0
00605          IF GAGEI NOT NUMERIC
00606              MOVE -1             TO  GAGEL
00607              MOVE AL-UABON       TO  GAGEA
00608              MOVE ER-2187        TO  EMI-ERROR
00609              PERFORM 9900-ERROR-FORMAT.
00610
00611      IF GBDTL NOT = 0
00612         MOVE GBDTI               TO DEEDIT-FIELD
00613         PERFORM 8600-DEEDIT
00614         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
00615         MOVE '4'                 TO DC-OPTION-CODE
00616         PERFORM 9700-DATE-LINK
00617         IF DATE-CONVERSION-ERROR
00618            MOVE -1               TO GBDTL
00619            MOVE AL-UABON         TO GBDTA
00620            MOVE ER-0220          TO EMI-ERROR
00621            PERFORM 9900-ERROR-FORMAT
00622         ELSE
042221           if dc-bin-date-1 > pi-cert-eff-dt
042221              MOVE DEEDIT-FIELD-V0
042221                                 TO DC-GREG-DATE-1-MDY
042221              MOVE '4'           TO DC-OPTION-CODE
042221              move '1'           to dc-century-adjustment
042221              PERFORM 9700-DATE-LINK
042221              if no-conversion-error
042221                 MOVE DC-BIN-DATE-1
042221                                 TO WS-BIRTHDT
042221              end-if
042221           else
042221              MOVE DC-BIN-DATE-1 TO WS-BIRTHDT
042221           end-if
042221        end-if
042221     end-if

090408     IF GJDOBL NOT = 0
090408         MOVE GJDOBI          TO  DEEDIT-FIELD
090408         PERFORM 8600-DEEDIT
090408         MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
090408         MOVE '4'                TO  DC-OPTION-CODE
090408         PERFORM 9700-DATE-LINK
090408         IF DATE-CONVERSION-ERROR
090408             MOVE -1             TO  GJDOBL
090408             MOVE AL-UABON       TO  GJDOBA
090408             MOVE ER-0220        TO  EMI-ERROR
090408             PERFORM 9900-ERROR-FORMAT
090408         ELSE
042221           if dc-bin-date-1 > pi-cert-eff-dt
042221              MOVE DEEDIT-FIELD-V0
042221                                 TO DC-GREG-DATE-1-MDY
042221              MOVE '4'           TO DC-OPTION-CODE
042221              move '1'           to dc-century-adjustment
042221              PERFORM 9700-DATE-LINK
042221              if no-conversion-error
042221                 MOVE DC-BIN-DATE-1
042221                                 TO WS-JNT-BIRTHDT
042221              end-if
042221           else
042221              MOVE DC-BIN-DATE-1 TO WS-JNT-BIRTHDT
042221           end-if
042221        end-if
042221     end-if

00625      IF GSEXL NOT = 0
00626          IF GSEXI NOT = 'M' AND 'F'
00627              MOVE -1             TO  GSEXL
00628              MOVE AL-UABON       TO  GSEXA
00629              MOVE ER-0219        TO  EMI-ERROR
00630              PERFORM 9900-ERROR-FORMAT.
00631
00632      IF GPHONEL NOT = 0
00633          MOVE GPHONEI            TO  DEEDIT-FIELD
00634          PERFORM 8600-DEEDIT
00635          MOVE DEEDIT-FIELD-V0    TO  WS-PHONE.
00636
120513     if  (GCITYI NOT GREATER THAN SPACES)
120513             or
120513         (GISTATEI NOT GREATER THAN SPACES)
00642          MOVE -1                 TO  GCITYL
                                           GISTATEL
00645          MOVE AL-UABON           TO  GCITYA
                                           GISTATEA
00648          MOVE ER-7428            TO  EMI-ERROR
00649          PERFORM 9900-ERROR-FORMAT.

           IF GISTATEL > +0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE GISTATEI            TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 MOVE AL-UANON         TO GISTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO GISTATEL
                 MOVE AL-UABON         TO GISTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

100217     if gzip1l <> zeros
100217        if (gzip1i numeric)
100217           and (gzip1i <> zeros)
100217           continue
100217        else
100217           move er-2050          to emi-error
100217           move -1               to gzip1l
100217           move al-uabon         to gzip1a
100217           perform 9900-error-format
100217                                 thru 9900-exit
100217        end-if
100217     end-if
100217
100217     IF EMI-NO-ERRORS
100217        move spaces              to zipcd-pass-area
100217        move gzip1i              to pa-zip
100217        move gcityi              to pa-city
100217        move gistatei            to pa-state
100217        perform 7790-call-zip-verify
100217                                 thru 7790-exit
100217        if pa-errorcode-zip (1:1) = ' '
100217           continue
100217        else
100217           MOVE ER-3061          TO emi-ERROR
100217           move -1               to gzip1l
100217           PERFORM 9900-ERROR-FORMAT
100217                                 THRU 9900-EXIT
100217        end-if
100217     end-if

           IF (GBSTATEL > +0)
              AND (GBSTATEI NOT = SPACES AND LOW-VALUES)
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE GBSTATEI            TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 MOVE AL-UANON         TO GBSTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO GBSTATEL
                 MOVE AL-UABON         TO GBSTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           .
00653  4000-CERTIFICATE-UPDATE         SECTION.
00654      EXEC CICS HANDLE CONDITION
00655          NOTFND   (4400-NOT-FOUND)
00656      END-EXEC.
00657
00658      EXEC CICS READ
00659          EQUAL
00660          DATASET   (ELCERT-ID)
00661          SET       (ADDRESS OF CERTIFICATE-MASTER)
00662          RIDFLD    (WS-CM-CONTROL-PRIMARY)
00663          UPDATE
00664      END-EXEC.
00665
00666  4200-REWRITE-CERT-MASTER.
00667      MOVE '1'                    TO  CM-INSURED-ADDRESS-SW

020816     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
              IF GBNAMEL > 0
                 IF GBNAMEI NOT = CM-BENEFICIARY
                    MOVE GBNAMEI          TO CM-BENEFICIARY
                 END-IF
              END-IF
           END-IF

00669      EXEC CICS REWRITE
00670          FROM      (CERTIFICATE-MASTER)
00671          DATASET   (ELCERT-ID)
00672      END-EXEC.
00673
00674      GO TO 4500-EXIT.
00675
00676  4400-NOT-FOUND.
00677      MOVE -1                     TO  GMAINTL.
00678      MOVE ER-0142                TO  EMI-ERROR.
00679      PERFORM 9900-ERROR-FORMAT.
00680      GO TO 8200-SEND-DATAONLY.
00681
00682  4500-EXIT.
00683      EXIT.
00684
00685      EJECT
00686
00687  6200-READ-MAIL-FILE             SECTION.
00688      EXEC CICS HANDLE CONDITION
00689          NOTFND   (6250-NOT-FOUND)
00690      END-EXEC.
00691
00692      EXEC CICS READ
00693          EQUAL
00694          DATASET   (ERMAIL-ID)
00695          SET       (ADDRESS OF MAILING-DATA)
00696          RIDFLD    (WS-CM-CONTROL-PRIMARY)
00697      END-EXEC.
00698
00699      GO TO 6290-EXIT.
00700
00701  6250-NOT-FOUND.
00702      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00703
00704  6290-EXIT.
00705       EXIT.
00706
00707      EJECT
00708
00709  6300-READ-MAIL-FILE-UPDT        SECTION.
00710      EXEC CICS HANDLE CONDITION
00711          NOTFND   (6350-NOT-FOUND)
00712      END-EXEC.
00713
00714      EXEC CICS READ
00715          EQUAL
00716          DATASET   (ERMAIL-ID)
00717          SET       (ADDRESS OF MAILING-DATA)
00718          RIDFLD    (WS-CM-CONTROL-PRIMARY)
00719          UPDATE
00720      END-EXEC.
00721
00722      GO TO 6390-EXIT.
00723
00724  6350-NOT-FOUND.
00725      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00726
00727  6390-EXIT.
00728       EXIT.
00729
00730       EJECT
00731
00732  6400-WRITE-MAIL-RECORD          SECTION.
00733      EXEC CICS HANDLE CONDITION
00734          DUPREC   (6450-DUPLICATE-RECORD)
00735      END-EXEC.
00736
00737      EXEC CICS WRITE
00738          FROM      (MAILING-DATA)
00739          DATASET   (ERMAIL-ID)
00740          RIDFLD    (WS-CM-CONTROL-PRIMARY)
00741      END-EXEC.
00742
00743      GO TO 6490-EXIT.
00744
00745  6450-DUPLICATE-RECORD.
00746      MOVE 'Y'                    TO  WS-DUPREC-SW.
00747
00748  6490-EXIT.
00749      EXIT.
00750
00751      EJECT
00752
00753  6500-REWRITE-MAIL-RECORD        SECTION.
00754      EXEC CICS REWRITE
00755          FROM      (MAILING-DATA)
00756          DATASET   (ERMAIL-ID)
00757      END-EXEC.
00758
00759  6590-EXIT.
00760      EXIT.
00761
00762      EJECT
00763
00764  6600-DELETE-MAIL-RECORD         SECTION.
00765      EXEC CICS HANDLE CONDITION
00766          NOTFND (6650-RECORD-PREVS-DELETED)
00767      END-EXEC.
00768
00769      EXEC CICS DELETE
00770          DATASET   (ERMAIL-ID)
00771      END-EXEC.
00772
00773      GO TO 6690-EXIT.
00774
00775  6650-RECORD-PREVS-DELETED.
00776      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00777
00778  6690-EXIT.
00779      EXIT.
00780
00781      EJECT
00782
00783  7000-FORMAT-SCREEN              SECTION.
00784      MOVE LOW-VALUES             TO  EL127GO.
00785
100217     move  -1                    to  gmaintl
00786      MOVE  PI-CARRIER            TO  GCARIERO.
00787      MOVE  PI-GROUPING           TO  GGROUPO.
00788      MOVE  PI-STATE              TO  GSTATEO.
00789      MOVE  PI-ACCOUNT            TO  GACCTNOO.
00790      MOVE  PI-CERT-PRIME         TO  GCERTNOO.
00791      MOVE  PI-CERT-SFX           TO  GCRTSFXO.
00792      MOVE  ' '                   TO  DC-OPTION-CODE.
00793      MOVE  PI-CERT-EFF-DT        TO  DC-BIN-DATE-1.
00794      PERFORM 9700-DATE-LINK.
00795      MOVE DC-GREG-DATE-1-EDIT    TO  GEFFDTO.
00796
00797      MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
00798      MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
00799      MOVE  PI-STATE              TO  WS-CM-STATE.
00800      MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
00801      MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
00802      MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
00803      MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
00804      MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
00805
00806      PERFORM 6200-READ-MAIL-FILE.
00807
00808      IF RECORD-NOT-FOUND
00809          IF PI-PEND-SW = 'P'
00810              MOVE 'S'            TO  GMAINTI
00811              MOVE AL-SANON       TO  GMAINTA
00812              MOVE ER-7049        TO  EMI-ERROR
00813              PERFORM 9900-ERROR-FORMAT
100217             GO TO 7000-EXIT
00815          ELSE
00816              MOVE 'A'            TO  GMAINTI
00817              MOVE AL-UANON       TO  GMAINTA
100217             GO TO 7000-EXIT.
00819
00820      MOVE 'C'                    TO  GMAINTO.
00821      MOVE AL-UANON               TO  GMAINTA.
00822
00823      IF PI-PEND-SW = 'P'
00824          MOVE 'S'                TO  GMAINTI
00825          MOVE AL-SANON           TO  GMAINTA
00826          MOVE ER-7049            TO  EMI-ERROR
00827          PERFORM 9900-ERROR-FORMAT.
00828
00829      MOVE MA-LAST-MAINT-BY       TO  GLSTUSRO.
00830      MOVE MA-LAST-MAINT-HHMMSS   TO  TIME-IN.
00831      MOVE TIME-OUT               TO  GLSTIMEO.
00832      MOVE ' '                    TO  DC-OPTION-CODE.
00833      MOVE MA-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
00834      PERFORM 9700-DATE-LINK.
00835      IF DATE-CONVERSION-ERROR
00836          MOVE ZEROS                  TO  GLSTDTO
00837      ELSE
00838          MOVE DC-GREG-DATE-1-EDIT    TO  GLSTDTO.
00839
00840      MOVE MA-RECORD-ADDED-BY     TO  GADDBYO.
00841      MOVE ' '                    TO  DC-OPTION-CODE.
00842      MOVE MA-RECORD-ADD-DT       TO  DC-BIN-DATE-1.
00843      PERFORM 9700-DATE-LINK.
00844      IF DATE-CONVERSION-ERROR
00845          MOVE ZEROS                  TO  GADDDTO
00846      ELSE
00847          MOVE DC-GREG-DATE-1-EDIT    TO  GADDDTO.
00848
00849      MOVE MA-QUALIFY-CODE-1      TO  GCODE1O.
00850      MOVE MA-QUALIFY-CODE-2      TO  GCODE2O.
00851      MOVE MA-QUALIFY-CODE-3      TO  GCODE3O.
00852      MOVE MA-QUALIFY-CODE-4      TO  GCODE4O.
00853      MOVE MA-QUALIFY-CODE-5      TO  GCODE5O.
00854      MOVE MA-INSURED-LAST-NAME   TO  GLNAMEO.
00855      MOVE MA-INSURED-FIRST-NAME  TO  GFNAMEO.
00856      MOVE MA-INSURED-MIDDLE-INIT TO  GINITO.
00857      MOVE MA-ADDRESS-LINE-1      TO  GADDR1O.
00858      MOVE MA-ADDRESS-LINE-2      TO  GADDR2O.
00859      MOVE MA-CITY                TO  GCITYO.
           MOVE MA-ADDR-STATE          TO  GISTATEO
00860
00861      IF  MA-CANADIAN-POST-CODE
00862          MOVE MA-CAN-POSTAL-CODE-1 TO GZIP1O
00863
00864      ELSE
00865          MOVE MA-ZIP-CODE        TO  GZIP1O.
00866
00867      IF  MA-CANADIAN-POST-CODE
00868          MOVE MA-CAN-POSTAL-CODE-2
00869                                  TO  GZIP2O
00870
00871      ELSE
00872          MOVE MA-ZIP-PLUS4       TO  GZIP2O.
00873
070313*    MOVE MA-INSURED-SOC-SEC-NO  TO  GSSNOO.
00875
00876      IF MA-INSURED-SEX NOT = SPACES
00877          MOVE MA-INSURED-SEX     TO  GSEXO
00878          MOVE AL-UANON           TO  GSEXA.
00879
00880      IF MA-INSURED-ISSUE-AGE NOT NUMERIC OR
00881          MA-INSURED-ISSUE-AGE = ZEROS
00882              MOVE LOW-VALUES             TO  GAGEO
00883              MOVE AL-UANOF               TO  GAGEA
00884      ELSE
00885              MOVE AL-UANON               TO  GAGEA
00886              MOVE MA-INSURED-ISSUE-AGE   TO  GAGEO.
00887
00888      IF MA-INSURED-BIRTH-DT IS EQUAL TO LOW-VALUES OR SPACES
00889          MOVE SPACES                 TO  GBDTO
00890          MOVE AL-UANOF               TO  GBDTA
00891      ELSE
00892          MOVE ' '                    TO  DC-OPTION-CODE
00893          MOVE MA-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1
00894          PERFORM 9700-DATE-LINK
00895          IF DATE-CONVERSION-ERROR
00896              MOVE LOW-VALUES             TO  GBDTO
00897              MOVE AL-UANOF               TO  GBDTA
00898          ELSE
00899              MOVE AL-UANON               TO  GBDTA
00900              MOVE DC-GREG-DATE-1-EDIT    TO  GBDTO.

090408     IF MA-JOINT-BIRTH-DT = LOW-VALUES OR SPACES
090408        MOVE SPACES              TO GJDOBO
090408        MOVE AL-UANOF            TO GJDOBA
090408     ELSE
090408        MOVE ' '                 TO DC-OPTION-CODE
090408        MOVE MA-JOINT-BIRTH-DT   TO DC-BIN-DATE-1
090408        PERFORM 9700-DATE-LINK
090408        IF DATE-CONVERSION-ERROR
090408           MOVE LOW-VALUES       TO GJDOBO
090408           MOVE AL-UANOF         TO GJDOBA
090408        ELSE
090408           MOVE AL-UANON         TO GJDOBA
090408           MOVE DC-GREG-DATE-1-EDIT
090408                                 TO  GJDOBO
090408        END-IF
090408     END-IF

00902      IF MA-PHONE-NO NOT NUMERIC OR
00903          MA-PHONE-NO = ZEROS
00904              MOVE LOW-VALUES             TO  GPHONEO
00905              MOVE AL-UANOF               TO  GPHONEA
00906      ELSE
00907              MOVE MA-PHONE-NO            TO  WS-PHONE
00908              MOVE WS-PHONE-AC            TO  ED-PHONE-AC
00909              MOVE WS-PHONE-EXT           TO  ED-PHONE-EXT
00910              MOVE WS-PHONE-NO            TO  ED-PHONE-NO
00911              MOVE '-'                    TO  ED-PHONE-DASH1
00912                                              ED-PHONE-DASH2
00913              MOVE EDITED-PHONE-NO        TO  GPHONEO
00914              MOVE AL-UANON               TO  GPHONEA
           END-IF

           MOVE MA-CRED-BENE-NAME      TO GBNAMEO
           MOVE MA-CRED-BENE-ADDR      TO GBADD1O
           MOVE MA-CRED-BENE-ADDR2     TO GBADD2O
           MOVE MA-CRED-BENE-CITY      TO GBCITYO
           MOVE MA-CRED-BENE-STATE     TO GBSTATEO

           IF MA-CB-CANADIAN-POST-CODE
              MOVE MA-CB-CAN-POSTAL-CODE-1
                                       TO GBZIP1O
           ELSE
              MOVE MA-CB-ZIP-CODE      TO GBZIP1O
           END-IF

           IF MA-CB-CANADIAN-POST-CODE
              MOVE MA-CB-CAN-POSTAL-CODE-2
                                       TO GBZIP2O
           ELSE
              MOVE MA-CB-ZIP-PLUS4     TO GBZIP2O
           END-IF

080406     MOVE +0 TO WS-SUB.
080406     PERFORM 7 TIMES
080406         ADD +1 TO WS-SUB
080406         IF MA-MAIL-TYPE (WS-SUB) NOT = LOW-VALUES AND SPACES
080406             IF MA-MAIL-TYPE (WS-SUB) = '1'
080406                MOVE '12M' TO WS-MAILED-TYPE (WS-SUB)
080406             ELSE
080406                MOVE 'EXP' TO WS-MAILED-TYPE (WS-SUB)
080406             END-IF
080406             IF MA-MAIL-STATUS (WS-SUB) = '3'
080406                MOVE 'NOT MAILED'  TO  WS-MAILED-STATUS (WS-SUB)
080406             ELSE 
080406                 IF MA-MAIL-STATUS (WS-SUB) = '2'
080406                     MOVE 'RETRN'  TO  WS-MAILED-STAT
080406                 ELSE
080406                     MOVE 'MAILD'  TO  WS-MAILED-STAT
080406                 END-IF
080406                 MOVE MA-MAIL-DATE (WS-SUB) TO  DC-BIN-DATE-1
080406                 PERFORM 9700-DATE-LINK
080406                 MOVE DC-GREG-DATE-1-EDIT   TO WS-MAILED-DATE
080406                 MOVE WS-MAILED-BLD-STATUS TO 
080406                                   WS-MAILED-STATUS (WS-SUB)
080406             END-IF
080406         ELSE
080406             MOVE SPACES TO WS-MAILED-TYPE (WS-SUB)
080406                            WS-MAILED-STATUS (WS-SUB)
080406         END-IF
080406     END-PERFORM.
080406
080406     MOVE WS-MAILED-TYPE (1)   TO GTYPE1O.
080406     MOVE WS-MAILED-STATUS (1) TO GSTAT1O.
080406     MOVE WS-MAILED-TYPE (2)   TO GTYPE2O.
080406     MOVE WS-MAILED-STATUS (2) TO GSTAT2O.
080406     MOVE WS-MAILED-TYPE (3)   TO GTYPE3O.
080406     MOVE WS-MAILED-STATUS (3) TO GSTAT3O.
080406     MOVE WS-MAILED-TYPE (4)   TO GTYPE4O.
080406     MOVE WS-MAILED-STATUS (4) TO GSTAT4O.
080406     MOVE WS-MAILED-TYPE (5)   TO GTYPE5O.
080406     MOVE WS-MAILED-STATUS (5) TO GSTAT5O.
080406     MOVE WS-MAILED-TYPE (6)   TO GTYPE6O.
080406     MOVE WS-MAILED-STATUS (6) TO GSTAT6O.
080406     MOVE WS-MAILED-TYPE (7)   TO GTYPE7O.
080406     MOVE WS-MAILED-STATUS (7) TO GSTAT7O.
00915
070313     move al-sadof               to  gssnoa
00916      MOVE AL-UANON               TO  GCODE1A   GCODE2A    GCODE3A
00917                                      GCODE4A   GCODE5A    GLNAMEA
070313                                     GFNAMEA   GINITA
00919                                      GADDR1A   GADDR2A    GCITYA
                                           GISTATEA
00920                                      GZIP1A    GZIP2A     GBNAMEA
                                           GBADD1A   GBADD2A    GBCITYA
                                           GBSTATEA
                                           GBZIP1A   GBZIP2A
           .
100217 7000-EXIT.
00923      EXIT.

100217 7790-CALL-ZIP-VERIFY section.
100217
100217     EXEC CICS LINK
100217         PROGRAM    ('WSZIPCD')
100217         COMMAREA   (ZIPCD-PASS-AREA)
100217         LENGTH     (ZIPCD-PASS-AREA-LEN)
100217     END-EXEC.
100217
100217 7790-EXIT.
100217     EXIT.

00927  8100-SEND-INITIAL-MAP SECTION.
00928      MOVE SAVE-DATE              TO  GDATEO.
00929      MOVE EIBTIME                TO  TIME-IN.
00930      MOVE TIME-OUT               TO  GTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00931      MOVE EMI-MESSAGE-AREA (1)   TO  GERMSG1O.
00932
00933      IF PI-PEND-SW = 'P'
00934          MOVE -1                 TO  GPFKEYL
00935      ELSE
00936          MOVE -1                 TO  GMAINTL.
00937
00938      EXEC CICS SEND
00939          MAP      (MAP-NAME)
00940          MAPSET   (MAPSET-NAME)
00941          FROM     (EL127GO)
00942          ERASE
00943          CURSOR
00944       END-EXEC.
00945
00946      GO TO 9100-RETURN-TRAN.
00947
00948  8200-SEND-DATAONLY     SECTION.
00949      MOVE SAVE-DATE              TO  GDATEO.
00950      MOVE EIBTIME                TO  TIME-IN.
00951      MOVE TIME-OUT               TO  GTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00952      MOVE EMI-MESSAGE-AREA (1)   TO  GERMSG1O.
00953
00954      IF PI-PEND-SW = 'P'
00955          MOVE -1                 TO  GPFKEYL.
00956 *    ELSE
00957 *        MOVE -1                 TO  GMAINTL.
00958
00959      EXEC CICS SEND
00960          MAP      (MAP-NAME)
00961          MAPSET   (MAPSET-NAME)
00962          FROM     (EL127GO)
00963          DATAONLY
00964          ERASEAUP
00965          CURSOR
00966       END-EXEC.
00967
00968      GO TO 9100-RETURN-TRAN.
00969
00970      EJECT
00971
00972  8300-SEND-TEXT         SECTION.
00973      EXEC CICS SEND TEXT
00974          FROM     (LOGOFF-TEXT)
00975          LENGTH   (LOGOFF-LENGTH)
00976          ERASE
00977          FREEKB
00978      END-EXEC.
00979
00980      EXEC CICS RETURN
00981      END-EXEC.
00982
00983      EJECT
00984
00985
00986  8600-DEEDIT           SECTION.
00987      EXEC CICS BIF DEEDIT
00988           FIELD   (DEEDIT-FIELD)
00989           LENGTH  (15)
00990      END-EXEC.
00991
00992  8800-UNAUTHORIZED-ACCESS        SECTION.
00993      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00994      GO TO 8300-SEND-TEXT.
00995
00996  8810-PF23              SECTION.
00997      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00998      MOVE XCTL-005               TO  PGM-NAME.
00999      GO TO 9300-XCTL.
01000
01001  9100-RETURN-TRAN       SECTION.
01002      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01003      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01004      EXEC CICS RETURN
01005          TRANSID    (TRANS-ID)
01006          COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01007          LENGTH     (PI-COMM-LENGTH)
01008      END-EXEC.
01009
01010  9200-RETURN-MAIN-MENU SECTION.
01011      MOVE XCTL-126               TO  PGM-NAME.
01012      GO TO 9300-XCTL.
01013
01014  9300-XCTL             SECTION.
01015      EXEC CICS XCTL
01016          PROGRAM    (PGM-NAME)
01017          COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01018          LENGTH     (PI-COMM-LENGTH)
01019      END-EXEC.
01020
01021  9400-CLEAR            SECTION.
01022      MOVE ' '                    TO  PI-PEND-SW.
01023      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01024      GO TO 9300-XCTL.
01025
01026  9500-PF12             SECTION.
01027      MOVE XCTL-010               TO  PGM-NAME.
01028      GO TO 9300-XCTL.
01029
01030  9600-PGMID-ERROR      SECTION.
01031      EXEC CICS HANDLE CONDITION
01032          PGMIDERR    (8300-SEND-TEXT)
01033      END-EXEC.
01034      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01035      MOVE ' '                    TO  PI-ENTRY-CD-1.
01036      MOVE XCTL-005               TO  PGM-NAME.
01037      MOVE PGM-NAME               TO  LOGOFF-PGM.
01038      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01039      GO TO 9300-XCTL.
01040
01041  9700-DATE-LINK         SECTION.
01042      MOVE LINK-ELDATCV           TO  PGM-NAME
01043      EXEC CICS LINK
01044          PROGRAM    (PGM-NAME)
01045          COMMAREA   (DATE-CONVERSION-DATA)
01046          LENGTH     (DC-COMM-LENGTH)
01047      END-EXEC.
01048
01049
01050  9900-ERROR-FORMAT       SECTION.
01051      IF NOT EMI-ERRORS-COMPLETE
01052          MOVE LINK-001           TO  PGM-NAME
01053          EXEC CICS LINK
01054              PROGRAM    (PGM-NAME)
01055              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01056              LENGTH     (EMI-COMM-LENGTH)
01057          END-EXEC.
01058
01059  9900-EXIT.
01060      EXIT.
01061
01062  9990-ABEND             SECTION.
01063      MOVE LINK-004               TO  PGM-NAME.
01064      MOVE DFHEIBLK               TO  EMI-LINE1.
01065
01066      EXEC CICS LINK
01067          PROGRAM   (PGM-NAME)
01068          COMMAREA  (EMI-LINE1)
01069          LENGTH    (72)
01070      END-EXEC.
01071
01072      MOVE -1                     TO  GMAINTL.
01073
01074      GO TO 8200-SEND-DATAONLY.
01075
01076  9995-SECURITY-VIOLATION.
01077                              COPY ELCSCTP.
01078
01079  9995-EXIT.
01080      EXIT.
01081
