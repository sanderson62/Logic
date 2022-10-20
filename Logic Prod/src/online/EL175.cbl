00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL175 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/09/95 10:08:20.
00007 *                            VMOD=2.028
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00024 *REMARKS.
00025 *        THIS PROGRAM IS USED TO INDICATE THE CHECKS BEING
00026 *    RELEASED TO PRINT.  EACH RELEASE CONSTITUTES A CONTROL GROUP
00027 *    THAT IS REFERENCED BY THE CHECK WRITER (EL177) AND THE CHECK
00028 *    PRINT STARTER (EL176).
00029
00030 *    SCREENS     - EL175A - CHECK RELEASE
00031
00032 *    ENTERED BY  - EL171  - REPORT MENU
00033
00034 *    EXIT TO     - EL171  - RESULT OF CLEAR
00035
00036 *    INPUT FILES - ELTRLR - PAYMENT TRAILERS
00037 *                  ELACTQ - ACTIVITY QUEUE
00038 *                  ELCNTL - CONTROL FILE
00039
00040 *    OUTPUT FILES - ELTRLR - PAYMENT TRAILERS
00041 *                   ELACTQ - ACTIVITY QUEUE
00042 *                   ELCNTL - CONTROL FILE
00043 *                   ELCHKQ - CHECK QUEUE
00044
00045 *    COMMAREA    - PASSED.
00046
00048 *    NARRATIVE   - ON FIRST ENTRY, A SKELETON SCREEN IS SENT SO
00049 *                  THE OPERATOR MAY ENTER THE QUALIFICATION DATA
00050 *                  FOR THE BATCH.  WHEN THE OPERATOR COMPLETES THE
00051 *                  DATA, THE PROGRAM IS RE-ENTERED TO SELECT THE
00052 *                  PAYMENTS AND CREATE THE CONTROL BATCH.
00053
00055 *                  THE PROCESSING STEPS INVOLVED IN CREATING
00056 *                  THE BATCH IS DETAILED AS FOLLOWS:
00057
00058 *                  1.  THE COMPANY CONTROL RECORD IS READ FOR
00059 *                      UPDATE.  A CHECK QUEUE SEQUENCE NUMBER IS
00060 *                      ASSIGNED USING CF-CO-CHECK-QUE-COUNTER + 1.
00061 *                      THE RECORD IS UPDATED WITH THE NEW NUMBER
00062 *                      AND FILED.
00063
00064 *                  2.  THE ACTIVITY QUE FILE IS READ SEQUENTIALLY
00065 *                      TO FIND THE CLAIMS HAVING PENDING PAYMENTS.
00066
00067 *                  3.  USING THE CONTROL FROM THE ACTIVITY QUE
00068 *                      RECORD, THE PAYMENT ACTIVITY TRAILER IS
00069 *                      READ SEQUENTIALLY LOOKING FOR PAYMENTS THAT
00070 *                      MEET THE QUALIFICATIONS INPUT.
00071
00072 *                      THE SEARCH OF THE TRAILER CHAIN WILL QUIT
00073 *                      UPON ENCOUNTERING PAYMENT TRAILERS CREATED
00074 *                      OVER 60 DAYS PRIOR TO THE CURRENT DATE.
00075 *                      THIS IS DONE TO SHORTEN THE SEARCH TIME.
00076 *                      THE CONSEQUENCE OF THIS IS THAT CHECKS MUST
00077 *                      BE WRITTEN WITHIN 60 DAYS OF ISSUE.
00078
00079 *                  4.  AS QUALIFYING PAYMENTS ARE FOUND, A CHECK
00080 *                      RECORD  IS CREATED AND FILED.  THE SEQUENCE
00081 *                      IN THIS RECORD (CQ-SEQUENCE-NO) IS A
00082 *                      COUNTER KEPT BY THE PROGRAM OF ENTRIES
00083 *                      SELECTED FOR PROCESSING.  THE SELECTED
00084 *                      TRAILER IS UPDATED WITH THE QUE POINTER AND
00085 *                      FILED BACK.
00086
00087 *                  5.  THE ACTIVITY QUEUE RECORD IS UPDATED TO
00088 *                      REFLECT THE PAYMENTS RELEASED.  IF ALL
00089 *                      PAYMENTS ARE RELEASED FOR A CONTROL AND NO
00090 *                      OTHER ACTIVITY IS PENDING, THE RECORD IS
00091 *                      DELETED.  IF OTHER ACTIVITY IS QUED OR THE
00092 *                      PAYMENT COUNT IS NOT ZERO, THE UPDATED
00093 *                      ACTIVITY QUEUE RECORD IS FILED.
00094
00095 *                  6.  WHEN THE RELEASE IS COMPLETE, A SKELETON IS
00096 *                      RETURNED ALONG WITH AN END-OF-JOB MESSAGE.
00097 *                      THE EOJ MESSAGE GIVES THE CONTROL NUMBER
00098 *                      ASSIGNED AND THE NUMBER OF PAYMENTS IN THE
00099 *                      BATCH.
00100
00101 *                  AT COMPLETION, THE OPERATOR MAY ENTER A NEW
00102 *                  QUALIFICATION OR PRESS CLEAR TO RETURN TO THE
00103 *                  REPORT MENU (EL171).
111502******************************************************************
111502*                   C H A N G E   L O G
111502*
111502* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111502*-----------------------------------------------------------------
111502*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111502* EFFECTIVE    NUMBER
111502*-----------------------------------------------------------------
111502* 111502    2002111800003  SMVA  STOP EXECUTION OF EX64
121902* 121902    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121203* 121203    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
031808* 031808    2006032200004  AJRA  ADD MESSAGE GIVING COUNT AND TOTA
031808*                                AMOUNT FOR UNAPPROVED PAYMENTS
090309* 090309    2008070200001  AJRA  DO NOT ALLOW USERS TO RELEASE ALL
090309*                                CHECKS. RELEASE DONE DURING CYCLE
092809* 092809    2009092500001  AJRA  FIX RELEASE WHEN FIRST CLAIM BLAN
080712* 080712  IR2012080300001  PEMA  CORRECT HANDLE CONDITION
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
111502******************************************************************
00104
00106  ENVIRONMENT DIVISION.
00107
00108  DATA DIVISION.
00109
00110  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00111
00112  77  FILLER  PIC X(32)  VALUE '********************************'.
00113  77  FILLER  PIC X(32)  VALUE '*    EL175 WORKING STORAGE     *'.
00114  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.028 **********'.
00115
00116 *                            COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00117
00118 *                            COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00119
00120 *                            COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00121
00122      EJECT
00123  01  WS-DATE-AREA.
00124      12  SAVE-DATE           PIC X(8)    VALUE SPACES.
00125      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00126
00127  01  DMD-DATE-YYYYMMDD.
00128      12  DMD-DECADE          PIC XX      VALUE SPACES.
00129      12  DMD-YYMMDD.
00130          16  DMD-YY          PIC XX      VALUE SPACES.
00131          16  DMD-MM          PIC XX      VALUE SPACES.
00132          16  DMD-DD          PIC XX      VALUE SPACES.
00133
00134  01  DMD-DATE-MMDDYYYY.
00135      12  DMD-MDY-MM          PIC XX      VALUE SPACES.
00136      12  DMD-MDY-SLASH1      PIC X       VALUE '/'.
00137      12  DMD-MDY-DD          PIC XX      VALUE SPACES.
00138      12  DMD-MDY-SLASH2      PIC X       VALUE '/'.
00139      12  DMD-MDY-DECADE      PIC XX      VALUE SPACES.
00140      12  DMD-MDY-YY          PIC XX      VALUE SPACES.
00141
00142  01  WS-MAIL-CODE.
00143      12  FILLER              PIC X.
00144      12  DMD-MAIL-CODE.
00145          16  WS-MAIL-4       PIC X(4).
00146          16  WS-MAIL-5       PIC X.
00147      12  FILLER              PIC X(4).
00148
00149      12  W-NAME-LAST             PIC  X(15).
00150      12  W-NAME-FIRST            PIC  X(15).
00151      12  W-NAME-MIDDLE.
00152          16  FILLER              PIC  X.
00153          16  W-NAME-MIDDLE-2     PIC  X.
00154          16  FILLER              PIC  X(13).
00155
00156  01  EN-EOB-LENGTH                   PIC S9(04) COMP VALUE +310.
00157  01  EB-EOB-LENGTH                   PIC S9(04) COMP VALUE +410.
00158
00159  01  WS-PREV-CLAIM.
00160      12  WS-PREV-COMPANY             PIC X.
00161      12  WS-PREV-CARRIER             PIC X.
00162      12  WS-PREV-CLAIM-NO            PIC X(7)   VALUE LOW-VALUES.
00163      12  WS-PREV-CERT-NO             PIC X(11).
00164      12  WS-PREV-SEQ-NO              PIC S9(4)  COMP VALUE ZEROS.
00165      12  WS-PREV-RECORD-TYPE         PIC X.
00166
00167  01  WS-HAVE-NOTE-HEADER             PIC X.
00168      88  HAVE-NOTE-HEADER                        VALUE 'Y'.
00169  01  WS-DMD-TAPE-PYMT-SW             PIC X.
00170      88  WS-DMD-TAPE-PYMT                        VALUE 'Y'.
090309 01  WS-ALL-CHECK-RELEASE            PIC X.
090309     88  WS-STOP-ALL-CHECKS                      VALUE 'Y'.
00171
00172  01  FILLER                          COMP-3.
00173      12  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.
00174      12  WS-UPDATE-SW                PIC S9      VALUE ZERO.
00175      12  WS-PAYMENT-COUNTER          PIC S9(3)   VALUE ZERO.
031808     12  WS-PMT-UNAPPROVED-COUNT     PIC S9(3)   VALUE ZERO.
00176
00177      12  TIME-IN                     PIC S9(7)   VALUE ZERO.
00178      12  TIME-OUT REDEFINES TIME-IN  PIC S999V9(4).
00179      12  WS-HHMM  REDEFINES TIME-IN  PIC S9(5)V99.
00180
00181      12  WS-ELACTQ-BROWSE-SW         PIC S9      VALUE ZERO.
00182      12  WS-ELTRLR-BROWSE-SW         PIC S9      VALUE ZERO.
00183      12  WS-ELNOTE-BROWSE-SW         PIC S9      VALUE ZERO.
00184
00185      12  WS-RELEASED-COUNT           PIC S9(5)   VALUE ZERO.
00186      12  WS-RELEASED-COUNT-2         PIC S9(5)   VALUE ZERO.
00187      12  WS-RELEASED-COUNT-3         PIC S9(5)   VALUE ZERO.
031808     12  WS-UNAPPROVED-COUNT         PIC S9(5)   VALUE ZERO.
00188
00189      12  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.
00190      12  WS-RELEASED-AMOUNT-2        PIC S9(9)V99 VALUE ZERO.
00191      12  WS-RELEASED-AMOUNT-3        PIC S9(9)V99 VALUE ZERO.
031808     12  WS-UNAPPROVED-AMOUNT        PIC S9(9)V99 VALUE ZERO.
00192
00193      12  WS-NON-CASH-REL-CNT         PIC S9(05)   VALUE ZERO.
00194      12  WS-NON-CASH-REL-AMT         PIC S9(9)V99 VALUE ZERO.
00195
00196      12  WS-DMD-PYMT-COUNT           PIC S9(5)   VALUE ZERO.
00197      12  WS-DMD-PYMT-AMOUNT          PIC S9(9)V99 VALUE ZERO.
00198      12  WS-DMD-DMO-COUNT            PIC S9(5)   VALUE ZERO.
00199      12  WS-DMD-DMO-AMOUNT           PIC S9(9)V99 VALUE ZERO.
00200
00201  01  FILLER     COMP  SYNC.
00202      12  SC-ITEM                     PIC S9(4)   VALUE +0001.
00203
00204      12  WS-KEY-LENGTH               PIC S9(4)   VALUE ZERO.
00205
00206      12  WS-CHECK-QUE-COUNTER        PIC S9(8)   VALUE ZERO.
00207      12  WS-CHECK-QUE-COUNTER-2      PIC S9(8)   VALUE ZERO.
00208      12  WS-CHECK-QUE-COUNTER-3      PIC S9(8)   VALUE ZERO.
00209
00210      12  WS-CHECK-COUNTER            PIC S9(4)   VALUE +10.
00211      12  WS-CHECK-COUNTER-2          PIC S9(4)   VALUE +10.
00212      12  WS-CHECK-COUNTER-3          PIC S9(4)   VALUE +10.
00213
00214      12  WS-DMO-LENGTH               PIC S9(4)   VALUE +108 COMP.
00215      12  WS-DCT-LENGTH               PIC S9(4)   VALUE +53  COMP.
00216
00217      EJECT
00218  01  FILLER.
00219      12  WS-CONTROL-FILE-KEY.
00220          16  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
00221          16  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.
00222          16  FILLER                  PIC XX      VALUE SPACES.
00223          16  WS-CFK-BENEFIT-NO       PIC XX      VALUE SPACES.
00224          16  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO
00225                                      COMP.
00226
00227      12  WS-ELMSTR-KEY               PIC X(20).
00228      12  WS-ACTIVITY-TRAILERS-KEY.
00229          16  WS-ATK-COMPANY-CD       PIC X.
00230          16  WS-ATK-CARRIER          PIC X.
00231          16  WS-ATK-CLAIM-NO         PIC X(7).
00232          16  WS-ATK-CERT-NO          PIC X(11).
00233          16  WS-ATK-SEQUENCE-NO      PIC S9(4) COMP.
00234
00235      12  WS-LAST-ACTIVITY-TRAILERS-KEY PIC X(22) VALUE LOW-VALUES.
00236
00237      12  WS-ACTIVITY-QUE-KEY.
00238          16  WS-AQK-COMPANY-CD       PIC X.
00239          16  WS-AQK-CARRIER          PIC X.
00240          16  WS-AQK-CLAIM-NO         PIC X(7).
00241          16  WS-AQK-CERT-NO          PIC X(11).
00242
00243      12  WS-LAST-ACTIVITY-QUE-KEY    PIC X(20) VALUE LOW-VALUES.
00244
00245      12  WS-CLAIM-MASTER-KEY.
00246          16  WS-CK-COMPANY-CD       PIC X.
00247          16  WS-CK-CARRIER          PIC X.
00248          16  WS-CK-CLAIM-NO         PIC X(7).
00249          16  WS-CK-CERT-NO          PIC X(11).
00250
00251      12  WS-NOTE-KEY.
00252          16  WS-EN-COMPANY-CD       PIC X.
00253          16  WS-EN-CARRIER          PIC X.
00254          16  WS-EN-CLAIM-NO         PIC X(7).
00255          16  WS-EN-CERT-NO          PIC X(11).
00256          16  WS-EN-PAYMENT-SEQ-NO   PIC S9(4)  COMP.
00257          16  WS-EN-RECORD-TYPE      PIC X.
00258
00259      12  W-NOTE-KEY.
00260          16  W-NOTE-COMP-CD         PIC X.
00261          16  W-NOTE-CERT-KEY.
00262              20  W-NOTE-CARRIER     PIC X.
00263              20  W-NOTE-GROUPING    PIC X(6).
00264              20  W-NOTE-STATE       PIC XX.
00265              20  W-NOTE-ACCOUNTG    PIC X(10).
00266              20  W-NOTE-EFF-DT      PIC XX.
00267              20  W-NOTE-CERT-NO     PIC X(11).
00268
00269      12  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL175S'.
00270
00271      12  WS-MAP-NAME                 PIC X(8)  VALUE 'EL175A'.
00272      12  FILLER REDEFINES WS-MAP-NAME.
00273          16  FILLER                  PIC XX.
00274          16  WS-MAP-NUMBER           PIC X(6).
00275
00276      12  THIS-PGM                    PIC X(8) VALUE 'EL175'.
00277
00278      12  EL001                       PIC X(8) VALUE 'EL001'.
00279      12  EL004                       PIC X(8) VALUE 'EL004'.
00280      12  EL005                       PIC X(8) VALUE 'EL005'.
00281      12  EL010                       PIC X(8) VALUE 'EL010'.
00282      12  EL126                       PIC X(8) VALUE 'EL126'.
00283      12  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.
00284
00285      12  WS-FORMS-PRINTER            PIC X(4) VALUE SPACES.
00286      12  WS-CHECK-QUE-DSID           PIC X(8) VALUE 'ELCHKQ'.
00287      12  WS-ENQ-COMPANY-ID           PIC X(3) VALUE SPACES.
00288      12  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.
00289      12  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
00290      12  WS-ACTIVITY-QUE-DSID        PIC X(8) VALUE 'ELACTQ'.
00291      12  WS-NOTE-DSID                PIC X(8) VALUE 'ELNOTE'.
00292      12  WS-EXTRACT-DSID             PIC X(8) VALUE 'ELNOTX'.
00293
00294      12  WS-SPACES                   PIC X       VALUE SPACES.
00295
00296      12  WS-CLAIM-NO                 PIC X(7)    VALUE SPACES.
00297      12  WS-CARRIER                  PIC X       VALUE SPACES.
00298
00299      12  WS-CERT-NO.
00300          16  WS-CERT-PRIME.
00301              20  WS-CERT-PRIME-1-3   PIC X(3)    VALUE SPACES.
00302              20  WS-CERT-PRIME-4-10  PIC X(7)    VALUE SPACES.
00303          16  WS-CERT-SFX             PIC X       VALUE SPACES.
00304
00305      12  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.
00306
00307      12  WS-TRANS-ID                 PIC X(4)    VALUE 'EX45'.
00308
00309      12  WS-TOTAL-LINE1.
00310          16  FILLER                  PIC X(14)   VALUE
00311              'CONTROL GROUP'.
00312          16  WS-TL1-CONTROL-GROUP    PIC 9(7)-   VALUE ZEROS.
00313          16  WS-TL1-RELEASE          PIC X(20)   VALUE
00314              ' RELEASED'.
00315
00316      12  WS-TOTAL-LINE2.
00317          16  WS-TL1-COUNT            PIC ZZ,ZZ9      VALUE ZEROS.
00318          16  FILLER                  PIC X(6)        VALUE
00319              ' CHECK'.
00320          16  WS-TL1-PLURAL           PIC X           VALUE
00321              'S'.
00322          16  FILLER                  PIC X(18)       VALUE
00323              ' IN THE AMOUNT OF'.
00324          16  WS-TL1-AMOUNT           PIC Z,ZZZ,ZZ9.99.
00325
031808     12  WS-TOTAL-LINE3.
031808         16  FILLER                  PIC X(20)   VALUE
031808             'AWAITING APPROVAL - '.
031808         16  WS-TL3-COUNT            PIC ZZ,ZZ9      VALUE ZEROS.
031808         16  FILLER                  PIC X(6)        VALUE
031808             ' CHECK'.
031808         16  WS-TL3-PLURAL           PIC X           VALUE
031808             'S'.
031808         16  FILLER                  PIC X(18)       VALUE
031808             ' IN THE AMOUNT OF'.
031808         16  WS-TL3-AMOUNT           PIC Z,ZZZ,ZZ9.99.
031808
00326      12  WS-TOTAL-LINE-1-3.
00327          16  FILLER                  PIC X(15)   VALUE
00328              'CONTROL GROUPS'.
00329          16  WS-TL1-3-CONTROL-GROUP  PIC 9(7)-.
00330          16  FILLER                  PIC X       VALUE SPACE.
00331          16  WS-TL2-3-CONTROL-GROUP  PIC 9(7)-.
00332          16  FILLER                  PIC X       VALUE SPACE.
00333          16  WS-TL3-3-CONTROL-GROUP  PIC 9(7)-.
00334          16  WS-TL-1-3-RELEASE       PIC X(20)   VALUE
00335              ' RELEASED'.
00336
00337      12  WS-TOTAL-LINE-2-3.
00338          16  WS-TL1-3-COUNT          PIC ZZZZ9.
00339          16  FILLER                  PIC X       VALUE SPACE.
00340          16  WS-TL2-3-COUNT          PIC ZZZZ9.
00341          16  FILLER                  PIC X       VALUE SPACE.
00342          16  WS-TL3-3-COUNT          PIC ZZZZ9.
00343          16  FILLER                  PIC X(18)   VALUE
00344              ' CKS IN THE AMT OF'.
00345          16  WS-TL1-3-AMOUNT         PIC ZZZZ,ZZ9.99.
00346          16  FILLER                  PIC X       VALUE SPACE.
00347          16  WS-TL2-3-AMOUNT         PIC ZZZZ,ZZ9.99.
00348          16  FILLER                  PIC X       VALUE SPACE.
00349          16  WS-TL3-3-AMOUNT         PIC ZZZZ,ZZ9.99.
00350
00351      12  WS-NON-CASH-TOTAL-LINE.
00352          16  WS-NC-TL1-COUNT         PIC ZZZZ9.
00353          16  WS-NC-TL1-LIT           PIC X(32)   VALUE SPACES.
00354
00355      12  WS-DMD-TOTAL-LINE1.
00356          16  FILLER                  PIC X(14)   VALUE
00357              'CONTROL GROUP'.
00358          16  WS-DMD-TL1-CONTROL-GROUP
00359                                      PIC 9(7)-.
00360          16  FILLER                  PIC X       VALUE SPACE.
00361          16  WS-DMD-TL1-COUNT        PIC ZZ,ZZ9.
00362          16  WS-DMD-TL1-RELEASE      PIC X(20)   VALUE
00363              ' RELEASED'.
00364          16  WS-DMD-TL1-AMOUNT       PIC Z,ZZZ,ZZ9.99.
00365
00366      12  WS-DMD-TOTAL-LINE2.
00367          16  WS-DMD-TL2-COUNT1       PIC ZZ,ZZ9.
00368          16  FILLER                  PIC X(6)        VALUE
00369              ' CHECK'.
00370          16  WS-DMD-TL2-PLURAL1      PIC X           VALUE
00371              ' '.
00372          16  FILLER                  PIC X(6)        VALUE
00373              ' FOR '.
00374          16  WS-DMD-TL2-AMOUNT1      PIC Z,ZZZ,ZZ9.99.
00375          16  FILLER                  PIC X(6)        VALUE SPACES.
00376          16  WS-DMD-TL2-COUNT2       PIC ZZ,ZZ9.
00377          16  FILLER                  PIC X           VALUE SPACE.
00378          16  FILLER                  PIC X(3)        VALUE
00379              'DMO'.
00380          16  WS-DMD-TL2-PLURAL2      PIC X           VALUE
00381              ' '.
00382          16  FILLER                  PIC X(7)        VALUE
00383              ' FOR '.
00384          16  WS-DMD-TL2-AMOUNT2      PIC Z,ZZZ,ZZ9.99.
00385
00386      12  WS-PMT-APPROVAL             PIC X.
00387           88 WS-PMT-APPROVAL-USED            VALUE 'Y' 'G'.
00388
00389      12  WS-SAVE-NOTE-RECORD         PIC X(310).
00390
00391      EJECT
00392  01  ERROR-MESSAGES.
00393      12  ER-0004                 PIC X(4)  VALUE '0004'.
00394      12  ER-0008                 PIC X(4)  VALUE '0008'.
00395      12  ER-0029                 PIC X(4)  VALUE '0029'.
00396      12  ER-0070                 PIC X(4)  VALUE '0070'.
00397      12  ER-0078                 PIC X(4)  VALUE '0078'.
00398      12  ER-0330                 PIC X(4)  VALUE '0330'.
00399      12  ER-0331                 PIC X(4)  VALUE '0331'.
00400      12  ER-0395                 PIC X(4)  VALUE '0395'.
00401      12  ER-0412                 PIC X(4)  VALUE '0412'.
00402      12  ER-0413                 PIC X(4)  VALUE '0413'.
00403      12  ER-0431                 PIC X(4)  VALUE '0431'.
00404      12  ER-0432                 PIC X(4)  VALUE '0432'.
00405      12  ER-0568                 PIC X(4)  VALUE '0568'.
00406      12  ER-0849                 PIC X(4)  VALUE '0849'.
00407      12  ER-0910                 PIC X(4)  VALUE '0910'.
00408      12  ER-0913                 PIC X(4)  VALUE '0913'.
00409      12  ER-0919                 PIC X(4)  VALUE '0919'.
00410      12  ER-0921                 PIC X(4)  VALUE '0921'.
00411      12  ER-0946                 PIC X(4)  VALUE '0946'.
00412      12  ER-0947                 PIC X(4)  VALUE '0947'.
00413      12  ER-0948                 PIC X(4)  VALUE '0948'.
00414      12  ER-0949                 PIC X(4)  VALUE '0949'.
00415      12  ER-0950                 PIC X(4)  VALUE '0950'.
00416      12  ER-0951                 PIC X(4)  VALUE '0951'.
00417      12  ER-0954                 PIC X(4)  VALUE '0954'.
00418      12  ER-0974                 PIC X(4)  VALUE '0974'.
00419      12  ER-0975                 PIC X(4)  VALUE '0975'.
00420      12  ER-1882                 PIC X(4)  VALUE '1882'.
00421      12  ER-2370                 PIC X(4)  VALUE '2370'.
00422      12  ER-3048                 PIC X(4)  VALUE '3048'.
00423      12  ER-8051                 PIC X(4)  VALUE '8051'.
00424      12  ER-8052                 PIC X(4)  VALUE '8052'.
00425      12  ER-8053                 PIC X(4)  VALUE '8053'.
00426      12  ER-8054                 PIC X(4)  VALUE '8054'.
00427      12  ER-8055                 PIC X(4)  VALUE '8055'.
00428      12  ER-8056                 PIC X(4)  VALUE '8056'.
00429      12  ER-8057                 PIC X(4)  VALUE '8057'.
00430      12  ER-8058                 PIC X(4)  VALUE '8058'.
00431      12  ER-8059                 PIC X(4)  VALUE '8059'.
00432      12  ER-8060                 PIC X(4)  VALUE '8060'.
00433      12  ER-8061                 PIC X(4)  VALUE '8061'.
00434      12  ER-8062                 PIC X(4)  VALUE '8062'.
00435      12  ER-8063                 PIC X(4)  VALUE '8063'.
00436      12  ER-8064                 PIC X(4)  VALUE '8064'.
00437      12  ER-8065                 PIC X(4)  VALUE '8065'.
00438      12  ER-8066                 PIC X(4)  VALUE '8066'.
00439      12  ER-8135                 PIC X(4)  VALUE '8135'.
00440      12  ER-8137                 PIC X(4)  VALUE '8137'.
00441      12  ER-8152                 PIC X(4)  VALUE '8152'.
00442      12  ER-8153                 PIC X(4)  VALUE '8153'.
00443      12  ER-8154                 PIC X(4)  VALUE '8154'.
00444      12  ER-8155                 PIC X(4)  VALUE '8155'.
00445
00446      EJECT
00447 *                            COPY ELCINTF SUPPRESS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00448
00449      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00450          16  FILLER                  PIC X.
00451          16  PI-CK-CONTROL-NO        PIC S9(8)      COMP.
00452          16  PI-PROC-SW              PIC S9.
00453              88  PI-SCREEN-PROCESSED        VALUE +1.
090309             88  PI-ALL-CHKS-NOT-ALLOWED    VALUE +2.
00454          16  PI-CHECK-AMOUNT         PIC S9(7)      COMP-3.
00455          16  FILLER                  PIC X(103).
00456          16  PI-END-CONTROL-NO       PIC S9(8)      COMP.
00457          16  PI-MONTH-END-SAVE       PIC XX.
00458          16  PI-NON-CASH-REL-CNT     PIC S9(5)      COMP-3.
00459          16  PI-NON-CASH-REL-AMT     PIC S9(9)V99   COMP-3.
00460          16  FILLER                  PIC X(512).
00461
00462      EJECT
00463 *    COPY EL175S.
       01  EL175AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  AOPTIONL PIC S9(0004) COMP.
           05  AOPTIONF PIC  X(0001).
           05  FILLER REDEFINES AOPTIONF.
               10  AOPTIONA PIC  X(0001).
           05  AOPTIONI PIC  X(0001).
      *    -------------------------------
           05  COVG1L PIC S9(0004) COMP.
           05  COVG1F PIC  X(0001).
           05  FILLER REDEFINES COVG1F.
               10  COVG1A PIC  X(0001).
           05  COVG1I PIC  X(0006).
      *    -------------------------------
           05  COVG2L PIC S9(0004) COMP.
           05  COVG2F PIC  X(0001).
           05  FILLER REDEFINES COVG2F.
               10  COVG2A PIC  X(0001).
           05  COVG2I PIC  X(0006).
      *    -------------------------------
           05  ABYL PIC S9(0004) COMP.
           05  ABYF PIC  X(0001).
           05  FILLER REDEFINES ABYF.
               10  ABYA PIC  X(0001).
           05  ABYI PIC  X(0004).
      *    -------------------------------
           05  AAMTL PIC S9(0004) COMP.
           05  AAMTF PIC  X(0001).
           05  FILLER REDEFINES AAMTF.
               10  AAMTA PIC  X(0001).
           05  AAMTI PIC  9(7).
      *    -------------------------------
           05  ACARRL PIC S9(0004) COMP.
           05  ACARRF PIC  X(0001).
           05  FILLER REDEFINES ACARRF.
               10  ACARRA PIC  X(0001).
           05  ACARRI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  GCARRL PIC S9(0004) COMP.
           05  GCARRF PIC  X(0001).
           05  FILLER REDEFINES GCARRF.
               10  GCARRA PIC  X(0001).
           05  GCARRI PIC  X(0001).
      *    -------------------------------
           05  GGROUPL PIC S9(0004) COMP.
           05  GGROUPF PIC  X(0001).
           05  FILLER REDEFINES GGROUPF.
               10  GGROUPA PIC  X(0001).
           05  GGROUPI PIC  X(0006).
      *    -------------------------------
           05  GSTATEL PIC S9(0004) COMP.
           05  GSTATEF PIC  X(0001).
           05  FILLER REDEFINES GSTATEF.
               10  GSTATEA PIC  X(0001).
           05  GSTATEI PIC  X(0002).
      *    -------------------------------
           05  GACCTL PIC S9(0004) COMP.
           05  GACCTF PIC  X(0001).
           05  FILLER REDEFINES GACCTF.
               10  GACCTA PIC  X(0001).
           05  GACCTI PIC  X(0010).
      *    -------------------------------
           05  GBENEL PIC S9(0004) COMP.
           05  GBENEF PIC  X(0001).
           05  FILLER REDEFINES GBENEF.
               10  GBENEA PIC  X(0001).
           05  GBENEI PIC  X(0010).
      *    -------------------------------
           05  ACARRD1L PIC S9(0004) COMP.
           05  ACARRD1F PIC  X(0001).
           05  FILLER REDEFINES ACARRD1F.
               10  ACARRD1A PIC  X(0001).
           05  ACARRD1I PIC  X(0007).
      *    -------------------------------
           05  ACARRD2L PIC S9(0004) COMP.
           05  ACARRD2F PIC  X(0001).
           05  FILLER REDEFINES ACARRD2F.
               10  ACARRD2A PIC  X(0001).
           05  ACARRD2I PIC  X(0007).
      *    -------------------------------
           05  ACARRD3L PIC S9(0004) COMP.
           05  ACARRD3F PIC  X(0001).
           05  FILLER REDEFINES ACARRD3F.
               10  ACARRD3A PIC  X(0001).
           05  ACARRD3I PIC  X(0007).
      *    -------------------------------
           05  ACLNO07L PIC S9(0004) COMP.
           05  ACLNO07F PIC  X(0001).
           05  FILLER REDEFINES ACLNO07F.
               10  ACLNO07A PIC  X(0001).
           05  ACLNO07I PIC  X(0007).
      *    -------------------------------
           05  ACARR07L PIC S9(0004) COMP.
           05  ACARR07F PIC  X(0001).
           05  FILLER REDEFINES ACARR07F.
               10  ACARR07A PIC  X(0001).
           05  ACARR07I PIC  X(0001).
      *    -------------------------------
           05  ACLNO08L PIC S9(0004) COMP.
           05  ACLNO08F PIC  X(0001).
           05  FILLER REDEFINES ACLNO08F.
               10  ACLNO08A PIC  X(0001).
           05  ACLNO08I PIC  X(0007).
      *    -------------------------------
           05  ACARR08L PIC S9(0004) COMP.
           05  ACARR08F PIC  X(0001).
           05  FILLER REDEFINES ACARR08F.
               10  ACARR08A PIC  X(0001).
           05  ACARR08I PIC  X(0001).
      *    -------------------------------
           05  ACLNO09L PIC S9(0004) COMP.
           05  ACLNO09F PIC  X(0001).
           05  FILLER REDEFINES ACLNO09F.
               10  ACLNO09A PIC  X(0001).
           05  ACLNO09I PIC  X(0007).
      *    -------------------------------
           05  ACARR09L PIC S9(0004) COMP.
           05  ACARR09F PIC  X(0001).
           05  FILLER REDEFINES ACARR09F.
               10  ACARR09A PIC  X(0001).
           05  ACARR09I PIC  X(0001).
      *    -------------------------------
           05  ACLNO10L PIC S9(0004) COMP.
           05  ACLNO10F PIC  X(0001).
           05  FILLER REDEFINES ACLNO10F.
               10  ACLNO10A PIC  X(0001).
           05  ACLNO10I PIC  X(0007).
      *    -------------------------------
           05  ACARR10L PIC S9(0004) COMP.
           05  ACARR10F PIC  X(0001).
           05  FILLER REDEFINES ACARR10F.
               10  ACARR10A PIC  X(0001).
           05  ACARR10I PIC  X(0001).
      *    -------------------------------
           05  ACLNO11L PIC S9(0004) COMP.
           05  ACLNO11F PIC  X(0001).
           05  FILLER REDEFINES ACLNO11F.
               10  ACLNO11A PIC  X(0001).
           05  ACLNO11I PIC  X(0007).
      *    -------------------------------
           05  ACARR11L PIC S9(0004) COMP.
           05  ACARR11F PIC  X(0001).
           05  FILLER REDEFINES ACARR11F.
               10  ACARR11A PIC  X(0001).
           05  ACARR11I PIC  X(0001).
      *    -------------------------------
           05  ACLNO12L PIC S9(0004) COMP.
           05  ACLNO12F PIC  X(0001).
           05  FILLER REDEFINES ACLNO12F.
               10  ACLNO12A PIC  X(0001).
           05  ACLNO12I PIC  X(0007).
      *    -------------------------------
           05  ACARR12L PIC S9(0004) COMP.
           05  ACARR12F PIC  X(0001).
           05  FILLER REDEFINES ACARR12F.
               10  ACARR12A PIC  X(0001).
           05  ACARR12I PIC  X(0001).
      *    -------------------------------
           05  ACLNO13L PIC S9(0004) COMP.
           05  ACLNO13F PIC  X(0001).
           05  FILLER REDEFINES ACLNO13F.
               10  ACLNO13A PIC  X(0001).
           05  ACLNO13I PIC  X(0007).
      *    -------------------------------
           05  ACARR13L PIC S9(0004) COMP.
           05  ACARR13F PIC  X(0001).
           05  FILLER REDEFINES ACARR13F.
               10  ACARR13A PIC  X(0001).
           05  ACARR13I PIC  X(0001).
      *    -------------------------------
           05  ACLNO14L PIC S9(0004) COMP.
           05  ACLNO14F PIC  X(0001).
           05  FILLER REDEFINES ACLNO14F.
               10  ACLNO14A PIC  X(0001).
           05  ACLNO14I PIC  X(0007).
      *    -------------------------------
           05  ACARR14L PIC S9(0004) COMP.
           05  ACARR14F PIC  X(0001).
           05  FILLER REDEFINES ACARR14F.
               10  ACARR14A PIC  X(0001).
           05  ACARR14I PIC  X(0001).
      *    -------------------------------
           05  ACLNO15L PIC S9(0004) COMP.
           05  ACLNO15F PIC  X(0001).
           05  FILLER REDEFINES ACLNO15F.
               10  ACLNO15A PIC  X(0001).
           05  ACLNO15I PIC  X(0007).
      *    -------------------------------
           05  ACARR15L PIC S9(0004) COMP.
           05  ACARR15F PIC  X(0001).
           05  FILLER REDEFINES ACARR15F.
               10  ACARR15A PIC  X(0001).
           05  ACARR15I PIC  X(0001).
      *    -------------------------------
           05  ACLNO16L PIC S9(0004) COMP.
           05  ACLNO16F PIC  X(0001).
           05  FILLER REDEFINES ACLNO16F.
               10  ACLNO16A PIC  X(0001).
           05  ACLNO16I PIC  X(0007).
      *    -------------------------------
           05  ACARR16L PIC S9(0004) COMP.
           05  ACARR16F PIC  X(0001).
           05  FILLER REDEFINES ACARR16F.
               10  ACARR16A PIC  X(0001).
           05  ACARR16I PIC  X(0001).
      *    -------------------------------
           05  ACLNO17L PIC S9(0004) COMP.
           05  ACLNO17F PIC  X(0001).
           05  FILLER REDEFINES ACLNO17F.
               10  ACLNO17A PIC  X(0001).
           05  ACLNO17I PIC  X(0007).
      *    -------------------------------
           05  ACARR17L PIC S9(0004) COMP.
           05  ACARR17F PIC  X(0001).
           05  FILLER REDEFINES ACARR17F.
               10  ACARR17A PIC  X(0001).
           05  ACARR17I PIC  X(0001).
      *    -------------------------------
           05  ACLNO18L PIC S9(0004) COMP.
           05  ACLNO18F PIC  X(0001).
           05  FILLER REDEFINES ACLNO18F.
               10  ACLNO18A PIC  X(0001).
           05  ACLNO18I PIC  X(0007).
      *    -------------------------------
           05  ACARR18L PIC S9(0004) COMP.
           05  ACARR18F PIC  X(0001).
           05  FILLER REDEFINES ACARR18F.
               10  ACARR18A PIC  X(0001).
           05  ACARR18I PIC  X(0001).
      *    -------------------------------
           05  ACLNO19L PIC S9(0004) COMP.
           05  ACLNO19F PIC  X(0001).
           05  FILLER REDEFINES ACLNO19F.
               10  ACLNO19A PIC  X(0001).
           05  ACLNO19I PIC  X(0007).
      *    -------------------------------
           05  ACARR19L PIC S9(0004) COMP.
           05  ACARR19F PIC  X(0001).
           05  FILLER REDEFINES ACARR19F.
               10  ACARR19A PIC  X(0001).
           05  ACARR19I PIC  X(0001).
      *    -------------------------------
           05  ACLNO20L PIC S9(0004) COMP.
           05  ACLNO20F PIC  X(0001).
           05  FILLER REDEFINES ACLNO20F.
               10  ACLNO20A PIC  X(0001).
           05  ACLNO20I PIC  X(0007).
      *    -------------------------------
           05  ACARR20L PIC S9(0004) COMP.
           05  ACARR20F PIC  X(0001).
           05  FILLER REDEFINES ACARR20F.
               10  ACARR20A PIC  X(0001).
           05  ACARR20I PIC  X(0001).
      *    -------------------------------
           05  ACLNO21L PIC S9(0004) COMP.
           05  ACLNO21F PIC  X(0001).
           05  FILLER REDEFINES ACLNO21F.
               10  ACLNO21A PIC  X(0001).
           05  ACLNO21I PIC  X(0007).
      *    -------------------------------
           05  ACARR21L PIC S9(0004) COMP.
           05  ACARR21F PIC  X(0001).
           05  FILLER REDEFINES ACARR21F.
               10  ACARR21A PIC  X(0001).
           05  ACARR21I PIC  X(0001).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  AEMSG2L PIC S9(0004) COMP.
           05  AEMSG2F PIC  X(0001).
           05  FILLER REDEFINES AEMSG2F.
               10  AEMSG2A PIC  X(0001).
           05  AEMSG2I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  9(2).
      *    -------------------------------
           05  AEMSG3L PIC S9(0004) COMP.
           05  AEMSG3F PIC  X(0001).
           05  FILLER REDEFINES AEMSG3F.
               10  AEMSG3A PIC  X(0001).
           05  AEMSG3I PIC  X(0068).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0014).
      *    -------------------------------
           05  APF1L PIC S9(0004) COMP.
           05  APF1F PIC  X(0001).
           05  FILLER REDEFINES APF1F.
               10  APF1A PIC  X(0001).
           05  APF1I PIC  X(0028).
       01  EL175AO REDEFINES EL175AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPTIONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVG1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVG2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAMTO PIC  ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRD1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRD2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRD3O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO19O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO20O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNO21O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR21O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG3O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APF1O PIC  X(0028).
      *    -------------------------------
00464
00465  01  FILLER  REDEFINES  EL175AI.
00466      12  FILLER                        PIC X(170).
00467      12  FILLER      OCCURS 5  INDEXED BY EL175A-INDEX1.
00468          16  FILLER  OCCURS 3  INDEXED BY EL175A-INDEX2.
00469              20  EL175A-CLAIM-LENGTH   PIC S9(4) COMP.
00470              20  EL175A-CLAIM-ATTRB    PIC X.
00471              20  EL175A-CLAIM          PIC X(7).
00472              20  EL175A-CARRIER-LENGTH PIC S9(4) COMP.
00473              20  EL175A-CARRIER-ATTRB  PIC X.
00474              20  EL175A-CARRIER        PIC X.
00475
00476      EJECT
00477 *                                COPY ELCDCTB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDCTB.                            *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *   DESCRIPTION = DISTRIBUTION CONTROL TABLE MAINTENANCE PROGRAM *
00007 *       COMMUNICATIONS AREA                                      *
00008 *                                                                *
00009 ******************************************************************
00010  01  DCT-COMMUNICATION-AREA.
00011      12  DCT-BILLING-BANK-ID      PIC  X(05).
00012      12  DCT-LOGIC-BENEFICIARY-ID PIC  X(10).
00013      12  DCT-CREDIT-CARD-NUMBER   PIC  X(16).
00014      12  DCT-PRODUCT-CODE         PIC  X(02).
00015      12  DCT-COLUMN-ID-REQUESTED  PIC  X(02).
00016      12  DCT-RETURN-CODE          PIC  X(02).
00017      12  DCT-MAIL-CODE            PIC  X(05).
00018      12  DCT-DISTRIBUTION-CODE    PIC  X(04).
00019      12  DCT-MSA-ACCT-NO          PIC  X(07).
00478      EJECT
00479 *                                COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00480      EJECT
00481 *                                COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00482      EJECT
00483 *                                COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00484      EJECT
00485 *                                COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00486      EJECT
00487 *                                COPY ELCDMO.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMO.                             *
00004 *                            VMOD=2.004                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DLO025 (DMO FILE MAINTENANCE PGRM)        *
00007 *        COMMUNICATION AREA                                      *
00008 *   FILE TYPE = NA                                               *
00009 *   RECORD SIZE = 110    RECFORM = FIXED                         *
00010 *                                                                *
00011 ******************************************************************
00012  01  DMO-COMMUNICATION-AREA.
00013      12  DM-RECORD-TYPE                  PIC  X(02).
00014              88  DM-ISSUE-TRAN                VALUE 'CC'.
00015              88  DM-CLAIM-STATUS-CHANGE       VALUE 'CS'.
00016              88  DM-CLAIM-PAYMENT             VALUE 'DR'.
00017      12  DM-DIST-CODE                    PIC  X(04).
00018      12  DM-MAIL-CODE                    PIC  X(05).
00019      12  DM-CREDIT-CARD-NUMBER           PIC  X(16).
00020      12  DM-INSURED-NAME                 PIC  X(30).
00021      12  DM-CLAIM-NO                     PIC  X(07).
00022      12  DM-CLAIM-TYPE                   PIC  X.
00023
00024      12  DM-STATUS-DATA-AREA.
00025          16  DM-CLAIM-STATUS             PIC  X.
00026              88  DM-OPEN-NO-PAYMENTS              VALUE '1'.
00027              88  DM-OPEN-WITH-PAYMENTS            VALUE '2'.
00028              88  DM-CLOSED                        VALUE '3'.
00029              88  DM-CLOSE-SETTLE-FINAL            VALUE '4'.
00030              88  DM-DEFAULT                       VALUE '9'.
00031          16  DM-STATUS-DATE              PIC  X(08).
00032 ******YYYYMMDD
00033          16  DM-STAT-CHANGE-TYPE         PIC  X.
00034              88  DM-MANUAL-CLOSE                  VALUE 'C'.
00035              88  DM-CLAIM-DENIED                  VALUE 'D'.
00036              88  DM-FINAL-PAYMENT                 VALUE 'F'.
00037              88  DM-INITIAL-PAYMENT               VALUE 'I'.
00038              88  DM-AUTO-CLOSE                    VALUE 'Q'.
00039              88  DM-RE-OPENED                     VALUE 'R'.
00040              88  DM-NEW-CLAIM-SETUP               VALUE 'S'.
00041              88  DM-VOIDED-PAYMENT                VALUE 'V'.
00042              88  DM-CLAIM-DELETED                 VALUE 'X'.
00043          16  DM-STAT-CARRIER             PIC X.
00044
00045      12  DM-DRAFT-DATA-AREA.
00046          16  DM-PAYMENT-TYPE             PIC  X.
00047              88  DM-VALID-CLAIM-TYPES VALUES 'L' 'D' 'U' 'A'.
00048          16  DM-PAYMENT-AMT              PIC  9(05)V9(02).
00049          16  DM-PAYMENT-DATE             PIC  X(08).
00050 ******YYYYMMDD
00051          16  DM-CERT-NO                  PIC  X(11).
00052          16  DM-TRLR-SEQ-NO              PIC  9(04).
00053          16  DM-CARRIER                  PIC  X.
00054
00055      12  DM-RETURN-CODE                  PIC  XX.
CIDMOD     EJECT
CIDMOD*                                COPY ELCDAR.
00001 ******************************************************************
00002 *                                                                *
00003 *   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
00004 *   FILE TYPE = VSAM,KSDS                                        *
00005 *   RECORD SIZE = 25   RECFORM = FIXED                           *
00006 *   BASE CLUSTER = DLYACTV                                       *
00007 *   LOG = YES                                                    *
00008 *   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
00009 *               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
00010 *               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
00011 *               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
00012 *               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
00013 *               BY PROGRAM "LGINFCE".                            *
00014 *                                                                *
00015 ******************************************************************
00016  01  DAILY-ACTIVITY-RECORD.
00017      05  DA-KEY.
00018          10  DA-COMP-CD          PIC X.
00019          10  DA-CARRIER          PIC X.
00020          10  DA-CLAIM-NO         PIC X(7).
00021          10  DA-CERT-NO.
00022              15  DA-CERT-PRIME   PIC X(10).
00023              15  DA-CERT-SFX     PIC X.
00024      05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
00025      05  DA-RECORD-TYPE          PIC X.
00026      05  FILLER                  PIC X(2).
00027 ******************************************************************
00488      EJECT
00489 *                                COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00490      EJECT
00491  01  FILLER REDEFINES DFHAID.
00492      12  FILLER                      PIC X(8).
00493      12  PF-VALUES                   PIC X
00494          OCCURS 24 TIMES.
00495
00496      EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00498
00499  01  DFHCOMMAREA                     PIC X(1024).
00500
00501      EJECT
00502 *                                COPY ELCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCHKQ.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID         VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-CONTROL-BY-PAYEE.
DJNA           16  CQ-CONTROL-BY-NUMBER.
DJNA               20  CQ-COMPANY-CD-A1     PIC X.
DJNA               20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
00030          16  CQ-PAYEE-CARRIER        PIC X.
00031          16  CQ-PAYEE-GROUPING       PIC X(6).
00032          16  CQ-PAYEE-STATE          PIC XX.
00033          16  CQ-PAYEE-BENE-ACCT      PIC X(10).
00034          16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00035
00036      12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
00037          16  CQ-DMD-COMPANY-CD-A2    PIC X.
00038          16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
00039          16  CQ-DMD-BENE-CODE-A2     PIC X(10).
00040          16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
00041          16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
00042          16  FILLER                  PIC X(3).
00043
00044      12  CQ-ENTRY-TYPE               PIC X.
00045              88  CHECK-ON-QUE           VALUE 'Q'.
00046              88  ALIGNMENT-CHECK        VALUE 'A'.
00047              88  SPOILED-CHECK          VALUE 'S'.
00048              88  PAYMENT-ABORTED        VALUE 'X'.
00049
00050      12  CQ-CLAIM-MAST-CNTL.
00051          16  CQ-CARRIER              PIC X.
00052          16  CQ-CLAIM-NO             PIC X(7).
00053          16  CQ-CERT-NO.
00054              20  CQ-CERT-PRIME       PIC X(10).
00055              20  CQ-CERT-SFX         PIC X.
00056          16  CQ-CLAIM-TYPE           PIC X.
00057              88  CQ-LIFE-CLAIM          VALUE 'L'.
00058              88  CQ-AH-CLAIM            VALUE 'A'.
00059          16  CQ-CLAIM-SUB-TYPE       PIC X.
00060              88  CQ-FIXED-COVERAGE      VALUE '1'.
00061              88  CQ-O-B-COVERAGE        VALUE '2'.
00062              88  CQ-OPEN-END-COVERAGE   VALUE '3'.
00063
00064      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
00065      12  CQ-CHECK-NUMBER             PIC X(7).
00066      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00067      12  CQ-PAYMENT-TYPE             PIC X.
00068              88  CQ-PARTIAL-PAYMENT        VALUE '1'.
00069              88  CQ-FINAL-PAYMENT          VALUE '2'.
00070              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
00071              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
00072              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
00073              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
00074              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
00075              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
00076      12  CQ-VOID-INDICATOR           PIC X.
00077              88  CHECK-IS-STOPPED          VALUE 'S'.
00078              88  CHECK-IS-VOID             VALUE 'V'.
00079      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00080      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00081      12  CQ-CHECK-BY-USER            PIC X(4).
00082      12  CQ-PRE-NUMBERING-SW         PIC X.
00083        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00084        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00085
00086      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00087      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00088      12  CQ-LEDGER-FLAG              PIC X(01).
00089      12  CQ-VOID-AFTER-LEDGER        PIC X(01).
00090      12  CQ-LAST-UPDATED-DT          PIC XX.
00091      12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
00092      12  CQ-APPLIED-TO-RCON-DT       PIC XX.
00093
00094      12  FILLER                      PIC X(04).
00095
00096 ******************************************************************
00503      EJECT
00504 *                                COPY ELCACTQ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCACTQ.                            *
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 60     RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
00017 ******************************************************************
00018
00019  01  ACTIVITY-QUE.
00020      12  AQ-RECORD-ID                PIC XX.
00021          88  VALID-AQ-ID                VALUE 'AQ'.
00022
00023      12  AQ-CONTROL-PRIMARY.
00024          16  AQ-COMPANY-CD           PIC X.
00025          16  AQ-CARRIER              PIC X.
00026          16  AQ-CLAIM-NO             PIC X(7).
00027          16  AQ-CERT-NO.
00028              20  AQ-CERT-PRIME       PIC X(10).
00029              20  AQ-CERT-SFX         PIC X.
00030
00031      12  AQ-PENDING-ACTIVITY-FLAGS.
00032          88  NO-PENDING-ACTIVITY        VALUE SPACES.
00033          16  AQ-PENDING-PAYMENT-FLAG PIC X.
00034              88  PENDING-PAYMENTS       VALUE '1'.
00035          16  AQ-PENDING-STATUS-FLAG  PIC X.
00036              88  PENDING-FULL-PRINT     VALUE '1'.
00037              88  PENDING-PART-PRINT     VALUE '2'.
00038          16  AQ-PENDING-LETTER-FLAG  PIC X.
00039              88  PENDING-LETTERS        VALUE '1'.
00040          16  AQ-PENDING-CLAIM-RESTORE PIC X.
00041              88  PENDING-RESTORE        VALUE 'C'.
00042              88  PENDING-RESTORE-LETTER VALUE 'L'.
00043
00044      12  FILLER                      PIC X(20).
00045
00046      12  AQ-RESEND-DATE              PIC XX.
00047      12  AQ-FOLLOWUP-DATE            PIC XX.
00048      12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
00049      12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
00050      12  AQ-AUTO-LETTER              PIC X(4).
00051      12  FILLER                      PIC XX.
00052      12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
00053 *****************************************************************
00505      EJECT
00506 *                                COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00507      EJECT
00508 *                                COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
061511* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
021213* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
102413* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
022614* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
040814* 040814    2014030500002  AJRA  ADD ICD CODES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
061511             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
021213             88  AT-VFY-CAUSAL-STATE          VALUE +94.
                   88  AT-ERROR-MSGS-TRL            VALUE +95.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
052614             88  PAID-FOR-FAM                   VALUE 'F'.
100518             88  PAID-FOR-OTH                   VALUE 'O'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
013017         16  AT-ACH-PAYMENT              PIC X.
013017*        16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
020413         16  FILLER REDEFINES AT-EOB-CODE3.
020413             20  AT-PRINT-CLM-FORM       PIC X.
020413             20  AT-PRINT-SURVEY         PIC X.
102413             20  AT-SPECIAL-RELEASE      PIC X.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
062217             20  AT-AUTH-RCVD            PIC X(1).
062217             20  FILLER                  PIC X(18).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
061511*         16  FILLER                      PIC X(23).
061511         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
061511         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
061511         16  FILLER                      PIC X(13).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
061013         16  FILLER REDEFINES AT-INFO-LINE-1.
061013             20  AT-NOTE-ERROR-NO OCCURS 15
061013                                         PIC X(4).
00324          16  AT-INFO-LINE-2              PIC X(60).
040814         16  FILLER REDEFINES AT-INFO-LINE-2.
040814             20  AT-ICD-CODE-1           PIC X(8).
040814             20  AT-ICD-CODE-2           PIC X(8).
040814             20  FILLER                  PIC X(44).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
061013             88  AT-ERRORS-NOTE          VALUE 'E'.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
022614             88  AT-CERT-CANCELLED       VALUE 'T'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
040814         16  AT-OLD-ICD-CODE-1           PIC X(8).
040814         16  AT-OLD-ICD-CODE-2           PIC X(8).
040814         16  FILLER                      PIC X(9).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00509      EJECT
00510 *                                COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
00511      EJECT
00512 *                                COPY ELCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCNOTE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = EXPLANATION OF BENEFITS NOTES             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 310    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELNOTE             RKP=2,LEN=23          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CLAIM-EOB-NOTES.
00019      12  EN-RECORD-ID                    PIC  XX.
00020          88  VALID-EN-ID                      VALUE 'EN'.
00021      12  EN-CONTROL-PRIMARY.
00022          16  EN-COMPANY-CD               PIC  X.
00023          16  EN-CARRIER                  PIC  X.
00024          16  EN-CLAIM-NO                 PIC  X(7).
00025          16  EN-CERT-NO.
00026              20  EN-CERT-PRIME           PIC  X(10).
00027              20  EN-CERT-SFX             PIC  X.
00028          16  EN-PAYMENT-SEQ-NO           PIC  S9(4)  COMP.
00029          16  EN-RECORD-TYPE              PIC  X.
00030 ***********THE CERTIFICATE NUMBER AND PAYMENT SEQ NUMBER
00031 ***********WILL BE LOW VALUES ON TYPE 1 RECORDS. THE FULL
00032 ***********KEY WILL BE USED WHEN BUILDING THE TYPE 2 RECORDS.
00033
00034      12  EN-EOB-NOTE1                    PIC  X(60).
00035      12  EN-EOB-NOTE2                    PIC  X(60).
00036      12  EN-EOB-NOTE3                    PIC  X(60).
00037      12  EN-EOB-NOTE4                    PIC  X(60).
00038      12  FILLER                          PIC  X(35).
00039
00040      12  EN-LAST-MAINT-DT                PIC  XX.
00041      12  EN-LAST-MAINT-BY                PIC  X(4).
00042      12  EN-LAST-MAINT-HHMMSS            PIC  S9(6) COMP-3.
00513      EJECT
00514 *                                COPY ELCNOTX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCNOTX.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = EXPLANATION OF BENEFITS EXTRACTS          *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 410    RECFORM = FIXED                         *
00010 *                                                                *
00011 *   BASE CLUSTER NAME = ELNOTX             RKP=2,LEN=22          *
00012 *       ALTERNATE INDEX = NONE                                   *
00013 *                                                                *
00014 *   LOG = YES                                                    *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017  01  CLAIM-EOB-EXTRACT.
00018      12  EB-RECORD-ID                    PIC  XX.
00019          88  DM-ISSUE-TRAN                    VALUE 'EB'.
00020      12  EB-CONTROL-PRIMARY.
00021          16  EB-COMPANY-CD               PIC  X.
00022          16  EB-CARRIER                  PIC  X.
00023          16  EB-CLAIM-NO                 PIC  X(7).
00024          16  EB-CERT-NUMBER.
00025              20  EB-CERT-NO              PIC  X(10).
00026              20  EB-CERT-SFX             PIC  X.
00027          16  EB-PMT-TRLR-SEQ-NO          PIC  S9(4)  COMP.
00028      12  EB-INCURRED-DT                  PIC  X(10).
00029      12  EB-PAID-FROM                    PIC  X(10).
00030      12  EB-PAID-THRU                    PIC  X(10).
00031      12  EB-PAYEE-NAME                   PIC  X(30).
00032      12  EB-PAID-AMOUNT                  PIC  S9(7)V99.
00033      12  EB-PAYMENT-TYPE                 PIC  X.
00034      12  EB-PAYMENT-ADDR-SEQ             PIC  S9(4)  COMP.
00035      12  EB-EOB-CODE1                    PIC  XXX.
00036      12  EB-EOB-CODE2                    PIC  XXX.
00037      12  EB-EOB-CODE3                    PIC  XXX.
00038      12  EB-EOB-CODE4                    PIC  XXX.
00039      12  EB-EOB-CODE5                    PIC  XXX.
00040      12  EB-EOB-NOTE1                    PIC  X(60).
00041      12  EB-EOB-NOTE2                    PIC  X(60).
00042      12  EB-EOB-NOTE3                    PIC  X(60).
00043      12  EB-EOB-NOTE4                    PIC  X(60).
00044      12  FILLER                          PIC  X(53).
00045
00046      12  EB-LAST-MAINT-DT                PIC  XX.
00047      12  EB-LAST-UPDATED-BY              PIC  X(4).
00515      EJECT
00516 *                                COPY ERCDMDNT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCDMDNT                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = DMD CERTIFICATE NOTES                *
00008 *                                                                *
00009 *        THIS COPYBOOK IS A REDEFINES OF ERCNOTE -               *
00010 *                                                                *
00011 *        FILE TYPE= VSAM,KSDS                                    *
00012 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00013 *                                                                *
00014 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
00015 *                                                                *
00016 *        LOG = YES                                               *
00017 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00018 *                                                                *
00019 ******************************************************************
00020
00021  01  CERTIFICATE-NOTE.
00022      12  CN-RECORD-ID                     PIC  XX.
00023          88  VALID-CN-ID                      VALUE 'CN'.
00024
00025      12  CN-CONTROL-PRIMARY.
00026          16  CN-COMPANY-CD                PIC X.
00027          16  CN-CERT-KEY.
00028              20  CN-CARRIER               PIC X.
00029              20  CN-GROUPING.
00030                  24  CN-GROUPING-PREFIX   PIC XXX.
00031                  24  CN-GROUPING-PRIME    PIC XXX.
00032              20  CN-STATE                 PIC XX.
00033              20  CN-ACCOUNT.
00034                  24  CN-ACCOUNT-PREFIX    PIC X(4).
00035                  24  CN-ACCOUNT-PRIME     PIC X(6).
00036              20  CN-CERT-EFF-DT           PIC XX.
00037              20  CN-CERT-NO.
00038                  24  CN-CERT-PRIME        PIC X(10).
00039                  24  CN-CERT-SFX          PIC X.
00040
00041      12  CN-BILLING-START-LINE-NO         PIC 99.
00042      12  CN-BILLING-END-LINE-NO           PIC 99.
00043
00044      12  CN-LINES.
00045          16  CN-LINE                      PIC X(77)  OCCURS 10.
00046
00047      12  CN-CSI-NOTES REDEFINES CN-LINES.
00048          16  CN-CSI-TEXT-NOTES            PIC X(77)  OCCURS 6.
00049          16  CN-CSI-GENERAL-DATA-AREA.
00050              20  CN-CSI-GENERAL-DATA      PIC X(77)  OCCURS 2.
00051
00052          16  CN-CSI-GENERAL-DATA-R REDEFINES
00053              CN-CSI-GENERAL-DATA-AREA.
00054              20  CN-CSI-GEN-NOC-KEY           PIC X(11).
00055              20  CN-CSI-GEN-PRI-INSD-1ST-NAME PIC X(15).
00056              20  CN-CSI-GEN-SEC-INSD-1ST-NAME PIC X(15).
00057              20  CN-CSI-GEN-INSD-WORK-PHONE   PIC X(10).
00058              20  CN-CSI-GEN-INFRM-1ST-NAME    PIC X(15).
00059              20  CN-CSI-GEN-INFRM-LAST-NAME   PIC X(20).
00060              20  CN-CSI-GEN-INFRM-MI          PIC X.
00061              20  CN-CSI-GEN-INFRM-PHONE       PIC X(10).
00062              20  CN-CSI-GEN-INFRM-REL         PIC X(15).
00063              20  FILLER                       PIC XX.
00064              20  CN-CSI-GEN-DATA-SOURCE       PIC XX.
00065              20  FILLER                       PIC X(38).
00066
00067          16  CN-CSI-PRODUCT-DATA-AREA.
00068              20  CN-CSI-PRODUCT-DATA      PIC X(77)  OCCURS 2.
00069
00070          16  CN-CSI-CREDIT-CARD-DATA REDEFINES
00071              CN-CSI-PRODUCT-DATA-AREA.
00072              20  CN-CSI-CC-BILL-BANK-ID   PIC X(6).
00073              20  CN-CSI-CC-CANCEL-CD      PIC XX.
00074              20  CN-CSI-CC-CANCEL-DT      PIC X(8).
00075              20  CN-CSI-CC-CARD-TYPE      PIC XX.
00076              20  CN-CSI-CC-CHANGE-AGE     PIC 999.
00077              20  CN-CSI-CC-DIAGNOSIS-CD   PIC X(6).
00078              20  FILLER                   PIC XX.
00079              20  CN-CSI-CC-INSURED-BAL    PIC S9(5)V99  COMP-3.
00080              20  CN-CSI-CC-INTEREST-AMT   PIC S9(5)V99  COMP-3.
00081              20  CN-CSI-CC-INTEREST-PAID  PIC X.
00082              20  CN-CSI-CC-ISSUE-ST       PIC XX.
00083              20  CN-CSI-CC-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
00084              20  CN-CSI-CC-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
00085              20  CN-CSI-CC-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
00086              20  CN-CSI-CC-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
00087              20  CN-CSI-CC-OLD-ACCT-NO    PIC X(20).
00088              20  CN-CSI-CC-POLICY-TYPE    PIC XXX.
00089              20  CN-CSI-CC-PREMIUM-AMT    PIC S999V99   COMP-3.
00090              20  CN-CSI-CC-PREMIUM-RT     PIC S999V999  COMP-3.
00091              20  CN-CSI-CC-PREV-CLAIM-NO  PIC X(7).
00092              20  CN-CSI-CC-SIGNED-DT      PIC X(8).
00093              20  CN-CSI-CC-SPECIAL-TERM   PIC S999      COMP-3.
00094              20  CN-CSI-CC-STMNT-DT       PIC X(8).
00095              20  CN-CSI-CC-TERM-AGE       PIC 999.
00096              20  CN-CSI-CC-TOL-BALANCE    PIC S9(5)V99  COMP-3.
00097              20  CN-CSI-CC-WAIV-PREM-FLAG PIC X.
00098              20  CN-CSI-CC-ISSUE-DT       PIC X(8).
00099              20  CN-CSI-CC-BEN-CALC-SW    PIC X.
00100              20  CN-CSI-CC-TERM-ROUND-SW  PIC X.
00101              20  FILLER                   PIC X(25).
00102
00103          16  CN-CSI-FAMILY-LEAVE-DATA REDEFINES
00104              CN-CSI-CREDIT-CARD-DATA.
00105              20  CN-CSI-FL-BILL-BANK-ID   PIC X(6).
00106              20  CN-CSI-FL-CANCEL-CD      PIC XX.
00107              20  CN-CSI-FL-CANCEL-DT      PIC X(8).
00108              20  CN-CSI-FL-CARD-TYPE      PIC XX.
00109              20  CN-CSI-FL-CHANGE-AGE     PIC 999.
00110              20  CN-CSI-FL-DIAGNOSIS-CD   PIC X(6).
00111              20  FILLER                   PIC XX.
00112              20  CN-CSI-FL-INSURED-BAL    PIC S9(5)V99  COMP-3.
00113              20  CN-CSI-FL-INTEREST-AMT   PIC S9(5)V99  COMP-3.
00114              20  CN-CSI-FL-INTEREST-PAID  PIC X.
00115              20  CN-CSI-FL-ISSUE-ST       PIC XX.
00116              20  CN-CSI-FL-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
00117              20  CN-CSI-FL-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
00118              20  CN-CSI-FL-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
00119              20  CN-CSI-FL-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
00120              20  CN-CSI-FL-OLD-ACCT-NO    PIC X(20).
00121              20  CN-CSI-FL-POLICY-TYPE    PIC XXX.
00122              20  CN-CSI-FL-PREMIUM-AMT    PIC S999V99   COMP-3.
00123              20  CN-CSI-FL-PREMIUM-RT     PIC S999V999  COMP-3.
00124              20  CN-CSI-FL-PREV-CLAIM-NO  PIC X(7).
00125              20  CN-CSI-FL-SIGNED-DT      PIC X(8).
00126              20  CN-CSI-FL-SPECIAL-TERM   PIC S999      COMP-3.
00127              20  CN-CSI-FL-STMNT-DT       PIC X(8).
00128              20  CN-CSI-FL-TERM-AGE       PIC 999.
00129              20  CN-CSI-FL-TOL-BALANCE    PIC S9(5)V99  COMP-3.
00130              20  CN-CSI-FL-WAIV-PREM-FLAG PIC X.
00131              20  CN-CSI-FL-ISSUE-DT       PIC X(8).
00132              20  CN-CSI-FL-BEN-CALC-SW    PIC X.
00133              20  CN-CSI-FL-TERM-ROUND-SW  PIC X.
00134              20  CN-CSI-FL-LAST-DAY-WRKED PIC X(8).
00135              20  FILLER                   PIC X(17).
00136
00137          16  CN-CSI-SENIOR-LIFE-DATA REDEFINES
00138              CN-CSI-FAMILY-LEAVE-DATA.
00139              20  CN-CSI-SL-BENE-DOB       PIC X(8).
00140              20  CN-CSI-SL-BENE-NAME      PIC X(27).
00141              20  CN-CSI-SL-BENE-REL       PIC X(8).
00142              20  CN-CSI-SL-BENE-SSN       PIC S9(9)     COMP-3.
00143              20  CN-CSI-SL-BILL-BANK-ID   PIC X(6).
00144              20  CN-CSI-SL-CANCEL-DT      PIC X(8).
00145              20  CN-CSI-SL-DIAGNOSIS-CD   PIC X(6).
00146              20  CN-CSI-SL-INT-CHECK-DT   PIC X(8).
00147              20  CN-CSI-SL-INT-CHECK-NO   PIC S9(7)     COMP-3.
00148              20  CN-CSI-SL-INT-ON-PROCEEDS
00149                                           PIC S9(5)V99  COMP-3.
00150              20  CN-CSI-SL-ISSUE-DT       PIC X(8).
00151              20  CN-CSI-SL-ISSUE-ST       PIC XX.
00152              20  CN-CSI-SL-LIFE-PROCEEDS  PIC S9(5)V99  COMP-3.
00153              20  CN-CSI-SL-LOAN-INT-DUE   PIC S9(5)V99  COMP-3.
00154              20  CN-CSI-SL-POLICY-BENEFITS
00155                                           PIC S9(5)V99  COMP-3.
00156              20  CN-CSI-SL-POLICY-TYPE    PIC XXX.
00157              20  CN-CSI-SL-PREM-AMT       PIC S9(5)V99  COMP-3.
00158              20  CN-CSI-SL-PREM-CHECK-DT  PIC X(8).
00159              20  CN-CSI-SL-PREM-CHECK-NO  PIC S9(7)     COMP-3.
00160              20  CN-CSI-SL-PREM-DUE       PIC S9(5)V99  COMP-3.
00161              20  CN-CSI-SL-PREM-MODE      PIC 99.
00162              20  CN-CSI-SL-PREM-REFUND    PIC S9(5)V99  COMP-3.
00163              20  CN-CSI-SL-PREM-SUSP-DT   PIC X(8).
00164              20  CN-CSI-SL-SIGNED-DT      PIC X(8).
00165              20  CN-CSI-SL-STATE-NOT      PIC X.
00166              20  FILLER                   PIC XX.
00167
00168          16  CN-CSI-PURCH-PROP-DATA REDEFINES
00169              CN-CSI-SENIOR-LIFE-DATA.
00170              20  CN-CSI-PP-CARD-TYPE      PIC XX.
00171              20  CN-CSI-PP-CHANGE-AGE     PIC 999.
00172              20  CN-CSI-PP-BEN-PAID-TO-DATE
00173                                           PIC S9(5)V99  COMP-3.
00174              20  CN-CSI-PP-BILL-BANK-ID   PIC X(6).
00175              20  CN-CSI-PP-CANCEL-CD      PIC XX.
00176              20  CN-CSI-PP-CANCEL-DT      PIC X(8).
00177              20  CN-CSI-PP-DIAGNOSIS-CD   PIC X(6).
00178              20  CN-CSI-PP-ISSUE-DT       PIC X(8).
00179              20  CN-CSI-PP-ISSUE-ST       PIC XX.
00180              20  CN-CSI-PP-MANUFACTURER   PIC X(17).
00181              20  CN-CSI-PP-MODEL-NO       PIC X(8).
00182              20  CN-CSI-PP-OLD-ACCT-NO    PIC X(20).
00183              20  CN-CSI-PP-POLICY-TYPE    PIC XXX.
00184              20  CN-CSI-PP-PREMIUM-RT     PIC S999V999  COMP-3.
00185              20  CN-CSI-PP-PREV-CLAIM-NO  PIC X(7).
00186              20  CN-CSI-PP-PURCHASE-DT    PIC X(8).
00187              20  CN-CSI-PP-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
00188              20  CN-CSI-PP-REPAIR         PIC X.
00189              20  CN-CSI-PP-REPLACE        PIC X.
00190              20  CN-CSI-PP-SERIAL-NO      PIC X(16).
00191              20  CN-CSI-PP-SIGNED-DT      PIC X(8).
00192              20  CN-CSI-PP-STMNT-DT       PIC X(8).
00193              20  CN-CSI-PP-TERM-AGE       PIC 999.
00194              20  FILLER                   PIC X(5).
00195
00196          16  CN-CSI-EXT-WARR-DATA REDEFINES
00197              CN-CSI-PURCH-PROP-DATA.
00198              20  CN-CSI-EW-CARD-TYPE      PIC XX.
00199              20  CN-CSI-EW-CHANGE-AGE     PIC 999.
00200              20  CN-CSI-EW-BILL-BANK-ID   PIC X(6).
00201              20  CN-CSI-EW-CANCEL-CD      PIC XX.
00202              20  CN-CSI-EW-CANCEL-DT      PIC X(8).
00203              20  CN-CSI-EW-DIAGNOSIS-CD   PIC X(6).
00204              20  CN-CSI-EW-ISSUE-DT       PIC X(8).
00205              20  CN-CSI-EW-ISSUE-ST       PIC XX.
00206              20  CN-CSI-EW-MANUFACTURER   PIC X(17).
00207              20  CN-CSI-EW-MODEL-NO       PIC X(8).
00208              20  CN-CSI-EW-OLD-ACCT-NO    PIC X(20).
00209              20  CN-CSI-EW-POLICY-TYPE    PIC XXX.
00210              20  CN-CSI-EW-PREMIUM-RT     PIC S999V999  COMP-3.
00211              20  CN-CSI-EW-PREV-CLAIM-NO  PIC X(7).
00212              20  CN-CSI-EW-PURCHASE-DT    PIC X(8).
00213              20  CN-CSI-EW-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
00214              20  CN-CSI-EW-REPAIR-COST    PIC S9(5)V99  COMP-3.
00215              20  CN-CSI-EW-REPLACE        PIC X.
00216              20  CN-CSI-EW-SERIAL-NO      PIC X(16).
00217              20  CN-CSI-EW-SIGNED-DT      PIC X(8).
00218              20  CN-CSI-EW-STMNT-DT       PIC X(8).
00219              20  CN-CSI-EW-TERM-AGE       PIC 999.
00220              20  CN-CSI-EW-WARRANTY-NO    PIC 99.
00221              20  FILLER                   PIC X(4).
00222
00223      12  CN-LAST-MAINT-DT                 PIC XX.
00224      12  CN-LAST-MAINT-HHMMSS             PIC S9(7)     COMP-3.
00225      12  CN-LAST-MAINT-USER               PIC X(4).
00226      12  FILLER                           PIC X(6).
00227
00228 ******************************************************************
00517      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                ACTIVITY-QUE CONTROL-FILE
                                ACTIVITY-TRAILERS CLAIM-MASTER
                                CLAIM-EOB-NOTES CLAIM-EOB-EXTRACT
                                CERTIFICATE-NOTE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL175' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00519
00520      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00521      MOVE '5'                   TO DC-OPTION-CODE.
00522      PERFORM 8500-DATE-CONVERSION.
00523      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00524      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00525
00526      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00527
00528 *    NOTE *******************************************************
00529 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00530 *         *  FROM ANOTHER MODULE.                               *
00531 *         *******************************************************.
00532
00533      IF EIBCALEN NOT GREATER THAN ZERO
00534          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00535          GO TO 8300-SEND-TEXT.
00536
00537      
      * EXEC CICS HANDLE CONDITION
00538 *        PGMIDERR   (9600-PGMIDERR)
00539 *        NOTFND     (0140-MAIN-LOGIC)
00540 *        ENDFILE    (0140-MAIN-LOGIC)
00541 *        ENQBUSY    (0910-ENQ-BUSY)
00542 *        TERMIDERR  (7010-TERMID-ERROR)
00543 *        TRANSIDERR (7020-TRANS-ERROR)
00544 *        ERROR      (9990-ERROR)
00545 *    END-EXEC.
      *    MOVE '"$LI'')[\.             ! " #00004798' TO DFHEIV0
           MOVE X'22244C4927295B5C2E202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00546
00547      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00548                                      EMI-SWITCH2.
00549
00550      EJECT
00551  0010-MAIN-LOGIC.
00552      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00553          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00554              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00555              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00556              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00557              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00558              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00559              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00560              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00561              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00562            ELSE
00563              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00564              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00565              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00566              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00567              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00568              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00569              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00570              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00571        ELSE
00572          GO TO 0040-PROCESS-INPUT.
00573
00574  0015-INITIALIZE.
00575 *    NOTE *******************************************************
00576 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00577 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00578 *         *******************************************************.
00579
00580      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00581      MOVE ZERO                   TO  PI-PROC-SW.
00582      MOVE LOW-VALUES             TO  PI-MONTH-END-SAVE.
00583      MOVE +0                     TO  PI-CK-CONTROL-NO
00584                                      PI-END-CONTROL-NO
00585                                      PI-NON-CASH-REL-CNT
00586                                      PI-NON-CASH-REL-AMT.
090309     MOVE 'N'                    TO  WS-ALL-CHECK-RELEASE.
00587
00588      MOVE LOW-VALUES             TO  EL175AI.
00589      MOVE -1                     TO  AOPTIONL.
00590      PERFORM 8100-SEND-INITIAL-MAP.
00591
00592      EJECT
00593  0040-PROCESS-INPUT.
00594 *    NOTE *******************************************************
00595 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00596 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00597 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00598 *         *******************************************************.
00599
00600      IF EIBAID = DFHCLEAR
00601          GO TO 9400-CLEAR.
00602
00603      IF PI-PROCESSOR-ID = 'LGXX'
00604          NEXT SENTENCE
00605      ELSE
00606          
      * EXEC CICS READQ TS
00607 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00608 *            INTO    (SECURITY-CONTROL)
00609 *            LENGTH  (SC-COMM-LENGTH)
00610 *            ITEM    (SC-ITEM)
00611 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004868' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00612          MOVE SC-CLAIMS-DISPLAY (12)   TO  PI-DISPLAY-CAP
00613          MOVE SC-CLAIMS-UPDATE  (12)   TO  PI-MODIFY-CAP
00614          IF NOT MODIFY-CAP
00615              MOVE 'UPDATE'             TO  SM-READ
00616              PERFORM 9995-SECURITY-VIOLATION
00617              MOVE ER-0070              TO  EMI-ERROR
00618              GO TO 8100-SEND-INITIAL-MAP.
00619
00620      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00621          MOVE ER-0008               TO  EMI-ERROR
00622          MOVE -1                 TO  APFKL
00623          GO TO 8200-SEND-DATAONLY.
00624
00625      
      * EXEC CICS RECEIVE
00626 *        INTO   (EL175AI)
00627 *        MAPSET (WS-MAPSET-NAME)
00628 *        MAP    (WS-MAP-NAME)
00629 *    END-EXEC.
           MOVE LENGTH OF
            EL175AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004887' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL175AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00630
00631      IF APFKL > ZERO
00632          IF EIBAID NOT = DFHENTER
00633              MOVE ER-0004           TO  EMI-ERROR
00634              MOVE AL-UNBOF       TO  APFKA
00635              MOVE -1             TO  APFKL
00636              GO TO 8200-SEND-DATAONLY
00637            ELSE
00638              IF APFKO IS NUMERIC
00639                AND APFKO > ZERO
00640                AND APFKO < '25'
00641                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00642                ELSE
00643                  MOVE ER-0029           TO  EMI-ERROR
00644                  MOVE AL-UNBOF       TO  APFKA
00645                  MOVE -1             TO  APFKL
00646                  GO TO 8200-SEND-DATAONLY.
00647
00648      IF EIBAID = DFHPF12
00649          MOVE EL010              TO  THIS-PGM
00650          GO TO 9300-XCTL.
00651
00652      IF EIBAID = DFHPF23
00653          GO TO 9000-RETURN-CICS.
00654
00655      IF EIBAID = DFHPF24
00656          MOVE EL126              TO  THIS-PGM
00657          GO TO 9300-XCTL.
092809
092809     IF EIBAID = DFHPF1 AND PI-SCREEN-PROCESSED
092809        IF AOPTIONI = '1' OR '2' OR '3' OR '4'
092809            IF ABYL = +0 AND
092809               AAMTL = +0 AND
092809               ACARRL = +0 AND
092809               AGROUPL = +0 AND
092809               AACCTL = +0 AND
092809               ACLNO07L = +0 AND
092809               ACLNO08L = +0 AND
092809               ACLNO09L = +0 AND
092809               ACLNO10L = +0 AND
092809               ACLNO11L = +0 AND
092809               ACLNO12L = +0 AND
092809               ACLNO13L = +0 AND
092809               ACLNO14L = +0 AND
092809               ACLNO15L = +0 AND
092809               ACLNO16L = +0 AND
092809               ACLNO17L = +0 AND
092809               ACLNO18L = +0 AND
092809               ACLNO19L = +0 AND
092809               ACLNO20L = +0 AND
092809               ACLNO21L = +0
092809                  MOVE +2       TO PI-PROC-SW
092809            END-IF
092809        END-IF
092809     END-IF.
00658
00659      IF EIBAID = DFHPF1 AND
00660         PI-SCREEN-PROCESSED
00661           GO TO 0200-PROCESS-CHECK-RELEASE.
00662
00663      IF EIBAID NOT = DFHENTER
00664          MOVE ER-0008            TO  EMI-ERROR
00665          MOVE -1                 TO  APFKL
00666          GO TO 8200-SEND-DATAONLY.
00667
00668      EJECT
00669  0100-MAIN-LOGIC.
00670
00671 *    NOTE *******************************************************
00672 *         *      OPTION                                         *
00673 *         *        1   -     ALL PAYMENTS                       *
00674 *         *        2   -     LIFE PAYMENTS  (COVERAGE 1)        *
00675 *         *        3   -     A&H PAYMENTS   (COVERAGE 2)        *
00676 *         *        4   -     AUTO PAYMENTS                      *
00677 *         *        5   -     COMBINED PAYMENTS                  *
00678 *         *******************************************************.
00679
00680      IF AOPTIONL GREATER ZERO AND
00681         (AOPTIONI GREATER ZERO AND LESS '6')
00682          MOVE AL-UNNON           TO  AOPTIONA
00683        ELSE
00684          MOVE -1                 TO  AOPTIONL
00685          MOVE AL-UNBON           TO  AOPTIONA
00686          MOVE ER-0330               TO  EMI-ERROR
00687          PERFORM 9900-ERROR-FORMAT.
00688
00689      IF AAMTL GREATER ZERO
00690         
      * EXEC CICS BIF DEEDIT
00691 *            FIELD (AAMTI)
00692 *            LENGTH (7)
00693 *       END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004979' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00694         IF AAMTI NOT NUMERIC
00695             MOVE -1             TO  AAMTL
00696             MOVE AL-UABON       TO  AAMTA
00697             MOVE ER-0078        TO  EMI-ERROR
00698             PERFORM 9900-ERROR-FORMAT
00699          ELSE
00700             MOVE AAMTI          TO  PI-CHECK-AMOUNT
00701                                     AAMTO
00702             MOVE AL-UANON       TO  AAMTA.
00703
00704      IF PI-COMPANY-ID = 'CSL' AND
00705         AOPTIONI = '1'
00706          MOVE -1                TO  AOPTIONL
00707          MOVE AL-UNBON          TO  AOPTIONA
00708          MOVE ER-0330           TO  EMI-ERROR
00709          PERFORM 9900-ERROR-FORMAT.
00710
00711      IF NOT PI-NO-CARRIER-SECURITY
00712          IF ACARRL GREATER THAN +0
00713             IF ACARRI NOT = PI-CARRIER-SECURITY
00714                MOVE -1      TO ACARRL
00715                MOVE ER-2370 TO EMI-ERROR
00716                PERFORM 9900-ERROR-FORMAT.
00717
00718      IF NOT PI-NO-CARRIER-SECURITY
00719         IF GCARRL GREATER THAN +0
00720            IF GCARRI NOT = PI-CARRIER-SECURITY
00721               MOVE -1            TO GCARRL
00722               MOVE AL-UABOF      TO  GCARRA
00723               MOVE ER-2370       TO EMI-ERROR
00724               PERFORM 9900-ERROR-FORMAT.
00725
00726      IF AOPTIONI NOT = '5'
00727         GO TO 0105-MAIN-LOGIC.
00728
00729      IF GCARRL  > +0 OR
00730         GGROUPL > +0 OR
00731         GSTATEL > +0 OR
00732         GACCTL  > +0 OR
00733         GBENEL  > +0
00734         NEXT SENTENCE
00735      ELSE
00736         GO TO 0105-MAIN-LOGIC.
00737
00738      IF (GCARRL > +0 AND
00739         GGROUPL > +0 AND
00740         GSTATEL > +0 AND
00741         GACCTL  > +0 AND
00742         GBENEL  = +0)
00743        OR
00744         (GCARRL = +0 AND
00745         GGROUPL = +0 AND
00746         GSTATEL = +0 AND
00747         GACCTL  = +0 AND
00748         GBENEL  > +0)
00749         NEXT SENTENCE
00750      ELSE
00751         MOVE ER-1882 TO EMI-ERROR
00752         MOVE -1      TO GCARRL
00753         PERFORM 9900-ERROR-FORMAT.
00754
00755  0105-MAIN-LOGIC.
00756
00757      IF NOT EMI-NO-ERRORS
00758         GO TO 8200-SEND-DATAONLY.
00759
090309     IF AOPTIONI = '1' OR '2' OR '3' OR '4'
090309         IF ABYL = +0 AND
090309            AAMTL = +0 AND
090309            ACARRL = +0 AND
090309            AGROUPL = +0 AND
090309            AACCTL = +0 AND
090309            ACLNO07L = +0 AND
090309            ACLNO08L = +0 AND
090309            ACLNO09L = +0 AND
090309            ACLNO10L = +0 AND
090309            ACLNO11L = +0 AND
090309            ACLNO12L = +0 AND
090309            ACLNO13L = +0 AND
090309            ACLNO14L = +0 AND
090309            ACLNO15L = +0 AND
090309            ACLNO16L = +0 AND
090309            ACLNO17L = +0 AND
090309            ACLNO18L = +0 AND
090309            ACLNO19L = +0 AND
090309            ACLNO20L = +0 AND
090309            ACLNO21L = +0
090309               MOVE 'Y'      TO WS-ALL-CHECK-RELEASE
090309         END-IF
090309     ELSE
090309         MOVE 'N'            TO WS-ALL-CHECK-RELEASE
090309     END-IF.
090309
00760      MOVE +9                     TO  WS-KEY-LENGTH.
00761
00762      SET EL175A-INDEX1
00763          EL175A-INDEX2  TO  +1.
00764
00765  0110-MAIN-LOGIC.
00766
00767      IF EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00768          IF NOT PI-NO-CARRIER-SECURITY
00769             IF PI-CARRIER-SECURITY NOT = EL175A-CARRIER
00770                                  (EL175A-INDEX1 EL175A-INDEX2)
00771                MOVE ER-2370 TO EMI-ERROR
00772                MOVE -1      TO EL175A-CARRIER-ATTRB
00773                               (EL175A-INDEX1 EL175A-INDEX2)
00774                PERFORM 9900-ERROR-FORMAT
00775                GO TO 0150-MAIN-LOGIC.
00776
00777      IF EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00778         IF NOT PI-NO-CARRIER-SECURITY
00779            MOVE PI-CARRIER-SECURITY TO EL175A-CARRIER
00780                               (EL175A-INDEX1 EL175A-INDEX2)
00781            MOVE +1 TO EL175A-CARRIER-LENGTH
00782                               (EL175A-INDEX1 EL175A-INDEX2).
00783
00784      MOVE EMI-FATAL-CTR          TO  WS-LAST-ERROR-COUNT.
00785
00786      IF EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00787                       OR
00788         EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00789          NEXT SENTENCE
00790        ELSE
00791          GO TO 0150-MAIN-LOGIC.
00792
00793      IF (EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2)
00794                                           NOT > ZERO
00795                         OR
00796          EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2)
00797                                           NOT > ZERO)
00798             MOVE ER-0431               TO  EMI-ERROR
00799             PERFORM 9900-ERROR-FORMAT
00800             GO TO 0150-MAIN-LOGIC.
00801
00802      MOVE SPACES                 TO  WS-ACTIVITY-QUE-KEY.
00803      MOVE PI-COMPANY-CD          TO  WS-AQK-COMPANY-CD.
00804
00805      MOVE EL175A-CLAIM   (EL175A-INDEX1 EL175A-INDEX2)
00806                                  TO  WS-AQK-CLAIM-NO
00807                                      WS-CLAIM-NO.
00808
00809      MOVE EL175A-CARRIER (EL175A-INDEX1 EL175A-INDEX2)
00810                                  TO  WS-AQK-CARRIER
00811                                      WS-CARRIER.
00812
00813      IF WS-ELACTQ-BROWSE-SW = ZERO
00814          
      * EXEC CICS STARTBR
00815 *            DATASET   (WS-ACTIVITY-QUE-DSID)
00816 *            RIDFLD    (WS-ACTIVITY-QUE-KEY)
00817 *            GENERIC   EQUAL
00818 *            KEYLENGTH (WS-KEY-LENGTH)
00819 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00005130' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 WS-ACTIVITY-QUE-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00820          MOVE +1                 TO  WS-ELACTQ-BROWSE-SW
00821        ELSE
00822          
      * EXEC CICS RESETBR
00823 *            DATASET   (WS-ACTIVITY-QUE-DSID)
00824 *            RIDFLD    (WS-ACTIVITY-QUE-KEY)
00825 *            GENERIC   EQUAL
00826 *            KEYLENGTH (WS-KEY-LENGTH)
00827 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4   KG    E          &   #00005138' TO DFHEIV0
           MOVE X'26342020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 WS-ACTIVITY-QUE-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00828
00829  0120-MAIN-LOGIC.
00830
00831      
      * EXEC CICS READNEXT
00832 *        DATASET (WS-ACTIVITY-QUE-DSID)
00833 *        RIDFLD  (WS-ACTIVITY-QUE-KEY)
00834 *        SET     (ADDRESS OF ACTIVITY-QUE)
00835 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005147' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00836
00837      IF AQ-PAYMENT-COUNTER NOT NUMERIC
00838          MOVE ZEROS              TO AQ-PAYMENT-COUNTER.
00839
00840      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC
00841          MOVE ZEROS              TO AQ-PMT-UNAPPROVED-COUNT.
00842
00843      IF WS-AQK-COMPANY-CD NOT = PI-COMPANY-CD  OR
00844         WS-AQK-CARRIER    NOT = WS-CARRIER     OR
00845         WS-AQK-CLAIM-NO   NOT = WS-CLAIM-NO
00846          MOVE ER-0432               TO  EMI-ERROR
00847          PERFORM 9900-ERROR-FORMAT
00848          GO TO 0150-MAIN-LOGIC.
00849
00850      IF NOT PENDING-PAYMENTS
00851          GO TO 0120-MAIN-LOGIC.
00852
00853      MOVE AL-UANON               TO EL175A-CLAIM-ATTRB
00854                                    (EL175A-INDEX1  EL175A-INDEX2).
00855
00856      IF EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00857          MOVE AL-UANON         TO  EL175A-CARRIER-ATTRB
00858                                   (EL175A-INDEX1 EL175A-INDEX2).
00859
00860      MOVE +1                     TO  ACLNO07L.
00861
00862      GO TO 0150-MAIN-LOGIC.
00863
00864  0140-MAIN-LOGIC.
00865      MOVE ER-0331                   TO  EMI-ERROR.
00866      PERFORM 9900-ERROR-FORMAT.
00867
00868  0150-MAIN-LOGIC.
00869
00870      IF EMI-FATAL-CTR NOT GREATER THAN WS-LAST-ERROR-COUNT
00871          GO TO 0160-MAIN-LOGIC.
00872
00873      IF EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00874          MOVE AL-UABON           TO  EL175A-CLAIM-ATTRB
00875                                     (EL175A-INDEX1  EL175A-INDEX2)
00876        ELSE
00877          MOVE AL-UABOF           TO  EL175A-CLAIM-ATTRB
00878                                    (EL175A-INDEX1  EL175A-INDEX2).
00879
00880      MOVE -1                     TO  EL175A-CLAIM-LENGTH
00881                                     (EL175A-INDEX1  EL175A-INDEX2)
00882
00883      IF EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0
00884          MOVE AL-UABON           TO  EL175A-CARRIER-ATTRB
00885                                    (EL175A-INDEX1  EL175A-INDEX2)
00886        ELSE
00887          MOVE AL-UABOF           TO  EL175A-CARRIER-ATTRB
00888                                    (EL175A-INDEX1  EL175A-INDEX2).
00889
00890  0160-MAIN-LOGIC.
00891      IF EL175A-INDEX1 LESS THAN +5
00892          SET EL175A-INDEX1 UP BY +1
00893          GO TO 0110-MAIN-LOGIC.
00894
00895      IF EL175A-INDEX2 LESS THAN +3
00896          SET EL175A-INDEX1 TO +1
00897          SET EL175A-INDEX2 UP BY +1
00898          GO TO 0110-MAIN-LOGIC.
00899
00900      IF WS-ELACTQ-BROWSE-SW NOT = ZERO
00901          MOVE ZERO               TO  WS-ELACTQ-BROWSE-SW
00902          
      * EXEC CICS ENDBR
00903 *            DATASET (WS-ACTIVITY-QUE-DSID)
00904 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005218' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00905
00906      IF EMI-FATAL-CTR GREATER THAN ZERO
00907          MOVE ZERO               TO PI-PROC-SW
00908          GO TO 8200-SEND-DATAONLY.
00909
00910      EJECT
00911  0200-PROCESS-CHECK-RELEASE.
00912 *    NOTE *******************************************************
00913 *         *      OBTAIN EXCLUSIVE CONTROL OF THE CHECK QUEUE    *
00914 *         *  DATASET DURING THE GENERATION OF THE CHECK QUEUE   *
00915 *         *  RECORDS.                                           *
00916 *         *******************************************************.
00917
00918      MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.
00919
00920      IF EIBAID = DFHPF1
00921          
      * EXEC CICS ENQ
00922 *            RESOURCE (WS-CHECK-QUE-DSID)
00923 *            LENGTH   (11)
00924 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '2$L                   $   #00005237' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00925
00926 *    NOTE *******************************************************
00927 *         *      GET THE CONTROL GROUP NUMBER FROM THE COMPANY  *
00928 *         *  CONTROL RECORD OF THE CONTROL FILE.                *
00929 *         *******************************************************.
00930
00931      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
00932
00933      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00934      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
00935      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00936
00937      IF EIBAID = DFHPF1
00938          
      * EXEC CICS READ UPDATE
00939 *            DATASET (WS-CONTROL-FILE-DSID)
00940 *            RIDFLD  (WS-CONTROL-FILE-KEY)
00941 *            SET     (ADDRESS OF CONTROL-FILE)
00942 *        END-EXEC
      *    MOVE '&"S        EU         (   #00005254' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00943        ELSE
00944          
      * EXEC CICS READ
00945 *            DATASET (WS-CONTROL-FILE-DSID)
00946 *            RIDFLD  (WS-CONTROL-FILE-KEY)
00947 *            SET     (ADDRESS OF CONTROL-FILE)
00948 *        END-EXEC.
      *    MOVE '&"S        E          (   #00005260' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00949
00950      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00951      MOVE '5'                    TO  DC-OPTION-CODE.
00952      PERFORM 8500-DATE-CONVERSION.
00953      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.
00954      MOVE EIBTIME                TO  TIME-IN.
00955
00956      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00957          IF AOPTIONI = '1'
00958              GO TO 0200-SET-CONTROL-GROUPS.
00959
00960      ADD +1                      TO  CF-CO-CHECK-QUE-COUNTER.
00961
00962      IF CO-QUE-COUNT-RESET
00963          MOVE +1                 TO  CF-CO-CHECK-QUE-COUNTER.
00964
00965      MOVE CF-FORMS-PRINTER-ID      TO  WS-FORMS-PRINTER
00966      MOVE CF-CO-CHECK-QUE-COUNTER  TO  WS-CHECK-QUE-COUNTER.
00967      MOVE CF-PAYMENT-APPROVAL-SW   TO  WS-PMT-APPROVAL.
00968      MOVE CF-CURRENT-MONTH-END     TO  PI-MONTH-END-SAVE.
00969
00970      GO TO 0200-REWRITE-CONTROL-FILE.
00971
00972  0200-SET-CONTROL-GROUPS.
00973
00974      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00975          ADD +1                    TO  CF-CO-CHECK-QUE-COUNTER
00976          IF CO-QUE-COUNT-RESET
00977              MOVE +1               TO  CF-CO-CHECK-QUE-COUNTER
00978                                        WS-CHECK-QUE-COUNTER
00979              ADD +1                TO  CF-CO-CHECK-QUE-COUNTER
00980              MOVE CF-CO-CHECK-QUE-COUNTER
00981                                    TO  WS-CHECK-QUE-COUNTER-2
00982              ADD +1                TO  CF-CO-CHECK-QUE-COUNTER
00983              MOVE CF-CO-CHECK-QUE-COUNTER
00984                                    TO  WS-CHECK-QUE-COUNTER-3
00985          ELSE
00986              MOVE CF-CO-CHECK-QUE-COUNTER
00987                                    TO  WS-CHECK-QUE-COUNTER
00988              ADD +1                TO  CF-CO-CHECK-QUE-COUNTER
00989              IF CO-QUE-COUNT-RESET
00990                  MOVE +1           TO  CF-CO-CHECK-QUE-COUNTER
00991                                        WS-CHECK-QUE-COUNTER-2
00992                  ADD +1            TO  CF-CO-CHECK-QUE-COUNTER
00993                  MOVE CF-CO-CHECK-QUE-COUNTER
00994                                    TO  WS-CHECK-QUE-COUNTER-3
00995             ELSE
00996                 MOVE CF-CO-CHECK-QUE-COUNTER
00997                                    TO  WS-CHECK-QUE-COUNTER-2
00998                 ADD +1             TO  CF-CO-CHECK-QUE-COUNTER
00999                 IF CO-QUE-COUNT-RESET
01000                     MOVE +1        TO  CF-CO-CHECK-QUE-COUNTER
01001                                        WS-CHECK-QUE-COUNTER-3
01002                 ELSE
01003                     MOVE CF-CO-CHECK-QUE-COUNTER
01004                                    TO  WS-CHECK-QUE-COUNTER-3.
01005
01006      MOVE CF-FORMS-PRINTER-ID      TO  WS-FORMS-PRINTER
01007      MOVE CF-PAYMENT-APPROVAL-SW   TO  WS-PMT-APPROVAL.
01008      MOVE CF-CURRENT-MONTH-END     TO  PI-MONTH-END-SAVE.
01009
01010  0200-REWRITE-CONTROL-FILE.
01011
01012      IF EIBAID = DFHPF1
01013          
      * EXEC CICS REWRITE
01014 *            DATASET   (WS-CONTROL-FILE-DSID)
01015 *            FROM      (CONTROL-FILE)
01016 *        END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005329' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01017
01018      EJECT
01019  0200-CONTINUE.
01020 *    NOTE *******************************************************
01021 *         *      GET STORAGE FOR WRITING CHECK QUEUE RECORDS    *
01022 *         *******************************************************.
01023
01024      
      * EXEC CICS GETMAIN
01025 *        SET     (ADDRESS OF CHECK-QUE)
01026 *        LENGTH  (100)
01027 *        INITIMG (WS-SPACES)
01028 *    END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00005340' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01029
01030 *    NOTE *******************************************************
01031 *         *      BROWSE THE ACTIVITY QUEUE TO FIND ANY CLAIMS   *
01032 *         *  THAT HAVE PAYMENTS PENDING.                        *
01033 *         *                                                     *
01034 *         *      WHEN A PAYMENT IS PENDING, BROWSE THE ACTIVITY *
01035 *         *  TRAILERS TO LOCATE THE PAYMENT TRAILER RECORDS. IF *
01036 *         *  THE PAYMENT TRAILER MEETS THE QUALIFICATION FOR    *
01037 *         *  THIS CHECK RELEASE RUN, GENERATE A CHECK QUEUE     *
01038 *         *  RECORD.                                            *
01039 *         *******************************************************.
01040
01041      MOVE LOW-VALUES             TO  WS-ACTIVITY-QUE-KEY.
01042      MOVE PI-COMPANY-CD          TO  WS-AQK-COMPANY-CD.
01043
01044      IF ACARRL GREATER THAN ZERO
01045          MOVE ACARRI             TO  WS-AQK-CARRIER.
01046
01047      EJECT
01048  0205-STARTBR-ELACTQ.
01049      
      * EXEC CICS HANDLE CONDITION
01050 *        NOTFND  (0300-END-OF-SEARCH)
01051 *        ENDFILE (0300-END-OF-SEARCH)
01052 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00005365' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01053
01054      IF WS-ELACTQ-BROWSE-SW = ZERO
01055          
      * EXEC CICS STARTBR
01056 *            DATASET (WS-ACTIVITY-QUE-DSID)
01057 *            RIDFLD  (WS-ACTIVITY-QUE-KEY)
01058 *            GTEQ
01059 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005371' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 WS-ACTIVITY-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01060          MOVE +1                 TO  WS-ELACTQ-BROWSE-SW.
01061
01062  0220-READNEXT-ELACTQ.
01063      
      * EXEC CICS READNEXT
01064 *        DATASET (WS-ACTIVITY-QUE-DSID)
01065 *        RIDFLD  (WS-ACTIVITY-QUE-KEY)
01066 *        SET     (ADDRESS OF ACTIVITY-QUE)
01067 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005379' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01068
01069      IF AQ-PAYMENT-COUNTER NOT NUMERIC
01070          MOVE ZEROS              TO AQ-PAYMENT-COUNTER.
01071
01072      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC
01073          MOVE ZEROS              TO AQ-PMT-UNAPPROVED-COUNT.
01074
01075      IF WS-ACTIVITY-QUE-KEY = WS-LAST-ACTIVITY-QUE-KEY
01076          GO TO 0220-READNEXT-ELACTQ.
01077
01078      MOVE WS-ACTIVITY-QUE-KEY    TO  WS-LAST-ACTIVITY-QUE-KEY.
01079
01080      IF WS-AQK-COMPANY-CD NOT = PI-COMPANY-CD
01081          GO TO 0300-END-OF-SEARCH.
01082
01083      IF NOT PENDING-PAYMENTS
01084          GO TO 0220-READNEXT-ELACTQ.
01085
01086      IF WS-PMT-APPROVAL-USED
01087         IF AQ-PMT-UNAPPROVED-COUNT NOT EQUAL +0
031808*01088            GO TO 0220-READNEXT-ELACTQ.
031808           PERFORM 3000-TOTAL-UNAPPROVED THRU 3999-EXIT.
080712     
      * EXEC CICS HANDLE CONDITION
080712*        NOTFND  (0300-END-OF-SEARCH)
080712*        ENDFILE (0300-END-OF-SEARCH)
080712*    END-EXEC
      *    MOVE '"$I''                  ! $ #00005406' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035343036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01090      IF ACARRL > ZERO   AND
01091         AQ-CARRIER > ACARRI
01092              GO TO 0300-END-OF-SEARCH.
01093
01094      IF ACLNO07L NOT GREATER THAN ZERO
092809       AND ACLNO08L NOT GREATER THAN ZERO
092809       AND ACLNO09L NOT GREATER THAN ZERO
092809       AND ACLNO10L NOT GREATER THAN ZERO
092809       AND ACLNO11L NOT GREATER THAN ZERO
092809       AND ACLNO12L NOT GREATER THAN ZERO
092809       AND ACLNO13L NOT GREATER THAN ZERO
092809       AND ACLNO14L NOT GREATER THAN ZERO
092809       AND ACLNO15L NOT GREATER THAN ZERO
092809       AND ACLNO16L NOT GREATER THAN ZERO
092809       AND ACLNO17L NOT GREATER THAN ZERO
092809       AND ACLNO18L NOT GREATER THAN ZERO
092809       AND ACLNO19L NOT GREATER THAN ZERO
092809       AND ACLNO20L NOT GREATER THAN ZERO
092809       AND ACLNO21L NOT GREATER THAN ZERO
01095          GO TO 0240-MAIN-LOGIC.
01096
01097      SET EL175A-INDEX1
01098          EL175A-INDEX2 TO +1.
01099
01100      EJECT
01101  0230-MAIN-LOGIC.
01102      IF AQ-CLAIM-NO = EL175A-CLAIM (EL175A-INDEX1 EL175A-INDEX2)
01103          GO TO 0240-MAIN-LOGIC.
01104
01105      IF EL175A-INDEX1 LESS THAN +5
01106          SET EL175A-INDEX1 UP BY +1
01107          GO TO 0230-MAIN-LOGIC.
01108
01109      IF EL175A-INDEX2 LESS THAN +3
01110          SET EL175A-INDEX1 TO +1
01111          SET EL175A-INDEX2 UP BY +1
01112          GO TO 0230-MAIN-LOGIC.
01113
01114      GO TO 0220-READNEXT-ELACTQ.
01115
01116  0240-MAIN-LOGIC.
01117      
      * EXEC CICS ENDBR
01118 *        DATASET (WS-ACTIVITY-QUE-DSID)
01119 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005451' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01120
01121      MOVE ZERO                   TO  WS-ELACTQ-BROWSE-SW.
01122
01123      IF EIBAID = DFHPF1
01124          
      * EXEC CICS READ UPDATE
01125 *            DATASET (WS-ACTIVITY-QUE-DSID)
01126 *            RIDFLD  (WS-ACTIVITY-QUE-KEY)
01127 *            SET     (ADDRESS OF ACTIVITY-QUE)
01128 *        END-EXEC
      *    MOVE '&"S        EU         (   #00005458' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01129       ELSE
01130          
      * EXEC CICS READ
01131 *            DATASET (WS-ACTIVITY-QUE-DSID)
01132 *            RIDFLD  (WS-ACTIVITY-QUE-KEY)
01133 *            SET     (ADDRESS OF ACTIVITY-QUE)
01134 *        END-EXEC.
      *    MOVE '&"S        E          (   #00005464' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01135
01136      MOVE ZERO                   TO  WS-UPDATE-SW.
01137
01138      MOVE AQ-PAYMENT-COUNTER     TO  WS-PAYMENT-COUNTER.
01139
01140      MOVE WS-ACTIVITY-QUE-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.
01141      MOVE ZERO                   TO  WS-ATK-SEQUENCE-NO.
01142
01143      MOVE LOW-VALUES             TO  WS-LAST-ACTIVITY-TRAILERS-KEY
01144
01145      
      * EXEC CICS HANDLE CONDITION
01146 *        NOTFND  (0260-MAIN-LOGIC)
01147 *        ENDFILE (0260-MAIN-LOGIC)
01148 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00005479' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01149
01150      EJECT
01151  0250-MAIN-LOGIC.
01152      IF AQ-PAYMENT-COUNTER NOT GREATER THAN ZERO
01153          GO TO 0260-MAIN-LOGIC.
01154
01155      IF WS-ELTRLR-BROWSE-SW = ZERO
01156          
      * EXEC CICS STARTBR
01157 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
01158 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
01159 *            EQUAL
01160 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00005490' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01161          MOVE +1               TO  WS-ELTRLR-BROWSE-SW
01162        ELSE
01163          
      * EXEC CICS RESETBR
01164 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
01165 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
01166 *            EQUAL
01167 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         E          &   #00005497' TO DFHEIV0
           MOVE X'263420202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01168
01169      EJECT
01170  0255-READNEXT-ELTRLR.
01171      
      * EXEC CICS READNEXT
01172 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
01173 *        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
01174 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
01175 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005505' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01176
01177      IF WS-ACTIVITY-TRAILERS-KEY = WS-LAST-ACTIVITY-TRAILERS-KEY
01178          GO TO 0255-READNEXT-ELTRLR.
01179
01180      MOVE WS-ACTIVITY-TRAILERS-KEY  TO
01181                                   WS-LAST-ACTIVITY-TRAILERS-KEY
01182
01183      IF WS-AQK-COMPANY-CD NOT = WS-ATK-COMPANY-CD  OR
01184         WS-AQK-CARRIER    NOT = WS-ATK-CARRIER     OR
01185         WS-AQK-CLAIM-NO   NOT = WS-ATK-CLAIM-NO    OR
01186         WS-AQK-CERT-NO    NOT = WS-ATK-CERT-NO
01187          GO TO 0260-MAIN-LOGIC.
01188
01189      IF AT-TRAILER-TYPE NOT = '2'
01190          GO TO 0255-READNEXT-ELTRLR.
01191
01192 *    NOTE *******************************************************
01193 *         *      OPTION                                         *
01194 *         *        1    -    ALL PAYMENTS                       *
01195 *         *        2    -    LIFE PAYMENTS (COVERAGE 1)         *
01196 *         *        3    -    A&H PAYMENTS  (COVERAGE 2)         *
01197 *         *        4    -    AUTO PAYMENTS                      *
01198 *         *        5    -    COMBINED PAYMENTS                  *
01199 *         *******************************************************.
01200
01201      IF (AOPTIONI = '2' AND
100518         AT-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O')
01203        OR
01204         (AOPTIONI = '3' AND
121203         (AT-CLAIM-TYPE NOT = PI-AH-OVERRIDE-L1 AND 'I' AND
052614          'G' AND 'F'))
01206        OR
01207         (AOPTIONI = '4' AND
01208          AT-PAYMENT-ORIGIN NOT = '2')
01209        OR
01210         (AOPTIONI = '5' AND
01211          AT-GROUPED-PAYMENT NOT = 'Y')
01212              GO TO 0255-READNEXT-ELTRLR.
01213
01214      IF AOPTIONI NOT = '5'
01215         IF AT-GROUPED-PAYMENT = 'Y'
01216            GO TO 0255-READNEXT-ELTRLR.
01217
01218      IF AT-AMOUNT-PAID   NEGATIVE
01219          GO TO 0255-READNEXT-ELTRLR.
01220
01221      IF AAMTL GREATER ZERO
01222          IF AT-AMOUNT-PAID LESS PI-CHECK-AMOUNT
01223              NEXT SENTENCE
01224            ELSE
01225              GO TO 0255-READNEXT-ELTRLR.
01226
01227      IF OFFLINE-PMT
01228          SUBTRACT +1 FROM WS-PAYMENT-COUNTER
01229          GO TO 0255-READNEXT-ELTRLR.
01230
01231      IF AT-VOID-DT NOT = LOW-VALUES
01232          GO TO 0255-READNEXT-ELTRLR.
01233
01234      IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
01235          GO TO 0255-READNEXT-ELTRLR.
01236
01237      IF PI-COMPANY-ID NOT = 'DMD'
01238        IF AT-CASH-PAYMENT = 'N'
01239          GO TO 0257-CONTINUE-PAYMENTS.
01240
01241      IF ABYL GREATER THAN ZERO
01242        AND ABYI NOT = AT-RECORDED-BY
01243          GO TO 0255-READNEXT-ELTRLR.
01244
01245      IF AT-TO-BE-WRITTEN-DT GREATER THAN WS-CURRENT-DATE
01246          GO TO 0255-READNEXT-ELTRLR.
01247
01248      IF AT-CHECK-QUE-CONTROL NOT = ZERO
01249          GO TO 0255-READNEXT-ELTRLR.
01250
01251      IF WS-PMT-APPROVAL-USED
01252         IF AT-PAYMENT-APPROVAL-SW NOT = 'A'
01253            GO TO 0255-READNEXT-ELTRLR.
01254
01255      IF AGROUPL > +0 OR
01256         AACCTL > +0 OR
01257         AOPTIONI = '5'
01258          NEXT SENTENCE
01259      ELSE
01260          GO TO 0257-CONTINUE-PAYMENTS.
01261
01262      MOVE AT-CONTROL-PRIMARY TO WS-ELMSTR-KEY.
01263
01264      
      * EXEC CICS READ
01265 *         DATASET   ('ELMSTR')
01266 *         RIDFLD    (WS-ELMSTR-KEY)
01267 *         SET       (ADDRESS OF CLAIM-MASTER)
01268 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005599' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01269
01270      IF AGROUPL > +0
01271          IF AGROUPI = CL-CERT-GROUPING
01272              NEXT SENTENCE
01273          ELSE
01274              GO TO 0255-READNEXT-ELTRLR.
01275
01276      IF AACCTL > +0
01277          IF AACCTI = CL-CERT-ACCOUNT
01278              NEXT SENTENCE
01279          ELSE
01280              GO TO 0255-READNEXT-ELTRLR.
01281
01282      IF GBENEL > +0
01283         IF AT-PAYEE-TYPE = 'B' AND
01284            GBENEI = CL-BENEFICIARY
01285            NEXT SENTENCE
01286         ELSE
01287            GO TO 0255-READNEXT-ELTRLR.
01288
01289      IF GACCTL > +0
01290         IF AT-PAYEE-TYPE = 'A'        AND
01291            GCARRI  = CL-CERT-CARRIER  AND
01292            GGROUPI = CL-CERT-GROUPING AND
01293            GSTATEI = CL-CERT-STATE    AND
01294            GACCTI  = CL-CERT-ACCOUNT
01295              NEXT SENTENCE
01296           ELSE
01297              GO TO 0255-READNEXT-ELTRLR.
01298
01299      EJECT
01300  0257-CONTINUE-PAYMENTS.
01301
01302 *    NOTE *******************************************************
01303 *         *      THE PAYMENT TRAILER HAS MET ALL OF THE         *
01304 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         *
01305 *         *  GENERATE A CHECK QUEUE RECORD.                     *
01306 *         *******************************************************.
01307
01308      SUBTRACT +1 FROM AQ-PAYMENT-COUNTER
01309                       WS-PAYMENT-COUNTER.
01310
01311      MOVE +1                         TO  WS-UPDATE-SW.
01312
01313      IF PI-COMPANY-ID NOT = 'DMD'
01314        IF AT-CASH-PAYMENT = 'N'
01315          ADD +1                      TO  WS-NON-CASH-REL-CNT
01316          ADD AT-AMOUNT-PAID          TO  WS-NON-CASH-REL-AMT
01317          GO TO 0257-END-BROWSE-ELACTQ.
01318
01319      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01320          IF AOPTIONI = '1'
01321              NEXT SENTENCE
01322          ELSE
01323              ADD +1                  TO  WS-RELEASED-COUNT
01324              ADD AT-AMOUNT-PAID      TO  WS-RELEASED-AMOUNT
01325              GO TO 0257-END-BROWSE-ELACTQ
01326      ELSE
01327          ADD +1                      TO  WS-RELEASED-COUNT
01328          ADD AT-AMOUNT-PAID          TO  WS-RELEASED-AMOUNT
01329          GO TO 0257-END-BROWSE-ELACTQ.
01330
01331      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01332          IF AT-PAYMENT-TYPE = '5' OR '6'
01333              ADD +1                  TO  WS-RELEASED-COUNT
01334              ADD AT-AMOUNT-PAID      TO  WS-RELEASED-AMOUNT
01335          ELSE
01336              MOVE AT-CERT-NO         TO  WS-CERT-NO
01337              IF WS-CERT-PRIME-1-3 = '888'
01338                  ADD +1              TO  WS-RELEASED-COUNT-2
01339                  ADD AT-AMOUNT-PAID  TO  WS-RELEASED-AMOUNT-2
01340              ELSE
01341                  ADD +1              TO  WS-RELEASED-COUNT-3
01342                  ADD AT-AMOUNT-PAID  TO  WS-RELEASED-AMOUNT-3.
01343
01344  0257-END-BROWSE-ELACTQ.
01345
01346      
      * EXEC CICS ENDBR
01347 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
01348 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005681' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01349
01350      MOVE ZERO                   TO  WS-ELTRLR-BROWSE-SW.
01351
01352      IF EIBAID = DFHPF1
01353          
      * EXEC CICS READ UPDATE
01354 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
01355 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
01356 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
01357 *        END-EXEC
      *    MOVE '&"S        EU         (   #00005688' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01358        ELSE
01359          
      * EXEC CICS READ
01360 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
01361 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
01362 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
01363 *        END-EXEC.
      *    MOVE '&"S        E          (   #00005694' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01364
01365      MOVE SPACES                 TO  CHECK-QUE.
01366
01367      IF AT-CASH-PAYMENT = 'N'
01368         GO TO 0259-REWRITE-PMT-TRLR.
01369
01370      IF AOPTIONI NOT = '5'
01371         GO TO 0258-CONTINUE-ELCHKQ.
01372
01373      IF AT-PAYEE-TYPE = 'B'
01374         MOVE CL-BENEFICIARY      TO  CQ-PAYEE-BENE-ACCT
01375      ELSE
01376      IF AT-PAYEE-TYPE = 'A'
01377         MOVE AT-CARRIER          TO  CQ-PAYEE-CARRIER
01378         MOVE CL-CERT-GROUPING    TO  CQ-PAYEE-GROUPING
01379         MOVE CL-CERT-STATE       TO  CQ-PAYEE-STATE
01380         MOVE CL-CERT-ACCOUNT     TO  CQ-PAYEE-BENE-ACCT.
01381
01382  0258-CONTINUE-ELCHKQ.
01383
01384      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01385          IF AOPTIONI = '1'
01386              NEXT SENTENCE
01387          ELSE
01388              MOVE WS-CHECK-QUE-COUNTER
01389                                  TO  CQ-CONTROL-NUMBER
01390                                      CQ-CONTROL-NUMBER-A1
01391                                      AT-CHECK-QUE-CONTROL
01392              MOVE WS-CHECK-COUNTER
01393                                  TO  CQ-SEQUENCE-NUMBER
01394                                      CQ-SEQUENCE-NUMBER-A1
01395                                      AT-CHECK-QUE-SEQUENCE
01396              ADD +1              TO  WS-CHECK-COUNTER
01397              GO TO 0259-CONTINUE-ELCHKQ
01398      ELSE
01399          MOVE WS-CHECK-QUE-COUNTER
01400                                  TO  CQ-CONTROL-NUMBER
01401                                      CQ-CONTROL-NUMBER-A1
01402                                      AT-CHECK-QUE-CONTROL
01403          MOVE WS-CHECK-COUNTER   TO  CQ-SEQUENCE-NUMBER
01404                                      AT-CHECK-QUE-SEQUENCE
01405                                      CQ-SEQUENCE-NUMBER-A1
01406          ADD +1  TO  WS-CHECK-COUNTER
01407          GO TO 0259-CONTINUE-ELCHKQ.
01408
01409      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01410        IF AT-PAYMENT-TYPE = '5' OR '6'
01411          MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER
01412                                          CQ-CONTROL-NUMBER-A1
01413                                          AT-CHECK-QUE-CONTROL
01414          MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER
01415                                          CQ-SEQUENCE-NUMBER-A1
01416                                          AT-CHECK-QUE-SEQUENCE
01417          ADD +1                      TO  WS-CHECK-COUNTER
01418          GO TO 0259-CONTINUE-ELCHKQ.
01419
01420      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01421        MOVE AT-CERT-NO               TO  WS-CERT-NO
01422        IF WS-CERT-PRIME-1-3 = '888'
01423          MOVE WS-CHECK-QUE-COUNTER-2 TO  CQ-CONTROL-NUMBER
01424                                          CQ-CONTROL-NUMBER-A1
01425                                          AT-CHECK-QUE-CONTROL
01426          MOVE WS-CHECK-COUNTER-2     TO  CQ-SEQUENCE-NUMBER
01427                                          CQ-SEQUENCE-NUMBER-A1
01428                                          AT-CHECK-QUE-SEQUENCE
01429          ADD +1                      TO  WS-CHECK-COUNTER-2
01430          GO TO 0259-CONTINUE-ELCHKQ
01431        ELSE
01432          MOVE WS-CHECK-QUE-COUNTER-3 TO  CQ-CONTROL-NUMBER
01433                                          CQ-CONTROL-NUMBER-A1
01434                                          AT-CHECK-QUE-CONTROL
01435          MOVE WS-CHECK-COUNTER-3     TO  CQ-SEQUENCE-NUMBER
01436                                          CQ-SEQUENCE-NUMBER-A1
01437                                          AT-CHECK-QUE-SEQUENCE
01438          ADD +1                      TO  WS-CHECK-COUNTER-3
01439          GO TO 0259-CONTINUE-ELCHKQ.
01440
01441  0259-CONTINUE-ELCHKQ.
01442
01443      MOVE 'CQ'                   TO  CQ-RECORD-ID.
01444      MOVE AT-COMPANY-CD          TO  CQ-COMPANY-CD
01445                                      CQ-COMPANY-CD-A1.
01446
01447      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
01448      MOVE AT-CARRIER             TO  CQ-CARRIER
01449      MOVE AT-CLAIM-NO            TO  CQ-CLAIM-NO.
01450
CIDMOD     MOVE 'CQ'                   TO  CQ-RECORD-ID.
CIDMOD     MOVE AT-COMPANY-CD          TO  CQ-COMPANY-CD
CIDMOD                                     CQ-COMPANY-CD-A1.
CIDMOD     MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER
CIDMOD                                     CQ-CONTROL-NUMBER-A1
CIDMOD                                     AT-CHECK-QUE-CONTROL.
CIDMOD     MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER
CIDMOD                                     AT-CHECK-QUE-SEQUENCE
CIDMOD                                     CQ-SEQUENCE-NUMBER-A1.
CIDMOD     ADD +1  TO  WS-CHECK-COUNTER.
CIDMOD
CIDMOD*    IF (AT-PAYMENT-ORIGIN = '2')  OR
CIDMOD*       (AT-TO-BE-WRITTEN-DT = WS-CURRENT-DATE)
CIDMOD*          PERFORM 0920-WRITE-DLYACTV THRU 0920-EXIT
CIDMOD*    END-IF
CIDMOD
PEMMOD     IF EIBAID = DFHPF1
PEMMOD        PERFORM 0920-WRITE-DLYACTV
PEMMOD                                 THRU 0920-EXIT
PEMMOD     END-IF
PEMMOD
01451      IF PI-COMPANY-ID = 'CVL' OR 'MNL'
01452          MOVE AT-CLAIM-NO        TO  CQ-PAYEE-BENE-ACCT
01453      ELSE
01454          IF PI-COMPANY-ID = 'DMD'
01455              PERFORM 0900-GET-MASTER THRU 0900-EXIT
01456              MOVE AT-COMPANY-CD  TO  CQ-DMD-COMPANY-CD-A2
01457              MOVE AT-PAYEE-TYPE  TO  CQ-DMD-PAYEE-TYPE-A2
01458              MOVE CL-BENEFICIARY TO  CQ-DMD-BENE-CODE-A2
01459              MOVE AT-CLAIM-NO    TO  CQ-DMD-CLAIM-NO-A2
01460              MOVE EIBTIME        TO  CQ-DMD-TIME-SEQ-A2.
01461
01462      MOVE AT-CERT-NO             TO  CQ-CERT-NO.
01463      MOVE AT-CLAIM-TYPE          TO  CQ-CLAIM-TYPE.
01464      MOVE AT-CLAIM-PREM-TYPE     TO  CQ-CLAIM-SUB-TYPE.
01465      MOVE AT-SEQUENCE-NO         TO  CQ-PMT-TRLR-SEQUENCE.
01466      MOVE AT-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.
01467      MOVE AT-CHECK-NO            TO  CQ-CHECK-NUMBER.
01468      MOVE AT-PAYMENT-TYPE        TO  CQ-PAYMENT-TYPE.
01469      MOVE ZERO                   TO  CQ-TIMES-PRINTED
01470                                      CQ-PRINT-AT-HHMM.
01471      MOVE AT-RECORDED-BY         TO  CQ-CHECK-BY-USER.
01472
01473      MOVE LOW-VALUES             TO  CQ-CHECK-WRITTEN-DT.
01474
01475      MOVE +1750                  TO  CQ-LAST-UPDATED-BY.
01476      MOVE SAVE-BIN-DATE          TO  CQ-LAST-UPDATED-DT.
01477      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
01478
01479      IF EIBAID = DFHPF1
01480          
      * EXEC CICS WRITE
01481 *            DATASET (WS-CHECK-QUE-DSID)
01482 *            FROM    (CHECK-QUE)
01483 *            RIDFLD  (CQ-CONTROL-PRIMARY)
01484 *        END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005836' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01485
01486  0259-REWRITE-PMT-TRLR.
01487
01488      MOVE +1750                  TO  AT-PAYMENT-LAST-UPDATED-BY.
01489      MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.
01490
01491      IF AT-CASH-PAYMENT = 'N'
01492          MOVE SAVE-BIN-DATE      TO  AT-CHECK-WRITTEN-DT
01493          MOVE PI-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT.
01494
01495      IF PI-COMPANY-ID = 'DMD' AND
01496         EIBAID = DFHPF1
01497          PERFORM 0900-GET-MASTER  THRU  0900-EXIT.
01498
01499      IF PI-COMPANY-ID = 'DMD'
01500          IF CQ-CHECK-NUMBER NOT NUMERIC
01501              IF EIBAID = DFHPF1
01502                  PERFORM 1000-CREATE-DMD-DMO  THRU  1000-EXIT
01503              ELSE
01504                  ADD +1 TO WS-DMD-DMO-COUNT
01505                  ADD AT-AMOUNT-PAID TO WS-DMD-DMO-AMOUNT
01506          ELSE
01507              ADD +1 TO WS-DMD-PYMT-COUNT
01508              ADD AT-AMOUNT-PAID TO WS-DMD-PYMT-AMOUNT.
01509
01510      IF PI-COMPANY-ID = 'DMD' AND
01511         EIBAID = DFHPF1
01512          PERFORM 2000-CREATE-DMD-NOTE  THRU  2099-EXIT
01513          IF AT-CLAIM-NO NOT = WS-PREV-CLAIM-NO
01514              PERFORM 2500-DELETE-DMD-NOTE  THRU  2599-EXIT.
01515
01516      IF EIBAID = DFHPF1
01517          
      * EXEC CICS REWRITE
01518 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
01519 *            FROM    (ACTIVITY-TRAILERS)
01520 *        END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005873' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01521
01522      GO TO 0250-MAIN-LOGIC.
01523
01524      EJECT
01525  0260-MAIN-LOGIC.
01526      IF AQ-PAYMENT-COUNTER NOT GREATER THAN ZERO
01527          MOVE SPACES             TO  AQ-PENDING-PAYMENT-FLAG.
01528
01529      MOVE +1750                  TO  AQ-LAST-UPDATED-BY.
01530
01531      IF WS-UPDATE-SW = ZERO
01532         
      * EXEC CICS UNLOCK
01533 *            DATASET  (WS-ACTIVITY-QUE-DSID)
01534 *       END-EXEC
      *    MOVE '&*                    #   #00005888' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01535         GO TO 0205-STARTBR-ELACTQ.
01536
01537      IF EIBAID = DFHPF1
01538          IF AQ-PENDING-ACTIVITY-FLAGS = SPACES
01539              
      * EXEC CICS DELETE
01540 *                DATASET (WS-ACTIVITY-QUE-DSID)
01541 *            END-EXEC
      *    MOVE '&(                    &   #00005895' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01542           ELSE
01543              
      * EXEC CICS REWRITE
01544 *                DATASET (WS-ACTIVITY-QUE-DSID)
01545 *                FROM    (ACTIVITY-QUE)
01546 *            END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005899' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01547
01548      GO TO 0205-STARTBR-ELACTQ.
01549
01550      EJECT
01551  0300-END-OF-SEARCH.
01552      IF WS-ELTRLR-BROWSE-SW = +1
01553          
      * EXEC CICS ENDBR
01554 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
01555 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005909' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01556
01557      IF WS-ELACTQ-BROWSE-SW = +1
01558          
      * EXEC CICS ENDBR
01559 *            DATASET (WS-ACTIVITY-QUE-DSID)
01560 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005914' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01561
01562      IF PI-COMPANY-ID = 'DMD'
01563          MOVE WS-CHECK-QUE-COUNTER
01564                                  TO  WS-DMD-TL1-CONTROL-GROUP
01565                                      PI-CK-CONTROL-NO
01566                                      PI-END-CONTROL-NO
01567          MOVE WS-RELEASED-COUNT  TO  WS-DMD-TL1-COUNT
01568          MOVE WS-RELEASED-AMOUNT TO  WS-DMD-TL1-AMOUNT
01569          MOVE WS-DMD-PYMT-COUNT  TO  WS-DMD-TL2-COUNT1
01570          MOVE WS-DMD-PYMT-AMOUNT TO  WS-DMD-TL2-AMOUNT1
01571          MOVE WS-DMD-DMO-COUNT   TO  WS-DMD-TL2-COUNT2
01572          MOVE WS-DMD-DMO-AMOUNT  TO  WS-DMD-TL2-AMOUNT2
01573          IF WS-DMD-PYMT-COUNT GREATER +1
01574              MOVE 'S'            TO  WS-DMD-TL2-PLURAL1
01575          END-IF
01576          IF WS-DMD-DMO-COUNT NOT = +1
01577              MOVE 'S'            TO  WS-DMD-TL2-PLURAL2
01578          END-IF
01579          GO TO 0301-MAIN-LOGIC.
01580
01581      IF (PI-COMPANY-ID = 'AIG' OR 'AUK')
01582                    AND
01583          AOPTIONI = '1'
01584              NEXT SENTENCE
01585          ELSE
01586              MOVE WS-CHECK-QUE-COUNTER   TO  WS-TL1-CONTROL-GROUP
01587                                              PI-CK-CONTROL-NO
01588                                              PI-END-CONTROL-NO
01589              MOVE WS-RELEASED-COUNT      TO  WS-TL1-COUNT
01590              MOVE WS-RELEASED-AMOUNT     TO  WS-TL1-AMOUNT
031808             MOVE WS-UNAPPROVED-COUNT    TO  WS-TL3-COUNT
031808             MOVE WS-UNAPPROVED-AMOUNT   TO  WS-TL3-AMOUNT
01591              GO TO 0301-MAIN-LOGIC.
01592
01593      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01594          MOVE WS-CHECK-QUE-COUNTER   TO  WS-TL1-3-CONTROL-GROUP
01595                                          PI-CK-CONTROL-NO
01596          MOVE WS-RELEASED-COUNT      TO  WS-TL1-3-COUNT
01597          MOVE WS-RELEASED-AMOUNT     TO  WS-TL1-3-AMOUNT
01598          MOVE WS-CHECK-QUE-COUNTER-2 TO  WS-TL2-3-CONTROL-GROUP
01599          MOVE WS-RELEASED-COUNT-2    TO  WS-TL2-3-COUNT
01600          MOVE WS-RELEASED-AMOUNT-2   TO  WS-TL2-3-AMOUNT
01601          MOVE WS-CHECK-QUE-COUNTER-3 TO  WS-TL3-3-CONTROL-GROUP
01602                                          PI-END-CONTROL-NO
01603          MOVE WS-RELEASED-COUNT-3    TO  WS-TL3-3-COUNT
01604          MOVE WS-RELEASED-AMOUNT-3   TO  WS-TL3-3-AMOUNT.
01605
01606  0301-MAIN-LOGIC.
01607
01608      MOVE WS-NON-CASH-REL-CNT        TO  PI-NON-CASH-REL-CNT.
01609      MOVE WS-NON-CASH-REL-AMT        TO  PI-NON-CASH-REL-AMT.
01610
01611      IF PI-COMPANY-ID = 'DMD' AND
01612         EIBAID = DFHPF1
01613          PERFORM 2500-DELETE-DMD-NOTE  THRU  2599-EXIT.
01614
01615      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01616          GO TO 0302-MAIN-LOGIC.
01617
01618      IF WS-RELEASED-COUNT GREATER 1
01619          MOVE 'S'                TO  WS-TL1-PLURAL
01620       ELSE
01621          MOVE ' '                TO  WS-TL1-PLURAL.
031808
031808     IF WS-UNAPPROVED-COUNT NOT EQUAL 1
031808         MOVE 'S'                TO  WS-TL3-PLURAL
031808     ELSE
031808         MOVE ' '                TO  WS-TL3-PLURAL
031808     END-IF.
01622
01623      IF EIBAID = DFHPF1
01624          MOVE LOW-VALUES         TO  EL175AI
01625              IF WS-RELEASED-COUNT > +0
031808               OR WS-UNAPPROVED-COUNT > +0
01626                  IF PI-COMPANY-ID = 'DMD'
01627                      MOVE ' RELEASED FOR '
01628                                  TO  WS-DMD-TL1-RELEASE
01629                      MOVE WS-DMD-TOTAL-LINE1
01630                                  TO  EMI-MESSAGE-AREA (1)
01631                      MOVE WS-DMD-TOTAL-LINE2
01632                                  TO  EMI-MESSAGE-AREA (2)
01633                      GO TO 0301-RELEASE-CHECKS
01634                  ELSE
01635                      MOVE 'RELEASED'
01636                                  TO  WS-TL1-RELEASE
01637                      MOVE WS-TOTAL-LINE1
01638                                  TO  EMI-MESSAGE-AREA (1)
01639                      MOVE WS-TOTAL-LINE2
01640                                  TO  EMI-MESSAGE-AREA (2)
031808                     MOVE WS-TOTAL-LINE3
031808                                 TO  EMI-MESSAGE-AREA (3)
01641                      GO TO 0301-RELEASE-CHECKS
01642              ELSE
01643                  IF WS-NON-CASH-REL-CNT > +0
01644                      MOVE WS-NON-CASH-REL-CNT
01645                                  TO  WS-NC-TL1-COUNT
01646                      MOVE ' NON CASH CHECKS RELEASED'
01647                                  TO  WS-NC-TL1-LIT
01648                      MOVE WS-NON-CASH-TOTAL-LINE
01649                                  TO  EMI-MESSAGE-AREA (1)
01650                      GO TO 0301-RELEASE-CHECKS.
01651
01652      IF WS-RELEASED-COUNT > +0
031808       OR WS-UNAPPROVED-COUNT > +0
01653          IF PI-COMPANY-ID = 'DMD'
01654              MOVE ' TO BE RELEASED FOR'
01655                                  TO  WS-DMD-TL1-RELEASE
01656              MOVE WS-DMD-TOTAL-LINE1
01657                                  TO  EMI-MESSAGE-AREA (1)
01658              MOVE WS-DMD-TOTAL-LINE2
01659                                  TO  EMI-MESSAGE-AREA (2)
01660              GO TO 0301-RELEASE-CHECKS
01661          ELSE
01662              MOVE 'TO BE RELEASED'
01663                                  TO  WS-TL1-RELEASE
01664              MOVE WS-TOTAL-LINE1 TO  EMI-MESSAGE-AREA (1)
01665              MOVE WS-TOTAL-LINE2 TO  EMI-MESSAGE-AREA (2)
031808             MOVE WS-TOTAL-LINE3 TO  EMI-MESSAGE-AREA (3)
01666              GO TO 0301-RELEASE-CHECKS.
01667
01668      IF WS-NON-CASH-REL-CNT > ZERO
01669          MOVE WS-NON-CASH-REL-CNT        TO  WS-NC-TL1-COUNT
01670          MOVE ' NON CASH CHECKS TO BE RELEASED'
01671                                          TO  WS-NC-TL1-LIT
01672          MOVE WS-NON-CASH-TOTAL-LINE     TO  EMI-MESSAGE-AREA (1)
01673          GO TO 0301-RELEASE-CHECKS.
01674
01675      MOVE +0                             TO  PI-PROC-SW.
01676      MOVE -1                             TO  AOPTIONL.
01677      MOVE ER-3048                        TO  EMI-ERROR.
01678      GO TO 8200-SEND-DATAONLY.
01679
01680  0301-RELEASE-CHECKS.
01681
01682      IF EIBAID = DFHPF1
111502*        PERFORM 7000-PRINT-CHECKS-WAITING
01684          GO TO 0015-INITIALIZE
01685        ELSE
031808        IF WS-RELEASED-COUNT > +0
01686          MOVE +1                 TO PI-PROC-SW
031808        ELSE
031808         MOVE +0                 TO PI-PROC-SW
031808        END-IF
090309        IF WS-STOP-ALL-CHECKS
090309         MOVE +2                 TO PI-PROC-SW
090309        END-IF
01687          MOVE -1                 TO AOPTIONL
01688          GO TO 8200-SEND-DATAONLY.
01689
01690  0302-MAIN-LOGIC.
01691
01692      IF EIBAID = DFHPF1
01693        MOVE LOW-VALUES                   TO  EL175AI
01694        IF WS-RELEASED-COUNT   > +0 OR
01695           WS-RELEASED-COUNT-2 > +0 OR
01696           WS-RELEASED-COUNT-3 > +0
01697            MOVE 'RELEASED'               TO  WS-TL-1-3-RELEASE
01698            MOVE WS-TOTAL-LINE-1-3        TO  EMI-MESSAGE-AREA (1)
01699            MOVE WS-TOTAL-LINE-2-3        TO  EMI-MESSAGE-AREA (2)
01700            GO TO 0302-RELEASE-CHECKS
01701        ELSE
01702          IF WS-NON-CASH-REL-CNT > +0
01703            MOVE WS-NON-CASH-REL-CNT      TO  WS-NC-TL1-COUNT
01704            MOVE ' NON CASH CHECKS RELEASED'  TO  WS-NC-TL1-LIT
01705            MOVE WS-NON-CASH-TOTAL-LINE   TO  EMI-MESSAGE-AREA (1)
01706            GO TO 0302-RELEASE-CHECKS.
01707
01708      IF WS-RELEASED-COUNT   > +0 OR
01709         WS-RELEASED-COUNT-2 > +0 OR
01710         WS-RELEASED-COUNT-3 > +0
01711          MOVE 'TO BE RELEASED'           TO  WS-TL-1-3-RELEASE
01712          MOVE WS-TOTAL-LINE-1-3          TO  EMI-MESSAGE-AREA (1)
01713          MOVE WS-TOTAL-LINE-2-3          TO  EMI-MESSAGE-AREA (2)
01714          GO TO 0302-RELEASE-CHECKS.
01715
01716      IF WS-NON-CASH-REL-CNT > +0
01717          MOVE WS-NON-CASH-REL-CNT        TO  WS-NC-TL1-COUNT
01718          MOVE ' NON CASH CHECKS TO BE RELEASED '
01719                                          TO  WS-NC-TL1-LIT
01720          MOVE WS-NON-CASH-TOTAL-LINE     TO  EMI-MESSAGE-AREA (1)
01721          GO TO 0302-RELEASE-CHECKS.
01722
01723      MOVE +0                             TO  PI-PROC-SW.
01724      MOVE -1                             TO  AOPTIONL.
01725      MOVE ER-3048                        TO  EMI-ERROR.
01726      GO TO 8200-SEND-DATAONLY.
01727
01728  0302-RELEASE-CHECKS.
01729
01730      IF EIBAID = DFHPF1
111502*        PERFORM 7000-PRINT-CHECKS-WAITING
01732          GO TO 0015-INITIALIZE
01733      ELSE
01734          MOVE +1                     TO  PI-PROC-SW
01735          MOVE -1                     TO  AOPTIONL
01736          GO TO 8200-SEND-DATAONLY.
01737
01738      EJECT
01739  0910-ENQ-BUSY.
01740 *    NOTE *******************************************************
01741 *         *      IF ONE OF THE OTHER PROGRAMS (EL176 OR EL177)  *
01742 *         *  HAS EXCLUSIVE CONTROL OF THE CHECK QUEUE DSID,     *
01743 *         *  SEND A MESSAGE TO THE OPERATOR TO WAIT A FEW       *
01744 *         *  MOMENTS AND TRY AGAIN.                             *
01745 *         *******************************************************.
01746
01747      MOVE ER-0395                TO  EMI-ERROR.
01748      MOVE -1                     TO  AOPTIONL.
01749      GO TO 8200-SEND-DATAONLY.
01750
CIDMOD     EJECT
CIDMOD 0920-WRITE-DLYACTV.
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
CIDMOD     MOVE AT-CONTROL-PRIMARY     TO DA-KEY
CIDMOD     MOVE AT-SEQUENCE-NO         TO DA-TRAILER-SEQ-NO
PEMMOD     MOVE 'P'                    TO DA-RECORD-TYPE
CIDMOD
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*         NOTOPEN(0920-NOTOPEN)
CIDMOD*         DUPREC(0920-EXIT)
CIDMOD*    END-EXEC.
      *    MOVE '"$J%                  ! & #00006134' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036313334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     
      * EXEC CICS WRITE
CIDMOD*         DATASET('DLYACTV')
CIDMOD*         RIDFLD(DA-KEY)
CIDMOD*         FROM(DAILY-ACTIVITY-RECORD)
CIDMOD*         LENGTH(25)
CIDMOD*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006139' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD     GO TO 0920-EXIT.
CIDMOD
CIDMOD 0920-NOTOPEN.
CIDMOD     MOVE '2955' TO  EMI-ERROR
CIDMOD     GO TO 8200-SEND-DATAONLY.
CIDMOD
CIDMOD 0920-EXIT.
CIDMOD     EXIT.
CIDMOD
01751      EJECT
01752 ******************************************************************
01753 *    DMD ONLY  -  READ ELMSTR IF NOT AVAILABLE
01754 ******************************************************************
01755  0900-GET-MASTER.
01756
01757 *    IF AT-CARRIER  = CL-CARRIER  AND
01758 *       AT-CLAIM-NO = CL-CLAIM-NO
01759 *        GO TO 0900-EXIT.
01760
01761      MOVE AT-CONTROL-PRIMARY     TO WS-ELMSTR-KEY.
01762
01763      
      * EXEC CICS READ
01764 *         DATASET   ('ELMSTR')
01765 *         RIDFLD    (WS-ELMSTR-KEY)
01766 *         SET       (ADDRESS OF CLAIM-MASTER)
01767 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00006166' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01768
01769  0900-EXIT.
01770      EXIT.
01771                                  EJECT
01772 ******************************************************************
01773 *    DMD ONLY  -  CREATE THE DIRECT MARKETING OUTPUT FILE
01774 ******************************************************************
01775  1000-CREATE-DMD-DMO.
01776
01777      MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.
01778      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.
01779      MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.
01780
01781      
      * EXEC CICS HANDLE CONDITION
01782 *         NOTFND   (1000-NOTE-NOT-FOUND)
01783 *         NOTOPEN  (1000-NOTE-NOT-OPEN)
01784 *    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00006184' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01785
01786      
      * EXEC CICS READ
01787 *         DATASET('ERNOTE')
01788 *         SET    (ADDRESS OF CERTIFICATE-NOTE)
01789 *         RIDFLD (W-NOTE-KEY)
01790 *    END-EXEC.
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00006189' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01791
01792      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
01793      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.
01794      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.
01795
01796      IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES
01797          MOVE 'CC'               TO DCT-PRODUCT-CODE
01798      ELSE
01799          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.
01800
01801      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
01802      MOVE '01'                   TO DCT-COLUMN-ID-REQUESTED.
01803
01804      
      * EXEC CICS LINK
01805 *        PROGRAM    ('DLO006')
01806 *        COMMAREA   (DCT-COMMUNICATION-AREA)
01807 *        LENGTH     (WS-DCT-LENGTH)
01808 *    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00006207' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01809
01810      IF DCT-RETURN-CODE = 'OK'
01811          GO TO 1000-CONT.
01812
01813      IF DCT-RETURN-CODE = '01'
01814          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006217' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01815          MOVE ER-0910            TO EMI-ERROR
01816          MOVE -1                 TO AOPTIONL
01817          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01818          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01819          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01820          GO TO 8200-SEND-DATAONLY.
01821
01822      IF DCT-RETURN-CODE = '02'
01823          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006226' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01824          MOVE ER-0913            TO EMI-ERROR
01825          MOVE -1                 TO AOPTIONL
01826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01827          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01828          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01829          GO TO 8200-SEND-DATAONLY.
01830
01831      IF DCT-RETURN-CODE = '03'
01832          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006235' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01833          MOVE ER-0951            TO EMI-ERROR
01834          MOVE -1                 TO AOPTIONL
01835          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01836          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01837          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01838          GO TO 8200-SEND-DATAONLY.
01839
01840      IF DCT-RETURN-CODE = '04'
01841          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006244' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01842          MOVE ER-0946            TO EMI-ERROR
01843          MOVE -1                 TO AOPTIONL
01844          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01845          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01846          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01847          GO TO 8200-SEND-DATAONLY.
01848
01849      IF DCT-RETURN-CODE = '05'
01850          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006253' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01851          MOVE ER-0947            TO EMI-ERROR
01852          MOVE -1                 TO AOPTIONL
01853          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01854          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01855          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01856          GO TO 8200-SEND-DATAONLY.
01857
01858      IF DCT-RETURN-CODE = '06'
01859          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006262' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01860          MOVE ER-0921            TO EMI-ERROR
01861          MOVE -1                 TO AOPTIONL
01862          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01863          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01864          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01865          GO TO 8200-SEND-DATAONLY.
01866
01867      IF DCT-RETURN-CODE = '07'
01868          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006271' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01869          MOVE ER-0919            TO EMI-ERROR
01870          MOVE -1                 TO AOPTIONL
01871          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01872          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01873          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01874          GO TO 8200-SEND-DATAONLY.
01875
01876      IF DCT-RETURN-CODE = '08'
01877          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006280' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01878          MOVE ER-0948            TO EMI-ERROR
01879          MOVE -1                 TO AOPTIONL
01880          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01881          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01882          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01883          GO TO 8200-SEND-DATAONLY.
01884
01885      IF DCT-RETURN-CODE = 'N1'
01886          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006289' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01887          MOVE ER-0950            TO EMI-ERROR
01888          MOVE -1                 TO AOPTIONL
01889          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01890          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01891          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01892          GO TO 8200-SEND-DATAONLY.
01893
01894      IF DCT-RETURN-CODE = 'E1'
01895          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006298' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01896          MOVE ER-0974            TO EMI-ERROR
01897          MOVE -1                 TO AOPTIONL
01898          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01899          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01900          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01901          GO TO 8200-SEND-DATAONLY.
01902
01903      IF DCT-RETURN-CODE = 'E2'
01904          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006307' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01905          MOVE ER-0975            TO EMI-ERROR
01906          MOVE -1                 TO AOPTIONL
01907          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01908          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01909          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01910          GO TO 8200-SEND-DATAONLY.
01911
01912      IF DCT-RETURN-CODE NOT = 'OK'
01913          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006316' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01914          MOVE ER-0949            TO EMI-ERROR
01915          MOVE -1                 TO AOPTIONL
01916          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01917          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01918          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01919          GO TO 8200-SEND-DATAONLY.
01920
01921  1000-CONT.
01922
01923      MOVE 'Y'                    TO WS-DMD-TAPE-PYMT-SW.
01924
01925      ADD +1 TO WS-DMD-DMO-COUNT.
01926      ADD AT-AMOUNT-PAID TO WS-DMD-DMO-AMOUNT.
01927
01928      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01929      MOVE '5'                    TO  DC-OPTION-CODE.
01930      PERFORM 8500-DATE-CONVERSION.
01931      MOVE DC-GREG-DATE-1-YMD     TO DMD-YYMMDD.
01932      MOVE DC-BIN-DATE-1          TO AT-CHECK-WRITTEN-DT.
01933
01934      IF DMD-YY GREATER THAN 70
01935          MOVE '19'               TO DMD-DECADE
01936      ELSE
01937          MOVE '20'               TO DMD-DECADE.
01938
01939      MOVE +88888888              TO AT-CHECK-QUE-CONTROL.
01940      MOVE +8888                  TO AT-CHECK-QUE-SEQUENCE.
01941
01942      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
01943      MOVE 'DR'                   TO DM-RECORD-TYPE.
01944      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
01945      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
01946      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.
01947      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.
01948      MOVE CL-CARRIER             TO DM-CARRIER.
01949      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.
01950      MOVE DMD-DATE-YYYYMMDD      TO DM-PAYMENT-DATE.
01951
01952      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.
01953      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.
01954      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.
01955      PERFORM 1100-FORMAT-LAST-NAME-1ST THRU 1100-EXIT.
01956      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
01957
01958      MOVE AT-PAYMENT-TYPE        TO DM-PAYMENT-TYPE.
01959      MOVE AT-AMOUNT-PAID         TO DM-PAYMENT-AMT.
01960      MOVE AT-CERT-NO             TO DM-CERT-NO.
01961      MOVE AT-SEQUENCE-NO         TO DM-TRLR-SEQ-NO.
01962
01963      
      * EXEC CICS LINK
01964 *        PROGRAM    ('DLO025')
01965 *        COMMAREA   (DMO-COMMUNICATION-AREA)
01966 *        LENGTH     (WS-DMO-LENGTH)
01967 *    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00006366' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01968
01969      IF DM-RETURN-CODE = 'OK'
01970          GO TO 1000-EXIT.
01971
01972      IF DM-RETURN-CODE = '01'
01973          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006376' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01974          MOVE ER-8051            TO EMI-ERROR
01975          MOVE -1                 TO AOPTIONL
01976          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01977          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01978          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01979          GO TO 8200-SEND-DATAONLY.
01980
01981      IF DM-RETURN-CODE = '02'
01982          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006385' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01983          MOVE ER-8052            TO EMI-ERROR
01984          MOVE -1                 TO AOPTIONL
01985          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01986          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01987          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01988          GO TO 8200-SEND-DATAONLY.
01989
01990      IF DM-RETURN-CODE = '03'
01991          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006394' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01992          MOVE ER-8053            TO EMI-ERROR
01993          MOVE -1                 TO AOPTIONL
01994          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01995          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
01996          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
01997          GO TO 8200-SEND-DATAONLY.
01998
01999      IF DM-RETURN-CODE = '04'
02000          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006403' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02001          MOVE ER-8054            TO EMI-ERROR
02002          MOVE -1                 TO AOPTIONL
02003          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02004          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02005          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02006          GO TO 8200-SEND-DATAONLY.
02007
02008      IF DM-RETURN-CODE = '05'
02009          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006412' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02010          MOVE ER-8055            TO EMI-ERROR
02011          MOVE -1                 TO AOPTIONL
02012          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02013          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02014          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02015          GO TO 8200-SEND-DATAONLY.
02016
02017      IF DM-RETURN-CODE = '06'
02018          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006421' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02019          MOVE ER-8056            TO EMI-ERROR
02020          MOVE -1                 TO AOPTIONL
02021          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02022          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02023          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02024          GO TO 8200-SEND-DATAONLY.
02025
02026      IF DM-RETURN-CODE = '07'
02027          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006430' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02028          MOVE ER-8057            TO EMI-ERROR
02029          MOVE -1                 TO AOPTIONL
02030          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02031          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02032          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02033          GO TO 8200-SEND-DATAONLY.
02034
02035      IF DM-RETURN-CODE = '08'
02036          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006439' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02037          MOVE ER-8058            TO EMI-ERROR
02038          MOVE -1                 TO AOPTIONL
02039          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02040          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02041          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02042          GO TO 8200-SEND-DATAONLY.
02043
02044      IF DM-RETURN-CODE = '09'
02045          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006448' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02046          MOVE ER-8059            TO EMI-ERROR
02047          MOVE -1                 TO AOPTIONL
02048          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02049          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02050          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02051          GO TO 8200-SEND-DATAONLY.
02052
02053      IF DM-RETURN-CODE = '10'
02054          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006457' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02055          MOVE ER-8060            TO EMI-ERROR
02056          MOVE -1                 TO AOPTIONL
02057          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02058          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02059          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02060          GO TO 8200-SEND-DATAONLY.
02061
02062      IF DM-RETURN-CODE = '11'
02063          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006466' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02064          MOVE ER-8061            TO EMI-ERROR
02065          MOVE -1                 TO AOPTIONL
02066          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02067          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02068          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02069          GO TO 8200-SEND-DATAONLY.
02070
02071      IF DM-RETURN-CODE = '12'
02072          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006475' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02073          MOVE ER-8062            TO EMI-ERROR
02074          MOVE -1                 TO AOPTIONL
02075          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02076          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02077          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02078          GO TO 8200-SEND-DATAONLY.
02079
02080      IF DM-RETURN-CODE = '13'
02081          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006484' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02082          MOVE ER-8063            TO EMI-ERROR
02083          MOVE -1                 TO AOPTIONL
02084          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02085          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02086          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02087          GO TO 8200-SEND-DATAONLY.
02088
02089      IF DM-RETURN-CODE = '14'
02090          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006493' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02091          MOVE ER-8064            TO EMI-ERROR
02092          MOVE -1                 TO AOPTIONL
02093          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02094          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02095          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02096          GO TO 8200-SEND-DATAONLY.
02097
02098      IF DM-RETURN-CODE = '15'
02099          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006502' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02100          MOVE ER-8065            TO EMI-ERROR
02101          MOVE -1                 TO AOPTIONL
02102          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02103          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02104          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02105          GO TO 8200-SEND-DATAONLY.
02106
02107      IF DM-RETURN-CODE = '16'
02108          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006511' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02109          MOVE ER-8154            TO EMI-ERROR
02110          MOVE -1                 TO AOPTIONL
02111          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
02112          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02113          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02114          GO TO 8200-SEND-DATAONLY.
02115
02116      IF DM-RETURN-CODE = '17'
02117          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006520' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02118          MOVE ER-8155            TO EMI-ERROR
02119          MOVE -1                 TO AOPTIONL
02120          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
02121          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02122          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02123          GO TO 8200-SEND-DATAONLY.
02124
02125      IF DM-RETURN-CODE = 'N1'
02126          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006529' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02127          MOVE ER-8152            TO EMI-ERROR
02128          MOVE -1                 TO AOPTIONL
02129          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
02130          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02131          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02132          GO TO 8200-SEND-DATAONLY.
02133
02134      IF DM-RETURN-CODE = 'E1'
02135          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00006538' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02136          MOVE ER-8153            TO EMI-ERROR
02137          MOVE -1                 TO AOPTIONL
02138          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
02139          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT
02140          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT
02141          GO TO 8200-SEND-DATAONLY.
02142
02143      
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
      *    MOVE '6"R                   !   #00006546' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02144      MOVE ER-8066                TO EMI-ERROR.
02145      MOVE -1                     TO AOPTIONL.
02146      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02147      PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT.
02148      PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT.
02149      GO TO 8200-SEND-DATAONLY.
02150
02151  1000-NOTE-NOT-FOUND.
02152
02153      MOVE ER-0954                TO EMI-ERROR.
02154      MOVE -1                     TO AOPTIONL.
02155      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02156      PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT.
02157      PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT.
02158      GO TO 8200-SEND-DATAONLY.
02159
02160  1000-NOTE-NOT-OPEN.
02161
02162      MOVE ER-8135                TO  EMI-ERROR.
02163      MOVE -1                     TO  APFKL.
02164      GO TO 8200-SEND-DATAONLY.
02165
02166  1000-EXIT.
02167      EXIT.
02168
02169  1100-FORMAT-LAST-NAME-1ST.
02170 *****************************************************************
02171 *             M O V E   N A M E   R O U T I N E                 *
02172 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
02173 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
02174 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
02175 *                  FIELD                   VALUE                *
02176 *           W-NAME-LAST    (CL15)      SMITH                    *
02177 *           W-NAME-FIRST   (CL15)      JOHN                     *
02178 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
02179 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
02180 *     CONTAIN                                                   *
02181 *                SMITH, JOHN ALLEN                              *
02182 *     OR                                                        *
02183 *                SMITH, JOHN A.                                 *
02184 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
02185 *     COPYBOOK, ELCNWA.                                         *
02186 *****************************************************************.
02187
02188      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
02189      MOVE ZERO                   TO  WS-NAME-SW.
02190      SET NWA-INDEX               TO +1.
02191
02192      IF W-NAME-LAST = SPACES
02193              AND
02194         W-NAME-MIDDLE = SPACES
02195          MOVE +1                 TO WS-NAME-SW.
02196
02197      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
02198      PERFORM 1150-MOVE-NAME THRU 1150-EXIT.
02199
02200      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
02201      PERFORM 1150-MOVE-NAME THRU 1150-EXIT.
02202
02203      SET NWA-INDEX UP BY +1.
02204
02205      IF W-NAME-MIDDLE NOT = SPACES
02206          IF W-NAME-MIDDLE-2 = SPACES
02207              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
02208              SET NWA-INDEX UP BY +1
02209              MOVE '.'            TO WS-NW (NWA-INDEX)
02210              SET NWA-INDEX UP BY +2
02211          ELSE
02212              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
02213              PERFORM 1150-MOVE-NAME THRU 1150-EXIT.
02214
02215  1100-EXIT.
02216      EXIT.
02217                                  EJECT
02218  1150-MOVE-NAME.
02219
02220      IF WS-NAME-SW > +1
02221          GO TO 1150-EXIT.
02222
02223      IF WS-NAME-WORK2 = SPACES
02224          GO TO 1150-EXIT.
02225
02226      SET NWA-INDEX2            TO +1.
02227      SET NWA-INDEX3            TO +2.
02228
02229  1150-MOVE-NAME-CYCLE.
02230
02231      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
02232
02233      IF NWA-INDEX < +30
02234          SET NWA-INDEX UP BY +1
02235      ELSE
02236          ADD +2                TO  WS-NAME-SW
02237          GO TO 1150-EXIT.
02238
02239      IF NWA-INDEX2 < +20
02240          SET NWA-INDEX3 UP BY +1
02241          SET NWA-INDEX2 UP BY +1.
02242
02243      IF WS-NW2 (NWA-INDEX2) = SPACES
02244              AND
02245         WS-NW2 (NWA-INDEX3) = SPACES
02246          IF WS-NAME-SW = ZERO
02247              MOVE ','            TO  WS-NW (NWA-INDEX)
02248              SET NWA-INDEX UP BY +2
02249              MOVE +1             TO  WS-NAME-SW
02250              GO TO 1150-EXIT
02251          ELSE
02252              GO TO 1150-EXIT.
02253
02254      GO TO 1150-MOVE-NAME-CYCLE.
02255
02256  1150-EXIT.
02257      EXIT.
02258                                  EJECT
02259  1200-UNLOCK-CLAIM-MSTR.
02260       
      * EXEC CICS UNLOCK
02261 *          DATASET  (WS-ACTIVITY-QUE-DSID)
02262 *     END-EXEC.
      *    MOVE '&*                    #   #00006663' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-QUE-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02263
02264  1200-EXIT.
02265      EXIT.
02266
02267  1250-UNLOCK-TRLR.
02268       
      * EXEC CICS UNLOCK
02269 *          DATASET (WS-ACTIVITY-TRAILERS-DSID)
02270 *     END-EXEC.
      *    MOVE '&*                    #   #00006671' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02271
02272  1250-EXIT.
02273      EXIT.
02274                                  EJECT
02275 ******************************************************************
02276 *    DMD ONLY  -  UPDATE THE CLAIM EOB NOTES FILE - ELNOTE
02277 ******************************************************************
02278  2000-CREATE-DMD-NOTE.
02279
02280      MOVE 'N'                    TO  WS-HAVE-NOTE-HEADER.
02281      MOVE +0                     TO  WS-ELNOTE-BROWSE-SW.
02282
02283      MOVE LOW-VALUES             TO  WS-NOTE-KEY.
02284      MOVE AT-COMPANY-CD          TO  WS-EN-COMPANY-CD.
02285      MOVE AT-CARRIER             TO  WS-EN-CARRIER
02286      MOVE AT-CLAIM-NO            TO  WS-EN-CLAIM-NO.
02287      MOVE ZEROS                  TO  WS-EN-PAYMENT-SEQ-NO.
02288      MOVE '1'                    TO  WS-EN-RECORD-TYPE.
02289
02290      
      * EXEC CICS HANDLE CONDITION
02291 *        NOTFND  (2025-EOB-EXTRACT)
02292 *        ENDFILE (2025-EOB-EXTRACT)
02293 *        NOTOPEN (2050-NOT-OPEN)
02294 *    END-EXEC.
      *    MOVE '"$I''J                 ! ( #00006693' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02295
02296      
      * EXEC CICS STARTBR
02297 *        DATASET (WS-NOTE-DSID)
02298 *        RIDFLD  (WS-NOTE-KEY)
02299 *        GTEQ
02300 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006699' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 WS-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02301
02302      MOVE +1                     TO  WS-ELNOTE-BROWSE-SW.
02303
02304      
      * EXEC CICS READNEXT
02305 *        DATASET (WS-NOTE-DSID)
02306 *        RIDFLD  (WS-NOTE-KEY)
02307 *        SET     (ADDRESS OF CLAIM-EOB-NOTES)
02308 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006707' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-EOB-NOTES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02309
02310      IF EN-COMPANY-CD NOT = AT-COMPANY-CD
02311          GO TO 2025-EOB-EXTRACT.
02312
02313      IF (AT-CARRIER NOT = EN-CARRIER) OR
02314         (AT-CLAIM-NO NOT = EN-CLAIM-NO)
02315          GO TO 2025-EOB-EXTRACT.
02316
02317      IF EN-RECORD-TYPE NOT = '1'
02318          GO TO 2025-EOB-EXTRACT.
02319
02320      MOVE 'Y'                    TO  WS-HAVE-NOTE-HEADER.
02321      MOVE CLAIM-EOB-NOTES        TO  WS-SAVE-NOTE-RECORD.
02322
02323      
      * EXEC CICS ENDBR
02324 *        DATASET (WS-NOTE-DSID)
02325 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006726' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02326
02327      MOVE +0                     TO  WS-ELNOTE-BROWSE-SW.
02328
02329      
      * EXEC CICS GETMAIN
02330 *         SET      (ADDRESS OF CLAIM-EOB-NOTES)
02331 *         LENGTH   (EN-EOB-LENGTH)
02332 *    END-EXEC.
      *    MOVE '," L                  $   #00006732' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 EN-EOB-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-EOB-NOTES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02333
02334      MOVE WS-SAVE-NOTE-RECORD    TO  CLAIM-EOB-NOTES.
02335
02336      MOVE AT-CERT-NO             TO  EN-CERT-NO.
02337      MOVE AT-SEQUENCE-NO         TO  EN-PAYMENT-SEQ-NO.
02338      MOVE '2'                    TO  EN-RECORD-TYPE.
02339      MOVE +1750                  TO  EN-LAST-MAINT-BY.
02340      MOVE SAVE-BIN-DATE          TO  EN-LAST-MAINT-DT.
02341
02342      
      * EXEC CICS WRITE
02343 *        DATASET (WS-NOTE-DSID)
02344 *        FROM    (CLAIM-EOB-NOTES)
02345 *        RIDFLD  (EN-CONTROL-PRIMARY)
02346 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-EOB-NOTES
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006745' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 CLAIM-EOB-NOTES, 
                 DFHEIV11, 
                 EN-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02347
02348  2025-EOB-EXTRACT.
02349
02350      IF WS-ELNOTE-BROWSE-SW = +1
02351          
      * EXEC CICS ENDBR
02352 *            DATASET (WS-NOTE-DSID)
02353 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006754' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02354
02355      
      * EXEC CICS GETMAIN
02356 *         SET      (ADDRESS OF CLAIM-EOB-EXTRACT)
02357 *         LENGTH   (EB-EOB-LENGTH)
02358 *    END-EXEC.
      *    MOVE '," L                  $   #00006758' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 EB-EOB-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-EOB-EXTRACT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02359
02360      MOVE SPACES                 TO  CLAIM-EOB-EXTRACT.
02361      MOVE 'EB'                   TO  EB-RECORD-ID.
02362      MOVE AT-COMPANY-CD          TO  EB-COMPANY-CD.
02363      MOVE AT-CARRIER             TO  EB-CARRIER.
02364      MOVE AT-CLAIM-NO            TO  EB-CLAIM-NO.
02365      MOVE AT-CERT-NO             TO  EB-CERT-NUMBER.
02366      MOVE AT-SEQUENCE-NO         TO  EB-PMT-TRLR-SEQ-NO.
02367
02368      MOVE CL-INCURRED-DT         TO  DC-BIN-DATE-1.
02369      MOVE ' '                    TO  DC-OPTION-CODE.
02370      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.
02371
02372      IF DATE-CONVERSION-ERROR
02373          MOVE ZEROS              TO  EB-INCURRED-DT
02374      ELSE
02375          MOVE DC-GREG-DATE-1-MDY (1:2) TO DMD-MDY-MM
02376          MOVE DC-GREG-DATE-1-MDY (3:2) TO DMD-MDY-DD
02377          MOVE DC-GREG-DATE-1-MDY (5:2) TO DMD-MDY-YY
02378          IF DMD-MDY-YY < '70'
02379              MOVE '20'           TO  DMD-MDY-DECADE
02380          ELSE
02381              MOVE '19'           TO  DMD-MDY-DECADE
02382          END-IF
02383          MOVE DMD-DATE-MMDDYYYY  TO  EB-INCURRED-DT.
02384
02385      MOVE AT-PAID-FROM-DT        TO  DC-BIN-DATE-1.
02386      MOVE ' '                    TO  DC-OPTION-CODE.
02387      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.
02388
02389      IF DATE-CONVERSION-ERROR
02390          MOVE ZEROS              TO  EB-PAID-FROM
02391      ELSE
02392          MOVE DC-GREG-DATE-1-MDY (1:2) TO DMD-MDY-MM
02393          MOVE DC-GREG-DATE-1-MDY (3:2) TO DMD-MDY-DD
02394          MOVE DC-GREG-DATE-1-MDY (5:2) TO DMD-MDY-YY
02395          IF DMD-MDY-YY < '70'
02396              MOVE '20'           TO  DMD-MDY-DECADE
02397          ELSE
02398              MOVE '19'           TO  DMD-MDY-DECADE
02399          END-IF
02400          MOVE DMD-DATE-MMDDYYYY  TO  EB-PAID-FROM.
02401
02402      MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1.
02403      MOVE ' '                    TO  DC-OPTION-CODE.
02404      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.
02405
02406      IF DATE-CONVERSION-ERROR
02407          MOVE ZEROS              TO  EB-PAID-THRU
02408      ELSE
02409          MOVE DC-GREG-DATE-1-MDY (1:2) TO DMD-MDY-MM
02410          MOVE DC-GREG-DATE-1-MDY (3:2) TO DMD-MDY-DD
02411          MOVE DC-GREG-DATE-1-MDY (5:2) TO DMD-MDY-YY
02412          IF DMD-MDY-YY < '70'
02413              MOVE '20'           TO  DMD-MDY-DECADE
02414          ELSE
02415              MOVE '19'           TO  DMD-MDY-DECADE
02416          END-IF
02417          MOVE DMD-DATE-MMDDYYYY  TO  EB-PAID-THRU.
02418
02419      MOVE CL-INSURED-ADDR-CNT    TO  EB-PAYMENT-ADDR-SEQ.
02420      MOVE AT-PAYEES-NAME         TO  EB-PAYEE-NAME.
02421      MOVE AT-AMOUNT-PAID         TO  EB-PAID-AMOUNT.
02422      MOVE AT-PAYMENT-TYPE        TO  EB-PAYMENT-TYPE.
02423      MOVE AT-EOB-CODE1           TO  EB-EOB-CODE1.
02424      MOVE AT-EOB-CODE2           TO  EB-EOB-CODE2.
02425      MOVE AT-EOB-CODE3           TO  EB-EOB-CODE3.
02426      MOVE AT-EOB-CODE4           TO  EB-EOB-CODE4.
02427      MOVE AT-EOB-CODE5           TO  EB-EOB-CODE5.
02428
02429      IF HAVE-NOTE-HEADER
02430          MOVE EN-EOB-NOTE1       TO  EB-EOB-NOTE1
02431          MOVE EN-EOB-NOTE2       TO  EB-EOB-NOTE2
02432          MOVE EN-EOB-NOTE3       TO  EB-EOB-NOTE3
02433          MOVE EN-EOB-NOTE4       TO  EB-EOB-NOTE4.
02434
02435      
      * EXEC CICS WRITE
02436 *        DATASET (WS-EXTRACT-DSID)
02437 *        FROM    (CLAIM-EOB-EXTRACT)
02438 *        RIDFLD  (EB-CONTROL-PRIMARY)
02439 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-EOB-EXTRACT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006838' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-EXTRACT-DSID, 
                 CLAIM-EOB-EXTRACT, 
                 DFHEIV11, 
                 EB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02440
02441      GO TO 2099-EXIT.
02442
02443  2050-NOT-OPEN.
02444
02445      MOVE ER-8137                TO  EMI-ERROR.
02446      MOVE -1                     TO  APFKL.
02447      GO TO 8200-SEND-DATAONLY.
02448
02449  2099-EXIT.
02450      EXIT.
02451                                  EJECT
02452 ******************************************************************
02453 *    DMD ONLY  -  DELETE THE CLAIM EOB NOTES HEADER
02454 ******************************************************************
02455  2500-DELETE-DMD-NOTE.
02456
02457      IF WS-PREV-CLAIM-NO = LOW-VALUES
02458          GO TO 2550-SETUP-NEXT.
02459
02460      
      * EXEC CICS HANDLE CONDITION
02461 *        NOTFND  (2550-SETUP-NEXT)
02462 *        ENDFILE (2550-SETUP-NEXT)
02463 *        NOTOPEN (2050-NOT-OPEN)
02464 *    END-EXEC.
      *    MOVE '"$I''J                 ! ) #00006863' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036383633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02465
02466      MOVE WS-PREV-CLAIM          TO  WS-NOTE-KEY.
02467
02468      
      * EXEC CICS READ
02469 *        DATASET (WS-NOTE-DSID)
02470 *        RIDFLD  (WS-NOTE-KEY)
02471 *        SET     (ADDRESS OF CLAIM-EOB-NOTES)
02472 *        UPDATE
02473 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006871' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-EOB-NOTES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02474
02475      
      * EXEC CICS DELETE
02476 *        DATASET (WS-NOTE-DSID)
02477 *    END-EXEC.
      *    MOVE '&(                    &   #00006878' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02478
02479  2550-SETUP-NEXT.
02480
02481      MOVE AT-COMPANY-CD          TO  WS-PREV-COMPANY.
02482      MOVE AT-CARRIER             TO  WS-PREV-CARRIER.
02483      MOVE AT-CLAIM-NO            TO  WS-PREV-CLAIM-NO.
02484      MOVE LOW-VALUES             TO  WS-PREV-CERT-NO.
02485      MOVE ZEROS                  TO  WS-PREV-SEQ-NO.
02486      MOVE '1'                    TO  WS-PREV-RECORD-TYPE.
02487
02488  2599-EXIT.
02489      EXIT.
02490                                  EJECT
031808
031808******************************************************************
031808*    TOTAL THE UNAPPROVED PAYMENTS
031808******************************************************************
031808 3000-TOTAL-UNAPPROVED.
031808     MOVE AQ-PMT-UNAPPROVED-COUNT TO WS-PMT-UNAPPROVED-COUNT.
031808
031808     MOVE WS-ACTIVITY-QUE-KEY TO  WS-ACTIVITY-TRAILERS-KEY.
031808     MOVE ZERO                TO  WS-ATK-SEQUENCE-NO.
031808
031808     MOVE LOW-VALUES          TO  WS-LAST-ACTIVITY-TRAILERS-KEY.
031808
031808     
      * EXEC CICS HANDLE CONDITION
031808*        NOTFND  (3999-EXIT)
031808*        ENDFILE (3999-EXIT)
031808*    END-EXEC.
      *    MOVE '"$I''                  ! * #00006906' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036393036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031808
031808 3100-MAIN-LOGIC.
031808     IF WS-PMT-UNAPPROVED-COUNT NOT GREATER THAN ZERO
031808         GO TO 3300-END-BROWSE-ELTRLR.
031808
031808     IF WS-ELTRLR-BROWSE-SW = ZERO
031808         
      * EXEC CICS STARTBR
031808*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
031808*            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
031808*            EQUAL
031808*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00006916' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031808         MOVE +1               TO  WS-ELTRLR-BROWSE-SW
031808       ELSE
031808         
      * EXEC CICS RESETBR
031808*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
031808*            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
031808*            EQUAL
031808*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         E          &   #00006923' TO DFHEIV0
           MOVE X'263420202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031808
031808     EJECT
031808 3200-READNEXT-ELTRLR.
031808
031808     IF WS-PMT-UNAPPROVED-COUNT NOT GREATER THAN ZERO
031808         GO TO 3300-END-BROWSE-ELTRLR.
031808
031808     
      * EXEC CICS READNEXT
031808*        DATASET (WS-ACTIVITY-TRAILERS-DSID)
031808*        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
031808*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
031808*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006935' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031808
031808     IF WS-ACTIVITY-TRAILERS-KEY = WS-LAST-ACTIVITY-TRAILERS-KEY
031808         GO TO 3200-READNEXT-ELTRLR.
031808
031808     MOVE WS-ACTIVITY-TRAILERS-KEY  TO
031808                                  WS-LAST-ACTIVITY-TRAILERS-KEY
031808
031808     IF WS-AQK-COMPANY-CD NOT = WS-ATK-COMPANY-CD  OR
031808        WS-AQK-CARRIER    NOT = WS-ATK-CARRIER     OR
031808        WS-AQK-CLAIM-NO   NOT = WS-ATK-CLAIM-NO    OR
031808        WS-AQK-CERT-NO    NOT = WS-ATK-CERT-NO
031808         GO TO 3300-END-BROWSE-ELTRLR
031808     END-IF.
031808
031808     IF AT-TRAILER-TYPE NOT = '2'
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     IF AT-AMOUNT-PAID   NEGATIVE
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     IF AT-VOID-DT NOT = LOW-VALUES
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     IF AT-TO-BE-WRITTEN-DT GREATER THAN WS-CURRENT-DATE
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     IF AT-PAYMENT-APPROVAL-SW NOT = 'U'
031808           GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     SUBTRACT +1 FROM WS-PMT-UNAPPROVED-COUNT.
031808     ADD +1                      TO  WS-UNAPPROVED-COUNT.
031808     ADD AT-AMOUNT-PAID          TO  WS-UNAPPROVED-AMOUNT.
031808
031808     GO TO 3200-READNEXT-ELTRLR.
031808
031808 3300-END-BROWSE-ELTRLR.
031808
031808     IF WS-ELTRLR-BROWSE-SW > ZERO
031808         
      * EXEC CICS ENDBR
031808*            DATASET (WS-ACTIVITY-TRAILERS-DSID)
031808*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006987' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031808     END-IF.
031808
031808     MOVE ZERO                   TO  WS-ELTRLR-BROWSE-SW.
031808
031808
031808 3999-EXIT.
031808     EXIT.
031808                                EJECT
031808
111502*7000-PRINT-CHECKS-WAITING SECTION.
111502*
111502*    IF PI-COMPANY-ID = 'TIH' OR 'OFL' OR 'CGL' OR 'FGL' OR 'TII'
111502*       GO TO 7090-EXIT.
111502*
111502*    IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC'
02498 *        MOVE EIBTRMID       TO WS-FORMS-PRINTER
111502*        EXEC CICS START
111502*             TRANSID    ('EX64')
111502*             FROM       (PROGRAM-INTERFACE-BLOCK)
111502*             LENGTH     (PI-COMM-LENGTH)
02503 *             TERMID     (WS-FORMS-PRINTER)
111502*        END-EXEC
111502*    ELSE
111502*        EXEC CICS START
111502*             TRANSID    ('EX64')
111502*             FROM       (PROGRAM-INTERFACE-BLOCK)
111502*             LENGTH     (PI-COMM-LENGTH)
111502*             TERMID     (WS-FORMS-PRINTER)
111502*        END-EXEC.
02512
111502*    GO TO 7090-EXIT.
02514
02515  7010-TERMID-ERROR.
02516      MOVE ER-0412                TO EMI-ERROR.
02517      MOVE -1                     TO APFKL.
02518      GO TO 8200-SEND-DATAONLY.
02519
02520  7020-TRANS-ERROR.
02521      MOVE ER-0413                TO EMI-ERROR.
02522      MOVE -1                     TO APFKL.
02523      GO TO 8200-SEND-DATAONLY.
02524
111502*7090-EXIT.
111502*    EXIT.
02527
02528      EJECT
02529  8100-SEND-INITIAL-MAP SECTION.
02530
02531      MOVE EIBTIME                TO  TIME-IN.
02532
02533      MOVE SAVE-DATE              TO ADATEO.
02534      MOVE TIME-OUT               TO ATIMEO.
02535      MOVE PI-LIFE-OVERRIDE-L6    TO COVG1O.
02536      MOVE PI-AH-OVERRIDE-L6      TO COVG2O.
02537      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.
02538      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.
031808     MOVE EMI-MESSAGE-AREA (3)   TO AEMSG3O.
02539
02540      
      * EXEC CICS SEND
02541 *        FROM   (EL175AI)
02542 *        MAPSET (WS-MAPSET-NAME)
02543 *        MAP    (WS-MAP-NAME)
02544 *        CURSOR ERASE
02545 *    END-EXEC.
           MOVE LENGTH OF
            EL175AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007048' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL175AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02546
02547      PERFORM 9100-RETURN-TRAN.
02548
02549  8200-EXIT.
02550      EXIT.
02551
02552      EJECT
02553  8200-SEND-DATAONLY SECTION.
02554      IF EMI-ERROR = ZERO
090309         MOVE 'PRESS PF1 TO RELEASE CHECKS' TO APF1O
02555          MOVE AL-PABOF           TO APF1A
02556          MOVE AL-PADOF           TO ACOMPA
02557       ELSE
02558          PERFORM 9900-ERROR-FORMAT
02559          MOVE AL-PADOF           TO APF1A
02560          MOVE AL-PABOF           TO ACOMPA.
031808
031808     IF WS-RELEASED-COUNT = +0
031808         MOVE AL-PADOF           TO APF1A
031808         MOVE AL-PABOF           TO ACOMPA
031808     END-IF.
090309
090309     IF PI-ALL-CHKS-NOT-ALLOWED
090309         MOVE 'RELEASE ALL CHKS NOT ALLOWED' TO APF1O
090309         MOVE AL-PABOF           TO APF1A
090309         MOVE AL-PADOF           TO ACOMPA
090309     END-IF.
02561
02562      MOVE EIBTIME                TO  TIME-IN.
02563
02564      MOVE SAVE-DATE              TO  ADATEO.
02565      MOVE TIME-OUT               TO  ATIMEO.
02566      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.
02567      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.
031808     MOVE EMI-MESSAGE-AREA (3)   TO AEMSG3O.
02568
02569      
      * EXEC CICS SEND DATAONLY
02570 *        FROM   (EL175AI)
02571 *        MAPSET (WS-MAPSET-NAME)
02572 *        MAP    (WS-MAP-NAME)
02573 *        CURSOR
02574 *    END-EXEC.
           MOVE LENGTH OF
            EL175AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007090' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL175AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02575
02576      PERFORM 9100-RETURN-TRAN.
02577
02578  8100-EXIT.
02579      EXIT.
02580
02581      EJECT
02582  8300-SEND-TEXT SECTION.
02583      
      * EXEC CICS SEND TEXT
02584 *        FROM   (LOGOFF-TEXT)
02585 *        LENGTH (LOGOFF-LENGTH)
02586 *        ERASE  FREEKB END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007104' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02587
02588      
      * EXEC CICS RETURN
02589 *    END-EXEC.
      *    MOVE '.(                    ''   #00007109' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02590
02591  8300-EXIT.
02592      EXIT.
02593
02594      EJECT
02595  8500-DATE-CONVERSION SECTION.
02596      
      * EXEC CICS LINK
02597 *        PROGRAM  (ELDATCV)
02598 *        COMMAREA (DATE-CONVERSION-DATA)
02599 *        LENGTH   (DC-COMM-LENGTH)
02600 *    END-EXEC.
      *    MOVE '."C                   (   #00007117' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02601
02602  8500-EXIT.
02603      EXIT.
02604
02605      EJECT
02606  9000-RETURN-CICS SECTION.
02607      MOVE EL005                  TO  THIS-PGM.
02608      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02609      PERFORM 9300-XCTL.
02610
02611  9000-EXIT.
02612      EXIT.
02613
02614  9100-RETURN-TRAN SECTION.
02615      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02616      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02617
02618      
      * EXEC CICS RETURN
02619 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02620 *        LENGTH   (PI-COMM-LENGTH)
02621 *        TRANSID  (WS-TRANS-ID)
02622 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007139' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02623
02624  9100-EXIT.
02625      EXIT.
02626
02627  9300-XCTL SECTION.
02628      MOVE DFHENTER               TO  EIBAID.
02629
02630      
      * EXEC CICS XCTL
02631 *        PROGRAM  (THIS-PGM)
02632 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02633 *        LENGTH   (PI-COMM-LENGTH)
02634 *    END-EXEC.
      *    MOVE '.$C                   %   #00007151' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02635
02636  9300-EXIT.
02637      EXIT.
02638
02639      EJECT
02640  9400-CLEAR SECTION.
02641      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
02642      PERFORM 9300-XCTL.
02643
02644  9400-EXIT.
02645      EXIT.
02646
02647  9600-PGMIDERR SECTION.
02648      
      * EXEC CICS HANDLE CONDITION
02649 *        PGMIDERR (8300-SEND-TEXT)
02650 *    END-EXEC.
      *    MOVE '"$L                   ! + #00007169' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303037313639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02651
02652      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
02653
02654      MOVE EL005                  TO  THIS-PGM
02655                                      LOGOFF-PGM.
02656      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02657
02658      MOVE SPACES                 TO  PI-ENTRY-CD-1
02659      PERFORM 9300-XCTL.
02660
02661  9600-EXIT.
02662      EXIT.
02663
02664      EJECT
02665  9900-ERROR-FORMAT SECTION.
02666      
      * EXEC CICS LINK
02667 *        PROGRAM  (EL001)
02668 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
02669 *        LENGTH   (EMI-COMM-LENGTH)
02670 *    END-EXEC.
      *    MOVE '."C                   (   #00007187' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02671
02672  9900-EXIT.
02673      EXIT.
02674
02675      EJECT
02676  9990-ERROR SECTION.
02677      MOVE DFHEIBLK TO EMI-LINE1.
02678      
      * EXEC CICS LINK
02679 *        PROGRAM  (EL004)
02680 *        COMMAREA (EMI-LINE1)
02681 *        LENGTH   (72)
02682 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007199' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02683
02684      GO TO 8200-SEND-DATAONLY.
02685
02686  9990-EXIT.
02687      EXIT.
02688
02689  9995-SECURITY-VIOLATION.
02690 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00007228' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
02691
02692  9995-EXIT.
02693      EXIT.
02694


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL175' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     0140-MAIN-LOGIC,
                     0140-MAIN-LOGIC,
                     0910-ENQ-BUSY,
                     7010-TERMID-ERROR,
                     7020-TRANS-ERROR,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0300-END-OF-SEARCH,
                     0300-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0300-END-OF-SEARCH,
                     0300-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0260-MAIN-LOGIC,
                     0260-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0920-NOTOPEN,
                     0920-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1000-NOTE-NOT-FOUND,
                     1000-NOTE-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 2025-EOB-EXTRACT,
                     2025-EOB-EXTRACT,
                     2050-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2550-SETUP-NEXT,
                     2550-SETUP-NEXT,
                     2050-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3999-EXIT,
                     3999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL175' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
