00001  ID DIVISION.                                                     04/25/96
00002                                                                   EL143
00003  PROGRAM-ID.                 EL143.                                  LV018
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 02/13/96 09:51:40.                    CL*17
00007 *                            VMOD=2.018.                             CL*18
00008 *                                                                 EL143
00008 *                                                                 EL143
00009 *AUTHOR.     LOGIC,INC.                                              CL*17
00010 *            DALLAS, TEXAS.                                          CL*17
00011                                                                   EL143
00012 *DATE-COMPILED.                                                      CL*17
00013                                                                   EL143
00014 *SECURITY.   *****************************************************   CL*17
00015 *            *                                                   *   CL*17
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*17
00017 *            *                                                   *   CL*17
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*17
00019 *                                                                *   CL*17
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*17
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*17
00022 *            *                                                   *   CL*17
00023 *            *****************************************************   CL*17
00024                                                                   EL143
00025 *REMARKS.    TRANSACTION - EX30 - PAYMENT APPROVAL.                  CL**3
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00026                                                                   EL143
00027      EJECT                                                        EL143
00028  ENVIRONMENT DIVISION.                                            EL143
00029  DATA DIVISION.                                                   EL143
00030                                                                   EL143
00031  WORKING-STORAGE SECTION.                                         EL143
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL143
00033  77  FILLER  PIC X(32)  VALUE '*    EL143  WORKING STORAGE    *'. EL143
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.    CL*18
061013 77  S1                          PIC S999 COMP-3 VALUE +0.
061013 77  S2                          PIC S999 COMP-3 VALUE +0.
       77  ws-max-bens                 pic s999 comp-3 value +0.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
       77  ws-at-amount-paid           pic s9(7)v99 comp-4 value +0.
       77  ws-at-days-in-period        pic s9(5) comp-3 value +0.
       77  ws-at-payment-type          pic x          value ' '.
       77  ws-cm-ah-orig-term          pic s999 comp-3 value +0.
       77  ws-cm-ah-benefit-amt        pic s9(7)v99 comp-3 value +0.

00035                                                                   EL143
00036      EJECT                                                        EL143
00037  01  WS-DATE-AREA.                                                EL143
00038      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL143
00039      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    EL143
00040                                                                      CL*14
00041  01  WS-QID.                                                         CL*14
00042      05  WS-QID-TERM                 PIC X(4)    VALUE SPACES.       CL*14
00043      05  FILLER                      PIC X(4)    VALUE '143A'.       CL*14
00044                                                                   EL143
00045  01  STANDARD-AREAS.                                              EL143
061013     12  WS-RESPONSE         PIC S9(8)   COMP.
061013         88  RESP-NORMAL              VALUE +00.
061013         88  RESP-ERROR               VALUE +01.
061013         88  RESP-NOTFND              VALUE +13.
061013         88  RESP-DUPREC              VALUE +14.
061013         88  RESP-ENDFILE             VALUE +20.
00046      12  WS-BROWSE-SW                PIC X       VALUE SPACES.    EL143
00047      12  MAP-NAME                    PIC X(8)    VALUE 'EL143A  '.EL143
00048      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL143S  '.EL143
00049      12  SCREEN-NUMBER               PIC X(4)    VALUE '143A'.    EL143
00050      12  TRANS-ID                    PIC X(4)    VALUE 'EX30'.    EL143
00051      12  THIS-PGM                    PIC X(8)    VALUE 'EL143'.   EL143
00052      12  PGM-NAME                    PIC X(8).                    EL143
00053      12  TIME-IN                     PIC S9(7).                   EL143
00054      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL143
00055          16  FILLER                  PIC X.                       EL143
00056          16  TIME-OUT                PIC 99V99.                   EL143
00057          16  FILLER                  PIC XX.                      EL143
00058      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL143
00059      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL143
00060      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.   EL143
00061      12  XCTL-150                    PIC X(8)    VALUE 'EL150'.      CL*12
00062      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL143
00063      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL143
00064      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL143
00065      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.  EL143
00066      12  ELACTQ-FILE-ID              PIC X(8)    VALUE 'ELACTQ'.  EL143
00067      12  ELMSTR-FILE-ID              PIC X(8)    VALUE 'ELMSTR'.  EL143
00068      12  ELTRLR-FILE-ID              PIC X(8)    VALUE 'ELTRLR'.  EL143
00069      12  ELCERT-FILE-ID              PIC X(8)    VALUE 'ELCERT'.  EL143
00070      12  EMPLCY-FILE-ID              PIC X(8)    VALUE 'MPPLCY'.     CL*16
00071                                                                   EL143
00072      12  DEEDIT-FIELD                PIC X(15).                   EL143
00073      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     EL143
00074      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.  EL143
00075                                                                   EL143
00076      12  RETURN-FROM                 PIC X(8).                    EL143
00077                                                                   EL143
00078      12  WS-LF-COVERAGE-TYPE         PIC X(01)   VALUE SPACE.        CL**4
00079      12  WS-BEN-SEARCH-SW            PIC X(01)   VALUE 'N'.          CL**4
00080          88  BENEFIT-FOUND                       VALUE 'Y'.          CL**4
00081          88  NO-BENEFIT-FOUND                    VALUE 'N'.          CL**4
00082      12  WS-ACCESS.                                                  CL**4
00083          16  FILLER                  PIC X(02)   VALUE SPACES.       CL**4
00084          16  WS-BEN-CD               PIC X(02)   VALUE SPACES.       CL**4
00085      12  SUB                         PIC 9(01)   VALUE ZEROS.        CL**4
00086      12  SUB-1                       PIC S9(04)  VALUE +0  COMP.     CL*13
00087                                                                      CL*16
00088      12  WS-CV-PMT-CODE              PIC X(01)   VALUE ' '.          CL*16
CIDMOD     12  WS-BLANK                    PIC X       VALUE ' '.            000
00089                                                                      CL**4
00090      EJECT                                                        EL143
00091  01   ERROR-MESSAGES.                                             EL143
00092      12  ER-0000                     PIC  X(4)   VALUE '0000'.    EL143
00093      12  ER-0004                     PIC  X(4)   VALUE '0004'.    EL143
00094      12  ER-0005                     PIC  X(4)   VALUE '0005'.    EL143
00095      12  ER-0023                     PIC  X(4)   VALUE '0023'.    EL143
00096      12  ER-0029                     PIC  X(4)   VALUE '0029'.    EL143
00097      12  ER-0050                     PIC  X(4)   VALUE '0050'.    EL143
00098      12  ER-0068                     PIC  X(4)   VALUE '0068'.    EL143
00099      12  ER-0070                     PIC  X(4)   VALUE '0070'.    EL143
00100      12  ER-0138                     PIC  X(4)   VALUE '0138'.    EL143
00101      12  ER-0142                     PIC  X(4)   VALUE '0142'.    EL143
00102      12  ER-0282                     PIC  X(4)   VALUE '0282'.       CL**4
00103      12  ER-0303                     PIC  X(4)   VALUE '0303'.    EL143
00104      12  ER-0627                     PIC  X(4)   VALUE '0627'.    EL143
00105      12  ER-0628                     PIC  X(4)   VALUE '0628'.    EL143
00106      12  ER-0629                     PIC  X(4)   VALUE '0629'.       CL**2
00107      12  ER-2237                     PIC  X(4)   VALUE '2237'.    EL143
00108      12  ER-2238                     PIC  X(4)   VALUE '2238'.    EL143
00109      12  ER-2779                     PIC  X(4)   VALUE '2779'.       CL*12
00110      12  ER-3342                     PIC  X(4)   VALUE '3342'.       CL*14
00111      12  ER-3343                     PIC  X(4)   VALUE '3343'.       CL*14
00112      12  ER-7008                     PIC  X(4)   VALUE '7008'.    EL143
00113      12  ER-9211                     PIC  X(4)   VALUE '9211'.       CL*16
00114      12  ER-9819                     PIC  X(4)   VALUE '9819'.       CL*16
00115                                                                   EL143
00116      EJECT                                                        EL143
00117  01  MISC.                                                        EL143
00118      12  WS-HOLD-KEY.                                             EL143
00119          16  WS-HOLD-COMPANY-CD     PIC X.                        EL143
00120          16  WS-HOLD-CARR           PIC X.                        EL143
00121          16  WS-HOLD-CLAIM          PIC X(7).                     EL143
00122          16  WS-HOLD-CERT.                                        EL143
00123              20  WS-HOLD-CERT-PRIME PIC X(10).                    EL143
00124              20  WS-HOLD-CERT-SFX   PIC X.                        EL143
00125      12  WS-PAY-TYPE                PIC X.                        EL143
00126      12  WS-APPROVAL-LEVEL          PIC X.                           CL*12
00127      12  WS-PAYMENT-APPROVAL-SW     PIC X.                           CL*12
00128          88 WS-GRADUATED-APPROVAL          VALUE 'G'.                CL*12
00129      12  SC-ITEM                    PIC S9(4)    COMP   VALUE +1.    CL*12
00130      12  WS-AMOUNT-PAID             PIC S9(7)V99 COMP-3 VALUE +0. EL143
CIDMOD
CIDMOD 01  CSO-WORK-FIELDS.                                                  000
CIDMOD     05  ERROR-ON-OUTPUT-SW             PIC X        VALUE 'N'.
CIDMOD       88  ERROR-ON-OUTPUT                           VALUE 'Y'.
00131                                                                   EL143
00132  01  ACCESS-KEYS.                                                 EL143
00133      12  WS-HOLD-ELTRLR-KEY             PIC X(20).                EL143
00134      12  ELCNTL-KEY.                                              EL143
00135          16  ELCNTL-COMPANY-ID          PIC  X(3).                EL143
00136          16  ELCNTL-REC-TYPE            PIC  X.                   EL143
00137          16  ELCNTL-ACCESS              PIC  X(4).                EL143
00138          16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.        EL143
00139      12  ELMSTR-KEY.                                              EL143
00140          16  ELMSTR-COMPANY-CD          PIC X.                    EL143
00141          16  ELMSTR-CARRIER             PIC X.                    EL143
00142          16  ELMSTR-CLAIM-NO            PIC X(7).                 EL143
00143          16  ELMSTR-CERT-NO.                                      EL143
00144              20  ELMSTR-CERT-PRIME      PIC X(10).                EL143
00145              20  ELMSTR-CERT-SFX        PIC X.                    EL143
00146      12  ELTRLR-KEY.                                              EL143
00147          16  ELTRLR-COMPANY-CD          PIC X.                    EL143
00148          16  ELTRLR-CARRIER             PIC X.                    EL143
00149          16  ELTRLR-CLAIM-NO            PIC X(7).                 EL143
00150          16  ELTRLR-CERT-NO.                                      EL143
00151              20  ELTRLR-CERT-PRIME      PIC X(10).                EL143
00152              20  ELTRLR-CERT-SFX        PIC X.                    EL143
00153          16  ELTRLR-SEQ-NO              PIC S9(4)   COMP.         EL143
00154      12  ELACTQ-KEY.                                              EL143
00155          16  ELACTQ-COMPANY-CD          PIC X.                    EL143
00156          16  ELACTQ-CARRIER             PIC X.                    EL143
00157          16  ELACTQ-CLAIM-NO            PIC X(7).                 EL143
00158          16  ELACTQ-CERT-NO.                                      EL143
00159              20  ELACTQ-CERT-PRIME      PIC X(10).                EL143
00160              20  ELACTQ-CERT-SFX        PIC X.                    EL143
00161      12  ELCERT-KEY.                                              EL143
00162          16  ELCERT-COMPANY-CD          PIC X.                    EL143
00163          16  ELCERT-CARRIER             PIC X.                    EL143
00164          16  ELCERT-GROUP               PIC X(6).                 EL143
00165          16  ELCERT-STATE               PIC X(2).                 EL143
00166          16  ELCERT-ACCOUNT             PIC X(10).                EL143
00167          16  ELCERT-EFF-DATE            PIC X(2).                 EL143
00168          16  ELCERT-CERT-NO             PIC X(11).                EL143
00169      12  EMPLCY-KEY.                                                 CL*16
00170          16  EMPLCY-COMPANY-CD          PIC X(01).                   CL*16
00171          16  EMPLCY-CARRIER             PIC X(01).                   CL*16
00172          16  EMPLCY-GROUPING            PIC X(06).                   CL*16
00173          16  EMPLCY-STATE               PIC X(02).                   CL*16
00174          16  EMPLCY-PRODUCER            PIC X(10).                   CL*16
00175          16  EMPLCY-EFF-DATE            PIC X(02).                   CL*16
00176          16  EMPLCY-REFERENCE-NO        PIC X(20).                   CL*16
00177                                                                   EL143
061013 01  ELCRTT-KEY.                                              
061013     05  CTRLR-COMP-CD       PIC X.                               
061013     05  CTRLR-CARRIER       PIC X.                               
061013     05  CTRLR-GROUPING      PIC X(6).                            
061013     05  CTRLR-STATE         PIC X(2).                            
061013     05  CTRLR-ACCOUNT       PIC X(10).
061013     05  CTRLR-EFF-DT        PIC XX.                              
061013     05  CTRLR-CERT-NO       PIC X(11).  
061013     05  CTRLR-REC-TYPE      PIC X.

00178      EJECT                                                        EL143
00179      COPY ELCSCTM.                                                   CL*12
00180      EJECT                                                        EL143
00181      COPY ELCSCRTY.                                                  CL*12
00182      EJECT                                                        EL143
00183      COPY ELCDATE.                                                   CL*12
00184      EJECT                                                           CL*12
00185      COPY ELCLOGOF.                                                  CL*12
00186      EJECT                                                           CL*12
00187      COPY ELCATTR.                                                   CL*12
00188      EJECT                                                           CL*12
00189      COPY ELCEMIB.                                                   CL*12
00190      EJECT                                                           CL*16
00191      COPY MPCPOLUP.                                                  CL*16
00192      EJECT                                                           CL*12
00193      COPY ELCINTF.                                                   CL*12
00194      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL*12
00195          16  PI-LAST-ELACTQ-KEY      PIC X(20).                      CL*12
00196          16  PI-LAST-TRLR-SEQ-NO     PIC S9(4)   COMP.               CL*12
00197          16  PI-ELTRLR-UPDATE-BY     PIC X(4).                       CL*12
00198          16  PI-ELTRLR-UPDATE-HHMMSS PIC S9(6)   COMP-3.             CL*12
00199          16  PI-UNAPPROVED-COUNT     PIC S9      COMP-3.             CL*12
00200          16  PI-DIAGNOSIS            PIC X(25).                      CL*12
00201          16  PI-FIRST-TIME-SW        PIC X(01).                      CL*13
00202          16  PI-LAST-ELTRLR-KEY      PIC X(22).                      CL*13
00203          16  PI-PAY-TYPE             PIC X(01).                      CL*13
00204          16  FILLER                  PIC X(560).                     CL*17
00205                                                                   EL143
00206      EJECT                                                        EL143
00207      COPY ELCAID.                                                    CL*12
00208  01  FILLER    REDEFINES DFHAID.                                  EL143
00209      12  FILLER              PIC X(8).                            EL143
00210      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL143
00211                                                                   EL143
00212      EJECT                                                        EL143
00213      COPY EL143S.                                                    CL*12
00214                                                                   EL143
00215      EJECT                                                        EL143
00216  LINKAGE SECTION.                                                 EL143
00217  01  DFHCOMMAREA             PIC X(1024).                         EL143
00218                                                                   EL143
00219      EJECT                                                        EL143
00220      COPY ELCCNTL.                                                   CL*12
00221      EJECT                                                        EL143
00222      COPY ELCMSTR.                                                   CL*12
00223      EJECT                                                        EL143
00224      COPY ELCTRLR.                                                   CL*12
00225      EJECT                                                        EL143
00226      COPY ELCACTQ.                                                   CL*12
00227      EJECT                                                        EL143
00228      COPY ELCCERT.                                                   CL*12
061013                                     copy ELCCRTT.
CIDMOD/                                                                      000
CIDMOD     COPY ELCDAR.                                                      000
00229      EJECT                                                           CL*16
00230      COPY MPCPLCY.                                                   CL*16
00231                                                                      CL*13
00232      EJECT                                                        EL143
00233  PROCEDURE DIVISION.                                              EL143
00234                                                                   EL143
00235      MOVE EIBTRMID               TO WS-QID-TERM.                     CL*14
00236                                                                      CL*14
00237      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL143
00238      MOVE '5'                    TO DC-OPTION-CODE.               EL143
00239      PERFORM 9700-DATE-LINK.                                      EL143
00240      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL143
00241      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL143
00242                                                                   EL143
00243      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL143
00244      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL143
00245                                                                   EL143
00246      IF EIBCALEN EQUAL 0                                             CL**2
00247          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL143
00248                                                                   EL143
00249      IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM                        CL**2
00250          IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM                  CL**2
00251              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL143
00252              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL143
00253              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL143
00254              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL143
00255              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL143
00256              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL143
00257              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL143
00258              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL*12
00259          ELSE                                                     EL143
00260              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM             EL143
00261              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL143
00262              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL143
00263              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL143
00264              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL143
00265              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL143
00266              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL143
00267              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL143
00268              MOVE SPACES               TO PI-SAVED-PROGRAM-6         CL*14
00269              PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT        CL*14
00270              GO TO 3000-SHOW-CLAIM-PAYMENT.                          CL*14
00271                                                                   EL143
00272      EXEC CICS    HANDLE    CONDITION                             EL143
00273           PGMIDERR          (9600-PGMID-ERROR)                    EL143
00274           ERROR             (9990-ABEND)                          EL143
00275      END-EXEC.                                                    EL143
00276                                                                   EL143
00277  0100-SEND-NEW.                                                      CL*14
00278      IF  EIBTRNID NOT EQUAL TRANS-ID                                 CL**2
00279          MOVE LOW-VALUES         TO  EL143AI PI-LAST-ELACTQ-KEY      CL*14
00280          MOVE 'Y'                TO  PI-FIRST-TIME-SW                CL*13
00281          MOVE +0                 TO  PI-UNAPPROVED-COUNT             CL*13
00282                                      PI-LAST-TRLR-SEQ-NO             CL*13
00283                                      PI-ELTRLR-UPDATE-HHMMSS         CL*13
00284          MOVE SPACES             TO  PI-ELTRLR-UPDATE-BY             CL*13
00285                                      PI-DIAGNOSIS                    CL*13
00286          GO TO 8100-SEND-INITIAL-MAP.                             EL143
00287                                                                   EL143
00288      IF EIBAID EQUAL DFHCLEAR                                        CL**2
00289          GO TO 9400-CLEAR.                                        EL143
00290                                                                   EL143
00291      IF PI-PROCESSOR-ID EQUAL 'LGXX'                                 CL**2
00292          GO TO 0200-RECEIVE.                                      EL143
00293                                                                   EL143
00294      EXEC CICS  READQ TS                                          EL143
00295          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL143
00296          INTO    (SECURITY-CONTROL)                               EL143
00297          LENGTH  (SC-COMM-LENGTH)                                 EL143
00298          ITEM    (SC-ITEM)                                        EL143
00299      END-EXEC.                                                    EL143
00300                                                                   EL143
00301      MOVE SC-CLAIMS-DISPLAY (28)  TO  PI-DISPLAY-CAP.                CL*17
00302      MOVE SC-CLAIMS-UPDATE  (28)  TO  PI-MODIFY-CAP.                 CL*17
00303                                                                   EL143
00304      IF NOT DISPLAY-CAP                                           EL143
00305          MOVE 'READ'          TO SM-READ                          EL143
00306          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL143
00307          MOVE ER-0070        TO  EMI-ERROR                        EL143
00308          PERFORM 9900-ERROR-FORMAT                                EL143
00309          GO TO 8100-SEND-INITIAL-MAP.                             EL143
00310                                                                   EL143
00311  0200-RECEIVE.                                                    EL143
00312                                                                      CL**2
00313      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                      CL**2
00314         MOVE ER-7008            TO EMI-ERROR                      EL143
00315         PERFORM 9900-ERROR-FORMAT                                 EL143
00316         MOVE -1                 TO MAINTL                         EL143
00317         GO TO 8200-SEND-DATAONLY.                                 EL143
00318                                                                   EL143
00319      EXEC CICS RECEIVE                                            EL143
00320          MAP      (MAP-NAME)                                      EL143
00321          MAPSET   (MAPSET-NAME)                                   EL143
00322          INTO     (EL143AI)                                       EL143
00323      END-EXEC.                                                    EL143
00324                                                                   EL143
00325      IF  PFKEYL EQUAL +0                                             CL**2
00326          GO TO 0300-CHECK-PFKEYS.                                 EL143
00327                                                                   EL143
00328      IF  EIBAID NOT EQUAL DFHENTER                                   CL**2
00329          MOVE ER-0004            TO EMI-ERROR                     EL143
00330          GO TO 0320-INPUT-ERROR.                                  EL143
00331                                                                   EL143
00332      IF PFKEYI NOT NUMERIC                                        EL143
00333         MOVE ER-0029     TO EMI-ERROR                             EL143
00334         GO TO 0320-INPUT-ERROR.                                   EL143
00335                                                                   EL143
00336      IF PFKEYI GREATER THAN 01 AND                                EL143
00337                   LESS THAN 25                                       CL*12
00338         MOVE PF-VALUES (PFKEYI) TO EIBAID                         EL143
00339      ELSE                                                         EL143
00340         MOVE ER-0029        TO EMI-ERROR                          EL143
00341         GO TO 0320-INPUT-ERROR.                                   EL143
00342                                                                   EL143
00343  0300-CHECK-PFKEYS.                                               EL143
00344      IF EIBAID EQUAL DFHPF23                                         CL**2
00345          GO TO 8810-PF23.                                         EL143
00346                                                                   EL143
00347      IF EIBAID EQUAL DFHPF24                                         CL**2
00348          GO TO 9200-RETURN-MAIN-MENU.                             EL143
00349                                                                   EL143
00350      IF EIBAID EQUAL DFHPF12                                         CL**2
00351          GO TO 9500-PF12.                                         EL143
00352                                                                   EL143
00353      IF EIBAID = DFHPF6 AND                                          CL*14
00354          PI-LAST-ELACTQ-KEY  NOT EQUAL TO LOW-VALUES                 CL*14
00355          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT             CL*14
00356          MOVE XCTL-150               TO  PGM-NAME                    CL*13
00357          GO TO 9300-XCTL.                                            CL*13
00358                                                                      CL*13
00359      IF MAINTL GREATER THAN +0 AND                                EL143
00360               EIBAID NOT EQUAL DFHENTER                              CL**2
00361         MOVE -1             TO  MAINTL                            EL143
00362         MOVE  ER-0050       TO  EMI-ERROR                         EL143
00363         PERFORM 9900-ERROR-FORMAT                                 EL143
00364         GO TO 8200-SEND-DATAONLY.                                 EL143
00365                                                                      CL*13
00366      PERFORM 7600-READ-COMPANY-REC THRU 7600-EXIT.                   CL*13
00367      PERFORM 7700-READ-USER-REC    THRU 7700-EXIT.                   CL*13
00368                                                                   EL143
00369      IF  EIBAID EQUAL DFHPF1                                         CL**2
00370          GO TO 7000-BROWSE-FWRD-NEXT-CLAIM.                       EL143
00371                                                                   EL143
00372      IF  EIBAID EQUAL DFHPF2                                         CL**2
00373          GO TO 7100-BROWSE-BWRD-NEXT-CLAIM.                       EL143
00374                                                                   EL143
00375      IF  EIBAID EQUAL DFHPF3                                         CL**2
00376          GO TO 7200-BROWSE-FWRD-NEXT-PAYMENT.                     EL143
00377                                                                   EL143
00378      IF  EIBAID EQUAL DFHPF4                                         CL**2
00379          GO TO 7300-BROWSE-BWRD-NEXT-PAYMENT.                     EL143
00380                                                                      CL*12
00381      IF EIBAID = DFHPF5                                              CL*12
00382          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.                       CL*12
00383                                                                   EL143
00384      IF EIBAID EQUAL DFHENTER                                        CL**2
00385          GO TO 0400-EDIT-INPUT-DATA.                              EL143
00386                                                                   EL143
00387      MOVE ER-0029                TO EMI-ERROR.                    EL143
00388                                                                   EL143
00389  0320-INPUT-ERROR.                                                EL143
00390                                                                      CL**2
00391      PERFORM 9900-ERROR-FORMAT.                                   EL143
00392      MOVE AL-UNBON               TO PFKEYA.                       EL143
00393      IF PFKEYL EQUAL 0                                               CL**2
00394          MOVE -1                 TO MAINTL                        EL143
00395      ELSE                                                         EL143
00396          MOVE -1                 TO PFKEYL.                       EL143
00397                                                                   EL143
00398      GO TO 8200-SEND-DATAONLY.                                    EL143
00399                                                                   EL143
00400      EJECT                                                        EL143
00401  0400-EDIT-INPUT-DATA.                                            EL143
00402                                                                      CL**2
00403      IF MAINTI EQUAL 'S'                                             CL**2
00404         GO TO 3000-SHOW-CLAIM-PAYMENT.                            EL143
00405                                                                   EL143
00406      IF NOT  MODIFY-CAP                                           EL143
00407         MOVE 'UPDATE'           TO  SM-READ                       EL143
00408         PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT          EL143
00409         MOVE ER-0070            TO  EMI-ERROR                     EL143
00410         MOVE -1                 TO MAINTL                         EL143
00411         PERFORM 9900-ERROR-FORMAT                                 EL143
00412         GO TO 8100-SEND-INITIAL-MAP.                              EL143
00413                                                                   EL143
00414      IF MAINTI EQUAL 'A'                                             CL**2
00415         GO TO 1000-APPROVE-PAYMENT.                               EL143
00416                                                                   EL143
00417      IF MAINTI EQUAL 'V'                                             CL**2
00418         GO TO 2000-VOID-PAYMENT.                                  EL143
00419                                                                   EL143
00420      MOVE  ER-0023            TO EMI-ERROR                        EL143
00421      MOVE -1                  TO MAINTL                           EL143
00422      MOVE AL-UABON            TO MAINTA                           EL143
00423      PERFORM 9900-ERROR-FORMAT                                    EL143
00424      GO TO 8200-SEND-DATAONLY.                                    EL143
00425                                                                      CL*14
00426      EJECT                                                           CL*14
00427  0500-CREATE-TEMP-STORAGE.                                           CL*14
00428                                                                      CL*14
00429      EXEC CICS WRITEQ TS                                             CL*14
00430          QUEUE    (WS-QID)                                           CL*14
00431          FROM     (PROGRAM-INTERFACE-BLOCK)                          CL*14
00432          LENGTH   (PI-COMM-LENGTH)                                   CL*14
00433      END-EXEC.                                                       CL*14
00434                                                                      CL*14
00435  0599-EXIT.                                                          CL*14
00436       EXIT.                                                          CL*14
00437                                                                      CL*14
00438      EJECT                                                           CL*14
00439  0600-RECOVER-TEMP-STORAGE.                                          CL*14
00440                                                                      CL*14
00441      MOVE LOW-VALUES                   TO EL143AI.                   CL*17
00442      EXEC CICS HANDLE CONDITION                                      CL*14
00443          NOTFND  (0100-SEND-NEW)                                     CL*14
00444          QIDERR  (0100-SEND-NEW)                                     CL*14
00445      END-EXEC.                                                       CL*14
00446                                                                      CL*14
00447                                                                      CL*14
00448      EXEC CICS READQ TS                                              CL*14
00449           QUEUE    (WS-QID)                                          CL*14
00450           INTO    (PROGRAM-INTERFACE-BLOCK)                          CL*14
00451           LENGTH  (PI-COMM-LENGTH)                                   CL*14
00452          END-EXEC.                                                   CL*14
00453                                                                      CL*14
00454      EXEC CICS DELETEQ TS                                            CL*14
00455           QUEUE    (WS-QID)                                          CL*14
00456      END-EXEC.                                                       CL*14
00457                                                                      CL*17
00458      MOVE PI-LAST-ELTRLR-KEY     TO ELACTQ-KEY.                      CL*17
00459      MOVE ELACTQ-CLAIM-NO        TO CLAIMO.                          CL*17
00460      MOVE ELACTQ-CARRIER         TO CARRO.                           CL*17
00461      MOVE ELACTQ-CERT-PRIME      TO CERTO.                           CL*17
00462      MOVE ELACTQ-CERT-SFX        TO SUFFIXO.                         CL*17
00463                                                                      CL*14
00464  0699-EXIT.                                                          CL*14
00465      EXIT.                                                           CL*14
00466                                                                   EL143
00467      EJECT                                                        EL143
00468  1000-APPROVE-PAYMENT.                                            EL143
00469                                                                      CL**2
00470      MOVE CARRI         TO WS-HOLD-CARR.                          EL143
00471      MOVE CLAIMI        TO WS-HOLD-CLAIM.                         EL143
00472      MOVE CERTI         TO WS-HOLD-CERT-PRIME.                    EL143
00473      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.                      EL143
00474      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.                    EL143
00475                                                                   EL143
00476      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY                     CL**2
00477         MOVE ER-0138     TO EMI-ERROR                             EL143
00478         MOVE -1          TO MAINTL                                EL143
00479         MOVE AL-UNBON    TO MAINTA                                EL143
00480         PERFORM 9900-ERROR-FORMAT                                 EL143
00481         GO TO 8200-SEND-DATAONLY.                                 EL143
00482                                                                   EL143
00483      MOVE PI-LAST-ELACTQ-KEY    TO ELACTQ-KEY.                    EL143
00484                                                                   EL143
00485      EXEC CICS HANDLE CONDITION                                   EL143
00486           NOTFND    (1010-NOT-FOUND)                              EL143
00487           ENDFILE   (1010-NOT-FOUND)                              EL143
00488      END-EXEC.                                                    EL143
00489                                                                   EL143
00490      EXEC CICS READ                                               EL143
00491           DATASET    (ELACTQ-FILE-ID)                             EL143
00492           RIDFLD     (ELACTQ-KEY)                                 EL143
00493           SET        (ADDRESS OF ACTIVITY-QUE)                       CL*17
00494           UPDATE                                                  EL143
00495      END-EXEC.                                                    EL143
00496                                                                   EL143
00497      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC OR                    CL*13
00498         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0                       CL*13
00499          MOVE -1                     TO  MAINTL                      CL*13
00500          MOVE  ER-0627               TO  EMI-ERROR                   CL*13
00501          PERFORM 9900-ERROR-FORMAT                                   CL*13
00502          GO TO 8200-SEND-DATAONLY.                                   CL*13
00503                                                                   EL143
00504      MOVE ELACTQ-KEY          TO ELTRLR-KEY.                         CL*12
00505      MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.                   EL143
00506                                                                   EL143
00507      EXEC CICS READ                                               EL143
00508           DATASET   (ELTRLR-FILE-ID)                              EL143
00509           RIDFLD    (ELTRLR-KEY)                                  EL143
00510           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*17
00511           UPDATE                                                  EL143
00512      END-EXEC.                                                    EL143
00513                                                                   EL143
00514      IF AT-PAYMENT-LAST-UPDATED-BY EQUAL PI-ELTRLR-UPDATE-BY AND     CL**2
00515         AT-LAST-MAINT-HHMMSS EQUAL PI-ELTRLR-UPDATE-HHMMSS           CL**2
00516         NEXT SENTENCE                                             EL143
00517      ELSE                                                         EL143
00518         MOVE ER-0068        TO EMI-ERROR                          EL143
00519         MOVE AL-UABON       TO MAINTA                             EL143
00520         MOVE -1             TO MAINTL                             EL143
00521         PERFORM 9900-ERROR-FORMAT                                 EL143
00522         GO TO 8200-SEND-DATAONLY.                                 EL143
00523                                                                   EL143
00524      IF (AT-TRAILER-TYPE NOT EQUAL '2')                              CL**2
00525             OR                                                    EL143
00526         (AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U')                       CL**2
00527         GO TO 1010-NOT-FOUND.                                     EL143
00528                                                                      CL**2
00529      IF (AT-RECORDED-BY EQUAL PI-PROCESSOR-ID)                       CL**3
00530       AND                                                            CL**3
00531         (PI-PROCESSOR-ID NOT EQUAL 'LGXX')                           CL**3
00532         MOVE ER-0629       TO EMI-ERROR                              CL**2
00533         MOVE AL-UABON      TO MAINTA                                 CL**2
00534         MOVE -1            TO MAINTL                                 CL**2
00535         PERFORM 9900-ERROR-FORMAT                                    CL**2
00536         GO TO 8200-SEND-DATAONLY.                                    CL**2
00537                                                                   EL143
00538      IF NOT WS-GRADUATED-APPROVAL                                    CL*12
00539          GO TO 1005-APPROVE.                                         CL*12
00540                                                                      CL*12
031808* If user approval level > trailer, make equal.
031808* This eliminates the need for intermediate approvals.
031808     IF AT-APPROVED-LEVEL < WS-APPROVAL-LEVEL
031808        MOVE WS-APPROVAL-LEVEL TO AT-APPROVED-LEVEL
031808     END-IF.
031808
00541      IF AT-APPROVED-LEVEL = WS-APPROVAL-LEVEL                        CL*12
00542          NEXT SENTENCE                                               CL*12
00543        ELSE                                                          CL*12
00544          MOVE ER-2779       TO EMI-ERROR                             CL*12
00545          MOVE AL-UABON      TO MAINTA                                CL*12
00546          MOVE -1            TO MAINTL                                CL*12
00547          PERFORM 9900-ERROR-FORMAT                                   CL*12
00548          GO TO 8200-SEND-DATAONLY.                                   CL*12
00549                                                                      CL*12
00550      IF AT-APPROVED-LEVEL = '1'                                      CL*12
00551          MOVE '2'             TO AT-APPROVED-LEVEL                   CL*12
00552       ELSE                                                           CL*12
00553      IF AT-APPROVED-LEVEL = '2'                                      CL*12
00554          MOVE '3'             TO AT-APPROVED-LEVEL                   CL*12
00555       ELSE                                                           CL*12
00556      IF AT-APPROVED-LEVEL = '3'                                      CL*12
031808*         MOVE '4'             TO AT-APPROVED-LEVEL.                 CL*12
031808         MOVE '4'             TO AT-APPROVED-LEVEL 
031808      ELSE                     
031808     IF AT-APPROVED-LEVEL = '4'
091813         MOVE '5'             TO AT-APPROVED-LEVEL
091813      ELSE
091813     IF AT-APPROVED-LEVEL = '5'
091813         MOVE '6'             TO AT-APPROVED-LEVEL.
00558                                                                      CL**3
00559      IF AT-APPROVED-LEVEL GREATER AT-APPROVAL-LEVEL-REQD             CL*12
00560          GO TO 1005-APPROVE.                                         CL*12
00561                                                                      CL*12
00562      MOVE PI-PROCESSOR-ID  TO AT-PAYMENT-LAST-UPDATED-BY.            CL*12
00563      MOVE SAVE-BIN-DATE    TO AT-PAYMENT-LAST-MAINT-DT.              CL*12
00564      MOVE EIBTIME          TO AT-LAST-MAINT-HHMMSS.                  CL*12
00565                                                                      CL*12
00566      EXEC CICS REWRITE                                               CL*12
00567          DATASET (ELTRLR-FILE-ID)                                    CL*12
00568          FROM    (ACTIVITY-TRAILERS)                                 CL*12
00569      END-EXEC.                                                       CL*12
00570                                                                      CL*12
00571      MOVE ER-3342          TO EMI-ERROR                              CL*14
00572                                                                      CL*14
00573      GO TO 1006-INTERMEDIATE-APPROVAL.                               CL*12
00574                                                                      CL*12
00575  1005-APPROVE.                                                       CL*12
00576      MOVE 'A'              TO AT-PAYMENT-APPROVAL-SW.                CL*12
00577                                                                      CL*12
00578      MOVE PI-PROCESSOR-ID  TO AT-PAYMENT-LAST-UPDATED-BY.            CL*12
031808     MOVE PI-PROCESSOR-ID  TO AT-PMT-APPROVED-BY.
00579      MOVE SAVE-BIN-DATE    TO AT-PAYMENT-LAST-MAINT-DT.              CL*12
00580      MOVE EIBTIME          TO AT-LAST-MAINT-HHMMSS.                  CL*12
00581                                                                   EL143
00582      EXEC CICS REWRITE                                            EL143
00583           DATASET   (ELTRLR-FILE-ID)                              EL143
00584           FROM      (ACTIVITY-TRAILERS)                           EL143
00585      END-EXEC.                                                    EL143
00586                                                                      CL*12
00587      IF AQ-PMT-UNAPPROVED-COUNT GREATER THAN +0                      CL*12
00588         SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT.                    CL*12
00589                                                                   EL143
00590      EXEC CICS REWRITE                                            EL143
00591           DATASET   (ELACTQ-FILE-ID)                              EL143
00592           FROM      (ACTIVITY-QUE)                                EL143
00593      END-EXEC.                                                    EL143
00594                                                                   EL143
00595      SUBTRACT +1    FROM PI-UNAPPROVED-COUNT.                     EL143
00596      MOVE PI-UNAPPROVED-COUNT    TO UCOUNTO.                      EL143
00597      MOVE ER-3343                TO  EMI-ERROR.                      CL*16
00598                                                                      CL*12
00599  1006-INTERMEDIATE-APPROVAL.                                         CL*12
00600      MOVE -1                     TO MAINTL.                       EL143
00601      MOVE SPACES                 TO MAINTO.                          CL**8
00602      MOVE AL-UANOF               TO MAINTA.                          CL*10
00603      PERFORM 9900-ERROR-FORMAT                                    EL143
00604      GO TO 8200-SEND-DATAONLY.                                    EL143
00605                                                                   EL143
00606      EJECT                                                        EL143
00607  1010-NOT-FOUND.                                                  EL143
00608                                                                      CL**2
00609      MOVE  ER-0142               TO EMI-ERROR.                    EL143
00610      MOVE AL-UNBON               TO CLAIMA                        EL143
00611                                     CERTA                            CL*12
00612                                     CARRA.                           CL*12
00613      MOVE -1                     TO CARRL.                           CL*11
00614                                                                   EL143
00615      PERFORM 9900-ERROR-FORMAT.                                   EL143
00616      GO TO 8200-SEND-DATAONLY.                                    EL143
00617                                                                   EL143
00618      EJECT                                                           CL*13
00619  2000-VOID-PAYMENT.                                               EL143
00620                                                                      CL**2
00621      MOVE CARRI         TO WS-HOLD-CARR.                          EL143
00622      MOVE CLAIMI        TO WS-HOLD-CLAIM.                         EL143
00623      MOVE CERTI         TO WS-HOLD-CERT-PRIME.                    EL143
00624      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.                      EL143
00625      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.                    EL143
00626                                                                   EL143
00627      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY                     CL**2
00628         MOVE ER-0138     TO EMI-ERROR                             EL143
00629         MOVE -1          TO MAINTL                                EL143
00630         MOVE AL-UNBON    TO MAINTA                                EL143
00631         PERFORM 9900-ERROR-FORMAT                                 EL143
00632         GO TO 8200-SEND-DATAONLY.                                 EL143
00633                                                                   EL143
00634      MOVE PI-LAST-ELACTQ-KEY    TO ELACTQ-KEY.                    EL143
00635                                                                   EL143
00636      EXEC CICS HANDLE CONDITION                                   EL143
00637           NOTFND    (2095-NOT-FOUND)                              EL143
00638           ENDFILE   (2095-NOT-FOUND)                              EL143
00639      END-EXEC.                                                    EL143
00640                                                                   EL143
00641      EXEC CICS READ                                               EL143
00642           DATASET    (ELACTQ-FILE-ID)                             EL143
00643           RIDFLD     (ELACTQ-KEY)                                 EL143
00644           SET        (ADDRESS OF ACTIVITY-QUE)                       CL*17
00645      END-EXEC.                                                    EL143
00646                                                                   EL143
00647      MOVE AQ-CONTROL-PRIMARY   TO ELMSTR-KEY.                     EL143
00648                                                                   EL143
00649      EXEC CICS READ                                               EL143
00650           DATASET    (ELMSTR-FILE-ID)                             EL143
00651           RIDFLD     (ELMSTR-KEY)                                 EL143
00652           SET        (ADDRESS OF CLAIM-MASTER)                       CL*17
00653           UPDATE                                                  EL143
00654      END-EXEC.                                                    EL143
00655                                                                   EL143
00656      MOVE CL-CONTROL-PRIMARY   TO ELTRLR-KEY.                     EL143
00657      MOVE PI-LAST-TRLR-SEQ-NO  TO ELTRLR-SEQ-NO.                  EL143
00658                                                                   EL143
00659      EXEC CICS READ                                               EL143
00660           DATASET    (ELTRLR-FILE-ID)                             EL143
00661           RIDFLD     (ELTRLR-KEY)                                 EL143
00662           SET        (ADDRESS OF ACTIVITY-TRAILERS)                  CL*17
00663           UPDATE                                                  EL143
00664      END-EXEC.                                                    EL143
00665                                                                   EL143
00666      IF AT-PAYMENT-LAST-UPDATED-BY EQUAL PI-ELTRLR-UPDATE-BY AND     CL**2
00667         AT-LAST-MAINT-HHMMSS EQUAL PI-ELTRLR-UPDATE-HHMMSS           CL**2
00668         NEXT SENTENCE                                             EL143
00669      ELSE                                                         EL143
00670         MOVE ER-0068        TO EMI-ERROR                          EL143
00671         MOVE AL-UABON       TO MAINTA                             EL143
00672         MOVE -1             TO MAINTL                             EL143
00673         PERFORM 9900-ERROR-FORMAT                                 EL143
00674         GO TO 8200-SEND-DATAONLY.                                 EL143
00675                                                                   EL143
00676      IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'                         CL**2
00677         GO TO 2095-NOT-FOUND.                                     EL143
00678                                                                      CL**2
00679      IF (AT-RECORDED-BY EQUAL PI-PROCESSOR-ID)                       CL**3
00680       AND                                                            CL**3
00681         (PI-PROCESSOR-ID NOT EQUAL 'LGXX')                           CL**3
00682         MOVE ER-0629       TO EMI-ERROR                              CL**2
00683         MOVE AL-UABON      TO MAINTA                                 CL**2
00684         MOVE -1            TO MAINTL                                 CL**2
00685         PERFORM 9900-ERROR-FORMAT                                    CL**2
00686         GO TO 8200-SEND-DATAONLY.                                    CL**2
00687                                                                   EL143
00688      MOVE '7'                  TO PI-PAY-TYPE.                       CL*13
00689      MOVE AT-PAYMENT-TYPE      TO WS-PAY-TYPE.                    EL143
00690      MOVE AT-AMOUNT-PAID       TO WS-AMOUNT-PAID.                 EL143
00691      MOVE AT-CV-PMT-CODE       TO WS-CV-PMT-CODE.                    CL*16
00692                                                                   EL143
00693      IF (AT-PAYMENT-TYPE NOT EQUAL '5' AND '6')                      CL**2
00694          SUBTRACT AT-AMOUNT-PAID     FROM CL-TOTAL-PAID-AMT       EL143
00695          SUBTRACT AT-DAYS-IN-PERIOD  FROM CL-NO-OF-DAYS-PAID      EL143
00696          IF (AT-PAYMENT-TYPE NOT EQUAL '4')                          CL**2
00697             SUBTRACT +1                 FROM CL-NO-OF-PMTS-MADE   EL143
00698             IF AT-PAID-THRU-DT NOT EQUAL CL-PAID-THRU-DT OR          CL**2
00699                AT-RECORDED-BY EQUAL 'ZZZZ'                           CL**2
00700                NEXT SENTENCE                                      EL143
00701             ELSE                                                  EL143
00702                MOVE AT-PREV-LAST-PMT-DT    TO CL-LAST-PMT-DT      EL143
00703                MOVE AT-PREV-PAID-THRU-DT   TO CL-PAID-THRU-DT     EL143
00704                MOVE AT-PREV-LAST-PMT-AMT   TO CL-LAST-PMT-AMT.    EL143
00705                                                                   EL143
00706      IF CL-TOTAL-PAID-AMT LESS THAN +0                            EL143
00707         MOVE +0                  TO CL-TOTAL-PAID-AMT.            EL143
00708                                                                   EL143
00709      IF CL-NO-OF-DAYS-PAID LESS THAN +0                           EL143
00710         MOVE +0                  TO CL-NO-OF-DAYS-PAID.           EL143
00711                                                                   EL143
00712      IF CL-NO-OF-PMTS-MADE LESS THAN +0                           EL143
00713         MOVE +0                  TO CL-NO-OF-PMTS-MADE.           EL143
00714                                                                   EL143
00715      MOVE SAVE-BIN-DATE          TO AT-VOID-DT                       CL*13
00716                                     CL-LAST-REOPEN-DT.               CL*13
00717                                                                   EL143
00718      MOVE 'PAYMENT DISAPPROVED'  TO AT-VOID-REASON.               EL143
00719      MOVE 'V'                    TO AT-PAYMENT-APPROVAL-SW.       EL143
00720                                                                   EL143
00721      MOVE LOW-VALUES             TO AT-PMT-SELECT-DT                 CL**5
00722                                     AT-VOID-SELECT-DT.               CL**5
00723                                                                      CL**3
00724      MOVE PI-PROCESSOR-ID        TO AT-PAYMENT-LAST-UPDATED-BY.      CL*12
00725      MOVE SAVE-BIN-DATE          TO AT-PAYMENT-LAST-MAINT-DT.        CL*12
00726      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.            CL*12
00727                                                                   EL143
062121     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' or 'FNL'
CIDMOD         PERFORM 9870-OUTPUT-ACTIVITY-RECORD THRU                      000
CIDMOD                 9870-EXIT                                             000
CIDMOD         IF ERROR-ON-OUTPUT                                            000
CIDMOD             MOVE -1             TO PFKEYL                             000
CIDMOD             MOVE AL-UANON       TO PFKEYA                             000
CIDMOD*            MOVE MAP-NAMEA      TO MAP-NAME                           000
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                            000
CIDMOD                     9995-EXIT                                         000
CIDMOD             GO TO 8200-SEND-DATAONLY                                  000
CIDMOD         END-IF                                                        000
CIDMOD     END-IF.                                                           000

           move at-amount-paid         to ws-at-amount-paid
           move at-days-in-period      to ws-at-days-in-period
           move at-payment-type        to ws-at-payment-type

00728      EXEC CICS REWRITE                                            EL143
00729           DATASET  (ELTRLR-FILE-ID)                               EL143
00730           FROM     (ACTIVITY-TRAILERS)                            EL143
00731      END-EXEC.                                                    EL143
00732                                                                      CL*13
00733  2010-UPDATE-ZERO-TRAILER.                                           CL*13
00734                                                                   EL143
00735      MOVE CL-CONTROL-PRIMARY    TO ELTRLR-KEY.                    EL143
00736      MOVE +0                    TO ELTRLR-SEQ-NO.                 EL143
00737                                                                   EL143
00738      EXEC CICS READ                                               EL143
00739           DATASET    (ELTRLR-FILE-ID)                             EL143
00740           RIDFLD     (ELTRLR-KEY)                                 EL143
00741           SET        (ADDRESS OF ACTIVITY-TRAILERS)                  CL*17
00742           UPDATE                                                  EL143
00743      END-EXEC.                                                    EL143
00744                                                                   EL143
00745      IF WS-PAY-TYPE EQUAL '5'                                        CL**2
00746         SUBTRACT WS-AMOUNT-PAID  FROM AT-ITD-CHARGEABLE-EXPENSE.     CL*12
00747                                                                   EL143
00748      IF WS-PAY-TYPE EQUAL '6'                                        CL**2
00749         SUBTRACT WS-AMOUNT-PAID  FROM AT-ITD-PAID-EXPENSES.       EL143
00750                                                                   EL143
00751      IF AT-INITIAL-MANUAL-RESERVE NOT EQUAL +0                       CL**2
00752         ADD WS-AMOUNT-PAID       TO AT-CURRENT-MANUAL-RESERVE.    EL143
00753                                                                      CL**3
00754  2010-CHECK-OPEN-CLOSE.                                              CL*13
00755                                                                      CL*13
00756      IF (PI-PAY-TYPE IS EQUAL TO '5' OR '6')                         CL*13
00757          GO TO 2010-REWRITE-ZERO-TRAILER.                            CL*13
00758                                                                      CL*13
00759      IF (PI-PAY-TYPE IS EQUAL TO '1' OR '4' OR '7') AND              CL*13
00760         CLAIM-IS-OPEN                                                CL*13
00761          GO TO 2010-REWRITE-ZERO-TRAILER.                            CL*13
00762                                                                      CL*13
00763      IF (PI-PAY-TYPE IS EQUAL TO '2' OR '3') AND                     CL*13
00764         CLAIM-IS-CLOSED                                              CL*13
00765          GO TO 2010-REWRITE-ZERO-TRAILER.                            CL*13
00766                                                                      CL*13
00767      MOVE +1                     TO  SUB-1.                          CL*13
00768                                                                      CL*13
00769  2010-OPEN-CLOSE-LOOP.                                               CL*13
00770                                                                      CL*13
00771      IF AT-OPEN-CLOSE-TYPE (SUB-1) IS EQUAL TO SPACES                CL*13
00772          MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB-1)      CL*13
00773          IF (PI-PAY-TYPE IS EQUAL TO '1' OR '4' OR '7')              CL*13
00774              MOVE 'O'            TO  AT-OPEN-CLOSE-TYPE (SUB-1)      CL*13
00775              MOVE 'FORCE'        TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*13
00776              GO TO 2010-REWRITE-ZERO-TRAILER                         CL*13
00777          ELSE                                                        CL*13
00778              MOVE 'C'            TO  AT-OPEN-CLOSE-TYPE   (SUB-1)    CL*13
00779              MOVE 'FINAL'        TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*13
00780              GO TO 2010-REWRITE-ZERO-TRAILER.                        CL*13
00781                                                                      CL*13
00782      IF SUB-1 IS EQUAL TO 6                                          CL*13
00783        MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)   CL*13
00784        MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)   CL*13
00785        MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)   CL*13
00786        MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)   CL*13
00787        MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)   CL*13
00788        MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)   CL*13
00789        GO TO 2010-OPEN-CLOSE-LOOP.                                   CL*13
00790                                                                      CL*13
00791      ADD +1                      TO  SUB-1.                          CL*13
00792      GO TO 2010-OPEN-CLOSE-LOOP.                                     CL*13
00793                                                                      CL*13
00794  2010-REWRITE-ZERO-TRAILER.                                          CL*13
00795                                                                      CL*13
00796      MOVE PI-PROCESSOR-ID        TO AT-RESERVES-LAST-UPDATED-BY.     CL*12
00797      MOVE SAVE-BIN-DATE          TO AT-RESERVES-LAST-MAINT-DT.       CL*12
00798      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.            CL*12
00799                                                                   EL143
00800      EXEC CICS REWRITE                                            EL143
00801           DATASET  (ELTRLR-FILE-ID)                               EL143
00802           FROM     (ACTIVITY-TRAILERS)                            EL143
00803      END-EXEC.                                                    EL143
00804                                                                      CL*13
00805  2020-UPDATE-ELCERT.                                                 CL*13
00806                                                                   EL143
00807      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*16
00808          GO TO 2050-UPDATE-EMPLCY.                                   CL*16
00809                                                                      CL*16
00810      MOVE PI-COMPANY-CD    TO ELCERT-COMPANY-CD.                  EL143
00811      MOVE CL-CERT-CARRIER  TO ELCERT-CARRIER.                     EL143
00812      MOVE CL-CERT-GROUPING TO ELCERT-GROUP.                       EL143
00813      MOVE CL-CERT-STATE    TO ELCERT-STATE.                       EL143
00814      MOVE CL-CERT-ACCOUNT  TO ELCERT-ACCOUNT.                     EL143
00815      MOVE CL-CERT-EFF-DT   TO ELCERT-EFF-DATE.                    EL143
00816      MOVE CL-CERT-NO       TO ELCERT-CERT-NO.                     EL143
00817                                                                   EL143
00818      EXEC CICS READ                                               EL143
00819           DATASET     (ELCERT-FILE-ID)                            EL143
00820           RIDFLD      (ELCERT-KEY)                                EL143
00821           SET         (ADDRESS OF CERTIFICATE-MASTER)                CL*17
00822           UPDATE                                                  EL143
00823      END-EXEC.                                                    EL143
00824                                                                   EL143
           move cm-ah-benefit-amt      to ws-cm-ah-benefit-amt
           move cm-ah-orig-term        to ws-cm-ah-orig-term

100518     IF CL-CLAIM-TYPE NOT EQUAL PI-LIFE-OVERRIDE-L1 AND 'O'          CL**2
00826         GO TO 2020-AH-VOID.                                          CL*13
00827                                                                   EL143
00828      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.                      CL**4
00829      MOVE WS-ACCESS              TO  ELCNTL-ACCESS.                  CL**4
00830      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.              CL**4
00831      MOVE '4'                    TO  ELCNTL-REC-TYPE.                CL**4
00832      MOVE ZEROS                  TO  ELCNTL-SEQ-NO.                  CL**4
00833      PERFORM 7500-FIND-BENEFIT THRU 7500-EXIT.                       CL**4
00834      IF NO-BENEFIT-FOUND                                             CL**4
00835          GO TO 8500-NOT-FOUND.                                       CL**4
00836      MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.        CL**4
00837                                                                      CL**4
00838      IF PI-LIFE-OVERRIDE-L1 IS EQUAL TO 'P' OR                       CL**4
00839         WS-LF-COVERAGE-TYPE IS EQUAL TO 'P'                          CL**4
00840          IF WS-PAY-TYPE IS EQUAL TO '4'                              CL**4
00841              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT        CL**4
00842              IF CM-LF-CURRENT-STATUS IS EQUAL TO '1' OR '2'          CL**4
00843                  GO TO 2020-REWRITE-CERT                             CL*13
00844              ELSE                                                    CL**4
00845                  MOVE CM-LF-STATUS-AT-DEATH                          CL*12
00846                                   TO CM-LF-CURRENT-STATUS            CL*12
00847                  MOVE SPACES      TO CM-LF-STATUS-AT-DEATH           CL*12
00848                  MOVE LOW-VALUES  TO CM-LF-DEATH-EXIT-DT             CL*12
00849                                      CM-LF-DEATH-DT                  CL**4
00850                  GO TO 2020-REWRITE-CERT.                            CL*13
00851                                                                      CL**4
00852      IF WS-PAY-TYPE EQUAL '4'                                        CL**2
00853         SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT          EL143
00854         GO TO 2020-REWRITE-CERT.                                     CL*13
00855                                                                   EL143
00856      IF WS-PAY-TYPE EQUAL '2'                                        CL**2
00857         IF CM-LF-CURRENT-STATUS IS EQUAL TO '1' OR '2'               CL**4
00858             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT         CL**4
00859             GO TO 2020-REWRITE-CERT                                  CL*13
00860         ELSE                                                         CL**4
00861             MOVE CM-LF-STATUS-AT-DEATH TO CM-LF-CURRENT-STATUS       CL**4
00862             MOVE SPACES        TO CM-LF-STATUS-AT-DEATH              CL**4
00863             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT         CL**4
00864             MOVE LOW-VALUES  TO CM-LF-DEATH-EXIT-DT CM-LF-DEATH-DT   CL**4
00865             GO TO 2020-REWRITE-CERT                                  CL*13
00866      ELSE                                                         EL143
00867          GO TO 2030-UNLOCK-CERT.                                     CL*16
00868                                                                   EL143
00869  2020-AH-VOID.                                                       CL*13
00870                                                                      CL**2
00871      IF WS-PAY-TYPE EQUAL '4'                                        CL**2
00872         SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT           EL143
00873         GO TO 2020-REWRITE-CERT.                                     CL*13
00874                                                                   EL143
00875      IF WS-PAY-TYPE EQUAL '3'                                        CL**2
00876         MOVE CM-AH-STATUS-AT-SETTLEMENT TO CM-AH-CURRENT-STATUS   EL143
00877         MOVE SPACES        TO CM-AH-STATUS-AT-SETTLEMENT          EL143
00878         SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT           EL143
00879         MOVE LOW-VALUES TO CM-AH-SETTLEMENT-DT                    EL143
00880                            CM-AH-SETTLEMENT-EXIT-DT               EL143
00881         GO TO 2020-REWRITE-CERT                                      CL*13
00882      ELSE                                                         EL143
00883         GO TO 2030-UNLOCK-CERT.                                      CL*16
00884                                                                   EL143
00885  2020-REWRITE-CERT.                                                  CL*13
00886                                                                      CL**2
00887      EXEC CICS REWRITE                                            EL143
00888           DATASET  (ELCERT-FILE-ID)                               EL143
00889           FROM     (CERTIFICATE-MASTER)                           EL143
00890      END-EXEC.                                                    EL143
00891                                                                   EL143
00892      GO TO 2090-UPDATE-ELACTQ.                                       CL*13
00893                                                                   EL143
00894  2030-UNLOCK-CERT.                                                   CL*16
00895                                                                      CL**2
00896      EXEC CICS UNLOCK                                             EL143
00897           DATASET  (ELCERT-FILE-ID)                               EL143
00898      END-EXEC.                                                    EL143
00899                                                                      CL*16
00900      GO TO 2090-UPDATE-ELACTQ.                                       CL*16
00901                                                                      CL*16
00902      EJECT                                                           CL*16
00903  2050-UPDATE-EMPLCY.                                                 CL*16
00904                                                                      CL*16
00905      MOVE PI-COMPANY-CD          TO  EMPLCY-COMPANY-CD.              CL*16
00906      MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER.                 CL*16
00907      MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.                CL*16
00908      MOVE CL-CERT-STATE          TO  EMPLCY-STATE.                   CL*16
00909      MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.                CL*16
00910      MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DATE.                CL*16
00911      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO.            CL*16
00912                                                                      CL*16
00913      EXEC CICS READ                                                  CL*16
00914          DATASET   (EMPLCY-FILE-ID)                                  CL*16
00915          RIDFLD    (EMPLCY-KEY)                                      CL*16
00916          SET       (ADDRESS OF POLICY-MASTER)                        CL*17
00917      END-EXEC.                                                       CL*16
00918                                                                      CL*16
00919      MOVE LOW-VALUES             TO WS-POLICY-UPDATE-WORKING-GRPS.   CL*18
00920      MOVE PM-COMPANY-CD          TO  WS-COMPANY-CD.                  CL*16
00921      MOVE PM-CARRIER             TO  WS-CARRIER.                     CL*16
00922      MOVE PM-GROUPING            TO  WS-GROUPING.                    CL*16
00923      MOVE PM-STATE               TO  WS-STATE.                       CL*16
00924      MOVE PM-PRODUCER            TO  WS-PRODUCER.                    CL*16
00925      MOVE PM-POLICY-EFF-DT       TO  WS-POLICY-EFF-DT.               CL*16
00926      MOVE PM-REFERENCE-NUMBER    TO  WS-REFERENCE-NUMBER.            CL*16
00927                                                                      CL*16
00928      MOVE 'RW'                   TO  WS-EMPLCY-FUNCTION.             CL*16
00929      MOVE PI-PROCESSOR-ID        TO  WS-LAST-CHANGE-PROCESSOR.       CL*16
00930      MOVE SAVE-BIN-DATE          TO  WS-LAST-CHANGE-DT.              CL*16
00931      MOVE EIBTIME                TO  WS-LAST-CHANGE-TIME.            CL*16
00932                                                                      CL*16
080322     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F' OR 'B' OR 'H'
00934          GO TO 2050-UPDATE-AH-POLICY-DATA.                           CL*16
00935                                                                      CL*16
00936  2050-UPDATE-LF-POLICY-DATA.                                         CL*16
00937                                                                      CL*16
00938      IF WS-PAY-TYPE IS EQUAL TO '2'                                  CL*16
00939 *** FROM AT-PAYMENT-TYPE - TYPES 1-6 IN CONVENIENCE VALUES           CL*18
00940        IF (WS-CV-PMT-CODE IS EQUAL TO '1' OR '2' OR '3' OR '4')      CL*16
00941 *** LIFE / HALF LIFE / ADD / HALF ADD - CONV VALUES                  CL*18
00942          COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -     CL*16
00943                                          WS-AMOUNT-PAID              CL*16
00944          COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -             CL*16
00945                                      WS-AMOUNT-PAID                  CL*16
00946          COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.    CL*16
00947                                                                      CL*16
00948      IF WS-PAY-TYPE IS EQUAL TO '2'                                  CL*16
00949        IF (WS-CV-PMT-CODE IS EQUAL TO '5' OR '6')                    CL*16
00950 *** RIDER AND HALF RIDER - CONV VALUES                               CL*18
00951          COMPUTE WS-CLAIM-RIDER-ITD = PM-CLAIM-RIDER-ITD -           CL*16
00952                                       WS-AMOUNT-PAID.                CL*16
00953                                                                      CL*16
00954      IF WS-PAY-TYPE IS EQUAL TO '4'                                  CL*16
00955 *** ADDITIONAL PAYMENT - TYPE 8 CONV VALUES                          CL*18
00956          COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -     CL*16
00957                                          WS-AMOUNT-PAID              CL*16
00958          COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -             CL*16
00959                                      WS-AMOUNT-PAID                  CL*16
00960          COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.    CL*16
00961                                                                      CL*18
00962      IF WS-PAY-TYPE = '6'                                            CL*18
00963 *** NON CHARGEABLE EXPENSE - TYPE 7 IN CONVENIENCE                   CL*18
00964          COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -     CL*18
00965                                          WS-AMOUNT-PAID.             CL*18
00966                                                                      CL*18
00967      IF  PM-CLAIM-SETTLEMENT                                         CL*18
00968                                                                      CL*18
00969          IF  WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'               CL*18
00970              MOVE '6'            TO WS-CURRENT-STATUS                CL*18
00971                                                                      CL*18
00972              IF  PM-EXIT-DT GREATER THAN LOW-VALUES                  CL*18
00973                  MOVE HIGH-VALUES                                    CL*18
00974                                  TO WS-EXIT-DT                       CL*18
00975              END-IF.                                                 CL*18
00976                                                                      CL*16
00977      GO TO 2050-UPDATE-CLAIM-HISTORY.                                CL*16
00978                                                                      CL*16
00979  2050-UPDATE-AH-POLICY-DATA.                                         CL*16
00980                                                                      CL*16
00981      IF (WS-PAY-TYPE IS EQUAL TO '1' OR '4')                         CL*16
00982          COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -     CL*16
00983                                          WS-AMOUNT-PAID              CL*16
00984          COMPUTE WS-CLAIM-AH-ITD = PM-CLAIM-AH-ITD -                 CL*16
00985                                    WS-AMOUNT-PAID                    CL*16
00986          COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.    CL*16
00987                                                                      CL*18
00988      IF WS-PAY-TYPE = '6'                                            CL*18
00989          COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -     CL*18
00990                                          WS-AMOUNT-PAID.             CL*18
00991                                                                      CL*18
00992      IF  PM-CLAIM-SETTLEMENT                                         CL*18
00993                                                                      CL*18
00994          IF  WS-CV-PMT-CODE = '2'                                    CL*18
00995              MOVE '6'            TO WS-CURRENT-STATUS                CL*18
00996                                                                      CL*18
00997          ELSE                                                        CL*18
00998              IF  WS-CV-PMT-CODE = '1'                                CL*18
00999                      AND                                             CL*18
01000                  WS-CLAIM-PAYMENTS-ITD LESS THAN                     CL*18
01001                      PM-INS-TOTAL-BENEFIT                            CL*18
01002                  MOVE '6'        TO WS-CURRENT-STATUS.               CL*18
01003                                                                      CL*16
01004  2050-UPDATE-CLAIM-HISTORY.                                          CL*16
01005                                                                      CL*16
01006      IF PM-CLAIM-ATTACH-CNT IS EQUAL TO +1 OR                        CL*16
01007         PM-CLAIM-INCURRED-DT IS EQUAL TO CL-INCURRED-DT              CL*16
01008          NEXT SENTENCE                                               CL*16
01009      ELSE                                                            CL*16
01010          GO TO 2050-FINISH-POLICY-UPDATE.                            CL*16
01011                                                                      CL*16
01012      IF (PM-CLAIM-ATTACH-CNT IS EQUAL TO +1) AND                     CL*16
01013         (CL-NO-OF-PMTS-MADE IS EQUAL TO +0)                          CL*16
01014          OR                                                          CL*16
01015         (PM-CLAIM-PAYMENT-CNT IS EQUAL TO +0)                        CL*16
01016          MOVE +0                 TO  WS-CLAIM-LAST-PAYMENT-AMT       CL*16
01017          MOVE '1'                TO  WS-CLAIM-INTERFACE-SW           CL*16
01018          MOVE HIGH-VALUES        TO  WS-CLAIM-INCURRED-DT            CL*16
01019                                      WS-CLAIM-PAID-TO-DT             CL*16
01020      ELSE                                                            CL*16
01021          MOVE CL-PAID-THRU-DT    TO  WS-CLAIM-PAID-TO-DT             CL*16
01022          MOVE CL-LAST-PMT-AMT    TO  WS-CLAIM-LAST-PAYMENT-AMT.      CL*16
01023                                                                      CL*16
01024  2050-FINISH-POLICY-UPDATE.                                          CL*16
01025                                                                      CL*16
01026      IF WS-CLAIM-PAYMENT-CNT IS NEGATIVE                             CL*16
01027          MOVE +0                 TO  WS-CLAIM-PAYMENT-CNT.           CL*16
01028                                                                      CL*16
01029      EXEC CICS LINK                                                  CL*16
01030          PROGRAM    ('EMPLCY')                                       CL*16
01031          COMMAREA   (WS-POLICY-MASTER-UPDATE-AREA)                   CL*16
01032          LENGTH     (WS-PM-COMM-LNGTH)                               CL*16
01033      END-EXEC.                                                       CL*16
01034                                                                      CL*16
01035      IF WS-EMPLCY-RETURN-CODE IS EQUAL TO LOW-VALUES                 CL*16
01036          NEXT SENTENCE                                               CL*16
01037      ELSE                                                            CL*16
01038          MOVE ER-9211            TO  EMI-ERROR                       CL*16
01039          MOVE -1                 TO  MAINTO                          CL*16
01040          MOVE AL-UABON           TO  MAINTA                          CL*16
01041          PERFORM 9900-ERROR-FORMAT                                   CL*16
01042          EXEC CICS SYNCPOINT                                         CL*16
01043              ROLLBACK                                                CL*16
01044          END-EXEC                                                    CL*16
01045          GO TO 8200-SEND-DATAONLY.                                   CL*16
01046                                                                   EL143
01047  2090-UPDATE-ELACTQ.                                                 CL*13
01048                                                                      CL**2
01049      EXEC CICS READ                                               EL143
01050           DATASET    (ELACTQ-FILE-ID)                             EL143
01051           RIDFLD     (ELACTQ-KEY)                                 EL143
01052           SET        (ADDRESS OF ACTIVITY-QUE)                       CL*17
01053           UPDATE                                                  EL143
01054      END-EXEC.                                                    EL143
01055                                                                   EL143
01056      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC                            CL**5
01057          MOVE +0                 TO AQ-PAYMENT-COUNTER.              CL**5
01058                                                                      CL**5
01059      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC                       CL**6
01060          MOVE +0                 TO  AQ-PMT-UNAPPROVED-COUNT.        CL**6
01061                                                                      CL**6
01062      IF AQ-PMT-UNAPPROVED-COUNT GREATER THAN +0                   EL143
01063         SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT.                 EL143
01064                                                                   EL143
01065      SUBTRACT +1    FROM PI-UNAPPROVED-COUNT.                     EL143
01066      MOVE PI-UNAPPROVED-COUNT    TO UCOUNTO.                      EL143
01067                                                                   EL143
01068      IF AQ-PAYMENT-COUNTER IS GREATER THAN +0                        CL**5
01069          SUBTRACT +1 FROM AQ-PAYMENT-COUNTER.                        CL**5
01070                                                                      CL**5
01071      IF AQ-PAYMENT-COUNTER IS EQUAL TO +0                            CL**5
01072          MOVE SPACE               TO AQ-PENDING-PAYMENT-FLAG.        CL**5
01073                                                                      CL**5
01074      IF AQ-PENDING-ACTIVITY-FLAGS IS EQUAL TO SPACES                 CL**5
01075          EXEC CICS DELETE                                            CL**5
01076              DATASET   (ELACTQ-FILE-ID)                              CL**5
01077          END-EXEC                                                    CL**5
01078      ELSE                                                            CL**5
01079          EXEC CICS REWRITE                                           CL**5
01080              DATASET  (ELACTQ-FILE-ID)                               CL**5
01081              FROM     (ACTIVITY-QUE)                                 CL**5
01082          END-EXEC.                                                   CL**5
01083                                                                   EL143
01084  2090-REWRITE-ELMSTR.                                                CL*13
01085                                                                      CL*13
01086      IF (WS-PAY-TYPE IS EQUAL TO '4' OR '5' OR '6')                  CL*13
01087          GO TO 2092-CONT-REWRITE.                                    CL*16
01088                                                                      CL*16
01089      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*16
01090          IF CL-NO-OF-PMTS-MADE IS GREATER THAN +0                    CL*16
01091              GO TO 2092-CONT-REWRITE.                                CL*16
01092                                                                      CL*16
01093      MOVE 'O'                    TO  CL-CLAIM-STATUS.                CL*16
01094                                                                      CL*16
01095  2092-CONT-REWRITE.                                                  CL*16
01096                                                                      CL*13
           if ws-at-payment-type not = '5' and '6' and 'I'
              perform 2100-upd-cert-trlr thru 2100-exit
           end-if

01097      EXEC CICS REWRITE                                            EL143
01098           DATASET  (ELMSTR-FILE-ID)                               EL143
01099           FROM     (CLAIM-MASTER)                                 EL143
01100      END-EXEC.                                                    EL143
01101                                                                   EL143
01102      MOVE ZEROS                  TO EMI-ERROR                     EL143
01103      MOVE -1                     TO MAINTL.                       EL143
01104      MOVE SPACES                 TO MAINTO.                          CL**8
01105      MOVE AL-UANOF               TO MAINTA.                          CL*14
01106      PERFORM 9900-ERROR-FORMAT                                    EL143
01107      GO TO 8200-SEND-DATAONLY.                                    EL143
01108                                                                   EL143
01109  2095-NOT-FOUND.                                                  EL143
01110                                                                      CL**2
01111      MOVE  ER-0142               TO EMI-ERROR.                    EL143
01112      MOVE AL-UNBON               TO CLAIMA                        EL143
01113                                     CERTA.                        EL143
01114      MOVE -1                     TO CARRL.                           CL*11
01115      MOVE AL-UNBON               TO CARRA.                           CL*11
01116                                                                   EL143
01117      PERFORM 9900-ERROR-FORMAT.                                   EL143
01118      GO TO 8200-SEND-DATAONLY.                                    EL143

061013 2100-UPD-CERT-TRLR.
061013
061013     MOVE CL-COMPANY-CD          TO CTRLR-COMP-CD
061013     MOVE CL-CERT-KEY-DATA       TO ELCRTT-KEY (2:21)
061013     MOVE CL-CERT-NO             TO CTRLR-CERT-NO
061013     MOVE 'B'                    TO CTRLR-REC-TYPE
061013
061013     EXEC CICS READ
061013        UPDATE
061013        DATASET   ('ELCRTT')
061013        RIDFLD    (ELCRTT-KEY)
061013        set       (address of CERTIFICATE-TRAILERS)
061013        RESP      (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cl-claim-no = cs-claim-no (s1))
              end-perform

           if s1 < +25
061013        subtract ws-at-amount-paid from cs-total-paid (s1)
061013        if cs-total-paid (s1) < zeros
061013           move zeros            to cs-total-paid (s1)
061013        end-if
061013        subtract at-days-in-period from cs-days-paid (s1)
061013        if cs-days-paid (s1) < zeros
061013           move zeros            to cs-days-paid (s1)
061013        end-if
              if cl-claim-type not = 'L' and 'P'
                 perform 2110-calc-rem-bens
                                       thru 2110-exit
              end-if
061013        exec cics rewrite
061013           dataset    ('ELCRTT')
061013           from       (certificate-trailers)
061013           resp       (ws-response)
061013        end-exec
061013     end-if              

061013     .
061013 2100-EXIT.
061013     EXIT.

       2110-calc-rem-bens.

           move cm-ah-orig-term        to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if

           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
              if (cs-benefit-period (s2) = cl-benefit-period)
                 and (cs-insured-type (s2) = cl-insured-type)
                 and (cs-claim-type (s2) = cl-claim-type)
                 compute ws-tot-days-paid =
                    ws-tot-days-paid + cs-days-paid (s2)
                 compute ws-tot-amt-paid =
                    ws-tot-amt-paid + cs-total-paid (s2)
              end-if
           end-perform
           compute cs-remaining-bens (s1) =
              ws-max-bens / cm-ah-benefit-amt
           if cs-remaining-bens (s1) < zeros
              move zeros            to cs-remaining-bens (s1)
           end-if

           .
       2110-exit.
           exit.

01121  3000-SHOW-CLAIM-PAYMENT.                                         EL143
01122                                                                      CL**2
01123      IF MAINTI EQUAL 'S'                                             CL**2
01124         IF CLAIML GREATER THAN +0 AND                             EL143
01125            CERTL  GREATER THAN +0                                 EL143
01126            NEXT SENTENCE                                          EL143
01127         ELSE                                                      EL143
01128            MOVE ER-0005     TO EMI-ERROR                          EL143
01129            MOVE -1          TO MAINTL                             EL143
01130            MOVE AL-UNBON    TO MAINTA                             EL143
01131            PERFORM 9900-ERROR-FORMAT                              EL143
01132            GO TO 8200-SEND-DATAONLY.                              EL143
01133                                                                   EL143
01134      IF MAINTI EQUAL 'S'                                             CL**2
01135         MOVE SPACES              TO ELACTQ-KEY                    EL143
01136         MOVE PI-COMPANY-CD       TO ELACTQ-COMPANY-CD             EL143
01137         MOVE CARRI               TO ELACTQ-CARRIER                EL143
01138         MOVE CLAIMI              TO ELACTQ-CLAIM-NO               EL143
01139         MOVE CERTI               TO ELACTQ-CERT-PRIME             EL143
01140         MOVE SUFFIXI             TO ELACTQ-CERT-SFX.                 CL*12
01141                                                                   EL143
01142      EXEC CICS HANDLE CONDITION                                   EL143
01143           NOTFND     (3100-NOT-FOUND)                             EL143
01144           ENDFILE    (3100-NOT-FOUND)                             EL143
01145      END-EXEC.                                                    EL143
01146                                                                   EL143
01147      EXEC CICS READ                                               EL143
01148           DATASET     (ELACTQ-FILE-ID)                            EL143
01149           RIDFLD      (ELACTQ-KEY)                                EL143
01150           SET         (ADDRESS OF ACTIVITY-QUE)                      CL*17
01151      END-EXEC.                                                    EL143
01152                                                                   EL143
01153      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR                         CL*13
01154         AQ-PAYMENT-COUNTER IS EQUAL TO +0                            CL*13
01155          GO TO 3100-NOT-FOUND.                                       CL*13
01156                                                                   EL143
01157      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC  OR                      CL*12
01158         AQ-PMT-UNAPPROVED-COUNT EQUAL +0                             CL*12
01159           GO TO 3100-NOT-FOUND.                                      CL*12
01160                                                                   EL143
01161      MOVE AQ-CONTROL-PRIMARY           TO PI-LAST-ELACTQ-KEY      EL143
01162                                           ELMSTR-KEY              EL143
01163                                           ELTRLR-KEY.             EL143
01164                                                                   EL143
01165      IF MAINTI EQUAL 'S'                                             CL**2
01166         MOVE +90                          TO ELTRLR-SEQ-NO           CL**3
01167         MOVE SPACES                       TO  PI-DIAGNOSIS           CL*14
01168      ELSE                                                         EL143
01169         MOVE PI-LAST-TRLR-SEQ-NO          TO ELTRLR-SEQ-NO.       EL143
01170                                                                   EL143
01171      EXEC CICS READ                                               EL143
01172           DATASET    (ELMSTR-FILE-ID)                             EL143
01173           RIDFLD     (ELMSTR-KEY)                                 EL143
01174           SET        (ADDRESS OF CLAIM-MASTER)                       CL*17
01175      END-EXEC.                                                    EL143
01176                                                                      CL*13
01177      MOVE CL-CARRIER                 TO  PI-CARRIER.                 CL*13
01178      MOVE CL-CLAIM-NO                TO  PI-CLAIM-NO.                CL*13
01179      MOVE CL-CERT-NO                 TO  PI-CERT-NO.                 CL*13
01180      MOVE CL-CERT-GROUPING           TO  PI-GROUPING.                CL*13
01181      MOVE CL-CERT-STATE              TO  PI-STATE.                   CL*13
01182      MOVE CL-CERT-ACCOUNT            TO  PI-ACCOUNT.                 CL*13
01183      MOVE CL-CERT-EFF-DT             TO  PI-CERT-EFF-DT.             CL*13
01184                                                                   EL143
01185                                                                      CL*14
01186  3010-READ-NEXT-ELTRLR.                                           EL143
01187                                                                      CL**2
01188      EXEC CICS READ                                               EL143
01189           DATASET    (ELTRLR-FILE-ID)                             EL143
01190           RIDFLD     (ELTRLR-KEY)                                 EL143
01191           SET        (ADDRESS OF ACTIVITY-TRAILERS)                  CL*17
01192           GTEQ                                                    EL143
01193      END-EXEC.                                                    EL143
01194                                                                   EL143
01195      MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.             EL143
01196      IF WS-HOLD-ELTRLR-KEY NOT EQUAL CL-CONTROL-PRIMARY              CL**2
01197         GO TO 3100-NOT-FOUND.                                     EL143
01198                                                                      CL**3
01199      IF AT-TRAILER-TYPE EQUAL '6'                                    CL**3
01200         IF AT-SEQUENCE-NO EQUAL +90                                  CL**9
01201            MOVE AT-INFO-LINE-1   TO  PI-DIAGNOSIS.                   CL*14
01202                                                                   EL143
01203      IF (AT-TRAILER-TYPE NOT EQUAL '2')                              CL**2
01204              OR                                                   EL143
01205         (AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U')                       CL**2
01206         MOVE AT-CONTROL-PRIMARY TO ELTRLR-KEY                     EL143
01207         ADD +1 TO ELTRLR-SEQ-NO                                   EL143
01208         GO TO 3010-READ-NEXT-ELTRLR.                              EL143
01209                                                                   EL143
01210      MOVE LOW-VALUES                   TO EL143AI.                EL143
01211                                                                   EL143
01212      MOVE PI-DIAGNOSIS          TO DIAGNO.                           CL**9
01213      MOVE CL-CARRIER            TO CARRO.                         EL143
01214      MOVE CL-CLAIM-NO           TO CLAIMO.                        EL143
01215      MOVE CL-CERT-PRIME         TO CERTO.                         EL143
01216      MOVE CL-CERT-SFX           TO SUFFIXO.                       EL143
01217                                                                   EL143
01218      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC                       CL**6
01219          MOVE +0                       TO  PI-UNAPPROVED-COUNT       CL**6
01220                                            UCOUNTO                   CL**6
01221      ELSE                                                            CL**6
01222          MOVE AQ-PMT-UNAPPROVED-COUNT  TO  PI-UNAPPROVED-COUNT       CL**6
01223                                            UCOUNTO.                  CL**6
01224                                                                   EL143

121802     EVALUATE TRUE
121802
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802        MOVE PI-AH-OVERRIDE-L6   TO TYPEO
121802
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802        MOVE PI-LIFE-OVERRIDE-L6 TO TYPEO
121802
121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE '  IU  '            TO TYPEO
121802
121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO TYPEO
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614        MOVE ' FAM  '            TO TYPEO
080322     WHEN CL-CLAIM-TYPE = 'B'
080322        MOVE ' BRV  '            TO TYPEO
080322     WHEN CL-CLAIM-TYPE = 'H'
080322        MOVE ' HSP '             TO TYPEO
080322
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO TYPEO
121802
121802     END-EVALUATE

01230      IF CL-CLAIM-STATUS  EQUAL 'O'                                   CL**2
01231         MOVE ' OPEN '           TO STATUSO                        EL143
01232      ELSE                                                         EL143
01233         MOVE 'CLOSED'           TO STATUSO.                       EL143
01234                                                                   EL143
01235      MOVE CL-INCURRED-DT        TO DC-BIN-DATE-1.                 EL143
01236      MOVE ' '                   TO DC-OPTION-CODE.                   CL*12
01237      PERFORM 9700-DATE-LINK.                                         CL*12
01238      IF NO-CONVERSION-ERROR                                       EL143
01239         MOVE DC-GREG-DATE-1-EDIT     TO INCDTEO.                  EL143
01240                                                                   EL143
01241      MOVE CL-REPORTED-DT        TO DC-BIN-DATE-1.                 EL143
01242      MOVE ' '                   TO DC-OPTION-CODE.                   CL*12
01243      PERFORM 9700-DATE-LINK.                                         CL*12
01244      IF NO-CONVERSION-ERROR                                       EL143
01245         MOVE DC-GREG-DATE-1-EDIT     TO REPDTEO.                  EL143
01246                                                                   EL143
01247      MOVE CL-PAID-THRU-DT       TO DC-BIN-DATE-1.                    CL*12
01248      MOVE ' '                   TO DC-OPTION-CODE.                   CL*12
01249      PERFORM 9700-DATE-LINK.                                         CL*12
01250                                                                      CL**3
01251      IF NO-CONVERSION-ERROR                                       EL143
01252         MOVE DC-GREG-DATE-1-EDIT     TO PAYTHRO                      CL**3
01253         IF PI-USES-PAID-TO                                           CL**3
01254            MOVE CL-PAID-THRU-DT      TO DC-BIN-DATE-1                CL**3
01255            MOVE '6'                  TO DC-OPTION-CODE               CL**3
01256            MOVE +1                   TO DC-ELAPSED-DAYS              CL**3
01257            MOVE +0                   TO DC-ELAPSED-MONTHS            CL**3
01258            PERFORM 9700-DATE-LINK                                    CL**3
01259            IF NO-CONVERSION-ERROR                                    CL**3
01260               MOVE DC-GREG-DATE-1-EDIT     TO PAYTHRO.               CL**3
01261                                                                   EL143
01262      MOVE CL-LAST-PMT-DT        TO DC-BIN-DATE-1.                 EL143
01263      MOVE ' '                   TO DC-OPTION-CODE.                   CL*12
01264      PERFORM 9700-DATE-LINK                                       EL143
01265      IF NO-CONVERSION-ERROR                                       EL143
01266         MOVE DC-GREG-DATE-1-EDIT     TO LSTPAIDO.                 EL143
01267                                                                   EL143
01268      IF CL-NO-OF-PMTS-MADE NUMERIC                                EL143
01269         MOVE CL-NO-OF-PMTS-MADE      TO NOPMTSO                   EL143
01270      ELSE                                                         EL143
01271         MOVE ZEROS                   TO NOPMTSO.                  EL143
01272                                                                   EL143
01273      MOVE CL-NO-OF-DAYS-PAID         TO DAYPAIDO.                    CL*12
01274                                                                   EL143
01275      MOVE CL-INSURED-LAST-NAME       TO LNAMEO.                   EL143
01276      MOVE CL-PROCESSOR-ID            TO PROCO.                       CL**2
01277                                                                   EL143
01278      IF AT-PAYMENT-TYPE EQUAL '1'                                    CL**2
01279         MOVE 'PARTIAL'               TO PMTTYPO                   EL143
01280      ELSE                                                         EL143
01281      IF AT-PAYMENT-TYPE EQUAL '2'                                    CL**2
01282         MOVE 'FINAL'                 TO PMTTYPO                   EL143
01283      ELSE                                                         EL143
01284      IF AT-PAYMENT-TYPE EQUAL '3'                                    CL**2
01285         MOVE 'SETTLEMENT'            TO PMTTYPO                   EL143
01286      ELSE                                                         EL143
01287      IF AT-PAYMENT-TYPE EQUAL '4'                                    CL**2
01288         MOVE 'ADDITIONAL'            TO PMTTYPO                   EL143
01289      ELSE                                                         EL143
01290      IF AT-PAYMENT-TYPE EQUAL '5'                                    CL**2
01291         MOVE 'CHG EXP'               TO PMTTYPO                   EL143
01292      ELSE                                                         EL143
01293      IF AT-PAYMENT-TYPE EQUAL '6'                                    CL**2
01294         MOVE 'N-CHG EXP'             TO PMTTYPO                   EL143
01295      ELSE                                                         EL143
01296         MOVE AT-PAYMENT-TYPE         TO PMTTYPO.                  EL143
01297                                                                   EL143
01298      MOVE AT-CHECK-NO                TO CHECKO.                   EL143
01299      MOVE AT-AMOUNT-PAID             TO AMTPDO.                   EL143
01300                                                                   EL143
01301      MOVE AT-PMT-SELECT-DT           TO DC-BIN-DATE-1.            EL143
01302      MOVE ' '                        TO DC-OPTION-CODE.           EL143
01303      PERFORM 9700-DATE-LINK.                                         CL*12
01304      IF NO-CONVERSION-ERROR                                       EL143
01305         MOVE DC-GREG-DATE-1-EDIT     TO SELDTEO.                  EL143
01306                                                                   EL143
01307      MOVE AT-PAID-FROM-DT            TO DC-BIN-DATE-1.            EL143
01308      MOVE ' '                        TO DC-OPTION-CODE.           EL143
01309      PERFORM 9700-DATE-LINK.                                         CL*12
01310      IF NO-CONVERSION-ERROR                                       EL143
01311         MOVE DC-GREG-DATE-1-EDIT     TO PDFROMO.                  EL143
01312                                                                   EL143
01313      MOVE AT-PAID-THRU-DT            TO DC-BIN-DATE-1.            EL143
01314      MOVE ' '                        TO DC-OPTION-CODE.           EL143
01315      PERFORM 9700-DATE-LINK.                                         CL*12
01316      IF NO-CONVERSION-ERROR                                       EL143
01317         MOVE DC-GREG-DATE-1-EDIT     TO PDTHRUO                      CL**3
01318         IF PI-USES-PAID-TO                                           CL**3
01319            MOVE AT-PAID-THRU-DT            TO DC-BIN-DATE-1          CL**3
01320            MOVE '6'                        TO DC-OPTION-CODE         CL**3
01321            MOVE +1                         TO DC-ELAPSED-DAYS        CL**3
01322            MOVE +0                         TO DC-ELAPSED-MONTHS      CL**3
01323            PERFORM 9700-DATE-LINK                                    CL**3
01324            IF NO-CONVERSION-ERROR                                    CL**3
01325               MOVE DC-GREG-DATE-1-EDIT     TO PDTHRUO.               CL**3
01326                                                                   EL143
01327      MOVE AT-VOID-REASON             TO COMMENTO.                 EL143
01328      MOVE AT-RECORDED-BY             TO PAIDBYO.                     CL**2
01329      MOVE AT-SEQUENCE-NO             TO SEQO.                     EL143
01330                                                                      CL*12
01331      MOVE AT-APPROVAL-LEVEL-REQD     TO AREQO.                       CL*12
01332      MOVE AT-APPROVED-LEVEL          TO ALEVO.                       CL*12
01333                                                                   EL143
01334      IF AT-PAYEE-TYPE EQUAL 'I'                                      CL**3
01335         MOVE 'INSURED   '            TO PDTOO                     EL143
01336      ELSE                                                         EL143
01337      IF AT-PAYEE-TYPE EQUAL 'B'                                      CL**3
01338         MOVE 'BENEFICARY'            TO PDTOO                     EL143
01339      ELSE                                                         EL143
01340      IF AT-PAYEE-TYPE EQUAL 'A'                                      CL**3
01341         MOVE 'ACCOUNT   '            TO PDTOO                     EL143
01342      ELSE                                                         EL143
01343      IF AT-PAYEE-TYPE EQUAL 'O'                                      CL**3
01344         MOVE 'OTHER-1   '            TO PDTOO                     EL143
01345      ELSE                                                         EL143
01346      IF AT-PAYEE-TYPE EQUAL 'Q'                                      CL**3
01347         MOVE 'OTHER-2   '            TO PDTOO                     EL143
01348      ELSE                                                         EL143
01349      IF AT-PAYEE-TYPE EQUAL 'P'                                      CL**3
01350         MOVE 'PHYSICIAN '            TO PDTOO                     EL143
01351      ELSE                                                         EL143
01352         MOVE AT-PAYEE-TYPE-CD        TO PDTOO.                    EL143
01353                                                                   EL143
01354      MOVE AT-RECORDED-DT             TO DC-BIN-DATE-1.            EL143
01355      MOVE ' '                        TO DC-OPTION-CODE.           EL143
01356      PERFORM 9700-DATE-LINK.                                         CL*12
01357      IF NO-CONVERSION-ERROR                                       EL143
01358         MOVE DC-GREG-DATE-1-EDIT     TO RECDTEO.                  EL143
01359                                                                   EL143
01360      IF AT-FORCE-CONTROL EQUAL '1'                                   CL**2
01361         MOVE 'Y'                     TO FORCEO                    EL143
01362      ELSE                                                         EL143
01363         MOVE 'N'                     TO FORCEO.                   EL143
01364                                                                   EL143
01365      MOVE AT-SEQUENCE-NO              TO PI-LAST-TRLR-SEQ-NO.     EL143
01366      MOVE AT-PAYMENT-LAST-UPDATED-BY  TO PI-ELTRLR-UPDATE-BY.     EL143
01367      MOVE AT-LAST-MAINT-HHMMSS        TO PI-ELTRLR-UPDATE-HHMMSS. EL143
01368      MOVE AT-CONTROL-PRIMARY          TO  PI-LAST-ELTRLR-KEY.        CL*14
01369                                                                   EL143
01370      GO TO 8100-SEND-INITIAL-MAP.                                 EL143
01371                                                                   EL143
01372  3100-NOT-FOUND.                                                  EL143
01373                                                                      CL**2
01374      IF EIBAID EQUAL DFHPF1                                          CL**3
01375         MOVE WS-HOLD-KEY TO ELACTQ-KEY                               CL**3
01376         GO TO 7005-CONTINUE-BROWSE.                                  CL**3
01377                                                                      CL**3
01378      IF EIBAID EQUAL DFHPF2                                          CL**3
01379         MOVE WS-HOLD-KEY TO ELACTQ-KEY                               CL**9
01380         GO TO 7105-CONTINUE-BROWSE.                                  CL**9
01381                                                                      CL**3
01382      MOVE  ER-0142               TO EMI-ERROR.                    EL143
01383      MOVE -1                     TO CARRL.                        EL143
01384      MOVE AL-UNBON               TO CARRA                         EL143
01385                                     CLAIMA                        EL143
01386                                     CERTA.                        EL143
01387      PERFORM 9900-ERROR-FORMAT.                                   EL143
01388      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01389                                                                   EL143
01390      EJECT                                                           CL**3
01391 ******************************************************************   CL*13
01392 *      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *   CL*13
01393 *      ACTIVITY QUE FILE.  WHEN AN ACTIVITY QUE RECORD WITH AN   *   CL*13
01394 *      UNAPPROVED PAYMENT COUNTER GREATER THAN +0 IS FOUND THE   *   CL*13
01395 *      FIRST UNAPPROVED PAYMENT FOR THAT CLAIM IS DISPLAYED.     *   CL*13
01396 ******************************************************************   CL*13
01397  7000-BROWSE-FWRD-NEXT-CLAIM.                                     EL143
01398                                                                      CL**2
01399      MOVE PI-COMPANY-CD     TO ELACTQ-COMPANY-CD.                 EL143
01400      MOVE CARRI             TO ELACTQ-CARRIER.                    EL143
01401      MOVE CLAIMI            TO ELACTQ-CLAIM-NO.                   EL143
01402      MOVE CERTI             TO ELACTQ-CERT-PRIME.                 EL143
01403      MOVE SUFFIXI           TO ELACTQ-CERT-SFX.                      CL**3
01404                                                                      CL**3
01405  7005-CONTINUE-BROWSE.                                               CL**3
01406                                                                   EL143
01407      EXEC CICS HANDLE CONDITION                                   EL143
01408           NOTFND    (7030-END-FILE)                               EL143
01409           ENDFILE   (7030-END-FILE)                               EL143
01410      END-EXEC.                                                    EL143
01411                                                                   EL143
01412      EXEC CICS STARTBR                                               CL**3
01413           DATASET   (ELACTQ-FILE-ID)                                 CL**3
01414           RIDFLD    (ELACTQ-KEY)                                     CL**3
01415      END-EXEC.                                                       CL**3
01416                                                                      CL**3
01417      MOVE 'Y' TO WS-BROWSE-SW.                                       CL**3
01418                                                                      CL**3
01419      MOVE ELACTQ-KEY     TO  WS-HOLD-KEY.                            CL**3
01420                                                                      CL**3
01421  7010-READ-NEXT-ELACTQ.                                           EL143
01422                                                                      CL**2
01423      EXEC CICS READNEXT                                              CL**3
01424           DATASET   (ELACTQ-FILE-ID)                              EL143
01425           RIDFLD    (ELACTQ-KEY)                                  EL143
01426           SET       (ADDRESS OF ACTIVITY-QUE)                        CL*17
01427      END-EXEC.                                                    EL143
01428                                                                      CL**3
01429      IF ELACTQ-KEY EQUAL WS-HOLD-KEY                                 CL**3
01430         GO TO 7010-READ-NEXT-ELACTQ.                                 CL**3
01431                                                                   EL143
01432      IF AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD                        CL**2
01433         GO TO 7030-END-FILE.                                      EL143
01434                                                                   EL143
01435      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR                         CL*13
01436         AQ-PAYMENT-COUNTER IS EQUAL TO +0                            CL*13
01437          GO TO 7010-READ-NEXT-ELACTQ.                                CL*13
01438                                                                      CL**7
01439      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC OR                       CL*13
01440         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0                       CL*13
01441          GO TO 7010-READ-NEXT-ELACTQ.                                CL*13
01442                                                                      CL**3
01443      MOVE ELACTQ-KEY     TO  WS-HOLD-KEY.                            CL**3
01444                                                                   EL143
01445      MOVE AQ-CONTROL-PRIMARY  TO ELACTQ-KEY.                      EL143
01446      MOVE +1                  TO PI-LAST-TRLR-SEQ-NO.             EL143
01447                                                                      CL**3
01448      IF WS-BROWSE-SW EQUAL 'Y'                                       CL**3
01449          PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.                   CL*13
01450                                                                      CL**3
01451      GO TO 3000-SHOW-CLAIM-PAYMENT.                               EL143
01452                                                                   EL143
01453  7030-END-FILE.                                                   EL143
01454                                                                      CL**3
01455      IF WS-BROWSE-SW EQUAL 'Y'                                       CL**3
01456          PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.                   CL*13
01457                                                                      CL**2
01458      MOVE -1                     TO MAINTL.                       EL143
01459      MOVE  ER-2237               TO EMI-ERROR.                    EL143
01460      PERFORM 9900-ERROR-FORMAT.                                   EL143
01461      GO TO 8200-SEND-DATAONLY.                                    EL143
01462                                                                   EL143
01463      EJECT                                                        EL143
01464 ******************************************************************   CL*13
01465 *      THIS ROUTINE BROWSES BACKWARD SEQUENTIALLY THROUGH THE    *   CL*13
01466 *      ACTIVITY QUE FILE.  WHEN AN ACTIVITY QUE RECORD WITH AN   *   CL*13
01467 *      UNAPPROVED PAYMENT COUNTER GREATER THAN +0 IS FOUND THE   *   CL*13
01468 *      FIRST UNAPPROVED PAYMENT FOR THAT CLAIM IS DISPLAYED.     *   CL*13
01469 ******************************************************************   CL*13
01470  7100-BROWSE-BWRD-NEXT-CLAIM.                                     EL143
01471                                                                      CL**2
01472      MOVE PI-COMPANY-CD     TO ELACTQ-COMPANY-CD.                 EL143
01473      MOVE CARRI             TO ELACTQ-CARRIER.                    EL143
01474      MOVE CLAIMI            TO ELACTQ-CLAIM-NO.                   EL143
01475      MOVE CERTI             TO ELACTQ-CERT-PRIME.                 EL143
01476      MOVE SUFFIXI           TO ELACTQ-CERT-SFX.                   EL143
01477                                                                   EL143
01478  7105-CONTINUE-BROWSE.                                               CL**9
01479                                                                      CL**9
01480      EXEC CICS HANDLE CONDITION                                   EL143
01481           NOTFND    (7130-END-FILE)                               EL143
01482           ENDFILE   (7130-END-FILE)                               EL143
01483      END-EXEC.                                                    EL143
01484                                                                   EL143
01485      EXEC CICS STARTBR                                            EL143
01486           DATASET   (ELACTQ-FILE-ID)                              EL143
01487           RIDFLD    (ELACTQ-KEY)                                  EL143
01488      END-EXEC.                                                    EL143
01489                                                                   EL143
01490      MOVE 'Y'     TO WS-BROWSE-SW.                                EL143
01491                                                                   EL143
01492  7110-READ-NEXT-ELACTQ.                                           EL143
01493                                                                      CL**2
01494      EXEC CICS READNEXT                                           EL143
01495           DATASET   (ELACTQ-FILE-ID)                              EL143
01496           RIDFLD    (ELACTQ-KEY)                                  EL143
01497           SET       (ADDRESS OF ACTIVITY-QUE)                        CL*17
01498      END-EXEC.                                                    EL143
01499                                                                   EL143
01500      EXEC CICS READPREV                                           EL143
01501           DATASET   (ELACTQ-FILE-ID)                              EL143
01502           RIDFLD    (ELACTQ-KEY)                                  EL143
01503           SET       (ADDRESS OF ACTIVITY-QUE)                        CL*17
01504      END-EXEC.                                                    EL143
01505                                                                   EL143
01506  7120-READ-PREV-ELACTQ.                                           EL143
01507                                                                      CL**2
01508      EXEC CICS READPREV                                           EL143
01509           DATASET   (ELACTQ-FILE-ID)                              EL143
01510           RIDFLD    (ELACTQ-KEY)                                  EL143
01511           SET       (ADDRESS OF ACTIVITY-QUE)                        CL*17
01512      END-EXEC.                                                    EL143
01513                                                                   EL143
01514      IF AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD                        CL**2
01515         GO TO 7130-END-FILE.                                      EL143
01516                                                                      CL**9
01517      MOVE ELACTQ-KEY             TO  WS-HOLD-KEY.                    CL**9
01518                                                                      CL**7
01519      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR                         CL*13
01520         AQ-PAYMENT-COUNTER IS EQUAL TO +0                            CL*13
01521          GO TO 7120-READ-PREV-ELACTQ.                                CL*13
01522                                                                   EL143
01523      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC OR                       CL*13
01524         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0                       CL*13
01525          GO TO 7120-READ-PREV-ELACTQ.                                CL*13
01526                                                                   EL143
01527      MOVE AQ-CONTROL-PRIMARY  TO ELACTQ-KEY.                      EL143
01528      MOVE +1                  TO PI-LAST-TRLR-SEQ-NO.             EL143
01529                                                                   EL143
01530      IF WS-BROWSE-SW EQUAL 'Y'                                       CL**2
01531         MOVE ' ' TO WS-BROWSE-SW                                  EL143
01532         PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.                    CL*13
01533                                                                   EL143
01534      GO TO 3000-SHOW-CLAIM-PAYMENT.                               EL143
01535                                                                   EL143
01536  7130-END-FILE.                                                   EL143
01537                                                                      CL**2
01538      IF WS-BROWSE-SW EQUAL 'Y'                                       CL**2
01539         MOVE ' ' TO WS-BROWSE-SW                                  EL143
01540         PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.                    CL*13
01541                                                                   EL143
01542      MOVE -1                     TO MAINTL.                       EL143
01543      MOVE  ER-2238               TO EMI-ERROR.                    EL143
01544      PERFORM 9900-ERROR-FORMAT.                                   EL143
01545      GO TO 8200-SEND-DATAONLY.                                    EL143
01546                                                                   EL143
01547      EJECT                                                           CL*13
01548 ******************************************************************   CL*13
01549 *      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *   CL*13
01550 *      ACTIVITY TRAILER FILE SEARCHING FOR AND DISPLAYING        *   CL*13
01551 *      UNAPPROVED PAYMENT TRAILER DATA ASSOCIATED WITH A         *   CL*13
01552 *      PARTICULAR CLAIM.                                         *   CL*13
01553 ******************************************************************   CL*13
01554  7200-BROWSE-FWRD-NEXT-PAYMENT.                                   EL143
01555                                                                      CL**2
01556      MOVE CARRI         TO WS-HOLD-CARR.                          EL143
01557      MOVE CLAIMI        TO WS-HOLD-CLAIM.                         EL143
01558      MOVE CERTI         TO WS-HOLD-CERT-PRIME.                    EL143
01559      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.                      EL143
01560      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.                    EL143
01561                                                                   EL143
01562      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY                     CL**2
01563         MOVE ER-0628     TO EMI-ERROR                             EL143
01564         MOVE -1          TO MAINTL                                EL143
01565         MOVE AL-UNBON    TO MAINTA                                EL143
01566         PERFORM 9900-ERROR-FORMAT                                 EL143
01567         GO TO 8200-SEND-DATAONLY.                                 EL143
01568                                                                   EL143
01569      MOVE PI-LAST-ELACTQ-KEY  TO ELTRLR-KEY.                         CL*12
01570      MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.                   EL143
01571      ADD +1   TO ELTRLR-SEQ-NO.                                   EL143
01572                                                                   EL143
01573      EXEC CICS HANDLE CONDITION                                   EL143
01574           NOTFND    (7230-END-FILE)                               EL143
01575           ENDFILE   (7230-END-FILE)                               EL143
01576      END-EXEC.                                                    EL143
01577                                                                   EL143
01578  7210-READ-NEXT-ELTRLR.                                           EL143
01579                                                                      CL**2
01580      EXEC CICS READ                                               EL143
01581           DATASET   (ELTRLR-FILE-ID)                              EL143
01582           RIDFLD    (ELTRLR-KEY)                                  EL143
01583           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*17
01584           GTEQ                                                    EL143
01585      END-EXEC.                                                    EL143
01586                                                                   EL143
01587      MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.             EL143
01588      IF WS-HOLD-ELTRLR-KEY NOT EQUAL PI-LAST-ELACTQ-KEY              CL**2
01589         GO TO 7230-END-FILE.                                      EL143
01590                                                                   EL143
01591      IF AT-TRAILER-TYPE NOT EQUAL '2'                                CL**2
01592         ADD +1 TO ELTRLR-SEQ-NO                                   EL143
01593         GO TO 7210-READ-NEXT-ELTRLR.                              EL143
01594                                                                   EL143
01595      IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'                         CL**2
01596         ADD +1 TO ELTRLR-SEQ-NO                                   EL143
01597         GO TO 7210-READ-NEXT-ELTRLR.                              EL143
01598                                                                   EL143
01599      MOVE AT-CONTROL-PRIMARY  TO ELACTQ-KEY.                      EL143
01600      MOVE AT-SEQUENCE-NO      TO PI-LAST-TRLR-SEQ-NO.             EL143
01601                                                                   EL143
01602      GO TO 3000-SHOW-CLAIM-PAYMENT.                               EL143
01603                                                                   EL143
01604  7230-END-FILE.                                                   EL143
01605                                                                      CL**2
01606      MOVE -1                     TO MAINTL.                       EL143
01607      MOVE  ER-0303               TO EMI-ERROR.                    EL143
01608      PERFORM 9900-ERROR-FORMAT.                                   EL143
01609      GO TO 8200-SEND-DATAONLY.                                    EL143
01610                                                                   EL143
01611      EJECT                                                        EL143
01612 ******************************************************************   CL*13
01613 *      THIS ROUTINE BROWSES BACKWARD SEQUENTIALLY THROUGH THE    *   CL*13
01614 *      ACTIVITY TRAILER FILE SEARCHING FOR AND DISPLAYING        *   CL*13
01615 *      UNAPPROVED PAYMENT TRAILER DATA ASSOCIATED WITH A         *   CL*13
01616 *      PARTICULAR CLAIM.                                         *   CL*13
01617 ******************************************************************   CL*13
01618  7300-BROWSE-BWRD-NEXT-PAYMENT.                                   EL143
01619                                                                      CL**2
01620      MOVE CARRI         TO WS-HOLD-CARR.                          EL143
01621      MOVE CLAIMI        TO WS-HOLD-CLAIM.                         EL143
01622      MOVE CERTI         TO WS-HOLD-CERT-PRIME.                    EL143
01623      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.                      EL143
01624      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.                    EL143
01625                                                                   EL143
01626      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY                     CL**2
01627         MOVE ER-0628     TO EMI-ERROR                             EL143
01628         MOVE -1          TO MAINTL                                EL143
01629         MOVE AL-UNBON    TO MAINTA                                EL143
01630         PERFORM 9900-ERROR-FORMAT                                 EL143
01631         GO TO 8200-SEND-DATAONLY.                                 EL143
01632                                                                   EL143
01633      MOVE PI-LAST-ELACTQ-KEY  TO ELTRLR-KEY.                         CL*12
01634      MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.                   EL143
01635                                                                   EL143
01636      EXEC CICS HANDLE CONDITION                                   EL143
01637           NOTFND    (7330-END-FILE)                               EL143
01638           ENDFILE   (7330-END-FILE)                               EL143
01639      END-EXEC.                                                    EL143
01640                                                                   EL143
01641      EXEC CICS STARTBR                                            EL143
01642           DATASET   (ELTRLR-FILE-ID)                              EL143
01643           RIDFLD    (ELTRLR-KEY)                                  EL143
01644      END-EXEC.                                                    EL143
01645                                                                   EL143
01646  7310-READ-NEXT-ELTRLR.                                           EL143
01647                                                                      CL**2
01648      EXEC CICS READNEXT                                           EL143
01649           DATASET   (ELTRLR-FILE-ID)                              EL143
01650           RIDFLD    (ELTRLR-KEY)                                  EL143
01651           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*17
01652      END-EXEC.                                                    EL143
01653                                                                   EL143
01654      EXEC CICS READPREV                                           EL143
01655           DATASET   (ELTRLR-FILE-ID)                              EL143
01656           RIDFLD    (ELTRLR-KEY)                                  EL143
01657           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*17
01658      END-EXEC.                                                    EL143
01659                                                                   EL143
01660  7320-READ-PREV-ELTRLR.                                           EL143
01661                                                                      CL**2
01662      EXEC CICS READPREV                                           EL143
01663           DATASET   (ELTRLR-FILE-ID)                              EL143
01664           RIDFLD    (ELTRLR-KEY)                                  EL143
01665           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*17
01666      END-EXEC.                                                    EL143
01667                                                                   EL143
01668      MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.             EL143
01669      IF WS-HOLD-ELTRLR-KEY NOT EQUAL PI-LAST-ELACTQ-KEY              CL**2
01670         GO TO 7330-END-FILE.                                      EL143
01671                                                                   EL143
01672      IF AT-TRAILER-TYPE NOT EQUAL '2'                                CL**2
01673         GO TO 7320-READ-PREV-ELTRLR.                              EL143
01674                                                                   EL143
01675      IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'                         CL**2
01676         GO TO 7320-READ-PREV-ELTRLR.                              EL143
01677                                                                   EL143
01678      MOVE AT-CONTROL-PRIMARY  TO ELACTQ-KEY.                      EL143
01679      MOVE AT-SEQUENCE-NO      TO PI-LAST-TRLR-SEQ-NO.             EL143
01680                                                                   EL143
01681      GO TO 3000-SHOW-CLAIM-PAYMENT.                               EL143
01682                                                                   EL143
01683  7330-END-FILE.                                                   EL143
01684                                                                      CL**2
01685      MOVE -1                     TO MAINTL.                       EL143
01686      MOVE  ER-0303               TO EMI-ERROR.                    EL143
01687      PERFORM 9900-ERROR-FORMAT.                                   EL143
01688      GO TO 8200-SEND-DATAONLY.                                    EL143
01689                                                                   EL143
01690      EJECT                                                        EL143
01691  7500-FIND-BENEFIT.                                                  CL**4
01692                                                                      CL**4
01693      EXEC CICS HANDLE CONDITION                                      CL**4
01694          ENDFILE   (7500-EXIT)                                       CL**4
01695          NOTFND    (7500-EXIT)                                       CL**4
01696      END-EXEC.                                                       CL**4
01697                                                                      CL**4
01698      EXEC CICS READ                                                  CL**4
01699          DATASET   ('ELCNTL')                                        CL**4
01700          RIDFLD    (ELCNTL-KEY)                                      CL**4
01701          SET       (ADDRESS OF CONTROL-FILE)                         CL*17
01702          GTEQ                                                        CL*13
01703      END-EXEC.                                                       CL**4
01704                                                                      CL**4
01705      IF ELCNTL-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR           CL**4
01706         ELCNTL-REC-TYPE   IS NOT EQUAL TO CF-RECORD-TYPE             CL*12
01707          GO TO 7500-EXIT.                                            CL**4
01708                                                                      CL**4
01709      PERFORM 7500-BENEFIT-DUMMY THRU 7500-DUMMY-EXIT                 CL**4
01710          VARYING SUB FROM 1 BY 1 UNTIL                               CL**4
01711              ((SUB IS GREATER THAN 8) OR                             CL**4
01712              (CF-BENEFIT-CODE (SUB) IS EQUAL TO WS-BEN-CD)).         CL**4
01713                                                                      CL**4
01714      IF SUB IS NOT EQUAL TO 9                                        CL**4
01715          MOVE 'Y'                TO  WS-BEN-SEARCH-SW.               CL**4
01716                                                                      CL**4
01717      GO TO 7500-EXIT.                                                CL**4
01718                                                                      CL**4
01719  7500-BENEFIT-DUMMY.                                                 CL**4
01720  7500-DUMMY-EXIT.                                                    CL**4
01721      EXIT.                                                           CL**4
01722                                                                      CL**4
01723  7500-EXIT.                                                          CL**4
01724      EXIT.                                                           CL**4
01725      EJECT                                                           CL*12
01726  7600-READ-COMPANY-REC.                                              CL*12
01727      EXEC CICS HANDLE CONDITION                                      CL*12
01728          ENDFILE   (7600-EXIT)                                       CL*12
01729          NOTFND    (7600-EXIT)                                       CL*12
01730      END-EXEC.                                                       CL*12
01731                                                                      CL*12
01732      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.              CL*12
01733      MOVE '1'                    TO  ELCNTL-REC-TYPE.                CL*12
01734      MOVE SPACES                 TO  ELCNTL-ACCESS.                  CL*12
01735      MOVE +0                     TO  ELCNTL-SEQ-NO.                  CL*12
01736                                                                      CL*12
01737      EXEC CICS READ                                                  CL*12
01738          DATASET   ('ELCNTL')                                        CL*12
01739          RIDFLD    (ELCNTL-KEY)                                      CL*12
01740          SET       (ADDRESS OF CONTROL-FILE)                         CL*17
01741      END-EXEC.                                                       CL*12
01742                                                                      CL*12
01743      MOVE CF-PAYMENT-APPROVAL-SW TO WS-PAYMENT-APPROVAL-SW.          CL*12
01744                                                                      CL*12
01745  7600-EXIT.                                                          CL*12
01746      EXIT.                                                           CL*12
01747                                                                      CL*12
01748  7700-READ-USER-REC.                                                 CL*12
01749      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*14
01750         GO TO 7700-EXIT.                                             CL*14
01751                                                                      CL*14
01752      EXEC CICS HANDLE CONDITION                                      CL*12
01753          ENDFILE   (7700-EXIT)                                       CL*12
01754          NOTFND    (7700-EXIT)                                       CL*12
01755      END-EXEC.                                                       CL*12
01756                                                                      CL*12
01757      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.              CL*12
01758      MOVE PI-PROCESSOR-ID        TO  ELCNTL-ACCESS.                  CL*12
01759      MOVE '2'                    TO  ELCNTL-REC-TYPE.                CL*12
01760                                                                      CL*12
01761      EXEC CICS READ                                                  CL*12
01762          DATASET   ('ELCNTL')                                        CL*12
01763          RIDFLD    (ELCNTL-KEY)                                      CL*12
01764          SET       (ADDRESS OF CONTROL-FILE)                         CL*17
01765      END-EXEC.                                                       CL*12
01766                                                                      CL*12
01767      MOVE CF-APPROVAL-LEVEL TO WS-APPROVAL-LEVEL.                    CL*12
01768                                                                      CL*12
01769  7700-EXIT.                                                          CL*12
01770      EXIT.                                                           CL*12
01771      EJECT                                                           CL*12
01772 ******************************************************************   CL*13
01773 *      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *   CL*13
01774 *      ACTIVITY QUE AND ACTIVITY TRAILER FILES SEARCHING FOR     *   CL*13
01775 *      AND DISPLAYING UNAPPROVED PAYMENT DATA RELATED TO A       *   CL*13
01776 *      USERS SPECIFIC APPROVAL LEVEL.                            *   CL*13
01777 ******************************************************************   CL*13
01778  8000-BROWSE-FWRD-NEXT-APPROVAL.                                     CL*12
01779                                                                      CL*12
01780      IF PI-UNAPPROVED-COUNT IS GREATER THAN +0                       CL*13
01781          MOVE PI-LAST-ELTRLR-KEY     TO  ELTRLR-KEY                  CL*13
01782          MOVE PI-LAST-ELACTQ-KEY     TO  ELACTQ-KEY                  CL*13
01783          SUBTRACT +1 FROM PI-UNAPPROVED-COUNT                        CL*13
01784          GO TO 8020-BROWSE-ACTIVITY-TRAILERS.                        CL*13
01785                                                                      CL*12
01786      IF PI-FIRST-TIME-SW IS EQUAL TO 'Y'                             CL*13
01787          MOVE LOW-VALUES             TO  ELACTQ-KEY                  CL*13
01788          MOVE PI-COMPANY-CD          TO  ELACTQ-COMPANY-CD           CL*13
01789          MOVE 'N'                    TO  PI-FIRST-TIME-SW            CL*13
01790      ELSE                                                            CL*13
01791          MOVE PI-LAST-ELACTQ-KEY     TO  ELACTQ-KEY.                 CL*13
01792                                                                      CL*12
01793      MOVE SPACES                     TO  PI-DIAGNOSIS.               CL*15
01794                                                                      CL*15
01795      EXEC CICS HANDLE CONDITION                                      CL*12
01796          NOTFND    (8040-END-FILE)                                   CL*13
01797          ENDFILE   (8040-END-FILE)                                   CL*13
01798      END-EXEC.                                                       CL*12
01799                                                                      CL*12
01800      EXEC CICS STARTBR                                               CL*13
01801          DATASET   (ELACTQ-FILE-ID)                                  CL*13
01802          RIDFLD    (ELACTQ-KEY)                                      CL*13
01803      END-EXEC.                                                       CL*13
01804                                                                      CL*13
01805  8010-READNEXT.                                                      CL*13
01806      EXEC CICS READNEXT                                              CL*13
01807          DATASET   (ELACTQ-FILE-ID)                                  CL*13
01808          RIDFLD    (ELACTQ-KEY)                                      CL*13
01809          SET       (ADDRESS OF ACTIVITY-QUE)                         CL*17
01810      END-EXEC.                                                       CL*13
01811                                                                      CL*13
01812      IF ELACTQ-KEY IS EQUAL TO PI-LAST-ELACTQ-KEY                    CL*13
01813          GO TO 8010-READNEXT.                                        CL*13
01814                                                                      CL*13
01815      IF AQ-COMPANY-CD IS NOT EQUAL TO PI-COMPANY-CD                  CL*13
01816          GO TO 8040-END-FILE.                                        CL*13
01817                                                                      CL*13
01818      MOVE ELACTQ-KEY             TO  PI-LAST-ELACTQ-KEY.             CL*13
01819                                                                      CL*13
01820      IF AQ-PENDING-PAYMENT-FLAG IS EQUAL TO '1'                      CL*13
01821          NEXT SENTENCE                                               CL*13
01822      ELSE                                                            CL*13
01823          GO TO 8010-READNEXT.                                        CL*13
01824                                                                      CL*13
01825      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR                         CL*13
01826         AQ-PAYMENT-COUNTER IS EQUAL TO +0                            CL*13
01827          GO TO 8010-READNEXT.                                        CL*13
01828                                                                      CL*13
01829      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC OR                    CL*13
01830         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0                       CL*13
01831          GO TO 8010-READNEXT.                                        CL*13
01832                                                                      CL*13
01833      MOVE AQ-PMT-UNAPPROVED-COUNT    TO  PI-UNAPPROVED-COUNT.        CL*13
01834      MOVE ELACTQ-KEY                 TO  ELTRLR-KEY.                 CL*13
01835      MOVE +0                         TO  ELTRLR-SEQ-NO.              CL*13
01836                                                                      CL*13
01837      PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.                       CL*13
01838                                                                      CL*13
01839  8020-BROWSE-ACTIVITY-TRAILERS.                                      CL*13
01840                                                                      CL*13
01841      EXEC CICS STARTBR                                               CL*13
01842          DATASET   (ELTRLR-FILE-ID)                                  CL*13
01843          RIDFLD    (ELTRLR-KEY)                                      CL*13
01844      END-EXEC.                                                       CL*13
01845                                                                      CL*13
01846  8030-READNEXT-ELTRLR.                                               CL*13
01847                                                                      CL*13
01848      EXEC CICS HANDLE CONDITION                                      CL*13
01849          NOTFND    (8045-END-FILE)                                   CL*13
01850          ENDFILE   (8045-END-FILE)                                   CL*13
01851      END-EXEC.                                                       CL*13
01852                                                                      CL*13
01853      EXEC CICS READNEXT                                              CL*13
01854          DATASET   (ELTRLR-FILE-ID)                                  CL*12
01855          RIDFLD    (ELTRLR-KEY)                                      CL*12
01856          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*17
01857      END-EXEC.                                                       CL*12
01858                                                                      CL*12
01859      IF ELTRLR-COMPANY-CD IS NOT EQUAL TO PI-COMPANY-CD              CL*13
01860          PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT                    CL*13
01861          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.                       CL*13
01862                                                                      CL*12
01863      IF ELTRLR-CLAIM-NO IS NOT EQUAL TO ELACTQ-CLAIM-NO              CL*13
01864          PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT                    CL*13
01865          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.                       CL*13
01866                                                                      CL*12
01867      IF ELTRLR-CERT-NO IS NOT EQUAL TO ELACTQ-CERT-NO                CL*13
01868          PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT                    CL*13
01869          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.                       CL*13
01870                                                                      CL*12
01871      IF ELTRLR-KEY IS EQUAL TO PI-LAST-ELTRLR-KEY                    CL*13
01872          GO TO 8030-READNEXT-ELTRLR.                                 CL*13
01873                                                                      CL*15
01874      IF AT-TRAILER-TYPE EQUAL '6'                                    CL*15
01875         IF AT-SEQUENCE-NO EQUAL +90                                  CL*15
01876            MOVE AT-INFO-LINE-1   TO  PI-DIAGNOSIS.                   CL*15
01877                                                                      CL*12
01878      IF AT-TRAILER-TYPE IS EQUAL TO '2'                              CL*13
01879          NEXT SENTENCE                                               CL*13
01880      ELSE                                                            CL*13
01881          GO TO 8030-READNEXT-ELTRLR.                                 CL*13
01882                                                                      CL*12
01883      IF AT-PAYMENT-APPROVAL-SW IS NOT EQUAL TO 'U'                   CL*13
01884          GO TO 8030-READNEXT-ELTRLR.                                 CL*13
01885                                                                      CL*13
031808*01886      IF AT-APPROVED-LEVEL IS NOT EQUAL TO WS-APPROVAL-LEVEL          CL*13
031808     IF AT-APPROVED-LEVEL IS NOT LESS THAN WS-APPROVAL-LEVEL
01887          GO TO 8030-READNEXT-ELTRLR.                                 CL*13
01888                                                                      CL*13
01889      MOVE AT-SEQUENCE-NO         TO  PI-LAST-TRLR-SEQ-NO.            CL*13
01890      MOVE AT-CONTROL-PRIMARY     TO  PI-LAST-ELTRLR-KEY.             CL*13
01891                                                                      CL*13
01892      PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT.                       CL*13
01893                                                                      CL*12
01894      GO TO 3000-SHOW-CLAIM-PAYMENT.                                  CL*12
01895                                                                      CL*12
01896  8040-END-FILE.                                                      CL*13
01897      MOVE -1                     TO MAINTL.                          CL*12
01898      MOVE  ER-0303               TO EMI-ERROR.                       CL*12
01899      PERFORM 9900-ERROR-FORMAT.                                      CL*12
01900      GO TO 8200-SEND-DATAONLY.                                       CL*12
01901                                                                      CL*13
01902  8045-END-FILE.                                                      CL*13
01903                                                                      CL*13
01904      PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT.                       CL*13
01905      GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.                           CL*13
01906                                                                      CL*13
01907      EJECT                                                           CL*13
01908  8050-ENDBR-ELACTQ.                                                  CL*13
01909                                                                      CL*13
01910      EXEC CICS ENDBR                                                 CL*13
01911          DATASET   (ELACTQ-FILE-ID)                                  CL*13
01912      END-EXEC.                                                       CL*13
01913                                                                      CL*13
01914  8050-EXIT.                                                          CL*13
01915      EXIT.                                                           CL*13
01916                                                                      CL*13
01917  8060-ENDBR-ELTRLR.                                                  CL*13
01918                                                                      CL*13
01919      EXEC CICS ENDBR                                                 CL*13
01920          DATASET   (ELTRLR-FILE-ID)                                  CL*13
01921      END-EXEC.                                                       CL*13
01922                                                                      CL*13
01923  8060-EXIT.                                                          CL*13
01924      EXIT.                                                           CL*13
01925                                                                      CL**4
01926      EJECT                                                           CL**4
01927  8100-SEND-INITIAL-MAP.                                           EL143
01928      MOVE SAVE-DATE              TO DATEO.                        EL143
01929      MOVE EIBTIME                TO TIME-IN.                      EL143
01930      MOVE TIME-OUT               TO TIMEO.                        EL143
01931      MOVE LOW-VALUES             TO MAINTO.                       EL143
01932      MOVE -1                     TO MAINTL.                       EL143
01933      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL143
01934                                                                   EL143
01935  8150-SEND-INITIAL-MAP.                                           EL143
01936                                                                      CL**2
01937      IF PI-USES-PAID-TO                                              CL**3
01938         MOVE 'PAID  TO :'       TO PDTHHD1O                          CL*12
01939         MOVE 'PAID  TO       :' TO PDTHHD2O.                         CL**3
01940                                                                      CL**3
01941      EXEC CICS SEND                                               EL143
01942          MAP      (MAP-NAME)                                      EL143
01943          MAPSET   (MAPSET-NAME)                                   EL143
01944          FROM     (EL143AO)                                       EL143
01945          ERASE                                                    EL143
01946          CURSOR                                                   EL143
01947      END-EXEC.                                                    EL143
01948                                                                   EL143
01949      GO TO 9100-RETURN-TRAN.                                      EL143
01950                                                                   EL143
01951  8200-SEND-DATAONLY.                                              EL143
01952                                                                      CL**2
01953      MOVE SAVE-DATE              TO DATEO.                        EL143
01954      MOVE EIBTIME                TO TIME-IN.                      EL143
01955      MOVE TIME-OUT               TO TIMEO.                        EL143
01956      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL143
01957                                                                      CL**3
01958      IF PI-USES-PAID-TO                                              CL**3
01959         MOVE 'PAID  TO :'       TO PDTHHD1O                          CL*12
01960         MOVE 'PAID  TO       :' TO PDTHHD2O.                         CL**3
01961                                                                   EL143
01962      EXEC CICS SEND                                               EL143
01963          MAP      (MAP-NAME)                                      EL143
01964          MAPSET   (MAPSET-NAME)                                   EL143
01965          FROM     (EL143AO)                                       EL143
01966          DATAONLY                                                 EL143
01967          CURSOR                                                   EL143
01968      END-EXEC.                                                    EL143
01969                                                                   EL143
01970      GO TO 9100-RETURN-TRAN.                                      EL143
01971                                                                   EL143
01972      EJECT                                                        EL143
01973  8300-SEND-TEXT.                                                  EL143
01974                                                                      CL**2
01975      EXEC CICS SEND TEXT                                          EL143
01976          FROM     (LOGOFF-TEXT)                                   EL143
01977          LENGTH   (LOGOFF-LENGTH)                                 EL143
01978          ERASE                                                    EL143
01979          FREEKB                                                   EL143
01980      END-EXEC.                                                    EL143
01981                                                                   EL143
01982      EXEC CICS RETURN                                             EL143
01983      END-EXEC.                                                    EL143
01984                                                                   EL143
01985      EJECT                                                        EL143
01986  8500-NOT-FOUND.                                                     CL**4
01987                                                                      CL**4
01988      MOVE ER-0282                TO  EMI-ERROR.                      CL**4
01989      MOVE -1                     TO  MAINTL.                         CL**4
01990      PERFORM 9900-ERROR-FORMAT.                                      CL**4
01991      GO TO 8200-SEND-DATAONLY.                                       CL**4
01992                                                                      CL**4
01993  8800-UNAUTHORIZED-ACCESS.                                        EL143
01994                                                                      CL**2
01995      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL143
01996      GO TO 8300-SEND-TEXT.                                        EL143
01997                                                                   EL143
01998  8810-PF23.                                                       EL143
01999                                                                      CL**2
02000      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL143
02001      MOVE XCTL-005               TO PGM-NAME.                     EL143
02002      GO TO 9300-XCTL.                                             EL143
02003                                                                   EL143
02004  9100-RETURN-TRAN.                                                EL143
02005                                                                      CL**2
02006      MOVE EMI-ERROR-NUMBER (1)      TO PI-LAST-ERROR-NO.             CL*12
02007      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.      EL143
02008      EXEC CICS RETURN                                             EL143
02009          TRANSID    (TRANS-ID)                                    EL143
02010          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL143
02011          LENGTH     (PI-COMM-LENGTH)                              EL143
02012      END-EXEC.                                                    EL143
02013                                                                   EL143
02014  9200-RETURN-MAIN-MENU.                                           EL143
02015                                                                      CL**2
02016      MOVE XCTL-126               TO PGM-NAME.                     EL143
02017      GO TO 9300-XCTL.                                             EL143
02018                                                                   EL143
02019  9300-XCTL.                                                       EL143
02020                                                                      CL**2
02021      EXEC CICS XCTL                                               EL143
02022          PROGRAM    (PGM-NAME)                                    EL143
02023          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL143
02024          LENGTH     (PI-COMM-LENGTH)                              EL143
02025      END-EXEC.                                                    EL143
02026                                                                   EL143
02027  9400-CLEAR.                                                      EL143
02028                                                                      CL**2
02029      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL143
02030      GO TO 9300-XCTL.                                             EL143
02031                                                                   EL143
02032  9500-PF12.                                                       EL143
02033                                                                      CL**2
02034      MOVE XCTL-010               TO PGM-NAME.                     EL143
02035      GO TO 9300-XCTL.                                             EL143
02036                                                                   EL143
02037  9600-PGMID-ERROR.                                                EL143
02038                                                                      CL**2
02039      EXEC CICS HANDLE CONDITION                                   EL143
02040          PGMIDERR    (8300-SEND-TEXT)                             EL143
02041      END-EXEC.                                                    EL143
02042                                                                   EL143
02043      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL143
02044      MOVE ' '                    TO PI-ENTRY-CD-1.                EL143
02045      MOVE XCTL-005               TO PGM-NAME.                     EL143
02046      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL143
02047      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL143
02048      GO TO 9300-XCTL.                                             EL143
02049                                                                   EL143
02050  9700-DATE-LINK.                                                  EL143
02051                                                                      CL**2
02052      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL*12
02053      EXEC CICS LINK                                               EL143
02054          PROGRAM    (PGM-NAME)                                    EL143
02055          COMMAREA   (DATE-CONVERSION-DATA)                        EL143
02056          LENGTH     (DC-COMM-LENGTH)                              EL143
02057      END-EXEC.                                                    EL143
02058                                                                   EL143
CIDMOD 9870-OUTPUT-ACTIVITY-RECORD.                                          000
CIDMOD                                                                       000
CIDMOD     EXEC CICS GETMAIN                                                 000
CIDMOD         SET (ADDRESS OF DAILY-ACTIVITY-RECORD)                        000
CIDMOD         LENGTH (25)                                                   000
CIDMOD         INITIMG (WS-BLANK)                                            000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.             000
CIDMOD     MOVE ELMSTR-KEY             TO DA-KEY.                            000
CIDMOD     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.                 000
CIDMOD     IF WS-PAY-TYPE EQUAL '7'                                          000
CIDMOD         MOVE 'V'                TO DA-RECORD-TYPE                     000
CIDMOD     ELSE                                                              000
CIDMOD         MOVE 'P'                TO DA-RECORD-TYPE.                    000
CIDMOD                                                                       000
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD         NOTOPEN (9870-NOTOPEN)                                        000
CIDMOD         DUPREC (9870-EXIT)                                            000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     EXEC CICS WRITE                                                   000
CIDMOD         DATASET ('DLYACTV')                                           000
CIDMOD         RIDFLD (DA-KEY)                                               000
CIDMOD         FROM (DAILY-ACTIVITY-RECORD)                                  000
CIDMOD         LENGTH (25)                                                   000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.                000
CIDMOD     GO TO 9870-EXIT.                                                  000
CIDMOD                                                                       000
CIDMOD 9870-NOTOPEN.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE '2955'                 TO EMI-ERROR.                         000
CIDMOD     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.                000
CIDMOD                                                                       000
CIDMOD 9870-EXIT.                                                            000
CIDMOD     EXIT.                                                             000
CIDMOD                                                                       000
02059  9900-ERROR-FORMAT.                                               EL143
02060                                                                      CL**2
02061      IF NOT EMI-ERRORS-COMPLETE                                   EL143
02062          MOVE LINK-001           TO PGM-NAME                      EL143
02063          EXEC CICS LINK                                           EL143
02064              PROGRAM    (PGM-NAME)                                EL143
02065              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL143
02066              LENGTH     (EMI-COMM-LENGTH)                         EL143
02067          END-EXEC.                                                EL143
02068                                                                   EL143
02069  9990-ABEND.                                                      EL143
02070                                                                      CL**2
02071      MOVE LINK-004               TO PGM-NAME.                     EL143
02072      MOVE DFHEIBLK               TO EMI-LINE1.                       CL*12
02073                                                                   EL143
02074      EXEC CICS LINK                                               EL143
02075          PROGRAM   (PGM-NAME)                                     EL143
02076          COMMAREA  (EMI-LINE1)                                    EL143
02077          LENGTH    (72)                                           EL143
02078      END-EXEC.                                                    EL143
02079                                                                   EL143
02080      MOVE -1                     TO MAINTL.                       EL143
02081      GO TO 8200-SEND-DATAONLY.                                    EL143
02082                                                                   EL143
02083  9995-SECURITY-VIOLATION.                                         EL143
02084      COPY ELCSCTP.                                                   CL*12
02085                                                                   EL143
02086  9995-EXIT.                                                       EL143
02087       EXIT.                                                       EL143
