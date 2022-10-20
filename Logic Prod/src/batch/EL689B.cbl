       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL689B.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS
      *   This program reads the ERARCH file sequentially and
      *    scans for records with resend  letters to be printed.
      *    Once a record has been selected the ELLETR file is read
      *    to find the letter to be used. Next, all the possible
      *    variables are resolved. Next, the program reads through
      *    the letter and substitutes the variables in the letter
      *    with the ones that were resolved. Finally, the finalized
      *    letter is written to the ERARCT file and the ERARCH record
      *    is updated with a resend print date.                   
      *   This program was modeled from EL689.            
      *
101705******************************************************************
101705*                   C H A N G E   L O G
101705*
101705* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101705*-----------------------------------------------------------------
101705*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101705* EFFECTIVE    NUMBER
101705*-----------------------------------------------------------------
101705* 101705    2004072800004  PEMA  NEW PROGRAM
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
071811* 071811  CR2011050600001  PEMA  STOP RESENDS WHEN CANCELLED
082211* 082211  IR2011081800001  PEMA  CORRECT NH01 LETTERS
020812* 020812  IR2012020800001  PEMA  CORRECT MISSING USER RECORD
031512* 031512  IF2012031400001  AJRA  ADD CRED BENE ADDR (LIKE ONLINE HAS)
062017* 062017 CR2015091000001   PEMA  ADD TN INTEREST PROCESSING
101705******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT ARCH-IN      ASSIGN TO SYS010.

031011     SELECT ARCH-EXT-OUT ASSIGN TO SYS011
031011        ORGANIZATION IS LINE SEQUENTIAL.
031011
031011     SELECT ERARCTI      ASSIGN TO ERARCTI
031011                         ACCESS IS DYNAMIC
031011                         ORGANIZATION IS INDEXED
031011                         FILE STATUS IS ERARCTI-FILE-STATUS
031011                         RECORD KEY IS LI-CONTROL-PRIMARY.

           SELECT ERACCT       ASSIGN TO ERACCT
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS AM-CONTROL-PRIMARY.

           SELECT ELCNTL       ASSIGN TO ELCNTL
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELCNTL-FILE-STATUS
                               RECORD KEY IS CF-CONTROL-PRIMARY.

           SELECT ELCERT       ASSIGN TO ELCERT
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELCERT-FILE-STATUS
                               RECORD KEY IS CM-CONTROL-PRIMARY.

           SELECT ERCOMP       ASSIGN TO ERCOMP
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.

           SELECT ERARCH       ASSIGN TO ERARCH
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERARCH-FILE-STATUS
                               RECORD KEY IS LA-CONTROL-PRIMARY.

           SELECT ERARCT       ASSIGN TO ERARCT
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERARCT-FILE-STATUS
                               RECORD KEY IS LT-CONTROL-PRIMARY.

           SELECT ERPNDB       ASSIGN TO ERPNDB2
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPNDB-FILE-STATUS
                               RECORD KEY IS PB-CONTROL-BY-ACCOUNT.

           SELECT ERMAIL       ASSIGN TO ERMAIL
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERMAIL-FILE-STATUS
                               RECORD KEY IS MA-CONTROL-PRIMARY.

           SELECT ERCHEK       ASSIGN TO ERCHEK
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCHEK-FILE-STATUS
                               RECORD KEY IS CH-CONTROL-PRIMARY.

           SELECT ERPYAJ       ASSIGN TO ERPYAJ
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPYAJ-FILE-STATUS
                               RECORD KEY IS PY-CONTROL-PRIMARY.

           SELECT ELLETR       ASSIGN TO ELLETR
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELLETR-FILE-STATUS
                               RECORD KEY IS TX-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

031011 FD  ARCH-EXT-OUT
031011     RECORDING MODE F
031011     BLOCK CONTAINS 0.
031011
031011 01  ARCH-OUT-RECORD             PIC X(73).

       FD  ARCH-IN
           RECORDING MODE F
           BLOCK CONTAINS 0.
           
       01  ARCH-IN-RECORD              PIC X(250).
       
031011 FD  ERARCTI.
031011                                 COPY ERCARCT
031011      REPLACING ==LETTER-ARCHIVE-TEXT==
031011         BY ==LETTER-ARCHIVE-INPUT==, 
031011      LEADING ==LT== BY ==LI==.

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  ELCNTL.
                                       COPY ELCCNTL.

       FD  ELCERT.
                                       COPY ELCCERT.

       FD  ERCOMP.
                                       COPY ERCCOMP.

       FD  ERARCH.
                                       COPY ERCARCH.

       FD  ERARCT.
                                       COPY ERCARCT.

       FD  ERPNDB.
                                       COPY ERCPNDB.

       FD  ERMAIL.
                                       COPY ERCMAIL.

       FD  ERCHEK.
                                       COPY ERCCHEK.

       FD  ERPYAJ.
                                       COPY ERCPYAJ.

       FD  ELLETR.
                                       COPY ELCTEXT.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

00029  WORKING-STORAGE SECTION.
00033  77  FILLER  PIC X(32) VALUE '********************************'.
00034  77  FILLER  PIC X(32) VALUE '*    EL689 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.037 *********'.
       77  WS-CURRENT-BIN-DATE         PIC XX  VALUE LOW-VALUES.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERPNDB-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERARCH-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
031011 77  ERARCTI-FILE-STATUS         PIC XX  VALUE LOW-VALUES.
       77  ERARCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELLETR-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERMAIL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERCHEK-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERPYAJ-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS     VALUE 'N'.
       77  WS-FORM-TO-RESEND           PIC X(4) VALUE SPACES.
       77  WS-FORM-TO-CREATE           PIC X(4) VALUE SPACES.
       77  ARCH-IN-CNT                 PIC 9(9) VALUE ZEROS.
       77  WS-WORK-NDX                 PIC 9(5) VALUE ZEROS.
       77  WS-SAVE-ERARCH-KEY          PIC X(5) VALUE LOW-VALUES.
       77  WS-DISP-ARCH-NO             PIC 9(11) VALUE ZEROS.
       77  WS-DISP-SEQ-NO              PIC 9(9)  VALUE ZEROS.
       77  WS-SAVE-ARCH-NO             PIC S9(9)  COMP VALUE +0.
       77  WS-FOUND-PROMPT-SW          PIC X     VALUE ' '.
           88  FOUND-PROMPT               VALUE 'Y'.
       77  N1                          PIC S999 VALUE +0 COMP-3.
       77  WS-FIND-SW                  PIC X  VALUE ' '.
           88  I-FOUND-IT                  VALUE 'Y'.
           88  I-DIDNT-FIND-IT             VALUE 'N'.
           88  I-AM-FINISHED               VALUE 'F'.
071811 77  WS-CERT-STATUS-SW           PIC X.
071811     88  CERT-CANCELLED             VALUE 'C'.
       77  WS-COV-CNT                  PIC S999 COMP-3 VALUE +0.
       77  WS-REPLY-DATE               PIC XX  VALUE LOW-VALUES.
00037
00038  01  W-WORK-AREAS.
00039      12  FILLER                  PIC  X(18)
00040                                       VALUE 'PROGRAM WORK AREA:'.
00041
00042      12  W-ASKTIME-CTR           PIC S9(04)  COMP   VALUE +0.
00043      12  W-CWA-NDX               PIC S9(04)  COMP   VALUE +0.
00044      12  W-DISPLAY-NDX           PIC S9(04)  COMP   VALUE +0.
00045      12  W-FIRST-BAD-VARIABLE    PIC S9(04)  COMP   VALUE +0.
00046      12  W-NDX                   PIC S9(04)  COMP   VALUE +0.
00047      12  W-NDX2                  PIC S9(04)  COMP   VALUE +0.
00048      12  W-SEQ-CTR               PIC S9(04)  COMP   VALUE +0.
00049      12  W-TS-ITEM               PIC S9(04)  COMP   VALUE +0.
00050      12  W-TOTAL-LINE-LENGTH     PIC S9(04)  COMP   VALUE +0.
00051      12  W-WORK-NDX              PIC S9(04)  COMP   VALUE +0.
00052
00053      12  W-ADJUST-SHORT          PIC S9(03) VALUE +0   COMP-3.
00054      12  W-DIFFERENCE            PIC S9(07)V9(02)
00055                                             VALUE +0   COMP-3.
           12  WS-CURRENT-TIME         PIC S9(7)   VALUE ZERO.
00056      12  W-INITIAL-COLUMN        PIC S9(03) VALUE +0   COMP-3.
00057      12  W-LAST-COLUMN           PIC S9(03) VALUE +70 COMP-3.
00058      12  W-LAST-SQUEEZED-SPACE   PIC S9(03) VALUE +0   COMP-3.
00059      12  W-LAST-TX-SPACE         PIC S9(03) VALUE +0   COMP-3.
00060      12  W-LAST-WC-SPACE         PIC S9(03) VALUE +0   COMP-3.
00061      12  W-LINE-COUNT            PIC S9(03) VALUE +0   COMP-3.
00062      12  W-LINE-INDENT-1         PIC  9(02) VALUE 0    COMP-3.
00063      12  W-LINE-INDENT-2         PIC  9(02) VALUE 0    COMP-3.
00064      12  W-LINE-INDENT-3         PIC  9(02) VALUE 0    COMP-3.
00065      12  W-LINE-INDENT-4         PIC  9(02) VALUE 0    COMP-3.
00066      12  W-LINE-INDENT-5         PIC  9(02) VALUE 0    COMP-3.
00067      12  W-MAX-LINES-PER-PAGE    PIC  9(02) VALUE 56   COMP-3.
00068      12  W-NEXT-INDENT           PIC  9(02) VALUE 0    COMP-3.
00069      12  W-PAGE                  PIC S9(02) VALUE +0   COMP-3.
00070      12  W-PAGE-LINE             PIC S9(03) VALUE +0   COMP-3.
00071      12  W-PARAGRAPH-INDENT      PIC  9(02) VALUE 0    COMP-3.
00072      12  W-ROLL-COUNTER          PIC S9(03) VALUE +0   COMP-3.
00073      12  W-START-COLUMN          PIC S9(03) VALUE +1   COMP-3.
00074      12  W-TEMP-CURRENT-LINE     PIC S9(03) VALUE +0   COMP-3.
00075      12  W-TOO-FAR               PIC S9(03) VALUE +71 COMP-3.
00076      12  W-TOP-MARGIN            PIC  9(02) VALUE 0    COMP-3.
00077      12  W-TOTAL-TX-LINES        PIC S9(03) VALUE +0   COMP-3.
00078      12  W-TS-GROUP-WORK         PIC  9(05) VALUE 0    COMP-3.
00079      12  W-WORK-INDENT           PIC  9(02) VALUE 0    COMP-3.
00080      12  W-WORK-AMOUNT           PIC S9(09)V9(02)
00081                                             VALUE +0   COMP-3.
00082
00083      12  W-DATA-SOURCE           PIC  9(01) VALUE 0.
00084      12  W-LABEL-SOURCE          PIC  9(01) value 0.
00085          88  W-LABEL-SOURCE-VALID           VALUE 1 THRU 7.
00086      12  W-LABEL-JOINT-NAME      PIC  X(30) VALUE SPACES.
00087      12  W-LAST-ERROR            PIC  9(04) VALUE 9999.
00088      12  W-NUMB-LABEL-LINES      PIC  9(01) VALUE 0.
00089      12  W-PRINT-CONTROL         PIC  9(02) VALUE 0.
00090      12  W-SAVE-REVISION         PIC  9(03) value 0.
00092      12  W-DISPLAY-3             PIC  9(03) value 0.
00093      12  W-DISPLAY-8             PIC  9(08) value 0.
00094      12  W-DISPLAY-7             PIC  9(07) value 0.
00095      12  W-EDIT-2-5-S            PIC  Z9.9(05).
00096      12  W-EDIT-3-0              PIC  ZZ9.
00097      12  W-EDIT-7-2              PIC  $$,$$$,$$9.99.
00098      12  W-EDIT-7-2-NEGATIVE     PIC  $$,$$$,$$9.99-.
00099      12  W-EDIT-9-2              PIC  $$$$,$$$,$$9.99.
00100      12  W-EDIT-12-2             PIC  $$$$,$$$,$$$,$$9.99.
031504
031504     12  WS-ACCT-PKEY-MATCH-SW   PIC  X(01) VALUE SPACE.
031504         88  PRIOR-MATCH-ACCT-PKEY          VALUE 'Y'.
00101
00102      12  W-ARCH-SUPPRESS         PIC ZZZZZZZ9.
00103      12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
00104                                  PIC  X(08).
00106      12  W-COMP-WORK-AREA.
00107          16  W-CWA-CHAR OCCURS 10 TIMES
00108                                  PIC  X(01).
00109      12  W-INCOMING-ARCHIVE      PIC  X(08).
00110      12  W-INCOMING-ARCH-NO REDEFINES W-INCOMING-ARCHIVE
00111                                  PIC  9(08).
00112
00113      12  W-BATCH-BREAKDOWN.
00114          16  W-BATCH-CONTROL     PIC S9(08) COMP.
00115          16  W-BATCH-SEQ         PIC S9(04) COMP.
00116      12  W-BEN-HOLD              PIC  X(02).
00117      12  W-BENEFIT-WORK          PIC  X(03).
00118      12  W-BEN-R REDEFINES W-BENEFIT-WORK.
00119          16  W-ELIM-DAYS         PIC  X(02).
00120          16  FILLER              PIC  X(01).
00121      12  W-CALL-PGM              PIC  X(08).
00122      12  W-CURRENT-SAVE          PIC  X(02) VALUE SPACES.
00123
00124      12  FILLER                  PIC  X(09) VALUE 'LINE CNTL'.
00125      12  W-LINE-CONTROL-RECORD.
00126          16  W-LC-LINE-WIDTH     PIC  9(02).
00127          16  FILLER              PIC  X(01).
00128          16  W-LC-PARAGRAPH-INDENT
00129                                  PIC  9(02).
00130          16  FILLER              PIC  X(01).
00131          16  W-LC-MAX-LINES-PER-PAGE
00132                                  PIC  9(02).
00133          16  FILLER              PIC  X(01).
00134          16  W-LC-LINE-ADJUST    PIC  9(02).
00135          16  FILLER              PIC  X(01).
00136          16  W-LC-LINE-INDENT-1  PIC  9(02).
00137          16  FILLER              PIC  X(01).
00138          16  W-LC-LINE-INDENT-2  PIC  9(02).
00139          16  FILLER              PIC  X(01).
00140          16  W-LC-LINE-INDENT-3  PIC  9(02).
00141          16  FILLER              PIC  X(01).
00142          16  W-LC-LINE-INDENT-4  PIC  9(02).
00143          16  FILLER              PIC  X(01).
00144          16  W-LC-LINE-INDENT-5  PIC  9(02).
00145          16  FILLER              PIC  X(01).
00146          16  W-LC-TOP-MARGIN     PIC  9(02).
00147          16  FILLER              PIC  X(01).
00148          16  W-LC-CASE-IND       PIC  X(01).
00149              88  W-LC-USE-BOTH-CASES        VALUE 'Y'.
00150          16  FILLER              PIC  X(01).
00151          16  W-LC-PAGE-IND       PIC  X(01).
00152              88  W-LC-CREATE-PAGES          VALUE 'Y'.
00154      12  W-DATE-WORK             PIC  9(07).
00155      12  W-DT-REDEF REDEFINES W-DATE-WORK.
00156          16  FILLER              PIC  X(02).
00157          16  W-DT-WORK           PIC  9(05).
00158
00159      12  W-DEEDIT-FIELD          PIC  X(15).
00160      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD PIC S9(15).
00161
00162      12  W-EDIT-DATE-1.
00163          16  W-ED1-MM            PIC  X(02).
00164          16  FILLER              PIC  X(01) VALUE '/'.
00165          16  W-ED1-DD            PIC  X(02).
00166          16  FILLER              PIC  X(01) VALUE '/'.
00167          16  W-ED1-YY            PIC  X(02).
00168
00169      12  W-EDIT-DATE-2.
00170          16  W-ED2-DD            PIC  X(02).
00171          16  FILLER              PIC  X(01) VALUE '/'.
00172          16  W-ED2-MM            PIC  X(02).
00173          16  FILLER              PIC  X(01) VALUE '/'.
00174          16  W-ED2-YY            PIC  X(02).
00175
00176      12  W-LABEL-HOLD-AREA.
00177          16  W-LABEL-LINES OCCURS 6 TIMES.
00178              20  W-LABEL-ZIP.
00179                  24  W-LABEL-1ST-ZIP  PIC  X(05).
00180                  24  W-LABEL-DASH     PIC  X(01).
00181                  24  W-LABEL-2ND-ZIP  PIC  X(04).
00182              20  W-LAB-CAN-POSTAL-CODES REDEFINES W-LABEL-ZIP.
00183                  24  W-LAB-CAN-POSTAL-CD-1
00184                                       PIC  X(03).
00185                  24  W-LAB-CAN-DASH   PIC  X(01).
00186                  24  W-LAB-CAN-POSTAL-CD-2
00187                                       PIC  X(03).
00188                  24  W-LAB-CAN-FILLER PIC  X(03).
00189              20  FILLER               PIC  X(10).
00190              20  W-LAST-ZIP.
00191                  24  W-LAST-1ST-ZIP   PIC  X(05).
00192                  24  W-LABEL-DASH-LAST
00193                                       PIC  X(01).
00194                  24  W-LAST-2ND-ZIP   PIC  X(04).
00195              20  W-LAST-CAN-POSTAL-CODES REDEFINES W-LAST-ZIP.
00196                  24  W-LAST-CAN-POSTAL-CD-1
00197                                       PIC  X(03).
00198                  24  W-LAST-CAN-DASH  PIC  X(01).
00199                  24  W-LAST-CAN-POSTAL-CD-2
00200                                       PIC  X(03).
00201                  24  W-LAST-CAN-FILLER
00202                                       PIC  X(03).
00203
00204      12  W-LAST-CHAR             PIC  X(01).
00205          88  W-LAST-CHAR-PUNC    VALUE '-' '/'.
00206      12  W-LAST-SQ-CHAR          PIC  X(01).
00207
00208      12  W-LINE-NUM.
00209          16  W-LINE1             PIC  X(01).
00210          16  W-LINE23            PIC  9(02).
00211      12  W-LIN-NUM REDEFINES W-LINE-NUM
00212                                  PIC  9(03).
00213
00214      12  W-NAME-LAST             PIC  X(15).
00215      12  W-NAME-FIRST.
00216          16  W-NAME-FIRST-INIT   PIC  X(01).
00217          16  W-NAME-FIRST-REMAIN PIC  X(14).
00218      12  W-NAME-MIDDLE.
00219          16  FILLER              PIC  X(01).
00220          16  W-NAME-MIDDLE-2     PIC  X(01).
00221          16  FILLER              PIC  X(13).
00222
00223      12  W-PAGE-PRT.
00224          16  FILLER              PIC  X(34) VALUE SPACES.
00225          16  W-PAGE-NUMBER       PIC  Z9.
00226          16  FILLER              PIC  X(01) VALUE '.'.
00227          16  FILLER              PIC  X(33) VALUE SPACES.
00228
00229      12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.
00230      12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.
00231          16  FILLER              PIC  9(01).
00232          16  W-PHI-AREA          PIC  9(03).
00233          16  W-PHI-PFX           PIC  9(03).
00234          16  W-PHI-SFX           PIC  9(04).
00235      12  W-PHONE-OUT.
00236          16  W-PO-AREA           PIC  X(03).
00237          16  FILLER              PIC  X(01) VALUE '-'.
00238          16  W-PO-PFX            PIC  X(03).
00239          16  FILLER              PIC  X(01) VALUE '-'.
00240          16  W-PO-SFX            PIC  X(04).
00241
00242      12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
00243      12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
00244      12  W-SAVE-PLAN             PIC  X(02) VALUE SPACES.
00245
00246      12  W-SINGLE-LINE           PIC  X(70).
00247      12  W-SINGLE-LINE-BY REDEFINES W-SINGLE-LINE.
00248          16  ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2 NDXA
00249                                  PIC  X(01).
00250
00251      12  W-SQUEEZED-LINE.
00252          16  W-SQ-CHAR OCCURS 70 TIMES
00253                        INDEXED BY W-SQ-NDX
00254                                  PIC  X(01).
00255
00256      12  W-TIME-IN               PIC S9(07).
00257      12  W-TIME-OUT-R REDEFINES W-TIME-IN.
00258          16  FILLER              PIC  X(01).
00259          16  W-TIME-OUT          PIC  99V9(02).
00260          16  FILLER              PIC  X(02).
00261
00262      12  W-TS-NAME-TEXT.
00263          16  W-TS-ID-TEXT        PIC  X(04) VALUE '104A'.
00264          16  W-TS-ID-TIME
00265              REDEFINES W-TS-ID-TEXT
00266                                  PIC S9(07) COMP-3.
00267          16  W-TS-TERM-TEXT.
00268              20 W-TS-TERM-PREFIX PIC  X(02).
00269              20 FILLER           PIC  X(02).
00270      12  W-TS-NAME-SCREEN.
00271          16  FILLER              PIC  X(04) VALUE '689X'.
00272          16  W-TS-TERM-SCREEN    PIC  X(04).
00273
00274      12  W-WORK-LINE.
00275          16  W-WORK-CHAR OCCURS 70 TIMES
00276                        INDEXED BY W-WC-NDX
00277                                   W-WC-NDX2
00278                                   W-WC-NDX3
00279                                  PIC  X(01).
00280
00281      12  W-WORK-ZIP-NUMERIC      PIC  9(09).
00282      12  W-WORK-ZIP REDEFINES W-WORK-ZIP-NUMERIC.
00283          16  W-WORK-ZIP5         PIC  X(05).
00284          16  W-WORK-ZIP4         PIC  X(04).
00285      12  W-CANADIAN-POSTAL-CODES REDEFINES W-WORK-ZIP-NUMERIC.
00286          16  W-CAN-POSTAL-CD-1.
00287              20  FILLER          PIC  X(01).
00288                  88 W-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00289              20  FILLER          PIC  X(02).
00290          16  W-CAN-POSTAL-CD-2   PIC  X(03).
00291          16  W-FILLER            PIC  X(03).

00293  01  W-Z-CONTROL-DATA.
00294      12  W-NUMBER-OF-COPIES  PIC  9.
00295      12  FILLER              PIC  X.
00296      12  W-DAYS-TO-FOLLOW-UP PIC  999.
00297      12  FILLER              PIC  X.
00298      12  W-DAYS-TO-RESEND    PIC  999.
00299      12  FILLER              PIC  X.
00300      12  W-FORM-TO-RESEND    PIC  X(4).
00301      12  FILLER              PIC  X.
00302      12  W-PROMPT-LETTER     PIC  X.
00303      12  FILLER              PIC  X.
           12  W-ENCLOSURE-CODE    PIC  XXX.
           12  F                   PIC  X.
           12  W-AUTO-CLOSE-IND    PIC  X.
           12  F                   PIC  X.
           12  W-LETTER-TO-BENE    PIC  X.

00311  01  W-SWITCH-INDICATORS-AREA.
00312      12  FILLER                  PIC  X(16)
00313                                       VALUE 'PROGRAM SWITCHS:'.
00314      12  W-ADDRESS-ONLY-SW       PIC  X(01) VALUE ' '.
00315          88  W-ADDRESS-ONLY      VALUE 'Y'.
00316          88  W-FULL-DATA         VALUE ' '.
00317      12  W-ACCT-BROWSE-STARTED-SW
00318                                  PIC  X(01) VALUE 'N'.
00319          88  W-ACCT-BROWSE-STARTED          VALUE 'Y'.
00320      12  W-ARCT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
00321      12  W-CERT-FOUND-SW         PIC  X(01) VALUE 'N'.
00322          88  W-CERT-FOUND                   VALUE 'Y'.
00323      12  W-CHARACTER-TYPE        PIC  X(01).
00324          88  W-END-OF-SENTENCE   VALUE '.' '?' '!'.
00325          88  W-PUNCTUATION       VALUE '.' '?' '!' ',' ';'.
00326          88  W-SPACE             VALUE ' '.
00327      12  W-COMP-BROWSE-SW        PIC  X(01) VALUE 'N'.
00328          88  W-COMP-BROWSE-STARTED          VALUE 'Y'.
00329      12  W-DATA-SHRINK-IND       PIC  X(01) VALUE SPACES.
00330          88  W-DATA-SHRINKING    VALUE 'Y'.
00331      12  W-END-OF-SENTENCE-IND   PIC  X(01) VALUE SPACES.
00332          88  W-END-OF-SENTENCE-WORKING      VALUE 'Y'.
00333      12  W-FIRST-BAD-VARIABLE-IND
00334                                  PIC  X(01) VALUE SPACES.
00335          88  W-FIRST-BAD-VARIABLE-FOUND     VALUE 'Y'.
00336      12  W-FIRST-CHAR-FOUND-IND  PIC  X(01) VALUE SPACES.
00337          88  W-FIRST-CHAR-FOUND             VALUE 'Y'.
00338          88  W-FIRST-CHAR-NOT-FOUND         VALUE SPACES.
00339      12  W-FORM-CHANGED-IND      PIC  X(01) VALUE SPACES.
00340          88  W-FORM-CHANGED                 VALUE 'Y'.
00341      12  W-FORM-SQUEEZE-IND      PIC  X(01) VALUE SPACES.
00342          88  W-FORM-SQUEEZE-ON              VALUE 'Y'.
00343          88  W-FORM-SQUEEZE-OFF             VALUE ' '.
00344      12  W-HOLD-IND              PIC  X(01) VALUE SPACES.
00345          88  W-HOLD-ON                      VALUE 'Y'.
00346          88  W-HOLD-OFF                     VALUE ' '.
00347      12  W-INDIVIDUAL-DATA-SW    PIC  X(01) VALUE ' '.
00348          88  W-INDIVIDUAL-DATA-COMPLETED    VALUE 'Y'.
00349          88  W-INDIV-DATA-NOT-COMPLETED     VALUE ' '.
00350      12  W-KEY-FIELDS-CHANGED-IND
00351                                  PIC  X(01) VALUE ' '.
00352          88  W-KEY-FIELDS-CHANGED           VALUE 'Y'.
00353          88  W-KEY-FIELDS-NOT-CHANGED       VALUE ' '.
00354      12  W-LAST-ONE              PIC  X(01) VALUE HIGH-VALUES.
00355          88  W-LAST-ONE-A-SPACE  VALUE ' '.
00356      12  W-LINE-SQUEEZE-IND      PIC  X(01) VALUE SPACES.
00357          88  W-NEW-PARAGRAPH                VALUE 'P' 'Q' 'R' 'S'
00358                                                    'T' 'U'.
00359          88  W-CONTINUE-PARAGRAPH           VALUE 'C' 'D' 'E' 'F'
00360                                                    'G' 'H'.
00361          88  W-FORM-CONTROL-LINE            VALUE 'K'.
00362          88  W-DO-NOT-ADJUST                VALUE 'N'.
00363          88  W-ADJUST-TO-LINE-LENGTH        VALUE 'A'.
00364          88  W-AS-IS                        VALUE 'A' 'N'.
00365          88  W-CONTINUE-PREVIOUS-PROCESS    VALUE ' ' '1' '2' '3'
00366                                                    '4' '5'.
00367
00368      12  W-PNDB-FOUND-SW         PIC  X(01) VALUE 'N'.
00369          88  W-PNDB-FOUND                   VALUE 'Y'.
00370      12  W-REMAINING-VAR-SW      PIC  X(01) VALUE SPACES.
00371          88  W-REMAINING-VAR-FOUND          VALUE 'Y'.
00372      12  W-REVERSE-DATE-SW       PIC  X(01) VALUE SPACES.
00373          88  W-REVERSE-DATE                 VALUE 'Y'.
00374      12  W-TEXT-BROWSED-SW       PIC  X(01) VALUE 'N'.
00375          88  W-TEXT-BROWSE-STARTED          VALUE 'Y'.
00376          88  W-TEXT-BROWSE-NOT-STARTED      VALUE 'N'.
00377                                  EJECT
031504 01  WS-SAVE-AREA.

031504     12  WS-SAV-AM-DEFN-1.
031504         16  WS-SAV-AM-AGT-COMMS OCCURS 10 TIMES.
031504             20  WS-SAV-AM-AGT             PIC  X(10).
031504             20  WS-SAV-AM-COM-TYP         PIC  X(01).
031504             20  FILLER                    PIC  X(15).

031504     12  WS-SAV-AM-REMIT-TO                PIC  9(02).
072308     12  WS-SAV-AM-CSR-CODE                PIC  X(04).

031504     12  WS-SAV-AM-CARRIER                 PIC  X(01).
031504     12  WS-SAV-AM-ACCOUNT                 PIC  X(10).
031504     12  WS-SAV-AM-CONTROL-NAME            PIC  X(30).
031504     12  WS-SAV-AM-NAME                    PIC  X(30).
031504     12  WS-SAV-AM-PERSON                  PIC  X(30).
031504     12  WS-SAV-AM-ADDRS                   PIC  X(30).
031504     12  WS-SAV-AM-CITY                    PIC  X(30).
031512     12  WS-SAV-AM-ERACCT-ACCOUNT          PIC  X(10).

031504     12  WS-SAV-AM-ZIP.
031504         16  WS-SAV-AM-ZIP-PRIME.   
031504             20  WS-SAV-AM-ZIP-PRI-1ST     PIC  X(01). 
031504                 88  SAVE-AM-CANADIAN-POST-CODE       VALUE 
031504                     'A' THRU 'Z'. 
031504             20  FILLER                    PIC  X(04). 
031504         16  FILLER                        PIC  X(04).
031504     12  WS-SAV-AM-CANADIAN-POSTAL-CODE REDEFINES 
031504                        WS-SAV-AM-ZIP      PIC  X(09).

031504     12  WS-SAV-AM-TEL-NO.                                     
031504         16  WS-SAV-AM-AREA-CODE           PIC 9(03).    
031504         16  WS-SAV-AM-TEL-PRE             PIC 9(03).   
031504         16  WS-SAV-AM-TEL-NBR             PIC 9(04). 

00378  01  W-KEY-AREAS.
00379      12  FILLER                    PIC  X(13)
00380                                       VALUE 'PROGRAM KEYS:'.
00381
00382      12  W-ACCT-SAVE-KEY           PIC  X(20).
00383      12  W-ACCT-KEY.
00384          16  W-ACCT-PARTIAL-KEY.
00385              20  W-ACCT-COMPANY-CD PIC  X(01).
00387              20  W-ACCT-CARRIER    PIC  X(01).
00388              20  W-ACCT-GROUPING   PIC  X(06).
00389              20  W-ACCT-STATE      PIC  X(02).
00390              20  W-ACCT-ACCOUNT    PIC  X(10).
00391          16  W-ACCT-EXP-DT         PIC  X(02).
00392
00393      12  W-ARCH-SAVE-KEY         PIC  X(05).
00394      12  W-ARCH-KEY.
00395          16  W-ARCH-PARTIAL-KEY.
00396              20  W-ARCH-COMPANY-CD
00397                                  PIC  X(01).
00398              20  W-ARCH-NUMBER   PIC S9(08)      COMP.
00399          16  W-ARCH-SEQ-NO       PIC S9(04)      COMP VALUE +0.
00400
00401      12  W-ARCT-KEY.
00402          16  W-ARCT-PARTIAL-KEY.
00403              20  W-ARCT-COMPANY-CD
00404                                  PIC  X(01).
00405              20  W-ARCT-NUMBER   PIC S9(08)      COMP.
00406          16  W-ARCT-REC-TYPE     PIC  X(01).
00407          16  W-ARCT-SEQ-NO       PIC S9(04)      COMP VALUE +0.
00408
00409      12  W-CERT-KEY.
00410          16  W-CERT-COMPANY-CD   PIC  X(01).
00411          16  W-CERT-CARRIER      PIC  X(01).
00412          16  W-CERT-GROUPING     PIC  X(06).
00413          16  W-CERT-STATE        PIC  X(02).
00414          16  W-CERT-ACCOUNT      PIC  X(10).
00415          16  W-CERT-EFF-DT       PIC  X(02).
00416          16  W-CERT-CERT-NO.
00417              20  W-CERT-CERT-PRIME
00418                                  PIC  X(10).
00419              20  W-CERT-CERT-SFX PIC  X(01).
00420
00421      12  W-CHEK-KEY.
00422          16  W-CHEK-COMPANY-CD   PIC  X(01).
00423          16  W-CHEK-CARRIER      PIC  X(01).
00424          16  W-CHEK-GROUPING     PIC  X(06).
00425          16  W-CHEK-STATE        PIC  X(02).
00426          16  W-CHEK-ACCOUNT      PIC  X(10).
00427          16  W-CHEK-EFF-DT       PIC  X(02).
00428          16  W-CHEK-CERT-NO.
00429              20  W-CHEK-CERT-PRIME
00430                                  PIC  X(10).
00431              20  W-CHEK-CERT-SFX PIC  X(01).
00432          16  W-CHEK-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
00433
00434      12  W-CNTL-KEY.
00435          16  W-CNTL-COMPANY-ID   PIC  X(03).
00436          16  W-CNTL-RECORD-TYPE  PIC  X(01)   VALUE '1'.
00437          16  W-CNTL-GENL.
00438              20  W-CNTL-GEN1     PIC  X(02)   VALUE SPACES.
00439              20  W-CNTL-GEN2.
00440                  24 W-CNTL-GEN3  PIC  X(01)   VALUE SPACES.
00441                  24 W-CNTL-GEN4  PIC  X(01)   VALUE SPACES.
00442          16  W-CNTL-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
00443
00444      12  W-COMP-SAVE-KEY         PIC  X(29).
00445      12  W-COMP-KEY.
00446          16  W-COMP-COMPANY-CD   PIC  X(01).
00447          16  W-COMP-CARRIER      PIC  X(01).
00448          16  W-COMP-GROUPING     PIC  X(06).
00449          16  W-COMP-RESP-PERSON.
00450              20  W-COMP-RP-CHAR OCCURS 10 TIMES
00451                                 INDEXED BY W-COMP-NDX
00452                                  PIC  X(01).
00453          16  W-COMP-ACCOUNT      PIC  X(10).
00454          16  W-COMP-TYPE         PIC  X(01).
00455
00456      12  W-DELETE-KEY.
00457          16  W-DELETE-PARTIAL-KEY.
00458              20  W-DELETE-COMPANY-CD
00459                                  PIC  X(01).
00460              20  W-DELETE-NUMBER PIC S9(08)      COMP.
00461          16  W-DELETE-RECORD-TYPE
00462                                  PIC  X(01).
00463          16  W-DELETE-SEQ        PIC S9(04)      COMP VALUE +0.
00464
00465      12  W-MAIL-KEY.
00466          16  W-MAIL-COMPANY-CD   PIC  X(01).
00467          16  W-MAIL-CARRIER      PIC  X(01).
00468          16  W-MAIL-GROUPING     PIC  X(06).
00469          16  W-MAIL-STATE        PIC  X(02).
00470          16  W-MAIL-ACCOUNT      PIC  X(10).
00471          16  W-MAIL-EFF-DT       PIC  X(02).
00472          16  W-MAIL-CERT-NO.
00473              20  W-MAIL-CERT-PRIME
00474                                  PIC  X(10).
00475              20  W-MAIL-CERT-SFX PIC  X(01).
00476
00477      12  W-PNDB-KEY.
00478          16  W-PNDB-COMPANY-CD   PIC  X(01).
00479          16  W-PNDB-ENTRY        PIC  X(06).
00480          16  W-PNDB-SEQ-NO       PIC S9(04)  COMP.
00481          16  W-PNDB-CHG-SEQ-NO   PIC S9(04)  COMP.
00482
00483      12  W-PNDB2-KEY.
00484          16  W-PNDB2-COMPANY-CD  PIC  X(01).
00485          16  W-PNDB2-CARRIER     PIC  X(01).
00486          16  W-PNDB2-GROUPING    PIC  X(06).
00487          16  W-PNDB2-STATE       PIC  X(02).
00488          16  W-PNDB2-ACCOUNT     PIC  X(10).
00489          16  W-PNDB2-EFF-DT      PIC  X(02).
00490          16  W-PNDB2-CERT-NO.
00491              20  W-PNDB2-CERT-PRIME
00492                                  PIC  X(10).
00493              20  W-PNDB2-CERT-SFX
00494                                  PIC  X(01).
00495          16  W-PNDB2-ALT-CHG-SEQ-NO
00496                                  PIC S9(04)  COMP.
00497          16  W-PNDB2-TYPE        PIC  X(01).
00498
00499      12  W-PYAJ-KEY.
00500          16  W-PYAJ-COMPANY-CD   PIC  X(01).
00501          16  W-PYAJ-CARRIER      PIC  X(01).
00502          16  W-PYAJ-GROUPING     PIC  X(06).
00503          16  W-PYAJ-FIN-RESP     PIC  X(10).
00504          16  W-PYAJ-ACCOUNT      PIC  X(10).
00505          16  W-PYAJ-FILE-SEQ-NO  PIC S9(08)    COMP.
00506          16  W-PYAJ-RECORD-TYPE  PIC  X(01).
00507
00508      12  W-SC-QUID-KEY.
00509          16  W-SC-QUID-TERMINAL  PIC  X(04).
00510          16  W-SC-QUID-SYSTEM    PIC  X(04).
00511
00512      12  W-TEXT-SAVE-KEY         PIC  X(05).
00513      12  W-TEXT-KEY.
00514          16  W-TEXT-PARTIAL-KEY.
00515              20  W-TEXT-COMPANY-CD
00516                                  PIC  X(01).
00517              20  W-TEXT-LETTER   PIC  X(04).
00518          16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.
00519          16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.
00520
00521                                  EJECT
00522  01  FILLER                      PIC  X(22)
00523                                  VALUE 'INTERFACE AREA STARTS:'.
00524      COPY ELCINTF.
00525      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00526      COPY ELC1042.
00527      COPY ELC689PI.
081008         16  FILLER              PIC X(276).
00528 *        16  FILLER                        PIC X(280).
00529
00530  01  FILLER                      PIC  X(20)
00531                                  VALUE ':INTERFACE AREA ENDS'.
00532                                  EJECT
00533  01  FILLER                      PIC  X(16)
00534                         VALUE 'MAP AREA STARTS:'.
00535      COPY EL689S.
00536  01  W-MAP-REDEF REDEFINES EL689AI.
101501     12  FILLER                  PIC X(240).
00538      12  EL689RI.
00539          16  W-TEXT-LINES OCCURS 12 TIMES INDEXED BY W-SC-NDX.
00540              20  W-SC-LINEL      PIC S9(04) COMP.
00541              20  W-SC-LINEA      PIC  X(01).
00542              20  W-SC-LINE       PIC  X(03).
00543              20  W-SC-TEXTL      PIC S9(04) COMP.
00544              20  W-SC-TEXTA      PIC  X(01).
00545              20  W-SC-TEXT       PIC  X(70).
00546      12  FILLER                  PIC  X(87).
00547                                  EJECT
00548  01  W-CONSTANT-AREA.
00549      12  FILLER                  PIC  X(18)
00550                                  VALUE 'PROGRAM CONSTANTS:'.
00551      12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP  VALUE +03.
00552      12  W-ARCH-LENGTH           PIC S9(04)  COMP  VALUE +250.
00553      12  W-ARCT-LENGTH           PIC S9(04)  COMP  VALUE +1640.
00554      12  W-MAX-LINES             PIC S9(03) VALUE +300 COMP-3.
00555      12  W-NUM-LINES-PER-SCREEN  PIC  9(02)        VALUE 12.
00556      12  W-TS-NUM-REC-IN-GROUP   PIC  9(02)        VALUE 50.
00557      12  W-TS-LENGTH             PIC S9(04)  COMP  VALUE +3650.
00558      12  W-TS-MAP-LENGTH         PIC S9(04)  COMP  VALUE +1260.
00559      12  W-ZEROS                 PIC S9(03) VALUE +000 COMP-3.
00560
00573      12  W-GETMAIN-SPACE         PIC  X(01) VALUE SPACE.
00574      12  W-LGXX-ID               PIC  X(04) VALUE 'LGXX'.
00575      12  W-LINK-001              PIC  X(05) VALUE 'EL001'.
00576      12  W-LINK-004              PIC  X(05) VALUE 'EL004'.
00577      12  W-LINK-ELDATCV          PIC  X(07) VALUE 'ELDATCV'.
00578      12  W-LOWER-CASE            PIC  X(26)
00579          VALUE 'abcdefghijklmnopqrstuvwxyz'.
00580      12  W-MAIL-FILE-ID          PIC  X(08) VALUE 'ERMAIL'.
00581      12  W-MAP.
00582          16  W-MAP-PREFIX        PIC  X(02) VALUE 'EL'.
00583          16  W-MAP-NUM           PIC  X(04) VALUE '689A'.
00584          16  W-MAP-FILLER        PIC  X(02) VALUE SPACES.
00585      12  W-MAPSET                PIC  X(08) VALUE 'EL689S'.
00586      12  W-PGM-EL1042            PIC  X(08) VALUE 'EL1042'.
00587      12  W-PGM-EL690             PIC  X(08) VALUE 'EL690'.
00588      12  W-PGM-EL626             PIC  X(08) VALUE 'EL626'.
00589      12  W-PNDB-FILE-ID          PIC  X(08) VALUE 'ERPNDB'.
00590      12  W-PRINT-TRANS           PIC  X(04) VALUE 'EXH5'.
00591      12  W-PYAJ-FILE-ID          PIC  X(08) VALUE 'ERPYAJ'.
00592
00593      12  W-TEXT-FILE-ID          PIC  X(08) VALUE 'ELLETR'.
00594      12  W-THIS-PGM              PIC  X(08) VALUE 'EL689'.
00595      12  W-TOP-FORM              PIC  X(70)
00596                               VALUE '*****TOP OF FORM *****'.
00597      12  W-TRANSACTION           PIC  X(04) VALUE 'EXH3'.
00598      12  W-UPPER-CASE            PIC  X(26)
00599          VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
00600      12  W-XCTL-005              PIC  X(05) VALUE 'EL005'.
00601      12  W-XCTL-010              PIC  X(05) VALUE 'EL010'.
00602      12  W-XCTL-626              PIC  X(05) VALUE 'EL626'.
00603                                  EJECT
00604  01  W-VARIABLE-PROCESS-CNTLS.
00605      12  FILLER                  PIC  X(26)
00606                         VALUE 'VARIABLE WORK AREA STARTS:'.
00607      12  W-NUM-OF-VARIABLES      PIC S9(03) VALUE +183 COMP-3.
00608      12  W-VAR-HOLD.
00609          16  W-V1                PIC  X(01).
00610          16  W-V2                PIC  X(01).
00611          16  W-V3                PIC  X(01).
00612      12  FILLER REDEFINES W-VAR-HOLD.
00613          16  W-VAR-RELATIVE-NUM  PIC  9(03).
00614      12  W-FIELD-SQUEEZE-IND     PIC  X(01).
00615          88  W-SQUEEZE-FIELD          VALUE '#'.
00616
00617  01  W-SUPPORTED-VARIABLES.
00618
00619 *****************COMPANY VARIABLES - ELCNTL ********************
00620 *****COMPANY NAME
00621      12  FILLER                  PIC  X(03) VALUE '001'.
00622      12  FILLER                  PIC S9(04) COMP VALUE +30.
00623      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00624      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00625      12  FILLER                  PIC S9(04) COMP VALUE +01.
00626
00627 *****FULL COMPANY ADDRESS
00628      12  FILLER                  PIC  X(03) VALUE '002'.
00629      12  FILLER                  PIC S9(04) COMP VALUE +30.
00630      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00631      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00632      12  FILLER                  PIC S9(04) COMP VALUE +01.
00633
00634      12  FILLER                  PIC  X(03) VALUE '003'.
00635      12  FILLER                  PIC S9(04) COMP VALUE +30.
00636      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00637      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00638      12  FILLER                  PIC S9(04) COMP VALUE +01.
00639
00640      12  FILLER                  PIC  X(03) VALUE '004'.
00641      12  FILLER                  PIC S9(04) COMP VALUE +30.
00642      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00643      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00644      12  FILLER                  PIC S9(04) COMP VALUE +01.
00645
00646      12  FILLER                  PIC  X(03) VALUE '005'.
00647      12  FILLER                  PIC S9(04) COMP VALUE +30.
00648      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00649      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00650      12  FILLER                  PIC S9(04) COMP VALUE +01.
00651
00652      12  FILLER                  PIC  X(03) VALUE '006'.
00653      12  FILLER                  PIC S9(04) COMP VALUE +30.
00654      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00655      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00656      12  FILLER                  PIC S9(04) COMP VALUE +01.
00657
00658 *****REMAINING 4 ARE NOT CURRENTLY USED
00659      12  FILLER                  PIC  X(03) VALUE '007'.
00660      12  FILLER                  PIC S9(04) COMP VALUE +30.
00661      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00662      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00663      12  FILLER                  PIC S9(04) COMP VALUE +01.
00664
00665      12  FILLER                  PIC  X(03) VALUE '008'.
00666      12  FILLER                  PIC S9(04) COMP VALUE +30.
00667      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00668      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00669      12  FILLER                  PIC S9(04) COMP VALUE +01.
00670
00671      12  FILLER                  PIC  X(03) VALUE '009'.
00672      12  FILLER                  PIC S9(04) COMP VALUE +30.
00673      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00674      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00675      12  FILLER                  PIC S9(04) COMP VALUE +01.
00676
00677      12  FILLER                  PIC  X(03) VALUE '010'.
00678      12  FILLER                  PIC S9(04) COMP VALUE +30.
00679      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00680      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00681      12  FILLER                  PIC S9(04) COMP VALUE +01.
00682
00683 ************ LIFE BENEFIT VARIABLES - ELCNTL *******************
00684 *****LIFE BENEFIT DESCRIPTION
00685      12  FILLER                  PIC  X(03) VALUE '011'.
00686      12  FILLER                  PIC S9(04) COMP VALUE +10.
00687      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00688      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00689      12  FILLER                  PIC S9(04) COMP VALUE +02.
00690
00691 *****REMAINING 3 ARE NOT CURRENTLY USED
00692      12  FILLER                  PIC  X(03) VALUE '012'.
00693      12  FILLER                  PIC S9(04) COMP VALUE +30.
00694      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00695      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00696      12  FILLER                  PIC S9(04) COMP VALUE +02.
00697
00698      12  FILLER                  PIC  X(03) VALUE '013'.
00699      12  FILLER                  PIC S9(04) COMP VALUE +30.
00700      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00701      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00702      12  FILLER                  PIC S9(04) COMP VALUE +02.
00703
00704      12  FILLER                  PIC  X(03) VALUE '014'.
00705      12  FILLER                  PIC S9(04) COMP VALUE +30.
00706      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00707      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00708      12  FILLER                  PIC S9(04) COMP VALUE +02.
00709
00710 ************* A&H BENEFIT VARIABLES - ELCNTL *******************
00711 *****AH BENEFIT DESCRIPTION
00712      12  FILLER                  PIC  X(03) VALUE '015'.
00713      12  FILLER                  PIC S9(04) COMP VALUE +10.
00714      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00715      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00716      12  FILLER                  PIC S9(04) COMP VALUE +03.
00717
00718 *****ELIMINATION PERIOD
00719      12  FILLER                  PIC  X(03) VALUE '016'.
00720      12  FILLER                  PIC S9(04) COMP VALUE +2.
00721      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00722      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00723      12  FILLER                  PIC S9(04) COMP VALUE +03.
00724
00725 *****REMAINING 3 ARE NOT CURRENTLY USED
00726      12  FILLER                  PIC  X(03) VALUE '017'.
00727      12  FILLER                  PIC S9(04) COMP VALUE +30.
00728      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00729      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00730      12  FILLER                  PIC S9(04) COMP VALUE +03.
00731
00732      12  FILLER                  PIC  X(03) VALUE '018'.
00733      12  FILLER                  PIC S9(04) COMP VALUE +30.
00734      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00735      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00736      12  FILLER                  PIC S9(04) COMP VALUE +03.
00737
00738      12  FILLER                  PIC  X(03) VALUE '019'.
00739      12  FILLER                  PIC S9(04) COMP VALUE +30.
00740      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00741      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00742      12  FILLER                  PIC S9(04) COMP VALUE +03.
00743
00744 *****************CARRIER VARIABLES - ELCNTL ********************
00745 *****CARRIER NAME
00746      12  FILLER                  PIC  X(03) VALUE '020'.
00747      12  FILLER                  PIC S9(04) COMP VALUE +30.
00748      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00749      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00750      12  FILLER                  PIC S9(04) COMP VALUE +04.
00751
00752 *****FULL CARRIER ADDRESS
00753      12  FILLER                  PIC  X(03) VALUE '021'.
00754      12  FILLER                  PIC S9(04) COMP VALUE +30.
00755      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00756      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00757      12  FILLER                  PIC S9(04) COMP VALUE +04.
00758
00759      12  FILLER                  PIC  X(03) VALUE '022'.
00760      12  FILLER                  PIC S9(04) COMP VALUE +30.
00761      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00762      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00763      12  FILLER                  PIC S9(04) COMP VALUE +04.
00764
00765      12  FILLER                  PIC  X(03) VALUE '023'.
00766      12  FILLER                  PIC S9(04) COMP VALUE +30.
00767      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00768      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00769      12  FILLER                  PIC S9(04) COMP VALUE +04.
00770
00771      12  FILLER                  PIC  X(03) VALUE '024'.
00772      12  FILLER                  PIC S9(04) COMP VALUE +30.
00773      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00774      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00775      12  FILLER                  PIC S9(04) COMP VALUE +04.
00776
00777      12  FILLER                  PIC  X(03) VALUE '025'.
00778      12  FILLER                  PIC S9(04) COMP VALUE +30.
00779      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00780      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00781      12  FILLER                  PIC S9(04) COMP VALUE +04.
00782
00783 *****CARRIER PHONE NUMBER
00784      12  FILLER                  PIC  X(03) VALUE '026'.
00785      12  FILLER                  PIC S9(04) COMP VALUE +12.
00786      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00787      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00788      12  FILLER                  PIC S9(04) COMP VALUE +04.
00789
00790 *****REMAINING 4 ARE NOT CURRENTLY USED
00791      12  FILLER                  PIC  X(03) VALUE '027'.
00792      12  FILLER                  PIC S9(04) COMP VALUE +30.
00793      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00794      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00795      12  FILLER                  PIC S9(04) COMP VALUE +04.
00796
00797      12  FILLER                  PIC  X(03) VALUE '028'.
00798      12  FILLER                  PIC S9(04) COMP VALUE +30.
00799      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00800      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00801      12  FILLER                  PIC S9(04) COMP VALUE +04.
00802
00803      12  FILLER                  PIC  X(03) VALUE '029'.
00804      12  FILLER                  PIC S9(04) COMP VALUE +30.
00805      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00806      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00807      12  FILLER                  PIC S9(04) COMP VALUE +04.
00808
00809      12  FILLER                  PIC  X(03) VALUE '030'.
00810      12  FILLER                  PIC S9(04) COMP VALUE +30.
00811      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00812      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00813      12  FILLER                  PIC S9(04) COMP VALUE +04.
00814
00815 ***************** MAIL VARIABLES - ELMAIL **********************
00816 *****FULL MAIL ADDRESS
00817      12  FILLER                  PIC  X(03) VALUE '031'.
00818      12  FILLER                  PIC S9(04) COMP VALUE +30.
00819      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00820      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00821      12  FILLER                  PIC S9(04) COMP VALUE +05.
00822
00823      12  FILLER                  PIC  X(03) VALUE '032'.
00824      12  FILLER                  PIC S9(04) COMP VALUE +30.
00825      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00826      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00827      12  FILLER                  PIC S9(04) COMP VALUE +05.
00828
00829      12  FILLER                  PIC  X(03) VALUE '033'.
00830      12  FILLER                  PIC S9(04) COMP VALUE +30.
00831      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00832      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00833      12  FILLER                  PIC S9(04) COMP VALUE +05.
00834
00835      12  FILLER                  PIC  X(03) VALUE '034'.
00836      12  FILLER                  PIC S9(04) COMP VALUE +30.
00837      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00838      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00839      12  FILLER                  PIC S9(04) COMP VALUE +05.
00840
00841      12  FILLER                  PIC  X(03) VALUE '035'.
00842      12  FILLER                  PIC S9(04) COMP VALUE +30.
00843      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00844      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00845      12  FILLER                  PIC S9(04) COMP VALUE +05.
00846
00847      12  FILLER                  PIC  X(03) VALUE '036'.
00848      12  FILLER                  PIC S9(04) COMP VALUE +30.
00849      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00850      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00851      12  FILLER                  PIC S9(04) COMP VALUE +05.
00852
031512***** CRED BENE NAME FROM ERMAIL
00854      12  FILLER                  PIC  X(03) VALUE '037'.
031512     12  FILLER                  PIC S9(04) COMP VALUE +25.
00856      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00857      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00858      12  FILLER                  PIC S9(04) COMP VALUE +05.
00859
031512*****REMAINING 3 ARE NOT CURRENTLY USED
00860      12  FILLER                  PIC  X(03) VALUE '038'.
00861      12  FILLER                  PIC S9(04) COMP VALUE +30.
00862      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00863      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00864      12  FILLER                  PIC S9(04) COMP VALUE +05.
00865
00866      12  FILLER                  PIC  X(03) VALUE '039'.
00867      12  FILLER                  PIC S9(04) COMP VALUE +30.
00868      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00869      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00870      12  FILLER                  PIC S9(04) COMP VALUE +05.
00871
00872 *************** ACCOUNT VARIABLES - ERACCT *********************
00873 *****ACCOUNT NAME
00874      12  FILLER                  PIC  X(03) VALUE '040'.
00875      12  FILLER                  PIC S9(04) COMP VALUE +30.
00876      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00877      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00878      12  FILLER                  PIC S9(04) COMP VALUE +06.
00879
00880 *****FULL ACCOUNT ADDRESS
00881      12  FILLER                  PIC  X(03) VALUE '041'.
00882      12  FILLER                  PIC S9(04) COMP VALUE +30.
00883      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00884      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00885      12  FILLER                  PIC S9(04) COMP VALUE +06.
00886
00887      12  FILLER                  PIC  X(03) VALUE '042'.
00888      12  FILLER                  PIC S9(04) COMP VALUE +30.
00889      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00890      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00891      12  FILLER                  PIC S9(04) COMP VALUE +06.
00892
00893      12  FILLER                  PIC  X(03) VALUE '043'.
00894      12  FILLER                  PIC S9(04) COMP VALUE +30.
00895      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00896      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00897      12  FILLER                  PIC S9(04) COMP VALUE +06.
00898
00899      12  FILLER                  PIC  X(03) VALUE '044'.
00900      12  FILLER                  PIC S9(04) COMP VALUE +30.
00901      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00902      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00903      12  FILLER                  PIC S9(04) COMP VALUE +06.
00904
00905      12  FILLER                  PIC  X(03) VALUE '045'.
00906      12  FILLER                  PIC S9(04) COMP VALUE +30.
00907      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00908      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00909      12  FILLER                  PIC S9(04) COMP VALUE +06.
00910
00911 *****ACCOUNT PHONE NUMBER
00912      12  FILLER                  PIC  X(03) VALUE '046'.
00913      12  FILLER                  PIC S9(04) COMP VALUE +12.
00914      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00915      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00916      12  FILLER                  PIC S9(04) COMP VALUE +06.
00917
00918 *****ACCOUNT CONTROL NAME AM-CONTROL-NAME
00919      12  FILLER                  PIC  X(03) VALUE '047'.
00920      12  FILLER                  PIC S9(04) COMP VALUE +30.
00921      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00922      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00923      12  FILLER                  PIC S9(04) COMP VALUE +06.
00924
072308****ACCOUNT CSR CODE
072308     12  FILLER                  PIC  X(03) VALUE '048'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +4..
072308     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072308     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +06.

031512*****ACCOUNT ACCOUNT NUMBER
00931      12  FILLER                  PIC  X(03) VALUE '049'.
031512     12  FILLER                  PIC S9(04) COMP VALUE +10.
00933      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00934      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00935      12  FILLER                  PIC S9(04) COMP VALUE +06.
00936
031512**** CRED BENE ADDRESS LINE 1 ERMAIL
00937      12  FILLER                  PIC  X(03) VALUE '050'.
00938      12  FILLER                  PIC S9(04) COMP VALUE +30.
00939      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00940      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00941      12  FILLER                  PIC S9(04) COMP VALUE +06.
00942
031512**** CRED BENE ADDRESS LINE 2 ERMAIL
00943      12  FILLER                  PIC  X(03) VALUE '051'.
00944      12  FILLER                  PIC S9(04) COMP VALUE +30.
00945      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00946      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00947      12  FILLER                  PIC S9(04) COMP VALUE +06.
00948
031512**** CRED BENE CITY STATE ERMAIL
00949      12  FILLER                  PIC  X(03) VALUE '052'.
00950      12  FILLER                  PIC S9(04) COMP VALUE +30.
00951      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00952      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00953      12  FILLER                  PIC S9(04) COMP VALUE +06.
00954
031512**** CRED BENE ZIP ERMAIL
00955      12  FILLER                  PIC  X(03) VALUE '053'.
00956      12  FILLER                  PIC S9(04) COMP VALUE +30.
00957      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00958      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00959      12  FILLER                  PIC S9(04) COMP VALUE +06.
00960
031512*****REMAINING 6 ARE NOT CURRENTLY USED
00961      12  FILLER                  PIC  X(03) VALUE '054'.
00962      12  FILLER                  PIC S9(04) COMP VALUE +30.
00963      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00964      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00965      12  FILLER                  PIC S9(04) COMP VALUE +06.
00966
00967      12  FILLER                  PIC  X(03) VALUE '055'.
00968      12  FILLER                  PIC S9(04) COMP VALUE +30.
00969      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00970      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00971      12  FILLER                  PIC S9(04) COMP VALUE +06.
00972
00973      12  FILLER                  PIC  X(03) VALUE '056'.
00974      12  FILLER                  PIC S9(04) COMP VALUE +30.
00975      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00976      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00977      12  FILLER                  PIC S9(04) COMP VALUE +06.
00978
00979      12  FILLER                  PIC  X(03) VALUE '057'.
00980      12  FILLER                  PIC S9(04) COMP VALUE +30.
00981      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00982      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00983      12  FILLER                  PIC S9(04) COMP VALUE +06.
00984
00985      12  FILLER                  PIC  X(03) VALUE '058'.
00986      12  FILLER                  PIC S9(04) COMP VALUE +30.
00987      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00988      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00989      12  FILLER                  PIC S9(04) COMP VALUE +06.
00990
00991      12  FILLER                  PIC  X(03) VALUE '059'.
00992      12  FILLER                  PIC S9(04) COMP VALUE +30.
00993      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00994      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00995      12  FILLER                  PIC S9(04) COMP VALUE +06.
00996
00997 *************** NON FILE VARIABLES *****************************
00998 *****CURRENT DATE
00999      12  FILLER                  PIC  X(03) VALUE '060'.
01000      12  FILLER                  PIC S9(04) COMP VALUE +08.
01001      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01002      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01003      12  FILLER                  PIC S9(04) COMP VALUE +07.
01004
01005 *****FULL CURRENT DATE
01006      12  FILLER                  PIC  X(03) VALUE '061'.
01007      12  FILLER                  PIC S9(04) COMP VALUE +18.
01008      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01009      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01010      12  FILLER                  PIC S9(04) COMP VALUE +07.
01011
01012 *****FORM
01013      12  FILLER                  PIC  X(03) VALUE '062'.
01014      12  FILLER                  PIC S9(04) COMP VALUE +04.
01015      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01016      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01017      12  FILLER                  PIC S9(04) COMP VALUE +07.
01018
01019 *****VARIABLE 1
01020      12  FILLER                  PIC  X(03) VALUE '063'.
01021      12  FILLER                  PIC S9(04) COMP VALUE +30.
01022      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01023      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01024      12  FILLER                  PIC S9(04) COMP VALUE +07.
01025
01026 *****VARIABLE 2
01027      12  FILLER                  PIC  X(03) VALUE '064'.
01028      12  FILLER                  PIC S9(04) COMP VALUE +30.
01029      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01030      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01031      12  FILLER                  PIC S9(04) COMP VALUE +07.
01032
01033 *****VARIABLE 3
01034      12  FILLER                  PIC  X(03) VALUE '065'.
01035      12  FILLER                  PIC S9(04) COMP VALUE +30.
01036      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01037      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01038      12  FILLER                  PIC S9(04) COMP VALUE +07.
01039
01040 *****VARIABLE 4
01041      12  FILLER                  PIC  X(03) VALUE '066'.
01042      12  FILLER                  PIC S9(04) COMP VALUE +30.
01043      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01044      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01045      12  FILLER                  PIC S9(04) COMP VALUE +07.
01046
01047 *****REMAINING 3 ARE NOT CURRENTLY USED
01048      12  FILLER                  PIC  X(03) VALUE '067'.
01049      12  FILLER                  PIC S9(04) COMP VALUE +30.
01050      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01051      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01052      12  FILLER                  PIC S9(04) COMP VALUE +07.
01053
01054      12  FILLER                  PIC  X(03) VALUE '068'.
01055      12  FILLER                  PIC S9(04) COMP VALUE +30.
01056      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01057      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01058      12  FILLER                  PIC S9(04) COMP VALUE +07.
01059
01060      12  FILLER                  PIC  X(03) VALUE '069'.
01061      12  FILLER                  PIC S9(04) COMP VALUE +30.
01062      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01063      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01064      12  FILLER                  PIC S9(04) COMP VALUE +07.
01065
01066 ************** CERTIFICATE VARIABLES - ELCERT *****************
01067 *****CARRIER CODE IN CERT
01068      12  FILLER                  PIC  X(03) VALUE '070'.
01069      12  FILLER                  PIC S9(04) COMP VALUE +1.
01070      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01071      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01072      12  FILLER                  PIC S9(04) COMP VALUE +08.
01073
01074 *****GROUPING CODE IN CERT
01075      12  FILLER                  PIC  X(03) VALUE '071'.
01076      12  FILLER                  PIC S9(04) COMP VALUE +06.
01077      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01078      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01079      12  FILLER                  PIC S9(04) COMP VALUE +08.
01080
01081 *****ACCOUNT NUMBER IN CERT
01082      12  FILLER                  PIC  X(03) VALUE '072'.
01083      12  FILLER                  PIC S9(04) COMP VALUE +10.
01084      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01085      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01086      12  FILLER                  PIC S9(04) COMP VALUE +08.
01087
01088 *****CERTIFICATE NUMBER
01089      12  FILLER                  PIC  X(03) VALUE '073'.
01090      12  FILLER                  PIC S9(04) COMP VALUE +11.
01091      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01092      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01093      12  FILLER                  PIC S9(04) COMP VALUE +08.
01094
01095 *****CERT EFFECTIVE DATE
01096      12  FILLER                  PIC  X(03) VALUE '074'.
01097      12  FILLER                  PIC S9(04) COMP VALUE +08.
01098      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01099      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01100      12  FILLER                  PIC S9(04) COMP VALUE +08.
01101
01102 *****CERT EXPIRATION DATE (LIFE)
01103      12  FILLER                  PIC  X(03) VALUE '075'.
01104      12  FILLER                  PIC S9(04) COMP VALUE +08.
01105      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01106      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01107      12  FILLER                  PIC S9(04) COMP VALUE +08.
01108
01109 *****CERT EXPIRATION DATE (AH)
01110      12  FILLER                  PIC  X(03) VALUE '076'.
01111      12  FILLER                  PIC S9(04) COMP VALUE +08.
01112      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01113      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01114      12  FILLER                  PIC S9(04) COMP VALUE +08.
01115
01116 *****LIFE TERM
01117      12  FILLER                  PIC  X(03) VALUE '077'.
01118      12  FILLER                  PIC S9(04) COMP VALUE +3.
01119      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01120      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01121      12  FILLER                  PIC S9(04) COMP VALUE +08.
01122
01123 *****AH  TERM
01124      12  FILLER                  PIC  X(03) VALUE '078'.
01125      12  FILLER                  PIC S9(04) COMP VALUE +3.
01126      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01127      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01128      12  FILLER                  PIC S9(04) COMP VALUE +08.
01129
01130 *****LIFE COVERAGE AMOUNT
01131      12  FILLER                  PIC  X(03) VALUE '079'.
01132      12  FILLER                  PIC S9(04) COMP VALUE +15.
01133      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01134      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01135      12  FILLER                  PIC S9(04) COMP VALUE +08.
01136
01137 *****AH MONTHLY BENEFIT
01138      12  FILLER                  PIC  X(03) VALUE '080'.
01139      12  FILLER                  PIC S9(04) COMP VALUE +13.
01140      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01141      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01142      12  FILLER                  PIC S9(04) COMP VALUE +08.
01143
01144 *****LIFE CANCEL DATE
01145      12  FILLER                  PIC  X(03) VALUE '081'.
01146      12  FILLER                  PIC S9(04) COMP VALUE +08.
01147      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01148      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01149      12  FILLER                  PIC S9(04) COMP VALUE +08.
01150
01151 *****AH CANCEL DATE
01152      12  FILLER                  PIC  X(03) VALUE '082'.
01153      12  FILLER                  PIC S9(04) COMP VALUE +08.
01154      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01155      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01156      12  FILLER                  PIC S9(04) COMP VALUE +08.
01157
01158 *****LIFE COVERAGE FORM NUMBER
01159      12  FILLER                  PIC  X(03) VALUE '083'.
01160      12  FILLER                  PIC S9(04) COMP VALUE +12.
01161      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01162      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01163      12  FILLER                  PIC S9(04) COMP VALUE +08.
01164
01165 *****UNUSED
01166      12  FILLER                  PIC  X(03) VALUE '084'.
01167      12  FILLER                  PIC S9(04) COMP VALUE +12.
01168      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01169      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01170      12  FILLER                  PIC S9(04) COMP VALUE +08.
01171
01172 *****INSUREDS AGE AT POLICY ISSUE (NOT USED)
01173      12  FILLER                  PIC  X(03) VALUE '085'.
01174      12  FILLER                  PIC S9(04) COMP VALUE +3.
01175      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01176      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01177      12  FILLER                  PIC S9(04) COMP VALUE +08.
01178
01179 *****LOAN NUMBER
01180      12  FILLER                  PIC  X(03) VALUE '086'.
01181      12  FILLER                  PIC S9(04) COMP VALUE +08.
01182      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01183      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01184      12  FILLER                  PIC S9(04) COMP VALUE +08.
01185
01186 *****LOAN BALANCE
01187      12  FILLER                  PIC  X(03) VALUE '087'.
01188      12  FILLER                  PIC S9(04) COMP VALUE +13.
01189      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01190      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01191      12  FILLER                  PIC S9(04) COMP VALUE +08.
01192
01193 *****MEMBER NUMBER
01194      12  FILLER                  PIC  X(03) VALUE '088'.
01195      12  FILLER                  PIC S9(04) COMP VALUE +12.
01196      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01197      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01198      12  FILLER                  PIC S9(04) COMP VALUE +08.
01199
01200 *****INSURED SOC SEC NUMBER
01201      12  FILLER                  PIC  X(03) VALUE '089'.
01202      12  FILLER                  PIC S9(04) COMP VALUE +11.
01203      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01204      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01205      12  FILLER                  PIC S9(04) COMP VALUE +08.
01206
01207 *****INSURED INITIALS & LAST NAME (CERTIFICATE)
01208      12  FILLER                  PIC  X(03) VALUE '090'.
01209      12  FILLER                  PIC S9(04) COMP VALUE +15.
01210      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01211      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01212      12  FILLER                  PIC S9(04) COMP VALUE +08.
01213
01214 *****INSURED FIRST NAME (CERTIFICATE)
01215      12  FILLER                  PIC  X(03) VALUE '091'.
01216      12  FILLER                  PIC S9(04) COMP VALUE +10.
01217      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01218      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01219      12  FILLER                  PIC S9(04) COMP VALUE +08.
01220
01221 *****INSURED MIDDLE INITIAL (CERTIFICATE)
01222      12  FILLER                  PIC  X(03) VALUE '092'.
01223      12  FILLER                  PIC S9(04) COMP VALUE +05.
01224      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01225      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01226      12  FILLER                  PIC S9(04) COMP VALUE +08.
01227
01228 *****ORIG TERM * MON BEN
01229      12  FILLER                  PIC  X(03) VALUE '093'.
01230      12  FILLER                  PIC S9(04) COMP VALUE +15.
01231      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01232      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01233      12  FILLER                  PIC S9(04) COMP VALUE +08.
01234
01235 *****INSURED'S NAME (LAST, FIRST, INIT)
01236      12  FILLER                  PIC  X(03) VALUE '094'.
01237      12  FILLER                  PIC S9(04) COMP VALUE +30.
01238      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01239      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01240      12  FILLER                  PIC S9(04) COMP VALUE +08.
01241
01242 *****INSURED'S NAME (FIRST, INIT, LAST)
01243      12  FILLER                  PIC  X(03) VALUE  '095'.
01244      12  FILLER                  PIC S9(04) COMP VALUE +30.
01245      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01246      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01247      12  FILLER                  PIC S9(04) COMP VALUE +08.
01248
01249 *****TITLE (MR/MS)
01250      12  FILLER                  PIC  X(03) VALUE '096'.
01251      12  FILLER                  PIC S9(04) COMP VALUE +3.
01252      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01253      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01254      12  FILLER                  PIC S9(04) COMP VALUE +08.
01255
01256 *****LIFE PREMIUM (CERTIFICATE)
01257      12  FILLER                  PIC  X(03) VALUE '097'.
01258      12  FILLER                  PIC S9(04) COMP VALUE +15.
01259      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01260      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01261      12  FILLER                  PIC S9(04) COMP VALUE +08.
01262
01263 *****A/H PREMIUM (CERTIFICATE)
01264      12  FILLER                  PIC  X(03) VALUE '098'.
01265      12  FILLER                  PIC S9(04) COMP VALUE +13.
01266      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01267      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01268      12  FILLER                  PIC S9(04) COMP VALUE +08.
01269
01270 *****JOINT'S INITIALS & LAST NAME (CERTIFICATE)
01271      12  FILLER                  PIC  X(03) VALUE '099'.
01272      12  FILLER                  PIC S9(04) COMP VALUE +15.
01273      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01274      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01275      12  FILLER                  PIC S9(04) COMP VALUE +08.
01276
01277 *****JOINT'S FIRST NAME (CERTIFICATE)
01278      12  FILLER                  PIC  X(03) VALUE '100'.
01279      12  FILLER                  PIC S9(04) COMP VALUE +10.
01280      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01281      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01282      12  FILLER                  PIC S9(04) COMP VALUE +08.
01283
01284 *****JOINT'S MIDDLE INITIAL (CERTIFICATE)
01285      12  FILLER                  PIC  X(03) VALUE '101'.
01286      12  FILLER                  PIC S9(04) COMP VALUE +05.
01287      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01288      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01289      12  FILLER                  PIC S9(04) COMP VALUE +08.
01290
01291 *****JOINT'S NAME (LAST, FIRST, INIT)
01292      12  FILLER                  PIC  X(03) VALUE '102'.
01293      12  FILLER                  PIC S9(04) COMP VALUE +30.
01294      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01295      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01296      12  FILLER                  PIC S9(04) COMP VALUE +08.
01297
01298 *****JOINT'S NAME (FIRST, INIT, LAST)
01299      12  FILLER                  PIC  X(03) VALUE '103'.
01300      12  FILLER                  PIC S9(04) COMP VALUE +30.
01301      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01302      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01303      12  FILLER                  PIC S9(04) COMP VALUE +08.
01304
01305 *****INSURED'S FIRST AND LAST NAME
01306      12  FILLER                  PIC  X(03) VALUE '104'.
01307      12  FILLER                  PIC S9(04) COMP VALUE +30.
01308      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01309      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01310      12  FILLER                  PIC S9(04) COMP VALUE +08.
01311
01312 *****JOINT'S FIRST AND LAST NAME
01313      12  FILLER                  PIC  X(03) VALUE '105'.
01314      12  FILLER                  PIC S9(04) COMP VALUE +30.
01315      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01316      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01317      12  FILLER                  PIC S9(04) COMP VALUE +08.
01318
01319 *****ENTERED LIFE REFUND (CERT)
01320      12  FILLER                  PIC  X(03) VALUE '106'.
01321      12  FILLER                  PIC S9(04) COMP VALUE +13.
01322      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01323      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01324      12  FILLER                  PIC S9(04) COMP VALUE +08.
01325
01326 *****ENTERED A/H REFUND (CERT)
01327      12  FILLER                  PIC  X(03) VALUE '107'.
01328      12  FILLER                  PIC S9(04) COMP VALUE +13.
01329      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01330      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01331      12  FILLER                  PIC S9(04) COMP VALUE +08.
01332
01333 *****INSURED'S LAST NAME
01334      12  FILLER                  PIC  X(03) VALUE '108'.
01335      12  FILLER                  PIC S9(04) COMP VALUE +15.
01336      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01337      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01338      12  FILLER                  PIC S9(04) COMP VALUE +08.
01339
01340 *****BENEFICIARY
01341      12  FILLER                  PIC  X(03) VALUE  '109'.
01342      12  FILLER                  PIC S9(04) COMP VALUE +25.
01343      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01344      12  FILLER                  PIC  X(01) VALUE 'Y'.
01345      12  FILLER                  PIC S9(04) COMP VALUE +08.
01346
01347 ************** PENDING VARIABLES - ERPNDB *********************
01348 *****INSURED DATE OF BIRTH
01349      12  FILLER                  PIC  X(03) VALUE '110'.
01350      12  FILLER                  PIC S9(04) COMP VALUE +08.
01351      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01352      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01353      12  FILLER                  PIC S9(04) COMP VALUE +09.
01354
01355 *****ENTERED LIFE PREMIUM (PENDING)
01356      12  FILLER                  PIC  X(03) VALUE '111'.
01357      12  FILLER                  PIC S9(04) COMP VALUE +13.
01358      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01359      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01360      12  FILLER                  PIC S9(04) COMP VALUE +09.
01361
01362 *****ENTERED A/H PREMIUM (PENDING)
01363      12  FILLER                  PIC  X(03) VALUE '112'.
01364      12  FILLER                  PIC S9(04) COMP VALUE +13.
01365      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01366      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01367      12  FILLER                  PIC S9(04) COMP VALUE +09.
01368
01369 *****CALCULATED LIFE PREMIUM (PENDING)
01370      12  FILLER                  PIC  X(03) VALUE '113'.
01371      12  FILLER                  PIC S9(04) COMP VALUE +13.
01372      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01373      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01374      12  FILLER                  PIC S9(04) COMP VALUE +09.
01375
01376 *****CALCULATED A/H PREMIUM (PENDING)
01377      12  FILLER                  PIC  X(03) VALUE '114'.
01378      12  FILLER                  PIC S9(04) COMP VALUE +13.
01379      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01380      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01381      12  FILLER                  PIC S9(04) COMP VALUE +09.
01382
01383 *****DIFFERENCE ENTER/COMPUTED LIFE PREMIUM (PENDING)
01384      12  FILLER                  PIC  X(03) VALUE '115'.
01385      12  FILLER                  PIC S9(04) COMP VALUE +13.
01386      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01387      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01388      12  FILLER                  PIC S9(04) COMP VALUE +09.
01389
01390 *****DIFFERENCE ENTER/COMPUTED A/H PREMIUM (PENDING)
01391      12  FILLER                  PIC  X(03) VALUE '116'.
01392      12  FILLER                  PIC S9(04) COMP VALUE +13.
01393      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01394      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01395      12  FILLER                  PIC S9(04) COMP VALUE +09.
01396
01397 *****PRIOR CANCEL DATE
01398      12  FILLER                  PIC  X(03) VALUE '117'.
01399      12  FILLER                  PIC S9(04) COMP VALUE +08.
01400      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01401      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01402      12  FILLER                  PIC S9(04) COMP VALUE +09.
01403
01404 *****ENTERED LIFE REFUND (PENDING)
01405      12  FILLER                  PIC  X(03) VALUE '118'.
01406      12  FILLER                  PIC S9(04) COMP VALUE +13.
01407      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01408      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01409      12  FILLER                  PIC S9(04) COMP VALUE +09.
01410
01411 *****ENTERED A/H REFUND (PENDING)
01412      12  FILLER                  PIC  X(03) VALUE '119'.
01413      12  FILLER                  PIC S9(04) COMP VALUE +13.
01414      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01415      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01416      12  FILLER                  PIC S9(04) COMP VALUE +09.
01417
01418 *****CALCULATED LIFE REFUND (PENDING)
01419      12  FILLER                  PIC  X(03) VALUE '120'.
01420      12  FILLER                  PIC S9(04) COMP VALUE +13.
01421      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01422      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01423      12  FILLER                  PIC S9(04) COMP VALUE +09.
01424
01425 *****CALCULATED A/H REFUND (PENDING)
01426      12  FILLER                  PIC  X(03) VALUE '121'.
01427      12  FILLER                  PIC S9(04) COMP VALUE +13.
01428      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01429      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01430      12  FILLER                  PIC S9(04) COMP VALUE +09.
01431
01432 *****DIFFERENCE ENTER/COMPUTED LIFE REFUND (PENDING)
01433      12  FILLER                  PIC  X(03) VALUE '122'.
01434      12  FILLER                  PIC S9(04) COMP VALUE +13.
01435      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01436      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01437      12  FILLER                  PIC S9(04) COMP VALUE +09.
01438
01439 *****DIFFERENCE ENTER/COMPUTED A/H REFUND (PENDING)
01440      12  FILLER                  PIC  X(03) VALUE '123'.
01441      12  FILLER                  PIC S9(04) COMP VALUE +13.
01442      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01443      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01444      12  FILLER                  PIC S9(04) COMP VALUE +09.
01445
01446 *****INSUREDS AGE
01447      12  FILLER                  PIC  X(03) VALUE '124'.
01448      12  FILLER                  PIC S9(04) COMP VALUE +3.
01449      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01450      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01451      12  FILLER                  PIC S9(04) COMP VALUE +09.
01452
01453 *****LIFE BENEFIT (PENDING)
01454      12  FILLER                  PIC  X(03) VALUE '125'.
01455      12  FILLER                  PIC S9(04) COMP VALUE +15.
01456      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01457      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01458      12  FILLER                  PIC S9(04) COMP VALUE +09.
01459
01460 *****A/H BENEFIT (PENDING)
01461      12  FILLER                  PIC  X(03) VALUE '126'.
01462      12  FILLER                  PIC S9(04) COMP VALUE +13.
01463      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01464      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01465      12  FILLER                  PIC S9(04) COMP VALUE +09.
01466
01467 *****LIFE RATE
01468      12  FILLER                  PIC  X(03) VALUE '127'.
01469      12  FILLER                  PIC S9(04) COMP VALUE +08.
01470      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01471      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01472      12  FILLER                  PIC S9(04) COMP VALUE +09.
01473
01474 *****A/H RATE
01475      12  FILLER                  PIC  X(03) VALUE '128'.
01476      12  FILLER                  PIC S9(04) COMP VALUE +08.
01477      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01478      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01479      12  FILLER                  PIC S9(04) COMP VALUE +09.
01480
01481 *****TERM (PENDING)
01482      12  FILLER                  PIC  X(03) VALUE '129'.
01483      12  FILLER                  PIC S9(04) COMP VALUE +3.
01484      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01485      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01486      12  FILLER                  PIC S9(04) COMP VALUE +09.
01487
01488 *****BATCH NUMBER
01489      12  FILLER                  PIC  X(03) VALUE '130'.
01490      12  FILLER                  PIC S9(04) COMP VALUE +06.
01491      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01492      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01493      12  FILLER                  PIC S9(04) COMP VALUE +09.
01494
072908*****TOTAL OF LIFE AND A&H REFUND
072908     12  FILLER                  PIC  X(03) VALUE '131'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +13.
072908     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +09.
01501
072908*****NH INTEREST ON REFUNDS
072908     12  FILLER                  PIC  X(03) VALUE '132'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +13.
072908     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +09.

072908*****GREATER OF THE LIFE AND AH CANCEL DATE
072908     12  FILLER                  PIC  X(03) VALUE '133'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +8.
072908     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +09.

031504*****THE NEXT 6 NOT CURRENTLY USED
01513
01514      12  FILLER                  PIC  X(03) VALUE '134'.
01515      12  FILLER                  PIC S9(04) COMP VALUE +30.
01516      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01517      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01518      12  FILLER                  PIC S9(04) COMP VALUE +09.
01519
01520      12  FILLER                  PIC  X(03) VALUE '135'.
01521      12  FILLER                  PIC S9(04) COMP VALUE +30.
01522      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01523      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01524      12  FILLER                  PIC S9(04) COMP VALUE +09.
01525
01526      12  FILLER                  PIC  X(03) VALUE '136'.
01527      12  FILLER                  PIC S9(04) COMP VALUE +30.
01528      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01529      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01530      12  FILLER                  PIC S9(04) COMP VALUE +09.
01531
01532      12  FILLER                  PIC  X(03) VALUE '137'.
01533      12  FILLER                  PIC S9(04) COMP VALUE +30.
01534      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01535      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01536      12  FILLER                  PIC S9(04) COMP VALUE +09.
01537
01538      12  FILLER                  PIC  X(03) VALUE '138'.
01539      12  FILLER                  PIC S9(04) COMP VALUE +30.
01540      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01541      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01542      12  FILLER                  PIC S9(04) COMP VALUE +09.
01543
01544      12  FILLER                  PIC  X(03) VALUE '139'.
01545      12  FILLER                  PIC S9(04) COMP VALUE +30.
01546      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01547      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01548      12  FILLER                  PIC S9(04) COMP VALUE +09.
01549
01550 ************** COMPENSATION VARIABLES - ERCOMP ****************
01551 *****COMPENSATION ACCT NAME
01552      12  FILLER                  PIC  X(03) VALUE '140'.
01553      12  FILLER                  PIC S9(04) COMP VALUE +30.
01554      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01555      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01556      12  FILLER                  PIC S9(04) COMP VALUE +10.
01557
01558 *****FULL COMPENSATION ADDRESS TYPE 'A'
01559      12  FILLER                  PIC  X(03) VALUE '141'.
01560      12  FILLER                  PIC S9(04) COMP VALUE +30.
01561      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01562      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01563      12  FILLER                  PIC S9(04) COMP VALUE +10.
01564
01565      12  FILLER                  PIC  X(03) VALUE '142'.
01566      12  FILLER                  PIC S9(04) COMP VALUE +30.
01567      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01568      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01569      12  FILLER                  PIC S9(04) COMP VALUE +10.
01570
01571      12  FILLER                  PIC  X(03) VALUE '143'.
01572      12  FILLER                  PIC S9(04) COMP VALUE +30.
01573      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01574      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01575      12  FILLER                  PIC S9(04) COMP VALUE +10.
01576
01577      12  FILLER                  PIC  X(03) VALUE '144'.
01578      12  FILLER                  PIC S9(04) COMP VALUE +30.
01579      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01580      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01581      12  FILLER                  PIC S9(04) COMP VALUE +10.
01582
01583      12  FILLER                  PIC  X(03) VALUE '145'.
01584      12  FILLER                  PIC S9(04) COMP VALUE +30.
01585      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01586      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01587      12  FILLER                  PIC S9(04) COMP VALUE +10.
01588
01589      12  FILLER                  PIC  X(03) VALUE '146'.
01590      12  FILLER                  PIC S9(04) COMP VALUE +12.
01591      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01592      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01593      12  FILLER                  PIC S9(04) COMP VALUE +10.
01594
01595 *****COMPENSATION PHONE NUMBER
01596      12  FILLER                  PIC  X(03) VALUE '147'.
01597      12  FILLER                  PIC S9(04) COMP VALUE +12.
01598      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01599      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01600      12  FILLER                  PIC S9(04) COMP VALUE +10.
01601
01602 *****COMPENSATION CSR NAME
01603      12  FILLER                  PIC  X(03) VALUE '148'.
01604      12  FILLER                  PIC S9(04) COMP VALUE +30.
01605      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01606      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01607      12  FILLER                  PIC S9(04) COMP VALUE +10.
01608
01609 *****COMPENSATION LAST STATEMENT DATE
01610      12  FILLER                  PIC  X(03) VALUE '149'.
01611      12  FILLER                  PIC S9(04) COMP VALUE +18.
01612      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01613      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01614      12  FILLER                  PIC S9(04) COMP VALUE +10.
01615
01616 *****COMPENSATION ENDING BALANCE
01617      12  FILLER                  PIC  X(03) VALUE '150'.
01618      12  FILLER                  PIC S9(04) COMP VALUE +14.
01619      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01620      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01621      12  FILLER                  PIC S9(04) COMP VALUE +10.
01622
01623 ******************  PROCESSOR DATA - ELCNTL (2) ****************
01624 *****EXECUTING PROCESSOR NAME
01625      12  FILLER                  PIC  X(03) VALUE '151'.
01626      12  FILLER                  PIC S9(04) COMP VALUE +30.
01627      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01628      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01629      12  FILLER                  PIC S9(04) COMP VALUE +11.
01630
01631 *****PROCESSOR TITLE
01632      12  FILLER                  PIC  X(03) VALUE '152'.
01633      12  FILLER                  PIC S9(04) COMP VALUE +26.
01634      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01635      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01636      12  FILLER                  PIC S9(04) COMP VALUE +11.
01637
01638 *****PROCESSOR
01639      12  FILLER                  PIC  X(03) VALUE '153'.
01640      12  FILLER                  PIC S9(04) COMP VALUE +04.
01641      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01642      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01643      12  FILLER                  PIC S9(04) COMP VALUE +11.
01644
072308*****CSR TITLE
072308     12  FILLER                  PIC  X(03) VALUE '154'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +30.
072308     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072308     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +11.

01645 *****REMAINING 2 ARE NOT CURRENTLY USED
01652      12  FILLER                  PIC  X(03) VALUE '155'.
01653      12  FILLER                  PIC S9(04) COMP VALUE +30.
01654      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01655      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01656      12  FILLER                  PIC S9(04) COMP VALUE +11.
01657
01658      12  FILLER                  PIC  X(03) VALUE '156'.
01659      12  FILLER                  PIC S9(04) COMP VALUE +30.
01660      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01661      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01662      12  FILLER                  PIC S9(04) COMP VALUE +11.
01663
01664 ******************  CHECK DATA - ERCHEK    *********************
01665 *****CHECK AMOUNT
01666      12  FILLER                  PIC  X(03) VALUE '157'.
01667      12  FILLER                  PIC S9(04) COMP VALUE +13.
01668      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01669      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01670      12  FILLER                  PIC S9(04) COMP VALUE +12.
01671
01672 *****CHECK NUMBER
01673      12  FILLER                  PIC  X(03) VALUE '158'.
01674      12  FILLER                  PIC S9(04) COMP VALUE +7.
01675      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01676      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01677      12  FILLER                  PIC S9(04) COMP VALUE +12.
01678
01679 *****PAYEE 1 NAME
01680      12  FILLER                  PIC  X(03) VALUE '159'.
01681      12  FILLER                  PIC S9(04) COMP VALUE +30.
01682      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01683      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01684      12  FILLER                  PIC S9(04) COMP VALUE +12.
01685
01686 *****PAYEE 2 NAME
01687      12  FILLER                  PIC  X(03) VALUE '160'.
01688      12  FILLER                  PIC S9(04) COMP VALUE +30.
01689      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01690      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01691      12  FILLER                  PIC S9(04) COMP VALUE +12.
01692
01693 *****ADDRSS 1
01694      12  FILLER                  PIC  X(03) VALUE '161'.
01695      12  FILLER                  PIC S9(04) COMP VALUE +30.
01696      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01697      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01698      12  FILLER                  PIC S9(04) COMP VALUE +12.
01699
01700 *****ADDRESS 2
01701      12  FILLER                  PIC  X(03) VALUE '162'.
01702      12  FILLER                  PIC S9(04) COMP VALUE +30.
01703      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01704      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01705      12  FILLER                  PIC S9(04) COMP VALUE +12.
01706
01707 *****PAYEE CITY STATE
01708      12  FILLER                  PIC  X(03) VALUE '163'.
01709      12  FILLER                  PIC S9(04) COMP VALUE +30.
01710      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01711      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01712      12  FILLER                  PIC S9(04) COMP VALUE +12.
01713
01714 *****ZIP CODE
01715      12  FILLER                  PIC  X(03) VALUE '164'.
01716      12  FILLER                  PIC S9(04) COMP VALUE +30.
01717      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01718      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01719      12  FILLER                  PIC S9(04) COMP VALUE +12.
01720
01721 *****CHECK CONTROL
01722      12  FILLER                  PIC  X(03) VALUE '165'.
01723      12  FILLER                  PIC S9(04) COMP VALUE +08.
01724      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01725      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01726      12  FILLER                  PIC S9(04) COMP VALUE +12.
01727
01728 *****REASON FOR CHECK
01729      12  FILLER                  PIC  X(03) VALUE '166'.
01730      12  FILLER                  PIC S9(04) COMP VALUE +25.
01731      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01732      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01733      12  FILLER                  PIC S9(04) COMP VALUE +12.
01734
01735 *****REMAINING 3 ARE NOT CURRENTLY USED
01736      12  FILLER                  PIC  X(03) VALUE '167'.
01737      12  FILLER                  PIC S9(04) COMP VALUE +30.
01738      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01739      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01740      12  FILLER                  PIC S9(04) COMP VALUE +12.
01741
01742      12  FILLER                  PIC  X(03) VALUE '168'.
01743      12  FILLER                  PIC S9(04) COMP VALUE +30.
01744      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01745      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01746      12  FILLER                  PIC S9(04) COMP VALUE +12.
01747
01748      12  FILLER                  PIC  X(03) VALUE '169'.
01749      12  FILLER                  PIC S9(04) COMP VALUE +30.
01750      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01751      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01752      12  FILLER                  PIC S9(04) COMP VALUE +11.
01753
01754
01755 *********** PAYMENT AND ADJUSTMENT DATA - ERPYAJ  **************
01756 *****CHECK AMOUNT - PYAJ
01757      12  FILLER                  PIC  X(03) VALUE '170'.
01758      12  FILLER                  PIC S9(04) COMP VALUE +13.
01759      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01760      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01761      12  FILLER                  PIC S9(04) COMP VALUE +13.
01762
01763 *****CHECK NUMBER - PYAJ
01764      12  FILLER                  PIC  X(03) VALUE '171'.
01765      12  FILLER                  PIC S9(04) COMP VALUE +7.
01766      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01767      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01768      12  FILLER                  PIC S9(04) COMP VALUE +13.
01769
01770 *****CHECK CONTROL - PYAJ
01771      12  FILLER                  PIC  X(03) VALUE '172'.
01772      12  FILLER                  PIC S9(04) COMP VALUE +08.
01773      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01774      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01775      12  FILLER                  PIC S9(04) COMP VALUE +13.
01776
01777 ***** COMMENT - PYAJ
01778      12  FILLER                  PIC  X(03) VALUE '173'.
01779      12  FILLER                  PIC S9(04) COMP VALUE +30.
01780      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01781      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01782      12  FILLER                  PIC S9(04) COMP VALUE +13.
01783
01784 *****NEW FIELDS.
01785 *****COMPENSATION FULL ADDRESS TYPE 'G'
01786      12  FILLER                  PIC  X(03) VALUE '174'.
01787      12  FILLER                  PIC S9(04) COMP VALUE +30.
01788      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01789      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01790      12  FILLER                  PIC S9(04) COMP VALUE +10.
01791
01792      12  FILLER                  PIC  X(03) VALUE '175'.
01793      12  FILLER                  PIC S9(04) COMP VALUE +30.
01794      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01795      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01796      12  FILLER                  PIC S9(04) COMP VALUE +10.
01797
01798      12  FILLER                  PIC  X(03) VALUE '176'.
01799      12  FILLER                  PIC S9(04) COMP VALUE +30.
01800      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01801      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01802      12  FILLER                  PIC S9(04) COMP VALUE +10.
01803
01804      12  FILLER                  PIC  X(03) VALUE '177'.
01805      12  FILLER                  PIC S9(04) COMP VALUE +30.
01806      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01807      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01808      12  FILLER                  PIC S9(04) COMP VALUE +10.
01809
01810      12  FILLER                  PIC  X(03) VALUE '178'.
01811      12  FILLER                  PIC S9(04) COMP VALUE +30.
01812      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01813      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01814      12  FILLER                  PIC S9(04) COMP VALUE +10.
01815
01816      12  FILLER                  PIC  X(03) VALUE '179'.
01817      12  FILLER                  PIC S9(04) COMP VALUE +12.
01818      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01819      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01820      12  FILLER                  PIC S9(04) COMP VALUE +10.
01821
01822 *****COMPENSATION FINANCIAL RESPONSIBLE NO.
01823      12  FILLER                  PIC  X(03) VALUE '180'.
01824      12  FILLER                  PIC S9(04) COMP VALUE +10.
01825      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01826      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01827      12  FILLER                  PIC S9(04) COMP VALUE +10.

100705**** COMPENSATION BILLING SWITCH
100705     12  FILLER                  PIC XXX    VALUE '181'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +7.
100705     12  FILLER                  PIC X(30)  VALUE ALL '*'.
100705     12  FILLER                  PIC X      VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +10.

100705*****COMPENSATION FAX NUMBER
100705     12  FILLER                  PIC  X(03) VALUE '182'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +12.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.

100705**** COMPENSATION STATUS         
100705     12  FILLER                  PIC XXX    VALUE '183'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +7.
100705     12  FILLER                  PIC X(30)  VALUE ALL '*'.
100705     12  FILLER                  PIC X      VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +10.

01828
01829
01830  01  FILLER REDEFINES W-SUPPORTED-VARIABLES.
01831 *    12  W-VARIABLE-GRP OCCURS 180 TIMES
100705     12  W-VARIABLE-GRP OCCURS 183 TIMES
01832                         INDEXED BY W-VG-NDX.
01833          16  W-VARIABLE-ID         PIC  X(03).
01834          16  W-VARIABLE-SIZE       PIC S9(04) COMP.
01835          16  W-VG-TEXT.
01836              20  W-VAR-CHAR
01837                         OCCURS 30 TIMES
01838                         INDEXED BY W-VC-NDX
01839                                    PIC  X(01).
01840          16  W-VARIABLE-UPLOW-IND  PIC  X(01).
01841              88  W-USE-UPPER-AND-LOWER-CASE VALUE 'Y'.
01842
01843          16  W-VARIABLE-SOURCE   PIC S9(04) COMP.
01844
01845  01  W-VAR-END                   PIC  X(23)
01846                         VALUE ':VARIABLE WORK AREA END'.
01847
01848  01  W-PROGRAM-TABLE-AREA.
01849      12  FILLER                  PIC  X(15)
01850                                  VALUE 'PROGRAM TABLES:'.
01851
01852      12  W-RECORD-TABLE              VALUE SPACES.
01853          16  W-RC-GRP OCCURS 300 TIMES
01854                        INDEXED BY W-RG-NDX
01855                                   W-RG-NDX1.
01856              20  W-RC-TEXT.
01857                  24  W-RC-CHAR OCCURS 70 TIMES
01858                                 INDEXED BY W-RC-NDX
01859                                            W-RC-NDX1
01860                                  PIC  X(01).
01861              20  W-RC-PC         PIC  9(02).
01862              20  W-RC-SC         PIC  X(01).
01863
01864      12  FILLER REDEFINES W-RECORD-TABLE.
01865          16  W-REC-CHAR OCCURS 21900 TIMES
01866                         INDEXED BY W-RVS-NDX
01867                                    W-RVS-NDX2
01868                                  PIC  X(01).
01869      12  FILLER REDEFINES W-RECORD-TABLE.
01870          16  W-TS-GROUP OCCURS 6 TIMES
01871                         INDEXED BY W-TS-NDX
01872                                  PIC X(3650).
01873
01874      12  FILLER                  PIC  X(11)
01875                                  VALUE 'TEXT TABLE:'.
01876      12  W-TX-TABLE                  VALUE SPACES.
01877          16  W-TX-GRP OCCURS 300 TIMES
01878                        INDEXED BY W-TG-NDX
01879                                   W-TG-NDX2.
01880              20  W-TX-TEXT.
01881                  24  W-TX-CHAR OCCURS 70 TIMES
01882                                     INDEXED BY W-TX-NDX
01883                                                W-TX-NDX1
01884                                                W-TX-NDX2
01885                                  PIC  X(01).
01886              20  W-TX-PC         PIC  9(02).
01887              20  W-TX-SC         PIC  X(01).
01888
01889      12  FILLER                  PIC  X(11)
01890                                  VALUE 'FILE TABLE:'.
01891      12  W-FILE-TABLE                VALUE SPACES.
01892          16  W-FILE-USE-IND OCCURS 20 TIMES
01893                        INDEXED BY W-FILE-NDX
01894                                  PIC  X(01).
01895              88  W-FILE-NOT-USED     VALUE SPACE.
01896              88  W-FILE-USED         VALUE 'Y'.
01897
01898      12  FILLER                  PIC  X(14)
01899                                  VALUE 'END OF TABLES:'.
01900                                  EJECT


       01  WS-NEW-LETTER.
           05  FILLER OCCURS 300.
               10  WS-NEW-LETTER-LINE.
                   15  WS-NEW-LTR-TEXT PIC X(70).
                   15  WS-NEW-LTR-SC   PIC XX.
                   15  FILLER          PIC X.
       01  PGM-SUB                     PIC S999 COMP  VALUE +350.
       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.
01901  01  ERROR-MESSAGES.
01902      12  ER-0000                 PIC  X(04) VALUE '0000'.
01903      12  ER-0004                 PIC  X(04) VALUE '0004'.
01904      12  ER-0006                 PIC  X(04) VALUE '0006'.
01905      12  ER-0008                 PIC  X(04) VALUE '0008'.
01906      12  ER-0013                 PIC  X(04) VALUE '0013'.
01907      12  ER-0023                 PIC  X(04) VALUE '0023'.
01908      12  ER-0029                 PIC  X(04) VALUE '0029'.
01909      12  ER-0033                 PIC  X(04) VALUE '0033'.
01910      12  ER-0042                 PIC  X(04) VALUE '0042'.
01911      12  ER-0047                 PIC  X(04) VALUE '0047'.
01912      12  ER-0051                 PIC  X(04) VALUE '0051'.
01913      12  ER-0066                 PIC  X(04) VALUE '0066'.
01914      12  ER-0067                 PIC  X(04) VALUE '0067'.
01915      12  ER-0070                 PIC  X(04) VALUE '0070'.
01916      12  ER-0168                 PIC  X(04) VALUE '0168'.
01917      12  ER-0169                 PIC  X(04) VALUE '0169'.
01918      12  ER-0174                 PIC  X(04) VALUE '0174'.
01919      12  ER-0175                 PIC  X(04) VALUE '0175'.
01920      12  ER-0176                 PIC  X(04) VALUE '0176'.
01921      12  ER-0177                 PIC  X(04) VALUE '0177'.
01922      12  ER-0179                 PIC  X(04) VALUE '0179'.
01923      12  ER-0180                 PIC  X(04) VALUE '0180'.
01924      12  ER-0181                 PIC  X(04) VALUE '0181'.
01925      12  ER-0182                 PIC  X(04) VALUE '0182'.
01926      12  ER-0184                 PIC  X(04) VALUE '0184'.
01927      12  ER-0185                 PIC  X(04) VALUE '0185'.
01928      12  ER-0187                 PIC  X(04) VALUE '0187'.
01929      12  ER-0188                 PIC  X(04) VALUE '0188'.
01930      12  ER-0189                 PIC  X(04) VALUE '0189'.
01931      12  ER-0190                 PIC  X(04) VALUE '0190'.
01932      12  ER-0191                 PIC  X(04) VALUE '0191'.
01933      12  ER-0215                 PIC  X(04) VALUE '0215'.
01934      12  ER-0279                 PIC  X(04) VALUE '0279'.
01935      12  ER-0280                 PIC  X(04) VALUE '0280'.
01936      12  ER-0412                 PIC  X(04) VALUE '0412'.
01937      12  ER-0413                 PIC  X(04) VALUE '0413'.
01938      12  ER-0454                 PIC  X(04) VALUE '0454'.
01939      12  ER-0533                 PIC  X(04) VALUE '0533'.
01940      12  ER-0537                 PIC  X(04) VALUE '0537'.
01941      12  ER-2055                 PIC  X(04) VALUE '2055'.
01942      12  ER-2114                 PIC  X(04) VALUE '2114'.
01943      12  ER-2208                 PIC  X(04) VALUE '2208'.
01944      12  ER-2209                 PIC  X(04) VALUE '2209'.
01945      12  ER-2216                 PIC  X(04) VALUE '2216'.
01946      12  ER-2232                 PIC  X(04) VALUE '2232'.
01947      12  ER-2369                 PIC  X(04) VALUE '2369'.
01948      12  ER-2398                 PIC  X(04) VALUE '2398'.
01949      12  ER-2433                 PIC  X(04) VALUE '2433'.
01950      12  ER-2908                 PIC  X(04) VALUE '2908'.
01951      12  ER-2999                 PIC  X(04) VALUE '2999'.
01952      12  ER-3000                 PIC  X(04) VALUE '3000'.
01953      12  ER-3770                 PIC  X(04) VALUE '3770'.
01954      12  ER-3771                 PIC  X(04) VALUE '3771'.
01955      12  ER-3775                 PIC  X(04) VALUE '3775'.
01956      12  ER-3783                 PIC  X(04) VALUE '3783'.
01957      12  ER-7243                 PIC  X(04) VALUE '7243'.
01958      12  ER-7245                 PIC  X(04) VALUE '7245'.
01959      12  ER-7246                 PIC  X(04) VALUE '7246'.
01960      12  ER-7247                 PIC  X(04) VALUE '7247'.
01961      12  ER-7250                 PIC  X(04) VALUE '7250'.
01962      12  ER-7365                 PIC  X(04) VALUE '7365'.
01963      12  ER-7367                 PIC  X(04) VALUE '7367'.
01964      12  ER-7368                 PIC  X(04) VALUE '7368'.
01965      12  ER-7369                 PIC  X(04) VALUE '7369'.
01966      12  ER-7370                 PIC  X(04) VALUE '7370'.
01967      12  ER-7371                 PIC  X(04) VALUE '7371'.
01968      12  ER-7372                 PIC  X(04) VALUE '7372'.
01969      12  ER-7373                 PIC  X(04) VALUE '7373'.
01970      12  ER-7374                 PIC  X(04) VALUE '7374'.
01971      12  ER-7376                 PIC  X(04) VALUE '7376'.
01972      12  ER-7377                 PIC  X(04) VALUE '7377'.
01973      12  ER-7378                 PIC  X(04) VALUE '7378'.
01974      12  ER-7379                 PIC  X(04) VALUE '7379'.
01975      12  ER-7381                 PIC  X(04) VALUE '7381'.
01976      12  ER-7388                 PIC  X(04) VALUE '7388'.
01977      12  ER-7390                 PIC  X(04) VALUE '7390'.
01978      12  ER-7393                 PIC  X(04) VALUE '7393'.
01979      12  ER-7395                 PIC  X(04) VALUE '7395'.
01980      12  ER-7396                 PIC  X(04) VALUE '7396'.
01981      12  ER-7398                 PIC  X(04) VALUE '7398'.
01982      12  ER-8965                 PIC  X(04) VALUE '8965'.
01983      12  ER-9095                 PIC  X(04) VALUE '9095'.
01984      12  ER-9097                 PIC  X(04) VALUE '9097'.
01985      12  ER-9281                 PIC  X(04) VALUE '9281'.
01986      12  ER-9283                 PIC  X(04) VALUE '9283'.
01987      12  ER-9298                 PIC  X(04) VALUE '9298'.
01988      12  ER-9299                 PIC  X(04) VALUE '9299'.
01989      12  ER-9320                 PIC  X(04) VALUE '9320'.
01990      12  ER-9327                 PIC  X(04) VALUE '9327'.
01991      12  ER-9426                 PIC  X(04) VALUE '9426'.
01992      12  ER-9427                 PIC  X(04) VALUE '9427'.
01993                                  EJECT
01994      COPY ELCAID.
01995  01  FILLER    REDEFINES DFHAID.
01996      12  FILLER                  PIC  X(08).
01997      12  PF-VALUES               PIC  X(01) OCCURS 2.
01998                                  EJECT
01999      COPY ELCATTR.
02000                                  EJECT
02002                                  EJECT
02003      COPY ELCNWA.

                                    COPY ELCFUNDT.


02004                                  EJECT
02005      COPY ELCEMIB.
02006  01  EMI-SAVE-AREA               PIC X(400).
02007                                  EJECT
02008      COPY ELCLOGOF.
02009                                  EJECT
02010      COPY ELCSCTM.
02011                                  EJECT
02012      COPY ELCSCRTY.
02013                                  EJECT
02014
02015  01  W-TS-WORK-AREA              PIC X(3650).
02016                                  EJECT
                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0050-SELECT-RECS    THRU 0050-EXIT UNTIL
               END-OF-INPUT

           DISPLAY ' ARCH IN RECORDS   ' ARCH-IN-CNT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK

             .
       0010-INITIALIZE.

           ACCEPT FUNCTION-DATE                                         

           DISPLAY '     '.                                             
           DISPLAY ' ACCEPT-DATE - ' FUNCTION-DATE                      
           DISPLAY '     '.                                             

           MOVE WS-FN-DATE             TO  DC-GREG-DATE-CYMD
           MOVE 'L'                    TO  DC-OPTION-CODE
           MOVE ' '                    TO  DC-ERROR-CODE
      
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF DATE-CONVERSION-ERROR                                     
               DISPLAY 'DATE CONVERSION ERROR: ' DC-ERROR-CODE          
               MOVE '75' TO RETURN-CODE                                 
               GO TO ABEND-PGM.                                         
     
           MOVE DC-BIN-DATE-1          TO BIN-RUN-DATE
                                          WS-CURRENT-BIN-DATE

      *    DISPLAY ' ACCEPT DATE ' WS-ACCEPT-DATE
      *
      *    MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
      *    MOVE '3'                    TO DC-OPTION-CODE
      *    PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
      *    IF NO-CONVERSION-ERROR
      *       MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
      *    ELSE
      *       DISPLAY ' ERROR CONVERTING CURRENT DATE '
      *    END-IF

           ACCEPT WS-TIME-OF-DAY       FROM  TIME
           MOVE WS-TIME                TO  WS-CURRENT-TIME

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

      *    PERFORM 0120-START-ERARCH   THRU 0120-EXIT
           PERFORM 0110-READ-ERARCH    THRU 0110-EXIT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ARCH-IN
                      ERARCTI
                      ERACCT
                      ELCERT
                      ERCOMP
                      ERPNDB
                      ERMAIL
                      ELLETR
                      ERCHEK
                      ERPYAJ
               I-O
                      ERARCH
                      ERARCT
                      ELCNTL
               OUTPUT ARCH-EXT-OUT

           IF ERARCTI-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCTI OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCTI-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERACCT OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPYAJ-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPYAJ OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPYAJ-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCHEK-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERCHEK OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERCHEK-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCH OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCT OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPNDB OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERMAIL OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERMAIL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR OPEN ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE      ARCH-IN
                      ERARCTI
                      ERACCT
                      ELCNTL
                      ELCERT
                      ERCOMP
                      ERPNDB
                      ERMAIL
                      ERARCH
                      ERARCT
                      ELLETR
                      ERCHEK
                      ERPYAJ
                      ARCH-EXT-OUT


           IF ERARCTI-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCTI CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCTI-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERACCT CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPYAJ-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPYAJ CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPYAJ-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCHEK-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERCHEK CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERCHEK-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCERT CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERCOMP CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERCOMP-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPNDB CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERMAIL CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERMAIL-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCH CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERARCT CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR CLOSE ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0050-SELECT-RECS.

           EVALUATE TRUE
              WHEN (LA-FORM-A3 = 'NH01')
                   AND (LA-INITIAL-PRINT-DATE = LOW-VALUES)
                 DISPLAY ' FOUND NEW HAMP REFUND ' LA-CERT-NO-A2
                 MOVE SPACES           TO WS-FORM-TO-RESEND
                 MOVE 'NH01'           TO WS-FORM-TO-CREATE
                 PERFORM 0040-RELEASE  THRU 0040-EXIT
              WHEN (LA-FORM-A3 = '9999')
                 OR (LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
                 CONTINUE
      *       WHEN LA-ARCHIVE-NO = +29
      *          MOVE LA-RESEND-LETR   TO WS-FORM-TO-RESEND
      *          DISPLAY ' FOUND 29  ' LA-CERT-NO-A2
      *          PERFORM 0040-RELEASE  THRU 0040-EXIT
              WHEN (LA-STATUS = 'A')
                 AND (LA-SENT-DATE = LOW-VALUES)
                 AND (LA-REPLY-DATE = LOW-VALUES)
                 AND (LA-RESEND-DATE <= WS-CURRENT-BIN-DATE)
                 AND (LA-RESEND-LETR NOT = SPACES AND LOW-VALUES)
                 MOVE LA-RESEND-LETR TO WS-FORM-TO-RESEND
                 DISPLAY ' FOUND ONE ' LA-CERT-NO-A2
                 MOVE LA-ARCHIVE-NO    TO WS-SAVE-ARCH-NO
                 PERFORM 0040-RELEASE  THRU 0040-EXIT
      *       WHEN ((LA-SENT-DATE-2 = LOW-VALUES)
      *          AND (LA-RESEND-DATE-2 <= WS-CURRENT-BIN-DATE)
      *          AND (LA-RESEND-LETR-2 NOT = SPACES AND LOW-VALUES))
      *          MOVE LA-RESEND-LETR-2 TO WS-FORM-TO-RESEND
      *          SET SECOND-RESEND     TO TRUE
      *          DISPLAY ' FOUND TWO ' LA-CERT-NO-A2
      *          PERFORM 0040-RELEASE  THRU 0040-EXIT
      *       WHEN((LA-SENT-DATE-3 = LOW-VALUES)
      *          AND (LA-RESEND-DATE-3 <= WS-CURRENT-BIN-DATE)
      *          AND (LA-RESEND-LETR-3 NOT = SPACES AND LOW-VALUES))
      *          MOVE LA-RESEND-LETR-3 TO WS-FORM-TO-RESEND
      *          SET THIRD-RESEND      TO TRUE
      *          DISPLAY ' FOUND THREE ' LA-CERT-NO-A2
      *          PERFORM 0040-RELEASE  THRU 0040-EXIT
              WHEN OTHER
                 CONTINUE
           END-EVALUATE

           PERFORM 0110-READ-ERARCH    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0110-READ-ERARCH.

           READ ARCH-IN INTO LETTER-ARCHIVE AT END
              SET END-OF-INPUT         TO TRUE.
              
           IF NOT END-OF-INPUT
              ADD 1                    TO ARCH-IN-CNT
              IF LA-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERARCH.

           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD

           START ERARCH KEY >= LA-CONTROL-PRIMARY

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 MOVE ' ERARCH START ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERARCH-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       0040-RELEASE.

071811     MOVE ' '                    TO WS-CERT-STATUS-SW
                                          W-CERT-FOUND-SW

           IF LA-FORM-A3 NOT = 'NH01'
071811        PERFORM 0200-CHECK-CERT-STATUS
                                       THRU 0200-EXIT

082211        IF (CERT-CANCELLED)
                 OR (NOT W-CERT-FOUND)
071811           DISPLAY 'CERT CANCELLED OR NOT FOUND WILL NOT RESEND '
071811              LA-CERT-NO-A2 ' ' WS-SAVE-ARCH-NO ' ' LA-FORM-A3
071811           MOVE 'C'                 TO LA-STATUS
                 MOVE WS-REPLY-DATE       TO LA-REPLY-DATE
PEMTST           REWRITE LETTER-ARCHIVE
071811           IF ERARCH-FILE-STATUS NOT = '00'
071811              DISPLAY ' ERROR ON ERARCH - REWRITE - CANCEL '
071811                 ERARCH-FILE-STATUS
071811           ELSE
071811              DISPLAY ' SUCCESS REWRITE FOR CANCELS OR NOT FOUND'
071811                 LA-CERT-NO-A2
071811           END-IF
071811           GO TO 0040-EXIT
071811        END-IF
           END-IF

           MOVE LA-CONTROL-PRIMARY     TO WS-SAVE-ERARCH-KEY
02817      MOVE ZEROS                  TO PI-CURRENT-LINE
02818                                     PI-TEMP-STOR-ITEMS
02819                                     PI-TOTAL-LINES
02820                                     PI-UPDATE-SW
02821                                     PI-689-NUMBER-LABEL-LINES
02822                                     PI-689-ERROR
02823                                     PI-689-NUMBER-TEXT-RECORDS.
02824      MOVE SPACES                 TO PI-689-PRINT-SW
02825                                     PI-689-ALT-PRINTER-ID
02826                                     PI-COMM-CONTROL
02827                                     PI-689-FORM-NUMBER
02828                                     PI-689-TEMP-STOR-ID
02829                                     PI-689-LABEL-SOURCE
02830                                     PI-689-USE-SCREEN-IND.

03051      MOVE SPACES                 TO PI-689-PRINT-SW
03052                                     PI-689-LBL-OVERRIDE.
03053      MOVE ZEROS                  TO PI-TOTAL-LINES
03054                                     PI-CURRENT-LINE
03055                                     PI-TEMP-STOR-ITEMS
03056                                     PI-UPDATE-SW
03057                                     PI-689-ERROR
03058                                     PI-689-NUMBER-LABEL-LINES
03059                                     PI-689-NUMBER-TEXT-RECORDS

           PERFORM 2000-CREATE         THRU 2000-EXIT

           PERFORM 5400-LETTER-RELEASE THRU 5400-EXIT

           MOVE WS-SAVE-ERARCH-KEY     TO LA-CONTROL-PRIMARY
           READ ERARCH
           IF ERARCH-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERARCH - READ - 0040 '
                 ERARCH-FILE-STATUS
           ELSE
              MOVE WS-CURRENT-BIN-DATE TO LA-SENT-DATE
PEMTST        MOVE 'C'                 TO LA-STATUS
              REWRITE LETTER-ARCHIVE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ERARCH - REWRITE - 0040 '
                    ERARCH-FILE-STATUS
              END-IF
           END-IF

           .
03208  0040-EXIT.
03209      EXIT.

071811 0200-CHECK-CERT-STATUS.
071811
           display ' made it to 0200 '
           MOVE +0                     TO WS-COV-CNT
           MOVE LOW-VALUES             TO WS-REPLY-DATE
071811     PERFORM 6450-READ-CERT-ONLY
           IF W-CERT-FOUND
              IF CM-LF-BENEFIT-CD NOT = '00' AND SPACES
                 ADD +1                TO WS-COV-CNT
              END-IF
              IF CM-AH-BENEFIT-CD NOT = '00' AND SPACES
                 ADD +1                TO WS-COV-CNT
              END-IF
              IF (CERT-PEND-ISSUE-ERROR)
                           AND
                 ((CERT-CANCELLED-ONLINE)
                 OR (CERT-PEND-CANCEL-ERROR))
071811           MOVE CM-CONTROL-PRIMARY
071811                                 TO PB-CONTROL-BY-ACCOUNT
071811           MOVE +0               TO PB-ALT-CHG-SEQ-NO
071811           MOVE '2'              TO PB-RECORD-TYPE
071811           READ ERPNDB
071811           IF ERPNDB-FILE-STATUS = '00'
071811              IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
                       SUBTRACT +1     FROM WS-COV-CNT
071811                 MOVE PB-C-LF-CANCEL-DT
071811                                 TO WS-REPLY-DATE
                    ELSE
                       IF CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES
                          MOVE +0      TO WS-COV-CNT
                          MOVE CM-LF-DEATH-DT TO WS-REPLY-DATE
                       ELSE
                          IF (CM-LF-LOAN-EXPIRE-DT NOT =
                             LOW-VALUES AND SPACES)
                             AND (CM-LF-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                             SUBTRACT +1 FROM WS-COV-CNT
                             MOVE CM-LF-LOAN-EXPIRE-DT TO WS-REPLY-DATE
                          END-IF
                       END-IF
                    END-IF
071811              IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
                       SUBTRACT +1     FROM WS-COV-CNT
071811                 MOVE PB-C-AH-CANCEL-DT
071811                                 TO WS-REPLY-DATE
                    ELSE
                       IF (CM-AH-SETTLEMENT-DT NOT =
                          LOW-VALUES AND SPACES)
                          SUBTRACT +1        FROM WS-COV-CNT
                          MOVE CM-AH-SETTLEMENT-DT TO WS-REPLY-DATE
                       ELSE
                          IF (CM-AH-LOAN-EXPIRE-DT NOT =
                             LOW-VALUES AND SPACES)
                             AND (CM-AH-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                             SUBTRACT +1 FROM WS-COV-CNT
                             MOVE CM-AH-LOAN-EXPIRE-DT TO WS-REPLY-DATE
                          END-IF
                       END-IF
071811              END-IF
                 ELSE
                    DISPLAY ' BAD READ ON ERPNDB ' CM-STATE ' '
                       CM-ACCOUNT ' ' CM-CERT-NO
                 END-IF
              ELSE
                 IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
                    SUBTRACT +1        FROM WS-COV-CNT
                    MOVE CM-LF-CANCEL-DT TO WS-REPLY-DATE
                 ELSE
                    IF CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES
                       MOVE +0         TO WS-COV-CNT
                       MOVE CM-LF-DEATH-DT TO WS-REPLY-DATE
                    ELSE
                       IF (CM-LF-LOAN-EXPIRE-DT NOT =
                          LOW-VALUES AND SPACES)
                          AND (CM-LF-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                          SUBTRACT +1 FROM WS-COV-CNT
                          MOVE CM-LF-LOAN-EXPIRE-DT TO WS-REPLY-DATE
                       END-IF
                    END-IF
                 END-IF
                 IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
                    SUBTRACT +1        FROM WS-COV-CNT
                    MOVE CM-AH-CANCEL-DT TO WS-REPLY-DATE
                 ELSE
                    IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES AND SPACES
                       SUBTRACT +1        FROM WS-COV-CNT
                       MOVE CM-AH-SETTLEMENT-DT TO WS-REPLY-DATE
                    ELSE
                       IF (CM-AH-LOAN-EXPIRE-DT NOT =
                          LOW-VALUES AND SPACES)
                          AND (CM-AH-LOAN-EXPIRE-DT < BIN-RUN-DATE)
                          SUBTRACT +1 FROM WS-COV-CNT
                          MOVE CM-AH-LOAN-EXPIRE-DT TO WS-REPLY-DATE
                       END-IF
                    END-IF
                 END-IF
              END-IF
              IF WS-COV-CNT <= +0
                 SET CERT-CANCELLED    TO TRUE
              END-IF
071811     ELSE
071811        DISPLAY ' ERROR READING ELCERT ' LA-CERT-NO-A2
071811     END-IF


071811     .
071811 0200-EXIT.
071811     EXIT.

04165  2000-CREATE.
04166 ***************************************************************
04167 *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
04168 *    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
04169 *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
04170 *    WILL BE DISPLAYED ON THE SCREEN.                         *
04171 *                                                             *
04172 ***************************************************************
04173
04174 ***************************************************************
04175 *    CHECK TO SEE IF IT IS THE SAME REQUEST OR NOT.           *
04176 *    IF IT IS A NEW REQUEST AND A LETTER WAS PRINTED          *
04177 *    THEN FORCE AN ERROR.                                     *
04178 ***************************************************************

04221  2000-GET-TEXT.
04222
04223      MOVE SPACES                 TO W-FILE-TABLE
04224                                     W-LINE-CONTROL-RECORD
04225                                     W-RECORD-TABLE
04226                                     W-SINGLE-LINE
04227                                     W-SQUEEZED-LINE
04228                                     W-TS-WORK-AREA
04229                                     W-TX-TABLE.
04230      SET W-TG-NDX                TO +1
04231      MOVE ZEROS                  TO W-INITIAL-COLUMN
04232                                     W-LINE-COUNT
04233                                     W-LINE-INDENT-1
04234                                     W-LINE-INDENT-2
04235                                     W-LINE-INDENT-3
04236                                     W-LINE-INDENT-4
04237                                     W-LINE-INDENT-5
04238                                     W-PAGE
04239                                     W-PARAGRAPH-INDENT
04240                                     W-TOP-MARGIN
04241                                     W-WORK-INDENT.
04242      MOVE 'Y'                    TO W-LC-CASE-IND.
04243      MOVE +70                    TO W-LAST-COLUMN.
04244      MOVE +56                    TO W-MAX-LINES-PER-PAGE.
04245      MOVE +1                     TO W-START-COLUMN.
04246      MOVE +71                    TO W-TOO-FAR.
04247
           MOVE LOW-VALUES             TO TX-CONTROL-PRIMARY
04248      MOVE DTE-CLASIC-COMPANY-CD  TO TX-COMPANY-CD

           IF WS-FORM-TO-RESEND = SPACES
              MOVE WS-FORM-TO-CREATE   TO TX-LETTER-NO
           ELSE
04249         MOVE WS-FORM-TO-RESEND   TO TX-LETTER-NO
           END-IF

04250      MOVE TX-CONTROL-PRIMARY (1:5)
                                       TO W-TEXT-SAVE-KEY
04251      MOVE 'N'                    TO W-TEXT-BROWSED-SW.
04252

           START ELLETR KEY >= TX-CONTROL-PRIMARY
           
           IF ELLETR-FILE-STATUS = '00'
              CONTINUE
           ELSE
              GO TO 2000-NOT-FOUND
           END-IF

           MOVE ' '                    TO WS-FOUND-PROMPT-SW

           .
       2000-READ-NEXT.

           READ ELLETR NEXT RECORD

           IF ELLETR-FILE-STATUS = '00'
              CONTINUE
           ELSE
              IF ELLETR-FILE-STATUS = '10' OR '23'
                 GO TO 2000-ENDBR
              ELSE
                 DISPLAY ' ERROR ON ELLETR - READNEXT - 2000 '
                    ELLETR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
04282
04283      IF TX-CONTROL-PRIMARY (1:5) NOT = W-TEXT-SAVE-KEY
04284          GO TO 2000-ENDBR.
04285
04286      MOVE 'Y'                    TO W-TEXT-BROWSED-SW.
04287
04288      MOVE TX-FORM-SQUEEZE-CONTROL
04289                                  TO W-FORM-SQUEEZE-IND.
04290
04293
04294      IF  TX-FORM-CONTROL-LINE
04295          PERFORM 2100-PROCESS-FORM-CONTROL-LINE THRU 2100-EXIT
04296          GO TO 2000-READ-NEXT.
04297
04298      IF  TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
04299          PERFORM 2150-PROCESS-Z-CONTROLS THRU 2150-EXIT
04300          GO TO 2000-READ-NEXT.
04301
04302      MOVE TX-TEXT-LINE           TO W-TX-TEXT (W-TG-NDX).
04303      MOVE TX-PROCESS-CONTROL     TO W-TX-PC (W-TG-NDX).
04304      PERFORM 2200-CHECK-FOR-VARIABLE THRU 2200-EXIT.
04305      MOVE TX-LINE-SQUEEZE-CONTROL
04306                                  TO W-TX-SC (W-TG-NDX).
04307
04308      SET W-TG-NDX UP BY 1.
04309      GO TO 2000-READ-NEXT.
04310
04311  2000-ENDBR.

04324      SET  W-TOTAL-TX-LINES       TO W-TG-NDX.
04325      SUBTRACT +1 FROM W-TOTAL-TX-LINES.

031011     MOVE '******** TEXT LINES BEFORE ******'
031011                                 TO ARCH-OUT-RECORD
031011     WRITE ARCH-OUT-RECORD
031011     PERFORM VARYING W-TG-NDX FROM +1 BY +1 UNTIL
031011        W-TG-NDX > W-TOTAL-TX-LINES
031011        MOVE W-TX-GRP (W-TG-NDX)    TO ARCH-OUT-RECORD
031011        WRITE ARCH-OUT-RECORD
031011        IF W-TX-GRP (W-TG-NDX) (1:6) = '&&&&&&'
031011           SET FOUND-PROMPT         TO TRUE
031011           DISPLAY ' SETTING FOUND PROMPT TO TRUE '
031011        END-IF
031011     END-PERFORM
031011
031011     MOVE '******** END OF TEXT LINES BEFORE ******'
031011                                 TO ARCH-OUT-RECORD
031011     WRITE ARCH-OUT-RECORD
031011
031011     IF FOUND-PROMPT
031011        PERFORM 5700-GET-PROMPT-TEXT
031011                                 THRU 5700-EXIT
031011     END-IF
031011
031011     MOVE '******** TEXT LINES AFTER  ******'
031011                                 TO ARCH-OUT-RECORD
031011     WRITE ARCH-OUT-RECORD
031011     PERFORM VARYING W-TG-NDX FROM +1 BY +1 UNTIL
031011        W-TG-NDX > W-TOTAL-TX-LINES
031011        MOVE W-TX-GRP (W-TG-NDX)    TO ARCH-OUT-RECORD
031011        WRITE ARCH-OUT-RECORD
031011     END-PERFORM
031011
031011     MOVE '******** END OF TEXT LINES AFTER ******'
031011                                 TO ARCH-OUT-RECORD
031011     WRITE ARCH-OUT-RECORD

04326      MOVE 1                      TO PI-CURRENT-LINE.
04327
04333
04334 ***************************************************************
04335 *    IF IT IS A FORM WITH VARIABLES, THEN RESOLVE ALL OF      *
04336 *    VARIABLE SYMBOLS AND INSERT THEM INTO THE TEXT DATA.     *
04337 ***************************************************************
04338
04339      PERFORM 6000-RESOLVE-VARIABLES THRU 6000-EXIT.

           DISPLAY ' MADE IT BACK FROM 6000 '
04340
04354
           SET W-RG-NDX                TO +1

           SET WS-WORK-NDX TO W-RG-NDX
           DISPLAY ' WORK NDX ' WS-WORK-NDX
           DISPLAY ' W TOP MARGIN ' W-TOP-MARGIN
04355      MOVE W-TOP-FORM             TO W-RC-TEXT (W-RG-NDX).
04356      SET W-RG-NDX UP BY W-TOP-MARGIN.
04357
           DISPLAY ' ABOUT TO PERFORM 7400 '

04358      PERFORM 7400-CREATE-LETTER THRU 7400-EXIT.
04359
04360      SET PI-TOTAL-LINES          TO W-RG-NDX.
04361
04362      IF  W-FIRST-BAD-VARIABLE-FOUND
04363          SET W-RG-NDX            TO W-FIRST-BAD-VARIABLE
04364          MOVE W-FIRST-BAD-VARIABLE
04365                                  TO PI-CURRENT-LINE
04366      ELSE
04367          SET W-RG-NDX            TO 1.

           GO TO 2000-EXIT

           .
       2000-NOT-FOUND.

           DISPLAY ' RESEND LETTER NOT FOUND ' WS-FORM-TO-RESEND

           .
04386  2000-EXIT.
04387      EXIT.
04388
           .
04397  2100-PROCESS-FORM-CONTROL-LINE.
04398
04399      MOVE TX-TEXT-LINE           TO W-LINE-CONTROL-RECORD.
04400
04401      IF  W-LC-LINE-WIDTH NUMERIC
04402              AND
04403          W-LC-LINE-WIDTH GREATER THAN ZEROS
04404          COMPUTE W-INITIAL-COLUMN
04405              = (70 - W-LC-LINE-WIDTH) / 2
04406          COMPUTE W-START-COLUMN = W-INITIAL-COLUMN + 1
04407          COMPUTE W-LAST-COLUMN ROUNDED
04408              = W-INITIAL-COLUMN + W-LC-LINE-WIDTH
04409          COMPUTE W-TOO-FAR = W-LAST-COLUMN + 1
04410      ELSE
04411          MOVE 70                 TO W-LC-LINE-WIDTH.
04412
04413      IF  W-LC-PARAGRAPH-INDENT NUMERIC
04414              AND
04415          W-LC-PARAGRAPH-INDENT GREATER THAN ZEROS
04416          MOVE W-LC-PARAGRAPH-INDENT
04417                                  TO W-PARAGRAPH-INDENT.
04418
04419      IF  W-LC-MAX-LINES-PER-PAGE NUMERIC
04420              AND
04421          W-LC-MAX-LINES-PER-PAGE GREATER THAN ZEROS
04422          MOVE W-LC-MAX-LINES-PER-PAGE
04423                                  TO W-MAX-LINES-PER-PAGE.
04424
04425      IF  W-LC-LINE-ADJUST NUMERIC
04426              AND
04427          W-LC-LINE-ADJUST GREATER THAN ZEROS
04428          COMPUTE W-TOTAL-LINE-LENGTH
04429              = W-LC-LINE-WIDTH + (W-LC-LINE-ADJUST * 2)
04430          IF  W-TOTAL-LINE-LENGTH LESS THAN +71
04431              ADD W-LC-LINE-ADJUST
04432                                  TO W-INITIAL-COLUMN
04433                                     W-LAST-COLUMN
04434                                     W-START-COLUMN
04435                                     W-TOO-FAR.
04436
04437      IF  W-LC-LINE-INDENT-1 NUMERIC
04438              AND
04439          W-LC-LINE-INDENT-1 LESS THAN W-LC-LINE-WIDTH
04440          MOVE W-LC-LINE-INDENT-1 TO W-LINE-INDENT-1
04441      ELSE
04442          MOVE +0                 TO W-LINE-INDENT-1.
04443
04444      IF  W-LC-LINE-INDENT-2 NUMERIC
04445              AND
04446          W-LC-LINE-INDENT-2 LESS THAN W-LC-LINE-WIDTH
04447          MOVE W-LC-LINE-INDENT-2 TO W-LINE-INDENT-2
04448      ELSE
04449          MOVE +0                 TO W-LINE-INDENT-2.
04450
04451      IF  W-LC-LINE-INDENT-3 NUMERIC
04452              AND
04453          W-LC-LINE-INDENT-3 LESS THAN W-LC-LINE-WIDTH
04454          MOVE W-LC-LINE-INDENT-3 TO W-LINE-INDENT-3
04455      ELSE
04456          MOVE +0                 TO W-LINE-INDENT-3.
04457
04458      IF  W-LC-LINE-INDENT-4 NUMERIC
04459              AND
04460          W-LC-LINE-INDENT-4 LESS THAN W-LC-LINE-WIDTH
04461          MOVE W-LC-LINE-INDENT-4 TO W-LINE-INDENT-4
04462      ELSE
04463          MOVE +0                 TO W-LINE-INDENT-4.
04464
04465      IF  W-LC-LINE-INDENT-5 NUMERIC
04466              AND
04467          W-LC-LINE-INDENT-5 LESS THAN W-LC-LINE-WIDTH
04468          MOVE W-LC-LINE-INDENT-5 TO W-LINE-INDENT-5
04469
04470      ELSE
04471          MOVE +0                 TO W-LINE-INDENT-5.
04472
04473      IF  W-LC-TOP-MARGIN NUMERIC
04474          MOVE W-LC-TOP-MARGIN    TO W-TOP-MARGIN
04475      ELSE
04476          MOVE +0                 TO W-TOP-MARGIN.
04477
04478      IF W-LC-CASE-IND = 'Y' OR 'N'
04479          NEXT SENTENCE
04480      ELSE
04481          MOVE PI-LOWER-CASE-LETTERS
04482                                  TO W-LC-CASE-IND.
04483
04484  2100-EXIT.
04485      EXIT.
04486                                  EJECT
04487  2150-PROCESS-Z-CONTROLS.

04489      MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
           
           .
04643  2150-EXIT.
04644      EXIT.
04645                                  EJECT
04646  2200-CHECK-FOR-VARIABLE.
04647
04648      IF  W-TX-TEXT (W-TG-NDX) EQUAL SPACES
04649          GO TO 2200-EXIT.
04650
04651      SET W-TX-NDX                TO +1.
04652
04653  2200-CONTINUE.
04654
04655      SEARCH W-TX-CHAR
04656          VARYING
04657              W-TX-NDX
04658          AT END
04659              GO TO 2200-EXIT
04660          WHEN
04661              W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
04662              NEXT SENTENCE.
04663
04664      SET W-TX-NDX UP BY +1.
04665
04666      IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
04667          PERFORM 2220-BUILD-VARIABLE-NUMBER THRU 2220-EXIT
04668          GO TO 2200-CONTINUE.
04669
04670  2200-EXIT.
04671      EXIT.
04672                                  EJECT
04673  2220-BUILD-VARIABLE-NUMBER.
04674
04675      SET W-TX-NDX UP BY +1.
04676      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V1.
04677      SET W-TX-NDX UP BY +1.
04678      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V2.
04679      SET W-TX-NDX UP BY +1.
04680      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V3.
04681
04682      IF  W-VAR-RELATIVE-NUM NUMERIC
04683          SET W-FILE-NDX TO W-VARIABLE-SOURCE (W-VAR-RELATIVE-NUM)
04684          MOVE 'Y' TO W-FILE-USE-IND (W-FILE-NDX).
04685
04686  2220-EXIT.
04687      EXIT.
04688                                  EJECT
04821  5400-LETTER-RELEASE.
04822 ***************************************************************
04823 *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
04824 *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS IN       *
04825 *    THE ARCHIVE FILE.                                        *
04826 *    THE FUNCTIONS BELOW WILL BE PERFORMED.                   *
04827 *        1. CHECK SECURITY.                                   *
04828 *        2. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS.        *
04829 *        3. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
04830 *        4. WRITE THE NEW ARCHIVE RECORDS FROM W-TS-TABLE.    *
04831 *        5. RESET INSURED'S CONTROL FIELDS AND RETURN THE     *
04832 *           ARCHIVE NUMBER USED  TO FILE THE RECORDS.         *
04833 ***************************************************************
04834
04853

04872      MOVE SPACES                 TO W-REMAINING-VAR-SW.
04873      PERFORM 7915-SEARCH-REMAINING-VARS THRU 7915-EXIT
04874              VARYING
04875          W-RVS-NDX FROM +1 BY +1
04876              UNTIL
04877          W-RVS-NDX GREATER THAN +21900
04878              OR
04879          W-REMAINING-VAR-FOUND.
04880
04884
           IF WS-FORM-TO-CREATE = SPACES
04885         MOVE SPACES              TO CF-CONTROL-PRIMARY
04886         MOVE DTE-CLIENT          TO CF-COMPANY-ID
04887         MOVE '1'                 TO CF-RECORD-TYPE
04888         MOVE ZEROS               TO CF-SEQUENCE-NO
04889         
              READ ELCNTL
              
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ELCNTL - READ - 5400'
                 PERFORM ABEND-PGM
              END-IF
              
04902         IF  CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
04903             MOVE ZEROS           TO CF-CREDIT-LAST-ARCH-NUM
04904                                     CF-CREDIT-START-ARCH-NUM
              END-IF
04905         
04906         ADD 1                    TO CF-CREDIT-LAST-ARCH-NUM
04907         MOVE CF-CREDIT-LAST-ARCH-NUM
04908                                  TO W-ARCH-NUMBER
04909                                     W-ARCT-NUMBER
04910         
04911         REWRITE CONTROL-FILE
              
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ELCNTL - REWRITE - 5400'
                 PERFORM ABEND-PGM
              END-IF

           ELSE
              MOVE LA-ARCHIVE-NO       TO W-ARCH-NUMBER
                                          W-ARCT-NUMBER
           END-IF

04940      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
04941                                     LA-ARCHIVE-NO-A2
04942                                     LA-ARCHIVE-NO-A3
04943                                     LA-ARCHIVE-NO-A4
04944                                     LA-ARCHIVE-NO-A5
04945                                     LA-ARCHIVE-NO-A6

05003      MOVE ZEROS                  TO W-SEQ-CTR.
05004
05007      MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
05008                                     LA-INITIAL-PRINT-DATE
05009                                     LA-SENT-DATE
05012                                     LA-REPLY-DATE
                                          LA-RESEND-DATE
                                          LA-FOLLOW-UP-DATE
071811                                    LA-PURGED-DATE
071811                                    LA-VOIDED-DATE
           MOVE SPACES                 TO LA-RESEND-LETR


           IF W-FORM-TO-RESEND NOT = SPACES
              MOVE W-FORM-TO-RESEND    TO LA-RESEND-LETR
           END-IF
05013      MOVE 'A'                    TO LA-STATUS.
05014
           IF W-DAYS-TO-RESEND NOT NUMERIC
              MOVE ZEROS               TO W-DAYS-TO-RESEND
           END-IF
           IF W-DAYS-TO-RESEND > 0
              MOVE WS-CURRENT-BIN-DATE TO DC-BIN-DATE-1
              MOVE W-DAYS-TO-RESEND    TO DC-ELAPSED-DAYS
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO LA-RESEND-DATE
                 DISPLAY ' FORM DATE ' LA-RESEND-LETR ' '
                    DC-GREG-DATE-1-EDIT
              END-IF
           END-IF

           IF W-NUMBER-OF-COPIES NOT NUMERIC
              MOVE 1                   TO W-NUMBER-OF-COPIES
           END-IF
           MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
05022
           IF WS-FORM-TO-CREATE = SPACES
05023         MOVE WS-FORM-TO-RESEND   TO LA-FORM-A3
           ELSE
              MOVE WS-FORM-TO-CREATE   TO LA-FORM-A3
           END-IF

05032      MOVE WS-CURRENT-BIN-DATE    TO LA-CREATION-DATE
05033
05040      MOVE PI-689-NUMBER-LABEL-LINES
05041                                  TO LA-NUMBER-LABEL-LINES.
05042      MOVE ZEROS                  TO PI-689-NUMBER-TEXT-RECORDS.
05043
05044      PERFORM 5410-CREATE-TEXT-RECORDS THRU 5410-EXIT.
05045
05046      MOVE PI-689-NUMBER-TEXT-RECORDS
05047                                  TO LA-NO-OF-TEXT-RECORDS.
05052
           IF WS-FORM-TO-CREATE = SPACES
05053         PERFORM 5650-WRITE-ARCHIVE
                                       THRU 5650-EXIT
           ELSE
              PERFORM 5652-REWRITE-ARCHIVE
                                       THRU 5652-EXIT
           END-IF
05054
05055      IF  PI-689-CREATE-NO-SCREENS
05056          GO TO 5400-END.
05063
05064  5400-END.
05065
05066      MOVE '2'                    TO PI-ACTION.
05067      MOVE ZEROS                  TO PI-TOTAL-LINES
05068                                     PI-CURRENT-LINE.
05069      MOVE SPACES                 TO PI-689-PRINT-SW
05070                                     PI-689-FORM-NUMBER

           .
05079  5400-EXIT.
05080      EXIT.
05081                                  EJECT
05082  5410-CREATE-TEXT-RECORDS.

           DISPLAY ' MADE IT TO 5410 '

05110      PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT.
05111
05112      MOVE ZEROS                  TO W-SEQ-CTR
05113
05126      SET W-RG-NDX                TO +1
05127
05128      SET LT-NDX                  TO W-ZEROS.
05129
           PERFORM VARYING W-RG-NDX FROM +1 BY +1 UNTIL
              W-RG-NDX > PI-TOTAL-LINES
              MOVE W-RC-GRP (W-RG-NDX)  TO ARCH-OUT-RECORD
              WRITE ARCH-OUT-RECORD
           END-PERFORM

           SET W-RG-NDX TO +1
05130      PERFORM 5500-FORMAT-TEXT THRU 5500-EXIT
05131              VARYING
05132          W-RG-NDX FROM W-RG-NDX BY +1
05133              UNTIL
05134          W-RG-NDX GREATER THAN PI-TOTAL-LINES.
05135
05136  5410-EXIT.
05137      EXIT.
05138                                  EJECT
05139  5415-INITIALIZE-TEXT.
05140
05141      MOVE SPACES                 TO LETTER-ARCHIVE-TEXT.
05142      MOVE 'LT'                   TO LT-RECORD-ID.
05143
05144      MOVE LA-ARCHIVE-NO          TO LT-ARCHIVE-NO.
05145      MOVE DTE-CLASIC-COMPANY-CD  TO LT-COMPANY-CD.
05146
05147      MOVE ZEROS                  TO LT-LINE-SEQ-NO
05148                                     LT-NUM-LINES-ON-RECORD.
05149
05150  5415-EXIT.
05151      EXIT.
05152                                  EJECT
05183  5500-FORMAT-TEXT.
05184
05185      SET LT-NDX UP BY +1.
05186
05187      IF  W-RC-TEXT (W-RG-NDX) EQUAL W-TOP-FORM
05188          SET W-RG-NDX UP BY 1
05189          MOVE 99                 TO LT-SKIP-CONTROL (LT-NDX)
05190          MOVE W-RC-TEXT (W-RG-NDX)
05191                                  TO LT-TEXT-LINE (LT-NDX)
05192      ELSE
05193          IF  W-RC-TEXT (W-RG-NDX) EQUAL SPACES
05194              MOVE ZEROS          TO W-PRINT-CONTROL
05195              PERFORM 5505-FIND-USED-LINE THRU 5505-EXIT
05196                      VARYING
05197                  W-RG-NDX FROM W-RG-NDX BY 1
05198                      UNTIL
05199                  W-RG-NDX GREATER THAN PI-TOTAL-LINES
05200                      OR
05201                  W-RC-TEXT (W-RG-NDX) GREATER THAN SPACES
05202              IF  W-RG-NDX GREATER THAN PI-TOTAL-LINES
05203                  GO TO 5500-WRITE-TEST
05204              ELSE
05205                  MOVE W-PRINT-CONTROL
05206                                  TO LT-SKIP-CONTROL (LT-NDX)
05207                  MOVE W-RC-TEXT (W-RG-NDX)
05208                                  TO LT-TEXT-LINE (LT-NDX)
05209          ELSE
05210              MOVE W-RC-TEXT (W-RG-NDX)
05211                                  TO LT-TEXT-LINE (LT-NDX)
05212              MOVE ZEROS          TO W-PRINT-CONTROL.
05213
05214      ADD +1                      TO LT-NUM-LINES-ON-RECORD.
05215
05216  5500-WRITE-TEST.
05217
05218      IF  W-RG-NDX NOT < PI-TOTAL-LINES
05219          IF  LT-TEXT-RECORD NOT EQUAL SPACES
05220              ADD +1              TO PI-689-NUMBER-TEXT-RECORDS
05221              MOVE '2'            TO LT-RECORD-TYPE
05222              ADD +1              TO W-SEQ-CTR
05223              MOVE W-SEQ-CTR      TO LT-LINE-SEQ-NO
05224              PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
05225          ELSE
05226              NEXT SENTENCE
05227      ELSE
05228          IF  LT-NDX EQUAL +20
05229              ADD +1              TO PI-689-NUMBER-TEXT-RECORDS
05230              MOVE '2'            TO LT-RECORD-TYPE
05231              ADD +1              TO W-SEQ-CTR
05232              MOVE W-SEQ-CTR      TO LT-LINE-SEQ-NO
05233              PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
05234              SET LT-NDX          TO W-ZEROS
05235              PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT.
05236
05237  5500-EXIT.
05238       EXIT.
05239                                  EJECT
05240  5505-FIND-USED-LINE.
05241
05242      ADD +1                      TO W-PRINT-CONTROL.
05243
05244  5505-EXIT.
05245       EXIT.
05246                                  EJECT
05247  5600-FORMAT-ADDRESS-LINE.
05248
05249      SET LT-NDX                  TO W-RG-NDX.
05250      MOVE W-RC-TEXT (W-RG-NDX)   TO LT-TEXT-LINE (LT-NDX).
05251
05252  5600-EXIT.
05253      EXIT.
05254                                  EJECT
05255  5650-WRITE-ARCHIVE.
05256
           WRITE LETTER-ARCHIVE
           
           IF ERARCH-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON WRITE - ERARCH '
                 ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       5650-EXIT.
           EXIT.

05255  5652-REWRITE-ARCHIVE.
05256
           REWRITE LETTER-ARCHIVE
           
           IF ERARCH-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON REWRITE - ERARCH '
                 ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       5652-EXIT.
           EXIT.

       5655-WRITE-ARCHIVE-TEXT.

           WRITE LETTER-ARCHIVE-TEXT

           IF ERARCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON WRITE - ERARCT '
                 ERARCT-FILE-STATUS
              MOVE LT-ARCHIVE-NO       TO WS-DISP-ARCH-NO
              MOVE LT-LINE-SEQ-NO      TO WS-DISP-SEQ-NO
              DISPLAY ' ARCH ' WS-DISP-ARCH-NO ' SEQ NO ' WS-DISP-SEQ-NO
              ' REC TYPE ' LT-RECORD-TYPE
              PERFORM ABEND-PGM
           END-IF

           .
05274  5655-EXIT.
05275      EXIT.

031011 5700-GET-PROMPT-TEXT.
031011
031011     MOVE DTE-CLASIC-COMPANY-CD  TO LI-COMPANY-CD
031011     MOVE WS-SAVE-ARCH-NO        TO LI-ARCHIVE-NO
031011     MOVE '2'                    TO LI-RECORD-TYPE
031011     MOVE +0                     TO LI-LINE-SEQ-NO
031011     START ERARCTI KEY >= LI-CONTROL-PRIMARY
031011     IF ERARCTI-FILE-STATUS NOT = '00'
031011        DISPLAY ' ERARCTI - ERROR - START ' ERARCTI-FILE-STATUS
031011           ' ' WS-SAVE-ARCH-NO
031011        GO TO ABEND-PGM
031011     END-IF
031011
031011     MOVE ' ' TO WS-FIND-SW
031011
031011     PERFORM UNTIL I-FOUND-IT OR I-DIDNT-FIND-IT
031011        READ ERARCTI NEXT RECORD
031011        IF (ERARCTI-FILE-STATUS = '00')
031011           AND (WS-SAVE-ARCH-NO = LI-ARCHIVE-NO)
031011           PERFORM VARYING LI-NDX FROM +1 BY +1 UNTIL
031011              (LI-TEXT-LINE (LI-NDX) (1:6) = '&&&&&&')
031011              OR (LI-NDX > +20)
031011           END-PERFORM
031011           IF LI-NDX < +21
031011              SET I-FOUND-IT     TO TRUE
031011           END-IF
031011        ELSE
031011           SET I-DIDNT-FIND-IT   TO TRUE
031011        END-IF
031011     END-PERFORM
031011
031011     IF I-DIDNT-FIND-IT
031011        DISPLAY ' SOMETHING WENT WRONG FINDING THE 1ST &&&&&& '
031011           WS-SAVE-ARCH-NO
071811*       PERFORM ABEND-PGM
031011     END-IF
031011
031011     MOVE +1                     TO N1
031011     MOVE SPACES                 TO WS-NEW-LETTER
031011     PERFORM VARYING W-TG-NDX FROM +1 BY +1 UNTIL
031011        (W-TX-GRP (W-TG-NDX) (1:6) = '&&&&&&')
031011        OR (W-TG-NDX > W-TOTAL-TX-LINES)
031011        MOVE W-TX-GRP (W-TG-NDX) TO WS-NEW-LETTER-LINE (N1)
031011        ADD +1 TO N1
031011     END-PERFORM
031011     IF W-TX-GRP (W-TG-NDX) (1:6) NOT = '&&&&&&'
031011        DISPLAY ' COULDNT FIND &&&&&& IN NEW LETTR '
031011           WS-SAVE-ARCH-NO
071811*       GO TO ABEND-PGM
031011     END-IF
031011
031011     PERFORM UNTIL I-AM-FINISHED
031011        IF LI-SKIP-CONTROL (LI-NDX) NOT NUMERIC
031011           MOVE ZEROS TO LI-SKIP-CONTROL (LI-NDX)
031011        END-IF
031011*       DISPLAY ' LI TEXT LINE ' LI-TEXT-LINE (LI-NDX)
031011*       DISPLAY ' LI TEXT SC   ' LI-SKIP-CONTROL (LI-NDX)
031011        MOVE LI-TEXT-LINE (LI-NDX) TO WS-NEW-LETTER-LINE (N1)
031011        MOVE LI-SKIP-CONTROL (LI-NDX) TO WS-NEW-LTR-SC (N1)
031011*       DISPLAY ' NL TEXT LINE ' WS-NEW-LETTER-LINE (N1)
031011*       DISPLAY ' NL SKIP C    ' WS-NEW-LTR-SC (N1)
031011        ADD +1 TO N1
031011        SET LI-NDX UP BY +1
031011        IF LI-NDX > +20
031011           SET LI-NDX TO +1
031011           READ ERARCTI NEXT RECORD
031011        END-IF
031011        IF (LI-TEXT-LINE (LI-NDX) (1:6) = '&&&&&&')
031011           OR (ERARCTI-FILE-STATUS NOT = '00')
031011           OR (WS-SAVE-ARCH-NO NOT = LI-ARCHIVE-NO)
031011           SET I-AM-FINISHED     TO TRUE
031011        END-IF
031011     END-PERFORM
031011
031011     IF LI-TEXT-LINE (LI-NDX) (1:6) NOT = '&&&&&&'
031011        DISPLAY ' DIDNT FIND THE END OF &&&&&& IN OLD LTR '
031011           WS-SAVE-ARCH-NO ' BYPASSING '
031011*       GO TO ABEND-PGM
              GO TO 5700-EXIT
031011     END-IF
031011     MOVE LI-TEXT-LINE (LI-NDX)  TO WS-NEW-LETTER-LINE (N1)
031011     MOVE LI-SKIP-CONTROL (LI-NDX) TO WS-NEW-LTR-SC (N1)
031011
031011     ADD +1 TO N1
031011
031011     SET W-TG-NDX UP BY +1
031011     PERFORM VARYING W-TG-NDX FROM W-TG-NDX BY +1 UNTIL
031011        (W-TX-GRP (W-TG-NDX) (1:6) = '&&&&&&')
031011        OR (W-TG-NDX > W-TOTAL-TX-LINES)
031011     END-PERFORM
031011
031011     IF W-TX-GRP (W-TG-NDX) (1:6) NOT = '&&&&&&'
031011        DISPLAY ' DIDNT FIND THE END OF &&&&&& IN RC '
031011           WS-SAVE-ARCH-NO
031011        GO TO ABEND-PGM
031011     END-IF
031011     SET W-TG-NDX UP BY +1
031011
031011     PERFORM VARYING W-TG-NDX FROM W-TG-NDX BY +1 UNTIL
031011        (W-TG-NDX > W-TOTAL-TX-LINES)
031011        MOVE W-TX-GRP (W-TG-NDX) TO WS-NEW-LETTER-LINE (N1)
031011        ADD +1 TO N1
031011     END-PERFORM
031011
031011     MOVE WS-NEW-LETTER          TO W-TX-TABLE
031011
031011     .
031011 5700-EXIT.
031011     EXIT.

05277  6000-RESOLVE-VARIABLES.
05278 ***************************************************************
05279 *    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *
05280 *    WITH DATA PERTAINING TO THE FORM WITH VARIABLES.         *
05281 *    THIS ROUTINE IS PERFORM THRU 6000-EXIT IN ORDER TO       *
05282 *    RESOLVE ALL OF THE SYMBOLS.                              *
05283 *                                                             *
05284 ***************************************************************
05285
           DISPLAY ' MADE IT TO 6000 '

05289      PERFORM 6200-GET-COMPANY-DATA THRU 6200-EXIT.
05290      PERFORM 6250-GET-CNTL2-DATA THRU 6250-EXIT.
05291      PERFORM 6400-GET-PENDING-DATA THRU 6400-EXIT.
05292      PERFORM 6450-GET-CERT-DATA THRU 6450-EXIT.
05293      PERFORM 6500-GET-ACCOUNT-DATA THRU 6500-EXIT.
072308     PERFORM 6350-GET-CSR-DATA   THRU 6350-EXIT
05294      PERFORM 6600-GET-COMPENSATION-DATA THRU 6600-EXIT.
05295      PERFORM 6700-GET-MAIL-DATA THRU 6700-EXIT.
05296      PERFORM 6300-GET-CARRIER-DATA THRU 6300-EXIT.
05297      PERFORM 6750-GET-LIFE-BENEFIT-DATA THRU 6750-EXIT.
05298      PERFORM 6800-GET-A-H-BENEFIT-DATA THRU 6800-EXIT.
05299      PERFORM 6850-GET-CHECK-DATA THRU 6850-EXIT.
05300      PERFORM 6900-GET-PYAJ-DATA THRU 6900-EXIT.
05301      PERFORM 6950-MOVE-SYSTEM-DATA THRU 6950-EXIT.
05302
05303      MOVE SPACES                 TO W-REVERSE-DATE-SW.
05304
05305  6000-EXIT.
05306      EXIT.
05307                                  EJECT
05308  6200-GET-COMPANY-DATA.
05309
05317      IF  W-FILE-NOT-USED (1)
05318          GO TO 6200-EXIT.

           MOVE SPACES                 TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE +0                     TO CF-SEQUENCE-NO

           READ ELCNTL

           IF ELCNTL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ELCNTL - READ - 6200 '
                 ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
05335
05336      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
05337      MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (1)
05338                                     W-VG-TEXT (25).
05339      MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (2).
05340      MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (3).
05341      MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (4).
05342      MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (5).
05343
05344      IF  CF-CL-CAN-POST-CODE
05345          MOVE CF-CL-CANADIAN-POSTAL-CODE
05346                                  TO W-CANADIAN-POSTAL-CODES
05347          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
05348          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
05349          MOVE SPACES             TO W-LAB-CAN-DASH (6)
05350                                     W-LAB-CAN-FILLER (6)
05351          GO TO 6200-CONTINUE.
05352
05353      IF CF-CL-ZIP-CODE NOT = SPACES AND ZEROS AND LOW-VALUES
05354          MOVE CF-CL-ZIP-CODE     TO W-WORK-ZIP
05355          MOVE SPACES             TO W-LABEL-ZIP (6)
05356          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
05357
05358          IF  W-WORK-ZIP4 GREATER THAN '0000'
05359              MOVE '-'            TO W-LABEL-DASH (6)
05360              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
05361              GO TO 6200-CONTINUE
05362          ELSE
05363              MOVE SPACES         TO W-LABEL-DASH (6)
05364                                     W-LABEL-2ND-ZIP (6)
05365              GO TO 6200-CONTINUE.
05366
05367      IF  CF-CL-ZIP-CODE-NUM NOT NUMERIC
05368          GO TO 6200-CONTINUE.
05369
05370      MOVE CF-CL-ZIP-CODE-NUM     TO W-WORK-ZIP-NUMERIC.
05371
05372      IF  W-WORK-ZIP-NUMERIC LESS THAN +100000
05373          COMPUTE W-WORK-ZIP-NUMERIC
05374              = W-WORK-ZIP-NUMERIC * 10000.
05375
05376      MOVE SPACES                 TO W-LABEL-ZIP (6).
05377      MOVE W-WORK-ZIP5            TO W-LABEL-1ST-ZIP (6).
05378
05379      IF  W-WORK-ZIP4 GREATER THAN '0000'
05380          MOVE '-'                TO W-LABEL-DASH (6)
05381          MOVE W-WORK-ZIP4        TO W-LABEL-2ND-ZIP (6)
05382          GO TO 6200-CONTINUE
05383      ELSE
05384          MOVE SPACES             TO W-LABEL-DASH (6)
05385                                     W-LABEL-2ND-ZIP (6)
05386          GO TO 6200-CONTINUE.
05387
05388  6200-CONTINUE.
05389
05390      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
05391
05407      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (1)
05408      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (2)
05409      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (3)
05410      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (4)
05411      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (5)
05412      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (6).
05413
           .
05424  6200-EXIT.
05425      EXIT.

05427  6250-GET-CNTL2-DATA.

05432      IF  W-FILE-NOT-USED (11)
05433          GO TO 6250-EXIT.
05434
020812     if la-processor-cd = 'LMLC'
020812        MOVE 'JLMC'              TO LA-PROCESSOR-CD
020812     END-IF
           MOVE SPACES                 TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           MOVE LA-PROCESSOR-CD        TO CF-PROCESSOR
           MOVE +0                     TO CF-SEQUENCE-NO

           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ELCNTL - READ - 6250 '
020812           ELCNTL-FILE-STATUS ' ' LA-PROCESSOR-CD
020812           MOVE 'LGXX'           TO W-VG-TEXT (153)
020812           MOVE ' CUSTOMER SERVICE REP '
020812                                 TO W-VG-TEXT (151)
020812                                    W-VG-TEXT (152)
020812        GO TO 6250-EXIT
020812*       PERFORM ABEND-PGM
           END-IF
05451
05452      MOVE CF-PROCESSOR           TO W-VG-TEXT (153).
05453      MOVE CF-PROCESSOR-NAME      TO W-VG-TEXT (151).
05454      MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (152).

           .
05465  6250-EXIT.
05466      EXIT.
05467                                  EJECT
05468  6300-GET-CARRIER-DATA.
05469
05477      IF W-FILE-NOT-USED (4)
05478         GO TO 6300-EXIT
           END-IF

           MOVE SPACES                 TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '6'                    TO CF-RECORD-TYPE
           MOVE LA-CARRIER-A2          TO CF-CARRIER-CNTL
           MOVE +0                     TO CF-SEQUENCE-NO

           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ELCNTL - READ - 6300 '
                 ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

05503      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
05504      MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (1).
05505      MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (2).
05506      MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (3).
05507      MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (4).
05508      MOVE CF-CITY-STATE          TO W-LABEL-LINES (5).
05509
05510      IF  CF-CANADIAN-POST-CODE
05511          MOVE CF-CANADIAN-POSTAL-CODE
05512                                  TO W-CANADIAN-POSTAL-CODES
05513          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
05514          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
05515          MOVE SPACES             TO W-LAB-CAN-DASH (6)
05516                                     W-LAB-CAN-FILLER (6)
05517          GO TO 6300-CONTINUE.
05518
05519      IF CF-ZIP-CODE NOT = SPACES AND ZEROS AND LOW-VALUES
05520          MOVE CF-ZIP-CODE        TO W-WORK-ZIP
05521          MOVE SPACES             TO W-LABEL-ZIP (6)
05522          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
05523          IF  W-WORK-ZIP4 GREATER THAN '0000'
05524              MOVE '-'            TO W-LABEL-DASH (6)
05525              MOVE W-WORK-ZIP4
05526                                  TO W-LABEL-2ND-ZIP (6)
05527              GO TO 6300-CONTINUE
05528          ELSE
05529              MOVE SPACES         TO W-LABEL-DASH (6)
05530                                     W-LABEL-2ND-ZIP (6)
05531              GO TO 6300-CONTINUE.
05532
05533      IF  CF-ZIP-CODE-NUM NOT NUMERIC
05534          GO TO 6300-CONTINUE.
05535
05536      MOVE CF-ZIP-CODE-NUM TO W-WORK-ZIP-NUMERIC.
05537
05538      IF  W-WORK-ZIP-NUMERIC LESS THAN +100000
05539          COMPUTE W-WORK-ZIP-NUMERIC
05540              = W-WORK-ZIP-NUMERIC * 10000.
05541
05542      MOVE SPACES                 TO W-LABEL-ZIP (6).
05543      MOVE W-WORK-ZIP5            TO W-LABEL-1ST-ZIP (6).
05544
05545      IF  W-WORK-ZIP4 > '0000'
05546          MOVE '-'                TO W-LABEL-DASH (6)
05547          MOVE W-WORK-ZIP4        TO W-LABEL-2ND-ZIP (6)
05549      ELSE
05550          MOVE SPACES             TO W-LABEL-DASH (6)
05551                                     W-LABEL-2ND-ZIP (6)
           END-IF

           .
05554  6300-CONTINUE.
05555
05556      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
05557
05573      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (20)
05574      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (21)
05575      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (22)
05576      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (23)
05577      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (24)
05578      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (25).
05579
05580      IF  W-FULL-DATA
05581          MOVE ZEROS              TO W-PHONE-IN
05582          MOVE CF-PHONE-NO        TO W-PHONE-IN
05583          MOVE W-PHI-AREA         TO W-PO-AREA
05584          MOVE W-PHI-PFX          TO W-PO-PFX
05585          MOVE W-PHI-SFX          TO W-PO-SFX
05586          MOVE W-PHONE-OUT        TO W-VG-TEXT (26).

           .
05598  6300-EXIT.
05599      EXIT.

072308 6350-GET-CSR-DATA.

072308     IF W-VG-TEXT (48) (1:4) = '****' OR SPACES
072308        GO TO 6350-EXIT
072308     END-IF

072308     IF W-FILE-NOT-USED (11)
072308        GO TO 6350-EXIT
072308     END-IF

           MOVE SPACES                 TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           MOVE W-VG-TEXT (48)         TO CF-ACCESS-CD-GENL
           MOVE +0                     TO CF-SEQUENCE-NO

           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              MOVE SPACES              TO W-VG-TEXT (154)
              GO TO 6350-EXIT
           ELSE
              IF ELCNTL-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' ERROR ON ELCNTL - READ - 6350 '
                    ELCNTL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (154)

           .
072308 6350-EXIT.
072308     EXIT.

05601  6400-GET-PENDING-DATA.
05602
05610      IF W-FILE-NOT-USED (9)
05611         GO TO 6400-EXIT
           END-IF

           .
05619  6400-READ-PNDB-ONLY.
05620
           MOVE LA-COMPANY-CD          TO PB-COMPANY-CD-A1
           MOVE LA-CARRIER-A2          TO PB-CARRIER
           MOVE LA-GROUPING-A2         TO PB-GROUPING
           MOVE LA-STATE-A2            TO PB-STATE
           MOVE LA-ACCOUNT-A2          TO PB-ACCOUNT
           MOVE LA-EFFECT-DATE-A2      TO PB-CERT-EFF-DT
           MOVE LA-CERT-NO-A2          TO PB-CERT-NO
           MOVE +0                     TO PB-ALT-CHG-SEQ-NO
           MOVE '1'                    TO PB-RECORD-TYPE

           START ERPNDB KEY IS NOT < PB-CONTROL-BY-ACCOUNT
           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERPNDB - START - 6400 '
                 ERPNDB-FILE-STATUS
              GO TO 6400-EXIT
           END-IF
           
           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERPNDB - READ - 6400 '
                 ERPNDB-FILE-STATUS
              GO TO 6400-EXIT
           END-IF

           IF (PB-COMPANY-CD      = LA-COMPANY-CD)
              AND (PB-CARRIER     = LA-CARRIER-A2)
              AND (PB-GROUPING    = LA-GROUPING-A2)
              AND (PB-STATE       = LA-STATE-A2)
              AND (PB-ACCOUNT     = LA-ACCOUNT-A2)
              AND (PB-CERT-EFF-DT = LA-EFFECT-DATE-A2)
              AND (PB-CERT-NO     = LA-CERT-NO-A2)
              MOVE 'Y'                 TO W-PNDB-FOUND-SW
           ELSE
              DISPLAY ' ERPNDB RECORD NOT FOUND '
              GO TO 6400-EXIT
           END-IF

           .
05649  6400-ONLY-STOP.
05650
05651      MOVE PB-ENTRY-BATCH         TO W-VG-TEXT (130).
05652
05653      IF  NOT PB-ISSUE
05654          GO TO 6400-CANCEL.
05655
05656      MOVE PB-I-BIRTHDAY          TO DC-BIN-DATE-1.
05657      MOVE SPACES                 TO DC-OPTION-CODE.
05658      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05659
05660      IF  NO-CONVERSION-ERROR
05661          MOVE DC-GREG-DATE-1-EDIT
05662                                  TO W-VG-TEXT (110).
05663
05664      MOVE PB-I-AGE               TO W-EDIT-3-0.
05665      MOVE W-EDIT-3-0             TO W-VG-TEXT (124).
05666      MOVE PB-I-LF-PREMIUM-AMT    TO W-EDIT-7-2.
05667      MOVE W-EDIT-7-2             TO W-VG-TEXT (111).
05668      MOVE PB-I-AH-PREMIUM-AMT    TO W-EDIT-7-2.
05669      MOVE W-EDIT-7-2             TO W-VG-TEXT (112).
05670      MOVE PB-I-LF-PREM-CALC      TO W-EDIT-7-2.
05671      MOVE W-EDIT-7-2             TO W-VG-TEXT (113).
05672      MOVE PB-I-AH-PREM-CALC      TO W-EDIT-7-2.
05673      MOVE W-EDIT-7-2             TO W-VG-TEXT (114).
05674      MOVE PB-I-LF-BENEFIT-AMT    TO W-EDIT-9-2.
05675      MOVE W-EDIT-9-2             TO W-VG-TEXT (125).
05676      MOVE PB-I-AH-BENEFIT-AMT    TO W-EDIT-7-2.
05677      MOVE W-EDIT-7-2             TO W-VG-TEXT (126).
05678      MOVE PB-I-LF-RATE           TO W-EDIT-2-5-S.
05679      MOVE W-EDIT-2-5-S           TO W-VG-TEXT (127).
05680      MOVE PB-I-AH-RATE           TO W-EDIT-2-5-S.
05681      MOVE W-EDIT-2-5-S           TO W-VG-TEXT (128).
05682      MOVE PB-I-LOAN-TERM         TO W-EDIT-3-0.
05683      MOVE W-EDIT-3-0             TO W-VG-TEXT (129).
05684
05685      COMPUTE W-DIFFERENCE
05686          = PB-I-LF-PREM-CALC - PB-I-LF-PREMIUM-AMT.
05687
05688      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05689      MOVE W-EDIT-7-2             TO W-VG-TEXT (115).
05690
05691      COMPUTE W-DIFFERENCE
05692          = PB-I-AH-PREM-CALC - PB-I-AH-PREMIUM-AMT.
05693
05694      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05695      MOVE W-EDIT-7-2             TO W-VG-TEXT (116).
05696
05697  6400-CANCEL.
05698
05699      IF  NOT PB-CANCELLATION
05700          GO TO 6400-EXIT.
05701
05702      MOVE PB-CI-AH-PRIOR-CANCEL-DT
05703                                  TO DC-BIN-DATE-1.
05704      MOVE SPACES                 TO DC-OPTION-CODE.
05705      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05706
05707      IF  NO-CONVERSION-ERROR
05708          MOVE DC-GREG-DATE-1-EDIT
05709                                  TO W-VG-TEXT (117).
05710
05711      MOVE PB-C-LF-CANCEL-AMT     TO W-EDIT-7-2.
05712      MOVE W-EDIT-7-2             TO W-VG-TEXT (118).
05713      MOVE PB-C-AH-CANCEL-AMT     TO W-EDIT-7-2.
05714      MOVE W-EDIT-7-2             TO W-VG-TEXT (119).
05715      MOVE PB-C-LF-REF-CALC       TO W-EDIT-7-2.
05716      MOVE W-EDIT-7-2             TO W-VG-TEXT (120).
05717      MOVE PB-C-AH-REF-CALC       TO W-EDIT-7-2.
05718      MOVE W-EDIT-7-2             TO W-VG-TEXT (121).

072908     COMPUTE W-EDIT-7-2 = PB-C-LF-CANCEL-AMT + PB-C-AH-CANCEL-AMT
072908     MOVE W-EDIT-7-2             TO W-VG-TEXT (131)
05719
062017     IF PB-C-INT-ON-REFS NOT NUMERIC
062017        MOVE ZEROS               TO PB-C-INT-ON-REFS
072908     END-IF

062017     MOVE PB-C-INT-ON-REFS       TO W-EDIT-7-2
072908     MOVE W-EDIT-7-2             TO W-VG-TEXT (132)

072908     MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
072908     IF PB-C-AH-CANCEL-DT > DC-BIN-DATE-1
072908        MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
072908     END-IF
072908     MOVE SPACES                 TO DC-OPTION-CODE
05705      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05706
072908     IF NO-CONVERSION-ERROR
072908        MOVE DC-GREG-DATE-1-EDIT TO W-VG-TEXT (133)
072908     END-IF

05720      COMPUTE W-DIFFERENCE = PB-C-LF-REF-CALC
05721                            - PB-C-LF-CANCEL-AMT.
05722
05723      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05724      MOVE W-EDIT-7-2             TO W-VG-TEXT (122).
05725
05726      COMPUTE W-DIFFERENCE = PB-C-AH-REF-CALC
05727                            - PB-C-AH-CANCEL-AMT.
05728
05729      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05730      MOVE W-EDIT-7-2             TO W-VG-TEXT (123).
05731
05732  6400-EXIT.
05733      EXIT.

05735  6450-GET-CERT-DATA.

05736      IF  W-FILE-NOT-USED (8)
05737          GO TO 6450-EXIT.
05738
05745  6450-READ-CERT-ONLY.

           MOVE LA-COMPANY-CD          TO CM-COMPANY-CD
           MOVE LA-CARRIER-A2          TO CM-CARRIER
           MOVE LA-GROUPING-A2         TO CM-GROUPING
           MOVE LA-STATE-A2            TO CM-STATE
           MOVE LA-ACCOUNT-A2          TO CM-ACCOUNT
           MOVE LA-EFFECT-DATE-A2      TO CM-CERT-EFF-DT
           MOVE LA-CERT-NO-A2          TO CM-CERT-NO

           READ ELCERT
           IF ELCERT-FILE-STATUS = '00'
              MOVE 'Y'                 TO W-CERT-FOUND-SW
           ELSE
              DISPLAY ' ERROR ON ELCERT - READ - 6450 '
                 ELCERT-FILE-STATUS
           END-IF

           .
05760  6450-ONLY-STOP.
05761
05762      MOVE CM-INSURED-LAST-NAME   TO W-VG-TEXT (108).
05763
05764      MOVE CM-BENEFICIARY         TO W-VG-TEXT (109).
05765
05766      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05767      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05768      MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
05769      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05770      MOVE WS-NAME-WORK           TO W-VG-TEXT (95).
05771
05772      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05773      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05774      MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
05775      PERFORM 7100-FORMAT-LAST-NAME-1ST THRU 7100-EXIT.
05776      MOVE WS-NAME-WORK           TO W-VG-TEXT (94).
05777
05778      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05779      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05780      MOVE '.'                    TO W-NAME-FIRST-REMAIN.
05781      MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
05782      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05783      MOVE WS-NAME-WORK           TO W-VG-TEXT (90).
05784
05785      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05786      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05787      MOVE SPACES                 TO W-NAME-MIDDLE.
05788      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05789      MOVE WS-NAME-WORK           TO W-VG-TEXT (104).
05790
05791      MOVE CM-INSURED-FIRST-NAME  TO W-VG-TEXT (91).
05792      MOVE CM-INSURED-INITIAL2    TO W-VG-TEXT (92).
05793
05794      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05795      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05796      MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
05797      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05798      MOVE WS-NAME-WORK           TO W-VG-TEXT (103)
05799                                     W-LABEL-JOINT-NAME.
05800
05801      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05802      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05803      MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
05804      PERFORM 7100-FORMAT-LAST-NAME-1ST THRU 7100-EXIT.
05805      MOVE WS-NAME-WORK           TO W-VG-TEXT (102).
05806
05807      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05808      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05809      MOVE '.'                    TO W-NAME-FIRST-REMAIN.
05810      MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
05811      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05812      MOVE WS-NAME-WORK           TO W-VG-TEXT (99).
05813
05814      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05815      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05816      MOVE SPACES                 TO W-NAME-MIDDLE.
05817      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05818      MOVE WS-NAME-WORK           TO W-VG-TEXT (105).
05819
05820      MOVE CM-JT-FIRST-NAME       TO W-VG-TEXT (100).
05821      MOVE CM-JT-INITIAL          TO W-VG-TEXT (101).
05822
05823      COMPUTE W-WORK-AMOUNT
05824          = CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT.
05825      MOVE W-WORK-AMOUNT          TO W-EDIT-9-2.
05826      MOVE W-EDIT-9-2             TO W-VG-TEXT (97).
05827      MOVE CM-AH-PREMIUM-AMT      TO W-EDIT-7-2.
05828      MOVE W-EDIT-7-2             TO W-VG-TEXT (98).
05829
05830      IF  CM-SEX-FEMAL
05831          MOVE 'MS.'              TO W-VG-TEXT (96)
05832
05833      ELSE
05834          MOVE 'MR.'              TO W-VG-TEXT (96).
05835
05836      MOVE CM-CARRIER OF CERTIFICATE-MASTER
05837                                  TO W-VG-TEXT (70).
05838      MOVE CM-GROUPING            TO W-VG-TEXT (71).
05839      MOVE CM-ACCOUNT             TO W-VG-TEXT (72).
05840      MOVE CM-CERT-NO             TO W-VG-TEXT (73).
05841      MOVE CM-INSURED-ISSUE-AGE   TO W-DISPLAY-3.
05842      MOVE W-DISPLAY-3            TO W-VG-TEXT (83).
05843      MOVE CM-LOAN-NUMBER         TO W-VG-TEXT (86).
05844      MOVE CM-LOAN-BALANCE        TO W-EDIT-7-2.
05845      MOVE W-EDIT-7-2             TO W-VG-TEXT (87).
05846      MOVE CM-MEMBER-NO           TO W-VG-TEXT (88).
05847      MOVE CM-SOC-SEC-NO          TO W-VG-TEXT (89).
05848      MOVE CM-POLICY-FORM-NO      TO W-VG-TEXT (83).
05849      MOVE CM-LF-ORIG-TERM        TO W-EDIT-3-0.
05850      MOVE W-EDIT-3-0             TO W-VG-TEXT (77).
05851      MOVE CM-LF-BENEFIT-AMT      TO W-EDIT-9-2.
05852      MOVE W-EDIT-9-2             TO W-VG-TEXT (79).
05853      MOVE CM-AH-ORIG-TERM        TO W-EDIT-3-0.
05854      MOVE W-EDIT-3-0             TO W-VG-TEXT (78).
05855      MOVE CM-AH-BENEFIT-AMT      TO W-EDIT-7-2.
05856      MOVE W-EDIT-7-2             TO W-VG-TEXT (80).
05857      MOVE CM-AH-ITD-CANCEL-AMT   TO W-EDIT-7-2.
05858      MOVE W-EDIT-7-2             TO W-VG-TEXT (107).
05859      MOVE CM-LF-ITD-CANCEL-AMT   TO W-EDIT-7-2.
05860      MOVE W-EDIT-7-2             TO W-VG-TEXT (106).
05861
05862      COMPUTE W-WORK-AMOUNT = CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT.
05863      MOVE W-WORK-AMOUNT          TO W-EDIT-9-2.
05864      MOVE W-EDIT-9-2             TO W-VG-TEXT (93).
05865
05866      MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1.
05867      MOVE SPACES                 TO DC-OPTION-CODE.
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05869
05870      IF  NO-CONVERSION-ERROR
05871          MOVE DC-GREG-DATE-1-EDIT
05872                                  TO W-VG-TEXT (81).
05873
05874      MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1.
05875      MOVE SPACES                 TO DC-OPTION-CODE.
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05877
05878      IF  NO-CONVERSION-ERROR
05879          MOVE DC-GREG-DATE-1-EDIT
05880                                  TO W-VG-TEXT (82).
05881
05882      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
05883      MOVE SPACES                 TO DC-OPTION-CODE.
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05885
05886      IF  NO-CONVERSION-ERROR
05887          MOVE DC-GREG-DATE-1-EDIT
05888                                  TO W-VG-TEXT (74).
05889
05890      MOVE CM-LF-ORIG-TERM        TO DC-ELAPSED-MONTHS.
05891
05892      IF  CM-PMT-EXTENSION-DAYS NUMERIC
05893          MOVE CM-PMT-EXTENSION-DAYS
05894                                  TO DC-ELAPSED-DAYS
05895      ELSE
05896          MOVE ZEROS              TO DC-ELAPSED-DAYS.
05897
05898      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
05899      MOVE '6'                    TO DC-OPTION-CODE.
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05901
05902      IF  NO-CONVERSION-ERROR
05903          MOVE DC-GREG-DATE-1-EDIT
05904                                  TO W-VG-TEXT (75).
05905
05906      MOVE CM-AH-ORIG-TERM        TO DC-ELAPSED-MONTHS.
05907      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
05908      MOVE '6'                    TO DC-OPTION-CODE.
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
05910
05911      IF  NO-CONVERSION-ERROR
05912          MOVE DC-GREG-DATE-1-EDIT
05913                                  TO W-VG-TEXT (76).
05914
05915  6450-EXIT.
05916      EXIT.
05917                                  EJECT
05918  6500-GET-ACCOUNT-DATA.
05919
05927      IF W-FILE-NOT-USED (6)
05928         GO TO 6500-EXIT
031504     END-IF

           .
05930  6500-READ-ACCT-ONLY.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE LA-CONTROL-BY-KEY-FIELDS (1:20)
                                       TO AM-CONTROL-PRIMARY (1:20)
           MOVE LA-EFFECT-DATE-A2      TO AM-EXPIRATION-DT

           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              GO TO 6500-ACCT-NOT-FOUND
           END-IF


05950      MOVE AM-CONTROL-PRIMARY (1:20)
                                       TO W-ACCT-SAVE-KEY.
05951      MOVE 'Y'                    TO W-ACCT-BROWSE-STARTED-SW.
05952
05953  6500-READNEXT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              GO TO 6500-ACCT-NOT-FOUND
           END-IF

05961      IF AM-CONTROL-PRIMARY (1:20) NOT = W-ACCT-SAVE-KEY
031504        IF PRIOR-MATCH-ACCT-PKEY
031504           GO TO 6500-ONLY-STOP
031504        ELSE
                 IF W-FILE-NOT-USED (6)
031504              GO TO 6500-EXIT
                 ELSE
                    GO TO 6500-ACCT-NOT-FOUND
031504           END-IF
031504        END-IF
031504     ELSE
031504         SET PRIOR-MATCH-ACCT-PKEY TO TRUE
031504         MOVE AM-CARRIER           TO WS-SAV-AM-CARRIER
031504         MOVE AM-ACCOUNT           TO WS-SAV-AM-ACCOUNT
031504         MOVE AM-DEFN-1            TO WS-SAV-AM-DEFN-1   
031504         MOVE AM-REMIT-TO          TO WS-SAV-AM-REMIT-TO 
031504         MOVE AM-NAME              TO WS-SAV-AM-NAME
031504         MOVE AM-PERSON            TO WS-SAV-AM-PERSON 
031504         MOVE AM-ADDRS             TO WS-SAV-AM-ADDRS
051810         MOVE SPACES               TO WS-SAV-AM-CITY
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO WS-SAV-AM-CITY
051810         END-STRING
031504         MOVE AM-ZIP               TO WS-SAV-AM-ZIP 
031504         MOVE AM-TEL-NO            TO WS-SAV-AM-TEL-NO
031504         MOVE AM-CONTROL-NAME      TO WS-SAV-AM-CONTROL-NAME
072308         MOVE AM-CSR-CODE          TO WS-SAV-AM-CSR-CODE
031512         MOVE AM-ACCOUNT           TO WS-SAV-AM-ERACCT-ACCOUNT
031504         GO TO 6500-READNEXT
031504     END-IF.

05976
05977  6500-ONLY-STOP.
05978
05988
05989      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
031504     MOVE WS-SAV-AM-NAME         TO W-LABEL-LINES (1).
031504     MOVE WS-SAV-AM-PERSON       TO W-LABEL-LINES (2).
031504     MOVE WS-SAV-AM-ADDRS        TO W-LABEL-LINES (3).
031504     MOVE WS-SAV-AM-CITY         TO W-LABEL-LINES (4).
05994 
031504     MOVE WS-SAV-AM-ZIP          TO W-WORK-ZIP.
05996      MOVE SPACES                 TO W-LABEL-ZIP (5).
05997
031504     IF  SAVE-AM-CANADIAN-POST-CODE
031504         MOVE WS-SAV-AM-CANADIAN-POSTAL-CODE
06000                                  TO W-CANADIAN-POSTAL-CODES
06001          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (5)
06002          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (5)
06003          MOVE SPACES             TO W-LAB-CAN-DASH (5)
06004                                     W-LAB-CAN-FILLER (5)
06005      ELSE
06006          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (5)
06007
06008          IF  W-WORK-ZIP4 GREATER THAN '0000'
06009              MOVE '-'            TO W-LABEL-DASH (5)
06010              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (5)
06011          ELSE
06012              MOVE SPACES         TO W-LABEL-DASH (5)
06013                                     W-LABEL-2ND-ZIP (5).
06014
06015      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06016
06038      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (40)
06039      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (41)
06040      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (42)
06041      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (43)
06042      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (44)
06043      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (45).
06044
031504     IF  WS-SAV-AM-TEL-NO NOT NUMERIC
06046              OR
031504         WS-SAV-AM-TEL-NO EQUAL ZEROS
06048          MOVE SPACES             TO W-VG-TEXT (46)
06049      ELSE
06050          MOVE ZEROS                  TO W-PHONE-IN
031504         MOVE WS-SAV-AM-AREA-CODE    TO W-PO-AREA
031504         MOVE WS-SAV-AM-TEL-PRE      TO W-PO-PFX
031504         MOVE WS-SAV-AM-TEL-NBR      TO W-PO-SFX
06054          MOVE W-PHONE-OUT            TO W-VG-TEXT (46).
06055
031504     IF WS-SAV-AM-CONTROL-NAME (1:1) NOT = LOW-VALUES
031504        MOVE WS-SAV-AM-CONTROL-NAME  TO W-VG-TEXT (47)
PEMMOD     ELSE
PEMMOD        MOVE SPACES                   TO W-VG-TEXT (47)
PEMMOD     END-IF.
PEMMOD
031504     MOVE WS-SAV-AM-ACCOUNT          TO W-VG-TEXT (72)
072308     MOVE WS-SAV-AM-CSR-CODE     TO W-VG-TEXT (48)
031512     MOVE WS-SAV-AM-ERACCT-ACCOUNT
031512                                 TO W-VG-TEXT (49)
           GO TO 6500-EXIT
           
           .
       6500-ACCT-NOT-FOUND.

           DISPLAY ' ERROR ON ERACCT - READ - 6500 '
              ERACCT-FILE-STATUS

           .
06084  6500-EXIT.
06085      EXIT.
06086                                  EJECT
06087  6600-GET-COMPENSATION-DATA.
06088
06096      IF W-FILE-NOT-USED (10)
06097         GO TO 6600-EXIT
           END-IF
06098
           MOVE LOW-VALUES             TO CO-CONTROL-PRIMARY
           MOVE LA-COMPANY-CD          TO CO-COMPANY-CD
           MOVE LA-CARRIER-A2          TO CO-CARRIER
           MOVE LA-GROUPING-A2         TO CO-GROUPING

06103      IF  PI-ZERO-CARRIER
06104              OR
06105          PI-ZERO-CAR-GROUP
06106          MOVE ZEROS              TO CO-CARRIER.
06107
06108      IF  PI-ZERO-GROUPING
06109              OR
06110          PI-ZERO-CAR-GROUP
06111          MOVE ZEROS              TO CO-GROUPING.
06112
06113      IF  W-FILE-NOT-USED (6)
06114          PERFORM 6500-READ-ACCT-ONLY THRU 6500-READNEXT.
06120
06122      MOVE  'A'                   TO CO-TYPE
06125
031504     IF  WS-SAV-AM-REMIT-TO > ZEROS
031504         MOVE WS-SAV-AM-AGT (WS-SAV-AM-REMIT-TO)
06128                                  TO CO-RESP-NO
06129      ELSE
06130          MOVE LA-ACCOUNT-A2      TO CO-ACCOUNT.
06131
06132      PERFORM VARYING W-NDX FROM +1 BY +1 UNTIL
06136         (W-NDX > +10)
06137         OR (WS-SAV-AM-COM-TYP (W-NDX) = 'C' OR 'D')
           END-PERFORM
06139
031504     IF WS-SAV-AM-COM-TYP (W-NDX) = 'C' OR 'D'
031504        MOVE WS-SAV-AM-AGT (W-NDX)
                                       TO CO-ACCOUNT
06142      ELSE
06143         MOVE LA-ACCOUNT-A2       TO CO-ACCOUNT
           END-IF
06144
06146      MOVE CO-CONTROL-PRIMARY     TO W-COMP-SAVE-KEY.
06147
06154  6600-START.
06155
           START ERCOMP KEY IS NOT < CO-CONTROL-PRIMARY
           
           IF ERCOMP-FILE-STATUS = '00'
              CONTINUE
           ELSE
              GO TO 6600-COMP-NOT-FOUND
           END-IF

06165
06166      MOVE 'Y'                    TO W-COMP-BROWSE-SW.
06167
06168  6600-READNEXT.
06169
           READ ERCOMP NEXT RECORD

           IF ERCOMP-FILE-STATUS = '00'
              CONTINUE
           ELSE
              GO TO 6600-COMP-NOT-FOUND
           END-IF

06176      IF CO-CONTROL-PRIMARY NOT = W-COMP-SAVE-KEY
06178         GO TO 6600-COMP-NOT-FOUND.
06179
06180      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06181      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1)
06182                                     W-VG-TEXT (140).
06183      MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
06184      MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
06185      MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
051810     MOVE SPACES                 TO W-LABEL-LINES (5).
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
051810     END-STRING
06187
06188      MOVE CO-ZIP                 TO W-WORK-ZIP.
06189
06190      IF  CO-CANADIAN-POST-CODE
06191          MOVE CO-CANADIAN-POSTAL-CODE
06192                                  TO W-CANADIAN-POSTAL-CODES
06193          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06194          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06195          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06196                                     W-LAB-CAN-FILLER (6)
06197      ELSE
06198          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06199
06200          IF  W-WORK-ZIP4 GREATER THAN '0000'
06201              MOVE '-'            TO W-LABEL-DASH (6)
06202              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06203          ELSE
06204              MOVE SPACES         TO W-LABEL-DASH (6)
06205                                     W-LABEL-2ND-ZIP (6).
06206
06207      IF  CO-TELEPHONE NOT NUMERIC
06208              OR
06209          CO-TELEPHONE = ZEROS
06210          MOVE SPACES             TO W-VG-TEXT (147)
06211      ELSE
06212          MOVE ZEROS              TO W-PHONE-IN
06213          MOVE CO-AREA-CODE       TO W-PO-AREA
06214          MOVE CO-PREFIX          TO W-PO-PFX
06215          MOVE CO-PHONE           TO W-PO-SFX
06216          MOVE W-PHONE-OUT        TO W-VG-TEXT (147).
06217
100705     IF (CO-FAXNO NOT NUMERIC)
100705        OR (CO-FAXNO = ZEROS)
100705        MOVE SPACES              TO W-VG-TEXT (182)
100705     ELSE
100705        MOVE ZEROS               TO W-PHONE-IN
100705        MOVE CO-FAX-AREA-CODE    TO W-PO-AREA
100705        MOVE CO-FAX-PREFIX       TO W-PO-PFX
100705        MOVE CO-FAX-PHONE        TO W-PO-SFX
100705        MOVE W-PHONE-OUT         TO W-VG-TEXT (182)
100705     END-IF

100705     IF CO-GA-STATUS-CODE NOT = 'A' AND 'I' AND 'P'
100705        MOVE SPACES              TO W-VG-TEXT (183)
100705     ELSE
100705        EVALUATE CO-GA-STATUS-CODE
100705           WHEN 'A'
100705              MOVE 'ACTIVE'      TO W-VG-TEXT (183)
100705           WHEN 'I'
100705              MOVE 'INACTIVE'    TO W-VG-TEXT (183)
100705           WHEN 'P'
100705              MOVE 'PENDING'     TO W-VG-TEXT (183)
100705        END-EVALUATE
100705     END-IF

100705     IF CO-BILL-SW NOT = 'B' AND 'R' AND 'T' AND 'S'
100705        MOVE SPACES              TO W-VG-TEXT (181)
100705     ELSE
100705        EVALUATE CO-BILL-SW
100705           WHEN 'B'
100705              MOVE 'BILLED'      TO W-VG-TEXT (181)
100705           WHEN 'R'
100705              MOVE 'REMIT'       TO W-VG-TEXT (181)
100705           WHEN 'T'
100705              MOVE 'T'           TO W-VG-TEXT (181)
100705           WHEN 'S'
100705              MOVE 'S'           TO W-VG-TEXT (181)
100705        END-EVALUATE
100705     END-IF

06218      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06219
06241      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (141)
06242      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (142)
06243      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (143)
06244      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (144)
06245      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (145)
06246      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (146)
06247
06248      PERFORM 6630-GET-CSR-NAME THRU 6630-EXIT.
06249
06250      MOVE CO-RESP-NO             TO W-VG-TEXT (180).
06251      MOVE CO-END-BAL             TO W-EDIT-7-2-NEGATIVE.
06252      MOVE W-EDIT-7-2-NEGATIVE    TO W-VG-TEXT (150).
06253
06254      MOVE CO-LAST-STMT-DT        TO DC-GREG-DATE-1-YMD.
06255      MOVE '3'                    TO DC-OPTION-CODE.
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
06257
06258      IF  NO-CONVERSION-ERROR
06259          MOVE DC-GREG-DATE-1-ALPHA
06260                                  TO W-VG-TEXT (149).
06261
06262      PERFORM 6620-GET-G-DATA THRU 6620-EXIT.
06263
06265      GO TO 6600-EXIT.
06266
06275  6600-COMP-NOT-FOUND.

           DISPLAY ' ERROR ON ERCOMP - START - 6600 '
              ERCOMP-FILE-STATUS

           .
06294  6600-EXIT.
06295      EXIT.
06296
06302  6620-GET-G-DATA.
06303
06304      MOVE W-COMP-SAVE-KEY        TO CO-CONTROL-PRIMARY
06305
06306      IF  CO-RPTCD2 GREATER THAN SPACES
06307          MOVE SPACES             TO CO-RESP-NO
06308          MOVE CO-RPTCD2          TO W-COMP-WORK-AREA
06309          MOVE +10                TO W-CWA-NDX
06310          PERFORM 6622-FIND-LAST-NON-SPACE
06311          SET W-COMP-NDX          TO +10
06312          PERFORM 6625-FILL-IN-RESP-PERSON
06313          INSPECT W-COMP-RESP-PERSON CONVERTING SPACES TO ZEROS
               MOVE W-COMP-WORK-AREA   TO CO-RESP-NO
           END-IF
06314
06315      MOVE LOW-VALUES             TO CO-ACCOUNT
06316      MOVE 'G'                    TO CO-TYPE
06317      MOVE CO-CONTROL-PRIMARY     TO W-COMP-SAVE-KEY.

           READ ERCOMP
           
           IF ERCOMP-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERCOMP - READ - 6620 '
                 ERCOMP-FILE-STATUS
              DISPLAY ' RESP ' CO-RESP-NO
              GO TO 6620-EXIT
           END-IF

06330      IF  CO-CONTROL-PRIMARY NOT = W-COMP-SAVE-KEY
06331          GO TO 6620-EXIT.
06332
06333      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06334      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1).
06335      MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
06336      MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
06337      MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
051810     MOVE SPACES                 TO W-LABEL-LINES (5).
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
051810     END-STRING
06339
06340      MOVE CO-ZIP                 TO W-WORK-ZIP.
06341
06342      IF  CO-CANADIAN-POST-CODE
06343          MOVE CO-CANADIAN-POSTAL-CODE
06344                                  TO W-CANADIAN-POSTAL-CODES
06345          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06346          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06347          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06348                                     W-LAB-CAN-FILLER (6)
06349      ELSE
06350          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06351          IF  W-WORK-ZIP4 GREATER THAN '0000'
06352              MOVE '-'            TO W-LABEL-DASH (6)
06353              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06354          ELSE
06355              MOVE SPACES         TO W-LABEL-DASH (6)
06356                                     W-LABEL-2ND-ZIP (6).
06357
06358      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06359
06360      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (174).
06361      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (175).
06362      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (176).
06363      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (177).
06364      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (178).
06365      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (179).
06366
06367  6620-EXIT.
06368      EXIT.
06369                                  EJECT
06370  6622-FIND-LAST-NON-SPACE.
06371
06372      IF  W-CWA-CHAR (W-CWA-NDX) EQUAL SPACES
06373          SUBTRACT +1 FROM W-CWA-NDX
06374          GO TO 6622-FIND-LAST-NON-SPACE.
06375
06376  6622-EXIT.
06377      EXIT.
06378                                  EJECT
06379  6625-FILL-IN-RESP-PERSON.
06380
06381      MOVE W-CWA-CHAR (W-CWA-NDX) TO W-COMP-RP-CHAR (W-COMP-NDX)
06382      SET W-COMP-NDX DOWN BY +1.
06383      SUBTRACT +1 FROM W-CWA-NDX.
06384
06385      IF  W-CWA-NDX GREATER THAN ZEROS
06386          GO TO 6625-FILL-IN-RESP-PERSON.
06387
06388  6625-EXIT.
06389      EXIT.
06390                                  EJECT
06391  6630-GET-CSR-NAME.

           DISPLAY ' MADE IT TO 6630 '
           MOVE SPACES                 TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           MOVE CO-CSR-CODE            TO CF-PROCESSOR
           MOVE +0                     TO CF-SEQUENCE-NO

           READ ELCNTL
           
           IF ELCNTL-FILE-STATUS = '00'
              MOVE CF-PROCESSOR-NAME   TO W-VG-TEXT (148)
           ELSE
              DISPLAY ' CSR CODE ' CO-CSR-CODE
              DISPLAY ' ERROR ON ELCNTL - READ - 6630 '
                 ELCNTL-FILE-STATUS
           END-IF

           .
06421  6630-EXIT.
06422      EXIT.
06423                                  EJECT
06424  6700-GET-MAIL-DATA.
06425
           DISPLAY ' MADE IT TO 6700 '

06433      IF  W-FILE-NOT-USED (5)
06434          GO TO 6700-EXIT.
06435
           MOVE CM-CONTROL-PRIMARY     TO MA-CONTROL-PRIMARY
           
           READ ERMAIL
           
           IF ERMAIL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERMAIL - READ - 6700 '
                 ERMAIL-FILE-STATUS
           END-IF
06452
06453      MOVE MA-INSURED-LAST-NAME   TO W-NAME-LAST.
06454      MOVE MA-INSURED-FIRST-NAME  TO W-NAME-FIRST.
06455      MOVE MA-INSURED-MIDDLE-INIT TO W-NAME-MIDDLE.
06456      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
06457
06458      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06459      MOVE WS-NAME-WORK           TO W-LABEL-LINES (1).
06460      MOVE W-LABEL-JOINT-NAME     TO W-LABEL-LINES (2).
06461      MOVE MA-ADDRESS-LINE-1      TO W-LABEL-LINES (3).
06462      MOVE MA-ADDRESS-LINE-2      TO W-LABEL-LINES (4).
051810     MOVE SPACES                 TO W-LABEL-LINES (5).
051810     STRING MA-CITY ' ' MA-ADDR-STATE
051810        DELIMITED BY '  ' INTO W-LABEL-LINES (5)
051810     END-STRING
06464
06465      MOVE MA-ZIP                 TO W-WORK-ZIP.
06466      MOVE SPACES                 TO W-LABEL-ZIP (6).
06467
06468      IF  MA-CANADIAN-POST-CODE
06469          MOVE MA-CANADIAN-POSTAL-CODE
06470                                  TO W-CANADIAN-POSTAL-CODES
06471          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06472          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06473          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06474                                     W-LAB-CAN-FILLER (6)
06475      ELSE
06476          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06477          IF  W-WORK-ZIP4 GREATER THAN '0000'
06478              MOVE '-'            TO W-LABEL-DASH (6)
06479              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06480          ELSE
06481              MOVE SPACES         TO W-LABEL-DASH (6)
06482                                     W-LABEL-2ND-ZIP (6).
06483
06484      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06485
06507      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (31)
06508      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (32)
06509      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (33)
06510      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (34)
06511      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (35)
06512      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (36)
06513
031512     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
031512     MOVE MA-CRED-BENE-NAME      TO W-LABEL-LINES (1)
031512     MOVE MA-CRED-BENE-ADDR      TO W-LABEL-LINES (2)
031512     MOVE MA-CRED-BENE-ADDR2     TO W-LABEL-LINES (3)
031512
031512     STRING MA-CRED-BENE-CITY ' ' MA-CRED-BENE-STATE
031512        DELIMITED BY '  ' INTO W-LABEL-LINES (4)
031512     END-STRING
031512
031512     MOVE MA-CRED-BENE-ZIP       TO W-WORK-ZIP
031512     MOVE SPACES                 TO W-LABEL-ZIP (5)
031512
031512     IF  MA-CB-CANADIAN-POST-CODE
031512         MOVE MA-CB-CANADIAN-POSTAL-CODE
031512                                 TO W-CANADIAN-POSTAL-CODES
031512         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (5)
031512         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (5)
031512         MOVE SPACES             TO W-LAB-CAN-DASH (5)
031512                                    W-LAB-CAN-FILLER (5)
031512     ELSE
031512         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (5)
031512         IF  W-WORK-ZIP4 GREATER THAN '0000'
031512             MOVE '-'            TO W-LABEL-DASH (5)
031512             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (5)
031512         ELSE
031512             MOVE SPACES         TO W-LABEL-DASH (5)
031512                                    W-LABEL-2ND-ZIP (5)
031512         END-IF
031512     END-IF
031512
031512     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT
031512
031512     MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (37)
031512     MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (50)
031512     MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (51)
031512     MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (52)
031512     MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (53)
           .
06526  6700-EXIT.
06527      EXIT.
06528                                  EJECT
06529  6750-GET-LIFE-BENEFIT-DATA.
06530
           DISPLAY ' MADE IT TO 6750 '

06531      IF  W-FILE-NOT-USED (2)
06532          GO TO 6750-EXIT.
06533
06534      MOVE SPACES                 TO W-CNTL-GEN2.
06535
06536      IF LA-DATA-SOURCE = '2'
06537         IF W-FILE-NOT-USED (8)
06538            PERFORM 6450-READ-CERT-ONLY
06539            IF W-CERT-FOUND
06540               MOVE CM-LF-BENEFIT-CD
06541                                  TO W-BEN-HOLD
06542                                     W-CNTL-GEN2
06543            END-IF
06545         ELSE
06546            MOVE CM-LF-BENEFIT-CD TO W-BEN-HOLD
06548                                     W-CNTL-GEN2
              END-IF
06549      ELSE
06550         IF LA-DATA-SOURCE = '4'
06551            IF W-FILE-NOT-USED (9)
06552               PERFORM 6400-READ-PNDB-ONLY
06553               IF W-PNDB-FOUND
06554                  IF PB-RECORD-TYPE = '1'
06555                     MOVE PB-I-LF-BENEFIT-CD
06556                                  TO W-BEN-HOLD
06557                                     W-CNTL-GEN2
06558                  ELSE
06559                     MOVE PB-CI-LF-BENEFIT-CD
06560                                  TO W-BEN-HOLD
06561                                     W-CNTL-GEN2
                       END-IF
06562               END-IF
06564            ELSE
06565               IF W-FILE-USED (8)
06566                  MOVE CM-LF-BENEFIT-CD
06567                                  TO W-BEN-HOLD
06568                                     W-CNTL-GEN2
06569               ELSE
06570                  IF W-FILE-USED (9)
06571                     IF PB-RECORD-TYPE = '1'
06572                        MOVE PB-I-LF-BENEFIT-CD
06573                                  TO W-BEN-HOLD
06574                                     W-CNTL-GEN2
06575                     ELSE
06576                        MOVE PB-CI-LF-BENEFIT-CD
06577                                  TO W-BEN-HOLD
06578                                     W-CNTL-GEN2
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
06579
06585
06586      MOVE SPACES                 TO W-CNTL-GEN1.
06587      MOVE '4'                    TO W-CNTL-RECORD-TYPE.
06588      MOVE ZEROS                  TO W-CNTL-SEQ-NO.
           MOVE W-CNTL-KEY             TO CF-CONTROL-PRIMARY

           START ELCNTL KEY IS NOT < CF-CONTROL-PRIMARY
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 6750-EXIT
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ELCNTL - START - 6750 '
                    ELCNTL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           READ ELCNTL NEXT RECORD
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 6750-EXIT
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ELCNTL - READ - 6750 '
                    ELCNTL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

06602      MOVE 1                      TO W-NDX.
06603
06604  6750-LOOP.
06605
06606      IF  W-NDX = 9
06607          GO TO 6750-EXIT.
06608
06609      IF  CF-BENEFIT-CODE (W-NDX) LESS W-BEN-HOLD
06610          ADD 1                   TO W-NDX
06611          GO TO 6750-LOOP.
06612
06613      IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-NDX)
06614          MOVE CF-BENEFIT-DESCRIP (W-NDX)
06615                                  TO W-VG-TEXT (11).
06616
06617  6750-EXIT.
06618      EXIT.
06619                                  EJECT
06620  6800-GET-A-H-BENEFIT-DATA.
06621
           DISPLAY ' MADE IT TO 6800 '

06622      IF  W-FILE-NOT-USED (3)
06623          GO TO 6800-EXIT.
06624
06625      MOVE SPACES                 TO W-CNTL-GEN2.
06626
06627      IF LA-DATA-SOURCE = '2'
06628         IF W-FILE-NOT-USED (8)
06629            PERFORM 6450-READ-CERT-ONLY
06630            IF W-CERT-FOUND
06631               MOVE CM-AH-BENEFIT-CD
06632                                  TO W-BEN-HOLD
06633                                     W-CNTL-GEN2
06634            END-IF
06636         ELSE
06637            MOVE CM-AH-BENEFIT-CD TO W-BEN-HOLD
06639                                     W-CNTL-GEN2
              END-IF
06640      ELSE
06641         IF LA-DATA-SOURCE = '4'
06642            IF W-FILE-NOT-USED (9)
06643               PERFORM 6400-READ-PNDB-ONLY
06644               IF W-PNDB-FOUND
06645                  IF PB-RECORD-TYPE = '1'
06646                     MOVE PB-I-AH-BENEFIT-CD
06647                                  TO W-BEN-HOLD
06648                                     W-CNTL-GEN2
06649                  ELSE
06650                     MOVE PB-CI-AH-BENEFIT-CD
06651                                  TO W-BEN-HOLD
06652                                     W-CNTL-GEN2
                       END-IF
06653               END-IF
06655            ELSE
06656               IF W-FILE-USED (8)
06657                  MOVE CM-AH-BENEFIT-CD
06658                                  TO W-BEN-HOLD
06659                                     W-CNTL-GEN2
06660               ELSE
06661                  IF W-FILE-USED (9)
06662                     IF PB-RECORD-TYPE = '1'
06663                        MOVE PB-I-AH-BENEFIT-CD
06664                                  TO W-BEN-HOLD
06665                                     W-CNTL-GEN2
06666                     ELSE
06667                        MOVE PB-CI-AH-BENEFIT-CD
06668                                  TO W-BEN-HOLD
06669                                     W-CNTL-GEN2
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
06670
06676
06677      MOVE SPACES                 TO W-CNTL-GEN1.
06678      MOVE '5'                    TO W-CNTL-RECORD-TYPE.
06679      MOVE ZEROS                  TO W-CNTL-SEQ-NO.
           MOVE W-CNTL-KEY             TO CF-CONTROL-PRIMARY

           START ELCNTL KEY IS NOT < CF-CONTROL-PRIMARY
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 6800-EXIT
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ELCNTL - START - 6800 '
                    ELCNTL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           READ ELCNTL NEXT RECORD
           IF ELCNTL-FILE-STATUS = '10' OR '23'
              GO TO 6800-EXIT
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ELCNTL - READ - 6800 '
                    ELCNTL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

06693      MOVE 1                      TO W-NDX.
06694
06695  6800-LOOP-AH.
06696
06697      IF  W-NDX = 9
06698          GO TO 6800-EXIT.
06699
06700      IF  CF-BENEFIT-CODE (W-NDX) LESS W-BEN-HOLD
06701          ADD 1                   TO W-NDX
06702          GO TO 6800-LOOP-AH.
06703
06704      IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-NDX)
06705          MOVE CF-BENEFIT-DESCRIP (W-NDX)
06706                                  TO W-VG-TEXT (15)
06707          MOVE CF-BENEFIT-ALPHA (W-NDX)
06708                                  TO W-BENEFIT-WORK
06709          MOVE W-ELIM-DAYS        TO W-VG-TEXT (16).
06710
06711  6800-EXIT.
06712      EXIT.
06713                                  EJECT
06714  6850-GET-CHECK-DATA.
06715
           DISPLAY ' MADE IT TO 6850 '

06723      IF  W-FILE-NOT-USED (12)
06724          GO TO 6850-EXIT.
06725
06731
           MOVE CM-CONTROL-PRIMARY     TO CH-CONTROL-PRIMARY
           MOVE +0                     TO CH-SEQUENCE-NO

           READ ERCHEK
           IF ERCHEK-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERCHEK - READ - 6850 '
                 ERCHEK-FILE-STATUS
              GO TO 6850-EXIT
           END-IF
           
06743      MOVE CH-CHECK-NO            TO W-VG-TEXT (158).
06744      MOVE CH-AMOUNT-PAID         TO W-EDIT-7-2.
06745      MOVE W-EDIT-7-2             TO W-VG-TEXT (157).
06746      MOVE CH-CHECK-QUE-CONTROL   TO W-DISPLAY-8
06747                                     PI-689-CONTROL.
06748      MOVE W-DISPLAY-8            TO W-VG-TEXT (165).
06749      MOVE CH-REASON-FOR-CHECK    TO W-VG-TEXT (166).
06750
06751      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06752      MOVE CH-PAYEE-NAME-1        TO W-LABEL-LINES (1).
06753      MOVE CH-PAYEE-NAME-2        TO W-LABEL-LINES (2).
06754      MOVE CH-PAYEE-ADDRESS-1     TO W-LABEL-LINES (3).
06755      MOVE CH-PAYEE-ADDRESS-2     TO W-LABEL-LINES (4).
06756      MOVE CH-PAYEE-CITY-ST       TO W-LABEL-LINES (5).
06757
06758      MOVE CH-PAYEE-ZIP           TO W-WORK-ZIP.
06759      MOVE SPACES                 TO W-LABEL-ZIP (6).
06760
06761      IF  CH-CANADIAN-POST-CODE
06762          MOVE CH-CANADIAN-POSTAL-CODE
06763                                  TO W-CANADIAN-POSTAL-CODES
06764          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06765          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06766          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06767                                     W-LAB-CAN-FILLER (6)
06768      ELSE
06769          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06770          IF  W-WORK-ZIP4 GREATER THAN '0000'
06771              MOVE '-'            TO W-LABEL-DASH (6)
06772              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06773          ELSE
06774              MOVE SPACES         TO W-LABEL-DASH (6)
06775                                     W-LABEL-2ND-ZIP (6).
06776
06777      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06778
06800      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (159)
06801      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (160)
06802      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (161)
06803      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (162)
06804      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (163)
06805      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (164)

           .
06819  6850-EXIT.
06820      EXIT.
06821                                  EJECT
06822  6900-GET-PYAJ-DATA.
06823
           DISPLAY ' MADE IT TO 6900 '

06824      IF  W-FILE-NOT-USED (13)
06825          GO TO 6900-EXIT.
06826
           MOVE LA-COMPANY-CD          TO PY-COMPANY-CD
           MOVE LA-CARRIER-A2          TO PY-CARRIER
           MOVE LA-GROUPING-A2         TO PY-GROUPING
           MOVE LA-RESP-PERSON-A2      TO PY-FIN-RESP
           MOVE LA-ACCOUNT-A2          TO PY-ACCOUNT
           MOVE +0                     TO PY-FILE-SEQ-NO
           MOVE LA-TYPE-A2             TO PY-RECORD-TYPE

           START ERPYAJ KEY IS NOT < PY-CONTROL-PRIMARY
           IF ERPYAJ-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERPYAJ - START - 6900 '
                 ERPYAJ-FILE-STATUS
              GO TO 6900-EXIT
           END-IF

           .
06838  6900-READ-NEXT.
06839
           READ ERPYAJ NEXT RECORD
           IF ERPYAJ-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERPYAJ - READ NEXT - 6900 '
                 ERPYAJ-FILE-STATUS
              GO TO 6900-EXIT
           END-IF

06847      IF  PY-COMPANY-CD NOT = LA-COMPANY-CD
06848              OR
06849          PY-CARRIER NOT = LA-CARRIER-A2
06850              OR
06851          PY-GROUPING NOT = LA-GROUPING-A2
06852              OR
06853          PY-FIN-RESP NOT = LA-RESP-PERSON-A2
06854              OR
06855          PY-ACCOUNT NOT = LA-ACCOUNT-A2
06856          GO TO 6900-PYAJ-NOT-FOUND.
06857
06858      IF  PI-689-SEQ-NO > ZEROS
06859              AND
06860          PY-FILE-SEQ-NO NOT = +0
06861          GO TO 6900-PYAJ-NOT-FOUND.
06862
06863      IF  PY-RECORD-TYPE NOT = W-PYAJ-RECORD-TYPE
06864          MOVE PY-CONTROL-PRIMARY TO W-PYAJ-KEY
06865          ADD +1                  TO W-PYAJ-FILE-SEQ-NO
06866          GO TO 6900-READ-NEXT.
06867
06868      MOVE PY-CHECK-NUMBER        TO W-DISPLAY-7.
06869      MOVE W-DISPLAY-7            TO W-VG-TEXT (171).
06870
06871      MOVE PY-ENTRY-AMT           TO W-EDIT-7-2.
06872      MOVE W-EDIT-7-2             TO W-VG-TEXT (170).
06873
06874      MOVE PY-CHECK-QUE-CONTROL   TO W-DISPLAY-8
06875                                     PI-689-CONTROL.
06876      MOVE W-DISPLAY-8            TO W-VG-TEXT (172).
06877      MOVE PY-ENTRY-COMMENT       TO W-VG-TEXT (173).
06878
06879      GO TO 6900-EXIT.
06880
06881  6900-PYAJ-NOT-FOUND.
06882
           DISPLAY ' ERROR ON ERPYAJ - READ - 6900 '
              ERPYAJ-FILE-STATUS

           .
06889  6900-EXIT.
06890      EXIT.

06892  6950-MOVE-SYSTEM-DATA.
06893
           DISPLAY ' MADE IT TO 6950 '

06894      IF  W-FILE-NOT-USED (7)
06895          GO TO 6950-EXIT.
06896
           MOVE WS-CURRENT-BIN-DATE    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
05868      PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
06901      MOVE DC-GREG-DATE-1-EDIT    TO W-VG-TEXT (60).
06902      MOVE DC-GREG-DATE-1-ALPHA   TO W-VG-TEXT (61).

           IF WS-FORM-TO-CREATE = SPACES
              MOVE WS-FORM-TO-RESEND   TO W-VG-TEXT (62)
           ELSE
              MOVE WS-FORM-TO-CREATE   TO W-VG-TEXT (62)
           END-IF

PEMTST*    MOVE LA-FORM-A3             TO W-VG-TEXT (62)

           .
06919  6950-EXIT.
06920      EXIT.
06921                                  EJECT
06946  7100-FORMAT-LAST-NAME-1ST.
06947 *****************************************************************
06948 *             M O V E   N A M E   R O U T I N E                 *
06949 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
06950 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
06951 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
06952 *                  FIELD                   VALUE                *
06953 *           W-NAME-LAST    (CL15)      SMITH                    *
06954 *           W-NAME-FIRST   (CL15)      JOHN                     *
06955 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
06956 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
06957 *     CONTAIN                                                   *
06958 *                SMITH, JOHN ALLEN                              *
06959 *     OR                                                        *
06960 *                SMITH, JOHN A.                                 *
06961 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
06962 *     COPYBOOK, ELCNWA.                                         *
06963 *****************************************************************.
06964
06965      MOVE SPACES                 TO WS-NAME-WORK-AREA.
06966      MOVE ZERO                   TO WS-NAME-SW.
06967      SET NWA-INDEX               TO +1.
06968
06969      IF  W-NAME-LAST EQUAL SPACES
06970              AND
06971          W-NAME-MIDDLE EQUAL SPACES
06972          MOVE +1                 TO WS-NAME-SW.
06973
06974      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
06975      PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
06976
06977      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
06978      PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
06979
06980      SET NWA-INDEX UP BY +1.
06981
06982      IF  W-NAME-MIDDLE NOT EQUAL SPACES
06983          IF  W-NAME-MIDDLE-2 EQUAL SPACES
06984              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
06985              SET NWA-INDEX UP BY +1
06986              MOVE '.'            TO WS-NW (NWA-INDEX)
06987              SET NWA-INDEX UP BY +2
06988          ELSE
06989              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
06990              PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
06991
06992  7100-EXIT.
06993      EXIT.
06994                                  EJECT
06995  7110-MOVE-NAME.
06996
06997      IF  WS-NAME-SW GREATER THAN +1
06998          GO TO 7110-EXIT.
06999
07000      IF  WS-NAME-WORK2 = SPACES
07001          GO TO 7110-EXIT.
07002
07003      SET NWA-INDEX2            TO +1.
07004      SET NWA-INDEX3            TO +2.
07005
07006  7110-MOVE-NAME-CYCLE.
07007
07008      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
07009
07010      IF  NWA-INDEX LESS THAN +30
07011          SET NWA-INDEX UP BY +1
07012      ELSE
07013          ADD +2                TO WS-NAME-SW
07014          GO TO 7110-EXIT.
07015
07016      IF  NWA-INDEX2 LESS THAN +20
07017          SET NWA-INDEX3 UP BY +1
07018          SET NWA-INDEX2 UP BY +1.
07019
07020      IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
07021              AND
07022          WS-NW2 (NWA-INDEX3) EQUAL SPACES
07023          IF  WS-NAME-SW EQUAL ZERO
07024              MOVE ','            TO WS-NW (NWA-INDEX)
07025              SET NWA-INDEX UP BY +2
07026              MOVE +1             TO WS-NAME-SW
07027              GO TO 7110-EXIT
07028          ELSE
07029              GO TO 7110-EXIT.
07030
07031      GO TO 7110-MOVE-NAME-CYCLE.
07032
07033  7110-EXIT.
07034      EXIT.
07035                                  EJECT
07036  7200-FORMAT-NAME-STRAIGHT.
07037 *****************************************************************
07038 *           M O V E   N A M E   R O U T I N E                   *
07039 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
07040 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
07041 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
07042 *                  FIELD                   VALUE                *
07043 *           W-NAME-LAST    (CL15)      SMITH                    *
07044 *           W-NAME-FIRST   (CL15)      JOHN                     *
07045 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
07046 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
07047 *     CONTAIN                                                   *
07048 *              JOHN A. SMITH                                    *
07049 *     OR                                                        *
07050 *              JOHN ALLEN SMITH                                 *
07051 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
07052 *     COPYBOOK, ELCNWA.                                         *
07053 *****************************************************************
07054
07055      MOVE SPACES                 TO WS-NAME-WORK-AREA.
07056      MOVE ZERO                   TO WS-NAME-SW.
07057      SET NWA-INDEX               TO +1.
07058
07059      IF  W-NAME-FIRST EQUAL SPACES
07060              AND
07061          W-NAME-MIDDLE EQUAL SPACES
07062          MOVE W-NAME-LAST        TO WS-NAME-WORK
07063          GO TO 7200-EXIT.
07064
07065      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
07066      PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
07067
07068      IF  W-NAME-MIDDLE NOT EQUAL SPACES
07069          SET NWA-INDEX UP BY +1
07070
07071          IF  W-NAME-MIDDLE-2 EQUAL SPACES
07072              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
07073              SET NWA-INDEX UP BY +1
07074              MOVE '.'            TO WS-NW (NWA-INDEX)
07075              SET NWA-INDEX UP BY +1
07076
07077          ELSE
07078              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
07079              PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
07080
07081      SET NWA-INDEX UP BY +1
07082      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
07083      PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
07084
07085  7200-EXIT.
07086      EXIT.
07087                                  EJECT
07088  7290-MOVE-NAME.
07089
07090      IF  WS-NAME-SW GREATER THAN +1
07091          GO TO 7290-EXIT.
07092
07093      IF  WS-NAME-WORK2 EQUAL SPACES
07094          GO TO 7290-EXIT.
07095
07096      SET NWA-INDEX2            TO +1.
07097      SET NWA-INDEX3            TO +2.
07098
07099  7290-MOVE-NAME-CYCLE.
07100
07101      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
07102
07103      IF  NWA-INDEX LESS THAN +30
07104          SET NWA-INDEX UP BY +1
07105      ELSE
07106          ADD +2                TO WS-NAME-SW
07107          GO TO 7290-EXIT.
07108
07109      IF  NWA-INDEX2 LESS THAN +20
07110          SET NWA-INDEX2 UP BY +1
07111          SET NWA-INDEX3 UP BY +1.
07112
07113      IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
07114              AND
07115          WS-NW2 (NWA-INDEX3) EQUAL SPACES
07116          GO TO 7290-EXIT.
07117
07118      GO TO 7290-MOVE-NAME-CYCLE.
07119
07120  7290-EXIT.
07121      EXIT.
07122                                  EJECT
07123  7300-LABEL-MOVE.
07124
07125      IF  W-LABEL-HOLD-AREA = SPACES
07126          MOVE +0                 TO W-NUMB-LABEL-LINES
07127          GO TO 7300-EXIT.
07128
07129      IF  W-LABEL-LINES (1) = SPACES
07130          MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)
07131          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
07132          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
07133          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07134          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07135          MOVE SPACES             TO W-LABEL-LINES (6)
07136          GO TO 7300-LABEL-MOVE.
07137
07138      IF  W-LABEL-LINES (2) = SPACES
07139              AND
07140          W-LABEL-LINES (3) = SPACES
07141              AND
07142          W-LABEL-LINES (4) = SPACES
07143              AND
07144          W-LABEL-LINES (5) = SPACES
07145              AND
07146          W-LABEL-LINES (6) = SPACES
07147          MOVE 1                  TO W-NDX
07148                                     W-NUMB-LABEL-LINES
07149          GO TO 7300-EXIT.
07150
07151  7300-TRY-2.
07152
07153      IF  W-LABEL-LINES (2) = SPACES
07154          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
07155          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
07156          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07157          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07158          MOVE SPACES             TO W-LABEL-LINES (6)
07159          GO TO 7300-TRY-2.
07160
07161      IF  W-LABEL-LINES (3) = SPACES
07162              AND
07163          W-LABEL-LINES (4) = SPACES
07164              AND
07165          W-LABEL-LINES (5) = SPACES
07166              AND
07167          W-LABEL-LINES (6) = SPACES
07168          MOVE 2                  TO W-NDX
07169          GO TO 7300-MOVE-ZIP.
07170
07171  7300-TRY-3.
07172
07173      IF  W-LABEL-LINES (3) = SPACES
07174          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
07175          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07176          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07177          MOVE SPACES             TO W-LABEL-LINES (6)
07178          GO TO 7300-TRY-3.
07179
07180      IF  W-LABEL-LINES (4) = SPACES
07181              AND
07182          W-LABEL-LINES (5) = SPACES
07183              AND
07184          W-LABEL-LINES (6) = SPACES
07185          MOVE 3                   TO W-NDX
07186          GO TO 7300-MOVE-ZIP.
07187
07188  7300-TRY-4.
07189
07190      IF  W-LABEL-LINES (4) = SPACES
07191          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07192          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07193          MOVE SPACES             TO W-LABEL-LINES (6)
07194          GO TO 7300-TRY-4.
07195
07196      IF  W-LABEL-LINES (5) = SPACES
07197              AND
07198          W-LABEL-LINES (6) = SPACES
07199          MOVE 4                  TO W-NDX
07200          GO TO 7300-MOVE-ZIP.
07201
07202  7300-TRY-5.
07203
07204      IF  W-LABEL-LINES (5) = SPACES
07205          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07206          MOVE SPACES             TO W-LABEL-LINES (6)
07207          GO TO 7300-TRY-5
07208      ELSE
07209          IF  W-LABEL-LINES (6) = SPACES
07210              MOVE 5              TO W-NDX
07211              GO TO 7300-MOVE-ZIP
07212          ELSE
07213              MOVE 6              TO W-NDX.
07214
07215  7300-MOVE-ZIP.
07216
07217      COMPUTE W-NDX2 = W-NDX - 1.
07218
07219      IF  W-LAST-ZIP (W-NDX2) = SPACES
07220 *****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
07221
07222          IF  W-LABEL-1ST-ZIP (W-NDX) NUMERIC
07225                  MOVE W-LABEL-ZIP (W-NDX)
07226                                  TO W-LAST-ZIP (W-NDX2)
07227                  MOVE SPACES     TO W-LABEL-LINES (W-NDX)
07228                  MOVE W-NDX2     TO W-NUMB-LABEL-LINES
07229                  GO TO 7300-EXIT.
07230
07231      MOVE W-NDX                  TO W-NUMB-LABEL-LINES.
07232
07233  7300-EXIT.
07234      EXIT.
07235                                  EJECT
07236  7400-CREATE-LETTER.
07237 ****************************************************************
07238 *    THIS AREA CONTROLS THE PROCESSING OF INDIVIDUAL INPUT     *
07239 *    RECORDS.                                                  *
07240 ****************************************************************
07241
           DISPLAY ' MADE IT TO 7400 '

07242      SET W-SQ-NDX                TO W-START-COLUMN.
07243      MOVE SPACES                 TO W-SQUEEZED-LINE
07244                                     W-WORK-LINE.
07245      MOVE W-START-COLUMN         TO W-LAST-SQUEEZED-SPACE
07246      MOVE +1                     TO W-LAST-WC-SPACE.
07247      MOVE HIGH-VALUES            TO W-LAST-SQ-CHAR.
07248
07249      SET W-TG-NDX                TO +1.
07250
07251      PERFORM 7410-TEXT-LINE-PROCESS THRU 7410-EXIT
07252             VARYING
07253          W-TG-NDX FROM W-TG-NDX BY +1
07254             UNTIL
07255          W-TG-NDX GREATER THAN W-TOTAL-TX-LINES.
07256
07257      IF  W-FORM-SQUEEZE-ON
07258              AND
07259          W-SQUEEZED-LINE GREATER THAN SPACES
07260          PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT.
07261
07262      PERFORM 7640-PAGING THRU 7640-EXIT.
07263
07264  7400-EXIT.
07265      EXIT.
07266                                  EJECT
07267  7410-TEXT-LINE-PROCESS.
07268 ***************************************************************
07269 *    THIS ROUTINE SEARCHES THE TEXT WORK AREA FOR ANY         *
07270 *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
07271 *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
07272 *    DATA THAT WAS GENERATED PREVIOUSLY.  WHILE PERFORMING    *
07273 *    THIS FUNCTION THE ROUTINE MOVES EACH CHARACTER OR ITS    *
07274 *    VARIABLE REPLACEMENT TO A WORK LINE.                     *
07275 ***************************************************************
07276
07277
07278      SET W-WC-NDX                TO W-ZEROS.
07279      MOVE HIGH-VALUES            TO W-LAST-CHAR.
07280
07281      MOVE W-TX-SC (W-TG-NDX)     TO W-LINE-SQUEEZE-IND.
07282
07283      PERFORM 7420-TEXT-CHAR-PROCESS THRU 7420-EXIT
07284              VARYING
07285          W-TX-NDX FROM +1 BY +1
07286              UNTIL
07287          W-TX-NDX GREATER THAN +70.
07288
07289 ****************************************************************
07290 *    IF SPACE 'SQUEEZING' IS INDICATED ON A FORM WIDE BASIS    *
07291 *    EACH LINE OF TEXT IS PROCESSED ACCORDING TO THE INDICATED *
07292 *    LINE SQUEEZE CONTROL.                                     *
07293 ****************************************************************
07294      IF  W-FORM-SQUEEZE-ON
07295
07296          IF  W-WORK-LINE GREATER THAN SPACES
07297                  OR
07298              W-AS-IS
07299              PERFORM 7450-SQUEEZING THRU 7450-EXIT
07300
07301          ELSE
07302              NEXT SENTENCE
07303 ****************************************************************
07304 *    IF NO SPACE 'SQUEEZING' IS INDICATED ON A FORM WIDE BASIS *
07305 *    EACH LINE OF TEXT IS PRINT AS IS EXCEPT FOR VARIABLES     *
07306 ****************************************************************
07307      ELSE
07308          IF  W-WORK-LINE GREATER THAN SPACES
07309              MOVE W-TX-PC (W-TG-NDX)
07310                                  TO W-PRINT-CONTROL
07311              MOVE W-WORK-LINE    TO W-SQUEEZED-LINE
07312              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07313              MOVE SPACES         TO W-WORK-LINE.
07314
07315  7410-EXIT.
07316      EXIT.
07317                                  EJECT
07318  7420-TEXT-CHAR-PROCESS.
07319 ****************************************************************
07320 *    LOOKING FOR VARIABLE INDICATORS (**XXX) THIS SECTION      *
07321 *    CONTROLS THE CONVERSION OF THESE INDICATORS INTO THEIR    *
07322 *    ACTUAL VALUES.  IT MOVES THESE REPLACEMENTS AND THE       *
07323 *    REMAINING CHARACTERS TO THE WORK LINE.                    *
07324 ****************************************************************
07325
07326      IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
07327              AND
07328          W-LAST-CHAR EQUAL '@'
07329          PERFORM 7430-PROCESS-VARIABLE THRU 7430-EXIT.
07330
07331      SET W-WC-NDX UP BY +1.
07332
07333      IF  W-WC-NDX GREATER THAN +70
07334          IF  W-FORM-SQUEEZE-ON
07335                  AND
07336              NOT W-AS-IS
07337              PERFORM 7440-BACK-TO-NEAREST-WORD THRU 7440-EXIT
07338                      VARYING
07339                  W-WC-NDX FROM W-LAST-WC-SPACE BY +1
07340                      UNTIL
07341                  W-WC-NDX EQUAL +71
07342              SET W-TX-NDX        TO W-LAST-TX-SPACE
07343
07344              IF  W-WORK-LINE GREATER THAN SPACES
07345                  PERFORM 7450-SQUEEZING THRU 7450-EXIT
07346                  MOVE SPACES     TO W-WORK-LINE
07347                                     W-LINE-SQUEEZE-IND
07348                  SET W-WC-NDX    TO +1
07349              ELSE
07350                  SET W-WC-NDX    TO +1
07351          ELSE
07352              PERFORM 7460-REMAIN-CHAR-CHECK THRU 7460-EXIT.
07353
07354      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07355                                  TO W-CHARACTER-TYPE.
07356
07357      SET W-WC-NDX2               TO W-WC-NDX.
07358      SET W-WC-NDX2 DOWN BY +1.
07359
07360      IF  W-PUNCTUATION
07361              AND
07362          W-WC-NDX2 GREATER THAN W-ZEROS
07363              AND
07364          W-WORK-CHAR (W-WC-NDX2) EQUAL SPACES
07365          SET W-WC-NDX DOWN BY +1.
07366
07367      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07368                                  TO W-LAST-CHAR
07369                                     W-WORK-CHAR (W-WC-NDX).
07370
07371      IF  W-LAST-CHAR EQUAL SPACES
07372          SET W-LAST-WC-SPACE     TO W-WC-NDX
07373          SET W-LAST-TX-SPACE     TO W-TX-NDX.
07374
07375  7420-EXIT.
07376      EXIT.
07377                                 EJECT
07378  7430-PROCESS-VARIABLE.
07379 ******************************************************************
07380 *    THIS SECTION CHECKS ON ALL '@@XXX' FORMATED CHARACTER       *
07381 *    GROUPS.  IF IN PROPER FORMAT, EACH GROUP IS REPLACED WITH   *
07382 *    ITS CORRESPONDING VALUES.                                   *
07383 ******************************************************************
07384
07385      SET W-TX-NDX UP BY +1.
07386
07387      IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
07388          MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07389                                  TO W-V1
07390          SET W-TX-NDX UP BY +1
07391
07392          IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
07393              MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07394                                  TO W-V2
07395              SET W-TX-NDX UP BY +1
07396              IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
07397                  MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07398                                  TO W-V3
07399                  SET W-TX-NDX UP BY +1
07400                  MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07401                                  TO W-FIELD-SQUEEZE-IND
07402                  GO TO 7430-CONTINUE
07403              ELSE
07404                  SET W-TX-NDX DOWN BY +3
07408                  MOVE HIGH-VALUES
07409                                  TO W-LAST-CHAR
07410          ELSE
07411              SET W-TX-NDX DOWN BY +2
07415              MOVE HIGH-VALUES    TO W-LAST-CHAR
07416      ELSE
07417          SET W-TX-NDX DOWN BY +1
07421          MOVE HIGH-VALUES        TO W-LAST-CHAR.
07422
07423 ****************************************************************
07424 *    WHEN THE PROPER FORM IS NOT FOUND THE CHARACTERS INVOLVED *
07425 *    ARE TRANSFERRED INTACT AND A NOTE MADE INDICATING THIS    *
07426 *    ACTION.                                                   *
07427 ****************************************************************
07428
07429      IF  W-FIRST-BAD-VARIABLE EQUAL ZEROS
07430          MOVE 'Y'                TO W-FIRST-BAD-VARIABLE-IND
07431          GO TO 7430-EXIT
07432      ELSE
07433          GO TO 7430-EXIT.
07434
07435  7430-CONTINUE.
07436
07437      IF  W-VAR-RELATIVE-NUM LESS THAN 001
07438              OR
07439          W-VAR-RELATIVE-NUM GREATER THAN W-NUM-OF-VARIABLES
07440          SET W-TX-NDX DOWN BY +4
07444          MOVE HIGH-VALUES        TO W-LAST-CHAR
07445          IF  W-FIRST-BAD-VARIABLE EQUAL ZEROS
07446              MOVE 'Y'                TO W-FIRST-BAD-VARIABLE-IND
07447              GO TO 7430-EXIT
07448          ELSE
07449              GO TO 7430-EXIT.
07450
07451      SET W-VG-NDX                TO W-VAR-RELATIVE-NUM.
07452
07453      SET W-WC-NDX DOWN BY +1.
07454
07455      IF  (W-FORM-SQUEEZE-OFF
07456                  AND
07457              NOT W-SQUEEZE-FIELD)
07458              OR
07459          (W-FORM-SQUEEZE-ON
07460                  AND
07461              W-AS-IS)
07462          PERFORM 7435-MOVE-VARIABLE-DIRECT THRU 7435-EXIT
07463                  VARYING
07464              W-VC-NDX FROM +1 BY +1
07465                  UNTIL
07466              W-VC-NDX GREATER THAN
07467                  W-VARIABLE-SIZE (W-VG-NDX)
07468
07469          IF  W-VARIABLE-SIZE (W-VG-NDX) GREATER THAN +5
07470              SET W-TX-NDX UP BY W-VARIABLE-SIZE (W-VG-NDX)
07471              SET W-TX-NDX DOWN BY +5
07472              IF  W-TX-NDX GREATER THAN +70
07473                  SET W-TX-NDX    TO +70
07474                  GO TO 7430-EXIT
07475              ELSE
07476                  GO TO 7430-EXIT
07477          ELSE
07478              COMPUTE W-ADJUST-SHORT
07479                  = 5 - W-VARIABLE-SIZE (W-VG-NDX)
07480              SET W-WC-NDX UP BY W-ADJUST-SHORT
07481              GO TO 7430-EXIT.
07482
07483      IF  W-SQUEEZE-FIELD
07484          SET W-TX-NDX UP BY +1.
07485
07486      MOVE SPACES                 TO W-FIRST-CHAR-FOUND-IND.
07487
07488      PERFORM 7470-MOVE-CHAR THRU 7470-EXIT
07489              VARYING
07490          W-VC-NDX FROM +1 BY +1
07491              UNTIL
07492          W-VC-NDX GREATER THAN
07493              W-VARIABLE-SIZE (W-VG-NDX).
07494
07495      MOVE HIGH-VALUES            TO W-LAST-CHAR.
07496
07497      IF  W-FORM-SQUEEZE-ON
07498          GO TO 7430-EXIT.
07499
07500      IF  W-VARIABLE-SIZE (W-VG-NDX) GREATER THAN +6
07501          SET W-TX-NDX UP BY W-VARIABLE-SIZE (W-VG-NDX)
07502          SET W-TX-NDX DOWN BY +6
07503          IF  W-TX-NDX GREATER THAN +70
07504              SET W-TX-NDX        TO +70.
07505
07506      IF  W-WORK-CHAR (W-WC-NDX) EQUAL SPACES
07507          SET W-WC-NDX DOWN BY +1.
07508
07509  7430-EXIT.
07510      EXIT.
07511                                  EJECT
07512  7435-MOVE-VARIABLE-DIRECT.
07513
07514      IF  W-USE-UPPER-AND-LOWER-CASE (W-VAR-RELATIVE-NUM)
07515              AND
07516          W-VAR-CHAR (W-VG-NDX W-VC-NDX) NOT EQUAL SPACES
07517              AND
07518          W-VAR-CHAR (W-VG-NDX W-VC-NDX) ALPHABETIC
07519              AND
07520          (W-VC-NDX GREATER THAN +1
07521                  AND
07522              W-LAST-CHAR NOT EQUAL SPACE
07523                  AND
07524              NOT W-LAST-CHAR-PUNC)
07525          IF  W-LC-USE-BOTH-CASES
07526              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07527                  CONVERTING W-UPPER-CASE TO W-LOWER-CASE
07528          ELSE
07529              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07530                  CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
07531
07532      SET W-WC-NDX UP BY +1.
07533      MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07534                                  TO W-WORK-CHAR (W-WC-NDX)
07535                                     W-LAST-CHAR.
07536
07537  7435-EXIT.
07538      EXIT.
07539                                  EJECT
07540  7437-FIND-NEXT-CHARACTER.
07541 *DELIBERATELY LEFT BLANK.
07542  7437-EXIT.
07543      EXIT.
07544                                  EJECT
07545  7440-BACK-TO-NEAREST-WORD.
07546
07547      MOVE SPACES                 TO W-WORK-CHAR (W-WC-NDX).
07548
07549  7440-EXIT.
07550      EXIT.
07551                                  EJECT
07552  7450-SQUEEZING.
07553
07554      IF  W-NEW-PARAGRAPH
07555          PERFORM 7600-SET-PARA-INDENT THRU 7600-EXIT
07556          MOVE SPACES             TO W-LINE-SQUEEZE-IND
07557          IF  W-SQUEEZED-LINE GREATER THAN SPACES
07558              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07559              MOVE W-TX-PC (W-TG-NDX)
07560                                  TO W-PRINT-CONTROL
07561              MOVE SPACES         TO W-SQUEEZED-LINE
07562              SET W-SQ-NDX        TO W-INITIAL-COLUMN
07563              SET W-SQ-NDX UP BY W-WORK-INDENT
07564          ELSE
07565              MOVE W-TX-PC (W-TG-NDX)
07566                                  TO W-PRINT-CONTROL
07567              SET W-SQ-NDX        TO W-INITIAL-COLUMN
07568              SET W-SQ-NDX UP BY W-WORK-INDENT
07569      ELSE
07570          IF  W-DO-NOT-ADJUST
07571              MOVE SPACES         TO W-LINE-SQUEEZE-IND
07572              IF  W-SQUEEZED-LINE GREATER THAN SPACES
07573                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07574                  MOVE W-WORK-LINE
07575                                  TO W-SQUEEZED-LINE
07576                  MOVE W-TX-PC (W-TG-NDX)
07577                                  TO W-PRINT-CONTROL
07578                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07579                  SET W-SQ-NDX    TO W-INITIAL-COLUMN
07580                  MOVE SPACES     TO W-SQUEEZED-LINE
07581                  GO TO 7450-EXIT
07582              ELSE
07583                  MOVE W-WORK-LINE
07584                                  TO W-SQUEEZED-LINE
07585                  MOVE W-TX-PC (W-TG-NDX)
07586                                  TO W-PRINT-CONTROL
07587                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07588                  MOVE SPACES     TO W-SQUEEZED-LINE
07589                                     W-WORK-LINE
07590                  SET W-SQ-NDX    TO W-INITIAL-COLUMN
07591                  GO TO 7450-EXIT
07592          ELSE
07593              IF  W-ADJUST-TO-LINE-LENGTH
07594                  MOVE SPACES     TO W-LINE-SQUEEZE-IND
07595
07596                  IF  W-SQUEEZED-LINE GREATER THAN SPACES
07597                      PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07598                      SET W-SQ-NDX
07599                                  TO W-INITIAL-COLUMN
07600                      PERFORM 7505-MOVE-STRAIGHT THRU 7505-EXIT
07601                              VARYING
07602                          W-WC-NDX FROM 1 BY 1
07603                              UNTIL
07604                          W-SQ-NDX EQUAL W-TOO-FAR
07605                      MOVE W-TX-PC (W-TG-NDX)
07606                                  TO W-PRINT-CONTROL
07607                      PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07608                      SET W-SQ-NDX
07609                                  TO W-INITIAL-COLUMN
07610                      MOVE SPACES TO W-SQUEEZED-LINE
07611                                     W-WORK-LINE
07612                      GO TO 7450-EXIT
07613                  ELSE
07614                      SET W-SQ-NDX
07615                                  TO W-INITIAL-COLUMN
07616                      PERFORM 7505-MOVE-STRAIGHT THRU 7505-EXIT
07617                              VARYING
07618                          W-WC-NDX FROM 1 BY 1
07619                              UNTIL
07620                          W-SQ-NDX EQUAL W-TOO-FAR
07621                      MOVE W-TX-PC (W-TG-NDX)
07622                                  TO W-PRINT-CONTROL
07623                      PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07624                      MOVE SPACES TO W-SQUEEZED-LINE
07625                                     W-WORK-LINE
07626                      SET W-SQ-NDX
07627                                  TO W-INITIAL-COLUMN
07628                      GO TO 7450-EXIT
07629              ELSE
07630                  IF  W-CONTINUE-PARAGRAPH
07631                      IF  W-SQUEEZED-LINE GREATER THAN SPACES
07632                          PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07633                          PERFORM 7610-SET-CONP-INDENT
07634                              THRU 7610-EXIT
07635                          MOVE SPACES
07636                                  TO W-LINE-SQUEEZE-IND
07637                          MOVE W-TX-PC (W-TG-NDX)
07638                                  TO W-PRINT-CONTROL
07639                          MOVE SPACES
07640                                  TO W-SQUEEZED-LINE
07641                          SET W-SQ-NDX
07642                                  TO W-INITIAL-COLUMN
07643                          SET W-SQ-NDX UP BY W-WORK-INDENT
07644                      ELSE
07645                          PERFORM 7610-SET-CONP-INDENT
07646                              THRU 7610-EXIT
07647                          MOVE SPACES
07648                                  TO W-LINE-SQUEEZE-IND
07649                          MOVE W-TX-PC (W-TG-NDX)
07650                                  TO W-PRINT-CONTROL
07651                          MOVE SPACES
07652                                  TO W-SQUEEZED-LINE
07653                          SET W-SQ-NDX
07654                                  TO W-INITIAL-COLUMN
07655                          SET W-SQ-NDX UP BY W-WORK-INDENT.
07656
07657
07658      PERFORM 7480-UPDATE-SQUEEZED-LINE THRU 7480-EXIT
07659              VARYING
07660          W-WC-NDX FROM +1 BY +1
07661              UNTIL
07662          W-WC-NDX GREATER THAN +70.
07663
07664  7450-EXIT.
07665      EXIT.
07666                                  EJECT
07667  7460-REMAIN-CHAR-CHECK.
07668
07669      IF  W-TX-NDX EQUAL W-WC-NDX
07670          MOVE W-TX-PC (W-TG-NDX)
07671                                  TO W-PRINT-CONTROL
07672          MOVE W-WORK-LINE        TO W-SQUEEZED-LINE
07673          PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07674          MOVE SPACES             TO W-WORK-LINE
07675          SET W-WC-NDX            TO +1
07676      ELSE
07677          SET W-TX-NDX1           TO W-TX-NDX
07678          SEARCH W-TX-CHAR
07679              VARYING W-TX-NDX1
07680              AT END
07681                  MOVE W-TX-PC (W-TG-NDX)
07682                                  TO W-PRINT-CONTROL
07683                  MOVE W-WORK-LINE
07684                                  TO W-SQUEEZED-LINE
07685                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07686                  MOVE SPACES     TO W-WORK-LINE
07687                  SET W-WC-NDX    TO +1
07688                  SET W-TX-NDX    TO +1
07689                  SET W-TG-NDX UP BY +1
07690              WHEN
07691                  W-TX-CHAR (W-TG-NDX W-TX-NDX1) NOT EQUAL SPACES
07695                  MOVE W-TX-PC (W-TG-NDX)
07696                                  TO W-PRINT-CONTROL
07697                  MOVE W-WORK-LINE
07698                                  TO W-SQUEEZED-LINE
07699                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07700                  MOVE SPACES     TO W-WORK-LINE
07701                  SET W-WC-NDX    TO +1.
07702
07703  7460-EXIT.
07704      EXIT.
07705                                  EJECT
07706  7470-MOVE-CHAR.
07707
07708      IF  W-FIRST-CHAR-NOT-FOUND
07709              AND
07710          W-VAR-CHAR (W-VG-NDX W-VC-NDX) EQUAL SPACES
07711          MOVE SPACES             TO W-LAST-CHAR
07712          GO TO 7470-EXIT.
07713
07714      MOVE 'Y'                    TO W-FIRST-CHAR-FOUND-IND.
07715
07716      IF  W-SQUEEZE-FIELD
07717              AND
07718          W-LAST-CHAR EQUAL SPACES
07719              AND
07720          W-VAR-CHAR (W-VG-NDX W-VC-NDX) EQUAL SPACES
07721          GO TO 7470-EXIT.
07722
07723      SET W-WC-NDX UP BY +1.
07724
07725      IF  W-WC-NDX GREATER THAN +70
07726          IF  W-FORM-SQUEEZE-ON
07727              GO TO 7470-EXIT
07728          ELSE
07732              GO TO 7470-EXIT.
07733
07734      IF  W-USE-UPPER-AND-LOWER-CASE (W-VAR-RELATIVE-NUM)
07735              AND
07736          W-VAR-CHAR (W-VG-NDX W-VC-NDX) NOT EQUAL SPACES
07737              AND
07738          W-VAR-CHAR (W-VG-NDX W-VC-NDX) ALPHABETIC
07739              AND
07740          (W-VC-NDX GREATER THAN +1
07741                  AND
07742              W-LAST-CHAR NOT EQUAL SPACE
07743                  AND
07744              NOT W-LAST-CHAR-PUNC)
07745          IF  W-LC-USE-BOTH-CASES
07746              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07747                  CONVERTING W-UPPER-CASE TO W-LOWER-CASE
07748          ELSE
07749              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07750                  CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
07751
07752      MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07753                                  TO W-WORK-CHAR (W-WC-NDX)
07754                                     W-LAST-CHAR.
07755
07756      IF  W-LAST-CHAR EQUAL SPACES
07757          SET W-LAST-WC-SPACE     TO W-WC-NDX.
07758
07759  7470-EXIT.
07760      EXIT.
07761                                  EJECT
07762  7480-UPDATE-SQUEEZED-LINE.
07763 ****************************************************************
07764 *    THIS SECTION CONTROLS THE MOVEMENT FROM THE WORK LINE TO *
07765 *    THE SQUEEZED LINE.  DURING THIS PROCESS ALL EXTRA SPACES  *
07766 *    (DEFINED AS TWO OR MORE SPACES NOT FOLLOWING A END OF     *
07767 *    SENTENCE INDICATOR) ARE REMOVED.  IF THE LAST CHARACTER   *
07768 *    IS NOT A SPACE OR PUNCTUATION MARK AN UNFINISHED WORD IS  *
07769 *    ASSUMED.  EVERYTHING TO THE PREVIOUS SPACE IS REMOVED     *
07770 ****************************************************************
07771
07772      SET W-SQ-NDX UP BY +1.
07773
07774      IF  W-SQ-NDX GREATER THAN W-LAST-COLUMN
07775          MOVE W-SQ-CHAR (W-LAST-COLUMN)
07776                                  TO W-CHARACTER-TYPE
07777          IF  W-PUNCTUATION
07778                  OR
07779              W-SPACE
07780              PERFORM 7485-FIND-NEXT-CHARACTER THRU 7485-EXIT
07781              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07782              MOVE SPACES         TO W-SQUEEZED-LINE
07783              SET W-SQ-NDX        TO W-START-COLUMN
07784              SET W-SQ-NDX UP BY W-NEXT-INDENT
07785              SET W-LAST-SQUEEZED-SPACE
07786                                  TO W-SQ-NDX
07787          ELSE
07788              PERFORM 7490-REMOVE-LAST-WORD THRU 7490-EXIT
07789              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07790              MOVE SPACES         TO W-SQUEEZED-LINE
07791              SET W-SQ-NDX        TO W-START-COLUMN
07792              SET W-SQ-NDX UP BY W-NEXT-INDENT
07793              SET W-LAST-SQUEEZED-SPACE
07794                                  TO W-SQ-NDX.
07795
07796      IF  W-WC-NDX GREATER THAN +70
07797          GO TO 7480-EXIT.
07798
07799      MOVE W-WORK-CHAR (W-WC-NDX) TO W-CHARACTER-TYPE.
07800
07801      IF  W-END-OF-SENTENCE
07802          MOVE 'Y'                TO W-END-OF-SENTENCE-IND
07803          SET W-WORK-NDX          TO W-SQ-NDX
07804          COMPUTE W-DISPLAY-NDX = W-WORK-NDX - 1
07805          IF  W-DISPLAY-NDX EQUAL W-LAST-SQUEEZED-SPACE
07806              SET W-SQ-NDX        TO W-LAST-SQUEEZED-SPACE
07807          ELSE
07808              NEXT SENTENCE
07809      ELSE
07810          IF  W-PUNCTUATION
07811                  AND
07812              W-LAST-SQ-CHAR EQUAL SPACES
07813              SET W-SQ-NDX DOWN BY +1
07814              MOVE SPACES         TO W-END-OF-SENTENCE-IND
07815          ELSE
07816              IF  W-WORK-CHAR (W-WC-NDX) EQUAL SPACES
07817                  IF  W-LAST-SQ-CHAR EQUAL SPACES
07818                      IF  W-END-OF-SENTENCE-WORKING
07819                          MOVE SPACES
07820                                  TO W-END-OF-SENTENCE-IND
07821                          SET W-LAST-SQUEEZED-SPACE
07822                                  TO W-SQ-NDX
07823                      ELSE
07824                          SET W-SQ-NDX DOWN BY +1
07825                          GO TO 7480-EXIT
07826                  ELSE
07827                      SET W-LAST-SQUEEZED-SPACE
07828                                  TO W-SQ-NDX
07829              ELSE
07830                  MOVE SPACES     TO W-END-OF-SENTENCE-IND.
07831
07832      MOVE W-WORK-CHAR (W-WC-NDX) TO W-SQ-CHAR (W-SQ-NDX)
07833                                     W-LAST-SQ-CHAR.
07834
07835      IF  W-WC-NDX EQUAL +70
07836              AND
07837          W-WORK-CHAR (W-WC-NDX) GREATER THAN SPACES
07838              AND
07839          W-SQ-NDX NOT EQUAL W-LAST-COLUMN
07840          SET W-SQ-NDX UP BY +1
07841          SET W-LAST-SQUEEZED-SPACE
07842                                  TO W-SQ-NDX
07843          MOVE SPACES             TO W-SQ-CHAR (W-SQ-NDX)
07844                                     W-LAST-SQ-CHAR.
07845
07846  7480-EXIT.
07847      EXIT.
07848                                  EJECT
07849  7485-FIND-NEXT-CHARACTER.
07850
07851      MOVE W-WORK-CHAR (W-WC-NDX) TO W-CHARACTER-TYPE.
07852
07853      IF  W-PUNCTUATION
07854          PERFORM 7490-REMOVE-LAST-WORD THRU 7490-EXIT
07855      ELSE
07856          SET W-WC-NDX            TO W-WC-NDX
07857          SEARCH W-WORK-CHAR
07858              VARYING W-WC-NDX
07859              AT END
07860                  GO TO 7485-EXIT
07861              WHEN
07862                  W-WORK-CHAR (W-WC-NDX) NOT EQUAL SPACES
07863                  GO TO 7485-EXIT.
07864
07865  7485-EXIT.
07866      EXIT.
07867                                  EJECT
07868  7490-REMOVE-LAST-WORD.
07869
07870 *********  NOTE: W-WORK-LINE ALWAYS STARTS WITH A FULL WORD*******
07871      IF  W-WC-NDX EQUAL +1
07872          GO TO 7490-EXIT.
07873
07874      ADD +1                      TO W-LAST-SQUEEZED-SPACE.
07875
07876      PERFORM 7500-CLEAR-SQUEEZED-EXCESS THRU 7500-EXIT
07877              VARYING
07878          W-SQ-NDX FROM W-LAST-SQUEEZED-SPACE BY +1
07879              UNTIL
07880          W-SQ-NDX EQUAL W-TOO-FAR.
07881
07882      SET W-WORK-NDX              TO W-WC-NDX.
07883      COMPUTE W-LAST-WC-SPACE
07884          = W-WORK-NDX - W-TOO-FAR + W-LAST-SQUEEZED-SPACE.
07885      SET W-WC-NDX                TO W-LAST-WC-SPACE.
07886
07887  7490-EXIT.
07888      EXIT.
07889                                  EJECT
07890  7500-CLEAR-SQUEEZED-EXCESS.
07891
07892      MOVE SPACES                 TO W-SQ-CHAR (W-SQ-NDX).
07893
07894  7500-EXIT.
07895      EXIT.
07896
07897  7505-MOVE-STRAIGHT.
07898
07899      SET W-SQ-NDX UP BY +1.
07900
07901      MOVE W-WORK-CHAR (W-WC-NDX) TO W-SQ-CHAR (W-SQ-NDX).
07902
07903  7505-EXIT.
07904      EXIT.
07905                                  EJECT
07906  7510-MOVE-TO-TABLE.
07907
07908      IF  W-PRINT-CONTROL NOT NUMERIC
07909          MOVE 00                 TO W-PRINT-CONTROL
07910          ADD +1                  TO W-LINE-COUNT
07911      ELSE
07912          IF  W-PRINT-CONTROL EQUAL 99
07913              PERFORM 7640-PAGING THRU 7640-EXIT
07914              MOVE 00             TO W-PRINT-CONTROL
07915              SET W-RG-NDX UP BY +1
07916              MOVE W-TOP-FORM     TO W-RC-TEXT (W-RG-NDX)
07917              MOVE +1             TO W-LINE-COUNT
07918              SET W-RG-NDX UP BY W-TOP-MARGIN
07919          ELSE
07920              COMPUTE W-LINE-COUNT
07921                  = W-LINE-COUNT + W-PRINT-CONTROL + 1.
07922
07923      IF  W-LINE-COUNT GREATER THAN W-MAX-LINES-PER-PAGE
07924          PERFORM 7640-PAGING THRU 7640-EXIT
07925          MOVE 00                 TO W-PRINT-CONTROL
07926          SET W-RG-NDX UP BY +1
07927          MOVE W-TOP-FORM         TO W-RC-TEXT (W-RG-NDX)
07928          SET W-RG-NDX UP BY W-TOP-MARGIN
07929          MOVE +1                 TO W-LINE-COUNT.
07930
07931      SET W-RG-NDX UP BY W-PRINT-CONTROL.
07932      SET W-RG-NDX UP BY +1.
07933      MOVE W-SQUEEZED-LINE        TO W-RC-TEXT (W-RG-NDX).
07934      MOVE 00                     TO W-PRINT-CONTROL.
07935
07936      IF  W-FIRST-BAD-VARIABLE-FOUND
07937              AND
07938          W-FIRST-BAD-VARIABLE EQUAL ZEROS
07939          SET W-FIRST-BAD-VARIABLE
07940                                  TO W-RG-NDX.
07941
07942  7510-EXIT.
07943      EXIT.
07944                                  EJECT
07945  7600-SET-PARA-INDENT.
07946
07947      IF  W-LINE-SQUEEZE-IND EQUAL 'P'
07948          MOVE W-PARAGRAPH-INDENT TO W-WORK-INDENT
07949      ELSE
07950      IF  W-LINE-SQUEEZE-IND EQUAL 'Q'
07951          MOVE W-LINE-INDENT-1    TO W-WORK-INDENT
07952          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07953      ELSE
07954      IF  W-LINE-SQUEEZE-IND EQUAL 'R'
07955          MOVE W-LINE-INDENT-2    TO W-WORK-INDENT
07956          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07957      ELSE
07958      IF  W-LINE-SQUEEZE-IND EQUAL 'S'
07959          MOVE W-LINE-INDENT-3    TO W-WORK-INDENT
07960          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07961      ELSE
07962      IF  W-LINE-SQUEEZE-IND EQUAL 'T'
07963          MOVE W-LINE-INDENT-4    TO W-WORK-INDENT
07964          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07965      ELSE
07966      IF  W-LINE-SQUEEZE-IND EQUAL 'U'
07967          MOVE W-LINE-INDENT-5    TO W-WORK-INDENT
07968          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT.
07969
07970      SET W-TG-NDX2               TO W-TG-NDX.
07971      SET W-TG-NDX2 UP BY +1.
07972
07973      IF  W-TG-NDX EQUAL W-TOTAL-TX-LINES
07974          MOVE W-WORK-INDENT      TO W-NEXT-INDENT
07975      ELSE
07976          PERFORM 7630-NEXT-LINE THRU 7630-EXIT.
07977
07978  7600-EXIT.
07979      EXIT.
07980                                  EJECT
07981  7610-SET-CONP-INDENT.
07982
07983      IF  W-LINE-SQUEEZE-IND EQUAL 'C'
07984          MOVE +0                 TO W-WORK-INDENT
07985      ELSE
07986      IF  W-LINE-SQUEEZE-IND EQUAL 'D'
07987          MOVE W-LINE-INDENT-1    TO W-WORK-INDENT
07988      ELSE
07989      IF  W-LINE-SQUEEZE-IND EQUAL 'E'
07990          MOVE W-LINE-INDENT-2    TO W-WORK-INDENT
07991      ELSE
07992      IF  W-LINE-SQUEEZE-IND EQUAL 'F'
07993          MOVE W-LINE-INDENT-3    TO W-WORK-INDENT
07994      ELSE
07995      IF  W-LINE-SQUEEZE-IND EQUAL 'G'
07996          MOVE W-LINE-INDENT-4    TO W-WORK-INDENT
07997      ELSE
07998      IF  W-LINE-SQUEEZE-IND EQUAL 'H'
07999          MOVE W-LINE-INDENT-5    TO W-WORK-INDENT.
08000
08001      SET W-TG-NDX2               TO W-TG-NDX.
08002      SET W-TG-NDX2 UP BY +1.
08003
08004      IF  W-TG-NDX EQUAL W-TOTAL-TX-LINES
08005          MOVE W-WORK-INDENT      TO W-NEXT-INDENT
08006      ELSE
08007          PERFORM 7630-NEXT-LINE THRU 7630-EXIT.
08008
08009  7610-EXIT.
08010      EXIT.
08011                                  EJECT
08012  7630-NEXT-LINE.
08013
08014      IF  W-TX-SC (W-TG-NDX2) EQUAL ' '
08015          MOVE ZEROS              TO W-NEXT-INDENT
08016      ELSE
08017      IF  W-TX-SC (W-TG-NDX2) EQUAL '1'
08018          MOVE W-LINE-INDENT-1    TO W-NEXT-INDENT
08019      ELSE
08020      IF  W-TX-SC (W-TG-NDX2) EQUAL '2'
08021          MOVE W-LINE-INDENT-2    TO W-NEXT-INDENT
08022      ELSE
08023      IF  W-TX-SC (W-TG-NDX2) EQUAL '3'
08024          MOVE W-LINE-INDENT-3    TO W-NEXT-INDENT
08025      ELSE
08026      IF  W-TX-SC (W-TG-NDX2) EQUAL '4'
08027          MOVE W-LINE-INDENT-4    TO W-NEXT-INDENT
08028      ELSE
08029      IF  W-TX-SC (W-TG-NDX2) EQUAL '5'
08030          MOVE W-LINE-INDENT-5    TO W-NEXT-INDENT
08031      ELSE
08032          MOVE W-WORK-INDENT      TO W-NEXT-INDENT
08033          SUBTRACT W-PARAGRAPH-INDENT
08034                                   FROM W-NEXT-INDENT.
08035
08036  7630-EXIT.
08037      EXIT.
08038                                  EJECT
08039  7640-PAGING.
08040
08041      IF  NOT W-LC-CREATE-PAGES
08042          GO TO 7640-EXIT.
08043
08044      IF  W-LINE-COUNT GREATER ZEROS
08045              AND
08046          W-RG-NDX GREATER THAN +1
08047          ADD +1                  TO W-PAGE
08048          MOVE W-PAGE             TO W-PAGE-NUMBER
08049          COMPUTE W-PAGE-LINE
08050              = W-MAX-LINES-PER-PAGE - W-LINE-COUNT + 3
08051          IF  W-PAGE-LINE GREATER THAN +2
08052              SET W-RG-NDX UP BY W-PAGE-LINE
08053              MOVE W-PAGE-PRT     TO W-RC-TEXT (W-RG-NDX)
08054          ELSE
08055              SET W-RG-NDX UP BY +2
08056              MOVE W-PAGE-PRT     TO W-RC-TEXT (W-RG-NDX).
08057
08058  7640-EXIT.
08059      EXIT.
08291      EJECT
08444
08445  7915-SEARCH-REMAINING-VARS.
08446
08447      IF  W-REC-CHAR (W-RVS-NDX) EQUAL '@'
08448          SET W-RVS-NDX2          TO W-RVS-NDX
08449          SET W-RVS-NDX2 UP BY +1
08450          IF  W-REC-CHAR (W-RVS-NDX2) EQUAL '@'
08451              SET W-RVS-NDX2 UP BY +1
08452              IF  W-REC-CHAR (W-RVS-NDX2) NUMERIC
08453                  SET PI-CURRENT-LINE  TO W-RVS-NDX
08454                  COMPUTE PI-CURRENT-LINE
08455                      = PI-CURRENT-LINE / 73
08456                  MOVE 'Y'        TO W-REMAINING-VAR-SW
08457                  MOVE +0         TO W-ROLL-COUNTER.
08461
08462  7915-EXIT.
08463      EXIT.
08783                                  EJECT
       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8500-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
08900
08952
08953  9999-GOBACK.
08954
08955      GOBACK.
08956
08957  9999-EXIT.
08958      EXIT.
