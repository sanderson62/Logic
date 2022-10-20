00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL1043
00003  PROGRAM-ID.                 EL1043.                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 09:40:03.                    CL**3
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL1043
00008 *                                                                 EL1043
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL1043
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL1043
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL1043
00024 *REMARKS.    TRANSACTION - EXM7 - CREDIT VARIABLE CONVERTER       EL1043
00025                                                                   EL1043
00026  ENVIRONMENT DIVISION.                                            EL1043
00027                                                                   EL1043
00028  DATA DIVISION.                                                   EL1043
00029  WORKING-STORAGE SECTION.                                         EL1043
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL1043
00031  77  FILLER  PIC X(32)  VALUE '*    EL1043 WORKING STORAGE    *'. EL1043
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.    CL**3
00033                                  EJECT                            EL1043
00034                                                                   EL1043
00035  01  W-WORK-AREAS.                                                EL1043
00036      12  FILLER                  PIC  X(18)                       EL1043
00037                                       VALUE 'PROGRAM WORK AREA:'. EL1043
00038      12  W-EXTRA-LENGTH          PIC S9(04)  COMP   VALUE +0.     EL1043
00039      12  W-TOTAL-TX-LINES        PIC S9(03)  COMP-3 VALUE +0.     EL1043
00040      12  W-LAST-SKIP-LINES       PIC  9(02)         VALUE  0.     EL1043
00041      12  W-SKIP-LINES            PIC  9(02)         VALUE  0.     EL1043
00042                                                                   EL1043
00043      12  W-CALL-PGM              PIC  X(08).                      EL1043
00044      12  W-SAVE-BIN-DATE         PIC  X(02)  VALUE SPACES.        EL1043
00045      12  W-SAVE-DATE             PIC  X(08)  VALUE SPACES.        EL1043
00046      12  W-SAVE-PLAN             PIC  X(02)  VALUE SPACES.        EL1043
00047      12  W-TEXT-RECORD.                                           EL1043
00048          16  FILLER              PIC  X(02).                      EL1043
00049          16  W-TR-KEY.                                            EL1043
00050              20  FILLER          PIC  X(01).                      EL1043
00051              20  W-TR-LETTER-NO  PIC  X(04).                      EL1043
00052              20  FILLER          PIC  X(08).                      EL1043
00053              20  W-TR-SEQ-NO     PIC S9(04) COMP.                 EL1043
00054          16  FILLER              PIC  X(02).                      EL1043
00055          16  W-TR-TEXT-LINE      PIC  X(70).                      EL1043
00056          16  FILLER              PIC  X(11).                      EL1043
00057                                                                   EL1043
00058                                  EJECT                            EL1043
00059  01  W-SWITCH-INDICATORS-AREA.                                    EL1043
00060      12  FILLER                  PIC  X(16)                       EL1043
00061                                       VALUE 'PROGRAM SWITCHS:'.   EL1043
00062      12  W-TEXT-BROWSED-SW       PIC  X(01)  VALUE 'N'.           EL1043
00063          88  W-TEXT-BROWSE-STARTED           VALUE 'Y'.           EL1043
00064          88  W-TEXT-BROWSE-NOT-STARTED       VALUE 'N'.           EL1043
00065                                  EJECT                            EL1043
00066  01  W-KEY-AREAS.                                                 EL1043
00067      12  FILLER                  PIC  X(13)                       EL1043
00068                                       VALUE 'PROGRAM KEYS:'.      EL1043
00069                                                                   EL1043
00070      12  W-TEXT-SAVE-KEY         PIC  X(05).                      EL1043
00071      12  W-TEXT-KEY.                                              EL1043
00072          16  W-TEXT-PARTIAL-KEY.                                  EL1043
00073              20  W-TEXT-COMPANY-CD                                EL1043
00074                                  PIC  X(01).                      EL1043
00075              20  W-TEXT-LETTER   PIC  X(04).                      EL1043
00076          16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.       EL1043
00077          16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.   EL1043
00078                                                                   EL1043
00079                                  EJECT                            EL1043
00080  01  FILLER                      PIC  X(22)                       EL1043
00081                                  VALUE 'INTERFACE AREA STARTS:'.  EL1043
00082      COPY ELCINTF.                                                EL1043
00083      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL1043
00084      COPY ELC1042.                                                EL1043
00085          16  FILLER              PIC X(444).                      EL1043
00086          16  PI-OLD-NAME         PIC  X(04).                      EL1043
00087          16  PI-NEW-NAME         PIC  X(04).                      EL1043
00088          16  PI-1043-ERROR       PIC  9(04).                      EL1043
00089          16  FILLER              PIC X(117).                         CL**3
00090                                                                   EL1043
00091  01  FILLER                      PIC  X(20)                       EL1043
00092                                  VALUE ':INTERFACE AREA ENDS'.    EL1043
00093                                  EJECT                            EL1043
00094  01  W-CONSTANT-AREA.                                             EL1043
00095      12  FILLER                  PIC  X(18)                       EL1043
00096                                  VALUE 'PROGRAM CONSTANTS:'.      EL1043
00097      12  W-ZEROS                 PIC S9(04)  VALUE ZEROS.         EL1043
00098                                                                   EL1043
00099      12  W-LGXX-ID               PIC  X(04)  VALUE 'LGXX'.        EL1043
00100      12  W-LINK-001              PIC  X(05)  VALUE 'EL001'.       EL1043
00101      12  W-LINK-004              PIC  X(05)  VALUE 'EL004'.       EL1043
00102      12  W-LINK-ELDATCV          PIC  X(07)  VALUE 'ELDATCV'.     EL1043
00103      12  W-PGM-EL626             PIC  X(08)  VALUE 'EL626'.       EL1043
00104      12  W-TEXT-FILE-ID          PIC  X(08)  VALUE 'ELLETR'.      EL1043
00105      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL1043'.      EL1043
00106      12  W-TOP-FORM              PIC  X(70)                       EL1043
00107                               VALUE '*****TOP OF FORM *****'.     EL1043
00108      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXM7'.        EL1043
00109      12  W-XCTL-005              PIC  X(05)  VALUE 'EL005'.       EL1043
00110      12  W-XCTL-010              PIC  X(05)  VALUE 'EL010'.       EL1043
00111      12  W-XCTL-626              PIC  X(05)  VALUE 'EL626'.       EL1043
00112                                  EJECT                            EL1043
00113  01  W-VARIABLE-PROCESS-CNTLS.                                    EL1043
00114      12  FILLER                  PIC  X(26)                       EL1043
00115                         VALUE 'VARIABLE WORK AREA STARTS:'.       EL1043
00116      12  W-VAR-HOLD.                                              EL1043
00117          16  W-V1                PIC  X(01).                      EL1043
00118          16  W-V2                PIC  X(01).                      EL1043
00119          16  W-V3                PIC  X(01).                      EL1043
00120          16  W-V4                PIC  X(01).                      EL1043
00121                                                                   EL1043
00122  01  W-SUPPORTED-VARIABLES.                                       EL1043
00123 *****************COMPANY VARIABLES - ELCNTL ********************  EL1043
00124 *****COMPANY NAME                                                 EL1043
00125      12  FILLER                  PIC  X(04) VALUE    '01.0'.      EL1043
00126      12  FILLER                  PIC  X(06) VALUE  '@@001#'.      EL1043
00127 *****FULL COMPANY ADDRESS                                         EL1043
00128      12  FILLER                  PIC  X(04) VALUE    '02.1'.      EL1043
00129      12  FILLER                  PIC  X(06) VALUE  '@@002#'.      EL1043
00130      12  FILLER                  PIC  X(04) VALUE    '02.2'.      EL1043
00131      12  FILLER                  PIC  X(06) VALUE  '@@003#'.      EL1043
00132      12  FILLER                  PIC  X(04) VALUE    '02.3'.      EL1043
00133      12  FILLER                  PIC  X(06) VALUE  '@@004#'.      EL1043
00134      12  FILLER                  PIC  X(04) VALUE    '02.4'.      EL1043
00135      12  FILLER                  PIC  X(06) VALUE  '@@005#'.      EL1043
00136      12  FILLER                  PIC  X(04) VALUE    '02.5'.      EL1043
00137      12  FILLER                  PIC  X(06) VALUE  '@@006#'.      EL1043
00138 ************ LIFE BENEFIT VARIABLES - ELCNTL *******************  EL1043
00139 *****LIFE BENEFIT DESCRIPTION                                     EL1043
00140      12  FILLER                  PIC  X(04) VALUE    '12.0'.      EL1043
00141      12  FILLER                  PIC  X(06) VALUE  '@@011#'.      EL1043
00142 ************* A&H BENEFIT VARIABLES - ELCNTL *******************  EL1043
00143 *****AH BENEFIT DESCRIPTION                                       EL1043
00144      12  FILLER                  PIC  X(04) VALUE    '12.5'.      EL1043
00145      12  FILLER                  PIC  X(06) VALUE  '@@015#'.      EL1043
00146 *****ELIMINATION PERIOD                                           EL1043
00147      12  FILLER                  PIC  X(04) VALUE    '27.0'.      EL1043
00148      12  FILLER                  PIC  X(06) VALUE  '@@016#'.      EL1043
00149 *****************CARRIER VARIABLES - ELCNTL ********************  EL1043
00150 *****CARRIER NAME                                                 EL1043
00151      12  FILLER                  PIC  X(04) VALUE    '03.0'.      EL1043
00152      12  FILLER                  PIC  X(06) VALUE  '@@020#'.      EL1043
00153 *****FULL CARRIER ADDRESS                                         EL1043
00154      12  FILLER                  PIC  X(04) VALUE    '04.1'.      EL1043
00155      12  FILLER                  PIC  X(06) VALUE  '@@021#'.      EL1043
00156      12  FILLER                  PIC  X(04) VALUE    '04.2'.      EL1043
00157      12  FILLER                  PIC  X(06) VALUE  '@@022#'.      EL1043
00158      12  FILLER                  PIC  X(04) VALUE    '04.3'.      EL1043
00159      12  FILLER                  PIC  X(06) VALUE  '@@023#'.      EL1043
00160      12  FILLER                  PIC  X(04) VALUE    '04.4'.      EL1043
00161      12  FILLER                  PIC  X(06) VALUE  '@@024#'.      EL1043
00162      12  FILLER                  PIC  X(04) VALUE    '04.5'.      EL1043
00163      12  FILLER                  PIC  X(06) VALUE  '@@025#'.      EL1043
00164 *****CARRIER PHONE NUMBER                                         EL1043
00165      12  FILLER                  PIC  X(04) VALUE    '04.6'.      EL1043
00166      12  FILLER                  PIC  X(06) VALUE  '@@026#'.      EL1043
00167 ***************** MAIL VARIABLES - ELMAIL **********************  EL1043
00168 *****FULL MAIL ADDRESS                                            EL1043
00169      12  FILLER                  PIC  X(04) VALUE    '05.1'.      EL1043
00170      12  FILLER                  PIC  X(06) VALUE  '@@031#'.      EL1043
00171      12  FILLER                  PIC  X(04) VALUE    '05.2'.      EL1043
00172      12  FILLER                  PIC  X(06) VALUE  '@@032#'.      EL1043
00173      12  FILLER                  PIC  X(04) VALUE    '05.3'.      EL1043
00174      12  FILLER                  PIC  X(06) VALUE  '@@033#'.      EL1043
00175      12  FILLER                  PIC  X(04) VALUE    '05.4'.      EL1043
00176      12  FILLER                  PIC  X(06) VALUE  '@@034#'.      EL1043
00177      12  FILLER                  PIC  X(04) VALUE    '05.5'.      EL1043
00178      12  FILLER                  PIC  X(06) VALUE  '@@035#'.      EL1043
00179      12  FILLER                  PIC  X(04) VALUE    '05.6'.      EL1043
00180      12  FILLER                  PIC  X(06) VALUE  '@@036#'.      EL1043
00181 *************** ACCOUNT VARIABLES - ERACCT *********************  EL1043
00182 *****ACCOUNT NAME                                                 EL1043
00183      12  FILLER                  PIC  X(04) VALUE    '06.0'.      EL1043
00184      12  FILLER                  PIC  X(06) VALUE  '@@040#'.      EL1043
00185 *****FULL ACCOUNT ADDRESS                                         EL1043
00186      12  FILLER                  PIC  X(04) VALUE    '07.1'.      EL1043
00187      12  FILLER                  PIC  X(06) VALUE  '@@041#'.      EL1043
00188      12  FILLER                  PIC  X(04) VALUE    '07.2'.      EL1043
00189      12  FILLER                  PIC  X(06) VALUE  '@@042#'.      EL1043
00190      12  FILLER                  PIC  X(04) VALUE    '07.3'.      EL1043
00191      12  FILLER                  PIC  X(06) VALUE  '@@043#'.      EL1043
00192      12  FILLER                  PIC  X(04) VALUE    '07.4'.      EL1043
00193      12  FILLER                  PIC  X(06) VALUE  '@@044#'.      EL1043
00194      12  FILLER                  PIC  X(04) VALUE    '07.5'.      EL1043
00195      12  FILLER                  PIC  X(06) VALUE  '@@045#'.      EL1043
00196 *****ACCOUNT PHONE NUMBER                                         EL1043
00197      12  FILLER                  PIC  X(04) VALUE    '07.6'.      EL1043
00198      12  FILLER                  PIC  X(06) VALUE  '@@046#'.      EL1043
00199 *************** NON FILE VARIABLES *****************************  EL1043
00200 *****CURRENT DATE                                                 EL1043
00201      12  FILLER                  PIC  X(04) VALUE    '10.0'.      EL1043
00202      12  FILLER                  PIC  X(06) VALUE  '@@060#'.      EL1043
00203 *****FULL CURRENT DATE                                            EL1043
00204      12  FILLER                  PIC  X(04) VALUE    '11.0'.      EL1043
00205      12  FILLER                  PIC  X(06) VALUE  '@@061#'.      EL1043
00206 ************** CERTIFICATE VARIABLES - ELCERT *****************   EL1043
00207 *****CARRIER CODE IN CERT                                         EL1043
00208      12  FILLER                  PIC  X(04) VALUE    '13.0'.      EL1043
00209      12  FILLER                  PIC  X(06) VALUE  '@@070#'.      EL1043
00210 *****GROUPING CODE IN CERT                                        EL1043
00211      12  FILLER                  PIC  X(04) VALUE    '14.0'.      EL1043
00212      12  FILLER                  PIC  X(06) VALUE  '@@071#'.      EL1043
00213 *****ACCOUNT NUMBER IN CERT                                       EL1043
00214      12  FILLER                  PIC  X(04) VALUE    '15.0'.      EL1043
00215      12  FILLER                  PIC  X(06) VALUE  '@@072#'.      EL1043
00216 *****CERTIFICATE NUMBER                                           EL1043
00217      12  FILLER                  PIC  X(04) VALUE    '16.0'.      EL1043
00218      12  FILLER                  PIC  X(06) VALUE  '@@073#'.      EL1043
00219 *****CERT EFFECTIVE DATE                                          EL1043
00220      12  FILLER7                 PIC  X(04) VALUE    '17.0'.      EL1043
00221      12  FILLER                  PIC  X(06) VALUE  '@@074#'.      EL1043
00222 *****CERT EXPIRATION DATE (LIFE)                                  EL1043
00223      12  FILLER                  PIC  X(04) VALUE    '18.0'.      EL1043
00224      12  FILLER                  PIC  X(06) VALUE  '@@075#'.      EL1043
00225 *****CERT EXPIRATION DATE (AH)                                    EL1043
00226      12  FILLER                  PIC  X(04) VALUE    '18.5'.      EL1043
00227      12  FILLER                  PIC  X(06) VALUE  '@@076#'.      EL1043
00228 *****LIFE TERM                                                    EL1043
00229      12  FILLER                  PIC  X(04) VALUE    '19.0'.      EL1043
00230      12  FILLER                  PIC  X(06) VALUE  '@@077#'.      EL1043
00231 *****AH  TERM                                                     EL1043
00232      12  FILLER                  PIC  X(04) VALUE    '19.5'.      EL1043
00233      12  FILLER                  PIC  X(06) VALUE  '@@078#'.      EL1043
00234 *****LIFE COVERAGE AMOUNT                                         EL1043
00235      12  FILLER                  PIC  X(04) VALUE    '20.0'.      EL1043
00236      12  FILLER                  PIC  X(06) VALUE  '@@079#'.      EL1043
00237 *****AH MONTHLY BENEFIT                                           EL1043
00238      12  FILLER                  PIC  X(04) VALUE    '20.5'.      EL1043
00239      12  FILLER                  PIC  X(06) VALUE  '@@080#'.      EL1043
00240 *****LIFE CANCEL DATE                                             EL1043
00241      12  FILLER                  PIC  X(04) VALUE    '21.0'.      EL1043
00242      12  FILLER                  PIC  X(06) VALUE  '@@081#'.      EL1043
00243 *****AH CANCEL DATE                                               EL1043
00244      12  FILLER                  PIC  X(04) VALUE    '21.5'.      EL1043
00245      12  FILLER                  PIC  X(06) VALUE  '@@082#'.      EL1043
00246 *****LIFE COVERAGE FORM NUMBER                                    EL1043
00247      12  FILLER                  PIC  X(04) VALUE    '22.0'.      EL1043
00248      12  FILLER                  PIC  X(06) VALUE  '@@083#'.      EL1043
00249 *****AH COVERAGE FORM NUMBER                                      EL1043
00250      12  FILLER                  PIC  X(04) VALUE    '22.5'.      EL1043
00251      12  FILLER                  PIC  X(06) VALUE  '@@083#'.      EL1043
00252 *****INSUREDS AGE AT POLICY ISSUE (NOT USED)                      EL1043
00253      12  FILLER                  PIC  X(04) VALUE    '23.0'.      EL1043
00254      12  FILLER                  PIC  X(06) VALUE  '@@085#'.      EL1043
00255 *****LOAN NUMBER                                                  EL1043
00256      12  FILLER                  PIC  X(04) VALUE    '24.0'.      EL1043
00257      12  FILLER                  PIC  X(06) VALUE  '@@086#'.      EL1043
00258 *****LOAN BALANCE                                                 EL1043
00259      12  FILLER                  PIC  X(04) VALUE    '25.0'.      EL1043
00260      12  FILLER                  PIC  X(06) VALUE  '@@087#'.      EL1043
00261 *****MEMBER NUMBER                                                EL1043
00262      12  FILLER                  PIC  X(04) VALUE    '26.0'.      EL1043
00263      12  FILLER                  PIC  X(06) VALUE  '@@088#'.      EL1043
00264 *****INSURED SOC SEC NUMBER                                       EL1043
00265      12  FILLER                  PIC  X(04) VALUE    '29.0'.      EL1043
00266      12  FILLER                  PIC  X(06) VALUE  '@@089#'.      EL1043
00267 *****INSURED INITIALS & LAST NAME (CERTIFICATE)                   EL1043
00268      12  FILLER                  PIC  X(04) VALUE    '30.0'.      EL1043
00269      12  FILLER                  PIC  X(06) VALUE  '@@090#'.      EL1043
00270 *****INSURED FIRST NAME (CERTIFICATE)                             EL1043
00271      12  FILLER                  PIC  X(04) VALUE    '30.1'.      EL1043
00272      12  FILLER                  PIC  X(06) VALUE  '@@091#'.      EL1043
00273 *****INSURED MIDDLE INITIAL (CERTIFICATE)                         EL1043
00274      12  FILLER                  PIC  X(04) VALUE    '30.2'.      EL1043
00275      12  FILLER                  PIC  X(06) VALUE  '@@092#'.      EL1043
00276 *****ORIG TERM * MON BEN                                          EL1043
00277      12  FILLER                  PIC  X(04) VALUE    '50.0'.      EL1043
00278      12  FILLER                  PIC  X(06) VALUE  '@@093#'.      EL1043
00279 *****INSURED'S NAME (LAST, FIRST, INIT)                           EL1043
00280      12  FILLER                  PIC  X(04) VALUE    '51.0'.      EL1043
00281      12  FILLER                  PIC  X(06) VALUE  '@@094#'.      EL1043
00282 *****INSURED'S NAME (FIRST, INIT, LAST)                           EL1043
00283      12  FILLER                  PIC  X(04) VALUE    '52.0'.      EL1043
00284      12  FILLER                  PIC  X(06) VALUE  '@@095#'.      EL1043
00285 *****TITLE (MR/MS)                                                EL1043
00286      12  FILLER                  PIC  X(04) VALUE    '53.0'.      EL1043
00287      12  FILLER                  PIC  X(06) VALUE  '@@096#'.      EL1043
00288 *****LIFE PREMIUM (CERTIFICATE)                                   EL1043
00289      12  FILLER                  PIC  X(04) VALUE    '55.0'.      EL1043
00290      12  FILLER                  PIC  X(06) VALUE  '@@097#'.      EL1043
00291 *****A/H PREMIUM (CERTIFICATE)                                    EL1043
00292      12  FILLER                  PIC  X(04) VALUE    '56.0'.      EL1043
00293      12  FILLER                  PIC  X(06) VALUE  '@@098#'.      EL1043
00294 ************** PENDING VARIABLES - ERPNDB *********************   EL1043
00295 *****INSURED DATE OF BIRTH                                        EL1043
00296      12  SS28                    PIC  X(04) VALUE    '28.0'.      EL1043
00297      12  FILLER                  PIC  X(06) VALUE  '@@110#'.      EL1043
00298 *****ENTERED LIFE PREMIUM (PENDING)                               EL1043
00299      12  FILLER                  PIC  X(04) VALUE    '31.0'.      EL1043
00300      12  FILLER                  PIC  X(06) VALUE  '@@111#'.      EL1043
00301 *****ENTERED A/H PREMIUM (PENDING)                                EL1043
00302      12  FILLER                  PIC  X(04) VALUE    '32.0'.      EL1043
00303      12  FILLER                  PIC  X(06) VALUE  '@@112#'.      EL1043
00304 *****CALCULATED LIFE PREMIUM (PENDING)                            EL1043
00305      12  FILLER                  PIC  X(04) VALUE    '33.0'.      EL1043
00306      12  FILLER                  PIC  X(06) VALUE  '@@113#'.      EL1043
00307 *****CALCULATED A/H PREMIUM (PENDING)                             EL1043
00308      12  FILLER                  PIC  X(04) VALUE    '34.0'.      EL1043
00309      12  FILLER                  PIC  X(06) VALUE  '@@114#'.      EL1043
00310 *****DIFFERENCE ENTER/COMPUTED LIFE PREMIUM (PENDING)             EL1043
00311      12  FILLER                  PIC  X(04) VALUE    '35.0'.      EL1043
00312      12  FILLER                  PIC  X(06) VALUE  '@@115#'.      EL1043
00313 *****DIFFERENCE ENTER/COMPUTED A/H PREMIUM (PENDING)              EL1043
00314      12  FILLER                  PIC  X(04) VALUE    '36.0'.      EL1043
00315      12  FILLER                  PIC  X(06) VALUE  '@@116#'.      EL1043
00316 *****PRIOR CANCEL DATE                                            EL1043
00317      12  FILLER                  PIC  X(04) VALUE    '37.0'.      EL1043
00318      12  FILLER                  PIC  X(06) VALUE  '@@117#'.      EL1043
00319 *****ENTERED LIFE REFUND (PENDING)                                EL1043
00320      12  FILLER                  PIC  X(04) VALUE    '38.0'.      EL1043
00321      12  FILLER                  PIC  X(06) VALUE  '@@118#'.      EL1043
00322 *****ENTERED A/H REFUND (PENDING)                                 EL1043
00323      12  FILLER                  PIC  X(04) VALUE    '39.0'.      EL1043
00324      12  FILLER                  PIC  X(06) VALUE  '@@119#'.      EL1043
00325 *****CALCULATED LIFE REFUND (PENDING)                             EL1043
00326      12  FILLER                  PIC  X(04) VALUE    '40.0'.      EL1043
00327      12  FILLER                  PIC  X(06) VALUE  '@@120#'.      EL1043
00328 *****CALCULATED A/H REFUND (PENDING)                              EL1043
00329      12  FILLER                  PIC  X(04) VALUE    '41.0'.      EL1043
00330      12  FILLER                  PIC  X(06) VALUE  '@@121#'.      EL1043
00331 *****DIFFERENCE ENTER/COMPUTED LIFE REFUND (PENDING)              EL1043
00332      12  FILLER                  PIC  X(04) VALUE    '42.0'.      EL1043
00333      12  FILLER                  PIC  X(06) VALUE  '@@122#'.      EL1043
00334 *****DIFFERENCE ENTER/COMPUTED A/H REFUND (PENDING)               EL1043
00335      12  FILLER                  PIC  X(04) VALUE    '43.0'.      EL1043
00336      12  FILLER                  PIC  X(06) VALUE  '@@123#'.      EL1043
00337 *****INSUREDS AGE                                                 EL1043
00338      12  FILLER                  PIC  X(04) VALUE    '44.0'.      EL1043
00339      12  FILLER                  PIC  X(06) VALUE  '@@124#'.      EL1043
00340 *****LIFE BENEFIT (PENDING)                                       EL1043
00341      12  FILLER                  PIC  X(04) VALUE    '45.0'.      EL1043
00342      12  FILLER                  PIC  X(06) VALUE  '@@125#'.      EL1043
00343 *****A/H BENEFIT (PENDING)                                        EL1043
00344      12  FILLER                  PIC  X(04) VALUE    '46.0'.      EL1043
00345      12  FILLER                  PIC  X(06) VALUE  '@@126#'.      EL1043
00346 *****LIFE RATE                                                    EL1043
00347      12  FILLER                  PIC  X(04) VALUE    '47.0'.      EL1043
00348      12  FILLER                  PIC  X(06) VALUE  '@@127#'.      EL1043
00349 *****A/H RATE                                                     EL1043
00350      12  FILLER                  PIC  X(04) VALUE    '48.0'.      EL1043
00351      12  FILLER                  PIC  X(06) VALUE  '@@128#'.      EL1043
00352 *****TERM (PENDING)                                               EL1043
00353      12  FILLER                  PIC  X(04) VALUE    '49.0'.      EL1043
00354      12  FILLER                  PIC  X(06) VALUE  '@@129#'.      EL1043
00355 *****TITLE BATCH NUMBER                                           EL1043
00356      12  FILLER                  PIC  X(04) VALUE    '54.0'.      EL1043
00357      12  FILLER                  PIC  X(06) VALUE  '@@130#'.      EL1043
00358 ************** COMPENSATION VARIABLES - ERCOMP ****************   EL1043
00359 *****COMPENSATION ACCT NAME                                       EL1043
00360      12  FILLER                  PIC  X(04) VALUE    '60.0'.      EL1043
00361      12  FILLER                  PIC  X(06) VALUE  '@@140#'.      EL1043
00362 *****FULL COMPENSATION ADDRESS                                    EL1043
00363      12  FILLER                  PIC  X(04) VALUE    '60.1'.      EL1043
00364      12  FILLER                  PIC  X(06) VALUE  '@@141#'.      EL1043
00365      12  FILLER                  PIC  X(04) VALUE    '60.2'.      EL1043
00366      12  FILLER                  PIC  X(06) VALUE  '@@142#'.      EL1043
00367      12  FILLER                  PIC  X(04) VALUE    '60.3'.      EL1043
00368      12  FILLER                  PIC  X(06) VALUE  '@@143#'.      EL1043
00369      12  FILLER                  PIC  X(04) VALUE    '60.4'.      EL1043
00370      12  FILLER                  PIC  X(06) VALUE  '@@144#'.      EL1043
00371      12  FILLER                  PIC  X(04) VALUE    '60.5'.      EL1043
00372      12  FILLER                  PIC  X(06) VALUE  '@@145#'.      EL1043
00373 *****COMPENSATION PHONE NUMBER                                    EL1043
00374      12  FILLER                  PIC  X(04) VALUE    '60.6'.      EL1043
00375      12  FILLER                  PIC  X(06) VALUE  '@@147#'.      EL1043
00376 ******************  PROCESSOR DATA - ELCNTL (2) ****************  EL1043
00377 *****EXECUTING PROCESSOR NAME                                     EL1043
00378      12  FILLER                  PIC  X(04) VALUE    '08.0'.      EL1043
00379      12  FILLER                  PIC  X(06) VALUE  '@@151#'.      EL1043
00380 *****PROCESSOR TITLE                                              EL1043
00381      12  FILLER                  PIC  X(04) VALUE    '09.0'.      EL1043
00382      12  FILLER                  PIC  X(06) VALUE  '@@152#'.      EL1043
00383  01  FILLER REDEFINES W-SUPPORTED-VARIABLES.                      EL1043
00384      12  W-VAR-HOLD-GRP OCCURS 90 TIMES                              CL**2
00385                         INDEXED BY W-VG-NDX.                      EL1043
00386          16  W-VAR-OLD           PIC  X(04).                      EL1043
00387          16  W-VAR-NEW.                                           EL1043
00388              20  W-VC-CHAR OCCURS 6 TIMES                         EL1043
00389                            INDEXED BY W-VC-NDX                    EL1043
00390                                  PIC  X(01).                      EL1043
00391                                                                   EL1043
00392  01  W-VAR-END                   PIC  X(23)                       EL1043
00393                         VALUE ':VARIABLE WORK AREA END'.          EL1043
00394                                                                   EL1043
00395  01  W-PROGRAM-TABLE-AREA.                                        EL1043
00396      12  FILLER                  PIC  X(15)                       EL1043
00397                                  VALUE 'PROGRAM TABLES:'.            CL**3
00398                                                                   EL1043
00399      12  W-TX-TABLE.                                              EL1043
00400          16  W-TX-GRP OCCURS 300 TIMES                            EL1043
00401                        INDEXED BY W-TG-NDX.                       EL1043
00402              20  FILLER              PIC  X(02).                  EL1043
00403              20  W-TX-KEY.                                        EL1043
00404                  24  FILLER          PIC  X(01).                  EL1043
00405                  24  W-TX-LETTER-NO  PIC  X(04).                  EL1043
00406                  24  FILLER          PIC  X(08).                  EL1043
00407                  24  W-TX-SEQ-NO     PIC S9(04) COMP.             EL1043
00408              20  FILLER              PIC  X(02).                  EL1043
00409                                                                   EL1043
00410              20  W-TEXT-LINE.                                     EL1043
00411                  24  W-TX-CHAR OCCURS 70 TIMES                    EL1043
00412                                INDEXED BY W-TX-NDX                EL1043
00413                                      PIC  X(01).                  EL1043
00414                                                                   EL1043
00415          16  FILLER                  PIC  X(11).                  EL1043
00416                                                                   EL1043
00417      12  W-WORK-LINE.                                             EL1043
00418          16  W-WK-CHAR OCCURS 70 TIMES                            EL1043
00419                        INDEXED BY W-WK-NDX                        EL1043
00420                                   W-WK-NDX1                       EL1043
00421                                      PIC  X(01).                  EL1043
00422                                                                   EL1043
00423      12  FILLER                  PIC  X(14)                       EL1043
00424                                  VALUE 'END OF TABLES:'.             CL**3
00425                                  EJECT                            EL1043
00426  01  ERROR-MESSAGES.                                              EL1043
00427      12  ER-0000                 PIC  X(04) VALUE '0000'.         EL1043
00428      12  ER-0004                 PIC  X(04) VALUE '0004'.         EL1043
00429      12  ER-0006                 PIC  X(04) VALUE '0006'.         EL1043
00430      12  ER-0013                 PIC  X(04) VALUE '0013'.         EL1043
00431      12  ER-0180                 PIC  X(04) VALUE '0180'.         EL1043
00432      12  ER-7391                 PIC  X(04) VALUE '7391'.         EL1043
00433                                  EJECT                            EL1043
00434 *****DFHAID                  COPY ELCAID.                         EL1043
00435      COPY ELCAID.                                                 EL1043
00436  01  FILLER    REDEFINES DFHAID.                                  EL1043
00437      12  FILLER                  PIC  X(08).                      EL1043
00438      12  PF-VALUES               PIC  X(01) OCCURS 2.             EL1043
00439                                  EJECT                            EL1043
00440      COPY ELCATTR.                                                EL1043
00441                                  EJECT                            EL1043
00442      COPY ELCNWA.                                                 EL1043
00443                                  EJECT                            EL1043
00444      COPY ELCEMIB.                                                EL1043
00445  01  EMI-SAVE-AREA               PIC  X(400).                     EL1043
00446                                  EJECT                            EL1043
00447      COPY ELCDATE.                                                EL1043
00448                                  EJECT                            EL1043
00449      COPY ELCLOGOF.                                               EL1043
00450                                  EJECT                            EL1043
00451      COPY ELCSCTM.                                                EL1043
00452                                  EJECT                            EL1043
00453      COPY ELCSCRTY.                                               EL1043
00454                                  EJECT                            EL1043
00455  LINKAGE SECTION.                                                 EL1043
00456  01  DFHCOMMAREA                 PIC  X(1024).                    EL1043
00457                                                                   EL1043
00458 *01 PARMLIST .                                                       CL**3
00459 *    02  FILLER                  PIC S9(08)  COMP.                   CL**3
00460                                                                   EL1043
00461 *    02  L-TEXT-POINTER          PIC S9(08)  COMP.                   CL**3
00462                                                                   EL1043
00463      COPY ELCTEXT.                                                EL1043
00464                                  EJECT                            EL1043
00465  PROCEDURE DIVISION.                                              EL1043
00466                                                                   EL1043
00467      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL1043
00468                                                                   EL1043
00469      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1043
00470      MOVE '5'                    TO DC-OPTION-CODE.               EL1043
00471      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.               EL1043
00472      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.                  EL1043
00473      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.              EL1043
00474                                                                   EL1043
00475      EXEC CICS HANDLE CONDITION                                   EL1043
00476          PGMIDERR (9700-PGMID-ERROR)                              EL1043
00477          ERROR    (9800-ABEND)                                    EL1043
00478      END-EXEC.                                                    EL1043
00479                                                                   EL1043
00480      MOVE 0000                   TO PI-1043-ERROR.                EL1043
00481                                                                   EL1043
00482      PERFORM 1000-CREATE-WORK-TABLE THRU 1000-EXIT.               EL1043
00483                                                                   EL1043
00484      IF  PI-1043-ERROR EQUAL 0000                                 EL1043
00485          PERFORM 3000-CREATE-NEW-TEXT THRU 3000-EXIT.             EL1043
00486                                                                   EL1043
00487      MOVE PROGRAM-INTERFACE-BLOCK                                 EL1043
00488                                  TO DFHCOMMAREA.                  EL1043
00489      EXEC CICS RETURN                                             EL1043
00490      END-EXEC.                                                    EL1043
00491                                  EJECT                            EL1043
00492  1000-CREATE-WORK-TABLE.                                          EL1043
00493                                                                   EL1043
00494      MOVE SPACES                 TO W-TX-TABLE.                   EL1043
00495      SET W-TG-NDX                TO +1                            EL1043
00496                                                                   EL1043
00497      MOVE PI-COMPANY-CD          TO W-TEXT-COMPANY-CD.            EL1043
00498      MOVE PI-OLD-NAME            TO W-TEXT-LETTER.                EL1043
00499      MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.              EL1043
00500                                                                   EL1043
00501      EXEC CICS HANDLE CONDITION                                   EL1043
00502           NOTFND     (1001-NOT-FOUND)                             EL1043
00503           ENDFILE    (1001-NOT-FOUND)                             EL1043
00504           NOTOPEN    (8050-TEXT-NOT-OPEN)                         EL1043
00505      END-EXEC.                                                    EL1043
00506                                                                   EL1043
00507      EXEC CICS STARTBR                                            EL1043
00508           DATASET    (W-TEXT-FILE-ID)                             EL1043
00509           RIDFLD     (W-TEXT-KEY)                                 EL1043
00510           GTEQ                                                    EL1043
00511      END-EXEC.                                                    EL1043
00512                                                                   EL1043
00513      EXEC CICS HANDLE CONDITION                                   EL1043
00514           ENDFILE    (1000-ENDBR)                                 EL1043
00515      END-EXEC.                                                    EL1043
00516                                                                   EL1043
00517  1000-READ-NEXT.                                                  EL1043
00518                                                                   EL1043
00519      EXEC CICS READNEXT                                           EL1043
00520           DATASET    (W-TEXT-FILE-ID)                             EL1043
00521           SET        (ADDRESS OF TEXT-FILES)                         CL**3
00522           RIDFLD     (W-TEXT-KEY)                                 EL1043
00523      END-EXEC.                                                    EL1043
00524                                                                   EL1043
00525      IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY             EL1043
00526          GO TO 1000-ENDBR.                                        EL1043
00527                                                                   EL1043
00528      MOVE 'Y'                    TO W-TEXT-BROWSED-SW.            EL1043
00529                                                                   EL1043
00530      IF  TX-FORM-CONTROL-LINE                                     EL1043
00531              OR                                                   EL1043
00532          TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'                        EL1043
00533          MOVE TEXT-FILES         TO W-TX-GRP (W-TG-NDX)           EL1043
00534          GO TO 1000-READ-NEXT.                                    EL1043
00535                                                                   EL1043
00536      MOVE TX-PROCESS-CONTROL     TO W-SKIP-LINES.                 EL1043
00537                                                                   EL1043
00538      IF  TX-TEXT-LINE GREATER THAN SPACES                         EL1043
00539          MOVE W-LAST-SKIP-LINES  TO TX-PROCESS-CONTROL            EL1043
00540          MOVE W-SKIP-LINES       TO W-LAST-SKIP-LINES             EL1043
00541          MOVE TEXT-FILES         TO W-TX-GRP (W-TG-NDX)           EL1043
00542          MOVE TX-TEXT-LINE       TO W-WORK-LINE                   EL1043
00543          SET W-TX-NDX            TO W-ZEROS                       EL1043
00544          PERFORM 2200-CHECK-FOR-VARIABLE THRU 2200-EXIT           EL1043
00545                  VARYING                                          EL1043
00546              W-WK-NDX FROM 1 BY 1                                 EL1043
00547                  UNTIL                                            EL1043
00548              W-WK-NDX GREATER THAN 70                             EL1043
00549                                                                   EL1043
00550      ELSE                                                         EL1043
00551          IF  W-SKIP-LINES EQUAL 99                                EL1043
00552              MOVE W-SKIP-LINES   TO W-LAST-SKIP-LINES             EL1043
00553              GO TO 1000-READ-NEXT                                 EL1043
00554                                                                   EL1043
00555          ELSE                                                     EL1043
00556              IF  W-SKIP-LINES GREATER THAN ZEROS                  EL1043
00557                  ADD W-SKIP-LINES                                 EL1043
00558                                  TO W-LAST-SKIP-LINES             EL1043
00559                  ADD +1          TO W-LAST-SKIP-LINES             EL1043
00560                  GO TO 1000-READ-NEXT                             EL1043
00561                                                                   EL1043
00562              ELSE                                                 EL1043
00563                  ADD +1          TO W-LAST-SKIP-LINES             EL1043
00564                  GO TO 1000-READ-NEXT.                            EL1043
00565                                                                   EL1043
00566      SET W-TG-NDX UP BY 1.                                        EL1043
00567      GO TO 1000-READ-NEXT.                                        EL1043
00568                                                                   EL1043
00569  1000-ENDBR.                                                      EL1043
00570                                                                   EL1043
00571      EXEC CICS ENDBR                                              EL1043
00572          DATASET     (W-TEXT-FILE-ID)                             EL1043
00573      END-EXEC.                                                    EL1043
00574                                                                   EL1043
00575      IF  W-TEXT-BROWSE-NOT-STARTED                                EL1043
00576          MOVE ER-0006            TO EMI-ERROR                     EL1043
00577          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL1043
00578                                                                   EL1043
00579      SET W-TOTAL-TX-LINES        TO W-TG-NDX.                     EL1043
00580      SUBTRACT +1 FROM W-TOTAL-TX-LINES.                           EL1043
00581                                                                   EL1043
00582  1000-EXIT.                                                       EL1043
00583      EXIT.                                                        EL1043
00584                                                                   EL1043
00585  1001-NOT-FOUND.                                                  EL1043
00586                                                                   EL1043
00587      MOVE ER-0006                TO EMI-ERROR.                    EL1043
00588      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1043
00589                                  EJECT                            EL1043
00590  2200-CHECK-FOR-VARIABLE.                                         EL1043
00591                                                                   EL1043
00592      SET W-TX-NDX UP BY +1.                                       EL1043
00593                                                                   EL1043
00594      IF  W-WK-CHAR (W-WK-NDX) EQUAL '@'                           EL1043
00595          SET W-WK-NDX1           TO W-WK-NDX                      EL1043
00596          SET W-WK-NDX1 UP BY +1                                   EL1043
00597                                                                   EL1043
00598          IF  W-WK-CHAR (W-WK-NDX1) EQUAL '@'                      EL1043
00599              MOVE W-WK-CHAR (W-WK-NDX)                            EL1043
00600                  TO W-TX-CHAR (W-TG-NDX W-TX-NDX)                 EL1043
00601              SET W-WK-NDX UP BY +1                                EL1043
00602              SET W-TX-NDX UP BY +1                                EL1043
00603                                                                   EL1043
00604          ELSE                                                     EL1043
00605              PERFORM 2220-BUILD-VARIABLE-NUMBER THRU 2220-EXIT    EL1043
00606              PERFORM 2240-CONVERT-VARIABLE THRU 2240-EXIT.        EL1043
00607                                                                   EL1043
00608      MOVE W-WK-CHAR (W-WK-NDX)   TO W-TX-CHAR (W-TG-NDX W-TX-NDX).EL1043
00609                                                                   EL1043
00610  2200-EXIT.                                                       EL1043
00611      EXIT.                                                        EL1043
00612                                  EJECT                            EL1043
00613  2220-BUILD-VARIABLE-NUMBER.                                      EL1043
00614                                                                   EL1043
00615      SET W-WK-NDX UP BY +1.                                       EL1043
00616      MOVE W-WK-CHAR (W-WK-NDX)   TO W-V1.                         EL1043
00617      SET W-WK-NDX UP BY +1.                                       EL1043
00618      MOVE W-WK-CHAR (W-WK-NDX)   TO W-V2.                         EL1043
00619      SET W-WK-NDX UP BY +1.                                       EL1043
00620                                                                   EL1043
00621      IF  W-WK-CHAR (W-WK-NDX) EQUAL '.'                           EL1043
00622          MOVE W-WK-CHAR (W-WK-NDX)                                EL1043
00623                                  TO W-V3                          EL1043
00624          SET W-WK-NDX UP BY +1                                    EL1043
00625          MOVE W-WK-CHAR (W-WK-NDX)                                EL1043
00626                                  TO W-V4                          EL1043
00627          SET W-WK-NDX UP BY +1                                    EL1043
00628          MOVE +1                 TO W-EXTRA-LENGTH                EL1043
00629                                                                   EL1043
00630      ELSE                                                         EL1043
00631          MOVE +3                 TO W-EXTRA-LENGTH                EL1043
00632          MOVE '.'                TO W-V3                          EL1043
00633          MOVE ZEROS              TO W-V4.                         EL1043
00634                                                                   EL1043
00635  2220-EXIT.                                                       EL1043
00636      EXIT.                                                        EL1043
00637                                  EJECT                            EL1043
00638  2240-CONVERT-VARIABLE.                                           EL1043
00639                                                                   EL1043
00640      SET W-VG-NDX                TO +1.                           EL1043
00641      SEARCH W-VAR-HOLD-GRP                                        EL1043
00642          AT END                                                   EL1043
00643              MOVE ER-0180        TO EMI-ERROR                     EL1043
00644              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1043
00645                                                                   EL1043
00646          WHEN                                                     EL1043
00647              W-VAR-OLD (W-VG-NDX) EQUAL W-VAR-HOLD                EL1043
00648              PERFORM 2260-MOVE-NEW-VAR-HOLD THRU 2260-EXIT        EL1043
00649                      VARYING                                      EL1043
00650                  W-VC-NDX FROM 1 BY 1                             EL1043
00651                      UNTIL                                        EL1043
00652                  W-VC-NDX EQUAL 7.                                EL1043
00653                                                                   EL1043
00654      PERFORM 2270-ADJUST-EXTRA-LENGTH THRU 2270-EXIT              EL1043
00655              VARYING                                              EL1043
00656          W-WK-NDX FROM W-WK-NDX BY +1                             EL1043
00657              UNTIL                                                EL1043
00658          W-EXTRA-LENGTH EQUAL ZERO.                               EL1043
00659                                                                   EL1043
00660  2240-EXIT.                                                       EL1043
00661      EXIT.                                                        EL1043
00662                                  EJECT                            EL1043
00663  2260-MOVE-NEW-VAR-HOLD.                                          EL1043
00664                                                                   EL1043
00665      MOVE W-VC-CHAR (W-VG-NDX W-VC-NDX)                           EL1043
00666                                  TO W-TX-CHAR (W-TG-NDX W-TX-NDX).EL1043
00667      SET W-TX-NDX UP BY +1.                                       EL1043
00668                                                                   EL1043
00669  2260-EXIT.                                                       EL1043
00670      EXIT.                                                        EL1043
00671                                  EJECT                            EL1043
00672  2270-ADJUST-EXTRA-LENGTH.                                        EL1043
00673                                                                   EL1043
00674      IF  W-WK-CHAR (W-WK-NDX) EQUAL SPACES                        EL1043
00675          SUBTRACT +1 FROM W-EXTRA-LENGTH                          EL1043
00676                                                                   EL1043
00677      ELSE                                                         EL1043
00678          MOVE ZEROS              TO W-EXTRA-LENGTH                EL1043
00679          SET W-WK-NDX DOWN BY +1.                                 EL1043
00680                                                                   EL1043
00681  2270-EXIT.                                                       EL1043
00682      EXIT.                                                        EL1043
00683                                  EJECT                            EL1043
00684  3000-CREATE-NEW-TEXT.                                            EL1043
00685                                                                   EL1043
00686      IF  PI-NEW-NAME EQUAL PI-OLD-NAME                            EL1043
00687          MOVE W-TX-KEY (1)       TO W-TEXT-KEY                    EL1043
00688          PERFORM 4000-DELETE-OLD-TEXT THRU 4000-EXIT.             EL1043
00689                                                                   EL1043
00690      EXEC CICS HANDLE CONDITION                                   EL1043
00691           NOTFND     (3200-NOT-FOUND)                             EL1043
00692           ENDFILE    (3200-NOT-FOUND)                             EL1043
00693           NOTOPEN    (8050-TEXT-NOT-OPEN)                         EL1043
00694           DUPREC     (3300-DUP-RECORD)                            EL1043
00695      END-EXEC.                                                    EL1043
00696                                                                   EL1043
00697      PERFORM 3100-WRITE-NEW-TEXT THRU 3100-EXIT                   EL1043
00698              VARYING                                              EL1043
00699          W-TG-NDX FROM 1 BY 1                                     EL1043
00700              UNTIL                                                EL1043
00701          W-TG-NDX GREATER THAN W-TOTAL-TX-LINES.                  EL1043
00702                                                                   EL1043
00703  3000-EXIT.                                                       EL1043
00704      EXIT.                                                        EL1043
00705                                  EJECT                            EL1043
00706  3100-WRITE-NEW-TEXT.                                             EL1043
00707                                                                   EL1043
00708      MOVE PI-NEW-NAME            TO W-TX-LETTER-NO (W-TG-NDX).    EL1043
00709      MOVE W-TX-GRP (W-TG-NDX)    TO W-TEXT-RECORD.                EL1043
00710      SET W-TR-SEQ-NO             TO W-TG-NDX.                     EL1043
00711                                                                   EL1043
00712      EXEC CICS WRITE                                              EL1043
00713          DATASET (W-TEXT-FILE-ID)                                 EL1043
00714          FROM    (W-TEXT-RECORD)                                  EL1043
00715          RIDFLD  (W-TR-KEY)                                       EL1043
00716      END-EXEC.                                                    EL1043
00717                                                                   EL1043
00718  3100-EXIT.                                                       EL1043
00719      EXIT.                                                        EL1043
00720                                                                   EL1043
00721  3200-NOT-FOUND.                                                  EL1043
00722                                                                   EL1043
00723      MOVE ER-0006                TO EMI-ERROR.                    EL1043
00724      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1043
00725                                                                   EL1043
00726  3200-EXIT.                                                       EL1043
00727      EXIT.                                                        EL1043
00728                                                                   EL1043
00729  3300-DUP-RECORD.                                                 EL1043
00730                                                                   EL1043
00731      MOVE ER-7391                TO EMI-ERROR.                    EL1043
00732      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1043
00733                                                                   EL1043
00734  3300-EXIT.                                                       EL1043
00735      EXIT.                                                        EL1043
00736                                  EJECT                            EL1043
00737  4000-DELETE-OLD-TEXT.                                            EL1043
00738                                                                   EL1043
00739      EXEC CICS DELETE                                             EL1043
00740          DATASET (W-TEXT-FILE-ID)                                 EL1043
00741          RIDFLD  (W-TEXT-KEY)                                     EL1043
00742          KEYLENGTH (5)                                            EL1043
00743          GENERIC                                                  EL1043
00744      END-EXEC.                                                    EL1043
00745                                                                   EL1043
00746  4000-EXIT.                                                       EL1043
00747      EXIT.                                                        EL1043
00748                                                                   EL1043
00749  8050-TEXT-NOT-OPEN.                                              EL1043
00750                                                                   EL1043
00751      MOVE ER-0013                TO EMI-ERROR.                    EL1043
00752      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1043
00753                                  EJECT                            EL1043
00754  9000-RETURN-TRANS.                                               EL1043
00755                                                                   EL1043
00756                                                                   EL1043
00757      EXEC CICS RETURN                                             EL1043
00758          TRANSID  (W-TRANSACTION)                                 EL1043
00759          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1043
00760          LENGTH   (PI-COMM-LENGTH)                                EL1043
00761      END-EXEC.                                                    EL1043
00762                                                                   EL1043
00763  9000-EXIT.                                                       EL1043
00764      EXIT.                                                        EL1043
00765                                                                   EL1043
00766  9400-XCTL.                                                       EL1043
00767                                                                   EL1043
00768      EXEC CICS XCTL                                               EL1043
00769          PROGRAM  (W-CALL-PGM)                                    EL1043
00770          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1043
00771          LENGTH   (PI-COMM-LENGTH)                                EL1043
00772      END-EXEC.                                                    EL1043
00773                                                                   EL1043
00774  9400-EXIT.                                                       EL1043
00775      EXIT.                                                        EL1043
00776                                  EJECT                            EL1043
00777  9500-LINK-DATE-CONVERT.                                          EL1043
00778                                                                   EL1043
00779      IF  DC-BIN-DATE-1 EQUAL HIGH-VALUES                          EL1043
00780              AND                                                  EL1043
00781          DC-OPTION-CODE EQUAL ' '                                 EL1043
00782          MOVE '99/99/99'         TO DC-GREG-DATE-1-EDIT           EL1043
00783          GO TO 9500-EXIT.                                         EL1043
00784                                                                   EL1043
00785      EXEC CICS LINK                                               EL1043
00786          PROGRAM    ('ELDATCV')                                   EL1043
00787          COMMAREA   (DATE-CONVERSION-DATA)                        EL1043
00788          LENGTH     (DC-COMM-LENGTH)                              EL1043
00789      END-EXEC.                                                    EL1043
00790                                                                   EL1043
00791  9500-EXIT.                                                       EL1043
00792      EXIT.                                                        EL1043
00793                                                                   EL1043
00794                                  EJECT                            EL1043
00795  9700-PGMID-ERROR.                                                EL1043
00796                                                                   EL1043
00797      MOVE '9999'                 TO PI-1043-ERROR.                EL1043
00798      MOVE PROGRAM-INTERFACE-BLOCK                                 EL1043
00799                                  TO DFHCOMMAREA.                  EL1043
00800      EXEC CICS RETURN                                             EL1043
00801      END-EXEC.                                                    EL1043
00802                                                                   EL1043
00803  9700-EXIT.                                                       EL1043
00804      EXIT.                                                        EL1043
00805                                                                   EL1043
00806  9800-ABEND.                                                      EL1043
00807                                                                   EL1043
00808      MOVE '9999'                 TO PI-1043-ERROR.                EL1043
00809      MOVE PROGRAM-INTERFACE-BLOCK                                 EL1043
00810                                  TO DFHCOMMAREA.                  EL1043
00811      EXEC CICS RETURN                                             EL1043
00812      END-EXEC.                                                    EL1043
00813                                                                   EL1043
00814  9800-EXIT.                                                       EL1043
00815      EXIT.                                                        EL1043
00816                                  EJECT                            EL1043
00817  9900-ERROR-FORMAT.                                               EL1043
00818                                                                   EL1043
00819      MOVE EMI-ERROR              TO PI-1043-ERROR                 EL1043
00820                                                                   EL1043
00821      MOVE PROGRAM-INTERFACE-BLOCK                                 EL1043
00822                                  TO DFHCOMMAREA.                  EL1043
00823      EXEC CICS RETURN                                             EL1043
00824      END-EXEC.                                                    EL1043
00825                                                                   EL1043
00826  9900-EXIT.                                                       EL1043
00827      EXIT.                                                        EL1043
00828                                                                   EL1043
00829  9999-GOBACK.                                                     EL1043
00830                                                                   EL1043
00831      GOBACK.                                                      EL1043
00832                                                                   EL1043
00833  9999-EXIT.                                                       EL1043
00834      EXIT.                                                        EL1043
