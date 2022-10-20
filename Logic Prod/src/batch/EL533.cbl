00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL533
00003  PROGRAM-ID.                 EL533 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL533
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL533
00006 *              CONVERSION DATE 04/10/96 10:18:13.                 EL533
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          EL533
00008 *                        VMOD=2.006.                              EL533
00009                                                                   EL533
00010 *AUTHOR.        LOGIC, INC.                                       EL533
00011 *               DALLAS, TEXAS.                                    EL533
00012                                                                   EL533
00013 *DATE-COMPILED.                                                   EL533
00014                                                                   EL533
00015 *SECURITY.   *****************************************************EL533
00016 *            *                                                   *EL533
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL533
00018 *            *                                                   *EL533
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL533
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL533
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL533
00022 *            *                                                   *EL533
00023 *            *****************************************************EL533
00024                                                                   EL533
00025 *REMARKS.                                                         EL533
00026 *        THIS PROGRAM WILL BUILD A RECORD FOR EVERY ACCOUNT       EL533
00027 *        MASTER, COMPENSATION MASTER AND REINSURANCE COMPANY      EL533
00028 *        RECORD.                                                  EL533
00029 *                                                                 EL533
012407******************************************************************
012407*                   C H A N G E   L O G
012407*
012407* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
012407*-----------------------------------------------------------------
012407*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
012407* EFFECTIVE    NUMBER
012407*-----------------------------------------------------------------
012407* 110706  CR2006071700002  PEMA  FIX NAME LOOK UP
071409* 071409                   AJRA  FIX LOOP WHEN BLANK NAME
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
012407******************************************************************

00031  ENVIRONMENT DIVISION.                                            EL533
00032  INPUT-OUTPUT SECTION.                                            EL533
00033  FILE-CONTROL.                                                    EL533
00034                                                                   EL533
00035      SELECT SORT-FILE    ASSIGN TO SYS001-UT-2314-S-SORTWK1.      EL533
00036                                                                   EL533
00037      SELECT ERACCT       ASSIGN TO SYS011-FBA1-ERACCT             EL533
00038                          ORGANIZATION IS INDEXED                  EL533
00039                          ACCESS IS DYNAMIC                        EL533
00040                          RECORD KEY IS AM-CONTROL-PRIMARY         EL533
00041                          FILE STATUS IS ERACCT-FILE-STATUS.       EL533
00042                                                                   EL533
00043      SELECT ERCOMP       ASSIGN TO SYS012-FBA1-ERCOMP             EL533
00044                          ORGANIZATION IS INDEXED                  EL533
00045                          ACCESS IS DYNAMIC                        EL533
00046                          RECORD KEY IS CO-CONTROL-PRIMARY         EL533
00047                          FILE STATUS IS ERCOMP-FILE-STATUS.       EL533
00048                                                                   EL533
00049      SELECT ERREIN       ASSIGN TO SYS013-FBA1-ERREIN             EL533
00050                          ORGANIZATION IS INDEXED                  EL533
00051                          ACCESS IS DYNAMIC                        EL533
00052                          RECORD KEY IS RE-CONTROL-PRIMARY         EL533
00053                          FILE STATUS IS ERREIN-FILE-STATUS.       EL533
00054                                                                   EL533
00055      SELECT ERNAME       ASSIGN TO SYS014-FBA1-ERNAME             EL533
00056                          ORGANIZATION IS INDEXED                  EL533
00057                          ACCESS IS DYNAMIC                        EL533
00058                          RECORD KEY IS NL-RECORD-KEY              EL533
00059                          FILE STATUS IS ERNAME-FILE-STATUS.       EL533
00060                                                                   EL533
00061      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       EL533
00062                                                                   EL533
00063  EJECT                                                            EL533
00064  DATA DIVISION.                                                   EL533
00065  FILE SECTION.                                                    EL533
00066                                                                   EL533
00067  SD  SORT-FILE.                                                   EL533
00068                                                                   EL533
00069  01  SORT-REC.                                                    EL533
00070      12  SORT-PARM.                                               EL533
00071          16  SR-COMPANY-CD       PIC  X(01).                      EL533
00072          16  SR-NAME             PIC  X(30).                      EL533
00073          16  SR-RECORD-TYPE      PIC  X(01).                      EL533
00074          16  SR-CITY             PIC  X(15).                      EL533
00075          16  SR-ST               PIC  XX.                         EL533
00076      12  SORT-PARM-AM.                                            EL533
00077          16  SR-AM-CARRIER       PIC  X(01).                      EL533
00078          16  SR-AM-GROUPING      PIC  X(06).                      EL533
00079          16  SR-AM-STATE         PIC  X(02).                      EL533
00080          16  SR-AM-ACCOUNT       PIC  X(10).                      EL533
00081          16  FILLER              PIC  X(09).                      EL533
00082      12  SORT-PARM-CO  REDEFINES  SORT-PARM-AM.                   EL533
00083          16  SR-CO-CARRIER       PIC  X(01).                      EL533
00084          16  SR-CO-GROUPING      PIC  X(06).                      EL533
00085          16  SR-CO-RESP-NO       PIC  X(10).                      EL533
00086          16  SR-CO-ACCOUNT       PIC  X(10).                      EL533
00087          16  SR-CO-TYPE          PIC  X(01).                      EL533
00088      12  SORT-PARM-RE  REDEFINES  SORT-PARM-AM.                   EL533
00089          16  SR-RE-CODE          PIC  X(01).                      EL533
00090          16  SR-RE-COMPANY       PIC  X(06).                      EL533
00091          16  SR-RE-TABLE         PIC  X(03).                      EL533
00092          16  SR-RE-LEVEL         PIC  9(02).                      EL533
00093          16  FILLER              PIC  X(16).                      EL533
00094  EJECT                                                            EL533
00095  FD  ERACCT.                                                      EL533
00096                                                                   EL533
00097                                  COPY ERCACCT.                    EL533
00098  EJECT                                                            EL533
00099  FD  ERCOMP.                                                      EL533
00100                                                                   EL533
00101                                  COPY ERCCOMP.                    EL533
00102  EJECT                                                            EL533
00103  FD  ERREIN.                                                      EL533
00104                                                                   EL533
00105                                  COPY ERCREIN.                    EL533
00106  EJECT                                                            EL533
00107  FD  ERNAME.                                                      EL533
00108                                                                   EL533
00109                                  COPY ERCNAME.                    EL533
00110  EJECT                                                            EL533
00111  FD  DISK-DATE                                                    EL533
00112                                  COPY ELCDTEFD.                   EL533
00113  EJECT                                                            EL533
00114  WORKING-STORAGE SECTION.                                         EL533
00115  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL533
00116  77  FILLER  PIC  X(32) VALUE '********************************'. EL533
00117  77  FILLER  PIC  X(32) VALUE '*   EL533  WORKING STORAGE     *'. EL533
00118  77  FILLER  PIC  X(32) VALUE '******** VMOD=2.006 ************'. EL533
00119                                                                   EL533
00120  77  N1                          PIC S9(04)  COMP    VALUE +0.    EL533
00121  77  N2                          PIC S9(04)  COMP    VALUE +0.    EL533
00122  77  DEL-CTR                     PIC S9(07)  COMP-3  VALUE +0.    EL533
00123  77  ACT-CTR                     PIC S9(07)  COMP-3  VALUE +0.    EL533
00124  77  CMP-CTR                     PIC S9(07)  COMP-3  VALUE +0.    EL533
00125  77  REI-CTR                     PIC S9(07)  COMP-3  VALUE +0.    EL533
00126  77  REC-CTR                     PIC S9(07)  COMP-3  VALUE +0.    EL533
00127  77  LAST-COMPANY                PIC  X(06).                      EL533
00128  77  SAVE-KEY                    PIC  X(08).                      EL533
00129  77  LAST-COMPANY-NAME           PIC  X(30).                      EL533
00130                                                                   EL533
00131  01  WS-AREA.                                                     EL533
00132      12  WS-RETURN-CODE          PIC S9(04) COMP.                 EL533
00133      12  NDX                     PIC S9(04) COMP.                 EL533
00134      12  WS-ZERO                 PIC S9(01) COMP-3   VALUE +0.    EL533
00135      12  PGM-SUB                 PIC S9(03) COMP-3   VALUE +533.  EL533
00136      12  WS-HEX-64               PIC S9(04) COMP VALUE +100.      EL533
00137      12  FILLER  REDEFINES  WS-HEX-64.                            EL533
00138          16  FILLER              PIC  X.                          EL533
00139          16  WS-LITTLE-D         PIC  X.                          EL533
00140      12  WS-EXPIRE-DT.                                            EL533
00141          16  FILLER              PIC  X(02).                      EL533
00142          16  WS-EXP-FILLER       PIC  X(04).                      EL533
00143      12  WS-ABEND-MESSAGE        PIC  X(80).                      EL533
00144      12  WS-ABEND-FILE-STATUS    PIC  X(02)      VALUE ZEROS.     EL533
00145      12  ABEND-CODE              PIC  X(04)      VALUE ZEROS.     EL533
00146      12  ABEND-OPTION            PIC  X(01)      VALUE 'Y'.       EL533
00147      12  ERACCT-FILE-STATUS.                                      EL533
00148          16  ERACCT-STAT-1       PIC  X(01)      VALUE ZERO.      EL533
00149          16  ERACCT-STAT-2       PIC  X(01)      VALUE ZERO.      EL533
00150      12  ERCOMP-FILE-STATUS.                                      EL533
00151          16  ERCOMP-STAT-1       PIC  X(01)      VALUE ZERO.      EL533
00152          16  ERCOMP-STAT-2       PIC  X(01)      VALUE ZERO.      EL533
00153      12  ERREIN-FILE-STATUS.                                      EL533
00154          16  ERREIN-STAT-1       PIC  X(01)      VALUE ZERO.      EL533
00155          16  ERREIN-STAT-2       PIC  X(01)      VALUE ZERO.      EL533
00156      12  ERNAME-FILE-STATUS.                                      EL533
00157          16  ERNAME-STAT-1       PIC  X(01)      VALUE ZERO.      EL533
00158          16  ERNAME-STAT-2       PIC  X(01)      VALUE ZERO.      EL533
00159  EJECT                                                            EL533
00160  01  WS-SORT-REC.                                                 EL533
00161      12  WSR-SORT-PARM.                                           EL533
00162          16  WSR-COMPANY-CD      PIC  X(01).                      EL533
00163          16  WSR-NAME            PIC  X(30).                      EL533
00164          16  WSR-RECORD-TYPE     PIC  X(01).                      EL533
00165          16  WSR-CITY            PIC  X(15).                      EL533
00166          16  WSR-ST              PIC  XX.                         EL533
00167      12  WSR-SORT-PARM-AM.                                        EL533
00168          16  WSR-AM-CARRIER      PIC  X(01).                      EL533
00169          16  WSR-AM-GROUPING     PIC  X(06).                      EL533
00170          16  WSR-AM-STATE        PIC  X(02).                      EL533
00171          16  WSR-AM-ACCOUNT      PIC  X(10).                      EL533
00172          16  FILLER              PIC  X(09).                      EL533
00173      12  WSR-SORT-PARM-CO  REDEFINES  WSR-SORT-PARM-AM.           EL533
00174          16  WSR-CO-CARRIER      PIC  X(01).                      EL533
00175          16  WSR-CO-GROUPING     PIC  X(06).                      EL533
00176          16  WSR-CO-RESP-NO      PIC  X(10).                      EL533
00177          16  WSR-CO-ACCOUNT      PIC  X(10).                      EL533
00178          16  WSR-CO-TYPE         PIC  X(01).                      EL533
00179      12  WSR-SORT-PARM-RE  REDEFINES  WSR-SORT-PARM-AM.           EL533
00180          16  WSR-SUB-SORT-PARM.                                   EL533
00181              20  WSR-RE-CODE     PIC  X(01).                      EL533
00182              20  WSR-RE-COMPANY  PIC  X(06).                      EL533
00183              20  WSR-RE-TABLE    PIC  X(03).                      EL533
00184          16  WSR-RE-LEVEL        PIC  9(02).                      EL533
00185          16  FILLER              PIC  X(16).                      EL533
00186                                                                   EL533
00187  01  LAST-REC.                                                    EL533
00188      12  LAST-PARM.                                               EL533
00189          16  LR-COMPANY-CD       PIC  X(01).                      EL533
00190          16  LR-NAME             PIC  X(30).                      EL533
00191          16  LR-RECORD-TYPE      PIC  X(01).                      EL533
00192          16  LR-CITY             PIC  X(15).                      EL533
00193          16  LR-ST               PIC  XX.                         EL533
00194      12  LAST-PARM-AM.                                            EL533
00195          16  LR-AM-CARRIER       PIC  X(01).                      EL533
00196          16  LR-AM-GROUPING      PIC  X(06).                      EL533
00197          16  LR-AM-STATE         PIC  X(02).                      EL533
00198          16  LR-AM-ACCOUNT       PIC  X(10).                      EL533
00199          16  FILLER              PIC  X(09).                      EL533
00200      12  LAST-PARM-CO  REDEFINES  LAST-PARM-AM.                   EL533
00201          16  LR-CO-CARRIER       PIC  X(01).                      EL533
00202          16  LR-CO-GROUPING      PIC  X(06).                      EL533
00203          16  LR-CO-RESP-NO       PIC  X(10).                      EL533
00204          16  LR-CO-ACCOUNT       PIC  X(10).                      EL533
00205          16  LR-CO-TYPE          PIC  X(01).                      EL533
00206      12  LAST-PARM-RE  REDEFINES  LAST-PARM-AM.                   EL533
00207          16  LR-SUB-SORT-PARM.                                    EL533
00208              20  LR-RE-CODE      PIC  X(01).                      EL533
00209              20  LR-RE-COMPANY   PIC  X(06).                      EL533
00210              20  LR-RE-TABLE     PIC  X(03).                      EL533
00211          16  LR-RE-LEVEL         PIC  9(02).                      EL533
00212          16  FILLER              PIC  X(16).                      EL533
00213  EJECT                                                            EL533
00214  01  NAME-WORK-AREA.                                              EL533
00215      12  OLD-NAME                PIC  X(30).                      EL533
00216      12  OLD-NAME-CHAR  REDEFINES  OLD-NAME                       EL533
00217                                  PIC  X(01)  OCCURS  30  TIMES.   EL533
00218      12  NEW-NAME                PIC  X(30).                      EL533
00219      12  NEW-NAME-CHAR  REDEFINES  NEW-NAME                       EL533
00220                                  PIC  X(01)  OCCURS  30  TIMES.   EL533
00221                                                                   EL533
00222 ******************************************************************EL533
00223 *                                                                *EL533
00224 *   THIS AREA IS TAKEN FROM COPYBOOK ERCREIN - THEREFORE ANY     *EL533
00225 *   CHANGE TO THE LENGTH OR FIELD POSITIONING IN THAT COPYBOOK   *EL533
00226 *   WILL NEED TO BE DONE TO THIS AREA AS WELL.                   *EL533
00227 *                                                                *EL533
00228 ******************************************************************EL533
00229                                                                   EL533
00230  01  SAVE-REINSURANCE-RECORD.                                     EL533
00231      12  SV-RE-RECORD-ID                   PIC  X(02).            EL533
00232      12  SV-RE-CONTROL-PRIMARY.                                   EL533
00233          16  SV-RE-COMPANY-CD              PIC  X(01).            EL533
00234          16  SV-RE-KEY.                                           EL533
00235              20  SV-RE-CODE                PIC  X(01).            EL533
00236              20  SV-RE-TABLE               PIC  X(03).            EL533
00237              20  FILLER                    PIC  X(03).            EL533
00238          16  SV-RE-COMPANY-KEY  REDEFINES  SV-RE-KEY.             EL533
00239              20  FILLER                    PIC  X(01).            EL533
00240              20  SV-RE-COMPANY.                                   EL533
00241                  24  SV-RE-COMP-PRIME      PIC  X(03).            EL533
00242                  24  SV-RE-COMP-SUB        PIC  X(03).            EL533
00243      12  SV-RE-MAINT-INFORMATION.                                 EL533
00244          16  SV-RE-LAST-MAINT-DT           PIC  X(02).            EL533
00245          16  SV-RE-LAST-MAINT-HHMMSS       PIC S9(07)     COMP-3. EL533
00246          16  SV-RE-LAST-MAINT-USER         PIC  X(04).            EL533
00247          16  FILLER                        PIC  X(10).            EL533
00248      12  SV-RE-TABLE-DATA.                                        EL533
00249          16  SV-RE-100-COMP                PIC  9(02).            EL533
00250          16  SV-RE-COMP-INFO OCCURS 30 TIMES.                     EL533
00251              20  SV-RE-REI-COMP-NO.                               EL533
00252                  24  SV-RE-REI-COMP        PIC  X(03).            EL533
00253                  24  SV-RE-REI-COMP-SUB    PIC  X(03).            EL533
00254              20  FILLER                    PIC  X(125).           EL533
00255          16  FILLER                        PIC  X(38).            EL533
00256 ******************************************************************EL533
00257  EJECT                                                            EL533
00258                                  COPY ELCDATE.                       CL**3
00259  EJECT                                                            EL533
00260                                  COPY ELCDTECX.                   EL533
00261  EJECT                                                            EL533
00262                                  COPY ELCDTEVR.                   EL533
00263  EJECT                                                            EL533
00264  PROCEDURE DIVISION.                                              EL533
00265                                                                   EL533
00266  0000-LOAD-DATE-WS.              COPY ELCDTERX.                   EL533
00267  EJECT                                                            EL533
00268  0100-DELETE-ERNAME-FILE.                                         EL533
00269      OPEN I-O  ERNAME.                                            EL533
00270                                                                   EL533
00271      IF ERNAME-FILE-STATUS  IS EQUAL TO  ZERO  OR  '97'           EL533
00272          NEXT SENTENCE                                            EL533
00273      ELSE                                                         EL533
00274          MOVE 'ERROR OCCURED OPEN - ERNAME'                       EL533
00275                                  TO  WS-ABEND-MESSAGE             EL533
00276          MOVE ERNAME-FILE-STATUS                                  EL533
00277                                  TO  WS-ABEND-FILE-STATUS         EL533
00278          GO TO ABEND-PGM.                                         EL533
00279                                                                   EL533
00280      MOVE LOW-VALUES             TO  NL-RECORD-KEY.               EL533
00281      MOVE DTE-CLASIC-COMPANY-CD  TO  NL-COMPANY-CD.               EL533
00282                                                                   EL533
00283      START ERNAME                                                 EL533
00284          KEY NOT LESS  NL-RECORD-KEY.                             EL533
00285                                                                   EL533
00286      IF ERNAME-FILE-STATUS  IS EQUAL TO  '23'                     EL533
00287          GO TO 0130-END-ERNAME-DELETE.                            EL533
00288                                                                   EL533
00289      IF ERNAME-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00290          MOVE 'ERROR OCCURED START - ERNAME'                      EL533
00291                                  TO  WS-ABEND-MESSAGE             EL533
00292          MOVE ERNAME-FILE-STATUS                                  EL533
00293                                  TO  WS-ABEND-FILE-STATUS         EL533
00294          GO TO ABEND-PGM.                                         EL533
00295                                                                   EL533
00296  0110-READ-ERNAME.                                                EL533
00297      READ ERNAME  NEXT RECORD.                                    EL533
00298                                                                   EL533
00299      IF ERNAME-STAT-1  IS EQUAL TO  '1'                           EL533
00300          GO TO 0130-END-ERNAME-DELETE.                            EL533
00301                                                                   EL533
00302      IF ERNAME-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00303          MOVE 'ERROR OCCURED READ - ERNAME'                       EL533
00304                                  TO  WS-ABEND-MESSAGE             EL533
00305          MOVE ERNAME-FILE-STATUS                                  EL533
00306                                  TO  WS-ABEND-FILE-STATUS         EL533
00307          GO TO ABEND-PGM.                                         EL533
00308                                                                   EL533
00309      IF NL-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL533
00310          GO TO 0130-END-ERNAME-DELETE.                            EL533
00311  EJECT                                                            EL533
00312  0120-DELETE-ERNAME.                                              EL533
00313      DELETE ERNAME  RECORD.                                       EL533
00314                                                                   EL533
00315      IF ERNAME-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00316          MOVE 'ERROR OCCURED DELETE - ERNAME'                     EL533
00317                                  TO  WS-ABEND-MESSAGE             EL533
00318          MOVE ERNAME-FILE-STATUS                                  EL533
00319                                  TO  WS-ABEND-FILE-STATUS         EL533
00320          GO TO ABEND-PGM.                                         EL533
00321                                                                   EL533
00322      ADD 1                       TO  DEL-CTR.                     EL533
00323                                                                   EL533
00324      GO TO 0110-READ-ERNAME.                                      EL533
00325                                                                   EL533
00326  0130-END-ERNAME-DELETE.                                          EL533
00327      CLOSE ERNAME.                                                EL533
00328                                                                   EL533
00329      IF ERNAME-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00330          MOVE 'ERROR OCCURED CLOSE - ERNAME'                      EL533
00331                                  TO  WS-ABEND-MESSAGE             EL533
00332          MOVE ERNAME-FILE-STATUS                                  EL533
00333                                  TO  WS-ABEND-FILE-STATUS         EL533
00334          GO TO ABEND-PGM.                                         EL533
00335                                                                   EL533
00336  0140-OPEN-FILES.                                                 EL533
00337      OPEN INPUT  ERACCT                                           EL533
00338                  ERCOMP                                           EL533
00339                  ERREIN                                           EL533
00340           I-O    ERNAME.                                          EL533
00341                                                                   EL533
00342      IF ERACCT-FILE-STATUS  IS EQUAL TO  '00'  OR  '97'           EL533
00343          NEXT SENTENCE                                            EL533
00344      ELSE                                                         EL533
00345          MOVE 'ERROR OCCURED OPEN - ERACCT'                       EL533
00346                                  TO  WS-ABEND-MESSAGE             EL533
00347          MOVE ERACCT-FILE-STATUS                                  EL533
00348                                  TO  WS-ABEND-FILE-STATUS         EL533
00349          GO TO ABEND-PGM.                                         EL533
00350                                                                   EL533
00351      IF ERCOMP-FILE-STATUS  IS EQUAL TO  '00'  OR  '97'           EL533
00352          NEXT SENTENCE                                            EL533
00353      ELSE                                                         EL533
00354          MOVE 'ERROR OCCURED OPEN - ERCOMP'                       EL533
00355                                  TO  WS-ABEND-MESSAGE             EL533
00356          MOVE ERCOMP-FILE-STATUS                                  EL533
00357                                  TO  WS-ABEND-FILE-STATUS         EL533
00358          GO TO ABEND-PGM.                                         EL533
00359                                                                   EL533
00360      IF ERREIN-FILE-STATUS  IS EQUAL TO  '00'  OR  '97'           EL533
00361          NEXT SENTENCE                                            EL533
00362      ELSE                                                         EL533
00363          MOVE 'ERROR OCCURED OPEN - ERREIN'                       EL533
00364                                  TO  WS-ABEND-MESSAGE             EL533
00365          MOVE ERREIN-FILE-STATUS                                  EL533
00366                                  TO  WS-ABEND-FILE-STATUS         EL533
00367          GO TO ABEND-PGM.                                         EL533
00368                                                                   EL533
00369      IF ERNAME-FILE-STATUS  IS EQUAL TO  '00'  OR  '97'           EL533
00370          NEXT SENTENCE                                            EL533
00371      ELSE                                                         EL533
00372          MOVE 'ERROR OCCURED OPEN - ERNAME - PARA 1010-'          EL533
00373                                  TO  WS-ABEND-MESSAGE             EL533
00374          MOVE ERNAME-FILE-STATUS                                  EL533
00375                                  TO  WS-ABEND-FILE-STATUS         EL533
00376          GO TO ABEND-PGM.                                         EL533
00377  EJECT                                                            EL533
00378  0200-SORT-ROUTINE SECTION.                                       EL533
00379                                                                   EL533
00380  0210-SORT-RTN.                                                   EL533
00381      SORT SORT-FILE  ON ASCENDING KEY  SR-COMPANY-CD              EL533
00382                                        SR-NAME                    EL533
00383                                        SR-RECORD-TYPE             EL533
00384                                        SORT-PARM-AM               EL533
00385          INPUT PROCEDURE 0300-INPUT-RTN  THRU  0999-EXIT          EL533
00386          OUTPUT PROCEDURE 1000-OUTPUT-RTN  THRU  8899-EXIT.       EL533
00387                                                                   EL533
00388      IF SORT-RETURN  IS EQUAL TO  ZERO  OR  4                     EL533
00389          NEXT SENTENCE                                            EL533
00390      ELSE                                                         EL533
00391          MOVE '0101'             TO ABEND-CODE                    EL533
00392          GO TO ABEND-PGM.                                         EL533
00393                                                                   EL533
00394      GO TO 9999-END-OF-JOB.                                       EL533
00395  EJECT                                                            EL533
00396  0300-INPUT-RTN SECTION.                                          EL533
00397                                                                   EL533
00398  0310-START-ERACCT.                                               EL533
00399      MOVE LOW-VALUES             TO  AM-CONTROL-PRIMARY.          EL533
00400      MOVE DTE-CLASIC-COMPANY-CD  TO  AM-COMPANY-CD.               EL533
00401                                                                   EL533
00402      START ERACCT                                                 EL533
00403          KEY NOT LESS  AM-CONTROL-PRIMARY.                        EL533
00404                                                                   EL533
00405      IF ERACCT-FILE-STATUS  IS EQUAL TO  '23'                     EL533
00406          GO TO 0340-END-ERACCT.                                   EL533
00407                                                                   EL533
00408      IF ERACCT-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00409          MOVE 'ERROR OCCURED START - ERACCT'                      EL533
00410                                  TO  WS-ABEND-MESSAGE             EL533
00411          MOVE ERACCT-FILE-STATUS                                  EL533
00412                                  TO  WS-ABEND-FILE-STATUS         EL533
00413          GO TO ABEND-PGM.                                         EL533
00414                                                                   EL533
00415  0320-READ-ERACCT.                                                EL533
00416      READ ERACCT  NEXT RECORD.                                    EL533
00417                                                                   EL533
00418      IF ERACCT-STAT-1  IS EQUAL TO  '1'                           EL533
00419          GO TO 0340-END-ERACCT.                                   EL533
00420                                                                   EL533
00421      IF ERACCT-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00422          MOVE 'ERROR OCCURED READ - ERACCT'                       EL533
00423                                  TO  WS-ABEND-MESSAGE             EL533
00424          MOVE ERACCT-FILE-STATUS                                  EL533
00425                                  TO  WS-ABEND-FILE-STATUS         EL533
00426          GO TO ABEND-PGM.                                         EL533
00427                                                                   EL533
00428      IF AM-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL533
00429          GO TO 0340-END-ERACCT.                                   EL533
00430  EJECT                                                            EL533
00431  0330-GET-ACCT-NAME.                                              EL533
00432      MOVE SPACES                 TO  WS-SORT-REC.                 EL533
00433      MOVE AM-COMPANY-CD          TO  WSR-COMPANY-CD.              EL533
00434      MOVE AM-NAME                TO  OLD-NAME.                    EL533
00435                                                                   EL533
00436      PERFORM 0700-NAME-ADJUSTMENT  THRU  0799-EXIT.               EL533
00437                                                                   EL533
00438      MOVE NEW-NAME               TO  WSR-NAME.                    EL533
00439      MOVE 'A'                    TO  WSR-RECORD-TYPE.             EL533
00440      MOVE AM-CARRIER             TO  WSR-AM-CARRIER.              EL533
00441      MOVE AM-GROUPING            TO  WSR-AM-GROUPING.             EL533
00442      MOVE AM-STATE               TO  WSR-AM-STATE.                EL533
00443      MOVE AM-ACCOUNT             TO  WSR-AM-ACCOUNT.              EL533
051810*    MOVE AM-CITY                TO  WSR-CITY.                    EL533
051810     MOVE AM-ADDR-CITY           TO  WSR-CITY
051810     MOVE AM-STATE               TO  WSR-ST.                      EL533
00446                                                                   EL533
00447      PERFORM 0600-RELEASE-SORT-REC  THRU  0699-EXIT.              EL533
00448                                                                   EL533
00449      GO TO 0320-READ-ERACCT.                                      EL533
00450                                                                   EL533
00451  0340-END-ERACCT.                                                 EL533
00452      CLOSE  ERACCT.                                               EL533
00453  EJECT                                                            EL533
00454  0400-START-ERCOMP.                                               EL533
00455      MOVE LOW-VALUES             TO  CO-CONTROL-PRIMARY.          EL533
00456      MOVE DTE-CLASIC-COMPANY-CD  TO  CO-COMPANY-CD.               EL533
00457                                                                   EL533
00458      START ERCOMP                                                 EL533
00459          KEY NOT LESS  CO-CONTROL-PRIMARY.                        EL533
00460                                                                   EL533
00461      IF ERCOMP-FILE-STATUS  IS EQUAL TO  '23'                     EL533
00462          GO TO 0430-END-ERCOMP.                                   EL533
00463                                                                   EL533
00464      IF ERCOMP-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00465          MOVE 'ERROR OCCURED START - ERCOMP'                      EL533
00466                                  TO  WS-ABEND-MESSAGE             EL533
00467          MOVE ERCOMP-FILE-STATUS                                  EL533
00468                                  TO  WS-ABEND-FILE-STATUS         EL533
00469          GO TO ABEND-PGM.                                         EL533
00470                                                                   EL533
00471  0410-READ-ERCOMP.                                                EL533
00472      READ ERCOMP  NEXT RECORD.                                    EL533
00473                                                                   EL533
00474      IF ERCOMP-STAT-1  IS EQUAL TO  '1'                           EL533
00475          GO TO 0430-END-ERCOMP.                                   EL533
00476                                                                   EL533
00477      IF ERCOMP-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00478          MOVE 'ERROR OCCURED READ - ERCOMP'                       EL533
00479                                  TO  WS-ABEND-MESSAGE             EL533
00480          MOVE ERCOMP-FILE-STATUS                                  EL533
00481                                  TO  WS-ABEND-FILE-STATUS         EL533
00482          GO TO ABEND-PGM.                                         EL533
00483                                                                   EL533
00484      IF CO-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL533
00485          GO TO 0430-END-ERCOMP.                                   EL533
00486  EJECT                                                            EL533
00487  0420-GET-COMP-NAME.                                              EL533
00488      MOVE SPACES                 TO  WS-SORT-REC.                 EL533
00489      MOVE CO-COMPANY-CD          TO  WSR-COMPANY-CD.              EL533
00490      MOVE CO-ACCT-NAME           TO  OLD-NAME.                    EL533
00491                                                                   EL533
00492      PERFORM 0700-NAME-ADJUSTMENT  THRU  0799-EXIT.               EL533
00493                                                                   EL533
00494      MOVE NEW-NAME               TO  WSR-NAME.                    EL533
00495      MOVE 'C'                    TO  WSR-RECORD-TYPE.             EL533
00496      MOVE CO-CARRIER             TO  WSR-CO-CARRIER.              EL533
00497      MOVE CO-GROUPING            TO  WSR-CO-GROUPING.             EL533
00498      MOVE CO-RESP-NO             TO  WSR-CO-RESP-NO.              EL533
00499      MOVE CO-ACCOUNT             TO  WSR-CO-ACCOUNT.              EL533
00500      MOVE CO-TYPE                TO  WSR-CO-TYPE.                 EL533
051810*    MOVE CO-ADDR-3              TO  WSR-CITY.                    EL533
051810     MOVE CO-ADDR-CITY           TO  WSR-CITY
051810     MOVE CO-ADDR-STATE          TO  WSR-ST
00502                                                                   EL533
00503      PERFORM 0600-RELEASE-SORT-REC  THRU  0699-EXIT.              EL533
00504                                                                   EL533
00505      GO TO 0410-READ-ERCOMP.                                      EL533
00506                                                                   EL533
00507  0430-END-ERCOMP.                                                 EL533
00508      CLOSE  ERCOMP.                                               EL533
00509  EJECT                                                            EL533
00510  0500-START-ERREIN.                                               EL533
00511      MOVE LOW-VALUES             TO  LAST-COMPANY.                EL533
00512      MOVE LOW-VALUES             TO  RE-CONTROL-PRIMARY.          EL533
00513      MOVE DTE-CLASIC-COMPANY-CD  TO  RE-COMPANY-CD.               EL533
00514                                                                   EL533
00515      START ERREIN                                                 EL533
00516          KEY NOT LESS  RE-CONTROL-PRIMARY.                        EL533
00517                                                                   EL533
00518      IF ERREIN-FILE-STATUS  IS EQUAL TO  '23'                     EL533
00519          GO TO 0560-END-ERREIN.                                   EL533
00520                                                                   EL533
00521      IF ERREIN-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00522          MOVE 'ERROR OCCURED START - ERREIN'                      EL533
00523                                  TO  WS-ABEND-MESSAGE             EL533
00524          MOVE ERREIN-FILE-STATUS                                  EL533
00525                                  TO  WS-ABEND-FILE-STATUS         EL533
00526          GO TO ABEND-PGM.                                         EL533
00527                                                                   EL533
00528  0510-READ-ERREIN.                                                EL533
00529      READ ERREIN  NEXT RECORD.                                    EL533
00530                                                                   EL533
00531      IF ERREIN-STAT-1  IS EQUAL TO  '1'                           EL533
00532          GO TO 0560-END-ERREIN.                                   EL533
00533                                                                   EL533
00534      IF ERREIN-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00535          MOVE 'ERROR OCCURED READ - ERREIN'                       EL533
00536                                  TO  WS-ABEND-MESSAGE             EL533
00537          MOVE ERREIN-FILE-STATUS                                  EL533
00538                                  TO  WS-ABEND-FILE-STATUS         EL533
00539          GO TO ABEND-PGM.                                         EL533
00540                                                                   EL533
00541      IF RE-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL533
00542          GO TO 0560-END-ERREIN.                                   EL533
00543                                                                   EL533
00544      IF RE-TABLE-RECORD                                           EL533
00545          NEXT SENTENCE                                            EL533
00546      ELSE                                                         EL533
00547          GO TO 0510-READ-ERREIN.                                  EL533
00548                                                                   EL533
00549      MOVE +1                     TO  NDX.                         EL533
00550      MOVE RE-CONTROL-PRIMARY     TO  SAVE-KEY.                    EL533
00551      MOVE REINSURANCE-RECORD     TO  SAVE-REINSURANCE-RECORD.     EL533
00552  EJECT                                                            EL533
00553  0520-GET-REIN-LOOP.                                              EL533
00554      IF SV-RE-REI-COMP-NO (NDX)  IS EQUAL TO  SPACES              EL533
00555          GO TO 0550-READY-NEXT-READ.                              EL533
00556                                                                   EL533
00557      IF SV-RE-REI-COMP-NO (NDX)  IS EQUAL TO  ZEROS               EL533
00558          GO TO 0550-READY-NEXT-READ.                              EL533
00559                                                                   EL533
00560      IF (SV-RE-REI-COMP (NDX) = SPACES OR ZEROS)                     CL**2
00561                      AND                                             CL**2
00562         (SV-RE-REI-COMP-SUB (NDX) = SPACES OR ZEROS)                 CL**2
00563          GO TO 0550-READY-NEXT-READ.                                 CL**2
00564                                                                      CL**2
00565      IF SV-RE-REI-COMP-NO (NDX)  IS EQUAL TO  LAST-COMPANY        EL533
00566          GO TO 0530-GET-REIN-NAME.                                EL533
00567                                                                   EL533
00568      MOVE SV-RE-REI-COMP-NO (NDX)                                 EL533
00569                                  TO  RE-COMPANY                   EL533
00570                                      LAST-COMPANY.                EL533
00571      MOVE 'B'                    TO  RE-CODE.                     EL533
00572                                                                   EL533
00573      READ ERREIN.                                                 EL533
00574                                                                   EL533
00575      IF ERREIN-FILE-STATUS  IS EQUAL TO  '10'  OR  '23'           EL533
00576          GO TO 0540-ADD-TO-NDX.                                   EL533
00577                                                                   EL533
00578      IF ERREIN-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00579          MOVE 'ERROR OCCURED READ - ERREIN'                       EL533
00580                                  TO  WS-ABEND-MESSAGE             EL533
00581          MOVE ERREIN-FILE-STATUS                                  EL533
00582                                  TO  WS-ABEND-FILE-STATUS         EL533
00583          GO TO ABEND-PGM.                                         EL533
00584                                                                   EL533
00585      IF RE-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL533
00586          GO TO 0540-ADD-TO-NDX.                                   EL533
00587                                                                   EL533
00588      IF RE-COMPANY-RECORD                                         EL533
00589          NEXT SENTENCE                                            EL533
00590      ELSE                                                         EL533
00591          GO TO 0540-ADD-TO-NDX.                                   EL533
00592                                                                   EL533
00593      MOVE RE-NAME                TO  OLD-NAME                     EL533
00594                                                                   EL533
00595      PERFORM 0700-NAME-ADJUSTMENT  THRU  0799-EXIT.               EL533
00596                                                                   EL533
00597      MOVE NEW-NAME               TO  LAST-COMPANY-NAME.           EL533
00598  EJECT                                                            EL533
00599  0530-GET-REIN-NAME.                                              EL533
00600      MOVE SPACES                 TO  WS-SORT-REC.                 EL533
00601      MOVE RE-COMPANY-CD          TO  WSR-COMPANY-CD.              EL533
00602      MOVE LAST-COMPANY-NAME      TO  WSR-NAME.                    EL533
00603      MOVE 'R'                    TO  WSR-RECORD-TYPE.             EL533
00604      MOVE 'B'                    TO  WSR-RE-CODE.                 EL533
00605      MOVE LAST-COMPANY           TO  WSR-RE-COMPANY.              EL533
00606      MOVE SV-RE-TABLE            TO  WSR-RE-TABLE.                EL533
00607      MOVE NDX                    TO  WSR-RE-LEVEL.                EL533
00608                                                                   EL533
00609      PERFORM 0600-RELEASE-SORT-REC  THRU  0699-EXIT.              EL533
00610                                                                   EL533
00611  0540-ADD-TO-NDX.                                                 EL533
00612      ADD +1                      TO  NDX.                         EL533
00613                                                                   EL533
00614      IF NDX  IS LESS THAN  +31                                    EL533
00615          GO TO 0520-GET-REIN-LOOP.                                EL533
00616                                                                   EL533
00617  0550-READY-NEXT-READ.                                            EL533
00618      MOVE SAVE-KEY               TO  RE-CONTROL-PRIMARY.          EL533
00619                                                                   EL533
00620      START ERREIN                                                 EL533
00621          KEY NOT LESS  RE-CONTROL-PRIMARY.                        EL533
00622                                                                   EL533
00623      IF ERREIN-FILE-STATUS  IS EQUAL TO  '23'                     EL533
00624          GO TO 0560-END-ERREIN.                                   EL533
00625                                                                   EL533
00626      IF ERREIN-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00627          MOVE 'ERROR OCCURED START - ERREIN'                      EL533
00628                                  TO  WS-ABEND-MESSAGE             EL533
00629          MOVE ERREIN-FILE-STATUS                                  EL533
00630                                  TO  WS-ABEND-FILE-STATUS         EL533
00631          GO TO ABEND-PGM.                                         EL533
00632                                                                   EL533
00633      READ ERREIN  NEXT RECORD.                                    EL533
00634                                                                   EL533
00635      IF ERREIN-STAT-1  IS EQUAL TO  '1'                           EL533
00636          GO TO 0560-END-ERREIN.                                   EL533
00637                                                                   EL533
00638      IF ERREIN-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00639          MOVE 'ERROR OCCURED READ - ERREIN'                       EL533
00640                                  TO  WS-ABEND-MESSAGE             EL533
00641          MOVE ERREIN-FILE-STATUS                                  EL533
00642                                  TO  WS-ABEND-FILE-STATUS         EL533
00643          GO TO ABEND-PGM.                                         EL533
00644                                                                   EL533
00645      GO TO 0510-READ-ERREIN.                                      EL533
00646                                                                   EL533
00647  0560-END-ERREIN.                                                 EL533
00648      CLOSE  ERREIN.                                               EL533
00649                                                                   EL533
00650      GO TO 0999-EXIT.                                             EL533
00651  EJECT                                                            EL533
00652  0600-RELEASE-SORT-REC.                                           EL533
00653      IF WSR-NAME  IS EQUAL TO  SPACES                             EL533
00654          GO TO 0699-EXIT.                                         EL533
00655                                                                   EL533
00656      RELEASE SORT-REC  FROM  WS-SORT-REC.                         EL533
00657                                                                   EL533
00658  0699-EXIT.                                                       EL533
00659      EXIT.                                                        EL533
00660                                                                   EL533
00661  0700-NAME-ADJUSTMENT.                                            EL533
00662      MOVE SPACES                 TO  NEW-NAME.                    EL533
00663      MOVE +1                     TO  N1  N2.                      EL533
071409
071409    IF OLD-NAME EQUAL SPACES OR LOW-VALUES
071409        GO TO 0799-EXIT
071409    END-IF.

          IF OLD-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
             MOVE OLD-NAME (5:26)      TO OLD-NAME
          END-IF

           .
00665  0710-NAME-LOOP.                                                  EL533
00666      IF OLD-NAME-CHAR (N1) = ' ' OR '%' OR LOW-VALUES                CL**2
00667                                         OR WS-LITTLE-D               CL**2
00668          ADD +1                  TO  N1                           EL533
00669          GO TO 0710-NAME-LOOP.                                    EL533
00670                                                                   EL533
00671  0720-MOVE-NAME-LOOP.                                             EL533
00672      MOVE OLD-NAME-CHAR (N1)     TO  NEW-NAME-CHAR (N2).          EL533
00673                                                                   EL533
00674      ADD +1                      TO  N1  N2.                      EL533
00675                                                                   EL533
00676      IF N1  IS LESS THAN  +31                                     EL533
00677          GO TO 0720-MOVE-NAME-LOOP.                               EL533
00678                                                                   EL533
00679  0799-EXIT.                                                       EL533
00680      EXIT.                                                        EL533
00681                                                                   EL533
00682  0999-EXIT.                                                       EL533
00683      EXIT.                                                        EL533
00684  EJECT                                                            EL533
00685  1000-OUTPUT-RTN SECTION.                                         EL533
00686                                                                   EL533
00687  1010-OPEN-OUTPUT.                                                EL533
00688      MOVE LOW-VALUES             TO  LAST-COMPANY                 EL533
00689                                      LAST-COMPANY-NAME            EL533
00690                                      LAST-REC.                    EL533
00691      MOVE +0                     TO  NDX.                         EL533
00692                                                                   EL533
00693  1020-RETURN-SORT.                                                EL533
00694      RETURN SORT-FILE  INTO  WS-SORT-REC  AT END                  EL533
00695          GO TO 7500-END-OUTPUT.                                   EL533
00696                                                                   EL533
00697      IF WSR-RECORD-TYPE  IS EQUAL TO  'R'                         EL533
00698          GO TO 1300-SET-UP-REIN-RECORD.                           EL533
00699                                                                   EL533
00700      IF NDX  IS GREATER THAN  +0                                  EL533
00701          PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.              EL533
00702                                                                   EL533
00703      IF WSR-RECORD-TYPE  IS EQUAL TO  'C'                         EL533
00704          GO TO 1200-SET-UP-COMP-RECORD.                           EL533
00705  EJECT                                                            EL533
00706  1100-SET-UP-ACCT-RECORD.                                         EL533
00707      IF WSR-NAME  IS EQUAL TO  LR-NAME                            EL533
00708        AND WSR-SORT-PARM-AM  IS EQUAL TO  LAST-PARM-AM            EL533
00709          GO TO 1020-RETURN-SORT.                                  EL533
00710                                                                   EL533
00711      IF NDX  IS GREATER THAN  +0                                  EL533
00712          PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.              EL533
00713                                                                   EL533
00714      ADD 1                       TO  ACT-CTR.                     EL533
00715                                                                   EL533
00716      MOVE WS-SORT-REC            TO  LAST-REC.                    EL533
00717      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.          EL533
00718      MOVE 'NL'                   TO  NL-RECORD-ID.                EL533
00719      MOVE WSR-COMPANY-CD         TO  NL-COMPANY-CD.               EL533
00720      MOVE WSR-NAME               TO  NL-NAME.                     EL533
00721      MOVE WSR-CITY               TO  NL-CITY.                     EL533
00722      MOVE WSR-ST                 TO  NL-ST.                       EL533
00723      MOVE 'A'                    TO  NL-RECORD-TYPE.              EL533
00724      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL533
00725      MOVE '2'                    TO  DC-OPTION-CODE.              EL533
00726                                                                   EL533
00727      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL533
00728                                                                   EL533
00729      MOVE DC-BIN-DATE-1          TO  NL-LAST-MAINT-DT.            EL533
00730      MOVE WS-TIME                TO  NL-LAST-MAINT-HHMMSS.        EL533
00731      MOVE DTE-CLIENT             TO  NL-LAST-MAINT-USER.          EL533
00732      MOVE WSR-COMPANY-CD         TO  NL-AM-COMPANY-CD.            EL533
00733      MOVE WSR-AM-CARRIER         TO  NL-AM-CARRIER.               EL533
00734      MOVE WSR-AM-GROUPING        TO  NL-AM-GROUPING.              EL533
00735      MOVE WSR-AM-STATE           TO  NL-AM-STATE.                 EL533
00736      MOVE WSR-AM-ACCOUNT         TO  NL-AM-ACCOUNT.               EL533
00737                                                                   EL533
00738      PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.                  EL533
00739                                                                   EL533
00740      GO TO 1020-RETURN-SORT.                                      EL533
00741  EJECT                                                            EL533
00742  1200-SET-UP-COMP-RECORD.                                         EL533
00743      IF WSR-NAME  IS EQUAL TO  LR-NAME                            EL533
00744        AND WSR-SORT-PARM-CO  IS EQUAL TO  LAST-PARM-CO            EL533
00745          GO TO 1020-RETURN-SORT.                                  EL533
00746                                                                   EL533
00747      IF NDX  IS GREATER THAN  +0                                  EL533
00748          PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.              EL533
00749                                                                   EL533
00750      ADD 1                       TO  CMP-CTR.                     EL533
00751                                                                   EL533
00752      MOVE WS-SORT-REC            TO  LAST-REC.                    EL533
00753      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.          EL533
00754      MOVE 'NL'                   TO  NL-RECORD-ID.                EL533
00755      MOVE WSR-COMPANY-CD         TO  NL-COMPANY-CD.               EL533
00756      MOVE WSR-NAME               TO  NL-NAME.                     EL533
00757      MOVE WSR-CITY               TO  NL-CITY.                     EL533
00758      MOVE WSR-ST                 TO  NL-ST.                       EL533
00759      MOVE 'C'                    TO  NL-RECORD-TYPE.              EL533
00760      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL533
00761      MOVE '2'                    TO  DC-OPTION-CODE.              EL533
00762                                                                   EL533
00763      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL533
00764                                                                   EL533
00765      MOVE DC-BIN-DATE-1          TO  NL-LAST-MAINT-DT.            EL533
00766      MOVE WS-TIME                TO  NL-LAST-MAINT-HHMMSS.        EL533
00767      MOVE DTE-CLIENT             TO  NL-LAST-MAINT-USER.          EL533
00768      MOVE WSR-COMPANY-CD         TO  NL-CO-COMPANY-CD.            EL533
00769      MOVE WSR-CO-CARRIER         TO  NL-CO-CARRIER.               EL533
00770      MOVE WSR-CO-GROUPING        TO  NL-CO-GROUPING.              EL533
00771      MOVE WSR-CO-RESP-NO         TO  NL-CO-RESP-NO.               EL533
00772      MOVE WSR-CO-ACCOUNT         TO  NL-CO-ACCOUNT.               EL533
00773      MOVE WSR-CO-TYPE            TO  NL-CO-TYPE.                  EL533
00774                                                                   EL533
00775      PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.                  EL533
00776                                                                   EL533
00777      GO TO 1020-RETURN-SORT.                                      EL533
00778  EJECT                                                            EL533
00779  1300-SET-UP-REIN-RECORD.                                         EL533
00780      IF WSR-NAME  IS EQUAL TO  LR-NAME                            EL533
00781        AND WSR-SORT-PARM-RE  IS EQUAL TO  LAST-PARM-RE            EL533
00782          GO TO 1020-RETURN-SORT.                                  EL533
00783                                                                   EL533
00784      IF WSR-NAME  IS EQUAL TO  LR-NAME                            EL533
00785        AND WSR-SUB-SORT-PARM  IS EQUAL TO  LR-SUB-SORT-PARM       EL533
00786          GO TO 1310-ADD-LEVEL.                                    EL533
00787                                                                   EL533
00788      IF NDX  IS GREATER THAN  +0                                  EL533
00789          PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.              EL533
00790                                                                   EL533
00791      ADD 1                       TO  REI-CTR.                     EL533
00792                                                                   EL533
00793      MOVE WS-SORT-REC            TO  LAST-REC.                    EL533
00794      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.          EL533
00795      MOVE 'NL'                   TO  NL-RECORD-ID.                EL533
00796      MOVE WSR-COMPANY-CD         TO  NL-COMPANY-CD.               EL533
00797      MOVE WSR-NAME               TO  NL-NAME.                     EL533
00798      MOVE 'R'                    TO  NL-RECORD-TYPE.              EL533
00799      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL533
00800      MOVE '2'                    TO  DC-OPTION-CODE.              EL533
00801                                                                   EL533
00802      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL533
00803                                                                   EL533
00804      MOVE DC-BIN-DATE-1          TO  NL-LAST-MAINT-DT.            EL533
00805      MOVE WS-TIME                TO  NL-LAST-MAINT-HHMMSS.        EL533
00806      MOVE DTE-CLIENT             TO  NL-LAST-MAINT-USER.          EL533
00807      MOVE WSR-COMPANY-CD         TO  NL-RE-COMPANY-CD.            EL533
00808      MOVE WSR-RE-CODE            TO  NL-RE-CODE.                  EL533
00809      MOVE WSR-RE-COMPANY         TO  NL-RE-COMPANY.               EL533
00810      MOVE WSR-RE-TABLE           TO  NL-RE-TABLE.                 EL533
00811      MOVE WSR-RE-LEVEL           TO  NL-RE-LEVEL (1).             EL533
00812      MOVE +1                     TO  NDX.                         EL533
00813                                                                   EL533
00814      GO TO 1020-RETURN-SORT.                                      EL533
00815                                                                   EL533
00816  1310-ADD-LEVEL.                                                  EL533
00817      ADD +1                      TO  NDX.                         EL533
00818                                                                   EL533
00819      IF NDX  IS GREATER THAN  +30                                 EL533
00820          MOVE 'TO MANY LEVELS - ERROR IN 1310'                    EL533
00821                                  TO  WS-ABEND-MESSAGE             EL533
00822          GO TO ABEND-PGM.                                         EL533
00823                                                                   EL533
00824      MOVE WSR-RE-LEVEL           TO  NL-RE-LEVEL (NDX).           EL533
00825                                                                   EL533
00826      GO TO 1020-RETURN-SORT.                                      EL533
00827  EJECT                                                            EL533
00828  7100-WRITE-RECORD.                                               EL533
00829      WRITE NAME-LOOKUP-MASTER.                                    EL533
00830                                                                   EL533
00831      IF ERNAME-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00832          MOVE 'ERROR OCCURED WRITE - ERNAME'                      EL533
00833                                  TO  WS-ABEND-MESSAGE             EL533
00834          MOVE ERNAME-FILE-STATUS                                  EL533
00835                                  TO  WS-ABEND-FILE-STATUS         EL533
00836          GO TO ABEND-PGM.                                         EL533
00837                                                                   EL533
00838      ADD 1                       TO  REC-CTR.                     EL533
00839                                                                   EL533
00840      MOVE 0                      TO  NDX.                         EL533
00841                                                                   EL533
00842  7199-EXIT.                                                       EL533
00843      EXIT.                                                        EL533
00844  EJECT                                                            EL533
00845  7500-END-OUTPUT.                                                 EL533
00846      IF NDX  IS GREATER THAN  +0                                  EL533
00847          PERFORM 7100-WRITE-RECORD  THRU  7199-EXIT.              EL533
00848                                                                   EL533
00849      CLOSE  ERNAME.                                               EL533
00850                                                                   EL533
00851      IF ERNAME-FILE-STATUS  IS NOT EQUAL TO  ZERO                 EL533
00852          MOVE 'ERROR OCCURED CLOSE - ERNAME'                      EL533
00853                                  TO  WS-ABEND-MESSAGE             EL533
00854          MOVE ERNAME-FILE-STATUS                                  EL533
00855                                  TO  WS-ABEND-FILE-STATUS         EL533
00856          GO TO ABEND-PGM.                                         EL533
00857                                                                   EL533
00858      GO TO 8899-EXIT.                                             EL533
00859                                                                   EL533
00860  8500-DATE-CONVERT.                                               EL533
00861      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 EL533
00862                                                                   EL533
00863      IF DC-ERROR-CODE  IS NOT EQUAL TO  SPACE                     EL533
00864          MOVE ZEROS              TO  DC-CONVERSION-DATES.         EL533
00865                                                                   EL533
00866  8599-EXIT.                                                       EL533
00867      EXIT.                                                        EL533
00868                                                                   EL533
00869  8899-EXIT.                                                       EL533
00870      EXIT.                                                        EL533
00871  EJECT                                                            EL533
00872  9000-END-JOB SECTION.                                            EL533
00873                                                                   EL533
00874  9999-END-OF-JOB.                                                 EL533
00875      DISPLAY '**** NAME-XRF CREATE ****'.                         EL533
00876      DISPLAY 'NAME-XRF RECORDS DELETED  = ' DEL-CTR.              EL533
00877      DISPLAY 'ACCOUNT EXTRACTS          = ' ACT-CTR.              EL533
00878      DISPLAY 'COMPENSATION EXTRACTS     = ' CMP-CTR.              EL533
00879      DISPLAY 'REINSURANCE EXTRACTS      = ' REI-CTR.              EL533
00880      DISPLAY 'NAME-XRF RECORDS ADDED    = ' REC-CTR.              EL533
00881      GOBACK.                                                      EL533
00882  EJECT                                                            EL533
00883  ABEND-PGM.                                                       EL533
00884                                  COPY ELCABEND.                   EL533
