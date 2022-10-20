00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS038
00003  PROGRAM-ID.                ECS038.                                  LV007
00004 *              PROGRAM CONVERTED BY                               ECS038
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS038
00006 *              CONVERSION DATE 11/28/95 11:11:38.                 ECS038
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS038
00008 *                           VMOD=2.005.                           ECS038
00009                                                                   ECS038
00010 *AUTHOR.        LOGIC, INC.                                       ECS038
00011 *               DALLAS, TEXAS.                                    ECS038
00012                                                                   ECS038
00013 *DATE-COMPILED.                                                   ECS038
00014                                                                   ECS038
00015 *SECURITY.   *****************************************************ECS038
00016 *            *                                                   *ECS038
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS038
00018 *            *                                                   *ECS038
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS038
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS038
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS038
00022 *            *                                                   *ECS038
00023 *            *****************************************************ECS038
00024                                                                   ECS038
00025 *REMARKS.                                                         ECS038
00026 *        PROGRAM MERGES CURRENT MONTH CLAIMS EXTRACTS FROM        ECS038
00027 *        THE UPDATE PROGRAM (10) WITH THE CLAIM HISTORY FILE.     ECS038
00028 *        CLAIM SEQUENCE IS REIN-COMPANY, REIN-SUB-COMPANY,        ECS038
00029 *        CARRIER, GROUP, STATE, ACCOUNT, CLAIM-NUMBER,            ECS038
00030 *        AND PAY-TO-DATE.                                         ECS038
00031                                                                   ECS038
070714******************************************************************
070714*                   C H A N G E   L O G
070714*
070714* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070714*-----------------------------------------------------------------
070714*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070714* EFFECTIVE    NUMBER
070714*-----------------------------------------------------------------
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
070714******************************************************************
00032  ENVIRONMENT DIVISION.                                            ECS038
00033  CONFIGURATION SECTION.                                           ECS038
00034                                                                      CL**4
00035  INPUT-OUTPUT SECTION.                                            ECS038
00036  FILE-CONTROL.                                                    ECS038
00037                                                                   ECS038
00038      SELECT SORT-FILE       ASSIGN TO SYS001-UT-3380-S-SORTWK1.   ECS038
00039      SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.    ECS038
00040      SELECT CLAIMS-HISTORY  ASSIGN TO SYS010-UT-2400-S-SYS010.    ECS038
00041      SELECT MERGE-CLAIM     ASSIGN TO SYS011-UT-2400-S-SYS011.    ECS038
00042      SELECT DE-EXTRACT      ASSIGN TO SYS018-UT-2400-S-SYS018.    ECS038
00043      SELECT DISK-DATE       ASSIGN TO SYS019-UT-3380-S-SYS019.    ECS038
00044      SELECT FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.    ECS038
00045                                                                   ECS038
00046      SELECT ERMEBL                                                ECS038
00047              ASSIGN SYS024-FBA1-ERMEBL                            ECS038
00048              ORGANIZATION INDEXED                                 ECS038
00049              ACCESS DYNAMIC                                       ECS038
00050              RECORD KEY ME-CONTROL-PRIMARY                        ECS038
00051              FILE STATUS ERMEBL-FILE-STATUS.                      ECS038
00052                                                                   ECS038
00053  EJECT                                                            ECS038
00054  DATA DIVISION.                                                   ECS038
00055  FILE SECTION.                                                    ECS038
00056                                                                   ECS038
00057  SD  SORT-FILE.                                                   ECS038
00058                                                                   ECS038
00059  01  SORT-REC.                                                    ECS038
00060      12  S-CLM-REC.                                               ECS038
00061          16  FILLER              PIC X(334).                      ECS038
00062          16  SR-DE-TYPE          PIC X.                           ECS038
00063              88  SR-DE-DTH                       VALUE '1'.       ECS038
00064              88  SR-DE-AH                        VALUE '2'.       ECS038
00065              88  SR-DE-OB-DTH                    VALUE '3'.       ECS038
00066              88  SR-DE-OB-AH                     VALUE '4'.       ECS038
00067          16  FILLER              PIC X(175).                      ECS038
00068      12  SR-CNTRL.                                                ECS038
00069          16  S-REIN-COMP.                                         ECS038
00070              20 S-REIN           PIC XXX.                         ECS038
00071              20 S-REIN-SUB       PIC XXX.                         ECS038
00072          16  S-CARR              PIC X.                           ECS038
00073          16  S-GROUP             PIC X(6).                        ECS038
00074          16  S-ST                PIC XX.                          ECS038
00075          16  S-ACCT              PIC X(10).                       ECS038
00076          16  S-CLAIM             PIC X(7).                        ECS038
00077          16  S-CLM REDEFINES S-CLAIM.                             ECS038
00078              20  S1-CLM          PIC XX.                          ECS038
00079              20  S2-CLM          PIC X(5).                        ECS038
00080          16  S-PAY               PIC 9(11) COMP-3.                   CL**2
00081                                                                      CL**2
00082  EJECT                                                            ECS038
00083  FD  PRNTR                                                        ECS038
00084                                  COPY ELCPRTFD.                   ECS038
00085  EJECT                                                            ECS038
00086  FD  CLAIMS-HISTORY                                               ECS038
00087      BLOCK CONTAINS 0 RECORDS
00088      RECORDING MODE IS F.                                         ECS038
00089                                                                   ECS038
00090  01  OLD-CLM.                                                        CL**4
00091      12  FILLER                  PIC X(504).                         CL**4
00092      12  OC-CLM-PROC-DT          PIC 9(11)     COMP-3.               CL**4
00093  EJECT                                                            ECS038
00094  FD  MERGE-CLAIM                                                  ECS038
00095      BLOCK CONTAINS 0 RECORDS
00096      RECORDING MODE IS F.                                         ECS038
00097                                                                   ECS038
00098  01  NEW-CLMS                    PIC X(510).                      ECS038
00099  EJECT                                                            ECS038
00100  FD  DE-EXTRACT                                                   ECS038
00101      BLOCK CONTAINS 0 RECORDS
00102      RECORDING MODE IS F.                                         ECS038
00103                                                                   ECS038
00104  01  IN-CLMS                     PIC X(510).                      ECS038
00105  EJECT                                                            ECS038
00106  FD  DISK-DATE                                                    ECS038
00107                                  COPY ELCDTEFD.                   ECS038
00108  EJECT                                                            ECS038
00109  FD  FICH                                                         ECS038
00110                                  COPY ELCFCHFD.                   ECS038
00111  EJECT                                                            ECS038
00112  FD  ERMEBL.                                                      ECS038
00113                                  COPY ERCMEBL.                    ECS038
00114  EJECT                                                            ECS038
00115  WORKING-STORAGE SECTION.                                         ECS038
00116  77  FILLER  PIC X(32) VALUE '********************************'.  ECS038
00117  77  FILLER  PIC X(32) VALUE '     ECS038 WORKING STORAGE     '.  ECS038
00118  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.005 *********'.     CL**4
00119
      *01  cobdir               pic x(100) value spaces.
      *01  errno is external pic 9(9) comp-5.
      *01  env-name             pic x(100).
      *01  return-pointer usage pointer.
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Month end balancing work area                           ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00120  01  MONTH-END-DATA.                                              ECS038
00121      12  ME-START-DATE.                                           ECS038
00122          16  ME-START-MO         PIC 99.                          ECS038
00123          16  FILLER              PIC X.                           ECS038
00124          16  ME-START-DA         PIC 99.                          ECS038
00125          16  FILLER              PIC X.                           ECS038
00126          16  ME-START-YR         PIC 99.                          ECS038
00127      12  ME-CNDS-DATE            PIC 9(6).                        ECS038
00128      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS038
00129          16  ME-CNDS-MO          PIC 99.                          ECS038
00130          16  ME-CNDS-DA          PIC 99.                          ECS038
00131          16  ME-CNDS-YR          PIC 99.                          ECS038
00132      12  ME-START-TIME           PIC 9(6).                        ECS038
00133      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 ECS038
00134          88  ME-DO-UPDATE        VALUE 'Y'.                       ECS038
00135          88  ME-NO-UPDATE        VALUE 'N'.                       ECS038
00136      12  ERMEBL-FILE-STATUS      PIC XX.                          ECS038
00137      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                ECS038
070714     12  hld-038-recs-in         pic s9(7) comp-3 value +0.
070714     12  hld-038-recs-out        pic s9(7) comp-3 value +0.

00139  01  WS.                                                          ECS038
00140      12  PGM-SUB                 PIC S999 COMP       VALUE +038.  ECS038
00141      12  X                       PIC X               VALUE SPACE. ECS038
00142                                                                   ECS038
00143  01  WS-ABEND-FIELDS.                                             ECS038
00144      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS038
00145      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS038
00146      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS038
00147      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS038
00148                                                                   ECS038
00149  01  TOTALS-COUNTERS.                                             ECS038
00150      12  CLAIMS-HIST-CNTR                PIC 9(8) VALUE ZERO.     ECS038
00151      12  CLAIMS-IN-CNTR                  PIC 9(8) VALUE ZERO.     ECS038
00152      12  CLAIMS-MERGED-OUT-CNTR          PIC 9(8) VALUE ZERO.     ECS038
00153      12  DEATH-HIST-CNTR                 PIC 9(8) VALUE ZERO.     ECS038
00154      12  DEATH-IN-CNTR                   PIC 9(8) VALUE ZERO.     ECS038
00155      12  DEATH-MERGED-OUT-CNTR           PIC 9(8) VALUE ZERO.     ECS038
00156      12  DISABILITY-HIST-CNTR            PIC 9(8) VALUE ZERO.     ECS038
00157      12  DISABILITY-IN-CNTR              PIC 9(8) VALUE ZERO.     ECS038
00158      12  DISABILITY-MERGED-OUT-CNTR      PIC 9(8) VALUE ZERO.     ECS038
00159                                                                   ECS038
00160  01  OLD-CNTRL.                                                   ECS038
00161      12  O-REIN-COMP.                                             ECS038
00162          16 O-REIN               PIC XXX.                         ECS038
00163          16 O-REIN               PIC XXX.                         ECS038
00164      12  O-CARR                  PIC X.                           ECS038
00165      12  O-GROUP                 PIC X(6).                        ECS038
00166      12  O-ST                    PIC XX.                          ECS038
00167      12  O-ACCT                  PIC X(10).                       ECS038
00168      12  O-CLAIM                 PIC X(7).                        ECS038
00169      12  O-PAY                   PIC 9(11)  COMP-3.                  CL**2
00170                                                                   ECS038
00171  01  HEAD-1.                                                      ECS038
00172      12  FILLER      PIC X(53) VALUE SPACES.                      ECS038
00173      12  FILLER      PIC X(20) VALUE 'CLAIMS HISTORY MERGE'.      ECS038
00174      12  FILLER      PIC X(47) VALUE SPACES.                      ECS038
00175      12  FILLER      PIC X(07) VALUE 'ECS038'.                    ECS038
00176                                                                   ECS038
00177  01  HEAD-2.                                                      ECS038
00178      12  FILLER      PIC X(47) VALUE SPACES.                      ECS038
00179      12  HD-CLIENT   PIC X(30) VALUE SPACES.                      ECS038
00180      12  FILLER      PIC X(43) VALUE SPACES.                      ECS038
00181      12  HD-DATE     PIC X(08) VALUE SPACES.                      ECS038
00182                                                                   ECS038
00183  01  HEAD-3.                                                      ECS038
00184      12  FILLER      PIC X(53) VALUE SPACES.                      ECS038
00185      12  HD-ALF-DTE  PIC X(18) VALUE SPACES.                      ECS038
00186      12  FILLER      PIC X(41) VALUE SPACES.                      ECS038
00187      12  FILLER      PIC X(05) VALUE 'PAGE'.                      ECS038
00188      12  HD-PAGE     PIC ZZ,ZZZ.                                  ECS038
00189                                                                   ECS038
00190  01  TOT-LINE.                                                    ECS038
00191      12  FILLER      PIC X     VALUE SPACES.                      ECS038
00192      12  TOT-DESC.                                                ECS038
00193          16  FILLER  PIC X(20) VALUE SPACES.                      ECS038
00194          16  TOT-L6  PIC X(6)  VALUE SPACES.                      ECS038
00195          16  FILLER  PIC X(2)  VALUE SPACES.                      ECS038
00196      12  FILLER      PIC X     VALUE SPACES.                      ECS038
00197      12  TOT-CNTR    PIC ZZ,ZZZ,ZZ9.                              ECS038
00198      12  FILLER      PIC X(94) VALUE SPACES.                      ECS038
00199  EJECT                                                            ECS038
00200                                  COPY ECSEXT01.                   ECS038
00201                                                                   ECS038
00202                                  COPY ELCDATE.                       CL**7
00203                                                                   ECS038
00204                                  COPY ELCDTECX.                      CL**7
00205                                  COPY ELCDTEVR.                   ECS038
00206  EJECT
      *linkage section.
      *01  name-buff       pic x(100).
00207  PROCEDURE DIVISION.                                              ECS038
00208                                                                   ECS038
00209  CAPTURE-START.                                                   ECS038
00210                                                                   ECS038
00218  0100-SET-START.                                                  ECS038
00219                                  COPY ELCDTERX SUPPRESS.          ECS038
00220                                                                   ECS038
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Set up the month-end auto balancing.                    ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00221      MOVE WS-TIME                TO ME-START-TIME.                ECS038
00222      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                ECS038
00223      MOVE ME-START-MO            TO ME-CNDS-MO.                   ECS038
00224      MOVE ME-START-DA            TO ME-CNDS-DA.                   ECS038
00225      MOVE ME-START-YR            TO ME-CNDS-YR.                   ECS038
00226                                                                   ECS038
00236  0110-DO-SORT             SECTION.                                ECS038
00237                                                                   ECS038
00238  0120-NOW-SORT.                                                   ECS038
00239                                                                   ECS038
00240      SORT SORT-FILE ASCENDING KEY SR-CNTRL                        ECS038
00241          INPUT PROCEDURE 0140-GET-ONLY-CLAIMS                     ECS038
00242          OUTPUT PROCEDURE 0180-MERGE-WITH-OLD.                    ECS038
00243                                                                   ECS038
00244      IF SORT-RETURN NOT = (ZEROS AND 4)                           ECS038
00245          MOVE  0101              TO WS-RETURN-CODE                ECS038
00246          GO TO ABEND-PGM.                                         ECS038
00247                                                                   ECS038
00248  0130-E-NOW-SORT.                                                 ECS038
00249                                                                   ECS038
00250      GO TO 0250-E-O-J-CLOSER.                                     ECS038
00251                                                                   ECS038
00252  EJECT                                                            ECS038
00253  0140-GET-ONLY-CLAIMS     SECTION.                                ECS038
00254                                                                   ECS038
00255  0150-OPEN-EM.                                                    ECS038
00302 *    move 0 to errno
      *    string "DD_SYS011" low-values delimited by size into
      *        env-name
      *    end-string
      *    call "cobgetenv" using env-name returning return-pointer
      *    if return-pointer = null
      *       display "cobdir not found"
      *       goback
      *    end-if
      *    set address of name-buff to return-pointer
      *    string name-buff delimited by low-values into cobdir
      *    end-string
      *    display cobdir
00257      OPEN  INPUT DE-EXTRACT
pemuni           OUTPUT MERGE-CLAIM.
00258                                                                   ECS038
00259  0160-R-INPUT.                                                    ECS038
00260                                                                   ECS038
00261      READ DE-EXTRACT INTO DETAIL-EXTRACT AT END                   ECS038
00262          GO TO 0170-E-GET-CLAIMS.                                 ECS038
00263                                                                   ECS038
00264      IF DE-RECORD-ID NOT = 'DE'                                   ECS038
00265          GO TO 0160-R-INPUT.                                      ECS038
00266                                                                   ECS038
00267      IF NOT DE-CLAIM                                              ECS038
00268          GO TO 0160-R-INPUT.                                      ECS038
00269                                                                   ECS038
00270      ADD 1 TO CLAIMS-IN-CNTR.                                     ECS038
00271                                                                   ECS038
00272      IF DE-DTH OR                                                 ECS038
00273         DE-OB-DTH                                                 ECS038
00274          ADD 1 TO DEATH-IN-CNTR.                                  ECS038
00275                                                                   ECS038
00276      IF DE-AH OR                                                  ECS038
00277         DE-OB-AH                                                  ECS038
00278          ADD 1 TO DISABILITY-IN-CNTR.                             ECS038
00279                                                                   ECS038
00280      MOVE DE-CLAIM-EXTRACT       TO SORT-REC.                     ECS038
00281      MOVE DE-CARRIER             TO S-CARR.                       ECS038
00282      MOVE DE-GROUPING            TO S-GROUP.                      ECS038
00283      MOVE SPACES                 TO S-REIN-COMP.                  ECS038
00284                                                                   ECS038
00285      IF DE-REIN NOT = SPACE                                       ECS038
00286          MOVE DE-REI-COMP        TO S-REIN-COMP.                  ECS038
00287                                                                   ECS038
00288      MOVE DE-STATE               TO S-ST.                         ECS038
00289      MOVE DE-ACCOUNT             TO S-ACCT.                       ECS038
00290      MOVE DE-CNUM                TO S-CLAIM.                      ECS038
00291      MOVE DE-PAID-TO             TO S-PAY.                        ECS038
00292                                                                   ECS038
00293      RELEASE SORT-REC.                                            ECS038
00294      GO TO 0160-R-INPUT.                                          ECS038
00295                                                                   ECS038
00296  0170-E-GET-CLAIMS.                                               ECS038
00297      EXIT.                                                        ECS038
00298  EJECT                                                            ECS038
00299  0180-MERGE-WITH-OLD      SECTION.                                ECS038
00300                                                                   ECS038
00301  0190-OPEN-OLDS.                                                  ECS038
00302 *    move 0 to errno
      *    string "DD_SYS011" low-values delimited by size into
      *        env-name
      *    end-string
      *    call "cobgetenv" using env-name returning return-pointer
      *    if return-pointer = null
      *       display "cobdir not found"
      *       goback
      *    end-if
      *    set address of name-buff to return-pointer
      *    string name-buff delimited by low-values into cobdir
      *    end-string
      *    display cobdir
00303      CLOSE DE-EXTRACT.                                            ECS038
00304      OPEN INPUT  CLAIMS-HISTORY.                                  ECS038
pemuni*         OUTPUT MERGE-CLAIM.                                     ECS038
00306                                                                   ECS038
00307  0200-R-OLDS.                                                     ECS038
00308                                                                   ECS038
00309      READ CLAIMS-HISTORY INTO DETAIL-EXTRACT AT END               ECS038
00310 *        MOVE ALL '9'            TO OLD-CNTRL                     ECS038
00310          MOVE HIGH-VALUES        TO OLD-CNTRL                     ECS038
00311          GO TO 0210-E-R-OLDS.                                     ECS038
00312                                                                   ECS038
00313      ADD 1 TO CLAIMS-HIST-CNTR.                                   ECS038
00314                                                                   ECS038
00315      IF DE-DTH OR                                                 ECS038
00316         DE-OB-DTH                                                 ECS038
00317          ADD 1 TO DEATH-HIST-CNTR.                                ECS038
00318                                                                   ECS038
00319      IF DE-AH OR                                                  ECS038
00320         DE-OB-AH                                                  ECS038
00321          ADD 1 TO DISABILITY-HIST-CNTR.                           ECS038
00322                                                                   ECS038
00323      MOVE DE-CARRIER             TO O-CARR.                       ECS038
00324      MOVE DE-GROUPING            TO O-GROUP.                      ECS038
00325      MOVE DE-STATE               TO O-ST.                         ECS038
00326      MOVE DE-ACCOUNT             TO O-ACCT.                       ECS038
00327      MOVE DE-CNUM                TO O-CLAIM.                      ECS038
00328      MOVE DE-PAID-TO             TO O-PAY.                        ECS038
00329                                                                   ECS038
00330      IF DE-CLM-PROC-DT NOT NUMERIC                                ECS038
00331           MOVE DE-PAY            TO OC-CLM-PROC-DT.                  CL**4
00332                                                                   ECS038
00333      MOVE SPACES                 TO O-REIN-COMP.                  ECS038
00334                                                                   ECS038
00335      IF DE-REIN NOT = SPACES                                      ECS038
00336          MOVE DE-REI-COMP        TO O-REIN-COMP.                  ECS038
00337                                                                   ECS038
00338  0210-E-R-OLDS.                                                   ECS038
00339      EXIT.                                                        ECS038
00340                                                                   ECS038
00341  0220-R-SORTED.                                                   ECS038
00342                                                                   ECS038
00343      RETURN SORT-FILE AT END                                      ECS038
00344 *        MOVE ALL '9'            TO SR-CNTRL.                     ECS038
00344          MOVE HIGH-VALUES        TO SR-CNTRL.                     ECS038
00345                                                                   ECS038
00346  0230-MATCH-EM.                                                   ECS038
00347                                                                   ECS038
00348      IF SR-CNTRL = OLD-CNTRL AND                                  ECS038
00349 *       SR-CNTRL = ALL '9'                                        ECS038
00349         SR-CNTRL = HIGH-VALUES                                    ECS038
00350          GO TO 0240-E-MERGE-EM.                                   ECS038
00351                                                                   ECS038
00352      ADD 1 TO CLAIMS-MERGED-OUT-CNTR.                             ECS038
00353                                                                   ECS038
00354      IF SR-CNTRL LESS OLD-CNTRL                                   ECS038
00355          PERFORM 0245-COUNT-EM THRU 0245-COUNT-EM-EXIT            ECS038
00356          MOVE SORT-REC           TO NEW-CLMS                      ECS038
00357          WRITE NEW-CLMS                                           ECS038
00358          GO TO 0220-R-SORTED.                                     ECS038
00359                                                                   ECS038
00360      MOVE OLD-CLM                TO NEW-CLMS.                     ECS038
00361                                                                   ECS038
00362      IF DE-DTH OR                                                 ECS038
00363         DE-OB-DTH                                                 ECS038
00364          ADD 1 TO DEATH-MERGED-OUT-CNTR.                          ECS038
00365                                                                   ECS038
00366      IF DE-AH OR                                                  ECS038
00367         DE-OB-AH                                                  ECS038
00368          ADD 1 TO DISABILITY-MERGED-OUT-CNTR.                     ECS038
00369                                                                   ECS038
00370      WRITE NEW-CLMS.                                              ECS038
00371      PERFORM 0200-R-OLDS THRU 0210-E-R-OLDS.                      ECS038
00372      GO TO 0230-MATCH-EM.                                         ECS038
00373                                                                   ECS038
00374  0240-E-MERGE-EM.                                                 ECS038
00375      EXIT.                                                        ECS038
00376  EJECT                                                            ECS038
00377  0245-COUNT-EM                SECTION.                            ECS038
00378                                                                   ECS038
00379      IF SR-DE-DTH OR                                              ECS038
00380         SR-DE-OB-DTH                                              ECS038
00381          ADD 1 TO DEATH-MERGED-OUT-CNTR.                          ECS038
00382                                                                   ECS038
00383      IF SR-DE-AH OR                                               ECS038
00384         SR-DE-OB-AH                                               ECS038
00385          ADD 1 TO DISABILITY-MERGED-OUT-CNTR.                     ECS038
00386                                                                   ECS038
00387  0245-COUNT-EM-EXIT.                                              ECS038
00388                                                                   ECS038
00389  0250-E-O-J-CLOSER            SECTION.                            ECS038
00390      OPEN OUTPUT PRNTR.                                           ECS038
00391                                                                   ECS038
00392  0260-PRINT-HEADINGS.                                             ECS038
00393                                                                   ECS038
00394      MOVE '1'                    TO X.                            ECS038
00395      MOVE HEAD-1                 TO P-DATA.                       ECS038
00396      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00397      MOVE ' '                    TO X.                            ECS038
00398      MOVE WS-CURRENT-DATE        TO HD-DATE.                      ECS038
00399      MOVE COMPANY-NAME           TO HD-CLIENT.                    ECS038
00400      MOVE HEAD-2                 TO P-DATA.                       ECS038
00401      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00402      MOVE 1                      TO HD-PAGE.                      ECS038
00403      MOVE ALPH-DATE              TO HD-ALF-DTE.                   ECS038
00404      MOVE HEAD-3                 TO P-DATA.                       ECS038
00405      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00406                                                                   ECS038
00407      IF ME-DO-UPDATE                                              ECS038
070714        MOVE CLAIMS-HIST-CNTR    TO hld-038-RECS-IN
070714        MOVE CLAIMS-MERGED-OUT-CNTR
070714                                 TO hld-038-RECS-OUT
070714     end-if
00410                                                                   ECS038
00411      MOVE 'NEW CLAIM RECORDS - XXXXXX =' TO TOT-DESC.             ECS038
00412      MOVE LIFE-OVERRIDE-L6               TO TOT-L6.               ECS038
00413      MOVE DEATH-IN-CNTR                  TO TOT-CNTR.             ECS038
00414      MOVE '-'                            TO X.                    ECS038
00415      MOVE TOT-LINE                       TO P-DATA.               ECS038
00416      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00417                                                                   ECS038
00418      MOVE 'NEW CLAIM RECORDS - XXXXXX =' TO TOT-DESC.             ECS038
00419      MOVE AH-OVERRIDE-L6                 TO TOT-L6.               ECS038
00420      MOVE DISABILITY-IN-CNTR             TO TOT-CNTR.             ECS038
00421      MOVE ' '                            TO X.                    ECS038
00422      MOVE TOT-LINE                       TO P-DATA.               ECS038
00423      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00424                                                                   ECS038
00425      MOVE 'NEW CLAIM RECORDS - TOTAL  =' TO TOT-DESC.             ECS038
00426      MOVE CLAIMS-IN-CNTR                 TO TOT-CNTR.             ECS038
00427      MOVE ' '                            TO X.                    ECS038
00428      MOVE TOT-LINE                       TO P-DATA.               ECS038
00429      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00430                                                                   ECS038
00431      MOVE 'HISTORY CLAIMS IN - XXXXXX =' TO TOT-DESC.             ECS038
00432      MOVE LIFE-OVERRIDE-L6               TO TOT-L6.               ECS038
00433      MOVE DEATH-HIST-CNTR                TO TOT-CNTR.             ECS038
00434      MOVE '0'                            TO X.                    ECS038
00435      MOVE TOT-LINE                       TO P-DATA.               ECS038
00436      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00437                                                                   ECS038
00438      MOVE 'HISTORY CLAIMS IN - XXXXXX =' TO TOT-DESC.             ECS038
00439      MOVE AH-OVERRIDE-L6                 TO TOT-L6.               ECS038
00440      MOVE DISABILITY-HIST-CNTR           TO TOT-CNTR.             ECS038
00441      MOVE ' '                            TO X.                    ECS038
00442      MOVE TOT-LINE                       TO P-DATA.               ECS038
00443      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00444                                                                   ECS038
00445      MOVE 'HISTORY CLAIMS IN - TOTAL  =' TO TOT-DESC.             ECS038
00446      MOVE CLAIMS-HIST-CNTR               TO TOT-CNTR.             ECS038
00447      MOVE ' '                            TO X.                    ECS038
00448      MOVE TOT-LINE                       TO P-DATA.               ECS038
00449      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00450                                                                   ECS038
00451      MOVE 'HISTORY CLAIM OUT - XXXXXX =' TO TOT-DESC.             ECS038
00452      MOVE LIFE-OVERRIDE-L6               TO TOT-L6.               ECS038
00453      MOVE DEATH-MERGED-OUT-CNTR          TO TOT-CNTR.             ECS038
00454      MOVE '0'                            TO X.                    ECS038
00455      MOVE TOT-LINE                       TO P-DATA.               ECS038
00456      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00457                                                                   ECS038
00458      MOVE 'HISTORY CLAIM OUT - XXXXXX =' TO TOT-DESC.             ECS038
00459      MOVE AH-OVERRIDE-L6                 TO TOT-L6.               ECS038
00460      MOVE DISABILITY-MERGED-OUT-CNTR     TO TOT-CNTR.             ECS038
00461      MOVE ' '                            TO X.                    ECS038
00462      MOVE TOT-LINE                       TO P-DATA.               ECS038
00463      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00464                                                                   ECS038
00465      MOVE 'HISTORY CLAIM OUT - TOTAL  =' TO TOT-DESC.             ECS038
00466      MOVE CLAIMS-MERGED-OUT-CNTR         TO TOT-CNTR.             ECS038
00467      MOVE ' '                            TO X.                    ECS038
00468      MOVE TOT-LINE                       TO P-DATA.               ECS038
00469      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00470                                                                   ECS038
00471                                                                   ECS038
00472      CLOSE CLAIMS-HISTORY MERGE-CLAIM.                            ECS038
00473                                                                   ECS038

070714     OPEN I-O ERMEBL.                                             ECS038
070714                                                                  ECS038
070714     IF ERMEBL-FILE-STATUS  = '00' OR '97'                        ECS038
070714         NEXT SENTENCE                                            ECS038
070714       ELSE                                                       ECS038
070714         MOVE 'N'                TO ME-UPDATE-FLAG.               ECS038
070714                                                                  ECS038
070714     MOVE DTE-CLIENT             TO ME-COMPANY.                   ECS038
070714     COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**5
070714     MOVE MONTH-END-MOYR         TO ME-MOYR.                      ECS038
070714                                                                  ECS038
070714     IF ME-DO-UPDATE                                              ECS038
070714         READ ERMEBL INVALID KEY                                  ECS038
070714         MOVE 'N'                TO ME-UPDATE-FLAG                ECS038
070714         CLOSE ERMEBL.                                            ECS038
070714                                                                  ECS038
070714     IF ME-DO-UPDATE
070714        move hld-038-recs-in    to me-038-recs-in
070714        move hld-038-recs-out   to me-038-recs-out
070714        MOVE ME-CNDS-DATE       TO ME-038-RUN-DT
070714        ADD 1                   TO ME-038-RUN-CT
070714        REWRITE MONTH-END-BALANCES
070714        CLOSE ERMEBL
070714     end-if
00482                                                                   ECS038
00483      IF ME-DO-UPDATE                                              ECS038
00484          MOVE 'MONTH-END BALANCES POSTED'     TO P-DATA           ECS038
00485      ELSE                                                         ECS038
00486          MOVE 'MONTH-END BALANCES NOT POSTED' TO P-DATA.          ECS038
00487                                                                   ECS038
00488      MOVE '-'                            TO X.                    ECS038
00489      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00490                                                                   ECS038
00491      MOVE '-'                            TO X.                    ECS038
00492      MOVE 'CLAIMS HISTORY UPDATE COMPLETED' TO P-DATA.            ECS038
00493      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS038
00494      GO TO 0290-CLOSE-FICH.                                       ECS038
00495                                                                   ECS038
00496  0270-PRINT-RTN.                                                  ECS038
00497                                  COPY ELCPRT2.                    ECS038
00498                                                                   ECS038
00499  0280-PRINT-RTN-EXIT.                                             ECS038
00500      EXIT.                                                        ECS038
00501                                                                   ECS038
00502  0290-CLOSE-FICH.                                                 ECS038
00503                                  COPY ELCPRTC.                    ECS038
00504                                                                   ECS038
00505  0300-CLOSE-PRINTER.                                              ECS038
00506      CLOSE PRNTR.                                                 ECS038
00507                                                                   ECS038
00508      GOBACK.                                                      ECS038
00509                                                                   ECS038
00510  ABEND-PGM SECTION.                                               ECS038
00511                                COPY ELCABEND.                     ECS038
00512                                                                      CL**4
