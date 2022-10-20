00001  IDENTIFICATION DIVISION.                                         10/11/97
00002                                                                   EL343
00003  PROGRAM-ID.                 EL343 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL343
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL343
00006 *              CONVERSION DATE 03/04/95 10:37:19.                 EL343
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL343
00008 *                            VMOD=2.005                           EL343
00009                                                                   EL343
00009                                                                   EL343
00010 *AUTHOR.     LOGIC, INC.                                          EL343
00011 *            DALLAS, TEXAS.                                       EL343
00012                                                                   EL343
00013 *DATE-COMPILED.                                                   EL343
00014                                                                   EL343
00015 *SECURITY.   *****************************************************EL343
00016 *            *                                                   *EL343
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL343
00018 *            *                                                   *EL343
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL343
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL343
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL343
00022 *            *                                                   *EL343
00023 *            *****************************************************EL343
00024                                                                   EL343
00025 *REMARKS.                                                         EL343
00026 *    PRINTS ROLODEX CARDS FROM THE BENEFICIARY MASTER.            EL343
00027                                                                   EL343
00028 *    SWITCH  -  DEFINITION                                        EL343
00029                                                                   EL343
00030 *        1   -   ROLODEX FOR MASTERS WITH MAINTENANCE THIS MONTH  EL343
00031 *        2   -   ROLODEX FOR ALL MASTERS                          EL343
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ******************************************************************
00033  ENVIRONMENT DIVISION.                                            EL343
00034                                                                   EL343
00035  INPUT-OUTPUT SECTION.                                            EL343
00036                                                                   EL343
00037  FILE-CONTROL.                                                    EL343
00038                                                                   EL343
00039      SELECT ELBENE  ASSIGN TO SYS024-FBA1-ELBENE                  EL343
00040              ORGANIZATION IS INDEXED                              EL343
00041              ACCESS IS DYNAMIC                                    EL343
00042              RECORD KEY IS BE-CONTROL-PRIMARY                     EL343
00043              FILE STATUS IS BE-STATUS.                            EL343
00044                                                                   EL343
00045      SELECT ELREPT ASSIGN TO SYS010-FBA1-ELREPT                   EL343
00046              ORGANIZATION IS INDEXED                              EL343
00047              ACCESS IS DYNAMIC                                    EL343
00048              RECORD KEY IS RF-CONTROL-PRIMARY                     EL343
00049              FILE STATUS IS DTE-VSAM-FLAGS.                       EL343
00050                                                                   EL343
00051      SELECT SORT-WORK        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL343
00052      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL343
00053      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL343
00054      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL343
00055  EJECT                                                            EL343
00056  DATA DIVISION.                                                   EL343
00057  FILE SECTION.                                                    EL343
00058                                                                   EL343
00059  FD  ELBENE.                                                      EL343
00060                                                                   EL343
00061      COPY ELCBENE.                                                EL343
00062  EJECT                                                            EL343
00063  FD  ELREPT                  COPY ELCRPTFD.                       EL343
00064                              COPY ELCREPT.                        EL343
00065      EJECT                                                        EL343
00066  SD  SORT-WORK.                                                   EL343
00067                                                                   EL343
00068  01  SORT-RECORD.                                                 EL343
00069      12  FILLER                      PIC X(61).                   EL343
00070      12  SORT-BE-NAME                PIC X(30).                   EL343
00071      12  FILLER                      PIC X(60).                   EL343
00072      12  SORT-BE-CITY                PIC X(30).                   EL343
00073      12  FILLER                      PIC X(319).                  EL343
00074                                                                   EL343
00075  FD  DISK-DATE                                                    EL343
00076                              COPY ELCDTEFD.                          CL**2
00077  FD  PRNTR                                                        EL343
00078                              COPY ELCPRTFD.                          CL**2
00079  FD  FICH                                                         EL343
00080                              COPY ELCFCHFD.                          CL**2
00081  EJECT                                                            EL343
00082  WORKING-STORAGE SECTION.                                         EL343
00083  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL343
00084  77  FILLER   PIC X(32) VALUE '********************************'. EL343
00085  77  FILLER   PIC X(32) VALUE '*          EL343               *'. EL343
00086  77  FILLER   PIC X(32) VALUE '******** VMOD=2.005*************'. EL343
00087                                                                   EL343
00088  77  X              PIC X VALUE ' '.                              EL343
00089  77  SPACE-NP       PIC X VALUE '1'.                              EL343
00090  77  SPACE-1        PIC X VALUE ' '.                              EL343
00091  77  SPACE-2        PIC X VALUE '0'.                              EL343
00092  77  SPACE-3        PIC X VALUE '-'.                              EL343
00093  77  K0             PIC S9           COMP-3 VALUE +0.             EL343
00094  77  K1             PIC S9           COMP-3 VALUE +1.             EL343
00095  77  K2             PIC S9           COMP-3 VALUE +2.             EL343
00096  77  K3             PIC S9           COMP-3 VALUE +3.             EL343
00097  77  X1             PIC S999         COMP-3 VALUE +0.             EL343
00098  77  PGM-SUB        PIC S999         COMP-3 VALUE +343.           EL343
00099  77  MAX-NDX        PIC S999         COMP-3 VALUE +002.           EL343
00100  77  MAX-ALIGN-CTR  PIC S9(9)        COMP-3 VALUE +004.           EL343
00101  77  ALIGN-CTR      PIC S9(9)        COMP-3 VALUE +0.             EL343
00102  77  NBR-OF-CARDS   PIC S9(7)        COMP-3 VALUE +0.             EL343
00103  77  A              PIC S999         COMP-3 VALUE +0.             EL343
00104  77  B              PIC S999         COMP-3 VALUE +0.             EL343
00105  77  OLC-REPORT-NAME             PIC X(6)      VALUE 'EL343 '.    EL343
00106  77  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.      EL343
00107  77  WS-ABEND-FILE-STATUS        PIC XX        VALUE ZERO.        EL343
00108  77  WS-ABEND-PROGRAM            PIC X(8)      VALUE SPACES.      EL343
00109  77  WS-RETURN-CODE              PIC S9(4)     VALUE +0 COMP.     EL343
00110  77  WS-ZERO                     PIC S9 COMP-3 VALUE +0.          EL343
00111  77  ABEND-CODE                  PIC X(4)      VALUE ZERO.        EL343
00112  77  ABEND-OPTION                PIC X         VALUE 'Y'.         EL343
00113                                                                   EL343
00114  01  BE-STATUS.                                                   EL343
00115      03  BE-STAT-1   PIC X.                                       EL343
00116      03  BE-STAT-2   PIC X.                                       EL343
00117  01  PHONE-NO            PIC 9(10).                               EL343
00118  01  PHONE-NO-S REDEFINES PHONE-NO.                               EL343
00119      03 PHN-ACOD         PIC 9(3).                                EL343
00120      03 PHN-PFX          PIC 9(3).                                EL343
00121      03 PHN-REST         PIC 9(4).                                EL343
00122  EJECT                                                            EL343
00123      COPY ELCDTECX.                                                  CL**3
00124      COPY ELCDTEVR.                                                  CL**3
00125  EJECT                                                            EL343
00126  01  MISC-WS.                                                     EL343
00127      03 A-NAME.                                                   EL343
00128         05 A-CHAR OCCURS 30 TIMES PIC X.                          EL343
00129      03 B-NAME.                                                   EL343
00130         05 B-CHAR OCCURS 30 TIMES PIC X.                          EL343
00131  01  ROLODEX-AREA.                                                EL343
00132                                                                   EL343
00133      03 ROLX-1.                                                   EL343
00134         05 RX-1          OCCURS 2 TIMES.                          EL343
00135            07 FILLER          PIC X(2).                           EL343
00136            07 R1-INFO.                                            EL343
00137               09 R1-CODE      PIC X(20).                          EL343
00138               09 R1-ACOD      PIC XXX.                            EL343
00139               09 R1-DASH1     PIC X.                              EL343
00140               09 R1-PFX       PIC XXX.                            EL343
00141               09 R1-DASH2     PIC X.                              EL343
00142               09 R1-REST      PIC X(4).                           EL343
00143               09 FILLER       PIC X(16).                          EL343
00144                                                                   EL343
00145      03 ROLX-2.                                                   EL343
00146         05 RX-2          OCCURS 2 TIMES.                          EL343
00147            07 FILLER          PIC X(2).                           EL343
00148            07 R2-INFO.                                            EL343
00149               09 R2-MAIL-NAME PIC X(29).                          EL343
00150               09 FILLER       PIC X.                              EL343
00151               09 R2-CON1      PIC X(6).                           EL343
00152               09 R2-AREA      PIC X(3).                           EL343
00153               09 FILLER       PIC X.                              EL343
00154               09 R2-PREF      PIC X(3).                           EL343
00155               09 R2-DSH1      PIC X.                              EL343
00156               09 R2-FONE      PIC X(4).                           EL343
00157                                                                   EL343
00158      03 ROLX-3.                                                   EL343
00159         05 RX-3          OCCURS 2 TIMES.                          EL343
00160            07 FILLER          PIC X(2).                           EL343
00161            07 R3-INFO.                                            EL343
00162               09 R3-ADDR-1    PIC X(29).                          EL343
00163               09 FILLER       PIC X.                              EL343
00164               09 R3-CON1      PIC X(5).                           EL343
00165               09 R3-SS-ID     PIC X(13).                          EL343
00166                                                                   EL343
00167      03 ROLX-4.                                                   EL343
00168         05 RX-4          OCCURS 2 TIMES.                          EL343
00169            07 FILLER          PIC X(2).                           EL343
00170            07 R4-INFO         PIC X(48).                          EL343
00171                                                                   EL343
00172      03 ROLX-5.                                                   EL343
00173         05 RX-5          OCCURS 2 TIMES.                          EL343
00174            07 FILLER          PIC X(2).                           EL343
00175            07 R5-INFO.                                            EL343
00176               09 FILLER       PIC X(30).                          EL343
00177               09 R5-AMER-ZIP.                                     EL343
00178                  12 R5-ZIP    PIC X(5).                           EL343
00179                  12 R5-ZIP-EXT                                    EL343
00180                               PIC X(4).                           EL343
00181               09 R5-CANADIAN-POSTAL-CODE REDEFINES R5-AMER-ZIP.   EL343
00182                  12 R5-CAN-POSTAL-CODE-1                          EL343
00183                               PIC X(3).                           EL343
00184                  12 R5-CAN-POSTAL-CODE-2                          EL343
00185                               PIC X(3).                           EL343
00186                  12 R5-CAN-FILLER                                 EL343
00187                               PIC X(3).                           EL343
00188               09 FILLER       PIC X(9).                           EL343
00189                                                                   EL343
00190      03 ROLX-6.                                                   EL343
00191         05 RX-6          OCCURS 2 TIMES.                          EL343
00192            07 FILLER          PIC X(2).                           EL343
00193            07 R6-INFO.                                            EL343
00194               09 FILLER       PIC X(48).                          EL343
00195  EJECT                                                            EL343
00196  01  P-REC.                                                       EL343
00197      03 P-CCSW           PIC X.                                   EL343
00198      03 P-LN.                                                     EL343
00199         05 FILLER PIC X(132).                                     EL343
00200  EJECT                                                            EL343
00201  PROCEDURE DIVISION.                                              EL343
00202  0000-STANDARD-COPY.                                              EL343
00203       COPY ELCDTERX.                                                 CL**2
00204                                                                   EL343
00205  1000-INTL-INPUT.                                                 EL343
00206      OPEN INPUT  ELBENE                                           EL343
00207          OUTPUT  PRNTR.                                           EL343
00208                                                                   EL343
00209      IF BE-STATUS = '00' OR '97'                                  EL343
00210          NEXT SENTENCE                                            EL343
00211        ELSE                                                       EL343
00212          MOVE BE-STATUS          TO  WS-ABEND-FILE-STATUS         EL343
00213          MOVE 'ERROR OCCURED OPEN - ELBENE'                       EL343
00214                                  TO  WS-ABEND-MESSAGE             EL343
00215          PERFORM ABEND-PGM.                                       EL343
00216                                                                   EL343
00217      IF DTE-PGM-OPT LESS    THAN 1 OR                             EL343
00218         DTE-PGM-OPT GREATER THAN 2                                EL343
00219         MOVE 1 TO DTE-PGM-OPT.                                    EL343
00220                                                                   EL343
00221      IF DTE-PGM-OPT = 2                                           EL343
00222         MOVE +001 TO MAX-ALIGN-CTR.                               EL343
00223                                                                   EL343
00224      MOVE SPACES       TO ROLODEX-AREA.                           EL343
00225      MOVE ALL 'X'      TO R1-INFO (1) R1-INFO (2)                 EL343
00226                           R2-INFO (1) R2-INFO (2)                 EL343
00227                           R3-INFO (1) R3-INFO (2)                 EL343
00228                           R4-INFO (1) R4-INFO (2)                 EL343
00229                           R5-INFO (1) R5-INFO (2)                 EL343
00230                           R6-INFO (1) R6-INFO (2).                EL343
00231      MOVE COMPANY-NAME TO R1-INFO (1) R1-INFO (2).                EL343
00232      MOVE MAX-NDX      TO X1.                                     EL343
00233                                                                   EL343
00234  1500-SORT-BENE.                                                  EL343
00235         SORT SORT-WORK   ASCENDING KEY                            EL343
00236                               SORT-BE-NAME                        EL343
00237                                    SORT-BE-CITY                   EL343
00238                                                                   EL343
00239                INPUT PROCEDURE  2000-RD-BENE                      EL343
00240                OUTPUT PROCEDURE 4000-READ-SORTED.                 EL343
00241                                                                   EL343
00242      CLOSE ELBENE                                                 EL343
00243            PRNTR.                                                 EL343
00244      COPY ELCPRTCX.                                                  CL**2
00245      GOBACK.                                                      EL343
00246  EJECT                                                            EL343
00247  2000-RD-BENE           SECTION.                                  EL343
00248      MOVE LOW-VALUES            TO BE-CONTROL-PRIMARY.            EL343
00249      MOVE DTE-CLASIC-COMPANY-CD TO BE-COMPANY-CD.                 EL343
00250                                                                   EL343
00251      START ELBENE   KEY NOT LESS THAN BE-CONTROL-PRIMARY.         EL343
00252                                                                   EL343
00253      IF BE-STATUS = '23'                                          EL343
00254          GO TO 3000-E-INPUT-SECT.                                 EL343
00255                                                                   EL343
00256      IF BE-STAT-1 NOT = ZERO                                      EL343
00257          MOVE BE-STATUS          TO  WS-ABEND-FILE-STATUS         EL343
00258          MOVE 'ERROR OCCURED START - ELBENE'                      EL343
00259                                  TO  WS-ABEND-MESSAGE             EL343
00260          PERFORM ABEND-PGM.                                       EL343
00261                                                                   EL343
00262                                                                   EL343
00263  2020-RD-BENE.                                                    EL343
00264      READ ELBENE  NEXT RECORD.                                    EL343
00265                                                                   EL343
00266      IF BE-STAT-1  = '1'                                          EL343
00267          GO TO 3000-E-INPUT-SECT.                                 EL343
00268                                                                   EL343
00269      IF BE-STAT-1 NOT = ZERO                                      EL343
00270          MOVE BE-STATUS          TO  WS-ABEND-FILE-STATUS         EL343
00271          MOVE 'ERROR OCCURED READ - ELBENE'                       EL343
00272                                  TO  WS-ABEND-MESSAGE             EL343
00273          PERFORM ABEND-PGM.                                       EL343
00274                                                                   EL343
00275      IF BE-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL343
00276            GO TO 2020-RD-BENE.                                    EL343
00277                                                                   EL343
00278  2500-RELEASE.                                                    EL343
00279      MOVE BENEFICIARY-MASTER TO SORT-RECORD.                      EL343
00280      RELEASE SORT-RECORD.                                         EL343
00281      GO TO 2020-RD-BENE.                                          EL343
00282                                                                   EL343
00283                                                                   EL343
00284  3000-E-INPUT-SECT.                                               EL343
00285      EXIT.                                                        EL343
00286                                                                   EL343
00287      EJECT                                                        EL343
00288  4000-READ-SORTED     SECTION.                                    EL343
00289  4000-READ-IT.                                                    EL343
00290      RETURN SORT-WORK                                             EL343
00291           AT END GO TO 9000-EOF.                                  EL343
00292                                                                   EL343
00293      MOVE SORT-RECORD TO BENEFICIARY-MASTER.                      EL343
00294                                                                   EL343
00295  5000-FORMAT-ROLODEX-RTN.                                         EL343
00296      ADD K1 TO X1.                                                EL343
00297      IF X1 GREATER THAN MAX-NDX                                   EL343
00298         PERFORM 5800-GEN-ROLX-RTN THRU 5999-GEN-ROLX-XIT.         EL343
00299                                                                   EL343
00300      ADD  +1                TO NBR-OF-CARDS.                      EL343
00301                                                                   EL343
00302      MOVE BE-BENEFICIARY    TO R1-CODE (X1).                      EL343
00303      MOVE BE-PHONE-NO       TO PHONE-NO.                          EL343
00304      MOVE PHN-ACOD          TO R1-ACOD (X1).                      EL343
00305      MOVE PHN-PFX           TO R1-PFX  (X1).                      EL343
00306      MOVE PHN-REST          TO R1-REST (X1).                      EL343
00307      MOVE '-'               TO R1-DASH1 (X1)  R1-DASH2 (X1).      EL343
00308      MOVE BE-MAIL-TO-NAME   TO R2-INFO (X1).                      EL343
00309      MOVE BE-ADDRESS-LINE-1 TO R3-INFO (X1).                      EL343
00310      MOVE BE-ADDRESS-LINE-2 TO R4-INFO (X1).                      EL343
051810     MOVE SPACES            TO R5-INFO (X1).                      EL343
051810     STRING BE-CITY ' ' BE-STATE
051810        DELIMITED BY '  ' INTO R5-INFO (X1)
051810     END-STRING
00312      MOVE BE-ZIP-CODE       TO R5-AMER-ZIP (X1).                  EL343
00313                                                                   EL343
00314      IF  BE-CANADIAN-POST-CODE                                    EL343
00315          MOVE SPACES        TO R5-CAN-FILLER (X1)                 EL343
00316      ELSE                                                         EL343
00317          IF  BE-ZIP-PLUS4 = ZEROS                                 EL343
00318              MOVE SPACES    TO R5-ZIP-EXT (X1).                   EL343
00319                                                                   EL343
00320      IF R4-INFO (X1) = SPACES                                     EL343
00321         MOVE R5-INFO (X1) TO R4-INFO (X1)                         EL343
00322         MOVE SPACES       TO R5-INFO (X1).                        EL343
00323                                                                   EL343
00324      GO TO 4000-READ-IT.                                          EL343
00325  EJECT                                                            EL343
00326  5800-GEN-ROLX-RTN.                                               EL343
00327                                                                   EL343
00328      MOVE    SPACE-NP     TO   P-CCSW.                            EL343
00329      MOVE    ROLX-1       TO   P-LN.                              EL343
00330      PERFORM 8800-PRT-RTN THRU 8999-PRT-XIT.                      EL343
00331      MOVE    SPACE-2      TO   P-CCSW.                            EL343
00332      MOVE    ROLX-2       TO   P-LN.                              EL343
00333      PERFORM 8800-PRT-RTN THRU 8999-PRT-XIT.                      EL343
00334      MOVE    SPACE-2      TO   P-CCSW.                            EL343
00335      MOVE    ROLX-3       TO   P-LN.                              EL343
00336      PERFORM 8800-PRT-RTN THRU 8999-PRT-XIT.                      EL343
00337      MOVE    SPACE-2      TO   P-CCSW.                            EL343
00338      MOVE    ROLX-4       TO   P-LN.                              EL343
00339      PERFORM 8800-PRT-RTN THRU 8999-PRT-XIT.                      EL343
00340      MOVE    SPACE-2      TO   P-CCSW.                            EL343
00341      MOVE    ROLX-5       TO   P-LN.                              EL343
00342      PERFORM 8800-PRT-RTN THRU 8999-PRT-XIT.                      EL343
00343      MOVE    SPACE-2      TO   P-CCSW.                            EL343
00344      MOVE    ROLX-6       TO   P-LN.                              EL343
00345      PERFORM 8800-PRT-RTN THRU 8999-PRT-XIT.                      EL343
00346      ADD     +1           TO   ALIGN-CTR.                         EL343
00347                                                                   EL343
00348      IF ALIGN-CTR LESS THAN MAX-ALIGN-CTR                         EL343
00349         GO TO 5800-GEN-ROLX-RTN.                                  EL343
00350                                                                   EL343
00351      MOVE SPACES TO ROLODEX-AREA.                                 EL343
00352      MOVE K1     TO X1.                                           EL343
00353                                                                   EL343
00354  5999-GEN-ROLX-XIT.  EXIT.                                        EL343
00355  EJECT                                                            EL343
00356  8800-PRT-RTN.                                                    EL343
00357                                                                   EL343
00358      MOVE 'P'     TO DTE-PRT-OPT.                                 EL343
00359      MOVE P-CCSW  TO X P-CTL.                                     EL343
00360      MOVE P-LN    TO P-DATA.                                      EL343
00361      MOVE SPACE-1 TO P-REC.                                       EL343
00362                                                                   EL343
00363  8900-PRT-COPY.                                                   EL343
00364                    COPY ELCPRT2X.                                    CL**2
00365  8999-PRT-XIT.  EXIT.                                             EL343
00366  EJECT                                                            EL343
00367  9000-EOF.                                                        EL343
00368      PERFORM 5800-GEN-ROLX-RTN THRU 5999-GEN-ROLX-XIT.            EL343
00369  9999-EOJ    SECTION.                                             EL343
00370                                                                   EL343
00371                                                                   EL343
00372  ABEND-PGM SECTION.                                               EL343
00373                     COPY ELCABEND.                                   CL**4
