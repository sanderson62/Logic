00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   EL510
00003  PROGRAM-ID.                 EL510 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL510
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL510
00006 *              CONVERSION DATE 02/19/96 16:48:40.                 EL510
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL510
00008 *                            VMOD=2.008                              CL**3
00009                                                                   EL510
00009                                                                   EL510
00010 *AUTHOR.     LOGIC INC.                                           EL510
00011 *            DALLAS, TEXAS.                                       EL510
00012                                                                   EL510
00013 *DATE-COMPILED.                                                   EL510
00014                                                                   EL510
00015 *SECURITY.   *****************************************************EL510
00016 *            *                                                   *EL510
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL510
00018 *            *                                                   *EL510
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL510
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL510
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL510
00022 *            *                                                   *EL510
00023 *            *****************************************************EL510
00024                                                                   EL510
00025 *REMARKS.                                                         EL510
00026 *      THIS PROGRAM IS USED TO UNLOAD THE ONLINE COMPENSATION     EL510
00027 *      MASTER TO A SEQUENTIAL FILE. THE CONTROL FILE WILL BE      EL510
00028 *      UPDATED TO REFLECT THE DATE THAT THE FILE IS CREATED.      EL510
00029 *      THE FILE WILL NOT BE CREATED IF NO MAINTENANCE HAS BEEN    EL510
00030 *      APPLIED.                                                   EL510
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
061402* 061802                   PEMA  ADD PROCESS FOR PEOPLES
080702* 080702    2002061800008  PEMA  ADD PROCESS FOR SUNFLOWER
061206* 061206  CR2006050500001  PEMA  ADD FIRST PREMIER BANK
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
022616* 022616  CR2016021100002  PEMA  MID MONTH BILLING FOR MACHENS
031102******************************************************************
00031                                                                   EL510
00032  ENVIRONMENT DIVISION.                                            EL510
00033                                                                   EL510
00034  INPUT-OUTPUT SECTION.                                            EL510
00035                                                                   EL510
00036  FILE-CONTROL.                                                    EL510
00037                                                                   EL510
00038      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL510
00039                                                                   EL510
00040      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL510
00041                                                                   EL510
00042      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL510
00043                                                                   EL510
00044      SELECT COMP-MSTR      ASSIGN TO SYS010-UT-FBA1-S-SYS010.     EL510
00045                                                                   EL510
00046      SELECT ERCOMP         ASSIGN TO ERCOMP                       EL510
00047              ORGANIZATION IS INDEXED                              EL510
00048              ACCESS IS DYNAMIC                                    EL510
00049              RECORD KEY IS CO-CONTROL-PRIMARY                     EL510
00050              FILE STATUS IS COMPENSATION-FILE-STATUS.             EL510
00051                                                                   EL510
00052      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL510
00053              ORGANIZATION IS INDEXED                              EL510
00054              ACCESS IS DYNAMIC                                    EL510
00055              RECORD KEY IS CF-CONTROL-PRIMARY                     EL510
00056              FILE STATUS IS CONTROL-FILE-STATUS.                  EL510
00057                                                                   EL510
00058      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL510
00059              ORGANIZATION IS INDEXED                              EL510
00060              ACCESS IS DYNAMIC                                    EL510
00061              RECORD KEY IS RF-CONTROL-PRIMARY                     EL510
00062              FILE STATUS IS DTE-VSAM-FLAGS.                       EL510
00063                                                                   EL510
00064      EJECT                                                        EL510
00065  DATA DIVISION.                                                   EL510
00066                                                                   EL510
00067  FILE SECTION.                                                    EL510
00068                                                                   EL510
00069  FD  PRNTR COPY ELCPRTFD SUPPRESS.                                EL510
00070                                                                   EL510
00071  FD  FICH COPY ELCFCHFD SUPPRESS.                                 EL510
00072                                                                   EL510
00073  FD  DISK-DATE COPY ELCDTEFD SUPPRESS.                            EL510
00074                                                                   EL510
00075  FD  ELREPT    COPY ELCRPTFD SUPPRESS.                            EL510
00076                                                                   EL510
00077                            COPY ELCREPT.                          EL510
00078      EJECT                                                        EL510
00079  FD  COMP-MSTR                                                    EL510
00080      BLOCK CONTAINS 0 RECORDS
00081      RECORDING MODE F.                                            EL510
00082  01  COMP-MSTR-REC         PIC X(700).                            EL510
00083                                                                   EL510
00084  FD  ERCOMP.                                                      EL510
00085                            COPY ERCCOMP.                          EL510
00086                                                                   EL510
00087      EJECT                                                        EL510
00088  FD  ELCNTL.                                                      EL510
00089                            COPY ELCCNTL.                          EL510
00090                                                                   EL510
00091      EJECT                                                        EL510
00092  WORKING-STORAGE SECTION.                                         EL510
00093  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL510
00094                                                                   EL510
00095  77  FILLER  PIC X(32) VALUE '********************************'.  EL510
00096  77  FILLER  PIC X(32) VALUE '      EL510 WORKING-STORAGE     '.  EL510
00097  77  FILLER  PIC X(32) VALUE '******** VMOD=2.008 ************'.     CL**3
00098                                                                   EL510
00099  01  WORK-AREAS.                                                  EL510
00100      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL510
00101      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL510
00102      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL510
00103      12  WS-CURRENT-BIN-DT      PIC XX.                           EL510
00104      12  WS-RETURN-CODE         PIC S9(4) COMP.                   EL510
00105      12  WS-ABEND-MESSAGE       PIC X(80).                        EL510
00106      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL510
00107      12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        EL510
00108      12  COMPENSATION-OPEN           PIC X   VALUE SPACES.        EL510
00109      12  COMPENSATION-FILE-STATUS    PIC XX  VALUE ZEROS.         EL510
00110      12  CMP REDEFINES COMPENSATION-FILE-STATUS.                  EL510
00111          16  CMP1               PIC X.                            EL510
00112          16  CMP2               PIC X.                            EL510
00113      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL510
00114                                                                   EL510
00115      12  ERROR-SW               PIC X    VALUE SPACE.             EL510
00116          88  ERROR-OCCURRED              VALUE 'E'.               EL510
00117          88  NO-ERRORS                   VALUE ' '.               EL510
00118      12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.          EL510
00119                                                                   EL510
00120  01  DTE-INTERFACE-CODES.                                         EL510
00121      05  X                 PIC X           VALUE SPACE.           EL510
00122      05  PGM-SUB           PIC S9(4)  COMP VALUE +510.            EL510
00123      05  ABEND-CODE        PIC 9999        VALUE ZERO.            EL510
00124      05  ABEND-OPTION      PIC X           VALUE SPACE.           EL510
00125      05  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL510'.         EL510
00126                                                                   EL510
00127      05 WS-IN                   PIC 9(5) VALUE ZEROS.             EL510
00128      05 WS-OUT                  PIC 9(5) VALUE ZEROS.             EL510
00129                                                                   EL510
00130      05  WS-CONTROL-PRIMARY.                                      EL510
00131          10  WS-COMPANY-CD      PIC X.                            EL510
00132          10  WS-CARRIER         PIC X.                            EL510
00133          10  WS-COMPANY         PIC X(3).                         EL510
00134          10  WS-RESP            PIC X(6).                         EL510
00135          10  WS-ACCT            PIC X(10).                        EL510
00136          10  WS-TYPE            PIC X.                            EL510
00137                                                                   EL510
00138     05 END-OF-PROCESS-SWT        PIC X VALUE 'N'.                 EL510
00139        88 END-OF-PROCESS         VALUE 'Y'.                       EL510
00140        88 NOT-END-OF-PROCESS     VALUE 'N'.                       EL510
00141                                                                   EL510
00142  01  COMP-3-WORK-AREA.                                            EL510
00143      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL510
00144      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL510
00145      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL510
00146                                                                   EL510
00147      EJECT                                                        EL510
00148                             COPY ELCDATE.                         EL510
00149                                                                   EL510
00150      EJECT                                                        EL510
00151                                                                   EL510
00152  01  WS-HEADING1.                                                 EL510
00153      05  FILLER                      PIC X(51)       VALUE '1'.   EL510
00154      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL510
00155          'COMPENSATION FILE UNLOAD'.                              EL510
00156      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL510  '.    EL510
00157                                                                   EL510
00158  01  WS-HEADING2.                                                 EL510
00159      05  FILLER                      PIC X(46)       VALUE SPACES.EL510
00160      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL510
00161      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL510
00162      05  FILLER                      PIC X           VALUE SPACES.EL510
00163                                                                   EL510
00164  01  WS-HEADING3.                                                 EL510
00165      05  FILLER                      PIC X(51)       VALUE SPACES.EL510
00166      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL510
00167      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL510
00168      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL510
00169      05  FILLER                      PIC X(11)       VALUE SPACES.EL510
00170                                                                   EL510
00171  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL510
00172                                                                   EL510
00173  01  WS-DETAIL1.                                                  EL510
00174      05  FILLER             PIC X.                                EL510
00175      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL UNLOAD '. EL510
00176      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL510
00177      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL510
00178      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL510
00179      05  FILLER             PIC X(10) VALUE ' RECORDS  '.         EL510
00180                                                                   EL510
00181                             COPY ELCDTECX.                        EL510
00182                                                                   EL510
00183                             COPY ELCDTEVR.                        EL510
00184                                                                   EL510
00185      EJECT                                                        EL510
00186  PROCEDURE DIVISION.                                              EL510
00187                                                                   EL510
00188  0000-LOAD-DATE-WS. COPY ELCDTERX SUPPRESS.                       EL510
00189                                                                   EL510
00190  000-MAINLINE.                                                    EL510
00191      PERFORM 100-INITIALIZE  THRU 100-EXIT.                       EL510
PEMTST     DISPLAY '*' COMPANY-NAME '*'.
00192                                                                   EL510
00193      IF NO-ERRORS                                                 EL510
00194          PERFORM 300-UNLOAD-COMPENSATION THRU 300-EXIT.           EL510
00195                                                                   EL510
00196      MOVE RECORD-COUNT TO D-RECORD-COUNT.                         EL510
00197      MOVE WS-DETAIL1   TO PRT.                                    EL510
00198      PERFORM WRITE-A-LINE.                                        EL510
00199                                                                   EL510
00200      PERFORM 800-FINALIZE THRU 800-EXIT.                          EL510
00201                                                                   EL510
00202      PERFORM 150-READ-CNTL THRU 150-EXIT.                         EL510
00203                                                                   EL510
00204  000-MAINLINE-EXIT.                                               EL510
00205      GOBACK.                                                      EL510
00206                                                                   EL510
00207  100-INITIALIZE.                                                  EL510
00208      MOVE ZEROS                  TO WS-RETURN-CODE.               EL510
00209      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL510
00210      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL510
00211      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL510
00212                                                                   EL510
00213      OPEN INPUT  ERCOMP                                           EL510
00214           OUTPUT PRNTR.                                           EL510
00215                                                                   EL510
00216      IF COMPENSATION-FILE-STATUS = '00' OR '97'                   EL510
00217          NEXT SENTENCE                                            EL510
00218        ELSE                                                       EL510
00219          MOVE COMPENSATION-FILE-STATUS  TO WS-ABEND-FILE-STATUS   EL510
00220          MOVE ' COMP OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL510
00221          GO TO ABEND-PGM.                                         EL510
00222                                                                   EL510
00223      MOVE WS-CURRENT-DATE TO DC-GREG-DATE-1-EDIT.                 EL510
00224      MOVE '2'             TO DC-OPTION-CODE.                      EL510
00225      PERFORM 310-DATE-RTN THRU 310-EXIT.                          EL510
00226      MOVE DC-BIN-DATE-1   TO WS-CURRENT-BIN-DT.                   EL510
00227                                                                   EL510
00228      MOVE LOW-VALUES TO WS-CONTROL-PRIMARY.                       EL510
00229      MOVE DTE-CLASIC-COMPANY-CD TO WS-COMPANY-CD.                 EL510
00230                                                                   EL510
00231  100-EXIT.                                                        EL510
00232      EXIT.                                                        EL510
00233      EJECT                                                        EL510
00234  150-READ-CNTL.                                                   EL510
00235      IF EP-SW = '1'  OR  '2'                                      EL510
00236          OPEN I-O ELCNTL                                          EL510
00237      ELSE                                                         EL510
00238          OPEN INPUT ELCNTL.                                       EL510
00239                                                                   EL510
00240      IF CONTROL-FILE-STATUS = '00' OR '97'                        EL510
00241          NEXT SENTENCE                                            EL510
00242        ELSE                                                       EL510
00243          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL510
00244          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL510
00245          GO TO ABEND-PGM.                                         EL510
00246                                                                   EL510
00247      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL510
00248      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL510
00249      MOVE '1'                    TO CF-RECORD-TYPE.               EL510
00250      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL510
00251                                                                   EL510
00252      READ ELCNTL.                                                 EL510
00253                                                                   EL510
00254      IF CONTROL-FILE-STATUS NOT = ZERO                            EL510
00255          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL510
00256          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL510
00257          GO TO ABEND-PGM.                                         EL510
00258                                                                   EL510
00259      IF EP-SW = '1'  OR  '2'                                      EL510
00260         MOVE WS-CURRENT-BIN-DT   TO CF-COMPENSATION-MSTR-CREATE-DTEL510
00261         MOVE LOW-VALUES          TO CF-COMPENSATION-MSTR-MAINT-DT EL510
00262      ELSE                                                         EL510
00263          GO TO 150-EXIT.                                          EL510
00264                                                                   EL510
00265      REWRITE CONTROL-FILE.                                        EL510
00266                                                                   EL510
00267      IF CONTROL-FILE-STATUS NOT = ZERO                            EL510
00268          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL510
00269          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL510
00270          GO TO ABEND-PGM.                                         EL510
00271                                                                   EL510
00272      CLOSE ELCNTL.                                                EL510
00273                                                                   EL510
00274      IF CONTROL-FILE-STATUS NOT = ZERO                            EL510
00275          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL510
00276          MOVE ' CNTL CLOSE ERROR- ' TO WS-ABEND-MESSAGE           EL510
00277          GO TO ABEND-PGM.                                         EL510
00278                                                                   EL510
00279  150-EXIT.  EXIT.                                                 EL510
00280      EJECT                                                        EL510
00281                                                                   EL510
00282  300-UNLOAD-COMPENSATION.                                         EL510
00283      OPEN OUTPUT COMP-MSTR.                                       EL510
00284      MOVE 'Y'                    TO COMPENSATION-OPEN.            EL510
00285      MOVE WS-CONTROL-PRIMARY     TO CO-CONTROL-PRIMARY.           EL510
00286                                                                   EL510
00287  305-START.                                                       EL510
00288      START ERCOMP KEY NOT LESS CO-CONTROL-PRIMARY.                EL510
00289                                                                   EL510
00290      IF COMPENSATION-FILE-STATUS  = '10' OR '23'                  EL510
00291           GO TO 300-EXIT.                                         EL510
00292                                                                   EL510
00293      IF COMPENSATION-FILE-STATUS NOT = ZERO                       EL510
00294          MOVE COMPENSATION-FILE-STATUS  TO WS-ABEND-FILE-STATUS   EL510
00295          MOVE ' COMP START ERROR- ' TO WS-ABEND-MESSAGE           EL510
00296          GO TO ABEND-PGM.                                         EL510
00297                                                                   EL510
00298  310-READNEXT.                                                    EL510
00299      READ ERCOMP  NEXT RECORD.                                    EL510
00300                                                                   EL510
00301      IF CMP1 = '1'                                                EL510
00302              GO TO 300-EXIT.                                      EL510
00303                                                                   EL510
00304      IF COMPENSATION-FILE-STATUS NOT = ZEROS                      EL510
00305          MOVE 'ERROR ON READ  ' TO WS-ABEND-MESSAGE               EL510
00306          MOVE COMPENSATION-FILE-STATUS TO  WS-ABEND-FILE-STATUS   EL510
00307          GO TO ABEND-PGM.                                         EL510
00308                                                                   EL510
00309      IF DTE-CLASIC-COMPANY-CD NOT = CO-COMPANY-CD                 EL510
00310          GO TO 300-EXIT.                                          EL510
00311                                                                   EL510
CIDMOD*************************************************************
CIDMOD* THIS LOGIC WAS ADDED TO ACCOMMODATE THE EXECUTION OF A
CIDMOD* "MID MONTH" BILLING (EL562) FOR COMMERCIAL FEDERAL
CIDMOD*************************************************************
CIDMOD     IF DTE-CLIENT = 'CID'
              evaluate company-name
                 when '      COMMERCIAL FEDERAL'
                    IF CO-RESP-NO = '0000713100'
                       CONTINUE
                    ELSE
                       GO TO 310-READNEXT
                    END-IF
                 when '        PEOPLES TRUST'
                    IF CO-RESP-NO = '0000845800'
                       CONTINUE
                    ELSE
                       GO TO 310-READNEXT
                    END-IF
                 when '        SUNFLOWER BANK'
                    IF CO-RESP-NO = '0000681700'
                       CONTINUE
                    ELSE
                       GO TO 310-READNEXT
                    END-IF
                 when '      FIRST PREMIER BANK'
                    IF CO-RESP-NO = '0001107200'
                       CONTINUE
                    ELSE
                       GO TO 310-READNEXT
                    END-IF
022616           when '        MACHENS CONLEY  '
                    DISPLAY ' FOUND MACHENS CONLEY '
022616              IF (CO-ACCOUNT = '0000311700' OR '0000312400' OR
022616                 '0000314000' OR '0000314100' OR '0000314200' OR
022616                 '0000314400' OR '0000314600' OR '0000314700' OR
022616                 '0000314800' OR '0000457900' OR '0000652900' OR
022616                 '0000763300' OR '0000763600' OR '0000763800' OR
022616                 '0000836500')
                           OR
                       (CO-RESP-NO = '0000566100' 
                        AND CO-ACCOUNT = LOW-VALUES)
022616                 CONTINUE
022616              ELSE
022616                 GO TO 310-READNEXT
022616              END-IF
              END-EVALUATE
           END-IF
CIDMOD*************************************************************
00312      IF DTE-SYS-G-AR-USED NOT = '1'                               EL510
00313          MOVE CO-CURRENT-MONTHLY-TOTALS                           EL510
00314                                   TO CO-MONTHLY-TOTALS            EL510
00315          MOVE CO-CURRENT-AGING-TOTALS                             EL510
00316                                   TO CO-AGING-TOTALS              EL510
00317          MOVE CO-CURRENT-YTD-TOTALS                               EL510
00318                                   TO CO-YTD-TOTALS                EL510
00319          MOVE CO-CURRENT-LAST-STMT-DT                             EL510
00320                                   TO CO-LAST-STMT-DT
080612         move co-current-ov120   to co-ov120
080612     end-if
00321                                                                   EL510
00322      MOVE COMPENSATION-MASTER     TO COMP-MSTR-REC.               EL510
00323                                                                   EL510
00324      WRITE COMP-MSTR-REC.                                         EL510
00325                                                                   EL510
00326      ADD 1  TO RECORD-COUNT.                                      EL510
00327                                                                   EL510
00328      GO TO 310-READNEXT.                                          EL510
00329                                                                   EL510
00330  300-EXIT.                                                        EL510
00331      EXIT.                                                        EL510
00332                                                                   EL510
00333      EJECT                                                        EL510
00334                                                                   EL510
00335  310-DATE-RTN.                                                    EL510
00336      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL510
00337                                                                   EL510
00338      IF DC-ERROR-CODE NOT = SPACE                                 EL510
00339          MOVE  ZEROS TO DC-BIN-DATE-1                             EL510
00340          ADD 1  TO DATE-ERROR-COUNT.                              EL510
00341                                                                   EL510
00342  310-EXIT.                                                        EL510
00343      EXIT.                                                        EL510
00344                                                                   EL510
00345      EJECT                                                        EL510
00346  800-FINALIZE.                                                    EL510
00347      IF COMPENSATION-OPEN = 'Y'                                   EL510
00348         CLOSE COMP-MSTR.                                          EL510
00349                                                                   EL510
00350      CLOSE PRNTR                                                  EL510
00351            ERCOMP.                                                EL510
00352                                                                   EL510
00353      EJECT                                                        EL510
00354  800-CLOSE-OTHER. COPY ELCPRTCX.                                  EL510
00355                                                                   EL510
00356  800-EXIT.                                                        EL510
00357      EXIT.                                                        EL510
00358      EJECT                                                        EL510
00359  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL510
00360                                                                   EL510
00361  WRITE-HEADINGS SECTION.                                          EL510
00362 ***************************************************************** EL510
00363 *                                                               * EL510
00364 *                            ELCWHS1.                           * EL510
00365 *                            VMOD=2.001                         * EL510
00366 *                                                               * EL510
00367 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL510
00368 *****************************************************************.EL510
00369  WHS-010.                                                         EL510
00370      ADD +1  TO  WS-PAGE.                                         EL510
00371      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL510
00372      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL510
00373      MOVE ZERO                   TO  WS-LINE-COUNT.               EL510
00374                                                                   EL510
00375      MOVE WS-HEADING1            TO  PRT.                         EL510
00376      MOVE '1'                    TO  X.                           EL510
00377      PERFORM WRITE-PRINTER.                                       EL510
00378                                                                   EL510
00379      MOVE WS-HEADING2            TO  PRT.                         EL510
00380      MOVE ' '                    TO  X.                           EL510
00381      PERFORM WRITE-PRINTER.                                       EL510
00382                                                                   EL510
00383      MOVE WS-HEADING3            TO  PRT.                         EL510
00384      MOVE ' '                    TO  X.                           EL510
00385      PERFORM WRITE-PRINTER.                                       EL510
00386                                                                   EL510
00387      MOVE WS-HEADING4            TO  PRT.                         EL510
00388      MOVE ' '                    TO  X.                           EL510
00389      PERFORM WRITE-PRINTER.                                       EL510
00390                                                                   EL510
00391      MOVE +4 TO WS-LINE-COUNT.                                    EL510
00392                                                                   EL510
00393  WHS-020.                                                         EL510
00394 ***************************************************************** EL510
00395 *                                                               * EL510
00396 *                            ELCWHS2.                           * EL510
00397 *                            VMOD=2.001                         * EL510
00398 *****************************************************************.EL510
00399      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         EL510
00400      MOVE '-'                    TO  P-CTL.                          CL**2
00401                                                                   EL510
00402  WHS-EXIT.                                                        EL510
00403      EXIT.                                                        EL510
00404                                                                   EL510
00405                                                                   EL510
00406  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL510
00407  WPS-020. COPY ELCPRT2X.                                          EL510
00408                                                                   EL510
00409  ABEND-PGM  SECTION.  COPY ELCABEND SUPPRESS.                     EL510
