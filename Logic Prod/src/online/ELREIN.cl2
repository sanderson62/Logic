00001  ID DIVISION.                                                     04/18/98
00002                                                                   ELREIN
00003  PROGRAM-ID.   ELREIN.                                               LV007
00004 *              PROGRAM CONVERTED BY                               ELREIN
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ELREIN
00006 *              CONVERSION DATE 03/05/96 16:21:39.                 ELREIN
00007 *                            VMOD=2.008                           ELREIN
00008 *                                                                 ELREIN
00009 *AUTHOR.     LOGIC, INC.                                          ELREIN
00010 *            DALLAS, TEXAS.                                       ELREIN
00011                                                                   ELREIN
00012 *DATE-COMPILED.                                                   ELREIN
00013                                                                   ELREIN
00014 *SECURITY.   *****************************************************ELREIN
00015 *            *                                                   *ELREIN
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELREIN
00017 *            *                                                   *ELREIN
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELREIN
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELREIN
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ELREIN
00021 *            *                                                   *ELREIN
00022 *            *****************************************************ELREIN
00023                                                                   ELREIN
00024 *REMARKS.                                                         ELREIN
00025                                                                   ELREIN
00026 *        THIS SUBROUTINE IS USED TO TEST REINSURANCE TABLES.      ELREIN
00027                                                                   ELREIN
00028      EJECT                                                        ELREIN
00029  ENVIRONMENT DIVISION.                                            ELREIN
00030  DATA DIVISION.                                                   ELREIN
00031                                                                   ELREIN
00032  WORKING-STORAGE SECTION.                                         ELREIN
00033  77  FILLER   PIC X(32) VALUE '********************************'. ELREIN
00034  77  FILLER   PIC X(32) VALUE '**  ELREIN  WORKING STORAGE   **'. ELREIN
00035  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.008 *********'. ELREIN
00036                                                                   ELREIN
00037  01  ACCESS-KEYS.                                                 ELREIN
00038      12  ELCNTL-KEY.                                              ELREIN
00039          16  ENTL-COMP-ID            PIC X(3).                    ELREIN
00040          16  ENTL-REC-TYPE           PIC X.                       ELREIN
00041          16  ENTL-ACCESS             PIC X(4).                    ELREIN
00042          16  ENTL-SEQ-NO             PIC S9(4)    COMP.           ELREIN
00043                                                                   ELREIN
00044      12  ERREIN-KEY.                                              ELREIN
00045          16  REIN-COMP-CD            PIC X.                       ELREIN
00046          16  REIN-TYPE               PIC X.                       ELREIN
00047          16  REIN-TABLE-CO.                                       ELREIN
00048              20  REIN-CODE-1         PIC X.                       ELREIN
00049              20  REIN-CODE-2         PIC X.                       ELREIN
00050              20  REIN-CODE-3         PIC X.                       ELREIN
00051          16  REIN-TABLE-SUB          PIC XXX.                     ELREIN
00052                                                                   ELREIN
00053      12  WS-ACCESS.                                               ELREIN
00054          16  FILLER                  PIC XX      VALUE SPACES.    ELREIN
00055          16  WS-BEN-CD               PIC XX.                      ELREIN
00056                                                                   ELREIN
00057      12  WS-SWITCHES.                                             ELREIN
00058          16  BEN-SEARCH-SW           PIC X       VALUE SPACES.    ELREIN
00059              88  NO-BENEFIT-FOUND                VALUE 'N'.       ELREIN
00060                                                                   ELREIN
00061          16  AH-BENEFIT-ERROR        PIC X       VALUE SPACE.     ELREIN
00062              88  AH-BENEFIT-FOUND                VALUE ' '.       ELREIN
00063              88  NO-AH-BENEFIT-MATCH             VALUE '1'.       ELREIN
00064                                                                   ELREIN
00065          16  LF-BENEFIT-ERROR        PIC X       VALUE SPACE.     ELREIN
00066              88  LF-BENEFIT-FOUND                VALUE ' '.       ELREIN
00067              88  NO-LF-BENEFIT-MATCH             VALUE '1'.       ELREIN
00068                                                                   ELREIN
00069  01  MISC.                                                        ELREIN
00070      12  SUB                         PIC 9999    VALUE ZEROS COMP.   CL**6
00071      12  SUB3                        PIC 9999    VALUE ZEROS COMP.   CL**6
00072                                                                      CL**6
00073      12  ABEND-CODE.                                              ELREIN
00074          16  ABEND-FILE-ID           PIC XX      VALUE ZEROS.     ELREIN
00075          16  ABEND-REASON            PIC XX      VALUE ZEROS.     ELREIN
00076      12  ABEND-OPTION                PIC X       VALUE 'Y'.       ELREIN
00077      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. ELREIN
00078      12  PGM-SUB                     PIC S999    VALUE +090  COMP.ELREIN
00079      12  REIN-RT-SW                  PIC X       VALUE SPACE.     ELREIN
00080                                                                   ELREIN
00081      12  DTE-CLIENT                  PIC  XXX.                       CL**6
00082                                                                      CL**6
00083      12  CLAS-INDEXL                 PIC S9(4) COMP.                 CL**6
00084      12  CLAS-INDEXA                 PIC S9(4) COMP.                 CL**6
00085                                                                      CL**6
00086                                                                      CL**6
00087  01  CLAS-INS-TYPES.                                                 CL**6
00088      12 CLAS-ALL-TYPES       OCCURS 2 TIMES.                         CL**6
00089          16  CLAS-I-BEN              PIC  XX.                        CL**6
00090          16  CLAS-I-AB3.                                             CL**6
00091              20  FILLER              PIC  X.                         CL**6
00092              20  CLAS-I-AB2          PIC  XX.                        CL**6
00093          16  CLAS-I-AB10.                                            CL**6
00094              20  FILLER              PIC  X(9).                      CL**6
00095              20  CLAS-I-REIN-YN      PIC  X.                         CL**6
00096          16  CLAS-I-JOINT            PIC  X.                         CL**6
00097          16  CLAS-I-RL-AH            PIC  X.                         CL**6
00098          16  CLAS-I-CALC-TYPE.                                       CL**6
00099              20  CLAS-I-BAL          PIC  X.                         CL**6
00100          16  CLAS-I-EP               PIC  X.                         CL**6
00101                                                                      CL**6
00102  EJECT                                                               CL**5
00103                                      COPY ELCDATE.                   CL**7
00104  EJECT                                                               CL**5
00105                                      COPY ECSCRT01.               ELREIN
00106                                                                   ELREIN
00107  EJECT                                                               CL**5
00108                                      COPY ELCREIN.                ELREIN
00109                                                                   ELREIN
00110  EJECT                                                               CL**5
00111  LINKAGE SECTION.                                                 ELREIN
00112  01  DFHCOMMAREA                     PIC X(4600).                 ELREIN
00113                                                                   ELREIN
00114 *01 PARMLIST .                                                    ELREIN
00115 *    02  FILLER                      PIC S9(8)   COMP.            ELREIN
00116 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.            ELREIN
00117 *    02  ERREIN-POINTER              PIC S9(8)   COMP.            ELREIN
00118                                                                   ELREIN
00119  EJECT                                                               CL**5
00120                                      COPY ELCCNTL.                ELREIN
00121  EJECT                                                               CL**5
00122                                      COPY ERCREIN.                ELREIN
00123  EJECT                                                               CL**5
00124  PROCEDURE DIVISION.                                              ELREIN
00125                                                                   ELREIN
00126      MOVE DFHCOMMAREA            TO  REINSURANCE-PASS-AREA.       ELREIN
00127                                                                   ELREIN
00128  0000-CALCULATE-REIN-TEST.                                        ELREIN
00129      MOVE SPACES                 TO  CERTIFICATE-RECORD.          ELREIN
00130      MOVE CP-COMPANY-ID          TO  DTE-CLIENT.                  ELREIN
00131      MOVE CP-ISSUE-AGE           TO  CR-AGE.                      ELREIN
00132      MOVE CP-ORIGINAL-TERM       TO  CR-LF-TERM                   ELREIN
00133                                      CR-AH-TERM.                  ELREIN
00134      MOVE CP-TBCOD               TO  CR-REIN-TABLE                ELREIN
00135                                      REIN-SRCH.                   ELREIN
00136                                                                   ELREIN
00137      MOVE CP-EFF-DT              TO  DC-BIN-DATE-1.               ELREIN
00138      MOVE SPACES                 TO  DC-OPTION-CODE.              ELREIN
00139      PERFORM 8510-DATE-RTN  THRU 8510-DATE-RTN-EXIT.              ELREIN
00140      MOVE DC-GREG-DATE-CYMD      TO  CR-DT.                          CL**2
00141                                                                   ELREIN
00142      MOVE CP-LF-TYPE             TO  CR-LFTYP.                    ELREIN
00143      MOVE CP-AH-TYPE             TO  CR-AHTYP.                    ELREIN
00144      MOVE CP-LF-BEN              TO  CR-LFAMT  RS-R-LB.           ELREIN
00145      MOVE CP-LF-PREM             TO  CR-LFPRM  CR-LFPRM-CALC      ELREIN
00146                                      RS-R-LP   RS-R-LPC.          ELREIN
00147      MOVE CP-LFPRM-ALT           TO  CR-LFPRM-ALT.                ELREIN
00148      MOVE CP-LFAMT-ALT           TO  CR-LFAMT-ALT.                ELREIN
00149      MOVE CP-AH-BEN              TO  CR-AHAMT  RS-R-AB.           ELREIN
00150      MOVE CP-AH-PREM             TO  CR-AHPRM  CR-AHPRM-CALC      ELREIN
00151                                      RS-R-AP   RS-R-APC.          ELREIN
00152      MOVE CP-LF-REF              TO  CR-LFRFND CR-LFRFND-CALC.    ELREIN
00153      MOVE CP-AH-REF              TO  CR-AHRFND CR-AHRFND-CALC.    ELREIN
00154                                                                   ELREIN
00155      IF CP-IG-CODE = 'G'                                          ELREIN
00156          MOVE '2'                TO  CR-IND-GRP                   ELREIN
00157        ELSE                                                       ELREIN
00158          MOVE '1'                TO  CR-IND-GRP.                  ELREIN
00159                                                                   ELREIN
00160      MOVE HIGH-VALUES            TO  REIN-HOLD-AREAS.             ELREIN
00161      MOVE SPACES                 TO  REIN-LEVELS-END.             ELREIN
00162                                                                   ELREIN
00163      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              ELREIN
00164                                                                   ELREIN
00165      PERFORM 6000-REINSURE-ROUTINE THRU 6000-EXIT.                ELREIN
00166      GO TO 8200-RETURN.                                           ELREIN
00167                                                                   ELREIN
00168  6000-REINSURE-ROUTINE.                                           ELREIN
00169      MOVE 1                      TO  CLAS-INDEXL.                 ELREIN
00170      MOVE 2                      TO  CLAS-INDEXA.                 ELREIN
00171                                                                   ELREIN
00172      IF CR-LFTYP = ZEROS                                          ELREIN
00173         GO TO 0310-FIND-AH-BENEFIT-RECORD.                        ELREIN
00174                                                                   ELREIN
00175      MOVE CP-COMPANY-ID          TO  ENTL-COMP-ID.                ELREIN
00176      MOVE '4'                    TO  ENTL-REC-TYPE.               ELREIN
00177      MOVE +0                     TO  ENTL-SEQ-NO.                 ELREIN
00178      MOVE CR-LFTYP               TO  WS-BEN-CD.                   ELREIN
00179      MOVE WS-ACCESS              TO  ENTL-ACCESS.                 ELREIN
00180                                                                   ELREIN
00181      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    ELREIN
00182                                                                   ELREIN
00183      IF NO-BENEFIT-FOUND                                          ELREIN
00184          GO TO 0305-LF-BENEFIT-NOT-FOUND.                         ELREIN
00185                                                                   ELREIN
00186      MOVE CF-LF-COVERAGE-TYPE  (SUB3) TO  CLAS-I-RL-AH (1).       ELREIN
00187      MOVE CF-SPECIAL-CALC-CD   (SUB3) TO  CLAS-I-BAL (1).         ELREIN
00188      MOVE CF-JOINT-INDICATOR   (SUB3) TO  CLAS-I-JOINT (1).       ELREIN
00189                                                                   ELREIN
00190      IF CLAS-I-BAL (1) = 'O'                                      ELREIN
00191          MOVE 'B' TO CLAS-I-BAL (1).                              ELREIN
00192                                                                   ELREIN
00193      MOVE CF-BENEFIT-COMMENT (SUB3)   TO  CLAS-I-AB10 (1).        ELREIN
00194                                                                   ELREIN
00195      GO TO 0310-FIND-AH-BENEFIT-RECORD.                           ELREIN
00196                                                                   ELREIN
00197  0305-LF-BENEFIT-NOT-FOUND.                                       ELREIN
00198                                                                   ELREIN
00199      MOVE '1'                    TO  CP-RETURN-CODE.              ELREIN
00200      GO TO 6000-EXIT.                                             ELREIN
00201                                                                   ELREIN
00202  0306-AH-BENEFIT-NOT-FOUND.                                       ELREIN
00203                                                                   ELREIN
00204      MOVE '2'                    TO  CP-RETURN-CODE.              ELREIN
00205      GO TO 6000-EXIT.                                             ELREIN
00206                                                                   ELREIN
00207  EJECT                                                               CL**5
00208  0310-FIND-AH-BENEFIT-RECORD.                                     ELREIN
00209                                                                   ELREIN
00210      IF CR-AHTYP = ZEROS                                          ELREIN
00211         GO TO 0400-READ-REIN.                                     ELREIN
00212                                                                   ELREIN
00213      MOVE CP-COMPANY-ID          TO  ENTL-COMP-ID.                ELREIN
00214      MOVE '5'                    TO  ENTL-REC-TYPE.               ELREIN
00215      MOVE +0                     TO  ENTL-SEQ-NO.                 ELREIN
00216      MOVE CR-AHTYP               TO  WS-BEN-CD.                   ELREIN
00217      MOVE WS-ACCESS              TO  ENTL-ACCESS.                 ELREIN
00218                                                                   ELREIN
00219      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    ELREIN
00220                                                                   ELREIN
00221      IF NO-BENEFIT-FOUND                                          ELREIN
00222          GO TO 0306-AH-BENEFIT-NOT-FOUND.                         ELREIN
00223                                                                   ELREIN
00224      MOVE CF-LF-COVERAGE-TYPE  (SUB3)    TO  CLAS-I-RL-AH (2).    ELREIN
00225      MOVE CF-SPECIAL-CALC-CD   (SUB3)    TO  CLAS-I-BAL (2).      ELREIN
00226      MOVE CF-JOINT-INDICATOR   (SUB3)    TO  CLAS-I-JOINT (2).    ELREIN
00227                                                                   ELREIN
00228      IF CLAS-I-BAL (2) = 'O'                                      ELREIN
00229          MOVE 'B'                TO  CLAS-I-BAL (2).              ELREIN
00230                                                                   ELREIN
00231      MOVE CF-BENEFIT-COMMENT (SUB3)      TO  CLAS-I-AB10 (2).     ELREIN
00232                                                                   ELREIN
00233  0400-READ-REIN.                                                  ELREIN
00234                                                                      CL**4
00235      MOVE CP-COMPANY-CD          TO  REIN-COMP-CD.                ELREIN
00236      MOVE 'A'                    TO  REIN-TYPE.                   ELREIN
00237      MOVE CR-REIN-TABLE          TO  REIN-TABLE-CO.               ELREIN
00238      MOVE LOW-VALUES             TO  REIN-TABLE-SUB.              ELREIN
00239                                                                   ELREIN
00240      PERFORM 9200-READ-REIN-TABLE THRU 9200-EXIT.                 ELREIN
00241                                                                   ELREIN
00242      PERFORM REINSURE-CALC THRU REINSURE-CALC-X                   ELREIN
00243              VARYING SUB1 FROM +1 BY +1 UNTIL                     ELREIN
00244              RE-REI-COMP (SUB1) = SPACES.                         ELREIN
00245                                                                   ELREIN
00246  6000-EXIT.                                                       ELREIN
00247      EXIT.                                                        ELREIN
00248                                                                   ELREIN
00249  EJECT                                                            ELREIN
00250  7200-FIND-BENEFIT.                                               ELREIN
00251                                                                   ELREIN
00252      MOVE 'N'                    TO  BEN-SEARCH-SW.               ELREIN
00253                                                                   ELREIN
00254      EXEC CICS HANDLE CONDITION                                   ELREIN
00255          ENDFILE   (7200-EXIT)                                    ELREIN
00256          NOTFND    (7200-EXIT)                                    ELREIN
00257          END-EXEC.                                                ELREIN
00258                                                                   ELREIN
00259      EXEC CICS READ                                               ELREIN
00260          DATASET     ('ELCNTL')                                   ELREIN
00261          SET         (ADDRESS OF CONTROL-FILE)                    ELREIN
00262          RIDFLD      (ELCNTL-KEY)                                 ELREIN
00263          GTEQ                                                     ELREIN
00264          END-EXEC.                                                ELREIN
00265                                                                   ELREIN
00266      IF ENTL-COMP-ID  NOT = CF-COMPANY-ID   OR                    ELREIN
00267         ENTL-REC-TYPE NOT = CF-RECORD-TYPE                        ELREIN
00268            GO TO 7200-EXIT.                                       ELREIN
00269                                                                   ELREIN
00270      MOVE +1                     TO SUB3.                         ELREIN
00271                                                                   ELREIN
00272  7200-BENEFIT-LOOP.                                               ELREIN
00273                                                                   ELREIN
00274      IF SUB3 GREATER THAN +8                                      ELREIN
00275          GO TO 7200-EXIT.                                         ELREIN
00276                                                                   ELREIN
00277      IF CF-BENEFIT-CODE (SUB3) EQUAL WS-BEN-CD                    ELREIN
00278          MOVE 'Y'                TO BEN-SEARCH-SW                 ELREIN
00279              GO TO 7200-EXIT.                                     ELREIN
00280                                                                   ELREIN
00281      ADD +1                      TO SUB3.                         ELREIN
00282      GO TO 7200-BENEFIT-LOOP.                                     ELREIN
00283                                                                   ELREIN
00284  7200-EXIT.                                                       ELREIN
00285      EXIT.                                                        ELREIN
00286                                                                   ELREIN
00287  EJECT                                                               CL**5
00288  8000-CALC-REIN.                                                  ELREIN
00289                                  COPY ECSRIRT2.                   ELREIN
00290                                                                   ELREIN
00291  EJECT                                                               CL**5
00292  8200-RETURN.                                                     ELREIN
00293      MOVE REINSURANCE-PASS-AREA  TO  DFHCOMMAREA.                 ELREIN
00294                                                                   ELREIN
00295      EXEC CICS RETURN                                             ELREIN
00296          END-EXEC.                                                ELREIN
00297       GOBACK.                                                     ELREIN
00298      EJECT                                                        ELREIN
00299  8500-DATE-CONVERSION SECTION.                                    ELREIN
00300                                                                   ELREIN
00301 *    NOTE ******************************************************* ELREIN
00302 *         *                                                     * ELREIN
00303 *         *  THIS SECTION CALLS THE DATE CONVERSION SUBROUTINE. * ELREIN
00304 *         *                                                     * ELREIN
00305 *         *******************************************************.ELREIN
00306                                                                   ELREIN
00307  8510-DATE-RTN.                                                   ELREIN
00308      EXEC CICS LINK                                               ELREIN
00309          PROGRAM  (LINK-ELDATCV)                                  ELREIN
00310          COMMAREA (DATE-CONVERSION-DATA)                          ELREIN
00311          LENGTH   (DC-COMM-LENGTH)                                ELREIN
00312      END-EXEC.                                                    ELREIN
00313                                                                   ELREIN
00314      IF DC-ERROR-CODE NOT = SPACES                                ELREIN
00315          MOVE '2'                TO  CP-RETURN-CODE.              ELREIN
00316                                                                   ELREIN
00317  8510-DATE-RTN-EXIT.                                              ELREIN
00318      EXIT.                                                        ELREIN
00319                                                                   ELREIN
00320  EJECT                                                               CL**5
00321  9200-READ-REIN-TABLE.                                            ELREIN
00322      EXEC CICS HANDLE CONDITION                                   ELREIN
00323          ENDFILE   (9250-INVALID-TABLE)                           ELREIN
00324          NOTFND    (9250-INVALID-TABLE)                           ELREIN
00325          END-EXEC.                                                ELREIN
00326                                                                   ELREIN
00327      EXEC CICS READ                                               ELREIN
00328          DATASET     ('ERREIN')                                   ELREIN
00329          SET         (ADDRESS OF REINSURANCE-RECORD)              ELREIN
00330          RIDFLD      (ERREIN-KEY)                                 ELREIN
00331          END-EXEC.                                                ELREIN
00332                                                                   ELREIN
00333  9200-EXIT.                                                       ELREIN
00334       EXIT.                                                       ELREIN
00335                                                                   ELREIN
00336  9250-INVALID-TABLE.                                              ELREIN
00337      MOVE '3'                    TO  CP-RETURN-CODE.              ELREIN
00338                                                                      CL**4
00339      GO TO 6000-EXIT.                                             ELREIN
