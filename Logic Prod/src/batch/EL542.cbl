00001  IDENTIFICATION DIVISION.                                         04/14/98
00002                                                                   EL542
00003  PROGRAM-ID.                 EL542 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL542
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL542
00006 *              CONVERSION DATE 02/12/96 16:44:13.                 EL542
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL542
00008 *                            VMOD=2.004.                             CL**2
00009                                                                   EL542
00010 *AUTHOR.        LOGIC, INC.                                       EL542
00011 *               DALLAS, TEXAS.                                    EL542
00012                                                                   EL542
00013 *DATE-COMPILED.                                                   EL542
00014                                                                   EL542
00015 *SECURITY.   *****************************************************EL542
00016 *            *                                                   *EL542
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL542
00018 *            *                                                   *EL542
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL542
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL542
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL542
00022 *            *                                                   *EL542
00023 *            *****************************************************EL542
00024                                                                   EL542
00025 *REMARKS.                                                         EL542
00026 *    BUILDS THE MONTH-END-BALANCES RECORD USED BY SEVERAL         EL542
00027 *    PROGRAMS TO HOLD MONTH-END TOTALS OF VARIOUS TYPES           EL542
00028 *    AND ULTIMATELY TO PRINT THE MONTH-END BALANCES,              EL542
00029 *    DISCREPANCIES AND TASK DURATION ANALYSIS.                    EL542
00030  EJECT                                                            EL542
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
00031  ENVIRONMENT DIVISION.                                            EL542
00032  INPUT-OUTPUT SECTION.                                            EL542
00033  FILE-CONTROL.                                                    EL542
00034                                                                   EL542
00035      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL542
00036                                                                   EL542
00037      SELECT ERMEBL                                                EL542
00038              ASSIGN SYS024-FBA1-ERMEBL                            EL542
00039              ORGANIZATION INDEXED                                 EL542
00040              ACCESS DYNAMIC                                       EL542
00041              RECORD KEY ME-CONTROL-PRIMARY                        EL542
00042              FILE STATUS ERMEBL-FILE-STATUS.                      EL542
00043  EJECT                                                            EL542
00044  DATA DIVISION.                                                   EL542
00045  FILE SECTION.                                                    EL542
00046                                                                   EL542
00047  FD  DISK-DATE                     COPY ELCDTEFD.                 EL542
00048  EJECT                                                            EL542
00049  FD  ERMEBL.                                                      EL542
00050  COPY ERCMEBL.                                                    EL542
00051      EJECT                                                        EL542
00052  WORKING-STORAGE SECTION.                                         EL542
00053  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL542
00054  77  FILLER  PIC X(32)   VALUE '********************************'.EL542
00055  77  FILLER  PIC X(32)   VALUE '*     EL542  WORKING STORAGE   *'.EL542
00056  77  FILLER  PIC X(32)   VALUE '*********** VM 2.004 ***********'.   CL**2
00057                                                                   EL542
00058  77  WS-ZERO                     PIC S9 COMP-3  VALUE +0.         EL542
00059  77  WS-RETURN-CODE              PIC S9999      VALUE +0 COMP.    EL542
00060  77  WS-ABEND-FILE-STATUS        PIC XX         VALUE ZERO.       EL542
00061  77  WS-ABEND-MESSAGE            PIC X(80)      VALUE SPACES.     EL542
00062                                                                   EL542
00063  01  PGM-SUB                     PIC S999 COMP VALUE +542.        EL542
00064  01  MONTH-END-DATA.                                              EL542
00065      12  ME-START-DATE.                                           EL542
00066          16  ME-START-MO         PIC 99.                          EL542
00067          16  FILLER              PIC X.                           EL542
00068          16  ME-START-DA         PIC 99.                          EL542
00069          16  FILLER              PIC X.                           EL542
00070          16  ME-START-YR         PIC 99.                          EL542
00071      12  ME-CNDS-DATE            PIC 9(6).                        EL542
00072      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL542
00073          16  ME-CNDS-MO          PIC 99.                          EL542
00074          16  ME-CNDS-DA          PIC 99.                          EL542
00075          16  ME-CNDS-YR          PIC 99.                          EL542
00076      12  ME-START-TIME           PIC 9(6).                        EL542
00077      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL542
00078          88  ME-DO-UPDATE        VALUE 'Y'.                       EL542
00079          88  ME-NO-UPDATE        VALUE 'N'.                       EL542
00080      12  ERMEBL-FILE-STATUS      PIC XX.                          EL542
00081      12  MONTH-END-MOYR          PIC 9999 COMP.                   EL542
00082      12  NEW-ME-REC-SW           PIC 9 VALUE ZERO.                EL542
00083                                                                   EL542
00084  EJECT                                                            EL542
00085  COPY ELCDTECX.                                                   EL542
00086  EJECT                                                            EL542
00087  COPY ELCDTEVR.                                                   EL542
00088                                                                   EL542
00089  PROCEDURE DIVISION.                                              EL542
00090                                                                   EL542
00091  CAPTURE-START.                                                   EL542
00092      OPEN I-O ERMEBL.                                             EL542
00093                                                                   EL542
00094      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        EL542
00095          NEXT SENTENCE                                            EL542
00096        ELSE
080202         DISPLAY 'OPEN ERROR ON ERMEBL - FILE STATUS IS '
080202                                 ERMEBL-FILE-STATUS               EL542
00097          MOVE 'N'                TO ME-UPDATE-FLAG.               EL542
00098                                                                   EL542
00099  0000-DATE-CARD-READ SECTION.    COPY ELCDTERX SUPPRESS.          EL542
00100                                                                   EL542
00101      MOVE WS-TIME                TO ME-START-TIME.                EL542
00102      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL542
00103      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL542
00104      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL542
00105      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL542
00106      EJECT                                                        EL542
00107      MOVE 'ME'                   TO ME-RECORD-ID.                 EL542
00108      MOVE DTE-CLIENT             TO ME-COMPANY.                   EL542
00109      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**3
00110      MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL542
00111                                                                   EL542
00112      IF ME-DO-UPDATE                                              EL542
00113          READ ERMEBL INVALID KEY                                  EL542
00114                      PERFORM BUILD-ME-REC.                        EL542
00115                                                                   EL542
00116      GO TO 1000-MAIN-LOGIC.                                       EL542
00117      EJECT                                                        EL542
00118  BUILD-ME-REC.                                                    EL542
00119      MOVE 1 TO NEW-ME-REC-SW.                                     EL542
00120      MOVE DTE-CLIENT        TO ME-COMPANY.                        EL542
00121      MOVE MONTH-END-MOYR    TO ME-MOYR.                           EL542
00122      MOVE ZEROS             TO ME-010-PREM-L   ME-010-PREM-AH     EL542
00123                                ME-010-REF-L    ME-010-REF-AH      EL542
00124                                ME-010-NET-L    ME-010-NET-AH      EL542
00125                                ME-010-COMM-L   ME-010-COMM-AH     EL542
00126                                ME-010-PMT-L    ME-010-PMT-AH      EL542
00127                                ME-010-CERT-IN  ME-010-CERT-OUT    EL542
00128                                ME-010-DUP-ISS  ME-010-RUN-DT      EL542
070714*                              ME-010-START    ME-010-END         EL542
00130                                ME-010-RUN-CT                      EL542
070714                               ME-018-COMM-y   ME-018-COMM-1      EL542
00132                                ME-018-Ow-y     ME-018-Ow-1
00133                                ME-018-RUN-DT
070714*                              ME-018-START ME-018-END
                                     ME-018-RUN-CT
00135                                ME-019-PREM-L   ME-019-PREM-AH     EL542
00136                                ME-019-REF-L    ME-019-REF-AH      EL542
00137                                ME-019-COMM-L   ME-019-COMM-AH     EL542
00138                                ME-019-OR-L     ME-019-OR-AH       EL542
00139                                ME-019-RUN-DT
070714*                              ME-019-START ME-019-END
                                     ME-019-RUN-CT
00141                                ME-030-CLMS-L   ME-030-CLMS-AH     EL542
00142                                ME-030-RUN-DT
070714*                              ME-030-START ME-030-END
                                     ME-030-RUN-CT
00144                                ME-032-RESV-L   ME-032-RESV-AH     EL542
00145                                ME-032-RUN-DT
070714*                              ME-032-START    ME-032-END
                                     ME-032-RUN-CT
00147                                ME-035-NET-L    ME-035-NET-AH      EL542
00148                                ME-035-RUN-DT
070714*                              ME-035-START    ME-035-END
                                     ME-035-RUN-CT
00150                                ME-038-RECS-IN  ME-038-RECS-OUT    EL542
00151                                ME-038-RUN-DT
070714*                              ME-038-START    ME-038-END
                                     ME-038-RUN-CT
00153                                ME-048-RECS-IN  ME-048-RECS-OUT    EL542
00154                                ME-048-RUN-DT   ME-048-START       EL542
00155                                ME-048-END      ME-048-RUN-CT      EL542
00156                                ME-041-RETROS   ME-041-RUN-DT      EL542
070714*                              ME-041-START    ME-041-END         EL542
00158                                ME-041-RUN-CT   ME-041-REIN-ADJ       CL**2
00159                                ME-061-PREM     ME-061-COMM        EL542
00160                                ME-061-OR       ME-061-PY-ADJ      EL542
00161                                ME-061-COMM-RCALC ME-061-OR-RCALC  EL542
00162                                ME-061-PREM-RCALC ME-061-RUN-DT    EL542
070714*                              ME-061-START     ME-061-END        EL542
00164                                ME-061-RUN-CT                      EL542
00165                                ME-080-MORT-ERRS ME-080-RUN-DT     EL542
070714*                              ME-080-START     ME-080-END        EL542
00167                                ME-080-RUN-CT                      EL542
00168                                ME-315-RESV-L    ME-315-RESV-AH    EL542
070714                               ME-315-RUN-DT    ME-315-RUN-CT     EL542
00171                                ME-331-FLAG      ME-331-RUN-DT     EL542
070714*                              ME-331-START     ME-331-END        EL542
00173                                ME-331-RUN-CT                      EL542
00174                                ME-341-NOT-FOUND ME-341-RUN-DT     EL542
070714*                              ME-341-START     ME-341-END        EL542
00176                                ME-341-RUN-CT                      EL542
00177                                ME-501-FLAG      ME-501-RUN-DT     EL542
00178                                ME-501-START     ME-501-END        EL542
00179                                ME-501-RUN-CT                      EL542
00180                                ME-509-FLAG      ME-509-RUN-DT     EL542
00181                                ME-509-START     ME-509-END        EL542
00182                                ME-509-RUN-CT                      EL542
00183                                ME-050-CERT-IN   ME-050-CERT-OUT   EL542
00184                                ME-050-RUN-DT
070714*                              ME-050-START     ME-050-END
                                     ME-050-RUN-CT
00186                                ME-522-PY-ADJ    ME-522-RETROS     EL542
00187                            ME-522-PROC-CLM-L ME-522-PROC-CLM-AH   EL542
00188                            ME-522-ALL-CLM-L  ME-522-ALL-CLM-AH    EL542
00189                            ME-522-PROC-RSV-L ME-522-PROC-RSV-AH   EL542
00190                            ME-522-ALL-RSV-L  ME-522-ALL-RSV-AH    EL542
00191                                ME-522-PREM-L    ME-522-PREM-AH    EL542
00192                                ME-522-REF-L     ME-522-REF-AH     EL542
00193                                ME-522-RUN-DT    ME-522-START      EL542
00194                                ME-522-END       ME-522-RUN-CT     EL542
00195                                ME-524-CLMS-L    ME-524-CLMS-AH    EL542
00196                                ME-524-RESV-L    ME-524-RESV-AH    EL542
00197                                ME-524-RUN-DT    ME-524-START      EL542
00198                                ME-524-END       ME-524-RUN-CT     EL542
00199                                ME-525-FLAG      ME-525-RUN-DT     EL542
00200                                ME-525-START     ME-525-END        EL542
00201                                ME-525-RUN-CT

070714                               me-010-mm-prem-tot
070714                               me-010-mm-tot-clms
070714                               me-010-mm-tot-resv
070714                               ME-019-CLMS-L       
070714                               ME-019-CLMS-AH      
070714                               ME-035-COMM-TOT     
070714                               ME-035-CLMS-L       
070714                               ME-035-CLMS-AH      
070714                               ME-041-RECS-IN      
070714                               ME-041-RECS-OUT     
070714                               ME-050-ACT-L        
070714                               ME-050-ACT-AH       
070714                               ME-061-CLMS         
070714                               ME-341-CLMS-L       
070714                               ME-341-CLMS-AH      
070714                               ME-515-CLMS-WRAP    
070714                               ME-515-RESV-WRAP    
070714                               ME-317-CLMS-TOT     
070714                               ME-325-CLMS-TOT     
070714                               ME-020-PREM-TOT     
070714                               ME-020-CLMS-TOT     
070714                               ME-020-RESV-TOT     
070714                               ME-063-OW           
070714                               ME-082-ACT-L        
070714                               ME-082-ACT-AH       
070714                               ME-016-RECS-IN      
070714                               ME-016-RECS-OUT     
070714                               ME-562-PREM-TOT     
070714                               ME-562-COMM-TOT     
070714                               ME-524-CLMS-TOT-CM  
070714                               ME-523-PREM-TOT     
070714                               ME-064-BEG-BAL      
070714                               ME-064-END-BAL
007714                               me-080-mort-resv
00202                                                                   EL542
           .
00204  1000-MAIN-LOGIC SECTION.                                         EL542
00205                                                                   EL542
00206      IF ME-DO-UPDATE AND NEW-ME-REC-SW = 1
00207          WRITE MONTH-END-BALANCES
080202         DISPLAY 'MONTH-END-BALANCES RECORD WRITTEN '
080202                             MONTH-END-BALANCES
           ELSE                                                         EL542
00208          IF ME-DO-UPDATE                                          EL542
00209              REWRITE MONTH-END-BALANCES
080202             DISPLAY 'MONTH-END-BALANCES RECORD WRITTEN '
080202                                 MONTH-END-BALANCES.
00210                                                                   EL542
00211      IF ME-DO-UPDATE                                              EL542
00212            CLOSE ERMEBL.                                          EL542
00213                                                                   EL542
00214      GOBACK.                                                      EL542
00215                                                                   EL542
00216  ABEND-PGM SECTION.                                               EL542
00217      COPY ELCABEND SUPPRESS.                                      EL542
