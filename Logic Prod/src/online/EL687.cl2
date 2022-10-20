00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL687
00003  PROGRAM-ID.                 EL687 .                                 LV019
00004 *              PROGRAM CONVERTED BY                                  CL*18
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*18
00006 *              CONVERSION DATE 09/20/95 08:42:38.                    CL*18
00007 *                            VMOD=2.019                              CL*19
00008 *                                                                 EL687
00009 *AUTHOR.    LOGIC, INC.                                              CL*18
00010 *           DALLAS, TEXAS.                                           CL*18
00011                                                                   EL687
00012 *DATE-COMPILED.                                                      CL*18
00013                                                                   EL687
00014 *SECURITY.   *****************************************************   CL*18
00015 *            *                                                   *   CL*18
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*18
00017 *            *                                                   *   CL*18
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*18
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*18
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*18
00021 *            *                                                   *   CL*18
00022 *            *****************************************************   CL*18
00023                                                                   EL687
00024 *REMARKS.   TRANSACTION - EXG3                                       CL*18
00025                                                                   EL687
00026 *        IN ORDER TO START THE PRINTING OF CHECKS, THIS FUNCTION     CL*18
00027 *    IS USED TO QUALIFY THE CONTROL BATCHES TO BE PRINTED AND        CL*18
00028 *    SPECIFY A PRINT TIME.  THE CHECK WRITER PROGRAM (EL688) IS      CL*18
00029 *    STARTED BY THIS PROGRAM.                                        CL*18
00030                                                                   EL687
00031 *    SCREENS     - EL687A - CHECK WRITER                             CL*18
00032                                                                   EL687
00033 *    ENTERED BY  - EL671  - REPORT MENU                              CL*18
00034                                                                   EL687
00035 *    EXIT TO     - EL671  - RESULT OF CLEAR                          CL*18
00036                                                                   EL687
00037 *    INPUT FILES - ERCHKQ - CHECK QUEUE                              CL*18
00038 *                  ERPYAJ - PENDING PAYMENTS AND ADJUSTMENTS         CL*18
00039 *                  ELCNTL - CONTROL FILE                             CL*18
00040                                                                   EL687
00041 *    OUTPUT FILES - ERPYAJ - PENDING PAYMENTS AND ADJUSTMENTS        CL*18
00042 *                   ERCHEK - CHECK MAINTENANCE                       CL*18
00043 *                   ERCHKQ - CHECK QUEUE                             CL*18
00044                                                                   EL687
00045 *    COMMAREA    - PASSED.  PRINT TIME PASSED TO CHECK WRITER AS     CL*18
00046 *                  A 2 BYTE COMP NUMBER IN THE FIRST TWO BYTES OF    CL*18
00047 *                  THE WORK AREA.  FORM TYPE IS PASSED AS            CL*18
00048 *                  ENTRY-CD-1.                                       CL*18
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
030612******************************************************************
00049                                                                   EL687
00050      EJECT                                                        EL687
00051  ENVIRONMENT DIVISION.                                            EL687
00052                                                                   EL687
00053  DATA DIVISION.                                                   EL687
00054                                                                   EL687
00055  WORKING-STORAGE SECTION.                                         EL687
00056  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.                   CL*18
00057  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP        CL*18
00058                                    USAGE POINTER.                    CL*18
00059  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*18
00060  01  LCP-CURRENT-DATE-68.                                            CL*18
00061      05  LCP-MONTH                 PIC X(2).                         CL*18
00062      05  FILLER                    PIC X VALUE '/'.                  CL*18
00063      05  LCP-DAY1                  PIC X(2).                         CL*18
00064      05  FILLER                    PIC X VALUE '/'.                  CL*18
00065      05  LCP-YEAR                  PIC X(2).                         CL*18
00066  01  LCP-CICS-DATE                 PIC 9(15).                        CL*18
00067                                                                   EL687
00068  77  FILLER  PIC X(32)  VALUE '********************************'. EL687
00069  77  FILLER  PIC X(32)  VALUE '*    EL687 WORKING STORAGE     *'. EL687
00070  77  FILLER  PIC X(32)  VALUE '************ V/M 2.019 *********'.    CL*19
00071                                                                   EL687
00072  01  FILLER                          COMP-3.                      EL687
00073      05  WS-NOT-FOUND                PIC S9          VALUE ZERO.  EL687
00074      05  WS-PRINTER-STARTED-SW       PIC S9          VALUE ZERO.  EL687
00075      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  EL687
00076      05  WS-LAST-ERROR-COUNT         PIC S9(3)       VALUE ZERO.  EL687
00077      05  WS-COMPLETED-SUCCESSFUL     PIC S9          VALUE ZERO.  EL687
00078        88  TRANSACTION-SUCCESSFUL                    VALUE +1 +2. EL687
00079        88  CHECKS-WITHOUT-ADDRESSES                  VALUE +2.    EL687
00080                                                                   EL687
00081      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.  EL687
00082      05  WS-TIME                     REDEFINES                    EL687
00083          WS-TIME-WORK                PIC S9(3)V9(4).              EL687
00084      05  WS-HHMM                     REDEFINES                    EL687
00085          WS-TIME-WORK                PIC S9(5)V99.                EL687
00086                                                                   EL687
00087      05  WS-PENDING-PAYMENTS-BROWSE-SW  PIC S9       VALUE ZERO.  EL687
00088      05  WS-CHECK-QUEUE-BROWSE-SW    PIC S9          VALUE ZERO.  EL687
00089      05  WS-CHECK-MAINT-BROWSE-SW    PIC S9          VALUE ZERO.  EL687
00090                                                                   EL687
00091      05  WS-NOT-RELEASED-COUNT       PIC S9(5)       VALUE ZERO.  EL687
00092      05  WS-NOT-RELEASED-AMOUNT      PIC S9(9)V99 VALUE ZERO.     EL687
00093      05  WS-RELEASED-COUNT           PIC S9(5)       VALUE ZERO.  EL687
00094      05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.     EL687
00095                                                                   EL687
00096                                                                   EL687
00097      EJECT                                                        EL687
00098  01  FILLER                          COMP SYNC.                      CL*18
00099      05  WS-TS-LENGTH                PIC S9(4)       VALUE +750.     CL**9
00100      05  WS-ERCHKQ-LENGTH            PIC S9(4)       VALUE +100.  EL687
00101      05  WS-KEY-LENGTH               PIC S9(4)       VALUE ZERO.  EL687
00102                                                                   EL687
00103      05  WS-CHECK-QUE-COUNTER        PIC S9(8)       VALUE ZERO.  EL687
00104      05  WS-CHECK-COUNTER            PIC S9(4)       VALUE +10.   EL687
00105                                                                   EL687
00106      05  WS-LAST-CONTROL-GROUP       PIC S9(8)       VALUE ZERO.  EL687
00107      05  WS-TIMES-PRINTED            PIC S9(4)       VALUE ZERO.  EL687
00108                                                                   EL687
00109      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.    EL687
00110      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.     CL**8
00111                                                                   EL687
00112      05  WS-SEQUENCE-NUMBER          PIC S9(4)       VALUE ZERO.  EL687
00113      05  WS-BEGIN-NUMBER             PIC S9(4)       VALUE ZERO.  EL687
00114                                                                   EL687
00115      05  WS-CQFCBAR                  PIC S9(9)       VALUE ZERO.  EL687
00116      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL687
00117                                                                   EL687
00118      EJECT                                                        EL687
00119  01  FILLER.                                                      EL687
00120                                                                   EL687
00121      05  WS-CONTROL-FILE-KEY.                                     EL687
00122          10  WS-CFK-COMPANY-ID       PIC X(3)        VALUE SPACES.EL687
00123          10  WS-CFK-RECORD-TYPE      PIC X           VALUE SPACES.EL687
00124          10  FILLER                  PIC XX          VALUE SPACES.EL687
00125          10  WS-CFK-BENEFIT-NO       PIC XX          VALUE SPACES.EL687
00126          10  WS-CFK-SEQUENCE-NO      PIC S9(4)       VALUE ZERO   EL687
00127                                      COMP.                        EL687
00128                                                                   EL687
00129      05  WS-PENDING-PAYMENTS-KEY.                                 EL687
00130          10  WS-PPK-COMPANY-CD       PIC X.                       EL687
00131          10  WS-PPK-CARRIER          PIC X.                       EL687
00132          10  WS-PPK-GROUPING         PIC X(6).                    EL687
00133          10  WS-PPK-FIN-RESP         PIC X(10).                   EL687
00134          10  WS-PPK-ACCOUNT          PIC X(10).                   EL687
00135          10  WS-PPK-FILE-SEQ-NO      PIC S9(8)      COMP.         EL687
00136          10  WS-PPK-RECORD-TYPE      PIC X.                       EL687
00137                                                                   EL687
00138      05  WS-CHECK-MAINT-KEY.                                      EL687
00139          10  WS-CMK-COMPANY-CD       PIC X.                       EL687
00140          10  WS-CMK-CARRIER          PIC X.                       EL687
00141          10  WS-CMK-GROUPING         PIC X(6).                    EL687
00142          10  WS-CMK-STATE            PIC X(2).                    EL687
00143          10  WS-CMK-ACCOUNT          PIC X(10).                   EL687
00144          10  WS-CMK-CERT-EFF-DT      PIC X(2).                    EL687
00145          10  WS-CMK-CERT-NO          PIC X(11).                   EL687
00146          10  WS-CMK-SEQ-NO           PIC S9(4)      COMP.         EL687
00147                                                                   EL687
00148      05  WS-CHECK-QUEUE-KEY.                                      EL687
00149          10  WS-CQK-COMPANY-CD       PIC X.                       EL687
00150          10  WS-CQK-CONTROL-NUMBER   PIC S9(8)                    EL687
00151                                      COMP.                        EL687
00152          10  WS-CQK-SEQUENCE-NUMBER  PIC S9(4)                    EL687
00153                                      COMP.                        EL687
00154                                                                   EL687
00155      05  WS-LAST-CHECK-QUEUE-KEY     PIC X(7) VALUE LOW-VALUE.    EL687
00156                                                                   EL687
00157      05  WS-COMPENSATION-MASTER-KEY.                              EL687
00158          10  WS-CM-COMPANY-CD        PIC X.                       EL687
00159          10  WS-CM-CARRIER           PIC X.                       EL687
00160          10  WS-CM-GROUPING          PIC X(6).                    EL687
00161          10  WS-CM-FIN-RESP          PIC X(10).                   EL687
00162          10  WS-CM-ACCOUNT           PIC X(10).                   EL687
00163          10  WS-CM-TYPE              PIC X.                       EL687
00164                                                                   EL687
00165      EJECT                                                        EL687
00166      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL687S'.EL687
00167      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL687A'.EL687
00168                                                                   EL687
00169      05  FILLER                      REDEFINES                    EL687
00170          WS-MAP-NAME.                                             EL687
00171          20  FILLER                  PIC XX.                      EL687
00172          20  WS-MAP-NUMBER           PIC X(6).                    EL687
00173                                                                   EL687
00174      05  WS-PROGRAM-ID               PIC X(8)      VALUE 'EL687'. EL687
00175      05  WS-CHECK-QUEUE-DSID         PIC X(8)      VALUE 'ERCHKQ'.EL687
00176      05  WS-ENQ-COMPANY-ID           PIC X(3)      VALUE  ZERO.   EL687
00177      05  WS-PENDING-PAYMENTS-DSID    PIC X(8)      VALUE 'ERPYAJ'.EL687
00178      05  WS-CONTROL-FILE-DSID        PIC X(8)      VALUE 'ELCNTL'.EL687
00179      05  WS-COMPENSATION-MASTER-DSID PIC X(8)      VALUE 'ERCOMP'.EL687
00180      05  WS-CHECK-MAINT-DSID         PIC X(8)      VALUE 'ERCHEK'.EL687
00181      05  WS-JOURNAL-TYPE-ID          PIC XX        VALUE 'ER'.    EL687
00182                                                                   EL687
00183      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      EL687
00184      05  WS-SPACES                   PIC X           VALUE SPACES.EL687
00185                                                                   EL687
00186      05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.     EL687
00187                                                                   EL687
00188      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EXG3'.EL687
00189      05  WS-CHECK-WRITER-TRANS-ID    PIC X(4)        VALUE 'EXG4'.EL687
00190                                                                   EL687
00191      05  WS-BENEFIT-NO               PIC XX          VALUE SPACES.   CL**5
00192      05  WS-KIND.                                                 EL687
00193          10  WS-RETRO-DAYS           PIC 99.                      EL687
00194          10  WS-RETRO-ELIM           PIC X.                       EL687
00195      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    EL687
00196                                      COMP SYNC.                      CL*18
00197      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.EL687
00198      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO   EL687
00199                                      COMP SYNC.                      CL*18
00200      05  WS-TEMP-STORAGE-KEY.                                     EL687
00201          10  WS-TSK-TERM-ID          PIC X(4)        VALUE SPACES.EL687
00202          10  WS-TSK-TIME             PIC S9(7)       VALUE ZERO   EL687
00203                                      COMP-3.                      EL687
00204      05  WS-MINUTES                  PIC S99         VALUE ZERO.  EL687
00205      05  WS-GREATEST-CHECK-NUMBER    PIC 9(7)        VALUE ZERO.  EL687
00206      05  WS-GREATEST-CHECK-NUMBER-X  REDEFINES                    EL687
00207          WS-GREATEST-CHECK-NUMBER    PIC X(7).                    EL687
00208      05  WS-CHECK-NUMBER             PIC 9(7)        VALUE ZERO.  EL687
00209      05  WS-CHECK-NUMBER-X           REDEFINES                    EL687
00210          WS-CHECK-NUMBER             PIC X(7).                    EL687
00211      05  WS-CHECK-NO.                                             EL687
00212          10  FILLER                  PIC X           VALUE ZERO.  EL687
00213          10  WS-CHECK-WORK           PIC X(6).                    EL687
00214      05  WS-ACKNO                    PIC 9(7)        VALUE ZERO.  EL687
00215      05  WS-ACKNO-X                  REDEFINES                    EL687
00216          WS-ACKNO                    PIC X(7).                    EL687
00217      05  WS-PAYMENT-TYPE             PIC X           VALUE ZERO.  EL687
00218      05  WS-PAYEE-TYPE-CD            PIC X           VALUE ZERO.  EL687
00219                                                                   EL687
00220      05  WS-INIT-CONTROL-GROUP.                                   EL687
00221          10  FILLER              PIC X               VALUE SPACES.EL687
00222          10  FILLER              PIC X(7) VALUE LOW-VALUES.       EL687
00223                                                                   EL687
00224      05  WS-SSN.                                                  EL687
00225          10  WS-SSN-STATE            PIC XX.                      EL687
00226          10  WS-SSN-ACCOUNT          PIC X(6).                    EL687
00227          10  WS-SSN-LN3              PIC X(3).                    EL687
00228                                                                   EL687
00229      05  WS-MEMBER-NUMBER.                                        EL687
00230          10  WS-MEMBER-STATE         PIC XX.                      EL687
00231          10  WS-MEMBER-ACCOUNT       PIC X(6).                    EL687
00232          10  WS-MEMBER-LN4           PIC X(4).                    EL687
00233                                                                   EL687
00234      05  WS-OLD-CHECK-QUEUE-RECORD   PIC X(100)      VALUE SPACES.EL687
00235      05  WS-NEW-CHECK-QUEUE-RECORD   PIC X(100)      VALUE SPACES.EL687
00236                                                                   EL687
00237      EJECT                                                        EL687
00238      05  WS-ERROR-NUMBERS.                                        EL687
00239          10  ER-0002                 PIC 9(4)        VALUE 0002.  EL687
00240          10  ER-0004                 PIC 9(4)        VALUE 0004.  EL687
00241          10  ER-0008                 PIC 9(4)        VALUE 0008.  EL687
00242          10  ER-0029                 PIC 9(4)        VALUE 0029.  EL687
00243          10  ER-0330                 PIC 9(4)        VALUE 0330.  EL687
00244          10  ER-0361                 PIC 9(4)        VALUE 0361.  EL687
00245          10  ER-0362                 PIC 9(4)        VALUE 0362.  EL687
00246          10  ER-0363                 PIC 9(4)        VALUE 0363.  EL687
00247          10  ER-0364                 PIC 9(4)        VALUE 0364.  EL687
00248          10  ER-0365                 PIC 9(4)        VALUE 0365.  EL687
00249          10  ER-0366                 PIC 9(4)        VALUE 0366.  EL687
00250          10  ER-0367                 PIC 9(4)        VALUE 0367.  EL687
00251          10  ER-0368                 PIC 9(4)        VALUE 0368.  EL687
00252          10  ER-0369                 PIC 9(4)        VALUE 0369.  EL687
00253          10  ER-0370                 PIC 9(4)        VALUE 0370.  EL687
00254          10  ER-0371                 PIC 9(4)        VALUE 0371.  EL687
00255          10  ER-0379                 PIC 9(4)        VALUE 0379.  EL687
00256          10  ER-0380                 PIC 9(4)        VALUE 0380.  EL687
00257          10  ER-0381                 PIC 9(4)        VALUE 0381.  EL687
00258          10  ER-0382                 PIC 9(4)        VALUE 0382.     CL**9
00259          10  ER-0383                 PIC 9(4)        VALUE 0383.     CL**9
00260          10  ER-0385                 PIC 9(4)        VALUE 0385.     CL**9
00261          10  ER-0387                 PIC 9(4)        VALUE 0387.     CL**9
00262          10  ER-0389                 PIC 9(4)        VALUE 0389.     CL**9
00263          10  ER-0390                 PIC 9(4)        VALUE 0390.  EL687
00264          10  ER-0391                 PIC 9(4)        VALUE 0391.  EL687
00265          10  ER-0392                 PIC 9(4)        VALUE 0392.  EL687
00266          10  ER-0393                 PIC 9(4)        VALUE 0393.  EL687
00267          10  ER-0394                 PIC 9(4)        VALUE 0394.  EL687
00268          10  ER-0395                 PIC 9(4)        VALUE 0395.  EL687
00269          10  ER-0490                 PIC 9(4)        VALUE 0490.  EL687
00270          10  ER-3130                 PIC 9(4)        VALUE 3130.     CL**4
00271                                                                   EL687
00272                                                                   EL687
00273      EJECT                                                        EL687
00274                              COPY ELCINTF.                           CL**9
00275                              COPY ERC687PI.                          CL**9
00276      EJECT                                                        EL687
00277                              COPY ERCCPA.                            CL**9
00278      EJECT                                                        EL687
00279                              COPY ELCNWA.                            CL**9
00280      EJECT                                                           CL**3
00281                              COPY EL687S.                            CL**9
00282  01  FILLER                          REDEFINES                    EL687
00283      EL687AI.                                                     EL687
00284                                                                   EL687
00285      05  FILLER                      PIC X(35).                   EL687
00286                                                                   EL687
00287      05  FILLER                      OCCURS 4 TIMES               EL687
00288                                      INDEXED BY EL687A-INDEX.     EL687
00289                                                                   EL687
00290          10  EL687A-CONTROL-GROUP-LENGTH   PIC S9(4)              EL687
00291                                            COMP.                  EL687
00292          10  EL687A-CONTROL-GROUP-ATTRB    PIC X.                 EL687
00293          10  EL687A-CONTROL-GROUP          PIC 9(7).              EL687
00294          10  EL687A-CONTROL-GROUP-X        REDEFINES              EL687
00295              EL687A-CONTROL-GROUP          PIC X(7).              EL687
00296                                                                   EL687
00297      EJECT                                                           CL**3
00298                              COPY ELCJPFX.                           CL**9
00299                                      PIC X(750).                     CL**8
00300                                                                   EL687
00301      EJECT                                                           CL**3
00302                              COPY ELCEMIB.                           CL*18
00303                                                                   EL687
00304      EJECT                                                           CL**3
00305                              COPY ELCDATE.                           CL**9
00306                                                                   EL687
00307      EJECT                                                           CL**3
00308                              COPY ELCLOGOF.                          CL**9
00309                                                                   EL687
00310      EJECT                                                           CL**3
00311                              COPY ELCATTR.                           CL**9
00312                                                                   EL687
00313      EJECT                                                           CL**3
00314                              COPY ELCAID.                            CL**9
00315                                                                   EL687
00316  01  FILLER                      REDEFINES                        EL687
00317      DFHAID.                                                      EL687
00318                                                                   EL687
00319      05  FILLER                      PIC X(8).                    EL687
00320                                                                   EL687
00321      05  PF-VALUES                   PIC X                        EL687
00322          OCCURS 24 TIMES.                                         EL687
00323                                                                   EL687
00324      EJECT                                                           CL**3
00325  LINKAGE SECTION.                                                 EL687
00326                                                                   EL687
00327  01  DFHCOMMAREA                     PIC X(1024).                 EL687
00328                                                                   EL687
00329 *01 DFHBLLDS                         COMP SYNC.                      CL*18
00330 *    05  BLLCBAR                     PIC S9(9).                      CL*18
00331 *    05  CQFCBAR                     PIC S9(9).                      CL*18
00332 *    05  CFCBAR                      PIC S9(9).                      CL*18
00333 *    05  PPFCBAR                     PIC S9(9).                      CL*18
00334 *    05  CMFCBAR                     PIC S9(9).                      CL*18
00335 *    05  CHFCBAR                     PIC S9(9).                      CL*18
00336                                                                   EL687
00337      EJECT                                                        EL687
00338                              COPY ERCCHKQ.                           CL**9
00339                                                                   EL687
00340      EJECT                                                        EL687
00341                              COPY ELCCNTL.                           CL**9
00342                                                                   EL687
00343      EJECT                                                        EL687
00344                              COPY ERCPYAJ.                           CL**9
00345                                                                   EL687
00346      EJECT                                                        EL687
00347                              COPY ERCCOMP.                           CL**9
00348                                                                   EL687
00349      EJECT                                                        EL687
00350                              COPY ERCCHEK.                           CL**9
00351                                                                   EL687
00352      EJECT                                                        EL687
00353  PROCEDURE DIVISION.                                              EL687
00354                                                                   EL687
00355      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL687
00356                                                                   EL687
00357 *    NOTE ******************************************************* EL687
00358 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL687
00359 *         *  FROM ANOTHER MODULE.                               * EL687
00360 *         *******************************************************.EL687
00361                                                                   EL687
00362      MOVE EIBDATE                TO  DC-JULIAN-YYDDD              EL687
00363      MOVE '5'                    TO  DC-OPTION-CODE               EL687
00364      PERFORM 8500-DATE-CONVERSION                                 EL687
00365      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE              EL687
00366                                                                   EL687
00367      IF EIBCALEN NOT GREATER THAN ZERO                            EL687
00368          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL687
00369          GO TO 8300-SEND-TEXT.                                    EL687
00370                                                                   EL687
00371      EXEC CICS HANDLE CONDITION                                   EL687
00372          PGMIDERR   (9600-PGMIDERR)                               EL687
00373          NOTFND     (0180-MAIN-LOGIC)                             EL687
00374          ENDFILE    (0190-MAIN-LOGIC)                             EL687
00375          TERMIDERR  (0900-TERMIDERR)                              EL687
00376          ENQBUSY    (0910-ENQ-BUSY)                               EL687
00377          ERROR      (9990-ERROR) END-EXEC.                        EL687
00378                                                                   EL687
00379      MOVE +3                     TO  EMI-NUMBER-OF-LINES          EL687
00380      MOVE +2                     TO  EMI-SWITCH2.                 EL687
00381                                                                   EL687
00382      EJECT                                                        EL687
00383  0010-MAIN-LOGIC.                                                 EL687
00384      IF PI-CALLING-PROGRAM NOT = WS-PROGRAM-ID                       CL**3
00385          IF PI-RETURN-TO-PROGRAM NOT = WS-PROGRAM-ID                 CL**3
00386              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL687
00387              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL687
00388              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL687
00389              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL687
00390              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL687
00391              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL687
00392              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL687
00393              MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM     EL687
00394            ELSE                                                   EL687
00395              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL687
00396              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL687
00397              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL687
00398              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL687
00399              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL687
00400              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL687
00401              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL687
00402              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL687
00403        ELSE                                                       EL687
00404          GO TO 0040-MAIN-LOGIC.                                   EL687
00405                                                                   EL687
00406  0015-MAIN-LOGIC.                                                 EL687
00407 *    NOTE ******************************************************* EL687
00408 *         *      INITIALIZE THE WORK FIELDS FOR THE PROGRAM     *    CL*18
00409 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL687
00410 *         *******************************************************.EL687
00411                                                                   EL687
00412      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.           CL**2
00413                                                                   EL687
00414      MOVE ZEROS                  TO  PI-NUMBER-OF-CONTROL-GROUPS     CL**2
00415                                      PI-NUMBER-OF-ALIGNMENT-CHECKSEL687
00416                                      PI-ALIGNMENT-CONTROL-GROUP   EL687
00417                                      PI-ALIGNMENT-SEQUENCE-NO     EL687
00418                                      PI-PROCESSING-SW             EL687
00419                                      PI-CONTROL-GROUP (1)         EL687
00420                                      PI-CONTROL-GROUP (2)         EL687
00421                                      PI-CONTROL-GROUP (3)         EL687
00422                                      PI-CONTROL-GROUP (4)         EL687
00423                                      PI-HIGH-SEQUENCE (1)         EL687
00424                                      PI-HIGH-SEQUENCE (2)         EL687
00425                                      PI-HIGH-SEQUENCE (3)         EL687
00426                                      PI-HIGH-SEQUENCE (4)         EL687
00427                                      PI-COMPANY-ZIP-CODE          EL687
00428                                      PI-COMPANY-PHONE-NUMBER.        CL**2
00429                                                                   EL687
00430      MOVE WS-PRINTER-STARTED-SW  TO  PI-PRINTER-STARTED-SW.       EL687
00431      MOVE WS-TEMP-STORAGE-KEY    TO  PI-TEMP-STORAGE-KEY.         EL687
00432                                                                   EL687
00433      MOVE LOW-VALUES             TO  EL687AI.                     EL687
00434      MOVE -1                     TO  AOPTIONL                     EL687
00435                                                                   EL687
00436      EXEC CICS ASSIGN                                             EL687
00437          SYSID (PI-SYSID)                                         EL687
00438          END-EXEC.                                                EL687
00439                                                                   EL687
00440      PERFORM 8100-SEND-INITIAL-MAP.                               EL687
00441                                                                   EL687
00442      EJECT                                                        EL687
00443  0040-MAIN-LOGIC.                                                 EL687
00444 *    NOTE ******************************************************* EL687
00445 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL687
00446 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL687
00447 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL687
00448 *         *******************************************************.EL687
00449                                                                   EL687
00450      IF EIBAID = DFHCLEAR                                         EL687
00451          GO TO 9400-CLEAR.                                        EL687
00452                                                                   EL687
00453      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL687
00454          MOVE LOW-VALUES         TO  EL687AI                      EL687
00455          MOVE ER-0008            TO  EMI-ERROR                    EL687
00456          MOVE -1                 TO  APFKL                        EL687
00457          PERFORM 8200-SEND-DATAONLY.                              EL687
00458                                                                   EL687
00459      EXEC CICS RECEIVE                                            EL687
00460          INTO   (EL687AI)                                         EL687
00461          MAPSET (WS-MAPSET-NAME)                                  EL687
00462          MAP    (WS-MAP-NAME) END-EXEC.                           EL687
00463                                                                   EL687
00464      IF APFKL IS GREATER THAN ZERO                                EL687
00465          IF EIBAID NOT = DFHENTER                                 EL687
00466              MOVE ER-0004        TO  EMI-ERROR                    EL687
00467              MOVE AL-UNBOF       TO  APFKA                        EL687
00468              MOVE -1             TO  APFKL                        EL687
00469              PERFORM 8200-SEND-DATAONLY                           EL687
00470            ELSE                                                   EL687
00471              IF APFKO GREATER ZERO AND LESS '25'                     CL**2
00472                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL687
00473                ELSE                                               EL687
00474                  MOVE ER-0029        TO  EMI-ERROR                EL687
00475                  MOVE AL-UNBOF       TO  APFKA                    EL687
00476                  MOVE -1             TO  APFKL                    EL687
00477                  PERFORM 8200-SEND-DATAONLY.                      EL687
00478                                                                   EL687
00479      IF EIBAID IS = DFHPF12                                       EL687
00480          MOVE 'EL010'            TO  WS-PROGRAM-ID                   CL**2
00481          GO TO 9300-XCTL.                                         EL687
00482                                                                   EL687
00483      IF EIBAID IS = DFHPF23                                       EL687
00484          GO TO 9000-RETURN-CICS.                                  EL687
00485                                                                   EL687
00486      IF EIBAID IS = DFHPF24                                       EL687
00487          MOVE 'EL126'            TO  WS-PROGRAM-ID                   CL**2
00488          GO TO 9300-XCTL.                                         EL687
00489                                                                   EL687
00490      IF EIBAID NOT = DFHENTER                                     EL687
00491          MOVE ER-0008            TO  EMI-ERROR                    EL687
00492          MOVE -1                 TO  APFKL                        EL687
00493          PERFORM 8200-SEND-DATAONLY.                              EL687
00494                                                                   EL687
00495      IF PI-PROCESSING-SW NOT = ZERO                               EL687
00496          GO TO 0240-MAIN-LOGIC.                                   EL687
00497                                                                   EL687
00498      EJECT                                                        EL687
00499  0100-MAIN-LOGIC.                                                 EL687
00500 *    NOTE ******************************************************* EL687
00501 *         *          SYNTAX CHECK THE MAP FIELDS                * EL687
00502 *         *******************************************************.EL687
00503                                                                   EL687
00504      IF AOPTIONL NOT GREATER THAN ZERO                            EL687
00505          MOVE -1                 TO  AOPTIONL                     EL687
00506          MOVE AL-UNBON           TO  AOPTIONA                     EL687
00507          MOVE ER-0002            TO  EMI-ERROR                    EL687
00508          PERFORM 8200-SEND-DATAONLY.                              EL687
00509                                                                   EL687
00510      IF (AOPTIONI GREATER ZERO AND LESS '4')                         CL**2
00511        OR                                                         EL687
00512          (PI-COMPANY-ID = 'POS' AND                               EL687
00513           AOPTIONI = ('2' OR '3'))                                EL687
00514              MOVE AL-UNNON           TO  AOPTIONA                 EL687
00515            ELSE                                                   EL687
00516              MOVE -1                 TO  AOPTIONL                 EL687
00517              MOVE AL-UNBON           TO  AOPTIONA                 EL687
00518              MOVE ER-0330            TO  EMI-ERROR                EL687
00519              PERFORM 9900-ERROR-FORMAT.                           EL687
00520                                                                   EL687
00521      IF AALIGNL GREATER THAN ZERO                                 EL687
00522          IF AALIGNI IS NUMERIC                                    EL687
00523              MOVE AALIGNO  TO  PI-NUMBER-OF-ALIGNMENT-CHECKS      EL687
00524              MOVE AL-UNNON       TO  AALIGNA                      EL687
00525            ELSE                                                   EL687
00526              MOVE ER-0365        TO  EMI-ERROR                    EL687
00527              MOVE -1             TO  AALIGNL                      EL687
00528              MOVE AL-UNBON       TO  AALIGNA                      EL687
00529              PERFORM 9900-ERROR-FORMAT.                           EL687
00530                                                                   EL687
00531      IF ACKNOL GREATER THAN ZERO                                  EL687
00532          IF ACKNOI IS NUMERIC                                     EL687
00533              MOVE AL-UNNON       TO  ACKNOA                       EL687
00534              MOVE ACKNOI         TO  WS-CHECK-NUMBER-X            EL687
00535            ELSE                                                   EL687
00536              MOVE ER-0366        TO  EMI-ERROR                    EL687
00537              MOVE -1             TO  ACKNOL                       EL687
00538              MOVE AL-UNBON       TO  ACKNOA                       EL687
00539              PERFORM 9900-ERROR-FORMAT                            EL687
00540        ELSE                                                       EL687
00541          MOVE ZERO               TO  WS-CHECK-NUMBER.             EL687
00542                                                                   EL687
00543      IF AACNL GREATER THAN ZERO                                   EL687
00544          MOVE AACNI              TO  PI-ASSIGN-CHECK-NUMBERS      EL687
00545          IF AACNI = ('Y' OR 'N')                                  EL687
00546              MOVE AL-UANON       TO  AACNA                        EL687
00547              MOVE AACNI         TO  PI-ENTRY-CD-1                 EL687
00548            ELSE                                                   EL687
00549              MOVE AL-UABON       TO  AACNA                        EL687
00550              MOVE -1             TO  AACNL                        EL687
00551              MOVE ER-0367        TO  EMI-ERROR                    EL687
00552              PERFORM 9900-ERROR-FORMAT                            EL687
00553        ELSE                                                       EL687
00554          MOVE AL-UABOF           TO  AACNA                        EL687
00555          MOVE -1                 TO  AACNL                        EL687
00556          MOVE ER-0368            TO  EMI-ERROR                    EL687
00557          PERFORM 9900-ERROR-FORMAT.                               EL687
00558                                                                   EL687
00559      IF AACNI = 'Y'                                               EL687
00560        AND ACKNOL NOT GREATER THAN ZERO                           EL687
00561          MOVE -1                 TO  ACKNOL                       EL687
00562          MOVE AL-UNBOF           TO  ACKNOA                       EL687
00563          MOVE ER-0392            TO  EMI-ERROR                    EL687
00564          PERFORM 9900-ERROR-FORMAT.                               EL687
00565                                                                   EL687
00566      IF AACNI = 'N'                                               EL687
00567        AND ACKNOL GREATER THAN ZERO                               EL687
00568          MOVE -1                 TO  ACKNOL                       EL687
00569          MOVE AL-UNBON           TO  ACKNOA                       EL687
00570          MOVE ER-0393            TO  EMI-ERROR                    EL687
00571          PERFORM 9900-ERROR-FORMAT.                               EL687
00572                                                                   EL687
00573      EJECT                                                        EL687
00574 *    NOTE ******************************************************* EL687
00575 *         *      CHECK THE VALIDITY OF ANY CONTROL GROUPS       * EL687
00576 *         *  ENTERED.                                           * EL687
00577 *         *******************************************************.EL687
00578                                                                   EL687
00579      SET EL687A-INDEX                                             EL687
00580          PI-INDEX TO +1.                                          EL687
00581                                                                   EL687
00582  0120-MAIN-LOGIC.                                                 EL687
00583      IF EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)                EL687
00584                                  NOT GREATER THAN ZERO            EL687
00585          MOVE AL-UNNOF  TO  EL687A-CONTROL-GROUP-ATTRB            EL687
00586                                                     (EL687A-INDEX)EL687
00587          GO TO 0190-MAIN-LOGIC.                                   EL687
00588                                                                   EL687
00589      IF EL687A-CONTROL-GROUP (EL687A-INDEX) IS NOT NUMERIC        EL687
00590          MOVE AL-UNBON  TO  EL687A-CONTROL-GROUP-ATTRB            EL687
00591                                                     (EL687A-INDEX)EL687
00592          MOVE -1  TO  EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)  EL687
00593          MOVE ER-0369            TO  EMI-ERROR                    EL687
00594          PERFORM 9900-ERROR-FORMAT                                EL687
00595          GO TO 0190-MAIN-LOGIC.                                   EL687
00596                                                                   EL687
00597      MOVE EL687A-CONTROL-GROUP (EL687A-INDEX)                     EL687
00598                                  TO  PI-CONTROL-GROUP (PI-INDEX)  EL687
00599      SET PI-INDEX UP BY +1                                        EL687
00600      MOVE AL-UNNON  TO  EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX) EL687
00601                                                                   EL687
00602      IF PI-INDEX IS GREATER THAN +2                               EL687
00603        AND PI-CONTROL-GROUP (PI-INDEX - 2)                        EL687
00604                      NOT LESS THAN PI-CONTROL-GROUP (PI-INDEX - 1)EL687
00605          MOVE ER-0385            TO  EMI-ERROR                    EL687
00606          PERFORM 9900-ERROR-FORMAT                                EL687
00607          MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)    EL687
00608          MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB              EL687
00609                                                    (EL687A-INDEX).EL687
00610                                                                   EL687
00611      MOVE ZERO                   TO  WS-NOT-FOUND                 EL687
00612                                                                   EL687
00613      MOVE LOW-VALUES             TO  WS-CHECK-QUEUE-KEY           EL687
00614                                                                   EL687
00615      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD            EL687
00616      MOVE EL687A-CONTROL-GROUP (EL687A-INDEX)                     EL687
00617                                  TO  WS-CQK-CONTROL-NUMBER        EL687
00618                                                                   EL687
00619      IF WS-CHECK-QUEUE-BROWSE-SW = ZERO                           EL687
00620          EXEC CICS STARTBR                                        EL687
00621              DATASET   (WS-CHECK-QUEUE-DSID)                      EL687
00622              RIDFLD    (WS-CHECK-QUEUE-KEY)                       EL687
00623              GENERIC   EQUAL                                      EL687
00624              KEYLENGTH (5) END-EXEC                               EL687
00625          MOVE +1                 TO  WS-CHECK-QUEUE-BROWSE-SW     EL687
00626        ELSE                                                       EL687
00627      EXEC CICS RESETBR                                            EL687
00628          DATASET   (WS-CHECK-QUEUE-DSID)                          EL687
00629          RIDFLD    (WS-CHECK-QUEUE-KEY)                           EL687
00630          GENERIC   EQUAL                                          EL687
00631          KEYLENGTH (5) END-EXEC.                                  EL687
00632                                                                   EL687
00633  0130-MAIN-LOGIC.                                                 EL687
00634      EXEC CICS READNEXT                                           EL687
00635          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
00636          RIDFLD  (WS-CHECK-QUEUE-KEY)                             EL687
00637          SET     (ADDRESS OF CHECK-QUE) END-EXEC                     CL*18
00638                                                                   EL687
00639      IF WS-CQK-COMPANY-CD NOT = PI-COMPANY-CD                     EL687
00640          GO TO 0170-MAIN-LOGIC.                                   EL687
00641                                                                   EL687
00642      IF WS-CQK-CONTROL-NUMBER NOT = EL687A-CONTROL-GROUP          EL687
00643                                                     (EL687A-INDEX)EL687
00644          GO TO 0170-MAIN-LOGIC.                                   EL687
00645                                                                   EL687
00646      IF CQ-ENTRY-TYPE NOT = 'Q'                                   EL687
00647          GO TO 0130-MAIN-LOGIC.                                   EL687
00648                                                                   EL687
00649      IF PI-COMPANY-ID = 'POS'                                     EL687
00650          MOVE CQ-PYAJ-CARRIER    TO  PI-CARRIER.                  EL687
00651                                                                   EL687
00652      IF AOPTIONI = '2'                                            EL687
00653        AND CQ-TIMES-PRINTED NOT GREATER THAN ZERO                 EL687
00654          GO TO 0190-MAIN-LOGIC.                                   EL687
00655                                                                   EL687
00656      IF AOPTIONI = '3'                                            EL687
00657        AND CQ-TIMES-PRINTED GREATER THAN ZERO                     EL687
00658          GO TO 0190-MAIN-LOGIC.                                   EL687
00659                                                                   EL687
00660      GO TO 0130-MAIN-LOGIC.                                       EL687
00661                                                                   EL687
00662  0170-MAIN-LOGIC.                                                 EL687
00663      MOVE ER-0394                TO  EMI-ERROR                    EL687
00664      PERFORM 9900-ERROR-FORMAT                                    EL687
00665      MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)        EL687
00666      MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)   EL687
00667                                                                   EL687
00668      GO TO 0190-MAIN-LOGIC.                                       EL687
00669                                                                   EL687
00670  0180-MAIN-LOGIC.                                                 EL687
00671      MOVE ER-0387                TO  EMI-ERROR                    EL687
00672      PERFORM 9900-ERROR-FORMAT                                    EL687
00673      MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)        EL687
00674      MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX).  EL687
00675                                                                   EL687
00676  0190-MAIN-LOGIC.                                                 EL687
00677      IF EL687A-INDEX LESS THAN +4                                 EL687
00678          SET EL687A-INDEX UP BY +1                                EL687
00679          GO TO 0120-MAIN-LOGIC.                                   EL687
00680                                                                   EL687
00681      IF WS-CHECK-QUEUE-BROWSE-SW NOT = ZERO                       EL687
00682          MOVE ZERO               TO  WS-CHECK-QUEUE-BROWSE-SW     EL687
00683          EXEC CICS ENDBR                                          EL687
00684              DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.              EL687
00685                                                                   EL687
00686      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL687
00687          PERFORM 8200-SEND-DATAONLY.                              EL687
00688                                                                   EL687
00689      IF AOPTIONI = ('2' OR '3')                                   EL687
00690        AND PI-INDEX NOT GREATER THAN +1                           EL687
00691          MOVE -1                 TO  ACG01L                       EL687
00692          MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A           EL687
00693          MOVE ER-0370            TO  EMI-ERROR                    EL687
00694          PERFORM 9900-ERROR-FORMAT.                               EL687
00695                                                                   EL687
00696      IF PI-INDEX GREATER THAN +1                                  EL687
00697          NEXT SENTENCE                                            EL687
00698        ELSE                                                       EL687
00699          GO TO 0200-MAIN-LOGIC.                                   EL687
00700                                                                   EL687
00701      SET PI-INDEX DOWN BY +1                                      EL687
00702      SET PI-NUMBER-OF-CONTROL-GROUPS TO PI-INDEX                  EL687
00703      SET PI-INDEX                                                 EL687
00704          EL687A-INDEX TO +1.                                      EL687
00705                                                                   EL687
00706  0195-MAIN-LOGIC.                                                 EL687
00707      IF PI-CONTROL-GROUP (PI-INDEX) GREATER THAN ZERO             EL687
00708          MOVE PI-CONTROL-GROUP (PI-INDEX)                         EL687
00709                      TO  EL687A-CONTROL-GROUP (EL687A-INDEX)      EL687
00710          MOVE AL-UNNON TO EL687A-CONTROL-GROUP-ATTRB              EL687
00711                                                     (EL687A-INDEX)EL687
00712        ELSE                                                       EL687
00713          MOVE WS-INIT-CONTROL-GROUP                               EL687
00714                      TO  EL687A-CONTROL-GROUP (EL687A-INDEX)      EL687
00715          MOVE AL-UNNOF TO EL687A-CONTROL-GROUP-ATTRB              EL687
00716                                                    (EL687A-INDEX).EL687
00717                                                                   EL687
00718      IF PI-INDEX LESS THAN +4                                     EL687
00719          SET PI-INDEX                                             EL687
00720              EL687A-INDEX UP BY +1                                EL687
00721          GO TO 0195-MAIN-LOGIC.                                   EL687
00722                                                                   EL687
00723      SET EL687A-INDEX                                             EL687
00724          PI-INDEX TO +1.                                          EL687
00725      EJECT                                                        EL687
00726  0200-MAIN-LOGIC.                                                 EL687
00727 *    NOTE ******************************************************* EL687
00728 *         *      ALL OF THE SYNTAX CHECKS HAVE BEEN SUCCESSFUL- *    CL**2
00729 *         *  NOW DO THE PRE-EDIT.                               * EL687
00730 *         *                                                     * EL687
00731 *         *      BEFORE A CHECK BATCH IS QUEUED FOR PRINT, A    * EL687
00732 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          * EL687
00733 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   * EL687
00734 *         *  STEPS:                                             * EL687
00735 *         *                                                     * EL687
00736 *         *  1. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    * EL687
00737 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  * EL687
00738 *         *     FILE FOR OVERLAPS AND GAPS.                     * EL687
00739 *         *                                                     * EL687
00740 *         *  2. IF ANY CHECKS IN THE RELEASED GROUPS HAVE       * EL687
00741 *         *     ALREADY BEEN QUEUED FOR PRINT OR PRINTED.       * EL687
00742 *         *                                                     * EL687
00743 *         *  3. IF PRE-NUMBERING IS NOT USED THAT ALL CHECKS    * EL687
00744 *         *     HAVE A CHECK NUMBER ASSIGNED.                   * EL687
00745 *         *                                                     * EL687
00746 *         *  4. IF DUPLICATE CHECK NUMBERS ARE ASSIGNED.        * EL687
00747 *         *                                                     * EL687
00748 *         *      BEFORE A CHECK BATCH IS QUEUED FOR RE-PRINT, A * EL687
00749 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          * EL687
00750 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   * EL687
00751 *         *  STEPS:                                             * EL687
00752 *         *                                                     * EL687
00753 *         *  1. ALL CHECKS IN THE INDICATED GROUP(S) MUST HAVE  * EL687
00754 *         *     BEEN PREVIOUSLY PRINTED.                        * EL687
00755 *         *                                                     * EL687
00756 *         *  2. IF THE PRE-NUMBERING SWITCH IS SET IN ANY RECORD* EL687
00757 *         *     IT MUST BE SET IN ALL RECORDS.                  * EL687
00758 *         *                                                     * EL687
00759 *         *  3. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    * EL687
00760 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  * EL687
00761 *         *     FILE FOR OVERLAPS AND GAPS.                     * EL687
00762 *         *******************************************************.EL687
00763                                                                   EL687
00764      MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.              CL**2
00765                                                                   EL687
00766      EXEC CICS ENQ                                                EL687
00767          RESOURCE (WS-CHECK-QUEUE-DSID)                           EL687
00768          LENGTH   (11) END-EXEC.                                     CL**2
00769                                                                   EL687
00770      EXEC CICS HANDLE CONDITION                                   EL687
00771          NOTFND  (0225-MAIN-LOGIC)                                EL687
00772          ENDFILE (0230-MAIN-LOGIC) END-EXEC.                         CL**2
00773                                                                   EL687
00774      MOVE LOW-VALUES             TO  WS-CHECK-QUEUE-KEY.             CL**2
00775      MOVE +5                     TO  WS-KEY-LENGTH.                  CL**2
00776                                                                   EL687
00777      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD.           EL687
00778                                                                   EL687
00779      EXEC CICS STARTBR                                            EL687
00780          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
00781          RIDFLD  (WS-CHECK-QUEUE-KEY)                             EL687
00782          GTEQ    END-EXEC.                                        EL687
00783                                                                   EL687
00784  0210-MAIN-LOGIC.                                                 EL687
00785      MOVE EMI-FATAL-CTR         TO  WS-LAST-ERROR-COUNT.             CL**2
00786                                                                   EL687
00787      EXEC CICS READNEXT                                           EL687
00788          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
00789          RIDFLD  (WS-CHECK-QUEUE-KEY)                             EL687
00790          SET     (ADDRESS OF CHECK-QUE) END-EXEC.                    CL*18
00791                                                                   EL687
00792      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL687
00793          GO TO 0230-MAIN-LOGIC.                                   EL687
00794                                                                      CL*11
00795 *    IF CQ-ENTRY-TYPE NOT = 'Q'                                      CL*16
00796 *        GO TO 0210-MAIN-LOGIC.                                      CL*16
00797                                                                      CL*16
00798      IF  (AOPTIONI = '1' OR '2')                                     CL*16
00799              AND                                                     CL*16
00800          CQ-ENTRY-TYPE NOT = 'Q'                                     CL*16
00801          GO TO 0210-MAIN-LOGIC.                                      CL*11
00802                                                                   EL687
00803 *    NOTE ******************************************************* EL687
00804 *         *      SAVE THE CHECK NUMBER SO AT THE END OF THE     * EL687
00805 *         *  BROWSE YOU CAN CHECK FOR GAPS OR OVERLAPS.         * EL687
00806 *         *******************************************************.EL687
00807                                                                   EL687
00808      IF CQ-CHECK-NUMBER GREATER THAN WS-GREATEST-CHECK-NUMBER-X   EL687
00809          MOVE CQ-CHECK-NUMBER    TO  WS-GREATEST-CHECK-NUMBER-X.  EL687
00810                                                                   EL687
00811 *    NOTE ******************************************************* EL687
00812 *         *      IF YOU ARE PROCESSING BY GROUPS BYPASS ALL     * EL687
00813 *         *  RECORDS IF NOT IN SPECIFIED GROUPS.  SAVE THE HIGH *    CL*15
00814 *         *  SEQUENCE NUMBER IN EACH GROUP FOR REPRINT.         * EL687
00815 *         *******************************************************.EL687
00816                                                                   EL687
00817      IF AOPTIONI = '2' OR '3'                                     EL687
00818          NEXT SENTENCE                                            EL687
00819        ELSE                                                       EL687
00820          GO TO 0213-MAIN-LOGIC.                                   EL687
00821                                                                   EL687
00822      SET PI-INDEX                                                 EL687
00823          EL687A-INDEX TO +1.                                      EL687
00824                                                                   EL687
00825  0212-MAIN-LOGIC.                                                 EL687
00826      IF CQ-CONTROL-NUMBER  = PI-CONTROL-GROUP (PI-INDEX)          EL687
00827          IF CQ-SEQUENCE-NUMBER GREATER PI-HIGH-SEQUENCE (PI-INDEX)EL687
00828              MOVE CQ-SEQUENCE-NUMBER                              EL687
00829                                  TO PI-HIGH-SEQUENCE (PI-INDEX)   EL687
00830              GO TO 0215-MAIN-LOGIC                                EL687
00831            ELSE                                                   EL687
00832              GO TO 0215-MAIN-LOGIC.                               EL687
00833                                                                   EL687
00834      IF PI-INDEX LESS THAN +4                                     EL687
00835          SET PI-INDEX                                             EL687
00836              EL687A-INDEX UP BY +1                                EL687
00837          GO TO 0212-MAIN-LOGIC.                                   EL687
00838                                                                   EL687
00839      GO TO 0210-MAIN-LOGIC.                                       EL687
00840                                                                   EL687
00841  0213-MAIN-LOGIC.                                                 EL687
00842 *    NOTE ******************************************************* EL687
00843 *         *      IF YOU ARE PRINTING ALL CONTROL GROUPS BYPASS  * EL687
00844 *         *  THE CONTROL GROUPS THAT HAVE ALREADY BEEN PRINTED. * EL687
00845 *         *******************************************************.EL687
00846                                                                   EL687
00847      IF CQ-CONTROL-NUMBER NOT = WS-LAST-CONTROL-GROUP             EL687
00848          MOVE CQ-CONTROL-NUMBER  TO  WS-LAST-CONTROL-GROUP        EL687
00849          MOVE CQ-TIMES-PRINTED   TO  WS-TIMES-PRINTED.            EL687
00850                                                                   EL687
00851      IF WS-TIMES-PRINTED GREATER THAN ZERO                        EL687
00852          GO TO 0210-MAIN-LOGIC.                                   EL687
00853                                                                   EL687
00854      EJECT                                                        EL687
00855  0215-MAIN-LOGIC.                                                 EL687
00856                                                                      CL*16
CIDMOD*    IF  AOPTIONI = '3'                                              CL*16
CIDMOD     IF AOPTIONI = '1' OR '2'                                          000
00858          GO TO 0220-MAIN-LOGIC.                                   EL687
00859                                                                   EL687
00860      IF CQ-TIMES-PRINTED GREATER THAN ZERO                        EL687
00861         MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)     EL687
00862         MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)EL687
00863         MOVE ER-0379             TO  EMI-ERROR                    EL687
00864         PERFORM 9900-ERROR-FORMAT.                                EL687
00865                                                                   EL687
00866      IF AACNI = 'Y'                                               EL687
00867        AND CQ-CHECK-NUMBER NOT = SPACES                           EL687
00868          MOVE ER-0382            TO  EMI-ERROR                    EL687
00869          PERFORM 9900-ERROR-FORMAT                                EL687
00870          MOVE -1                 TO  AACNL                        EL687
00871          MOVE AL-UABON           TO  AACNA                        EL687
00872      ELSE                                                            CL**9
00873          IF AACNI = 'N'                                              CL**9
00874            AND CQ-CHECK-NUMBER = SPACES                              CL**9
00875              MOVE -1             TO  AACNL                           CL**9
00876              MOVE AL-UABON       TO  AACNA                           CL**9
00877              MOVE ER-0383        TO  EMI-ERROR                       CL**9
00878              PERFORM 9900-ERROR-FORMAT.                              CL**9
00879                                                                   EL687
00880      GO TO 0210-MAIN-LOGIC.                                       EL687
00881                                                                   EL687
00882  0220-MAIN-LOGIC.                                                 EL687
00883                                                                      CL*16
00884      IF  CQ-ENTRY-TYPE NOT = 'Q'                                     CL*16
00885          GO TO 0210-MAIN-LOGIC.                                      CL*16
00886                                                                      CL*16
00887      IF CQ-TIMES-PRINTED NOT GREATER THAN ZERO                    EL687
00888         MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)     EL687
00889         MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)EL687
00890         MOVE ER-0389             TO  EMI-ERROR                    EL687
00891         PERFORM 9900-ERROR-FORMAT.                                EL687
00892                                                                   EL687
00893      IF CQ-PRE-NUMBERING-SW = '1'                                 EL687
00894        AND AACNI = 'N'                                            EL687
00895          MOVE -1                 TO  AACNL                        EL687
00896          MOVE AL-UABON           TO  AACNA                        EL687
00897          MOVE ER-0390            TO  EMI-ERROR                    EL687
00898          PERFORM 9900-ERROR-FORMAT.                               EL687
00899                                                                   EL687
00900      IF CQ-PRE-NUMBERING-SW = SPACES                              EL687
00901        AND AACNI = 'Y'                                            EL687
00902          MOVE -1                 TO  AACNL                        EL687
00903          MOVE AL-UABON           TO  AACNA                        EL687
00904          MOVE ER-0391            TO  EMI-ERROR                    EL687
00905          PERFORM 9900-ERROR-FORMAT.                               EL687
00906                                                                   EL687
00907      GO TO 0210-MAIN-LOGIC.                                       EL687
00908                                                                   EL687
00909  0225-MAIN-LOGIC.                                                 EL687
00910      MOVE ER-0490                TO  EMI-ERROR                    EL687
00911      MOVE -1                     TO  AOPTIONL                     EL687
00912      PERFORM 8200-SEND-DATAONLY                                   EL687
00913      PERFORM 9100-RETURN-TRAN.                                    EL687
00914                                                                   EL687
00915      EJECT                                                        EL687
00916  0230-MAIN-LOGIC.                                                 EL687
00917      EXEC CICS ENDBR                                              EL687
00918          DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.                  EL687
00919                                                                   EL687
00920      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL687
00921          PERFORM 8200-SEND-DATAONLY.                              EL687
00922                                                                   EL687
00923 *    NOTE ******************************************************* EL687
00924 *         *      READ THE COMPANY RECORD FROM THE CONTROL FILE  * EL687
00925 *         *  TO GET THE CICS/VS PRINTER TERMINAL ID AND CHECK   * EL687
00926 *         *  TO SEE IF THE PRINTER HAS BEEN SPECIFIED.          * EL687
00927 *         *******************************************************.EL687
00928                                                                   EL687
00929      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.            CL**2
00930      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.             CL**2
00931      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**2
00932                                                                   EL687
00933      EXEC CICS READ                                               EL687
00934          DATASET (WS-CONTROL-FILE-DSID)                           EL687
00935          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL687
00936          SET    (ADDRESS OF CONTROL-FILE) END-EXEC.                  CL*18
00937                                                                   EL687
00938      IF CF-CHECK-PRINTER-ID = SPACES                              EL687
00939          MOVE ER-0371            TO  EMI-ERROR                    EL687
00940          MOVE -1                 TO  APFKL                        EL687
00941          PERFORM 9900-ERROR-FORMAT.                               EL687
00942                                                                   EL687
00943      MOVE SPACES                 TO  PI-ALT-DMD-PRT-ID.              CL*19
00944      IF APRTL GREATER THAN ZEROS                                     CL*17
00945          MOVE AL-UANON           TO  APRTA                           CL*17
00946          MOVE APRTI              TO  PI-CHECK-PRINTER-ID             CL*17
00947                                      PI-ALT-DMD-PRT-ID               CL*19
00948      ELSE                                                            CL*17
00949          MOVE CF-CHECK-PRINTER-ID                                    CL*17
00950                                  TO  PI-CHECK-PRINTER-ID.            CL*17
00951                                                                      CL*17
00952      MOVE CF-COMPANY-ADDRESS     TO  PI-COMPANY-ADDRESS.             CL**2
00953                                                                   EL687
00954      IF AACNI = 'Y'                                               EL687
00955          IF WS-GREATEST-CHECK-NUMBER-X NOT LESS THAN ACKNOI       EL687
00956              MOVE ER-0380        TO  EMI-ERROR                    EL687
00957              PERFORM 9900-ERROR-FORMAT                            EL687
00958              MOVE -1             TO  ACKNOL                       EL687
00959              MOVE AL-UNBON       TO  ACKNOA                       EL687
00960          ELSE                                                        CL**9
00961              SUBTRACT +1 FROM ACKNOO GIVING WS-ACKNO              EL687
00962              IF WS-GREATEST-CHECK-NUMBER-X NOT = WS-ACKNO-X       EL687
00963                  MOVE ER-0381    TO  EMI-ERROR                    EL687
00964                  PERFORM 9900-ERROR-FORMAT                        EL687
00965                  MOVE -1         TO  ACKNOL                       EL687
00966                  MOVE AL-UNBON   TO  ACKNOA                          CL**9
00967              ELSE                                                    CL**9
00968                  MOVE AL-SANON   TO  ACKNOA.                         CL**9
00969                                                                   EL687
00970      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL687
00971          PERFORM 8200-SEND-DATAONLY.                              EL687
00972                                                                   EL687
00973      MOVE AL-SANON               TO  AOPTIONA                     EL687
00974                                      ACG01A                       EL687
00975                                      ACG02A                       EL687
00976                                      ACG03A                       EL687
00977                                      ACG04A                       EL687
00978                                      AACNA                           CL*17
00979                                      APRTA.                          CL*17
00980                                                                   EL687
00981      MOVE +1                     TO  PI-PROCESSING-SW             EL687
00982                                      AALIGNL.                        CL**2
00983                                                                   EL687
00984      IF PI-NUMBER-OF-ALIGNMENT-CHECKS GREATER THAN ZERO           EL687
00985          MOVE ER-0361            TO  EMI-ERROR                    EL687
00986        ELSE                                                       EL687
00987          MOVE ER-0362            TO  EMI-ERROR.                   EL687
00988                                                                   EL687
00989      PERFORM 8200-SEND-DATAONLY.                                     CL**2
00990                                                                   EL687
00991      EXEC CICS HANDLE AID                                         EL687
00992          CLEAR (9400-CLEAR)                                       EL687
00993          PA1   (0040-MAIN-LOGIC)                                  EL687
00994          PA2   (0040-MAIN-LOGIC)                                  EL687
00995          PA3   (0040-MAIN-LOGIC) END-EXEC.                           CL**2
00996                                                                   EL687
00997      EXEC CICS SYNCPOINT                                          EL687
00998          END-EXEC.                                                   CL**2
00999                                                                   EL687
01000      GO TO 0040-MAIN-LOGIC.                                       EL687
01001                                                                   EL687
01002      EJECT                                                        EL687
01003  0240-MAIN-LOGIC.                                                 EL687
01004      IF AALIGNL GREATER THAN ZERO                                 EL687
01005          IF AALIGNI IS NUMERIC                                    EL687
01006              MOVE AALIGNO  TO  PI-NUMBER-OF-ALIGNMENT-CHECKS      EL687
01007              MOVE AL-UNNON       TO  AALIGNA                      EL687
01008            ELSE                                                   EL687
01009              MOVE ER-0365        TO  EMI-ERROR                    EL687
01010              MOVE -1             TO  AALIGNL                      EL687
01011              MOVE AL-UNBON       TO  AALIGNA                      EL687
01012              PERFORM 8200-SEND-DATAONLY                           EL687
01013              GO TO 0040-MAIN-LOGIC.                               EL687
01014                                                                   EL687
01015      IF PI-NUMBER-OF-ALIGNMENT-CHECKS NOT GREATER THAN ZERO       EL687
01016          GO TO 0300-MAIN-LOGIC.                                   EL687
01017                                                                   EL687
01018      IF PI-ALIGNMENT-CONTROL-GROUP GREATER THAN ZERO              EL687
01019          GO TO 0245-MAIN-LOGIC.                                   EL687
01020                                                                   EL687
01021      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.            CL**2
01022      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.             CL**2
01023      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**2
01024                                                                   EL687
01025      EXEC CICS READ UPDATE                                        EL687
01026          DATASET (WS-CONTROL-FILE-DSID)                           EL687
01027          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL687
01028          SET    (ADDRESS OF CONTROL-FILE) END-EXEC.                  CL*18
01029                                                                   EL687
01030      ADD +1  TO  CF-CR-CHECK-QUE-COUNTER.                            CL**2
01031                                                                   EL687
01032      IF CR-QUE-COUNT-RESET                                        EL687
01033          MOVE +1                 TO  CF-CR-CHECK-QUE-COUNTER.     EL687
01034                                                                   EL687
01035      MOVE CF-CR-CHECK-QUE-COUNTER TO PI-ALIGNMENT-CONTROL-GROUP.     CL**2
01036                                                                   EL687
01037      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.                 CL**2
01038      MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.                     CL**2
01039      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**2
01040                                                                   EL687
01041      EXEC CICS REWRITE                                            EL687
01042          DATASET (WS-CONTROL-FILE-DSID)                           EL687
01043          FROM    (CONTROL-FILE) END-EXEC.                            CL**2
01044                                                                   EL687
01045      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**2
01046                                                                   EL687
01047      MOVE EIBTRMID               TO  PI-TSK-TERM-ID.                 CL**2
01048      MOVE EIBTIME                TO  PI-TSK-TIME.                    CL**2
01049                                                                   EL687
030612     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'                      CL*19
01051 *        MOVE EIBTRMID       TO PI-CHECK-PRINTER-ID                  CL*19
01052          EXEC CICS START                                             CL*19
01053              TRANSID (WS-CHECK-WRITER-TRANS-ID)                      CL*19
01054              FROM    (PROGRAM-INTERFACE-BLOCK)                       CL*19
01055              LENGTH  (PI-COMM-LENGTH)                                CL*19
01056 *            TERMID  (PI-CHECK-PRINTER-ID)                           CL*19
01057          END-EXEC                                                    CL*19
01058      ELSE                                                            CL*19
01059          EXEC CICS START                                             CL*19
01060              TRANSID (WS-CHECK-WRITER-TRANS-ID)                      CL*19
01061              FROM    (PROGRAM-INTERFACE-BLOCK)                       CL*19
01062              LENGTH  (PI-COMM-LENGTH)                                CL*19
01063              TERMID  (PI-CHECK-PRINTER-ID)                           CL*19
01064          END-EXEC.                                                   CL*19
01065                                                                   EL687
01066      MOVE +1                     TO  PI-PRINTER-STARTED-SW.       EL687
01067                                                                   EL687
01068      EXEC CICS GETMAIN                                            EL687
01069          SET     (ADDRESS OF CHECK-QUE)                              CL*18
01070          LENGTH  (WS-ERCHKQ-LENGTH)                               EL687
01071          INITIMG (WS-SPACES) END-EXEC.                               CL**2
01072                                                                   EL687
01073  0245-MAIN-LOGIC.                                                 EL687
01074      MOVE SPACES                 TO  CHECK-QUE.                   EL687
01075                                                                   EL687
01076      MOVE 'CQ'                   TO  CQ-RECORD-ID.                   CL**2
01077                                                                   EL687
01078      MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD.                  CL**2
01079      MOVE 'A'                    TO  CQ-ENTRY-TYPE.                  CL**2
01080                                                                   EL687
01081      MOVE PI-ALIGNMENT-CONTROL-GROUP  TO  CQ-CONTROL-NUMBER.         CL**2
01082                                                                   EL687
01083      MOVE ZERO                   TO  CQ-CHECK-AMOUNT.                CL**2
01084      MOVE +1                     TO  CQ-TIMES-PRINTED.               CL**2
01085      MOVE WS-CURRENT-DATE        TO  CQ-CHECK-WRITTEN-DT.            CL**2
01086                                                                   EL687
01087      MOVE WS-CHECK-QUEUE-DSID    TO  JP-FILE-ID.                     CL**2
01088      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL687
01089                                                                   EL687
01090      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.          EL687
01091                                                                   EL687
01092  0250-MAIN-LOGIC.                                                 EL687
01093      IF AACNI = 'Y'                                               EL687
01094          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER              EL687
01095          ADD +1  TO  WS-CHECK-NUMBER                              EL687
01096          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.         EL687
01097                                                                   EL687
01098      MOVE PI-ALIGNMENT-SEQUENCE-NO  TO  CQ-SEQUENCE-NUMBER.          CL**2
01099      ADD +1  TO  PI-ALIGNMENT-SEQUENCE-NO.                           CL**2
01100                                                                   EL687
01101      MOVE SPACES                 TO  CHECK-PASS-AREA.                CL**2
01102                                                                   EL687
01103      MOVE +1                     TO  CPA-ALIGNMENT.                  CL**2
01104      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER.               CL**2
01105                                                                   EL687
01106      PERFORM 0800-PRINT-CHECK.                                       CL**2
01107                                                                   EL687
01108      IF AACNI = 'Y'                                               EL687
01109          MOVE CHECK-QUE          TO  JP-RECORD-AREA               EL687
01110          EXEC CICS WRITE                                          EL687
01111              DATASET (WS-CHECK-QUEUE-DSID)                        EL687
01112              RIDFLD  (CQ-CONTROL-PRIMARY)                         EL687
01113              FROM    (CHECK-QUE) END-EXEC                         EL687
01114          PERFORM 8400-LOG-JOURNAL-RECORD.                         EL687
01115                                                                   EL687
01116      SUBTRACT +1 FROM PI-NUMBER-OF-ALIGNMENT-CHECKS.                 CL**2
01117                                                                   EL687
01118      IF PI-NUMBER-OF-ALIGNMENT-CHECKS IS GREATER THAN ZERO        EL687
01119          GO TO 0250-MAIN-LOGIC.                                   EL687
01120                                                                   EL687
01121      MOVE SPACES                 TO  AALIGNI.                        CL**2
01122      MOVE AL-UNNOF               TO  AALIGNA.                        CL**2
01123      MOVE -1                     TO  AALIGNL.                        CL**2
01124      MOVE ER-0362                TO  EMI-ERROR.                      CL**2
01125      PERFORM 8200-SEND-DATAONLY.                                     CL**2
01126                                                                   EL687
01127      EXEC CICS SYNCPOINT                                          EL687
01128          END-EXEC.                                                EL687
01129                                                                   EL687
01130      GO TO 0040-MAIN-LOGIC.                                       EL687
01131                                                                   EL687
01132      EJECT                                                        EL687
01133  0300-MAIN-LOGIC.                                                 EL687
01134      IF PI-PRINTER-STARTED-SW = ZERO                              EL687
01135          MOVE +1                 TO  PI-PRINTER-STARTED-SW        EL687
01136          MOVE EIBTRMID TO PI-TSK-TERM-ID                          EL687
01137          MOVE EIBTIME  TO PI-TSK-TIME                             EL687
01138                                                                      CL*19
030612         IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'                  CL*19
01140 *            MOVE EIBTRMID       TO PI-CHECK-PRINTER-ID              CL*19
01141              EXEC CICS START                                         CL*19
01142                  TRANSID (WS-CHECK-WRITER-TRANS-ID)                  CL*19
01143                  FROM    (PROGRAM-INTERFACE-BLOCK)                   CL*19
01144                  LENGTH  (PI-COMM-LENGTH)                            CL*19
01145 *                TERMID  (PI-CHECK-PRINTER-ID)                       CL*19
01146              END-EXEC                                                CL*19
01147          ELSE                                                        CL*19
01148              EXEC CICS START                                         CL*19
01149                  TRANSID (WS-CHECK-WRITER-TRANS-ID)                  CL*19
01150                  FROM    (PROGRAM-INTERFACE-BLOCK)                   CL*19
01151                  LENGTH  (PI-COMM-LENGTH)                            CL*19
01152                  TERMID  (PI-CHECK-PRINTER-ID)                       CL*19
01153              END-EXEC.                                               CL*19
01154                                                                   EL687
01155      SET PI-INDEX TO +1.                                             CL**2
01156                                                                   EL687
01157      MOVE LOW-VALUES             TO  WS-CHECK-QUEUE-KEY.             CL**2
01158      MOVE +5                     TO  WS-KEY-LENGTH.                  CL**2
01159                                                                   EL687
01160      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD.           EL687
01161                                                                   EL687
01162  0310-MAIN-LOGIC.                                                 EL687
01163      IF AOPTIONI = '1'                                            EL687
01164          EXEC CICS STARTBR                                        EL687
01165              DATASET (WS-CHECK-QUEUE-DSID)                        EL687
01166              RIDFLD  (WS-CHECK-QUEUE-KEY)                         EL687
01167              GTEQ    END-EXEC                                     EL687
01168        ELSE                                                       EL687
01169          MOVE PI-CONTROL-GROUP (PI-INDEX) TO WS-CQK-CONTROL-NUMBEREL687
01170          ADD +1  PI-HIGH-SEQUENCE (PI-INDEX)                      EL687
01171              GIVING WS-SEQUENCE-NUMBER                            EL687
01172          MOVE ZERO               TO  WS-CQK-SEQUENCE-NUMBER       EL687
01173          IF WS-CHECK-QUEUE-BROWSE-SW = ZERO                       EL687
01174              EXEC CICS STARTBR                                    EL687
01175                  DATASET   (WS-CHECK-QUEUE-DSID)                  EL687
01176                  RIDFLD    (WS-CHECK-QUEUE-KEY)                   EL687
01177                  KEYLENGTH (WS-KEY-LENGTH)                        EL687
01178                  GENERIC   EQUAL END-EXEC                         EL687
01179              MOVE +1             TO  WS-CHECK-QUEUE-BROWSE-SW     EL687
01180            ELSE                                                   EL687
01181              EXEC CICS RESETBR                                    EL687
01182                  DATASET   (WS-CHECK-QUEUE-DSID)                  EL687
01183                  RIDFLD    (WS-CHECK-QUEUE-KEY)                   EL687
01184                  KEYLENGTH (WS-KEY-LENGTH)                        EL687
01185                  GENERIC   EQUAL END-EXEC.                        EL687
01186                                                                   EL687
01187      EJECT                                                        EL687
01188  0320-MAIN-LOGIC.                                                 EL687
01189      EXEC CICS HANDLE CONDITION                                   EL687
01190          NOTFND                                                   EL687
01191          ENDFILE (0390-MAIN-LOGIC) END-EXEC.                      EL687
01192                                                                   EL687
01193  0325-MAIN-LOGIC.                                                 EL687
01194                                                                      CL**9
01195      EXEC CICS READNEXT                                           EL687
01196          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
01197          RIDFLD  (WS-CHECK-QUEUE-KEY)                             EL687
01198          SET     (ADDRESS OF CHECK-QUE) END-EXEC.                    CL*18
01199                                                                   EL687
01200      IF WS-CHECK-QUEUE-KEY NOT GREATER WS-LAST-CHECK-QUEUE-KEY    EL687
01201          GO TO 0325-MAIN-LOGIC.                                   EL687
01202                                                                   EL687
01203      MOVE WS-CHECK-QUEUE-KEY    TO WS-LAST-CHECK-QUEUE-KEY.       EL687
01204                                                                   EL687
01205      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL687
01206          GO TO 0390-MAIN-LOGIC.                                   EL687
01207                                                                   EL687
01208      IF AOPTIONI = ('2' OR '3')                                   EL687
01209          IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (PI-INDEX)       EL687
01210           AND CQ-SEQUENCE-NUMBER NOT GREATER THAN PI-HIGH-SEQUENCEEL687
01211                                                        (PI-INDEX) EL687
01212              NEXT SENTENCE                                        EL687
01213            ELSE                                                   EL687
01214              GO TO 0390-MAIN-LOGIC.                               EL687
01215                                                                   EL687
01216      IF CQ-ENTRY-TYPE NOT = 'Q'                                   EL687
01217          GO TO 0325-MAIN-LOGIC.                                   EL687
01218                                                                   EL687
01219      IF (AOPTIONI NOT = '3' AND                                   EL687
01220          CQ-TIMES-PRINTED GREATER THAN ZERO)                      EL687
01221        OR                                                         EL687
01222         (AOPTIONI = '3' AND                                       EL687
01223          CQ-TIMES-PRINTED NOT GREATER THAN ZERO)                  EL687
01224              GO TO 0325-MAIN-LOGIC.                               EL687
01225                                                                   EL687
01226      EXEC CICS ENDBR                                              EL687
01227          DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.                     CL**2
01228                                                                   EL687
01229      EXEC CICS READ UPDATE                                        EL687
01230          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
01231          RIDFLD  (WS-CHECK-QUEUE-KEY)                             EL687
01232          SET     (ADDRESS OF CHECK-QUE) END-EXEC.                    CL*18
01233                                                                   EL687
01234      MOVE CHECK-QUE              TO  WS-OLD-CHECK-QUEUE-RECORD.   EL687
01235                                                                   EL687
01236      IF AACNI = 'Y'                                               EL687
01237          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER              EL687
01238          ADD +1  TO  WS-CHECK-NUMBER                              EL687
01239          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.         EL687
01240                                                                   EL687
01241 *    NOTE ******************************************************* EL687
01242 *         *                PRINT THE CHECK                      * EL687
01243 *         *******************************************************.EL687
01244                                                                   EL687
01245      IF CQ-CHECK-MAINT-PMT OR                                        CL**9
01246         CQ-REFUND-PMT                                                CL**9
01247          GO TO 0350-MAIN-LOGIC.                                   EL687
01248                                                                      CL**9
01249 *    NOTE *******************************************************    CL**9
01250 *         *                PRINT BILLING CHECKS                 *    CL**9
01251 *         *******************************************************.   CL**9
01252                                                                   EL687
01253      MOVE SPACES                 TO  CHECK-PASS-AREA.                CL**2
01254      MOVE LOW-VALUES             TO  WS-COMPENSATION-MASTER-KEY   EL687
01255                                      WS-PENDING-PAYMENTS-KEY.     EL687
01256      MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD             EL687
01257                                      WS-PPK-COMPANY-CD.              CL**2
01258      MOVE CQ-PYAJ-CARRIER        TO  WS-CM-CARRIER                EL687
01259                                      CPA-CARRIER                  EL687
01260                                      WS-PPK-CARRIER.                 CL**2
01261      MOVE CQ-PYAJ-GROUPING       TO  WS-CM-GROUPING               EL687
01262                                      CPA-GROUPING                 EL687
01263                                      WS-PPK-GROUPING.                CL**2
01264      MOVE CQ-PYAJ-FIN-RESP       TO  WS-CM-FIN-RESP               EL687
01265                                      CPA-FIN-RESP                 EL687
01266                                      WS-PPK-FIN-RESP.                CL**2
01267      MOVE CQ-PYAJ-ACCOUNT        TO  WS-CM-ACCOUNT                EL687
01268                                      CPA-ACCOUNT                  EL687
01269                                      WS-PPK-ACCOUNT.              EL687
01270      MOVE CQ-PYAJ-SEQ            TO  WS-PPK-FILE-SEQ-NO.          EL687
01271      MOVE 'C'                    TO  WS-PPK-RECORD-TYPE.          EL687
01272                                                                   EL687
01273      IF CQ-PYAJ-ACCOUNT NOT = LOW-VALUES                          EL687
01274         MOVE 'A'                 TO  WS-CM-TYPE                   EL687
01275      ELSE                                                            CL**9
01276         MOVE 'G'                 TO  WS-CM-TYPE.                     CL**9
01277                                                                   EL687
01278      EXEC CICS READ                                               EL687
01279          DATASET (WS-COMPENSATION-MASTER-DSID)                    EL687
01280          RIDFLD  (WS-COMPENSATION-MASTER-KEY)                     EL687
01281          SET     (ADDRESS OF COMPENSATION-MASTER) END-EXEC.          CL*18
01282                                                                   EL687
01283      EXEC CICS READ UPDATE                                        EL687
01284          DATASET (WS-PENDING-PAYMENTS-DSID)                       EL687
01285          RIDFLD  (WS-PENDING-PAYMENTS-KEY)                        EL687
01286          SET     (ADDRESS OF PENDING-PAY-ADJ) END-EXEC.              CL*18
01287                                                                   EL687
01288      MOVE ZERO                   TO  CPA-ALIGNMENT.                  CL**2
01289      MOVE SPACES                 TO  CPA-CERT-NO.                    CL**2
01290      MOVE ZEROS                  TO  WS-CHECK-NO.                    CL**2
01291      MOVE CQ-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.               CL*12
01292      MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.                 CL**2
01293      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER             EL687
01294                                      WS-CHECK-NO.                    CL**2
01295      MOVE WS-CHECK-WORK          TO  PY-CHECK-NUMBER.                CL**2
01296      MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.                CL**2
01297      MOVE ZEROS                  TO  CPA-AMOUNT-PAID-TO-DATE      EL687
01298                                      CPA-NO-OF-PMTS-MADE             CL*12
01299                                      CPA-LF-REFUND                   CL*12
01300                                      CPA-AH-REFUND                   CL*12
01301                                      CPA-DEDUCT-WITHHELD             CL*12
01302                                      CPA-ADDITIONAL-CHARGE.          CL*13
01303      MOVE PY-REPORTED-DT         TO  CPA-REPORTED-DT.                CL**2
01304      MOVE LOW-VALUES             TO  CPA-INCURRED-DT              EL687
01305                                      CPA-PAID-THRU-DT             EL687
01306                                      CPA-PAID-FROM-DT             EL687
01307                                      CPA-PAID-DT                     CL*12
01308                                      CPA-CERT-EFF-DT.                CL*12
01309      MOVE ZEROS                  TO  CPA-CAR-ZIP-CODE.            EL687
01310      MOVE CO-MAIL-NAME           TO  CPA-PAYEE-IN-CARE-OF.           CL**2
01311      MOVE CO-ACCT-NAME           TO  CPA-PAYEE-NAME.                 CL**2
01312      MOVE CO-ADDR-1              TO  CPA-PAYEE-ADDRESS-LINE1.        CL**2
01313      MOVE CO-ADDR-2              TO  CPA-PAYEE-ADDRESS-LINE2.        CL**2
01314 *    MOVE CO-ADDR-3              TO  CPA-PAYEE-CITY-ST.              CL**2
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO CPA-PAYEE-CITY-ST
           END-STRING
01315      MOVE CO-TELEPHONE           TO  CPA-PAYEE-PHONE-NO.             CL**2
01316      MOVE CO-ZIP                 TO  CPA-PAYEE-ZIP-CODE.             CL**7
01317                                                                   EL687
01318      IF NOT CO-CANADIAN-POST-CODE                                    CL**7
01319          INSPECT CPA-PAYEE-ZIP-CODE REPLACING ALL ' ' BY ZEROS       CL**7
01320          IF CPA-PAYEE-ZIP-PRIME NUMERIC                              CL**7
01321              MOVE SPACES         TO CPA-PAYEE-ZIP-PLUS4              CL**7
01322          ELSE                                                        CL**7
01323              MOVE ZEROS          TO CPA-PAYEE-ZIP-CODE.              CL**7
01324                                                                   EL687
01325      MOVE SPACES                 TO  WS-SSN-STATE                 EL687
01326                                      WS-MEMBER-STATE              EL687
01327                                      CPA-MEMBER-NUMBER            EL687
01328                                      WS-SSN-LN3                   EL687
01329                                      WS-MEMBER-LN4.               EL687
01330      MOVE CO-ACCT-PRIME          TO  WS-SSN-ACCOUNT               EL687
01331                                      WS-MEMBER-ACCOUNT.              CL**2
01332      MOVE CO-SOC-SEC             TO  CPA-SOC-SEC-NO.              EL687
01333      MOVE WS-CURRENT-DATE        TO  CPA-CHECK-DATE               EL687
01334                                      CQ-CHECK-WRITTEN-DT          EL687
01335                                      PY-CHECK-WRITTEN-DT.         EL687
01336                                                                      CL**6
01337      IF  PY-LETTER (1) GREATER THAN SPACES                           CL*15
01338          MOVE 'Y'                TO  PI-LETTERS-IND                  CL*15
01339                                                                      CL*15
01340      ELSE                                                            CL*15
01341          MOVE LOW-VALUES         TO  PI-LETTERS-IND.                 CL*15
01342                                                                      CL*15
01343      IF PI-COMPANY-ID = 'ITY'                                        CL**6
01344          MOVE PY-ENTRY-COMMENT   TO  CPA-STUB5.                      CL**6
01345                                                                      CL**6
01346      IF AOPTIONI = '3'                                            EL687
01347        AND CQ-PRE-NUMBERING-SW = '1'                              EL687
01348          MOVE WS-SEQUENCE-NUMBER TO  PY-CHECK-QUE-SEQUENCE.       EL687
01349                                                                   EL687
01350      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**2
01351                                                                   EL687
01352      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.          EL687
01353                                                                   EL687
01354      EXEC CICS REWRITE                                            EL687
01355          DATASET (WS-PENDING-PAYMENTS-DSID)                       EL687
01356          FROM    (PENDING-PAY-ADJ)  END-EXEC.                        CL**2
01357                                                                   EL687
01358      MOVE WS-PENDING-PAYMENTS-DSID TO  JP-FILE-ID.                   CL**2
01359      MOVE 'C'                      TO  JP-RECORD-TYPE.               CL**2
01360      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL687
01361                                                                   EL687
01362      GO TO 0375-MAIN-LOGIC.                                       EL687
01363                                                                   EL687
01364      EJECT                                                           CL**9
01365  0350-MAIN-LOGIC.                                                 EL687
01366 *    NOTE *******************************************************    CL**9
01367 *         *         PRINT REFUND OR MISCELLANEOUS CHECKS        *    CL**9
01368 *         *******************************************************.   CL**9
01369                                                                      CL**9
01370      MOVE SPACES                 TO  CHECK-PASS-AREA.                CL**2
01371      MOVE ZERO                   TO  CPA-ALIGNMENT.                  CL**2
01372      MOVE LOW-VALUES             TO  WS-CHECK-MAINT-KEY.             CL**2
01373      MOVE PI-COMPANY-CD          TO  WS-CMK-COMPANY-CD.              CL**2
01374      MOVE CQ-CHEK-CARRIER        TO  WS-CMK-CARRIER               EL687
01375                                      CPA-CARRIER.                    CL**2
01376      MOVE CQ-CHEK-GROUPING       TO  WS-CMK-GROUPING              EL687
01377                                      CPA-GROUPING.                   CL**2
01378      MOVE CQ-CHEK-STATE          TO  WS-CMK-STATE                 EL687
01379                                      CPA-STATE.                      CL**2
01380      MOVE CQ-CHEK-ACCOUNT        TO  WS-CMK-ACCOUNT               EL687
01381                                      CPA-ACCOUNT.                    CL**2
01382      MOVE CQ-CHEK-CERT-NO        TO  WS-CMK-CERT-NO               EL687
01383                                      CPA-CERT-NO.                 EL687
01384      MOVE CQ-CHEK-CERT-EFF-DT    TO  WS-CMK-CERT-EFF-DT.          EL687
01385      MOVE CQ-CHEK-SEQ-NO         TO  WS-CMK-SEQ-NO.               EL687
01386                                                                   EL687
01387      EXEC CICS READ UPDATE                                        EL687
01388          DATASET (WS-CHECK-MAINT-DSID)                            EL687
01389          RIDFLD  (WS-CHECK-MAINT-KEY)                             EL687
01390          SET     (ADDRESS OF CHECK-RECORDS) END-EXEC.                CL*18
01391                                                                   EL687
01392      IF CH-LF-REFUND NOT NUMERIC                                     CL*12
01393          MOVE ZEROS              TO  CH-LF-REFUND.                   CL*12
01394      IF CH-AH-REFUND NOT NUMERIC                                     CL*12
01395          MOVE ZEROS              TO  CH-AH-REFUND.                   CL*12
01396      IF CH-DEDUCT-WITHHELD NOT NUMERIC                               CL*12
01397          MOVE ZEROS              TO  CH-DEDUCT-WITHHELD.             CL*12
01398      IF CH-ADDITIONAL-CHARGE NOT NUMERIC                             CL*13
01399          MOVE ZEROS              TO  CH-ADDITIONAL-CHARGE.           CL*13
01400                                                                      CL*12
01401      MOVE ZERO                   TO  CPA-ALIGNMENT.                  CL**2
01402      MOVE CQ-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.               CL*12
01403      MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.                 CL**2
01404      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER             EL687
01405                                      CH-CHECK-NO.                    CL**2
01406      MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.                CL**2
01407      MOVE ZEROS                  TO  CPA-AMOUNT-PAID-TO-DATE      EL687
01408                                      CPA-NO-OF-PMTS-MADE.            CL**2
01409      MOVE CH-RECORDED-DT         TO  CPA-REPORTED-DT.                CL**2
01410      MOVE LOW-VALUES             TO  CPA-INCURRED-DT              EL687
01411                                      CPA-PAID-THRU-DT             EL687
01412                                      CPA-PAID-FROM-DT             EL687
01413                                      CPA-PAID-DT.                 EL687
01414      MOVE ZEROS                  TO  CPA-CAR-ZIP-CODE.            EL687
01415      MOVE CH-PAYEE-NAME-1        TO  CPA-PAYEE-NAME.                 CL**2
01416      MOVE CH-PAYEE-NAME-2        TO  CPA-PAYEE-IN-CARE-OF.           CL**2
01417      MOVE CH-PAYEE-ADDRESS-1     TO  CPA-PAYEE-ADDRESS-LINE1.        CL**9
01418                                                                   EL687
01419      IF CH-PAYEE-CITY-ST EQUAL SPACES                             EL687
01420          MOVE CH-PAYEE-ADDRESS-2                                     CL**9
01421                                  TO  CPA-PAYEE-CITY-ST               CL**9
01422          MOVE SPACES             TO  CPA-PAYEE-ADDRESS-LINE2         CL**9
01423      ELSE                                                         EL687
01424          MOVE CH-PAYEE-ADDRESS-2                                     CL**9
01425                                  TO  CPA-PAYEE-ADDRESS-LINE2         CL**9
01426          MOVE CH-PAYEE-CITY-ST   TO  CPA-PAYEE-CITY-ST.           EL687
01427                                                                   EL687
01428      MOVE CH-STUB-LINE-1         TO  CPA-STUB1.                      CL**2
01429      MOVE CH-STUB-LINE-2         TO  CPA-STUB2.                      CL**2
01430      MOVE CH-STUB-LINE-3         TO  CPA-STUB3.                      CL**2
01431      MOVE CH-STUB-LINE-4         TO  CPA-STUB4.                      CL**2
01432      MOVE CH-PAYEE-ZIP-CODE      TO  CPA-PAYEE-ZIP-CODE.          EL687
01433                                                                      CL**9
01434      IF CQ-REFUND-PMT                                                CL**9
01435          MOVE CH-CANC-DT         TO  CPA-CANC-DT                     CL**9
01436          MOVE CH-CERT-EFF-DT     TO  CPA-CERT-EFF-DT                 CL*12
01437          MOVE CH-INSURED-NAME    TO  CPA-INSURED-NAME                CL**9
01438      ELSE                                                            CL**9
01439          MOVE LOW-VALUES         TO  CPA-CANC-DT                     CL**9
01440                                      CPA-CERT-EFF-DT                 CL*12
01441          MOVE SPACES             TO  CPA-INSURED-NAME.               CL*14
01442                                                                      CL*14
01443      MOVE CH-LF-REFUND           TO  CPA-LF-REFUND.                  CL*14
01444      MOVE CH-AH-REFUND           TO  CPA-AH-REFUND.                  CL*14
01445      MOVE CH-DEDUCT-WITHHELD     TO  CPA-DEDUCT-WITHHELD.            CL*14
01446      MOVE CH-ADDITIONAL-CHARGE   TO  CPA-ADDITIONAL-CHARGE.          CL*14
01447                                                                   EL687
01448      MOVE SPACES                 TO  WS-SSN-STATE                 EL687
01449                                      WS-MEMBER-STATE              EL687
01450                                      CPA-MEMBER-NUMBER            EL687
01451                                      WS-SSN-LN3                   EL687
01452                                      WS-MEMBER-LN4.               EL687
01453      MOVE CO-ACCT-PRIME          TO  WS-SSN-ACCOUNT               EL687
01454                                      WS-MEMBER-ACCOUNT.              CL**2
01455      MOVE CO-SOC-SEC             TO  CPA-SOC-SEC-NO.              EL687
01456      MOVE WS-CURRENT-DATE        TO  CPA-CHECK-DATE               EL687
01457                                      CQ-CHECK-WRITTEN-DT          EL687
01458                                      CH-CHECK-WRITTEN-DT.         EL687
01459      IF AOPTIONI = '3'                                            EL687
01460        AND CQ-PRE-NUMBERING-SW = '1'                              EL687
01461          MOVE WS-SEQUENCE-NUMBER TO  CH-CHECK-QUE-SEQUENCE.       EL687
01462                                                                   EL687
01463      MOVE CHECK-RECORDS          TO  JP-RECORD-AREA.                 CL**2
01464                                                                   EL687
01465      IF  CH-LETTERS (1) GREATER THAN SPACES                          CL*15
01466          MOVE 'Y'                TO  PI-LETTERS-IND                  CL*15
01467                                                                      CL*15
01468      ELSE                                                            CL*15
01469          MOVE LOW-VALUES         TO  PI-LETTERS-IND.                 CL*15
01470                                                                      CL*15
01471      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.          EL687
01472                                                                   EL687
01473      EXEC CICS REWRITE                                            EL687
01474          DATASET (WS-CHECK-MAINT-DSID)                            EL687
01475          FROM    (CHECK-RECORDS)  END-EXEC.                          CL**2
01476                                                                   EL687
01477      MOVE WS-CHECK-MAINT-DSID    TO  JP-FILE-ID.                     CL**2
01478      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**2
01479      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL687
01480                                                                      CL**9
01481      EJECT                                                        EL687
01482                                                                   EL687
01483  0375-MAIN-LOGIC.                                                 EL687
01484      ADD +1  TO  CQ-TIMES-PRINTED.                                   CL**2
01485                                                                   EL687
01486      IF AOPTIONI = '3'                                            EL687
01487        AND CQ-PRE-NUMBERING-SW = '1'                              EL687
01488          MOVE CHECK-QUE          TO  WS-NEW-CHECK-QUEUE-RECORD    EL687
01489          MOVE WS-OLD-CHECK-QUEUE-RECORD  TO  CHECK-QUE            EL687
01490          MOVE 'S'                TO  CQ-ENTRY-TYPE.               EL687
01491                                                                   EL687
01492      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.          EL687
01493                                                                   EL687
01494      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**2
01495      MOVE CHECK-QUE              TO  JP-RECORD-AREA.                 CL**2
01496      MOVE WS-CHECK-QUEUE-DSID    TO  JP-FILE-ID.                     CL**2
01497      MOVE PI-LETTERS-IND         TO  CQ-LETTERS-IND.                 CL*15
01498                                                                   EL687
01499      EXEC CICS REWRITE                                            EL687
01500          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
01501          FROM    (CHECK-QUE) END-EXEC.                               CL**2
01502                                                                   EL687
01503      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**2
01504                                                                   EL687
01505      IF AOPTIONI NOT = '3'                                        EL687
01506          GO TO 0380-MAIN-LOGIC.                                   EL687
01507                                                                   EL687
01508      IF LCP-ONCTR-01 =  0                                            CL*18
01509          ADD 1 TO LCP-ONCTR-01                                       CL*18
01510                               ,                                      CL*18
01511          EXEC CICS GETMAIN                                        EL687
01512              SET    (WS-CQFCBAR)                                  EL687
01513              LENGTH  (WS-ERCHKQ-LENGTH)                           EL687
01514              END-EXEC.                                            EL687
01515                                                                   EL687
01516      MOVE WS-CQFCBAR             TO LCP-WS-ADDR-COMP                 CL*18
01517      SET ADDRESS OF CHECK-QUE TO LCP-WS-ADDR-PNTR.                   CL*18
01518                                                                   EL687
01519      MOVE WS-NEW-CHECK-QUEUE-RECORD  TO  CHECK-QUE.                  CL**2
01520                                                                   EL687
01521      IF CQ-PRE-NUMBERING-SW NOT = '1'                             EL687
01522          GO TO 0380-MAIN-LOGIC.                                   EL687
01523                                                                   EL687
01524      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.                  CL**2
01525      MOVE PI-LETTERS-IND         TO  CQ-LETTERS-IND.                 CL*15
01526                                                                   EL687
01527      MOVE WS-SEQUENCE-NUMBER     TO  CQ-SEQUENCE-NUMBER.             CL**2
01528                                                                   EL687
01529      ADD +1  TO  WS-SEQUENCE-NUMBER.                                 CL**2
01530                                                                   EL687
01531      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.          EL687
01532                                                                   EL687
01533      MOVE CHECK-QUE              TO  JP-RECORD-AREA.                 CL**2
01534                                                                   EL687
01535      EXEC CICS WRITE                                              EL687
01536          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
01537          RIDFLD  (CQ-CONTROL-PRIMARY)                             EL687
01538          FROM    (CHECK-QUE) END-EXEC.                               CL**2
01539                                                                   EL687
01540      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**2
01541      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL687
01542                                                                   EL687
01543  0380-MAIN-LOGIC.                                                 EL687
01544      PERFORM 0800-PRINT-CHECK.                                       CL**2
01545                                                                   EL687
01546      EXEC CICS STARTBR                                            EL687
01547          DATASET (WS-CHECK-QUEUE-DSID)                            EL687
01548          RIDFLD  (WS-CHECK-QUEUE-KEY)                             EL687
01549          EQUAL   END-EXEC.                                           CL**2
01550                                                                   EL687
01551      GO TO 0320-MAIN-LOGIC.                                       EL687
01552                                                                   EL687
01553      EJECT                                                        EL687
01554  0390-MAIN-LOGIC.                                                 EL687
01555      IF AOPTIONI NOT = '1'                                        EL687
01556        AND PI-INDEX LESS THAN PI-NUMBER-OF-CONTROL-GROUPS         EL687
01557          SET PI-INDEX UP BY +1                                    EL687
01558          MOVE LOW-VALUES         TO WS-LAST-CHECK-QUEUE-KEY          CL*10
01559          GO TO 0310-MAIN-LOGIC.                                   EL687
01560                                                                   EL687
01561      EXEC CICS ENDBR                                              EL687
01562          DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.                  EL687
01563                                                                   EL687
01564      IF PI-CHECK-PRINTER-ID NOT = 'R2T7'                          EL687
01565          PERFORM 0700-END-PRINT.                                  EL687
01566                                                                   EL687
01567      MOVE PI-PRINTER-STARTED-SW  TO  WS-PRINTER-STARTED-SW.          CL**2
01568      MOVE PI-TEMP-STORAGE-KEY    TO  WS-TEMP-STORAGE-KEY.            CL**2
01569      ADD +1  TO  WS-COMPLETED-SUCCESSFUL.                            CL**2
01570      GO TO 0015-MAIN-LOGIC.                                       EL687
01571                                                                   EL687
01572  0700-END-PRINT SECTION.                                          EL687
01573      MOVE HIGH-VALUES            TO  CHECK-PASS-AREA.                CL**2
01574      MOVE +1                     TO  WS-TS-LENGTH.                   CL**2
01575                                                                   EL687
01576      PERFORM 0800-PRINT-CHECK.                                    EL687
01577                                                                   EL687
01578      MOVE ZERO                   TO  PI-PRINTER-STARTED-SW.       EL687
01579                                                                   EL687
01580  0700-EXIT.                                                       EL687
01581      EXIT.                                                        EL687
01582                                                                   EL687
01583  0800-PRINT-CHECK SECTION.                                        EL687
01584      EXEC CICS WRITEQ TS                                          EL687
01585           QUEUE  (PI-TEMP-STORAGE-KEY)                            EL687
01586           ITEM   (WS-TEMP-STORAGE-ITEM)                           EL687
01587           FROM   (CHECK-PASS-AREA)                                EL687
01588           LENGTH (WS-TS-LENGTH) END-EXEC.                         EL687
01589                                                                   EL687
01590  0800-EXIT.                                                       EL687
01591      EXIT.                                                        EL687
01592                                                                   EL687
01593      EJECT                                                        EL687
01594  0900-TERMIDERR SECTION.                                          EL687
01595                                                                   EL687
01596      EXEC CICS SYNCPOINT                                          EL687
01597          ROLLBACK END-EXEC.                                          CL**2
01598                                                                   EL687
01599      MOVE ER-0371                TO  EMI-ERROR.                      CL**2
01600      MOVE -1                     TO  APFKL.                          CL**2
01601      PERFORM 8200-SEND-DATAONLY.                                  EL687
01602      PERFORM 9100-RETURN-TRAN.                                       CL*10
01603                                                                   EL687
01604  0910-ENQ-BUSY.                                                   EL687
01605      MOVE ER-0395                TO  EMI-ERROR.                      CL**2
01606      MOVE -1                     TO  AOPTIONL.                       CL**2
01607      PERFORM 8200-SEND-DATAONLY.                                  EL687
01608      PERFORM 9100-RETURN-TRAN.                                       CL*10
01609                                                                   EL687
01610      EJECT                                                        EL687
01611 *5000-MOVE-NAME SECTION. COPY ELCMNS.                             EL687
01612                                                                   EL687
01613      EJECT                                                        EL687
01614  8100-SEND-INITIAL-MAP SECTION.                                   EL687
01615                                                                   EL687
01616      IF EMI-ERROR NOT = ZERO                                      EL687
01617          PERFORM 9900-ERROR-FORMAT                                EL687
01618        ELSE                                                       EL687
01619          IF TRANSACTION-SUCCESSFUL                                EL687
01620              PERFORM 9900-ERROR-FORMAT                            EL687
01621              IF CHECKS-WITHOUT-ADDRESSES                          EL687
01622                  MOVE ER-0364    TO  EMI-ERROR                    EL687
01623                  PERFORM 9900-ERROR-FORMAT.                       EL687
01624                                                                   EL687
01625      MOVE EIBTIME                TO  WS-TIME-WORK.                EL687
01626      EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)                        CL*18
01627      END-EXEC                                                        CL*18
01628      EXEC CICS FORMATTIME                                            CL*18
01629                ABSTIME(LCP-CICS-DATE)                                CL*18
01630                YYMMDD(LCP-CURRENT-DATE-68)                           CL*18
01631                DATESEP('/')                                          CL*18
01632      END-EXEC                                                        CL*18
01633                                                                   EL687
01634      MOVE LCP-CURRENT-DATE-68 TO ADATEO.                             CL*18
01635      MOVE WS-TIME                TO  ATIMEO.                         CL**2
01636      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                        CL**2
01637      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                        CL**2
01638      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.                        CL**2
01639                                                                   EL687
01640      EXEC CICS SEND                                               EL687
01641          FROM   (EL687AI)                                         EL687
01642          MAPSET (WS-MAPSET-NAME)                                  EL687
01643          MAP    (WS-MAP-NAME)                                     EL687
01644          CURSOR ERASE END-EXEC.                                   EL687
01645                                                                   EL687
01646      PERFORM 9100-RETURN-TRAN.                                    EL687
01647                                                                   EL687
01648  8100-EXIT.                                                       EL687
01649      EXIT.                                                        EL687
01650                                                                   EL687
01651      EJECT                                                        EL687
01652  8200-SEND-DATAONLY SECTION.                                      EL687
01653                                                                   EL687
01654      IF EMI-ERROR NOT = 3130                                         CL**4
01655          PERFORM 9900-ERROR-FORMAT                                EL687
01656        ELSE                                                       EL687
01657          IF TRANSACTION-SUCCESSFUL                                EL687
01658              PERFORM 9900-ERROR-FORMAT                            EL687
01659              IF CHECKS-WITHOUT-ADDRESSES                          EL687
01660                  MOVE ER-0364    TO  EMI-ERROR                    EL687
01661                  PERFORM 9900-ERROR-FORMAT.                       EL687
01662                                                                   EL687
01663      MOVE EIBTIME                TO  WS-TIME-WORK.                EL687
01664      EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)                        CL*18
01665      END-EXEC                                                        CL*18
01666      EXEC CICS FORMATTIME                                            CL*18
01667                ABSTIME(LCP-CICS-DATE)                                CL*18
01668                YYMMDD(LCP-CURRENT-DATE-68)                           CL*18
01669                DATESEP('/')                                          CL*18
01670      END-EXEC                                                        CL*18
01671                                                                   EL687
01672      MOVE LCP-CURRENT-DATE-68 TO ADATEO.                             CL*18
01673      MOVE WS-TIME                TO  ATIMEO.                         CL**2
01674      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                        CL**2
01675      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                        CL**2
01676      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.                        CL**2
01677                                                                   EL687
01678      EXEC CICS SEND DATAONLY                                      EL687
01679          FROM   (EL687AI)                                         EL687
01680          MAPSET (WS-MAPSET-NAME)                                  EL687
01681          MAP    (WS-MAP-NAME)                                     EL687
01682          CURSOR END-EXEC.                                         EL687
01683                                                                   EL687
01684      IF PI-PROCESSING-SW = ZERO                                   EL687
01685          PERFORM 9100-RETURN-TRAN.                                EL687
01686                                                                   EL687
01687      MOVE ZERO                   TO  EMI-SUB                      EL687
01688                                      EMI-NOTE-CTR                 EL687
01689                                      EMI-WARNING-CTR              EL687
01690                                      EMI-FORCABLE-CTR             EL687
01691                                      EMI-FATAL-CTR.                  CL**2
01692                                                                   EL687
01693      MOVE '1'                    TO  EMI-SWITCH-AREA-1            EL687
01694                                      EMI-SWITCH-AREA-2.              CL**2
01695                                                                   EL687
01696      MOVE SPACES                 TO  EMI-ERROR-LINES.             EL687
01697                                                                   EL687
01698  8200-EXIT.                                                       EL687
01699      EXIT.                                                        EL687
01700                                                                   EL687
01701      EJECT                                                        EL687
01702  8300-SEND-TEXT SECTION.                                          EL687
01703                                                                   EL687
01704      IF PI-PRINTER-STARTED-SW NOT = ZERO                          EL687
01705          PERFORM 0700-END-PRINT.                                  EL687
01706                                                                   EL687
01707      EXEC CICS SEND TEXT                                          EL687
01708          FROM   (LOGOFF-TEXT)                                     EL687
01709          LENGTH (LOGOFF-LENGTH)                                   EL687
01710          ERASE  FREEKB END-EXEC.                                  EL687
01711                                                                   EL687
01712      EXEC CICS RETURN                                             EL687
01713          END-EXEC.                                                EL687
01714                                                                   EL687
01715                                                                   EL687
01716  8300-EXIT.                                                       EL687
01717      EXIT.                                                        EL687
01718                                                                   EL687
01719      EJECT                                                        EL687
01720  8400-LOG-JOURNAL-RECORD SECTION.                                 EL687
01721                                                                   EL687
01722      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                     CL**2
01723      MOVE WS-PROGRAM-ID          TO  JP-PROGRAM-ID.                  CL**2
01724                                                                   EL687
pemuni*    EXEC CICS JOURNAL                                            EL687
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)                             EL687
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)                             EL687
pemuni*        FROM    (JOURNAL-RECORD)                                 EL687
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH) END-EXEC.             EL687
01730                                                                   EL687
01731  8400-EXIT.                                                       EL687
01732      EXIT.                                                        EL687
01733                                                                   EL687
01734      EJECT                                                        EL687
01735  8500-DATE-CONVERSION SECTION.                                    EL687
01736                                                                   EL687
01737      EXEC CICS LINK                                               EL687
01738          PROGRAM  ('ELDATCV')                                        CL**3
01739          COMMAREA (DATE-CONVERSION-DATA)                          EL687
01740          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL687
01741                                                                   EL687
01742                                                                   EL687
01743  8500-EXIT.                                                       EL687
01744      EXIT.                                                        EL687
01745                                                                   EL687
01746      EJECT                                                        EL687
01747  8700-LOCATE-BENEFIT SECTION.                                     EL687
01748                                                                   EL687
01749      EXEC CICS HANDLE CONDITION                                   EL687
01750          NOTFND (8700-EXIT) END-EXEC.                             EL687
01751                                                                   EL687
01752      MOVE SPACES                 TO  WS-KIND.                     EL687
01753                                                                   EL687
01754      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**2
01755      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.              CL**2
01756                                                                   EL687
01757      EXEC CICS READ                                               EL687
01758          DATASET (WS-CONTROL-FILE-DSID)                           EL687
01759          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL687
01760          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01761          GTEQ    END-EXEC.                                        EL687
01762                                                                   EL687
01763      IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID                     EL687
01764        OR WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE                 EL687
01765          GO TO 8700-EXIT.                                         EL687
01766                                                                   EL687
01767      MOVE +1                     TO  WS-INDEX.                    EL687
01768                                                                   EL687
01769  8700-LOOKUP-BENEFIT.                                             EL687
01770      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)                   CL**5
01771          MOVE CF-BENEFIT-ALPHA (WS-INDEX)  TO  WS-KIND            EL687
01772          GO TO 8700-EXIT.                                         EL687
01773                                                                   EL687
01774      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC         CL**5
01775          GO TO 8700-EXIT.                                         EL687
01776                                                                   EL687
01777      IF WS-INDEX LESS THAN +8                                     EL687
01778          ADD +1  TO  WS-INDEX                                     EL687
01779          GO TO 8700-LOOKUP-BENEFIT.                               EL687
01780                                                                   EL687
01781  8700-EXIT.                                                       EL687
01782      EXIT.                                                        EL687
01783                                                                   EL687
01784      EJECT                                                        EL687
01785  9000-RETURN-CICS SECTION.                                        EL687
01786                                                                   EL687
01787      MOVE 'EL005'                TO  WS-PROGRAM-ID.                  CL**2
01788      MOVE EIBAID                 TO  PI-ENTRY-CD-1.                  CL**2
01789      PERFORM 9300-XCTL.                                           EL687
01790                                                                   EL687
01791  9000-EXIT.                                                       EL687
01792      EXIT.                                                        EL687
01793                                                                   EL687
01794  9100-RETURN-TRAN SECTION.                                        EL687
01795                                                                   EL687
01796      MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO.                CL**2
01797      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.           CL**2
01798                                                                   EL687
01799      EXEC CICS RETURN                                             EL687
01800          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL687
01801          LENGTH   (PI-COMM-LENGTH)                                EL687
01802          TRANSID  (WS-TRANS-ID) END-EXEC.                         EL687
01803                                                                   EL687
01804  9100-EXIT.                                                       EL687
01805      EXIT.                                                        EL687
01806                                                                   EL687
01807  9300-XCTL SECTION.                                               EL687
01808                                                                   EL687
01809      IF PI-PRINTER-STARTED-SW NOT = ZERO                          EL687
01810          PERFORM 0700-END-PRINT.                                  EL687
01811                                                                   EL687
01812      MOVE DFHENTER               TO  EIBAID.                         CL**2
01813                                                                   EL687
01814      EXEC CICS XCTL                                               EL687
01815          PROGRAM  (WS-PROGRAM-ID)                                 EL687
01816          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL687
01817          LENGTH   (PI-COMM-LENGTH) END-EXEC.                      EL687
01818                                                                   EL687
01819  9300-EXIT.                                                       EL687
01820      EXIT.                                                        EL687
01821                                                                   EL687
01822      EJECT                                                        EL687
01823  9400-CLEAR SECTION.                                              EL687
01824                                                                   EL687
01825      MOVE PI-RETURN-TO-PROGRAM  TO  WS-PROGRAM-ID.                   CL**2
01826      PERFORM 9300-XCTL.                                           EL687
01827                                                                   EL687
01828  9400-EXIT.                                                       EL687
01829      EXIT.                                                        EL687
01830                                                                   EL687
01831  9600-PGMIDERR SECTION.                                           EL687
01832                                                                   EL687
01833      EXEC CICS HANDLE CONDITION                                   EL687
01834          PGMIDERR (8300-SEND-TEXT) END-EXEC.                      EL687
01835                                                                   EL687
01836      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM.             CL**2
01837                                                                   EL687
01838      MOVE 'EL005'                TO  WS-PROGRAM-ID                   CL**2
01839                                      LOGOFF-PGM.                     CL**2
01840      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                    CL**2
01841      MOVE SPACES                 TO  PI-ENTRY-CD-1.                  CL**2
01842      PERFORM 9300-XCTL.                                           EL687
01843                                                                   EL687
01844  9600-EXIT.                                                       EL687
01845      EXIT.                                                        EL687
01846                                                                   EL687
01847  9900-ERROR-FORMAT SECTION.                                       EL687
01848                                                                   EL687
01849      IF EMI-ERRORS-COMPLETE                                       EL687
01850          MOVE ER-3130            TO  EMI-ERROR                       CL**4
01851          GO TO 9900-EXIT.                                         EL687
01852                                                                   EL687
01853      EXEC CICS LINK                                               EL687
01854          PROGRAM  ('EL001')                                          CL**3
01855          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL687
01856          LENGTH   (EMI-COMM-LENGTH) END-EXEC.                     EL687
01857                                                                   EL687
01858      MOVE ER-3130                TO  EMI-ERROR.                      CL**4
01859                                                                   EL687
01860  9900-EXIT.                                                       EL687
01861      EXIT.                                                        EL687
01862                                                                   EL687
01863      EJECT                                                        EL687
01864  9990-ERROR SECTION.                                              EL687
01865                                                                   EL687
01866      MOVE DFHEIBLK TO EMI-LINE1.                                  EL687
01867      EXEC CICS LINK                                               EL687
01868          PROGRAM  ('EL004')                                          CL**3
01869          COMMAREA (EMI-LINE1)                                     EL687
01870          LENGTH   (72) END-EXEC.                                  EL687
01871      PERFORM 8200-SEND-DATAONLY.                                  EL687
01872      GO TO 9100-RETURN-TRAN.                                      EL687
01873                                                                   EL687
01874  9990-EXIT.                                                       EL687
01875      EXIT.                                                        EL687
01876                                                                   EL687
01877  9999-LAST-PARAGRAPH SECTION.                                     EL687
01878                                                                   EL687
01879      GOBACK.                                                      EL687
01880                                                                   EL687
