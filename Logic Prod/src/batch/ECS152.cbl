00001  IDENTIFICATION DIVISION.                                         09/28/98
00002                                                                   ECS152
00003  PROGRAM-ID.                 ECS152.                                 LV008
00004 *              PROGRAM CONVERTED BY                               ECS152
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS152
00006 *              CONVERSION DATE 10/17/97 08:54:30.                 ECS152
00007 *                            VMOD=2.005.                          ECS152
00008 *                                                                 ECS152
00009 *AUTHOR.     LOGIC, INC.                                          ECS152
00010 *            DALLAS, TEXAS                                        ECS152
00011                                                                   ECS152
00012 *DATE-COMPILED.                                                   ECS152
00013 *SECURITY.   *****************************************************ECS152
00014 *            *                                                   *ECS152
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS152
00016 *            *                                                   *ECS152
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS152
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS152
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS152
00020 *            *                                                   *ECS152
00021 *            *****************************************************ECS152
00022                                                                   ECS152
00023 *REMARKS.  PRINT POLICY EXHIBIT FOR FILES B AND C.                ECS152
00024 *          NO PROGRAM OPTION (DEFAULT TO '2') WILL NOT PRINT      ECS152
00025 *          ACCOUNT TOTALS.                                        ECS152
00026 *          OPTION '1' WILL PRINT ACCOUNT TOTALS.                  ECS152
00027  EJECT                                                            ECS152
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
121510* 121510 CR2009120700001   PEMA  FORCE BEGINNING TOTALS TO MATCH ENDING
      ******************************************************************
00028  ENVIRONMENT DIVISION.                                            ECS152
00029  CONFIGURATION SECTION.                                           ECS152
00030  SPECIAL-NAMES.                                                   ECS152
00031      C02 IS LCP-CH2                                               ECS152
00032      C03 IS LCP-CH3                                               ECS152
00033      C04 IS LCP-CH4                                               ECS152
00034      C05 IS LCP-CH5                                               ECS152
00035      C06 IS LCP-CH6                                               ECS152
00036      C07 IS LCP-CH7                                               ECS152
00037      C08 IS LCP-CH8                                               ECS152
00038      C09 IS LCP-CH9                                               ECS152
00039      C10 IS LCP-CH10                                              ECS152
00040      C11 IS LCP-CH11                                              ECS152
00041      C12 IS LCP-CH12                                              ECS152
00042      S01 IS LCP-P01                                               ECS152
00043      S02 IS LCP-P02.                                              ECS152
00044  INPUT-OUTPUT SECTION.                                            ECS152
00045  FILE-CONTROL.                                                    ECS152
00046      SELECT EXTR-IN-B    ASSIGN TO SYS010-UT-2400-S-SYS010.       ECS152
00047      SELECT EXTR-IN-C    ASSIGN TO SYS011-UT-2400-S-SYS011.       ECS152
00048      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       ECS152
00049      SELECT PRT-FILE     ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS152
00050      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS152
00051      SELECT SORT-FILE    ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      ECS152
00052  EJECT                                                            ECS152
00053  DATA DIVISION.                                                   ECS152
00054  FILE SECTION.                                                    ECS152
00055  FD  EXTR-IN-B                                                    ECS152
00056      BLOCK CONTAINS 0 RECORDS
00057      RECORDING MODE F.                                            ECS152
00058                                                                   ECS152
00059  01  EXTR-RECORD-B               PIC X(2701).                     ECS152
00060                                                                   ECS152
00061                                                                   ECS152
00062  FD  EXTR-IN-C                                                    ECS152
00063      BLOCK CONTAINS 0 RECORDS
00064      RECORDING MODE F.                                            ECS152
00065                                                                   ECS152
00066  01  EXTR-RECORD-C               PIC X(2701).                     ECS152
121510 01  EXTR-C-REDEFINE.
121510     05  EC-REINS-CO             PIC X(6).
121510     05  EC-CARRIER              PIC X.
121510     05  EC-GROUP                PIC X(6).
121510     05  F                       PIC X(2688).
121510
00067  EJECT                                                            ECS152
00068  FD  DISK-DATE                                                    ECS152
00069                              COPY ELCDTEFD.                       ECS152
00070  EJECT                                                            ECS152
00071  FD  PRT-FILE                                                     ECS152
00072                              COPY ELCPRTFD.                       ECS152
00073                                                                   ECS152
00074  FD  FICH                                                         ECS152
00075                              COPY ELCFCHFD.                       ECS152
00076  EJECT                                                            ECS152
00077  SD  SORT-FILE.                                                   ECS152
00078                                                                   ECS152
00079  01  SORT-REC.                                                    ECS152
00080      05  SORT-KEY        PIC X(41).                               ECS152
00081      05  FILLER          PIC X(2660).                             ECS152
00082  EJECT                                                            ECS152
00083  WORKING-STORAGE SECTION.                                         ECS152
00084  77  FILLER  PIC X(32) VALUE '********************************'.  ECS152
00085  77  FILLER  PIC X(32) VALUE '     ECS152 WORKING STORAGE     '.  ECS152
00086  77  FILLER  PIC X(32) VALUE '*****VMOD=2.005 ****************'.  ECS152
00087                                                                   ECS152
00088  77  EOJ-SW                      PIC X VALUE 'N'.                 ECS152
00089  77  PAGE-CNT                    PIC 9(5) VALUE ZERO.             ECS152
00090  77  FIRST-READ                  PIC X VALUE 'Y'.                 ECS152
00091  77  STATE-INDEX                 PIC XX VALUE SPACES.             ECS152
00092  77  DO-PRINT                    PIC X VALUE SPACE.               ECS152
00093  77  PRINT-SW                    PIC X VALUE SPACE.               ECS152
00094  77  LCP-ASA                     PIC X.                           ECS152
00095                                                                   ECS152
00096  01  FILE-SW                     PIC X.                           ECS152
00097      88  FILE-1                            VALUE '1'.             ECS152
00098      88  FILE-2                            VALUE '2'.             ECS152
00099      88  FILE-3                            VALUE '3'.             ECS152
00100                                                                   ECS152
CIDMOD 01  WS-REPORT-TITLE             PIC X(10)  VALUE SPACES.
00101  01  WS.                                                          ECS152
00102      03  WS-ZERO                 PIC S9      VALUE +0 COMP-3.     ECS152
00103      03  WS-RETURN-CODE          PIC S9(4)   VALUE +0 COMP-3.     ECS152
00104      03  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.        ECS152
00105      03  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.          ECS152
00106      03  ABEND-CODE              PIC X(4)    VALUE ZERO.          ECS152
00107      03  ABEND-OPTION            PIC X       VALUE 'Y'.           ECS152
00108      03  PGM-SUB                 PIC S9(3)   VALUE +152 COMP.     ECS152
00109                                                                   ECS152
00110  01  WS-WORK-DATE                PIC 9(11).                          CL**6
00111  01  WS-WORK-DATE-R  REDEFINES  WS-WORK-DATE.                        CL**5
00112      03  FILLER                  PIC 999.                            CL**5
00113      03  WS-CCYY                 PIC 9(4).                           CL**5
00114      03  WS-CCYR  REDEFINES  WS-CCYY.                                CL**5
00115          05  WS-CENT             PIC 99.                             CL**5
00116          05  WS-YEAR             PIC 99.                             CL**5
00117      03  WS-MONTH                PIC 99.                             CL**8
00118      03  WS-DAY                  PIC 99.                             CL**5
00119  EJECT                                                            ECS152
00120  01  HD-1.                                                        ECS152
00121      03  FILLER                  PIC X(12) VALUE '1TOTALS FOR '.  ECS152
00122      03  HD-1-MSG                PIC X(21) VALUE SPACES.          ECS152
CIDMOD     03  HD-1-MSGA               PIC X(17) VALUE SPACES.          ECS152
CIDMOD*    03  FILLER                  PIC X(17) VALUE SPACES.          ECS152
00124      03  HD-1-COMP               PIC X(30) VALUE SPACES.          ECS152
00125      03  FILLER                  PIC X(45) VALUE SPACES.          ECS152
00126      03  FILLER                  PIC X(8)  VALUE 'ECS-152'.       ECS152
00127                                                                   ECS152
00128  01  HD-2.                                                        ECS152
00129      03  FILLER                  PIC X(12) VALUE '   CARRIER  '.  ECS152
00130      03  HD-2-CARRIER            PIC X VALUE SPACE.               ECS152
00131      03  FILLER                  PIC X(38) VALUE SPACES.          ECS152
00132      03  HD-2-REIN.                                               ECS152
00133          05  HD-2-REIN-MSG       PIC X(26) VALUE SPACES.          ECS152
00134          05  HD-2-REIN-COMP      PIC X(4) VALUE SPACES.           ECS152
00135      03  FILLER                  PIC X(44) VALUE SPACES.          ECS152
00136      03  HD-2-IPL                PIC X(8) VALUE SPACES.           ECS152
00137                                                                   ECS152
00138  01  HD-3.                                                        ECS152
00139      03  FILLER                  PIC X(12) VALUE '  GROUPING  '.  ECS152
00140      03  HD-3-COMPANY            PIC X(6)  VALUE SPACES.          ECS152
00141      03  FILLER                  PIC X(28) VALUE SPACES.          ECS152
00142      03  HD-3-TITLE              PIC X(26) VALUE SPACES.          ECS152
00143      03  HD-3-MSG                PIC X(20) VALUE SPACES.          ECS152
00144      03  FILLER                  PIC X(41) VALUE SPACES.          ECS152
00145                                                                   ECS152
00146  01  HD-4.                                                        ECS152
00147      03  FILLER                  PIC X(12) VALUE '   STATE    '.  ECS152
00148      03  HD-4-STATE              PIC X(20) VALUE SPACES.          ECS152
00149      03  FILLER                  PIC X(86) VALUE SPACES.          ECS152
00150      03  FILLER                  PIC X(5)  VALUE 'PAGE '.         ECS152
00151      03  HD-4-PAGE               PIC ZZ,ZZZ.                      ECS152
00152      03  FILLER                  PIC X(4) VALUE SPACES.           ECS152
00153                                                                   ECS152
00154  01  HD-5.                                                        ECS152
00155      03  FILLER                  PIC X(12) VALUE '   ACCOUNT  '.  ECS152
00156      03  HD-5-ACCOUNT            PIC X(10) VALUE SPACES.          ECS152
00157      03  FILLER                  PIC X(26) VALUE SPACES.          ECS152
00158      03  FILLER                  PIC X(22)                        ECS152
00159          VALUE 'FOR PERIOD STARTING - '.                          ECS152
00160      03  HD-5-MONTH              PIC X(10).                       ECS152
00161      03  HD-5-DAY                PIC XX.                          ECS152
00162      03  FILLER                  PIC X(2) VALUE ', '.                CL**3
00163      03  HD-5-CENT               PIC XX.                             CL**3
00164      03  HD-5-YEAR               PIC XX.                             CL**3
00165      03  FILLER                  PIC X(45) VALUE SPACES.          ECS152
00166                                                                   ECS152
00167  01  HD-6.                                                        ECS152
00168      03  FILLER                  PIC X(3)  VALUE SPACES.          ECS152
00169      03  HD-6-MSG                PIC X(53) VALUE SPACES.          ECS152
00170      03  FILLER                  PIC X(10)                        ECS152
00171          VALUE 'ENDING  - '.                                      ECS152
00172      03  HD-6-MONTH              PIC X(10).                       ECS152
00173      03  HD-6-DAY                PIC XX.                          ECS152
00174      03  FILLER                  PIC X(2) VALUE ', '.                CL**3
00175      03  HD-6-CENT               PIC XX.                             CL**3
00176      03  HD-6-YEAR               PIC XX.                             CL**3
00177      03  FILLER                  PIC X(49) VALUE SPACES.          ECS152
00178                                                                   ECS152
00179  01  HD-7.                                                        ECS152
00180      03  FILLER                  PIC X(41) VALUE '0'.             ECS152
00181      03  FILLER                  PIC X(43)                        ECS152
00182          VALUE '*****  G R O U P   C E R T I F I C A T E S '.     ECS152
00183      03  FILLER                  PIC X(6)                         ECS152
00184          VALUE ' *****'.                                          ECS152
00185      03  FILLER                  PIC X(43) VALUE SPACES.          ECS152
00186                                                                   ECS152
00187  01  HD-8.                                                        ECS152
00188      03  FILLER                  PIC X(41) VALUE '0'.             ECS152
00189      03  FILLER                  PIC X(44)                        ECS152
00190          VALUE '*****  I N D I V I D U A L   P O L I C I E S'.    ECS152
00191      03  FILLER                  PIC X(7) VALUE '  *****'.        ECS152
00192      03  FILLER                  PIC X(41) VALUE SPACES.          ECS152
00193                                                                   ECS152
00194  01  HD-9.                                                        ECS152
00195      03  FILLER                  PIC X(50) VALUE SPACES.          ECS152
00196      03  HD-9-MSG                PIC X(32) VALUE SPACES.          ECS152
00197      03  FILLER                  PIC X(51) VALUE SPACES.          ECS152
00198                                                                   ECS152
00199  01  HD-MESSAGES.                                                 ECS152
00200      03  HD-MSG1                 PIC X(20)                        ECS152
00201          VALUE 'COMPLETE BUSINESS - '.                            ECS152
00202      03  HD-MSG2                 PIC X(20)                        ECS152
00203          VALUE 'REINSURED BUSINESS -'.                            ECS152
00204      03  HD-MSG3                 PIC X(26)                        ECS152
00205          VALUE 'FOR REINSURANCE COMPANY - '.                      ECS152
00206      03  HD-MSG4                 PIC X(26)                        ECS152
00207          VALUE '- CERTIFICATE ACTIVITY ON '.                      ECS152
00208      03  HD-MSG5                 PIC X(26)                        ECS152
00209          VALUE ' - CERTIFICATE EXHIBIT ON '.                      ECS152
00210      03  HD-MSG6.                                                 ECS152
00211          05  FILLER              PIC X(21)                        ECS152
00212              VALUE 'REINSURANCE COMPANY  '.                       ECS152
00213          05  HD-MSG6-REIN        PIC X(7).                        ECS152
00214      03  HD-MSG7                 PIC X(31)                        ECS152
00215          VALUE '******************************'.                  ECS152
00216                                                                   ECS152
00217  01  HD-10.                                                       ECS152
00218      03  FILLER                  PIC X(26)                        ECS152
00219          VALUE '0 BENEFIT TYPE'.                                  ECS152
00220      03  FILLER                  PIC X(37)                        ECS152
00221          VALUE 'I S S U E D'.                                     ECS152
00222      03  FILLER                  PIC X(36)                        ECS152
00223          VALUE 'C A N C E L L E D'.                               ECS152
00224      03  FILLER                  PIC X(23)                        ECS152
00225          VALUE 'C L A I M S'.                                     ECS152
00226      03  FILLER                  PIC X(11)                        ECS152
00227          VALUE 'E A R N E D'.                                     ECS152
00228                                                                   ECS152
00229  01  HD-11.                                                       ECS152
00230      03  FILLER                  PIC X(17)                        ECS152
00231          VALUE ' **************'.                                 ECS152
00232      03  FILLER                  PIC X(40)                        ECS152
00233          VALUE '************************************'.            ECS152
00234      03  FILLER                  PIC X(39)                        ECS152
00235          VALUE '************************************'.            ECS152
00236      03  FILLER                  PIC X(24)                        ECS152
00237          VALUE '*********************'.                           ECS152
00238      03  FILLER                  PIC X(13)                        ECS152
00239          VALUE '*************'.                                   ECS152
00240                                                                   ECS152
00241  01  HD-12.                                                       ECS152
00242      03  FILLER                  PIC X(17) VALUE SPACES.          ECS152
00243      03  FILLER                  PIC X(8) VALUE '-COUNT-'.        ECS152
00244      03  FILLER                  PIC X(16)                        ECS152
00245          VALUE '----AMOUNT----'.                                  ECS152
00246      03  FILLER                  PIC X(12)                        ECS152
00247          VALUE '--PREMIUM---'.                                    ECS152
00248      03  FILLER                  PIC X(4) VALUE SPACES.           ECS152
00249      03  FILLER                  PIC X(8) VALUE '-COUNT-'.        ECS152
00250      03  FILLER                  PIC X(16)                        ECS152
00251          VALUE '----AMOUNT----'.                                  ECS152
00252      03  FILLER                  PIC X(12)                        ECS152
00253          VALUE '--PREMIUM---'.                                    ECS152
00254      03  FILLER                  PIC X(3) VALUE SPACES.           ECS152
00255      03  FILLER                  PIC X(8) VALUE '-COUNT-'.        ECS152
00256      03  FILLER                  PIC X(16)                        ECS152
00257          VALUE '---AMOUNT----'.                                   ECS152
00258      03  FILLER                  PIC X(13)                        ECS152
00259          VALUE '----AMOUNT---'.                                   ECS152
00260                                                                   ECS152
00261  01  HD-13.                                                       ECS152
00262      03  FILLER                  PIC X(7) VALUE '-'.              ECS152
00263      03  FILLER                  PIC X(33)                        ECS152
00264          VALUE '**** E X H I B I T   I T E M ****'.               ECS152
00265      03  FILLER                  PIC X(10) VALUE SPACES.          ECS152
00266      03  FILLER                  PIC X(30)                        ECS152
00267          VALUE '***** GROUP CERTIFICATES *****'.                  ECS152
00268      03  FILLER                  PIC X(10) VALUE SPACES.          ECS152
00269      03  FILLER                  PIC X(31)                        ECS152
00270          VALUE '***** INDIVIDUAL POLICIES *****'.                 ECS152
00271      03  FILLER                  PIC X(12) VALUE SPACES.          ECS152
00272                                                                   ECS152
00273  01  HD-14.                                                       ECS152
00274      03  FILLER                  PIC X(50) VALUE SPACES.          ECS152
00275      03  FILLER                  PIC X(16) VALUE '-COUNT-'.       ECS152
00276      03  FILLER                  PIC X(24) VALUE '----AMOUNT----'.ECS152
00277      03  FILLER                  PIC X(17) VALUE '-COUNT-'.       ECS152
00278      03  FILLER                  PIC X(26) VALUE '----AMOUNT----'.ECS152
00279                                                                   ECS152
00280  01  HD-15.                                                       ECS152
00281      03  FILLER                  PIC X(41) VALUE '-'.             ECS152
00282      03  FILLER                  PIC X(46)                        ECS152
00283          VALUE '****  A AND H CLAIMS PAID IN THIS PERIOD  ****'.  ECS152
00284      03  FILLER                  PIC X(46) VALUE SPACES.          ECS152
00285  EJECT                                                            ECS152
00286  01  DETAIL-1.                                                    ECS152
00287      03  FILLER                  PIC X VALUE SPACE.               ECS152
00288      03  D1-MSG                  PIC X(15) VALUE SPACES.          ECS152
00289      03  D1-ISS-CNT              PIC ZZZ,ZZZ-.                    ECS152
00290      03  D1-ISS-AMT              PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS152
00291      03  D1-ISS-PRM              PIC ZZ,ZZZ,ZZZ.ZZ-.              ECS152
00292      03  FILLER                  PIC XX VALUE SPACES.             ECS152
00293      03  D1-CAN-CNT              PIC ZZZ,ZZZ-.                    ECS152
00294      03  D1-CAN-AMT              PIC ZZZZ,ZZZ,ZZZ.ZZ-.            ECS152
00295      03  D1-CAN-PRM              PIC ZZ,ZZZ,ZZZ.ZZ-.              ECS152
00296      03  FILLER                  PIC X VALUE SPACES.              ECS152
00297      03  D1-CLM-CNT              PIC ZZZ,ZZZ-.                    ECS152
00298      03  D1-CLM-AMT              PIC ZZ,ZZZ,ZZZ.ZZ-.              ECS152
00299      03  FILLER                  PIC X VALUE SPACE.               ECS152
00300      03  D1-ERN-AMT              PIC ZZZ,ZZZ,ZZZ.ZZ-.             ECS152
00301                                                                   ECS152
00302  01  DETAIL-2.                                                    ECS152
00303      03  FILLER                  PIC X(7) VALUE SPACES.           ECS152
00304      03  D2-MSG                  PIC X(34) VALUE SPACES.          ECS152
00305      03  FILLER                  PIC X(6) VALUE SPACES.           ECS152
00306      03  D2-GRP-CNT              PIC ZZ,ZZZ,ZZZ-.                 ECS152
00307      03  FILLER                  PIC X(6) VALUE SPACES.           ECS152
00308      03  D2-GRP-AMT              PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.          ECS152
00309      03  FILLER                  PIC X(5) VALUE SPACES.           ECS152
00310      03  D2-IND-CNT              PIC ZZ,ZZZ,ZZZ-.                 ECS152
00311      03  FILLER                  PIC X(7) VALUE SPACES.           ECS152
00312      03  D2-IND-AMT              PIC ZZ,ZZZ,ZZZ,ZZZ.ZZ-.          ECS152
00313      03  FILLER                  PIC X(10) VALUE SPACES.          ECS152
00314                                                                   ECS152
00315  01  WORK-INDEX.                                                  ECS152
00316      03 Y                        PIC S9(2) COMP.                  ECS152
00317      03 Z                        PIC S9(2) COMP.                  ECS152
00318                                                                   ECS152
00319  01  DETAIL-3.                                                    ECS152
00320      03  FILLER                  PIC X(41) VALUE SPACES.          ECS152
00321      03  D3-MSG                  PIC X(27) VALUE SPACES.          ECS152
00322      03  D3-AH-AMT               PIC ZZ,ZZZ,ZZZ,ZZZ.99-.          ECS152
00323      03  FILLER                  PIC X(47) VALUE SPACES.          ECS152
00324  EJECT                                                            ECS152
00325  01  MONTH-TABLE.                                                 ECS152
00326      03  FILLER                  PIC X(10) VALUE 'JANUARY   '.    ECS152
00327      03  FILLER                  PIC X(10) VALUE 'FEBRUARY  '.    ECS152
00328      03  FILLER                  PIC X(10) VALUE 'MARCH     '.    ECS152
00329      03  FILLER                  PIC X(10) VALUE 'APRIL     '.    ECS152
00330      03  FILLER                  PIC X(10) VALUE 'MAY       '.    ECS152
00331      03  FILLER                  PIC X(10) VALUE 'JUNE      '.    ECS152
00332      03  FILLER                  PIC X(10) VALUE 'JULY      '.    ECS152
00333      03  FILLER                  PIC X(10) VALUE 'AUGUST    '.    ECS152
00334      03  FILLER                  PIC X(10) VALUE 'SEPTEMBER '.    ECS152
00335      03  FILLER                  PIC X(10) VALUE 'OCTOBER   '.    ECS152
00336      03  FILLER                  PIC X(10) VALUE 'NOVEMBER  '.    ECS152
00337      03  FILLER                  PIC X(10) VALUE 'DECEMBER  '.    ECS152
00338                                                                   ECS152
00339  01  MONTH-TABLE-A     REDEFINES MONTH-TABLE.                     ECS152
00340      03  MONTH-SUB               OCCURS 12 TIMES.                 ECS152
00341          05  MONTH-ALPHA         PIC X(10).                       ECS152
00342  EJECT                                                            ECS152
00343  01  TABLE-1.                                                     ECS152
00344      03  TABLE-1-GRP     OCCURS 3 TIMES.                          ECS152
00345          05  TABLE-1-DTL OCCURS 10 TIMES.                         ECS152
00346              07  TBL-ISSUE-CNT   PIC S9(13)          COMP-3.      ECS152
00347              07  TBL-ISSUE-AMT   PIC S9(11)V99       COMP-3.      ECS152
00348              07  TBL-ISSUE-PREM  PIC S9(11)V99       COMP-3.      ECS152
00349              07  TBL-CANCEL-CNT  PIC S9(13)          COMP-3.      ECS152
00350              07  TBL-CANCEL-AMT  PIC S9(11)V99       COMP-3.      ECS152
00351              07  TBL-CANCEL-PREM PIC S9(11)V99       COMP-3.      ECS152
00352              07  TBL-CLAIM-CNT   PIC S9(13)          COMP-3.      ECS152
00353              07  TBL-CLAIM-AMT   PIC S9(11)V99       COMP-3.      ECS152
00354              07  TBL-EARN-PREM   PIC S9(11)V99       COMP-3.      ECS152
00355                                                                   ECS152
00356  01  TABLE-2.                                                     ECS152
00357      03  TABLE-2-GRP     OCCURS 3 TIMES.                          ECS152
00358          05  TBL-2-DTL   OCCURS 9 TIMES.                          ECS152
00359              07  TBL-GRP-CNT     PIC S9(13)          COMP-3.      ECS152
00360              07  TBL-GRP-AMT     PIC S9(11)V99       COMP-3.      ECS152
00361              07  TBL-IND-CNT     PIC S9(13)          COMP-3.      ECS152
00362              07  TBL-IND-AMT     PIC S9(11)V99       COMP-3.      ECS152
00363                                                                   ECS152
00364  01  TABLE-3.                                                     ECS152
00365      03  TABLE-3-GRP     OCCURS 9 TIMES.                          ECS152
00366          05  T3-GRP-CNT          PIC S9(13)          COMP-3.      ECS152
00367          05  T3-GRP-AMT          PIC S9(11)V99       COMP-3.      ECS152
00368          05  T3-IND-CNT          PIC S9(13)          COMP-3.      ECS152
00369          05  T3-IND-AMT          PIC S9(11)V99       COMP-3.      ECS152
00370                                                                   ECS152
00371  01  TABLE-4.                                                     ECS152
00372      03  TABLE-4-GRP     OCCURS 3 TIMES.                          ECS152
00373          05  TABLE-4-DTL OCCURS 10 TIMES.                         ECS152
00374              07  T4-ISSUE-CNT    PIC S9(13)          COMP-3.      ECS152
00375              07  T4-ISSUE-AMT    PIC S9(11)V99       COMP-3.      ECS152
00376              07  T4-ISSUE-PREM   PIC S9(11)V99       COMP-3.      ECS152
00377              07  T4-CANCEL-CNT   PIC S9(13)          COMP-3.      ECS152
00378              07  T4-CANCEL-AMT   PIC S9(11)V99       COMP-3.      ECS152
00379              07  T4-CANCEL-PREM  PIC S9(11)V99       COMP-3.      ECS152
00380              07  T4-CLAIM-CNT    PIC S9(13)          COMP-3.      ECS152
00381              07  T4-CLAIM-AMT    PIC S9(11)V99       COMP-3.      ECS152
00382              07  T4-EARN-PREM    PIC S9(11)V99       COMP-3.      ECS152
00383                                                                   ECS152
00384  01  TABLE-5.                                                     ECS152
00385      03  TABLE-5-GRP     OCCURS 3 TIMES.                          ECS152
00386          05  TBL-5-DTL   OCCURS 9 TIMES.                          ECS152
00387              07  T5-GRP-CNT      PIC S9(13)          COMP-3.      ECS152
00388              07  T5-GRP-AMT      PIC S9(11)V99       COMP-3.      ECS152
00389              07  T5-IND-CNT      PIC S9(13)          COMP-3.      ECS152
00390              07  T5-IND-AMT      PIC S9(11)V99       COMP-3.      ECS152
00391                                                                   ECS152
00392  01  P1-MESS-R.                                                   ECS152
00393      03  FILLER                  PIC X(32)                        ECS152
00394          VALUE 'TERMS OF 1 MONTH THRU 120 MONTHS'.                ECS152
00395      03  FILLER                  PIC X(32)                        ECS152
00396          VALUE 'TERMS 61 MONTHS THRU 120 MONTHS '.                ECS152
00397      03  FILLER                  PIC X(32)                        ECS152
00398          VALUE 'TERMS GREATER THAN 120 MONTHS   '.                ECS152
00399                                                                   ECS152
00400  01  PAGE-1-MESSAGE REDEFINES P1-MESS-R.                          ECS152
00401      03  P1-MESS         OCCURS 3 TIMES.                          ECS152
00402          05  P1-MSG-1            PIC X(32).                       ECS152
00403                                                                   ECS152
00404  01  TABLE-FOR-PRINT-G.                                           ECS152
00405      03  REPORT-G-SWITCHES  OCCURS 3 TIMES.                       ECS152
00406          05  R-SW-G              PIC X.                           ECS152
00407                                                                   ECS152
00408  01  TABLE-FOR-PRINT-I.                                           ECS152
00409      03  REPORT-I-SWITCHES  OCCURS 3 TIMES.                       ECS152
00410          05  R-SW-I              PIC X.                           ECS152
00411  EJECT                                                            ECS152
00412  01  TOTAL-PRINT-AMOUNTS         COMP-3.                          ECS152
00413      03  G1-ISS-CNT              PIC S9(13).                      ECS152
00414      03  G1-ISS-AMT              PIC S9(11)V99.                   ECS152
00415      03  G1-ISS-PRM              PIC S9(11)V99.                   ECS152
00416      03  G1-CAN-CNT              PIC S9(13).                      ECS152
00417      03  G1-CAN-AMT              PIC S9(11)V99.                   ECS152
00418      03  G1-CAN-PRM              PIC S9(11)V99.                   ECS152
00419      03  G1-CLM-CNT              PIC S9(13).                      ECS152
00420      03  G1-CLM-AMT              PIC S9(11)V99.                   ECS152
00421      03  G1-ERN-PRM              PIC S9(11)V99.                   ECS152
00422                                                                   ECS152
00423  01  TOTAL-AMOUNTS               COMP-3.                          ECS152
00424      03  T1-ISS-CNT              PIC S9(13).                      ECS152
00425      03  T1-ISS-AMT              PIC S9(11)V99.                   ECS152
00426      03  T1-ISS-PRM              PIC S9(11)V99.                   ECS152
00427      03  T1-CAN-CNT              PIC S9(13).                      ECS152
00428      03  T1-CAN-AMT              PIC S9(11)V99.                   ECS152
00429      03  T1-CAN-PRM              PIC S9(11)V99.                   ECS152
00430      03  T1-CLM-CNT              PIC S9(13).                      ECS152
00431      03  T1-CLM-AMT              PIC S9(11)V99.                   ECS152
00432      03  T1-ERN-PRM              PIC S9(11)V99.                   ECS152
00433                                                                   ECS152
00434  01  TOT-AH-ACCUM.                                                ECS152
00435      03  TOT-AH-PD-THIS          PIC S9(11)V99 COMP-3.            ECS152
00436      03  TOT-AH-PD-LAST          PIC S9(11)V99 COMP-3.            ECS152
00437                                                                   ECS152
00438  01  GRAND-TOT-AH-ACCUM.                                          ECS152
00439      03  G-TOT-AH-PD-THIS        PIC S9(11)V99 COMP-3.            ECS152
00440      03  G-TOT-AH-PD-LAST        PIC S9(11)V99 COMP-3.            ECS152
00441                                                                   ECS152
00442  01  ZERO-FIELDS-1               COMP-3.                          ECS152
00443      03  FILLER                  PIC S9(13)      VALUE +0.        ECS152
00444      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00445      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00446      03  FILLER                  PIC S9(13)      VALUE +0.        ECS152
00447      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00448      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00449      03  FILLER                  PIC S9(13)      VALUE +0.        ECS152
00450      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00451      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00452                                                                   ECS152
00453  01  ZERO-FIELDS-2               COMP-3.                          ECS152
00454      03  FILLER                  PIC S9(13)      VALUE +0.        ECS152
00455      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00456      03  FILLER                  PIC S9(13)      VALUE +0.        ECS152
00457      03  FILLER                  PIC S9(11)V99   VALUE +0.        ECS152
00458  EJECT                                                            ECS152
00459  01  CONTROL-AREA.                                                ECS152
00460      03  CONTROL-A.                                               ECS152
00461          05  CNTRL-REIN-CO       PIC X(6)  VALUE LOW-VALUES.      ECS152
00462          05  CNTRL-CARRIER       PIC X     VALUE LOW-VALUES.      ECS152
00463          05  CNTRL-COMPANY       PIC X(6)  VALUE LOW-VALUES.      ECS152
00464          05  CNTRL-STATE         PIC XX    VALUE LOW-VALUES.      ECS152
00465          05  CNTRL-ACCOUNT       PIC X(10) VALUE LOW-VALUES.      ECS152
00466      03  CNTRL-ST-SEQ            PIC XX    VALUE LOW-VALUES.      ECS152
00467      03  CNTRL-REC-TYPE          PIC 99    VALUE ZERO.            ECS152
00468                                                                   ECS152
00469  01  RANGE-DATES.                                                 ECS152
00470      03  CNTRL-START             PIC 9(11).                          CL**7
00471      03  CNTRL-END               PIC 9(11).                          CL**7
00472  EJECT                                                            ECS152
00473  01  THE-RECORD.                                                  ECS152
00474      03  ER-SORT-KEY.                                             ECS152
00475          05  ER-CONTROL.                                          ECS152
00476              07  ER-REINS-CO     PIC X(06).                       ECS152
00477              07  ER-CARRIER      PIC X(01).                       ECS152
00478              07  ER-COMPANY      PIC X(06).                       ECS152
00479              07  ER-STATE        PIC X(02).                       ECS152
00480              07  ER-ACCOUNT      PIC X(10).                       ECS152
00481          05  ER-ST-SEQ           PIC X(02).                       ECS152
00482          05  ER-REC-TYPE         PIC 99.                          ECS152
00483      03  ER-PERIOD-START         PIC 9(11)   COMP-3.                 CL**3
00484      03  ER-PERIOD-END           PIC 9(11)   COMP-3.                 CL**3
00485      03  ER-ACTIVITY-TOTALS.                                      ECS152
00486          05  ER-TERM-GROUP       OCCURS 3 TIMES.                  ECS152
00487 *                1 = 0-60 MONTHS                                  ECS152
00488 *                2 = 61-120 MONTHS                                ECS152
00489 *                3 = 121 AND OVER.                                ECS152
00490              07  ER-TYPE-GROUPS  OCCURS 10 TIMES.                 ECS152
00491 *                    1 = LIFE(R-GP)      6 = LIFE(R-IND)          ECS152
00492 *                    2 = LIFE(L-GP)      7 = LIFE(L-IND)          ECS152
00493 *                    3 = LIFE(OB-GP)     8 = LIFE(OB-IND)         ECS152
00494 *                    4 = AH(GP)          9 = AH(IND)              ECS152
00495 *                    5 = AH(OB-GP)      10 = AH(OB-IND)           ECS152
00496                  09  ER-ISSUE-CNT    PIC S9(13)      COMP-3.      ECS152
00497                  09  ER-ISSUE-AMT    PIC S9(11)V99   COMP-3.      ECS152
00498                  09  ER-ISSUE-PREM   PIC S9(11)V99   COMP-3.      ECS152
00499                  09  ER-CANCEL-CNT   PIC S9(13)      COMP-3.      ECS152
00500                  09  ER-CANCEL-AMT   PIC S9(11)V99   COMP-3.      ECS152
00501                  09  ER-CANCEL-PREM  PIC S9(11)V99   COMP-3.      ECS152
00502                  09  ER-CLAIM-CNT    PIC S9(13)      COMP-3.      ECS152
00503                  09  ER-CLAIM-AMT    PIC S9(11)V99   COMP-3.      ECS152
00504                  09  ER-EARN-PREM    PIC S9(11)V99   COMP-3.      ECS152
00505      03  ER-EXHIBIT-TOTALS.                                       ECS152
00506          05  ER-TERM-GROUP-E     OCCURS 3 TIMES.                  ECS152
00507 *                1 = 0-60 MONTHS                                  ECS152
00508 *                2 = 61-120 MONTHS                                ECS152
00509 *                3 = 121 AND OVER                                 ECS152
00510              07  ER-LINE-DETAIL  OCCURS 9 TIMES.                  ECS152
00511 *                1 = INFORCE PREVIOUS PER  6 = CANCEL IN PERIOD   ECS152
00512 *                2 = ISSUED IN PERIOD      7 = DECREASES IN PERIODECS152
00513 *                3 = TOTAL INFORCE-START   8 = TOTAL DEC IN PERIODECS152
00514 *                4 = DEATHS IN PERIOD      9 = TOTAL INFORCE (END)ECS152
00515 *                5 = EXPIRED IN PERIOD                            ECS152
00516                                                                   ECS152
00517                  09  ER-GROUP-CNT    PIC S9(13)      COMP-3.      ECS152
00518                  09  ER-GROUP-AMT    PIC S9(11)V99   COMP-3.      ECS152
00519                  09  ER-IND-CNT      PIC S9(13)      COMP-3.      ECS152
00520                  09  ER-IND-AMT      PIC S9(11)V99   COMP-3.      ECS152
00521                                                                   ECS152
00522      03  ER-AH-PD-THIS               PIC 9(11)V99    COMP-3.      ECS152
00523      03  ER-AH-PD-LAST               PIC 9(11)V99    COMP-3.      ECS152
00524  EJECT                                                            ECS152
00525  COPY                       ELCDTECX.                                CL**3
00526                                                                   ECS152
00527  EJECT                                                            ECS152
00528  COPY                       ELCDTEVR.                                CL**3
00529                                                                      CL**3
00530  EJECT                                                               CL**3
00531  PROCEDURE DIVISION.                                              ECS152
00532  0100-READ-DATE.                                                  ECS152
CIDMOD
CIDMOD     ACCEPT      WS-REPORT-TITLE.
CIDMOD
CIDMOD     IF WS-REPORT-TITLE = 'NON CREDIT'
CIDMOD        MOVE ' NON CREDIT '      TO HD-1-MSGA
CIDMOD     ELSE
CIDMOD        IF WS-REPORT-TITLE = 'CREDIT'
CIDMOD           MOVE ' ONLY CREDIT '  TO HD-1-MSGA
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO HD-1-MSGA
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00533                              COPY ELCDTERX.                       ECS152
00534      MOVE COMPANY-NAME           TO HD-1-COMP.                    ECS152
00535      MOVE  WS-CURRENT-DATE       TO HD-2-IPL.                        CL**2
00536      MOVE '1'                    TO FILE-SW.                      ECS152
00537      OPEN OUTPUT PRT-FILE.                                        ECS152
00538                                                                   ECS152
00539      PERFORM 0620-ZERO-TABLE-4 THRU 0630-Z-T4-EXIT                ECS152
00540          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER            ECS152
00541              Z FROM 1 BY 1 UNTIL Z GREATER 10.                    ECS152
00542                                                                   ECS152
00543      PERFORM 0640-ZERO-TABLE-5 THRU 0650-Z-T5-EXIT                ECS152
00544          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER            ECS152
00545              Z FROM 1 BY 1 UNTIL Z GREATER 9.                     ECS152
00546                                                                   ECS152
00547      MOVE +0                     TO G-TOT-AH-PD-THIS.             ECS152
00548      MOVE +0                     TO G-TOT-AH-PD-LAST.             ECS152
00549  EJECT                                                            ECS152
00550  0120-OPEN-FILES.                                                 ECS152
00551      PERFORM 0540-CLEAR-WORK-TABLES THRU 0550-C-W-EXIT.           ECS152
00552      IF FILE-1                                                    ECS152
00553          OPEN INPUT EXTR-IN-B                                     ECS152
00554          GO TO 0170-READ-EXTRACT                                  ECS152
00555          ELSE                                                     ECS152
00556          OPEN INPUT EXTR-IN-C.                                    ECS152
00557                                                                   ECS152
00558  0130-SORT-PROCEDURE.                                             ECS152
00559      SORT SORT-FILE ASCENDING SORT-KEY                            ECS152
00560          INPUT PROCEDURE 0140-GET-EXTRACT THRU 0150-EXTRACT-END   ECS152
00561          OUTPUT PROCEDURE 0170-READ-EXTRACT THRU 0980-EOJ-EXIT.   ECS152
00562                                                                   ECS152
00563      IF SORT-RETURN NOT = ZEROES                                  ECS152
00564          MOVE '0101'             TO ABEND-CODE                    ECS152
00565          GO TO ABEND-PGM.                                         ECS152
00566                                                                   ECS152
00567      MOVE ZEROS  TO RETURN-CODE.
00567      GOBACK.                                                      ECS152
00568                                                                   ECS152
00569  0140-GET-EXTRACT  SECTION.                                       ECS152
00570      READ EXTR-IN-C AT END                                        ECS152
00571          CLOSE EXTR-IN-C GO TO 0150-EXTRACT-END.                  ECS152

121510     MOVE LOW-VALUES             TO EC-REINS-CO
121510                                    EC-CARRIER
121510                                    EC-GROUP

00572      RELEASE SORT-REC FROM EXTR-RECORD-C.                         ECS152
00573      GO TO 0140-GET-EXTRACT.                                      ECS152
00574  0150-EXTRACT-END.                                                   CL**3
00575       EXIT.                                                          CL**3
00576                                                                      CL**3
00577  EJECT                                                            ECS152
00578  0170-READ-EXTRACT  SECTION.                                      ECS152
00579      IF FILE-1                                                    ECS152
00580          READ EXTR-IN-B INTO THE-RECORD AT END                    ECS152
00581              MOVE 'Y'            TO EOJ-SW                        ECS152
00582              GO TO 0220-CONTROL-BREAK.                            ECS152
00583                                                                   ECS152
00584      IF FILE-2                                                    ECS152
00585          RETURN SORT-FILE INTO THE-RECORD AT END                  ECS152
00586              MOVE 'Y'            TO EOJ-SW                        ECS152
00587              GO TO 0220-CONTROL-BREAK.                            ECS152
00588                                                                   ECS152
00589      IF DTE-PGM-OPT NOT = 1                                       ECS152
00590          IF ER-REC-TYPE = 15                                      ECS152
00591              GO TO 0170-READ-EXTRACT.                             ECS152
00592                                                                   ECS152
00593  0180-SET-CONTROL.                                                ECS152
00594      IF FIRST-READ = 'Y'                                          ECS152
00595          MOVE ER-SORT-KEY        TO CONTROL-AREA                  ECS152
00596          MOVE ER-PERIOD-START    TO CNTRL-START                   ECS152
00597          MOVE ER-PERIOD-END      TO CNTRL-END                     ECS152
00598          MOVE 'N'                TO FIRST-READ.                   ECS152
00599                                                                   ECS152
00600  0190-CHECK-SEQUENCE.                                             ECS152
00601      IF ER-SORT-KEY LESS CONTROL-AREA                             ECS152
00602          DISPLAY 'ECS152 - SEQUENCE ERROR PROGRAM TERMINATED'     ECS152
00603          MOVE '0610'             TO ABEND-CODE                    ECS152
00604          GO TO ABEND-PGM.                                         ECS152
00605                                                                   ECS152
00606      IF ER-SORT-KEY NOT EQUAL CONTROL-AREA                        ECS152
00607          GO TO 0220-CONTROL-BREAK.                                ECS152
00608                                                                   ECS152
00609  0200-ACCUM-AMOUNTS.                                              ECS152
00610      PERFORM 0310-ADD-UP-DETAIL THRU 0320-ADD-DETAIL-EXIT         ECS152
00611          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER            ECS152
00612              Z FROM 1 BY 1 UNTIL Z GREATER 10.                    ECS152
00613                                                                   ECS152
00614      PERFORM 0350-ADD-UP-A-H THRU 0360-ADD-AH-EXIT                ECS152
00615          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER            ECS152
00616              Z FROM 1 BY 1 UNTIL Z GREATER 9.                     ECS152
00617                                                                   ECS152
00618      ADD ER-AH-PD-THIS TO TOT-AH-PD-THIS.                         ECS152
00619      ADD ER-AH-PD-LAST TO TOT-AH-PD-LAST.                         ECS152
00620                                                                   ECS152
00621      IF CNTRL-REC-TYPE NOT = 20                                   ECS152
00622          NEXT SENTENCE                                            ECS152
00623      ELSE                                                         ECS152
00624          PERFORM 0330-ADD-UP-GRAND-DETAIL THRU 0340-ADD-GRAND-EXITECS152
00625              VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER        ECS152
00626                  Z FROM 1 BY 1 UNTIL Z GREATER 10                 ECS152
00627          PERFORM 0370-ADD-UP-GRAND-AH THRU 0380-ADD-GRAND-AH-EXIT ECS152
00628              VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER        ECS152
00629                  Z FROM 1 BY 1 UNTIL Z GREATER 9                  ECS152
00630          ADD ER-AH-PD-THIS TO G-TOT-AH-PD-THIS                    ECS152
00631          ADD ER-AH-PD-LAST TO G-TOT-AH-PD-LAST.                   ECS152
00632                                                                   ECS152
00633      GO TO 0170-READ-EXTRACT.                                     ECS152
00634  EJECT                                                            ECS152
00635  0220-CONTROL-BREAK.                                              ECS152
00636      MOVE '*                   ' TO                               ECS152
00637                                    HD-2-CARRIER                   ECS152
00638                                    HD-3-COMPANY                   ECS152
00639                                    HD-4-STATE                     ECS152
00640                                    HD-5-ACCOUNT.                  ECS152
00641                                                                   ECS152
00642      IF EOJ-SW = 'Y' AND PRINT-SW = SPACE                            CL**3
00643          GO TO 0960-NO-PRINT.                                        CL**3
00644                                                                   ECS152
00645      MOVE SPACES                 TO HD-6-MSG  HD-2-REIN.          ECS152
00646      MOVE CNTRL-STATE            TO STATE-INDEX.                  ECS152
00647                                                                   ECS152
00648      IF CNTRL-REC-TYPE = 15                                       ECS152
00649          MOVE 'ACCOUNT              ' TO HD-1-MSG                 ECS152
00650          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                  ECS152
00651          MOVE CNTRL-COMPANY      TO HD-3-COMPANY                  ECS152
00652          PERFORM 0670-FIND-STATE THRU 0690-FIND-S-EXIT            ECS152
00653          MOVE CNTRL-ACCOUNT      TO HD-5-ACCOUNT                  ECS152
00654          GO TO 0240-END-BREAK.                                    ECS152
00655                                                                   ECS152
00656      IF CNTRL-REC-TYPE = 16                                       ECS152
00657          MOVE 'STATE                ' TO HD-1-MSG                 ECS152
00658          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                  ECS152
00659          MOVE CNTRL-COMPANY      TO HD-3-COMPANY                  ECS152
00660          PERFORM 0670-FIND-STATE THRU 0690-FIND-S-EXIT            ECS152
00661          GO TO 0240-END-BREAK.                                    ECS152
00662                                                                   ECS152
00663      IF CNTRL-REC-TYPE = 17                                       ECS152
00664          MOVE 'COMPANY              ' TO HD-1-MSG                 ECS152
00665          MOVE CNTRL-CARRIER      TO HD-2-CARRIER                  ECS152
00666          MOVE CNTRL-COMPANY      TO HD-3-COMPANY                  ECS152
00667          GO TO 0240-END-BREAK.                                    ECS152
00668                                                                   ECS152
00669      IF CNTRL-REC-TYPE = 18                                       ECS152
00670          MOVE 'CARRIER              ' TO HD-1-MSG                 ECS152
00671          MOVE CNTRL-CARRIER TO HD-2-CARRIER                       ECS152
00672          GO TO 0240-END-BREAK.                                    ECS152
00673                                                                   ECS152
00674      IF CNTRL-REC-TYPE = 19                                       ECS152
00675          MOVE 'REINSURANCE COMPANY  ' TO HD-1-MSG                 ECS152
00676          GO TO 0240-END-BREAK.                                    ECS152
00677                                                                   ECS152
00678      IF CNTRL-REC-TYPE = 20                                       ECS152
00679          MOVE 'STATE OVERALL        ' TO HD-1-MSG                 ECS152
00680          PERFORM 0670-FIND-STATE THRU 0690-FIND-S-EXIT            ECS152
00681          GO TO 0240-END-BREAK.                                    ECS152
00682                                                                   ECS152
00683      IF CNTRL-REC-TYPE = 21                                       ECS152
00684          MOVE 'GRAND TOTAL          ' TO HD-1-MSG                 ECS152
00685          GO TO 0240-END-BREAK.                                    ECS152
00686  EJECT                                                            ECS152
00687  0240-END-BREAK.                                                  ECS152
00688      IF CNTRL-REC-TYPE LESS 20                                    ECS152
00689          MOVE CNTRL-REIN-CO TO HD-MSG6-REIN  HD-2-REIN-COMP       ECS152
00690          MOVE HD-MSG3            TO HD-2-REIN-MSG                 ECS152
00691          MOVE HD-MSG6            TO HD-6-MSG.                     ECS152
00692                                                                   ECS152
00693      MOVE CNTRL-START            TO WS-WORK-DATE.                 ECS152
00694      MOVE WS-DAY                 TO HD-5-DAY.                     ECS152
00695      MOVE WS-CENT                TO HD-5-CENT.                       CL**8
00696      MOVE WS-YEAR                TO HD-5-YEAR.                       CL**3
00697      MOVE MONTH-ALPHA (WS-MONTH) TO HD-5-MONTH.                   ECS152
00698                                                                   ECS152
00699      MOVE CNTRL-END              TO WS-WORK-DATE.                 ECS152
00700      MOVE WS-DAY                 TO HD-6-DAY.                     ECS152
00701      MOVE WS-CENT                TO HD-6-CENT.                       CL**8
00702      MOVE WS-YEAR                TO HD-6-YEAR.                       CL**3
00703      MOVE MONTH-ALPHA (WS-MONTH) TO HD-6-MONTH.                   ECS152
00704                                                                   ECS152
00705      MOVE HD-MSG2                TO HD-3-MSG.                     ECS152
00706                                                                   ECS152
00707      PERFORM 0400-ACCUMULATE-1-120 THRU 0410-A-1-120-EXIT         ECS152
00708          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 10.                ECS152
00709                                                                   ECS152
00710      PERFORM 0420-ADD-3-AH-LEVELS THRU 0430-ADD-3-EXIT            ECS152
00711          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 9 AFTER            ECS152
00712              Y FROM 1 BY 1 UNTIL Y GREATER 3.                     ECS152
00713                                                                   ECS152
00714      MOVE +1                     TO Y.                            ECS152
00715      PERFORM 0810-CHECK-FOR-PRINT THRU 0820-CHECK-PRINT-EXIT      ECS152
00716          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 5.                 ECS152
00717      MOVE DO-PRINT               TO R-SW-G (Y).                   ECS152
00718                                                                   ECS152
00719      MOVE +2                     TO Y.                            ECS152
00720      PERFORM 0810-CHECK-FOR-PRINT THRU 0820-CHECK-PRINT-EXIT      ECS152
00721          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 5.                 ECS152
00722      MOVE DO-PRINT               TO R-SW-G (Y).                   ECS152
00723                                                                   ECS152
00724      MOVE +3                     TO Y.                            ECS152
00725      PERFORM 0810-CHECK-FOR-PRINT THRU 0820-CHECK-PRINT-EXIT      ECS152
00726          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 5.                 ECS152
00727      MOVE DO-PRINT               TO R-SW-G (Y).                   ECS152
00728                                                                   ECS152
00729      MOVE +1                     TO Y.                            ECS152
00730      PERFORM 0810-CHECK-FOR-PRINT THRU 0820-CHECK-PRINT-EXIT      ECS152
00731          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 10.                ECS152
00732      MOVE DO-PRINT               TO R-SW-I (Y).                   ECS152
00733                                                                   ECS152
00734      MOVE +2                     TO Y.                            ECS152
00735      PERFORM 0810-CHECK-FOR-PRINT THRU 0820-CHECK-PRINT-EXIT      ECS152
00736          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 10.                ECS152
00737      MOVE DO-PRINT               TO R-SW-I (Y).                   ECS152
00738                                                                   ECS152
00739      MOVE +3                     TO Y.                            ECS152
00740      PERFORM 0810-CHECK-FOR-PRINT THRU 0820-CHECK-PRINT-EXIT      ECS152
00741          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 10.                ECS152
00742      MOVE DO-PRINT               TO R-SW-I (Y).                   ECS152
00743  EJECT                                                            ECS152
00744                                                                   ECS152
00745  0260-CONTROL-BREAK-PRINT.                                        ECS152
00746      MOVE HD-MSG4                TO HD-3-TITLE.                   ECS152
00747      IF R-SW-G (1) = SPACE AND R-SW-G (2) = SPACE AND             ECS152
00748         R-SW-G (3) = SPACE AND R-SW-I (1) = SPACE AND                CL**3
00749         R-SW-I (2) = SPACE AND R-SW-I (3) = SPACE                    CL**3
00750              GO TO 0290-CONTROL-4.                                ECS152
00751                                                                   ECS152
00752      IF PRINT-SW = SPACE                                             CL**3
00753          MOVE 'Y'                TO PRINT-SW.                        CL**3
00754                                                                   ECS152
00755      PERFORM 0710-PRINT-HEADINGS THRU 0720-P-H-X.                 ECS152
00756                                                                   ECS152
00757      IF R-SW-G (1) = SPACE AND R-SW-G (2) = SPACE AND             ECS152
00758         R-SW-G (3) = SPACE                                           CL**3
00759          GO TO 0270-CONTROL-2.                                       CL**3
00760                                                                   ECS152
00761      PERFORM 0840-DUMP-PAGE-1 THRU 0860-DUMP-1-EXIT               ECS152
00762          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3.                 ECS152
00763                                                                   ECS152
00764      IF R-SW-I (1) = SPACE AND R-SW-I (2) = SPACE AND             ECS152
00765         R-SW-I (3) = SPACE                                           CL**3
00766          MOVE +3                 TO Y                                CL**3
00767          PERFORM 0900-DUMP-2-FINAL THRU 0910-DUMP-2-EXIT             CL**3
00768          GO TO 0280-CONTROL-3.                                       CL**3
00769                                                                   ECS152
00770      PERFORM 0710-PRINT-HEADINGS THRU 0720-P-H-X.                 ECS152
00771                                                                   ECS152
00772  0270-CONTROL-2.                                                  ECS152
00773      PERFORM 0880-DUMP-PAGE-2 THRU 0910-DUMP-2-EXIT               ECS152
00774          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3.                 ECS152
00775                                                                   ECS152
00776  0280-CONTROL-3.                                                  ECS152
00777      MOVE HD-MSG5                TO HD-3-TITLE.                   ECS152
00778      PERFORM 0710-PRINT-HEADINGS THRU 0720-P-H-X.                 ECS152
00779      PERFORM 0930-DUMP-PAGE-3 THRU 0940-DUMP-3-EXIT.              ECS152
00780                                                                   ECS152
00781  0290-CONTROL-4.                                                  ECS152
00782      MOVE 'Y'                    TO FIRST-READ.                   ECS152
00783                                                                   ECS152
00784      IF EOJ-SW = 'Y'                                              ECS152
00785          IF FILE-1                                                ECS152
00786              MOVE 'N'            TO EOJ-SW                        ECS152
00787              MOVE ZERO           TO PAGE-CNT                      ECS152
00788              CLOSE EXTR-IN-B                                      ECS152
00789              MOVE '2'            TO FILE-SW                       ECS152
00790              GO TO 0120-OPEN-FILES.                               ECS152
00791                                                                   ECS152
00792      IF EOJ-SW = 'Y'                                              ECS152
00793          IF FILE-2                                                ECS152
00794              PERFORM 0600-ZERO-TABLE-3 THRU 0610-Z-T3-EXIT        ECS152
00795                  VARYING Z FROM 1 BY 1 UNTIL Z GREATER 9          ECS152
00796              MOVE '3'            TO FILE-SW                       ECS152
00797              MOVE '21'           TO CNTRL-REC-TYPE                ECS152
00798              MOVE TABLE-4        TO TABLE-1                       ECS152
00799              MOVE TABLE-5        TO TABLE-2                       ECS152
00800              MOVE GRAND-TOT-AH-ACCUM                              ECS152
00801                                  TO TOT-AH-ACCUM                  ECS152
00802              GO TO 0220-CONTROL-BREAK.                            ECS152
00803                                                                   ECS152
00804      IF EOJ-SW = 'Y'                                              ECS152
00805          IF FILE-3                                                ECS152
00806              GO TO 0970-END-JOB.                                  ECS152
00807                                                                   ECS152
00808      PERFORM 0540-CLEAR-WORK-TABLES THRU 0550-C-W-EXIT.           ECS152
00809      GO TO 0180-SET-CONTROL.                                      ECS152
00810                                                                   ECS152
00811  EJECT                                                            ECS152
00812 *                                                                 ECS152
00813 *        PERFORMED PROCEDURES NEXT                                ECS152
00814 *                                                                 ECS152
00815  0310-ADD-UP-DETAIL.                                              ECS152
00816      ADD ER-ISSUE-CNT   (Y, Z) TO TBL-ISSUE-CNT (Y, Z).           ECS152
00817      ADD ER-ISSUE-AMT   (Y, Z) TO TBL-ISSUE-AMT (Y, Z).           ECS152
00818      ADD ER-ISSUE-PREM  (Y, Z) TO TBL-ISSUE-PREM (Y, Z).          ECS152
00819      ADD ER-CANCEL-CNT  (Y, Z) TO TBL-CANCEL-CNT (Y, Z).          ECS152
00820      ADD ER-CANCEL-AMT  (Y, Z) TO TBL-CANCEL-AMT (Y, Z).          ECS152
00821      ADD ER-CANCEL-PREM (Y, Z) TO TBL-CANCEL-PREM (Y, Z).         ECS152
00822      ADD ER-CLAIM-CNT   (Y, Z) TO TBL-CLAIM-CNT (Y, Z).           ECS152
00823      ADD ER-CLAIM-AMT   (Y, Z) TO TBL-CLAIM-AMT (Y, Z).           ECS152
00824      ADD ER-EARN-PREM   (Y, Z) TO TBL-EARN-PREM (Y, Z).           ECS152
00825  0320-ADD-DETAIL-EXIT.                                            ECS152
00826      EXIT.                                                        ECS152
00827                                                                   ECS152
00828  0330-ADD-UP-GRAND-DETAIL.                                        ECS152
00829      ADD ER-ISSUE-CNT   (Y, Z) TO T4-ISSUE-CNT (Y, Z).            ECS152
00830      ADD ER-ISSUE-AMT   (Y, Z) TO T4-ISSUE-AMT (Y, Z).            ECS152
00831      ADD ER-ISSUE-PREM  (Y, Z) TO T4-ISSUE-PREM (Y, Z).           ECS152
00832      ADD ER-CANCEL-CNT  (Y, Z) TO T4-CANCEL-CNT (Y, Z).           ECS152
00833      ADD ER-CANCEL-AMT  (Y, Z) TO T4-CANCEL-AMT (Y, Z).           ECS152
00834      ADD ER-CANCEL-PREM (Y, Z) TO T4-CANCEL-PREM (Y, Z).          ECS152
00835      ADD ER-CLAIM-CNT   (Y, Z) TO T4-CLAIM-CNT (Y, Z).            ECS152
00836      ADD ER-CLAIM-AMT   (Y, Z) TO T4-CLAIM-AMT (Y, Z).            ECS152
00837      ADD ER-EARN-PREM   (Y, Z) TO T4-EARN-PREM (Y, Z).            ECS152
00838  0340-ADD-GRAND-EXIT.                                             ECS152
00839      EXIT.                                                        ECS152
00840                                                                   ECS152
00841  0350-ADD-UP-A-H.                                                 ECS152
00842      ADD ER-GROUP-CNT (Y, Z) TO TBL-GRP-CNT (Y, Z).               ECS152
00843      ADD ER-GROUP-AMT (Y, Z) TO TBL-GRP-AMT (Y, Z).               ECS152
00844      ADD ER-IND-CNT   (Y, Z) TO TBL-IND-CNT (Y, Z).               ECS152
00845      ADD ER-IND-AMT   (Y, Z) TO TBL-IND-AMT (Y, Z).               ECS152
00846  0360-ADD-AH-EXIT.                                                ECS152
00847      EXIT.                                                        ECS152
00848                                                                   ECS152
00849  0370-ADD-UP-GRAND-AH.                                            ECS152
00850      ADD ER-GROUP-CNT (Y, Z) TO T5-GRP-CNT (Y, Z).                ECS152
00851      ADD ER-GROUP-AMT (Y, Z) TO T5-GRP-AMT (Y, Z).                ECS152
00852      ADD ER-IND-CNT   (Y, Z) TO T5-IND-CNT (Y, Z).                ECS152
00853      ADD ER-IND-AMT   (Y, Z) TO T5-IND-AMT (Y, Z).                ECS152
00854  0380-ADD-GRAND-AH-EXIT.                                          ECS152
00855      EXIT.                                                        ECS152
00856  EJECT                                                            ECS152
00857                                                                      CL**4
00858  0400-ACCUMULATE-1-120.                                           ECS152
00859      ADD TBL-ISSUE-CNT   (2, Z)   TO TBL-ISSUE-CNT   (1, Z).      ECS152
00860      ADD TBL-ISSUE-AMT   (2, Z)   TO TBL-ISSUE-AMT   (1, Z).      ECS152
00861      ADD TBL-ISSUE-PREM  (2, Z)   TO TBL-ISSUE-PREM  (1, Z).      ECS152
00862      ADD TBL-CANCEL-CNT  (2, Z)   TO TBL-CANCEL-CNT  (1, Z).      ECS152
00863      ADD TBL-CANCEL-AMT  (2, Z)   TO TBL-CANCEL-AMT  (1, Z).      ECS152
00864      ADD TBL-CANCEL-PREM (2, Z)   TO TBL-CANCEL-PREM (1, Z).      ECS152
00865      ADD TBL-CLAIM-CNT   (2, Z)   TO TBL-CLAIM-CNT   (1, Z).      ECS152
00866      ADD TBL-CLAIM-AMT   (2, Z)   TO TBL-CLAIM-AMT   (1, Z).      ECS152
00867      ADD TBL-EARN-PREM   (2, Z)   TO TBL-EARN-PREM   (1, Z).      ECS152
00868  0410-A-1-120-EXIT.                                               ECS152
00869      EXIT.                                                        ECS152
00870                                                                   ECS152
00871  0420-ADD-3-AH-LEVELS.                                            ECS152
00872      ADD TBL-GRP-CNT (Y, Z) TO T3-GRP-CNT (Z).                    ECS152
00873      ADD TBL-GRP-AMT (Y, Z) TO T3-GRP-AMT (Z).                    ECS152
00874      ADD TBL-IND-CNT (Y, Z) TO T3-IND-CNT (Z).                    ECS152
00875      ADD TBL-IND-AMT (Y, Z) TO T3-IND-AMT (Z).                    ECS152
00876  0430-ADD-3-EXIT.                                                 ECS152
00877      EXIT.                                                        ECS152
00878                                                                   ECS152
00879  0440-ADD-SUB-TOTALS.                                             ECS152
00880      ADD TBL-ISSUE-CNT   (Y, Z) TO G1-ISS-CNT   T1-ISS-CNT.       ECS152
00881      ADD TBL-ISSUE-AMT   (Y, Z) TO G1-ISS-AMT   T1-ISS-AMT.       ECS152
00882      ADD TBL-ISSUE-PREM  (Y, Z) TO G1-ISS-PRM   T1-ISS-PRM.       ECS152
00883      ADD TBL-CANCEL-CNT  (Y, Z) TO G1-CAN-CNT   T1-CAN-CNT.       ECS152
00884      ADD TBL-CANCEL-AMT  (Y, Z) TO G1-CAN-AMT   T1-CAN-AMT.       ECS152
00885      ADD TBL-CANCEL-PREM (Y, Z) TO G1-CAN-PRM   T1-CAN-PRM.       ECS152
00886      ADD TBL-CLAIM-CNT   (Y, Z) TO G1-CLM-CNT   T1-CLM-CNT.       ECS152
00887      ADD TBL-CLAIM-AMT   (Y, Z) TO G1-CLM-AMT   T1-CLM-AMT.       ECS152
00888      ADD TBL-EARN-PREM   (Y, Z) TO G1-ERN-PRM   T1-ERN-PRM.       ECS152
00889  0450-ADD-SUB-EXIT.                                               ECS152
00890      EXIT.                                                        ECS152
00891  EJECT                                                            ECS152
00892                                                                      CL**4
00893  0470-BUILD-DETAIL-1.                                             ECS152
00894      MOVE TBL-ISSUE-CNT   (Y, Z) TO D1-ISS-CNT.                   ECS152
00895      MOVE TBL-ISSUE-AMT   (Y, Z) TO D1-ISS-AMT.                   ECS152
00896      MOVE TBL-ISSUE-PREM  (Y, Z) TO D1-ISS-PRM.                   ECS152
00897      MOVE TBL-CANCEL-CNT  (Y, Z) TO D1-CAN-CNT.                   ECS152
00898      MOVE TBL-CANCEL-AMT  (Y, Z) TO D1-CAN-AMT.                   ECS152
00899      MOVE TBL-CANCEL-PREM (Y, Z) TO D1-CAN-PRM.                   ECS152
00900      MOVE TBL-CLAIM-CNT   (Y, Z) TO D1-CLM-CNT.                   ECS152
00901      MOVE TBL-CLAIM-AMT   (Y, Z) TO D1-CLM-AMT.                   ECS152
00902      MOVE TBL-EARN-PREM   (Y, Z) TO D1-ERN-AMT.                   ECS152
00903  0480-BUILD-D-EXIT.                                               ECS152
00904      EXIT.                                                        ECS152
00905                                                                   ECS152
00906  0490-BUILD-DETAIL-2.                                             ECS152
00907      MOVE T3-GRP-CNT (Z)         TO D2-GRP-CNT.                   ECS152
00908      MOVE T3-GRP-AMT (Z)         TO D2-GRP-AMT.                   ECS152
00909      MOVE T3-IND-CNT (Z)         TO D2-IND-CNT.                   ECS152
00910      MOVE T3-IND-AMT (Z)         TO D2-IND-AMT.                   ECS152
00911  0500-BUILD-2-EXIT.                                               ECS152
00912      EXIT.                                                        ECS152
00913                                                                   ECS152
00914  0510-BUILD-DETAIL-2A.                                            ECS152
00915      MOVE TBL-GRP-CNT (Y, Z)     TO D2-GRP-CNT.                   ECS152
00916      MOVE TBL-GRP-AMT (Y, Z)     TO D2-GRP-AMT.                   ECS152
00917      MOVE TBL-IND-CNT (Y, Z)     TO D2-IND-CNT.                   ECS152
00918      MOVE TBL-IND-AMT (Y, Z)     TO D2-IND-AMT.                   ECS152
00919  0520-BUILD-2A-EXIT.                                              ECS152
00920      EXIT.                                                        ECS152
00921  EJECT                                                            ECS152
00922                                                                      CL**4
00923  0540-CLEAR-WORK-TABLES.                                          ECS152
00924      PERFORM 0560-ZERO-TABLE-1 THRU 0570-Z-T1-EXIT                ECS152
00925          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER            ECS152
00926              Z FROM 1 BY 1 UNTIL Z GREATER 10.                    ECS152
00927                                                                   ECS152
00928      PERFORM 0580-ZERO-TABLE-2 THRU 0590-Z-T2-EXIT                ECS152
00929          VARYING Y FROM 1 BY 1 UNTIL Y GREATER 3 AFTER            ECS152
00930              Z FROM 1 BY 1 UNTIL Z GREATER 9.                     ECS152
00931                                                                   ECS152
00932      PERFORM 0600-ZERO-TABLE-3 THRU 0610-Z-T3-EXIT                ECS152
00933          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 9.                 ECS152
00934                                                                   ECS152
00935      MOVE +0                     TO TOT-AH-PD-THIS.               ECS152
00936      MOVE +0                     TO TOT-AH-PD-LAST.               ECS152
00937  0550-C-W-EXIT.                                                   ECS152
00938      EXIT.                                                        ECS152
00939                                                                   ECS152
00940  0560-ZERO-TABLE-1.                                               ECS152
00941      MOVE ZERO-FIELDS-1          TO TABLE-1-DTL (Y, Z).           ECS152
00942  0570-Z-T1-EXIT.                                                  ECS152
00943      EXIT.                                                        ECS152
00944                                                                   ECS152
00945  0580-ZERO-TABLE-2.                                               ECS152
00946      MOVE ZERO-FIELDS-2          TO TBL-2-DTL (Y, Z).             ECS152
00947  0590-Z-T2-EXIT.                                                  ECS152
00948      EXIT.                                                        ECS152
00949                                                                   ECS152
00950  0600-ZERO-TABLE-3.                                               ECS152
00951      MOVE ZERO-FIELDS-2          TO TABLE-3-GRP (Z).              ECS152
00952  0610-Z-T3-EXIT.                                                  ECS152
00953      EXIT.                                                        ECS152
00954                                                                   ECS152
00955  0620-ZERO-TABLE-4.                                               ECS152
00956      MOVE ZERO-FIELDS-1          TO TABLE-4-DTL (Y, Z).           ECS152
00957  0630-Z-T4-EXIT.                                                  ECS152
00958      EXIT.                                                        ECS152
00959                                                                   ECS152
00960  0640-ZERO-TABLE-5.                                               ECS152
00961      MOVE ZERO-FIELDS-2          TO TBL-5-DTL (Y, Z).             ECS152
00962  0650-Z-T5-EXIT.                                                  ECS152
00963      EXIT.                                                        ECS152
00964  EJECT                                                            ECS152
00965                                                                      CL**4
00966  0670-FIND-STATE.                                                 ECS152
00967      MOVE +1                     TO Y.                            ECS152
00968  0680-FIND-STATE-NOW.                                             ECS152
00969      IF Y GREATER +75                                             ECS152
00970          MOVE 'UNKNOWN             ' TO HD-4-STATE                ECS152
00971          GO TO 0690-FIND-S-EXIT.                                  ECS152
00972                                                                   ECS152
00973      IF STATE-INDEX = STATE-SUB (Y) OR STATE-ABBR (Y)             ECS152
00974          MOVE STATE-PIC (Y)      TO HD-4-STATE                    ECS152
00975          GO TO 0690-FIND-S-EXIT.                                  ECS152
00976                                                                   ECS152
00977      ADD +1 TO Y.                                                 ECS152
00978      GO TO 0680-FIND-STATE-NOW.                                   ECS152
00979  0690-FIND-S-EXIT.                                                ECS152
00980      EXIT.                                                        ECS152
00981  EJECT                                                            ECS152
00982                                                                   ECS152
00983  0710-PRINT-HEADINGS.                                             ECS152
00984      ADD 1 TO PAGE-CNT.                                           ECS152
00985      MOVE PAGE-CNT               TO HD-4-PAGE.                    ECS152
00986      MOVE HD-1                   TO PRT.                          ECS152
00987      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
00988      MOVE HD-2                   TO PRT.                          ECS152
00989      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
00990      MOVE HD-3                   TO PRT.                          ECS152
00991      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
00992      MOVE HD-4                   TO PRT.                          ECS152
00993      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
00994      MOVE HD-5                   TO PRT.                          ECS152
00995      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
00996      MOVE HD-6                   TO PRT.                          ECS152
00997      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
00998  0720-P-H-X.                                                      ECS152
00999      EXIT.                                                        ECS152
01000                                                                   ECS152
01001  0730-PRT-RTN.                                                    ECS152
01002      IF DTE-FICH IS NOT EQUAL TO SPACE AND                        ECS152
01003          FICH-OPEN IS EQUAL TO SPACE                              ECS152
01004          OPEN OUTPUT FICH                                         ECS152
01005          MOVE 'X'                TO FICH-OPEN.                    ECS152
01006                                                                      CL**4
01007      IF DTE-FICH IS NOT EQUAL TO SPACE                            ECS152
01008          WRITE FICH-REC FROM PRT.                                 ECS152
01009                                                                      CL**4
01010      IF DTE-FICH IS EQUAL TO SPACE OR '2'                         ECS152
01011          MOVE P-CTL TO LCP-ASA                                    ECS152
01012          PERFORM LCP-WRITE-POS-PRT                                ECS152
01013              THRU LCP-WRITE-END-PRT.                              ECS152
01014  0740-PRT-RTN-X.                                                  ECS152
01015      EXIT.                                                        ECS152
01016  EJECT                                                            ECS152
01017                                                                      CL**4
01018  0760-MOVE-G1-TO-PRINT.                                           ECS152
01019      MOVE G1-ISS-CNT             TO D1-ISS-CNT.                   ECS152
01020      MOVE G1-ISS-AMT             TO D1-ISS-AMT.                   ECS152
01021      MOVE G1-ISS-PRM             TO D1-ISS-PRM.                   ECS152
01022      MOVE G1-CAN-CNT             TO D1-CAN-CNT.                   ECS152
01023      MOVE G1-CAN-AMT             TO D1-CAN-AMT.                   ECS152
01024      MOVE G1-CAN-PRM             TO D1-CAN-PRM.                   ECS152
01025      MOVE G1-CLM-CNT             TO D1-CLM-CNT.                   ECS152
01026      MOVE G1-CLM-AMT             TO D1-CLM-AMT.                   ECS152
01027      MOVE G1-ERN-PRM             TO D1-ERN-AMT.                   ECS152
01028  0770-MOVE-G1-EXIT.                                               ECS152
01029      EXIT.                                                        ECS152
01030                                                                   ECS152
01031  0780-MOVE-T1-TO-PRINT.                                           ECS152
01032      MOVE T1-ISS-CNT             TO D1-ISS-CNT.                   ECS152
01033      MOVE T1-ISS-AMT             TO D1-ISS-AMT.                   ECS152
01034      MOVE T1-ISS-PRM             TO D1-ISS-PRM.                   ECS152
01035      MOVE T1-CAN-CNT             TO D1-CAN-CNT.                   ECS152
01036      MOVE T1-CAN-AMT             TO D1-CAN-AMT.                   ECS152
01037      MOVE T1-CAN-PRM             TO D1-CAN-PRM.                   ECS152
01038      MOVE T1-CLM-CNT             TO D1-CLM-CNT.                   ECS152
01039      MOVE T1-CLM-AMT             TO D1-CLM-AMT.                   ECS152
01040      MOVE T1-ERN-PRM             TO D1-ERN-AMT.                   ECS152
01041  0790-MOVE-T1-EXIT.                                               ECS152
01042      EXIT.                                                        ECS152
01043  EJECT                                                            ECS152
01044                                                                      CL**4
01045  0810-CHECK-FOR-PRINT.                                            ECS152
01046      MOVE SPACE                  TO DO-PRINT.                     ECS152

121510     MOVE 'Y'                    TO DO-PRINT
121510     MOVE +11                    TO Z
121510     GO TO 0820-CHECK-PRINT-EXIT

01047      IF TBL-ISSUE-CNT (Y, Z) NOT = +0                             ECS152
01048          MOVE 'Y'                TO DO-PRINT                      ECS152
01049          MOVE +11                TO Z                             ECS152
01050          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01051                                                                   ECS152
01052      IF TBL-ISSUE-AMT (Y, Z) NOT = +0                             ECS152
01053          MOVE 'Y'                TO DO-PRINT                      ECS152
01054          MOVE +11                TO Z                             ECS152
01055          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01056                                                                   ECS152
01057      IF TBL-ISSUE-PREM (Y, Z) NOT = +0                            ECS152
01058          MOVE 'Y'                TO DO-PRINT                      ECS152
01059          MOVE +11                TO Z                             ECS152
01060          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01061                                                                   ECS152
01062      IF TBL-CANCEL-CNT (Y, Z) NOT = +0                            ECS152
01063          MOVE 'Y'                TO DO-PRINT                      ECS152
01064          MOVE +11                TO Z                             ECS152
01065          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01066                                                                   ECS152
01067      IF TBL-CANCEL-AMT (Y, Z) NOT = +0                            ECS152
01068          MOVE 'Y'                TO DO-PRINT                      ECS152
01069          MOVE +11                TO Z                             ECS152
01070          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01071                                                                   ECS152
01072      IF TBL-CANCEL-PREM (Y, Z) NOT = +0                           ECS152
01073          MOVE 'Y'                TO DO-PRINT                      ECS152
01074          MOVE +11                TO Z                             ECS152
01075          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01076                                                                   ECS152
01077      IF TBL-CLAIM-CNT (Y, Z) NOT = +0                             ECS152
01078          MOVE 'Y'                TO DO-PRINT                      ECS152
01079          MOVE +11                TO Z                             ECS152
01080          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01081                                                                   ECS152
01082      IF TBL-CLAIM-AMT (Y, Z) NOT = +0                             ECS152
01083          MOVE 'Y'                TO DO-PRINT                      ECS152
01084          MOVE +11                TO Z                             ECS152
01085          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01086                                                                   ECS152
01087      IF TBL-EARN-PREM (Y, Z) NOT = +0                             ECS152
01088          MOVE 'Y'                TO DO-PRINT                      ECS152
01089          MOVE +11                TO Z                             ECS152
01090          GO TO 0820-CHECK-PRINT-EXIT.                             ECS152
01091                                                                   ECS152
01092  0820-CHECK-PRINT-EXIT.                                           ECS152
01093      EXIT.                                                        ECS152
01094  EJECT                                                            ECS152
01095                                                                      CL**4
01096  0840-DUMP-PAGE-1.                                                ECS152
01097 *                                                                 ECS152
01098 *    THIS PRINT STARTS WITH LINE 8 THRU END OF PAGE 1.            ECS152
01099 *                                                                 ECS152
01100      IF R-SW-G (Y) = SPACE                                        ECS152
01101          GO TO 0860-DUMP-1-EXIT.                                  ECS152
01102                                                                   ECS152
01103      MOVE HD-7                   TO PRT.                          ECS152
01104      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01105                                                                      CL**4
01106      MOVE P1-MESS (Y)            TO HD-9-MSG.                     ECS152
01107      MOVE HD-9                   TO PRT.                          ECS152
01108      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01109                                                                      CL**4
01110      IF Y GREATER +1                                                 CL**4
01111          GO TO 0850-CONT-DUMP-1.                                     CL**4
01112                                                                      CL**4
01113      MOVE HD-10                  TO PRT.                          ECS152
01114      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01115                                                                      CL**4
01116      MOVE HD-11                  TO PRT.                          ECS152
01117      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01118                                                                      CL**4
01119      MOVE HD-12                  TO PRT.                          ECS152
01120      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01121                                                                      CL**4
01122  0850-CONT-DUMP-1.                                                ECS152
01123                                                                      CL**4
01124      MOVE 'LIFE REDUCING  '      TO D1-MSG.                       ECS152
01125      MOVE +1                     TO Z.                            ECS152
01126      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01127      MOVE DETAIL-1               TO PRT.                          ECS152
01128      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01129                                                                      CL**4
01130      MOVE 'LIFE LEVEL     '      TO D1-MSG.                       ECS152
01131      MOVE +2                     TO Z.                            ECS152
01132      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01133      MOVE DETAIL-1               TO PRT.                          ECS152
01134      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01135                                                                      CL**4
01136      MOVE 'LIFE OUT BAL   '      TO D1-MSG.                       ECS152
01137      MOVE +3                     TO Z.                            ECS152
01138      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01139      MOVE DETAIL-1               TO PRT.                          ECS152
01140      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01141                                                                      CL**4
01142      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS.          ECS152
01143      MOVE ZERO-FIELDS-1          TO TOTAL-AMOUNTS.                ECS152
01144      PERFORM 0440-ADD-SUB-TOTALS THRU 0450-ADD-SUB-EXIT           ECS152
01145          VARYING Z FROM 1 BY 1 UNTIL Z GREATER 3.                 ECS152
01146      PERFORM 0760-MOVE-G1-TO-PRINT THRU 0770-MOVE-G1-EXIT.        ECS152
01147      MOVE '  TOTAL LIFE   '      TO D1-MSG.                       ECS152
01148      MOVE DETAIL-1               TO PRT.                          ECS152
01149      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01150                                                                      CL**4
01151      MOVE 'A-H RETRO/ELIM '      TO D1-MSG.                       ECS152
01152      MOVE +4                     TO Z.                            ECS152
01153      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01154      MOVE DETAIL-1               TO PRT.                          ECS152
01155      MOVE '0'                    TO P-CTL.                        ECS152
01156      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01157                                                                      CL**4
01158      MOVE 'A-H OUT BAL    '      TO D1-MSG.                       ECS152
01159      MOVE +5                     TO Z.                            ECS152
01160      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01161      MOVE DETAIL-1               TO PRT.                          ECS152
01162      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01163                                                                      CL**4
01164      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS.          ECS152
01165      PERFORM 0440-ADD-SUB-TOTALS THRU 0450-ADD-SUB-EXIT           ECS152
01166              VARYING Z FROM 4 BY 1 UNTIL Z GREATER 5.             ECS152
01167      PERFORM 0760-MOVE-G1-TO-PRINT THRU 0770-MOVE-G1-EXIT.        ECS152
01168      MOVE '  TOTAL A-H    '      TO D1-MSG.                       ECS152
01169      MOVE DETAIL-1               TO PRT.                          ECS152
01170      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01171                                                                      CL**4
01172      PERFORM 0780-MOVE-T1-TO-PRINT THRU 0790-MOVE-T1-EXIT.        ECS152
01173      MOVE '  TOTAL INS    '      TO D1-MSG.                       ECS152
01174      MOVE DETAIL-1 TO PRT.                                        ECS152
01175      MOVE '0'                    TO P-CTL.                        ECS152
01176      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01177                                                                      CL**4
01178  0860-DUMP-1-EXIT.                                                ECS152
01179      EXIT.                                                        ECS152
01180  EJECT                                                            ECS152
01181                                                                   ECS152
01182  0880-DUMP-PAGE-2.                                                ECS152
01183 *                                                                 ECS152
01184 *    THIS PRINT STARTS WITH LINE 8 OF PAGE 2 THRU END OF PAGE 2.  ECS152
01185 *                                                                 ECS152
01186      IF R-SW-I (Y) = SPACE                                        ECS152
01187          GO TO 0900-DUMP-2-FINAL.                                 ECS152
01188                                                                   ECS152
01189      MOVE HD-8                   TO PRT.                          ECS152
01190      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01191                                                                      CL**4
01192      MOVE P1-MESS (Y)            TO HD-9-MSG.                     ECS152
01193      MOVE HD-9                   TO PRT.                          ECS152
01194      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01195                                                                      CL**4
01196      IF Y GREATER +1                                                 CL**4
01197          GO TO 0890-CONT-DUMP-2.                                     CL**4
01198                                                                      CL**4
01199      MOVE HD-10                  TO PRT.                          ECS152
01200      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01201                                                                      CL**4
01202      MOVE HD-11                  TO PRT.                          ECS152
01203      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01204                                                                      CL**4
01205      MOVE HD-12                  TO PRT.                          ECS152
01206      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01207                                                                   ECS152
01208  0890-CONT-DUMP-2.                                                ECS152
01209      MOVE 'LIFE REDUCING  '      TO D1-MSG.                       ECS152
01210      MOVE +6                     TO Z.                            ECS152
01211      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01212      MOVE DETAIL-1               TO PRT.                          ECS152
01213      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01214                                                                      CL**4
01215      MOVE 'LIFE LEVEL     '      TO D1-MSG.                       ECS152
01216      MOVE +7                     TO Z.                            ECS152
01217      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01218      MOVE DETAIL-1               TO PRT.                          ECS152
01219      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01220                                                                      CL**4
01221      MOVE 'LIFE OUT BAL   '      TO D1-MSG.                       ECS152
01222      MOVE +8                     TO Z.                            ECS152
01223      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01224      MOVE DETAIL-1               TO PRT.                          ECS152
01225      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01226                                                                      CL**4
01227      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS.          ECS152
01228      MOVE ZERO-FIELDS-1          TO TOTAL-AMOUNTS.                ECS152
01229      PERFORM 0440-ADD-SUB-TOTALS THRU 0450-ADD-SUB-EXIT           ECS152
01230          VARYING Z FROM 6 BY 1 UNTIL Z GREATER 8.                 ECS152
01231      PERFORM 0760-MOVE-G1-TO-PRINT THRU 0770-MOVE-G1-EXIT.        ECS152
01232      MOVE '  TOTAL LIFE   '      TO D1-MSG.                       ECS152
01233      MOVE DETAIL-1               TO PRT.                          ECS152
01234      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01235                                                                      CL**4
01236      MOVE 'A-H RETRO/ELIM '      TO D1-MSG.                       ECS152
01237      MOVE +9                     TO Z.                            ECS152
01238      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01239      MOVE DETAIL-1               TO PRT.                          ECS152
01240      MOVE '0'                    TO P-CTL.                        ECS152
01241      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01242                                                                      CL**4
01243      MOVE 'A-H OUT BAL    '      TO D1-MSG.                       ECS152
01244      MOVE +10                    TO Z.                            ECS152
01245      PERFORM 0470-BUILD-DETAIL-1 THRU 0480-BUILD-D-EXIT.          ECS152
01246      MOVE DETAIL-1               TO PRT.                          ECS152
01247      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01248                                                                      CL**4
01249      MOVE ZERO-FIELDS-1          TO TOTAL-PRINT-AMOUNTS.          ECS152
01250      PERFORM 0440-ADD-SUB-TOTALS THRU 0450-ADD-SUB-EXIT           ECS152
01251          VARYING Z FROM 9 BY 1 UNTIL Z GREATER 10.                ECS152
01252      PERFORM 0760-MOVE-G1-TO-PRINT THRU 0770-MOVE-G1-EXIT.        ECS152
01253      MOVE '  TOTAL A-H    '      TO D1-MSG.                       ECS152
01254      MOVE DETAIL-1               TO PRT.                          ECS152
01255      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01256                                                                      CL**4
01257      PERFORM 0780-MOVE-T1-TO-PRINT THRU 0790-MOVE-T1-EXIT.        ECS152
01258      MOVE '  TOTAL INS    '      TO D1-MSG.                       ECS152
01259      MOVE DETAIL-1               TO PRT.                          ECS152
01260      MOVE '0'                    TO P-CTL.                        ECS152
01261      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01262                                                                   ECS152
01263  0900-DUMP-2-FINAL.                                               ECS152
01264                                                                      CL**4
01265      IF Y LESS +3                                                    CL**4
01266          GO TO 0910-DUMP-2-EXIT.                                     CL**4
01267                                                                      CL**4
01268      MOVE HD-15                  TO PRT.                          ECS152
01269      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01270                                                                      CL**4
01271      MOVE 'INCURRED IN THIS PERIOD -  ' TO D3-MSG.                ECS152
01272      MOVE TOT-AH-PD-THIS         TO D3-AH-AMT.                    ECS152
01273      MOVE DETAIL-3               TO PRT.                          ECS152
01274      MOVE '0'                    TO P-CTL.                        ECS152
01275      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01276                                                                      CL**4
01277      MOVE 'INCURRED IN PRIOR PERIOD - ' TO D3-MSG.                ECS152
01278      MOVE TOT-AH-PD-LAST         TO D3-AH-AMT.                    ECS152
01279      MOVE DETAIL-3               TO PRT.                          ECS152
01280      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01281                                                                      CL**4
01282  0910-DUMP-2-EXIT.                                                ECS152
01283      EXIT.                                                        ECS152
01284  EJECT                                                            ECS152
01285                                                                      CL**4
01286  0930-DUMP-PAGE-3.                                                ECS152
01287 *                                                                 ECS152
01288 *    THIS PRINT,  LIKE THE OTHERS,  STARTS WITH LINE 8.           ECS152
01289 *                                                                 ECS152
01290      MOVE HD-13                  TO PRT.                          ECS152
01291      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01292                                                                      CL**4
01293      MOVE HD-14                  TO PRT.                          ECS152
01294      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01295                                                                      CL**4
01296      MOVE 'IN FORCE AT END OF PREVIOUS PERIOD' TO D2-MSG.         ECS152
01297      MOVE +1                     TO Z.                            ECS152
01298      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01299      MOVE DETAIL-2               TO PRT.                          ECS152
01300      MOVE '0'                    TO P-CTL.                        ECS152
01301      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01302                                                                      CL**4
01303      MOVE 'ISSUED DURING THIS PERIOD         ' TO D2-MSG.         ECS152
01304      MOVE +2                     TO Z.                            ECS152
01305      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01306      MOVE DETAIL-2               TO PRT.                          ECS152
01307      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01308                                                                      CL**4
01309      MOVE '         SUB-TOTAL IN FORCE       ' TO D2-MSG.         ECS152
01310      MOVE +3                     TO Z.                            ECS152
01311      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01312      MOVE DETAIL-2               TO PRT.                          ECS152
01313      MOVE '0'                    TO P-CTL.                        ECS152
01314      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01315                                                                      CL**4
01316      MOVE 'DEATH CLAIMS PAID THIS PERIOD     ' TO D2-MSG.         ECS152
01317      MOVE +4                     TO Z.                            ECS152
01318      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01319      MOVE DETAIL-2               TO PRT.                          ECS152
01320      MOVE '0'                    TO P-CTL.                        ECS152
01321      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01322                                                                      CL**4
01323      MOVE 'EXPIRATIONS IN THIS PERIOD        ' TO D2-MSG.         ECS152
01324      MOVE +5                     TO Z.                            ECS152
01325      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01326      MOVE DETAIL-2               TO PRT.                          ECS152
01327      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01328                                                                      CL**4
01329      MOVE 'CANCELLATIONS IN THIS PERIOD      ' TO D2-MSG.         ECS152
01330      MOVE +6                     TO Z.                            ECS152
01331      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01332      MOVE DETAIL-2               TO PRT.                          ECS152
01333      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01334                                                                      CL**4
01335      MOVE 'OTHER DECREASES                   ' TO D2-MSG.         ECS152
01336      MOVE +7                     TO Z.                            ECS152
01337      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01338      MOVE DETAIL-2               TO PRT.                          ECS152
01339      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01340                                                                      CL**4
01341      MOVE '         SUB-TOTAL OF DEDUCTIONS  ' TO D2-MSG.         ECS152
01342      MOVE +8                     TO Z.                            ECS152
01343      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01344      MOVE DETAIL-2               TO PRT.                          ECS152
01345      MOVE '0'                    TO P-CTL.                        ECS152
01346      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01347                                                                      CL**4
01348      MOVE 'IN FORCE AT END OF CURRENT PERIOD ' TO D2-MSG.         ECS152
01349      MOVE +9                     TO Z.                            ECS152
01350      PERFORM 0490-BUILD-DETAIL-2 THRU 0500-BUILD-2-EXIT.          ECS152
01351      MOVE DETAIL-2               TO PRT.                          ECS152
01352      MOVE '0'                    TO P-CTL.                        ECS152
01353      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01354                                                                      CL**4
01355      MOVE ZERO TO D2-GRP-CNT  D2-GRP-AMT  D2-IND-CNT  D2-IND-AMT. ECS152
01356      MOVE ALL '*'                TO D2-MSG.                       ECS152
01357      MOVE DETAIL-2               TO PRT.                          ECS152
01358      MOVE '0'                    TO P-CTL.                        ECS152
01359      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01360                                                                      CL**4
01361      MOVE 'IN FORCE AT END OF PREVIOUS PERIOD' TO D2-MSG.         ECS152
01362      MOVE DETAIL-2               TO PRT.                          ECS152
01363      MOVE '0'                    TO P-CTL.                        ECS152
01364      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01365                                                                      CL**4
01366      MOVE '    TERMS OF 1 THRU 60 MONTHS     ' TO D2-MSG.         ECS152
01367      MOVE +1                     TO Y.                            ECS152
01368      MOVE +1                     TO Z.                            ECS152
01369      PERFORM 0510-BUILD-DETAIL-2A THRU 0520-BUILD-2A-EXIT.        ECS152
01370      MOVE DETAIL-2               TO PRT.                          ECS152
01371      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01372                                                                      CL**4
01373      MOVE '    TERMS OF 61 THRU 120 MONTHS   ' TO D2-MSG.         ECS152
01374      MOVE +2                     TO Y.                            ECS152
01375      PERFORM 0510-BUILD-DETAIL-2A THRU 0520-BUILD-2A-EXIT.        ECS152
01376      MOVE DETAIL-2               TO PRT.                          ECS152
01377      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01378                                                                      CL**4
01379      MOVE '    TERMS GREATER THAN 120 MONTHS ' TO D2-MSG.         ECS152
01380      MOVE +3                     TO Y.                            ECS152
01381      PERFORM 0510-BUILD-DETAIL-2A THRU 0520-BUILD-2A-EXIT.        ECS152
01382      MOVE DETAIL-2               TO PRT.                          ECS152
01383      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01384                                                                      CL**4
01385      MOVE ZERO TO D2-GRP-CNT  D2-GRP-AMT  D2-IND-CNT  D2-IND-AMT. ECS152
01386      MOVE 'IN FORCE AT END OF CURRENT PERIOD ' TO D2-MSG.         ECS152
01387      MOVE DETAIL-2               TO PRT.                          ECS152
01388      MOVE '0'                    TO P-CTL.                        ECS152
01389      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01390                                                                      CL**4
01391      MOVE '    TERMS OF 1 THRU 60 MONTHS     ' TO D2-MSG.         ECS152
01392      MOVE +1                     TO Y.                            ECS152
01393      MOVE +9                     TO Z.                            ECS152
01394      PERFORM 0510-BUILD-DETAIL-2A THRU 0520-BUILD-2A-EXIT.        ECS152
01395      MOVE DETAIL-2               TO PRT.                          ECS152
01396      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01397                                                                      CL**4
01398      MOVE '    TERMS OF 61 THRU 120 MONTHS   ' TO D2-MSG.         ECS152
01399      MOVE +2                     TO Y.                            ECS152
01400      PERFORM 0510-BUILD-DETAIL-2A THRU 0520-BUILD-2A-EXIT.        ECS152
01401      MOVE DETAIL-2               TO PRT.                          ECS152
01402      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01403                                                                      CL**4
01404      MOVE '    TERMS GREATER THAN 120 MONTHS ' TO D2-MSG.         ECS152
01405      MOVE +3                     TO Y.                            ECS152
01406      PERFORM 0510-BUILD-DETAIL-2A THRU 0520-BUILD-2A-EXIT.        ECS152
01407      MOVE DETAIL-2               TO PRT.                          ECS152
01408      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01409                                                                      CL**4
01410  0940-DUMP-3-EXIT.                                                ECS152
01411      EXIT.                                                        ECS152
01412  EJECT                                                            ECS152
01413                                                                   ECS152
01414  0960-NO-PRINT.                                                   ECS152
01415      MOVE RUN-DATE TO WS-WORK-DATE.                               ECS152
01416      MOVE WS-DAY                 TO HD-5-DAY.                     ECS152
01417      MOVE WS-YEAR                TO HD-5-YEAR.                    ECS152
01418      MOVE MONTH-ALPHA (WS-MONTH) TO HD-5-MONTH.                   ECS152
01419                                                                   ECS152
01420      MOVE EP-DT                  TO WS-WORK-DATE.                 ECS152
01421      MOVE WS-DAY                 TO HD-6-DAY.                     ECS152
01422      MOVE WS-YEAR                TO HD-6-YEAR.                    ECS152
01423      MOVE MONTH-ALPHA (WS-MONTH) TO HD-6-MONTH.                   ECS152
01424                                                                   ECS152
01425      PERFORM 0710-PRINT-HEADINGS THRU 0720-P-H-X.                 ECS152
01426      MOVE 'PROGRAM COMPLETED - NO PRINT DATA' TO P-DATA.          ECS152
01427      PERFORM 0730-PRT-RTN THRU 0740-PRT-RTN-X.                    ECS152
01428                                                                   ECS152
01429  0970-END-JOB.                                                    ECS152
01430      CLOSE  PRT-FILE.                                             ECS152
01431      IF FICH-OPEN = 'X'                                           ECS152
01432          CLOSE FICH.                                              ECS152
01433  0980-EOJ-EXIT.                                                      CL**4
01434      EXIT.                                                           CL**4
01435                                                                      CL**4
01436  ABEND-PGM.                      COPY ELCABEND.                   ECS152
01437 /                                                                 ECS152
01438  LCP-WRITE-POS-PRT SECTION.                                       ECS152
01439      IF LCP-ASA = '+'                                             ECS152
01440          WRITE PRT AFTER 0 LINE                                   ECS152
01441      ELSE                                                         ECS152
01442      IF LCP-ASA = ' '                                             ECS152
01443          WRITE PRT AFTER ADVANCING 1 LINE                         ECS152
01444      ELSE                                                         ECS152
01445      IF LCP-ASA = '0'                                             ECS152
01446          WRITE PRT AFTER ADVANCING 2 LINE                         ECS152
01447      ELSE                                                         ECS152
01448      IF LCP-ASA = '-'                                             ECS152
01449          WRITE PRT AFTER ADVANCING 3 LINE                         ECS152
01450      ELSE                                                         ECS152
01451      IF LCP-ASA = '1'                                             ECS152
01452          WRITE PRT AFTER ADVANCING PAGE                           ECS152
01453      ELSE                                                         ECS152
01454      IF LCP-ASA = '2'                                             ECS152
01455          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS152
01456      ELSE                                                         ECS152
01457      IF LCP-ASA = '3'                                             ECS152
01458          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS152
01459      ELSE                                                         ECS152
01460      IF LCP-ASA = '4'                                             ECS152
01461          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS152
01462      ELSE                                                         ECS152
01463      IF LCP-ASA = '5'                                             ECS152
01464          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS152
01465      ELSE                                                         ECS152
01466      IF LCP-ASA = '6'                                             ECS152
01467          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS152
01468      ELSE                                                         ECS152
01469      IF LCP-ASA = '7'                                             ECS152
01470          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS152
01471      ELSE                                                         ECS152
01472      IF LCP-ASA = '8'                                             ECS152
01473          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS152
01474      ELSE                                                         ECS152
01475      IF LCP-ASA = '9'                                             ECS152
01476          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS152
01477      ELSE                                                         ECS152
01478      IF LCP-ASA = 'A'                                             ECS152
01479          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS152
01480      ELSE                                                         ECS152
01481      IF LCP-ASA = 'B'                                             ECS152
01482          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS152
01483      ELSE                                                         ECS152
01484      IF LCP-ASA = 'C'                                             ECS152
01485          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS152
01486      ELSE                                                         ECS152
01487      IF LCP-ASA = 'V'                                             ECS152
01488          WRITE PRT AFTER ADVANCING LCP-P01                        ECS152
01489      ELSE                                                         ECS152
01490      IF LCP-ASA = 'W'                                             ECS152
01491          WRITE PRT AFTER ADVANCING LCP-P02                        ECS152
01492      ELSE                                                         ECS152
01493      DISPLAY 'ASA CODE ERROR'.                                    ECS152
01494  LCP-WRITE-END-PRT.                                               ECS152
01495      EXIT.                                                        ECS152
01496                                                                   ECS152
