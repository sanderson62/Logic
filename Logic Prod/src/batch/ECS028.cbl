00001  IDENTIFICATION DIVISION.                                         05/06/98
00002                                                                   ECS028
00003  PROGRAM-ID.                ECS028.                                  LV009
00004 *              PROGRAM CONVERTED BY                               ECS028
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS028
00006 *              CONVERSION DATE 11/28/95 11:05:26.                 ECS028
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS028
00008 *                           VMOD=2.011.                           ECS028
00009                                                                   ECS028
00010 *AUTHOR.        LOGIC, INC.                                       ECS028
00011 *               DALLAS, TEXAS.                                    ECS028
00012                                                                   ECS028
00013 *DATE-COMPILED.                                                   ECS028
00014                                                                   ECS028
00015 *SECURITY.   *****************************************************ECS028
00016 *            *                                                   *ECS028
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS028
00018 *            *                                                   *ECS028
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS028
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS028
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS028
00022 *            *                                                   *ECS028
00023 *            *****************************************************ECS028
00024                                                                   ECS028
00025 *REMARKS.  PRODUCTION SUMMARY REPORT                              ECS028
00026                                                                   ECS028
00027 *            SUMMARIZE DETAIL PRODUCTION EXTRACTS FOR STORAGE     ECS028
00028 *            AND REPORT PER SUMMARIZED PRODUCTION EXTRACT.        ECS028
00029 *                                                                 ECS028
00030 *        ----FUNCTIONAL OVERVIEW:                                 ECS028
00031 *            ACCUMULATE FIGURES FROM DETAIL PRODUCTION EXTRACTS   ECS028
00032 *            UNTIL THE CONTROL KEY DIFFERS FROM THE PREVIOUS, THENECS028
00033 *                                                                 ECS028
00034 *            CALCULATE  -   LOSS RATIOS                           ECS028
00035 *                         * PREMIUM DISTRIBUTION                  ECS028
00036 *                         * CLAIM PAYMENT DISTRIBUTION            ECS028
00037 *                                BY TERM AND AGE                  ECS028
00038 *                                                                 ECS028
00039 *            WRITE THE SUMMARIZED PRODUCTION EXTRACT              ECS028
00040 *                                                                 ECS028
00041 *            PRINT THE PRODUCTION SUMMARY AND IT'S GRAPH          ECS028
00042                                                                   ECS028
00043 *            REPEAT ABOVE STEPS UNTIL ALL DETAIL PRODUCTION       ECS028
00044 *            EXTRACTS HAVE BEEN PROCESSED.                        ECS028
00045 *                                                                 ECS028
00046 *        ----DIRECT PROGRAM INPUTS:                               ECS028
00047 *            DATE (DISK) CARD                                     ECS028
00048 *            PRODUCTION SUMMARY DETAIL EXTRACTS                   ECS028
00049 *                                                                 ECS028
00050 *        ----DIRECT PROGRAM OUTPUTS:                              ECS028
00051 *            PRODUCTION SUMMARY EXTRACTS (SUMMARIZED)             ECS028
00052 *            PRODUCTION SUMMARY REPORT (ECS028)                   ECS028
00053  EJECT                                                            ECS028
00054  ENVIRONMENT DIVISION.                                            ECS028
00055  CONFIGURATION SECTION.                                           ECS028
00056  SPECIAL-NAMES.                                                   ECS028
00057      C02 IS LCP-CH2                                               ECS028
00058      C03 IS LCP-CH3                                               ECS028
00059      C04 IS LCP-CH4                                               ECS028
00060      C05 IS LCP-CH5                                               ECS028
00061      C06 IS LCP-CH6                                               ECS028
00062      C07 IS LCP-CH7                                               ECS028
00063      C08 IS LCP-CH8                                               ECS028
00064      C09 IS LCP-CH9                                               ECS028
00065      C10 IS LCP-CH10                                              ECS028
00066      C11 IS LCP-CH11                                              ECS028
00067      C12 IS LCP-CH12                                              ECS028
00068      S01 IS LCP-P01                                               ECS028
00069      S02 IS LCP-P02.                                              ECS028
00070  INPUT-OUTPUT SECTION.                                            ECS028
00071  FILE-CONTROL.                                                    ECS028
00072                                                                   ECS028
00073      SELECT PRINTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS028
00074      SELECT PROD-SUM-EXT     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS028
00075      SELECT PROD-DTL-EXT     ASSIGN TO SYS016-UT-FBA1-S-SYS016.   ECS028
00076      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS028
00077      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS028
00078                                                                   ECS028
00079  DATA DIVISION.                                                   ECS028
00080  FILE SECTION.                                                    ECS028
00081                                                                   ECS028
00082  FD  PRINTR                                                       ECS028
00083      RECORDING MODE F.                                            ECS028
00084                                                                   ECS028
00085  01  PRT.                                                         ECS028
00086      12  PRT-DATA.                                                ECS028
00087          16  PRT-CNTL        PIC X.                               ECS028
00088          16  FILLER          PIC X(132).                          ECS028
00089                                                                   ECS028
00090  FD  PROD-SUM-EXT                                                 ECS028
00091      BLOCK CONTAINS 0 RECORDS
00092      RECORDING MODE F.                                            ECS028
00093                                                                   ECS028
00094  01  PROD-SUMMARY-EXTRACT.                                        ECS028
00095      12  PSX-LEAD-DATA       PIC X(224).                             CL**7
00096      12  PSX-ACCUM-DATA      PIC X(2016).                            CL**7
00097                                                                   ECS028
00098  FD  PROD-DTL-EXT                                                 ECS028
00099      BLOCK CONTAINS 0 RECORDS
00100      RECORDING MODE F.                                            ECS028
00101                                                                   ECS028
00102  01  PROD-DETAIL-EXTRACT.                                         ECS028
00103      12  FILLER              PIC X(2240).                         ECS028
00104                                                                   ECS028
00105  FD  DISK-DATE                                                    ECS028
00106                              COPY ELCDTEFD.                       ECS028
00107  EJECT                                                            ECS028
00108  FD  FICH                                                         ECS028
00109                              COPY ELCFCHFD.                       ECS028
00110  EJECT                                                            ECS028
00111                                                                   ECS028
00112  WORKING-STORAGE SECTION.                                         ECS028
00113  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS028
00114  77  LCP-ASA                       PIC X.                         ECS028
00115  77  FILLER PIC X(32) VALUE '********************************'.   ECS028
00116  77  FILLER PIC X(32) VALUE '**  ECS028  WORKING STORAGE   **'.   ECS028
00117  77  FILLER PIC X(32) VALUE '***********VMOD=2.011 **********'.   ECS028
00118                                                                   ECS028
00119  77  INDX                    PIC S9(4)     COMP   VALUE ZEROS.    ECS028
00120  77  PAGE-CNT                PIC S9(9)     COMP-3 VALUE ZEROS.    ECS028
00121  77  PERCENT-1               PIC S9(9)     COMP-3 VALUE ZEROS.    ECS028
00122  77  PERCENT-2               PIC S9(9)     COMP-3 VALUE ZEROS.    ECS028
00123  77  PERCENT-3               PIC S9(9)     COMP-3 VALUE ZEROS.    ECS028
00124  77  DETAIL-RECORD-COUNT     PIC S9(9)     COMP-3 VALUE ZEROS.    ECS028
00125  77  SUMMARY-RECORD-COUNT    PIC S9(9)     COMP-3 VALUE ZEROS.    ECS028
00126  77  PLUG-SUM                PIC S9(11)    COMP-3 VALUE +22000.   ECS028
00127  77  PLUG-BASE               PIC S9(11)    COMP-3 VALUE +500.     ECS028
00128  77  FLAT-SUM-1              PIC S9(11)    COMP-3 VALUE ZEROS.    ECS028
00129  77  FLAT-SUM-2              PIC S9(11)    COMP-3 VALUE ZEROS.    ECS028
00130  77  AMT-1                   PIC S9(11)V99 COMP-3 VALUE ZEROS.    ECS028
00131  77  AMT-2                   PIC S9(11)V99 COMP-3 VALUE ZEROS.    ECS028
00132  77  AMT-3                   PIC S9(11)V99 COMP-3 VALUE ZEROS.    ECS028
00133  77  AMT-4                   PIC S9(11)V99 COMP-3 VALUE ZEROS.    ECS028
00134  77  AMT-SUM                 PIC S9(11)V99 COMP-3 VALUE ZEROS.    ECS028
00135  77  RECORD-READ             PIC X                VALUE 'N'.      ECS028
00136  77  TOTAL-INDIC-SW          PIC X                VALUE 'N'.      ECS028
00137  77  GRAPH-PRT-SW            PIC X                VALUE 'N'.      ECS028
00138  77  DETAIL-EXTRACTS-EOF-INDIC                                    ECS028
00139                              PIC X                VALUE 'N'.      ECS028
00140      88  DETAIL-EXTRACTS-EOF                      VALUE 'Y'.      ECS028
00141  77  NWPC                    PIC X                VALUE 'W'.      ECS028
00142  77  EPC                     PIC X                VALUE 'E'.      ECS028
00143  77  CPC                     PIC X                VALUE 'C'.      ECS028
00144  77  INTERSEC                PIC X                VALUE '+'.      ECS028
00145  77  FILL-CHAR               PIC X                VALUE ' '.      ECS028
00146  77  FOUR                    PIC 9                VALUE 4.        ECS028
00147  77  POS-ROUNDER             PIC SV9              VALUE +.5.      ECS028
00148  77  NEG-ROUNDER             PIC SV9              VALUE -.5.      ECS028
00149  77  NINES-DATE              PIC 9(11) COMP-3  VALUE 99999999999.    CL**3
00150  77  CURRENT-LABEL           PIC X(8)            VALUE 'CURRENT'. ECS028
00151  77  COUNT-DISPLAY           PIC ZZZ,ZZZ,ZZ9      VALUE ZEROS.    ECS028
00152                                                                   ECS028
00153  01  ABEND-DATA.                                                  ECS028
00154      12  ABEND-CODE          PIC X(4)            VALUE ZEROS.     ECS028
00155      12  ABEND-OPTION        PIC X               VALUE 'Y'.       ECS028
00156      12  PGM-SUB             PIC S9(3)   COMP    VALUE +028.      ECS028
00157      12  WS-RETURN-CODE      PIC S9(4)   COMP.                    ECS028
00158      12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.    ECS028
00159      12  WS-ZERO             PIC S9      COMP-3  VALUE +0.        ECS028
00160      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZEROS.     ECS028
00161  EJECT                                                            ECS028
00162                                  COPY ECSPSE01.                   ECS028
00163                                                                   ECS028
00164  01  SSC-DYNAMIC-REQUESTS.                                        ECS028
00165      12  SSC-DYN-HEIRARCHY-LIST.                                  ECS028
00166          16  SSC-H-RPT-CD-1      PIC X.                           ECS028
00167              88  SSC-RPT-CD-1        VALUE '1', '2', '3',         ECS028
00168                                            '4', '5', '6',         ECS028
00169                                            '7', '8', '9'.         ECS028
00170          16  SSC-H-CARRIER       PIC X.                           ECS028
00171              88  SSC-CARRIER         VALUE '1', '2', '3',         ECS028
00172                                            '4', '5', '6',         ECS028
00173                                            '7', '8', '9'.         ECS028
00174          16  SSC-H-GROUP         PIC X.                           ECS028
00175              88  SSC-GROUP           VALUE '1', '2', '3',         ECS028
00176                                            '4', '5', '6',         ECS028
00177                                            '7', '8', '9'.         ECS028
00178          16  SSC-H-RPT-CD-2      PIC X.                           ECS028
00179              88  SSC-RPT-CD-2        VALUE '1', '2', '3',         ECS028
00180                                            '4', '5', '6',         ECS028
00181                                            '7', '8', '9'.         ECS028
00182          16  SSC-H-STATE         PIC X.                           ECS028
00183              88  SSC-STATE           VALUE '1', '2', '3',         ECS028
00184                                            '4', '5', '6',         ECS028
00185                                            '7', '8', '9'.         ECS028
00186          16  SSC-H-RETRO-GROUP   PIC X.                           ECS028
00187              88  SSC-RETRO           VALUE '1', '2', '3',         ECS028
00188                                            '4', '5', '6',         ECS028
00189                                            '7', '8', '9'.         ECS028
00190          16  SSC-H-ACCOUNT       PIC X.                           ECS028
00191              88  SSC-ACCOUNT         VALUE '1', '2', '3',         ECS028
00192                                            '4', '5', '6',         ECS028
00193                                            '7', '8', '9'.         ECS028
00194          16  SSC-H-REGION        PIC X.                           ECS028
00195              88  SSC-REGION          VALUE '1', '2', '3',         ECS028
00196                                            '4', '5', '6',         ECS028
00197                                            '7', '8', '9'.         ECS028
00198          16  SSC-H-BUSINESS-TYPE PIC X.                           ECS028
00199              88  SSC-BUS-TYPE        VALUE '1', '2', '3',         ECS028
00200                                            '4', '5', '6',         ECS028
00201                                            '7', '8', '9'.         ECS028
00202          16  SSC-H-FILLER        PIC XX.                          ECS028
00203  EJECT                                                            ECS028
00204  01  HOLD-LEAD-DETAIL.                                            ECS028
00205      12  HLD-SORT-KEY.                                            ECS028
00206          16  HLD-SK-ID           PIC X.                           ECS028
00207              88  HLD-ACCOUNT-REQ                 VALUE 'A'.       ECS028
00208              88  HLD-GA-REQ                      VALUE 'B'.       ECS028
00209              88  HLD-GA-ACCT-REQ                 VALUE 'C'.       ECS028
00210              88  HLD-STATE-REQ                   VALUE 'D'.       ECS028
00211              88  HLD-CARRIER-REQ                 VALUE 'E'.       ECS028
00212              88  HLD-GROUP-REQ                   VALUE 'F'.       ECS028
00213              88  HLD-RETRO-GRP-REQ               VALUE 'G'.       ECS028
00214              88  HLD-REGION-REQ                  VALUE 'H'.       ECS028
00215              88  HLD-BUSINESS-TYPE-REQ           VALUE 'I'.       ECS028
00216              88  HLD-RPT-CD-1-REQ                VALUE 'J'.       ECS028
00217              88  HLD-RPT-CD-2-REQ                VALUE 'K'.       ECS028
00218              88  HLD-DYNAMIC-REQUEST             VALUE 'Z'.       ECS028
00219          16  FILLER              PIC X(90).                       ECS028
00220      12  HLD-RPT-CD-1            PIC X(10).                       ECS028
00221      12  HLD-CARRIER             PIC X.                           ECS028
00222      12  HLD-GROUP               PIC X(6).                        ECS028
00223      12  HLD-RPT-CD-2            PIC X(10).                       ECS028
00224      12  HLD-STATE               PIC XX.                          ECS028
00225      12  HLD-ACCOUNT-NUMBER      PIC X(10).                       ECS028
00226      12  HLD-ACCT-TERMIN-DATE    PIC 9(11)    COMP-3.                CL**6
00227      12  HLD-NAME                PIC X(30).                       ECS028
00228      12  HLD-ACCT-STATUS         PIC X(10).                       ECS028
00229      12  HLD-ACCT-CONTRACT-DATE  PIC 9(11)    COMP-3.                CL**5
00230      12  HLD-ACCT-ANNIV-DATE     PIC 9(11)    COMP-3.                CL**5
00231      12  HLD-BUSINESS-TYPE       PIC 99.                          ECS028
00232      12  HLD-GA                  PIC X(10).                       ECS028
00233      12  HLD-RETRO-GROUP         PIC X(6).                        ECS028
00234      12  HLD-REGION              PIC XX.                          ECS028
00235      12  FILLER                  PIC X(4).                           CL**5
00236      12  HLD-RETRO-AMT-PAID      PIC S9(9)V99 COMP-3.             ECS028
00237      12  HLD-ACCT-RETEN-LIFE     PIC S9(3)V99 COMP-3.             ECS028
00238      12  HLD-ACCT-RETEN-AH       PIC S9(3)V99 COMP-3.             ECS028
00239  EJECT                                                            ECS028
00240  01  EXTRACT-ACCUMULATION-DATA.                                   ECS028
00241      12  EAD-MONTH-ARRAY-AREA.                                    ECS028
00242          16  EAD-MONTH-ARRAY OCCURS 7 TIMES                       ECS028
00243                                  INDEXED BY EADM.                 ECS028
00244              20  EAD-ML-COUNT            PIC S9(7)       COMP-3.  ECS028
00245              20  EAD-ML-PREM             PIC S9(9)V99    COMP-3.  ECS028
00246              20  EAD-ML-EARN-PREM        PIC S9(9)V99    COMP-3.  ECS028
00247              20  EAD-ML-COMMIS           PIC S9(9)V99    COMP-3.  ECS028
00248              20  EAD-ML-CLAIM-INCURRED   PIC S9(9)V99    COMP-3.  ECS028
00249              20  EAD-ML-CLAIMS-PAID      PIC S9(9)V99    COMP-3.  ECS028
00250              20  EAD-MA-COUNT            PIC S9(7)       COMP-3.  ECS028
00251              20  EAD-MA-PREM             PIC S9(9)V99    COMP-3.  ECS028
00252              20  EAD-MA-EARN-PREM        PIC S9(9)V99    COMP-3.  ECS028
00253              20  EAD-MA-COMMIS           PIC S9(9)V99    COMP-3.  ECS028
00254              20  EAD-MA-CLAIM-INCURRED   PIC S9(9)V99    COMP-3.  ECS028
00255              20  EAD-MA-CLAIMS-PAID      PIC S9(9)V99    COMP-3.  ECS028
00256      12  EAD-TERM-AGE-ARRAY.                                      ECS028
00257          16  EAD-TERM-ARRAY  OCCURS 7 TIMES                       ECS028
00258                                  INDEXED BY EADT.                 ECS028
00259              20  EAD-L-WTD-PREM          PIC S9(11)V99 COMP-3.    ECS028
00260              20  EAD-L-AGE-COUNT-PREM    PIC S9(7)       COMP-3.  ECS028
00261              20  EAD-L-AGE-SUM-PREM      PIC S9(9)       COMP-3.  ECS028
00262              20  EAD-L-WTD-CLAIMS        PIC S9(11)V99 COMP-3.    ECS028
00263              20  EAD-L-AGE-COUNT-CLMS    PIC S9(7)       COMP-3.  ECS028
00264              20  EAD-L-AGE-SUM-CLMS      PIC S9(9)       COMP-3.  ECS028
00265              20  EAD-A-WTD-PREM          PIC S9(11)V99 COMP-3.    ECS028
00266              20  EAD-A-AGE-COUNT-PREM    PIC S9(7)       COMP-3.  ECS028
00267              20  EAD-A-AGE-SUM-PREM      PIC S9(9)       COMP-3.  ECS028
00268              20  EAD-A-WTD-CLAIMS        PIC S9(11)V99 COMP-3.    ECS028
00269              20  EAD-A-AGE-COUNT-CLMS    PIC S9(7)       COMP-3.  ECS028
00270              20  EAD-A-AGE-SUM-CLMS      PIC S9(9)       COMP-3.  ECS028
00271              20  EAD-AGE-ARRAY                                    ECS028
00272                              OCCURS 5 TIMES                       ECS028
00273                                  INDEXED BY EADA.                 ECS028
00274                  24  EAD-LIFE-DATA.                               ECS028
00275                      28  EAD-L-PREM      PIC S9(9)V99    COMP-3.  ECS028
00276                      28  EAD-L-CLAIMS-PAID                        ECS028
00277                                          PIC S9(9)V99    COMP-3.  ECS028
00278                  24  EAD-AH-DATA.                                 ECS028
00279                      28  EAD-A-PREM      PIC S9(9)V99    COMP-3.  ECS028
00280                      28  EAD-A-CLAIMS-PAID                        ECS028
00281                                          PIC S9(9)V99    COMP-3.  ECS028
00282     12  EAD-GRAPH-DATA       OCCURS 12 TIMES                      ECS028
00283                                  INDEXED BY EADGP.                ECS028
00284         16  EAD-NET-WRITTEN-PREM         PIC S9(11)V99 COMP-3.    ECS028
00285         16  EAD-EARNED-PREM              PIC S9(11)V99 COMP-3.    ECS028
00286         16  EAD-CLAIMS-PAID              PIC S9(11)V99 COMP-3.    ECS028
00287  EJECT                                                            ECS028
00288  01  SUM-ACCUMULATION-DATA.                                       ECS028
00289      12  SUM-MONTH-ARRAY-AREA.                                    ECS028
00290          16  SUM-MONTH-ARRAY OCCURS 7 TIMES                       ECS028
00291                                  INDEXED BY SUMM.                 ECS028
00292              20  SUM-ML-COUNT            PIC S9(7)       COMP-3.  ECS028
00293              20  SUM-ML-PREM             PIC S9(9)V99    COMP-3.  ECS028
00294              20  SUM-ML-EARN-PREM        PIC S9(9)V99    COMP-3.  ECS028
00295              20  SUM-ML-COMMIS           PIC S9(9)V99    COMP-3.  ECS028
00296              20  SUM-ML-CLAIM-INCURRED   PIC S9(9)V99    COMP-3.  ECS028
00297              20  SUM-ML-CLAIMS-PAID      PIC S9(9)V99    COMP-3.  ECS028
00298              20  SUM-MA-COUNT            PIC S9(7)       COMP-3.  ECS028
00299              20  SUM-MA-PREM             PIC S9(9)V99    COMP-3.  ECS028
00300              20  SUM-MA-EARN-PREM        PIC S9(9)V99    COMP-3.  ECS028
00301              20  SUM-MA-COMMIS           PIC S9(9)V99    COMP-3.  ECS028
00302              20  SUM-MA-CLAIM-INCURRED   PIC S9(9)V99    COMP-3.  ECS028
00303              20  SUM-MA-CLAIMS-PAID      PIC S9(9)V99    COMP-3.  ECS028
00304      12  SUM-TERM-AGE-ARRAY.                                      ECS028
00305          16  SUM-TERM-ARRAY  OCCURS 7 TIMES                       ECS028
00306                                  INDEXED BY SUMT.                 ECS028
00307              20  SUM-L-WTD-PREM          PIC S9(11)V99 COMP-3.    ECS028
00308              20  SUM-L-AGE-COUNT-PREM    PIC S9(7)       COMP-3.  ECS028
00309              20  SUM-L-AGE-SUM-PREM      PIC S9(9)       COMP-3.  ECS028
00310              20  SUM-L-WTD-CLAIMS        PIC S9(11)V99 COMP-3.    ECS028
00311              20  SUM-L-AGE-COUNT-CLMS    PIC S9(7)       COMP-3.  ECS028
00312              20  SUM-L-AGE-SUM-CLMS      PIC S9(9)       COMP-3.  ECS028
00313              20  SUM-A-WTD-PREM          PIC S9(11)V99 COMP-3.    ECS028
00314              20  SUM-A-AGE-COUNT-PREM    PIC S9(7)       COMP-3.  ECS028
00315              20  SUM-A-AGE-SUM-PREM      PIC S9(9)       COMP-3.  ECS028
00316              20  SUM-A-WTD-CLAIMS        PIC S9(11)V99 COMP-3.    ECS028
00317              20  SUM-A-AGE-COUNT-CLMS    PIC S9(7)       COMP-3.  ECS028
00318              20  SUM-A-AGE-SUM-CLMS      PIC S9(9)       COMP-3.  ECS028
00319              20  SUM-AGE-ARRAY                                    ECS028
00320                              OCCURS 5 TIMES                       ECS028
00321                                  INDEXED BY SUMA.                 ECS028
00322                  24  SUM-LIFE-DATA.                               ECS028
00323                      28  SUM-L-PREM      PIC S9(9)V99    COMP-3.  ECS028
00324                      28  SUM-L-CLAIMS-PAID                        ECS028
00325                                          PIC S9(9)V99    COMP-3.  ECS028
00326                  24  SUM-AH-DATA.                                 ECS028
00327                      28  SUM-A-PREM      PIC S9(9)V99    COMP-3.  ECS028
00328                      28  SUM-A-CLAIMS-PAID                        ECS028
00329                                          PIC S9(9)V99    COMP-3.  ECS028
00330      12  SUM-GRAPH-DATA      OCCURS 12 TIMES                      ECS028
00331                                  INDEXED BY SUMGP.                ECS028
00332          16  SUM-NET-WRITTEN-PREM        PIC S9(11)V99 COMP-3.    ECS028
00333          16  SUM-EARNED-PREM             PIC S9(11)V99 COMP-3.    ECS028
00334          16  SUM-CLAIMS-PAID             PIC S9(11)V99 COMP-3.    ECS028
00335      12  SUM-RPT-TYPE-BASE               PIC X VALUE SPACE.       ECS028
00336  EJECT                                                            ECS028
00337  01  DATE-WORK.                                                   ECS028
00338      12  DATE-WORK-CYMD    PIC  9(11).                               CL**8
00339      12  DATE-WORK-CYMD-N  REDEFINES  DATE-WORK-CYMD.                CL**8
00340          16  FILLER          PIC  999.                               CL**2
00341          16  DATE-CC         PIC  99.                                CL**2
00342          16  DATE-YR         PIC  99.                                CL**2
00343          16  DATE-MO         PIC  99.                             ECS028
00344          16  DATE-DA         PIC  99.                             ECS028
00345                                                                      CL**2
00346      12  MONTH-TABLE.                                             ECS028
00347          16  FILLER          PIC X(6)            VALUE 'JAN 31'.  ECS028
00348          16  FILLER          PIC X(6)            VALUE 'FEB 28'.  ECS028
00349          16  FILLER          PIC X(6)            VALUE 'MAR 31'.  ECS028
00350          16  FILLER          PIC X(6)            VALUE 'APR 30'.  ECS028
00351          16  FILLER          PIC X(6)            VALUE 'MAY 31'.  ECS028
00352          16  FILLER          PIC X(6)            VALUE 'JUN 30'.  ECS028
00353          16  FILLER          PIC X(6)            VALUE 'JUL 31'.  ECS028
00354          16  FILLER          PIC X(6)            VALUE 'AUG 31'.  ECS028
00355          16  FILLER          PIC X(6)            VALUE 'SEP 30'.  ECS028
00356          16  FILLER          PIC X(6)            VALUE 'OCT 31'.  ECS028
00357          16  FILLER          PIC X(6)            VALUE 'NOV 30'.  ECS028
00358          16  FILLER          PIC X(6)            VALUE 'DEC 31'.  ECS028
00359      12  MONTH-TBL REDEFINES MONTH-TABLE.                         ECS028
00360          16  FILLER      OCCURS 12 TIMES.                         ECS028
00361              20  MONTH-N     PIC XXX.                             ECS028
00362              20  FILLER      PIC X.                               ECS028
00363              20  MONTH-D     PIC 99.                              ECS028
00364                                                                      CL**4
00365      12  PRT-DATE.                                                ECS028
00366          16  PD-MO           PIC 99.                              ECS028
00367          16  FILLER          PIC X               VALUE '-'.       ECS028
00368          16  PD-DAY          PIC 99.                              ECS028
00369          16  FILLER          PIC X               VALUE '-'.       ECS028
00370          16  PD-YR           PIC 99.                              ECS028
00371  EJECT                                                            ECS028
00372  01  TERM-LABLES.                                                 ECS028
00373      12  LABLE1-TERM.                                             ECS028
00374          16  FILLER              PIC XXX         VALUE '01-'.     ECS028
00375          16  LABLE1-TERM-HIGH    PIC 99.                          ECS028
00376      12  LABLE2-TERM.                                             ECS028
00377          16  LABLE2-TERM-LOW     PIC 99.                          ECS028
00378          16  FILLER              PIC X           VALUE '-'.       ECS028
00379          16  LABLE2-TERM-HIGH    PIC 99.                          ECS028
00380      12  LABLE3-TERM.                                             ECS028
00381          16  LABLE3-TERM-LOW     PIC 99.                          ECS028
00382          16  FILLER              PIC X           VALUE '-'.       ECS028
00383          16  LABLE3-TERM-HIGH    PIC 99.                          ECS028
00384      12  LABLE4-TERM.                                             ECS028
00385          16  FILLER              PIC X           VALUE SPACE.     ECS028
00386          16  LABLE4-TERM-LOW     PIC 99.                          ECS028
00387          16  FILLER              PIC XX          VALUE '+'.       ECS028
00388      12  LABLET-TERM             PIC X(5)        VALUE 'TOTAL'.   ECS028
00389      12  LABLEY-TERM             PIC X(5)        VALUE ' YTD'.    ECS028
00390      12  LABLEI-TERM             PIC X(5)        VALUE ' ITD'.    ECS028
00391  EJECT                                                            ECS028
00392  01  PRT-HDR-1.                                                   ECS028
00393      12  FILLER              PIC X(32)           VALUE '1'.       ECS028
00394      12  FILLER              PIC X(18)           VALUE            ECS028
00395                                          'PRODUCTION SUMMARY'.    ECS028
00396      12  FILLER              PIC X(22)           VALUE SPACES.    ECS028
00397      12  FILLER              PIC X(6)            VALUE 'ECS028'.  ECS028
00398      12  HD1-REPORT-TYPE     PIC X.                               ECS028
00399                                                                   ECS028
00400  01  PRT-HDR-2.                                                   ECS028
00401      12  FILLER              PIC X(26)           VALUE ' '.       ECS028
00402      12  HD2-CO-NAME         PIC X(30).                           ECS028
00403      12  FILLER              PIC X(16)           VALUE SPACES.    ECS028
00404      12  HD2-RUN-DATE        PIC X(8).                            ECS028
00405                                                                   ECS028
00406  01  PRT-HDR-2A.                                                  ECS028
00407      12  FILLER              PIC X(32)           VALUE ' '.       ECS028
00408      12  HD2A-ALPHA-DATE     PIC X(18)           VALUE SPACES.    ECS028
00409      12  FILLER              PIC X(16)           VALUE SPACES.    ECS028
00410      12  FILLER              PIC X(6)            VALUE 'PAGE'.    ECS028
00411      12  HD2A-PAGE           PIC ZZZZZZZZ9.                       ECS028
00412                                                                   ECS028
00413  01  PRT-DTL-1.                                                   ECS028
00414      12  FILLER              PIC X               VALUE ' '.       ECS028
00415      12  FILLER              PIC X(13)           VALUE ' NAME'.   ECS028
00416      12  DT1-NAME            PIC X(30).                           ECS028
00417      12  FILLER              PIC X(8)            VALUE SPACES.    ECS028
00418      12  FILLER              PIC X(18)           VALUE            ECS028
00419                                                 'RETRO GROUP'.    ECS028
00420      12  DT1-RETRO-GRP       PIC X(6).                            ECS028
00421                                                                   ECS028
00422  01  PRT-DTL-2.                                                   ECS028
00423      12  FILLER              PIC X               VALUE ' '.       ECS028
00424      12  FILLER              PIC X(9)            VALUE            ECS028
00425                                                    ' CARRIER'.    ECS028
00426      12  DT2-CARRIER         PIC X.                               ECS028
00427      12  FILLER              PIC X               VALUE SPACES.    ECS028
00428      12  DT2-CARR-NAME       PIC X(30)           VALUE SPACES.    ECS028
00429      12  FILLER              PIC X(18)           VALUE            ECS028
00430                                                 'RETROS PAID'.    ECS028
00431      12  DT2-RETRO-AMT       PIC ZZZ,ZZZ,ZZ9.99.                  ECS028
00432  EJECT                                                            ECS028
00433  01  PRT-DTL-3.                                                   ECS028
00434      12  FILLER              PIC X               VALUE ' '.       ECS028
00435      12  FILLER              PIC X(13)           VALUE            ECS028
00436                                                     ' GROUP'.     ECS028
00437      12  DT3-GROUP           PIC X(6).                            ECS028
00438      12  FILLER              PIC X(22)           VALUE SPACES.    ECS028
00439      12  FILLER              PIC X(18)           VALUE            ECS028
00440                                                     'REGION'.     ECS028
00441      12  DT3-REGION          PIC XX.                              ECS028
00442                                                                   ECS028
00443  01  PRT-DTL-4.                                                   ECS028
00444      12  FILLER              PIC X               VALUE ' '.       ECS028
00445      12  FILLER              PIC X(13)           VALUE            ECS028
00446                                                      ' STATE'.    ECS028
00447      12  DT4-STATE           PIC XX.                              ECS028
00448      12  FILLER              PIC XX              VALUE SPACES.    ECS028
00449      12  DT4-STATE-DESC      PIC X(20)           VALUE SPACES.    ECS028
00450      12  FILLER              PIC X(4)            VALUE SPACES.    ECS028
00451      12  FILLER              PIC X(18)           VALUE            ECS028
00452                                               'GENERAL AGENT'.    ECS028
00453      12  DT4-GA              PIC X(10).                           ECS028
00454                                                                   ECS028
00455  01  PRT-DTL-4A.                                                  ECS028
00456      12  FILLER              PIC X               VALUE ' '.       ECS028
00457      12  FILLER              PIC X(13)           VALUE            ECS028
00458                                                ' REPORT CD 1'.    ECS028
00459      12  DT4A-RPT-CD-1       PIC X(10)           VALUE SPACES.    ECS028
00460      12  FILLER              PIC X(17)           VALUE SPACES.    ECS028
00461      12  FILLER              PIC X(14)           VALUE            ECS028
00462                                                ' REPORT CD 2'.    ECS028
00463      12  DT4A-RPT-CD-2       PIC X(10)           VALUE SPACES.    ECS028
00464                                                                   ECS028
00465  01  PRT-DTL-5.                                                   ECS028
00466      12  FILLER              PIC X               VALUE ' '.       ECS028
00467      12  FILLER              PIC X(13)           VALUE            ECS028
00468                                               ' ACCT NUMBER '.    ECS028
00469      12  DT5-ACCT-NUMBER     PIC X(10).                           ECS028
00470      12  FILLER              PIC X(18)           VALUE SPACES.    ECS028
00471      12  FILLER              PIC X(10)           VALUE            ECS028
00472                                                  'RETENTION-'.    ECS028
00473      12  DT5-RETEN-HDR       PIC X(6)            VALUE SPACES.    ECS028
00474      12  FILLER              PIC XX              VALUE SPACES.    ECS028
00475      12  DT5-RETEN-LIFE      PIC ZZ9.99.                          ECS028
00476                                                                   ECS028
00477  01  PRT-DTL-6.                                                   ECS028
00478      12  FILLER              PIC X               VALUE ' '.       ECS028
00479      12  FILLER              PIC X(13)           VALUE            ECS028
00480                                                ' BUSINESS TY'.    ECS028
00481      12  DT6-BUS-TYPE        PIC XX.                              ECS028
00482      12  FILLER              PIC XX              VALUE SPACES.    ECS028
00483      12  DT6-BUS-TYPE-DESC   PIC X(24).                           ECS028
00484      12  FILLER              PIC X(10)           VALUE SPACES.    ECS028
00485      12  DT6-RETEN-HDR       PIC X(6)            VALUE SPACES.    ECS028
00486      12  FILLER              PIC XX              VALUE SPACES.    ECS028
00487      12  DT6-RETEN-AH        PIC ZZ9.99.                          ECS028
00488  EJECT                                                            ECS028
00489  01  PRT-DTL-7.                                                   ECS028
00490      12  FILLER              PIC X               VALUE ' '.       ECS028
00491      12  FILLER              PIC X(13)           VALUE            ECS028
00492                                              ' CONTRACT DT'.      ECS028
00493      12  DT7-CNTR-DATE.                                           ECS028
00494          16  DT7-CNTR-MO     PIC 99.                              ECS028
00495          16  FILLER          PIC X               VALUE '-'.       ECS028
00496          16  DT7-CNTR-DAY    PIC 99.                              ECS028
00497          16  FILLER          PIC X               VALUE '-'.       ECS028
00498          16  DT7-CNTR-YR     PIC 99.                              ECS028
00499      12  FILLER              PIC X(20)           VALUE SPACES.    ECS028
00500      12  FILLER              PIC X(18)           VALUE            ECS028
00501                                                    'STATUS'.      ECS028
00502      12  DT7-STATUS          PIC X(10).                           ECS028
00503                                                                   ECS028
00504  01  PRT-DTL-8.                                                   ECS028
00505      12  FILLER              PIC X               VALUE ' '.       ECS028
00506      12  FILLER              PIC X(13)           VALUE            ECS028
00507                                             ' ANNIVER. DT'.       ECS028
00508      12  DT8-ANNV-DATE.                                           ECS028
00509          16  DT8-ANNV-MO     PIC 99.                              ECS028
00510          16  FILLER          PIC X               VALUE '-'.       ECS028
00511          16  DT8-ANNV-DAY    PIC 99.                              ECS028
00512          16  FILLER          PIC X               VALUE '-'.       ECS028
00513          16  DT8-ANNV-YR     PIC 99.                              ECS028
00514      12  FILLER              PIC X(20)           VALUE SPACES.    ECS028
00515      12  FILLER              PIC X(18)           VALUE            ECS028
00516                                              'TERMINATION DT'.    ECS028
00517      12  DT8-TERMIN-DATE     PIC X(8)            VALUE SPACES.    ECS028
00518  EJECT                                                            ECS028
00519  01  PRT-DTL-9.                                                   ECS028
00520      12  FILLER              PIC X(15)           VALUE ' '.       ECS028
00521      12  FILLER              PIC X(12)           VALUE            ECS028
00522                                              'QUARTERS  1='.      ECS028
00523      12  DT9-QTR-1A          PIC X(3).                            ECS028
00524      12  FILLER              PIC X               VALUE '-'.       ECS028
00525      12  DT9-QTR-1B          PIC X(3).                            ECS028
00526      12  FILLER              PIC X(4)            VALUE '  2='.    ECS028
00527      12  DT9-QTR-2A          PIC X(3).                            ECS028
00528      12  FILLER              PIC X               VALUE '-'.       ECS028
00529      12  DT9-QTR-2B          PIC X(3).                            ECS028
00530      12  FILLER              PIC X(4)            VALUE '  3='.    ECS028
00531      12  DT9-QTR-3A          PIC X(3).                            ECS028
00532      12  FILLER              PIC X               VALUE '-'.       ECS028
00533      12  DT9-QTR-3B          PIC X(3).                            ECS028
00534      12  FILLER              PIC X(4)            VALUE '  4='.    ECS028
00535      12  DT9-QTR-4A          PIC X(3).                            ECS028
00536      12  FILLER              PIC X               VALUE '-'.       ECS028
00537      12  DT9-QTR-4B          PIC X(3).                            ECS028
00538                                                                   ECS028
00539  01  PRT-HDR-3.                                                   ECS028
00540      12  FILLER              PIC X(9)            VALUE '0'.       ECS028
00541      12  FILLER              PIC X(31)           VALUE            ECS028
00542                      'NET ISSUE COUNT    *       NET '.           ECS028
00543      12  HD3-COMP-DESC       PIC X(39)           VALUE            ECS028
00544              'PREMIUM       *    ACCT COMPENSATION   '.           ECS028
00545                                                                   ECS028
00546  01  PRT-HDR-4.                                                   ECS028
00547      12  FILLER              PIC X               VALUE ' '.       ECS028
00548      12  FILLER              PIC X(7)            VALUE ' QTR'.    ECS028
00549      12  HD4-LF-1            PIC X(6)            VALUE SPACES.    ECS028
00550      12  FILLER              PIC X(6)            VALUE SPACES.    ECS028
00551      12  HD4-AH-1            PIC X(6)            VALUE SPACES.    ECS028
00552      12  FILLER              PIC X(6)            VALUE SPACES.    ECS028
00553      12  HD4-LF-2            PIC X(6)            VALUE SPACES.    ECS028
00554      12  FILLER              PIC X(7)            VALUE SPACES.    ECS028
00555      12  HD4-AH-2            PIC X(6)            VALUE SPACES.    ECS028
00556      12  FILLER              PIC X(7)            VALUE SPACES.    ECS028
00557      12  HD4-LF-3            PIC X(6)            VALUE SPACES.    ECS028
00558      12  FILLER              PIC X(7)            VALUE SPACES.    ECS028
00559      12  HD4-AH-3            PIC X(6)            VALUE SPACES.    ECS028
00560  EJECT                                                            ECS028
00561  01  PRT-DTL-10.                                                  ECS028
00562      12  PRT-10-LINE     OCCURS 7 TIMES                           ECS028
00563                              INDEXED BY P10.                      ECS028
00564          16  DT10-PRT-CNTL       PIC X.                           ECS028
00565          16  DT10-LABLE          PIC X(5).                        ECS028
00566          16  FILLER              PIC X.                           ECS028
00567          16  DT10-LIFE-ISSUES    PIC Z,ZZZ,ZZ9-.                  ECS028
00568          16  FILLER              PIC X.                           ECS028
00569          16  DT10-AH-ISSUES      PIC Z,ZZZ,ZZ9-.                  ECS028
00570          16  FILLER              PIC X.                           ECS028
00571          16  DT10-LIFE-PREM      PIC ZZZ,ZZZ,ZZ9-.                ECS028
00572          16  FILLER              PIC X.                           ECS028
00573          16  DT10-AH-PREM        PIC ZZZ,ZZZ,ZZ9-.                ECS028
00574          16  FILLER              PIC X.                           ECS028
00575          16  DT10-LIFE-COMMIS    PIC ZZZ,ZZZ,ZZ9-.                ECS028
00576          16  FILLER              PIC X.                           ECS028
00577          16  DT10-AH-COMMIS      PIC ZZZ,ZZZ,ZZ9-.                ECS028
00578                                                                   ECS028
00579  01  PRT-HDR-5.                                                   ECS028
00580      12  FILLER              PIC X(12)           VALUE '0'.       ECS028
00581      12  FILLER              PIC X(30)           VALUE            ECS028
00582                         'EARNED PREMIUM     *    CLAIMS'.         ECS028
00583      12  FILLER              PIC X(31)           VALUE            ECS028
00584              ' INCURRED    *      CLAIMS PAID'.                   ECS028
00585                                                                   ECS028
00586  01  PRT-HDR-6.                                                   ECS028
00587      12  FILLER              PIC X               VALUE ' '.       ECS028
00588      12  FILLER              PIC X(6)            VALUE 'QTR'.     ECS028
00589      12  HD6-LF-1            PIC X(6)            VALUE SPACES.    ECS028
00590      12  FILLER              PIC X(7)            VALUE SPACES.    ECS028
00591      12  HD6-AH-1            PIC X(6)            VALUE SPACES.    ECS028
00592      12  FILLER              PIC X(8)            VALUE SPACES.    ECS028
00593      12  HD6-LF-2            PIC X(6)            VALUE SPACES.    ECS028
00594      12  FILLER              PIC X(6)            VALUE SPACES.    ECS028
00595      12  HD6-AH-2            PIC X(6)            VALUE SPACES.    ECS028
00596      12  FILLER              PIC X(7)            VALUE SPACES.    ECS028
00597      12  HD6-LF-3            PIC X(6)            VALUE SPACES.    ECS028
00598      12  FILLER              PIC X(6)            VALUE SPACES.    ECS028
00599      12  HD6-AH-3            PIC X(6)            VALUE SPACES.    ECS028
00600                                                                   ECS028
00601  01  PRT-DTL-11.                                                  ECS028
00602      12  PRT-11-LINE     OCCURS 7 TIMES                           ECS028
00603                              INDEXED BY P11.                      ECS028
00604          16  DT11-PRT-CNTL       PIC X.                           ECS028
00605          16  DT11-LABLE          PIC X(4).                        ECS028
00606          16  DT11-LIFE-EARNED    PIC ZZZ,ZZZ,ZZ9-.                ECS028
00607          16  FILLER              PIC X.                           ECS028
00608          16  DT11-AH-EARNED      PIC ZZZ,ZZZ,ZZ9-.                ECS028
00609          16  FILLER              PIC X.                           ECS028
00610          16  DT11-LIFE-INCUR     PIC ZZZ,ZZZ,ZZ9-.                ECS028
00611          16  FILLER              PIC X.                           ECS028
00612          16  DT11-AH-INCUR       PIC ZZZ,ZZZ,ZZ9-.                ECS028
00613          16  FILLER              PIC X.                           ECS028
00614          16  DT11-LIFE-CLM-PAID  PIC ZZZ,ZZZ,ZZ9.                 ECS028
00615          16  FILLER              PIC X.                           ECS028
00616          16  DT11-AH-CLM-PAID    PIC ZZZ,ZZZ,ZZ9.                 ECS028
00617  EJECT                                                            ECS028
00618  01  PRT-HDR-7.                                                   ECS028
00619      12  FILLER                  PIC X(34)       VALUE '0'.       ECS028
00620      12  FILLER                  PIC X(14)       VALUE            ECS028
00621                                        'CLAIMS RATIO %'.          ECS028
00622                                                                   ECS028
00623  01  PRT-HDR-8.                                                   ECS028
00624      12  FILLER                  PIC X(11)       VALUE SPACES.    ECS028
00625      12  FILLER                  PIC X(27)       VALUE            ECS028
00626                        'QTR-1    QTR-2    QTR-3'.                 ECS028
00627      12  FILLER                  PIC X(32)       VALUE            ECS028
00628              'QTR-4   LAST 12     YTD      ITD'.                  ECS028
00629                                                                   ECS028
00630  01  PRT-DTL-12.                                                  ECS028
00631      12  PRT-12-LINE     OCCURS 3 TIMES                           ECS028
00632                              INDEXED BY P12.                      ECS028
00633          16  DT12-PRT-CNTL       PIC X.                           ECS028
00634          16  FILLER              PIC X(4).                        ECS028
00635          16  DT12-LABLE          PIC X(7).                        ECS028
00636          16  FILLER      OCCURS 7 TIMES                           ECS028
00637                              INDEXED BY P12Q.                     ECS028
00638              20  DT12-RATIO-QTR.                                  ECS028
00639                  24  DT12-R-QTR  PIC ZZZ9-.                       ECS028
00640              20  FILLER          PIC X(4).                        ECS028
00641                                                                   ECS028
00642  01  PRT-HDR-9A.                                                  ECS028
00643      12  FILLER              PIC X(30)           VALUE '0'.       ECS028
00644      12  FILLER              PIC X(22)           VALUE            ECS028
00645                                    'PREMIUM DISTRIBUTION %'.      ECS028
00646                                                                   ECS028
00647  01  PRT-HDR-9B.                                                  ECS028
00648      12  FILLER              PIC X               VALUE '0'.       ECS028
00649      12  FILLER              PIC X(26)           VALUE SPACES.    ECS028
00650      12  FILLER              PIC X(28)           VALUE            ECS028
00651              'CLAIM PAYMENT DISTRIBUTION %'.                      ECS028
00652                                                                   ECS028
00653  01  PRT-HDR-10.                                                  ECS028
00654      12  FILLER              PIC X               VALUE ' '.       ECS028
00655      12  FILLER              PIC X(40)           VALUE            ECS028
00656              '            LIFE   (LAST 12 MONTHS)'.               ECS028
00657      12  FILLER              PIC X(33)           VALUE            ECS028
00658              '   *       A&H   (LAST 12 MONTHS)'.                 ECS028
00659  EJECT                                                            ECS028
00660  01  PRT-DTL-14.                                                  ECS028
00661      12  FILLER              PIC X               VALUE ' '.       ECS028
00662      12  FILLER              PIC X(10)           VALUE            ECS028
00663                                               '   AGE  1-'.       ECS028
00664      12  DT14-L-AGE-1-HIGH   PIC 99.                              ECS028
00665      12  FILLER              PIC X               VALUE SPACE.     ECS028
00666      12  DT14-L-AGE-2-LOW    PIC 99.                              ECS028
00667      12  FILLER              PIC X               VALUE '-'.       ECS028
00668      12  DT14-L-AGE-2-HIGH   PIC 99.                              ECS028
00669      12  FILLER              PIC X               VALUE SPACE.     ECS028
00670      12  DT14-L-AGE-3-LOW    PIC 99.                              ECS028
00671      12  FILLER              PIC X               VALUE '-'.       ECS028
00672      12  DT14-L-AGE-3-HIGH   PIC 99.                              ECS028
00673      12  FILLER              PIC X               VALUE SPACE.     ECS028
00674      12  DT14-L-AGE-4-LOW    PIC 99.                              ECS028
00675      12  FILLER              PIC X               VALUE '-'.       ECS028
00676      12  DT14-L-AGE-4-HIGH   PIC 99.                              ECS028
00677      12  FILLER              PIC X               VALUE SPACE.     ECS028
00678      12  DT14-L-AGE-5-LOW    PIC 99.                              ECS028
00679      12  FILLER              PIC X(12)           VALUE            ECS028
00680                                                 '+ AVG WTD'.      ECS028
00681      12  FILLER              PIC XX              VALUE '1-'.      ECS028
00682      12  DT14-A-AGE-1-HIGH   PIC 99.                              ECS028
00683      12  FILLER              PIC X               VALUE SPACE.     ECS028
00684      12  DT14-A-AGE-2-LOW    PIC 99.                              ECS028
00685      12  FILLER              PIC X               VALUE '-'.       ECS028
00686      12  DT14-A-AGE-2-HIGH   PIC 99.                              ECS028
00687      12  FILLER              PIC X               VALUE SPACE.     ECS028
00688      12  DT14-A-AGE-3-LOW    PIC 99.                              ECS028
00689      12  FILLER              PIC X               VALUE '-'.       ECS028
00690      12  DT14-A-AGE-3-HIGH   PIC 99.                              ECS028
00691      12  FILLER              PIC X               VALUE SPACE.     ECS028
00692      12  DT14-A-AGE-4-LOW    PIC 99.                              ECS028
00693      12  FILLER              PIC X               VALUE '-'.       ECS028
00694      12  DT14-A-AGE-4-HIGH   PIC 99.                              ECS028
00695      12  FILLER              PIC X               VALUE SPACE.     ECS028
00696      12  DT14-A-AGE-5-LOW    PIC 99.                              ECS028
00697      12  FILLER              PIC X(9)            VALUE            ECS028
00698                                                  '+ AVG WTD'.     ECS028
00699                                                                   ECS028
00700  01  PRT-HDR-11.                                                  ECS028
00701      12  FILLER              PIC X(5)            VALUE            ECS028
00702                                                   ' TERM'.        ECS028
00703  EJECT                                                            ECS028
00704  01  PRT-DTL-15.                                                  ECS028
00705      12  PRT-15-LINE     OCCURS 7 TIMES                           ECS028
00706                              INDEXED BY P15.                      ECS028
00707          16  DT15-PRT-CNTL       PIC X.                           ECS028
00708          16  FILLER              PIC X.                           ECS028
00709          16  DT15-TERM-LABLE     PIC X(5).                        ECS028
00710          16  FILLER              PIC X(3).                        ECS028
00711          16  DT15-L-AGE-1.                                        ECS028
00712              20  DT15-L-AGE1     PIC ZZ9.                         ECS028
00713          16  FILLER              PIC XX.                          ECS028
00714          16  DT15-L-AGE-2.                                        ECS028
00715              20  DT15-L-AGE2     PIC ZZ9.                         ECS028
00716          16  FILLER              PIC X(3).                        ECS028
00717          16  DT15-L-AGE-3.                                        ECS028
00718              20  DT15-L-AGE3     PIC ZZ9.                         ECS028
00719          16  FILLER              PIC X(3).                        ECS028
00720          16  DT15-L-AGE-4.                                        ECS028
00721              20  DT15-L-AGE4     PIC ZZ9.                         ECS028
00722          16  FILLER              PIC XX.                          ECS028
00723          16  DT15-L-AGE-5.                                        ECS028
00724              20  DT15-L-AGE5     PIC ZZ9.                         ECS028
00725          16  FILLER              PIC X.                           ECS028
00726          16  DT15-L-AGE-AVG.                                      ECS028
00727              20  DT15-L-AVG      PIC ZZ9.                         ECS028
00728          16  FILLER              PIC X.                           ECS028
00729          16  DT15-L-WTD-X.                                        ECS028
00730              20  DT15-L-WTD      PIC ZZ9.                         ECS028
00731          16  FILLER              PIC X(4).                        ECS028
00732          16  DT15-A-AGE-1.                                        ECS028
00733              20  DT15-A-AGE1     PIC ZZ9.                         ECS028
00734          16  FILLER              PIC XX.                          ECS028
00735          16  DT15-A-AGE-2.                                        ECS028
00736              20  DT15-A-AGE2     PIC ZZ9.                         ECS028
00737          16  FILLER              PIC X(3).                        ECS028
00738          16  DT15-A-AGE-3.                                        ECS028
00739              20  DT15-A-AGE3     PIC ZZ9.                         ECS028
00740          16  FILLER              PIC X(3).                        ECS028
00741          16  DT15-A-AGE-4.                                        ECS028
00742              20  DT15-A-AGE4     PIC ZZ9.                         ECS028
00743          16  FILLER              PIC XX.                          ECS028
00744          16  DT15-A-AGE-5.                                        ECS028
00745              20  DT15-A-AGE5     PIC ZZ9.                         ECS028
00746          16  FILLER              PIC X.                           ECS028
00747          16  DT15-A-AGE-AVG.                                      ECS028
00748              20  DT15-A-AVG      PIC ZZ9.                         ECS028
00749          16  FILLER              PIC X.                           ECS028
00750          16  DT15-A-WTD-X.                                        ECS028
00751              20  DT15-A-WTD      PIC ZZ9.                         ECS028
00752  EJECT                                                            ECS028
00753  01  HOLD-BRACKET-AMOUNTS.                                        ECS028
00754      12  HLD-BRKT-AMOUNT     PIC S9(11)  COMP-3.                  ECS028
00755      12  FILLER          OCCURS 45 TIMES.                         ECS028
00756          16  HLD-BRKT-AMT    PIC S9(11)  COMP-3.                  ECS028
00757                                                                   ECS028
00758  01  PRT-GRAPH-LINE-1.                                            ECS028
00759      12  FILLER          OCCURS 45 TIMES                          ECS028
00760                              INDEXED BY PRT1A.                    ECS028
00761          16  PRT-1-DATA.                                          ECS028
00762              20  PRT-1-PRT-CNTL  PIC X.                           ECS028
00763              20  PRT-BRACKET-AMT PIC ZZ,ZZZ,ZZZ,ZZ9.              ECS028
00764              20  FILLER          PIC X(4).                        ECS028
00765              20  FILLER  OCCURS 12 TIMES                          ECS028
00766                              INDEXED BY PRT1B.                    ECS028
00767                  24  PRT-1-MONTH PIC X.                           ECS028
00768                  24  PRT-FILL-1  PIC X.                           ECS028
00769                  24  PRT-FILL-2  PIC X.                           ECS028
00770                  24  PRT-FILL-3  PIC X.                           ECS028
00771                  24  PRT-FILL-4  PIC X.                           ECS028
00772                                                                   ECS028
00773  01  PRT-GRAPH-LINE-2.                                            ECS028
00774      12  FILLER              PIC X(11)           VALUE '0'.       ECS028
00775      12  FILLER              PIC X(5)            VALUE 'MONTH'.   ECS028
00776      12  PRT-2-MONTHS.                                            ECS028
00777          16  FILLER      OCCURS 12 TIMES                          ECS028
00778                              INDEXED BY PRT2.                     ECS028
00779              20  FILLER      PIC XX.                              ECS028
00780              20  PRT-2-MONTH PIC X(3).                            ECS028
00781                                                                   ECS028
00782  01  GRAPH-HEADER-LINE-1.                                         ECS028
00783      12  FILLER              PIC X(29)           VALUE '1'.       ECS028
00784      12  FILLER              PIC X(24)           VALUE            ECS028
00785                                      'PRODUCTION SUMMARY GRAPH'.  ECS028
00786      12  FILLER              PIC X(19)           VALUE SPACES.    ECS028
00787      12  FILLER              PIC X(6)            VALUE 'ECS028'.  ECS028
00788      12  GH1-REPORT-TYPE     PIC X.                               ECS028
00789  EJECT                                                            ECS028
00790  01  GRAPH-FILL-IN-WORK.                                          ECS028
00791      12  GFI-HOLD-LINE-P     PIC S9(4)   COMP.                    ECS028
00792      12  GFI-HOLD-LINE-E     PIC S9(4)   COMP.                    ECS028
00793      12  GFI-HOLD-LINE-C     PIC S9(4)   COMP.                    ECS028
00794      12  GFI-LINE-DIFF-1     PIC S9(4)   COMP.                    ECS028
00795          88  GFI-LD1-4               VALUE -4, -3, -2, -1, -0,    ECS028
00796                                            +0, +1, +2, +3, +4.    ECS028
00797      12  GFI-LINE-DIFF-2     PIC  9(4).                           ECS028
00798      12  FILLER          OCCURS 12 TIMES                          ECS028
00799                              INDEXED BY GFI.                      ECS028
00800          16  GFI-PREM-LINE   PIC S9(4)   COMP.                    ECS028
00801          16  GFI-EARN-LINE   PIC S9(4)   COMP.                    ECS028
00802          16  GFI-CLM-LINE    PIC S9(4)   COMP.                    ECS028
00803                                                                   ECS028
00804  01  LEGION-OF-GRAPH-CHARS.                                       ECS028
00805      12  FILLER              PIC X               VALUE '0'.       ECS028
00806      12  FILLER              PIC X(5)            VALUE SPACES.    ECS028
00807      12  LGC-NWPC            PIC X.                               ECS028
00808      12  FILLER              PIC X(3)            VALUE ' = '.     ECS028
00809      12  FILLER              PIC X(19)           VALUE            ECS028
00810                                         'NET WRITTEN PREMIUM'.    ECS028
00811      12  FILLER              PIC X(5)            VALUE SPACES.    ECS028
00812      12  LGC-EPC             PIC X.                               ECS028
00813      12  FILLER              PIC X(3)            VALUE ' = '.     ECS028
00814      12  FILLER              PIC X(14)           VALUE            ECS028
00815                                              'EARNED PREMIUM'.    ECS028
00816      12  FILLER              PIC X(5)            VALUE SPACES.    ECS028
00817      12  LGC-CPC             PIC X.                               ECS028
00818      12  FILLER              PIC X(3)            VALUE ' = '.     ECS028
00819      12  FILLER              PIC X(15)           VALUE            ECS028
00820                                            'INCURRED CLAIMS'.     ECS028
00821  EJECT                                                            ECS028
00822  01  REPORT-TYPE-IDENT-AREA.                                      ECS028
00823      12  RPT-TID-DETAIL.                                          ECS028
00824          16  RPT-TID-1       PIC X(51)           VALUE            ECS028
00825         '1     ECS028 REPORT TYPE IDENTIFICATIONS           '.    ECS028
00826          16  RPT-TID-2       PIC X(51)           VALUE            ECS028
00827         '-REPORT ID  DESCRIPTION                            '.    ECS028
00828          16  RPT-TID-3       PIC X(51)           VALUE            ECS028
00829         '0ECS028A  STANDARD REQUEST BY ACCOUNT (FULL CNTRL) '.    ECS028
00830          16  RPT-TID-4       PIC X(51)           VALUE            ECS028
00831         ' ECS028B  STANDARD REQUEST BY G.A.                 '.    ECS028
00832          16  RPT-TID-5       PIC X(51)           VALUE            ECS028
00833         ' ECS028C  STANDARD REQUEST BY ACCOUNT WITHIN G.A.  '.    ECS028
00834          16  RPT-TID-6       PIC X(51)           VALUE            ECS028
00835         ' ECS028D  STANDARD REQUEST BY STATE                '.    ECS028
00836          16  RPT-TID-7       PIC X(51)           VALUE            ECS028
00837         ' ECS028E  STANDARD REQUEST BY CARRIER              '.    ECS028
00838          16  RPT-TID-8       PIC X(51)           VALUE            ECS028
00839         ' ECS028F  STANDARD REQUEST BY GROUP                '.    ECS028
00840          16  RPT-TID-9       PIC X(51)           VALUE            ECS028
00841         ' ECS028G  STANDARD REQUEST BY RETRO GROUP          '.    ECS028
00842          16  RPT-TID-A       PIC X(51)           VALUE            ECS028
00843         ' ECS028H  STANDARD REQUEST BY REGION               '.    ECS028
00844          16  RPT-TID-B       PIC X(51)           VALUE            ECS028
00845         ' ECS028I  STANDARD REQUEST BY BUSINESS TYPE        '.    ECS028
00846          16  RPT-TID-C       PIC X(51)           VALUE            ECS028
00847         ' ECS028J  STANDARD REQUEST BY REPORT CODE 1        '.    ECS028
00848          16  RPT-TID-D       PIC X(51)           VALUE            ECS028
00849         ' ECS028K  STANDARD REQUEST BY REPORT CODE 2        '.    ECS028
00850          16  RPT-TID-E       PIC X(51)           VALUE            ECS028
00851         '        *                                          '.    ECS028
00852          16  RPT-TID-F       PIC X(51)           VALUE            ECS028
00853         ' ECS028Z   DYNAMIC REQUEST (REFER TO REQUEST)      '.    ECS028
00854      12  FILLER REDEFINES RPT-TID-DETAIL.                         ECS028
00855           16  RPT-TID-LINE   PIC X(51)                            ECS028
00856                          OCCURS 15 TIMES                          ECS028
00857                              INDEXED BY RPTX.                     ECS028
00858                                                                   ECS028
00859                              COPY ELCDTECX.                       ECS028
00860                                                                   ECS028
00861                              COPY ELCDTEVR.                       ECS028
00862  EJECT                                                            ECS028
00863  PROCEDURE DIVISION.                                              ECS028
00864                                                                   ECS028
00865  0001-MAINLINE.                                                   ECS028
00866                                                                   ECS028
00867      PERFORM 0003-INITIALIZATION THRU 0101-EXIT.                  ECS028
00868                                                                   ECS028
00869      PERFORM 1000-PROCESS-DETAIL-EXTRACTS THRU 1001-EXIT          ECS028
00870                             UNTIL DETAIL-EXTRACTS-EOF.            ECS028
00871                                                                   ECS028
00872      PERFORM 0200-WRAPUP THRU 0210-EXIT.                          ECS028
00873                                                                   ECS028
00874      GOBACK.                                                      ECS028
00875  EJECT                                                            ECS028
00876  0003-INITIALIZATION.                                             ECS028
00877                    COPY ELCDTERX.                                 ECS028
00878                                                                   ECS028
00879      OPEN INPUT  PROD-DTL-EXT                                     ECS028
00880           OUTPUT PRINTR                                           ECS028
00881                  PROD-SUM-EXT.                                    ECS028
00882                                                                   ECS028
00883      IF DTE-FICH NOT = SPACE                                      ECS028
00884          MOVE 'X'       TO FICH-OPEN                              ECS028
00885          OPEN OUTPUT FICH.                                        ECS028
00886                                                                   ECS028
00887      PERFORM 1100-READ-DETAIL-EXTR THRU 1100-EXIT.                ECS028
00888                                                                   ECS028
00889      IF DETAIL-EXTRACTS-EOF                                       ECS028
00890          MOVE '0727' TO ABEND-CODE                                ECS028
00891          DISPLAY 'NO DATA IN PRODUCTION DETAIL EXTRACT FILE'      ECS028
00892          GO TO ABEND-PGM.                                         ECS028
00893                                                                   ECS028
00894      MOVE PSC-DYN-HEIRARCHY-LIST TO SSC-DYNAMIC-REQUESTS.         ECS028
00895                                                                   ECS028
00896      IF GRAPH-OUT                                                 ECS028
00897          MOVE 'Y' TO GRAPH-PRT-SW                                 ECS028
00898       ELSE                                                        ECS028
00899          MOVE 'N' TO GRAPH-PRT-SW.                                ECS028
00900                                                                   ECS028
00901      SET PSC12 TO +2.                                             ECS028
00902      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00903      MOVE MONTH-N (INDX)        TO DT9-QTR-1A.                    ECS028
00904                                                                   ECS028
00905      SET PSC12 TO +4.                                             ECS028
00906      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00907      MOVE MONTH-N (INDX)        TO DT9-QTR-1B.                    ECS028
00908                                                                   ECS028
00909      SET PSC12 TO +5.                                             ECS028
00910      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00911      MOVE MONTH-N (INDX)        TO DT9-QTR-2A.                    ECS028
00912                                                                   ECS028
00913      SET PSC12 TO +7.                                             ECS028
00914      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00915      MOVE MONTH-N (INDX)        TO DT9-QTR-2B.                    ECS028
00916                                                                   ECS028
00917      SET PSC12 TO +8.                                             ECS028
00918      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00919      MOVE MONTH-N (INDX)        TO DT9-QTR-3A.                    ECS028
00920                                                                   ECS028
00921      SET PSC12 TO +10.                                            ECS028
00922      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00923      MOVE MONTH-N (INDX)        TO DT9-QTR-3B.                    ECS028
00924                                                                   ECS028
00925      SET PSC12 TO +11.                                            ECS028
00926      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00927      MOVE MONTH-N (INDX)        TO DT9-QTR-4A.                    ECS028
00928                                                                   ECS028
00929      SET PSC12 TO +13.                                            ECS028
00930      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
00931      MOVE MONTH-N (INDX)        TO DT9-QTR-4B.                    ECS028
00932                                                                   ECS028
00933      MOVE SPACES          TO PRT-DTL-10.                          ECS028
00934      SET P10 TO +1.                                               ECS028
00935      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00936      MOVE '  1'           TO DT10-LABLE    (P10).                 ECS028
00937      SET P10 TO +2.                                               ECS028
00938      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00939      MOVE '  2'           TO DT10-LABLE    (P10).                 ECS028
00940      SET P10 TO +3.                                               ECS028
00941      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00942      MOVE '  3'           TO DT10-LABLE    (P10).                 ECS028
00943      SET P10 TO +4.                                               ECS028
00944      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00945      MOVE '  4'           TO DT10-LABLE    (P10).                 ECS028
00946      SET P10 TO +5.                                               ECS028
00947      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00948      MOVE ' L12'          TO DT10-LABLE    (P10).                 ECS028
00949      SET P10 TO +6.                                               ECS028
00950      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00951      MOVE ' YTD'          TO DT10-LABLE    (P10).                 ECS028
00952      SET P10 TO +7.                                               ECS028
00953      MOVE SPACE           TO DT10-PRT-CNTL (P10).                 ECS028
00954      MOVE ' ITD'          TO DT10-LABLE    (P10).                 ECS028
00955      MOVE SPACES          TO PRT-DTL-11.                          ECS028
00956      SET P11 TO +1.                                               ECS028
00957      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00958      MOVE ' 1'            TO DT11-LABLE    (P11).                 ECS028
00959      SET P11 TO +2.                                               ECS028
00960      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00961      MOVE ' 2'            TO DT11-LABLE    (P11).                 ECS028
00962      SET P11 TO +3.                                               ECS028
00963      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00964      MOVE ' 3'            TO DT11-LABLE    (P11).                 ECS028
00965      SET P11 TO +4.                                               ECS028
00966      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00967      MOVE ' 4'            TO DT11-LABLE    (P11).                 ECS028
00968      SET P11 TO +5.                                               ECS028
00969      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00970      MOVE 'L12'           TO DT11-LABLE    (P11).                 ECS028
00971      SET P11 TO +6.                                               ECS028
00972      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00973      MOVE 'YTD'           TO DT11-LABLE    (P11).                 ECS028
00974      SET P11 TO +7.                                               ECS028
00975      MOVE SPACE           TO DT11-PRT-CNTL (P11).                 ECS028
00976      MOVE 'ITD'           TO DT11-LABLE    (P11).                 ECS028
00977                                                                   ECS028
00978      MOVE SPACES             TO PRT-DTL-12.                       ECS028
00979      SET P12 TO +1.                                               ECS028
00980      MOVE SPACE              TO DT12-PRT-CNTL (P12).              ECS028
00981      MOVE LIFE-OVERRIDE-L6   TO DT12-LABLE (P12).                 ECS028
00982      SET P12 TO +2.                                               ECS028
00983      MOVE SPACE              TO  DT12-PRT-CNTL (P12).             ECS028
00984      MOVE AH-OVERRIDE-L6     TO  DT12-LABLE   (P12).              ECS028
00985      SET P12 TO +3.                                               ECS028
00986      MOVE SPACE              TO DT12-PRT-CNTL (P12).              ECS028
00987      MOVE 'TOTAL'            TO DT12-LABLE    (P12).              ECS028
00988                                                                   ECS028
00989      MOVE PSC-AGE-1-HIGH TO DT14-L-AGE-1-HIGH DT14-A-AGE-1-HIGH.  ECS028
00990      MOVE PSC-AGE-2-LOW  TO DT14-L-AGE-2-LOW  DT14-A-AGE-2-LOW.   ECS028
00991      MOVE PSC-AGE-2-HIGH TO DT14-L-AGE-2-HIGH DT14-A-AGE-2-HIGH.  ECS028
00992      MOVE PSC-AGE-3-LOW  TO DT14-L-AGE-3-LOW  DT14-A-AGE-3-LOW.   ECS028
00993      MOVE PSC-AGE-3-HIGH TO DT14-L-AGE-3-HIGH DT14-A-AGE-3-HIGH.  ECS028
00994      MOVE PSC-AGE-4-LOW  TO DT14-L-AGE-4-LOW  DT14-A-AGE-4-LOW.   ECS028
00995      MOVE PSC-AGE-4-HIGH TO DT14-L-AGE-4-HIGH DT14-A-AGE-4-HIGH.  ECS028
00996      MOVE PSC-AGE-5-LOW  TO DT14-L-AGE-5-LOW  DT14-A-AGE-5-LOW.   ECS028
00997                                                                   ECS028
00998      MOVE PSC-TERM-1-HIGH TO LABLE1-TERM-HIGH.                    ECS028
00999      MOVE PSC-TERM-2-LOW  TO LABLE2-TERM-LOW.                     ECS028
01000      MOVE PSC-TERM-2-HIGH TO LABLE2-TERM-HIGH.                    ECS028
01001      MOVE PSC-TERM-3-LOW  TO LABLE3-TERM-LOW.                     ECS028
01002      MOVE PSC-TERM-3-HIGH TO LABLE3-TERM-HIGH.                    ECS028
01003      MOVE PSC-TERM-4-LOW  TO LABLE4-TERM-LOW.                     ECS028
01004      MOVE SPACES          TO PRT-DTL-15.                          ECS028
01005                                                                   ECS028
01006      SET P15 TO    +1.                                            ECS028
01007      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01008      MOVE LABLE1-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01009      SET P15 UP BY +1.                                            ECS028
01010      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01011      MOVE LABLE2-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01012      SET P15 UP BY +1.                                            ECS028
01013      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01014      MOVE LABLE3-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01015      SET P15 UP BY +1.                                            ECS028
01016      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01017      MOVE LABLE4-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01018      SET P15 UP BY +1.                                            ECS028
01019      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01020      MOVE LABLET-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01021      SET P15 UP BY +1.                                            ECS028
01022      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01023      MOVE LABLEY-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01024      SET P15 UP BY +1.                                            ECS028
01025      MOVE SPACE           TO DT15-PRT-CNTL   (P15).               ECS028
01026      MOVE LABLEI-TERM     TO DT15-TERM-LABLE (P15).               ECS028
01027                                                                   ECS028
01028      MOVE SPACES          TO HOLD-LEAD-DETAIL                     ECS028
01029                              PRT-GRAPH-LINE-1.                    ECS028
01030                                                                   ECS028
01031      IF DTE-CLIENT = 'HER'                                        ECS028
01032          MOVE 'PREMIUM       *  ACCT & GA COMPENSATION'           ECS028
01033                           TO HD3-COMP-DESC.                       ECS028
01034                                                                   ECS028
01035      PERFORM 1400-INIT-HOLD-N-ACCUM.                              ECS028
01036                                                                   ECS028
01037      SET PRT1A PRT2 TO +1.                                        ECS028
01038      SET PSC12      TO +2.                                        ECS028
01039      MOVE SPACES TO PRT-2-MONTHS.                                 ECS028
01040      PERFORM 0110-INIT-GRAPH-LINES 12 TIMES.                      ECS028
01041                                                                   ECS028
01042      MOVE NWPC TO LGC-NWPC.                                       ECS028
01043      MOVE EPC  TO LGC-EPC.                                        ECS028
01044      MOVE CPC  TO LGC-CPC.                                        ECS028
01045                                                                   ECS028
01046      SET RPTX TO +1.                                              ECS028
01047      PERFORM 0120-PRINT-REPORT-TYPE-IDS THRU 0121-EXIT 15 TIMES.  ECS028
01048                                                                   ECS028
01049  0101-EXIT.                                                       ECS028
01050      EXIT.                                                        ECS028
01051  EJECT                                                            ECS028
01052  0110-INIT-GRAPH-LINES.                                           ECS028
01053      MOVE PSC-R12-MONTH (PSC12) TO INDX.                          ECS028
01054      MOVE MONTH-N       (INDX)  TO PRT-2-MONTH    (PRT2).         ECS028
01055      MOVE SPACE                 TO PRT-1-PRT-CNTL (PRT1A).        ECS028
01056                                                                   ECS028
01057      SET PRT2  PSC12  PRT1A UP BY +1.                             ECS028
01058                                                                   ECS028
01059  0120-PRINT-REPORT-TYPE-IDS.                                      ECS028
01060      MOVE RPT-TID-LINE (RPTX) TO PRT-DATA.                        ECS028
01061                                                                   ECS028
01062      MOVE PRT-CNTL TO LCP-ASA                                     ECS028
01063      PERFORM LCP-WRITE-POS-PRT                                    ECS028
01064          THRU LCP-WRITE-END-PRT.                                  ECS028
01065                                                                   ECS028
01066      SET RPTX UP BY +1.                                           ECS028
01067                                                                   ECS028
01068  0121-EXIT.                                                       ECS028
01069      EXIT.                                                        ECS028
01070                                                                   ECS028
01071  0200-WRAPUP.                                                     ECS028
01072      MOVE HLD-RETRO-AMT-PAID    TO AMT-1.                         ECS028
01073      MOVE SPACES                TO HOLD-LEAD-DETAIL               ECS028
01074                                    SSC-DYNAMIC-REQUESTS.          ECS028
01075      MOVE AMT-1                 TO HLD-RETRO-AMT-PAID.            ECS028
01076      MOVE ' REPORT TYPE TOTALS' TO DT1-NAME.                      ECS028
01077      MOVE SUM-ACCUMULATION-DATA TO EXTRACT-ACCUMULATION-DATA.     ECS028
01078                                                                   ECS028
01079      PERFORM 4100-PRINT-MAIN-PAGE  THRU 4101-EXIT.                ECS028
01080                                                                   ECS028
01081      PERFORM 4500-PRINT-GRAPH-PAGE THRU 4501-EXIT.                ECS028
01082                                                                   ECS028
01083      MOVE '1'    TO PRT-DATA.                                     ECS028
01084                                                                   ECS028
01085      MOVE PRT-CNTL TO LCP-ASA                                     ECS028
01086      PERFORM LCP-WRITE-POS-PRT                                    ECS028
01087          THRU LCP-WRITE-END-PRT.                                  ECS028
01088                                                                   ECS028
01089      CLOSE PROD-DTL-EXT                                           ECS028
01090            PRINTR                                                 ECS028
01091            PROD-SUM-EXT.                                          ECS028
01092                                                                   ECS028
01093      IF FICH-OPEN NOT = SPACE                                     ECS028
01094          CLOSE FICH.                                              ECS028
01095                                                                   ECS028
01096  0210-EXIT.                                                       ECS028
01097      EXIT.                                                        ECS028
01098  EJECT                                                            ECS028
01099  1000-PROCESS-DETAIL-EXTRACTS.                                    ECS028
01100      IF RECORD-READ = 'N'                                         ECS028
01101          PERFORM 1100-READ-DETAIL-EXTR THRU 1100-EXIT             ECS028
01102      ELSE                                                         ECS028
01103          MOVE PSD-SORT-KEY TO HLD-SORT-KEY                        ECS028
01104          MOVE 'N'          TO RECORD-READ.                        ECS028
01105                                                                   ECS028
01106      IF NOT DETAIL-EXTRACTS-EOF                                   ECS028
01107          IF HLD-SORT-KEY = PSD-SORT-KEY OR                        ECS028
01108             DETAIL-RECORD-COUNT = +2                              ECS028
01109              PERFORM 1300-SAVE-LEAD-DETAIL                        ECS028
01110              PERFORM 2000-ACCUMULATION-ONLY THRU 2001-EXIT        ECS028
01111              GO TO 1000-PROCESS-DETAIL-EXTRACTS.                  ECS028
01112                                                                   ECS028
01113      MOVE 'Y' TO RECORD-READ.                                     ECS028
01114                                                                   ECS028
01115      PERFORM 4100-PRINT-MAIN-PAGE THRU 4101-EXIT.                 ECS028
01116                                                                   ECS028
01117      IF GRAPH-PRT-SW = 'Y'                                        ECS028
01118          PERFORM 4500-PRINT-GRAPH-PAGE THRU 4501-EXIT.            ECS028
01119                                                                   ECS028
01120      PERFORM 1200-WRITE-SUMMARY-EXTRACT THRU 1200-EXIT.           ECS028
01121                                                                   ECS028
01122      IF PSD-STD-REQUEST-NUMBER NOT = SUM-RPT-TYPE-BASE            ECS028
01123          MOVE 'Y'                    TO TOTAL-INDIC-SW            ECS028
01124          MOVE HLD-RETRO-AMT-PAID     TO AMT-1                     ECS028
01125          MOVE SPACES                 TO HOLD-LEAD-DETAIL          ECS028
01126          MOVE SUM-RPT-TYPE-BASE      TO HLD-SK-ID                 ECS028
01127          MOVE ' REPORT TYPE TOTALS'  TO DT1-NAME                  ECS028
01128          MOVE SUM-ACCUMULATION-DATA  TO EXTRACT-ACCUMULATION-DATA ECS028
01129          MOVE AMT-1                  TO HLD-RETRO-AMT-PAID        ECS028
01130          PERFORM 4100-PRINT-MAIN-PAGE  THRU 4101-EXIT             ECS028
01131          PERFORM 4500-PRINT-GRAPH-PAGE THRU 4501-EXIT             ECS028
01132          MOVE +0                     TO PAGE-CNT                  ECS028
01133          MOVE SPACE                  TO SUM-RPT-TYPE-BASE         ECS028
01134          PERFORM 1400-INIT-HOLD-N-ACCUM                           ECS028
01135          MOVE PSD-STD-REQUEST-NUMBER TO SUM-RPT-TYPE-BASE         ECS028
01136          MOVE 'N'                    TO TOTAL-INDIC-SW            ECS028
01137          MOVE SPACES                 TO DT1-NAME                  ECS028
01138      ELSE                                                         ECS028
01139          PERFORM 1400-INIT-HOLD-N-ACCUM.                          ECS028
01140                                                                   ECS028
01141  1001-EXIT.                                                       ECS028
01142      EXIT.                                                        ECS028
01143  EJECT                                                            ECS028
01144  1100-READ-DETAIL-EXTR.                                           ECS028
01145      READ PROD-DTL-EXT INTO PROD-SUMMARY-DETAIL-EXTR AT END       ECS028
01146          MOVE 'Y' TO DETAIL-EXTRACTS-EOF-INDIC.                   ECS028
01147                                                                   ECS028
01148      IF NOT DETAIL-EXTRACTS-EOF                                   ECS028
01149          ADD +1 TO DETAIL-RECORD-COUNT.                           ECS028
01150                                                                   ECS028
01151      IF SUM-RPT-TYPE-BASE = SPACE                                 ECS028
01152          MOVE PSD-STD-REQUEST-NUMBER TO SUM-RPT-TYPE-BASE.        ECS028
01153                                                                   ECS028
01154  1100-EXIT.                                                       ECS028
01155      EXIT.                                                        ECS028
01156                                                                   ECS028
01157  1200-WRITE-SUMMARY-EXTRACT.                                      ECS028
01158      MOVE HOLD-LEAD-DETAIL          TO PSX-LEAD-DATA.             ECS028
01159      MOVE EXTRACT-ACCUMULATION-DATA TO PSX-ACCUM-DATA.            ECS028
01160                                                                   ECS028
01161      WRITE PROD-SUMMARY-EXTRACT.                                  ECS028
01162                                                                   ECS028
01163      ADD +1 TO SUMMARY-RECORD-COUNT.                              ECS028
01164                                                                   ECS028
01165  1200-EXIT.                                                       ECS028
01166      EXIT.                                                        ECS028
01167                                                                   ECS028
01168  1300-SAVE-LEAD-DETAIL.                                           ECS028
01169      MOVE PSD-SORT-KEY            TO HLD-SORT-KEY.                ECS028
01170      MOVE PSD-RPT-CD-1            TO HLD-RPT-CD-1.                ECS028
01171      MOVE PSD-CARRIER             TO HLD-CARRIER.                 ECS028
01172      MOVE PSD-GROUP               TO HLD-GROUP.                   ECS028
01173      MOVE PSD-RPT-CD-2            TO HLD-RPT-CD-2.                ECS028
01174      MOVE PSD-STATE               TO HLD-STATE.                   ECS028
01175      MOVE PSD-ACCOUNT-NUMBER      TO HLD-ACCOUNT-NUMBER.          ECS028
01176      MOVE PSD-ACCT-TERMIN-DATE    TO HLD-ACCT-TERMIN-DATE.           CL**6
01177      MOVE PSD-NAME                TO HLD-NAME.                    ECS028
01178      MOVE PSD-ACCT-STATUS         TO HLD-ACCT-STATUS.             ECS028
01179      MOVE PSD-ACCT-CONTRACT-DATE  TO HLD-ACCT-CONTRACT-DATE.      ECS028
01180      MOVE PSD-ACCT-ANNIV-DATE     TO HLD-ACCT-ANNIV-DATE.         ECS028
01181      MOVE PSD-BUS-TYPE            TO HLD-BUSINESS-TYPE.           ECS028
01182      MOVE PSD-GA                  TO HLD-GA.                      ECS028
01183      MOVE PSD-REGION              TO HLD-REGION.                  ECS028
01184      MOVE PSD-RETRO-GROUP         TO HLD-RETRO-GROUP.             ECS028
01185      MOVE PSD-RETRO-AMT-PAID      TO HLD-RETRO-AMT-PAID.          ECS028
01186      MOVE PSD-ACCT-RETEN-LIFE     TO HLD-ACCT-RETEN-LIFE.         ECS028
01187      MOVE PSD-ACCT-RETEN-AH       TO HLD-ACCT-RETEN-AH.           ECS028
01188  EJECT                                                            ECS028
01189  1400-INIT-HOLD-N-ACCUM.                                          ECS028
01190      MOVE SPACES     TO HOLD-LEAD-DETAIL.                         ECS028
01191                                                                   ECS028
01192      MOVE ZEROS      TO HLD-RETRO-AMT-PAID                        ECS028
01193                         HLD-ACCT-RETEN-LIFE                       ECS028
01194                         HLD-ACCT-RETEN-AH.                        ECS028
01195                                                                   ECS028
01196      SET EADM SUMM TO +1.                                         ECS028
01197      PERFORM 1410-INIT-MONTH-ARRAY     7 TIMES.                   ECS028
01198                                                                   ECS028
01199      SET EADT SUMT EADA SUMA TO +1.                               ECS028
01200      PERFORM 1420-INIT-TERM-AGE-ARRAY 35 TIMES.                   ECS028
01201                                                                   ECS028
01202      SET EADGP SUMGP TO +1.                                       ECS028
01203      PERFORM 1430-INIT-GRAPH-DATA     12 TIMES.                   ECS028
01204                                                                   ECS028
01205  1410-INIT-MONTH-ARRAY.                                           ECS028
01206      MOVE +0 TO EAD-ML-COUNT          (EADM)                      ECS028
01207                 EAD-ML-PREM           (EADM)                      ECS028
01208                 EAD-ML-EARN-PREM      (EADM)                      ECS028
01209                 EAD-ML-COMMIS         (EADM)                      ECS028
01210                 EAD-ML-CLAIM-INCURRED (EADM)                      ECS028
01211                 EAD-ML-CLAIMS-PAID    (EADM)                      ECS028
01212                 EAD-MA-COUNT          (EADM)                      ECS028
01213                 EAD-MA-PREM           (EADM)                      ECS028
01214                 EAD-MA-EARN-PREM      (EADM)                      ECS028
01215                 EAD-MA-COMMIS         (EADM)                      ECS028
01216                 EAD-MA-CLAIM-INCURRED (EADM)                      ECS028
01217                 EAD-MA-CLAIMS-PAID    (EADM).                     ECS028
01218                                                                   ECS028
01219      IF SUM-RPT-TYPE-BASE = SPACE                                 ECS028
01220          MOVE +0 TO SUM-ML-COUNT          (SUMM)                  ECS028
01221                     SUM-ML-PREM           (SUMM)                  ECS028
01222                     SUM-ML-EARN-PREM      (SUMM)                  ECS028
01223                     SUM-ML-COMMIS         (SUMM)                  ECS028
01224                     SUM-ML-CLAIM-INCURRED (SUMM)                  ECS028
01225                     SUM-ML-CLAIMS-PAID    (SUMM)                  ECS028
01226                     SUM-MA-COUNT          (SUMM)                  ECS028
01227                     SUM-MA-PREM           (SUMM)                  ECS028
01228                     SUM-MA-EARN-PREM      (SUMM)                  ECS028
01229                     SUM-MA-COMMIS         (SUMM)                  ECS028
01230                     SUM-MA-CLAIM-INCURRED (SUMM)                  ECS028
01231                     SUM-MA-CLAIMS-PAID    (SUMM).                 ECS028
01232                                                                   ECS028
01233      SET EADM SUMM UP BY +1.                                      ECS028
01234  EJECT                                                            ECS028
01235  1420-INIT-TERM-AGE-ARRAY.                                        ECS028
01236      IF EADA GREATER +5                                           ECS028
01237          SET EADA SUMA TO +1                                      ECS028
01238          SET EADT SUMT UP BY +1.                                  ECS028
01239                                                                   ECS028
01240      IF EADA = +1                                                 ECS028
01241          MOVE +0 TO EAD-L-WTD-PREM       (EADT)                   ECS028
01242                     EAD-L-AGE-COUNT-PREM (EADT)                   ECS028
01243                     EAD-L-AGE-SUM-PREM   (EADT)                   ECS028
01244                     EAD-L-WTD-CLAIMS     (EADT)                   ECS028
01245                     EAD-L-AGE-COUNT-CLMS (EADT)                   ECS028
01246                     EAD-L-AGE-SUM-CLMS   (EADT)                   ECS028
01247                     EAD-A-WTD-PREM       (EADT)                   ECS028
01248                     EAD-A-AGE-COUNT-PREM (EADT)                   ECS028
01249                     EAD-A-AGE-SUM-PREM   (EADT)                   ECS028
01250                     EAD-A-WTD-CLAIMS     (EADT)                   ECS028
01251                     EAD-A-AGE-COUNT-CLMS (EADT)                   ECS028
01252                     EAD-A-AGE-SUM-CLMS   (EADT)                   ECS028
01253          IF SUM-RPT-TYPE-BASE = SPACE                             ECS028
01254              MOVE +0 TO SUM-L-WTD-PREM       (SUMT)               ECS028
01255                         SUM-L-AGE-COUNT-PREM (SUMT)               ECS028
01256                         SUM-L-AGE-SUM-PREM   (SUMT)               ECS028
01257                         SUM-L-WTD-CLAIMS     (SUMT)               ECS028
01258                         SUM-L-AGE-COUNT-CLMS (SUMT)               ECS028
01259                         SUM-L-AGE-SUM-CLMS   (SUMT)               ECS028
01260                         SUM-A-WTD-PREM       (SUMT)               ECS028
01261                         SUM-A-AGE-COUNT-PREM (SUMT)               ECS028
01262                         SUM-A-AGE-SUM-PREM   (SUMT)               ECS028
01263                         SUM-A-WTD-CLAIMS     (SUMT)               ECS028
01264                         SUM-A-AGE-COUNT-CLMS (SUMT)               ECS028
01265                         SUM-A-AGE-SUM-CLMS   (SUMT).              ECS028
01266                                                                   ECS028
01267      MOVE +0 TO EAD-L-PREM        (EADT, EADA)                    ECS028
01268                 EAD-L-CLAIMS-PAID (EADT, EADA)                    ECS028
01269                 EAD-A-PREM        (EADT, EADA)                    ECS028
01270                 EAD-A-CLAIMS-PAID (EADT, EADA).                   ECS028
01271                                                                   ECS028
01272      IF SUM-RPT-TYPE-BASE = SPACE                                 ECS028
01273          MOVE +0 TO SUM-L-PREM        (SUMT, SUMA)                ECS028
01274                     SUM-L-CLAIMS-PAID (SUMT, SUMA)                ECS028
01275                     SUM-A-PREM        (SUMT, SUMA)                ECS028
01276                     SUM-A-CLAIMS-PAID (SUMT, SUMA).               ECS028
01277                                                                   ECS028
01278      SET EADA SUMA UP BY +1.                                      ECS028
01279  EJECT                                                            ECS028
01280  1430-INIT-GRAPH-DATA.                                            ECS028
01281      MOVE +0 TO EAD-NET-WRITTEN-PREM (EADGP)                      ECS028
01282                 EAD-EARNED-PREM      (EADGP)                      ECS028
01283                 EAD-CLAIMS-PAID      (EADGP).                     ECS028
01284                                                                   ECS028
01285      IF SUM-RPT-TYPE-BASE = SPACE                                 ECS028
01286          MOVE +0 TO SUM-NET-WRITTEN-PREM (SUMGP)                  ECS028
01287                     SUM-EARNED-PREM      (SUMGP)                  ECS028
01288                     SUM-CLAIMS-PAID      (SUMGP).                 ECS028
01289                                                                   ECS028
01290      SET EADGP SUMGP UP BY +1.                                    ECS028
01291                                                                   ECS028
01292  2000-ACCUMULATION-ONLY.                                          ECS028
01293      SET PSDM EADM SUMM TO +1.                                    ECS028
01294      PERFORM 2010-MONTH-ARRAY-ACCUMS        7 TIMES.              ECS028
01295                                                                   ECS028
01296      SET PSDT EADT SUMT TO +1.                                    ECS028
01297      PERFORM 2020-TERM-ARRAY-ACCUMS         7 TIMES.              ECS028
01298                                                                   ECS028
01299      SET PSDT EADT SUMT PSDA EADA SUMA TO +1.                     ECS028
01300      PERFORM 2030-AGE-ARRAY-ACCUMS THRU 2031-EXIT 35 TIMES.       ECS028
01301                                                                   ECS028
01302      SET PSDGP EADGP SUMGP TO +1.                                 ECS028
01303      PERFORM 2040-GRAPH-ACCUMS 12 TIMES.                          ECS028
01304                                                                   ECS028
01305  2001-EXIT.                                                       ECS028
01306      EXIT.                                                        ECS028
01307  EJECT                                                            ECS028
01308  2010-MONTH-ARRAY-ACCUMS.                                         ECS028
01309      ADD PSD-ML-COUNT     (PSDM) TO EAD-ML-COUNT     (EADM).      ECS028
01310      ADD PSD-ML-PREM      (PSDM) TO EAD-ML-PREM      (EADM).      ECS028
01311      ADD PSD-ML-EARN-PREM (PSDM) TO EAD-ML-EARN-PREM (EADM).      ECS028
01312      ADD PSD-ML-COMMIS    (PSDM) TO EAD-ML-COMMIS    (EADM).      ECS028
01313      ADD PSD-ML-CLAIM-INCURRED (PSDM)                             ECS028
01314                                  TO EAD-ML-CLAIM-INCURRED         ECS028
01315                                                      (EADM).      ECS028
01316      ADD PSD-ML-CLAIMS-PAID (PSDM)                                ECS028
01317                                  TO EAD-ML-CLAIMS-PAID            ECS028
01318                                                      (EADM).      ECS028
01319                                                                   ECS028
01320      ADD PSD-MA-COUNT     (PSDM) TO EAD-MA-COUNT     (EADM).      ECS028
01321      ADD PSD-MA-PREM      (PSDM) TO EAD-MA-PREM      (EADM).      ECS028
01322      ADD PSD-MA-EARN-PREM (PSDM) TO EAD-MA-EARN-PREM (EADM).      ECS028
01323      ADD PSD-MA-COMMIS    (PSDM) TO EAD-MA-COMMIS    (EADM).      ECS028
01324      ADD PSD-MA-CLAIM-INCURRED (PSDM)                             ECS028
01325                                  TO EAD-MA-CLAIM-INCURRED         ECS028
01326                                                      (EADM).      ECS028
01327      ADD PSD-MA-CLAIMS-PAID (PSDM)                                ECS028
01328                                  TO EAD-MA-CLAIMS-PAID            ECS028
01329                                                      (EADM).      ECS028
01330                                                                   ECS028
01331      IF SUM-RPT-TYPE-BASE = PSD-STD-REQUEST-NUMBER                ECS028
01332          ADD PSD-ML-COUNT     (PSDM) TO SUM-ML-COUNT     (SUMM)   ECS028
01333          ADD PSD-ML-PREM      (PSDM) TO SUM-ML-PREM      (SUMM)   ECS028
01334          ADD PSD-ML-EARN-PREM (PSDM) TO SUM-ML-EARN-PREM (SUMM)   ECS028
01335          ADD PSD-ML-COMMIS    (PSDM) TO SUM-ML-COMMIS    (SUMM)   ECS028
01336          ADD PSD-ML-CLAIM-INCURRED (PSDM)                         ECS028
01337                                      TO SUM-ML-CLAIM-INCURRED     ECS028
01338                                                          (SUMM)   ECS028
01339          ADD PSD-ML-CLAIMS-PAID (PSDM)                            ECS028
01340                                      TO SUM-ML-CLAIMS-PAID        ECS028
01341                                                          (SUMM)   ECS028
01342          ADD PSD-MA-COUNT     (PSDM) TO SUM-MA-COUNT     (SUMM)   ECS028
01343          ADD PSD-MA-PREM      (PSDM) TO SUM-MA-PREM      (SUMM)   ECS028
01344          ADD PSD-MA-EARN-PREM (PSDM) TO SUM-MA-EARN-PREM (SUMM)   ECS028
01345          ADD PSD-MA-COMMIS    (PSDM) TO SUM-MA-COMMIS    (SUMM)   ECS028
01346          ADD PSD-MA-CLAIM-INCURRED (PSDM)                         ECS028
01347                                      TO SUM-MA-CLAIM-INCURRED     ECS028
01348                                                          (SUMM)   ECS028
01349          ADD PSD-MA-CLAIMS-PAID (PSDM)                            ECS028
01350                                      TO SUM-MA-CLAIMS-PAID        ECS028
01351                                                          (SUMM).  ECS028
01352                                                                   ECS028
01353      SET PSDM EADM SUMM UP BY +1.                                 ECS028
01354  EJECT                                                            ECS028
01355  2020-TERM-ARRAY-ACCUMS.                                          ECS028
01356      ADD PSD-L-WTD-PREM (PSDT)     TO EAD-L-WTD-PREM     (EADT).  ECS028
01357      ADD PSD-L-AGE-COUNT-PREM (PSDT)                              ECS028
01358                                    TO EAD-L-AGE-COUNT-PREM        ECS028
01359                                                          (EADT).  ECS028
01360      ADD PSD-L-AGE-SUM-PREM (PSDT) TO EAD-L-AGE-SUM-PREM (EADT).  ECS028
01361      ADD PSD-L-WTD-CLAIMS   (PSDT) TO EAD-L-WTD-CLAIMS   (EADT).  ECS028
01362      ADD PSD-L-AGE-COUNT-CLMS (PSDT)                              ECS028
01363                                    TO EAD-L-AGE-COUNT-CLMS        ECS028
01364                                                          (EADT).  ECS028
01365      ADD PSD-L-AGE-SUM-CLMS (PSDT) TO EAD-L-AGE-SUM-CLMS (EADT).  ECS028
01366                                                                   ECS028
01367      ADD PSD-A-WTD-PREM     (PSDT) TO EAD-A-WTD-PREM     (EADT).  ECS028
01368      ADD PSD-A-AGE-COUNT-PREM (PSDT)                              ECS028
01369                                    TO EAD-A-AGE-COUNT-PREM        ECS028
01370                                                          (EADT).  ECS028
01371      ADD PSD-A-AGE-SUM-PREM (PSDT) TO EAD-A-AGE-SUM-PREM (EADT).  ECS028
01372      ADD PSD-A-WTD-CLAIMS   (PSDT) TO EAD-A-WTD-CLAIMS   (EADT).  ECS028
01373      ADD PSD-A-AGE-COUNT-CLMS (PSDT)                              ECS028
01374                                    TO EAD-A-AGE-COUNT-CLMS        ECS028
01375                                                          (EADT).  ECS028
01376      ADD PSD-A-AGE-SUM-CLMS (PSDT) TO EAD-A-AGE-SUM-CLMS (EADT).  ECS028
01377                                                                   ECS028
01378      IF SUM-RPT-TYPE-BASE = PSD-STD-REQUEST-NUMBER                ECS028
01379          ADD PSD-L-WTD-PREM       (PSDT)                          ECS028
01380                      TO SUM-L-WTD-PREM       (SUMT)               ECS028
01381          ADD PSD-L-AGE-COUNT-PREM (PSDT)                          ECS028
01382                      TO SUM-L-AGE-COUNT-PREM (SUMT)               ECS028
01383          ADD PSD-L-AGE-SUM-PREM   (PSDT)                          ECS028
01384                      TO SUM-L-AGE-SUM-PREM   (SUMT)               ECS028
01385          ADD PSD-L-WTD-CLAIMS     (PSDT)                          ECS028
01386                      TO SUM-L-WTD-CLAIMS     (SUMT)               ECS028
01387          ADD PSD-L-AGE-COUNT-CLMS (PSDT)                          ECS028
01388                      TO SUM-L-AGE-COUNT-CLMS (SUMT)               ECS028
01389          ADD PSD-L-AGE-SUM-CLMS   (PSDT)                          ECS028
01390                      TO SUM-L-AGE-SUM-CLMS   (SUMT)               ECS028
01391          ADD PSD-A-WTD-PREM       (PSDT)                          ECS028
01392                      TO SUM-A-WTD-PREM       (SUMT)               ECS028
01393          ADD PSD-A-AGE-COUNT-PREM (PSDT)                          ECS028
01394                      TO SUM-A-AGE-COUNT-PREM (SUMT)               ECS028
01395          ADD PSD-A-AGE-SUM-PREM   (PSDT)                          ECS028
01396                      TO SUM-A-AGE-SUM-PREM   (SUMT)               ECS028
01397          ADD PSD-A-WTD-CLAIMS     (PSDT)                          ECS028
01398                      TO SUM-A-WTD-CLAIMS     (SUMT)               ECS028
01399          ADD PSD-A-AGE-COUNT-CLMS (PSDT)                          ECS028
01400                      TO SUM-A-AGE-COUNT-CLMS (SUMT)               ECS028
01401          ADD PSD-A-AGE-SUM-CLMS   (PSDT)                          ECS028
01402                      TO SUM-A-AGE-SUM-CLMS   (SUMT).              ECS028
01403                                                                   ECS028
01404      SET PSDT EADT SUMT UP BY +1.                                 ECS028
01405  EJECT                                                            ECS028
01406  2030-AGE-ARRAY-ACCUMS.                                           ECS028
01407      ADD PSD-L-PREM (PSDT, PSDA) TO EAD-L-PREM (EADT, EADA).      ECS028
01408      ADD PSD-L-CLAIMS-PAID (PSDT, PSDA)                           ECS028
01409                      TO EAD-L-CLAIMS-PAID (EADT, EADA).           ECS028
01410      ADD PSD-A-PREM (PSDT, PSDA) TO EAD-A-PREM (EADT, EADA).      ECS028
01411      ADD PSD-A-CLAIMS-PAID (PSDT, PSDA)                           ECS028
01412                      TO EAD-A-CLAIMS-PAID (EADT, EADA).           ECS028
01413                                                                   ECS028
01414      IF SUM-RPT-TYPE-BASE = PSD-STD-REQUEST-NUMBER                ECS028
01415          ADD PSD-L-PREM (PSDT, PSDA) TO SUM-L-PREM (SUMT, SUMA)   ECS028
01416          ADD PSD-L-CLAIMS-PAID (PSDT, PSDA)                       ECS028
01417                          TO SUM-L-CLAIMS-PAID (SUMT, SUMA)        ECS028
01418          ADD PSD-A-PREM (PSDT, PSDA) TO SUM-A-PREM (SUMT, SUMA)   ECS028
01419          ADD PSD-A-CLAIMS-PAID (PSDT, PSDA)                       ECS028
01420                          TO SUM-A-CLAIMS-PAID (SUMT, SUMA).       ECS028
01421                                                                   ECS028
01422      SET PSDA EADA SUMA UP BY +1.                                 ECS028
01423                                                                   ECS028
01424      IF PSDA GREATER +5                                           ECS028
01425          SET PSDT EADT SUMT UP BY +1                              ECS028
01426          SET PSDA EADA SUMA TO +1.                                ECS028
01427                                                                   ECS028
01428  2031-EXIT.                                                       ECS028
01429      EXIT.                                                        ECS028
01430                                                                   ECS028
01431  2040-GRAPH-ACCUMS.                                               ECS028
01432      ADD PSD-NET-WRITTEN-PREM (PSDGP)                             ECS028
01433                      TO EAD-NET-WRITTEN-PREM (EADGP).             ECS028
01434      ADD PSD-EARNED-PREM      (PSDGP)                             ECS028
01435                      TO EAD-EARNED-PREM      (EADGP).             ECS028
01436      ADD PSD-CLAIMS-PAID      (PSDGP)                             ECS028
01437                      TO EAD-CLAIMS-PAID      (EADGP).             ECS028
01438                                                                   ECS028
01439      IF SUM-RPT-TYPE-BASE = PSD-STD-REQUEST-NUMBER                ECS028
01440          ADD PSD-NET-WRITTEN-PREM (PSDGP)                         ECS028
01441                          TO SUM-NET-WRITTEN-PREM (SUMGP)          ECS028
01442          ADD PSD-EARNED-PREM      (PSDGP)                         ECS028
01443                          TO SUM-EARNED-PREM      (SUMGP)          ECS028
01444          ADD PSD-CLAIMS-PAID      (PSDGP)                         ECS028
01445                          TO SUM-CLAIMS-PAID      (SUMGP).         ECS028
01446                                                                   ECS028
01447      SET PSDGP EADGP SUMGP UP BY +1.                              ECS028
01448  EJECT                                                            ECS028
01449  4100-PRINT-MAIN-PAGE.                                            ECS028
01450      PERFORM 4200-SET-UP-PRINT-DATA THRU 4201-EXIT.               ECS028
01451                                                                   ECS028
01452      ADD +1           TO PAGE-CNT.                                ECS028
01453      MOVE PAGE-CNT    TO HD2A-PAGE.                               ECS028
01454                                                                   ECS028
01455      MOVE PRT-HDR-1   TO PRT-DATA.                                ECS028
01456      PERFORM 4900-PRINT-LINE.                                     ECS028
01457      MOVE PRT-HDR-2   TO PRT-DATA.                                ECS028
01458      PERFORM 4900-PRINT-LINE.                                     ECS028
01459      MOVE PRT-HDR-2A  TO PRT-DATA.                                ECS028
01460      PERFORM 4900-PRINT-LINE.                                     ECS028
01461      MOVE PRT-DTL-1   TO PRT-DATA.                                ECS028
01462      PERFORM 4900-PRINT-LINE.                                     ECS028
01463      MOVE PRT-DTL-2   TO PRT-DATA.                                ECS028
01464      PERFORM 4900-PRINT-LINE.                                     ECS028
01465      MOVE PRT-DTL-3   TO PRT-DATA.                                ECS028
01466      PERFORM 4900-PRINT-LINE.                                     ECS028
01467      MOVE PRT-DTL-4   TO PRT-DATA.                                ECS028
01468      PERFORM 4900-PRINT-LINE.                                     ECS028
01469      MOVE PRT-DTL-4A  TO PRT-DATA.                                ECS028
01470      PERFORM 4900-PRINT-LINE.                                     ECS028
01471      MOVE LIFE-OVERRIDE-L6   TO  DT5-RETEN-HDR.                   ECS028
01472      MOVE PRT-DTL-5   TO PRT-DATA.                                ECS028
01473      PERFORM 4900-PRINT-LINE.                                     ECS028
01474      MOVE AH-OVERRIDE-L6     TO  DT6-RETEN-HDR.                   ECS028
01475      MOVE PRT-DTL-6   TO PRT-DATA.                                ECS028
01476      PERFORM 4900-PRINT-LINE.                                     ECS028
01477      MOVE PRT-DTL-7   TO PRT-DATA.                                ECS028
01478      PERFORM 4900-PRINT-LINE.                                     ECS028
01479      MOVE PRT-DTL-8   TO PRT-DATA.                                ECS028
01480      PERFORM 4900-PRINT-LINE.                                     ECS028
01481      MOVE PRT-DTL-9   TO PRT-DATA.                                ECS028
01482      PERFORM 4900-PRINT-LINE.                                     ECS028
01483      MOVE PRT-HDR-3   TO PRT-DATA.                                ECS028
01484      PERFORM 4900-PRINT-LINE.                                     ECS028
01485      MOVE PRT-HDR-4   TO PRT-DATA.                                ECS028
01486      PERFORM 4900-PRINT-LINE.                                     ECS028
01487                                                                   ECS028
01488      SET P10 TO +1.                                               ECS028
01489      PERFORM 4300-PRT-LINE-10 7 TIMES.                            ECS028
01490                                                                   ECS028
01491      MOVE PRT-HDR-5   TO PRT-DATA.                                ECS028
01492      PERFORM 4900-PRINT-LINE.                                     ECS028
01493      MOVE PRT-HDR-6   TO PRT-DATA.                                ECS028
01494      PERFORM 4900-PRINT-LINE.                                     ECS028
01495                                                                   ECS028
01496      SET P11 TO +1.                                               ECS028
01497      PERFORM 4310-PRT-LINE-11 7 TIMES.                            ECS028
01498                                                                   ECS028
01499      MOVE PRT-HDR-7   TO PRT-DATA.                                ECS028
01500      PERFORM 4900-PRINT-LINE.                                     ECS028
01501      MOVE PRT-HDR-8   TO PRT-DATA.                                ECS028
01502      PERFORM 4900-PRINT-LINE.                                     ECS028
01503                                                                   ECS028
01504      SET P12 TO +1.                                               ECS028
01505      PERFORM 4320-PRT-LINE-12 3 TIMES.                            ECS028
01506                                                                   ECS028
01507      MOVE PRT-HDR-9A  TO PRT-DATA.                                ECS028
01508      PERFORM 4900-PRINT-LINE.                                     ECS028
01509      MOVE PRT-HDR-10  TO PRT-DATA.                                ECS028
01510      PERFORM 4900-PRINT-LINE.                                     ECS028
01511      MOVE PRT-DTL-14  TO PRT-DATA.                                ECS028
01512      PERFORM 4900-PRINT-LINE.                                     ECS028
01513      MOVE PRT-HDR-11  TO PRT-DATA.                                ECS028
01514      PERFORM 4900-PRINT-LINE.                                     ECS028
01515                                                                   ECS028
01516      SET P15 TO +1.                                               ECS028
01517      PERFORM 4330-PRT-LINE-15 7 TIMES.                            ECS028
01518                                                                   ECS028
01519      MOVE PRT-HDR-9B  TO PRT-DATA.                                ECS028
01520      PERFORM 4900-PRINT-LINE.                                     ECS028
01521      MOVE PRT-HDR-10  TO PRT-DATA.                                ECS028
01522      PERFORM 4900-PRINT-LINE.                                     ECS028
01523      MOVE PRT-DTL-14  TO PRT-DATA.                                ECS028
01524      PERFORM 4900-PRINT-LINE.                                     ECS028
01525      MOVE PRT-HDR-11  TO PRT-DATA.                                ECS028
01526      PERFORM 4900-PRINT-LINE.                                     ECS028
01527                                                                   ECS028
01528      SET EADT P15 TO +1.                                          ECS028
01529                                                                   ECS028
01530      PERFORM 4400-LOAD-CLAIM-DISTRIBUTION THRU 4400-EXIT 7 TIMES. ECS028
01531                                                                   ECS028
01532      SET P15 TO +1.                                               ECS028
01533      PERFORM 4330-PRT-LINE-15 7 TIMES.                            ECS028
01534                                                                   ECS028
01535  4101-EXIT.                                                       ECS028
01536      EXIT.                                                        ECS028
01537  EJECT                                                            ECS028
01538  4200-SET-UP-PRINT-DATA.                                          ECS028
01539      MOVE HLD-SK-ID                  TO HD1-REPORT-TYPE           ECS028
01540                                         GH1-REPORT-TYPE.          ECS028
01541      MOVE COMPANY-NAME               TO HD2-CO-NAME.              ECS028
01542      MOVE WS-CURRENT-DATE            TO HD2-RUN-DATE.             ECS028
01543      MOVE ALPH-DATE                  TO HD2A-ALPHA-DATE.          ECS028
01544      MOVE LIFE-OVERRIDE-L6           TO HD4-LF-1  HD4-LF-2        ECS028
01545                                         HD4-LF-3  HD6-LF-1        ECS028
01546                                         HD6-LF-2  HD6-LF-3.       ECS028
01547      MOVE AH-OVERRIDE-L6             TO HD4-AH-1  HD4-AH-2        ECS028
01548                                         HD4-AH-3  HD6-AH-1        ECS028
01549                                         HD6-AH-2  HD6-AH-3.       ECS028
01550                                                                   ECS028
01551      IF TOTAL-INDIC-SW = 'Y'                                      ECS028
01552          MOVE SPACES                TO DT2-CARRIER                ECS028
01553                                        DT2-CARR-NAME              ECS028
01554                                        DT3-GROUP                  ECS028
01555                                        DT3-REGION                 ECS028
01556                                        DT4-STATE                  ECS028
01557                                        DT4-STATE-DESC             ECS028
01558                                        DT4A-RPT-CD-1              ECS028
01559                                        DT4A-RPT-CD-2              ECS028
01560                                        DT5-ACCT-NUMBER            ECS028
01561                                        DT8-TERMIN-DATE            ECS028
01562                                        DT7-STATUS                 ECS028
01563                                        DT7-CNTR-DATE              ECS028
01564                                        DT8-ANNV-DATE              ECS028
01565                                        DT6-BUS-TYPE-DESC          ECS028
01566                                        DT6-BUS-TYPE               ECS028
01567                                        DT4-GA                     ECS028
01568                                        DT1-RETRO-GRP              ECS028
01569          MOVE ZEROS                 TO DT2-RETRO-AMT              ECS028
01570                                        DT5-RETEN-LIFE             ECS028
01571                                        DT6-RETEN-AH               ECS028
01572          GO TO 4201-SET-UP-CONT.                                  ECS028
01573                                                                   ECS028
01574      MOVE SPACES                     TO DT2-CARRIER.              ECS028
01575                                                                   ECS028
01576      IF HLD-ACCOUNT-REQ  OR                                       ECS028
01577         HLD-STATE-REQ    OR                                       ECS028
01578         HLD-GROUP-REQ    OR                                       ECS028
01579         HLD-CARRIER-REQ  OR                                       ECS028
01580         HLD-RPT-CD-2-REQ OR                                       ECS028
01581         (SSC-CARRIER AND HLD-DYNAMIC-REQUEST)                     ECS028
01582          MOVE HLD-CARRIER            TO DT2-CARRIER.              ECS028
01583                                                                   ECS028
01584      MOVE SPACES                     TO DT2-CARR-NAME.            ECS028
01585                                                                   ECS028
01586      IF HLD-ACCOUNT-REQ  OR                                       ECS028
01587         HLD-STATE-REQ    OR                                       ECS028
01588         HLD-GROUP-REQ    OR                                       ECS028
01589         HLD-CARRIER-REQ  OR                                       ECS028
01590         HLD-RPT-CD-2-REQ OR                                       ECS028
01591         (SSC-CARRIER AND HLD-DYNAMIC-REQUEST)                     ECS028
01592          PERFORM 4205-EMPTY-LOOP                                  ECS028
01593              VARYING CLAS-INDEXCN FROM +1 BY +1                   ECS028
01594              UNTIL HLD-CARRIER = CARRIER-SUB (CLAS-INDEXCN)       ECS028
01595              OR CLAS-INDEXCN GREATER CLAS-MAXCN                   ECS028
01596          IF HLD-CARRIER = CARRIER-SUB      (CLAS-INDEXCN)         ECS028
01597              MOVE CARRIER-PIC (CLAS-INDEXCN) TO DT2-CARR-NAME     ECS028
01598          ELSE                                                     ECS028
01599              MOVE SPACES                     TO DT2-CARR-NAME.    ECS028
01600                                                                   ECS028
01601      MOVE SPACES                     TO DT3-GROUP.                ECS028
01602                                                                   ECS028
01603      IF HLD-ACCOUNT-REQ  OR                                       ECS028
01604         HLD-STATE-REQ    OR                                       ECS028
01605         HLD-GROUP-REQ    OR                                       ECS028
01606         HLD-RPT-CD-2-REQ OR                                       ECS028
01607         (SSC-GROUP AND HLD-DYNAMIC-REQUEST)                       ECS028
01608          MOVE HLD-GROUP              TO DT3-GROUP.                ECS028
01609                                                                   ECS028
01610      MOVE SPACES                     TO DT3-REGION.               ECS028
01611                                                                   ECS028
01612      IF HLD-ACCOUNT-REQ OR                                        ECS028
01613         HLD-REGION-REQ  OR                                        ECS028
01614         (SSC-REGION AND HLD-DYNAMIC-REQUEST)                      ECS028
01615          MOVE HLD-REGION             TO DT3-REGION.               ECS028
01616                                                                   ECS028
01617      MOVE SPACES                     TO DT4-STATE.                ECS028
01618                                                                   ECS028
01619      IF HLD-ACCOUNT-REQ OR                                        ECS028
01620         SSC-ACCOUNT     OR                                        ECS028
01621         HLD-STATE-REQ   OR                                        ECS028
01622         (SSC-STATE AND HLD-DYNAMIC-REQUEST)                       ECS028
01623          MOVE HLD-STATE              TO DT4-STATE.                ECS028
01624                                                                   ECS028
01625      MOVE SPACES                     TO DT4-STATE-DESC.           ECS028
01626                                                                   ECS028
01627      IF HLD-ACCOUNT-REQ OR                                        ECS028
01628         SSC-ACCOUNT     OR                                        ECS028
01629         HLD-STATE-REQ   OR                                        ECS028
01630         (SSC-STATE AND HLD-DYNAMIC-REQUEST)                       ECS028
01631          PERFORM 4205-EMPTY-LOOP                                  ECS028
01632              VARYING CLAS-INDEXS FROM +1 BY +1                    ECS028
01633              UNTIL HLD-STATE = (STATE-SUB (CLAS-INDEXS)  OR       ECS028
01634                                STATE-ABBR (CLAS-INDEXS))          ECS028
01635              OR CLAS-INDEXS GREATER CLAS-MAXS                     ECS028
01636          IF HLD-STATE = (STATE-SUB (CLAS-INDEXS) OR               ECS028
01637                         STATE-ABBR (CLAS-INDEXS))                 ECS028
01638              MOVE STATE-PIC (CLAS-INDEXS) TO DT4-STATE-DESC.      ECS028
01639                                                                   ECS028
01640  EJECT                                                            ECS028
01641      MOVE SPACES                     TO DT4A-RPT-CD-1.            ECS028
01642      IF HLD-RPT-CD-1-REQ OR                                       ECS028
01643          (SSC-RPT-CD-1 AND HLD-DYNAMIC-REQUEST)                   ECS028
01644              MOVE HLD-RPT-CD-1       TO DT4A-RPT-CD-1.            ECS028
01645                                                                   ECS028
01646      MOVE SPACES                     TO DT4A-RPT-CD-2.            ECS028
01647      IF HLD-RPT-CD-2-REQ OR                                       ECS028
01648          (SSC-RPT-CD-2 AND HLD-DYNAMIC-REQUEST)                   ECS028
01649              MOVE HLD-RPT-CD-2       TO DT4A-RPT-CD-2.            ECS028
01650                                                                   ECS028
01651      IF HLD-SK-ID NOT = SPACE                                     ECS028
01652          MOVE SPACES                 TO DT1-NAME.                 ECS028
01653                                                                   ECS028
01654      MOVE SPACES                     TO DT5-ACCT-NUMBER           ECS028
01655                                         DT8-TERMIN-DATE.          ECS028
01656                                                                   ECS028
01657      IF HLD-ACCOUNT-REQ OR                                        ECS028
01658         (HLD-GA-ACCT-REQ AND HLD-ACCOUNT-NUMBER NOT = SPACE) OR   ECS028
01659         (SSC-ACCOUNT AND HLD-DYNAMIC-REQUEST)                     ECS028
01660          MOVE HLD-NAME                TO DT1-NAME                 ECS028
01661          MOVE HLD-ACCOUNT-NUMBER      TO DT5-ACCT-NUMBER          ECS028
01662          MOVE HLD-ACCT-TERMIN-DATE    TO DATE-WORK-CYMD              CL**9
01663          MOVE DATE-YR                 TO PD-YR                       CL**2
01664          MOVE DATE-MO                 TO PD-MO                       CL**2
01665          MOVE DATE-DA                 TO PD-DAY                      CL**6
01666          MOVE PRT-DATE                TO DT8-TERMIN-DATE          ECS028
01667          IF HLD-ACCT-TERMIN-DATE = NINES-DATE                        CL**6
01668              MOVE CURRENT-LABEL       TO DT8-TERMIN-DATE.         ECS028
01669                                                                   ECS028
01670      MOVE SPACES             TO DT7-STATUS.                       ECS028
01671                                                                   ECS028
01672      IF (HLD-ACCOUNT-REQ OR SSC-ACCOUNT)                          ECS028
01673          IF HLD-ACCT-STATUS = 0                                   ECS028
01674              MOVE 'ACTIVE'   TO DT7-STATUS                        ECS028
01675          ELSE                                                     ECS028
01676              MOVE 'INACTIVE' TO DT7-STATUS.                       ECS028
01677                                                                   ECS028
01678      MOVE SPACES                     TO DT7-CNTR-DATE             ECS028
01679                                         DT8-ANNV-DATE.            ECS028
01680      IF HLD-ACCOUNT-REQ OR                                        ECS028
01681         (HLD-GA-ACCT-REQ AND HLD-ACCOUNT-NUMBER NOT = SPACE) OR   ECS028
01682         (SSC-ACCOUNT AND HLD-DYNAMIC-REQUEST)                     ECS028
01683          MOVE HLD-ACCT-CONTRACT-DATE TO DATE-WORK-CYMD               CL**9
01684          MOVE DATE-YR                TO PD-YR                        CL**2
01685          MOVE DATE-MO                TO PD-MO                        CL**2
01686          MOVE DATE-DA                TO PD-DAY                       CL**2
01687          MOVE PRT-DATE               TO DT7-CNTR-DATE             ECS028
01688          MOVE HLD-ACCT-ANNIV-DATE    TO DATE-WORK-CYMD               CL**9
01689          MOVE DATE-YR                TO PD-YR                     ECS028
01690          MOVE DATE-MO                TO PD-MO                     ECS028
01691          MOVE DATE-DA                TO PD-DAY                    ECS028
01692          MOVE PRT-DATE               TO DT8-ANNV-DATE.            ECS028
01693                                                                   ECS028
01694      MOVE SPACES                     TO DT6-BUS-TYPE-DESC         ECS028
01695                                         DT6-BUS-TYPE.             ECS028
01696      IF HLD-ACCOUNT-REQ OR                                        ECS028
01697         (SSC-ACCOUNT AND HLD-DYNAMIC-REQUEST) OR                  ECS028
01698         HLD-BUSINESS-TYPE-REQ OR                                  ECS028
01699         (SSC-BUS-TYPE AND HLD-DYNAMIC-REQUEST)                    ECS028
01700          MOVE HLD-BUSINESS-TYPE      TO DT6-BUS-TYPE              ECS028
01701          PERFORM 4205-EMPTY-LOOP                                  ECS028
01702            VARYING CLAS-INDEXB FROM +1 BY +1                      ECS028
01703            UNTIL HLD-BUSINESS-TYPE = CLAS-BUSC-CODE (CLAS-INDEXB) ECS028
01704               OR CLAS-INDEXB       = CLAS-MAXB                    ECS028
01705          IF HLD-BUSINESS-TYPE = CLAS-BUSC-CODE (CLAS-INDEXB)      ECS028
01706            MOVE CLAS-BUSC-DESC (CLAS-INDEXB) TO DT6-BUS-TYPE-DESC.ECS028
01707                                                                   ECS028
01708      MOVE SPACES                     TO DT4-GA.                   ECS028
01709                                                                   ECS028
01710      IF HLD-ACCOUNT-REQ OR                                        ECS028
01711         HLD-GA-ACCT-REQ OR                                        ECS028
01712         HLD-GA-REQ                                                ECS028
01713          MOVE HLD-GA        TO DT4-GA.                            ECS028
01714                                                                   ECS028
01715      MOVE SPACES                     TO DT1-RETRO-GRP.            ECS028
01716                                                                   ECS028
01717      IF HLD-ACCOUNT-REQ OR                                        ECS028
01718         (SSC-ACCOUNT AND HLD-DYNAMIC-REQUEST) OR                  ECS028
01719         HLD-RETRO-GRP-REQ OR                                      ECS028
01720         (SSC-RETRO AND HLD-DYNAMIC-REQUEST)                       ECS028
01721          MOVE HLD-RETRO-GROUP        TO DT1-RETRO-GRP.            ECS028
01722                                                                   ECS028
01723      MOVE HLD-RETRO-AMT-PAID         TO DT2-RETRO-AMT.            ECS028
01724                                                                   ECS028
01725      MOVE +0                         TO DT5-RETEN-LIFE            ECS028
01726                                         DT6-RETEN-AH.             ECS028
01727      IF HLD-ACCOUNT-REQ OR                                        ECS028
01728         (SSC-ACCOUNT AND HLD-DYNAMIC-REQUEST)                     ECS028
01729          MULTIPLY HLD-ACCT-RETEN-LIFE                             ECS028
01730              BY +100 GIVING AMT-SUM                               ECS028
01731          MOVE AMT-SUM                TO DT5-RETEN-LIFE            ECS028
01732          MULTIPLY HLD-ACCT-RETEN-AH                               ECS028
01733              BY +100 GIVING AMT-SUM                               ECS028
01734          MOVE AMT-SUM                TO DT6-RETEN-AH.             ECS028
01735                                                                   ECS028
01736  4201-SET-UP-CONT.                                                ECS028
01737      SET EADM P10 TO +1.                                          ECS028
01738      PERFORM 4210-SET-LINE-TYPE-10 THRU 4210-EXIT 7 TIMES.        ECS028
01739                                                                   ECS028
01740      SET EADM P11 TO +1.                                          ECS028
01741      PERFORM 4220-SET-LINE-TYPE-11 THRU 4220-EXIT 7 TIMES.        ECS028
01742                                                                   ECS028
01743      SET EADM P12Q TO +1.                                         ECS028
01744      PERFORM 4230-SET-LINE-TYPE-12 THRU 4230-EXIT 7 TIMES.        ECS028
01745                                                                   ECS028
01746      SET EADT EADA P15 TO +1.                                     ECS028
01747      PERFORM 4240-SET-LINE-TYPE-15P THRU 4240-EXIT 7 TIMES.       ECS028
01748                                                                   ECS028
01749  4201-EXIT.                                                       ECS028
01750      EXIT.                                                        ECS028
01751                                                                   ECS028
01752  4205-EMPTY-LOOP.                                                 ECS028
01753      EXIT.                                                        ECS028
01754  EJECT                                                            ECS028
01755  4210-SET-LINE-TYPE-10.                                           ECS028
01756      MOVE EAD-ML-COUNT  (EADM) TO DT10-LIFE-ISSUES (P10).         ECS028
01757      MOVE EAD-MA-COUNT  (EADM) TO DT10-AH-ISSUES   (P10).         ECS028
01758                                                                   ECS028
01759      MOVE +0 TO DT10-LIFE-PREM (P10).                             ECS028
01760                                                                   ECS028
01761      IF EAD-ML-PREM (EADM) GREATER THAN ZEROS                     ECS028
01762          ADD  POS-ROUNDER        TO   EAD-ML-PREM    (EADM)       ECS028
01763          MOVE EAD-ML-PREM (EADM) TO   DT10-LIFE-PREM (P10)        ECS028
01764          SUBTRACT POS-ROUNDER    FROM EAD-ML-PREM    (EADM)       ECS028
01765      ELSE                                                         ECS028
01766          IF EAD-ML-PREM (EADM) LESS THAN ZEROS                    ECS028
01767              ADD  NEG-ROUNDER        TO   EAD-ML-PREM    (EADM)   ECS028
01768              MOVE EAD-ML-PREM (EADM) TO   DT10-LIFE-PREM (P10)    ECS028
01769              SUBTRACT NEG-ROUNDER    FROM EAD-ML-PREM    (EADM).  ECS028
01770                                                                   ECS028
01771      MOVE +0 TO DT10-AH-PREM (P10).                               ECS028
01772                                                                   ECS028
01773      IF EAD-MA-PREM (EADM) GREATER THAN ZEROS                     ECS028
01774          ADD  POS-ROUNDER        TO   EAD-MA-PREM    (EADM)       ECS028
01775          MOVE EAD-MA-PREM (EADM) TO   DT10-AH-PREM   (P10)        ECS028
01776          SUBTRACT POS-ROUNDER    FROM EAD-MA-PREM    (EADM)       ECS028
01777      ELSE                                                         ECS028
01778          IF EAD-MA-PREM (EADM) LESS THAN ZEROS                    ECS028
01779              ADD  NEG-ROUNDER        TO   EAD-MA-PREM    (EADM)   ECS028
01780              MOVE EAD-MA-PREM (EADM) TO   DT10-AH-PREM   (P10)    ECS028
01781              SUBTRACT NEG-ROUNDER    FROM EAD-MA-PREM    (EADM).  ECS028
01782                                                                   ECS028
01783      MOVE +0 TO DT10-LIFE-COMMIS (P10).                           ECS028
01784                                                                   ECS028
01785      IF EAD-ML-COMMIS (EADM) GREATER THAN ZEROS                   ECS028
01786          ADD  POS-ROUNDER          TO   EAD-ML-COMMIS    (EADM)   ECS028
01787          MOVE EAD-ML-COMMIS (EADM) TO   DT10-LIFE-COMMIS (P10)    ECS028
01788          SUBTRACT POS-ROUNDER      FROM EAD-ML-COMMIS    (EADM)   ECS028
01789      ELSE                                                         ECS028
01790          IF EAD-ML-COMMIS (EADM) LESS THAN ZEROS                  ECS028
01791              ADD  NEG-ROUNDER          TO EAD-ML-COMMIS    (EADM) ECS028
01792              MOVE EAD-ML-COMMIS (EADM) TO DT10-LIFE-COMMIS (P10)  ECS028
01793              SUBTRACT NEG-ROUNDER FROM EAD-ML-COMMIS    (EADM).   ECS028
01794                                                                   ECS028
01795      MOVE +0 TO DT10-AH-COMMIS (P10).                             ECS028
01796                                                                   ECS028
01797      IF EAD-MA-COMMIS (EADM) GREATER THAN ZEROS                   ECS028
01798          ADD  POS-ROUNDER          TO   EAD-MA-COMMIS   (EADM)    ECS028
01799          MOVE EAD-MA-COMMIS (EADM) TO   DT10-AH-COMMIS  (P10)     ECS028
01800          SUBTRACT POS-ROUNDER      FROM EAD-MA-COMMIS   (EADM)    ECS028
01801      ELSE                                                         ECS028
01802          IF EAD-MA-COMMIS (EADM) LESS THAN ZEROS                  ECS028
01803              ADD  NEG-ROUNDER          TO EAD-MA-COMMIS  (EADM)   ECS028
01804              MOVE EAD-MA-COMMIS (EADM) TO DT10-AH-COMMIS (P10)    ECS028
01805              SUBTRACT NEG-ROUNDER FROM EAD-MA-COMMIS  (EADM).     ECS028
01806                                                                   ECS028
01807      SET EADM P10 UP BY +1.                                       ECS028
01808                                                                   ECS028
01809  4210-EXIT.                                                       ECS028
01810      EXIT.                                                        ECS028
01811  EJECT                                                            ECS028
01812  4220-SET-LINE-TYPE-11.                                           ECS028
01813      MOVE +0 TO DT11-LIFE-EARNED (P11).                           ECS028
01814                                                                   ECS028
01815      IF EAD-ML-EARN-PREM (EADM) GREATER THAN ZEROS                ECS028
01816          ADD  POS-ROUNDER     TO   EAD-ML-EARN-PREM (EADM)        ECS028
01817          MOVE EAD-ML-EARN-PREM (EADM)                             ECS028
01818                               TO   DT11-LIFE-EARNED (P11)         ECS028
01819          SUBTRACT POS-ROUNDER FROM EAD-ML-EARN-PREM (EADM)        ECS028
01820      ELSE                                                         ECS028
01821          IF EAD-ML-EARN-PREM (EADM) LESS THAN ZEROS               ECS028
01822              ADD  NEG-ROUNDER     TO   EAD-ML-EARN-PREM (EADM)    ECS028
01823              MOVE EAD-ML-EARN-PREM (EADM)                         ECS028
01824                                   TO   DT11-LIFE-EARNED (P11)     ECS028
01825              SUBTRACT NEG-ROUNDER FROM EAD-ML-EARN-PREM (EADM).   ECS028
01826                                                                   ECS028
01827      MOVE +0 TO DT11-AH-EARNED (P11).                             ECS028
01828                                                                   ECS028
01829      IF EAD-MA-EARN-PREM (EADM) GREATER THAN ZEROS                ECS028
01830          ADD  POS-ROUNDER     TO   EAD-MA-EARN-PREM (EADM)        ECS028
01831          MOVE EAD-MA-EARN-PREM (EADM)                             ECS028
01832                               TO   DT11-AH-EARNED   (P11)         ECS028
01833          SUBTRACT POS-ROUNDER FROM EAD-MA-EARN-PREM (EADM)        ECS028
01834      ELSE                                                         ECS028
01835          IF EAD-MA-EARN-PREM (EADM) LESS THAN ZEROS               ECS028
01836              ADD  NEG-ROUNDER     TO   EAD-MA-EARN-PREM (EADM)    ECS028
01837              MOVE EAD-MA-EARN-PREM (EADM)                         ECS028
01838                                   TO   DT11-AH-EARNED   (P11)     ECS028
01839              SUBTRACT NEG-ROUNDER FROM EAD-MA-EARN-PREM (EADM).   ECS028
01840                                                                   ECS028
01841      MOVE +0 TO DT11-LIFE-INCUR (P11).                            ECS028
01842                                                                   ECS028
01843      IF EAD-ML-CLAIM-INCURRED (EADM) GREATER THAN ZEROS           ECS028
01844          ADD  POS-ROUNDER     TO   EAD-ML-CLAIM-INCURRED (EADM)   ECS028
01845          MOVE EAD-ML-CLAIM-INCURRED (EADM)                        ECS028
01846                               TO   DT11-LIFE-INCUR       (P11)    ECS028
01847          SUBTRACT POS-ROUNDER FROM EAD-ML-CLAIM-INCURRED (EADM)   ECS028
01848      ELSE                                                         ECS028
01849          IF EAD-ML-CLAIM-INCURRED (EADM) LESS THAN ZEROS          ECS028
01850              ADD  NEG-ROUNDER                                     ECS028
01851                            TO   EAD-ML-CLAIM-INCURRED (EADM)      ECS028
01852              MOVE EAD-ML-CLAIM-INCURRED (EADM)                    ECS028
01853                            TO   DT11-LIFE-INCUR       (P11)       ECS028
01854              SUBTRACT NEG-ROUNDER                                 ECS028
01855                            FROM EAD-ML-CLAIM-INCURRED (EADM).     ECS028
01856                                                                   ECS028
01857      MOVE +0 TO DT11-AH-INCUR (P11).                              ECS028
01858                                                                   ECS028
01859      IF EAD-MA-CLAIM-INCURRED (EADM) GREATER THAN ZEROS           ECS028
01860          ADD  POS-ROUNDER     TO   EAD-MA-CLAIM-INCURRED (EADM)   ECS028
01861          MOVE EAD-MA-CLAIM-INCURRED (EADM)                        ECS028
01862                               TO   DT11-AH-INCUR         (P11)    ECS028
01863          SUBTRACT POS-ROUNDER FROM EAD-MA-CLAIM-INCURRED (EADM)   ECS028
01864      ELSE                                                         ECS028
01865          IF EAD-MA-CLAIM-INCURRED (EADM) LESS THAN ZEROS          ECS028
01866              ADD  NEG-ROUNDER                                     ECS028
01867                            TO   EAD-MA-CLAIM-INCURRED (EADM)      ECS028
01868              MOVE EAD-MA-CLAIM-INCURRED (EADM)                    ECS028
01869                            TO   DT11-AH-INCUR         (P11)       ECS028
01870              SUBTRACT NEG-ROUNDER                                 ECS028
01871                            FROM EAD-MA-CLAIM-INCURRED (EADM).     ECS028
01872                                                                   ECS028
01873      MOVE +0 TO DT11-LIFE-CLM-PAID (P11).                         ECS028
01874                                                                   ECS028
01875      IF EAD-ML-CLAIMS-PAID (EADM) GREATER THAN ZEROS              ECS028
01876          ADD POS-ROUNDER      TO   EAD-ML-CLAIMS-PAID (EADM)      ECS028
01877          MOVE EAD-ML-CLAIMS-PAID (EADM)                           ECS028
01878                               TO   DT11-LIFE-CLM-PAID (P11)       ECS028
01879          SUBTRACT POS-ROUNDER FROM EAD-ML-CLAIMS-PAID (EADM)      ECS028
01880      ELSE                                                         ECS028
01881          IF EAD-ML-CLAIMS-PAID (EADM) LESS THAN ZEROS             ECS028
01882              ADD NEG-ROUNDER      TO   EAD-ML-CLAIMS-PAID (EADM)  ECS028
01883              MOVE EAD-ML-CLAIMS-PAID (EADM)                       ECS028
01884                                   TO   DT11-LIFE-CLM-PAID (P11)   ECS028
01885              SUBTRACT NEG-ROUNDER FROM EAD-ML-CLAIMS-PAID (EADM). ECS028
01886                                                                   ECS028
01887      MOVE +0 TO DT11-AH-CLM-PAID (P11).                           ECS028
01888                                                                   ECS028
01889      IF EAD-MA-CLAIMS-PAID (EADM) GREATER THAN ZEROS              ECS028
01890          ADD POS-ROUNDER      TO   EAD-MA-CLAIMS-PAID (EADM)      ECS028
01891          MOVE EAD-MA-CLAIMS-PAID (EADM)                           ECS028
01892                               TO   DT11-AH-CLM-PAID   (P11)       ECS028
01893          SUBTRACT POS-ROUNDER FROM EAD-MA-CLAIMS-PAID (EADM)      ECS028
01894      ELSE                                                         ECS028
01895          IF EAD-MA-CLAIMS-PAID (EADM) LESS THAN ZEROS             ECS028
01896              ADD NEG-ROUNDER      TO   EAD-MA-CLAIMS-PAID (EADM)  ECS028
01897              MOVE EAD-MA-CLAIMS-PAID (EADM)                       ECS028
01898                                   TO   DT11-AH-CLM-PAID   (P11)   ECS028
01899              SUBTRACT NEG-ROUNDER FROM EAD-MA-CLAIMS-PAID (EADM). ECS028
01900                                                                   ECS028
01901      SET EADM P11 UP BY +1.                                       ECS028
01902                                                                   ECS028
01903  4220-EXIT.                                                       ECS028
01904      EXIT.                                                        ECS028
01905  EJECT                                                            ECS028
01906  4230-SET-LINE-TYPE-12.                                           ECS028
01907      MOVE EAD-ML-CLAIM-INCURRED (EADM)  TO AMT-1.                 ECS028
01908      MOVE EAD-ML-EARN-PREM      (EADM)  TO AMT-2.                 ECS028
01909      MOVE EAD-MA-CLAIM-INCURRED (EADM)  TO AMT-3.                 ECS028
01910      MOVE EAD-MA-EARN-PREM      (EADM)  TO AMT-4.                 ECS028
01911                                                                   ECS028
01912      IF  AMT-1 = ZEROS AND                                        ECS028
01913          AMT-3 = ZEROS                                            ECS028
01914           MOVE ZEROS TO PERCENT-1                                 ECS028
01915                         PERCENT-2                                 ECS028
01916                         PERCENT-3                                 ECS028
01917       ELSE                                                        ECS028
01918          PERFORM 4231-CALCULATE-RATIOS THRU 4231-EXIT.            ECS028
01919                                                                   ECS028
01920      SET P12 TO +1.                                               ECS028
01921                                                                   ECS028
01922      IF PERCENT-1 LESS THAN +10000                                ECS028
01923          MOVE PERCENT-1  TO DT12-R-QTR      (P12, P12Q)           ECS028
01924      ELSE                                                         ECS028
01925          MOVE '****'     TO DT12-RATIO-QTR  (P12, P12Q).          ECS028
01926                                                                   ECS028
01927      SET P12 UP BY +1.                                            ECS028
01928                                                                   ECS028
01929      IF PERCENT-2 LESS THAN +10000                                ECS028
01930          MOVE PERCENT-2  TO DT12-R-QTR      (P12, P12Q)           ECS028
01931      ELSE                                                         ECS028
01932          MOVE '****'     TO DT12-RATIO-QTR  (P12, P12Q).          ECS028
01933                                                                   ECS028
01934      SET P12 UP BY +1.                                            ECS028
01935                                                                   ECS028
01936      IF PERCENT-3 LESS THAN +10000                                ECS028
01937          MOVE PERCENT-3  TO DT12-R-QTR      (P12, P12Q)           ECS028
01938      ELSE                                                         ECS028
01939          MOVE '****'     TO DT12-RATIO-QTR  (P12, P12Q).          ECS028
01940                                                                   ECS028
01941      SET EADM P12Q UP BY +1.                                      ECS028
01942                                                                   ECS028
01943  4230-EXIT.                                                       ECS028
01944      EXIT.                                                        ECS028
01945  EJECT                                                            ECS028
01946  4231-CALCULATE-RATIOS.                                           ECS028
01947      IF AMT-2 = ZEROS                                             ECS028
01948          MOVE +1 TO AMT-2.                                        ECS028
01949                                                                   ECS028
01950      IF AMT-1 NOT = ZEROS                                         ECS028
01951          COMPUTE PERCENT-1 ROUNDED = (AMT-1 / AMT-2) * 100        ECS028
01952      ELSE                                                         ECS028
01953          MOVE ZEROS TO PERCENT-1.                                 ECS028
01954                                                                   ECS028
01955      IF AMT-4 = ZEROS                                             ECS028
01956          MOVE +1 TO AMT-4.                                        ECS028
01957                                                                   ECS028
01958      IF AMT-3 NOT = ZEROS                                         ECS028
01959          COMPUTE PERCENT-2 ROUNDED = (AMT-3 / AMT-4) * 100        ECS028
01960      ELSE                                                         ECS028
01961          MOVE ZEROS TO PERCENT-2.                                 ECS028
01962                                                                   ECS028
01963      IF AMT-2 = +1 AND                                            ECS028
01964         AMT-4 GREATER THAN +1                                     ECS028
01965          MOVE +0 TO AMT-2                                         ECS028
01966      ELSE                                                         ECS028
01967          IF AMT-4 = +1 AND                                        ECS028
01968             AMT-2 GREATER THAN +1                                 ECS028
01969              MOVE +0 TO AMT-4                                     ECS028
01970          ELSE                                                     ECS028
01971              IF AMT-2 = +1 AND                                    ECS028
01972                 AMT-4 = +1                                        ECS028
01973                  MOVE +0 TO AMT-2   MOVE +1 TO AMT-4.             ECS028
01974                                                                   ECS028
01975      IF (AMT-1 + AMT-3) NOT = ZERO                                ECS028
01976          COMPUTE PERCENT-3 ROUNDED =                              ECS028
01977                 (AMT-1 + AMT-3) / (AMT-2 + AMT-4) * 100           ECS028
01978      ELSE                                                         ECS028
01979          MOVE ZERO TO PERCENT-3.                                  ECS028
01980                                                                   ECS028
01981  4231-EXIT.                                                       ECS028
01982      EXIT.                                                        ECS028
01983  EJECT                                                            ECS028
01984  4240-SET-LINE-TYPE-15P.                                          ECS028
01985 *                        ---PREMIUM DISTRIBUTION COMPLETION---    ECS028
01986      MOVE ZEROS TO AMT-1                                          ECS028
01987                    AMT-2.                                         ECS028
01988      SET EADA   TO +1.                                            ECS028
01989      PERFORM 4241-CALC-TOTAL-FOR-TERM  5 TIMES.                   ECS028
01990                                                                   ECS028
01991      SET EADA TO +1.                                              ECS028
01992      PERFORM 4242-CALC-N-SET-AGE-PCTS  5 TIMES.                   ECS028
01993                                                                   ECS028
01994 *   ---AVERAGE PERCENTAGE FOR TERM---                             ECS028
01995                                                                   ECS028
01996      IF ZEROS = EAD-L-AGE-COUNT-PREM (EADT)  OR                   ECS028
01997                 EAD-L-AGE-SUM-PREM   (EADT)                       ECS028
01998          MOVE ZEROS TO PERCENT-1                                  ECS028
01999      ELSE                                                         ECS028
02000          COMPUTE PERCENT-1 ROUNDED =                              ECS028
02001          EAD-L-AGE-SUM-PREM (EADT) / EAD-L-AGE-COUNT-PREM (EADT). ECS028
02002                                                                   ECS028
02003      IF ZEROS = EAD-A-AGE-COUNT-PREM (EADT) OR                    ECS028
02004                 EAD-A-AGE-SUM-PREM   (EADT)                       ECS028
02005          MOVE ZEROS TO PERCENT-2                                  ECS028
02006      ELSE                                                         ECS028
02007          COMPUTE PERCENT-2 ROUNDED =                              ECS028
02008          EAD-A-AGE-SUM-PREM (EADT) / EAD-A-AGE-COUNT-PREM (EADT). ECS028
02009                                                                   ECS028
02010      IF PERCENT-1 LESS THAN +1000                                 ECS028
02011          MOVE PERCENT-1 TO DT15-L-AVG     (P15)                   ECS028
02012      ELSE                                                         ECS028
02013          MOVE '***'     TO DT15-L-AGE-AVG (P15).                  ECS028
02014                                                                   ECS028
02015      IF PERCENT-2 LESS THAN +1000                                 ECS028
02016          MOVE PERCENT-2 TO DT15-A-AVG     (P15)                   ECS028
02017      ELSE                                                         ECS028
02018          MOVE '***'     TO DT15-A-AGE-AVG (P15).                  ECS028
02019                                                                   ECS028
02020 *   ---WEIGHTED PERCENTAGE FOR TERM---                            ECS028
02021                                                                   ECS028
02022      IF AMT-1 = ZEROS                                             ECS028
02023          MOVE ZEROS TO PERCENT-1                                  ECS028
02024      ELSE                                                         ECS028
02025          COMPUTE PERCENT-1 ROUNDED =                              ECS028
02026             EAD-L-WTD-PREM (EADT) / AMT-1.                        ECS028
02027                                                                   ECS028
02028      IF AMT-2 = ZEROS                                             ECS028
02029          MOVE ZEROS TO PERCENT-2                                  ECS028
02030      ELSE                                                         ECS028
02031          COMPUTE PERCENT-2 ROUNDED =                              ECS028
02032             EAD-A-WTD-PREM (EADT) / AMT-2.                        ECS028
02033                                                                   ECS028
02034      IF PERCENT-1 LESS THAN +1000                                 ECS028
02035          MOVE PERCENT-1 TO DT15-L-WTD   (P15)                     ECS028
02036      ELSE                                                         ECS028
02037          MOVE '***'     TO DT15-L-WTD-X (P15).                    ECS028
02038                                                                   ECS028
02039      IF PERCENT-2 LESS THAN +1000                                 ECS028
02040          MOVE PERCENT-2 TO DT15-A-WTD   (P15)                     ECS028
02041      ELSE                                                         ECS028
02042          MOVE '***'     TO DT15-A-WTD-X (P15).                    ECS028
02043                                                                   ECS028
02044      SET EADT P15 UP BY +1.                                       ECS028
02045                                                                   ECS028
02046  4240-EXIT.                                                       ECS028
02047      EXIT.                                                        ECS028
02048                                                                   ECS028
02049  4241-CALC-TOTAL-FOR-TERM.                                        ECS028
02050      ADD EAD-L-PREM (EADT, EADA) TO AMT-1.                        ECS028
02051      ADD EAD-A-PREM (EADT, EADA) TO AMT-2.                        ECS028
02052                                                                   ECS028
02053      SET EADA UP BY +1.                                           ECS028
02054  EJECT                                                            ECS028
02055  4242-CALC-N-SET-AGE-PCTS.                                        ECS028
02056      IF EADA = +1                                                 ECS028
02057          IF EAD-L-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02058              COMPUTE PERCENT-1 ROUNDED =                          ECS028
02059                  (EAD-L-PREM (EADT, EADA) / AMT-1) * 100          ECS028
02060              IF PERCENT-1 LESS THAN +1000                         ECS028
02061                  MOVE PERCENT-1 TO DT15-L-AGE1  (P15)             ECS028
02062              ELSE                                                 ECS028
02063                  MOVE '***'     TO DT15-L-AGE-1 (P15)             ECS028
02064          ELSE                                                     ECS028
02065              MOVE ZEROS         TO DT15-L-AGE1  (P15).            ECS028
02066                                                                   ECS028
02067      IF EADA = +1                                                 ECS028
02068          IF EAD-A-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02069              COMPUTE PERCENT-2 ROUNDED =                          ECS028
02070                  (EAD-A-PREM (EADT, EADA) / AMT-2) * 100          ECS028
02071              IF PERCENT-2 LESS THAN +1000                         ECS028
02072                  MOVE PERCENT-2 TO DT15-A-AGE1  (P15)             ECS028
02073              ELSE                                                 ECS028
02074                  MOVE '***'     TO DT15-A-AGE-1 (P15)             ECS028
02075          ELSE                                                     ECS028
02076              MOVE ZEROS         TO DT15-A-AGE1  (P15).            ECS028
02077                                                                   ECS028
02078      IF EADA = +2                                                 ECS028
02079          IF EAD-L-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02080              COMPUTE PERCENT-1 ROUNDED =                          ECS028
02081                  (EAD-L-PREM (EADT, EADA) / AMT-1) * 100          ECS028
02082              IF PERCENT-1 LESS THAN +1000                         ECS028
02083                  MOVE PERCENT-1 TO DT15-L-AGE2  (P15)             ECS028
02084              ELSE                                                 ECS028
02085                  MOVE '***'     TO DT15-L-AGE-2 (P15)             ECS028
02086          ELSE                                                     ECS028
02087              MOVE ZEROS         TO DT15-L-AGE2  (P15).            ECS028
02088                                                                   ECS028
02089      IF EADA = +2                                                 ECS028
02090          IF EAD-A-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02091              COMPUTE PERCENT-2 ROUNDED =                          ECS028
02092                  (EAD-A-PREM (EADT, EADA) / AMT-2) * 100          ECS028
02093              IF PERCENT-2 LESS THAN +1000                         ECS028
02094                  MOVE PERCENT-2 TO DT15-A-AGE2  (P15)             ECS028
02095              ELSE                                                 ECS028
02096                  MOVE '***'     TO DT15-A-AGE-2 (P15)             ECS028
02097          ELSE                                                     ECS028
02098              MOVE ZEROS         TO DT15-A-AGE2  (P15).            ECS028
02099  EJECT                                                            ECS028
02100      IF EADA = +3                                                 ECS028
02101          IF EAD-L-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02102              COMPUTE PERCENT-1 ROUNDED =                          ECS028
02103                  (EAD-L-PREM (EADT, EADA) / AMT-1) * 100          ECS028
02104              IF PERCENT-1 LESS THAN +1000                         ECS028
02105                  MOVE PERCENT-1 TO DT15-L-AGE3  (P15)             ECS028
02106              ELSE                                                 ECS028
02107                  MOVE '***'     TO DT15-L-AGE-3 (P15)             ECS028
02108          ELSE                                                     ECS028
02109              MOVE ZEROS         TO DT15-L-AGE3  (P15).            ECS028
02110                                                                   ECS028
02111      IF EADA = +3                                                 ECS028
02112          IF EAD-A-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02113              COMPUTE PERCENT-2 ROUNDED =                          ECS028
02114                  (EAD-A-PREM (EADT, EADA) / AMT-2) * 100          ECS028
02115              IF PERCENT-2 LESS THAN +1000                         ECS028
02116                  MOVE PERCENT-2 TO DT15-A-AGE3  (P15)             ECS028
02117              ELSE                                                 ECS028
02118                  MOVE '***'     TO DT15-A-AGE-3 (P15)             ECS028
02119          ELSE                                                     ECS028
02120              MOVE ZEROS         TO DT15-A-AGE3  (P15).            ECS028
02121                                                                   ECS028
02122      IF EADA = +4                                                 ECS028
02123          IF EAD-L-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02124              COMPUTE PERCENT-1 ROUNDED =                          ECS028
02125                  (EAD-L-PREM (EADT, EADA) / AMT-1) * 100          ECS028
02126              IF PERCENT-1 LESS THAN +1000                         ECS028
02127                  MOVE PERCENT-1 TO DT15-L-AGE4  (P15)             ECS028
02128              ELSE                                                 ECS028
02129                  MOVE '***'     TO DT15-L-AGE-4 (P15)             ECS028
02130          ELSE                                                     ECS028
02131              MOVE ZEROS         TO DT15-L-AGE4  (P15).            ECS028
02132                                                                   ECS028
02133      IF EADA = +4                                                 ECS028
02134          IF EAD-A-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02135              COMPUTE PERCENT-2 ROUNDED =                          ECS028
02136                  (EAD-A-PREM (EADT, EADA) / AMT-2) * 100          ECS028
02137              IF PERCENT-2 LESS THAN +1000                         ECS028
02138                  MOVE PERCENT-2 TO DT15-A-AGE4  (P15)             ECS028
02139              ELSE                                                 ECS028
02140                  MOVE '***'     TO DT15-A-AGE-4 (P15)             ECS028
02141          ELSE                                                     ECS028
02142              MOVE ZEROS         TO DT15-A-AGE4  (P15).            ECS028
02143  EJECT                                                            ECS028
02144      IF EADA = +5                                                 ECS028
02145          IF EAD-L-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02146              COMPUTE PERCENT-1 ROUNDED =                          ECS028
02147                  (EAD-L-PREM (EADT, EADA) / AMT-1) * 100          ECS028
02148              IF PERCENT-1 LESS THAN +1000                         ECS028
02149                  MOVE PERCENT-1 TO DT15-L-AGE5  (P15)             ECS028
02150              ELSE                                                 ECS028
02151                  MOVE '***'     TO DT15-L-AGE-5 (P15)             ECS028
02152          ELSE                                                     ECS028
02153              MOVE ZEROS         TO DT15-L-AGE5  (P15).            ECS028
02154                                                                   ECS028
02155      IF EADA = +5                                                 ECS028
02156          IF EAD-A-PREM (EADT, EADA) NOT = ZEROS                   ECS028
02157              COMPUTE PERCENT-2 ROUNDED =                          ECS028
02158                  (EAD-A-PREM (EADT, EADA) / AMT-2) * 100          ECS028
02159              IF PERCENT-2 LESS THAN +1000                         ECS028
02160                  MOVE PERCENT-2 TO DT15-A-AGE5  (P15)             ECS028
02161              ELSE                                                 ECS028
02162                  MOVE '***'     TO DT15-A-AGE-5 (P15)             ECS028
02163          ELSE                                                     ECS028
02164              MOVE ZEROS         TO DT15-A-AGE5  (P15).            ECS028
02165                                                                   ECS028
02166       SET EADA UP BY +1.                                          ECS028
02167                                                                   ECS028
02168  4300-PRT-LINE-10.                                                ECS028
02169      MOVE PRT-10-LINE (P10) TO PRT-DATA.                          ECS028
02170      PERFORM 4900-PRINT-LINE.                                     ECS028
02171      SET P10 UP BY +1.                                            ECS028
02172                                                                   ECS028
02173  4310-PRT-LINE-11.                                                ECS028
02174      MOVE PRT-11-LINE (P11) TO PRT-DATA.                          ECS028
02175      PERFORM 4900-PRINT-LINE.                                     ECS028
02176      SET P11 UP BY +1.                                            ECS028
02177                                                                   ECS028
02178  4320-PRT-LINE-12.                                                ECS028
02179      MOVE PRT-12-LINE (P12) TO PRT-DATA.                          ECS028
02180      PERFORM 4900-PRINT-LINE.                                     ECS028
02181      SET P12 UP BY +1.                                            ECS028
02182                                                                   ECS028
02183  4330-PRT-LINE-15.                                                ECS028
02184      MOVE PRT-15-LINE (P15) TO PRT-DATA.                          ECS028
02185      PERFORM 4900-PRINT-LINE.                                     ECS028
02186      SET P15 UP BY +1.                                            ECS028
02187  EJECT                                                            ECS028
02188  4400-LOAD-CLAIM-DISTRIBUTION.                                    ECS028
02189 *                      ---CLAIM PAYMENT DISTRIBUTION COMPLETION---ECS028
02190      MOVE ZEROS TO AMT-1                                          ECS028
02191                    AMT-2.                                         ECS028
02192      SET EADA   TO +1.                                            ECS028
02193      PERFORM 4441-CALC-TOTAL-FOR-TERM  5 TIMES.                   ECS028
02194                                                                   ECS028
02195      SET EADA TO +1.                                              ECS028
02196      PERFORM 4442-CALC-N-SET-AGE-PCTS  5 TIMES.                   ECS028
02197                                                                   ECS028
02198 *   ---AVERAGE PERCENTAGE FOR TERM---                             ECS028
02199                                                                   ECS028
02200      IF ZEROS = EAD-L-AGE-COUNT-CLMS (EADT)  OR                   ECS028
02201                 EAD-L-AGE-SUM-CLMS   (EADT)                       ECS028
02202          MOVE ZEROS TO PERCENT-1                                  ECS028
02203      ELSE                                                         ECS028
02204          COMPUTE PERCENT-1 ROUNDED = EAD-L-AGE-SUM-CLMS (EADT)    ECS028
02205                                    / EAD-L-AGE-COUNT-CLMS (EADT). ECS028
02206                                                                   ECS028
02207      IF ZEROS = EAD-A-AGE-COUNT-CLMS (EADT)  OR                   ECS028
02208                 EAD-A-AGE-SUM-CLMS   (EADT)                       ECS028
02209          MOVE ZEROS TO PERCENT-2                                  ECS028
02210      ELSE                                                         ECS028
02211          COMPUTE PERCENT-2 ROUNDED = EAD-A-AGE-SUM-CLMS (EADT)    ECS028
02212                                    / EAD-A-AGE-COUNT-CLMS (EADT). ECS028
02213                                                                   ECS028
02214      IF PERCENT-1 LESS THAN +1001                                 ECS028
02215          MOVE PERCENT-1 TO DT15-L-AVG     (P15)                   ECS028
02216      ELSE                                                         ECS028
02217          MOVE '***'     TO DT15-L-AGE-AVG (P15).                  ECS028
02218                                                                   ECS028
02219      IF PERCENT-2 LESS THAN +1001                                 ECS028
02220          MOVE PERCENT-2 TO DT15-A-AVG     (P15)                   ECS028
02221      ELSE                                                         ECS028
02222          MOVE '***'     TO DT15-A-AGE-AVG (P15).                  ECS028
02223                                                                   ECS028
02224 *   ---WEIGHTED PERCENTAGE FOR TERM---                            ECS028
02225                                                                   ECS028
02226      IF AMT-1 = ZEROS OR                                          ECS028
02227         EAD-L-AGE-SUM-CLMS (EADT) = ZEROS                         ECS028
02228          MOVE ZEROS TO PERCENT-1                                  ECS028
02229      ELSE                                                         ECS028
02230          COMPUTE PERCENT-1 ROUNDED =                              ECS028
02231              EAD-L-WTD-CLAIMS (EADT) / AMT-1.                     ECS028
02232                                                                   ECS028
02233      IF AMT-2 = ZEROS                                             ECS028
02234          MOVE ZEROS TO PERCENT-2                                  ECS028
02235      ELSE                                                         ECS028
02236          COMPUTE PERCENT-2 ROUNDED =                              ECS028
02237              EAD-A-WTD-CLAIMS (EADT) / AMT-2.                     ECS028
02238  EJECT                                                            ECS028
02239      IF PERCENT-1 LESS THAN +1001                                 ECS028
02240          MOVE PERCENT-1 TO DT15-L-WTD   (P15)                     ECS028
02241      ELSE                                                         ECS028
02242          MOVE '***'     TO DT15-L-WTD-X (P15).                    ECS028
02243                                                                   ECS028
02244      IF PERCENT-2 LESS THAN +1001                                 ECS028
02245          MOVE PERCENT-2 TO DT15-A-WTD   (P15)                     ECS028
02246      ELSE                                                         ECS028
02247          MOVE '***'     TO DT15-A-WTD-X (P15).                    ECS028
02248                                                                   ECS028
02249      SET EADT P15 UP BY +1.                                       ECS028
02250                                                                   ECS028
02251  4400-EXIT.                                                       ECS028
02252      EXIT.                                                        ECS028
02253                                                                   ECS028
02254  4441-CALC-TOTAL-FOR-TERM.                                        ECS028
02255      ADD EAD-L-CLAIMS-PAID (EADT, EADA) TO AMT-1.                 ECS028
02256      ADD EAD-A-CLAIMS-PAID (EADT, EADA) TO AMT-2.                 ECS028
02257                                                                   ECS028
02258      SET EADA UP BY +1.                                           ECS028
02259  EJECT                                                            ECS028
02260  4442-CALC-N-SET-AGE-PCTS.                                        ECS028
02261      IF EADA = +1                                                 ECS028
02262          IF (EAD-L-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02263              (AMT-1 NOT = ZEROS)                                  ECS028
02264                  COMPUTE PERCENT-1 ROUNDED =                      ECS028
02265                     (EAD-L-CLAIMS-PAID (EADT, EADA) / AMT-1) * 100ECS028
02266                  IF PERCENT-1 LESS THAN +1001                     ECS028
02267                      MOVE PERCENT-1 TO DT15-L-AGE1  (P15)         ECS028
02268                  ELSE                                             ECS028
02269                      MOVE '***'     TO DT15-L-AGE-1 (P15)         ECS028
02270          ELSE                                                     ECS028
02271              MOVE ZEROS             TO DT15-L-AGE1  (P15).        ECS028
02272                                                                   ECS028
02273      IF EADA = +1                                                 ECS028
02274          IF (EAD-A-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02275              (AMT-2 NOT = ZEROS)                                  ECS028
02276                  COMPUTE PERCENT-2 ROUNDED =                      ECS028
02277                     (EAD-A-CLAIMS-PAID (EADT, EADA) / AMT-2) * 100ECS028
02278                  IF PERCENT-2 LESS THAN +1001                     ECS028
02279                      MOVE PERCENT-2 TO DT15-A-AGE1  (P15)         ECS028
02280                  ELSE                                             ECS028
02281                      MOVE '***'     TO DT15-A-AGE-1 (P15)         ECS028
02282          ELSE                                                     ECS028
02283              MOVE ZEROS             TO DT15-A-AGE1  (P15).        ECS028
02284                                                                   ECS028
02285      IF EADA = +2                                                 ECS028
02286          IF (EAD-L-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02287              (AMT-1 NOT = ZEROS)                                  ECS028
02288                  COMPUTE PERCENT-1 ROUNDED =                      ECS028
02289                     (EAD-L-CLAIMS-PAID (EADT, EADA) / AMT-1) * 100ECS028
02290                  IF PERCENT-1 LESS THAN +1001                     ECS028
02291                      MOVE PERCENT-1 TO DT15-L-AGE2  (P15)         ECS028
02292                  ELSE                                             ECS028
02293                      MOVE '***'     TO DT15-L-AGE-2 (P15)         ECS028
02294          ELSE                                                     ECS028
02295              MOVE ZEROS             TO DT15-L-AGE2  (P15).        ECS028
02296                                                                   ECS028
02297      IF EADA = +2                                                 ECS028
02298          IF (EAD-A-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02299              (AMT-2 NOT = ZEROS)                                  ECS028
02300                  COMPUTE PERCENT-2 ROUNDED =                      ECS028
02301                     (EAD-A-CLAIMS-PAID (EADT, EADA) / AMT-2) * 100ECS028
02302                  IF PERCENT-2 LESS THAN +1001                     ECS028
02303                      MOVE PERCENT-2 TO DT15-A-AGE2  (P15)         ECS028
02304                  ELSE                                             ECS028
02305                      MOVE '***'     TO DT15-A-AGE-2 (P15)         ECS028
02306          ELSE                                                     ECS028
02307              MOVE ZEROS             TO DT15-A-AGE2  (P15).        ECS028
02308  EJECT                                                            ECS028
02309      IF EADA = +3                                                 ECS028
02310          IF (EAD-L-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02311              (AMT-1 NOT = ZEROS)                                  ECS028
02312                  COMPUTE PERCENT-1 ROUNDED =                      ECS028
02313                     (EAD-L-CLAIMS-PAID (EADT, EADA) / AMT-1) * 100ECS028
02314                  IF PERCENT-1 LESS THAN +1001                     ECS028
02315                      MOVE PERCENT-1 TO DT15-L-AGE3  (P15)         ECS028
02316                  ELSE                                             ECS028
02317                      MOVE '***'     TO DT15-L-AGE-3 (P15)         ECS028
02318          ELSE                                                     ECS028
02319              MOVE ZEROS             TO DT15-L-AGE3  (P15).        ECS028
02320                                                                   ECS028
02321      IF EADA = +3                                                 ECS028
02322          IF (EAD-A-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02323              (AMT-2 NOT = ZEROS)                                  ECS028
02324                  COMPUTE PERCENT-2 ROUNDED =                      ECS028
02325                     (EAD-A-CLAIMS-PAID (EADT, EADA) / AMT-2) * 100ECS028
02326                  IF PERCENT-2 LESS THAN +1001                     ECS028
02327                      MOVE PERCENT-2 TO DT15-A-AGE3  (P15)         ECS028
02328                  ELSE                                             ECS028
02329                      MOVE '***'     TO DT15-A-AGE-3 (P15)         ECS028
02330          ELSE                                                     ECS028
02331              MOVE ZEROS             TO DT15-A-AGE3  (P15).        ECS028
02332                                                                   ECS028
02333      IF EADA = +4                                                 ECS028
02334          IF (EAD-L-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02335              (AMT-1 NOT = ZEROS)                                  ECS028
02336                  COMPUTE PERCENT-1 ROUNDED =                      ECS028
02337                     (EAD-L-CLAIMS-PAID (EADT, EADA) / AMT-1) * 100ECS028
02338                  IF PERCENT-1 LESS THAN +1001                     ECS028
02339                      MOVE PERCENT-1 TO DT15-L-AGE4  (P15)         ECS028
02340                  ELSE                                             ECS028
02341                      MOVE '***'     TO DT15-L-AGE-4 (P15)         ECS028
02342          ELSE                                                     ECS028
02343              MOVE ZEROS             TO DT15-L-AGE4  (P15).        ECS028
02344                                                                   ECS028
02345      IF EADA = +4                                                 ECS028
02346          IF (EAD-A-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02347              (AMT-2 NOT = ZEROS)                                  ECS028
02348                  COMPUTE PERCENT-2 ROUNDED =                      ECS028
02349                     (EAD-A-CLAIMS-PAID (EADT, EADA) / AMT-2) * 100ECS028
02350                  IF PERCENT-2 LESS THAN +1001                     ECS028
02351                      MOVE PERCENT-2 TO DT15-A-AGE4  (P15)         ECS028
02352                  ELSE                                             ECS028
02353                      MOVE '***'     TO DT15-A-AGE-4 (P15)         ECS028
02354          ELSE                                                     ECS028
02355              MOVE ZEROS             TO DT15-A-AGE4  (P15).        ECS028
02356  EJECT                                                            ECS028
02357      IF EADA = +5                                                 ECS028
02358          IF (EAD-L-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02359              (AMT-1 NOT = ZEROS)                                  ECS028
02360                  COMPUTE PERCENT-1 ROUNDED =                      ECS028
02361                     (EAD-L-CLAIMS-PAID (EADT, EADA) / AMT-1) * 100ECS028
02362                  IF PERCENT-1 LESS THAN +1001                     ECS028
02363                      MOVE PERCENT-1 TO DT15-L-AGE5  (P15)         ECS028
02364                  ELSE                                             ECS028
02365                      MOVE '***'     TO DT15-L-AGE-5 (P15)         ECS028
02366          ELSE                                                     ECS028
02367              MOVE ZEROS             TO DT15-L-AGE5  (P15).        ECS028
02368                                                                   ECS028
02369      IF EADA = +5                                                 ECS028
02370          IF (EAD-A-CLAIMS-PAID (EADT, EADA) NOT = ZEROS) AND      ECS028
02371              (AMT-2 NOT = ZEROS)                                  ECS028
02372                  COMPUTE PERCENT-2 ROUNDED =                      ECS028
02373                     (EAD-A-CLAIMS-PAID (EADT, EADA) / AMT-2) * 100ECS028
02374                  IF PERCENT-2 LESS THAN +1001                     ECS028
02375                      MOVE PERCENT-2 TO DT15-A-AGE5  (P15)         ECS028
02376                  ELSE                                             ECS028
02377                      MOVE '***'     TO DT15-A-AGE-5 (P15)         ECS028
02378          ELSE                                                     ECS028
02379              MOVE ZEROS             TO DT15-A-AGE5  (P15).        ECS028
02380                                                                   ECS028
02381       SET EADA UP BY +1.                                          ECS028
02382  EJECT                                                            ECS028
02383  4500-PRINT-GRAPH-PAGE.                                           ECS028
02384      MOVE SPACES    TO PRT-GRAPH-LINE-1.                          ECS028
02385      MOVE PLUG-BASE TO AMT-1.                                     ECS028
02386      SET EADGP      TO +1.                                        ECS028
02387      PERFORM 4510-FIND-GREATEST-AMT 12 TIMES.                     ECS028
02388                                                                   ECS028
02389      IF AMT-1 NOT GREATER THAN ZEROS                              ECS028
02390          MOVE PLUG-SUM TO AMT-1.                                  ECS028
02391                                                                   ECS028
02392      DIVIDE AMT-1 BY PLUG-SUM GIVING FLAT-SUM-1                   ECS028
02393                            REMAINDER FLAT-SUM-2.                  ECS028
02394                                                                   ECS028
02395      IF FLAT-SUM-2 GREATER THAN +0                                ECS028
02396          ADD +1 TO FLAT-SUM-1.                                    ECS028
02397                                                                   ECS028
02398      MULTIPLY FLAT-SUM-1 BY PLUG-BASE GIVING FLAT-SUM-2.          ECS028
02399      MOVE FLAT-SUM-2     TO FLAT-SUM-1.                           ECS028
02400                                                                   ECS028
02401      MOVE +45  TO INDX.                                           ECS028
02402      SET PRT1A TO +45.                                            ECS028
02403      MOVE LOW-VALUES TO HOLD-BRACKET-AMOUNTS                      ECS028
02404                         GRAPH-FILL-IN-WORK.                       ECS028
02405      MOVE +0         TO HLD-BRKT-AMT (INDX)                       ECS028
02406                         PRT-BRACKET-AMT (PRT1A)                   ECS028
02407                         FLAT-SUM-2.                               ECS028
02408                                                                   ECS028
02409      PERFORM 4520-INIT-REST-OF-GRAPH 44 TIMES.                    ECS028
02410                                                                   ECS028
02411      MOVE '0'        TO PRT-1-PRT-CNTL (PRT1A)                    ECS028
02412                         GFI-LINE-DIFF-2.                          ECS028
02413                                                                   ECS028
02414      MOVE +1   TO INDX.                                           ECS028
02415      SET PRT1B EADGP GFI TO +1.                                   ECS028
02416      PERFORM 4530-LOAD-MONTH-CHARACTERS THRU 4530-EXIT 12 TIMES.  ECS028
02417                                                                   ECS028
02418      SET GFI PRT1A PRT1B  TO +1.                                  ECS028
02419      PERFORM 4540-FILL-IN-GRAPH THRU 4540-EXIT 11 TIMES.          ECS028
02420                                                                   ECS028
02421      ADD +1           TO PAGE-CNT.                                ECS028
02422      MOVE PAGE-CNT    TO HD2A-PAGE.                               ECS028
02423      MOVE GRAPH-HEADER-LINE-1 TO PRT-DATA.                        ECS028
02424      PERFORM 4900-PRINT-LINE.                                     ECS028
02425      MOVE PRT-HDR-2           TO PRT-DATA.                        ECS028
02426      PERFORM 4900-PRINT-LINE.                                     ECS028
02427      MOVE PRT-HDR-2A          TO PRT-DATA.                        ECS028
02428      PERFORM 4900-PRINT-LINE.                                     ECS028
02429                                                                   ECS028
02430      SET PRT1A TO +1.                                             ECS028
02431                                                                   ECS028
02432      PERFORM 4650-PRINT-GRAPH-LINE-1 THRU 4651-EXIT 45 TIMES.     ECS028
02433                                                                   ECS028
02434      MOVE PRT-GRAPH-LINE-2 TO PRT-DATA.                           ECS028
02435      PERFORM 4900-PRINT-LINE.                                     ECS028
02436                                                                   ECS028
02437      MOVE LEGION-OF-GRAPH-CHARS TO PRT-DATA.                      ECS028
02438      PERFORM 4900-PRINT-LINE.                                     ECS028
02439                                                                   ECS028
02440  4501-EXIT.                                                       ECS028
02441      EXIT.                                                        ECS028
02442  EJECT                                                            ECS028
02443  4510-FIND-GREATEST-AMT.                                          ECS028
02444      IF EAD-NET-WRITTEN-PREM (EADGP) GREATER THAN AMT-1           ECS028
02445          MOVE EAD-NET-WRITTEN-PREM (EADGP) TO AMT-1.              ECS028
02446                                                                   ECS028
02447      IF EAD-EARNED-PREM      (EADGP) GREATER THAN AMT-1           ECS028
02448          MOVE EAD-EARNED-PREM      (EADGP) TO AMT-1.              ECS028
02449                                                                   ECS028
02450      IF EAD-CLAIMS-PAID      (EADGP) GREATER THAN AMT-1           ECS028
02451          MOVE EAD-CLAIMS-PAID      (EADGP) TO AMT-1.              ECS028
02452                                                                   ECS028
02453      SET EADGP UP BY +1.                                          ECS028
02454                                                                   ECS028
02455  4520-INIT-REST-OF-GRAPH.                                         ECS028
02456      SUBTRACT +1 FROM INDX.                                       ECS028
02457      SET PRT1A TO INDX.                                           ECS028
02458                                                                   ECS028
02459      ADD FLAT-SUM-1                                               ECS028
02460          FLAT-SUM-2  GIVING HLD-BRKT-AMT (INDX).                  ECS028
02461                                                                   ECS028
02462      MOVE HLD-BRKT-AMT (INDX) TO PRT-BRACKET-AMT (PRT1A).         ECS028
02463                                                                   ECS028
02464      ADD FLAT-SUM-1 TO FLAT-SUM-2.                                ECS028
02465                                                                   ECS028
02466  4530-LOAD-MONTH-CHARACTERS.                                      ECS028
02467      MOVE +0 TO GFI-HOLD-LINE-P                                   ECS028
02468                 GFI-HOLD-LINE-E                                   ECS028
02469                 GFI-HOLD-LINE-C.                                  ECS028
02470                                                                   ECS028
02471      SET PRT1A TO +1.                                             ECS028
02472      MOVE +1   TO INDX.                                           ECS028
02473                                                                   ECS028
02474      PERFORM 4531-LOAD-PER-BRACKET-AMT THRU 4531-EXIT 45 TIMES.   ECS028
02475                                                                   ECS028
02476      MOVE GFI-HOLD-LINE-P TO GFI-PREM-LINE (GFI).                 ECS028
02477      MOVE GFI-HOLD-LINE-E TO GFI-EARN-LINE (GFI).                 ECS028
02478      MOVE GFI-HOLD-LINE-C TO GFI-CLM-LINE  (GFI).                 ECS028
02479                                                                   ECS028
02480      SET PRT1B EADGP GFI UP BY +1.                                ECS028
02481                                                                   ECS028
02482  4530-EXIT.                                                       ECS028
02483      EXIT.                                                        ECS028
02484  EJECT                                                            ECS028
02485  4531-LOAD-PER-BRACKET-AMT.                                       ECS028
02486      IF INDX = +1                                                 ECS028
02487          MOVE +99999999999        TO AMT-1                        ECS028
02488      ELSE                                                         ECS028
02489          SUBTRACT +1 FROM INDX                                    ECS028
02490          MOVE HLD-BRKT-AMT (INDX) TO AMT-1                        ECS028
02491          ADD  +1                  TO INDX.                        ECS028
02492                                                                   ECS028
02493      MOVE HLD-BRKT-AMT (INDX) TO AMT-2.                           ECS028
02494                                                                   ECS028
02495      IF INDX = +45                                                ECS028
02496          MOVE +0                  TO AMT-2.                       ECS028
02497                                                                   ECS028
02498      IF EAD-NET-WRITTEN-PREM (EADGP) LESS AMT-1 AND GREATER AMT-2 ECS028
02499          SET  GFI-HOLD-LINE-P TO PRT1A                            ECS028
02500          MOVE NWPC TO PRT-1-MONTH (PRT1A, PRT1B).                 ECS028
02501                                                                   ECS028
02502      IF EAD-EARNED-PREM (EADGP) LESS AMT-1 AND GREATER AMT-2      ECS028
02503          SET GFI-HOLD-LINE-E TO PRT1A                             ECS028
02504          IF PRT-1-MONTH (PRT1A, PRT1B) = SPACE                    ECS028
02505              MOVE EPC      TO PRT-1-MONTH (PRT1A, PRT1B)          ECS028
02506          ELSE                                                     ECS028
02507              MOVE INTERSEC TO PRT-1-MONTH (PRT1A, PRT1B).         ECS028
02508                                                                   ECS028
02509      IF EAD-CLAIMS-PAID (EADGP) LESS AMT-1 AND GREATER AMT-2      ECS028
02510          SET GFI-HOLD-LINE-C TO PRT1A                             ECS028
02511          IF PRT-1-MONTH (PRT1A, PRT1B) = SPACE                    ECS028
02512              MOVE CPC      TO PRT-1-MONTH (PRT1A, PRT1B)          ECS028
02513          ELSE                                                     ECS028
02514              MOVE INTERSEC TO PRT-1-MONTH (PRT1A, PRT1B).         ECS028
02515                                                                   ECS028
02516      SET PRT1A UP BY +1.                                          ECS028
02517                                                                   ECS028
02518      ADD +1 TO INDX.                                              ECS028
02519                                                                   ECS028
02520  4531-EXIT.                                                       ECS028
02521      EXIT.                                                        ECS028
02522  EJECT                                                            ECS028
02523  4540-FILL-IN-GRAPH.                                              ECS028
02524      PERFORM 4541-SET-GFI-HOLD-LINES.                             ECS028
02525                                                                   ECS028
02526      IF GFI-HOLD-LINE-P     = ZEROS OR                            ECS028
02527         GFI-PREM-LINE (GFI) = ZEROS                               ECS028
02528          NEXT SENTENCE                                            ECS028
02529      ELSE                                                         ECS028
02530          SET PRT1A TO GFI-PREM-LINE (GFI)                         ECS028
02531          SUBTRACT GFI-HOLD-LINE-P FROM GFI-PREM-LINE (GFI)        ECS028
02532              GIVING GFI-LINE-DIFF-1                               ECS028
02533          IF GFI-LD1-4                                             ECS028
02534              PERFORM 5000-PREM-DIRECT-FILL-IN   THRU 5001-EXIT    ECS028
02535          ELSE                                                     ECS028
02536              PERFORM 5100-PREM-COMPUTED-FILL-IN THRU 5101-EXIT.   ECS028
02537                                                                   ECS028
02538      IF GFI-HOLD-LINE-E     = ZEROS OR                            ECS028
02539         GFI-EARN-LINE (GFI) = ZEROS                               ECS028
02540          NEXT SENTENCE                                            ECS028
02541      ELSE                                                         ECS028
02542          SET PRT1A TO GFI-EARN-LINE (GFI)                         ECS028
02543          SUBTRACT GFI-HOLD-LINE-E FROM GFI-EARN-LINE (GFI)        ECS028
02544              GIVING GFI-LINE-DIFF-1                               ECS028
02545          IF GFI-LD1-4                                             ECS028
02546              PERFORM 5200-EARN-DIRECT-FILL-IN   THRU 5201-EXIT    ECS028
02547          ELSE                                                     ECS028
02548              PERFORM 5300-EARN-COMPUTED-FILL-IN THRU 5301-EXIT.   ECS028
02549                                                                   ECS028
02550      IF GFI-HOLD-LINE-C    = ZEROS OR                             ECS028
02551         GFI-CLM-LINE (GFI) = ZEROS                                ECS028
02552          NEXT SENTENCE                                            ECS028
02553      ELSE                                                         ECS028
02554          SET PRT1A TO GFI-CLM-LINE (GFI)                          ECS028
02555          SUBTRACT GFI-HOLD-LINE-C FROM GFI-CLM-LINE (GFI)         ECS028
02556              GIVING GFI-LINE-DIFF-1                               ECS028
02557          IF GFI-LD1-4                                             ECS028
02558              PERFORM 5400-CLM-DIRECT-FILL-IN   THRU 5401-EXIT     ECS028
02559          ELSE                                                     ECS028
02560              PERFORM 5500-CLM-COMPUTED-FILL-IN THRU 5501-EXIT.    ECS028
02561                                                                   ECS028
02562      SET PRT1B GFI UP BY +1.                                      ECS028
02563                                                                   ECS028
02564  4540-EXIT.                                                       ECS028
02565      EXIT.                                                        ECS028
02566  EJECT                                                            ECS028
02567  4541-SET-GFI-HOLD-LINES.                                         ECS028
02568      SET GFI UP BY +1.                                            ECS028
02569      MOVE GFI-PREM-LINE (GFI) TO GFI-HOLD-LINE-P.                 ECS028
02570      MOVE GFI-EARN-LINE (GFI) TO GFI-HOLD-LINE-E.                 ECS028
02571      MOVE GFI-CLM-LINE  (GFI) TO GFI-HOLD-LINE-C.                 ECS028
02572      SET GFI DOWN BY +1.                                          ECS028
02573                                                                   ECS028
02574  4650-PRINT-GRAPH-LINE-1.                                         ECS028
02575      MOVE PRT-1-DATA (PRT1A) TO PRT-DATA.                         ECS028
02576      PERFORM 4900-PRINT-LINE.                                     ECS028
02577                                                                   ECS028
02578      SET PRT1A UP BY +1.                                          ECS028
02579                                                                   ECS028
02580  4651-EXIT.                                                       ECS028
02581      EXIT.                                                        ECS028
02582                                                                   ECS028
02583  4900-PRINT-LINE.                                                 ECS028
02584      MOVE PRT-CNTL TO LCP-ASA                                     ECS028
02585      PERFORM LCP-WRITE-POS-PRT                                    ECS028
02586          THRU LCP-WRITE-END-PRT.                                  ECS028
02587  EJECT                                                            ECS028
02588  5000-PREM-DIRECT-FILL-IN.                                        ECS028
02589      IF GFI-PREM-LINE (GFI) = GFI-HOLD-LINE-P                     ECS028
02590          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02591                       PRT-FILL-2 (PRT1A, PRT1B)                   ECS028
02592                       PRT-FILL-3 (PRT1A, PRT1B)                   ECS028
02593                       PRT-FILL-4 (PRT1A, PRT1B)                   ECS028
02594          GO TO 5001-EXIT.                                         ECS028
02595                                                                   ECS028
02596      IF GFI-LINE-DIFF-1 = +1 OR -1                                ECS028
02597          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02598                       PRT-FILL-2 (PRT1A, PRT1B)                   ECS028
02599          IF GFI-LINE-DIFF-1 = +1                                  ECS028
02600              SET PRT1A DOWN BY +1                                 ECS028
02601              MOVE NWPC TO PRT-FILL-3 (PRT1A, PRT1B)               ECS028
02602                           PRT-FILL-4 (PRT1A, PRT1B)               ECS028
02603              SET PRT1A UP BY +1                                   ECS028
02604              GO TO 5001-EXIT                                      ECS028
02605          ELSE                                                     ECS028
02606              SET PRT1A UP BY +1                                   ECS028
02607              MOVE NWPC TO PRT-FILL-3 (PRT1A, PRT1B)               ECS028
02608                           PRT-FILL-4 (PRT1A, PRT1B)               ECS028
02609              SET PRT1A DOWN BY +1                                 ECS028
02610              GO TO 5001-EXIT.                                     ECS028
02611                                                                   ECS028
02612      IF GFI-LINE-DIFF-1 = +2 OR -2                                ECS028
02613          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02614          IF GFI-LINE-DIFF-1 = +2                                  ECS028
02615              SET PRT1A DOWN BY +1                                 ECS028
02616              MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)               ECS028
02617                           PRT-FILL-3 (PRT1A, PRT1B)               ECS028
02618              SET PRT1A DOWN BY +1                                 ECS028
02619              MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)               ECS028
02620              SET PRT1A UP BY +2                                   ECS028
02621              GO TO 5001-EXIT                                      ECS028
02622          ELSE                                                     ECS028
02623              SET PRT1A UP BY +1                                   ECS028
02624              MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)               ECS028
02625                           PRT-FILL-3 (PRT1A, PRT1B)               ECS028
02626              SET PRT1A UP BY +1                                   ECS028
02627              MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)               ECS028
02628              SET PRT1A DOWN BY +2                                 ECS028
02629              GO TO 5001-EXIT.                                     ECS028
02630  EJECT                                                            ECS028
02631      IF GFI-LINE-DIFF-1 = +3 OR -3                                ECS028
02632          IF GFI-LINE-DIFF-1 = +3                                  ECS028
02633              SET PRT1A DOWN BY +1                                 ECS028
02634              MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)               ECS028
02635              SET PRT1A DOWN BY +1                                 ECS028
02636              MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)               ECS028
02637                           PRT-FILL-3 (PRT1A, PRT1B)               ECS028
02638              SET PRT1A DOWN BY +1                                 ECS028
02639              MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)               ECS028
02640              SET PRT1A UP BY +3                                   ECS028
02641              GO TO 5001-EXIT                                      ECS028
02642          ELSE                                                     ECS028
02643              SET PRT1A UP BY +1                                   ECS028
02644              MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)               ECS028
02645              SET PRT1A UP BY +1                                   ECS028
02646              MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)               ECS028
02647                           PRT-FILL-3 (PRT1A, PRT1B)               ECS028
02648              SET PRT1A UP BY +1                                   ECS028
02649              MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)               ECS028
02650              SET PRT1A DOWN BY +3                                 ECS028
02651              GO TO 5001-EXIT.                                     ECS028
02652                                                                   ECS028
02653 * IF IT GETS TO HERE, THE DIFFERENCE IS + OR -4.                  ECS028
02654                                                                   ECS028
02655      IF GFI-LINE-DIFF-1 = +4                                      ECS028
02656          SET PRT1A DOWN BY +1                                     ECS028
02657          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02658          SET PRT1A DOWN BY +1                                     ECS028
02659          MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)                   ECS028
02660          SET PRT1A DOWN BY +1                                     ECS028
02661          MOVE NWPC TO PRT-FILL-3 (PRT1A, PRT1B)                   ECS028
02662          SET PRT1A DOWN BY +1                                     ECS028
02663          MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)                   ECS028
02664          SET PRT1A UP BY +4                                       ECS028
02665      ELSE                                                         ECS028
02666          SET PRT1A UP BY +1                                       ECS028
02667          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02668          SET PRT1A UP BY +1                                       ECS028
02669          MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)                   ECS028
02670          SET PRT1A UP BY +1                                       ECS028
02671          MOVE NWPC TO PRT-FILL-3 (PRT1A, PRT1B)                   ECS028
02672          SET PRT1A UP BY +1                                       ECS028
02673          MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)                   ECS028
02674          SET PRT1A DOWN BY +4.                                    ECS028
02675                                                                   ECS028
02676  5001-EXIT.                                                       ECS028
02677      EXIT.                                                        ECS028
02678  EJECT                                                            ECS028
02679  5100-PREM-COMPUTED-FILL-IN.                                      ECS028
02680      DIVIDE GFI-LINE-DIFF-1 BY +4 GIVING GFI-LINE-DIFF-2.         ECS028
02681                                                                   ECS028
02682      IF GFI-LINE-DIFF-1 GREATER THAN ZERO                         ECS028
02683          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02684          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02685          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02686          MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)                   ECS028
02687          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02688          MOVE NWPC TO PRT-FILL-3 (PRT1A, PRT1B)                   ECS028
02689          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02690          MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)                   ECS028
02691          MULTIPLY GFI-LINE-DIFF-2 BY FOUR                         ECS028
02692          SET PRT1A UP BY GFI-LINE-DIFF-2.                         ECS028
02693                                                                   ECS028
02694      IF GFI-LINE-DIFF-1 LESS THAN ZERO                            ECS028
02695          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02696          MOVE NWPC TO PRT-FILL-1 (PRT1A, PRT1B)                   ECS028
02697          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02698          MOVE NWPC TO PRT-FILL-2 (PRT1A, PRT1B)                   ECS028
02699          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02700          MOVE NWPC TO PRT-FILL-3 (PRT1A, PRT1B)                   ECS028
02701          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02702          MOVE NWPC TO PRT-FILL-4 (PRT1A, PRT1B)                   ECS028
02703          MULTIPLY GFI-LINE-DIFF-2 BY FOUR                         ECS028
02704          SET PRT1A DOWN BY GFI-LINE-DIFF-2.                       ECS028
02705                                                                   ECS028
02706  5101-EXIT.                                                       ECS028
02707      EXIT.                                                        ECS028
02708  EJECT                                                            ECS028
02709  5200-EARN-DIRECT-FILL-IN.                                        ECS028
02710      IF GFI-EARN-LINE (GFI) = GFI-HOLD-LINE-E                     ECS028
02711          MOVE EPC TO FILL-CHAR                                    ECS028
02712          PERFORM 5610-FILL1-CHAR                                  ECS028
02713          MOVE EPC TO FILL-CHAR                                    ECS028
02714          PERFORM 5620-FILL2-CHAR                                  ECS028
02715          MOVE EPC TO FILL-CHAR                                    ECS028
02716          PERFORM 5630-FILL3-CHAR                                  ECS028
02717          MOVE EPC TO FILL-CHAR                                    ECS028
02718          PERFORM 5640-FILL4-CHAR                                  ECS028
02719          GO TO 5201-EXIT.                                         ECS028
02720                                                                   ECS028
02721      IF GFI-LINE-DIFF-1 = +1 OR -1                                ECS028
02722          MOVE EPC TO FILL-CHAR                                    ECS028
02723          PERFORM 5610-FILL1-CHAR                                  ECS028
02724          MOVE EPC TO FILL-CHAR                                    ECS028
02725          PERFORM 5620-FILL2-CHAR                                  ECS028
02726          IF GFI-LINE-DIFF-1 = +1                                  ECS028
02727              SET PRT1A DOWN BY +1                                 ECS028
02728              MOVE EPC TO FILL-CHAR                                ECS028
02729              PERFORM 5630-FILL3-CHAR                              ECS028
02730              MOVE EPC TO FILL-CHAR                                ECS028
02731              PERFORM 5640-FILL4-CHAR                              ECS028
02732              SET PRT1A UP BY +1                                   ECS028
02733              GO TO 5201-EXIT                                      ECS028
02734          ELSE                                                     ECS028
02735              SET PRT1A UP BY +1                                   ECS028
02736              MOVE EPC TO FILL-CHAR                                ECS028
02737              PERFORM 5630-FILL3-CHAR                              ECS028
02738              MOVE EPC TO FILL-CHAR                                ECS028
02739              PERFORM 5640-FILL4-CHAR                              ECS028
02740              SET PRT1A DOWN BY +1                                 ECS028
02741              GO TO 5201-EXIT.                                     ECS028
02742                                                                   ECS028
02743      IF GFI-LINE-DIFF-1 = +2 OR -2                                ECS028
02744          MOVE EPC TO FILL-CHAR                                    ECS028
02745          PERFORM 5610-FILL1-CHAR                                  ECS028
02746          IF GFI-LINE-DIFF-1 = +2                                  ECS028
02747              SET PRT1A DOWN BY +1                                 ECS028
02748              MOVE EPC TO FILL-CHAR                                ECS028
02749              PERFORM 5620-FILL2-CHAR                              ECS028
02750              MOVE EPC TO FILL-CHAR                                ECS028
02751              PERFORM 5630-FILL3-CHAR                              ECS028
02752              SET PRT1A DOWN BY +1                                 ECS028
02753              MOVE EPC TO FILL-CHAR                                ECS028
02754              PERFORM 5640-FILL4-CHAR                              ECS028
02755              SET PRT1A UP BY +2                                   ECS028
02756              GO TO 5201-EXIT                                      ECS028
02757          ELSE                                                     ECS028
02758              SET PRT1A UP BY +1                                   ECS028
02759              MOVE EPC TO FILL-CHAR                                ECS028
02760              PERFORM 5620-FILL2-CHAR                              ECS028
02761              MOVE EPC TO FILL-CHAR                                ECS028
02762              PERFORM 5630-FILL3-CHAR                              ECS028
02763              SET PRT1A UP BY +1                                   ECS028
02764              MOVE EPC TO FILL-CHAR                                ECS028
02765              PERFORM 5640-FILL4-CHAR                              ECS028
02766              SET PRT1A DOWN BY +2                                 ECS028
02767              GO TO 5201-EXIT.                                     ECS028
02768                                                                   ECS028
02769      IF GFI-LINE-DIFF-1 = +3 OR -3                                ECS028
02770          IF GFI-LINE-DIFF-1 = +3                                  ECS028
02771              SET PRT1A DOWN BY +1                                 ECS028
02772              MOVE EPC TO FILL-CHAR                                ECS028
02773              PERFORM 5610-FILL1-CHAR                              ECS028
02774              SET PRT1A DOWN BY +1                                 ECS028
02775              MOVE EPC TO FILL-CHAR                                ECS028
02776              PERFORM 5620-FILL2-CHAR                              ECS028
02777              MOVE EPC TO FILL-CHAR                                ECS028
02778              PERFORM 5630-FILL3-CHAR                              ECS028
02779              SET PRT1A DOWN BY +1                                 ECS028
02780              MOVE EPC TO FILL-CHAR                                ECS028
02781              PERFORM 5640-FILL4-CHAR                              ECS028
02782              SET PRT1A UP BY +3                                   ECS028
02783              GO TO 5201-EXIT                                      ECS028
02784          ELSE                                                     ECS028
02785              SET PRT1A UP BY +1                                   ECS028
02786              MOVE EPC TO FILL-CHAR                                ECS028
02787              PERFORM 5610-FILL1-CHAR                              ECS028
02788              SET PRT1A UP BY +1                                   ECS028
02789              MOVE EPC TO FILL-CHAR                                ECS028
02790              PERFORM 5620-FILL2-CHAR                              ECS028
02791              MOVE EPC TO FILL-CHAR                                ECS028
02792              PERFORM 5630-FILL3-CHAR                              ECS028
02793              SET PRT1A UP BY +1                                   ECS028
02794              MOVE EPC TO FILL-CHAR                                ECS028
02795              PERFORM 5640-FILL4-CHAR                              ECS028
02796              SET PRT1A DOWN BY +3                                 ECS028
02797              GO TO 5201-EXIT.                                     ECS028
02798  EJECT                                                            ECS028
02799 * IF IT GETS TO HERE, THE DIFFERENCE IS + OR -4.                  ECS028
02800                                                                   ECS028
02801      IF GFI-LINE-DIFF-1 = +4                                      ECS028
02802          SET PRT1A DOWN BY +1                                     ECS028
02803          MOVE EPC TO FILL-CHAR                                    ECS028
02804          PERFORM 5610-FILL1-CHAR                                  ECS028
02805          SET PRT1A DOWN BY +1                                     ECS028
02806          MOVE EPC TO FILL-CHAR                                    ECS028
02807          PERFORM 5620-FILL2-CHAR                                  ECS028
02808          SET PRT1A DOWN BY +1                                     ECS028
02809          MOVE EPC TO FILL-CHAR                                    ECS028
02810          PERFORM 5630-FILL3-CHAR                                  ECS028
02811          SET PRT1A DOWN BY +1                                     ECS028
02812          MOVE EPC TO FILL-CHAR                                    ECS028
02813          PERFORM 5640-FILL4-CHAR                                  ECS028
02814          SET PRT1A UP BY +4                                       ECS028
02815      ELSE                                                         ECS028
02816          SET PRT1A UP BY +1                                       ECS028
02817          MOVE EPC TO FILL-CHAR                                    ECS028
02818          PERFORM 5610-FILL1-CHAR                                  ECS028
02819          SET PRT1A UP BY +1                                       ECS028
02820          MOVE EPC TO FILL-CHAR                                    ECS028
02821          PERFORM 5620-FILL2-CHAR                                  ECS028
02822          SET PRT1A UP BY +1                                       ECS028
02823          MOVE EPC TO FILL-CHAR                                    ECS028
02824          PERFORM 5630-FILL3-CHAR                                  ECS028
02825          SET PRT1A UP BY +1                                       ECS028
02826          MOVE EPC TO FILL-CHAR                                    ECS028
02827          PERFORM 5640-FILL4-CHAR                                  ECS028
02828          SET PRT1A DOWN BY +4.                                    ECS028
02829                                                                   ECS028
02830  5201-EXIT.                                                       ECS028
02831      EXIT.                                                        ECS028
02832  EJECT                                                            ECS028
02833  5300-EARN-COMPUTED-FILL-IN.                                      ECS028
02834      DIVIDE GFI-LINE-DIFF-1 BY +4 GIVING GFI-LINE-DIFF-2.         ECS028
02835                                                                   ECS028
02836      IF GFI-LINE-DIFF-1 GREATER THAN ZERO                         ECS028
02837          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02838          MOVE EPC TO FILL-CHAR                                    ECS028
02839          PERFORM 5610-FILL1-CHAR                                  ECS028
02840          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02841          MOVE EPC TO FILL-CHAR                                    ECS028
02842          PERFORM 5620-FILL2-CHAR                                  ECS028
02843          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02844          MOVE EPC TO FILL-CHAR                                    ECS028
02845          PERFORM 5630-FILL3-CHAR                                  ECS028
02846          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
02847          MOVE EPC TO FILL-CHAR                                    ECS028
02848          PERFORM 5640-FILL4-CHAR                                  ECS028
02849          MULTIPLY GFI-LINE-DIFF-2 BY FOUR                         ECS028
02850          SET PRT1A UP BY GFI-LINE-DIFF-2.                         ECS028
02851                                                                   ECS028
02852      IF GFI-LINE-DIFF-1 LESS THAN ZERO                            ECS028
02853          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02854          MOVE EPC TO FILL-CHAR                                    ECS028
02855          PERFORM 5610-FILL1-CHAR                                  ECS028
02856          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02857          MOVE EPC TO FILL-CHAR                                    ECS028
02858          PERFORM 5620-FILL2-CHAR                                  ECS028
02859          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02860          MOVE EPC TO FILL-CHAR                                    ECS028
02861          PERFORM 5630-FILL3-CHAR                                  ECS028
02862          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
02863          MOVE EPC TO FILL-CHAR                                    ECS028
02864          PERFORM 5640-FILL4-CHAR                                  ECS028
02865          MULTIPLY GFI-LINE-DIFF-2 BY FOUR                         ECS028
02866          SET PRT1A DOWN BY GFI-LINE-DIFF-2.                       ECS028
02867                                                                   ECS028
02868  5301-EXIT.                                                       ECS028
02869      EXIT.                                                        ECS028
02870  EJECT                                                            ECS028
02871  5400-CLM-DIRECT-FILL-IN.                                         ECS028
02872      IF GFI-CLM-LINE (GFI) = GFI-HOLD-LINE-C                      ECS028
02873          MOVE CPC TO FILL-CHAR                                    ECS028
02874          PERFORM 5610-FILL1-CHAR                                  ECS028
02875          MOVE CPC TO FILL-CHAR                                    ECS028
02876          PERFORM 5620-FILL2-CHAR                                  ECS028
02877          MOVE CPC TO FILL-CHAR                                    ECS028
02878          PERFORM 5630-FILL3-CHAR                                  ECS028
02879          MOVE CPC TO FILL-CHAR                                    ECS028
02880          PERFORM 5640-FILL4-CHAR                                  ECS028
02881          GO TO 5401-EXIT.                                         ECS028
02882                                                                   ECS028
02883      IF GFI-LINE-DIFF-1 = +1 OR -1                                ECS028
02884          MOVE CPC TO FILL-CHAR                                    ECS028
02885          PERFORM 5610-FILL1-CHAR                                  ECS028
02886          MOVE CPC TO FILL-CHAR                                    ECS028
02887          PERFORM 5620-FILL2-CHAR                                  ECS028
02888          IF GFI-LINE-DIFF-1 = +1                                  ECS028
02889              SET PRT1A DOWN BY +1                                 ECS028
02890              MOVE CPC TO FILL-CHAR                                ECS028
02891              PERFORM 5630-FILL3-CHAR                              ECS028
02892              MOVE CPC TO FILL-CHAR                                ECS028
02893              PERFORM 5640-FILL4-CHAR                              ECS028
02894              SET PRT1A UP BY +1                                   ECS028
02895              GO TO 5401-EXIT                                      ECS028
02896          ELSE                                                     ECS028
02897              SET PRT1A UP BY +1                                   ECS028
02898              MOVE CPC TO FILL-CHAR                                ECS028
02899              PERFORM 5630-FILL3-CHAR                              ECS028
02900              MOVE CPC TO FILL-CHAR                                ECS028
02901              PERFORM 5640-FILL4-CHAR                              ECS028
02902              SET PRT1A DOWN BY +1                                 ECS028
02903              GO TO 5401-EXIT.                                     ECS028
02904                                                                   ECS028
02905  EJECT                                                            ECS028
02906      IF GFI-LINE-DIFF-1 = +2 OR -2                                ECS028
02907          MOVE CPC TO FILL-CHAR                                    ECS028
02908          PERFORM 5610-FILL1-CHAR                                  ECS028
02909          IF GFI-LINE-DIFF-1 = +2                                  ECS028
02910              SET PRT1A DOWN BY +1                                 ECS028
02911              MOVE CPC TO FILL-CHAR                                ECS028
02912              PERFORM 5620-FILL2-CHAR                              ECS028
02913              MOVE CPC TO FILL-CHAR                                ECS028
02914              PERFORM 5630-FILL3-CHAR                              ECS028
02915              SET PRT1A DOWN BY +1                                 ECS028
02916              MOVE CPC TO FILL-CHAR                                ECS028
02917              PERFORM 5640-FILL4-CHAR                              ECS028
02918              SET PRT1A UP BY +2                                   ECS028
02919              GO TO 5401-EXIT                                      ECS028
02920          ELSE                                                     ECS028
02921              SET PRT1A UP BY +1                                   ECS028
02922              MOVE CPC TO FILL-CHAR                                ECS028
02923              PERFORM 5620-FILL2-CHAR                              ECS028
02924              MOVE CPC TO FILL-CHAR                                ECS028
02925              PERFORM 5630-FILL3-CHAR                              ECS028
02926              SET PRT1A UP BY +1                                   ECS028
02927              MOVE CPC TO FILL-CHAR                                ECS028
02928              PERFORM 5640-FILL4-CHAR                              ECS028
02929              SET PRT1A DOWN BY +2                                 ECS028
02930              GO TO 5401-EXIT.                                     ECS028
02931  EJECT                                                            ECS028
02932      IF GFI-LINE-DIFF-1 = +3 OR -3                                ECS028
02933          IF GFI-LINE-DIFF-1 = +3                                  ECS028
02934              SET PRT1A DOWN BY +1                                 ECS028
02935              MOVE CPC TO FILL-CHAR                                ECS028
02936              PERFORM 5610-FILL1-CHAR                              ECS028
02937              SET PRT1A DOWN BY +1                                 ECS028
02938              MOVE CPC TO FILL-CHAR                                ECS028
02939              PERFORM 5620-FILL2-CHAR                              ECS028
02940              MOVE CPC TO FILL-CHAR                                ECS028
02941              PERFORM 5630-FILL3-CHAR                              ECS028
02942              SET PRT1A DOWN BY +1                                 ECS028
02943              MOVE CPC TO FILL-CHAR                                ECS028
02944              PERFORM 5640-FILL4-CHAR                              ECS028
02945              SET PRT1A UP BY +3                                   ECS028
02946              GO TO 5401-EXIT                                      ECS028
02947          ELSE                                                     ECS028
02948              SET PRT1A UP BY +1                                   ECS028
02949              MOVE CPC TO FILL-CHAR                                ECS028
02950              PERFORM 5610-FILL1-CHAR                              ECS028
02951              SET PRT1A UP BY +1                                   ECS028
02952              MOVE CPC TO FILL-CHAR                                ECS028
02953              PERFORM 5620-FILL2-CHAR                              ECS028
02954              MOVE CPC TO FILL-CHAR                                ECS028
02955              PERFORM 5630-FILL3-CHAR                              ECS028
02956              SET PRT1A UP BY +1                                   ECS028
02957              MOVE CPC TO FILL-CHAR                                ECS028
02958              PERFORM 5640-FILL4-CHAR                              ECS028
02959              SET PRT1A DOWN BY +3                                 ECS028
02960              GO TO 5401-EXIT.                                     ECS028
02961                                                                   ECS028
02962  EJECT                                                            ECS028
02963 * IF IT GETS HERE, THE DIFFERENCE IS + OR -4.                     ECS028
02964                                                                   ECS028
02965      IF GFI-LINE-DIFF-1 = +4                                      ECS028
02966          SET PRT1A DOWN BY +1                                     ECS028
02967          MOVE CPC TO FILL-CHAR                                    ECS028
02968          PERFORM 5610-FILL1-CHAR                                  ECS028
02969          SET PRT1A DOWN BY +1                                     ECS028
02970          MOVE CPC TO FILL-CHAR                                    ECS028
02971          PERFORM 5620-FILL2-CHAR                                  ECS028
02972          SET PRT1A DOWN BY +1                                     ECS028
02973          MOVE CPC TO FILL-CHAR                                    ECS028
02974          PERFORM 5630-FILL3-CHAR                                  ECS028
02975          SET PRT1A DOWN BY +1                                     ECS028
02976          MOVE CPC TO FILL-CHAR                                    ECS028
02977          PERFORM 5640-FILL4-CHAR                                  ECS028
02978          SET PRT1A UP BY +4                                       ECS028
02979      ELSE                                                         ECS028
02980          SET PRT1A UP BY +1                                       ECS028
02981          MOVE CPC TO FILL-CHAR                                    ECS028
02982          PERFORM 5610-FILL1-CHAR                                  ECS028
02983          SET PRT1A UP BY +1                                       ECS028
02984          MOVE CPC TO FILL-CHAR                                    ECS028
02985          PERFORM 5620-FILL2-CHAR                                  ECS028
02986          SET PRT1A UP BY +1                                       ECS028
02987          MOVE CPC TO FILL-CHAR                                    ECS028
02988          PERFORM 5630-FILL3-CHAR                                  ECS028
02989          SET PRT1A UP BY +1                                       ECS028
02990          MOVE CPC TO FILL-CHAR                                    ECS028
02991          PERFORM 5640-FILL4-CHAR                                  ECS028
02992          SET PRT1A DOWN BY +4.                                    ECS028
02993                                                                   ECS028
02994  5401-EXIT.                                                       ECS028
02995      EXIT.                                                        ECS028
02996  EJECT                                                            ECS028
02997  5500-CLM-COMPUTED-FILL-IN.                                       ECS028
02998      DIVIDE GFI-LINE-DIFF-1 BY +4 GIVING GFI-LINE-DIFF-2.         ECS028
02999                                                                   ECS028
03000      IF GFI-LINE-DIFF-1 GREATER THAN ZERO                         ECS028
03001          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
03002          MOVE CPC TO FILL-CHAR                                    ECS028
03003          PERFORM 5610-FILL1-CHAR                                  ECS028
03004          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
03005          MOVE CPC TO FILL-CHAR                                    ECS028
03006          PERFORM 5620-FILL2-CHAR                                  ECS028
03007          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
03008          MOVE CPC TO FILL-CHAR                                    ECS028
03009          PERFORM 5630-FILL3-CHAR                                  ECS028
03010          SET PRT1A DOWN BY GFI-LINE-DIFF-2                        ECS028
03011          MOVE CPC TO FILL-CHAR                                    ECS028
03012          PERFORM 5640-FILL4-CHAR                                  ECS028
03013          MULTIPLY GFI-LINE-DIFF-2 BY FOUR                         ECS028
03014          SET PRT1A UP BY GFI-LINE-DIFF-2.                         ECS028
03015                                                                   ECS028
03016      IF GFI-LINE-DIFF-1 LESS THAN ZERO                            ECS028
03017          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
03018          MOVE CPC TO FILL-CHAR                                    ECS028
03019          PERFORM 5610-FILL1-CHAR                                  ECS028
03020          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
03021          MOVE CPC TO FILL-CHAR                                    ECS028
03022          PERFORM 5620-FILL2-CHAR                                  ECS028
03023          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
03024          MOVE CPC TO FILL-CHAR                                    ECS028
03025          PERFORM 5630-FILL3-CHAR                                  ECS028
03026          SET PRT1A UP BY GFI-LINE-DIFF-2                          ECS028
03027          MOVE CPC TO FILL-CHAR                                    ECS028
03028          PERFORM 5640-FILL4-CHAR                                  ECS028
03029          MULTIPLY GFI-LINE-DIFF-2 BY FOUR                         ECS028
03030          SET PRT1A DOWN BY GFI-LINE-DIFF-2.                       ECS028
03031                                                                   ECS028
03032  5501-EXIT.                                                       ECS028
03033      EXIT.                                                        ECS028
03034  EJECT                                                            ECS028
03035  5610-FILL1-CHAR.                                                 ECS028
03036      IF PRT-FILL-1 (PRT1A, PRT1B) NOT = SPACE                     ECS028
03037          MOVE INTERSEC TO FILL-CHAR.                              ECS028
03038                                                                   ECS028
03039      MOVE FILL-CHAR TO PRT-FILL-1 (PRT1A, PRT1B).                 ECS028
03040                                                                   ECS028
03041  5620-FILL2-CHAR.                                                 ECS028
03042      IF PRT-FILL-2 (PRT1A, PRT1B) NOT = SPACE                     ECS028
03043          MOVE INTERSEC TO FILL-CHAR.                              ECS028
03044                                                                   ECS028
03045      MOVE FILL-CHAR TO PRT-FILL-2 (PRT1A, PRT1B).                 ECS028
03046                                                                   ECS028
03047  5630-FILL3-CHAR.                                                 ECS028
03048      IF PRT-FILL-3 (PRT1A, PRT1B) NOT = SPACE                     ECS028
03049          MOVE INTERSEC TO FILL-CHAR.                              ECS028
03050                                                                   ECS028
03051      MOVE FILL-CHAR TO PRT-FILL-3 (PRT1A, PRT1B).                 ECS028
03052                                                                   ECS028
03053  5640-FILL4-CHAR.                                                 ECS028
03054      IF PRT-FILL-4 (PRT1A, PRT1B) NOT = SPACE                     ECS028
03055          MOVE INTERSEC TO FILL-CHAR.                              ECS028
03056                                                                   ECS028
03057      MOVE FILL-CHAR TO PRT-FILL-4 (PRT1A, PRT1B).                 ECS028
03058                                                                   ECS028
03059  ABEND-PGM       SECTION.                                         ECS028
03060                             COPY ELCABEND.                        ECS028
03061 /                                                                 ECS028
03062  LCP-WRITE-POS-PRT SECTION.                                       ECS028
03063      IF DTE-FICH NOT = SPACE                                      ECS028
03064          MOVE LCP-ASA            TO  PRT-CNTL                     ECS028
03065          WRITE FICH-REC FROM PRT.                                 ECS028
03066                                                                   ECS028
03067      IF DTE-FICH = SPACE OR '2'                                   ECS028
03068          NEXT SENTENCE                                            ECS028
03069         ELSE                                                      ECS028
03070          GO TO LCP-WRITE-END-PRT.                                 ECS028
03071                                                                   ECS028
03072      IF LCP-ASA = '+'                                             ECS028
03073          WRITE PRT AFTER 0 LINE                                   ECS028
03074      ELSE                                                         ECS028
03075      IF LCP-ASA = ' '                                             ECS028
03076          WRITE PRT AFTER ADVANCING 1 LINE                         ECS028
03077      ELSE                                                         ECS028
03078      IF LCP-ASA = '0'                                             ECS028
03079          WRITE PRT AFTER ADVANCING 2 LINE                         ECS028
03080      ELSE                                                         ECS028
03081      IF LCP-ASA = '-'                                             ECS028
03082          WRITE PRT AFTER ADVANCING 3 LINE                         ECS028
03083      ELSE                                                         ECS028
03084      IF LCP-ASA = '1'                                             ECS028
03085          WRITE PRT AFTER ADVANCING PAGE                           ECS028
03086      ELSE                                                         ECS028
03087      IF LCP-ASA = '2'                                             ECS028
03088          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS028
03089      ELSE                                                         ECS028
03090      IF LCP-ASA = '3'                                             ECS028
03091          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS028
03092      ELSE                                                         ECS028
03093      IF LCP-ASA = '4'                                             ECS028
03094          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS028
03095      ELSE                                                         ECS028
03096      IF LCP-ASA = '5'                                             ECS028
03097          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS028
03098      ELSE                                                         ECS028
03099      IF LCP-ASA = '6'                                             ECS028
03100          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS028
03101      ELSE                                                         ECS028
03102      IF LCP-ASA = '7'                                             ECS028
03103          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS028
03104      ELSE                                                         ECS028
03105      IF LCP-ASA = '8'                                             ECS028
03106          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS028
03107      ELSE                                                         ECS028
03108      IF LCP-ASA = '9'                                             ECS028
03109          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS028
03110      ELSE                                                         ECS028
03111      IF LCP-ASA = 'A'                                             ECS028
03112          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS028
03113      ELSE                                                         ECS028
03114      IF LCP-ASA = 'B'                                             ECS028
03115          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS028
03116      ELSE                                                         ECS028
03117      IF LCP-ASA = 'C'                                             ECS028
03118          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS028
03119      ELSE                                                         ECS028
03120      IF LCP-ASA = 'V'                                             ECS028
03121          WRITE PRT AFTER ADVANCING LCP-P01                        ECS028
03122      ELSE                                                         ECS028
03123      IF LCP-ASA = 'W'                                             ECS028
03124          WRITE PRT AFTER ADVANCING LCP-P02                        ECS028
03125      ELSE                                                         ECS028
03126      DISPLAY 'ASA CODE ERROR'.                                    ECS028
03127                                                                   ECS028
03128  LCP-WRITE-END-PRT.                                               ECS028
03129      EXIT.                                                        ECS028
