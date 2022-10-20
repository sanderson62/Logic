00001  IDENTIFICATION DIVISION.
00002
00002
00003  PROGRAM-ID.                 ELRESV.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 16:22:23.
00007 *                            VMOD=2.007
00008 *
00009 *AUTHOR.     LOGIC, INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.
00025
00026 *        THIS SUBROUTINE IS USED TO CALCULATE FUTURE (ACCRUED),
00027 *    PAY-TO-CURRENT, AND I.B.N.R. LOSS RESERVES FOR BOTH LIFE AND
00028 *    A/H.
00029
00030      EJECT
00031  ENVIRONMENT DIVISION.
00032
00033  DATA DIVISION.
00034
00035  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00036  77  FILLER   PIC X(32) VALUE '********************************'.
00037  77  FILLER   PIC X(32) VALUE '**  ELRESV  WORKING STORAGE   **'.
00038  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.007 *********'.
00039
00040 *                           COPY ELCRESW1.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRESW1.                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   DESCRIPTION:  WORKING STORAGE AREA FOR RESERVE CALCULATION   *
00008 *                 ROUTINE                                        *
00009 ******************************************************************
00010
00011  01  WS-WORK-AREAS                   COMP-3.
00012      12  WS-MONTHS-DISABLED          PIC S9(5)   VALUE ZERO.
00013      12  WS-DISABILITY-TABLE         PIC S9      VALUE ZERO.
00014      12  WS-DISABLED-AGE             PIC S9(3)   VALUE ZERO.
00015      12  WS-EXPIRE-AGE               PIC S9(3)   VALUE ZERO.
00016      12  WS-REMAINING-TERM           PIC S9(3)   VALUE ZERO.
00017
00018      12  WS-ELAPSED-MONTHS           PIC S9(5)   VALUE ZERO.
00019      12  WS-ODD-DAYS-OVER            PIC S9(3)   VALUE ZERO.
00020      12  WS-DAYS-IN-MONTH            PIC S9(3)   VALUE ZERO.
00021
00022      12  WS-VALUATION-DAY            PIC S9(3)   VALUE ZERO.
00023      12  WS-PAID-THRU-DAY            PIC S9(3)   VALUE ZERO.
00024
00025      12  AGE-INDEX               PIC S9(3)V9(4)  VALUE ZERO.
00026      12  AGE-INDEX2              PIC S9(3)       VALUE ZERO.
00027      12  AGE-INDEX-LOW           PIC S9(3)       VALUE ZERO.
00028      12  AGE-INDEX-HIGH          PIC S9(3)       VALUE ZERO.
00029      12  AGE-INDEX-DIFF          PIC SV9(5)      VALUE ZERO.
00030      12  MONTH-INDEX             PIC S9(3)V9(4)  VALUE ZERO.
00031      12  YEAR-INDEX              REDEFINES
00032          MONTH-INDEX             PIC S9(3)V9(4).
00033      12  TERM-INDEX              REDEFINES
00034          MONTH-INDEX             PIC S9(3)V9(4).
00035      12  TERM-INDEX2             PIC S9(3)       VALUE ZERO.
00036      12  TERM-INDEX-LOW          PIC S9(3)       VALUE ZERO.
00037      12  TERM-INDEX-HIGH         PIC S9(3)       VALUE ZERO.
00038      12  TERM-INDEX-DIFF         PIC SV9(5)      VALUE ZERO.
00039
00040      12  FACTOR-A                PIC S9(5)V9(6)  VALUE ZERO.
00041      12  FACTOR-B                PIC S9(5)V9(6)  VALUE ZERO.
00042      12  FACTOR-V                PIC S9(5)V9(5)  VALUE ZERO.
00043      12  RESERVE-FACTOR          PIC S9(5)V9(6)  VALUE ZERO.
00044      12  FACTOR-INDEX            PIC S9(3)       VALUE ZERO.
00045
00046  01  WS-INITIAL-WORK-AREAS       PIC X(100) VALUE LOW-VALUES.
00047
00048  01  FILLER.
00049      12  LCP-ONCTR-01            PIC S9(8) COMP-3 VALUE ZERO.
00050      12  WS-VALUATION-DATE-2     PIC XX  VALUE LOW-VALUES.
00051      12  WS-VALUATION-DATE-3     PIC XX  VALUE LOW-VALUES.
00052      12  WS-EXPIRE-DATE          PIC XX  VALUE LOW-VALUES.
00053
00054      12  WS-DATE-CONVERSION-PROGRAM  PIC X(8) VALUE 'ELDATCV'.
00055
00056      12  WS-DATE-WORK.
00057          16  WS-DW-MONTH             PIC 99.
00058          16  FILLER                  PIC X.
00059          16  WS-DW-DAY               PIC 99.
00060          16  FILLER                  PIC X.
00061          16  WS-DW-CCYR              PIC 9(4).
00062
00063      12  WS-WORK-CYMD.
00064          16  WS-WK-CCYR              PIC 9(4).
00065          16  WS-WK-MONTH             PIC 99.
00066          16  WS-WK-DAY               PIC 99.
00067
00068      12  POS-RESERVE-FACTORS.
00069          16  FILLER                  PIC X(30)
00070                        VALUE '966669455339273230194170150137'.
00071          16  FILLER                  PIC X(30)
00072                        VALUE '127115105098092088082076073068'.
00073          16  FILLER                  PIC X(30)
00074                        VALUE '061056052047043037035031028023'.
00075          16  FILLER                  PIC X(30)
00076                        VALUE '023020018016016015013012012011'.
00077
00078      12  POS-RESERVE-FACTOR-TABLE REDEFINES POS-RESERVE-FACTORS.
00079          16  POS-FACTOR-TABLE-ENTRIES  OCCURS 40 TIMES.
00080              20  POS-FACTOR          PIC SV999.
00081
00082      12  TABLE-WORK-AREA             COMP-3.
00083        16  TWA-MONTHS   OCCURS 42 TIMES  INDEXED BY TWA-INDEX1.
00084          20  TWA-AGE  OCCURS 11 TIMES  INDEXED BY TWA-INDEX2.
00085              24  TWA-FACTOR          PIC S9(5).
00086
00041
00042 *                           COPY ELC64CDT SUPPRESS.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC64CDT.                          *
00004 *                            VMOD=2.001                         *
00005 *
00006 *        1964 COMM. DISABILITY TABLE AT 3% INTEREST.            *
00002 *                                                               *
00007 *****************************************************************.
00008
00009  01  FILLER                          COMP-3.
00010      05  TAB-01-VALUES.
00011          10  FILLER.
00012              15  FILLER              PIC S9(5)   VALUE +00035.
00013              15  FILLER              PIC S9(5)   VALUE +00035.
00014              15  FILLER              PIC S9(5)   VALUE +00036.
00015              15  FILLER              PIC S9(5)   VALUE +00036.
00016              15  FILLER              PIC S9(5)   VALUE +00037.
00017              15  FILLER              PIC S9(5)   VALUE +00038.
00018              15  FILLER              PIC S9(5)   VALUE +00039.
00019              15  FILLER              PIC S9(5)   VALUE +00040.
00020              15  FILLER              PIC S9(5)   VALUE +00041.
00021              15  FILLER              PIC S9(5)   VALUE +00042.
00022              15  FILLER              PIC S9(5)   VALUE +00044.
00023
00024          10  FILLER.
00025              15  FILLER              PIC S9(5)   VALUE +00066.
00026              15  FILLER              PIC S9(5)   VALUE +00066.
00027              15  FILLER              PIC S9(5)   VALUE +00067.
00028              15  FILLER              PIC S9(5)   VALUE +00070.
00029              15  FILLER              PIC S9(5)   VALUE +00073.
00030              15  FILLER              PIC S9(5)   VALUE +00078.
00031              15  FILLER              PIC S9(5)   VALUE +00082.
00032              15  FILLER              PIC S9(5)   VALUE +00087.
00033              15  FILLER              PIC S9(5)   VALUE +00092.
00034              15  FILLER              PIC S9(5)   VALUE +00098.
00035              15  FILLER              PIC S9(5)   VALUE +00106.
00036
00037          10  FILLER.
00038              15  FILLER              PIC S9(5)   VALUE +00078.
00039              15  FILLER              PIC S9(5)   VALUE +00078.
00040              15  FILLER              PIC S9(5)   VALUE +00080.
00041              15  FILLER              PIC S9(5)   VALUE +00084.
00042              15  FILLER              PIC S9(5)   VALUE +00090.
00043              15  FILLER              PIC S9(5)   VALUE +00097.
00044              15  FILLER              PIC S9(5)   VALUE +00104.
00045              15  FILLER              PIC S9(5)   VALUE +00113.
00046              15  FILLER              PIC S9(5)   VALUE +00124.
00047              15  FILLER              PIC S9(5)   VALUE +00135.
00048              15  FILLER              PIC S9(5)   VALUE +00151.
00049
00050          10  FILLER.
00051              15  FILLER              PIC S9(5)   VALUE +00085.
00052              15  FILLER              PIC S9(5)   VALUE +00085.
00053              15  FILLER              PIC S9(5)   VALUE +00087.
00054              15  FILLER              PIC S9(5)   VALUE +00092.
00055              15  FILLER              PIC S9(5)   VALUE +00100.
00056              15  FILLER              PIC S9(5)   VALUE +00108.
00057              15  FILLER              PIC S9(5)   VALUE +00118.
00058              15  FILLER              PIC S9(5)   VALUE +00131.
00059              15  FILLER              PIC S9(5)   VALUE +00146.
00060              15  FILLER              PIC S9(5)   VALUE +00165.
00061              15  FILLER              PIC S9(5)   VALUE +00191.
00062
00063          10  FILLER.
00064              15  FILLER              PIC S9(5)   VALUE +00088.
00065              15  FILLER              PIC S9(5)   VALUE +00089.
00066              15  FILLER              PIC S9(5)   VALUE +00091.
00067              15  FILLER              PIC S9(5)   VALUE +00097.
00068              15  FILLER              PIC S9(5)   VALUE +00105.
00069              15  FILLER              PIC S9(5)   VALUE +00116.
00070              15  FILLER              PIC S9(5)   VALUE +00127.
00071              15  FILLER              PIC S9(5)   VALUE +00143.
00072              15  FILLER              PIC S9(5)   VALUE +00164.
00073              15  FILLER              PIC S9(5)   VALUE +00189.
00074              15  FILLER              PIC S9(5)   VALUE +00227.
00075
00076          10  FILLER.
00077              15  FILLER              PIC S9(5)   VALUE +00091.
00078              15  FILLER              PIC S9(5)   VALUE +00091.
00079              15  FILLER              PIC S9(5)   VALUE +00094.
00080              15  FILLER              PIC S9(5)   VALUE +00100.
00081              15  FILLER              PIC S9(5)   VALUE +00109.
00082              15  FILLER              PIC S9(5)   VALUE +00121.
00083              15  FILLER              PIC S9(5)   VALUE +00134.
00084              15  FILLER              PIC S9(5)   VALUE +00153.
00085              15  FILLER              PIC S9(5)   VALUE +00179.
00086              15  FILLER              PIC S9(5)   VALUE +00211.
00087              15  FILLER              PIC S9(5)   VALUE +00261.
00088
00089          10  FILLER.
00090              15  FILLER              PIC S9(5)   VALUE +00093.
00091              15  FILLER              PIC S9(5)   VALUE +00093.
00092              15  FILLER              PIC S9(5)   VALUE +00096.
00093              15  FILLER              PIC S9(5)   VALUE +00103.
00094              15  FILLER              PIC S9(5)   VALUE +00112.
00095              15  FILLER              PIC S9(5)   VALUE +00125.
00096              15  FILLER              PIC S9(5)   VALUE +00140.
00097              15  FILLER              PIC S9(5)   VALUE +00162.
00098              15  FILLER              PIC S9(5)   VALUE +00191.
00099              15  FILLER              PIC S9(5)   VALUE +00231.
00100              15  FILLER              PIC S9(5)   VALUE +00294.
00101
00102          10  FILLER.
00103              15  FILLER              PIC S9(5)   VALUE +00094.
00104              15  FILLER              PIC S9(5)   VALUE +00094.
00105              15  FILLER              PIC S9(5)   VALUE +00098.
00106              15  FILLER              PIC S9(5)   VALUE +00105.
00107              15  FILLER              PIC S9(5)   VALUE +00115.
00108              15  FILLER              PIC S9(5)   VALUE +00128.
00109              15  FILLER              PIC S9(5)   VALUE +00145.
00110              15  FILLER              PIC S9(5)   VALUE +00169.
00111              15  FILLER              PIC S9(5)   VALUE +00202.
00112              15  FILLER              PIC S9(5)   VALUE +00249.
00113              15  FILLER              PIC S9(5)   VALUE +00325.
00114
00115          10  FILLER.
00116              15  FILLER              PIC S9(5)   VALUE +00096.
00117              15  FILLER              PIC S9(5)   VALUE +00096.
00118              15  FILLER              PIC S9(5)   VALUE +00099.
00119              15  FILLER              PIC S9(5)   VALUE +00106.
00120              15  FILLER              PIC S9(5)   VALUE +00117.
00121              15  FILLER              PIC S9(5)   VALUE +00131.
00122              15  FILLER              PIC S9(5)   VALUE +00149.
00123              15  FILLER              PIC S9(5)   VALUE +00176.
00124              15  FILLER              PIC S9(5)   VALUE +00213.
00125              15  FILLER              PIC S9(5)   VALUE +00266.
00126              15  FILLER              PIC S9(5)   VALUE +00354.
00127
00128          10  FILLER.
00129              15  FILLER              PIC S9(5)   VALUE +00097.
00130              15  FILLER              PIC S9(5)   VALUE +00097.
00131              15  FILLER              PIC S9(5)   VALUE +00100.
00132              15  FILLER              PIC S9(5)   VALUE +00108.
00133              15  FILLER              PIC S9(5)   VALUE +00119.
00134              15  FILLER              PIC S9(5)   VALUE +00134.
00135              15  FILLER              PIC S9(5)   VALUE +00153.
00136              15  FILLER              PIC S9(5)   VALUE +00182.
00137              15  FILLER              PIC S9(5)   VALUE +00223.
00138              15  FILLER              PIC S9(5)   VALUE +00282.
00139              15  FILLER              PIC S9(5)   VALUE +00382.
00140
00141          10  FILLER.
00142              15  FILLER              PIC S9(5)   VALUE +00098.
00143              15  FILLER              PIC S9(5)   VALUE +00098.
00144              15  FILLER              PIC S9(5)   VALUE +00102.
00145              15  FILLER              PIC S9(5)   VALUE +00109.
00146              15  FILLER              PIC S9(5)   VALUE +00120.
00147              15  FILLER              PIC S9(5)   VALUE +00136.
00148              15  FILLER              PIC S9(5)   VALUE +00156.
00149              15  FILLER              PIC S9(5)   VALUE +00188.
00150              15  FILLER              PIC S9(5)   VALUE +00232.
00151              15  FILLER              PIC S9(5)   VALUE +00298.
00152              15  FILLER              PIC S9(5)   VALUE +00408.
00153
00154          10  FILLER.
00155              15  FILLER              PIC S9(5)   VALUE +00099.
00156              15  FILLER              PIC S9(5)   VALUE +00099.
00157              15  FILLER              PIC S9(5)   VALUE +00103.
00158              15  FILLER              PIC S9(5)   VALUE +00110.
00159              15  FILLER              PIC S9(5)   VALUE +00122.
00160              15  FILLER              PIC S9(5)   VALUE +00139.
00161              15  FILLER              PIC S9(5)   VALUE +00160.
00162              15  FILLER              PIC S9(5)   VALUE +00193.
00163              15  FILLER              PIC S9(5)   VALUE +00241.
00164              15  FILLER              PIC S9(5)   VALUE +00313.
00165              15  FILLER              PIC S9(5)   VALUE +00433.
00166
00167          10  FILLER.
00168              15  FILLER              PIC S9(5)   VALUE +00100.
00169              15  FILLER              PIC S9(5)   VALUE +00100.
00170              15  FILLER              PIC S9(5)   VALUE +00104.
00171              15  FILLER              PIC S9(5)   VALUE +00111.
00172              15  FILLER              PIC S9(5)   VALUE +00124.
00173              15  FILLER              PIC S9(5)   VALUE +00141.
00174              15  FILLER              PIC S9(5)   VALUE +00163.
00175              15  FILLER              PIC S9(5)   VALUE +00199.
00176              15  FILLER              PIC S9(5)   VALUE +00249.
00177              15  FILLER              PIC S9(5)   VALUE +00327.
00178              15  FILLER              PIC S9(5)   VALUE +00458.
00179
00180          10  FILLER.
00181              15  FILLER              PIC S9(5)   VALUE +00101.
00182              15  FILLER              PIC S9(5)   VALUE +00101.
00183              15  FILLER              PIC S9(5)   VALUE +00105.
00184              15  FILLER              PIC S9(5)   VALUE +00113.
00185              15  FILLER              PIC S9(5)   VALUE +00125.
00186              15  FILLER              PIC S9(5)   VALUE +00144.
00187              15  FILLER              PIC S9(5)   VALUE +00167.
00188              15  FILLER              PIC S9(5)   VALUE +00204.
00189              15  FILLER              PIC S9(5)   VALUE +00258.
00190              15  FILLER              PIC S9(5)   VALUE +00341.
00191              15  FILLER              PIC S9(5)   VALUE +00482.
00192
00193          10  FILLER.
00194              15  FILLER              PIC S9(5)   VALUE +00101.
00195              15  FILLER              PIC S9(5)   VALUE +00102.
00196              15  FILLER              PIC S9(5)   VALUE +00106.
00197              15  FILLER              PIC S9(5)   VALUE +00114.
00198              15  FILLER              PIC S9(5)   VALUE +00127.
00199              15  FILLER              PIC S9(5)   VALUE +00146.
00200              15  FILLER              PIC S9(5)   VALUE +00170.
00201              15  FILLER              PIC S9(5)   VALUE +00210.
00202              15  FILLER              PIC S9(5)   VALUE +00266.
00203              15  FILLER              PIC S9(5)   VALUE +00355.
00204              15  FILLER              PIC S9(5)   VALUE +00505.
00205
00206          10  FILLER.
00207              15  FILLER              PIC S9(5)   VALUE +00102.
00208              15  FILLER              PIC S9(5)   VALUE +00102.
00209              15  FILLER              PIC S9(5)   VALUE +00107.
00210              15  FILLER              PIC S9(5)   VALUE +00115.
00211              15  FILLER              PIC S9(5)   VALUE +00128.
00212              15  FILLER              PIC S9(5)   VALUE +00148.
00213              15  FILLER              PIC S9(5)   VALUE +00173.
00214              15  FILLER              PIC S9(5)   VALUE +00215.
00215              15  FILLER              PIC S9(5)   VALUE +00274.
00216              15  FILLER              PIC S9(5)   VALUE +00368.
00217              15  FILLER              PIC S9(5)   VALUE +00529.
00218
00219          10  FILLER.
00220              15  FILLER              PIC S9(5)   VALUE +00103.
00221              15  FILLER              PIC S9(5)   VALUE +00103.
00222              15  FILLER              PIC S9(5)   VALUE +00107.
00223              15  FILLER              PIC S9(5)   VALUE +00116.
00224              15  FILLER              PIC S9(5)   VALUE +00129.
00225              15  FILLER              PIC S9(5)   VALUE +00150.
00226              15  FILLER              PIC S9(5)   VALUE +00176.
00227              15  FILLER              PIC S9(5)   VALUE +00220.
00228              15  FILLER              PIC S9(5)   VALUE +00282.
00229              15  FILLER              PIC S9(5)   VALUE +00381.
00230              15  FILLER              PIC S9(5)   VALUE +00551.
00231
00232          10  FILLER.
00233              15  FILLER              PIC S9(5)   VALUE +00104.
00234              15  FILLER              PIC S9(5)   VALUE +00104.
00235              15  FILLER              PIC S9(5)   VALUE +00108.
00236              15  FILLER              PIC S9(5)   VALUE +00117.
00237              15  FILLER              PIC S9(5)   VALUE +00131.
00238              15  FILLER              PIC S9(5)   VALUE +00152.
00239              15  FILLER              PIC S9(5)   VALUE +00179.
00240              15  FILLER              PIC S9(5)   VALUE +00225.
00241              15  FILLER              PIC S9(5)   VALUE +00290.
00242              15  FILLER              PIC S9(5)   VALUE +00395.
00243              15  FILLER              PIC S9(5)   VALUE +00574.
00244
00245          10  FILLER.
00246              15  FILLER              PIC S9(5)   VALUE +00105.
00247              15  FILLER              PIC S9(5)   VALUE +00105.
00248              15  FILLER              PIC S9(5)   VALUE +00109.
00249              15  FILLER              PIC S9(5)   VALUE +00118.
00250              15  FILLER              PIC S9(5)   VALUE +00132.
00251              15  FILLER              PIC S9(5)   VALUE +00154.
00252              15  FILLER              PIC S9(5)   VALUE +00182.
00253              15  FILLER              PIC S9(5)   VALUE +00230.
00254              15  FILLER              PIC S9(5)   VALUE +00298.
00255              15  FILLER              PIC S9(5)   VALUE +00408.
00256              15  FILLER              PIC S9(5)   VALUE +00596.
00257
00258          10  FILLER.
00259              15  FILLER              PIC S9(5)   VALUE +00105.
00260              15  FILLER              PIC S9(5)   VALUE +00105.
00261              15  FILLER              PIC S9(5)   VALUE +00110.
00262              15  FILLER              PIC S9(5)   VALUE +00119.
00263              15  FILLER              PIC S9(5)   VALUE +00133.
00264              15  FILLER              PIC S9(5)   VALUE +00156.
00265              15  FILLER              PIC S9(5)   VALUE +00185.
00266              15  FILLER              PIC S9(5)   VALUE +00235.
00267              15  FILLER              PIC S9(5)   VALUE +00305.
00268              15  FILLER              PIC S9(5)   VALUE +00420.
00269              15  FILLER              PIC S9(5)   VALUE +00618.
00270
00271          10  FILLER.
00272              15  FILLER              PIC S9(5)   VALUE +00106.
00273              15  FILLER              PIC S9(5)   VALUE +00106.
00274              15  FILLER              PIC S9(5)   VALUE +00111.
00275              15  FILLER              PIC S9(5)   VALUE +00120.
00276              15  FILLER              PIC S9(5)   VALUE +00135.
00277              15  FILLER              PIC S9(5)   VALUE +00158.
00278              15  FILLER              PIC S9(5)   VALUE +00188.
00279              15  FILLER              PIC S9(5)   VALUE +00239.
00280              15  FILLER              PIC S9(5)   VALUE +00313.
00281              15  FILLER              PIC S9(5)   VALUE +00433.
00282              15  FILLER              PIC S9(5)   VALUE +00640.
00283
00284          10  FILLER.
00285              15  FILLER              PIC S9(5)   VALUE +00107.
00286              15  FILLER              PIC S9(5)   VALUE +00107.
00287              15  FILLER              PIC S9(5)   VALUE +00112.
00288              15  FILLER              PIC S9(5)   VALUE +00121.
00289              15  FILLER              PIC S9(5)   VALUE +00136.
00290              15  FILLER              PIC S9(5)   VALUE +00160.
00291              15  FILLER              PIC S9(5)   VALUE +00191.
00292              15  FILLER              PIC S9(5)   VALUE +00244.
00293              15  FILLER              PIC S9(5)   VALUE +00320.
00294              15  FILLER              PIC S9(5)   VALUE +00446.
00295              15  FILLER              PIC S9(5)   VALUE +00662.
00296
00297          10  FILLER.
00298              15  FILLER              PIC S9(5)   VALUE +00107.
00299              15  FILLER              PIC S9(5)   VALUE +00107.
00300              15  FILLER              PIC S9(5)   VALUE +00112.
00301              15  FILLER              PIC S9(5)   VALUE +00122.
00302              15  FILLER              PIC S9(5)   VALUE +00137.
00303              15  FILLER              PIC S9(5)   VALUE +00162.
00304              15  FILLER              PIC S9(5)   VALUE +00194.
00305              15  FILLER              PIC S9(5)   VALUE +00249.
00306              15  FILLER              PIC S9(5)   VALUE +00328.
00307              15  FILLER              PIC S9(5)   VALUE +00458.
00308              15  FILLER              PIC S9(5)   VALUE +00683.
00309
00310          10  FILLER.
00311              15  FILLER              PIC S9(5)   VALUE +00108.
00312              15  FILLER              PIC S9(5)   VALUE +00108.
00313              15  FILLER              PIC S9(5)   VALUE +00113.
00314              15  FILLER              PIC S9(5)   VALUE +00123.
00315              15  FILLER              PIC S9(5)   VALUE +00138.
00316              15  FILLER              PIC S9(5)   VALUE +00163.
00317              15  FILLER              PIC S9(5)   VALUE +00196.
00318              15  FILLER              PIC S9(5)   VALUE +00253.
00319              15  FILLER              PIC S9(5)   VALUE +00335.
00320              15  FILLER              PIC S9(5)   VALUE +00470.
00321              15  FILLER              PIC S9(5)   VALUE +00704.
00322
00323          10  FILLER.
00324              15  FILLER              PIC S9(5)   VALUE +00111.
00325              15  FILLER              PIC S9(5)   VALUE +00112.
00326              15  FILLER              PIC S9(5)   VALUE +00117.
00327              15  FILLER              PIC S9(5)   VALUE +00128.
00328              15  FILLER              PIC S9(5)   VALUE +00145.
00329              15  FILLER              PIC S9(5)   VALUE +00174.
00330              15  FILLER              PIC S9(5)   VALUE +00212.
00331              15  FILLER              PIC S9(5)   VALUE +00280.
00332              15  FILLER              PIC S9(5)   VALUE +00378.
00333              15  FILLER              PIC S9(5)   VALUE +00543.
00334              15  FILLER              PIC S9(5)   VALUE +00831.
00335
00336          10  FILLER.
00337              15  FILLER              PIC S9(5)   VALUE +00117.
00338              15  FILLER              PIC S9(5)   VALUE +00118.
00339              15  FILLER              PIC S9(5)   VALUE +00124.
00340              15  FILLER              PIC S9(5)   VALUE +00136.
00341              15  FILLER              PIC S9(5)   VALUE +00157.
00342              15  FILLER              PIC S9(5)   VALUE +00192.
00343              15  FILLER              PIC S9(5)   VALUE +00240.
00344              15  FILLER              PIC S9(5)   VALUE +00327.
00345              15  FILLER              PIC S9(5)   VALUE +00452.
00346              15  FILLER              PIC S9(5)   VALUE +00668.
00347              15  FILLER              PIC S9(5)   VALUE +01047.
00348
00349          10  FILLER.
00350              15  FILLER              PIC S9(5)   VALUE +00122.
00351              15  FILLER              PIC S9(5)   VALUE +00122.
00352              15  FILLER              PIC S9(5)   VALUE +00130.
00353              15  FILLER              PIC S9(5)   VALUE +00143.
00354              15  FILLER              PIC S9(5)   VALUE +00167.
00355              15  FILLER              PIC S9(5)   VALUE +00207.
00356              15  FILLER              PIC S9(5)   VALUE +00263.
00357              15  FILLER              PIC S9(5)   VALUE +00367.
00358              15  FILLER              PIC S9(5)   VALUE +00517.
00359              15  FILLER              PIC S9(5)   VALUE +00776.
00360              15  FILLER              PIC S9(5)   VALUE +01233.
00361
00362          10  FILLER.
00363              15  FILLER              PIC S9(5)   VALUE +00125.
00364              15  FILLER              PIC S9(5)   VALUE +00126.
00365              15  FILLER              PIC S9(5)   VALUE +00135.
00366              15  FILLER              PIC S9(5)   VALUE +00149.
00367              15  FILLER              PIC S9(5)   VALUE +00175.
00368              15  FILLER              PIC S9(5)   VALUE +00220.
00369              15  FILLER              PIC S9(5)   VALUE +00284.
00370              15  FILLER              PIC S9(5)   VALUE +00402.
00371              15  FILLER              PIC S9(5)   VALUE +00573.
00372              15  FILLER              PIC S9(5)   VALUE +00870.
00373              15  FILLER              PIC S9(5)   VALUE +01393.
00374
00375          10  FILLER.
00376              15  FILLER              PIC S9(5)   VALUE +00128.
00377              15  FILLER              PIC S9(5)   VALUE +00130.
00378              15  FILLER              PIC S9(5)   VALUE +00139.
00379              15  FILLER              PIC S9(5)   VALUE +00155.
00380              15  FILLER              PIC S9(5)   VALUE +00183.
00381              15  FILLER              PIC S9(5)   VALUE +00232.
00382              15  FILLER              PIC S9(5)   VALUE +00303.
00383              15  FILLER              PIC S9(5)   VALUE +00433.
00384              15  FILLER              PIC S9(5)   VALUE +00622.
00385              15  FILLER              PIC S9(5)   VALUE +00952.
00386              15  FILLER              PIC S9(5)   VALUE +01529.
00387
00388          10  FILLER.
00389              15  FILLER              PIC S9(5)   VALUE +00131.
00390              15  FILLER              PIC S9(5)   VALUE +00133.
00391              15  FILLER              PIC S9(5)   VALUE +00142.
00392              15  FILLER              PIC S9(5)   VALUE +00160.
00393              15  FILLER              PIC S9(5)   VALUE +00190.
00394              15  FILLER              PIC S9(5)   VALUE +00243.
00395              15  FILLER              PIC S9(5)   VALUE +00319.
00396              15  FILLER              PIC S9(5)   VALUE +00461.
00397              15  FILLER              PIC S9(5)   VALUE +00665.
00398              15  FILLER              PIC S9(5)   VALUE +01023.
00399              15  FILLER              PIC S9(5)   VALUE +01643.
00400
00401          10  FILLER.
00402              15  FILLER              PIC S9(5)   VALUE +00133.
00403              15  FILLER              PIC S9(5)   VALUE +00136.
00404              15  FILLER              PIC S9(5)   VALUE +00146.
00405              15  FILLER              PIC S9(5)   VALUE +00164.
00406              15  FILLER              PIC S9(5)   VALUE +00196.
00407              15  FILLER              PIC S9(5)   VALUE +00253.
00408              15  FILLER              PIC S9(5)   VALUE +00334.
00409              15  FILLER              PIC S9(5)   VALUE +00486.
00410              15  FILLER              PIC S9(5)   VALUE +00704.
00411              15  FILLER              PIC S9(5)   VALUE +01085.
00412              15  FILLER              PIC S9(5)   VALUE +01739.
00413
00414          10  FILLER.
00415              15  FILLER              PIC S9(5)   VALUE +00136.
00416              15  FILLER              PIC S9(5)   VALUE +00138.
00417              15  FILLER              PIC S9(5)   VALUE +00149.
00418              15  FILLER              PIC S9(5)   VALUE +00168.
00419              15  FILLER              PIC S9(5)   VALUE +00202.
00420              15  FILLER              PIC S9(5)   VALUE +00262.
00421              15  FILLER              PIC S9(5)   VALUE +00348.
00422              15  FILLER              PIC S9(5)   VALUE +00508.
00423              15  FILLER              PIC S9(5)   VALUE +00738.
00424              15  FILLER              PIC S9(5)   VALUE +01138.
00425              15  FILLER              PIC S9(5)   VALUE +01819.
00426
00427          10  FILLER.
00428              15  FILLER              PIC S9(5)   VALUE +00137.
00429              15  FILLER              PIC S9(5)   VALUE +00140.
00430              15  FILLER              PIC S9(5)   VALUE +00152.
00431              15  FILLER              PIC S9(5)   VALUE +00172.
00432              15  FILLER              PIC S9(5)   VALUE +00207.
00433              15  FILLER              PIC S9(5)   VALUE +00270.
00434              15  FILLER              PIC S9(5)   VALUE +00360.
00435              15  FILLER              PIC S9(5)   VALUE +00528.
00436              15  FILLER              PIC S9(5)   VALUE +00767.
00437              15  FILLER              PIC S9(5)   VALUE +01183.
00438              15  FILLER              PIC S9(5)   VALUE +01884.
00439
00440          10  FILLER.
00441              15  FILLER              PIC S9(5)   VALUE +00139.
00442              15  FILLER              PIC S9(5)   VALUE +00142.
00443              15  FILLER              PIC S9(5)   VALUE +00154.
00444              15  FILLER              PIC S9(5)   VALUE +00175.
00445              15  FILLER              PIC S9(5)   VALUE +00211.
00446              15  FILLER              PIC S9(5)   VALUE +00278.
00447              15  FILLER              PIC S9(5)   VALUE +00371.
00448              15  FILLER              PIC S9(5)   VALUE +00546.
00449              15  FILLER              PIC S9(5)   VALUE +00793.
00450              15  FILLER              PIC S9(5)   VALUE +01222.
00451              15  FILLER              PIC S9(5)   VALUE +01937.
00452
00453          10  FILLER.
00454              15  FILLER              PIC S9(5)   VALUE +00141.
00455              15  FILLER              PIC S9(5)   VALUE +00144.
00456              15  FILLER              PIC S9(5)   VALUE +00156.
00457              15  FILLER              PIC S9(5)   VALUE +00178.
00458              15  FILLER              PIC S9(5)   VALUE +00216.
00459              15  FILLER              PIC S9(5)   VALUE +00284.
00460              15  FILLER              PIC S9(5)   VALUE +00381.
00461              15  FILLER              PIC S9(5)   VALUE +00561.
00462              15  FILLER              PIC S9(5)   VALUE +00816.
00463              15  FILLER              PIC S9(5)   VALUE +01255.
00464              15  FILLER              PIC S9(5)   VALUE +01979.
00465
00466          10  FILLER.
00467              15  FILLER              PIC S9(5)   VALUE +00142.
00468              15  FILLER              PIC S9(5)   VALUE +00146.
00469              15  FILLER              PIC S9(5)   VALUE +00159.
00470              15  FILLER              PIC S9(5)   VALUE +00181.
00471              15  FILLER              PIC S9(5)   VALUE +00220.
00472              15  FILLER              PIC S9(5)   VALUE +00290.
00473              15  FILLER              PIC S9(5)   VALUE +00390.
00474              15  FILLER              PIC S9(5)   VALUE +00576.
00475              15  FILLER              PIC S9(5)   VALUE +00836.
00476              15  FILLER              PIC S9(5)   VALUE +01283.
00477              15  FILLER              PIC S9(5)   VALUE +02012.
00478
00479          10  FILLER.
00480              15  FILLER              PIC S9(5)   VALUE +00143.
00481              15  FILLER              PIC S9(5)   VALUE +00147.
00482              15  FILLER              PIC S9(5)   VALUE +00161.
00483              15  FILLER              PIC S9(5)   VALUE +00184.
00484              15  FILLER              PIC S9(5)   VALUE +00224.
00485              15  FILLER              PIC S9(5)   VALUE +00296.
00486              15  FILLER              PIC S9(5)   VALUE +00398.
00487              15  FILLER              PIC S9(5)   VALUE +00588.
00488              15  FILLER              PIC S9(5)   VALUE +00853.
00489              15  FILLER              PIC S9(5)   VALUE +01306.
00490              15  FILLER              PIC S9(5)   VALUE +02038.
00491
00492          10  FILLER.
00493              15  FILLER              PIC S9(5)   VALUE +00153.
00494              15  FILLER              PIC S9(5)   VALUE +00156.
00495              15  FILLER              PIC S9(5)   VALUE +00165.
00496              15  FILLER              PIC S9(5)   VALUE +00000.
00497              15  FILLER              PIC S9(5)   VALUE +00000.
00498              15  FILLER              PIC S9(5)   VALUE +00000.
00499              15  FILLER              PIC S9(5)   VALUE +00000.
00500              15  FILLER              PIC S9(5)   VALUE +00000.
00501              15  FILLER              PIC S9(5)   VALUE +00000.
00502              15  FILLER              PIC S9(5)   VALUE +00000.
00503              15  FILLER              PIC S9(5)   VALUE +00000.
00504
00505          10  FILLER.
00506              15  FILLER              PIC S9(5)   VALUE +00155.
00507              15  FILLER              PIC S9(5)   VALUE +00159.
00508              15  FILLER              PIC S9(5)   VALUE +00171.
00509              15  FILLER              PIC S9(5)   VALUE +00190.
00510              15  FILLER              PIC S9(5)   VALUE +00000.
00511              15  FILLER              PIC S9(5)   VALUE +00000.
00512              15  FILLER              PIC S9(5)   VALUE +00000.
00513              15  FILLER              PIC S9(5)   VALUE +00000.
00514              15  FILLER              PIC S9(5)   VALUE +00000.
00515              15  FILLER              PIC S9(5)   VALUE +00000.
00516              15  FILLER              PIC S9(5)   VALUE +00000.
00517
00518          10  FILLER.
00519              15  FILLER              PIC S9(5)   VALUE +00156.
00520              15  FILLER              PIC S9(5)   VALUE +00161.
00521              15  FILLER              PIC S9(5)   VALUE +00175.
00522              15  FILLER              PIC S9(5)   VALUE +00198.
00523              15  FILLER              PIC S9(5)   VALUE +00233.
00524              15  FILLER              PIC S9(5)   VALUE +00000.
00525              15  FILLER              PIC S9(5)   VALUE +00000.
00526              15  FILLER              PIC S9(5)   VALUE +00000.
00527              15  FILLER              PIC S9(5)   VALUE +00000.
00528              15  FILLER              PIC S9(5)   VALUE +00000.
00529              15  FILLER              PIC S9(5)   VALUE +00000.
00530
00531          10  FILLER.
00532              15  FILLER              PIC S9(5)   VALUE +00157.
00533              15  FILLER              PIC S9(5)   VALUE +00162.
00534              15  FILLER              PIC S9(5)   VALUE +00177.
00535              15  FILLER              PIC S9(5)   VALUE +00204.
00536              15  FILLER              PIC S9(5)   VALUE +00244.
00537              15  FILLER              PIC S9(5)   VALUE +00310.
00538              15  FILLER              PIC S9(5)   VALUE +00000.
00539              15  FILLER              PIC S9(5)   VALUE +00000.
00540              15  FILLER              PIC S9(5)   VALUE +00000.
00541              15  FILLER              PIC S9(5)   VALUE +00000.
00542              15  FILLER              PIC S9(5)   VALUE +00000.
00543
00544          10  FILLER.
00545              15  FILLER              PIC S9(5)   VALUE +00158.
00546              15  FILLER              PIC S9(5)   VALUE +00163.
00547              15  FILLER              PIC S9(5)   VALUE +00180.
00548              15  FILLER              PIC S9(5)   VALUE +00210.
00549              15  FILLER              PIC S9(5)   VALUE +00259.
00550              15  FILLER              PIC S9(5)   VALUE +00342.
00551              15  FILLER              PIC S9(5)   VALUE +00458.
00552              15  FILLER              PIC S9(5)   VALUE +00665.
00553              15  FILLER              PIC S9(5)   VALUE +00935.
00554              15  FILLER              PIC S9(5)   VALUE +01388.
00555              15  FILLER              PIC S9(5)   VALUE +02103.
00556
00557      05  TAB-02-VALUES.
00558
00559          10  FILLER.
00560              15  FILLER              PIC S9(5)   VALUE +00039.
00561              15  FILLER              PIC S9(5)   VALUE +00039.
00562              15  FILLER              PIC S9(5)   VALUE +00039.
00563              15  FILLER              PIC S9(5)   VALUE +00040.
00564              15  FILLER              PIC S9(5)   VALUE +00040.
00565              15  FILLER              PIC S9(5)   VALUE +00041.
00566              15  FILLER              PIC S9(5)   VALUE +00041.
00567              15  FILLER              PIC S9(5)   VALUE +00042.
00568              15  FILLER              PIC S9(5)   VALUE +00043.
00569              15  FILLER              PIC S9(5)   VALUE +00044.
00570              15  FILLER              PIC S9(5)   VALUE +00045.
00571
00572          10  FILLER.
00573              15  FILLER              PIC S9(5)   VALUE +00082.
00574              15  FILLER              PIC S9(5)   VALUE +00082.
00575              15  FILLER              PIC S9(5)   VALUE +00083.
00576              15  FILLER              PIC S9(5)   VALUE +00086.
00577              15  FILLER              PIC S9(5)   VALUE +00087.
00578              15  FILLER              PIC S9(5)   VALUE +00091.
00579              15  FILLER              PIC S9(5)   VALUE +00094.
00580              15  FILLER              PIC S9(5)   VALUE +00099.
00581              15  FILLER              PIC S9(5)   VALUE +00105.
00582              15  FILLER              PIC S9(5)   VALUE +00111.
00583              15  FILLER              PIC S9(5)   VALUE +00120.
00584
00585          10  FILLER.
00586              15  FILLER              PIC S9(5)   VALUE +00105.
00587              15  FILLER              PIC S9(5)   VALUE +00105.
00588              15  FILLER              PIC S9(5)   VALUE +00107.
00589              15  FILLER              PIC S9(5)   VALUE +00111.
00590              15  FILLER              PIC S9(5)   VALUE +00115.
00591              15  FILLER              PIC S9(5)   VALUE +00121.
00592              15  FILLER              PIC S9(5)   VALUE +00128.
00593              15  FILLER              PIC S9(5)   VALUE +00137.
00594              15  FILLER              PIC S9(5)   VALUE +00150.
00595              15  FILLER              PIC S9(5)   VALUE +00164.
00596              15  FILLER              PIC S9(5)   VALUE +00184.
00597
00598          10  FILLER.
00599              15  FILLER              PIC S9(5)   VALUE +00118.
00600              15  FILLER              PIC S9(5)   VALUE +00118.
00601              15  FILLER              PIC S9(5)   VALUE +00121.
00602              15  FILLER              PIC S9(5)   VALUE +00126.
00603              15  FILLER              PIC S9(5)   VALUE +00131.
00604              15  FILLER              PIC S9(5)   VALUE +00141.
00605              15  FILLER              PIC S9(5)   VALUE +00150.
00606              15  FILLER              PIC S9(5)   VALUE +00165.
00607              15  FILLER              PIC S9(5)   VALUE +00185.
00608              15  FILLER              PIC S9(5)   VALUE +00208.
00609              15  FILLER              PIC S9(5)   VALUE +00243.
00610
00611          10  FILLER.
00612              15  FILLER              PIC S9(5)   VALUE +00127.
00613              15  FILLER              PIC S9(5)   VALUE +00127.
00614              15  FILLER              PIC S9(5)   VALUE +00130.
00615              15  FILLER              PIC S9(5)   VALUE +00135.
00616              15  FILLER              PIC S9(5)   VALUE +00142.
00617              15  FILLER              PIC S9(5)   VALUE +00154.
00618              15  FILLER              PIC S9(5)   VALUE +00167.
00619              15  FILLER              PIC S9(5)   VALUE +00186.
00620              15  FILLER              PIC S9(5)   VALUE +00213.
00621              15  FILLER              PIC S9(5)   VALUE +00247.
00622              15  FILLER              PIC S9(5)   VALUE +00299.
00623
00624          10  FILLER.
00625              15  FILLER              PIC S9(5)   VALUE +00133.
00626              15  FILLER              PIC S9(5)   VALUE +00133.
00627              15  FILLER              PIC S9(5)   VALUE +00136.
00628              15  FILLER              PIC S9(5)   VALUE +00143.
00629              15  FILLER              PIC S9(5)   VALUE +00150.
00630              15  FILLER              PIC S9(5)   VALUE +00164.
00631              15  FILLER              PIC S9(5)   VALUE +00180.
00632              15  FILLER              PIC S9(5)   VALUE +00204.
00633              15  FILLER              PIC S9(5)   VALUE +00238.
00634              15  FILLER              PIC S9(5)   VALUE +00282.
00635              15  FILLER              PIC S9(5)   VALUE +00352.
00636
00637          10  FILLER.
00638              15  FILLER              PIC S9(5)   VALUE +00138.
00639              15  FILLER              PIC S9(5)   VALUE +00138.
00640              15  FILLER              PIC S9(5)   VALUE +00142.
00641              15  FILLER              PIC S9(5)   VALUE +00148.
00642              15  FILLER              PIC S9(5)   VALUE +00157.
00643              15  FILLER              PIC S9(5)   VALUE +00173.
00644              15  FILLER              PIC S9(5)   VALUE +00191.
00645              15  FILLER              PIC S9(5)   VALUE +00220.
00646              15  FILLER              PIC S9(5)   VALUE +00260.
00647              15  FILLER              PIC S9(5)   VALUE +00315.
00648              15  FILLER              PIC S9(5)   VALUE +00402.
00649
00650          10  FILLER.
00651              15  FILLER              PIC S9(5)   VALUE +00142.
00652              15  FILLER              PIC S9(5)   VALUE +00143.
00653              15  FILLER              PIC S9(5)   VALUE +00146.
00654              15  FILLER              PIC S9(5)   VALUE +00153.
00655              15  FILLER              PIC S9(5)   VALUE +00163.
00656              15  FILLER              PIC S9(5)   VALUE +00181.
00657              15  FILLER              PIC S9(5)   VALUE +00202.
00658              15  FILLER              PIC S9(5)   VALUE +00235.
00659              15  FILLER              PIC S9(5)   VALUE +00281.
00660              15  FILLER              PIC S9(5)   VALUE +00346.
00661              15  FILLER              PIC S9(5)   VALUE +00450.
00662
00663          10  FILLER.
00664              15  FILLER              PIC S9(5)   VALUE +00146.
00665              15  FILLER              PIC S9(5)   VALUE +00146.
00666              15  FILLER              PIC S9(5)   VALUE +00150.
00667              15  FILLER              PIC S9(5)   VALUE +00158.
00668              15  FILLER              PIC S9(5)   VALUE +00168.
00669              15  FILLER              PIC S9(5)   VALUE +00188.
00670              15  FILLER              PIC S9(5)   VALUE +00211.
00671              15  FILLER              PIC S9(5)   VALUE +00248.
00672              15  FILLER              PIC S9(5)   VALUE +00300.
00673              15  FILLER              PIC S9(5)   VALUE +00375.
00674              15  FILLER              PIC S9(5)   VALUE +00495.
00675
00676          10  FILLER.
00677              15  FILLER              PIC S9(5)   VALUE +00150.
00678              15  FILLER              PIC S9(5)   VALUE +00150.
00679              15  FILLER              PIC S9(5)   VALUE +00154.
00680              15  FILLER              PIC S9(5)   VALUE +00162.
00681              15  FILLER              PIC S9(5)   VALUE +00173.
00682              15  FILLER              PIC S9(5)   VALUE +00195.
00683              15  FILLER              PIC S9(5)   VALUE +00220.
00684              15  FILLER              PIC S9(5)   VALUE +00261.
00685              15  FILLER              PIC S9(5)   VALUE +00318.
00686              15  FILLER              PIC S9(5)   VALUE +00403.
00687              15  FILLER              PIC S9(5)   VALUE +00538.
00688
00689          10  FILLER.
00690              15  FILLER              PIC S9(5)   VALUE +00153.
00691              15  FILLER              PIC S9(5)   VALUE +00153.
00692              15  FILLER              PIC S9(5)   VALUE +00158.
00693              15  FILLER              PIC S9(5)   VALUE +00166.
00694              15  FILLER              PIC S9(5)   VALUE +00178.
00695              15  FILLER              PIC S9(5)   VALUE +00201.
00696              15  FILLER              PIC S9(5)   VALUE +00228.
00697              15  FILLER              PIC S9(5)   VALUE +00274.
00698              15  FILLER              PIC S9(5)   VALUE +00336.
00699              15  FILLER              PIC S9(5)   VALUE +00430.
00700              15  FILLER              PIC S9(5)   VALUE +00579.
00701
00702          10  FILLER.
00703              15  FILLER              PIC S9(5)   VALUE +00156.
00704              15  FILLER              PIC S9(5)   VALUE +00157.
00705              15  FILLER              PIC S9(5)   VALUE +00161.
00706              15  FILLER              PIC S9(5)   VALUE +00170.
00707              15  FILLER              PIC S9(5)   VALUE +00183.
00708              15  FILLER              PIC S9(5)   VALUE +00207.
00709              15  FILLER              PIC S9(5)   VALUE +00237.
00710              15  FILLER              PIC S9(5)   VALUE +00285.
00711              15  FILLER              PIC S9(5)   VALUE +00353.
00712              15  FILLER              PIC S9(5)   VALUE +00455.
00713              15  FILLER              PIC S9(5)   VALUE +00619.
00714
00715          10  FILLER.
00716              15  FILLER              PIC S9(5)   VALUE +00159.
00717              15  FILLER              PIC S9(5)   VALUE +00160.
00718              15  FILLER              PIC S9(5)   VALUE +00165.
00719              15  FILLER              PIC S9(5)   VALUE +00174.
00720              15  FILLER              PIC S9(5)   VALUE +00187.
00721              15  FILLER              PIC S9(5)   VALUE +00213.
00722              15  FILLER              PIC S9(5)   VALUE +00244.
00723              15  FILLER              PIC S9(5)   VALUE +00297.
00724              15  FILLER              PIC S9(5)   VALUE +00369.
00725              15  FILLER              PIC S9(5)   VALUE +00480.
00726              15  FILLER              PIC S9(5)   VALUE +00658.
00727
00728          10  FILLER.
00729              15  FILLER              PIC S9(5)   VALUE +00162.
00730              15  FILLER              PIC S9(5)   VALUE +00163.
00731              15  FILLER              PIC S9(5)   VALUE +00168.
00732              15  FILLER              PIC S9(5)   VALUE +00177.
00733              15  FILLER              PIC S9(5)   VALUE +00191.
00734              15  FILLER              PIC S9(5)   VALUE +00219.
00735              15  FILLER              PIC S9(5)   VALUE +00252.
00736              15  FILLER              PIC S9(5)   VALUE +00309.
00737              15  FILLER              PIC S9(5)   VALUE +00386.
00738              15  FILLER              PIC S9(5)   VALUE +00505.
00739              15  FILLER              PIC S9(5)   VALUE +00696.
00740
00741          10  FILLER.
00742              15  FILLER              PIC S9(5)   VALUE +00165.
00743              15  FILLER              PIC S9(5)   VALUE +00165.
00744              15  FILLER              PIC S9(5)   VALUE +00171.
00745              15  FILLER              PIC S9(5)   VALUE +00180.
00746              15  FILLER              PIC S9(5)   VALUE +00195.
00747              15  FILLER              PIC S9(5)   VALUE +00224.
00748              15  FILLER              PIC S9(5)   VALUE +00260.
00749              15  FILLER              PIC S9(5)   VALUE +00320.
00750              15  FILLER              PIC S9(5)   VALUE +00402.
00751              15  FILLER              PIC S9(5)   VALUE +00529.
00752              15  FILLER              PIC S9(5)   VALUE +00734.
00753
00754          10  FILLER.
00755              15  FILLER              PIC S9(5)   VALUE +00168.
00756              15  FILLER              PIC S9(5)   VALUE +00168.
00757              15  FILLER              PIC S9(5)   VALUE +00174.
00758              15  FILLER              PIC S9(5)   VALUE +00184.
00759              15  FILLER              PIC S9(5)   VALUE +00199.
00760              15  FILLER              PIC S9(5)   VALUE +00230.
00761              15  FILLER              PIC S9(5)   VALUE +00267.
00762              15  FILLER              PIC S9(5)   VALUE +00331.
00763              15  FILLER              PIC S9(5)   VALUE +00417.
00764              15  FILLER              PIC S9(5)   VALUE +00554.
00765              15  FILLER              PIC S9(5)   VALUE +00771.
00766
00767          10  FILLER.
00768              15  FILLER              PIC S9(5)   VALUE +00170.
00769              15  FILLER              PIC S9(5)   VALUE +00171.
00770              15  FILLER              PIC S9(5)   VALUE +00177.
00771              15  FILLER              PIC S9(5)   VALUE +00187.
00772              15  FILLER              PIC S9(5)   VALUE +00203.
00773              15  FILLER              PIC S9(5)   VALUE +00235.
00774              15  FILLER              PIC S9(5)   VALUE +00274.
00775              15  FILLER              PIC S9(5)   VALUE +00342.
00776              15  FILLER              PIC S9(5)   VALUE +00433.
00777              15  FILLER              PIC S9(5)   VALUE +00577.
00778              15  FILLER              PIC S9(5)   VALUE +00808.
00779
00780          10  FILLER.
00781              15  FILLER              PIC S9(5)   VALUE +00173.
00782              15  FILLER              PIC S9(5)   VALUE +00173.
00783              15  FILLER              PIC S9(5)   VALUE +00179.
00784              15  FILLER              PIC S9(5)   VALUE +00190.
00785              15  FILLER              PIC S9(5)   VALUE +00207.
00786              15  FILLER              PIC S9(5)   VALUE +00240.
00787              15  FILLER              PIC S9(5)   VALUE +00282.
00788              15  FILLER              PIC S9(5)   VALUE +00352.
00789              15  FILLER              PIC S9(5)   VALUE +00448.
00790              15  FILLER              PIC S9(5)   VALUE +00601.
00791              15  FILLER              PIC S9(5)   VALUE +00844.
00792
00793          10  FILLER.
00794              15  FILLER              PIC S9(5)   VALUE +00175.
00795              15  FILLER              PIC S9(5)   VALUE +00176.
00796              15  FILLER              PIC S9(5)   VALUE +00182.
00797              15  FILLER              PIC S9(5)   VALUE +00193.
00798              15  FILLER              PIC S9(5)   VALUE +00211.
00799              15  FILLER              PIC S9(5)   VALUE +00245.
00800              15  FILLER              PIC S9(5)   VALUE +00289.
00801              15  FILLER              PIC S9(5)   VALUE +00363.
00802              15  FILLER              PIC S9(5)   VALUE +00463.
00803              15  FILLER              PIC S9(5)   VALUE +00624.
00804              15  FILLER              PIC S9(5)   VALUE +00880.
00805
00806          10  FILLER.
00807              15  FILLER              PIC S9(5)   VALUE +00178.
00808              15  FILLER              PIC S9(5)   VALUE +00178.
00809              15  FILLER              PIC S9(5)   VALUE +00185.
00810              15  FILLER              PIC S9(5)   VALUE +00196.
00811              15  FILLER              PIC S9(5)   VALUE +00214.
00812              15  FILLER              PIC S9(5)   VALUE +00250.
00813              15  FILLER              PIC S9(5)   VALUE +00295.
00814              15  FILLER              PIC S9(5)   VALUE +00373.
00815              15  FILLER              PIC S9(5)   VALUE +00478.
00816              15  FILLER              PIC S9(5)   VALUE +00646.
00817              15  FILLER              PIC S9(5)   VALUE +00916.
00818
00819          10  FILLER.
00820              15  FILLER              PIC S9(5)   VALUE +00180.
00821              15  FILLER              PIC S9(5)   VALUE +00181.
00822              15  FILLER              PIC S9(5)   VALUE +00187.
00823              15  FILLER              PIC S9(5)   VALUE +00199.
00824              15  FILLER              PIC S9(5)   VALUE +00218.
00825              15  FILLER              PIC S9(5)   VALUE +00255.
00826              15  FILLER              PIC S9(5)   VALUE +00302.
00827              15  FILLER              PIC S9(5)   VALUE +00383.
00828              15  FILLER              PIC S9(5)   VALUE +00493.
00829              15  FILLER              PIC S9(5)   VALUE +00669.
00830              15  FILLER              PIC S9(5)   VALUE +00951.
00831
00832          10  FILLER.
00833              15  FILLER              PIC S9(5)   VALUE +00182.
00834              15  FILLER              PIC S9(5)   VALUE +00183.
00835              15  FILLER              PIC S9(5)   VALUE +00190.
00836              15  FILLER              PIC S9(5)   VALUE +00202.
00837              15  FILLER              PIC S9(5)   VALUE +00221.
00838              15  FILLER              PIC S9(5)   VALUE +00260.
00839              15  FILLER              PIC S9(5)   VALUE +00309.
00840              15  FILLER              PIC S9(5)   VALUE +00394.
00841              15  FILLER              PIC S9(5)   VALUE +00508.
00842              15  FILLER              PIC S9(5)   VALUE +00691.
00843              15  FILLER              PIC S9(5)   VALUE +00985.
00844
00845          10  FILLER.
00846              15  FILLER              PIC S9(5)   VALUE +00184.
00847              15  FILLER              PIC S9(5)   VALUE +00185.
00848              15  FILLER              PIC S9(5)   VALUE +00192.
00849              15  FILLER              PIC S9(5)   VALUE +00205.
00850              15  FILLER              PIC S9(5)   VALUE +00225.
00851              15  FILLER              PIC S9(5)   VALUE +00265.
00852              15  FILLER              PIC S9(5)   VALUE +00315.
00853              15  FILLER              PIC S9(5)   VALUE +00403.
00854              15  FILLER              PIC S9(5)   VALUE +00522.
00855              15  FILLER              PIC S9(5)   VALUE +00713.
00856              15  FILLER              PIC S9(5)   VALUE +01020.
00857
00858          10  FILLER.
00859              15  FILLER              PIC S9(5)   VALUE +00186.
00860              15  FILLER              PIC S9(5)   VALUE +00187.
00861              15  FILLER              PIC S9(5)   VALUE +00194.
00862              15  FILLER              PIC S9(5)   VALUE +00207.
00863              15  FILLER              PIC S9(5)   VALUE +00228.
00864              15  FILLER              PIC S9(5)   VALUE +00269.
00865              15  FILLER              PIC S9(5)   VALUE +00321.
00866              15  FILLER              PIC S9(5)   VALUE +00413.
00867              15  FILLER              PIC S9(5)   VALUE +00535.
00868              15  FILLER              PIC S9(5)   VALUE +00733.
00869              15  FILLER              PIC S9(5)   VALUE +01052.
00870
00871          10  FILLER.
00872              15  FILLER              PIC S9(5)   VALUE +00199.
00873              15  FILLER              PIC S9(5)   VALUE +00200.
00874              15  FILLER              PIC S9(5)   VALUE +00208.
00875              15  FILLER              PIC S9(5)   VALUE +00223.
00876              15  FILLER              PIC S9(5)   VALUE +00247.
00877              15  FILLER              PIC S9(5)   VALUE +00297.
00878              15  FILLER              PIC S9(5)   VALUE +00360.
00879              15  FILLER              PIC S9(5)   VALUE +00472.
00880              15  FILLER              PIC S9(5)   VALUE +00621.
00881              15  FILLER              PIC S9(5)   VALUE +00865.
00882              15  FILLER              PIC S9(5)   VALUE +01258.
00883
00884          10  FILLER.
00885              15  FILLER              PIC S9(5)   VALUE +00218.
00886              15  FILLER              PIC S9(5)   VALUE +00220.
00887              15  FILLER              PIC S9(5)   VALUE +00231.
00888              15  FILLER              PIC S9(5)   VALUE +00249.
00889              15  FILLER              PIC S9(5)   VALUE +00280.
00890              15  FILLER              PIC S9(5)   VALUE +00342.
00891              15  FILLER              PIC S9(5)   VALUE +00424.
00892              15  FILLER              PIC S9(5)   VALUE +00571.
00893              15  FILLER              PIC S9(5)   VALUE +00766.
00894              15  FILLER              PIC S9(5)   VALUE +01087.
00895              15  FILLER              PIC S9(5)   VALUE +01606.
00896
00897          10  FILLER.
00898              15  FILLER              PIC S9(5)   VALUE +00233.
00899              15  FILLER              PIC S9(5)   VALUE +00236.
00900              15  FILLER              PIC S9(5)   VALUE +00249.
00901              15  FILLER              PIC S9(5)   VALUE +00270.
00902              15  FILLER              PIC S9(5)   VALUE +00307.
00903              15  FILLER              PIC S9(5)   VALUE +00381.
00904              15  FILLER              PIC S9(5)   VALUE +00480.
00905              15  FILLER              PIC S9(5)   VALUE +00657.
00906              15  FILLER              PIC S9(5)   VALUE +00891.
00907              15  FILLER              PIC S9(5)   VALUE +01279.
00908              15  FILLER              PIC S9(5)   VALUE +01905.
00909
00910          10  FILLER.
00911              15  FILLER              PIC S9(5)   VALUE +00245.
00912              15  FILLER              PIC S9(5)   VALUE +00249.
00913              15  FILLER              PIC S9(5)   VALUE +00265.
00914              15  FILLER              PIC S9(5)   VALUE +00289.
00915              15  FILLER              PIC S9(5)   VALUE +00331.
00916              15  FILLER              PIC S9(5)   VALUE +00416.
00917              15  FILLER              PIC S9(5)   VALUE +00529.
00918              15  FILLER              PIC S9(5)   VALUE +00733.
00919              15  FILLER              PIC S9(5)   VALUE +01001.
00920              15  FILLER              PIC S9(5)   VALUE +01446.
00921              15  FILLER              PIC S9(5)   VALUE +02161.
00922
00923          10  FILLER.
00924              15  FILLER              PIC S9(5)   VALUE +00256.
00925              15  FILLER              PIC S9(5)   VALUE +00261.
00926              15  FILLER              PIC S9(5)   VALUE +00279.
00927              15  FILLER              PIC S9(5)   VALUE +00306.
00928              15  FILLER              PIC S9(5)   VALUE +00352.
00929              15  FILLER              PIC S9(5)   VALUE +00447.
00930              15  FILLER              PIC S9(5)   VALUE +00573.
00931              15  FILLER              PIC S9(5)   VALUE +00800.
00932              15  FILLER              PIC S9(5)   VALUE +01097.
00933              15  FILLER              PIC S9(5)   VALUE +01592.
00934              15  FILLER              PIC S9(5)   VALUE +02379.
00935
00936          10  FILLER.
00937              15  FILLER              PIC S9(5)   VALUE +00265.
00938              15  FILLER              PIC S9(5)   VALUE +00271.
00939              15  FILLER              PIC S9(5)   VALUE +00291.
00940              15  FILLER              PIC S9(5)   VALUE +00321.
00941              15  FILLER              PIC S9(5)   VALUE +00371.
00942              15  FILLER              PIC S9(5)   VALUE +00475.
00943              15  FILLER              PIC S9(5)   VALUE +00612.
00944              15  FILLER              PIC S9(5)   VALUE +00860.
00945              15  FILLER              PIC S9(5)   VALUE +01182.
00946              15  FILLER              PIC S9(5)   VALUE +01718.
00947              15  FILLER              PIC S9(5)   VALUE +02563.
00948
00949          10  FILLER.
00950              15  FILLER              PIC S9(5)   VALUE +00273.
00951              15  FILLER              PIC S9(5)   VALUE +00281.
00952              15  FILLER              PIC S9(5)   VALUE +00302.
00953              15  FILLER              PIC S9(5)   VALUE +00334.
00954              15  FILLER              PIC S9(5)   VALUE +00389.
00955              15  FILLER              PIC S9(5)   VALUE +00501.
00956              15  FILLER              PIC S9(5)   VALUE +00648.
00957              15  FILLER              PIC S9(5)   VALUE +00914.
00958              15  FILLER              PIC S9(5)   VALUE +01257.
00959              15  FILLER              PIC S9(5)   VALUE +01828.
00960              15  FILLER              PIC S9(5)   VALUE +02717.
00961
00962          10  FILLER.
00963              15  FILLER              PIC S9(5)   VALUE +00280.
00964              15  FILLER              PIC S9(5)   VALUE +00289.
00965              15  FILLER              PIC S9(5)   VALUE +00312.
00966              15  FILLER              PIC S9(5)   VALUE +00347.
00967              15  FILLER              PIC S9(5)   VALUE +00405.
00968              15  FILLER              PIC S9(5)   VALUE +00524.
00969              15  FILLER              PIC S9(5)   VALUE +00680.
00970              15  FILLER              PIC S9(5)   VALUE +00961.
00971              15  FILLER              PIC S9(5)   VALUE +01323.
00972              15  FILLER              PIC S9(5)   VALUE +01923.
00973              15  FILLER              PIC S9(5)   VALUE +02844.
00974
00975          10  FILLER.
00976              15  FILLER              PIC S9(5)   VALUE +00286.
00977              15  FILLER              PIC S9(5)   VALUE +00296.
00978              15  FILLER              PIC S9(5)   VALUE +00321.
00979              15  FILLER              PIC S9(5)   VALUE +00358.
00980              15  FILLER              PIC S9(5)   VALUE +00420.
00981              15  FILLER              PIC S9(5)   VALUE +00545.
00982              15  FILLER              PIC S9(5)   VALUE +00709.
00983              15  FILLER              PIC S9(5)   VALUE +01004.
00984              15  FILLER              PIC S9(5)   VALUE +01381.
00985              15  FILLER              PIC S9(5)   VALUE +02004.
00986              15  FILLER              PIC S9(5)   VALUE +02949.
00987
00988          10  FILLER.
00989              15  FILLER              PIC S9(5)   VALUE +00292.
00990              15  FILLER              PIC S9(5)   VALUE +00303.
00991              15  FILLER              PIC S9(5)   VALUE +00329.
00992              15  FILLER              PIC S9(5)   VALUE +00368.
00993              15  FILLER              PIC S9(5)   VALUE +00433.
00994              15  FILLER              PIC S9(5)   VALUE +00564.
00995              15  FILLER              PIC S9(5)   VALUE +00735.
00996              15  FILLER              PIC S9(5)   VALUE +01042.
00997              15  FILLER              PIC S9(5)   VALUE +01432.
00998              15  FILLER              PIC S9(5)   VALUE +02073.
00999              15  FILLER              PIC S9(5)   VALUE +03033.
01000
01001          10  FILLER.
01002              15  FILLER              PIC S9(5)   VALUE +00298.
01003              15  FILLER              PIC S9(5)   VALUE +00309.
01004              15  FILLER              PIC S9(5)   VALUE +00337.
01005              15  FILLER              PIC S9(5)   VALUE +00378.
01006              15  FILLER              PIC S9(5)   VALUE +00446.
01007              15  FILLER              PIC S9(5)   VALUE +00581.
01008              15  FILLER              PIC S9(5)   VALUE +00759.
01009              15  FILLER              PIC S9(5)   VALUE +01077.
01010              15  FILLER              PIC S9(5)   VALUE +01476.
01011              15  FILLER              PIC S9(5)   VALUE +02132.
01012              15  FILLER              PIC S9(5)   VALUE +03101.
01013
01014          10  FILLER.
01015              15  FILLER              PIC S9(5)   VALUE +00302.
01016              15  FILLER              PIC S9(5)   VALUE +00315.
01017              15  FILLER              PIC S9(5)   VALUE +00344.
01018              15  FILLER              PIC S9(5)   VALUE +00387.
01019              15  FILLER              PIC S9(5)   VALUE +00457.
01020              15  FILLER              PIC S9(5)   VALUE +00597.
01021              15  FILLER              PIC S9(5)   VALUE +00781.
01022              15  FILLER              PIC S9(5)   VALUE +01107.
01023              15  FILLER              PIC S9(5)   VALUE +01515.
01024              15  FILLER              PIC S9(5)   VALUE +02181.
01025              15  FILLER              PIC S9(5)   VALUE +03153.
01026
01027          10  FILLER.
01028              15  FILLER              PIC S9(5)   VALUE +00307.
01029              15  FILLER              PIC S9(5)   VALUE +00320.
01030              15  FILLER              PIC S9(5)   VALUE +00350.
01031              15  FILLER              PIC S9(5)   VALUE +00395.
01032              15  FILLER              PIC S9(5)   VALUE +00468.
01033              15  FILLER              PIC S9(5)   VALUE +00612.
01034              15  FILLER              PIC S9(5)   VALUE +00800.
01035              15  FILLER              PIC S9(5)   VALUE +01134.
01036              15  FILLER              PIC S9(5)   VALUE +01549.
01037              15  FILLER              PIC S9(5)   VALUE +02221.
01038              15  FILLER              PIC S9(5)   VALUE +03194.
01039
01040          10  FILLER.
01041              15  FILLER              PIC S9(5)   VALUE +00340.
01042              15  FILLER              PIC S9(5)   VALUE +00348.
01043              15  FILLER              PIC S9(5)   VALUE +00365.
01044              15  FILLER              PIC S9(5)   VALUE +00000.
01045              15  FILLER              PIC S9(5)   VALUE +00000.
01046              15  FILLER              PIC S9(5)   VALUE +00000.
01047              15  FILLER              PIC S9(5)   VALUE +00000.
01048              15  FILLER              PIC S9(5)   VALUE +00000.
01049              15  FILLER              PIC S9(5)   VALUE +00000.
01050              15  FILLER              PIC S9(5)   VALUE +00000.
01051              15  FILLER              PIC S9(5)   VALUE +00000.
01052
01053          10  FILLER.
01054              15  FILLER              PIC S9(5)   VALUE +00346.
01055              15  FILLER              PIC S9(5)   VALUE +00358.
01056              15  FILLER              PIC S9(5)   VALUE +00384.
01057              15  FILLER              PIC S9(5)   VALUE +00415.
01058              15  FILLER              PIC S9(5)   VALUE +00000.
01059              15  FILLER              PIC S9(5)   VALUE +00000.
01060              15  FILLER              PIC S9(5)   VALUE +00000.
01061              15  FILLER              PIC S9(5)   VALUE +00000.
01062              15  FILLER              PIC S9(5)   VALUE +00000.
01063              15  FILLER              PIC S9(5)   VALUE +00000.
01064              15  FILLER              PIC S9(5)   VALUE +00000.
01065
01066          10  FILLER.
01067              15  FILLER              PIC S9(5)   VALUE +00350.
01068              15  FILLER              PIC S9(5)   VALUE +00365.
01069              15  FILLER              PIC S9(5)   VALUE +00396.
01070              15  FILLER              PIC S9(5)   VALUE +00440.
01071              15  FILLER              PIC S9(5)   VALUE +00494.
01072              15  FILLER              PIC S9(5)   VALUE +00000.
01073              15  FILLER              PIC S9(5)   VALUE +00000.
01074              15  FILLER              PIC S9(5)   VALUE +00000.
01075              15  FILLER              PIC S9(5)   VALUE +00000.
01076              15  FILLER              PIC S9(5)   VALUE +00000.
01077              15  FILLER              PIC S9(5)   VALUE +00000.
01078
01079          10  FILLER.
01080              15  FILLER              PIC S9(5)   VALUE +00352.
01081              15  FILLER              PIC S9(5)   VALUE +00370.
01082              15  FILLER              PIC S9(5)   VALUE +00404.
01083              15  FILLER              PIC S9(5)   VALUE +00456.
01084              15  FILLER              PIC S9(5)   VALUE +00526.
01085              15  FILLER              PIC S9(5)   VALUE +00647.
01086              15  FILLER              PIC S9(5)   VALUE +00000.
01087              15  FILLER              PIC S9(5)   VALUE +00000.
01088              15  FILLER              PIC S9(5)   VALUE +00000.
01089              15  FILLER              PIC S9(5)   VALUE +00000.
01090              15  FILLER              PIC S9(5)   VALUE +00000.
01091
01092          10  FILLER.
01093              15  FILLER              PIC S9(5)   VALUE +00355.
01094              15  FILLER              PIC S9(5)   VALUE +00375.
01095              15  FILLER              PIC S9(5)   VALUE +00414.
01096              15  FILLER              PIC S9(5)   VALUE +00477.
01097              15  FILLER              PIC S9(5)   VALUE +00567.
01098              15  FILLER              PIC S9(5)   VALUE +00732.
01099              15  FILLER              PIC S9(5)   VALUE +00940.
01100              15  FILLER              PIC S9(5)   VALUE +01298.
01101              15  FILLER              PIC S9(5)   VALUE +01708.
01102              15  FILLER              PIC S9(5)   VALUE +02366.
01103              15  FILLER              PIC S9(5)   VALUE +03298.
01104
01105      05  TAB-03-VALUES.
01106
01107          10  FILLER.
01108              15  FILLER              PIC S9(5)   VALUE +00041.
01109              15  FILLER              PIC S9(5)   VALUE +00041.
01110              15  FILLER              PIC S9(5)   VALUE +00041.
01111              15  FILLER              PIC S9(5)   VALUE +00041.
01112              15  FILLER              PIC S9(5)   VALUE +00041.
01113              15  FILLER              PIC S9(5)   VALUE +00042.
01114              15  FILLER              PIC S9(5)   VALUE +00043.
01115              15  FILLER              PIC S9(5)   VALUE +00044.
01116              15  FILLER              PIC S9(5)   VALUE +00045.
01117              15  FILLER              PIC S9(5)   VALUE +00047.
01118              15  FILLER              PIC S9(5)   VALUE +00048.
01119
01120          10  FILLER.
01121              15  FILLER              PIC S9(5)   VALUE +00108.
01122              15  FILLER              PIC S9(5)   VALUE +00109.
01123              15  FILLER              PIC S9(5)   VALUE +00108.
01124              15  FILLER              PIC S9(5)   VALUE +00109.
01125              15  FILLER              PIC S9(5)   VALUE +00111.
01126              15  FILLER              PIC S9(5)   VALUE +00113.
01127              15  FILLER              PIC S9(5)   VALUE +00117.
01128              15  FILLER              PIC S9(5)   VALUE +00122.
01129              15  FILLER              PIC S9(5)   VALUE +00127.
01130              15  FILLER              PIC S9(5)   VALUE +00134.
01131              15  FILLER              PIC S9(5)   VALUE +00142.
01132
01133          10  FILLER.
01134              15  FILLER              PIC S9(5)   VALUE +00155.
01135              15  FILLER              PIC S9(5)   VALUE +00155.
01136              15  FILLER              PIC S9(5)   VALUE +00155.
01137              15  FILLER              PIC S9(5)   VALUE +00155.
01138              15  FILLER              PIC S9(5)   VALUE +00159.
01139              15  FILLER              PIC S9(5)   VALUE +00166.
01140              15  FILLER              PIC S9(5)   VALUE +00173.
01141              15  FILLER              PIC S9(5)   VALUE +00184.
01142              15  FILLER              PIC S9(5)   VALUE +00196.
01143              15  FILLER              PIC S9(5)   VALUE +00212.
01144              15  FILLER              PIC S9(5)   VALUE +00231.
01145
01146          10  FILLER.
01147              15  FILLER              PIC S9(5)   VALUE +00190.
01148              15  FILLER              PIC S9(5)   VALUE +00190.
01149              15  FILLER              PIC S9(5)   VALUE +00190.
01150              15  FILLER              PIC S9(5)   VALUE +00191.
01151              15  FILLER              PIC S9(5)   VALUE +00197.
01152              15  FILLER              PIC S9(5)   VALUE +00207.
01153              15  FILLER              PIC S9(5)   VALUE +00219.
01154              15  FILLER              PIC S9(5)   VALUE +00238.
01155              15  FILLER              PIC S9(5)   VALUE +00257.
01156              15  FILLER              PIC S9(5)   VALUE +00283.
01157              15  FILLER              PIC S9(5)   VALUE +00315.
01158
01159          10  FILLER.
01160              15  FILLER              PIC S9(5)   VALUE +00219.
01161              15  FILLER              PIC S9(5)   VALUE +00219.
01162              15  FILLER              PIC S9(5)   VALUE +00220.
01163              15  FILLER              PIC S9(5)   VALUE +00221.
01164              15  FILLER              PIC S9(5)   VALUE +00229.
01165              15  FILLER              PIC S9(5)   VALUE +00243.
01166              15  FILLER              PIC S9(5)   VALUE +00260.
01167              15  FILLER              PIC S9(5)   VALUE +00286.
01168              15  FILLER              PIC S9(5)   VALUE +00313.
01169              15  FILLER              PIC S9(5)   VALUE +00350.
01170              15  FILLER              PIC S9(5)   VALUE +00395.
01171
01172          10  FILLER.
01173              15  FILLER              PIC S9(5)   VALUE +00244.
01174              15  FILLER              PIC S9(5)   VALUE +00245.
01175              15  FILLER              PIC S9(5)   VALUE +00246.
01176              15  FILLER              PIC S9(5)   VALUE +00247.
01177              15  FILLER              PIC S9(5)   VALUE +00258.
01178              15  FILLER              PIC S9(5)   VALUE +00276.
01179              15  FILLER              PIC S9(5)   VALUE +00297.
01180              15  FILLER              PIC S9(5)   VALUE +00331.
01181              15  FILLER              PIC S9(5)   VALUE +00364.
01182              15  FILLER              PIC S9(5)   VALUE +00413.
01183              15  FILLER              PIC S9(5)   VALUE +00471.
01184
01185          10  FILLER.
01186              15  FILLER              PIC S9(5)   VALUE +00267.
01187              15  FILLER              PIC S9(5)   VALUE +00268.
01188              15  FILLER              PIC S9(5)   VALUE +00269.
01189              15  FILLER              PIC S9(5)   VALUE +00271.
01190              15  FILLER              PIC S9(5)   VALUE +00284.
01191              15  FILLER              PIC S9(5)   VALUE +00306.
01192              15  FILLER              PIC S9(5)   VALUE +00332.
01193              15  FILLER              PIC S9(5)   VALUE +00373.
01194              15  FILLER              PIC S9(5)   VALUE +00413.
01195              15  FILLER              PIC S9(5)   VALUE +00473.
01196              15  FILLER              PIC S9(5)   VALUE +00543.
01197
01198          10  FILLER.
01199              15  FILLER              PIC S9(5)   VALUE +00288.
01200              15  FILLER              PIC S9(5)   VALUE +00289.
01201              15  FILLER              PIC S9(5)   VALUE +00291.
01202              15  FILLER              PIC S9(5)   VALUE +00294.
01203              15  FILLER              PIC S9(5)   VALUE +00308.
01204              15  FILLER              PIC S9(5)   VALUE +00335.
01205              15  FILLER              PIC S9(5)   VALUE +00364.
01206              15  FILLER              PIC S9(5)   VALUE +00413.
01207              15  FILLER              PIC S9(5)   VALUE +00459.
01208              15  FILLER              PIC S9(5)   VALUE +00530.
01209              15  FILLER              PIC S9(5)   VALUE +00612.
01210
01211          10  FILLER.
01212              15  FILLER              PIC S9(5)   VALUE +00309.
01213              15  FILLER              PIC S9(5)   VALUE +00309.
01214              15  FILLER              PIC S9(5)   VALUE +00312.
01215              15  FILLER              PIC S9(5)   VALUE +00315.
01216              15  FILLER              PIC S9(5)   VALUE +00331.
01217              15  FILLER              PIC S9(5)   VALUE +00362.
01218              15  FILLER              PIC S9(5)   VALUE +00396.
01219              15  FILLER              PIC S9(5)   VALUE +00451.
01220              15  FILLER              PIC S9(5)   VALUE +00504.
01221              15  FILLER              PIC S9(5)   VALUE +00585.
01222              15  FILLER              PIC S9(5)   VALUE +00678.
01223
01224          10  FILLER.
01225              15  FILLER              PIC S9(5)   VALUE +00328.
01226              15  FILLER              PIC S9(5)   VALUE +00328.
01227              15  FILLER              PIC S9(5)   VALUE +00332.
01228              15  FILLER              PIC S9(5)   VALUE +00336.
01229              15  FILLER              PIC S9(5)   VALUE +00353.
01230              15  FILLER              PIC S9(5)   VALUE +00388.
01231              15  FILLER              PIC S9(5)   VALUE +00426.
01232              15  FILLER              PIC S9(5)   VALUE +00489.
01233              15  FILLER              PIC S9(5)   VALUE +00547.
01234              15  FILLER              PIC S9(5)   VALUE +00638.
01235              15  FILLER              PIC S9(5)   VALUE +00742.
01236
01237          10  FILLER.
01238              15  FILLER              PIC S9(5)   VALUE +00347.
01239              15  FILLER              PIC S9(5)   VALUE +00347.
01240              15  FILLER              PIC S9(5)   VALUE +00351.
01241              15  FILLER              PIC S9(5)   VALUE +00356.
01242              15  FILLER              PIC S9(5)   VALUE +00375.
01243              15  FILLER              PIC S9(5)   VALUE +00414.
01244              15  FILLER              PIC S9(5)   VALUE +00456.
01245              15  FILLER              PIC S9(5)   VALUE +00525.
01246              15  FILLER              PIC S9(5)   VALUE +00589.
01247              15  FILLER              PIC S9(5)   VALUE +00691.
01248              15  FILLER              PIC S9(5)   VALUE +00805.
01249
01250          10  FILLER.
01251              15  FILLER              PIC S9(5)   VALUE +00364.
01252              15  FILLER              PIC S9(5)   VALUE +00365.
01253              15  FILLER              PIC S9(5)   VALUE +00369.
01254              15  FILLER              PIC S9(5)   VALUE +00375.
01255              15  FILLER              PIC S9(5)   VALUE +00396.
01256              15  FILLER              PIC S9(5)   VALUE +00439.
01257              15  FILLER              PIC S9(5)   VALUE +00485.
01258              15  FILLER              PIC S9(5)   VALUE +00561.
01259              15  FILLER              PIC S9(5)   VALUE +00631.
01260              15  FILLER              PIC S9(5)   VALUE +00742.
01261              15  FILLER              PIC S9(5)   VALUE +00867.
01262
01263          10  FILLER.
01264              15  FILLER              PIC S9(5)   VALUE +00382.
01265              15  FILLER              PIC S9(5)   VALUE +00382.
01266              15  FILLER              PIC S9(5)   VALUE +00387.
01267              15  FILLER              PIC S9(5)   VALUE +00394.
01268              15  FILLER              PIC S9(5)   VALUE +00416.
01269              15  FILLER              PIC S9(5)   VALUE +00463.
01270              15  FILLER              PIC S9(5)   VALUE +00513.
01271              15  FILLER              PIC S9(5)   VALUE +00597.
01272              15  FILLER              PIC S9(5)   VALUE +00672.
01273              15  FILLER              PIC S9(5)   VALUE +00793.
01274              15  FILLER              PIC S9(5)   VALUE +00929.
01275
01276          10  FILLER.
01277              15  FILLER              PIC S9(5)   VALUE +00398.
01278              15  FILLER              PIC S9(5)   VALUE +00399.
01279              15  FILLER              PIC S9(5)   VALUE +00404.
01280              15  FILLER              PIC S9(5)   VALUE +00411.
01281              15  FILLER              PIC S9(5)   VALUE +00436.
01282              15  FILLER              PIC S9(5)   VALUE +00487.
01283              15  FILLER              PIC S9(5)   VALUE +00541.
01284              15  FILLER              PIC S9(5)   VALUE +00631.
01285              15  FILLER              PIC S9(5)   VALUE +00713.
01286              15  FILLER              PIC S9(5)   VALUE +00843.
01287              15  FILLER              PIC S9(5)   VALUE +00989.
01288
01289          10  FILLER.
01290              15  FILLER              PIC S9(5)   VALUE +00413.
01291              15  FILLER              PIC S9(5)   VALUE +00414.
01292              15  FILLER              PIC S9(5)   VALUE +00421.
01293              15  FILLER              PIC S9(5)   VALUE +00429.
01294              15  FILLER              PIC S9(5)   VALUE +00455.
01295              15  FILLER              PIC S9(5)   VALUE +00510.
01296              15  FILLER              PIC S9(5)   VALUE +00568.
01297              15  FILLER              PIC S9(5)   VALUE +00666.
01298              15  FILLER              PIC S9(5)   VALUE +00753.
01299              15  FILLER              PIC S9(5)   VALUE +00892.
01300              15  FILLER              PIC S9(5)   VALUE +01049.
01301
01302          10  FILLER.
01303              15  FILLER              PIC S9(5)   VALUE +00429.
01304              15  FILLER              PIC S9(5)   VALUE +00430.
01305              15  FILLER              PIC S9(5)   VALUE +00437.
01306              15  FILLER              PIC S9(5)   VALUE +00446.
01307              15  FILLER              PIC S9(5)   VALUE +00474.
01308              15  FILLER              PIC S9(5)   VALUE +00532.
01309              15  FILLER              PIC S9(5)   VALUE +00595.
01310              15  FILLER              PIC S9(5)   VALUE +00699.
01311              15  FILLER              PIC S9(5)   VALUE +00792.
01312              15  FILLER              PIC S9(5)   VALUE +00941.
01313              15  FILLER              PIC S9(5)   VALUE +01108.
01314
01315          10  FILLER.
01316              15  FILLER              PIC S9(5)   VALUE +00444.
01317              15  FILLER              PIC S9(5)   VALUE +00445.
01318              15  FILLER              PIC S9(5)   VALUE +00453.
01319              15  FILLER              PIC S9(5)   VALUE +00463.
01320              15  FILLER              PIC S9(5)   VALUE +00492.
01321              15  FILLER              PIC S9(5)   VALUE +00554.
01322              15  FILLER              PIC S9(5)   VALUE +00621.
01323              15  FILLER              PIC S9(5)   VALUE +00732.
01324              15  FILLER              PIC S9(5)   VALUE +00831.
01325              15  FILLER              PIC S9(5)   VALUE +00989.
01326              15  FILLER              PIC S9(5)   VALUE +01166.
01327
01328          10  FILLER.
01329              15  FILLER              PIC S9(5)   VALUE +00458.
01330              15  FILLER              PIC S9(5)   VALUE +00460.
01331              15  FILLER              PIC S9(5)   VALUE +00469.
01332              15  FILLER              PIC S9(5)   VALUE +00479.
01333              15  FILLER              PIC S9(5)   VALUE +00510.
01334              15  FILLER              PIC S9(5)   VALUE +00576.
01335              15  FILLER              PIC S9(5)   VALUE +00647.
01336              15  FILLER              PIC S9(5)   VALUE +00765.
01337              15  FILLER              PIC S9(5)   VALUE +00870.
01338              15  FILLER              PIC S9(5)   VALUE +01037.
01339              15  FILLER              PIC S9(5)   VALUE +01224.
01340
01341          10  FILLER.
01342              15  FILLER              PIC S9(5)   VALUE +00472.
01343              15  FILLER              PIC S9(5)   VALUE +00474.
01344              15  FILLER              PIC S9(5)   VALUE +00484.
01345              15  FILLER              PIC S9(5)   VALUE +00494.
01346              15  FILLER              PIC S9(5)   VALUE +00528.
01347              15  FILLER              PIC S9(5)   VALUE +00597.
01348              15  FILLER              PIC S9(5)   VALUE +00672.
01349              15  FILLER              PIC S9(5)   VALUE +00797.
01350              15  FILLER              PIC S9(5)   VALUE +00908.
01351              15  FILLER              PIC S9(5)   VALUE +01084.
01352              15  FILLER              PIC S9(5)   VALUE +01281.
01353
01354          10  FILLER.
01355              15  FILLER              PIC S9(5)   VALUE +00485.
01356              15  FILLER              PIC S9(5)   VALUE +00488.
01357              15  FILLER              PIC S9(5)   VALUE +00498.
01358              15  FILLER              PIC S9(5)   VALUE +00510.
01359              15  FILLER              PIC S9(5)   VALUE +00545.
01360              15  FILLER              PIC S9(5)   VALUE +00618.
01361              15  FILLER              PIC S9(5)   VALUE +00697.
01362              15  FILLER              PIC S9(5)   VALUE +00829.
01363              15  FILLER              PIC S9(5)   VALUE +00945.
01364              15  FILLER              PIC S9(5)   VALUE +01130.
01365              15  FILLER              PIC S9(5)   VALUE +01338.
01366
01367          10  FILLER.
01368              15  FILLER              PIC S9(5)   VALUE +00498.
01369              15  FILLER              PIC S9(5)   VALUE +00501.
01370              15  FILLER              PIC S9(5)   VALUE +00512.
01371              15  FILLER              PIC S9(5)   VALUE +00524.
01372              15  FILLER              PIC S9(5)   VALUE +00561.
01373              15  FILLER              PIC S9(5)   VALUE +00638.
01374              15  FILLER              PIC S9(5)   VALUE +00721.
01375              15  FILLER              PIC S9(5)   VALUE +00859.
01376              15  FILLER              PIC S9(5)   VALUE +00981.
01377              15  FILLER              PIC S9(5)   VALUE +01174.
01378              15  FILLER              PIC S9(5)   VALUE +01392.
01379
01380          10  FILLER.
01381              15  FILLER              PIC S9(5)   VALUE +00509.
01382              15  FILLER              PIC S9(5)   VALUE +00513.
01383              15  FILLER              PIC S9(5)   VALUE +00524.
01384              15  FILLER              PIC S9(5)   VALUE +00538.
01385              15  FILLER              PIC S9(5)   VALUE +00576.
01386              15  FILLER              PIC S9(5)   VALUE +00656.
01387              15  FILLER              PIC S9(5)   VALUE +00743.
01388              15  FILLER              PIC S9(5)   VALUE +00888.
01389              15  FILLER              PIC S9(5)   VALUE +01015.
01390              15  FILLER              PIC S9(5)   VALUE +01217.
01391              15  FILLER              PIC S9(5)   VALUE +01443.
01392
01393          10  FILLER.
01394              15  FILLER              PIC S9(5)   VALUE +00521.
01395              15  FILLER              PIC S9(5)   VALUE +00525.
01396              15  FILLER              PIC S9(5)   VALUE +00537.
01397              15  FILLER              PIC S9(5)   VALUE +00551.
01398              15  FILLER              PIC S9(5)   VALUE +00591.
01399              15  FILLER              PIC S9(5)   VALUE +00675.
01400              15  FILLER              PIC S9(5)   VALUE +00766.
01401              15  FILLER              PIC S9(5)   VALUE +00917.
01402              15  FILLER              PIC S9(5)   VALUE +01049.
01403              15  FILLER              PIC S9(5)   VALUE +01260.
01404              15  FILLER              PIC S9(5)   VALUE +01495.
01405
01406          10  FILLER.
01407              15  FILLER              PIC S9(5)   VALUE +00533.
01408              15  FILLER              PIC S9(5)   VALUE +00537.
01409              15  FILLER              PIC S9(5)   VALUE +00550.
01410              15  FILLER              PIC S9(5)   VALUE +00565.
01411              15  FILLER              PIC S9(5)   VALUE +00607.
01412              15  FILLER              PIC S9(5)   VALUE +00693.
01413              15  FILLER              PIC S9(5)   VALUE +00788.
01414              15  FILLER              PIC S9(5)   VALUE +00945.
01415              15  FILLER              PIC S9(5)   VALUE +01083.
01416              15  FILLER              PIC S9(5)   VALUE +01302.
01417              15  FILLER              PIC S9(5)   VALUE +01547.
01418
01419          10  FILLER.
01420              15  FILLER              PIC S9(5)   VALUE +00608.
01421              15  FILLER              PIC S9(5)   VALUE +00615.
01422              15  FILLER              PIC S9(5)   VALUE +00632.
01423              15  FILLER              PIC S9(5)   VALUE +00652.
01424              15  FILLER              PIC S9(5)   VALUE +00705.
01425              15  FILLER              PIC S9(5)   VALUE +00814.
01426              15  FILLER              PIC S9(5)   VALUE +00934.
01427              15  FILLER              PIC S9(5)   VALUE +01133.
01428              15  FILLER              PIC S9(5)   VALUE +01306.
01429              15  FILLER              PIC S9(5)   VALUE +01579.
01430              15  FILLER              PIC S9(5)   VALUE +01885.
01431
01432          10  FILLER.
01433              15  FILLER              PIC S9(5)   VALUE +00719.
01434              15  FILLER              PIC S9(5)   VALUE +00731.
01435              15  FILLER              PIC S9(5)   VALUE +00757.
01436              15  FILLER              PIC S9(5)   VALUE +00788.
01437              15  FILLER              PIC S9(5)   VALUE +00861.
01438              15  FILLER              PIC S9(5)   VALUE +01006.
01439              15  FILLER              PIC S9(5)   VALUE +01169.
01440              15  FILLER              PIC S9(5)   VALUE +01437.
01441              15  FILLER              PIC S9(5)   VALUE +01668.
01442              15  FILLER              PIC S9(5)   VALUE +02030.
01443              15  FILLER              PIC S9(5)   VALUE +02435.
01444
01445          10  FILLER.
01446              15  FILLER              PIC S9(5)   VALUE +00807.
01447              15  FILLER              PIC S9(5)   VALUE +00824.
01448              15  FILLER              PIC S9(5)   VALUE +00863.
01449              15  FILLER              PIC S9(5)   VALUE +00903.
01450              15  FILLER              PIC S9(5)   VALUE +00994.
01451              15  FILLER              PIC S9(5)   VALUE +01173.
01452              15  FILLER              PIC S9(5)   VALUE +01374.
01453              15  FILLER              PIC S9(5)   VALUE +01702.
01454              15  FILLER              PIC S9(5)   VALUE +01983.
01455              15  FILLER              PIC S9(5)   VALUE +02421.
01456              15  FILLER              PIC S9(5)   VALUE +02908.
01457
01458          10  FILLER.
01459              15  FILLER              PIC S9(5)   VALUE +00880.
01460              15  FILLER              PIC S9(5)   VALUE +00905.
01461              15  FILLER              PIC S9(5)   VALUE +00953.
01462              15  FILLER              PIC S9(5)   VALUE +01004.
01463              15  FILLER              PIC S9(5)   VALUE +01111.
01464              15  FILLER              PIC S9(5)   VALUE +01322.
01465              15  FILLER              PIC S9(5)   VALUE +01555.
01466              15  FILLER              PIC S9(5)   VALUE +01936.
01467              15  FILLER              PIC S9(5)   VALUE +02259.
01468              15  FILLER              PIC S9(5)   VALUE +02761.
01469              15  FILLER              PIC S9(5)   VALUE +03313.
01470
01471          10  FILLER.
01472              15  FILLER              PIC S9(5)   VALUE +00943.
01473              15  FILLER              PIC S9(5)   VALUE +00976.
01474              15  FILLER              PIC S9(5)   VALUE +01032.
01475              15  FILLER              PIC S9(5)   VALUE +01094.
01476              15  FILLER              PIC S9(5)   VALUE +01216.
01477              15  FILLER              PIC S9(5)   VALUE +01455.
01478              15  FILLER              PIC S9(5)   VALUE +01718.
01479              15  FILLER              PIC S9(5)   VALUE +02144.
01480              15  FILLER              PIC S9(5)   VALUE +02502.
01481              15  FILLER              PIC S9(5)   VALUE +03057.
01482              15  FILLER              PIC S9(5)   VALUE +03656.
01483
01484          10  FILLER.
01485              15  FILLER              PIC S9(5)   VALUE +00997.
01486              15  FILLER              PIC S9(5)   VALUE +01040.
01487              15  FILLER              PIC S9(5)   VALUE +01104.
01488              15  FILLER              PIC S9(5)   VALUE +01176.
01489              15  FILLER              PIC S9(5)   VALUE +01311.
01490              15  FILLER              PIC S9(5)   VALUE +01575.
01491              15  FILLER              PIC S9(5)   VALUE +01865.
01492              15  FILLER              PIC S9(5)   VALUE +02330.
01493              15  FILLER              PIC S9(5)   VALUE +02716.
01494              15  FILLER              PIC S9(5)   VALUE +03315.
01495              15  FILLER              PIC S9(5)   VALUE +03946.
01496
01497          10  FILLER.
01498              15  FILLER              PIC S9(5)   VALUE +01046.
01499              15  FILLER              PIC S9(5)   VALUE +01094.
01500              15  FILLER              PIC S9(5)   VALUE +01168.
01501              15  FILLER              PIC S9(5)   VALUE +01250.
01502              15  FILLER              PIC S9(5)   VALUE +01398.
01503              15  FILLER              PIC S9(5)   VALUE +01684.
01504              15  FILLER              PIC S9(5)   VALUE +01997.
01505              15  FILLER              PIC S9(5)   VALUE +02496.
01506              15  FILLER              PIC S9(5)   VALUE +02905.
01507              15  FILLER              PIC S9(5)   VALUE +03538.
01508              15  FILLER              PIC S9(5)   VALUE +04188.
01509
01510          10  FILLER.
01511              15  FILLER              PIC S9(5)   VALUE +01090.
01512              15  FILLER              PIC S9(5)   VALUE +01144.
01513              15  FILLER              PIC S9(5)   VALUE +01225.
01514              15  FILLER              PIC S9(5)   VALUE +01317.
01515              15  FILLER              PIC S9(5)   VALUE +01477.
01516              15  FILLER              PIC S9(5)   VALUE +01783.
01517              15  FILLER              PIC S9(5)   VALUE +02115.
01518              15  FILLER              PIC S9(5)   VALUE +02645.
01519              15  FILLER              PIC S9(5)   VALUE +03072.
01520              15  FILLER              PIC S9(5)   VALUE +03731.
01521              15  FILLER              PIC S9(5)   VALUE +04388.
01522
01523          10  FILLER.
01524              15  FILLER              PIC S9(5)   VALUE +01129.
01525              15  FILLER              PIC S9(5)   VALUE +01189.
01526              15  FILLER              PIC S9(5)   VALUE +01278.
01527              15  FILLER              PIC S9(5)   VALUE +01378.
01528              15  FILLER              PIC S9(5)   VALUE +01550.
01529              15  FILLER              PIC S9(5)   VALUE +01873.
01530              15  FILLER              PIC S9(5)   VALUE +02222.
01531              15  FILLER              PIC S9(5)   VALUE +02777.
01532              15  FILLER              PIC S9(5)   VALUE +03217.
01533              15  FILLER              PIC S9(5)   VALUE +03895.
01534              15  FILLER              PIC S9(5)   VALUE +04551.
01535
01536          10  FILLER.
01537              15  FILLER              PIC S9(5)   VALUE +01164.
01538              15  FILLER              PIC S9(5)   VALUE +01230.
01539              15  FILLER              PIC S9(5)   VALUE +01326.
01540              15  FILLER              PIC S9(5)   VALUE +01434.
01541              15  FILLER              PIC S9(5)   VALUE +01616.
01542              15  FILLER              PIC S9(5)   VALUE +01956.
01543              15  FILLER              PIC S9(5)   VALUE +02320.
01544              15  FILLER              PIC S9(5)   VALUE +02895.
01545              15  FILLER              PIC S9(5)   VALUE +03345.
01546              15  FILLER              PIC S9(5)   VALUE +04036.
01547              15  FILLER              PIC S9(5)   VALUE +04683.
01548
01549          10  FILLER.
01550              15  FILLER              PIC S9(5)   VALUE +01196.
01551              15  FILLER              PIC S9(5)   VALUE +01266.
01552              15  FILLER              PIC S9(5)   VALUE +01370.
01553              15  FILLER              PIC S9(5)   VALUE +01485.
01554              15  FILLER              PIC S9(5)   VALUE +01678.
01555              15  FILLER              PIC S9(5)   VALUE +02030.
01556              15  FILLER              PIC S9(5)   VALUE +02408.
01557              15  FILLER              PIC S9(5)   VALUE +03001.
01558              15  FILLER              PIC S9(5)   VALUE +03457.
01559              15  FILLER              PIC S9(5)   VALUE +04154.
01560              15  FILLER              PIC S9(5)   VALUE +04787.
01561
01562          10  FILLER.
01563              15  FILLER              PIC S9(5)   VALUE +01225.
01564              15  FILLER              PIC S9(5)   VALUE +01299.
01565              15  FILLER              PIC S9(5)   VALUE +01411.
01566              15  FILLER              PIC S9(5)   VALUE +01533.
01567              15  FILLER              PIC S9(5)   VALUE +01734.
01568              15  FILLER              PIC S9(5)   VALUE +02099.
01569              15  FILLER              PIC S9(5)   VALUE +02488.
01570              15  FILLER              PIC S9(5)   VALUE +03095.
01571              15  FILLER              PIC S9(5)   VALUE +03554.
01572              15  FILLER              PIC S9(5)   VALUE +04253.
01573              15  FILLER              PIC S9(5)   VALUE +04869.
01574
01575          10  FILLER.
01576              15  FILLER              PIC S9(5)   VALUE +01251.
01577              15  FILLER              PIC S9(5)   VALUE +01329.
01578              15  FILLER              PIC S9(5)   VALUE +01448.
01579              15  FILLER              PIC S9(5)   VALUE +01576.
01580              15  FILLER              PIC S9(5)   VALUE +01786.
01581              15  FILLER              PIC S9(5)   VALUE +02161.
01582              15  FILLER              PIC S9(5)   VALUE +02560.
01583              15  FILLER              PIC S9(5)   VALUE +03179.
01584              15  FILLER              PIC S9(5)   VALUE +03639.
01585              15  FILLER              PIC S9(5)   VALUE +04335.
01586              15  FILLER              PIC S9(5)   VALUE +04931.
01587
01588          10  FILLER.
01589              15  FILLER              PIC S9(5)   VALUE +01453.
01590              15  FILLER              PIC S9(5)   VALUE +01499.
01591              15  FILLER              PIC S9(5)   VALUE +01530.
01592              15  FILLER              PIC S9(5)   VALUE +00000.
01593              15  FILLER              PIC S9(5)   VALUE +00000.
01594              15  FILLER              PIC S9(5)   VALUE +00000.
01595              15  FILLER              PIC S9(5)   VALUE +00000.
01596              15  FILLER              PIC S9(5)   VALUE +00000.
01597              15  FILLER              PIC S9(5)   VALUE +00000.
01598              15  FILLER              PIC S9(5)   VALUE +00000.
01599              15  FILLER              PIC S9(5)   VALUE +00000.
01600
01601          10  FILLER.
01602              15  FILLER              PIC S9(5)   VALUE +01488.
01603              15  FILLER              PIC S9(5)   VALUE +01562.
01604              15  FILLER              PIC S9(5)   VALUE +01641.
01605              15  FILLER              PIC S9(5)   VALUE +01679.
01606              15  FILLER              PIC S9(5)   VALUE +00000.
01607              15  FILLER              PIC S9(5)   VALUE +00000.
01608              15  FILLER              PIC S9(5)   VALUE +00000.
01609              15  FILLER              PIC S9(5)   VALUE +00000.
01610              15  FILLER              PIC S9(5)   VALUE +00000.
01611              15  FILLER              PIC S9(5)   VALUE +00000.
01612              15  FILLER              PIC S9(5)   VALUE +00000.
01613
01614          10  FILLER.
01615              15  FILLER              PIC S9(5)   VALUE +01511.
01616              15  FILLER              PIC S9(5)   VALUE +01604.
01617              15  FILLER              PIC S9(5)   VALUE +01714.
01618              15  FILLER              PIC S9(5)   VALUE +01818.
01619              15  FILLER              PIC S9(5)   VALUE +01907.
01620              15  FILLER              PIC S9(5)   VALUE +00000.
01621              15  FILLER              PIC S9(5)   VALUE +00000.
01622              15  FILLER              PIC S9(5)   VALUE +00000.
01623              15  FILLER              PIC S9(5)   VALUE +00000.
01624              15  FILLER              PIC S9(5)   VALUE +00000.
01625              15  FILLER              PIC S9(5)   VALUE +00000.
01626
01627          10  FILLER.
01628              15  FILLER              PIC S9(5)   VALUE +01526.
01629              15  FILLER              PIC S9(5)   VALUE +01631.
01630              15  FILLER              PIC S9(5)   VALUE +01761.
01631              15  FILLER              PIC S9(5)   VALUE +01908.
01632              15  FILLER              PIC S9(5)   VALUE +02069.
01633              15  FILLER              PIC S9(5)   VALUE +02301.
01634              15  FILLER              PIC S9(5)   VALUE +00000.
01635              15  FILLER              PIC S9(5)   VALUE +00000.
01636              15  FILLER              PIC S9(5)   VALUE +00000.
01637              15  FILLER              PIC S9(5)   VALUE +00000.
01638              15  FILLER              PIC S9(5)   VALUE +00000.
01639
01640          10  FILLER.
01641              15  FILLER              PIC S9(5)   VALUE +01545.
01642              15  FILLER              PIC S9(5)   VALUE +01665.
01643              15  FILLER              PIC S9(5)   VALUE +01821.
01644              15  FILLER              PIC S9(5)   VALUE +02023.
01645              15  FILLER              PIC S9(5)   VALUE +02274.
01646              15  FILLER              PIC S9(5)   VALUE +02673.
01647              15  FILLER              PIC S9(5)   VALUE +03077.
01648              15  FILLER              PIC S9(5)   VALUE +03682.
01649              15  FILLER              PIC S9(5)   VALUE +04035.
01650              15  FILLER              PIC S9(5)   VALUE +04622.
01651              15  FILLER              PIC S9(5)   VALUE +05089.
01652
01653      05  TAB-04-VALUES.
01654
01655          10  FILLER.
01656              15  FILLER              PIC S9(5)   VALUE +00048.
01657              15  FILLER              PIC S9(5)   VALUE +00048.
01658              15  FILLER              PIC S9(5)   VALUE +00048.
01659              15  FILLER              PIC S9(5)   VALUE +00048.
01660              15  FILLER              PIC S9(5)   VALUE +00048.
01661              15  FILLER              PIC S9(5)   VALUE +00048.
01662              15  FILLER              PIC S9(5)   VALUE +00049.
01663              15  FILLER              PIC S9(5)   VALUE +00049.
01664              15  FILLER              PIC S9(5)   VALUE +00049.
01665              15  FILLER              PIC S9(5)   VALUE +00049.
01666              15  FILLER              PIC S9(5)   VALUE +00049.
01667
01668          10  FILLER.
01669              15  FILLER              PIC S9(5)   VALUE +00140.
01670              15  FILLER              PIC S9(5)   VALUE +00140.
01671              15  FILLER              PIC S9(5)   VALUE +00140.
01672              15  FILLER              PIC S9(5)   VALUE +00141.
01673              15  FILLER              PIC S9(5)   VALUE +00141.
01674              15  FILLER              PIC S9(5)   VALUE +00142.
01675              15  FILLER              PIC S9(5)   VALUE +00143.
01676              15  FILLER              PIC S9(5)   VALUE +00143.
01677              15  FILLER              PIC S9(5)   VALUE +00143.
01678              15  FILLER              PIC S9(5)   VALUE +00144.
01679              15  FILLER              PIC S9(5)   VALUE +00144.
01680
01681          10  FILLER.
01682              15  FILLER              PIC S9(5)   VALUE +00225.
01683              15  FILLER              PIC S9(5)   VALUE +00224.
01684              15  FILLER              PIC S9(5)   VALUE +00225.
01685              15  FILLER              PIC S9(5)   VALUE +00228.
01686              15  FILLER              PIC S9(5)   VALUE +00228.
01687              15  FILLER              PIC S9(5)   VALUE +00230.
01688              15  FILLER              PIC S9(5)   VALUE +00232.
01689              15  FILLER              PIC S9(5)   VALUE +00234.
01690              15  FILLER              PIC S9(5)   VALUE +00233.
01691              15  FILLER              PIC S9(5)   VALUE +00234.
01692              15  FILLER              PIC S9(5)   VALUE +00235.
01693
01694          10  FILLER.
01695              15  FILLER              PIC S9(5)   VALUE +00305.
01696              15  FILLER              PIC S9(5)   VALUE +00306.
01697              15  FILLER              PIC S9(5)   VALUE +00305.
01698              15  FILLER              PIC S9(5)   VALUE +00311.
01699              15  FILLER              PIC S9(5)   VALUE +00310.
01700              15  FILLER              PIC S9(5)   VALUE +00314.
01701              15  FILLER              PIC S9(5)   VALUE +00318.
01702              15  FILLER              PIC S9(5)   VALUE +00320.
01703              15  FILLER              PIC S9(5)   VALUE +00319.
01704              15  FILLER              PIC S9(5)   VALUE +00321.
01705              15  FILLER              PIC S9(5)   VALUE +00321.
01706
01707          10  FILLER.
01708              15  FILLER              PIC S9(5)   VALUE +00384.
01709              15  FILLER              PIC S9(5)   VALUE +00382.
01710              15  FILLER              PIC S9(5)   VALUE +00383.
01711              15  FILLER              PIC S9(5)   VALUE +00390.
01712              15  FILLER              PIC S9(5)   VALUE +00390.
01713              15  FILLER              PIC S9(5)   VALUE +00395.
01714              15  FILLER              PIC S9(5)   VALUE +00402.
01715              15  FILLER              PIC S9(5)   VALUE +00405.
01716              15  FILLER              PIC S9(5)   VALUE +00403.
01717              15  FILLER              PIC S9(5)   VALUE +00405.
01718              15  FILLER              PIC S9(5)   VALUE +00406.
01719
01720          10  FILLER.
01721              15  FILLER              PIC S9(5)   VALUE +00457.
01722              15  FILLER              PIC S9(5)   VALUE +00457.
01723              15  FILLER              PIC S9(5)   VALUE +00456.
01724              15  FILLER              PIC S9(5)   VALUE +00466.
01725              15  FILLER              PIC S9(5)   VALUE +00467.
01726              15  FILLER              PIC S9(5)   VALUE +00474.
01727              15  FILLER              PIC S9(5)   VALUE +00483.
01728              15  FILLER              PIC S9(5)   VALUE +00487.
01729              15  FILLER              PIC S9(5)   VALUE +00486.
01730              15  FILLER              PIC S9(5)   VALUE +00488.
01731              15  FILLER              PIC S9(5)   VALUE +00489.
01732
01733          10  FILLER.
01734              15  FILLER              PIC S9(5)   VALUE +00529.
01735              15  FILLER              PIC S9(5)   VALUE +00528.
01736              15  FILLER              PIC S9(5)   VALUE +00528.
01737              15  FILLER              PIC S9(5)   VALUE +00539.
01738              15  FILLER              PIC S9(5)   VALUE +00540.
01739              15  FILLER              PIC S9(5)   VALUE +00551.
01740              15  FILLER              PIC S9(5)   VALUE +00562.
01741              15  FILLER              PIC S9(5)   VALUE +00568.
01742              15  FILLER              PIC S9(5)   VALUE +00567.
01743              15  FILLER              PIC S9(5)   VALUE +00570.
01744              15  FILLER              PIC S9(5)   VALUE +00572.
01745
01746          10  FILLER.
01747              15  FILLER              PIC S9(5)   VALUE +00597.
01748              15  FILLER              PIC S9(5)   VALUE +00597.
01749              15  FILLER              PIC S9(5)   VALUE +00597.
01750              15  FILLER              PIC S9(5)   VALUE +00611.
01751              15  FILLER              PIC S9(5)   VALUE +00612.
01752              15  FILLER              PIC S9(5)   VALUE +00626.
01753              15  FILLER              PIC S9(5)   VALUE +00639.
01754              15  FILLER              PIC S9(5)   VALUE +00648.
01755              15  FILLER              PIC S9(5)   VALUE +00647.
01756              15  FILLER              PIC S9(5)   VALUE +00651.
01757              15  FILLER              PIC S9(5)   VALUE +00653.
01758
01759          10  FILLER.
01760              15  FILLER              PIC S9(5)   VALUE +00662.
01761              15  FILLER              PIC S9(5)   VALUE +00662.
01762              15  FILLER              PIC S9(5)   VALUE +00665.
01763              15  FILLER              PIC S9(5)   VALUE +00679.
01764              15  FILLER              PIC S9(5)   VALUE +00683.
01765              15  FILLER              PIC S9(5)   VALUE +00699.
01766              15  FILLER              PIC S9(5)   VALUE +00715.
01767              15  FILLER              PIC S9(5)   VALUE +00726.
01768              15  FILLER              PIC S9(5)   VALUE +00726.
01769              15  FILLER              PIC S9(5)   VALUE +00730.
01770              15  FILLER              PIC S9(5)   VALUE +00732.
01771
01772          10  FILLER.
01773              15  FILLER              PIC S9(5)   VALUE +00724.
01774              15  FILLER              PIC S9(5)   VALUE +00726.
01775              15  FILLER              PIC S9(5)   VALUE +00730.
01776              15  FILLER              PIC S9(5)   VALUE +00747.
01777              15  FILLER              PIC S9(5)   VALUE +00751.
01778              15  FILLER              PIC S9(5)   VALUE +00770.
01779              15  FILLER              PIC S9(5)   VALUE +00789.
01780              15  FILLER              PIC S9(5)   VALUE +00803.
01781              15  FILLER              PIC S9(5)   VALUE +00803.
01782              15  FILLER              PIC S9(5)   VALUE +00808.
01783              15  FILLER              PIC S9(5)   VALUE +00811.
01784
01785          10  FILLER.
01786              15  FILLER              PIC S9(5)   VALUE +00785.
01787              15  FILLER              PIC S9(5)   VALUE +00788.
01788              15  FILLER              PIC S9(5)   VALUE +00794.
01789              15  FILLER              PIC S9(5)   VALUE +00813.
01790              15  FILLER              PIC S9(5)   VALUE +00818.
01791              15  FILLER              PIC S9(5)   VALUE +00839.
01792              15  FILLER              PIC S9(5)   VALUE +00862.
01793              15  FILLER              PIC S9(5)   VALUE +00879.
01794              15  FILLER              PIC S9(5)   VALUE +00880.
01795              15  FILLER              PIC S9(5)   VALUE +00886.
01796              15  FILLER              PIC S9(5)   VALUE +00889.
01797
01798          10  FILLER.
01799              15  FILLER              PIC S9(5)   VALUE +00844.
01800              15  FILLER              PIC S9(5)   VALUE +00848.
01801              15  FILLER              PIC S9(5)   VALUE +00855.
01802              15  FILLER              PIC S9(5)   VALUE +00876.
01803              15  FILLER              PIC S9(5)   VALUE +00884.
01804              15  FILLER              PIC S9(5)   VALUE +00908.
01805              15  FILLER              PIC S9(5)   VALUE +00934.
01806              15  FILLER              PIC S9(5)   VALUE +00953.
01807              15  FILLER              PIC S9(5)   VALUE +00956.
01808              15  FILLER              PIC S9(5)   VALUE +00962.
01809              15  FILLER              PIC S9(5)   VALUE +00966.
01810
01811          10  FILLER.
01812              15  FILLER              PIC S9(5)   VALUE +00901.
01813              15  FILLER              PIC S9(5)   VALUE +00906.
01814              15  FILLER              PIC S9(5)   VALUE +00915.
01815              15  FILLER              PIC S9(5)   VALUE +00938.
01816              15  FILLER              PIC S9(5)   VALUE +00947.
01817              15  FILLER              PIC S9(5)   VALUE +00974.
01818              15  FILLER              PIC S9(5)   VALUE +01004.
01819              15  FILLER              PIC S9(5)   VALUE +01027.
01820              15  FILLER              PIC S9(5)   VALUE +01030.
01821              15  FILLER              PIC S9(5)   VALUE +01037.
01822              15  FILLER              PIC S9(5)   VALUE +01042.
01823
01824          10  FILLER.
01825              15  FILLER              PIC S9(5)   VALUE +00956.
01826              15  FILLER              PIC S9(5)   VALUE +00963.
01827              15  FILLER              PIC S9(5)   VALUE +00974.
01828              15  FILLER              PIC S9(5)   VALUE +00999.
01829              15  FILLER              PIC S9(5)   VALUE +01010.
01830              15  FILLER              PIC S9(5)   VALUE +01040.
01831              15  FILLER              PIC S9(5)   VALUE +01073.
01832              15  FILLER              PIC S9(5)   VALUE +01099.
01833              15  FILLER              PIC S9(5)   VALUE +01104.
01834              15  FILLER              PIC S9(5)   VALUE +01112.
01835              15  FILLER              PIC S9(5)   VALUE +01117.
01836
01837          10  FILLER.
01838              15  FILLER              PIC S9(5)   VALUE +01011.
01839              15  FILLER              PIC S9(5)   VALUE +01019.
01840              15  FILLER              PIC S9(5)   VALUE +01030.
01841              15  FILLER              PIC S9(5)   VALUE +01058.
01842              15  FILLER              PIC S9(5)   VALUE +01071.
01843              15  FILLER              PIC S9(5)   VALUE +01105.
01844              15  FILLER              PIC S9(5)   VALUE +01141.
01845              15  FILLER              PIC S9(5)   VALUE +01170.
01846              15  FILLER              PIC S9(5)   VALUE +01176.
01847              15  FILLER              PIC S9(5)   VALUE +01185.
01848              15  FILLER              PIC S9(5)   VALUE +01191.
01849
01850          10  FILLER.
01851              15  FILLER              PIC S9(5)   VALUE +01060.
01852              15  FILLER              PIC S9(5)   VALUE +01071.
01853              15  FILLER              PIC S9(5)   VALUE +01083.
01854              15  FILLER              PIC S9(5)   VALUE +01113.
01855              15  FILLER              PIC S9(5)   VALUE +01129.
01856              15  FILLER              PIC S9(5)   VALUE +01165.
01857              15  FILLER              PIC S9(5)   VALUE +01206.
01858              15  FILLER              PIC S9(5)   VALUE +01238.
01859              15  FILLER              PIC S9(5)   VALUE +01246.
01860              15  FILLER              PIC S9(5)   VALUE +01256.
01861              15  FILLER              PIC S9(5)   VALUE +01262.
01862
01863          10  FILLER.
01864              15  FILLER              PIC S9(5)   VALUE +01107.
01865              15  FILLER              PIC S9(5)   VALUE +01119.
01866              15  FILLER              PIC S9(5)   VALUE +01132.
01867              15  FILLER              PIC S9(5)   VALUE +01165.
01868              15  FILLER              PIC S9(5)   VALUE +01183.
01869              15  FILLER              PIC S9(5)   VALUE +01222.
01870              15  FILLER              PIC S9(5)   VALUE +01267.
01871              15  FILLER              PIC S9(5)   VALUE +01303.
01872              15  FILLER              PIC S9(5)   VALUE +01312.
01873              15  FILLER              PIC S9(5)   VALUE +01323.
01874              15  FILLER              PIC S9(5)   VALUE +01331.
01875
01876          10  FILLER.
01877              15  FILLER              PIC S9(5)   VALUE +01153.
01878              15  FILLER              PIC S9(5)   VALUE +01167.
01879              15  FILLER              PIC S9(5)   VALUE +01181.
01880              15  FILLER              PIC S9(5)   VALUE +01217.
01881              15  FILLER              PIC S9(5)   VALUE +01237.
01882              15  FILLER              PIC S9(5)   VALUE +01280.
01883              15  FILLER              PIC S9(5)   VALUE +01328.
01884              15  FILLER              PIC S9(5)   VALUE +01368.
01885              15  FILLER              PIC S9(5)   VALUE +01378.
01886              15  FILLER              PIC S9(5)   VALUE +01391.
01887              15  FILLER              PIC S9(5)   VALUE +01399.
01888
01889          10  FILLER.
01890              15  FILLER              PIC S9(5)   VALUE +01199.
01891              15  FILLER              PIC S9(5)   VALUE +01214.
01892              15  FILLER              PIC S9(5)   VALUE +01230.
01893              15  FILLER              PIC S9(5)   VALUE +01269.
01894              15  FILLER              PIC S9(5)   VALUE +01291.
01895              15  FILLER              PIC S9(5)   VALUE +01337.
01896              15  FILLER              PIC S9(5)   VALUE +01389.
01897              15  FILLER              PIC S9(5)   VALUE +01433.
01898              15  FILLER              PIC S9(5)   VALUE +01445.
01899              15  FILLER              PIC S9(5)   VALUE +01458.
01900              15  FILLER              PIC S9(5)   VALUE +01468.
01901
01902          10  FILLER.
01903              15  FILLER              PIC S9(5)   VALUE +01245.
01904              15  FILLER              PIC S9(5)   VALUE +01262.
01905              15  FILLER              PIC S9(5)   VALUE +01279.
01906              15  FILLER              PIC S9(5)   VALUE +01321.
01907              15  FILLER              PIC S9(5)   VALUE +01345.
01908              15  FILLER              PIC S9(5)   VALUE +01394.
01909              15  FILLER              PIC S9(5)   VALUE +01450.
01910              15  FILLER              PIC S9(5)   VALUE +01498.
01911              15  FILLER              PIC S9(5)   VALUE +01511.
01912              15  FILLER              PIC S9(5)   VALUE +01526.
01913              15  FILLER              PIC S9(5)   VALUE +01536.
01914
01915          10  FILLER.
01916              15  FILLER              PIC S9(5)   VALUE +01291.
01917              15  FILLER              PIC S9(5)   VALUE +01310.
01918              15  FILLER              PIC S9(5)   VALUE +01328.
01919              15  FILLER              PIC S9(5)   VALUE +01372.
01920              15  FILLER              PIC S9(5)   VALUE +01400.
01921              15  FILLER              PIC S9(5)   VALUE +01452.
01922              15  FILLER              PIC S9(5)   VALUE +01512.
01923              15  FILLER              PIC S9(5)   VALUE +01562.
01924              15  FILLER              PIC S9(5)   VALUE +01578.
01925              15  FILLER              PIC S9(5)   VALUE +01593.
01926              15  FILLER              PIC S9(5)   VALUE +01605.
01927
01928          10  FILLER.
01929              15  FILLER              PIC S9(5)   VALUE +01337.
01930              15  FILLER              PIC S9(5)   VALUE +01358.
01931              15  FILLER              PIC S9(5)   VALUE +01377.
01932              15  FILLER              PIC S9(5)   VALUE +01424.
01933              15  FILLER              PIC S9(5)   VALUE +01454.
01934              15  FILLER              PIC S9(5)   VALUE +01509.
01935              15  FILLER              PIC S9(5)   VALUE +01573.
01936              15  FILLER              PIC S9(5)   VALUE +01627.
01937              15  FILLER              PIC S9(5)   VALUE +01644.
01938              15  FILLER              PIC S9(5)   VALUE +01661.
01939              15  FILLER              PIC S9(5)   VALUE +01673.
01940
01941          10  FILLER.
01942              15  FILLER              PIC S9(5)   VALUE +01384.
01943              15  FILLER              PIC S9(5)   VALUE +01406.
01944              15  FILLER              PIC S9(5)   VALUE +01426.
01945              15  FILLER              PIC S9(5)   VALUE +01476.
01946              15  FILLER              PIC S9(5)   VALUE +01508.
01947              15  FILLER              PIC S9(5)   VALUE +01566.
01948              15  FILLER              PIC S9(5)   VALUE +01634.
01949              15  FILLER              PIC S9(5)   VALUE +01692.
01950              15  FILLER              PIC S9(5)   VALUE +01711.
01951              15  FILLER              PIC S9(5)   VALUE +01728.
01952              15  FILLER              PIC S9(5)   VALUE +01741.
01953
01954          10  FILLER.
01955              15  FILLER              PIC S9(5)   VALUE +01430.
01956              15  FILLER              PIC S9(5)   VALUE +01453.
01957              15  FILLER              PIC S9(5)   VALUE +01475.
01958              15  FILLER              PIC S9(5)   VALUE +01528.
01959              15  FILLER              PIC S9(5)   VALUE +01562.
01960              15  FILLER              PIC S9(5)   VALUE +01624.
01961              15  FILLER              PIC S9(5)   VALUE +01695.
01962              15  FILLER              PIC S9(5)   VALUE +01757.
01963              15  FILLER              PIC S9(5)   VALUE +01777.
01964              15  FILLER              PIC S9(5)   VALUE +01796.
01965              15  FILLER              PIC S9(5)   VALUE +01810.
01966
01967          10  FILLER.
01968              15  FILLER              PIC S9(5)   VALUE +01697.
01969              15  FILLER              PIC S9(5)   VALUE +01730.
01970              15  FILLER              PIC S9(5)   VALUE +01765.
01971              15  FILLER              PIC S9(5)   VALUE +01834.
01972              15  FILLER              PIC S9(5)   VALUE +01886.
01973              15  FILLER              PIC S9(5)   VALUE +01969.
01974              15  FILLER              PIC S9(5)   VALUE +02066.
01975              15  FILLER              PIC S9(5)   VALUE +02151.
01976              15  FILLER              PIC S9(5)   VALUE +02181.
01977              15  FILLER              PIC S9(5)   VALUE +02206.
01978              15  FILLER              PIC S9(5)   VALUE +02226.
01979
01980          10  FILLER.
01981              15  FILLER              PIC S9(5)   VALUE +02098.
01982              15  FILLER              PIC S9(5)   VALUE +02150.
01983              15  FILLER              PIC S9(5)   VALUE +02216.
01984              15  FILLER              PIC S9(5)   VALUE +02318.
01985              15  FILLER              PIC S9(5)   VALUE +02402.
01986              15  FILLER              PIC S9(5)   VALUE +02526.
01987              15  FILLER              PIC S9(5)   VALUE +02669.
01988              15  FILLER              PIC S9(5)   VALUE +02797.
01989              15  FILLER              PIC S9(5)   VALUE +02844.
01990              15  FILLER              PIC S9(5)   VALUE +02880.
01991              15  FILLER              PIC S9(5)   VALUE +02908.
01992
01993          10  FILLER.
01994              15  FILLER              PIC S9(5)   VALUE +02421.
01995              15  FILLER              PIC S9(5)   VALUE +02499.
01996              15  FILLER              PIC S9(5)   VALUE +02601.
01997              15  FILLER              PIC S9(5)   VALUE +02735.
01998              15  FILLER              PIC S9(5)   VALUE +02850.
01999              15  FILLER              PIC S9(5)   VALUE +03016.
02000              15  FILLER              PIC S9(5)   VALUE +03199.
02001              15  FILLER              PIC S9(5)   VALUE +03363.
02002              15  FILLER              PIC S9(5)   VALUE +03423.
02003              15  FILLER              PIC S9(5)   VALUE +03464.
02004              15  FILLER              PIC S9(5)   VALUE +03492.
02005
02006          10  FILLER.
02007              15  FILLER              PIC S9(5)   VALUE +02695.
02008              15  FILLER              PIC S9(5)   VALUE +02803.
02009              15  FILLER              PIC S9(5)   VALUE +02934.
02010              15  FILLER              PIC S9(5)   VALUE +03104.
02011              15  FILLER              PIC S9(5)   VALUE +03247.
02012              15  FILLER              PIC S9(5)   VALUE +03454.
02013              15  FILLER              PIC S9(5)   VALUE +03673.
02014              15  FILLER              PIC S9(5)   VALUE +03865.
02015              15  FILLER              PIC S9(5)   VALUE +03931.
02016              15  FILLER              PIC S9(5)   VALUE +03973.
02017              15  FILLER              PIC S9(5)   VALUE +03991.
02018
02019          10  FILLER.
02020              15  FILLER              PIC S9(5)   VALUE +02930.
02021              15  FILLER              PIC S9(5)   VALUE +03076.
02022              15  FILLER              PIC S9(5)   VALUE +03226.
02023              15  FILLER              PIC S9(5)   VALUE +03438.
02024              15  FILLER              PIC S9(5)   VALUE +03606.
02025              15  FILLER              PIC S9(5)   VALUE +03848.
02026              15  FILLER              PIC S9(5)   VALUE +04098.
02027              15  FILLER              PIC S9(5)   VALUE +04312.
02028              15  FILLER              PIC S9(5)   VALUE +04379.
02029              15  FILLER              PIC S9(5)   VALUE +04417.
02030              15  FILLER              PIC S9(5)   VALUE +04413.
02031
02032          10  FILLER.
02033              15  FILLER              PIC S9(5)   VALUE +03136.
02034              15  FILLER              PIC S9(5)   VALUE +03313.
02035              15  FILLER              PIC S9(5)   VALUE +03493.
02036              15  FILLER              PIC S9(5)   VALUE +03740.
02037              15  FILLER              PIC S9(5)   VALUE +03933.
02038              15  FILLER              PIC S9(5)   VALUE +04205.
02039              15  FILLER              PIC S9(5)   VALUE +04482.
02040              15  FILLER              PIC S9(5)   VALUE +04711.
02041              15  FILLER              PIC S9(5)   VALUE +04774.
02042              15  FILLER              PIC S9(5)   VALUE +04802.
02043              15  FILLER              PIC S9(5)   VALUE +04768.
02044
02045          10  FILLER.
02046              15  FILLER              PIC S9(5)   VALUE +03322.
02047              15  FILLER              PIC S9(5)   VALUE +03522.
02048              15  FILLER              PIC S9(5)   VALUE +03731.
02049              15  FILLER              PIC S9(5)   VALUE +04014.
02050              15  FILLER              PIC S9(5)   VALUE +04230.
02051              15  FILLER              PIC S9(5)   VALUE +04529.
02052              15  FILLER              PIC S9(5)   VALUE +04826.
02053              15  FILLER              PIC S9(5)   VALUE +05068.
02054              15  FILLER              PIC S9(5)   VALUE +05123.
02055              15  FILLER              PIC S9(5)   VALUE +05135.
02056              15  FILLER              PIC S9(5)   VALUE +05063.
02057
02058          10  FILLER.
02059              15  FILLER              PIC S9(5)   VALUE +03489.
02060              15  FILLER              PIC S9(5)   VALUE +03712.
02061              15  FILLER              PIC S9(5)   VALUE +03945.
02062              15  FILLER              PIC S9(5)   VALUE +04260.
02063              15  FILLER              PIC S9(5)   VALUE +04502.
02064              15  FILLER              PIC S9(5)   VALUE +04823.
02065              15  FILLER              PIC S9(5)   VALUE +05136.
02066              15  FILLER              PIC S9(5)   VALUE +05387.
02067              15  FILLER              PIC S9(5)   VALUE +05428.
02068              15  FILLER              PIC S9(5)   VALUE +05422.
02069              15  FILLER              PIC S9(5)   VALUE +05306.
02070
02071          10  FILLER.
02072              15  FILLER              PIC S9(5)   VALUE +03637.
02073              15  FILLER              PIC S9(5)   VALUE +03887.
02074              15  FILLER              PIC S9(5)   VALUE +04143.
02075              15  FILLER              PIC S9(5)   VALUE +04485.
02076              15  FILLER              PIC S9(5)   VALUE +04751.
02077              15  FILLER              PIC S9(5)   VALUE +05091.
02078              15  FILLER              PIC S9(5)   VALUE +05418.
02079              15  FILLER              PIC S9(5)   VALUE +05671.
02080              15  FILLER              PIC S9(5)   VALUE +05696.
02081              15  FILLER              PIC S9(5)   VALUE +05666.
02082              15  FILLER              PIC S9(5)   VALUE +05503.
02083
02084          10  FILLER.
02085              15  FILLER              PIC S9(5)   VALUE +03773.
02086              15  FILLER              PIC S9(5)   VALUE +04042.
02087              15  FILLER              PIC S9(5)   VALUE +04325.
02088              15  FILLER              PIC S9(5)   VALUE +04694.
02089              15  FILLER              PIC S9(5)   VALUE +04980.
02090              15  FILLER              PIC S9(5)   VALUE +05335.
02091              15  FILLER              PIC S9(5)   VALUE +05673.
02092              15  FILLER              PIC S9(5)   VALUE +05926.
02093              15  FILLER              PIC S9(5)   VALUE +05931.
02094              15  FILLER              PIC S9(5)   VALUE +05874.
02095              15  FILLER              PIC S9(5)   VALUE +05661.
02096
02097          10  FILLER.
02098              15  FILLER              PIC S9(5)   VALUE +03895.
02099              15  FILLER              PIC S9(5)   VALUE +04179.
02100              15  FILLER              PIC S9(5)   VALUE +04491.
02101              15  FILLER              PIC S9(5)   VALUE +04885.
02102              15  FILLER              PIC S9(5)   VALUE +05192.
02103              15  FILLER              PIC S9(5)   VALUE +05557.
02104              15  FILLER              PIC S9(5)   VALUE +05904.
02105              15  FILLER              PIC S9(5)   VALUE +06153.
02106              15  FILLER              PIC S9(5)   VALUE +06136.
02107              15  FILLER              PIC S9(5)   VALUE +06048.
02108              15  FILLER              PIC S9(5)   VALUE +05785.
02109
02110          10  FILLER.
02111              15  FILLER              PIC S9(5)   VALUE +04004.
02112              15  FILLER              PIC S9(5)   VALUE +04303.
02113              15  FILLER              PIC S9(5)   VALUE +04645.
02114              15  FILLER              PIC S9(5)   VALUE +05061.
02115              15  FILLER              PIC S9(5)   VALUE +05386.
02116              15  FILLER              PIC S9(5)   VALUE +05760.
02117              15  FILLER              PIC S9(5)   VALUE +06112.
02118              15  FILLER              PIC S9(5)   VALUE +06355.
02119              15  FILLER              PIC S9(5)   VALUE +06315.
02120              15  FILLER              PIC S9(5)   VALUE +06194.
02121              15  FILLER              PIC S9(5)   VALUE +05882.
02122
02123          10  FILLER.
02124              15  FILLER              PIC S9(5)   VALUE +04109.
02125              15  FILLER              PIC S9(5)   VALUE +04422.
02126              15  FILLER              PIC S9(5)   VALUE +04783.
02127              15  FILLER              PIC S9(5)   VALUE +05223.
02128              15  FILLER              PIC S9(5)   VALUE +05566.
02129              15  FILLER              PIC S9(5)   VALUE +05945.
02130              15  FILLER              PIC S9(5)   VALUE +06301.
02131              15  FILLER              PIC S9(5)   VALUE +06535.
02132              15  FILLER              PIC S9(5)   VALUE +06469.
02133              15  FILLER              PIC S9(5)   VALUE +06314.
02134              15  FILLER              PIC S9(5)   VALUE +05955.
02135
02136          10  FILLER.
02137              15  FILLER              PIC S9(5)   VALUE +04870.
02138              15  FILLER              PIC S9(5)   VALUE +05050.
02139              15  FILLER              PIC S9(5)   VALUE +05045.
02140              15  FILLER              PIC S9(5)   VALUE +00000.
02141              15  FILLER              PIC S9(5)   VALUE +00000.
02142              15  FILLER              PIC S9(5)   VALUE +00000.
02143              15  FILLER              PIC S9(5)   VALUE +00000.
02144              15  FILLER              PIC S9(5)   VALUE +00000.
02145              15  FILLER              PIC S9(5)   VALUE +00000.
02146              15  FILLER              PIC S9(5)   VALUE +00000.
02147              15  FILLER              PIC S9(5)   VALUE +00000.
02148
02149          10  FILLER.
02150              15  FILLER              PIC S9(5)   VALUE +05011.
02151              15  FILLER              PIC S9(5)   VALUE +05304.
02152              15  FILLER              PIC S9(5)   VALUE +05476.
02153              15  FILLER              PIC S9(5)   VALUE +05550.
02154              15  FILLER              PIC S9(5)   VALUE +00000.
02155              15  FILLER              PIC S9(5)   VALUE +00000.
02156              15  FILLER              PIC S9(5)   VALUE +00000.
02157              15  FILLER              PIC S9(5)   VALUE +00000.
02158              15  FILLER              PIC S9(5)   VALUE +00000.
02159              15  FILLER              PIC S9(5)   VALUE +00000.
02160              15  FILLER              PIC S9(5)   VALUE +00000.
02161
02162          10  FILLER.
02163              15  FILLER              PIC S9(5)   VALUE +05103.
02164              15  FILLER              PIC S9(5)   VALUE +05471.
02165              15  FILLER              PIC S9(5)   VALUE +05760.
02166              15  FILLER              PIC S9(5)   VALUE +06087.
02167              15  FILLER              PIC S9(5)   VALUE +05923.
02168              15  FILLER              PIC S9(5)   VALUE +00000.
02169              15  FILLER              PIC S9(5)   VALUE +00000.
02170              15  FILLER              PIC S9(5)   VALUE +00000.
02171              15  FILLER              PIC S9(5)   VALUE +00000.
02172              15  FILLER              PIC S9(5)   VALUE +00000.
02173              15  FILLER              PIC S9(5)   VALUE +00000.
02174
02175          10  FILLER.
02176              15  FILLER              PIC S9(5)   VALUE +05163.
02177              15  FILLER              PIC S9(5)   VALUE +05579.
02178              15  FILLER              PIC S9(5)   VALUE +05943.
02179              15  FILLER              PIC S9(5)   VALUE +06433.
02180              15  FILLER              PIC S9(5)   VALUE +06499.
02181              15  FILLER              PIC S9(5)   VALUE +06303.
02182              15  FILLER              PIC S9(5)   VALUE +00000.
02183              15  FILLER              PIC S9(5)   VALUE +00000.
02184              15  FILLER              PIC S9(5)   VALUE +00000.
02185              15  FILLER              PIC S9(5)   VALUE +00000.
02186              15  FILLER              PIC S9(5)   VALUE +00000.
02187
02188          10  FILLER.
02189              15  FILLER              PIC S9(5)   VALUE +05238.
02190              15  FILLER              PIC S9(5)   VALUE +05714.
02191              15  FILLER              PIC S9(5)   VALUE +06175.
02192              15  FILLER              PIC S9(5)   VALUE +06873.
02193              15  FILLER              PIC S9(5)   VALUE +07231.
02194              15  FILLER              PIC S9(5)   VALUE +07452.
02195              15  FILLER              PIC S9(5)   VALUE +07636.
02196              15  FILLER              PIC S9(5)   VALUE +07596.
02197              15  FILLER              PIC S9(5)   VALUE +07177.
02198              15  FILLER              PIC S9(5)   VALUE +06722.
02199              15  FILLER              PIC S9(5)   VALUE +06135.
02200
02201      05  TAB-05-VALUES.
02202
02203          10  FILLER.
02204              15  FILLER              PIC S9(5)   VALUE +00548.
02205              15  FILLER              PIC S9(5)   VALUE +00551.
02206              15  FILLER              PIC S9(5)   VALUE +00555.
02207              15  FILLER              PIC S9(5)   VALUE +00558.
02208              15  FILLER              PIC S9(5)   VALUE +00561.
02209              15  FILLER              PIC S9(5)   VALUE +00565.
02210              15  FILLER              PIC S9(5)   VALUE +00569.
02211              15  FILLER              PIC S9(5)   VALUE +00573.
02212              15  FILLER              PIC S9(5)   VALUE +00576.
02213              15  FILLER              PIC S9(5)   VALUE +00578.
02214              15  FILLER              PIC S9(5)   VALUE +00579.
02215
02216          10  FILLER.
02217              15  FILLER              PIC S9(5)   VALUE +01421.
02218              15  FILLER              PIC S9(5)   VALUE +01445.
02219              15  FILLER              PIC S9(5)   VALUE +01467.
02220              15  FILLER              PIC S9(5)   VALUE +01489.
02221              15  FILLER              PIC S9(5)   VALUE +01511.
02222              15  FILLER              PIC S9(5)   VALUE +01534.
02223              15  FILLER              PIC S9(5)   VALUE +01559.
02224              15  FILLER              PIC S9(5)   VALUE +01586.
02225              15  FILLER              PIC S9(5)   VALUE +01603.
02226              15  FILLER              PIC S9(5)   VALUE +01613.
02227              15  FILLER              PIC S9(5)   VALUE +01622.
02228
02229          10  FILLER.
02230              15  FILLER              PIC S9(5)   VALUE +02084.
02231              15  FILLER              PIC S9(5)   VALUE +02139.
02232              15  FILLER              PIC S9(5)   VALUE +02191.
02233              15  FILLER              PIC S9(5)   VALUE +02242.
02234              15  FILLER              PIC S9(5)   VALUE +02293.
02235              15  FILLER              PIC S9(5)   VALUE +02345.
02236              15  FILLER              PIC S9(5)   VALUE +02398.
02237              15  FILLER              PIC S9(5)   VALUE +02454.
02238              15  FILLER              PIC S9(5)   VALUE +02487.
02239              15  FILLER              PIC S9(5)   VALUE +02503.
02240              15  FILLER              PIC S9(5)   VALUE +02519.
02241
02242          10  FILLER.
02243              15  FILLER              PIC S9(5)   VALUE +02622.
02244              15  FILLER              PIC S9(5)   VALUE +02712.
02245              15  FILLER              PIC S9(5)   VALUE +02799.
02246              15  FILLER              PIC S9(5)   VALUE +02885.
02247              15  FILLER              PIC S9(5)   VALUE +02969.
02248              15  FILLER              PIC S9(5)   VALUE +03051.
02249              15  FILLER              PIC S9(5)   VALUE +03132.
02250              15  FILLER              PIC S9(5)   VALUE +03212.
02251              15  FILLER              PIC S9(5)   VALUE +03256.
02252              15  FILLER              PIC S9(5)   VALUE +03274.
02253              15  FILLER              PIC S9(5)   VALUE +03290.
02254
02255          10  FILLER.
02256              15  FILLER              PIC S9(5)   VALUE +03074.
02257              15  FILLER              PIC S9(5)   VALUE +03204.
02258              15  FILLER              PIC S9(5)   VALUE +03329.
02259              15  FILLER              PIC S9(5)   VALUE +03452.
02260              15  FILLER              PIC S9(5)   VALUE +03569.
02261              15  FILLER              PIC S9(5)   VALUE +03681.
02262              15  FILLER              PIC S9(5)   VALUE +03786.
02263              15  FILLER              PIC S9(5)   VALUE +03884.
02264              15  FILLER              PIC S9(5)   VALUE +03931.
02265              15  FILLER              PIC S9(5)   VALUE +03946.
02266              15  FILLER              PIC S9(5)   VALUE +03948.
02267
02268          10  FILLER.
02269              15  FILLER              PIC S9(5)   VALUE +03465.
02270              15  FILLER              PIC S9(5)   VALUE +03636.
02271              15  FILLER              PIC S9(5)   VALUE +03800.
02272              15  FILLER              PIC S9(5)   VALUE +03960.
02273              15  FILLER              PIC S9(5)   VALUE +04110.
02274              15  FILLER              PIC S9(5)   VALUE +04249.
02275              15  FILLER              PIC S9(5)   VALUE +04373.
02276              15  FILLER              PIC S9(5)   VALUE +04482.
02277              15  FILLER              PIC S9(5)   VALUE +04525.
02278              15  FILLER              PIC S9(5)   VALUE +04531.
02279              15  FILLER              PIC S9(5)   VALUE +04506.
02280
02281          10  FILLER.
02282              15  FILLER              PIC S9(5)   VALUE +03809.
02283              15  FILLER              PIC S9(5)   VALUE +04020.
02284              15  FILLER              PIC S9(5)   VALUE +04223.
02285              15  FILLER              PIC S9(5)   VALUE +04420.
02286              15  FILLER              PIC S9(5)   VALUE +04601.
02287              15  FILLER              PIC S9(5)   VALUE +04762.
02288              15  FILLER              PIC S9(5)   VALUE +04901.
02289              15  FILLER              PIC S9(5)   VALUE +05016.
02290              15  FILLER              PIC S9(5)   VALUE +05050.
02291              15  FILLER              PIC S9(5)   VALUE +05040.
02292              15  FILLER              PIC S9(5)   VALUE +04976.
02293
02294          10  FILLER.
02295              15  FILLER              PIC S9(5)   VALUE +04113.
02296              15  FILLER              PIC S9(5)   VALUE +04364.
02297              15  FILLER              PIC S9(5)   VALUE +04606.
02298              15  FILLER              PIC S9(5)   VALUE +04838.
02299              15  FILLER              PIC S9(5)   VALUE +05046.
02300              15  FILLER              PIC S9(5)   VALUE +05227.
02301              15  FILLER              PIC S9(5)   VALUE +05377.
02302              15  FILLER              PIC S9(5)   VALUE +05493.
02303              15  FILLER              PIC S9(5)   VALUE +05512.
02304              15  FILLER              PIC S9(5)   VALUE +05481.
02305              15  FILLER              PIC S9(5)   VALUE +05368.
02306
02307          10  FILLER.
02308              15  FILLER              PIC S9(5)   VALUE +04385.
02309              15  FILLER              PIC S9(5)   VALUE +04675.
02310              15  FILLER              PIC S9(5)   VALUE +04953.
02311              15  FILLER              PIC S9(5)   VALUE +05218.
02312              15  FILLER              PIC S9(5)   VALUE +05453.
02313              15  FILLER              PIC S9(5)   VALUE +05650.
02314              15  FILLER              PIC S9(5)   VALUE +05807.
02315              15  FILLER              PIC S9(5)   VALUE +05919.
02316              15  FILLER              PIC S9(5)   VALUE +05919.
02317              15  FILLER              PIC S9(5)   VALUE +05860.
02318              15  FILLER              PIC S9(5)   VALUE +05692.
02319
02320          10  FILLER.
02321              15  FILLER              PIC S9(5)   VALUE +04629.
02322              15  FILLER              PIC S9(5)   VALUE +04955.
02323              15  FILLER              PIC S9(5)   VALUE +05269.
02324              15  FILLER              PIC S9(5)   VALUE +05565.
02325              15  FILLER              PIC S9(5)   VALUE +05824.
02326              15  FILLER              PIC S9(5)   VALUE +06035.
02327              15  FILLER              PIC S9(5)   VALUE +06196.
02328              15  FILLER              PIC S9(5)   VALUE +06300.
02329              15  FILLER              PIC S9(5)   VALUE +06275.
02330              15  FILLER              PIC S9(5)   VALUE +06185.
02331              15  FILLER              PIC S9(5)   VALUE +05955.
02332
02333          10  FILLER.
02334              15  FILLER              PIC S9(5)   VALUE +04848.
02335              15  FILLER              PIC S9(5)   VALUE +05210.
02336              15  FILLER              PIC S9(5)   VALUE +05557.
02337              15  FILLER              PIC S9(5)   VALUE +05883.
02338              15  FILLER              PIC S9(5)   VALUE +06164.
02339              15  FILLER              PIC S9(5)   VALUE +06386.
02340              15  FILLER              PIC S9(5)   VALUE +06547.
02341              15  FILLER              PIC S9(5)   VALUE +06639.
02342              15  FILLER              PIC S9(5)   VALUE +06587.
02343              15  FILLER              PIC S9(5)   VALUE +06461.
02344              15  FILLER              PIC S9(5)   VALUE +06167.
02345
02346          10  FILLER.
02347              15  FILLER              PIC S9(5)   VALUE +05047.
02348              15  FILLER              PIC S9(5)   VALUE +05441.
02349              15  FILLER              PIC S9(5)   VALUE +05820.
02350              15  FILLER              PIC S9(5)   VALUE +06173.
02351              15  FILLER              PIC S9(5)   VALUE +06477.
02352              15  FILLER              PIC S9(5)   VALUE +06706.
02353              15  FILLER              PIC S9(5)   VALUE +06865.
02354              15  FILLER              PIC S9(5)   VALUE +06942.
02355              15  FILLER              PIC S9(5)   VALUE +06860.
02356              15  FILLER              PIC S9(5)   VALUE +06694.
02357              15  FILLER              PIC S9(5)   VALUE +06334.
02358
02359          10  FILLER.
02360              15  FILLER              PIC S9(5)   VALUE +05228.
02361              15  FILLER              PIC S9(5)   VALUE +05652.
02362              15  FILLER              PIC S9(5)   VALUE +06060.
02363              15  FILLER              PIC S9(5)   VALUE +06439.
02364              15  FILLER              PIC S9(5)   VALUE +06764.
02365              15  FILLER              PIC S9(5)   VALUE +06999.
02366              15  FILLER              PIC S9(5)   VALUE +07153.
02367              15  FILLER              PIC S9(5)   VALUE +07213.
02368              15  FILLER              PIC S9(5)   VALUE +07099.
02369              15  FILLER              PIC S9(5)   VALUE +06888.
02370              15  FILLER              PIC S9(5)   VALUE +06464.
02371
02372          10  FILLER.
02373              15  FILLER              PIC S9(5)   VALUE +05392.
02374              15  FILLER              PIC S9(5)   VALUE +05844.
02375              15  FILLER              PIC S9(5)   VALUE +06280.
02376              15  FILLER              PIC S9(5)   VALUE +06683.
02377              15  FILLER              PIC S9(5)   VALUE +07027.
02378              15  FILLER              PIC S9(5)   VALUE +07266.
02379              15  FILLER              PIC S9(5)   VALUE +07413.
02380              15  FILLER              PIC S9(5)   VALUE +07454.
02381              15  FILLER              PIC S9(5)   VALUE +07305.
02382              15  FILLER              PIC S9(5)   VALUE +07048.
02383              15  FILLER              PIC S9(5)   VALUE +06563.
02384
02385          10  FILLER.
02386              15  FILLER              PIC S9(5)   VALUE +06557.
02387              15  FILLER              PIC S9(5)   VALUE +06848.
02388              15  FILLER              PIC S9(5)   VALUE +06754.
02389              15  FILLER              PIC S9(5)   VALUE +00000.
02390              15  FILLER              PIC S9(5)   VALUE +00000.
02391              15  FILLER              PIC S9(5)   VALUE +00000.
02392              15  FILLER              PIC S9(5)   VALUE +00000.
02393              15  FILLER              PIC S9(5)   VALUE +00000.
02394              15  FILLER              PIC S9(5)   VALUE +00000.
02395              15  FILLER              PIC S9(5)   VALUE +00000.
02396              15  FILLER              PIC S9(5)   VALUE +00000.
02397
02398          10  FILLER.
02399              15  FILLER              PIC S9(5)   VALUE +06765.
02400              15  FILLER              PIC S9(5)   VALUE +07235.
02401              15  FILLER              PIC S9(5)   VALUE +07447.
02402              15  FILLER              PIC S9(5)   VALUE +07212.
02403              15  FILLER              PIC S9(5)   VALUE +00000.
02404              15  FILLER              PIC S9(5)   VALUE +00000.
02405              15  FILLER              PIC S9(5)   VALUE +00000.
02406              15  FILLER              PIC S9(5)   VALUE +00000.
02407              15  FILLER              PIC S9(5)   VALUE +00000.
02408              15  FILLER              PIC S9(5)   VALUE +00000.
02409              15  FILLER              PIC S9(5)   VALUE +00000.
02410
02411          10  FILLER.
02412              15  FILLER              PIC S9(5)   VALUE +06902.
02413              15  FILLER              PIC S9(5)   VALUE +07490.
02414              15  FILLER              PIC S9(5)   VALUE +07903.
02415              15  FILLER              PIC S9(5)   VALUE +07989.
02416              15  FILLER              PIC S9(5)   VALUE +07597.
02417              15  FILLER              PIC S9(5)   VALUE +00000.
02418              15  FILLER              PIC S9(5)   VALUE +00000.
02419              15  FILLER              PIC S9(5)   VALUE +00000.
02420              15  FILLER              PIC S9(5)   VALUE +00000.
02421              15  FILLER              PIC S9(5)   VALUE +00000.
02422              15  FILLER              PIC S9(5)   VALUE +00000.
02423
02424          10  FILLER.
02425              15  FILLER              PIC S9(5)   VALUE +06991.
02426              15  FILLER              PIC S9(5)   VALUE +07654.
02427              15  FILLER              PIC S9(5)   VALUE +08197.
02428              15  FILLER              PIC S9(5)   VALUE +08490.
02429              15  FILLER              PIC S9(5)   VALUE +08419.
02430              15  FILLER              PIC S9(5)   VALUE +07834.
02431              15  FILLER              PIC S9(5)   VALUE +00000.
02432              15  FILLER              PIC S9(5)   VALUE +00000.
02433              15  FILLER              PIC S9(5)   VALUE +00000.
02434              15  FILLER              PIC S9(5)   VALUE +00000.
02435              15  FILLER              PIC S9(5)   VALUE +00000.
02436
02437          10  FILLER.
02438              15  FILLER              PIC S9(5)   VALUE +07103.
02439              15  FILLER              PIC S9(5)   VALUE +07863.
02440              15  FILLER              PIC S9(5)   VALUE +08571.
02441              15  FILLER              PIC S9(5)   VALUE +09127.
02442              15  FILLER              PIC S9(5)   VALUE +09461.
02443              15  FILLER              PIC S9(5)   VALUE +09463.
02444              15  FILLER              PIC S9(5)   VALUE +09275.
02445              15  FILLER              PIC S9(5)   VALUE +08889.
02446              15  FILLER              PIC S9(5)   VALUE +08262.
02447              15  FILLER              PIC S9(5)   VALUE +07603.
02448              15  FILLER              PIC S9(5)   VALUE +06810.
02449
02450      05  TAB-06-VALUES.
02451
02452          10  FILLER.
02453              15  FILLER              PIC S9(5)   VALUE +00554.
02454              15  FILLER              PIC S9(5)   VALUE +00558.
02455              15  FILLER              PIC S9(5)   VALUE +00561.
02456              15  FILLER              PIC S9(5)   VALUE +00564.
02457              15  FILLER              PIC S9(5)   VALUE +00567.
02458              15  FILLER              PIC S9(5)   VALUE +00570.
02459              15  FILLER              PIC S9(5)   VALUE +00572.
02460              15  FILLER              PIC S9(5)   VALUE +00575.
02461              15  FILLER              PIC S9(5)   VALUE +00576.
02462              15  FILLER              PIC S9(5)   VALUE +00577.
02463              15  FILLER              PIC S9(5)   VALUE +00577.
02464
02465          10  FILLER.
02466              15  FILLER              PIC S9(5)   VALUE +01467.
02467              15  FILLER              PIC S9(5)   VALUE +01491.
02468              15  FILLER              PIC S9(5)   VALUE +01513.
02469              15  FILLER              PIC S9(5)   VALUE +01535.
02470              15  FILLER              PIC S9(5)   VALUE +01555.
02471              15  FILLER              PIC S9(5)   VALUE +01573.
02472              15  FILLER              PIC S9(5)   VALUE +01589.
02473              15  FILLER              PIC S9(5)   VALUE +01603.
02474              15  FILLER              PIC S9(5)   VALUE +01609.
02475              15  FILLER              PIC S9(5)   VALUE +01609.
02476              15  FILLER              PIC S9(5)   VALUE +01609.
02477
02478          10  FILLER.
02479              15  FILLER              PIC S9(5)   VALUE +02205.
02480              15  FILLER              PIC S9(5)   VALUE +02261.
02481              15  FILLER              PIC S9(5)   VALUE +02314.
02482              15  FILLER              PIC S9(5)   VALUE +02364.
02483              15  FILLER              PIC S9(5)   VALUE +02409.
02484              15  FILLER              PIC S9(5)   VALUE +02448.
02485              15  FILLER              PIC S9(5)   VALUE +02479.
02486              15  FILLER              PIC S9(5)   VALUE +02502.
02487              15  FILLER              PIC S9(5)   VALUE +02507.
02488              15  FILLER              PIC S9(5)   VALUE +02503.
02489              15  FILLER              PIC S9(5)   VALUE +02496.
02490
02491          10  FILLER.
02492              15  FILLER              PIC S9(5)   VALUE +02827.
02493              15  FILLER              PIC S9(5)   VALUE +02922.
02494              15  FILLER              PIC S9(5)   VALUE +03011.
02495              15  FILLER              PIC S9(5)   VALUE +03095.
02496              15  FILLER              PIC S9(5)   VALUE +03168.
02497              15  FILLER              PIC S9(5)   VALUE +03228.
02498              15  FILLER              PIC S9(5)   VALUE +03271.
02499              15  FILLER              PIC S9(5)   VALUE +03297.
02500              15  FILLER              PIC S9(5)   VALUE +03296.
02501              15  FILLER              PIC S9(5)   VALUE +03282.
02502              15  FILLER              PIC S9(5)   VALUE +03254.
02503
02504          10  FILLER.
02505              15  FILLER              PIC S9(5)   VALUE +03365.
02506              15  FILLER              PIC S9(5)   VALUE +03501.
02507              15  FILLER              PIC S9(5)   VALUE +03631.
02508              15  FILLER              PIC S9(5)   VALUE +03750.
02509              15  FILLER              PIC S9(5)   VALUE +03851.
02510              15  FILLER              PIC S9(5)   VALUE +03930.
02511              15  FILLER              PIC S9(5)   VALUE +03982.
02512              15  FILLER              PIC S9(5)   VALUE +04006.
02513              15  FILLER              PIC S9(5)   VALUE +03991.
02514              15  FILLER              PIC S9(5)   VALUE +03961.
02515              15  FILLER              PIC S9(5)   VALUE +03896.
02516
02517          10  FILLER.
02518              15  FILLER              PIC S9(5)   VALUE +03837.
02519              15  FILLER              PIC S9(5)   VALUE +04017.
02520              15  FILLER              PIC S9(5)   VALUE +04187.
02521              15  FILLER              PIC S9(5)   VALUE +04343.
02522              15  FILLER              PIC S9(5)   VALUE +04470.
02523              15  FILLER              PIC S9(5)   VALUE +04566.
02524              15  FILLER              PIC S9(5)   VALUE +04622.
02525              15  FILLER              PIC S9(5)   VALUE +04639.
02526              15  FILLER              PIC S9(5)   VALUE +04604.
02527              15  FILLER              PIC S9(5)   VALUE +04551.
02528              15  FILLER              PIC S9(5)   VALUE +04436.
02529
02530          10  FILLER.
02531              15  FILLER              PIC S9(5)   VALUE +04255.
02532              15  FILLER              PIC S9(5)   VALUE +04480.
02533              15  FILLER              PIC S9(5)   VALUE +04691.
02534              15  FILLER              PIC S9(5)   VALUE +04882.
02535              15  FILLER              PIC S9(5)   VALUE +05033.
02536              15  FILLER              PIC S9(5)   VALUE +05142.
02537              15  FILLER              PIC S9(5)   VALUE +05199.
02538              15  FILLER              PIC S9(5)   VALUE +05204.
02539              15  FILLER              PIC S9(5)   VALUE +05144.
02540              15  FILLER              PIC S9(5)   VALUE +05062.
02541              15  FILLER              PIC S9(5)   VALUE +04887.
02542
02543          10  FILLER.
02544              15  FILLER              PIC S9(5)   VALUE +04629.
02545              15  FILLER              PIC S9(5)   VALUE +04897.
02546              15  FILLER              PIC S9(5)   VALUE +05148.
02547              15  FILLER              PIC S9(5)   VALUE +05373.
02548              15  FILLER              PIC S9(5)   VALUE +05547.
02549              15  FILLER              PIC S9(5)   VALUE +05665.
02550              15  FILLER              PIC S9(5)   VALUE +05720.
02551              15  FILLER              PIC S9(5)   VALUE +05709.
02552              15  FILLER              PIC S9(5)   VALUE +05619.
02553              15  FILLER              PIC S9(5)   VALUE +05502.
02554              15  FILLER              PIC S9(5)   VALUE +05260.
02555
02556          10  FILLER.
02557              15  FILLER              PIC S9(5)   VALUE +04964.
02558              15  FILLER              PIC S9(5)   VALUE +05274.
02559              15  FILLER              PIC S9(5)   VALUE +05564.
02560              15  FILLER              PIC S9(5)   VALUE +05820.
02561              15  FILLER              PIC S9(5)   VALUE +06016.
02562              15  FILLER              PIC S9(5)   VALUE +06142.
02563              15  FILLER              PIC S9(5)   VALUE +06191.
02564              15  FILLER              PIC S9(5)   VALUE +06159.
02565              15  FILLER              PIC S9(5)   VALUE +06035.
02566              15  FILLER              PIC S9(5)   VALUE +05879.
02567              15  FILLER              PIC S9(5)   VALUE +05563.
02568
02569          10  FILLER.
02570              15  FILLER              PIC S9(5)   VALUE +05266.
02571              15  FILLER              PIC S9(5)   VALUE +05616.
02572              15  FILLER              PIC S9(5)   VALUE +05943.
02573              15  FILLER              PIC S9(5)   VALUE +06229.
02574              15  FILLER              PIC S9(5)   VALUE +06446.
02575              15  FILLER              PIC S9(5)   VALUE +06576.
02576              15  FILLER              PIC S9(5)   VALUE +06616.
02577              15  FILLER              PIC S9(5)   VALUE +06562.
02578              15  FILLER              PIC S9(5)   VALUE +06400.
02579              15  FILLER              PIC S9(5)   VALUE +06199.
02580              15  FILLER              PIC S9(5)   VALUE +05806.
02581
02582          10  FILLER.
02583              15  FILLER              PIC S9(5)   VALUE +05539.
02584              15  FILLER              PIC S9(5)   VALUE +05927.
02585              15  FILLER              PIC S9(5)   VALUE +06288.
02586              15  FILLER              PIC S9(5)   VALUE +06603.
02587              15  FILLER              PIC S9(5)   VALUE +06840.
02588              15  FILLER              PIC S9(5)   VALUE +06973.
02589              15  FILLER              PIC S9(5)   VALUE +07002.
02590              15  FILLER              PIC S9(5)   VALUE +06921.
02591              15  FILLER              PIC S9(5)   VALUE +06719.
02592              15  FILLER              PIC S9(5)   VALUE +06468.
02593              15  FILLER              PIC S9(5)   VALUE +05999.
02594
02595          10  FILLER.
02596              15  FILLER              PIC S9(5)   VALUE +05787.
02597              15  FILLER              PIC S9(5)   VALUE +06210.
02598              15  FILLER              PIC S9(5)   VALUE +06605.
02599              15  FILLER              PIC S9(5)   VALUE +06946.
02600              15  FILLER              PIC S9(5)   VALUE +07203.
02601              15  FILLER              PIC S9(5)   VALUE +07335.
02602              15  FILLER              PIC S9(5)   VALUE +07351.
02603              15  FILLER              PIC S9(5)   VALUE +07241.
02604              15  FILLER              PIC S9(5)   VALUE +06998.
02605              15  FILLER              PIC S9(5)   VALUE +06694.
02606              15  FILLER              PIC S9(5)   VALUE +06148.
02607
02608          10  FILLER.
02609              15  FILLER              PIC S9(5)   VALUE +06013.
02610              15  FILLER              PIC S9(5)   VALUE +06468.
02611              15  FILLER              PIC S9(5)   VALUE +06894.
02612              15  FILLER              PIC S9(5)   VALUE +07262.
02613              15  FILLER              PIC S9(5)   VALUE +07536.
02614              15  FILLER              PIC S9(5)   VALUE +07666.
02615              15  FILLER              PIC S9(5)   VALUE +07666.
02616              15  FILLER              PIC S9(5)   VALUE +07526.
02617              15  FILLER              PIC S9(5)   VALUE +07239.
02618              15  FILLER              PIC S9(5)   VALUE +06879.
02619              15  FILLER              PIC S9(5)   VALUE +06263.
02620
02621          10  FILLER.
02622              15  FILLER              PIC S9(5)   VALUE +07615.
02623              15  FILLER              PIC S9(5)   VALUE +07817.
02624              15  FILLER              PIC S9(5)   VALUE +07517.
02625              15  FILLER              PIC S9(5)   VALUE +00000.
02626              15  FILLER              PIC S9(5)   VALUE +00000.
02627              15  FILLER              PIC S9(5)   VALUE +00000.
02628              15  FILLER              PIC S9(5)   VALUE +00000.
02629              15  FILLER              PIC S9(5)   VALUE +00000.
02630              15  FILLER              PIC S9(5)   VALUE +00000.
02631              15  FILLER              PIC S9(5)   VALUE +00000.
02632              15  FILLER              PIC S9(5)   VALUE +00000.
02633
02634          10  FILLER.
02635              15  FILLER              PIC S9(5)   VALUE +07901.
02636              15  FILLER              PIC S9(5)   VALUE +08337.
02637              15  FILLER              PIC S9(5)   VALUE +08429.
02638              15  FILLER              PIC S9(5)   VALUE +07943.
02639              15  FILLER              PIC S9(5)   VALUE +00000.
02640              15  FILLER              PIC S9(5)   VALUE +00000.
02641              15  FILLER              PIC S9(5)   VALUE +00000.
02642              15  FILLER              PIC S9(5)   VALUE +00000.
02643              15  FILLER              PIC S9(5)   VALUE +00000.
02644              15  FILLER              PIC S9(5)   VALUE +00000.
02645              15  FILLER              PIC S9(5)   VALUE +00000.
02646
02647          10  FILLER.
02648              15  FILLER              PIC S9(5)   VALUE +08090.
02649              15  FILLER              PIC S9(5)   VALUE +08679.
02650              15  FILLER              PIC S9(5)   VALUE +09029.
02651              15  FILLER              PIC S9(5)   VALUE +08945.
02652              15  FILLER              PIC S9(5)   VALUE +08256.
02653              15  FILLER              PIC S9(5)   VALUE +00000.
02654              15  FILLER              PIC S9(5)   VALUE +00000.
02655              15  FILLER              PIC S9(5)   VALUE +00000.
02656              15  FILLER              PIC S9(5)   VALUE +00000.
02657              15  FILLER              PIC S9(5)   VALUE +00000.
02658              15  FILLER              PIC S9(5)   VALUE +00000.
02659
02660          10  FILLER.
02661              15  FILLER              PIC S9(5)   VALUE +08211.
02662              15  FILLER              PIC S9(5)   VALUE +08900.
02663              15  FILLER              PIC S9(5)   VALUE +09416.
02664              15  FILLER              PIC S9(5)   VALUE +09591.
02665              15  FILLER              PIC S9(5)   VALUE +09294.
02666              15  FILLER              PIC S9(5)   VALUE +08369.
02667              15  FILLER              PIC S9(5)   VALUE +00000.
02668              15  FILLER              PIC S9(5)   VALUE +00000.
02669              15  FILLER              PIC S9(5)   VALUE +00000.
02670              15  FILLER              PIC S9(5)   VALUE +00000.
02671              15  FILLER              PIC S9(5)   VALUE +00000.
02672
02673          10  FILLER.
02674              15  FILLER              PIC S9(5)   VALUE +08365.
02675              15  FILLER              PIC S9(5)   VALUE +09180.
02676              15  FILLER              PIC S9(5)   VALUE +09908.
02677              15  FILLER              PIC S9(5)   VALUE +10412.
02678              15  FILLER              PIC S9(5)   VALUE +10611.
02679              15  FILLER              PIC S9(5)   VALUE +10386.
02680              15  FILLER              PIC S9(5)   VALUE +09922.
02681              15  FILLER              PIC S9(5)   VALUE +09226.
02682              15  FILLER              PIC S9(5)   VALUE +08357.
02683              15  FILLER              PIC S9(5)   VALUE +07522.
02684              15  FILLER              PIC S9(5)   VALUE +06547.
02685
02686      05  TAB-07-VALUES.
02687
02688          10  FILLER.
02689              15  FILLER              PIC S9(5)   VALUE +00565.
02690              15  FILLER              PIC S9(5)   VALUE +00568.
02691              15  FILLER              PIC S9(5)   VALUE +00571.
02692              15  FILLER              PIC S9(5)   VALUE +00574.
02693              15  FILLER              PIC S9(5)   VALUE +00576.
02694              15  FILLER              PIC S9(5)   VALUE +00577.
02695              15  FILLER              PIC S9(5)   VALUE +00578.
02696              15  FILLER              PIC S9(5)   VALUE +00579.
02697              15  FILLER              PIC S9(5)   VALUE +00579.
02698              15  FILLER              PIC S9(5)   VALUE +00578.
02699              15  FILLER              PIC S9(5)   VALUE +00578.
02700
02701          10  FILLER.
02702              15  FILLER              PIC S9(5)   VALUE +01537.
02703              15  FILLER              PIC S9(5)   VALUE +01559.
02704              15  FILLER              PIC S9(5)   VALUE +01580.
02705              15  FILLER              PIC S9(5)   VALUE +01598.
02706              15  FILLER              PIC S9(5)   VALUE +01613.
02707              15  FILLER              PIC S9(5)   VALUE +01623.
02708              15  FILLER              PIC S9(5)   VALUE +01628.
02709              15  FILLER              PIC S9(5)   VALUE +01628.
02710              15  FILLER              PIC S9(5)   VALUE +01624.
02711              15  FILLER              PIC S9(5)   VALUE +01618.
02712              15  FILLER              PIC S9(5)   VALUE +01609.
02713
02714          10  FILLER.
02715              15  FILLER              PIC S9(5)   VALUE +02355.
02716              15  FILLER              PIC S9(5)   VALUE +02409.
02717              15  FILLER              PIC S9(5)   VALUE +02458.
02718              15  FILLER              PIC S9(5)   VALUE +02501.
02719              15  FILLER              PIC S9(5)   VALUE +02534.
02720              15  FILLER              PIC S9(5)   VALUE +02556.
02721              15  FILLER              PIC S9(5)   VALUE +02563.
02722              15  FILLER              PIC S9(5)   VALUE +02557.
02723              15  FILLER              PIC S9(5)   VALUE +02540.
02724              15  FILLER              PIC S9(5)   VALUE +02524.
02725              15  FILLER              PIC S9(5)   VALUE +02489.
02726
02727          10  FILLER.
02728              15  FILLER              PIC S9(5)   VALUE +03062.
02729              15  FILLER              PIC S9(5)   VALUE +03155.
02730              15  FILLER              PIC S9(5)   VALUE +03238.
02731              15  FILLER              PIC S9(5)   VALUE +03311.
02732              15  FILLER              PIC S9(5)   VALUE +03363.
02733              15  FILLER              PIC S9(5)   VALUE +03396.
02734              15  FILLER              PIC S9(5)   VALUE +03403.
02735              15  FILLER              PIC S9(5)   VALUE +03384.
02736              15  FILLER              PIC S9(5)   VALUE +03348.
02737              15  FILLER              PIC S9(5)   VALUE +03313.
02738              15  FILLER              PIC S9(5)   VALUE +03236.
02739
02740          10  FILLER.
02741              15  FILLER              PIC S9(5)   VALUE +03683.
02742              15  FILLER              PIC S9(5)   VALUE +03819.
02743              15  FILLER              PIC S9(5)   VALUE +03940.
02744              15  FILLER              PIC S9(5)   VALUE +04044.
02745              15  FILLER              PIC S9(5)   VALUE +04116.
02746              15  FILLER              PIC S9(5)   VALUE +04156.
02747              15  FILLER              PIC S9(5)   VALUE +04159.
02748              15  FILLER              PIC S9(5)   VALUE +04123.
02749              15  FILLER              PIC S9(5)   VALUE +04061.
02750              15  FILLER              PIC S9(5)   VALUE +03999.
02751              15  FILLER              PIC S9(5)   VALUE +03864.
02752
02753          10  FILLER.
02754              15  FILLER              PIC S9(5)   VALUE +04233.
02755              15  FILLER              PIC S9(5)   VALUE +04414.
02756              15  FILLER              PIC S9(5)   VALUE +04574.
02757              15  FILLER              PIC S9(5)   VALUE +04709.
02758              15  FILLER              PIC S9(5)   VALUE +04799.
02759              15  FILLER              PIC S9(5)   VALUE +04845.
02760              15  FILLER              PIC S9(5)   VALUE +04839.
02761              15  FILLER              PIC S9(5)   VALUE +04783.
02762              15  FILLER              PIC S9(5)   VALUE +04689.
02763              15  FILLER              PIC S9(5)   VALUE +04593.
02764              15  FILLER              PIC S9(5)   VALUE +04388.
02765
02766          10  FILLER.
02767              15  FILLER              PIC S9(5)   VALUE +04724.
02768              15  FILLER              PIC S9(5)   VALUE +04950.
02769              15  FILLER              PIC S9(5)   VALUE +05150.
02770              15  FILLER              PIC S9(5)   VALUE +05316.
02771              15  FILLER              PIC S9(5)   VALUE +05423.
02772              15  FILLER              PIC S9(5)   VALUE +05471.
02773              15  FILLER              PIC S9(5)   VALUE +05454.
02774              15  FILLER              PIC S9(5)   VALUE +05372.
02775              15  FILLER              PIC S9(5)   VALUE +05241.
02776              15  FILLER              PIC S9(5)   VALUE +05105.
02777              15  FILLER              PIC S9(5)   VALUE +04821.
02778
02779          10  FILLER.
02780              15  FILLER              PIC S9(5)   VALUE +05165.
02781              15  FILLER              PIC S9(5)   VALUE +05435.
02782              15  FILLER              PIC S9(5)   VALUE +05674.
02783              15  FILLER              PIC S9(5)   VALUE +05869.
02784              15  FILLER              PIC S9(5)   VALUE +05992.
02785              15  FILLER              PIC S9(5)   VALUE +06040.
02786              15  FILLER              PIC S9(5)   VALUE +06009.
02787              15  FILLER              PIC S9(5)   VALUE +05898.
02788              15  FILLER              PIC S9(5)   VALUE +05725.
02789              15  FILLER              PIC S9(5)   VALUE +05542.
02790              15  FILLER              PIC S9(5)   VALUE +05174.
02791
02792          10  FILLER.
02793              15  FILLER              PIC S9(5)   VALUE +05563.
02794              15  FILLER              PIC S9(5)   VALUE +05875.
02795              15  FILLER              PIC S9(5)   VALUE +06151.
02796              15  FILLER              PIC S9(5)   VALUE +06374.
02797              15  FILLER              PIC S9(5)   VALUE +06514.
02798              15  FILLER              PIC S9(5)   VALUE +06560.
02799              15  FILLER              PIC S9(5)   VALUE +06512.
02800              15  FILLER              PIC S9(5)   VALUE +06368.
02801              15  FILLER              PIC S9(5)   VALUE +06149.
02802              15  FILLER              PIC S9(5)   VALUE +05914.
02803              15  FILLER              PIC S9(5)   VALUE +05457.
02804
02805          10  FILLER.
02806              15  FILLER              PIC S9(5)   VALUE +05923.
02807              15  FILLER              PIC S9(5)   VALUE +06275.
02808              15  FILLER              PIC S9(5)   VALUE +06587.
02809              15  FILLER              PIC S9(5)   VALUE +06836.
02810              15  FILLER              PIC S9(5)   VALUE +06993.
02811              15  FILLER              PIC S9(5)   VALUE +07034.
02812              15  FILLER              PIC S9(5)   VALUE +06967.
02813              15  FILLER              PIC S9(5)   VALUE +06787.
02814              15  FILLER              PIC S9(5)   VALUE +06520.
02815              15  FILLER              PIC S9(5)   VALUE +06228.
02816              15  FILLER              PIC S9(5)   VALUE +05680.
02817
02818          10  FILLER.
02819              15  FILLER              PIC S9(5)   VALUE +06249.
02820              15  FILLER              PIC S9(5)   VALUE +06639.
02821              15  FILLER              PIC S9(5)   VALUE +06986.
02822              15  FILLER              PIC S9(5)   VALUE +07260.
02823              15  FILLER              PIC S9(5)   VALUE +07434.
02824              15  FILLER              PIC S9(5)   VALUE +07468.
02825              15  FILLER              PIC S9(5)   VALUE +07379.
02826              15  FILLER              PIC S9(5)   VALUE +07161.
02827              15  FILLER              PIC S9(5)   VALUE +06844.
02828              15  FILLER              PIC S9(5)   VALUE +06490.
02829              15  FILLER              PIC S9(5)   VALUE +05854.
02830
02831          10  FILLER.
02832              15  FILLER              PIC S9(5)   VALUE +06545.
02833              15  FILLER              PIC S9(5)   VALUE +06971.
02834              15  FILLER              PIC S9(5)   VALUE +07351.
02835              15  FILLER              PIC S9(5)   VALUE +07650.
02836              15  FILLER              PIC S9(5)   VALUE +07838.
02837              15  FILLER              PIC S9(5)   VALUE +07863.
02838              15  FILLER              PIC S9(5)   VALUE +07751.
02839              15  FILLER              PIC S9(5)   VALUE +07494.
02840              15  FILLER              PIC S9(5)   VALUE +07124.
02841              15  FILLER              PIC S9(5)   VALUE +06706.
02842              15  FILLER              PIC S9(5)   VALUE +05987.
02843
02844          10  FILLER.
02845              15  FILLER              PIC S9(5)   VALUE +08653.
02846              15  FILLER              PIC S9(5)   VALUE +08706.
02847              15  FILLER              PIC S9(5)   VALUE +08136.
02848              15  FILLER              PIC S9(5)   VALUE +00000.
02849              15  FILLER              PIC S9(5)   VALUE +00000.
02850              15  FILLER              PIC S9(5)   VALUE +00000.
02851              15  FILLER              PIC S9(5)   VALUE +00000.
02852              15  FILLER              PIC S9(5)   VALUE +00000.
02853              15  FILLER              PIC S9(5)   VALUE +00000.
02854              15  FILLER              PIC S9(5)   VALUE +00000.
02855              15  FILLER              PIC S9(5)   VALUE +00000.
02856
02857          10  FILLER.
02858              15  FILLER              PIC S9(5)   VALUE +09029.
02859              15  FILLER              PIC S9(5)   VALUE +09375.
02860              15  FILLER              PIC S9(5)   VALUE +09284.
02861              15  FILLER              PIC S9(5)   VALUE +08492.
02862              15  FILLER              PIC S9(5)   VALUE +00000.
02863              15  FILLER              PIC S9(5)   VALUE +00000.
02864              15  FILLER              PIC S9(5)   VALUE +00000.
02865              15  FILLER              PIC S9(5)   VALUE +00000.
02866              15  FILLER              PIC S9(5)   VALUE +00000.
02867              15  FILLER              PIC S9(5)   VALUE +00000.
02868              15  FILLER              PIC S9(5)   VALUE +00000.
02869
02870          10  FILLER.
02871              15  FILLER              PIC S9(5)   VALUE +09277.
02872              15  FILLER              PIC S9(5)   VALUE +09815.
02873              15  FILLER              PIC S9(5)   VALUE +10040.
02874              15  FILLER              PIC S9(5)   VALUE +09730.
02875              15  FILLER              PIC S9(5)   VALUE +08712.
02876              15  FILLER              PIC S9(5)   VALUE +00000.
02877              15  FILLER              PIC S9(5)   VALUE +00000.
02878              15  FILLER              PIC S9(5)   VALUE +00000.
02879              15  FILLER              PIC S9(5)   VALUE +00000.
02880              15  FILLER              PIC S9(5)   VALUE +00000.
02881              15  FILLER              PIC S9(5)   VALUE +00000.
02882
02883          10  FILLER.
02884              15  FILLER              PIC S9(5)   VALUE +09437.
02885              15  FILLER              PIC S9(5)   VALUE +10099.
02886              15  FILLER              PIC S9(5)   VALUE +10528.
02887              15  FILLER              PIC S9(5)   VALUE +10529.
02888              15  FILLER              PIC S9(5)   VALUE +09973.
02889              15  FILLER              PIC S9(5)   VALUE +08705.
02890              15  FILLER              PIC S9(5)   VALUE +00000.
02891              15  FILLER              PIC S9(5)   VALUE +00000.
02892              15  FILLER              PIC S9(5)   VALUE +00000.
02893              15  FILLER              PIC S9(5)   VALUE +00000.
02894              15  FILLER              PIC S9(5)   VALUE +00000.
02895
02896          10  FILLER.
02897              15  FILLER              PIC S9(5)   VALUE +09640.
02898              15  FILLER              PIC S9(5)   VALUE +10460.
02899              15  FILLER              PIC S9(5)   VALUE +11147.
02900              15  FILLER              PIC S9(5)   VALUE +11542.
02901              15  FILLER              PIC S9(5)   VALUE +11572.
02902              15  FILLER              PIC S9(5)   VALUE +11117.
02903              15  FILLER              PIC S9(5)   VALUE +10414.
02904              15  FILLER              PIC S9(5)   VALUE +09479.
02905              15  FILLER              PIC S9(5)   VALUE +08425.
02906              15  FILLER              PIC S9(5)   VALUE +07454.
02907              15  FILLER              PIC S9(5)   VALUE +06318.
02908
02909      05  TAB-08-VALUES.
02910
02911          10  FILLER.
02912              15  FILLER              PIC S9(5)   VALUE +00572.
02913              15  FILLER              PIC S9(5)   VALUE +00575.
02914              15  FILLER              PIC S9(5)   VALUE +00577.
02915              15  FILLER              PIC S9(5)   VALUE +00579.
02916              15  FILLER              PIC S9(5)   VALUE +00581.
02917              15  FILLER              PIC S9(5)   VALUE +00582.
02918              15  FILLER              PIC S9(5)   VALUE +00582.
02919              15  FILLER              PIC S9(5)   VALUE +00581.
02920              15  FILLER              PIC S9(5)   VALUE +00580.
02921              15  FILLER              PIC S9(5)   VALUE +00579.
02922              15  FILLER              PIC S9(5)   VALUE +00577.
02923
02924          10  FILLER.
02925              15  FILLER              PIC S9(5)   VALUE +01583.
02926              15  FILLER              PIC S9(5)   VALUE +01604.
02927              15  FILLER              PIC S9(5)   VALUE +01622.
02928              15  FILLER              PIC S9(5)   VALUE +01637.
02929              15  FILLER              PIC S9(5)   VALUE +01647.
02930              15  FILLER              PIC S9(5)   VALUE +01652.
02931              15  FILLER              PIC S9(5)   VALUE +01651.
02932              15  FILLER              PIC S9(5)   VALUE +01643.
02933              15  FILLER              PIC S9(5)   VALUE +01633.
02934              15  FILLER              PIC S9(5)   VALUE +01624.
02935              15  FILLER              PIC S9(5)   VALUE +01601.
02936
02937          10  FILLER.
02938              15  FILLER              PIC S9(5)   VALUE +02457.
02939              15  FILLER              PIC S9(5)   VALUE +02508.
02940              15  FILLER              PIC S9(5)   VALUE +02551.
02941              15  FILLER              PIC S9(5)   VALUE +02585.
02942              15  FILLER              PIC S9(5)   VALUE +02607.
02943              15  FILLER              PIC S9(5)   VALUE +02616.
02944              15  FILLER              PIC S9(5)   VALUE +02610.
02945              15  FILLER              PIC S9(5)   VALUE +02589.
02946              15  FILLER              PIC S9(5)   VALUE +02560.
02947              15  FILLER              PIC S9(5)   VALUE +02534.
02948              15  FILLER              PIC S9(5)   VALUE +02470.
02949
02950          10  FILLER.
02951              15  FILLER              PIC S9(5)   VALUE +03224.
02952              15  FILLER              PIC S9(5)   VALUE +03311.
02953              15  FILLER              PIC S9(5)   VALUE +03385.
02954              15  FILLER              PIC S9(5)   VALUE +03443.
02955              15  FILLER              PIC S9(5)   VALUE +03477.
02956              15  FILLER              PIC S9(5)   VALUE +03488.
02957              15  FILLER              PIC S9(5)   VALUE +03474.
02958              15  FILLER              PIC S9(5)   VALUE +03434.
02959              15  FILLER              PIC S9(5)   VALUE +03379.
02960              15  FILLER              PIC S9(5)   VALUE +03326.
02961              15  FILLER              PIC S9(5)   VALUE +03202.
02962
02963          10  FILLER.
02964              15  FILLER              PIC S9(5)   VALUE +03903.
02965              15  FILLER              PIC S9(5)   VALUE +04032.
02966              15  FILLER              PIC S9(5)   VALUE +04140.
02967              15  FILLER              PIC S9(5)   VALUE +04223.
02968              15  FILLER              PIC S9(5)   VALUE +04269.
02969              15  FILLER              PIC S9(5)   VALUE +04279.
02970              15  FILLER              PIC S9(5)   VALUE +04252.
02971              15  FILLER              PIC S9(5)   VALUE +04189.
02972              15  FILLER              PIC S9(5)   VALUE +04100.
02973              15  FILLER              PIC S9(5)   VALUE +04012.
02974              15  FILLER              PIC S9(5)   VALUE +03812.
02975
02976          10  FILLER.
02977              15  FILLER              PIC S9(5)   VALUE +04510.
02978              15  FILLER              PIC S9(5)   VALUE +04682.
02979              15  FILLER              PIC S9(5)   VALUE +04825.
02980              15  FILLER              PIC S9(5)   VALUE +04933.
02981              15  FILLER              PIC S9(5)   VALUE +04990.
02982              15  FILLER              PIC S9(5)   VALUE +04997.
02983              15  FILLER              PIC S9(5)   VALUE +04955.
02984              15  FILLER              PIC S9(5)   VALUE +04863.
02985              15  FILLER              PIC S9(5)   VALUE +04734.
02986              15  FILLER              PIC S9(5)   VALUE +04602.
02987              15  FILLER              PIC S9(5)   VALUE +04316.
02988
02989          10  FILLER.
02990              15  FILLER              PIC S9(5)   VALUE +05055.
02991              15  FILLER              PIC S9(5)   VALUE +05269.
02992              15  FILLER              PIC S9(5)   VALUE +05448.
02993              15  FILLER              PIC S9(5)   VALUE +05581.
02994              15  FILLER              PIC S9(5)   VALUE +05649.
02995              15  FILLER              PIC S9(5)   VALUE +05651.
02996              15  FILLER              PIC S9(5)   VALUE +05589.
02997              15  FILLER              PIC S9(5)   VALUE +05465.
02998              15  FILLER              PIC S9(5)   VALUE +05290.
02999              15  FILLER              PIC S9(5)   VALUE +05107.
03000              15  FILLER              PIC S9(5)   VALUE +04726.
03001
03002          10  FILLER.
03003              15  FILLER              PIC S9(5)   VALUE +05546.
03004              15  FILLER              PIC S9(5)   VALUE +05802.
03005              15  FILLER              PIC S9(5)   VALUE +06016.
03006              15  FILLER              PIC S9(5)   VALUE +06173.
03007              15  FILLER              PIC S9(5)   VALUE +06253.
03008              15  FILLER              PIC S9(5)   VALUE +06247.
03009              15  FILLER              PIC S9(5)   VALUE +06164.
03010              15  FILLER              PIC S9(5)   VALUE +06002.
03011              15  FILLER              PIC S9(5)   VALUE +05777.
03012              15  FILLER              PIC S9(5)   VALUE +05536.
03013              15  FILLER              PIC S9(5)   VALUE +05056.
03014
03015          10  FILLER.
03016              15  FILLER              PIC S9(5)   VALUE +05991.
03017              15  FILLER              PIC S9(5)   VALUE +06286.
03018              15  FILLER              PIC S9(5)   VALUE +06534.
03019              15  FILLER              PIC S9(5)   VALUE +06715.
03020              15  FILLER              PIC S9(5)   VALUE +06807.
03021              15  FILLER              PIC S9(5)   VALUE +06791.
03022              15  FILLER              PIC S9(5)   VALUE +06684.
03023              15  FILLER              PIC S9(5)   VALUE +06481.
03024              15  FILLER              PIC S9(5)   VALUE +06203.
03025              15  FILLER              PIC S9(5)   VALUE +05898.
03026              15  FILLER              PIC S9(5)   VALUE +05316.
03027
03028          10  FILLER.
03029              15  FILLER              PIC S9(5)   VALUE +06394.
03030              15  FILLER              PIC S9(5)   VALUE +06727.
03031              15  FILLER              PIC S9(5)   VALUE +07008.
03032              15  FILLER              PIC S9(5)   VALUE +07211.
03033              15  FILLER              PIC S9(5)   VALUE +07317.
03034              15  FILLER              PIC S9(5)   VALUE +07288.
03035              15  FILLER              PIC S9(5)   VALUE +07154.
03036              15  FILLER              PIC S9(5)   VALUE +06909.
03037              15  FILLER              PIC S9(5)   VALUE +06575.
03038              15  FILLER              PIC S9(5)   VALUE +06201.
03039              15  FILLER              PIC S9(5)   VALUE +05518.
03040
03041          10  FILLER.
03042              15  FILLER              PIC S9(5)   VALUE +06760.
03043              15  FILLER              PIC S9(5)   VALUE +07129.
03044              15  FILLER              PIC S9(5)   VALUE +07442.
03045              15  FILLER              PIC S9(5)   VALUE +07667.
03046              15  FILLER              PIC S9(5)   VALUE +07785.
03047              15  FILLER              PIC S9(5)   VALUE +07742.
03048              15  FILLER              PIC S9(5)   VALUE +07580.
03049              15  FILLER              PIC S9(5)   VALUE +07290.
03050              15  FILLER              PIC S9(5)   VALUE +06897.
03051              15  FILLER              PIC S9(5)   VALUE +06450.
03052              15  FILLER              PIC S9(5)   VALUE +05673.
03053
03054          10  FILLER.
03055              15  FILLER              PIC S9(5)   VALUE +09364.
03056              15  FILLER              PIC S9(5)   VALUE +09230.
03057              15  FILLER              PIC S9(5)   VALUE +08376.
03058              15  FILLER              PIC S9(5)   VALUE +00000.
03059              15  FILLER              PIC S9(5)   VALUE +00000.
03060              15  FILLER              PIC S9(5)   VALUE +00000.
03061              15  FILLER              PIC S9(5)   VALUE +00000.
03062              15  FILLER              PIC S9(5)   VALUE +00000.
03063              15  FILLER              PIC S9(5)   VALUE +00000.
03064              15  FILLER              PIC S9(5)   VALUE +00000.
03065              15  FILLER              PIC S9(5)   VALUE +00000.
03066
03067          10  FILLER.
03068              15  FILLER              PIC S9(5)   VALUE +09828.
03069              15  FILLER              PIC S9(5)   VALUE +10040.
03070              15  FILLER              PIC S9(5)   VALUE +09743.
03071              15  FILLER              PIC S9(5)   VALUE +86530.
03072              15  FILLER              PIC S9(5)   VALUE +00000.
03073              15  FILLER              PIC S9(5)   VALUE +00000.
03074              15  FILLER              PIC S9(5)   VALUE +00000.
03075              15  FILLER              PIC S9(5)   VALUE +00000.
03076              15  FILLER              PIC S9(5)   VALUE +00000.
03077              15  FILLER              PIC S9(5)   VALUE +00000.
03078              15  FILLER              PIC S9(5)   VALUE +00000.
03079
03080          10  FILLER.
03081              15  FILLER              PIC S9(5)   VALUE +10134.
03082              15  FILLER              PIC S9(5)   VALUE +10574.
03083              15  FILLER              PIC S9(5)   VALUE +10642.
03084              15  FILLER              PIC S9(5)   VALUE +10103.
03085              15  FILLER              PIC S9(5)   VALUE +08797.
03086              15  FILLER              PIC S9(5)   VALUE +00000.
03087              15  FILLER              PIC S9(5)   VALUE +00000.
03088              15  FILLER              PIC S9(5)   VALUE +00000.
03089              15  FILLER              PIC S9(5)   VALUE +00000.
03090              15  FILLER              PIC S9(5)   VALUE +00000.
03091              15  FILLER              PIC S9(5)   VALUE +00000.
03092
03093          10  FILLER.
03094              15  FILLER              PIC S9(5)   VALUE +10332.
03095              15  FILLER              PIC S9(5)   VALUE +10918.
03096              15  FILLER              PIC S9(5)   VALUE +11222.
03097              15  FILLER              PIC S9(5)   VALUE +11039.
03098              15  FILLER              PIC S9(5)   VALUE +10256.
03099              15  FILLER              PIC S9(5)   VALUE +08708.
03100              15  FILLER              PIC S9(5)   VALUE +00000.
03101              15  FILLER              PIC S9(5)   VALUE +00000.
03102              15  FILLER              PIC S9(5)   VALUE +00000.
03103              15  FILLER              PIC S9(5)   VALUE +00000.
03104              15  FILLER              PIC S9(5)   VALUE +00000.
03105
03106          10  FILLER.
03107              15  FILLER              PIC S9(5)   VALUE +10583.
03108              15  FILLER              PIC S9(5)   VALUE +11354.
03109              15  FILLER              PIC S9(5)   VALUE +11959.
03110              15  FILLER              PIC S9(5)   VALUE +12226.
03111              15  FILLER              PIC S9(5)   VALUE +12106.
03112              15  FILLER              PIC S9(5)   VALUE +11475.
03113              15  FILLER              PIC S9(5)   VALUE +10623.
03114              15  FILLER              PIC S9(5)   VALUE +09560.
03115              15  FILLER              PIC S9(5)   VALUE +08390.
03116              15  FILLER              PIC S9(5)   VALUE +07312.
03117              15  FILLER              PIC S9(5)   VALUE +06058.
03118
03119      05  TAB-09-VALUES.
03120
03121          10  FILLER.
03122              15  FILLER              PIC S9(5)   VALUE +00577.
03123              15  FILLER              PIC S9(5)   VALUE +00579.
03124              15  FILLER              PIC S9(5)   VALUE +00581.
03125              15  FILLER              PIC S9(5)   VALUE +00583.
03126              15  FILLER              PIC S9(5)   VALUE +00584.
03127              15  FILLER              PIC S9(5)   VALUE +00584.
03128              15  FILLER              PIC S9(5)   VALUE +00584.
03129              15  FILLER              PIC S9(5)   VALUE +00582.
03130              15  FILLER              PIC S9(5)   VALUE +00581.
03131              15  FILLER              PIC S9(5)   VALUE +00580.
03132              15  FILLER              PIC S9(5)   VALUE +00576.
03133
03134          10  FILLER.
03135              15  FILLER              PIC S9(5)   VALUE +01614.
03136              15  FILLER              PIC S9(5)   VALUE +01633.
03137              15  FILLER              PIC S9(5)   VALUE +01648.
03138              15  FILLER              PIC S9(5)   VALUE +01659.
03139              15  FILLER              PIC S9(5)   VALUE +01665.
03140              15  FILLER              PIC S9(5)   VALUE +01665.
03141              15  FILLER              PIC S9(5)   VALUE +01661.
03142              15  FILLER              PIC S9(5)   VALUE +01651.
03143              15  FILLER              PIC S9(5)   VALUE +01638.
03144              15  FILLER              PIC S9(5)   VALUE +01625.
03145              15  FILLER              PIC S9(5)   VALUE +01594.
03146
03147          10  FILLER.
03148              15  FILLER              PIC S9(5)   VALUE +02524.
03149              15  FILLER              PIC S9(5)   VALUE +02570.
03150              15  FILLER              PIC S9(5)   VALUE +02606.
03151              15  FILLER              PIC S9(5)   VALUE +02633.
03152              15  FILLER              PIC S9(5)   VALUE +02645.
03153              15  FILLER              PIC S9(5)   VALUE +02643.
03154              15  FILLER              PIC S9(5)   VALUE +02630.
03155              15  FILLER              PIC S9(5)   VALUE +02606.
03156              15  FILLER              PIC S9(5)   VALUE +02572.
03157              15  FILLER              PIC S9(5)   VALUE +02535.
03158              15  FILLER              PIC S9(5)   VALUE +02450.
03159
03160          10  FILLER.
03161              15  FILLER              PIC S9(5)   VALUE +03331.
03162              15  FILLER              PIC S9(5)   VALUE +03410.
03163              15  FILLER              PIC S9(5)   VALUE +03473.
03164              15  FILLER              PIC S9(5)   VALUE +03517.
03165              15  FILLER              PIC S9(5)   VALUE +03536.
03166              15  FILLER              PIC S9(5)   VALUE +03530.
03167              15  FILLER              PIC S9(5)   VALUE +03504.
03168              15  FILLER              PIC S9(5)   VALUE +03459.
03169              15  FILLER              PIC S9(5)   VALUE +03394.
03170              15  FILLER              PIC S9(5)   VALUE +03322.
03171              15  FILLER              PIC S9(5)   VALUE +03165.
03172
03173          10  FILLER.
03174              15  FILLER              PIC S9(5)   VALUE +04051.
03175              15  FILLER              PIC S9(5)   VALUE +04167.
03176              15  FILLER              PIC S9(5)   VALUE +04260.
03177              15  FILLER              PIC S9(5)   VALUE +04323.
03178              15  FILLER              PIC S9(5)   VALUE +04348.
03179              15  FILLER              PIC S9(5)   VALUE +04335.
03180              15  FILLER              PIC S9(5)   VALUE +04292.
03181              15  FILLER              PIC S9(5)   VALUE +04220.
03182              15  FILLER              PIC S9(5)   VALUE +04116.
03183              15  FILLER              PIC S9(5)   VALUE +04000.
03184              15  FILLER              PIC S9(5)   VALUE +03755.
03185
03186          10  FILLER.
03187              15  FILLER              PIC S9(5)   VALUE +04698.
03188              15  FILLER              PIC S9(5)   VALUE +04852.
03189              15  FILLER              PIC S9(5)   VALUE +04976.
03190              15  FILLER              PIC S9(5)   VALUE +05058.
03191              15  FILLER              PIC S9(5)   VALUE +05091.
03192              15  FILLER              PIC S9(5)   VALUE +05068.
03193              15  FILLER              PIC S9(5)   VALUE +05005.
03194              15  FILLER              PIC S9(5)   VALUE +04900.
03195              15  FILLER              PIC S9(5)   VALUE +04750.
03196              15  FILLER              PIC S9(5)   VALUE +04581.
03197              15  FILLER              PIC S9(5)   VALUE +04235.
03198
03199          10  FILLER.
03200              15  FILLER              PIC S9(5)   VALUE +05281.
03201              15  FILLER              PIC S9(5)   VALUE +05474.
03202              15  FILLER              PIC S9(5)   VALUE +05628.
03203              15  FILLER              PIC S9(5)   VALUE +05730.
03204              15  FILLER              PIC S9(5)   VALUE +05771.
03205              15  FILLER              PIC S9(5)   VALUE +05737.
03206              15  FILLER              PIC S9(5)   VALUE +05650.
03207              15  FILLER              PIC S9(5)   VALUE +05507.
03208              15  FILLER              PIC S9(5)   VALUE +05305.
03209              15  FILLER              PIC S9(5)   VALUE +05074.
03210              15  FILLER              PIC S9(5)   VALUE +04621.
03211
03212          10  FILLER.
03213              15  FILLER              PIC S9(5)   VALUE +05808.
03214              15  FILLER              PIC S9(5)   VALUE +06038.
03215              15  FILLER              PIC S9(5)   VALUE +06223.
03216              15  FILLER              PIC S9(5)   VALUE +06344.
03217              15  FILLER              PIC S9(5)   VALUE +06395.
03218              15  FILLER              PIC S9(5)   VALUE +06347.
03219              15  FILLER              PIC S9(5)   VALUE +06233.
03220              15  FILLER              PIC S9(5)   VALUE +06048.
03221              15  FILLER              PIC S9(5)   VALUE +05791.
03222              15  FILLER              PIC S9(5)   VALUE +05490.
03223              15  FILLER              PIC S9(5)   VALUE +04926.
03224
03225          10  FILLER.
03226              15  FILLER              PIC S9(5)   VALUE +06286.
03227              15  FILLER              PIC S9(5)   VALUE +06552.
03228              15  FILLER              PIC S9(5)   VALUE +06768.
03229              15  FILLER              PIC S9(5)   VALUE +06908.
03230              15  FILLER              PIC S9(5)   VALUE +06968.
03231              15  FILLER              PIC S9(5)   VALUE +06905.
03232              15  FILLER              PIC S9(5)   VALUE +06761.
03233              15  FILLER              PIC S9(5)   VALUE +06532.
03234              15  FILLER              PIC S9(5)   VALUE +06215.
03235              15  FILLER              PIC S9(5)   VALUE +05837.
03236              15  FILLER              PIC S9(5)   VALUE +05163.
03237
03238          10  FILLER.
03239              15  FILLER              PIC S9(5)   VALUE +06721.
03240              15  FILLER              PIC S9(5)   VALUE +07021.
03241              15  FILLER              PIC S9(5)   VALUE +07267.
03242              15  FILLER              PIC S9(5)   VALUE +07425.
03243              15  FILLER              PIC S9(5)   VALUE +07496.
03244              15  FILLER              PIC S9(5)   VALUE +07413.
03245              15  FILLER              PIC S9(5)   VALUE +07239.
03246              15  FILLER              PIC S9(5)   VALUE +06962.
03247              15  FILLER              PIC S9(5)   VALUE +06582.
03248              15  FILLER              PIC S9(5)   VALUE +06123.
03249              15  FILLER              PIC S9(5)   VALUE +05345.
03250
03251          10  FILLER.
03252              15  FILLER              PIC S9(5)   VALUE +09811.
03253              15  FILLER              PIC S9(5)   VALUE +09471.
03254              15  FILLER              PIC S9(5)   VALUE +08339.
03255              15  FILLER              PIC S9(5)   VALUE +00000.
03256              15  FILLER              PIC S9(5)   VALUE +00000.
03257              15  FILLER              PIC S9(5)   VALUE +00000.
03258              15  FILLER              PIC S9(5)   VALUE +00000.
03259              15  FILLER              PIC S9(5)   VALUE +00000.
03260              15  FILLER              PIC S9(5)   VALUE +00000.
03261              15  FILLER              PIC S9(5)   VALUE +00000.
03262              15  FILLER              PIC S9(5)   VALUE +00000.
03263
03264          10  FILLER.
03265              15  FILLER              PIC S9(5)   VALUE +10363.
03266              15  FILLER              PIC S9(5)   VALUE +10416.
03267              15  FILLER              PIC S9(5)   VALUE +09908.
03268              15  FILLER              PIC S9(5)   VALUE +08544.
03269              15  FILLER              PIC S9(5)   VALUE +00000.
03270              15  FILLER              PIC S9(5)   VALUE +00000.
03271              15  FILLER              PIC S9(5)   VALUE +00000.
03272              15  FILLER              PIC S9(5)   VALUE +00000.
03273              15  FILLER              PIC S9(5)   VALUE +00000.
03274              15  FILLER              PIC S9(5)   VALUE +00000.
03275              15  FILLER              PIC S9(5)   VALUE +00000.
03276
03277          10  FILLER.
03278              15  FILLER              PIC S9(5)   VALUE +10726.
03279              15  FILLER              PIC S9(5)   VALUE +11038.
03280              15  FILLER              PIC S9(5)   VALUE +10941.
03281              15  FILLER              PIC S9(5)   VALUE +10189.
03282              15  FILLER              PIC S9(5)   VALUE +08635.
03283              15  FILLER              PIC S9(5)   VALUE +00000.
03284              15  FILLER              PIC S9(5)   VALUE +00000.
03285              15  FILLER              PIC S9(5)   VALUE +00000.
03286              15  FILLER              PIC S9(5)   VALUE +00000.
03287              15  FILLER              PIC S9(5)   VALUE +00000.
03288              15  FILLER              PIC S9(5)   VALUE +00000.
03289
03290          10  FILLER.
03291              15  FILLER              PIC S9(5)   VALUE +10961.
03292              15  FILLER              PIC S9(5)   VALUE +11439.
03293              15  FILLER              PIC S9(5)   VALUE +11608.
03294              15  FILLER              PIC S9(5)   VALUE +11251.
03295              15  FILLER              PIC S9(5)   VALUE +10277.
03296              15  FILLER              PIC S9(5)   VALUE +08496.
03297              15  FILLER              PIC S9(5)   VALUE +00000.
03298              15  FILLER              PIC S9(5)   VALUE +00000.
03299              15  FILLER              PIC S9(5)   VALUE +00000.
03300              15  FILLER              PIC S9(5)   VALUE +00000.
03301              15  FILLER              PIC S9(5)   VALUE +00000.
03302
03303          10  FILLER.
03304              15  FILLER              PIC S9(5)   VALUE +11258.
03305              15  FILLER              PIC S9(5)   VALUE +11948.
03306              15  FILLER              PIC S9(5)   VALUE +12454.
03307              15  FILLER              PIC S9(5)   VALUE +12599.
03308              15  FILLER              PIC S9(5)   VALUE +12361.
03309              15  FILLER              PIC S9(5)   VALUE +11600.
03310              15  FILLER              PIC S9(5)   VALUE +10656.
03311              15  FILLER              PIC S9(5)   VALUE +09527.
03312              15  FILLER              PIC S9(5)   VALUE +08284.
03313              15  FILLER              PIC S9(5)   VALUE +07114.
03314              15  FILLER              PIC S9(5)   VALUE +05795.
03315
03316
00043
00044          EJECT
00045 *                           COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00046
00047
00048      EJECT
00049 *                           COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
012820                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
012820       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
00050
00051      EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00053  01  DFHCOMMAREA              PIC X(450).
00054
00055      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELRESV' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00057
00058      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.
00059
uktdel*0000-CALCULATE-RESERVES SECTION. COPY ELCRESP1.
uktins 0000-CALCULATE-RESERVES SECTION.
uktins*    COPY ELCRESP1.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCRESP1.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                         *
00006 *                                                               *
00007 *                PROCEDURE DIVISION USED TO CALCULATE THE       *
00008 *            FUTURE, I.B.N.R., AND PAY TO CURRENT RESERVES.     *
00009 *                                                               *
00010 *****************************************************************
032612******************************************************************
032612*                   C H A N G E   L O G
032612*
032612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
032612*-----------------------------------------------------------------
032612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
032612* EFFECTIVE    NUMBER
032612*-----------------------------------------------------------------
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
060414* 060414  IR2014060200001  PEMA  DISAB REM AMT CHANGES
092920* 092920  CR2020092100001  PEMA  Increase adj on > 23 mos disab
032612******************************************************************
00011
00012      MOVE ZERO                   TO  CP-CDT-TABLE
00013                                      CP-CDT-FACTOR
00014                                      CP-PTC-RESERVE
00015                                      CP-IBNR-RESERVE
00016                                      CP-FUTURE-RESERVE.
00017
00018      IF LCP-ONCTR-01 =  0
00019          ADD 1 TO LCP-ONCTR-01
00020          MOVE WS-WORK-AREAS          TO  WS-INITIAL-WORK-AREAS
00021        ELSE
00022          MOVE WS-INITIAL-WORK-AREAS  TO  WS-WORK-AREAS.
00023
00024      IF CP-LIFE-CLAIM
00025          MOVE CP-REMAINING-AMT   TO CP-PTC-RESERVE
00026          GO TO 0100-CALCULATE-AGE.
00027
           IF (CP-R-MAX-MON-BEN NOT = ZEROS)
060414        and (cp-r-max-mon-ben < cp-original-benefit)
              MOVE CP-R-MAX-MON-BEN    TO CP-ORIGINAL-BENEFIT
           END-IF
00028      IF CP-CLAIM-STATUS = 'C'
00029          GO TO 0100-CALCULATE-AGE.
00030
00031 *    NOTE *******************************************************
00032 *         *                                                     *
00033 *         *      CALCULATE THE NUMBER OF MONTHS DISABLED TO     *
00034 *         *  DETERMINE WHICH DISABILITY TABLE TO USE.           *
00035 *         *                                                     *
00036 *         *******************************************************.
00037
00038      IF CP-COMPANY-ID = 'CVL'  OR  'CNL'
00039         IF CP-PAID-THRU-DT EQUAL LOW-VALUES
00040            MOVE CP-INCURRED-DT   TO  DC-BIN-DATE-2
00041         ELSE
00042            MOVE CP-PAID-THRU-DT  TO  DC-BIN-DATE-2
00043      ELSE
00044         MOVE CP-VALUATION-DT     TO  DC-BIN-DATE-2.
00045
00046      MOVE CP-INCURRED-DT         TO  DC-BIN-DATE-1.
00047      MOVE '1'                    TO  DC-OPTION-CODE.
00048      PERFORM 8500-DATE-CONVERSION.
00049
00050      IF DC-ERROR-CODE NOT = SPACES
00051          MOVE '2'                TO  CP-RETURN-CODE
00052          GO TO 0999-EXIT.
00053
00054      MOVE DC-ELAPSED-MONTHS      TO  WS-MONTHS-DISABLED.
00055
00056      IF WS-MONTHS-DISABLED LESS +1
00057          MOVE TAB-01-VALUES      TO  TABLE-WORK-AREA
00058          MOVE +1                 TO  WS-DISABILITY-TABLE
00059          GO TO 0100-CALCULATE-AGE.
00060
00061      IF WS-MONTHS-DISABLED LESS +2
00062          MOVE TAB-02-VALUES      TO  TABLE-WORK-AREA
00063          MOVE +2                 TO  WS-DISABILITY-TABLE
00064          GO TO 0100-CALCULATE-AGE.
00065
00066      IF WS-MONTHS-DISABLED LESS +6
00067          MOVE TAB-03-VALUES      TO  TABLE-WORK-AREA
00068          MOVE +3                 TO  WS-DISABILITY-TABLE
00069          GO TO 0100-CALCULATE-AGE.
00070
00071      IF WS-MONTHS-DISABLED LESS +12
00072          MOVE TAB-04-VALUES      TO  TABLE-WORK-AREA
00073          MOVE +4                 TO  WS-DISABILITY-TABLE
00074          GO TO 0100-CALCULATE-AGE.
00075
00076      IF WS-MONTHS-DISABLED LESS +24
00077          MOVE TAB-05-VALUES      TO  TABLE-WORK-AREA
00078          MOVE +5                 TO  WS-DISABILITY-TABLE
00079          GO TO 0100-CALCULATE-AGE.
00080
00081      IF WS-MONTHS-DISABLED LESS +36
00082          MOVE TAB-06-VALUES      TO  TABLE-WORK-AREA
00083          MOVE +6                 TO  WS-DISABILITY-TABLE
00084          GO TO 0100-CALCULATE-AGE.
00085
00086      IF WS-MONTHS-DISABLED LESS +48
00087          MOVE TAB-07-VALUES      TO  TABLE-WORK-AREA
00088          MOVE +7                 TO  WS-DISABILITY-TABLE
00089          GO TO 0100-CALCULATE-AGE.
00090
00091      IF WS-MONTHS-DISABLED LESS +60
00092          MOVE TAB-08-VALUES      TO  TABLE-WORK-AREA
00093          MOVE +8                 TO  WS-DISABILITY-TABLE
00094          GO TO 0100-CALCULATE-AGE.
00095
00096      MOVE TAB-09-VALUES          TO  TABLE-WORK-AREA.
00097      MOVE +9                     TO  WS-DISABILITY-TABLE.
00098
00099  0100-CALCULATE-AGE.
00100 *    NOTE *******************************************************
00101 *         *                                                     *
00102 *         *      CALCULATE THE AGE AT THE TIME OF DISABLEMENT.  *
00103 *         *                                                     *
00104 *         *******************************************************.
00105
00106      MOVE +0                     TO CP-INCURRED-AGE.
00107
032612     IF (CP-COMPANY-ID = 'MON' or 'AHL')
032612        AND (CP-INSURED-BIRTH-DT NOT = LOW-VALUES)
032612        CONTINUE
032612     ELSE
032612        GO TO 0110-CONTINUE-CALC-AGE
032612     END-IF
00113
00114      MOVE CP-INSURED-BIRTH-DT    TO DC-BIN-DATE-1
00115      MOVE CP-INCURRED-DT         TO DC-BIN-DATE-2
00116      MOVE '1'                    TO DC-OPTION-CODE
00117      PERFORM 8500-DATE-CONVERSION.
00118
00119      IF DATE-CONVERSION-ERROR
00120         GO TO 0110-CONTINUE-CALC-AGE.
00121
00122      COMPUTE WS-DISABLED-AGE =
00123                  (DC-ELAPSED-MONTHS / +12).
00124
00125      MOVE WS-DISABLED-AGE    TO CP-INCURRED-AGE.
00126
00127      GO TO 0120-CONTINUE-CALC-AGE.
00128
00129  0110-CONTINUE-CALC-AGE.
00130
00131      MOVE CP-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00132      MOVE CP-INCURRED-DT         TO  DC-BIN-DATE-2.
00133      MOVE '1'                    TO  DC-OPTION-CODE.
00134      PERFORM 8500-DATE-CONVERSION.
00135
00136      IF DC-ERROR-CODE NOT = SPACE
00137          MOVE '2'                TO  CP-RETURN-CODE
00138          GO TO 0999-EXIT.
00139
00140      IF CP-ISSUE-AGE NOT GREATER ZERO
00141          MOVE +40                TO  CP-ISSUE-AGE.
00142
00143      COMPUTE WS-DISABLED-AGE =
00144                  (DC-ELAPSED-MONTHS / +12) + CP-ISSUE-AGE.
00145
00146      MOVE WS-DISABLED-AGE    TO CP-INCURRED-AGE.
00147
00148  0120-CONTINUE-CALC-AGE.
00149
00150      IF CP-LIFE-CLAIM
00151          GO TO 0999-EXIT.
00152
00153      IF CP-CLAIM-STATUS = 'C'
00154          GO TO 0405-CALCULATE-IBNR-RESERVES.
00155
00156      MOVE WS-DISABLED-AGE        TO  AGE-INDEX.
00157
00158      IF AGE-INDEX LESS +22
00159          MOVE +22                TO  AGE-INDEX.
00160
00161      IF AGE-INDEX GREATER +72
00162          MOVE +72                TO  AGE-INDEX.
00163
00164      SUBTRACT +17 FROM AGE-INDEX.
00165      DIVIDE +5 INTO AGE-INDEX.
00166
00167 *    NOTE *******************************************************
00168 *         *                                                     *
00169 *         *          CALCULATE COVERAGE EXPIRE DATE             *
00170 *         *                                                     *
00171 *         *******************************************************.
00172
00173      MOVE CP-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00174      MOVE CP-ORIGINAL-TERM       TO  DC-ELAPSED-MONTHS.
00175      MOVE ZERO                   TO  DC-ELAPSED-DAYS.
00176      MOVE '6'                    TO  DC-OPTION-CODE.
00177      PERFORM 8500-DATE-CONVERSION.
00178
00179      IF DC-ERROR-CODE NOT = SPACES
00180          MOVE '2'                TO  CP-RETURN-CODE
00181          GO TO 0999-EXIT.
00182
00183      MOVE DC-BIN-DATE-2          TO  WS-EXPIRE-DATE.
032612     if cp-company-id = 'AHL'
032612        if cp-expire-dt not = low-values and spaces
032612           move cp-expire-dt     to ws-expire-date
032612        end-if
032612     end-if
00185 *    NOTE *******************************************************
00186 *         *                                                     *
00187 *         *      CALCULATE NUMBER OF MONTHS FROM VALUATION      *
00188 *         *  DATE TO EXPIRY DATE (IE. REMAINING TERM).          *
00189 *         *                                                     *
00190 *         *      IF THE EXPIRE DATE HAS PAST BYPASS THE FUTURE  *
00191 *         *  RESERVE CALCULATIONS AND ONLY CALCULATE THE PTC    *
00192 *         *  RESERVE.                                           *
00193 *         *                                                     *
00194 *         *******************************************************
00195
00196      IF CP-COMPANY-ID = 'CVL'  OR  'CNL'
00197         IF CP-PAID-THRU-DT EQUAL LOW-VALUES
00198            MOVE CP-INCURRED-DT   TO  DC-BIN-DATE-1
00199         ELSE
00200            MOVE CP-PAID-THRU-DT  TO  DC-BIN-DATE-1
00201      ELSE
00202         MOVE CP-VALUATION-DT     TO  DC-BIN-DATE-1.
00203
00204 *    IF CP-COMPANY-ID = 'FIM'
00205 *        IF CP-PAID-THRU-DT = LOW-VALUES
00206 *            MOVE CP-INCURRED-DT     TO  DC-BIN-DATE-1
00207 *        ELSE
00208 *            MOVE CP-PAID-THRU-DT    TO  DC-BIN-DATE-1.
00209 *
00210      IF DC-BIN-DATE-1 NOT LESS WS-EXPIRE-DATE
00211          GO TO 0405-CALCULATE-IBNR-RESERVES.
00212
00213      MOVE WS-EXPIRE-DATE         TO  DC-BIN-DATE-2.
00214      MOVE '1'                    TO  DC-OPTION-CODE.
00215      PERFORM 8500-DATE-CONVERSION.
00216
00217      IF DC-ERROR-CODE NOT = SPACES
00218          MOVE '2'                TO  CP-RETURN-CODE
00219          GO TO 0999-EXIT.
00220
00221      MOVE DC-ELAPSED-MONTHS      TO  MONTH-INDEX
00222                                      WS-REMAINING-TERM.
00223
00224      IF (CP-CRITICAL-PERIOD)
00225         AND (CP-CRITICAL-MONTHS > +0)
00226         AND (WS-REMAINING-TERM > (CP-CRITICAL-MONTHS -
                   WS-MONTHS-DISABLED))
00228         COMPUTE WS-REMAINING-TERM =
00229         (CP-CRITICAL-MONTHS - WS-MONTHS-DISABLED)
00230         MOVE WS-REMAINING-TERM TO MONTH-INDEX.
00231
00232
00233 *    NOTE *******************************************************
00234 *         *                                                     *
00235 *         *      IF THE INSURED HAS BEEN DISABLED MORE THAN     *
00236 *         *  TWELVE (12) MONTHS THE CALCULATION IS DONE IN      *
00237 *         *  YEARS.                                             *
00238 *         *                                                     *
00239 *         *******************************************************.
00240
00241      IF WS-DISABILITY-TABLE GREATER +4
00242          GO TO 0200-CALCULATE-YEARS.
00243
00244      DIVIDE DC-ODD-DAYS-OVER BY DC-DAYS-IN-MONTH
00245                                  GIVING TERM-INDEX-DIFF.
00246
00247      MOVE DC-ODD-DAYS-OVER       TO  WS-ODD-DAYS-OVER.
00248      MOVE DC-DAYS-IN-MONTH       TO  WS-DAYS-IN-MONTH.
00249
00250      COMPUTE WS-EXPIRE-AGE =
00251                            CP-ISSUE-AGE + (CP-ORIGINAL-TERM / 12)
00252
00253      IF MONTH-INDEX LESS +24
00254          GO TO 0300-CALCULATE-RESERVES.
00255
00256      IF MONTH-INDEX NOT GREATER +180
00257          COMPUTE MONTH-INDEX = ((MONTH-INDEX - 24) / 12) + 24
00258          MOVE MONTH-INDEX        TO  TERM-INDEX-DIFF
00259          GO TO 0305-CALCULATE-RESERVES.
00260
00261      IF WS-EXPIRE-AGE GREATER +65
00262          MOVE +42                TO  MONTH-INDEX
00263          MOVE ZERO               TO  TERM-INDEX-DIFF
00264          GO TO 0300-CALCULATE-RESERVES.
00265
00266      IF WS-EXPIRE-AGE GREATER +60
00267          MOVE +41                TO  MONTH-INDEX
00268          COMPUTE TERM-INDEX-DIFF = (WS-EXPIRE-AGE - +60) / +5
00269          IF AGE-INDEX GREATER +6
00270              MOVE +6             TO  AGE-INDEX
00271              GO TO 0300-CALCULATE-RESERVES
00272            ELSE
00273              GO TO 0300-CALCULATE-RESERVES.
00274
00275      IF WS-EXPIRE-AGE GREATER +55
00276          MOVE +40                TO  MONTH-INDEX
00277          COMPUTE TERM-INDEX-DIFF = (WS-EXPIRE-AGE - +55) / +5
00278          IF AGE-INDEX GREATER +5
00279              MOVE +5             TO  AGE-INDEX
00280              GO TO 0300-CALCULATE-RESERVES
00281            ELSE
00282              GO TO 0300-CALCULATE-RESERVES.
00283
00284      IF WS-EXPIRE-AGE GREATER +50
00285          MOVE +39                TO  MONTH-INDEX
00286          COMPUTE TERM-INDEX-DIFF = (WS-EXPIRE-AGE - +50) / +5
00287          IF AGE-INDEX GREATER +4
00288              MOVE +4             TO  AGE-INDEX
00289              GO TO 0300-CALCULATE-RESERVES
00290            ELSE
00291              GO TO 0300-CALCULATE-RESERVES.
00292
00293      MOVE +38                    TO  MONTH-INDEX.
00294      MOVE ZERO                   TO  TERM-INDEX-DIFF.
00295
00296      IF AGE-INDEX GREATER +3
00297          MOVE +3                 TO  AGE-INDEX.
00298
00299      GO TO 0300-CALCULATE-RESERVES.
00300
00301      EJECT
00302  0200-CALCULATE-YEARS.
00303      DIVIDE +12 INTO MONTH-INDEX.
00304
00305      MOVE MONTH-INDEX            TO  TERM-INDEX-DIFF.
00306
00307      IF YEAR-INDEX LESS +1  AND
00308         NOT CP-CDT-INTERPOLATED
00309          MOVE +1                 TO  YEAR-INDEX.
00310
00311      IF (WS-DISABILITY-TABLE = +5 AND
00312          YEAR-INDEX LESS +15)
00313        OR
00314         (WS-DISABILITY-TABLE = +6 AND
00315          YEAR-INDEX LESS +14)
00316        OR
00317         (WS-DISABILITY-TABLE = +7 AND
00318          YEAR-INDEX LESS +13)
00319        OR
00320         (WS-DISABILITY-TABLE = +8 AND
00321          YEAR-INDEX LESS +12)
00322        OR
00323         (WS-DISABILITY-TABLE = +9 AND
00324          YEAR-INDEX LESS +13)
00325              GO TO 0305-CALCULATE-RESERVES.
00326
00327      COMPUTE WS-EXPIRE-AGE =
00328                            CP-ISSUE-AGE + (CP-ORIGINAL-TERM / 12).
00329
00330      COMPUTE YEAR-INDEX = (WS-DISABILITY-TABLE * -1) + 5.
00331
00332      IF WS-EXPIRE-AGE GREATER +65
00333          ADD +19                 TO  YEAR-INDEX
00334          MOVE ZERO               TO  TERM-INDEX-DIFF
00335          GO TO 0300-CALCULATE-RESERVES.
00336
00337      IF WS-EXPIRE-AGE GREATER +60
00338          ADD +18  TO  YEAR-INDEX
00339          COMPUTE TERM-INDEX-DIFF = (WS-EXPIRE-AGE - +60) / +5
00340          IF AGE-INDEX GREATER +6
00341              MOVE +6             TO  AGE-INDEX
00342              GO TO 0300-CALCULATE-RESERVES
00343            ELSE
00344              GO TO 0300-CALCULATE-RESERVES.
00345
00346      IF WS-EXPIRE-AGE GREATER +55
00347          ADD +17  TO  YEAR-INDEX
00348          COMPUTE TERM-INDEX-DIFF = (WS-EXPIRE-AGE - +55) / +5
00349          IF AGE-INDEX GREATER +5
00350              MOVE +5             TO  AGE-INDEX
00351              GO TO 0300-CALCULATE-RESERVES
00352            ELSE
00353              GO TO 0300-CALCULATE-RESERVES.
00354
00355      IF WS-EXPIRE-AGE GREATER +50
00356          ADD +16  TO  YEAR-INDEX
00357          COMPUTE TERM-INDEX-DIFF = (WS-EXPIRE-AGE - +50) / +5
00358          IF AGE-INDEX GREATER +4
00359              MOVE +4             TO  AGE-INDEX
00360              GO TO 0300-CALCULATE-RESERVES
00361            ELSE
00362              GO TO 0300-CALCULATE-RESERVES.
00363
00364      ADD +15  TO  YEAR-INDEX.
00365      MOVE ZERO                   TO  TERM-INDEX-DIFF.
00366
00367      IF AGE-INDEX GREATER +3
00368          MOVE +3             TO  AGE-INDEX.
00369
00370      GO TO 0300-CALCULATE-RESERVES.
00371
00372      EJECT
00373  0300-CALCULATE-RESERVES.
00374
00375      ADD TERM-INDEX-DIFF  TO  TERM-INDEX.
00376
00377  0305-CALCULATE-RESERVES.
00378
00379      IF CP-CDT-METHOD EQUAL '4'
00380          ADD +0.5  AGE-INDEX  GIVING  AGE-INDEX2
00381          ADD +0.9167   TERM-INDEX GIVING  TERM-INDEX2
00382      ELSE
00383      IF CP-CDT-INTERPOLATED
00384          GO TO 0310-CALCULATE-RESERVES
00385      ELSE
00386      IF CP-CDT-ROUND-NEAR
00387          ADD +0.5  AGE-INDEX  GIVING  AGE-INDEX2
00388          ADD +0.5  TERM-INDEX GIVING  TERM-INDEX2
00389      ELSE
00390      IF CP-CDT-ROUND-HIGH
00391          ADD +0.9167   AGE-INDEX  GIVING  AGE-INDEX2
00392          ADD +0.9167   TERM-INDEX GIVING  TERM-INDEX2.
00393
00394      IF CP-COMPANY-ID = 'UFL' OR 'UFR' OR 'WFL' OR 'WSL' OR 'CSL'
00395          ADD +0.5  AGE-INDEX  GIVING  AGE-INDEX2
00396          ADD +0.9167   TERM-INDEX GIVING  TERM-INDEX2.
00397
00398      SET TWA-INDEX1  TO  TERM-INDEX2.
00399      SET TWA-INDEX2  TO  AGE-INDEX2.
00400
00401      IF TWA-INDEX1 LESS +1
00402          MOVE ZERO               TO  RESERVE-FACTOR
00403        ELSE
00404          MOVE TWA-FACTOR (TWA-INDEX1, TWA-INDEX2)
00405                                  TO  RESERVE-FACTOR.
00406
00407      GO TO 0400-CALCULATE-FUTURE-RESERVES.
00408
00409      EJECT
00410  0310-CALCULATE-RESERVES.
00411      MOVE AGE-INDEX              TO  AGE-INDEX-LOW
00412                                      AGE-INDEX-HIGH
00413                                      AGE-INDEX-DIFF.
00414
00415      IF WS-DISABLED-AGE LESS +22
00416          MOVE ZERO               TO  AGE-INDEX-LOW  AGE-INDEX-HIGH
00417          DIVIDE WS-DISABLED-AGE BY +22
00418                                  GIVING AGE-INDEX-DIFF ROUNDED.
00419
00420      MOVE TERM-INDEX             TO  TERM-INDEX-LOW
00421                                      TERM-INDEX-HIGH.
00422
00423      IF AGE-INDEX-DIFF GREATER ZERO
00424          ADD +1 TO AGE-INDEX-HIGH.
00425
00426      IF TERM-INDEX-DIFF GREATER ZERO
00427          ADD +1 TO TERM-INDEX-HIGH.
00428
00429      IF TERM-INDEX-LOW LESS +1
00430          MOVE ZERO               TO  FACTOR-A
00431        ELSE
00432      IF AGE-INDEX-LOW LESS +1
00433          COMPUTE FACTOR-A =
00434              TWA-FACTOR (TERM-INDEX-LOW, AGE-INDEX-HIGH) *
00435                                  AGE-INDEX-DIFF
00436        ELSE
00437          COMPUTE FACTOR-A =
00438              ((TWA-FACTOR (TERM-INDEX-LOW, AGE-INDEX-HIGH) -
00439                TWA-FACTOR (TERM-INDEX-LOW, AGE-INDEX-LOW)) *
00440                                                 AGE-INDEX-DIFF) +
00441                TWA-FACTOR (TERM-INDEX-LOW, AGE-INDEX-LOW).
00442
00443      IF TERM-INDEX-HIGH LESS +1
00444          MOVE ZERO               TO  FACTOR-B
00445        ELSE
00446      IF AGE-INDEX-LOW LESS +1
00447          COMPUTE FACTOR-B =
00448              TWA-FACTOR (TERM-INDEX-HIGH, AGE-INDEX-HIGH) *
00449                                  AGE-INDEX-DIFF
00450        ELSE
00451          COMPUTE FACTOR-B =
00452              ((TWA-FACTOR (TERM-INDEX-HIGH, AGE-INDEX-HIGH) -
00453                TWA-FACTOR (TERM-INDEX-HIGH, AGE-INDEX-LOW)) *
00454                                                 AGE-INDEX-DIFF) +
00455                TWA-FACTOR (TERM-INDEX-HIGH, AGE-INDEX-LOW).
00456
00457      COMPUTE RESERVE-FACTOR = ((FACTOR-B - FACTOR-A) *
00458                                TERM-INDEX-DIFF) + FACTOR-A.
00459
00460      EJECT
00461  0400-CALCULATE-FUTURE-RESERVES.
00462
00463 *    NOTE *******************************************************
00464 *         *                                                     *
00465 *         *          CALCULATE THE FUTURE RESERVE.              *
00466 *         *******************************************************.
00467
00468      COMPUTE CP-FUTURE-RESERVE =
00469                      (CP-ORIGINAL-BENEFIT / 100) * RESERVE-FACTOR.
00470
00471      if cp-cdt-percent <> zeros
092920        if ws-months-disabled > +23
092920           move +150.00          to cp-cdt-percent
092920        end-if
00472         COMPUTE CP-FUTURE-RESERVE =
00473            CP-FUTURE-RESERVE * (CP-CDT-PERCENT / +100)
092920     end-if
00474
00475      MOVE WS-DISABILITY-TABLE    TO  CP-CDT-TABLE.
00476      MOVE RESERVE-FACTOR         TO  CP-CDT-FACTOR.
00477
00478      IF CP-COMPANY-ID = 'POS'  AND
00479         CP-CARRIER = ('3'  OR  '5')
00480          NEXT SENTENCE
00481      ELSE
00482          GO TO 0405-CALCULATE-IBNR-RESERVES.
00483
00484      MOVE WS-MONTHS-DISABLED     TO FACTOR-INDEX.
00485      MOVE ZEROS                  TO RESERVE-FACTOR.
00486      MOVE +0.99585               TO FACTOR-V.
00487      PERFORM 0403-CALC-POS-RESERVE-FACTOR THRU 0403-POS-EXIT
00488                        WS-REMAINING-TERM TIMES.
00489
00490      IF WS-MONTHS-DISABLED = ZEROS
00491          MOVE +1.000             TO FACTOR-A
00492      ELSE
00493          IF WS-MONTHS-DISABLED GREATER THAN +40
00494              MOVE +0.011         TO FACTOR-A
00495          ELSE
00496              MOVE POS-FACTOR (WS-MONTHS-DISABLED)  TO FACTOR-A.
00497
00498      COMPUTE CP-FUTURE-RESERVE = ((CP-ORIGINAL-BENEFIT * 2.50) +
00499             ((RESERVE-FACTOR / FACTOR-A) * CP-ORIGINAL-BENEFIT))
00500                                                        * 1.10.
00501
00502      MOVE ZEROS                  TO CP-CDT-TABLE.
00503      COMPUTE CP-CDT-FACTOR = RESERVE-FACTOR / FACTOR-A.
00504
00505      GO TO 0405-CALCULATE-IBNR-RESERVES.
00506
00507  0403-CALC-POS-RESERVE-FACTOR.
00508
00509      ADD +1 TO FACTOR-INDEX.
00510
00511      IF FACTOR-INDEX LESS THAN +41
00512          COMPUTE RESERVE-FACTOR ROUNDED = RESERVE-FACTOR +
00513                         (POS-FACTOR (FACTOR-INDEX) * FACTOR-V)
00514      ELSE
00515          COMPUTE RESERVE-FACTOR ROUNDED = RESERVE-FACTOR +
00516                                           (.011 * FACTOR-V).
00517
00518      COMPUTE FACTOR-V  =  FACTOR-V * .99585.
00519
00520 * NOTE **********************************************************
00521 *      *   FACTOR-V  =  .99585  =  ( 1 / ( 1 + I ))             *
00522 *      *                                 WHERE I = .05 / 12     *
00523 *      **********************************************************.
00524
00525  0403-POS-EXIT.
00526      EXIT.
00527
00528      EJECT
00529  0405-CALCULATE-IBNR-RESERVES.
00530
00531 *    NOTE *******************************************************
00532 *         *                                                     *
00533 *         *          CALCULATE THE I.B.N.R. RESERVE.            *
00534 *         *******************************************************.
00535
00536      MOVE CP-VALUATION-DT        TO  DC-BIN-DATE-1.
00537
00538      IF CP-IBNR-RESERVE-SW = '2'
00539          MOVE -6                 TO  DC-ELAPSED-MONTHS
00540      ELSE
00541          MOVE -3                 TO  DC-ELAPSED-MONTHS.
00542
00543      MOVE ZERO                   TO  DC-ELAPSED-DAYS.
00544      MOVE '6'                    TO  DC-OPTION-CODE.
00545      PERFORM 8500-DATE-CONVERSION.
00546
00547      IF DC-ERROR-CODE NOT = SPACES
00548          MOVE '2'                TO  CP-RETURN-CODE
00549          GO TO 0999-EXIT.
00550
00551      MOVE DC-BIN-DATE-2          TO  WS-VALUATION-DATE-3.
00552
00553      IF CP-INCURRED-DT LESS WS-VALUATION-DATE-3  AND
00554         CP-REPORTED-DT GREATER WS-VALUATION-DATE-3
00555          MOVE CP-FUTURE-RESERVE  TO  CP-IBNR-RESERVE
00556      ELSE
00557          MOVE ZERO               TO  CP-IBNR-RESERVE.
00558
00559      IF CP-CLAIM-STATUS = 'C'
00560          GO TO 0500-PTC-RESERVE.
00561
00562      IF CP-COMPANY-ID = 'POS'
00563        IF CP-CARRIER = '3'  OR  '5'
00564            COMPUTE CP-IBNR-RESERVE = CP-ORIGINAL-BENEFIT * 1.10.
00565
00566      EJECT
00567 *    NOTE *******************************************************
00568 *         *                                                     *
00569 *         *        CALCULATE THE PAY-TO-CURRENT RESERVE.        *
00570 *         *******************************************************
00571
00572  0415-CALCULATE-PTC-RESERVE.
00573
00574 *    IF CP-COMPANY-ID NOT = 'FIM'
00575 *        GO TO 0420-PTC-RESERVE.
00576
00577      GO TO 0420-PTC-RESERVE.
00578
00579      MOVE CP-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00580      MOVE CP-VALUATION-DT        TO  DC-BIN-DATE-2.
00581      MOVE '1'                    TO  DC-OPTION-CODE.
00582      PERFORM 8500-DATE-CONVERSION.
00583
00584      IF DC-ERROR-CODE NOT = SPACE
00585          MOVE '2'                TO  CP-RETURN-CODE
00586          GO TO 0999-EXIT.
00587
00588      MOVE DC-GREG-DATE-A-EDIT    TO  WS-DATE-WORK.
00589      MOVE WS-DW-DAY              TO  WS-WK-DAY.
00590
00591      MOVE DC-GREG-DATE-B-EDIT    TO  WS-DATE-WORK.
00592      MOVE WS-DW-CCYR             TO  WS-WK-CCYR.
00593      MOVE WS-DW-MONTH            TO  WS-WK-MONTH.
00594
00595      MOVE WS-WORK-CYMD           TO  DC-GREG-DATE-CYMD.
00596      MOVE 'L'                    TO  DC-OPTION-CODE.
00597      PERFORM 8500-DATE-CONVERSION.
00598
00599      IF DC-ERROR-CODE NOT = SPACES
00600          MOVE '2'                TO  CP-RETURN-CODE
00601          GO TO 0999-EXIT.
00602
00603      MOVE DC-BIN-DATE-1          TO WS-VALUATION-DATE-2.
00604
00605  0420-PTC-RESERVE.
00606
00607      IF CP-COMPANY-ID = 'POS'
00608        IF CP-CARRIER = ('3'  OR  '5')
00609            COMPUTE CP-PTC-RESERVE = CP-ORIGINAL-BENEFIT * 1.65
00610            GO TO 0500-PTC-RESERVE.
00611
00612      IF CP-PAID-THRU-DT = LOW-VALUES
00613          MOVE CP-INCURRED-DT     TO  DC-BIN-DATE-1
00614      ELSE
00615          MOVE CP-PAID-THRU-DT    TO  DC-BIN-DATE-1.
00616
00617      MOVE CP-VALUATION-DT        TO  DC-BIN-DATE-2.
00618
00619 *    IF CP-COMPANY-ID = 'FIM'
00620 *        IF WS-VALUATION-DATE-2  LESS CP-VALUATION-DT
00621 *            MOVE WS-VALUATION-DATE-2 TO  DC-BIN-DATE-2.
00622 *
032612     if cp-company-id = 'AHL'
032612        if dc-bin-date-1 > dc-bin-date-2
032612           move dc-bin-date-1    to dc-bin-date-2
032612           move cp-valuation-dt  to dc-bin-date-1
032612           move '1'              to dc-option-code
032612           perform 8500-date-conversion
032612           if dc-error-code = spaces
032612              compute cp-ptc-reserve =
032612                 cp-original-benefit * (dc-elapsed-months +
032612                 (dc-odd-days-over / 30))
032612              compute cp-ptc-reserve = cp-ptc-reserve * -1
032612              move zero          to cp-return-code
032612              go to 0500-ptc-reserve
032612           end-if
032612        end-if
032612     end-if
00623      IF DC-BIN-DATE-2 GREATER WS-EXPIRE-DATE
00624         MOVE WS-EXPIRE-DATE      TO  DC-BIN-DATE-2.
00625
00626      IF DC-BIN-DATE-1 NOT LESS DC-BIN-DATE-2
00627          MOVE ZERO               TO  CP-RETURN-CODE
00628          GO TO 0500-PTC-RESERVE.
00629
00630      MOVE '1'                    TO  DC-OPTION-CODE.
00631      PERFORM 8500-DATE-CONVERSION.
00632
00633      IF DC-ERROR-CODE NOT = SPACES
00634          MOVE '2'                TO  CP-RETURN-CODE
00635          GO TO 0999-EXIT.
00636
           IF CP-CRITICAL-PERIOD
              IF DC-ELAPSED-MONTHS > CP-CRITICAL-MONTHS
                 MOVE CP-CRITICAL-MONTHS TO DC-ELAPSED-MONTHS
                 MOVE ZEROS            TO DC-ODD-DAYS-OVER
              ELSE
                 IF CP-CRITICAL-MONTHS = DC-ELAPSED-MONTHS
                    MOVE ZEROS         TO DC-ODD-DAYS-OVER
                 END-IF
              END-IF
           END-IF
00637      COMPUTE CP-PTC-RESERVE =
00638          CP-ORIGINAL-BENEFIT * (DC-ELAPSED-MONTHS +
00639                                    (DC-ODD-DAYS-OVER / 30)).
00640
00641      MOVE ZERO                   TO  CP-RETURN-CODE.
00642
00643  0500-PTC-RESERVE.
00644      IF CP-IBNR-RESERVE-SW NOT = '2'
00645          GO TO 0999-EXIT.
00646
00647      IF CP-INCURRED-DT LESS WS-VALUATION-DATE-3  AND
00648         CP-REPORTED-DT GREATER WS-VALUATION-DATE-3
00649          NEXT SENTENCE
00650      ELSE
00651          GO TO 0999-EXIT.
00652
00653      IF CP-CLAIM-STATUS = 'C'
00654          MOVE CP-TOTAL-PAID      TO  CP-IBNR-RESERVE
00655          GO TO 0999-EXIT.
00656
00657      MOVE CP-INCURRED-DT         TO  DC-BIN-DATE-1.
00658 *    IF CP-COMPANY-ID = 'FIM'
00659 *        MOVE WS-VALUATION-DATE-2 TO  DC-BIN-DATE-2
00660 *    ELSE
00661      MOVE CP-VALUATION-DT     TO  DC-BIN-DATE-2.
00662
00663      MOVE '1'                     TO  DC-OPTION-CODE.
00664
00665      PERFORM 8500-DATE-CONVERSION.
00666
00667      IF DC-ERROR-CODE NOT = SPACES
00668          MOVE '2'                TO  CP-RETURN-CODE
00669          GO TO 0999-EXIT.
00670
00671      COMPUTE CP-IBNR-RESERVE = CP-IBNR-RESERVE +
00672              (CP-ORIGINAL-BENEFIT * (DC-ELAPSED-MONTHS +
00673                                  (DC-ODD-DAYS-OVER / 30))).
00674
00675  0999-EXIT.
032612     if cp-company-id = 'AHL'
032612        continue
032612     else
032612        IF CP-PTC-RESERVE < ZERO
032612           MOVE ZERO             TO CP-PTC-RESERVE
032612        end-if
032612     end-if
00679 *    IF CP-COMPANY-ID = 'FIM'  AND  CP-AH-CLAIM
00680 *        COMPUTE CP-FUTURE-RESERVE =
00681 *                         CP-FUTURE-RESERVE - CP-PTC-RESERVE.
00682 *
00683      IF CP-FUTURE-RESERVE NOT GREATER ZERO
00684          MOVE ZERO               TO  CP-FUTURE-RESERVE.
00685
00686      IF CP-IBNR-RESERVE NOT GREATER ZERO
00687          MOVE ZERO               TO  CP-IBNR-RESERVE.
00061
00062      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.
00063
00064      
      * EXEC CICS RETURN
00065 *        END-EXEC.
      *    MOVE '.(                    ''   #00005097' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00066
00067      EJECT
00068  8500-DATE-CONVERSION SECTION.
00069
00070 *    NOTE *******************************************************
00071 *         *                                                     *
00072 *         *  THIS SECTION CALLS THE DATE CONVERSION SUBROUTINE. *
00073 *         *                                                     *
00074 *         *******************************************************.
00075
00076  8510-DATE-CONVERSION.
00077      
      * EXEC CICS LINK
00078 *        PROGRAM  (WS-DATE-CONVERSION-PROGRAM)
00079 *        COMMAREA (DATE-CONVERSION-DATA)
00080 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
      *    MOVE '."C                   (   #00005110' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DATE-CONVERSION-PROGRAM, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00081
00082      IF DC-ERROR-CODE NOT = SPACES
00083          MOVE '2'                TO  CP-RETURN-CODE
00084          GO TO 0999-EXIT.
00085
00086  8590-EXIT.
00087      EXIT.
00088
00089  9999-LAST-PARAGRAPH SECTION.
00090
00091      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRESV' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRESV' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRESV' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
