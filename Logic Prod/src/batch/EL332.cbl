00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL332
00003  PROGRAM-ID.                 EL332 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL332
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL332
00006 *              CONVERSION DATE 02/15/96 18:42:32.                 EL332
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL332
00008 *                            VMOD=2.003                           EL332
00009                                                                   EL332
00009                                                                   EL332
00010 *AUTHOR.     LOGIC INC.                                           EL332
00011 *            DALLAS, TEXAS.                                       EL332
00012                                                                   EL332
00013 *DATE-COMPILED.                                                   EL332
00014                                                                   EL332
00015 *SECURITY.   *****************************************************EL332
00016 *            *                                                   *EL332
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL332
00018 *            *                                                   *EL332
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL332
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL332
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL332
00022 *            *                                                   *EL332
00023 *            *****************************************************EL332
00024                                                                   EL332
00025 *REMARKS.                                                         EL332
00026 ******************************************************************EL332
00027 *                                                                 EL332
00028 *       THIS PROGRAM PRODUCES THE LOSS CAUSE REPORT, IN TWO       EL332
00029 *       VARIATIONS (OR SECTIONS):                                 EL332
00030 *                                                                 EL332
00031 *       - REPORT 'A' SUMMARIZES LOSS COUNT AND AMOUNT BY EITHER   EL332
00032 *         LIFE OR A&H CATEGORIES, WITHIN TWO LEVELS OF LOSS       EL332
00033 *         CAUSE, AND THEN WITHIN STATE.                           EL332
00034 *                                                                 EL332
00035 *       - REPORT 'B' SUMMARIZES LOSS COUNT AND AMOUNT BY EITHER   EL332
00036 *         LIFE OR A&H CATEGORIES, WITHIN TWO LEVELS OF LOSS       EL332
00037 *         CAUSE.                                                  EL332
00038 *                                                                 EL332
00039 *       THIS PROGRAM CAN BE RUN TO PRODUCE REPORTS EITHER FOR A   EL332
00040 *       SPECIFIED MONTH, OR FOR THE ENTIRE YEAR.                  EL332
00041 *                                                                 EL332
00042 *       IN ORDER TO MAINTAIN SIMPLICITY, A WORK RECORD IS BUILT   EL332
00043 *       FOR THE 'A' REPORT, AND RELEASED TO THE SORT.  HIGH-VALUESEL332
00044 *       ARE THEN MOVED TO THE STATE FIELD, AND A SECOND RECORD IS EL332
00045 *       RELEASED TO THE SORT.  IN THIS WAY, SINCE THE STATE FIELD EL332
00046 *       IS THE MAJOR PORTION OF THE SORT KEY, DATA FOR REPORT 'A' EL332
00047 *       IS PROCESSED FIRST; THEN WHEN THE FIRST RECORD WITH THE   EL332
00048 *       STATE CODE OF HIGH-VALUES IS ENCOUNTERED, THE 'A' REPORT  EL332
00049 *       IS COMPLETED, AND THE 'B' REPORT IS PRODUCED UNTIL THE ENDEL332
00050 *       OF THE SORT FILE.                                         EL332
00051 *                                                                 EL332
00052 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  EL332
00053 *                                                                 EL332
00054 *       THE FILE STATISTICS  AND PROCESSOR STATISTICS:            EL332
00055 *                                                                 EL332
00056 *       INPUTS:    CLAIM DETAIL EXTRACT FILE                      EL332
00057 *                  DATE CARD FILE                                 EL332
00058 *                                                                 EL332
00059 *       OUTPUTS:   LOSS CAUSE REPORT EL332-A                      EL332
00060 *                  LOSS CAUSE REPORT EL332-B                      EL332
00061 *                                                                 EL332
00062 ******************************************************************EL332
00063                                                                   EL332
00064      EJECT                                                        EL332
00065  ENVIRONMENT DIVISION.                                            EL332
00066  CONFIGURATION SECTION.                                           EL332
00067  SPECIAL-NAMES.                                                   EL332
00068      C01 IS TO-TOP-OF-PAGE.                                       EL332
00069                                                                   EL332
00070  INPUT-OUTPUT SECTION.                                            EL332
00071                                                                   EL332
00072  FILE-CONTROL.                                                    EL332
00073                                                                   EL332
00074      SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.    EL332
00075                                                                   EL332
00076      SELECT EXTRACT-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010. EL332
00077                                                                   EL332
00078      SELECT REPORT-FILE        ASSIGN TO SYS008-UR-1403-S-SYS008. EL332
00079                                                                   EL332
00080      SELECT SORT-FILE          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.EL332
00081                                                                   EL332
00082                                                                   EL332
00083  DATA DIVISION.                                                   EL332
00084                                                                   EL332
00085  FILE SECTION.                                                    EL332
00086                                                                   EL332
00087  FD  EXTRACT-FILE                                                 EL332
00088      BLOCK  CONTAINS   0 RECORDS                                  EL332
00089      RECORDING MODE F.                                            EL332
00090  01  EXTRACT-RECORD              PIC X(510).                      EL332
00091                                                                   EL332
00092  FD  REPORT-FILE                 COPY ELCPRTFD.                   EL332
00093                                                                   EL332
00094  FD  DISK-DATE                   COPY ELCDTEFD.                   EL332
00095                                                                   EL332
00096  SD  SORT-FILE.                                                   EL332
00097  01  SORT-RECORD.                                                 EL332
00098      12  SR-KEY.                                                  EL332
00099          16  SR-STATE            PIC X(02).                       EL332
00100          16  SR-LOSS-CAUSE.                                       EL332
00101              20  SR-MAJOR-CAUSE  PIC X(01).                       EL332
00102              20  SR-MINOR-CAUSE  PIC X(02).                       EL332
00103      12  SR-TYPE                 PIC X(01).                       EL332
00104          88  SR-LOSS-LF              VALUES '1' '3'.              EL332
00105          88  SR-LOSS-AH              VALUES '2' '4'.              EL332
00106      12  SR-CLAIM-AMT            PIC S9(9)V99  COMP-3.            EL332
00107                                                                   EL332
00108      EJECT                                                        EL332
00109  WORKING-STORAGE SECTION.                                         EL332
00110  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL332
00111                                                                   EL332
00112  01  FILLER.                                                      EL332
00113      12  FILLER  PIC X(32) VALUE '******************************'.EL332
00114      12  FILLER  PIC X(32) VALUE '    EL332 WORKING-STORAGE     '.EL332
00115      12  FILLER  PIC X(32) VALUE '******** VMOD=2.003 **********'.EL332
00116                                                                   EL332
00117  01  EL332-WORKING-STORAGE.                                       EL332
00118      12  WS-PAGE-COUNT           PIC S9(05)     VALUE +0  COMP-3. EL332
00119      12  WS-LINE-COUNT           PIC S9(03)     VALUE +99 COMP-3. EL332
00120      12  WS-CC                   PIC  9(01)     VALUE 1.          EL332
00121      12  WS-RUN-DT               PIC  X(02)     VALUE SPACES.        CL**2
00122      12  WS-EOF-SW               PIC  X(01)     VALUE 'N'.        EL332
00123          88  WS-EOF-YES                         VALUE 'Y'.        EL332
00124      12  WS-SORT-EOF-SW          PIC  X(01)     VALUE 'N'.        EL332
00125          88  WS-SORT-EOF-YES                    VALUE 'Y'.        EL332
00126      12  WS-RECS-READ            PIC S9(07)     VALUE +0 COMP-3.  EL332
00127      12  WS-CLAIM-RECS           PIC S9(07)     VALUE +0 COMP-3.  EL332
00128      12  WS-YEAR-RECS            PIC S9(07)     VALUE +0 COMP-3.  EL332
00129      12  WS-RECS-SELECTED        PIC S9(07)     VALUE +0 COMP-3.  EL332
00130      12  WS-RECS-RELEASED        PIC S9(07)     VALUE +0 COMP-3.  EL332
00131      12  WS-RECS-RETURNED-A      PIC S9(07)     VALUE +0 COMP-3.  EL332
00132      12  WS-RECS-RETURNED-B      PIC S9(07)     VALUE +0 COMP-3.  EL332
00133                                                                   EL332
00134      12  WS-RETURN-CODE          PIC S9(03)     VALUE ZERO.       EL332
00135      12  WS-ABEND-FILE-STATUS    PIC  X(02)     VALUE ZERO.       EL332
00136      12  WS-ABEND-MESSAGE        PIC  X(80)     VALUE SPACES.     EL332
00137      12  WS-ZERO                 PIC S9(01)     VALUE +0 COMP-3.  EL332
00138      12  PGM-SUB                 PIC S9(4)      VALUE +332  COMP. EL332
00139                                                                   EL332
00140      12  WS-SAVE-CONTROL-FIELDS.                                  EL332
00141          16  WS-SAVE-STATE           PIC  X(02).                  EL332
00142          16  WS-SAVE-CAUSE-CODE.                                  EL332
00143              20  WS-SAVE-MAJOR-CAUSE PIC  X(01).                  EL332
00144              20  WS-SAVE-MINOR-CAUSE PIC  X(02).                  EL332
00145                                                                   EL332
00146      12  WS-GRAND-TOTALS.                                         EL332
00147          16  WS-GRAND-LF-CNT        PIC S9(7)     VALUE +0 COMP-3.EL332
00148          16  WS-GRAND-LF-AMT        PIC S9(11)V99 VALUE +0 COMP-3.EL332
00149          16  WS-GRAND-AH-CNT        PIC S9(7)     VALUE +0 COMP-3.EL332
00150          16  WS-GRAND-AH-AMT        PIC S9(11)V99 VALUE +0 COMP-3.EL332
00151      12  WS-STATE-TOTALS.                                         EL332
00152          16  WS-STATE-LF-CNT        PIC S9(7)     VALUE +0 COMP-3.EL332
00153          16  WS-STATE-LF-AMT        PIC S9(11)V99 VALUE +0 COMP-3.EL332
00154          16  WS-STATE-AH-CNT        PIC S9(7)     VALUE +0 COMP-3.EL332
00155          16  WS-STATE-AH-AMT        PIC S9(11)V99 VALUE +0 COMP-3.EL332
00156      12  WS-MAJOR-CAUSE-TOTALS.                                   EL332
00157          16  WS-MAJOR-CAUSE-LF-CNT  PIC S9(7)     VALUE +0 COMP-3.EL332
00158          16  WS-MAJOR-CAUSE-LF-AMT  PIC S9(11)V99 VALUE +0 COMP-3.EL332
00159          16  WS-MAJOR-CAUSE-AH-CNT  PIC S9(7)     VALUE +0 COMP-3.EL332
00160          16  WS-MAJOR-CAUSE-AH-AMT  PIC S9(11)V99 VALUE +0 COMP-3.EL332
00161      12  WS-MINOR-CAUSE-TOTALS.                                   EL332
00162          16  WS-MINOR-CAUSE-LF-CNT  PIC S9(7)     VALUE +0 COMP-3.EL332
00163          16  WS-MINOR-CAUSE-LF-AMT  PIC S9(11)V99 VALUE +0 COMP-3.EL332
00164          16  WS-MINOR-CAUSE-AH-CNT  PIC S9(7)     VALUE +0 COMP-3.EL332
00165          16  WS-MINOR-CAUSE-AH-AMT  PIC S9(11)V99 VALUE +0 COMP-3.EL332
00166     EJECT                                                         EL332
00167  01  MAJOR-CAUSE-DESCRIPTION-TABLE.                               EL332
00168      12  FILLER    PIC X(27) VALUE 'B-WORK                     '. EL332
00169      12  FILLER    PIC X(27) VALUE 'C-HOME                     '. EL332
00170      12  FILLER    PIC X(27) VALUE 'D-VEHICLE / VEHICLE        '. EL332
00171      12  FILLER    PIC X(27) VALUE 'E-SINGLE VEHICLE           '. EL332
00172      12  FILLER    PIC X(27) VALUE 'F-SUICIDE / SELF-INFLICTED '. EL332
00173      12  FILLER    PIC X(27) VALUE 'G-CIVIL DISORDER           '. EL332
00174      12  FILLER    PIC X(27) VALUE 'H-NATURAL CATASTROPHE      '. EL332
00175      12  FILLER    PIC X(27) VALUE 'I-HOBBY / SPORTS           '. EL332
00176      12  FILLER    PIC X(27) VALUE 'J-OTHER                    '. EL332
00177      12  FILLER    PIC X(27) VALUE 'K-MILITARY                 '. EL332
00178      12  FILLER    PIC X(27) VALUE 'M-AIDS                     '. EL332
00179      12  FILLER    PIC X(27) VALUE 'U-UNEMPLOYMENT             '. EL332
00180      12  FILLER    PIC X(27) VALUE 'Z-MATERNITY                '. EL332
00181  01  FILLER  REDEFINES  MAJOR-CAUSE-DESCRIPTION-TABLE.            EL332
00182      12  MAJOR-CAUSE-ENTRIES     OCCURS 13 TIMES                  EL332
00183                                  ASCENDING KEY MAJ-CODE           EL332
00184                                  INDEXED BY SUB-MAJOR.            EL332
00185          16  MAJ-CODE            PIC X(01).                       EL332
00186          16  FILLER              PIC X(01).                       EL332
00187          16  MAJ-DESCRIPTION     PIC X(25).                       EL332
00188                                                                   EL332
00189  01  MINOR-CAUSE-DESCRIPTION-TABLE.                               EL332
00190      12  FILLER    PIC X(28) VALUE '01-INFLUENZA/UPPER RESPIR.  '.EL332
00191      12  FILLER    PIC X(28) VALUE '02-RHEUMATISM               '.EL332
00192      12  FILLER    PIC X(28) VALUE '03-ANEMIA                   '.EL332
00193      12  FILLER    PIC X(28) VALUE '04-DIABETES                 '.EL332
00194      12  FILLER    PIC X(28) VALUE '05-CANCER                   '.EL332
00195      12  FILLER    PIC X(28) VALUE '06-VENEREAL DISEASE         '.EL332
00196      12  FILLER    PIC X(28) VALUE '07-TUBERCULOSIS             '.EL332
00197      12  FILLER    PIC X(28) VALUE '08-ARTHRITIS                '.EL332
00198      12  FILLER    PIC X(28) VALUE '09-BURSITIS                 '.EL332
00199      12  FILLER    PIC X(28) VALUE '10-CYSTS,LESIONS,WARTS,ETC. '.EL332
00200      12  FILLER    PIC X(28) VALUE '11-OTHER GENERAL DISEASES   '.EL332
00201      12  FILLER    PIC X(28) VALUE '13-NEURITIS,HERPES ZOSTER...'.EL332
00202      12  FILLER    PIC X(28) VALUE '14-NERVOUS/MENTAL DISORDERS '.EL332
00203      12  FILLER    PIC X(28) VALUE '15-HEART/CORONARY DISEASE   '.EL332
00204      12  FILLER    PIC X(28) VALUE '16-HYPER-/HYPOTENSION       '.EL332
00205      12  FILLER    PIC X(28) VALUE '17-CVA,STROKE,APOPLEXY      '.EL332
00206      12  FILLER    PIC X(28) VALUE '18-OTHER CIRCULARY DISEASE  '.EL332
00207      12  FILLER    PIC X(28) VALUE '19-LEUKEMIA                 '.EL332
00208      12  FILLER    PIC X(28) VALUE '20-THYROID                  '.EL332
00209      12  FILLER    PIC X(28) VALUE '21-VARICOSE VEINS, PHLEBITIS'.EL332
00210      12  FILLER    PIC X(28) VALUE '23-BRONCHITIS               '.EL332
00211      12  FILLER    PIC X(28) VALUE '24-PNEUMONIA                '.EL332
00212      12  FILLER    PIC X(28) VALUE '25-ASTHMA,ALLERGY,HAY FEVER '.EL332
00213      12  FILLER    PIC X(28) VALUE '26-BLACK LUNG (SILICOSIS)   '.EL332
00214      12  FILLER    PIC X(28) VALUE '27-PNEUMONOSIS              '.EL332
00215      12  FILLER    PIC X(28) VALUE '28-OTHER RESPIRATORY        '.EL332
00216      12  FILLER    PIC X(28) VALUE '30-APPENDICITIS             '.EL332
00217      12  FILLER    PIC X(28) VALUE '31-HEMMORHOIDS, RECTAL      '.EL332
00218      12  FILLER    PIC X(28) VALUE '32-GASTRITIS, INTESTINAL    '.EL332
00219      12  FILLER    PIC X(28) VALUE '33-GALL BLADDER, PANCREAS   '.EL332
00220      12  FILLER    PIC X(28) VALUE '34-LIVER                    '.EL332
00221      12  FILLER    PIC X(28) VALUE '35-HERNIA                   '.EL332
00222      12  FILLER    PIC X(28) VALUE '36-ULCERS                   '.EL332
00223      12  FILLER    PIC X(28) VALUE '37-OTHER DIGESTIVE DISORDER '.EL332
00224      12  FILLER    PIC X(28) VALUE '39-KIDNEY,URETER,BLADDER... '.EL332
00225      12  FILLER    PIC X(28) VALUE '40-PROSTATE,EPIDIDYMITIS... '.EL332
00226      12  FILLER    PIC X(28) VALUE '41-OTHER GENITO/URINARY     '.EL332
00227      12  FILLER    PIC X(28) VALUE '42-SKIN DISEASE/DISORDERS   '.EL332
00228      12  FILLER    PIC X(28) VALUE '44-CATARACT                 '.EL332
00229      12  FILLER    PIC X(28) VALUE '45-OTHER EYE DISEASES       '.EL332
00230      12  FILLER    PIC X(28) VALUE '47-EAR DISEASE/DISORDERS    '.EL332
00231      12  FILLER    PIC X(28) VALUE '49-TONSILITIS/THROAT DISORDR'.EL332
00232      12  FILLER    PIC X(28) VALUE '51-NOSE DISEASE/DISORDERS   '.EL332
00233      12  FILLER    PIC X(28) VALUE '53-FEMALE DISEASE/DISORDERS '.EL332
00234      12  FILLER    PIC X(28) VALUE '55-CHILDBIRTH               '.EL332
00235      12  FILLER    PIC X(28) VALUE '56-ABORTION/MISCARRIAGE...  '.EL332
00236      12  FILLER    PIC X(28) VALUE '57-SPINAL DISC DISORDERS    '.EL332
00237      12  FILLER    PIC X(28) VALUE '58-SPINAL STRAINS/SPRAINS   '.EL332
00238      12  FILLER    PIC X(28) VALUE '60-SKULL/SPINE/TRUNK FRACTUR'.EL332
00239      12  FILLER    PIC X(28) VALUE '61-RIB CAGE FRACTURES       '.EL332
00240      12  FILLER    PIC X(28) VALUE '62-UPPER LIMB FRACTURES     '.EL332
00241      12  FILLER    PIC X(28) VALUE '63-LOWER LIMB FRACTURES     '.EL332
00242      12  FILLER    PIC X(28) VALUE '64-OTHER BONE FRACTURES     '.EL332
00243      12  FILLER    PIC X(28) VALUE '65-DISLOCATION (NO FRACTURE)'.EL332
00244      12  FILLER    PIC X(28) VALUE '66-UPPER EXTR STRAINS...    '.EL332
00245      12  FILLER    PIC X(28) VALUE '67-LOWER EXTR STRAINS...    '.EL332
00246      12  FILLER    PIC X(28) VALUE '68-OTHER SPRAINS/STRAINS    '.EL332
00247      12  FILLER    PIC X(28) VALUE '69-INTRACRANIAL INJURIES    '.EL332
00248      12  FILLER    PIC X(28) VALUE '70-INTERNAL INJURIES        '.EL332
00249      12  FILLER    PIC X(28) VALUE '71-LACERATIONS - HEAD/NECK  '.EL332
00250      12  FILLER    PIC X(28) VALUE '72-LACERATIONS - UPPER LIMBS'.EL332
00251      12  FILLER    PIC X(28) VALUE '73-LACERATIONS - LOWER LIMBS'.EL332
00252      12  FILLER    PIC X(28) VALUE '74-LACERATIONS - MULTIPLE   '.EL332
00253      12  FILLER    PIC X(28) VALUE '75-BURNS - HEAD/NECK/TRUNK  '.EL332
00254      12  FILLER    PIC X(28) VALUE '76-BURNS - UPPER LIMBS      '.EL332
00255      12  FILLER    PIC X(28) VALUE '77-BURNS - LOWER LIMBS      '.EL332
00256      12  FILLER    PIC X(28) VALUE '78-BURNS - MULTIPLE         '.EL332
00257      12  FILLER    PIC X(28) VALUE '79-UNSPECIFIED INJURIES     '.EL332
00258      12  FILLER    PIC X(28) VALUE '80-DENTAL TREATMENT         '.EL332
00259      12  FILLER    PIC X(28) VALUE '81-ENDOCRINE DISEASE/DISORDR'.EL332
00260      12  FILLER    PIC X(28) VALUE '82-BONE/JOINT DISEASE       '.EL332
00261      12  FILLER    PIC X(28) VALUE '83-AMPUTATION (UPPER EXTREM)'.EL332
00262      12  FILLER    PIC X(28) VALUE '84-AMPUTATION (LOWER EXTREM)'.EL332
00263      12  FILLER    PIC X(28) VALUE '85-LOSS OF EYE(S)           '.EL332
00264      12  FILLER    PIC X(28) VALUE '86-MUSCLE DISEASE/DISORDER  '.EL332
00265      12  FILLER    PIC X(28) VALUE '99-NOT INDEXED              '.EL332
00266  01  FILLER  REDEFINES  MINOR-CAUSE-DESCRIPTION-TABLE.            EL332
00267      12  MINOR-CAUSE-ENTRIES     OCCURS 76 TIMES                  EL332
00268                                  ASCENDING KEY MIN-CODE           EL332
00269                                  INDEXED BY SUB-MINOR.            EL332
00270          16  MIN-CODE            PIC X(02).                       EL332
00271          16  FILLER              PIC X(01).                       EL332
00272          16  MIN-DESCRIPTION     PIC X(25).                       EL332
00273                                                                   EL332
00274     EJECT                                                         EL332
00275                                  COPY ECSEXT01.                   EL332
00276                                  COPY ELCEXTVR.                      CL**5
00277     EJECT                                                         EL332
00278  01  REPORT-LINES.                                                EL332
00279      12  WS-HEADING-1.                                            EL332
00280          16  FILLER              PIC X(53)      VALUE SPACES.     EL332
00281          16  WS-H1-RUN-TYPE      PIC X(07).                       EL332
00282          16  FILLER              PIC X(26)      VALUE             EL332
00283              ' LOSS CAUSE REPORT'.                                EL332
00284          16  FILLER              PIC X(39)      VALUE SPACES.     EL332
00285          16  WS-H1-REPORT-ID     PIC X(08)      VALUE 'EL332-A'.  EL332
00286                                                                   EL332
00287      12  WS-HEADING-2.                                            EL332
00288          16  FILLER              PIC X(54)      VALUE SPACES.     EL332
00289          16  WS-H2-COMPANY-NAME  PIC X(30).                       EL332
00290          16  FILLER              PIC X(41)      VALUE SPACES.     EL332
00291          16  WS-H2-RUN-DATE.                                      EL332
00292              20  WS-H2-RUN-MM    PIC X(02).                       EL332
00293              20  FILLER          PIC X(01)      VALUE '/'.        EL332
00294              20  WS-H2-RUN-DD    PIC X(02).                       EL332
00295              20  FILLER          PIC X(01)      VALUE '/'.        EL332
00296              20  WS-H2-RUN-YY    PIC X(02).                       EL332
00297                                                                   EL332
00298      12  WS-HEADING-3.                                            EL332
00299          16  FILLER              PIC X(57)      VALUE SPACES.     EL332
00300          16  WS-H3-REPORT-DATE   PIC X(18).                       EL332
00301          16  FILLER              PIC X(47)      VALUE SPACES.     EL332
00302          16  FILLER              PIC X(05)      VALUE 'PAGE '.    EL332
00303          16  WS-H3-PAGE          PIC ZZ,ZZ9.                      EL332
00304                                                                   EL332
00305      12  WS-HEADING-4.                                            EL332
00306          16  FILLER              PIC X(50)      VALUE             EL332
00307              '                                                  '.EL332
00308          16  FILLER              PIC X(50)      VALUE             EL332
00309              '       *----------- LIFE -----------*     *-------'.EL332
00310          16  FILLER              PIC X(33)      VALUE             EL332
00311              '---- A&H -----------*            '.                 EL332
00312                                                                   EL332
00313      12  WS-HEADING-5.                                            EL332
00314          16  WS-H5-STATE         PIC X(21)      VALUE             EL332
00315              '              STATE  '.                             EL332
00316          16  FILLER              PIC X(29)      VALUE             EL332
00317              'CODE   DESCRIPTION           '.                     EL332
00318          16  FILLER              PIC X(50)      VALUE             EL332
00319              '           COUNT            AMOUNT           COUNT'.EL332
00320          16  FILLER              PIC X(33)      VALUE             EL332
00321              '            AMOUNT               '.                 EL332
00322                                                                   EL332
00323      12  WS-DETAIL1.                                              EL332
00324          16  FILLER              PIC X(11)      VALUE  SPACES.    EL332
00325          16  WS-D1-LITERAL.                                       EL332
00326              20  FILLER          PIC X(04)      VALUE  SPACES.    EL332
00327              20  WS-D1-STATE     PIC X(02).                       EL332
00328          16  FILLER              PIC X(04)      VALUE  SPACES.    EL332
00329          16  WS-D1-CAUSE-CODE    PIC X(03).                       EL332
00330          16  FILLER              PIC X(04)      VALUE  SPACES.    EL332
00331          16  WS-D1-DESCRIPTION   PIC X(25).                       EL332
00332          16  FILLER              PIC X(04)      VALUE  SPACES.    EL332
00333          16  WS-D1-LF-CNT        PIC Z,ZZZ,ZZ9.                   EL332
00334          16  WS-D1-FLAG1         PIC X(04).                       EL332
00335          16  WS-D1-LF-AMT        PIC ZZ,ZZZ,ZZZ,ZZ9.99-.          EL332
00336          16  WS-D1-FLAG2         PIC X(04).                       EL332
00337          16  WS-D1-AH-CNT        PIC Z,ZZZ,ZZ9.                   EL332
00338          16  WS-D1-FLAG3         PIC X(04).                       EL332
00339          16  WS-D1-AH-AMT        PIC ZZ,ZZZ,ZZZ,ZZ9.99-.          EL332
00340          16  WS-D1-FLAG4         PIC X(01).                       EL332
00341          16  FILLER              PIC X(09)      VALUE  SPACES.    EL332
00342                                                                   EL332
00343      12  WS-TOTAL-LINE1.                                          EL332
00344          16  FILLER              PIC X(01)      VALUE  SPACE.     EL332
00345          16  WS-T1-DESCRIPTION   PIC X(15).                       EL332
00346          16  FILLER              PIC X(41)      VALUE  SPACES.    EL332
00347          16  WS-T1-LF-CNT        PIC Z,ZZZ,ZZ9.                   EL332
00348          16  FILLER              PIC X(04)      VALUE  ' ** '.    EL332
00349          16  WS-T1-LF-AMT        PIC ZZ,ZZZ,ZZZ,ZZ9.99-.          EL332
00350          16  FILLER              PIC X(04)      VALUE  '**  '.    EL332
00351          16  WS-T1-AH-CNT        PIC Z,ZZZ,ZZ9.                   EL332
00352          16  FILLER              PIC X(04)      VALUE  ' ** '.    EL332
00353          05  WS-T1-AH-AMT        PIC ZZ,ZZZ,ZZZ,ZZ9.99-.          EL332
00354          05  FILLER              PIC X(10)      VALUE  '**  '.    EL332
00355                                                                   EL332
00356      EJECT                                                        EL332
00357                                  COPY ELCDATE.                       CL**6
00358                                  COPY ELCDTECX.                      CL**3
00359                                  COPY ELCDTEVR.                      CL**4
00360      EJECT                                                        EL332
00361  PROCEDURE DIVISION.                                              EL332
00362                                                                   EL332
00363  0000-LOAD-DATE-CARD.            COPY ELCDTERX.                      CL**3
00364                                                                   EL332
00365      IF DTE-PGM-OPT = '1'                                         EL332
00366          MOVE 'MONTHLY'          TO WS-H1-RUN-TYPE                EL332
00367        ELSE                                                       EL332
00368      IF DTE-PGM-OPT = '2'                                         EL332
00369          MOVE ' ANNUAL'          TO WS-H1-RUN-TYPE                EL332
00370        ELSE                                                       EL332
00371          MOVE 'MONTHLY'          TO WS-H1-RUN-TYPE                EL332
00372          MOVE '1'                TO DTE-PGM-OPT.                  EL332
00373                                                                   EL332
00374      SORT SORT-FILE                                               EL332
00375          ON ASCENDING KEY SR-KEY                                  EL332
00376              INPUT  PROCEDURE IS 0100-INPUT-PROCEDURE             EL332
00377              OUTPUT PROCEDURE IS 1000-OUTPUT-PROCEDURE.           EL332
00378                                                                   EL332
00379          DISPLAY WS-RECS-READ       ' RECORDS READ'.              EL332
00380          DISPLAY WS-CLAIM-RECS      '   CLAIM RECORDS'.           EL332
00381          DISPLAY WS-YEAR-RECS       '     FOR CURRENT YEAR'.      EL332
00382          DISPLAY WS-RECS-SELECTED   '     SELECTED'.              EL332
00383          DISPLAY SPACE.                                           EL332
00384          DISPLAY WS-RECS-RELEASED   ' SORT RECORDS'.              EL332
00385          DISPLAY WS-RECS-RETURNED-A '   A RECORDS'.               EL332
00386          DISPLAY WS-RECS-RETURNED-B '   B RECORDS'.               EL332
00387                                                                   EL332
00388      GOBACK.                                                      EL332
00389                                                                   EL332
00390  0100-INPUT-PROCEDURE            SECTION.                         EL332
00391                                                                   EL332
00392      OPEN INPUT EXTRACT-FILE.                                     EL332
00393                                                                   EL332
00394      READ EXTRACT-FILE INTO DETAIL-EXTRACT                        EL332
00395          AT END MOVE 'Y' TO WS-EOF-SW.                            EL332
00396                                                                   EL332
00397      PERFORM 0200-INPUT-LOOP                                      EL332
00398          UNTIL WS-EOF-YES.                                        EL332
00399                                                                   EL332
00400      CLOSE EXTRACT-FILE.                                          EL332
00401                                                                   EL332
00402  0200-INPUT-LOOP                 SECTION.                         EL332
00403                                                                   EL332
00404      ADD  +1                     TO WS-RECS-READ.                 EL332
00405                                                                   EL332
00406 *** SELECT ONLY CLAIM RECORDS                                     EL332
00407      IF DE-CLAIM                                                  EL332
00408          ADD  +1                 TO WS-CLAIM-RECS                 EL332
00409        ELSE                                                       EL332
00410          GO TO 0290-READ-NEXT.                                    EL332
00411                                                                   EL332
00412     COPY ELCEXTM1.                                                   CL**5
00413                                                                      CL**5
00414 *** SELECT RECORDS FOR CURRENT YEAR                               EL332
00415      IF DE-CP-YR = RUN-YR                                         EL332
00416          ADD  +1                 TO WS-YEAR-RECS                  EL332
00417        ELSE                                                       EL332
00418          GO TO 0290-READ-NEXT.                                    EL332
00419                                                                   EL332
00420 *** MONTHLY REPORT OPTION CODE = '1' ***                          EL332
00421 *** ANNUAL  REPORT OPTION CODE = '2' ***                          EL332
00422                                                                   EL332
00423      IF DTE-PGM-OPT = '2'                                         EL332
00424          NEXT SENTENCE                                            EL332
00425        ELSE                                                       EL332
00426 ***  IF DTE-PGM-OPT = '1'                                         EL332
00427         IF DE-CP-MO = RUN-MO                                      EL332
00428             NEXT SENTENCE                                         EL332
00429         ELSE                                                      EL332
00430             GO TO 0290-READ-NEXT.                                 EL332
00431                                                                   EL332
00432      ADD  +1                     TO WS-RECS-SELECTED.             EL332
00433                                                                   EL332
00434      MOVE DE-STATE               TO SR-STATE.                     EL332
00435      MOVE DE-LOSS-CODE           TO SR-LOSS-CAUSE.                EL332
00436      MOVE DE-TYPE                TO SR-TYPE.                      EL332
00437      IF DE-CLAIM-AMT NUMERIC                                      EL332
00438          MOVE DE-CLAIM-AMT       TO SR-CLAIM-AMT                  EL332
00439        ELSE                                                       EL332
00440          MOVE ZERO               TO SR-CLAIM-AMT.                 EL332
00441                                                                   EL332
00442      RELEASE SORT-RECORD.                                         EL332
00443                                                                   EL332
00444      ADD  +1                     TO WS-RECS-RELEASED.             EL332
00445                                                                   EL332
00446      MOVE HIGH-VALUES            TO SR-STATE.                     EL332
00447                                                                   EL332
00448      RELEASE SORT-RECORD.                                         EL332
00449                                                                   EL332
00450      ADD  +1                     TO WS-RECS-RELEASED.             EL332
00451                                                                   EL332
00452  0290-READ-NEXT.                                                  EL332
00453                                                                   EL332
00454      READ EXTRACT-FILE INTO DETAIL-EXTRACT                        EL332
00455          AT END MOVE 'Y' TO WS-EOF-SW.                            EL332
00456                                                                   EL332
00457      EJECT                                                        EL332
00458  1000-OUTPUT-PROCEDURE            SECTION.                        EL332
00459                                                                   EL332
00460      OPEN OUTPUT REPORT-FILE.                                     EL332
00461                                                                   EL332
00462      MOVE COMPANY-NAME           TO WS-H2-COMPANY-NAME.           EL332
00463      MOVE RUN-MO                 TO WS-H2-RUN-MM.                 EL332
00464      MOVE RUN-DA                 TO WS-H2-RUN-DD.                 EL332
00465      MOVE RUN-YR                 TO WS-H2-RUN-YY.                 EL332
00466      MOVE ALPH-DATE              TO WS-H3-REPORT-DATE.            EL332
00467                                                                   EL332
00468      RETURN SORT-FILE                                             EL332
00469          AT END MOVE 'Y' TO WS-SORT-EOF-SW.                       EL332
00470                                                                   EL332
00471      PERFORM 1100-REPORT-A-LOOP                                   EL332
00472          UNTIL SR-STATE = HIGH-VALUES OR                          EL332
00473                WS-SORT-EOF-YES.                                   EL332
00474                                                                   EL332
00475 ***** PRINT GRAND TOTALS *****                                    EL332
00476                                                                   EL332
00477      MOVE 'GRAND TOTALS:'           TO WS-T1-DESCRIPTION.         EL332
00478      MOVE WS-GRAND-LF-CNT           TO WS-T1-LF-CNT.              EL332
00479      MOVE WS-GRAND-LF-AMT           TO WS-T1-LF-AMT.              EL332
00480      MOVE WS-GRAND-AH-CNT           TO WS-T1-AH-CNT.              EL332
00481      MOVE WS-GRAND-AH-AMT           TO WS-T1-AH-AMT.              EL332
00482                                                                   EL332
00483      IF WS-LINE-COUNT > +54                                       EL332
00484          PERFORM 3000-PRINT-HEADINGS.                             EL332
00485                                                                   EL332
00486      WRITE PRT FROM WS-TOTAL-LINE1                                EL332
00487          AFTER ADVANCING 2 LINES.                                 EL332
00488                                                                   EL332
00489 *****************************                                     EL332
00490 ***** END OF REPORT 'A' *****                                     EL332
00491 *****************************                                     EL332
00492                                                                   EL332
00493 ****************************                                      EL332
00494 ***** BEGIN REPORT 'B' *****                                      EL332
00495 ****************************                                      EL332
00496                                                                   EL332
00497      MOVE 'EL332-B'               TO WS-H1-REPORT-ID.             EL332
00498      MOVE SPACES                  TO WS-H5-STATE.                 EL332
00499      MOVE +99                     TO WS-LINE-COUNT.               EL332
00500      MOVE ZERO                    TO WS-PAGE-COUNT.               EL332
00501      MOVE ZERO                    TO WS-GRAND-LF-CNT,             EL332
00502                                      WS-GRAND-LF-AMT,             EL332
00503                                      WS-GRAND-AH-CNT,             EL332
00504                                      WS-GRAND-AH-AMT.             EL332
00505                                                                   EL332
00506      PERFORM 2100-REPORT-B-LOOP                                   EL332
00507          UNTIL WS-SORT-EOF-YES.                                   EL332
00508                                                                   EL332
00509 ***** PRINT GRAND TOTALS *****                                    EL332
00510                                                                   EL332
00511      MOVE 'GRAND TOTALS:'           TO WS-T1-DESCRIPTION.         EL332
00512      MOVE WS-GRAND-LF-CNT           TO WS-T1-LF-CNT.              EL332
00513      MOVE WS-GRAND-LF-AMT           TO WS-T1-LF-AMT.              EL332
00514      MOVE WS-GRAND-AH-CNT           TO WS-T1-AH-CNT.              EL332
00515      MOVE WS-GRAND-AH-AMT           TO WS-T1-AH-AMT.              EL332
00516                                                                   EL332
00517      IF WS-LINE-COUNT > +54                                       EL332
00518          PERFORM 3000-PRINT-HEADINGS.                             EL332
00519                                                                   EL332
00520      WRITE PRT FROM WS-TOTAL-LINE1                                EL332
00521          AFTER ADVANCING 2 LINES.                                 EL332
00522                                                                   EL332
00523 *****************************                                     EL332
00524 ***** END OF REPORT 'B' *****                                     EL332
00525 *****************************                                     EL332
00526                                                                   EL332
00527      CLOSE REPORT-FILE.                                           EL332
00528                                                                   EL332
00529      EJECT                                                        EL332
00530  1100-REPORT-A-LOOP               SECTION.                        EL332
00531                                                                   EL332
00532      MOVE SR-STATE                TO WS-SAVE-STATE.               EL332
00533      MOVE ZERO                    TO WS-STATE-LF-CNT,             EL332
00534                                      WS-STATE-LF-AMT,             EL332
00535                                      WS-STATE-AH-CNT,             EL332
00536                                      WS-STATE-AH-AMT.             EL332
00537                                                                   EL332
00538      PERFORM 1200-REPORT-A-STATE-LOOP                             EL332
00539          UNTIL SR-STATE = HIGH-VALUES        OR                   EL332
00540                SR-STATE NOT = WS-SAVE-STATE  OR                   EL332
00541                WS-SORT-EOF-YES.                                   EL332
00542                                                                   EL332
00543 ***** PRINT STATE TOTALS *****                                    EL332
00544                                                                   EL332
00545      MOVE 'STATE TOTALS:'           TO WS-T1-DESCRIPTION.         EL332
00546      MOVE WS-STATE-LF-CNT           TO WS-T1-LF-CNT.              EL332
00547      MOVE WS-STATE-LF-AMT           TO WS-T1-LF-AMT.              EL332
00548      MOVE WS-STATE-AH-CNT           TO WS-T1-AH-CNT.              EL332
00549      MOVE WS-STATE-AH-AMT           TO WS-T1-AH-AMT.              EL332
00550                                                                   EL332
00551      IF WS-LINE-COUNT > +54                                       EL332
00552          PERFORM 3000-PRINT-HEADINGS.                             EL332
00553                                                                   EL332
00554      WRITE PRT FROM WS-TOTAL-LINE1                                EL332
00555          AFTER ADVANCING 2 LINES.                                 EL332
00556                                                                   EL332
00557      ADD +2                        TO WS-LINE-COUNT.              EL332
00558      ADD WS-STATE-LF-CNT           TO WS-GRAND-LF-CNT.            EL332
00559      ADD WS-STATE-LF-AMT           TO WS-GRAND-LF-AMT.            EL332
00560      ADD WS-STATE-AH-CNT           TO WS-GRAND-AH-CNT.            EL332
00561      ADD WS-STATE-AH-AMT           TO WS-GRAND-AH-AMT.            EL332
00562                                                                   EL332
00563      EJECT                                                        EL332
00564  1200-REPORT-A-STATE-LOOP         SECTION.                        EL332
00565                                                                   EL332
00566      MOVE SR-MAJOR-CAUSE          TO WS-SAVE-MAJOR-CAUSE.         EL332
00567      MOVE ZERO                    TO WS-MAJOR-CAUSE-LF-CNT,       EL332
00568                                      WS-MAJOR-CAUSE-LF-AMT,       EL332
00569                                      WS-MAJOR-CAUSE-AH-CNT,       EL332
00570                                      WS-MAJOR-CAUSE-AH-AMT.       EL332
00571      MOVE SPACES                  TO WS-D1-LITERAL,               EL332
00572                                      WS-D1-FLAG1,                 EL332
00573                                      WS-D1-FLAG2,                 EL332
00574                                      WS-D1-FLAG3,                 EL332
00575                                      WS-D1-FLAG4.                 EL332
00576      MOVE 2                       TO WS-CC.                       EL332
00577                                                                   EL332
00578      PERFORM 1300-REPORT-A-MAJOR-CAUSE-LOOP                       EL332
00579          UNTIL SR-STATE           = HIGH-VALUES          OR       EL332
00580                SR-MAJOR-CAUSE NOT = WS-SAVE-MAJOR-CAUSE  OR       EL332
00581                SR-STATE       NOT = WS-SAVE-STATE        OR       EL332
00582                WS-SORT-EOF-YES.                                   EL332
00583                                                                   EL332
00584 ***** PRINT MAJOR CAUSE TOTALS *****                              EL332
00585                                                                   EL332
00586      MOVE 'TOTAL:'                  TO WS-D1-LITERAL.             EL332
00587      MOVE ' *'                      TO WS-D1-FLAG1,               EL332
00588                                        WS-D1-FLAG3,               EL332
00589      MOVE '*'                       TO WS-D1-FLAG2,               EL332
00590                                        WS-D1-FLAG4.               EL332
00591      MOVE WS-SAVE-MAJOR-CAUSE       TO WS-D1-CAUSE-CODE.          EL332
00592      MOVE WS-MAJOR-CAUSE-LF-CNT     TO WS-D1-LF-CNT.              EL332
00593      MOVE WS-MAJOR-CAUSE-LF-AMT     TO WS-D1-LF-AMT.              EL332
00594      MOVE WS-MAJOR-CAUSE-AH-CNT     TO WS-D1-AH-CNT.              EL332
00595      MOVE WS-MAJOR-CAUSE-AH-AMT     TO WS-D1-AH-AMT.              EL332
00596                                                                   EL332
00597      SET SUB-MAJOR TO +1.                                         EL332
00598      SEARCH ALL MAJOR-CAUSE-ENTRIES                               EL332
00599        AT END MOVE 'UNDEFINED MAJOR CAUSE' TO WS-D1-DESCRIPTION   EL332
00600          WHEN MAJ-CODE ( SUB-MAJOR ) = WS-SAVE-MAJOR-CAUSE        EL332
00601            MOVE MAJ-DESCRIPTION (SUB-MAJOR) TO WS-D1-DESCRIPTION. EL332
00602                                                                   EL332
00603      IF WS-LINE-COUNT > +54                                       EL332
00604          PERFORM 3000-PRINT-HEADINGS.                             EL332
00605                                                                   EL332
00606      WRITE PRT FROM WS-DETAIL1                                    EL332
00607          AFTER ADVANCING 2 LINES.                                 EL332
00608                                                                   EL332
00609      ADD +2                        TO WS-LINE-COUNT.              EL332
00610      ADD WS-MAJOR-CAUSE-LF-CNT     TO WS-STATE-LF-CNT.            EL332
00611      ADD WS-MAJOR-CAUSE-LF-AMT     TO WS-STATE-LF-AMT.            EL332
00612      ADD WS-MAJOR-CAUSE-AH-CNT     TO WS-STATE-AH-CNT.            EL332
00613      ADD WS-MAJOR-CAUSE-AH-AMT     TO WS-STATE-AH-AMT.            EL332
00614                                                                   EL332
00615      EJECT                                                        EL332
00616  1300-REPORT-A-MAJOR-CAUSE-LOOP   SECTION.                        EL332
00617                                                                   EL332
00618      MOVE SR-MINOR-CAUSE          TO WS-SAVE-MINOR-CAUSE.         EL332
00619      MOVE ZERO                    TO WS-MINOR-CAUSE-LF-CNT,       EL332
00620                                      WS-MINOR-CAUSE-LF-AMT,       EL332
00621                                      WS-MINOR-CAUSE-AH-CNT,       EL332
00622                                      WS-MINOR-CAUSE-AH-AMT.       EL332
00623                                                                   EL332
00624      PERFORM 1400-REPORT-A-MINOR-CAUSE-LOOP                       EL332
00625          UNTIL SR-STATE           = HIGH-VALUES          OR       EL332
00626                SR-MINOR-CAUSE NOT = WS-SAVE-MINOR-CAUSE  OR       EL332
00627                SR-MAJOR-CAUSE NOT = WS-SAVE-MAJOR-CAUSE  OR       EL332
00628                SR-STATE       NOT = WS-SAVE-STATE        OR       EL332
00629                WS-SORT-EOF-YES.                                   EL332
00630                                                                   EL332
00631 ***** PRINT MINOR CAUSE TOTALS *****                              EL332
00632                                                                   EL332
00633      MOVE WS-SAVE-STATE             TO WS-D1-STATE.               EL332
00634      MOVE WS-SAVE-CAUSE-CODE        TO WS-D1-CAUSE-CODE.          EL332
00635      MOVE WS-MINOR-CAUSE-LF-CNT     TO WS-D1-LF-CNT.              EL332
00636      MOVE WS-MINOR-CAUSE-LF-AMT     TO WS-D1-LF-AMT.              EL332
00637      MOVE WS-MINOR-CAUSE-AH-CNT     TO WS-D1-AH-CNT.              EL332
00638      MOVE WS-MINOR-CAUSE-AH-AMT     TO WS-D1-AH-AMT.              EL332
00639                                                                   EL332
00640      SET SUB-MINOR TO +1.                                         EL332
00641      SEARCH ALL MINOR-CAUSE-ENTRIES                               EL332
00642        AT END MOVE 'UNDEFINED MINOR CAUSE' TO WS-D1-DESCRIPTION   EL332
00643          WHEN MIN-CODE ( SUB-MINOR ) = WS-SAVE-MINOR-CAUSE        EL332
00644            MOVE MIN-DESCRIPTION (SUB-MINOR) TO WS-D1-DESCRIPTION. EL332
00645                                                                   EL332
00646      IF WS-LINE-COUNT > +53                                       EL332
00647          PERFORM 3000-PRINT-HEADINGS.                             EL332
00648                                                                   EL332
00649      WRITE PRT FROM WS-DETAIL1                                    EL332
00650          AFTER ADVANCING WS-CC LINES.                             EL332
00651                                                                   EL332
00652      ADD  WS-CC                    TO WS-LINE-COUNT.              EL332
00653      MOVE 1                        TO WS-CC.                      EL332
00654      ADD WS-MINOR-CAUSE-LF-CNT     TO WS-MAJOR-CAUSE-LF-CNT.      EL332
00655      ADD WS-MINOR-CAUSE-LF-AMT     TO WS-MAJOR-CAUSE-LF-AMT.      EL332
00656      ADD WS-MINOR-CAUSE-AH-CNT     TO WS-MAJOR-CAUSE-AH-CNT.      EL332
00657      ADD WS-MINOR-CAUSE-AH-AMT     TO WS-MAJOR-CAUSE-AH-AMT.      EL332
00658                                                                   EL332
00659  1400-REPORT-A-MINOR-CAUSE-LOOP  SECTION.                         EL332
00660                                                                   EL332
00661      ADD  +1                     TO WS-RECS-RETURNED-A.           EL332
00662                                                                   EL332
00663      IF SR-LOSS-LF                                                EL332
00664          ADD +1                  TO WS-MINOR-CAUSE-LF-CNT         EL332
00665          ADD SR-CLAIM-AMT        TO WS-MINOR-CAUSE-LF-AMT         EL332
00666        ELSE                                                       EL332
00667 ***  IF SR-LOSS-AH                                                EL332
00668          ADD +1                  TO WS-MINOR-CAUSE-AH-CNT         EL332
00669          ADD SR-CLAIM-AMT        TO WS-MINOR-CAUSE-AH-AMT.        EL332
00670                                                                   EL332
00671      RETURN SORT-FILE                                             EL332
00672          AT END MOVE 'Y' TO WS-SORT-EOF-SW.                       EL332
00673                                                                   EL332
00674 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  EL332
00675      EJECT                                                        EL332
00676  2100-REPORT-B-LOOP               SECTION.                        EL332
00677                                                                   EL332
00678      MOVE SR-MAJOR-CAUSE          TO WS-SAVE-MAJOR-CAUSE.         EL332
00679      MOVE ZERO                    TO WS-MAJOR-CAUSE-LF-CNT,       EL332
00680                                      WS-MAJOR-CAUSE-LF-AMT,       EL332
00681                                      WS-MAJOR-CAUSE-AH-CNT,       EL332
00682                                      WS-MAJOR-CAUSE-AH-AMT.       EL332
00683      MOVE SPACES                  TO WS-D1-LITERAL,               EL332
00684                                      WS-D1-FLAG1,                 EL332
00685                                      WS-D1-FLAG2,                 EL332
00686                                      WS-D1-FLAG3,                 EL332
00687                                      WS-D1-FLAG4.                 EL332
00688      MOVE 2                       TO WS-CC.                       EL332
00689                                                                   EL332
00690      PERFORM 2200-REPORT-B-MAJOR-CAUSE-LOOP                       EL332
00691          UNTIL SR-MAJOR-CAUSE NOT = WS-SAVE-MAJOR-CAUSE  OR       EL332
00692                WS-SORT-EOF-YES.                                   EL332
00693                                                                   EL332
00694 ***** PRINT MAJOR CAUSE TOTALS *****                              EL332
00695                                                                   EL332
00696      MOVE 'TOTAL:'                  TO WS-D1-LITERAL.             EL332
00697      MOVE ' *'                      TO WS-D1-FLAG1,               EL332
00698                                        WS-D1-FLAG3,               EL332
00699      MOVE '*'                       TO WS-D1-FLAG2,               EL332
00700                                        WS-D1-FLAG4.               EL332
00701                                                                   EL332
00702      MOVE WS-SAVE-MAJOR-CAUSE       TO WS-D1-CAUSE-CODE.          EL332
00703      MOVE WS-MAJOR-CAUSE-LF-CNT     TO WS-D1-LF-CNT.              EL332
00704      MOVE WS-MAJOR-CAUSE-LF-AMT     TO WS-D1-LF-AMT.              EL332
00705      MOVE WS-MAJOR-CAUSE-AH-CNT     TO WS-D1-AH-CNT.              EL332
00706      MOVE WS-MAJOR-CAUSE-AH-AMT     TO WS-D1-AH-AMT.              EL332
00707                                                                   EL332
00708      SET SUB-MAJOR TO +1.                                         EL332
00709      SEARCH ALL MAJOR-CAUSE-ENTRIES                               EL332
00710        AT END MOVE 'UNDEFINED MAJOR CAUSE' TO WS-D1-DESCRIPTION   EL332
00711          WHEN MAJ-CODE ( SUB-MAJOR ) = WS-SAVE-MAJOR-CAUSE        EL332
00712            MOVE MAJ-DESCRIPTION (SUB-MAJOR) TO WS-D1-DESCRIPTION. EL332
00713                                                                   EL332
00714      IF WS-LINE-COUNT > +54                                       EL332
00715          PERFORM 3000-PRINT-HEADINGS.                             EL332
00716                                                                   EL332
00717      WRITE PRT FROM WS-DETAIL1                                    EL332
00718          AFTER ADVANCING 2 LINES.                                 EL332
00719                                                                   EL332
00720      ADD +2                        TO WS-LINE-COUNT.              EL332
00721      ADD WS-MAJOR-CAUSE-LF-CNT     TO WS-GRAND-LF-CNT.            EL332
00722      ADD WS-MAJOR-CAUSE-LF-AMT     TO WS-GRAND-LF-AMT.            EL332
00723      ADD WS-MAJOR-CAUSE-AH-CNT     TO WS-GRAND-AH-CNT.            EL332
00724      ADD WS-MAJOR-CAUSE-AH-AMT     TO WS-GRAND-AH-AMT.            EL332
00725                                                                   EL332
00726      EJECT                                                        EL332
00727  2200-REPORT-B-MAJOR-CAUSE-LOOP   SECTION.                        EL332
00728                                                                   EL332
00729      MOVE SR-MINOR-CAUSE          TO WS-SAVE-MINOR-CAUSE.         EL332
00730      MOVE ZERO                    TO WS-MINOR-CAUSE-LF-CNT,       EL332
00731                                      WS-MINOR-CAUSE-LF-AMT,       EL332
00732                                      WS-MINOR-CAUSE-AH-CNT,       EL332
00733                                      WS-MINOR-CAUSE-AH-AMT.       EL332
00734                                                                   EL332
00735      PERFORM 2300-REPORT-B-MINOR-CAUSE-LOOP                       EL332
00736          UNTIL SR-MINOR-CAUSE NOT = WS-SAVE-MINOR-CAUSE  OR       EL332
00737                SR-MAJOR-CAUSE NOT = WS-SAVE-MAJOR-CAUSE  OR       EL332
00738                WS-SORT-EOF-YES.                                   EL332
00739                                                                   EL332
00740 ***** PRINT MINOR CAUSE TOTALS *****                              EL332
00741                                                                   EL332
00742      MOVE WS-SAVE-CAUSE-CODE        TO WS-D1-CAUSE-CODE.          EL332
00743      MOVE WS-MINOR-CAUSE-LF-CNT     TO WS-D1-LF-CNT.              EL332
00744      MOVE WS-MINOR-CAUSE-LF-AMT     TO WS-D1-LF-AMT.              EL332
00745      MOVE WS-MINOR-CAUSE-AH-CNT     TO WS-D1-AH-CNT.              EL332
00746      MOVE WS-MINOR-CAUSE-AH-AMT     TO WS-D1-AH-AMT.              EL332
00747                                                                   EL332
00748      SET SUB-MINOR TO +1.                                         EL332
00749      SEARCH ALL MINOR-CAUSE-ENTRIES                               EL332
00750        AT END MOVE 'UNDEFINED MINOR CAUSE' TO WS-D1-DESCRIPTION   EL332
00751          WHEN MIN-CODE ( SUB-MINOR ) = WS-SAVE-MINOR-CAUSE        EL332
00752            MOVE MIN-DESCRIPTION (SUB-MINOR) TO WS-D1-DESCRIPTION. EL332
00753                                                                   EL332
00754      IF WS-LINE-COUNT > +53                                       EL332
00755          PERFORM 3000-PRINT-HEADINGS.                             EL332
00756                                                                   EL332
00757      WRITE PRT FROM WS-DETAIL1                                    EL332
00758          AFTER ADVANCING WS-CC LINES.                             EL332
00759                                                                   EL332
00760      ADD  WS-CC                    TO WS-LINE-COUNT.              EL332
00761      MOVE 1                        TO WS-CC.                      EL332
00762      ADD WS-MINOR-CAUSE-LF-CNT     TO WS-MAJOR-CAUSE-LF-CNT.      EL332
00763      ADD WS-MINOR-CAUSE-LF-AMT     TO WS-MAJOR-CAUSE-LF-AMT.      EL332
00764      ADD WS-MINOR-CAUSE-AH-CNT     TO WS-MAJOR-CAUSE-AH-CNT.      EL332
00765      ADD WS-MINOR-CAUSE-AH-AMT     TO WS-MAJOR-CAUSE-AH-AMT.      EL332
00766                                                                   EL332
00767  2300-REPORT-B-MINOR-CAUSE-LOOP    SECTION.                       EL332
00768                                                                   EL332
00769      ADD  +1                     TO WS-RECS-RETURNED-B.           EL332
00770                                                                   EL332
00771      IF SR-LOSS-LF                                                EL332
00772          ADD +1                  TO WS-MINOR-CAUSE-LF-CNT         EL332
00773          ADD SR-CLAIM-AMT        TO WS-MINOR-CAUSE-LF-AMT         EL332
00774        ELSE                                                       EL332
00775 ***  IF SR-LOSS-AH                                                EL332
00776          ADD +1                  TO WS-MINOR-CAUSE-AH-CNT         EL332
00777          ADD SR-CLAIM-AMT        TO WS-MINOR-CAUSE-AH-AMT.        EL332
00778                                                                   EL332
00779      RETURN SORT-FILE                                             EL332
00780          AT END MOVE 'Y'         TO WS-SORT-EOF-SW.               EL332
00781                                                                   EL332
00782      EJECT                                                        EL332
00783  3000-PRINT-HEADINGS          SECTION.                            EL332
00784                                                                   EL332
00785      MOVE ZERO                   TO WS-LINE-COUNT.                EL332
00786      ADD  +1                     TO WS-PAGE-COUNT.                EL332
00787      MOVE WS-PAGE-COUNT          TO WS-H3-PAGE.                   EL332
00788      MOVE 2                      TO WS-CC.                        EL332
00789                                                                   EL332
00790      WRITE PRT FROM WS-HEADING-1                                  EL332
00791          AFTER ADVANCING TO-TOP-OF-PAGE.                          EL332
00792      WRITE PRT FROM WS-HEADING-2                                  EL332
00793          AFTER ADVANCING 1 LINES.                                 EL332
00794      WRITE PRT FROM WS-HEADING-3                                  EL332
00795          AFTER ADVANCING 1 LINES.                                 EL332
00796      WRITE PRT FROM WS-HEADING-4                                  EL332
00797          AFTER ADVANCING 2 LINES.                                 EL332
00798      WRITE PRT FROM WS-HEADING-5                                  EL332
00799          AFTER ADVANCING 1 LINES.                                 EL332
00800                                                                   EL332
00801  ABEND-PGM                    SECTION.                            EL332
00802      COPY ELCABEND.                                               EL332
00803 *                                                                 EL332
00804 * ** ***  END OF REPORT PROGRAM EL332  *** ** *                   EL332
