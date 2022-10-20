00001  IDENTIFICATION DIVISION.                                         06/11/98
00002                                                                   ELDATCX
00003  PROGRAM-ID.                 ELDATCX.                                LV014
00004 *              PROGRAM CONVERTED BY                               ELDATCX
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ELDATCX
00006 *              CONVERSION DATE 11/22/95 08:57:08.                 ELDATCX
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ELDATCX
00008 *                            VMOD=2.006                              CL**2
00009                                                                   ELDATCX
00010 *AUTHOR.     LOGIC, INC.                                          ELDATCX
00011 *            DALLAS, TEXAS.                                       ELDATCX
00012                                                                   ELDATCX
00013 *DATE-COMPILED.                                                   ELDATCX
00014                                                                   ELDATCX
00015 *SECURITY.   *****************************************************ELDATCX
00016 *            *                                                   *ELDATCX
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELDATCX
00018 *            *                                                   *ELDATCX
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELDATCX
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELDATCX
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ELDATCX
00022 *            *                                                   *ELDATCX
00023 *            *****************************************************ELDATCX
00024                                                                   ELDATCX
00025 *REMARKS.    *****************************************************ELDATCX
00026 *            *                                                   *ELDATCX
00027 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *ELDATCX
00028 *            *    OPTION SPECIFIED, CONVERT A GIVEN DATE INTO    *ELDATCX
00029 *            *    THE FOLLOWING:  I. EDITED GREGORIAN (MM/DD/YY) *ELDATCX
00030 *            *                   II. Y-M-D GREGORIAN  (YYMMDD)   *ELDATCX
00031 *            *                  III. M-D-Y GREGORIAN  (MMDDYY)   *ELDATCX
00032 *            *                   IV. ALPHA GREG  (MMM DD, YYYY)  *ELDATCX
00033 *            *                    V. JULIAN  (YYDDD)             *ELDATCX
00034 *            *                   VI. TOTAL DAYS SINCE 1900.      *ELDATCX
00035 *            *    ADDITIONALLY, IF TWO DATES (EXPRESSED AS DAYS) *ELDATCX
00036 *            *    ARE GIVEN, THE ELAPSED MONTHS AND DAYS CAN BE  *ELDATCX
00037 *            *    CALCULATED.                                    *ELDATCX
00038 *            *                                                   *ELDATCX
00039 *            *****************************************************ELDATCX
00040  ENVIRONMENT DIVISION.                                            ELDATCX
00041                                                                   ELDATCX
00042  DATA DIVISION.                                                   ELDATCX
00043      EJECT                                                        ELDATCX
00044  WORKING-STORAGE SECTION.                                         ELDATCX
00045  77  FILLER   PIC X(32) VALUE '********************************'. ELDATCX
00046  77  FILLER   PIC X(32) VALUE '**  ELDATCX WORKING STORAGE   **'. ELDATCX
00047  77  FILLER   PIC X(32) VALUE '********* VMOD 2.006 ***********'.    CL**2
00048                                                                   ELDATCX
00049  01  W-PROGRAM-WORK-AREA.                                         ELDATCX
00050      12  W-CURRENT-DATE.                                          ELDATCX
00051          16  W-CD-CCYY               PIC  999     VALUE ZERO.        CL*14
00052          16  W-CD-CCYY-R  REDEFINES W-CD-CCYY.                       CL**6
00053              20  W-CD-YEAR-CD        PIC  9.                      ELDATCX
00054              20  W-CURRENT-YEAR-YY   PIC  99.                     ELDATCX
00055          16  W-CURRENT-MONTH         PIC  99      VALUE ZERO.     ELDATCX
00056          16  W-CURRENT-DAY           PIC  99      VALUE ZERO.     ELDATCX
00057      12  W-FLOAT-YEAR                PIC  999     VALUE ZERO.        CL**2
00058      12  W-FLOAT-YY REDEFINES W-FLOAT-YEAR.                       ELDATCX
00059          16  W-FLOAT-YEAR-CD         PIC  9.                         CL**2
00060          16  W-FLOAT-YEAR-YY         PIC  99.                        CL**2
CIDMOD*    12  FUNCTION-DATE               PIC  X(21).
00062                                                                   ELDATCX
CIDMOD 01  SYSTEM-DATE.
CIDMOD     05  SYS-MO         PIC 9(2).
CIDMOD     05  SYS-DA         PIC 9(2).
CIDMOD     05  SYS-CCYY       PIC 9(4).
CIDMOD
CIDMOD                               COPY ELCFUNDT.                     ELDATCX
CIDMOD                                                                  ELDATCX
00063      COPY ELCDATW1.                                               ELDATCX
00064                                                                   ELDATCX
00065      EJECT                                                        ELDATCX
00066  LINKAGE SECTION. COPY ELCDATE.                                      CL**5
00067                                                                   ELDATCX
00068      EJECT                                                        ELDATCX
00069  PROCEDURE DIVISION USING DATE-CONVERSION-DATA.                   ELDATCX
00070                                                                      CL**4
CIDMOD     IF DC-FORCE-EL310-DATE
CIDMOD        MOVE DC-EL310-DATE       TO FUNCTION-DATE
CIDMOD     ELSE
pemuni        MOVE FUNCTION CURRENT-DATE
pemuni                                 TO FUNCTION-DATE
pemuni*       CALL 'SYSDATE'           USING SYSTEM-DATE
pemuni*       MOVE SYS-MO              TO WS-FN-MO
pemuni*       MOVE SYS-DA              TO WS-FN-DA
pemuni*       MOVE SYS-CCYY            TO WS-FN-CCYR
CIDMOD     END-IF
CIDMOD
CIDMOD     MOVE WS-FN-CC               TO CURRENT-CENTURY-1-N
CIDMOD     MOVE WS-FN-YR               TO W-CURRENT-YEAR-YY
CIDMOD     MOVE WS-FN-MO               TO W-CURRENT-MONTH
CIDMOD     MOVE WS-FN-DA               TO W-CURRENT-DAY
CIDMOD
CIDMOD*    ACCEPT W-CURRENT-DATE(2:6) FROM DATE.                           CL*13
00072                                                                      CL*14
00073 *    MOVE FUNCTION CURRENT-DATE TO FUNCTION-DATE.                    CL*10
00074 *    MOVE FUNCTION-DATE(1:2)    TO CURRENT-CENTURY-1-N.              CL*10
00075                                                                      CL*14
00076      MOVE CURRENT-CENTURY-1-N    TO SEARCH-CENTURY.                  CL*14
00077                                                                      CL*14
00078      MOVE '2'                    TO FIND-CENTURY-FLAG.               CL**7
00079      PERFORM 0800-FIND-CENTURY                                       CL**7
CIDMOD*    IF NO-CONVERSION-ERROR                                          CL**7
CIDMOD*        MOVE FOUND-CENTURY-CD-N TO W-CD-YEAR-CD                     CL**7
CIDMOD*                                   CONTROL-YEAR-CD                  CL*14
CIDMOD*    END-IF
00083                                                                      CL*14
CIDMOD     MOVE FOUND-CENTURY-CD-N     TO W-CD-YEAR-CD
CIDMOD                                    CONTROL-YEAR-CD
CIDMOD
00084      COMPUTE W-FLOAT-YEAR = W-CD-CCYY - CONTROL-YEAR.                CL**6
00085                                                                   ELDATCX
00086  0100-PROGRAM-PROCESSING SECTION.  COPY ELCDATP1.                 ELDATCX
00087                                                                   ELDATCX
00088  0100-EXIT.                                                       ELDATCX
CIDMOD     MOVE ZEROS                  TO RETURN-CODE
00089      GOBACK.                                                      ELDATCX
00090                                                                      CL*14
00091      EJECT                                                           CL*14
00092  0200-DATE-CONVERSION-ROUTINES SECTION. COPY ELCDATP2.            ELDATCX
00093                                                                   ELDATCX
