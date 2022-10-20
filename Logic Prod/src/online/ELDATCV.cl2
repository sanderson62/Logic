00001  IDENTIFICATION DIVISION.                                         06/11/98
00002                                                                   ELDATCV
00003  PROGRAM-ID.                 ELDATCV.                                LV037
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 03/05/96 14:47:01.                    CL**6
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*25
00008 *                            VMOD=2.007                              CL*21
00009                                                                   ELDATCV
00010 *AUTHOR.     LOGIC, INC.                                             CL**6
00011 *            DALLAS, TEXAS.                                          CL**6
00012                                                                   ELDATCV
00013 *DATE-COMPILED.                                                      CL**6
00014                                                                   ELDATCV
00015 *SECURITY.   *****************************************************   CL**6
00016 *            *                                                   *   CL**6
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**6
00018 *            *                                                   *   CL**6
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00022 *            *                                                   *   CL**6
00023 *            *****************************************************   CL**6
00024                                                                   ELDATCV
00025 *REMARKS.    *****************************************************   CL**6
00026 *            *                                                   *   CL**6
00027 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *   CL**6
00028 *            *    OPTION SPECIFIED, CONVERT A GIVEN DATE TO      *   CL**6
00029 *            *    THE FOLLOWING:  I. EDITED GREGORIAN (MM/DD/YY) *   CL**6
00030 *            *                   II. Y-M-D GREGORIAN  (YYMMDD)   *   CL**6
00031 *            *                  III. M-D-Y GREGORIAN  (MMDDYY)   *   CL**6
00032 *            *                   IV. ALPHA GREG  (MMM DD, YYYY)  *   CL**6
00033 *            *                    V. JULIAN  (YYDDD)             *   CL**6
00034 *            *                   VI. TOTAL DAYS SINCE 1900.      *   CL**6
00035 *            *    ADDITIONALLY, IF TWO DATES (EXPRESSED AS DAYS) *   CL**6
00036 *            *    ARE GIVEN, THE ELAPSED MONTHS AND DAYS CAN BE  *   CL**6
00037 *            *    CALCULATED.                                    *   CL**6
00038 *            *                                                   *   CL**6
00039 *            *****************************************************   CL**6
00040  ENVIRONMENT DIVISION.                                            ELDATCV
00041                                                                   ELDATCV
00042  DATA DIVISION.                                                   ELDATCV
00043      EJECT                                                        ELDATCV
00044  WORKING-STORAGE SECTION.                                         ELDATCV
00045  77  FILLER   PIC X(32) VALUE '********************************'. ELDATCV
00046  77  FILLER   PIC X(32) VALUE '*   ELDATCV WORKING STORAGE    *'. ELDATCV
00047  77  FILLER   PIC X(32) VALUE '********* VMOD 2.007 ***********'.    CL*21
00048                                                                   ELDATCV
pemuni 01 WKS-IN.
pemuni    10  FILLER    PIC X(6) VALUE 'DATEIN'.
pemuni    10  WKS-IN-TN PIC 9(10).
pemuni
pemuni 01 WKS-OT.
pemuni    10  FILLER    PIC X(6) VALUE 'DATEOT'.
pemuni    10  WKS-OT-TN PIC 9(10).
pemuni
00049  01  W-PROGRAM-WORK-AREA.                                            CL**2
00050      12  W-CURRENT-DATE              PIC  9(07).                     CL*10
00051      12  FILLER REDEFINES W-CURRENT-DATE.                            CL**2
00052          16  FILLER                  PIC  9.                         CL*11
00053          16  W-CD-CCYY               PIC  999.                       CL*35
00054          16  W-CD-CCYY-R  REDEFINES W-CD-CCYY.                       CL*32
00055              20  W-CD-YEAR-CD        PIC  9.                         CL**7
00056              20  W-CURRENT-YEAR-YY   PIC  99.                        CL**7
00057          16  W-CURRENT-DAYS          PIC  999.                       CL*11
00058      12  W-FLOAT-YEAR                PIC  999.                       CL*35
00059      12  W-FLOAT-YY REDEFINES W-FLOAT-YEAR.                          CL**7
00060          16  W-FLOAT-YEAR-CD         PIC  9.                         CL*11
00061          16  W-FLOAT-YEAR-YY         PIC  99.                        CL*11
00062      EJECT                                                           CL**2
00063                              COPY ELCDATW1.                          CL**4
00064      EJECT                                                        ELDATCV
00065                              COPY ELCDATE.                           CL*26
00066      EJECT                                                        ELDATCV
00067  LINKAGE SECTION.                                                 ELDATCV
00068                                                                      CL*17
00069  01  DFHCOMMAREA                     PIC X(200).                     CL*27
00070                                                                      CL*24
00071  PROCEDURE DIVISION.

           MOVE eibtaskn to WKS-IN-TN.
pemuni*    EXEC CICS WRITE OPERATOR
pemuni*         TEXT(WKS-IN)
pemuni*          TEXTLENGTH(16)
pemuni*    END-EXEC.
00072                                                                      CL*28
00073      MOVE DFHCOMMAREA          TO  DATE-CONVERSION-DATA.             CL*29
00074                                                                      CL*29
00075      MOVE SPACES                TO DC-ERROR-CODE.                    CL*34
00076                                                                      CL*34
00077      INITIALIZE DC-GREG-DATE-1-ALPHA.                                CL*35
00078                                                                      CL*29
00079      MOVE EIBDATE              TO  W-CURRENT-DATE.                   CL*29
00080                                                                      CL*35
00081      MOVE W-CD-YEAR-CD         TO CONTROL-YEAR-CD                    CL*33
00082                                   SEARCH-CENTURY-CD-N.               CL*33
00083                                                                      CL*33
00084      MOVE '1'                  TO FIND-CENTURY-FLAG.                 CL*33
00085      PERFORM 0800-FIND-CENTURY.                                      CL*35
00086      IF NO-CONVERSION-ERROR                                          CL*33
00087          MOVE FOUND-CENTURY-N  TO CURRENT-CENTURY-1-N.               CL*35
00088                                                                      CL*33
00089      COMPUTE W-FLOAT-YEAR = W-CD-CCYY - CONTROL-YEAR.                CL*32
pemuni*    display 'Test cobsw cpdebug ' eibtaskn.
00090                                                                   ELDATCV
00091  0100-PROGRAM-PROCESSING SECTION. COPY ELCDATP1.                  ELDATCV
00092                                                                   ELDATCV
00093      MOVE DATE-CONVERSION-DATA   TO  DFHCOMMAREA.                 ELDATCV

pemuni*    MOVE eibtaskn to WKS-OT-TN.
pemuni*    EXEC CICS WRITE OPERATOR
pemuni*         TEXT(WKS-OT)
pemuni*         TEXTLENGTH(16)
pemuni*    END-EXEC.
00094                                                                   ELDATCV
00095      EXEC CICS RETURN                                             ELDATCV
00096           END-EXEC.                                               ELDATCV
00097                                                                   ELDATCV
00098        GOBACK.                                                    ELDATCV
00099      EJECT                                                        ELDATCV
00100  0200-DATE-CONVERSION-ROUTINES SECTION. COPY ELCDATP2.            ELDATCV
00101                                                                   ELDATCV
