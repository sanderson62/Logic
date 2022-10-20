00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ELRESV
00002                                                                   ELRESV
00003  PROGRAM-ID.                 ELRESV.                                 LV003
00004 *              PROGRAM CONVERTED BY                               ELRESV
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ELRESV
00006 *              CONVERSION DATE 03/05/96 16:22:23.                 ELRESV
00007 *                            VMOD=2.007                           ELRESV
00008 *                                                                 ELRESV
00009 *AUTHOR.     LOGIC, INC.                                          ELRESV
00010 *            DALLAS, TEXAS.                                       ELRESV
00011                                                                   ELRESV
00012 *DATE-COMPILED.                                                   ELRESV
00013                                                                   ELRESV
00014 *SECURITY.   *****************************************************ELRESV
00015 *            *                                                   *ELRESV
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELRESV
00017 *            *                                                   *ELRESV
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELRESV
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELRESV
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ELRESV
00021 *            *                                                   *ELRESV
00022 *            *****************************************************ELRESV
00023                                                                   ELRESV
00024 *REMARKS.                                                         ELRESV
00025                                                                   ELRESV
00026 *        THIS SUBROUTINE IS USED TO CALCULATE FUTURE (ACCRUED),   ELRESV
00027 *    PAY-TO-CURRENT, AND I.B.N.R. LOSS RESERVES FOR BOTH LIFE AND ELRESV
00028 *    A/H.                                                         ELRESV
00029                                                                   ELRESV
00030      EJECT                                                        ELRESV
00031  ENVIRONMENT DIVISION.                                            ELRESV
00032                                                                   ELRESV
00033  DATA DIVISION.                                                   ELRESV
00034                                                                   ELRESV
00035  WORKING-STORAGE SECTION.                                         ELRESV
00036  77  FILLER   PIC X(32) VALUE '********************************'. ELRESV
00037  77  FILLER   PIC X(32) VALUE '**  ELRESV  WORKING STORAGE   **'. ELRESV
00038  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.007 *********'. ELRESV
00039                                                                   ELRESV
00040                             COPY ELCRESW1.                        ELRESV
00041                                                                   ELRESV
00042                             COPY ELC64CDT SUPPRESS.               ELRESV
00043                                                                   ELRESV
00044          EJECT                                                    ELRESV
00045                             COPY ELCDATE.                            CL**3
00046                                                                   ELRESV
00047                                                                   ELRESV
00048      EJECT                                                        ELRESV
00049                             COPY ELCCALC.                         ELRESV
00050                                                                   ELRESV
00051      EJECT                                                        ELRESV
00052  LINKAGE SECTION.                                                 ELRESV
00053  01  DFHCOMMAREA              PIC X(450).                         ELRESV
00054                                                                   ELRESV
00055      EJECT                                                        ELRESV
00056  PROCEDURE DIVISION.                                              ELRESV
00057                                                                   ELRESV
00058      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.       ELRESV
00059                                                                   ELRESV
uktdel*0000-CALCULATE-RESERVES SECTION. COPY ELCRESP1.                  ELRESV
uktins 0000-CALCULATE-RESERVES SECTION.
uktins     COPY ELCRESP1.
00061                                                                   ELRESV
00062      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.                 ELRESV
00063                                                                   ELRESV
00064      EXEC CICS RETURN                                             ELRESV
00065          END-EXEC.                                                ELRESV
00066                                                                   ELRESV
00067      EJECT                                                        ELRESV
00068  8500-DATE-CONVERSION SECTION.                                    ELRESV
00069                                                                   ELRESV
00070 *    NOTE ******************************************************* ELRESV
00071 *         *                                                     * ELRESV
00072 *         *  THIS SECTION CALLS THE DATE CONVERSION SUBROUTINE. * ELRESV
00073 *         *                                                     * ELRESV
00074 *         *******************************************************.ELRESV
00075                                                                   ELRESV
00076  8510-DATE-CONVERSION.                                            ELRESV
00077      EXEC CICS LINK                                               ELRESV
00078          PROGRAM  (WS-DATE-CONVERSION-PROGRAM)                    ELRESV
00079          COMMAREA (DATE-CONVERSION-DATA)                          ELRESV
00080          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      ELRESV
00081                                                                   ELRESV
00082      IF DC-ERROR-CODE NOT = SPACES                                ELRESV
00083          MOVE '2'                TO  CP-RETURN-CODE               ELRESV
00084          GO TO 0999-EXIT.                                         ELRESV
00085                                                                   ELRESV
00086  8590-EXIT.                                                       ELRESV
00087      EXIT.                                                        ELRESV
00088                                                                   ELRESV
00089  9999-LAST-PARAGRAPH SECTION.                                     ELRESV
00090                                                                   ELRESV
00091      GOBACK.                                                      ELRESV
