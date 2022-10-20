00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ELRESVX
00003  PROGRAM-ID.                 ELRESVX.                                LV005
00004 *              PROGRAM CONVERTED BY                               ELRESVX
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ELRESVX
00006 *              CONVERSION DATE 12/20/95 09:15:46.                 ELRESVX
00007 *                            VMOD=2.008                           ELRESVX
00008 *                                                                 ELRESVX
00009 *AUTHOR.       LOGIC, INC.                                        ELRESVX
00010 *              DALLAS, TEXAS.                                     ELRESVX
00011                                                                   ELRESVX
00012 *DATE-COMPILED.                                                   ELRESVX
00013                                                                   ELRESVX
00014 *SECURITY.   *****************************************************ELRESVX
00015 *            *                                                   *ELRESVX
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELRESVX
00017 *            *                                                   *ELRESVX
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELRESVX
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELRESVX
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ELRESVX
00021 *            *                                                   *ELRESVX
00022 *            *****************************************************ELRESVX
00023                                                                   ELRESVX
00024 *REMARKS.                                                         ELRESVX
00025                                                                   ELRESVX
00026 *        THIS SUBROUTINE IS USED TO CALCULATE FUTURE (ACCRUED),   ELRESVX
00027 *    PAY-TO-CURRENT, AND I.B.N.R. LOSS RESERVES FOR BOTH LIFE AND ELRESVX
00028 *    A/H.                                                         ELRESVX
00029                                                                   ELRESVX
00030      EJECT                                                        ELRESVX
00031  ENVIRONMENT DIVISION.                                            ELRESVX
00032                                                                   ELRESVX
00033  DATA DIVISION.                                                   ELRESVX
00034                                                                   ELRESVX
00035  WORKING-STORAGE SECTION.                                         ELRESVX
00036  77  FILLER   PIC X(32) VALUE '********************************'. ELRESVX
00037  77  FILLER   PIC X(32) VALUE '**  ELRESVX WORKING STORAGE   **'. ELRESVX
00038  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.008 *********'. ELRESVX
00039                                                                   ELRESVX
00040                             COPY ELCRESW1.                        ELRESVX
00041                                                                   ELRESVX
00042                             COPY ELC64CDT  SUPPRESS.              ELRESVX
00043                                                                   ELRESVX
00044          EJECT                                                    ELRESVX
00045                             COPY ELCDATE.                            CL**5
00046                                                                   ELRESVX
00047      EJECT                                                        ELRESVX
00048  LINKAGE SECTION.                                                 ELRESVX
00049                             COPY ELCCALC.                         ELRESVX
00050                                                                   ELRESVX
00051      EJECT                                                        ELRESVX
00052  PROCEDURE DIVISION USING CALCULATION-PASS-AREA.                  ELRESVX
00053                                                                   ELRESVX
00054  0000-CALCULATE-RESERVES SECTION. COPY ELCRESP1.                  ELRESVX
00055                                                                   ELRESVX
00056      IF CP-RETURN-CODE NOT = ZERO                                 ELRESVX
00057          MOVE ZERO               TO  CP-RESERVE-REMAINING-TERM    ELRESVX
00058        ELSE                                                       ELRESVX
00059          MOVE WS-REMAINING-TERM  TO  CP-RESERVE-REMAINING-TERM.   ELRESVX
00060                                                                   ELRESVX
00061      GOBACK.                                                      ELRESVX
00062                                                                   ELRESVX
00063      EJECT                                                        ELRESVX
00064  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       ELRESVX
00065                                                                   ELRESVX
