00001  IDENTIFICATION DIVISION.                                         10/08/97
00002                                                                   ecs220
00003  PROGRAM-ID.                 ecs220.                                 LV002
00004 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ecs220
00005 *                            VMOD=2.005.                             CL**2
00006 *                                                                 ecs220
00007 *AUTHOR.        LOGIC, INC.                                       ecs220
00008 *               DALLAS, TEXAS.                                    ecs220
00009 *                                                                 ecs220
00010 *DATE-COMPILED.                                                   ecs220
00011 *                                                                 ecs220
00012 *SECURITY.   *****************************************************ecs220
00013 *            *                                                   *ecs220
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ecs220
00015 *            *                                                   *ecs220
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ecs220
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ecs220
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ecs220
00019 *            *                                                   *ecs220
00020 *            *****************************************************ecs220
00021 *                                                                 ecs220
00022 *REMARKS.                                                         ecs220
00023 *        THIS PROGRAM SORTS THE cert FILE INTO THE                ecs220
00024 *        CORRECT SEQUENCE.                                        ecs220
00025                                                                   ecs220
00026  ENVIRONMENT DIVISION.                                            ecs220
00027  INPUT-OUTPUT SECTION.                                            ecs220
00028  FILE-CONTROL.                                                    ecs220
00029                                                                   ecs220
00030      SELECT  SORT-FILE     ASSIGN TO   SYS001-UT-FBA1-S-SORTWK1.  ecs220
00031      SELECT  cert-IN       ASSIGN TO SYS010-UT-2400-S-SYS010.     ecs220
00032      SELECT  cert-OUT      ASSIGN TO SYS011-UT-2400-S-SYS011.     ecs220
00033  EJECT                                                            ecs220
00034  DATA DIVISION.                                                   ecs220
00035  FILE SECTION.                                                    ecs220
00036                                                                   ecs220
00037  SD  SORT-FILE                                                    ecs220
00038      RECORDING MODE F                                             ecs220
00039      RECORD CONTAINS 1056 CHARACTERS.                             ecs220
00040  01  SORT-WORK-REC.                                               ecs220
00041      12  filler                  PIC XXx.                         ecs220
00043      12  SW-key                  pic x(36).                       ecs220
00056      12  filler                  PIC X(1017).                     ecs220
00057                                                                   ecs220
00058  FD  cert-IN                                                      ecs220
00059      RECORDING MODE F                                             ecs220
00060      LABEL RECORDS STANDARD                                       ecs220
00061      BLOCK CONTAINS 0 RECORDS
00062      DATA RECORD IS ct-EXTR.                                      ecs220
00063  01  ct-EXTR                 PIC X(1056).                         ecs220
00064                                                                   ecs220
00065  FD  cert-OUT                                                     ecs220
00066      RECORDING MODE F                                             ecs220
00067      LABEL RECORDS STANDARD                                       ecs220
00068      BLOCK CONTAINS 0 RECORDS
00069      DATA RECORD IS cert-RECORD.                                  ecs220
00070  01  cert-RECORD             PIC X(1056).                         ecs220
00071  EJECT                                                            ecs220
00072  WORKING-STORAGE SECTION.                                         ecs220
00073  77  FILLER  PIC X(32) VALUE '********************************'.  ecs220
00074  77  FILLER  PIC X(32) VALUE '   ecs220   WORKING-STORAGE     '.  ecs220
00075  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.     CL**2
00076                                                                   ecs220
00077  PROCEDURE DIVISION.                                              ecs220
00078  BEGIN.                                                           ecs220
00079                                                                   ecs220
00085                                                                   ecs220
00086      SORT SORT-FILE ASCENDING KEY SW-key                          ecs220
00096              USING  cert-IN                                       ecs220
00097              GIVING cert-OUT.                                     ecs220
00098                                                                   ecs220
00099      GOBACK.                                                      ecs220
