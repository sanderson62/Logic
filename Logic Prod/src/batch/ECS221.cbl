00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ecs221
00003  PROGRAM-ID.                ecs221.                                  LV007
00004 *              PROGRAM CONVERTED BY                               ecs221
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ecs221
00006 *              CONVERSION DATE 11/28/95 11:11:38.                 ecs221
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ecs221
00008 *                           VMOD=2.005.                           ecs221
00009                                                                   ecs221
00010 *AUTHOR.        LOGIC, INC.                                       ecs221
00011 *               DALLAS, TEXAS.                                    ecs221
00012                                                                   ecs221
00013 *DATE-COMPILED.                                                   ecs221
00014                                                                   ecs221
00015 *SECURITY.   *****************************************************ecs221
00016 *            *                                                   *ecs221
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ecs221
00018 *            *                                                   *ecs221
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ecs221
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ecs221
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ecs221
00022 *            *                                                   *ecs221
00023 *            *****************************************************ecs221
00024                                                                   ecs221
00025 *REMARKS.                                                         ecs221
00026 *        PROGRAM sorts the output of CIRE010 and creates a        ecs221
00027 *        print file.                                              ecs221
00031                                                                   ecs221
00032  ENVIRONMENT DIVISION.                                            ecs221
00033  CONFIGURATION SECTION.                                           ecs221
00034                                                                      CL**4
00035  INPUT-OUTPUT SECTION.                                            ecs221
00036  FILE-CONTROL.                                                    ecs221
00037                                                                   ecs221
00038      SELECT SORT-FILE       ASSIGN TO SYS001-UT-3380-S-SORTWK1.   ecs221
00040      SELECT file-in         ASSIGN TO SYS010-UT-2400-S-SYS010.    ecs221
00041      SELECT file-out        ASSIGN TO SYS011-UT-2400-S-SYS011.    ecs221
00045                                                                   ecs221
00052                                                                   ecs221
00053  EJECT                                                            ecs221
00054  DATA DIVISION.                                                   ecs221
00055  FILE SECTION.                                                    ecs221
00056                                                                   ecs221
00057  SD  SORT-FILE.                                                   ecs221
00058                                                                   ecs221
00059  01  SORT-REC.                                                    ecs221
00060      12  sort-key                pic x(45).                       ecs221
00061      12  sort-rest               pic x(133).
           12  sort-fill               pic x.
00081                                                                      CL**2
00082  EJECT                                                            ecs221
00086  FD  file-in                                                      ecs221
00087      BLOCK CONTAINS 0 RECORDS
00088      RECORDING MODE IS F.                                         ecs221
00089                                                                   ecs221
00090  01  file-in-rec                 pic x(179).                         CL**4
00094  FD  file-out                                                     ecs221
00095      BLOCK CONTAINS 0 RECORDS
00096      RECORDING MODE IS F.                                         ecs221
00097                                                                   ecs221
00098  01  file-out-rec                PIC X(133).                      ecs221
00099  EJECT                                                            ecs221
00114  EJECT                                                            ecs221
00115  WORKING-STORAGE SECTION.                                         ecs221
00116  77  FILLER  PIC X(32) VALUE '********************************'.  ecs221
00117  77  FILLER  PIC X(32) VALUE '     ecs221 WORKING STORAGE     '.  ecs221
00118  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.005 *********'.     CL**4
00119
00143  01  WS-ABEND-FIELDS.                                             ecs221
00144      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ecs221
00145      12  WS-ZERO                 PIC S9          VALUE ZERO.      ecs221
00146      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ecs221
00147      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ecs221
00148                                                                   ecs221
00207  PROCEDURE DIVISION.                                              ecs221
00237                                                                   ecs221
00238  0120-NOW-SORT.                                                   ecs221
00239                                                                   ecs221
00240      SORT SORT-FILE ASCENDING KEY sort-key                        ecs221
00241          INPUT PROCEDURE 0140-GET-file-in                         ecs221
00242          OUTPUT PROCEDURE 0180-write-file-out.                    ecs221
00243                                                                   ecs221
00244      IF SORT-RETURN NOT = (ZEROS AND 4)                           ecs221
00245          MOVE  0101              TO WS-RETURN-CODE                ecs221
00246          GO TO ABEND-PGM.                                         ecs221
00247
           close file-in file-out
           goback.

00253  0140-GET-file-in     SECTION.                                    ecs221
00254                                                                   ecs221
00255  0150-OPEN-EM.                                                    ecs221
00257      OPEN  INPUT file-in
                 OUTPUT file-out.
00258                                                                   ecs221
00259  0160-R-INPUT.                                                    ecs221
00260                                                                   ecs221
00261      READ file-in at end                                          ecs221
00262          GO TO 0170-E-GET-CLAIMS.                                 ecs221
00292                                                                   ecs221
00293      RELEASE SORT-REC from file-in-rec.                           ecs221
00294      GO TO 0160-R-INPUT.                                          ecs221
00295                                                                   ecs221
00296  0170-E-GET-CLAIMS.                                               ecs221
00297      EXIT.                                                        ecs221
00298  EJECT                                                            ecs221
00299  0180-write-file-out   SECTION.                                   ecs221
00300                                                                   ecs221
00307  0200-R-OLDS.                                                     ecs221
00308                                                                   ecs221
00309      return sort-file AT END                                      ecs221
00311          GO TO 0200-exit.                                         ecs221
00312                                                                   ecs221
00313      write file-out-rec from sort-rest.                           ecs221
00314      go to 0200-r-olds.

00509  0200-exit.
           exit.

00510  ABEND-PGM SECTION.                                               ecs221
00511                                COPY ELCABEND.                     ecs221
00512                                                                      CL**4
