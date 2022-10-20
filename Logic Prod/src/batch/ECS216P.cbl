00001  IDENTIFICATION DIVISION.                                         10/08/97
00002                                                                   ecs216p
00003  PROGRAM-ID.                 ecs216p.                                 LV002
00004 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ecs216p
00005 *                            VMOD=2.005.                             CL**2
00006 *                                                                 ecs216p
00007 *AUTHOR.        LOGIC, INC.                                       ecs216p
00008 *               DALLAS, TEXAS.                                    ecs216p
00009 *                                                                 ecs216p
00010 *DATE-COMPILED.                                                   ecs216p
00011 *                                                                 ecs216p
00012 *SECURITY.   *****************************************************ecs216p
00013 *            *                                                   *ecs216p
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ecs216p
00015 *            *                                                   *ecs216p
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ecs216p
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ecs216p
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ecs216p
00019 *            *                                                   *ecs216p
00020 *            *****************************************************ecs216p
00021 *                                                                 ecs216p
00022 *REMARKS.                                                         ecs216p
00023 *        THIS PROGRAM SORTS THE EPEC FILE INTO THE                ecs216p
00024 *        CORRECT SEQUENCE.                                        ecs216p
00025                                                                   ecs216p
00026  ENVIRONMENT DIVISION.                                            ecs216p
00027  INPUT-OUTPUT SECTION.                                            ecs216p
00028  FILE-CONTROL.                                                    ecs216p
00029                                                                   ecs216p
00030      SELECT  SORT-FILE     ASSIGN TO EXTERNAL SORTWORK.
00031      SELECT  EPEC-IN1      ASSIGN TO SYS010.
00031      SELECT  EPEC-IN2      ASSIGN TO SYS012.                      ecs216p
00032      SELECT  EPEC-OUT      ASSIGN TO SYS011.                      ecs216p
00033  EJECT                                                            ecs216p
00034  DATA DIVISION.                                                   ecs216p
00035  FILE SECTION.                                                    ecs216p
00036                                                                   ecs216p
00037  SD  SORT-FILE                                                    ecs216p
00038      RECORDING MODE F                                             ecs216p
00039      RECORD CONTAINS 325 CHARACTERS.                              ecs216p
00040  01  SORT-WORK-REC.                                               ecs216p
00041      12  SW-REC-TYPE             PIC XX.                          ecs216p
00042      12  FILLER                  PIC XX.                          ecs216p
00043      12  SW-CCSA.                                                 ecs216p
00044          16  SW-CARR             PIC X.                           ecs216p
00045          16  SW-GROUPING         PIC X(6).                        ecs216p
00046          16  SW-STATE            PIC X(2).                        ecs216p
00047          16  SW-ACCOUNT          PIC X(10).                       ecs216p
00048      12  SW-EXPIRATION           PIC X(6).                        ecs216p
00049      12  SW-EFFECTIVE            PIC X(6).                        ecs216p
00050      12  SW-REIN-COMP            PIC X(6).                        ecs216p
00051      12  SW-RCD-TYPE             PIC X.                           ecs216p
00052      12  SW-BEN-TYPE             PIC X(2).                        ecs216p
00053      12  SW-COMM-SEQ-NO          PIC X.                           ecs216p
00054      12  FILLER                  PIC X(273).                      ecs216p
00055      12  SW-PURGE-CODE           PIC X.                           ecs216p
00056      12  SW-RUN-DATE             PIC X(6).                        ecs216p
00057                                                                   ecs216p
00058  FD  EPEC-IN1                                                     ecs216p
00059      RECORDING MODE F                                             ecs216p
00060      LABEL RECORDS STANDARD                                       ecs216p
00061      BLOCK CONTAINS 0 RECORDS
00062      DATA RECORD IS EP-EXTR1.                                     ecs216p
00063  01  EP-EXTR1                PIC X(325).

00058  FD  EPEC-IN2                                                     ecs216p
00059      RECORDING MODE F                                             ecs216p
00060      LABEL RECORDS STANDARD                                       ecs216p
00061      BLOCK CONTAINS 0 RECORDS
00062      DATA RECORD IS EP-EXTR2.                                     ecs216p
00063  01  EP-EXTR2                PIC X(325).                          ecs216p
00064                                                                   ecs216p
00065  FD  EPEC-OUT                                                     ecs216p
00066      RECORDING MODE F                                             ecs216p
00067      LABEL RECORDS STANDARD                                       ecs216p
00068      BLOCK CONTAINS 0 RECORDS
00069      DATA RECORD IS EPEC-RECORD.                                  ecs216p
00070  01  EPEC-RECORD             PIC X(325).                          ecs216p
00071  EJECT                                                            ecs216p
00072  WORKING-STORAGE SECTION.                                         ecs216p
00073  77  FILLER  PIC X(32) VALUE '********************************'.  ecs216p
00074  77  FILLER  PIC X(32) VALUE '   ecs216p   WORKING-STORAGE    '.  ecs216p
00075  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.005. ********'.
       77  WS-STATUS-CODE PIC XX COMP-5.
       77  WS-F-DIR       PIC XXX     VALUE 'F:/'.
00076                                                                   ecs216p
00077  PROCEDURE DIVISION.                                              ecs216p
00078  BEGIN.                                                           ecs216p
00079                                                                   ecs216p
00080 ******************************************************************ecs216p
00081 **  SORT SEQUENCE:                                              **ecs216p
00082 **   CARR/CO/ACCT/EXP/EFF, REIN CO., RUN DATE, L OR A/BEN TYPE, **ecs216p
00083 **          PURGE CODE, EP-CODE, AND SEQ. NO.                   **ecs216p
00084 ******************************************************************ecs216p
pemtst*    call "CBL_CHANGE_DIR"  USING WS-F-DIR
      *           RETURNING WS-STATUS-CODE

00086      SORT SORT-FILE ASCENDING KEY SW-CCSA                         ecs216p
00087                                   SW-EXPIRATION                   ecs216p
00088                                   SW-EFFECTIVE                    ecs216p
00089                                   SW-REIN-COMP                    ecs216p
00090                                   SW-RUN-DATE                     ecs216p
00091                                   SW-RCD-TYPE                     ecs216p
00092                                   SW-BEN-TYPE                     ecs216p
00093                                   SW-PURGE-CODE                   ecs216p
00094                                   SW-REC-TYPE                     ecs216p
00095                                   SW-COMM-SEQ-NO                  ecs216p
00096              USING  EPEC-IN1 EPEC-IN2                             ecs216p
00097              GIVING EPEC-OUT.                                     ecs216p
00098                                                                   ecs216p
00099      GOBACK.                                                      ecs216p
