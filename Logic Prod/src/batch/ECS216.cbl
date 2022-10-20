00001  IDENTIFICATION DIVISION.                                         10/08/97
00002                                                                   ECS216
00003  PROGRAM-ID.                 ECS216.                                 LV002
00004 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ECS216
00005 *                            VMOD=2.005.                             CL**2
00006 *                                                                 ECS216
00007 *AUTHOR.        LOGIC, INC.                                       ECS216
00008 *               DALLAS, TEXAS.                                    ECS216
00009 *                                                                 ECS216
00010 *DATE-COMPILED.                                                   ECS216
00011 *                                                                 ECS216
00012 *SECURITY.   *****************************************************ECS216
00013 *            *                                                   *ECS216
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS216
00015 *            *                                                   *ECS216
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS216
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS216
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS216
00019 *            *                                                   *ECS216
00020 *            *****************************************************ECS216
00021 *                                                                 ECS216
00022 *REMARKS.                                                         ECS216
00023 *        THIS PROGRAM SORTS THE EPEC FILE INTO THE                ECS216
00024 *        CORRECT SEQUENCE.                                        ECS216
00025                                                                   ECS216
00026  ENVIRONMENT DIVISION.                                            ECS216
00027  INPUT-OUTPUT SECTION.                                            ECS216
00028  FILE-CONTROL.                                                    ECS216
00029                                                                   ECS216
00030      SELECT  SORT-FILE     ASSIGN TO EXTERNAL SORTWORK.
00031      SELECT  EPEC-IN       ASSIGN TO SYS010-UT-2400-S-SYS010.     ECS216
00032      SELECT  EPEC-OUT      ASSIGN TO SYS011-UT-2400-S-SYS011.     ECS216
00033  EJECT                                                            ECS216
00034  DATA DIVISION.                                                   ECS216
00035  FILE SECTION.                                                    ECS216
00036                                                                   ECS216
00037  SD  SORT-FILE                                                    ECS216
00038      RECORDING MODE F                                             ECS216
00039      RECORD CONTAINS 325 CHARACTERS.                              ECS216
00040  01  SORT-WORK-REC.                                               ECS216
00041      12  SW-REC-TYPE             PIC XX.                          ECS216
00042      12  FILLER                  PIC XX.                          ECS216
00043      12  SW-CCSA.                                                 ECS216
00044          16  SW-CARR             PIC X.                           ECS216
00045          16  SW-GROUPING         PIC X(6).                        ECS216
00046          16  SW-STATE            PIC X(2).                        ECS216
00047          16  SW-ACCOUNT          PIC X(10).                       ECS216
00048      12  SW-EXPIRATION           PIC X(6).                        ECS216
00049      12  SW-EFFECTIVE            PIC X(6).                        ECS216
00050      12  SW-REIN-COMP            PIC X(6).                        ECS216
00051      12  SW-RCD-TYPE             PIC X.                           ECS216
00052      12  SW-BEN-TYPE             PIC X(2).                        ECS216
00053      12  SW-COMM-SEQ-NO          PIC X.                           ECS216
00054      12  FILLER                  PIC X(273).                      ECS216
00055      12  SW-PURGE-CODE           PIC X.                           ECS216
00056      12  SW-RUN-DATE             PIC X(6).                        ECS216
00057                                                                   ECS216
00058  FD  EPEC-IN                                                      ECS216
00059      RECORDING MODE F                                             ECS216
00060      LABEL RECORDS STANDARD                                       ECS216
00061      BLOCK CONTAINS 0 RECORDS
00062      DATA RECORD IS EP-EXTR.                                      ECS216
00063  01  EP-EXTR                 PIC X(325).                          ECS216
00064                                                                   ECS216
00065  FD  EPEC-OUT                                                     ECS216
00066      RECORDING MODE F                                             ECS216
00067      LABEL RECORDS STANDARD                                       ECS216
00068      BLOCK CONTAINS 0 RECORDS
00069      DATA RECORD IS EPEC-RECORD.                                  ECS216
00070  01  EPEC-RECORD             PIC X(325).                          ECS216
00071  EJECT                                                            ECS216
00072  WORKING-STORAGE SECTION.                                         ECS216
00073  77  FILLER  PIC X(32) VALUE '********************************'.  ECS216
00074  77  FILLER  PIC X(32) VALUE '   ECS216   WORKING-STORAGE     '.  ECS216
00075  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.005. ********'.
       77  WS-STATUS-CODE PIC XX COMP-5.
       77  WS-F-DIR       PIC XXX     VALUE 'F:/'.
00076                                                                   ECS216
00077  PROCEDURE DIVISION.                                              ECS216
00078  BEGIN.                                                           ECS216
00079                                                                   ECS216
00080 ******************************************************************ECS216
00081 **  SORT SEQUENCE:                                              **ECS216
00082 **   CARR/CO/ACCT/EXP/EFF, REIN CO., RUN DATE, L OR A/BEN TYPE, **ECS216
00083 **          PURGE CODE, EP-CODE, AND SEQ. NO.                   **ECS216
00084 ******************************************************************ECS216
pemtst*    call "CBL_CHANGE_DIR"  USING WS-F-DIR
      *           RETURNING WS-STATUS-CODE

00086      SORT SORT-FILE ASCENDING KEY SW-CCSA                         ECS216
00087                                   SW-EXPIRATION                   ECS216
00088                                   SW-EFFECTIVE                    ECS216
00089                                   SW-REIN-COMP                    ECS216
00090                                   SW-RUN-DATE                     ECS216
00091                                   SW-RCD-TYPE                     ECS216
00092                                   SW-BEN-TYPE                     ECS216
00093                                   SW-PURGE-CODE                   ECS216
00094                                   SW-REC-TYPE                     ECS216
00095                                   SW-COMM-SEQ-NO                  ECS216
00096              USING  EPEC-IN                                       ECS216
00097              GIVING EPEC-OUT.                                     ECS216
00098                                                                   ECS216
00099      GOBACK.                                                      ECS216
