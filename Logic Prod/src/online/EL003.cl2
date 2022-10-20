00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL003
00003  PROGRAM-ID.                 EL003 .                                 LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/12/96 09:22:08.                    CL**2
00007 *                            VMOD=2.002                              CL**2
00008 *                                                                 EL003
00008 *                                                                 EL003
00009 *AUTHOR.           LOGIC,INC.                                        CL**2
00010 *                  DALLAS,TEXAS.                                     CL**2
00011                                                                   EL003
00012 *DATE-COMPILED.                                                      CL**2
00013 *SECURITY.   *****************************************************   CL**2
00014 *            *                                                   *   CL**2
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00016 *            *                                                   *   CL**2
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00020 *            *                                                   *   CL**2
00021 *            *****************************************************   CL**2
00022                                                                   EL003
00023 *REMARKS. COMMON  SECURITY VIOLATION ROUTINE.                        CL**2
00024 *        THIS PROGRAM IS LINK'D TO WHENEVER A SECURITY VIOLATION     CL**2
00025 *        HAS OCCURED WITHIN THE CLAS-IC SYSTEM.                      CL**2
00026                                                                   EL003
00027                                                                   EL003
00028      EJECT                                                        EL003
00029  ENVIRONMENT DIVISION.                                            EL003
00030                                                                   EL003
00031  DATA DIVISION.                                                   EL003
00032                                                                   EL003
00033  WORKING-STORAGE SECTION.                                         EL003
00034                                                                   EL003
00035  77  FILLER  PIC X(32)  VALUE '********************************'. EL003
00036  77  FILLER  PIC X(32)  VALUE '*    EL003 WORKING STORAGE     *'. EL003
00037  77  FILLER  PIC X(32)  VALUE '********* V/M 2.002 ************'.    CL**2
00038                                                                   EL003
00039      COPY ELCSCTM.                                                   CL**2
00040                                                                      CL**2
00041      12  FILLER                 PIC X(10) VALUE '  ********'.        CL**2
00042                                                                   EL003
00043  LINKAGE SECTION.                                                 EL003
00044                                                                   EL003
00045  01  DFHCOMMAREA                PIC X(80).                        EL003
00046                                                                   EL003
00047      EJECT                                                        EL003
00048  PROCEDURE DIVISION.                                              EL003
00049                                                                   EL003
00050      MOVE DFHCOMMAREA   TO SECURITY-MESSAGE.                         CL**2
00051                                                                   EL003
00052      EXEC CICS HANDLE CONDITION                                   EL003
00053           QIDERR  (0100-RETURN)                                   EL003
00054           NOTOPEN (0100-RETURN)                                   EL003
00055           NOSPACE (0100-RETURN)                                   EL003
00056           END-EXEC.                                               EL003
00057                                                                   EL003
00058      EXEC CICS WRITEQ TD                                          EL003
00059           QUEUE   ('CSMT')                                        EL003
00060           FROM    (SECURITY-MESSAGE)                              EL003
00061           LENGTH  (90)                                            EL003
00062           END-EXEC.                                               EL003
00063                                                                   EL003
00064  0100-RETURN.                                                     EL003
00065      EXEC CICS RETURN END-EXEC.                                   EL003
00066      GOBACK.                                                      EL003
