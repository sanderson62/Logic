00001 ******************************************************************04/15/98
00002 *                                                                *ELCBENE
00003 *                            ELCBENE.                            *   LV002
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
00005 *                            VMOD=2.006                          *   CL**2
00006 *                                                                *ELCBENE
00007 *   FILE DESCRIPTION = BENEFICIARY FILE                          *ELCBENE
00008 *                                                                *ELCBENE
00009 *   FILE TYPE = VSAM,KSDS                                        *ELCBENE
00010 *   RECORD SIZE = 500   RECFORM = FIX                            *ELCBENE
00011 *                                                                *ELCBENE
00012 *   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *ELCBENE
00013 *     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *ELCBENE
00014 *                                                                *ELCBENE
00015 *   LOG = YES                                                    *ELCBENE
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *ELCBENE
00017 *                                                                *ELCBENE
CIDMOD*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *ELCBENE
00018 ******************************************************************ELCBENE
00019                                                                   ELCBENE
00020  01  BENEFICIARY-MASTER.                                          ELCBENE
00021      12  BE-RECORD-ID                PIC XX.                      ELCBENE
00022          88  VALID-BE-ID                VALUE 'BE'.               ELCBENE
00023                                                                   ELCBENE
00024      12  BE-CONTROL-PRIMARY.                                      ELCBENE
00025          16  BE-COMPANY-CD           PIC X.                       ELCBENE
00026          16  BE-RECORD-TYPE          PIC X.                       ELCBENE
00027              88  BENEFICIARY-RECORD  VALUE 'B'.                   ELCBENE
00028              88  ADJUSTOR-RECORD     VALUE 'A'.                   ELCBENE
00029          16  BE-BENEFICIARY          PIC X(10).                   ELCBENE
00030      12  BE-CONTROL-BY-NAME.                                      ELCBENE
00031          16  BE-COMPANY-CD-A1        PIC X.                       ELCBENE
00032          16  BE-RECORD-TYPE-A1       PIC X.                       ELCBENE
00033          16  BE-MAIL-TO-NAME-A1      PIC X(30).                   ELCBENE
00034          16  BE-ALTERNATE-PRIME-A1   PIC X(10).                   ELCBENE
00035                                                                   ELCBENE
00036      12  BE-LAST-MAINT-DT            PIC XX.                      ELCBENE
00037      12  BE-LAST-MAINT-BY            PIC X(4).                    ELCBENE
00038      12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.        ELCBENE
00039                                                                   ELCBENE
00040      12  BE-ADDRESS-DATA.                                         ELCBENE
00041          16  BE-MAIL-TO-NAME         PIC X(30).                   ELCBENE
00042          16  BE-ADDRESS-LINE-1       PIC X(30).                   ELCBENE
00043          16  BE-ADDRESS-LINE-2       PIC X(30).                   ELCBENE
00044          16  BE-ADDRESS-LINE-3       PIC X(30).                   ELCBENE
00045          16  BE-CITY-STATE           PIC X(30).                   ELCBENE
00046          16  BE-ZIP-CODE.                                         ELCBENE
00047              20  BE-ZIP-PRIME.                                    ELCBENE
00048                  24  BE-ZIP-1ST      PIC X.                       ELCBENE
00049                      88  BE-CANADIAN-POST-CODE                    ELCBENE
00050                                          VALUE 'A' THRU 'Z'.      ELCBENE
00051                  24  FILLER          PIC X(4).                    ELCBENE
00052              20  BE-ZIP-PLUS4        PIC X(4).                    ELCBENE
00053          16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.     ELCBENE
00054              20  BE-CAN-POSTAL-1     PIC XXX.                     ELCBENE
00055              20  BE-CAN-POSTAL-2     PIC XXX.                     ELCBENE
00056              20  FILLER              PIC XXX.                     ELCBENE
00057          16  BE-PHONE-NO             PIC 9(11)     COMP-3.        ELCBENE
00058          16  BE-GROUP-CHECKS-Y-N     PIC X.                       ELCBENE
00059                                                                   ELCBENE
00060 ******************************************************************ELCBENE
00061 *    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *ELCBENE
00062 *    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *ELCBENE
00063 ******************************************************************ELCBENE
00064      12  BE-CARRIER                  PIC X.                       ELCBENE
00065                                                                   ELCBENE
00066      12  BE-ADDRESS-DATA2.                                        ELCBENE
00067          16  BE-MAIL-TO-NAME2        PIC X(30).                   ELCBENE
00068          16  BE-ADDRESS-LINE-12      PIC X(30).                   ELCBENE
00069          16  BE-ADDRESS-LINE-22      PIC X(30).                   ELCBENE
00070          16  BE-ADDRESS-LINE-32      PIC X(30).                   ELCBENE
00071          16  BE-CITY-STATE2          PIC X(30).                   ELCBENE
00072          16  BE-ZIP-CODE2.                                        ELCBENE
00073              20  BE-ZIP-PRIME2.                                   ELCBENE
00074                  24  BE-ZIP-1ST2     PIC X.                       ELCBENE
00075                      88  BE-CANADIAN-POST-CODE2                   ELCBENE
00076                                          VALUE 'A' THRU 'Z'.      ELCBENE
00077                  24  FILLER          PIC X(4).                    ELCBENE
00078              20  BE-ZIP-PLUS42       PIC X(4).                    ELCBENE
00079          16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.    ELCBENE
00080              20  BE-CAN-POSTAL-12    PIC XXX.                     ELCBENE
00081              20  BE-CAN-POSTAL-22    PIC XXX.                     ELCBENE
00082              20  FILLER              PIC XXX.                     ELCBENE
00083          16  BE-PHONE-NO2            PIC 9(11)     COMP-3.        ELCBENE
00084          16  BE-BILLING-STMT-DATA.                                ELCBENE
00085              20  BE-BSR              PIC X.                       ELCBENE
00086                  88  BE-AUTOMATED-BSR      VALUE 'A'.             ELCBENE
00087                  88  BE-NOT-AUTOMATED-BSR  VALUE ' '.             ELCBENE
00088              20  BE-BSR-FROM         PIC X(30).                   ELCBENE
00089              20  BE-BSR-DEPT         PIC X(30).                   ELCBENE
00090              20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.        ELCBENE
00091              20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.        ELCBENE
00092              20  BE-OUTPUT-TYPE      PIC X.                       ELCBENE
00093                  88  BE-FAX-OUTPUT         VALUE 'F'.             ELCBENE
00094                  88  BE-PRINT-OUTPUT       VALUE 'P' ' '.         ELCBENE
00095                                                                   ELCBENE
00096      12  FILLER                      PIC X(28).                   ELCBENE
00097 ******************************************************************ELCBENE
