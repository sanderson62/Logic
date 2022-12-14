00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6522.
00009 *AUTHOR.                     SUZAN VUKOV.
00022 *
00023 *REMARKS.     TRANSACTION - EXDC - COMPENSATION NOTE MAINTENANCE.
00022 *
022803******************************************************************
022803*                   C H A N G E   L O G
022803*
022803* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
022803*-----------------------------------------------------------------
022803*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
022803* EFFECTIVE    NUMBER
022803*-----------------------------------------------------------------
022803* 022803    2002032200002  SMVA  NEW ONLINE PROGRAM
021511* 021511  IR2011021400003  AJRA  FIX ACCOUNT NAME
022803******************************************************************
00025  ENVIRONMENT DIVISION.
00026
00027      EJECT
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*     EL6522 WORKING STORAGE   *'.
00032  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'.
00033
00034 *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00035 *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00036
00037      EJECT
00038  01  STANDARD-AREAS.
00039      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00040      12  MAP-NAME                    PIC X(8)    VALUE 'EL6522A'.
00041      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6522S '.
00042      12  SCRN-NUMBER                 PIC X(4)    VALUE '652C'.
00043      12  TRANS-ID                    PIC X(4)    VALUE 'EXDC'.
00044      12  W-PRINT-TRANS               PIC X(4)    VALUE 'EXDD'.
00045      12  THIS-PGM                    PIC X(8)    VALUE 'EL6522'.
00046      12  PGM-NAME                    PIC X(8).
00047      12  PGM-EL652                   PIC X(8)    VALUE 'EL652'.
00048
00049      12  ERCONT-FILE-ID              PIC X(8)    VALUE 'ERCONT'.
00050      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
00051      12  QID.
00052          16  QID-TERM                PIC X(4).
00053          16  FILLER                  PIC X(4)    VALUE '652C'.
00054      12  QID-ITEM                    PIC S9(4)   COMP VALUE +0.
00055      12  SC-ITEM                     PIC S9(4)   COMP VALUE +1.
00056
00057  01  WORK-AREA.
00058      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00059      12  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
00060
00061      12  FN-CONVERT                  PIC XX.
00062      12  FN-BIN REDEFINES FN-CONVERT PIC S9(4)  COMP.
00063
00064      12  ERCONT-LENGTH               PIC S9(4)   COMP VALUE +128.
00065      12  ERCONT-KEY-LENGTH           PIC S9(4)   COMP VALUE +31.
00066      12  ERCONT-START-LENGTH         PIC S9(4)   COMP VALUE +29.
00067      12  ERCONT-KEY.
00068          16  ERCONT-PARTIAL-KEY.
00069              20 ERCONT-COMPANY-CD    PIC X.
00071              20 ERCONT-CARRIER       PIC X.
00072              20 ERCONT-GROUPING      PIC X(06).
00073              20 ERCONT-FINRESP       PIC X(10).
00074              20 ERCONT-ACCOUNT       PIC X(10).
                   20 ERCONT-REC-TYP       PIC X(01).
00075          16 ERCONT-SEQ               PIC S9(4) COMP.
00076      12  SV-PRIOR-KEY.
00077          20 SV-COMPANY-CD            PIC X.
00079          20 SV-CARRIER               PIC X.
00080          20 SV-GROUPING              PIC X(06).
00081          20 SV-FINRESP               PIC X(10).
00081          20 SV-ACCOUNT               PIC X(10).
00078          20 SV-REC-TYP               PIC X.
00083          20 SV-SEQ                   PIC S9(4) COMP.
00094      12  ELCNTL-KEY.
00095          16  ELCNTL-COMPANY          PIC XXX.
00096          16  ELCNTL-REC-TYPE         PIC X.
00097          16  FILLER                  PIC X(4).
00098          16  ELCNTL-SEQ-NO           PIC S9(4)   COMP.
00099      12  TIME-IN                     PIC S9(7).
00100      12  TIME-SPLIT REDEFINES TIME-IN.
00101          16  FILLER                  PIC X.
00102          16  TIME-OUT                PIC 99V99.
00103          16  FILLER                  PIC 9(2).
00104      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00105      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
00106      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00107      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00108      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00109      12  MAX-LINES                   PIC 9(03)   VALUE 300.
00110      12  NUM-LINES-PER-SCREEN        PIC 9(02)   VALUE 12.
00111      12  TS-NUM-REC-IN-GROUP         PIC 9(02)   VALUE 50.
00112      12  TS-GROUP-WORK               PIC 9(5)    VALUE 0  COMP-3.
00113      12  TS-LENGTH                   PIC S9(4)   VALUE +3600 COMP.
00114      12  ROLL-COUNTER                PIC S999    VALUE +0 COMP-3.
00115      12  TEMP-CURR-LINE              PIC S9(3)   COMP-3.
00116      EJECT
00117  01  ERROR-MESSAGES.
00118      12  ER-0000                     PIC X(04)       VALUE '0000'.
00119      12  ER-0004                     PIC X(04)       VALUE '0004'.
00120      12  ER-0006                     PIC X(04)       VALUE '0006'.
00121      12  ER-0008                     PIC X(04)       VALUE '0008'.
00122      12  ER-0023                     PIC X(04)       VALUE '0023'.
00123      12  ER-0029                     PIC X(04)       VALUE '0029'.
00124      12  ER-0030                     PIC X(04)       VALUE '0030'.
00125      12  ER-0031                     PIC X(04)       VALUE '0031'.
00126      12  ER-0032                     PIC X(04)       VALUE '0032'.
00127      12  ER-0033                     PIC X(04)       VALUE '0033'.
00128      12  ER-0041                     PIC X(04)       VALUE '0041'.
00129      12  ER-0044                     PIC X(04)       VALUE '0044'.
00130      12  ER-0045                     PIC X(04)       VALUE '0045'.
00131      12  ER-0047                     PIC X(04)       VALUE '0047'.
00132      12  ER-0048                     PIC X(04)       VALUE '0048'.
00133      12  ER-0049                     PIC X(04)       VALUE '0049'.
00134      12  ER-0050                     PIC X(04)       VALUE '0050'.
00135      12  ER-0051                     PIC X(04)       VALUE '0051'.
00136      12  ER-0066                     PIC X(04)       VALUE '0066'.
00137      12  ER-0067                     PIC X(04)       VALUE '0067'.
00138      12  ER-0069                     PIC X(04)       VALUE '0069'.
00139      12  ER-0070                     PIC X(04)       VALUE '0070'.
00140      12  ER-0140                     PIC X(04)       VALUE '0140'.
00141      12  ER-0412                     PIC X(04)       VALUE '0412'.
00142      12  ER-0413                     PIC X(04)       VALUE '0413'.
00143      12  ER-3784                     PIC X(04)       VALUE '3784'.
00144      12  ER-3786                     PIC X(04)       VALUE '3786'.
00145      12  ER-3787                     PIC X(04)       VALUE '3787'.
00146      12  ER-3788                     PIC X(04)       VALUE '3788'.
00147      EJECT
00148 *                       COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00149      EJECT
00150 *                       COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00151  01  FILLER  REDEFINES DFHAID.
00152      12  FILLER                      PIC X(8).
00153      12  PF-VALUES OCCURS 24 TIMES   PIC X.
00154      EJECT
00155 *                       COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00156      EJECT
00157 *                       COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
00131      12  FILLER                          PIC X(4).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
           12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
               16  PI-CHECK-MAINT-TYPE     PIC X.
                   88  VALID-MAINT-TYPE            VALUE 'S' 'A'
                                                         'C' 'D'.
                   88  ADD-FUNCTION                VALUE 'A'.
                   88  SHOW-FUNCTION               VALUE 'S'.
                   88  DELETE-FUNCTION             VALUE 'D'.
                   88  CHANGE-FUNCTION             VALUE 'C'.
               16  PI-CHECK-TYPE           PIC X.
                   88  VALID-TYPE                  VALUE 'A' 'G'
                                                         'C'.
               16  PI-CHECK-CARRY-BAL      PIC X.
                   88  VALID-CARRY-BAL             VALUE 'Y' 'N'.
               16  PI-FIRST-TIME-SW        PIC X.
               16  PI-ERCOMP-EOF-SW        PIC X.
                   88  ERCOMP-EOF                  VALUE 'Y'.
               16  PI-SAVE-PHONE           PIC X(10).
               16  PI-SAVE-PHONE-RED REDEFINES
                                     PI-SAVE-PHONE  PIC 9(10).
               16  PI-ERC-END-BAL          PIC S9(9)V99   COMP-3.
               16  PI-ERCOMP-KEY.
                   20  PI-ERC-COMPANY-CD   PIC X.
                   20  PI-ERC-CARRIER      PIC X.
                   20  PI-ERC-GROUP        PIC X(6).
                   20  PI-ERC-RESP         PIC X(10).
                   20  PI-ERC-ACCT         PIC X(10).
                   20  PI-ERC-TYPE         PIC X.
               16  PI-SAVE-ERCOMP-KEY      PIC X(29).
               16  PI-SAVE-FAXNO           PIC X(10).
               16  PI-SAVE-FAXNO-RED REDEFINES
                                     PI-SAVE-FAXNO  PIC 9(10).
               16  PI-SAVE-ADDR2           PIC X(30).
021511         16  PI-SAVE-CITYST.
021511             20 PI-SAVE-CITY         PIC X(28).
021511             20  PI-SAVE-STATE       PIC XX.
               16  PI-SAVE-ACCT-NAME       PIC X(30).
               16  PI-EL652-DEL-SW         PIC X.
               16  PI-UPDATE-SW            PIC X.
                   88  PI-CHANGES-MADE                 VALUE '1'.
               16  PI-TOTAL-LINES          PIC S99.
               16  PI-CURRENT-LINE         PIC S9(3)   COMP-3.
               16  PI-TEMP-STOR-ITEMS      PIC S9(4)   COMP.
021511         16  FILLER                  PIC X(453).
00161 *                       COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00162      EJECT
00163 *                       COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00164      EJECT
00165 *                       COPY EL6522S.
       01  EL6522AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  FINRESPL PIC S9(0004) COMP.
           05  FINRESPF PIC  X(0001).
           05  FILLER REDEFINES FINRESPF.
               10  FINRESPA PIC  X(0001).
           05  FINRESPI PIC  X(0010).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  NAMEL PIC S9(0004) COMP.
           05  NAMEF PIC  X(0001).
           05  FILLER REDEFINES NAMEF.
               10  NAMEA PIC  X(0001).
           05  NAMEI PIC  X(0030).
      *    -------------------------------
           05  TOTL PIC S9(0004) COMP.
           05  TOTF PIC  X(0001).
           05  FILLER REDEFINES TOTF.
               10  TOTA PIC  X(0001).
           05  TOTI PIC  999.
      *    -------------------------------
           05  L1L PIC S9(0004) COMP.
           05  L1F PIC  X(0001).
           05  FILLER REDEFINES L1F.
               10  L1A PIC  X(0001).
           05  L1I PIC  X(0003).
      *    -------------------------------
           05  TEXT1L PIC S9(0004) COMP.
           05  TEXT1F PIC  X(0001).
           05  FILLER REDEFINES TEXT1F.
               10  TEXT1A PIC  X(0001).
           05  TEXT1I PIC  X(0060).
      *    -------------------------------
           05  MTBY1L PIC S9(0004) COMP.
           05  MTBY1F PIC  X(0001).
           05  FILLER REDEFINES MTBY1F.
               10  MTBY1A PIC  X(0001).
           05  MTBY1I PIC  X(0004).
      *    -------------------------------
           05  MTDT1L PIC S9(0004) COMP.
           05  MTDT1F PIC  X(0001).
           05  FILLER REDEFINES MTDT1F.
               10  MTDT1A PIC  X(0001).
           05  MTDT1I PIC  X(0006).
      *    -------------------------------
           05  L2L PIC S9(0004) COMP.
           05  L2F PIC  X(0001).
           05  FILLER REDEFINES L2F.
               10  L2A PIC  X(0001).
           05  L2I PIC  X(0003).
      *    -------------------------------
           05  TEXT2L PIC S9(0004) COMP.
           05  TEXT2F PIC  X(0001).
           05  FILLER REDEFINES TEXT2F.
               10  TEXT2A PIC  X(0001).
           05  TEXT2I PIC  X(0060).
      *    -------------------------------
           05  MTBY2L PIC S9(0004) COMP.
           05  MTBY2F PIC  X(0001).
           05  FILLER REDEFINES MTBY2F.
               10  MTBY2A PIC  X(0001).
           05  MTBY2I PIC  X(0004).
      *    -------------------------------
           05  MTDT2L PIC S9(0004) COMP.
           05  MTDT2F PIC  X(0001).
           05  FILLER REDEFINES MTDT2F.
               10  MTDT2A PIC  X(0001).
           05  MTDT2I PIC  X(0006).
      *    -------------------------------
           05  L3L PIC S9(0004) COMP.
           05  L3F PIC  X(0001).
           05  FILLER REDEFINES L3F.
               10  L3A PIC  X(0001).
           05  L3I PIC  X(0003).
      *    -------------------------------
           05  TEXT3L PIC S9(0004) COMP.
           05  TEXT3F PIC  X(0001).
           05  FILLER REDEFINES TEXT3F.
               10  TEXT3A PIC  X(0001).
           05  TEXT3I PIC  X(0060).
      *    -------------------------------
           05  MTBY3L PIC S9(0004) COMP.
           05  MTBY3F PIC  X(0001).
           05  FILLER REDEFINES MTBY3F.
               10  MTBY3A PIC  X(0001).
           05  MTBY3I PIC  X(0004).
      *    -------------------------------
           05  MTDT3L PIC S9(0004) COMP.
           05  MTDT3F PIC  X(0001).
           05  FILLER REDEFINES MTDT3F.
               10  MTDT3A PIC  X(0001).
           05  MTDT3I PIC  X(0006).
      *    -------------------------------
           05  L4L PIC S9(0004) COMP.
           05  L4F PIC  X(0001).
           05  FILLER REDEFINES L4F.
               10  L4A PIC  X(0001).
           05  L4I PIC  X(0003).
      *    -------------------------------
           05  TEXT4L PIC S9(0004) COMP.
           05  TEXT4F PIC  X(0001).
           05  FILLER REDEFINES TEXT4F.
               10  TEXT4A PIC  X(0001).
           05  TEXT4I PIC  X(0060).
      *    -------------------------------
           05  MTBY4L PIC S9(0004) COMP.
           05  MTBY4F PIC  X(0001).
           05  FILLER REDEFINES MTBY4F.
               10  MTBY4A PIC  X(0001).
           05  MTBY4I PIC  X(0004).
      *    -------------------------------
           05  MTDT4L PIC S9(0004) COMP.
           05  MTDT4F PIC  X(0001).
           05  FILLER REDEFINES MTDT4F.
               10  MTDT4A PIC  X(0001).
           05  MTDT4I PIC  X(0006).
      *    -------------------------------
           05  L5L PIC S9(0004) COMP.
           05  L5F PIC  X(0001).
           05  FILLER REDEFINES L5F.
               10  L5A PIC  X(0001).
           05  L5I PIC  X(0003).
      *    -------------------------------
           05  TEXT5L PIC S9(0004) COMP.
           05  TEXT5F PIC  X(0001).
           05  FILLER REDEFINES TEXT5F.
               10  TEXT5A PIC  X(0001).
           05  TEXT5I PIC  X(0060).
      *    -------------------------------
           05  MTBY5L PIC S9(0004) COMP.
           05  MTBY5F PIC  X(0001).
           05  FILLER REDEFINES MTBY5F.
               10  MTBY5A PIC  X(0001).
           05  MTBY5I PIC  X(0004).
      *    -------------------------------
           05  MTDT5L PIC S9(0004) COMP.
           05  MTDT5F PIC  X(0001).
           05  FILLER REDEFINES MTDT5F.
               10  MTDT5A PIC  X(0001).
           05  MTDT5I PIC  X(0006).
      *    -------------------------------
           05  L6L PIC S9(0004) COMP.
           05  L6F PIC  X(0001).
           05  FILLER REDEFINES L6F.
               10  L6A PIC  X(0001).
           05  L6I PIC  X(0003).
      *    -------------------------------
           05  TEXT6L PIC S9(0004) COMP.
           05  TEXT6F PIC  X(0001).
           05  FILLER REDEFINES TEXT6F.
               10  TEXT6A PIC  X(0001).
           05  TEXT6I PIC  X(0060).
      *    -------------------------------
           05  MTBY6L PIC S9(0004) COMP.
           05  MTBY6F PIC  X(0001).
           05  FILLER REDEFINES MTBY6F.
               10  MTBY6A PIC  X(0001).
           05  MTBY6I PIC  X(0004).
      *    -------------------------------
           05  MTDT6L PIC S9(0004) COMP.
           05  MTDT6F PIC  X(0001).
           05  FILLER REDEFINES MTDT6F.
               10  MTDT6A PIC  X(0001).
           05  MTDT6I PIC  X(0006).
      *    -------------------------------
           05  L7L PIC S9(0004) COMP.
           05  L7F PIC  X(0001).
           05  FILLER REDEFINES L7F.
               10  L7A PIC  X(0001).
           05  L7I PIC  X(0003).
      *    -------------------------------
           05  TEXT7L PIC S9(0004) COMP.
           05  TEXT7F PIC  X(0001).
           05  FILLER REDEFINES TEXT7F.
               10  TEXT7A PIC  X(0001).
           05  TEXT7I PIC  X(0060).
      *    -------------------------------
           05  MTBY7L PIC S9(0004) COMP.
           05  MTBY7F PIC  X(0001).
           05  FILLER REDEFINES MTBY7F.
               10  MTBY7A PIC  X(0001).
           05  MTBY7I PIC  X(0004).
      *    -------------------------------
           05  MTDT7L PIC S9(0004) COMP.
           05  MTDT7F PIC  X(0001).
           05  FILLER REDEFINES MTDT7F.
               10  MTDT7A PIC  X(0001).
           05  MTDT7I PIC  X(0006).
      *    -------------------------------
           05  L8L PIC S9(0004) COMP.
           05  L8F PIC  X(0001).
           05  FILLER REDEFINES L8F.
               10  L8A PIC  X(0001).
           05  L8I PIC  X(0003).
      *    -------------------------------
           05  TEXT8L PIC S9(0004) COMP.
           05  TEXT8F PIC  X(0001).
           05  FILLER REDEFINES TEXT8F.
               10  TEXT8A PIC  X(0001).
           05  TEXT8I PIC  X(0060).
      *    -------------------------------
           05  MTBY8L PIC S9(0004) COMP.
           05  MTBY8F PIC  X(0001).
           05  FILLER REDEFINES MTBY8F.
               10  MTBY8A PIC  X(0001).
           05  MTBY8I PIC  X(0004).
      *    -------------------------------
           05  MTDT8L PIC S9(0004) COMP.
           05  MTDT8F PIC  X(0001).
           05  FILLER REDEFINES MTDT8F.
               10  MTDT8A PIC  X(0001).
           05  MTDT8I PIC  X(0006).
      *    -------------------------------
           05  L9L PIC S9(0004) COMP.
           05  L9F PIC  X(0001).
           05  FILLER REDEFINES L9F.
               10  L9A PIC  X(0001).
           05  L9I PIC  X(0003).
      *    -------------------------------
           05  TEXT9L PIC S9(0004) COMP.
           05  TEXT9F PIC  X(0001).
           05  FILLER REDEFINES TEXT9F.
               10  TEXT9A PIC  X(0001).
           05  TEXT9I PIC  X(0060).
      *    -------------------------------
           05  MTBY9L PIC S9(0004) COMP.
           05  MTBY9F PIC  X(0001).
           05  FILLER REDEFINES MTBY9F.
               10  MTBY9A PIC  X(0001).
           05  MTBY9I PIC  X(0004).
      *    -------------------------------
           05  MTDT9L PIC S9(0004) COMP.
           05  MTDT9F PIC  X(0001).
           05  FILLER REDEFINES MTDT9F.
               10  MTDT9A PIC  X(0001).
           05  MTDT9I PIC  X(0006).
      *    -------------------------------
           05  L10L PIC S9(0004) COMP.
           05  L10F PIC  X(0001).
           05  FILLER REDEFINES L10F.
               10  L10A PIC  X(0001).
           05  L10I PIC  X(0003).
      *    -------------------------------
           05  TEXT10L PIC S9(0004) COMP.
           05  TEXT10F PIC  X(0001).
           05  FILLER REDEFINES TEXT10F.
               10  TEXT10A PIC  X(0001).
           05  TEXT10I PIC  X(0060).
      *    -------------------------------
           05  MTBY10L PIC S9(0004) COMP.
           05  MTBY10F PIC  X(0001).
           05  FILLER REDEFINES MTBY10F.
               10  MTBY10A PIC  X(0001).
           05  MTBY10I PIC  X(0004).
      *    -------------------------------
           05  MTDT10L PIC S9(0004) COMP.
           05  MTDT10F PIC  X(0001).
           05  FILLER REDEFINES MTDT10F.
               10  MTDT10A PIC  X(0001).
           05  MTDT10I PIC  X(0006).
      *    -------------------------------
           05  L11L PIC S9(0004) COMP.
           05  L11F PIC  X(0001).
           05  FILLER REDEFINES L11F.
               10  L11A PIC  X(0001).
           05  L11I PIC  X(0003).
      *    -------------------------------
           05  TEXT11L PIC S9(0004) COMP.
           05  TEXT11F PIC  X(0001).
           05  FILLER REDEFINES TEXT11F.
               10  TEXT11A PIC  X(0001).
           05  TEXT11I PIC  X(0060).
      *    -------------------------------
           05  MTBY11L PIC S9(0004) COMP.
           05  MTBY11F PIC  X(0001).
           05  FILLER REDEFINES MTBY11F.
               10  MTBY11A PIC  X(0001).
           05  MTBY11I PIC  X(0004).
      *    -------------------------------
           05  MTDT11L PIC S9(0004) COMP.
           05  MTDT11F PIC  X(0001).
           05  FILLER REDEFINES MTDT11F.
               10  MTDT11A PIC  X(0001).
           05  MTDT11I PIC  X(0006).
      *    -------------------------------
           05  L12L PIC S9(0004) COMP.
           05  L12F PIC  X(0001).
           05  FILLER REDEFINES L12F.
               10  L12A PIC  X(0001).
           05  L12I PIC  X(0003).
      *    -------------------------------
           05  TEXT12L PIC S9(0004) COMP.
           05  TEXT12F PIC  X(0001).
           05  FILLER REDEFINES TEXT12F.
               10  TEXT12A PIC  X(0001).
           05  TEXT12I PIC  X(0060).
      *    -------------------------------
           05  MTBY12L PIC S9(0004) COMP.
           05  MTBY12F PIC  X(0001).
           05  FILLER REDEFINES MTBY12F.
               10  MTBY12A PIC  X(0001).
           05  MTBY12I PIC  X(0004).
      *    -------------------------------
           05  MTDT12L PIC S9(0004) COMP.
           05  MTDT12F PIC  X(0001).
           05  FILLER REDEFINES MTDT12F.
               10  MTDT12A PIC  X(0001).
           05  MTDT12I PIC  X(0006).
      *    -------------------------------
           05  ERRMSGBL PIC S9(0004) COMP.
           05  ERRMSGBF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGBF.
               10  ERRMSGBA PIC  X(0001).
           05  ERRMSGBI PIC  X(0072).
      *    -------------------------------
           05  FUNCTL PIC S9(0004) COMP.
           05  FUNCTF PIC  X(0001).
           05  FILLER REDEFINES FUNCTF.
               10  FUNCTA PIC  X(0001).
           05  FUNCTI PIC  X(0001).
      *    -------------------------------
           05  LINE1L PIC S9(0004) COMP.
           05  LINE1F PIC  X(0001).
           05  FILLER REDEFINES LINE1F.
               10  LINE1A PIC  X(0001).
           05  LINE1I PIC  999.
      *    -------------------------------
           05  LINE2L PIC S9(0004) COMP.
           05  LINE2F PIC  X(0001).
           05  FILLER REDEFINES LINE2F.
               10  LINE2A PIC  X(0001).
           05  LINE2I PIC  999.
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
       01  EL6522AO REDEFINES EL6522AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT3O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT4O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT5O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT6O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT7O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT8O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT9O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY9O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT10O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT11O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT12O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTBY12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTDT12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGBO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FUNCTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00166      EJECT
00167  01  EL6522R REDEFINES EL6522AI.
022803     12  FILLER                      PIC X(126).
00169      12  SC-ALL-LINES.
00170       14 SC-LINES OCCURS 12 TIMES INDEXED BY SC-INDX.
00171          16  SC-LINL                 PIC S9(4)   COMP.
00172          16  SC-LINA                 PIC X.
00173          16  SC-LIN                  PIC Z99.
00174          16  SC-TEXTL                PIC S9(4)   COMP.
00175          16  SC-TEXTA                PIC X.
00176          16  SC-TEXT                 PIC X(60).
00177          16  SC-MAINT-BYL            PIC S9(4)   COMP.
00178          16  SC-MAINT-BYA            PIC X.
00179          16  SC-MAINT-BY             PIC XXXX.
00180          16  SC-MAINT-DTL            PIC S9(4)   COMP.
00181          16  SC-MAINT-DTA            PIC X.
00182          16  SC-MAINT-DT             PIC X(6).
00183      12  FILLER                      PIC X(23).
00184      EJECT
00185  01  RECORD-TABLE                    PIC X(21600) VALUE SPACES.
00186  01  REC-TABLE  REDEFINES RECORD-TABLE.
00187      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3600).
00188  01  REC-ENTRIES REDEFINES RECORD-TABLE.
00189      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
00190          16  REC-TEXT                PIC X(60).
00191          16  REC-LAST-MAINT-BY       PIC XXXX.
00192          16  REC-LAST-MAINT-DT       PIC XX.
00193          16  REC-LAST-MAINT-HHMMSS   PIC S9(7) COMP-3.
00194          16  REC-PC                  PIC X(2).
00195
00196  01  TS-WORK-AREA                    PIC X(3600).
00197      EJECT
00198
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00200  01  DFHCOMMAREA                     PIC X(1500).
00201
00203      EJECT
00204 *                       COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00203      EJECT
00204 *                       COPY ERCCONT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCONT.
00006 *                                                                *
00007 *   FILE DESCRIPTION = NOTE FILE FOR COMPENSATION MASTER
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 128   RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCONT             RKP=2,LEN=31          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
00019  01  COMP-NOTE-FILE.
00020      12  NF-FILE-ID                  PIC X(02).
00021          88  VALID-NOTE-ID                       VALUE 'NF'.
00022
00023      12  NF-CONTROL-PRIMARY.
00024          16  NF-COMPANY-CD           PIC X(01).
00027          16  NF-COMP-NOTE-KEY.
00028              18  NF-CARRIER          PIC X(01).
00029              18  NF-GROUPING         PIC X(06).
00030              18  NF-FIN-RESP-NO      PIC X(10).
00030              18  NF-ACCOUNT          PIC X(10).
00025          16  NF-RECORD-TYPE          PIC X(01).
00026              88  GA-NOTE                         VALUE 'G'.
00026              88  ACCT-NOTE                       VALUE 'A'.
00032          16  NF-LINE-SEQUENCE        PIC S9(04)  COMP.
00033
00034      12  NF-LAST-MAINT-DT            PIC X(02).
00035      12  NF-LAST-MAINT-BY            PIC X(04).
00036      12  NF-LAST-MAINT-HHMMSS        PIC S9(07)  COMP-3.
00037
00038      12  NF-NOTE-LINE                PIC X(60).
00039
00040      12  FILLER                      PIC X(25).
00041 *****************************************************************
00205      EJECT
00206 *                       COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
031808
031808         16  FILLER                         PIC X(82).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
00607              20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
011410         16  FILLER                         PIC X(187).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
082603             20  FILLER                     PIC X(11).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
092705         16  FILLER                         PIC X(448).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00207      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                COMPENSATION-MASTER COMP-NOTE-FILE
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6522' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00209
00211      MOVE EIBDATE                     TO DC-JULIAN-YYDDD
00212      MOVE '5'                         TO DC-OPTION-CODE
00213      PERFORM 9700-LINK-DATE-CONVERT   THRU 9700-EXIT
00214      MOVE DC-GREG-DATE-1-EDIT         TO SAVE-DATE
00215      MOVE DC-BIN-DATE-1               TO SAVE-BIN-DATE
00216
00217      MOVE DFHCOMMAREA                 TO PROGRAM-INTERFACE-BLOCK
00210      MOVE EIBTRMID                    TO QID-TERM
00218      IF EIBCALEN = ZEROS
00219          GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
00220
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00223          MOVE PI-SAVED-PROGRAM-5         TO PI-SAVED-PROGRAM-6
00224          MOVE PI-SAVED-PROGRAM-4         TO PI-SAVED-PROGRAM-5
00225          MOVE PI-SAVED-PROGRAM-3         TO PI-SAVED-PROGRAM-4
00226          MOVE PI-SAVED-PROGRAM-2         TO PI-SAVED-PROGRAM-3
00227          MOVE PI-SAVED-PROGRAM-1         TO PI-SAVED-PROGRAM-2
00228          MOVE PI-RETURN-TO-PROGRAM       TO PI-SAVED-PROGRAM-1
00229          MOVE PI-CALLING-PROGRAM         TO PI-RETURN-TO-PROGRAM
00230          MOVE THIS-PGM                   TO PI-CALLING-PROGRAM
           END-IF
00240
CIDMOD     MOVE LOW-VALUES                     TO EL6522AI
00241      MOVE SPACES                         TO ERCONT-KEY
00242
00247      MOVE PI-CR-CARRIER                  TO ERCONT-CARRIER
00250      MOVE PI-CR-GROUPING                 TO ERCONT-GROUPING
           MOVE PI-CR-FIN-RESP                 TO ERCONT-FINRESP
           IF PI-CR-ACCOUNT = SPACES OR LOW-VALUES
               MOVE PI-CR-FIN-RESP             TO ERCONT-ACCOUNT
           ELSE
00256          MOVE PI-CR-ACCOUNT              TO ERCONT-ACCOUNT
           END-IF
00245      MOVE PI-CR-TYPE                     TO ERCONT-REC-TYP
00282      MOVE PI-COMPANY-CD                  TO ERCONT-COMPANY-CD
00283      MOVE ZEROS                          TO ERCONT-SEQ
00284      MOVE ERCONT-PARTIAL-KEY             TO SV-PRIOR-KEY
00285
00286      IF EIBTRNID NOT = TRANS-ID
00287          MOVE '0'                        TO PI-UPDATE-SW
00289          
      * EXEC CICS READQ TS
00290 *             QUEUE (PI-SECURITY-TEMP-STORE-ID)
00291 *             INTO (SECURITY-CONTROL)
00292 *             LENGTH (SC-COMM-LENGTH)
00293 *             ITEM (SC-ITEM)
00294 *        END-EXEC
      *    MOVE '*$II   L              ''   #00003479' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00295          MOVE SC-CREDIT-DISPLAY (05)  TO PI-DISPLAY-CAP
00296          MOVE SC-CREDIT-UPDATE (05)   TO PI-MODIFY-CAP
           END-IF
00297
00298      IF NOT DISPLAY-CAP
00299          MOVE 'READ'                     TO SM-READ
00300          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00301          MOVE ER-0070                    TO EMI-ERROR
00302          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
00303          GO TO 8100-SEND-INITIAL-MAP
           END-IF
00304
00305      
      * EXEC CICS HANDLE AID
00306 *         CLEAR (9400-CLEAR)
00307 *    END-EXEC
      *    MOVE '"&=                  V! " #00003497' TO DFHEIV0
           MOVE X'22263D202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00308
00309      
      * EXEC CICS HANDLE CONDITION
00310 *         ERROR (9990-ABEND)
00311 *         PGMIDERR (9600-PGMID-ERROR)
00312 *    END-EXEC
      *    MOVE '"$.L                  ! # #00003501' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00313
00314      IF EIBTRNID NOT = TRANS-ID
00315          GO TO 7000-BUILD-TABLE
           END-IF
00316      .
00318  2000-RECEIVE.
00319      IF EIBAID = DFHPA1 OR
00320         EIBAID = DFHPA2 OR
00321         EIBAID = DFHPA3
00322          MOVE ER-0008                  TO EMI-ERROR
00323          PERFORM 9900-ERROR-FORMAT     THRU 9900-EXIT
00324          GO TO 8200-SEND-DATAONLY
           END-IF
00325
00326      
      * EXEC CICS RECEIVE
00327 *         MAP(MAP-NAME)
00328 *         MAPSET(MAPSET-NAME)
00329 *         INTO(EL6522AI)
00330 *    END-EXEC
           MOVE LENGTH OF
            EL6522AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003519' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6522AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00331
00332      IF PFENTERL = ZEROS
00333          GO TO 2001-CHECK-PFKEYS
           END-IF
00334
00335      IF EIBAID NOT = DFHENTER
00336          MOVE ER-0004                  TO EMI-ERROR
00337          GO TO 2002-INPUT-ERROR
           END-IF
00338
00339      IF PFENTERI NUMERIC AND
00340         (PFENTERI > 00 AND  < 25)
00341          MOVE PF-VALUES (PFENTERI)     TO EIBAID
00342      ELSE
00343          MOVE ER-0029                  TO EMI-ERROR
00344          GO TO 2002-INPUT-ERROR
           END-IF
00345
           .
00346  2001-CHECK-PFKEYS.
00347      IF EIBAID = DFHPF23
00348          GO TO 9000-RETURN-CICS
           END-IF
00349
00350      IF EIBAID = DFHPF24
00351          GO TO 9200-RETURN-MAIN-MENU
           END-IF
00352
00353      IF EIBAID = DFHPF12
00354          GO TO 9500-PF12
           END-IF
00355
00356      IF FUNCTL NOT = ZEROS AND
              EIBAID NOT = DFHENTER
00357          IF FUNCTI = 'A' OR = SPACES
                   CONTINUE
00359          ELSE
00360              MOVE ER-0050              TO EMI-ERROR
00361              MOVE -1 TO FUNCTL
00362              MOVE AL-UABON             TO FUNCTA PFENTERA
00363              GO TO 2002-INPUT-ERROR
               END-IF
           END-IF
00364
00365      IF EIBAID = DFHPF1
00366          MOVE NUM-LINES-PER-SCREEN     TO ROLL-COUNTER
00367          GO TO 7400-PAGE-ROUTINE
           END-IF
00368
00369      IF EIBAID = DFHPF2
00370          SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER
00371          GO TO 7400-PAGE-ROUTINE
           END-IF
00372
00373      IF EIBAID = DFHPF3
00374          MOVE 5                        TO ROLL-COUNTER
00375          GO TO 7400-PAGE-ROUTINE
           END-IF
00376
00377      IF EIBAID = DFHPF4
00378          MOVE -5                       TO ROLL-COUNTER
00379          GO TO 7400-PAGE-ROUTINE
           END-IF
00380
00384      IF EIBAID = DFHENTER
00385          GO TO 2003-EDIT-DATA
           END-IF
00386
00387      MOVE ER-0029                      TO EMI-ERROR
           .
00388
00389  2002-INPUT-ERROR.
00390      MOVE -1                           TO PFENTERL
00391      MOVE AL-UNBON                     TO PFENTERA
00392      PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
00393      GO TO 8200-SEND-DATAONLY
           .
00394
00395  2003-EDIT-DATA.
00396
00397      IF FUNCTI = 'L'
               CONTINUE
00399      ELSE
00400          IF NOT MODIFY-CAP
00401              MOVE 'UPDATE'                   TO SM-READ
00402              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00403              MOVE ER-0070                    TO EMI-ERROR
00404              PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
00405              GO TO 8100-SEND-INITIAL-MAP
               END-IF
           END-IF
00406
00407      IF FUNCTL = ZEROS OR FUNCTI = SPACES
00408          GO TO 4000-CHANGE-ROUTINE
           END-IF
00409
00410      IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR
00411                 = 'I' OR = 'A' OR = 'L')
               CONTINUE
00413      ELSE
00414          MOVE ER-0023                  TO EMI-ERROR
00415          PERFORM 9900-ERROR-FORMAT     THRU 9900-EXIT
00416          MOVE AL-UABON                 TO FUNCTA
00417          MOVE -1                       TO FUNCTL
00418          GO TO 8200-SEND-DATAONLY
           END-IF
00419
00420      IF FUNCTI = 'D'  OR = 'I' OR = 'L'
00421          PERFORM 2500-LINE-CHECK       THRU 2599-EXIT
00422      ELSE
00423          IF (LINE1L NOT = ZEROS OR
00424              LINE2L NOT = ZEROS)
00425              MOVE ER-0030              TO EMI-ERROR
00426              MOVE -1                   TO LINE1L
00427              MOVE AL-UNBON             TO LINE1A LINE2A
00428              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00429              GO TO 8200-SEND-DATAONLY
               END-IF
           END-IF
00430
00431      IF FUNCTI = 'A'
00432          GO TO 5000-ADD-NEW-LINES
           END-IF
00433      IF FUNCTI = 'Q'
00434          GO TO 9410-RETURN
           END-IF
00435      IF FUNCTI = 'S'
00436          GO TO 4500-SAVE-DATA
           END-IF
00437      IF PI-TOTAL-LINES = 0
00438          MOVE ER-0048                 TO EMI-ERROR
00439          MOVE -1                      TO FUNCTL
00440          MOVE AL-UNBON                TO FUNCTA
00441          PERFORM 9900-ERROR-FORMAT    THRU 9900-EXIT
00442          GO TO 8200-SEND-DATAONLY
           END-IF
00443      IF FUNCTI = 'L'
00444          GO TO 5500-LOOKUP
           END-IF
00445      IF FUNCTI = 'D'
00446          GO TO 3000-DELETE-LINES
           END-IF
00447
00448      GO TO 3500-INSERT-LINES
           .
00450  2500-LINE-CHECK.
00451      IF (LINE1L = ZEROS AND
00452          LINE2L = ZEROS)
00453          MOVE ER-0069                 TO EMI-ERROR
00454          PERFORM 9900-ERROR-FORMAT    THRU 9900-EXIT
00455          MOVE -1                      TO LINE1L
00456          GO TO 8200-SEND-DATAONLY
           END-IF
00457
00458      IF LINE1L NOT = ZEROS
00459          IF (LINE1I NOT NUMERIC OR
00460              LINE1I > PI-TOTAL-LINES)
00461              MOVE ER-0031              TO EMI-ERROR
00462              MOVE AL-UNBON             TO LINE1A
00463              MOVE -1                   TO LINE1L
00464              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00465              GO TO 8200-SEND-DATAONLY
00466          ELSE
00467              IF LINE2L = ZEROS
00468                  MOVE 1                TO LINE2I
                   END-IF
00469              IF FUNCTI = 'I'
00470                  GO TO 2510-MAX-CHECK
                   END-IF
00476              IF LINE2I NOT NUMERIC
00477                  MOVE AL-UNBON             TO LINE2A
00478                  MOVE ER-0032              TO EMI-ERROR
00479                  MOVE -1                   TO LINE2L
00480                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00481                  GO TO 8200-SEND-DATAONLY
00482              ELSE
                       CONTINUE
                   END-IF
               END-IF
00484      ELSE
00485          IF LINE2L = ZEROS
                   CONTINUE
00487          ELSE
00488              MOVE -1                   TO LINE2L
00489              MOVE ER-0041              TO EMI-ERROR
00490              MOVE AL-UNBON             TO LINE2A
00491              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00492              GO TO 8200-SEND-DATAONLY
               END-IF
           END-IF
00493      GO TO 2599-EXIT
           .
00494  2510-MAX-CHECK.
00495      IF LINE2I NOT NUMERIC
00496          MOVE -1                       TO LINE2L
00497          MOVE ER-0032                  TO EMI-ERROR
00498          MOVE AL-UNBON                 TO LINE2A
00499          PERFORM 9900-ERROR-FORMAT     THRU 9900-EXIT
00500          GO TO 8200-SEND-DATAONLY
00501      ELSE
00502          COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES
00503          IF ROLL-COUNTER > MAX-LINES
00504              MOVE -1                   TO LINE2L
00505              MOVE ER-0044              TO EMI-ERROR
00506              MOVE AL-UNBON             TO LINE2A
00507              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00508              GO TO 8200-SEND-DATAONLY
               END-IF
           END-IF
           .
00509  2599-EXIT.
00510      EXIT.
00512  3000-DELETE-LINES.
00513      IF LINE2L = ZEROS AND LINE2I = 1
00514          MOVE LINE1I                 TO LINE2I
           END-IF
00515
00516      IF LINE2I > PI-TOTAL-LINES OR < LINE1I
00517          MOVE ER-0049                TO EMI-ERROR
00518          MOVE AL-UNBON               TO LINE2A
00519          MOVE -1                     TO LINE2L
00520          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00521          GO TO 8200-SEND-DATAONLY
           END-IF
00522
00523      PERFORM 7450-SET-INDX           THRU 7450-EXIT
00524      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00525              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00527      SET TB-INDX TO LINE1I
00528
00529      PERFORM 3050-CHECK-FOR-CURRENT-DATE
00530              UNTIL TB-INDX > LINE2I
00531
00532      IF NOT EMI-NO-ERRORS
00533          GO TO 8200-SEND-DATAONLY
           END-IF
00534
00535      SET TB-INDX TO LINE1I
00536      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1
00537
00538      IF LINE2I NOT = PI-TOTAL-LINES
00539          SET TB-INDX1 TO LINE2I
00540          SET TB-INDX1 UP BY 1
00541          PERFORM 3100-DELETE-TABLE-ENTRIES
00542                  UNTIL TB-INDX1 > PI-TOTAL-LINES
           END-IF
00543
00544      PERFORM 3150-BLANK-TABLE-ENTRIES
00545              ROLL-COUNTER TIMES
00546      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES
00547
00548      IF PI-CURRENT-LINE > PI-TOTAL-LINES
00549          MOVE PI-TOTAL-LINES         TO PI-CURRENT-LINE
00550          SUBTRACT 1 FROM PI-CURRENT-LINE
           END-IF
00551
00552      SET TB-INDX                     TO PI-CURRENT-LINE
00553      MOVE LOW-VALUES                 TO EL6522AI
00554
00555      IF PI-CURRENT-LINE > ZERO
00556          PERFORM 7100-FORMAT-SCREEN  THRU 7100-EXIT
00557                  VARYING SC-INDX FROM 1
                       BY 1
                       UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00559          PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT
           END-IF
00560
00561      MOVE '1'                        TO PI-UPDATE-SW
00562      IF PI-TOTAL-LINES = ZEROS
00563          MOVE ZEROS                  TO PI-CURRENT-LINE
           END-IF
00564      GO TO 8100-SEND-INITIAL-MAP
           .
00566
00567  3050-CHECK-FOR-CURRENT-DATE.
00568      IF REC-LAST-MAINT-DT (TB-INDX) = SAVE-BIN-DATE
               CONTINUE
00570      ELSE
00571          MOVE ER-3786                TO EMI-ERROR
00572          MOVE AL-UNBON               TO FUNCTA
00573          MOVE -1                     TO FUNCTL
00574          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00575          GO TO 8200-SEND-DATAONLY
           END-IF
00576
00577      IF REC-LAST-MAINT-BY (TB-INDX) = PI-PROCESSOR-ID
               CONTINUE
00579      ELSE
00580          MOVE ER-3787                TO EMI-ERROR
00581          MOVE AL-UNBON               TO FUNCTA
00582          MOVE -1                     TO FUNCTL
00583          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00584          GO TO 8200-SEND-DATAONLY
           END-IF
00585
00586      SET TB-INDX TB-INDX1 UP BY 1
           .
00587
00588  3100-DELETE-TABLE-ENTRIES.
00589      MOVE REC-ENT (TB-INDX1)         TO REC-ENT (TB-INDX)
00590      SET TB-INDX TB-INDX1 UP BY 1
           .
00591
00592  3150-BLANK-TABLE-ENTRIES.
00593      MOVE SPACES               TO REC-ENT (TB-INDX)
00594      MOVE LOW-VALUES           TO REC-LAST-MAINT-DT (TB-INDX)
00595      MOVE ZERO                 TO REC-LAST-MAINT-HHMMSS (TB-INDX)
00596      SET TB-INDX UP BY 1
           .
00598  3500-INSERT-LINES.
00599
00600      PERFORM 7450-SET-INDX                 THRU 7450-EXIT
00601      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00602              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00604
00605      IF NOT EMI-NO-ERRORS
00606          GO TO 8200-SEND-DATAONLY
           END-IF
00607
00608      SET TB-INDX TO PI-TOTAL-LINES
00609      ADD LINE2I TO PI-TOTAL-LINES
00610      SET TB-INDX1 TO PI-TOTAL-LINES
00611      PERFORM 3600-INSERT-TABLE-ENTRIES
00612              UNTIL TB-INDX = LINE1I
00613      SET TB-INDX UP BY 1
00614
00615      COMPUTE ROLL-COUNTER = PI-CURRENT-LINE +
                                  NUM-LINES-PER-SCREEN
00616      IF TB-INDX NOT < ROLL-COUNTER OR
00617                     < PI-CURRENT-LINE
00618          SET SC-INDX TO 1
00619          SET SC-INDX DOWN BY 1
00620      ELSE
00621          SET ROLL-COUNTER TO TB-INDX
00622          COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE
00623                   + 1
00624          SET SC-INDX TO ROLL-COUNTER
           END-IF
00625
00626      PERFORM 3150-BLANK-TABLE-ENTRIES
                   LINE2I TIMES
00627      SET TB-INDX TO PI-CURRENT-LINE
00628      MOVE LOW-VALUES                 TO EL6522AI
00629
00630      IF SC-INDX NOT = ZERO
00631         MOVE -1                      TO SC-TEXTL (SC-INDX)
           END-IF
00632
00633      PERFORM 7100-FORMAT-SCREEN      THRU 7100-EXIT
00634             VARYING SC-INDX FROM 1
                  BY 1
                  UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00636      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00637      MOVE '1'                        TO PI-UPDATE-SW
00638      GO TO 8100-SEND-INITIAL-MAP
           .
00639
00640  3600-INSERT-TABLE-ENTRIES.
00641      MOVE REC-ENT (TB-INDX)          TO REC-ENT (TB-INDX1)
00642      SET TB-INDX TB-INDX1 DOWN BY 1
           .
00644
00645  4000-CHANGE-ROUTINE.
00646      PERFORM 7450-SET-INDX                 THRU 7450-EXIT
00647      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00648              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00650
00651      IF NOT EMI-NO-ERRORS
00652          GO TO 8200-SEND-DATAONLY
           END-IF
00653
00654      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00655      MOVE SPACES                     TO ERRMSGBO
00656      GO TO 8200-SEND-DATAONLY
           .
00657
00659  4500-SAVE-DATA.
00660      PERFORM 7450-SET-INDX                 THRU 7450-EXIT
00661      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00662              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00664      IF NOT EMI-NO-ERRORS
00665          GO TO 8200-SEND-DATAONLY
           END-IF
00666
00667      
      * EXEC CICS HANDLE CONDITION
00668 *         NOTFND(4610-ENDBR)
00669 *         NOTOPEN(6000-NOT-OPEN)
00670 *         ENDFILE(4610-ENDBR)
00671 *    END-EXEC
      *    MOVE '"$IJ''                 ! $ #00003916' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
00672
00673  4610-LOOP.
00674      
      * EXEC CICS READ
00675 *         DATASET (ERCONT-FILE-ID)
00676 *         RIDFLD (ERCONT-KEY)
00677 *         SET (ADDRESS OF COMP-NOTE-FILE)
00678 *         GTEQ
00679 *    END-EXEC
      *    MOVE '&"S        G          (   #00003924' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCONT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCONT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMP-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00680
00681      MOVE NF-CONTROL-PRIMARY         TO ERCONT-KEY
00682
00683      IF ERCONT-PARTIAL-KEY NOT = SV-PRIOR-KEY
00684          MOVE SV-PRIOR-KEY           TO ERCONT-PARTIAL-KEY
00685          GO TO 4610-ENDBR
           END-IF
00686
00687      
      * EXEC CICS DELETE
00688 *        DATASET (ERCONT-FILE-ID)
00689 *        RIDFLD (ERCONT-KEY)
00690 *    END-EXEC
      *    MOVE '&(  R                 &   #00003938' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCONT-FILE-ID, 
                 ERCONT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00691
00692      GO TO 4610-LOOP
           .
00693  4610-ENDBR.
00694      
      * EXEC CICS GETMAIN
00695 *         LENGTH(ERCONT-LENGTH)
00696 *         SET(ADDRESS OF COMP-NOTE-FILE)
00697 *         INITIMG(GETMAIN-SPACE)
00698 *    END-EXEC
      *    MOVE ',"IL                  $   #00003946' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCONT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMP-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00699
00700      PERFORM 4700-WRITE-FILE         THRU 4799-EXIT
00701              VARYING TB-INDX FROM 1
                   BY 1
                   UNTIL TB-INDX > PI-TOTAL-LINES
00703
00707      GO TO 9410-RETURN
           .
00708
00709  4700-WRITE-FILE.
00710      MOVE SPACES                     TO COMP-NOTE-FILE
00711      ADD 1                           TO ERCONT-SEQ
00712      MOVE ERCONT-KEY                 TO NF-CONTROL-PRIMARY
00713      MOVE  'NF'                      TO NF-FILE-ID
00714      MOVE REC-TEXT (TB-INDX)         TO NF-NOTE-LINE
00715      MOVE REC-LAST-MAINT-BY (TB-INDX)
00716                                      TO NF-LAST-MAINT-BY
00717      MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)
00718                                      TO NF-LAST-MAINT-HHMMSS
00719      MOVE REC-LAST-MAINT-DT (TB-INDX)
00720                                      TO NF-LAST-MAINT-DT
00721
00722      
      * EXEC CICS WRITE
00723 *         DATASET(ERCONT-FILE-ID)
00724 *         FROM(COMP-NOTE-FILE)
00725 *         RIDFLD(ERCONT-KEY)
00726 *    END-EXEC
           MOVE LENGTH OF
            COMP-NOTE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003973' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCONT-FILE-ID, 
                 COMP-NOTE-FILE, 
                 DFHEIV11, 
                 ERCONT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
00727  4799-EXIT.
00728       EXIT.
00729
00731  5000-ADD-NEW-LINES.
00732      PERFORM 7450-SET-INDX                 THRU 7450-EXIT
00733      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00734              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00736
00737      IF NOT EMI-NO-ERRORS
00738          GO TO 8200-SEND-DATAONLY
           END-IF
00739
00740      MOVE PI-TOTAL-LINES             TO PI-CURRENT-LINE
00741      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00742      MOVE LOW-VALUES                 TO EL6522AI
00743      SET TB-INDX                     TO PI-CURRENT-LINE
00744      MOVE 'A'                        TO FUNCTI
00745      MOVE -1                         TO SC-TEXTL (2)
00746      MOVE AL-UANON                   TO FUNCTA
00747      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT
00748              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00750      MOVE '1'                        TO PI-UPDATE-SW
00751      GO TO 8100-SEND-INITIAL-MAP
           .
00753  5500-LOOKUP.
00754      PERFORM 7500-READ-TS            THRU 7599-EXIT
00755      SET TB-INDX                     TO PI-CURRENT-LINE
00756      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00757              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00759
00760      IF NOT EMI-NO-ERRORS
00761          GO TO 8200-SEND-DATAONLY
           END-IF
00762
00763      MOVE LINE1I                     TO PI-CURRENT-LINE
00764      SET TB-INDX                     TO PI-CURRENT-LINE
00765      MOVE LOW-VALUES                 TO EL6522AI
00766      PERFORM 7100-FORMAT-SCREEN      THRU 7100-EXIT
00767              VARYING SC-INDX FROM 1
                   BY 1
00768              UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00769      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00770      GO TO 8100-SEND-INITIAL-MAP
           .
00772  6000-NOT-OPEN.
00773      MOVE ER-3788                    TO EMI-ERROR
00774      PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
00775
00776      IF EIBAID = DFHCLEAR
00777          GO TO 9410-RETURN
00778      ELSE
00779          GO TO 8100-SEND-INITIAL-MAP
           END-IF
           .
00781  7000-BUILD-TABLE.
00782
00783      SET TB-INDX                     TO 1
00784      MOVE ZEROS                      TO PI-TOTAL-LINES
00785                                         PI-CURRENT-LINE
00786                                         PI-TEMP-STOR-ITEMS
00787                                         PI-UPDATE-SW
00788      MOVE LOW-VALUES                 TO EL6522AI
00789      PERFORM 7500-READ-TS            THRU 7599-EXIT
00790
00791      IF PI-TEMP-STOR-ITEMS NOT = ZERO
00792          MOVE ER-0140                TO EMI-ERROR
00793          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00794          MULTIPLY PI-TEMP-STOR-ITEMS BY TS-NUM-REC-IN-GROUP
                    GIVING PI-TOTAL-LINES
00796          MOVE 1                      TO PI-CURRENT-LINE
00797          SET TB-INDX TO 1
00798          PERFORM 7100-FORMAT-SCREEN  THRU 7100-EXIT
00799                  VARYING SC-INDX FROM 1
00800                  BY 1
                       UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00801          GO TO 8100-SEND-INITIAL-MAP
           END-IF
00802
00803      
      * EXEC CICS HANDLE CONDITION
00804 *         NOTFND(7010-ENDBR)
00805 *         NOTOPEN(6000-NOT-OPEN)
00806 *         ENDFILE(7010-ENDBR)
00807 *    END-EXEC
      *    MOVE '"$IJ''                 ! % #00004063' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034303633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00808
00809      
      * EXEC CICS STARTBR
00810 *         DATASET(ERCONT-FILE-ID)
00811 *         RIDFLD(ERCONT-KEY)
00812 *         KEYLENGTH(ERCONT-START-LENGTH)
00813 *         GENERIC
00814 *         GTEQ
00815 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &   #00004069' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCONT-FILE-ID, 
                 ERCONT-KEY, 
                 ERCONT-START-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
00816
00817  7001-LOOP.
00818      
      * EXEC CICS READNEXT
00819 *         SET(ADDRESS OF COMP-NOTE-FILE)
00820 *         DATASET(ERCONT-FILE-ID)
00821 *         RIDFLD(ERCONT-KEY)
00822 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004079' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCONT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCONT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMP-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00823
00824      IF NF-COMPANY-CD NOT = SV-COMPANY-CD
00825          GO TO 7010-ENDBR
           END-IF
00826
00827      IF (NF-CARRIER = SV-CARRIER) AND
00828         (NF-GROUPING = SV-GROUPING) AND
00829         (NF-FIN-RESP-NO = SV-FINRESP) AND
00830         (NF-ACCOUNT = SV-ACCOUNT)
00831          MOVE NF-NOTE-LINE         TO REC-TEXT (TB-INDX)
00832          MOVE NF-LAST-MAINT-BY     TO REC-LAST-MAINT-BY (TB-INDX)
00833          MOVE NF-LAST-MAINT-DT     TO REC-LAST-MAINT-DT (TB-INDX)
00834          MOVE NF-LAST-MAINT-HHMMSS TO
00835                                   REC-LAST-MAINT-HHMMSS (TB-INDX)
00836          SET TB-INDX UP BY 1
00837          GO TO 7001-LOOP
           END-IF
           .
00838  7010-ENDBR.
00839      IF TB-INDX = 1
00840          MOVE ER-0006                TO EMI-ERROR
00841          MOVE 'A'                    TO FUNCTI
00842          MOVE -1                     TO SC-TEXTL (1)
00843          MOVE AL-UANON               TO FUNCTA
00844          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00845          MOVE ZEROS                  TO PI-TOTAL-LINES
00846          GO TO 8100-SEND-INITIAL-MAP
           END-IF
00848      
      * EXEC CICS ENDBR
00849 *         DATASET(ERCONT-FILE-ID)
00850 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004112' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCONT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00851
00852      SET TB-INDX DOWN BY 1
00853      SET PI-TOTAL-LINES              TO TB-INDX
00854      MOVE 1                          TO PI-CURRENT-LINE
           .
00855
00856  7050-FORMAT-LINES.
00857      SET TB-INDX                     TO PI-CURRENT-LINE
00858      PERFORM 7100-FORMAT-SCREEN      THRU 7100-EXIT
00859              VARYING SC-INDX FROM 1
00860              BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00861      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00862      GO TO 8100-SEND-INITIAL-MAP
           .
00864  7100-FORMAT-SCREEN.
00865      IF TB-INDX > PI-TOTAL-LINES
00866          IF FUNCTI NOT = 'A'
00867              MOVE AL-PANON           TO SC-TEXTA (SC-INDX)
               END-IF
           END-IF
00868
00869      IF TB-INDX > PI-TOTAL-LINES
00870          GO TO 7100-EXIT
           END-IF
00871
00872      MOVE REC-TEXT (TB-INDX)          TO SC-TEXT (SC-INDX)
00873      MOVE REC-LAST-MAINT-BY (TB-INDX) TO SC-MAINT-BY (SC-INDX)
00875      PERFORM 7110-FORMAT-DATE         THRU 7110-EXIT
00876      SET ROLL-COUNTER                 TO TB-INDX
00877      MOVE ROLL-COUNTER                TO SC-LIN (SC-INDX)
00878
00879      IF NOT MODIFY-CAP
00880          MOVE AL-PANOF                TO SC-TEXTA (SC-INDX)
00881          SET TB-INDX UP BY 1
00882          GO TO 7100-EXIT
           END-IF
00883
00884      IF (REC-LAST-MAINT-BY (TB-INDX) NOT = SPACES AND LOW-VALUES)
00885           AND
00886         (REC-LAST-MAINT-BY (TB-INDX) NOT EQUAL PI-PROCESSOR-ID)
00887          MOVE AL-PANOF                TO SC-TEXTA (SC-INDX)
00888          SET TB-INDX UP BY 1
00889          GO TO 7100-EXIT
           END-IF
00890
00891      IF (REC-LAST-MAINT-DT (TB-INDX) = SAVE-BIN-DATE OR
00892                               LOW-VALUES OR SPACES)
00893          SET TB-INDX UP BY 1
00894      ELSE
00895          MOVE AL-PANOF                TO SC-TEXTA (SC-INDX)
00896          SET TB-INDX UP BY 1
           END-IF
           .
00898  7100-EXIT.
00899       EXIT.
00900
00901  7110-FORMAT-DATE.
00902      IF REC-LAST-MAINT-DT (TB-INDX) = LOW-VALUES OR SPACES
               CONTINUE
00904      ELSE
00905          MOVE REC-LAST-MAINT-DT (TB-INDX) TO DC-BIN-DATE-1
00907          MOVE SPACES                      TO DC-OPTION-CODE
00908          PERFORM 9700-LINK-DATE-CONVERT   THRU 9700-EXIT
00909          MOVE DC-GREG-DATE-1-MDY          TO
                                                SC-MAINT-DT (SC-INDX)
           END-IF
           .
00911  7110-EXIT.
           EXIT.
00912
00913  7200-PUT-TEMP-STOR.
00914      PERFORM 7250-DELETE-TEMP-STOR    THRU 7299-EXIT
00915      SET TS-INDX                      TO 1
00916      MOVE 0                           TO PI-TEMP-STOR-ITEMS
00917      PERFORM 7300-WRITE-TS            THRU 7399-EXIT
00918              VARYING TS-GROUP-WORK FROM 0
                   BY TS-NUM-REC-IN-GROUP
00919              UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES
           .
00920  7249-EXIT.
00921      EXIT.
00922  7250-DELETE-TEMP-STOR.
00923      
      * EXEC CICS HANDLE CONDITION
00924 *         QIDERR(7299-EXIT)
00925 *    END-EXEC
      *    MOVE '"$N                   ! & #00004198' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034313938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00926      
      * EXEC CICS DELETEQ TS
00927 *         QUEUE(QID)
00928 *    END-EXEC
      *    MOVE '*&                    #   #00004201' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
00929  7299-EXIT.
00930      EXIT.
00932  7300-WRITE-TS.
00933      MOVE TS-GROUP (TS-INDX)          TO TS-WORK-AREA
00934      SET TS-INDX UP BY 1
00935      ADD 1                            TO PI-TEMP-STOR-ITEMS
00936      
      * EXEC CICS WRITEQ TS
00937 *         FROM(TS-WORK-AREA)
00938 *         QUEUE(QID)
00939 *         LENGTH(TS-LENGTH)
00940 *         ITEM(PI-TEMP-STOR-ITEMS)
00941 *    END-EXEC
      *    MOVE '*" I                  ''   #00004211' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
00942  7399-EXIT.
00943      EXIT.
00945  7400-PAGE-ROUTINE.
00946
00947      IF PFENTERL NOT = ZEROS
00948          MOVE -1                           TO PFENTERL
00949      ELSE
00950          MOVE -1                           TO FUNCTL
           END-IF
00951
00952      IF PI-TOTAL-LINES = 0
00953          MOVE ER-0047                      TO EMI-ERROR
00954          MOVE -1                           TO FUNCTL
00955          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
00956          GO TO 8200-SEND-DATAONLY
           END-IF
00957
00958      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER
00959
00960      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
00961          MOVE ER-0067                      TO EMI-ERROR
00962          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
00963          MOVE 1                            TO TEMP-CURR-LINE
           END-IF
00964
00965      IF TEMP-CURR-LINE > PI-TOTAL-LINES
00966          MOVE ER-0066                      TO EMI-ERROR
00967          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
00968          COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1
00969                                - NUM-LINES-PER-SCREEN
00970          IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
00971              MOVE 1                        TO TEMP-CURR-LINE
               END-IF
           END-IF
00972
00973      PERFORM 7450-SET-INDX                 THRU 7450-EXIT
00974      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT
00975              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00977
00978      IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS
               CONTINUE
00980      ELSE
00981          GO TO 8200-SEND-DATAONLY
           END-IF
00982
00983      MOVE TEMP-CURR-LINE                   TO PI-CURRENT-LINE
00984      SET TB-INDX                           TO PI-CURRENT-LINE
00985      MOVE LOW-VALUES                       TO EL6522AI
00986      PERFORM 7100-FORMAT-SCREEN            THRU 7100-EXIT
00987              VARYING SC-INDX FROM 1
                   BY 1
00988              UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00989      PERFORM 7200-PUT-TEMP-STOR            THRU 7249-EXIT
00990      GO TO 8100-SEND-INITIAL-MAP
           .
00992
00993  7450-SET-INDX.
00994      IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0
00995          SET TB-INDX                       TO 1
00996      ELSE
00997          PERFORM 7500-READ-TS              THRU 7599-EXIT
00998          IF PI-CURRENT-LINE = 0
00999              SET TB-INDX                   TO 1
01000          ELSE
01001              SET TB-INDX                   TO PI-CURRENT-LINE
               END-IF
           END-IF
           .
01002  7450-EXIT.
01003      EXIT.
01005  7500-READ-TS.
01006      
      * EXEC CICS HANDLE CONDITION
01007 *         QIDERR(7590-TS-QIDERR)
01008 *         ITEMERR(7585-QID-ITEMERR)
01009 *    END-EXEC
      *    MOVE '"$N<                  ! '' #00004291' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034323931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01010      SET TS-INDX                           TO 1
01011      MOVE 1                                TO QID-ITEM
           PERFORM 7501-LOOP THRU 7501-EXIT
00918              VARYING TS-GROUP-WORK FROM 0
                   BY TS-NUM-REC-IN-GROUP
00919              UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES
01025      IF EIBTRNID NOT = TRANS-ID
01026          SUBTRACT +1 FROM QID-ITEM
01027          MOVE QID-ITEM                     TO PI-TEMP-STOR-ITEMS
           END-IF
           GO TO 7599-EXIT
           .
       7500-EXIT.
           EXIT.
01012  7501-LOOP.
01013      
      * EXEC CICS READQ TS
01014 *         INTO (TS-WORK-AREA)
01015 *         QUEUE (QID)
01016 *         LENGTH (TS-LENGTH)
01017 *         ITEM (QID-ITEM)
01018 *    END-EXEC
      *    MOVE '*$II   L              ''   #00004310' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01019      MOVE TS-WORK-AREA                     TO TS-GROUP (TS-INDX)
01020      SET TS-INDX UP BY 1
01021      ADD +1                                TO QID-ITEM
           .
       7501-EXIT.
           EXIT.
01023
01024  7585-QID-ITEMERR.
01025      IF EIBTRNID NOT = TRANS-ID
01026          SUBTRACT 1 FROM QID-ITEM
01027          MOVE QID-ITEM                     TO PI-TEMP-STOR-ITEMS
01028          GO TO 7599-EXIT
           END-IF
01029
           .
01030  7590-TS-QIDERR.
01031      IF EIBTRNID = TRANS-ID
01032          AND EIBAID = DFHCLEAR
01033          GO TO 9410-RETURN
           END-IF
01034      IF EIBTRNID = TRANS-ID
01035          MOVE ER-0033                      TO EMI-ERROR
01036          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
01037          GO TO 8100-SEND-INITIAL-MAP
           END-IF
           .
01039  7599-EXIT.
01040      EXIT.
01041
01043  7600-UPDATE-TABLE-FROM-SCREEN.
01044
01045      IF SC-TEXTL (SC-INDX) NOT = ZEROS
01046          IF TB-INDX NOT > PI-TOTAL-LINES
01047              PERFORM 7700-MOVE-DATA        THRU 7700-EXIT
01048              SET TB-INDX UP BY 1
01049          ELSE
01050              IF PI-TOTAL-LINES = MAX-LINES
01051                  MOVE ER-0051              TO EMI-ERROR
01052                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01053                  GO TO 8200-SEND-DATAONLY
01054              ELSE
01055                  PERFORM 7700-MOVE-DATA    THRU 7700-EXIT
01056                  SET TB-INDX UP BY 1
01057                  ADD 1                     TO PI-TOTAL-LINES
                   END-IF
               END-IF
01058      ELSE
01059          IF TB-INDX NOT > PI-TOTAL-LINES
01060              SET TB-INDX UP BY 1
               END-IF
           END-IF
           .
01062  7699-EXIT.
01063      EXIT.
01064
01065  7700-MOVE-DATA.
01066      MOVE '1'                              TO PI-UPDATE-SW
01067
01068      IF SC-TEXTL (SC-INDX) NOT = ZEROS
01069          MOVE SC-TEXT (SC-INDX)            TO REC-TEXT (TB-INDX)
           END-IF
01070
01071      IF REC-LAST-MAINT-DT (TB-INDX) = LOW-VALUES OR SPACES
01072          MOVE PI-PROCESSOR-ID  TO REC-LAST-MAINT-BY (TB-INDX)
01073                                   SC-MAINT-BY (SC-INDX)
01074          MOVE EIBTIME          TO REC-LAST-MAINT-HHMMSS (TB-INDX)
01075          MOVE SAVE-BIN-DATE    TO REC-LAST-MAINT-DT (TB-INDX)
           END-IF
01076
01077      PERFORM 7110-FORMAT-DATE
           .
01079  7700-EXIT.
01080      EXIT.
01082  8100-SEND-INITIAL-MAP.
01083      MOVE SAVE-DATE                        TO DATEO
01084      MOVE EIBTIME                          TO TIME-IN
01085      MOVE TIME-OUT                         TO TIMEO
022803     MOVE PI-COMPANY-ID                    TO CMPNYIDO
022803     MOVE PI-PROCESSOR-ID                  TO USERIDO
01086      MOVE EMI-MESSAGE-AREA (1)             TO ERRMSGBO
01087      MOVE PI-CR-CARRIER                    TO CARRO
01088      MOVE PI-CR-GROUPING                   TO GROUPO
           MOVE PI-CR-TYPE                       TO TYPEO
01090      MOVE PI-CR-FIN-RESP                   TO FINRESPO
01090      MOVE PI-CR-ACCOUNT                    TO ACCTO
01092      MOVE PI-SAVE-ACCT-NAME                TO NAMEO
01091      MOVE PI-TOTAL-LINES                   TO TOTO
01093      MOVE -1                               TO FUNCTL
01094
01095      
      * EXEC CICS SEND
01096 *         MAP(MAP-NAME)
01097 *         MAPSET(MAPSET-NAME)
01098 *         FROM(EL6522AO)
01099 *         ERASE
01100 *         CURSOR
01101 *    END-EXEC
           MOVE LENGTH OF
            EL6522AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004405' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6522AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01102
01103      GO TO 9100-RETURN-TRAN
           .
01104
01105  8200-SEND-DATAONLY.
01106      MOVE EIBTIME                          TO TIME-IN
01107      MOVE TIME-OUT                         TO TIMEO
022803     MOVE PI-COMPANY-ID                    TO CMPNYIDO
022803     MOVE PI-PROCESSOR-ID                  TO USERIDO
01108      MOVE PI-TOTAL-LINES                   TO TOTO
01109
01110      IF NOT EMI-NO-ERRORS
01111          MOVE EMI-MESSAGE-AREA (1)         TO ERRMSGBO
01112      ELSE
01113          MOVE -1                           TO FUNCTL
           END-IF
01114
01115      
      * EXEC CICS SEND
01116 *         MAP(MAP-NAME)
01117 *         MAPSET(MAPSET-NAME)
01118 *         FROM(EL6522AO)
01119 *         DATAONLY
01120 *         CURSOR
01121 *    END-EXEC
           MOVE LENGTH OF
            EL6522AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004429' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6522AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01122
01123      GO TO 9100-RETURN-TRAN
           .
01124
01125  8300-SEND-TEXT.
01126      
      * EXEC CICS SEND TEXT
01127 *         FROM(LOGOFF-TEXT)
01128 *         ERASE
01129 *         FREEKB
01130 *         LENGTH(LOGOFF-LENGTH)
01131 *    END-EXEC
      *    MOVE '8&      T  E F  H   F -   #00004441' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01132
01133      PERFORM 7250-DELETE-TEMP-STOR         THRU 7299-EXIT
01134
01135      
      * EXEC CICS RETURN
01136 *    END-EXEC
      *    MOVE '.(                    &   #00004450' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
01137
01138  8800-UNAUTHORIZED-ACCESS.
01139      MOVE UNACCESS-MSG                     TO LOGOFF-MSG
01140      GO TO 8300-SEND-TEXT
           .
01141
01142  9000-RETURN-CICS.
01143      IF PI-CHANGES-MADE
01144          MOVE ER-0045                      TO EMI-ERROR
01145          MOVE -1                           TO FUNCTL
01146          MOVE SPACES                       TO PFENTERO
01147          MOVE AL-UNNOF                     TO PFENTERA
01148          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
01149          GO TO 8200-SEND-DATAONLY
           END-IF
01150
01151      MOVE EIBAID                           TO PI-ENTRY-CD-1
01152      MOVE XCTL-005                         TO PGM-NAME
01153      GO TO 9300-XCTL
           .
01154
01155  9100-RETURN-TRAN.
01156      MOVE SCRN-NUMBER                    TO PI-CURRENT-SCREEN-NO
01157      MOVE EMI-ERROR-NUMBER (1)           TO PI-LAST-ERROR-NO
01158      
      * EXEC CICS RETURN
01159 *         TRANSID(TRANS-ID)
01160 *         COMMAREA(PROGRAM-INTERFACE-BLOCK)
01161 *         LENGTH(PI-COMM-LENGTH)
01162 *    END-EXEC
      *    MOVE '.(CT                  &   #00004477' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01163
           .
01165  9200-RETURN-MAIN-MENU.
01166      IF PI-CHANGES-MADE
01167          MOVE -1                         TO FUNCTL
01168          MOVE SPACES                     TO PFENTERO
01169          MOVE AL-UNNOF                   TO PFENTERA
01170          MOVE ER-0045                    TO EMI-ERROR
01171          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
01172          GO TO 8200-SEND-DATAONLY
           END-IF
01173
01174      MOVE XCTL-126                       TO PGM-NAME
           .
01175
01176  9300-XCTL.
01177      PERFORM 7250-DELETE-TEMP-STOR       THRU 7299-EXIT
01178      
      * EXEC CICS XCTL
01179 *         PROGRAM  (PGM-NAME)
01180 *         COMMAREA (PROGRAM-INTERFACE-BLOCK)
01181 *         LENGTH   (PI-COMM-LENGTH)
01182 *    END-EXEC
      *    MOVE '.$C                   $   #00004499' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
01183
01184  9400-CLEAR.
01185
01186      IF PI-CHANGES-MADE
01187          MOVE ER-0045                    TO EMI-ERROR
01188          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
01189          IF PI-CURRENT-LINE > ZERO
01190              PERFORM 7500-READ-TS        THRU 7599-EXIT
01191              SET TB-INDX                 TO PI-CURRENT-LINE
01192              PERFORM 7100-FORMAT-SCREEN  THRU 7100-EXIT
01193                      VARYING SC-INDX FROM 1
                           BY 1
                           UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN
01195          END-IF
01197          GO TO 8100-SEND-INITIAL-MAP
           END-IF
           .
01198
01199  9410-RETURN.
01200      MOVE PI-RETURN-TO-PROGRAM           TO PGM-NAME
01201      GO TO 9300-XCTL
           .
01202
01203  9500-PF12.
01204      IF PI-CHANGES-MADE
01205          MOVE -1                         TO FUNCTL
01206          MOVE SPACES                     TO PFENTERO
01207          MOVE AL-UNNOF                   TO PFENTERA
01208          MOVE ER-0045                    TO EMI-ERROR
01209          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
01210          GO TO 8200-SEND-DATAONLY
           END-IF
01211
01212      MOVE 'EL010'                        TO PGM-NAME
01213      GO TO 9300-XCTL
           .
01214
01216  9550-START-PRINT.
01217      PERFORM 7200-PUT-TEMP-STOR          THRU 7249-EXIT
01218
01219      MOVE SPACES                         TO ELCNTL-KEY
01220      MOVE PI-COMPANY-ID                  TO ELCNTL-COMPANY
01221      MOVE '1'                            TO ELCNTL-REC-TYPE
01222      MOVE ZEROS                          TO ELCNTL-SEQ-NO
01223
01224      
      * EXEC CICS HANDLE CONDITION
01225 *         NOTFND(9410-RETURN)
01226 *    END-EXEC
      *    MOVE '"$I                   ! ( #00004550' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01227
01228      
      * EXEC CICS READ DATASET (ELCNTL-FILE-ID)
01229 *         SET (ADDRESS OF CONTROL-FILE)
01230 *         RIDFLD (ELCNTL-KEY)
01231 *    END-EXEC
      *    MOVE '&"S        E          (   #00004554' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01232
01233      
      * EXEC CICS HANDLE CONDITION
01234 *         TERMIDERR (9560-BAD-TERMID)
01235 *         TRANSIDERR (9570-BAD-TRANSID)
01236 *    END-EXEC
      *    MOVE '"$[\                  ! ) #00004559' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034353539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01237
01247      
      * EXEC CICS START
01248 *         TRANSID (W-PRINT-TRANS)
01249 *         TERMID (CF-FORMS-PRINTER-ID)
01250 *         FROM (PROGRAM-INTERFACE-BLOCK)
01251 *         LENGTH (PI-COMM-LENGTH)
01252 *    END-EXEC
      *    MOVE '0( LFT                0   #00004564' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 W-PRINT-TRANS, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 CF-FORMS-PRINTER-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01253
01254      MOVE -1                             TO LINE1L
01255      MOVE ER-3784                        TO EMI-ERROR
01256      PERFORM 9900-ERROR-FORMAT           THRU 9900-EXIT
01257      GO TO 8200-SEND-DATAONLY
           .
01258
01259  9560-BAD-TERMID.
01260      MOVE ER-0412                        TO EMI-ERROR
01261      PERFORM 9900-ERROR-FORMAT           THRU 9900-EXIT
01262      GO TO 8200-SEND-DATAONLY
           .
01263
01264  9570-BAD-TRANSID.
01265      MOVE ER-0413                        TO EMI-ERROR
01266      PERFORM 9900-ERROR-FORMAT           THRU 9900-EXIT
01267      GO TO 8200-SEND-DATAONLY
           .
01268
01270  9600-PGMID-ERROR.
01271      
      * EXEC CICS HANDLE CONDITION
01272 *         PGMIDERR (8300-SEND-TEXT)
01273 *    END-EXEC
      *    MOVE '"$L                   ! * #00004590' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034353930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01274
01275      MOVE PGM-NAME                       TO PI-CALLING-PROGRAM
01276      MOVE SPACES                         TO PI-ENTRY-CD-1
01277      MOVE XCTL-005                       TO PGM-NAME
01278      MOVE PGM-NAME                       TO LOGOFF-PGM
01279      MOVE PGMIDERR-MSG                   TO LOGOFF-FILL
01280
01281      GO TO 9300-XCTL
           .
01282
01283  9700-LINK-DATE-CONVERT.
01284
01285      MOVE LINK-ELDATCV                   TO PGM-NAME
01286      
      * EXEC CICS LINK
01287 *         PROGRAM (PGM-NAME)
01288 *         COMMAREA (DATE-CONVERSION-DATA)
01289 *         LENGTH (DC-COMM-LENGTH)
01290 *    END-EXEC
      *    MOVE '."C                   ''   #00004606' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
01292  9700-EXIT.
01293      EXIT.
01294
01295  9900-ERROR-FORMAT.
01296      IF NOT EMI-ERRORS-COMPLETE
01297          MOVE LINK-001                   TO PGM-NAME
01298          
      * EXEC CICS LINK
01299 *             PROGRAM (PGM-NAME)
01300 *             COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01301 *             LENGTH (EMI-COMM-LENGTH)
01302 *        END-EXEC
      *    MOVE '."C                   ''   #00004618' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
01303  9900-EXIT.
           EXIT.
01304
01305  9990-ABEND.
01306      MOVE LINK-004                       TO PGM-NAME
01307      MOVE DFHEIBLK                       TO EMI-LINE1
01308
01309      
      * EXEC CICS LINK
01310 *         PROGRAM (PGM-NAME)
01311 *         COMMAREA (EMI-LINE1)
01312 *         LENGTH (72)
01313 *    END-EXEC
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004632' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01314
01315      MOVE EMI-MESSAGE-AREA (1)           TO ERRMSGBO
01316      MOVE -1                             TO FUNCTL
01317
01318      GO TO 8100-SEND-INITIAL-MAP
01319
01320      
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6522' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
           .
01321
01322  9995-SECURITY-VIOLATION.
01323 *                                COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004664' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
01324
01325  9995-EXIT.
01326      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6522' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9400-CLEAR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     9600-PGMID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4610-ENDBR,
                     6000-NOT-OPEN,
                     4610-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7010-ENDBR,
                     6000-NOT-OPEN,
                     7010-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7299-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7590-TS-QIDERR,
                     7585-QID-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 9410-RETURN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 9560-BAD-TERMID,
                     9570-BAD-TRANSID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6522' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
