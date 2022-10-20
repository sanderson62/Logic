00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 ELXXX .
00009 *AUTHOR.    pablo.
00010 *           colleyville, texas.
00011
00012 *DATE-COMPILED.
00024 *REMARKS.
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR
00026 *    THE CERTIFICATE LOOK-UP.
00027
00057
00058      EJECT
00059  ENVIRONMENT DIVISION.
00060
00061  DATA DIVISION.
00062
00063  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00064
00065
00066  77  FILLER  PIC X(32)  VALUE '********************************'.
00067  77  FILLER  PIC X(32)  VALUE '*    ELxxx WORKING STORAGE     *'.
00068  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
00069
       01  dm-record.
           12  dm-control-primary          pic x(09).
           12  dm-rest-of-record           pic x(200).
00074  01  WS-DATE-AREA.
00075      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
00076      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
00077
00078  01  FILLER                          COMP-3.
00079      05  WS-READNEXT-SW              PIC x     value ' '.
               88  the-end-of-the-world              value 'Y'.
00083
00084  01  FILLER         COMP SYNC.
00085      05  SC-ITEM                     PIC S9(4)    VALUE +0001.
00086
00087  01  FILLER.
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           05  ws-browse-sw                pic x     value ' '.
               88  browse-started                    value 'y'.
00088      05  XCTL-725                    PIC X(8)     VALUE 'EL725'.
00089      05  QID.
00090          10  QID-TERM                PIC X(4).
00091          10  FILLER                  PIC X(4)     VALUE '127A'.
00092      05  QID-ITEM                    PIC S9(4)    VALUE +1 COMP.
00093      05  WS-KEY-LENGTH               PIC S9(4)    VALUE +0 COMP.
00094
00095      05  PART-KEY-ON-SW              PIC X(01)    VALUE 'N'.
00096          88  PART-KEY-ON                          VALUE 'Y'.
00097
00098      05  PART-FIELD-ON-SW            PIC X(01)    VALUE ' '.
00099          88  PART-FIELD-ACCT                      VALUE 'A'.
00100          88  PART-FIELD-STATE                     VALUE 'S'.
00101          88  PART-FIELD-CERT                      VALUE 'C'.
00102
00103      05  WS-CNTL-KEY.
00104          10  WS-CNTL-ID              PIC X(3).
00105          10  WS-CNTL-TYPE            PIC X.
00106          10  WS-CNTL-USER            PIC X(4)     VALUE SPACES.
00107          10  WS-CNTL-SEQ             PIC S9(4)    VALUE +0 COMP.
00108
00109      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL127S'.
00110      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127A'.
00111
00112      05  FILLER                      REDEFINES
00113          WS-MAP-NAME.
00114          10  FILLER                  PIC XX.
00115          10  WS-MAP-NUMBER           PIC X(4).
00116          10  FILLER                  PIC XX.
00117
00118      05  THIS-PGM                    PIC X(8)     VALUE 'EL127'.
00119
00120      05  WS-CNTL-REC-FOUND-SW        PIC X(01)    VALUE SPACE.
00121      05  WS-NEXT-COMPANY-ID          PIC X(03)    VALUE SPACES.
00122
00123      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
00124      05  WS-ACCOUNT-MASTER-DSID      PIC X(8)     VALUE 'ERACCT2'.
00125      05  WS-CERT-MASTER-DSID         PIC X(8)     VALUE 'ELCERT'.
00126      05  WS-CERT-AIX01-DSID          PIC X(8)     VALUE 'ELCERT2'.
00127      05  WS-CERT-AIX02-DSID          PIC X(8)     VALUE 'ELCERT3'.
00128      05  WS-CERT-AIX03-DSID          PIC X(8)     VALUE 'ELCERT4'.
00129      05  WS-CERT-AIX04-DSID          PIC X(8)     VALUE 'ELCERT5'.
00130      05  WS-CERT-AIX05-DSID          PIC X(8)     VALUE 'ELCERT6'.
00131
00132      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX1'.
00133
00134      05  WK-SC-STATE.
00135          12  WK-SC-STATE-1           PIC X.
00136          12  WK-SC-STATE-2           PIC X.
00137
00138      05  WK-SC-CERT.
00139          12  WK-SC-CERT-1            PIC X.
00140          12  WK-SC-CERT-2            PIC X.
00141          12  WK-SC-CERT-3            PIC X.
00142          12  WK-SC-CERT-4            PIC X.
00143          12  WK-SC-CERT-5            PIC X.
00144          12  WK-SC-CERT-6            PIC X.
00145          12  WK-SC-CERT-7            PIC X.
00146          12  WK-SC-CERT-8            PIC X.
00147          12  WK-SC-CERT-9            PIC X.
00148          12  WK-SC-CERT-10           PIC X.
00149
00150      05  WS-DEEDIT-FIELD             PIC X(15)    VALUE ZERO.
00151
00152      05  WS-DEEDIT-FIELD-V0          REDEFINES
00153          WS-DEEDIT-FIELD             PIC S9(15).
00154
00155      05  WS-INPUT-FIELD              PIC X(50)    VALUE SPACES.
00156
00157      05  WS-INPUT-CHAR               REDEFINES
00158          WS-INPUT-FIELD              PIC X
00159          OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.
00160
00161  01  WS-FIRST-NAME.
00162      05  WS-FIRST-INITIAL            PIC X        VALUE SPACES.
00163      05  WS-FIRST-REST               PIC X(14)    VALUE SPACES.
00164
00165  01  WS-INITIALS.
00166      05  WS-INITIAL-FIRST            PIC X        VALUE SPACES.
00167      05  WS-INITIAL-MIDDLE           PIC X        VALUE SPACES.
00168
00169      05  PI-ACCOUNT-KEY.
00170          10  PI-AK-COMPANY-CD        PIC X.
00171          10  PI-AK-CARRIER           PIC X.
00172          10  PI-AK-GROUP             PIC X(06).
00173          10  PI-AK-STATE             PIC XX.
00174          10  PI-AK-ACCOUNT           PIC X(10).
00175          10  PI-AK-EXPIRE-DATE       PIC XX.
00176
00177      EJECT
00178      05  ERROR-MESSAGES.
00179          10  ER-0004                 PIC X(4)     VALUE '0004'.
00180          10  ER-0008                 PIC X(4)     VALUE '0008'.
00181          10  ER-0019                 PIC X(4)     VALUE '0019'.
00182          10  ER-0022                 PIC X(4)     VALUE '0022'.
00183          10  ER-0029                 PIC X(4)     VALUE '0029'.
00184          10  ER-0070                 PIC X(4)     VALUE '0070'.
00185          10  ER-0089                 PIC X(4)     VALUE '0089'.
00186          10  ER-0194                 PIC X(4)     VALUE '0194'.
00187          10  ER-0195                 PIC X(4)     VALUE '0195'.
00188          10  ER-0196                 PIC X(4)     VALUE '0196'.
00189          10  ER-0197                 PIC X(4)     VALUE '0197'.
00190          10  ER-0198                 PIC X(4)     VALUE '0198'.
00191          10  ER-0201                 PIC X(4)     VALUE '0201'.
00192          10  ER-0210                 PIC X(4)     VALUE '0210'.
00193          10  ER-0215                 PIC X(4)     VALUE '0215'.
00194          10  ER-0216                 PIC X(4)     VALUE '0216'.
00195          10  ER-0228                 PIC X(4)     VALUE '0228'.
00196          10  ER-0488                 PIC X(4)     VALUE '0488'.
00197          10  ER-0671                 PIC X(4)     VALUE '0671'.
00198          10  ER-0764                 PIC X(4)     VALUE '0764'.
00199          10  ER-0765                 PIC X(4)     VALUE '0765'.
00200          10  ER-2370                 PIC X(4)     VALUE '2370'.
00201          10  ER-2371                 PIC X(4)     VALUE '2371'.
00202          10  ER-2373                 PIC X(4)     VALUE '2373'.
00203          10  ER-8100                 PIC X(4)     VALUE '8100'.
00204          10  ER-8101                 PIC X(4)     VALUE '8101'.
00205          10  ER-8102                 PIC X(4)     VALUE '8102'.
00206          10  ER-8103                 PIC X(4)     VALUE '8103'.
00207          10  ER-8104                 PIC X(4)     VALUE '8104'.
00208          10  ER-8105                 PIC X(4)     VALUE '8105'.
00209          10  ER-8106                 PIC X(4)     VALUE '8106'.
00210          10  ER-8107                 PIC X(4)     VALUE '8107'.
00211
00220
00221
00226 *                                    COPY ELCDATE.
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
00227
00239
00246
00247      EJECT
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
00249  01  DFHCOMMAREA                     PIC X(1024).
00250
00266
00267      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELXXX' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00269
00270      CONTINUE.
00271
00275      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
00276      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE
00277      perform 0015-startbr-erdump thru 0015-exit
           perform 0020-read-next      thru 0020-exit
           perform 0030-process        thru 0030-exit until
               the-end-of-the-world
00328      .
00329  0015-startbr-erdump.
00812      move zeros         to Dm-control-PRimary.
00813      
      * EXEC CICS STARTBR
00814 *        DATASET   ('erdump')
00815 *        RIDFLD    (dm-control-primary)
00816 *        GTEQ
      *        resp      (ws-response)
00817 *    END-EXEC
           MOVE 'erdump' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00000485' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 dm-control-primary, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              set browse-started to true
           end-if
           .
       0015-exit.
           exit.
       0020-read-next.
00822      
      * EXEC CICS READNEXT
00823 *        DATASET   ('erdump')
00824 *        RIDFLD    (dm-control-primary)
00825 *        into      (dm-record)
      *        resp      (ws-response)
00826 *    END-EXEC.
           MOVE LENGTH OF
            dm-record
             TO DFHEIV12
           MOVE 'erdump' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00000498' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 dm-record, 
                 DFHEIV12, 
                 dm-control-primary, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00827
       0020-exit.
           exit.
       0030-process.
           if resp-normal
00350         move dm-rest-of-record   to date-conversion-data
              perform 8500-date-conversion
           else
              if browse-started
                 move ' ' to ws-browse-sw
                 perform 0040-end-browse thru 0040-exit
                 move zeros  to dm-control-primary
pemtst           perform 0015-startbr-erdump thru 0015-exit
              end-if
           end-if
00352
00353      PERFORM 0020-READ-NEXT thru 0020-exit
00354      .
01737  0030-exit.
           exit.
00329  0040-end-browse.
00813      
      * EXEC CICS endbr
00814 *        DATASET   ('erdump')
00817 *    END-EXEC
           MOVE 'erdump' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00000525' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303030353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0040-exit.
           exit.
01738      EJECT
01754  8500-DATE-CONVERSION SECTION.
01755      
      * EXEC CICS LINK
01756 *        PROGRAM  ('ELDATCV')
01757 *        COMMAREA (DATE-CONVERSION-DATA)
01758 *        LENGTH   (DC-COMM-LENGTH)
01759 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00000533' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01760
01761  8500-EXIT.
01762      EXIT.
01763
01773      EJECT
01864  9999-LAST-PARAGRAPH SECTION.
01865      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELXXX' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELXXX' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELXXX' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
