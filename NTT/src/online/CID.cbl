      *((program: CID.cl2))
000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. CID.
000003 AUTHOR. Paul.
000004 DATE-COMPILED.
000005*SECURITY.   *****************************************************
000006*            *                                                   *
000007*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
000008*            *                                                   *
000009*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000010*            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
000011*            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
000012*            *                                                   *
000013*            *****************************************************
000014
000015******************************************************************
000016*REMARKS.                                                        *
000017*  Main menu for CSO's QuickCalc                                 *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 101821                   PEMA  New Program
000027******************************************************************
000028*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
000029 ENVIRONMENT DIVISION.
000030*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
000031 CONFIGURATION SECTION.
000032*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
000033 INPUT-OUTPUT SECTION.
000034 FILE-CONTROL.
000035     SELECT MAIN-CTRL-FILE ASSIGN TO 'MAINCTRL.DAT[X]'
000036         ORGANIZATION IS INDEXED
000037         ACCESS MODE IS DYNAMIC
000038         RECORD KEY IS MAIN-CTRL-KEY
000039         FILE STATUS IS MAIN-CTRL-FILE-ERR.
000040*
000041     SELECT LAST-STATE-FILE ASSIGN TO 'LAST-STATE.DAT[N]'
000042         ORGANIZATION IS SEQUENTIAL
000043         FILE STATUS IS LAST-STATE-FILE-ERR.
000044*
000045     SELECT AGENT-FILE ASSIGN TO dynamic AGENT-FILE-PATH
000046         ORGANIZATION IS SEQUENTIAL
000047         ACCESS MODE IS SEQUENTIAL
000048         FILE STATUS IS AGENT-FILE-ERR.
000049*
000050     SELECT PROTECT-AGENT-FILE ASSIGN TO 'PROTECT.DAT[N]'
000051         ORGANIZATION IS SEQUENTIAL
000052         ACCESS MODE IS SEQUENTIAL
000053         FILE STATUS IS PROTECT-AGENT-FILE-ERR.
000054*
000055     SELECT LIMIT-FILE ASSIGN TO dynamic LIMIT-FILE-PATH
000056         ORGANIZATION IS SEQUENTIAL
000057         ACCESS MODE IS SEQUENTIAL
000058         FILE STATUS IS LIMIT-FILE-ERR.
000059*
000060     SELECT ACCT-ET-FILE ASSIGN TO dynamic ACCT-ET-FILE-PATH
000061         ORGANIZATION IS SEQUENTIAL
000062         ACCESS MODE IS SEQUENTIAL
000063         FILE STATUS IS ACCT-ET-FILE-ERR.
000064*
000065     SELECT HOEPA-TRIGGER-FILE ASSIGN TO 'Hoepa Trigger.DAT[X]'
000066         ORGANIZATION IS INDEXED
000067         ACCESS MODE IS DYNAMIC
000068         RECORD KEY IS HOEPA-TRIGGER-KEY
000069         FILE STATUS IS HOEPA-TRIGGER-FILE-ERR.
000070*
000071     SELECT LOANS-NA-FILE ASSIGN TO 'QC-Loan-Type-NA.DAT[X]'
000072            ORGANIZATION IS INDEXED
000073            ACCESS MODE IS DYNAMIC
000074            RECORD KEY IS LOANS-NA-KEY
000075            FILE STATUS IS LOANS-NA-FILE-ERR.
000076*
000077*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
000078 DATA DIVISION.
000079*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
000080 FILE SECTION.
000081* -====-
000082 FD  MAIN-CTRL-FILE
000083         LABEL RECORDS ARE STANDARD.
000084* -====-
000085 01  MAIN-CTRL-RECORD.
000086     05  MAIN-CTRL-KEY.
000087       10  MAIN-CTRL-STATE            PIC X(20).
000088       10  MAIN-CTRL-PRODUCT          PIC X(35).
000089     05  MAIN-CTRL-DATA.
000090       10  MAIN-CTRL-PRODUCT-DLL      PIC X(20).
000091       10  MAIN-CTRL-RATE-FILE-REQUIRED
000092                                      PIC X.
000093       10  FILLER OCCURS 40 TIMES.
000094         15  MAIN-CTRL-RATE-FILE-DESCR
000095                                      PIC X(60).
000096         15  MAIN-CTRL-RATE-FILE-NAME PIC X(60).
000097         15  MAIN-CTRL-LOAN-REFUND-TYPE
000098                                      PIC X(50).
000099* -====-
000100 FD  LAST-STATE-FILE
000101         LABEL RECORDS ARE STANDARD.
000102* -====-
000103 01  LAST-STATE-RECORD.
000104     05  LAST-STATE                   PIC X(20).
000105     05  LAST-PRODUCT                 PIC X(35).
000106* -====-
000107 FD  AGENT-FILE
000108         LABEL RECORDS ARE STANDARD.
000109*
000110 01  AGENT-RECORD.
000111     05  AGENT-NAME                   PIC X(0030).
000112     05  AGENT-COMPANY                PIC X(0030).
000113     05  AGENT-ADDRESS                PIC X(0030).
000114     05  AGENT-STATE                  PIC X(0030).
000115     05  AGENT-PHONE                  PIC 9(010).
000116     05  AGENT-PHONE-EXT              PIC 9(005).
000117     05  AGENT-COLOR-TRIGGER          PIC 9(002)V99.
000118* -====-
000119 FD  PROTECT-AGENT-FILE
000120         LABEL RECORDS ARE STANDARD.
000121*
000122 01  PROTECT-AGENT-RECORD             PIC X.
000123* -====-
000124 FD  LIMIT-FILE
000125         LABEL RECORDS ARE STANDARD.
000126* -====-
000127 01  LIMIT-ID-RECORD                  PIC X(50).
000128 01  LIMIT-RECORD                     PIC X(20000).
000129* -====-
000130 FD  ACCT-ET-FILE
000131         LABEL RECORDS ARE STANDARD.
000132* -====-
000133 01  ACCT-ET-RECORD.
000134     05  ACCT-ET-ID                   PIC X(0050).
000135     05  ACCT-ET-GROUP-NAME-A         PIC X(0020).
000136     05  ACCT-ET-GROUP-NAME-B         PIC X(0020).
000137     05  ACCT-ET-GROUP-NAME-C         PIC X(0020).
000138     05  ACCT-ET-DATA.
000139       10  FILLER OCCURS 82.
000140         15  ACCT-ET-TERM-AGE-A       PIC 9(002).
000141         15  ACCT-ET-L-MAXI-TERM-A    PIC 9(003).
000142         15  ACCT-ET-L-MAXI-AMT-A     PIC 9(007).
000143         15  ACCT-ET-L-ADD-SIGNATURE-A
000144                                      PIC X(020).
000145         15  ACCT-ET-D-S-MAXI-TERM-A  PIC 9(003).
000146         15  ACCT-ET-D-J-MAXI-TERM-A  PIC 9(003).
000147         15  ACCT-ET-D-MN-MAXI-PAY-A  PIC 9(005).
000148         15  ACCT-ET-D-TOTAL-PAY-A    PIC 9(007).
000149       10  FILLER OCCURS 82.
000150         15  ACCT-ET-TERM-AGE-B       PIC 9(002).
000151         15  ACCT-ET-L-MAXI-TERM-B    PIC 9(003).
000152         15  ACCT-ET-L-MAXI-AMT-B     PIC 9(007).
000153         15  ACCT-ET-L-ADD-SIGNATURE-B
000154                                      PIC X(020).
000155         15  ACCT-ET-D-S-MAXI-TERM-B  PIC 9(003).
000156         15  ACCT-ET-D-J-MAXI-TERM-B  PIC 9(003).
000157         15  ACCT-ET-D-MN-MAXI-PAY-B  PIC 9(005).
000158         15  ACCT-ET-D-TOTAL-PAY-B    PIC 9(007).
000159       10  FILLER OCCURS 82.
000160         15  ACCT-ET-TERM-AGE-C       PIC 9(002).
000161         15  ACCT-ET-L-MAXI-TERM-C    PIC 9(003).
000162         15  ACCT-ET-L-MAXI-AMT-C     PIC 9(007).
000163         15  ACCT-ET-L-ADD-SIGNATURE-C
000164                                      PIC X(020).
000165         15  ACCT-ET-D-S-MAXI-TERM-C  PIC 9(003).
000166         15  ACCT-ET-D-J-MAXI-TERM-C  PIC 9(003).
000167         15  ACCT-ET-D-MN-MAXI-PAY-C  PIC 9(005).
000168         15  ACCT-ET-D-TOTAL-PAY-C    PIC 9(007).
000169* -====-
000170 FD  HOEPA-TRIGGER-FILE
000171         LABEL RECORDS ARE STANDARD.
000172* -====-
000173 01  HOEPA-TRIGGER-RECORD.
000174     05  HOEPA-TRIGGER-KEY.
000175       10  HOEPA-TRIGGER-STATE        PIC X(20).
000176     05  HOEPA-TRIGGER-DATA.
000177       10  HOEPA-TRIGGER-RATE         PIC 99V99 COMP-5.
000178* -====-
000179 FD  LOANS-NA-FILE
000180            LABEL RECORDS ARE STANDARD.
000181* -====-
000182 01  LOANS-NA-RECORD.
000183     05  LOANS-NA-KEY.
000184         10  LOANS-NA-STATE             PIC X(20).
000185         10  LOANS-NA-RATE-FILE-DESCR   PIC X(60).
000186         10  LOANS-NA-LOAN-TYPE-DESCR   PIC X(50).
000187     05  LOANS-NA-DATA                  PIC X.
000188* -====-
000189*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
000190 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000191* -====-
000192 01  MAIN-CTRL-FILE-ERR               PIC XX.
000193 01  LAST-STATE-FILE-ERR              PIC XX.
000194 01  AGENT-FILE-ERR                   PIC XX.
000195 01  PROTECT-AGENT-FILE-ERR           PIC XX.
000196 01  LIMIT-FILE-ERR                   PIC XX.
000197 01  ACCT-ET-FILE-ERR                 PIC XX.
000198 01  HOEPA-TRIGGER-FILE-ERR           PIC XX.
000199 01  LOANS-NA-FILE-ERR                PIC XX.
000200* -====-
000201 01  AGENT-DLL                        PIC X(9) VALUE 'AGENT.DLL'.
000202* -====-
000203 01  AGENT-FILE-PATH                  PIC X(260).
000204 01  LIMIT-FILE-PATH                  PIC X(260).
000205 01  ACCT-ET-FILE-PATH                PIC X(260).
000206* -====-
000207 01  LIMIT-SOFTWARE-ID                PIC X(50) VALUE
000208                                             'CSO CID QuikCalc 4'.
000209* -====-
000210 01  ACCT-ET-SOFTWARE-ID              PIC X(50) VALUE
000211                               'CSO CID QuikCalc Extended Term 1'.
000212* -====-
000213 01  CARRIAGE-RETURN                  PIC X(1) VALUE X'0D'.
000214* -====-
000215 01  CNT-1                            PIC S9(4) COMP-5.
000216 01  CNT-2                            PIC S9(4) COMP-5.
000217 01  CNT-3                            PIC S9(4) COMP-5.
000218 01  CNT-4                            PIC S9(4) COMP-5.
000219* -====-
000220 01  STATE-ABBR                       PIC X(2).
000221* -====-
000222 01  TEMP-FILE-EXT                    PIC X(3).
000223* -====-
000224 01  FLAG-1                           PIC X.
000225 01  FLAG-2                           PIC X.
000226* -====-
000227 01  STATE-FLAG                       PIC X.
000228* -====-
000229 01  MAIN-MENU-OPTIONS.
000230     05  MENU-OPT-RUN                 PIC S9(4) COMP-5
000231                                      VALUE 23.
000232     05  MENU-OPT-AGENT               PIC S9(4) COMP-5
000233                                      VALUE 22.
000234     05  MENU-OPT-EXIT                PIC S9(4) COMP-5
000235                                      VALUE 21.
000236     05  MENU-OPT-ABOUT               PIC S9(4) COMP-5
000237                                      VALUE 41.
000238     05  MENU-OPT-GLOSSARY            PIC S9(4) COMP-5
000239                                      VALUE 42.
000240* -====-
000241 01  TEMP-MAIN-PRODUCT                PIC X(35).
000242* -====-
000243 01  CURRENT-DIRECTORY-NAME           PIC X(260).
000244 01  BUFFER-LENGTH                    PIC S9(9) BINARY.
000245 01  STATUS-CODE                      PIC S9(9) BINARY.
000246* -====-
000247 01  PASSED-DATA.
000248     05  PASSED-PRODUCT               PIC X(35).
000249     05  PASSED-STATE                 PIC X(20).
000250     05  PASSED-RATE-FILE-DESCR       PIC X(60).
000251     05  PASSED-RATE-FILE-NAME        PIC X(100).
000252     05  PASSED-LOAN-REFUND-TYPE      PIC X(50).
000253     05  PASSED-LIMIT-FILE-DESCR      PIC X(60).
000254     05  PASSED-LIMIT-FILE-NAME       PIC X(100).
000255     05  PASSED-FILE-FOOTNOTE         PIC X(100).
000256* -====-
000257 01  SEARCH-HANDLE POINTER.
000258* -====-
000259*01  COBOLFILESEARCH.
000260*    05  FILEATTRS                    PIC 9(9) BINARY.
000261*      88  FILE-ATTRIBUTE-READONLY    VALUE H'00000001'.
000262*      88  FILE-ATTRIBUTE-HIDDEN      VALUE H'00000002'.
000263*      88  FILE-ATTRIBUTE-SYSTEM      VALUE H'00000004'.
000264*      88  FILE-ATTRIBUTE-DIRECTORY   VALUE H'00000010'.
000265*      88  FILE-ATTRIBUTE-ARCHIVE     VALUE H'00000020'.
000266*      88  FILE-ATTRIBUTE-NORMAL      VALUE H'00000080'.
000267*      88  FILE-ATTRIBUTE-TEMPORARY   VALUE H'00000100'.
000268*      88  FILE-ATTRIBUTE-COMPRESSED  VALUE H'00000800'.
000269*    05  FILESIZE                     PIC 9(18) BINARY.
000270*
000271* The time foramt is the standard structure used
000272* COBOL intrinsic date functions
000273*
000274 01  filler.
000275     05  CREATIONTIME.
000276       10  CREATIONTIME-YEAR          PIC 9999.
000277       10  CREATIONTIME-MONTH         PIC 99.
000278       10  CREATIONTIME-DAY           PIC 99.
000279       10  CREATIONTIME-HOUR          PIC 99.
000280       10  CREATIONTIME-MINUTE        PIC 99.
000281       10  CREATIONTIME-SECOND        PIC 99.
000282       10  CREATIONTIME-HUNDREDTHS    PIC 99.
000283     05  LASTACCESS.
000284       10  LASTACCESS-YEAR            PIC 9999.
000285       10  LASTACCESS-MONTH           PIC 99.
000286       10  LASTACCESS-DAY             PIC 99.
000287       10  LASTACCESS-HOUR            PIC 99.
000288       10  LASTACCESS-MINUTE          PIC 99.
000289       10  LASTACCESS-SECOND          PIC 99.
000290       10  LASTACCESS-HUNDREDTHS      PIC 99.
000291     05  LASTWRITE.
000292       10  LASTWRITE-YEAR             PIC 9999.
000293       10  LASTWRITE-MONTH            PIC 99.
000294       10  LASTWRITE-DAY              PIC 99.
000295       10  LASTWRITE-HOUR             PIC 99.
000296       10  LASTWRITE-MINUTE           PIC 99.
000297       10  LASTWRITE-SECOND           PIC 99.
000298       10  LASTWRITE-HUNDREDTHS       PIC 99.
000299     05  FULLFILENAME                 PIC X(260).
000300* -====-
000301 01  SEARCHFILENAME                   PIC X(260).
000302* -====-
000303 01  SEARCHRESULT                     PIC S9(9) BINARY.
000304     88  SEARCH-OK                    VALUE 0.
000305     88  NO-MORE-FILES                VALUE 18.
000306* -====-
000307 01  PARSE-FULLFILENAME               PIC X(2).
000308* -====-
000309 01  SWITCH-FLAGS.
000310     05  HOME-OFFICE-FLAG             PIC X.
000311     05  F-I-ONLINE-FLAG              PIC X.
000312     05  SWITCH-FLAG-3                PIC X.
000313     05  SWITCH-FLAG-4                PIC X.
000314     05  SWITCH-FLAG-5                PIC X.
000315     05  SWITCH-FLAG-6                PIC X.
000316* -====-
000317 01  PARAMETER-ARRAY.
000318     05  PARAMETER-SWITCH             PIC X(20) OCCURS 6 TIMES.
000319* -====-
000320 01  PARAMETER-LINE                   PIC X(121).
000321 01  PARAMETER-SIZE                   PIC S9(9) BINARY.
000322* -====-
000323* Working-storage for the setval paragraph.  Id and len
000324* must be set before performing the paragraph
000325*
000326 01  SETVAL-STUFF.
000327     05  SETVAL-ID                    PIC S9(4) COMP-5.
000328     05  SETVAL-LEN                   PIC S9(4) COMP-5.
000329     05  SETVAL-1                     PIC S9(4) COMP-5.
000330     05  SETVAL-2                     PIC S9(4) COMP-5.
000331     05  SETVAL-SAVE                  PIC X(1000).
000332* -====-
000333* Setval-data used by the setval paragraph.  May be any
000334* number of array elements.
000335*
000336 01  SETVAL-DATA.
000337     05  RANGE-20                     PIC X(20) OCCURS 90 TIMES.
000338* -====-
000339 01  FILLER REDEFINES SETVAL-DATA.
000340     05  RANGE-30                     PIC X(30) OCCURS 60 TIMES.
000341* -====-
000342 01  FILLER REDEFINES SETVAL-DATA.
000343     05  RANGE-35                     PIC X(35) OCCURS 50 TIMES.
000344     05  FILLER                       PIC X(50).
000345* -====-
000346 01  FILLER REDEFINES SETVAL-DATA.
000347     05  RANGE-40                     PIC X(40) OCCURS 45 TIMES.
000348* -====-
000349 01  FILLER REDEFINES SETVAL-DATA.
000350     05  RANGE-60                     PIC X(60) OCCURS 30 TIMES.
000351* -====-
000352 01  FILLER REDEFINES SETVAL-DATA.
000353     05  RANGE-50                     PIC X(50) OCCURS 36 TIMES.
000354* -====-
000355 01  SYS-DATE-WITH-CC.
000356     05  SYS-CC                       PIC 99 VALUE 20.
000357     05  SYS-DATE.
000358       10  SYS-YY                     PIC 99.
000359       10  SYS-MM                     PIC 99.
000360       10  SYS-DD                     PIC 99.
000361* -====-
000362* Larger then default message box.
000363*********************************
000364* message definition            *
000365* parameter for DISPLAY-MESSAGE *
000366*********************************
000367*
000368 01  SP2-LG-MESSAGE-DATA.
000369     05  SP2-LG-MS-RET-CODE           PIC S9(4) COMP-5.
000370     05  SP2-LG-MS-LENS.
000371       10  SP2-LG-MS-LEN-LEN          PIC S9(4) COMP-5 VALUE +12.
000372       10  SP2-LG-MS-NUM-LEN          PIC S9(4) COMP-5 VALUE +2.
000373       10  SP2-LG-MS-CHAR-LEN         PIC S9(4) COMP-5 VALUE +4.
000374       10  SP2-LG-MS-VAR-LEN          PIC S9(4) COMP-5 VALUE
000375                                                            +2080.
000376       10  SP2-LG-MS-TITLE-LEN        PIC S9(4) COMP-5 VALUE +80.
000377       10  SP2-LG-MS-LINE-LEN         PIC S9(4) COMP-5 VALUE
000378                                                            +2000.
000379     05  SP2-LG-MS-DATA.
000380******** SP2-LG-MS-NUM-DATA ********
000381       10  SP2-LG-MS-LINE-CNT         PIC S9(4) COMP-5.
000382******** SP2-LG-MS-CHAR-DATA *******
000383       10  SP2-LG-MS-ICON             PIC X.
000384       10  SP2-LG-MS-BUTTON           PIC X.
000385       10  SP2-LG-MS-CANCEL           PIC X.
000386       10  SP2-LG-MS-REPLY            PIC X.
000387******** SP2-LG-MS-VAR-DATA *******
000388       10  SP2-LG-MS-TITLE            PIC X(80).
000389       10  SP2-LG-MS-TEXT             PIC X(2000).
000390* -====-
000391 01  VERSION-NO                       PIC 9.99 VALUE '1.98'.
000392* -====-
000393 01  RELEASE-DATE                     PIC X(10) VALUE
000394                                                     '01/02/2016'.
000395* -====-
000396*        COPY "SP2.CPY".
      *>>((file: SP2.CPY))
000001*********************
000002* SP2.CPY contains: *
000003* function codes    *
000004* parameter layouts *
000005* key values        *
000006* line-drawing codes*
000007*********************
000008
000009***************************************************
000010* CHANGES LOG                                     *
000011* 4/17/96 toolbar function codes                  *
000012* 4/17/96 sp2-wd-toolbar-rows                     *
000013* 4/17/96 sp2-key-toolbar                         *
000014* 5/6/96 vbx function codes                       *
000015* 5/6/96 sp2-vbx-parm                             *
000016* 5/6/96 sp2-key-vbx                              *
000017* 5/14/96 sp2-pd-syns-len                         *
000018* 5/18/96 sp2-cd-menu-option                      *
000019* 8/2/96 sp2-pd-div-width, sp2-pd-div-height      *
000020* 8/2/96 sp2-pd-help-len, sp2-pd-help-keyword     *
000021* 8/2/96 sp2-fd-help-len, sp2-fd-help-keyword     *
000022* 8/2/96 sp2-cd-next-hor, sp2-cd-last-hor         *
000023* 8/2/96 sp2-delete-static                        *
000024* 10/1/96 set-icon-file-name                      *
000025* 10/23/96 sp2-mark-field                         *
000026* 10/30/96 sp2-set-keyboard-buffer                *
000027* 11/19/96 sp2-fd-misc-options                    *
000028* 12/31/96 sp2-key-escape redefines sp2-key-esc   *
000029* 1/2/96 remove unused items from menu-def        *
000030* 1/31/96 sp2-reserve-memory                      *
000031* 2/28/97 correct sp2-pd-var-len                  *
000032* 4/22/97 sp2-fo-char-set                         *
000033* 5/14/97 sp2-pd-cell-size, sp2-wd-cell-size      *
000034* 8/7/97 sp2-get-field-data                       *
000035* 8/18/98 sp2-set-configuration                   *
000036* 9/2/98 sp2-execute-program                      *
000037* 11/10/98 sp2-rx-long-data                       *
000038* 11/10/98 sp2-get-repeat-ext                     *
000039* 1/6/99 sp2-execute-client-prog                  *
000040* 3/19/99 sp2-copy-file funcs                     *
000041* 3/24/99 sp2-get-command-line                    *
000042* 5/24/99 thin client funcs into sp2tc.cpy        *
000043* 5/24/99 sp2-execute-program-2                   *
000044* 6/3/99 pd-options-3                             *
000045* 11/8/99 wd-options-3                            *
000046* 11/16/99 rd-misc-options                        *
000047* 2/8/00 wd-div-width/height                      *
000048* 7/28/00 fo-decipoints                           *
000049* 8/1/00 key-scroll-click                         *
000050* 8/1/00 key-switch-denied                        *
000051* 9/21/00 correct fo-num-len                      *
000052* 9/21/00 increase pd-var-lens                    *
000053* 3/14/01 property stuff                          *
000054* 11/27/01 key-sys-shutdown, key-app-close        *
000055***************************************************
000056
000057**********************
000058* SP2 FUNCTION CODES *
000059**********************
000060
000061 01  SP2-FUNCTION-CODES.
000062******** primary functions ******
000063     05  SP2-OPEN-FILE           PIC S9(4) COMP-5 VALUE +1.
000064     05  SP2-CLOSE-WINDOW        PIC S9(4) COMP-5 VALUE +12.
000065     05  SP2-END-SESSION         PIC S9(4) COMP-5 VALUE +16.
000066     05  SP2-CONVERSE-PANEL      PIC S9(4) COMP-5 VALUE +19.
000067     05  SP2-CLOSE-FILE          PIC S9(4) COMP-5 VALUE +23.
000068******** window functions *******
000069     05  SP2-OPEN-WINDOW         PIC S9(4) COMP-5 VALUE +0.
000070     05  SP2-DISPLAY-WINDOW      PIC S9(4) COMP-5 VALUE +3.
000071     05  SP2-CLEAR-WINDOW        PIC S9(4) COMP-5 VALUE +11.
000072     05  SP2-ACTIVATE-WINDOW     PIC S9(4) COMP-5 VALUE +15.
000073     05  SP2-SET-WINDOW-DEF      PIC S9(4) COMP-5 VALUE +33.
000074     05  SP2-GET-WINDOW-DEF      PIC S9(4) COMP-5 VALUE +34.
000075     05  SP2-ACTIVATE-INTERNAL   PIC S9(4) COMP-5 VALUE +73.
000076******** panel functions ********
000077     05  SP2-READ-PANEL          PIC S9(4) COMP-5 VALUE +2.
000078     05  SP2-GET-INPUT           PIC S9(4) COMP-5 VALUE +4.
000079     05  SP2-SET-PANEL-DEF       PIC S9(4) COMP-5 VALUE +5.
000080     05  SP2-GET-PANEL-DEF       PIC S9(4) COMP-5 VALUE +6.
000081     05  SP2-SET-PANEL-FIELDS    PIC S9(4) COMP-5 VALUE +13.
000082     05  SP2-WRITE-PANEL         PIC S9(4) COMP-5 VALUE +22.
000083     05  SP2-COPY-PANEL          PIC S9(4) COMP-5 VALUE +35.
000084     05  SP2-SET-PANEL-COLORS    PIC S9(4) COMP-5 VALUE +42.
000085     05  SP2-SET-PANEL-TYPES     PIC S9(4) COMP-5 VALUE +43.
000086     05  SP2-READ-NEXT-PANEL     PIC S9(4) COMP-5 VALUE +56.
000087     05  SP2-DELETE-RECORD       PIC S9(4) COMP-5 VALUE +60.
000088     05  SP2-DISPLAY-PANEL       PIC S9(4) COMP-5 VALUE +70.
000089     05  SP2-CLEAR-PANEL         PIC S9(4) COMP-5 VALUE +71.
000090******** field functions ********
000091     05  SP2-SET-FIELD-DEF       PIC S9(4) COMP-5 VALUE +7.
000092     05  SP2-GET-FIELD-DEF       PIC S9(4) COMP-5 VALUE +8.
000093     05  SP2-DELETE-FIELD        PIC S9(4) COMP-5 VALUE +36.
000094     05  SP2-DISPLAY-FIELD       PIC S9(4) COMP-5 VALUE +58.
000095     05  SP2-GET-NEXT-FIELD-DEF  PIC S9(4) COMP-5 VALUE +67.
000096     05  SP2-MARK-FIELD          PIC S9(4) COMP-5 VALUE +84.
000097     05  SP2-GET-FIELD-DATA      PIC S9(4) COMP-5 VALUE +89.
000098******** static field functions *
000099     05  SP2-SET-STATIC-DEF      PIC S9(4) COMP-5 VALUE +9.
000100     05  SP2-GET-STATIC-DEF      PIC S9(4) COMP-5 VALUE +10.
000101     05  SP2-GET-NEXT-STATIC-DEF PIC S9(4) COMP-5 VALUE +68.
000102     05  SP2-DELETE-STATIC       PIC S9(4) COMP-5 VALUE +82.
000103******** group functions ********
000104     05  SP2-SET-GROUP-DEF       PIC S9(4) COMP-5 VALUE +28.
000105     05  SP2-GET-GROUP-DEF       PIC S9(4) COMP-5 VALUE +29.
000106     05  SP2-DELETE-GROUP        PIC S9(4) COMP-5 VALUE +39.
000107     05  SP2-GET-NEXT-GROUP-DEF  PIC S9(4) COMP-5 VALUE +69.
000108******** repeat group functions *
000109     05  SP2-SET-REPEAT-DEF      PIC S9(4) COMP-5 VALUE +20.
000110     05  SP2-GET-REPEAT-DEF      PIC S9(4) COMP-5 VALUE +21.
000111     05  SP2-DELETE-REPEAT       PIC S9(4) COMP-5 VALUE +37.
000112     05  SP2-SET-REPEAT-EXT      PIC S9(4) COMP-5 VALUE +57.
000113     05  SP2-GET-REPEAT-EXT      PIC S9(4) COMP-5 VALUE +106.
000114******** font functions *********
000115     05  SP2-SET-FONT-DEF        PIC S9(4) COMP-5 VALUE +30.
000116     05  SP2-GET-FONT-DEF        PIC S9(4) COMP-5 VALUE +31.
000117     05  SP2-WRITE-FONTS         PIC S9(4) COMP-5 VALUE +32.
000118     05  SP2-QUERY-FONT          PIC S9(4) COMP-5 VALUE +61.
000119     05  SP2-DELETE-FONT         PIC S9(4) COMP-5 VALUE +122.
000120******** color functions ********
000121     05  SP2-SET-COLOR-DEF       PIC S9(4) COMP-5 VALUE +40.
000122     05  SP2-WRITE-COLORS        PIC S9(4) COMP-5 VALUE +41.
000123     05  SP2-GET-COLOR-DEF       PIC S9(4) COMP-5 VALUE +45.
000124     05  SP2-QUERY-COLOR         PIC S9(4) COMP-5 VALUE +62.
000125     05  SP2-DELETE-COLOR        PIC S9(4) COMP-5 VALUE +123.
000126******** menu functions *********
000127     05  SP2-SET-MENU-DEF        PIC S9(4) COMP-5 VALUE +17.
000128     05  SP2-READ-MENU           PIC S9(4) COMP-5 VALUE +25.
000129     05  SP2-WRITE-MENU          PIC S9(4) COMP-5 VALUE +26.
000130     05  SP2-GET-MENU-DEF        PIC S9(4) COMP-5 VALUE +38.
000131     05  SP2-SET-MENU-OPTION     PIC S9(4) COMP-5 VALUE +46.
000132     05  SP2-GET-MENU-OPTION     PIC S9(4) COMP-5 VALUE +47.
000133     05  SP2-INSERT-MENU-OPTION  PIC S9(4) COMP-5 VALUE +51.
000134     05  SP2-DELETE-MENU-OPTION  PIC S9(4) COMP-5 VALUE +52.
000135     05  SP2-CLEAR-MENU          PIC S9(4) COMP-5 VALUE +55.
000136******** message functions ******
000137     05  SP2-DISPLAY-MESSAGE     PIC S9(4) COMP-5 VALUE +48.
000138     05  SP2-SET-MOUSE-SHAPE     PIC S9(4) COMP-5 VALUE +59.
000139******** clipboard functions ****
000140     05  SP2-SET-CLIPBOARD       PIC S9(4) COMP-5 VALUE +53.
000141     05  SP2-GET-CLIPBOARD       PIC S9(4) COMP-5 VALUE +54.
000142******** toolbar functions ******
000143     05  SP2-OPEN-TOOLBAR        PIC S9(4) COMP-5 VALUE +75.
000144     05  SP2-ACTIVATE-TOOLBAR    PIC S9(4) COMP-5 VALUE +76.
000145     05  SP2-DEACTIVATE-TOOLBAR  PIC S9(4) COMP-5 VALUE +77.
000146     05  SP2-DISPLAY-TOOLBAR     PIC S9(4) COMP-5 VALUE +78.
000147     05  SP2-CLOSE-TOOLBAR       PIC S9(4) COMP-5 VALUE +79.
000148******** file functions *********
000149     05  SP2-CREATE-FILE         PIC S9(4) COMP-5 VALUE +24.
000150     05  SP2-QUERY-FILE          PIC S9(4) COMP-5 VALUE +63.
000151     05  SP2-SET-ICON-FILE-NAME  PIC S9(4) COMP-5 VALUE +83.
000152******** miscellaneous functions
000153     05  SP2-CHECK-OLD-VERSION   PIC S9(4) COMP-5 VALUE +44.
000154     05  SP2-GET-VERSION         PIC S9(4) COMP-5 VALUE +64.
000155     05  SP2-SET-TEXT-MODE       PIC S9(4) COMP-5 VALUE +66.
000156     05  SP2-SET-KEYBOARD-BUFFER PIC S9(4) COMP-5 VALUE +85.
000157     05  SP2-RESERVE-MEMORY      PIC S9(4) COMP-5 VALUE +88.
000158     05  SP2-SET-CONFIGURATION   PIC S9(4) COMP-5 VALUE +92.
000159     05  SP2-SET-PROPERTY        PIC S9(4) COMP-5 VALUE +93.
000160     05  SP2-EXECUTE-PROGRAM-2   PIC S9(4) COMP-5 VALUE +97.
000161     05  SP2-GET-PROPERTY        PIC S9(4) COMP-5 VALUE +121.
000162******** add-on functions *******
000163     05  SP2-SET-VBX             PIC S9(4) COMP-5 VALUE +80.
000164     05  SP2-GET-VBX             PIC S9(4) COMP-5 VALUE +81.
000165
000166*************************
000167* END OF FUNCTION CODES *
000168*************************
000169
000170*************************
000171* SP2 PARAMETER LAYOUTS *
000172*************************
000173
000174******************************
000175* file definition            *
000176* parameter for OPEN-FILE    *
000177* also used with CREATE-FILE *
000178*                QUERY-FILE  *
000179******************************
000180
000181 01  SP2-FILE-DEF.
000182     05  SP2-FI-RET-CODE         PIC S9(4) COMP-5.
000183     05  SP2-FI-LENS.
000184         10  SP2-FI-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
000185         10  SP2-FI-NUM-LEN      PIC S9(4) COMP-5 VALUE +0.
000186         10  SP2-FI-CHAR-LEN     PIC S9(4) COMP-5 VALUE +2.
000187         10  SP2-FI-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
000188         10  SP2-FI-NAME-LEN     PIC S9(4) COMP-5 VALUE +80.
000189     05  SP2-FI-DATA.
000190******** SP2-FI-CHAR-DATA *******
000191         10  SP2-FI-MODE         PIC X.
000192         10  SP2-FI-SHARE        PIC X.
000193******** SP2-FI-VAR-DATA ********
000194         10  SP2-FI-NAME         PIC X(80).
000195
000196********************************
000197* parameter for CONVERSE-PANEL *
000198* also used with GET-INPUT     *
000199********************************
000200
000201**************************************************************
000202* this parameter will only be used in this form if you are   *
000203* defining panels dynamically.  Otherwise you should use the *
000204* parameter layout generated by the panel editor.  GET-INPUT *
000205* actually only uses the data up to the end of IP-CHAR-DATA. *
000206* FIELD-LEN, etc. are set to zero because it is usually      *
000207* easier to define data areas for panels separately and use  *
000208* SET-PANEL-FIELDS, etc. to identify them to SP2.  This      *
000209* allows you to use the same CONVERSE-DATA area for multiple *
000210* panels yet have separate data areas for the panels (these  *
000211* data areas must be kept intact while the corresponding     *
000212* windows are open)                                          *
000213**************************************************************
000214
000215 01  SP2-CONVERSE-DATA.
000216     05  SP2-CD-RET-CODE         PIC S9(4) COMP-5.
000217     05  SP2-CD-LENS.
000218         10  SP2-CD-LEN-LEN      PIC S9(4) COMP-5 VALUE +20.
000219         10  SP2-CD-IP-NUM-LEN   PIC S9(4) COMP-5 VALUE +40.
000220         10  SP2-CD-IP-CHAR-LEN  PIC S9(4) COMP-5 VALUE +106.
000221         10  SP2-CD-OP-NUM-LEN   PIC S9(4) COMP-5 VALUE +6.
000222         10  SP2-CD-OP-CHAR-LEN  PIC S9(4) COMP-5 VALUE +2.
000223         10  SP2-CD-FIELD-LEN    PIC S9(4) COMP-5 VALUE +0.
000224         10  SP2-CD-COLR-LEN     PIC S9(4) COMP-5 VALUE +0.
000225         10  SP2-CD-TYPE-LEN     PIC S9(4) COMP-5 VALUE +0.
000226         10  FILLER              PIC S9(4) COMP-5 VALUE +0.
000227         10  FILLER              PIC S9(4) COMP-5 VALUE +0.
000228     05  SP2-CD-DATA.
000229******** SP2-CD-IP-NUM-DATA *****
000230         10  SP2-CD-KEY          PIC S9(4) COMP-5.
000231         10  SP2-CD-NEXT-FLD-ID  PIC S9(4) COMP-5.
000232         10  SP2-CD-NEXT-FLD-NUM PIC S9(4) COMP-5.
000233         10  SP2-CD-NEXT-TAB-NUM PIC S9(4) COMP-5.
000234         10  SP2-CD-NEXT-OCCURS  PIC S9(4) COMP-5.
000235         10  SP2-CD-LAST-FLD-ID  PIC S9(4) COMP-5.
000236         10  SP2-CD-LAST-FLD-NUM PIC S9(4) COMP-5.
000237         10  SP2-CD-LAST-TAB-NUM PIC S9(4) COMP-5.
000238         10  SP2-CD-LAST-OCCURS  PIC S9(4) COMP-5.
000239         10  SP2-CD-MENU-ID      PIC S9(4) COMP-5.
000240         10  SP2-CD-CTRL-FIELD-KEY REDEFINES SP2-CD-MENU-ID
000241                                 PIC S9(4) COMP-5.
000242         10  SP2-CD-BUTTON-ID REDEFINES SP2-CD-MENU-ID
000243                                 PIC S9(4) COMP-5.
000244         10  SP2-CD-ROW-COL-SW   PIC S9(4) COMP-5.
000245         10  SP2-CD-CURSOR-ROW   PIC S9(4) COMP-5.
000246         10  SP2-CD-CURSOR-COL   PIC S9(4) COMP-5.
000247         10  SP2-CD-LAST-ROW     PIC S9(4) COMP-5.
000248         10  SP2-CD-LAST-COL     PIC S9(4) COMP-5.
000249         10  SP2-CD-DISP-SW      PIC S9(4) COMP-5.
000250         10  SP2-CD-NEXT-VERT    PIC S9(4) COMP-5.
000251         10  SP2-CD-LAST-VERT    PIC S9(4) COMP-5.
000252         10  SP2-CD-NEXT-HOR     PIC S9(4) COMP-5.
000253         10  SP2-CD-LAST-HOR     PIC S9(4) COMP-5.
000254******** SP2-CD-IP-CHAR-DATA ****
000255         10  SP2-CD-NEXT-PANEL   PIC X(8).
000256         10  SP2-CD-NEXT-FIELD   PIC X(30).
000257         10  SP2-CD-LAST-FIELD   PIC X(30).
000258         10  SP2-CD-MENU-OPTION  PIC X(30).
000259         10  SP2-CD-SWITCH-SW    PIC X.
000260         10  SP2-CD-SIZE-SW      PIC X.
000261         10  SP2-CD-MOUSE-SW     PIC X.
000262         10  SP2-CD-CAPTURE-SW   PIC X.
000263         10  SP2-CD-WAIT-SW      PIC X.
000264         10  SP2-CD-CURS-SW      PIC X.
000265         10  SP2-CD-CHG-SW       PIC X.
000266         10  SP2-CD-TIMEOUT      PIC X.
000267******** SP2-CD-OP-NUM-DATA *****
000268         10  SP2-CD-PAN-POS-SW   PIC S9(4) COMP-5.
000269         10  SP2-CD-PAN-ROW      PIC S9(4) COMP-5.
000270         10  SP2-CD-PAN-COL      PIC S9(4) COMP-5.
000271******** SP2-CD-OP-CHAR-DATA ****
000272         10  SP2-CD-NEW-WINDOW   PIC X.
000273         10  SP2-CD-DISPLAY-SW   PIC X.
000274******** SP2-CD-FIELD-DATA ******
000275******** SP2-CD-COLR-DATA *******
000276******** SP2-CD-TYPE-DATA *******
000277
000278*********************************
000279* null parameter                *
000280* parameter for CLOSE-WINDOW    *
000281*               CLOSE-FILE      *
000282*               END-SESSION     *
000283* also used with DISPLAY-WINDOW *
000284*                CLEAR-WINDOW   *
000285*                WRITE-PANEL    *
000286*                WRITE-MENU     *
000287*                WRITE-FONTS    *
000288*                WRITE-COLORS   *
000289*                CLEAR-MENU     *
000290*                SET-MOUSE-SHAPE*
000291*********************************
000292
000293 01  SP2-NULL-PARM.
000294     05  SP2-NP-RET-CODE         PIC S9(4) COMP-5.
000295
000296*********************************
000297* window definition             *
000298* parameter for OPEN-WINDOW     *
000299* also used with GET-WINDOW-DEF *
000300*                SET-WINDOW-DEF *
000301*********************************
000302
000303 01  SP2-WINDOW-DEF.
000304     05  SP2-WD-RET-CODE         PIC S9(4) COMP-5.
000305     05  SP2-WD-LENS.
000306         10  SP2-WD-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
000307         10  SP2-WD-NUM-LEN      PIC S9(4) COMP-5 VALUE +38.
000308         10  SP2-WD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +38.
000309         10  SP2-WD-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
000310         10  SP2-WD-TITLE-LEN    PIC S9(4) COMP-5 VALUE +80.
000311     05  SP2-WD-DATA.
000312******** SP2-WD-NUM-DATA ********
000313         10  SP2-WD-WINDOW-ID    PIC S9(4) COMP-5.
000314         10  SP2-WD-OWNR-ID      PIC S9(4) COMP-5.
000315         10  SP2-WD-GUI-ID       PIC S9(4) COMP-5.
000316         10  SP2-WD-GUI-ID2      PIC S9(4) COMP-5.
000317         10  SP2-WD-WIDTH        PIC S9(4) COMP-5.
000318         10  SP2-WD-HEIGHT       PIC S9(4) COMP-5.
000319         10  SP2-WD-ROW          PIC S9(4) COMP-5.
000320         10  SP2-WD-COL          PIC S9(4) COMP-5.
000321         10  SP2-WD-TOT-WIDTH    PIC S9(4) COMP-5.
000322         10  SP2-WD-TOT-HEIGHT   PIC S9(4) COMP-5.
000323         10  SP2-WD-HOR-DISP     PIC S9(4) COMP-5.
000324         10  SP2-WD-VERT-DISP    PIC S9(4) COMP-5.
000325         10  SP2-WD-TITLE-ROWS   PIC S9(4) COMP-5.
000326         10  SP2-WD-MENU-ROWS    PIC S9(4) COMP-5.
000327         10  SP2-WD-MENU-ID      PIC S9(4) COMP-5.
000328         10  SP2-WD-MENU-ID2     PIC S9(4) COMP-5.
000329         10  SP2-WD-CELL-WIDTH   PIC S9(4) COMP-5.
000330         10  SP2-WD-CELL-HEIGHT  PIC S9(4) COMP-5.
000331         10  SP2-WD-TOOLBAR-ROWS PIC S9(4) COMP-5.
000332******** SP2-WD-CHAR-DATA *******
000333         10  SP2-WD-NAME         PIC X(8).
000334         10  SP2-WD-PANEL-NAME   PIC X(8).
000335         10  SP2-WD-MENU-NAME    PIC X(8).
000336         10  SP2-WD-COLR         PIC X.
000337         10  SP2-WD-BOR-TYPE     PIC X.
000338         10  SP2-WD-INIT-SW      PIC X.
000339         10  SP2-WD-PAINT-SW     PIC X.
000340         10  SP2-WD-OPTS-SW      PIC X.
000341         10  SP2-WD-HIDE-SW      PIC X.
000342         10  SP2-WD-SBAR-SW      PIC X.
000343         10  SP2-WD-NO-TABS-SW   PIC X.
000344         10  SP2-WD-MORE-OPTIONS PIC X.
000345         10  SP2-WD-CELL-SIZE    PIC X.
000346         10  SP2-WD-DIV-WIDTH    PIC X.
000347         10  SP2-WD-DIV-HEIGHT   PIC X.
000348         10  SP2-WD-OPTIONS-3    PIC X.
000349         10  FILLER              PIC X.
000350******** SP2-WD-VAR-DATA *******
000351         10  SP2-WD-TITLE        PIC X(80).
000352
000353**********************************
000354* name definition                *
000355* parameter for READ-PANEL       *
000356*               ACTIVATE-WINDOW  *
000357*               ACTIVATE-PANEL   *
000358*               READ-MENU        *
000359* also used with COPY-PANEL      *
000360*                READ-NEXT-PANEL *
000361**********************************
000362
000363 01  SP2-NAME-DEF.
000364     05  SP2-ND-RET-CODE         PIC S9(4) COMP-5.
000365     05  SP2-ND-NAME             PIC X(8).
000366
000367************************************
000368* menu option definition           *
000369* parameter for GET-MENU-OPTION    *
000370*               SET-MENU-OPTION    *
000371*               INSERT-MENU-OPTION *
000372*               DELETE-MENU-OPTION *
000373************************************
000374
000375 01  SP2-MENU-OPTION.
000376     05  SP2-MO-RET-CODE         PIC S9(4) COMP-5.
000377     05  SP2-MO-LENS.
000378         10  SP2-MO-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
000379         10  SP2-MO-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
000380         10  SP2-MO-CHAR-LEN     PIC S9(4) COMP-5 VALUE +2.
000381         10  SP2-MO-VAR-LEN      PIC S9(4) COMP-5 VALUE +60.
000382         10  SP2-MO-NAME-LEN     PIC S9(4) COMP-5 VALUE +30.
000383         10  SP2-MO-TEXT-LEN     PIC S9(4) COMP-5 VALUE +30.
000384     05  SP2-MO-DATA.
000385******** SP2-MO-NUM-DATA ********
000386         10  SP2-MO-ID           PIC S9(4) COMP-5.
000387         10  SP2-MO-OWNR-ID      PIC S9(4) COMP-5.
000388         10  SP2-MO-ACC-KEY      PIC S9(4) COMP-5.
000389******** SP2-MO-CHAR-DATA *******
000390         10  SP2-MO-TYPE         PIC X.
000391         10  SP2-MO-STATE        PIC X.
000392******** SP2-MO-VAR-DATA *******
000393         10  SP2-MO-NAME         PIC X(30).
000394         10  SP2-MO-TEXT         PIC X(30).
000395
000396*********************************
000397* message definition            *
000398* parameter for DISPLAY-MESSAGE *
000399*********************************
000400
000401 01  SP2-MESSAGE-DATA.
000402     05  SP2-MS-RET-CODE         PIC S9(4) COMP-5.
000403     05  SP2-MS-LENS.
000404         10  SP2-MS-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
000405         10  SP2-MS-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
000406         10  SP2-MS-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
000407         10  SP2-MS-VAR-LEN      PIC S9(4) COMP-5 VALUE +160.
000408         10  SP2-MS-TITLE-LEN    PIC S9(4) COMP-5 VALUE +80.
000409         10  SP2-MS-LINE-LEN     PIC S9(4) COMP-5 VALUE +80.
000410     05  SP2-MS-DATA.
000411******** SP2-MS-NUM-DATA ********
000412         10  SP2-MS-LINE-CNT     PIC S9(4) COMP-5.
000413******** SP2-MS-CHAR-DATA *******
000414         10  SP2-MS-ICON         PIC X.
000415         10  SP2-MS-BUTTON       PIC X.
000416         10  SP2-MS-CANCEL       PIC X.
000417         10  SP2-MS-REPLY        PIC X.
000418******** SP2-MS-VAR-DATA *******
000419         10  SP2-MS-TITLE        PIC X(80).
000420         10  SP2-MS-TEXT         PIC X(80).
000421
000422*******************************
000423* clipboard data definition   *
000424* parameter for SET-CLIPBOARD *
000425*               GET-CLIPBOARD *
000426*******************************
000427
000428 01  SP2-CLIPBOARD-DATA.
000429     05  SP2-CB-RET-CODE         PIC S9(4) COMP-5.
000430     05  SP2-CB-LENS.
000431         10  SP2-CB-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
000432         10  SP2-CB-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
000433         10  SP2-CB-CHAR-LEN     PIC S9(4) COMP-5 VALUE +0.
000434         10  SP2-CB-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
000435         10  SP2-CB-TEXT-LEN     PIC S9(4) COMP-5 VALUE +80.
000436     05  SP2-CB-DATA.
000437******** SP2-CB-NUM-DATA ********
000438         10  FILLER              PIC S9(4) COMP-5.
000439******** SP2-CB-VAR-DATA *******
000440         10  SP2-CB-TEXT         PIC X(80).
000441
000442********************************
000443* repeat extension data        *
000444* parameter for SET-REPEAT-EXT *
000445********************************
000446
000447 01  SP2-REPEAT-EXT.
000448     05  SP2-RX-RET-CODE         PIC S9(4) COMP-5.
000449     05  SP2-RX-LENS.
000450         10  SP2-RX-LEN-LEN      PIC S9(4) COMP-5 VALUE +18.
000451         10  SP2-RX-NUM-LEN      PIC S9(4) COMP-5 VALUE +20.
000452         10  SP2-RX-CHAR-LEN     PIC S9(4) COMP-5 VALUE +0.
000453         10  SP2-RX-PTR-LEN      PIC S9(4) COMP-5 VALUE +12.
000454         10  SP2-RX-FIELD-LEN    PIC S9(4) COMP-5 VALUE +0.
000455         10  SP2-RX-COLR-LEN     PIC S9(4) COMP-5 VALUE +0.
000456         10  SP2-RX-TYPE-LEN     PIC S9(4) COMP-5 VALUE +0.
000457         10  SP2-RX-LONG-LEN     PIC S9(4) COMP-5 VALUE +16.
000458         10  FILLER              PIC S9(4) COMP-5.
000459     05  SP2-RX-DATA.
000460******** SP2-RX-NUM-DATA ********
000461         10  SP2-RX-ID           PIC S9(4) COMP-5.
000462         10  SP2-RX-NEXT-OCC     PIC S9(4) COMP-5.
000463         10  SP2-RX-DISP-SW      PIC S9(4) COMP-5.
000464         10  SP2-RX-NEW-DISP     PIC S9(4) COMP-5.
000465         10  SP2-RX-BLOCK-SW     PIC S9(4) COMP-5.
000466         10  SP2-RX-BLOCK-DISP   PIC S9(4) COMP-5.
000467         10  SP2-RX-BLOCK-OCCS   PIC S9(4) COMP-5.
000468         10  SP2-RX-TOTAL-SW     PIC S9(4) COMP-5.
000469         10  SP2-RX-TOTAL-OCCS   PIC S9(4) COMP-5.
000470         10  FILLER              PIC S9(4) COMP-5.
000471******** SP2-RX-PTR-DATA ********
000472         10  SP2-RX-FIELD-PTR    POINTER.
000473         10  SP2-RX-COLR-PTR     POINTER.
000474         10  SP2-RX-TYPE-PTR     POINTER.
000475******** SP2-RX-FIELD-DATA ******
000476******** SP2-RX-COLR-DATA *******
000477******** SP2-RX-TYPE-DATA *******
000478******** SP2-RX-LONG-DATA *******
000479         10  SP2-RX-NEXT-OCC-L   PIC S9(8) COMP-5.
000480         10  SP2-RX-NEW-DISP-L   PIC S9(8) COMP-5.
000481         10  SP2-RX-BLOCK-DISP-L PIC S9(8) COMP-5.
000482         10  SP2-RX-TOTAL-OCCS-L PIC S9(8) COMP-5.
000483
000484********************************
000485* vbx property data            *
000486* parameter for SET-VBX        *
000487********************************
000488
000489 01  SP2-VBX-PARM.
000490     05  SP2-VB-RET-CODE         PIC S9(4) COMP-5.
000491     05  SP2-VB-LENS.
000492         10  SP2-VB-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
000493         10  SP2-VB-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
000494         10  SP2-VB-CHAR-LEN     PIC S9(4) COMP-5 VALUE +30.
000495         10  SP2-VB-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
000496     05  SP2-VB-DATA.
000497******** SP2-VB-NUM-DATA ********
000498         10  SP2-VB-ID           PIC S9(4) COMP-5.
000499         10  SP2-VB-PROP         PIC S9(4) COMP-5.
000500         10  SP2-VB-INDEX        PIC S9(4) COMP-5.
000501******** SP2-VB-CHAR-DATA *******
000502         10  SP2-VB-PROP-NAME    PIC X(30).
000503******** SP2-VB-VAR-DATA ********
000504         10  SP2-VB-VALUE        PIC X(80).
000505
000506********************************
000507* panel definition             *
000508* parameter for GET-PANEL-DEF  *
000509* also used with SET-PANEL-DEF *
000510********************************
000511
000512 01  SP2-PANEL-DEF.
000513     05  SP2-PD-RET-CODE         PIC S9(4) COMP-5.
000514     05  SP2-PD-LENS.
000515         10  SP2-PD-LEN-LEN      PIC S9(4) COMP-5 VALUE +24.
000516         10  SP2-PD-NUM-LEN      PIC S9(4) COMP-5 VALUE +40.
000517         10  SP2-PD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +76.
000518         10  SP2-PD-VAR-LEN      PIC S9(4) COMP-5 VALUE +291.
000519         10  SP2-PD-DESC-LEN     PIC S9(4) COMP-5 VALUE +40.
000520         10  SP2-PD-TITLE-LEN    PIC S9(4) COMP-5 VALUE +40.
000521         10  SP2-PD-CURS-KEY-LEN PIC S9(4) COMP-5 VALUE +50.
000522         10  SP2-PD-CTRL-KEY-LEN PIC S9(4) COMP-5 VALUE +40.
000523         10  SP2-PD-SYNS-LEN     PIC S9(4) COMP-5 VALUE +0.
000524         10  SP2-PD-MSG-TEXT-LEN PIC S9(4) COMP-5 VALUE +80.
000525         10  SP2-PD-USER-LEN     PIC S9(4) COMP-5 VALUE +1.
000526         10  SP2-PD-HELP-LEN     PIC S9(4) COMP-5 VALUE +40.
000527     05  SP2-PD-DATA.
000528******** SP2-PD-NUM-DATA ********
000529         10  SP2-PD-WIDTH        PIC S9(4) COMP-5.
000530         10  SP2-PD-HEIGHT       PIC S9(4) COMP-5.
000531         10  SP2-PD-ROW          PIC S9(4) COMP-5.
000532         10  SP2-PD-COL          PIC S9(4) COMP-5.
000533         10  SP2-PD-FLD-CNT      PIC S9(4) COMP-5.
000534         10  SP2-PD-PROG-CNT     PIC S9(4) COMP-5.
000535         10  SP2-PD-PROG-LEN     PIC S9(4) COMP-5.
000536         10  SP2-PD-HELP-KEY     PIC S9(4) COMP-5.
000537         10  SP2-PD-EDIT-OV-KEY  PIC S9(4) COMP-5.
000538         10  SP2-PD-MSG-REFRESH  PIC S9(4) COMP-5.
000539         10  SP2-PD-TITLE-ROWS   PIC S9(4) COMP-5.
000540         10  SP2-PD-DEF-PB       PIC S9(4) COMP-5.
000541         10  FILLER              PIC S9(4) COMP-5.
000542         10  SP2-PD-MENU-ROWS    PIC S9(4) COMP-5.
000543         10  SP2-PD-TOT-WIDTH    PIC S9(4) COMP-5.
000544         10  SP2-PD-TOT-HEIGHT   PIC S9(4) COMP-5.
000545         10  SP2-PD-MSG-LEN      PIC S9(4) COMP-5.
000546         10  SP2-PD-CELL-WIDTH   PIC S9(4) COMP-5.
000547         10  SP2-PD-CELL-HEIGHT  PIC S9(4) COMP-5.
000548         10  SP2-PD-FONT-ID      PIC S9(4) COMP-5.
000549******** SP2-PD-CHAR-DATA *******
000550         10  SP2-PD-NAME         PIC X(8).
000551         10  SP2-PD-MENU-NAME    PIC X(8).
000552         10  FILLER              PIC X.
000553         10  SP2-PD-CUR-FLD-COLR PIC X.
000554         10  SP2-PD-CURS-SKIP    PIC X.
000555         10  SP2-PD-CURS-SHOW    PIC X.
000556         10  SP2-PD-CURS-IN-FLD  PIC X.
000557         10  SP2-PD-SHIFT-NUMS   PIC X.
000558         10  SP2-PD-BLANK-NUMS   PIC X.
000559         10  SP2-PD-ASSUME-DEC   PIC X.
000560         10  SP2-PD-FORMAT-NUMS  PIC X.
000561         10  SP2-PD-CURS-WRAP    PIC X.
000562         10  SP2-PD-INIT-NUMS    PIC X.
000563         10  SP2-PD-OVERRIDE-REQ PIC X.
000564         10  SP2-PD-CELL-SIZE    PIC X.
000565         10  FILLER              PIC X.
000566         10  SP2-PD-MISC-OPTIONS PIC X.
000567         10  SP2-PD-DIV-WIDTH    PIC X.
000568         10  SP2-PD-DIV-HEIGHT   PIC X.
000569         10  FILLER              PIC X.
000570         10  FILLER              PIC X.
000571         10  FILLER              PIC X.
000572         10  FILLER              PIC X.
000573         10  FILLER              PIC X.
000574         10  SP2-PD-COLR         PIC X.
000575         10  SP2-PD-PROG-DATE    PIC X.
000576         10  SP2-PD-HELP         PIC X(8).
000577         10  SP2-PD-TEXT-OPTIONS PIC X.
000578         10  FILLER              PIC X(8).
000579         10  SP2-PD-TOOLBAR-NAME PIC X(8).
000580         10  SP2-PD-MSG-COLR     PIC X.
000581         10  SP2-PD-MORE-OPTIONS PIC X.
000582         10  SP2-PD-OPTIONS-3    PIC X.
000583         10  FILLER              PIC X.
000584         10  FILLER              PIC X.
000585         10  FILLER              PIC X.
000586         10  FILLER              PIC X.
000587         10  FILLER              PIC X.
000588         10  FILLER              PIC X.
000589         10  SP2-PD-WIN-BOR-TYPE PIC X.
000590         10  SP2-PD-INITIAL-SW   PIC X.
000591******** SP2-PD-VAR-DATA ********
000592         10  SP2-PD-DESCRIPTION  PIC X(40).
000593         10  SP2-PD-TITLE        PIC X(40).
000594         10  SP2-PD-CURS-KEYS.
000595             15  SP2-PD-LEFT     PIC S9(4) COMP-5.
000596             15  SP2-PD-RIGHT    PIC S9(4) COMP-5.
000597             15  SP2-PD-UP       PIC S9(4) COMP-5.
000598             15  SP2-PD-DOWN     PIC S9(4) COMP-5.
000599             15  SP2-PD-TAB      PIC S9(4) COMP-5.
000600             15  SP2-PD-BACKTAB  PIC S9(4) COMP-5.
000601             15  SP2-PD-TB-ERASE PIC S9(4) COMP-5.
000602             15  SP2-PD-BT-ERASE PIC S9(4) COMP-5.
000603             15  SP2-PD-DELETE   PIC S9(4) COMP-5.
000604             15  SP2-PD-BACKSPAC PIC S9(4) COMP-5.
000605             15  SP2-PD-ERASE    PIC S9(4) COMP-5.
000606             15  SP2-PD-INSERT   PIC S9(4) COMP-5.
000607             15  SP2-PD-HOME     PIC S9(4) COMP-5.
000608             15  SP2-PD-END      PIC S9(4) COMP-5.
000609             15  SP2-PD-SCRL-UP  PIC S9(4) COMP-5.
000610             15  SP2-PD-SCRL-DN  PIC S9(4) COMP-5.
000611             15  SP2-PD-SCRL-LT  PIC S9(4) COMP-5.
000612             15  SP2-PD-SCRL-RT  PIC S9(4) COMP-5.
000613             15  FILLER          PIC S9(4) COMP-5.
000614             15  FILLER          PIC S9(4) COMP-5.
000615             15  FILLER          PIC S9(4) COMP-5.
000616             15  FILLER          PIC S9(4) COMP-5.
000617             15  FILLER          PIC S9(4) COMP-5.
000618             15  SP2-PD-HOME-PAN PIC S9(4) COMP-5.
000619             15  SP2-PD-END-PAN  PIC S9(4) COMP-5.
000620         10  SP2-PD-CTRL-KEYS.
000621             15  SP2-PD-CTRL-KEY OCCURS 20
000622                                 PIC S9(4) COMP-5.
000623         10  SP2-PD-MSG-TEXT     PIC X(80).
000624         10  SP2-PD-USER-DATA    PIC X.
000625         10  SP2-PD-HELP-KEYWORD PIC X(40).
000626
000627*********************************
000628* static definition             *
000629* parameter for GET-STATIC-DEF  *
000630* also used with SET-STATIC-DEF *
000631*********************************
000632
000633 01  SP2-STATIC-DEF.
000634     05  SP2-SD-RET-CODE         PIC S9(4) COMP-5.
000635     05  SP2-SD-LENS.
000636         10  SP2-SD-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
000637         10  SP2-SD-NUM-LEN      PIC S9(4) COMP-5 VALUE +12.
000638         10  SP2-SD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
000639         10  SP2-SD-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
000640         10  SP2-SD-TEXT-LEN     PIC S9(4) COMP-5 VALUE +80.
000641     05  SP2-SD-DATA.
000642******** SP2-SD-NUM-DATA ********
000643         10  SP2-SD-ID           PIC S9(4) COMP-5.
000644         10  SP2-SD-ROW          PIC S9(4) COMP-5.
000645         10  SP2-SD-COL          PIC S9(4) COMP-5.
000646         10  SP2-SD-WIDTH        PIC S9(4) COMP-5.
000647         10  SP2-SD-HEIGHT       PIC S9(4) COMP-5.
000648         10  SP2-SD-FONT-ID      PIC S9(4) COMP-5.
000649******** SP2-SD-CHAR-DATA *******
000650         10  SP2-SD-COLR         PIC X.
000651         10  SP2-SD-TYPE         PIC X.
000652         10  SP2-SD-JUSTIFY      PIC X.
000653         10  SP2-SD-MISC-OPTIONS PIC X.
000654******** SP2-SD-VAR-DATA ********
000655         10  SP2-SD-TEXT         PIC X(80).
000656
000657********************************
000658* field definition             *
000659* parameter for GET-FIELD-DEF  *
000660* also used with SET-FIELD-DEF *
000661*                DELETE-FIELD  *
000662********************************
000663
000664**************************************************************
000665* the variable portion of this parameter is set up slightly  *
000666* differently to the other information parameters.  This     *
000667* variable data is defined as one contiguous block of length *
000668* 2000.  This is because the data can vary so much depending *
000669* on the field type.  Individual data items within the block *
000670* should be extracted based on the corresponding length      *
000671* values.  SP2 knows to reset these individual lengths to    *
000672* their proper values if they are all originally passed as   *
000673* zero.  All the information parameters can be handled this  *
000674* way if you prefer.  If you are using this parameter for    *
000675* maintenance you should set the length values yourself so   *
000676* so that they correspond to the actual variable data that   *
000677* you are passing.                                           *
000678**************************************************************
000679
000680 01  SP2-FIELD-DEF.
000681     05  SP2-FD-RET-CODE         PIC S9(4) COMP-5.
000682     05  SP2-FD-LENS.
000683         10  SP2-FD-LEN-LEN      PIC S9(4) COMP-5 VALUE +26.
000684         10  SP2-FD-NUM-LEN      PIC S9(4) COMP-5 VALUE +44.
000685         10  SP2-FD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +74.
000686         10  SP2-FD-VAR-LEN      PIC S9(4) COMP-5 VALUE +2000.
000687         10  SP2-FD-VAR-LENS.
000688             15  SP2-FD-FORMAT-LEN
000689                                 PIC S9(4) COMP-5 VALUE +0.
000690             15  SP2-FD-CAPTION-LEN
000691                                 PIC S9(4) COMP-5 VALUE +0.
000692             15  SP2-FD-INITIAL-LEN
000693                                 PIC S9(4) COMP-5 VALUE +0.
000694             15  FILLER          PIC S9(4) COMP-5 VALUE +0.
000695             15  SP2-FD-RANGE-LEN
000696                                 PIC S9(4) COMP-5 VALUE +0.
000697             15  SP2-FD-DISCRETE-LEN
000698                                 PIC S9(4) COMP-5 VALUE +0.
000699             15  SP2-FD-MSG-TEXT-LEN
000700                                 PIC S9(4) COMP-5 VALUE +0.
000701             15  SP2-FD-USER-LEN PIC S9(4) COMP-5 VALUE +0.
000702             15  SP2-FD-HELP-LEN PIC S9(4) COMP-5 VALUE +0.
000703     05  SP2-FD-DATA.
000704******** SP2-FD-NUM-DATA ********
000705         10  SP2-FD-ID           PIC S9(4) COMP-5.
000706         10  SP2-FD-GUI-ID       PIC S9(4) COMP-5.
000707         10  SP2-FD-GUI-ID-2     PIC S9(4) COMP-5.
000708         10  SP2-FD-OCCURRENCE   PIC S9(4) COMP-5.
000709         10  SP2-FD-BASE-ID      PIC S9(4) COMP-5.
000710         10  SP2-FD-ROW          PIC S9(4) COMP-5.
000711         10  SP2-FD-COL          PIC S9(4) COMP-5.
000712         10  SP2-FD-PROG-OFF     PIC S9(4) COMP-5.
000713         10  SP2-FD-FLD-NUM      PIC S9(4) COMP-5.
000714         10  SP2-FD-TAB-NUM      PIC S9(4) COMP-5.
000715         10  SP2-FD-PROG-NUM     PIC S9(4) COMP-5.
000716         10  SP2-FD-WIDTH        PIC S9(4) COMP-5.
000717         10  SP2-FD-HEIGHT       PIC S9(4) COMP-5.
000718         10  SP2-FD-MAX-LEN      PIC S9(4) COMP-5.
000719         10  SP2-FD-PROG-LEN     PIC S9(4) COMP-5.
000720         10  SP2-FD-ITEM-LEN     PIC S9(4) COMP-5.
000721         10  FILLER              PIC S9(4) COMP-5.
000722         10  SP2-FD-HELP-KEY     PIC S9(4) COMP-5.
000723         10  FILLER              PIC S9(4) COMP-5.
000724         10  SP2-FD-GROUP-ID     PIC S9(4) COMP-5.
000725         10  SP2-FD-REPEAT-ID    PIC S9(4) COMP-5.
000726         10  SP2-FD-FONT-ID      PIC S9(4) COMP-5.
000727******** SP2-FD-CHAR-DATA *******
000728         10  SP2-FD-NAME         PIC X(30).
000729         10  SP2-FD-TYPE         PIC X.
000730         10  SP2-FD-OUTPUT       PIC X.
000731         10  SP2-FD-PROG-DEC     PIC X.
000732         10  FILLER              PIC X(4).
000733         10  SP2-FD-INIT-NUMS    PIC X.
000734         10  SP2-FD-MISC-OPTIONS PIC X.
000735         10  FILLER              PIC X.
000736         10  SP2-FD-HELP         PIC X(8).
000737         10  FILLER              PIC X(8).
000738         10  SP2-FD-REQUIRED     PIC X.
000739         10  SP2-FD-PROG-CTRL    PIC X.
000740         10  SP2-FD-JUSTIFY      PIC X.
000741         10  SP2-FD-FILL         PIC X.
000742         10  SP2-FD-ASSUME-DEC   PIC X.
000743         10  SP2-FD-SPEC-FMT     PIC X.
000744         10  SP2-FD-CASE         PIC X.
000745         10  SP2-FD-IMBED-BLANKS PIC X.
000746         10  SP2-FD-CUR-COLR     PIC X.
000747         10  SP2-FD-CURS-SKIP    PIC X.
000748         10  SP2-FD-CURS-SHOW    PIC X.
000749         10  SP2-FD-BLANK-FIRST  PIC X.
000750         10  SP2-FD-BLANK-ZERO   PIC X.
000751         10  SP2-FD-CTRL-TYPE    PIC X.
000752         10  SP2-FD-COLR         PIC X.
000753         10  SP2-FD-MNEMONIC     PIC X.
000754         10  SP2-FD-BOR-TYPE     PIC X.
000755         10  SP2-FD-PROG-SPEC    PIC X.
000756******** SP2-FD-VAR-DATA ********
000757         10  SP2-FD-VAR-DATA     PIC X(2000).
000758******** SP2-FD-FMT *************
000759******** SP2-FD-CAPTION *********
000760******** SP2-FD-INITIAL-VAL *****
000761******** SP2-FD-RANGE-VALS ******
000762******** SP2-FD-DISC-VALS *******
000763******** SP2-FD-MSG-TEXT ********
000764******** SP2-FD-USER-DATA *******
000765******** SP2-FD-HELP-KEYWORD ****
000766
000767********************************
000768* group definition             *
000769* parameter for GET-GROUP-DEF  *
000770* also used with SET-GROUP-DEF *
000771*                DELETE-GROUP  *
000772********************************
000773
000774 01  SP2-GROUP-DEF.
000775     05  SP2-GD-RET-CODE         PIC S9(4) COMP-5.
000776     05  SP2-GD-LENS.
000777         10  SP2-GD-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
000778         10  SP2-GD-NUM-LEN      PIC S9(4) COMP-5 VALUE +12.
000779         10  SP2-GD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +48.
000780         10  SP2-GD-VAR-LEN      PIC S9(4) COMP-5 VALUE +20.
000781     05  SP2-GD-DATA.
000782******** SP2-GD-NUM-DATA ********
000783         10  SP2-GD-ID           PIC S9(4) COMP-5.
000784         10  SP2-GD-ROW          PIC S9(4) COMP-5.
000785         10  SP2-GD-COL          PIC S9(4) COMP-5.
000786         10  SP2-GD-WIDTH        PIC S9(4) COMP-5.
000787         10  SP2-GD-HEIGHT       PIC S9(4) COMP-5.
000788         10  SP2-GD-FLD-CNT      PIC S9(4) COMP-5.
000789******** SP2-GD-CHAR-DATA *******
000790         10  SP2-GD-NAME         PIC X(30).
000791         10  SP2-GD-TAB-WITHIN   PIC X.
000792         10  SP2-GD-CUR-COLR     PIC X.
000793         10  FILLER              PIC X.
000794         10  SP2-GD-SELECT-TYPE  PIC X.
000795         10  FILLER              PIC X.
000796         10  FILLER              PIC X.
000797         10  FILLER              PIC X(3).
000798         10  FILLER              PIC X(3).
000799         10  FILLER              PIC X.
000800         10  FILLER              PIC X.
000801         10  SP2-GD-COLR         PIC X.
000802         10  FILLER              PIC X.
000803         10  SP2-GD-BOR-TYPE     PIC X.
000804         10  FILLER              PIC X.
000805******** SP2-GD-VAR-DATA ********
000806         10  SP2-GD-FLD-ID OCCURS 10
000807                                 PIC S9(4) COMP-5.
000808
000809*********************************
000810* repeat definition             *
000811* parameter for GET-REPEAT-DEF  *
000812* also used with SET-REPEAT-DEF *
000813*                DELETE-REREAT  *
000814*********************************
000815
000816 01  SP2-REPEAT-DEF.
000817     05  SP2-RD-RET-CODE         PIC S9(4) COMP-5.
000818     05  SP2-RD-LENS.
000819         10  SP2-RD-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
000820         10  SP2-RD-NUM-LEN      PIC S9(4) COMP-5 VALUE +44.
000821         10  SP2-RD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
000822         10  SP2-RD-VAR-LEN      PIC S9(4) COMP-5 VALUE +104.
000823         10  SP2-RD-BASE-LEN     PIC S9(4) COMP-5 VALUE +20.
000824     05  SP2-RD-DATA.
000825******** SP2-RD-NUM-DATA ********
000826         10  SP2-RD-ID           PIC S9(4) COMP-5.
000827         10  SP2-RD-ROW          PIC S9(4) COMP-5.
000828         10  SP2-RD-COL          PIC S9(4) COMP-5.
000829         10  SP2-RD-WIDTH        PIC S9(4) COMP-5.
000830         10  SP2-RD-HEIGHT       PIC S9(4) COMP-5.
000831         10  SP2-RD-VERT-OCC     PIC S9(4) COMP-5.
000832         10  SP2-RD-VERT-VIS     PIC S9(4) COMP-5.
000833         10  SP2-RD-VERT-GAP     PIC S9(4) COMP-5.
000834         10  SP2-RD-VERT-DISP    PIC S9(4) COMP-5.
000835         10  SP2-RD-VERT-SHIFT   PIC S9(4) COMP-5.
000836         10  SP2-RD-VERT-BAR-ID  PIC S9(4) COMP-5.
000837         10  SP2-RD-HOR-OCC      PIC S9(4) COMP-5.
000838         10  SP2-RD-HOR-VIS      PIC S9(4) COMP-5.
000839         10  SP2-RD-HOR-GAP      PIC S9(4) COMP-5.
000840         10  SP2-RD-HOR-DISP     PIC S9(4) COMP-5.
000841         10  SP2-RD-HOR-SHIFT    PIC S9(4) COMP-5.
000842         10  SP2-RD-HOR-BAR-ID   PIC S9(4) COMP-5.
000843         10  SP2-RD-LAST-OCCURS  PIC S9(4) COMP-5.
000844         10  SP2-RD-BEG-LINE-NUM PIC S9(4) COMP-5.
000845         10  SP2-RD-PROG-NUM     PIC S9(4) COMP-5.
000846         10  SP2-RD-PROG-LEN     PIC S9(4) COMP-5.
000847         10  SP2-RD-FLD-CNT      PIC S9(4) COMP-5.
000848******** SP2-RD-CHAR-DATA *******
000849         10  SP2-RD-BOR-TYPE     PIC X.
000850         10  SP2-RD-TAB-SW       PIC X.
000851         10  SP2-RD-MISC-OPTIONS PIC X.
000852         10  FILLER              PIC X.
000853******** SP2-RD-VAR-DATA ********
000854         10  SP2-RD-BASE-ID OCCURS 10
000855                                 PIC S9(4) COMP-5.
000856         10  SP2-RD-FILE-IND     PIC S9(4) COMP-5.
000857         10  SP2-RD-FILE-LEN     PIC S9(4) COMP-5.
000858         10  SP2-RD-FILE-NAME    PIC X(80).
000859
000860*******************************
000861* font definition             *
000862* parameter for GET-FONT-DEF  *
000863* also used with SET-FONT-DEF *
000864*                QUERY-FONT   *
000865*******************************
000866
000867 01  SP2-FONT-DEF.
000868     05  SP2-FO-RET-CODE         PIC S9(4) COMP-5.
000869     05  SP2-FO-LENS.
000870         10  SP2-FO-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
000871         10  SP2-FO-NUM-LEN      PIC S9(4) COMP-5 VALUE +12.
000872         10  SP2-FO-CHAR-LEN     PIC S9(4) COMP-5 VALUE +6.
000873         10  SP2-FO-VAR-LEN      PIC S9(4) COMP-5 VALUE +30.
000874     05  SP2-FO-DATA.
000875******** SP2-FO-NUM-DATA ********
000876         10  SP2-FO-ID           PIC S9(4) COMP-5.
000877         10  SP2-FO-WIDTH        PIC S9(4) COMP-5.
000878         10  SP2-FO-HEIGHT       PIC S9(4) COMP-5.
000879         10  SP2-FO-GUI-ID       PIC S9(4) COMP-5.
000880         10  SP2-FO-GUI-ID-2     PIC S9(4) COMP-5.
000881         10  SP2-FO-DECIPOINTS   PIC S9(4) COMP-5.
000882******** SP2-FO-CHAR-DATA *******
000883         10  SP2-FO-PITCH        PIC X.
000884         10  SP2-FO-WEIGHT       PIC X.
000885         10  SP2-FO-ITALIC       PIC X.
000886         10  SP2-FO-STRIKE-OUT   PIC X.
000887         10  SP2-FO-UNDERLINE    PIC X.
000888         10  SP2-FO-CHAR-SET     PIC X.
000889******** SP2-FO-VAR-DATA ********
000890         10  SP2-FO-NAME         PIC X(30).
000891
000892********************************
000893* color definition             *
000894* parameter for GET-COLOR-DEF  *
000895* also used with SET-COLOR-DEF *
000896*                QUERY-COLOR   *
000897********************************
000898
000899 01  SP2-COLOR-DEF.
000900     05  SP2-CO-RET-CODE         PIC S9(4) COMP-5.
000901     05  SP2-CO-LENS.
000902         10  SP2-CO-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
000903         10  SP2-CO-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
000904         10  SP2-CO-CHAR-LEN     PIC S9(4) COMP-5 VALUE +16.
000905         10  SP2-CO-VAR-LEN      PIC S9(4) COMP-5 VALUE +0.
000906     05  SP2-CO-DATA.
000907******** SP2-CO-NUM-DATA ********
000908         10  SP2-CO-ID           PIC S9(4) COMP-5.
000909******** SP2-CO-CHAR-DATA *******
000910         10  SP2-CO-FG-BG OCCURS 2.
000911             15  SP2-CO-NUM      PIC X.
000912             15  SP2-CO-TYPE     PIC X.
000913             15  SP2-CO-SYSTEM   PIC X.
000914             15  SP2-CO-TEXT     PIC X.
000915             15  SP2-CO-RED      PIC X.
000916             15  SP2-CO-GREEN    PIC X.
000917             15  SP2-CO-BLUE     PIC X.
000918             15  FILLER          PIC X.
000919******** SP2-CO-VAR-DATA ********
000920         10  SP2-CO-NAME         PIC X(30).
000921
000922*******************************
000923* menu definition             *
000924* parameter for GET-MENU-DEF  *
000925* also used with SET-MENU-DEF *
000926*******************************
000927
000928 01  SP2-MENU-DEF.
000929     05  SP2-MD-RET-CODE         PIC S9(4) COMP-5.
000930     05  SP2-MD-LENS.
000931         10  SP2-MD-LEN-LEN      PIC S9(4) COMP-5 VALUE +18.
000932         10  SP2-MD-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
000933         10  SP2-MD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +18.
000934         10  SP2-MD-VAR-LEN      PIC S9(4) COMP-5 VALUE +3800.
000935         10  SP2-MD-OPTN-LEN     PIC S9(4) COMP-5 VALUE +6.
000936         10  SP2-MD-OPTC-LEN     PIC S9(4) COMP-5 VALUE +2.
000937         10  SP2-MD-OPTV-LEN     PIC S9(4) COMP-5 VALUE +30.
000938         10  SP2-MD-NAME-LEN     PIC S9(4) COMP-5 VALUE +0.
000939         10  SP2-MD-TEXT-LEN     PIC S9(4) COMP-5 VALUE +30.
000940     05  SP2-MD-DATA.
000941******** SP2-MD-NUM-DATA ********
000942         10  SP2-MD-OPTION-CNT   PIC S9(4) COMP-5.
000943******** SP2-MD-CHAR-DATA *******
000944         10  SP2-MD-NAME         PIC X(8).
000945         10  SP2-MD-DRAW-SW      PIC X.
000946         10  FILLER              PIC X.
000947         10  FILLER              PIC X.
000948         10  FILLER              PIC X.
000949         10  FILLER              PIC X.
000950         10  FILLER              PIC X.
000951         10  FILLER              PIC X.
000952         10  FILLER              PIC X.
000953         10  FILLER              PIC X.
000954         10  FILLER              PIC X.
000955******** SP2-MD-VAR-DATA ********
000956         10  SP2-MD-OPTION OCCURS 100.
000957******** SP2-MD-OPTN-DATA *******
000958             15  SP2-MDO-ID      PIC S9(4) COMP-5.
000959             15  SP2-MDO-OWNR-ID PIC S9(4) COMP-5.
000960             15  SP2-MDO-ACC-KEY PIC S9(4) COMP-5.
000961******** SP2-MD-OPTC-DATA *******
000962             15  SP2-MDO-TYPE    PIC X.
000963             15  SP2-MDO-STATE   PIC X.
000964******** SP2-MD-OPTV-DATA *******
000965******** SP2-MDO-NAME ***********
000966             15  SP2-MDO-TEXT    PIC X(30).
000967
000968**********************************
000969* common buffer area             *
000970* parameter for GET-VERSION      *
000971**********************************
000972
000973 01  SP2-BUFFER.
000974     05  SP2-BF-RET-CODE         PIC S9(4) COMP-5.
000975     05  SP2-BF-LEN              PIC S9(4) COMP-5 VALUE +80.
000976     05  SP2-BF-DATA             PIC X(80).
000977
000978********************
000979* property get/set *
000980********************
000981
000982 01  SP2-PROPERTY.
000983     05  SP2-PR-RET-CODE         PIC S9(4) COMP-5.
000984     05  SP2-PR-LENS.
000985         10  SP2-PR-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
000986         10  SP2-PR-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
000987         10  SP2-PR-CHAR-LEN     PIC S9(4) COMP-5 VALUE +20.
000988         10  SP2-PR-VAR-LEN      PIC S9(4) COMP-5 VALUE +2000.
000989     05  SP2-PR-DATA.
000990******** SP2-PR-NUM-DATA ********
000991         10  SP2-PR-ID           PIC S9(4) COMP-5.
000992         10  SP2-PR-ROW          PIC S9(4) COMP-5.
000993         10  SP2-PR-COL          PIC S9(4) COMP-5.
000994******** SP2-PR-CHAR-DATA *******
000995         10  SP2-PR-KEY.
000996             15  SP2-PR-OBJECT-TYPE
000997                                 PIC X.
000998                 88  SP2-PR-WINDOW   VALUE "W".
000999                 88  SP2-PR-PANEL    VALUE "P".
001000                 88  SP2-PR-STATIC   VALUE "S".
001001                 88  SP2-PR-FIELD    VALUE "F".
001002                 88  SP2-PR-GROUP    VALUE "G".
001003                 88  SP2-PR-REPEAT   VALUE "R".
001004             15  SP2-PR-TYPE     PIC X.
001005                 88  SP2-PR-LEN-T    VALUE "L".
001006                 88  SP2-PR-NUM-T    VALUE "N".
001007                 88  SP2-PR-CHAR-T   VALUE "C".
001008                 88  SP2-PR-VAR-T    VALUE "V".
001009             15  SP2-PR-VAR-TYPE PIC X.
001010                 88  SP2-PR-VAR-1    VALUE "A".
001011                 88  SP2-PR-VAR-2    VALUE "B".
001012                 88  SP2-PR-VAR-3    VALUE "C".
001013                 88  SP2-PR-VAR-4    VALUE "D".
001014                 88  SP2-PR-VAR-5    VALUE "E".
001015                 88  SP2-PR-VAR-6    VALUE "F".
001016                 88  SP2-PR-VAR-7    VALUE "G".
001017                 88  SP2-PR-VAR-8    VALUE "H".
001018                 88  SP2-PR-VAR-9    VALUE "I".
001019                 88  SP2-PR-VAR-10   VALUE "J".
001020             15  SP2-PR-OFFSET   PIC 9(5).
001021             15  SP2-PR-LEN      PIC 9(5).
001022             15  SP2-PR-FORMAT   PIC X.
001023                 88  SP2-PR-NUMBER   VALUE "N".
001024                 88  SP2-PR-BINARY   VALUE "B".
001025                 88  SP2-PR-DECIMAL  VALUE "D".
001026             15  SP2-PR-ACTION   PIC X.
001027                 88  SP2-PR-REDRAW   VALUE "R".
001028                 88  SP2-PR-RECREATE VALUE "C".
001029             15  SP2-PR-VAR-ACT  PIC X.
001030                 88  SP2-PR-RESET-LEN
001031                                     VALUE "L".
001032         10  FILLER              PIC X(4).
001033******** SP2-VB-VAR-DATA ********
001034         10  SP2-PR-VALUE        PIC X(2000).
001035         10  SP2-PR-NUM-VALUE REDEFINES SP2-PR-VALUE
001036                                 PIC 9(5).
001037         10  SP2-PR-BIN-VALUE REDEFINES SP2-PR-VALUE.
001038             15  SP2-PR-BIT-VALUE OCCURS 8
001039                                 PIC X.
001040
001041*********************
001042* END OF PARAMETERS *
001043*********************
001044
001045*************************
001046* SP2 KEY VALUES        *
001047* returned in KEY field *
001048* in CONVERSE-DATA      *
001049*************************
001050
001051 01  SP2-KEYS.
001052     05  SP2-KEY-CTRL-A          PIC S9(4) COMP-5 VALUE +1.
001053     05  SP2-KEY-CTRL-B          PIC S9(4) COMP-5 VALUE +2.
001054     05  SP2-KEY-CTRL-C          PIC S9(4) COMP-5 VALUE +3.
001055     05  SP2-KEY-CTRL-D          PIC S9(4) COMP-5 VALUE +4.
001056     05  SP2-KEY-CTRL-E          PIC S9(4) COMP-5 VALUE +5.
001057     05  SP2-KEY-CTRL-F          PIC S9(4) COMP-5 VALUE +6.
001058     05  SP2-KEY-CTRL-G          PIC S9(4) COMP-5 VALUE +7.
001059     05  SP2-KEY-BACKSPAC        PIC S9(4) COMP-5 VALUE +8.
001060     05  SP2-KEY-TAB             PIC S9(4) COMP-5 VALUE +9.
001061     05  SP2-KEY-CTRL-J          PIC S9(4) COMP-5 VALUE +10.
001062     05  SP2-KEY-CTRL-K          PIC S9(4) COMP-5 VALUE +11.
001063     05  SP2-KEY-CTRL-L          PIC S9(4) COMP-5 VALUE +12.
001064     05  SP2-KEY-ENTER           PIC S9(4) COMP-5 VALUE +13.
001065     05  SP2-KEY-CTRL-N          PIC S9(4) COMP-5 VALUE +14.
001066     05  SP2-KEY-CTRL-O          PIC S9(4) COMP-5 VALUE +15.
001067     05  SP2-KEY-CTRL-P          PIC S9(4) COMP-5 VALUE +16.
001068     05  SP2-KEY-CTRL-Q          PIC S9(4) COMP-5 VALUE +17.
001069     05  SP2-KEY-CTRL-R          PIC S9(4) COMP-5 VALUE +18.
001070     05  SP2-KEY-CTRL-S          PIC S9(4) COMP-5 VALUE +19.
001071     05  SP2-KEY-CTRL-T          PIC S9(4) COMP-5 VALUE +20.
001072     05  SP2-KEY-CTRL-U          PIC S9(4) COMP-5 VALUE +21.
001073     05  SP2-KEY-CTRL-V          PIC S9(4) COMP-5 VALUE +22.
001074     05  SP2-KEY-CTRL-W          PIC S9(4) COMP-5 VALUE +23.
001075     05  SP2-KEY-CTRL-X          PIC S9(4) COMP-5 VALUE +24.
001076     05  SP2-KEY-CTRL-Y          PIC S9(4) COMP-5 VALUE +25.
001077     05  SP2-KEY-CTRL-Z          PIC S9(4) COMP-5 VALUE +26.
001078     05  SP2-KEY-ESC             PIC S9(4) COMP-5 VALUE +27.
001079     05  SP2-KEY-ESCAPE REDEFINES SP2-KEY-ESC
001080                                 PIC S9(4) COMP-5.
001081     05  SP2-KEY-CTRL-BACKSLASH  PIC S9(4) COMP-5 VALUE +28.
001082     05  SP2-KEY-CTRL-BRACKET    PIC S9(4) COMP-5 VALUE +29.
001083     05  SP2-KEY-CTRL-MINUS      PIC S9(4) COMP-5 VALUE +31.
001084     05  SP2-KEY-SPACEBAR        PIC S9(4) COMP-5 VALUE +32.
001085     05  SP2-KEY-BACKTAB         PIC S9(4) COMP-5 VALUE +271.
001086     05  SP2-KEY-ALT-Q           PIC S9(4) COMP-5 VALUE +272.
001087     05  SP2-KEY-ALT-W           PIC S9(4) COMP-5 VALUE +273.
001088     05  SP2-KEY-ALT-E           PIC S9(4) COMP-5 VALUE +274.
001089     05  SP2-KEY-ALT-R           PIC S9(4) COMP-5 VALUE +275.
001090     05  SP2-KEY-ALT-T           PIC S9(4) COMP-5 VALUE +276.
001091     05  SP2-KEY-ALT-Y           PIC S9(4) COMP-5 VALUE +277.
001092     05  SP2-KEY-ALT-U           PIC S9(4) COMP-5 VALUE +278.
001093     05  SP2-KEY-ALT-I           PIC S9(4) COMP-5 VALUE +279.
001094     05  SP2-KEY-ALT-O           PIC S9(4) COMP-5 VALUE +280.
001095     05  SP2-KEY-ALT-P           PIC S9(4) COMP-5 VALUE +281.
001096     05  SP2-KEY-ALT-A           PIC S9(4) COMP-5 VALUE +286.
001097     05  SP2-KEY-ALT-S           PIC S9(4) COMP-5 VALUE +287.
001098     05  SP2-KEY-ALT-D           PIC S9(4) COMP-5 VALUE +288.
001099     05  SP2-KEY-ALT-F           PIC S9(4) COMP-5 VALUE +289.
001100     05  SP2-KEY-ALT-G           PIC S9(4) COMP-5 VALUE +290.
001101     05  SP2-KEY-ALT-H           PIC S9(4) COMP-5 VALUE +291.
001102     05  SP2-KEY-ALT-J           PIC S9(4) COMP-5 VALUE +292.
001103     05  SP2-KEY-ALT-K           PIC S9(4) COMP-5 VALUE +293.
001104     05  SP2-KEY-ALT-L           PIC S9(4) COMP-5 VALUE +294.
001105     05  SP2-KEY-ALT-Z           PIC S9(4) COMP-5 VALUE +300.
001106     05  SP2-KEY-ALT-X           PIC S9(4) COMP-5 VALUE +301.
001107     05  SP2-KEY-ALT-C           PIC S9(4) COMP-5 VALUE +302.
001108     05  SP2-KEY-ALT-V           PIC S9(4) COMP-5 VALUE +303.
001109     05  SP2-KEY-ALT-B           PIC S9(4) COMP-5 VALUE +304.
001110     05  SP2-KEY-ALT-N           PIC S9(4) COMP-5 VALUE +305.
001111     05  SP2-KEY-ALT-M           PIC S9(4) COMP-5 VALUE +306.
001112     05  SP2-KEY-F1              PIC S9(4) COMP-5 VALUE +315.
001113     05  SP2-KEY-F2              PIC S9(4) COMP-5 VALUE +316.
001114     05  SP2-KEY-F3              PIC S9(4) COMP-5 VALUE +317.
001115     05  SP2-KEY-F4              PIC S9(4) COMP-5 VALUE +318.
001116     05  SP2-KEY-F5              PIC S9(4) COMP-5 VALUE +319.
001117     05  SP2-KEY-F6              PIC S9(4) COMP-5 VALUE +320.
001118     05  SP2-KEY-F7              PIC S9(4) COMP-5 VALUE +321.
001119     05  SP2-KEY-F8              PIC S9(4) COMP-5 VALUE +322.
001120     05  SP2-KEY-F9              PIC S9(4) COMP-5 VALUE +323.
001121     05  SP2-KEY-F10             PIC S9(4) COMP-5 VALUE +324.
001122     05  SP2-KEY-HOME            PIC S9(4) COMP-5 VALUE +327.
001123     05  SP2-KEY-UP              PIC S9(4) COMP-5 VALUE +328.
001124     05  SP2-KEY-PGUP            PIC S9(4) COMP-5 VALUE +329.
001125     05  SP2-KEY-LEFT            PIC S9(4) COMP-5 VALUE +331.
001126     05  SP2-KEY-RIGHT           PIC S9(4) COMP-5 VALUE +333.
001127     05  SP2-KEY-END             PIC S9(4) COMP-5 VALUE +335.
001128     05  SP2-KEY-DOWN            PIC S9(4) COMP-5 VALUE +336.
001129     05  SP2-KEY-PGDN            PIC S9(4) COMP-5 VALUE +337.
001130     05  SP2-KEY-INSERT          PIC S9(4) COMP-5 VALUE +338.
001131     05  SP2-KEY-DELETE          PIC S9(4) COMP-5 VALUE +339.
001132     05  SP2-KEY-SHIFT-F1        PIC S9(4) COMP-5 VALUE +340.
001133     05  SP2-KEY-SHIFT-F2        PIC S9(4) COMP-5 VALUE +341.
001134     05  SP2-KEY-SHIFT-F3        PIC S9(4) COMP-5 VALUE +342.
001135     05  SP2-KEY-SHIFT-F4        PIC S9(4) COMP-5 VALUE +343.
001136     05  SP2-KEY-SHIFT-F5        PIC S9(4) COMP-5 VALUE +344.
001137     05  SP2-KEY-SHIFT-F6        PIC S9(4) COMP-5 VALUE +345.
001138     05  SP2-KEY-SHIFT-F7        PIC S9(4) COMP-5 VALUE +346.
001139     05  SP2-KEY-SHIFT-F8        PIC S9(4) COMP-5 VALUE +347.
001140     05  SP2-KEY-SHIFT-F9        PIC S9(4) COMP-5 VALUE +348.
001141     05  SP2-KEY-SHIFT-F10       PIC S9(4) COMP-5 VALUE +349.
001142     05  SP2-KEY-CTRL-F1         PIC S9(4) COMP-5 VALUE +350.
001143     05  SP2-KEY-CTRL-F2         PIC S9(4) COMP-5 VALUE +351.
001144     05  SP2-KEY-CTRL-F3         PIC S9(4) COMP-5 VALUE +352.
001145     05  SP2-KEY-CTRL-F4         PIC S9(4) COMP-5 VALUE +353.
001146     05  SP2-KEY-CTRL-F5         PIC S9(4) COMP-5 VALUE +354.
001147     05  SP2-KEY-CTRL-F6         PIC S9(4) COMP-5 VALUE +355.
001148     05  SP2-KEY-CTRL-F7         PIC S9(4) COMP-5 VALUE +356.
001149     05  SP2-KEY-CTRL-F8         PIC S9(4) COMP-5 VALUE +357.
001150     05  SP2-KEY-CTRL-F9         PIC S9(4) COMP-5 VALUE +358.
001151     05  SP2-KEY-CTRL-F10        PIC S9(4) COMP-5 VALUE +359.
001152     05  SP2-KEY-ALT-F1          PIC S9(4) COMP-5 VALUE +360.
001153     05  SP2-KEY-ALT-F2          PIC S9(4) COMP-5 VALUE +361.
001154     05  SP2-KEY-ALT-F3          PIC S9(4) COMP-5 VALUE +362.
001155     05  SP2-KEY-ALT-F4          PIC S9(4) COMP-5 VALUE +363.
001156     05  SP2-KEY-ALT-F5          PIC S9(4) COMP-5 VALUE +364.
001157     05  SP2-KEY-ALT-F6          PIC S9(4) COMP-5 VALUE +365.
001158     05  SP2-KEY-ALT-F7          PIC S9(4) COMP-5 VALUE +366.
001159     05  SP2-KEY-ALT-F8          PIC S9(4) COMP-5 VALUE +367.
001160     05  SP2-KEY-ALT-F9          PIC S9(4) COMP-5 VALUE +368.
001161     05  SP2-KEY-ALT-F10         PIC S9(4) COMP-5 VALUE +369.
001162     05  SP2-KEY-CTRL-PRTSC      PIC S9(4) COMP-5 VALUE +370.
001163     05  SP2-KEY-CTRL-LEFT       PIC S9(4) COMP-5 VALUE +371.
001164     05  SP2-KEY-CTRL-RIGHT      PIC S9(4) COMP-5 VALUE +372.
001165     05  SP2-KEY-CTRL-END        PIC S9(4) COMP-5 VALUE +373.
001166     05  SP2-KEY-CTRL-PGDN       PIC S9(4) COMP-5 VALUE +374.
001167     05  SP2-KEY-CTRL-HOME       PIC S9(4) COMP-5 VALUE +375.
001168     05  SP2-KEY-ALT-1           PIC S9(4) COMP-5 VALUE +376.
001169     05  SP2-KEY-ALT-2           PIC S9(4) COMP-5 VALUE +377.
001170     05  SP2-KEY-ALT-3           PIC S9(4) COMP-5 VALUE +378.
001171     05  SP2-KEY-ALT-4           PIC S9(4) COMP-5 VALUE +379.
001172     05  SP2-KEY-ALT-5           PIC S9(4) COMP-5 VALUE +380.
001173     05  SP2-KEY-ALT-6           PIC S9(4) COMP-5 VALUE +381.
001174     05  SP2-KEY-ALT-7           PIC S9(4) COMP-5 VALUE +382.
001175     05  SP2-KEY-ALT-8           PIC S9(4) COMP-5 VALUE +383.
001176     05  SP2-KEY-ALT-9           PIC S9(4) COMP-5 VALUE +384.
001177     05  SP2-KEY-ALT-0           PIC S9(4) COMP-5 VALUE +385.
001178     05  SP2-KEY-ALT-MINUS       PIC S9(4) COMP-5 VALUE +386.
001179     05  SP2-KEY-ALT-EQUAL       PIC S9(4) COMP-5 VALUE +387.
001180     05  SP2-KEY-CTRL-PGUP       PIC S9(4) COMP-5 VALUE +388.
001181     05  SP2-KEY-F11             PIC S9(4) COMP-5 VALUE +389.
001182     05  SP2-KEY-F12             PIC S9(4) COMP-5 VALUE +390.
001183     05  SP2-KEY-SHIFT-F11       PIC S9(4) COMP-5 VALUE +391.
001184     05  SP2-KEY-SHIFT-F12       PIC S9(4) COMP-5 VALUE +392.
001185     05  SP2-KEY-CTRL-F11        PIC S9(4) COMP-5 VALUE +393.
001186     05  SP2-KEY-CTRL-F12        PIC S9(4) COMP-5 VALUE +394.
001187     05  SP2-KEY-ALT-F11         PIC S9(4) COMP-5 VALUE +395.
001188     05  SP2-KEY-ALT-F12         PIC S9(4) COMP-5 VALUE +396.
001189     05  SP2-KEY-CTRL-UP         PIC S9(4) COMP-5 VALUE +397.
001190     05  SP2-KEY-CTRL-DOWN       PIC S9(4) COMP-5 VALUE +401.
001191     05  SP2-KEY-CTRL-INSERT     PIC S9(4) COMP-5 VALUE +402.
001192     05  SP2-KEY-CTRL-DELETE     PIC S9(4) COMP-5 VALUE +403.
001193     05  SP2-KEY-CTRL-TAB        PIC S9(4) COMP-5 VALUE +404.
001194     05  SP2-KEY-TIMEOUT         PIC S9(4) COMP-5 VALUE -1.
001195     05  SP2-KEY-SELECT          PIC S9(4) COMP-5 VALUE -2.
001196     05  SP2-KEY-SWITCH          PIC S9(4) COMP-5 VALUE -3.
001197     05  SP2-KEY-CTRL-FIELD      PIC S9(4) COMP-5 VALUE -4.
001198     05  SP2-KEY-CLOSE           PIC S9(4) COMP-5 VALUE -5.
001199     05  SP2-KEY-MENU            PIC S9(4) COMP-5 VALUE -6.
001200     05  SP2-KEY-MORE            PIC S9(4) COMP-5 VALUE -7.
001201     05  SP2-KEY-SIZE            PIC S9(4) COMP-5 VALUE -9.
001202     05  SP2-KEY-MOUSE           PIC S9(4) COMP-5 VALUE -10.
001203     05  SP2-KEY-CLICK-RIGHT     PIC S9(4) COMP-5 VALUE -11.
001204     05  SP2-KEY-DOUBLE-CLICK    PIC S9(4) COMP-5 VALUE -12.
001205     05  SP2-KEY-DOUBLE-RIGHT    PIC S9(4) COMP-5 VALUE -13.
001206     05  SP2-KEY-CLICK-OUTSIDE   PIC S9(4) COMP-5 VALUE -14.
001207     05  SP2-KEY-TOOLBAR         PIC S9(4) COMP-5 VALUE -15.
001208     05  SP2-KEY-VBX             PIC S9(4) COMP-5 VALUE -16.
001209     05  SP2-KEY-SCROLL-CLICK    PIC S9(4) COMP-5 VALUE -18.
001210     05  SP2-KEY-SWITCH-DENIED   PIC S9(4) COMP-5 VALUE -19.
001211     05  SP2-KEY-SYS-SHUTDOWN    PIC S9(4) COMP-5 VALUE -22.
001212     05  SP2-KEY-APP-CLOSE       PIC S9(4) COMP-5 VALUE -23.
001213
001214*********************
001215* END OF KEY VALUES *
001216*********************
001217
001218******************************
001219* SP2 FIELD TYPE CODES       *
001220* used with SP2-CD-TYPE-DATA *
001221******************************
001222
001223 01  SP2-FIELD-TYPES.
001224     05  SP2-DISPLAY-ONLY        PIC X VALUE "o".
001225     05  SP2-GREYED-OUT          PIC X VALUE "g".
001226
001227**********************
001228* END OF FIELD TYPES *
001229**********************
001230
001231**************************
001232* SP2 LINE DRAWING CODES *
001233* used with static data  *
001234**************************
001235
001236 01  SP2-LINES.
001237     05  SP2-LINE-TOP-LEFT       PIC X VALUE X"DA".
001238     05  SP2-LINE-HORIZONTAL     PIC X VALUE X"C4".
001239     05  SP2-LINE-TOP-RIGHT      PIC X VALUE X"BF".
001240     05  SP2-LINE-VERTICAL       PIC X VALUE X"B3".
001241     05  SP2-LINE-BOTTOM-LEFT    PIC X VALUE X"C0".
001242     05  SP2-LINE-BOTTOM-RIGHT   PIC X VALUE X"D9".
001243
001244***********************
001245* END OF LINE DRAWING *
001246***********************
001247
001248*************************
001249* SP2 MOUSE SHAPE CODES *
001250*************************
001251
001252 01  SP2-MOUSE-SHAPES.
001253     05  SP2-MOUSE-ARROW         PIC S9(4) COMP-5 VALUE +0.
001254     05  SP2-MOUSE-LOCATE        PIC S9(4) COMP-5 VALUE +1.
001255     05  SP2-MOUSE-TOP-LEFT      PIC S9(4) COMP-5 VALUE +2.
001256     05  SP2-MOUSE-TOP           PIC S9(4) COMP-5 VALUE +3.
001257     05  SP2-MOUSE-TOP-RIGHT     PIC S9(4) COMP-5 VALUE +4.
001258     05  SP2-MOUSE-RIGHT         PIC S9(4) COMP-5 VALUE +5.
001259     05  SP2-MOUSE-WAIT          PIC S9(4) COMP-5 VALUE +6.
001260     05  SP2-MOUSE-BOTTOM-RIGHT  PIC S9(4) COMP-5 VALUE +7.
001261     05  SP2-MOUSE-BOTTOM        PIC S9(4) COMP-5 VALUE +8.
001262     05  SP2-MOUSE-BOTTOM-LEFT   PIC S9(4) COMP-5 VALUE +9.
001263     05  SP2-MOUSE-LEFT          PIC S9(4) COMP-5 VALUE +10.
001264
001265***********************
001266* END OF MOUSE SHAPES *
001267***********************
001268
001269**************************
001270* SP2 SYSTEM COLOR CODES *
001271**************************
001272
001273 01  SP2-SYSTEM-COLRS.
001274     05  SP2-COLR-DEFAULT        PIC X VALUE X"00".
001275     05  SP2-COLR-WINDOW         PIC X VALUE X"01".
001276     05  SP2-COLR-HIGHLIGHT      PIC X VALUE X"02".
001277     05  SP2-COLR-MENU           PIC X VALUE X"03".
001278
001279*****************************
001280* END OF SYSTEM COLOR CODES *
001281*****************************
      *<<((file: SP2.CPY))
000397*
000398*        COPY "MAIN.CPY".
      *>>((file: MAIN.CPY))
000001********************************
000002* parameter for CONVERSE-PANEL *
000003* parameter for GET-INPUT      *
000004********************************
000005 01  MAIN-CONVERSE-DATA.
000006     05  MAIN-RET-CODE
000007                                 PIC S9(4) COMP-5.
000008     05  MAIN-LENS.
000009         10  MAIN-LEN-LEN
000010                                 PIC S9(4) COMP-5 VALUE +20.
000011         10  MAIN-IP-NUM-LEN
000012                                 PIC S9(4) COMP-5 VALUE +40.
000013         10  MAIN-IP-CHAR-LEN
000014                                 PIC S9(4) COMP-5 VALUE +106.
000015         10  MAIN-OP-NUM-LEN
000016                                 PIC S9(4) COMP-5 VALUE +6.
000017         10  MAIN-OP-CHAR-LEN
000018                                 PIC S9(4) COMP-5 VALUE +2.
000019         10  MAIN-FIELD-LEN
000020                                 PIC S9(4) COMP-5 VALUE +290.
000021         10  MAIN-COLR-LEN
000022                                 PIC S9(4) COMP-5 VALUE +13.
000023         10  MAIN-TYPE-LEN
000024                                 PIC S9(4) COMP-5 VALUE +13.
000025         10  FILLER
000026                                 PIC S9(4) COMP-5 VALUE +0.
000027         10  FILLER
000028                                 PIC S9(4) COMP-5 VALUE +0.
000029     05  MAIN-DATA.
000030******** MAIN-IP-NUM-DATA ********
000031         10  MAIN-KEY
000032                                 PIC S9(4) COMP-5.
000033             88  MAIN-Continue-HIT
000034                                 VALUE 302.
000035         10  MAIN-NEXT-FLD-ID
000036                                 PIC S9(4) COMP-5.
000037         10  MAIN-NEXT-FLD-NUM
000038                                 PIC S9(4) COMP-5.
000039         10  MAIN-NEXT-TAB-NUM
000040                                 PIC S9(4) COMP-5.
000041         10  MAIN-NEXT-OCCURS
000042                                 PIC S9(4) COMP-5.
000043         10  MAIN-LAST-FLD-ID
000044                                 PIC S9(4) COMP-5.
000045         10  MAIN-LAST-FLD-NUM
000046                                 PIC S9(4) COMP-5.
000047         10  MAIN-LAST-TAB-NUM
000048                                 PIC S9(4) COMP-5.
000049         10  MAIN-LAST-OCCURS
000050                                 PIC S9(4) COMP-5.
000051         10  MAIN-MENU-ID
000052                                 PIC S9(4) COMP-5.
000053         10  MAIN-ROW-COL-SW
000054                                 PIC S9(4) COMP-5.
000055         10  MAIN-CURSOR-ROW
000056                                 PIC S9(4) COMP-5.
000057         10  MAIN-CURSOR-COL
000058                                 PIC S9(4) COMP-5.
000059         10  MAIN-LAST-ROW
000060                                 PIC S9(4) COMP-5.
000061         10  MAIN-LAST-COL
000062                                 PIC S9(4) COMP-5.
000063         10  MAIN-DISP-SW
000064                                 PIC S9(4) COMP-5.
000065         10  MAIN-NEXT-VERT
000066                                 PIC S9(4) COMP-5.
000067         10  MAIN-LAST-VERT
000068                                 PIC S9(4) COMP-5.
000069         10  MAIN-NEXT-HOR
000070                                 PIC S9(4) COMP-5.
000071         10  MAIN-LAST-HOR
000072                                 PIC S9(4) COMP-5.
000073******** MAIN-IP-CHAR-DATA ********
000074         10  MAIN-NEXT-PANEL
000075                                 PIC X(8).
000076         10  MAIN-NEXT-FIELD
000077                                 PIC X(30).
000078         10  MAIN-LAST-FIELD
000079                                 PIC X(30).
000080         10  MAIN-MENU-OPTION
000081                                 PIC X(30).
000082         10  MAIN-SWITCH-SW
000083                                 PIC X.
000084         10  MAIN-SIZE-SW
000085                                 PIC X.
000086         10  MAIN-MOUSE-SW
000087                                 PIC X.
000088         10  MAIN-CAPTURE-SW
000089                                 PIC X.
000090         10  MAIN-WAIT-SW
000091                                 PIC X.
000092         10  MAIN-CURS-SW
000093                                 PIC X.
000094         10  MAIN-CHG-SW
000095                                 PIC X.
000096         10  MAIN-TIMEOUT
000097                                 PIC X.
000098******** MAIN-OP-NUM-DATA ********
000099         10  MAIN-PAN-POS-SW
000100                                 PIC S9(4) COMP-5.
000101         10  MAIN-PAN-ROW
000102                                 PIC S9(4) COMP-5.
000103         10  MAIN-PAN-COL
000104                                 PIC S9(4) COMP-5.
000105******** MAIN-OP-CHAR-DATA ********
000106         10  MAIN-NEW-WINDOW
000107                                 PIC X.
000108         10  MAIN-DISPLAY-SW
000109                                 PIC X.
000110******** MAIN-OP-VAR-DATA ********
000111     05  MAIN-FIELDS.
000112         10  MAIN-STATE
000113                                 PIC X(00020).
000114         10  MAIN-PRODUCT
000115                                 PIC X(00035).
000116         10  MAIN-LOAN-REFUND-TYPE
000117                                 PIC X(00050).
000118         10  MAIN-LOAN-REFUND-TYPE-TEXT
000119                                 PIC X(00025).
000120         10  MAIN-RATE-FILE-DESCR
000121                                 PIC X(00060).
000122         10  MAIN-RATE-FILE-DESCR-TEXT
000123                                 PIC X(00025).
000124         10  MAIN-LIMIT-FILE
000125                                 PIC X(00060).
000126         10  MAIN-LIMIT-FILE-TEXT
000127                                 PIC X(00015).
000128     05  MAIN-COLRS.
000129         10  MAIN-FIELD-ID-8-C
000130                                 PIC X.
000131         10  MAIN-STATE-C
000132                                 PIC X.
000133         10  MAIN-PRODUCT-C
000134                                 PIC X.
000135         10  MAIN-LOAN-REFUND-TYPE-C
000136                                 PIC X.
000137         10  MAIN-LOAN-REFUND-TYPE-TEXT-C
000138                                 PIC X.
000139         10  MAIN-RATE-FILE-DESCR-C
000140                                 PIC X.
000141         10  MAIN-RATE-FILE-DESCR-TEXT-C
000142                                 PIC X.
000143         10  MAIN-LIMIT-FILE-C
000144                                 PIC X.
000145         10  MAIN-LIMIT-FILE-TEXT-C
000146                                 PIC X.
000147         10  MAIN-Continue-C
000148                                 PIC X.
000149         10  TB-RUN-ICON-C
000150                                 PIC X.
000151         10  TB-AGENT-ICON-C
000152                                 PIC X.
000153         10  TB-EXIT-ICON-C
000154                                 PIC X.
000155     05  MAIN-TYPES.
000156         10  MAIN-FIELD-ID-8-T
000157                                 PIC X.
000158         10  MAIN-STATE-T
000159                                 PIC X.
000160         10  MAIN-PRODUCT-T
000161                                 PIC X.
000162         10  MAIN-LOAN-REFUND-TYPE-T
000163                                 PIC X.
000164         10  MAIN-LOAN-REFUND-TYPE-TEXT-T
000165                                 PIC X.
000166         10  MAIN-RATE-FILE-DESCR-T
000167                                 PIC X.
000168         10  MAIN-RATE-FILE-DESCR-TEXT-T
000169                                 PIC X.
000170         10  MAIN-LIMIT-FILE-T
000171                                 PIC X.
000172         10  MAIN-LIMIT-FILE-TEXT-T
000173                                 PIC X.
000174         10  MAIN-Continue-T
000175                                 PIC X.
000176         10  TB-RUN-ICON-T
000177                                 PIC X.
000178         10  TB-AGENT-ICON-T
000179                                 PIC X.
000180         10  TB-EXIT-ICON-T
000181                                 PIC X.
000182************************************************
000183* field ids - use for cursor positioning, etc. *
000184************************************************
000185 01  MAIN-IDS.
000186     05  MAIN-FIELD-ID-8-I
000187                                 PIC S9(4) COMP-5 VALUE +8.
000188     05  MAIN-STATE-I
000189                                 PIC S9(4) COMP-5 VALUE +1.
000190     05  MAIN-PRODUCT-I
000191                                 PIC S9(4) COMP-5 VALUE +2.
000192     05  MAIN-LOAN-REFUND-TYPE-I
000193                                 PIC S9(4) COMP-5 VALUE +4.
000194     05  MAIN-LOAN-REFUND-TYPE-TEXT-I
000195                                 PIC S9(4) COMP-5 VALUE +7.
000196     05  MAIN-RATE-FILE-DESCR-I
000197                                 PIC S9(4) COMP-5 VALUE +3.
000198     05  MAIN-RATE-FILE-DESCR-TEXT-I
000199                                 PIC S9(4) COMP-5 VALUE +6.
000200     05  MAIN-LIMIT-FILE-I
000201                                 PIC S9(4) COMP-5 VALUE +9.
000202     05  MAIN-LIMIT-FILE-TEXT-I
000203                                 PIC S9(4) COMP-5 VALUE +10.
000204     05  MAIN-Continue-I
000205                                 PIC S9(4) COMP-5 VALUE +5.
000206     05  TB-RUN-ICON-I
000207                                 PIC S9(4) COMP-5 VALUE +5.
000208     05  TB-AGENT-ICON-I
000209                                 PIC S9(4) COMP-5 VALUE +1.
000210     05  TB-EXIT-ICON-I
000211                                 PIC S9(4) COMP-5 VALUE +6.
      *<<((file: MAIN.CPY))
000399*
000400*        COPY "QPR.CPY".
      *>>((file: QPR.CPY))
000001
000002*****************************
000003* Standard copy for QPR.DLL *
000004*****************************
000005
000006 01  QPR-FUNCTIONS.
000007     03  QPR-SELECT-PRINTER    PIC S9(4) COMP-5 VALUE 0.
000008     03  QPR-PRINT-PAGE        PIC S9(4) COMP-5 VALUE 1.
000009     03  QPR-END-PRINT         PIC S9(4) COMP-5 VALUE 2.
000010     03  QPR-ABOUT             PIC S9(4) COMP-5 VALUE 3.
000011     03  QPR-INIT              PIC S9(4) COMP-5 VALUE 4.
000012     03  QPR-SELECT-PRINTER-EX PIC S9(4) COMP-5 VALUE 6.
000013
000014 01  QPR-AREA.
000015     05  QPR-RET-CODE          PIC S9(4) COMP-5.
000016     05  QPR-DATA.
000017         10  QPR-DIALOG        PIC X.
000018         10  QPR-DOC-NAME      PIC X(30).
000019         10  QPR-ORIENTATION   PIC X.
000020             88  QPR-LANDSCAPE           VALUE "l".
000021         10  QPR-COPIES        PIC S9(4) COMP-5.
000022         10  QPR-STRETCH       PIC X.
000023         10  QPR-DEVICE-LIST   PIC X(800).
000024         10  FILLER REDEFINES QPR-DEVICE-LIST.
000025             15  QPR-DEVICE-TO-SELECT
000026                               PIC X(256).
000027             15  QPR-PRINT-FILE-NAME
000028                               PIC X(256).
000029         10  QPR-DRIVER        PIC X(32).
000030         10  QPR-OUTPUT        PIC X(32).
000031         10  QPR-DATA-EX.
000032             15  QPR-PAPER-SIZE
000033                               PIC X.
000034                 88  QPR-LETTER          VALUE X"01".
000035                 88  QPR-LEGAL           VALUE X"05".
000036                 88  QPR-A4              VALUE X"09".
000037                 88  QPR-B5              VALUE X"0D".
000038             15  QPR-PAPER-SOURCE
000039                               PIC X.
000040                 88  QPR-UPPER-TRAY      VALUE X"01".
000041                 88  QPR-LOWER-TRAY      VALUE X"02".
000042                 88  QPR-MIDDLE-TRAY     VALUE X"03".
000043                 88  QPR-MANUAL-FEED     VALUE X"04".
000044             15  QPR-PREVIEW   PIC X.
000045                 88  QPR-DO-PREVIEW      VALUE "y".
000046             15  QPR-LEFT-MARGIN
000047                               PIC S9(4) COMP-5.
000048             15  QPR-TOP-MARGIN
000049                               PIC S9(4) COMP-5.
000050             15  QPR-ALLOW-COPIES
000051                               PIC X.
000052             15  QPR-DUPLEX    PIC X.
000053                 88  QPR-DUP-SIMPLEX     VALUE X"01".
000054                 88  QPR-DUP-VERTICAL    VALUE X"02".
000055                 88  QPR-DUP-HORIZONTAL  VALUE X"03".
000056             15  FILLER        PIC X(8).
000057             15  QPR-PAPER-LENGTH
000058                               PIC S9(4) COMP-5.
000059             15  QPR-PAPER-WIDTH
000060                               PIC S9(4) COMP-5.
000061             15  QPR-PRINT-TO-FILE
000062                               PIC X.
000063             15  QPR-RANGE     PIC X.
000064             15  QPR-RANGE-FROM
000065                               PIC S9(4) COMP-5.
000066             15  QPR-RANGE-TO  PIC S9(4) COMP-5.
000067             15  FILLER        PIC X(75).
000068
000069 01  QPR-VERSION.
000070     05  QPR-QE-RET            PIC S9(4) COMP-5.
000071     05  QPR-QE-DATA.
000072         10  QPR-VERSION-INFO  PIC X(80).
000073         10  QPR-DISPLAY-BOX   PIC X.
000074
000075 01  QPR-INIT-AREA.
000076     05  QPR-QI-RET            PIC S9(4) COMP-5.
000077     05  QPR-QI-DATA.
000078         10  QPR-LIBRARY       PIC X.
000079         10  QPR-METHOD        PIC X.
000080         10  QPR-FILE          PIC X(80).
000081         10  FILLER            PIC X(118).
000082
000083 01  QPR-NULL-PARM.
000084     05  QPR-NP-RET-CODE       PIC S9(4) COMP-5.
000085
      *<<((file: QPR.CPY))
000401*
000402*        COPY "P-CID-GL.CPY".
      *>>((file: P-CID-GL.CPY))
000001********************************
000002* parameter for CONVERSE-PANEL *
000003********************************
000004 01  P-CID-GL-CONVERSE-DATA.
000005     05  P-CID-GL-RET-CODE
000006                                 PIC S9(4) COMP-5.
000007     05  P-CID-GL-LENS.
000008         10  P-CID-GL-LEN-LEN
000009                                 PIC S9(4) COMP-5 VALUE +20.
000010         10  P-CID-GL-IP-NUM-LEN
000011                                 PIC S9(4) COMP-5 VALUE +40.
000012         10  P-CID-GL-IP-CHAR-LEN
000013                                 PIC S9(4) COMP-5 VALUE +104.
000014         10  P-CID-GL-OP-NUM-LEN
000015                                 PIC S9(4) COMP-5 VALUE +6.
000016         10  P-CID-GL-OP-CHAR-LEN
000017                                 PIC S9(4) COMP-5 VALUE +2.
000018         10  P-CID-GL-FIELD-LEN
000019                                 PIC S9(4) COMP-5 VALUE +100.
000020         10  P-CID-GL-COLR-LEN
000021                                 PIC S9(4) COMP-5 VALUE +2.
000022         10  P-CID-GL-TYPE-LEN
000023                                 PIC S9(4) COMP-5 VALUE +2.
000024         10  FILLER
000025                                 PIC S9(4) COMP-5 VALUE +0.
000026         10  FILLER
000027                                 PIC S9(4) COMP-5 VALUE +0.
000028     05  P-CID-GL-DATA.
000029******** P-CID-GL-IP-NUM-DATA ********
000030         10  P-CID-GL-KEY
000031                                 PIC S9(4) COMP-5.
000032         10  P-CID-GL-NEXT-FLD-ID
000033                                 PIC S9(4) COMP-5.
000034         10  P-CID-GL-NEXT-FLD-NUM
000035                                 PIC S9(4) COMP-5.
000036         10  P-CID-GL-NEXT-TAB-NUM
000037                                 PIC S9(4) COMP-5.
000038         10  P-CID-GL-NEXT-OCCURS
000039                                 PIC S9(4) COMP-5.
000040         10  P-CID-GL-LAST-FLD-ID
000041                                 PIC S9(4) COMP-5.
000042         10  P-CID-GL-LAST-FLD-NUM
000043                                 PIC S9(4) COMP-5.
000044         10  P-CID-GL-LAST-TAB-NUM
000045                                 PIC S9(4) COMP-5.
000046         10  P-CID-GL-LAST-OCCURS
000047                                 PIC S9(4) COMP-5.
000048         10  P-CID-GL-MENU-ID
000049                                 PIC S9(4) COMP-5.
000050         10  P-CID-GL-ROW-COL-SW
000051                                 PIC S9(4) COMP-5.
000052         10  P-CID-GL-CURSOR-ROW
000053                                 PIC S9(4) COMP-5.
000054         10  P-CID-GL-CURSOR-COL
000055                                 PIC S9(4) COMP-5.
000056         10  P-CID-GL-LAST-ROW
000057                                 PIC S9(4) COMP-5.
000058         10  P-CID-GL-LAST-COL
000059                                 PIC S9(4) COMP-5.
000060         10  P-CID-GL-DISP-SW
000061                                 PIC S9(4) COMP-5.
000062         10  P-CID-GL-NEXT-VERT
000063                                 PIC S9(4) COMP-5.
000064         10  P-CID-GL-LAST-VERT
000065                                 PIC S9(4) COMP-5.
000066         10  P-CID-GL-NEXT-HOR
000067                                 PIC S9(4) COMP-5.
000068         10  P-CID-GL-LAST-HOR
000069                                 PIC S9(4) COMP-5.
000070******** P-CID-GL-IP-NUM-DATA ********
000071         10  P-CID-GL-NEXT-PANEL
000072                                 PIC X(8).
000073         10  P-CID-GL-NEXT-FIELD
000074                                 PIC X(30).
000075         10  P-CID-GL-LAST-FIELD
000076                                 PIC X(30).
000077         10  P-CID-GL-MENU-OPTION
000078                                 PIC X(30).
000079         10  P-CID-GL-SWITCH-SW
000080                                 PIC X.
000081         10  P-CID-GL-SIZE-SW
000082                                 PIC X.
000083         10  P-CID-GL-MOUSE-SW
000084                                 PIC X.
000085         10  P-CID-GL-CAPTURE-SW
000086                                 PIC X.
000087         10  P-CID-GL-WAIT-SW
000088                                 PIC X.
000089         10  P-CID-GL-CURS-SW
000090                                 PIC X.
000091******** P-CID-GL-OP-NUM-DATA ********
000092         10  P-CID-GL-PAN-POS-SW
000093                                 PIC S9(4) COMP-5.
000094         10  P-CID-GL-PAN-ROW
000095                                 PIC S9(4) COMP-5.
000096         10  P-CID-GL-PAN-COL
000097                                 PIC S9(4) COMP-5.
000098******** P-CID-GL-OP-CHAR-DATA ********
000099         10  P-CID-GL-NEW-WINDOW
000100                                 PIC X.
000101         10  P-CID-GL-CUR-COLR-SW
000102                                 PIC X.
000103******** P-CID-GL-OP-VAR-DATA ********
000104     05  P-CID-GL-FIELDS.
000105         10  P-CID-GL-VER-REL
000106                                 PIC X(0100).
000107     05  P-CID-GL-COLRS.
000108         10  P-CID-GL-FIELD-ID-73-C
000109                                 PIC X.
000110         10  P-CID-GL-VER-REL-C
000111                                 PIC X.
000112     05  P-CID-GL-TYPES.
000113         10  P-CID-GL-FIELD-ID-73-T
000114                                 PIC X.
000115         10  P-CID-GL-VER-REL-T
000116                                 PIC X.
000117************************************************
000118* field ids - use for cursor positioning, etc. *
000119************************************************
000120 01  P-CID-GL-IDS.
000121         05  P-CID-GL-FIELD-ID-73-I
000122                             PIC S9(4) COMP-5 VALUE +73.
000123         05  P-CID-GL-VER-REL-I
000124                             PIC S9(4) COMP-5 VALUE +72.
000125
      *<<((file: P-CID-GL.CPY))
000403*
000404*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ****************************************************************
      *                                                               
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        
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
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
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
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA       PIC X(01).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'CID' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000406* -====-
000407 0001-INITIALIZATION-PROC.
000408     ACCEPT SYS-DATE FROM DATE.
000409*
000410* PARAMETER-LINE contains the actual command tail
000411* (i.e. The "SWITCHES".)  PARAMETER-SIZE contains the number of
000412* characters in the command line tail
000413*
000414* You may want to use:
000415* the FUNCTION UPPER-CASE(PARAMETER-LINE) to eliminate the problem
000416* of usres mixing upper and lower case.
000417*
000418* The command line tail is entered after the .EXE in the
000419* shortcut's command line.  (i.e. ANNUITY.EXE HOME OFFICE).  The
000420* command line can be virtually anything you want up to 121
000421* characters in length.
000422*
000423* The delimiter in the command line will be a double space
000424* (i.e HOME OFFICE  IN FORCE)
000425*
000426*     CALL 'REALIA_GET_COMMAND_TAIL' USING PARAMETER-LINE
000427*            GIVING PARAMETER-SIZE.
000428     move 'HOME OFFICE' to parameter-line
000429*     MOVE FUNCTION UPPER-CASE(PARAMETER-LINE) TO PARAMETER-LINE.
000430*
000431      INSPECT PARAMETER-LINE REPLACING ALL LOW-VALUE BY SPACES.
000432*
000433      INITIALIZE PARAMETER-ARRAY.
000434*
000435     UNSTRING PARAMETER-LINE DELIMITED BY '  '
000436         INTO
000437         PARAMETER-SWITCH(1)
000438         PARAMETER-SWITCH(2)
000439         PARAMETER-SWITCH(3)
000440         PARAMETER-SWITCH(4)
000441         PARAMETER-SWITCH(5)
000442         PARAMETER-SWITCH(6)
000443         END-UNSTRING.
000444*
000445* set flag switches
000446*
000447     INITIALIZE SWITCH-FLAGS.
000448*
000449     PERFORM
000450           VARYING CNT-1 FROM 1 BY 1
000451           UNTIL CNT-1 > 6
000452       EVALUATE PARAMETER-SWITCH(CNT-1)
000453         WHEN 'HOME OFFICE'
000454           MOVE 'Y' TO HOME-OFFICE-FLAG
000455         WHEN 'F AND I ONLINE'
000456           MOVE 'Y' TO F-I-ONLINE-FLAG
000457       END-EVALUATE
000458     END-PERFORM.
000459*
000460* f & i online
000461*
000462     IF F-I-ONLINE-FLAG = 'Y'
000463       MOVE 'Qc.dll' TO MAIN-CTRL-PRODUCT-DLL
000464*
000465       INITIALIZE PASSED-DATA
000466*
000467       CALL MAIN-CTRL-PRODUCT-DLL USING PASSED-DATA
000468*
000469* cleanup - delete tempory agent file
000470*
000471       MOVE LOW-VALUE TO AGENT-FILE-PATH
000472       STRING
000473           'Tempory Agent.DAT[N]'
000474           DELIMITED BY SIZE
000475           INTO AGENT-FILE-PATH
000476     END-STRING
000477*
000478     DELETE FILE AGENT-FILE
000479*
000480     
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CID' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
000481     END-IF.
000482*
000483     OPEN INPUT LAST-STATE-FILE.
000484     MOVE SPACES TO LAST-STATE-RECORD.
000485     READ LAST-STATE-FILE.
000486     CLOSE LAST-STATE-FILE.
000487*
000488* get agent data
000489*
000490     MOVE LOW-VALUE TO AGENT-FILE-PATH.
000491     STRING
000492         'AGENT-1.DAT[N]'
000493         DELIMITED BY SIZE
000494          INTO AGENT-FILE-PATH
000495     END-STRING.
000496*
000497     OPEN INPUT AGENT-FILE.
000498     INITIALIZE AGENT-RECORD.
000499*
000500     READ AGENT-FILE.
000501*
000502     IF AGENT-FILE-ERR NOT = '00' AND '9A' AND '02'
000503       MOVE '<<Enter Company Name>>' TO AGENT-COMPANY
000504       MOVE '<<Enter Company Address>>' TO AGENT-ADDRESS
000505       MOVE '<<Enter  City, State and Zip>>' TO AGENT-STATE
000506       MOVE 9999999999 TO AGENT-PHONE
000507       MOVE 0 TO AGENT-COLOR-TRIGGER
000508       CLOSE AGENT-FILE
000509       OPEN OUTPUT AGENT-FILE
000510       WRITE AGENT-RECORD
000511       CLOSE AGENT-FILE
000512     ELSE
000513       CLOSE AGENT-FILE
000514     END-IF.
000515*
000516     IF AGENT-COMPANY = '<<Enter Company Name>>'
000517       MOVE LOW-VALUES TO SP2-LG-MS-DATA
000518       MOVE 'Information' TO SP2-LG-MS-TITLE
000519       MOVE 1 TO SP2-LG-MS-LINE-CNT
000520       MOVE 'i' TO SP2-LG-MS-ICON
000521       MOVE 'o' TO SP2-LG-MS-BUTTON
000522       MOVE SPACES TO SP2-LG-MS-TEXT
000523       STRING
000524           'CSO Credit Insurance Division'
000525           CARRIAGE-RETURN
000526           CARRIAGE-RETURN
000527           'Initial user data must be entered.'
000528           DELIMITED BY SIZE
000529           INTO SP2-LG-MS-TEXT
000530     END-STRING
000531*    CALL 'SP2' USING
000532*        SP2-DISPLAY-MESSAGE SP2-LG-MESSAGE-DATA
000533     CALL AGENT-DLL
000534     END-IF.
000535*
000536* you must use this sp2 intialization, if you are loading up
000537* comboboxes or changing icon within this program
000538* starts here
000539*
000540* set main file & panel
000541*
000542     MOVE LOW-VALUES TO SP2-FI-DATA.
000543     MOVE "CID.PAN" TO SP2-FI-NAME.
000544*    CALL "SP2" USING SP2-OPEN-FILE SP2-FILE-DEF.
000545*
000546     MOVE LOW-VALUES TO MAIN-DATA.
000547     MOVE "MAIN" TO MAIN-NEXT-PANEL.
000548     MOVE "y" TO MAIN-NEW-WINDOW.
000549     MOVE LOW-VALUE TO
000550         MAIN-FIELDS
000551         MAIN-COLRS
000552         MAIN-TYPES.
000553*
000554* open panel but do not display it
000555*
000556     MOVE LOW-VALUE TO SP2-WD-DATA.
000557     MOVE "MAIN" TO SP2-WD-PANEL-NAME.
000558*    CALL "SP2" USING SP2-OPEN-WINDOW SP2-WINDOW-DEF.
000559     MOVE LOW-VALUE TO MAIN-NEW-WINDOW.
000560*
000561     MOVE MAIN-STATE-I TO MAIN-NEXT-FLD-ID.
000562*
000563* ends here
000564*
000565     MOVE 260 TO BUFFER-LENGTH.
000566
000567***  Don't really need current directory. Will figure out where
000568***   to store what ever is needed here!!!
000569
000570*    CALL 'REALIA_GET_CURRENT_DIRECTORY' USING
000571*        CURRENT-DIRECTORY-NAME BUFFER-LENGTH
000572*           GIVING STATUS-CODE.
000573*
000574     MOVE 'N' TO STATE-FLAG.
000575*
000576* load up combo boxes
000577*
000578     PERFORM 0005-LOAD-STATE-PROC.
000579*
000580     PERFORM 0009-LOAD-PRODUCT-PROC.
000581*
000582     PERFORM 0006-LOAD-LIMIT-FILE-PROC.
000583*
000584     PERFORM 0010-LOAD-RATE-FILE-DESCR-PROC.
000585*
000586     PERFORM 0011-LOAD-AVAILABLE-TYPES.
000587*
000588     MOVE SPACES TO LAST-STATE-RECORD.
000589* -====-
000590 0002-MAIN-WINDOW-CONTROL.
000591     IF MAIN-CONTINUE-HIT
000592       OPEN INPUT MAIN-CTRL-FILE
000593       INITIALIZE MAIN-CTRL-RECORD
000594       MOVE MAIN-STATE TO MAIN-CTRL-STATE
000595       MOVE MAIN-PRODUCT TO MAIN-CTRL-PRODUCT
000596       READ MAIN-CTRL-FILE
000597           INVALID KEY
000598         CONTINUE
000599       END-READ
000600       CLOSE MAIN-CTRL-FILE
000601*
000602       IF MAIN-RATE-FILE-DESCR-T = 'h' AND
000603             MAIN-CTRL-RATE-FILE-REQUIRED = 'Y'
000604         MOVE LOW-VALUES TO SP2-LG-MS-DATA
000605         MOVE 'Input Error' TO SP2-LG-MS-TITLE
000606         MOVE 1 TO SP2-LG-MS-LINE-CNT
000607         MOVE 'b' TO SP2-LG-MS-ICON
000608         MOVE 'o' TO SP2-LG-MS-BUTTON
000609         MOVE SPACES TO SP2-LG-MS-TEXT
000610         MOVE 0 TO
000611             MAIN-KEY
000612             MAIN-MENU-ID
000613             MAIN-LAST-FLD-ID
000614         STRING
000615             'The rate file is not available, please '
000616             'select different software or state.'
000617             DELIMITED BY SIZE
000618             INTO SP2-LG-MS-TEXT
000619       END-STRING
000620       CALL 'SP2' USING SP2-DISPLAY-MESSAGE SP2-LG-MESSAGE-DATA
000621       MOVE MAIN-PRODUCT-I TO MAIN-NEXT-FLD-ID
000622       GO TO 0002-MAIN-WINDOW-CONTROL
000623       ELSE
000624         IF MAIN-PRODUCT > SPACES
000625           PERFORM 0015-SET-HOEPA-TRIGGER-PROC
000626           PERFORM 0014-CALL-DLL-PROC
000627         END-IF
000628       END-IF
000629     END-IF.
000630*
000631* when a toolbar action was selected
000632*
000633     IF MAIN-KEY = SP2-KEY-TOOLBAR
000634       EVALUATE MAIN-MENU-ID
000635* run software
000636         WHEN 510
000637           OPEN INPUT MAIN-CTRL-FILE
000638           INITIALIZE MAIN-CTRL-RECORD
000639           MOVE MAIN-STATE TO MAIN-CTRL-STATE
000640           MOVE MAIN-PRODUCT TO MAIN-CTRL-PRODUCT
000641           READ MAIN-CTRL-FILE
000642               INVALID KEY
000643             CONTINUE
000644           END-READ
000645           CLOSE MAIN-CTRL-FILE
000646*
000647           IF MAIN-RATE-FILE-DESCR-T = 'h' AND
000648                 MAIN-CTRL-RATE-FILE-REQUIRED = 'Y'
000649             MOVE LOW-VALUES TO SP2-LG-MS-DATA
000650             MOVE 'Input Error' TO SP2-LG-MS-TITLE
000651             MOVE 1 TO SP2-LG-MS-LINE-CNT
000652             MOVE 'b' TO SP2-LG-MS-ICON
000653             MOVE 'o' TO SP2-LG-MS-BUTTON
000654             MOVE SPACES TO SP2-LG-MS-TEXT
000655             MOVE 0 TO
000656                 MAIN-KEY
000657                 MAIN-MENU-ID
000658                 MAIN-LAST-FLD-ID
000659             STRING
000660                 'The rate file is not available, please '
000661                 'select different software or state.'
000662                 DELIMITED BY SIZE
000663                 INTO SP2-LG-MS-TEXT
000664           END-STRING
000665           CALL 'SP2' USING
000666               SP2-DISPLAY-MESSAGE SP2-LG-MESSAGE-DATA
000667           MOVE MAIN-PRODUCT-I TO MAIN-NEXT-FLD-ID
000668           GO TO 0002-MAIN-WINDOW-CONTROL
000669           ELSE
000670             IF MAIN-PRODUCT > SPACES
000671               PERFORM 0015-SET-HOEPA-TRIGGER-PROC
000672               PERFORM 0014-CALL-DLL-PROC
000673             END-IF
000674           END-IF
000675* agent file
000676         WHEN 508
000677           PERFORM 0015-SET-HOEPA-TRIGGER-PROC
000678           OPEN OUTPUT PROTECT-AGENT-FILE
000679           CLOSE PROTECT-AGENT-FILE
000680           CALL AGENT-DLL
000681* exit program
000682         WHEN 599
000683           GO TO 0003-EXIT-PROGRAM-PROC
000684       END-EVALUATE
000685     END-IF.
000686*
000687* menu items selections
000688*
000689     IF MAIN-KEY = -6
000690       EVALUATE MAIN-MENU-ID
000691* run software
000692         WHEN MENU-OPT-RUN
000693           OPEN INPUT MAIN-CTRL-FILE
000694           INITIALIZE MAIN-CTRL-RECORD
000695           MOVE MAIN-STATE TO MAIN-CTRL-STATE
000696           MOVE MAIN-PRODUCT TO MAIN-CTRL-PRODUCT
000697           READ MAIN-CTRL-FILE
000698               INVALID KEY
000699             CONTINUE
000700           END-READ
000701           CLOSE MAIN-CTRL-FILE
000702*
000703           IF MAIN-RATE-FILE-DESCR-T = 'h' AND
000704                 MAIN-CTRL-RATE-FILE-REQUIRED = 'Y'
000705             MOVE LOW-VALUES TO SP2-LG-MS-DATA
000706             MOVE 'Input Error' TO SP2-LG-MS-TITLE
000707             MOVE 1 TO SP2-LG-MS-LINE-CNT
000708             MOVE 'b' TO SP2-LG-MS-ICON
000709             MOVE 'o' TO SP2-LG-MS-BUTTON
000710             MOVE SPACES TO SP2-LG-MS-TEXT
000711             MOVE 0 TO
000712                 MAIN-KEY
000713                 MAIN-MENU-ID
000714                 MAIN-LAST-FLD-ID
000715             STRING
000716                 'The rate file is not available, please '
000717                 'select different software or state.'
000718                 DELIMITED BY SIZE
000719                 INTO SP2-LG-MS-TEXT
000720           END-STRING
000721           CALL 'SP2' USING
000722               SP2-DISPLAY-MESSAGE SP2-LG-MESSAGE-DATA
000723           MOVE MAIN-PRODUCT-I TO MAIN-NEXT-FLD-ID
000724           GO TO 0002-MAIN-WINDOW-CONTROL
000725           ELSE
000726             IF MAIN-PRODUCT > SPACES
000727               PERFORM 0015-SET-HOEPA-TRIGGER-PROC
000728               PERFORM 0014-CALL-DLL-PROC
000729             END-IF
000730           END-IF
000731* agent file
000732         WHEN MENU-OPT-AGENT
000733           OPEN OUTPUT PROTECT-AGENT-FILE
000734           CLOSE PROTECT-AGENT-FILE
000735           CALL AGENT-DLL
000736* glossary of terms
000737         WHEN MENU-OPT-GLOSSARY
000738           PERFORM 0013-PREVIEW-GLOSSARY-PROC
000739* version information
000740         WHEN MENU-OPT-ABOUT
000741           PERFORM 0004-ABOUT-PROC
000742* exit program
000743         WHEN MENU-OPT-EXIT
000744           GO TO 0003-EXIT-PROGRAM-PROC
000745       END-EVALUATE
000746     END-IF.
000747*
000748* 1 of 3 different exit program points
000749*
000750     IF MAIN-KEY = -5
000751       GO TO 0003-EXIT-PROGRAM-PROC
000752     END-IF.
000753*
000754* screen and product specific checks
000755*
000756     EVALUATE MAIN-LAST-FLD-ID
000757* state
000758       WHEN MAIN-STATE-I
000759         PERFORM 0009-LOAD-PRODUCT-PROC
000760         PERFORM 0006-LOAD-LIMIT-FILE-PROC
000761         PERFORM 0010-LOAD-RATE-FILE-DESCR-PROC
000762         PERFORM 0011-LOAD-AVAILABLE-TYPES
000763* product
000764       WHEN MAIN-PRODUCT-I
000765         PERFORM 0006-LOAD-LIMIT-FILE-PROC
000766         PERFORM 0010-LOAD-RATE-FILE-DESCR-PROC
000767         PERFORM 0011-LOAD-AVAILABLE-TYPES
000768* rate file
000769       WHEN MAIN-RATE-FILE-DESCR-I
000770         PERFORM 0011-LOAD-AVAILABLE-TYPES
000771       END-EVALUATE.
000772*
000773*    CALL "SP2" USING SP2-CONVERSE-PANEL MAIN-CONVERSE-DATA.
000774     MOVE LOW-VALUE TO MAIN-NEW-WINDOW.
000775*
000776     GO TO 0002-MAIN-WINDOW-CONTROL.
000777* -====-
000778 0003-EXIT-PROGRAM-PROC.
000779     OPEN OUTPUT LAST-STATE-FILE.
000780     MOVE MAIN-PRODUCT TO LAST-PRODUCT.
000781     MOVE MAIN-STATE TO LAST-STATE.
000782     WRITE LAST-STATE-RECORD.
000783     CLOSE LAST-STATE-FILE.
000784*
000785* cleanup - delete tempory agent file
000786*
000787     MOVE LOW-VALUE TO AGENT-FILE-PATH.
000788     STRING
000789         'Tempory Agent.DAT[N]'
000790         DELIMITED BY SIZE
000791          INTO AGENT-FILE-PATH
000792     END-STRING.
000793*
000794     DELETE FILE AGENT-FILE.
000795*
000796*    CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM.
000797*
000798*    CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM.
000799*
000800*    CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM.
000801*
000802     STOP RUN.
000803* -====-
000804 0004-ABOUT-PROC.
000805     MOVE LOW-VALUES TO SP2-LG-MS-DATA.
000806     MOVE 'About' TO SP2-LG-MS-TITLE.
000807     MOVE 1 TO SP2-LG-MS-LINE-CNT.
000808     MOVE 'i' TO SP2-LG-MS-ICON.
000809     MOVE 'o' TO SP2-LG-MS-BUTTON.
000810     MOVE SPACES TO SP2-LG-MS-TEXT.
000811     STRING
000812         'CSO Credit Insurance Division - Version '
000813         VERSION-NO
000814         ', Release ' RELEASE-DATE
000815         CARRIAGE-RETURN
000816         CARRIAGE-RETURN
000817*
000818         'QuikCalc - Standard: '
000819         'Version 2.50, Release 04/05/2021'
000820         CARRIAGE-RETURN
000821         CARRIAGE-RETURN
000822*
000823         'QuikCalc - Extended Term: '
000824         'Version 1.30, Release 09/28/2010'
000825         CARRIAGE-RETURN
000826         CARRIAGE-RETURN
000827*
000828         'QuikCalc - Prima Facie: '
000829         'Version 1.29, Release 09/28/2010'
000830         CARRIAGE-RETURN
000831         CARRIAGE-RETURN
000832*
000833         'QuikCalc - Refund: '
000834         'Version 2.00, Release 01/02/2016'
000835         CARRIAGE-RETURN
000836         CARRIAGE-RETURN
000837*
000838         'QuikCalc - Extended Term Refund: '
000839         'Version 1.78, Release 09/28/2010'
000840         CARRIAGE-RETURN
000841         CARRIAGE-RETURN
000842*
000843         'QuikApp: '
000844         'Version 1.16, Release 09/28/2010'
000845         CARRIAGE-RETURN
000846         CARRIAGE-RETURN
000847*
000848         'QuikApp - Refund: '
000849         'Version 1.14, Release 09/28/2010'
000850         CARRIAGE-RETURN
000851         CARRIAGE-RETURN
000852*
000853         'SecureLife: '
000854         'Version 1.20, Release 09/28/2010'
000855         CARRIAGE-RETURN
000856*
000857         DELIMITED BY SIZE
000858          INTO SP2-LG-MS-TEXT.
000859     CALL 'SP2' USING SP2-DISPLAY-MESSAGE SP2-LG-MESSAGE-DATA.
000860* -====-
000861 0005-LOAD-STATE-PROC.
000862     OPEN INPUT MAIN-CTRL-FILE.
000863*
000864     MOVE LOW-VALUE TO SETVAL-DATA.
000865*
000866     INITIALIZE MAIN-CTRL-KEY.
000867*
000868     START MAIN-CTRL-FILE KEY NOT < MAIN-CTRL-KEY
000869         NOT
000870         INVALID KEY
000871       READ MAIN-CTRL-FILE
000872       NEXT RECORD
000873     END-READ
000874     END-START.
000875*
000876     IF MAIN-CTRL-STATE = LAST-STATE
000877           AND LAST-STATE NOT = SPACES
000878       MOVE 'Y' TO FLAG-1
000879     ELSE
000880       MOVE 'N' TO FLAG-1
000881     END-IF.
000882*
000883     MOVE MAIN-CTRL-STATE TO
000884         MAIN-STATE
000885         RANGE-20(1)
000886         RANGE-20(2).
000887*
000888     MOVE 3 TO CNT-1.
000889*
000890     READ MAIN-CTRL-FILE
000891     NEXT RECORD
000892     END-READ.
000893*
000894     PERFORM
000895           UNTIL MAIN-CTRL-FILE-ERR NOT = '00' AND '02' AND '9A'
000896       SUBTRACT 1 FROM CNT-1 GIVING CNT-2
000897       IF RANGE-20(CNT-2) NOT = MAIN-CTRL-STATE
000898         MOVE MAIN-CTRL-STATE TO RANGE-20(CNT-1)
000899*
000900         IF MAIN-CTRL-STATE = LAST-STATE
000901               AND LAST-STATE NOT = SPACES
000902           MOVE 'Y' TO FLAG-1
000903         END-IF
000904*
000905         ADD 1 TO CNT-1
000906       END-IF
000907*
000908       READ MAIN-CTRL-FILE
000909       NEXT RECORD
000910     END-READ
000911     END-PERFORM.
000912*
000913     MOVE MAIN-STATE-I TO SETVAL-ID.
000914*
000915     PERFORM 0012-SETUP-COMBO-BOX-PROC.
000916*
000917     CLOSE MAIN-CTRL-FILE.
000918*
000919     IF FLAG-1 = 'Y'
000920       MOVE LAST-STATE TO MAIN-STATE
000921     END-IF.
000922* -====-
000923 0006-LOAD-LIMIT-FILE-PROC.
000924     EVALUATE MAIN-PRODUCT
000925*
000926* check for quikcalc limit files
000927*
000928       WHEN 'QuikCalc - Standard'
000929         PERFORM 0007-LOAD-QC-LIMIT-FILE
000930*
000931* check for quikcalc extended term limit files
000932*
000933       WHEN 'QuikCalc - Extended Term'
000934         PERFORM 0008-LOAD-QC-ET-LIMIT-FILE
000935*
000936* limit files are not available
000937*
000938       WHEN OTHER
000939         MOVE LOW-VALUE TO SETVAL-DATA
000940         MOVE MAIN-LIMIT-FILE-I TO SETVAL-ID
000941         PERFORM 0012-SETUP-COMBO-BOX-PROC
000942         MOVE SPACES TO
000943             MAIN-LIMIT-FILE-TEXT
000944             MAIN-LIMIT-FILE
000945         MOVE 'h' TO MAIN-LIMIT-FILE-T
000946       END-EVALUATE.
000947* -====-
000948 0007-LOAD-QC-LIMIT-FILE.
000949     MOVE LOW-VALUE TO SETVAL-DATA.
000950*
000951     EVALUATE MAIN-STATE
000952       WHEN 'Alaska'
000953         MOVE 'AK' TO STATE-ABBR
000954       WHEN 'Alabama'
000955         MOVE 'AL' TO STATE-ABBR
000956       WHEN 'Arkansas'
000957         MOVE 'AR' TO STATE-ABBR
000958       WHEN 'Arizona'
000959         MOVE 'AZ' TO STATE-ABBR
000960       WHEN 'California'
000961         MOVE 'CA' TO STATE-ABBR
000962       WHEN 'Colorado'
000963         MOVE 'CO' TO STATE-ABBR
000964       WHEN 'Connecticut'
000965         MOVE 'CT' TO STATE-ABBR
000966       WHEN 'District of Columbia'
000967         MOVE 'DC' TO STATE-ABBR
000968       WHEN 'Delaware'
000969         MOVE 'DE' TO STATE-ABBR
000970       WHEN 'Florida'
000971         MOVE 'FL' TO STATE-ABBR
000972       WHEN 'Georgia'
000973         MOVE 'GA' TO STATE-ABBR
000974       WHEN 'Hawaii'
000975         MOVE 'HI' TO STATE-ABBR
000976       WHEN 'Iowa'
000977         MOVE 'IA' TO STATE-ABBR
000978       WHEN 'Idaho'
000979         MOVE 'ID' TO STATE-ABBR
000980       WHEN 'Illinois'
000981         MOVE 'IL' TO STATE-ABBR
000982       WHEN 'Indiana'
000983         MOVE 'IN' TO STATE-ABBR
000984       WHEN 'Kansas'
000985         MOVE 'KS' TO STATE-ABBR
000986       WHEN 'Kentucky'
000987         MOVE 'KY' TO STATE-ABBR
000988       WHEN 'Louisiana'
000989         MOVE 'LA' TO STATE-ABBR
000990       WHEN 'Massachusetts'
000991         MOVE 'MA' TO STATE-ABBR
000992       WHEN 'Maryland'
000993         MOVE 'MD' TO STATE-ABBR
000994       WHEN 'Maine'
000995         MOVE 'ME' TO STATE-ABBR
000996       WHEN 'Michigan'
000997         MOVE 'MI' TO STATE-ABBR
000998       WHEN 'Minnesota'
000999         MOVE 'MN' TO STATE-ABBR
001000       WHEN 'Missouri'
001001         MOVE 'MO' TO STATE-ABBR
001002       WHEN 'Mississippi'
001003         MOVE 'MS' TO STATE-ABBR
001004       WHEN 'Montana'
001005         MOVE 'MT' TO STATE-ABBR
001006       WHEN 'North Carolina'
001007         MOVE 'NC' TO STATE-ABBR
001008       WHEN 'North Dakota'
001009         MOVE 'ND' TO STATE-ABBR
001010       WHEN 'Nebraska'
001011         MOVE 'NE' TO STATE-ABBR
001012       WHEN 'New Hampshire'
001013         MOVE 'NH' TO STATE-ABBR
001014       WHEN 'New Jersey'
001015         MOVE 'NJ' TO STATE-ABBR
001016       WHEN 'New Mexico'
001017         MOVE 'NM' TO STATE-ABBR
001018       WHEN 'Nevada'
001019         MOVE 'NV' TO STATE-ABBR
001020       WHEN 'New York'
001021         MOVE 'NY' TO STATE-ABBR
001022       WHEN 'Ohio'
001023         MOVE 'OH' TO STATE-ABBR
001024       WHEN 'Oklahoma'
001025         MOVE 'OK' TO STATE-ABBR
001026       WHEN 'Oregon'
001027         MOVE 'OR' TO STATE-ABBR
001028       WHEN 'Pennsylvania'
001029         MOVE 'PA' TO STATE-ABBR
001030       WHEN 'Rhode Island'
001031         MOVE 'RI' TO STATE-ABBR
001032       WHEN 'South Carolina'
001033         MOVE 'SC' TO STATE-ABBR
001034       WHEN 'South Dakota'
001035         MOVE 'SD' TO STATE-ABBR
001036       WHEN 'Tennessee'
001037         MOVE 'TN' TO STATE-ABBR
001038       WHEN 'Texas'
001039         MOVE 'TX' TO STATE-ABBR
001040       WHEN 'Utah'
001041         MOVE 'UT' TO STATE-ABBR
001042       WHEN 'Virginia'
001043         MOVE 'VA' TO STATE-ABBR
001044       WHEN 'Vermont'
001045         MOVE 'VT' TO STATE-ABBR
001046       WHEN 'Washington'
001047         MOVE 'WA' TO STATE-ABBR
001048       WHEN 'Wisconsin'
001049         MOVE 'WI' TO STATE-ABBR
001050       WHEN 'West Virginia'
001051         MOVE 'WV' TO STATE-ABBR
001052       WHEN 'Wyoming'
001053         MOVE 'WY' TO STATE-ABBR
001054       END-EVALUATE.
001055*
001056* search for quikcalc limit files
001057*
001058     MOVE CURRENT-DIRECTORY-NAME TO SEARCHFILENAME.
001059*
001060     ADD 1 BUFFER-LENGTH GIVING CNT-3.
001061*
001062     STRING
001063         '\DATA\'
001064         '*.LMT'
001065         DELIMITED BY SIZE
001066          INTO SEARCHFILENAME WITH POINTER CNT-3
001067     END-STRING.
001068*
001069*    CALL 'REALIA_FINDFIRST'
001070*          USING BY REFERENCE SEARCH-HANDLE
001071*        BY REFERENCE SEARCHFILENAME
001072*        BY REFERENCE COBOLFILESEARCH
001073*           GIVING SEARCHRESULT
001074*    END-CALL.
001075*
001076     MOVE 2 TO CNT-2.
001077*
001078     PERFORM
001079           UNTIL CNT-2 > 71 OR NOT SEARCH-OK
001080*
001081       MOVE FULLFILENAME TO PARSE-FULLFILENAME
001082*
001083* read file and check for correct version number
001084*
001085       MOVE CURRENT-DIRECTORY-NAME TO LIMIT-FILE-PATH
001086*
001087       ADD 1 BUFFER-LENGTH GIVING CNT-3
001088*
001089       STRING
001090           '\DATA\'
001091           DELIMITED BY SIZE
001092           FULLFILENAME
001093           DELIMITED BY '  '
001094           LOW-VALUE
001095           DELIMITED BY SIZE
001096           INTO LIMIT-FILE-PATH WITH POINTER CNT-3
001097     END-STRING
001098     INITIALIZE LIMIT-RECORD
001099     OPEN INPUT LIMIT-FILE
001100     READ LIMIT-FILE
001101     CLOSE LIMIT-FILE
001102*
001103     IF PARSE-FULLFILENAME = STATE-ABBR AND
001104           LIMIT-ID-RECORD = LIMIT-SOFTWARE-ID
001105       MOVE FUNCTION UPPER-CASE(FULLFILENAME) TO
001106           FULLFILENAME
001107       MOVE SPACES TO RANGE-60(CNT-2)
001108       STRING
001109           FULLFILENAME
001110           DELIMITED BY '.LMT'
001111           INTO RANGE-60(CNT-2)
001112     END-STRING
001113     ADD 1 TO CNT-2
001114     END-IF
001115*
001116*    CALL 'REALIA_FINDNEXT'
001117*          USING BY REFERENCE SEARCH-HANDLE
001118*        BY REFERENCE COBOLFILESEARCH
001119*           GIVING SEARCHRESULT
001120*    END-CALL
001121     END-PERFORM.
001122*
001123     MOVE RANGE-60(2) TO RANGE-60(1).
001124*
001125     MOVE MAIN-LIMIT-FILE-I TO SETVAL-ID.
001126*
001127     PERFORM 0012-SETUP-COMBO-BOX-PROC.
001128*
001129     MOVE 'N' TO FLAG-1.
001130*
001131     PERFORM
001132           VARYING CNT-1 FROM 1 BY 1
001133           UNTIL CNT-1 > CNT-2
001134       IF MAIN-LIMIT-FILE = RANGE-40(CNT-1)
001135         MOVE 'Y' TO FLAG-1
001136       END-IF
001137     END-PERFORM.
001138*
001139     IF FLAG-1 = 'N'
001140       MOVE RANGE-60(1) TO MAIN-LIMIT-FILE
001141     END-IF.
001142*
001143* check to see if quikcalc limit files available
001144*
001145     IF CNT-2 > 2
001146       MOVE 'Limit Option:' TO MAIN-LIMIT-FILE-TEXT
001147       MOVE LOW-VALUE TO MAIN-LIMIT-FILE-T
001148     ELSE
001149       MOVE SPACES TO
001150           MAIN-LIMIT-FILE-TEXT
001151           MAIN-LIMIT-FILE
001152       MOVE 'h' TO MAIN-LIMIT-FILE-T
001153     END-IF.
001154* -====-
001155 0008-LOAD-QC-ET-LIMIT-FILE.
001156     MOVE LOW-VALUE TO SETVAL-DATA.
001157*
001158     EVALUATE MAIN-STATE
001159       WHEN 'Alaska'
001160         MOVE 'AK' TO STATE-ABBR
001161       WHEN 'Alabama'
001162         MOVE 'AL' TO STATE-ABBR
001163       WHEN 'Arkansas'
001164         MOVE 'AR' TO STATE-ABBR
001165       WHEN 'Arizona'
001166         MOVE 'AZ' TO STATE-ABBR
001167       WHEN 'California'
001168         MOVE 'CA' TO STATE-ABBR
001169       WHEN 'Colorado'
001170         MOVE 'CO' TO STATE-ABBR
001171       WHEN 'Connecticut'
001172         MOVE 'CT' TO STATE-ABBR
001173       WHEN 'District of Columbia'
001174         MOVE 'DC' TO STATE-ABBR
001175       WHEN 'Delaware'
001176         MOVE 'DE' TO STATE-ABBR
001177       WHEN 'Florida'
001178         MOVE 'FL' TO STATE-ABBR
001179       WHEN 'Georgia'
001180         MOVE 'GA' TO STATE-ABBR
001181       WHEN 'Hawaii'
001182         MOVE 'HI' TO STATE-ABBR
001183       WHEN 'Iowa'
001184         MOVE 'IA' TO STATE-ABBR
001185       WHEN 'Idaho'
001186         MOVE 'ID' TO STATE-ABBR
001187       WHEN 'Illinois'
001188         MOVE 'IL' TO STATE-ABBR
001189       WHEN 'Indiana'
001190         MOVE 'IN' TO STATE-ABBR
001191       WHEN 'Kansas'
001192         MOVE 'KS' TO STATE-ABBR
001193       WHEN 'Kentucky'
001194         MOVE 'KY' TO STATE-ABBR
001195       WHEN 'Louisiana'
001196         MOVE 'LA' TO STATE-ABBR
001197       WHEN 'Massachusetts'
001198         MOVE 'MA' TO STATE-ABBR
001199       WHEN 'Maryland'
001200         MOVE 'MD' TO STATE-ABBR
001201       WHEN 'Maine'
001202         MOVE 'ME' TO STATE-ABBR
001203       WHEN 'Michigan'
001204         MOVE 'MI' TO STATE-ABBR
001205       WHEN 'Minnesota'
001206         MOVE 'MN' TO STATE-ABBR
001207       WHEN 'Missouri'
001208         MOVE 'MO' TO STATE-ABBR
001209       WHEN 'Mississippi'
001210         MOVE 'MS' TO STATE-ABBR
001211       WHEN 'Montana'
001212         MOVE 'MT' TO STATE-ABBR
001213       WHEN 'North Carolina'
001214         MOVE 'NC' TO STATE-ABBR
001215       WHEN 'North Dakota'
001216         MOVE 'ND' TO STATE-ABBR
001217       WHEN 'Nebraska'
001218         MOVE 'NE' TO STATE-ABBR
001219       WHEN 'New Hampshire'
001220         MOVE 'NH' TO STATE-ABBR
001221       WHEN 'New Jersey'
001222         MOVE 'NJ' TO STATE-ABBR
001223       WHEN 'New Mexico'
001224         MOVE 'NM' TO STATE-ABBR
001225       WHEN 'Nevada'
001226         MOVE 'NV' TO STATE-ABBR
001227       WHEN 'New York'
001228         MOVE 'NY' TO STATE-ABBR
001229       WHEN 'Ohio'
001230         MOVE 'OH' TO STATE-ABBR
001231       WHEN 'Oklahoma'
001232         MOVE 'OK' TO STATE-ABBR
001233       WHEN 'Oregon'
001234         MOVE 'OR' TO STATE-ABBR
001235       WHEN 'Pennsylvania'
001236         MOVE 'PA' TO STATE-ABBR
001237       WHEN 'Rhode Island'
001238         MOVE 'RI' TO STATE-ABBR
001239       WHEN 'South Carolina'
001240         MOVE 'SC' TO STATE-ABBR
001241       WHEN 'South Dakota'
001242         MOVE 'SD' TO STATE-ABBR
001243       WHEN 'Tennessee'
001244         MOVE 'TN' TO STATE-ABBR
001245       WHEN 'Texas'
001246         MOVE 'TX' TO STATE-ABBR
001247       WHEN 'Utah'
001248         MOVE 'UT' TO STATE-ABBR
001249       WHEN 'Virginia'
001250         MOVE 'VA' TO STATE-ABBR
001251       WHEN 'Vermont'
001252         MOVE 'VT' TO STATE-ABBR
001253       WHEN 'Washington'
001254         MOVE 'WA' TO STATE-ABBR
001255       WHEN 'Wisconsin'
001256         MOVE 'WI' TO STATE-ABBR
001257       WHEN 'West Virginia'
001258         MOVE 'WV' TO STATE-ABBR
001259       WHEN 'Wyoming'
001260         MOVE 'WY' TO STATE-ABBR
001261       END-EVALUATE.
001262*
001263* search for quikcalc extended term limit files
001264*
001265     MOVE CURRENT-DIRECTORY-NAME TO SEARCHFILENAME.
001266*
001267     ADD 1 BUFFER-LENGTH GIVING CNT-3.
001268*
001269     STRING
001270         '\DATA\'
001271         '*.LIM'
001272         DELIMITED BY SIZE
001273          INTO SEARCHFILENAME WITH POINTER CNT-3
001274     END-STRING.
001275*
001276*    CALL 'REALIA_FINDFIRST'
001277*          USING BY REFERENCE SEARCH-HANDLE
001278*        BY REFERENCE SEARCHFILENAME
001279*        BY REFERENCE COBOLFILESEARCH
001280*           GIVING SEARCHRESULT
001281*    END-CALL.
001282*
001283     MOVE 2 TO CNT-2.
001284*
001285     PERFORM
001286           UNTIL CNT-2 > 71 OR NOT SEARCH-OK
001287*
001288       MOVE FULLFILENAME TO PARSE-FULLFILENAME
001289*
001290* read file and check for correct version number
001291*
001292       MOVE CURRENT-DIRECTORY-NAME TO ACCT-ET-FILE-PATH
001293*
001294       ADD 1 BUFFER-LENGTH GIVING CNT-3
001295*
001296       STRING
001297           '\DATA\'
001298           DELIMITED BY SIZE
001299           FULLFILENAME
001300           DELIMITED BY '  '
001301           LOW-VALUE
001302           DELIMITED BY SIZE
001303           INTO ACCT-ET-FILE-PATH WITH POINTER CNT-3
001304     END-STRING
001305     INITIALIZE ACCT-ET-RECORD
001306     OPEN INPUT ACCT-ET-FILE
001307     READ ACCT-ET-FILE
001308     CLOSE ACCT-ET-FILE
001309*
001310     IF PARSE-FULLFILENAME = STATE-ABBR AND
001311           ACCT-ET-ID = ACCT-ET-SOFTWARE-ID
001312       MOVE FUNCTION UPPER-CASE(FULLFILENAME) TO
001313           FULLFILENAME
001314       MOVE SPACES TO RANGE-40(CNT-2)
001315       STRING
001316           FULLFILENAME
001317           DELIMITED BY '.LIM'
001318           INTO RANGE-60(CNT-2)
001319     END-STRING
001320     ADD 1 TO CNT-2
001321     END-IF
001322*
001323*    CALL 'REALIA_FINDNEXT'
001324*          USING BY REFERENCE SEARCH-HANDLE
001325*        BY REFERENCE COBOLFILESEARCH
001326*           GIVING SEARCHRESULT
001327*    END-CALL
001328     END-PERFORM.
001329*
001330     MOVE RANGE-60(2) TO RANGE-60(1).
001331*
001332     MOVE MAIN-LIMIT-FILE-I TO SETVAL-ID.
001333*
001334     PERFORM 0012-SETUP-COMBO-BOX-PROC.
001335*
001336     MOVE 'N' TO FLAG-1.
001337*
001338     PERFORM
001339           VARYING CNT-1 FROM 1 BY 1
001340           UNTIL CNT-1 > CNT-2
001341       IF MAIN-LIMIT-FILE = RANGE-40(CNT-1)
001342         MOVE 'Y' TO FLAG-1
001343       END-IF
001344     END-PERFORM.
001345*
001346     IF FLAG-1 = 'N'
001347       MOVE RANGE-60(1) TO MAIN-LIMIT-FILE
001348     END-IF.
001349*
001350* check to see if quikcalc extended term limit files available
001351*
001352     IF CNT-2 > 2
001353       MOVE 'Limit Option:' TO MAIN-LIMIT-FILE-TEXT
001354       MOVE LOW-VALUE TO MAIN-LIMIT-FILE-T
001355     ELSE
001356       MOVE SPACES TO
001357           MAIN-LIMIT-FILE-TEXT
001358           MAIN-LIMIT-FILE
001359       MOVE 'h' TO MAIN-LIMIT-FILE-T
001360     END-IF.
001361* -====-
001362 0009-LOAD-PRODUCT-PROC.
001363     OPEN INPUT MAIN-CTRL-FILE.
001364*
001365     MOVE LOW-VALUE TO SETVAL-DATA.
001366*
001367     INITIALIZE MAIN-CTRL-KEY.
001368     MOVE MAIN-STATE TO MAIN-CTRL-STATE.
001369*
001370     START MAIN-CTRL-FILE KEY NOT < MAIN-CTRL-KEY
001371         NOT
001372         INVALID KEY
001373       READ MAIN-CTRL-FILE
001374       NEXT RECORD
001375     END-READ
001376     END-START.
001377*
001378     MOVE 2 TO CNT-1.
001379*
001380     MOVE 'N' TO
001381         FLAG-1
001382         FLAG-2.
001383*
001384     PERFORM
001385           UNTIL MAIN-STATE NOT = MAIN-CTRL-STATE OR
001386           (MAIN-CTRL-FILE-ERR NOT = '00' AND '02' AND '9A')
001387*
001388* search for dll's
001389*
001390       MOVE LOW-VALUE TO SEARCHFILENAME
001391       STRING
001392           MAIN-CTRL-PRODUCT-DLL
001393           DELIMITED BY '  '
001394           INTO SEARCHFILENAME
001395     END-STRING
001396*    CALL 'REALIA_FINDFIRST'
001397*          USING BY REFERENCE SEARCH-HANDLE
001398*        BY REFERENCE SEARCHFILENAME
001399*        BY REFERENCE COBOLFILESEARCH
001400*           GIVING SEARCHRESULT
001401*    END-CALL
001402*
001403     IF MAIN-STATE = MAIN-CTRL-STATE AND SEARCH-OK
001404       MOVE MAIN-CTRL-PRODUCT TO RANGE-35(CNT-1)
001405       ADD 1 TO CNT-1
001406*
001407       IF MAIN-CTRL-PRODUCT = MAIN-PRODUCT AND
001408             MAIN-PRODUCT NOT = SPACES
001409         MOVE 'Y' TO FLAG-1
001410       END-IF
001411*
001412       IF MAIN-CTRL-PRODUCT = LAST-PRODUCT AND
001413             LAST-PRODUCT NOT = SPACES
001414         MOVE 'Y' TO FLAG-2
001415       END-IF
001416     END-IF
001417*
001418     READ MAIN-CTRL-FILE
001419     NEXT RECORD
001420     END-READ
001421     END-PERFORM.
001422*
001423     MOVE RANGE-35(2) TO RANGE-35(1).
001424     MOVE MAIN-PRODUCT-I TO SETVAL-ID.
001425*
001426     PERFORM 0012-SETUP-COMBO-BOX-PROC.
001427*
001428     CLOSE MAIN-CTRL-FILE.
001429*
001430     IF FLAG-1 = 'N'
001431       MOVE RANGE-35(2) TO MAIN-PRODUCT
001432     END-IF.
001433*
001434* restore prior product selection
001435*
001436     IF FLAG-2 = 'Y'
001437       MOVE LAST-PRODUCT TO MAIN-PRODUCT
001438     END-IF.
001439* -====-
001440 0010-LOAD-RATE-FILE-DESCR-PROC.
001441     OPEN INPUT MAIN-CTRL-FILE.
001442*
001443     MOVE LOW-VALUE TO SETVAL-DATA.
001444*
001445     MOVE MAIN-STATE TO MAIN-CTRL-STATE.
001446     MOVE MAIN-PRODUCT TO MAIN-CTRL-PRODUCT.
001447*
001448     READ MAIN-CTRL-FILE
001449         INVALID KEY
001450       INITIALIZE MAIN-CTRL-DATA
001451     END-READ.
001452*
001453     MOVE 'N' TO FLAG-1.
001454     MOVE 2 TO CNT-2.
001455*
001456     PERFORM
001457           VARYING CNT-1 FROM 1 BY 1
001458           UNTIL CNT-1 > 40
001459*
001460* search for rate file
001461*
001462       MOVE CURRENT-DIRECTORY-NAME TO SEARCHFILENAME
001463       ADD 1 BUFFER-LENGTH GIVING CNT-3
001464       STRING
001465           '\DATA\'
001466           DELIMITED BY SIZE
001467           MAIN-CTRL-RATE-FILE-NAME(CNT-1)
001468           DELIMITED BY '   '
001469           INTO SEARCHFILENAME WITH POINTER CNT-3
001470     END-STRING
001471*    CALL 'REALIA_FINDFIRST'
001472*          USING BY REFERENCE SEARCH-HANDLE
001473*        BY REFERENCE SEARCHFILENAME
001474*        BY REFERENCE COBOLFILESEARCH
001475*           GIVING SEARCHRESULT
001476*    END-CALL
001477*
001478     IF SEARCH-OK
001479       IF MAIN-CTRL-RATE-FILE-DESCR(CNT-1) = MAIN-RATE-FILE-DESCR
001480             AND MAIN-RATE-FILE-DESCR NOT = SPACES
001481         MOVE 'Y' TO FLAG-1
001482       END-IF
001483       MOVE MAIN-CTRL-RATE-FILE-DESCR(CNT-1) TO
001484           RANGE-60(CNT-2)
001485       ADD 1 TO CNT-2
001486     END-IF
001487*
001488     END-PERFORM.
001489*
001490     MOVE RANGE-60(2) TO RANGE-60(1).
001491*
001492     MOVE MAIN-RATE-FILE-DESCR-I TO SETVAL-ID.
001493*
001494     PERFORM 0012-SETUP-COMBO-BOX-PROC.
001495*
001496     CLOSE MAIN-CTRL-FILE.
001497*
001498     IF FLAG-1 = 'N'
001499       MOVE RANGE-60(2) TO MAIN-RATE-FILE-DESCR
001500     END-IF.
001501*
001502     IF MAIN-RATE-FILE-DESCR = SPACES OR LOW-VALUE
001503       MOVE RANGE-60(2) TO MAIN-RATE-FILE-DESCR
001504     END-IF.
001505*
001506     IF RANGE-60(2) = SPACES OR LOW-VALUE
001507       MOVE 'h' TO MAIN-RATE-FILE-DESCR-T
001508       MOVE SPACE TO MAIN-RATE-FILE-DESCR-TEXT
001509     ELSE
001510       MOVE LOW-VALUE TO MAIN-RATE-FILE-DESCR-T
001511       MOVE 'Available Rate Files:' TO MAIN-RATE-FILE-DESCR-TEXT
001512     END-IF.
001513*
001514*    CALL "SP2" USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.
001515* -====-
001516 0011-LOAD-AVAILABLE-TYPES.
001517     OPEN INPUT
001518       MAIN-CTRL-FILE
001519       LOANS-NA-FILE.
001520*
001521     MOVE LOW-VALUE TO SETVAL-DATA.
001522*
001523     MOVE MAIN-STATE TO
001524       MAIN-CTRL-STATE
001525       LOANS-NA-STATE.
001526*
001527     MOVE MAIN-PRODUCT TO MAIN-CTRL-PRODUCT.
001528*
001529     MOVE MAIN-RATE-FILE-DESCR TO LOANS-NA-RATE-FILE-DESCR.
001530*
001531     READ MAIN-CTRL-FILE
001532         INVALID KEY
001533       INITIALIZE MAIN-CTRL-DATA
001534     END-READ.
001535*
001536     MOVE 2 TO CNT-2.
001537     MOVE 'N' TO FLAG-1.
001538*
001539     PERFORM
001540           VARYING CNT-1 FROM 1 BY 1
001541           UNTIL CNT-1 > 40
001542*
001543       MOVE 'Y' TO LOANS-NA-DATA
001544*
001545       MOVE MAIN-CTRL-LOAN-REFUND-TYPE(CNT-1) TO
001546         LOANS-NA-LOAN-TYPE-DESCR
001547*
001548       READ LOANS-NA-FILE
001549         INVALID KEY
001550           CONTINUE
001551       END-READ
001552*
001553       IF MAIN-CTRL-LOAN-REFUND-TYPE(CNT-1) NOT = SPACES AND
001554         LOANS-NA-DATA = 'Y'
001555         IF MAIN-CTRL-LOAN-REFUND-TYPE(CNT-1) =
001556               MAIN-LOAN-REFUND-TYPE AND
001557               MAIN-LOAN-REFUND-TYPE NOT = SPACES
001558           MOVE 'Y' TO FLAG-1
001559         END-IF
001560         MOVE MAIN-CTRL-LOAN-REFUND-TYPE(CNT-1) TO
001561             RANGE-50(CNT-2)
001562         ADD 1 TO CNT-2
001563       END-IF
001564     END-PERFORM.
001565*
001566     MOVE RANGE-50(2) TO RANGE-50(1).
001567*
001568     MOVE MAIN-LOAN-REFUND-TYPE-I TO SETVAL-ID.
001569*
001570     PERFORM 0012-SETUP-COMBO-BOX-PROC.
001571*
001572     CLOSE
001573       MAIN-CTRL-FILE
001574       LOANS-NA-FILE.
001575*
001576     IF FLAG-1 = 'N'
001577       MOVE RANGE-50(2) TO MAIN-LOAN-REFUND-TYPE
001578     END-IF.
001579*
001580     IF MAIN-LOAN-REFUND-TYPE = SPACES OR LOW-VALUE
001581       MOVE RANGE-50(2) TO MAIN-LOAN-REFUND-TYPE
001582     END-IF.
001583*
001584     IF RANGE-50(2) = SPACES OR LOW-VALUE
001585       MOVE 'h' TO MAIN-LOAN-REFUND-TYPE-T
001586       MOVE SPACES TO MAIN-LOAN-REFUND-TYPE-TEXT
001587     ELSE
001588       MOVE LOW-VALUE TO MAIN-LOAN-REFUND-TYPE-T
001589       MOVE 'Type of Loan/Refund:' TO MAIN-LOAN-REFUND-TYPE-TEXT
001590     END-IF.
001591*
001592*    CALL "SP2" USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.
001593* -====-
001594 0012-SETUP-COMBO-BOX-PROC.
001595*
001596* This paragraph can be included in any program needing
001597* to reset the initial value for a field.  This initial
001598* value is important because it is used for a variety of
001599* different purposed, in particular:
001600* - List of entries for a combobox
001601* - List of entries for a listbox
001602* - Bitmaps to be displayed for an icon
001603* Before performing the paragraph:
001604* 1. Copy "SP2.CPY" into working-storage
001605* 2. Copy "SETVALWS.CPY" into working-storage
001606* 3. Define item called SETVAL-DATA containing new value
001607* 4. Open a window for the panel containing the field
001608* 5. Set SETVAL-ID to id of field
001609* 6. Set SETVAL-LEN to length of new value
001610*
001611* find the maximum number of lines in a drop down box
001612* times maximum width a box
001613* (check all box to find individual maximums)
001614* (one box may short and another may be wide)
001615* example 52 * 20 = 1200
001616*
001617     MOVE 1800 TO SETVAL-LEN.
001618*
001619     MOVE 2000 TO SP2-FD-VAR-LEN.
001620     MOVE LOW-VALUES TO SP2-FD-VAR-LENS.
001621     MOVE LOW-VALUES TO SP2-FD-DATA.
001622     MOVE SETVAL-ID TO SP2-FD-ID.
001623*    CALL "SP2" USING SP2-GET-FIELD-DEF SP2-FIELD-DEF.
001624     COMPUTE SETVAL-1 = SP2-FD-FORMAT-LEN
001625         + SP2-FD-CAPTION-LEN + 1.
001626     COMPUTE SETVAL-2 = SP2-FD-RANGE-LEN + SP2-FD-DISCRETE-LEN
001627         + SP2-FD-MSG-TEXT-LEN.
001628*
001629     IF SETVAL-2 > 0
001630       ADD SP2-FD-INITIAL-LEN TO SETVAL-1
001631       MOVE SP2-FD-VAR-DATA (SETVAL-1 : SETVAL-2)
001632           TO SETVAL-SAVE
001633       SUBTRACT SP2-FD-INITIAL-LEN FROM SETVAL-1.
001634     SUBTRACT SP2-FD-INITIAL-LEN FROM SP2-FD-VAR-LEN.
001635     MOVE SETVAL-LEN TO SP2-FD-INITIAL-LEN.
001636     ADD SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN.
001637     MOVE SETVAL-DATA
001638         TO SP2-FD-VAR-DATA (SETVAL-1 : SP2-FD-INITIAL-LEN).
001639*
001640     IF SETVAL-2 > 0
001641       ADD SP2-FD-INITIAL-LEN TO SETVAL-1
001642       MOVE SETVAL-SAVE (1 : SETVAL-2)
001643           TO SP2-FD-VAR-DATA (SETVAL-1 : SETVAL-2).
001644*    CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.
001645* -====-
001646 0013-PREVIEW-GLOSSARY-PROC.
001647     MOVE LOW-VALUES TO QPR-INIT-AREA.
001648     MOVE "n" TO QPR-METHOD.
001649     MOVE "P-CID.PAN" TO QPR-FILE.
001650     CALL "QPR" USING QPR-INIT QPR-INIT-AREA.
001651*
001652     MOVE LOW-VALUE TO QPR-AREA.
001653     MOVE "d" TO QPR-DIALOG.
001654     MOVE "p" TO QPR-ORIENTATION.
001655     MOVE 1 TO QPR-COPIES.
001656     SET QPR-DO-PREVIEW TO TRUE.
001657*
001658     MOVE 'CSO CID - Quikcalc Glossary' TO QPR-DOC-NAME.
001659*
001660     CALL "QPR" USING QPR-SELECT-PRINTER-EX QPR-AREA.
001661*
001662     MOVE LOW-VALUES TO
001663         P-CID-GL-DATA
001664         P-CID-GL-COLRS
001665         P-CID-GL-TYPES.
001666*
001667     INITIALIZE
001668         P-CID-GL-FIELDS.
001669*
001670     MOVE 'P-CID-GL' TO P-CID-GL-NEXT-PANEL.
001671*
001672     STRING
001673         'Version '
001674         VERSION-NO
001675         ', Release '
001676         RELEASE-DATE
001677         DELIMITED BY SIZE
001678          INTO P-CID-GL-VER-REL
001679     END-STRING.
001680*
001681     CALL 'QPR' USING QPR-PRINT-PAGE P-CID-GL-CONVERSE-DATA.
001682*
001683     CALL 'QPR' USING QPR-END-PRINT QPR-NULL-PARM.
001684* -====-
001685 0014-CALL-DLL-PROC.
001686     INITIALIZE PASSED-DATA.
001687*
001688     MOVE MAIN-PRODUCT TO PASSED-PRODUCT.
001689     MOVE MAIN-STATE TO PASSED-STATE.
001690     MOVE MAIN-RATE-FILE-DESCR TO PASSED-RATE-FILE-DESCR.
001691     MOVE MAIN-LOAN-REFUND-TYPE TO PASSED-LOAN-REFUND-TYPE.
001692*
001693     PERFORM
001694           VARYING CNT-1 FROM 1 BY 1
001695           UNTIL MAIN-RATE-FILE-DESCR =
001696           MAIN-CTRL-RATE-FILE-DESCR(CNT-1)
001697           OR CNT-1 > 40
001698     END-PERFORM.
001699*
001700     MOVE CURRENT-DIRECTORY-NAME TO PASSED-RATE-FILE-NAME.
001701     ADD 1 BUFFER-LENGTH GIVING CNT-2.
001702*
001703     STRING
001704         '\DATA\'
001705         DELIMITED BY SIZE
001706         MAIN-CTRL-RATE-FILE-NAME(CNT-1)
001707         DELIMITED BY '   '
001708          INTO PASSED-RATE-FILE-NAME WITH POINTER CNT-2
001709     END-STRING.
001710*
001711     MOVE MAIN-LIMIT-FILE TO PASSED-LIMIT-FILE-DESCR.
001712*
001713     IF MAIN-LIMIT-FILE-T = LOW-VALUE
001714       IF MAIN-PRODUCT = 'QuikCalc - Standard'
001715         MOVE 'LMT' TO TEMP-FILE-EXT
001716       ELSE
001717         MOVE 'LIM' TO TEMP-FILE-EXT
001718       END-IF
001719       MOVE CURRENT-DIRECTORY-NAME TO PASSED-LIMIT-FILE-NAME
001720       ADD 1 BUFFER-LENGTH GIVING CNT-2
001721       STRING
001722           '\DATA\'
001723           DELIMITED BY SIZE
001724           PASSED-LIMIT-FILE-DESCR
001725           DELIMITED BY '   '
001726           '.'
001727           TEMP-FILE-EXT
001728           DELIMITED BY SIZE
001729           INTO PASSED-LIMIT-FILE-NAME WITH POINTER CNT-2
001730     END-STRING
001731*
001732     STRING
001733         PASSED-LIMIT-FILE-DESCR
001734         DELIMITED BY '   '
001735         '.'
001736         TEMP-FILE-EXT
001737         ', '
001738         DELIMITED BY SIZE
001739         MAIN-CTRL-RATE-FILE-NAME(CNT-1)
001740         DELIMITED BY '   '
001741          INTO PASSED-FILE-FOOTNOTE
001742     END-STRING
001743     ELSE
001744*
001745       STRING
001746           MAIN-CTRL-RATE-FILE-NAME(CNT-1)
001747           DELIMITED BY '   '
001748           INTO PASSED-FILE-FOOTNOTE
001749     END-STRING
001750     END-IF.
001751*
001752     MOVE FUNCTION LOWER-CASE(PASSED-FILE-FOOTNOTE) TO
001753         PASSED-FILE-FOOTNOTE.
001754*
001755     CALL MAIN-CTRL-PRODUCT-DLL USING PASSED-DATA.
001756* -====-
001757 0015-SET-HOEPA-TRIGGER-PROC.
001758     OPEN INPUT
001759         AGENT-FILE
001760         HOEPA-TRIGGER-FILE.
001761*
001762     INITIALIZE HOEPA-TRIGGER-RECORD.
001763     MOVE MAIN-STATE TO HOEPA-TRIGGER-STATE.
001764*
001765     READ HOEPA-TRIGGER-FILE
001766         INVALID KEY
001767       CONTINUE
001768     END-READ.
001769*
001770     READ AGENT-FILE.
001771*
001772     CLOSE AGENT-FILE
001773*
001774     MOVE HOEPA-TRIGGER-RATE TO AGENT-COLOR-TRIGGER.
001775*
001776     OPEN OUTPUT AGENT-FILE.
001777*
001778     WRITE AGENT-RECORD.
001779*
001780     CLOSE
001781         AGENT-FILE
001782         HOEPA-TRIGGER-FILE.
001783* -====-
001784*
001785

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CID' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CID' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
