00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS083
00003  PROGRAM-ID.                CSO083.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS083
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS083
00006 *              CONVERSION DATE 02/08/96 18:42:05.                 ECS083
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS083
00008 *                           VMOD=2.008.                           ECS083
00009 *AUTHOR.     LOGIC, INC.                                          ECS083
00010 *            DALLAS, TEXAS.                                       ECS083
00011                                                                   ECS083
00012 *DATE-COMPILED.                                                   ECS083
00013                                                                   ECS083
000009                                                                  00000009
000010 SECURITY.   *****************************************************00000010
000011             *                                                   *00000011
000012             *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000012
000013             *                                                   *00000013
000014             *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000014
000015             *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000015
000016             *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000016
000017             *                                                   *00000017
000018             *****************************************************00000018
000019                                                                  00000019
000020*REMARKS.                                                         00000020
000021******************************************************************00000021
000022* ****************************************************************00000022
000023*                                                                 00000023
000024* THIS PROGRAM IS A COPY OF LOGIC PROGRAM "ECS083". IT HAS BEEN   00000024
000025*  MODIFIED TO PROVIDE FINANCIAL AND ACTUARIAL DEPARTMENTS WITH   00000025
000026*  ADDITIONAL REPORTS FOR BALANCING MONTHLY PRODUCTION REPORTS.   00000026
000027*                                                                 00000027
000028* REGULAR "ECS083" PROCESSING IS CONTINUED WITH NO CHANGES.       00000028
000029*                                                                 00000029
000030* IT RUNS ONLY IN THE MONTH END PROCESSING. CURRENTLY IT RUNS IN  00000030
000031*  CILGM17 (AS OF 12-04-96), AND REPLACES "ECS083" IN THIS JOB.   00000031
000032*                                                                 00000032
000033* ****************************************************************00000033
000034******************************************************************00000034
000035*                                                                 00000035
000036*                                                                 00000036
000037*        EXTRACT UNEARNED PREMIUM AND COMMISSION REPORT RECORDS.  00000037
000038*        EXTRACT BY PROGRAM SWITCHES                              00000038
000039*            1 - STATE                                            00000039
000040*                COMPANY                                          00000040
000041*                STATE WITHIN CARRIER                             00000041
000042*                CARRIER                                          00000042
000043*                STATE OVERALL                                    00000043
000044*                FINAL TOTALS                                     00000044
000045*            2 - COMPANY                                          00000045
000046*                STATE WITHIN CARRIER                             00000046
000047*                CARRIER                                          00000047
000048*                STATE OVERALL                                    00000048
000049*                FINAL TOTALS                                     00000049
000050*            3 - STATE WITHIN CARRIER                             00000050
000051*                CARRIER                                          00000051
000052*                STATE OVERALL                                    00000052
000053*                FINAL TOTALS                                     00000053
000054*            4 - CARRIER                                          00000054
000055*                STATE OVERALL                                    00000055
000056*                FINAL TOTALS                                     00000056
000057*            5 - STATE OVERALL                                    00000057
000058*                FINAL TOTALS                                     00000058
000059*            6 - FINAL TOTALS                                     00000059
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
061402* 061402    2002040100003  PEMA  ADD CODE FOR VA MEAN
010508* 010508    2008010200008  PEMA  GO BACK 15 YEARS FOR ISS DATE
121610* 121610    2010120900001  PEMA  CORRECT ISS YR SORT
020113* 020113  IR2013020100001  PEMA  INCREASE # OF MORT TABLES
040114* 040114  CR2011122200002  AJRA  MODIFY DOMICILE PREM, COMM, AND TAX
120816* 120816  CR2016111600001  PEMA  Add stat load to ahstat uep
011719* 011719  IR2019011700001  PEMA  Fix bug when > 99 mort tables
031102******************************************************************
000060                                                                  00000060
000061 ENVIRONMENT DIVISION.                                            00000061
000062 INPUT-OUTPUT SECTION.                                            00000062
000063 FILE-CONTROL.                                                    00000063
000064                                                                  00000064
000065     SELECT REPTFL         ASSIGN TO SYS004-UT-FBA1-S-SYS004.     00000065
000066     SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     00000066
000067     SELECT GAAP-EXTR      ASSIGN TO SYS011-UT-2400-S-SYS011.     00000067
000068     SELECT ACT-RPT        ASSIGN TO SYS012-UR-1403-S-SYS012.     00000068
000069     SELECT FIN-RPT        ASSIGN TO SYS013-UR-1403-S-SYS013.     00000069
000070     SELECT DISK-DATE      ASSIGN TO SYS019-UT-FBA1-S-SYS019.     00000070
000071     SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     00000071
000072 EJECT                                                            00000072
000073 DATA DIVISION.                                                   00000073
000074 FILE SECTION.                                                    00000074
000075                                                                  00000075
000076 FD  ACT-RPT                                                      00000076
000077     RECORDING MODE F                                             00000077
000080     BLOCK CONTAINS 0 RECORDS.                                    00000080
000082 01  ACT-REC.                                                     00000082
CIDMOD*    12  ACT-CC              PIC X(01).                           00000083
000084     12  ACT-INFO            PIC X(132).                          00000084
000085                                                                  00000085
000086                                                                  00000086
000087 FD  FIN-RPT                                                      00000087
000088     RECORDING MODE F                                             00000088
000091     BLOCK CONTAINS 0 RECORDS.                                    00000091
000093 01  FIN-REC.                                                     00000093
CIDMOD*    12  FIN-CC              PIC X(01).                           00000094
000095     12  FIN-INFO            PIC X(132).                          00000095
000096                                                                  00000096
00062  FD  REPTFL                                                       ECS083
00063      BLOCK CONTAINS 0 RECORDS
00064      RECORDING MODE F.                                            ECS083
00065                                                                   ECS083
00066  01  RPT-REC.                                                     ECS083
011719     12  R-PARM              PIC X(27).
040114     12  FILLER              PIC X(130).                          ECS083
00069  EJECT                                                            ECS083
00070  FD  PRNTR                                                        ECS083
00071                              COPY ELCPRTFD.                       ECS083
00072  EJECT                                                            ECS083
00073  FD  GAAP-EXTR                                                    ECS083
00074                              COPY ECSGAPFD.                       ECS083
00075                                                                   ECS083
00076                              COPY ECSGAP01.                       ECS083
00077  EJECT                                                            ECS083
00078  FD  DISK-DATE                                                    ECS083
00079                              COPY ELCDTEFD.                       ECS083
00080  EJECT                                                            ECS083
00081  FD  FICH                                                         ECS083
00082                              COPY ECSFICH.                        ECS083
000097                                                                  00000097
000122 EJECT                                                            00000122
000123 WORKING-STORAGE SECTION.                                         00000123
000124 77  FILLER  PIC X(32) VALUE '********************************'.  00000124
000125 77  FILLER  PIC X(32) VALUE '     ECS083 WORKING STORAGE     '.  00000125
000126 77  FILLER  PIC X(32) VALUE '***** CLONED FROM VMOD=2.008 ***'.  00000126
000127                                                                  00000127
000128 77  AH-SUB                  PIC  999            VALUE ZEROS.     00000128
000129 77  CO-SAVE-SUB             PIC  999            VALUE ZEROS.     00000129
CIDMOD 77  GA-SAVE-SUB             PIC  999            VALUE ZEROS.     00000129
000130 77  CA-SAVE-SUB             PIC  999            VALUE ZEROS.     00000130
CIDMOD 77  OH-SAVE-SUB             PIC  999            VALUE ZEROS.     00000130
000131 77  OR-SAVE-SUB             PIC  999            VALUE ZEROS.     00000131
CIDMOD 77  TX-SAVE-SUB             PIC  999            VALUE ZEROS.     00000131
000132 77  WA-SAVE-SUB             PIC  999            VALUE ZEROS.     00000132
CIDMOD 77  CA-OH-OR-TX-WA-PRAT     PIC  9(11)V99       VALUE ZEROS.     00000133
CIDMOD 77  CA-OH-OR-TX-WA-R78      PIC  9(11)V99       VALUE ZEROS.     00000134
000135 77  TOT-CO-PRAT             PIC  9(11)V99       VALUE ZEROS.     00000135
CIDMOD 77  TOT-GA-PRAT             PIC  9(11)V99       VALUE ZEROS.     00000135
000136 77  TOT-CO-R78              PIC  9(11)V99       VALUE ZEROS.     00000136
CIDMOD 77  TOT-GA-R78              PIC  9(11)V99       VALUE ZEROS.     00000136
000137 77  CO-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
061402 77  VA-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
CIDMOD 77  GA-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
000138 77  CO-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
061402 77  VA-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
CIDMOD 77  GA-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
000139 77  COLO-TOT                PIC  9(11)V99       VALUE ZEROS.     00000139
000140 77  COLO-MEAN               PIC  9(11)V99       VALUE ZEROS.     00000140
000141 77  COLO-UNEARNED           PIC  9(11)V99       VALUE ZEROS.     00000141
061402 77  VA-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
061402 77  VA-MEAN                 PIC  9(11)V99       VALUE ZEROS.     00000140
061402 77  VA-UNEARNED             PIC  9(11)V99       VALUE ZEROS.     00000141
CIDMOD 77  GA-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
CIDMOD 77  GEORGIA-MEAN            PIC  9(11)V99       VALUE ZEROS.     00000140
CIDMOD 77  GA-UNEARNED             PIC  9(11)V99       VALUE ZEROS.     00000141
CIDMOD 77  OH-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
CIDMOD 77  OH-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
CIDMOD 77  OH-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
CIDMOD 77  OHIO-MEAN               PIC  9(11)V99       VALUE ZEROS.     00000140
CIDMOD 77  TX-OVRALL-R78           PIC  9(11)V99       VALUE ZEROS.     00000138
CIDMOD 77  TX-OVRALL-PRAT          PIC  9(11)V99       VALUE ZEROS.     00000137
CIDMOD 77  TX-TOT                  PIC  9(11)V99       VALUE ZEROS.     00000139
CIDMOD 77  TEXAS-MEAN              PIC  9(11)V99       VALUE ZEROS.     00000140
000142 77  A-WORK-GRAND-TOT        PIC  9(11)V99       VALUE ZEROS.     00000142
000143 77  A-WORK-FINAL-TOT        PIC  9(11)V99       VALUE ZEROS.     00000143
000144 77  TOT-PRAT                PIC  9(11)V99       VALUE ZEROS.     00000144
000145 77  TOT-R78                 PIC  9(11)V99       VALUE ZEROS.     00000145
000146 77  TOT-CNT                 PIC  9(11)          VALUE ZEROS.     00000146
000147 77  CO-COUNT                PIC  9(11)          VALUE ZEROS.     00000147
CIDMOD 77  GA-COUNT                PIC  9(11)          VALUE ZEROS.     00000147
000148 77  CA-COUNT                PIC  9(11)          VALUE ZEROS.     00000148
CIDMOD 77  OH-COUNT                PIC  9(11)          VALUE ZEROS.     00000148
000149 77  OR-COUNT                PIC  9(11)          VALUE ZEROS.     00000149
CIDMOD 77  TX-COUNT                PIC  9(11)          VALUE ZEROS.     00000149
000150 77  WA-COUNT                PIC  9(11)          VALUE ZEROS.     00000150
000151 77  PGM-SUB                 PIC S999    COMP    VALUE +083.      00000151
000152 77  X1                      PIC S9(4)   COMP.                    00000152
000153 77  X2                      PIC S9(4)   COMP.                    00000153
000154 77  X3                      PIC S9(4)   COMP.                    00000154
000155 77  MAX-BEN                 PIC S9(4)   COMP    VALUE +100.      00000155
000156 77  X                       PIC X               VALUE SPACE.     00000156
CIDMOD*77  X-YR                    PIC 99              VALUE ZEROS.     00000157
121610 77  WORK-YR                 PIC S9999           VALUE ZEROS.     00000157
000158 77  DIS-EDIT                PIC $ZZ,ZZZ,ZZZ,ZZZ.99.              00000158
000159 77  REC-CNT-EDIT            PIC  ZZ,ZZZ,ZZZ,ZZ9.                 00000159
CIDMOD 77  CON-19961231            PIC  9(11)          VALUE 19961231.  00000149
061402 77  CON-20020630            PIC  9(11)          VALUE 20020630.  00000149
040114 77  WRK-DOMI                PIC S9(9)V99 COMP-3 VALUE +0.
000160                                                                  00000160
000161 SKIP2                                                            00000161
000162                                                                  00000162
000163                                                                  00000163
000164 01 STATE-ACCUMS.                                                 00000164
000165     12  CO-LF-DET.                                               00000165
000166         16  CO-LF-COUNT            PIC  9(10)    VALUE ZEROS.    00000166
000167         16  CO-LF-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000167
000168         16  CO-LF-P78              PIC  9(11)V99 VALUE ZEROS.    00000168
000169         16  CO-LF-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000169
000170         16  CO-LF-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000170
000171         16  CO-LF-STATE            PIC  9(11)V99 VALUE ZEROS.    00000171
000172         16  CO-LF-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000172
000173         16  CO-LF-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000173
000174         16  CO-LF-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000174
000175         16  CO-LF-PAID             PIC  9(11)V99 VALUE ZEROS.    00000175
000176         16  CO-LF-C78              PIC  9(11)V99 VALUE ZEROS.    00000176
000177         16  CO-LF-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000177
040114         16  CO-LF-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000178         16  CO-LF-TAX              PIC  9(11)V99 VALUE ZEROS.    00000178
000179         16  CO-LF-T78              PIC  9(11)V99 VALUE ZEROS.    00000179
000180         16  CO-LF-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000180
040114         16  CO-LF-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000181                                                                  00000181
000165     12  GA-LF-DET.                                               00000165
000166         16  GA-LF-COUNT            PIC  9(10)    VALUE ZEROS.    00000166
000167         16  GA-LF-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000167
000168         16  GA-LF-P78              PIC  9(11)V99 VALUE ZEROS.    00000168
000169         16  GA-LF-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000169
000170         16  GA-LF-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000170
000171         16  GA-LF-STATE            PIC  9(11)V99 VALUE ZEROS.    00000171
000172         16  GA-LF-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000172
000173         16  GA-LF-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000173
000174         16  GA-LF-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000174
000175         16  GA-LF-PAID             PIC  9(11)V99 VALUE ZEROS.    00000175
000176         16  GA-LF-C78              PIC  9(11)V99 VALUE ZEROS.    00000176
000177         16  GA-LF-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000177
040114         16  GA-LF-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000178         16  GA-LF-TAX              PIC  9(11)V99 VALUE ZEROS.    00000178
000179         16  GA-LF-T78              PIC  9(11)V99 VALUE ZEROS.    00000179
000180         16  GA-LF-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000180
040114         16  GA-LF-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000181                                                                  00000181
000182     12  CA-LF-DET.                                               00000182
000183         16  CA-LF-COUNT            PIC  9(10)    VALUE ZEROS.    00000183
000184         16  CA-LF-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000184
000185         16  CA-LF-P78              PIC  9(11)V99 VALUE ZEROS.    00000185
000186         16  CA-LF-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000186
000187         16  CA-LF-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000187
000188         16  CA-LF-STATE            PIC  9(11)V99 VALUE ZEROS.    00000188
000189         16  CA-LF-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000189
000190         16  CA-LF-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000190
000191         16  CA-LF-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000191
000192         16  CA-LF-PAID             PIC  9(11)V99 VALUE ZEROS.    00000192
000193         16  CA-LF-C78              PIC  9(11)V99 VALUE ZEROS.    00000193
000194         16  CA-LF-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000194
040114         16  CA-LF-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000195         16  CA-LF-TAX              PIC  9(11)V99 VALUE ZEROS.    00000195
000196         16  CA-LF-T78              PIC  9(11)V99 VALUE ZEROS.    00000196
000197         16  CA-LF-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000197
040114         16  CA-LF-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000198                                                                  00000198
000199     12  OR-LF-DET.                                               00000199
000200         16  OR-LF-COUNT            PIC  9(10)    VALUE ZEROS.    00000200
000201         16  OR-LF-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000201
000202         16  OR-LF-P78              PIC  9(11)V99 VALUE ZEROS.    00000202
000203         16  OR-LF-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000203
000204         16  OR-LF-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000204
000205         16  OR-LF-STATE            PIC  9(11)V99 VALUE ZEROS.    00000205
000206         16  OR-LF-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000206
000207         16  OR-LF-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000207
000208         16  OR-LF-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000208
000209         16  OR-LF-PAID             PIC  9(11)V99 VALUE ZEROS.    00000209
000210         16  OR-LF-C78              PIC  9(11)V99 VALUE ZEROS.    00000210
000211         16  OR-LF-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000211
040114         16  OR-LF-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000212         16  OR-LF-TAX              PIC  9(11)V99 VALUE ZEROS.    00000212
000213         16  OR-LF-T78              PIC  9(11)V99 VALUE ZEROS.    00000213
000214         16  OR-LF-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000214
040114         16  OR-LF-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000215                                                                  00000215
000216     12  WA-LF-DET.                                               00000216
000217         16  WA-LF-COUNT            PIC  9(10)    VALUE ZEROS.    00000217
000218         16  WA-LF-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000218
000219         16  WA-LF-P78              PIC  9(11)V99 VALUE ZEROS.    00000219
000220         16  WA-LF-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000220
000221         16  WA-LF-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000221
000222         16  WA-LF-STATE            PIC  9(11)V99 VALUE ZEROS.    00000222
000223         16  WA-LF-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000223
000224         16  WA-LF-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000224
000225         16  WA-LF-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000225
000226         16  WA-LF-PAID             PIC  9(11)V99 VALUE ZEROS.    00000226
000227         16  WA-LF-C78              PIC  9(11)V99 VALUE ZEROS.    00000227
000228         16  WA-LF-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000228
040114         16  WA-LF-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000229         16  WA-LF-TAX              PIC  9(11)V99 VALUE ZEROS.    00000229
000230         16  WA-LF-T78              PIC  9(11)V99 VALUE ZEROS.    00000230
000231         16  WA-LF-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000231
040114         16  WA-LF-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000232                                                                  00000232
000233     12  ALL-LF-DET.                                              00000233
000234         16  ALL-LF-COUNT           PIC  9(10)    VALUE ZEROS.    00000234
000235         16  ALL-LF-WRITTEN         PIC  9(11)V99 VALUE ZEROS.    00000235
000236         16  ALL-LF-P78             PIC  9(11)V99 VALUE ZEROS.    00000236
000237         16  ALL-LF-PRATA           PIC  9(11)V99 VALUE ZEROS.    00000237
000238         16  ALL-LF-DOMICILE        PIC  9(11)V99 VALUE ZEROS.    00000238
000239         16  ALL-LF-STATE           PIC  9(11)V99 VALUE ZEROS.    00000239
000240         16  ALL-LF-RESERV          PIC  9(11)V99 VALUE ZEROS.    00000240
000241         16  ALL-LF-ALTRSV          PIC  9(11)V99 VALUE ZEROS.    00000241
000242         16  ALL-LF-REMAIN          PIC  9(11)V99 VALUE ZEROS.    00000242
000243         16  ALL-LF-PAID            PIC  9(11)V99 VALUE ZEROS.    00000243
000244         16  ALL-LF-C78             PIC  9(11)V99 VALUE ZEROS.    00000244
000245         16  ALL-LF-CRATA           PIC  9(11)V99 VALUE ZEROS.    00000245
040114         16  ALL-LF-CDOMI           PIC  9(11)V99 VALUE ZEROS.
000246         16  ALL-LF-TAX             PIC  9(11)V99 VALUE ZEROS.    00000246
000247         16  ALL-LF-T78             PIC  9(11)V99 VALUE ZEROS.    00000247
000248         16  ALL-LF-TRATA           PIC  9(11)V99 VALUE ZEROS.    00000248
040114         16  ALL-LF-TDOMI           PIC  9(11)V99 VALUE ZEROS.
000249                                                                  00000249
000250     12  CO-AH-DET.                                               00000250
000251         16  CO-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000251
000252         16  CO-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000252
000253         16  CO-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000253
000254         16  CO-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000254
000255         16  CO-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000255
000256         16  CO-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000256
000257         16  CO-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000257
000258         16  CO-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000258
000259         16  CO-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000259
000260         16  CO-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000260
000261         16  CO-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000261
000262         16  CO-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000262
040114         16  CO-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000263         16  CO-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000263
000264         16  CO-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000264
000265         16  CO-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000265
040114         16  CO-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000266                                                                  00000266
000250     12  GA-AH-DET.                                               00000250
000251         16  GA-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000251
000252         16  GA-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000252
000253         16  GA-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000253
000254         16  GA-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000254
000255         16  GA-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000255
000256         16  GA-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000256
000257         16  GA-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000257
000258         16  GA-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000258
000259         16  GA-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000259
000260         16  GA-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000260
000261         16  GA-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000261
000262         16  GA-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000262
040114         16  GA-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000263         16  GA-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000263
000264         16  GA-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000264
000265         16  GA-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000265
040114         16  GA-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000266                                                                  00000266
000267                                                                  00000267
000268     12  CA-AH-DET.                                               00000268
000269         16  CA-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000269
000270         16  CA-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000270
000271         16  CA-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000271
000272         16  CA-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000272
000273         16  CA-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000273
000274         16  CA-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000274
000275         16  CA-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000275
000276         16  CA-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000276
000277         16  CA-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000277
000278         16  CA-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000278
000279         16  CA-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000279
000280         16  CA-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000280
040114         16  CA-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000281         16  CA-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000281
000282         16  CA-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000282
000283         16  CA-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000283
040114         16  CA-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000284                                                                  00000284
CIDMOD     12  OH-AH-DET.                                               00000268
CIDMOD         16  OH-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000269
CIDMOD         16  OH-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000270
CIDMOD         16  OH-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000271
CIDMOD         16  OH-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000272
CIDMOD         16  OH-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000273
CIDMOD         16  OH-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000274
CIDMOD         16  OH-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000275
CIDMOD         16  OH-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000276
CIDMOD         16  OH-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000277
CIDMOD         16  OH-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000278
CIDMOD         16  OH-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000279
CIDMOD         16  OH-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000280
040114         16  OH-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
CIDMOD         16  OH-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000281
CIDMOD         16  OH-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000282
CIDMOD         16  OH-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000283
040114         16  OH-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
CIDMOD                                                                  00000284
000285     12  OR-AH-DET.                                               00000285
000286         16  OR-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000286
000287         16  OR-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000287
000288         16  OR-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000288
000289         16  OR-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000289
000290         16  OR-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000290
000291         16  OR-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000291
000292         16  OR-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000292
000293         16  OR-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000293
000294         16  OR-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000294
000295         16  OR-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000295
000296         16  OR-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000296
000297         16  OR-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000297
040114         16  OR-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000298         16  OR-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000298
000299         16  OR-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000299
000300         16  OR-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000300
040114         16  OR-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000301                                                                  00000301
CIDMOD     12  TX-AH-DET.                                               00000285
CIDMOD         16  TX-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000286
CIDMOD         16  TX-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000287
CIDMOD         16  TX-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000288
CIDMOD         16  TX-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000289
CIDMOD         16  TX-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000290
CIDMOD         16  TX-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000291
CIDMOD         16  TX-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000292
CIDMOD         16  TX-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000293
CIDMOD         16  TX-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000294
CIDMOD         16  TX-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000295
CIDMOD         16  TX-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000296
CIDMOD         16  TX-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000297
040114         16  TX-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
CIDMOD         16  TX-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000298
CIDMOD         16  TX-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000299
CIDMOD         16  TX-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000300
040114         16  TX-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
CIDMOD                                                                  00000301
000302     12  WA-AH-DET.                                               00000302
000303         16  WA-AH-COUNT            PIC  9(10)    VALUE ZEROS.    00000303
000304         16  WA-AH-WRITTEN          PIC  9(11)V99 VALUE ZEROS.    00000304
000305         16  WA-AH-P78              PIC  9(11)V99 VALUE ZEROS.    00000305
000306         16  WA-AH-PRATA            PIC  9(11)V99 VALUE ZEROS.    00000306
000307         16  WA-AH-DOMICILE         PIC  9(11)V99 VALUE ZEROS.    00000307
000308         16  WA-AH-STATE            PIC  9(11)V99 VALUE ZEROS.    00000308
000309         16  WA-AH-RESERV           PIC  9(11)V99 VALUE ZEROS.    00000309
000310         16  WA-AH-ALTRSV           PIC  9(11)V99 VALUE ZEROS.    00000310
000311         16  WA-AH-REMAIN           PIC  9(11)V99 VALUE ZEROS.    00000311
000312         16  WA-AH-PAID             PIC  9(11)V99 VALUE ZEROS.    00000312
000313         16  WA-AH-C78              PIC  9(11)V99 VALUE ZEROS.    00000313
000314         16  WA-AH-CRATA            PIC  9(11)V99 VALUE ZEROS.    00000314
040114         16  WA-AH-CDOMI            PIC  9(11)V99 VALUE ZEROS.
000315         16  WA-AH-TAX              PIC  9(11)V99 VALUE ZEROS.    00000315
000316         16  WA-AH-T78              PIC  9(11)V99 VALUE ZEROS.    00000316
000317         16  WA-AH-TRATA            PIC  9(11)V99 VALUE ZEROS.    00000317
040114         16  WA-AH-TDOMI            PIC  9(11)V99 VALUE ZEROS.
000318                                                                  00000318
000319     12  ALL-AH-DET.                                              00000319
000320         16  ALL-AH-COUNT           PIC  9(10)    VALUE ZEROS.    00000320
000321         16  ALL-AH-WRITTEN         PIC  9(11)V99 VALUE ZEROS.    00000321
000322         16  ALL-AH-P78             PIC  9(11)V99 VALUE ZEROS.    00000322
000323         16  ALL-AH-PRATA           PIC  9(11)V99 VALUE ZEROS.    00000323
000324         16  ALL-AH-DOMICILE        PIC  9(11)V99 VALUE ZEROS.    00000324
000325         16  ALL-AH-STATE           PIC  9(11)V99 VALUE ZEROS.    00000325
000326         16  ALL-AH-RESERV          PIC  9(11)V99 VALUE ZEROS.    00000326
000327         16  ALL-AH-ALTRSV          PIC  9(11)V99 VALUE ZEROS.    00000327
000328         16  ALL-AH-REMAIN          PIC  9(11)V99 VALUE ZEROS.    00000328
000329         16  ALL-AH-PAID            PIC  9(11)V99 VALUE ZEROS.    00000329
000330         16  ALL-AH-C78             PIC  9(11)V99 VALUE ZEROS.    00000330
000331         16  ALL-AH-CRATA           PIC  9(11)V99 VALUE ZEROS.    00000331
040114         16  ALL-AH-CDOMI           PIC  9(11)V99 VALUE ZEROS.
000332         16  ALL-AH-TAX             PIC  9(11)V99 VALUE ZEROS.    00000332
000333         16  ALL-AH-T78             PIC  9(11)V99 VALUE ZEROS.    00000333
000334         16  ALL-AH-TRATA           PIC  9(11)V99 VALUE ZEROS.    00000334
040114         16  ALL-AH-TDOMI           PIC  9(11)V99 VALUE ZEROS.
000336                                                                  00000335
000337                                                                  00000336
000338 01  NEW-RPT.                                                     00000337
000339     05  NR-CC           PIC X(01)     VALUE SPACES.              00000338
000340     05  NR-INFO         PIC X(50)     VALUE SPACES.              00000339
000341     05  FILLER          PIC X(01)     VALUE SPACES.              00000340
000342     05  PAREN           PIC X(01)     VALUE SPACES.              00000341
000343     05  NR-AMT          PIC X(50)     VALUE SPACES.              00000342
000344     05  FILLER          PIC X(29)     VALUE SPACES.              00000343
000345                                                                  00000344
000346                                                                  00000345
000347 01  NEW-RPT-LINES.                                               00000346
000348                                                                  00000347
000349     05  NR-TOT.                                                  00000348
000350         10  FILLER     PIC X(23)     VALUE SPACES.               00000349
000351         10  FILLER     PIC X(27)     VALUE                       00000350
000352           'TOTAL ALL STATES RULE OF 78'.                         00000351
000353                                                                  00000352
000354     05  CA-R78.                                                  00000353
000355         10  FILLER     PIC X(40)     VALUE SPACES.               00000354
000356         10  FILLER     PIC X(10)     VALUE                       00000355
000357           'CA RULE 78'.                                          00000356
000358                                                                  00000357
000354     05  GA-R78.                                                  00000353
000355         10  FILLER     PIC X(40)     VALUE SPACES.               00000354
000356         10  FILLER     PIC X(10)     VALUE                       00000355
000357           'GA RULE 78'.                                          00000356
000358                                                                  00000357
CIDMOD     05  OH-R78.                                                  00000353
CIDMOD         10  FILLER     PIC X(40)     VALUE SPACES.               00000354
CIDMOD         10  FILLER     PIC X(10)     VALUE                       00000355
CIDMOD           'OH RULE 78'.                                          00000356
CIDMOD                                                                  00000357
000359     05  OR-R78.                                                  00000358
000360         10  FILLER     PIC X(40)     VALUE SPACES.               00000359
000361         10  FILLER     PIC X(10)     VALUE                       00000360
000362           'OR RULE 78'.                                          00000361
000363                                                                  00000362
CIDMOD     05  TX-R78.                                                  00000358
CIDMOD         10  FILLER     PIC X(40)     VALUE SPACES.               00000359
CIDMOD         10  FILLER     PIC X(10)     VALUE                       00000360
CIDMOD           'TX RULE 78'.                                          00000361
CIDMOD                                                                  00000362
000364     05  WA-R78.                                                  00000363
000365         10  FILLER     PIC X(40)     VALUE SPACES.               00000364
000366         10  FILLER     PIC X(10)     VALUE                       00000365
000367           'WA RULE 78'.                                          00000366
000368                                                                  00000367
000369     05  CA-PRAT.                                                 00000368
000370         10  FILLER     PIC X(39)     VALUE SPACES.               00000369
000371         10  FILLER     PIC X(11)     VALUE                       00000370
000372           'CA PRO-RATA'.                                         00000371
000373                                                                  00000372
CIDMOD     05  OH-PRAT.                                                 00000368
CIDMOD         10  FILLER     PIC X(39)     VALUE SPACES.               00000369
CIDMOD         10  FILLER     PIC X(11)     VALUE                       00000370
CIDMOD           'OH PRO-RATA'.                                         00000371
CIDMOD                                                                  00000372
000374     05  OR-PRAT.                                                 00000373
000375         10  FILLER     PIC X(39)     VALUE SPACES.               00000374
000376         10  FILLER     PIC X(11)     VALUE                       00000375
000377           'OR PRO-RATA'.                                         00000376
000378                                                                  00000377
CIDMOD     05  TX-PRAT.                                                 00000373
CIDMOD         10  FILLER     PIC X(39)     VALUE SPACES.               00000374
CIDMOD         10  FILLER     PIC X(11)     VALUE                       00000375
CIDMOD           'TX PRO-RATA'.                                         00000376
CIDMOD                                                                  00000377
000379     05  WA-PRAT.                                                 00000378
000380         10  FILLER     PIC X(39)     VALUE SPACES.               00000379
000381         10  FILLER     PIC X(11)     VALUE                       00000380
000382           'WA PRO-RATA'.                                         00000381
000383                                                                  00000382
000384     05  CO-R78.                                                  00000383
000385         10  FILLER     PIC X(18)     VALUE SPACES.               00000384
000386         10  FILLER     PIC X(32)     VALUE                       00000385
CIDMOD           'CO RULE 78 FOR 97 & LATER ISSUES'.                    00000386
000388                                                                  00000387
000389     05  CO-MEAN.                                                 00000388
000390         10  FILLER     PIC X(02)     VALUE SPACES.               00000389
000391         10  FILLER     PIC X(48)     VALUE                       00000390
CIDMOD           'CO MEAN OF RULE 78 & PRO-RATA FOR 97 & LATER ISS'.    00000391
000393                                                                  00000392
000384     05  VA-R78.                                                  00000383
000385         10  FILLER     PIC X(15)     VALUE SPACES.               00000384
000386         10  FILLER     PIC X(35)     VALUE                       00000385
CIDMOD           'VA RULE 78 FOR JUL02 & LATER ISSUES'.                 00000386
000388                                                                  00000387
000389     05  VA-MEAN-HEAD.                                            00000388
000390         10  FILLER     PIC X(02)     VALUE SPACES.               00000389
000391         10  FILLER     PIC X(48)     VALUE                       00000390
CIDMOD           'VA MEAN OF R 78 & PRO-RATA FOR JUL02 & LATER ISS'.    00000391
000393                                                                  00000392
000389     05  GA-MEAN.                                                 00000388
000390         10  FILLER     PIC X(05)     VALUE SPACES.               00000389
000391         10  FILLER     PIC X(45)     VALUE                       00000390
CIDMOD           'GA MEAN OF RULE 78 & PRO-RATA FOR ALL ISSUES '.       00000391
000393                                                                  00000392
CIDMOD     05  OH-MEAN.                                                 00000388
CIDMOD         10  FILLER     PIC X(05)     VALUE SPACES.               00000389
CIDMOD         10  FILLER     PIC X(45)     VALUE                       00000390
CIDMOD           'OH MEAN OF RULE 78 & PRO-RATA FOR ALL ISSUES '.       00000391
CIDMOD                                                                  00000392
CIDMOD     05  TX-MEAN.                                                 00000388
CIDMOD         10  FILLER     PIC X(05)     VALUE SPACES.               00000389
CIDMOD         10  FILLER     PIC X(45)     VALUE                       00000390
CIDMOD           'TX MEAN OF RULE 78 & PRO-RATA FOR ALL ISSUES '.       00000391
CIDMOD                                                                  00000392
000394     05  MO-TOTAL.                                                00000393
000395         10  FILLER     PIC X(25)     VALUE SPACES.               00000394
000396         10  FILLER     PIC X(15)     VALUE                       00000395
000397           'UNEARNED FOR - '.                                     00000396
000398         10  UNEARNED-MO  PIC X(10)   VALUE SPACES.               00000397
000399                                                                  00000398
000400                                                                  00000399
000401     05  PRINT-MEAN.                                              00000400
000402         10  PM-R78     PIC X(18)     VALUE SPACES.               00000401
000403         10  FILLER     PIC X(02)     VALUE SPACES.               00000402
000404         10  PM-PLUS    PIC X(01)     VALUE '+'.                  00000403
000405         10  FILLER     PIC X(02)     VALUE SPACES.               00000404
000406         10  PM-PRAT    PIC X(18)     VALUE SPACES.               00000405
000407         10  FILLERS    PIC X(01)     VALUE ')'.                  00000406
000408         10  FILLER     PIC X(01)     VALUE SPACES.               00000407
000409         10  PM-DIVIDE  PIC X(03)     VALUE '/ 2'.                00000408
000410         10  FILLER     PIC X(04)     VALUE SPACES.               00000409
000411                                                                  00000410
000412 01  GET-MONTH.                                                   00000411
000413     05  PULL-MONTH.                                              00000412
000414         10  FIRST4           PIC X(08)     VALUE SPACES.         00000413
000415         10  LAST-14          PIC X(10)     VALUE SPACES.         00000414
000419                                                                  00000415
000420                                                                  00000416
000421 01  PRT-LINE.                                                    00000417
000422     05  PRT-CC           PIC X(01)     VALUE SPACES.             00000418
000423     05  PRT-INFO         PIC X(32)     VALUE SPACES.             00000419
000424     05  PRT-AMT          PIC X(30)     VALUE SPACES.             00000420
000425     05  FILLER           PIC X(69)     VALUE SPACES.             00000421
000428                                                                  00000422
000429                                                                  00000423
000430 01  DIS-RCD.                                                     00000424
000431     05  DIS-INFO         PIC X(35)     VALUE SPACES.             00000425
000432     05  DIS-AMT          PIC X(35)     VALUE SPACES.             00000426
000433                                                                  00000427
000434                                                                  00000428
000435 01  PRNT-TOTALS.                                                 00000429
000436     05  FILLER           PIC XX        VALUE SPACES.             00000430
000437     05  FILLER           PIC X(14)     VALUE '- OVERALL -   '.   00000431
000438     05  FILLER           PIC X(08)     VALUE 'COUNT = '.         00000432
000439     05  TOT-COUNT-P      PIC ZZ,ZZZ,ZZ9.                         00000433
000440     05  FILLER           PIC X(05)     VALUE SPACES.             00000434
000441     05  FILLER           PIC X(07)     VALUE 'R-78 = '.          00000435
000442     05  TOT-R78-P        PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000436
000443     05  FILLER           PIC X(05)     VALUE SPACES.             00000437
000444     05  FILLER           PIC X(07)     VALUE 'PRAT = '.          00000438
000445     05  TOT-PRAT-P       PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000439
000446                                                                  00000440
000447                                                                  00000441
000448 01  PRNT-CODES.                                                  00000442
000449     05  FILLER           PIC XX        VALUE SPACES.             00000443
000450     05  FILLER           PIC X(07)     VALUE 'CODE = '.          00000444
000451     05  BEN-CODE-P       PIC XX.                                 00000445
000452     05  FILLER           PIC X(05)     VALUE SPACES.             00000446
000453     05  FILLER           PIC X(08)     VALUE 'COUNT = '.         00000447
000454     05  BEN-COUNT-P      PIC ZZ,ZZZ,ZZ9.                         00000448
000455     05  FILLER           PIC X(05)     VALUE SPACES.             00000449
000456     05  FILLER           PIC X(07)     VALUE 'R-78 = '.          00000450
000457     05  BEN-R78-P        PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000451
000458     05  FILLER           PIC X(05)     VALUE SPACES.             00000452
000459     05  FILLER           PIC X(07)     VALUE 'PRAT = '.          00000453
000460     05  BEN-PRAT-P       PIC $ZZ,ZZZ,ZZZ,ZZZ.99.                 00000454
000461                                                                  00000455
000462 01  ALL-BEN-COUNT.                                               00000456
000463     05  FILLER               PIC X(08)  VALUE SPACES.            00000457
000464     05  ALL-BEN-COUNT-P      PIC X(10)  VALUE SPACES.            00000458
000465                                                                  00000459
000466 01  CO-BEN-TAB-WORK.                                             00000460
000467     05  CO-BEN-TABLE.                                            00000461
000468         10  CO-B-T-WORK           OCCURS 100 TIMES.              00000462
000469             15  CO-BEN-CODE         PIC XX.                      00000463
000470             15  CO-BEN-COUNT        PIC 99999999.                00000464
000471             15  CO-BEN-R78          PIC 99999999999V99.          00000465
000472             15  CO-BEN-PRAT         PIC 99999999999V99.          00000466
000473                                                                  00000467
000466 01  GA-BEN-TAB-WORK.                                             00000460
000467     05  GA-BEN-TABLE.                                            00000461
000468         10  GA-B-T-WORK           OCCURS 100 TIMES.              00000462
000469             15  GA-BEN-CODE         PIC XX.                      00000463
000470             15  GA-BEN-COUNT        PIC 99999999.                00000464
000471             15  GA-BEN-R78          PIC 99999999999V99.          00000465
000472             15  GA-BEN-PRAT         PIC 99999999999V99.          00000466
000473                                                                  00000467
000474                                                                  00000467
000476 01  CA-BEN-TAB-WORK.                                             00000469
000477     05  CA-BEN-TABLE.                                            00000470
000478         10  CA-B-T-WORK            OCCURS 100 TIMES.             00000471
000479             15  CA-BEN-CODE          PIC XX.                     00000472
000480             15  CA-BEN-COUNT         PIC 99999999.               00000473
000481             15  CA-BEN-R78           PIC 99999999999V99.         00000474
000482             15  CA-BEN-PRAT          PIC 99999999999V99.         00000475
000483                                                                  00000476
000484                                                                  00000477
CIDMOD 01  OH-BEN-TAB-WORK.                                             00000469
CIDMOD     05  OH-BEN-TABLE.                                            00000470
CIDMOD         10  OH-B-T-WORK            OCCURS 100 TIMES.             00000471
CIDMOD             15  OH-BEN-CODE          PIC XX.                     00000472
CIDMOD             15  OH-BEN-COUNT         PIC 99999999.               00000473
CIDMOD             15  OH-BEN-R78           PIC 99999999999V99.         00000474
CIDMOD             15  OH-BEN-PRAT          PIC 99999999999V99.         00000475
CIDMOD                                                                  00000476
CIDMOD                                                                  00000477
000485 01  OR-BEN-TAB-WORK.                                             00000478
000486     05  OR-BEN-TABLE.                                            00000479
000487         10  OR-B-T-WORK            OCCURS 100 TIMES.             00000480
000488             15  OR-BEN-CODE          PIC XX.                     00000481
000489             15  OR-BEN-COUNT         PIC 99999999.               00000482
000490             15  OR-BEN-R78           PIC 99999999999V99.         00000483
000491             15  OR-BEN-PRAT          PIC 99999999999V99.         00000484
000492                                                                  00000485
000493                                                                  00000486
CIDMOD 01  TX-BEN-TAB-WORK.                                             00000478
CIDMOD     05  TX-BEN-TABLE.                                            00000479
CIDMOD         10  TX-B-T-WORK            OCCURS 100 TIMES.             00000480
CIDMOD             15  TX-BEN-CODE          PIC XX.                     00000481
CIDMOD             15  TX-BEN-COUNT         PIC 99999999.               00000482
CIDMOD             15  TX-BEN-R78           PIC 99999999999V99.         00000483
CIDMOD             15  TX-BEN-PRAT          PIC 99999999999V99.         00000484
CIDMOD                                                                  00000485
CIDMOD                                                                  00000486
000494 01  WA-BEN-TAB-WORK.                                             00000487
000495     05  WA-BEN-TABLE.                                            00000488
000496         10  WA-B-T-WORK            OCCURS 100 TIMES.             00000489
000497             15  WA-BEN-CODE          PIC XX.                     00000490
000498             15  WA-BEN-COUNT         PIC 99999999.               00000491
000499             15  WA-BEN-R78           PIC 99999999999V99.         00000492
000500             15  WA-BEN-PRAT          PIC 99999999999V99.         00000493
000501                                                                  00000494
000503                                                                  00000495
000504 01  WORK-REC.                                                    00000496
000505     12  W-REIN              PIC X(6).                            00000497
000506     12  W-SEQ.                                                   00000498
000507         16  W-SEQ1.                                              00000499
000508             20  W-CARR      PIC X.                               00000500
000509             20  W-CO        PIC X(6).                            00000501
000510             20  W-ST        PIC XX.                              00000502
000511         16  W-SEQ2.                                              00000503
121610             20  W-YR        PIC 9999.                            00000504
000513             20  W-CODE      PIC 9.                               00000505
000514         16  W-SEQ3.                                              00000506
000515             20  W-LAH       PIC X.                               00000507
000516             20  W-BEN       PIC XX.                              00000508
011719             20  W-MORT      PIC 999.
000518     12  W-FL                PIC X.                               00000510
000519     12  W-AMTS.                                                  00000511
000520         16  W-DUP           PIC S9(7)     COMP-3.                00000512
000521         16  W-COUNT         PIC S9(7)     COMP-3.                00000513
000522         16  W-WRITTEN       PIC S9(9)V99  COMP-3.                00000514
000523         16  W-P78           PIC S9(9)V99  COMP-3.                00000515
000524         16  W-PRATA         PIC S9(9)V99  COMP-3.                00000516
000525         16  W-DOMICILE      PIC S9(9)V99  COMP-3.                00000517
000526         16  W-STATE         PIC S9(9)V99  COMP-3.                00000518
000527         16  W-RESERV        PIC S9(9)V99  COMP-3.                00000519
000528         16  W-ALTRSV        PIC S9(9)V99  COMP-3.                00000520
000529         16  W-REMAIN        PIC S9(13)V99 COMP-3.                00000521
000530         16  W-PAID          PIC S9(9)V99  COMP-3.                00000522
000531         16  W-C78           PIC S9(9)V99  COMP-3.                00000523
000532         16  W-CRATA         PIC S9(9)V99  COMP-3.                00000524
040114         16  W-CDOMI         PIC S9(9)V99  COMP-3.
000533         16  W-TAX           PIC S9(9)V99  COMP-3.                00000525
000534         16  W-T78           PIC S9(9)V99  COMP-3.                00000526
000535         16  W-TRATA         PIC S9(9)V99  COMP-3.                00000527
040114         16  W-TDOMI         PIC S9(9)V99  COMP-3.
000536     12  W-M-AMTS.                                                00000528
000537         16  W-IUNDR         PIC S9(9)V99  COMP-3.                00000529
000538         16  W-IOVER         PIC S9(9)V99  COMP-3.                00000530
000539         16  W-GUNDR         PIC S9(9)V99  COMP-3.                00000531
000540         16  W-GOVER         PIC S9(9)V99  COMP-3.                00000532
000541 EJECT                                                            00000533
000542 01  MISC-WS.                                                     00000534
000543     12  WS-RETURN-CODE      PIC S9(4)   COMP.                    00000535
000544     12  WS-ABEND-MESSAGE    PIC X(80).                           00000536
000545     12  WS-ABEND-FILE-STATUS PIC XX     VALUE ZEROS.             00000537
000546     12  WS-ZERO             PIC S9       VALUE +0 COMP-3.        00000538
000547     12  CUR-SEQ.                                                 00000539
000548         16  CUR-CARR        PIC X.                               00000540
000549         16  CUR-CO          PIC X(6).                            00000541
000550         16  CUR-ST          PIC XX.                              00000542
000551     12  PRE-SEQ.                                                 00000543
000552         16  PRE-CARR        PIC X          VALUE LOW-VALUE.      00000544
000553         16  PRE-CO          PIC X(6)       VALUE LOW-VALUE.      00000545
000554         16  PRE-ST          PIC XX         VALUE LOW-VALUE.      00000546
000555     12  PRE-REIN            PIC X(6)       VALUE LOW-VALUE.      00000547
CIDMOD     12  X-YR                PIC 9(4)       VALUE ZEROS.          00000547
000556                                                                  00000548
000557 01  PASS-ONE-WORK.                                               00000549
000558     12  XT-TABLE.                                                00000550
010508         16  XT-YEARS        OCCURS 15 TIMES.                     00000551
000560             20  XT-SEQ      OCCURS 100 TIMES.                    00000552
000561                 24  XT-SEQ1     PIC XXX.                         00000553
011719                 24  XT-SEQ2     PIC XXX.
000563                 24  XT-SEQ3     PIC X(6).                        00000555
000564     12  YT-TABLE.                                                00000556
010508         16  YT-YEARS        OCCURS 15 TIMES.                     00000557
000566             20  YT-SEQ      OCCURS 100 TIMES.                    00000558
000567                 24  YT-SEQ1     PIC XXX.                         00000559
011719                 24  YT-SEQ2     PIC XXX.
000569                 24  YT-SEQ3     PIC X(6).                        00000561
000570     12  X-SEQ.                                                   00000562
000571         16  X-SEQ1.                                              00000563
000572             20  X-LAH           PIC X.                           00000564
000573             20  X-BEN           PIC XX.                          00000565
000574         16  X-SEQ2.                                              00000566
011719             20  X-MORT          PIC 999.
000576         16  X-SEQ3.                                              00000568
000577             20  X-REIN          PIC X(6).                        00000569
000578                                                                  00000570
000579 01  COMMON-TOTALS.                                               00000571
000580     12  X-DETL.                                                  00000572
000581         16  X-DUP               PIC S9(7)     COMP-3.            00000573
000582         16  X-COUNT             PIC S9(7)     COMP-3.            00000574
000583         16  X-WRITTEN           PIC S9(9)V99  COMP-3.            00000575
000584         16  X-P78               PIC S9(9)V99  COMP-3.            00000576
000585         16  X-PRATA             PIC S9(9)V99  COMP-3.            00000577
000586         16  X-DOMICILE          PIC S9(9)V99  COMP-3.            00000578
000587         16  X-STATE             PIC S9(9)V99  COMP-3.            00000579
000588         16  X-RESERV            PIC S9(9)V99  COMP-3.            00000580
000589         16  X-ALTRSV            PIC S9(9)V99  COMP-3.            00000581
000590         16  X-REMAIN            PIC S9(13)V99 COMP-3.            00000582
000591         16  X-PAID              PIC S9(9)V99  COMP-3.            00000583
000592         16  X-C78               PIC S9(9)V99  COMP-3.            00000584
000593         16  X-CRATA             PIC S9(9)V99  COMP-3.            00000585
040114         16  X-CDOMI             PIC S9(9)V99  COMP-3.
000594         16  X-TAX               PIC S9(9)V99  COMP-3.            00000586
000595         16  X-T78               PIC S9(9)V99  COMP-3.            00000587
000596         16  X-TRATA             PIC S9(9)V99  COMP-3.            00000588
040114         16  X-TDOMI             PIC S9(9)V99  COMP-3.
000597     12  X-M-DETL.                                                00000589
000598         16  X-IUNDR             PIC S9(9)V99  COMP-3.            00000590
000599         16  X-IOVER             PIC S9(9)V99  COMP-3.            00000591
000600         16  X-GUNDR             PIC S9(9)V99  COMP-3.            00000592
000601         16  X-GOVER             PIC S9(9)V99  COMP-3.            00000593
000602                                                                  00000594
000603 01  COMMON-TOTALS-1.                                             00000595
000604     12  X-TOTALS.                                                00000596
010508         16  X-YEARS         OCCURS 15 TIMES.                     00000597
000606             20  X-TYPES     OCCURS 100 TIMES.                    00000598
040114                 24  X-AMTS      PIC X(106).                      00000599
000608                                                                  00000600
000609 01  COMMON-TOTALS-2.                                             00000601
000610     12  Y-TOTALS.                                                00000602
010508         16  Y-YEARS         OCCURS 15 TIMES.                     00000603
000612             20  Y-TYPES     OCCURS 100 TIMES.                    00000604
040114                 24  Y-AMTS      PIC X(106).                      00000605
000614                                                                  00000606
000615 01  COMMON-TOTALS-3.                                             00000607
000616     12  X-FILLER1.                                               00000608
010508         16  X-FILLER3       OCCURS 15  TIMES.                    00000609
000618             20  X-FILLER4   OCCURS 100 TIMES.                    00000610
000619                 24  X-M-AMTS    PIC X(24).                       00000611
000620     12  Y-FILLER1.                                               00000612
010508         16  Y-FILLER3       OCCURS 15  TIMES.                    00000613
000622             20  Y-FILLER4   OCCURS 100 TIMES.                    00000614
000623                 24  Y-M-AMTS    PIC X(24).                       00000615
000624                                                                  00000616
000625                                                                  00000617
000626 01  HEAD-1.                                                      00000618
000627     12  FILLER              PIC X(42)           VALUE SPACES.    00000619
000628     12  FILLER              PIC X(39)           VALUE            00000620
000629             'UNEARNED PREMIUM AND COMMISSION EXTRACT'.           00000621
CIDMOD     12  FILLER              PIC X(38)           VALUE SPACES.    00000622
000631     12  FILLER              PIC X(7)            VALUE 'ECS-083'. 00000623
000632                                                                  00000624
000633 01  HEAD-2.                                                      00000625
000634     12  FILLER              PIC X(47)           VALUE SPACES.    00000626
000635     12  HD-CLIENT           PIC X(30)           VALUE SPACES.    00000627
CIDMOD     12  FILLER              PIC X(42)           VALUE SPACES.    00000628
000637     12  HD-DATE             PIC X(8)            VALUE SPACES.    00000629
000638                                                                  00000630
000639 01  HEAD-3.                                                      00000631
000640     12  FILLER              PIC X(53)           VALUE SPACES.    00000632
000641     12  HD-ALF-DTE          PIC X(18)           VALUE SPACES.    00000633
CIDMOD     12  FILLER              PIC X(48)           VALUE SPACES.    00000634
000643     12  FILLER              PIC X(5)            VALUE 'PAGE'.    00000635
000644     12  HD-PAGE             PIC ZZ,ZZZ.                          00000636
000645                                                                  00000637
000646                             COPY ELCDTECX.                       00000638
CIDMOD                                                                  00000637
CIDMOD                             COPY ELCDTEVR.                       00000638
CIDMOD                                                                  00000637
CIDMOD                             COPY ELCGAPVR.                       00000638
CIDMOD                                                                  00000637
000648 EJECT                                                            00000640
000649 PROCEDURE DIVISION.                                              00000641
000650                                                                  00000642
000651 0100-SET-START.                                                  00000643
000652                             COPY ELCDTERX.                       00000644
000654 0110-OPEN-RTN.                                                   00000646
000655     OPEN INPUT  GAAP-EXTR                                        00000647
000656          OUTPUT REPTFL                                           00000648
000657                 ACT-RPT                                          00000649
000658                 FIN-RPT                                          00000650
000659                 PRNTR.                                           00000651

000661     MOVE ZEROS TO AH-SUB.                                        00000653
000662     MOVE ZEROS TO CA-SAVE-SUB.                                   00000654
000663     MOVE ZEROS TO CO-SAVE-SUB.                                   00000655
000664     MOVE ZEROS TO OR-SAVE-SUB.                                   00000656
000665     MOVE ZEROS TO WA-SAVE-SUB.                                   00000657
000666                                                                  00000658
000667 0120-ZERO-LOOP.                                                  00000659
000668                                                                  00000660
000669     ADD  1     TO AH-SUB.                                        00000661
000670                                                                  00000662
000671     IF  AH-SUB > 100                                             00000663
000672       MOVE ZEROS TO AH-SUB                                       00000664
000673         GO TO 0120-ZERO-LOOP-END.                                00000665
000674                                                                  00000666
000675     MOVE ZEROS  TO  CO-BEN-CODE (AH-SUB)                         00000667
000676     MOVE ZEROS  TO  CO-BEN-COUNT (AH-SUB)                        00000668
000677     MOVE ZEROS  TO  CO-BEN-R78 (AH-SUB)                          00000669
000678     MOVE ZEROS  TO  CO-BEN-PRAT(AH-SUB).                         00000670
000680                                                                  00000672
000675     MOVE ZEROS  TO  GA-BEN-CODE (AH-SUB)                         00000667
000676     MOVE ZEROS  TO  GA-BEN-COUNT (AH-SUB)                        00000668
000677     MOVE ZEROS  TO  GA-BEN-R78 (AH-SUB)                          00000669
000678     MOVE ZEROS  TO  GA-BEN-PRAT(AH-SUB).                         00000670
000680                                                                  00000672
000681     MOVE ZEROS  TO  CA-BEN-CODE (AH-SUB)                         00000673
000682     MOVE ZEROS  TO  CA-BEN-COUNT (AH-SUB)                        00000674
000683     MOVE ZEROS  TO  CA-BEN-R78 (AH-SUB)                          00000675
000684     MOVE ZEROS  TO  CA-BEN-PRAT(AH-SUB).                         00000676
000686                                                                  00000677
CIDMOD     MOVE ZEROS  TO  OH-BEN-CODE (AH-SUB)                         00000673
CIDMOD     MOVE ZEROS  TO  OH-BEN-COUNT (AH-SUB)                        00000674
CIDMOD     MOVE ZEROS  TO  OH-BEN-R78 (AH-SUB)                          00000675
CIDMOD     MOVE ZEROS  TO  OH-BEN-PRAT(AH-SUB).                         00000676
CIDMOD                                                                  00000677
000687     MOVE ZEROS  TO  OR-BEN-CODE (AH-SUB)                         00000678
000688     MOVE ZEROS  TO  OR-BEN-COUNT (AH-SUB)                        00000679
000689     MOVE ZEROS  TO  OR-BEN-R78 (AH-SUB)                          00000680
000690     MOVE ZEROS  TO  OR-BEN-PRAT (AH-SUB).                        00000681
000692                                                                  00000682
CIDMOD     MOVE ZEROS  TO  TX-BEN-CODE (AH-SUB)                         00000678
CIDMOD     MOVE ZEROS  TO  TX-BEN-COUNT (AH-SUB)                        00000679
CIDMOD     MOVE ZEROS  TO  TX-BEN-R78 (AH-SUB)                          00000680
CIDMOD     MOVE ZEROS  TO  TX-BEN-PRAT (AH-SUB).                        00000681
CIDMOD                                                                  00000682
000693     MOVE ZEROS  TO  WA-BEN-CODE (AH-SUB)                         00000683
000694     MOVE ZEROS  TO  WA-BEN-COUNT (AH-SUB)                        00000684
000695     MOVE ZEROS  TO  WA-BEN-R78 (AH-SUB)                          00000685
000696     MOVE ZEROS  TO  WA-BEN-PRAT (AH-SUB).                        00000686
000697                                                                  00000687
000698     GO TO 0120-ZERO-LOOP.                                        00000688
000699                                                                  00000689
000700 0120-ZERO-LOOP-END.                                              00000690
000702                                                                  00000691
000703     PERFORM 0500-PRINT-HEADINGS THRU 0510-HEADING-EXIT.          00000692
000704                                                                  00000693
000705 0120-INTL-RTN.                                                   00000694
000706                                                                  00000695
000707     IF CLAS-MAXM = ZEROS                                         00000696
000708         MOVE +1 TO CLAS-STARTM.                                  00000697
000709                                                                  00000698
000710     ADD +1 TO CLAS-MAXM.                                         00000699
000711                                                                  00000700
CIDMOD*    IF CLAS-MAXM GREATER THAN +60                                00000701
CIDMOD*        MOVE +60 TO CLAS-MAXM.                                   00000702
011719     IF CLAS-MAXM > +130
011719        MOVE +130                TO CLAS-MAXM
011719     end-if

000715     MOVE CLAS-MAXM TO X1.                                        00000704
000716     MOVE SPACES    TO CLAS-MORT-CODE (X1).                       00000705
000717     MOVE 'OTHERS'  TO CLAS-MORT-DESC (X1).                       00000706
000718                                                                  00000707
000719     MOVE HIGH-VALUES      TO XT-TABLE  YT-TABLE.                 00000708
000720     PERFORM 0330-PRESS-RTN.                                      00000709
000721                                                                  00000710
000722     MOVE +0 TO X2.                                               00000711
000723 0125-INTL-X2.                                                    00000712
000724     ADD +1 TO X2.                                                00000713
000725                                                                  00000714
010508     IF X2 GREATER THAN +15                                       00000715
000727         GO TO 0130-READ-RTN.                                     00000716
000728                                                                  00000717
000729     MOVE +0 TO X3.                                               00000718
000730                                                                  00000719
000731 0127-INTL-X3.                                                    00000720
000732     ADD +1 TO X3.                                                00000721
000733                                                                  00000722
000734     IF X3 GREATER THAN MAX-BEN                                   00000723
000735         GO TO 0125-INTL-X2.                                      00000724
000736                                                                  00000725
000737     MOVE X-DETL   TO X-AMTS (X2 X3)                              00000726
000738                      Y-AMTS (X2 X3).                             00000727
000739     MOVE X-M-DETL TO X-M-AMTS (X2 X3)                            00000728
000740                      Y-M-AMTS (X2 X3).                           00000729
000741                                                                  00000730
000742     GO TO 0127-INTL-X3.                                          00000731
000743 EJECT                                                            00000732
000744 0130-READ-RTN.                                                   00000733
000745     READ GAAP-EXTR AT END                                        00000734
000746         GO TO 0520-END-INPUT.                                    00000735
000747                                                                  00000736
000748     IF GR-REIN = 'P' OR 'R'                                      00000737
000749         NEXT SENTENCE                                            00000738
000750     ELSE                                                         00000739
000751        MOVE 0301 TO WS-RETURN-CODE                               00000740
000752        MOVE ' INVALID GAAP RECORD ' TO WS-ABEND-MESSAGE          00000741
000753        GO TO ABEND-PGM.                                          00000742
000754                                                                  00000743
CIDMOD     COPY  ELCGAPM1.                                              00000743
CIDMOD                                                                  00000743
000755     MOVE GR-CARRIER   TO CUR-CARR.                               00000744
000756     MOVE GR-GROUPING  TO CUR-CO.                                 00000745
000757     MOVE GR-STATE     TO CUR-ST.                                 00000746
000758                                                                  00000747
000759     IF CUR-SEQ NOT = PRE-SEQ                                     00000748
000760         PERFORM 0330-PRESS-RTN THRU 0380-PRESS-XIT.              00000749
000761                                                                  00000750
000762 0140-SET-X1.                                                     00000751
000763     IF GR-REIN = 'P'                                             00000752
000764         MOVE +1 TO X1                                            00000753
000765     ELSE                                                         00000754
000766         MOVE +2 TO X1.                                           00000755
000767                                                                  00000756
000768 0150-SET-X2.                                                     00000757
121610     MOVE GR-CCYY TO X-YR
121610*    MOVE GR-CC TO X-YR(1:2).                                     ECS083
121610*    MOVE GR-YR TO X-YR(3:2).                                     ECS083
000770*                                                                 00000759
CIDMOD*    COMPUTE X2 = (RUN-YR - X-YR) + +1.                           00000760
CIDMOD     COMPUTE X2 = (RUN-CCYY - X-YR) + +1.                         00000760
000772                                                                  00000761
010508     IF X2 GREATER THAN +15                                       00000762
010508         MOVE +15 TO X2.                                          00000763
000775                                                                  00000764
000776     IF X2 LESS THAN +1                                           00000765
000777         MOVE +1 TO X2.                                           00000766
000778                                                                  00000767
000779 0160-FIND-LIFE.                                                  00000768
000780     IF GR-LFTYP = ZEROS                                          00000769
000781         GO TO 0210-FIND-AH.                                      00000770
000782                                                                  00000771
000783     MOVE '1'         TO X-LAH.                                   00000772
000784     MOVE GR-LFTYP    TO X-BEN.                                   00000773
000785     MOVE CLAS-STARTM TO CLAS-INDEXM.                             00000774
000786                                                                  00000775
000787 0170-LOOP-CLAS-INDEXM.                                           00000776
000788     IF CLAS-MORT-CODE (CLAS-INDEXM) = GR-MORT-CODE OR            00000777
000789        CLAS-MORT-CODE (CLAS-INDEXM) = SPACES                     00000778
000790         MOVE CLAS-INDEXM TO X-MORT                               00000779
000791     ELSE                                                         00000780
000792         ADD +1           TO CLAS-INDEXM                          00000781
000793         GO TO 0170-LOOP-CLAS-INDEXM.                             00000782
000794                                                                  00000783
000795     IF GR-REIN = 'R'                                             00000784
000796         MOVE GR-REIN-COMP TO X-REIN                              00000785
000797     ELSE                                                         00000786
000798         MOVE LOW-VALUE    TO X-REIN.                             00000787
000799                                                                  00000788
000800     PERFORM 0300-LOCATE-X3 THRU 0320-LOCATE-X3-XIT.              00000789
000801                                                                  00000790
000802     IF X1 = +1                                                   00000791
000803         MOVE X-AMTS (X2 X3)   TO X-DETL                          00000792
000804         MOVE X-M-AMTS (X2 X3) TO X-M-DETL                        00000793
000805     ELSE                                                         00000794
000806         MOVE Y-AMTS (X2 X3)   TO X-DETL                          00000795
000807         MOVE Y-M-AMTS (X2 X3) TO X-M-DETL.                       00000796
000808                                                                  00000797
000809     IF GR-SUMMARY-REC                                            00000798
000810        ADD GR-CNT-LF TO X-COUNT                                  00000799
000811      ELSE                                                        00000800
000812        ADD +1        TO X-COUNT.                                 00000801
000813                                                                  00000802
000814     ADD GR-LFPRM     TO X-WRITTEN.                               00000803
000815     ADD GRR-LFPRM    TO X-P78.                                   00000804
000816     ADD GRP-LFPRM    TO X-PRATA.                                 00000805
040114     IF DTE-CLIENT = 'DCC'
040114        ADD GRD-LFPRM    TO X-DOMICILE
040114     ELSE
040114        ADD GRS-LFPRM    TO X-DOMICILE
040114     END-IF
000818     ADD GRS-LFPRM    TO X-STATE.                                 00000807
000819     ADD GR-LFCOM     TO X-PAID.                                  00000808
000820     ADD GRR-LFCOM    TO X-C78.                                   00000809
000821     ADD GRP-LFCOM    TO X-CRATA.                                 00000810
040114     ADD GRS-LFCOM    TO X-CDOMI.
000822     ADD GR-LFTAX     TO X-TAX.                                   00000811
000823     ADD GRR-LFTAX    TO X-T78.                                   00000812
000824     ADD GRP-LFTAX    TO X-TRATA.                                 00000813
040114     COMPUTE WRK-DOMI ROUNDED = 
040114               (GRS-LFPRM / GR-LFPRM) * GR-LFTAX.
040114     ADD WRK-DOMI     TO X-TDOMI.
000825                                                                  00000814
000826     IF DTE-CLIENT = 'HER'                                        00000815
000827         ADD GR-LFBEN     TO X-REMAIN                             00000816
000828     ELSE                                                         00000817
000829         ADD GR-REM-AMT   TO X-REMAIN.                            00000818
000830                                                                  00000819
000831     ADD GR-RESV      TO X-RESERV.                                00000820
000832     ADD GR-ALT-RESV  TO X-ALTRSV.                                00000821
000834                                                                  00000822
000835     IF GR-SUMMARY-REC                                            00000823
000836        GO TO  NO-SELECT-LF-ACCUM.                                00000824
000838                                                                  00000825
000839 SELECT-LF-ACCUM-DONE.                                            00000826
000840                                                                  00000827
000841     IF GR-REIN = 'P'                                             00000828
000842      ADD +1           TO  ALL-LF-COUNT                           00000829
000843      ADD GR-LFPRM     TO  ALL-LF-WRITTEN                         00000830
000844      ADD GRR-LFPRM    TO  ALL-LF-P78                             00000831
000845      ADD GRP-LFPRM    TO  ALL-LF-PRATA                           00000832
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-LFPRM    TO  ALL-LF-DOMICILE
040114      ELSE
040114         ADD GRS-LFPRM    TO  ALL-LF-DOMICILE
040114      END-IF
000847      ADD GRS-LFPRM    TO  ALL-LF-STATE                           00000834
000848      ADD GR-ALT-RESV  TO  ALL-LF-ALTRSV                          00000835
000849      ADD GR-RESV      TO  ALL-LF-RESERV                          00000836
000850      ADD GR-REM-AMT   TO  ALL-LF-REMAIN                          00000837
000851      ADD GR-LFCOM     TO  ALL-LF-PAID                            00000838
000852      ADD GRR-LFCOM    TO  ALL-LF-C78                             00000839
000853      ADD GRP-LFCOM    TO  ALL-LF-CRATA                           00000840
040114      ADD GRS-LFCOM    TO  ALL-LF-CDOMI
000854      ADD GR-LFTAX     TO  ALL-LF-TAX                             00000841
000855      ADD GRR-LFTAX    TO  ALL-LF-T78                             00000842
000856      ADD GRP-LFTAX    TO  ALL-LF-TRATA.                          00000843
040114      COMPUTE WRK-DOMI ROUNDED = 
040114                 (GRS-LFPRM / GR-LFPRM) * GR-LFTAX.
040114      ADD WRK-DOMI     TO  ALL-LF-TDOMI.
000857                                                                  00000844
000858                                                                  00000845
000859 NO-SELECT-LF-ACCUM.                                              00000846
000861                                                                  00000847
000862     IF GR-IG = '1'                                               00000848
000863         GO TO 0190-LIFE-IND.                                     00000849
000864                                                                  00000850
000865 0180-LIFE-GRP.                                                   00000851
000866     IF GR-LF-TERM LESS THAN +121                                 00000852
000867        ADD GR-RESV TO X-GUNDR                                    00000853
000868     ELSE                                                         00000854
000869        ADD GR-RESV TO X-GOVER.                                   00000855
000870                                                                  00000856
000871     GO TO 0200-END-LIFE.                                         00000857
000872                                                                  00000858
000873 0190-LIFE-IND.                                                   00000859
000874     IF GR-LF-TERM LESS THAN +121                                 00000860
000875        ADD GR-RESV TO X-IUNDR                                    00000861
000876     ELSE                                                         00000862
000877        ADD GR-RESV TO X-IOVER.                                   00000863
000878                                                                  00000864
000879 0200-END-LIFE.                                                   00000865
000880     IF GR-AHTYP NOT = ZEROS                                      00000866
000881         IF GR-SUMMARY-REC                                        00000867
000882             NEXT SENTENCE                                        00000868
000883          ELSE                                                    00000869
000884             ADD +1 TO X-DUP.                                     00000870
000885                                                                  00000871
000886     IF X1 = +1                                                   00000872
000887         MOVE X-DETL   TO X-AMTS (X2 X3)                          00000873
000888         MOVE X-M-DETL TO X-M-AMTS (X2 X3)                        00000874
000889     ELSE                                                         00000875
000890         MOVE X-DETL   TO Y-AMTS (X2 X3)                          00000876
000891         MOVE X-M-DETL TO Y-M-AMTS (X2 X3).                       00000877
000892                                                                  00000878
000893 0210-FIND-AH.                                                    00000879
000894     IF GR-AHTYP = ZEROS                                          00000880
000895         GO TO 0130-READ-RTN.                                     00000881
000896                                                                  00000882
000897     MOVE '2'      TO X-LAH.                                      00000883
000898     MOVE GR-AHTYP TO X-BEN.                                      00000884
000899     MOVE ZEROS    TO X-SEQ2.                                     00000885
000900                                                                  00000886
000901     IF GR-REIN = 'R'                                             00000887
000902         MOVE GR-REIN-COMP TO X-REIN                              00000888
000903     ELSE                                                         00000889
000904         MOVE LOW-VALUE    TO X-REIN.                             00000890
000905                                                                  00000891
000906     PERFORM 0300-LOCATE-X3 THRU 0320-LOCATE-X3-XIT.              00000892
000907                                                                  00000893
000908     IF X1 = +1                                                   00000894
000909         MOVE X-AMTS (X2 X3)   TO X-DETL                          00000895
000910         MOVE X-M-AMTS (X2 X3) TO X-M-DETL                        00000896
000911     ELSE                                                         00000897
000912         MOVE Y-AMTS (X2 X3)   TO X-DETL                          00000898
000913         MOVE Y-M-AMTS (X2 X3) TO X-M-DETL.                       00000899
000914                                                                  00000900
000915     IF GR-SUMMARY-REC                                            00000901
000916        ADD GR-CNT-AH TO X-COUNT                                  00000902
000917      ELSE                                                        00000903
000918        ADD +1        TO X-COUNT.                                 00000904
000919                                                                  00000905
000920     ADD GR-AHPRM     TO X-WRITTEN.                               00000906
000921     ADD GRR-AHPRM    TO X-P78.                                   00000907
000922     ADD GRP-AHPRM    TO X-PRATA.                                 00000908
040114     IF DTE-CLIENT = 'DCC'
040114        ADD GRD-AHPRM    TO X-DOMICILE
040114     ELSE
040114        COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114        ADD WRK-DOMI     TO X-DOMICILE
040114     END-IF.
120816     compute x-state = x-state +
120816        grs-ahprm + gr-loaded-stat-uep
000925     ADD GR-AHCOM     TO X-PAID.                                  00000911
000926     ADD GRR-AHCOM    TO X-C78.                                   00000912
000927     ADD GRP-AHCOM    TO X-CRATA.                                 00000913
040114     COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2.
040114     ADD WRK-DOMI     TO X-CDOMI.
000928     ADD GR-AHTAX     TO X-TAX.                                   00000914
000929     ADD GRR-AHTAX    TO X-T78.                                   00000915
000930     ADD GRP-AHTAX    TO X-TRATA.                                 00000916
040114     COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2.
040114     ADD WRK-DOMI     TO X-TDOMI.
000931                                                                  00000917
000932     IF DTE-CLIENT = 'HER'                                        00000918
000933         COMPUTE X-REMAIN ROUNDED EQUAL                           00000919
000934                 (GR-AHBEN * GR-AH-TERM) + X-REMAIN               00000920
000935     ELSE                                                         00000921
000936         COMPUTE X-REMAIN ROUNDED EQUAL                           00000922
000937                 (GR-AHBEN * GR-AH-REMTERM) + X-REMAIN.           00000923
000939                                                                  00000924
061402     if (gr-rein = 'P')
061402        and (gr-state = 'VA')
061402        and (gr-eff > con-20020630)
061402        ADD GRR-AHPRM    TO  VA-OVRALL-R78
061402        ADD GRP-AHPRM    TO  VA-OVRALL-PRAT
061402     end-if
061402       .
000940 CK-FOR-CO.                                                       00000925
000941                                                                  00000926
000942     IF GR-REIN =   'P'  AND                                      00000927
000943        GR-STATE = 'CO'                                           00000928
000944        NEXT SENTENCE                                             00000929
000945      ELSE                                                        00000930
000946        GO TO CK-FOR-GA.                                          00000931
000947                                                                  00000932
CIDMOD     IF GR-EFF  >  CON-19961231                                   00000933
000949        NEXT SENTENCE                                             00000934
000950      ELSE                                                        00000935
000951        GO TO CK-FOR-GA.                                          00000936
000952                                                                  00000937
000953      MOVE ZEROS         TO AH-SUB.                               00000938
000954      ADD 1              TO CO-COUNT.                             00000939
000955                                                                  00000940
000956 CO-TABLE-LOOP.                                                   00000941
000957                                                                  00000942
000958     ADD 1               TO AH-SUB.                               00000943
000959                                                                  00000944
000960     IF CO-BEN-CODE (AH-SUB) = ZEROS                              00000945
000961        ADD 1            TO  CO-SAVE-SUB                          00000946
000962        MOVE GR-AHTYP TO CO-BEN-CODE (AH-SUB).                    00000947
000963                                                                  00000948
000964     IF GR-AHTYP = CO-BEN-CODE (AH-SUB)                           00000949
000965        ADD 1            TO  CO-BEN-COUNT (AH-SUB)                00000950
000966        ADD GRR-AHPRM    TO  CO-BEN-R78 (AH-SUB)                  00000951
000967        ADD GRR-AHPRM    TO  CO-OVRALL-R78                        00000952
000968        ADD GRP-AHPRM    TO  CO-BEN-PRAT (AH-SUB)                 00000953
000969        ADD GRP-AHPRM    TO  CO-OVRALL-PRAT                       00000954
000970            GO TO  AH-CONTINUE                                    00000955
000971      ELSE                                                        00000956
000972            GO TO CO-TABLE-LOOP.                                  00000957
000974                                                                  00000958
000940 CK-FOR-GA.                                                       00000925
000941                                                                  00000926
000942     IF GR-REIN =   'P'  AND                                      00000927
000943        GR-STATE = 'GA'                                           00000928
000944        NEXT SENTENCE                                             00000929
000945      ELSE                                                        00000930
000946        GO TO CK-FOR-CA.                                          00000931
000947                                                                  00000932
000953      MOVE ZEROS         TO AH-SUB.                               00000938
000954      ADD 1              TO GA-COUNT.                             00000939
000955                                                                  00000940
000956 GA-TABLE-LOOP.                                                   00000941
000957                                                                  00000942
000958     ADD 1               TO AH-SUB.                               00000943
000959                                                                  00000944
000960     IF GA-BEN-CODE (AH-SUB) = ZEROS                              00000945
000961        ADD 1            TO  GA-SAVE-SUB                          00000946
000962        MOVE GR-AHTYP TO GA-BEN-CODE (AH-SUB).                    00000947
000963                                                                  00000948
000964     IF GR-AHTYP = GA-BEN-CODE (AH-SUB)                           00000949
000965        ADD 1            TO  GA-BEN-COUNT (AH-SUB)                00000950
000966        ADD GRR-AHPRM    TO  GA-BEN-R78 (AH-SUB)                  00000951
000967        ADD GRR-AHPRM    TO  GA-OVRALL-R78                        00000952
000968        ADD GRP-AHPRM    TO  GA-BEN-PRAT (AH-SUB)                 00000953
000969        ADD GRP-AHPRM    TO  GA-OVRALL-PRAT                       00000954
000970            GO TO  AH-CONTINUE                                    00000955
000971      ELSE                                                        00000956
000972            GO TO GA-TABLE-LOOP.                                  00000957
000974                                                                  00000958
000975 CK-FOR-CA.                                                       00000959
000977                                                                  00000960
000978     IF GR-REIN =   'P'  AND                                      00000961
000979        GR-STATE = 'CA'                                           00000962
000980        NEXT SENTENCE                                             00000963
000981      ELSE                                                        00000964
CIDMOD        GO TO CK-FOR-OH.                                          00000965
000983                                                                  00000966
000984      MOVE ZEROS         TO AH-SUB.                               00000967
000985      ADD 1              TO CA-COUNT.                             00000968
000986                                                                  00000969
000987 CA-TABLE-LOOP.                                                   00000970
000988                                                                  00000971
000989     ADD 1               TO AH-SUB.                               00000972
000990                                                                  00000973
000991     IF CA-BEN-CODE (AH-SUB) = ZEROS                              00000974
000992        ADD 1            TO  CA-SAVE-SUB                          00000975
000993        MOVE GR-AHTYP TO CA-BEN-CODE (AH-SUB).                    00000976
000994                                                                  00000977
000995     IF GR-AHTYP = CA-BEN-CODE (AH-SUB)                           00000978
000996        ADD 1            TO  CA-BEN-COUNT (AH-SUB)                00000979
000997        ADD GRR-AHPRM    TO  CA-BEN-R78 (AH-SUB)                  00000980
000998        ADD GRP-AHPRM    TO  CA-BEN-PRAT (AH-SUB)                 00000981
000999            GO TO  AH-CONTINUE                                    00000982
001000      ELSE                                                        00000983
001001            GO TO CA-TABLE-LOOP.                                  00000984
001004                                                                  00000985
CIDMOD CK-FOR-OH.                                                       00000959
CIDMOD                                                                  00000960
CIDMOD     IF GR-REIN =   'P'  AND                                      00000961
CIDMOD        GR-STATE = 'OH'                                           00000962
CIDMOD        NEXT SENTENCE                                             00000963
CIDMOD      ELSE                                                        00000964
CIDMOD        GO TO CK-FOR-OR.                                          00000965
CIDMOD                                                                  00000966
CIDMOD      MOVE ZEROS         TO AH-SUB.                               00000967
CIDMOD      ADD 1              TO OH-COUNT.                             00000968
CIDMOD                                                                  00000969
CIDMOD OH-TABLE-LOOP.                                                   00000970
CIDMOD                                                                  00000971
CIDMOD     ADD 1               TO AH-SUB.                               00000972
CIDMOD                                                                  00000973
CIDMOD     IF OH-BEN-CODE (AH-SUB) = ZEROS                              00000974
CIDMOD        ADD 1            TO  OH-SAVE-SUB                          00000975
CIDMOD        MOVE GR-AHTYP TO OH-BEN-CODE (AH-SUB).                    00000976
CIDMOD                                                                  00000977
CIDMOD     IF GR-AHTYP = OH-BEN-CODE (AH-SUB)                           00000978
CIDMOD        ADD 1            TO  OH-BEN-COUNT (AH-SUB)                00000979
CIDMOD        ADD GRR-AHPRM    TO  OH-BEN-R78 (AH-SUB)                  00000980
CIDMOD        ADD GRR-AHPRM    TO  OH-OVRALL-R78                        00000952
CIDMOD        ADD GRP-AHPRM    TO  OH-BEN-PRAT (AH-SUB)                 00000981
CIDMOD        ADD GRP-AHPRM    TO  OH-OVRALL-PRAT                       00000954
CIDMOD            GO TO  AH-CONTINUE                                    00000982
CIDMOD      ELSE                                                        00000983
CIDMOD            GO TO OH-TABLE-LOOP.                                  00000984
CIDMOD                                                                  00000985
001005 CK-FOR-OR.                                                       00000986
001006                                                                  00000987
001007     IF GR-REIN =   'P'  AND                                      00000988
001008        GR-STATE = 'OR'                                           00000989
001009        NEXT SENTENCE                                             00000990
001010      ELSE                                                        00000991
CIDMOD        GO TO CK-FOR-TX.                                          00000992
001012                                                                  00000993
001013      MOVE ZEROS         TO AH-SUB.                               00000994
001014      ADD 1              TO OR-COUNT.                             00000995
001015                                                                  00000996
001016 OR-TABLE-LOOP.                                                   00000997
001017                                                                  00000998
001018     ADD 1               TO AH-SUB.                               00000999
001019                                                                  00001000
001020     IF OR-BEN-CODE (AH-SUB) = ZEROS                              00001001
001021        ADD 1            TO  OR-SAVE-SUB                          00001002
001022        MOVE GR-AHTYP TO OR-BEN-CODE (AH-SUB).                    00001003
001023                                                                  00001004
001024     IF GR-AHTYP = OR-BEN-CODE (AH-SUB)                           00001005
001025        ADD 1            TO  OR-BEN-COUNT (AH-SUB)                00001006
001026        ADD GRR-AHPRM    TO  OR-BEN-R78 (AH-SUB)                  00001007
001027        ADD GRP-AHPRM    TO  OR-BEN-PRAT (AH-SUB)                 00001008
001028            GO TO  AH-CONTINUE                                    00001009
001029      ELSE                                                        00001010
001030            GO TO OR-TABLE-LOOP.                                  00001011
001032                                                                  00001012
CIDMOD CK-FOR-TX.                                                       00000986
CIDMOD                                                                  00000987
CIDMOD     IF GR-REIN =   'P'  AND                                      00000988
CIDMOD        GR-STATE = 'TX'                                           00000989
CIDMOD        NEXT SENTENCE                                             00000990
CIDMOD      ELSE                                                        00000991
CIDMOD        GO TO CK-FOR-WA.                                          00000992
CIDMOD                                                                  00000993
CIDMOD      MOVE ZEROS         TO AH-SUB.                               00000994
CIDMOD      ADD 1              TO TX-COUNT.                             00000995
CIDMOD                                                                  00000996
CIDMOD TX-TABLE-LOOP.                                                   00000997
CIDMOD                                                                  00000998
CIDMOD     ADD 1               TO AH-SUB.                               00000999
CIDMOD                                                                  00001000
CIDMOD     IF TX-BEN-CODE (AH-SUB) = ZEROS                              00001001
CIDMOD        ADD 1            TO  TX-SAVE-SUB                          00001002
CIDMOD        MOVE GR-AHTYP TO TX-BEN-CODE (AH-SUB).                    00001003
CIDMOD                                                                  00001004
CIDMOD     IF GR-AHTYP = TX-BEN-CODE (AH-SUB)                           00001005
CIDMOD        ADD 1            TO  TX-BEN-COUNT (AH-SUB)                00001006
CIDMOD        ADD GRR-AHPRM    TO  TX-BEN-R78 (AH-SUB)                  00001007
CIDMOD        ADD GRR-AHPRM    TO  TX-OVRALL-R78                        00000952
CIDMOD        ADD GRP-AHPRM    TO  TX-BEN-PRAT (AH-SUB)                 00001008
CIDMOD        ADD GRP-AHPRM    TO  TX-OVRALL-PRAT                       00000954
CIDMOD            GO TO  AH-CONTINUE                                    00001009
CIDMOD      ELSE                                                        00001010
CIDMOD            GO TO TX-TABLE-LOOP.                                  00001011
CIDMOD                                                                  00001012
001033 CK-FOR-WA.                                                       00001013
001034                                                                  00001014
001035     IF GR-REIN =   'P'  AND                                      00001015
001036        GR-STATE = 'WA'                                           00001016
001037        NEXT SENTENCE                                             00001017
001038      ELSE                                                        00001018
001039        GO TO AH-CONTINUE.                                        00001019
001040                                                                  00001020
001041      MOVE ZEROS         TO AH-SUB.                               00001021
001042      ADD 1              TO WA-COUNT.                             00001022
001043                                                                  00001023
001044 WA-TABLE-LOOP.                                                   00001024
001045                                                                  00001025
001046     ADD 1               TO AH-SUB.                               00001026
001047                                                                  00001027
001048     IF WA-BEN-CODE (AH-SUB) = ZEROS                              00001028
001049        ADD 1            TO  WA-SAVE-SUB                          00001029
001050        MOVE GR-AHTYP TO WA-BEN-CODE (AH-SUB).                    00001030
001051                                                                  00001031
001052     IF GR-AHTYP = WA-BEN-CODE (AH-SUB)                           00001032
001053        ADD 1            TO  WA-BEN-COUNT (AH-SUB)                00001033
001054        ADD GRR-AHPRM    TO  WA-BEN-R78 (AH-SUB)                  00001034
001055        ADD GRP-AHPRM    TO  WA-BEN-PRAT (AH-SUB)                 00001035
001056            GO TO  AH-CONTINUE                                    00001036
001057      ELSE                                                        00001037
001058            GO TO WA-TABLE-LOOP.                                  00001038
001061                                                                  00001039
001062 AH-CONTINUE.                                                     00001040
001063                                                                  00001041
001064     IF GR-REIN =   'P'  AND                                      00001042
001065        GR-STATE = 'CO'                                           00001043
001066      ADD +1           TO  CO-AH-COUNT                            00001044
001067      ADD GR-AHPRM     TO  CO-AH-WRITTEN                          00001045
001068      ADD GRR-AHPRM    TO  CO-AH-P78                              00001046
001069      ADD GRR-AHPRM    TO  TOT-CO-R78                             00001047
001070      ADD GRP-AHPRM    TO  CO-AH-PRATA                            00001048
001071      ADD GRP-AHPRM    TO  TOT-CO-PRAT                            00001049
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  CO-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  CO-AH-DOMICILE
040114      END-IF
120816      compute co-ah-state = co-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
001073*     ADD GRS-AHPRM    TO  CO-AH-STATE                            00001051
001074      ADD GR-ALT-RESV  TO  CO-AH-ALTRSV                           00001052
001075      ADD GR-RESV      TO  CO-AH-RESERV                           00001053
001076      ADD GR-REM-AMT   TO  CO-AH-REMAIN                           00001054
001077      ADD GR-AHCOM     TO  CO-AH-PAID                             00001055
001078      ADD GRR-AHCOM    TO  CO-AH-C78                              00001056
001079      ADD GRP-AHCOM    TO  CO-AH-CRATA                            00001057
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  CO-AH-CDOMI
001080      ADD GR-AHTAX     TO  CO-AH-TAX                              00001058
001081      ADD GRR-AHTAX    TO  CO-AH-T78                              00001059
001082      ADD GRP-AHTAX    TO  CO-AH-TRATA                            00001060
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  CO-AH-TDOMI
001083          GO TO SELECT-AH-ACCUM-DONE.                             00001061
001084                                                                  00001062
001064     IF GR-REIN =   'P'  AND                                      00001042
001065        GR-STATE = 'GA'                                           00001043
001066      ADD +1           TO  GA-AH-COUNT                            00001044
001067      ADD GR-AHPRM     TO  GA-AH-WRITTEN                          00001045
001068      ADD GRR-AHPRM    TO  GA-AH-P78                              00001046
001069      ADD GRR-AHPRM    TO  TOT-GA-R78                             00001047
001070      ADD GRP-AHPRM    TO  GA-AH-PRATA                            00001048
001071      ADD GRP-AHPRM    TO  TOT-GA-PRAT                            00001049
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  GA-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  GA-AH-DOMICILE
040114      END-IF
120816      compute ga-ah-state = ga-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
001073*     ADD GRS-AHPRM    TO  GA-AH-STATE                            00001051
001074      ADD GR-ALT-RESV  TO  GA-AH-ALTRSV                           00001052
001075      ADD GR-RESV      TO  GA-AH-RESERV                           00001053
001076      ADD GR-REM-AMT   TO  GA-AH-REMAIN                           00001054
001077      ADD GR-AHCOM     TO  GA-AH-PAID                             00001055
001078      ADD GRR-AHCOM    TO  GA-AH-C78                              00001056
001079      ADD GRP-AHCOM    TO  GA-AH-CRATA                            00001057
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  GA-AH-CDOMI
001080      ADD GR-AHTAX     TO  GA-AH-TAX                              00001058
001081      ADD GRR-AHTAX    TO  GA-AH-T78                              00001059
001082      ADD GRP-AHTAX    TO  GA-AH-TRATA                            00001060
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  GA-AH-TDOMI
001083          GO TO SELECT-AH-ACCUM-DONE.                             00001061
001084                                                                  00001062
001086     IF GR-REIN =   'P'  AND                                      00001063
001087        GR-STATE = 'CA'                                           00001064
001088      ADD +1           TO  CA-AH-COUNT                            00001065
001089      ADD GR-AHPRM     TO  CA-AH-WRITTEN                          00001066
001090      ADD GRR-AHPRM    TO  CA-AH-P78                              00001067
CIDMOD      ADD GRR-AHPRM    TO  CA-OH-OR-TX-WA-R78                     00001068
001092      ADD GRP-AHPRM    TO  CA-AH-PRATA                            00001069
CIDMOD      ADD GRP-AHPRM    TO  CA-OH-OR-TX-WA-PRAT                    00001070
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  CA-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  CA-AH-DOMICILE
040114      END-IF
120816      compute ca-ah-state = ca-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
001095*     ADD GRS-AHPRM    TO  CA-AH-STATE                            00001072
001096      ADD GR-ALT-RESV  TO  CA-AH-ALTRSV                           00001073
001097      ADD GR-RESV      TO  CA-AH-RESERV                           00001074
001098      ADD GR-REM-AMT   TO  CA-AH-REMAIN                           00001075
001099      ADD GR-AHCOM     TO  CA-AH-PAID                             00001076
001100      ADD GRR-AHCOM    TO  CA-AH-C78                              00001077
001101      ADD GRP-AHCOM    TO  CA-AH-CRATA                            00001078
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  CA-AH-CDOMI
001102      ADD GR-AHTAX     TO  CA-AH-TAX                              00001079
001103      ADD GRR-AHTAX    TO  CA-AH-T78                              00001080
001104      ADD GRP-AHTAX    TO  CA-AH-TRATA                            00001081
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  CA-AH-TDOMI
001105          GO TO SELECT-AH-ACCUM-DONE.                             00001082
001106                                                                  00001083
CIDMOD     IF GR-REIN =   'P'  AND                                      00001063
CIDMOD        GR-STATE = 'OH'                                           00001064
CIDMOD      ADD +1           TO  OH-AH-COUNT                            00001065
CIDMOD      ADD GR-AHPRM     TO  OH-AH-WRITTEN                          00001066
CIDMOD      ADD GRR-AHPRM    TO  OH-AH-P78                              00001067
CIDMOD      ADD GRR-AHPRM    TO  CA-OH-OR-TX-WA-R78                     00001068
CIDMOD      ADD GRP-AHPRM    TO  OH-AH-PRATA                            00001069
CIDMOD      ADD GRP-AHPRM    TO  CA-OH-OR-TX-WA-PRAT                    00001070
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  OH-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  OH-AH-DOMICILE
040114      END-IF
120816      compute oh-ah-state = oh-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
CIDMOD*     ADD GRS-AHPRM    TO  OH-AH-STATE                            00001072
CIDMOD      ADD GR-ALT-RESV  TO  OH-AH-ALTRSV                           00001073
CIDMOD      ADD GR-RESV      TO  OH-AH-RESERV                           00001074
CIDMOD      ADD GR-REM-AMT   TO  OH-AH-REMAIN                           00001075
CIDMOD      ADD GR-AHCOM     TO  OH-AH-PAID                             00001076
CIDMOD      ADD GRR-AHCOM    TO  OH-AH-C78                              00001077
CIDMOD      ADD GRP-AHCOM    TO  OH-AH-CRATA                            00001078
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  OH-AH-CDOMI
CIDMOD      ADD GR-AHTAX     TO  OH-AH-TAX                              00001079
CIDMOD      ADD GRR-AHTAX    TO  OH-AH-T78                              00001080
CIDMOD      ADD GRP-AHTAX    TO  OH-AH-TRATA                            00001081
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  OH-AH-TDOMI
CIDMOD          GO TO SELECT-AH-ACCUM-DONE.                             00001082
CIDMOD                                                                  00001083
001107     IF GR-REIN =   'P'  AND                                      00001084
001108        GR-STATE = 'OR'                                           00001085
001109      ADD +1           TO  OR-AH-COUNT                            00001086
001110      ADD GR-AHPRM     TO  OR-AH-WRITTEN                          00001087
001111      ADD GRR-AHPRM    TO  OR-AH-P78                              00001088
CIDMOD      ADD GRR-AHPRM    TO  CA-OH-OR-TX-WA-R78                     00001089
001113      ADD GRP-AHPRM    TO  OR-AH-PRATA                            00001090
CIDMOD      ADD GRP-AHPRM    TO  CA-OH-OR-TX-WA-PRAT                    00001091
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  OR-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  OR-AH-DOMICILE
040114      END-IF
120816      compute or-ah-state = or-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
001116*     ADD GRS-AHPRM    TO  OR-AH-STATE                            00001093
001117      ADD GR-ALT-RESV  TO  OR-AH-ALTRSV                           00001094
001118      ADD GR-RESV      TO  OR-AH-RESERV                           00001095
001119      ADD GR-REM-AMT   TO  OR-AH-REMAIN                           00001096
001120      ADD GR-AHCOM     TO  OR-AH-PAID                             00001097
001121      ADD GRR-AHCOM    TO  OR-AH-C78                              00001098
001122      ADD GRP-AHCOM    TO  OR-AH-CRATA                            00001099
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  OR-AH-CDOMI
001123      ADD GR-AHTAX     TO  OR-AH-TAX                              00001100
001124      ADD GRR-AHTAX    TO  OR-AH-T78                              00001101
001125      ADD GRP-AHTAX    TO  OR-AH-TRATA                            00001102
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  OR-AH-TDOMI
001126           GO TO SELECT-AH-ACCUM-DONE.                            00001103
001127                                                                  00001104
CIDMOD     IF GR-REIN =   'P'  AND                                      00001084
CIDMOD        GR-STATE = 'TX'                                           00001085
CIDMOD      ADD +1           TO  TX-AH-COUNT                            00001086
CIDMOD      ADD GR-AHPRM     TO  TX-AH-WRITTEN                          00001087
CIDMOD      ADD GRR-AHPRM    TO  TX-AH-P78                              00001088
CIDMOD      ADD GRR-AHPRM    TO  CA-OH-OR-TX-WA-R78                     00001089
CIDMOD      ADD GRP-AHPRM    TO  TX-AH-PRATA                            00001090
CIDMOD      ADD GRP-AHPRM    TO  CA-OH-OR-TX-WA-PRAT                    00001091
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  TX-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  TX-AH-DOMICILE
040114      END-IF
120816      compute tx-ah-state = tx-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
CIDMOD*     ADD GRS-AHPRM    TO  TX-AH-STATE                            00001093
CIDMOD      ADD GR-ALT-RESV  TO  TX-AH-ALTRSV                           00001094
CIDMOD      ADD GR-RESV      TO  TX-AH-RESERV                           00001095
CIDMOD      ADD GR-REM-AMT   TO  TX-AH-REMAIN                           00001096
CIDMOD      ADD GR-AHCOM     TO  TX-AH-PAID                             00001097
CIDMOD      ADD GRR-AHCOM    TO  TX-AH-C78                              00001098
CIDMOD      ADD GRP-AHCOM    TO  TX-AH-CRATA                            00001099
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  TX-AH-CDOMI
CIDMOD      ADD GR-AHTAX     TO  TX-AH-TAX                              00001100
CIDMOD      ADD GRR-AHTAX    TO  TX-AH-T78                              00001101
CIDMOD      ADD GRP-AHTAX    TO  TX-AH-TRATA                            00001102
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  TX-AH-TDOMI
CIDMOD           GO TO SELECT-AH-ACCUM-DONE.                            00001103
CIDMOD                                                                  00001104
001128     IF GR-REIN =   'P' AND                                       00001105
001129        GR-STATE = 'WA'                                           00001106
001130      ADD +1           TO  WA-AH-COUNT                            00001107
001131      ADD GR-AHPRM     TO  WA-AH-WRITTEN                          00001108
001132      ADD GRR-AHPRM    TO  WA-AH-P78                              00001109
CIDMOD      ADD GRR-AHPRM    TO  CA-OH-OR-TX-WA-R78                     00001110
001134      ADD GRP-AHPRM    TO  WA-AH-PRATA                            00001111
CIDMOD      ADD GRP-AHPRM    TO  CA-OH-OR-TX-WA-PRAT                    00001112
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  WA-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  WA-AH-DOMICILE
040114      END-IF
120816      compute wa-ah-state = wa-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
001137*     ADD GRS-AHPRM    TO  WA-AH-STATE                            00001114
001138      ADD GR-ALT-RESV  TO  WA-AH-ALTRSV                           00001115
001139      ADD GR-RESV      TO  WA-AH-RESERV                           00001116
001140      ADD GR-REM-AMT   TO  WA-AH-REMAIN                           00001117
001141      ADD GR-AHCOM     TO  WA-AH-PAID                             00001118
001142      ADD GRR-AHCOM    TO  WA-AH-C78                              00001119
001143      ADD GRP-AHCOM    TO  WA-AH-CRATA                            00001120
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  WA-AH-CDOMI
001144      ADD GR-AHTAX     TO  WA-AH-TAX                              00001121
001145      ADD GRR-AHTAX    TO  WA-AH-T78                              00001122
001146      ADD GRP-AHTAX    TO  WA-AH-TRATA                            00001123
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  WA-AH-TDOMI
001147           GO TO SELECT-AH-ACCUM-DONE.                            00001124
001148                                                                  00001125
001149 SELECT-AH-ACCUM-DONE.                                            00001126
001150                                                                  00001127
001151     IF GR-REIN =   'P'                                           00001128
001152      ADD +1           TO  ALL-AH-COUNT                           00001129
001153      ADD GR-AHPRM     TO  ALL-AH-WRITTEN                         00001130
001154      ADD GRR-AHPRM    TO  ALL-AH-P78                             00001131
001155      ADD GRP-AHPRM    TO  ALL-AH-PRATA                           00001132
040114      IF DTE-CLIENT = 'DCC'
040114         ADD GRD-AHPRM    TO  ALL-AH-DOMICILE
040114      ELSE
040114         COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114         ADD WRK-DOMI     TO  ALL-AH-DOMICILE
040114      END-IF
120816      compute all-ah-state = all-ah-state +
120816        grs-ahprm + gr-loaded-stat-uep
001157*     ADD GRS-AHPRM    TO  ALL-AH-STATE                           00001134
001158      ADD GR-ALT-RESV  TO  ALL-AH-ALTRSV                          00001135
001159      ADD GR-RESV      TO  ALL-AH-RESERV                          00001136
001160      ADD GR-REM-AMT   TO  ALL-AH-REMAIN                          00001137
001161      ADD GR-AHCOM     TO  ALL-AH-PAID                            00001138
001162      ADD GRR-AHCOM    TO  ALL-AH-C78                             00001139
001163      ADD GRP-AHCOM    TO  ALL-AH-CRATA                           00001140
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2
040114      ADD WRK-DOMI     TO  ALL-AH-CDOMI
001164      ADD GR-AHTAX     TO  ALL-AH-TAX                             00001141
001165      ADD GRR-AHTAX    TO  ALL-AH-T78                             00001142
040114      COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2
040114      ADD WRK-DOMI     TO  ALL-AH-TDOMI
001166      ADD GRP-AHTAX    TO  ALL-AH-TRATA.                          00001143
001167                                                                  00001144
001168 NO-SELECT-AH-ACCUM.                                              00001145
001169                                                                  00001146
001170     IF X1 = +1                                                   00001147
001171         MOVE X-DETL   TO X-AMTS (X2 X3)                          00001148
001172         MOVE X-M-DETL TO X-M-AMTS (X2 X3)                        00001149
001173     ELSE                                                         00001150
001174         MOVE X-DETL   TO Y-AMTS (X2 X3)                          00001151
001175         MOVE X-M-DETL TO Y-M-AMTS (X2 X3).                       00001152
001176                                                                  00001153
001177     GO TO 0130-READ-RTN.                                         00001154
001178                                                                  00001155
001179 EJECT                                                            00001156
001180 0300-LOCATE-X3.                                                  00001157
001181     MOVE +0 TO X3.                                               00001158
001182                                                                  00001159
001183 0310-LOOP-LX3.                                                   00001160
001184     ADD +1 TO X3.                                                00001161
001185                                                                  00001162
001186     IF X3 GREATER THAN MAX-BEN                                   00001163
001187         PERFORM 0390-PRESS-ONE-RTN THRU 0420-PRESS-ONE-XIT.      00001164
001188                                                                  00001165
001189     IF X1 = +1                                                   00001166
001190       IF XT-SEQ (X2 X3) = HIGH-VALUE  OR                         00001167
001191          XT-SEQ (X2 X3) = X-SEQ                                  00001168
001192           MOVE X-SEQ TO XT-SEQ (X2 X3)                           00001169
001193           GO TO 0320-LOCATE-X3-XIT.                              00001170
001194                                                                  00001171
001195     IF X1 = +2                                                   00001172
001196       IF YT-SEQ (X2 X3) = HIGH-VALUE  OR                         00001173
001197          YT-SEQ (X2 X3) = X-SEQ                                  00001174
001198           MOVE X-SEQ TO YT-SEQ (X2 X3)                           00001175
001199           GO TO 0320-LOCATE-X3-XIT.                              00001176
001200                                                                  00001177
001201     GO TO 0310-LOOP-LX3.                                         00001178
001202                                                                  00001179
001203 0320-LOCATE-X3-XIT.                                              00001180
001204     EXIT.                                                        00001181
001205                                                                  00001182
001206 0330-PRESS-RTN.                                                  00001183
001207     MOVE +0 TO X1                                                00001184
001208                X-DUP                                             00001185
001209                X-COUNT                                           00001186
001210                X-WRITTEN                                         00001187
001211                X-P78                                             00001188
001212                X-PRATA                                           00001189
001213                X-DOMICILE                                        00001190
001214                X-STATE                                           00001191
001215                X-RESERV                                          00001192
001216                X-ALTRSV                                          00001193
001217                X-REMAIN                                          00001194
001218                X-PAID                                            00001195
001219                X-C78                                             00001196
001220                X-CRATA                                           00001197
040114                X-CDOMI
001221                X-TAX                                             00001198
001222                X-T78                                             00001199
001223                X-TRATA                                           00001200
040114                X-TDOMI
001224                X-IUNDR                                           00001201
001225                X-IOVER                                           00001202
001226                X-GUNDR                                           00001203
001227                X-GOVER.                                          00001204
001228                                                                  00001205
001229 0340-PRESS-LOOP-X1.                                              00001206
001230     ADD +1 TO X1.                                                00001207
001231                                                                  00001208
001232     IF X1 GREATER THAN +2                                        00001209
001233         GO TO 0370-INTL-PRESS.                                   00001210
001234                                                                  00001211
001235     MOVE +0 TO X2.                                               00001212
001236                                                                  00001213
001237 0350-PRESS-LOOP-X2.                                              00001214
001238     ADD +1 TO X2.                                                00001215
001239                                                                  00001216
010508     IF X2 GREATER THAN +15                                       00001217
001241         GO TO 0340-PRESS-LOOP-X1.                                00001218
001242                                                                  00001219
001243     MOVE +0 TO X3.                                               00001220
001244                                                                  00001221
001245 0360-PRESS-LOOP-X3.                                              00001222
001246     ADD +1 TO X3.                                                00001223
001247                                                                  00001224
001248     IF X3 GREATER THAN MAX-BEN                                   00001225
001249         GO TO 0350-PRESS-LOOP-X2.                                00001226
001250                                                                  00001227
001251     IF X1 = +1                                                   00001228
001252       IF XT-SEQ (X2 X3) = HIGH-VALUE                             00001229
001253           GO TO 0350-PRESS-LOOP-X2                               00001230
001254       ELSE                                                       00001231
001255           MOVE X-AMTS (X2 X3)       TO W-AMTS                    00001232
001256           IF W-COUNT NOT = +0                                    00001233
001257               MOVE XT-SEQ (X2 X3)   TO W-SEQ3                    00001234
001258               MOVE X-M-AMTS (X2 X3) TO W-M-AMTS                  00001235
001259               MOVE XT-SEQ3 (X2 X3)  TO PRE-REIN                  00001236
001260           ELSE                                                   00001237
001261               GO TO 0365-ZERO-PRESS.                             00001238
001262                                                                  00001239
001263     IF X1 = +2                                                   00001240
001264       IF YT-SEQ (X2 X3) = HIGH-VALUE                             00001241
001265           GO TO 0350-PRESS-LOOP-X2                               00001242
001266       ELSE                                                       00001243
001267           MOVE Y-AMTS (X2 X3)       TO W-AMTS                    00001244
001268           IF W-COUNT NOT = +0                                    00001245
001269               MOVE YT-SEQ (X2 X3)   TO W-SEQ3                    00001246
001270               MOVE Y-M-AMTS (X2 X3) TO W-M-AMTS                  00001247
001271               MOVE YT-SEQ3 (X2 X3)  TO PRE-REIN                  00001248
001272           ELSE                                                   00001249
001273               GO TO 0365-ZERO-PRESS.                             00001250
001274                                                                  00001251
001275     MOVE LOW-VALUE TO W-REIN.                                    00001252
001276     MOVE PRE-SEQ   TO W-SEQ1.                                    00001253
001277     MOVE X1        TO W-CODE.                                    00001254
001278     MOVE X2        TO W-YR.                                      00001255
001279                                                                  00001256
001280     PERFORM 0430-GEN-RTN THRU 0450-GEN-XIT.                      00001257
001281                                                                  00001258
001282 0365-ZERO-PRESS.                                                 00001259
001283                                                                  00001260
001284     IF X1 = +1                                                   00001261
001285         MOVE HIGH-VALUE TO XT-SEQ (X2 X3)                        00001262
001286         MOVE X-DETL     TO X-AMTS (X2 X3)                        00001263
001287         MOVE X-M-DETL   TO X-M-AMTS (X2 X3)                      00001264
001288     ELSE                                                         00001265
001289         MOVE HIGH-VALUE TO YT-SEQ (X2 X3)                        00001266
001290         MOVE X-DETL     TO Y-AMTS (X2 X3)                        00001267
001291         MOVE X-M-DETL   TO Y-M-AMTS (X2 X3).                     00001268
001292                                                                  00001269
001293     GO TO 0360-PRESS-LOOP-X3.                                    00001270
001294                                                                  00001271
001295 0370-INTL-PRESS.                                                 00001272
001296     MOVE CUR-SEQ TO PRE-SEQ.                                     00001273
001297                                                                  00001274
001298 0380-PRESS-XIT.                                                  00001275
001299     EXIT.                                                        00001276
001300                                                                  00001277
001301                                                                  00001278
001302 0390-PRESS-ONE-RTN.                                              00001279
001303     MOVE +0 TO X3                                                00001280
001304                X-DUP                                             00001281
001305                X-COUNT                                           00001282
001306                X-WRITTEN                                         00001283
001307                X-P78                                             00001284
001308                X-PRATA                                           00001285
001309                X-DOMICILE                                        00001286
001310                X-STATE                                           00001287
001311                X-RESERV                                          00001288
001312                X-ALTRSV                                          00001289
001313                X-REMAIN                                          00001290
001314                X-PAID                                            00001291
001315                X-C78                                             00001292
001316                X-CRATA                                           00001293
040114                X-CDOMI
001317                X-TAX                                             00001294
001318                X-T78                                             00001295
001319                X-TRATA                                           00001296
040114                X-TDOMI
001320                X-IUNDR                                           00001297
001321                X-IOVER                                           00001298
001322                X-GUNDR                                           00001299
001323                X-GOVER.                                          00001300
001324                                                                  00001301
001325 0400-PRESS-ONE-LOOP.                                             00001302
001326     ADD +1 TO X3.                                                00001303
001327                                                                  00001304
001328     IF X3 GREATER THAN MAX-BEN                                   00001305
001329         GO TO 0410-PRESS-ONE-END.                                00001306
001330                                                                  00001307
001331     MOVE LOW-VALUE TO W-REIN.                                    00001308
001332     MOVE PRE-SEQ   TO W-SEQ1.                                    00001309
001333     MOVE X1        TO W-CODE.                                    00001310
001334     MOVE X2        TO W-YR.                                      00001311
001335                                                                  00001312
001336     IF X1 = +1                                                   00001313
001337         MOVE XT-SEQ (X2 X3)   TO W-SEQ3                          00001314
001338         MOVE X-AMTS (X2 X3)   TO W-AMTS                          00001315
001339         MOVE X-M-AMTS (X2 X3) TO W-M-AMTS                        00001316
001340         MOVE XT-SEQ3 (X2 X3)  TO PRE-REIN                        00001317
001341     ELSE                                                         00001318
001342         MOVE YT-SEQ (X2 X3)   TO W-SEQ3                          00001319
001343         MOVE Y-AMTS (X2 X3)   TO W-AMTS                          00001320
001344         MOVE Y-M-AMTS (X2 X3) TO W-M-AMTS                        00001321
001345         MOVE YT-SEQ3 (X2 X3)  TO PRE-REIN.                       00001322
001346                                                                  00001323
001347     PERFORM 0430-GEN-RTN THRU 0450-GEN-XIT.                      00001324
001348                                                                  00001325
001349     IF X1 = +1                                                   00001326
001350         MOVE HIGH-VALUE TO XT-SEQ (X2 X3)                        00001327
001351         MOVE X-DETL     TO X-AMTS (X2 X3)                        00001328
001352         MOVE X-M-DETL   TO X-M-AMTS (X2 X3)                      00001329
001353     ELSE                                                         00001330
001354         MOVE HIGH-VALUE TO YT-SEQ (X2 X3)                        00001331
001355         MOVE X-DETL     TO Y-AMTS (X2 X3)                        00001332
001356         MOVE X-M-DETL   TO Y-M-AMTS (X2 X3).                     00001333
001357                                                                  00001334
001358     GO TO 0400-PRESS-ONE-LOOP.                                   00001335
001359                                                                  00001336
001360 0410-PRESS-ONE-END.                                              00001337
001361     MOVE +1 TO X3.                                               00001338
001362                                                                  00001339
001363 0420-PRESS-ONE-XIT.                                              00001340
001364     EXIT.                                                        00001341
001365                                                                  00001342
001367 0430-GEN-RTN.                                                    00001344
001368**   COMPUTE W-YR = (RUN-YR - W-YR) + 1.                          00001345
121610     COMPUTE WORK-YR = (RUN-CCYY - W-YR) + 1
001369                                                                  00001346
CIDMOD     IF WORK-YR < 0                                               ECS083
CIDMOD        ADD 100 TO WORK-YR.                                          CL**2
CIDMOD     MOVE WORK-YR TO W-YR.                                        ECS083
CIDMOD                                                                  ECS083
001370 0440-GEN-LOOP.                                                   00001347
001371     IF DTE-PGM-OPT NOT GREATER THAN 1                            00001348
001372         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001349
001373                                                                  00001350
001374     MOVE HIGH-VALUE TO W-ST.                                     00001351
001375                                                                  00001352
001376     IF DTE-PGM-OPT NOT GREATER THAN 2                            00001353
001377         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001354
001378                                                                  00001355
001379     MOVE HIGH-VALUE TO W-CO.                                     00001356
001380                                                                  00001357
001381     IF DTE-PGM-OPT NOT GREATER THAN 4                            00001358
001382         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001359
001383                                                                  00001360
001384     MOVE HIGH-VALUE TO W-CARR.                                   00001361
001385                                                                  00001362
001386     IF DTE-PGM-OPT NOT GREATER THAN 6                            00001363
001387         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001364
001388                                                                  00001365
001389     MOVE PRE-SEQ    TO W-SEQ1.                                   00001366
001390     MOVE HIGH-VALUE TO W-CO.                                     00001367
001391                                                                  00001368
001392     IF DTE-PGM-OPT NOT GREATER THAN 3                            00001369
001393         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001370
001394                                                                  00001371
001395     MOVE HIGH-VALUE TO W-CARR.                                   00001372
001396                                                                  00001373
001397     IF DTE-PGM-OPT NOT GREATER THAN 5                            00001374
001398         PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  00001375
001399                                                                  00001376
001400     IF W-REIN NOT = PRE-REIN                                     00001377
001401         MOVE PRE-SEQ  TO W-SEQ1                                  00001378
001402         MOVE PRE-REIN TO W-REIN                                  00001379
001403         GO TO 0440-GEN-LOOP.                                     00001380
001404                                                                  00001381
001405 0450-GEN-XIT.                                                    00001382
001406     EXIT.                                                        00001383
001407                                                                  00001384
001408 0460-RLS-RTN.                                                    00001385
001409     MOVE WORK-REC TO RPT-REC.                                    00001386
001410                                                                  00001387
001411     WRITE RPT-REC.                                               00001388
001412                                                                  00001389
001413 0470-RLS-XIT.                                                    00001390
001414     EXIT.                                                        00001391
001415 EJECT                                                            00001392
001416 0480-PRT-RTN.                                                    00001393
CIDMOD*                            COPY ELCPRTN.                        00001394
CIDMOD                             COPY ELCPRT2.                        00001394
001418                                                                  00001395
001419 0490-E-PRT-RTN.                                                  00001396
001420     EXIT.                                                        00001397
001421                                                                  00001398
001422 0500-PRINT-HEADINGS.                                             00001399
001423     MOVE '1'    TO X.                                            00001400
001424     MOVE HEAD-1 TO P-DATA.                                       00001401
001425     PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001402
001426                                                                  00001403
001427     MOVE ' '             TO X.                                   00001404
001428     MOVE WS-CURRENT-DATE TO HD-DATE.                             00001405
001429     MOVE COMPANY-NAME    TO HD-CLIENT.                           00001406
001430     MOVE HEAD-2          TO P-DATA.                              00001407
001431     PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001408
001432                                                                  00001409
001433     MOVE 1         TO HD-PAGE.                                   00001410
001434     MOVE ALPH-DATE TO HD-ALF-DTE.                                00001411
001435     MOVE ALPH-DATE TO PULL-MONTH.                                00001412
001436     MOVE FIRST4    TO UNEARNED-MO.                               00001413
001437     MOVE HEAD-3    TO P-DATA.                                    00001414
001438     PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001415
001439                                                                  00001416
001440 0510-HEADING-EXIT.                                               00001417
001441     EXIT.                                                        00001418
001442                                                                  00001419
001443 0520-END-INPUT.                                                  00001420
001444     PERFORM 0330-PRESS-RTN THRU 0380-PRESS-XIT.                  00001421
001445                                                                  00001422
001446     CLOSE GAAP-EXTR REPTFL.                                      00001423
001447                                                                  00001424
001448*    MOVE '0' TO X.                                               00001425
001449*    MOVE 'UNEARNED PREMIUM AND COMMMISON EXTRACT COMPLETED'      00001426
001450*             TO P-DATA.                                          00001427
001451*    PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    00001428
001452                                                                  00001429
001453 0530-CLOSE-FICH.                                                 00001430
001454                             COPY ELCPRTC.                        00001431
CIDMOD                                                                  00000028
CIDMOD ACT-PRNT.                                                           CL**4
CIDMOD                                                                     CL**4
CIDMOD       IF P-CTL = ' '                                                CL**4
CIDMOD         WRITE ACT-REC FROM PRT AFTER ADVANCING 1 LINE               CL**4
CIDMOD       ELSE                                                          CL**4
CIDMOD         IF P-CTL = '0'                                              CL**4
CIDMOD           WRITE ACT-REC FROM PRT AFTER ADVANCING 2 LINE             CL**4
CIDMOD         ELSE                                                        CL**4
CIDMOD           IF P-CTL = '-'                                            CL**4
CIDMOD             WRITE ACT-REC FROM PRT AFTER ADVANCING 3 LINE           CL**4
CIDMOD           ELSE                                                      CL**4
CIDMOD             WRITE ACT-REC FROM PRT AFTER ADVANCING PAGE.            CL**4
CIDMOD                                                                     CL**4
CIDMOD ACT-EXIT.                                                           CL**4
CIDMOD     EXIT.                                                           CL**4
CIDMOD                                                                     CL**4
CIDMOD******************************************************************   CL**4
CIDMOD                                                                  00000028
CIDMOD FIN-PRNT.                                                           CL**4
CIDMOD                                                                     CL**4
CIDMOD       IF P-CTL = ' '                                                CL**4
CIDMOD         WRITE FIN-REC FROM PRT AFTER ADVANCING 1 LINE               CL**4
CIDMOD       ELSE                                                          CL**4
CIDMOD         IF P-CTL = '0'                                              CL**4
CIDMOD           WRITE FIN-REC FROM PRT AFTER ADVANCING 2 LINE             CL**4
CIDMOD         ELSE                                                        CL**4
CIDMOD           IF P-CTL = '-'                                            CL**4
CIDMOD             WRITE FIN-REC FROM PRT AFTER ADVANCING 3 LINE           CL**4
CIDMOD           ELSE                                                      CL**4
CIDMOD             WRITE FIN-REC FROM PRT AFTER ADVANCING PAGE.            CL**4
CIDMOD                                                                     CL**4
CIDMOD FIN-EXIT.                                                           CL**4
CIDMOD     EXIT.                                                           CL**4
CIDMOD                                                                     CL**4
CIDMOD******************************************************************   CL**4
CIDMOD                                                                     CL**4
001455                                                                  00001432
001456 9000-PRINT-REPORTS.                                              00001433
001458                                                                  00001434
001459     MOVE      SPACES    TO    P-DATA.                            00001435
001460     MOVE     '1'        TO    X.                                 00001436
001461     MOVE      X         TO    P-CTL.                             00001437
001462     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001438
001463     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001439
001464                                                                  00001440
001465     MOVE      SPACES    TO    P-DATA.                            00001441
001466     MOVE     ' '        TO    X.                                 00001442
001467     MOVE      X         TO    P-CTL.                             00001443
001468     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001444
001469     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001445
001470                                                                  00001446
001471     MOVE      SPACES    TO    P-DATA.                            00001447
001472     MOVE     ' '        TO    X.                                 00001448
001473     MOVE      X         TO    P-CTL.                             00001449
001474     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001450
001475     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001451
001476                                                                  00001452
001477     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00001453
001478     MOVE     ' '        TO    X.                                 00001454
001479     MOVE      X         TO    P-CTL.                             00001455
001480     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001456
001481     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001457
001482                                                                  00001458
001483     MOVE    '========================= ' TO P-DATA.              00001459
001484     MOVE     ' '        TO    X.                                 00001460
001485     MOVE      X         TO    P-CTL.                             00001461
001486     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001462
001487     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001463
001488                                                                  00001464
001489     MOVE      SPACES    TO    P-DATA.                            00001465
001490     MOVE     ' '        TO    X.                                 00001466
001491     MOVE      X         TO    P-CTL.                             00001467
001492     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001468
001493     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001469
001494     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001470
001495     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001471
001496                                                                  00001472
001497     MOVE      SPACES    TO    P-DATA.                            00001473
001498     MOVE     ' '        TO    X.                                 00001474
001499     MOVE      X         TO    P-CTL.                             00001475
001500     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001476
001501     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001477
001502     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001478
001503     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001479
001504                                                                  00001480
001505     MOVE    'R O U T E  T O   B E L I N D A   W E I D N E R '    00001481
001505                                                    TO  P-DATA.   00001481
001506     MOVE     ' '        TO    X.                                 00001482
001507     MOVE      X         TO    P-CTL.                             00001483
001508     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001484
001509                                                                  00001485
001589     MOVE    'R O U T E  T O   J E A N N E   D A H A R S H '      00001565
001510                                                     TO  P-DATA.  00001486
001511     MOVE     ' '        TO    X.                                 00001487
001512     MOVE      X         TO    P-CTL.                             00001488
001513     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001489
001514                                                                  00001490
001515     MOVE      SPACES    TO    P-DATA.                            00001491
001516     MOVE     ' '        TO    X.                                 00001492
001517     MOVE      X         TO    P-CTL.                             00001493
001518     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001494
001519     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001495
001520     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001496
001521     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001497
001522                                                                  00001498
001523     MOVE    '5 T H   F L R   -   F I N A N C I A L   D E P T '   00001499
001524              TO  P-DATA.                                         00001500
001525     MOVE     ' '        TO    X.                                 00001501
001526     MOVE      X         TO    P-CTL.                             00001502
001527     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001503
001529                                                                  00001505
001523     MOVE    ' A C T U A R I A L   D E P T '                      00001499
001524              TO  P-DATA.                                         00001500
001525     MOVE     ' '        TO    X.                                 00001501
001526     MOVE      X         TO    P-CTL.                             00001502
001528     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001504
001529                                                                  00001505
001530     MOVE      SPACES    TO    P-DATA.                            00001506
001531     MOVE     ' '        TO    X.                                 00001507
001532     MOVE      X         TO    P-CTL.                             00001508
001533     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001509
001534     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001510
001535     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001511
001536     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001512
001537                                                                  00001513
001538     MOVE      SPACES    TO    P-DATA.                            00001514
001539     MOVE     '1'        TO    X.                                 00001515
001540     MOVE      X         TO    P-CTL.                             00001516
001541     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001517
001542     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001518
001543                                                                  00001519
001544     MOVE      SPACES    TO    P-DATA.                            00001520
001545     MOVE     ' '        TO    X.                                 00001521
001546     MOVE      X         TO    P-CTL.                             00001522
001547     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001523
001548     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001524
001549                                                                  00001525
001550     MOVE      SPACES    TO    P-DATA.                            00001526
001551     MOVE     ' '        TO    X.                                 00001527
001552     MOVE      X         TO    P-CTL.                             00001528
001553     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001529
001554     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001530
001555                                                                  00001531
001556     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00001532
001557     MOVE     ' '        TO    X.                                 00001533
001558     MOVE      X         TO    P-CTL.                             00001534
001559     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001535
001560     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001536
001561                                                                  00001537
001562     MOVE    '========================= ' TO P-DATA.              00001538
001563     MOVE     ' '        TO    X.                                 00001539
001564     MOVE      X         TO    P-CTL.                             00001540
001565     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001541
001566     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001542
001567                                                                  00001543
001568     MOVE      SPACES    TO    P-DATA.                            00001544
001569     MOVE     ' '        TO    X.                                 00001545
001570     MOVE      X         TO    P-CTL.                             00001546
001571     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001547
001572     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001548
001573     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001549
001574     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001550
001575                                                                  00001551
001576     MOVE      SPACES    TO    P-DATA.                            00001552
001577     MOVE     ' '        TO    X.                                 00001553
001578     MOVE      X         TO    P-CTL.                             00001554
001579     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001555
001580     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001556
001581     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001557
001582     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001558
001583                                                                  00001559
001505     MOVE    'R O U T E  T O   B E L I N D A   W E I D N E R '    00001481
001505                                                    TO  P-DATA.   00001481
001585     MOVE     ' '        TO    X.                                 00001561
001586     MOVE      X         TO    P-CTL.                             00001562
001587     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001563
001588                                                                  00001564
001589     MOVE    'R O U T E  T O   J E A N N E   D A H A R S H '      00001565
001589                                                     TO  P-DATA.  00001565
001590     MOVE     ' '        TO    X.                                 00001566
001591     MOVE      X         TO    P-CTL.                             00001567
001592     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001568
001593                                                                  00001569
001594     MOVE      SPACES    TO    P-DATA.                            00001570
001595     MOVE     ' '        TO    X.                                 00001571
001596     MOVE      X         TO    P-CTL.                             00001572
001597     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001573
001598     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001574
001599     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001575
001600     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001576
001601                                                                  00001577
001602     MOVE    '5 T H   F L R   -   F I N A N C I A L   D E P T '   00001578
001603              TO  P-DATA.                                         00001579
001604     MOVE     ' '        TO    X.                                 00001580
001605     MOVE      X         TO    P-CTL.                             00001581
001606     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001582
001608                                                                  00001584
001602     MOVE    ' A C T U A R I A L   D E P T '                      00001578
001603              TO  P-DATA.                                         00001579
001604     MOVE     ' '        TO    X.                                 00001580
001605     MOVE      X         TO    P-CTL.                             00001581
001607     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001583
001608                                                                  00001584
001609     MOVE      SPACES    TO    P-DATA.                            00001585
001610     MOVE     ' '        TO    X.                                 00001586
001611     MOVE      X         TO    P-CTL.                             00001587
001612     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001588
001613     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001589
001614     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001590
001615     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001591
001616                                                                  00001592
001617     MOVE      SPACES    TO    P-DATA.                            00001593
001618     MOVE     ' '        TO    X.                                 00001594
001619     MOVE      X         TO    P-CTL.                             00001595
001620     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001596
001621     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001597
001622     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001598
001623     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001599
001624                                                                  00001600
001625     MOVE  ALL-LF-RESERV TO  DIS-EDIT.                            00001601
001626     MOVE  'LIFE   -   GRAND TOTAL           = ' TO  DIS-INFO.    00001602
001627     MOVE  DIS-EDIT      TO    DIS-AMT.                           00001603
001628     MOVE  DIS-RCD       TO    P-DATA.                            00001604
001629     MOVE     ' '        TO    X.                                 00001605
001630     MOVE      X         TO    P-CTL.                             00001606
001631     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001607
001632     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001608
001633                                                                  00001609
001634     MOVE      SPACES    TO    P-DATA.                            00001610
001635     MOVE     ' '        TO    X.                                 00001611
001636     MOVE      X         TO    P-CTL.                             00001612
001637     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001613
001638     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001614
001639     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001615
001640     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001616
001641                                                                  00001617
CIDMOD     MOVE  CA-OH-OR-TX-WA-R78 TO DIS-EDIT.                        00001618
CIDMOD     MOVE  'A&H RULE 78 - CA, OH, OR, TX, WA = ' TO DIS-INFO.     00001619
001644     MOVE  DIS-EDIT      TO  DIS-AMT.                             00001620
001645     MOVE  DIS-RCD       TO    P-DATA.                            00001621
001646     MOVE     ' '        TO    X.                                 00001622
001647     MOVE      X         TO    P-CTL.                             00001623
001648     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001624
001649     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001625
001650                                                                  00001626
001651     MOVE      SPACES    TO    P-DATA.                            00001627
001652     MOVE     ' '        TO    X.                                 00001628
001653     MOVE      X         TO    P-CTL.                             00001629
001654     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001630
001655     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001631
001656     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001632
001657     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001633
001658                                                                  00001634
CIDMOD     MOVE  CA-OH-OR-TX-WA-PRAT TO DIS-EDIT.                       00001635
CIDMOD     MOVE  'A&H P-RATA - CA, OH, OR, TX, WA  = ' TO  DIS-INFO.    00001636
001661     MOVE  DIS-EDIT      TO    DIS-AMT.                           00001637
001662     MOVE  DIS-RCD       TO    P-DATA.                            00001638
001663     MOVE     ' '        TO    X.                                 00001639
001664     MOVE      X         TO    P-CTL.                             00001640
001665     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001641
001666     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001642
001667                                                                  00001643
CIDMOD     SUBTRACT CA-OH-OR-TX-WA-R78 FROM ALL-AH-P78                  00001644
001669        GIVING                                                    00001645
001670           A-WORK-GRAND-TOT.                                      00001646
001671                                                                  00001647
CIDMOD     ADD      CA-OH-OR-TX-WA-PRAT     A-WORK-GRAND-TOT            00001648
001673        GIVING                                                    00001649
001674           A-WORK-FINAL-TOT.                                      00001650
001675                                                                  00001651
001676     MOVE      SPACES    TO    P-DATA.                            00001652
001677     MOVE     ' '        TO    X.                                 00001653
001678     MOVE      X         TO    P-CTL.                             00001654
001679     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001655
001680     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001656
001681     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001657
001682     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001658
001683                                                                  00001659
001684     MOVE  A-WORK-FINAL-TOT  TO  DIS-EDIT.                        00001660
001685     MOVE  'A&H UNEARNED PREMIUM TOTAL       = ' TO  DIS-INFO.    00001661
001686     MOVE  DIS-EDIT      TO    DIS-AMT.                           00001662
001687     MOVE  DIS-RCD       TO    P-DATA.                            00001663
001688     MOVE     ' '        TO    X.                                 00001664
001689     MOVE      X         TO    P-CTL.                             00001665
001690     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001666
001691     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001667
001745                                                                  00001668
001746     MOVE      SPACES    TO    P-DATA.                            00001669
001747     MOVE     '1'        TO    X.                                 00001670
001748     MOVE      X         TO    P-CTL.                             00001671
001749     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001672
001750     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001673
001751                                                                  00001674
001752     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00001675
001753     MOVE     '1'        TO    X.                                 00001676
001754     MOVE      X         TO    P-CTL.                             00001677
001755     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001678
001756     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001679
001757                                                                  00001680
001758     MOVE    '========================= ' TO P-DATA.              00001681
001759     MOVE     ' '        TO    X.                                 00001682
001760     MOVE      X         TO    P-CTL.                             00001683
001761     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001684
001762     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001685
001764                                                                  00001686
001765     MOVE      SPACES    TO    P-DATA.                            00001687
001766     MOVE     ' '        TO    X.                                 00001688
001767     MOVE      X         TO    P-CTL.                             00001689
001768     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001690
001769     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001691
001770     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001692
001771     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001693
001772                                                                  00001694
001773     MOVE      SPACES    TO    P-DATA.                            00001695
001774     MOVE     ' '        TO    X.                                 00001696
001775     MOVE      X         TO    P-CTL.                             00001697
001776     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001698
001777     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001699
001778     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001700
001779     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001701
001780                                                                  00001702
001781     MOVE    'L I F E   T O T A L S '  TO  P-DATA.                00001703
001782     MOVE     ' '        TO    P-CTL.                             00001704
001783     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001705
001784     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001706
001785                                                                  00001707
001786     MOVE    '===================== '  TO  P-DATA.                00001708
001787     MOVE     ' '        TO    P-CTL.                             00001709
001788     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001710
001789     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001711
001790                                                                  00001712
001791     MOVE      SPACES    TO    P-DATA.                            00001713
001792     MOVE     ' '        TO    X.                                 00001714
001793     MOVE      X         TO    P-CTL.                             00001715
001794     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001716
001795     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001717
001796     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001718
001797     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001719
001798                                                                  00001720
001799     MOVE                                                         00001721
001800     'L I F E   G R A N D   T O T A L S,   A L L   S T A T E S '  00001722
001801         TO  P-DATA.                                              00001723
001802     MOVE     ' '        TO    P-CTL.                             00001724
001803     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001725
001804     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001726
001805                                                                  00001727
001806     MOVE      SPACES    TO    P-DATA.                            00001728
001807     MOVE     ' '        TO    X.                                 00001729
001808     MOVE      X         TO    P-CTL.                             00001730
001809     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001731
001810     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001732
001811     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001733
001812     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001734
001814                                                                  00001735
001815     MOVE ALL-LF-COUNT                  TO  BEN-COUNT-P.          00001736
001816     MOVE 'L-ALL-REC-COUNT  =         ' TO PRT-INFO.              00001737
001817     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00001738
001818     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00001739
001819     MOVE SPACES    TO    P-DATA.                                 00001740
001820     MOVE PRT-LINE  TO    P-DATA.                                 00001741
001821     MOVE     ' '        TO    X.                                 00001742
001822     MOVE      X         TO    P-CTL.                             00001743
001823     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001744
001824     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001745
001825                                                                  00001746
001826     MOVE    ALL-LF-WRITTEN    TO  DIS-EDIT.                      00001747
001827     MOVE    'L-ALL-WRITTEN    = ' TO PRT-INFO.                   00001748
001828     MOVE    DIS-EDIT          TO PRT-AMT.                        00001749
001829     MOVE SPACES    TO    P-DATA.                                 00001750
001830     MOVE PRT-LINE  TO    P-DATA.                                 00001751
001831     MOVE     ' '        TO    X.                                 00001752
001832     MOVE      X         TO    P-CTL.                             00001753
001833     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001754
001834     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001755
001835                                                                  00001756
001836     MOVE    ALL-LF-P78        TO  DIS-EDIT.                      00001757
001837     MOVE    'L-ALL-P78        = ' TO PRT-INFO.                   00001758
001838     MOVE    DIS-EDIT          TO PRT-AMT.                        00001759
001839     MOVE SPACES    TO    P-DATA.                                 00001760
001840     MOVE PRT-LINE  TO    P-DATA.                                 00001761
001841     MOVE     ' '        TO    X.                                 00001762
001842     MOVE      X         TO    P-CTL.                             00001763
001843     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001764
001844     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001765
001845                                                                  00001766
001846     MOVE    ALL-LF-PRATA      TO  DIS-EDIT.                      00001767
001847     MOVE    'L-ALL-PRATA      = ' TO PRT-INFO.                   00001768
001848     MOVE    DIS-EDIT          TO PRT-AMT.                        00001769
001849     MOVE SPACES    TO    P-DATA.                                 00001770
001850     MOVE PRT-LINE  TO    P-DATA.                                 00001771
001851     MOVE     ' '        TO    X.                                 00001772
001852     MOVE      X         TO    P-CTL.                             00001773
001853     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001774
001854     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001775
001855                                                                  00001776
001856     MOVE    ALL-LF-DOMICILE   TO  DIS-EDIT.                      00001777
001857     MOVE    'L-ALL-DOMICILE   = ' TO PRT-INFO.                   00001778
001858     MOVE    DIS-EDIT          TO PRT-AMT.                        00001779
001859     MOVE SPACES    TO    P-DATA.                                 00001780
001860     MOVE PRT-LINE  TO    P-DATA.                                 00001781
001861     MOVE     ' '        TO    X.                                 00001782
001862     MOVE      X         TO    P-CTL.                             00001783
001863     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001784
001864     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001785
001865                                                                  00001786
001866     MOVE    ALL-LF-STATE      TO  DIS-EDIT.                      00001787
001867     MOVE    'L-ALL-STATE      = ' TO PRT-INFO.                   00001788
001868     MOVE    DIS-EDIT          TO PRT-AMT.                        00001789
001869     MOVE SPACES    TO    P-DATA.                                 00001790
001870     MOVE PRT-LINE  TO    P-DATA.                                 00001791
001871     MOVE     ' '        TO    X.                                 00001792
001872     MOVE      X         TO    P-CTL.                             00001793
001873     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001794
001874     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001795
001875                                                                  00001796
001876     MOVE    ALL-LF-ALTRSV     TO  DIS-EDIT.                      00001797
001877     MOVE    'L-ALL-ALTRSV     = ' TO PRT-INFO.                   00001798
001878     MOVE    DIS-EDIT          TO PRT-AMT.                        00001799
001879     MOVE SPACES    TO    P-DATA.                                 00001800
001880     MOVE PRT-LINE  TO    P-DATA.                                 00001801
001881     MOVE     ' '        TO    X.                                 00001802
001882     MOVE      X         TO    P-CTL.                             00001803
001883     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001804
001884     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001805
001885                                                                  00001806
001886     MOVE    ALL-LF-RESERV     TO  DIS-EDIT.                      00001807
001887     MOVE    'L-ALL-RESERV     = ' TO PRT-INFO.                   00001808
001888     MOVE    DIS-EDIT          TO PRT-AMT.                        00001809
001889     MOVE SPACES    TO    P-DATA.                                 00001810
001890     MOVE PRT-LINE  TO    P-DATA.                                 00001811
001891     MOVE     ' '        TO    X.                                 00001812
001892     MOVE      X         TO    P-CTL.                             00001813
001893     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001814
001894     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001815
001895                                                                  00001816
001896     MOVE    ALL-LF-REMAIN     TO  DIS-EDIT.                      00001817
001897     MOVE    'L-ALL-REMAIN     = ' TO PRT-INFO.                   00001818
001898     MOVE    DIS-EDIT          TO PRT-AMT.                        00001819
001899     MOVE SPACES    TO    P-DATA.                                 00001820
001900     MOVE PRT-LINE  TO    P-DATA.                                 00001821
001901     MOVE     ' '        TO    X.                                 00001822
001902     MOVE      X         TO    P-CTL.                             00001823
001903     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001824
001904     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001825
001905                                                                  00001826
001906     MOVE    ALL-LF-PAID       TO  DIS-EDIT.                      00001827
001907     MOVE    'L-ALL-PAID       = ' TO PRT-INFO.                   00001828
001908     MOVE    DIS-EDIT          TO PRT-AMT.                        00001829
001909     MOVE SPACES    TO    P-DATA.                                 00001830
001910     MOVE PRT-LINE  TO    P-DATA.                                 00001831
001911     MOVE     ' '        TO    X.                                 00001832
001912     MOVE      X         TO    P-CTL.                             00001833
001913     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001834
001914     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001835
001915                                                                  00001836
001916     MOVE    ALL-LF-C78        TO  DIS-EDIT.                      00001837
001917     MOVE    'L-ALL-C78        = ' TO PRT-INFO.                   00001838
001918     MOVE    DIS-EDIT          TO PRT-AMT.                        00001839
001919     MOVE SPACES    TO    P-DATA.                                 00001840
001920     MOVE PRT-LINE  TO    P-DATA.                                 00001841
001921     MOVE     ' '        TO    X.                                 00001842
001922     MOVE      X         TO    P-CTL.                             00001843
001923     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001844
001924     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001845
001925                                                                  00001846
001926     MOVE    ALL-LF-CRATA      TO  DIS-EDIT.                      00001847
001927     MOVE    'L-ALL-CRATA      = ' TO PRT-INFO.                   00001848
001928     MOVE    DIS-EDIT          TO PRT-AMT.                        00001849
001929     MOVE SPACES    TO    P-DATA.                                 00001850
001930     MOVE PRT-LINE  TO    P-DATA.                                 00001851
001931     MOVE     ' '        TO    X.                                 00001852
001932     MOVE      X         TO    P-CTL.                             00001853
001933     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001854
001934     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001855
040114
040114     MOVE    ALL-LF-CDOMI      TO  DIS-EDIT.
040114     MOVE    'L-ALL-CDOMI      = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
001935                                                                  00001856
001936     MOVE    ALL-LF-TAX        TO  DIS-EDIT.                      00001857
001937     MOVE    'L-ALL-TAX        = ' TO PRT-INFO.                   00001858
001938     MOVE    DIS-EDIT          TO PRT-AMT.                        00001859
001939     MOVE SPACES    TO    P-DATA.                                 00001860
001940     MOVE PRT-LINE  TO    P-DATA.                                 00001861
001941     MOVE     ' '        TO    X.                                 00001862
001942     MOVE      X         TO    P-CTL.                             00001863
001943     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001864
001944     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001865
001945                                                                  00001866
001946     MOVE    ALL-LF-T78        TO  DIS-EDIT.                      00001867
001947     MOVE    'L-ALL-T78        = ' TO PRT-INFO.                   00001868
001948     MOVE    DIS-EDIT          TO PRT-AMT.                        00001869
001949     MOVE SPACES    TO    P-DATA.                                 00001870
001950     MOVE PRT-LINE  TO    P-DATA.                                 00001871
001951     MOVE     ' '        TO    X.                                 00001872
001952     MOVE      X         TO    P-CTL.                             00001873
001953     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001874
001954     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001875
001955                                                                  00001876
001956     MOVE    ALL-LF-TRATA      TO  DIS-EDIT.                      00001877
001957     MOVE    'L-ALL-TRATA      = ' TO PRT-INFO.                   00001878
001958     MOVE    DIS-EDIT          TO PRT-AMT.                        00001879
001959     MOVE SPACES    TO    P-DATA.                                 00001880
001960     MOVE PRT-LINE  TO    P-DATA.                                 00001881
001961     MOVE     ' '        TO    X.                                 00001882
001962     MOVE      X         TO    P-CTL.                             00001883
001963     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001884
001964     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001885
040114
040114     MOVE    ALL-LF-TDOMI      TO  DIS-EDIT.
040114     MOVE    'L-ALL-TDOMI      = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
001966                                                                  00001886
001967     MOVE      SPACES    TO    P-DATA.                            00001887
001968     MOVE     '1'        TO    X.                                 00001888
001969     MOVE      X         TO    P-CTL.                             00001889
001970     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001890
001971     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001891
001972                                                                  00001892
001973     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00001893
001974     MOVE     '1'        TO    X.                                 00001894
001975     MOVE      X         TO    P-CTL.                             00001895
001976     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001896
001977     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001897
001978                                                                  00001898
001979     MOVE    '========================= ' TO P-DATA.              00001899
001980     MOVE     ' '        TO    X.                                 00001900
001981     MOVE      X         TO    P-CTL.                             00001901
001982     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001902
001983     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001903
001984                                                                  00001904
001985     MOVE SPACES    TO    P-DATA.                                 00001905
001986     MOVE     ' '        TO    X.                                 00001906
001987     MOVE      X         TO    P-CTL.                             00001907
001988     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001908
001989     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001909
001990                                                                  00001910
001991     MOVE    'A & H   T O T A L S '  TO  P-DATA.                  00001911
001992     MOVE     ' '        TO    X.                                 00001912
001993     MOVE      X         TO    P-CTL.                             00001913
001994     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001914
001995     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001915
001996                                                                  00001916
001997     MOVE    '==================== '  TO  P-DATA.                 00001917
001998     MOVE     ' '        TO    X.                                 00001918
001999     MOVE      X         TO    P-CTL.                             00001919
002000     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001920
002001     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001921
002002                                                                  00001922
002003     MOVE SPACES    TO    P-DATA.                                 00001923
002004     MOVE     ' '        TO    X.                                 00001924
002005     MOVE      X         TO    P-CTL.                             00001925
002006     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001926
002007     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001927
002008                                                                  00001928
002009     MOVE SPACES    TO    P-DATA.                                 00001929
002010     MOVE    'C O L O R A D O           '  TO P-DATA.             00001930
002011     MOVE     ' '        TO    X.                                 00001931
002012     MOVE      X         TO    P-CTL.                             00001932
002013     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001933
002014     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001934
002015                                                                  00001935
002016     MOVE SPACES TO P-DATA.                                       00001936
002017     MOVE     ' '        TO    X.                                 00001937
002018     MOVE      X         TO    P-CTL.                             00001938
002019     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001939
002020     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001940
002021                                                                  00001941
002022     MOVE    CO-AH-COUNT       TO  BEN-COUNT-P.                   00001942
002023     MOVE    'A-CO-REC-COUNT   =         ' TO PRT-INFO.           00001943
002024     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00001944
002025     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00001945
002026     MOVE SPACES    TO    P-DATA.                                 00001946
002027     MOVE PRT-LINE  TO    P-DATA.                                 00001947
002028     MOVE     ' '        TO    X.                                 00001948
002029     MOVE      X         TO    P-CTL.                             00001949
002030     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001950
002031     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001951
002032                                                                  00001952
002033     MOVE    CO-AH-WRITTEN     TO  DIS-EDIT.                      00001953
002034     MOVE    'A-CO-WRITTEN     = ' TO PRT-INFO.                   00001954
002035     MOVE    DIS-EDIT          TO PRT-AMT.                        00001955
002036     MOVE SPACES    TO    P-DATA.                                 00001956
002037     MOVE PRT-LINE  TO    P-DATA.                                 00001957
002038     MOVE     ' '        TO    X.                                 00001958
002039     MOVE      X         TO    P-CTL.                             00001959
002040     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001960
002041     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001961
002042                                                                  00001962
002043     MOVE    CO-AH-P78         TO  DIS-EDIT.                      00001963
002044     MOVE    'A-CO-P78         = ' TO PRT-INFO.                   00001964
002045     MOVE    DIS-EDIT          TO PRT-AMT.                        00001965
002046     MOVE SPACES    TO    P-DATA.                                 00001966
002047     MOVE PRT-LINE  TO    P-DATA.                                 00001967
002048     MOVE     ' '        TO    X.                                 00001968
002049     MOVE      X         TO    P-CTL.                             00001969
002050     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001970
002051     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001971
002052                                                                  00001972
002053     MOVE    CO-AH-PRATA       TO  DIS-EDIT.                      00001973
002054     MOVE    'A-CO-PRATA       = ' TO PRT-INFO.                   00001974
002055     MOVE    DIS-EDIT          TO PRT-AMT.                        00001975
002056     MOVE SPACES    TO    P-DATA.                                 00001976
002057     MOVE PRT-LINE  TO    P-DATA.                                 00001977
002058     MOVE     ' '        TO    X.                                 00001978
002059     MOVE      X         TO    P-CTL.                             00001979
002060     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001980
002061     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001981
002062                                                                  00001982
002063     MOVE    CO-AH-DOMICILE    TO  DIS-EDIT.                      00001983
002064     MOVE    'A-CO-DOMICILE    = ' TO PRT-INFO.                   00001984
002065     MOVE    DIS-EDIT          TO PRT-AMT.                        00001985
002066     MOVE SPACES    TO    P-DATA.                                 00001986
002067     MOVE PRT-LINE  TO    P-DATA.                                 00001987
002068     MOVE     ' '        TO    X.                                 00001988
002069     MOVE      X         TO    P-CTL.                             00001989
002070     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001990
002071     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001991
002072                                                                  00001992
002073     MOVE    CO-AH-STATE       TO  DIS-EDIT.                      00001993
002074     MOVE    'A-CO-STATE       = ' TO PRT-INFO.                   00001994
002075     MOVE    DIS-EDIT          TO PRT-AMT.                        00001995
002076     MOVE SPACES    TO    P-DATA.                                 00001996
002077     MOVE PRT-LINE  TO    P-DATA.                                 00001997
002078     MOVE     ' '        TO    X.                                 00001998
002079     MOVE      X         TO    P-CTL.                             00001999
002080     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002000
002081     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002001
002082                                                                  00002002
002083     MOVE    CO-AH-ALTRSV      TO  DIS-EDIT.                      00002003
002084     MOVE    'A-CO-ALTRSV      = ' TO PRT-INFO.                   00002004
002085     MOVE    DIS-EDIT          TO PRT-AMT.                        00002005
002086     MOVE SPACES    TO    P-DATA.                                 00002006
002087     MOVE PRT-LINE  TO    P-DATA.                                 00002007
002088     MOVE     ' '        TO    X.                                 00002008
002089     MOVE      X         TO    P-CTL.                             00002009
002090     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002010
002091     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002011
002092                                                                  00002012
002093     MOVE    CO-AH-RESERV      TO  DIS-EDIT.                      00002013
002094     MOVE    'A-CO-RESERV      = ' TO PRT-INFO.                   00002014
002095     MOVE    DIS-EDIT          TO PRT-AMT.                        00002015
002096     MOVE SPACES    TO    P-DATA.                                 00002016
002097     MOVE PRT-LINE  TO    P-DATA.                                 00002017
002098     MOVE     ' '        TO    X.                                 00002018
002099     MOVE      X         TO    P-CTL.                             00002019
002100     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002020
002101     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002021
002102                                                                  00002022
002103     MOVE    CO-AH-REMAIN      TO  DIS-EDIT.                      00002023
002104     MOVE    'A-CO-REMAIN      = ' TO PRT-INFO.                   00002024
002105     MOVE    DIS-EDIT          TO PRT-AMT.                        00002025
002106     MOVE SPACES    TO    P-DATA.                                 00002026
002107     MOVE PRT-LINE  TO    P-DATA.                                 00002027
002108     MOVE     ' '        TO    X.                                 00002028
002109     MOVE      X         TO    P-CTL.                             00002029
002110     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002030
002111     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002031
002112                                                                  00002032
002113     MOVE    CO-AH-PAID        TO  DIS-EDIT.                      00002033
002114     MOVE    'A-CO-PAID        = ' TO PRT-INFO.                   00002034
002115     MOVE    DIS-EDIT          TO PRT-AMT.                        00002035
002116     MOVE SPACES    TO    P-DATA.                                 00002036
002117     MOVE PRT-LINE  TO    P-DATA.                                 00002037
002118     MOVE     ' '        TO    X.                                 00002038
002119     MOVE      X         TO    P-CTL.                             00002039
002120     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002040
002121     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002041
002122                                                                  00002042
002123     MOVE    CO-AH-C78         TO  DIS-EDIT.                      00002043
002124     MOVE    'A-CO-C78         = ' TO PRT-INFO.                   00002044
002125     MOVE    DIS-EDIT          TO PRT-AMT.                        00002045
002126     MOVE SPACES    TO    P-DATA.                                 00002046
002127     MOVE PRT-LINE  TO    P-DATA.                                 00002047
002128     MOVE     ' '        TO    X.                                 00002048
002129     MOVE      X         TO    P-CTL.                             00002049
002130     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002050
002131     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002051
002132                                                                  00002052
002133     MOVE    CO-AH-CRATA       TO  DIS-EDIT.                      00002053
002134     MOVE    'A-CO-CRATA       = ' TO PRT-INFO.                   00002054
002135     MOVE    DIS-EDIT          TO PRT-AMT.                        00002055
002136     MOVE SPACES    TO    P-DATA.                                 00002056
002137     MOVE PRT-LINE  TO    P-DATA.                                 00002057
002138     MOVE     ' '        TO    X.                                 00002058
002139     MOVE      X         TO    P-CTL.                             00002059
002140     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002060
002141     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002061
040114
040114     MOVE    CO-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-CO-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002142                                                                  00002062
002143     MOVE    CO-AH-TAX         TO  DIS-EDIT.                      00002063
002144     MOVE    'A-CO-TAX         = ' TO PRT-INFO.                   00002064
002145     MOVE    DIS-EDIT          TO PRT-AMT.                        00002065
002146     MOVE SPACES    TO    P-DATA.                                 00002066
002147     MOVE PRT-LINE  TO    P-DATA.                                 00002067
002148     MOVE     ' '        TO    X.                                 00002068
002149     MOVE      X         TO    P-CTL.                             00002069
002150     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002070
002151     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002071
002152                                                                  00002072
002153     MOVE    CO-AH-T78         TO  DIS-EDIT.                      00002073
002154     MOVE    'A-CO-T78         = ' TO PRT-INFO.                   00002074
002155     MOVE    DIS-EDIT          TO PRT-AMT.                        00002075
002156     MOVE SPACES    TO    P-DATA.                                 00002076
002157     MOVE PRT-LINE  TO    P-DATA.                                 00002077
002158     MOVE     ' '        TO    X.                                 00002078
002159     MOVE      X         TO    P-CTL.                             00002079
002160     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002080
002161     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002081
002162                                                                  00002082
002163     MOVE    CO-AH-TRATA       TO  DIS-EDIT.                      00002083
002164     MOVE    'A-CO-TRATA       = ' TO PRT-INFO.                   00002084
002165     MOVE    DIS-EDIT          TO PRT-AMT.                        00002085
002166     MOVE SPACES    TO    P-DATA.                                 00002086
002167     MOVE PRT-LINE  TO    P-DATA.                                 00002087
002168     MOVE     ' '        TO    X.                                 00002088
002169     MOVE      X         TO    P-CTL.                             00002089
002170     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002090
002171     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002091
040114
040114     MOVE    CO-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-CO-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002172                                                                  00002092
002173     MOVE SPACES TO P-DATA.                                       00002093
002174     MOVE     ' '        TO    X.                                 00002094
002175     MOVE      X         TO    P-CTL.                             00002095
002176     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002096
002177     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002097
002179                                                                  00002098
002180* COLO PRECEDING THIS LINE                                        00002099
002182                                                                  00002100
002009     MOVE SPACES    TO    P-DATA.                                 00001929
002010     MOVE    'G E O R G I A             '  TO P-DATA.             00001930
002011     MOVE     ' '        TO    X.                                 00001931
002012     MOVE      X         TO    P-CTL.                             00001932
002013     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001933
002014     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001934
002015                                                                  00001935
002016     MOVE SPACES TO P-DATA.                                       00001936
002017     MOVE     ' '        TO    X.                                 00001937
002018     MOVE      X         TO    P-CTL.                             00001938
002019     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001939
002020     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001940
002021                                                                  00001941
002022     MOVE    GA-AH-COUNT       TO  BEN-COUNT-P.                   00001942
002023     MOVE    'A-GA-REC-COUNT   =         ' TO PRT-INFO.           00001943
002024     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00001944
002025     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00001945
002026     MOVE SPACES    TO    P-DATA.                                 00001946
002027     MOVE PRT-LINE  TO    P-DATA.                                 00001947
002028     MOVE     ' '        TO    X.                                 00001948
002029     MOVE      X         TO    P-CTL.                             00001949
002030     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001950
002031     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001951
002032                                                                  00001952
002033     MOVE    GA-AH-WRITTEN     TO  DIS-EDIT.                      00001953
002034     MOVE    'A-GA-WRITTEN     = ' TO PRT-INFO.                   00001954
002035     MOVE    DIS-EDIT          TO PRT-AMT.                        00001955
002036     MOVE SPACES    TO    P-DATA.                                 00001956
002037     MOVE PRT-LINE  TO    P-DATA.                                 00001957
002038     MOVE     ' '        TO    X.                                 00001958
002039     MOVE      X         TO    P-CTL.                             00001959
002040     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001960
002041     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001961
002042                                                                  00001962
002043     MOVE    GA-AH-P78         TO  DIS-EDIT.                      00001963
002044     MOVE    'A-GA-P78         = ' TO PRT-INFO.                   00001964
002045     MOVE    DIS-EDIT          TO PRT-AMT.                        00001965
002046     MOVE SPACES    TO    P-DATA.                                 00001966
002047     MOVE PRT-LINE  TO    P-DATA.                                 00001967
002048     MOVE     ' '        TO    X.                                 00001968
002049     MOVE      X         TO    P-CTL.                             00001969
002050     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001970
002051     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001971
002052                                                                  00001972
002053     MOVE    GA-AH-PRATA       TO  DIS-EDIT.                      00001973
002054     MOVE    'A-GA-PRATA       = ' TO PRT-INFO.                   00001974
002055     MOVE    DIS-EDIT          TO PRT-AMT.                        00001975
002056     MOVE SPACES    TO    P-DATA.                                 00001976
002057     MOVE PRT-LINE  TO    P-DATA.                                 00001977
002058     MOVE     ' '        TO    X.                                 00001978
002059     MOVE      X         TO    P-CTL.                             00001979
002060     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001980
002061     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001981
002062                                                                  00001982
002063     MOVE    GA-AH-DOMICILE    TO  DIS-EDIT.                      00001983
002064     MOVE    'A-GA-DOMICILE    = ' TO PRT-INFO.                   00001984
002065     MOVE    DIS-EDIT          TO PRT-AMT.                        00001985
002066     MOVE SPACES    TO    P-DATA.                                 00001986
002067     MOVE PRT-LINE  TO    P-DATA.                                 00001987
002068     MOVE     ' '        TO    X.                                 00001988
002069     MOVE      X         TO    P-CTL.                             00001989
002070     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00001990
002071     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00001991
002072                                                                  00001992
002073     MOVE    GA-AH-STATE       TO  DIS-EDIT.                      00001993
002074     MOVE    'A-GA-STATE       = ' TO PRT-INFO.                   00001994
002075     MOVE    DIS-EDIT          TO PRT-AMT.                        00001995
002076     MOVE SPACES    TO    P-DATA.                                 00001996
002077     MOVE PRT-LINE  TO    P-DATA.                                 00001997
002078     MOVE     ' '        TO    X.                                 00001998
002079     MOVE      X         TO    P-CTL.                             00001999
002080     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002000
002081     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002001
002082                                                                  00002002
002083     MOVE    GA-AH-ALTRSV      TO  DIS-EDIT.                      00002003
002084     MOVE    'A-GA-ALTRSV      = ' TO PRT-INFO.                   00002004
002085     MOVE    DIS-EDIT          TO PRT-AMT.                        00002005
002086     MOVE SPACES    TO    P-DATA.                                 00002006
002087     MOVE PRT-LINE  TO    P-DATA.                                 00002007
002088     MOVE     ' '        TO    X.                                 00002008
002089     MOVE      X         TO    P-CTL.                             00002009
002090     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002010
002091     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002011
002092                                                                  00002012
002093     MOVE    GA-AH-RESERV      TO  DIS-EDIT.                      00002013
002094     MOVE    'A-GA-RESERV      = ' TO PRT-INFO.                   00002014
002095     MOVE    DIS-EDIT          TO PRT-AMT.                        00002015
002096     MOVE SPACES    TO    P-DATA.                                 00002016
002097     MOVE PRT-LINE  TO    P-DATA.                                 00002017
002098     MOVE     ' '        TO    X.                                 00002018
002099     MOVE      X         TO    P-CTL.                             00002019
002100     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002020
002101     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002021
002102                                                                  00002022
002103     MOVE    GA-AH-REMAIN      TO  DIS-EDIT.                      00002023
002104     MOVE    'A-GA-REMAIN      = ' TO PRT-INFO.                   00002024
002105     MOVE    DIS-EDIT          TO PRT-AMT.                        00002025
002106     MOVE SPACES    TO    P-DATA.                                 00002026
002107     MOVE PRT-LINE  TO    P-DATA.                                 00002027
002108     MOVE     ' '        TO    X.                                 00002028
002109     MOVE      X         TO    P-CTL.                             00002029
002110     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002030
002111     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002031
002112                                                                  00002032
002113     MOVE    GA-AH-PAID        TO  DIS-EDIT.                      00002033
002114     MOVE    'A-GA-PAID        = ' TO PRT-INFO.                   00002034
002115     MOVE    DIS-EDIT          TO PRT-AMT.                        00002035
002116     MOVE SPACES    TO    P-DATA.                                 00002036
002117     MOVE PRT-LINE  TO    P-DATA.                                 00002037
002118     MOVE     ' '        TO    X.                                 00002038
002119     MOVE      X         TO    P-CTL.                             00002039
002120     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002040
002121     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002041
002122                                                                  00002042
002123     MOVE    GA-AH-C78         TO  DIS-EDIT.                      00002043
002124     MOVE    'A-GA-C78         = ' TO PRT-INFO.                   00002044
002125     MOVE    DIS-EDIT          TO PRT-AMT.                        00002045
002126     MOVE SPACES    TO    P-DATA.                                 00002046
002127     MOVE PRT-LINE  TO    P-DATA.                                 00002047
002128     MOVE     ' '        TO    X.                                 00002048
002129     MOVE      X         TO    P-CTL.                             00002049
002130     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002050
002131     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002051
002132                                                                  00002052
002133     MOVE    GA-AH-CRATA       TO  DIS-EDIT.                      00002053
002134     MOVE    'A-GA-CRATA       = ' TO PRT-INFO.                   00002054
002135     MOVE    DIS-EDIT          TO PRT-AMT.                        00002055
002136     MOVE SPACES    TO    P-DATA.                                 00002056
002137     MOVE PRT-LINE  TO    P-DATA.                                 00002057
002138     MOVE     ' '        TO    X.                                 00002058
002139     MOVE      X         TO    P-CTL.                             00002059
002140     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002060
002141     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002061
040114
040114     MOVE    GA-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-GA-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002142                                                                  00002062
002143     MOVE    GA-AH-TAX         TO  DIS-EDIT.                      00002063
002144     MOVE    'A-GA-TAX         = ' TO PRT-INFO.                   00002064
002145     MOVE    DIS-EDIT          TO PRT-AMT.                        00002065
002146     MOVE SPACES    TO    P-DATA.                                 00002066
002147     MOVE PRT-LINE  TO    P-DATA.                                 00002067
002148     MOVE     ' '        TO    X.                                 00002068
002149     MOVE      X         TO    P-CTL.                             00002069
002150     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002070
002151     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002071
002152                                                                  00002072
002153     MOVE    GA-AH-T78         TO  DIS-EDIT.                      00002073
002154     MOVE    'A-GA-T78         = ' TO PRT-INFO.                   00002074
002155     MOVE    DIS-EDIT          TO PRT-AMT.                        00002075
002156     MOVE SPACES    TO    P-DATA.                                 00002076
002157     MOVE PRT-LINE  TO    P-DATA.                                 00002077
002158     MOVE     ' '        TO    X.                                 00002078
002159     MOVE      X         TO    P-CTL.                             00002079
002160     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002080
002161     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002081
002162                                                                  00002082
002163     MOVE    GA-AH-TRATA       TO  DIS-EDIT.                      00002083
002164     MOVE    'A-GA-TRATA       = ' TO PRT-INFO.                   00002084
002165     MOVE    DIS-EDIT          TO PRT-AMT.                        00002085
002166     MOVE SPACES    TO    P-DATA.                                 00002086
002167     MOVE PRT-LINE  TO    P-DATA.                                 00002087
002168     MOVE     ' '        TO    X.                                 00002088
002169     MOVE      X         TO    P-CTL.                             00002089
002170     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002090
002171     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002091
040114
040114     MOVE    GA-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-GA-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002172                                                                  00002092
002173     MOVE SPACES TO P-DATA.                                       00002093
002174     MOVE     ' '        TO    X.                                 00002094
002175     MOVE      X         TO    P-CTL.                             00002095
002176     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002096
002177     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002097
002179                                                                  00002098
002180* GA   PRECEDING THIS LINE                                        00002099
002182                                                                  00002100
002183     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00002101
002184     MOVE     '1'        TO    X.                                 00002102
002185     MOVE      X         TO    P-CTL.                             00002103
002186     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002104
002187     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002105
002188                                                                  00002106
002189     MOVE    '========================= ' TO P-DATA.              00002107
002190     MOVE     ' '        TO    X.                                 00002108
002191     MOVE      X         TO    P-CTL.                             00002109
002192     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002110
002193     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002111
002195                                                                  00002112
002196     MOVE SPACES    TO    P-DATA.                                 00002113
002197     MOVE     ' '        TO    X.                                 00002114
002198     MOVE      X         TO    P-CTL.                             00002115
002199     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002116
002200     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002117
002201                                                                  00002118
002202     MOVE SPACES    TO    P-DATA.                                 00002119
002203     MOVE    'C A L I F O R N I A       '  TO P-DATA.             00002120
002204     MOVE     ' '        TO    X.                                 00002121
002205     MOVE      X         TO    P-CTL.                             00002122
002206     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002123
002207     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002124
002208                                                                  00002125
002209     MOVE SPACES TO P-DATA.                                       00002126
002210     MOVE     ' '        TO    X.                                 00002127
002211     MOVE      X         TO    P-CTL.                             00002128
002212     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002129
002213     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002130
002214                                                                  00002131
002215     MOVE    CA-AH-COUNT       TO  BEN-COUNT-P.                   00002132
002216     MOVE    'A-CA-REC-COUNT   =         ' TO PRT-INFO.           00002133
002217     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00002134
002218     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00002135
002219     MOVE SPACES    TO    P-DATA.                                 00002136
002220     MOVE PRT-LINE  TO    P-DATA.                                 00002137
002221     MOVE     ' '        TO    X.                                 00002138
002222     MOVE      X         TO    P-CTL.                             00002139
002223     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002140
002224     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002141
002225                                                                  00002142
002226     MOVE    CA-AH-WRITTEN     TO  DIS-EDIT.                      00002143
002227     MOVE    'A-CA-WRITTEN     = ' TO PRT-INFO.                   00002144
002228     MOVE    DIS-EDIT          TO PRT-AMT.                        00002145
002229     MOVE SPACES    TO    P-DATA.                                 00002146
002230     MOVE PRT-LINE  TO    P-DATA.                                 00002147
002231     MOVE     ' '        TO    X.                                 00002148
002232     MOVE      X         TO    P-CTL.                             00002149
002233     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002150
002234     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002151
002235                                                                  00002152
002236     MOVE    CA-AH-P78         TO  DIS-EDIT.                      00002153
002237     MOVE    'A-CA-P78         = ' TO PRT-INFO.                   00002154
002238     MOVE    DIS-EDIT          TO PRT-AMT.                        00002155
002239     MOVE SPACES    TO    P-DATA.                                 00002156
002240     MOVE PRT-LINE  TO    P-DATA.                                 00002157
002241     MOVE     ' '        TO    X.                                 00002158
002242     MOVE      X         TO    P-CTL.                             00002159
002243     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002160
002244     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002161
002245                                                                  00002162
002246     MOVE    CA-AH-PRATA       TO  DIS-EDIT.                      00002163
002247     MOVE    'A-CA-PRATA       = ' TO PRT-INFO.                   00002164
002248     MOVE    DIS-EDIT          TO PRT-AMT.                        00002165
002249     MOVE SPACES    TO    P-DATA.                                 00002166
002250     MOVE PRT-LINE  TO    P-DATA.                                 00002167
002251     MOVE     ' '        TO    X.                                 00002168
002252     MOVE      X         TO    P-CTL.                             00002169
002253     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002170
002254     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002171
002255                                                                  00002172
002256     MOVE    CA-AH-DOMICILE    TO  DIS-EDIT.                      00002173
002257     MOVE    'A-CA-DOMICILE    = ' TO PRT-INFO.                   00002174
002258     MOVE    DIS-EDIT          TO PRT-AMT.                        00002175
002259     MOVE SPACES    TO    P-DATA.                                 00002176
002260     MOVE PRT-LINE  TO    P-DATA.                                 00002177
002261     MOVE     ' '        TO    X.                                 00002178
002262     MOVE      X         TO    P-CTL.                             00002179
002263     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002180
002264     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002181
002265                                                                  00002182
002266     MOVE    CA-AH-STATE       TO  DIS-EDIT.                      00002183
002267     MOVE    'A-CA-STATE       = ' TO PRT-INFO.                   00002184
002268     MOVE    DIS-EDIT          TO PRT-AMT.                        00002185
002269     MOVE SPACES    TO    P-DATA.                                 00002186
002270     MOVE PRT-LINE  TO    P-DATA.                                 00002187
002271     MOVE     ' '        TO    X.                                 00002188
002272     MOVE      X         TO    P-CTL.                             00002189
002273     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002190
002274     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002191
002275                                                                  00002192
002276     MOVE    CA-AH-ALTRSV      TO  DIS-EDIT.                      00002193
002277     MOVE    'A-CA-ALTRSV      = ' TO PRT-INFO.                   00002194
002278     MOVE    DIS-EDIT          TO PRT-AMT.                        00002195
002279     MOVE SPACES    TO    P-DATA.                                 00002196
002280     MOVE PRT-LINE  TO    P-DATA.                                 00002197
002281     MOVE     ' '        TO    X.                                 00002198
002282     MOVE      X         TO    P-CTL.                             00002199
002283     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002200
002284     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002201
002285                                                                  00002202
002286     MOVE    CA-AH-RESERV      TO  DIS-EDIT.                      00002203
002287     MOVE    'A-CA-RESERV      = ' TO PRT-INFO.                   00002204
002288     MOVE    DIS-EDIT          TO PRT-AMT.                        00002205
002289     MOVE SPACES    TO    P-DATA.                                 00002206
002290     MOVE PRT-LINE  TO    P-DATA.                                 00002207
002291     MOVE     ' '        TO    X.                                 00002208
002292     MOVE      X         TO    P-CTL.                             00002209
002293     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002210
002294     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002211
002295                                                                  00002212
002296     MOVE    CA-AH-REMAIN      TO  DIS-EDIT.                      00002213
002297     MOVE    'A-CA-REMAIN      = ' TO PRT-INFO.                   00002214
002298     MOVE    DIS-EDIT          TO PRT-AMT.                        00002215
002299     MOVE SPACES    TO    P-DATA.                                 00002216
002300     MOVE PRT-LINE  TO    P-DATA.                                 00002217
002301     MOVE     ' '        TO    X.                                 00002218
002302     MOVE      X         TO    P-CTL.                             00002219
002303     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002220
002304     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002221
002305                                                                  00002222
002306     MOVE    CA-AH-PAID        TO  DIS-EDIT.                      00002223
002307     MOVE    'A-CA-PAID        = ' TO PRT-INFO.                   00002224
002308     MOVE    DIS-EDIT          TO PRT-AMT.                        00002225
002309     MOVE SPACES    TO    P-DATA.                                 00002226
002310     MOVE PRT-LINE  TO    P-DATA.                                 00002227
002311     MOVE     ' '        TO    X.                                 00002228
002312     MOVE      X         TO    P-CTL.                             00002229
002313     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002230
002314     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002231
002315                                                                  00002232
002316     MOVE    CA-AH-C78         TO  DIS-EDIT.                      00002233
002317     MOVE    'A-CA-C78         = ' TO PRT-INFO.                   00002234
002318     MOVE    DIS-EDIT          TO PRT-AMT.                        00002235
002319     MOVE SPACES    TO    P-DATA.                                 00002236
002320     MOVE PRT-LINE  TO    P-DATA.                                 00002237
002321     MOVE     ' '        TO    X.                                 00002238
002322     MOVE      X         TO    P-CTL.                             00002239
002323     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002240
002324     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002241
002325                                                                  00002242
002326     MOVE    CA-AH-CRATA       TO  DIS-EDIT.                      00002243
002327     MOVE    'A-CA-CRATA       = ' TO PRT-INFO.                   00002244
002328     MOVE    DIS-EDIT          TO PRT-AMT.                        00002245
002329     MOVE SPACES    TO    P-DATA.                                 00002246
002330     MOVE PRT-LINE  TO    P-DATA.                                 00002247
002331     MOVE     ' '        TO    X.                                 00002248
002332     MOVE      X         TO    P-CTL.                             00002249
002333     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002250
002334     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002251
040114
040114     MOVE    CA-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-CA-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002335                                                                  00002252
002336     MOVE    CA-AH-TAX         TO  DIS-EDIT.                      00002253
002337     MOVE    'A-CA-TAX         = ' TO PRT-INFO.                   00002254
002338     MOVE    DIS-EDIT          TO PRT-AMT.                        00002255
002339     MOVE SPACES    TO    P-DATA.                                 00002256
002340     MOVE PRT-LINE  TO    P-DATA.                                 00002257
002341     MOVE     ' '        TO    X.                                 00002258
002342     MOVE      X         TO    P-CTL.                             00002259
002343     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002260
002344     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002261
002345                                                                  00002262
002346     MOVE    CA-AH-T78         TO  DIS-EDIT.                      00002263
002347     MOVE    'A-CA-T78         = ' TO PRT-INFO.                   00002264
002348     MOVE    DIS-EDIT          TO PRT-AMT.                        00002265
002349     MOVE SPACES    TO    P-DATA.                                 00002266
002350     MOVE PRT-LINE  TO    P-DATA.                                 00002267
002351     MOVE     ' '        TO    X.                                 00002268
002352     MOVE      X         TO    P-CTL.                             00002269
002353     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002270
002354     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002271
002355                                                                  00002272
002356     MOVE    CA-AH-TRATA       TO  DIS-EDIT.                      00002273
002357     MOVE    'A-CA-TRATA       = ' TO PRT-INFO.                   00002274
002358     MOVE    DIS-EDIT          TO PRT-AMT.                        00002275
002359     MOVE SPACES    TO    P-DATA.                                 00002276
002360     MOVE PRT-LINE  TO    P-DATA.                                 00002277
002361     MOVE     ' '        TO    X.                                 00002278
002362     MOVE      X         TO    P-CTL.                             00002279
002363     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002280
002364     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002281
040114
040114     MOVE    CA-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-CA-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002365                                                                  00002282
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002113
CIDMOD     MOVE     ' '        TO    X.                                 00002114
CIDMOD     MOVE      X         TO    P-CTL.                             00002115
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002116
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002117
CIDMOD                                                                  00002118
CIDMOD     MOVE    'O H I O        '  TO P-DATA.                        00002120
CIDMOD     MOVE     ' '        TO    X.                                 00002121
CIDMOD     MOVE      X         TO    P-CTL.                             00002122
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002123
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002124
CIDMOD                                                                  00002125
CIDMOD     MOVE SPACES TO P-DATA.                                       00002126
CIDMOD     MOVE     ' '        TO    X.                                 00002127
CIDMOD     MOVE      X         TO    P-CTL.                             00002128
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002129
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002130
CIDMOD                                                                  00002131
CIDMOD     MOVE    OH-AH-COUNT       TO  BEN-COUNT-P.                   00002132
CIDMOD     MOVE    'A-OH-REC-COUNT   =         ' TO PRT-INFO.           00002133
CIDMOD     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00002134
CIDMOD     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00002135
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002136
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002137
CIDMOD     MOVE     ' '        TO    X.                                 00002138
CIDMOD     MOVE      X         TO    P-CTL.                             00002139
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002140
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002141
CIDMOD                                                                  00002142
CIDMOD     MOVE    OH-AH-WRITTEN     TO  DIS-EDIT.                      00002143
CIDMOD     MOVE    'A-OH-WRITTEN     = ' TO PRT-INFO.                   00002144
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002145
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002146
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002147
CIDMOD     MOVE     ' '        TO    X.                                 00002148
CIDMOD     MOVE      X         TO    P-CTL.                             00002149
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002150
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002151
CIDMOD                                                                  00002152
CIDMOD     MOVE    OH-AH-P78         TO  DIS-EDIT.                      00002153
CIDMOD     MOVE    'A-OH-P78         = ' TO PRT-INFO.                   00002154
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002155
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002156
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002157
CIDMOD     MOVE     ' '        TO    X.                                 00002158
CIDMOD     MOVE      X         TO    P-CTL.                             00002159
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002160
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002161
CIDMOD                                                                  00002162
CIDMOD     MOVE    OH-AH-PRATA       TO  DIS-EDIT.                      00002163
CIDMOD     MOVE    'A-OH-PRATA       = ' TO PRT-INFO.                   00002164
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002165
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002166
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002167
CIDMOD     MOVE     ' '        TO    X.                                 00002168
CIDMOD     MOVE      X         TO    P-CTL.                             00002169
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002170
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002171
CIDMOD                                                                  00002172
CIDMOD     MOVE    OH-AH-DOMICILE    TO  DIS-EDIT.                      00002173
CIDMOD     MOVE    'A-OH-DOMICILE    = ' TO PRT-INFO.                   00002174
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002175
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002176
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002177
CIDMOD     MOVE     ' '        TO    X.                                 00002178
CIDMOD     MOVE      X         TO    P-CTL.                             00002179
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002180
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002181
CIDMOD                                                                  00002182
CIDMOD     MOVE    OH-AH-STATE       TO  DIS-EDIT.                      00002183
CIDMOD     MOVE    'A-OH-STATE       = ' TO PRT-INFO.                   00002184
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002185
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002186
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002187
CIDMOD     MOVE     ' '        TO    X.                                 00002188
CIDMOD     MOVE      X         TO    P-CTL.                             00002189
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002190
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002191
CIDMOD                                                                  00002192
CIDMOD     MOVE    OH-AH-ALTRSV      TO  DIS-EDIT.                      00002193
CIDMOD     MOVE    'A-OH-ALTRSV      = ' TO PRT-INFO.                   00002194
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002195
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002196
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002197
CIDMOD     MOVE     ' '        TO    X.                                 00002198
CIDMOD     MOVE      X         TO    P-CTL.                             00002199
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002200
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002201
CIDMOD                                                                  00002202
CIDMOD     MOVE    OH-AH-RESERV      TO  DIS-EDIT.                      00002203
CIDMOD     MOVE    'A-OH-RESERV      = ' TO PRT-INFO.                   00002204
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002205
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002206
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002207
CIDMOD     MOVE     ' '        TO    X.                                 00002208
CIDMOD     MOVE      X         TO    P-CTL.                             00002209
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002210
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002211
CIDMOD                                                                  00002212
CIDMOD     MOVE    OH-AH-REMAIN      TO  DIS-EDIT.                      00002213
CIDMOD     MOVE    'A-OH-REMAIN      = ' TO PRT-INFO.                   00002214
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002215
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002216
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002217
CIDMOD     MOVE     ' '        TO    X.                                 00002218
CIDMOD     MOVE      X         TO    P-CTL.                             00002219
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002220
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002221
CIDMOD                                                                  00002222
CIDMOD     MOVE    OH-AH-PAID        TO  DIS-EDIT.                      00002223
CIDMOD     MOVE    'A-OH-PAID        = ' TO PRT-INFO.                   00002224
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002225
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002226
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002227
CIDMOD     MOVE     ' '        TO    X.                                 00002228
CIDMOD     MOVE      X         TO    P-CTL.                             00002229
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002230
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002231
CIDMOD                                                                  00002232
CIDMOD     MOVE    OH-AH-C78         TO  DIS-EDIT.                      00002233
CIDMOD     MOVE    'A-OH-C78         = ' TO PRT-INFO.                   00002234
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002235
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002236
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002237
CIDMOD     MOVE     ' '        TO    X.                                 00002238
CIDMOD     MOVE      X         TO    P-CTL.                             00002239
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002240
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002241
CIDMOD                                                                  00002242
CIDMOD     MOVE    OH-AH-CRATA       TO  DIS-EDIT.                      00002243
CIDMOD     MOVE    'A-OH-CRATA       = ' TO PRT-INFO.                   00002244
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002245
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002246
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002247
CIDMOD     MOVE     ' '        TO    X.                                 00002248
CIDMOD     MOVE      X         TO    P-CTL.                             00002249
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002250
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002251
040114
040114     MOVE    OH-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-OH-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
CIDMOD                                                                  00002252
CIDMOD     MOVE    OH-AH-TAX         TO  DIS-EDIT.                      00002253
CIDMOD     MOVE    'A-OH-TAX         = ' TO PRT-INFO.                   00002254
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002255
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002256
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002257
CIDMOD     MOVE     ' '        TO    X.                                 00002258
CIDMOD     MOVE      X         TO    P-CTL.                             00002259
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002260
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002261
CIDMOD                                                                  00002262
CIDMOD     MOVE    OH-AH-T78         TO  DIS-EDIT.                      00002263
CIDMOD     MOVE    'A-OH-T78         = ' TO PRT-INFO.                   00002264
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002265
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002266
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002267
CIDMOD     MOVE     ' '        TO    X.                                 00002268
CIDMOD     MOVE      X         TO    P-CTL.                             00002269
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002270
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002271
CIDMOD                                                                  00002272
CIDMOD     MOVE    OH-AH-TRATA       TO  DIS-EDIT.                      00002273
CIDMOD     MOVE    'A-OH-TRATA       = ' TO PRT-INFO.                   00002274
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002275
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002276
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002277
CIDMOD     MOVE     ' '        TO    X.                                 00002278
CIDMOD     MOVE      X         TO    P-CTL.                             00002279
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002280
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002281
040114
040114     MOVE    OH-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-OH-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
CIDMOD                                                                  00002282
002366     MOVE SPACES TO P-DATA.                                       00002283
002367     MOVE     ' '        TO    X.                                 00002284
002368     MOVE      X         TO    P-CTL.                             00002285
002369     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002286
002370     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002287
002372                                                                  00002288
002373     MOVE    'O R E G O N  '  TO  P-DATA.                         00002289
002374     MOVE     ' '        TO    X.                                 00002290
002375     MOVE      X         TO    P-CTL.                             00002291
002376     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002292
002377     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002293
002378                                                                  00002294
002379     MOVE SPACES TO P-DATA.                                       00002295
002380     MOVE     ' '        TO    X.                                 00002296
002381     MOVE      X         TO    P-CTL.                             00002297
002382     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002298
002383     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002299
002384                                                                  00002300
002385     MOVE    OR-AH-COUNT       TO BEN-COUNT-P.                    00002301
002386     MOVE    'A-OR-REC-COUNT   =         ' TO PRT-INFO.           00002302
002387     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00002303
002388     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00002304
002389     MOVE SPACES    TO    P-DATA.                                 00002305
002390     MOVE PRT-LINE  TO    P-DATA.                                 00002306
002391     MOVE     ' '        TO    X.                                 00002307
002392     MOVE      X         TO    P-CTL.                             00002308
002393     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002309
002394     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002310
002395                                                                  00002311
002396     MOVE    OR-AH-WRITTEN     TO  DIS-EDIT.                      00002312
002397     MOVE    'A-OR-WRITTEN     = ' TO PRT-INFO.                   00002313
002398     MOVE    DIS-EDIT          TO PRT-AMT.                        00002314
002399     MOVE SPACES    TO    P-DATA.                                 00002315
002400     MOVE PRT-LINE  TO    P-DATA.                                 00002316
002401     MOVE     ' '        TO    X.                                 00002317
002402     MOVE      X         TO    P-CTL.                             00002318
002403     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002319
002404     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002320
002405                                                                  00002321
002406     MOVE    OR-AH-P78         TO  DIS-EDIT.                      00002322
002407     MOVE    'A-OR-P78         = ' TO PRT-INFO.                   00002323
002408     MOVE    DIS-EDIT          TO PRT-AMT.                        00002324
002409     MOVE SPACES    TO    P-DATA.                                 00002325
002410     MOVE PRT-LINE  TO    P-DATA.                                 00002326
002411     MOVE     ' '        TO    X.                                 00002327
002412     MOVE      X         TO    P-CTL.                             00002328
002413     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002329
002414     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002330
002415                                                                  00002331
002416     MOVE    OR-AH-PRATA       TO  DIS-EDIT.                      00002332
002417     MOVE    'A-OR-PRATA       = ' TO PRT-INFO.                   00002333
002418     MOVE    DIS-EDIT          TO PRT-AMT.                        00002334
002419     MOVE SPACES    TO    P-DATA.                                 00002335
002420     MOVE PRT-LINE  TO    P-DATA.                                 00002336
002421     MOVE     ' '        TO    X.                                 00002337
002422     MOVE      X         TO    P-CTL.                             00002338
002423     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002339
002424     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002340
002425                                                                  00002341
002426     MOVE    OR-AH-DOMICILE    TO  DIS-EDIT.                      00002342
002427     MOVE    'A-OR-DOMICILE    = ' TO PRT-INFO.                   00002343
002428     MOVE    DIS-EDIT          TO PRT-AMT.                        00002344
002429     MOVE SPACES    TO    P-DATA.                                 00002345
002430     MOVE PRT-LINE  TO    P-DATA.                                 00002346
002431     MOVE     ' '        TO    X.                                 00002347
002432     MOVE      X         TO    P-CTL.                             00002348
002433     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002349
002434     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002350
002435                                                                  00002351
002436     MOVE    OR-AH-STATE       TO  DIS-EDIT.                      00002352
002437     MOVE    'A-OR-STATE       = ' TO PRT-INFO.                   00002353
002438     MOVE    DIS-EDIT          TO PRT-AMT.                        00002354
002439     MOVE SPACES    TO    P-DATA.                                 00002355
002440     MOVE PRT-LINE  TO    P-DATA.                                 00002356
002441     MOVE     ' '        TO    X.                                 00002357
002442     MOVE      X         TO    P-CTL.                             00002358
002443     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002359
002444     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002360
002445                                                                  00002361
002446     MOVE    OR-AH-ALTRSV      TO  DIS-EDIT.                      00002362
002447     MOVE    'A-OR-ALTRSV      = ' TO PRT-INFO.                   00002363
002448     MOVE    DIS-EDIT          TO PRT-AMT.                        00002364
002449     MOVE SPACES    TO    P-DATA.                                 00002365
002450     MOVE PRT-LINE  TO    P-DATA.                                 00002366
002451     MOVE     ' '        TO    X.                                 00002367
002452     MOVE      X         TO    P-CTL.                             00002368
002453     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002369
002454     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002370
002455                                                                  00002371
002456     MOVE    OR-AH-RESERV      TO  DIS-EDIT.                      00002372
002457     MOVE    'A-OR-RESERV      = ' TO PRT-INFO.                   00002373
002458     MOVE    DIS-EDIT          TO PRT-AMT.                        00002374
002459     MOVE SPACES    TO    P-DATA.                                 00002375
002460     MOVE PRT-LINE  TO    P-DATA.                                 00002376
002461     MOVE     ' '        TO    X.                                 00002377
002462     MOVE      X         TO    P-CTL.                             00002378
002463     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002379
002464     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002380
002465                                                                  00002381
002466     MOVE    OR-AH-REMAIN      TO  DIS-EDIT.                      00002382
002467     MOVE    'A-OR-REMAIN      = ' TO PRT-INFO.                   00002383
002468     MOVE    DIS-EDIT          TO PRT-AMT.                        00002384
002469     MOVE SPACES    TO    P-DATA.                                 00002385
002470     MOVE PRT-LINE  TO    P-DATA.                                 00002386
002471     MOVE     ' '        TO    X.                                 00002387
002472     MOVE      X         TO    P-CTL.                             00002388
002473     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002389
002474     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002390
002475                                                                  00002391
002476     MOVE    OR-AH-PAID        TO  DIS-EDIT.                      00002392
002477     MOVE    'A-OR-PAID        = ' TO PRT-INFO.                   00002393
002478     MOVE    DIS-EDIT          TO PRT-AMT.                        00002394
002479     MOVE SPACES    TO    P-DATA.                                 00002395
002480     MOVE PRT-LINE  TO    P-DATA.                                 00002396
002481     MOVE     ' '        TO    X.                                 00002397
002482     MOVE      X         TO    P-CTL.                             00002398
002483     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002399
002484     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002400
002485                                                                  00002401
002486     MOVE    OR-AH-C78         TO  DIS-EDIT.                      00002402
002487     MOVE    'A-OR-C78         = ' TO PRT-INFO.                   00002403
002488     MOVE    DIS-EDIT          TO PRT-AMT.                        00002404
002489     MOVE SPACES    TO    P-DATA.                                 00002405
002490     MOVE PRT-LINE  TO    P-DATA.                                 00002406
002491     MOVE     ' '        TO    X.                                 00002407
002492     MOVE      X         TO    P-CTL.                             00002408
002493     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002409
002494     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002410
002495                                                                  00002411
002496     MOVE    OR-AH-CRATA       TO  DIS-EDIT.                      00002412
002497     MOVE    'A-OR-CRATA       = ' TO PRT-INFO.                   00002413
002498     MOVE    DIS-EDIT          TO PRT-AMT.                        00002414
002499     MOVE SPACES    TO    P-DATA.                                 00002415
002500     MOVE PRT-LINE  TO    P-DATA.                                 00002416
002501     MOVE     ' '        TO    X.                                 00002417
002502     MOVE      X         TO    P-CTL.                             00002418
002503     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002419
002504     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002420
040114
040114     MOVE    OR-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-OR-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002505                                                                  00002421
002506     MOVE    OR-AH-TAX         TO  DIS-EDIT.                      00002422
002507     MOVE    'A-OR-TAX         = ' TO PRT-INFO.                   00002423
002508     MOVE    DIS-EDIT          TO PRT-AMT.                        00002424
002509     MOVE SPACES    TO    P-DATA.                                 00002425
002510     MOVE PRT-LINE  TO    P-DATA.                                 00002426
002511     MOVE     ' '        TO    X.                                 00002427
002512     MOVE      X         TO    P-CTL.                             00002428
002513     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002429
002514     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002430
002515                                                                  00002431
002516     MOVE    OR-AH-T78         TO  DIS-EDIT.                      00002432
002517     MOVE    'A-OR-T78         = ' TO PRT-INFO.                   00002433
002518     MOVE    DIS-EDIT          TO PRT-AMT.                        00002434
002519     MOVE SPACES    TO    P-DATA.                                 00002435
002520     MOVE PRT-LINE  TO    P-DATA.                                 00002436
002521     MOVE     ' '        TO    X.                                 00002437
002522     MOVE      X         TO    P-CTL.                             00002438
002523     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002439
002524     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002440
002525                                                                  00002441
002526     MOVE    OR-AH-TRATA       TO  DIS-EDIT.                      00002442
002527     MOVE    'A-OR-TRATA       = ' TO PRT-INFO.                   00002443
002528     MOVE    DIS-EDIT          TO PRT-AMT.                        00002444
002529     MOVE SPACES    TO    P-DATA.                                 00002445
002530     MOVE PRT-LINE  TO    P-DATA.                                 00002446
002531     MOVE     ' '        TO    X.                                 00002447
002532     MOVE      X         TO    P-CTL.                             00002448
002533     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002449
002534     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002450
040114
040114     MOVE    OR-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-OR-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002535                                                                  00002451
002536     MOVE SPACES    TO    P-DATA.                                 00002452
002537     MOVE     ' '        TO    X.                                 00002453
002538     MOVE      X         TO    P-CTL.                             00002454
002539     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002455
002540     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002456
CIDMOD                                                                  00002288
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002452
CIDMOD     MOVE     ' '        TO    X.                                 00002453
CIDMOD     MOVE      X         TO    P-CTL.                             00002454
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002455
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002456
CIDMOD                                                                  00002288
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002452
CIDMOD     MOVE     ' '        TO    X.                                 00002453
CIDMOD     MOVE      X         TO    P-CTL.                             00002454
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002455
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002456
CIDMOD                                                                  00002288
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002452
CIDMOD     MOVE     ' '        TO    X.                                 00002453
CIDMOD     MOVE      X         TO    P-CTL.                             00002454
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002455
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002456
CIDMOD                                                                  00002288
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002452
CIDMOD     MOVE     ' '        TO    X.                                 00002453
CIDMOD     MOVE      X         TO    P-CTL.                             00002454
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002455
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002456
CIDMOD                                                                  00002288
CIDMOD     MOVE    'T E X A S    '  TO  P-DATA.                         00002289
CIDMOD     MOVE     ' '        TO    X.                                 00002290
CIDMOD     MOVE      X         TO    P-CTL.                             00002291
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002292
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002293
CIDMOD                                                                  00002294
CIDMOD     MOVE SPACES TO P-DATA.                                       00002295
CIDMOD     MOVE     ' '        TO    X.                                 00002296
CIDMOD     MOVE      X         TO    P-CTL.                             00002297
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002298
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002299
CIDMOD                                                                  00002300
CIDMOD     MOVE    TX-AH-COUNT       TO BEN-COUNT-P.                    00002301
CIDMOD     MOVE    'A-TX-REC-COUNT   =         ' TO PRT-INFO.           00002302
CIDMOD     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00002303
CIDMOD     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00002304
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002305
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002306
CIDMOD     MOVE     ' '        TO    X.                                 00002307
CIDMOD     MOVE      X         TO    P-CTL.                             00002308
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002309
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002310
CIDMOD                                                                  00002311
CIDMOD     MOVE    TX-AH-WRITTEN     TO  DIS-EDIT.                      00002312
CIDMOD     MOVE    'A-TX-WRITTEN     = ' TO PRT-INFO.                   00002313
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002314
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002315
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002316
CIDMOD     MOVE     ' '        TO    X.                                 00002317
CIDMOD     MOVE      X         TO    P-CTL.                             00002318
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002319
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002320
CIDMOD                                                                  00002321
CIDMOD     MOVE    TX-AH-P78         TO  DIS-EDIT.                      00002322
CIDMOD     MOVE    'A-TX-P78         = ' TO PRT-INFO.                   00002323
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002324
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002325
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002326
CIDMOD     MOVE     ' '        TO    X.                                 00002327
CIDMOD     MOVE      X         TO    P-CTL.                             00002328
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002329
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002330
CIDMOD                                                                  00002331
CIDMOD     MOVE    TX-AH-PRATA       TO  DIS-EDIT.                      00002332
CIDMOD     MOVE    'A-TX-PRATA       = ' TO PRT-INFO.                   00002333
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002334
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002335
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002336
CIDMOD     MOVE     ' '        TO    X.                                 00002337
CIDMOD     MOVE      X         TO    P-CTL.                             00002338
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002339
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002340
CIDMOD                                                                  00002341
CIDMOD     MOVE    TX-AH-DOMICILE    TO  DIS-EDIT.                      00002342
CIDMOD     MOVE    'A-TX-DOMICILE    = ' TO PRT-INFO.                   00002343
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002344
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002345
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002346
CIDMOD     MOVE     ' '        TO    X.                                 00002347
CIDMOD     MOVE      X         TO    P-CTL.                             00002348
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002349
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002350
CIDMOD                                                                  00002351
CIDMOD     MOVE    TX-AH-STATE       TO  DIS-EDIT.                      00002352
CIDMOD     MOVE    'A-TX-STATE       = ' TO PRT-INFO.                   00002353
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002354
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002355
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002356
CIDMOD     MOVE     ' '        TO    X.                                 00002357
CIDMOD     MOVE      X         TO    P-CTL.                             00002358
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002359
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002360
CIDMOD                                                                  00002361
CIDMOD     MOVE    TX-AH-ALTRSV      TO  DIS-EDIT.                      00002362
CIDMOD     MOVE    'A-TX-ALTRSV      = ' TO PRT-INFO.                   00002363
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002364
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002365
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002366
CIDMOD     MOVE     ' '        TO    X.                                 00002367
CIDMOD     MOVE      X         TO    P-CTL.                             00002368
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002369
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002370
CIDMOD                                                                  00002371
CIDMOD     MOVE    TX-AH-RESERV      TO  DIS-EDIT.                      00002372
CIDMOD     MOVE    'A-TX-RESERV      = ' TO PRT-INFO.                   00002373
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002374
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002375
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002376
CIDMOD     MOVE     ' '        TO    X.                                 00002377
CIDMOD     MOVE      X         TO    P-CTL.                             00002378
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002379
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002380
CIDMOD                                                                  00002381
CIDMOD     MOVE    TX-AH-REMAIN      TO  DIS-EDIT.                      00002382
CIDMOD     MOVE    'A-TX-REMAIN      = ' TO PRT-INFO.                   00002383
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002384
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002385
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002386
CIDMOD     MOVE     ' '        TO    X.                                 00002387
CIDMOD     MOVE      X         TO    P-CTL.                             00002388
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002389
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002390
CIDMOD                                                                  00002391
CIDMOD     MOVE    TX-AH-PAID        TO  DIS-EDIT.                      00002392
CIDMOD     MOVE    'A-TX-PAID        = ' TO PRT-INFO.                   00002393
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002394
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002395
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002396
CIDMOD     MOVE     ' '        TO    X.                                 00002397
CIDMOD     MOVE      X         TO    P-CTL.                             00002398
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002399
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002400
CIDMOD                                                                  00002401
CIDMOD     MOVE    TX-AH-C78         TO  DIS-EDIT.                      00002402
CIDMOD     MOVE    'A-TX-C78         = ' TO PRT-INFO.                   00002403
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002404
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002405
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002406
CIDMOD     MOVE     ' '        TO    X.                                 00002407
CIDMOD     MOVE      X         TO    P-CTL.                             00002408
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002409
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002410
CIDMOD                                                                  00002411
CIDMOD     MOVE    TX-AH-CRATA       TO  DIS-EDIT.                      00002412
CIDMOD     MOVE    'A-TX-CRATA       = ' TO PRT-INFO.                   00002413
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002414
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002415
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002416
CIDMOD     MOVE     ' '        TO    X.                                 00002417
CIDMOD     MOVE      X         TO    P-CTL.                             00002418
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002419
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002420
040114
040114     MOVE    TX-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-TX-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
CIDMOD                                                                  00002421
CIDMOD     MOVE    TX-AH-TAX         TO  DIS-EDIT.                      00002422
CIDMOD     MOVE    'A-TX-TAX         = ' TO PRT-INFO.                   00002423
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002424
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002425
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002426
CIDMOD     MOVE     ' '        TO    X.                                 00002427
CIDMOD     MOVE      X         TO    P-CTL.                             00002428
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002429
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002430
CIDMOD                                                                  00002431
CIDMOD     MOVE    TX-AH-T78         TO  DIS-EDIT.                      00002432
CIDMOD     MOVE    'A-TX-T78         = ' TO PRT-INFO.                   00002433
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002434
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002435
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002436
CIDMOD     MOVE     ' '        TO    X.                                 00002437
CIDMOD     MOVE      X         TO    P-CTL.                             00002438
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002439
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002440
CIDMOD                                                                  00002441
CIDMOD     MOVE    TX-AH-TRATA       TO  DIS-EDIT.                      00002442
CIDMOD     MOVE    'A-TX-TRATA       = ' TO PRT-INFO.                   00002443
CIDMOD     MOVE    DIS-EDIT          TO PRT-AMT.                        00002444
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002445
CIDMOD     MOVE PRT-LINE  TO    P-DATA.                                 00002446
CIDMOD     MOVE     ' '        TO    X.                                 00002447
CIDMOD     MOVE      X         TO    P-CTL.                             00002448
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002449
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002450
040114
040114     MOVE    TX-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-TX-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
CIDMOD                                                                  00002451
CIDMOD     MOVE SPACES    TO    P-DATA.                                 00002452
CIDMOD     MOVE     ' '        TO    X.                                 00002453
CIDMOD     MOVE      X         TO    P-CTL.                             00002454
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002455
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002456
002542                                                                  00002457
002543     MOVE    'W A S H I N G T O N       ' TO P-DATA.              00002458
002544     MOVE     ' '        TO    X.                                 00002459
002545     MOVE      X         TO    P-CTL.                             00002460
002546     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002461
002547     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002462
002548                                                                  00002463
002549     MOVE SPACES    TO    P-DATA.                                 00002464
002550     MOVE     ' '        TO    X.                                 00002465
002551     MOVE      X         TO    P-CTL.                             00002466
002552     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002467
002553     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002468
002554                                                                  00002469
002555     MOVE    WA-AH-COUNT       TO  BEN-COUNT-P.                   00002470
002556     MOVE    'A-WA-REC-COUNT   =         ' TO PRT-INFO.           00002471
002557     MOVE BEN-COUNT-P                   TO ALL-BEN-COUNT-P.       00002472
002558     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00002473
002559     MOVE SPACES    TO    P-DATA.                                 00002474
002560     MOVE PRT-LINE  TO    P-DATA.                                 00002475
002561     MOVE     ' '        TO    X.                                 00002476
002562     MOVE      X         TO    P-CTL.                             00002477
002563     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002478
002564     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002479
002565                                                                  00002480
002566     MOVE    WA-AH-WRITTEN     TO  DIS-EDIT.                      00002481
002567     MOVE    'A-WA-WRITTEN     = ' TO PRT-INFO.                   00002482
002568     MOVE    DIS-EDIT          TO PRT-AMT.                        00002483
002569     MOVE SPACES    TO    P-DATA.                                 00002484
002570     MOVE PRT-LINE  TO    P-DATA.                                 00002485
002571     MOVE     ' '        TO    X.                                 00002486
002572     MOVE      X         TO    P-CTL.                             00002487
002573     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002488
002574     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002489
002575                                                                  00002490
002576     MOVE    WA-AH-P78         TO  DIS-EDIT.                      00002491
002577     MOVE    'A-WA-P78         = ' TO PRT-INFO.                   00002492
002578     MOVE    DIS-EDIT          TO PRT-AMT.                        00002493
002579     MOVE SPACES    TO    P-DATA.                                 00002494
002580     MOVE PRT-LINE  TO    P-DATA.                                 00002495
002581     MOVE     ' '        TO    X.                                 00002496
002582     MOVE      X         TO    P-CTL.                             00002497
002583     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002498
002584     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002499
002585                                                                  00002500
002586     MOVE    WA-AH-PRATA       TO  DIS-EDIT.                      00002501
002587     MOVE    'A-WA-PRATA       = ' TO PRT-INFO.                   00002502
002588     MOVE    DIS-EDIT          TO PRT-AMT.                        00002503
002589     MOVE SPACES    TO    P-DATA.                                 00002504
002590     MOVE PRT-LINE  TO    P-DATA.                                 00002505
002591     MOVE     ' '        TO    X.                                 00002506
002592     MOVE      X         TO    P-CTL.                             00002507
002593     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002508
002594     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002509
002595                                                                  00002510
002596     MOVE    WA-AH-DOMICILE    TO  DIS-EDIT.                      00002511
002597     MOVE    'A-WA-DOMICILE    = ' TO PRT-INFO.                   00002512
002598     MOVE    DIS-EDIT          TO PRT-AMT.                        00002513
002599     MOVE SPACES    TO    P-DATA.                                 00002514
002600     MOVE PRT-LINE  TO    P-DATA.                                 00002515
002601     MOVE     ' '        TO    X.                                 00002516
002602     MOVE      X         TO    P-CTL.                             00002517
002603     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002518
002604     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002519
002605                                                                  00002520
002606     MOVE    WA-AH-STATE       TO  DIS-EDIT.                      00002521
002607     MOVE    'A-WA-STATE       = ' TO PRT-INFO.                   00002522
002608     MOVE    DIS-EDIT          TO PRT-AMT.                        00002523
002609     MOVE SPACES    TO    P-DATA.                                 00002524
002610     MOVE PRT-LINE  TO    P-DATA.                                 00002525
002611     MOVE     ' '        TO    X.                                 00002526
002612     MOVE      X         TO    P-CTL.                             00002527
002613     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002528
002614     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002529
002615                                                                  00002530
002616     MOVE    WA-AH-ALTRSV      TO  DIS-EDIT.                      00002531
002617     MOVE    'A-WA-ALTRSV      = ' TO PRT-INFO.                   00002532
002618     MOVE    DIS-EDIT          TO PRT-AMT.                        00002533
002619     MOVE SPACES    TO    P-DATA.                                 00002534
002620     MOVE PRT-LINE  TO    P-DATA.                                 00002535
002621     MOVE     ' '        TO    X.                                 00002536
002622     MOVE      X         TO    P-CTL.                             00002537
002623     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002538
002624     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002539
002625                                                                  00002540
002626     MOVE    WA-AH-RESERV      TO  DIS-EDIT.                      00002541
002627     MOVE    'A-WA-RESERV      = ' TO PRT-INFO.                   00002542
002628     MOVE    DIS-EDIT          TO PRT-AMT.                        00002543
002629     MOVE SPACES    TO    P-DATA.                                 00002544
002630     MOVE PRT-LINE  TO    P-DATA.                                 00002545
002631     MOVE     ' '        TO    X.                                 00002546
002632     MOVE      X         TO    P-CTL.                             00002547
002633     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002548
002634     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002549
002635                                                                  00002550
002636     MOVE    WA-AH-REMAIN      TO  DIS-EDIT.                      00002551
002637     MOVE    'A-WA-REMAIN      = ' TO PRT-INFO.                   00002552
002638     MOVE    DIS-EDIT          TO PRT-AMT.                        00002553
002639     MOVE SPACES    TO    P-DATA.                                 00002554
002640     MOVE PRT-LINE  TO    P-DATA.                                 00002555
002641     MOVE     ' '        TO    X.                                 00002556
002642     MOVE      X         TO    P-CTL.                             00002557
002643     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002558
002644     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002559
002645                                                                  00002560
002646     MOVE    WA-AH-PAID        TO  DIS-EDIT.                      00002561
002647     MOVE    'A-WA-PAID        = ' TO PRT-INFO.                   00002562
002648     MOVE    DIS-EDIT          TO PRT-AMT.                        00002563
002649     MOVE SPACES    TO    P-DATA.                                 00002564
002650     MOVE PRT-LINE  TO    P-DATA.                                 00002565
002651     MOVE     ' '        TO    X.                                 00002566
002652     MOVE      X         TO    P-CTL.                             00002567
002653     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002568
002654     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002569
002655                                                                  00002570
002656     MOVE    WA-AH-C78         TO  DIS-EDIT.                      00002571
002657     MOVE    'A-WA-C78         = ' TO PRT-INFO.                   00002572
002658     MOVE    DIS-EDIT          TO PRT-AMT.                        00002573
002659     MOVE SPACES    TO    P-DATA.                                 00002574
002660     MOVE PRT-LINE  TO    P-DATA.                                 00002575
002661     MOVE     ' '        TO    X.                                 00002576
002662     MOVE      X         TO    P-CTL.                             00002577
002663     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002578
002664     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002579
002665                                                                  00002580
002666     MOVE    WA-AH-CRATA       TO  DIS-EDIT.                      00002581
002667     MOVE    'A-WA-CRATA       = ' TO PRT-INFO.                   00002582
002668     MOVE    DIS-EDIT          TO PRT-AMT.                        00002583
002669     MOVE SPACES    TO    P-DATA.                                 00002584
002670     MOVE PRT-LINE  TO    P-DATA.                                 00002585
002671     MOVE     ' '        TO    X.                                 00002586
002672     MOVE      X         TO    P-CTL.                             00002587
002673     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002588
002674     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002589
040114
040114     MOVE    WA-AH-CDOMI       TO  DIS-EDIT.
040114     MOVE    'A-WA-CDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002675                                                                  00002590
002676     MOVE    WA-AH-TAX         TO  DIS-EDIT.                      00002591
002677     MOVE    'A-WA-TAX         = ' TO PRT-INFO.                   00002592
002678     MOVE    DIS-EDIT          TO PRT-AMT.                        00002593
002679     MOVE SPACES    TO    P-DATA.                                 00002594
002680     MOVE PRT-LINE  TO    P-DATA.                                 00002595
002681     MOVE     ' '        TO    X.                                 00002596
002682     MOVE      X         TO    P-CTL.                             00002597
002683     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002598
002684     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002599
002685                                                                  00002600
002686     MOVE    WA-AH-T78         TO  DIS-EDIT.                      00002601
002687     MOVE    'A-WA-T78         = ' TO PRT-INFO.                   00002602
002688     MOVE    DIS-EDIT          TO PRT-AMT.                        00002603
002689     MOVE SPACES    TO    P-DATA.                                 00002604
002690     MOVE PRT-LINE  TO    P-DATA.                                 00002605
002691     MOVE     ' '        TO    X.                                 00002606
002692     MOVE      X         TO    P-CTL.                             00002607
002693     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002608
002694     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002609
002695                                                                  00002610
002696     MOVE    WA-AH-TRATA       TO  DIS-EDIT.                      00002611
002697     MOVE    'A-WA-TRATA       = ' TO PRT-INFO.                   00002612
002698     MOVE    DIS-EDIT          TO PRT-AMT.                        00002613
002699     MOVE SPACES    TO    P-DATA.                                 00002614
002700     MOVE PRT-LINE  TO    P-DATA.                                 00002615
002701     MOVE     ' '        TO    X.                                 00002616
002702     MOVE      X         TO    P-CTL.                             00002617
002703     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002618
002704     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002619
040114
040114     MOVE    WA-AH-TDOMI       TO  DIS-EDIT.
040114     MOVE    'A-WA-TDOMI       = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002706                                                                  00002620
002707     MOVE      SPACES    TO    P-DATA.                            00002621
002708     MOVE     '1'        TO    X.                                 00002622
002709     MOVE      X         TO    P-CTL.                             00002623
002710     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002624
002711     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002625
002712                                                                  00002626
002713     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00002627
002714     MOVE     '1'        TO    X.                                 00002628
002715     MOVE      X         TO    P-CTL.                             00002629
002716     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002630
002717     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002631
002718                                                                  00002632
002719     MOVE    '========================= ' TO P-DATA.              00002633
002720     MOVE     ' '        TO    X.                                 00002634
002721     MOVE      X         TO    P-CTL.                             00002635
002722     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002636
002723     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002637
002724                                                                  00002638
002725     MOVE SPACES    TO    P-DATA.                                 00002639
002726     MOVE     ' '        TO    X.                                 00002640
002727     MOVE      X         TO    P-CTL.                             00002641
002728     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002642
002729     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002643
002730                                                                  00002644
002731     MOVE SPACES    TO    P-DATA.                                 00002645
002732     MOVE     ' '        TO    X.                                 00002646
002733     MOVE      X         TO    P-CTL.                             00002647
002734     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002648
002735     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002649
002736                                                                  00002650
002737     MOVE                                                         00002651
002738     'A H   G R A N D   T O T A L S,   A L L   S T A T E S '      00002652
002739          TO    P-DATA.                                           00002653
002740     MOVE     ' '        TO    X.                                 00002654
002741     MOVE      X         TO    P-CTL.                             00002655
002742     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002656
002743     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002657
002744                                                                  00002658
002745     MOVE SPACES    TO    P-DATA.                                 00002659
002746     MOVE     ' '        TO    X.                                 00002660
002747     MOVE      X         TO    P-CTL.                             00002661
002748     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002662
002749     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002663
002750                                                                  00002664
002751     MOVE    ALL-AH-COUNT      TO  BEN-COUNT-P.                   00002665
002752     MOVE    'A-ALL-REC-COUNT  =         ' TO PRT-INFO.           00002666
002753     MOVE    BEN-COUNT-P       TO ALL-BEN-COUNT-P.                00002667
002754     MOVE ALL-BEN-COUNT                 TO PRT-AMT.               00002668
002755     MOVE SPACES    TO    P-DATA.                                 00002669
002756     MOVE PRT-LINE  TO    P-DATA.                                 00002670
002757     MOVE     ' '        TO    X.                                 00002671
002758     MOVE      X         TO    P-CTL.                             00002672
002759     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002673
002760     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002674
002761                                                                  00002675
002762     MOVE    ALL-AH-WRITTEN    TO  DIS-EDIT.                      00002676
002763     MOVE    'A-ALL-WRITTEN    = ' TO PRT-INFO.                   00002677
002764     MOVE    DIS-EDIT          TO PRT-AMT.                        00002678
002765     MOVE SPACES    TO    P-DATA.                                 00002679
002766     MOVE PRT-LINE  TO    P-DATA.                                 00002680
002767     MOVE     ' '        TO    X.                                 00002681
002768     MOVE      X         TO    P-CTL.                             00002682
002769     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002683
002770     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002684
002771                                                                  00002685
002772     MOVE    ALL-AH-P78        TO  DIS-EDIT.                      00002686
002773     MOVE    'A-ALL-P78        = ' TO PRT-INFO.                   00002687
002774     MOVE    DIS-EDIT          TO PRT-AMT.                        00002688
002775     MOVE SPACES    TO    P-DATA.                                 00002689
002776     MOVE PRT-LINE  TO    P-DATA.                                 00002690
002777     MOVE     ' '        TO    X.                                 00002691
002778     MOVE      X         TO    P-CTL.                             00002692
002779     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002693
002780     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002694
002781                                                                  00002695
002782     MOVE    ALL-AH-PRATA      TO  DIS-EDIT.                      00002696
002783     MOVE    'A-ALL-PRATA      = ' TO PRT-INFO.                   00002697
002784     MOVE    DIS-EDIT          TO PRT-AMT.                        00002698
002785     MOVE SPACES    TO    P-DATA.                                 00002699
002786     MOVE PRT-LINE  TO    P-DATA.                                 00002700
002787     MOVE     ' '        TO    X.                                 00002701
002788     MOVE      X         TO    P-CTL.                             00002702
002789     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002703
002790     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002704
002791                                                                  00002705
002792     MOVE    ALL-AH-DOMICILE   TO  DIS-EDIT.                      00002706
002793     MOVE    'A-ALL-DOMICILE   = ' TO PRT-INFO.                   00002707
002794     MOVE    DIS-EDIT          TO PRT-AMT.                        00002708
002795     MOVE SPACES    TO    P-DATA.                                 00002709
002796     MOVE PRT-LINE  TO    P-DATA.                                 00002710
002797     MOVE     ' '        TO    X.                                 00002711
002798     MOVE      X         TO    P-CTL.                             00002712
002799     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002713
002800     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002714
002801                                                                  00002715
002802     MOVE    ALL-AH-STATE      TO  DIS-EDIT.                      00002716
002803     MOVE    'A-ALL-STATE      = ' TO PRT-INFO.                   00002717
002804     MOVE    DIS-EDIT          TO PRT-AMT.                        00002718
002805     MOVE SPACES    TO    P-DATA.                                 00002719
002806     MOVE PRT-LINE  TO    P-DATA.                                 00002720
002807     MOVE     ' '        TO    X.                                 00002721
002808     MOVE      X         TO    P-CTL.                             00002722
002809     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002723
002810     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002724
002811                                                                  00002725
002812     MOVE    ALL-AH-ALTRSV     TO  DIS-EDIT.                      00002726
002813     MOVE    'A-ALL-ALTRSV     = ' TO PRT-INFO.                   00002727
002814     MOVE    DIS-EDIT          TO PRT-AMT.                        00002728
002815     MOVE SPACES    TO    P-DATA.                                 00002729
002816     MOVE PRT-LINE  TO    P-DATA.                                 00002730
002817     MOVE     ' '        TO    X.                                 00002731
002818     MOVE      X         TO    P-CTL.                             00002732
002819     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002733
002820     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002734
002821                                                                  00002735
002822     MOVE    ALL-AH-RESERV     TO  DIS-EDIT.                      00002736
002823     MOVE    'A-ALL-RESERV     = ' TO PRT-INFO.                   00002737
002824     MOVE    DIS-EDIT          TO PRT-AMT.                        00002738
002825     MOVE SPACES    TO    P-DATA.                                 00002739
002826     MOVE PRT-LINE  TO    P-DATA.                                 00002740
002827     MOVE     ' '        TO    X.                                 00002741
002828     MOVE      X         TO    P-CTL.                             00002742
002829     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002743
002830     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002744
002831                                                                  00002745
002832     MOVE    ALL-AH-REMAIN     TO  DIS-EDIT.                      00002746
002833     MOVE    'A-ALL-REMAIN     = ' TO PRT-INFO.                   00002747
002834     MOVE    DIS-EDIT          TO PRT-AMT.                        00002748
002835     MOVE SPACES    TO    P-DATA.                                 00002749
002836     MOVE PRT-LINE  TO    P-DATA.                                 00002750
002837     MOVE     ' '        TO    X.                                 00002751
002838     MOVE      X         TO    P-CTL.                             00002752
002839     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002753
002840     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002754
002841                                                                  00002755
002842     MOVE    ALL-AH-PAID       TO  DIS-EDIT.                      00002756
002843     MOVE    'A-ALL-PAID       = ' TO PRT-INFO.                   00002757
002844     MOVE    DIS-EDIT          TO PRT-AMT.                        00002758
002845     MOVE SPACES    TO    P-DATA.                                 00002759
002846     MOVE PRT-LINE  TO    P-DATA.                                 00002760
002847     MOVE     ' '        TO    X.                                 00002761
002848     MOVE      X         TO    P-CTL.                             00002762
002849     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002763
002850     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002764
002851                                                                  00002765
002852     MOVE    ALL-AH-C78        TO  DIS-EDIT.                      00002766
002853     MOVE    'A-ALL-C78        = ' TO PRT-INFO.                   00002767
002854     MOVE    DIS-EDIT          TO PRT-AMT.                        00002768
002855     MOVE SPACES    TO    P-DATA.                                 00002769
002856     MOVE PRT-LINE  TO    P-DATA.                                 00002770
002857     MOVE     ' '        TO    X.                                 00002771
002858     MOVE      X         TO    P-CTL.                             00002772
002859     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002773
002860     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002774
002861                                                                  00002775
002862     MOVE    ALL-AH-CRATA      TO  DIS-EDIT.                      00002776
002863     MOVE    'A-ALL-CRATA      = ' TO PRT-INFO.                   00002777
002864     MOVE    DIS-EDIT          TO PRT-AMT.                        00002778
002865     MOVE SPACES    TO    P-DATA.                                 00002779
002866     MOVE PRT-LINE  TO    P-DATA.                                 00002780
002867     MOVE     ' '        TO    X.                                 00002781
002868     MOVE      X         TO    P-CTL.                             00002782
002869     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002783
002870     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002784
040114
040114     MOVE    ALL-AH-CDOMI      TO  DIS-EDIT.
040114     MOVE    'A-ALL-CDOMI      = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002871                                                                  00002785
002872     MOVE    ALL-AH-TAX        TO  DIS-EDIT.                      00002786
002873     MOVE    'A-ALL-TAX        = ' TO PRT-INFO.                   00002787
002874     MOVE    DIS-EDIT          TO PRT-AMT.                        00002788
002875     MOVE SPACES    TO    P-DATA.                                 00002789
002876     MOVE PRT-LINE  TO    P-DATA.                                 00002790
002877     MOVE     ' '        TO    X.                                 00002791
002878     MOVE      X         TO    P-CTL.                             00002792
002879     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002793
002880     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002794
002881                                                                  00002795
002882     MOVE    ALL-AH-T78        TO  DIS-EDIT.                      00002796
002883     MOVE    'A-ALL-T78        = ' TO PRT-INFO.                   00002797
002884     MOVE    DIS-EDIT          TO PRT-AMT.                        00002798
002885     MOVE SPACES    TO    P-DATA.                                 00002799
002886     MOVE PRT-LINE  TO    P-DATA.                                 00002800
002887     MOVE     ' '        TO    X.                                 00002801
002888     MOVE      X         TO    P-CTL.                             00002802
002889     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002803
002890     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002804
002891                                                                  00002805
002892     MOVE    ALL-AH-TRATA      TO  DIS-EDIT.                      00002806
002893     MOVE    'A-ALL-TRATA      = ' TO PRT-INFO.                   00002807
002894     MOVE    DIS-EDIT          TO PRT-AMT.                        00002808
002895     MOVE SPACES    TO    P-DATA.                                 00002809
002896     MOVE PRT-LINE  TO    P-DATA.                                 00002810
002897     MOVE     ' '        TO    X.                                 00002811
002898     MOVE      X         TO    P-CTL.                             00002812
002899     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002813
002900     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002814
040114
040114     MOVE    ALL-AH-TDOMI      TO  DIS-EDIT.
040114     MOVE    'A-ALL-TDOMI      = ' TO PRT-INFO.
040114     MOVE    DIS-EDIT          TO PRT-AMT.
040114     MOVE SPACES    TO    P-DATA.
040114     MOVE PRT-LINE  TO    P-DATA.
040114     MOVE     ' '        TO    X.
040114     MOVE      X         TO    P-CTL.
040114     PERFORM  FIN-PRNT THRU FIN-EXIT .
040114     PERFORM  ACT-PRNT THRU ACT-EXIT .
002901                                                                  00002815
002902     MOVE SPACES    TO    P-DATA.                                 00002816
002903     MOVE     ' '        TO    X.                                 00002817
002904     MOVE      X         TO    P-CTL.                             00002818
002905     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002819
002906     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002820
002907                                                                  00002821
002908     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00002822
002909     MOVE     '1'        TO    X.                                 00002823
002910     MOVE      X         TO    P-CTL.                             00002824
002911     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002825
002912     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002826
002913                                                                  00002827
002914     MOVE    '========================= ' TO P-DATA.              00002828
002915     MOVE     ' '        TO    X.                                 00002829
002916     MOVE      X         TO    P-CTL.                             00002830
002917     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002831
002918     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002832
002919                                                                  00002833
002920     MOVE SPACES    TO    P-DATA.                                 00002834
002921     MOVE     ' '        TO    X.                                 00002835
002922     MOVE      X         TO    P-CTL.                             00002836
002923     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002837
002924     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002838
002925                                                                  00002839
002926     MOVE SPACES    TO    P-DATA.                                 00002840
002927     MOVE     ' '        TO    X.                                 00002841
002928     MOVE      X         TO    P-CTL.                             00002842
002929     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002843
002930     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002844
002933                                                                  00002845
002934* ADD COLORADO PRINTING                                           00002846
002936                                                                  00002847
002937     MOVE  ZEROS TO TOT-CNT.                                      00002848
002938     MOVE  ZEROS TO TOT-R78.                                      00002849
002939     MOVE  ZEROS TO TOT-PRAT.                                     00002850
002940                                                                  00002851
002941      MOVE ZEROS TO AH-SUB.                                       00002852
002943                                                                  00002853
002944     MOVE SPACES    TO    P-DATA.                                 00002854
002945     MOVE 'PRINT OF COLORADO TOTALS, BY CODE ' TO  P-DATA.        00002855
002946     MOVE     ' '        TO    X.                                 00002856
002947     MOVE      X         TO    P-CTL.                             00002857
002948     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002858
002949     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002859
002950                                                                  00002860
002951     MOVE SPACES    TO    P-DATA.                                 00002861
002952     MOVE     ' '        TO    X.                                 00002862
002953     MOVE      X         TO    P-CTL.                             00002863
002954     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002864
002955     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002865
002956                                                                  00002866
002957     MOVE SPACES    TO    P-DATA.                                 00002867
002958     MOVE 'CO-SAVE-SUB = ' TO PRT-INFO.                           00002868
002959     MOVE  CO-SAVE-SUB     TO PRT-AMT.                            00002869
002960     MOVE PRT-LINE  TO    P-DATA.                                 00002870
002961     MOVE     ' '        TO    X.                                 00002871
002962     MOVE      X         TO    P-CTL.                             00002872
002963     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002873
002964     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002874
002965                                                                  00002875
002966     MOVE SPACES    TO    P-DATA.                                 00002876
002967     MOVE     ' '        TO    X.                                 00002877
002968     MOVE      X         TO    P-CTL.                             00002878
002969     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002879
002970     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002880
002971                                                                  00002881
002972     MOVE SPACES    TO    P-DATA.                                 00002882
002973     MOVE     ' '        TO    X.                                 00002883
002974     MOVE      X         TO    P-CTL.                             00002884
002975     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002885
002976     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002886
002977                                                                  00002887
002978 CO-PRNT-LOOP.                                                    00002888
002979                                                                  00002889
002980     ADD 1       TO AH-SUB.                                       00002890
002981                                                                  00002891
002982     IF CO-BEN-CODE (AH-SUB) = ZEROS                              00002892
002983        GO TO CO-PRNT-LOOP-END.                                   00002893
002984                                                                  00002894
002985     MOVE  CO-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00002895
002986     ADD   CO-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00002896
002987     MOVE  CO-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00002897
002988     MOVE  CO-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00002898
002989     ADD   CO-BEN-R78 (AH-SUB)       TO TOT-R78.                  00002899
002990     MOVE  CO-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00002900
002991     ADD   CO-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00002901
002992                                                                  00002902
002993     MOVE PRNT-CODES     TO    P-DATA.                            00002903
002994     MOVE     ' '        TO    X.                                 00002904
002995     MOVE      X         TO    P-CTL.                             00002905
002996     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002906
002997     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002907
003000                                                                  00002908
003001     GO TO CO-PRNT-LOOP.                                          00002909
003002                                                                  00002910
003003 CO-PRNT-LOOP-END.                                                00002911
003004                                                                  00002912
003005     MOVE SPACES         TO    P-DATA.                            00002913
003006     MOVE     ' '        TO    X.                                 00002914
003007     MOVE      X         TO    P-CTL.                             00002915
003008     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002916
003009     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002917
003010                                                                  00002918
003011     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00002919
003012     MOVE  TOT-R78                  TO TOT-R78-P.                 00002920
003013     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00002921
003014                                                                  00002922
003015     MOVE PRNT-TOTALS    TO    P-DATA.                            00002923
003016     MOVE     ' '        TO    X.                                 00002924
003017     MOVE      X         TO    P-CTL.                             00002925
003018     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002926
003019     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002927
003020                                                                  00002928
003022     MOVE  ZEROS TO TOT-CNT.                                      00002929
003023     MOVE  ZEROS TO TOT-R78.                                      00002930
003024     MOVE  ZEROS TO TOT-PRAT.                                     00002931
003025                                                                  00002932
003026     MOVE ZEROS TO AH-SUB.                                        00002933
003028                                                                  00002934
003029     MOVE      SPACES    TO    P-DATA.                            00002935
003030     MOVE     ' '        TO    X.                                 00002936
003031     MOVE      X         TO    P-CTL.                             00002937
003032     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002938
003033     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002939
003034                                                                  00002940
002908     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00002822
002909     MOVE     '1'        TO    X.                                 00002823
002910     MOVE      X         TO    P-CTL.                             00002824
002911     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002825
002912     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002826
002913                                                                  00002827
002914     MOVE    '========================= ' TO P-DATA.              00002828
002915     MOVE     ' '        TO    X.                                 00002829
002916     MOVE      X         TO    P-CTL.                             00002830
002917     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002831
002918     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002832
002919                                                                  00002833
002920     MOVE SPACES    TO    P-DATA.                                 00002834
002921     MOVE     ' '        TO    X.                                 00002835
002922     MOVE      X         TO    P-CTL.                             00002836
002923     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002837
002924     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002838
002925                                                                  00002839
002926     MOVE SPACES    TO    P-DATA.                                 00002840
002927     MOVE     ' '        TO    X.                                 00002841
002928     MOVE      X         TO    P-CTL.                             00002842
002929     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002843
002930     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002844
002933                                                                  00002845
002934* ADD GEORGIA  PRINTING                                           00002846
002936                                                                  00002847
002937     MOVE  ZEROS TO TOT-CNT.                                      00002848
002938     MOVE  ZEROS TO TOT-R78.                                      00002849
002939     MOVE  ZEROS TO TOT-PRAT.                                     00002850
002940                                                                  00002851
002941      MOVE ZEROS TO AH-SUB.                                       00002852
002943                                                                  00002853
002944     MOVE SPACES    TO    P-DATA.                                 00002854
002945     MOVE 'PRINT OF GEORGIA  TOTALS, BY CODE ' TO  P-DATA.        00002855
002946     MOVE     ' '        TO    X.                                 00002856
002947     MOVE      X         TO    P-CTL.                             00002857
002948     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002858
002949     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002859
002950                                                                  00002860
002951     MOVE SPACES    TO    P-DATA.                                 00002861
002952     MOVE     ' '        TO    X.                                 00002862
002953     MOVE      X         TO    P-CTL.                             00002863
002954     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002864
002955     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002865
002956                                                                  00002866
002957     MOVE SPACES    TO    P-DATA.                                 00002867
002958     MOVE 'GA-SAVE-SUB = ' TO PRT-INFO.                           00002868
002959     MOVE  GA-SAVE-SUB     TO PRT-AMT.                            00002869
002960     MOVE PRT-LINE  TO    P-DATA.                                 00002870
002961     MOVE     ' '        TO    X.                                 00002871
002962     MOVE      X         TO    P-CTL.                             00002872
002963     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002873
002964     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002874
002965                                                                  00002875
002966     MOVE SPACES    TO    P-DATA.                                 00002876
002967     MOVE     ' '        TO    X.                                 00002877
002968     MOVE      X         TO    P-CTL.                             00002878
002969     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002879
002970     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002880
002971                                                                  00002881
002972     MOVE SPACES    TO    P-DATA.                                 00002882
002973     MOVE     ' '        TO    X.                                 00002883
002974     MOVE      X         TO    P-CTL.                             00002884
002975     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002885
002976     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002886
002977                                                                  00002887
002978 GA-PRNT-LOOP.                                                    00002888
002979                                                                  00002889
002980     ADD 1       TO AH-SUB.                                       00002890
002981                                                                  00002891
002982     IF GA-BEN-CODE (AH-SUB) = ZEROS                              00002892
002983        GO TO GA-PRNT-LOOP-END.                                   00002893
002984                                                                  00002894
002985     MOVE  GA-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00002895
002986     ADD   GA-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00002896
002987     MOVE  GA-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00002897
002988     MOVE  GA-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00002898
002989     ADD   GA-BEN-R78 (AH-SUB)       TO TOT-R78.                  00002899
002990     MOVE  GA-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00002900
002991     ADD   GA-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00002901
002992                                                                  00002902
002993     MOVE PRNT-CODES     TO    P-DATA.                            00002903
002994     MOVE     ' '        TO    X.                                 00002904
002995     MOVE      X         TO    P-CTL.                             00002905
002996     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002906
002997     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002907
003000                                                                  00002908
003001     GO TO GA-PRNT-LOOP.                                          00002909
003002                                                                  00002910
003003 GA-PRNT-LOOP-END.                                                00002911
003004                                                                  00002912
003005     MOVE SPACES         TO    P-DATA.                            00002913
003006     MOVE     ' '        TO    X.                                 00002914
003007     MOVE      X         TO    P-CTL.                             00002915
003008     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002916
003009     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002917
003010                                                                  00002918
003011     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00002919
003012     MOVE  TOT-R78                  TO TOT-R78-P.                 00002920
003013     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00002921
003014                                                                  00002922
003015     MOVE PRNT-TOTALS    TO    P-DATA.                            00002923
003016     MOVE     ' '        TO    X.                                 00002924
003017     MOVE      X         TO    P-CTL.                             00002925
003018     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002926
003019     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002927
003020                                                                  00002928
003022     MOVE  ZEROS TO TOT-CNT.                                      00002929
003023     MOVE  ZEROS TO TOT-R78.                                      00002930
003024     MOVE  ZEROS TO TOT-PRAT.                                     00002931
003025                                                                  00002932
003026     MOVE ZEROS TO AH-SUB.                                        00002933
003028                                                                  00002934
003029     MOVE      SPACES    TO    P-DATA.                            00002935
003030     MOVE     ' '        TO    X.                                 00002936
003031     MOVE      X         TO    P-CTL.                             00002937
003032     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002938
003033     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002939
003034                                                                  00002940
003035     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00002941
003036     MOVE     '1'        TO    X.                                 00002942
003037     MOVE      X         TO    P-CTL.                             00002943
003038     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002944
003039     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002945
003040                                                                  00002946
003041     MOVE    '========================= ' TO P-DATA.              00002947
003042     MOVE     ' '        TO    X.                                 00002948
003043     MOVE      X         TO    P-CTL.                             00002949
003044     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002950
003045     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002951
003047                                                                  00002952
003048     MOVE      SPACES    TO    P-DATA.                            00002953
003049     MOVE     ' '        TO    X.                                 00002954
003050     MOVE      X         TO    P-CTL.                             00002955
003051     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002956
003052     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002957
003053                                                                  00002958
003054     MOVE      SPACES    TO    P-DATA.                            00002959
003055     MOVE     ' '        TO    X.                                 00002960
003056     MOVE      X         TO    P-CTL.                             00002961
003057     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002962
003058     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002963
003059                                                                  00002964
003060     MOVE    'PRINT OF CALIFORNIA TOTALS, BY CODE ' TO P-DATA.    00002965
003061     MOVE     ' '        TO    X.                                 00002966
003062     MOVE      X         TO    P-CTL.                             00002967
003063     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002968
003064     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002969
003065                                                                  00002970
003066     MOVE    SPACES      TO P-DATA.                               00002971
003067     MOVE     ' '        TO    X.                                 00002972
003068     MOVE      X         TO    P-CTL.                             00002973
003069     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002974
003070     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002975
003072                                                                  00002976
003073     MOVE  'CA-SAVE-SUB = ' TO PRT-INFO.                          00002977
003074     MOVE   CA-SAVE-SUB     TO PRT-AMT.                           00002978
003075     MOVE PRT-LINE         TO P-DATA.                             00002979
003076     MOVE     ' '        TO    X.                                 00002980
003077     MOVE      X         TO    P-CTL.                             00002981
003078     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002982
003079     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002983
003085                                                                  00002984
003086     MOVE    SPACES      TO P-DATA.                               00002985
003087     MOVE     ' '        TO    X.                                 00002986
003088     MOVE      X         TO    P-CTL.                             00002987
003089     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002988
003090     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002989
003091                                                                  00002990
003092     MOVE    SPACES      TO P-DATA.                               00002991
003093     MOVE     ' '        TO    X.                                 00002992
003094     MOVE      X         TO    P-CTL.                             00002993
003095     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002994
003096     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002995
003099                                                                  00002996
003100 CA-PRNT-LOOP.                                                    00002997
003101                                                                  00002998
003102     ADD 1       TO AH-SUB.                                       00002999
003103                                                                  00003000
003104     IF CA-BEN-CODE (AH-SUB) = ZEROS                              00003001
003105        GO TO CA-PRNT-LOOP-END.                                   00003002
003106                                                                  00003003
003107     MOVE  CA-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00003004
003108     ADD   CA-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00003005
003109     MOVE  CA-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00003006
003110     MOVE  CA-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00003007
003111     ADD   CA-BEN-R78 (AH-SUB)       TO TOT-R78.                  00003008
003112     MOVE  CA-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00003009
003113     ADD   CA-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00003010
003114                                                                  00003011
003115     MOVE PRNT-CODES     TO    P-DATA.                            00003012
003116     MOVE     ' '        TO    X.                                 00003013
003117     MOVE      X         TO    P-CTL.                             00003014
003118     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003015
003119     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003016
003121                                                                  00003017
003122     GO TO CA-PRNT-LOOP.                                          00003018
003123                                                                  00003019
003124 CA-PRNT-LOOP-END.                                                00003020
003125                                                                  00003021
003126     MOVE SPACES         TO    P-DATA.                            00003022
003127     MOVE     ' '        TO    X.                                 00003023
003128     MOVE      X         TO    P-CTL.                             00003024
003129     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003025
003130     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003026
003131                                                                  00003027
003132     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00003028
003133     MOVE  TOT-R78                  TO TOT-R78-P.                 00003029
003134     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00003030
003135                                                                  00003031
003136     MOVE PRNT-TOTALS    TO    P-DATA.                            00003032
003137     MOVE     ' '        TO    X.                                 00003033
003138     MOVE      X         TO    P-CTL.                             00003034
003139     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003035
003140     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003036
003142                                                                  00003037
003143     MOVE  ZEROS TO TOT-CNT.                                      00003038
003144     MOVE  ZEROS TO TOT-R78.                                      00003039
003145     MOVE  ZEROS TO TOT-PRAT.                                     00003040
003146                                                                  00003041
003147     MOVE ZEROS TO AH-SUB.                                        00003042
003148                                                                  00003043
003149     MOVE      SPACES    TO    P-DATA.                            00003044
003150     MOVE     ' '        TO    X.                                 00003045
003151     MOVE      X         TO    P-CTL.                             00003046
003152     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003047
003153     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003048
CIDMOD                                                                  00002940
CIDMOD     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00002941
CIDMOD     MOVE     '1'        TO    X.                                 00002942
CIDMOD     MOVE      X         TO    P-CTL.                             00002943
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002944
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002945
CIDMOD                                                                  00002946
CIDMOD     MOVE    '========================= ' TO P-DATA.              00002947
CIDMOD     MOVE     ' '        TO    X.                                 00002948
CIDMOD     MOVE      X         TO    P-CTL.                             00002949
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002950
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002951
CIDMOD                                                                  00002952
CIDMOD     MOVE      SPACES    TO    P-DATA.                            00002953
CIDMOD     MOVE     ' '        TO    X.                                 00002954
CIDMOD     MOVE      X         TO    P-CTL.                             00002955
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002956
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002957
CIDMOD                                                                  00002958
CIDMOD     MOVE      SPACES    TO    P-DATA.                            00002959
CIDMOD     MOVE     ' '        TO    X.                                 00002960
CIDMOD     MOVE      X         TO    P-CTL.                             00002961
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002962
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002963
CIDMOD                                                                  00002964
CIDMOD     MOVE    'PRINT OF OHIO TOTALS, BY CODE ' TO P-DATA.          00002965
CIDMOD     MOVE     ' '        TO    X.                                 00002966
CIDMOD     MOVE      X         TO    P-CTL.                             00002967
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002968
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002969
CIDMOD                                                                  00002970
CIDMOD     MOVE    SPACES      TO P-DATA.                               00002971
CIDMOD     MOVE     ' '        TO    X.                                 00002972
CIDMOD     MOVE      X         TO    P-CTL.                             00002973
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002974
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002975
CIDMOD                                                                  00002976
CIDMOD     MOVE  'OH-SAVE-SUB = ' TO PRT-INFO.                          00002977
CIDMOD     MOVE   OH-SAVE-SUB     TO PRT-AMT.                           00002978
CIDMOD     MOVE PRT-LINE         TO P-DATA.                             00002979
CIDMOD     MOVE     ' '        TO    X.                                 00002980
CIDMOD     MOVE      X         TO    P-CTL.                             00002981
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002982
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002983
CIDMOD                                                                  00002984
CIDMOD     MOVE    SPACES      TO P-DATA.                               00002985
CIDMOD     MOVE     ' '        TO    X.                                 00002986
CIDMOD     MOVE      X         TO    P-CTL.                             00002987
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002988
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002989
CIDMOD                                                                  00002990
CIDMOD     MOVE    SPACES      TO P-DATA.                               00002991
CIDMOD     MOVE     ' '        TO    X.                                 00002992
CIDMOD     MOVE      X         TO    P-CTL.                             00002993
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00002994
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00002995
CIDMOD                                                                  00002996
CIDMOD OH-PRNT-LOOP.                                                    00002997
CIDMOD                                                                  00002998
CIDMOD     ADD 1       TO AH-SUB.                                       00002999
CIDMOD                                                                  00003000
CIDMOD     IF OH-BEN-CODE (AH-SUB) = ZEROS                              00003001
CIDMOD        GO TO OH-PRNT-LOOP-END.                                   00003002
CIDMOD                                                                  00003003
CIDMOD     MOVE  OH-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00003004
CIDMOD     ADD   OH-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00003005
CIDMOD     MOVE  OH-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00003006
CIDMOD     MOVE  OH-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00003007
CIDMOD     ADD   OH-BEN-R78 (AH-SUB)       TO TOT-R78.                  00003008
CIDMOD     MOVE  OH-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00003009
CIDMOD     ADD   OH-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00003010
CIDMOD                                                                  00003011
CIDMOD     MOVE PRNT-CODES     TO    P-DATA.                            00003012
CIDMOD     MOVE     ' '        TO    X.                                 00003013
CIDMOD     MOVE      X         TO    P-CTL.                             00003014
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003015
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003016
CIDMOD                                                                  00003017
CIDMOD     GO TO OH-PRNT-LOOP.                                          00003018
CIDMOD                                                                  00003019
CIDMOD OH-PRNT-LOOP-END.                                                00003020
CIDMOD                                                                  00003021
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003022
CIDMOD     MOVE     ' '        TO    X.                                 00003023
CIDMOD     MOVE      X         TO    P-CTL.                             00003024
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003025
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003026
CIDMOD                                                                  00003027
CIDMOD     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00003028
CIDMOD     MOVE  TOT-R78                  TO TOT-R78-P.                 00003029
CIDMOD     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00003030
CIDMOD                                                                  00003031
CIDMOD     MOVE PRNT-TOTALS    TO    P-DATA.                            00003032
CIDMOD     MOVE     ' '        TO    X.                                 00003033
CIDMOD     MOVE      X         TO    P-CTL.                             00003034
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003035
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003036
CIDMOD                                                                  00003037
CIDMOD     MOVE  ZEROS TO TOT-CNT.                                      00003038
CIDMOD     MOVE  ZEROS TO TOT-R78.                                      00003039
CIDMOD     MOVE  ZEROS TO TOT-PRAT.                                     00003040
CIDMOD                                                                  00003041
CIDMOD     MOVE ZEROS TO AH-SUB.                                        00003042
CIDMOD                                                                  00003043
CIDMOD     MOVE      SPACES    TO    P-DATA.                            00003044
CIDMOD     MOVE     ' '        TO    X.                                 00003045
CIDMOD     MOVE      X         TO    P-CTL.                             00003046
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003047
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003048
003154                                                                  00003049
003155     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00003050
003156     MOVE     '1'        TO    X.                                 00003051
003157     MOVE      X         TO    P-CTL.                             00003052
003158     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003053
003159     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003054
003160                                                                  00003055
003161     MOVE    '========================= ' TO P-DATA.              00003056
003162     MOVE     ' '        TO    X.                                 00003057
003163     MOVE      X         TO    P-CTL.                             00003058
003164     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003059
003165     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003060
003173                                                                  00003061
003174     MOVE      SPACES    TO    P-DATA.                            00003062
003175     MOVE     ' '        TO    X.                                 00003063
003176     MOVE      X         TO    P-CTL.                             00003064
003177     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003065
003178     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003066
003179                                                                  00003067
003180     MOVE 'PRINT OF OREGON TOTALS, BY CODE ' TO P-DATA.           00003068
003181     MOVE     ' '        TO    X.                                 00003069
003182     MOVE      X         TO    P-CTL.                             00003070
003183     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003071
003184     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003072
003185                                                                  00003073
003186     MOVE      SPACES    TO    P-DATA.                            00003074
003187     MOVE     ' '        TO    X.                                 00003075
003188     MOVE      X         TO    P-CTL.                             00003076
003189     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003077
003190     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003078
003191                                                                  00003079
003192     MOVE  'OR-SAVE-SUB   '  TO PRT-INFO.                         00003080
003193     MOVE   OR-SAVE-SUB      TO PRT-AMT.                          00003081
003194     MOVE   PRT-LINE      TO P-DATA.                              00003082
003195     MOVE     ' '        TO    X.                                 00003083
003196     MOVE      X         TO    P-CTL.                             00003084
003197     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003085
003198     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003086
003203                                                                  00003087
003204     MOVE   SPACES       TO PRT.                                  00003088
003205     MOVE     ' '        TO    X.                                 00003089
003206     MOVE      X         TO    P-CTL.                             00003090
003207     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003091
003208     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003092
003209                                                                  00003093
003210     MOVE   SPACES       TO PRT.                                  00003094
003211     MOVE     ' '        TO    X.                                 00003095
003212     MOVE      X         TO    P-CTL.                             00003096
003213     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003097
003214     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003098
003215                                                                  00003099
003216 OR-PRNT-LOOP.                                                    00003100
003219                                                                  00003101
003220     ADD 1       TO AH-SUB.                                       00003102
003221                                                                  00003103
003222     IF OR-BEN-CODE (AH-SUB) = ZEROS                              00003104
003223        GO TO OR-PRNT-LOOP-END.                                   00003105
003224                                                                  00003106
003225     MOVE  OR-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00003107
003226     ADD   OR-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00003108
003227     MOVE  OR-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00003109
003228     MOVE  OR-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00003110
003229     ADD   OR-BEN-R78 (AH-SUB)       TO TOT-R78.                  00003111
003230     MOVE  OR-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00003112
003231     ADD   OR-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00003113
003232                                                                  00003114
003233     MOVE PRNT-CODES     TO    P-DATA.                            00003115
003234     MOVE     ' '        TO    X.                                 00003116
003235     MOVE      X         TO    P-CTL.                             00003117
003236     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003118
003237     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003119
003238                                                                  00003120
003239     GO TO OR-PRNT-LOOP.                                          00003121
003242                                                                  00003122
003243 OR-PRNT-LOOP-END.                                                00003123
003244                                                                  00003124
003245     MOVE SPACES         TO    P-DATA.                            00003125
003246     MOVE     ' '        TO    X.                                 00003126
003247     MOVE      X         TO    P-CTL.                             00003127
003248     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003128
003249     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003129
003251                                                                  00003130
003252     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00003131
003253     MOVE  TOT-R78                  TO TOT-R78-P.                 00003132
003254     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00003133
003255                                                                  00003134
003256     MOVE PRNT-TOTALS    TO    P-DATA.                            00003135
003257     MOVE     ' '        TO    X.                                 00003136
003258     MOVE      X         TO    P-CTL.                             00003137
003259     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003138
003260     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003139
003262                                                                  00003140
003263     MOVE  ZEROS TO TOT-CNT.                                      00003141
003264     MOVE  ZEROS TO TOT-R78.                                      00003142
003265     MOVE  ZEROS TO TOT-PRAT.                                     00003143
003266                                                                  00003144
003267     MOVE ZEROS TO AH-SUB.                                        00003145
003269                                                                  00003146
003270     MOVE      SPACES    TO    P-DATA.                            00003147
003271     MOVE     ' '        TO    X.                                 00003148
003272     MOVE      X         TO    P-CTL.                             00003149
003273     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003150
003274     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003151
003276                                                                  00003152
CIDMOD     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00003050
CIDMOD     MOVE     '1'        TO    X.                                 00003051
CIDMOD     MOVE      X         TO    P-CTL.                             00003052
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003053
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003054
CIDMOD                                                                  00003055
CIDMOD     MOVE    '========================= ' TO P-DATA.              00003056
CIDMOD     MOVE     ' '        TO    X.                                 00003057
CIDMOD     MOVE      X         TO    P-CTL.                             00003058
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003059
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003060
CIDMOD                                                                  00003061
CIDMOD     MOVE      SPACES    TO    P-DATA.                            00003062
CIDMOD     MOVE     ' '        TO    X.                                 00003063
CIDMOD     MOVE      X         TO    P-CTL.                             00003064
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003065
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003066
CIDMOD                                                                  00003067
CIDMOD     MOVE 'PRINT OF TEXAS TOTALS, BY CODE ' TO P-DATA.            00003068
CIDMOD     MOVE     ' '        TO    X.                                 00003069
CIDMOD     MOVE      X         TO    P-CTL.                             00003070
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003071
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003072
CIDMOD                                                                  00003073
CIDMOD     MOVE      SPACES    TO    P-DATA.                            00003074
CIDMOD     MOVE     ' '        TO    X.                                 00003075
CIDMOD     MOVE      X         TO    P-CTL.                             00003076
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003077
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003078
CIDMOD                                                                  00003079
CIDMOD     MOVE  'TX-SAVE-SUB   '  TO PRT-INFO.                         00003080
CIDMOD     MOVE   TX-SAVE-SUB      TO PRT-AMT.                          00003081
CIDMOD     MOVE   PRT-LINE      TO P-DATA.                              00003082
CIDMOD     MOVE     ' '        TO    X.                                 00003083
CIDMOD     MOVE      X         TO    P-CTL.                             00003084
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003085
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003086
CIDMOD                                                                  00003087
CIDMOD     MOVE   SPACES       TO PRT.                                  00003088
CIDMOD     MOVE     ' '        TO    X.                                 00003089
CIDMOD     MOVE      X         TO    P-CTL.                             00003090
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003091
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003092
CIDMOD                                                                  00003093
CIDMOD     MOVE   SPACES       TO PRT.                                  00003094
CIDMOD     MOVE     ' '        TO    X.                                 00003095
CIDMOD     MOVE      X         TO    P-CTL.                             00003096
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003097
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003098
CIDMOD                                                                  00003099
CIDMOD TX-PRNT-LOOP.                                                    00003100
CIDMOD                                                                  00003101
CIDMOD     ADD 1       TO AH-SUB.                                       00003102
CIDMOD                                                                  00003103
CIDMOD     IF TX-BEN-CODE (AH-SUB) = ZEROS                              00003104
CIDMOD        GO TO TX-PRNT-LOOP-END.                                   00003105
CIDMOD                                                                  00003106
CIDMOD     MOVE  TX-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00003107
CIDMOD     ADD   TX-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00003108
CIDMOD     MOVE  TX-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00003109
CIDMOD     MOVE  TX-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00003110
CIDMOD     ADD   TX-BEN-R78 (AH-SUB)       TO TOT-R78.                  00003111
CIDMOD     MOVE  TX-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00003112
CIDMOD     ADD   TX-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00003113
CIDMOD                                                                  00003114
CIDMOD     MOVE PRNT-CODES     TO    P-DATA.                            00003115
CIDMOD     MOVE     ' '        TO    X.                                 00003116
CIDMOD     MOVE      X         TO    P-CTL.                             00003117
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003118
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003119
CIDMOD                                                                  00003120
CIDMOD     GO TO TX-PRNT-LOOP.                                          00003121
CIDMOD                                                                  00003122
CIDMOD TX-PRNT-LOOP-END.                                                00003123
CIDMOD                                                                  00003124
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003125
CIDMOD     MOVE     ' '        TO    X.                                 00003126
CIDMOD     MOVE      X         TO    P-CTL.                             00003127
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003128
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003129
CIDMOD                                                                  00003130
CIDMOD     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00003131
CIDMOD     MOVE  TOT-R78                  TO TOT-R78-P.                 00003132
CIDMOD     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00003133
CIDMOD                                                                  00003134
CIDMOD     MOVE PRNT-TOTALS    TO    P-DATA.                            00003135
CIDMOD     MOVE     ' '        TO    X.                                 00003136
CIDMOD     MOVE      X         TO    P-CTL.                             00003137
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003138
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003139
CIDMOD                                                                  00003140
CIDMOD     MOVE  ZEROS TO TOT-CNT.                                      00003141
CIDMOD     MOVE  ZEROS TO TOT-R78.                                      00003142
CIDMOD     MOVE  ZEROS TO TOT-PRAT.                                     00003143
CIDMOD                                                                  00003144
CIDMOD     MOVE ZEROS TO AH-SUB.                                        00003145
CIDMOD                                                                  00003146
CIDMOD     MOVE      SPACES    TO    P-DATA.                            00003147
CIDMOD     MOVE     ' '        TO    X.                                 00003148
CIDMOD     MOVE      X         TO    P-CTL.                             00003149
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003150
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003151
CIDMOD                                                                  00003152
003277     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00003153
003278     MOVE     '1'        TO    X.                                 00003154
003279     MOVE      X         TO    P-CTL.                             00003155
003280     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003156
003281     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003157
003282                                                                  00003158
003283     MOVE    '========================= ' TO P-DATA.              00003159
003284     MOVE     ' '        TO    X.                                 00003160
003285     MOVE      X         TO    P-CTL.                             00003161
003286     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003162
003287     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003163
003291                                                                  00003164
003292     MOVE    SPACES      TO P-DATA.                               00003165
003293     MOVE     ' '        TO    X.                                 00003166
003294     MOVE      X         TO    P-CTL.                             00003167
003295     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003168
003296     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003169
003297                                                                  00003170
003298     MOVE  'PRINT OF WASHINGTON TOTALS, BY CODE ' TO P-DATA.      00003171
003299     MOVE     ' '        TO    X.                                 00003172
003300     MOVE      X         TO    P-CTL.                             00003173
003301     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003174
003302     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003175
003305                                                                  00003176
003306     MOVE    SPACES      TO P-DATA.                               00003177
003307     MOVE     ' '        TO    X.                                 00003178
003308     MOVE      X         TO    P-CTL.                             00003179
003309     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003180
003310     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003181
003311                                                                  00003182
003312     MOVE  'WA-SAVE-SUB   '  TO PRT-INFO.                         00003183
003313     MOVE   WA-SAVE-SUB      TO PRT-AMT.                          00003184
003314     MOVE   PRT-LINE      TO P-DATA.                              00003185
003316     MOVE     ' '        TO    X.                                 00003186
003317     MOVE      X         TO    P-CTL.                             00003187
003318     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003188
003319     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003189
003320                                                                  00003190
003321     MOVE   SPACES        TO P-DATA.                              00003191
003322     MOVE     ' '        TO    X.                                 00003192
003323     MOVE      X         TO    P-CTL.                             00003193
003324     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003194
003325     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003195
003326                                                                  00003196
003327 WA-PRNT-LOOP.                                                    00003197
003328                                                                  00003198
003329     ADD 1       TO AH-SUB.                                       00003199
003330                                                                  00003200
003331     IF WA-BEN-CODE (AH-SUB) = ZEROS                              00003201
003332        GO TO WA-PRNT-LOOP-END.                                   00003202
003333                                                                  00003203
003334     MOVE  WA-BEN-COUNT(AH-SUB)      TO BEN-COUNT-P.              00003204
003335     ADD   WA-BEN-COUNT(AH-SUB)      TO TOT-CNT.                  00003205
003336     MOVE  WA-BEN-CODE (AH-SUB)      TO BEN-CODE-P.               00003206
003337     MOVE  WA-BEN-R78 (AH-SUB)       TO BEN-R78-P.                00003207
003338     ADD   WA-BEN-R78 (AH-SUB)       TO TOT-R78.                  00003208
003339     MOVE  WA-BEN-PRAT (AH-SUB)      TO BEN-PRAT-P.               00003209
003340     ADD   WA-BEN-PRAT (AH-SUB)      TO TOT-PRAT.                 00003210
003341                                                                  00003211
003342     MOVE PRNT-CODES     TO    P-DATA.                            00003212
003343     MOVE     ' '        TO    X.                                 00003213
003344     MOVE      X         TO    P-CTL.                             00003214
003345     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003215
003346     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003216
003347                                                                  00003217
003348     GO TO WA-PRNT-LOOP.                                          00003218
003349                                                                  00003219
003350 WA-PRNT-LOOP-END.                                                00003220
003351                                                                  00003221
003352     MOVE SPACES         TO    P-DATA.                            00003222
003353     MOVE     ' '        TO    X.                                 00003223
003354     MOVE      X         TO    P-CTL.                             00003224
003355     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003225
003356     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003226
003357                                                                  00003227
003358     MOVE  TOT-CNT                  TO TOT-COUNT-P.               00003228
003359     MOVE  TOT-R78                  TO TOT-R78-P.                 00003229
003360     MOVE  TOT-PRAT                 TO TOT-PRAT-P.                00003230
003365                                                                  00003231
003366     MOVE PRNT-TOTALS    TO    P-DATA.                            00003232
003367     MOVE     ' '        TO    X.                                 00003233
003368     MOVE      X         TO    P-CTL.                             00003234
003369     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003235
003370     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003236
003373                                                                  00003237
003374     MOVE  ZEROS TO TOT-CNT.                                      00003238
003375     MOVE  ZEROS TO TOT-R78.                                      00003239
003376     MOVE  ZEROS TO TOT-PRAT.                                     00003240
003377                                                                  00003241
003378     MOVE      SPACES    TO    P-DATA.                            00003242
003379     MOVE     ' '        TO    X.                                 00003243
003380     MOVE      X         TO    P-CTL.                             00003244
003381     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003245
003382     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003246
003383                                                                  00003247
003384     MOVE    'C S O 0 8 3   R E P O R T ' TO  P-DATA.             00003248
003385     MOVE     '1'        TO    X.                                 00003249
003386     MOVE      X         TO    P-CTL.                             00003250
003387     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003251
003388     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003252
003389                                                                  00003253
003390     MOVE    '========================= ' TO P-DATA.              00003254
003391     MOVE     ' '        TO    X.                                 00003255
003392     MOVE      X         TO    P-CTL.                             00003256
003393     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003257
003394     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003258
003396                                                                  00003259
003397     MOVE SPACES         TO    P-DATA.                            00003260
003398     MOVE     ' '        TO    X.                                 00003261
003399     MOVE      X         TO    P-CTL.                             00003262
003400     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003263
003401     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003264
003402                                                                  00003265
003403     MOVE    CO-COUNT                        TO  REC-CNT-EDIT.    00003266
003404     MOVE 'TOTAL REC COUNT FOR COLORADO = ' TO PRT-INFO.          00003267
003405     MOVE  REC-CNT-EDIT                     TO PRT-AMT.           00003268
003406     MOVE PRT-LINE       TO    P-DATA.                            00003269
003407     MOVE     ' '        TO    X.                                 00003270
003408     MOVE      X         TO    P-CTL.                             00003271
003409     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003272
003410     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003273
003411                                                                  00003274
003412     MOVE SPACES         TO    P-DATA.                            00003275
003413     MOVE     ' '        TO    X.                                 00003276
003414     MOVE      X         TO    P-CTL.                             00003277
003415     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003278
003416     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003279
003417                                                                  00003280
003418     MOVE SPACES         TO    P-DATA.                            00003281
003419     MOVE     ' '        TO    X.                                 00003282
003420     MOVE      X         TO    P-CTL.                             00003283
003421     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003284
003422     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003285
003423                                                                  00003286
003424     MOVE    CA-COUNT                      TO  REC-CNT-EDIT.      00003287
003425     MOVE 'TOTAL REC COUNT FOR CALIF    = ' TO PRT-INFO.          00003288
003426     MOVE  REC-CNT-EDIT                     TO PRT-AMT.           00003289
003427     MOVE PRT-LINE       TO    P-DATA.                            00003290
003428     MOVE     ' '        TO    X.                                 00003291
003429     MOVE      X         TO    P-CTL.                             00003292
003430     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003293
003431     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003294
003440                                                                  00003295
003441     MOVE SPACES         TO    P-DATA.                            00003296
003442     MOVE     ' '        TO    X.                                 00003297
003443     MOVE      X         TO    P-CTL.                             00003298
003444     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003299
003445     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003300
003446                                                                  00003301
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003296
CIDMOD     MOVE     ' '        TO    X.                                 00003297
CIDMOD     MOVE      X         TO    P-CTL.                             00003298
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003299
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003300
CIDMOD                                                                  00003301
CIDMOD     MOVE    OH-COUNT                      TO  REC-CNT-EDIT.      00003287
CIDMOD     MOVE 'TOTAL REC COUNT FOR OHIO     = ' TO PRT-INFO.          00003288
CIDMOD     MOVE  REC-CNT-EDIT                     TO PRT-AMT.           00003289
CIDMOD     MOVE PRT-LINE       TO    P-DATA.                            00003290
CIDMOD     MOVE     ' '        TO    X.                                 00003291
CIDMOD     MOVE      X         TO    P-CTL.                             00003292
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003293
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003294
CIDMOD                                                                  00003295
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003296
CIDMOD     MOVE     ' '        TO    X.                                 00003297
CIDMOD     MOVE      X         TO    P-CTL.                             00003298
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003299
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003300
CIDMOD                                                                  00003301
003447     MOVE SPACES         TO    P-DATA.                            00003302
003448     MOVE     ' '        TO    X.                                 00003303
003449     MOVE      X         TO    P-CTL.                             00003304
003450     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003305
003451     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003306
003454                                                                  00003307
003455     MOVE    OR-COUNT                       TO  REC-CNT-EDIT.     00003308
003456     MOVE 'TOTAL REC COUNT FOR OREGON   = ' TO PRT-INFO.          00003309
003457     MOVE  REC-CNT-EDIT                     TO PRT-AMT.           00003310
003458     MOVE PRT-LINE       TO    P-DATA.                            00003311
003459     MOVE     ' '        TO    X.                                 00003312
003460     MOVE      X         TO    P-CTL.                             00003313
003461     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003314
003462     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003315
003463                                                                  00003316
003464     MOVE SPACES         TO    P-DATA.                            00003317
003465     MOVE     ' '        TO    X.                                 00003318
003466     MOVE      X         TO    P-CTL.                             00003319
003467     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003320
003468     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003321
003469                                                                  00003322
003470     MOVE SPACES         TO    P-DATA.                            00003323
003473     MOVE     ' '        TO    X.                                 00003324
003474     MOVE      X         TO    P-CTL.                             00003325
003475     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003326
003476     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003327
003477                                                                  00003328
CIDMOD     MOVE    TX-COUNT                       TO  REC-CNT-EDIT.     00003308
CIDMOD     MOVE 'TOTAL REC COUNT FOR TEXAS    = ' TO PRT-INFO.          00003309
CIDMOD     MOVE  REC-CNT-EDIT                     TO PRT-AMT.           00003310
CIDMOD     MOVE PRT-LINE       TO    P-DATA.                            00003311
CIDMOD     MOVE     ' '        TO    X.                                 00003312
CIDMOD     MOVE      X         TO    P-CTL.                             00003313
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003314
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003315
CIDMOD                                                                  00003316
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003317
CIDMOD     MOVE     ' '        TO    X.                                 00003318
CIDMOD     MOVE      X         TO    P-CTL.                             00003319
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003320
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003321
CIDMOD                                                                  00003322
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003323
CIDMOD     MOVE     ' '        TO    X.                                 00003324
CIDMOD     MOVE      X         TO    P-CTL.                             00003325
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003326
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003327
CIDMOD                                                                  00003328
003478     MOVE    WA-COUNT                       TO  REC-CNT-EDIT.     00003329
003479     MOVE 'TOTAL REC COUNT FOR WASH     = ' TO PRT-INFO.          00003330
003480     MOVE  REC-CNT-EDIT                     TO PRT-AMT.           00003331
003481     MOVE PRT-LINE       TO    P-DATA.                            00003332
003482     MOVE     ' '        TO    X.                                 00003333
003483     MOVE      X         TO    P-CTL.                             00003334
003484     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003335
003485     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003336
003486                                                                  00003337
003487     MOVE SPACES         TO    P-DATA.                            00003338
003488     MOVE     ' '        TO    X.                                 00003339
003489     MOVE      X         TO    P-CTL.                             00003340
003490     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003341
003491     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003342
003492                                                                  00003343
003493* BUILD NEW REPORT THAT ACTUARIAL REQUESTED.                      00003344
003494                                                                  00003345
003495     MOVE SPACES         TO    P-DATA.                            00003346
003496     MOVE     '1'        TO    X.                                 00003347
003497     MOVE      X         TO    P-CTL.                             00003348
003498     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003349
003499     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003350
003504                                                                  00003351
CIDMOD     MOVE ' CID DISABILITY UNEARNED PREMIUM RESERVE'              00003352
003505                         TO P-DATA.                               00003352
003506     MOVE     ' '        TO    X.                                 00003353
003507     MOVE      X         TO    P-CTL.                             00003354
003508     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003355
003509     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003356
003510                                                                  00003357
003511     MOVE SPACES         TO    P-DATA.                            00003358
003512     MOVE     ' '        TO    X.                                 00003359
003513     MOVE      X         TO    P-CTL.                             00003360
003514     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003361
003515     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003362
003516     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003363
003517     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003364
003518     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003365
003519     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003366
003520                                                                  00003367
003521     MOVE NR-TOT         TO    NR-INFO.                           00003368
003522     MOVE ALL-AH-P78     TO    DIS-EDIT.                          00003369
003523     MOVE DIS-EDIT       TO    NR-AMT.                            00003370
003524     MOVE NEW-RPT        TO    PRT.                               00003371
003525     MOVE     ' '        TO    X.                                 00003372
003526     MOVE      X         TO    P-CTL.                             00003373
003527     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003374
003528     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003375
003529                                                                  00003376
003530     MOVE SPACES         TO    NR-INFO.                           00003377
003531     MOVE SPACES         TO    P-DATA.                            00003378
003532     MOVE     ' '        TO    X.                                 00003379
003533     MOVE      X         TO    P-CTL.                             00003380
003534     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003381
003535     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003382
003536                                                                  00003383
003537     MOVE CA-R78         TO    NR-INFO.                           00003384
003538     MOVE CA-AH-P78      TO    DIS-EDIT.                          00003385
003539     MOVE DIS-EDIT       TO    NR-AMT.                            00003386
003540     MOVE NEW-RPT        TO    PRT.                               00003387
003541     MOVE     ' '        TO    X.                                 00003388
003542     MOVE      X         TO    P-CTL.                             00003389
003543     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003390
003544     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003391
003545                                                                  00003392
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003377
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003378
CIDMOD     MOVE     ' '        TO    X.                                 00003379
CIDMOD     MOVE      X         TO    P-CTL.                             00003380
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003381
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003382
CIDMOD                                                                  00003383
CIDMOD     MOVE GA-R78         TO    NR-INFO.                           00003384
CIDMOD     MOVE GA-AH-P78      TO    DIS-EDIT.                          00003385
CIDMOD     MOVE DIS-EDIT       TO    NR-AMT.                            00003386
CIDMOD     MOVE NEW-RPT        TO    PRT.                               00003387
CIDMOD     MOVE     ' '        TO    X.                                 00003388
CIDMOD     MOVE      X         TO    P-CTL.                             00003389
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003390
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003391
CIDMOD                                                                  00003392
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003377
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003378
CIDMOD     MOVE     ' '        TO    X.                                 00003379
CIDMOD     MOVE      X         TO    P-CTL.                             00003380
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003381
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003382
CIDMOD                                                                  00003383
CIDMOD     MOVE OH-R78         TO    NR-INFO.                           00003384
CIDMOD     MOVE OH-AH-P78      TO    DIS-EDIT.                          00003385
CIDMOD     MOVE DIS-EDIT       TO    NR-AMT.                            00003386
CIDMOD     MOVE NEW-RPT        TO    PRT.                               00003387
CIDMOD     MOVE     ' '        TO    X.                                 00003388
CIDMOD     MOVE      X         TO    P-CTL.                             00003389
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003390
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003391
CIDMOD                                                                  00003392
003546     MOVE SPACES         TO    NR-INFO.                           00003393
003547     MOVE SPACES         TO    P-DATA.                            00003394
003548     MOVE     ' '        TO    X.                                 00003395
003549     MOVE      X         TO    P-CTL.                             00003396
003550     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003397
003551     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003398
003553                                                                  00003399
003554     MOVE OR-R78         TO    NR-INFO.                           00003400
003555     MOVE OR-AH-P78      TO    DIS-EDIT.                          00003401
003556     MOVE DIS-EDIT       TO    NR-AMT.                            00003402
003557     MOVE NEW-RPT        TO    PRT.                               00003403
003558     MOVE     ' '        TO    X.                                 00003404
003559     MOVE      X         TO    P-CTL.                             00003405
003560     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003406
003561     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003407
003562                                                                  00003408
003563     MOVE SPACES         TO    NR-INFO.                           00003409
003564     MOVE SPACES         TO    P-DATA.                            00003410
003565     MOVE     ' '        TO    X.                                 00003411
003566     MOVE      X         TO    P-CTL.                             00003412
003567     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003413
003568     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003414
003569                                                                  00003415
CIDMOD     MOVE TX-R78         TO    NR-INFO.                           00003400
CIDMOD     MOVE TX-AH-P78      TO    DIS-EDIT.                          00003401
CIDMOD     MOVE DIS-EDIT       TO    NR-AMT.                            00003402
CIDMOD     MOVE NEW-RPT        TO    PRT.                               00003403
CIDMOD     MOVE     ' '        TO    X.                                 00003404
CIDMOD     MOVE      X         TO    P-CTL.                             00003405
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003406
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003407
CIDMOD                                                                  00003408
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003409
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003410
CIDMOD     MOVE     ' '        TO    X.                                 00003411
CIDMOD     MOVE      X         TO    P-CTL.                             00003412
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003413
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003414
CIDMOD                                                                  00003415
003570     MOVE WA-R78         TO    NR-INFO.                           00003416
003571     MOVE WA-AH-P78      TO    DIS-EDIT.                          00003417
003572     MOVE DIS-EDIT       TO    NR-AMT.                            00003418
003573     MOVE NEW-RPT        TO    PRT.                               00003419
003574     MOVE     ' '        TO    X.                                 00003420
003575     MOVE      X         TO    P-CTL.                             00003421
003576     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003422
003577     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003423
003578                                                                  00003424
003579     MOVE SPACES         TO    NR-INFO.                           00003425
003580     MOVE SPACES         TO    P-DATA.                            00003426
003581     MOVE     ' '        TO    X.                                 00003427
003582     MOVE      X         TO    P-CTL.                             00003428
003583     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003429
003584     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003430
003585                                                                  00003431
003586     MOVE CA-PRAT        TO    NR-INFO.                           00003432
003587     MOVE CA-AH-PRATA    TO    DIS-EDIT.                          00003433
003588     MOVE DIS-EDIT       TO    NR-AMT.                            00003434
003589     MOVE NEW-RPT        TO    PRT.                               00003435
003590     MOVE     ' '        TO    X.                                 00003436
003591     MOVE      X         TO    P-CTL.                             00003437
003592     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003438
003593     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003439
003594                                                                  00003440
003595     MOVE SPACES         TO    NR-INFO.                           00003441
003596     MOVE SPACES         TO    P-DATA.                            00003442
003597     MOVE     ' '        TO    X.                                 00003443
003598     MOVE      X         TO    P-CTL.                             00003444
003599     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003445
003600     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003446
003601                                                                  00003447
003602     MOVE OR-PRAT        TO    NR-INFO.                           00003448
003603     MOVE OR-AH-PRATA    TO    DIS-EDIT.                          00003449
003604     MOVE DIS-EDIT       TO    NR-AMT.                            00003450
003605     MOVE NEW-RPT        TO    PRT.                               00003451
003606     MOVE     ' '        TO    X.                                 00003452
003607     MOVE      X         TO    P-CTL.                             00003453
003608     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003454
003609     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003455
003610                                                                  00003456
003611     MOVE SPACES         TO    NR-INFO.                           00003457
003612     MOVE SPACES         TO    P-DATA.                            00003458
003613     MOVE     ' '        TO    X.                                 00003459
003614     MOVE      X         TO    P-CTL.                             00003460
003615     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003461
003616     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003462
003617                                                                  00003463
003618     MOVE WA-PRAT        TO    NR-INFO.                           00003464
003619     MOVE WA-AH-PRATA    TO    DIS-EDIT.                          00003465
003620     MOVE DIS-EDIT       TO    NR-AMT.                            00003466
003621     MOVE NEW-RPT        TO    PRT.                               00003467
003622     MOVE     ' '        TO    X.                                 00003468
003623     MOVE      X         TO    P-CTL.                             00003469
003624     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003470
003625     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003471
003626                                                                  00003472
003627     MOVE SPACES         TO    NR-INFO.                           00003473
003628     MOVE SPACES         TO    P-DATA.                            00003474
003629     MOVE     ' '        TO    X.                                 00003475
003630     MOVE      X         TO    P-CTL.                             00003476
003631     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003477
003632     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003478
003633                                                                  00003479
003634     MOVE CO-R78         TO    NR-INFO.                           00003480
003635     MOVE CO-OVRALL-R78  TO    DIS-EDIT.                          00003481
003636     MOVE DIS-EDIT       TO    NR-AMT.                            00003482
003637     MOVE NEW-RPT        TO    PRT.                               00003483
003638     MOVE     ' '        TO    X.                                 00003484
003639     MOVE      X         TO    P-CTL.                             00003485
003640     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003486
003641     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003487
003642                                                                  00003488
003643     MOVE SPACES         TO    NR-INFO.                           00003489
003644     MOVE SPACES         TO    P-DATA.                            00003490
003645     MOVE     ' '        TO    X.                                 00003491
003646     MOVE      X         TO    P-CTL.                             00003492
003647     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003493
003648     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003494
003649                                                                  00003495
061402     MOVE VA-R78         TO    NR-INFO.                           00003480
061402     MOVE VA-OVRALL-R78  TO    DIS-EDIT.                          00003481
061402     MOVE DIS-EDIT       TO    NR-AMT.                            00003482
061402     MOVE NEW-RPT        TO    PRT.                               00003483
061402     MOVE     ' '        TO    X.                                 00003484
061402     MOVE      X         TO    P-CTL.                             00003485
061402     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003486
061402     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003487
061402                                                                  00003488
061402     MOVE SPACES         TO    NR-INFO.                           00003489
061402     MOVE SPACES         TO    P-DATA.                            00003490
061402     MOVE     ' '        TO    X.                                 00003491
061402     MOVE      X         TO    P-CTL.                             00003492
061402     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003493
061402     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003494
061402                                                                  00003495
003650     MOVE CO-MEAN        TO    NR-INFO.                           00003496
003651     MOVE '('            TO    PAREN.                             00003497
003652     MOVE CO-OVRALL-R78  TO    DIS-EDIT                           00003498
003653     MOVE DIS-EDIT       TO    PM-R78.                            00003499
003654     MOVE CO-OVRALL-PRAT TO    DIS-EDIT.                          00003500
003655     MOVE DIS-EDIT       TO    PM-PRAT                            00003501
003656     MOVE PRINT-MEAN     TO    NR-AMT.                            00003502
003657     MOVE NEW-RPT        TO    PRT.                               00003503
003658     MOVE     ' '        TO    X.                                 00003504
003659     MOVE      X         TO    P-CTL.                             00003505
003660     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003506
003661     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003507
003662                                                                  00003508
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003489
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003490
CIDMOD     MOVE     ' '        TO    X.                                 00003491
CIDMOD     MOVE      X         TO    P-CTL.                             00003492
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003493
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003494
CIDMOD                                                                  00003495
003650     MOVE VA-MEAN-HEAD   TO    NR-INFO.                           00003496
003651     MOVE '('            TO    PAREN.                             00003497
003652     MOVE VA-OVRALL-R78  TO    DIS-EDIT                           00003498
003653     MOVE DIS-EDIT       TO    PM-R78.                            00003499
003654     MOVE VA-OVRALL-PRAT TO    DIS-EDIT.                          00003500
003655     MOVE DIS-EDIT       TO    PM-PRAT                            00003501
003656     MOVE PRINT-MEAN     TO    NR-AMT.                            00003502
003657     MOVE NEW-RPT        TO    PRT.                               00003503
003658     MOVE     ' '        TO    X.                                 00003504
003659     MOVE      X         TO    P-CTL.                             00003505
003660     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003506
003661     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003507
003662                                                                  00003508
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003489
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003490
CIDMOD     MOVE     ' '        TO    X.                                 00003491
CIDMOD     MOVE      X         TO    P-CTL.                             00003492
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003493
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003494
CIDMOD                                                                  00003495
CIDMOD     MOVE GA-MEAN        TO    NR-INFO.                           00003496
CIDMOD     MOVE '('            TO    PAREN.                             00003497
CIDMOD     MOVE GA-OVRALL-R78  TO    DIS-EDIT                           00003498
CIDMOD     MOVE DIS-EDIT       TO    PM-R78.                            00003499
CIDMOD     MOVE GA-OVRALL-PRAT TO    DIS-EDIT.                          00003500
CIDMOD     MOVE DIS-EDIT       TO    PM-PRAT                            00003501
CIDMOD     MOVE PRINT-MEAN     TO    NR-AMT.                            00003502
CIDMOD     MOVE NEW-RPT        TO    PRT.                               00003503
CIDMOD     MOVE     ' '        TO    X.                                 00003504
CIDMOD     MOVE      X         TO    P-CTL.                             00003505
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003506
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003507
CIDMOD                                                                  00003508
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003489
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003490
CIDMOD     MOVE     ' '        TO    X.                                 00003491
CIDMOD     MOVE      X         TO    P-CTL.                             00003492
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003493
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003494
CIDMOD                                                                  00003495
CIDMOD     MOVE OH-MEAN        TO    NR-INFO.                           00003496
CIDMOD     MOVE '('            TO    PAREN.                             00003497
CIDMOD     MOVE OH-OVRALL-R78  TO    DIS-EDIT                           00003498
CIDMOD     MOVE DIS-EDIT       TO    PM-R78.                            00003499
CIDMOD     MOVE OH-OVRALL-PRAT TO    DIS-EDIT.                          00003500
CIDMOD     MOVE DIS-EDIT       TO    PM-PRAT                            00003501
CIDMOD     MOVE PRINT-MEAN     TO    NR-AMT.                            00003502
CIDMOD     MOVE NEW-RPT        TO    PRT.                               00003503
CIDMOD     MOVE     ' '        TO    X.                                 00003504
CIDMOD     MOVE      X         TO    P-CTL.                             00003505
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003506
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003507
CIDMOD                                                                  00003508
CIDMOD     MOVE SPACES         TO    NR-INFO.                           00003489
CIDMOD     MOVE SPACES         TO    P-DATA.                            00003490
CIDMOD     MOVE     ' '        TO    X.                                 00003491
CIDMOD     MOVE      X         TO    P-CTL.                             00003492
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003493
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003494
CIDMOD                                                                  00003495
CIDMOD     MOVE TX-MEAN        TO    NR-INFO.                           00003496
CIDMOD     MOVE '('            TO    PAREN.                             00003497
CIDMOD     MOVE TX-OVRALL-R78  TO    DIS-EDIT                           00003498
CIDMOD     MOVE DIS-EDIT       TO    PM-R78.                            00003499
CIDMOD     MOVE TX-OVRALL-PRAT TO    DIS-EDIT.                          00003500
CIDMOD     MOVE DIS-EDIT       TO    PM-PRAT                            00003501
CIDMOD     MOVE PRINT-MEAN     TO    NR-AMT.                            00003502
CIDMOD     MOVE NEW-RPT        TO    PRT.                               00003503
CIDMOD     MOVE     ' '        TO    X.                                 00003504
CIDMOD     MOVE      X         TO    P-CTL.                             00003505
CIDMOD     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003506
CIDMOD     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003507
CIDMOD                                                                  00003508
003663     MOVE SPACES         TO    PAREN.                             00003509
003664                                                                  00003510
003665     MOVE SPACES         TO    NR-INFO.                           00003511
003666     MOVE SPACES         TO    P-DATA.                            00003512
003667     MOVE     ' '        TO    X.                                 00003513
003668     MOVE      X         TO    P-CTL.                             00003514
003669     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003515
003670     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003516
003671     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003517
003672     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003518
003673     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003519
003674     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003520
003675                                                                  00003521
003676     MOVE MO-TOTAL       TO    NR-INFO.                           00003522
061402     ADD  VA-OVRALL-PRAT VA-OVRALL-R78 GIVING VA-TOT.             00003523
061402     DIVIDE VA-TOT BY 2  GIVING VA-MEAN.                          00003524
003677     ADD  CO-OVRALL-PRAT CO-OVRALL-R78 GIVING COLO-TOT.           00003523
003678     DIVIDE COLO-TOT BY 2  GIVING COLO-MEAN.                      00003524
CIDMOD     ADD  GA-OVRALL-PRAT GA-OVRALL-R78 GIVING GA-TOT.             00003523
CIDMOD     DIVIDE GA-TOT BY 2  GIVING GEORGIA-MEAN.                     00003524
CIDMOD     ADD  OH-OVRALL-PRAT OH-OVRALL-R78 GIVING OH-TOT.             00003523
CIDMOD     DIVIDE OH-TOT BY 2  GIVING OHIO-MEAN.                        00003524
CIDMOD     ADD  TX-OVRALL-PRAT TX-OVRALL-R78 GIVING TX-TOT.             00003523
CIDMOD     DIVIDE TX-TOT BY 2  GIVING TEXAS-MEAN.                       00003524
003679                                                                  00003525
003680     ADD   ALL-AH-P78    TO       COLO-UNEARNED.                  00003526
003681                                                                  00003527
003682     SUBTRACT CA-AH-P78 FROM      COLO-UNEARNED.                  00003528
CIDMOD     SUBTRACT GA-AH-P78 FROM      COLO-UNEARNED.                  00003528
003683     SUBTRACT OR-AH-P78 FROM      COLO-UNEARNED.                  00003529
003684     SUBTRACT WA-AH-P78 FROM      COLO-UNEARNED.                  00003530
003685     SUBTRACT CO-OVRALL-R78 FROM  COLO-UNEARNED.                  00003531
061402     SUBTRACT VA-OVRALL-R78 FROM  COLO-UNEARNED.                  00003531
CIDMOD     SUBTRACT OH-OVRALL-R78 FROM  COLO-UNEARNED.                  00003531
CIDMOD     SUBTRACT TX-OVRALL-R78 FROM  COLO-UNEARNED.                  00003531
003686                                                                  00003532
003687     ADD      CA-AH-PRATA TO      COLO-UNEARNED.                  00003533
003688     ADD      OR-AH-PRATA TO      COLO-UNEARNED.                  00003534
003689     ADD      WA-AH-PRATA TO      COLO-UNEARNED.                  00003535
CIDMOD     ADD      GEORGIA-MEAN TO     COLO-UNEARNED.                  00003536
003690     ADD      COLO-MEAN   TO      COLO-UNEARNED.                  00003536
061402     ADD      VA-MEAN     TO      COLO-UNEARNED.                  00003536
CIDMOD     ADD      OHIO-MEAN   TO      COLO-UNEARNED.                  00003536
LFC173     ADD      TEXAS-MEAN  TO      COLO-UNEARNED.                  00003536
003691                                                                  00003537
003692     MOVE  COLO-UNEARNED TO DIS-EDIT.                             00003538
003693     MOVE  DIS-EDIT      TO    NR-AMT.                            00003539
003694     MOVE NEW-RPT        TO    PRT.                               00003540
003695     MOVE     ' '        TO    X.                                 00003541
003696     MOVE      X         TO    P-CTL.                             00003542
003697     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003543
003698     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003544
003699                                                                  00003545
003700     MOVE SPACES         TO    NR-INFO.                           00003546
003701     MOVE SPACES         TO    P-DATA.                            00003547
003702     MOVE     ' '        TO    X.                                 00003548
003703     MOVE      X         TO    P-CTL.                             00003549
003704     PERFORM  FIN-PRNT THRU FIN-EXIT .                            00003550
003705     PERFORM  ACT-PRNT THRU ACT-EXIT .                            00003551
003707                                                                  00003552
003708     CLOSE PRNTR                                                  00003553
003709           ACT-RPT                                                00003554
003710           FIN-RPT.                                               00003555
003712                                                                  00003556
003713     GOBACK.                                                      00003557
003714                                                                  00003558
003715 ABEND-PGM.                                                       00003559
003716                     COPY ELCABEND SUPPRESS.                      00003560
003717     EJECT                                                        00003561
