      *((program: EL680.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL680 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 04/20/94 15:21:06.
000007*                            VMOD 2.015
000008*
000009*AUTHOR.    LOGIC, INC.
000010*           DALLAS, TEXAS.
000011
000012*DATE-COMPILED.
000013
000014*SECURITY.   *****************************************************
000015*            *                                                   *
000016*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000017*            *                                                   *
000018*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000019*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000020*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000021*            *                                                   *
000022*            *****************************************************
000023
000024*REMARKS.    TRANSACTION - EXF6 - PAYMENT CALCULATIONS.
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000034* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000035******************************************************************
000036
000037     EJECT
000038 ENVIRONMENT DIVISION.
000039
000040 DATA DIVISION.
000041
000042 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000043 77  FILLER  PIC X(32)  VALUE '********************************'.
000044 77  FILLER  PIC X(32)  VALUE '*   EL680  WORKING STORAGE     *'.
000045 77  FILLER  PIC X(32)  VALUE '************VMOD=2.015 *********'.
000046
000047 01  WS-DATE-AREA.
000048     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000049     05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
000050
000051 01  ACCESS-KEYS.
000052     12  ELCNTL-KEY.
000053         16  CNTL-COMP-ID    PIC XXX.
000054         16  CNTL-REC-TYPE   PIC X.
000055         16  CNTL-ACCESS     PIC X(4).
000056         16  CNTL-SEQ-NO     PIC S9(4)    COMP.
000057
000058 01  SPC-FLD.
000059     03  SPC-FLD-9    PIC 99.
000060
000061*********************  INPUT AREAS *******************************
000062 01  FILLER.
000063     05  WS-NAME                 PIC X(20)           VALUE SPACES.
000064     05  WS-FREQ                 PIC XX              VALUE SPACES.
000065     05  WS-BASIS-X              PIC XX              VALUE SPACES.
000066     05  WS-NPBEN                PIC X               VALUE SPACES.
000067     05  WS-BALYN                PIC X               VALUE SPACES.
000068     05  WS-OBYN                 PIC X               VALUE SPACES.
000069     05  WS-ADDCHG               PIC X               VALUE SPACES.
000070         88 NO-CHG                                   VALUE ' '.
000071         88 CHG-LF                                   VALUE '1'.
000072         88 CHG-AH                                   VALUE '2'.
000073         88 CHG-LF-AH                                VALUE '3'.
000074
000075 01  FILLER      COMP-3.
000076     05  WS-AMOUNT-REQUESTED     PIC S9(9)V99        VALUE +0.
000077     05  WS-APR                  PIC S9(6)V9(8)      VALUE +0.
000078     05  WS-NUMBER-OF-PMTS       PIC S999            VALUE +0.
000079     05  WS-ADDTL-DAYS           PIC S999            VALUE +0.
000080     05  WS-TRUNC                PIC S999            VALUE +0.
000081     05  WS-EXTRA                PIC S9              VALUE +0.
000082     05  WS-LIFE-RATE            PIC S9(6)V9(8)      VALUE +0.
000083     05  WS-LIFE-DEVIATION       PIC S9(6)V9(8)      VALUE +0.
000084     05  WS-AH-RATE              PIC S9(6)V9(8)      VALUE +0.
000085     05  WS-BL-RATE              PIC S9(6)V9(8)      VALUE +0.
000086     05  WS-AH-DEVIATION         PIC S9(6)V9(8)      VALUE +0.
000087     05  WS-PAYMENTS-COVERED     PIC S999            VALUE +0.
000088     05  WS-BALLOON-AMOUNT       PIC S9(6)V99        VALUE +0.
000089*    05  WS-REGULAR-PAYMENT      PIC S9(6)V99        VALUE +0.
000090
000091     EJECT
000092
000093 01  FILLER         COMP-3.
000094     05  ANGLM                   PIC S9(7)V9(11) VALUE ZERO.
000095     05  ANGLN                   PIC S9(7)V9(11) VALUE ZERO.
000096
000097     05  AHN                     PIC S999        VALUE ZERO.
000098     05  ATF                     PIC S9(7)V9(11) VALUE ZERO.
000099     05  EMII                    PIC S999        VALUE ZERO.
000100     05  OTF                     PIC S9(7)V9(11) VALUE ZERO.
000101     05  PPY                     PIC S999        VALUE ZERO.
000102
000103     05  CA                      PIC S9(7)V9(11) VALUE ZERO.
000104     05  I                       PIC S9(7)V9(11) VALUE ZERO.
000105     05  K1                      PIC S9(7)V9(11) VALUE ZERO.
000106     05  L                       PIC S9(7)V9(11) VALUE ZERO.
000107     05  L1                      PIC S9(7)V9(11) VALUE ZERO.
000108     05  M                       PIC S9(7)V9(11) VALUE ZERO.
000109     05  N                       PIC S9(5)       VALUE ZERO.
000110     05  N1                      PIC S9(7)V9(11) VALUE ZERO.
000111     05  RA                      PIC S9(7)V9(11) VALUE ZERO.
000112     05  TA                      PIC S9(7)V9(11) VALUE ZERO.
000113     05  V                       PIC S9(7)V9(11) VALUE ZERO.
000114     05  VA                      PIC S9(7)V9(11) VALUE ZERO.
000115     05  VU                      PIC S9(7)V9(11) VALUE ZERO.
000116     05  VU1                     PIC S9(7)V9(11) VALUE ZERO.
000117
000118     05  NALF                    PIC S9(7)V9(11) VALUE ZERO.
000119     05  NPLF                    PIC S9(7)V9(11) VALUE ZERO.
000120     05  PADJ                    PIC S9(7)V9(11) VALUE ZERO.
000121
000122     05  TOT-PMTS                PIC S9(7)V9(11) VALUE ZERO.
000123     05  ATOT-PMTS               PIC S9(7)V9(11) VALUE ZERO.
000124     05  ADL-DAYS                PIC S999        VALUE ZERO.
000125     05  ADL-ADJ                 PIC S9(7)V9(11) VALUE ZERO.
000126     05  COV-TERM                PIC S999        VALUE ZERO.
000127
000128     05  LIFE-RATE               PIC S9(7)V9(11) VALUE ZERO.
000129     05  LIFEDEV                 PIC S9(7)V9(11) VALUE ZERO.
000130     05  LR                      PIC S9(7)V9(11) VALUE ZERO.
000131     05  LIFE-PREM               PIC S9(7)V9(11) VALUE ZERO.
000132
000133     05  AH-RATE                 PIC S9(7)V9(11) VALUE ZERO.
000134     05  AHDEV                   PIC S9(7)V9(11) VALUE ZERO.
000135     05  AH                      PIC S9(7)V9(11) VALUE ZERO.
000136     05  AH-PREM                 PIC S9(7)V9(11) VALUE ZERO.
000137
000138     05  BL-RATE                 PIC S9(7)V9(11) VALUE ZERO.
000139     05  BL                      PIC S9(7)V9(11) VALUE ZERO.
000140
000141************ OUTPUT AREAS ******
000142     05  L-PREMIUM               PIC S9(7)V99    VALUE ZERO.
000143     05  A-PREMIUM               PIC S9(7)V99    VALUE ZERO.
000144     05  PAYMENT                 PIC S9(7)V99    VALUE ZERO.
000145     05  TOT-FIN                 PIC S9(7)V99    VALUE ZERO.
000146     05  TOT-PAYMT               PIC S9(7)V99    VALUE ZERO.
000147     05  TOT-INTRS               PIC S9(7)V99    VALUE ZERO.
000148
000149 01  FILLER         COMP-3.
000150     05  V-NTH                   PIC S9(7)V9(11) VALUE ZERO.
000151     05  B                       PIC S9(7)V9(11) VALUE ZERO.
000152     05  ANGLN-LESS-1            PIC S9(7)V9(11) VALUE ZERO.
000153     05  NM1                     PIC S9(7)V9(11) VALUE ZERO.
000154     05  AEQ                     PIC S9(7)V9(11) VALUE ZERO.
000155     05  MPT                     PIC S9(7)V9(11) VALUE ZERO.
000156     05  MPB                     PIC S9(7)V9(11) VALUE ZERO.
000157     05  MP                      PIC S9(7)V9(11) VALUE ZERO.
000158     05  IA                      PIC S9(7)V9(11) VALUE ZERO.
000159
000160 01  FILLER.
000161     05  XREPT                       PIC S999    COMP.
000162     05  WS-ERROR-COUNT              PIC S999        VALUE ZERO.
000163     05  WS-MESSAGE                  PIC X(79).
000164     05  WS-BASIS                    PIC S99         VALUE ZERO.
000165     05  WS-TIME-WORK                PIC S9(7)    VALUE ZERO.
000166     05  WS-TIME                     REDEFINES
000167         WS-TIME-WORK                PIC S999V9(4).
000168
000169     EJECT
000170 01  ERROR-MESSAGES.
000171     12  ER-0004                 PIC X(4)  VALUE '0004'.
000172     12  ER-0008                 PIC X(4)  VALUE '0008'.
000173     12  ER-0029                 PIC X(4)  VALUE '0029'.
000174     12  ER-0042                 PIC X(4)  VALUE '0042'.
000175     12  ER-0190                 PIC X(4)  VALUE '0190'.
000176     12  ER-0412                 PIC X(4)  VALUE '0412'.
000177     12  ER-0413                 PIC X(4)  VALUE '0413'.
000178     12  ER-2361                 PIC X(4)  VALUE '2361'.
000179     12  ER-2363                 PIC X(4)  VALUE '2363'.
000180     12  ER-2364                 PIC X(4)  VALUE '2364'.
000181     12  ER-2365                 PIC X(4)  VALUE '2365'.
000182     12  ER-2366                 PIC X(4)  VALUE '2366'.
000183     12  ER-2367                 PIC X(4)  VALUE '2367'.
000184     12  ER-2920                 PIC X(4)  VALUE '2920'.
000185     12  ER-2921                 PIC X(4)  VALUE '2921'.
000186     12  ER-2922                 PIC X(4)  VALUE '2922'.
000187     12  ER-2923                 PIC X(4)  VALUE '2923'.
000188     12  ER-2924                 PIC X(4)  VALUE '2924'.
000189     12  ER-2925                 PIC X(4)  VALUE '2925'.
000190     12  ER-2926                 PIC X(4)  VALUE '2926'.
000191     12  ER-2927                 PIC X(4)  VALUE '2927'.
000192     12  ER-4004                 PIC X(4)  VALUE '4004'.
000193     12  ER-4007                 PIC X(4)  VALUE '4007'.
000194     EJECT
000195 01  STANDARD-AREAS.
000196     12  MAPSET-NAME         PIC X(8)    VALUE 'EL680S'.
000197     12  MAP-NAME            PIC X(8)    VALUE 'EL680A'.
000198     12  SCREEN-NUMBER       PIC X(4)    VALUE '680A'.
000199     12  TRANS-ID            PIC X(4)    VALUE 'EXF6'.
000200     12  START-TRANS-ID      PIC X(4)    VALUE 'EXF7'.
000201     12  THIS-PGM            PIC X(8)    VALUE 'EL680'.
000202     12  PGM-NAME            PIC X(8).
000203     12  XCTL-005            PIC X(8)    VALUE 'EL005'.
000204     12  XCTL-010            PIC X(8)    VALUE 'EL010'.
000205     12  XCTL-626            PIC X(8)    VALUE 'EL626'.
000206     12  LINK-001            PIC X(8)    VALUE 'EL001'.
000207     12  LINK-004            PIC X(8)    VALUE 'EL004'.
000208
000209     EJECT
000210*                            COPY EL680S.
      *>>((file: EL680S))
000001 01  EL680AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  ADATEL PIC S9(0004) COMP.
000005     05  ADATEF PIC  X(0001).
000006     05  FILLER REDEFINES ADATEF.
000007         10  ADATEA PIC  X(0001).
000008     05  ADATEI PIC  X(0008).
000009*    -------------------------------
000010     05  ATIMEL PIC S9(0004) COMP.
000011     05  ATIMEF PIC  X(0001).
000012     05  FILLER REDEFINES ATIMEF.
000013         10  ATIMEA PIC  X(0001).
000014     05  ATIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  ACOMPIDL PIC S9(0004) COMP.
000017     05  ACOMPIDF PIC  X(0001).
000018     05  FILLER REDEFINES ACOMPIDF.
000019         10  ACOMPIDA PIC  X(0001).
000020     05  ACOMPIDI PIC  X(0003).
000021*    -------------------------------
000022     05  ANAMEL PIC S9(0004) COMP.
000023     05  ANAMEF PIC  X(0001).
000024     05  FILLER REDEFINES ANAMEF.
000025         10  ANAMEA PIC  X(0001).
000026     05  ANAMEI PIC  X(0020).
000027*    -------------------------------
000028     05  AAMOUNTL PIC S9(0004) COMP.
000029     05  AAMOUNTF PIC  X(0001).
000030     05  FILLER REDEFINES AAMOUNTF.
000031         10  AAMOUNTA PIC  X(0001).
000032     05  AAMOUNTI PIC  9(10)V99.
000033*    -------------------------------
000034     05  AINTRATL PIC S9(0004) COMP.
000035     05  AINTRATF PIC  X(0001).
000036     05  FILLER REDEFINES AINTRATF.
000037         10  AINTRATA PIC  X(0001).
000038     05  AINTRATI PIC  9(4)V9(4).
000039*    -------------------------------
000040     05  AFREQL PIC S9(0004) COMP.
000041     05  AFREQF PIC  X(0001).
000042     05  FILLER REDEFINES AFREQF.
000043         10  AFREQA PIC  X(0001).
000044     05  AFREQI PIC  X(0002).
000045*    -------------------------------
000046     05  BAMOUNTL PIC S9(0004) COMP.
000047     05  BAMOUNTF PIC  X(0001).
000048     05  FILLER REDEFINES BAMOUNTF.
000049         10  BAMOUNTA PIC  X(0001).
000050     05  BAMOUNTI PIC  X(0011).
000051*    -------------------------------
000052     05  ANOPMTSL PIC S9(0004) COMP.
000053     05  ANOPMTSF PIC  X(0001).
000054     05  FILLER REDEFINES ANOPMTSF.
000055         10  ANOPMTSA PIC  X(0001).
000056     05  ANOPMTSI PIC  999.
000057*    -------------------------------
000058     05  BLAMTL PIC S9(0004) COMP.
000059     05  BLAMTF PIC  X(0001).
000060     05  FILLER REDEFINES BLAMTF.
000061         10  BLAMTA PIC  X(0001).
000062     05  BLAMTI PIC  X(0011).
000063*    -------------------------------
000064     05  AADDAYSL PIC S9(0004) COMP.
000065     05  AADDAYSF PIC  X(0001).
000066     05  FILLER REDEFINES AADDAYSF.
000067         10  AADDAYSA PIC  X(0001).
000068     05  AADDAYSI PIC  999.
000069*    -------------------------------
000070     05  ADDCHGL PIC S9(0004) COMP.
000071     05  ADDCHGF PIC  X(0001).
000072     05  FILLER REDEFINES ADDCHGF.
000073         10  ADDCHGA PIC  X(0001).
000074     05  ADDCHGI PIC  X(0001).
000075*    -------------------------------
000076     05  BDAMTL PIC S9(0004) COMP.
000077     05  BDAMTF PIC  X(0001).
000078     05  FILLER REDEFINES BDAMTF.
000079         10  BDAMTA PIC  X(0001).
000080     05  BDAMTI PIC  X(0011).
000081*    -------------------------------
000082     05  ABASISL PIC S9(0004) COMP.
000083     05  ABASISF PIC  X(0001).
000084     05  FILLER REDEFINES ABASISF.
000085         10  ABASISA PIC  X(0001).
000086     05  ABASISI PIC  X(0002).
000087*    -------------------------------
000088     05  BMOPMTL PIC S9(0004) COMP.
000089     05  BMOPMTF PIC  X(0001).
000090     05  FILLER REDEFINES BMOPMTF.
000091         10  BMOPMTA PIC  X(0001).
000092     05  BMOPMTI PIC  X(0011).
000093*    -------------------------------
000094     05  OBYORNL PIC S9(0004) COMP.
000095     05  OBYORNF PIC  X(0001).
000096     05  FILLER REDEFINES OBYORNF.
000097         10  OBYORNA PIC  X(0001).
000098     05  OBYORNI PIC  X(0001).
000099*    -------------------------------
000100     05  BALPMTL PIC S9(0004) COMP.
000101     05  BALPMTF PIC  X(0001).
000102     05  FILLER REDEFINES BALPMTF.
000103         10  BALPMTA PIC  X(0001).
000104     05  BALPMTI PIC  X(0011).
000105*    -------------------------------
000106     05  ANPREML PIC S9(0004) COMP.
000107     05  ANPREMF PIC  X(0001).
000108     05  FILLER REDEFINES ANPREMF.
000109         10  ANPREMA PIC  X(0001).
000110     05  ANPREMI PIC  X(0001).
000111*    -------------------------------
000112     05  BTOTPMTL PIC S9(0004) COMP.
000113     05  BTOTPMTF PIC  X(0001).
000114     05  FILLER REDEFINES BTOTPMTF.
000115         10  BTOTPMTA PIC  X(0001).
000116     05  BTOTPMTI PIC  X(0011).
000117*    -------------------------------
000118     05  ATRUNCL PIC S9(0004) COMP.
000119     05  ATRUNCF PIC  X(0001).
000120     05  FILLER REDEFINES ATRUNCF.
000121         10  ATRUNCA PIC  X(0001).
000122     05  ATRUNCI PIC  999.
000123*    -------------------------------
000124     05  BTOTINTL PIC S9(0004) COMP.
000125     05  BTOTINTF PIC  X(0001).
000126     05  FILLER REDEFINES BTOTINTF.
000127         10  BTOTINTA PIC  X(0001).
000128     05  BTOTINTI PIC  X(0011).
000129*    -------------------------------
000130     05  AEXTRAL PIC S9(0004) COMP.
000131     05  AEXTRAF PIC  X(0001).
000132     05  FILLER REDEFINES AEXTRAF.
000133         10  AEXTRAA PIC  X(0001).
000134     05  AEXTRAI PIC  9.
000135*    -------------------------------
000136     05  ALRATEL PIC S9(0004) COMP.
000137     05  ALRATEF PIC  X(0001).
000138     05  FILLER REDEFINES ALRATEF.
000139         10  ALRATEA PIC  X(0001).
000140     05  ALRATEI PIC  9(4)V9(4).
000141*    -------------------------------
000142     05  ALFDEVL PIC S9(0004) COMP.
000143     05  ALFDEVF PIC  X(0001).
000144     05  FILLER REDEFINES ALFDEVF.
000145         10  ALFDEVA PIC  X(0001).
000146     05  ALFDEVI PIC  9(4)V999.
000147*    -------------------------------
000148     05  ADRATEL PIC S9(0004) COMP.
000149     05  ADRATEF PIC  X(0001).
000150     05  FILLER REDEFINES ADRATEF.
000151         10  ADRATEA PIC  X(0001).
000152     05  ADRATEI PIC  9(4)V9(4).
000153*    -------------------------------
000154     05  AAHDEVL PIC S9(0004) COMP.
000155     05  AAHDEVF PIC  X(0001).
000156     05  FILLER REDEFINES AAHDEVF.
000157         10  AAHDEVA PIC  X(0001).
000158     05  AAHDEVI PIC  9(4)V999.
000159*    -------------------------------
000160     05  ANOTLFL PIC S9(0004) COMP.
000161     05  ANOTLFF PIC  X(0001).
000162     05  FILLER REDEFINES ANOTLFF.
000163         10  ANOTLFA PIC  X(0001).
000164     05  ANOTLFI PIC  999.
000165*    -------------------------------
000166     05  BALAMTL PIC S9(0004) COMP.
000167     05  BALAMTF PIC  X(0001).
000168     05  FILLER REDEFINES BALAMTF.
000169         10  BALAMTA PIC  X(0001).
000170     05  BALAMTI PIC  9(10)V99.
000171*    -------------------------------
000172     05  BALRL PIC S9(0004) COMP.
000173     05  BALRF PIC  X(0001).
000174     05  FILLER REDEFINES BALRF.
000175         10  BALRA PIC  X(0001).
000176     05  BALRI PIC  X(0036).
000177*    -------------------------------
000178     05  BLRATEL PIC S9(0004) COMP.
000179     05  BLRATEF PIC  X(0001).
000180     05  FILLER REDEFINES BLRATEF.
000181         10  BLRATEA PIC  X(0001).
000182     05  BLRATEI PIC  9(4)V9(4).
000183*    -------------------------------
000184     05  BALYNL PIC S9(0004) COMP.
000185     05  BALYNF PIC  X(0001).
000186     05  FILLER REDEFINES BALYNF.
000187         10  BALYNA PIC  X(0001).
000188     05  BALYNI PIC  X(0001).
000189*    -------------------------------
000190     05  ERRMSG1L PIC S9(0004) COMP.
000191     05  ERRMSG1F PIC  X(0001).
000192     05  FILLER REDEFINES ERRMSG1F.
000193         10  ERRMSG1A PIC  X(0001).
000194     05  ERRMSG1I PIC  X(0079).
000195*    -------------------------------
000196     05  PFENTERL PIC S9(0004) COMP.
000197     05  PFENTERF PIC  X(0001).
000198     05  FILLER REDEFINES PFENTERF.
000199         10  PFENTERA PIC  X(0001).
000200     05  PFENTERI PIC  99.
000201 01  EL680AO REDEFINES EL680AI.
000202     05  FILLER            PIC  X(0012).
000203*    -------------------------------
000204     05  FILLER            PIC  X(0003).
000205     05  ADATEO PIC  X(0008).
000206*    -------------------------------
000207     05  FILLER            PIC  X(0003).
000208     05  ATIMEO PIC  99.99.
000209*    -------------------------------
000210     05  FILLER            PIC  X(0003).
000211     05  ACOMPIDO PIC  X(0003).
000212*    -------------------------------
000213     05  FILLER            PIC  X(0003).
000214     05  ANAMEO PIC  X(0020).
000215*    -------------------------------
000216     05  FILLER            PIC  X(0003).
000217     05  AAMOUNTO PIC  Z,ZZZ,ZZ9.99.
000218*    -------------------------------
000219     05  FILLER            PIC  X(0003).
000220     05  AINTRATO PIC  ZZ9.9(4).
000221*    -------------------------------
000222     05  FILLER            PIC  X(0003).
000223     05  AFREQO PIC  X(0002).
000224*    -------------------------------
000225     05  FILLER            PIC  X(0003).
000226     05  BAMOUNTO PIC  ZZZ,ZZ9.99-.
000227*    -------------------------------
000228     05  FILLER            PIC  X(0003).
000229     05  ANOPMTSO PIC  ZZ9.
000230*    -------------------------------
000231     05  FILLER            PIC  X(0003).
000232     05  BLAMTO PIC  ZZZ,ZZ9.99-.
000233*    -------------------------------
000234     05  FILLER            PIC  X(0003).
000235     05  AADDAYSO PIC  ZZ9.
000236*    -------------------------------
000237     05  FILLER            PIC  X(0003).
000238     05  ADDCHGO PIC  X(0001).
000239*    -------------------------------
000240     05  FILLER            PIC  X(0003).
000241     05  BDAMTO PIC  ZZZ,ZZ9.99-.
000242*    -------------------------------
000243     05  FILLER            PIC  X(0003).
000244     05  ABASISO PIC  X(0002).
000245*    -------------------------------
000246     05  FILLER            PIC  X(0003).
000247     05  BMOPMTO PIC  ZZZ,ZZ9.99-.
000248*    -------------------------------
000249     05  FILLER            PIC  X(0003).
000250     05  OBYORNO PIC  X(0001).
000251*    -------------------------------
000252     05  FILLER            PIC  X(0003).
000253     05  BALPMTO PIC  ZZZ,ZZ9.99-.
000254*    -------------------------------
000255     05  FILLER            PIC  X(0003).
000256     05  ANPREMO PIC  X(0001).
000257*    -------------------------------
000258     05  FILLER            PIC  X(0003).
000259     05  BTOTPMTO PIC  ZZZ,ZZ9.99-.
000260*    -------------------------------
000261     05  FILLER            PIC  X(0003).
000262     05  ATRUNCO PIC  ZZ9.
000263*    -------------------------------
000264     05  FILLER            PIC  X(0003).
000265     05  BTOTINTO PIC  ZZZ,ZZ9.99-.
000266*    -------------------------------
000267     05  FILLER            PIC  X(0003).
000268     05  AEXTRAO PIC  9.
000269*    -------------------------------
000270     05  FILLER            PIC  X(0003).
000271     05  ALRATEO PIC  ZZ9.9(4).
000272*    -------------------------------
000273     05  FILLER            PIC  X(0003).
000274     05  ALFDEVO PIC  ZZ9.999.
000275*    -------------------------------
000276     05  FILLER            PIC  X(0003).
000277     05  ADRATEO PIC  ZZ9.9(4).
000278*    -------------------------------
000279     05  FILLER            PIC  X(0003).
000280     05  AAHDEVO PIC  ZZ9.999.
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  ANOTLFO PIC  ZZ9.
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  BALAMTO PIC  Z,ZZZ,ZZ9.99.
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  BALRO PIC  X(0036).
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  BLRATEO PIC  ZZ9.9(4).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  BALYNO PIC  X(0001).
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  ERRMSG1O PIC  X(0079).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  PFENTERO PIC  Z9.
000302*    -------------------------------
      *<<((file: EL680S))
000211     EJECT
000212*                            COPY ELCLOGOF.
      *>>((file: ELCLOGOF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCLOGOF.                           *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
000008*                                                                *
000009******************************************************************
000010 01  CLASIC-LOGOFF.
000011     12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
000012     12  LOGOFF-TEXT.
000013         16  FILLER          PIC X(5)    VALUE SPACES.
000014         16  LOGOFF-MSG.
000015             20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
000016             20  FILLER      PIC X       VALUE SPACES.
000017             20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
000018         16  FILLER          PIC X(80)
000019           VALUE '* YOU ARE NOW LOGGED OFF'.
000020         16  FILLER          PIC X(7)    VALUE '* LOGIC'.
000021         16  FILLER          PIC X       VALUE QUOTE.
000022         16  LOGOFF-SYS-MSG  PIC X(17)
000023           VALUE 'S CLAS-IC SYSTEM '.
000024     12  TEXT-MESSAGES.
000025         16  UNACCESS-MSG    PIC X(29)
000026             VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
000027         16  PGMIDERR-MSG    PIC X(17)
000028             VALUE 'PROGRAM NOT FOUND'.
      *<<((file: ELCLOGOF))
000213     EJECT
000214*                            COPY ELCDATE.
      *>>((file: ELCDATE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDATE.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*                                                                *
000009*   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
000010*                 LENGTH = 200                                   *
000011******************************************************************
000012
000013 01  DATE-CONVERSION-DATA.
000014     12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
000015     12  DC-OPTION-CODE                PIC X.
000016         88  BIN-TO-GREG                VALUE ' '.
000017         88  ELAPSED-BETWEEN-BIN        VALUE '1'.
000018         88  EDIT-GREG-TO-BIN           VALUE '2'.
000019         88  YMD-GREG-TO-BIN            VALUE '3'.
000020         88  MDY-GREG-TO-BIN            VALUE '4'.
000021         88  JULIAN-TO-BIN              VALUE '5'.
000022         88  BIN-PLUS-ELAPSED           VALUE '6'.
000023         88  FIND-CENTURY               VALUE '7'.
000024         88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
000025         88  EDIT-GREG-TO-BIN-3         VALUE '9'.
000026         88  YMD-GREG-TO-BIN-3          VALUE 'A'.
000027         88  MDY-GREG-TO-BIN-3          VALUE 'B'.
000028         88  JULIAN-TO-BIN-3            VALUE 'C'.
000029         88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
000030         88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
000031         88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
000032         88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
000033         88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
000034         88  CHECK-LEAP-YEAR            VALUE 'H'.
000035         88  BIN-3-TO-GREG              VALUE 'I'.
000036         88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
000037         88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
000038         88  CYMD-GREG-TO-BIN           VALUE 'L'.
000039         88  MDCY-GREG-TO-BIN           VALUE 'M'.
000040         88  MDY-GREG-TO-JULIAN         VALUE 'N'.
000041         88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
000042         88  YMD-GREG-TO-JULIAN         VALUE 'P'.
000043         88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
000044         88  THREE-CHARACTER-BIN
000045                  VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
000046         88  GREGORIAN-TO-BIN
000047                  VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
000048         88  BIN-TO-GREGORIAN
000049                  VALUES ' ' '1' 'I' '8' 'G'.
000050         88  JULIAN-TO-BINARY
000051                  VALUES '5' 'C' 'E' 'F'.
000052     12  DC-ERROR-CODE                 PIC X.
000053         88  NO-CONVERSION-ERROR        VALUE ' '.
000054         88  DATE-CONVERSION-ERROR
000055                  VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
000056         88  DATE-IS-ZERO               VALUE '1'.
000057         88  DATE-IS-NON-NUMERIC        VALUE '2'.
000058         88  DATE-IS-INVALID            VALUE '3'.
000059         88  DATE1-GREATER-DATE2        VALUE '4'.
000060         88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
000061         88  DATE-INVALID-OPTION        VALUE '9'.
000062         88  INVALID-CENTURY            VALUE 'A'.
000063         88  ONLY-CENTURY               VALUE 'B'.
000064         88  ONLY-LEAP-YEAR             VALUE 'C'.
000065         88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
000066     12  DC-END-OF-MONTH               PIC X.
000067         88  CALCULATE-END-OF-MONTH     VALUE '1'.
000068     12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
000069         88  USE-NORMAL-PROCESS         VALUE ' '.
000070         88  ADJUST-DOWN-100-YRS        VALUE '1'.
000071         88  ADJUST-UP-100-YRS          VALUE '2'.
000072     12  FILLER                        PIC X.
000073     12  DC-CONVERSION-DATES.
000074         16  DC-BIN-DATE-1             PIC XX.
000075         16  DC-BIN-DATE-2             PIC XX.
000076         16  DC-GREG-DATE-1-EDIT       PIC X(08).
000077         16  DC-GREG-DATE-1-EDIT-R REDEFINES
000078                       DC-GREG-DATE-1-EDIT.
000079             20  DC-EDIT1-MONTH        PIC 99.
000080             20  SLASH1-1              PIC X.
000081             20  DC-EDIT1-DAY          PIC 99.
000082             20  SLASH1-2              PIC X.
000083             20  DC-EDIT1-YEAR         PIC 99.
000084         16  DC-GREG-DATE-2-EDIT       PIC X(08).
000085         16  DC-GREG-DATE-2-EDIT-R REDEFINES
000086                     DC-GREG-DATE-2-EDIT.
000087             20  DC-EDIT2-MONTH        PIC 99.
000088             20  SLASH2-1              PIC X.
000089             20  DC-EDIT2-DAY          PIC 99.
000090             20  SLASH2-2              PIC X.
000091             20  DC-EDIT2-YEAR         PIC 99.
000092         16  DC-GREG-DATE-1-YMD        PIC 9(06).
000093         16  DC-GREG-DATE-1-YMD-R  REDEFINES
000094                     DC-GREG-DATE-1-YMD.
000095             20  DC-YMD-YEAR           PIC 99.
000096             20  DC-YMD-MONTH          PIC 99.
000097             20  DC-YMD-DAY            PIC 99.
000098         16  DC-GREG-DATE-1-MDY        PIC 9(06).
000099         16  DC-GREG-DATE-1-MDY-R REDEFINES
000100                      DC-GREG-DATE-1-MDY.
000101             20  DC-MDY-MONTH          PIC 99.
000102             20  DC-MDY-DAY            PIC 99.
000103             20  DC-MDY-YEAR           PIC 99.
000104         16  DC-GREG-DATE-1-ALPHA.
000105             20  DC-ALPHA-MONTH        PIC X(10).
000106             20  DC-ALPHA-DAY          PIC 99.
000107             20  FILLER                PIC XX.
000108             20  DC-ALPHA-CENTURY.
000109                 24 DC-ALPHA-CEN-N     PIC 99.
000110             20  DC-ALPHA-YEAR         PIC 99.
000111         16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
000112         16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
000113         16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
000114         16  DC-JULIAN-DATE            PIC 9(05).
000115         16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
000116                                       PIC 9(05).
000117         16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
000118             20  DC-JULIAN-YEAR        PIC 99.
000119             20  DC-JULIAN-DAYS        PIC 999.
000120         16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
000121         16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
000122         16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
000123     12  DATE-CONVERSION-VARIBLES.
000124         16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
000125         16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
000126             20  FILLER                PIC 9(3).
000127             20  HOLD-CEN-1-CCYY.
000128                 24  HOLD-CEN-1-CC     PIC 99.
000129                 24  HOLD-CEN-1-YY     PIC 99.
000130             20  HOLD-CEN-1-MO         PIC 99.
000131             20  HOLD-CEN-1-DA         PIC 99.
000132         16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
000133             20  HOLD-CEN-1-R-MO       PIC 99.
000134             20  HOLD-CEN-1-R-DA       PIC 99.
000135             20  HOLD-CEN-1-R-CCYY.
000136                 24  HOLD-CEN-1-R-CC   PIC 99.
000137                 24  HOLD-CEN-1-R-YY   PIC 99.
000138             20  FILLER                PIC 9(3).
000139         16  HOLD-CENTURY-1-X.
000140             20  FILLER                PIC X(3)  VALUE SPACES.
000141             20  HOLD-CEN-1-X-CCYY.
000142                 24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
000143                 24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
000144             20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
000145             20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
000146         16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
000147             20  HOLD-CEN-1-R-X-MO     PIC XX.
000148             20  HOLD-CEN-1-R-X-DA     PIC XX.
000149             20  HOLD-CEN-1-R-X-CCYY.
000150                 24  HOLD-CEN-1-R-X-CC PIC XX.
000151                 24  HOLD-CEN-1-R-X-YY PIC XX.
000152             20  FILLER                PIC XXX.
000153         16  DC-BIN-DATE-EXPAND-1      PIC XXX.
000154         16  DC-BIN-DATE-EXPAND-2      PIC XXX.
000155         16  DC-JULIAN-DATE-1          PIC 9(07).
000156         16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
000157             20  DC-JULIAN-1-CCYY.
000158                 24  DC-JULIAN-1-CC    PIC 99.
000159                 24  DC-JULIAN-1-YR    PIC 99.
000160             20  DC-JULIAN-DA-1        PIC 999.
000161         16  DC-JULIAN-DATE-2          PIC 9(07).
000162         16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
000163             20  DC-JULIAN-2-CCYY.
000164                 24  DC-JULIAN-2-CC    PIC 99.
000165                 24  DC-JULIAN-2-YR    PIC 99.
000166             20  DC-JULIAN-DA-2        PIC 999.
000167         16  DC-GREG-DATE-A-EDIT.
000168             20  DC-EDITA-MONTH        PIC 99.
000169             20  SLASHA-1              PIC X VALUE '/'.
000170             20  DC-EDITA-DAY          PIC 99.
000171             20  SLASHA-2              PIC X VALUE '/'.
000172             20  DC-EDITA-CCYY.
000173                 24  DC-EDITA-CENT     PIC 99.
000174                 24  DC-EDITA-YEAR     PIC 99.
000175         16  DC-GREG-DATE-B-EDIT.
000176             20  DC-EDITB-MONTH        PIC 99.
000177             20  SLASHB-1              PIC X VALUE '/'.
000178             20  DC-EDITB-DAY          PIC 99.
000179             20  SLASHB-2              PIC X VALUE '/'.
000180             20  DC-EDITB-CCYY.
000181                 24  DC-EDITB-CENT     PIC 99.
000182                 24  DC-EDITB-YEAR     PIC 99.
000183         16  DC-GREG-DATE-CYMD         PIC 9(08).
000184         16  DC-GREG-DATE-CYMD-R REDEFINES
000185                              DC-GREG-DATE-CYMD.
000186             20  DC-CYMD-CEN           PIC 99.
000187             20  DC-CYMD-YEAR          PIC 99.
000188             20  DC-CYMD-MONTH         PIC 99.
000189             20  DC-CYMD-DAY           PIC 99.
000190         16  DC-GREG-DATE-MDCY         PIC 9(08).
000191         16  DC-GREG-DATE-MDCY-R REDEFINES
000192                              DC-GREG-DATE-MDCY.
000193             20  DC-MDCY-MONTH         PIC 99.
000194             20  DC-MDCY-DAY           PIC 99.
000195             20  DC-MDCY-CEN           PIC 99.
000196             20  DC-MDCY-YEAR          PIC 99.
000197    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
000198        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
000199    12  DC-EL310-DATE                  PIC X(21).
000200    12  FILLER                         PIC X(28).
      *<<((file: ELCDATE))
000215     EJECT
000216*                            COPY ELCATTR.
      *>>((file: ELCATTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCATTR.                            *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*             LIST OF STANDARD ATTRIBUTE VALUES                  *
000007*                                                                *
000008*   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
000009*                                                                *
000010*                   POS 1   P=PROTECTED                          *
000011*                           U=UNPROTECTED                        *
000012*                           S=ASKIP                              *
000013*                   POS 2   A=ALPHA/NUMERIC                      *
000014*                           N=NUMERIC                            *
000015*                   POS 3   N=NORMAL                             *
000016*                           B=BRIGHT                             *
000017*                           D=DARK                               *
000018*                   POS 4-5 ON=MODIFIED DATA TAG ON              *
000019*                           OF=MODIFIED DATA TAG OFF             *
000020*                                                                *
000021*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
000022******************************************************************
000023 01  ATTRIBUTE-LIST.
000024     12  AL-PABOF            PIC X       VALUE 'Y'.
000025     12  AL-PABON            PIC X       VALUE 'Z'.
000026     12  AL-PADOF            PIC X       VALUE '%'.
000027     12  AL-PADON            PIC X       VALUE '_'.
000028     12  AL-PANOF            PIC X       VALUE '-'.
000029     12  AL-PANON            PIC X       VALUE '/'.
000030     12  AL-SABOF            PIC X       VALUE '8'.
000031     12  AL-SABON            PIC X       VALUE '9'.
000032     12  AL-SADOF            PIC X       VALUE '@'.
000033     12  AL-SADON            PIC X       VALUE QUOTE.
000034     12  AL-SANOF            PIC X       VALUE '0'.
000035     12  AL-SANON            PIC X       VALUE '1'.
000036     12  AL-UABOF            PIC X       VALUE 'H'.
000037     12  AL-UABON            PIC X       VALUE 'I'.
000038     12  AL-UADOF            PIC X       VALUE '<'.
000039     12  AL-UADON            PIC X       VALUE '('.
000040     12  AL-UANOF            PIC X       VALUE ' '.
000041     12  AL-UANON            PIC X       VALUE 'A'.
000042     12  AL-UNBOF            PIC X       VALUE 'Q'.
000043     12  AL-UNBON            PIC X       VALUE 'R'.
000044     12  AL-UNDOF            PIC X       VALUE '*'.
000045     12  AL-UNDON            PIC X       VALUE ')'.
000046     12  AL-UNNOF            PIC X       VALUE '&'.
000047     12  AL-UNNON            PIC X       VALUE 'J'.
      *<<((file: ELCATTR))
000217     EJECT
000218*                            COPY ELCAID.
      *>>((file: ELCAID))
000001******************************************************************
000002*                                                                *
000003*                            ELCAID.                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
000007*                                                                *
000008*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
000009*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
000010******************************************************************
000011
000012 01  DFHAID.
000013   02  DFHNULL   PIC  X  VALUE  ' '.
000014   02  DFHENTER  PIC  X  VALUE  QUOTE.
000015   02  DFHCLEAR  PIC  X  VALUE  '_'.
000016   02  DFHPEN    PIC  X  VALUE  '='.
000017   02  DFHOPID   PIC  X  VALUE  'W'.
000018   02  DFHPA1    PIC  X  VALUE  '%'.
000019   02  DFHPA2    PIC  X  VALUE  '>'.
000020   02  DFHPA3    PIC  X  VALUE  ','.
000021   02  DFHPF1    PIC  X  VALUE  '1'.
000022   02  DFHPF2    PIC  X  VALUE  '2'.
000023   02  DFHPF3    PIC  X  VALUE  '3'.
000024   02  DFHPF4    PIC  X  VALUE  '4'.
000025   02  DFHPF5    PIC  X  VALUE  '5'.
000026   02  DFHPF6    PIC  X  VALUE  '6'.
000027   02  DFHPF7    PIC  X  VALUE  '7'.
000028   02  DFHPF8    PIC  X  VALUE  '8'.
000029   02  DFHPF9    PIC  X  VALUE  '9'.
000030   02  DFHPF10   PIC  X  VALUE  ':'.
000031   02  DFHPF11   PIC  X  VALUE  '#'.
000032   02  DFHPF12   PIC  X  VALUE  '@'.
000033   02  DFHPF13   PIC  X  VALUE  'A'.
000034   02  DFHPF14   PIC  X  VALUE  'B'.
000035   02  DFHPF15   PIC  X  VALUE  'C'.
000036   02  DFHPF16   PIC  X  VALUE  'D'.
000037   02  DFHPF17   PIC  X  VALUE  'E'.
000038   02  DFHPF18   PIC  X  VALUE  'F'.
000039   02  DFHPF19   PIC  X  VALUE  'G'.
000040   02  DFHPF20   PIC  X  VALUE  'H'.
000041   02  DFHPF21   PIC  X  VALUE  'I'.
000042*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
000043   02  DFHPF22   PIC  X  VALUE  '['.
000044   02  DFHPF23   PIC  X  VALUE  '.'.
000045   02  DFHPF24   PIC  X  VALUE  '<'.
000046   02  DFHMSRE   PIC  X  VALUE  'X'.
000047   02  DFHSTRF   PIC  X  VALUE  'h'.
000048   02  DFHTRIG   PIC  X  VALUE  '"'.
      *<<((file: ELCAID))
000219 01  FILLER    REDEFINES DFHAID.
000220     12  FILLER              PIC X(8).
000221     12  PF-VALUES           PIC X       OCCURS 24 TIMES.
000222     EJECT
000223*                            COPY ELCEMIB.
      *>>((file: ELCEMIB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEMIB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.005                          *
000007*                                                                *
000008*    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
000009*                                                                *
000010******************************************************************
000011 01  ERROR-MESSAGE-INTERFACE-BLOCK.
000012     12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
000013     12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
000014     12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
000015     12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
000016     12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
000017     12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
000018     12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
000019     12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
000020     12  EMI-SWITCH1             PIC X        VALUE '1'.
000021         88  EMI-NO-ERRORS                    VALUE '1'.
000022         88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
000023         88  EMI-ERRORS-COMPLETE              VALUE '3'.
000024     12  EMI-SWITCH2             PIC X        VALUE '1'.
000025         88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
000026     12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
000027         88  EMI-AREA1-EMPTY                  VALUE '1'.
000028         88  EMI-AREA1-FULL                   VALUE '2'.
000029     12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
000030         88  EMI-AREA2-EMPTY                  VALUE '1'.
000031         88  EMI-AREA2-FULL                   VALUE '2'.
000032     12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
000033         88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
000034         88  EMI-BYPASS-NOTES                 VALUE 'N'.
000035         88  EMI-BYPASS-WARNINGS              VALUE 'W'.
000036         88  EMI-BYPASS-FORCABLES             VALUE 'F'.
000037         88  EMI-BYPASS-FATALS                VALUE 'X'.
000038     12  EMI-ERROR-LINES.
000039         16  EMI-LINE1           PIC X(72)   VALUE SPACES.
000040         16  EMI-LINE2           PIC X(72)   VALUE SPACES.
000041         16  EMI-LINE3           PIC X(72)   VALUE SPACES.
000042         16  EMI-CODE-LINE REDEFINES EMI-LINE3.
000043             20  EMI-ERR-CODES OCCURS 10 TIMES.
000044                 24  EMI-ERR-NUM         PIC X(4).
000045                 24  EMI-FILLER          PIC X.
000046                 24  EMI-SEV             PIC X.
000047                 24  FILLER              PIC X.
000048             20  FILLER                  PIC X(02).
000049     12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
000050         16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
000051             20  EMI-ERROR-NUMBER    PIC X(4).
000052             20  EMI-FILL            PIC X.
000053             20  EMI-SEVERITY        PIC X.
000054             20  FILLER              PIC X.
000055             20  EMI-ERROR-TEXT.
000056                 24  EMI-TEXT-VARIABLE   PIC X(10).
000057                 24  FILLER          PIC X(55).
000058     12  EMI-SEVERITY-SAVE           PIC X.
000059         88  EMI-NOTE                    VALUE 'N'.
000060         88  EMI-WARNING                 VALUE 'W'.
000061         88  EMI-FORCABLE                VALUE 'F'.
000062         88  EMI-FATAL                   VALUE 'X'.
000063     12  EMI-MESSAGE-FLAG            PIC X.
000064         88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
000065         88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
000066     12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
000067     12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
000068         88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
000069         88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
000070         88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
000071     12  emi-claim-no                pic x(7).
000072     12  emi-claim-type              pic x(6).
000073     12  FILLER                      PIC X(124)  VALUE SPACES.
000074     12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
000075     12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
000076     12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
000077     12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *<<((file: ELCEMIB))
000224     EJECT
000225*                            COPY ELCINTF.
      *>>((file: ELCINTF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCINTF.                            *
000005*                            VMOD=2.017                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
000008*                                                                *
000009*       LENGTH = 1024                                            *
000010*                                                                *
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000020******************************************************************
000021 01  PROGRAM-INTERFACE-BLOCK.
000022     12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
000023     12  PI-CALLING-PROGRAM              PIC X(8).
000024     12  PI-SAVED-PROGRAM-1              PIC X(8).
000025     12  PI-SAVED-PROGRAM-2              PIC X(8).
000026     12  PI-SAVED-PROGRAM-3              PIC X(8).
000027     12  PI-SAVED-PROGRAM-4              PIC X(8).
000028     12  PI-SAVED-PROGRAM-5              PIC X(8).
000029     12  PI-SAVED-PROGRAM-6              PIC X(8).
000030     12  PI-RETURN-TO-PROGRAM            PIC X(8).
000031     12  PI-COMPANY-ID                   PIC XXX.
000032     12  PI-COMPANY-CD                   PIC X.
000033
000034     12  PI-COMPANY-PASSWORD             PIC X(8).
000035
000036     12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
000037
000038     12  PI-CONTROL-IN-PROGRESS.
000039         16  PI-CARRIER                  PIC X.
000040         16  PI-GROUPING                 PIC X(6).
000041         16  PI-STATE                    PIC XX.
000042         16  PI-ACCOUNT                  PIC X(10).
000043         16  PI-PRODUCER REDEFINES PI-ACCOUNT
000044                                         PIC X(10).
000045         16  PI-CLAIM-CERT-GRP.
000046             20  PI-CLAIM-NO             PIC X(7).
000047             20  PI-CERT-NO.
000048                 25  PI-CERT-PRIME       PIC X(10).
000049                 25  PI-CERT-SFX         PIC X.
000050             20  PI-CERT-EFF-DT          PIC XX.
000051         16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
000052             20  PI-PLAN-CODE            PIC X(2).
000053             20  PI-REVISION-NUMBER      PIC X(3).
000054             20  PI-PLAN-EFF-DT          PIC X(2).
000055             20  PI-PLAN-EXP-DT          PIC X(2).
000056             20  FILLER                  PIC X(11).
000057         16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
000058             20  PI-OE-REFERENCE-1.
000059                 25  PI-OE-REF-1-PRIME   PIC X(18).
000060                 25  PI-OE-REF-1-SUFF    PIC XX.
000061
000062     12  PI-SESSION-IN-PROGRESS          PIC X.
000063         88  CLAIM-SESSION                   VALUE '1'.
000064         88  CREDIT-SESSION                  VALUE '2'.
000065         88  WARRANTY-SESSION                VALUE '3'.
000066         88  MORTGAGE-SESSION                VALUE '4'.
000067         88  GENERAL-LEDGER-SESSION          VALUE '5'.
000068
000069
000070*THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
000071
000072     12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
000073     12  PI-ORIGINAL-COMPANY-CD          PIC X.
000074
000075     12  PI-CREDIT-USER                  PIC X.
000076         88  PI-NOT-CREDIT-USER              VALUE 'N'.
000077         88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
000078
000079     12  PI-CLAIM-USER                   PIC X.
000080         88  PI-NOT-CLAIM-USER               VALUE 'N'.
000081         88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
000082
000083     12  PI-PROCESSOR-SYS-ACCESS         PIC X.
000084         88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
000085         88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
000086         88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
000087         88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
000088         88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
000089
000090     12  PI-PROCESSOR-ID                 PIC X(4).
000091
000092     12  PI-PROCESSOR-PASSWORD           PIC X(11).
000093
000094     12  PI-MEMBER-CAPTION               PIC X(10).
000095
000096     12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
000097         88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
000098
000099     12  PI-LIFE-OVERRIDE-L1             PIC X.
000100     12  PI-LIFE-OVERRIDE-L2             PIC XX.
000101     12  PI-LIFE-OVERRIDE-L6             PIC X(6).
000102     12  PI-LIFE-OVERRIDE-L12            PIC X(12).
000103
000104     12  PI-AH-OVERRIDE-L1               PIC X.
000105     12  PI-AH-OVERRIDE-L2               PIC XX.
000106     12  PI-AH-OVERRIDE-L6               PIC X(6).
000107     12  PI-AH-OVERRIDE-L12              PIC X(12).
000108
000109     12  PI-NEW-SYSTEM                   PIC X(2).
000110
000111     12  PI-PRIMARY-CERT-NO              PIC X(11).
000112     12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
000113         88  PI-USES-PAID-TO                 VALUE '1'.
000114     12  PI-CRDTCRD-SYSTEM.
000115         16  PI-CRDTCRD-USER             PIC X.
000116             88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
000117             88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
000118         16  PI-CC-MONTH-END-DT          PIC XX.
000119     12  PI-PROCESSOR-PRINTER            PIC X(4).
000120
000121     12  PI-OE-REFERENCE-2.
000122         16  PI-OE-REF-2-PRIME           PIC X(10).
000123         16  PI-OE-REF-2-SUFF            PIC X.
000124
000125     12  PI-REM-TRM-CALC-OPTION          PIC X.
000126
000127     12  PI-LANGUAGE-TYPE                PIC X.
000128             88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
000129             88  PI-LANGUAGE-IS-FR           VALUE 'F'.
000130             88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
000131
000132     12  PI-POLICY-LINKAGE-IND           PIC X.
000133         88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
000134         88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
000135                                                   LOW-VALUES.
000136
000137     12  PI-ALT-DMD-PRT-ID               PIC X(4).
000138     12  PI-CLAIM-PW-SESSION             PIC X(1).
000139         88  PI-CLAIM-CREDIT                 VALUE '1'.
000140         88  PI-CLAIM-CONVEN                 VALUE '2'.
000141
000142     12  PI-PROCESSOR-CSR-IND            PIC X.
000143         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
000144         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
000145
000146     12  FILLER                          PIC X(3).
000147
000148     12  PI-SYSTEM-LEVEL                 PIC X(145).
000149
000150     12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
000151         PI-SYSTEM-LEVEL.
000152
000153         16  PI-ENTRY-CODES.
000154             20  PI-ENTRY-CD-1           PIC X.
000155             20  PI-ENTRY-CD-2           PIC X.
000156
000157         16  PI-RETURN-CODES.
000158             20  PI-RETURN-CD-1          PIC X.
000159             20  PI-RETURN-CD-2          PIC X.
000160
000161         16  PI-UPDATE-STATUS-SAVE.
000162             20  PI-UPDATE-BY            PIC X(4).
000163             20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
000164
000165         16  PI-LOWER-CASE-LETTERS       PIC X.
000166             88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
000167
000168*        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
000169*            88  CLAIM-NO-UNIQUE             VALUE '1'.
000170*            88  CARRIER-CLM-CNTL            VALUE '2'.
000171
000172         16  PI-CERT-ACCESS-CONTROL      PIC X.
000173             88  ST-ACCNT-CNTL               VALUE ' '.
000174             88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000175             88  CARR-ST-ACCNT-CNTL          VALUE '2'.
000176             88  ACCNT-CNTL                  VALUE '3'.
000177             88  CARR-ACCNT-CNTL             VALUE '4'.
000178
000179         16  PI-PROCESSOR-CAP-LIST.
000180             20  PI-SYSTEM-CONTROLS.
000181                24 PI-SYSTEM-DISPLAY     PIC X.
000182                 88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
000183                24 PI-SYSTEM-MODIFY      PIC X.
000184                 88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
000185             20  FILLER                  PIC XX.
000186             20  PI-DISPLAY-CAP          PIC X.
000187                 88  DISPLAY-CAP             VALUE 'Y'.
000188             20  PI-MODIFY-CAP           PIC X.
000189                 88  MODIFY-CAP              VALUE 'Y'.
000190             20  PI-MSG-AT-LOGON-CAP     PIC X.
000191                 88  MSG-AT-LOGON-CAP        VALUE 'Y'.
000192             20  PI-FORCE-CAP            PIC X.
000193                 88  FORCE-CAP               VALUE 'Y'.
000194
000195         16  PI-PROGRAM-CONTROLS.
000196             20  PI-PGM-PRINT-OPT        PIC X.
000197             20  PI-PGM-FORMAT-OPT       PIC X.
000198             20  PI-PGM-PROCESS-OPT      PIC X.
000199             20  PI-PGM-TOTALS-OPT       PIC X.
000200
000201         16  PI-HELP-INTERFACE.
000202             20  PI-LAST-ERROR-NO        PIC X(4).
000203             20  PI-CURRENT-SCREEN-NO    PIC X(4).
000204
000205         16  PI-CARRIER-CONTROL-LEVEL    PIC X.
000206             88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
000207
000208         16  PI-CR-CONTROL-IN-PROGRESS.
000209             20  PI-CR-CARRIER           PIC X.
000210             20  PI-CR-GROUPING          PIC X(6).
000211             20  PI-CR-STATE             PIC XX.
000212             20  PI-CR-ACCOUNT           PIC X(10).
000213             20  PI-CR-FIN-RESP          PIC X(10).
000214             20  PI-CR-TYPE              PIC X.
000215
000216         16  PI-CR-BATCH-NUMBER          PIC X(6).
000217
000218         16  PI-CR-MONTH-END-DT          PIC XX.
000219
000220         16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
000221             88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
000222             88  PI-ZERO-CARRIER             VALUE '1'.
000223             88  PI-ZERO-GROUPING            VALUE '2'.
000224             88  PI-ZERO-CAR-GROUP           VALUE '3'.
000225
000226         16  PI-CARRIER-SECURITY         PIC X.
000227             88  PI-NO-CARRIER-SECURITY      VALUE ' '.
000228
000229         16  PI-ACCOUNT-SECURITY         PIC X(10).
000230             88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
000231             88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
000232
000233         16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
000234             20  PI-ACCESS-CODE          OCCURS 10 TIMES
000235                                         INDEXED BY PI-ACCESS-NDX
000236                                         PIC X.
000237
000238         16  PI-GA-BILLING-CONTROL       PIC X.
000239             88  PI-GA-BILLING               VALUE '1'.
000240
000241         16  PI-MAIL-PROCESSING          PIC X.
000242             88  PI-MAIL-YES                 VALUE 'Y'.
000243
000244         16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
000245
000246         16  PI-AR-SYSTEM.
000247             20  PI-AR-PROCESSING-CNTL   PIC X.
000248                 88  PI-AR-PROCESSING        VALUE 'Y'.
000249             20  PI-AR-SUMMARY-CODE      PIC X(6).
000250             20  PI-AR-MONTH-END-DT      PIC XX.
000251
000252         16  PI-MP-SYSTEM.
000253             20  PI-MORTGAGE-USER            PIC X.
000254                 88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
000255                 88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
000256             20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
000257                 88  PI-MP-ST-PROD-CNTL              VALUE ' '.
000258                 88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
000259                 88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
000260                 88  PI-MP-PROD-CNTL                 VALUE '3'.
000261                 88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
000262             20  PI-MP-MONTH-END-DT          PIC XX.
000263             20  PI-MP-REFERENCE-NO.
000264                 24  PI-MP-REFERENCE-PRIME   PIC X(18).
000265                 24  PI-MP-REFERENCE-SFX     PIC XX.
000266
000267         16  PI-LABEL-CONTROL            PIC X(01).
000268             88  PI-CREATE-LABELS                    VALUE 'Y'.
000269             88  PI-BYPASS-LABELS                    VALUE 'N'.
000270
000271         16  PI-BILL-GROUPING-CODE       PIC X(01).
000272             88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
000273
000274         16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
000275             88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
000276             88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
000277
000278         16  FILLER                      PIC X(14).
000279
000280     12  PI-PROGRAM-WORK-AREA            PIC X(640).
000281******************************************************************
      *<<((file: ELCINTF))
000226     EJECT
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
000228 01  DFHCOMMAREA         PIC X(1024).
000229*01 PARMLIST   COMP.
000230*    02  FILLER          PIC S9(8).
000231*    02  ELCNTL-POINTER  PIC S9(8).
000232
000233*                             COPY ELCCNTL.
      *>>((file: ELCCNTL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCNTL.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.059                          *
000007*                                                                *
000008*   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 750  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 082503                   PEMA  ADD BENEFIT GROUP
000027* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000028* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000029* 092705    2005050300006  PEMA  ADD SPP LEASES
000030* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
000031* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
000032* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
000033* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
000034* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
000035* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000036* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
000037* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
000038* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000039* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000040* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
000041******************************************************************
000042*
000043 01  CONTROL-FILE.
000044     12  CF-RECORD-ID                       PIC XX.
000045         88  VALID-CF-ID                        VALUE 'CF'.
000046
000047     12  CF-CONTROL-PRIMARY.
000048         16  CF-COMPANY-ID                  PIC XXX.
000049         16  CF-RECORD-TYPE                 PIC X.
000050             88  CF-COMPANY-MASTER              VALUE '1'.
000051             88  CF-PROCESSOR-MASTER            VALUE '2'.
000052             88  CF-STATE-MASTER                VALUE '3'.
000053             88  CF-LF-BENEFIT-MASTER           VALUE '4'.
000054             88  CF-AH-BENEFIT-MASTER           VALUE '5'.
000055             88  CF-CARRIER-MASTER              VALUE '6'.
000056             88  CF-MORTALITY-MASTER            VALUE '7'.
000057             88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
000058             88  CF-TERMINAL-MASTER             VALUE '9'.
000059             88  CF-AH-EDIT-MASTER              VALUE 'A'.
000060             88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
000061             88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
000062             88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
000063             88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
000064             88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
000065             88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
000066             88  CF-REMINDERS-MASTER            VALUE 'R'.
000067             88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
000068         16  CF-ACCESS-CD-GENL              PIC X(4).
000069         16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
000070             20  CF-PROCESSOR               PIC X(4).
000071         16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
000072             20  CF-STATE-CODE              PIC XX.
000073             20  FILLER                     PIC XX.
000074         16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
000075             20  FILLER                     PIC XX.
000076             20  CF-HI-BEN-IN-REC           PIC XX.
000077         16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
000078             20  FILLER                     PIC XXX.
000079             20  CF-CARRIER-CNTL            PIC X.
000080         16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
000081             20  FILLER                     PIC XX.
000082             20  CF-HI-TYPE-IN-REC          PIC 99.
000083         16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
000084             20  CF-CRDB-TABLE-INDICATOR    PIC X.
000085                 88  CF-CRDB-NAIC-TABLE         VALUE '9'.
000086             20  CF-CRDB-BENEFIT-TYPE       PIC X.
000087             20  CF-CRDB-WAITING-PERIOD     PIC XX.
000088         16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
000089             20  FILLER                     PIC X.
000090             20  CF-CUSTOM-REPORT-NO        PIC 999.
000091         16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
000092             20  FILLER                     PIC XX.
000093             20  CF-MORTGAGE-PLAN           PIC XX.
000094         16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
000095
000096     12  CF-LAST-MAINT-DT                   PIC XX.
000097     12  CF-LAST-MAINT-BY                   PIC X(4).
000098     12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
000099
000100     12  CF-RECORD-BODY                     PIC X(728).
000101
000102
000103****************************************************************
000104*             COMPANY MASTER RECORD                            *
000105****************************************************************
000106
000107     12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000108         16  CF-COMPANY-ADDRESS.
000109             20  CF-CL-MAIL-TO-NAME         PIC X(30).
000110             20  CF-CL-IN-CARE-OF           PIC X(30).
000111             20  CF-CL-ADDR-LINE-1          PIC X(30).
000112             20  CF-CL-ADDR-LINE-2          PIC X(30).
000113             20  CF-CL-CITY-STATE           PIC X(30).
000114             20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
000115             20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
000116         16  CF-COMPANY-CD                  PIC X.
000117         16  CF-COMPANY-PASSWORD            PIC X(8).
000118         16  CF-SECURITY-OPTION             PIC X.
000119             88  ALL-SECURITY                   VALUE '1'.
000120             88  COMPANY-VERIFY                 VALUE '2'.
000121             88  PROCESSOR-VERIFY               VALUE '3'.
000122             88  NO-SECURITY                    VALUE '4'.
000123             88  ALL-BUT-TERM                   VALUE '5'.
000124         16  CF-CARRIER-CONTROL-LEVEL       PIC X.
000125             88  USE-ACTUAL-CARRIER             VALUE SPACE.
000126         16  CF-LGX-INTERFACE-CNTL          PIC X.
000127             88  LGX-TIME-SHR-COMPANY           VALUE '1'.
000128         16  CF-INFORCE-LOCATION            PIC X.
000129             88  CERTS-ARE-ONLINE               VALUE '1'.
000130             88  CERTS-ARE-OFFLINE              VALUE '2'.
000131             88  NO-CERTS-AVAILABLE             VALUE '3'.
000132         16  CF-LOWER-CASE-LETTERS          PIC X.
000133         16  CF-CERT-ACCESS-CONTROL         PIC X.
000134             88  CF-ST-ACCNT-CNTL               VALUE ' '.
000135             88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000136             88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
000137             88  CF-ACCNT-CNTL                  VALUE '3'.
000138             88  CF-CARR-ACCNT-CNTL             VALUE '4'.
000139
000140         16  CF-FORMS-PRINTER-ID            PIC X(4).
000141         16  CF-CHECK-PRINTER-ID            PIC X(4).
000142
000143         16  CF-LGX-CREDIT-USER             PIC X.
000144             88  CO-IS-NOT-USER                 VALUE 'N'.
000145             88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
000146
000147         16 CF-CREDIT-CALC-CODES.
000148             20  CF-CR-REM-TERM-CALC PIC X.
000149               88  CR-EARN-AFTER-15TH           VALUE '1'.
000150               88  CR-EARN-ON-HALF-MO           VALUE '2'.
000151               88  CR-EARN-ON-1ST-DAY           VALUE '3'.
000152               88  CR-EARN-ON-FULL-MO           VALUE '4'.
000153               88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
000154               88  CR-EARN-AFTER-14TH           VALUE '6'.
000155               88  CR-EARN-AFTER-16TH           VALUE '7'.
000156             20  CF-CR-R78-METHOD           PIC X.
000157               88  USE-TERM-PLUS-ONE            VALUE SPACE.
000158               88  DONT-USE-PLUS-ONE            VALUE '1'.
000159
000160         16  CF-CLAIM-CONTROL-COUNTS.
000161             20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
000162                 88  CO-CLM-COUNT-RESET         VALUE +99999.
000163
000164             20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
000165                 88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
000166
000167             20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
000168                 88  CO-CHECK-COUNT-RESET       VALUE +9999999.
000169
000170             20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
000171                 88  CO-QUE-COUNT-RESET         VALUE +9999999.
000172
000173         16  CF-CURRENT-MONTH-END           PIC XX.
000174
000175         16  CF-CO-CALC-QUOTE-TOLERANCE.
000176             20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
000177             20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
000178             20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
000179             20  CF-CO-CLAIM-REJECT-SW      PIC X.
000180                 88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
000181                 88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
000182             20  CF-CO-PREM-REJECT-SW       PIC X.
000183                 88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
000184                 88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
000185             20  CF-CO-REF-REJECT-SW        PIC X.
000186                 88 CO-WARN-IF-REF-OUT          VALUE SPACE.
000187                 88 CO-FORCE-IF-REF-OUT         VALUE '1'.
000188
000189         16  CF-CO-REPORTING-DT             PIC XX.
000190         16  CF-CO-REPORTING-MONTH-DT       PIC XX.
000191         16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
000192           88  CF-CO-NOT-MONTH-END              VALUE SPACES.
000193           88  CF-CO-MONTH-END                  VALUE '1'.
000194
000195         16  CF-LGX-CLAIM-USER              PIC X.
000196             88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
000197             88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
000198
000199         16  CF-CREDIT-EDIT-CONTROLS.
000200             20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
000201             20  CF-MIN-AGE                 PIC 99.
000202             20  CF-DEFAULT-AGE             PIC 99.
000203             20  CF-MIN-TERM                PIC S999      COMP-3.
000204             20  CF-MAX-TERM                PIC S999      COMP-3.
000205             20  CF-DEFAULT-SEX             PIC X.
000206             20  CF-JOINT-AGE-INPUT         PIC X.
000207                 88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
000208             20  CF-BIRTH-DATE-INPUT        PIC X.
000209                 88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
000210             20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
000211                 88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
000212                 88  CF-ZERO-CARRIER            VALUE '1'.
000213                 88  CF-ZERO-GROUPING           VALUE '2'.
000214                 88  CF-ZERO-CAR-GROUP          VALUE '3'.
000215             20  CF-EDIT-SW                 PIC X.
000216                 88  CF-START-EDIT-TONIGHT      VALUE '1'.
000217             20  CF-EDIT-RESTART-BATCH      PIC X(6).
000218             20  CF-CR-PR-METHOD            PIC X.
000219               88  USE-NORMAL-PR-METHOD         VALUE SPACE.
000220               88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
000221             20  FILLER                     PIC X.
000222
000223         16  CF-CREDIT-MISC-CONTROLS.
000224             20  CF-REIN-TABLE-SW           PIC X.
000225                 88 REIN-TABLES-ARE-USED        VALUE '1'.
000226             20  CF-COMP-TABLE-SW           PIC X.
000227                 88 COMP-TABLES-ARE-USED        VALUE '1'.
000228             20  CF-EXPERIENCE-RETENTION-AGE
000229                                            PIC S9        COMP-3.
000230             20  CF-CONVERSION-DT           PIC XX.
000231             20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
000232             20  CF-RUN-FREQUENCY-SW        PIC X.
000233                 88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
000234                 88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
000235
000236             20  CF-CR-CHECK-NO-CONTROL.
000237                 24  CF-CR-CHECK-NO-METHOD    PIC X.
000238                     88  CR-CHECK-NO-MANUAL       VALUE '1'.
000239                     88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
000240                     88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
000241                 24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
000242                     88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
000243
000244                 24  CF-CR-CHECK-COUNT       REDEFINES
000245                     CF-CR-CHECK-COUNTER      PIC X(4).
000246
000247                 24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
000248                     88  CR-QUE-COUNT-RESET      VALUE +9999999.
000249
000250                 24  CF-CR-CHECK-QUE-COUNT   REDEFINES
000251                     CF-CR-CHECK-QUE-COUNTER  PIC X(4).
000252                 24  CF-MAIL-PROCESSING       PIC X.
000253                     88  MAIL-PROCESSING          VALUE 'Y'.
000254
000255         16  CF-MISC-SYSTEM-CONTROL.
000256             20  CF-SYSTEM-C                 PIC X.
000257                 88  CONFIRMATION-SYS-USED       VALUE '1'.
000258             20  CF-SYSTEM-D                 PIC X.
000259                 88  DAILY-BILL-SYS-USED         VALUE '1'.
000260             20  CF-SOC-SEC-NO-SW            PIC X.
000261                 88  SOC-SEC-NO-USED             VALUE '1'.
000262             20  CF-MEMBER-NO-SW             PIC X.
000263                 88  MEMBER-NO-USED              VALUE '1'.
000264             20  CF-TAX-ID-NUMBER            PIC X(11).
000265             20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
000266             20  CF-PAYMENT-APPROVAL-SW      PIC X.
000267                 88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
000268                 88  CF-NO-APPROVAL              VALUE ' ' 'N'.
000269                 88  CF-ALL-APPROVED             VALUE 'Y'.
000270                 88  CF-GRADUATED-APPROVAL       VALUE 'G'.
000271             20  CF-SYSTEM-E                 PIC X.
000272                 88  CF-AR-SYSTEM-USED           VALUE 'Y'.
000273
000274         16  CF-LGX-LIFE-USER               PIC X.
000275             88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
000276             88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
000277
000278         16  CF-CR-MONTH-END-DT             PIC XX.
000279
000280         16  CF-FILE-MAINT-DATES.
000281             20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
000282                 88  CF-LAST-BATCH-RESET        VALUE +999999.
000283             20  CF-LAST-BATCH       REDEFINES
000284                 CF-LAST-BATCH-NO               PIC X(4).
000285             20  CF-RATES-FILE-MAINT-DT         PIC XX.
000286             20  CF-RATES-FILE-CREATE-DT        PIC XX.
000287             20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
000288             20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
000289             20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
000290             20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
000291             20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
000292             20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
000293             20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
000294             20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
000295
000296         16  CF-NEXT-COMPANY-ID             PIC XXX.
000297         16  FILLER                         PIC X.
000298
000299         16  CF-ALT-MORT-CODE               PIC X(4).
000300         16  CF-MEMBER-CAPTION              PIC X(10).
000301
000302         16  CF-LIFE-ACCESS-CONTROL         PIC X.
000303             88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
000304             88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
000305             88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
000306             88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
000307             88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
000308
000309         16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
000310
000311         16  CF-LIFE-OVERRIDE-L1            PIC X.
000312         16  CF-LIFE-OVERRIDE-L2            PIC XX.
000313         16  CF-LIFE-OVERRIDE-L6            PIC X(6).
000314         16  CF-LIFE-OVERRIDE-L12           PIC X(12).
000315
000316         16  CF-AH-OVERRIDE-L1              PIC X.
000317         16  CF-AH-OVERRIDE-L2              PIC XX.
000318         16  CF-AH-OVERRIDE-L6              PIC X(6).
000319         16  CF-AH-OVERRIDE-L12             PIC X(12).
000320
000321         16  CF-REPORT-CD1-CAPTION          PIC X(10).
000322         16  CF-REPORT-CD2-CAPTION          PIC X(10).
000323
000324         16  CF-CLAIM-CUTOFF-DATE           PIC XX.
000325         16  CF-AR-LAST-EL860-DT            PIC XX.
000326         16  CF-MP-MONTH-END-DT             PIC XX.
000327
000328         16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
000329         16  CF-CLAIM-PAID-THRU-TO          PIC X.
000330             88  CF-CLAIM-PAID-TO               VALUE '1'.
000331
000332         16  CF-AR-MONTH-END-DT             PIC XX.
000333
000334         16  CF-CRDTCRD-USER                PIC X.
000335             88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
000336             88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
000337
000338         16  CF-CC-MONTH-END-DT             PIC XX.
000339
000340         16  CF-PRINT-ADDRESS-LABELS        PIC X.
000341
000342         16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
000343             88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
000344             88  CF-USE-ALL-AGE-LAST            VALUE '2'.
000345             88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
000346         16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
000347         16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
000348         16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
000349         16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
000350             88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
000351             88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
000352         16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
000353         16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
000354         16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
000355             88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
000356         16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
000357
000358         16  CF-CL-ZIP-CODE.
000359             20  CF-CL-ZIP-PRIME.
000360                 24  CF-CL-ZIP-1ST          PIC X.
000361                     88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
000362                 24  FILLER                 PIC X(4).
000363             20  CF-CL-ZIP-PLUS4            PIC X(4).
000364         16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
000365             20  CF-CL-CAN-POSTAL-1         PIC XXX.
000366             20  CF-CL-CAN-POSTAL-2         PIC XXX.
000367             20  FILLER                     PIC XXX.
000368
000369         16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
000370         16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
000371         16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
000372         16  CF-CO-OPTION-START-DATE        PIC XX.
000373         16  CF-REM-TRM-CALC-OPTION         PIC X.
000374           88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
000375                                                      '3' '4'.
000376           88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
000377           88  CF-30-DAY-MONTH                  VALUE '1' '3'.
000378           88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
000379           88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
000380           88  CF-EXT-30-DAY-MONTH              VALUE '3'.
000381           88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
000382
000383         16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
000384
000385         16  CF-PAYMENT-APPROVAL-LEVELS.
000386             20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
000387             20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
000388             20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
000389             20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
000390             20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
000391             20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
000392
000393         16  CF-END-USER-REPORTING-USER     PIC X.
000394             88  CO-NO-END-USER-REPORTING       VALUE 'N'.
000395             88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
000396
000397         16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
000398             88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
000399             88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
000400
000401         16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
000402
000403         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
000404         16  FILLER                         PIC X.
000405
000406         16  CF-CREDIT-ARCHIVE-CNTL.
000407             20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
000408             20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
000409             20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
000410
000411         16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
000412
000413         16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
000414             88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
000415             88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
000416
000417         16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
000418             88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
000419             88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
000420
000421         16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
000422
000423         16  CF-CO-ACH-ID-CODE              PIC  X.
000424             88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
000425             88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
000426             88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
000427         16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
000428         16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
000429         16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
000430         16  CF-CO-ACH-ADMIN-NO             PIC X(09).
000431         16  CF-CO-ACH-RECV-NAME            PIC X(23).
000432         16  CF-CO-ACH-RECV-NO              PIC X(08).
000433         16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
000434         16  CF-CO-ACH-COMPANY-ID           PIC X(09).
000435         16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
000436                 88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
000437         16  CF-CO-ACH-TRACE-SPACE REDEFINES
000438                 CF-CO-ACH-TRACE-NO         PIC X(4).
000439
000440         16  CF-CO-OVER-SHORT.
000441             20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
000442             20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000443
000444*         16  FILLER                         PIC X(102).
000445         16  CF-PAYMENT-APPROVAL-LEVELS-2.
000446             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
000447             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
000448
000449         16  CF-AH-APPROVAL-DAYS.
000450             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
000451             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
000452             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
000453             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
000454
000455         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
000456
000457         16  CF-APPROV-LEV-5.
000458             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
000459             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
000460             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
000461
000462         16  FILLER                         PIC X(68).
000463****************************************************************
000464*             PROCESSOR/USER RECORD                            *
000465****************************************************************
000466
000467     12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000468         16  CF-PROCESSOR-NAME              PIC X(30).
000469         16  CF-PROCESSOR-PASSWORD          PIC X(11).
000470         16  CF-PROCESSOR-TITLE             PIC X(26).
000471         16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
000472                 88  MESSAGE-YES                VALUE 'Y'.
000473                 88  MESSAGE-NO                 VALUE ' ' 'N'.
000474
000475*****************************************************
000476****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
000477****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
000478****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
000479****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
000480*****************************************************
000481
000482         16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
000483             20  CF-ADMINISTRATION-CONTROLS PIC XX.
000484             20  CF-APPLICATION-FORCE       PIC X.
000485             20  CF-INDIVIDUAL-APP.
000486                 24  CF-APP-SWITCHES  OCCURS  44 TIMES.
000487                     28  CF-BROWSE-APP      PIC X.
000488                     28  CF-UPDATE-APP      PIC X.
000489
000490         16  CF-CURRENT-TERM-ON             PIC X(4).
000491         16  CF-PROCESSOR-LIMITS-CLAIMS.
000492             20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
000493             20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
000494             20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
000495             20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
000496             20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
000497             20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
000498             20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
000499         16  CF-PROCESSOR-CARRIER           PIC X.
000500             88  NO-CARRIER-SECURITY            VALUE ' '.
000501         16  CF-PROCESSOR-ACCOUNT           PIC X(10).
000502             88  NO-ACCOUNT-SECURITY            VALUE SPACES.
000503         16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
000504             88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
000505         16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
000506             88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
000507
000508         16  CF-PROC-SYS-ACCESS-SW.
000509             20  CF-PROC-CREDIT-CLAIMS-SW.
000510                 24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
000511                     88  ACCESS-TO-CREDIT           VALUE 'Y'.
000512                 24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
000513                     88  ACCESS-TO-CLAIMS           VALUE 'Y'.
000514             20  CF-PROC-CREDIT-CLAIMS   REDEFINES
000515                 CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
000516                 88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
000517             20  CF-PROC-LIFE-GNRLDGR-SW.
000518                 24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
000519                     88  ACCESS-TO-LIFE             VALUE 'Y'.
000520                 24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
000521                     88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
000522             20  CF-PROC-LIFE-GNRLDGR    REDEFINES
000523                 CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
000524                 88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
000525         16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
000526             CF-PROC-SYS-ACCESS-SW              PIC X(4).
000527             88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
000528         16  CF-PROCESSOR-PRINTER               PIC X(4).
000529
000530         16  CF-APPROVAL-LEVEL                  PIC X.
000531             88  APPROVAL-LEVEL-1                   VALUE '1'.
000532             88  APPROVAL-LEVEL-2                   VALUE '2'.
000533             88  APPROVAL-LEVEL-3                   VALUE '3'.
000534             88  APPROVAL-LEVEL-4                   VALUE '4'.
000535             88  APPROVAL-LEVEL-5                   VALUE '5'.
000536
000537         16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
000538
000539         16  CF-LANGUAGE-TYPE                   PIC X.
000540             88  CF-LANG-IS-ENG                     VALUE 'E'.
000541             88  CF-LANG-IS-FR                      VALUE 'F'.
000542
000543         16  CF-CSR-IND                         PIC X.
000544         16  FILLER                             PIC X(239).
000545
000546****************************************************************
000547*             PROCESSOR/REMINDERS RECORD                       *
000548****************************************************************
000549
000550     12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
000551         16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
000552             20  CF-START-REMIND-DT         PIC XX.
000553             20  CF-END-REMIND-DT           PIC XX.
000554             20  CF-REMINDER-TEXT           PIC X(50).
000555         16  FILLER                         PIC X(296).
000556
000557
000558****************************************************************
000559*             STATE MASTER RECORD                              *
000560****************************************************************
000561
000562     12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000563         16  CF-STATE-ABBREVIATION          PIC XX.
000564         16  CF-STATE-NAME                  PIC X(25).
000565         16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
000566         16  CF-ST-CALC-QUOTE-TOLERANCE.
000567             20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
000568             20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
000569             20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
000570             20  CF-ST-CLAIM-REJECT-SW      PIC X.
000571                 88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
000572                 88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
000573             20  CF-ST-PREM-REJECT-SW       PIC X.
000574                 88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
000575                 88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
000576             20  CF-ST-REF-REJECT-SW        PIC X.
000577                 88 ST-WARN-IF-REF-OUT          VALUE SPACE.
000578                 88 ST-FORCE-IF-REF-OUT         VALUE '1'.
000579         16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
000580         16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
000581         16  CF-ST-REFUND-RULES.
000582             20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
000583             20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
000584             20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
000585         16  CF-ST-FST-PMT-EXTENSION.
000586             20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
000587             20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
000588                 88  CF-ST-EXT-NO-CHG           VALUE ' '.
000589                 88  CF-ST-EXT-CHG-LF           VALUE '1'.
000590                 88  CF-ST-EXT-CHG-AH           VALUE '2'.
000591                 88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
000592         16  CF-ST-STATE-CALL.
000593             20  CF-ST-CALL-UNEARNED        PIC X.
000594             20  CF-ST-CALL-RPT-CNTL        PIC X.
000595             20  CF-ST-CALL-RATE-DEV        PIC XXX.
000596         16  CF-REPLACEMENT-LAW-SW          PIC X.
000597             88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
000598             88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
000599         16  CF-REPLACEMENT-LETTER          PIC X(4).
000600         16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
000601         16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
000602         16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
000603         16  CF-ST-SPLIT-PAYMENT            PIC X.
000604         16  FILLER                         PIC X.
000605         16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
000606             20  CF-ST-BENEFIT-CD           PIC XX.
000607             20  CF-ST-BENEFIT-KIND         PIC X.
000608                 88  CF-ST-LIFE-KIND            VALUE 'L'.
000609                 88  CF-ST-AH-KIND              VALUE 'A'.
000610             20  CF-ST-REM-TERM-CALC        PIC X.
000611                 88  ST-REM-TERM-NOT-USED       VALUE SPACE.
000612                 88  ST-EARN-AFTER-15TH         VALUE '1'.
000613                 88  ST-EARN-ON-HALF-MO         VALUE '2'.
000614                 88  ST-EARN-ON-1ST-DAY         VALUE '3'.
000615                 88  ST-EARN-ON-FULL-MO         VALUE '4'.
000616                 88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
000617                 88  ST-EARN-AFTER-14TH         VALUE '6'.
000618                 88  ST-EARN-AFTER-16TH         VALUE '7'.
000619
000620             20  CF-ST-REFUND-CALC          PIC X.
000621                 88  ST-REFUND-NOT-USED         VALUE SPACE.
000622                 88  ST-REFD-BY-R78             VALUE '1'.
000623                 88  ST-REFD-BY-PRO-RATA        VALUE '2'.
000624                 88  ST-REFD-AS-CALIF           VALUE '3'.
000625                 88  ST-REFD-AS-TEXAS           VALUE '4'.
000626                 88  ST-REFD-IS-NET-PAY         VALUE '5'.
000627                 88  ST-REFD-ANTICIPATION       VALUE '6'.
000628                 88  ST-REFD-UTAH               VALUE '7'.
000629                 88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
000630                 88  ST-REFD-REG-BALLOON        VALUE 'B'.
000631                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
000632
000633             20  CF-ST-EARNING-CALC         PIC X.
000634                 88  ST-EARNING-NOT-USED        VALUE SPACE.
000635                 88  ST-EARN-BY-R78             VALUE '1'.
000636                 88  ST-EARN-BY-PRO-RATA        VALUE '2'.
000637                 88  ST-EARN-AS-CALIF           VALUE '3'.
000638                 88  ST-EARN-AS-TEXAS           VALUE '4'.
000639                 88  ST-EARN-IS-NET-PAY         VALUE '5'.
000640                 88  ST-EARN-ANTICIPATION       VALUE '6'.
000641                 88  ST-EARN-MEAN               VALUE '8'.
000642                 88  ST-EARN-REG-BALLOON        VALUE 'B'.
000643
000644             20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
000645                 88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
000646                 88  ST-OVRD-BY-R78             VALUE '1'.
000647                 88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
000648                 88  ST-OVRD-AS-CALIF           VALUE '3'.
000649                 88  ST-OVRD-AS-TEXAS           VALUE '4'.
000650                 88  ST-OVRD-IS-NET-PAY         VALUE '5'.
000651                 88  ST-OVRD-ANTICIPATION       VALUE '6'.
000652                 88  ST-OVRD-MEAN               VALUE '8'.
000653                 88  ST-OVRD-REG-BALLOON        VALUE 'B'.
000654             20  cf-st-extra-periods        pic 9.
000655*            20  FILLER                     PIC X.
000656
000657         16  CF-ST-COMMISSION-CAPS.
000658             20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
000659             20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
000660             20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
000661             20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
000662         16  CF-COMM-CAP-LIMIT-TO           PIC X.
000663                 88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
000664                 88  ST-LIMIT-TO-GA             VALUE 'G'.
000665                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
000666
000667         16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
000668
000669         16  CF-ST-STATUTORY-INTEREST.
000670             20  CF-ST-STAT-DATE-FROM       PIC X.
000671                 88  ST-STAT-FROM-INCURRED      VALUE 'I'.
000672                 88  ST-STAT-FROM-REPORTED      VALUE 'R'.
000673             20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
000674             20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
000675             20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
000676             20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
000677             20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
000678
000679         16  CF-ST-OVER-SHORT.
000680             20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
000681             20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
000682
000683         16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
000684
000685         16  CF-ST-RT-CALC                  PIC X.
000686
000687         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
000688         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
000689         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
000690         16  CF-ST-RF-LR-CALC               PIC X.
000691         16  CF-ST-RF-LL-CALC               PIC X.
000692         16  CF-ST-RF-LN-CALC               PIC X.
000693         16  CF-ST-RF-AH-CALC               PIC X.
000694         16  CF-ST-RF-CP-CALC               PIC X.
000695*        16  FILLER                         PIC X(206).
000696*CIDMOD         16  FILLER                         PIC X(192).
000697         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
000698             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
000699         16  CF-ST-REF-AH-DEATH-IND         PIC X.
000700         16  CF-ST-VFY-2ND-BENE             PIC X.
000701         16  CF-ST-CAUSAL-STATE             PIC X.
000702         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
000703         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
000704         16  CF-ST-AGENT-SIG-EDIT           PIC X.
000705             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
000706         16  CF-ST-NET-ONLY-STATE           PIC X.
000707             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
000708         16  cf-commission-cap-required     pic x.
000709         16  CF-ST-GA-COMMISSION-CAPS.
000710             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
000711             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
000712             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
000713             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
000714         16  CF-ST-TOT-COMMISSION-CAPS.
000715             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
000716             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
000717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
000718             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
000719         16  FILLER                         PIC X(156).
000720
000721****************************************************************
000722*             BENEFIT MASTER RECORD                            *
000723****************************************************************
000724
000725     12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000726         16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
000727             20  CF-BENEFIT-CODE            PIC XX.
000728             20  CF-BENEFIT-NUMERIC  REDEFINES
000729                 CF-BENEFIT-CODE            PIC XX.
000730             20  CF-BENEFIT-ALPHA           PIC XXX.
000731             20  CF-BENEFIT-DESCRIP         PIC X(10).
000732             20  CF-BENEFIT-COMMENT         PIC X(10).
000733
000734             20  CF-LF-COVERAGE-TYPE        PIC X.
000735                 88  CF-REDUCING                VALUE 'R'.
000736                 88  CF-LEVEL                   VALUE 'L' 'P'.
000737
000738             20  CF-SPECIAL-CALC-CD         PIC X.
000739                 88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
000740                 88  CF-NP-0-MO-INT             VALUE 'A'.
000741                 88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
000742                 88  CF-CRITICAL-PERIOD         VALUE 'C'.
000743                 88  CF-TERM-IN-DAYS            VALUE 'D'.
000744                 88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
000745                 88  CF-FARM-PLAN               VALUE 'F'.
000746                 88  CF-RATE-AS-STANDARD        VALUE 'G'.
000747                 88  CF-2-MTH-INTEREST          VALUE 'I'.
000748                 88  CF-3-MTH-INTEREST          VALUE 'J'.
000749                 88  CF-4-MTH-INTEREST          VALUE 'K'.
000750                 88  CF-BALLOON-LAST-PMT        VALUE 'L'.
000751                 88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
000752                 88  CF-PRUDENTIAL              VALUE 'P'.
000753                 88  CF-OUTSTANDING-BAL         VALUE 'O'.
000754                 88  CF-TRUNCATED-LIFE          VALUE 'T'.
000755                 88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
000756                 88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
000757                 88  CF-NET-PAY-SIMPLE          VALUE 'S'.
000758                 88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
000759
000760             20  CF-JOINT-INDICATOR         PIC X.
000761                 88  CF-JOINT-COVERAGE          VALUE 'J'.
000762
000763*            20  FILLER                     PIC X(12).
000764             20  cf-maximum-benefits        pic s999 comp-3.
000765             20  FILLER                     PIC X(09).
000766             20  CF-BENEFIT-CATEGORY        PIC X.
000767             20  CF-LOAN-TYPE               PIC X(8).
000768
000769             20  CF-CO-REM-TERM-CALC        PIC X.
000770                 88  CO-EARN-AFTER-15TH         VALUE '1'.
000771                 88  CO-EARN-ON-HALF-MO         VALUE '2'.
000772                 88  CO-EARN-ON-1ST-DAY         VALUE '3'.
000773                 88  CO-EARN-ON-FULL-MO         VALUE '4'.
000774                 88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
000775
000776             20  CF-CO-EARNINGS-CALC        PIC X.
000777                 88  CO-EARN-BY-R78             VALUE '1'.
000778                 88  CO-EARN-BY-PRO-RATA        VALUE '2'.
000779                 88  CO-EARN-AS-CALIF           VALUE '3'.
000780                 88  CO-EARN-AS-TEXAS           VALUE '4'.
000781                 88  CO-EARN-IS-NET-PAY         VALUE '5'.
000782                 88  CO-EARN-ANTICIPATION       VALUE '6'.
000783                 88  CO-EARN-AS-MEAN            VALUE '8'.
000784                 88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
000785
000786             20  CF-CO-REFUND-CALC          PIC X.
000787                 88  CO-REFUND-NOT-USED         VALUE SPACE.
000788                 88  CO-REFD-BY-R78             VALUE '1'.
000789                 88  CO-REFD-BY-PRO-RATA        VALUE '2'.
000790                 88  CO-REFD-AS-CALIF           VALUE '3'.
000791                 88  CO-REFD-AS-TEXAS           VALUE '4'.
000792                 88  CO-REFD-IS-NET-PAY         VALUE '5'.
000793                 88  CO-REFD-ANTICIPATION       VALUE '6'.
000794                 88  CO-REFD-MEAN               VALUE '8'.
000795                 88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
000796                 88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
000797                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
000798
000799             20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
000800                 88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
000801                 88  CO-OVRD-BY-R78             VALUE '1'.
000802                 88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
000803                 88  CO-OVRD-AS-CALIF           VALUE '3'.
000804                 88  CO-OVRD-AS-TEXAS           VALUE '4'.
000805                 88  CO-OVRD-IS-NET-PAY         VALUE '5'.
000806                 88  CO-OVRD-ANTICIPATION       VALUE '6'.
000807                 88  CO-OVRD-MEAN               VALUE '8'.
000808                 88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
000809
000810             20  CF-CO-BEN-I-G-CD           PIC X.
000811                 88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
000812                 88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
000813                 88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
000814
000815         16  FILLER                         PIC X(304).
000816
000817
000818****************************************************************
000819*             CARRIER MASTER RECORD                            *
000820****************************************************************
000821
000822     12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000823         16  CF-ADDRESS-DATA.
000824             20  CF-MAIL-TO-NAME            PIC X(30).
000825             20  CF-IN-CARE-OF              PIC X(30).
000826             20  CF-ADDRESS-LINE-1          PIC X(30).
000827             20  CF-ADDRESS-LINE-2          PIC X(30).
000828             20  CF-CITY-STATE              PIC X(30).
000829             20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
000830             20  CF-PHONE-NO                PIC 9(11)     COMP-3.
000831
000832         16  CF-CLAIM-NO-CONTROL.
000833             20  CF-CLAIM-NO-METHOD         PIC X.
000834                 88  CLAIM-NO-MANUAL            VALUE '1'.
000835                 88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
000836                 88  CLAIM-NO-SEQ               VALUE '3'.
000837                 88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
000838             20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
000839                 88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
000840                 88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
000841                 88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
000842
000843         16  CF-CHECK-NO-CONTROL.
000844             20  CF-CHECK-NO-METHOD         PIC X.
000845                 88  CHECK-NO-MANUAL            VALUE '1'.
000846                 88  CHECK-NO-AUTO-SEQ          VALUE '2'.
000847                 88  CHECK-NO-CARR-SEQ          VALUE '3'.
000848                 88  CHECK-NO-AT-PRINT          VALUE '4'.
000849             20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
000850                 88  CHECK-CNT-RESET-VALUE      VALUE +999999.
000851
000852         16  CF-DOMICILE-STATE              PIC XX.
000853
000854         16  CF-EXPENSE-CONTROLS.
000855             20  CF-EXPENSE-METHOD          PIC X.
000856                 88  EXPENSE-CALC-MANUAL        VALUE '1'.
000857                 88  DOLLARS-PER-PMT            VALUE '2'.
000858                 88  PERCENT-OF-PAYMENT         VALUE '3'.
000859                 88  DOLLARS-PER-MONTH          VALUE '4'.
000860             20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
000861             20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
000862
000863         16  CF-CORRESPONDENCE-CONTROL.
000864             20  CF-LETTER-RESEND-OPT       PIC X.
000865                 88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
000866                 88  LETTERS-ARE-ARCHIVED       VALUE '1'.
000867             20  FILLER                     PIC X(4).
000868
000869         16  CF-RESERVE-CONTROLS.
000870             20  CF-MANUAL-SW               PIC X.
000871                 88  CF-MANUAL-RESERVES-USED    VALUE '1'.
000872             20  CF-FUTURE-SW               PIC X.
000873                 88  CF-FUTURE-RESERVES-USED    VALUE '1'.
000874             20  CF-PTC-SW                  PIC X.
000875                 88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
000876             20  CF-IBNR-SW                 PIC X.
000877                 88  CF-IBNR-RESERVES-USED      VALUE '1'.
000878             20  CF-PTC-LF-SW               PIC X.
000879                 88  CF-LF-PTC-USED             VALUE '1'.
000880             20  CF-CDT-ACCESS-METHOD       PIC X.
000881                 88  CF-CDT-ROUND-NEAR          VALUE '1'.
000882                 88  CF-CDT-ROUND-HIGH          VALUE '2'.
000883                 88  CF-CDT-INTERPOLATED        VALUE '3'.
000884             20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
000885
000886         16  CF-CLAIM-CALC-METHOD           PIC X.
000887             88  360-PLUS-MONTHS                VALUE '1'.
000888             88  365-PLUS-MONTHS                VALUE '2'.
000889             88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
000890             88  360-DAILY                      VALUE '4'.
000891             88  365-DAILY                      VALUE '5'.
000892
000893         16  CF-LAST-ALPHA-CHARACTER        PIC X.
000894         16  FILLER                         PIC X(11).
000895
000896         16  CF-LIMIT-AMOUNTS.
000897             20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
000898             20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
000899             20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
000900             20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
000901             20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
000902             20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
000903             20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
000904             20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
000905             20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
000906             20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
000907
000908         16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
000909         16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
000910         16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
000911
000912         16  CF-ZIP-CODE.
000913             20  CF-ZIP-PRIME.
000914                 24  CF-ZIP-1ST             PIC X.
000915                     88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000916                 24  FILLER                 PIC X(4).
000917             20  CF-ZIP-PLUS4               PIC X(4).
000918         16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
000919             20  CF-CAN-POSTAL-1            PIC XXX.
000920             20  CF-CAN-POSTAL-2            PIC XXX.
000921             20  FILLER                     PIC XXX.
000922
000923         16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
000924         16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
000925         16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
000926
000927         16  CF-RATING-SWITCH               PIC X.
000928             88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
000929             88  CF-NO-RATING                   VALUE 'N'.
000930
000931         16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
000932
000933         16  CF-CARRIER-OVER-SHORT.
000934             20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
000935             20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000936
000937         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
000938         16  CF-SECPAY-SWITCH               PIC X.
000939             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
000940             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
000941         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
000942         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
000943         16  FILLER                         PIC X(444).
000944*        16  FILLER                         PIC X(452).
000945
000946
000947****************************************************************
000948*             MORTALITY MASTER RECORD                          *
000949****************************************************************
000950
000951     12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
000952         16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
000953                                INDEXED BY CF-MORT-NDX.
000954             20  CF-MORT-TABLE              PIC X(5).
000955             20  CF-MORT-TABLE-TYPE         PIC X.
000956                 88  CF-MORT-JOINT              VALUE 'J'.
000957                 88  CF-MORT-SINGLE             VALUE 'S'.
000958                 88  CF-MORT-COMBINED           VALUE 'C'.
000959                 88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
000960                 88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
000961             20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
000962             20  CF-MORT-AGE-METHOD         PIC XX.
000963                 88  CF-AGE-LAST                VALUE 'AL'.
000964                 88  CF-AGE-NEAR                VALUE 'AN'.
000965             20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
000966             20  CF-MORT-ADJUSTMENT-DIRECTION
000967                                            PIC X.
000968                 88  CF-MINUS                   VALUE '-'.
000969                 88  CF-PLUS                    VALUE '+'.
000970             20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
000971             20  CF-MORT-JOINT-CODE         PIC X.
000972                 88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
000973             20  CF-MORT-PC-Q               PIC X.
000974                 88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
000975             20  CF-MORT-TABLE-CODE         PIC X(4).
000976             20  CF-MORT-COMMENTS           PIC X(15).
000977             20  FILLER                     PIC X(14).
000978
000979         16  FILLER                         PIC X(251).
000980
000981
000982****************************************************************
000983*             BUSSINESS TYPE MASTER RECORD                     *
000984****************************************************************
000985
000986     12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
000987* FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
000988* RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
000989* AND RECORD 05 IS TYPES 81-99
000990         16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
000991             20  CF-BUSINESS-TITLE          PIC  X(19).
000992             20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
000993                                            PIC S9V9(4) COMP-3.
000994             20  CF-BUS-EXCL-ST-CALL        PIC  X.
000995             20  FILLER                     PIC  X.
000996         16  FILLER                         PIC  X(248).
000997
000998
000999****************************************************************
001000*             TERMINAL MASTER RECORD                           *
001001****************************************************************
001002
001003     12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001004
001005         16  CF-COMPANY-TERMINALS.
001006             20  CF-TERMINAL-ID  OCCURS 120 TIMES
001007                                  PIC X(4).
001008         16  FILLER               PIC X(248).
001009
001010
001011****************************************************************
001012*             LIFE EDIT MASTER RECORD                          *
001013****************************************************************
001014
001015     12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001016         16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
001017             20  CF-LIFE-CODE-IN            PIC XX.
001018             20  CF-LIFE-CODE-OUT           PIC XX.
001019         16  FILLER                         PIC X(248).
001020
001021
001022****************************************************************
001023*             AH EDIT MASTER RECORD                            *
001024****************************************************************
001025
001026     12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001027         16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
001028             20  CF-AH-CODE-IN              PIC XXX.
001029             20  CF-AH-CODE-OUT             PIC XX.
001030         16  FILLER                         PIC X(248).
001031
001032
001033****************************************************************
001034*             CREDIBILITY TABLES                               *
001035****************************************************************
001036
001037     12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
001038         16  CF-CRDB-ENTRY   OCCURS 36 TIMES
001039                             INDEXED BY CF-CRDB-NDX.
001040             20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
001041             20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
001042             20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
001043         16  FILLER                         PIC  X(332).
001044
001045
001046****************************************************************
001047*             REPORT CUSTOMIZATION RECORD                      *
001048****************************************************************
001049
001050     12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
001051         16  CF-ACCOUNT-MASTER-STATUS       PIC X.
001052             88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
001053             88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
001054             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
001055**** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
001056****       A T-TRANSFER.                                   ****
001057             88  CF-ALL-ACCOUNTS                VALUE 'B'.
001058
001059         16  FILLER                         PIC XX.
001060
001061         16  CF-CARRIER-CNTL-OPT.
001062             20  CF-CARRIER-OPT-SEQ         PIC 9.
001063                 88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
001064                 88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
001065             20  CF-CARRIER-SELECT OCCURS 3 TIMES
001066                                            PIC X.
001067         16  CF-GROUP-CNTL-OPT.
001068             20  CF-GROUP-OPT-SEQ           PIC 9.
001069                 88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
001070                 88  CF-GROUP-OPT-NOT-USED      VALUE 0.
001071             20  CF-GROUP-SELECT OCCURS 3 TIMES
001072                                            PIC X(6).
001073         16  CF-STATE-CNTL-OPT.
001074             20  CF-STATE-OPT-SEQ           PIC 9.
001075                 88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
001076                 88  CF-STATE-OPT-NOT-USED      VALUE 0.
001077             20  CF-STATE-SELECT OCCURS 3 TIMES
001078                                            PIC XX.
001079         16  CF-ACCOUNT-CNTL-OPT.
001080             20  CF-ACCOUNT-OPT-SEQ         PIC 9.
001081                 88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
001082                 88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
001083             20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
001084                                            PIC X(10).
001085         16  CF-BUS-TYP-CNTL-OPT.
001086             20  CF-BUS-TYP-OPT-SEQ         PIC 9.
001087                 88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
001088                 88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
001089             20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
001090                                            PIC XX.
001091         16  CF-LF-TYP-CNTL-OPT.
001092             20  CF-LF-TYP-OPT-SEQ          PIC 9.
001093                 88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
001094                 88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
001095             20  CF-BUS-LF-SELECT OCCURS 3 TIMES
001096                                            PIC XX.
001097         16  CF-AH-TYP-CNTL-OPT.
001098             20  CF-AH-TYP-OPT-SEQ          PIC 9.
001099                 88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
001100                 88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
001101             20  CF-BUS-AH-SELECT OCCURS 3 TIMES
001102                                            PIC XX.
001103         16  CF-REPTCD1-CNTL-OPT.
001104             20  CF-REPTCD1-OPT-SEQ         PIC 9.
001105                 88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
001106                 88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
001107             20  CF-REPTCD1-SELECT OCCURS 3 TIMES
001108                                            PIC X(10).
001109         16  CF-REPTCD2-CNTL-OPT.
001110             20  CF-REPTCD2-OPT-SEQ         PIC 9.
001111                 88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
001112                 88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
001113             20  CF-REPTCD2-SELECT OCCURS 3 TIMES
001114                                            PIC X(10).
001115         16  CF-USER1-CNTL-OPT.
001116             20  CF-USER1-OPT-SEQ           PIC 9.
001117                 88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
001118                 88  CF-USER1-OPT-NOT-USED      VALUE 0.
001119             20  CF-USER1-SELECT OCCURS 3 TIMES
001120                                            PIC X(10).
001121         16  CF-USER2-CNTL-OPT.
001122             20  CF-USER2-OPT-SEQ           PIC 9.
001123                 88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
001124                 88  CF-USER2-OPT-NOT-USED      VALUE 0.
001125             20  CF-USER2-SELECT OCCURS 3 TIMES
001126                                            PIC X(10).
001127         16  CF-USER3-CNTL-OPT.
001128             20  CF-USER3-OPT-SEQ           PIC 9.
001129                 88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
001130                 88  CF-USER3-OPT-NOT-USED      VALUE 0.
001131             20  CF-USER3-SELECT OCCURS 3 TIMES
001132                                            PIC X(10).
001133         16  CF-USER4-CNTL-OPT.
001134             20  CF-USER4-OPT-SEQ           PIC 9.
001135                 88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
001136                 88  CF-USER4-OPT-NOT-USED      VALUE 0.
001137             20  CF-USER4-SELECT OCCURS 3 TIMES
001138                                            PIC X(10).
001139         16  CF-USER5-CNTL-OPT.
001140             20  CF-USER5-OPT-SEQ           PIC 9.
001141                 88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
001142                 88  CF-USER5-OPT-NOT-USED      VALUE 0.
001143             20  CF-USER5-SELECT OCCURS 3 TIMES
001144                                            PIC X(10).
001145         16  CF-REINS-CNTL-OPT.
001146             20  CF-REINS-OPT-SEQ           PIC 9.
001147                 88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
001148                 88  CF-REINS-OPT-NOT-USED      VALUE 0.
001149             20  CF-REINS-SELECT OCCURS 3 TIMES.
001150                 24  CF-REINS-PRIME         PIC XXX.
001151                 24  CF-REINS-SUB           PIC XXX.
001152
001153         16  CF-AGENT-CNTL-OPT.
001154             20  CF-AGENT-OPT-SEQ           PIC 9.
001155                 88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
001156                 88  CF-AGENT-OPT-NOT-USED      VALUE 0.
001157             20  CF-AGENT-SELECT OCCURS 3 TIMES
001158                                            PIC X(10).
001159
001160         16  FILLER                         PIC X(43).
001161
001162         16  CF-LOSS-RATIO-SELECT.
001163             20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
001164             20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
001165         16  CF-ENTRY-DATE-SELECT.
001166             20  CF-SEL-LO-ENTRY-DATE       PIC XX.
001167             20  CF-SEL-HI-ENTRY-DATE       PIC XX.
001168         16  CF-EFFECTIVE-DATE-SELECT.
001169             20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
001170             20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
001171
001172         16  CF-EXCEPTION-LIST-IND          PIC X.
001173             88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
001174
001175         16  FILLER                         PIC X(318).
001176
001177****************************************************************
001178*                  EXCEPTION REPORTING RECORD                  *
001179****************************************************************
001180
001181     12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
001182         16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
001183             88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
001184
001185         16  CF-COMBINED-LIFE-AH-OPT.
001186             20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
001187             20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
001188             20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
001189             20  CF-CANCELLATION-RATIO      PIC S9(02).
001190
001191         16  CF-LIFE-OPT.
001192             20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001193             20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001194             20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001195             20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001196             20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001197             20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001198             20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001199             20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001200             20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001201             20  CF-LF-AVG-AGE-MAX          PIC S9(02).
001202
001203         16  CF-AH-OPT.
001204             20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001205             20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001206             20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001207             20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001208             20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001209             20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001210             20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001211             20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001212             20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001213             20  CF-AH-AVG-AGE-MAX          PIC S9(02).
001214
001215         16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
001216             88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
001217             88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
001218             88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
001219
001220         16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
001221
001222         16  FILLER                         PIC X(673).
001223
001224
001225****************************************************************
001226*             MORTGAGE SYSTEM PLAN RECORD                      *
001227****************************************************************
001228
001229     12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
001230         16  CF-PLAN-TYPE                   PIC X.
001231             88  CF-LIFE-MORT-PLAN             VALUE 'L'.
001232             88  CF-DISAB-MORT-PLAN            VALUE 'D'.
001233             88  CF-AD-D-MORT-PLAN             VALUE 'A'.
001234         16  CF-PLAN-ABBREV                 PIC XXX.
001235         16  CF-PLAN-DESCRIPT               PIC X(10).
001236         16  CF-PLAN-NOTES                  PIC X(20).
001237         16  CF-PLAN-ESTABLISH-DATE         PIC XX.
001238         16  CF-PLAN-UNDERWRITING.
001239             20  CF-PLAN-TERM-DATA.
001240                 24  CF-MINIMUM-TERM        PIC S999      COMP-3.
001241                 24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
001242             20  CF-PLAN-AGE-DATA.
001243                 24  CF-MINIMUM-AGE         PIC S999      COMP-3.
001244                 24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
001245                 24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
001246             20  CF-PLAN-BENEFIT-DATA.
001247                 24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001248                 24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001249                 24  CF-MAXIMUM-MONTHLY-BENEFIT
001250                                            PIC S9(7)V99  COMP-3.
001251         16  CF-PLAN-POLICY-FORMS.
001252             20  CF-POLICY-FORM             PIC X(12).
001253             20  CF-MASTER-APPLICATION      PIC X(12).
001254             20  CF-MASTER-POLICY           PIC X(12).
001255         16  CF-PLAN-RATING.
001256             20  CF-RATE-CODE               PIC X(5).
001257             20  CF-SEX-RATING              PIC X.
001258                 88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
001259                 88  CF-PLAN-SEX-RATED         VALUE '2'.
001260             20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
001261             20  CF-SUB-STD-TYPE            PIC X.
001262                 88  CF-PCT-OF-PREM            VALUE '1'.
001263                 88  CF-PCT-OF-BENE            VALUE '2'.
001264         16  CF-PLAN-PREM-TOLERANCES.
001265             20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
001266             20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
001267         16  CF-PLAN-PYMT-TOLERANCES.
001268             20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
001269             20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
001270         16  CF-PLAN-MISC-DATA.
001271             20  FILLER                     PIC X.
001272             20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
001273             20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
001274         16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
001275         16  CF-PLAN-IND-GRP                PIC X.
001276             88  CF-MORT-INDIV-PLAN            VALUE 'I'
001277                                                     '1'.
001278             88  CF-MORT-GROUP-PLAN            VALUE 'G'
001279                                                     '2'.
001280         16  CF-MIB-SEARCH-SW               PIC X.
001281             88  CF-MIB-SEARCH-ALL             VALUE '1'.
001282             88  CF-MIB-SEARCH-NONE            VALUE '2'.
001283             88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
001284             88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
001285         16  CF-ALPHA-SEARCH-SW             PIC X.
001286             88  CF-MIB-ALPHA-ALL              VALUE '1'.
001287             88  CF-MIB-ALPHA-NONE             VALUE '2'.
001288             88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
001289             88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
001290             88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
001291             88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
001292             88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
001293             88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
001294             88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
001295             88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
001296                                                     'A' 'B' 'C'
001297                                                     'X' 'Y' 'Z'.
001298         16  CF-EFF-DT-RULE-SW              PIC X.
001299             88  CF-EFF-DT-ENTER               VALUE 'E'.
001300             88  CF-EFF-DT-MONTH               VALUE 'M'.
001301             88  CF-EFF-DT-QTR                 VALUE 'Q'.
001302             88  CF-EFF-DT-SEMI                VALUE 'S'.
001303             88  CF-EFF-DT-ANN                 VALUE 'A'.
001304         16  FILLER                         PIC X(4).
001305         16  CF-HEALTH-QUESTIONS            PIC X.
001306             88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
001307         16  CF-GRACE-PERIOD                PIC S999      COMP-3.
001308         16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
001309         16  CF-PLAN-SNGL-JNT               PIC X.
001310             88  CF-COMBINED-PLAN              VALUE 'C'.
001311             88  CF-JNT-PLAN                   VALUE 'J'.
001312             88  CF-SNGL-PLAN                  VALUE 'S'.
001313         16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
001314         16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
001315         16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
001316         16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
001317         16  CF-RERATE-CNTL                 PIC  X.
001318             88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
001319             88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
001320             88  CF-DO-NOT-RERATE               VALUE '3' ' '.
001321             88  CF-AUTO-RECALC                 VALUE '4'.
001322         16  CF-BENEFIT-TYPE                PIC  X.
001323             88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
001324             88  CF-BENEFIT-REDUCES             VALUE '2'.
001325         16  CF-POLICY-FEE                  PIC S999V99
001326                                                    COMP-3.
001327         16  CF-1ST-NOTICE-FORM             PIC  X(04).
001328         16  CF-2ND-NOTICE-FORM             PIC  X(04).
001329         16  CF-3RD-NOTICE-FORM             PIC  X(04).
001330         16  CF-4TH-NOTICE-FORM             PIC  X(04).
001331         16  FILLER                         PIC  X(32).
001332         16  CF-TERMINATION-FORM            PIC  X(04).
001333         16  FILLER                         PIC  X(08).
001334         16  CF-CLAIM-CAP                   PIC S9(7)V99
001335                                                       COMP-3.
001336         16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
001337         16  CF-ISSUE-LETTER                PIC  X(4).
001338         16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
001339         16  CF-DEPENDENT-COVERAGE          PIC  X.
001340             88  CF-YES-DEP-COV                 VALUE 'Y'.
001341             88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
001342         16  CF-MP-REFUND-CALC              PIC X.
001343             88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
001344             88  CF-MP-REFD-BY-R78              VALUE '1'.
001345             88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
001346             88  CF-MP-REFD-AS-CALIF            VALUE '3'.
001347             88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
001348             88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
001349             88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
001350             88  CF-MP-REFD-MEAN                VALUE '8'.
001351         16  CF-ALT-RATE-CODE               PIC  X(5).
001352
001353
001354         16  FILLER                         PIC X(498).
001355****************************************************************
001356*             MORTGAGE COMPANY MASTER RECORD                   *
001357****************************************************************
001358
001359     12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001360         16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
001361         16  CF-MORTG-ACCESS-CONTROL        PIC X.
001362             88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
001363             88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
001364             88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
001365             88  CF-MORT-PROD-CNTL                   VALUE '3'.
001366             88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
001367
001368         16  CF-MORTG-CONVERSION-DATE       PIC XX.
001369         16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
001370         16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
001371         16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
001372         16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
001373
001374         16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
001375             88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
001376         16  CF-MP-RECON-USE-IND            PIC X(1).
001377             88  CF-MP-USE-RECON             VALUE 'Y'.
001378         16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
001379             88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
001380         16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
001381             88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
001382             88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
001383         16  FILLER                         PIC X(1).
001384         16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
001385             88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
001386         16  CF-MORTG-MIB-VERSION           PIC X.
001387             88  CF-MORTG-MIB-BATCH         VALUE '1'.
001388             88  CF-MORTG-MIB-ONLINE        VALUE '2'.
001389             88  CF-MORTG-MIB-BOTH          VALUE '3'.
001390         16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
001391             20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
001392                 88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
001393             20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
001394                 88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
001395             20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
001396                 88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
001397             20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
001398                 88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
001399             20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
001400                 88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
001401             20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
001402                 88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
001403         16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
001404         16  FILLER                         PIC X(7).
001405         16  CF-MORTG-DESTINATION-SYMBOL.
001406             20  CF-MORTG-MIB-COMM          PIC X(5).
001407             20  CF-MORTG-MIB-TERM          PIC X(5).
001408         16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
001409             88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
001410         16  FILLER                         PIC X(03).
001411         16  CF-MP-CHECK-NO-CONTROL.
001412             20  CF-MP-CHECK-NO-METHOD      PIC X(01).
001413                 88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
001414                 88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
001415                                                ' ' LOW-VALUES.
001416                 88  CF-MP-CHECK-NO-PRE-PRINTED
001417                                               VALUE '3'.
001418         16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
001419         16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
001420         16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
001421             20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
001422                 88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
001423             20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
001424                 88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
001425             20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
001426                 88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
001427             20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
001428                 88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
001429             20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
001430                 88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
001431             20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
001432                 88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
001433         16  CF-MORTG-BILLING-AREA.
001434             20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
001435                                            PIC X.
001436         16  CF-MORTG-MONTH-END-DT          PIC XX.
001437         16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
001438         16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
001439         16  CF-MORTG-MIB-DEST-SW           PIC X.
001440             88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
001441             88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
001442         16  FILLER                         PIC X.
001443         16  CF-MORTG-LABEL-CONTROL         PIC X.
001444             88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
001445             88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
001446         16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
001447         16  FILLER                         PIC X(8).
001448         16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
001449         16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
001450         16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
001451         16  CF-ACH-COMPANY-ID.
001452             20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
001453                 88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
001454                 88  CF-ACH-ICD-DUNS                VALUE '3'.
001455                 88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
001456             20  CF-ACH-COMPANY-ID-NO       PIC X(9).
001457         16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
001458             88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
001459         16  CF-RATE-DEV-AUTHORIZATION      PIC X.
001460             88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
001461             88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
001462         16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
001463         16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
001464         16  FILLER                         PIC X(536).
001465
001466****************************************************************
001467*             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
001468****************************************************************
001469
001470     12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
001471         16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
001472             20  CF-FEMALE-HEIGHT.
001473                 24  CF-FEMALE-FT           PIC 99.
001474                 24  CF-FEMALE-IN           PIC 99.
001475             20  CF-FEMALE-MIN-WT           PIC 999.
001476             20  CF-FEMALE-MAX-WT           PIC 999.
001477         16  FILLER                         PIC X(428).
001478
001479     12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
001480         16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
001481             20  CF-MALE-HEIGHT.
001482                 24  CF-MALE-FT             PIC 99.
001483                 24  CF-MALE-IN             PIC 99.
001484             20  CF-MALE-MIN-WT             PIC 999.
001485             20  CF-MALE-MAX-WT             PIC 999.
001486         16  FILLER                         PIC X(428).
001487******************************************************************
001488*             AUTOMATIC ACTIVITY RECORD                          *
001489******************************************************************
001490     12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
001491         16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
001492             20  CF-SYS-ACTIVE-SW           PIC X(01).
001493             20  CF-SYS-LETTER-ID           PIC X(04).
001494             20  CF-SYS-RESEND-DAYS         PIC 9(03).
001495             20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
001496             20  CF-SYS-RESET-SW            PIC X(01).
001497             20  CF-SYS-REPORT-DAYS         PIC 9(03).
001498             20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
001499
001500         16  FILLER                         PIC X(50).
001501
001502         16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
001503             20  CF-USER-ACTIVE-SW          PIC X(01).
001504             20  CF-USER-LETTER-ID          PIC X(04).
001505             20  CF-USER-RESEND-DAYS        PIC 9(03).
001506             20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
001507             20  CF-USER-RESET-SW           PIC X(01).
001508             20  CF-USER-REPORT-DAYS        PIC 9(03).
001509             20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
001510             20  CF-USER-ACTIVITY-DESC      PIC X(20).
001511
001512         16  FILLER                         PIC X(246).
      *<<((file: ELCCNTL))
000234     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL680' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000235 VCOBOL-DUMMY-PROCEDURE.
000236     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000237     MOVE '5'                   TO DC-OPTION-CODE.
000238     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000239     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000240     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
000241
000242     MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
000243     MOVE 2           TO EMI-NUMBER-OF-LINES.
000244
000245     IF EIBCALEN = 0
000246         GO TO 8800-UNAUTHORIZED-ACCESS.
000247
000248     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000249         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000250             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000251             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000252             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000253             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000254             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000255             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000256             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000257             MOVE THIS-PGM             TO PI-CALLING-PROGRAM.
000258
000259     IF EIBTRNID NOT = TRANS-ID
000260         GO TO 8100-SEND-INITIAL-MAP.
000261
000262     IF EIBAID = DFHCLEAR
000263         GO TO 9400-CLEAR.
000264
000265     
      * EXEC CICS HANDLE CONDITION
000266*        MAPFAIL (8100-SEND-INITIAL-MAP)
000267*    END-EXEC.
      *    MOVE '"$?                   ! " #00002876' TO DFHEIV0
           MOVE X'22243F202020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303032383736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000268
000269 0100-RECEIVE-MAP.
000270     
      * EXEC CICS RECEIVE
000271*        MAP    (MAP-NAME)
000272*        MAPSET (MAPSET-NAME)
000273*        INTO   (EL680AI)
000274*    END-EXEC.
           MOVE LENGTH OF
            EL680AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002881' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303032383831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL680AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000275
000276     EJECT
000277 0200-RECEIVE.
000278     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000279         MOVE ER-0008  TO EMI-ERROR
000280         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000281         MOVE -1    TO ANAMEL
000282         GO TO 8200-SEND-DATAONLY.
000283
000284     IF PFENTERL = 0
000285         GO TO 0300-CHECK-PFKEYS.
000286     IF EIBAID NOT = DFHENTER
000287         MOVE ER-0004  TO EMI-ERROR
000288         GO TO 0320-INPUT-ERROR.
000289
000290     IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
000291         MOVE PF-VALUES (PFENTERI) TO EIBAID
000292     ELSE
000293         MOVE ER-0029  TO EMI-ERROR
000294         GO TO 0320-INPUT-ERROR.
000295
000296 0300-CHECK-PFKEYS.
000297     IF EIBAID = DFHPF1
000298         GO TO 1000-EDIT-INPUT-DATA.
000299
000300     IF EIBAID = DFHPF23
000301         GO TO 8810-PF23.
000302
000303     IF EIBAID = DFHPF24
000304         GO TO 9200-RETURN-MAIN-MENU.
000305
000306     IF EIBAID = DFHPF12
000307         GO TO 9500-PF12.
000308
000309     IF EIBAID = DFHENTER
000310         GO TO 1000-EDIT-INPUT-DATA.
000311
000312     MOVE ER-0029 TO EMI-ERROR.
000313 0320-INPUT-ERROR.
000314     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000315     MOVE AL-UNBON               TO PFENTERA.
000316     IF PFENTERL = 0
000317         MOVE -1                 TO ANAMEL
000318     ELSE
000319         MOVE -1                 TO PFENTERL.
000320
000321     GO TO 8200-SEND-DATAONLY.
000322
000323     EJECT
000324 1000-EDIT-INPUT-DATA.
000325******EDIT AMOUNT REQUESTED
000326
000327     
      * EXEC CICS BIF DEEDIT
000328*        FIELD  (AAMOUNTI)
000329*        LENGTH (12)
000330*    END-EXEC.
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002938' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032393338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AAMOUNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000331
000332     IF (AAMOUNTI NUMERIC)  AND
000333        (AAMOUNTI NOT = ZERO)
000334         MOVE AAMOUNTI       TO  WS-AMOUNT-REQUESTED
000335                                 AAMOUNTO
000336         MOVE AL-UNNON       TO  AAMOUNTA
000337       ELSE
000338         MOVE -1             TO  AAMOUNTL
000339         MOVE AL-UNBON       TO  AAMOUNTA
000340         MOVE ER-2361        TO  EMI-ERROR
000341         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000342         ADD +1  TO  WS-ERROR-COUNT.
000343
000344***********INTEREST RATE (APR)
000345     
      * EXEC CICS BIF DEEDIT
000346*        FIELD  (AINTRATI)
000347*        LENGTH (8)
000348*    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002956' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032393536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AINTRATI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000349
000350     IF (AINTRATI NUMERIC)  AND
000351        (AINTRATI NOT = ZERO)
000352         MOVE AINTRATI       TO  WS-APR
000353                                 AINTRATO
000354         MOVE AL-UNNON       TO  AINTRATA
000355       ELSE
000356         MOVE -1             TO  AINTRATL
000357         MOVE AL-UNBON       TO  AINTRATA
000358         MOVE ER-2363        TO  EMI-ERROR
000359         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000360         ADD +1  TO  WS-ERROR-COUNT.
000361
000362***********PAYMENT FREQUENCY
000363     IF AFREQI = 'MO' OR 'SM' OR 'BW' OR 'WK' OR
000364                 'SA' OR 'AN' OR '13'
000365         MOVE AL-UANON       TO  AFREQA
000366         MOVE AFREQI         TO  WS-FREQ
000367       ELSE
000368         MOVE -1             TO  AFREQL
000369         MOVE AL-UABON       TO  AFREQA
000370         MOVE ER-2920        TO  EMI-ERROR
000371         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000372         ADD +1  TO  WS-ERROR-COUNT.
000373
000374***********NUMBER OF PAYMENTS
000375     
      * EXEC CICS BIF DEEDIT
000376*        FIELD  (ANOPMTSI)
000377*        LENGTH (3)
000378*    END-EXEC.
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002986' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303032393836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ANOPMTSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000379
000380     IF (ANOPMTSI NUMERIC) AND
000381        (ANOPMTSI NOT = ZERO)
000382         MOVE ANOPMTSI       TO  WS-NUMBER-OF-PMTS
000383                                 ANOPMTSO
000384         MOVE AL-UNNON       TO  ANOPMTSA
000385       ELSE
000386         MOVE -1             TO  ANOPMTSL
000387         MOVE AL-UNBON       TO  ANOPMTSA
000388         MOVE ER-2364        TO  EMI-ERROR
000389         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000390         ADD +1  TO  WS-ERROR-COUNT.
000391
000392***********ADDITIONAL DAYS TO FIRST PAYMENT
000393     IF AADDAYSL GREATER THAN ZERO
000394         
      * EXEC CICS BIF DEEDIT
000395*            FIELD  (AADDAYSI)
000396*            LENGTH (3)
000397*        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003005' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033303035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AADDAYSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000398         IF AADDAYSI NUMERIC
000399             MOVE AADDAYSI       TO  WS-ADDTL-DAYS
000400                                     AADDAYSO
000401             MOVE AL-UNNON       TO  AADDAYSA
000402           ELSE
000403             MOVE -1             TO  AADDAYSL
000404             MOVE AL-UNBON       TO  AADDAYSA
000405             MOVE ER-2921        TO  EMI-ERROR
000406             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000407             ADD +1  TO  WS-ERROR-COUNT.
000408
000409***********ADDITIONAL DAYS CHARGING METHOD
000410********** SPACE=NO CHG, 1=LF, 2=AH, 3=LF AND AH
000411     IF ADDCHGL  GREATER THAN ZERO
000412     IF ADDCHGI = '1' OR '2' OR '3' OR ' '
000413         MOVE AL-UANON       TO  ADDCHGA
000414         MOVE ADDCHGI        TO  WS-ADDCHG
000415       ELSE
000416         MOVE -1             TO  ADDCHGL
000417         MOVE AL-UABON       TO  ADDCHGA
000418         MOVE ER-4004        TO  EMI-ERROR
000419         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000420         ADD +1  TO  WS-ERROR-COUNT.
000421
000422***********BASIS
000423     IF ABASISI = 'AF' OR 'TP' OR 'NP'
000424         MOVE AL-UANON       TO  ABASISA
000425         MOVE ABASISI        TO  WS-BASIS-X
000426       ELSE
000427         MOVE -1             TO  ABASISL
000428         MOVE AL-UABON       TO  ABASISA
000429         MOVE ER-2367        TO  EMI-ERROR
000430         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000431         ADD +1  TO  WS-ERROR-COUNT.
000432
000433**********USE O/B NET ACTUARIAL RATE
000434     IF OBYORNL GREATER ZERO
000435     IF OBYORNI = 'Y' OR 'N'
000436         MOVE AL-UANON       TO  OBYORNA
000437         MOVE OBYORNI        TO  WS-OBYN
000438       ELSE
000439         MOVE -1             TO  OBYORNL
000440         MOVE AL-UABON       TO  OBYORNA
000441         MOVE ER-2922        TO  EMI-ERROR
000442         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000443         ADD +1  TO  WS-ERROR-COUNT.
000444
000445***********NP BENEFIT INCLUDES PREMIUM (Y/N)
000446     IF WS-BASIS-X = 'NP'
000447         IF ANPREMI = 'Y' OR 'N'
000448             MOVE AL-UANON       TO  ANPREMA
000449             MOVE ANPREMI        TO  WS-NPBEN
000450           ELSE
000451             MOVE -1             TO  ANPREML
000452             MOVE AL-UABON       TO  ANPREMA
000453             MOVE ER-2922        TO  EMI-ERROR
000454             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000455             ADD +1  TO  WS-ERROR-COUNT.
000456
000457***********TRUNCATED LIFE NO. OF PAYMENTS COVERED
000458     IF ATRUNCL GREATER THAN ZERO
000459         
      * EXEC CICS BIF DEEDIT
000460*            FIELD  (ATRUNCI)
000461*            LENGTH (3)
000462*        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003070' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033303730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ATRUNCI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000463         IF ATRUNCI IS NUMERIC
000464             MOVE ATRUNCI        TO  WS-TRUNC
000465                                     ATRUNCO
000466             MOVE AL-UNNON       TO  ATRUNCA
000467           IF WS-TRUNC GREATER WS-NUMBER-OF-PMTS
000468             MOVE -1             TO  ATRUNCL
000469             MOVE AL-UNBON       TO  ATRUNCA
000470             MOVE ER-4007        TO  EMI-ERROR
000471             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000472             ADD +1  TO  WS-ERROR-COUNT
000473           ELSE
000474             NEXT SENTENCE
000475           ELSE
000476             MOVE -1             TO  ATRUNCL
000477             MOVE AL-UNBON       TO  ATRUNCA
000478             MOVE ER-2923        TO  EMI-ERROR
000479             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000480             ADD +1  TO  WS-ERROR-COUNT.
000481
000482***********EXTRA INTEREST PERIODS
000483     IF AEXTRAL GREATER THAN ZERO
000484         
      * EXEC CICS BIF DEEDIT
000485*            FIELD  (AEXTRAI)
000486*            LENGTH (1)
000487*        END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003095' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033303935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXTRAI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000488         IF AEXTRAI NUMERIC
000489             MOVE AEXTRAI        TO  WS-EXTRA
000490                                     AEXTRAO
000491             MOVE AL-UNNON       TO  AEXTRAA
000492           ELSE
000493             MOVE -1             TO  AEXTRAL
000494             MOVE AL-UNBON       TO  AEXTRAA
000495             MOVE ER-2924        TO  EMI-ERROR
000496             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000497             ADD +1  TO  WS-ERROR-COUNT.
000498
000499***********LIFE RATE
000500     IF ALRATEL GREATER THAN ZERO
000501         
      * EXEC CICS BIF DEEDIT
000502*            FIELD  (ALRATEI)
000503*            LENGTH (8)
000504*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003112' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033313132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000505         IF ALRATEI NUMERIC
000506             MOVE ALRATEI        TO  WS-LIFE-RATE
000507                                     ALRATEO
000508             MOVE AL-UNNON       TO  ALRATEA
000509           ELSE
000510             MOVE -1             TO  ALRATEL
000511             MOVE AL-UNBON       TO  ALRATEA
000512             MOVE ER-2365        TO  EMI-ERROR
000513             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000514             ADD +1  TO  WS-ERROR-COUNT.
000515
000516***********LIFE DEVIATION
000517     IF ALFDEVL GREATER THAN ZERO
000518         
      * EXEC CICS BIF DEEDIT
000519*            FIELD  (ALFDEVI)
000520*            LENGTH (7)
000521*        END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003129' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033313239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALFDEVI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000522         IF ALFDEVI NUMERIC
000523             MOVE ALFDEVI        TO  WS-LIFE-DEVIATION
000524                                     ALFDEVO
000525             MOVE AL-UNNON       TO  ALFDEVA
000526           ELSE
000527             MOVE -1             TO  ALFDEVL
000528             MOVE AL-UNBON       TO  ALFDEVA
000529             MOVE ER-2925        TO  EMI-ERROR
000530             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000531             ADD +1  TO  WS-ERROR-COUNT
000532     ELSE
000533        MOVE 100 TO WS-LIFE-DEVIATION
000534                    ALFDEVO.
000535
000536***********DISABILITY RATE
000537     IF ADRATEL GREATER THAN ZERO
000538         
      * EXEC CICS BIF DEEDIT
000539*            FIELD  (ADRATEI)
000540*            LENGTH (8)
000541*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003149' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033313439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ADRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000542         IF ADRATEI NUMERIC
000543             MOVE ADRATEI        TO  WS-AH-RATE
000544                                     ADRATEO
000545             MOVE AL-UNNON       TO  ADRATEA
000546           ELSE
000547             MOVE -1             TO  ADRATEL
000548             MOVE AL-UNBON       TO  ADRATEA
000549             MOVE ER-2366        TO  EMI-ERROR
000550             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000551             ADD +1  TO  WS-ERROR-COUNT.
000552
000553***********AH DEVIATION
000554     IF AAHDEVL GREATER THAN ZERO
000555         
      * EXEC CICS BIF DEEDIT
000556*            FIELD  (AAHDEVI)
000557*            LENGTH (7)
000558*        END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003166' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033313636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AAHDEVI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000559         IF AAHDEVI NUMERIC
000560             MOVE AAHDEVI        TO  WS-AH-DEVIATION
000561                                     AAHDEVO
000562             MOVE AL-UNNON       TO  AAHDEVA
000563           ELSE
000564             MOVE -1             TO  AAHDEVL
000565             MOVE AL-UNBON       TO  AAHDEVA
000566             MOVE ER-2926        TO  EMI-ERROR
000567             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000568             ADD +1  TO  WS-ERROR-COUNT
000569     ELSE
000570        MOVE 100 TO WS-AH-DEVIATION
000571                    AAHDEVO.
000572
000573********** NO. PAYMENTS COVERED IF NOT SAME AS LIFE
000574     IF ANOTLFL GREATER THAN ZERO
000575         
      * EXEC CICS BIF DEEDIT
000576*            FIELD  (ANOTLFI)
000577*            LENGTH (3)
000578*        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003186' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033313836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ANOTLFI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000579         IF ANOTLFI NUMERIC
000580             MOVE ANOTLFI        TO  WS-PAYMENTS-COVERED
000581                                     ANOTLFO
000582             MOVE AL-UNNON       TO  ANOTLFA
000583           ELSE
000584             MOVE -1             TO  ANOTLFL
000585             MOVE AL-UNBON       TO  ANOTLFA
000586             MOVE ER-2927        TO  EMI-ERROR
000587             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000588             ADD +1  TO  WS-ERROR-COUNT.
000589
000590********** BALLOON AMOUNT
000591     IF BALAMTL GREATER THAN ZERO
000592         
      * EXEC CICS BIF DEEDIT
000593*            FIELD  (BALAMTI)
000594*            LENGTH (12)
000595*        END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003203' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033323033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000596         IF BALAMTI NUMERIC
000597             MOVE BALAMTI        TO  WS-BALLOON-AMOUNT
000598                                     BALAMTO
000599             MOVE AL-UNNON       TO  BALAMTA
000600           ELSE
000601             MOVE -1             TO  BALAMTL
000602             MOVE AL-UNBON       TO  BALAMTA
000603             MOVE ER-2927        TO  EMI-ERROR
000604             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000605             ADD +1  TO  WS-ERROR-COUNT.
000606
000607***********BALLOON RATE
000608     IF BLRATEL GREATER THAN ZERO
000609         
      * EXEC CICS BIF DEEDIT
000610*            FIELD  (BLRATEI)
000611*            LENGTH (8)
000612*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003220' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033323230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BLRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000613         IF BLRATEI NUMERIC
000614             MOVE BLRATEI        TO  WS-BL-RATE
000615                                     BLRATEO
000616             MOVE AL-UNNON       TO  BLRATEA
000617           ELSE
000618             MOVE -1             TO  BLRATEL
000619             MOVE AL-UNBON       TO  BLRATEA
000620             MOVE ER-2366        TO  EMI-ERROR
000621             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000622             ADD +1  TO  WS-ERROR-COUNT.
000623
000624********** BALLOON PAYMENT COINCIDE W/LAST PAYMENT (Y/N)
000625     IF BALYNL GREATER THAN ZERO
000626         IF BALYNI = 'Y' OR 'N'
000627             MOVE BALYNI         TO  WS-BALYN
000628             MOVE AL-UANON       TO  BALYNA
000629           ELSE
000630             MOVE -1             TO  BALYNL
000631             MOVE AL-UABON       TO  BALYNA
000632             MOVE ER-2927        TO  EMI-ERROR
000633             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000634             ADD +1  TO  WS-ERROR-COUNT.
000635
000636********** REGULAR PAYMENT AMOUNT
000637*    IF BALPAYL GREATER THAN ZERO
000638*        EXEC CICS BIF DEEDIT
000639*            FIELD  (BALPAYI)
000640*            LENGTH (12)
000641*        END-EXEC
000642*        IF BALPAYI NUMERIC  AND
000643*           BALPAYI NOT = ZERO
000644*            MOVE BALPAYI        TO  WS-REGULAR-PAYMENT
000645*                                    BALPAYO
000646*            MOVE AL-UNNON       TO  BALPAYA
000647*          ELSE
000648*            MOVE -1             TO  BALPAYL
000649*            MOVE AL-UNBON       TO  BALPAYA
000650*            MOVE ER-2927        TO  EMI-ERROR
000651*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000652*            ADD +1  TO  WS-ERROR-COUNT.
000653
000654     IF WS-ERROR-COUNT GREATER THAN ZERO
000655         GO TO 8200-SEND-DATAONLY.
000656    EJECT
000657
000658 4000-PERFORM-CALCULATIONS.
000659     IF WS-FREQ = 'MO'
000660          MOVE 12 TO PPY.
000661     IF WS-FREQ = '13'
000662          MOVE 13 TO PPY.
000663     IF WS-FREQ = 'SM'
000664          MOVE 24 TO PPY.
000665     IF WS-FREQ = 'BW'
000666          MOVE 26 TO PPY.
000667     IF WS-FREQ = 'WK'
000668          MOVE 52 TO PPY.
000669     IF WS-FREQ = 'SA'
000670          MOVE 2 TO PPY.
000671     IF WS-FREQ = 'AN'
000672          MOVE 1 TO PPY.
000673
000674     IF WS-BASIS-X = 'AF'
000675          MOVE 1 TO WS-BASIS.
000676     IF WS-BASIS-X = 'TP'
000677          MOVE 2 TO WS-BASIS.
000678     IF WS-BASIS-X = 'NP'
000679          MOVE 3 TO WS-BASIS.
000680
000681     MOVE WS-AMOUNT-REQUESTED TO OTF.
000682
000683     COMPUTE I = WS-APR / (100 * PPY).
000684
000685     MOVE WS-ADDTL-DAYS TO ADL-DAYS.
000686
000687     COMPUTE ADL-ADJ = 1 + I * ADL-DAYS / (365 / PPY).
000688
000689     COMPUTE ATF = OTF * ADL-ADJ.
000690
000691     MOVE WS-TRUNC TO COV-TERM.
000692
000693     MOVE WS-PAYMENTS-COVERED TO AHN.
000694
000695     MOVE WS-NUMBER-OF-PMTS TO N.
000696
000697     IF AHN LESS 1
000698         MOVE COV-TERM TO AHN.
000699
000700     IF AHN LESS 1
000701         MOVE N TO AHN.
000702
000703     MOVE WS-EXTRA TO EMII.
000704
000705     EJECT
000706
000707 4000-CONTINUE-CALCS-1.
000708********* M USED FOR TRUNCATED NET PAY
000709     IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'
000710                         OR 'LGX'
000711        IF WS-TRUNC NOT EQUAL +0
000712           MOVE WS-TRUNC         TO M.
000713
000714     MOVE WS-LIFE-RATE TO LIFE-RATE.
000715
000716     COMPUTE LR = (LIFE-RATE / 100) * 12 / PPY.
000717
000718     COMPUTE LIFEDEV = WS-LIFE-DEVIATION / 100.
000719
000720     COMPUTE LR = LR * LIFEDEV.
000721
000722     MOVE WS-AH-RATE TO AH-RATE.
000723
000724     COMPUTE AH = AH-RATE / 100.
000725
000726     COMPUTE AHDEV = WS-AH-DEVIATION / 100.
000727
000728     COMPUTE AH = AH * AHDEV.
000729
000730****** BALLOON RATE
000731     MOVE WS-BL-RATE TO BL-RATE.
000732
000733     COMPUTE BL = BL-RATE / 100.
000734*******************
000735
000736*****  N1 USED FOR O/B
000737     IF COV-TERM = ZERO
000738        COMPUTE N1 = N * 12 / PPY
000739       ELSE
000740        COMPUTE N1 = COV-TERM * 12 / PPY.
000741
000742     COMPUTE K1 = 12 / PPY.
000743
000744     IF PPY = 1 OR 2
000745         MOVE 1     TO K1.
000746
000747     COMPUTE PADJ = (N1 + K1) / (N1 + 1).
000748
000749     IF WS-OBYN = 'Y'
000750        IF PI-COMPANY-ID EQUAL 'CSO' OR 'CID' OR 'LGX' OR 'AHL'
000751              or 'FNL'
000752        COMPUTE L = LR * PADJ * M / 12 * (N1 + 1) / 20 * 12 / N1
000753        ELSE
000754        COMPUTE L = LR * PADJ * N / 12 * (N1 + 1) / 20 * 12 / N1
000755     ELSE
000756        IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'
000757                         OR 'CID' OR 'AHL' OR 'FNL'
000758                         OR 'LGX'
000759           COMPUTE L = LR * PADJ * M / 12
000760        ELSE
000761*          COMPUTE L = LR * PADJ * N / 12.
000762           COMPUTE L = LR * PADJ * AHN / 12.
000763
000764     IF WS-BALLOON-AMOUNT NOT = ZERO
000765         GO TO 7000-BALLOON-CALC.
000766
000767     COMPUTE V = 1 / (1 + I).
000768
000769     COMPUTE VU = 1 - V ** N.
000770
000771     COMPUTE ANGLN = VU / I.
000772
000773     IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'
000774                         OR 'CID' OR 'AHL' or 'FNL'
000775                         OR 'LGX'
000776        COMPUTE TOT-PMTS = I / VU * M
000777     ELSE
000778        COMPUTE TOT-PMTS = I / VU * N.
000779
000780     COMPUTE ATOT-PMTS = I / VU * AHN.
000781
000782     EJECT
000783
000784 4000-CONTINUE-CALCS-2.
000785     GO TO 4000-COMM, 4000-COMM, 4000-NP
000786         DEPENDING ON WS-BASIS.
000787
000788 4000-NP.
000789     MOVE COV-TERM TO M.
000790     IF M = 0
000791        MOVE N TO M.
000792
000793     COMPUTE VU1 = 1 - V ** (N - M).
000794
000795     COMPUTE ANGLM = VU1 / I.
000796
000797     COMPUTE VA = (M - ANGLN + ANGLM) * 2 * N
000798                   / (I * M * (2 * N - M + 1) * ANGLN).
000799
000800     IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'
000801                         OR 'CID' OR 'AHL' OR 'FNL'
000802                         OR 'LGX'
000803        COMPUTE VA = VA * ((1 + N) / (1 + M)).
000804
000805     COMPUTE RA = 1 - (N - M) * (N - M + 1) / (N * (N + 1)).
000806
000807     COMPUTE TA = VA * RA.
000808
000809     COMPUTE NPLF = TA * L * (1 + EMII * I).
000810
000811     IF WS-NPBEN = 'Y'
000812        COMPUTE NPLF = NPLF / (1 - NPLF).
000813
000814 4000-COMM.
000815     MOVE ZERO TO LIFE-PREM AH-PREM.
000816
000817     PERFORM 4000-GET-PREMIUM THRU 4000-EXIT.
000818     GO TO 6000-DISPLAY-RESULTS.
000819
000820 4000-GET-PREMIUM.
000821     PERFORM 5000-COMPUTE-PREMIUMS THRU 5000-EXIT
000822         VARYING XREPT FROM +1 BY +1 UNTIL XREPT = +15.
000823 4000-EXIT.
000824     EXIT.
000825
000826    EJECT
000827
000828 5000-COMPUTE-PREMIUMS.
000829     GO TO 5000-AF, 5000-TP, 5000-NP
000830         DEPENDING ON WS-BASIS.
000831
000832 5000-AF.
000833     COMPUTE LIFE-PREM = L / (1 - L) * (1 + AH-PREM).
000834     GO TO 5000-GET-AH-PREM.
000835
000836 5000-TP.
000837     COMPUTE LIFE-PREM = TOT-PMTS * L / (1 - TOT-PMTS * L)
000838                             * (1 + AH-PREM).
000839     GO TO 5000-GET-AH-PREM.
000840
000841 5000-NP.
000842     COMPUTE LIFE-PREM = NPLF * (1 + AH-PREM).
000843     GO TO 5000-GET-AH-PREM.
000844
000845 5000-GET-AH-PREM.
000846     COMPUTE AH-PREM = ATOT-PMTS * AH / (1 - ATOT-PMTS * AH)
000847                             * (1 + LIFE-PREM).
000848
000849     IF AH = 0 OR
000850        LR = 0
000851          GO TO 4000-EXIT.
000852
000853 5000-EXIT.
000854     EXIT.
000855
000856   EJECT
000857
000858 6000-DISPLAY-RESULTS.
000859     IF CHG-LF OR CHG-LF-AH
000860         COMPUTE L-PREMIUM = LIFE-PREM * ATF
000861       ELSE
000862         COMPUTE L-PREMIUM = LIFE-PREM * OTF.
000863
000864     COMPUTE L-PREMIUM = (L-PREMIUM * 100 + .502) / 100.
000865
000866     IF CHG-AH OR CHG-LF-AH
000867         COMPUTE A-PREMIUM = AH-PREM * ATF
000868       ELSE
000869         COMPUTE A-PREMIUM = AH-PREM * OTF.
000870
000871     COMPUTE A-PREMIUM = (A-PREMIUM * 100 + .502) / 100.
000872
000873     COMPUTE PAYMENT = I / VU * (ATF + L-PREMIUM + A-PREMIUM).
000874
000875     COMPUTE PAYMENT = (PAYMENT * 100 + .002) / 100.
000876
000877     COMPUTE TOT-FIN = OTF + L-PREMIUM + A-PREMIUM.
000878
000879     COMPUTE TOT-PAYMT = N * PAYMENT.
000880
000881     COMPUTE TOT-INTRS = TOT-PAYMT - TOT-FIN.
000882
000883 6500-COMMON-DISPLAY.
000884     MOVE TOT-FIN             TO BAMOUNTO.
000885
000886     MOVE L-PREMIUM           TO BLAMTO.
000887     MOVE A-PREMIUM           TO BDAMTO.
000888     MOVE PAYMENT             TO BMOPMTO.
000889     MOVE TOT-PAYMT           TO BTOTPMTO.
000890     MOVE TOT-INTRS           TO BTOTINTO.
000891     MOVE B                   TO BALPMTO.
000892
000893     MOVE -1                  TO ANAMEL.
000894     GO TO 8200-SEND-DATAONLY.
000895
000896     EJECT
000897 7000-BALLOON-CALC.
000898     MOVE WS-BALLOON-AMOUNT   TO B.
000899*    MOVE WS-REGULAR-PAYMENT  TO MP.
000900
000901     COMPUTE V = 1 / (1 + I).
000902
000903     COMPUTE V-NTH = V ** N.
000904
000905     COMPUTE ANGLN = (1 - V-NTH) / I.
000906
000907     COMPUTE ANGLN-LESS-1 = (1 - V-NTH / V) / I.
000908
000909     COMPUTE NM1 = N - 1.
000910
000911     COMPUTE AEQ = N / (N + 1).
000912
000913     IF WS-BALYN = 'Y'
000914         MOVE N TO NM1
000915         MOVE ANGLN TO ANGLN-LESS-1
000916         MOVE 1 TO AEQ.
000917
000918     IF WS-BASIS = 2
000919         GO TO 7500-TP-BALLOON.
000920
000921*    IF MP NOT = ZERO
000922*        GO TO 7200-REGULAR-PAYMENT.
000923
000924     COMPUTE MPT = (N + 1) / 2 * (ATF - B * V-NTH) + L * (1 + EMII
000925                  * I) * B * ANGLN.
000926
000927     COMPUTE MPB = (N + 1) / 2 * (ANGLN-LESS-1 - NM1 * AH) - L
000928                   * (1 + EMII * I) * ((NM1 - ANGLN-LESS-1) / I).
000929
000930     DIVIDE MPT BY MPB GIVING MP.
000931
000932     GO TO 8000-DISPLAY-BALLOON-RESULTS.
000933
000934 7200-REGULAR-PAYMENT.
000935     COMPUTE MPB = (N + 1) / 2 * (ANGLN-LESS-1 - NM1 * AH) - L
000936                    * (1 + EMII * I) * (NM1 - ANGLN-LESS-1) / I.
000937
000938     COMPUTE B = (MPB * MP - (N + 1) / 2 * ATF) / (L * (1 + EMII
000939                   * I) * ANGLN - (N + 1) / 2 * V-NTH).
000940
000941     GO TO 8000-DISPLAY-BALLOON-RESULTS.
000942
000943     EJECT
000944 7500-TP-BALLOON.
000945*    IF MP NOT = ZERO
000946*        GO TO 7600-REGULAR-PAYMENT.
000947
000948     COMPUTE MPT = (ATF - B * V-NTH) + L * B * 2 * N / (N + 1).
000949
000950     COMPUTE MPB = (ANGLN-LESS-1 - NM1 * AH) - L * NM1 * AEQ.
000951
000952     COMPUTE MP = MPT / MPB.
000953
000954     GO TO 7700-SKIP.
000955
000956 7600-REGULAR-PAYMENT.
000957     COMPUTE MPB = (ANGLN-LESS-1 - NM1 * AH) - L * NM1 * AEQ.
000958
000959     COMPUTE B = (MPB * MP - ATF) / (L * 2 * N / (N + 1) - V-NTH).
000960
000961 7700-SKIP.
000962     COMPUTE IA = MP * NM1 + B.
000963
000964     IF WS-BALYN = 'Y'
000965         COMPUTE VA = 1 + B / IA * (N - 1) / (N + 1)
000966      ELSE
000967         COMPUTE VA = N * (IA + B) / (IA * (N + 1)).
000968
000969 8000-DISPLAY-BALLOON-RESULTS.
000970     COMPUTE A-PREMIUM = NM1 * MP * AH.
000971
000972     IF WS-BASIS = 2
000973        COMPUTE L-PREMIUM = L * IA * VA
000974      ELSE
000975        COMPUTE L-PREMIUM = MP * ANGLN-LESS-1 + B * V-NTH - ATF -
000976                A-PREMIUM.
000977
000978** IF BALLOON RATE KEYED, ADD PREMIUM TO LIFE PREMIUM
000979     IF BL GREATER ZERO
000980         COMPUTE L-PREMIUM = L-PREMIUM + (B * BL).
000981
000982     COMPUTE L-PREMIUM = (L-PREMIUM * 100 + .502) / 100.
000983
000984     COMPUTE A-PREMIUM = (A-PREMIUM * 100 + .502) / 100.
000985
000986     COMPUTE TOT-FIN = OTF + L-PREMIUM + A-PREMIUM.
000987
000988     COMPUTE MP = (MP * 100 + .002) / 100.
000989     COMPUTE TOT-PAYMT = MP * NM1 + B.
000990     MOVE MP TO PAYMENT.
000991
000992     COMPUTE TOT-INTRS = TOT-PAYMT - TOT-FIN.
000993
000994     GO TO 6500-COMMON-DISPLAY.
000995
000996     EJECT
000997 8100-SEND-INITIAL-MAP SECTION.
000998     MOVE LOW-VALUES             TO  EL680AI.
000999
001000     MOVE SAVE-DATE              TO  ADATEO.
001001     MOVE EIBTIME                TO  WS-TIME-WORK.
001002     MOVE WS-TIME                TO  ATIMEO.
001003
001004     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
001005
001006     
      * EXEC CICS SEND
001007*        FROM   (EL680AI)
001008*        MAPSET (MAPSET-NAME)
001009*        MAP    (MAP-NAME)
001010*        ERASE
001011*    END-EXEC.
           MOVE LENGTH OF
            EL680AI
             TO DFHEIV11
      *    MOVE '8$      T  E    H L F ,   #00003617' TO DFHEIV0
           MOVE X'382420202020202054202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303033363137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL680AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001012
001013     GO TO 9100-RETURN-TRAN.
001014
001015 8100-EXIT.
001016      EXIT.
001017
001018 8200-SEND-DATAONLY SECTION.
001019     MOVE SAVE-DATE              TO  ADATEO.
001020     MOVE EIBTIME                TO  WS-TIME-WORK.
001021     MOVE WS-TIME                TO  ATIMEO.
001022
001023     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
001024
001025     IF EMI-ERROR = 0
001026        IF EIBAID = DFHPF1
001027           MOVE SPACE            TO EIBAID
001028           PERFORM 9000-START-PRINTER THRU 9000-EXIT
001029           MOVE SPACES           TO SPC-FLD
001030           MOVE SPC-FLD          TO PFENTERI
001031           MOVE 0                TO PFENTERL
001032           MOVE AL-UNNOF         TO PFENTERA.
001033
001034     
      * EXEC CICS SEND DATAONLY
001035*        FROM   (EL680AI)
001036*        MAPSET (MAPSET-NAME)
001037*        MAP    (MAP-NAME)
001038*        CURSOR
001039*    END-EXEC.
           MOVE LENGTH OF
            EL680AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003645' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303033363435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL680AI, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001040
001041     GO TO 9100-RETURN-TRAN.
001042
001043 8200-EXIT.
001044     EXIT.
001045
001046     EJECT
001047 8300-SEND-TEXT SECTION.
001048     
      * EXEC CICS SEND TEXT
001049*        FROM   (LOGOFF-TEXT)
001050*        LENGTH (LOGOFF-LENGTH)
001051*        ERASE  FREEKB
001052*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003659' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303033363539' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001053
001054     
      * EXEC CICS RETURN
001055*    END-EXEC.
      *    MOVE '.(                    ''   #00003665' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001056
001057 8300-EXIT.
001058     EXIT.
001059
001060 8400-NOT-FOUND.
001061     MOVE ER-0190                TO EMI-ERROR.
001062     PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
001063     GO TO 8200-SEND-DATAONLY.
001064
001065 8820-TERMID-ERROR.
001066     MOVE ER-0412                TO EMI-ERROR.
001067     GO TO 8999-OPEN-ERROR.
001068
001069 8830-TRANS-ERROR.
001070     MOVE ER-0413                TO EMI-ERROR.
001071     GO TO 8999-OPEN-ERROR.
001072
001073 8840-CNTL-NOT-OPEN.
001074     MOVE ER-0042 TO EMI-ERROR.
001075     GO TO 8999-OPEN-ERROR.
001076
001077 8800-UNAUTHORIZED-ACCESS.
001078     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
001079     GO TO 8300-SEND-TEXT.
001080
001081 8810-PF23.
001082     MOVE EIBAID                 TO PI-ENTRY-CD-1.
001083     MOVE XCTL-005               TO PGM-NAME.
001084     GO TO 9300-XCTL.
001085
001086 8999-OPEN-ERROR.
001087     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001088     GO TO 8200-SEND-DATAONLY.
001089
001090     EJECT
001091 9000-START-PRINTER.
001092     MOVE PI-COMPANY-ID TO CNTL-COMP-ID.
001093
001094     
      * EXEC CICS HANDLE CONDITION
001095*         NOTOPEN     (8840-CNTL-NOT-OPEN)
001096*         NOTFND      (8400-NOT-FOUND)
001097*         TERMIDERR   (8820-TERMID-ERROR)
001098*         TRANSIDERR  (8830-TRANS-ERROR)
001099*    END-EXEC.
      *    MOVE '"$JI[\                ! # #00003705' TO DFHEIV0
           MOVE X'22244A495B5C202020202020' &
                X'202020202020202020202120' &
                X'2320233030303033373035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001100
001101     MOVE '1'        TO CNTL-REC-TYPE.
001102     MOVE SPACES     TO CNTL-ACCESS.
001103     MOVE +0         TO CNTL-SEQ-NO.
001104
001105     
      * EXEC CICS READ
001106*        DATASET ('ELCNTL')
001107*        SET     (ADDRESS OF CONTROL-FILE)
001108*        RIDFLD  (ELCNTL-KEY)
001109*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00003716' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303033373136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001110
001111     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL' OR 'FNL'
001112*        MOVE EIBTRMID       TO CF-FORMS-PRINTER-ID
001113         MOVE EL680AI        TO PI-PROGRAM-WORK-AREA
001114         
      * EXEC CICS START
001115*             INTERVAL  (0)
001116*             TRANSID   (START-TRANS-ID)
001117*             FROM      (PROGRAM-INTERFACE-BLOCK)
001118*             LENGTH    (PI-COMM-LENGTH)
001119*             TERMID (CF-FORMS-PRINTER-ID)
001120*             END-EXEC
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILF                 1   #00003725' TO DFHEIV0
           MOVE X'3028494C4620202020202020' &
                X'202020202020202020203120' &
                X'2020233030303033373235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
001121     ELSE
001122         
      * EXEC CICS START
001123*            TRANSID(START-TRANS-ID)
001124*            TERMID (CF-FORMS-PRINTER-ID)
001125*            FROM   (EL680AI)
001126*            LENGTH (426)
001127*        END-EXEC.
           MOVE 426
             TO DFHEIV11
      *    MOVE '0( LFT                1   #00003733' TO DFHEIV0
           MOVE X'3028204C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303033373333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 EL680AI, 
                 DFHEIV11, 
                 CF-FORMS-PRINTER-ID, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001128
001129 9000-EXIT.
001130     EXIT.
001131
001132     EJECT
001133 9100-RETURN-TRAN.
001134     MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
001135     MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.
001136     
      * EXEC CICS RETURN
001137*        TRANSID    (TRANS-ID)
001138*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
001139*        LENGTH     (PI-COMM-LENGTH)
001140*    END-EXEC.
      *    MOVE '.(CT                  ''   #00003747' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033373437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001141
001142     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL680' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
001143
001144 9200-RETURN-MAIN-MENU.
001145     MOVE XCTL-626               TO PGM-NAME.
001146     GO TO 9300-XCTL.
001147
001148 9300-XCTL.
001149     
      * EXEC CICS XCTL
001150*        PROGRAM    (PGM-NAME)
001151*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
001152*        LENGTH     (PI-COMM-LENGTH)
001153*    END-EXEC.
      *    MOVE '.$C                   %   #00003760' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033373630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001154
001155 9400-CLEAR.
001156     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
001157     GO TO 9300-XCTL.
001158
001159 9500-PF12.
001160     MOVE XCTL-010               TO PGM-NAME.
001161     GO TO 9300-XCTL.
001162
001163 9600-PGMID-ERROR.
001164     
      * EXEC CICS HANDLE CONDITION
001165*        PGMIDERR    (8300-SEND-TEXT)
001166*    END-EXEC.
      *    MOVE '"$L                   ! $ #00003775' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303033373735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001167
001168     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
001169     MOVE ' '                    TO PI-ENTRY-CD-1.
001170     MOVE XCTL-005               TO PGM-NAME.
001171     MOVE PGM-NAME               TO LOGOFF-PGM.
001172     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
001173     GO TO 9300-XCTL.
001174
001175 9700-LINK-DATE-CONVERT.
001176     
      * EXEC CICS LINK
001177*        PROGRAM    ('ELDATCV')
001178*        COMMAREA   (DATE-CONVERSION-DATA)
001179*        LENGTH     (DC-COMM-LENGTH)
001180*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00003787' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033373837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001181
001182 9700-EXIT.
001183     EXIT.
001184
001185 9900-ERROR-FORMAT.
001186     IF NOT EMI-ERRORS-COMPLETE
001187         MOVE LINK-001           TO PGM-NAME
001188         
      * EXEC CICS LINK
001189*            PROGRAM    (PGM-NAME)
001190*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
001191*            LENGTH     (EMI-COMM-LENGTH)
001192*        END-EXEC.
      *    MOVE '."C                   (   #00003799' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033373939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001193
001194 9900-EXIT.
001195     EXIT.
001196
001197 9990-ABEND.
001198     MOVE LINK-004               TO PGM-NAME.
001199     MOVE DFHEIBLK               TO EMI-LINE1.
001200
001201     
      * EXEC CICS LINK
001202*        PROGRAM   (PGM-NAME)
001203*        COMMAREA  (EMI-LINE1)
001204*        LENGTH    (72)
001205*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003812' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033383132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001206
001207     GO TO 8200-SEND-DATAONLY.
001208

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL680' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8840-CNTL-NOT-OPEN,
                     8400-NOT-FOUND,
                     8820-TERMID-ERROR,
                     8830-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL680' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
