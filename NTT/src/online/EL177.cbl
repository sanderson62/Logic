      *((program: EL177.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL177.
000004*                            VMOD=1.7.
000005
000006 AUTHOR. LOGIC, INC.
000007            DALLAS, TEXAS.
000008
000009 DATE-WRITTEN. AUGUST, 1981.
000010
000011*SECURITY.   *****************************************************
000012*            **                                                  *
000013*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000014*            *                                                   *
000015*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000016*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000017*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
000018*            *                                                   *
000019*            *****************************************************
000020*
000021*
000022*REMARKS.
000023******************************************************************
000024*                   C H A N G E   L O G
000025*
000026* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000027*-----------------------------------------------------------------
000028*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000029* EFFECTIVE    NUMBER
000030*-----------------------------------------------------------------
000031****************************************************************
000032*****     THIS PROGRAM WAS DIRECTLY COPIED FROM CL177.     *****
000033*****     ALL CHANGES HAVE BEEN FLAGGED WITH LGC007 AT     *****
000034*****     THE FRONT OF THE STATEMENT.                      *****
000035****************************************************************
000036* CASB  02/02/94 - ADDED CODE SO THAT WHEN A DRAFT AMOUNT IS
000037*                  OVER $10,000.00 IT WILL NOT PRINT BILL
000038*                  KIZER'S SIGNATURE. THIS IS ACCOMPLISHED
000039*                  MY PLACING A VALUE OF 'C' IN THE
000040*                  M420C-SIGNATURE FIELD.
000041* JWBA  03/02/94 - CHG  CODE SO THAT IF CID DRAFT AMOUNT IS
000042*                  OVER $100,000.00 THEN THE CHECK MUST BE
000043*                  SIGNED BY W.M. KIZER.
000044* DJNA  04/01/00 - CR#2000030100009 DRAFT NUMBER EXPANSION.
000045* DANA  05/12/00 - CR#2000021500004 NEW MESSAGES ON CHECK
000046* 102902    2001061800003  PEMA  ADD DCC TO MICR PROCESSING
000047* 121903    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
000048*           2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
000049*           2002100700001  SMVA  ADD ACCT STATE AND CLM TYPE FOR P
000050*                                STATE SPEC PROGRESS RPTS
000051* 011105    2004071200002  PEMA  ADD ONE COMMENT LINE
000052* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000053* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000054* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000055* 080322  CR2021100800003  TANA  Add B and H claim types
000056****************************************************************
000057
000058*        THIS FUNCTION READS THE CHECK QUEUE FILE AND PRINTS
000059*    CHECKS FROM THOSE ENTRIES QUEUED.  QUEUEING IS DONE USING
000060*    TIME (HH.MM) BY THE CHECK PRINT RELEASE PROGRAM (EL176).
000061*
000062*    SCREENS     - NONE - USERS PRINTED OUTPUT (CHECKS)
000063*
000064*    ENTERED BY  - EL176  - CHECK WRITER - VIA START
000065*
000066*    EXIT TO     - CICS
000067*
000068*    INPUT FILES - NONE
000069*
000070*    OUTPUT FILES - NONE
000071*
000072*    COMMAREA    - PASSED.
000073*
000074     EJECT
000075 ENVIRONMENT DIVISION.
000076
000077 DATA DIVISION.
000078
000079 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000080
000081 77  FILLER       PIC X(32)  VALUE '**************************'.
000082 77  FILLER       PIC X(32)  VALUE '*   EL177 WORKING STORAGE '.
000083 77  FILLER       PIC X(32)  VALUE '*********** V/M 1.7 ******'.
000084*
000085 77  PI-PRINT-REPORT     PIC X(08)  VALUE SPACES.
000086 77  PI-PRINT-ID         PIC X(04)  VALUE SPACES.
000087 77  WS-FIRST-TIME-SWA   PIC X VALUE 'Y'.
000088 77  WS-FIRST-TIME-SWB   PIC X VALUE 'Y'.
000089
000090
000091 01  WS-COMMON-PRINT.
000092     05  THIS-PGM                   PIC X(8)   VALUE 'EL177   '.
000093     05  WS-BLANK                   PIC X      VALUE ' '.
000094     05  WS-I                       PIC X      VALUE 'I'.
000095     05  WS-F                       PIC X      VALUE 'F'.
000096     05  WS-R                       PIC X      VALUE 'R'.
000097     05  WS-L                       PIC X      VALUE 'L'.
000098
000099 01  WS-FINAL-MESS-CONSTANT.
000100     05  WS-LINE-9                  PIC X(17)  VALUE
000101         'IF YOU HAVE ANY  '.
000102     05  WS-LINE-10                 PIC X(17)  VALUE
000103         'QUESTIONS, PLEASE'.
000104     05  WS-LINE-11                 PIC X(17)  VALUE
000105         'LET US KNOW.     '.
000106
000107     05  WS-TP-RECORD-AREA.
000108         10  WS-TP-ACCOUNT-NO        PIC X(6).
000109         10  WS-TP-REFENCE-NO        PIC X(6).
000110
000111     05  WS-TP-ADDRESS-AREA.
000112         10  WS-TP-REFNCE-NO         PIC X(6).
000113         10  WS-TP-ADD-LINE1         PIC X(40).
000114         10  WS-TP-ADD-LINE2         PIC X(40).
000115         10  WS-TP-ADD-LINE3         PIC X(40).
000116
000117 01  FILLER                          COMPUTATIONAL-3.
000118
000119     05  WS-RECORD-COUNT             PICTURE S9(5)   VALUE ZERO.
000120     05  WS-TIME-WORK                PICTURE S9(7)   VALUE ZERO.
000121     05  WS-TIME                     REDEFINES
000122         WS-TIME-WORK                PICTURE S9(3)V9(4).
000123     05  WS-HHMM                     REDEFINES
000124         WS-TIME-WORK                PICTURE S9(5)V99.
000125
000126     05  WS-DELAY-INTERVAL           PICTURE S9(7)   VALUE +10.
000127
000128     05  WS-DATA-SENT-SW             PICTURE S9      VALUE ZERO.
000129     05  WS-SW                       PICTURE S9      VALUE ZERO.
000130
000131 01  FILLER                          COMPUTATIONAL
000132                                     SYNCHRONIZED.
000133
000134     05  WS-TS-LENGTH                PICTURE S9(4)   VALUE +1158.
000135     05  WS-COMM-LENGTH              PICTURE S9(4)   VALUE +1024.
000136     05  WS-CHECK-LINES-LENGTH       PICTURE S9(4)   VALUE +904.
000137     05  WS-FIA-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +1344.
000138     05  WS-CSO-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +855.
000139     05  WS-OLI-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +1230.
000140     05  WS-POS-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +1163.
000141     05  WS-TCL-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +666.
000142 01  CSO-PRINT-STARTED-SW        PIC X VALUE 'Y'.
000143 01  WS-SOC-SEC-NO               PICTURE 9(18)   VALUE ZEROS.
000144 01  WS-CSO-VOID-LINE-1          PICTURE X(11)
000145     VALUE '** VOID ** '.
000146 01  WS-CSO-VOID-LINE-2          PICTURE X(30)
000147     VALUE '** VOID **** VOID **** VOID **'.
000148 01  WS-DRAFT-ORDER              PIC 9(5)        VALUE ZEROS.
000149 01  WS-PLAN-CODE-AREA.
000150     05  WS-IND-GRP-TYPE         PICTURE X       VALUE SPACE.
000151     05  WS-BENEFIT-TYPE         PICTURE 99      VALUE ZEROS.
000152 01  WS-CHECK-AREA.
000153     05  COMPANY-CHECK           PICTURE X(2)    VALUE SPACE.
000154     05  FILLER                  PICTURE X(6)    VALUE SPACE.
000155 01  WS-COMPANY-NAME             PICTURE X(43)   VALUE SPACE.
000156 01  WS-COMPANY-NAME2            PICTURE X(43)   VALUE SPACE.
000157 01  CSO-COMPANY-HEADINGS.
000158     05  01-HEADING              PICTURE X(43)
000159         VALUE ' CENTRAL STATES HEALTH & LIFE CO. OF OMAHA '.
000160     05  02-HEADING              PICTURE X(43)
000161         VALUE '   CENTRAL STATES INDEMNITY CO. OF OMAHA   '.
000162     05  03-HEADING              PICTURE X(43)
000163         VALUE '        NATIONAL INDEMNITY COMPANY         '.
000164     05  04-HEADING              PICTURE X(43)
000165         VALUE ' MASSACHUSETTS INDEMNITY AND LIFE COMPANY  '.
000166     05  05-HEADING              PICTURE X(43)
000167         VALUE '        COLUMBIA INSURANCE COMPANY         '.
000168     05  06-HEADING              PICTURE X(43)
000169         VALUE '  NATIONAL FIRE AND MARINE INSURANCE CO.   '.
000170 01  COMPANY-NAMES.
000171     05  01-COMP-NAME            PICTURE X(43)
000172         VALUE ' CENTRAL STATES HEALTH & LIFE CO. OF OMAHA '.
000173     05  02-COMP-NAME            PICTURE X(43)
000174         VALUE 'CENTRAL STATES INDEMNITY CO. OF OMAHA      '.
000175     05  03-COMP-NAME            PICTURE X(43)
000176         VALUE 'NATIONAL INDEMNITY COMPANY                 '.
000177     05  04-COMP-NAME            PICTURE X(43)
000178         VALUE 'MASSACHUSETTS INDEMNITY AND LIFE COMPANY   '.
000179     05  05-COMP-NAME            PICTURE X(43)
000180         VALUE 'COLUMBIA INSURANCE COMPANY                 '.
000181     05  06-COMP-NAME            PICTURE X(43)
000182         VALUE 'NATIONAL FIRE AND MARINE INSURANCE CO.     '.
000183
000184 01  FILLER.
000185
000186     05  S-MSG                       PICTURE X(37)  VALUE
000187              'FULL SETTLEMENT OF ANY AND ALL CLAIMS'.
000188     05  M-MSG                       PICTURE X(37)  VALUE
000189              '  MEDICAL EXPENSE                    '.
000190     05  L-MSG                       PICTURE X(37)  VALUE
000191              '  LEGAL EXPENSE                      '.
000192     05  I-MSG                       PICTURE X(37)  VALUE
000193              '  INVESTIGATION EXPENSE              '.
000194
000195     05  WS-BIN-CURRENT-DT           PICTURE XX     VALUE SPACES.
000196     05  WS-EDT-CURRENT-DT           PICTURE X(8)   VALUE SPACES.
000197     05  WS-PROGRAM-ID               PICTURE X(8)   VALUE 'EL177'.
000198
000199     05  WS-TEXT-MESSAGE-LENGTH      PICTURE S9(4)   VALUE +70
000200                                     COMPUTATIONAL
000201                                     SYNCHRONIZED.
000202
000203     05  WS-TEXT-MESSAGE             PICTURE X(70)   VALUE SPACES.
000204
000205     05  WS-ZIP-CODE-LINE                            VALUE SPACES.
000206         10  WS-ZIP-CHAR             PICTURE X
000207             OCCURS 133 TIMES        INDEXED BY ZIP-INDEX1
000208                                                ZIP-INDEX2
000209                                                ZIP-INDEX3.
000210
000211     05  WS-DUMP-CODE.
000212         10  FILLER                  PICTURE X       VALUE 'S'.
000213         10  WS-DUMP-COUNT           PICTURE 999     VALUE ZERO.
000214     05  WS-CPA-COMMENT.
000215         10  WS-CPA-COMMENT-P1       PIC X(29).
000216         10  WS-CPA-VOID-INDICATOR   PIC X.
000217
000218     EJECT
000219 01  420C-MSG1.
000220     05  PIC X(35) VALUE 'A payment for benefits was made to '.
000221     05  PIC X(35) VALUE 'your account today.  Since more    '.
000222     05  PIC X(35) VALUE 'benefits may be payable in the futu'.
000223     05  PIC X(35) VALUE 're, we are enclosing another claim '.
000224     05  PIC X(35) VALUE 'form.  This form must be completed '.
000225     05  PIC X(35) VALUE 'IN FULL and sent to us when you    '.
000226     05  PIC X(35) VALUE 'return to work or on       whicheve'.
000227     05  PIC X(35) VALUE 'r is sooner.  Thank you for your   '.
000228     05  PIC X(35) VALUE 'cooperation.                       '.
000229     05  PIC X(35) VALUE '                                   '.
000230 01  REDEFINES 420C-MSG1.
000231     05  420C-MSG1-LINE OCCURS 5 TIMES PIC X(70).
000232
000233 01  420C-MSG2.
000234     05  PIC X(35) VALUE 'This is the last benefit payment fo'.
000235     05  PIC X(35) VALUE 'r this period of disability under  '.
000236     05  PIC X(35) VALUE 'this claim on your credit insurance'.
000237     05  PIC X(35) VALUE ' policy.  Please check with your   '.
000238     05  PIC X(35) VALUE 'financial institution within the ne'.
000239     05  PIC X(35) VALUE 'xt week to make sure it was        '.
000240     05  PIC X(35) VALUE 'credited to your account.          '.
000241     05  PIC X(35) VALUE '                                   '.
000242 01  REDEFINES 420C-MSG2.
000243     05  420C-MSG2-LINE OCCURS 4 TIMES PIC X(70).
000244
000245 01  420C-MSG3.
000246     05  PIC X(35) VALUE 'A claim payment was made on your ac'.
000247     05  PIC X(35) VALUE 'count today.  Please check with    '.
000248     05  PIC X(35) VALUE 'your financial institution within t'.
000249     05  PIC X(35) VALUE 'he next week to make sure it was   '.
000250     05  PIC X(35) VALUE 'credited to your account.          '.
000251     05  PIC X(35) VALUE '                                   '.
000252 01  REDEFINES 420C-MSG3.
000253     05  420C-MSG3-LINE OCCURS 3 TIMES PIC X(70).
000254
000255 01  420C-MSG4.
000256     05  PIC X(35) VALUE 'This payment represents the total b'.
000257     05  PIC X(35) VALUE 'enefits payable under this credit  '.
000258     05  PIC X(35) VALUE 'life insurance policy.  This claim '.
000259     05  PIC X(35) VALUE 'has been closed.                   '.
000260 01  REDEFINES 420C-MSG4.
000261     05  420C-MSG4-LINE OCCURS 2 TIMES PIC X(70).
000262
000263 01  420C-MSG5.
000264     05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
000265     05  PIC X(35) VALUE 'e credit life insurance was        '.
000266     05  PIC X(35) VALUE 'purchased to protect this loan, we '.
000267     05  PIC X(35) VALUE 'have paid the loan off at the      '.
000268     05  PIC X(35) VALUE 'financial institution.  This paymen'.
000269     05  PIC X(35) VALUE 't to the estate represents the     '.
000270     05  PIC X(35) VALUE 'remaining benefit amount available '.
000271     05  PIC X(35) VALUE 'under this policy.  This claim has '.
000272     05  PIC X(35) VALUE 'been closed.                       '.
000273     05  PIC X(35) VALUE '                                   '.
000274 01  REDEFINES 420C-MSG5.
000275     05  420C-MSG5-LINE OCCURS 5 TIMES PIC X(70).
000276
000277 01  DCC1-MSG1.
000278     05  PIC X(35) VALUE 'A payment was made to your account '.
000279     05  PIC X(35) VALUE 'today.  Please check with your     '.
000280     05  PIC X(35) VALUE 'financial institution within the ne'.
000281     05  PIC X(35) VALUE 'xt week to make sure it was        '.
000282     05  PIC X(35) VALUE 'credited to your account.  Since mo'.
000283     05  PIC X(35) VALUE 're benefits may be payable in the  '.
000284     05  PIC X(35) VALUE 'future, we are enclosing another fo'.
000285     05  PIC X(35) VALUE 'rm.  This form must be completed   '.
000286     05  PIC X(35) VALUE 'IN FULL and sent to us when you ret'.
000287     05  PIC X(35) VALUE 'urn to work or on       whichever  '.
000288     05  PIC X(35) VALUE 'is sooner.  Thank you for your coop'.
000289     05  PIC X(35) VALUE 'eration.                           '.
000290 01  REDEFINES DCC1-MSG1.
000291     05  DCC1-MSG1-LINE OCCURS 6 TIMES PIC X(70).
000292
000293 01  DCC1-MSG2.
000294     05  PIC X(35) VALUE 'This is the last benefit payment fo'.
000295     05  PIC X(35) VALUE 'r this period of disability under  '.
000296     05  PIC X(35) VALUE 'this file on your debt protection a'.
000297     05  PIC X(35) VALUE 'ddendum.  Please check with your   '.
000298     05  PIC X(35) VALUE 'financial institution within the ne'.
000299     05  PIC X(35) VALUE 'xt week to make sure it was        '.
000300     05  PIC X(35) VALUE 'credited to your account properly. '.
000301     05  PIC X(35) VALUE '                                   '.
000302 01  REDEFINES DCC1-MSG2.
000303     05  DCC1-MSG2-LINE OCCURS 4 TIMES PIC X(70).
000304
000305 01  DCC1-MSG3.
000306     05  PIC X(35) VALUE 'A payment was made on your account '.
000307     05  PIC X(35) VALUE 'today. Please check with your      '.
000308     05  PIC X(35) VALUE 'financial institution within the ne'.
000309     05  PIC X(35) VALUE 'xt week to make sure it was        '.
000310     05  PIC X(35) VALUE 'credited to your account properly. '.
000311     05  PIC X(35) VALUE '                                   '.
000312 01  REDEFINES DCC1-MSG3.
000313     05  DCC1-MSG3-LINE OCCURS 3 TIMES PIC X(70).
000314
000315 01  DCC1-MSG4.
000316     05  PIC X(35) VALUE 'This payment represents the total b'.
000317     05  PIC X(35) VALUE 'enefits payable under your debt    '.
000318     05  PIC X(35) VALUE 'protection addendum.  This file has'.
000319     05  PIC X(35) VALUE ' been closed.                      '.
000320 01  REDEFINES DCC1-MSG4.
000321     05  DCC1-MSG4-LINE OCCURS 2 TIMES PIC X(70).
000322
000323 01  DCC1-MSG5.
000324     05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
000325     05  PIC X(35) VALUE 'e debt protection coverage was     '.
000326     05  PIC X(35) VALUE 'purchased to protect this loan, we '.
000327     05  PIC X(35) VALUE 'have made a payment to the         '.
000328     05  PIC X(35) VALUE 'financial institution.  The file ha'.
000329     05  PIC X(35) VALUE 'been closed.                       '.
000330 01  REDEFINES DCC1-MSG5.
000331     05  DCC1-MSG5-LINE OCCURS 3 TIMES PIC X(70).
000332
000333     EJECT
000334*                            COPY ELCINTF.
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
000335*           COPY ELC176PI.
      *>>((file: ELC176PI))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELC176PI.                          *
000005*                            VMOD=2.005                         *
000006*****************************************************************.
000007
000008     12  FILLER                      REDEFINES
000009         PI-PROGRAM-WORK-AREA.
000010
000011         16  PI-TEMP-STORAGE-KEY.
000012             20  PI-TSK-TERM-ID      PIC X(4).
000013             20  PI-TSK-TIME         PIC S9(7)     COMP-3.
000014
000015         16  PI-PROCESSING-SW        PIC S9        COMP-3.
000016
000017         16  PI-NUMBER-OF-ALIGNMENT-CHECKS
000018                                     PIC S9        COMP-3.
000019         16  PI-ALIGNMENT-CONTROL-GROUP
000020                                     PIC S9(8)     COMP-3.
000021         16  PI-ALIGNMENT-SEQUENCE-NO
000022                                     PIC S9(4)     COMP-3.
000023         16  PI-NUMBER-OF-CONTROL-GROUPS
000024                                     PIC S9(4)     COMP-3.
000025
000026         16  PI-CONTROL-GROUPS                     COMP
000027             OCCURS 4 TIMES          INDEXED BY PI-INDEX.
000028             20  PI-CONTROL-GROUP    PIC S9(8).
000029             20  PI-HIGH-SEQUENCE    PIC S9(4).
000030
000031         16  PI-CHECK-PRINTER-ID     PIC X(4).
000032
000033         16  PI-PRINTER-STARTED-SW   PIC S9        COMP-3.
000034
000035         16  PI-ASSIGN-CHECK-NUMBERS PIC X.
000036
000037         16  PI-COMPANY-ADDRESS.
000038             20  PI-COMPANY-NAME             PIC X(30).
000039             20  PI-COMPANY-ADDRESS-LINE1    PIC X(30).
000040             20  PI-COMPANY-ADDRESS-LINE2    PIC X(30).
000041             20  PI-COMPANY-ADDRESS-LINE3    PIC X(30).
000042             20  PI-COMPANY-CITY-ST          PIC X(30).
000043             20  PI-COMPANY-ZIP.
000044                 24  PI-COMPANY-ZIP-CODE.
000045                     28  PI-COMPANY-ZIP-1ST  PIC X.
000046                         88  PI-COMPANY-CAN-POST-CODE
000047                                             VALUE 'A' THRU 'Z'.
000048                     28  FILLER              PIC X(4).
000049                 24  PI-COMPANY-ZIP-PLUS4    PIC X(4).
000050             20  PI-CO-CANADIAN-POSTAL-CODE
000051                     REDEFINES PI-COMPANY-ZIP.
000052                 24  PI-CO-CAN-POSTAL-1      PIC XXX.
000053                 24  PI-CO-CAN-POSTAL-2      PIC XXX.
000054                 24  FILLER                  PIC XXX.
000055             20  PI-COMPANY-PHONE-NUMBER     PIC S9(11) COMP-3.
000056
000057         16  PI-MONTH-END-SAVE               PIC XX.
000058
000059         16  PI-VALID-RCD-SW                 PIC X.
000060
000061         16  FILLER                          PIC X(419).
000062
      *<<((file: ELC176PI))
000336
000337         16  PI-TEMP-STORAGE-ITEM    PICTURE S9(4)
000338                                     COMPUTATIONAL
000339                                     SYNCHRONIZED.
000340
000341     EJECT
000342*                           COPY ELC176W2.
      *>>((file: ELC176W2))
000001*****************************************************************
000002*                                                               *
000003*                            ELC176W2.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                         *
000006*****************************************************************.
000007
000008 01  SPELL-DOLLAR-PASS-AREA.
000009
000010     05  SD-PASS-AMOUNT              PIC S9(9)V99
000011                                     COMP-3        VALUE ZEROS.
000012
000013     05  SD-PASS-SPELLED-AMOUNT      PIC X(200)    VALUE SPACES.
000014
000015     05  FILLER.
000016         10  SD-PSA-LINE1            PIC X(100)    VALUE SPACES.
000017         10  SD-PSA-LINE2            PIC X(100)    VALUE SPACES.
000018
      *<<((file: ELC176W2))
000343
000344     EJECT
000345*                           COPY ELC176W1.
      *>>((file: ELC176W1))
000001*****************************************************************
000002*                                                               *
000003*                            ELC176W1.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.004                         *
000006*****************************************************************.
000007
000008 01  FILLER.
000009     05  WS-1ST-LINE-LENGTH          PIC S9(4)   VALUE +77
000010                                     COMP
000011                                     SYNC.
000012
000013     05  WS-1ST-LINE-LENGTH-PLUS-1   PIC S9(4)   VALUE +78
000014                                     COMP
000015                                     SYNC.
000016
000017     05  WS-1ST-LINE-LENGTH-PLUS-2   PIC S9(4)   VALUE +79
000018                                     COMP
000019                                     SYNC.
000020
000021     05  WS-1ST-LINE-LENGTH-MINUS-1  PIC S9(4)   VALUE +76
000022                                     COMP
000023                                     SYNC.
000024
000025     05  WS-2ND-LINE-LENGTH          PIC S9(4)   VALUE +48
000026                                     COMP
000027                                     SYNC.
000028
000029     05  WS-AMOUNT                   PIC 9(9)V99 VALUE ZEROS.
000030
000031     05  FILLER                      REDEFINES
000032         WS-AMOUNT.
000033         10  WS-MILLIONS             PIC 9(3).
000034         10  WS-THOUSANDS            PIC 9(3).
000035         10  WS-HUNDREDS             PIC 9(3).
000036
000037         10  WS-CENTS                PIC 99.
000038         10  WS-CENTS-X              REDEFINES
000039             WS-CENTS                PIC XX.
000040
000041     05  WS-AMOUNT-WORK              PIC 9(3)    VALUE ZEROS.
000042
000043     05  FILLER                      REDEFINES
000044         WS-AMOUNT-WORK.
000045         10  WS-HUNDRED              PIC 9.
000046         10  WS-TEEN                 PIC 99.
000047
000048         10  FILLER                  REDEFINES
000049             WS-TEEN.
000050
000051             15  WS-TEN              PIC 9.
000052             15  WS-ONE              PIC 9.
000053
000054     05  WS-SPELLED-AMOUNT           PIC X(200)  VALUE SPACES.
000055
000056     05  WS-CHAR                     REDEFINES
000057         WS-SPELLED-AMOUNT           PIC X
000058         OCCURS 200 TIMES            INDEXED BY SA-INDEX
000059                                                SA-INDEX2.
000060
000061     05  WS-SPELLED-LINE1            PIC X(100)  VALUE SPACES.
000062
000063     05  WS-SL1                      REDEFINES
000064         WS-SPELLED-LINE1            PIC X
000065         OCCURS 100 TIMES            INDEXED BY SL1-INDEX.
000066
000067     05  WS-SPELLED-LINE2            PIC X(100)  VALUE SPACES.
000068
000069     05  WS-SL2                      REDEFINES
000070         WS-SPELLED-LINE2            PIC X
000071         OCCURS 100 TIMES            INDEXED BY SL2-INDEX.
000072
000073     05  WS-WORD                     PIC X(21)   VALUE SPACES.
000074
000075     05  WS-CHAR2                    REDEFINES
000076         WS-WORD                     PIC X
000077         OCCURS 21 TIMES             INDEXED BY CHAR-INDEX.
000078
000079     EJECT
000080     05  WS-SINGLE-AREA.
000081         10  FILLER    PIC X(21)  VALUE 'ONE'.
000082         10  FILLER    PIC X(21)  VALUE 'TWO'.
000083         10  FILLER    PIC X(21)  VALUE 'THREE'.
000084         10  FILLER    PIC X(21)  VALUE 'FOUR'.
000085         10  FILLER    PIC X(21)  VALUE 'FIVE'.
000086         10  FILLER    PIC X(21)  VALUE 'SIX'.
000087         10  FILLER    PIC X(21)  VALUE 'SEVEN'.
000088         10  FILLER    PIC X(21)  VALUE 'EIGHT'.
000089         10  FILLER    PIC X(21)  VALUE 'NINE'.
000090         10  FILLER    PIC X(21)  VALUE 'TEN'.
000091         10  FILLER    PIC X(21)  VALUE 'ELEVEN'.
000092         10  FILLER    PIC X(21)  VALUE 'TWELVE'.
000093         10  FILLER    PIC X(21)  VALUE 'THIRTEEN'.
000094         10  FILLER    PIC X(21)  VALUE 'FOURTEEN'.
000095         10  FILLER    PIC X(21)  VALUE 'FIFTEEN'.
000096         10  FILLER    PIC X(21)  VALUE 'SIXTEEN'.
000097         10  FILLER    PIC X(21)  VALUE 'SEVENTEEN'.
000098         10  FILLER    PIC X(21)  VALUE 'EIGHTEEN'.
000099         10  FILLER    PIC X(21)  VALUE 'NINETEEN'.
000100
000101     05  WS-SINGLE-DESC              REDEFINES
000102         WS-SINGLE-AREA              PIC X(21)
000103         OCCURS 19 TIMES             INDEXED BY SINGLE-INDEX.
000104
000105     05  WS-UPPER-AREA.
000106         10  FILLER    PIC X(21)  VALUE SPACES.
000107         10  FILLER    PIC X(21)  VALUE 'TWENTY'.
000108         10  FILLER    PIC X(21)  VALUE 'THIRTY'.
000109         10  FILLER    PIC X(21)  VALUE 'FORTY'.
000110         10  FILLER    PIC X(21)  VALUE 'FIFTY'.
000111         10  FILLER    PIC X(21)  VALUE 'SIXTY'.
000112         10  FILLER    PIC X(21)  VALUE 'SEVENTY'.
000113         10  FILLER    PIC X(21)  VALUE 'EIGHTY'.
000114         10  FILLER    PIC X(21)  VALUE 'NINETY'.
000115
000116     05  WS-UPPER-DESC               REDEFINES
000117         WS-UPPER-AREA               PIC X(21)
000118         OCCURS 9 TIMES              INDEXED BY UPPER-INDEX.
000119
000120     05  WS-DOLLARS-AND-CENTS        PIC X(21)   VALUE
000121         'DOLLARS-AND-XX-CENTS'.
000122
000123     05  FILLER                      REDEFINES
000124         WS-DOLLARS-AND-CENTS.
000125
000126         10  FILLER                  PIC X(12).
000127         10  WS-PENNEYS              PIC XX.
000128         10  FILLER                  PIC X(07).
000129
      *<<((file: ELC176W1))
000346
000347     EJECT
000348*                           COPY ELC176W3.
      *>>((file: ELC176W3))
000001*****************************************************************
000002*                                                               *
000003*                            ELC176W3.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.002                         *
000006*****************************************************************.
000007
000008 01  CHECK-PRINT-LINES.
000009
000010     05  CHECK-PRINT-LINE-1.
000011         10  FILLER                     PIC X       VALUE '1'.
000012
000013     05  CHECK-PRINT-LINE-3.
000014         10  CPL3-CARRIAGE-CONTROL      PIC X       VALUE '-'.
000015         10  FILLER                     PIC X(50)   VALUE SPACES.
000016         10  CPL3-CHECK-NUMBER-DESC     PIC X(10)
000017                                            VALUE 'CHECK NO.'.
000018         10  CPL3-CHECK-NUMBER          PIC X(7)    VALUE ALL 'X'.
000019
000020     05  CHECK-PRINT-LINE-6.
000021         10  FILLER                     PIC X       VALUE '-'.
000022
000023     05  CHECK-PRINT-LINE-9.
000024         10  FILLER                     PIC X(9)    VALUE '-'.
000025         10  CPL9-SPELLED-AMOUNT-LINE1  PIC X(77)   VALUE ALL 'X'.
000026
000027     05  CHECK-PRINT-LINE-10.
000028         10  FILLER                     PIC X(9)    VALUE SPACES.
000029         10  CPL10-SPELLED-AMOUNT-LINE2 PIC X(45)   VALUE ALL 'X'.
000030         10  FILLER                     PIC X(4)    VALUE SPACES.
000031         10  CPL10-CLAIM-NO             PIC X(7)    VALUE ALL 'X'.
000032         10  FILLER                     PIC X(9)    VALUE SPACES.
000033         10  CPL10-CERT-NO              PIC X(11)   VALUE ALL 'X'.
000034
000035     05  CHECK-PRINT-LINE-13.
000036         10  FILLER                     PIC X(12)   VALUE '-'.
000037         10  CPL13-DATA.
000038             15  CPL13-NAME             PIC X(30)   VALUE ALL 'X'.
000039             15  FILLER                 PIC X(15)   VALUE SPACES.
000040             15  CPL13-CHECK-DATE       PIC X(8)
000041                                            VALUE 'XX/XX/XX'.
000042             15  FILLER                 PIC X(2)    VALUE SPACES.
000043             15  CPL13-CHECK-AMOUNT     PIC $$,$$$,$$9.99-.
000044
000045     05  CHECK-PRINT-LINE-14.
000046         10  FILLER                     PIC X(12)   VALUE SPACES.
000047         10  CPL14-ADDRESS-LINE1        PIC X(30)   VALUE ALL 'X'.
000048
000049     05  CHECK-PRINT-LINE-15.
000050         10  FILLER                     PIC X(12)   VALUE SPACES.
000051         10  CPL15-ADDRESS-LINE2        PIC X(30)   VALUE ALL 'X'.
000052         10  FILLER                     PIC X(12)   VALUE SPACES.
000053         10  CPL15-VOID-MESSAGE1        PIC X(30)   VALUE
000054             '*** VOID *** VOID *** VOID ***'.
000055
000056     05  CHECK-PRINT-LINE-16.
000057         10  FILLER                     PIC X(12)   VALUE SPACES.
000058         10  CPL16-ADDRESS-LINE3        PIC X(30)   VALUE ALL 'X'.
000059         10  FILLER                     PIC X(12)   VALUE SPACES.
000060         10  CPL16-VOID-MESSAGE2        PIC X(30)   VALUE
000061             '*** VOID *** VOID *** VOID ***'.
000062
000063     05  CHECK-PRINT-LINE-17.
000064         10  FILLER                     PIC X(12)   VALUE SPACES.
000065         10  CPL17-ADDRESS-LINE4        PIC X(30)   VALUE ALL 'X'.
000066         10  FILLER                     PIC X(12)   VALUE SPACES.
000067         10  CPL17-VOID-MESSAGE3        PIC X(30)   VALUE
000068             '*** VOID *** VOID *** VOID ***'.
000069
000070     05  CHECK-PRINT-LINE-18.
000071         10  FILLER                     PIC X(11)   VALUE SPACES.
000072         10  CPL18-ADDRESS-LINE5.
000073             15  FILLER                 PIC X(21)   VALUE SPACES.
000074             15  CPL18-ZIP-CODE         PIC X(10)   VALUE SPACES.
000075
000076     05  CHECK-PRINT-LINE-21            PIC X       VALUE '-'.
000077
000078     05  CHECK-PRINT-LINE-23            PIC X       VALUE '0'.
000079
000080     05  CHECK-PRINT-LINE-25.
000081         10  FILLER                     PIC X(6)    VALUE '0'.
000082         10  CPL25-INSURED-NAME         PIC X(30)   VALUE ALL 'X'.
000083         10  FILLER                     PIC X(3)    VALUE SPACES.
000084         10  CPL25-CLAIM-TYPE           PIC X(10)   VALUE ALL 'X'.
000085         10  FILLER                     PIC X       VALUE SPACES.
000086         10  CPL25-PAYMENT-TYPE         PIC X(15)   VALUE ALL 'X'.
000087         10  FILLER                     PIC X(2)    VALUE SPACES.
000088         10  CPL25-BY                   PIC X(4)    VALUE ALL 'X'.
000089         10  FILLER                     PIC X(2)    VALUE SPACES.
000090         10  CPL25-PAID-TO-DATE         PIC Z,ZZZ,ZZ9.99-.
000091
000092     05  CHECK-PRINT-LINE-28.
000093         10  FILLER                     PIC X(5)    VALUE '-'.
000094         10  CPL28-STATE                PIC X(2)    VALUE ALL 'X'.
000095         10  FILLER                     PIC X(6)    VALUE SPACES.
000096         10  CPL28-ACCOUNT              PIC X(10)   VALUE ALL 'X'.
000097         10  FILLER                     PIC X       VALUE SPACES.
000098         10  CPL28-CERT-NO              PIC X(11)   VALUE ALL 'X'.
000099         10  FILLER                     PIC XX      VALUE SPACES.
000100         10  CPL28-CERT-EFF-DATE        PIC X(8)
000101                                            VALUE 'XX/XX/XX'.
000102         10  FILLER                     PIC X(5)    VALUE SPACES.
000103         10  CPL28-CLAIM-CODE           PIC X       VALUE ALL 'X'.
000104         10  FILLER                     PIC X(2)    VALUE SPACES.
000105         10  CPL28-INCURRED-DATE        PIC X(8)
000106                                            VALUE 'XX/XX/XX'.
000107         10  FILLER              REDEFINES
000108             CPL28-INCURRED-DATE.
000109             15  FILLER                 PIC X(6).
000110             15  CPL28-ID-YR1           PIC X.
000111             15  CPL28-ID-YR2           PIC X.
000112         10  CPL28-REPORTED-DATE        PIC X(8)
000113                                            VALUE 'XX/XX/XX'.
000114         10  FILLER              REDEFINES
000115             CPL28-REPORTED-DATE.
000116             15  FILLER                 PIC X(6).
000117             15  CPL28-RD-YR1           PIC X.
000118             15  CPL28-RD-YR2           PIC X.
000119         10  FILLER                     PIC X(1)    VALUE SPACES.
000120         10  CPL28-DATE-PAID            PIC X(8)
000121                                            VALUE 'XX/XX/XX'.
000122         10  FILLER              REDEFINES
000123             CPL28-DATE-PAID.
000124             15  FILLER                 PIC X(6).
000125             15  CPL28-DP-YR1           PIC X.
000126             15  CPL28-DP-YR2           PIC X.
000127         10  FILLER                     PIC X(1)    VALUE SPACES.
000128         10  CPL28-PAID-THRU            PIC X(8)
000129                                            VALUE 'XX/XX/XX'.
000130         10  FILLER              REDEFINES
000131             CPL28-PAID-THRU.
000132             15  FILLER                 PIC X(6).
000133             15  CPL28-PT-YR1           PIC X.
000134             15  CPL28-PT-YR2           PIC X.
000135
000136     05  ABL-PRINT-LINE-28.
000137         10  FILLER                     PIC X(5)    VALUE '-'.
000138         10  ABL28-STATE                PIC X(2)    VALUE ALL 'X'.
000139         10  FILLER                     PIC X(6)    VALUE SPACES.
000140         10  ABL28-ACCOUNT              PIC X(10)   VALUE ALL 'X'.
000141         10  FILLER                     PIC X       VALUE SPACES.
000142         10  ABL28-CERT-NO              PIC X(11)   VALUE ALL 'X'.
000143         10  FILLER                     PIC X       VALUE SPACES.
000144         10  ABL28-CERT-EFF-DATE        PIC X(8)
000145                                            VALUE 'XX/XX/XX'.
000146         10  FILLER                     PIC X(4)    VALUE SPACES.
000147         10  ABL28-CLAIM-CODE           PIC X       VALUE ALL 'X'.
000148         10  FILLER                     PIC X(2)    VALUE SPACES.
000149         10  ABL28-INCURRED-DATE        PIC X(8)
000150                                            VALUE 'XX/XX/XX'.
000151         10  FILLER              REDEFINES
000152             ABL28-INCURRED-DATE.
000153             15  FILLER                 PIC X(6).
000154             15  ABL28-ID-YR1           PIC X.
000155             15  ABL28-ID-YR2           PIC X.
000156         10  FILLER                     PIC X       VALUE SPACES.
000157         10  ABL28-REPORTED-DATE        PIC X(8)
000158                                            VALUE 'XX/XX/XX'.
000159         10  FILLER              REDEFINES
000160             ABL28-REPORTED-DATE.
000161             15  FILLER                 PIC X(6).
000162             15  ABL28-RD-YR1           PIC X.
000163             15  ABL28-RD-YR2           PIC X.
000164         10  FILLER                     PIC X(1)    VALUE SPACES.
000165         10  ABL28-DATE-PAID            PIC X(8)
000166                                            VALUE 'XX/XX/XX'.
000167         10  FILLER              REDEFINES
000168             ABL28-DATE-PAID.
000169             15  FILLER                 PIC X(6).
000170             15  ABL28-DP-YR1           PIC X.
000171             15  ABL28-DP-YR2           PIC X.
000172         10  FILLER                     PIC X(1)    VALUE SPACES.
000173         10  ABL28-PAID-THRU            PIC X(8)
000174                                            VALUE 'XX/XX/XX'.
000175         10  FILLER              REDEFINES
000176             ABL28-PAID-THRU.
000177             15  FILLER                 PIC X(6).
000178             15  ABL28-PT-YR1           PIC X.
000179             15  ABL28-PT-YR2           PIC X.
000180         10  FILLER                     PIC X(1)    VALUE SPACES.
000181
000182     05  CHECK-PRINT-LINE-31.
000183         10  FILLER                     PIC X(5)    VALUE '-'.
000184         10  CPL31-CHECK-AMOUNT         PIC Z,ZZZ,ZZ9.99-.
000185         10  FILLER                     PIC X(3)    VALUE SPACES.
000186         10  CPL31-CLAIM-NO             PIC X(7)    VALUE ALL 'X'.
000187         10  FILLER                     PIC X(4)    VALUE SPACES.
000188         10  CPL31-CHECK-NO             PIC X(7)    VALUE ALL 'X'.
000189         10  FILLER                     PIC X(4)    VALUE SPACES.
000190         10  CPL31-DAYS-PAID            PIC Z,ZZ9-.
000191         10  FILLER                     PIC X(2)    VALUE SPACES.
000192         10  CPL31-AGE                  PIC 99-.
000193         10  FILLER                     PIC X(4)    VALUE SPACES.
000194         10  CPL31-PAY-CODE             PIC X       VALUE ALL 'X'.
000195         10  FILLER                     PIC X(6)    VALUE SPACES.
000196         10  CPL31-CARRIER              PIC X       VALUE ALL 'X'.
000197         10  FILLER                     PIC X(6)    VALUE SPACES.
000198         10  CPL31-GROUP                PIC X(6)    VALUE ALL 'X'.
000199         10  FILLER                     PIC XX      VALUE SPACES.
000200         10  CPL31-ABL-ACCT             PIC X(3)    VALUE SPACES.
000201
000202     05  CHECK-PRINT-LINE-34.
000203         10  FILLER                     PIC X(5)    VALUE '0'.
000204         10  CPL34-COMMENT              PIC X(30)   VALUE SPACES.
000205
000206 01  CHECK-PRINT-LINES-SAVE-AREA        PIC X(1500) VALUE SPACES.
000207
      *<<((file: ELC176W3))
000349
000350     EJECT
000351*                           COPY ELCCSOCL.
      *>>((file: ELCCSOCL))
000001 01  CSO-CHECK-PRINT-LINES.
000002     05  CSO-CHECK-PRINT-LINE-1.
000003         10  FILLER                  PIC X         VALUE '1'.
000004
000005     05  CSO-CHECK-PRINT-LINE-2.
000006         10  FILLER                  PIC X(13)     VALUE ' '.
000007         10  CSO2-COMPANY-NAME       PIC X(43)     VALUE ALL 'X'.
000008         10  FILLER                  PIC X(7)      VALUE ' '.
000009         10  CSO2-CHECK-NUMBER       PIC X(7)      VALUE ALL 'X'.
000010
000011     05  CSO-CHECK-PRINT-LINE-3.
000012         10  FILLER                  PIC X(18)     VALUE SPACES.
000013         10  FILLER                  PIC X(33)
000014             VALUE 'P.O. BOX 34350   OMAHA, NE  68134'.
000015
000016     05  CSO-CHECK-PRINT-LINE-4.
000017         10  FILLER                  PIC X         VALUE ' '.
000018
000019     05  CSO-CHECK-PRINT-LINE-5.
000020         10  FILLER                  PIC X(2)      VALUE ' '.
000021         10  CSO5-CLAIM-NO           PIC X(7)      VALUE ALL 'X'.
000022         10  FILLER                  PIC X(3)      VALUE SPACES.
000023         10  CSO5-CERT-NO            PIC X(11)     VALUE ALL 'X'.
000024         10  FILLER                  PIC X(2)      VALUE SPACES.
000025         10  CSO5-PLAN-CODE          PIC X(4)      VALUE ALL 'X'.
000026         10  FILLER                  PIC X(3)      VALUE SPACES.
000027         10  CSO5-PAID-FROM-DATE     PIC X(8)   VALUE 'XX/XX/XX'.
000028         10  FILLER                  PIC X(2)      VALUE SPACES.
000029         10  CSO5-PAID-THRU-DATE     PIC X(8)   VALUE 'XX/XX/XX'.
000030         10  FILLER                  PIC X         VALUE SPACE.
000031         10  CSO5-AMOUNT-PAID        PIC ZZZ,ZZ9.99-.
000032         10  FILLER                  PIC X         VALUE SPACE.
000033         10  CSO5-PAYMENT-TYPE       PIC X      VALUE 'X'.
000034         10  FILLER                  PIC X         VALUE SPACE.
000035         10  CSO5-ACCT-NO            PIC X(10)  VALUE SPACES.
000036
000037     05  CSO-CHECK-PRINT-LINE-6.
000038         10  FILLER                  PIC X         VALUE ' '.
000039
000040     05  CSO-CHECK-PRINT-LINE-7.
000041         10  FILLER                  PIC X(5)      VALUE SPACES.
000042         10  CSO7-CC-ACCT            PIC X(7)      VALUE SPACES.
000043         10  CSO7-CC-ACCT-NUMBER     PIC X(18)     VALUE SPACES.
000044         10  FILLER                  PIC X(21)     VALUE SPACES.
000045         10  CSO7-TYPE-MESSAGE       PIC X(15)     VALUE ALL 'X'.
000046
000047     05  CSO-CHECK-PRINT-LINE-8.
000048         10  FILLER                  PIC X         VALUE ' '.
000049
000050     05  CSO-CHECK-PRINT-LINE-9.
000051         10  FILLER                  PIC X(50)     VALUE SPACES.
000052         10  CSO9-FINAL-MESS         PIC X(17)     VALUE ALL 'X'.
000053
000054     05  CSO-CHECK-PRINT-LINE-10.
000055         10  FILLER                  PIC X(50)     VALUE ' '.
000056         10  CSO10-FINAL-MESS        PIC X(17)     VALUE ALL 'X'.
000057
000058     05  CSO-CHECK-PRINT-LINE-11.
000059         10  FILLER                  PIC X(5)      VALUE ' '.
000060         10  CSO11-MEMBER-NAME       PIC X(30)     VALUE ALL 'X'.
000061         10  FILLER                  PIC X(15)     VALUE SPACES.
000062         10  CSO11-FINAL-MESS        PIC X(17)     VALUE ALL 'X'.
000063
000064     05  CSO-CHECK-PRINT-LINE-12.
000065         10  FILLER                  PIC X(5)      VALUE SPACES.
000066         10  CSO12-MEMBER-ADDRESS1   PIC X(30)     VALUE ALL 'X'.
000067
000068     05  CSO-CHECK-PRINT-LINE-13.
000069         10  FILLER                  PIC X(5)      VALUE SPACES.
000070         10  CSO13-MEMBER-ADDRESS2   PIC X(30)     VALUE ALL 'X'.
000071
000072     05  CSO-CHECK-PRINT-LINE-14.
000073         10  FILLER                  PIC X(5)      VALUE SPACES.
000074         10  CSO14-MEMBER-ADDRESS3   PIC X(30)     VALUE ALL 'X'.
000075
000076     05  CSO-CHECK-PRINT-LINE-15.
000077         10  FILLER                  PIC X(5)      VALUE SPACES.
000078         10  CSO15-MEMBER-ADDRESS4   PIC X(30)     VALUE ALL 'X'.
000079
000080     05  CSO-CHECK-PRINT-LINE-16.
000081         10  FILLER                  PIC X(5)      VALUE ' '.
000082         10  CSO16-MEMBER-ADDRESS5.
000083             15  FILLER                 PIC X(21).
000084             15  CSO16-MEMBER-ZIP-CODE  PIC ZZZZ99999.
000085         10  FILLER                  PIC X(9)      VALUE SPACES.
000086
000087     05  CSO-CHECK-PRINT-LINE-17.
000088         10  FILLER                  PIC X(5)      VALUE SPACES.
000089         10  CSO17-3RD-NAME          PIC X(40)     VALUE ALL 'X'.
000090
000091     05  CSO-CHECK-PRINT-LINE-18.
000092         10  FILLER                  PIC X(5)      VALUE SPACES.
000093         10  CSO18-3RDADD-LINE1      PIC X(40)     VALUE ALL 'X'.
000094
000095     05  CSO-CHECK-PRINT-LINE-19.
000096         10  FILLER                  PIC X(5)      VALUE SPACES.
000097         10  CSO19-3RDADD-LINE2      PIC X(40)     VALUE ALL 'X'.
000098
000099     05  CSO-CHECK-PRINT-LINE-20.
000100         10  FILLER                  PIC X(5)      VALUE SPACES.
000101         10  CSO20-3RD-CITY-STATE    PIC X(40)     VALUE ALL 'X'.
000102
000103     05  CSO-CHECK-PRINT-LINE-21.
000104         10  FILLER                  PIC X(26)     VALUE ' '.
000105         10  CSO21-3RD-ZIP           PIC ZZZZ99999.
000106*        10  FILLER                  PIC X(9)      VALUE SPACES.
000107         10  FILLER                  PIC X(5)      VALUE SPACES.
000108         10  CSO-NOTE-LINE1          PIC X(40)     VALUE SPACES.
000109
000110     05  CSO-CHECK-PRINT-LINE-22.
000111*        10  FILLER                  PIC X         VALUE ' '.
000112         10  FILLER                  PIC X(40)     VALUE ' '.
000113         10  CSO-NOTE-LINE2          PIC X(40)     VALUE SPACES.
000114
000115     05  CSO-CHECK-PRINT-LINE-23.
000116         10  FILLER                  PIC X(22)     VALUE SPACES.
000117         10  CSO23-REPLY-DT          PIC X(5)      VALUE ALL 'X'.
000118
000119     05  CSO-CHECK-PRINT-LINE-24.
000120         10  FILLER                  PIC X(64)     VALUE ' '.
000121         10  CSO24-CHECK-NUMBER      PIC X(7)      VALUE ALL 'X'.
000122
000123     05  CSO-CHECK-PRINT-LINE-25.
000124         10  FILLER                  PIC X         VALUE ' '.
000125
000126     05  CSO-CHECK-PRINT-LINE-26.
000127         10  FILLER                  PIC X(3)      VALUE SPACES.
000128         10  CSO26-COMPANY-NAME      PIC X(43)     VALUE ALL 'X'.
000129
000130     05  CSO-CHECK-PRINT-LINE-27.
000131         10  FILLER                  PIC X         VALUE ' '.
000132
000133     05  CSO-CHECK-PRINT-LINE-28.
000134         10  FILLER                  PIC X         VALUE ' '.
000135
000136     05  CSO-CHECK-PRINT-LINE-29.
000137         10  FILLER                  PIC X         VALUE ' '.
000138
000139     05  CSO-CHECK-PRINT-LINE-30.
000140         10  FILLER                  PIC X         VALUE ' '.
000141
000142     05  CSO-CHECK-PRINT-LINE-31.
000143         10  FILLER                  PIC X(3)      VALUE ' '.
000144         10  CSO31-CHECK-DATE        PIC X(8) VALUE 'XX/XX/XX'.
000145         10  FILLER                  PIC X(6)      VALUE SPACES.
000146         10  CSO31-CLAIM-NO          PIC X(7)      VALUE ALL 'X'.
000147         10  FILLER                  PIC XX        VALUE SPACES.
000148         10  CSO31-CC-ACCT           PIC X(7)      VALUE SPACES.
000149         10  CSO31-CC-ACCT-NUMBER    PIC X(18)     VALUE SPACES.
000150         10  FILLER                  PIC X(10)     VALUE SPACES.
000151         10  CSO31-CHECK-AMOUNT      PIC ***,**9.99-.
000152         10  CSO31-CHECK-AMOUNT-R REDEFINES CSO31-CHECK-AMOUNT
000153                                     PIC X(11).
000154
000155     05  CSO-CHECK-PRINT-LINE-32.
000156         10  FILLER                  PIC X(17)     VALUE ' '.
000157         10  FILLER                  PIC X(9) VALUE 'INSURED: '.
000158         10  CSO32-MEMBER-NAME       PIC X(30)     VALUE ALL 'X'.
000159
000160     05  CSO-CHECK-PRINT-LINE-33.
000161         10  FILLER                  PIC X(40)     VALUE ' '.
000162
000163     05  CSO-CHECK-PRINT-LINE-34.
000164         10  FILLER                  PIC X(40)     VALUE ' '.
000165
000166     05  CSO-CHECK-PRINT-LINE-35.
000167         10  FILLER                  PIC X(4)      VALUE SPACES.
000168         10  CSO35-PAYEE-NAME        PIC X(30)     VALUE ALL 'X'.
000169
000170     05  CSO-CHECK-PRINT-LINE-36.
000171         10  FILLER                  PIC X(4)      VALUE SPACES.
000172         10  CSO36-PAYEE-ADDRESS1    PIC X(30)     VALUE ALL 'X'.
000173
000174     05  CSO-CHECK-PRINT-LINE-37.
000175         10  FILLER                  PIC X(4)      VALUE SPACES.
000176         10  CSO37-PAYEE-ADDRESS2    PIC X(30)     VALUE ALL 'X'.
000177
000178     05  CSO-CHECK-PRINT-LINE-38.
000179         10  FILLER                  PIC X(4)      VALUE SPACES.
000180         10  CSO38-PAYEE-ADDRESS3    PIC X(30)     VALUE ALL 'X'.
000181
000182     05  CSO-CHECK-PRINT-LINE-39.
000183         10  FILLER                  PIC X(4)      VALUE SPACES.
000184         10  CSO39-PAYEE-ADDRESS4    PIC X(30)     VALUE ALL 'X'.
000185
000186     05  CSO-CHECK-PRINT-LINE-40.
000187         10  FILLER                  PIC X(4)      VALUE SPACES.
000188         10  CSO40-PAYEE-ADDRESS5.
000189             15  FILLER              PIC X(21)     VALUE SPACES.
000190             15  CSO40-PAYEE-ZIP-CODE  PIC ZZZZ99999 VALUE ZEROS.
000191
000192     05  CSO-CHECK-PRINT-LINE-41.
000193         10  FILLER                  PIC X         VALUE ' '.
000194
000195     05  CSO-CHECK-PRINT-LINE-42.
000196         10  FILLER                  PIC X         VALUE ' '.
000197
      *<<((file: ELCCSOCL))
000352
000353     EJECT
000354*                           COPY ELCDATE.
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
000355
000356*01  S-WORK-AREA COPY ELPRTCVD.
000357*                       COPY ELPRTCVD.
000358
000359 01  CSO-DRAFT-420C.
000360     05  CSO-DRAFT-KEY           PIC X(19).
000361     05  FILLER                  PIC X(1235).
000362 01  CSO-DRAFT-420C-RED REDEFINES CSO-DRAFT-420C.
000363*    COPY MICR420C.
      *>>((file: MICR420C))
000001******************************************************************
000002*                   C H A N G E   L O G
000003*
000004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000005*-----------------------------------------------------------------
000006*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000007* EFFECTIVE    NUMBER
000008*-----------------------------------------------------------------
000009* 120803    2002100700001  SMVA  ADD ACCT STATE & CLM TYPE FOR
000010*                                PRINTING STATE SPEC PROGRESS RPTS
000011* 011105    2004072100002  PEMA  ADD ONE COMMENT LINE
000012* 013017  CR2016053100001  PEMA  ACH PROCESSING
000013******************************************************************
000014*
000015* CURRENT LENGTH OF LAYOUT IS 1254
000016*
000017******************************************************************
000018       10 MICR-KEY.
000019           15 M420C-FORM         PIC X(4).
000020           15 M420C-DRAFT-ORDER  PIC 9(5).
000021           15 M420C-DRAFT        PIC X(10).
000022       10 M420C-PRINT-COUNT      PIC S9(4) COMP.
000023       10 M420C-SEQ-NUMBER       PIC S9(4) COMP.
000024       10 M420C-AMOUNT-PAID      PIC S9(9)V9(2) COMP-3.
000025       10 M420C-COMPANY-NAME     PIC X(43).
000026       10 M420C-CSO-ADDRESS      PIC X(33).
000027       10 M420C-CLAIM-NO         PIC X(7).
000028       10 M420C-CERT-NO          PIC X(11).
000029       10 M420C-ACCT-NO          PIC X(10).
000030       10 M420C-PLAN-CODE        PIC X(4).
000031       10 M420C-PAID-FROM-DATE   PIC X(8).
000032       10 M420C-PAID-THRU-DATE   PIC X(8).
000033       10 M420C-PAYMENT-TYPE     PIC X.
000034       10 M420C-CC-ACCT          PIC X(7).
000035       10 M420C-CC-ACCT-NUMBER   PIC X(18).
000036       10 M420C-TYPE-MESSAGE     PIC X(15).
000037       10 M420C-FINAL-MESS9      PIC X(17).
000038       10 M420C-FINAL-MESS10     PIC X(17).
000039       10 M420C-FINAL-MESS11     PIC X(17).
000040       10 M420C-MEMBER-NAME      PIC X(30).
000041       10 M420C-MEMBER-ADDRESS1  PIC X(30).
000042       10 M420C-MEMBER-ADDRESS2  PIC X(30).
000043       10 M420C-MEMBER-ADDRESS3  PIC X(30).
000044       10 M420C-MEMBER-ADDRESS4  PIC X(30).
000045       10 M420C-MEMBER-ZIP-CODE  PIC X(9).
000046       10 M420C-3RDADD-NAME      PIC X(30).
000047       10 M420C-3RDADD-LINE1     PIC X(30).
000048       10 M420C-3RDADD-LINE2     PIC X(40).
000049       10 M420C-3RDADD-LINE3     PIC X(40).
000050       10 M420C-3RDADD-ZIP       PIC X(9).
000051       10 M420C-CHECK-DATE       PIC X(8).
000052       10 M420C-DFT-NOTES1       PIC X(40).
000053       10 M420C-DFT-NOTES2       PIC X(39).
000054       10 M420C-ACH-PAYMENT      PIC X.
000055       10 M420C-PAYEE-NAME       PIC X(30).
000056       10 M420C-PAYEE-ADDRESS1   PIC X(30).
000057       10 M420C-PAYEE-ADDRESS2   PIC X(30).
000058       10 M420C-PAYEE-ADDRESS3   PIC X(30).
000059       10 M420C-PAYEE-ADDRESS4   PIC X(30).
000060       10 M420C-PAYEE-ZIP-CODE   PIC X(9).
000061       10 M420C-REPLY-DATE       PIC X(5).
000062       10 M420C-SIGNATURE        PIC X.
000063       10 M420C-LOAN-NUMBER      PIC X(25).
000064       10 M420C-DRAFT-MESSAGES.
000065          15 M420C-DRAFT-MESSAGE OCCURS 6 TIMES  PIC X(70).
000066       10 M420C-ACCT-STATE       PIC X(02).
000067       10 M420C-CLAIM-TYPE       PIC X(01).
      *<<((file: MICR420C))
000364 01  CSO-RESP                    PIC S9(5) COMP.
000365 01  CSO-ZIP                     PIC Z(9).
000366     EJECT
000367*                    COPY ELCCPA.
      *>>((file: ELCCPA))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCPA.                             *
000005*                            VMOD=2.013                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO CHECK WRITER ROUTINE.     *
000008******************************************************************
000009******************************************************************
000010*                   C H A N G E   L O G
000011*
000012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000013*-----------------------------------------------------------------
000014*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000015* EFFECTIVE    NUMBER
000016*-----------------------------------------------------------------
000017* 072110    2009122800001  AJRA  CHANGE COMMENT,COMMENT-2 FROM 40
000018*                                SEPARATE CITY AND STATE
000019* 013017  CR2016053100001  PEMA  ACH PROCESSING
000020******************************************************************
000021
000022 01  CHECK-PASS-AREA.
000023     12  CPA-ALIGNMENT               PIC S9(3)    COMP-3.
000024     12  CPA-CARRIER                 PIC X.
000025     12  CPA-GROUP                   PIC X(6).
000026     12  CPA-ACCOUNT                 PIC X(10).
000027     12  CPA-STATE                   PIC XX.
000028     12  CPA-CLAIM-NO                PIC X(7).
000029     12  CPA-CERT-NO                 PIC X(11).
000030     12  CPA-CERT-EFF-DT             PIC XX.
000031     12  CPA-CLAIM-TYPE              PIC X.
000032     12  CPA-PAYEE-TYPE-CD           PIC X.
000033     12  CPA-IND-GRP-TYPE            PIC X.
000034         88  CPA-INDIVIDUAL                     VALUE 'I'.
000035         88  CPA-GROUP-POLICY                   VALUE 'G'.
000036     12  CPA-PAYEE-NAME              PIC X(30).
000037     12  CPA-PAYEE-ADDRESS-LINE1     PIC X(30).
000038     12  CPA-PAYEE-ADDRESS-LINE2     PIC X(30).
000039     12  CPA-PAYEE-ADDRESS-LINE3     PIC X(30).
000040     12  CPA-PAYEE-CITY-STATE.
000041         16  CPA-PAYEE-CITY          PIC X(28).
000042         16  CPA-PAYEE-STATE         PIC X(2).
000043     12  CPA-PAYEE-ZIP.
000044         16  CPA-PAYEE-ZIP-CODE.
000045             24  CPA-ZIP-1ST-PAYEE   PIC X.
000046                 88  CPA-CAN-POST-CODE-PAYEE  VALUE 'A' THRU 'Z'.
000047             24  FILLER              PIC X(4).
000048         16  CPA-PAYEE-ZIP-PLUS4     PIC X(4).
000049     12  CPA-CANADIAN-POSTAL-CODE-PAYEE REDEFINES CPA-PAYEE-ZIP.
000050         16  CPA-CAN-POSTAL-1-PAYEE  PIC XXX.
000051         16  CPA-CAN-POSTAL-2-PAYEE  PIC XXX.
000052         16  FILLER                  PIC XXX.
000053     12  CPA-INSURED-NAME            PIC X(30).
000054     12  CPA-INSURED-ADDRESS-LINE1   PIC X(30).
000055     12  CPA-INSURED-ADDRESS-LINE2   PIC X(30).
000056     12  CPA-INSURED-ADDRESS-LINE3   PIC X(30).
000057     12  CPA-INSURED-CITY-STATE.
000058         16  CPA-INSURED-CITY        PIC X(28).
000059         16  CPA-INSURED-STATE       PIC X(2).
000060     12  CPA-INSURED-ZIP.
000061         16  CPA-INSURED-ZIP-CODE.
000062             24  CPA-ZIP-1ST-INS     PIC X.
000063                 88  CPA-CAN-POST-CODE-INS    VALUE 'A' THRU 'Z'.
000064             24  FILLER              PIC X(4).
000065         16  CPA-INSURED-ZIP-PLUS4   PIC X(4).
000066     12  CPA-CANADIAN-POSTAL-CODE-INS REDEFINES CPA-INSURED-ZIP.
000067         16  CPA-CAN-POSTAL-1-INS    PIC XXX.
000068         16  CPA-CAN-POSTAL-2-INS    PIC XXX.
000069         16  FILLER                  PIC XXX.
000070     12  CPA-INSURED-AGE             PIC 99.
000071     12  CPA-PAYMENT-TYPE            PIC X.
000072     12  CPA-PAYMENT-BY              PIC X(4).
000073     12  CPA-CHECK-DATE              PIC X(2).
000074     12  CPA-CHECK-NUMBER            PIC X(7).
000075     12  CPA-AMOUNT-PAID             PIC S9(7)V99    COMP-3.
000076     12  CPA-AMOUNT-PAID-TO-DATE     PIC S9(7)V99    COMP-3.
000077     12  CPA-DAYS-PAID               PIC S9(5)       COMP-3.
000078     12  CPA-DAILY-RATE              PIC S9(3)V99    COMP-3.
000079     12  CPA-ELIMINATION-DAYS        PIC S9(3)       COMP-3.
000080     12  CPA-CLAIM-CODE              PIC X.
000081     12  CPA-PAY-CODE                PIC X.
000082     12  CPA-INCURRED-DT             PIC XX.
000083     12  CPA-REPORTED-DT             PIC XX.
000084     12  CPA-PAID-THRU-DT            PIC XX.
000085     12  CPA-PAID-FROM-DT            PIC XX.
000086     12  CPA-PAID-DT                 PIC XX.
000087
000088     12  CPA-ACCOUNT-NAME            PIC X(30).
000089     12  CPA-ACCOUNT-IN-CARE-OF      PIC X(30).
000090     12  CPA-ACCOUNT-ADDRESS-LINE1   PIC X(30).
000091     12  CPA-ACCOUNT-ADDRESS-LINE2   PIC X(30).
000092     12  CPA-ACCOUNT-CITY-ST.
000093         16  CPA-ACCOUNT-CITY        PIC X(28).
000094         16  CPA-ACCOUNT-STATE       PIC X(2).
000095     12  CPA-ACCOUNT-ZIP-CODE.
000096         16  CPA-ACCOUNT-ZIP.
000097             24  CPA-ZIP-1ST-ACCT    PIC X.
000098                 88  CPA-CAN-POST-CODE-ACCT VALUE 'A' THRU 'Z'.
000099             24  FILLER              PIC X(4).
000100         16  CPA-ACCOUNT-ZIP-PLUS4   PIC X(4).
000101     12  CPA-CANADIAN-POSTAL-CODE-ACCT
000102             REDEFINES CPA-ACCOUNT-ZIP-CODE.
000103         16  CPA-CAN-POSTAL-1-ACCT   PIC XXX.
000104         16  CPA-CAN-POSTAL-2-ACCT   PIC XXX.
000105         16  FILLER                  PIC XXX.
000106     12  CPA-ACCOUNT-PHONE-NO        PIC S9(11)     COMP-3.
000107
000108     12  CPA-SOC-SEC-NO              PIC X(11).
000109     12  CPA-MEMBER-NUMBER           PIC X(12).
000110     12  CPA-LOAN-NUMBER             PIC X(8).
000111
000112     12  CPA-BENEFIT-TYPE            PIC X.
000113
000114     12  CPA-NO-OF-PMTS-MADE         PIC S9(3)       COMP-3.
000115
000116     12  CPA-EXPIRE-DT               PIC XX.
000117
000118     12  CPA-MONTHLY-BENEFIT         PIC S9(7)V99    COMP-3.
000119
000120     12  CPA-COMMENT                 PIC X(60).
000121
000122     12  CPA-CLAIM-STATUS            PIC X.
000123*      88  CLAIM-IS-OPEN                             VALUE 'O'.
000124*      88  CLAIM-IS-CLOSED                           VALUE 'C'.
000125
000126     12  CPA-LAST-CLOSE-REASON       PIC X.
000127*      88  FINAL-PAID                                VALUE '1'.
000128*      88  CLAIM-DENIED                              VALUE '2'.
000129*      88  AUTO-CLOSE                                VALUE '3'.
000130
000131     12  CPA-INSURED-ADDR-TRLR-NAME  PIC X(30).
000132
000133     12  CPA-EXAMINER                PIC X(4).
000134     12  CPA-EXPENSE-TYPE            PIC X.
000135
000136     12  CPA-CARRIER-ADDRESS-DATA.
000137         16  CPA-CARRIER-NAME            PIC X(30).
000138         16  CPA-CARRIER-ADDRESS-LINE1   PIC X(30).
000139         16  CPA-CARRIER-ADDRESS-LINE2   PIC X(30).
000140         16  CPA-CARRIER-ADDRESS-LINE3   PIC X(30).
000141         16  CPA-CARRIER-CITY-STATE.
000142             24  CPA-CARRIER-CITY        PIC X(28).
000143             24  CPA-CARRIER-STATE       PIC X(2).
000144         16  CPA-CARRIER-ZIP.
000145             24  CPA-CARRIER-ZIP-CODE.
000146                 28  CPA-ZIP-1ST-CARRIER PIC X.
000147                     88  CPA-CAN-POST-CODE-CARR
000148                                         VALUE 'A' THRU 'Z'.
000149                 28  FILLER              PIC X(4).
000150             24  CPA-CARRIER-ZIP-PLUS4   PIC X(4).
000151         16  CPA-CANADIAN-POSTAL-CODE-CARR
000152                           REDEFINES CPA-CARRIER-ZIP.
000153             24  CPA-CAN-POSTAL-1-CARR   PIC XXX.
000154             24  CPA-CAN-POSTAL-2-CARR   PIC XXX.
000155             24  FILLER                  PIC XXX.
000156
000157     12  CPA-PAYMENT-ORIGIN              PIC X.
000158     12  CPA-BENEFIT-CD                  PIC XX.
000159     12  CPA-COMMENT-2                   PIC X(60).
000160
000161     12  CPA-NOTIFY-ADDRESS-DATA.
000162         16  CPA-NOTIFY-NAME             PIC X(30).
000163         16  CPA-NOTIFY-ADDRESS-LINE1    PIC X(30).
000164         16  CPA-NOTIFY-ADDRESS-LINE2    PIC X(30).
000165         16  CPA-NOTIFY-ADDRESS-LINE3    PIC X(30).
000166         16  CPA-NOTIFY-CITY-STATE.
000167             20  CPA-NOTIFY-CITY         PIC X(28).
000168             20  CPA-NOTIFY-STATE        PIC X(2).
000169         16  CPA-NOTIFY-ZIP.
000170             20  CPA-NOTIFY-ZIP-CODE     PIC X(5).
000171             20  CPA-NOTIFY-ZIP-PLUS4    PIC X(4).
000172     12  CPA-COVERAGE-TYPE               PIC X.
000173         88  CPA-LEVEL-COV                 VALUE 'L'.
000174         88  CPA-REDUCE-COV                VALUE 'R'.
000175         88  CPA-PROP-COV                  VALUE 'P'.
000176         88  CPA-DISAB-COV                 VALUE 'A'.
000177
000178     12  CPA-AUTO-PAY-END-DT             PIC X(02).
000179     12  CPA-COMPANY-NAME                PIC X(30).
000180     12  CPA-TOTAL-BENEFIT               PIC S9(09)V99  COMP-3.
000181     12  CPA-FORM-CTL-SEQ-NO             PIC S9(4)      COMP.
000182     12  CPA-BENEFICIARY                 PIC X(25).
000183     12  CPA-REFERENCE-NO.
000184         16  CPA-REFERENCE-PRIME         PIC X(17).
000185         16  CPA-REFERENCE-SFX           PIC X(02).
000186     12  CPA-ACH-PAYMENT                 PIC X.
000187     12  CPA-CV-LOAN-NUMBER              PIC X(20).
000188     12  CPA-CREDIT-CARD-NO REDEFINES
000189         CPA-CV-LOAN-NUMBER              PIC X(20).
      *<<((file: ELCCPA))
000368     EJECT
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
000370
000371 01  DFHCOMMAREA                 PIC X(1024).
000372
000373     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL177' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000374 VCOBOL-DUMMY-PROCEDURE.
000375
000376     
      * EXEC CICS HANDLE CONDITION
000377*        QIDERR  (5000-MAIN-LOGIC)
000378*        ITEMERR (5000-MAIN-LOGIC)
000379*        ERROR   (9990-ERROR) END-EXEC.
      *    MOVE '"$N<.                 ! " #00001844' TO DFHEIV0
           MOVE X'22244E3C2E20202020202020' &
                X'202020202020202020202120' &
                X'2220233030303031383434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000380
000381     EJECT
000382 0010-MAIN-LOGIC.
000383
000384
000385     
      * EXEC CICS RETRIEVE
000386*        INTO   (PROGRAM-INTERFACE-BLOCK)
000387*        LENGTH (PI-COMM-LENGTH) END-EXEC
      *    MOVE '0*I L                 &   #00001853' TO DFHEIV0
           MOVE X'302A49204C20202020202020' &
                X'202020202020202020202620' &
                X'2020233030303031383533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000388
000389     MOVE +1                     TO  PI-TEMP-STORAGE-ITEM.
000390
000391
000392 0100-MAIN-LOGIC.
000393
000394
000395     
      * EXEC CICS READQ TS
000396*        INTO   (CHECK-PASS-AREA)
000397*        QUEUE  (PI-TEMP-STORAGE-KEY)
000398*        ITEM   (PI-TEMP-STORAGE-ITEM)
000399*        LENGTH (WS-TS-LENGTH)
000400*    END-EXEC
      *    MOVE '*$II   L              ''   #00001863' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303031383633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 CHECK-PASS-AREA, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000401
000402     IF WS-TS-LENGTH NOT GREATER THAN +1
000403         
      * EXEC CICS DELETEQ TS
000404*            QUEUE (PI-TEMP-STORAGE-KEY) END-EXEC
      *    MOVE '*&                    #   #00001871' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303031383731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000405         
      * EXEC CICS RETURN
000406*            END-EXEC.
      *    MOVE '.(                    ''   #00001873' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303031383733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000407
000408     IF PI-COMPANY-ID = ('CID' OR 'DCC' OR 'AHL')
000409        GO TO 0600-MAIN-LOGIC.
000410
000411 0600-MAIN-LOGIC.
000412
000413     IF CPA-ALIGNMENT NOT EQUAL TO ZERO
000414         MOVE +999999.99         TO  CSO31-CHECK-AMOUNT
000415         MOVE +999999.99         TO  CSO5-AMOUNT-PAID
000416
000417         GO TO 0650-MAIN-LOGIC.
000418
000419*    MOVE CSO-CHECK-PRINT-LINES  TO  CHECK-PRINT-LINES-SAVE-AREA.
000420
000421     MOVE CHECK-PRINT-LINES-SAVE-AREA  TO  CSO-CHECK-PRINT-LINES
000422
000423
000424* THE FOLLOWING IF IS USED TO PRINT OR SPACE THE FINAL MESSAGE
000425     IF (CPA-PAYMENT-TYPE = '2')
000426        AND (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000427                                  OR 'B' OR 'H' )
000428          MOVE WS-LINE-9  TO CSO9-FINAL-MESS
000429          MOVE WS-LINE-10 TO CSO10-FINAL-MESS
000430          MOVE WS-LINE-11 TO CSO11-FINAL-MESS
000431     ELSE
000432          MOVE SPACES TO CSO9-FINAL-MESS
000433                         CSO10-FINAL-MESS
000434                         CSO11-FINAL-MESS
000435     END-IF
000436
000437     MOVE EIBDATE           TO DC-JULIAN-YYDDD.
000438     MOVE '5'               TO DC-OPTION-CODE.
000439     PERFORM 8500-DATE-CONVERSION.
000440     MOVE DC-BIN-DATE-1       TO WS-BIN-CURRENT-DT.
000441     MOVE DC-GREG-DATE-1-EDIT TO WS-EDT-CURRENT-DT
000442     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000443                                  OR 'B' OR 'H' )
000444         AND (CPA-PAYMENT-TYPE = '1' OR '2')
000445             MOVE CPA-PAID-THRU-DT TO DC-BIN-DATE-1
000446             MOVE +1               TO DC-ELAPSED-MONTHS
000447                                      DC-ELAPSED-DAYS
000448             MOVE '6' TO DC-OPTION-CODE
000449             PERFORM 8500-DATE-CONVERSION
000450             IF DC-BIN-DATE-2 > WS-BIN-CURRENT-DT
000451                MOVE DC-GREG-DATE-1-EDIT TO CSO23-REPLY-DT
000452             ELSE
000453                MOVE WS-EDT-CURRENT-DT   TO CSO23-REPLY-DT
000454***             MOVE ' NOW ' TO CSO23-REPLY-DT
000455     ELSE
000456         MOVE SPACES TO CSO23-REPLY-DT.
000457
000458     MOVE CPA-SOC-SEC-NO TO WS-SOC-SEC-NO.
000459     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL'
000460         MOVE SPACES TO CSO31-CC-ACCT
000461                        CSO31-CC-ACCT-NUMBER
000462                        CSO7-CC-ACCT
000463                        CSO7-CC-ACCT-NUMBER
000464     ELSE
000465         MOVE 'ACCT # ' TO CSO31-CC-ACCT, CSO7-CC-ACCT
000466         MOVE WS-SOC-SEC-NO TO CSO31-CC-ACCT-NUMBER
000467                               CSO7-CC-ACCT-NUMBER.
000468
000469     IF (CPA-CLAIM-TYPE = 'L' OR 'O') AND (CPA-PAYMENT-TYPE = '4')
000470        MOVE '  FINAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
000471        MOVE 'F'                TO  CSO5-PAYMENT-TYPE
000472     ELSE
000473        IF CPA-PAYMENT-TYPE EQUAL TO '2'
000474           MOVE '  FINAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
000475           MOVE 'F'                TO  CSO5-PAYMENT-TYPE
000476        ELSE
000477           MOVE 'PARTIAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
000478           MOVE 'P'                TO  CSO5-PAYMENT-TYPE
000479        END-IF
000480     END-IF
000481
000482     MOVE CPA-INSURED-ADDR-TRLR-NAME TO CSO11-MEMBER-NAME.
000483     MOVE CPA-INSURED-NAME TO CSO32-MEMBER-NAME.
000484     MOVE CPA-INSURED-ADDRESS-LINE1  TO  CSO12-MEMBER-ADDRESS1
000485     MOVE SPACES            TO CSO5-PLAN-CODE.
000486
000487     MOVE CPA-INSURED-ADDRESS-LINE2  TO  CSO13-MEMBER-ADDRESS2
000488
000489     MOVE CPA-INSURED-ADDRESS-LINE3  TO  CSO14-MEMBER-ADDRESS3
000490     MOVE CPA-CLAIM-NO           TO  CSO5-CLAIM-NO
000491                                     CSO31-CLAIM-NO
000492     MOVE CPA-CERT-NO            TO  CSO5-CERT-NO
000493
000494     MOVE CPA-ACCOUNT            TO  CSO5-ACCT-NO.
000495
000496     MOVE CPA-INSURED-CITY-STATE  TO  CSO15-MEMBER-ADDRESS4
000497
000498     IF CPA-INSURED-ZIP-CODE NOT EQUAL TO ZERO
000499         MOVE CPA-INSURED-ZIP-CODE  TO  CSO16-MEMBER-ZIP-CODE.
000500
000501     IF CSO15-MEMBER-ADDRESS4 EQUAL TO SPACES
000502         MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
000503         MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
000504
000505     IF CSO14-MEMBER-ADDRESS3 EQUAL TO SPACES
000506         MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
000507         MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
000508         MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
000509
000510     IF CSO13-MEMBER-ADDRESS2 EQUAL TO SPACES
000511         MOVE CSO14-MEMBER-ADDRESS3 TO CSO13-MEMBER-ADDRESS2
000512         MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
000513         MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
000514         MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
000515
000516     IF CSO12-MEMBER-ADDRESS1 EQUAL TO SPACES
000517         MOVE CSO13-MEMBER-ADDRESS2 TO CSO12-MEMBER-ADDRESS1
000518         MOVE CSO14-MEMBER-ADDRESS3 TO CSO13-MEMBER-ADDRESS2
000519         MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
000520         MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
000521         MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
000522
000523     IF CPA-PAID-FROM-DT NOT EQUAL TO LOW-VALUES
000524         MOVE CPA-PAID-FROM-DT   TO  DC-BIN-DATE-1
000525         MOVE SPACES             TO  DC-OPTION-CODE
000526         PERFORM 8500-DATE-CONVERSION
000527         IF DC-ERROR-CODE EQUAL TO SPACES
000528             MOVE DC-GREG-DATE-1-EDIT  TO  CSO5-PAID-FROM-DATE.
000529
000530     IF CPA-PAID-THRU-DT NOT EQUAL TO LOW-VALUES
000531         MOVE CPA-PAID-THRU-DT   TO  DC-BIN-DATE-1
000532         MOVE SPACES             TO  DC-OPTION-CODE
000533         PERFORM 8500-DATE-CONVERSION
000534         IF DC-ERROR-CODE EQUAL TO SPACES
000535             MOVE DC-GREG-DATE-1-EDIT  TO  CSO5-PAID-THRU-DATE.
000536
000537     MOVE CPA-CERT-NO            TO WS-CHECK-AREA.
000538
000539     IF PI-COMPANY-ID EQUAL TO 'CID' OR 'DCC' OR 'AHL'
000540       MOVE 01-HEADING TO WS-COMPANY-NAME
000541       MOVE 01-COMP-NAME TO WS-COMPANY-NAME2
000542     END-IF.
000543
000544     MOVE WS-COMPANY-NAME TO CSO2-COMPANY-NAME
000545     MOVE WS-COMPANY-NAME2 TO CSO26-COMPANY-NAME
000546
000547     IF CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000548                                  OR 'B' OR 'H'
000549         MOVE 'A&H ' TO CSO5-PLAN-CODE
000550     ELSE
000551         MOVE 'LIFE' TO CSO5-PLAN-CODE.
000552
000553*****CLAIM TYPE P WAS ADDED TO FLAG THOSE LIFE CLAIMS WHICH
000554*****ARE IN REALITY PROPERTY CLAIMS BEING PROCESSED THRU THE
000555*****LOGIC SYSTEM LIFE SECTIONS.  FOR CID CHANGE COMPANY NAME.
000556
000557     IF CPA-COVERAGE-TYPE = 'P'
000558         MOVE 'PROP' TO CSO5-PLAN-CODE
000559         IF PI-COMPANY-ID EQUAL TO 'CID' OR 'DCC' OR 'AHL'
000560            MOVE 02-HEADING TO CSO2-COMPANY-NAME
000561            MOVE 02-COMP-NAME TO CSO26-COMPANY-NAME.
000562
000563     MOVE CPA-AMOUNT-PAID        TO  CSO5-AMOUNT-PAID
000564                                     CSO31-CHECK-AMOUNT
000565
000566     MOVE CPA-CHECK-NUMBER       TO  CSO2-CHECK-NUMBER
000567                                     CSO24-CHECK-NUMBER
000568
000569     IF CPA-CHECK-DATE NOT EQUAL TO LOW-VALUES
000570         MOVE CPA-CHECK-DATE     TO  DC-BIN-DATE-1
000571         MOVE SPACES             TO  DC-OPTION-CODE
000572         PERFORM 8500-DATE-CONVERSION
000573         IF DC-ERROR-CODE EQUAL TO SPACES
000574             MOVE DC-GREG-DATE-1-EDIT TO  CSO31-CHECK-DATE.
000575
000576     MOVE CPA-COMMENT TO WS-CPA-COMMENT.
000577
000578     MOVE CPA-COMMENT              TO  CSO-CHECK-PRINT-LINE-33
000579     MOVE CPA-COMMENT-2            TO  CSO-CHECK-PRINT-LINE-34
000580
000581     MOVE CPA-PAYEE-NAME           TO  CSO35-PAYEE-NAME
000582     MOVE CPA-PAYEE-ADDRESS-LINE1  TO  CSO36-PAYEE-ADDRESS1
000583     MOVE CPA-PAYEE-ADDRESS-LINE2  TO  CSO37-PAYEE-ADDRESS2
000584     MOVE CPA-PAYEE-ADDRESS-LINE3  TO  CSO38-PAYEE-ADDRESS3
000585     MOVE CPA-PAYEE-CITY-STATE     TO  CSO39-PAYEE-ADDRESS4
000586
000587     IF CPA-PAYEE-ZIP-CODE NOT EQUAL TO ZERO
000588         MOVE CPA-PAYEE-ZIP-CODE  TO  CSO40-PAYEE-ZIP-CODE.
000589
000590     IF CSO39-PAYEE-ADDRESS4 EQUAL TO SPACES
000591         MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
000592         MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
000593
000594     IF CSO38-PAYEE-ADDRESS3 EQUAL TO SPACES
000595         MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
000596         MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
000597         MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
000598
000599     IF CSO37-PAYEE-ADDRESS2 EQUAL TO SPACES
000600         MOVE CSO38-PAYEE-ADDRESS3 TO CSO37-PAYEE-ADDRESS2
000601         MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
000602         MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
000603         MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
000604
000605     IF CSO36-PAYEE-ADDRESS1 EQUAL TO SPACES
000606         MOVE CSO37-PAYEE-ADDRESS2 TO CSO36-PAYEE-ADDRESS1
000607         MOVE CSO38-PAYEE-ADDRESS3 TO CSO37-PAYEE-ADDRESS2
000608         MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
000609         MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
000610         MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
000611
000612 0600-MAIN-LOGIC-CONTINUE.
000613     MOVE CPA-NOTIFY-NAME                TO CSO17-3RD-NAME.
000614     MOVE CPA-NOTIFY-ADDRESS-LINE1       TO CSO18-3RDADD-LINE1.
000615     MOVE CPA-NOTIFY-ADDRESS-LINE2       TO CSO19-3RDADD-LINE2.
000616     MOVE CPA-NOTIFY-CITY-STATE          TO CSO20-3RD-CITY-STATE.
000617     MOVE CPA-NOTIFY-ZIP                 TO CSO21-3RD-ZIP.
000618
000619     EJECT
000620 0650-MAIN-LOGIC.
000621
000622     ADD +1  TO  PI-TEMP-STORAGE-ITEM
000623
000624     IF CPA-ALIGNMENT NOT EQUAL   TO ZERO THEN
000625        GO TO 0100-MAIN-LOGIC.
000626
000627     MOVE LOW-VALUES              TO CSO-DRAFT-420C.
000628     IF PI-COMPANY-ID = 'DCC'
000629        MOVE 'DCC1'               TO M420C-FORM
000630     ELSE
000631        MOVE '420C'               TO M420C-FORM
000632     END-IF
000633
000634     MOVE '0'                     TO M420C-DRAFT(1:1).
000635     MOVE CSO2-CHECK-NUMBER(1:1)  TO M420C-DRAFT(2:1).
000636     MOVE '00'                    TO M420C-DRAFT(3:2).
000637     MOVE CSO2-CHECK-NUMBER(2:6)  TO M420C-DRAFT(5:6).
000638     IF WS-DRAFT-ORDER = 99999
000639         MOVE ZEROS               TO WS-DRAFT-ORDER.
000640     ADD 1 TO WS-DRAFT-ORDER.
000641     MOVE WS-DRAFT-ORDER          TO M420C-DRAFT-ORDER.
000642     MOVE CPA-BENEFICIARY         TO M420C-LOAN-NUMBER
000643     MOVE CPA-AMOUNT-PAID         TO M420C-AMOUNT-PAID.
000644     MOVE CSO2-COMPANY-NAME       TO M420C-COMPANY-NAME.
000645     MOVE 'P.O. BOX 34350   OMAHA, NE  68134'
000646                                  TO M420C-CSO-ADDRESS.
000647     MOVE CSO5-CLAIM-NO           TO M420C-CLAIM-NO.
000648     MOVE CSO5-CERT-NO            TO M420C-CERT-NO.
000649     MOVE CSO5-PLAN-CODE          TO M420C-PLAN-CODE.
000650     MOVE CSO5-PAID-FROM-DATE     TO M420C-PAID-FROM-DATE.
000651     MOVE CSO5-PAID-THRU-DATE     TO M420C-PAID-THRU-DATE.
000652     MOVE CSO5-PAYMENT-TYPE       TO M420C-PAYMENT-TYPE.
000653     MOVE CSO5-ACCT-NO            TO M420C-ACCT-NO.
000654     MOVE CSO7-CC-ACCT            TO M420C-CC-ACCT.
000655     MOVE CSO7-CC-ACCT-NUMBER     TO M420C-CC-ACCT-NUMBER.
000656     MOVE CSO7-TYPE-MESSAGE       TO M420C-TYPE-MESSAGE.
000657     IF CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000658                                  OR 'B' OR 'H'
000659        MOVE 'NOTICE TO INSURED:' TO M420C-FINAL-MESS9
000660     ELSE
000661        MOVE SPACES               TO M420C-FINAL-MESS9
000662     END-IF
000663     MOVE SPACES                  TO M420C-FINAL-MESS10
000664     MOVE SPACES                  TO M420C-FINAL-MESS11
000665     MOVE CSO11-MEMBER-NAME       TO M420C-MEMBER-NAME.
000666
000667     MOVE CPA-INSURED-ADDRESS-LINE1 TO M420C-MEMBER-ADDRESS1.
000668     MOVE CPA-INSURED-ADDRESS-LINE2 TO M420C-MEMBER-ADDRESS2.
000669     MOVE CPA-INSURED-ADDRESS-LINE3 TO M420C-MEMBER-ADDRESS3.
000670     MOVE CPA-INSURED-CITY-STATE    TO M420C-MEMBER-ADDRESS4.
000671     MOVE CPA-INSURED-ZIP-CODE      TO CSO-ZIP.
000672     MOVE CSO-ZIP                   TO M420C-MEMBER-ZIP-CODE.
000673     MOVE CSO17-3RD-NAME          TO M420C-3RDADD-NAME.
000674     MOVE CSO18-3RDADD-LINE1      TO M420C-3RDADD-LINE1.
000675     MOVE CSO19-3RDADD-LINE2      TO M420C-3RDADD-LINE2.
000676     MOVE CSO20-3RD-CITY-STATE    TO M420C-3RDADD-LINE3.
000677     MOVE CSO21-3RD-ZIP           TO M420C-3RDADD-ZIP.
000678     MOVE CSO31-CHECK-DATE        TO M420C-CHECK-DATE.
000679     MOVE CSO-CHECK-PRINT-LINE-33 TO M420C-DFT-NOTES1.
000680     MOVE CSO-CHECK-PRINT-LINE-34 TO M420C-DFT-NOTES2.
000681     MOVE CSO35-PAYEE-NAME        TO M420C-PAYEE-NAME.
000682     MOVE CPA-PAYEE-ADDRESS-LINE1 TO M420C-PAYEE-ADDRESS1.
000683     MOVE CPA-PAYEE-ADDRESS-LINE2 TO M420C-PAYEE-ADDRESS2.
000684     MOVE CPA-PAYEE-ADDRESS-LINE3 TO M420C-PAYEE-ADDRESS3.
000685     MOVE CPA-PAYEE-CITY-STATE    TO M420C-PAYEE-ADDRESS4.
000686     MOVE CPA-PAYEE-ZIP-CODE      TO CSO-ZIP.
000687     MOVE CSO-ZIP                 TO M420C-PAYEE-ZIP-CODE.
000688     MOVE CSO23-REPLY-DT          TO M420C-REPLY-DATE.
000689********
000690* USE A IF YOU WANT BILL KIZER, AUTHORIZED SIGNATURE OR
000691* B IF YOU WANT BILL KIZER, PRESIDENT
000692********
000693     IF M420C-AMOUNT-PAID > 100000.00
000694       MOVE 'C'                   TO M420C-SIGNATURE
000695     ELSE
000696       MOVE 'B'                   TO M420C-SIGNATURE.
000697
000698     PERFORM FORMAT-DRAFT-MESSAGE
000699        THRU FORMAT-DRAFT-MESSAGE-EXIT
000700
000701     MOVE CPA-STATE               TO M420C-ACCT-STATE.
000702     MOVE CPA-CLAIM-TYPE          TO M420C-CLAIM-TYPE.
000703
000704     
      * EXEC CICS WRITE FILE('MICRDRFT') FROM(CSO-DRAFT-420C)
000705*       LENGTH(1254) RESP(CSO-RESP) RIDFLD(CSO-DRAFT-KEY)
000706*       END-EXEC.
           MOVE 'MICRDRFT' TO DFHEIV1
           MOVE 1254
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00002172' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303032313732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CSO-DRAFT-420C, 
                 DFHEIV11, 
                 CSO-DRAFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO CSO-RESP
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000707     GO TO 0100-MAIN-LOGIC.
000708     EJECT
000709
000710 5000-MAIN-LOGIC.
000711
000712     
      * EXEC CICS DELAY
000713*        INTERVAL (WS-DELAY-INTERVAL)
000714*    END-EXEC
      *    MOVE '0$I                   &   #00002180' TO DFHEIV0
           MOVE X'302449202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303032313830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000715
000716     GO TO 0100-MAIN-LOGIC.
000717
000718 8500-DATE-CONVERSION SECTION.
000719
000720
000721     
      * EXEC CICS LINK
000722*        PROGRAM  ('ELDATCV')
000723*        COMMAREA (DATE-CONVERSION-DATA)
000724*        LENGTH   (DC-COMM-LENGTH) END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002189' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032313839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000725
000726
000727 8500-EXIT.
000728
000729     EXIT.
000730
000731     EJECT
000732 FORMAT-DRAFT-MESSAGE SECTION.
000733
000734     MOVE SPACE TO M420C-DRAFT-MESSAGES
000735
000736     IF PI-COMPANY-ID = 'CID' OR 'AHL'
000737
000738     IF CPA-CLAIM-TYPE = ('L' OR 'O') AND CPA-PAYMENT-TYPE = '2'
000739        MOVE 420C-MSG4-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000740        MOVE 420C-MSG4-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000741        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000742     END-IF
000743
000744     IF CPA-CLAIM-TYPE = ('L' OR 'O') AND CPA-PAYMENT-TYPE = '4'
000745        MOVE 420C-MSG5-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000746        MOVE 420C-MSG5-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000747        MOVE 420C-MSG5-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000748        MOVE 420C-MSG5-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000749        MOVE 420C-MSG5-LINE(5) TO M420C-DRAFT-MESSAGE(5)
000750        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000751     END-IF
000752
000753     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000754                                  OR 'B' OR 'H' )
000755        AND CPA-PAYMENT-ORIGIN = '2'
000756        AND CPA-PAYMENT-TYPE = '1'
000757        MOVE 420C-MSG3-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000758        MOVE 420C-MSG3-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000759        MOVE 420C-MSG3-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000760        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000761     END-IF
000762
000763     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000764                                  OR 'B' OR 'H' )
000765        AND CPA-PAYMENT-ORIGIN = '2'
000766        AND CPA-PAYMENT-TYPE = '2'
000767        MOVE 420C-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000768        MOVE 420C-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000769        MOVE 420C-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000770        MOVE CSO23-REPLY-DT TO 420C-MSG1-LINE(4)(22:5)
000771        MOVE 420C-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000772        MOVE 420C-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
000773        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000774     END-IF
000775
000776     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000777                              OR 'B' OR 'H' )
000778        AND CPA-PAYMENT-TYPE = '1'
000779        MOVE 420C-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000780        MOVE 420C-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000781        MOVE 420C-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000782        MOVE CSO23-REPLY-DT TO 420C-MSG1-LINE(4)(22:5)
000783        MOVE 420C-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000784        MOVE 420C-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
000785        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000786     END-IF
000787
000788     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000789                              OR 'B' OR 'H' )
000790        AND CPA-PAYMENT-TYPE = '2'
000791        MOVE 420C-MSG2-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000792        MOVE 420C-MSG2-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000793        MOVE 420C-MSG2-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000794        MOVE 420C-MSG2-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000795        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000796     END-IF
000797     ELSE
000798     IF CPA-CLAIM-TYPE = 'L' AND CPA-PAYMENT-TYPE = '2'
000799        MOVE DCC1-MSG4-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000800        MOVE DCC1-MSG4-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000801        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000802     END-IF
000803
000804     IF CPA-CLAIM-TYPE = 'L' AND CPA-PAYMENT-TYPE = '4'
000805        MOVE DCC1-MSG5-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000806        MOVE DCC1-MSG5-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000807        MOVE DCC1-MSG5-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000808        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000809     END-IF
000810
000811     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000812                              OR 'B' OR 'H' )
000813        AND CPA-PAYMENT-ORIGIN = '2'
000814        AND CPA-PAYMENT-TYPE = '1'
000815        MOVE DCC1-MSG3-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000816        MOVE DCC1-MSG3-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000817        MOVE DCC1-MSG3-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000818        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000819     END-IF
000820
000821     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000822                              OR 'B' OR 'H' )
000823        AND CPA-PAYMENT-ORIGIN = '2'
000824        AND CPA-PAYMENT-TYPE = '2'
000825        MOVE DCC1-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000826        MOVE DCC1-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000827        MOVE DCC1-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000828        MOVE CSO23-REPLY-DT TO DCC1-MSG1-LINE(5)(54:5)
000829        MOVE DCC1-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000830        MOVE DCC1-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
000831        MOVE DCC1-MSG1-LINE(6) TO M420C-DRAFT-MESSAGE(6)
000832        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000833     END-IF
000834
000835     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000836                              OR 'B' OR 'H' )
000837        AND CPA-PAYMENT-TYPE = '1'
000838        MOVE DCC1-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000839        MOVE DCC1-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000840        MOVE DCC1-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000841        MOVE CSO23-REPLY-DT TO DCC1-MSG1-LINE(5)(54:5)
000842        MOVE DCC1-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000843        MOVE DCC1-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
000844        MOVE DCC1-MSG1-LINE(6) TO M420C-DRAFT-MESSAGE(6)
000845        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000846     END-IF
000847
000848     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
000849                              OR 'B' OR 'H' )
000850        AND CPA-PAYMENT-TYPE = '2'
000851        MOVE DCC1-MSG2-LINE(1) TO M420C-DRAFT-MESSAGE(1)
000852        MOVE DCC1-MSG2-LINE(2) TO M420C-DRAFT-MESSAGE(2)
000853        MOVE DCC1-MSG2-LINE(3) TO M420C-DRAFT-MESSAGE(3)
000854        MOVE DCC1-MSG2-LINE(4) TO M420C-DRAFT-MESSAGE(4)
000855        GO TO FORMAT-DRAFT-MESSAGE-EXIT
000856     END-IF
000857     END-IF
000858
000859     .
000860 FORMAT-DRAFT-MESSAGE-EXIT.
000861     EXIT.
000862
000863     EJECT
000864 SPELL-DOLLAR SECTION.
      *                      COPY ELC176P1.
      *>>((file: ELC176P1))
000001*****************************************************************
000002*                                                               *
000003*                            ELC176P1.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                         *
000006*                                                               *
000007* THIS SECTION CONVERTS A DOLLAR FIGURE INTO A SPELLED OUT AMT. *
000008*                                                               *
000009*****************************************************************
000010
000011 SDS-010.
000012     MOVE SPACES                 TO  WS-SPELLED-AMOUNT
000013                                     SD-PASS-SPELLED-AMOUNT
000014                                     WS-SPELLED-LINE1
000015                                     WS-SPELLED-LINE2.
000016
000017     SET SA-INDEX TO +1.
000018
000019     MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT.
000020
000021     IF WS-MILLIONS GREATER ZERO
000022         MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK
000023         PERFORM SPELL-AMOUNT
000024         MOVE 'MILLION'          TO  WS-WORD
000025         PERFORM MOVE-WORD.
000026
000027     IF WS-THOUSANDS GREATER ZERO
000028         MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK
000029         PERFORM SPELL-AMOUNT
000030         MOVE 'THOUSAND'         TO  WS-WORD
000031         PERFORM MOVE-WORD.
000032
000033     IF WS-HUNDREDS GREATER ZERO
000034         MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK
000035         PERFORM SPELL-AMOUNT.
000036
000037     IF WS-AMOUNT LESS +1.00
000038         MOVE 'NO'               TO  WS-WORD
000039         PERFORM MOVE-WORD.
000040
000041     IF WS-CENTS NOT GREATER ZERO
000042         MOVE 'NO'               TO  WS-CENTS-X.
000043
000044     MOVE WS-CENTS-X             TO  WS-PENNEYS.
000045
000046     MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD.
000047     PERFORM MOVE-WORD.
000048
000049     INSPECT WS-SPELLED-AMOUNT REPLACING ALL '-' BY SPACES.
000050
000051     MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT.
000052
000053     PERFORM MOVE-SPELLED-AMOUNT.
000054
000055 SDS-EXIT.
000056     EXIT.
000057
000058     EJECT
000059 SPELL-AMOUNT SECTION.
000060
000061*SAS-NOTE.
000062*
000063*    NOTE *******************************************************
000064*         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     *
000065*         *  INTO A SPELLED AMOUNT.                             *
000066*         *******************************************************
000067
000068 SAS-010.
000069     IF WS-HUNDRED GREATER ZERO
000070         SET SINGLE-INDEX        TO  WS-HUNDRED
000071         MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
000072         PERFORM MOVE-WORD
000073         MOVE 'HUNDRED'          TO  WS-WORD
000074         PERFORM MOVE-WORD.
000075
000076     IF WS-TEEN GREATER ZERO
000077         IF WS-TEEN LESS +20
000078             SET SINGLE-INDEX TO WS-TEEN
000079             MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
000080             PERFORM MOVE-WORD
000081           ELSE
000082             SET UPPER-INDEX TO WS-TEN
000083             MOVE WS-UPPER-DESC (UPPER-INDEX)  TO  WS-WORD
000084             PERFORM MOVE-WORD
000085             IF WS-ONE GREATER ZERO
000086                 SET SINGLE-INDEX TO WS-ONE
000087                 MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
000088                 PERFORM MOVE-WORD.
000089
000090 SAS-EXIT.
000091     EXIT.
000092
000093     EJECT
000094 MOVE-WORD SECTION.
000095
000096*MWS-NOTE.
000097*
000098*    NOTE *******************************************************
000099*         *      THIS SECTION MOVES ONE WORD TO THE SPELLED     *
000100*         *  AMOUNT OUTPUT LINE.                                *
000101*         *******************************************************.
000102
000103 MWD-010.
000104     PERFORM MOVE-CHARACTERS
000105         VARYING CHAR-INDEX FROM +1 BY +1
000106             UNTIL WS-CHAR2 (CHAR-INDEX) = SPACES.
000107
000108     SET SA-INDEX UP BY +1.
000109
000110 MWD-EXIT.
000111     EXIT.
000112
000113 MOVE-CHARACTERS SECTION.
000114
000115*MCS-NOTE.
000116*
000117*    NOTE *******************************************************
000118*         *      THIS SECTION MOVES ONE CHARACTER TO THE SPELLED*
000119*         *  AMOUNT OUTPUT LINE.                                *
000120*         *******************************************************.
000121
000122 MCD-010.
000123     MOVE WS-CHAR2 (CHAR-INDEX)  TO  WS-CHAR (SA-INDEX).
000124
000125     SET SA-INDEX UP BY +1.
000126
000127 MCD-EXIT.
000128     EXIT.
000129
000130     EJECT
000131 MOVE-SPELLED-AMOUNT SECTION.
000132
000133*MSA-NOTE.
000134*
000135*    NOTE *******************************************************
000136*         *      THIS SECTION MOVES THE SPELLED DOLLAR AMOUNT   *
000137*         *  TO TWO LINES IF NECESSARY.                         *
000138*         *******************************************************.
000139
000140 MSA-010.
000141     ADD WS-1ST-LINE-LENGTH +1 GIVING WS-1ST-LINE-LENGTH-PLUS-1.
000142     ADD WS-1ST-LINE-LENGTH +2 GIVING WS-1ST-LINE-LENGTH-PLUS-2.
000143     ADD WS-1ST-LINE-LENGTH -1 GIVING WS-1ST-LINE-LENGTH-MINUS-1.
000144
000145     MOVE WS-SPELLED-AMOUNT  TO  WS-SPELLED-LINE1.
000146
000147     IF SA-INDEX GREATER WS-1ST-LINE-LENGTH-PLUS-1
000148         SET SL2-INDEX TO +1
000149         IF WS-CHAR (WS-1ST-LINE-LENGTH-PLUS-1) = SPACES
000150             PERFORM MOVE-LINE2 VARYING SA-INDEX2
000151               FROM WS-1ST-LINE-LENGTH-PLUS-2 BY +1
000152                 UNTIL SL2-INDEX GREATER WS-2ND-LINE-LENGTH
000153           ELSE
000154             PERFORM CLEAR-LINE1 VARYING SL1-INDEX
000155                 FROM WS-1ST-LINE-LENGTH BY -1
000156                     UNTIL WS-SL1 (SL1-INDEX) = SPACES
000157             SET SL1-INDEX UP BY +1
000158             PERFORM MOVE-LINE2
000159               VARYING SA-INDEX2 FROM SL1-INDEX BY +1
000160                 UNTIL SL2-INDEX GREATER WS-2ND-LINE-LENGTH.
000161
000162     MOVE WS-SPELLED-LINE1       TO  SD-PSA-LINE1.
000163     MOVE WS-SPELLED-LINE2       TO  SD-PSA-LINE2.
000164
000165 MSA-EXIT.
000166     EXIT.
000167
000168     EJECT
000169 CLEAR-LINE1 SECTION.
000170
000171*CLS-NOTE.
000172*
000173*    NOTE *******************************************************
000174*         *      THIS SECTION CLEARS THE TRAILING WORD IN THE   *
000175*         *  SPELLED LINE 1 IF THE AMOUNT IS GREATER THAN 78.   *
000176*         *******************************************************.
000177
000178 CLS-010.
000179     MOVE SPACES                 TO  WS-SL1 (SL1-INDEX).
000180
000181 CLS-EXIT.
000182     EXIT.
000183
000184 MOVE-LINE2 SECTION.
000185
000186*MLS-NOTE.
000187*
000188*    NOTE *******************************************************
000189*         *      THIS SECTION MOVES ONE CHARACTER TO THE SPELLED*
000190*         *  AMOUNT OUTPUT LINE.                                *
000191*         *******************************************************.
000192
000193 MLS-010.
000194     MOVE WS-CHAR (SA-INDEX2)    TO  WS-SL2 (SL2-INDEX).
000195
000196     SET SL2-INDEX UP BY +1.
000197
000198     IF WS-CHAR (SA-INDEX2)     = SPACES   AND
000199        WS-CHAR (SA-INDEX2 + 1) = SPACES
000200         SET SL2-INDEX TO +99.
000201
000202 MLS-EXIT.
000203     EXIT.
000204
      *<<((file: ELC176P1))
000865
000866     EJECT
000867 POS-SPELL-DOLLAR SECTION.
000868
000869*SDS-NOTE.
000870*
000871*    NOTE *******************************************************
000872*         *                                                     *
000873*         *      THIS SECTION CONVERTS A DOLLAR FIGURE INTO A   *
000874*         *  SPELLED OUT AMOUNT.                                *
000875*         *                                                     *
000876*         *******************************************************.
000877
000878 SDS-010.
000879
000880     MOVE SPACES                 TO  WS-SPELLED-AMOUNT
000881                                     SD-PASS-SPELLED-AMOUNT
000882                                     WS-SPELLED-LINE1
000883                                     WS-SPELLED-LINE2
000884     MOVE ZERO                   TO  WS-SW
000885
000886     SET SA-INDEX TO +1
000887
000888     MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT
000889
000890     IF WS-MILLIONS IS GREATER THAN ZERO
000891         MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK
000892         PERFORM POS-SPELL-AMOUNT
000893         MOVE +1                 TO  WS-SW.
000894
000895     IF WS-THOUSANDS IS GREATER THAN ZERO
000896         MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK
000897         PERFORM POS-SPELL-AMOUNT
000898         MOVE +1                 TO  WS-SW
000899       ELSE
000900         IF WS-MILLIONS IS GREATER THAN ZERO
000901             MOVE 'ZERO'         TO  WS-WORD
000902             PERFORM MOVE-WORD 3 TIMES.
000903
000904     IF WS-HUNDREDS IS GREATER THAN ZERO
000905         MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK
000906         PERFORM POS-SPELL-AMOUNT
000907         MOVE +1                 TO  WS-SW
000908       ELSE
000909         IF WS-MILLIONS IS GREATER THAN ZERO
000910           OR WS-THOUSANDS IS GREATER THAN ZERO
000911             MOVE 'ZERO'         TO  WS-WORD
000912             PERFORM MOVE-WORD 3 TIMES.
000913
000914     IF WS-AMOUNT IS LESS THAN +1.00
000915         MOVE 'NO'               TO  WS-WORD
000916         PERFORM MOVE-WORD.
000917
000918     IF WS-CENTS IS NOT GREATER THAN ZERO
000919         MOVE 'NO'               TO  WS-CENTS-X.
000920
000921     MOVE WS-CENTS-X             TO  WS-PENNEYS
000922
000923     MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD
000924     PERFORM MOVE-WORD
000925
000926*    TRANSFORM WS-SPELLED-AMOUNT FROM '-' TO SPACES
000927     INSPECT WS-SPELLED-AMOUNT REPLACING ALL '-' BY ' '.
000928
000929     MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT
000930
000931     PERFORM MOVE-SPELLED-AMOUNT.
000932
000933 SDS-EXIT.
000934
000935     EXIT.
000936
000937     EJECT
000938 POS-SPELL-AMOUNT SECTION.
000939
000940*SAS-NOTE.
000941*
000942*    NOTE *******************************************************
000943*         *                                                     *
000944*         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     *
000945*         *  INTO A SPELLED AMOUNT.                             *
000946*         *                                                     *
000947*         *                                                     *
000948*         *******************************************************.
000949
000950 SAS-010.
000951
000952     IF WS-HUNDRED IS GREATER THAN ZERO
000953         SET SINGLE-INDEX        TO  WS-HUNDRED
000954         MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
000955         PERFORM MOVE-WORD
000956       ELSE
000957         IF WS-SW NOT EQUAL TO ZERO
000958             MOVE 'ZERO'         TO  WS-WORD
000959             PERFORM MOVE-WORD.
000960
000961     IF WS-TEN IS GREATER THAN ZERO
000962         SET SINGLE-INDEX TO WS-TEN
000963         MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
000964         PERFORM MOVE-WORD
000965       ELSE
000966         IF WS-HUNDRED IS GREATER THAN ZERO
000967           OR WS-SW NOT EQUAL TO ZERO
000968             MOVE 'ZERO'         TO  WS-WORD
000969             PERFORM MOVE-WORD.
000970
000971     IF WS-ONE IS GREATER THAN ZERO
000972         SET SINGLE-INDEX TO WS-ONE
000973         MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
000974         PERFORM MOVE-WORD
000975       ELSE
000976         IF WS-HUNDRED IS GREATER THAN ZERO
000977           OR WS-TEN IS GREATER THAN ZERO
000978           OR WS-SW NOT EQUAL TO ZERO
000979             MOVE 'ZERO'         TO  WS-WORD
000980             PERFORM MOVE-WORD.
000981
000982 SAS-EXIT.
000983
000984     EXIT.
000985
000986     EJECT
000987 9990-ERROR SECTION.
000988
000989
000990     
      * EXEC CICS LINK
000991*        PROGRAM  ('EL004')
000992*        COMMAREA (DFHEIBLK)
000993*        LENGTH   (64) END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00002665' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303032363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000994
000995
000996 9990-EXIT.
000997
000998     EXIT.
000999
001000     SKIP3
001001 9999-LAST-PARAGRAPH SECTION.
001002
001003     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL177' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
001004

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL177' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 5000-MAIN-LOGIC,
                     5000-MAIN-LOGIC,
                     9990-ERROR
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL177' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
